/**
 *@file amod.c
 */

#define amod_c_
#define ALO_LIB

#include "astr.h"
#include "amem.h"
#include "agc.h"
#include "aerr.h"

#include "amod.h"

#define MOD_MAX_CAP (u32c(1) << 31)

#define dead_key ((GStr*) sizeof(a_usize))

static Impl const mod_impl;

GMod* ai_mod_new(a_henv env, a_usize extra) {
    a_usize size = mod_size(extra);
    GMod* self = ai_mem_alloc(env, size);

    memclr(self, size);
    self->impl = &mod_impl;
    self->size = size;

    ai_gc_register_object(env, self);

    return self;
}

void ai_mod_mark(Global* gbl, GMod* self) {
    if (self->ptr != null) {
        for (a_u32 i = 0; i <= self->hmask; ++i) {
            MNode* node = &self->ptr[i];
            if (node->key > dead_key) {
                ai_gc_trace_mark(gbl, node->key);
                ai_gc_trace_mark_val(gbl, node->value);
            }
        }
        ai_gc_trace_work(gbl, sizeof(MNode) * (self->hmask + 1));
    }
}

void ai_mod_deinit(Global* gbl, GMod* self) {
    if (self->ptr != null) {
        ai_mem_vdel(gbl, self->ptr, self->hmask + 1);
    }
}

a_bool ai_mod_get(a_henv env, GMod* self, Value vk, Value* pv) {
    if (!v_is_str(vk)) {
        ai_err_bad_key(env, "mod", v_nameof(env, vk));
    }
    return ai_mod_gets(env, self, v_as_str(vk), pv);
}

a_bool ai_mod_gets(a_henv env, GMod* self, GStr* key, Value* pval) {
    if (self->len == 0) return true;

    a_u32 index = key->hash & self->hmask;
    a_u32 perturb = key->hash;

    MNode* field = &self->ptr[index];
    if (field->key == null) return true;

    loop {
        if (field->key == key) {
            v_cpy(env, pval, &field->value);
            return false;
        }
        if (field->key == null) {
            return true;
        }

        index = (index * 5 + perturb + 1) & self->hmask;
        perturb >>= 5;

        field = &self->ptr[index];
    }
}

a_bool ai_mod_getls(a_henv env, GMod* self, char const* src, a_usize len, Value* pval) {
    a_hash hash = ai_str_hashof(env, src, len);

    GStr* str = ai_str_get_or_null_with_hash(env, src, len, hash);
    if (str == null) {
        return true;
    }

    return ai_mod_gets(env, self, str, pval);
}

static a_bool mod_needs_grow_one(GMod* self) {
    return self->len >= (self->hmask + 1) / 4 * 3;
}

a_bool ai_mod_refs_or_empty(unused a_henv env, GMod* self, GStr* key, MNode** pnode) {
    if (self->len == 0) {
        *pnode = null;
        return true;
    }

    a_u32 index = key->hash & self->hmask;
    a_u32 perturb = key->hash;

    a_bool can_put_inplace = !mod_needs_grow_one(self);
    MNode* first_empty_field = null;

    MNode* field = &self->ptr[index];
    if (field->key == null) {
        *pnode = can_put_inplace ? field : null;
        return true;
    }

    loop {
        if (field->key == key) {
            *pnode = field;
            return false;
        }
        if (field->key == null) {
            *pnode = can_put_inplace ? (first_empty_field ?: field) : null;
            return true;
        }
        if (field->key == dead_key && can_put_inplace && first_empty_field == null) {
            first_empty_field = field;
        }

        index = (index * 5 + perturb + 1) & self->hmask;
        perturb >>= 5;

        field = &self->ptr[index];
    }
}

static MNode* mod_ref_empty(GMod* self, GStr* key) {
    a_u32 index = key->hash & self->hmask;
    a_u32 perturb = key->hash;

    MNode* node = &self->ptr[index];
    if (node->key == null) return node;

    loop {
        if (node->key <= dead_key) {
            return node;
        }

        index = (index * 5 + perturb + 1) & self->hmask;
        perturb >>= 5;

        node = &self->ptr[index];
    }
}

static void mod_put(a_henv env, GMod* self, GStr* key, Value val) {
    MNode* field = mod_ref_empty(self, key);
    field->key = key;
    v_set(env, &field->value, val);
}

static void mod_grow(a_henv env, GMod* self) {
    a_u32 old_cap = (self->hmask + 1) & ~1;
    MNode* old_ptr = self->ptr;

    if (unlikely(old_cap == MOD_MAX_CAP)) {
        ai_err_raisef(env, ALO_EINVAL, "too many fields");
    }

    a_u32 new_cap = max(old_cap * 2, 4);
    MNode* new_ptr = ai_mem_vnew(env, MNode, new_cap);

    memclr(new_ptr, new_cap * sizeof(MNode));

    self->ptr = new_ptr;
    self->hmask = new_cap - 1;

    if (old_ptr != null) {
        for (a_u32 i = 0; i < old_cap; ++i) {
            MNode* field = &old_ptr[i];
            if (field->key > dead_key) {
                mod_put(env, self, field->key, field->value);
            }
        }

        ai_mem_vdel(G(env), old_ptr, old_cap);
    }
}

a_bool ai_mod_set(a_henv env, GMod* self, Value vk, Value vv) {
    if (!v_is_str(vk)) {
        ai_err_bad_key(env, "mod", v_nameof(env, vk));
    }
    ai_mod_sets(env, self, v_as_str(vk), vv);
    return false;
}

void ai_mod_sets(a_henv env, GMod* self, GStr* key, Value val) {
    MNode* node;
    if (ai_mod_refs_or_empty(env, self, key, &node)) {
        if (node != null) {
            node->key = key;
            v_set(env, &node->value, val);
        }
        else {
            mod_grow(env, self);
            mod_put(env, self, key, val);
        }

        ai_gc_barrier_backward(env, self, key);
        ai_gc_barrier_backward_val(env, self, val);

        self->len += 1;
        self->nchg += 1;
        self->ftmz = 0;
    }
    else {
        assume(node != null);

        v_set(env, &node->value, val);

        ai_gc_barrier_forward_val(env, self, val);

        self->nchg += 1;
    }
}

Value* ai_mod_refls(a_henv env, GMod* self, char const* src, a_usize len) {
    a_hash hash = ai_str_hashof(env, src, len);
    MNode* node;

    GStr* str = ai_str_get_or_null_with_hash(env, src, len, hash);
    if (str != null) {
        if (!ai_mod_refs_or_empty(env, self, str, &node)) {
            return &node->value;
        }

        if (node != null) {
            goto place;
        }

        mod_grow(env, self);
    }
    else if (mod_needs_grow_one(self)) {
        mod_grow(env, self);
    }

    str = ai_str_get_or_new(env, src, len);
    node = mod_ref_empty(self, str);

place:
    node->key = str;

    ai_gc_barrier_backward(env, self, str);

    self->len += 1;
    self->nchg += 1;
    self->ftmz = 0;

    return &node->value;
}

static void mod_drop(Global* gbl, GMod* self) {
    ai_mod_deinit(gbl, self);
    ai_mem_dealloc(gbl, self, self->size);
}

static void mod_mark(Global* gbl, GMod* self) {
    ai_mod_mark(gbl, self);
    ai_gc_trace_work(gbl, self->size);
}

static Impl const mod_impl = {
    .tag = ALO_TMOD,
    .flags = VTABLE_FLAG_NONE,
    .drop = mod_drop,
    .mark = mod_mark
};