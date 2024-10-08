/**
 *@file atype.c
 */

#define atype_c_
#define ALO_LIB

#include "atable.h"
#include "aenv.h"
#include "amem.h"
#include "agc.h"
#include "aerr.h"

#include "atype.h"

static Impl const type_impl;

#define TYPE_MAX_CAP (u32c(1) << 31)

#define dead_key ((GStr*) sizeof(a_usize))

GType* ai_type_new(a_henv env, GStr* name) {
    a_usize size = type_size(0);

    GType* self = ai_mem_alloc(env, size);
    memclr(self, size);

    self->impl = &type_impl;
    self->name = name ?: g_str(env, STR_EMPTY);
    self->size = size;

    ai_gc_register_normal(env, self);
    return self;
}

a_bool ai_type_get(a_henv env, GType* self, Value vk, Value* pv) {
    if (!v_is_str(vk)) {
        ai_err_bad_key(env, "type", v_nameof(env, vk));
    }
    return ai_type_gets(env, self, v_as_str(vk), pv);
}

a_bool ai_type_gets(a_henv env, GType* self, GStr* key, Value* pval) {
    if (self->len == 0)
        return true;

    a_u32 index = key->hash & self->hmask;
    a_u32 perturb = key->hash;

    MNode* field = &self->ptr[index];
    if (field->key == null)
        return true;

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

a_bool ai_type_getls(a_henv env, GType* self, char const* src, a_usize len, Value* pval) {
    a_hash hash = ai_str_hashof(env, src, len);

    GStr* str = ai_str_get_or_null_with_hash(env, src, len, hash);
    if (str == null)
        return true;

    return ai_type_gets(env, self, str, pval);
}

static a_bool type_needs_grow(GType* self) {
    return self->len >= (self->hmask + 1) / 4 * 3;
}

a_bool ai_type_refs_or_empty(unused a_henv env, GType* self, GStr* key, MNode** pnode) {
    if (self->len == 0) {
        *pnode = null;
        return true;
    }

    a_u32 index = key->hash & self->hmask;
    a_u32 perturb = key->hash;

    a_bool can_put_inplace = !type_needs_grow(self);
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

static MNode* type_ref_empty(GType* self, GStr* key) {
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

static void type_put(a_henv env, GType* self, GStr* key, Value val) {
    MNode* field = type_ref_empty(self, key);
    field->key = key;
    v_set(env, &field->value, val);
}

static void type_grow(a_henv env, GType* self) {
    a_u32 old_cap = (self->hmask + 1) & ~u32c(1);
    MNode* old_ptr = self->ptr;

    if (unlikely(old_cap == TYPE_MAX_CAP)) {
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
                type_put(env, self, field->key, field->value);
            }
        }

        ai_mem_vdel(G(env), old_ptr, old_cap);
    }
}

a_bool ai_type_set(a_henv env, GType* self, Value vk, Value vv) {
    if (!v_is_str(vk)) {
        ai_err_bad_key(env, "type", v_nameof(env, vk));
    }
    ai_type_sets(env, self, v_as_str(vk), vv);
    return false;
}

void ai_type_sets(a_henv env, GType* self, GStr* key, Value val) {
    MNode* node;
    if (ai_type_refs_or_empty(env, self, key, &node)) {
        if (node != null) {
            node->key = key;
            v_set(env, &node->value, val);
        }
        else {
            type_grow(env, self);
            type_put(env, self, key, val);
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

Value* ai_type_refls(a_henv env, GType* self, char const* src, a_usize len) {
    a_hash hash = ai_str_hashof(env, src, len);
    MNode* node;

    GStr* str = ai_str_get_or_null_with_hash(env, src, len, hash);
    if (str != null) {
        if (!ai_type_refs_or_empty(env, self, str, &node)) {
            return &node->value;
        }

        if (node != null) {
            goto place;
        }

        type_grow(env, self);
    }
    else if (type_needs_grow(self)) {
        type_grow(env, self);
    }

    str = ai_str_get_or_new(env, src, len);
    node = type_ref_empty(self, str);

place:
    node->key = str;

    ai_gc_barrier_backward(env, self, str);

    self->len += 1;
    self->nchg += 1;
    self->ftmz = 0;

    return &node->value;
}

void ai_type_boost(a_henv env) {
    Global* gbl = G(env);

    static a_u8 const l_name_tags[] = {
        [ALO_TNIL] = STR_nil,
        [ALO_TBOOL] = STR_bool,
        [ALO_TINT] = STR_int,
        [ALO_TFLOAT] = STR_float,
        [ALO_TPTR] = STR_ptr,
        [ALO_TSTR] = STR_str,
        [ALO_TTUPLE] = STR_tuple,
        [ALO_TLIST] = STR_list,
        [ALO_TTABLE] = STR_table,
        [ALO_TFUNC] = STR_func,
        [ALO_TTYPE] = STR_type,
        [ALO_TROUTE] = STR_route
    };

    static_assert(sizeof(l_name_tags) == TYPE__COUNT);

    for (a_u32 i = 0; i < TYPE__COUNT; ++i) {
        init(gbl->fast_types[i]) {
            .impl = &type_impl,
            .size = 0,
            .name = g_str(env, l_name_tags[i])
        };
    }
}

static void type_clean(Global* gbl, GType* self) {
    if (self->ptr != null) {
        ai_mem_vdel(gbl, self->ptr, self->hmask + 1);
    }
}

void ai_type_clean(Global* gbl) {
    for (a_u32 i = 0; i < TYPE__COUNT; ++i) {
        type_clean(gbl, gbl->fast_types[i]);
    }
}

static void type_drop(Global* gbl, GType* self) {
    type_clean(gbl, self);
    ai_mem_dealloc(gbl, self, self->size);
}

static void type_mark(Global* gbl, GType* self) {
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
    ai_gc_trace_mark(gbl, self->name);
    ai_gc_trace_work(gbl, self->size);
}

static Impl const type_impl = {
    .tag = ALO_TTYPE,
    .drop = type_drop,
    .mark = type_mark
};
