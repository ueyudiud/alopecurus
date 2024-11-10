/**
 *@file atype.c
 */

#define atype_c_
#define ALO_LIB

#include "atable.h"
#include "auser.h"
#include "aenv.h"
#include "amem.h"
#include "agc.h"
#include "aerr.h"
#include "atm.h"
#include "aapi.h"

#include "atype.h"

static KLocal const ptype_klass;
static KHeap const itype_klass;
static KHeap const utype_klass;

#define TYPE_MAX_CAP (u32c(1) << 31)

#define dead_key ((GStr*) sizeof(a_usize))

GType* ai_utype_new(a_henv env, GStr* name, a_u32 extra_size, a_u32 block_size, a_u32 num_slot) {
    a_usize size = type_size(extra_size);

    GType* self = ai_mem_alloc(env, size);
    memclr(self, size);

    init(self->as_utype) {
        .klass = &utype_klass,
        .size = size,
        .body = {
            .tag = ALO_TUSER,
            .name = name != null ? str2ntstr(name) : null,
            .flags = (num_slot == 0 ? KLASS_FLAG_PLAIN : 0) | KLASS_FLAG_BUILD,
            .mark = ai_user_mark,
            .close = ai_tm_close,
            .drop = ai_user_drop
        },
        .num_slot = num_slot,
        .block_size = block_size
    };

    ai_gc_register_normal(env, self);
    return self;
}

GType* ai_itype_new(a_henv env, void const* ksrc, a_usize klen) {
    assume(klen >= sizeof(KHeap));

    Klass const* klass = ksrc;

    a_usize res_size = klen - offsetof(Klass, mark);
    a_usize size = sizeof(GBType) + res_size;

    GType* self = ai_mem_alloc(env, size);
    memclr(self, size);

    init(self->as_btype) {
        .klass = &itype_klass,
        .size = size,
        .tag = klass->tag,
        .name = klass->name,
        .flags = klass->flags & ~(KLASS_FLAG_BUILD | KLASS_FLAG_HIDDEN)
    };

    memcpy(&self->as_itype->body.mark, &klass->mark, res_size);

    ai_gc_register_normal(env, self);
    return self;
}

a_bool ai_type_get(a_henv env, GType* self, Value vk, Value* pv) {
    if (!v_is_str(vk)) {
        ai_err_bad_key(env, "type", v_name(env, vk));
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

a_bool ai_type_getls(a_henv env, GType* self, a_lstr k, Value* pval) {
    a_hash hash = ai_str_hashof(env, k);

    GStr* str = ai_str_get_or_null_with_hash(env, k, hash);
    if (str == null)
        return true;

    return ai_type_gets(env, self, str, pval);
}

static a_bool type_needs_grow(GType* self) {
    return self->len >= (self->hmask + 1) / 4 * 3;
}

static a_bool type_ref(unused a_henv env, GType* self, GStr* key, MNode** pnode) {
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

static MNode* type_reserve(GType* self, GStr* key) {
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
    MNode* field = type_reserve(self, key);
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
        ai_err_bad_key(env, "type", v_name(env, vk));
    }
    ai_type_sets(env, self, v_as_str(vk), vv);
    return false;
}

void ai_type_sets(a_henv env, GType* self, GStr* key, Value val) {
    MNode* node;
    if (type_ref(env, self, key, &node)) {
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

Value* ai_type_refls(a_henv env, GType* self, a_lstr k) {
    a_hash hash = ai_str_hashof(env, k);
    MNode* node;

    GStr* str = ai_str_get_or_null_with_hash(env, k, hash);
    if (str != null) {
        if (!type_ref(env, self, str, &node)) {
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

    str = ai_str_get_or_new(env, k);
    node = type_reserve(self, str);

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

    for (a_u32 i = 0; i < PTYPE_COUNT; ++i) {
        init(&gbl->fast_types[i]) {
            .klass = &ptype_klass,
            .size = 0,
            .name = ai_api_tagname[i]
        };
    }
}

static void type_clean(Global* gbl, GType* self) {
    if (self->ptr != null) {
        ai_mem_vdel(gbl, self->ptr, self->hmask + 1);
    }
}

void ai_type_clean(Global* gbl) {
    for (a_u32 i = 0; i < PTYPE_COUNT; ++i) {
        type_clean(gbl, g_ptype(gbl, i));
    }
}

static void type_mark_dict(Global* gbl, GType* self) {
    if (self->ptr != null) {
        for (a_u32 i = 0; i <= self->hmask; ++i) {
            MNode* node = &self->ptr[i];
            if (node->key > dead_key) {
                g_trace(gbl, node->key);
                v_trace(gbl, node->value);
            }
        }
        ai_gc_trace_work(gbl, sizeof(MNode) * (self->hmask + 1));
    }
}

static void ptype_mark(Global* gbl, GType* self) {
    type_mark_dict(gbl, self);
}

static void itype_mark(Global* gbl, GType* self) {
    type_mark_dict(gbl, self);
    ai_gc_trace_work(gbl, self->size);
}

static void utype_mark(Global* gbl, GType* self) {
    type_mark_dict(gbl, self);
    if (self->name != null) {
        g_trace(gbl, from_member(GStr, ptr, self->name));
    }
    ai_gc_trace_work(gbl, self->size);
}

static void nptype_drop(Global* gbl, GType* self) {
    type_clean(gbl, self);
    ai_mem_dealloc(gbl, self, self->size);
}

static KLocal const ptype_klass = {
    .tag = ALO_TTYPE,
    .name = "type",
    .flags = KLASS_FLAG_NONE,
    .mark = ptype_mark
};

static KHeap const itype_klass = {
    .tag = ALO_TTYPE,
    .name = "type",
    .flags = KLASS_FLAG_NONE,
    .drop = nptype_drop,
    .mark = itype_mark
};

static KHeap const utype_klass = {
    .tag = ALO_TTYPE,
    .name = "type",
    .flags = KLASS_FLAG_BUILD,
    .drop = nptype_drop,
    .mark = utype_mark
};
