/**
 *@file adict.c
 */

#define adict_c_
#define ALO_LIB

#include "amem.h"
#include "agc.h"

#include "adict.h"

#define ALO_DICT_LOAD_FACTOR 3 / 4

#define dead_key ((GStr*) sizeof(GStr))

void ai_dict_init(unused a_henv env, Dict* self) {
    self->_ptr = null;
    self->_hmask = 0;
    self->_len = 0;
}

void ai_dict_deinit(Global* g, Dict* self) {
    if (self->_ptr != null) {
        ai_mem_vdel(g, self->_ptr, self->_hmask + 1);
    }
}

void ai_dict_mark(Global* g, Dict* self) {
    if (self->_ptr == null)
        return;
    ai_gc_trace_work(g, (self->_hmask + 1) * sizeof(DNode));
    if (self->_len == 0)
        return;
    for (a_u32 i = 0; i <= self->_hmask; ++i) {
        DNode* node = &self->_ptr[i];
        if (node->_key > dead_key) {
            ai_gc_trace_mark(g, node->_key);
            ai_gc_trace_mark_val(g, node->_value);
        }
    }
}

static void dict_put_inplace(a_henv env, Dict* self, GStr* key, Value val) {
    a_u32 hash = key->_hash;
    a_u32 hmask = self->_hmask;
    a_u32 i = hash & hmask;

    assume(self->_len < (hmask + 1) * ALO_DICT_LOAD_FACTOR);

    loop {
        DNode* node = &self->_ptr[i];
        if (node->_key == dead_key || node->_key == null) {
            node->_key = key;
            v_set(env, &node->_value, val);
            return;
        }
        hash >>= 5;
        i = (i * 5 + 1 + hash) & hmask;
    }
}

void ai_dict_hint(a_henv env, Dict* self, a_usize len) {
    a_usize need = self->_len + len;
    if (self->_ptr == null) {
        a_usize new_cap = ceil_pow2m1_usize(need) + 1;
        if (need >= new_cap * ALO_DICT_LOAD_FACTOR) {
            new_cap <<= 1;
        }

        DNode* new_ptr = ai_mem_vnew(env, DNode, new_cap);

        self->_ptr = new_ptr;
        self->_hmask = new_cap - 1;

        memclr(new_ptr, sizeof(DNode) * new_cap);
    }
    else {
        a_usize old_cap = self->_hmask + 1;
        a_usize new_cap = ceil_pow2m1_usize(need) + 1;
        if (need >= new_cap * ALO_DICT_LOAD_FACTOR) {
            new_cap <<= 1;
        }
        if (unlikely(old_cap < new_cap)) {
            DNode* old_ptr = self->_ptr;
            DNode* new_ptr = ai_mem_vnew(env, DNode, new_cap);

            self->_ptr = new_ptr;
            self->_hmask = new_cap - 1;

            memclr(new_ptr, sizeof(DNode) * new_cap);

            for (a_usize i = 0; i < old_cap; ++i) {
                DNode *node = &old_ptr[i];
                if (node->_key > dead_key) {
                    dict_put_inplace(env, self, node->_key, node->_value);
                }
            }

            ai_mem_vdel(G(env), old_ptr, old_cap);
        }
    }
}

a_msg ai_dict_uget(a_henv env, Dict* self, GStr* key, Value* pval) {
    a_u32 hash = key->_hash;
    a_u32 hmask = self->_hmask;
    a_u32 i = hash & hmask;

    if (self->_len == 0)
        return ALO_EEMPTY;

    loop {
        DNode* node = &self->_ptr[i];
        if (node->_key == key) {
            v_set(env, pval, node->_value);
            return ALO_SOK;
        }
        if (node->_key == null) {
            return ALO_EEMPTY;
        }
        hash >>= 5;
        i = (i * 5 + 1 + hash) & hmask;
    }
}

Value* ai_dict_uref(a_henv env, Dict* self, GStr* key, a_usize* pctx) {
    a_u32 hash = key->_hash;
    a_u32 hmask = self->_hmask;
    a_u32 i = hash & hmask;
    DNode* empty = null;

    if (self->_len == 0)
        goto empty;

    if (self->_len >= (hmask + 1) * ALO_DICT_LOAD_FACTOR)
        goto find;

    loop {
        DNode* node = &self->_ptr[i];
        if (node->_key == key) {
            return &node->_value;
        }
        if (node->_key == dead_key) {
            empty = node;
            goto find2;
        }
        if (node->_key == null) {
            goto empty;
        }
        hash >>= 5;
        i = (i * 5 + 1 + hash) & hmask;
    }

find:
    loop {
        DNode* node = &self->_ptr[i];
        if (node->_key == key) {
            return &node->_value;
        }
        if (node->_key == null) {
            goto empty;
        }
    find2:
        hash >>= 5;
        i = (i * 5 + 1 + hash) & hmask;
    }

empty:
    *pctx = addr_of(empty);
    return null;
}

a_msg ai_dict_uset(a_henv env, Dict* self, GStr* key, Value val, a_usize* pctx) {
    Value* pv = ai_dict_uref(env, self, key, pctx);
    if (pv != null) {
        v_set(env, pv, val);
        return ALO_SOK;
    }
    return ALO_EEMPTY;
}

a_msg ai_dict_uput(a_henv env, Dict* self, GStr* key, Value val, a_usize* pctx) {
    DNode* empty = ptr_of(DNode, *pctx);

    if (empty != null) {
        empty->_key = key;
        v_set(env, &empty->_value, val);
        self->_len += 1;
    }
    else {
        ai_dict_hint(env, self, 1);
        ai_dict_put_inplace(env, self, key, val);
    }

    return ALO_SOK;
}

void ai_dict_put_inplace(a_henv env, Dict* self, GStr* key, Value val) {
    dict_put_inplace(env, self, key, val);
    self->_len += 1;
}