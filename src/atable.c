/**
 *@file atable.c
 */

#define atable_c_
#define ALO_LIB

#include "aop.h"
#include "astr.h"
#include "amem.h"
#include "agc.h"
#include "aerr.h"
#include "avm.h"
#include "aapi.h"

#include "atable.h"

static VTable const table_vtable;

#define MAX_BUCKET_CAPACITY ((UINT32_MAX >> 1) + 1)

GTable* ai_table_new(a_henv env) {
    GTable* self = ai_mem_alloc(env, sizeof(GTable));

	self->_vptr = &table_vtable;
	self->_ptr = null;
	self->_len = 0;
	self->_hmask = 0;
    self->_lfirst = tn_nokey;
    self->_llast = tn_nokey;

	ai_gc_register_object(env, self);
    return self;
}

#define table_hfor(self,hash,node) \
    for (a_u32 _index = (hash) & (self)->_hmask, _perturb = _index; \
            (node) = &(self)->_ptr[_index], true; \
            _perturb >>= 5, _index = (_index * 5 + 1 + _perturb) & (self)->_hmask)

static void table_put_inplace(a_henv env, GTable* self, Value vk, a_hash hash, Value vv) {
    TNode* node;
    assume(self->_ptr != null, "table is empty.");

    table_hfor(self, hash, node) {
        if (v_is_nil(node->_key)) {
            v_set(env, &node->_key, vk);
            v_set(env, &node->_value, vv);
            node->_hash = hash;

            node->_lprev = self->_llast;
            node->_lnext = tn_nokey;

            if (self->_llast != tn_nokey) {
                self->_ptr[self->_llast]._lnext = _index;
                self->_llast = _index;
            }
            else {
                self->_lfirst = self->_llast = _index;
            }
            break;
        }
    }
}

static void table_resize(a_henv env, GTable* self, a_usize new_cap) {
    assume(new_cap <= MAX_BUCKET_CAPACITY);

    TNode* new_ptr = ai_mem_vnew(env, TNode, new_cap);
    TNode* old_ptr = self->_ptr;

    a_usize old_cap = self->_hmask + 1;

    self->_hmask = new_cap - 1;
    self->_ptr = new_ptr;

    memset(new_ptr, -1, sizeof(TNode) * new_cap); /* Set all fields to nil. */

    a_u32 old_lfirst = self->_lfirst;

    self->_lfirst = self->_llast = tn_nokey;

    if (old_ptr != null) {
        for (a_u32 id = old_lfirst; id != tn_nokey;) {
            TNode* node = &old_ptr[id];
            table_put_inplace(env, self, node->_key, node->_hash, node->_value);
            id = node->_lnext;
        }

        ai_mem_vdel(G(env), old_ptr, old_cap);
    }
}

static a_bool table_hint(a_henv env, GTable* self, a_usize len) {
    if (len == 0) return false;

    a_usize old_cap = self->_ptr != null ? self->_hmask + 1 : 0;
    a_usize need;

    try(checked_add_usize(self->_len, len, &need));
    if (unlikely(need >= MAX_BUCKET_CAPACITY / 4 * 3)) return true;

    if (need <= old_cap * 3 / 4) return false;

    a_usize new_cap = ceil_pow2m1_usize(need * 4 / 3) + 1;
    new_cap = max(new_cap, 4);

    table_resize(env, self, new_cap);
    return false;
}

static void table_hint_failed(a_henv env) {
    ai_err_raisef(env, ALO_EINVAL, "too many elements.");
}

void ai_table_hint(a_henv env, GTable* self, a_usize len) {
    if (unlikely(table_hint(env, self, len))) {
        table_hint_failed(env);
    }
}

static void table_unlink(GTable* self, TNode* node) {
    v_set_raw(&node->_key, v_of_empty());

    if (node->_lprev != tn_nokey) {
        self->_ptr[node->_lprev]._lnext = node->_lnext;
    }
    else {
        self->_lfirst = node->_lnext;
    }

    if (node->_lnext != tn_nokey) {
        node[node->_lnext]._lprev = node->_lprev;
    }
    else {
        self->_llast = node->_lprev;
    }

	self->_len -= 1;
}

static Value* table_ref_id(unused a_henv env, GTable* self, a_hash hash, Value vk) {
    TNode* node;
    if (self->_len == 0) return null;
    table_hfor(self, hash, node) {
        if (v_trivial_equals(vk, node->_key)) {
            return &node->_value;
        }
        else if (v_is_strict_nil(node->_key)) {
            return null;
        }
    }
}

static Value* table_ref_str(unused a_henv env, GTable* self, a_hash hash, a_lstr const* k) {
    TNode* node;
    if (self->_len == 0) return null;
    table_hfor(self, hash, node) {
        if (v_is_str(node->_key) && v_as_str(node->_key)->_hash == hash &&
                ai_str_requals(v_as_str(node->_key), k->_ptr, k->_len)) {
            return &node->_value;
        }
        else if (v_is_strict_nil(node->_key)) {
            return null;
        }
    }
}

static Value* table_ref_any(a_henv env, GTable* self, a_hash hash, Value vk) {
    TNode* node;
    if (self->_len == 0) return null;
    table_hfor(self, hash, node) {
        if (ai_vm_equals(env, vk, node->_key)) {
            return &node->_value;
        }
        else if (v_is_strict_nil(node->_key)) {
            return null;
        }
    }
}

static Value const* table_refi(a_henv env, GTable* self, a_int k) {
    return table_ref_id(env, self, v_trivial_hash(v_of_int(k)), v_of_int(k));
}

static Value const* table_refls(a_henv env, GTable* self, a_lstr const* k) {
    return table_ref_str(env, self, ai_str_hashof(env, k->_ptr, k->_len), k);
}

static Value* table_ref(a_henv env, GTable* self, Value vk, a_u32* restrict phash) {
	if (likely(v_has_trivial_equals(vk))) {
		*phash = v_trivial_hash(vk);
		return table_ref_id(env, self, *phash, vk);
	}
	else if (unlikely(v_is_float(vk))) {
		if (unlikely(v_is_nan(vk))) {
			return null;
		}
		*phash = v_float_hash(vk);
		return table_ref_id(env, self, *phash, v_float_key(vk));
	}
	else {
		*phash = ai_vm_hash(env, vk);
		return table_ref_any(env, self, *phash, vk);
	}
}

Value ai_table_get(a_henv env, GTable* self, Value vk) {
	a_u32 hash;
	Value const* value = table_ref(env, self, vk, &hash);
	return value != null ? *value : v_of_nil();
}

Value ai_table_gets(a_henv env, GTable* self, GStr* k) {
	Value const* v = table_ref_id(env, self, k->_hash, v_of_obj(k));
	return v != null ? *v : v_of_nil();
}

void ai_table_set(a_henv env, GTable* self, Value vk, Value vv) {
	a_hash hash;
	Value* pv = table_ref(env, self, vk, &hash);
	if (pv != null) {
		v_set(env, pv, vv);
		ai_gc_barrier_backward_val(env, self, vv);
	}
	else {
		ai_table_hint(env, self, 1);
		ai_table_put(env, self, vk, hash, vv);
	}
}

void ai_table_put(a_henv env, GTable* self, Value vk, a_hash hash, Value vv) {
	assume(self->_ptr != null && self->_len + 1 <= (self->_hmask + 1) * 3 / 4, "need hint before emplace.");
    table_put_inplace(env, self, vk, hash, vv);
	ai_gc_barrier_backward_val(env, self, vk);
	ai_gc_barrier_backward_val(env, self, vv);
	self->_len += 1;
}

a_bool ai_table_del(a_henv env, GTable* self, Value vk) {
    a_hash hash;
    Value* pv = table_ref(env, self, vk, &hash);
    if (pv == null) return false;
    table_unlink(self, from_member(TNode, _value, pv));
    return true;
}

a_msg ai_table_ugeti(a_henv env, GTable* self, a_int k, Value* pv) {
    Value const* psrc = table_refi(env, self, k);
    if (psrc == null) return ALO_EEMPTY;
    v_cpy(env, pv, psrc);
    return ALO_SOK;
}

a_msg ai_table_uget(a_henv env, GTable* self, Value vk, Value* pv) {
    a_hash hash;
    Value* psrc = table_ref(env, self, vk, &hash);
    if (psrc == null) return ALO_EEMPTY;
    v_cpy(env, pv, psrc);
    return ALO_SOK;
}

a_msg ai_table_uset(a_henv env, GTable* self, Value vk, Value vv) {
    a_hash hash;
    Value* pdst = table_ref(env, self, vk, &hash);

    if (pdst == null) {
        ai_table_put(env, self, vk, hash, vv);
        return ALO_SOK;
    }

    v_set(env, pdst, vv);
    ai_gc_barrier_backward_val(env, self, vv);
    return ALO_SOK;
}

static void table_drop(Global* g, GTable* self) {
	if (self->_ptr != null) {
        ai_mem_vdel(g, self->_ptr, self->_hmask + 1);
	}
	ai_mem_dealloc(g, self, sizeof(GTable));
}

static void table_mark(Global* g, GTable* self) {
	if (self->_ptr != null) {
		for (a_u32 i = 0; i <= self->_hmask; ++i) {
			TNode* node = &self->_ptr[i];
			if (!v_is_nil(node->_key)) {
				ai_gc_trace_mark_val(g, node->_key);
				ai_gc_trace_mark_val(g, node->_value);
			}
		}
		ai_gc_trace_work(g, sizeof(TNode) * (self->_hmask + 1));
	}
	ai_gc_trace_work(g, sizeof(GTable));
}

static VTable const table_vtable = {
	._stencil = V_STENCIL(T_TABLE),
	._type_ref = g_type_ref(_table),
    ._flags = VTABLE_FLAG_NONE,
	._slots = {
        [vfp_slot(drop)] = table_drop,
        [vfp_slot(mark)] = table_mark
	}
};