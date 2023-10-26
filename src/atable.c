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

#if ALO_M64
# define MAX_BUCKET_CAPACITY (cast(a_usize, UINT32_MAX) + 1)
#elif ALO_M32
# define MAX_BUCKET_CAPACITY ceil_pow2_usize(UINT32_MAX / sizeof(TNode))
#endif

static VTable const table_vtable;

#define TKey Key
#define Node TNode
#define Dict GTable

#define key_hash(k) ((k)._hash)

#define node_is_empty(n) v_is_nil((n)->_key._value)
#define node_is_ended(n) v_is_strict_nil((n)->_key._value)
#define node_match(n,k) ai_vm_equals(env, (n)->_key._value, (k)._value)
#define node_init(n) memset(n, -1, sizeof(TNode))
#define node_erase(n) v_set_raw(&(n)->_key._value, v_of_empty())

#define at_dict_emplace table_emplace
#define at_dict_grow_amortized table_grow_amortized
#define at_dict_erase table_erase
#define at_dict_find table_find

static void at_dict_emplace(a_henv env, GTable* self, a_u32 index, Key key, Value value);
static a_bool at_dict_grow_amortized(a_henv env, Dict* self, a_usize addition);
static void at_dict_erase(a_henv env, Dict* self, a_u32 index);
static a_bool at_dict_find(a_henv env, Dict* self, TKey key, a_usize* pindex);

#include "adict.h"

GTable* ai_table_new(a_henv env) {
    GTable* self = ai_mem_alloc(env, sizeof(GTable));

	self->_vptr = &table_vtable;
    at_dict_init(self);
    self->_lfirst = tn_nokey;
    self->_llast = tn_nokey;

	ai_gc_register_object(env, self);
    return self;
}

static void table_emplace(a_henv env, GTable* self, a_u32 index, Key key, Value value) {
    TNode* node = &self->_ptr[index];
    v_set(env, &node->_key._value, key._value);
    node->_key._hash = key._hash;
    v_set(env, &node->_value, value);

    node->_lprev = self->_llast;
    node->_lnext = tn_nokey;

    if (self->_llast != tn_nokey) {
        self->_ptr[self->_llast]._lnext = index;
        self->_llast = index;
    }
    else {
        self->_lfirst = self->_llast = index;
    }
}

static a_bool table_grow_amortized(a_henv env, GTable* self, a_usize addition) {
    a_usize need;
    try(checked_add_usize(self->_len, addition, &need));
    if (unlikely(need >= MAX_BUCKET_CAPACITY)) return true;

    a_usize old_cap = (cast(a_usize, self->_hmask) + 1) & ~usizec(1);
    a_usize new_cap = ceil_pow2_usize(need);
    new_cap = max(new_cap, 4);
    if (need > new_cap * ALO_DICT_LOAD_FACTOR) new_cap <<= 1;
    new_cap = min(MAX_BUCKET_CAPACITY, new_cap);

    at_dict_resize(env, self, old_cap, new_cap);
    return false;
}

void ai_table_grow(a_henv env, GTable* self, a_usize len) {
    if (unlikely(at_dict_grow_amortized(env, self, len))) {
        ai_err_raisef(env, ALO_EINVAL, "table size too large.");
    }
}

static void table_erase(unused a_henv env, GTable* self, a_u32 index) {
    TNode* node = &self->_ptr[index];
    v_set_raw(&node->_key._value, v_of_empty());

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
}

static a_bool table_find_with_trivial_equality(GTable* self, Key key, a_usize* pindex) {
    if (self->_len == 0) {
        return false;
    }
    dict_for_hash(self, key._hash, index) {
        TNode* node = &self->_ptr[index];
        if (v_trivial_equals(node->_key._value, key._value)) {
            *pindex = index;
            return true;
        }
        if (v_is_nil(node->_key._value)) {
            return false;
        }
    }
}

static a_bool table_find_with_generic_equality(a_henv env, GTable* self, Key key, a_usize* pindex) {
    if (self->_len == 0) {
        return false;
    }
    dict_for_hash(self, key._hash, index) {
        TNode* node = &self->_ptr[index];
        if (ai_vm_equals(env, node->_key._value, key._value)) {
            *pindex = index;
            return true;
        }
        if (v_is_nil(node->_key._value)) {
            return false;
        }
    }
}

static void key_from_value_with_trivial_equality(Value v, Key* pk) {
    init(pk) { ._value = v, ._hash = v_trivial_hash(v) };
}

static a_bool key_from_value(a_henv env, Value v, Key* pk) {
    if (likely(v_has_trivial_hash(v))) {
        key_from_value_with_trivial_equality(v, pk);
        init(pk) { ._value = v, ._hash = v_trivial_hash(v) };
        return true;
    }
    else if (likely(v_is_str(v))) {
        init(pk) { ._value = v, ._hash = v_as_str(v)->_hash };
        return true;
    }
    else if (unlikely(v_is_float(v))) {
        if (isnan(v_as_float(v))) return false;
        v = v_float_key(v);
        init(pk) { ._value = v, ._hash = v_trivial_hash_unchecked(v) };
        return true;
    }
    else if (unlikely(v_is_nil(v))) {
        return false;
    }
    else {
        init(pk) { ._value = v, ._hash = ai_vm_hash(env, v) };
        return true;
    }
}

static a_bool table_find(a_henv env, GTable* self, Key key, a_usize* pindex) {
    if (likely(v_has_trivial_equals(key._value))) {
        return table_find_with_trivial_equality(self, key, pindex);
    }
    else {
        return table_find_with_generic_equality(env, self, key, pindex);
    }
}

static a_bool table_find_by_value(a_henv env, GTable* self, Value vk, Key* key, a_usize* pindex) {
    if (likely(v_has_trivial_equals(vk))) {
        if (v_is_nil(vk)) {
            return false;
        }
        init(key) { ._value = vk, ._hash = v_trivial_hash(vk) };
        return table_find_with_trivial_equality(self, *key, pindex);
    }
    else if (unlikely(v_is_float(vk))) {
        if (unlikely(v_is_nan(vk))) {
            return false;
        }
        init(key) { ._value = v_float_key(vk), ._hash = v_float_hash(vk) };
        return table_find_with_trivial_equality(self, *key, pindex);
    }
    else {
        init(key) { ._value = vk, ._hash = ai_vm_hash(env, vk) };
        return table_find_with_generic_equality(env, self, *key, pindex);
    }
}

Value ai_table_get(a_henv env, GTable* self, Value vk) {
    a_usize index;
    Key key;
	return table_find_by_value(env, self, vk, &key, &index) ? self->_ptr[index]._value : v_of_nil();
}

Value ai_table_gets(a_henv env, GTable* self, GStr* k) {
    a_usize index;
    Key key;
    key_from_value_with_trivial_equality(v_of_obj(k), &key);
	return table_find_with_trivial_equality(self, key, &index) ? self->_ptr[index]._value : v_of_nil();
}

void ai_table_set(a_henv env, GTable* self, Value vk, Value vv) {
    a_usize index;
    Key key;

    if (table_find_by_value(env, self, vk, &key, &index)) {
        v_set(env, &self->_ptr[index]._value, vv);
    }
    else {
        at_dict_put(env, self, key, vv);

        ai_gc_barrier_backward_val(env, self, vk);
        ai_gc_barrier_backward_val(env, self, vv);
    }
}

a_bool ai_table_del(a_henv env, GTable* self, Value vk) {
    Key key;
    return key_from_value(env, vk, &key) && at_dict_del(env, self, key);
}

a_msg ai_table_ugeti(a_henv env, GTable* self, a_int k, Value* pv) {
    a_usize index;
    Key key;

    key_from_value_with_trivial_equality(v_of_int(k), &key);

    if (!table_find_with_trivial_equality(self, key, &index))
        return ALO_EEMPTY;

    v_cpy(env, pv, &self->_ptr[index]._value);
    return ALO_SOK;
}

a_msg ai_table_uget(a_henv env, GTable* self, Value vk, Value* pv) {
    a_usize index;
    Key key;

    if (!table_find_by_value(env, self, vk, &key, &index))
        return ALO_EEMPTY;

    v_cpy(env, pv, &self->_ptr[index]._value);
    return ALO_SOK;
}

a_msg ai_table_uset(a_henv env, GTable* self, Value vk, Value vv) {
    a_usize index;
    Key key;

    if (table_find_by_value(env, self, vk, &key, &index)) {
        v_set(env, &self->_ptr[index]._value, vv);
        return ALO_SOK;
    }

    return ALO_EEMPTY;
}

static void table_drop(Global* g, GTable* self) {
    at_dict_deinit(g, self);
	ai_mem_dealloc(g, self, sizeof(GTable));
}

static void table_mark(Global* g, GTable* self) {
	if (self->_ptr != null) {
		for (a_u32 i = 0; i <= self->_hmask; ++i) {
			TNode* node = &self->_ptr[i];
			if (!v_is_nil(node->_key._value)) {
				ai_gc_trace_mark_val(g, node->_key._value);
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