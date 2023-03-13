/**
 *@file atable.c
 */

#define atable_c_
#define ALO_LIB

#include <math.h>
#include <string.h>

#include "aop.h"
#include "astr.h"
#include "amem.h"
#include "agc.h"
#include "avm.h"
#include "aerr.h"

#include "atable.h"

#ifndef ALOI_TABLE_LOAD_FACTOR
# define ALOI_TABLE_LOAD_FACTOR 0.75
#endif

static VTable const table_vtable;

void table_init_array(GTable* self, TNode* array, a_usize cap) {
	assume(array != null);

	self->_ptr = array;
	self->_hmask = cap - 1;
	self->_hfree = 0;

	self->_lfirst = self->_llast = 0;

	for (a_u32 i = 0; i < cap; ++i) {
		array[i]._key = v_of_empty();
	}

	map_init_links_template(self, _lprev, _lnext);
}

static void table_alloc_array(a_henv env, GTable* self, a_usize new_cap) {
	if (likely(new_cap > 0)) {
		TNode* array = ai_mem_vnew(env, TNode, new_cap);
		table_init_array(self, array, new_cap);
	}
	else {
		self->BUF_PTR_REF = null;
		self->_hmask = 0;
		self->_hfree = 0;
	}
}

GTable* ai_table_new(a_henv env) {
    GTable* self = ai_mem_alloc(env, sizeof(GTable));
	self->_vtable = &table_vtable;
    self->_len = 0;
	table_alloc_array(env, self, 0);
    return self;
}

static a_bool tnode_is_hhead(GTable* table, TNode* node, a_hash hash) {
    return !tnode_is_empty(node) && ((node->_hash ^ hash) & table->_hmask) == 0;
}

static void tnode_emplace(a_henv env, TNode* node, Value key, a_hash hash, Value value) {
    v_set(env, &node->_key, key);
    v_set(env, &node->_value, value);
    node->_hash = hash;
}

/**
 ** Move entry and linked list part data, the hash part data will be ignored.
 **
 *@param env the runtime environment.
 *@param noded the destination node.
 *@param nodes the source node.
 */
static void tnode_move(a_henv env, TNode* noded, TNode* nodes) {
	tnode_emplace(env, noded, nodes->_key, nodes->_hash, nodes->_value);
	link_move_template(nodes, noded, _lprev, _lnext);
}

/**
 ** Pop a node from free list.
 *@param self the table.
 *@return the free node.
 */
static TNode* table_pop_free(GTable* self) {
	TNode* node = list_pop_template(self, _hfree, _lprev, _lnext);
	assume(node != null && tnode_is_empty(node));
	return node;
}

/**
 ** Link node at end of linked list of table.
 *@param self the table.
 *@param node the node to be linked.
 */
static void table_link_tail(GTable* self, TNode* node) {
	link_push_template(self, node, _lfirst, _llast, _lprev, _lnext);
}

static TNode* table_get_hprev(GTable* self, TNode* node) {
	TNode* nodep = map_hash_first(self, node->_hash);
	TNode* noden;
	while ((noden = link_get(nodep, _hnext)) != node) {
		nodep = noden;
	}
	return nodep;
}

static void table_emplace(a_henv env, GTable* self, Value key, a_hash hash, Value value) {
    TNode* nodeh = map_hash_first(self, hash);
    if (tnode_is_hhead(self, nodeh, hash)) {
        /* Insert into hash list. */
        TNode* nodet = table_pop_free(self);
		TNode* noden = link_get(nodeh, _hnext);

		/* Emplace node at another free node. */
        /*                       v
         * h -> n -> ... => h -> t -> n -> ...
         */
		tnode_emplace(env, nodet, key, hash, value);

		link_set(nodeh, _hnext, nodet);
		link_set(nodet, _hnext, noden);

		table_link_tail(self, nodet);
    }
    else {
		if (tnode_is_empty(nodeh)) {
#define Ff(n) list_set(self, _hfree, n)
			link_remove_local_template(nodeh, _lprev, _lnext, Ff, quiet);
#undef Ff
		}
		else {
			/* Move placed entry to other node. */
			/*                         v
			 * ... -> p -> h -> ... => h; ... -> p -> f -> ...
			 */
			TNode* nodef = table_pop_free(self);
			TNode* nodep = table_get_hprev(self, nodeh);
			tnode_move(env, nodef, nodeh);
			link_set(nodep, _hnext, nodef);
		}
		/* Emplace node locally. */
		/*    v
		 * => h
		 */
		tnode_emplace(env, nodeh, key, hash, value);
        nodeh->_hnext = nil;
		table_link_tail(self, nodeh);
    }
}

void ai_table_hint(a_henv env, GTable* self, a_usize len) {
    a_usize old_cap = self->_hmask + 1;
    a_usize expect = self->_len + len;
    if (expect > cast(a_usize, old_cap * ALOI_TABLE_LOAD_FACTOR)) {
		a_usize new_cap = ceil_pow2m1_usize(cast(a_usize, ceil(expect / ALOI_TABLE_LOAD_FACTOR))) + 1;
		assume(expect <= new_cap && new_cap > 0);

		TNode* arr = self->_ptr;
		TNode* node = list_first(self);

		table_alloc_array(env, self, new_cap);

		if (arr != null) {
			for (; node != null; node = link_next(node)) {
				table_emplace(env, self, node->_key, node->_hash, node->_value);
			}

			ai_mem_vdel(G(env), arr, old_cap);
		}
	}
}

static Value* table_find_id(unused a_henv env, GTable* self, a_hash hash, Value key) {
#define test(n) v_trivial_equals(key, (n)->_key)
#define con(n) return &(n)->_value
	return map_find_template(self, hash, test, tnode_is_empty, con);
#undef test
#undef con
}

static Value* table_find_str(unused a_henv env, GTable* self, a_hash hash, a_lstr const* key) {
#define test(n) (v_is_str((n)->_key) && ai_str_requals(v_as_str((n)->_key), key->_ptr, key->_len))
#define con(n) return &(n)->_value
	return map_find_template(self, hash, test, tnode_is_empty, con);
#undef test
#undef con
}

Value const* ai_table_refi(a_henv env, GTable* self, a_int key) {
    return table_find_id(env, self, v_trivial_hash(v_of_int(key)), v_of_int(key));
}

Value const* ai_table_refs(a_henv env, GTable* self, a_lstr const* key) {
    return table_find_str(env, self, ai_str_hashof(G(env)->_seed, key->_ptr, key->_len), key);
}

Value const* ai_table_refis(a_henv env, GTable* self, GStr* key) {
	return table_find_id(env, self, key->_hash, v_of_obj(key));
}

static Value* table_get_opt(a_henv env, GTable* self, Value key, a_u32* phash) {
	if (likely(v_is_istr(key))) {
		GStr* str = v_as_str(key);
		*phash = str->_hash;
		return table_find_id(env, self, str->_hash, key);
	}
	else if (likely(v_is_hstr(key))) {
		GStr* str = v_as_str(key);
		*phash = str->_hash;
		a_lstr lstr = { ._ptr = str2ntstr(str), ._len = str->_len };
		return table_find_str(env, self, str->_hash, &lstr);
	}
	else if (unlikely(v_is_float(key))) {
		if (unlikely(v_is_nan(key))) {
			return null;
		}
		return table_find_id(env, self, v_trivial_hash(key), key);
	}
	else if (likely(v_has_trivial_equals(key))) {
		return table_find_id(env, self, v_trivial_hash(key), key);
	}
	else {
		GObj* obj = v_as_obj(key);

		a_fp_equals equals_fp = obj->_vtable->_equals;
		a_fp_hash hash_fp = obj->_vtable->_hash;

		a_u32 hash = hash_fp != null ? (*hash_fp)(env, obj) : v_trivial_hash(key);

#define test(n) (*equals_fp)(env, obj, (n)->_key)
#define con(n) return &(n)->_value
		return map_find_template(self, hash, test, tnode_is_empty, con);
#undef test
#undef con
	}
}

Value const* ai_table_ref(a_henv env, GTable* self, Value key) {
	a_u32 hash;
	return table_get_opt(env, self, key, &hash);
}

Value ai_table_getis(a_henv env, GTable* self, GStr* key) {
	assume(g_is_istr(gobj_cast(key)));
	Value const* v = table_find_id(env, self, key->_hash, v_of_obj(key));
	return v != null ? *v : v_of_nil();
}

Value ai_table_get(a_henv env, GTable* self, Value key) {
	a_u32 hash;
	Value const* value = table_get_opt(env, self, key, &hash);
	return value != null ? *value : v_of_nil();
}

void ai_table_set(a_henv env, GTable* self, Value key, Value value) {
	a_u32 hash;
	Value* ref = table_get_opt(env, self, key, &hash);
	if (ref != null) {
		v_set(env, ref, value);
		ai_gc_barrier_backward_val(env, self, value);
	}
	else {
		ai_table_hint(env, self, 1);
		table_emplace(env, self, key, hash, value);
		ai_gc_barrier_backward_val(env, self, key);
		ai_gc_barrier_backward_val(env, self, value);
		self->_len += 1;
	}
}

void ai_table_emplaces(a_henv env, GTable* self, GStr* key, Value value) {
	assume(self->_len < (self->_hmask + 1) * ALOI_TABLE_LOAD_FACTOR, "cannot emplace entry.");
	table_emplace(env, self, v_of_obj(key), key->_hash, value);
}

static void table_mark(Global* g, GTable* self) {
    a_usize len = self->_ptr != null ? self->_hmask + 1 : 0;
    for (a_usize i = 0; i < len; ++i) {
        TNode* node = &self->_ptr[i];
        if (!tnode_is_empty(node)) {
			ai_gc_trace_mark_val(g, node->_key);
			ai_gc_trace_mark_val(g, node->_value);
        }
    }
	ai_gc_trace_work(g, sizeof(GTable) + sizeof(TNode) * len);
}

static void table_drop(Global* g, GTable* self) {
	if (self->_ptr != null) {
		ai_mem_vdel(g, self->_ptr, self->_hmask + 1);
	}
    ai_mem_dealloc(g, self, sizeof(GTable));
}

static VTable const table_vtable = {
	._val_mask = V_MASKED_TAG(T_TABLE),
	._api_tag = ALO_TTABLE,
	._repr_id = REPR_TABLE,
	._flags = VTABLE_FLAG_PLAIN_LEN,
	._name = "table",
	._mark = fpcast(a_fp_mark, table_mark),
	._drop = fpcast(a_fp_drop, table_drop)
};