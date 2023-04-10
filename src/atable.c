/**
 *@file atable.c
 */

#define atable_c_
#define ALO_LIB

#include "aop.h"
#include "astr.h"
#include "amem.h"
#include "agc.h"
#include "avm.h"
#include "aapi.h"

#include "atable.h"

#ifndef ALOI_TABLE_LOAD_FACTOR
# define ALOI_TABLE_LOAD_FACTOR 0.75
#endif

static VTable const table_vtable;

static void table_init_array(GTable* self, HNode* array, a_usize cap) {
	assume(array != null);

	self->_ptr = array;
	self->_hmask = cap - 1;
	self->_hfree = 0;

	self->_lfirst = self->_llast = 0;

	for (a_u32 i = 0; i < cap; ++i) {
		array[i]._key = v_of_nil();
	}

	map_init_links_template(self, _lprev, _lnext);
}

static void table_alloc_array(a_henv env, GTable* self, a_usize new_cap) {
	if (likely(new_cap > 0)) {
		HNode* array = ai_mem_vnew(env, HNode, new_cap);
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

	self->_vptr = &table_vtable;
    self->_len = 0;
	table_alloc_array(env, self, 0);

	ai_gc_register_object(env, self);
    return self;
}

static a_bool hnode_is_hhead(GTable* table, HNode* node) {
    return !hnode_is_empty(node) && node == map_hash_first(table, node->_hash);
}

static void hnode_emplace(a_henv env, HNode* node, Value key, a_hash hash, Value value) {
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
static void hnode_move(a_henv env, HNode* noded, HNode* nodes) {
	hnode_emplace(env, noded, nodes->_key, nodes->_hash, nodes->_value);
	link_move_template(nodes, noded, _lprev, _lnext);
}

/**
 ** Pop a node from free list.
 *@param self the table.
 *@return the free node.
 */
static HNode* table_pop_free(GTable* self) {
	HNode* node = list_pop_template(self, _hfree, _lprev, _lnext);
	assume(node != null && hnode_is_empty(node));
	return node;
}

/**
 ** Link node at end of linked list of table.
 *@param self the table.
 *@param node the node to be linked.
 */
static void table_link_tail(GTable* self, HNode* node) {
	link_push_template(self, node, _lfirst, _llast, _lprev, _lnext);
}

static HNode* table_get_hprev(GTable* self, HNode* node) {
	HNode* nodep = map_hash_first(self, node->_hash);
	HNode* noden;
	while ((noden = link_get(nodep, _hnext)) != node) {
		nodep = noden;
	}
	return nodep;
}

static void table_emplace(a_henv env, GTable* self, Value key, a_hash hash, Value value) {
    HNode* nodeh = map_hash_first(self, hash);
    if (hnode_is_hhead(self, nodeh)) {
        /* Insert into hash list. */
        HNode* nodet = table_pop_free(self);
		HNode* noden = link_get(nodeh, _hnext);

		/* Emplace node at another free node. */
        /*                       v
         * h -> n -> ... => h -> t -> n -> ...
         */
		hnode_emplace(env, nodet, key, hash, value);

		link_set(nodeh, _hnext, nodet);
		link_set(nodet, _hnext, noden);

		table_link_tail(self, nodet);
    }
    else {
		if (hnode_is_empty(nodeh)) {
#define Ff(n) list_set(self, _hfree, n)
			link_remove_local_template(nodeh, _lprev, _lnext, Ff, quiet);
#undef Ff
		}
		else {
			/* Move placed entry to other node. */
			/*                         v
			 * ... -> p -> h -> ... => h; ... -> p -> f -> ...
			 */
			HNode* nodef = table_pop_free(self);
			HNode* nodep = table_get_hprev(self, nodeh);
			hnode_move(env, nodef, nodeh);
			link_set(nodep, _hnext, nodef);
		}
		/* Emplace node locally. */
		/*    v
		 * => h
		 */
		hnode_emplace(env, nodeh, key, hash, value);
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

		HNode* arr = self->_ptr;
		HNode* node = list_first(self);

		table_alloc_array(env, self, new_cap);

		if (arr != null) {
			for (; node != null; node = link_next(node)) {
				table_emplace(env, self, node->_key, node->_hash, node->_value);
			}

			ai_mem_vdel(G(env), arr, old_cap);
		}
	}
}

static void tnode_remove(GTable* table, HNode* node) {
	assume(!hnode_is_empty(node), "cannot remove empty node.");

#define first(n) list_set(table, _lfirst, n)
#define last(n) list_set(table, _llast, n)
	link_remove_local_template(node, _lprev, _lnext, first, last);
#undef first
#undef last

	HNode* noden = link_next(node);
	if (hnode_is_hhead(table, node)) {
		if (noden != null) {
			HNode* noden2 = link_next(noden);
			link_set(noden, _hnext, noden2);
		}
	}
	else {
		HNode* nodep = table_get_hprev(table, node);
		link_set(nodep, _hnext, noden);
	}
}

static Value* table_find_id(unused a_henv env, GTable* self, a_hash hash, Value key) {
#define test(n) v_trivial_equals(key, (n)->_key)
#define con(n) ({ return &(n)->_value; })
	return map_find_template(self, hash, test, hnode_is_empty, con);
#undef test
#undef con
}

static Value* table_find_str(unused a_henv env, GTable* self, a_hash hash, a_lstr const* key) {
#define test(n) (v_is_str((n)->_key) && ai_str_requals(v_as_str((n)->_key), key->_ptr, key->_len))
#define con(n) ({ return &(n)->_value; })
	return map_find_template(self, hash, test, hnode_is_empty, con);
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

Value* ai_table_ref(a_henv env, GTable* self, Value key, a_u32* restrict phash) {
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
		*phash = v_trivial_hash(key);
		return table_find_id(env, self, *phash, key);
	}
	else if (likely(v_has_trivial_equals(key))) {
		*phash = v_trivial_hash(key);
		return table_find_id(env, self, *phash, key);
	}
	else {
		*phash = ai_vm_hash(env, key);

#define test(n) ai_vm_equals(env, key, (n)->_key)
#define con(n) ({ return &(n)->_value; })
		return map_find_template(self, *phash, test, hnode_is_empty, con);
#undef test
#undef con
	}
}

a_usize alo_hnext(a_henv env, a_isize id, a_ritr itr) {
	api_check_slot(env, 2);

	Value v = api_elem(env, id);
	api_check(v_is_table(v), "not table.");

	GTable* self = v_as_table(v);
	a_u32 cursor = itr[0];
	a_u32 pos;

	/* Get next position. */
	if (cursor == 0) {
		if (self->_len == 0)
			return 0;
		pos = self->_lfirst;
	}
	else {
		pos = ~cursor;

		HNode* prev = &self->_ptr[pos];
		api_check(!hnode_is_empty(prev), "invalid iterator.");
		if (is_nil(prev->_lnext))
			return 0;
		pos += unwrap(prev->_lnext);
		api_check(pos <= self->_hmask, "invalid iterator.");
	}

	itr[0] = ~pos; /* Store cursor. */

	HNode* node = &self->_ptr[pos];
	v_set(env, api_incr_stack(env), node->_key);
	v_set(env, api_incr_stack(env), node->_value);

	return 2;
}

a_bool alo_hremove(a_henv env, a_isize id, a_ritr itr) {
	Value v = api_elem(env, id);
	api_check(v_is_table(v), "not table.");

	GTable* self = v_as_table(v);
	a_u32 cursor = itr[0];

	api_check(cursor != 0, "no element need to remove.");

	HNode* node = &self->_ptr[~cursor];
	itr[0] = is_nil(node->_lprev) ? 0 : cursor - unwrap(node->_lprev);
	tnode_remove(self, node);
	self->_len -= 1;

	return true;
}

Value ai_table_getis(a_henv env, GTable* self, GStr* key) {
	assume(g_is_istr(key), "not short string.");
	Value const* v = table_find_id(env, self, key->_hash, v_of_obj(key));
	return v != null ? *v : v_of_nil();
}

Value ai_table_get(a_henv env, GTable* self, Value key) {
	a_u32 hash;
	Value const* value = ai_table_ref(env, self, key, &hash);
	return value != null ? *value : v_of_nil();
}

void ai_table_set(a_henv env, GTable* self, Value key, Value value) {
	a_hash hash;
	Value* ref = ai_table_ref(env, self, key, &hash);
	if (ref != null) {
		v_set(env, ref, value);
		ai_gc_barrier_backward_val(env, self, value);
	}
	else {
		ai_table_hint(env, self, 1);
		ai_table_put(env, self, key, hash, value);
	}
}

void ai_table_put(a_henv env, GTable* self, Value key, a_hash hash, Value value) {
	assume(self->_ptr != null && self->_len + 1 <= (self->_hmask + 1) * ALOI_TABLE_LOAD_FACTOR, "need hint before emplace.");
	table_emplace(env, self, key, hash, value);
	ai_gc_barrier_backward_val(env, self, key);
	ai_gc_barrier_backward_val(env, self, value);
	self->_len += 1;
}

void ai_table_drop(Global* g, GTable* self) {
	if (self->_ptr != null) {
		ai_mem_vdel(g, self->_ptr, self->_hmask + 1);
	}
	ai_mem_dealloc(g, self, sizeof(GTable));
}

void ai_table_mark(Global* g, GTable* self) {
    a_usize len = self->_ptr != null ? self->_hmask + 1 : 0;
    for (a_usize i = 0; i < len; ++i) {
        HNode* node = &self->_ptr[i];
        if (!hnode_is_empty(node)) {
			ai_gc_trace_mark_val(g, node->_key);
			ai_gc_trace_mark_val(g, node->_value);
        }
    }
	ai_gc_trace_work(g, sizeof(HNode) * len);
}

static VTable const table_vtable = {
	._mask = V_MASKED_TAG(T_TABLE),
	._iname = env_type_iname(_table),
	._sname = "table",
	._base_size = sizeof(GTable),
	._elem_size = 0,
	._flags = VTABLE_FLAG_NONE,
	._vfps = (a_vslot[]) {
		vfp_def(drop, ai_table_drop),
		vfp_def(mark, ai_table_mark)
	}
};