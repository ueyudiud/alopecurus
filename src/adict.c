/**
 *@file adict.c
 */

#define adict_c_
#define ALO_LIB

#include "agbl.h"
#include "amem.h"
#include "agc.h"

#include "adict.h"

#define dnode_is_empty(node) ((node)->_key == null)

#define dict_first(dict,key) (&(dict)->_arr[((key)->_hash & (dict)->_mask)])

#define ALOI_DICT_LOAD_FACTOR 0.75

static a_u32 dict_soft_grow_size(a_u32 cap) {
	return min(cast(a_u32, cap / ALOI_DICT_LOAD_FACTOR), 4);
}

void ai_dict_close(a_henv env, Dict* dict) {
	if (dict->_arr != null) {
		ai_mem_vdel(G(env), dict->_arr, dict->_mask + 1);
	}
}

static DNode* dnode_prev(Dict* dict, DNode* node) {
	DNode* nodep = dict_first(dict, node->_key);
	loop {
		DNode* noden = nodep + unwrap(nodep->_next);
		if (noden == node)
			return nodep;
		nodep = noden;
	}
}

static DNode* dict_next_free(Dict* restrict dict, a_u32* restrict hfree) {
	a_u32 pos;
	DNode* node;
	do {
		pos = *hfree++;
		assume(pos <= dict->_mask);
		node = &dict->_arr[pos];
	}
	while (!dnode_is_empty(node));
	return node;
}

static void dict_insert(Dict* restrict dict, GStr* key, a_u32 index, a_u32* restrict hfree) {
	DNode* node = dict_first(dict, key);
	if (dnode_is_empty(node)) {
		*node = new(DNode) {
			._key = key,
			._index = index,
			._next = nil
		};
	}
	else if (node == dict_first(dict, node->_key)) {
		DNode* node2 = dict_next_free(dict, hfree);
		*node2 = new(DNode) {
			._key = key,
			._index = index,
			._next = !is_nil(node->_next) ? wrap(node + unwrap(node->_next) - node2) : nil
		};
		node->_next = wrap(node2 - node);
	}
	else {
		DNode* node2 = dict_next_free(dict, hfree);
		DNode* nodep = dnode_prev(dict, node);
		nodep->_next = wrap(nodep - node2);
		*node2 = new(DNode) {
			._key = node->_key,
			._index = node->_index,
			._next = !is_nil(node->_next) ? wrap(node + unwrap(node->_next) - node2) : nil
		};
		*node = new(DNode) {
			._key = key,
			._index = index,
			._next = nil
		};
	}
}

static void dict_grow(a_henv env, Dict* restrict dict, a_u32* restrict hfree) {
	a_u32 old_cap = dict->_arr != null ? dict->_mask + 1 : 0;
	a_u32 new_cap = dict_soft_grow_size(old_cap);
	DNode* old_arr = dict->_arr;
	DNode* new_arr = ai_mem_vnew(env, DNode, new_cap);

	dict->_arr = new_arr;
	dict->_mask = new_cap - 1;
	*hfree = 0;

	if (dict->_len > 0) {
		for (a_u32 i = 0; i <= dict->_mask; ++i) {
			DNode* node = &old_arr[i];
			if (!dnode_is_empty(node)) {
				dict_insert(dict, node->_key, node->_index, hfree);
			}
		}
	}
	ai_mem_vdel(G(env), old_arr, old_cap);
}

a_u32 ai_dict_push_in_place(Dict* dict, GStr* key, a_u32* hfree) {
	assume(dict->_len <= dict->_mask && dict->_arr != null);
	a_u32 index = dict->_len++;
	dict_insert(dict, key, index, hfree);
	return index;
}

a_u32 ai_dict_push(a_henv env, Dict* dict, GStr* key, a_u32* hfree) {
	if (dict->_arr == null || dict->_len >= (dict->_mask + 1) * ALOI_DICT_LOAD_FACTOR) {
		dict_grow(env, dict, hfree);
	}

	a_u32 index = dict->_len++;
	dict_insert(dict, key, index, hfree);
	return index;
}

a_u32 const* ai_dict_find(Dict* dict, GStr* key) {
	DNode* node = dict_first(dict, key);

	if (dnode_is_empty(node))
		return null;

	loop {
		if (ai_str_equals(key, node->_key))
			return &node->_index;
		if (is_nil(node->_next))
			return null;
		node += unwrap(node->_next);
	}
}

void ai_dict_splash(Global* g, Dict* dict) {
	for (a_u32 i = 0; i <= dict->_mask; ++i) {
		GStr* key = dict->_arr[i]._key;
		if (key != null) {
			ai_gc_trace_mark(g, key);
		}
	}
}
