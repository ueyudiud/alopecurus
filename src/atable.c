/**
 *@file atable.c
 */

#define atable_c_
#define ALO_LIB

#include <math.h>
#include <string.h>

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
static VTable const mod_vtable;

static a_u32 wrap_abs(GTable* self, TNode* node) {
	return node != null ? cast(a_u32, node - self->BUF_PTR_REF) : 0;
}

static TNode* unwrap_abs(GTable* self, a_u32 id) {
	return &self->BUF_PTR_REF[id];
}

static void table_init_array(GTable* self, TNode* array, a_usize cap) {
	assume(array != null);

	self->_arr = array;
	self->_hmask = cap - 1;
	self->_hfree = 0;

	self->_lhead = new(LHead) { ._first = 0, ._last = 0 };

	for (a_u32 i = 0; i < cap; ++i) {
		array[i] = new(TNode) {
				._key = v_of_empty(),
				._hnext = nil,
				._link = new(Link) {
						._prev = i > 0 ? x32c(-1) : nil,
						._next = i < cap - 1 ? x32c(1) : nil
				}
		};
	}
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

static a_bool tnode_is_empty(TNode* node) {
	return v_is_empty(node->_key);
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
	ai_link_move(nodes, noded);
}

/**
 ** Pop a node from free list.
 *@param self the table.
 *@return the free node.
 */
static TNode* table_pop_free(GTable* self) {
	TNode* node = unwrap_abs(self, self->_hfree);
	assume(tnode_is_empty(node));
	TNode* noden = ai_link_next(node);
	if (noden != null) {
		noden->_link._prev = nil;
		self->_hfree = wrap_abs(self, noden);
	}
	return node;
}

/**
 ** Link node at end of linked list of table.
 *@param self the table.
 *@param node the node to be linked.
 */
static void table_link_tail(GTable* self, TNode* node) {
	ai_link_push_last(self, node, self->_len > 0);
}

static TNode* table_get_hprev(GTable* self, TNode* node) {
	TNode* nodep = unwrap_abs(self, node->_hash & self->_hmask);
	TNode* noden;
	while ((noden = ai_link_unwrap(nodep, nodep->_hnext)) != node) {
		nodep = noden;
	}
	return nodep;
}

static TNode* table_emplace(a_henv env, GTable* self, Value key, a_hash hash, Value value) {
    TNode* nodeh = unwrap_abs(self, hash & self->_hmask);
    if (tnode_is_hhead(self, nodeh, hash)) {
        /* Insert into hash list. */
        TNode* nodet = table_pop_free(self);
		TNode* noden = ai_link_unwrap(nodeh, nodeh->_hnext);

		/* Emplace node at another free node. */
        /*                       v
         * h -> n -> ... => h -> t -> n -> ...
         */
		tnode_emplace(env, nodet, key, hash, value);

		nodet->_hnext = ai_link_wrap(nodet, noden);
		nodeh->_hnext = ai_link_wrap(nodeh, nodet);

		table_link_tail(self, nodet);
		return nodet;
    }
    else {
		if (!tnode_is_empty(nodeh)) {
			/* Move placed entry to other node. */
			/*                         v
			 * ... -> p -> h -> ... => h; ... -> p -> f -> ...
			 */
			TNode* nodef = table_pop_free(self);
			TNode* nodep = table_get_hprev(self, nodeh);
			tnode_move(env, nodef, nodeh);
			nodep->_hnext = ai_link_wrap(nodep, nodef);
		}
		/* Emplace node locally. */
		/*    v
		 * => h
		 */
		tnode_emplace(env, nodeh, key, hash, value);
        nodeh->_hnext = nil;
		table_link_tail(self, nodeh);
		return nodeh;
    }
}

void ai_table_hint(a_henv env, GTable* self, a_usize len) {
    a_usize old_cap = self->_hmask + 1;
    a_usize expect = self->_len + len;
    if (expect > cast(a_usize, old_cap * ALOI_TABLE_LOAD_FACTOR)) {
		a_usize new_cap = ceil_pow2m1_usize(cast(a_usize, ceil(expect / ALOI_TABLE_LOAD_FACTOR))) + 1;
		assume(expect <= new_cap && new_cap > 0);

		TNode* arr = self->_arr;
		TNode* node = ai_link_first(self);

		table_alloc_array(env, self, new_cap);

		if (arr != null) {
			for (; node != null; node = ai_link_next(node)) {
				table_emplace(env, self, node->_key, node->_hash, node->_value);
			}

			ai_mem_vdel(G(env), arr + 1, old_cap);
		}
	}
}

typedef a_bool (*Pred)(a_henv env, a_usize ctx, Value v);

static Value* table_find(a_henv env, GTable* self, a_hash hash, Pred pred, a_usize ctx) {
	if (likely(self->_len > 0)) {
		TNode* node = unwrap_abs(self, hash & self->_hmask);
		if (!tnode_is_empty(node) && tnode_is_hhead(self, node, hash)) {
			do {
				if (node->_hash == hash && (*pred)(env, ctx, node->_key))
					return &node->_value;
			}
			while ((node = ai_link_unwrap(node, node->_hnext)) != null);
		}
	}
    return null;
}

static a_bool pred_id(unused a_henv env, a_usize ctx, Value vk) {
	return v_trivial_equals(new(Value) { ctx }, vk);
}

static Value* table_find_id(a_henv env, GTable* self, a_hash hash, Value key) {
	return table_find(env, self, hash, pred_id, key._);
}

static a_bool pred_str(unused a_henv env, a_usize ctx, Value vk) {
	a_lstr const* key = ptr_of(a_lstr, ctx);
	if (v_is_str(vk)) {
		GStr* str = v_as_str(vk);
		return key->_len == str->_len && memcmp(key->_ptr, str->_data, key->_len) == 0;
	}
	return false;
}

static Value* table_find_str(a_henv env, GTable* self, a_hash hash, a_lstr const* key) {
	return table_find(env, self, hash, pred_str, addr_of(key));
}

Value const* ai_table_refi(a_henv env, GTable* self, a_int key) {
    return table_find_id(env, self, v_trivial_hash(v_of_int(key)), v_of_int(key));
}

Value const* ai_table_refs(a_henv env, GTable* self, a_lstr const* key) {
    return table_find_str(env, self, ai_str_hashof(G(env)->_seed, key->_ptr, key->_len), key);
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
		Pred pred = fpcast(Pred, equals_fp);
		a_usize ctx = addr_of(v_as_obj(key));
		return table_find(env, self, hash, pred, ctx);
	}
}

Value const* ai_table_ref(a_henv env, GTable* self, Value key) {
	a_u32 hash;
	return table_get_opt(env, self, key, &hash);
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

static void table_mark(Global* g, GTable* self) {
    a_usize len = self->_arr != null ? self->_hmask + 1 : 0;
    for (a_usize i = 0; i < len; ++i) {
        TNode* node = &self->_arr[i];
        if (!tnode_is_empty(node)) {
			ai_gc_trace_mark_val(g, node->_key);
			ai_gc_trace_mark_val(g, node->_value);
        }
    }
	ai_gc_trace_work(g, sizeof(GTable) + sizeof(TNode) * len);
}

static void table_drop(Global* g, GTable* self) {
	if (self->_arr != null) {
		ai_mem_vdel(g, self->_arr, self->_hmask + 1);
	}
    ai_mem_dealloc(g, self, sizeof(GTable));
}

static Value table_get(a_henv env, GTable* self, Value key) {
	a_u32 hash;
	Value const* value = table_get_opt(env, self, key, &hash);
	return value != null ? *value : v_of_nil();
}

static a_usize mod_capacity_hint(a_usize len) {
	return ceil_pow2m1_usize(len * 6 / 5) + 1;
}

static a_usize mod_size(a_usize cap) {
	return sizeof(GMod) + sizeof(TNode) * cap;
}

GMod* ai_mod_alloc(a_henv env, a_usize len) {
	a_usize cap = mod_capacity_hint(len);
	if (cap > (UINT32_MAX >> 1)) ai_err_raisef(env, ALO_EINVAL, "module size too large.");

	GMod* self = ai_mem_alloc(env, mod_size(cap));
	self->_vtable = &mod_vtable;

	table_init_array(&self->_table, self->_data, cap);
	self->_len = len;

	self->_mc = 0;
	self->_rc = 0;
	self->_next = null;
	self->_name = null;
	self->_loader = null;

	ai_gc_register_object(env, self);

	return self;
}

Value* ai_mod_emplace(a_henv env, GMod* self, GStr* key) {
	TNode* node = table_emplace(env, &self->_table, v_of_obj(key), key->_hash, v_of_nil());
	return &node->_value;
}

static void mod_mark(Global* g, GMod* self) {
	if (self->_loader != null) {
		ai_gc_trace_mark(g, self->_loader);
	}
	if (self->_name != null) {
		ai_gc_trace_mark(g, self->_name);
	}
	for (a_usize i = 0; i <= self->_hmask; ++i) {
		TNode* node = &self->_arr[i];
		if (!tnode_is_empty(node)) {
			ai_gc_trace_mark_val(g, node->_key);
			ai_gc_trace_mark_val(g, node->_value);
		}
	}
	ai_gc_trace_work(g, mod_size(self->_hmask + 1));
}

static void mod_drop(Global* g, GMod* self) {
	ai_mem_dealloc(g, self, mod_size(self->_hmask + 1));
}

static GMod* cache_get_mod(ModCache* cache, GStr* name) {
	a_u32 index = name->_hash & cache->_hmask;
	for (GMod* mod = cache->_table[index]; mod != null; mod = mod->_next) {
		if (ai_str_equals(name, mod->_name)) {
			return mod;
		}
	}
	return null;
}

static GMod* loader_load_mod(a_henv env, GModLoader* loader, GStr* name, a_bool load) {
	if (loader == null) {
		return cache_get_mod(&G(env)->_mod_cache, name);
	}
	else {
		GMod* mod = cache_get_mod(&loader->_body._cache, name);
		if (mod == null) {
			mod = loader_load_mod(env, loader->_body._parent, name, load);
			if (mod == null && load) {
				panic("TODO"); //TODO
				if (mod != null) {
					ai_mod_cache(env, loader, mod);
				}
			}
		}
		return mod;
	}
}

GMod* ai_mod_load(a_henv env, GModLoader* loader, GStr* name, a_bool load) {
	return loader_load_mod(env, loader, name, load);
}

static void cache_put_in_place(ModCache* cache, GMod* mod) {
	a_u32 id = mod->_name->_hash & cache->_hmask;
	GMod** pmod = &cache->_table[id];
	GMod* mod2;
	while ((mod2 = *pmod) != null) {
		pmod = &mod2->_next;
	}
	*pmod = mod;
}

static void cache_grow(a_henv env, ModCache* cache) {
	a_usize old_cap = cache->_table != null ? cache->_hmask + 1 : 0;
	GMod** old_ptr = cache->_table;

	a_usize new_cap = max((cache->_hmask + 1) * 2, 4);
	GMod** new_ptr = ai_mem_vnew(env, GMod*, new_cap);

	memclr(new_ptr, sizeof(GMod*) * new_cap);

	cache->_table = new_ptr;
	cache->_hmask = new_cap - 1;

	for (a_usize i = 0; i < old_cap; ++i) {
		GMod* mod = old_ptr[i];
		while (mod != null) {
			GMod* next = mod->_next;
			mod->_next = null;
			cache_put_in_place(cache, mod);
			mod = next;
		}
	}

	ai_mem_vdel(G(env), old_ptr, old_cap);
}

static void cache_put(a_henv env, ModCache* cache, GMod* mod) {
	if (cache->_len >= cache->_hmask) {
		cache_grow(env, cache);
	}
	cache_put_in_place(cache, mod);
	cache->_len += 1;
}

void ai_mod_cache(a_henv env, GModLoader* loader, GMod* mod) {
	assume(mod->_mc == 0, "module already registered.");
	ModCache* cache = loader != null ? &loader->_body._cache : &G(env)->_mod_cache;
	cache_put(env, cache, mod);
	mod->_loader = loader;
	mod->_mc += 1;
}

void ai_mod_cache_mark(Global* g, ModCache* cache) {
	for (a_u32 i = 0; i <= cache->_hmask; ++i) {
		GMod** pmod = &cache->_table[i];
		GMod* mod;
		while ((mod = *pmod) != null) {
			ai_gc_trace_mark(g, mod);
			pmod = &mod->_next;
		}
	}
}

static void mod_cache_drop(Global* g, ModCache* cache) {
	if (cache->_table != null) {
		ai_mem_vdel(g, cache->_table, cache->_hmask + 1);
	}
}

void ai_mod_clean(Global* g) {
	mod_cache_drop(g, &g->_mod_cache);
}

static VTable const table_vtable = {
	._tid = T_TABLE,
	._api_tag = ALO_TTABLE,
	._repr_id = REPR_TABLE,
	._flags = VTABLE_FLAG_IDENTITY_EQUAL,
	._name = "table",
	._mark = fpcast(a_fp_mark, table_mark),
	._drop = fpcast(a_fp_drop, table_drop),
	._get = fpcast(a_fp_get, table_get),
	._set = fpcast(a_fp_set, ai_table_set)
};

static VTable const mod_vtable = {
	._tid = T_MOD,
	._api_tag = ALO_TMOD,
	._repr_id = REPR_TABLE,
	._flags = VTABLE_FLAG_IDENTITY_EQUAL,
	._name = "mod",
	._mark = fpcast(a_fp_mark, mod_mark),
	._drop = fpcast(a_fp_drop, mod_drop),
	._get = fpcast(a_fp_get, table_get)
};