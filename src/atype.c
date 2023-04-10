/**
 *@file atype.c
 */

#define atype_c_
#define ALO_LIB

#include <string.h>

#include "atable.h"
#include "auser.h"
#include "aenv.h"
#include "amem.h"
#include "agc.h"
#include "avm.h"
#include "aerr.h"

#include "atype.h"

static VTable const type_vtable;

static a_usize sizeof_GType(a_usize extra) {
	return sizeof(GType) + extra;
}

GType* ai_type_alloc(a_henv env, a_usize len, a_vptr proto) {
	a_usize size = sizeof_GType(proto != null || len > 0 ? sizeof(VTable) + sizeof(a_vslot) * len : 0);

	GType* self = ai_mem_alloc(env, size);
	memclr(self, size);

	self->_vptr = &type_vtable;
	self->_size = size;
	if (proto != null) {
		VTable* vtbl = self->_opt_vtbl;
		memcpy(vtbl, proto, sizeof(VTable) + sizeof(a_vslot) * len);
		vtbl->_iname = ptr_diff(self, G(env));
	}

	ai_gc_register_object(env, self);

	return self;
}

void ai_type_init(a_henv env, GType* self, a_tag tag, a_u32 nid) {
	self->_vptr = &type_vtable;
	self->_tag = tag;
	self->_name = nid != 0 ? env_name(env, nid) : null;
}

GType* ai_atype_new(a_henv env) {
	GType* self = ai_type_alloc(env, 3, &ai_auser_vtable); //TODO
	self->_tag = ALO_TUSER;
	return self;
}

static void type_alloc_array(a_henv env, GType* self, a_usize new_cap) {
	void* block = ai_mem_alloc(env, (sizeof(TDNode) + sizeof(Value)) * new_cap);

	self->_hmask = new_cap - 1;
	self->_ptr = block;
	self->_values = block + sizeof(TDNode) * new_cap;

	memclr(block, sizeof(TDNode) * new_cap);
}

static a_bool tdnode_is_head(GType* self, TDNode* node) {
	return node->_key != null && node == map_hash_first(self, node->_key->_hash);
}

static Value* type_refis(unused a_henv env, GType* self, GStr* key) {
	assume(g_is_istr(key), "not short string.");
#define test(n) ((n)->_key == key)
#define empty(n) (!tdnode_is_head(self, n))
#define con(n) ({ return &self->_values[(n)->_index]; })
	map_find_template(self, key->_hash, test, empty, con);
#undef test
#undef empty
#undef con
	return null;
}

static TDNode* type_find_free(GType* self, TDNode* node) {
	a_u32 index_head = node - self->_ptr;
	a_u32 index = index_head;
	loop {
		index = (index + 1) & self->_hmask;
		assume(index != index_head, "no free slot remains.");
		node = &self->_ptr[index];
		if (node->_key == null) {
			return node;
		}
	}
}

static TDNode* type_get_hprev(GType* self, TDNode* node) {
	TDNode* nodep = map_hash_first(self, node->_key->_hash);
	TDNode* noden;
	while ((noden = link_get(nodep, _hnext)) != node) {
		nodep = noden;
	}
	return nodep;
}

static void type_emplace(a_henv env, GType* self, GStr* key, Value value, a_u32 index) {
	TDNode* nodeh = map_hash_first(self, key->_hash);
	if (tdnode_is_head(self, nodeh)) {
		/* Insert into hash list. */
		TDNode* nodet = type_find_free(self, nodeh);
		TDNode* noden = link_get(nodeh, _hnext);

		/* Emplace node at another free node. */
		/*                       v
		 * h -> n -> ... => h -> t -> n -> ...
		 */

		nodet->_key = key;
		nodet->_index = index;

		link_set(nodeh, _hnext, nodet);
		link_set(nodet, _hnext, noden);
	}
	else {
		if (nodeh->_key != null) {
			/* Move placed entry to other node. */
			/*                         v
			 * ... -> p -> h -> ... => h; ... -> p -> f -> ...
			 */
			TDNode* nodef = type_find_free(self, nodeh);
			TDNode* nodep = type_get_hprev(self, nodeh);
			TDNode* noden = link_get(nodeh, _hnext);
			nodef->_key = nodeh->_key;
			nodef->_index = nodeh->_index;
			link_set(nodef, _hnext, noden);
			link_set(nodep, _hnext, nodef);
		}
		/* Emplace node locally. */
		/*    v
		 * => h
		 */

		nodeh->_key = key;
		nodeh->_index = index;
		nodeh->_hnext = nil;
	}
	v_set(env, &self->_values[index], value);
}

void ai_type_hint(a_henv env, GType* self, a_usize len) {
	a_usize old_cap = self->_ptr != null ? self->_hmask + 1 : 0;
	a_usize expect = self->_len + len;
	if (expect > old_cap) {
		a_usize new_cap = ceil_pow2m1_usize(expect) + 1;
		assume(expect <= new_cap && new_cap > 0);

		TDNode* keys = self->_ptr;
		Value* values = self->_values;

		type_alloc_array(env, self, new_cap);

		if (keys != null) {
			for (a_usize i = 0; i < old_cap; ++i) {
				TDNode* node = &keys[i];
				if (node->_key != null) {
					type_emplace(env, self, node->_key, values[node->_index], node->_index);
				}
			}

			ai_mem_dealloc(G(env), keys, (sizeof(TDNode) + sizeof(Value)) * old_cap);
		}
	}
}

void ai_type_set(a_henv env, GType* self, Value key, Value value) {
	if (!v_is_str(key)) {
		ai_err_bad_get(env, "type", v_nameof(env, key));
	}
	else if (!v_is_istr(key)) {
		ai_err_raisef(env, ALO_EINVAL, "type field name too long.");
	}
	else {
		ai_type_setis(env, self, v_as_str(key), value);
	}
}

void ai_type_setis(a_henv env, GType* self, GStr* key, Value value) {
	Value* pv = type_refis(env, self, key);

	if (pv != null) {
		v_set(env, pv, value);
	}
	else {
		ai_type_hint(env, self, 1);
		type_emplace(env, self, key, value, self->_len);
		self->_len += 1;
		if (name_istm(key)) {
			a_enum tm = name_totm(key);
			if (tm <= TM__FAST_MAX) {
				self->_flags |= TYPE_FLAG_FAST_TM(tm);
			}
		}

		ai_gc_barrier_backward(env, self, key);
	}

	ai_gc_barrier_backward_val(env, self, value);
}

Value ai_type_get(a_henv env, GType* self, Value key) {
	if (!v_is_str(key)) {
		ai_err_bad_get(env, "type", v_nameof(env, key));
	}
	else if (!v_is_istr(key)) {
		return v_of_nil();
	}
	else {
		return ai_type_getis(env, self, v_as_str(key));
	}
}

Value ai_type_getis(a_henv env, GType* self, GStr* key) {
	Value* pv = type_refis(env, self, key);
	return pv != null ? *pv : v_of_nil();
}

static void type_drop(Global* g, a_hobj raw_self) {
	GType* self = g_cast(GType, raw_self);
	if (self->_ptr != null) {
		ai_mem_dealloc(g, self->_ptr, (sizeof(TDNode) + sizeof(Value)) * (self->_hmask + 1));
	}
	ai_mem_dealloc(g, self, self->_size);
}

static void type_mark(Global* g, a_hobj raw_self) {
	GType* self = g_cast(GType, raw_self);
	if (self->_loader != null) {
		ai_gc_trace_mark(g, self->_loader);
	}
	if (self->_name != null) {
		ai_gc_trace_mark(g, self->_name);
	}
	if (self->_ptr != null) {
		for (a_u32 i = 0; i <= self->_hmask; ++i) {
			TDNode* node = &self->_ptr[i];
			if (node->_key != null) {
				ai_gc_trace_mark(g, node->_key);
			}
		}
		for (a_u32 i = 0; i < self->_len; ++i) {
			ai_gc_trace_mark_val(g, self->_values[i]);
		}
		ai_gc_trace_work(g, (sizeof(TDNode) + sizeof(Value)) * (self->_hmask + 1));
	}
}

static GType* cache_get(TypeCache* cache, GStr* name) {
	a_u32 index = name->_hash & cache->_hmask;
	for (GType* mod = cache->_table[index]; mod != null; mod = mod->_next) {
		if (ai_str_equals(name, mod->_name)) {
			return mod;
		}
	}
	return null;
}

static GType* loader_load(a_henv env, GLoader* loader, GStr* name, a_bool load) {
	if (loader == null) {
		return cache_get(&G(env)->_type_cache, name);
	}
	else {
		GType* type = cache_get(&loader->_body._cache, name);
		if (type == null) {
			type = loader_load(env, loader->_body._parent, name, load);
			if (type == null && load) {
				panic("TODO"); //TODO
				if (type != null) {
					ai_type_cache(env, loader, type);
				}
			}
		}
		return type;
	}
}

GType* ai_type_load(a_henv env, GLoader* loader, GStr* name, a_bool load) {
	return loader_load(env, loader, name, load);
}

static void cache_put_in_place(TypeCache* cache, GType* mod) {
	a_u32 id = mod->_name->_hash & cache->_hmask;
	GType** pmod = &cache->_table[id];
	GType* mod2;
	while ((mod2 = *pmod) != null) {
		pmod = &mod2->_next;
	}
	*pmod = mod;
}

static void cache_grow(a_henv env, TypeCache* cache) {
	a_usize old_cap = cache->_table != null ? cache->_hmask + 1 : 0;
	GType** old_ptr = cache->_table;

	a_usize new_cap = max((cache->_hmask + 1) * 2, 4);
	GType** new_ptr = ai_mem_vnew(env, GType*, new_cap);

	memclr(new_ptr, sizeof(GType*) * new_cap);

	cache->_table = new_ptr;
	cache->_hmask = new_cap - 1;

	for (a_usize i = 0; i < old_cap; ++i) {
		GType* mod = old_ptr[i];
		while (mod != null) {
			GType* next = mod->_next;
			mod->_next = null;
			cache_put_in_place(cache, mod);
			mod = next;
		}
	}

	ai_mem_vdel(G(env), old_ptr, old_cap);
}

static void cache_put(a_henv env, TypeCache* cache, GType* mod) {
	if (cache->_len >= cache->_hmask) {
		cache_grow(env, cache);
	}
	cache_put_in_place(cache, mod);
	cache->_len += 1;
}

void ai_type_cache(a_henv env, GLoader* loader, GType* type) {
	assume(type->_nref == 0, "module already registered.");
	TypeCache* cache = loader != null ? &loader->_body._cache : &G(env)->_type_cache;
	cache_put(env, cache, type);
	type->_loader = loader;
	type->_nref += 1;
}

void ai_type_cache_mark(Global* g, TypeCache* cache) {
	for (a_u32 i = 0; i <= cache->_hmask; ++i) {
		GType** pmod = &cache->_table[i];
		GType* mod;
		while ((mod = *pmod) != null) {
			ai_gc_trace_mark(g, mod);
			pmod = &mod->_next;
		}
	}
}

static void type_cache_drop(Global* g, TypeCache* cache) {
	if (cache->_table != null) {
		ai_mem_vdel(g, cache->_table, cache->_hmask + 1);
	}
}

void ai_type_clean(Global* g) {
	type_cache_drop(g, &g->_type_cache);
}

static VTable const type_vtable = {
	._mask = V_MASKED_TAG(T_TYPE),
	._iname = env_type_iname(_type),
	._sname = "type",
	._base_size = 0,
	._elem_size = 1,
	._flags = VTABLE_FLAG_NONE,
	._vfps = (a_vslot[]) {
		vfp_def(drop, type_drop),
		vfp_def(mark, type_mark)
	}
};
