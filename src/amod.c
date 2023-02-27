/**
 *@file amod.c
 */

#define amod_c_
#define ALO_LIB

#include "astr.h"
#include "agbl.h"
#include "amem.h"
#include "agc.h"
#include "aerr.h"

#include "amod.h"

static VTable const mod_vtable;

static a_usize mod_compute_capacity(a_usize len) {
	a_usize required = len * 6 / 5;
	return ceil_pow2m1_usize(required);
}

static a_usize mod_alloc_size(a_usize cap, a_usize len) {
	return sizeof(GMod) + sizeof(Value) * len + sizeof(DNode) * cap;
}

static a_usize mod_alloc_size_by(GMod* self) {
	a_usize len = self->_table._len;
	a_usize cap = self->_table._len > 0 ? self->_table._mask + 1 : 0;
	return mod_alloc_size(cap, len);
}

GMod* ai_mod_new(a_henv env, GStr* name, GStr** pkey, a_usize len) {
	a_usize cap = mod_compute_capacity(len);
	if (cap > (UINT32_MAX >> 1)) ai_err_raisef(env, ALO_EINVAL, "module size too large.");
	GMod* self = cast(GMod*, ai_mem_alloc(env, mod_alloc_size(cap, len)));
	self->_vtable = &mod_vtable;
	self->_mc = 0;
	self->_rc = 0;
	self->_next = null;
	self->_name = name;
	self->_loader = null;

	a_usize addr = addr_of(self) + sizeof(GMod) + sizeof(Value) * len;
	self->_table = new(Dict) {
		._mask = likely(cap > 0) ? cap - 1 : 0,
		._arr = ptr_of(DNode, addr)
	};
	memclr(self->_table._arr, sizeof(DNode) * cap);

	for (a_u32 i = 0; i < len; ++i) {
		v_set_nil(&self->_values[i]);
	}

	a_u32 hfree = 0;
	for (a_u32 i = 0; i < len; ++i) {
		ai_dict_push_in_place(&self->_table, pkey[i], &hfree);
	}

	ai_gc_register_object(env, self);

	return self;
}

a_i32 ai_mod_find(unused a_henv env, GMod* self, GStr* key) {
	a_u32 const* ppos = ai_dict_find(&self->_table, key);
	return ppos != null ? cast(a_i32, *ppos) : -1;
}

static void mod_splash(Global* g, GMod* self) {
	if (self->_loader != null)
		ai_gc_trace_mark(g, self->_loader);
	ai_gc_trace_mark(g, self->_name);
	ai_dict_splash(g, &self->_table);
	for (a_u32 i = 0; i < self->_table._len; ++i) {
		ai_gc_trace_mark_val(g, self->_values[i]);
	}
	ai_gc_trace_work(g, mod_alloc_size_by(self));
}

static void mod_destruct(Global* g, GMod* self) {
	ai_mem_dealloc(g, self, mod_alloc_size_by(self));
}

static Value mod_get(a_henv env, GMod* self, Value key) {
	if (!v_is_str(key)) {
		ai_err_raisef(env, ALO_EINVAL, "bad index for mod.");
	}
	a_i32 id = ai_mod_find(env, self, v_as_str(key));
	if (id < 0) {
		ai_err_raisef(env, ALO_EINVAL, "symbol does not exists.");
	}
	return self->_values[id];
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

void ai_mod_cache_splash(Global* g, ModCache* cache) {
	for (a_u32 i = 0; i <= cache->_hmask; ++i) {
		GMod** pmod = &cache->_table[i];
		GMod* mod;
		while ((mod = *pmod) != null) {
			ai_gc_trace_mark(g, mod);
			pmod = &mod->_next;
		}
	}
}

static void mod_cache_destruct(Global* g, ModCache* cache) {
	if (cache->_table != null) {
		ai_mem_vdel(g, cache->_table, cache->_hmask + 1);
	}
}

void ai_mod_clean(Global* g) {
	mod_cache_destruct(g, &g->_mod_cache);
}

static VTable const mod_vtable = {
	._tid = T_MOD,
	._api_tag = ALO_TMOD,
	._repr_id = REPR_MOD,
	._flags = VTABLE_FLAG_IDENTITY_EQUAL,
	._name = "mod",
	._splash = fpcast(a_fp_splash, mod_splash),
	._delete = fpcast(a_fp_delete, mod_destruct),
	._get = fpcast(a_fp_get, mod_get)
};