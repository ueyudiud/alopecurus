/**
 *@file amod.c
 */

#define amod_c_
#define ALO_LIB

#include "atable.h"
#include "agbl.h"
#include "amem.h"
#include "agc.h"
#include "avm.h"

#include "amod.h"

static VTable const mod_vtable;

/* Defined in atable.c */
intern void table_init_array(GTable* self, TNode* array, a_usize cap);

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
	memclr(self, sizeof(GMod));

	self->_vtable = &mod_vtable;
	table_init_array(&self->_table, self->_data, cap);

	self->_next = null;
	self->_name = null;
	self->_loader = null;

	ai_gc_register_object(env, self);

	return self;
}

static void generic_close(a_henv env, a_hobj self) {
	Value vf = *ai_obj_vlookup_(env, self->_vtable, TM_CLOSE);
	Value* base = vm_push_args(env, vf, v_of_obj(self));
	ai_vm_call(env, base, RFLAGS_META_CALL);
}

static a_hash generic_hash(a_henv env, a_hobj self) {
	Value vf = *ai_obj_vlookup_(env, self->_vtable, TM_HASH);
	Value* base = vm_push_args(env, vf, v_of_obj(self));
	Value vh = ai_vm_call(env, base, RFLAGS_META_CALL);
	if (!v_is_int(vh)) ai_err_raisef(env, ALO_EINVAL, "bad hash code.");
	return cast(a_hash, v_as_int(vh));
}

static a_bool generic_equals(a_henv env, a_hobj self, Value other) {
	Value vf = *ai_obj_vlookup_(env, self->_vtable, TM_EQ);
	Value* base = vm_push_args(env, vf, v_of_obj(self), other);
	return v_to_bool(ai_vm_call(env, base, RFLAGS_META_CALL));
}

void ai_mod_ready(a_henv env, GMod* self, VTable const* impl) {
	quiet(env);
	if (impl != null) {
		a_u32 op_flags = self->_impl._flags;
		self->_impl = new(VTable) {
			._val_mask = v_masked_tag(op_flags & VTABLE_FLAG_FAST_TM(TM_EQ) ? T_USER_NEQ : T_USER_TEQ),
			._api_tag = ALO_TUSER,
			._repr_id = impl->_repr_id,
			._flags = op_flags | (impl->_flags & VTABLE_FLAG_PLAIN_LEN) | VTABLE_FLAG_VLOOKUP,
			._name = str2ntstr(self->_name),
			._mark = impl->_mark,
			._drop = impl->_drop,
			._close = op_flags & VTABLE_FLAG_FAST_TM(TM_CLOSE) ? generic_close : impl->_close,
			._hash = op_flags & VTABLE_FLAG_FAST_TM(TM_HASH) ? generic_hash : impl->_hash,
			._equals = op_flags & VTABLE_FLAG_FAST_TM(TM_EQ) ? generic_equals : impl->_equals
		};
	}
	else {
		self->_impl = new(VTable) {
			._repr_id = REPR_TRAIT,
			._name = self->_name != null ? str2ntstr(self->_name) : null,
		};
	}
	self->_mc = 0;
	self->_rc = 0;
}

void ai_mod_emplace(a_henv env, GMod* self, GStr* key, Value value) {
	ai_table_set(env, &self->_table, v_of_obj(key), value);
	self->_len += 1;
	if (strx_istm(key)) {
		a_enum tm = strx_totm(key);
		if (tm <= TM__FAST_MAX) {
			self->_impl._flags |= 1 << tm;
		}
	}
}

Value const* ai_obj_vlookup_(a_henv env, VTable const* self, a_enum tm) {
	GStr* key = ai_env_strx(G(env),  STRX_KW__BEGIN + tm);
	GTable* table = &from_member(GMod, _impl, self)->_table;
	return ai_table_refis(env, table, key);
}

static void mod_mark(Global* g, a_hobj raw_self) {
	GMod* self = g_cast(GMod, raw_self);
	if (self->_loader != null) {
		ai_gc_trace_mark(g, self->_loader);
	}
	if (self->_name != null) {
		ai_gc_trace_mark(g, self->_name);
	}
	for (a_usize i = 0; i <= self->_hmask; ++i) {
		TNode* node = &self->_ptr[i];
		if (!tnode_is_empty(node)) {
			ai_gc_trace_mark_val(g, node->_key);
			ai_gc_trace_mark_val(g, node->_value);
		}
	}
	ai_gc_trace_work(g, mod_size(self->_hmask + 1));
}

static void mod_drop(Global* g, a_hobj raw_self) {
	GMod* self = g_cast(GMod, raw_self);
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

static VTable const mod_vtable = {
	._val_mask = V_MASKED_TAG(T_MOD),
	._api_tag = ALO_TMOD,
	._repr_id = REPR_TABLE,
	._flags = VTABLE_FLAG_PLAIN_LEN | VTABLE_FLAG_READONLY,
	._name = "mod",
	._mark = mod_mark,
	._drop = mod_drop
};
