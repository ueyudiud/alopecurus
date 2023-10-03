/**
 *@file ameta.c
 */

#define ameta_c_
#define ALO_LIB

#include <string.h>

#include "adict.h"
#include "atable.h"
#include "auser.h"
#include "aenv.h"
#include "amem.h"
#include "agc.h"
#include "avm.h"
#include "aerr.h"

#include "ameta.h"

static VTable const meta_vtable;

static a_usize sizeof_GMeta(a_usize extra) {
	return sizeof(GMeta) + extra;
}

GMeta* ai_meta_alloc(a_henv env, a_usize len, a_vtbl vtbl) {
	a_usize size = sizeof_GMeta(vtbl != null || len > 0 ? sizeof(VTable) + sizeof(a_vptr) * len : 0);

	GMeta* self = ai_mem_alloc(env, size);
	memclr(self, size);

	self->_vptr = &meta_vtable;
	self->_size = size;

	if (vtbl != null) {
		memcpy(self->_opt_vtbl, vtbl, sizeof(VTable) + sizeof(a_vptr) * len);
		self->_opt_vtbl->_htype = meta2htype(G(env), self);
	}

	ai_gc_register_object(env, self);

	return self;
}

GMeta* ai_ameta_new(a_henv env) {
	GMeta* self = ai_meta_alloc(env, 3, &ai_auser_vtable); //TODO 3 is a magic number.
	self->_tag = ALO_TUSER;
	return self;
}

static GMeta* cache_look(MetaCache* cache, GStr* name) {
	a_u32 index = name->_hash & cache->_hmask;
	for (GMeta* mod = cache->_table[index]; mod != null; mod = mod->_next) {
		if (ai_str_equals(name, mod->_uid)) {
			return mod;
		}
	}
	return null;
}

static GMeta* loader_load(a_henv env, GLoader* loader, GStr* name, a_bool load) {
	if (loader == null) {
		return cache_look(&G(env)->_mod_cache, name);
	}
	else {
		GMeta* type = cache_look(&loader->_body._cache, name);
		if (type == null) {
			type = loader_load(env, loader->_body._parent, name, load);
			if (type == null && load) {
				panic("TODO"); //TODO
				if (type != null) {
                    ai_meta_cache(env, loader, type);
				}
			}
		}
		return type;
	}
}

GMeta* ai_meta_load(a_henv env, GLoader* loader, GStr* name, a_bool load) {
	return loader_load(env, loader, name, load);
}

static void cache_put_in_place(MetaCache* cache, GMeta* mod) {
	a_u32 id = mod->_uid->_hash & cache->_hmask;
	GMeta** pmod = &cache->_table[id];
	GMeta* mod2;
	while ((mod2 = *pmod) != null) {
		pmod = &mod2->_next;
	}
	*pmod = mod;
}

static void cache_grow(a_henv env, MetaCache* cache) {
	a_usize old_cap = cache->_table != null ? cache->_hmask + 1 : 0;
	GMeta** old_ptr = cache->_table;

	a_usize new_cap = max((cache->_hmask + 1) * 2, 4);
	GMeta** new_ptr = ai_mem_vnew(env, GMeta*, new_cap);

	memclr(new_ptr, sizeof(GMeta*) * new_cap);

	cache->_table = new_ptr;
	cache->_hmask = new_cap - 1;

	for (a_usize i = 0; i < old_cap; ++i) {
		GMeta* mod = old_ptr[i];
		while (mod != null) {
			GMeta* next = mod->_next;
			mod->_next = null;
			cache_put_in_place(cache, mod);
			mod = next;
		}
	}

	ai_mem_vdel(G(env), old_ptr, old_cap);
}

static void cache_put(a_henv env, MetaCache* cache, GMeta* mod) {
	if (cache->_len >= cache->_hmask) {
		cache_grow(env, cache);
	}
	cache_put_in_place(cache, mod);
	cache->_len += 1;
}

void ai_meta_cache(a_henv env, GLoader* loader, GMeta* meta) {
	assume(meta->_nref == 0, "module already registered.");
	MetaCache* cache = loader != null ? &loader->_body._cache : &G(env)->_mod_cache;
	cache_put(env, cache, meta);
    meta->_loader = loader;
    meta->_nref += 1;
}

void ai_meta_cache_mark(Global* g, MetaCache* cache) {
	for (a_u32 i = 0; i <= cache->_hmask; ++i) {
		GMeta** pmod = &cache->_table[i];
		GMeta* mod;
		while ((mod = *pmod) != null) {
			ai_gc_trace_mark(g, mod);
			pmod = &mod->_next;
		}
	}
}

static void cache_drop(Global* g, MetaCache* cache) {
	if (cache->_table != null) {
		ai_mem_vdel(g, cache->_table, cache->_hmask + 1);
	}
}

static void ptype_init(a_henv env, GMeta* self, a_msg tag, a_u32 uid_tag) {
    self->_vptr = &meta_vtable;
    self->_tag = tag;
    self->_uid = env_int_str(env, uid_tag);
}

static void meta_mark(Global* g, GMeta* self) {
    if (self->_loader != null) {
        ai_gc_trace_mark(g, self->_loader);
    }
    if (self->_uid != null) {
        ai_gc_trace_mark(g, self->_uid);
    }
    ai_dict_mark(g, &self->_dict);
    ai_gc_trace_work(g, self->_size);
}

static void meta_drop(Global* g, GMeta* self) {
    ai_mem_dealloc(g, self, self->_size);
}

void ai_meta_boost(a_henv env) {
	Global* g = G(env);

    ptype_init(env, &g->_types._nil, ALO_TNIL, STR_nil);
    ptype_init(env, &g->_types._bool, ALO_TBOOL, STR_bool);
    ptype_init(env, &g->_types._int, ALO_TINT, STR_int);
    ptype_init(env, &g->_types._float, ALO_TFLOAT, STR_float);
    ptype_init(env, &g->_types._ptr, ALO_TPTR, STR_ptr);
    ptype_init(env, &g->_types._str, ALO_TSTR, STR_str);
    ptype_init(env, &g->_types._tuple, ALO_TTUPLE, STR_tuple);
    ptype_init(env, &g->_types._list, ALO_TLIST, STR_list);
    ptype_init(env, &g->_types._table, ALO_TTABLE, STR_table);
    ptype_init(env, &g->_types._route, ALO_TROUTE, STR_route);
    ptype_init(env, &g->_types._func, ALO_TFUNC, STR_func);
    ptype_init(env, &g->_types._type, ALO_TTYPE, STR_type);
}

void ai_meta_clean(Global* g) {
    cache_drop(g, &g->_mod_cache);
}

Value ai_meta_get(a_henv env, GMeta* self, Value key) {
    Value val;
    a_msg msg;

    if (!v_is_str(key)) {
        ai_err_raisef(env, ALO_EINVAL, "bad %s key for meta", v_nameof(env, key));
    }

    msg = ai_dict_uget(env, &self->_dict, v_as_str(key), &val);

    if (msg != ALO_SOK) {
        assume(msg == ALO_EEMPTY, "unexpected error.");
        ai_err_raisef(env, ALO_EBADOP, "no entry %s.%s",
                      str2ntstr(self->_uid),
                      str2ntstr(v_as_str(key)));
    }

    return val;
}

a_msg ai_meta_uget(a_henv env, GMeta* self, Value key, Value* pval) {
    if (!v_is_str(key))
        return ALO_EINVAL;

    return ai_dict_uget(env, &self->_dict, v_as_str(key), pval);
}

a_msg ai_meta_uset(a_henv env, GMeta* self, Value key, Value val) {
    if (!v_is_str(key))
        return ALO_EINVAL;

    a_usize ctx;
    GStr* k = v_as_str(key);

    try(ai_dict_uset(env, &self->_dict, k, val, &ctx));

    self->_mver += 1;

    ai_gc_barrier_forward(env, self, k);
    ai_gc_barrier_backward_val(env, self, val);

    return ALO_SOK;
}

static VTable const meta_vtable = {
	._stencil = V_STENCIL(T_META),
	._htype = g_htype(_type),
	._uid = "meta",
	._flags = VTABLE_FLAG_NONE,
	._vfps = {
        vfp_def(drop, meta_drop),
		vfp_def(mark, meta_mark)
	}
};
