/**
 *@file ameta.c
 */

#define ameta_c_
#define ALO_LIB

#include "adict.h"
#include "atable.h"
#include "auser.h"
#include "aenv.h"
#include "amem.h"
#include "agc.h"
#include "avm.h"
#include "aerr.h"

#include "ameta.h"

static VTable const mod_vtable;
static VTable const type_vtable;

GMeta* ai_meta_alloc(a_henv env, a_usize size, VTable const* vptr) {
	GMeta* self = ai_mem_alloc(env, size);
	memclr(self, size);

	self->_vptr = vptr;
	self->_size = size;

	ai_gc_register_object(env, self);
	return self;
}

GMeta* ai_mod_new(a_henv env, GStr* name, GLoader* loader) {
	GMeta* self = ai_meta_alloc(env, sizeof(GMeta), &mod_vtable);

    self->_name = name;
    self->_loader = loader;

	return self;
}

static GMeta* cache_look(MetaCache* cache, GStr* name) {
	a_u32 index = name->_hash & cache->_hmask;
	for (GMeta* meta = cache->_table[index]; meta != null; meta = meta->_mnext) {
		if (ai_str_equals(name, meta->_name)) {
			return meta;
		}
	}
	return null;
}

static GMeta* loader_look(a_henv env, GLoader* loader, GStr* name, a_bool load) {
	if (loader == null) {
		return cache_look(&G(env)->_meta_cache, name);
	}
	else {
		GMeta* type = cache_look(&loader->_cache, name);
		if (type == null) {
			type = loader_look(env, loader->_parent, name, load);
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

GMeta* ai_meta_look(a_henv env, GLoader* loader, GStr* name, a_bool load) {
	return loader_look(env, loader, name, load);
}

static void cache_put_in_place(MetaCache* cache, GMeta* meta) {
	a_u32 id = meta->_name->_hash & cache->_hmask;
	GMeta** pmeta = &cache->_table[id];
	GMeta* meta2;
	while ((meta2 = *pmeta) != null) {
        pmeta = &meta2->_mnext;
	}
	*pmeta = meta;
}

static void cache_grow(a_henv env, MetaCache* cache) {
	a_usize old_cap = cache->_table != null ? cache->_hmask + 1 : 0;
	GMeta** old_ptr = cache->_table;

	a_usize new_cap = max(old_cap * 2, 4);
	GMeta** new_ptr = ai_mem_vnew(env, GMeta*, new_cap);

	memclr(new_ptr, sizeof(GMeta*) * new_cap);

	cache->_table = new_ptr;
	cache->_hmask = new_cap - 1;

	for (a_usize i = 0; i < old_cap; ++i) {
		GMeta* meta = old_ptr[i];
		while (meta != null) {
			GMeta* next = meta->_mnext;
            meta->_mnext = null;
			cache_put_in_place(cache, meta);
            meta = next;
		}
	}

	ai_mem_vdel(G(env), old_ptr, old_cap);
}

static void cache_put(a_henv env, MetaCache* cache, GMeta* meta) {
	if (cache->_len >= cache->_hmask) {
		cache_grow(env, cache);
	}
	cache_put_in_place(cache, meta);
	cache->_len += 1;
}

void ai_meta_cache(a_henv env, GLoader* loader, GMeta* meta) {
	assume(meta->_nref == 0, "module already registered.");
	MetaCache* cache = loader != null ? &loader->_cache : &G(env)->_meta_cache;
	cache_put(env, cache, meta);
    meta->_loader = loader;
    meta->_nref += 1;
}

void ai_meta_cache_mark(Global* g, MetaCache* cache) {
	for (a_u32 i = 0; i <= cache->_hmask; ++i) {
		GMeta** pmeta = &cache->_table[i];
		GMeta* meta;
		while ((meta = *pmeta) != null) {
			ai_gc_trace_mark(g, meta);
            pmeta = &meta->_mnext;
		}
	}
}

static void cache_drop(Global* g, MetaCache* cache) {
	if (cache->_table != null) {
		ai_mem_vdel(g, cache->_table, cache->_hmask + 1);
	}
}

void ai_meta_boost(a_henv env) {
	Global* g = G(env);

    g->_types._nil = new(GType) {
        ._vptr = &type_vtable,
        ._flags = META_FLAG_NONE,
        ._tag = ALO_TNIL,
        ._name = env_int_str(env, STR_nil)
    };
    g->_types._bool = new(GType) {
        ._vptr = &type_vtable,
        ._flags = META_FLAG_NONE,
        ._tag = ALO_TBOOL,
        ._name = env_int_str(env, STR_bool)
    };
    g->_types._int = new(GType) {
        ._vptr = &type_vtable,
        ._flags = META_FLAG_NONE,
        ._tag = ALO_TINT,
        ._name = env_int_str(env, STR_int)
    };
    g->_types._float = new(GType) {
        ._vptr = &type_vtable,
        ._flags = META_FLAG_NONE,
        ._tag = ALO_TFLOAT,
        ._name = env_int_str(env, STR_float)
    };
    g->_types._ptr = new(GType) {
        ._vptr = &type_vtable,
        ._flags = META_FLAG_NONE,
        ._tag = ALO_TPTR,
        ._name = env_int_str(env, STR_ptr)
    };
    g->_types._str = new(GRefType) {
        ._vptr = &type_vtable,
        ._flags = META_FLAG_NONE,
        ._tag = ALO_TSTR,
        ._name = env_int_str(env, STR_str)
    };
    g->_types._tuple = new(GRefType) {
        ._vptr = &type_vtable,
        ._flags = META_FLAG_NONE,
        ._tag = ALO_TTUPLE,
        ._name = env_int_str(env, STR_tuple)
    };
    g->_types._list = new(GRefType) {
        ._vptr = &type_vtable,
        ._flags = META_FLAG_NONE,
        ._tag = ALO_TLIST,
        ._name = env_int_str(env, STR_list)
    };
    g->_types._table = new(GRefType) {
        ._vptr = &type_vtable,
        ._flags = META_FLAG_NONE,
        ._tag = ALO_TTABLE,
        ._name = env_int_str(env, STR_table)
    };
    g->_types._func = new(GRefType) {
        ._vptr = &type_vtable,
        ._flags = META_FLAG_NONE,
        ._tag = ALO_TFUNC,
        ._name = env_int_str(env, STR_func)
    };
    g->_types._route = new(GRefType) {
        ._vptr = &type_vtable,
        ._flags = META_FLAG_NONE,
        ._tag = ALO_TROUTE,
        ._name = env_int_str(env, STR_route)
    };
    g->_types._mod = new(GRefType) {
        ._vptr = &type_vtable,
        ._flags = META_FLAG_NONE,
        ._tag = ALO_TUSER,
        ._name = env_int_str(env, STR_mod)
    };
    g->_types._type = new(GRefType) {
        ._vptr = &type_vtable,
        ._flags = META_FLAG_NONE,
        ._tag = ALO_TTYPE,
        ._name = env_int_str(env, STR_type)
    };
}

Value ai_meta_get(a_henv env, GMeta* self, Value vk) {
    if (!v_is_str(vk)) {
        ai_err_bad_key(env, g_nameof(env, self), v_nameof(env, vk));
    }

    GStr* k = v_as_str(vk);
    Value vv;
    a_msg msg = ai_dict_uget(env, &self->_fields, k, &vv);

    if (msg != ALO_SOK) {
        assume(msg == ALO_EEMPTY, "unexpected error.");
        ai_err_raisef(env, ALO_EBADOP, "no meta entry '%s.%s'",
                      str2ntstr(self->_name),
                      str2ntstr(k));
    }

    return vv;
}

void ai_meta_set(a_henv env, GMeta* self, Value vk, Value vv) {
    if (!v_is_str(vk)) {
        ai_err_bad_key(env, g_nameof(env, self), v_nameof(env, vk));
    }

    a_usize ctx;
    a_msg msg = ai_dict_uset(env, &self->_fields, v_as_str(vk), vv, &ctx);

    if (msg != ALO_SOK) {
        msg = ai_dict_uput(env, &self->_fields, v_as_str(vk), vv, &ctx);
    }

    assume(msg == ALO_SOK);

    self->_mver += 1;
}

a_msg ai_meta_uget(a_henv env, GMeta* self, Value vk, Value* pv) {
    if (!v_is_str(vk)) return ALO_EINVAL;
    return ai_meta_ugets(env, self, v_as_str(vk), pv);
}

a_msg ai_meta_ugets(a_henv env, GMeta* self, GStr* k, Value* pv) {
    return ai_dict_uget(env, &self->_fields, k, pv);
}

a_msg ai_meta_uset(a_henv env, GMeta* self, Value vk, Value vv) {
    if (!v_is_str(vk)) return ALO_EINVAL;
    return ai_meta_usets(env, self, v_as_str(vk), vv);
}

a_msg ai_meta_usets(a_henv env, GMeta* self, GStr* k, Value vv) {
    a_usize ctx;

    try(ai_dict_uset(env, &self->_fields, k, vv, &ctx));

    if (str_istm(k) && str_id(k) <= TM__FAST_MAX) {
        self->_flags |= META_FLAG_FAST_TM(str_id(k));
    }

    self->_mver += 1;

    ai_gc_barrier_backward(env, self, k);
    ai_gc_barrier_backward_val(env, self, vv);

    return ALO_SOK;
}

static void meta_mark(Global* g, GMeta* self) {
    if (self->_loader != null) {
        ai_gc_trace_mark(g, self->_loader);
    }
    if (self->_name != null) {
        ai_gc_trace_mark(g, self->_name);
    }
    ai_dict_mark(g, &self->_fields);
    ai_gc_trace_work(g, self->_size);
}

static void meta_clean(Global* g, GMeta* self) {
    ai_dict_deinit(g, &self->_fields);
}

void ai_meta_clean(Global* g) {
    meta_clean(g, g_cast(GMeta, &g->_types._nil));
    meta_clean(g, g_cast(GMeta, &g->_types._bool));
    meta_clean(g, g_cast(GMeta, &g->_types._int));
    meta_clean(g, g_cast(GMeta, &g->_types._float));
    meta_clean(g, g_cast(GMeta, &g->_types._ptr));
    meta_clean(g, g_cast(GMeta, &g->_types._tuple));
    meta_clean(g, g_cast(GMeta, &g->_types._list));
    meta_clean(g, g_cast(GMeta, &g->_types._table));
    meta_clean(g, g_cast(GMeta, &g->_types._func));
    meta_clean(g, g_cast(GMeta, &g->_types._route));
    meta_clean(g, g_cast(GMeta, &g->_types._type));
    meta_clean(g, g_cast(GMeta, &g->_types._mod));
    cache_drop(g, &g->_meta_cache);
}

static void meta_drop(Global* g, GMeta* self) {
    meta_clean(g, self);
    ai_mem_dealloc(g, self, self->_size);
}

static VTable const mod_vtable = {
    ._stencil = V_STENCIL(T_META),
    ._type_ref = g_type_ref(_mod),
    ._slots = {
        [vfp_slot(drop)] = meta_drop,
        [vfp_slot(mark)] = meta_mark
    }
};

static VTable const type_vtable = {
	._stencil = V_STENCIL(T_META),
    ._type_ref = g_type_ref(_type),
	._slots = {
        [vfp_slot(drop)] = meta_drop,
        [vfp_slot(mark)] = meta_mark
	}
};
