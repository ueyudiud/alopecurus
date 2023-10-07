/**
 *@file atype.c
 */

#define atype_c_
#define ALO_LIB

#include "adict.h"
#include "atable.h"
#include "auser.h"
#include "aenv.h"
#include "amem.h"
#include "agc.h"
#include "avm.h"
#include "aerr.h"

#include "atype.h"

static VTable const type_vtable;

GType* ai_type_alloc(a_henv env, a_usize size, VTable const* vptr) {
	GType* self = ai_mem_alloc(env, size);
	memclr(self, size);

	self->_vptr = vptr;
	self->_size = size;

	ai_gc_register_object(env, self);
	return self;
}

GType* ai_stype_new(a_henv env, GStr* name, GLoader* loader) {
	GType* self = ai_type_alloc(env, sizeof(GType), &type_vtable);

    self->_name = name;
    self->_loader = loader;

	return self;
}

static GType* cache_look(TypeCache* cache, GStr* name) {
	a_u32 id = name->_hash & cache->_hmask;
	for (GType* type = cache->_ptr[id]; type != null; type = type->_mnext) {
		if (ai_str_equals(name, type->_name)) {
			return type;
		}
	}
	return null;
}

static GType* loader_look(a_henv env, GLoader* loader, GStr* name, a_bool load) {
	if (loader == null) {
		return cache_look(&G(env)->_type_cache, name);
	}
	else {
		GType* type = cache_look(&loader->_cache, name);
		if (type == null) {
			type = loader_look(env, loader->_parent, name, load);
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

GType* ai_type_look(a_henv env, GLoader* loader, GStr* name, a_bool load) {
	return loader_look(env, loader, name, load);
}

static void cache_put_in_place(TypeCache* cache, GType* type) {
	a_u32 id = type->_name->_hash & cache->_hmask;
	GType** slot = &cache->_ptr[id];
	GType* type2;
	while ((type2 = *slot) != null) {
        slot = &type2->_mnext;
	}
	*slot = type;
}

static void cache_grow(a_henv env, TypeCache* cache) {
	a_usize old_cap = cache->_ptr != null ? cache->_hmask + 1 : 0;
	GType** old_ptr = cache->_ptr;

	a_usize new_cap = max(old_cap * 2, 4);
	GType** new_ptr = ai_mem_vnew(env, GType*, new_cap);

	memclr(new_ptr, sizeof(GType*) * new_cap);

	cache->_ptr = new_ptr;
	cache->_hmask = new_cap - 1;

	for (a_usize i = 0; i < old_cap; ++i) {
		GType* type = old_ptr[i];
		while (type != null) {
            GType* next = type->_mnext;
            type->_mnext = null;
			cache_put_in_place(cache, type);
            type = next;
		}
	}

	ai_mem_vdel(G(env), old_ptr, old_cap);
}

static void cache_put(a_henv env, TypeCache* cache, GType* type) {
	if (cache->_len >= cache->_hmask) {
		cache_grow(env, cache);
	}
	cache_put_in_place(cache, type);
	cache->_len += 1;
}

void ai_type_cache(a_henv env, GLoader* loader, GType* type) {
	TypeCache* cache = loader != null ? &loader->_cache : &G(env)->_type_cache;
	cache_put(env, cache, type);
    type->_loader = loader;
}

void ai_type_cache_mark(Global* g, TypeCache* cache) {
	for (a_u32 i = 0; i <= cache->_hmask; ++i) {
		GType** slot = &cache->_ptr[i];
		GType* type;
		while ((type = *slot) != null) {
			ai_gc_trace_mark(g, type);
            slot = &type->_mnext;
		}
	}
}

static void cache_drop(Global* g, TypeCache* cache) {
	if (cache->_ptr != null) {
		ai_mem_vdel(g, cache->_ptr, cache->_hmask + 1);
	}
}

void ai_type_boost(a_henv env) {
	Global* g = G(env);

    g->_types._nil = new(GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TNIL,
        ._name = env_int_str(env, STR_nil)
    };
    g->_types._bool = new(GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TBOOL,
        ._name = env_int_str(env, STR_bool)
    };
    g->_types._int = new(GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TINT,
        ._name = env_int_str(env, STR_int)
    };
    g->_types._float = new(GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TFLOAT,
        ._name = env_int_str(env, STR_float)
    };
    g->_types._ptr = new(GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TPTR,
        ._name = env_int_str(env, STR_ptr)
    };
    g->_types._str = new(GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TSTR,
        ._name = env_int_str(env, STR_str)
    };
    g->_types._tuple = new(GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TTUPLE,
        ._name = env_int_str(env, STR_tuple)
    };
    g->_types._list = new(GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TLIST,
        ._name = env_int_str(env, STR_list)
    };
    g->_types._table = new(GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TTABLE,
        ._name = env_int_str(env, STR_table)
    };
    g->_types._func = new(GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TFUNC,
        ._name = env_int_str(env, STR_func)
    };
    g->_types._route = new(GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TROUTE,
        ._name = env_int_str(env, STR_route)
    };
    g->_types._type = new(GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TTYPE,
        ._name = env_int_str(env, STR_type)
    };
}

Value ai_type_get(a_henv env, GType* self, Value vk) {
    if (!v_is_str(vk)) {
        ai_err_bad_key(env, g_nameof(env, self), v_nameof(env, vk));
    }

    GStr* k = v_as_str(vk);
    Value vv;
    a_msg msg = ai_dict_uget(env, &self->_fields, k, &vv);

    if (msg != ALO_SOK) {
        assume(msg == ALO_EEMPTY, "unexpected error.");
        ai_err_raisef(env, ALO_EXIMPL, "no meta entry '%s.%s'",
                      str2ntstr(self->_name),
                      str2ntstr(k));
    }

    return vv;
}

void ai_type_set(a_henv env, GType* self, Value vk, Value vv) {
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

a_msg ai_type_uget(a_henv env, GType* self, Value vk, Value* pv) {
    if (!v_is_str(vk)) return ALO_EINVAL;
    return ai_type_ugets(env, self, v_as_str(vk), pv);
}

a_msg ai_type_ugets(a_henv env, GType* self, GStr* k, Value* pv) {
    return ai_dict_uget(env, &self->_fields, k, pv);
}

a_msg ai_type_uset(a_henv env, GType* self, Value vk, Value vv) {
    if (!v_is_str(vk)) return ALO_EINVAL;
    return ai_type_usets(env, self, v_as_str(vk), vv);
}

a_msg ai_type_usets(a_henv env, GType* self, GStr* k, Value vv) {
    a_usize ctx;

    try(ai_dict_uset(env, &self->_fields, k, vv, &ctx));

    if (str_istm(k) && str_id(k) <= TM__FAST_MAX) {
        self->_flags |= TYPE_FLAG_FAST_TM(str_id(k));
    }

    self->_mver += 1;

    ai_gc_barrier_backward(env, self, k);
    ai_gc_barrier_backward_val(env, self, vv);

    return ALO_SOK;
}

static void type_mark(Global* g, GType* self) {
    if (self->_loader != null) {
        ai_gc_trace_mark(g, self->_loader);
    }
    if (self->_name != null) {
        ai_gc_trace_mark(g, self->_name);
    }
    ai_dict_mark(g, &self->_fields);
    ai_gc_trace_work(g, self->_size);
}

static void type_clean(Global* g, GType* self) {
    ai_dict_deinit(g, &self->_fields);
}

void ai_type_clean(Global* g) {
    type_clean(g, &g->_types._nil);
    type_clean(g, &g->_types._bool);
    type_clean(g, &g->_types._int);
    type_clean(g, &g->_types._float);
    type_clean(g, &g->_types._ptr);
    type_clean(g, &g->_types._tuple);
    type_clean(g, &g->_types._list);
    type_clean(g, &g->_types._table);
    type_clean(g, &g->_types._func);
    type_clean(g, &g->_types._route);
    type_clean(g, &g->_types._type);
    type_clean(g, &g->_types._mod);
    cache_drop(g, &g->_type_cache);
}

static void type_drop(Global* g, GType* self) {
    type_clean(g, self);
    ai_mem_dealloc(g, self, self->_size);
}

static VTable const type_vtable = {
	._stencil = V_STENCIL(T_TYPE),
    ._type_ref = g_type_ref(_type),
	._slots = {
        [vfp_slot(drop)] = type_drop,
        [vfp_slot(mark)] = type_mark
	}
};
