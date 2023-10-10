/**
 *@file atype.c
 */

#define atype_c_
#define ALO_LIB

#include "abuf.h"
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

GType* ai_type_alloc(a_henv env, a_usize size) {
	GType* self = ai_mem_alloc(env, size);
	memclr(self, size);

	self->_vptr = &type_vtable;
	self->_size = size;

	ai_gc_register_object(env, self);
	return self;
}

GType* ai_stype_new(a_henv env, GStr* name, GLoader* loader) {
	GType* self = ai_type_alloc(env, sizeof(GType));

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

static a_msg obj_get_field(a_henv env, GObj* self, GStr* k, Value* pv) {
    GType* type = g_typeof(env, self);

    Value v;
    try(ai_dict_uget(env, &type->_fields, k, &v));

    if (v_is_meta(v)) {
        Meta* meta = &type->_metas._ptr[v_get_payload(v)];

        switch (meta->_tags) {
            case META_MEMBER_FIELD_ANY: {
                Value* p = ptr_disp(Value, self, meta->_field_offset);
                v_cpy(env, pv, p);
                return ALO_SOK;
            }
            case META_MEMBER_FIELD_STR:
            case META_MEMBER_FIELD_OPT_STR: {
                a_hobj* pp = ptr_disp(a_hobj, self, meta->_field_offset);
                if (pp != null) {
                    v_set_obj(env, pv, *pp);
                }
                else {
                    v_set_nil(pv);
                }
                return ALO_SOK;
            }
            case META_MEMBER_FIELD_INT:
            case META_MEMBER_FIELD_UINT: {
                a_int* pi = ptr_disp(a_int, self, meta->_field_offset);
                v_set_int(pv, *pi);
                return ALO_SOK;
            }
            default: break;
        }
    }

    return ALO_EEMPTY;
}

static a_msg obj_set_field(a_henv env, GObj* self, GStr* k, Value vv) {
    GType* type = g_typeof(env, self);

    Value v;
    try(ai_dict_uget(env, &type->_fields, k, &v));

    if (v_is_meta(v)) {
        Meta* meta = &type->_metas._ptr[v_get_payload(v)];

        if (!(meta->_modifiers & META_MODIFIER_MUTABLE)) return ALO_EINVAL;

        switch (meta->_tags) {
            case META_MEMBER_FIELD_ANY: {
                Value* p = ptr_disp(Value, self, meta->_field_offset);
                v_set(env, p, v);
                return ALO_SOK;
            }
            case META_MEMBER_FIELD_STR: {
                if (!v_is_str(vv)) return ALO_EINVAL;
                GStr** pp = ptr_disp(GStr*, self, meta->_field_offset);
                *pp = v_as_str(vv);
                return ALO_SOK;
            }
            case META_MEMBER_FIELD_OPT_STR: {
                if (!(v_is_str(vv) || v_is_nil(vv))) return ALO_EINVAL;
                GStr** pp = ptr_disp(GStr*, self, meta->_field_offset);
                *pp = v_is_nil(vv) ? null : v_as_str(vv);
                return ALO_SOK;
            }
            case META_MEMBER_FIELD_INT:
            case META_MEMBER_FIELD_UINT: {
                if (!(v_is_int(vv))) return ALO_EINVAL;
                a_int* pi = ptr_disp(a_int, self, meta->_field_offset);
                *pi = v_as_int(vv);
                return ALO_SOK;
            }
            default: return ALO_EINVAL;
        }
    }

    return ALO_EEMPTY;
}

static Value type_get_meta_mirror(a_henv env, GType* self, Meta* meta) {
    if (!v_is_empty(meta->_mirror))
        return meta->_mirror;

    quiet(env);

    switch (meta->_tags) {
        case META_CONST_FIELD: panic("constant field always has mirror.");
        case META_STATIC_FIELD: panic("static field always has mirror.");
        case META_MEMBER_METHOD: panic("member method always has mirror.");
        default: unreachable(); //TODO
    }
}

static Value type_unwrap_maybe_meta(a_henv env, GType* self, Value v) {
    return v_is_meta(v) ? type_get_meta_mirror(env, self, &self->_metas._ptr[v_get_payload(v)]) : v;
}

Value ai_type_get(a_henv env, GType* self, Value vk) {
    if (!v_is_str(vk)) {
        ai_err_bad_key(env, g_nameof(env, self), v_nameof(env, vk));
    }

    GStr* k = v_as_str(vk);
    Value vv;

    a_msg msg = obj_get_field(env, gobj_cast(self), k, &vv);

    if (msg == ALO_SOK) return vv;

    msg = ai_dict_uget(env, &self->_fields, k, &vv);

    if (msg != ALO_SOK) {
        assume(msg == ALO_EEMPTY, "unexpected error.");
        ai_err_raisef(env, ALO_EXIMPL, "no meta entry '%s.%s'",
                      str2ntstr(self->_name),
                      str2ntstr(k));
    }

    return type_unwrap_maybe_meta(env, self, vv);
}

void ai_type_set(a_henv env, GType* self, Value vk, Value vv) {
    if (!v_is_str(vk)) {
        ai_err_bad_key(env, g_nameof(env, self), v_nameof(env, vk));
    }

    GStr* k = v_as_str(vk);

    a_msg msg = obj_set_field(env, gobj_cast(self), k, vv);

    if (msg == ALO_SOK)
        return;
    if (msg == ALO_EINVAL)
        goto invalid;

    msg = ai_type_usets(env, self, v_as_str(vk), vv);

    if (msg == ALO_EINVAL)
        goto invalid;

    return;

invalid:
    ai_err_raisef(env, msg, "cannot overwrite field for type: '%s.%s'",
                  str2ntstr(self->_name),
                  str2ntstr(v_as_str(vk)));
}

a_msg ai_type_uget(a_henv env, GType* self, Value vk, Value* pv) {
    if (!v_is_str(vk)) return ALO_EINVAL;
    return ai_type_ugets(env, self, v_as_str(vk), pv);
}

a_msg ai_type_ugets(a_henv env, GType* self, GStr* k, Value* pv) {
    Value v;
    a_msg msg = obj_get_field(env, gobj_cast(self), k, pv);
    if (msg != ALO_EEMPTY) return msg;

    try(ai_dict_uget(env, &self->_fields, k, &v));

    *pv = type_unwrap_maybe_meta(env, self, v);
    return ALO_SOK;
}

a_msg ai_type_uset(a_henv env, GType* self, Value vk, Value vv) {
    if (!v_is_str(vk)) return ALO_EINVAL;
    return ai_type_usets(env, self, v_as_str(vk), vv);
}

a_msg ai_type_usets(a_henv env, GType* self, GStr* k, Value vv) {
    a_usize ctx;

    a_msg msg = obj_set_field(env, gobj_cast(self), k, vv);
    if (msg != ALO_EEMPTY) return msg;

    Value* pv = ai_dict_uref(env, &self->_fields, k, &ctx);
    if (pv != null) {
        Value v = *pv;
        if (v_is_meta(v)) {
            return ALO_EINVAL;
        }
        v_set(env, pv, vv);
    }
    else {
        try(ai_dict_uput(env, &self->_fields, k, vv, &ctx));

        if (str_istm(k) && str_id(k) <= TM__FAST_MAX) {
            self->_flags |= TYPE_FLAG_FAST_TM(str_id(k));
        }
    }

    self->_mver += 1;

    ai_gc_barrier_backward(env, self, k);
    ai_gc_barrier_backward_val(env, self, vv);

    return ALO_SOK;
}

a_msg ai_obj_vlook(a_henv env, Value v, GStr* k, Value* pv) {
    GType* type = v_typeof(env, v);

    Value vm;
    try(ai_dict_uget(env, &type->_fields, k, &vm));

    if (v_is_meta(v)) {
        Meta* meta = &type->_metas._ptr[v_get_payload(v)];

        if (meta->_modifiers & META_MODIFIER_MEMBER_VISIBLE) {
            v_set(env, pv, type_get_meta_mirror(env, type, meta));
            return ALO_SOK;
        }

        return ALO_EEMPTY;
    }

    return ALO_EEMPTY;
}

a_msg ai_dyn_ugets(a_henv env, GObj* self, GStr* k, Value* pv) {
    a_msg msg = obj_get_field(env, self, k, pv);
    if (msg == ALO_SOK) return ALO_SOK;

    return msg;
}

static void metas_hint1(a_henv env, Metas* metas) {
    if (metas->_len == metas->_cap) {
        a_usize old_cap = metas->_cap;
        a_usize new_cap = old_cap;
        catch(ai_buf_nhint(&new_cap, metas->_len, 1, INT32_MAX), ai_buf_error, env, "meta");

        metas->_ptr = ai_mem_vgrow(env, metas->_ptr, old_cap, new_cap);
        metas->_cap = new_cap;
    }
}

static void type_new_meta_fast_inplace(a_henv env, GType* self, char const* key, Meta meta) {
    Metas* metas = &self->_metas;
    metas_hint1(env, metas);

    a_u32 id = metas->_len++;
    metas->_ptr[id] = meta;

    ai_dict_hint(env, &self->_fields, 1);
    ai_dict_put_inplace(env, &self->_fields, ai_str_newc(env, key), v_of_meta(id));
}

static void type_clean(Global* g, GType* self) {
    ai_dict_deinit(g, &self->_fields);
    if (self->_metas._ptr != null) {
        ai_mem_vdel(g, self->_metas._ptr, self->_metas._cap);
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

    type_new_meta_fast_inplace(env, g_type(env, _type), "__name__", new(Meta) {
        ._mirror = v_of_empty(),
        ._tags = META_MEMBER_FIELD_STR,
        ._field_offset = offsetof(GType, _name),
        ._modifiers = 0,
    });
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

static void metas_mark(Global* g, Metas* metas) {
    for (a_u32 i = 0; i < metas->_cap; ++i) {
        ai_gc_trace_mark_val(g, metas->_ptr[i]._mirror);
    }
}

static void type_mark(Global* g, GType* self) {
    if (self->_loader != null) {
        ai_gc_trace_mark(g, self->_loader);
    }
    if (self->_name != null) {
        ai_gc_trace_mark(g, self->_name);
    }
    metas_mark(g, &self->_metas);
    ai_dict_mark(g, &self->_fields);
    ai_gc_trace_work(g, self->_size);
}

static VTable const type_vtable = {
	._stencil = V_STENCIL(T_TYPE),
    ._type_ref = g_type_ref(_type),
	._slots = {
        [vfp_slot(drop)] = type_drop,
        [vfp_slot(mark)] = type_mark
	}
};
