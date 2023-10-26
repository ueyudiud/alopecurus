/**
 *@file atype.c
 */

#define atype_c_
#define ALO_LIB

#include "abuf.h"
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

    self->_id = name;
    self->_loader = loader;

	return self;
}

static GStr* str_from_key(a_usize key) {
    return ptr_of(GStr, key & ~usizec(0x7));
}

static a_usize key_from(GStr* str, a_usize cat) {
    return addr_of(str) | cat;
}

#define dead_key usizec(8)

#define TKey a_usize
#define Node MetaRef
#define Dict MetaTable

#define node_is_empty(n) ((n)->_key <= dead_key)
#define node_is_ended(n) ((n)->_key == 0)
#define node_match(n,k) ((n)->_key == (k))
#define node_init(n) init(n) { }
#define node_erase(n) quiet((n)->_key = dead_key)

#define key_hash(k) (str_from_key(k)->_hash)

#include "adict.h"

static Value* type_get_meta_ref(a_henv env, GType* self, GStr* key, a_usize cat) {
    return at_dict_get(env, &self->_table, key_from(key, cat));
}

static a_msg obj_get_field(a_henv env, GObj* self, GStr* key, Value* pv) {
    GType* type = g_typeof(env, self);

    Value const* pv_meta = type_get_meta_ref(env, type, key, TYPE_META_CAT_GETTER);
    if (pv_meta == null) return ALO_EEMPTY;

    Value v_meta = *pv_meta;
    Meta* meta = &type->_metas._ptr[v_as_stub(v_meta)];

    switch (meta->_tag) {
        case META_MEMBER_FIELD_ANY: {
            Value v = *ptr_disp(Value, self, meta->_gptr);
            v_set(env, pv, v);
            return ALO_SOK;
        }
        case META_MEMBER_FIELD_STR:
        case META_MEMBER_FIELD_OPT_STR: {
            a_hobj p = *ptr_disp(a_hobj, self, meta->_gptr);
            if (p != null) {
                v_set_obj(env, pv, p);
            }
            else {
                v_set_nil(pv);
            }
            return ALO_SOK;
        }
        case META_MEMBER_FIELD_INT:
        case META_MEMBER_FIELD_UINT: {
            a_int i = *ptr_disp(a_int, self, meta->_gptr);
            v_set_int(pv, i);
            return ALO_SOK;
        }
        default: return ALO_EEMPTY;
    }
}

static a_msg obj_set_field(a_henv env, GObj* self, GStr* key, Value vv) {
    GType* type = g_typeof(env, self);

    Value const* pv_meta = type_get_meta_ref(env, type, key, TYPE_META_CAT_SETTER);
    if (pv_meta == null) return ALO_EEMPTY;

    Value v_meta = *pv_meta;
    Meta* meta = &type->_metas._ptr[v_as_stub(v_meta)];

    switch (meta->_tag) {
        case META_MEMBER_FIELD_ANY: {
            Value* p = ptr_disp(Value, self, meta->_gptr);
            v_set(env, p, v_meta);
            return ALO_SOK;
        }
        case META_MEMBER_FIELD_STR: {
            if (!v_is_str(vv)) return ALO_EINVAL;
            GStr** pp = ptr_disp(GStr*, self, meta->_gptr);
            *pp = v_as_str(vv);
            return ALO_SOK;
        }
        case META_MEMBER_FIELD_OPT_STR: {
            if (!(v_is_str(vv) || v_is_nil(vv))) return ALO_EINVAL;
            GStr** pp = ptr_disp(GStr*, self, meta->_gptr);
            *pp = v_is_nil(vv) ? null : v_as_str(vv);
            return ALO_SOK;
        }
        case META_MEMBER_FIELD_INT:
        case META_MEMBER_FIELD_UINT: {
            if (!(v_is_int(vv))) return ALO_EINVAL;
            a_int* pi = ptr_disp(a_int, self, meta->_gptr);
            *pi = v_as_int(vv);
            return ALO_SOK;
        }
        default: return ALO_EINVAL;
    }
}

static Value type_get_meta_mirror(a_henv env, GType* self, Meta* meta) {
    if (!v_is_empty(meta->_slot))
        return meta->_slot;

    quiet(env);

    switch (meta->_tag) {
        case META_CONST_FIELD: panic("constant field always has mirror.");
        case META_STATIC_FIELD: panic("static field always has mirror.");
        case META_MEMBER_METHOD: panic("member method always has mirror.");
        default: unreachable(); //TODO
    }
}

static Value type_unwrap_maybe_meta(a_henv env, GType* self, Value v) {
    return v_is_stub(v) ? type_get_meta_mirror(env, self, &self->_metas._ptr[v_as_stub(v)]) : v;
}

static Value type_gets(a_henv env, GType* self, GStr* key) {
    Value v;

    a_msg msg = obj_get_field(env, gobj_cast(self), key, &v);

    if (msg == ALO_SOK)
        return v;

    Value const* p = type_get_meta_ref(env, self, key, TYPE_META_CAT_STATIC);

    if (p == null)
        goto invalid;

    return type_unwrap_maybe_meta(env, self, *p);

invalid:
    ai_err_raisef(env, ALO_EXIMPL, "cannot access field for type: '%s.%s'",
                  str2ntstr(self->_id),
                  str2ntstr(key));
}

static void type_sets(a_henv env, GType* self, GStr* key, Value value) {
    a_msg msg = obj_set_field(env, gobj_cast(self), key, value);

    if (msg == ALO_SOK)
        return;
    if (msg == ALO_EINVAL)
        goto invalid;

    Value* p = type_get_meta_ref(env, self, key, TYPE_META_CAT_STATIC);

    if (p != null) {
        Value v = *p;
        if (v_is_stub(v)) {
            Meta* meta = &self->_metas._ptr[v_as_stub(v)];
            if (!(meta->_modifiers & META_MODIFIER_CONFIGURABLE))
                goto invalid;
        }

        v_set(env, p, value);
    }
    else {
        catch (at_dict_put(env, &self->_table, key_from(key, TYPE_META_CAT_STATIC), value), _) {
            ai_err_raisef(env, ALO_EINVAL, "too many fields.");
        }

        ai_gc_barrier_forward(env, self, key);
    }

    self->_mver += 1;

    ai_gc_barrier_forward_val(env, self, value);

    return;

invalid:
    ai_err_raisef(env, msg, "cannot overwrite field for type: '%s.%s'",
                  str2ntstr(self->_id),
                  str2ntstr(key));
}

Value ai_type_get(a_henv env, GType* self, Value vk) {
    if (!v_is_str(vk)) {
        ai_err_bad_key(env, g_nameof(env, self), v_nameof(env, vk));
    }
    return type_gets(env, self, v_as_str(vk));
}

void ai_type_set(a_henv env, GType* self, Value vk, Value vv) {
    if (!v_is_str(vk)) {
        ai_err_bad_key(env, g_nameof(env, self), v_nameof(env, vk));
    }

    type_sets(env, self, v_as_str(vk), vv);
}

a_msg ai_type_uget(a_henv env, GType* self, Value vk, Value* pv) {
    if (!v_is_str(vk)) return ALO_EINVAL;
    return ai_type_ugets(env, self, v_as_str(vk), pv);
}

a_msg ai_type_ugets(a_henv env, GType* self, GStr* k, Value* pv) {
    a_msg msg = obj_get_field(env, gobj_cast(self), k, pv);
    if (msg != ALO_EEMPTY) return msg;

    Value const* p = type_get_meta_ref(env, self, k, TYPE_META_CAT_STATIC);

    if (p == null) return ALO_EEMPTY;

    v_set(env, pv, type_unwrap_maybe_meta(env, self, *p));
    return ALO_SOK;
}

a_msg ai_type_uset(a_henv env, GType* self, Value vk, Value vv) {
    if (!v_is_str(vk)) return ALO_EINVAL;
    return ai_type_usets(env, self, v_as_str(vk), vv);
}

a_msg ai_type_usets(a_henv env, GType* self, GStr* k, Value vv) {
    a_msg msg = obj_set_field(env, gobj_cast(self), k, vv);

    if (msg != ALO_EEMPTY)
        return msg;

    Value* p = type_get_meta_ref(env, self, k, TYPE_META_CAT_STATIC);

    if (p != null) {
        Value v = *p;
        if (v_is_stub(v)) {
            Meta* meta = &self->_metas._ptr[v_as_stub(v)];
            if (!(meta->_modifiers & META_MODIFIER_CONFIGURABLE))
                return ALO_EINVAL;
        }

        v_set(env, p, vv);
    }
    else {
        catch (at_dict_put(env, &self->_table, addr_of(k) | TYPE_META_CAT_STATIC, vv), _) {
            return ALO_EINVAL;
        }

        ai_gc_barrier_forward(env, self, k);
    }

    self->_mver += 1;

    ai_gc_barrier_forward_val(env, self, vv);
}

a_msg ai_obj_vlook(a_henv env, Value v, GStr* k, Value* pv) {
    GType* type = v_typeof(env, v);

    Value const* pv_meta = type_get_meta_ref(env, type, k, TYPE_META_CAT_MEMBER);
    if (pv_meta == null) return ALO_EEMPTY;

    *pv = type_unwrap_maybe_meta(env, type, *pv_meta);
    return ALO_SOK;
}

static GType* cache_look(TypeCache* cache, GStr* name) {
	a_u32 id = name->_hash & cache->_hmask;
	for (GType* type = cache->_ptr[id]; type != null; type = type->_mnext) {
		if (ai_str_equals(name, type->_id)) {
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
	a_u32 id = type->_id->_hash & cache->_hmask;
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

static void metas_hint1(a_henv env, Metas* metas) {
    if (metas->_len == metas->_cap) {
        a_usize old_cap = metas->_cap;
        a_usize new_cap = old_cap;
        catch(ai_buf_nhint(&new_cap, metas->_len, 1, INT32_MAX), msg) {
            ai_buf_error(msg, env, "meta");
        }

        metas->_ptr = ai_mem_vgrow(env, metas->_ptr, old_cap, new_cap);
        metas->_cap = new_cap;
    }
}

static void type_new_meta_fast_inplace(a_henv env, GType* self, char const* name, a_enum cat, Meta meta) {
    Metas* metas = &self->_metas;
    metas_hint1(env, metas);

    a_u32 id = metas->_len++;
    metas->_ptr[id] = meta;

    at_dict_hint(env, &self->_table, 2);

    GStr* str = ai_str_newc(env, name);

    at_dict_put(env, &self->_table, key_from(str, TYPE_META_CAT_STATIC), v_of_stub(id));
    at_dict_put(env, &self->_table, key_from(str, cat), v_of_stub(id));
}

static void type_clean(Global* g, GType* self) {
    at_dict_deinit(g, &self->_table);
    if (self->_metas._ptr != null) {
        ai_mem_vdel(g, self->_metas._ptr, self->_metas._cap);
    }
}

void ai_type_boost(a_henv env) {
    Global* g = G(env);

    g->_types._nil = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TNIL,
        ._id = g_str(env, STR_nil)
    };
    g->_types._bool = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TBOOL,
        ._id = g_str(env, STR_bool)
    };
    g->_types._int = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TINT,
        ._id = g_str(env, STR_int)
    };
    g->_types._float = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TFLOAT,
        ._id = g_str(env, STR_float)
    };
    g->_types._ptr = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TPTR,
        ._id = g_str(env, STR_ptr)
    };
    g->_types._str = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TSTR,
        ._id = g_str(env, STR_str)
    };
    g->_types._tuple = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TTUPLE,
        ._id = g_str(env, STR_tuple)
    };
    g->_types._list = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TLIST,
        ._id = g_str(env, STR_list)
    };
    g->_types._table = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TTABLE,
        ._id = g_str(env, STR_table)
    };
    g->_types._func = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TFUNC,
        ._id = g_str(env, STR_func)
    };
    g->_types._route = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TROUTE,
        ._id = g_str(env, STR_route)
    };
    g->_types._type = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TTYPE,
        ._id = g_str(env, STR_type)
    };

    type_new_meta_fast_inplace(env, g_type(env, _type), "__id__", TYPE_META_CAT_GETTER, (Meta) {
        ._gptr = offsetof(GType, _id),
        ._tag = META_MEMBER_FIELD_STR,
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
    cache_drop(g, &g->_type_cache);
}

static void type_drop(Global* g, GType* self) {
    type_clean(g, self);
    ai_mem_dealloc(g, self, self->_size);
}

static void metas_mark(Global* g, Metas* metas) {
    for (a_u32 i = 0; i < metas->_cap; ++i) {
        ai_gc_trace_mark_val(g, metas->_ptr[i]._slot);
    }
}

static void type_mark(Global* g, GType* self) {
    if (self->_loader != null) {
        ai_gc_trace_mark(g, self->_loader);
    }
    if (self->_id != null) {
        ai_gc_trace_mark(g, self->_id);
    }
    metas_mark(g, &self->_metas);
    if (self->_table._ptr != null) {
        MetaRef* ref;
        at_dict_for(&self->_table, ref) {
            if (ref->_key > dead_key) {
                ai_gc_trace_mark(g, str_from_key(ref->_key));
                ai_gc_trace_mark_val(g, ref->_value);
            }
        }
    }
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
