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
#include "ameta.h"
#include "avm.h"
#include "aerr.h"

#include "atype.h"

#define dead_key ((GStr*) null)

static VTable const type_vtable;

GType* ai_type_new(a_henv env, GStr* name, GLoader* loader, a_usize extra) {
    a_usize size = pad_to(sizeof(GType) + extra, sizeof(a_usize));

    GType* self = ai_mem_alloc(env, size);
    memclr(self, size);

    self->_vptr = &type_vtable;
    self->_size = size;
    self->_sig = name;
    self->_loader = loader;

    ai_gc_register_object(env, self);
    return self;
}

static Meta* metas_get(Metas* metas, GStr* key) {
    if (metas->_len == 0) return null;

    a_u32 index = key->_hash & metas->_hmask;
    a_u32 perturb = key->_hash;

    loop {
        Meta* meta = &metas->_ptr[index];
        if (meta->_key == key) {
            return meta;
        }
        if (meta->_key == null) {
            return null;
        }

        perturb >>= 5;
        index = (index * 5 + perturb + 1) & metas->_hmask;
    }
}

static void metas_put_inplace(a_henv env, Metas* metas, GStr* key, Value value) {
    a_u32 index = key->_hash & metas->_hmask;
    a_u32 perturb = key->_hash;

    loop {
        Meta* meta = &metas->_ptr[index];
        if (meta->_key == null) {
            meta->_key = key;
            v_set(env, &meta->_value, value);
            break;
        }

        perturb >>= 5;
        index = (index * 5 + perturb + 1) & metas->_hmask;
    }
}

static a_bool metas_need_grow(Metas* metas) {
    return metas->_len >= (metas->_hmask + 1) / 4 * 3;
}

static void metas_resize(a_henv env, Metas* metas, a_u32 old_cap, a_u32 new_cap) {
    Meta* old_ptr = metas->_ptr;
    Meta* new_ptr = ai_mem_vnew(env, Meta, new_cap);

    memclr(new_ptr, sizeof(Meta) * new_cap);
    metas->_ptr = new_ptr;
    metas->_hmask = new_cap - 1;

    if (old_ptr != null) {
        for (a_u32 i = 0; i < old_cap; ++i) {
            Meta* meta = &old_ptr[i];
            if (meta->_key > dead_key) {
                metas_put_inplace(env, metas, meta->_key, meta->_value);
            }
        }

        ai_mem_vdel(G(env), old_ptr, old_cap);
    }
}

static void metas_grow(a_henv env, Metas* metas) {
    assume(metas->_ptr != null, "grow nothing.");

    a_u32 old_cap = metas->_hmask + 1;
    a_u32 new_cap = old_cap << 1;

    metas_resize(env, metas, old_cap, new_cap);
}

static a_bool metas_set(a_henv env, Metas* metas, GStr* key, Value value) {
    if (unlikely(metas->_ptr == null)) {
        metas_resize(env, metas, 0, 4);
        metas_put_inplace(env, metas, key, value);
    }
    else {
        a_bool need_grow = metas_need_grow(metas);
        a_bool need_seek = !need_grow;
        Meta* meta_slot = null;

        a_u32 index = key->_hash & metas->_hmask;
        a_u32 perturb = key->_hash;

        loop {
            Meta* meta = &metas->_ptr[index];
            if (meta->_key == key) {
                v_set(env, &meta->_value, value);
                return true;
            }
            if (meta->_key == dead_key) {
                if (need_seek) {
                    meta_slot = meta;
                    need_seek = false;
                }
            }
            if (meta->_key == null) {
                if (need_seek) {
                    meta_slot = meta;
                }
                break;
            }

            perturb >>= 5;
            index = (index * 5 + perturb + 1) & metas->_hmask;
        }

        if (meta_slot != null) {
            meta_slot->_key = key;
            v_set(env, &meta_slot->_value, value);
        }
        else {
            if (need_grow) metas_grow(env, metas);
            metas_put_inplace(env, metas, key, value);
        }
    }

    metas->_len += 1;
    return false;
}

static Value type_gets(a_henv env, GType* self, GStr* key) {
    Meta* meta = metas_get(&self->_metas, key);
    if (meta != null) {
        return meta->_value;
    }

    Value v;
    catch (ai_meta_get(env, v_of_obj(self), v_of_obj(key), &v)) {
        goto invalid;
    }

    return v;

invalid:
    ai_err_raisef(env, ALO_EXIMPL, "cannot access field for type: '%s.%s'",
                  str2ntstr(self->_sig),
                  str2ntstr(key));
}

static void type_sets(a_henv env, GType* self, GStr* key, Value value) {
    Meta* meta = metas_get(&self->_metas, key);

    if (meta != null) {
        v_set(env, &meta->_value, value);
        self->_mver += 1;

        ai_gc_barrier_forward_val(env, self, value);
    }
    else {
        catch (ai_meta_set(env, v_of_obj(self), v_of_obj(key), value)) {
            metas_set(env, &self->_metas, key, value);
            self->_mver += 1;

            ai_gc_barrier_forward(env, self, key);
            ai_gc_barrier_forward_val(env, self, value);
            break;
        }
    }
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
    Meta* meta = metas_get(&self->_metas, k);
    if (meta == null) return ALO_EEMPTY;

    v_set(env, pv, meta->_value);
    return ALO_SOK;
}

a_msg ai_type_uset(a_henv env, GType* self, Value vk, Value vv) {
    if (!v_is_str(vk)) return ALO_EINVAL;
    return ai_type_usets(env, self, v_as_str(vk), vv);
}

a_msg ai_type_usets(a_henv env, GType* self, GStr* k, Value vv) {
    metas_set(env, &self->_metas, k, vv);

    ai_gc_barrier_forward(env, self, k);
    ai_gc_barrier_forward_val(env, self, vv);

    self->_mver += 1;

    return ALO_SOK;
}

a_msg ai_obj_ulook(a_henv env, Value v, GStr* k, Value* pv) {
    GType* type = v_typeof(env, v);

    Meta* meta = metas_get(&type->_metas, k);
    if (meta == null) return ALO_EEMPTY;

    v_cpy(env, pv, &meta->_value);
    return ALO_SOK;
}

static GType* cache_look(TypeCache* cache, GStr* name) {
	a_u32 id = name->_hash & cache->_hmask;
	for (GType* type = cache->_ptr[id]; type != null; type = type->_mnext) {
		if (name == type->_sig) {
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
	a_u32 id = type->_sig->_hash & cache->_hmask;
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

static void type_clean(Global* g, GType* self) {
    run {
        Metas* metas = &self->_metas;
        if (metas->_ptr != null) {
            ai_mem_vdel(g, metas->_ptr, metas->_hmask + 1);
        }
    }
}

void ai_type_boost(a_henv env) {
    Global* g = G(env);

    g->_types._nil = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TNIL,
        ._sig = g_str(env, STR_nil)
    };
    g->_types._bool = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TBOOL,
        ._sig = g_str(env, STR_bool)
    };
    g->_types._int = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TINT,
        ._sig = g_str(env, STR_int)
    };
    g->_types._float = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TFLOAT,
        ._sig = g_str(env, STR_float)
    };
    g->_types._ptr = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TPTR,
        ._sig = g_str(env, STR_ptr)
    };
    g->_types._str = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TSTR,
        ._sig = g_str(env, STR_str)
    };
    g->_types._tuple = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TTUPLE,
        ._sig = g_str(env, STR_tuple)
    };
    g->_types._list = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TLIST,
        ._sig = g_str(env, STR_list)
    };
    g->_types._table = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TTABLE,
        ._sig = g_str(env, STR_table)
    };
    g->_types._func = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TFUNC,
        ._sig = g_str(env, STR_func)
    };
    g->_types._route = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TROUTE,
        ._sig = g_str(env, STR_route)
    };
    g->_types._type = (GType) {
        ._vptr = &type_vtable,
        ._flags = TYPE_FLAG_NONE,
        ._tag = ALO_TTYPE,
        ._sig = g_str(env, STR_type)
    };
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
    if (metas->_ptr != null) {
        a_u32 cap = metas->_hmask + 1;
        for (a_u32 i = 0; i < cap; ++i) {
            Meta* meta = &metas->_ptr[i];
            if (meta->_key > dead_key) {
                ai_gc_trace_mark(g, meta->_key);
                ai_gc_trace_mark_val(g, meta->_value);
            }
        }
        ai_gc_trace_work(g, sizeof(Meta) * cap);
    }
}

static void type_mark(Global* g, GType* self) {
    if (self->_loader != null) {
        ai_gc_trace_mark(g, self->_loader);
    }
    if (self->_sig != null) {
        ai_gc_trace_mark(g, self->_sig);
    }
    metas_mark(g, &self->_metas);
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
