/**
 *@file atuple.c=
 */

#define atuple_c_
#define ALO_LIB

#include "amem.h"
#include "agc.h"
#include "aerr.h"
#include "avm.h"

#include "atuple.h"

static VTable const tuple_vtable;

GTuple* ai_tuple_new(a_henv env, Value const* src, a_usize len) {
    if (len > INT32_MAX) ai_err_raisef(env, ALO_EINVAL, "tuple size too large.");

    GTuple* self = ai_mem_gnew(env, GTuple, tuple_size(len));

	self->_vptr = &tuple_vtable;
    self->_len = len;
	self->_hash = 0;
	v_cpy_all(env, self->_ptr, src, len);

    ai_gc_register_object(env, self);
    return self;
}

a_bool ai_tuple_equals(a_henv env, GTuple* self, GTuple* o) {
	if (self->_len != o->_len)
		return false;
	for (a_u32 i = 0; i < self->_len; ++i) {
		if (!ai_vm_equals(env, self->_ptr[i], o->_ptr[i]))
			return false;
	}
	return true;
}

a_hash ai_tuple_hash(a_henv env, GTuple* self) {
	if (self->_hash == 0) {
		a_hash hash = G(env)->_seed;
		for (a_u32 i = 0; i < self->_len; ++i) {
            hash = hash * 31 + ai_vm_hash(env, self->_ptr[i]);
		}
		self->_hash = hash != 0 ? hash : 1;
	}
	return self->_hash;
}

Value ai_tuple_get(a_henv env, GTuple* self, Value vk) {
	if (unlikely(!v_is_int(vk))) {
        ai_err_bad_key(env, "tuple", v_nameof(env, vk));
	}
	return ai_tuple_geti(env, self, v_as_int(vk));
}

static Value const* tuple_refi(unused a_henv env, GTuple* self, a_int key) {
    a_uint i = obj_idx(key, self->_len, null);
    return &self->_ptr[i];
}

Value ai_tuple_geti(a_henv env, GTuple* self, a_int k) {
	Value const* value = tuple_refi(env, self, k);
	if (unlikely(value == null)) {
		ai_err_raisef(env, ALO_EINVAL, "index out of bound.");
	}
	return *value;
}

a_msg ai_tuple_ugeti(a_henv env, GTuple* self, a_int k, Value* pv) {
    a_uint i = obj_idx(k, self->_len, ALO_EINVAL);
    v_set(env, pv, self->_ptr[i]);
    return ALO_SOK;
}

a_msg ai_tuple_uget(a_henv env, GTuple* self, Value vk, Value* pv) {
    if (!v_is_int(vk)) return ALO_EINVAL;
    return ai_tuple_ugeti(env, self, v_as_int(vk), pv);
}

static void tuple_drop(Global* g, GTuple* self) {
    ai_mem_gdel(g, self, tuple_size(self->_len));
}

static void tuple_mark(Global* g, GTuple* self) {
    a_u32 len = self->_len;
    for (a_u32 i = 0; i < len; ++i) {
        ai_gc_trace_mark_val(g, self->_ptr[i]);
    }
    ai_gc_trace_work(g, tuple_size(self->_len));
}

static VTable const tuple_vtable = {
    ._stencil = V_STENCIL(T_TUPLE),
    ._flags = VTABLE_FLAG_NONE,
    ._type_ref = g_type_ref(_tuple),
    ._slots = {
        [vfp_slot(drop)] = tuple_drop,
        [vfp_slot(mark)] = tuple_mark
    }
};
