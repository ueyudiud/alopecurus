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
    GTuple* self = ai_mem_alloc(env, sizeof_GTuple(len));

	self->_vptr = &tuple_vtable;
    self->_len = len;
	self->_hash = 0;
	v_cpy_all(env, self->_ptr, src, len);

    ai_gc_register_object(env, self);
    return self;
}

Value const* ai_tuple_refi(unused a_henv env, GTuple* self, a_int key) {
    a_uint i = obj_idx(key, self->_len, null);
    return &self->_ptr[i];
}

a_bool ai_tuple_equals(a_henv env, GTuple* self, GTuple* other) {
	if (self->_len != other->_len)
		return false;
	for (a_u32 i = 0; i < self->_len; ++i) {
		if (!ai_vm_equals(env, self->_ptr[i], other->_ptr[i]))
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

Value ai_tuple_get(a_henv env, GTuple* self, Value key) {
	if (unlikely(!v_is_int(key))) {
        ai_err_bad_key(env, "tuple", v_nameof(env, key));
	}
	return ai_tuple_geti(env, self, v_as_int(key));
}

Value ai_tuple_geti(a_henv env, GTuple* self, a_int key) {
	Value const* value = ai_tuple_refi(env, self, key);
	if (unlikely(value == null)) {
		ai_err_raisef(env, ALO_EINVAL, "index out of bound.");
	}
	return *value;
}

a_msg ai_tuple_ugeti(a_henv env, GTuple* self, a_int key, Value* pval) {
    a_uint i = obj_idx(key, self->_len, ALO_EINVAL);
    v_set(env, pval, self->_ptr[i]);
    return ALO_SOK;
}

a_msg ai_tuple_uget(a_henv env, GTuple* self, Value key, Value* pval) {
    if (!v_is_int(key)) return ALO_EINVAL;
    return ai_tuple_ugeti(env, self, v_as_int(key), pval);
}

static void tuple_drop(Global* g, GTuple* self) {
    ai_mem_dealloc(g, self, sizeof_GTuple(self->_len));
}

static void tuple_mark(Global* g, GTuple* self) {
    a_u32 len = self->_len;
    for (a_u32 i = 0; i < len; ++i) {
        ai_gc_trace_mark_val(g, self->_ptr[i]);
    }
    ai_gc_trace_work(g, sizeof_GTuple(self->_len));
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
