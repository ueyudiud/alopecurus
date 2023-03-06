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
    GTuple* self = ai_mem_alloc(env, tuple_size(len));
	self->_vtable = &tuple_vtable;
    self->_len = len;
	self->_hash = 0;
	v_cpy_multi(env, self->_body, src, len);
    ai_gc_register_object(env, self);
    return self;
}

Value const* ai_tuple_refi(unused a_henv env, GTuple* self, a_isize pos) {
    if (pos >= 0) {
        if (pos >= self->_len) {
            return null;
        }
    }
    else {
        pos += self->_len;
        if (pos < 0) {
            return null;
        }
    }
    return &self->_body[pos];
}

a_bool ai_tuple_equals(a_henv env, GTuple* self, GTuple* other) {
	if (self->_len != other->_len)
		return false;
	for (a_u32 i = 0; i < self->_len; ++i) {
		if (!ai_vm_equals(env, self->_body[i], other->_body[i]))
			return false;
	}
	return true;
}

a_hash ai_tuple_hash(a_henv env, GTuple* self) {
	if (self->_hash == 0) {
		a_hash hash = G(env)->_seed;
		for (a_u32 i = 0; i < self->_len; ++i) {
			hash += hash * 31 + ai_vm_hash(env, self->_body[i]);
		}
		self->_hash = hash != 0 ? hash : 1;
	}
	return self->_hash;
}

static a_bool tuple_equals(a_henv env, GTuple* self, Value other) {
	return v_is_tuple(other) && ai_tuple_equals(env, self, v_as_tuple(other));
}

static void tuple_mark(Global* g, GTuple* self) {
    a_u32 len = self->_len;
    for (a_u32 i = 0; i < len; ++i) {
		ai_gc_trace_mark_val(g, self->_body[i]);
    }
	ai_gc_trace_work(g, tuple_size(len));
}

static void tuple_drop(Global* g, GTuple* self) {
    ai_mem_dealloc(g, self, tuple_size(self->_len));
}

static Value tuple_get(a_henv env, GTuple* self, Value key) {
	if (unlikely(!v_is_int(key))) {
		ai_err_raisef(env, ALO_EINVAL, "bad index for tuple.");
	}
	Value const* value = ai_tuple_refi(env, self, v_as_int(key));
	if (unlikely(value == null)) {
		ai_err_raisef(env, ALO_EINVAL, "index out of bound.");
	}
	return *value;
}

static VTable const tuple_vtable = {
	._tid = T_TUPLE,
	._api_tag = ALO_TTUPLE,
	._repr_id = REPR_TUPLE,
	._flags = VTABLE_FLAG_NONE,
	._name = "tuple",
	._mark = fpcast(a_fp_mark, tuple_mark),
	._drop = fpcast(a_fp_drop, tuple_drop),
	._get = fpcast(a_fp_get, tuple_get),
	._hash = fpcast(a_fp_hash, ai_tuple_hash),
	._equals = fpcast(a_fp_equals, tuple_equals)
};