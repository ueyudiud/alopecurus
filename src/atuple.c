/**
 *@file atuple.c=
 */

#define atuple_c_
#define ALO_LIB

#include "amem.h"
#include "agc.h"
#include "aerr.h"

#include "atuple.h"

GTuple* ai_tuple_new(a_henv env, Value const* src, a_usize len) {
    GTuple* self = ai_mem_alloc(env, sizeof(GTuple) + sizeof(Value) * len);
	self->_meta = &G(env)->_metas._tuple;
    self->_len = len;
	v_cpy_multi(G(env), self->_body, src, len);
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

static void tuple_splash(Global* g, GTuple* self) {
    a_usize len = self->_len;
    for (a_usize i = 0; i < len; ++i) {
		ai_gc_trace_mark_val(g, self->_body[i]);
    }
	ai_gc_trace_work(g, sizeof(GTuple) + sizeof(Value) * len);
}

static void tuple_destruct(Global* g, GTuple* self) {
    ai_mem_dealloc(g, self, sizeof(GTuple) + sizeof(Value) * self->_len);
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

VTable const ai_tuple_vtable = {
	._splash = fpcast(a_fp_splash, tuple_splash),
	._destruct = fpcast(a_fp_destruct, tuple_destruct),
	._get = fpcast(a_fp_get, tuple_get)
};