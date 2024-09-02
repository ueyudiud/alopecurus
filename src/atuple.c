/**
 *@file atuple.c
 */

#define atuple_c_
#define ALO_LIB

#include "amem.h"
#include "agc.h"
#include "aerr.h"
#include "avm.h"

#include "atuple.h"

static VTable const tuple_vtable;

#if ALO_M64
# define TUPLE_MAX_LEN cast(a_u32, INT32_MAX)
#else
# define TUPLE_MAX_LEN ((UINT32_MAX - sizeof(GTuple)) / sizeof(Value))
#endif

GTuple* ai_tuple_new(a_henv env, Value const* src, a_ulen len) {
    if (len > TUPLE_MAX_LEN) {
        ai_err_raisef(env, ALO_EINVAL, "tuple size too large.");
    }

    GTuple* self = ai_mem_alloc(env, tuple_size(len));

	self->vptr = &tuple_vtable;
    self->len = len;
	self->hash = 0;
	v_cpy_all(env, self->ptr, src, len);

    ai_gc_register_object(env, self);
    return self;
}

a_bool ai_tuple_equals(a_henv env, GTuple* self, GTuple* o) {
	if (self == o)
        return true;
    if (self->len != o->len)
		return false;
	for (a_u32 i = 0; i < self->len; ++i) {
		if (!ai_vm_equals(env, self->ptr[i], o->ptr[i]))
			return false;
	}
	return true;
}

a_hash ai_tuple_hash(a_henv env, GTuple* self) {
	if (self->hash == 0) {
		a_hash hash = G(env)->seed;
		for (a_u32 i = 0; i < self->len; ++i) {
            hash = hash * 31 + ai_vm_hash(env, self->ptr[i]);
		}
		self->hash = hash != 0 ? hash : 1;
	}
	return self->hash;
}

Value ai_tuple_get(a_henv env, GTuple* self, Value vk) {
	if (unlikely(!v_is_int(vk))) {
        ai_err_bad_key(env, "tuple", v_nameof(env, vk));
	}
	return ai_tuple_geti(env, self, v_as_int(vk));
}

static Value const* tuple_refi(unused a_henv env, GTuple* self, a_int key) {
    a_uint i = obj_idx(key, self->len, null);
    return &self->ptr[i];
}

Value ai_tuple_geti(a_henv env, GTuple* self, a_int k) {
	Value const* value = tuple_refi(env, self, k);
	if (unlikely(value == null)) {
		ai_err_raisef(env, ALO_EINVAL, "index out of bound.");
	}
	return *value;
}

a_msg ai_tuple_ugeti(a_henv env, GTuple* self, a_int k, Value* pv) {
    a_uint i = obj_idx(k, self->len, ALO_EINVAL);
    v_set(env, pv, self->ptr[i]);
    return ALO_SOK;
}

a_msg ai_tuple_uget(a_henv env, GTuple* self, Value vk, Value* pv) {
    if (!v_is_int(vk)) return ALO_EINVAL;
    return ai_tuple_ugeti(env, self, v_as_int(vk), pv);
}

static void tuple_drop(Global* gbl, GTuple* self) {
    ai_mem_dealloc(gbl, self, tuple_size(self->len));
}

static void tuple_mark(Global* gbl, GTuple* self) {
    a_u32 len = self->len;
    for (a_u32 i = 0; i < len; ++i) {
        ai_gc_trace_mark_val(gbl, self->ptr[i]);
    }
    ai_gc_trace_work(gbl, tuple_size(self->len));
}

static VTable const tuple_vtable = {
    .tag = ALO_TTUPLE,
    .flags = VTABLE_FLAG_NONE,
    .type_ref = g_type_ref(ALO_TTUPLE),
    .impl = {
        .drop = cast(void const*, tuple_drop),
        .mark = cast(void const*, tuple_mark)
    }
};
