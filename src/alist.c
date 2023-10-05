/**
 *@file alist.c
 */

#define alist_c_
#define ALO_LIB

#include "aop.h"
#include "avec.h"
#include "amem.h"
#include "agc.h"
#include "aerr.h"

#include "alist.h"

static VTable const list_vtable;

GList* ai_list_new(a_henv env) {
    GList* self = ai_mem_alloc(env, sizeof(GList));

    self->_vptr = &list_vtable;

    ai_vec_init(env, &self->_vec);

    ai_gc_register_object(env, self);

    return self;
}

void ai_list_hint(a_henv env, GList* self, a_usize len) {
    ai_vec_hint(env, &self->_vec, len);
}

void ai_list_push(a_henv env, GList* self, Value v) {
    ai_vec_push(env, &self->_vec, v);

	ai_gc_barrier_backward_val(env, self, v);
}

void ai_list_push_all(a_henv env, GList* self, Value const* src, a_usize len) {
    ai_vec_push_all(env, &self->_vec, src, len);

	/* We assume the elements in source has white value. */
	if (g_has_black_color(self)) {
		join_trace(&G(env)->_tr_regray, self);
	}
}

static Value* list_refi(GList *self, a_int key) {
    a_uint i = obj_idx(key, self->_len, null);
    return &self->_ptr[i];
}

Value ai_list_get(a_henv env, GList* self, Value vk) {
	if (unlikely(!v_is_int(vk))) {
        ai_err_bad_key(env, "list", v_nameof(env, vk));
	}
	return ai_list_geti(env, self, v_as_int(vk));
}

Value ai_list_geti(unused a_henv env, GList* self, a_int k) {
	Value* ref = list_refi(self, k);
	return ref ? *ref : v_of_nil();
}

void ai_list_set(a_henv env, GList* self, Value vk, Value vv) {
	if (unlikely(!v_is_int(vk))) {
        ai_err_bad_key(env, "list", v_nameof(env, vv));
	}
	ai_list_seti(env, self, v_as_int(vk), vv);
}

void ai_list_seti(a_henv env, GList* self, a_int key, Value value) {
	Value* ref = list_refi(self, key);
	if (unlikely(ref == null)) {
		ai_err_raisef(env, ALO_EINVAL, "list index out of bound.");
	}
	v_set(env, ref, value);
	ai_gc_barrier_forward_val(env, self, value);
}

a_msg ai_list_ugeti(a_henv env, GList* self, a_int k, Value* pv) {
    a_uint i = obj_idx(k, self->_len, ALO_EEMPTY);
    v_set(env, pv, self->_ptr[i]);
    return ALO_SOK;
}

a_msg ai_list_uget(a_henv env, GList* self, Value vk, Value* pv) {
    if (!v_is_int(vk)) return ALO_EINVAL;
    return ai_list_ugeti(env, self, v_as_int(vk), pv);
}

a_msg ai_list_useti(a_henv env, GList* self, a_int k, Value v) {
    a_uint i = obj_idx(k, self->_len, ALO_EEMPTY);

    v_set(env, &self->_ptr[i], v);

    ai_gc_barrier_backward_val(env, self, v);
    return ALO_SOK;
}

a_msg ai_list_uset(a_henv env, GList* self, Value vk, Value v) {
    if (!v_is_int(vk)) return ALO_EINVAL;
    return ai_list_useti(env, self, v_as_int(vk), v);
}

static void list_mark(Global* g, GList* self) {
    ai_vec_mark(g, &self->_vec);
    ai_gc_trace_work(g, sizeof(GList));
}

static void list_drop(Global* g, GList* self) {
    ai_vec_deinit(g, &self->_vec);
    ai_mem_dealloc(g, self, sizeof(GList));
}

static VTable const list_vtable = {
	._stencil = V_STENCIL(T_LIST),
    ._type_ref = g_type_ref(_list),
	._slots = {
        [vfp_slot(drop)] = list_drop,
        [vfp_slot(mark)] = list_mark
	}
};