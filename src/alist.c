/**
 *@file alist.c
 */

#define alist_c_
#define ALO_LIB

#include "aop.h"
#include "amem.h"
#include "agc.h"
#include "aerr.h"

#include "alist.h"

static VTable const list_vtable;

GList* ai_list_new(a_henv env) {
    GList* self = ai_mem_alloc(env, list_size());

    self->_vptr = &list_vtable;
    self->_ptr = null;
    self->_cap = 0;
    self->_len = 0;

    ai_gc_register_object(env, self);

    return self;
}

static a_bool list_grow(a_henv env, GList* self, a_usize add) {
    a_usize need;
    try (checked_add_usize(self->_len, add, &need));

    assume(need >= self->_cap, "not need to grow.");

    a_usize old_cap = self->_cap;
    a_usize new_cap = max(old_cap * 2, need + 4);

    if (unlikely(need > INT32_MAX)) {
        return true;
    }
    else if (unlikely(new_cap > INT32_MAX)) {
        new_cap = INT32_MAX; /* Trim to maximum capacity. */
    }

    self->_ptr = ai_mem_vgrow(env, self->_ptr, old_cap, new_cap);
    self->_cap = new_cap;

    return false;
}

void ai_list_grow(a_henv env, GList* self, a_usize add) {
    catch (list_grow(env, self, add)) {
        ai_err_raisef(env, ALO_EINVAL, "too many elements.");
    }
}

void ai_list_hint(a_henv env, GList* self, a_usize add) {
    if (unlikely(add > self->_cap - self->_len)) {
        ai_list_grow(env, self, add);
    }
}

void ai_list_push(a_henv env, GList* self, Value v) {
    ai_list_hint(env, self, 1);

    v_set(env, &self->_ptr[self->_len], v);
    self->_len += 1;

	ai_gc_barrier_backward_val(env, self, v);
}

void ai_list_push_all(a_henv env, GList* self, Value const* src, a_usize len) {
    ai_list_hint(env, self, len);

    v_cpy_all(env, &self->_ptr[self->_len], src, len);
    self->_len += len;

    /* We assume the elements in source has white value. */
	if (g_has_black_color(self)) {
		join_trace(&G(env)->_tr_regray, self);
	}
}

Value ai_list_get(a_henv env, GList* self, Value vk) {
	if (unlikely(!v_is_int(vk))) {
        ai_err_bad_key(env, "list", v_nameof(env, vk));
	}
	return ai_list_geti(env, self, v_as_int(vk));
}

Value ai_list_geti(unused a_henv env, GList* self, a_int k) {
    a_u32 i = obj_idx(k, self->_len, v_of_nil());
	return self->_ptr[i];
}

void ai_list_set(a_henv env, GList* self, Value vk, Value vv) {
	if (unlikely(!v_is_int(vk))) {
        ai_err_bad_key(env, "list", v_nameof(env, vv));
	}
	ai_list_seti(env, self, v_as_int(vk), vv);
}

void ai_list_seti(a_henv env, GList* self, a_int key, Value value) {
    a_u32 i = obj_idx(key, self->_len,
        ai_err_raisef(env, ALO_EINVAL, "list index out of bound.")
    );
	v_set(env, &self->_ptr[i], value);
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

static void list_mark(Global* gbl, GList* self) {
    for (a_u32 i = 0; i < self->_len; ++i) {
        ai_gc_trace_mark_val(gbl, self->_ptr[i]);
    }
    ai_gc_trace_work(gbl, list_size() + sizeof(Value) * self->_cap);
}

static void list_drop(Global* gbl, GList* self) {
    if (self->_ptr != null) {
        ai_mem_vdel(gbl, self->_ptr, self->_cap);
    }
    ai_mem_dealloc(gbl, self, list_size());
}

static VTable const list_vtable = {
	._stencil = V_STENCIL(T_LIST),
    ._tag = ALO_TLIST,
    ._type_ref = g_type_ref(ALO_TLIST),
	._slots = {
        [vfp_slot(drop)] = list_drop,
        [vfp_slot(mark)] = list_mark
	}
};