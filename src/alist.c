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

#define Vec GList

#define at_vec_grow_amortized list_grow_amortized

static a_bool at_vec_grow_amortized(a_henv env, Vec* self, a_usize addition);

#include "avec.h"

GList* ai_list_new(a_henv env) {
    GList* self = ai_mem_alloc(env, sizeof(GList));

    self->_vptr = &list_vtable;
    at_vec_init(self);

    ai_gc_register_object(env, self);

    return self;
}

static void list_hint_failed(a_henv env) {
    ai_err_raisef(env, ALO_EINVAL, "too many elements.");
}

static a_bool list_grow_amortized(a_henv env, GList* self, a_usize addition) {
    a_usize need;
    try(checked_add_usize(self->_len, addition, &need));

    assume(need >= self->_cap, "not need to grow.");

    a_usize old_cap = self->_cap;
    a_usize new_cap = max(old_cap * 2, need + 4);

    if (unlikely(need > INT32_MAX)) {
        return true;
    }
    else if (unlikely(new_cap > INT32_MAX)) {
        new_cap = INT32_MAX; /* Trim to maximum capacity. */
    }

    at_vec_resize(env, self, old_cap, new_cap);
    return false;
}

void ai_list_hint(a_henv env, GList* self, a_usize len) {
    catch (at_vec_hint(env, self, len)) {
        list_hint_failed(env);
    }
}

void ai_list_push(a_henv env, GList* self, Value v) {
    catch (at_vec_push(env, self, v)) {
        list_hint_failed(env);
    }

	ai_gc_barrier_backward_val(env, self, v);
}

void ai_list_push_all(a_henv env, GList* self, Value const* src, a_usize len) {
    catch (at_vec_push_all(env, self, src, len)) {
        list_hint_failed(env);
    }

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

static void list_mark(Global* g, GList* self) {
    for (a_u32 i = 0; i < self->_len; ++i) {
        ai_gc_trace_mark_val(g, self->_ptr[i]);
    }
    ai_gc_trace_work(g, sizeof(GList) + sizeof(Value) * self->_cap);
}

static void list_drop(Global* g, GList* self) {
    at_vec_deinit(g, self);
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