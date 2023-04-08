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
    GList* self = ai_mem_alloc(env, sizeof(GList));
	self->_vptr = &list_vtable;
    self->_ptr = null;
    self->_len = 0;
    self->_cap = 0;
    ai_gc_register_object(env, self);
    return self;
}

void ai_list_hint(a_henv env, GList* self, a_usize len) {
    a_usize expect = self->_len + len;
    a_usize old_cap = self->_cap;
    if (expect > old_cap) {
        a_usize new_cap = expect;
        self->_ptr = ai_mem_vgrow(env, self->_ptr, old_cap, new_cap);
        self->_cap = new_cap;
    }
}

void ai_list_push(a_henv env, GList* self, Value value) {
    if (self->_len == self->_cap) {
        a_usize old_cap = self->_cap;
        a_usize new_cap = old_cap * 2;
        self->_ptr = ai_mem_vgrow(env, self->_ptr, old_cap, new_cap);
    }

    v_set(env, &self->_ptr[self->_len++], value);

	ai_gc_barrier_backward_val(env, self, value);
}

void ai_list_push_all(a_henv env, GList* self, Value const* src, a_usize len) {
	ai_list_hint(env, self, len);

	v_cpy_all(env, self->_ptr + self->_len, src, len);
	self->_len += 1;
	/* We assume the elements in source has white value. */
	if (g_has_black_color(self)) {
		join_trace(&G(env)->_tr_regray, self);
	}
}

Value* ai_list_refi(unused a_henv env, GList* self, a_isize pos) {
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
    return &self->_ptr[pos];
}

static void list_mark(Global* g, GList* self) {
    a_usize len = self->_len;
    for (a_usize i = 0; i < len; ++i) {
		ai_gc_trace_mark_val(g, self->_ptr[i]);
    }
	ai_gc_trace_work(g, sizeof(Value) * self->_cap);
}

static void list_drop(Global* g, GList* self) {
    ai_mem_vdel(g, self->_ptr, self->_cap);
    ai_mem_dealloc(g, self, sizeof(GList));
}

Value ai_list_get(a_henv env, GList* self, Value index) {
	if (unlikely(!v_is_int(index))) {
		ai_err_bad_get(env, "list", v_nameof(env, index));
	}
	return ai_list_geti(env, self, v_as_int(index));
}

Value ai_list_geti(a_henv env, GList* self, a_int index) {
	Value* ref = ai_list_refi(env, self, index);
	return ref ? *ref : v_of_nil();
}

void ai_list_set(a_henv env, GList* self, Value index, Value value) {
	if (unlikely(!v_is_int(index))) {
		ai_err_raisef(env, ALO_EINVAL, "bad index for list.");
	}
	ai_list_seti(env, self, v_as_int(index), value);
}

void ai_list_seti(a_henv env, GList* self, a_int index, Value value) {
	Value* ref = ai_list_refi(env, self, index);
	if (unlikely(ref == null)) {
		ai_err_raisef(env, ALO_EINVAL, "list index out of bound.");
	}
	v_set(env, ref, value);
	ai_gc_barrier_forward_val(env, self, value);
}

static VTable const list_vtable = {
	._mask = V_MASKED_TAG(T_LIST),
	._iname = env_type_iname(_list),
	._base_size = sizeof(GList),
	._elem_size = 0,
	._flags = VTABLE_FLAG_NONE,
	._body = {
		vfp_def(mark, list_mark),
		vfp_def(drop, list_drop)
	}
};