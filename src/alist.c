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
	self->_vtable = &list_vtable;
    self->_data = null;
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
        self->_data = ai_mem_vgrow(env, self->_data, old_cap, new_cap);
        self->_cap = new_cap;
    }
}

void ai_list_insert(a_henv env, GList* self, Value value) {
    if (self->_len == self->_cap) {
        a_usize old_cap = self->_cap;
        a_usize new_cap = old_cap * 2;
        self->_data = ai_mem_vgrow(env, self->_data, old_cap, new_cap);
    }
    v_set(G(env), &self->_data[self->_len++], value);
	ai_gc_barrier_val(env, self, value);
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
    return &self->_data[pos];
}

static void list_splash(Global* g, GList* self) {
    a_usize len = self->_len;
    for (a_usize i = 0; i < len; ++i) {
		ai_gc_trace_mark_val(g, self->_data[i]);
    }
	ai_gc_trace_work(g, sizeof(GList) + sizeof(Value) * self->_cap);
}

static void list_destruct(Global* g, GList* self) {
    ai_mem_vdel(g, self->_data, self->_cap);
    ai_mem_dealloc(g, self, sizeof(GList));
}

static Value list_get(a_henv env, GList* self, Value index) {
	if (unlikely(!v_is_int(index))) {
		ai_err_raisef(env, ALO_EINVAL, "bad index for list.");
	}
	Value* ref = ai_list_refi(env, self, v_as_int(index));
	return ref ? *ref : v_of_nil();
}

static void list_set(a_henv env, GList* self, Value index, Value value) {
	if (unlikely(!v_is_int(index))) {
		ai_err_raisef(env, ALO_EINVAL, "bad index for list.");
	}
	Value* ref = ai_list_refi(env, self, v_as_int(index));
	if (unlikely(ref == null)) {
		ai_err_raisef(env, ALO_EINVAL, "list index out of bound.");
	}
	v_set(G(env), ref, value);
	ai_gc_barrier_val(env, self, value);
}

static VTable const list_vtable = {
	._tid = T_LIST,
	._api_tag = ALO_TLIST,
	._repr_id = REPR_LIST,
	._flags = VTABLE_FLAG_IDENTITY_EQUAL,
	._name = "list",
	._splash = fpcast(a_fp_splash, list_splash),
	._delete = fpcast(a_fp_delete, list_destruct),
	._get = fpcast(a_fp_get, list_get),
	._set = fpcast(a_fp_set, list_set)
};