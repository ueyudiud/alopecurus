/**
 *@file alist.c
 */

#define alist_c_
#define ALO_LIB

#include "amem.h"
#include "agc.h"
#include "aerr.h"

#include "alist.h"

GList* ai_list_new(a_henv env) {
    GList* self = ai_mem_alloc(env, sizeof(GList));
	self->_meta = &G(env)->_metas._list;
    self->_data = null;
    self->_len = 0;
    self->_cap = 0;
    ai_gc_register_object(env, self);
    return self;
}

void ai_list_hint(a_henv env, GList* self, a_usize len) {
    a_usize expect = self->_len + len;
    a_usize old_cap = self->_cap;
    if (expect >= old_cap) {
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
}

Value const* ai_list_refi(unused a_henv env, GList* self, a_isize pos) {
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
        ai_gc_trace_markv(g, &self->_data[i]);
    }
    g->_mem_work -= sizeof(GList) + sizeof(Value) * self->_cap;
}

static void list_destruct(Global* g, GList* self) {
    ai_mem_vdel(g, self->_data, self->_cap);
    ai_mem_dealloc(g, self, sizeof(GList));
}

static Value list_get(a_henv env, GList* self, Value key) {
	if (unlikely(!v_is_int(&key))) {
		ai_err_raisef(env, ALO_EINVAL, "bad index for list.");
	}
	Value const* ref = ai_list_refi(env, self, v_as_int(&key));
	return ref ? *ref : v_of_nil();
}

VTable const ai_list_vtable = {
	._splash = fpcast(a_fp_splash, list_splash),
	._destruct = fpcast(a_fp_destruct, list_destruct),
	._get = fpcast(a_fp_get, list_get)
};