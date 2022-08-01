/**
 *@file alist.c
 */

#define alist_c_

#include "amem.h"
#include "agc.h"

#include "alist.h"

GList* ai_list_new(a_henv env) {
    GList* self = ai_mem_alloc(env, sizeof(GList));
    self->_data = null;
    self->_len = 0;
    self->_cap = 0;
    self->_meta = &G(env)->_metas._list;
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

void ai_list_insert(a_henv env, GList* self, Value val) {
    if (self->_len == self->_cap) {
        a_usize old_cap = self->_cap;
        a_usize new_cap = old_cap * 2;
        self->_data = ai_mem_vgrow(env, self->_data, old_cap, new_cap);
    }
    v_set(G(env), &self->_data[self->_len++], val);
}

Value const* ai_list_geti(unused a_henv env, GList* self, a_isize pos) {
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

void ai_list_splash(Global* g, GList* self) {
    a_usize len = self->_len;
    for (a_usize i = 0; i < len; ++i) {
        ai_gc_trace_markv(g, &self->_data[i]);
    }
    g->_mem_work -= sizeof(GList) + sizeof(Value) * self->_cap;
}

void ai_list_destruct(Global* g, GList* self) {
    ai_mem_vdel(g, self->_data, self->_cap);
    ai_mem_dealloc(g, self, sizeof(GList));
}
