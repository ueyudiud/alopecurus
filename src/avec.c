/**
 *@file avec_c_
 */

#define avec_c_
#define ALO_LIB

#include "amem.h"
#include "agc.h"

#include "avec.h"

void ai_vec_init(unused a_henv env, Vec* self) {
    self->_ptr = null;
    self->_cap = 0;
    self->_len = 0;
}

void ai_vec_mark(Global* g, Vec* self) {
    ai_gc_trace_work(g, self->_cap * sizeof(Value));
    for (a_u32 i = 0; i < self->_len; ++i) {
        ai_gc_trace_mark_val(g, self->_ptr[i]);
    }
}

void ai_vec_deinit(Global* g, Vec* self) {
    if (self->_ptr != null) {
        ai_mem_vdel(g, self->_ptr, self->_cap);
    }
}

a_bool ai_vec_hint(a_henv env, Vec* self, a_usize len) {
    a_usize need;

    try(checked_add_usize(self->_cap, len, &need));

    if (need > self->_cap) {
        a_usize old_cap = self->_cap;
        a_usize new_cap = max(old_cap * 2, need + 4);

        self->_ptr = ai_mem_vgrow(env, self->_ptr, old_cap, new_cap);
        self->_cap = new_cap;
    }

    return false;
}

static a_bool vec_hint1(a_henv env, Vec* self) {
    if (unlikely(self->_len == self->_cap)) {
        a_usize old_cap = self->_cap;
        a_usize new_cap;

        if (old_cap != 0) {
            try(checked_mul_usize(old_cap, 2, &new_cap));
        }
        else {
            new_cap = 4;
        }

        self->_ptr = ai_mem_vgrow(env, self->_ptr, old_cap, new_cap);
        self->_cap = new_cap;
    }

    return false;
}

a_bool ai_vec_push(a_henv env, Vec* self, Value val) {
    try(vec_hint1(env, self));

    v_set(env, &self->_ptr[self->_len], val);
    self->_len += 1;

    return false;
}

a_bool ai_vec_push_all(a_henv env, Vec* self, Value const* src, a_usize len) {
    try(ai_vec_hint(env, self, len));

    v_cpy_all(env, self->_ptr + self->_len, src, len);
    self->_len += len;

    return false;
}