/**
 ** Template file.
 **
 *@file avec_h_
 */

#define avec_h_

#include "aobj.h"

#ifndef Vec

# error Vec required.

/* For type hint. */
typedef struct {
    Value* _ptr;
    a_usize _len;
    a_usize _cap;
} Vec;

#endif

#include "amem.h"
#include "agc.h"
#include "aerr.h"

static void at_vec_init(Vec* self) {
    self->_ptr = null;
    self->_cap = 0;
    self->_len = 0;
}

static void at_vec_deinit(Global* g, Vec* self) {
    if (self->_ptr != null) {
        ai_mem_vdel(g, self->_ptr, self->_cap);
    }
}

static void at_vec_resize(a_henv env, Vec* self, a_usize old_cap, a_usize new_cap) {
    self->_ptr = ai_mem_vgrow(env, self->_ptr, old_cap, new_cap);
    self->_cap = new_cap;
}

#ifndef at_vec_grow_amortized

static a_bool at_vec_grow_amortized(a_henv env, Vec* self, a_usize addition) {
    a_usize need;
    try(checked_add_usize(self->_len, addition, &need));

    assume(need >= self->_cap, "not need to grow.");

    a_usize old_cap = self->_cap;
    a_usize new_cap = max(old_cap * 2, need + 4);

    at_vec_resize(env, self, old_cap, new_cap);
    return false;
}

#endif

static a_bool at_vec_hint(a_henv env, Vec* self, a_usize addition) {
    if (addition > self->_cap - self->_len) {
        try(at_vec_grow_amortized(env, self, addition));
    }

    return false;
}

static a_bool at_vec_push(a_henv env, Vec* self, Value val) {
    if (unlikely(self->_len == self->_cap)) {
        try(at_vec_grow_amortized(env, self, 0));
    }

    v_set(env, &self->_ptr[self->_len], val);
    self->_len += 1;

    return false;
}

static a_bool at_vec_push_all(a_henv env, Vec* self, Value const* src, a_usize len) {
    try(at_vec_hint(env, self, len));

    v_cpy_all(env, &self->_ptr[self->_len], src, len);
    self->_len += len;

    return false;
}

#undef Vec