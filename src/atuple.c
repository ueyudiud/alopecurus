/**
 *@file atuple.c=
 */

#define atuple_c_

#include "amem.h"
#include "agc.h"

#include "atuple.h"

GTuple* ai_tuple_new(a_henv env, Value const* src, a_usize len) {
    GTuple* self = ai_mem_alloc(env, sizeof(GTuple) + sizeof(Value) * len);
    self->_len = len;
    for (a_usize i = 0; i < len; ++i) {
        v_cpy(G(env), &self->_body[i], &src[i]);
    }
    self->_meta = &G(env)->_metas._tuple;
    ai_gc_register_object(env, self);
    return self;
}

Value const* ai_tuple_geti(unused a_henv env, GTuple* self, a_isize pos) {
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
    return &self->_body[pos];
}

void ai_tuple_splash(Global* g, GTuple* self) {
    a_usize len = self->_len;
    for (a_usize i = 0; i < len; ++i) {
        ai_gc_trace_markv(g, &self->_body[i]);
    }
    g->_mem_work -= sizeof(GTuple) + sizeof(Value) * len;
}

void ai_tuple_destruct(Global* g, GTuple* self) {
    ai_mem_dealloc(g, self, sizeof(GTuple) + sizeof(Value) * self->_len);
}
