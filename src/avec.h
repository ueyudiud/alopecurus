/**
 *@file avec_h_
 */

#ifndef avec_h_
#define avec_h_

#include "aobj.h"

intern void ai_vec_mark(Global* g, Vec* self);
intern void ai_vec_deinit(Global* g, Vec* self);
intern void ai_vec_grow(a_henv env, Vec* self, a_usize need);
intern a_bool ai_vec_push(a_henv env, Vec* self, Value val);
intern a_bool ai_vec_push_all(a_henv env, Vec* self, Value const* src, a_usize len);

always_inline a_bool ai_vec_hint(a_henv env, Vec* self, a_usize addition) {
    if (addition > self->_cap - self->_len) {
        a_usize need;
        try(checked_add_usize(self->_cap, addition, &need));
        ai_vec_grow(env, self, need);
    }

    return false;
}

always_inline void ai_vec_init(Vec* self) {
    init(self) {
        ._ptr = null,
        ._cap = 0,
        ._len = 0
    };
}

#endif /* avec_h_ */
