/**
 *@file atuple.h
 */

#ifndef atuple_h_
#define atuple_h_

#include "aobj.h"

intern GTuple* ai_tuple_new(a_henv env, Value const* src, a_usize len);
intern Value const* ai_tuple_refi(a_henv env, GTuple* self, a_int key);
intern Value ai_tuple_get(a_henv env, GTuple* self, Value key);
intern Value ai_tuple_geti(a_henv env, GTuple* self, a_int key);
intern a_hash ai_tuple_hash(a_henv env, GTuple* self);
intern a_bool ai_tuple_equals(a_henv env, GTuple* self, GTuple* other);

inline a_msg ai_tuple_ugeti(a_henv env, GTuple* self, a_int key, Value* pval) {
    a_uint i = obj_idx(key, self->_len, ALO_EINVAL);
    v_set(env, pval, self->_ptr[i]);
    return ALO_SOK;
}

inline a_msg ai_tuple_uget(a_henv env, GTuple* self, Value key, Value* pval) {
    if (!v_is_int(key)) return ALO_EINVAL;
    return ai_tuple_ugeti(env, self, v_as_int(key), pval);
}

#endif /* atuple_h_ */
