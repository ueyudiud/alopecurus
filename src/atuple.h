/**
 *@file atuple.h
 */

#ifndef atuple_h_
#define atuple_h_

#include "aobj.h"

intern GTuple* ai_tuple_new(a_henv env, Value const* src, a_ulen len);
intern Value ai_tuple_get(a_henv env, GTuple* self, Value vk);
intern Value ai_tuple_geti(a_henv env, GTuple* self, a_int k);
intern a_hash ai_tuple_hash(a_henv env, GTuple* self);
intern a_bool ai_tuple_equals(a_henv env, GTuple* self, GTuple* o);
intern a_msg ai_tuple_ugeti(a_henv env, GTuple* self, a_int k, Value* pv);
intern a_msg ai_tuple_uget(a_henv env, GTuple* self, Value vk, Value* pv);

struct GTuple {
    GOBJ_STRUCT_HEADER;
    a_u32 _len;
    a_hash _hash;
    Value _ptr[0];
};

#define v_is_tuple(v) v_is(v, T_TUPLE)

always_inline GTuple* v_as_tuple(Value v) {
    assume(v_is_tuple(v), "not tuple.");
    return g_cast(GTuple, v_as_obj(v));
}

#define tuple_size(l) (sizeof(GTuple) + sizeof(Value) * (l))

#endif /* atuple_h_ */
