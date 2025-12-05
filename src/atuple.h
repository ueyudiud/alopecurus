/**
 *@file atuple.h
 */

#ifndef atuple_h_
#define atuple_h_

#include "aobj.h"

ALO_PRIVATE GTuple* ai_tuple_new(a_henv env, Value const* src, a_ulen len);
ALO_PRIVATE Value ai_tuple_get(a_henv env, GTuple* self, Value vk);
ALO_PRIVATE Value ai_tuple_geti(a_henv env, GTuple* self, a_int k);
ALO_PRIVATE a_hash ai_tuple_hash(a_henv env, GTuple* self);
ALO_PRIVATE a_bool ai_tuple_equals(a_henv env, GTuple* self, GTuple* o);
ALO_PRIVATE a_msg ai_tuple_ugeti(a_henv env, GTuple* self, a_int k, Value* pv);
ALO_PRIVATE a_msg ai_tuple_uget(a_henv env, GTuple* self, Value vk, Value* pv);

struct GTuple {
    GOBJ_STRUCT_HEADER;
    a_ulen len;
    a_hash hash;
    Value ptr[0];
};

#define g_is_tuple(o) g_is(o, ALO_TTUPLE)

#define v_is_tuple(v) v_is(v, T_TUPLE)

ALO_INLINE GTuple* v_as_tuple(Value v) {
    assume(v_is_tuple(v), "not tuple.");
    return g_as(GTuple, v_as_ref(v));
}

ALO_INLINE Value v_of_tuple(GTuple* o) {
    assume(g_is_tuple(o), "invalid instance.");
    return v_of_ref(o, T_TUPLE);
}

ALO_INLINE void v_set_tuple(a_henv env, Value* d, GTuple* o) {
    Value v = v_of_tuple(o);
    v_set(env, d, v);
}

#define tuple_size(l) (sizeof(GTuple) + sizeof(Value) * (l))

#endif /* atuple_h_ */
