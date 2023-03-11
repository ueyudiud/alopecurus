/**
 *@file atuple.h
 */

#ifndef atuple_h_
#define atuple_h_

#include "aobj.h"

intern GTuple* ai_tuple_new(a_henv env, Value const* src, a_usize len);
intern Value const* ai_tuple_refi(a_henv env, GTuple* self, a_isize pos);
intern Value ai_tuple_get(a_henv env, GTuple* self, Value key);
intern Value ai_tuple_geti(a_henv env, GTuple* self, a_int key);
intern a_hash ai_tuple_hash(a_henv env, GTuple* self);
intern a_bool ai_tuple_equals(a_henv env, GTuple* self, GTuple* other);

struct GTuple {
	GOBJ_STRUCT_HEADER;
	a_u32 _len;
	a_hash _hash;
	Value _body[0];
};

always_inline a_bool g_is_tuple(a_hobj v) {
	return v->_vtable->_repr_id == REPR_TUPLE;
}

always_inline GTuple* g_as_tuple(a_hobj v) {
	assume(g_is_tuple(v));
	return g_cast(GTuple, v);
}

always_inline GTuple* v_as_tuple(Value v) {
	assume(v_is_tuple(v), "not tuple.");
	return g_as_tuple(v_as_obj(v));
}

always_inline a_usize tuple_size(a_usize len) {
	return sizeof(GTuple) + sizeof(Value) * len;
}

#endif /* atuple_h_ */


