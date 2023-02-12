/**
 *@file atuple.h
 */

#ifndef atuple_h_
#define atuple_h_

#include "aobj.h"

struct GTuple {
	GOBJ_STRUCT_HEADER;
	a_u32 _len;
	Value _body[0];
};

intern GTuple* ai_tuple_new(a_henv env, Value const* src, a_usize len);
intern Value const* ai_tuple_refi(a_henv env, GTuple* self, a_isize pos);

intern VTable const ai_tuple_vtable;

#endif /* atuple_h_ */


