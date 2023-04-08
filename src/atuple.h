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

#endif /* atuple_h_ */


