/**
 *@file avec_h_
 */

#ifndef avec_h_
#define avec_h_

#include "aobj.h"

intern void ai_vec_init(a_henv env, Vec* self);
intern void ai_vec_mark(Global* g, Vec* self);
intern void ai_vec_deinit(Global* g, Vec* self);
intern a_bool ai_vec_hint(a_henv env, Vec* self, a_usize len);
intern a_bool ai_vec_push(a_henv env, Vec* self, Value val);
intern a_bool ai_vec_push_all(a_henv env, Vec* self, Value const* src, a_usize len);

#endif /* avec_h_ */
