/**
 *@file alist.h
 */

#ifndef alist_h_
#define alist_h_

#include "aobj.h"

intern GList* ai_list_new(a_henv env);
intern void ai_list_hint(a_henv env, GList* self, a_usize len);
intern void ai_list_push(a_henv env, GList* self, Value value);
intern void ai_list_push_all(a_henv env, GList* self, Value const* src, a_usize len);
intern Value* ai_list_refi(a_henv env, GList* self, a_isize pos);
intern Value ai_list_get(a_henv env, GList* self, Value index);
intern Value ai_list_geti(a_henv env, GList* self, a_int index);
intern void ai_list_set(a_henv env, GList* self, Value index, Value value);
intern void ai_list_seti(a_henv env, GList* self, a_int index, Value value);

#endif /* alist_h_ */
