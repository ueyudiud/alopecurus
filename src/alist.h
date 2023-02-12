/**
 *@file alist.h
 */

#ifndef alist_h_
#define alist_h_

#include "aobj.h"

struct GList {
	GOBJ_STRUCT_HEADER;
	a_u32 _len;
	a_u32 _cap;
	Value* _data;
};

intern GList* ai_list_new(a_henv env);
intern void ai_list_hint(a_henv env, GList* self, a_usize len);
intern void ai_list_insert(a_henv env, GList* self, Value value);
intern Value const* ai_list_refi(a_henv env, GList* self, a_isize pos);

intern VTable const ai_list_vtable;

#endif /* alist_h_ */
