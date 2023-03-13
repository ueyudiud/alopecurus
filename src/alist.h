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

struct GList {
	GOBJ_STRUCT_HEADER;
	a_u32 _len;
	a_u32 _cap;
	Value* _ptr;
};

static_assert(offsetof(GObj, _len) == offsetof(GList, _len));

always_inline a_bool g_is_list(a_hobj v) {
	return v->_vtable->_repr_id == REPR_LIST;
}

always_inline GList* g_as_list(a_hobj v) {
	assume(g_is_list(v));
	return g_cast(GList, v);
}

always_inline GList* v_as_list(Value v) {
	assume(v_is_list(v), "not list.");
	return g_as_list(v_as_obj(v));
}

#endif /* alist_h_ */
