/**
 *@file alist.h
 */

#ifndef alist_h_
#define alist_h_

#include "aobj.h"

intern GList* ai_list_new(a_henv env);
intern void ai_list_hint(a_henv env, GList* self, a_usize add);
intern void ai_list_grow(a_henv env, GList* self, a_usize add);
intern void ai_list_push(a_henv env, GList* self, Value v);
intern void ai_list_push_all(a_henv env, GList* self, Value const* src, a_usize len);
intern Value ai_list_get(a_henv env, GList* self, Value vk);
intern Value ai_list_geti(a_henv env, GList* self, a_int k);
intern void ai_list_set(a_henv env, GList* self, Value vk, Value vv);
intern void ai_list_seti(a_henv env, GList* self, a_int key, Value value);

intern a_msg ai_list_ugeti(a_henv env, GList* self, a_int k, Value* pv);
intern a_msg ai_list_uget(a_henv env, GList* self, Value vk, Value* pv);
intern a_msg ai_list_useti(a_henv env, GList* self, a_int k, Value v);
intern a_msg ai_list_uset(a_henv env, GList* self, Value vk, Value v);

struct GList {
    GOBJ_STRUCT_HEADER;
    a_u32 _len;
    a_u32 _cap;
    Value* _ptr;
};

#define v_is_list(v) v_is(v, T_LIST)

always_inline GList* v_as_list(Value v) {
    assume(v_is_list(v), "not list.");
    return g_cast(GList, v_as_obj(v));
}

#define list_size() sizeof(GList)

#endif /* alist_h_ */
