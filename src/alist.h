/**
 *@file alist.h
 */

#ifndef alist_h_
#define alist_h_

#include "aobj.h"

ALO_PRIVATE GList* ai_list_new(a_henv env);
ALO_PRIVATE void ai_list_hint(a_henv env, GList* self, a_ulen add);
ALO_PRIVATE void ai_list_grow(a_henv env, GList* self, a_ulen add);
ALO_PRIVATE void ai_list_push(a_henv env, GList* self, Value v);
ALO_PRIVATE void ai_list_push_all(a_henv env, GList* self, Value const* src, a_ulen len);
ALO_PRIVATE Value ai_list_get(a_henv env, GList* self, Value vk);
ALO_PRIVATE Value ai_list_geti(a_henv env, GList* self, a_int k);
ALO_PRIVATE void ai_list_set(a_henv env, GList* self, Value vk, Value vv);
ALO_PRIVATE void ai_list_seti(a_henv env, GList* self, a_int key, Value value);

ALO_PRIVATE a_msg ai_list_ugeti(a_henv env, GList* self, a_int k, Value* pv);
ALO_PRIVATE a_msg ai_list_uget(a_henv env, GList* self, Value vk, Value* pv);
ALO_PRIVATE a_msg ai_list_useti(a_henv env, GList* self, a_int k, Value v);
ALO_PRIVATE a_msg ai_list_uset(a_henv env, GList* self, Value vk, Value v);

struct GList {
    GOBJ_STRUCT_HEADER;
    a_ulen len;
    a_ulen cap;
    Value* ptr;
};

#define g_is_list(o) g_is(o, ALO_TLIST)

#define v_is_list(v) v_is(v, T_LIST)

ALO_INLINE GList* v_as_list(Value v) {
    assume(v_is_list(v), "not list.");
    return g_as(GList, v_as_ref(v));
}

ALO_INLINE Value v_of_list(GList* o) {
    assume(g_is_list(o), "invalid instance.");
    return v_of_ref(o, T_LIST);
}

ALO_INLINE void v_set_list(a_henv env, Value* d, GList* o) {
    Value v = v_of_list(o);
    v_set(env, d, v);
}

#define list_size() sizeof(GList)

#endif /* alist_h_ */
