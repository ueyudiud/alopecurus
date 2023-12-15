/**
 *@file atable.h
 */

#ifndef atable_h_
#define atable_h_

#include "aobj.h"

intern GTable* ai_table_new(a_henv env);
intern void ai_table_grow(a_henv env, GTable* self, a_ulen add);
intern void ai_table_hint(a_henv env, GTable* self, a_ulen add);
intern a_bool ai_table_get(a_henv env, GTable* self, Value vk, Value* pv);
intern a_bool ai_table_geti(a_henv env, GTable* self, a_int k, Value* pv);
intern a_bool ai_table_getls(a_henv env, GTable* self, char const* ptr, a_usize len, Value* pv);
intern a_bool ai_table_gets(a_henv env, GTable* self, GStr* k, Value* pv);
intern a_bool ai_table_set(a_henv env, GTable* self, Value vk, Value vv);
intern Value* ai_table_refls(a_henv env, GTable* self, char const* ptr, a_usize len);
intern a_bool ai_table_del(a_henv env, GTable* self, Value vk);
intern void ai_table_delr(a_henv env, GTable* self, Value* ref);
intern a_bool ai_table_next(a_henv env, GTable* self, Value* rk, a_int* pindex);
intern a_msg ai_table_uset(a_henv env, GTable* self, Value vk, Value vv);

typedef struct TNode TNode;

/**
 ** Linked hash table.
 */
struct GTable {
    GOBJ_STRUCT_HEADER;
    a_u32 _len;
    a_u32 _hmask;
    TNode* _ptr;
};

/**
 ** Table node.
 */
struct TNode {
    a_hash _hash;
    a_i32 _hnext;
    a_i32 _lprev;
    a_i32 _lnext;
    Value _key;
    Value _value;
};

#define v_is_table(v) v_is(v, T_TABLE)

always_inline GTable* v_as_table(Value v) {
    assume(v_is_table(v), "not table.");
    return g_cast(GTable, v_as_obj(v));
}

#define table_size() sizeof(GTable)

#endif /* atable_h_ */
