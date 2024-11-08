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
intern a_bool ai_table_getls(a_henv env, GTable* self, a_lstr k, Value* pv);
intern a_bool ai_table_gets(a_henv env, GTable* self, GStr* k, Value* pv);
intern a_bool ai_table_set(a_henv env, GTable* self, Value vk, Value vv);
intern Value* ai_table_refls(a_henv env, GTable* self, a_lstr k);
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
    a_u32 len;
    a_u32 hmask;
    TNode* ptr;
};

/**
 ** Table node.
 */
struct TNode {
    a_hash hash;
    a_i32 hnext;
    a_i32 lprev;
    a_i32 lnext;
    Value key;
    Value value;
};

#define g_is_table(o) g_is(o, ALO_TTABLE)

#define v_is_table(v) v_is(v, T_TABLE)

always_inline GTable* v_as_table(Value v) {
    assume(v_is_table(v), "not table.");
    return g_as(GTable, v_as_obj(v));
}

always_inline Value v_of_table(GTable* o) {
    assume(g_is_table(o), "invalid instance.");
    return v_of_obj_(o, T_TABLE);
}

always_inline void v_set_table(a_henv env, Value* d, GTable* o) {
    Value v = v_of_table(o);
    v_set(env, d, v);
}

#define table_size() sizeof(GTable)

#endif /* atable_h_ */
