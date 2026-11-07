/**
 *@file atable.h
 */

#ifndef atable_h_
#define atable_h_

#include "aobj.h"

ALO_PRIVATE GTable* ai_table_new(a_henv env, a_enum kv);
ALO_PRIVATE void ai_table_grow(a_henv env, GTable* self, a_ulen add);
ALO_PRIVATE void ai_table_hint(a_henv env, GTable* self, a_ulen add);
ALO_PRIVATE a_bool ai_table_get(a_henv env, GTable* self, Value vk, Value* pv);
ALO_PRIVATE a_bool ai_table_geti(a_henv env, GTable* self, a_int k, Value* pv);
ALO_PRIVATE a_bool ai_table_getls(a_henv env, GTable* self, char const* kptr, a_usize klen, Value* pv);
ALO_PRIVATE a_bool ai_table_gets(a_henv env, GTable* self, GStr* k, Value* pv);
ALO_PRIVATE a_bool ai_table_set(a_henv env, GTable* self, Value vk, Value vv);
ALO_PRIVATE Value* ai_table_refls(a_henv env, GTable* self, char const* kptr, a_usize klen);
ALO_PRIVATE a_bool ai_table_del(a_henv env, GTable* self, Value vk);
ALO_PRIVATE void ai_table_delr(a_henv env, GTable* self, Value* ref);
ALO_PRIVATE a_bool ai_table_next(a_henv env, GTable* self, Value* rk, a_int* pindex, Value* pv);
ALO_PRIVATE a_msg ai_table_uset(a_henv env, GTable* self, Value vk, Value vv);

typedef struct TLink TLink;

/**
 ** SwissTable with an insertion-ordered linked list.
 */
struct GTable {
    GOBJ_STRUCT_HEADER;
    a_ulen len;
    a_ulen hmask;
    /**
     * Pointer to bucket.
     *
     * The bucket block of a table is laid out as:
     *
     *   [TNode cap-1] ... [TNode 0][TLink ctrl][TCtrl 0] ... [TCtrl cap-1]
     *                     ^
     *                    ptr
     *
     * 'hmask' is the mask of the capacity, which is a power of two and is
     * aligned to TCTRL_GROUP_WIDTH, so that the control block can be probed
     * group by group.
     */
    TLink* ptr;
    /* Empty slots count. */
    a_ulen nempty;
};

#define g_is_table(o) g_is(o, ALO_TTABLE)

#define v_is_table(v) v_is(v, T_TABLE)

ALO_INLINE GTable* v_as_table(Value v) {
    assume(v_is_table(v), "not table.");
    return g_as(GTable, v_as_ref(v));
}

ALO_INLINE Value v_of_table(GTable* o) {
    assume(g_is_table(o), "invalid instance.");
    return v_of_ref(o, T_TABLE);
}

ALO_INLINE void v_set_table(a_henv env, Value* d, GTable* o) {
    Value v = v_of_table(o);
    v_set(env, d, v);
}

#define table_size() sizeof(GTable)

#endif /* atable_h_ */
