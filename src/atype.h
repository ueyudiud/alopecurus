/**
 *@file atype.h
 */

#ifndef atype_h_
#define atype_h_

#include "aobj.h"

typedef struct MNode MNode;

intern GType* ai_type_new(a_henv env, GStr* name, a_u32 extra_size, a_u32 block_size, a_u32 num_slot);
intern void ai_type_boost(a_henv env);
intern a_bool ai_type_get(a_henv env, GType* self, Value vk, Value* pv);
intern a_bool ai_type_gets(a_henv env, GType* self, GStr* k, Value* pv);
intern a_bool ai_type_getls(a_henv env, GType* self, a_lstr k, Value* pv);
intern a_bool ai_type_refs_or_empty(a_henv env, GType* self, GStr* k, MNode** pe);
intern a_bool ai_type_set(a_henv env, GType* self, Value vk, Value vv);
intern void ai_type_sets(a_henv env, GType* self, GStr* key, Value val);
intern Value* ai_type_refls(a_henv env, GType* self, a_lstr k);
intern void ai_type_clean(Global* gbl);

struct MNode {
    Value value;
    GStr* key;
};

#define GTYPE_STRUCT_HEADER \
    GOBJ_STRUCT_HEADER;     \
    /* Size of instance */  \
    a_u32 size;             \
    /* Type signature */    \
    a_u32 sig;              \
    /* Type name. */        \
    GStr* name;             \
    /* Named values */      \
    MNode* ptr;             \
    a_u32 len;              \
    a_u32 hmask;            \
    /* Changed counter */   \
    a_u32 nchg;             \
    /* Fast TM flags */     \
    a_u32 ftmz

typedef struct {
    GTYPE_STRUCT_HEADER;
    Impl body;
    a_u32 block_size;
    a_u32 num_slot;
    a_usize user[0];
} GUType;

/**
 ** Type.
 */
union GType {
    struct {
        GTYPE_STRUCT_HEADER;
    };
    GUType as_utype[1];
};

#define FTM_BIT(tm) (u16c(1) << (tm))

#define mt_has_ftm(t,tm) (((t)->ftmz & FTM_BIT(tm)) == 0)

#define g_is_type(o) g_is(o, ALO_TTYPE)

always_inline a_bool v_is_type(Value v) {
    return v_is(v, T_TYPE);
}

always_inline GType* v_as_type(Value v) {
    assume(v_is_type(v), "not type.");
    return g_as(GType, v_as_obj(v));
}

always_inline Value v_of_type(GType* o) {
    assume(g_is_type(o), "invalid instance.");
    return v_of_obj_(o, T_TYPE);
}

always_inline void v_set_type(a_henv env, Value* d, GType* o) {
    Value v = v_of_type(o);
    v_set(env, d, v);
}

#define type_size(e) align_to(sizeof(GUType) + (e), sizeof(a_usize))

#endif /* atype_h_ */
