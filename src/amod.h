/**
 *@file amod.h
 */

#ifndef amod_h_
#define amod_h_

#include "aobj.h"

typedef struct MNode MNode;

intern GMod* ai_mod_new(a_henv env, a_usize extra);
intern void ai_mod_mark(Global* gbl, GMod* self);
intern void ai_mod_deinit(Global* gbl, GMod* self);
intern a_bool ai_mod_get(a_henv env, GMod* self, Value vk, Value* pv);
intern a_bool ai_mod_gets(a_henv env, GMod* self, GStr* key, Value* pval);
intern a_bool ai_mod_getls(a_henv env, GMod* self, char const* src, a_usize len, Value* pval);
intern a_bool ai_mod_refs_or_empty(a_henv env, GMod* self, GStr* key, MNode** pnode);
intern a_bool ai_mod_set(a_henv env, GMod* self, Value vk, Value vv);
intern void ai_mod_sets(a_henv env, GMod* self, GStr* key, Value val);
intern Value* ai_mod_refls(a_henv env, GMod* self, char const* src, a_usize len);

struct ModHead { };

#define GMOD_STRUCT_HEADER \
    struct ModHead _mod_head_mark[0]; \
    GOBJ_STRUCT_HEADER;    \
    a_u32 size;           \
    a_u32 sig;            \
    a_u32 len;            \
    a_u32 hmask;          \
    MNode* ptr;           \
    a_u32 nchg;           \
    a_u32 ftmz

enum {
    FIELD_MOD_TACC = 0x01, /* Private accessibility */
    FIELD_MOD_TUPD = 0x02, /* Private mutability */
    FIELD_MOD_TCFG = 0x04, /* Private configurability */
    FIELD_MOD_PACC = 0x08, /* Public accessibility */
    FIELD_MOD_PUPD = 0x10, /* Public mutability */
    FIELD_MOD_PCFG = 0x20, /* Public configurability */
};

struct MNode {
    Value value;
    GStr* key;
};

struct GMod {
    GMOD_STRUCT_HEADER;
    a_byte extra[];
};

#define v_is_mod(v) v_is(v, T_MOD)

always_inline GMod* v_as_mod(Value v) {
    assume(v_is_mod(v), "not mod.");
    return g_cast(GMod, v_as_obj(v));
}

#define gmod_cast(p) from_member(GMod, _mod_head_mark, &(p)->_mod_head_mark)

#define mod_size(l) pad_to(sizeof(GMod) + (l), sizeof(a_usize))

#endif /* amod_h_ */
