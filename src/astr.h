/**
 *@file astr.h
 */

#ifndef astr_h_
#define astr_h_

#include "astrs.h"
#include "aobj.h"
#include "aio.h"

typedef a_msg (*a_sbfun)(void*, void*, a_usize);

intern a_hash ai_str_hashof(a_henv env, char const* src, a_usize len);
intern GStr* ai_str_get_or_null_with_hash(a_henv env, char const* src, a_usize len, a_hash hash);
intern GStr* ai_str_new_with_hash(a_henv env, char const* src, a_usize len, a_hash hash);
intern GStr* ai_str_get_or_new_with_hash(a_henv env, char const* src, a_usize len, a_hash hash);
intern GStr* ai_str_get_or_new(a_henv env, char const* src, a_usize len);
intern a_msg ai_str_load(a_henv env, a_sbfun fun, a_usize len, void* ctx, GStr** pstr);
intern GStr* ai_str_format(a_henv env, char const* fmt, va_list varg);
intern a_bool ai_str_requals(GStr* self, void const* dat, a_usize len);
intern void ai_str_boost1(a_henv env, void* block);
intern void ai_str_boost2(a_henv env);
intern void ai_str_clean(Global* gbl);

struct GStr {
    GOBJ_STRUCT_HEADER;
    a_u32 len;
    a_hash hash;
    GStr* snext;
    char ptr[];
};

#define g_is_str(p) ((p)->vptr->tag == ALO_TSTR)

#define v_is_str(v) v_is(v, T_STR)

always_inline GStr* v_as_str(Value v) {
    assume(v_is_str(v), "not string.");
    a_gptr p = v_as_obj(v);
    assume(g_is_str(p), "invalid instance.");
    return g_cast(GStr, p);
}

always_inline Value v_of_str(GStr* p) {
    assume(g_is_str(p), "invalid instance.");
    return v_new(v_stencil(T_STR) | ptr2int(p));
}

always_inline void v_set_str(a_henv env, Value* d, GStr* p) {
    assume(g_is_str(p), "invalid instance.");
    v_set(env, d, v_of_str(p));
}

#define str_size(l) pad_to_raw(sizeof(GStr) + sizeof(char) * (l) + 1, sizeof(a_usize))

always_inline char const* str2ntstr(GStr* self) {
    return self->ptr;
}

#define ai_str_from_ntstr(env,src) ai_str_get_or_new(env, src, strlen(src))

intern char const ai_str_interns[STR__TOTAL_LEN];

#endif /* astr_h_ */
