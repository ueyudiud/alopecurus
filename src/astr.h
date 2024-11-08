/**
 *@file astr.h
 */

#ifndef astr_h_
#define astr_h_

#include "aobj.h"

typedef a_msg (*a_sbfun)(void* ctx, void* dst, a_usize len);

intern a_hash ai_str_hashof(a_henv env, a_lstr src);
intern GStr* ai_str_get_or_null_with_hash(a_henv env, a_lstr src, a_hash hash);
intern GStr* ai_str_new_with_hash(a_henv env, a_lstr src, a_hash hash);
intern GStr* ai_str_get_or_new_with_hash(a_henv env, a_lstr src, a_hash hash);
intern GStr* ai_str_get_or_new(a_henv env, a_lstr src);
intern a_msg ai_str_load(a_henv env, a_sbfun fun, a_usize len, void* ctx, GStr** pstr);
intern GStr* ai_str_format(a_henv env, char const* fmt, va_list varg);
intern void ai_str_boost1(a_henv env, void* block);
intern void ai_str_boost2(a_henv env);
intern void ai_str_cache_shrink_if_need(Global* gbl);
intern void ai_str_clean(Global* gbl);

struct GStr {
    GOBJ_STRUCT_HEADER;
    a_u32 len;
    a_hash hash;
    GStr* snext;
    char ptr[];
};

enum {
#define SYMDEF(n,r) BEGIN_STR_##n, END_STR_##n = BEGIN_STR_##n + sizeof(r) - 1,
    SYM_ALL(SYMDEF, M_void, M_void)
#undef SYMDEF
    STR__TOTAL_LEN,

    STR_POS__DUMMY = ISIZE_MAX /* Pad enumeration to a_isize type. */
};

intern char const ai_str_interns[STR__TOTAL_LEN];

always_inline a_enum str_id(GStr* o) {
    return bit_cast(a_usize, o->gnext) >> 48;
}

#define str_iskw(str) (str_id(str) >= STR__KW_BEGIN && str_id(str) <= STR__KW_END)
#define str_totk(str) (str_id(str) - STR__KW_BEGIN + TK__KW_BEGIN)

#define str_id_set(str,id) quiet((str)->gnext = bit_cast(a_gcnext, cast(a_usize, id) << 48))

#define g_is_str(o) g_is(o, ALO_TSTR)

#define v_is_str(v) v_is(v, T_STR)

always_inline GStr* v_as_str(Value v) {
    assume(v_is_str(v), "not string.");
    a_gptr p = v_as_obj(v);
    assume(g_is_str(p), "invalid instance.");
    return g_as(GStr, p);
}

always_inline Value v_of_str(GStr* o) {
    assume(g_is_str(o), "invalid instance.");
    return v_of_obj_(o, T_STR);
}

always_inline void v_set_str(a_henv env, Value* d, GStr* o) {
    Value v = v_of_str(o);
    v_set(env, d, v);
}

#define str_size(l) align_forward(sizeof(GStr) + sizeof(char) * ((l) + 1), sizeof(a_usize))

always_inline char const* str2ntstr(GStr* self) {
    return self->ptr;
}

#define ai_str_from_ntstr(env,src) ai_str_get_or_new(env, nt2lstr(src))

#endif /* astr_h_ */
