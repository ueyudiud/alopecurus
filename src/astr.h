/**
 *@file astr.h
 */

#ifndef astr_h_
#define astr_h_

#include "asym.h"
#include "aobj.h"

typedef a_msg (*a_sbfun)(void* ctx, void* dst, a_usize len);

ALO_PRIVATE a_hash ai_str_hashof(a_henv env, char const* ptr, a_usize len);
ALO_PRIVATE GStr* ai_str_get_or_nullptr_with_hash(a_henv env, char const* ptr, a_usize len, a_hash hash);
ALO_PRIVATE GStr* ai_str_new_with_hash(a_henv env, char const* ptr, a_usize len, a_hash hash);
ALO_PRIVATE GStr* ai_str_get_or_new_with_hash(a_henv env, char const* ptr, a_usize len, a_hash hash);
ALO_PRIVATE GStr* ai_str_get_or_new(a_henv env, char const* ptr, a_usize len);
ALO_PRIVATE a_msg ai_str_load(a_henv env, a_sbfun fun, a_usize len, void* ctx, GStr** pstr);
ALO_PRIVATE GStr* ai_str_format(a_henv env, char const* fmt, va_list varg);
ALO_PRIVATE void ai_str_boost1(a_henv env, void* block);
ALO_PRIVATE void ai_str_boost2(a_henv env);
ALO_PRIVATE void ai_str_cache_shrink_if_need(Global* gbl);
ALO_PRIVATE void ai_str_clean(Global* gbl);

struct GStr {
    GOBJ_STRUCT_HEADER;
    a_ulen len;
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

ALO_PRIVATE char const ai_str_interns[];

ALO_INLINE a_enum str_id(GStr* o) {
    return ptr2int(o->gnext) >> 48;
}

#define str_iskw(str) (str_id(str) >= STR__KW_BEGIN && str_id(str) <= STR__KW_END)
#define str_totk(str) (str_id(str) - STR__KW_BEGIN + TK__KW_BEGIN)

#define str_id_set(str,id) quiet((str)->gnext = bit_cast(a_gcnext, cast(a_usize, id) << 48))

#define g_is_str(o) g_is(o, ALO_TSTR)

#define v_is_str(v) v_is(v, T_STR)

ALO_INLINE GStr* v_as_str(Value v) {
    assume(v_is_str(v), "not string.");
    a_gptr p = v_as_ref(v);
    assume(g_is_str(p), "invalid instance.");
    return g_as(GStr, p);
}

ALO_INLINE Value v_of_str(GStr* o) {
    assume(g_is_str(o), "invalid instance.");
    return v_of_ref(o, T_STR);
}

ALO_INLINE void v_set_str(a_henv env, Value* d, GStr* o) {
    Value v = v_of_str(o);
    v_set(env, d, v);
}

#define str_size(l) align_forward(sizeof(GStr) + sizeof(char) * ((l) + 1), sizeof(a_usize))

ALO_INLINE char const* str2ntstr(GStr* self) {
    return self->ptr;
}

ALO_INLINE GStr* ai_str_from_ntstr(a_henv env, char const* src) {
    return ai_str_get_or_new(env, src, strlen(src));
}

#endif /* astr_h_ */
