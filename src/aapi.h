/**
 *@file aapi.h
 */

#ifndef aapi_h_
#define aapi_h_

#include "aenv.h"

#ifdef ALOI_CHECK_API
# define api_panic(...) ai_dbg_panic("API: " __VA_ARGS__)
#else
# define api_panic(...) unreachable()
#endif

#define api_check(e,...) ((e) || (api_panic(__VA_ARGS__), false))

ALO_PRIVATE Value const* api_roslot(a_henv env, a_ilen id);
ALO_PRIVATE Value const* api_rdslot(a_henv env, a_ilen id);
ALO_PRIVATE Value* api_wrslot(a_henv env, a_ilen id);
ALO_PRIVATE Value* api_stack(a_henv env, a_ilen id);
ALO_PRIVATE Value api_elem(a_henv env, a_ilen id);
ALO_PRIVATE a_msg api_tagof(a_henv env, Value v);

ALO_PRIVATE char const ai_api_tagname[][8];

ALO_INLINE Value* api_stack_limit(a_henv env) {
#ifdef ALOI_CHECK_API
    return stk2val(env, env->frame->stack_limit);
#else
    return env->stack.limit;
#endif
}

ALO_INLINE void api_check_slot(a_henv env, a_ulen size) {
    api_check(env->stack.top + size <= api_stack_limit(env), "no enough stack slot.");
}

ALO_INLINE void api_check_elem(a_henv env, a_ulen size) {
    api_check(ai_stk_bot(env) + size <= env->stack.top, "no enough stack element.");
}

ALO_INLINE Value* api_incr_stack(a_henv env) {
    api_check_slot(env, 1);
    Value* p = env->stack.top++;
    v_check_alive(env, *p); /* Aliveness check. */
    return p;
}

ALO_INLINE Value api_pre_decr_stack(a_henv env) {
    api_check_elem(env, 1);
    return *(env->stack.top - 1);
}

ALO_INLINE void api_post_decr_stack(a_henv env) {
    env->stack.top -= 1;
}

ALO_INLINE Value api_decr_stack(a_henv env) {
    Value v = api_pre_decr_stack(env);
    api_post_decr_stack(env);
    return v;
}

ALO_PRIVATE char const ai_api_version[];

#endif /* aapi_h_ */
