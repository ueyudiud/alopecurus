/*
 * aapi.h
 */

#ifndef aapi_h_
#define aapi_h_

#include "aenv.h"

#ifdef ALOI_CHECK_API
# define api_panic(m...) ai_dbg_panic("API: "m)
#else
# define api_panic(...) unreachable()
#endif

#define api_check(e,m...) ((e) || (api_panic(m), false))

intern Value const* api_roslot(a_henv env, a_istk id);
intern Value const* api_rdslot(a_henv env, a_istk id);
intern Value* api_wrslot(a_henv env, a_istk id);
intern Value* api_stack(a_henv env, a_istk id);
intern Value api_elem(a_henv env, a_istk id);
intern a_msg api_tagof(a_henv env, Value v);

intern char const ai_api_tagname[][8];

always_inline Value* api_stack_limit(a_henv env) {
#ifdef ALOI_CHECK_API
    return stk2val(env, env->_frame->_stack_limit);
#else
    return env->_stack._limit;
#endif
}

always_inline void api_check_slot(a_henv env, a_ustk size) {
    api_check(env->_stack._top + size <= api_stack_limit(env), "no enough stack slot.");
}

always_inline void api_check_elem(a_henv env, a_ustk size) {
    api_check(ai_stk_bot(env) + size <= env->_stack._top, "no enough stack element.");
}

always_inline Value* api_incr_stack(a_henv env) {
    api_check_slot(env, 1);
    Value* p = env->_stack._top++;
    v_check_alive(env, *p); /* Aliveness check. */
    return p;
}

always_inline Value api_pre_decr_stack(a_henv env) {
    api_check_elem(env, 1);
    return *(env->_stack._top - 1);
}

always_inline void api_post_decr_stack(a_henv env) {
    env->_stack._top -= 1;
}

always_inline Value api_decr_stack(a_henv env) {
    Value v = api_pre_decr_stack(env);
    api_post_decr_stack(env);
    return v;
}

#endif /* aapi_h_ */
