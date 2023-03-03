/*
 * aapi.h
 */

#ifndef aapi_h_
#define aapi_h_

#include "aenv.h"

#define api_panic(m...) panic(m)
#define api_check(e,m...) assume(e, "API: "m)

intern Value const* api_roslot(a_henv env, a_isize id);
intern Value const* api_rdslot(a_henv env, a_isize id);
intern Value* api_wrslot(a_henv env, a_isize id);
intern Value api_elem(a_henv env, a_isize id);

always_inline Value const* api_stack_limit(a_henv env) {
#if ALO_STRICT_STACK_CHECK
	return env->_frame->_bound;
#else
    return env->_stack._limit;
#endif
}

always_inline void api_check_slot(a_henv env, a_usize size) {
    api_check(env->_stack._top + size <= api_stack_limit(env), "no enough stack slot.");
}

always_inline void api_check_elem(a_henv env, a_usize size) {
    api_check(ai_stk_bot(env) + size <= env->_stack._top, "no enough stack element.");
}

always_inline Value* api_incr_stack(a_henv env) {
    api_check_slot(env, 1);
    return env->_stack._top++;
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
