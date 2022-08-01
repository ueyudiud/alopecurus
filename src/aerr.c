/**
 *@file aerr.c
 */

#define aerr_c_

#include "astr.h"
#include "aenv.h"
#include "agc.h"

#include "aerr.h"

a_none ai_err_raise(a_henv env, a_msg code, char const* fmt, ...) {
    va_list varg;
    va_start(varg, fmt);
    ai_err_raisev(env, code, fmt, varg);
    va_end(varg);
}

a_none ai_err_raisev(a_henv env, a_msg code, char const* fmt, va_list varg) {
    Global* g = G(env);
    GStr* str = ai_str_formatv(env, fmt, varg);
    v_set(g, &env->_error, v_of_ref(str));
    if (g->_erf != null) {
        (*g->_erf)(env, code, g->_erc);
    }
    ai_env_raise(env, code);
}