/**
 *@file aerr.c
 */

#define aerr_c_
#define ALO_LIB

#include "astr.h"
#include "aenv.h"
#include "agc.h"
#include "avm.h"

#include "aerr.h"

a_none ai_err_raisef(a_henv env, a_msg code, char const* fmt, ...) {
    va_list varg;
    va_start(varg, fmt);
	ai_err_raisevf(env, code, fmt, varg);
	va_end(varg);
}

a_none ai_err_raisevf(a_henv env, a_msg code, char const* fmt, va_list varg) {
	assume(code < 0, "cannot raise non error message.");
    GStr* str = ai_str_format(env, fmt, varg);
	ai_err_raise(env, code, v_of_obj(str));
}

a_none ai_err_raise(a_henv env, a_msg code, Value err) {
	v_set(env, &env->_error, err);
	ai_vm_hook(env, code, ALO_HMRAISE);
	ai_env_raise(env, code);
}