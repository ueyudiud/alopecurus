/**
 *@file aerr.c
 */

#define aerr_c_
#define ALO_LIB

#include "astr.h"
#include "aenv.h"
#include "avm.h"

#include "aerr.h"

a_noret ai_err_raisef(a_henv env, a_msg msg, char const* fmt, ...) {
    va_list varg;
    va_start(varg, fmt);
	ai_err_raisevf(env, msg, fmt, varg);
	va_end(varg);
}

a_noret ai_err_raisevf(a_henv env, a_msg msg, char const* fmt, va_list varg) {
	assume(msg < 0, "cannot raise non error message.");
    GStr* str = ai_str_format(env, fmt, varg);
	ai_err_raise(env, msg, v_of_str(str));
}

a_noret ai_err_raise(a_henv env, a_msg msg, Value err) {
	v_set(env, &env->error, err); /* Push object into error slot. */
	ai_vm_hook(env, msg, ALO_HMRAISE);
	ai_env_raise(env, msg);
}

void ai_err_except(a_henv env, a_msg msg) {
    if (env->errf != null) {
        (*env->errf)(env, env->errc, msg);
    }
}