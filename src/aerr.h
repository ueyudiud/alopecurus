/**
 *@file aerr.h
 */

#ifndef aerr_h_
#define aerr_h_

#include "aobj.h"

intern a_noret ai_err_raisef(a_henv env, a_msg msg, char const* fmt, ...);
intern a_noret ai_err_raisevf(a_henv env, a_msg msg, char const* fmt, va_list varg);
intern a_noret ai_err_raise(a_henv env, a_msg msg, Value err);
intern void ai_err_except(a_henv env, a_msg msg);

always_inline a_noret ai_err_bad_key(a_henv env, char const* coll, char const* key) {
	ai_err_raisef(env, ALO_EINVAL, "%s cannot be index for %s.", key, coll);
}

always_inline a_noret ai_err_bad_look(a_henv env, char const* type, GStr* key) {
	ai_err_raisef(env, ALO_EINVAL, "method '%s' not found for %s.", str2ntstr(key), type);
}

always_inline a_noret ai_err_bad_tm(a_henv env, a_u32 tm) {
	GStr* name = g_str(env, STR_TM__FIRST + tm);
	ai_err_raisef(env, ALO_EINVAL, "method '%s' not found.", str2ntstr(name));
}

#endif /* aerr_h_ */