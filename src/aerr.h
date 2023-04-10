/**
 *@file aerr.h
 */

#ifndef aerr_h_
#define aerr_h_

#include "aobj.h"

intern a_none ai_err_raisef(a_henv env, a_msg code, char const* fmt, ...);
intern a_none ai_err_raisevf(a_henv env, a_msg code, char const* fmt, va_list varg);
intern a_none ai_err_raise(a_henv env, a_msg code, Value err);

always_inline a_none ai_err_bad_get(a_henv env, char const* coll, char const* key) {
	ai_err_raisef(env, ALO_EINVAL, "%s cannot be index for %s.", key, coll);
}

always_inline a_none ai_err_bad_tm(a_henv env, a_u32 tm) {
	GStr* name = env_name(env, NAME_TM__FIRST + tm);
	ai_err_raisef(env, ALO_EINVAL, "'%s' method not found.", str2ntstr(name));
}

#endif /* aerr_h_ */