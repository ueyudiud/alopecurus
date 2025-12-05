/**
 *@file aerr.h
 */

#ifndef aerr_h_
#define aerr_h_

#include "asym.h"
#include "astr.h"

struct GError {
	GOBJ_STRUCT_HEADER;
	a_u32 size; /* size of allocated error size, 0 for interned error */

};

ALO_NORETURN ALO_PRIVATE void ai_err_raisef(a_henv env, a_msg msg, char const* fmt, ...);
ALO_NORETURN ALO_PRIVATE void ai_err_raisevf(a_henv env, a_msg msg, char const* fmt, va_list varg);
ALO_NORETURN ALO_PRIVATE void ai_err_raise(a_henv env, a_msg msg, Value err);
ALO_PRIVATE void ai_err_except(a_henv env, a_msg msg);

ALO_NORETURN ALO_INLINE void ai_err_bad_key(a_henv env, char const* coll, char const* key) {
	ai_err_raisef(env, ALO_EINVAL, "%s cannot be index for %s.", key, coll);
}

ALO_NORETURN ALO_INLINE void ai_err_bad_look(a_henv env, char const* type, GStr* key) {
	ai_err_raisef(env, ALO_EINVAL, "method '%s.%s' not found.", type, str2ntstr(key));
}

ALO_NORETURN ALO_INLINE void ai_err_bad_tm(a_henv env, a_u32 tm) {
	GStr* name = g_str(env, STR__TM_BEGIN + tm);
	ai_err_raisef(env, ALO_EINVAL, "method '%s' not found.", str2ntstr(name));
}

#endif /* aerr_h_ */