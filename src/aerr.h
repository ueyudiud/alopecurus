/**
 *@file aerr.h
 */

#ifndef aerr_h_
#define aerr_h_

#include "aobj.h"

intern a_none ai_err_raise(a_henv env, a_msg code, char const* fmt, ...);
intern a_none ai_err_raisev(a_henv env, a_msg code, char const* fmt, va_list varg);

#endif /* aerr_h_ */