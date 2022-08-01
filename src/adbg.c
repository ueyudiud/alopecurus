/*
 * adbg.c
 */

#define adbg_c_

#include <stdio.h>

#include "adbg.h"

#if defined(ALOI_DEBUG)

a_none ai_dbg_panic(char const* fmt, ...) {
	va_list varg;
	va_start(varg, fmt);
	vfprintf(stderr, fmt, varg);
	fflush(stderr);
	va_end(varg);
	trap();
}

#endif
