/**
 *@file adbglib.h
 */

#ifndef adbglib_h_
#define adbglib_h_

#include "alo.h"

#define ALOE_DUMP_OPT_NOTHING 0x0
#define ALOE_DUMP_OPT_CONST_POOL 0x1
#define ALOE_DUMP_OPT_LOCAL 0x2
#define ALOE_DUMP_OPT_LINE 0x4

ALO_EXPORT void aloE_fdump(a_henv env, a_isize id, a_flags options);

#endif
