/*
 * adbglib.c
 *
 *  Created on: 2023/2/15
 *      Author: ueyudiud
 */

#define adbglib_c_
#define ALO_LIB

#include "adef.h"

#include "alolib.h"

static a_u32 debug_dump(a_henv env) {
	aloL_checktag(env, 0, ALO_TFUNC);
	alo_dump(env, 0, ALO_DUMP_OPT_NOTHING);
	return 0;
}

void aloopen_debug(a_henv env) {
	static aloL_Entry bindings[] = {
		{ "dump", debug_dump }
	};

	alo_newtype(env, ALO_LIB_DEBUG_NAME, ALO_NEWTYPE_FLAG_STATIC);
	aloL_putfields(env, -1, bindings);
}
