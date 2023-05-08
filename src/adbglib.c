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

static a_flags l_parse_debug_options(char const* str) {
	a_flags flags = ALO_DUMP_OPT_NOTHING;
	char const* p = str;
	while (*p != '\0') {
		switch (*(p++)) {
			case 'l': {
				flags |= ALO_DUMP_OPT_LINE;
				break;
			}
			case 'k': {
				flags |= ALO_DUMP_OPT_CONST_POOL;
				break;
			}
			case 'v': {
				flags |= ALO_DUMP_OPT_LOCAL;
				break;
			}
		}
	}
	return flags;
}

static a_u32 debug_dump(a_henv env) {
	aloL_checktag(env, 0, ALO_TFUNC);
	char const* options = aloL_optstr(env, 1) ?: "";
	alo_dump(env, 0, l_parse_debug_options(options));
	return 0;
}

void aloopen_debug(a_henv env) {
	static aloL_Entry bindings[] = {
		{ "dump", debug_dump }
	};

	alo_newtype(env, ALO_LIB_DEBUG_NAME, ALO_NEWTYPE_FLAG_STATIC);
	aloL_putfields(env, -1, bindings);
}
