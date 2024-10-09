/**
 *@file asyslib.c
 */

#define asyslib_c_
#define ALO_LIB

#include <stdlib.h>
#include <time.h>
#include <errno.h>

#include "agc.h"
#include "avm.h"
#include "aapi.h"

#include "alolib.h"

static a_msg sys_exit(a_henv env) {
	Value v_code = api_elem(env, 0);
	a_i32 code;
	switch (v_get_tag(v_code)) {
		case T_NIL:
		case T_FALSE: {
			code = EXIT_SUCCESS;
			break;
		}
		case T_TRUE: {
			code = EXIT_FAILURE;
			break;
		}
		case T_INT: {
			code = v_as_int(v_code);
			break;
		}
		default: {
			aloL_argerror(env, 0, "bad exit code.");
		}
	}
	alo_destroy(env);
	exit(code);
}

static a_msg sys_command(a_henv env) {
	char const* cmd = aloL_optstr(env, 0);
	errno = 0;
	int stat = system(cmd);
	if (cmd != null) {
		return aloL_resulte(env, stat);
	}
	else {
		alo_pushbool(env, stat);
		return 1;
	}
}

static a_msg sys_getenv(a_henv env) {
	char const* key = aloL_checkstr(env, 0);
	char const* val = getenv(key);
	if (val != null) {
		alo_pushntstr(env, val);
	}
	else {
		alo_pushnil(env);
	}
	return 1;
}

static a_msg sys_gc(a_henv env) {
    alo_gchint(env, ALO_GCHINT_FULL, 0);
    return 0;
}

static a_msg sys_clock(a_henv env) {
    clock_t time = clock();
    alo_pushfloat(env, cast(a_float, time) / cast(a_float, CLOCKS_PER_SEC));
    return 1;
}

void aloopen_sys(a_henv env) {
	static aloL_Entry const bindings[] = {
        { "clock", sys_clock },
		{ "command", sys_command },
		{ "exit", sys_exit },
        { "gc", sys_gc },
		{ "getenv", sys_getenv }
	};

    alo_NewType const info = {
        .name = ALO_LIB_SYS_NAME,
        .flags = ALO_NEWTYPE_STATIC
    };

    alo_newtype(env, &info);
    aloL_putalls(env, -1, bindings);
}