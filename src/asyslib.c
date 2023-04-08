/**
 *@file asyslib.c
 */

#define asyslib_c_
#define ALO_LIB

#include <stdlib.h>
#include <process.h>

#include "avm.h"
#include "aapi.h"

#include "alolib.h"

static a_u32 sys_exit(a_henv env) {
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
			unreachable();
		}
	}
	ai_vm_hook(env, ALO_SEXIT, ALO_HMRAISE);
	alo_destroy(env);
	exit(code);
}

void aloopen_sys(a_henv env) {
	static aloL_Binding bindings[] = {
		{ "exit", sys_exit }
	};

	aloL_newmod(env, ALO_LIB_SYS_NAME, bindings);
}