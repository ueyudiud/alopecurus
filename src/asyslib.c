/**
 *@file asyslib.c
 */

#define asyslib_c_
#define ALO_LIB

#include <stdlib.h>
#include <process.h>

#include "amod.h"
#include "avm.h"
#include "aapi.h"

#include "alolib.h"
#include "asyslib.h"

static a_bool l_to_exit_code(Value const* v, a_i32* pcode) {
	if (v != null) {
		switch (v_get_tag(*v)) {
			case T_FALSE: {
				*pcode = 0;
				return true;
			}
			case T_TRUE: {
				*pcode = 1;
				return true;
			}
			case T_INT: {
				*pcode = v_as_int(*v);
				return true;
			}
			default: {
				return false;
			}
		}
	}
	else {
		*pcode = 0;
		return true;
	}
}

static a_none l_exit(a_henv env, a_i32 code) {
	ai_vm_hook(env, ALO_SEXIT, ALO_HMRAISE);
	alo_destroy(env);
	exit(code);
}

a_none aloL_sys_exit(a_henv env, a_isize id) {
	a_i32 code;
	if (!l_to_exit_code(api_roslot(env, id), &code)) {
		api_panic("bad exit code.");
	}
	l_exit(env, code);
}

static a_u32 sys_exit(a_henv env) {
	a_i32 code;
	if (!l_to_exit_code(api_roslot(env, 0), &code)) {
		aloL_errorf(env, "bad exit code.");
	}
	l_exit(env, code);
}

void aloopen_sys(a_henv env) {
	static aloL_Binding bindings[] = {
		{ "exit", sys_exit }
	};

	aloL_newmod(env, ALO_LIB_SYS_NAME, bindings);
}