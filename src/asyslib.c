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

static a_bool l_try_to_code(Value const* v, a_i32* pcode) {
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

static a_u32 sys_exit(a_henv env) {
	a_i32 code;
	if (!l_try_to_code(api_roslot(env, 0), &code)) {
		aloL_errorf(env, "bad exit code.");
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