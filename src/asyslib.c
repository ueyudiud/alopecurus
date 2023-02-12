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

a_none aloL_sys_exit(a_henv env, a_isize id) {
	Value const* v = api_rdslot(env, id);
	a_i32 exit_code;
	switch (v_raw_tag(v)) {
		case T_NIL:
		case T_FALSE:
			exit_code = 0;
			break;
		case T_TRUE:
			exit_code = 1;
			break;
		case T_INT:
			exit_code = v_as_int(v);
			break;
		default:
			api_panic("bad exit code.");
	}
	ai_vm_hook(env, ALO_SEXIT, ALO_HMRAISE);
	alo_destroy(env);
	exit(exit_code);
}

static a_u32 sys_exit(a_henv env) {
	aloL_sys_exit(env, 0);
}

void aloopen_sys(a_henv env) {
	static aloL_Binding bindings[] = {
		{ "exit", sys_exit }
	};

	aloL_newmod(env, ALO_LIB_SYS_NAME, bindings);
}