/*
 * adump.c
 *
 *  Created on: 2022/7/30
 *      Author: ueyudiud
 */

#define adump_c_

#include <stdio.h>

#include "abc.h"
#include "aenv.h"
#include "agc.h"
#include "adump.h"

static void dump_const(a_henv env, Value const* v) {
	switch (v_raw_tag(v)) {
		case T_NIL: {
			printf("nil");
			break;
		}
		case T_FALSE: {
			printf("false");
			break;
		}
		case T_TRUE: {
			printf("true");
			break;
		}
		case T_INT: {
			printf("%d", v_as_int(v));
			break;
		}
		case T_FLOAT: {
			printf("%g", v_as_float(v));
			break;
		}
		case T_HSTR:
		case T_ISTR: {
			GStr* str = v_as_str(G(env), v);
			if (str->_len > 16) {
				printf("<%u bytes string>", str->_len);
			}
			else {
				printf("\"%s\"", str->_data);
			}
			break;
		}
		default: unreachable();
	}
}

static void dump_code(a_henv env, GFunMeta* meta) {
	static char const* g_names[] = {
#define BCNAME(id,name,a,b,c) name,
		ALO_BC_LIST(BCNAME)
#undef BCNAME
		null
	};

	a_insn* begin = meta->_insns;
	a_insn* end = begin + meta->_ninsn;
	for (a_insn* p = begin; p < end; ++p) {
		a_insn i = *p;
		a_u32 op = bc_load_op(i);
		assume(op < BC__MAX, "bad opcode.");
		a_u32 n = cast(a_u32, p - meta->_insns);
		printf("\t%5u %5s ", n, g_names[op]);
		switch (op) {
			case BC_NOP:
			case BC_RET0: {
				printf("   _    _    _\n");
				break;
			}
			case BC_KF:
			case BC_KT: {
				printf("%4u    _    _\n", bc_load_a(i));
				break;
			}
			case BC_MOV:
			case BC_LDC:
			case BC_STC:
			case BC_TNZ:
			case BC_TZ:
			case BC_NEG: {
				printf("%4u %4u    _\n", bc_load_a(i), bc_load_b(i));
				break;
			}
			case BC_BNZ:
			case BC_BZ:
			case BC_GET:
			case BC_SET:
			case BC_ADD:
			case BC_SUB:
			case BC_MUL:
			case BC_DIV:
			case BC_MOD:
			case BC_SHL:
			case BC_SHR:
			case BC_BAND:
			case BC_BOR:
			case BC_BXOR:
			case BC_UNBOX:
			case BC_CALL: {
				printf("%4u %4u %4u\n", bc_load_a(i), bc_load_b(i), bc_load_c(i));
				break;
			}
			case BC_GETK:
			case BC_CGETK:
			case BC_SETK: {
				a_u32 c = bc_load_c(i);
				printf("%4u %4u %4u ; ", bc_load_a(i), bc_load_b(i), c);
				dump_const(env, &meta->_consts[c]);
				printf("\n");
				break;
			}
			case BC_GETI:
			case BC_SETI:
			case BC_ADDI:
			case BC_SUBI:
			case BC_MULI:
			case BC_DIVI:
			case BC_MODI:
			case BC_SHLI:
			case BC_SHRI:
			case BC_BANDI:
			case BC_BORI:
			case BC_BXORI: {
				printf("%4u %4u %4d\n", bc_load_a(i), bc_load_b(i), bc_load_sc(i));
				break;
			}
			case BC_RETN: {
				printf("   _ %4u %4u\n", bc_load_b(i), bc_load_c(i));
				break;
			}
			case BC_RETV: {
				printf("   _ %4u    _\n", bc_load_b(i));
				break;
			}
			case BC_K: {
				a_u32 b = bc_load_bx(i);
				printf("%4u %9u ; ", bc_load_a(i), b);
				dump_const(env, &meta->_consts[b]);
				printf("\n");
				break;
			}
			case BC_KN: {
				printf("%4u    _ %4u\n", bc_load_a(i), bc_load_sbx(op));
				break;
			}
			case BC_KI: {
				printf("%4u %9d\n", bc_load_a(i), bc_load_sbx(i));
				break;
			}
			case BC_J: {
				a_i32 a = bc_load_sax(i);
				printf("%14d ; -> %u\n", a, n + a + 1);
				break;
			}
			default: unreachable();
		}
		fflush(stdout);
	}
}

#define Ks(s,n) n, ((n) > 1 ? s"s" : s"")

static void dump_meta(a_henv env, GFunMeta* meta, a_u32 options) {
	printf("function <%p>\n", meta);
	printf("%u stack, %u %s, %u %s, %u %s, %u %s\n",
		   meta->_nstack,
		   Ks("local", meta->_nlocal),
		   Ks("capture", meta->_ncap),
		   Ks("constant", meta->_nconst),
		   Ks("sub", meta->_nsub));
	dump_code(env, meta);
	/* Dump sub functions. */
	for (a_u32 i = 0; i < meta->_nsub; ++i) {
		dump_meta(env, meta->_subs[i], options);
	}
}

void ai_dump_print(a_henv env, GFunMeta* meta, a_u32 options) {
	dump_meta(env, meta, options);
}
