/*
 * adump.c
 *
 *  Created on: 2022/7/30
 *      Author: ueyudiud
 */

#define adump_c_

#include <stdio.h>

#include "abc.h"
#include "afun.h"
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
		default: {
			printf("%g", v_as_float(v));
			break;
		}
	}
}

typedef struct {
	a_u32 _index;
	a_u32 _limit;
} LineItr;

static void dump_line(GFunMeta* meta, LineItr* itr, a_u32 pc) {
	if (pc >= itr->_limit) {
		a_u32 index = itr->_index++;
		assume(index < meta->_nline);
		LineInfo* info = &meta->_lines[index];
		printf("line %u\n", info->_lineno);
		itr->_limit = info->_end;
	}
}

static void dump_code(a_henv env, GFunMeta* meta, a_bool fline) {
	static char const* g_names[] = {
#define BCNAME(id,name,a,b,c) name,
		ALO_BC_LIST(BCNAME)
#undef BCNAME
		null
	};

	a_insn* begin = meta->_insns;
	a_insn* end = begin + meta->_ninsn;
	LineItr line_itr = {};
	for (a_insn* p = begin; p < end; ++p) {
		a_insn i = *p;
		a_u32 op = bc_load_op(i);
		assume(op < BC__MAX, "bad opcode.");
		a_u32 n = cast(a_u32, p - meta->_insns);
		if (fline) {
			dump_line(meta, &line_itr, n);
		}
		printf("\t%5u %5s ", n, g_names[op]);
		switch (op) {
			case BC_NOP: {
				printf("   _    _    _\n");
				break;
			}
			case BC_KF:
			case BC_KT: {
				printf("%4u    _    _\n", bc_load_a(i));
				break;
			}
			case BC_BNZ:
			case BC_BZ: {
				printf("   _ %4u    _\n", bc_load_b(i));
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
			case BC_TEQ:
			case BC_TNE:
			case BC_TLT:
			case BC_TLE:
			case BC_TNEW:
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
			case BC_CALL:
			case BC_CAT: {
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
			case BC_TEQI:
			case BC_TNEI:
			case BC_TLTI:
			case BC_TLEI:
			case BC_TGTI:
			case BC_TGEI:
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
			case BC_BEQ:
			case BC_BNE:
			case BC_BLT:
			case BC_BLE:
			case BC_RET: {
				printf("   _ %4u %4u\n", bc_load_b(i), bc_load_c(i));
				break;
			}
			case BC_BEQI:
			case BC_BNEI:
			case BC_BLTI:
			case BC_BLEI:
			case BC_BGTI:
			case BC_BGEI: {
				printf("   _ %4u %4d\n", bc_load_b(i), bc_load_sc(i));
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
				printf("%4u    _ %4u\n", bc_load_a(i), bc_load_c(i));
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
	dump_code(env, meta, (options & ALO_DUMP_OPT_LINE) != 0);
	if (options & ALO_DUMP_OPT_CONST_POOL) {
		printf("constant pool <%p>\n", meta->_consts);
		for (a_u32 i = 0; i < meta->_nconst; ++i) {
			printf("\t%5u\t", i);
			dump_const(env, &meta->_consts[i]);
			printf("\n");
		}
	}
	if (options & ALO_DUMP_OPT_LOCAL) {
		if (meta->_locals != null) {
			printf("local info table <%p>\n", meta->_locals);
			for (a_u32 i = 0; i < meta->_nlocal; ++i) {
				LocalInfo* info = &meta->_locals[i];
				printf("\t%5u\tR[%u]\t%s ; %u %u\n", i, info->_reg, info->_name->_data, info->_begin_label, info->_end_label);
			}
			printf("capture info table <%p>\n", meta->_caps);
			for (a_u32 i = 0; i < meta->_ncap; ++i) {
				CapInfo* info = &meta->_caps[i];
				GStr* name = meta->_cap_names[i];
				printf("\t%5u\t%c[%u]\t%s\n", i, info->_up ? 'C' : 'R', info->_reg, name->_data);
			}
		}
		else {
			printf("local info table <NULL>\n");
		}
	}
	printf("\n");
	/* Dump sub functions. */
	for (a_u32 i = 0; i < meta->_nsub; ++i) {
		dump_meta(env, meta->_subs[i], options);
	}
}

void ai_dump_print(a_henv env, GFunMeta* meta, a_u32 options) {
	dump_meta(env, meta, options);
}
