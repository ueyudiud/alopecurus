/**
 *@file adump.c
 */

#define adump_c_
#define ALO_LIB

#include "abc.h"
#include "afun.h"
#include "agc.h"
#include "adump.h"

#include "abaselib.h"

static void dump_const(Value v) {
	switch (v_get_tag(v)) {
		case T_NIL: {
			aloi_show("nil");
			break;
		}
		case T_FALSE: {
			aloi_show("false");
			break;
		}
		case T_TRUE: {
			aloi_show("true");
			break;
		}
		case T_INT: {
			aloi_show("%d", v_as_int(v));
			break;
		}
		case T_FLOAT: {
			aloi_show("%g", v_as_float(v));
			break;
		}
		case T_HSTR:
		case T_ISTR: {
			GStr* str = v_as_str(v);
			if (str->_len > 16) {
				aloi_show("<%u bytes string>", str->_len);
			}
			else {
				aloi_show("\"%s\"", str2ntstr(str));
			}
			break;
		}
		default: {
			aloi_show("%g", v_as_float(v));
			break;
		}
	}
}

typedef struct {
	a_u32 _index;
	a_u32 _limit;
} LineItr;

static void dump_line(GProto* meta, LineItr* itr, a_u32 pc) {
	if (pc >= itr->_limit) {
		a_u32 index = itr->_index++;
		assume(index < meta->_nline);
		LineInfo* info = &meta->_dbg_lines[index];
		aloi_show("line %u\n", info->_lineno);
		itr->_limit = info->_end;
	}
}

static void dump_code(GProto* meta, a_bool fline) {
	a_insn* begin = meta->_code;
	a_insn* end = begin + meta->_ninsn;
	LineItr line_itr = {};
	for (a_insn* p = begin; p < end; ++p) {
		a_insn i = *p;
		a_u32 op = bc_load_op(i);
		assume(op < BC__MAX, "bad opcode.");
		a_u32 n = cast(a_u32, p - meta->_code);
		if (fline) {
			dump_line(meta, &line_itr, n);
		}
		aloi_show("\t%5u %5s ", n, ai_bc_names[op]);
		switch (op) {
			case BC_NOP:
			case BC_RET0: {
				aloi_show("   _    _    _\n");
				break;
			}
			case BC_KF:
			case BC_KT:
			case BC_BKF:
			case BC_BKT:
			case BC_CLOSE:
			case BC_BNZ:
			case BC_BZ:
			case BC_RET1:
			case BC_RETV: {
				aloi_show("%4u    _    _\n", bc_load_a(i));
				break;
			}
			case BC_K: {
				a_u32 b = bc_load_bx(i);
				aloi_show("%4u %9u ; ", bc_load_a(i), b);
				dump_const(meta->_consts[b]);
				aloi_show("\n");
				break;
			}
			case BC_KN: {
				aloi_show("%4u    _ %4u\n", bc_load_a(i), bc_load_c(i));
				break;
			}
			case BC_KI:
			case BC_BEQI:
			case BC_BNEI:
			case BC_BLTI:
			case BC_BLEI:
			case BC_BGTI:
			case BC_BGEI: {
				aloi_show("%4u %9d\n", bc_load_a(i), bc_load_sbx(i));
				break;
			}
			case BC_LDF: {
				a_u32 b = bc_load_bx(i);
				aloi_show("%4u %4u    _ ; %p\n", bc_load_a(i), b, meta->_subs[b]);
				break;
			}
			case BC_MOV:
			case BC_LDC:
			case BC_STC:
			case BC_NEG:
			case BC_BEQ:
			case BC_BNE:
			case BC_BLT:
			case BC_BLE:
			case BC_RET: {
				aloi_show("%4u %4u    _\n", bc_load_a(i), bc_load_b(i));
				break;
			}
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
				aloi_show("%4u %4u %4u\n", bc_load_a(i), bc_load_b(i), bc_load_c(i));
				break;
			}
			case BC_GETS:
			case BC_CGETS:
			case BC_SETK: {
				a_u32 c = bc_load_c(i);
				aloi_show("%4u %4u %4u ; ", bc_load_a(i), bc_load_b(i), c);
				dump_const(meta->_consts[c]);
				aloi_show("\n");
				break;
			}
			case BC_GETSX:
			case BC_CGETSX: {
				a_u32 ex = bc_load_ax(*p++);
				aloi_show("%4u %4u %4u ; ", bc_load_a(i), bc_load_b(i), ex);
				dump_const(meta->_consts[ex]);
				aloi_show("\n");
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
				aloi_show("%4u %4u %4d\n", bc_load_a(i), bc_load_b(i), bc_load_sc(i));
				break;
			}
			case BC_J: {
				a_i32 a = bc_load_sax(i);
				aloi_show("%14d ; -> %u\n", a, n + a + 1);
				break;
			}
			default: unreachable();
		}
		fflush(stdout);
	}
}

#define Ks(s,n) n, ((n) > 1 ? s"s" : s"")

static void dump_proto(a_henv env, GProto* proto, a_u32 options) {
	aloi_show("fn ");
	if (proto->_name != null) {
		aloi_show("%s ", str2ntstr(proto->_name));
	}
	if (proto->_dbg_file != null) {
		aloi_show("<%s:%u,%u>", str2ntstr(proto->_dbg_file), proto->_dbg_lndef, proto->_dbg_lnldef);
	}
	aloi_show("\n%u %s, %u %s, %u %s, %u %s, %u %s, %u %s\n",
			Ks("param", proto->_nparam),
			Ks("slot", proto->_nstack),
			Ks("local", proto->_nlocal),
			Ks("capture", proto->_ncap),
			Ks("constant", proto->_nconst),
			Ks("sub", proto->_nsub));
	dump_code(proto, (options & ALO_DUMP_OPT_LINE) != 0);
	if (options & ALO_DUMP_OPT_CONST_POOL) {
		aloi_show("constant pool <%p>\n", proto->_consts);
		for (a_u32 i = 0; i < proto->_nconst; ++i) {
			aloi_show("\t%5u\t", i);
			dump_const(proto->_consts[i]);
			aloi_show("\n");
		}
	}
	if (options & ALO_DUMP_OPT_LOCAL) {
		if (proto->_dbg_locals != null) {
			aloi_show("local info table <%p>\n", proto->_dbg_locals);
			for (a_u32 i = 0; i < proto->_nlocal; ++i) {
				LocalInfo* info = &proto->_dbg_locals[i];
				aloi_show("\t%5u\tR[%u]\t%s ; %u %u\n", i, info->_reg, str2ntstr(info->_name), info->_begin_label, info->_end_label);
			}
			printf("capture info table <%p>\n", proto->_caps);
			for (a_u32 i = 0; i < proto->_ncap; ++i) {
				CapInfo* info = &proto->_caps[i];
				GStr* name = proto->_dbg_cap_names[i];
				aloi_show("\t%5u\t%c[%u]\t%s\n", i, info->_fup ? 'C' : 'R', info->_reg, str2ntstr(name));
			}
		}
		else {
			aloi_show("local info table <NULL>\n");
		}
	}
	/* Dump sub functions. */
	for (a_u32 i = 0; i < proto->_nsub; ++i) {
		aloi_show_newline();
		dump_proto(env, proto->_subs[i], options);
	}
}

void ai_dump_print(a_henv env, GProto* proto, a_u32 options) {
	dump_proto(env, proto, options);
	aloi_show_flush();
}
