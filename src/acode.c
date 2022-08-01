/**
 *@file acode.c
 */

#define acode_c_

#include <string.h>

#include "abc.h"
#include "afun.h"
#include "aenv.h"
#include "aop.h"
#include "agc.h"
#include "astrx.h"

#include "acode.h"

enum {
	/* Mark operand which is not determined. */
	DYN = 0,
	/* Mark unused operand. */
	DMB = 0
};

static void l_nomem_error(Parser* par) {
	ai_code_close(par);
	ai_mem_nomem(par->_env);
}

#define l_vgrow(par,vec,size_old,size_new) ({ \
    typeof(vec) _v = ai_mem_vxgrow((par)->_env, vec, size_old, size_new); \
	if (unlikely(_v == null)) { l_nomem_error(par); }                        \
	_v;                                          \
})

#define l_vdel(par,vec,len) ai_mem_vdel(G((par)->_env), vec, len)

inline void expr_copy(OutExpr dst, InExpr src) {
	*dst = *src;
}

static void l_close_scope(Parser* par, FnScope* scope) {
	if (scope->_consts._dat != null) {
		l_vdel(par, scope->_consts._dat, scope->_consts._cap);
	}
	l_vdel(par, scope->_caps, scope->_ccap);
}

/**
 ** Get last instruction if it is in sequential control flow.
 *@param par the parser.
 *@return the instruction.
 */
static a_u32 l_last_insn(Parser* par) {
	if (par->_scope->_begin_label < par->_ninsn) {
		a_insn insn = par->_insns[par->_ninsn - 1];
		assume(insn != 0); /* Instruction should not be 0. */
		return insn;
	}
	return 0;
}

#define LINE_GROW_UNIT 1024
#define INSN_GROW_UNIT 4096

static void l_emit_line(Parser* par, a_u32 line) {
	Lines* lines = &par->_lines;
	if (lines->_top == lines->_cap) {
		a_usize old_cap = par->_cinsn;
		a_usize new_cap = old_cap + LINE_GROW_UNIT;
		lines->_stack = l_vgrow(par, lines->_stack, old_cap, new_cap);
		lines->_cap = new_cap;
	}
	lines->_stack[lines->_top++] = new(Line) { par->_ninsn, line };
}

static a_u32 l_emit_direct(Parser* par, a_insn i, a_u32 line) {
	if (par->_head_line != line) {
		l_emit_line(par, line);
	}
	if (par->_ninsn == par->_cinsn) {
		a_usize old_cap = par->_cinsn;
		a_usize new_cap = old_cap + INSN_GROW_UNIT;
		par->_insns = l_vgrow(par, par->_insns, old_cap, new_cap);
		par->_cinsn = new_cap;
	}
	a_u32 label = par->_ninsn ++;
	par->_insns[label] = i;
	return label;
}

static a_bool l_should_eval(Parser* par) {
	return likely(par->_fpass || par->_fland);
}

static a_i32 l_make_jump_diff(Parser* par, a_u32 from, a_u32 to, a_u32 line) {
	a_i32 diff = cast(a_i32, to - from - 1);
	if (unlikely(diff < BC_MIN_SAX || diff > BC_MAX_SAX)) {
		l_error(par, "instruction jump out of bound.", line);
	}
	return diff;
}

static a_u32 l_emit_jump_direct(Parser* par, a_u32 label, a_u32 line) {
	a_i32 diff = label != NO_LABEL ? l_make_jump_diff(par, par->_ninsn, label, line) : -1;
	return l_emit_direct(par, bc_make_isax(BC_J, diff), line);
}

static a_u32 l_last_jump(Parser* par, a_u32 label) {
	assume(label <= par->_ninsn, "not valid label.");
	if (label == par->_ninsn)
		return par->_head_land;

	a_insn i = par->_insns[label];
	assume(bc_load_op(i) == BC_J);

	a_i32 disp = bc_load_sax(i);
	return disp != -1 ? cast(a_u32, cast(a_i32, label + 1) + disp) : NO_LABEL;
}

static void l_redirect(Parser* par, a_u32 from, a_u32 to, a_u32 line) {
	a_i32 diff = l_make_jump_diff(par, from, to, line);
	a_insn* pinsn = &par->_insns[from];
	assume(bc_load_op(*pinsn) == BC_J);
	bc_swap_sax(pinsn, diff);
}

/**
 ** Redirect all jump instruction in jump chain to the current position.
 *@param par the parser.
 *@param from the unresolved jump chain.
 *@param to the destination.
 *@param line the line number for operation.
 */
static void l_redirect_chain(Parser* par, a_u32 from, a_u32 to, a_u32 line) {
	loop {
		a_u32 label2 = l_last_jump(par, from);
		l_redirect(par, from, to, line);
		if (label2 == NO_LABEL) break;
		from = label2;
	}
}

static void l_clear_jump(Parser* par) {
	par->_fjump = false;
	par->_head_jump = NO_LABEL;
}

static void l_clear_land(Parser* par) {
	par->_fland = false;
	par->_head_land = NO_LABEL;
}

static void l_emit_fast(Parser* par, a_insn i, a_u32 line) {
	if (par->_fpass) {
		l_emit_direct(par, i, line);
	}
}

static void l_flush_jump(Parser* par, a_u32 line) {
	if (par->_fjump) {
		assume(par->_fland); /* When jump is defined, the branch is reachable only if land is also defined. */
		/* Link to previous jump instruction. */
		l_emit_jump_direct(par, par->_head_jump, par->_head_jump_line);
		l_clear_jump(par);
	}
	if (par->_fland) {
		l_redirect_chain(par, par->_head_land, par->_ninsn, line);
		l_clear_land(par);
	}
	par->_fpass = true;
}

static a_u32 l_emit(Parser* par, a_insn i, a_u32 line) {
	if (l_should_eval(par)) {
		l_flush_jump(par, line);
		return l_emit_direct(par, i, line);
	}
	return NO_LABEL;
}

static a_u32 l_emit_mov(Parser* par, a_u32 dst, a_u32 src, a_u32 line) {
	return l_emit(par, bc_make_iabc(BC_MOV, dst, src, DMB), line);
}

static a_u32 l_emit_kz(Parser* par, a_u32 dst, a_bool val, a_u32 line) {
	assume(val == false || val == true);
	return l_emit(par, bc_make_iabc(BC_KF | val, dst, DMB, DMB), line);
}

static void l_emit_ret(Parser* par, a_u32 base, a_u32 len, a_u32 line) {
	a_u32 insn = len > 0 ? bc_make_iabc(BC_RETN, DMB, base, len) : bc_make_iabc(BC_RET0, DMB, DMB, DMB);
	l_emit(par, insn, line);
	par->_fpass = false;
}

static a_u32 l_emit_kn(Parser* par, a_u32 dst, a_u32 len, a_u32 line) {
	assume(len > 0);
	if (par->_fpass && par->_fland) {
		a_insn insn = l_last_insn(par);
		if (bc_load_op(insn) == BC_KN) {
			a_u32 b = bc_load_b(insn);
			a_u32 c = bc_load_c(insn);
			a_u32 dst1 = dst;
			a_u32 dst2 = dst + len;
			a_u32 src1 = b;
			a_u32 src2 = b + c;
			if (!(dst2 < src1 || src2 < dst1)) {
				a_insn* pinsn = &par->_insns[par->_ninsn - 1];
				b = min(src1, dst1);
				c = max(src2, dst2) - b;
				bc_swap_b(pinsn, b);
				bc_swap_c(pinsn, c);
				return par->_ninsn - 1;
			}
		}
	}
	return l_emit(par, bc_make_iabc(BC_KN, dst, DMB, 1), line);
}

static a_u32 l_emit_branch(Parser* par, a_insn i, a_u32 label, a_u32 line) {
	a_u32 label2 = l_emit(par, i, line);
	return label2 != NO_LABEL ? l_emit_direct(par, bc_make_isax(BC_J, label), line) : NO_LABEL;
}

static a_u32 l_emit_test(Parser* par, a_u32 op, a_u32 reg, a_u32 label, a_u32 line) {
	return l_emit_branch(par, bc_make_iabc(op, DMB, reg, DMB), label, line);
}

static void l_merge_label(Parser* par, a_u32* plabel, a_u32 label2, a_u32 line) {
	if (label2 == NO_LABEL) return;
	a_u32 label1 = *plabel;
	if (label1 == NO_LABEL) {
		*plabel = label2;
	}
	else if (likely(label1 != label2)) {
		if (label1 < label2) {
			*plabel = label2;
			swap(label1, label2);
		}

		loop {
			a_u32 label3 = l_last_jump(par, label1);
			if (label3 == NO_LABEL) {
				l_redirect(par, label1, label2, line);
				return;
			}
			else if (label3 > label2) {
				label1 = label3;
			}
			else if (label3 < label2) {
				l_redirect(par, label1, label2, line);
				label1 = label2;
				label2 = label3;
			}
			else {
				return;
			}
		}
	}
}

#define l_lazy_jump ai_code_gotoU
#define l_mark_label ai_code_label

static void l_never(OutExpr e, a_u32 line) {
	e->_kind = EXPR_NEVER;
	e->_line = line;
}

void ai_code_constK(Parser* par, OutExpr e, a_u32 val, a_u32 line) {
	assume(val == EXPR_NIL || val == EXPR_FALSE || val == EXPR_TRUE || val == EXPR_UNIT);
	if (l_should_eval(par)) {
		e->_kind = val;
		e->_line = line;
	}
	else {
		l_never(e, line);
	}
}

void ai_code_constI(Parser* par, OutExpr e, a_int val, a_u32 line) {
	if (l_should_eval(par)) {
		e->_kind = EXPR_INT;
		e->_int = val;
		e->_line = line;
	}
	else {
		l_never(e, line);
	}
}

void ai_code_constF(Parser* par, OutExpr e, a_float val, a_u32 line) {
	if (l_should_eval(par)) {
		e->_kind = EXPR_FLOAT;
		e->_float = val;
		e->_line = line;
	}
	else {
		l_never(e, line);
	}
}

void ai_code_constS(Parser* par, OutExpr e, GStr* val, a_u32 line) {
	if (l_should_eval(par)) {
		e->_kind = EXPR_STR;
		e->_str = val;
		e->_line = line;
	}
	else {
		l_never(e, line);
	}
}

static a_u32 l_alloc_stack(Parser* par, a_u32 line) {
	assume(par->_scope->_num_fur == 0, "cannot allocate register while fragmented section exists.");
	a_u32 reg = par->_scope->_top_reg++;
	if (par->_scope->_top_reg > par->_fnscope->_max_reg) {
		par->_fnscope->_max_reg = par->_scope->_top_reg;
		if (reg >= BC_MAX_A) {
			l_error(par, "too many register used.", line);
		}
	}
	return reg;
}

static a_bool l_is_tmpR(Parser* par, a_u32 reg) {
	return reg >= par->_scope->_top_ntr;
}

/**
 ** Free register from temporary value stack.
 ** The register freed with the order different with reversed order of
 ** allocation.
 *@param par the parser.
 *@param reg the temporary register.
 */
static void l_free_stack(Parser* par, a_u32 reg) {
	Scope* scope = par->_scope;
	assume(reg < scope->_top_reg && l_is_tmpR(par, reg));
	/* The registers are likely freed with the reversed order of allocation. */
	if (likely(reg + 1 == scope->_top_reg)) {
		scope->_top_reg -= 1;
	}
	/* Or, mark register position to top of stack into critical section. */
	else {
		if (reg < scope->_bot_fur) {
			scope->_bot_fur = reg;
		}
		scope->_num_fur += 1;
	}
	/* Remove critical section if all registers in the section are fully freed. */
	if (unlikely(scope->_num_fur > 0) && scope->_bot_fur + scope->_num_fur == scope->_top_reg) {
		scope->_top_reg = scope->_bot_fur;
		scope->_num_fur = 0;
	}
}

/**
 ** Drop ownership for register if it is a temporary register.
 *@param par the parser.
 *@param reg the index of register.
 */
static void l_drop(Parser* par, a_u32 reg) {
	if (l_is_tmpR(par, reg)) {
		l_free_stack(par, reg);
	}
}

/**
 ** Load the index for constant.
 *@param par the parser.
 *@param consts the constant table.
 *@param val the constant to lookup.
 *@return the index of constant in constant pool.
 */
static a_u32 l_const_index(Parser* par, Value val) {
	ValBuf* consts = &par->_fnscope->_consts;
	for (a_u32 i = 0; i < consts->_len; ++i) {
		/*
		 * Since all literals which have the same format provided by compiler should
		 * have same binary data, use identity equality for comparison.
		 */
		if (v_id_eq(&consts->_dat[i], &val)) {
			return i;
		}
	}

	if (consts->_len == consts->_cap) {
		if (unlikely(consts->_len == BC_MAX_BX + 1)) {
			l_error(par, "too many constants.", par->_lex._line);
		}
		a_usize old_cap = consts->_cap;
		a_usize new_cap = old_cap + 256;
		consts->_dat = l_vgrow(par, consts->_dat, old_cap, new_cap);
		consts->_cap = new_cap;
	}

	a_u32 i = consts->_len ++;
	v_set(G(par->_env), &consts->_dat[i], val);
	return i;
}

static void l_dynR(Parser* par, InoutExpr e);
static void l_topR(Parser* par, InoutExpr e);
static void l_anyR(Parser* par, InoutExpr e);
static void l_anyRK(Parser* par, InoutExpr e);
static void l_fixR(Parser* par, InExpr e, a_u32 reg);
static a_u32 l_condT(Parser* par, InoutExpr e, a_u32* plabel, a_u32 line);
static a_u32 l_condF(Parser* par, InoutExpr e, a_u32* plabel, a_u32 line);
static a_u32 l_try(Parser* par, InoutExpr e, a_u32 line);

static a_u32 l_capture(Parser* par, FnScope* scope, Name* name, a_u32 depth) {
	/* Find in captured values. */
	for (a_u32 i = 0; i < scope->_ncap; ++i) {
		CapInfo* info = &scope->_caps[i];
		if (info->_iname == name->_index) {
			/* Already captured. */
			return i;
		}
	}
	/* Not found, create a new capture value. */
	if (scope->_ncap == scope->_ccap) {
		a_usize old_cap = scope->_ccap;
		a_usize new_cap = old_cap + 32;
		scope->_caps = l_vgrow(par, scope->_caps, old_cap, new_cap);
		scope->_ccap = new_cap;
	}

	a_u32 cid = scope->_ncap++;
	CapInfo* info = &scope->_caps[cid];
	info->_iname = name->_index;
	info->_scope = name->_scope;
	info->_index = name->_scope < depth - 1 ? l_capture(par, scope->_upper, name, depth - 1) : name->_index;
	return cid;
}

/* Get environment name. */
#define l_env_name(par) (&(par)->_names._stack[0])

static void l_load_name(Parser* par, OutExpr e, Name* name, a_u32 line) {
	switch (name->_kind) {
		case NAME_LOCAL: {
			if (par->_scope_depth == name->_scope) {
				e->_kind = EXPR_REG;
				e->_reg = name->_index;
			}
			else {
				e->_kind = EXPR_CAP;
				e->_reg = l_capture(par, par->_fnscope, name, par->_scope_depth);
			}
			break;
		}
		default: unreachable();
	}
}

void ai_code_lookupU(Parser* par, OutExpr e, GStr* name, a_u32 line) {
	Names* names = &par->_names;
	for (a_isize i = cast(a_isize, names->_top - 1); i >= 0; --i) {
		Name* info = &names->_stack[i];
		if (info->_name == name) {
			l_load_name(par, e, info, line);
			return;
		}
	}

	e->_kind = EXPR_CREFK;
	e->_ref._base = l_capture(par, par->_fnscope, l_env_name(par), par->_scope_depth);
	e->_ref._key = l_const_index(par, v_of_ref(name));
	e->_line = line;
}

void ai_code_lookupC(Parser* par, InoutExpr e, GStr* name, a_u32 line) {
	switch (e->_kind) {
		case EXPR_CAP: {
			e->_kind = EXPR_CREFK;
			e->_ref._base = e->_reg;
			break;
		}
		default: {
			l_anyR(par, e);
			e->_kind = EXPR_REFK;
			e->_ref._base = e->_reg;
			break;
		}
	}
	e->_ref._key = l_const_index(par, v_of_ref(name));
	e->_line = line;
}

/**
 ** Make reference of indexed expression.
 *@param par the parser.
 *@param ev the view expression.
 *@param ek the key expression.
 *@param line the line of operation.
 */
void ai_code_index(Parser* par, InoutExpr ev, InExpr ek, a_u32 line) {
	if (unlikely(ev->_kind == EXPR_NEVER)) {
		ev->_kind = EXPR_NEVER;
		ev->_line = line;
		return;
	}
	switch (ek->_kind) {
		/* Optimize for specific expressions. */
		case EXPR_NEVER: {
			break;
		}
		case EXPR_INT: {
			l_anyR(par, ev);
			a_int val = ek->_int;
			if (val >= 0 && val <= BC_MAX_C) {
				ev->_kind = EXPR_REFI;
				ev->_ref._key = cast(a_u32, val);
			}
			else {
				ev->_kind = EXPR_REF;
				ev->_ref._key = l_const_index(par, v_of_int(val));
			}
			ev->_ref._base = ev->_reg;
			ev->_line = line;
			break;
		}
		case EXPR_STR: {
			ai_code_lookupC(par, ev, ek->_str, line);
			break;
		}
		default: {
			/* Handle by normal expression. */
			l_anyR(par, ev);
			l_anyR(par, ek);
			if (likely(ev->_kind != EXPR_NEVER)) {
				ev->_kind = EXPR_REF;
				ev->_ref._base = ev->_reg;
				ev->_ref._key = ek->_reg;
				ev->_line = line;
			}
			break;
		}
	}
}

static void l_negate_branch(Parser* par, a_u32 label, a_u32* plabel, a_u32 line) {
	if (label + 1 == par->_ninsn) {
		/* Try to swap duality opcodes for */
		a_insn* pinsn = &par->_insns[label - 1];

		switch (bc_load_op(*pinsn)) { /* Lookup duality opcodes. */
			case BC_BZ: {
				bc_swap_op(pinsn, BC_BNZ);
				goto pack;
			}
			case BC_BNZ: {
				bc_swap_op(pinsn, BC_BZ);
				goto pack;
			}
		}
	}

	*plabel = l_lazy_jump(par, *plabel, line);
	l_mark_label(par, label, line);
	return;

	run pack: {
		l_merge_label(par, plabel, label - 1, line);
		return;
	}
}

static void l_instantiate_branch(Parser* par, InoutExpr e, a_bool flip) {
	assume(par->_ninsn == e->_label + 1);
	a_u32 label = e->_label - 1;
	a_insn* pinsn = &par->_insns[label];
	par->_ninsn = label + 1;
	e->_label = label;
	switch (bc_load_op(*pinsn)) {
		case BC_BZ: {
			bc_swap_op(pinsn, BC_TZ ^ flip);
			break;
		}
		case BC_BNZ: {
			bc_swap_op(pinsn, BC_TNZ ^ flip);
			break;
		}
		default: unreachable();
	}
	e->_kind = EXPR_DST_A;
}

void ai_code_prefix(Parser* par, InoutExpr e, a_u32 op, a_u32 line) {
	switch (op) {
		case OP_NEG: {
			switch (e->_kind) {
				case EXPR_NEVER: {
					break;
				}
				case EXPR_INT: {
					e->_int = ai_op_neg_int(par->_env, e->_int);
					e->_line = line;
					break;
				}
				case EXPR_FLOAT: {
					e->_float = ai_op_neg_float(par->_env, e->_float);
					e->_line = line;
					break;
				}
				default: {
					l_anyR(par, e);
					l_drop(par, e->_reg);
					e->_kind = EXPR_DST_A;
					e->_label = l_emit(par, bc_make_iabc(BC_NEG, DYN, e->_reg, DMB), line);
					break;
				}
			}
			break;
		}
		case OP_BIT_INV: {
			switch (e->_kind) {
				case EXPR_NEVER: {
					break;
				}
				case EXPR_INT: {
					e->_int = ai_op_bnot_int(par->_env, e->_int);
					e->_line = line;
					break;
				}
				default: {
					l_anyR(par, e);
					l_drop(par, e->_reg);
					e->_kind = EXPR_DST_A;
					e->_label = l_emit(par, bc_make_iabc(BC_NEG, DYN, e->_reg, DMB), line);
					break;
				}
			}
			break;
		}
		case OP_NOT: {
			switch (e->_kind) {
				case EXPR_NIL: {
					e->_kind = EXPR_TRUE;
					break;
				}
				case EXPR_FALSE:
				case EXPR_TRUE:
				case EXPR_RESIDUAL_FALSE:
				case EXPR_RESIDUAL_TRUE:
				case EXPR_TRY_TF:
				case EXPR_TRY_FT: {
					e->_kind ^= 1; /* Flip expression true value. */
					e->_line = line;
					break;
				}
				case EXPR_UNIT:
				case EXPR_INT:
				case EXPR_FLOAT:
				case EXPR_STR: {
					e->_kind = EXPR_FALSE;
					break;
				}
				default: {
					l_anyR(par, e);
					assume(e->_kind == EXPR_REG);

					a_u32 reg = e->_reg;
					a_u32 label = l_emit_test(par, BC_BNZ, reg, NO_LABEL, line);
					l_drop(par, reg);

					e->_kind = EXPR_TRY_TF;
					e->_label = label;
					e->_line = line;
					break;
				}
			}
			break;
		}
		case OP_UNBOX: {
			l_anyR(par, e);
			if (e->_kind != EXPR_NEVER) {
				e->_kind = EXPR_UNBOX;
			}
			break;
		}
		default: unreachable();
	}
}

void ai_code_infix(Parser* par, InoutExpr e, a_u32 op, a_u32 line) {
	switch (op) {
		case OP_ADD:
		case OP_SUB:
		case OP_MUL:
		case OP_DIV:
		case OP_MOD:
		case OP_SHL:
		case OP_SHR:
		case OP_BIT_AND:
		case OP_BIT_OR:
		case OP_BIT_XOR: {
			l_anyRK(par, e);
			break;
		}
		case OP_AND: {
			a_u32 label = NO_LABEL;
			a_u32 kind = l_condT(par, e, &label, line);
			ai_code_drop(par, e);
			e->_kind = kind;
			e->_label = label;
			break;
		}
		case OP_OR: {
			a_u32 label = NO_LABEL;
			a_u32 kind = l_condF(par, e, &label, line);
			ai_code_drop(par, e);
			e->_kind = kind;
			e->_label = label;
			break;
		}
		default: unreachable();
	}
}

static a_bool expr_are_ints(InExpr e1, a_int* i1, InExpr e2, a_int* i2) {
	if (e1->_kind == EXPR_INT && e2->_kind == EXPR_INT) {
		*i1 = e1->_int;
		*i2 = e2->_int;
		return true;
	}
	return false;
}

static a_bool expr_are_floats(InExpr e1, a_float* i1, InExpr e2, a_float* i2) {
	if (e1->_kind == EXPR_INT) {
		*i1 = cast(a_float, e1->_int);
	}
	else if (e1->_kind == EXPR_FLOAT) {
		*i1 = e1->_float;
	}
	else return false;
	if (e2->_kind == EXPR_INT) {
		*i2 = cast(a_float, e2->_int);
	}
	else if (e2->_kind == EXPR_FLOAT) {
		*i2 = e2->_float;
	}
	else return false;
	return true;
}

static a_int l_compute_int(a_henv env, a_int a, a_int b, a_u32 op) {
	switch (op) {
		case OP_ADD: return ai_op_add_int(env, a, b);
		case OP_SUB: return ai_op_sub_int(env, a, b);
		case OP_MUL: return ai_op_mul_int(env, a, b);
		case OP_DIV: return ai_op_div_int(env, a, b);
		case OP_MOD: return ai_op_mod_int(env, a, b);
		case OP_SHL: return ai_op_shl_int(env, a, b);
		case OP_SHR: return ai_op_shr_int(env, a, b);
		case OP_BIT_AND: return ai_op_band_int(env, a, b);
		case OP_BIT_OR: return ai_op_bor_int(env, a, b);
		case OP_BIT_XOR: return ai_op_bxor_int(env, a, b);
		default: unreachable();
	}
}

static a_float l_compute_float(a_henv env, a_float a, a_float b, a_u32 op) {
	switch (op) {
		case OP_ADD: return ai_op_add_float(env, a, b);
		case OP_SUB: return ai_op_sub_float(env, a, b);
		case OP_MUL: return ai_op_mul_float(env, a, b);
		case OP_DIV: return ai_op_div_float(env, a, b);
		case OP_MOD: return ai_op_mod_float(env, a, b);
		default: unreachable();
	}
}

void ai_code_suffix(Parser* par, InoutExpr e1, InExpr e2, a_u32 op, a_u32 line) {
	if (unlikely(e1->_kind == EXPR_NEVER || e2->_kind == EXPR_NEVER)) return;
	switch (op) {
		case OP_ADD:
		case OP_SUB:
		case OP_MUL:
		case OP_DIV:
		case OP_MOD: {
			run {
				a_int i1, i2;
				if (expr_are_ints(e1, &i1, e2, &i2)) {
					e1->_int = l_compute_int(par->_env, e1->_int, e2->_int, op);
					e1->_line = line;
					break;
				}
			}
			run {
				a_float f1, f2;
				if (expr_are_floats(e1, &f1, e2, &f2)) {
					e1->_float = l_compute_float(par->_env, e1->_float, e2->_float, op);
					e1->_line = line;
					break;
				}
			}
			l_anyR(par, e1);
			e1->_kind = EXPR_DST_A;
			if (e2->_kind == EXPR_INT && e2->_int >= BC_MIN_SC && e2->_kind <= BC_MAX_SC) {
				l_drop(par, e1->_reg);
				e1->_label = l_emit(par, bc_make_iabsc(BC_ADDI + op - OP_ADD, DYN, e1->_reg, e2->_int), line);
			}
			else {
				l_anyR(par, e2);
				l_drop(par, e2->_reg);
				l_drop(par, e1->_reg);
				e1->_label = l_emit(par, bc_make_iabc(BC_ADD + op - OP_ADD, DYN, e1->_reg, e2->_reg), line);
			}
			break;
		}
		case OP_SHL:
		case OP_SHR:
		case OP_BIT_AND:
		case OP_BIT_OR:
		case OP_BIT_XOR: {
			run {
				a_int i1, i2;
				if (expr_are_ints(e1, &i1, e2, &i2)) {
					e1->_int = l_compute_int(par->_env, e1->_int, e2->_int, op);
					e1->_line = line;
					break;
				}
			}
			l_anyR(par, e1);
			e1->_kind = EXPR_DST_A;
			if (e2->_kind == EXPR_INT && e2->_int >= BC_MIN_SC && e2->_kind <= BC_MAX_SC) {
				l_drop(par, e1->_reg);
				e1->_label = l_emit(par, bc_make_iabsc(BC_SHLI + op - OP_SHL, DYN, e1->_reg, e2->_int), line);
			}
			else {
				l_anyR(par, e2);
				l_drop(par, e2->_reg);
				l_drop(par, e1->_reg);
				e1->_label = l_emit(par, bc_make_iabc(BC_SHL + op - OP_SHL, DYN, e1->_reg, e2->_reg), line);
			}
			break;
		}
		case OP_AND: {
			switch (e1->_kind) {
				case EXPR_NEVER:
				case EXPR_RESIDUAL_FALSE:
					break;
				case EXPR_TRUE: {
					a_u32 label = NO_LABEL;
					e1->_kind = l_condT(par, e2, &label, line);
					e1->_label = label;
					ai_code_drop(par, e2);
					break;
				}
				case EXPR_TRY_TF: {
					e1->_kind = l_condT(par, e2, &e1->_label, line);
					ai_code_drop(par, e2);
					break;
				}
				default: unreachable();
			}
			break;
		}
		case OP_OR: {
			switch (e1->_kind) {
				case EXPR_NEVER:
				case EXPR_RESIDUAL_TRUE:
					break;
				case EXPR_FALSE: {
					a_u32 label = NO_LABEL;
					e1->_kind = l_condF(par, e2, &label, line);
					e1->_label = label;
					ai_code_drop(par, e2);
					break;
				}
				case EXPR_TRY_FT: {
					e1->_kind = l_condF(par, e2, &e1->_label, line);
					ai_code_drop(par, e2);
					break;
				}
				default: unreachable();
			}
			break;
		}
		default: unreachable();
	}
}

static void l_merge_optR(Parser* par, a_u32 label, a_u32 reg, a_u32 line) {
	a_u32 label2 = l_lazy_jump(par, NO_LABEL, line);
	l_mark_label(par, label, line);
	l_emit_kn(par, reg, 1, line);
	l_mark_label(par, label2, line);
}

void ai_code_merge(Parser* par, InoutExpr e1, InExpr e2, a_u32 label, a_u32 line) {
	switch (e1->_kind) {
		case EXPR_NEVER: {
			assume(label == NO_LABEL);
			expr_copy(e1, e2);
			break;
		}
		case EXPR_TRY_AN: {
			a_u32 label2 = l_try(par, e1, line);
			l_merge_label(par, &e1->_cond._2, label2, line);
			l_dynR(par, e2);
			l_merge_optR(par, e1->_cond._2, e1->_reg, line);
			break;
		}
		case EXPR_TRY_RN: {
			assume(l_is_tmpR(par, e1->_cond._1));
			a_u32 label2 = l_try(par, e1, line);
			l_merge_label(par, &e1->_cond._2, label2, line);
			l_fixR(par, e2, e1->_reg);
			l_merge_optR(par, e1->_cond._2, e1->_reg, line);
			break;
		}
		case EXPR_REG: {
			assume(l_is_tmpR(par, e1->_reg));
			l_fixR(par, e2, e1->_reg);
			l_mark_label(par, label, line);
			break;
		}
		case EXPR_DST_A: {
			a_u32 reg = l_alloc_stack(par, line);
			l_fixR(par, e1, reg);
			l_fixR(par, e2, reg);
			l_mark_label(par, label, line);
			e1->_kind = EXPR_REG;
			e1->_reg = reg;
			break;
		}
		default: unreachable();
	}
}

void ai_code_monad(Parser* par, InoutExpr e, a_u32* plabel, a_u32 op, a_u32 line) {
	switch (op) {
		case OP_OPTION: {
			a_u32 kind = l_condT(par, e, plabel, line);
			if (unlikely(kind == EXPR_RESIDUAL_FALSE)) {
				e->_kind = EXPR_NEVER;
				e->_line = line;
			}
			break;
		}
		case OP_MERGE: {
			a_u32 label = *plabel;
			if (label == NO_LABEL) return;
			switch (e->_kind) {
				case EXPR_NEVER: {
					l_lazy_jump(par, label, line);
					l_mark_label(par, label, line);
					e->_kind = EXPR_NIL;
					break;
				}
				case EXPR_REG: {
					e->_kind = EXPR_TRY_RN;
					e->_cond._1 = e->_reg;
					e->_cond._2 = label;
					break;
				}
				default: {
					l_dynR(par, e);
					fallthrough;
				}
				case EXPR_DST_A: {
					e->_kind = EXPR_TRY_AN;
					e->_cond._1 = e->_label;
					e->_cond._2 = label;
					break;
				}
				case EXPR_TRY_RN:
				case EXPR_TRY_AN: {
					l_merge_label(par, &e->_cond._2, label, line);
					break;
				}
			}
			e->_line = line;
			break;
		}
		case OP_OR_ELSE: {
			switch (e->_kind) {
				case EXPR_REG: {
					if (l_is_tmpR(par, e->_reg)) {
						l_drop(par, e->_reg);
					}
					else {
						l_dynR(par, e);
					}
					break;
				}
				case EXPR_TRY_RN: {
					if (l_is_tmpR(par, e->_cond._1)) {
						l_drop(par, e->_cond._1);
					}
					else {
						l_dynR(par, e);
					}
					break;
				}
				default: {
					l_dynR(par, e);
					break;
				}
			}
			*plabel = l_lazy_jump(par, NO_LABEL, line);
			break;
		}
		default: unreachable();
	}
}

void ai_code_multi(Parser* par, Exprs* es, a_u32 op, a_u32 line) {
	switch (op) {
		case OP_VA_PUSH: {
			if (es->_count == 0) {
				l_topR(par, &es->_last);
				es->_base = es->_last._reg;
				es->_count = 1;
			}
			else {
				l_fixR(par, &es->_last, es->_base + es->_count);
				es->_count += 1;
				assume(par->_scope->_top_reg == es->_base + es->_count, "not successive allocation.");
			}
			break;
		}
		case OP_VA_POP: {
			assume(es->_count > 0);
			ai_code_drop(par, &es->_last);
			es->_last._kind = EXPR_REG;
			es->_last._reg = es->_base + --es->_count;
			es->_last._line = line;
			break;
		}
		case OP_CONCAT: {
			//TODO
			break;
		}
		default: unreachable();
	}
}

static a_bool l_take(Parser* par, InoutExpr e, a_u32 n, a_u32 line) {
	//TODO
	return false;
}

void ai_code_take(Parser* par, Exprs* es, a_u32 n, a_u32 op, a_u32 line) {
	switch (op) {
		case OP_VA_FIT: {
			if (es->_count > n) {
				ai_code_drop(par, &es->_last);
				es->_last._kind = EXPR_REG;
				es->_last._reg = es->_base + n - 1;
				es->_last._line = line;
				l_emit_kn(par, es->_base + n, es->_count - n, line);
			}
			else if (es->_count < n) {
				l_take(par, &es->_last, n - es->_count, line);
			}
			es->_count = n;
			break;
		}
		case OP_VA_FILL: {
			switch (es->_last._kind) {
				case EXPR_UNIT: {
					ai_code_multi(par, es, OP_VA_POP, line);
					break;
				}
				default: {
					a_u32 reg = es->_base + es->_count;
					l_fixR(par, &es->_last, reg);
					es->_last._kind = EXPR_REG;
					es->_last._reg = reg;
					es->_last._line = line;
					break;
				}
			}
			break;
		}
		case OP_BOX_TOP: {
			//TODO
			break;
		}
		default: unreachable();
	}
}

/**
 ** Jump to determined label.
 *@param par the parser.
 *@param label the label jump to.
 *@param line the line number.
 */
void ai_code_gotoD(Parser* par, a_u32 label, a_u32 line) {
	if (l_should_eval(par)) {
		if (par->_fjump) {
			assume(par->_fland); /* When jump is defined, the branch is reachable only if land is also defined. */
			/* Link to previous jump instruction. */
			l_emit_jump_direct(par, par->_head_jump, par->_head_jump_line);
			l_clear_jump(par);
		}
		if (par->_fland) {
			l_redirect_chain(par, par->_head_land, label, line);
			l_clear_land(par);
		}
		if (par->_fpass) {
			l_emit_jump_direct(par, label, line);
			par->_fpass = false;
		}
	}
}

a_u32 ai_code_gotoU(Parser* par, a_u32 label, a_u32 line) {
	if (l_should_eval(par)) {
		if (likely(par->_fpass)) {
			if (par->_head_jump == NO_LABEL || label > par->_head_jump) {
				par->_head_jump_line = line;
			}
			par->_fpass = false;
			par->_fjump = true;
			par->_head_jump = label;
			l_merge_label(par, &par->_head_jump, par->_head_land, line);
			l_clear_land(par);
			return par->_ninsn; /* Return next instruction as pseudo label. */
		}
		else {
			assume(par->_fland);
			l_merge_label(par, &label, par->_head_land, line);
			l_clear_land(par);
			return label;
		}
	}
	return label;
}

void ai_code_label(Parser* par, a_u32 label, a_u32 line) {
	if (label != NO_LABEL) {
		if (label != par->_ninsn) { /* If from label is not pseudo head label, merge with head jump label. */
			par->_fland = true;
			l_merge_label(par, &par->_head_land, label, line);
		}
		else {
			/* Pseudo head jump. */
			assume(par->_fjump && !par->_fpass);
			par->_fpass = true;
			par->_fland = par->_head_jump != NO_LABEL;
			l_merge_label(par, &par->_head_land, par->_head_jump, line);
			l_clear_jump(par);
		}
	}
}

/**
 ** Force flush control flow.
 *@param par the parser.
 */
void ai_code_flush_jump(Parser* par, a_u32 line) {
	if (l_should_eval(par)) {
		l_flush_jump(par, line);
	}
}

a_u32 ai_code_testT(Parser* par, InoutExpr e, a_u32 line) {
	a_u32 label = NO_LABEL;
	l_condT(par, e, &label, line);
	return label;
}

void ai_code_drop(Parser* par, InExpr e) {
	switch (e->_kind) {
		case EXPR_DST_C: {
			bc_swap_c(&par->_insns[e->_label], 1);
			break;
		}
		case EXPR_DST_A: {
			l_topR(par, e);
			fallthrough;
		}
		case EXPR_REG: {
			l_drop(par, e->_reg);
			break;
		}
		default: {
			break;
		}
	}
	e->_kind = EXPR_UNIT;
}

void ai_code_bind(Parser* par, InExpr e1, InExpr e2, a_u32 line) {
	switch (e1->_kind) {
		case EXPR_REG: {
			l_fixR(par, e2, e1->_reg);
			break;
		}
		case EXPR_CAP: {
			l_anyR(par, e2);
			l_emit(par, bc_make_iabc(BC_STC, e1->_reg, e2->_reg, DMB), line);
			l_drop(par, e2->_reg);
			break;
		}
		case EXPR_REF: {
			l_anyR(par, e2);
			l_emit(par, bc_make_iabc(BC_SET, e1->_ref._base, e1->_ref._key, e2->_reg), line);
			l_drop(par, e2->_reg);
			l_drop(par, e1->_ref._key);
			l_drop(par, e1->_ref._base);
			break;
		}
		case EXPR_REFI: {
			l_anyR(par, e2);
			l_emit(par, bc_make_iabc(BC_SETI, e1->_ref._base, e1->_ref._key, e2->_reg), line);
			l_drop(par, e2->_reg);
			l_drop(par, e1->_ref._base);
			break;
		}
		case EXPR_CREFK: {
			a_u32 k = e1->_ref._key;

			l_anyR(par, e2);

			a_u32 reg = l_alloc_stack(par, line);
			l_emit(par, bc_make_iabc(BC_LDC, reg, e1->_ref._base, DMB), line);

			if (likely(k <= BC_MAX_C)) {
				l_emit(par, bc_make_iabc(BC_SETK, e2->_reg, reg, k), line);
			}
			else {
				l_emit(par, bc_make_iabc(BC_SETKX, e2->_reg, reg, DMB), line);
				l_emit_fast(par, bc_make_iax(BC_EX, k), line);
			}

			l_drop(par, e2->_reg);
			l_drop(par, reg);
			break;
		}
		case EXPR_REFK: {
			a_u32 k = e1->_ref._key;
			l_anyR(par, e2);

			if (likely(k <= BC_MAX_C)) {
				l_emit(par, bc_make_iabc(BC_SETK, e2->_reg, e1->_ref._base, k), line);
			}
			else {
				l_emit(par, bc_make_iabc(BC_SETKX, e2->_reg, e1->_ref._base, DMB), line);
				l_emit_fast(par, bc_make_iax(BC_EX, k), line);
			}

			l_drop(par, e2->_reg);
			l_drop(par, e1->_ref._base);
			break;
		}
		default: {
			l_error(par, "cannot assign to the expression.", e1->_line);
		}
	}
}

#define NAMES_GROW_UNIT 64

static void l_push_name(Parser* par, Name name) {
	Names* names = &par->_names;
	if (names->_top == names->_cap) {
		a_usize old_cap = names->_cap;
		a_usize new_cap = old_cap + NAMES_GROW_UNIT;
		names->_stack = l_vgrow(par, names->_stack, old_cap, new_cap);
		names->_cap = new_cap;
	}
	names->_stack[names->_top++] = name;
}

static void l_push_scope(Parser* par, Scope* scope, a_u32 reg) {
	*scope = new(Scope) {
		_bot_reg: reg,
		_top_reg: reg,
		_bot_fur: reg,
		_num_fur: 0,
		_begin_label: par->_ninsn,
		_end_label: NO_LABEL
	};
	par->_scope = scope;
}

static void l_pop_scope(Parser* par, Scope* scope) {
	par->_scope = scope;
}

void ai_code_prologue(Parser* par, FnScope* fnscope) {
	assume(par->_head_land == NO_LABEL && par->_head_jump == NO_LABEL, "residual jump not flushed.");

	*fnscope = new(FnScope) {
		_upper: par->_fnscope,
		_base_subs: cast(GFunMeta**, par->_rq._tail)
	};
	l_push_scope(par, &fnscope->_scope, 0);

	par->_fnscope = fnscope;
	par->_scope_depth += 1;
}

GFunMeta* ai_code_epilogue(Parser* par, a_bool root) {
	FnScope* scope = par->_fnscope;

	if (l_should_eval(par)) {
		l_emit_ret(par, DMB, 0, scope->_scope._end_line);
	}

	l_pop_scope(par, &scope->_scope);

	FnInfo info = {
		_nconst: scope->_consts._len,
		_ninsn: par->_ninsn - scope->_scope._begin_label,
		_nlocal: scope->_locals._len,
		_ncap: scope->_ncap,
		_nstack: scope->_max_reg
	};

	fninfo_hint_size(&info);

	a_usize size = info._size;
	if (root) {
		info._size += sizeof(GFun) + sizeof(Value) * info._ncap;
	}

	GFunMeta* meta = ai_fun_xalloc(par->_env, &info);
	if (meta == null) {
		l_nomem_error(par);
	}

	memcpy(meta->_consts, scope->_consts._dat, sizeof(Value) * info._nconst);
	memcpy(meta->_insns + scope->_scope._begin_label, par->_insns, sizeof(a_insn) * info._ninsn);
	run {
		GFunMeta** dst = meta->_subs;
		GFunMeta** end = cast(GFunMeta**, par->_rq._tail);
		for (GFunMeta** src = scope->_base_subs; src != end; src = cast(GFunMeta**, &(*src)->_gnext)) {
			*dst = *src;
		}
	}

	if (root) {
		GFun* fun = ptr_of(GFun, addr_of(meta) + size);
		fun->_meta = downcast(GMeta, meta);
		fun->_len = info._ncap;
		meta->_cache = fun;
	}

	l_close_scope(par, scope);

	par->_fnscope = scope->_upper;
	par->_ninsn = scope->_scope._begin_label;
	par->_rq._tail = cast(a_hobj*, scope->_base_subs);

	rq_push(&par->_rq, meta);

	return meta;
}

static void parser_splash(Global* g, void* ctx) {
	Parser* par = ctx;
	run {
		Strs* strs = &par->_lex._strs;
		for (a_u32 i = 0; i <= strs->_hmask; ++i) {
			StrNode* node = &strs->_table[i];
			if (node->_str != null) {
				ai_gc_trace_mark(g, node->_str);
			}
		}
	}
}

static void parser_close(Parser* par) {
	l_vdel(par, par->_insns, par->_cinsn);
	l_vdel(par, par->_names._stack, par->_names._cap);

	ai_env_gsplash_clear(par->_env);
}

static void l_del_meta(a_henv env, GFunMeta* meta) {
	for (a_u32 i = 0; i < meta->_nsub; ++i) {
		l_del_meta(env, meta->_subs[i]);
	}
	ai_fun_meta_destruct(G(env), meta);
}

void ai_code_open(Parser* par) {
	ai_env_gsplash(par->_env, parser_splash, &par);

	/* Add '_ENV' name. */
	l_push_name(par, new(Name) {
		_scope: SCOPE_DEPTH_ENV,
		_index: 0,
		_kind: NAME_LOCAL,
		_name: ai_env_strx(G(par->_env), STRX_KW__ENV)
	});

	/* Attach root control flow. */
	par->_fpass = true;
	par->_head_jump = NO_LABEL;
	par->_head_land = NO_LABEL;
}

static void l_register_meta(a_henv env, GFunMeta* meta) {
	assume(meta->_gnext == null, "duplicate root function.");
	ai_gc_register_object(env, meta);
	for (a_u32 i = 0; i < meta->_nsub; ++i) {
		l_register_meta(env, meta->_subs[i]);
	}
}

GFun* ai_code_build(Parser* par) {
	GFunMeta* meta = downcast(GFunMeta, par->_rq._head); /* Get root function metadata. */
	l_register_meta(par->_env, meta);
	parser_close(par);
	GFun* fun = meta->_cache;
	g_set_white(G(par->_env), upcast(fun));
	return fun;
}

void ai_code_close(Parser* par) {
	/* Close unclosed scopes. */
	FnScope* scope = par->_fnscope;
	do l_close_scope(par, scope);
	while ((scope = scope->_upper) != null);
	/* Destroy queued metadata. */
	rq_for(obj, &par->_rq) { l_del_meta(par->_env, downcast(GFunMeta, obj)); }
	/* Close parser. */
	parser_close(par);
}

static a_none l_error_unit_expr(Parser* par, a_u32 line) {
	l_error(par, "attempt to use result of an unit expression.", line);
}

static void l_dynR(Parser* par, InoutExpr e) {
	switch (e->_kind) {
		case EXPR_UNIT: {
			l_error_unit_expr(par, e->_line);
		}
		case EXPR_FALSE:
		case EXPR_TRUE: {
			e->_kind = EXPR_DST_A;
			e->_label = l_emit_kz(par, DYN, (e->_kind & 1) != 0, e->_line);
			break;
		}
		case EXPR_INT: {
			e->_kind = EXPR_DST_A;
			if (e->_int >= BC_MIN_SBX && e->_int <= BC_MAX_SBX) {
				e->_label = l_emit(par, bc_make_iasbx(BC_KI, DYN, e->_int), e->_line);
			}
			else {
				a_u32 k = l_const_index(par, v_of_int(e->_int));
				e->_label = l_emit(par, bc_make_iabx(BC_K, DYN, k), e->_line);
			}
			break;
		}
		case EXPR_FLOAT: {
			e->_kind = EXPR_DST_A;
			a_u32 k = l_const_index(par, v_of_float(e->_float));
			e->_label = l_emit(par, bc_make_iabx(BC_K, DYN, k), e->_line);
			break;
		}
		case EXPR_STR: {
			e->_kind = EXPR_DST_A;
			a_u32 k = l_const_index(par, v_of_ref(e->_str));
			e->_label = l_emit(par, bc_make_iabx(BC_K, DYN, k), e->_line);
			break;
		}
		case EXPR_REG: {
			e->_kind = EXPR_DST_A;
			e->_label = l_emit_mov(par, DYN, e->_reg, e->_line);
			break;
		}
		case EXPR_CAP: {
			e->_kind = EXPR_DST_A;
			e->_label = l_emit(par, bc_make_iabc(BC_LDC, DYN, e->_reg, DMB), e->_line);
			break;
		}
		case EXPR_TRY_FT:  {
			l_instantiate_branch(par, e, true);
			break;
		}
		case EXPR_TRY_TF:  {
			l_instantiate_branch(par, e, false);
			break;
		}
		case EXPR_TRY_RN: {
			e->_kind = EXPR_TRY_AN;
			e->_cond._1 = l_emit_mov(par, DYN, e->_cond._1, e->_line);
			break;
		}
		case EXPR_REF: {
			l_drop(par, e->_ref._key);
			l_drop(par, e->_ref._base);
			e->_kind = EXPR_DST_A;
			e->_label = l_emit(par, bc_make_iabc(BC_GET, DYN, e->_ref._base, e->_ref._key), e->_line);
			break;
		}
		case EXPR_REFI: {
			l_drop(par, e->_ref._base);
			e->_kind = EXPR_DST_A;
			e->_label = l_emit(par, bc_make_iabc(BC_GETI, DYN, e->_ref._base, e->_ref._key), e->_line);
			break;
		}
		case EXPR_REFK: {
			a_u32 k = e->_ref._key;
			l_drop(par, e->_ref._base);
			e->_kind = EXPR_DST_A;
			if (likely(k <= BC_MAX_C)) {
				e->_label = l_emit(par, bc_make_iabc(BC_GETK, DYN, e->_ref._base, k), e->_line);
			}
			else {
				e->_label = l_emit(par, bc_make_iabc(BC_GETKX, DYN, e->_ref._base, 0), e->_line);
				l_emit_fast(par, bc_make_iax(BC_EX, k), e->_line);
			}
			break;
		}
		case EXPR_CREFK: {
			a_u32 k = e->_ref._key;
			e->_kind = EXPR_DST_A;
			if (likely(k < BC_MAX_C)) {
				e->_label = l_emit(par, bc_make_iabc(BC_CGETK, DYN, e->_ref._base, k), e->_line);
			}
			else {
				e->_label = l_emit(par, bc_make_iabc(BC_CGETKX, DYN, e->_ref._base, 0), e->_line);
				l_emit_fast(par, bc_make_iax(BC_EX, k), e->_line);
			}
			break;
		}
		case EXPR_UNBOX: {
			l_drop(par, e->_reg);
			e->_kind = EXPR_DST_A;
			e->_label = l_emit(par, bc_make_iabc(BC_UNBOX, DYN, e->_reg, 1), e->_line);
			break;
		}
		default: {
			break;
		}
	}
}

static void l_bindR(Parser* par, InoutExpr e, a_u32 reg) {
	switch (e->_kind) {
		case EXPR_DST_A: {
			bc_swap_a(&par->_insns[e->_label], reg);
			break;
		}
		case EXPR_TRY_RN: {
			a_u32 reg2 = e->_cond._1;
			if (reg == reg2) {
				l_merge_optR(par, e->_cond._2, reg, e->_line);
				break;
			}
			l_drop(par, reg2); /* Should drop this register? */
			e->_cond._1 = l_emit_mov(par, DYN, reg2, e->_line);
			fallthrough;
		}
		case EXPR_TRY_AN: {
			bc_swap_a(&par->_insns[e->_cond._1], reg);
			l_merge_optR(par, e->_cond._2, reg, e->_line);
			break;
		}
		default: unreachable();
	}
	e->_kind = EXPR_REG;
	e->_reg = reg;
}

/**
 ** Fix the result of expression to specific register.
 ** The expression is unavailable after fix.
 *@param par the parser.
 *@param e the computed expression.
 *@param reg the register index to bind result.
 */
static void l_fixR(Parser* par, InExpr e, a_u32 reg) {
	switch (e->_kind) {
		case EXPR_TRY_RN: {
			if (e->_cond._1 != reg) {
				l_drop(par, e->_cond._1);
				l_emit_mov(par, reg, e->_cond._1, e->_line);
			}
			l_merge_optR(par, e->_cond._2, reg, e->_line);
			break;
		}
		case EXPR_REG: {
			if (e->_reg != reg) {
				l_drop(par, e->_reg);
				l_emit_mov(par, reg, e->_cond._1, e->_line);
			}
			break;
		}
		default: {
			l_dynR(par, e);
			l_bindR(par, e, reg);
			break;
		}
	}
}

static void l_topR(Parser* par, InoutExpr e) {
	switch (e->_kind) {
		case EXPR_NEVER: {
			break;
		}
		case EXPR_UNIT: {
			l_error_unit_expr(par, e->_line);
		}
		case EXPR_NIL: {
			a_u32 reg = l_alloc_stack(par, e->_line);
			l_emit_kn(par, reg, 1, e->_line);
			break;
		}
		case EXPR_REG: {
			a_u32 reg1 = e->_reg;
			a_u32 reg2;

			l_drop(par, reg1);
			reg2 = l_alloc_stack(par, e->_line);
			if (reg1 != reg2) {
				l_emit_mov(par, reg2, reg1, e->_line);
				e->_reg = reg2;
			}
			break;
		}
		default: {
			l_dynR(par, e);
			fallthrough;
		}
		case EXPR_DST_A: {
			l_bindR(par, e, l_alloc_stack(par, e->_line));
			break;
		}
	}
}

static void l_anyR(Parser* par, InoutExpr e) {
	switch (e->_kind) {
		case EXPR_NEVER:
		case EXPR_REG: {
			break;
		}
		case EXPR_UNIT: {
			l_error_unit_expr(par, e->_line);
		}
		case EXPR_TRY_RN: {
			a_u32 reg = e->_cond._1;
			l_merge_optR(par, e->_cond._2, reg, e->_line);
			e->_kind = EXPR_REG;
			e->_reg = reg;
			break;
		}
		default: {
			l_dynR(par, e);
			fallthrough;
		}
		case EXPR_DST_A: {
			l_bindR(par, e, l_alloc_stack(par, e->_line));
			break;
		}
	}
}

static void l_anyRK(Parser* par, InoutExpr e) {
	switch (e->_kind) {
		case EXPR_NEVER:
		case EXPR_UNIT:
		case EXPR_REG:
		case EXPR_NIL:
		case EXPR_FALSE:
		case EXPR_TRUE:
		case EXPR_INT:
		case EXPR_FLOAT:
		case EXPR_STR: {
			break;
		}
		default: {
			l_anyR(par, e);
			break;
		}
	}
}

static a_u32 l_condT(Parser* par, InoutExpr e, a_u32* plabel, a_u32 line) {
	switch (e->_kind) {
		case EXPR_NEVER:
		case EXPR_TRUE:
		case EXPR_INT:
		case EXPR_FLOAT:
		case EXPR_STR: {
			return EXPR_TRUE;
		}
		case EXPR_NIL: {
			e->_kind = EXPR_FALSE;
			fallthrough;
		}
		case EXPR_FALSE: {
			*plabel = l_lazy_jump(par, *plabel, line);
			return EXPR_RESIDUAL_FALSE;
		}
		case EXPR_TRY_TF: {
			return EXPR_TRY_TF;
		}
		case EXPR_TRY_FT: {
			l_negate_branch(par, e->_label, plabel, line);
			return EXPR_TRY_TF;
		}
		default: {
			l_anyR(par, e);
			assume(e->_kind == EXPR_REG);
			fallthrough;
		}
		case EXPR_REG: {
			*plabel = l_emit_test(par, BC_BZ, e->_reg, *plabel, line);
			return EXPR_TRY_TF;
		}
	}
}

static a_u32 l_condF(Parser* par, InoutExpr e, a_u32* plabel, a_u32 line) {
	switch (e->_kind) {
		case EXPR_NEVER:
		case EXPR_NIL:
		case EXPR_FALSE: {
			return EXPR_FALSE;
		}
		case EXPR_INT:
		case EXPR_FLOAT:
		case EXPR_STR:
		case EXPR_TRUE: {
			*plabel = l_lazy_jump(par, *plabel, line);
			return EXPR_RESIDUAL_TRUE;
		}
		case EXPR_TRY_FT: {
			return EXPR_TRY_FT;
		}
		case EXPR_TRY_TF: {
			l_negate_branch(par, e->_label, plabel, line);
			return EXPR_TRY_FT;
		}
		default: {
			l_anyR(par, e);
			fallthrough;
		}
		case EXPR_REG: {
			*plabel = l_emit_test(par, BC_BNZ, e->_reg, *plabel, line);
			return EXPR_TRY_FT;
		}
	}
}

a_u32 l_try(Parser* par, InoutExpr e, a_u32 line) {
	switch (e->_kind) {
		case EXPR_NIL: {
			a_u32 label = l_lazy_jump(par, NO_LABEL, line);
			e->_kind = EXPR_NIL;
			return label;
		}
		case EXPR_TRY_AN: {
			a_u32 label = e->_cond._2;
			e->_kind = EXPR_DST_A;
			e->_label = e->_cond._1;
			return label;
		}
		case EXPR_TRY_RN: {
			a_u32 label = e->_cond._2;
			e->_kind = EXPR_REG;
			e->_reg = e->_cond._1;
			return label;
		}
		default: {
			return NO_LABEL;
		}
	}
}