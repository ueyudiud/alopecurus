/**
 *@file acode.c
 */

#define acode_c_

#include <string.h>

#include "abc.h"
#include "afun.h"
#include "aop.h"
#include "agc.h"
#include "afmt.h"
#include "astrx.h"

#include "acode.h"

enum {
	/* Mark operand which is not determined. */
	DYN = 0,
	/* Mark unused operand. */
	DMB = 0
};

/* Variable length of arguments. */
#define VARARG cast(a_u32, -1)

static void l_nomem_error(Parser* par) {
	ai_code_close(par);
	ai_mem_nomem(par->_env);
}

#define l_check_alloc(e) if (unlikely((e) != ALO_SOK)) l_nomem_error(par)

#define l_vgrow(par,vec,size_old,size_new) ({ \
    typeof(vec) _v = ai_mem_vxgrow((par)->_env, vec, size_old, size_new); \
	if (unlikely(_v == null)) { l_nomem_error(par); }                        \
	_v;                                          \
})

#define l_bput(par,b,v,u)  ({ \
    typeof(b)* _buf = &(b); \
    if (unlikely(_buf->_len == _buf->_cap)) { \
        l_check_alloc(ai_buf_resize_((par)->_env, _buf, _buf->_cap + u)); \
    } \
	a_usize _index = _buf->_len++; \
    _buf->_arr[_index] = (v); \
	_index; \
})

#define l_bdel(par,buf) ai_buf_close(G((par)->_env), &(buf))
#define l_vdel(par,vec,len) ai_mem_vdel(G((par)->_env), vec, len)

inline void expr_copy(OutExpr dst, InExpr src) {
	*dst = *src;
}

static LocalInfo* l_local_at(Parser* par, a_u32 index) {
	return &par->_locals._arr[par->_fnscope->_begin_local + index];
}

/**
 ** Load the index for constant.
 *@return the index of constant in constant pool.
 */
static a_u32 l_const_index(Parser* par, Value val) {
	ValBuf* consts = &par->_fnscope->_consts;
	for (a_u32 i = 0; i < consts->_len; ++i) {
		/*
		 * Since all literals which have the same format provided by compiler should
		 * have same binary data, use identity equality for comparison.
		 */
		if (v_id_eq(&consts->_arr[i], &val)) {
			return i;
		}
	}

	if (unlikely(consts->_len == BC_MAX_BX + 1)) {
		ai_par_report(par, false, par_err_f_arg(par, "too many constants."));
	}

	return l_bput(par, *consts, val, 256);
}

static void l_close_fn_scope(Parser* par, FnScope* scope) {
	l_bdel(par, scope->_consts);
}

/**
 ** Get last instruction if it is in sequential control flow.
 *@return the last instruction.
 */
static a_u32 l_last_insn(Parser* par) {
	if (par->_scope->_begin_label < par->_head_label) {
		a_insn insn = par->_code[par->_head_label - 1];
		assume(insn != 0); /* Instruction should not be 0. */
		return insn;
	}
	return 0;
}

#define LINE_GROW_UNIT 1024

static void l_emit_line(Parser* par, a_u32 line) {
	if (par->_head_line != line) {
		LineInfo info = new(LineInfo) {UINT32_MAX, line};
		a_u32 index = l_bput(par, par->_lines, info, LINE_GROW_UNIT);
		if (index > 0) {
			par->_lines._arr[index - 1]._end = par->_head_label;
		}
		par->_head_line = line;
	}
}

static a_u32 l_emit_direct(Parser* par, a_insn insn, a_u32 line) {
	l_emit_line(par, line);
	return l_bput(par, par->_insns, insn, 4096);
}

static a_bool l_should_eval(Parser* par) {
	return likely(par->_fpass || par->_fland);
}

static a_i32 l_make_jump_diff(Parser* par, a_u32 from, a_u32 to, a_u32 line) {
	a_i32 diff = cast(a_i32, to - from - 1);
	if (unlikely(diff < BC_MIN_SAX || diff > BC_MAX_SAX)) {
		ai_par_error(par, "instruction jump out of bound.", line);
	}
	return diff;
}

static a_u32 l_emit_jump_direct(Parser* par, a_u32 label, a_u32 line) {
	a_i32 diff = label != NO_LABEL ? l_make_jump_diff(par, par->_head_label, label, line) : -1;
	return l_emit_direct(par, bc_make_isax(BC_J, diff), line);
}

static a_u32 l_next_jump(Parser* par, a_u32 label) {
	assume(label <= par->_head_label, "not valid label.");
	if (label == par->_head_label)
		return par->_head_land;

	a_insn i = par->_code[label];
	assume(bc_load_op(i) == BC_J);

	a_i32 disp = bc_load_sax(i);
	return disp != -1 ? cast(a_u32, cast(a_i32, label + 1) + disp) : NO_LABEL;
}

static void l_reloc(Parser* par, a_u32 from, a_u32 to, a_u32 line) {
	a_insn* pi = &par->_code[from];

	assume(bc_load_op(*pi) == BC_J);

	a_i32 diff = l_make_jump_diff(par, from, to, line);
	bc_swap_sax(pi, diff);
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
		a_u32 next = l_next_jump(par, from);
		l_reloc(par, from, to, line);
		if (next == NO_LABEL) break;
		from = next;
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

static void l_flush_jump(Parser* par, unused a_u32 line) {
	if (par->_fjump) {
		assume(par->_fland); /* When jump is defined, the branch is reachable only if land is also defined. */
		/* Link to previous jump instruction. */
		l_emit_jump_direct(par, par->_head_jump, par->_head_jump_line);
		l_clear_jump(par);
	}
}

static void l_flush_land(Parser* par, a_u32 line) {
	if (par->_fland) {
		l_redirect_chain(par, par->_head_land, par->_head_label, line);
		l_clear_land(par);
	}
}

/**
 ** Emit an instruction to leave current function.
 */
static void l_emit_leave(Parser* par, a_insn i, a_u32 line) {
	if (l_should_eval(par)) {
		l_flush_jump(par, line);
		if (par->_fland) {
			a_u32 label = par->_head_land;
			loop {
				a_u32 next = l_next_jump(par, label);
				par->_code[label] = i;
				if (next == NO_LABEL) break;
				label = next;
			}
			l_clear_land(par);
		}
		if (par->_fpass) {
			l_emit_direct(par, i, line);
		}
		par->_fpass = false;
	}
}

static a_u32 l_emit(Parser* par, a_insn i, a_u32 line) {
	if (l_should_eval(par)) {
		l_flush_jump(par, line);
		l_flush_land(par, line);
		par->_fpass = true;
		return l_emit_direct(par, i, line);
	}
	return NO_LABEL;
}

static a_u32 l_emit_mov(Parser* par, a_u32 dst, a_u32 src, a_u32 line) {
	return l_emit(par, bc_make_iab(BC_MOV, dst, src), line);
}

static a_u32 l_emit_kz(Parser* par, a_u32 dst, a_bool val, a_u32 line) {
	assume(val == false || val == true);
	return l_emit(par, bc_make_iab(BC_KF | val, dst, DMB), line);
}

static a_u32 l_emit_k(Parser* par, a_u32 dst, Value val, a_u32 line) {
	a_u32 index = l_const_index(par, val);
	return l_emit(par, bc_make_iabx(BC_K, dst, index), line);
}

static a_u32 l_emit_tnew(Parser* par, a_u32 dst, a_u32 base, a_u32 len, a_u32 line) {
	return l_emit(par, bc_make_iabc(BC_TNEW, dst, base, len + 1), line);
}

static void l_emit_ret(Parser* par, a_u32 base, a_u32 len, a_u32 line) {
	l_emit_leave(par, bc_make_iabc(BC_RET, DMB, base, len + 1), line);
}

static a_u32 l_emit_kn(Parser* par, a_u32 dst, a_u32 len, a_u32 line) {
	assume(len > 0);
	if (par->_fpass && !par->_fland) {
		a_insn insn = l_last_insn(par);
		if (bc_load_op(insn) == BC_KN) {
			a_u32 a = bc_load_a(insn);
			a_u32 c = bc_load_c(insn);
			a_u32 dst1 = dst;
			a_u32 dst2 = dst + len;
			a_u32 src1 = a;
			a_u32 src2 = a + c;
			if (!(dst2 < src1 || src2 < dst1)) {
				a_insn* pinsn = &par->_code[par->_head_label - 1];
				a = min(src1, dst1);
				c = max(src2, dst2) - a;
				bc_swap_a(pinsn, a);
				bc_swap_c(pinsn, c);
				return par->_head_label - 1;
			}
		}
	}
	return l_emit(par, bc_make_iabc(BC_KN, dst, DMB, len), line);
}

static a_u32 l_emit_branch(Parser* par, a_insn i, a_u32 label, a_u32 line) {
	a_u32 label2 = l_emit(par, i, line);
	a_i32 diff = label != NO_LABEL ? l_make_jump_diff(par, par->_head_label, label, line) : -1;
	return label2 != NO_LABEL ? l_emit_direct(par, bc_make_isax(BC_J, diff), line) : NO_LABEL;
}

static a_u32 l_emit_test(Parser* par, a_u32 op, a_u32 reg, a_u32 label, a_u32 line) {
	return l_emit_branch(par, bc_make_iabc(op, DMB, reg, DMB), label, line);
}

static void l_merge_label(Parser* par, a_u32* plabel, a_u32 label2, a_u32 line) {
	if (label2 == NO_LABEL)
		return;

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
			a_u32 label3 = l_next_jump(par, label1);
			if (label3 == NO_LABEL) {
				l_reloc(par, label1, label2, line);
				return;
			}
			else if (label3 > label2) {
				label1 = label3;
			}
			else if (label3 < label2) {
				l_reloc(par, label1, label2, line);
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

inline void expr_never(OutExpr e, a_u32 line) {
	e->_kind = EXPR_NEVER;
	e->_line = line;
}

inline void expr_var(OutExpr e, a_u32 reg, a_u32 line) {
	*e = new(Expr) {
		._kind = EXPR_VAR,
		._line = line,
		._reg = reg
	};
}

inline void expr_tmp(OutExpr e, a_u32 reg, a_u32 line) {
	*e = new(Expr) {
		._kind = EXPR_TMP,
		._line = line,
		._reg = reg
	};
}

inline void expr_dyn(OutExpr e, a_u32 label) {
	e->_kind = EXPR_DST_A;
	e->_label = label;
}

void ai_code_never(unused Parser* par, OutExpr e, a_u32 line) {
	expr_never(e, line);
}

void ai_code_constK(Parser* par, OutExpr e, a_u32 val, a_u32 line) {
	assume(val == EXPR_NIL || val == EXPR_FALSE || val == EXPR_TRUE || val == EXPR_UNIT);
	if (l_should_eval(par)) {
		e->_kind = val;
		e->_line = line;
	}
	else {
		expr_never(e, line);
	}
}

void ai_code_constI(Parser* par, OutExpr e, a_int val, a_u32 line) {
	if (l_should_eval(par)) {
		e->_kind = EXPR_INT;
		e->_int = val;
		e->_line = line;
	}
	else {
		expr_never(e, line);
	}
}

void ai_code_constF(Parser* par, OutExpr e, a_float val, a_u32 line) {
	if (l_should_eval(par)) {
		e->_kind = EXPR_FLOAT;
		e->_float = val;
		e->_line = line;
	}
	else {
		expr_never(e, line);
	}
}

void ai_code_constS(Parser* par, OutExpr e, GStr* val, a_u32 line) {
	if (l_should_eval(par)) {
		e->_kind = EXPR_STR;
		e->_str = val;
		e->_line = line;
	}
	else {
		expr_never(e, line);
	}
}

static a_u32 l_succ_alloc_stack(Parser* par, a_u32 num, a_u32 line) {
	Scope* scope = par->_scope;
	assume(scope->_num_fur == 0, "cannot allocate register while fragmented section exists.");
	assume(!par->_fvarg, "cannot allocate register when stack is not rebalanced.");
	a_u32 reg = scope->_top_reg;
	scope->_top_reg += num;
	if (scope->_top_reg > par->_fnscope->_max_reg) {
		par->_fnscope->_max_reg = scope->_top_reg;
		if (reg >= BC_MAX_A) {
			ai_par_error(par, "too many register used.", line);
		}
	}
	return reg;
}

static a_u32 l_alloc_stack(Parser* par, a_u32 line) {
	return l_succ_alloc_stack(par, 1, line);
}

static a_bool l_is_tmpR(Parser* par, a_u32 reg) {
	return reg >= par->_scope->_top_ntr;
}

static a_bool l_is_topR(Parser* par, a_u32 reg) {
	return reg == cast(a_u32, par->_scope->_top_reg) - 1;
}

static void l_check_free_used_stack(Scope* scope) {
	/* Remove critical section if all registers in the section are fully freed. */
	if (unlikely(scope->_num_fur > 0) && scope->_bot_fur + scope->_num_fur == scope->_top_reg) {
		scope->_top_reg = scope->_bot_fur;
		scope->_num_fur = 0;
	}
}

/**
 ** Free register from temporary value stack.
 ** The register can be freed with the different order with
 ** reversed order of allocation.
 *@param par the parser.
 *@param reg the temporary register.
 */
static void l_free_stack(Parser* par, a_u32 reg) {
	Scope* scope = par->_scope;
	assume(reg < scope->_top_reg && l_is_tmpR(par, reg));
	/* The registers are likely freed with the reversed order of allocation. */
	if (likely(reg + 1 == scope->_top_reg)) {
		scope->_top_reg = reg;
	}
	/* Or, mark register position to top of stack into critical section. */
	else {
		if (scope->_num_fur == 0 || reg < scope->_bot_fur) {
			scope->_bot_fur = reg;
		}
		scope->_num_fur += 1;
	}
	l_check_free_used_stack(scope);
}

/**
 ** Free a set of registers from temporary value stack, the registers is allocated successively.
 ** The registers must be freed with the order of allocation.
 *@param par the parser.
 *@param reg the first temporary register to free.
 */
static void l_succ_free_stack(Parser* par, a_u32 reg, a_u32 num) {
	Scope* scope = par->_scope;
	assume(l_is_tmpR(par, reg));
	assume(reg + num == scope->_top_reg, "not in successive allocation.");

	scope->_top_reg = reg;
	l_check_free_used_stack(scope);
}

/**
 ** Drop ownership for register if it is a temporary register.
 *@param par the parser.
 *@param e the expression.
 */
static void l_drop(Parser* par, Expr* e) {
	if (e->_kind == EXPR_TMP) {
		l_free_stack(par, e->_reg);
	}
}

/**
 ** Drop ownership for register list.
 *@param par the parser.
 *@param ep the register list.
 */
static void l_va_drop(Parser* par, ExprPack* ep) {
	if (ep->_len != 0) {
		a_u32 len = ep->_len;
		if (unlikely(len == VARARG)) {
			assume(par->_fvarg);
			len = par->_scope->_top_reg - ep->_base;
			par->_fvarg = false;
		}
		l_succ_free_stack(par, ep->_base, len);
	}
}

static void l_dynR(Parser* par, InoutExpr e);
static void l_topR(Parser* par, InoutExpr e);
static void l_tmpR(Parser* par, InoutExpr e);
static void l_anyR(Parser* par, InoutExpr e);
static void l_anyRK(Parser* par, InoutExpr e);
static void l_fixR(Parser* par, InExpr e, a_u32 reg);
static a_u32 l_condT(Parser* par, InoutExpr e, a_u32* plabel, a_u32 line);
static a_u32 l_condF(Parser* par, InoutExpr e, a_u32* plabel, a_u32 line);
static a_u32 l_try(Parser* par, InoutExpr e, a_u32 line);

static a_u32 l_reg_bit(InExpr e) {
	assume(e->_kind == EXPR_VAR || e->_kind == EXPR_TMP || e->_kind == EXPR_NEVER);
	return e->_kind & 0x1;
}

static a_u32 l_capture(Parser* par, FnScope* scope, Sym* sym, a_u32 depth) {
	/* Find in captured values. */
	for (a_u32 i = scope->_begin_cap; i < par->_captures._len; ++i) {
		CompCapInfo* info = &par->_captures._arr[i];
		if (info->_iname == sym->_index) {
			/* Already captured. */
			return i;
		}
	}
	/* Not found, create a new capture value. */
	CompCapInfo info = {
		._iname = sym->_index,
		._scope = sym->_scope,
		._index = sym->_scope < depth - 1 ? l_capture(par, scope->_fn_up, sym, depth - 1) : sym->_index,
		._name = sym->_name
	};
	return l_bput(par, par->_captures, info, 32);
}

/* Get environment name. */
#define l_env_name(par) (&(par)->_syms._stack[0])

static void l_load_name(Parser* par, OutExpr e, Sym* sym, a_u32 line) {
	switch (sym->_kind) {
		case SYM_LOCAL: {
			if (par->_scope_depth == sym->_scope) {
				expr_var(e, l_local_at(par, sym->_index)->_reg, line);
			}
			else {
				e->_kind = EXPR_CAP;
				e->_line = line;
				e->_reg = l_capture(par, par->_fnscope, sym, par->_scope_depth);
			}
			break;
		}
		default: unreachable();
	}
}

void ai_code_lookupU(Parser* par, OutExpr e, GStr* name, a_u32 line) {
	Syms* syms = &par->_syms;
	for (a_isize i = cast(a_isize, syms->_top - 1); i >= 0; --i) {
		Sym* sym = &syms->_stack[i];
		if (sym->_name == name) {
			l_load_name(par, e, sym, line);
			return;
		}
	}

	e->_kind = EXPR_REFCK;
	e->_ref._base = l_capture(par, par->_fnscope, l_env_name(par), par->_scope_depth);
	e->_ref._key = l_const_index(par, v_of_ref(name));
	e->_line = line;
}

void ai_code_lookupC(Parser* par, InoutExpr e, GStr* name, a_u32 line) {
	switch (e->_kind) {
		case EXPR_CAP: {
			e->_kind = EXPR_REFCK;
			e->_ref._base = e->_reg;
			break;
		}
		default: {
			l_anyR(par, e);
			e->_kind = EXPR_REFTK | l_reg_bit(e);
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
				ev->_kind = EXPR_REFTI | l_reg_bit(ev);
				ev->_ref._key = cast(a_u32, val);
			}
			else {
				ev->_kind = EXPR_REFTT | l_reg_bit(ev);
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
				ev->_kind = EXPR_REFTT | l_reg_bit(ek) << 1 | l_reg_bit(ev);
				ev->_ref._base = ev->_reg;
				ev->_ref._key = ek->_reg;
				ev->_line = line;
			}
			break;
		}
	}
}

static void l_negate_branch(Parser* par, a_u32 label, a_u32* plabel, a_u32 line) {
	if (label + 1 == par->_head_label) {
		/* Try to swap duality opcodes for */
		a_insn* pi = &par->_code[label - 1];

		a_u32 op = bc_load_op(*pi);
		if (bc_is_branch_op(op)) {
			bc_swap_op(pi, op ^ 1);
			l_merge_label(par, plabel, label - 1, line);
			return;
		}
	}

	*plabel = l_lazy_jump(par, *plabel, line);
	l_mark_label(par, label, line);
}

static void l_instantiate_branch(Parser* par, InoutExpr e, a_bool flip) {
	assume(par->_head_label == e->_label + 1);
	a_u32 label = e->_label - 1;
	a_insn* pi = &par->_code[label];

	a_u32 op = bc_load_op(*pi);
	assume(bc_is_branch_op(op));
	bc_swap_op(pi, (op + 2) ^ flip);

	par->_head_label = label + 1;
	expr_dyn(e, label);
}

static ExprPack l_to_pack(Parser* par, InExpr e) {
	switch (e->_kind) {
		case EXPR_UNIT: {
			return new(ExprPack) { DMB, 0 };
		}
		case EXPR_PACK: {
			return e->_pack;
		}
		case EXPR_DST_C: {
			a_insn* pi = &par->_code[e->_label];
			a_u32 len = par->_fvarg ? VARARG : 1;
			bc_swap_c(pi, len + 1);
			par->_fvarg = false;
			return new(ExprPack) { bc_load_a(*pi), len };
		}
		case EXPR_DST_AC: {
			a_insn* pi = &par->_code[e->_label];
			a_u32 len = par->_fvarg ? VARARG : 1;
			bc_swap_c(pi, len + 1);
			par->_fvarg = false;
			return new(ExprPack) { par->_scope->_top_reg, len };
		}
		default: {
			l_anyR(par, e);
			return new(ExprPack) { e->_reg, 1 };
		}
	}
}

void ai_code_unary(Parser* par, InoutExpr e, a_u32 op, a_u32 line) {
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
					l_drop(par, e);
					expr_dyn(e, l_emit(par, bc_make_iab(BC_NEG, DYN, e->_reg), line));
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
					l_drop(par, e);
					expr_dyn(e, l_emit(par, bc_make_iab(BC_NEG, DYN, e->_reg), line));
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
				case EXPR_TRY_TRUE:
				case EXPR_TRY_FALSE: {
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
					l_drop(par, e);

					a_u32 label = l_emit_test(par, BC_BNZ, e->_reg, NO_LABEL, line);

					e->_kind = EXPR_TRY_TRUE;
					e->_label = label;
					e->_line = line;
					break;
				}
			}
			break;
		}
		case OP_UNBOX: {
			l_anyR(par, e);
			l_drop(par, e);

			e->_kind = EXPR_DST_AC;
			e->_label = l_emit(par, bc_make_iabc(BC_UNBOX, DYN, e->_reg, DYN), line);
			e->_line = line;
			break;
		}
		case OP_LEN: {
			l_anyR(par, e);
			l_drop(par, e);

			e->_kind = EXPR_DST_A;
			e->_label = l_emit(par, bc_make_iab(BC_LEN, DYN, e->_reg), line);
			e->_line = line;
			break;
		}
		case OP_TNEW: {
			ExprPack pack = l_to_pack(par, e);
			expr_dyn(e, l_emit_tnew(par, DYN, pack._base, pack._len, line));
			l_va_drop(par, &pack);
			break;
		}
		case OP_RETURN: {
			ExprPack pack = l_to_pack(par, e);
			l_emit_ret(par, pack._base, pack._len, line);
			l_va_drop(par, &pack);
			break;
		}
		case OP_CALL: { /* Only handle for empty argument call. */
			switch (e->_kind) {
				case EXPR_NEVER: {
					break;
				}
				case EXPR_PACK: {
					ExprPack* pack = &e->_pack;
					e->_kind = EXPR_DST_C;
					e->_label = l_emit(par, bc_make_iabc(BC_CALL, pack->_base, pack->_len, DYN), line);
					e->_line = line;
					break;
				}
				default: {
					l_topR(par, e);
					e->_kind = EXPR_DST_C;
					e->_label = l_emit(par, bc_make_iabc(BC_CALL, e->_reg, 1, DYN), line);
					e->_line = line;
					break;
				}
			}
			break;
		}
		case OP_UNPACK: {
			switch (e->_kind) {
				case EXPR_DST_C:
				case EXPR_DST_AC: {
					par->_fvarg = true;
					break;
				}
				default: {
					ai_par_error(par, "bad unpack expression.", line);
				}
			}
			break;
		}
		default: unreachable();
	}
}

void ai_code_binary1(Parser* par, InoutExpr e, a_u32 op, a_u32 line) {
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
		case OP_BIT_XOR:
		case OP_LT:
		case OP_LE:
		case OP_GT:
		case OP_GE: {
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

static void l_compute_int(a_henv env, a_int a, a_int b, OutExpr e, a_u32 op) {
	switch (op) {
		case OP_ADD: {
			e->_kind = EXPR_INT;
			e->_int = ai_op_add_int(env, a, b);
			break;
		}
		case OP_SUB: {
			e->_kind = EXPR_INT;
			e->_int = ai_op_sub_int(env, a, b);
			break;
		}
		case OP_MUL: {
			e->_kind = EXPR_INT;
			e->_int = ai_op_mul_int(env, a, b);
			break;
		}
		case OP_DIV: {
			e->_kind = EXPR_INT;
			e->_int = ai_op_div_int(env, a, b);
			break;
		}
		case OP_MOD: {
			e->_kind = EXPR_INT;
			e->_int = ai_op_mod_int(env, a, b);
			break;
		}
		case OP_SHL: {
			e->_kind = EXPR_INT;
			e->_int = ai_op_shl_int(env, a, b);
			break;
		}
		case OP_SHR: {
			e->_kind = EXPR_INT;
			e->_int = ai_op_shr_int(env, a, b);
			break;
		}
		case OP_BIT_AND: {
			e->_kind = EXPR_INT;
			e->_int = ai_op_band_int(env, a, b);
			break;
		}
		case OP_BIT_OR: {
			e->_kind = EXPR_INT;
			e->_int = ai_op_bor_int(env, a, b);
			break;
		}
		case OP_BIT_XOR: {
			e->_kind = EXPR_INT;
			e->_int = ai_op_bxor_int(env, a, b);
			break;
		}
		case OP_EQ: {
			e->_kind = ai_op_eq_int(env, a, b) ? EXPR_TRUE : EXPR_FALSE;
			break;
		}
		case OP_NE: {
			e->_kind = !ai_op_eq_int(env, a, b) ? EXPR_TRUE : EXPR_FALSE;
			break;
		}
		case OP_LT: {
			e->_kind = ai_op_lt_int(env, a, b) ? EXPR_TRUE : EXPR_FALSE;
			break;
		}
		case OP_LE: {
			e->_kind = ai_op_le_int(env, a, b) ? EXPR_TRUE : EXPR_FALSE;
			break;
		}
		case OP_GT: {
			e->_kind = ai_op_lt_int(env, b, a) ? EXPR_TRUE : EXPR_FALSE;
			break;
		}
		case OP_GE: {
			e->_kind = ai_op_le_int(env, b, a) ? EXPR_TRUE : EXPR_FALSE;
			break;
		}
		default: unreachable();
	}
}

static void l_compute_float(a_henv env, a_float a, a_float b, OutExpr e, a_u32 op) {
	quiet(env);
	switch (op) {
		case OP_ADD: {
			e->_kind = EXPR_FLOAT;
			e->_float = ai_op_add_float(env, a, b);
			break;
		}
		case OP_SUB: {
			e->_kind = EXPR_FLOAT;
			e->_float = ai_op_sub_float(env, a, b);
			break;
		}
		case OP_MUL: {
			e->_kind = EXPR_FLOAT;
			e->_float = ai_op_mul_float(env, a, b);
			break;
		}
		case OP_DIV: {
			e->_kind = EXPR_FLOAT;
			e->_float = ai_op_div_float(env, a, b);
			break;
		}
		case OP_MOD: {
			e->_kind = EXPR_FLOAT;
			e->_float = ai_op_mod_float(env, a, b);
			break;
		}
		case OP_EQ: {
			e->_kind = ai_op_eq_float(env, a, b) ? EXPR_TRUE : EXPR_FALSE;
			break;
		}
		case OP_NE: {
			e->_kind = !ai_op_eq_float(env, a, b) ? EXPR_TRUE : EXPR_FALSE;
			break;
		}
		case OP_LT: {
			e->_kind = ai_op_lt_float(env, a, b) ? EXPR_TRUE : EXPR_FALSE;
			break;
		}
		case OP_LE: {
			e->_kind = ai_op_le_float(env, a, b) ? EXPR_TRUE : EXPR_FALSE;
			break;
		}
		case OP_GT: {
			e->_kind = ai_op_lt_float(env, b, a) ? EXPR_TRUE : EXPR_FALSE;
			break;
		}
		case OP_GE: {
			e->_kind = ai_op_le_float(env, b, a) ? EXPR_TRUE : EXPR_FALSE;
			break;
		}
		default: unreachable();
	}
}

static a_bool l_fold_const_int(Parser* par, InoutExpr e1, InExpr e2, a_u32 op, a_u32 line) {
	a_int i1, i2;
	if (expr_are_ints(e1, &i1, e2, &i2)) {
		l_compute_int(par->_env, e1->_int, e2->_int, e1, op);
		e1->_line = line;
		return true;
	}
	return false;
}

static a_bool l_fold_const_float(Parser* par, InoutExpr e1, InExpr e2, a_u32 op, a_u32 line) {
	a_float f1, f2;
	if (expr_are_floats(e1, &f1, e2, &f2)) {
		l_compute_float(par->_env, e1->_float, e2->_float, e1, op);
		e1->_line = line;
		return true;
	}
	return false;
}

static void l_compare_suffix(Parser* par, InExpr e1, InExpr e2, OutExpr e3,
							 InExpr ord1, InExpr ord2,
							 a_u32 oprr, a_u32 oprk, a_u32 opkr, a_u32 line) {
	if (e2->_kind == EXPR_INT && e2->_int >= BC_MIN_SC && e2->_int <= BC_MAX_SC) {
		l_anyR(par, e1);
		l_drop(par, e1);
		l_emit(par, bc_make_iabsc(oprk, DYN, e1->_reg, e2->_int), line);
	}
	else if (e1->_kind == EXPR_INT && e1->_int >= BC_MIN_SC && e1->_int <= BC_MAX_SC) {
		l_anyR(par, e2);
		l_drop(par, e2);
		l_emit(par, bc_make_iabsc(opkr, DYN, e2->_reg, e1->_int), line);
	}
	else {
		l_anyR(par, ord1);
		l_anyR(par, ord2);
		l_drop(par, e1);
		l_drop(par, e2);
		l_emit(par, bc_make_iabc(oprr, DYN, e1->_reg, e2->_reg), line);
	}

	e3->_kind = EXPR_TRY_TRUE;
	e3->_label = l_emit_jump_direct(par, NO_LABEL, line);
}

static void l_va_push(Parser* par, ExprPack* es, InExpr e2, a_u32 line) {
	if (unlikely(es->_len == VARARG)) {
		ai_par_error(par, "cannot add argument after vararg.", line);
	}
	switch (e2->_kind) {
		case EXPR_UNIT: {
			break;
		}
		case EXPR_PACK: {
			assume(e2->_pack._len == VARARG);
			es->_len = VARARG;
			break;
		}
		case EXPR_DST_AC: {
			bc_swap_a(&par->_code[e2->_label], es->_base + es->_len);
			fallthrough;
		}
		case EXPR_DST_C: {
			assume(bc_load_a(par->_code[e2->_label]) == es->_base + es->_len);
			es->_len = par->_fvarg ? VARARG : es->_len + 1;
			break;
		}
		default: {
			l_topR(par, e2);
			assume(e2->_reg == es->_base + es->_len);
			es->_len += 1;
			break;
		}
	}
}

/**
 ** Post evaluate expression `e1 op e2` and bind result to e1.
 ** The left hand expression should be a constant or a non-volatile expression.
 *@param par the parser.
 *@param e1 the left hand sub expression.
 *@param e2 the right hand sub expression.
 *@param op the operation.
 *@param line the line number of the operation.
 */
void ai_code_binary2(Parser* par, InoutExpr e1, InExpr e2, a_u32 op, a_u32 line) {
	if (unlikely(e1->_kind == EXPR_NEVER || e2->_kind == EXPR_NEVER)) return;
	switch (op) {
		case OP_ADD:
		case OP_SUB:
		case OP_MUL:
		case OP_DIV:
		case OP_MOD: {
			if (l_fold_const_int(par, e1, e2, op, line) || l_fold_const_float(par, e1, e2, op, line))
				return;
			if (e2->_kind == EXPR_INT && e2->_int >= BC_MIN_SC && e2->_kind <= BC_MAX_SC) {
				l_anyR(par, e1);
				l_drop(par, e1);
				expr_dyn(e1, l_emit(par, bc_make_iabsc(BC_ADDI + op - OP_ADD, DYN, e1->_reg, e2->_int), line));
			}
			else {
				l_anyR(par, e2);
				l_anyR(par, e1);
				l_drop(par, e2);
				l_drop(par, e1);
				expr_dyn(e1, l_emit(par, bc_make_iabc(BC_ADD + op - OP_ADD, DYN, e1->_reg, e2->_reg), line));
			}
			break;
		}
		case OP_SHL:
		case OP_SHR:
		case OP_BIT_AND:
		case OP_BIT_OR:
		case OP_BIT_XOR: {
			if (l_fold_const_int(par, e1, e2, op, line))
				return;
			if (e2->_kind == EXPR_INT && e2->_int >= BC_MIN_SC && e2->_kind <= BC_MAX_SC) {
				l_anyR(par, e1);
				l_drop(par, e1);
				expr_dyn(e1, l_emit(par, bc_make_iabsc(BC_SHLI + op - OP_SHL, DYN, e1->_reg, e2->_int), line));
			}
			else {
				l_anyR(par, e2);
				l_anyR(par, e1);
				l_drop(par, e2);
				l_drop(par, e1);
				expr_dyn(e1, l_emit(par, bc_make_iabc(BC_SHL + op - OP_SHL, DYN, e1->_reg, e2->_reg), line));
			}
			break;
		}
		case OP_EQ: {
			if (l_fold_const_int(par, e1, e2, op, line) || l_fold_const_float(par, e1, e2, op, line))
				return;
			l_compare_suffix(par, e1, e2, e1, e2, e1, BC_BEQ, BC_BEQI, BC_BEQI, line);
			break;
		}
		case OP_NE: {
			if (l_fold_const_int(par, e1, e2, op, line) || l_fold_const_float(par, e1, e2, op, line))
				return;
			l_compare_suffix(par, e1, e2, e1, e2, e1, BC_BNE, BC_BNEI, BC_BNEI, line);
			break;
		}
		case OP_LT: {
			if (l_fold_const_int(par, e1, e2, op, line) || l_fold_const_float(par, e1, e2, op, line))
				return;
			l_compare_suffix(par, e1, e2, e1, e2, e1, BC_BLT, BC_BLTI, BC_BGTI, line);
			break;
		}
		case OP_GT: {
			if (l_fold_const_int(par, e1, e2, op, line) || l_fold_const_float(par, e1, e2, op, line))
				return;
			l_compare_suffix(par, e2, e1, e1, e2, e1, BC_BLT, BC_BLTI, BC_BGTI, line);
			break;
		}
		case OP_LE: {
			if (l_fold_const_int(par, e1, e2, op, line) || l_fold_const_float(par, e1, e2, op, line))
				return;
			l_compare_suffix(par, e1, e2, e1, e2, e1, BC_BLE, BC_BLEI, BC_BGEI, line);
			break;
		}
		case OP_GE: {
			if (l_fold_const_int(par, e1, e2, op, line) || l_fold_const_float(par, e1, e2, op, line))
				return;
			l_compare_suffix(par, e2, e1, e1, e2, e1, BC_BLE, BC_BLEI, BC_BGEI, line);
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
				case EXPR_TRY_TRUE: {
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
				case EXPR_TRY_FALSE: {
					e1->_kind = l_condF(par, e2, &e1->_label, line);
					ai_code_drop(par, e2);
					break;
				}
				default: unreachable();
			}
			break;
		}
		case OP_VA_PUSH: {
			if (unlikely(e2->_kind == EXPR_NEVER)) {
				expr_never(e1, line);
				return;
			}
			if (unlikely(e1->_kind != EXPR_PACK)) {
				switch (e1->_kind) {
					case EXPR_NEVER: {
						return;
					}
					case EXPR_UNIT: {
						expr_copy(e1, e2);
						return;
					}
					default: {
						assume(e1->_kind != EXPR_PACK);
						l_topR(par, e1);
						e1->_kind = EXPR_PACK;
						e1->_pack = new(ExprPack) { e1->_reg, 1 };
						break;
					}
				}
			}
			l_va_push(par, &e1->_pack, e2, line);
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
		case EXPR_DST_OR_NIL: {
			a_u32 label2 = l_try(par, e1, line);
			l_merge_label(par, &e1->_cond._whenf, label2, line);
			l_dynR(par, e2);
			l_merge_optR(par, e1->_cond._whenf, e1->_reg, line);
			break;
		}
		case EXPR_TMP_OR_NIL: {
			a_u32 label2 = l_try(par, e1, line);
			l_merge_label(par, &e1->_cond._whenf, label2, line);
			l_fixR(par, e2, e1->_reg);
			l_merge_optR(par, e1->_cond._whenf, e1->_reg, line);
			break;
		}
		case EXPR_TMP: {
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
			expr_tmp(e1, reg, line);
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
			if (label == NO_LABEL)
				return;
			switch (e->_kind) {
				case EXPR_NEVER: {
					l_lazy_jump(par, label, line);
					l_mark_label(par, label, line);
					e->_kind = EXPR_NIL;
					break;
				}
				case EXPR_TMP: {
					e->_kind = EXPR_TMP_OR_NIL;
					e->_cond._whent = e->_reg;
					e->_cond._whenf = label;
					break;
				}
				default: {
					l_dynR(par, e);
					fallthrough;
				}
				case EXPR_DST_A: {
					e->_kind = EXPR_DST_OR_NIL;
					e->_cond._whent = e->_label;
					e->_cond._whenf = label;
					break;
				}
				case EXPR_TMP_OR_NIL:
				case EXPR_DST_OR_NIL: {
					l_merge_label(par, &e->_cond._whenf, label, line);
					break;
				}
			}
			e->_line = line;
			break;
		}
		case OP_OR_ELSE: {
			switch (e->_kind) {
				case EXPR_TMP: {
					l_free_stack(par, e->_reg);
					break;
				}
				case EXPR_TMP_OR_NIL: {
					l_free_stack(par, e->_cond._whent);
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

static void l_va_pop(unused Parser* par, InoutExpr es, InoutExpr e, a_u32 line) {
	if (es->_kind == EXPR_PACK) {
		ExprPack* pack = &es->_pack;
		assume(pack->_len > 0);
		pack->_len -= 1;
		expr_tmp(e, pack->_base + pack->_len, line);
	}
	else {
		expr_copy(e, es);
	}
}

void ai_code_multi(Parser* par, InoutExpr es, InoutExpr e, a_u32 op, a_u32 line) {
	switch (op) {
		case OP_VA_POP: {
			l_va_pop(par, es, e, line);
			break;
		}
		default: unreachable();
	}
}

/**
 ** Take expected argument.
 *@param par the parser.
 *@param es the head expressions, and become expression pack in result.
 *@param e the tail expression, and become the last expression in result.
 *@param n the argument expected.
 *@param line the line number.
 *@return true if success and false for otherwise.
 */
a_bool ai_code_balance(Parser* par, InoutExpr es, InoutExpr e, a_u32 n, a_u32 line) {
	assume(n > 0);
	a_u32 m;
	switch (es->_kind) {
		case EXPR_NEVER: {
			return true;
		}
		case EXPR_UNIT: {
			m = 0;
			break;
		}
		default: {
			if (n == 1) {
				expr_copy(e, es);
				es->_kind = EXPR_UNIT;
				return true;
			}
			else {
				m = 1;
			}
			break;
		}
		case EXPR_PACK: {
			ExprPack* pack = &es->_pack;
			m = pack->_len;
			if (m >= n) {
				ai_code_drop(par, e);
				es->_pack._len = n;
				l_succ_free_stack(par, es->_pack._base + n, m - n);
				return true;
			}
			break;
		}
	}
	switch (e->_kind) {
		case EXPR_NEVER: {
			return true;
		}
		case EXPR_UNIT: {
			assume(m <= n);
			if (m == n) {
				if (n > 0) {
					l_va_pop(par, es, e, line);
				}
				return true;
			}
			return false;
		}
		default: {
			assume(m + 1 <= n);
			return m + 1 == n;
		}
	}
}

static a_bool l_try_append(Parser* par, QBuf* buf, InExpr e) {
	switch (e->_kind) {
		case EXPR_INT: {
			l_check_alloc(ai_fmt_puti(par->_env, buf, e->_int));
			return true;
		}
		case EXPR_FLOAT: {
			l_check_alloc(ai_fmt_putf(par->_env, buf, e->_float));
			return true;
		}
		case EXPR_STR: {
			GStr* str = e->_str;
			l_check_alloc(ai_buf_putsx_(par->_env, buf, str->_data, str->_len));
			return true;
		}
		default: {
			return false;
		}
	}
}

static GStr* buf_to_str(Parser* par, QBuf* buf) {
	GStr* str = ai_lex_tostr(&par->_lex, buf->_arr, buf->_len);
	ai_buf_reset(buf);
	return str;
}

void ai_code_concat_next(Parser* par, ConExpr* ce, InExpr e, a_u32 line) {
	if (unlikely(e->_kind == EXPR_NEVER)) return;
	if (l_try_append(par, &ce->_buf, e)) {
		if (par->_qbq != &ce->_buf) {
			/* Link queue. */
			ce->_buf._last = par->_qbq;
			par->_qbq = &ce->_buf;
		}
	}
	else if (ce->_buf._len > 0) {
		l_dynR(par, e); /* Drop used register. */

		a_u32 reg = l_alloc_stack(par, line);
		l_fixR(par, e, l_alloc_stack(par, line));

		GStr* str = buf_to_str(par, &ce->_buf);
		l_emit_k(par, reg, v_of_ref(str), line);
		if (ce->_head._len == 0) {
			ce->_head._base = reg;
		}
		ce->_head._len += 2;

		ai_buf_reset(&ce->_buf);
	}
	else {
		l_va_push(par, &ce->_head, e, line);
	}
}

void ai_code_concat_end(Parser* par, ConExpr* ce, OutExpr e, a_u32 line) {
	if (ce->_head._len == 0) {
		e->_kind = EXPR_STR;
		e->_str = buf_to_str(par, &ce->_buf);
		e->_line = line;
	}
	else {
		if (ce->_buf._len > 0) {
			GStr* str = buf_to_str(par, &ce->_buf);
			a_u32 reg = l_alloc_stack(par, line);
			assume(reg == ce->_head._base + ce->_head._len);
			l_emit_k(par, reg, v_of_ref(str), line);
			ce->_head._len += 1;
		}
		expr_dyn(e, l_emit(par, bc_make_iabc(BC_CAT, DYN, ce->_head._base, ce->_head._len), line));
		l_va_drop(par, &ce->_head);
	}
	/* Check and drop string buffer. */
	if (par->_qbq == &ce->_buf) {
		l_bdel(par, ce->_buf);
		par->_qbq = ce->_buf._last;
	}
}

static a_insn l_is_leave(Parser* par, a_u32 label) {
	if (unlikely(label == par->_head_label))
		return 0;
	a_insn i = par->_code[label];
	a_u32 op = bc_load_op(i);
	return (op == BC_RET) ? i : 0;
}

/**
 ** Jump to determined label.
 *@param par the parser.
 *@param label the label jump to.
 *@param line the line number.
 */
void ai_code_gotoD(Parser* par, a_u32 label, a_u32 line) {
	if (l_should_eval(par)) {
		l_flush_jump(par, line);

		a_insn i = l_is_leave(par, label);
		if (likely(i == 0)) {
			l_flush_land(par, line);
			if (par->_fpass) {
				l_emit_jump_direct(par, label, line);
				par->_fpass = false;
			}
		}
		else {
			l_emit_leave(par, i, line);
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
			return par->_head_label; /* Return next instruction as pseudo label. */
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

a_u32 ai_code_label(Parser* par, a_u32 label, a_u32 line) {
	if (label != NO_LABEL) {
		if (label != par->_head_label) { /* If from label is not pseudo head label, merge with head jump label. */
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
	return par->_head_label;
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

static void l_drop_ref(Parser* par, InExpr e) {
	switch (e->_kind) {
		case EXPR_REF_ALL: {
			if (e->_kind & 0x2) {
				l_free_stack(par, e->_ref._key);
			}
			if (e->_kind & 0x1) {
				l_free_stack(par, e->_ref._base);
			}
			break;
		}
		default: {
			panic("not index expression.");
		}
	}
}

void ai_code_drop(Parser* par, InExpr e) {
	switch (e->_kind) {
		case EXPR_DST_C:
		case EXPR_DST_AC: {
			bc_swap_c(&par->_code[e->_label], 1);
			break;
		}
		case EXPR_DST_A: {
			a_u32 reg = l_alloc_stack(par, e->_line);
			bc_swap_a(&par->_code[e->_label], reg);
			l_free_stack(par, reg);
			break;
		}
		case EXPR_TMP: {
			l_free_stack(par, e->_reg);
			break;
		}
		case EXPR_REF_ALL: {
			l_drop_ref(par, e);
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
		case EXPR_VAR:
		case EXPR_TMP: {
			l_fixR(par, e2, e1->_reg);
			break;
		}
		case EXPR_CAP: {
			l_anyR(par, e2);
			l_emit(par, bc_make_iab(BC_STC, e1->_reg, e2->_reg), line);
			l_drop(par, e2);
			break;
		}
		case EXPR_REFR_ALL: {
			l_anyR(par, e2);
			l_emit(par, bc_make_iabc(BC_SET, e1->_ref._base, e1->_ref._key, e2->_reg), line);
			l_drop(par, e2);
			break;
		}
		case EXPR_REFI_ALL: {
			l_anyR(par, e2);
			l_emit(par, bc_make_iabc(BC_SETI, e1->_ref._base, e1->_ref._key, e2->_reg), line);
			l_drop(par, e2);
			break;
		}
		case EXPR_REFK_ALL: {
			l_anyR(par, e2);

			a_u32 k = e1->_ref._key;
			if (likely(k <= BC_MAX_C)) {
				l_emit(par, bc_make_iabc(BC_SETK, e2->_reg, e1->_ref._base, k), line);
			}
			else {
				l_emit(par, bc_make_iabc(BC_SETKX, e2->_reg, e1->_ref._base, DMB), line);
				l_emit_fast(par, bc_make_iax(BC_EX, k), line);
			}

			l_drop(par, e2);
			break;
		}
		case EXPR_REFCK: {
			l_anyR(par, e2);

			a_u32 reg = l_alloc_stack(par, line);
			l_emit(par, bc_make_iab(BC_LDC, reg, e1->_ref._base), line);
			expr_tmp(e1, reg, line);

			a_u32 k = e1->_ref._key;
			if (likely(k <= BC_MAX_C)) {
				l_emit(par, bc_make_iabc(BC_SETK, e2->_reg, reg, k), line);
			}
			else {
				l_emit(par, bc_make_iabc(BC_SETKX, e2->_reg, reg, DMB), line);
				l_emit_fast(par, bc_make_iax(BC_EX, k), line);
			}
			l_drop(par, e2);
			break;
		}
		default: {
			ai_par_error(par, "cannot assign to the expression.", e1->_line);
		}
	}
	ai_code_drop(par, e1);
}

#define NAMES_GROW_UNIT 64

static void l_push_name(Parser* par, Sym sym) {
	Syms* syms = &par->_syms;
	if (syms->_top == syms->_cap) {
		a_usize old_cap = syms->_cap;
		a_usize new_cap = old_cap + NAMES_GROW_UNIT;
		syms->_stack = l_vgrow(par, syms->_stack, old_cap, new_cap);
		syms->_cap = new_cap;
	}
	syms->_stack[syms->_top++] = sym;
}

static a_u32 l_push_local(Parser* par, LocalInfo info) {
	return l_bput(par, par->_locals, info, 32);
}

static void l_bind_local(Parser* par, GStr* name, a_u32 reg, a_u32 begin_label) {
	a_u32 index = l_push_local(par, new(LocalInfo) {
		._begin_label = begin_label,
		._end_label = NO_LABEL,
		._name = name,
		._reg = reg,
	});

	l_push_name(par, new(Sym) {
		._scope = par->_scope_depth,
		._index = index,
		._kind = SYM_LOCAL,
		._name = name
	});
}

static a_u32 l_let_node_count(LetNode* node) {
	a_u32 n = 0;
	for (; node != null; node = node->_sibling) {
		n += 1;
	}
	return n;
}

void ai_code_let_nils(Parser* par, LetStat* s, a_u32 line) {
	Scope* scope = par->_scope;
	if (l_let_node_count(s->_head) != s->_nnode)
		ai_par_error(par, "missing assignment for pattern.", line);

	assume(scope->_top_ntr == scope->_top_reg);
	a_u32 num = s->_nnode;
	a_u32 reg = l_succ_alloc_stack(par, num, line);
	a_u32 label = l_emit_kn(par, reg, num, line);
	for (LetNode* node = s->_head; node != null; node = node->_sibling) {
		l_bind_local(par, node->_expr._str, reg++, label);
	}

	scope->_top_ntr = scope->_top_reg = reg + num;
}

static void l_let_bind(Parser* par, LetStat* s, LetNode* n, InExpr e) {
	switch (n->_kind) {
		case PAT_DROP: {
			ai_code_drop(par, e);
			break;
		}
		case PAT_BIND: {
			l_tmpR(par, e);
			l_bind_local(par, n->_expr._str, e->_reg, par->_head_label);
			break;
		}
		case PAT_TUPLE: {
			a_u32 line = n->_expr._line;
			a_u32 num = l_let_node_count(n->_child);
			l_topR(par, e);
			if (s->_ftest) {
				a_u32 reg = l_alloc_stack(par, line);
				l_emit(par, bc_make_iab(BC_LEN, reg, e->_reg), line);
				s->_label_fail = l_emit_branch(par, bc_make_iabc(BC_BNEI, DMB, reg, num), s->_label_fail, line);
				l_free_stack(par, reg);
			}
			l_drop(par, e);
			a_u32 reg = l_succ_alloc_stack(par, num, line);
			l_emit(par, bc_make_iabc(BC_UNBOX, reg, e->_reg, num), line);
			for (LetNode* nchild = n->_child; nchild != null; nchild = nchild->_sibling) {
				Expr e2;
				expr_tmp(&e2, reg++, line);
				l_let_bind(par, s, nchild, &e2);
			}
			break;
		}
		default:
			unreachable();
	}
}

a_bool ai_code_let_bind(Parser* par, LetStat* s, InExpr e) {
	LetNode* node = s->_head;

	assume(node != null);

	l_let_bind(par, s, node, e);

	node = node->_sibling;
	s->_head = node;
	return node != null;
}

static void l_push_scope(Parser* par, Scope* scope, a_u32 reg) {
	*scope = new(Scope) {
		._up = par->_scope,
		._bot_reg = reg,
		._top_reg = reg,
		._bot_fur = reg,
		._num_fur = 0,
		._begin_label = par->_head_label,
		._end_label = NO_LABEL,
		._bot_sym = par->_syms._top
	};
	par->_scope = scope;
}

static void l_pop_scope(Parser* par) {
	Scope* scope = par->_scope;
	par->_scope = scope->_up;
	run {
		a_u32 bot = scope->_bot_sym;
		a_u32 top = par->_syms._top;
		a_u32 label = par->_head_label;
		for (a_u32 i = bot; i < top; ++i) {
			Sym* sym = &par->_syms._stack[i];
			switch (par->_syms._stack[i]._kind) {
				case SYM_LOCAL:
					l_local_at(par, sym->_index)->_end_label = label;
					break;
				default:
					unreachable();
			}
		}
		par->_syms._top = bot;
	}
}

void ai_code_enter(Parser* par, Scope* scope) {
	l_push_scope(par, scope, par->_scope->_top_reg);
}

void ai_code_leave(Parser* par) {
	assume(par->_scope->_up != null);
	l_pop_scope(par);
}

void ai_code_prologue(Parser* par, FnScope* fnscope) {
	assume(par->_head_land == NO_LABEL && par->_head_jump == NO_LABEL, "residual jump not flushed.");

	if (par->_fnscope != null) {
		par->_fnscope->_top_scope = par->_scope;
	}

	par->_scope = null;

	*fnscope = new(FnScope) {
		._fn_up = par->_fnscope,
		._base_subs = cast(GFunMeta**, par->_rq._tail),
		._begin_line = par->_lines._len,
		._begin_local = par->_locals._len
	};
	l_push_scope(par, &fnscope->_scope, 0);

	par->_fnscope = fnscope;
	par->_scope_depth += 1;
}

GFunMeta* ai_code_epilogue(Parser* par, a_bool root, a_u32 line) {
	FnScope* scope = par->_fnscope;

	if (l_should_eval(par)) {
		l_emit_ret(par, DMB, 0, line);
	}

	l_pop_scope(par);

	FnCreateInfo info = {
		._nconst = scope->_consts._len,
		._ninsn = par->_head_label - scope->_begin_label,
		._nlocal = par->_locals._len - scope->_begin_local,
		._ncap = par->_captures._len - scope->_begin_cap,
		._nstack = scope->_max_reg,
		._nline = par->_lines._len - scope->_begin_line,
		._debug = {
			._fline = (par->_options & ALO_COMP_OPT_DROP_DEBUG_LINE) == 0,
			._fname = (par->_options & ALO_COMP_OPT_DROP_DEBUG_NAME) == 0
		}
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

	ai_buf_copy(meta->_consts, &scope->_consts);
	memcpy(meta->_insns, par->_code + scope->_scope._begin_label, sizeof(a_insn) * info._ninsn);
	if (info._debug._fline) {
		memcpy(meta->_lines, par->_lines._arr + scope->_begin_line, sizeof(LineInfo) * info._nline);
	}
	if (info._debug._fname) {
		memcpy(meta->_locals, par->_locals._arr + scope->_begin_local, sizeof(LocalInfo) * info._nlocal);
	}
	run {
		for (a_u32 i = 0; i < info._ncap; ++i) {
			CompCapInfo* cap_info = &par->_captures._arr[scope->_begin_cap + i];
			meta->_caps[i] = new(CapInfo) {
				._up = cap_info->_scope != par->_scope_depth,
				._reg = cap_info->_index
			};
			if (info._debug._fname) {
				meta->_cap_names[i] = cap_info->_name;
			}
		}
	}
	run { /* Build sub function */
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

	l_close_fn_scope(par, scope);

	FnScope* up_scope = scope->_fn_up;

	par->_fnscope = up_scope;
	par->_scope = up_scope != null ? up_scope->_top_scope : null;
	par->_head_label = scope->_begin_label;
	par->_syms._top = scope->_bot_sym;
	par->_locals._len = scope->_begin_local;
	par->_captures._len = scope->_begin_cap;
	par->_lines._len = scope->_begin_line;
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
	l_bdel(par, par->_insns);
	l_vdel(par, par->_syms._stack, par->_syms._cap);
	ai_lex_close(&par->_lex);

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
	l_push_name(par, new(Sym) {
		._scope =  SCOPE_DEPTH_ENV,
		._index =  0,
		._kind =  SYM_LOCAL,
		._name =  ai_env_strx(G(par->_env), STRX_KW__ENV)
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
	do l_close_fn_scope(par, scope);
	while ((scope = scope->_fn_up) != null);
	/* Destroy queued prototypes. */
	rq_for(obj, &par->_rq) { l_del_meta(par->_env, downcast(GFunMeta, obj)); }
	/* Close queued string buffers. */
	for (QBuf* qb = par->_qbq; qb != null; qb = qb->_last) {
		ai_buf_close(G(par->_env), qb);
	}
	/* Close parser. */
	parser_close(par);
}

static a_none l_error_unit_expr(Parser* par, a_u32 line) {
	ai_par_error(par, "attempt to use result of an unit expression.", line);
}

static a_none l_error_vararg_expr(Parser* par, a_u32 line) {
	ai_par_error(par, "attempt to use variable length argument expression.", line);
}

static void l_dynR(Parser* par, InoutExpr e) {
	switch (e->_kind) {
		case EXPR_UNIT: {
			l_error_unit_expr(par, e->_line);
		}
		case EXPR_PACK: {
			l_error_vararg_expr(par, e->_line);
			break;
		}
		case EXPR_NIL: {
			expr_dyn(e, l_emit_kn(par, DYN, 1, e->_line));
			break;
		}
		case EXPR_FALSE:
		case EXPR_TRUE: {
			expr_dyn(e, l_emit_kz(par, DYN, (e->_kind & 1) != 0, e->_line));
			break;
		}
		case EXPR_INT: {
			if (e->_int >= BC_MIN_SBX && e->_int <= BC_MAX_SBX) {
				expr_dyn(e, l_emit(par, bc_make_iasbx(BC_KI, DYN, e->_int), e->_line));
			}
			else {
				expr_dyn(e, l_emit_k(par, DYN, v_of_int(e->_int), e->_line));
			}
			break;
		}
		case EXPR_FLOAT: {
			expr_dyn(e, l_emit_k(par, DYN, v_of_float(e->_float), e->_line));
			break;
		}
		case EXPR_STR: {
			expr_dyn(e, l_emit_k(par, DYN, v_of_ref(e->_str), e->_line));
			break;
		}
		case EXPR_TMP:
		case EXPR_VAR: {
			expr_dyn(e, l_emit_mov(par, DYN, e->_reg, e->_line));
			break;
		}
		case EXPR_CAP: {
			expr_dyn(e, l_emit(par, bc_make_iab(BC_LDC, DYN, e->_reg), e->_line));
			break;
		}
		case EXPR_TRY_FALSE:  {
			l_instantiate_branch(par, e, true);
			break;
		}
		case EXPR_TRY_TRUE:  {
			l_instantiate_branch(par, e, false);
			break;
		}
		case EXPR_TMP_OR_NIL: {
			e->_kind = EXPR_DST_OR_NIL;
			e->_cond._whent = l_emit_mov(par, DYN, e->_cond._whent, e->_line);
			break;
		}
		case EXPR_REFR_ALL: {
			l_drop_ref(par, e);
			expr_dyn(e, l_emit(par, bc_make_iabc(BC_GET, DYN, e->_ref._base, e->_ref._key), e->_line));
			ai_code_drop(par, e);
			break;
		}
		case EXPR_REFI_ALL: {
			l_drop_ref(par, e);
			expr_dyn(e, l_emit(par, bc_make_iabc(BC_GETI, DYN, e->_ref._base, e->_ref._key), e->_line));
			break;
		}
		case EXPR_REFK_ALL: {
			a_u32 k = e->_ref._key;
			l_drop_ref(par, e);
			if (likely(k <= BC_MAX_C)) {
				expr_dyn(e, l_emit(par, bc_make_iabc(BC_GETK, DYN, e->_ref._base, k), e->_line));
			}
			else {
				expr_dyn(e, l_emit(par, bc_make_iabc(BC_GETKX, DYN, e->_ref._base, DMB), e->_line));
				l_emit_fast(par, bc_make_iax(BC_EX, k), e->_line);
			}
			break;
		}
		case EXPR_REFCK: {
			a_u32 k = e->_ref._key;
			if (likely(k < BC_MAX_C)) {
				expr_dyn(e,  l_emit(par, bc_make_iabc(BC_CGETK, DYN, e->_ref._base, k), e->_line));
			}
			else {
				expr_dyn(e, l_emit(par, bc_make_iabc(BC_CGETKX, DYN, e->_ref._base, DMB), e->_line));
				l_emit_fast(par, bc_make_iax(BC_EX, k), e->_line);
			}
			break;
		}
		case EXPR_DST_AC: {
			a_u32 label = e->_label;
			bc_swap_c(&par->_code[label], 2);
			par->_fvarg = false;
			expr_dyn(e, label);
			break;
		}
		case EXPR_DST_C: {
			a_insn* pi = &par->_code[e->_label];
			bc_swap_c(pi, 2);
			par->_fvarg = false;
			expr_dyn(e, l_emit_mov(par, DYN, bc_load_a(*pi), e->_line));
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
			bc_swap_a(&par->_code[e->_label], reg);
			break;
		}
		case EXPR_TMP_OR_NIL: {
			a_u32 reg2 = e->_cond._whent;
			if (reg == reg2) {
				l_merge_optR(par, e->_cond._whenf, reg, e->_line);
				break;
			}
			l_free_stack(par, reg2);
			e->_cond._whent = l_emit_mov(par, DYN, reg2, e->_line);
			fallthrough;
		}
		case EXPR_DST_OR_NIL: {
			bc_swap_a(&par->_code[e->_cond._whent], reg);
			l_merge_optR(par, e->_cond._whenf, reg, e->_line);
			break;
		}
		default: unreachable();
	}
	expr_tmp(e, reg, e->_line);
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
		case EXPR_TMP_OR_NIL: {
			if (e->_cond._whent != reg) {
				l_free_stack(par, e->_cond._whent);
				l_emit_mov(par, reg, e->_cond._whent, e->_line);
			}
			l_merge_optR(par, e->_cond._whenf, reg, e->_line);
			expr_tmp(e, reg, e->_line);
			break;
		}
		case EXPR_VAR:
		case EXPR_TMP: {
			if (e->_reg != reg) {
				l_drop(par, e);
				l_emit_mov(par, reg, e->_cond._whent, e->_line);
			}
			e->_reg = reg;
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
		case EXPR_PACK: {
			l_error_vararg_expr(par, e->_line);
		}
		case EXPR_NIL: {
			a_u32 reg = l_alloc_stack(par, e->_line);
			l_emit_kn(par, reg, 1, e->_line);
			break;
		}
		case EXPR_VAR: {
			a_u32 reg = l_alloc_stack(par, e->_line);
			l_emit_mov(par, reg, e->_reg, e->_line);
			break;
		}
		case EXPR_TMP: {
			a_u32 reg1 = e->_reg;
			a_u32 reg2;

			l_free_stack(par, reg1);
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
		case EXPR_DST_C: {
			a_insn* pi = &par->_code[e->_label];
			bc_swap_c(pi, 2);

			a_u32 reg = bc_load_a(*pi);
			assume(l_is_topR(par, reg));
			expr_tmp(e, reg, e->_line);
			break;
		}
	}
}

static void l_anyR(Parser* par, InoutExpr e) {
	switch (e->_kind) {
		case EXPR_NEVER: {
			e->_reg = DMB;
			break;
		}
		case EXPR_TMP:
		case EXPR_VAR: {
			break;
		}
		case EXPR_UNIT: {
			l_error_unit_expr(par, e->_line);
		}
		case EXPR_TMP_OR_NIL: {
			a_u32 reg = e->_cond._whent;
			l_merge_optR(par, e->_cond._whenf, reg, e->_line);
			expr_tmp(e, reg, e->_line);
			break;
		}
		case EXPR_DST_C: {
			a_insn* pc = &par->_code[e->_label];
			a_u32 reg = bc_load_a(*pc);
			bc_swap_c(pc, 2);
			expr_tmp(e, reg, e->_line);
			break;
		}
		default: {
			l_topR(par, e);
			break;
		}
	}
}

static void l_tmpR(Parser* par, InoutExpr e) {
	if (e->_kind != EXPR_TMP) {
		l_topR(par, e);
	}
}

static void l_anyRK(Parser* par, InoutExpr e) {
	switch (e->_kind) {
		case EXPR_NEVER:
		case EXPR_UNIT:
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
		case EXPR_TRY_TRUE: {
			*plabel = e->_label;
			return EXPR_TRY_TRUE;
		}
		case EXPR_TRY_FALSE: {
			l_negate_branch(par, e->_label, plabel, line);
			return EXPR_TRY_TRUE;
		}
		default: {
			l_anyR(par, e);
			assume(e->_kind == EXPR_TMP || e->_kind == EXPR_VAR);
			*plabel = l_emit_test(par, BC_BZ, e->_reg, *plabel, line);
			return EXPR_TRY_TRUE;
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
		case EXPR_TRY_FALSE: {
			return EXPR_TRY_FALSE;
		}
		case EXPR_TRY_TRUE: {
			l_negate_branch(par, e->_label, plabel, line);
			return EXPR_TRY_FALSE;
		}
		default: {
			l_anyR(par, e);
			*plabel = l_emit_test(par, BC_BNZ, e->_reg, *plabel, line);
			return EXPR_TRY_FALSE;
		}
	}
}

static a_u32 l_try(Parser* par, InoutExpr e, a_u32 line) {
	switch (e->_kind) {
		case EXPR_NIL: {
			a_u32 label = l_lazy_jump(par, NO_LABEL, line);
			e->_kind = EXPR_NIL;
			return label;
		}
		case EXPR_DST_OR_NIL: {
			a_u32 label = e->_cond._whenf;
			expr_dyn(e, e->_cond._whent);
			return label;
		}
		case EXPR_TMP_OR_NIL: {
			a_u32 label = e->_cond._whenf;
			expr_tmp(e, e->_cond._whent, line);
			return label;
		}
		default: {
			return NO_LABEL;
		}
	}
}