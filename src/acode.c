/**
 *@file acode.c
 */

#define acode_c_
#define ALO_LIB

#include <string.h>

#include "abc.h"
#include "afun.h"
#include "agc.h"
#include "afmt.h"
#include "astrx.h"

#include "acode.h"

enum {
	/* Mark operand which is not determined. */
	DYN = 255,
	/* Mark unused operand. */
	DMB = 0
};

/* Variable length of arguments. */
#define VARARG cast(a_u32, -1)

#define l_check_alloc(e) if (unlikely((e) != ALO_SOK)) ai_mem_nomem((par)->_env)

#define l_bput(par,b,v,u)  ({ \
    typeof(b)* _buf = &(b); \
    if (unlikely(_buf->_len == _buf->_cap)) { \
        l_check_alloc(ai_buf_ngrow((par)->_env, _buf, _buf->_cap + u)); \
    } \
	a_usize _index = _buf->_len++; \
    _buf->BUF_PTR_REF[_index] = (v); \
	_index; \
})

#define l_bdel(par,buf) ai_buf_deinit(G((par)->_env), &(buf))

/**
 ** Load the index for constant.
 *@return the index of constant in constant pool.
 */
static a_u32 l_const_index(Parser* par, Value val) {
	ConstBuf* consts = &par->_consts;
	for (a_u32 i = par->_fnscope->_const_off; i < consts->_len; ++i) {
		/*
		 * Since all literals which have the same format provided by compiler should
		 * have same binary data, use identity equality for comparison.
		 */
		if (v_trivial_equals_unchecked(consts->_ptr[i], val)) {
			return i;
		}
	}

	if (unlikely(par->_fnscope->_const_off + consts->_len == BC_MAX_BX + 1)) {
		ai_par_report(par, false, par_err_f_arg(par, "too many constants."));
	}

	return l_bput(par, *consts, val, 256);
}

static a_bool l_const_is_istr(Parser* par, a_u32 index) {
	return v_is_istr(par->_consts._ptr[par->_fnscope->_const_off + index]);
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

static void l_emit_line(Parser* par, a_line line) {
	FnScope* scope = par->_fnscope;
	if (scope->_head_line != line) {
		LineInfo info = new(LineInfo) {UINT32_MAX, line};
		a_u32 index = l_bput(par, par->_lines, info, 512);
		if (index > scope->_line_off) { /* Settle end label for last line info. */
			par->_lines._ptr[index - 1]._end = par->_head_label;
		}
		scope->_head_line = line;
	}
}

static a_u32 l_emit_direct(Parser* par, a_insn insn, a_u32 line) {
	insn_check(insn);
	l_emit_line(par, line);
	return l_bput(par, par->_insns, insn, 1024);
}

static a_bool l_should_eval(Parser* par) {
	return likely(par->_fnscope->_fpass || par->_fnscope->_fland);
}

static a_i32 l_make_jump_diff(Parser* par, a_u32 from, a_u32 to, a_line line) {
	a_i32 diff = cast(a_i32, to - from - 1);
	if (unlikely(diff < BC_MIN_SAX || diff > BC_MAX_SAX)) {
		ai_par_error(par, "instruction jump out of bound.", line);
	}
	return diff;
}

static a_u32 l_emit_jump_direct(Parser* par, a_u32 label, a_line line) {
	a_i32 diff = label != NO_LABEL ? l_make_jump_diff(par, par->_head_label, label, line) : -1;
	return l_emit_direct(par, bc_make_isax(BC_J, diff), line);
}

static a_u32 l_next_jump(Parser* par, a_u32 label) {
	assume(label <= par->_head_label, "not valid label.");
	if (label == par->_head_label)
		return par->_fnscope->_head_land;

	a_insn i = par->_code[label];
	assume(bc_load_op(i) == BC_J);

	a_i32 disp = bc_load_sax(i);
	return disp != -1 ? label + 1 + cast(a_u32, disp) : NO_LABEL;
}

static void l_redirect(Parser* par, a_u32 from, a_u32 to, a_line line) {
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
static void l_redirect_chain(Parser* par, a_u32 from, a_u32 to, a_line line) {
	loop {
		a_u32 next = l_next_jump(par, from);
		l_redirect(par, from, to, line);
		if (next == NO_LABEL) break;
		from = next;
	}
}

static void l_clear_close(Parser* par) {
	par->_fnscope->_fclose = false;
}

static void l_clear_jump(Parser* par) {
	par->_fnscope->_fjump = false;
	par->_fnscope->_head_jump = NO_LABEL;
}

static void l_clear_land(Parser* par) {
	par->_fnscope->_fland = false;
	par->_fnscope->_head_land = NO_LABEL;
}

static void l_emit_fast(Parser* par, a_insn i, a_line line) {
	if (par->_fnscope->_fpass) {
		l_emit_direct(par, i, line);
	}
}

static void l_flush_close(Parser* par) {
	if (par->_fnscope->_fclose) {
		l_emit_direct(par, bc_make_ia(BC_CLOSE, par->_scope->_top_ntr), par->_fnscope->_close_line);
		l_clear_close(par);
	}
}

static void l_flush_jump(Parser* par, unused a_line line) {
	if (par->_fnscope->_fjump) {
		assume(par->_fnscope->_fland); /* When jump is defined, the branch is reachable only if land is also defined. */
		/* Link to previous jump instruction. */
		l_emit_jump_direct(par, par->_fnscope->_head_jump, par->_fnscope->_head_jump_line);
		l_clear_jump(par);
	}
}

static void l_flush_land(Parser* par, a_line line) {
	if (par->_fnscope->_fland) {
		l_redirect_chain(par, par->_fnscope->_head_land, par->_head_label, line);
		l_clear_land(par);
	}
}

/**
 ** Emit an instruction to leave current function.
 */
static void l_emit_leave(Parser* par, a_insn i, a_line line) {
	if (l_should_eval(par)) {
		FnScope* scope = par->_fnscope;

		l_flush_close(par);
		l_flush_jump(par, line);
		if (scope->_fland) {
			a_u32 label = scope->_head_land;
			loop {
				a_u32 next = l_next_jump(par, label);
				par->_code[label] = i;
				if (next == NO_LABEL) break;
				label = next;
			}
			l_clear_land(par);
		}
		if (scope->_fpass) {
			l_emit_direct(par, i, line);
		}
		scope->_fpass = false;
	}
}

static a_u32 l_emit(Parser* par, a_insn i, a_line line) {
	if (l_should_eval(par)) {
		l_flush_close(par);
		l_flush_jump(par, line);
		l_flush_land(par, line);
		par->_fnscope->_fpass = true;
		return l_emit_direct(par, i, line);
	}
	return NO_LABEL;
}

static a_u32 l_emit_mov(Parser* par, a_u32 dst, a_u32 src, a_line line) {
	return l_emit(par, bc_make_iab(BC_MOV, dst, src), line);
}

static a_u32 l_emit_kz(Parser* par, a_u32 dst, a_bool val, a_line line) {
	assume(val == false || val == true);
	return l_emit(par, bc_make_ia(BC_KF | val, dst), line);
}

static a_u32 l_emit_k(Parser* par, a_u32 dst, Value val, a_line line) {
	a_u32 index = l_const_index(par, val);
	return l_emit(par, bc_make_iabx(BC_K, dst, index), line);
}

static void l_emit_ret(Parser* par, a_u32 base, a_u32 len, a_line line) {
	l_clear_close(par); /* The captures will be close automatically after return. */
	a_insn i;
	switch (len) {
		case 0: {
			i = bc_make_i(BC_RET0);
			break;
		}
		case 1: {
			i = bc_make_ia(BC_RET1, base);
			break;
		}
		case VARARG: {
			i = bc_make_ia(BC_RETM, base);
			break;
		}
		default: {
			i = bc_make_iab(BC_RET, base, len);
			break;
		}
	}
	l_emit_leave(par, i, line);
}

static a_u32 l_emit_kn(Parser* par, a_u32 dst, a_u32 len, a_line line) {
	assume(len > 0);
	if (par->_fnscope->_fpass && !par->_fnscope->_fland) {
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
	return l_emit(par, bc_make_iac(BC_KN, dst, len), line);
}

static a_u32 l_emit_branch(Parser* par, a_insn i, a_u32 label, a_u32 line) {
	a_u32 label2 = l_emit(par, i, line);
	a_i32 diff = label != NO_LABEL ? l_make_jump_diff(par, par->_head_label, label, line) : -1;
	return label2 != NO_LABEL ? l_emit_direct(par, bc_make_isax(BC_J, diff), line) : NO_LABEL;
}

static a_u32 l_emit_refx(Parser* par, a_enum op, a_u32 a, a_u32 b, a_u32 c, a_u32 line) {
	assume(ai_bc_fmts[op] == INSN_iABC && ai_bc_fmts[op + 1] == INSN_iABEx, "bad opcode.");
	if (likely(c <= BC_MAX_C)) {
		return l_emit(par, bc_make_iabc(op, a, b, c), line);
	}
	else {
		a_u32 label = l_emit(par, bc_make_iab(op, a, b), line);
		l_emit_fast(par, bc_make_iax(BC_EX, c), line);
		return label;
	}
}

static void l_merge_branch(Parser* par, a_u32* plabel, a_u32 label2, a_u32 line) {
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

void ai_code_never(unused Parser* par, OutExpr e, a_line line) {
	expr_never(e, line);
}

void ai_code_constK(Parser* par, OutExpr e, a_u32 val, a_line line) {
	assume(val == EXPR_NIL || val == EXPR_FALSE || val == EXPR_TRUE || val == EXPR_UNIT);
	if (l_should_eval(par)) {
		e->_kind = val;
		e->_line = line;
	}
	else {
		expr_never(e, line);
	}
}

void ai_code_constI(Parser* par, OutExpr e, a_int val, a_line line) {
	if (l_should_eval(par)) {
		e->_kind = EXPR_INT;
		e->_int = val;
		e->_line = line;
	}
	else {
		expr_never(e, line);
	}
}

void ai_code_constF(Parser* par, OutExpr e, a_float val, a_line line) {
	if (l_should_eval(par)) {
		e->_kind = EXPR_FLOAT;
		e->_float = val;
		e->_line = line;
	}
	else {
		expr_never(e, line);
	}
}

void ai_code_constS(Parser* par, OutExpr e, GStr* val, a_line line) {
	if (l_should_eval(par)) {
		e->_kind = EXPR_STR;
		e->_str = val;
		e->_line = line;
	}
	else {
		expr_never(e, line);
	}
}

void ai_code_loadfunc(Parser* par, OutExpr e, GProto* fun) {
	FnScope* scope = par->_fnscope;
	a_u16 index = scope->_nsub ++;
	expr_dyn(e, l_emit(par, bc_make_iabx(BC_LDF, DYN, index), fun->_dbg_lndef));
}

static a_u32 l_succ_alloc_stack(Parser* par, a_u32 num, a_u32 line) {
	Scope* scope = par->_scope;
	assume(scope->_num_fur == 0, "cannot allocate register while fragmented section exists.");
	assume(!par->_fnscope->_fvarg, "cannot allocate register when stack is not rebalanced.");
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

static a_bool l_is_in_tmp(Parser* par, a_u32 reg) {
	return reg >= par->_scope->_top_ntr;
}

static void l_check_free_used_stack(Scope* scope) {
	/* Remove critical section if all registers in the section are fully freed. */
	if (unlikely(scope->_num_fur > 0) && scope->_bot_fur + scope->_num_fur == scope->_top_reg) {
		scope->_top_reg = scope->_bot_fur;
		scope->_num_fur = 0;
	}
}

/**
 ** Reallocate the register which is already freed, used for phi operation.
 *@param par the parser.
 *@param reg the register to allocate.
 */
static void l_realloc_stack(Parser* par, a_u32 reg) {
	Scope* scope = par->_scope;
	assume(l_is_in_tmp(par, reg), "cannot reallocate a using register twice.");
	assume(!par->_fnscope->_fvarg, "cannot allocate register when stack is not rebalanced.");
	if (likely(scope->_top_reg == reg)) {
		scope->_top_reg += 1;
		assume(scope->_top_reg <= par->_fnscope->_max_reg);
	}
	else {
		if (reg == scope->_bot_fur) {
			scope->_bot_fur += 1;
		}
		scope->_num_fur -= 1;
		l_check_free_used_stack(scope);
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
	assume(reg < scope->_top_reg && l_is_in_tmp(par, reg));
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
static void l_succ_free_stack(Parser* par, a_u32 reg) {
	Scope* scope = par->_scope;
	assume(l_is_in_tmp(par, reg));

	scope->_top_reg = reg;
	l_check_free_used_stack(scope);
}

/**
 ** Store temporary value in register to a variable in place, the register will
 ** be dropped until leave the scope.
 *@param par the parser.
 *@param reg the register to store.
 */
static void l_store(Parser* par, a_u32 reg) {
	Scope* scope = par->_scope;
	assume(reg >= scope->_bot_fur && reg < scope->_top_reg, "cannot store register in place.");
	scope->_top_ntr = max(reg + 1, scope->_top_ntr);
}

/**
 ** Drop ownership for register if it is a temporary register.
 *@param par the parser.
 *@param e the expression.
 */
static void l_drop_tmp(Parser* par, InExpr e) {
	if (e->_kind == EXPR_TMP) {
		l_free_stack(par, e->_reg);
	}
	else {
		assume(
				e->_kind == EXPR_NEVER ||
				e->_kind == EXPR_VAR ||
				e->_kind == EXPR_VAL, "register expression expected.");
	}
}

/**
 ** Drop ownership for vararg.
 *@param par the parser.
 *@param varg the register list.
 */
static void l_drop_ntmp(Parser* par, InExpr e) {
	if (e->_kind == EXPR_NTMP) {
		l_succ_free_stack(par, e->_reg);
	}
	else {
		assume(e->_kind == EXPR_NEVER, "number of registers expression expected.");
	}
}

static void l_dynR(Parser* par, InoutExpr e);
static void l_topR(Parser* par, InoutExpr e);
static void l_tmpR(Parser* par, InoutExpr e);
static void l_anyR(Parser* par, InoutExpr e);
static void l_anyRK(Parser* par, InoutExpr e);
static void l_fixR(Parser* par, InExpr e, a_u32 reg);
static void l_stbV(Parser* par, InoutExpr e);
static void l_topV(Parser* par, InoutExpr e);
static a_u32 l_condT(Parser* par, InExpr e, a_u32* plabel, a_u32 line);
static a_u32 l_condF(Parser* par, InExpr e, a_u32* plabel, a_u32 line);
static a_u32 l_try(Parser* par, InoutExpr e, a_u32 line);

static a_u32 l_local(Parser* par, Sym* sym) {
	return par->_locals._ptr[par->_fnscope->_local_off + sym->_index]._reg;
}

static a_u32 l_lookup_capture(Parser* par, FnScope* scope, Sym* sym, a_u32 depth) {
	/* Find in captured values. */
	for (a_u32 i = 0; i < scope->_caps._len; ++i) {
		RichCapInfo* info = &scope->_caps._ptr[i];
		if (info->_sym_index == sym->_index) {
			/* Already captured. */
			return i;
		}
	}

	/* Not found, create a new capture value. */
	RichCapInfo info = {
		._scope = sym->_scope,
		._sym_index = sym->_index,
		._mods = sym->_mods,
		._name = sym->_name
	};
	if (sym->_scope >= depth - 1) {
		switch (sym->_kind) {
			case SYM_LOCAL: {
				info._src_index = l_local(par, sym); /* Get variable index. */
				break;
			}
			case SYM_CAPTURE: {
				info._src_index = sym->_index;
				break;
			}
			default: {
				unreachable();
			}
		}
	}
	else { /* Acquire capture index from upper function. */
		info._src_index = l_lookup_capture(par, scope->_fn_up, sym,depth - 1);
	}
	return l_bput(par, scope->_caps, info, 16);
}

static a_u32 l_capture(Parser* par, Sym* sym) {
	return l_lookup_capture(par, par->_fnscope, sym, par->_scope_depth);
}

static void l_load_symbol(Parser* par, OutExpr e, a_u32 id, a_u32 line) {
	Sym* sym = &par->_syms._ptr[id];
	switch (sym->_kind) {
		case SYM_LOCAL: {
			if (par->_scope_depth == sym->_scope) {
				expr_var(e, l_local(par, sym), id, line);
			}
			else {
				e->_kind = EXPR_CAP;
				e->_line = line;
				e->_reg = l_capture(par, sym);
				e->_sym = id;
			}
			break;
		}
		case SYM_CAPTURE: {
			e->_kind = EXPR_CAP;
			e->_line = line;
			e->_reg = l_capture(par, sym);
			e->_sym = id;
			break;
		}
		case SYM_EXPORT: {
			expr_export(e, sym->_name, line);
			break;
		}
		default: unreachable();
	}
}

static void l_resolve_symbol(Parser* par, InoutExpr e) {
	SymBuf* syms = &par->_syms;

	GStr* env_name = ai_env_strx(G(par->_env), STRX_KW__ENV);
	GStr* free_name = e->_str;

	assume(e->_kind == EXPR_EXPORT || e->_kind == EXPR_FREE, "not unresolved symbol.");

	for (a_u32 i = syms->_len; ; --i) {
		assume(i > 0, "_ENV should not be free symbol.");
		a_u32 id = i - 1;
		if (syms->_ptr[id]._name == env_name) {
			l_load_symbol(par, e, id, e->_line);
			break;
		}
	}

	ai_code_lookupS(par, e, free_name, e->_line);
}

/**
 ** Lookup symbol in global scope.
 *@param par the parser.
 *@param e the expression for output.
 *@param name the lookup name.
 *@param line the line number of name reference.
 */
void ai_code_lookupG(Parser* par, OutExpr e, GStr* name, a_line line) {
	SymBuf* syms = &par->_syms;
	for (a_u32 i = syms->_len; i > 0; --i) {
		a_u32 id = i - 1;
		if (syms->_ptr[id]._name == name) {
			l_load_symbol(par, e, id, line);
			return;
		}
	}

	expr_free(e, name, line);
}

void ai_code_lookupS(Parser* par, InoutExpr e, GStr* name, a_line line) {
	switch (e->_kind) {
		case EXPR_NEVER: {
			e->_line = line;
			return;
		}
		case EXPR_CAP: {
			e->_kind = EXPR_REFCK;
			e->_base = e->_reg;
			break;
		}
		default: {
			l_anyR(par, e);
			e->_kind = EXPR_REFK(expr_has_tmp_val(e));
			e->_base = e->_reg;
			break;
		}
	}
	e->_key = l_const_index(par, v_of_obj(name));
	e->_line = line;
}

/**
 ** Make reference of indexed expression.
 *@param par the parser.
 *@param ev the view expression.
 *@param ek the key expression.
 *@param line the line of operation.
 */
void ai_code_index(Parser* par, InoutExpr ev, InExpr ek, a_line line) {
	if (unlikely(ev->_kind == EXPR_NEVER)) {
		expr_never(ev, line);
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
				ev->_kind = EXPR_REFI(expr_has_tmp_val(ev));
				ev->_key = cast(a_u32, val);
			}
			else {
				ev->_kind = EXPR_REF(false, expr_has_tmp_val(ev));
				ev->_key = l_const_index(par, v_of_int(val));
			}
			ev->_base = ev->_reg;
			ev->_line = line;
			break;
		}
		case EXPR_STR: {
			ai_code_lookupS(par, ev, ek->_str, line);
			break;
		}
		default: {
			/* Handle by normal expression. */
			l_anyR(par, ev);
			l_anyR(par, ek);
			if (likely(ev->_kind != EXPR_NEVER)) {
				ev->_kind = EXPR_REF(expr_has_tmp_val(ek), expr_has_tmp_val(ev));
				ev->_base = ev->_reg;
				ev->_key = ek->_reg;
				ev->_line = line;
			}
			break;
		}
	}
}

static void l_negate_branch(Parser* par, a_u32* plabel, a_u32 label, a_line line) {
	if (label + 1 == par->_head_label) {
		/* Try to swap duality opcodes for */
		a_insn* pi = &par->_code[label - 1];

		a_u32 op = bc_load_op(*pi);
		if (bc_is_branch(op)) {
			bc_swap_op(pi, op ^ 1);
			l_mark_label(par, l_next_jump(par, label), line);
			l_redirect(par, label, *plabel, line);
			*plabel = label;
			return;
		}
	}

	*plabel = l_lazy_jump(par, *plabel, line);
	l_mark_label(par, label, line);
}

static void l_instantiate_branch(Parser* par, InoutExpr e, a_u32 reg) {
	assume(par->_head_label == e->_label + 1);
	assume(bc_is_branch(bc_load_op(par->_code[e->_label - 1])));

	a_u32 label = l_next_jump(par, e->_label);
	if (label != NO_LABEL) {
		l_redirect_chain(par, label, e->_label, e->_line);
	}

	/* Swap last instruction. */
	par->_code[par->_head_label - 1] = bc_make_ia(e->_kind == EXPR_TRY_TRUE ? BC_BKF : BC_BKT, reg);
	l_emit(par, bc_make_ia(e->_kind == EXPR_TRY_TRUE ? BC_KT : BC_KF, reg), e->_line);
}

void ai_code_new_list(Parser* par, InoutExpr e, a_line line) {
	expr_dyn(e, l_emit(par, bc_make_iabx(BC_LNEW, DYN, 0), line));
}

void ai_code_new_table(Parser* par, InoutExpr e, a_line line) {
	expr_dyn(e, l_emit(par, bc_make_iabx(BC_HNEW, DYN, 0), line));
}

void ai_code_unary(Parser* par, InoutExpr e, a_enum op, a_line line) {
	switch (op) {
		case OP_NEG: {
			switch (e->_kind) {
				case EXPR_NEVER: {
					break;
				}
				case EXPR_INT: {
					e->_int = ai_op_neg_int(e->_int);
					e->_line = line;
					break;
				}
				case EXPR_FLOAT: {
					e->_float = ai_op_neg_float(e->_float);
					e->_line = line;
					break;
				}
				default: {
					l_anyR(par, e);
					l_drop_tmp(par, e);
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
					e->_int = ai_op_bnot_int(e->_int);
					e->_line = line;
					break;
				}
				default: {
					l_anyR(par, e);
					l_drop_tmp(par, e);
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
					l_drop_tmp(par, e);

					a_u32 label = l_emit_branch(par, bc_make_ia(BC_BNZ, e->_reg), NO_LABEL, line);

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
			l_drop_tmp(par, e);

			e->_kind = EXPR_DYN_AC;
			e->_label = l_emit(par, bc_make_iabc(BC_UNBOX, DYN, e->_reg, 1), line);
			break;
		}
		case OP_LEN: {
			l_anyR(par, e);
			l_drop_tmp(par, e);

			expr_dyn(e, l_emit(par, bc_make_iab(BC_LEN, DYN, e->_reg), line));
			break;
		}
		default: unreachable();
	}
}

void ai_code_binary1(Parser* par, InoutExpr e, a_enum op, a_line line) {
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
		case OP_EQ:
		case OP_NE:
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

static void l_compute_int(Parser* par , a_int a, a_int b, OutExpr e, a_u32 op, a_line line) {
	switch (op) {
		case OP_ADD:
		case OP_SUB:
		case OP_MUL:
		case OP_SHL:
		case OP_SHR:
		case OP_BIT_AND:
		case OP_BIT_OR:
		case OP_BIT_XOR: {
			e->_kind = EXPR_INT;
			e->_int = ai_op_bin_int(a, b, op);
			break;
		}
		case OP_DIV:
		case OP_MOD: {
			if (unlikely(b == 0)) {
				ai_par_error(par, "attempt to divide by 0.", line);
			}
			e->_kind = EXPR_INT;
			e->_int = ai_op_bin_int(a, b, op);
			break;
		}
		case OP_EQ:
		case OP_NE:
		case OP_LT:
		case OP_LE:
		case OP_GT:
		case OP_GE: {
			e->_kind = EXPR_BOOL(ai_op_cmp_int(a, b, op));
			break;
		}
		default: unreachable();
	}
}

static void l_compute_float(a_henv env, a_float a, a_float b, OutExpr e, a_u32 op) {
	quiet(env);
	switch (op) {
		case OP_ADD:
		case OP_SUB:
		case OP_MUL:
		case OP_DIV:
		case OP_MOD: {
			e->_kind = EXPR_FLOAT;
			e->_float = ai_op_bin_float(a, b, op);
			break;
		}
		case OP_EQ:
		case OP_NE:
		case OP_LT:
		case OP_LE:
		case OP_GT:
		case OP_GE: {
			e->_kind = EXPR_BOOL(ai_op_cmp_float(a, b, op));
			break;
		}
		default: unreachable();
	}
}

static a_bool l_fold_const_int(Parser* par, InoutExpr e1, InExpr e2, a_u32 op, a_line line) {
	a_int i1, i2;
	if (expr_are_ints(e1, &i1, e2, &i2)) {
		l_compute_int(par, i1, i2, e1, op, line);
		e1->_line = line;
		return true;
	}
	return false;
}

static a_bool l_fold_const_float(Parser* par, InoutExpr e1, InExpr e2, a_u32 op, a_line line) {
	a_float f1, f2;
	if (expr_are_floats(e1, &f1, e2, &f2)) {
		l_compute_float(par->_env, f1, f2, e1, op);
		e1->_line = line;
		return true;
	}
	return false;
}

static a_bool l_compare_const(Parser* par, InExpr e1, InExpr e2, a_u32 bc1, a_u32 bc2, a_line line) {
	if (e2->_kind == EXPR_INT && e2->_int >= BC_MIN_SBX && e2->_int <= BC_MAX_SBX) {
		l_anyR(par, e1);
		l_drop_tmp(par, e1);
		l_emit(par, bc_make_iasbx(bc1, e1->_reg, e2->_int), line);
		return false;
	}
	else if (e1->_kind == EXPR_INT && e1->_int >= BC_MIN_SBX && e1->_int <= BC_MAX_SBX) {
		l_anyR(par, e2);
		l_drop_tmp(par, e2);
		l_emit(par, bc_make_iasbx(bc2, e2->_reg, e1->_int), line);
		return false;
	}

	return true;
}

static void l_emit_bin(Parser* par, InoutExpr e1, InExpr e2, a_u32 bc, a_line line) {
	l_anyR(par, e2);
	l_anyR(par, e1);
	l_drop_tmp(par, e1);
	l_drop_tmp(par, e2);
	expr_dyn(e1, l_emit(par, bc_make_iabc(bc, DYN, e1->_reg, e2->_reg), line));
}

static void l_compare(Parser* par, InoutExpr e1, InExpr e2, a_u32 bc, a_line line) {
	l_anyR(par, e2);
	l_anyR(par, e1);
	l_drop_tmp(par, e1);
	l_drop_tmp(par, e2);
	l_emit(par, bc_make_iab(bc, e1->_reg, e2->_reg), line);
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
void ai_code_binary2(Parser* par, InoutExpr e1, InExpr e2, a_enum op, a_line line) {
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
				l_drop_tmp(par, e1);
				expr_dyn(e1, l_emit(par, bc_make_iabsc(BC_ADDI + op - OP_ADD, DYN, e1->_reg, e2->_int), line));
			}
			else {
				l_emit_bin(par, e1, e2, BC_ADD + (op - OP_ADD), line);
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
				l_drop_tmp(par, e1);
				expr_dyn(e1, l_emit(par, bc_make_iabsc(BC_SHLI + op - OP_SHL, DYN, e1->_reg, e2->_int), line));
			}
			else {
				l_emit_bin(par, e1, e2, BC_ADD + (op - OP_ADD), line);
			}
			break;
		}
		case OP_EQ: {
			if (l_fold_const_int(par, e1, e2, op, line) || l_fold_const_float(par, e1, e2, op, line))
				return;
			if (l_compare_const(par, e1, e2, BC_BEQI, BC_BEQI, line)) {
				l_compare(par, e1, e2, BC_BEQ, line);
			}

			goto try_true;
		}
		case OP_NE: {
			if (l_fold_const_int(par, e1, e2, op, line) || l_fold_const_float(par, e1, e2, op, line))
				return;
			if (l_compare_const(par, e1, e2, BC_BNEI, BC_BNEI, line)) {
				l_compare(par, e1, e2, BC_BNE, line);
			}

			goto try_true;
		}
		case OP_LT: {
			if (l_fold_const_int(par, e1, e2, op, line) || l_fold_const_float(par, e1, e2, op, line))
				return;
			if (l_compare_const(par, e1, e2, BC_BLTI, BC_BGTI, line)) {
				l_compare(par, e1, e2, BC_BLT, line);
			}
			goto try_true;
		}
		case OP_LE: {
			if (l_fold_const_int(par, e1, e2, op, line) || l_fold_const_float(par, e1, e2, op, line))
				return;
			if (l_compare_const(par, e1, e2, BC_BLEI, BC_BGEI, line)) {
				l_compare(par, e1, e2, BC_BLE, line);
			}
			goto try_true;
		}
		case OP_GT: {
			if (l_fold_const_int(par, e1, e2, op, line) || l_fold_const_float(par, e1, e2, op, line))
				return;
			if (l_compare_const(par, e1, e2, BC_BGTI, BC_BLTI, line)) {
				l_compare(par, e2, e1, BC_BLT, line);
			}
			goto try_true;
		}
		case OP_GE: {
			if (l_fold_const_int(par, e1, e2, op, line) || l_fold_const_float(par, e1, e2, op, line))
				return;
			if (l_compare_const(par, e1, e2, BC_BGEI, BC_BLEI, line)) {
				l_compare(par, e2, e1, BC_BLE, line);
			}
			goto try_true;
		}
		try_true: {
			e1->_kind = EXPR_TRY_TRUE;
			e1->_label = l_emit_jump_direct(par, NO_LABEL, line);
			e1->_line = line;
			break;
		}
		case OP_AND: {
			switch (e1->_kind) {
				case EXPR_NEVER:
				case EXPR_RESIDUAL_FALSE:
					break;
				case EXPR_TRUE: {
					e1->_label = NO_LABEL;
					fallthrough;
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
					e1->_label = NO_LABEL;
					fallthrough;
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
		default: unreachable();
	}
}

static void l_merge_optR(Parser* par, a_u32 label, a_u32 reg, a_line line) {
	a_u32 label2 = l_lazy_jump(par, NO_LABEL, line);
	l_mark_label(par, label, line);
	l_emit_kn(par, reg, 1, line);
	l_mark_label(par, label2, line);
}

void ai_code_merge(Parser* par, InoutExpr e1, InExpr e2, a_u32 label, a_line line) {
	if (e2->_kind == EXPR_NEVER) {
		l_mark_label(par, label, line);
		return;
	}
	switch (e1->_kind) {
		case EXPR_NEVER: {
			assume(label == NO_LABEL);
			expr_copy(e1, e2);
			break;
		}
		case EXPR_DYN_OR_NIL: {
			a_u32 label2 = l_try(par, e2, line);
			l_merge_branch(par, &e1->_residual, label2, line);

			l_tmpR(par, e2);
			a_u32 reg = e2->_reg;
			l_merge_optR(par, e1->_residual, reg, line);

			l_mark_label(par, label, line);
			l_realloc_stack(par, reg);
			expr_tmp(e1, reg, line);
			break;
		}
		case EXPR_TMP_OR_NIL: {
			a_u32 label2 = l_try(par, e2, line);
			l_merge_branch(par, &e1->_residual, label2, line);

			a_u32 reg = e1->_reg;
			l_fixR(par, e2, reg);
			l_merge_optR(par, e1->_residual, reg, line);

			l_mark_label(par, label, line);
			l_realloc_stack(par, reg);
			expr_tmp(e1, reg, line);
			break;
		}
		case EXPR_TMP: {
			a_u32 reg = e1->_reg;
			l_realloc_stack(par, reg);
			l_fixR(par, e2, reg);
			l_mark_label(par, label, line);
			break;
		}
		case EXPR_DYN_A: {
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

void ai_code_monad(Parser* par, InoutExpr e, a_u32* plabel, a_enum op, a_line line) {
	switch (op) {
		case OP_OR_NIL: {
			a_u32 kind = l_condT(par, e, plabel, line);
			if (unlikely(kind == EXPR_RESIDUAL_FALSE)) {
				e->_kind = EXPR_NEVER;
				e->_line = line;
			}
			break;
		}
		case OP_OR_RET: {
			//TODO
			break;
		}
		case OP_OR_ELSE: {
			switch (e->_kind) {
				case EXPR_TMP: {
					l_free_stack(par, e->_reg);
					break;
				}
				case EXPR_TMP_OR_NIL: {
					l_free_stack(par, e->_try);
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
					e->_try = e->_reg;
					e->_residual = label;
					break;
				}
				default: {
					l_dynR(par, e);
							fallthrough;
				}
				case EXPR_DYN_A: {
					e->_kind = EXPR_DYN_OR_NIL;
					e->_try = e->_label;
					e->_residual = label;
					break;
				}
				case EXPR_TMP_OR_NIL:
				case EXPR_DYN_OR_NIL: {
					l_merge_branch(par, &e->_residual, label, line);
					break;
				}
			}
			e->_line = line;
			break;
		}
		default: unreachable();
	}
}

static void l_va_push(Parser* par, InoutExpr es, InExpr e, a_line line) {
	assume(!par->_fnscope->_fvarg, "the vararg must pack before next push.");

	if (es->_kind == EXPR_UNIT) {
		expr_copy(es, e);
	}
	else if (unlikely(es->_kind == EXPR_NEVER)) {
		return;
	}
	else if (es->_kind != EXPR_NTMP) {
		l_topV(par, es);
	}

	switch (e->_kind) {
		case EXPR_NEVER: {
			es->_kind = EXPR_NEVER;
			break;
		}
		case EXPR_UNIT: {
			break;
		}
		case EXPR_DYN_C: {
			a_insn* pi = &par->_code[e->_label];
			a_insn i = *pi;
			assume(bc_load_a(i) == par->_scope->_top_reg, "can not place variable.");

			if (par->_fnscope->_fvarg) {
				bc_swap_op(pi, bc_load_op(i) + 1);
				bc_swap_c(pi, DMB);
			}
			else {
				a_u32 reg = l_alloc_stack(par, line);
				assume(es->_args._top == reg);

				bc_swap_c(pi, 1);
				es->_args._top += 1;
			}
			break;
		}
		case EXPR_DYN_AC: {
			a_insn* pi = &par->_code[e->_label];
			a_insn i = *pi;

			if (par->_fnscope->_fvarg) {
				bc_swap_op(pi, bc_load_op(i) + 1);
				bc_swap_a(pi, es->_args._top);
				bc_swap_c(pi, DMB);
			}
			else {
				a_u32 reg = l_alloc_stack(par, line);
				assume(es->_args._top == reg);

				bc_swap_a(pi, es->_args._top);
				bc_swap_c(pi, 1);
				es->_args._top += 1;
			}
			break;
		}
		default: {
			l_topR(par, e);
			es->_args._top += 1;
			break;
		}
	}
}

void ai_code_vararg1(Parser* par, InoutExpr es, a_enum op, a_line line) {
	switch (op) {
		case OP_VA_PUSH: {
			switch (es->_kind) {
				case EXPR_UNIT:
				case EXPR_NEVER: {
					break;
				}
				default: {
					l_stbV(par, es);
					break;
				}
			}
			break;
		}
		case OP_TNEW: {
			l_topV(par, es);
			l_drop_ntmp(par, es);

			a_insn i;
			Args* args = &es->_args;
			if (par->_fnscope->_fvarg) {
				i = bc_make_iab(BC_TNEWM, DYN, args->_base);
			}
			else {
				i = bc_make_iabc(BC_TNEW, DYN, args->_base, args->_top - args->_base);
			}

			expr_dyn(es, l_emit(par, i, line));
			break;
		}
		case OP_CALL: {
			l_topV(par, es);

			a_insn i;
			Args* args = &es->_args;
			if (par->_fnscope->_fvarg) {
				i = bc_make_iac(BC_CALLM, args->_base, DYN);
				par->_fnscope->_fvarg = false;
			}
			else {
				i = bc_make_iabc(BC_CALL, args->_base, args->_top - args->_base, DYN);
			}

			l_drop_ntmp(par, es);

			es->_kind = EXPR_DYN_C;
			es->_label = l_emit(par, i, line);
			break;
		}
		case OP_RETURN: {
			a_u32 len;
			if (es->_kind == EXPR_VAR) {
				len = 1;
			}
			else {
				l_topV(par, es);

				len = par->_fnscope->_fvarg ? VARARG : par->_scope->_top_reg - es->_reg;

				l_drop_ntmp(par, es);

				par->_fnscope->_fvarg = false;
			}
			l_clear_close(par);
			l_emit_ret(par, es->_reg, len, line);
			break;
		}
		default: unreachable();
	}
}

void ai_code_vararg2(Parser* par, InoutExpr es, InExpr e, a_enum op, a_line line) {
	switch (op) {
		case OP_VA_PUSH: {
			l_va_push(par, es, e, line);
			break;
		}
		case OP_VA_POP: {
			if (es->_kind == EXPR_NTMP) {
				expr_tmp(e, par->_scope->_top_reg - 1, line);
			}
			else {
				assume(es->_kind != EXPR_NTMPC);
				expr_copy(e, es);
				if (es->_kind != EXPR_NEVER) {
					expr_unit(es);
				}
			}
			break;
		}
		default: unreachable();
	}
}

/**
 ** Take expected argument.
 *@param par the parser.
 *@param ei the initial expressions, and become expression pack in result.
 *@param el the last expression, and become the last expression in result.
 *@param n the argument expected.
 *@param line the line number.
 *@return true if success and false for otherwise.
 */
a_u32 ai_code_args_trunc(Parser* par, InoutExpr ei, InoutExpr el, a_u32 n, a_line line) {
	assume(n > 0, "truncate nothing.");
	switch (ei->_kind) {
		case EXPR_NEVER: {
			return 0;
		}
		default: {
			if (par->_fnscope->_fvarg) {
				a_insn* pi = &par->_code[ei->_label];
				a_u32 reg = par->_scope->_top_reg;

				if (ei->_kind == EXPR_DYN_AC) {
					bc_swap_a(pi, reg);
				}
				else {
					assume(bc_load_a(*pi) == reg);
				}
				bc_swap_c(pi, n);

				ei->_kind = EXPR_NTMP;
				ei->_reg = reg;

				expr_tmp(el, reg + n - 1, line);
				return 0;
			}
			else {
				expr_copy(el, ei);
				expr_unit(ei);
				return n - 1;
			}
		}
		case EXPR_NTMP: {
			if (par->_fnscope->_fvarg) {
				a_insn* pi = &par->_code[ei->_label];
				a_u32 reg = par->_scope->_top_reg;

				if (ei->_kind == EXPR_DYN_AC) {
					bc_swap_a(pi, reg);
				}
				else {
					assume(bc_load_a(*pi) == reg);
				}
				bc_swap_c(pi, n);

				expr_tmp(el, reg + n - 1, line);
				return 0;
			}
			else {
				a_u32 m = ei->_args._top - ei->_args._base;
				if (m >= n) {
					ai_code_drop(par, el);

					a_u32 top = ei->_args._base + n;
					l_succ_free_stack(par, top);

					expr_tmp(el, top - 1, line);
					ei->_args._top = top - 1;

					return 0;
				}
				else {
					return n - m - 1;
				}
			}
		}
		case EXPR_NTMPC: {
			l_va_push(par, ei, el, line);
			return VARARG;
		}
	}
}

static a_bool l_try_append(Parser* par, QBuf* buf, InExpr e) {
	switch (e->_kind) {
		case EXPR_INT: {
			l_check_alloc(ai_fmt_nputi(par->_env, buf, e->_int));
			return true;
		}
		case EXPR_FLOAT: {
			l_check_alloc(ai_fmt_nputf(par->_env, buf, e->_float));
			return true;
		}
		case EXPR_STR: {
			GStr* str = e->_str;
			l_check_alloc(ai_buf_nputls(par->_env, buf, str->_data, str->_len));
			return true;
		}
		default: {
			return false;
		}
	}
}

static GStr* buf_to_str(Parser* par, QBuf* buf) {
	GStr* str = ai_lex_to_str(&par->_lex, buf->_ptr, buf->_len);
	ai_buf_reset(buf);
	return str;
}

void ai_code_concat_next(Parser* par, ConExpr* ce, InExpr e, a_line line) {
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
		l_emit_k(par, reg, v_of_obj(str), line);
		if (ce->_expr->_kind != EXPR_NTMP) {
			l_topV(par, ce->_expr);
			ce->_expr->_base = reg;
		}

		ai_buf_reset(&ce->_buf);
	}
	else {
		l_va_push(par, ce->_expr, e, line);
	}
}

void ai_code_concat_end(Parser* par, ConExpr* ce, OutExpr e, a_line line) {
	if (ce->_expr->_kind == EXPR_UNIT) {
		e->_kind = EXPR_STR;
		e->_str = buf_to_str(par, &ce->_buf);
		e->_line = line;
	}
	else {
		if (ce->_buf._len > 0) {
			GStr* str = buf_to_str(par, &ce->_buf);
			a_u32 reg = l_alloc_stack(par, line);
			l_emit_k(par, reg, v_of_obj(str), line);
		}
		expr_dyn(e, l_emit(par, bc_make_iabc(BC_CAT, DYN, ce->_expr->_base, par->_scope->_top_reg - ce->_expr->_base), line));
		l_drop_ntmp(par, ce->_expr);
	}
	/* Check and drop_object string buffer. */
	if (par->_qbq == &ce->_buf) {
		l_bdel(par, ce->_buf);
		par->_qbq = ce->_buf._last;
	}
}

void ai_code_unpack(Parser* par, InoutExpr e, a_line line) {
	assume(!par->_fnscope->_fvarg, "already unpacked.");
	if (e->_kind == EXPR_DYN_C || e->_kind == EXPR_DYN_A) {
		par->_fnscope->_fvarg = true;
	}
	else {
		ai_par_error(par, "cannot unpack expression.", line);
	}
}

static a_insn l_is_leave(Parser* par, a_u32 label) {
	if (unlikely(label == par->_head_label))
		return 0;
	a_insn i = par->_code[label];
	a_u32 op = bc_load_op(i);
	return bc_is_leave(op) ? i : 0;
}

/**
 ** Jump to determined label.
 *@param par the parser.
 *@param label the label jump to.
 *@param line the line number.
 */
void ai_code_gotoD(Parser* par, a_u32 label, a_line line) {
	if (l_should_eval(par)) {
		l_flush_jump(par, line);

		a_insn i = l_is_leave(par, label);
		if (likely(i == 0)) {
			l_flush_land(par, line);
			if (par->_fnscope->_fpass) {
				l_emit_jump_direct(par, label, line);
				par->_fnscope->_fpass = false;
			}
		}
		else {
			l_emit_leave(par, i, line);
		}
	}
}

a_u32 ai_code_gotoU(Parser* par, a_u32 label, a_line line) {
	if (l_should_eval(par)) {
		if (likely(par->_fnscope->_fpass)) {
			if (par->_fnscope->_head_jump == NO_LABEL || label > par->_fnscope->_head_jump) {
				par->_fnscope->_head_jump_line = line;
			}
			par->_fnscope->_fpass = false;
			par->_fnscope->_fjump = true;
			par->_fnscope->_head_jump = label;
			l_merge_branch(par, &par->_fnscope->_head_jump, par->_fnscope->_head_land, line);
			l_clear_land(par);
			return par->_head_label; /* Return next instruction as pseudo label. */
		}
		else {
			assume(par->_fnscope->_fland);
			l_merge_branch(par, &label, par->_fnscope->_head_land, line);
			l_clear_land(par);
			return label;
		}
	}
	return label;
}

a_u32 ai_code_label(Parser* par, a_u32 label, a_line line) {
	if (label != NO_LABEL) {
		if (label != par->_head_label) { /* If from label is not pseudo head label, merge with head jump label. */
			par->_fnscope->_fland = true;
			l_merge_branch(par, &par->_fnscope->_head_land, label, line);
		}
		else {
			/* Pseudo head jump. */
			assume(par->_fnscope->_fjump && !par->_fnscope->_fpass);
			par->_fnscope->_fpass = true;
			par->_fnscope->_fland = par->_fnscope->_head_jump != NO_LABEL;
			l_merge_branch(par, &par->_fnscope->_head_land, par->_fnscope->_head_jump, line);
			l_clear_jump(par);
		}
	}
	return par->_head_label;
}

/**
 ** Force flush control flow.
 *@param par the parser.
 */
void ai_code_flush_jump(Parser* par, a_line line) {
	if (l_should_eval(par)) {
		l_flush_close(par);
		l_flush_jump(par, line);
	}
}

a_u32 ai_code_testT(Parser* par, InoutExpr e, a_line line) {
	a_u32 label = NO_LABEL;
	l_condT(par, e, &label, line);
	return label;
}

void ai_code_drop(Parser* par, InExpr e) {
	switch (e->_kind) {
		case EXPR_DYN_AC: {
			bc_swap_a(&par->_code[e->_label], par->_scope->_top_reg);
			fallthrough;
		}
		case EXPR_DYN_C: {
			bc_swap_c(&par->_code[e->_label], 0);
			break;
		}
		case EXPR_DYN_A: {
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
			if (expr_has_tmp_key(e)) {
				l_free_stack(par, e->_key);
			}
			if (expr_has_tmp_val(e)) {
				l_free_stack(par, e->_base);
			}
			break;
		}
		case EXPR_REFI_ALL:
		case EXPR_REFK_ALL: {
			if (expr_has_tmp_val(e)) {
				l_free_stack(par, e->_base);
			}
			break;
		}
		default: {
			break;
		}
	}
	expr_unit(e);
}

static void l_check_writable(Parser* par, a_u32 id, a_line line) {
	Sym* sym = &par->_syms._ptr[id];
	if (sym->_mods & SYM_MOD_READONLY) {
		ai_par_error(par, "cannot assign to readonly variable %s.", line, str2ntstr(sym->_name));
	}
}

void ai_code_bind(Parser* par, InExpr e1, InExpr e2, a_line line) {
	switch (e1->_kind) {
		case EXPR_VAR: {
			l_check_writable(par, e1->_sym, line);
			l_fixR(par, e2, e1->_reg);
			break;
		}
		case EXPR_CAP: {
			l_anyR(par, e2);
			l_check_writable(par, e1->_sym, line);
			l_emit(par, bc_make_iab(BC_STC, e1->_reg, e2->_reg), line);

			l_drop_tmp(par, e2);
			break;
		}
		case EXPR_REF_ALL: {
			l_anyR(par, e2);
			l_emit(par, bc_make_iabc(BC_SET, e2->_reg, e1->_base, e1->_key), line);

			l_drop_tmp(par, e2);
			ai_code_drop(par, e1);
			break;
		}
		case EXPR_REFI_ALL: {
			l_anyR(par, e2);
			l_emit(par, bc_make_iabc(BC_SETI, e2->_reg, e1->_base, e1->_key), line);

			l_drop_tmp(par, e2);
			ai_code_drop(par, e1);
			break;
		}
		case EXPR_REFK_ALL: {
			l_anyR(par, e2);
			l_emit_refx(par, BC_SETS, e2->_reg, e1->_base, e1->_key, line);

			l_drop_tmp(par, e2);
			ai_code_drop(par, e1);
			break;
		}
		case EXPR_REFCK: {
			l_anyR(par, e2);

			if (l_const_is_istr(par, e1->_key)) {
				l_emit_refx(par, BC_CSETS, e2->_reg, e1->_base, e1->_key, line);
			}
			else {
				a_u32 reg = l_succ_alloc_stack(par, 2, line);
				l_emit(par, bc_make_iab(BC_LDC, reg, e1->_base), line);
				l_emit(par, bc_make_iabx(BC_K, reg + 1, e1->_key), line);
				l_emit_refx(par, BC_SETS, e2->_reg, reg, reg + 1, line);
				l_succ_free_stack(par, reg);
			}

			l_drop_tmp(par, e2);
			break;
		}
		case EXPR_EXPORT: {
			l_resolve_symbol(par, e1);
			ai_code_bind(par, e1, e2, line);
			break;
		}
		case EXPR_FREE: {
			ai_par_error(par, "add 'pub' modifier to export variable '%s'", e1->_line, str2ntstr(e1->_str));
			break;
		}
		default: {
			panic("cannot bind to the expression.");
		}
	}
}

#define NAMES_GROW_UNIT 64

static a_u32 l_push_symbol(Parser* par, Sym sym) {
	return l_bput(par, par->_syms, sym, NAMES_GROW_UNIT);
}

static a_u32 l_push_local(Parser* par, LocalInfo info) {
	a_u32 index = l_bput(par, par->_locals, info, 32);
	return index - par->_fnscope->_local_off;
}

static a_u32 l_bind_local(Parser* par, GStr* name, a_u32 reg, a_u32 begin_label, a_u32 mods) {
	l_store(par, reg);

	a_u32 index = l_push_local(par, new(LocalInfo) {
		._begin_label = begin_label,
		._end_label = NO_LABEL,
		._name = name,
		._reg = reg,
	});

	return l_push_symbol(par, new(Sym) {
		._kind = SYM_LOCAL,
		._scope = par->_scope_depth,
		._mods = mods,
		._index = index,
		._name = name
	});
}

void ai_code_let_nils(Parser* par, LetStat* s, a_line line) {
	Scope* scope = par->_scope;

	/* If all node is bind. */
	if (s->_root._sibling == null)
		return;

	if (s->_root._faggr) {
		ai_par_error(par, "nil binding is only available for plain pattern.", line);
	}

	assume(scope->_top_ntr == scope->_top_reg);
	a_u32 num = s->_root._count - s->_root._sibling->_index;
	a_u32 reg = l_succ_alloc_stack(par, num, line);
	a_u32 label = l_emit_kn(par, reg, num, line);
	for (LetNode* node = s->_root._sibling; node != null; node = node->_sibling) {
		l_bind_local(par, node->_expr->_str, reg++, label, SYM_MOD_NONE);
	}

	scope->_top_ntr = scope->_top_reg;
}

static void l_let_bind(Parser* par, LetStat* s, LetNode* n, InExpr e, a_u32 base) {
	a_u32 reg = base + n->_abs_bot + n->_tmp_pos;
	switch (n->_kind) {
		case PAT_DROP: {
			ai_code_drop(par, e);
			break;
		}
		case PAT_BIND: {
			l_fixR(par, e, reg);
			l_bind_local(par, n->_expr->_str, reg, par->_head_label, SYM_MOD_NONE);
			break;
		}
		case PAT_TUPLE: {
			a_u32 line = n->_expr->_line;
			a_u32 num = n->_count;
			l_emit(par, bc_make_iabc(BC_UNBOX, reg, e->_reg, num), line);
			for (LetNode* nchild = n->_child; nchild != null; nchild = nchild->_sibling) {
				Expr e2;
				expr_val(e2, reg++, line);
				l_let_bind(par, s, nchild, e2, base);
			}
			break;
		}
		default: {
			unreachable();
		}
	}
}

static void l_let_compute(unused Parser* par, LetNode* const nrt) {
	LetNode* n = nrt;
	a_u32 abs_top = 0;
	loop {
		n->_abs_bot = abs_top;
		n->_tmp_pos = 0;
		switch (n->_kind) {
			case PAT_BIND: {
				n->_tmp_top = 1;
				abs_top += 1;
				break;
			}
			case PAT_DROP: {
				n->_tmp_top = 1;
				break;
			}
			case PAT_TUPLE: {
				n->_tmp_top = 0;
				if (n->_child != null) {
					n = n->_child;
					continue;
				}
				break;
			}
			default: {
				unreachable();
			}
		}

		if (n == nrt)
			return;

		while (n->_sibling == null) {
			LetNode* nup = n->_parent;

			a_u32 used = (n->_abs_bot - nup->_abs_bot) + n->_tmp_pos;
			nup->_tmp_pos = used > n->_index ? max(nup->_tmp_pos, used - n->_index) : nup->_tmp_pos;
			nup->_tmp_top = max(nup->_tmp_pos + nup->_count, (n->_abs_bot - nup->_abs_bot) + n->_tmp_top);

			n = nup;

			if (n == nrt) {
				return;
			}
		}

		LetNode* nup = n->_parent;
		nup->_tmp_pos = max(nup->_tmp_pos, (n->_abs_bot - nup->_abs_bot) + n->_tmp_top - n->_index - 1);
		n = n->_sibling;
	}
}

a_bool ai_code_let_bind(Parser* par, LetStat* s, InExpr e) {
	LetNode* node = s->_root._child;

	assume(node != null);

	l_let_compute(par, node);

	if (e->_kind == EXPR_TMP) {
		a_u32 reg = e->_reg;
		l_free_stack(par, reg);
		expr_val(e, reg, e->_line);
	}
	a_u32 reg = l_succ_alloc_stack(par, node->_tmp_top, e->_line);
	if (e->_kind != EXPR_VAL) {
		l_fixR(par, e, reg);
		expr_val(e, reg, e->_line);
	}
	l_let_bind(par, s, node, e, reg);

	node = node->_sibling;
	s->_root._child = node;
	return node != null;
}

void ai_code_local(Parser* par, OutExpr e, GStr* name, a_line line) {
	Scope* scope = par->_scope;
	assume(scope->_top_ntr == scope->_top_reg, "stack not balanced.");
	a_u32 reg = l_alloc_stack(par, line);
	a_u32 sym = l_bind_local(par, name, reg, par->_head_label, SYM_MOD_NONE);
	expr_var(e, reg, sym, line);
	scope->_top_ntr = scope->_top_reg;
}

void ai_code_export(Parser* par, OutExpr e, GStr* name, a_line line) {
	Scope* scope = par->_scope;
	assume(scope->_top_ntr == scope->_top_reg, "stack not balanced.");
	l_push_symbol(par, new(Sym) {
		._kind = SYM_EXPORT,
		._name = name,
		._scope = par->_scope_depth,
		._mods = SYM_MOD_NONE
	});
	expr_export(e, name, line);
}

void ai_code_bind_param(Parser* par, GStr* name) {
	FnScope* scope = par->_fnscope;
	assume(&scope->_scope == par->_scope);
	assume(scope->_top_ntr == scope->_top_reg);

	a_u32 reg = l_alloc_stack(par, scope->_begin_line);
	l_bind_local(par, name, reg, scope->_begin_label, SYM_MOD_NONE);
	scope->_top_ntr = scope->_top_reg;

	assume(reg == scope->_nparam);
	scope->_nparam += 1;
}

/**
 ** Compact fragment section.
 *@param par the parser.
 */
void ai_code_compact(Parser* par) {
	Scope* scope = par->_scope;
	assume(scope->_top_reg == scope->_top_ntr, "some temporary register is not freed.");
	scope->_num_fur = 0;
}

static void scope_push(Parser* par, Scope* scope, a_u32 reg, a_line line) {
	*scope = new(Scope) {
		._up = par->_scope,
		._bot_reg = reg,
		._top_reg = reg,
		._bot_fur = reg,
		._num_fur = 0,
		._begin_line = line,
		._begin_label = par->_head_label,
		._end_label = NO_LABEL,
		._sym_off = par->_syms._len
	};
	par->_scope = scope;
}

static void scope_pop(Parser* par, a_line line) {
	Scope* scope = par->_scope;
	Scope* up = scope->_up;
	par->_scope = up;
	run {
		a_u32 bot = scope->_sym_off;
		a_u32 top = par->_syms._len;
		a_u32 label = par->_head_label;
		for (a_u32 i = bot; i < top; ++i) {
			Sym* sym = &par->_syms._ptr[i];
			switch (sym->_kind) {
				case SYM_LOCAL:
					par->_locals._ptr[par->_fnscope->_local_off + sym->_index]._end_label = label;
					break;
				default:
					unreachable();
			}
		}
		par->_syms._len = bot;
	}
	scope->_end_label = l_mark_label(par, scope->_end_label, line);
	if (up != null && scope->_top_ntr != up->_top_ntr) {
		par->_fnscope->_fclose = true;
		par->_fnscope->_close_line = line;
	}
}

void ai_code_enter(Parser* par, Scope* scope, a_line line) {
	scope_push(par, scope, par->_scope->_top_reg, line);
}

void ai_code_leave(Parser* par, a_line line) {
	assume(par->_scope->_up != null);
	scope_pop(par, line);
}

void ai_code_prologue(Parser* par, FnScope* fnscope, a_line line) {
	if (par->_scope_depth == UINT8_MAX) {
		ai_par_error(par, "function nested level overflow.", line);
	}

	if (par->_fnscope != null) {
		par->_fnscope->_top_scope = par->_scope;
	}

	par->_scope = null;

	*fnscope = new(FnScope) {
		._fn_up = par->_fnscope,
		._base_subs = cast(GProto**, par->_rq._tail),
		._line_off = par->_lines._len,
		._local_off = par->_locals._len,
		._head_jump = NO_LABEL,
		._head_land = NO_LABEL,
		._fpass = true,
		._fland = false,
		._fjump = false
	};
	scope_push(par, &fnscope->_scope, 0, line);

	par->_fnscope = fnscope;
	par->_scope_depth += 1;
}

GProto* ai_code_epilogue(Parser* par, GStr* name, a_bool root, a_line line) {
	FnScope* scope = par->_fnscope;

	l_emit_ret(par, DMB, 0, line);

	scope_pop(par, line);

	ProtoDesc desc = {
		._nconst = par->_consts._len - scope->_const_off,
		._ninsn = par->_head_label - scope->_begin_label,
		._nsub = scope->_nsub,
		._nlocal = par->_locals._len - scope->_local_off,
		._ncap = scope->_caps._len,
		._nstack = scope->_max_reg,
		._nline = par->_lines._len - scope->_line_off,
		._flags = {
			._fdebug = (par->_options & ALO_COMP_OPT_STRIP_DEBUG) == 0,
			._froot = root
		}
	};

	GProto* proto = ai_proto_xalloc(par->_env, &desc);
	if (proto == null) {
		ai_mem_nomem(par->_env);
	}

	memcpy(proto->_consts, par->_consts._ptr + scope->_const_off, sizeof(Value) * desc._nconst);
	memcpy(proto->_code, par->_code + scope->_begin_label, sizeof(a_insn) * desc._ninsn);
	if (desc._flags._fdebug) {
		proto->_dbg_lndef = scope->_begin_line;
		proto->_dbg_lnldef = line;
		memcpy(proto->_dbg_lines, par->_lines._ptr + scope->_line_off, sizeof(LineInfo) * desc._nline);
		memcpy(proto->_dbg_locals, par->_locals._ptr + scope->_local_off, sizeof(LocalInfo) * desc._nlocal);
	}
	run {
		for (a_u32 i = 0; i < desc._ncap; ++i) {
			RichCapInfo* cap_info = &scope->_caps._ptr[i];
			proto->_caps[i] = new(CapInfo) {
				._reg = cap_info->_src_index,
				._fup = cap_info->_scope != par->_scope_depth
			};
			if (desc._flags._fdebug) {
				proto->_dbg_cap_names[i] = cap_info->_name;
			}
		}
	}
	run { /* Build sub function */
		GProto** dst = proto->_subs;
		GProto** end = cast(GProto**, par->_rq._tail);
		for (GProto** src = scope->_base_subs; src != end;
				src = cast(GProto**, &(*src)->_gnext),
				dst += 1) {
			*dst = *src;
		}
	}
	proto->_name = name;
	if (desc._flags._fdebug) {
		proto->_dbg_file = par->_lex._file;
		proto->_dbg_lndef = scope->_begin_line;
		proto->_dbg_lnldef = line;
	}

	l_bdel(par, scope->_caps);

	FnScope* up_scope = scope->_fn_up;

	par->_fnscope = up_scope;
	par->_scope = up_scope != null ? up_scope->_top_scope : null;
	par->_scope_depth -= 1;
	par->_head_label = scope->_begin_label;
	par->_locals._len = scope->_local_off;
	par->_lines._len = scope->_line_off;
	par->_rq._tail = cast(a_gclist*, scope->_base_subs);

	rq_push(&par->_rq, proto);

	return proto;
}

static void l_del_proto(a_henv env, GProto* proto) {
	for (a_u32 i = 0; i < proto->_nsub; ++i) {
		l_del_proto(env, proto->_subs[i]);
	}
	ai_proto_drop(G(env), proto);
}

static void parser_close(Parser* par) {
	l_bdel(par, par->_insns);
	l_bdel(par, par->_consts);
	l_bdel(par, par->_lines);
	l_bdel(par, par->_locals);
	l_bdel(par, par->_syms);
	ai_lex_close(&par->_lex);

	ai_env_gprotect_clear(par->_env);
}

static void parser_mark(Global* g, void* ctx) {
	Parser* par = ctx;
	run {
		LexStrs* strs = &par->_lex._strs;
		for (a_u32 i = 0; i <= strs->_hmask; ++i) {
			StrNode* node = &strs->_ptr[i];
			if (node->_str != null) {
				ai_gc_trace_mark(g, node->_str);
			}
		}
	}
}

static void parser_except(a_henv env, void* ctx, unused a_msg msg) {
	Parser* par = ctx;
	assume(env == par->_env);
	/* Destroy queued prototypes. */
	rq_for(obj, &par->_rq) { l_del_proto(par->_env, g_cast(GProto, obj)); }
	/* Close linked buffers. */
	for (QBuf* qb = par->_qbq; qb != null; qb = qb->_last) {
		l_bdel(par, *qb);
	}
	for (FnScope* scope = par->_fnscope; scope != null; scope = scope->_fn_up) {
		l_bdel(par, scope->_caps);
	}
	/* Close parser. */
	parser_close(par);
}

void ai_code_open(Parser* par) {
	ai_env_gprotect(par->_env, parser_mark, parser_except, par);

	/* Add predefined '_ENV' name. */
	l_push_symbol(par, new(Sym) {
		._kind = SYM_CAPTURE,
		._scope = SCOPE_DEPTH_ENV,
		._mods = SYM_MOD_READONLY /* Predefined environment is always readonly variable. */,
		._index = 0,
		._name = ai_env_strx(G(par->_env), STRX_KW__ENV)
	});
}

static void l_register_proto(a_henv env, GProto* proto) {
	assume(proto->_gnext == null, "duplicate root function.");
	ai_gc_register_object(env, proto);
	for (a_u32 i = 0; i < proto->_nsub; ++i) {
		l_register_proto(env, proto->_subs[i]);
	}
}

GFun* ai_code_build_and_close(Parser* par) {
	GProto* proto = g_cast(GProto, par->_rq._head); /* Get root prototype. */
	l_register_proto(par->_env, proto);
	parser_close(par);
	GFun* fun = proto->_cache;
	g_set_white(G(par->_env), gobj_cast(fun));
	return fun;
}

static a_none l_error_unit_expr(Parser* par, a_u32 line) {
	ai_par_error(par, "bad use of unit.", line);
}

static void l_dynR(Parser* par, InoutExpr e) {
	switch (e->_kind) {
		case EXPR_UNIT: {
			l_error_unit_expr(par, e->_line);
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
			expr_dyn(e, l_emit_k(par, DYN, v_of_obj(e->_str), e->_line));
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
		case EXPR_FREE: {
			l_resolve_symbol(par, e);
			l_dynR(par, e);
			break;
		}
		case EXPR_TRY_FALSE:
		case EXPR_TRY_TRUE: {
			a_u32 reg = l_alloc_stack(par, e->_line);
			l_instantiate_branch(par, e, reg);
			expr_dyn(e, l_emit_mov(par, DYN, reg, e->_line));
			break;
		}
		case EXPR_TMP_OR_NIL: {
			e->_kind = EXPR_DYN_OR_NIL;
			e->_try = l_emit_mov(par, DYN, e->_try, e->_line);
			break;
		}
		case EXPR_REF_ALL: {
			if (expr_has_tmp_key(e)) {
				l_free_stack(par, e->_key);
			}
			if (expr_has_tmp_val(e)) {
				l_free_stack(par, e->_base);
			}
			expr_dyn(e, l_emit(par, bc_make_iabc(BC_GET, DYN, e->_base, e->_key), e->_line));
			ai_code_drop(par, e);
			break;
		}
		case EXPR_REFI(true): {
			l_free_stack(par, e->_base);
			fallthrough;
		}
		case EXPR_REFI(false): {
			expr_dyn(e, l_emit(par, bc_make_iabc(BC_GETI, DYN, e->_base, e->_key), e->_line));
			break;
		}
		case EXPR_REFK(true): {
			l_free_stack(par, e->_base);
			fallthrough;
		}
		case EXPR_REFK(false): {
			a_u32 k = e->_key;
			if (v_is_istr(par->_consts._ptr[par->_fnscope->_const_off + k])) {
				expr_dyn(e, l_emit_refx(par, BC_GETS, DYN, e->_base, k, e->_line));
			}
			else {
				a_u32 reg = l_alloc_stack(par, e->_line);
				l_emit(par, bc_make_iabx(BC_K, reg, k), e->_line);
				l_emit_fast(par, bc_make_iabc(BC_GET, DYN, e->_base, reg), e->_line);
				l_free_stack(par, reg);
			}
			break;
		}
		case EXPR_REFCK: {
			a_u32 k = e->_key;
			if (l_const_is_istr(par, k)) {
				expr_dyn(e, l_emit_refx(par, BC_CGETS, DYN, e->_base, k, e->_line));
			}
			else {
				a_u32 reg = l_succ_alloc_stack(par, 2, e->_line);
				l_emit(par, bc_make_iabx(BC_K, reg, k), e->_line);
				l_emit_fast(par, bc_make_iab(BC_LDC, reg + 1, e->_base), e->_line);
				l_emit_fast(par, bc_make_iabc(BC_GET, DYN, reg + 1, reg), e->_line);
				l_succ_free_stack(par, reg);
			}
			break;
		}
		case EXPR_DYN_C: {
			a_insn* pi = &par->_code[e->_label];
			bc_swap_c(pi, 1);
			expr_dyn(e, l_emit_mov(par, DYN, bc_load_a(*pi), e->_line));
			break;
		}
		case EXPR_DYN_AC: {
			bc_swap_c(&par->_code[e->_label], 1);
			e->_kind = EXPR_DYN_A;
			break;
		}
		default: {
			break;
		}
	}
}

static void l_movR(Parser* par, InoutExpr e, a_u32 reg) {
	switch (e->_kind) {
		case EXPR_DYN_AC: {
			bc_swap_c(&par->_code[e->_label], 1);
			fallthrough;
		}
		case EXPR_DYN_A: {
			bc_swap_a(&par->_code[e->_label], reg);
			break;
		}
		case EXPR_TMP_OR_NIL: {
			a_u32 reg2 = e->_try;
			if (reg == reg2) {
				l_merge_optR(par, e->_residual, reg, e->_line);
				break;
			}
			l_free_stack(par, reg2);
			e->_try = l_emit_mov(par, DYN, reg2, e->_line);
			fallthrough;
		}
		case EXPR_DYN_OR_NIL: {
			bc_swap_a(&par->_code[e->_try], reg);
			l_merge_optR(par, e->_residual, reg, e->_line);
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
			if (e->_try != reg) {
				l_free_stack(par, e->_try);
				l_emit_mov(par, reg, e->_try, e->_line);
			}
			l_merge_optR(par, e->_residual, reg, e->_line);
			expr_tmp(e, reg, e->_line);
			break;
		}
		case EXPR_VAR:
		case EXPR_TMP:
		case EXPR_VAL: {
			if (e->_reg != reg) {
				l_drop_tmp(par, e);
				l_emit_mov(par, reg, e->_try, e->_line);
			}
			e->_reg = reg;
			break;
		}
		default: {
			l_dynR(par, e);
			l_movR(par, e, reg);
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
			expr_tmp(e, reg, e->_line);
			break;
		}
		case EXPR_VAR: {
			a_u32 reg = l_alloc_stack(par, e->_line);
			l_emit_mov(par, reg, e->_reg, e->_line);
			expr_tmp(e, reg, e->_line);
			break;
		}
		case EXPR_TMP: {
			l_free_stack(par, e->_reg);
			fallthrough;
		}
		case EXPR_VAL: {
			a_u32 reg1 = e->_reg;
			a_u32 reg2 = l_alloc_stack(par, e->_line);
			if (reg1 != reg2) {
				l_emit_mov(par, reg2, reg1, e->_line);
				e->_reg = reg2;
			}
			break;
		}
		case EXPR_TRY_TRUE:
		case EXPR_TRY_FALSE: {
			a_u32 reg = l_alloc_stack(par, e->_line);
			l_instantiate_branch(par, e, reg);
			expr_tmp(e, reg, e->_line);
			break;
		}
		default: {
			l_dynR(par, e);
			fallthrough;
		}
		case EXPR_DYN_A: {
			l_movR(par, e, l_alloc_stack(par, e->_line));
			break;
		}
		case EXPR_DYN_AC: {
			a_u32 reg = bc_load_a(par->_code[e->_label]);
			assume(par->_scope->_top_reg == reg, "vararg are not in the top.");
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
			a_u32 reg = e->_try;
			l_merge_optR(par, e->_residual, reg, e->_line);
			expr_tmp(e, reg, e->_line);
			break;
		}
		case EXPR_DYN_C: {
			a_insn* pc = &par->_code[e->_label];
			a_u32 reg = bc_load_a(*pc);
			bc_swap_c(pc, 1);
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

static void l_stbV(Parser* par, InoutExpr e) {
	if (par->_fnscope->_fvarg) {
		switch (e->_kind) {
			case EXPR_DYN_C: {
				a_insn* pi = &par->_code[e->_label];
				a_u32 reg = bc_load_a(*pi);

				bc_swap_op(pi, bc_load_op(*pi) + 1);
				bc_swap_c(pi, DMB);

				l_emit(par, bc_make_iab(BC_LBOXM, reg, reg), e->_line);

				e->_kind = EXPR_NTMPC;
				e->_args = new(Args) { ._base = reg, ._top = reg + 1, ._coll = reg };
				break;
			}
			case EXPR_DYN_AC: {
				a_insn* pi = &par->_code[e->_label];
				a_u32 reg = par->_scope->_top_reg;

				bc_swap_op(pi, bc_load_op(*pi) + 1);
				bc_swap_a(pi, reg);
				bc_swap_c(pi, DMB);

				l_emit(par, bc_make_iab(BC_LBOXM, reg, reg), e->_line);

				e->_kind = EXPR_NTMPC;
				e->_args = new(Args) { ._base = reg, ._top = reg + 1, ._coll = reg };
				break;
			}
			case EXPR_NTMP: {
				a_u32 bot = e->_args._base;

				l_emit(par, bc_make_iab(BC_LBOXM, bot, bot), e->_line);

				e->_kind = EXPR_NTMPC;
				e->_args._coll = bot;
				break;
			}
			case EXPR_NTMPC: {
				a_u32 col = e->_args._coll;
				a_u32 bot = col + 1;

				l_emit(par, bc_make_iab(BC_LPUSHM, col, bot), e->_line);
				l_succ_free_stack(par, bot);

				break;
			}
			default: {
				panic("bad expression for vararg.");
			}
		}
	}
	else if (e->_kind != EXPR_UNIT) {
		l_topV(par, e);
	}
}

static void l_topV(Parser* par, InoutExpr e) {
	if (par->_fnscope->_fvarg) {
		switch (e->_kind) {
			case EXPR_DYN_C: {
				a_insn* pi = &par->_code[e->_label];
				a_u32 reg = bc_load_a(*pi);

				bc_swap_op(pi, bc_load_op(*pi) + 1);
				bc_swap_c(pi, DMB);

				e->_kind = EXPR_NTMP;
				e->_args = new(Args) { ._base = reg, ._top = reg + 1 };
				break;
			}
			case EXPR_DYN_AC: {
				a_insn* pi = &par->_code[e->_label];
				a_u32 reg = par->_scope->_top_reg;

				bc_swap_op(pi, bc_load_op(*pi) + 1);
				bc_swap_a(pi, reg);
				bc_swap_c(pi, DMB);

				e->_kind = EXPR_NTMP;
				e->_args = new(Args) { ._base = reg, ._top = reg + 1 };
				break;
			}
			case EXPR_NTMP: {
				break;
			}
			case EXPR_NTMPC: {
				a_u32 col = e->_args._coll;
				a_u32 bot = col + 1;
				a_u32 top = par->_scope->_top_reg;

				if (bot < top) {
					l_emit(par, bc_make_iab(BC_LPUSHM, bot, bot), e->_line);
					l_succ_free_stack(par, bot);
				}

				l_emit(par, bc_make_iab(BC_UNBOXV, e->_reg, e->_reg), e->_line);

				e->_kind = EXPR_NTMP;
				break;
			}
			default: {
				panic("bad expression for vararg.");
			}
		}
	}
	else {
		switch (e->_kind) {
			case EXPR_UNIT: {
				e->_kind = EXPR_NTMP;
				e->_reg = par->_scope->_top_reg;
				break;
			}
			case EXPR_NEVER:
			case EXPR_NTMP: {
				break;
			}
			case EXPR_NTMPC: {
				a_u32 bot = e->_reg + 1;
				a_u32 top = par->_scope->_top_reg;

				if (bot < top) {
					l_emit(par, bc_make_iabc(BC_LPUSH, e->_reg, bot, top - bot), e->_line);
					l_succ_free_stack(par, bot);
				}

				l_emit(par, bc_make_iab(BC_UNBOXV, e->_reg, e->_reg), e->_line);

				par->_fnscope->_fvarg = true;
				e->_kind = EXPR_NTMP;
				break;
			}
			default: {
				l_topR(par, e);
				e->_kind = EXPR_NTMP;
				e->_args = new(Args) { ._base = e->_reg, ._top = e->_reg + 1 };
				break;
			}
		}
	}
}

static a_u32 l_condT(Parser* par, InExpr e, a_u32* plabel, a_u32 line) {
	switch (e->_kind) {
		case EXPR_NEVER:
		case EXPR_TRUE:
		case EXPR_INT:
		case EXPR_FLOAT:
		case EXPR_STR: {
			return EXPR_TRUE;
		}
		case EXPR_NIL:
		case EXPR_FALSE: {
			*plabel = l_lazy_jump(par, *plabel, line);
			return EXPR_RESIDUAL_FALSE;
		}
		case EXPR_TRY_TRUE: {
			l_merge_branch(par, plabel, e->_label, line);
			return EXPR_TRY_TRUE;
		}
		case EXPR_TRY_FALSE: {
			l_negate_branch(par, plabel, e->_label, line);
			return EXPR_TRY_TRUE;
		}
		default: {
			l_anyR(par, e);
			assume(e->_kind == EXPR_TMP || e->_kind == EXPR_VAR);
			*plabel = l_emit_branch(par, bc_make_ia(BC_BZ, e->_reg), *plabel, line);
			return EXPR_TRY_TRUE;
		}
	}
}

static a_u32 l_condF(Parser* par, InExpr e, a_u32* plabel, a_u32 line) {
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
			l_merge_branch(par, plabel, e->_label, line);
			return EXPR_TRY_FALSE;
		}
		case EXPR_TRY_TRUE: {
			l_negate_branch(par, plabel, e->_label, line);
			return EXPR_TRY_FALSE;
		}
		default: {
			l_anyR(par, e);
			*plabel = l_emit_branch(par, bc_make_ia(BC_BNZ, e->_reg), *plabel, line);
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
		case EXPR_DYN_OR_NIL: {
			a_u32 label = e->_residual;
			expr_dyn(e, e->_try);
			return label;
		}
		case EXPR_TMP_OR_NIL: {
			a_u32 label = e->_residual;
			expr_tmp(e, e->_try, line);
			return label;
		}
		default: {
			return NO_LABEL;
		}
	}
}