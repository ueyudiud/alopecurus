/**
 *@file aparse.c
 */

#define aparse_c_
#define ALO_LIB

#include "afun.h"
#include "aenv.h"
#include "aerr.h"
#include "acode.h"

#include "aparse.h"

#define lex(par) (&(par)->_lex)
#define ln_cur(par) (lex(par)->_current._line)
#define tk_cur(par) (&lex(par)->_current)

always_inline a_i32 l_peek(Parser* par) {
	return ai_lex_peek(lex(par));
}

always_inline void l_sync(Parser* par) {
	quiet(l_peek(par));
}

always_inline void l_skip(Parser* par) {
	tk_cur(par)->_tag = TK__NONE;
}

always_inline a_bool l_test(Parser* par, a_i32 tk) {
	return l_peek(par) == tk;
}

always_inline a_i32 l_forward(Parser* par) {
	return ai_lex_forward(lex(par));
}

static a_bool l_test_skip(Parser* par, a_i32 tk) {
	if (l_test(par, tk)) {
		l_skip(par);
		return true;
	}
	return false;
}

a_none ai_par_report(Parser* par, a_bool eof, char const* fmt, ...) {
	if (!eof || !(par->_options & ALO_COMP_OPT_ALOC1)) {
		va_list varg;
		va_start(varg, fmt);
		ai_err_raisevf(par->_env, ALO_ECHUNK, fmt, varg);
		va_end(varg);
	}
	else {
		ai_env_raise(par->_env, ALO_ESTMUF);
	}
}

#define l_error_got(par,fmt,args...) ({ \
	a_tkbuf _buf; \
    ai_par_report( \
		par, \
		tk_cur(par)->_tag == TK_EOF, \
		par_err_fl_arg(par, fmt", got %s", ln_cur(par)), \
		##args, \
		ai_lex_tkrepr(tk_cur(par), _buf)); \
})

static void l_error_expected(Parser* par, a_i32 tk) {
	l_error_got(par, "%s expected", ai_lex_tagname(tk));
}

static void l_check_skip(Parser* par, a_i32 tk) {
	if (!l_test(par, tk)) {
		l_error_expected(par, tk);
	}
	l_skip(par);
}

static GStr* l_check_ident(Parser* par) {
	if (!l_test(par, TK_IDENT)) {
		l_error_expected(par, TK_IDENT);
	}
	GStr* str = tk_cur(par)->_str;
	l_skip(par);
	return str;
}

static a_none l_error_bracket(Parser* par, a_i32 ltk, a_i32 rtk, a_line line) {
	if (ln_cur(par) == line) {
		l_error_got(par, "%s expected to match %s",
					ai_lex_tagname(ltk),
					ai_lex_tagname(rtk));
	}
	else {
		l_error_got(par, "%s expected to match %s at line %u",
					ai_lex_tagname(ltk),
					ai_lex_tagname(rtk),
					line);
	}
}

static void l_check_pair_right(Parser* par, a_i32 ltk, a_i32 rtk, a_line line) {
	if (!l_test(par, rtk))
		l_error_bracket(par, ltk, rtk, line);
	l_skip(par);
}

static a_bool l_test_sep(Parser* par) {
	switch (l_peek(par)) {
		case TK_RBK:
		case TK_RBR:
		case TK_RSQ:
		case TK_EOF:
		case TK_SEMI:
			return true;
		default:
			return false;
	}
}

static void l_scan_atom_expr(Parser* par, OutExpr e);
static void l_scan_expr(Parser* par, OutExpr e);
static void l_scan_exprs(Parser* par, InoutExpr es, a_bool exists);
static void l_scan_stat(Parser* par);
static void l_scan_stats(Parser* par);

static void l_scan_tstring(Parser* par, OutExpr e) {
	LexScope scope;
	a_bool stop = false;

	ConExpr ce = {};
	ai_code_constS(par, e, tk_cur(par)->_str, ln_cur(par));
	ai_code_concat_next(par, &ce, e, ln_cur(par));
	l_skip(par);

	while (!stop) {
		a_u32 line = ln_cur(par);
		switch (l_peek(par)) {
			case TK_TSTRING: {
				ai_code_constS(par, e, tk_cur(par)->_str, line);
				l_skip(par);
				break;
			}
			case TK_STRING: {
				ai_code_constS(par, e, tk_cur(par)->_str, line);
				l_skip(par);
				stop = true;
				break;
			}
			case TK_LBR: {
				l_skip(par);
				ai_lex_push_scope(&par->_lex, &scope);
				l_scan_expr(par, e);
				ai_lex_pop_scope(&par->_lex);
				l_check_pair_right(par, TK_LBR, TK_RBR, line);
				break;
			}
			default: {
				l_scan_atom_expr(par, e);
				break;
			}
		}
		ai_code_concat_next(par, &ce, e, line);
	}

	ai_code_concat_end(par, &ce, e, ln_cur(par));
}

static void l_scan_fun_args(Parser* par, a_i32 ltk, a_i32 rtk) {
	l_check_skip(par, ltk);

	a_u32 line2 = ln_cur(par);
	if (!l_test_skip(par, rtk)) {
		do {
			GStr* name = l_check_ident(par);
			if (l_test_skip(par, TK_TDOT)) {
				//TODO
				panic("unimplemented");
				break;
			}
			ai_code_bind_param(par, name);
		}
		while (l_test_skip(par, TK_COMMA));
		l_check_pair_right(par, ltk, rtk, line2);
	}
}

static void l_scan_function(Parser* par, OutExpr e, GStr* name, a_line line) {
	FnScope scope;

	ai_code_prologue(par, &scope, line);

	l_scan_fun_args(par, TK_LBK, TK_RBK);

	l_check_skip(par, TK_LBR);
	line = ln_cur(par);

	l_scan_stats(par);
	l_check_pair_right(par, TK_LBR, TK_RBR, line);

	GProto* meta = ai_code_epilogue(par, name, false, ln_cur(par));
	ai_code_loadfunc(par, e, meta);
}

static void l_scan_lambda(Parser* par, OutExpr e) {
	FnScope scope;

	ai_code_prologue(par, &scope, ln_cur(par));

	if (!l_test_skip(par, TK_BBAR)) {
		l_scan_fun_args(par, TK_BAR, TK_BAR);
	}

	if (l_test_skip(par, TK_LBR)) {
		a_line line = ln_cur(par);

		l_scan_stats(par);
		l_check_pair_right(par, TK_LBR, TK_RBR, line);
	}
	else {
		Expr e2;

		l_scan_expr(par, e2);
		ai_code_vararg1(par, e2, OP_RETURN, scope._begin_line);
	}

	GProto* meta = ai_code_epilogue(par, null, false, ln_cur(par));
	ai_code_loadfunc(par, e, meta);
}

static void l_scan_atom_expr(Parser* par, OutExpr e) {
	switch (l_peek(par)) {
		case TK_NIL: {
			ai_code_constK(par, e, EXPR_NIL, ln_cur(par));
			l_skip(par);
			break;
		}
		case TK_FALSE: {
			ai_code_constK(par, e, EXPR_FALSE, ln_cur(par));
			l_skip(par);
			break;
		}
		case TK_TRUE: {
			ai_code_constK(par, e, EXPR_TRUE, ln_cur(par));
			l_skip(par);
			break;
		}
		case TK_INTEGER: {
			ai_code_constI(par, e, tk_cur(par)->_int, ln_cur(par));
			l_skip(par);
			break;
		}
		case TK_FLOAT: {
			ai_code_constF(par, e, tk_cur(par)->_float, ln_cur(par));
			l_skip(par);
			break;
		}
		case TK_STRING: {
			ai_code_constS(par, e, tk_cur(par)->_str, ln_cur(par));
			l_skip(par);
			break;
		}
		case TK_TSTRING: {
			l_scan_tstring(par, e);
			break;
		}
		case TK_IDENT: {
			ai_code_lookupG(par, e, tk_cur(par)->_str, ln_cur(par));
			l_skip(par);
			break;
		}
		case TK_LBK: {
			a_u32 line = ln_cur(par);
			l_skip(par);

			if (l_test_skip(par, TK_RBK)) {
				ai_code_constK(par, e, EXPR_UNIT, line);
				ai_code_vararg1(par, e, OP_TNEW, line);
			}
			else {
				l_scan_expr(par, e);
				if (!l_test_skip(par, TK_RBK)) {
					if (l_test_skip(par, TK_COMMA) && !l_test(par, TK_RBK)) {
						l_scan_exprs(par, e, true);
					}
					l_check_pair_right(par, TK_LBK, TK_RBK, line);
					ai_code_vararg1(par, e, OP_TNEW, line);
				}
			}
			break;
		}
		case TK_LSQ: {
			a_u32 line = ln_cur(par);
			l_skip(par);
			l_check_pair_right(par, TK_LSQ, TK_RSQ, line); //TODO
			ai_code_new_list(par, e, line);
			break;
		}
		case TK_LBR: {
			a_u32 line = ln_cur(par);
			l_skip(par);

			l_check_pair_right(par, TK_LBR, TK_RBR, line); //TODO
			ai_code_new_table(par, e, line);
			break;
		}
		case TK_BAR:
		case TK_BBAR: {
			l_scan_lambda(par, e);
			break;
		}
		default: {
			l_error_got(par, "expression expected");
		}
	}
}

static void l_scan_suffixed_expr(Parser* par, OutExpr e) {
	l_sync(par);
	a_line line0 = e->_line;
	l_scan_atom_expr(par, e);
	a_u32 label = NO_LABEL;
	loop {
		switch (l_peek(par)) {
			case TK_QDOT: {
				a_line line = ln_cur(par);
				l_skip(par);
				ai_code_monad(par, e, &label, OP_OR_NIL, line);
				GStr* name = l_check_ident(par);
				ai_code_lookupS(par, e, name, line);
				break;
			}
			case TK_BANG: {
				a_line line = ln_cur(par);
				l_skip(par);
				ai_code_monad(par, e, &label, OP_OR_RET, line);
				break;
			}
			case TK_DOT: {
				a_line line = ln_cur(par);
				l_skip(par);
				GStr* name = l_check_ident(par);
				ai_code_lookupS(par, e, name, line);
				break;
			}
			case TK_COLON: {
				a_line line = ln_cur(par);
				l_skip(par);
				GStr* name = l_check_ident(par);
				ai_code_lookupS(par, e, name, line);
				break;
			}
			case TK_LSQ: {
				Expr e2;
				a_line line = ln_cur(par);
				l_skip(par);
				l_scan_expr(par, e2);
				ai_code_index(par, e, e2, line);
				while (l_test_skip(par, TK_COMMA)) {
					l_scan_expr(par, e2);
					ai_code_index(par, e, e2, line);
				}
				l_check_pair_right(par, TK_LSQ, TK_RSQ, line);
				break;
			}
			case TK_LBK: {
				a_line line = ln_cur(par);
				l_skip(par);

				if (!l_test_skip(par, TK_RBK)) {
					l_scan_exprs(par, e, true);
					l_check_pair_right(par, TK_LBK, TK_RBK, line);
				}
				ai_code_vararg1(par, e, OP_CALL, line);
				break;
			}
			default: {
				ai_code_monad(par, e, &label, OP_MERGE, line0);
				return;
			}
		}
	}
}

static void l_scan_prefixed_expr(Parser* par, OutExpr e) {
	switch (l_peek(par)) {
		case TK_PLUS: {
			//TODO should VM check type for this operation?
			l_skip(par);
			l_scan_prefixed_expr(par, e);
			break;
		}
		case TK_MINUS: {
			a_line line = ln_cur(par);
			l_skip(par);
			l_scan_prefixed_expr(par, e);
			ai_code_unary(par, e, OP_NEG, line);
			break;
		}
		case TK_TILDE: {
			a_line line = ln_cur(par);
			l_skip(par);
			l_scan_prefixed_expr(par, e);
			ai_code_unary(par, e, OP_BIT_INV, line);
			break;
		}
		case TK_BANG: {
			a_line line = ln_cur(par);
			l_skip(par);
			l_scan_prefixed_expr(par, e);
			ai_code_unary(par, e, OP_NOT, line);
			break;
		}
		case TK_SHARP: {
			a_line line = ln_cur(par);
			l_skip(par);
			l_scan_prefixed_expr(par, e);
			ai_code_unary(par, e, OP_LEN, line);
			break;
		}
		case TK_STAR: {
			a_line line = ln_cur(par);
			l_skip(par);
			l_scan_prefixed_expr(par, e);
			ai_code_unary(par, e, OP_UNBOX, line);
			break;
		}
		case TK_FN: {
			a_line line = ln_cur(par);
			l_skip(par);
			l_scan_function(par, e, null, line);
			break;
		}
		default: {
			l_scan_suffixed_expr(par, e);
			break;
		}
	}
}

static void l_scan_unpack_expr(Parser* par, OutExpr e) {
	l_scan_prefixed_expr(par, e);
	if (l_test_skip(par, TK_TDOT)) {
		ai_code_unpack(par, e, ln_cur(par));
	}
}

static a_u32 l_scan_arith_op(Parser* par) {
	switch (l_peek(par)) {
		case TK_PLUS:
			return OP_ADD;
		case TK_MINUS:
			return OP_SUB;
		case TK_STAR:
			return OP_MUL;
		case TK_LSLASH:
			return OP_DIV;
		case TK_PERCENT:
			return OP_MOD;
		case TK_SHL:
			return OP_SHL;
		case TK_SHR:
			return OP_SHR;
		case TK_AMP:
			return OP_BIT_AND;
		case TK_BAR:
			return OP_BIT_OR;
		case TK_HAT:
			return OP_BIT_XOR;
		default:
			return OP__NOT_BIN;
	}
}

static void l_scan_arith_expr(Parser* par, OutExpr e, a_u32 lv) {
	static a_u8 const prios[] = {
		[OP_ADD] = 5,
		[OP_SUB] = 5,
		[OP_MUL] = 6,
		[OP_DIV] = 6,
		[OP_MOD] = 6,
		[OP_SHL] = 4,
		[OP_SHR] = 4,
		[OP_BIT_AND] = 3,
		[OP_BIT_OR] = 2,
		[OP_BIT_XOR] = 1
	};

	a_u32 op;
	l_scan_unpack_expr(par, e);
	while ((op = l_scan_arith_op(par)) != OP__NOT_BIN && prios[op] >= lv) {
		Expr e2;
		a_line line = ln_cur(par);
		ai_code_binary1(par, e, op, line);
		l_skip(par);
		l_scan_arith_expr(par, e2, prios[op] + 1);
		ai_code_binary2(par, e, e2, op, line);
		lv = prios[op];
	}
}

static a_u32 l_scan_compare_op(Parser* par) {
	switch (l_peek(par)) {
		case TK_GT:
			return OP_GT;
		case TK_GE:
			return OP_GE;
		case TK_LT:
			return OP_LT;
		case TK_LE:
			return OP_LE;
		default:
			return OP__NOT_BIN;
	}
}

static void l_scan_compare_expr(Parser* par, OutExpr e) {
	a_u32 op1, op2;
	a_line line1;

	l_scan_arith_expr(par, e, 0);
	op1 = l_scan_compare_op(par);
	if (op1 == 0) return;

	l_skip(par);
	line1 = ln_cur(par);
	ai_code_binary1(par, e, op1, line1);

	Expr e2;
	l_scan_arith_expr(par, e2, 0);
	ai_code_binary2(par, e, e2, op1, line1);

	op2 = l_scan_compare_op(par);
	while (op2 != OP__NOT_BIN) {
		a_u32 line2;

		l_skip(par);
		line2 = ln_cur(par);
		ai_code_binary1(par, e, OP_AND, line2);
		ai_code_binary1(par, e2, op2, line2);

		Expr e3;
		l_scan_arith_expr(par, e3, 0);
		ai_code_binary2(par, e2, e3, op2, line2);

		ai_code_drop(par, e3);
		ai_code_binary2(par, e, e2, OP_AND, line2);

		op2 = l_scan_compare_op(par);
	}
}

static a_u32 l_scan_relation_op(Parser* par) {
	switch (l_peek(par)) {
		case TK_EQ:
			return OP_EQ;
		case TK_NE:
			return OP_NE;
		case TK_IS:
			return OP_IS;
		case TK_IN:
			return OP_IN;
		case TK_BANG:
			switch (l_forward(par)) {
				case TK_IS:
					l_skip(par);
					return OP_IS_NOT;
				case TK_IN:
					l_skip(par);
					return OP_NOT_IN;
			}
			break;
	}
	return 0;
}

static void l_scan_relation_expr(Parser* par, OutExpr e) {
	l_scan_compare_expr(par, e);
	a_u32 op = l_scan_relation_op(par);
	if (op != 0) {
		a_u32 line = ln_cur(par);
		l_skip(par);
		ai_code_binary1(par, e, op, line);

		Expr e2;
		l_scan_compare_expr(par, e2);
		ai_code_binary2(par, e, e2, op, line);
	}
}

static void l_scan_concatenate_expr(Parser* par, OutExpr e) {
	a_line line1 = ln_cur(par);
	l_scan_relation_expr(par, e);
	if (l_test_skip(par, TK_TILDE)) {
		ConExpr ce = { };
		ai_code_concat_next(par, &ce, e, line1);
		do {
			a_u32 line2 = ln_cur(par);
			l_scan_relation_expr(par, e);
			ai_code_concat_next(par, &ce, e, line2);
		}
		while (l_test_skip(par, TK_TILDE));
		ai_code_concat_end(par, &ce, e, line1);
	}
}

static void l_scan_conjunctive_expr(Parser* par, OutExpr e) {
	l_scan_concatenate_expr(par, e);
	while (l_test(par, TK_BAMP)) {
		Expr e2;
		ai_code_binary1(par, e, OP_AND, ln_cur(par));
		l_skip(par);
		l_scan_concatenate_expr(par, e2);
		ai_code_binary2(par, e, e2, OP_AND, ln_cur(par));
	}
}

static void l_scan_disjunctive_expr(Parser* par, OutExpr e) {
	l_scan_conjunctive_expr(par, e);
	while (l_test(par, TK_BBAR)) {
		Expr e2;
		ai_code_binary1(par, e, OP_OR, ln_cur(par));
		l_skip(par);
		l_scan_conjunctive_expr(par, e2);
		ai_code_binary2(par, e, e2, OP_OR, ln_cur(par));
	}
}

static void l_scan_ternary_expr(Parser* par, OutExpr e) {
	l_scan_disjunctive_expr(par, e);
	switch (l_peek(par)) {
		case TK_QUESTION: {
			a_line line = ln_cur(par);
			a_u32 label1 = ai_code_testT(par, e, line);
			ai_code_drop(par, e);

			l_skip(par);
			l_scan_ternary_expr(par, e);

			a_u32 label2;
			ai_code_monad(par, e, &label2, OP_OR_ELSE, line);

			l_check_skip(par, TK_RSLASH);

			Expr e2;
			ai_code_label(par, label1, line);
			l_scan_ternary_expr(par, e2);
			ai_code_merge(par, e, e2, label2, line);
			break;
		}
		case TK_ELVIS: {
			a_line line = ln_cur(par);
			a_u32 label1 = ai_code_testT(par, e, line);

			l_skip(par);

			a_u32 label2;
			ai_code_monad(par, e, &label2, OP_OR_ELSE, line);

			Expr e2;
			ai_code_label(par, label1, line);
			l_scan_ternary_expr(par, e2);
			ai_code_merge(par, e, e2, label2, line);
			break;
		}
	}
}

static void l_scan_expr(Parser* par, OutExpr e) {
	l_scan_ternary_expr(par, e);
}

static void l_scan_exprs(Parser* par, InoutExpr es, a_bool exists) {
	Expr e;

	if (!exists) {
		l_scan_expr(par, es);
		if (!l_test_skip(par, TK_COMMA))
			return;
	}

	do {
		ai_code_vararg1(par, es, OP_VA_PUSH, ln_cur(par));

		l_scan_expr(par, e);
		ai_code_vararg2(par, es, e, OP_VA_PUSH, ln_cur(par));
	}
	while (l_test_skip(par, TK_COMMA));
}

typedef struct Lhs Lhs;
typedef struct LhsNode LhsNode;

struct LhsNode {
	Expr _expr;
	LhsNode* _last;
};

struct Lhs {
	LhsNode _head;
	LhsNode* _tail;
	a_u32 _count;
};

static void l_scan_assign_rhs(Parser* par, LhsNode* tail, a_u32 count) {
	l_check_skip(par, TK_ASSIGN);
	a_u32 line = ln_cur(par);

	Expr rhs;
	Expr rhs_last;

	l_scan_expr(par, rhs);
	if (l_test_skip(par, TK_COMMA)) {
		ai_code_vararg1(par, rhs, OP_VA_PUSH, ln_cur(par));
		loop {
			l_scan_expr(par, rhs_last);

			if (!l_test_skip(par, TK_COMMA))
				break;

			ai_code_vararg2(par, rhs, rhs_last, OP_VA_PUSH, ln_cur(par));
			ai_code_vararg1(par, rhs, OP_VA_PUSH, ln_cur(par));
		}
	}

	a_u32 num_nil = ai_code_args_trunc(par, rhs, rhs_last, count, line);

	LhsNode* node = tail;

	while (num_nil > 0) {
		Expr en;
		ai_code_constK(par, en, EXPR_NIL, line);
		ai_code_bind(par, node->_expr, en, line);

		node = node->_last;
		assume(node != null);
		num_nil -= 1;
	}

	loop {
		ai_code_bind(par, node->_expr, rhs_last, line);
		if ((node = node->_last) == null)
			return;

		ai_code_vararg2(par, rhs, rhs_last, OP_VA_POP, line);
	}
}

static void l_scan_assign_lhs_tail(Parser* par, Lhs* lhs) {
	LhsNode n = {};

	if (l_test_skip(par, TK_COMMA)) {
		n._last = lhs->_tail;

		lhs->_count += 1;
		lhs->_tail = &n;

		l_scan_prefixed_expr(par, n._expr);
		return l_scan_assign_lhs_tail(par, lhs);
	}

	l_scan_assign_rhs(par, lhs->_tail, lhs->_count + 1);
}

static void l_scan_assign_or_call(Parser* par) {
	Lhs lhs = {};

	l_scan_expr(par, lhs._head._expr);
	if (l_test(par, TK_COMMA) || l_test_skip(par, TK_TDOT) || l_test(par, TK_ASSIGN)) {
		lhs._tail = &lhs._head;
		l_scan_assign_lhs_tail(par, &lhs);
	}

	l_test_skip(par, TK_SEMI);
	ai_code_drop(par, lhs._head._expr);
}

static void l_scan_if_stat(Parser* par) {
	l_skip(par);

	l_check_skip(par, TK_LBK);
	a_u32 line = ln_cur(par);

	Expr ep;
	l_scan_expr(par, ep);
	a_u32 label1 = ai_code_testT(par, ep, line);
	ai_code_drop(par, ep);

	l_check_pair_right(par, TK_LBK, TK_RBK, line);
	l_skip(par);

	l_scan_stat(par);

	if (l_test_skip(par, TK_ELSE)) {
		a_u32 label2 = ai_code_gotoU(par, NO_LABEL, ln_cur(par));

		l_sync(par);
		ai_code_label(par, label1, ln_cur(par));
		l_scan_stat(par);

		ai_code_label(par, label2, ln_cur(par));
	}
	else {
		ai_code_label(par, label1, ln_cur(par));
	}
}

static void l_scan_while_stat(Parser* par) {
	l_skip(par);

	l_check_skip(par, TK_LBK);
	a_line line = ln_cur(par);

	a_u32 label1 = ai_code_label(par, NO_LABEL, line);

	Expr ep;
	l_scan_expr(par, ep);
	a_u32 label2 = ai_code_testT(par, ep, line);
	ai_code_drop(par, ep);

	l_check_pair_right(par, TK_LBK, TK_RBK, line);
	l_skip(par);

	l_scan_stat(par);
	ai_code_gotoD(par, label1, ln_cur(par));

	ai_code_label(par, label2, ln_cur(par));

	if (l_test_skip(par, TK_ELSE)) {
		l_sync(par);
		l_scan_stat(par);
	}
}

static void l_scan_return_stat(Parser* par) {
	a_line line = ln_cur(par);

	l_skip(par);

	Expr e;

	if (!l_test_sep(par)) {
		l_scan_exprs(par, e, false);
	}

	l_test_skip(par, TK_SEMI);
	ai_code_vararg1(par, e, OP_RETURN, line);
}

static void l_scan_let_rhs(Parser* par, LetStat* stat, a_u32 line) {
	Expr e;

	do {
		l_scan_expr(par, e);
		ai_code_let_bind(par, stat, e);
	}
	while (l_test_skip(par, TK_COMMA));

	ai_code_let_nils(par, stat, line);
	ai_code_compact(par);
}

static a_none l_unmatched_pattern(Parser* par, LetNode* node) {
	if (node == null)
		l_error_got(par, "malformed pattern");
	switch (node->_kind) {
		case PAT_TUPLE:
			l_error_bracket(par, '(', ')', node->_expr->_line);
		case PAT_LIST:
			l_error_bracket(par, '[', ']', node->_expr->_line);
		case PAT_TABLE:
			l_error_bracket(par, '{', '}', node->_expr->_line);
		default:
			unreachable();
	}
}

static void l_scan_pattern(Parser* par, LetStat* stat, LetNode* parent, LetNode** slot, a_u32 tag) {
	enum {
		GOTO_ROOT,
		GOTO_TUPLE,
		GOTO_LIST,
		GOTO_TABLE
	};

	static_assert(GOTO_ROOT == 0);

	static_assert(GOTO_TUPLE - GOTO_ROOT + PAT_ROOT == PAT_TUPLE);
	static_assert(GOTO_LIST - GOTO_ROOT + PAT_ROOT == PAT_LIST);
	static_assert(GOTO_TABLE - GOTO_ROOT + PAT_ROOT == PAT_TABLE);

	LetNode node[1] = {};

	node->_parent = parent;
	node->_index = parent->_count;
	parent->_count += 1;
	*slot = node;

	switch (tag) {
		case GOTO_ROOT:
		case GOTO_TUPLE:
			goto branch_standard;
		default:
			unreachable();
	}

branch_standard:
	switch (l_peek(par)) {
		case TK_NIL: {
			ai_code_constK(par, node->_expr, EXPR_NIL, ln_cur(par));
			l_skip(par);
			break;
		}
		case TK_INTEGER: {
			ai_code_constI(par, node->_expr, tk_cur(par)->_int, ln_cur(par));
			l_skip(par);
			break;
		}
		case TK_FLOAT: {
			ai_code_constF(par, node->_expr, tk_cur(par)->_float, ln_cur(par));
			l_skip(par);
			break;
		}
		case TK_STRING: {
			ai_code_constS(par, node->_expr, tk_cur(par)->_str, ln_cur(par));
			l_skip(par);
			break;
		}
		case TK_IDENT: {
			l_skip(par);
			expr_init(node->_expr, PAT_BIND, ._str = tk_cur(par)->_str, ._line = ln_cur(par));
			break;
		}
		case TK_UNDERSCORE: {
			l_skip(par);
			parent->_faggr = false;
			node->_kind = PAT_DROP;
			node->_line = ln_cur(par);
			break;
		}
		case TK_LBK: {
			l_skip(par);
			parent->_faggr = false;
			node->_kind = PAT_TUPLE;
			node->_line = ln_cur(par);
			node->_count = 0;
			if (!l_test_skip(par, TK_RBK)) {
				return l_scan_pattern(par, stat, node, &node->_child, GOTO_TUPLE);
			}
			break;
		}
		case TK_LSQ: {
			l_skip(par);
			parent->_faggr = false;
			node->_kind = PAT_LIST;
			node->_line = ln_cur(par);
			node->_count = 0;
			if (!l_test_skip(par, TK_RSQ)) {
				return l_scan_pattern(par, stat, node, &node->_child, GOTO_LIST);
			}
			break;
		}
		case TK_LBR: {
			l_skip(par);
			parent->_faggr = false;
			node->_kind = PAT_TABLE;
			node->_line = ln_cur(par);
			node->_count = 0;
			if (!l_test_skip(par, TK_RBR)) {
				return l_scan_pattern(par, stat, node, &node->_child, GOTO_TABLE);
			}
			break;
		}
		default:
			goto error;
	}

	slot = &node->_sibling;
	loop {
		switch (l_peek(par)) {
			case TK_COMMA: {
				l_skip(par);
				tag = parent->_kind - PAT_ROOT + GOTO_ROOT; /* Recover goto label. */
				return l_scan_pattern(par, stat, parent, slot, tag);
			}
			case TK_RBK: {
				if (parent->_kind != PAT_TUPLE) {
					l_unmatched_pattern(par, parent);
				}
				l_skip(par);
				break;
			}
			case TK_RSQ: {
				if (parent->_kind != PAT_LIST) {
					l_unmatched_pattern(par, parent);
				}
				l_skip(par);
				break;
			}
			case TK_RBR: {
				if (parent->_kind != PAT_TABLE) {
					l_unmatched_pattern(par, parent);
				}
				l_skip(par);
				break;
			}
			case TK_ASSIGN: {
				l_skip(par);
				if (parent->_kind != PAT_ROOT) {
					return l_scan_let_rhs(par, stat, ln_cur(par));
				}
				else {
					//TODO
					goto error;
				}
			}
			default: {
				return ai_code_let_nils(par, stat, ln_cur(par));
			}
		}

		assume(parent != null);
		slot = &parent->_sibling;
		parent = parent->_parent;
	}

error:
	l_error_got(par, "malformed pattern");
}

static void l_scan_let_stat(Parser* par) {
	a_line line = ln_cur(par);
	l_skip(par);

	switch (l_peek(par)) {
		case TK_FN: {
			Expr e1, e2;
			l_skip(par);
			GStr* name = l_check_ident(par);
			ai_code_local(par, e1, name, line);
			l_scan_function(par, e2, name, line);
			ai_code_bind(par, e1, e2, line);
			break;
		}
		default: {
			LetStat stat = { };
			l_scan_pattern(par, &stat, &stat._root, &stat._root._child, 0);
			break;
		}
	}
}

static void l_scan_fun_def_stat(Parser* par) {
	a_line line = ln_cur(par);
	l_skip(par);

	Expr en;
	Expr ef;

	GStr* name = l_check_ident(par);
	ai_code_lookupG(par, en, name, ln_cur(par));
	while (l_test_skip(par, TK_DOT)) {
		name = l_check_ident(par);
		ai_code_lookupS(par, en, name, ln_cur(par));
	}

	l_scan_function(par, ef, name, line);

	ai_code_bind(par, en, ef, line);
}

static void l_scan_pub_stat(Parser* par) {
	a_line line = ln_cur(par);
	l_skip(par);

	switch (l_peek(par)) {
		case TK_FN: {
			Expr e1, e2;
			l_skip(par);
			GStr* name = l_check_ident(par);
			ai_code_export(par, e1, name, line);
			l_scan_function(par, e2, name, line);
			ai_code_bind(par, e1, e2, line);
			break;
		}
		default: {
			Expr e;
			GStr* name = l_check_ident(par);
			ai_code_export(par, e, name, line);
			if (l_test_skip(par, TK_ASSIGN)) {
				Expr e2;
				l_scan_expr(par, e2);
				ai_code_bind(par, e, e2, line);
			}
			break;
		}
	}
}

static void l_scan_stat_pack(Parser* par) {
	Scope scope;

	a_line line = ln_cur(par);
	l_skip(par);
	ai_code_enter(par, &scope, line);

	l_scan_stats(par);

	l_check_pair_right(par, TK_LBR, TK_RBR, line);

	ai_code_leave(par, ln_cur(par));
}

static void l_scan_stat(Parser* par) {
	assume(par->_scope->_top_ntr == par->_scope->_top_reg, "compute stack leaked.");
	switch (l_peek(par)) {
		case TK_IF: {
			l_scan_if_stat(par);
			break;
		}
		case TK_RETURN: {
			l_scan_return_stat(par);
			break;
		}
		case TK_WHILE: {
			l_scan_while_stat(par);
			break;
		}
		case TK_FN: {
			l_scan_fun_def_stat(par);
			break;
		}
		case TK_LBR: {
			l_scan_stat_pack(par);
			break;
		}
		case TK_PUB: {
			l_scan_pub_stat(par);
			break;
		}
		default: {
			l_scan_assign_or_call(par);
			break;
		}
	}
}

static void l_scan_stats(Parser* par) {
	loop {
		switch (l_peek(par)) {
			case TK_RBR:
			case TK_EOF:
				return;
			case TK_SEMI: {
				l_skip(par);
				break;
			}
			case TK_LET: {
				l_scan_let_stat(par);
				break;
			}
			case TK_RETURN: {
				l_scan_return_stat(par);
				break;
			}
			case TK_IF: {
				l_scan_if_stat(par);
				break;
			}
			case TK_WHILE: {
				l_scan_while_stat(par);
				break;
			}
			case TK_FN: {
				l_scan_fun_def_stat(par);
				break;
			}
			case TK_PUB: {
				l_scan_pub_stat(par);
				break;
			}
			default: {
				l_scan_assign_or_call(par);
				break;
			}
		}
	}
}

static void l_scan_root_return_stat(Parser* par) {
	Expr e;

	if (!l_test_sep(par)) {
		l_scan_exprs(par, e, false);
		/* Unpack the return value if only return one value and it can be unpacked. */
		if (!par->_fnscope->_fvarg && (e->_kind == EXPR_DYN_AC || e->_kind == EXPR_DYN_C)) {
			ai_code_unpack(par, e, ln_cur(par));
		}
	}

	l_test_skip(par, TK_SEMI);
	ai_code_vararg1(par, e, OP_RETURN, 1);
}

static void l_scan_root(unused a_henv env, void* ctx) {
	FnScope scope;

	Parser* par = ctx;
	ai_code_open(par);

	ai_code_prologue(par, &scope, 1);
	if (unlikely(par->_options & ALO_COMP_OPT_EVAL)) {
		l_scan_root_return_stat(par);
	}
	else {
		l_scan_stats(par);
	}

	if (!l_test(par, TK_EOF)) {
		l_error_got(par, "statement expected");
	}

	ai_code_epilogue(par, par->_name, true, ln_cur(par));
}

static void l_init_parser(a_henv env, a_ifun fun, void* ctx, GStr* file, GStr* name, a_u32 options, Parser* par) {
	*par = new(Parser) { ._options = options };
	ai_lex_init(env, &par->_lex, fun, ctx);
	par->_lex._file = file;
	par->_name = name;
	rq_init(&par->_rq);
}

a_msg ai_parse(a_henv env, a_ifun fun, void* ctx, GStr* file, GStr* name, a_u32 options, GFun** pfun) {
	Parser par;
	l_init_parser(env, fun, ctx, file, name, options, &par);

	a_msg msg = ai_env_pcall(env, l_scan_root, &par);

	if (msg == ALO_SOK) {
		*pfun = ai_code_build_and_close(&par);
	}

	return msg;
}