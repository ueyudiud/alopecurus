/**
 *@file aparse.c
 */

#define aparse_c_

#include "afun.h"
#include "aenv.h"
#include "acode.h"

#include "aparse.h"

#define lex(par) (&(par)->_lex)
#define ln_cur(par) (lex(par)->_current._line)
#define tk_cur(par) (&lex(par)->_current)

#define l_peek(par) ai_lex_peek(lex(par))
#define l_sync(par) quiet(l_peek(par))
#define l_skip(par) quiet(tk_cur(par)->_tag = TK__NONE)
#define l_test(par,tk) (l_peek(par) == (tk))
#define l_forward(par) ai_lex_forward(lex(par))

static void l_check(Parser* par, a_i32 tk, char const* what) {
	if (!l_test(par, tk)) {
		l_error(par, "%s expected.", ln_cur(par), what);
	}
}

static GStr* l_check_ident(Parser* par) {
	if (!l_test(par, TK_IDENT)) {
		l_error(par, "identifier expected.", ln_cur(par));
	}
	GStr* str = tk_cur(par)->_str;
	l_skip(par);
	return str;
}

static void l_check_pair_right(Parser* par, unused a_i32 ltk, char const* lwhat,
							   a_i32 rtk, char const* rwhat, a_u32 line) {
	if (!l_test(par, rtk)) {
		if (ln_cur(par) == line) {
			l_error(par, "%s expected to match %s.", ln_cur(par), lwhat, rwhat);
		}
		else {
			l_error(par, "%s expected to match %s at line %u.", ln_cur(par), lwhat, rwhat, line);
		}
	}
	l_skip(par);
}

static void l_scan_expr(Parser* par, OutExpr e);
static void l_scan_stat(Parser* par);

static void l_scan_tstring(Parser* par, Exprs* con) {
	LexScope scope;
	a_bool stop = false;
	while (!stop) {
		a_u32 line = ln_cur(par);
		switch (l_peek(par)) {
			case TK_TSTRING: {
				ai_code_constS(par, &con->_last, tk_cur(par)->_str, line);
				l_skip(par);
				break;
			}
			case TK_STRING: {
				ai_code_constS(par, &con->_last, tk_cur(par)->_str, line);
				l_skip(par);
				stop = true;
				break;
			}
			case TK_LBK: {
				l_skip(par);
				ai_lex_push_scope(&par->_lex, &scope);
				l_scan_expr(par, &con->_last);
				ai_lex_pop_scope(&par->_lex);
				l_check_pair_right(par, TK_LBK, "'{'", TK_RBK, "'}'", line);
				break;
			}
			default: {
				l_error(par, "bad template string.", ln_cur(par));
			}
		}
		ai_code_multi(par, con, OP_CONCAT, line);
	}
}

static void l_scan_atom_expr(Parser* par, OutExpr e) {
	switch (l_peek(par)) {
		case TK_NIL: {
			ai_code_constK(par, e, EXPR_NIL, ln_cur(par));
			break;
		}
		case TK_FALSE: {
			ai_code_constK(par, e, EXPR_FALSE, ln_cur(par));
			break;
		}
		case TK_TRUE: {
			ai_code_constK(par, e, EXPR_TRUE, ln_cur(par));
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
			Exprs es = {};
			ai_code_constS(par, &es._last, tk_cur(par)->_str, ln_cur(par));
			l_skip(par);
			l_scan_tstring(par, &es);
			ai_code_multi(par, &es, OP_CONCAT, ln_cur(par));
			break;
		}
		case TK_IDENT: {
			ai_code_lookupU(par, e, tk_cur(par)->_str, ln_cur(par));
			l_skip(par);
			break;
		}
		case TK_LBK: {
			a_u32 line = ln_cur(par);
			l_skip(par);
			l_scan_expr(par, e);
			if (l_test(par, TK_RBK)) {
				l_skip(par);
			}
			else {
				//TODO
			}
			break;
		}
		default: l_error(par, "bad expression.", ln_cur(par));
	}
}

static void l_scan_suffixed_expr(Parser* par, OutExpr e) {
	a_u32 line0 = e->_line;
	l_scan_atom_expr(par, e);
	a_u32 label = NO_LABEL;
	loop {
		switch (l_peek(par)) {
			case TK_QDOT: {
				a_u32 line = ln_cur(par);
				l_skip(par);
				ai_code_monad(par, e, &label, OP_OPTION, line);
				GStr* name = l_check_ident(par);
				ai_code_lookupC(par, e, name, line);
				break;
			}
			case TK_DOT: {
				a_u32 line = ln_cur(par);
				l_skip(par);
				GStr* name = l_check_ident(par);
				ai_code_lookupC(par, e, name, line);
				break;
			}
			case TK_LSQ: {
				Expr e2;
				a_u32 line = ln_cur(par);
				l_skip(par);
				l_scan_expr(par, &e2);
				ai_code_index(par, e, &e2, line);
				while (l_test(par, TK_COMMA)) {
					l_skip(par);
					l_scan_expr(par, &e2);
					ai_code_index(par, e, &e2, line);
				}
				l_check_pair_right(par, TK_LSQ, "'['", TK_RSQ, "']'", line);
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
			//TODO
			l_skip(par);
			l_scan_prefixed_expr(par, e);
			break;
		}
		case TK_MINUS: {
			a_u32 line = ln_cur(par);
			l_skip(par);
			l_scan_prefixed_expr(par, e);
			ai_code_prefix(par, e, OP_NEG, line);
			break;
		}
		case TK_TILDE: {
			a_u32 line = ln_cur(par);
			l_skip(par);
			l_scan_prefixed_expr(par, e);
			ai_code_prefix(par, e, OP_BIT_INV, line);
			break;
		}
		case TK_NOT: {
			a_u32 line = ln_cur(par);
			l_skip(par);
			l_scan_prefixed_expr(par, e);
			ai_code_prefix(par, e, OP_NOT, line);
			break;
		}
		case TK_STAR: {
			a_u32 line = ln_cur(par);
			l_skip(par);
			l_scan_prefixed_expr(par, e);
			ai_code_prefix(par, e, OP_UNBOX, line);
			break;
		}
		default: {
			l_scan_suffixed_expr(par, e);
			break;
		}
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
			return OP__NONE;
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
	l_scan_prefixed_expr(par, e);
	while ((op = l_scan_arith_op(par)) && prios[op] >= lv) {
		Expr e2;
		a_u32 line = ln_cur(par);
		ai_code_infix(par, e, op, line);
		l_skip(par);
		l_scan_arith_expr(par, &e2, prios[op] + 1);
		ai_code_suffix(par, e, &e2, op, line);
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
			return TK__NONE;
	}
}

static void l_scan_compare_expr(Parser* par, OutExpr e) {
	a_u32 op1, op2;
	a_u32 line1;

	l_scan_arith_expr(par, e, 0);
	op1 = l_scan_compare_op(par);
	if (op1 == OP__NONE || l_forward(par) == TK_BDOT) return;

	l_skip(par);
	line1 = ln_cur(par);
	ai_code_infix(par, e, op1, line1);

	Expr e2;
	l_scan_arith_expr(par, &e2, 0);
	ai_code_suffix(par, e, &e2, op1, line1);

	op2 = l_scan_compare_op(par);
	while (op2 != OP__NONE) {
		a_u32 line2;

		l_skip(par);
		line2 = ln_cur(par);
		ai_code_infix(par, e, OP_AND, line2);
		ai_code_infix(par, &e2, op2, line2);

		Expr e3;
		l_scan_arith_expr(par, &e3, 0);
		ai_code_suffix(par, &e2, &e3, op2, line2);

		ai_code_drop(par, &e3);
		ai_code_suffix(par, e, &e2, OP_AND, line2);

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
		case TK_NOT:
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
	return OP__NONE;
}

static void l_scan_relation_expr(Parser* par, OutExpr e) {
	l_scan_compare_expr(par, e);
	a_u32 op = l_scan_relation_op(par);
	if (op != OP__NONE) {
		a_u32 line = ln_cur(par);
		l_skip(par);
		ai_code_infix(par, e, op, line);

		Expr e2;
		l_scan_compare_expr(par, &e2);
		ai_code_suffix(par, e, &e2, op, line);
		ai_code_drop(par, &e2);
	}
}

static void l_scan_conjunctive_expr(Parser* par, OutExpr e) {
	l_scan_relation_expr(par, e);
	while (l_test(par, TK_BAMP)) {
		Expr e2;
		ai_code_infix(par, e, OP_AND, ln_cur(par));
		l_skip(par);
		l_scan_relation_expr(par, &e2);
		ai_code_suffix(par, e, &e2, OP_AND, ln_cur(par));
	}
}

static void l_scan_disjunctive_expr(Parser* par, OutExpr e) {
	l_scan_conjunctive_expr(par, e);
	while (l_test(par, TK_BBAR)) {
		Expr e2;
		ai_code_infix(par, e, OP_OR, ln_cur(par));
		l_skip(par);
		l_scan_conjunctive_expr(par, &e2);
		ai_code_suffix(par, e, &e2, OP_OR, ln_cur(par));
	}
}

static void l_scan_ternary_expr(Parser* par, OutExpr e) {
	l_scan_disjunctive_expr(par, e);
	switch (l_peek(par)) {
		case TK_QUESTION: {
			a_u32 line = ln_cur(par);
			a_u32 label1 = ai_code_testT(par, e, line);
			ai_code_drop(par, e);

			l_skip(par);
			l_scan_ternary_expr(par, e);

			a_u32 label2;
			ai_code_monad(par, e, &label2, OP_OR_ELSE, line);

			l_check(par, TK_COLON, "':'");
			l_skip(par);

			Expr e2;
			ai_code_label(par, label1, line);
			l_scan_ternary_expr(par, &e2);
			ai_code_merge(par, e, &e2, label2, line);
			break;
		}
		case TK_ELVIS: {
			a_u32 line = ln_cur(par);
			a_u32 label1 = ai_code_testT(par, e, line);

			a_u32 label2;
			ai_code_monad(par, e, &label2, OP_OR_ELSE, line);

			Expr e2;
			ai_code_label(par, label1, line);
			l_scan_ternary_expr(par, &e2);
			ai_code_merge(par, e, &e2, label2, line);
			break;
		}
		default: break;
	}
}

static void l_scan_expr(Parser* par, OutExpr e) {
	l_scan_ternary_expr(par, e);
}

typedef struct Lhs Lhs;
typedef struct LhsNode LhsNode;

struct LhsNode {
	Expr _expr;
	a_u32 _line;
	LhsNode* _last;
};

struct Lhs {
	LhsNode _head;
	LhsNode* _tail;
	a_u32 _count;
};

static void l_scan_normal_assign_frag(Parser* par, Lhs* lhs) {
	a_bool lhs_multi = false;
	a_bool lhs_wrap = false;
	LhsNode node0 = {};

	if (l_test(par, TK_COMMA)) {
		l_skip(par);

		a_u32 line = ln_cur(par);

		if (l_test(par, TK_TDOT)) {
			lhs_multi = true;
			lhs_wrap = true;
			l_skip(par);
		}
		else {
			node0._line = ln_cur(par);
			node0._last = lhs->_tail;

			lhs->_count += 1;
			lhs->_tail = &node0;

			l_scan_prefixed_expr(par, &node0._expr);
			return l_scan_normal_assign_frag(par, lhs);
		}
	}
	else if (l_test(par, TK_TDOT)) {
		lhs_multi = true;
		l_skip(par);
	}

	l_check(par, TK_ASSIGN, "'='");
	a_u32 line0 = ln_cur(par);
	l_skip(par);

	Exprs rhs = {};
	a_bool rhs_multi = false;

	l_scan_expr(par, &rhs._last);
	while (l_test(par, TK_COMMA)) {
		a_u32 line = ln_cur(par);
		l_skip(par);

		ai_code_multi(par, &rhs, OP_VA_PUSH, line);
		l_scan_expr(par, &rhs._last);
	}

	if (l_test(par, TK_TDOT)) {
		l_skip(par);
		rhs_multi = true;
	}

	if (!((lhs->_count >= rhs._count || lhs_multi) && (lhs->_count <= rhs._count || rhs_multi))) {
		l_error(par, "assign statement argument count mismatch.", lhs->_head._line);
	}

	LhsNode* node = lhs->_tail;

	if (!lhs_multi || lhs_wrap) {
		ai_code_take(par, &rhs, lhs->_count, OP_VA_FIT, line0);
	}
	else {
		ai_code_take(par, &rhs, lhs->_count, OP_VA_FILL, line0);
	}

	loop {
		ai_code_bind(par, &node->_expr, &rhs._last, node->_line);
		if ((node = node->_last) == null) break;
		ai_code_multi(par, &rhs, OP_VA_POP, node->_line);
	}
}

static void l_scan_normal_stat(Parser* par) {
	Lhs lhs = {};
	lhs._head._line = ln_cur(par);
	l_scan_expr(par, &lhs._head._expr);
	if (l_test(par, TK_COMMA) || l_test(par, TK_ASSIGN)) {
		lhs._tail = &lhs._head;
		l_scan_normal_assign_frag(par, &lhs);
	}
	ai_code_drop(par, &lhs._head._expr);
}

static void l_scan_if_stat(Parser* par) {
	l_skip(par);

	l_check(par, TK_LBK, "'('");
	a_u32 line1 = ln_cur(par);
	l_skip(par);

	Expr ep;
	l_scan_expr(par, &ep);
	a_u32 label1 = ai_code_testT(par, &ep, line1);
	ai_code_drop(par, &ep);

	l_check_pair_right(par, TK_LBK, "'('", TK_RBK, "')'", line1);
	l_skip(par);

	l_scan_stat(par);

	if (l_test(par, TK_ELSE)) {
		a_u32 label2 = ai_code_gotoU(par, NO_LABEL, ln_cur(par));
		l_skip(par);

		l_sync(par);
		ai_code_label(par, label1, ln_cur(par));
		l_scan_stat(par);

		ai_code_label(par, label2, ln_cur(par));
	}
	else {
		ai_code_label(par, label1, ln_cur(par));
	}
}

static void l_scan_let_stat(Parser* par) {
	l_skip(par);
	//TODO
}

static void l_scan_stat(Parser* par) {
	switch (l_peek(par)) {
		case TK_IF: {
			l_scan_if_stat(par);
			break;
		}
		default: {
			l_scan_normal_stat(par);
			break;
		}
	}
}

static void l_scan_stat_seq(Parser* par) {
	loop {
		switch (l_peek(par)) {
			case TK_RBK:
			case TK_EOF:
				return;
			case TK_LET: {
				l_scan_let_stat(par);
				break;
			}
			case TK_IF: {
				l_scan_if_stat(par);
				break;
			}
			default: {
				l_scan_normal_stat(par);
				break;
			}
		}
	}
}

static GFunMeta* l_scan_function(Parser* par, a_bool root) {
	FnScope scope;
	ai_code_prologue(par, &scope);
	l_scan_stat_seq(par);
	return ai_code_epilogue(par, root);
}

static void l_scan_root(unused a_henv env, void* ctx) {
	Parser* par = ctx;
	ai_code_open(par);
	l_scan_function(par, true);
}

a_msg ai_parse(a_henv env, a_ifun fun, void* ctx, char const* fname, a_u32 options, GFun** pfun) {
	Parser par = { _options: options };
	ai_io_iinit(env, fun, ctx, &par._lex._in);
	ai_lex_init(&par._lex, fname);
	rq_init(&par._rq);

	a_msg msg = ai_env_protect(env, l_scan_root, &par);

	if (msg == ALO_SOK) {
		*pfun = ai_code_build(&par);
	}

	return msg;
}