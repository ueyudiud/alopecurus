/**
 *@file aparse.c
 */

#define aparse_c_

#include "afun.h"
#include "aenv.h"
#include "aerr.h"
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

static a_bool l_test_skip(Parser* par, a_i32 tk) {
	if (l_test(par, tk)) {
		l_skip(par);
		return true;
	}
	return false;
}

a_none ai_par_report(Parser* par, a_bool eof, char const* fmt, ...) {
	va_list varg;
	ai_code_close(par);
	if (!eof || !(par->_options & ALO_COMP_OPT_CHECK_STMUF)) {
		va_start(varg, fmt);
		ai_err_raisevf(par->_env, ALO_ECHUNK, fmt, varg);
		va_end(varg);
	}
	else {
		ai_env_raise(par->_env, ALO_ESTMUF);
	}
}

#define l_error_got(par,fmt,args...) ({ \
	a_tksbuf _buf; \
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

static a_none l_error_bracket(Parser* par, a_i32 ltk, a_i32 rtk, a_u32 line) {
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

static void l_check_pair_right(Parser* par, a_i32 ltk, a_i32 rtk, a_u32 line) {
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
static void l_scan_expr_pack(Parser* par, InoutExpr e1, OutExpr e2);
static void l_scan_stat(Parser* par);
static void l_scan_stat_seq(Parser* par);

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
			ai_code_lookupU(par, e, tk_cur(par)->_str, ln_cur(par));
			l_skip(par);
			break;
		}
		case TK_LBK: {
			a_u32 line = ln_cur(par);
			l_skip(par);

			if (l_test_skip(par, TK_RBK)) {
				ai_code_constK(par, e, EXPR_UNIT, line);
			}
			else {
				l_scan_expr(par, e);
				if (!l_test_skip(par, TK_RBK)) {
					if (l_test_skip(par, TK_COMMA)) {
						Expr e2;
						l_scan_expr_pack(par, e, &e2);
						ai_code_binary2(par, e, &e2, OP_VA_PUSH, ln_cur(par));
					}
					l_check_pair_right(par, TK_LBK, TK_RBK, line);
				}
			}
			ai_code_unary(par, e, OP_TNEW, line);
			break;
		}
		default: {
			l_error_got(par, "expression expected");
		}
	}
}

static void l_scan_suffixed_expr(Parser* par, OutExpr e) {
	l_sync(par);
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
				while (l_test_skip(par, TK_COMMA)) {
					l_scan_expr(par, &e2);
					ai_code_index(par, e, &e2, line);
				}
				l_check_pair_right(par, TK_LSQ, TK_RSQ, line);
				break;
			}
			case TK_LBK: {
				a_u32 line = ln_cur(par);
				l_skip(par);

				if (!l_test_skip(par, TK_RBK)) {
					Expr e2;
					l_scan_expr_pack(par, e, &e2);
					l_check_pair_right(par, TK_LBK, TK_RBK, line);
					ai_code_binary2(par, e, &e2, OP_VA_PUSH, ln_cur(par));
				}
				ai_code_unary(par, e, OP_CALL, line);
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
			a_u32 line = ln_cur(par);
			l_skip(par);
			l_scan_prefixed_expr(par, e);
			ai_code_unary(par, e, OP_NEG, line);
			break;
		}
		case TK_TILDE: {
			a_u32 line = ln_cur(par);
			l_skip(par);
			l_scan_prefixed_expr(par, e);
			ai_code_unary(par, e, OP_BIT_INV, line);
			break;
		}
		case TK_NOT: {
			a_u32 line = ln_cur(par);
			l_skip(par);
			l_scan_prefixed_expr(par, e);
			ai_code_unary(par, e, OP_NOT, line);
			break;
		}
		case TK_SHARP: {
			a_u32 line = ln_cur(par);
			l_skip(par);
			l_scan_prefixed_expr(par, e);
			ai_code_unary(par, e, OP_LEN, line);
			break;
		}
		case TK_STAR: {
			a_u32 line = ln_cur(par);
			l_skip(par);
			l_scan_prefixed_expr(par, e);
			ai_code_unary(par, e, OP_UNBOX, line);
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
		ai_code_binary1(par, e, op, line);
		l_skip(par);
		l_scan_arith_expr(par, &e2, prios[op] + 1);
		ai_code_binary2(par, e, &e2, op, line);
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
	if (op1 == OP__NONE) return;

	l_skip(par);
	line1 = ln_cur(par);
	ai_code_binary1(par, e, op1, line1);

	Expr e2;
	l_scan_arith_expr(par, &e2, 0);
	ai_code_binary2(par, e, &e2, op1, line1);

	op2 = l_scan_compare_op(par);
	while (op2 != OP__NONE) {
		a_u32 line2;

		l_skip(par);
		line2 = ln_cur(par);
		ai_code_binary1(par, e, OP_AND, line2);
		ai_code_binary1(par, &e2, op2, line2);

		Expr e3;
		l_scan_arith_expr(par, &e3, 0);
		ai_code_binary2(par, &e2, &e3, op2, line2);

		ai_code_drop(par, &e3);
		ai_code_binary2(par, e, &e2, OP_AND, line2);

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
		ai_code_binary1(par, e, op, line);

		Expr e2;
		l_scan_compare_expr(par, &e2);
		ai_code_binary2(par, e, &e2, op, line);
		ai_code_drop(par, &e2);
	}
}

static void l_scan_concatenate_expr(Parser* par, OutExpr e) {
	a_u32 line1 = ln_cur(par);
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
		l_scan_concatenate_expr(par, &e2);
		ai_code_binary2(par, e, &e2, OP_AND, ln_cur(par));
	}
}

static void l_scan_disjunctive_expr(Parser* par, OutExpr e) {
	l_scan_conjunctive_expr(par, e);
	while (l_test(par, TK_BBAR)) {
		Expr e2;
		ai_code_binary1(par, e, OP_OR, ln_cur(par));
		l_skip(par);
		l_scan_conjunctive_expr(par, &e2);
		ai_code_binary2(par, e, &e2, OP_OR, ln_cur(par));
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

			l_check_skip(par, TK_COLON);

			Expr e2;
			ai_code_label(par, label1, line);
			l_scan_ternary_expr(par, &e2);
			ai_code_merge(par, e, &e2, label2, line);
			break;
		}
		case TK_ELVIS: {
			a_u32 line = ln_cur(par);
			a_u32 label1 = ai_code_testT(par, e, line);

			l_skip(par);

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

static void l_scan_expr_pack(Parser* par, InoutExpr e1, OutExpr e2) {
	l_scan_expr(par, e2);
	while (l_test_skip(par, TK_COMMA)) {
		ai_code_binary2(par, e1, e2, OP_VA_PUSH, ln_cur(par));
		l_scan_expr(par, e2);
		if (l_test_skip(par, TK_TDOT)) {
			ai_code_unary(par, e2, OP_UNPACK, ln_cur(par));
			break;
		}
	}
}

typedef struct Lhs Lhs;
typedef struct LhsNode LhsNode;

struct LhsNode {
	Expr _expr;
	LhsNode* _last;
};

struct Lhs {
	LhsNode* _tail;
	a_u32 _count;
	LhsNode _head;
};

static void l_scan_normal_assign_frag(Parser* par, Lhs* lhs) {
	LhsNode n = {};

	if (l_test_skip(par, TK_COMMA)) {
		n._last = lhs->_tail;

		lhs->_count += 1;
		lhs->_tail = &n;

		l_scan_prefixed_expr(par, &n._expr);
		return l_scan_normal_assign_frag(par, lhs);
	}

	lhs->_count += 1;

	l_check_skip(par, TK_ASSIGN);
	a_u32 line = ln_cur(par);

	Expr rhs = {};
	Expr rhs_last;

	l_scan_expr_pack(par, &rhs, &rhs_last);
	if (!ai_code_balance(par, &rhs, &rhs_last, lhs->_count, line)) {
		ai_par_error(par, "unbalanced assignment.", line);
	}

	LhsNode* node = lhs->_tail;
	loop {
		ai_code_bind(par, &node->_expr, &rhs_last, line);
		if ((node = node->_last) == null)
			return;
		ai_code_multi(par, &rhs, &rhs_last, OP_VA_POP, line);
	}
}

static void l_scan_normal_stat(Parser* par) {
	Lhs lhs = {};
	l_scan_expr(par, &lhs._head._expr);
	if (l_test(par, TK_COMMA) || l_test_skip(par, TK_TDOT) || l_test(par, TK_ASSIGN)) {
		lhs._tail = &lhs._head;
		l_scan_normal_assign_frag(par, &lhs);
	}

	l_test_skip(par, TK_SEMI);
	ai_code_drop(par, &lhs._head._expr);
}

static void l_scan_if_stat(Parser* par) {
	l_skip(par);

	l_check_skip(par, TK_LBK);
	a_u32 line = ln_cur(par);

	Expr ep;
	l_scan_expr(par, &ep);
	a_u32 label1 = ai_code_testT(par, &ep, line);
	ai_code_drop(par, &ep);

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
	a_u32 line = ln_cur(par);

	a_u32 label1 = ai_code_label(par, NO_LABEL, line);

	Expr ep;
	l_scan_expr(par, &ep);
	a_u32 label2 = ai_code_testT(par, &ep, line);
	ai_code_drop(par, &ep);

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
	a_u32 line = ln_cur(par);

	l_skip(par);

	Expr e = {};

	if (!l_test_sep(par)) {
		Expr e2 = {};
		l_scan_expr_pack(par, &e, &e2);
		ai_code_binary2(par, &e, &e2, OP_VA_PUSH, line);
	}

	l_test_skip(par, TK_SEMI);
	ai_code_unary(par, &e, OP_RETURN, line);
}

static void l_scan_let_rhs(Parser* par, LetStat* stat, a_u32 line) {
	Expr e0 = {}, e;

	ai_code_label(par, stat->_label_test, line);

	l_scan_expr(par, &e);
	while (ai_code_let_bind(par, stat, &e)) {
		if (!l_test_skip(par, TK_COMMA)) {
			ai_code_let_nils(par, stat, line);
			return;
		}
		l_scan_expr(par, &e);
		if (l_test_skip(par, TK_TDOT)) {
			ai_code_unary(par, &e, OP_UNPACK, ln_cur(par));
			if (!ai_code_balance(par, &e, &e0, stat->_nnode, line)) {
				ai_par_error(par, "unbalanced binding.", line);
			}
			while (ai_code_let_bind(par, stat, &e)) {
				ai_code_multi(par, &e0, &e, OP_VA_POP, line);
			}
			return;
		}
	}

	while (l_test_skip(par, TK_COMMA)) {
		l_scan_expr(par, &e);
		ai_code_drop(par, &e);
		if (l_test_skip(par, TK_TDOT))
			break;
	}
}

#define TAG_BITS_PACK 0x03

#define TAG_PACK_ROOT 0x00
#define TAG_PACK_TUPLE 0x01
#define TAG_PACK_LIST 0x02
#define TAG_PACK_DICT 0x03

static a_none l_unmatched_pattern(Parser* par, LetNode* node) {
	if (node == null)
		l_error_got(par, "malformed pattern");
	switch (node->_kind) {
		case PAT_TUPLE:
			l_error_bracket(par, '(', ')', node->_expr._line);
		case PAT_LIST:
			l_error_bracket(par, '[', ']', node->_expr._line);
		case PAT_DICT:
			l_error_bracket(par, '{', '}', node->_expr._line);
		default:
			unreachable();
	}
}

static void l_scan_pattern(Parser* par, LetStat* stat, LetNode* parent, LetNode** slot, a_u32 tag) {
	LetNode node_buf = {};
	LetNode* const node = &node_buf;

	stat->_nnode += 1;
	*slot = node;
	node->_parent = parent;

	switch (tag) {
		case TAG_PACK_ROOT:
		case TAG_PACK_TUPLE:
			goto branch_standard;
		default:
			unreachable();
	}

branch_standard:
	switch (l_peek(par)) {
		case TK_NIL: {
			ai_code_constK(par, &node->_expr, EXPR_NIL, ln_cur(par));
			l_skip(par);
			break;
		}
		case TK_INTEGER: {
			ai_code_constI(par, &node->_expr, tk_cur(par)->_int, ln_cur(par));
			l_skip(par);
			break;
		}
		case TK_FLOAT: {
			ai_code_constF(par, &node->_expr, tk_cur(par)->_float, ln_cur(par));
			l_skip(par);
			break;
		}
		case TK_STRING: {
			ai_code_constS(par, &node->_expr, tk_cur(par)->_str, ln_cur(par));
			l_skip(par);
			break;
		}
		case TK_IDENT: {
			l_skip(par);
			node->_expr = new(Expr) {
				._kind = PAT_BIND,
				._line = ln_cur(par),
				._str = tk_cur(par)->_str
			};
			break;
		}
		case TK_UNDERSCORE: {
			l_skip(par);
			node->_kind = PAT_DROP;
			node->_line = ln_cur(par);
			break;
		}
		case TK_LBK: {
			node->_kind = PAT_TUPLE;
			node->_line = ln_cur(par);
			node->_succ_tag = TAG_PACK_TUPLE;
			l_skip(par);
			if (!l_test_skip(par, TK_RBK)) {
				return l_scan_pattern(par, stat, node, &node->_child, TAG_PACK_TUPLE);
			}
			break;
		}
		case TK_LSQ: {
			node->_kind = PAT_LIST;
			node->_line = ln_cur(par);
			node->_succ_tag = TAG_PACK_LIST;
			l_skip(par);
			if (!l_test_skip(par, TK_RSQ)) {
				return l_scan_pattern(par, stat, node, &node->_child, TAG_PACK_LIST);
			}
			break;
		}
		case TK_LBR: {
			node->_kind = PAT_DICT;
			node->_line = ln_cur(par);
			node->_succ_tag = TAG_PACK_DICT;
			l_skip(par);
			if (!l_test_skip(par, TK_RBR)) {
				return l_scan_pattern(par, stat, node, &node->_child, TAG_PACK_DICT);
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
				if (parent != null) {
					tag = parent->_succ_tag;
				}
				else {
					assume(tag == TAG_PACK_ROOT);
				}
				return l_scan_pattern(par, stat, parent, slot, tag);
			}
			case TK_RBK: {
				if (parent == null || parent->_kind != PAT_TUPLE)
					l_unmatched_pattern(par, parent);
				l_skip(par);
				break;
			}
			case TK_RSQ: {
				if (parent == null || parent->_kind != PAT_LIST)
					l_unmatched_pattern(par, parent);
				l_skip(par);
				break;
			}
			case TK_RBR: {
				if (parent == null || parent->_kind != PAT_DICT)
					l_unmatched_pattern(par, parent);
				l_skip(par);
				break;
			}
			case TK_ASSIGN: {
				l_skip(par);
				if (parent == null) {
					return l_scan_let_rhs(par, stat, ln_cur(par));
				}
				else {
					//TODO
					goto error;
				}
			}
			default:
				goto error;
		}

		assume(parent != null);
		tag = parent->_succ_tag;
		slot = &parent->_sibling;
		parent = parent->_parent;
	}

error:
	l_error_got(par, "malformed pattern");
}

static void l_scan_let_clause(Parser* par, a_u32 line) {
	LetStat stat = { };

	stat._label_test = ai_code_gotoU(par, NO_LABEL, line);
	l_scan_pattern(par, &stat, NULL, &stat._head, TAG_PACK_ROOT);
}

static void l_scan_let_stat(Parser* par) {
	a_u32 line = ln_cur(par);
	l_skip(par);

	switch (l_peek(par)) {
		case TK_FN: {
			//TODO
			break;
		}
		default: {
			l_scan_let_clause(par, line);
			break;
		}
	}
}

static void l_scan_stat_pack(Parser* par) {
	Scope scope;

	a_u32 line = ln_cur(par);
	l_skip(par);
	ai_code_enter(par, &scope);

	l_scan_stat_seq(par);

	ai_code_leave(par);

	l_check_pair_right(par, TK_LBR, TK_RBR, line);
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
		case TK_LBR: {
			l_scan_stat_pack(par);
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
			default: {
				l_scan_normal_stat(par);
				break;
			}
		}
	}
}

static void l_scan_root(unused a_henv env, void* ctx) {
	FnScope scope;

	Parser* par = ctx;
	ai_code_open(par);

	ai_code_prologue(par, &scope);
	l_scan_stat_seq(par);

	if (!l_test(par, TK_EOF)) {
		l_error_got(par, "statement expected");
	}

	ai_code_epilogue(par, true, ln_cur(par));
}

a_msg ai_parse(a_henv env, a_ifun fun, void* ctx, char const* fname, a_u32 options, GFun** pfun) {
	Parser par = { ._options =  options };
	ai_io_iinit(env, fun, ctx, &par._lex._in);
	ai_lex_init(&par._lex, fname);
	rq_init(&par._rq);

	a_msg msg = ai_env_protect(env, l_scan_root, &par);

	if (msg == ALO_SOK) {
		*pfun = ai_code_build(&par);
	}

	return msg;
}