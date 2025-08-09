/**
 *@file aparse.c
 */

#define aparse_c_
#define ALO_LIB

#include "aop.h"
#include "abc.h"
#include "afun.h"
#include "aenv.h"
#include "afmt.h"
#include "agc.h"
#include "aerr.h"
#include "aapi.h"
#include "acg.h"

#include "aparse.h"

#define lex_line(par) (lex(par)->ahead[0].line)
#define lex_token(par) (&lex(par)->ahead[0])

static a_i32 lex_peek(Parser* par) {
    Lexer* lex = lex(par);
    if (lex->ahead[0].tag == TK__NONE) {
        ai_lex_next(lex);
    }
	return lex->ahead[0].tag;
}

static a_i32 lex_peek2(Parser* par) {
    Lexer* lex = lex(par);
    if (lex->ahead[1].tag == TK__NONE) {
        ai_lex_next2(lex);
    }
    return lex->ahead[1].tag;
}

static a_i32 lex_peek_dqstr(Parser* par, a_line line) {
    Lexer* lex = lex(par);
    if (lex->ahead[0].tag == TK__NONE) {
        ai_lex_next_dqstr(lex(par), line);
    }
    return lex->ahead[0].tag;
}

static void lex_skip(Parser* par) {
	lex_token(par)->tag = TK__NONE;
}

static GStr* lex_ident(Parser* par) {
	Token* token = lex_token(par);
	assume(token->tag == TK_IDENT);
	return token->as_str;
}

static a_bool lex_test(Parser* par, a_i32 tk) {
	return lex_peek(par) == tk;
}

static a_bool lex_test2(Parser* par, a_i32 tk) {
    return lex_peek2(par) == tk;
}

static a_bool lex_test_skip(Parser* par, a_i32 tk) {
	if (lex_test(par, tk)) {
        lex_skip(par);
		return true;
	}
	return false;
}

#define lex_error_got(par,fmt,args...) ({ \
	a_tkbuf _buf; \
    parse_error(par, fmt", got %s", lex_line(par), ##args, ai_lex_tkrepr(lex_token(par), _buf)); \
})

static void lex_error_expected(Parser* par, a_i32 tk) {
	lex_error_got(par, "%s expected", ai_lex_tagname(tk));
}

static void lex_check_skip(Parser* par, a_i32 tk) {
	if (!lex_test(par, tk)) {
        lex_error_expected(par, tk);
	}
    lex_skip(par);
}

static GStr* lex_check_ident(Parser* par) {
	if (!lex_test(par, TK_IDENT)) {
        lex_error_expected(par, TK_IDENT);
	}
	GStr* ident = lex_ident(par);
    lex_skip(par);
	return ident;
}

static a_noret lex_error_bracket(Parser* par, a_i32 ltk, a_i32 rtk, a_line line) {
	if (lex_line(par) == line) {
		lex_error_got(par, "%s expected to match %s",
                      ai_lex_tagname(rtk),
                      ai_lex_tagname(ltk));
	}
	else {
		lex_error_got(par, "%s expected to match %s at line %u",
                      ai_lex_tagname(rtk),
                      ai_lex_tagname(ltk),
                      line);
	}
}

static void lex_check_pair_right(Parser* par, a_i32 ltk, a_i32 rtk, a_line line) {
	if (!lex_test(par, rtk))
        lex_error_bracket(par, ltk, rtk, line);
    lex_skip(par);
}

static a_bool lex_test_sep(Parser* par) {
	switch (lex_peek(par)) {
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

static void scan_atom(Parser* par, Expr* e);
static void scan_expr(Parser* par, Expr* e);
static void scan_exprs(Parser* par, Expr* es, a_bool exists);
static void scan_pat(Parser* par, void (*con)(Parser*, Pat*, a_usize), a_usize ctx);
static void scan_stat(Parser* par);
static void scan_stats(Parser* par);

static GStr* scan_label(Parser* par) {
    return lex_test_skip(par, TK_COLON) ? lex_check_ident(par) : null;
}

static GStr* scan_label_for_scope(Parser* par) {
    GStr* name = scan_label(par);
    par->scope->label_name = name;
    return name;
}

static a_bool scan_dqstr_frag(Parser* par, Expr* e, a_u32 line) {
    switch (lex_peek_dqstr(par, line)) {
        case TK_STRING: {
            expr_str(e, lex_token(par)->as_str, lex_line(par));
            lex_skip(par);
            return false;
        }
        case TK_TSESCAPE: {
            lex_skip(par);
            scan_atom(par, e);
            return false;
        }
        case TK_TSFINISH: {
            lex_skip(par);
            return true;
        }
        default: unreachable();
    }
}

static void scan_dqstr(Parser* par, Expr* e) {
	ConExpr ce;
	a_u32 line = lex_line(par);
    scan_dqstr_frag(par, e, line);
    ai_cg_str_init(par, &ce, e, line);

	while (!scan_dqstr_frag(par, e, line)) {
        ai_cg_str_cat(par, &ce, e, lex_line(par));
	}

    ai_cg_str_build(par, &ce, e, lex_line(par));
}

static void scan_function(Parser* par, Expr* e, GStr* name, a_line line) {
	FnScope scope;

    ai_cg_func_start(par, &scope, line);

	a_line line1 = lex_line(par);
	lex_check_skip(par, TK_LBK);
    if (!lex_test_skip(par, TK_RBK)) {
        scan_pat(par, ai_cg_bind_args, line1);
        lex_check_pair_right(par, TK_LBK, TK_RBK, line1);
    }

	switch (lex_peek(par)) {
		case TK_LBR: {
			lex_skip(par);
            scan_stats(par);
    		lex_check_pair_right(par, TK_LBR, TK_RBR, line1);
			break;
		}
		case TK_ASSIGN: {
			lex_skip(par);
            scan_expr(par, e);
			ai_cg_return(par, e, line1);
			break;
		}
        default: lex_error_got(par, "function expected");
	}

	ai_cg_func_end(par, e, name, lex_line(par));
}

static void scan_lambda(Parser* par, Expr* e) {
	FnScope scope;

    ai_cg_func_start(par, &scope, lex_line(par));

	if (!lex_test_skip(par, TK_BBAR)) {
		a_line line = lex_line(par);
		lex_check_skip(par, TK_BAR);
        scan_pat(par, ai_cg_bind_args, scope.begin_line);
		lex_check_pair_right(par, TK_BAR, TK_BAR, line);
	}

	if (lex_test_skip(par, TK_LBR)) {
		a_line line = lex_line(par);

        scan_stats(par);
        lex_check_pair_right(par, TK_LBR, TK_RBR, line);
	}
	else {
        Expr e2[1];

        scan_expr(par, e2);
		ai_cg_return(par, e2, scope.begin_line);
	}

	ai_cg_func_end(par, e, null, lex_line(par));
}

static void scan_table_constructor(Parser* par, Expr* e) {
    a_u32 line = lex_line(par);
    Scope scope;
    Expr er[1];
    Expr ek[1];
    Expr ev[1];

    lex_skip(par);
    if (lex_test_skip(par, TK_RBR)) {
        ai_cg_table_new(par, e, line);
        return;
    }

    ai_cg_block_start(par, &scope, line, 0);

    ai_cg_local(par, e, null, (ExprMod) { }, line);
    ai_cg_table_new(par, er, line);
    ai_cg_assign(par, e, er, line);

    a_int index = 0;

    do {
        expr_copy(er, e);

        if (lex_test(par, TK_IDENT) && lex_test2(par, TK_ASSIGN)) {
            a_line line2 = lex_line(par);

            ai_cg_index_str(par, er, lex_ident(par), line2);
            lex_skip(par);
            lex_skip(par);

            scan_expr(par, ev);

            ai_cg_assign(par, er, ev, line2);
        }
        else {
            a_line line2 = lex_line(par);
            scan_expr(par, ek);
            if (lex_test_skip(par, TK_COLON)) {
                line2 = lex_line(par);
                scan_expr(par, ev);
                ai_cg_index(par, er, ek, line2);
            }
            else {
                ai_cg_index_int(par, er, index++, line2);
                ai_cg_assign(par, er, ek, line2);
            }
        }
    }
    while (lex_test_skip(par, TK_COMMA) || lex_test_skip(par, TK_SEMI));

    lex_check_pair_right(par, TK_LBR, TK_RBR, line);

    ai_cg_block_end_with(par, lex_line(par), e);
}

static void scan_atom(Parser* par, Expr* e) {
	switch (lex_peek(par)) {
		case TK_nil: {
			expr_nil(e, lex_line(par));
            lex_skip(par);
			break;
		}
		case TK_false: {
			expr_bool(e, false, lex_line(par));
            lex_skip(par);
			break;
		}
		case TK_true: {
			expr_bool(e, true, lex_line(par));
            lex_skip(par);
			break;
		}
		case TK_INTEGER: {
			expr_int(e, lex_token(par)->as_int, lex_line(par));
            lex_skip(par);
			break;
		}
		case TK_FLOAT: {
			expr_float(e, lex_token(par)->as_float, lex_line(par));
            lex_skip(par);
			break;
		}
		case TK_STRING: {
            if (lex(par)->ahead[1].tag != TK_TSESCAPE) {
                expr_str(e, lex_token(par)->as_str, lex_line(par));
                lex_skip(par);
                break;
            }
            fallthrough;
        }
        case TK_TSESCAPE: {
            scan_dqstr(par, e);
            break;
        }
		case TK_IDENT: {
            ai_cg_symbol(par, e, lex_ident(par), lex_line(par));
            lex_skip(par);
			break;
		}
		case TK_LBK: {
			a_u32 line = lex_line(par);
            lex_skip(par);

			if (lex_test_skip(par, TK_RBK)) {
				expr_unit(e);
                ai_cg_tuple_box(par, e, line);
			}
			else {
                scan_expr(par, e);
				if (!lex_test_skip(par, TK_RBK)) {
					if (lex_test_skip(par, TK_COMMA) && !lex_test(par, TK_RBK)) {
                        scan_exprs(par, e, true);
					}
                    lex_check_pair_right(par, TK_LBK, TK_RBK, line);
                    ai_cg_tuple_box(par, e, line);
				}
			}
			break;
		}
		case TK_LSQ: {
			a_u32 line = lex_line(par);
            lex_skip(par);
			if (!lex_test_skip(par, TK_RSQ)) {
                scan_exprs(par, e, false);
                lex_check_pair_right(par, TK_LSQ, TK_RSQ, line);
			}
			else {
				expr_unit(e);
			}
            ai_cg_list_box(par, e, line);
			break;
		}
		case TK_LBR: {
            scan_table_constructor(par, e);
			break;
		}
		default: {
			lex_error_got(par, "expression expected");
		}
	}
}

static void scan_term_suffix(Parser* par, Expr* e, a_line line) {
	a_u32 label = NO_LABEL;
	loop {
		switch (lex_peek(par)) {
			case TK_QDOT: {
				a_line line2 = lex_line(par);
                lex_skip(par);
                ai_cg_test_ne_nil(par, e, &label, line2);
				GStr* name = lex_check_ident(par);
                if (lex_test_skip(par, TK_LBK)) {
                    ai_cg_lookup(par, e, name, line2);

                    if (!lex_test_skip(par, TK_RBK)) {
                        scan_exprs(par, e, true);
                        lex_check_pair_right(par, TK_LBK, TK_RBK, line2);
                    }

                    ai_cg_call(par, e, line2);
                }
                else {
                    ai_cg_index_str(par, e, name, line2);
                }
				break;
			}
			case TK_BANG: {
				a_line line2 = lex_line(par);
                lex_skip(par);

                ai_cg_check_not_nil(par, e, line2);
				break;
			}
			case TK_DOT: {
				a_line line2 = lex_line(par);
                lex_skip(par);
				GStr* name = lex_check_ident(par);
                if (lex_test_skip(par, TK_LBK)) {
                    ai_cg_lookup(par, e, name, line2);

                    if (!lex_test_skip(par, TK_RBK)) {
                        scan_exprs(par, e, true);
                        lex_check_pair_right(par, TK_LBK, TK_RBK, line2);
                    }

                    ai_cg_call(par, e, line2);
                }
                else {
                    ai_cg_index_str(par, e, name, line2);
                }
				break;
			}
			case TK_LSQ: {
                Expr e2[1] = {};
				a_line line2 = lex_line(par);
                lex_skip(par);
                scan_expr(par, e2);
                ai_cg_index(par, e, e2, line2);
				while (lex_test_skip(par, TK_COMMA)) {
                    scan_expr(par, e2);
                    ai_cg_index(par, e, e2, line2);
				}
                lex_check_pair_right(par, TK_LSQ, TK_RSQ, line2);
				break;
			}
			case TK_LBK: {
				a_line line2 = lex_line(par);
                lex_skip(par);

				if (!lex_test_skip(par, TK_RBK)) {
                    scan_exprs(par, e, true);
                    lex_check_pair_right(par, TK_LBK, TK_RBK, line2);
				}
                ai_cg_call(par, e, line2);
				break;
			}
			default: {
                ai_cg_merge_nil(par, e, label, line);
				return;
			}
		}
	}
}

static void scan_term(Parser* par, Expr* e) {
	if (lex_test(par, TK_BAR) || lex_test(par, TK_BBAR)) {
        scan_lambda(par, e);
	}
	else {
		a_line line = lex_line(par);
        scan_atom(par, e);
        scan_term_suffix(par, e, line);
	}
}

static void scan_term_with_prefix(Parser* par, Expr* e) {
    a_i32 tag = lex_peek(par);
    a_line line = lex_line(par);
	switch (tag) {
		case TK_PLUS: {
			//TODO should VM check type for this operation?
            lex_skip(par);
            scan_term_with_prefix(par, e);
			break;
		}
		case TK_MINUS: {
            lex_skip(par);
            scan_term_with_prefix(par, e);
            ai_cg_neg(par, e, line);
			break;
		}
		case TK_TILDE: {
            lex_skip(par);
            scan_term_with_prefix(par, e);
            ai_cg_bit_inv(par, e, line);
			break;
		}
		case TK_BANG: {
            lex_skip(par);
            scan_term_with_prefix(par, e);
			ai_cg_not(par, e, line);
			break;
		}
		case TK_SHARP: {
            lex_skip(par);
            scan_term_with_prefix(par, e);
            ai_cg_len(par, e, line);
			break;
		}
		case TK_STAR: {
            lex_skip(par);
            scan_term_with_prefix(par, e);
            ai_cg_unbox(par, e, line);
			break;
		}
		case TK_fn: {
            lex_skip(par);
            scan_function(par, e, null, line);
			break;
		}
		default: {
            scan_term(par, e);
			break;
		}
	}
}

static void scan_terms(Parser *par, Expr* e) {
    scan_term_with_prefix(par, e);
	if (lex_test_skip(par, TK_TDOT)) {
        ai_cg_unpack(par, e, lex_line(par));
	}
}

static a_u32 scan_arith_op(Parser* par) {
	switch (lex_peek(par)) {
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
	}
	return OP__NOT_BIN;
}

static void scan_arith_expr_leveled(Parser* par, Expr* e, a_u32 lv_min) {
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
    a_u32 lv_max = UINT32_MAX;
    a_u32 lv;

    scan_terms(par, e);

    while ((op = scan_arith_op(par)) != OP__NOT_BIN) {
        lv = prios[op];
        if (!(lv_min <= lv && lv <= lv_max))
            break;

        Expr e2[1];
        a_line line = lex_line(par);
        ai_cg_binary_left(par, e, op, line);
        lex_skip(par);
        scan_arith_expr_leveled(par, e2, lv + 1);
        ai_cg_binary_right(par, e, e2, op, line);

        lv_max = lv;
    }
}

#define scan_arith_expr(par,e) scan_arith_expr_leveled(par, e, 0)

static a_u32 scan_compare_op(Parser* par) {
	switch (lex_peek(par)) {
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

static void scan_compare_expr(Parser* par, Expr* e);

static void scan_compare_expr(Parser* par, Expr* e) {
    a_u32 op1, op2;
    a_line line1;

    scan_arith_expr(par, e);
    op1 = scan_compare_op(par);
    if (op1 == 0) return;

    lex_skip(par);
    line1 = lex_line(par);
    ai_cg_binary_left(par, e, op1, line1);

    Expr e2[1];
    scan_arith_expr(par, e2);
    ai_cg_binary_right(par, e, e2, op1, line1);

    op2 = scan_compare_op(par);
    while (op2 != OP__NOT_BIN) {
        a_u32 line2;

        lex_skip(par);
        line2 = lex_line(par);
        ai_cg_binary_left(par, e, OP_AND, line2);
        ai_cg_binary_left(par, e2, op2, line2);

        Expr e3[1];
        scan_arith_expr(par, e3);
        ai_cg_binary_right(par, e2, e3, op2, line2);

        ai_cg_wild(par, e3);
        ai_cg_binary_right(par, e, e2, OP_AND, line2);

        op2 = scan_compare_op(par);
    }
}

static a_u32 scan_relation_op(Parser* par) {
	switch (lex_peek(par)) {
		case TK_EQ:
			return OP_EQ;
		case TK_NE:
			return OP_NE;
		case TK_is:
			return OP_IS;
		case TK_in:
			return OP_IN;
		case TK_BANG:
			switch (lex_peek2(par)) {
				case TK_is:
                    lex_skip(par);
					return OP_IS_NOT;
				case TK_in:
                    lex_skip(par);
					return OP_NOT_IN;
			}
			break;
	}
	return OP__NOT_BIN;
}

static void scan_relation_expr(Parser* par, Expr* e) {
    scan_compare_expr(par, e);
    a_u32 op = scan_relation_op(par);
    if (op != 0) {
        a_u32 line = lex_line(par);
        lex_skip(par);
        ai_cg_binary_left(par, e, op, line);

        Expr e2[1];
        scan_compare_expr(par, e2);
        ai_cg_binary_right(par, e, e2, op, line);
    }
}

static void scan_concatenate_expr(Parser* par, Expr* e) {
    scan_relation_expr(par, e);
    a_line line1 = lex_line(par);
    if (lex_test_skip(par, TK_BDOT)) {
        ConExpr ce;
        ai_cg_str_init(par, &ce, e, line1);
        do {
            a_u32 line2 = lex_line(par);
            scan_relation_expr(par, e);
            ai_cg_str_cat(par, &ce, e, line2);
        }
        while (lex_test_skip(par, TK_BDOT));
        ai_cg_str_build(par, &ce, e, line1);
    }
}

static void scan_conjunctive_expr(Parser* par, Expr* e) {
    scan_concatenate_expr(par, e);
    while (lex_test(par, TK_BAMP)) {
        Expr e2[1];
        ai_cg_binary_left(par, e, OP_AND, lex_line(par));
        lex_skip(par);
        scan_concatenate_expr(par, e2);
        ai_cg_binary_right(par, e, e2, OP_AND, lex_line(par));
    }
}

static void scan_disjunctive_expr(Parser* par, Expr* e) {
    scan_conjunctive_expr(par, e);
    while (lex_test(par, TK_BBAR)) {
        Expr e2[1];
        ai_cg_binary_left(par, e, OP_OR, lex_line(par));
        lex_skip(par);
        scan_conjunctive_expr(par, e2);
        ai_cg_binary_right(par, e, e2, OP_OR, lex_line(par));
    }
}

static void scan_ternary_expr(Parser* par, Expr* e) {
    scan_disjunctive_expr(par, e);
    switch (lex_peek(par)) {
        case TK_QUESTION: {
            a_line line = lex_line(par);
            a_u32 label1 = ai_cg_test_true(par, e, line);

            lex_skip(par);

            ai_cg_wild(par, e);
            scan_ternary_expr(par, e);

            lex_check_pair_right(par, TK_QUESTION, TK_COLON, line);

            a_u32 label2 = NO_LABEL;
            ai_cg_break_and_case(par, e, &label2, line);

            Expr e2[1];
            ai_cg_mark_label(par, label1, line);
            scan_ternary_expr(par, e2);
            ai_cg_merge(par, e, e2, label2, line);
            break;
        }
        case TK_ELVIS: {
            a_line line = lex_line(par);
            a_u32 label1 = ai_cg_test_true(par, e, line);

            lex_skip(par);

            a_u32 label2 = NO_LABEL;
            ai_cg_break_and_case(par, e, &label2, line);

            Expr e2[1];
            ai_cg_mark_label(par, label1, line);
            scan_ternary_expr(par, e2);
            ai_cg_merge(par, e, e2, label2, line);
            break;
        }
        case TK_BQUESTION: {
            a_line line = lex_line(par);
            a_u32 label1 = NO_LABEL;
            ai_cg_test_eq_nil(par, e, &label1, line);

            lex_skip(par);

            a_u32 label2 = NO_LABEL;
            ai_cg_break_and_case(par, e, &label2, line);

            Expr e2[1];
            ai_cg_mark_label(par, label1, line);
            scan_ternary_expr(par, e2);
            ai_cg_merge(par, e, e2, label2, line);
            break;
        }
    }
}

static void scan_expr(Parser* par, Expr* e) {
    scan_ternary_expr(par, e);
}

static void scan_exprs(Parser* par, Expr* es, a_bool exists) {
	if (!exists) {
        scan_expr(par, es);
		if (!lex_test_skip(par, TK_COMMA))
			return;
	}

    Expr e[1];

	do {
        ai_cg_vec_push_left(par, es);
        scan_expr(par, e);
        ai_cg_vec_push_right(par, es, e);
	}
	while (lex_test_skip(par, TK_COMMA));
}

typedef struct Lhs Lhs;
typedef struct LhsNode LhsNode;

struct LhsNode {
    Expr expr[1];
	LhsNode* last;
};

struct Lhs {
	LhsNode head;
	LhsNode* tail;
	a_u32 count;
};

static void scan_assign_rhs(Parser* par, LhsNode* tail, a_u32 count) {
    lex_check_skip(par, TK_ASSIGN);
	a_u32 line = lex_line(par);

    Expr rhs[1];
    Expr rhs_last[1];

    ai_cg_vec_init(par, rhs);
    scan_expr(par, rhs_last);
	if (lex_test_skip(par, TK_COMMA)) {

		expr_copy(rhs, rhs_last);

		loop {
            ai_cg_vec_push_left(par, rhs);
            scan_expr(par, rhs_last);

			if (!lex_test_skip(par, TK_COMMA))
				break;

            ai_cg_vec_push_right(par, rhs, rhs_last);
		}
	}

	a_u32 num_nil = ai_cg_vec_trim(par, rhs, rhs_last, count, line);

	LhsNode* node = tail;

	while (num_nil > 0) {
        Expr en[1];
		expr_nil(en, line);
        ai_cg_assign(par, node->expr, en, line);

		node = node->last;
		assume(node != null);
		num_nil -= 1;
	}

	loop {
        ai_cg_assign(par, node->expr, rhs_last, line);
		if ((node = node->last) == null)
			return;

        ai_cg_vec_pop(par, rhs, rhs_last, line);
	}
}

static void scan_assign_lhs_tail(Parser* par, Lhs* lhs) {
	LhsNode n = {};

	if (lex_test_skip(par, TK_COMMA)) {
		n.last = lhs->tail;

		lhs->count += 1;
		lhs->tail = &n;

        scan_term_with_prefix(par, n.expr);
		return scan_assign_lhs_tail(par, lhs);
	}

    scan_assign_rhs(par, lhs->tail, lhs->count + 1);
}

static void scan_assign_or_call(Parser* par) {
	Lhs lhs = {};

	a_line line = lex_line(par);
    scan_term(par, lhs.head.expr);
	if (lex_test(par, TK_COMMA) || lex_test_skip(par, TK_TDOT) || lex_test(par, TK_ASSIGN)) {
		lhs.tail = &lhs.head;
        scan_assign_lhs_tail(par, &lhs);
	}
	else if (lhs.head.expr->tag != EXPR_CALL) {
		parse_error(par, "assignment or function call expected.", line);
	}

    lex_test_skip(par, TK_SEMI);
    ai_cg_wild(par, lhs.head.expr);
}

static void scan_do_stat(Parser* par) {
    Scope scope;
    lex_skip(par);

    lex_check_skip(par, TK_LBR);
    a_line line = lex_line(par);

    ai_cg_block_start(par, &scope, line, 0);
    scan_stats(par);

    lex_check_pair_right(par, TK_LBR, TK_RBR, line);
    ai_cg_block_end(par, lex_line(par));
}

static void scan_if_stat(Parser* par) {
	lex_skip(par);

	lex_check_skip(par, TK_LBK);
	a_line line = lex_line(par);

    Expr ep[1];
    scan_expr(par, ep);
	a_u32 label1 = ai_cg_test_true(par, ep, line);
    ai_cg_wild(par, ep);

	lex_check_pair_right(par, TK_LBK, TK_RBK, line);
	lex_skip(par);

    scan_stat(par);

	if (lex_test_skip(par, TK_else)) {
		a_u32 label2 = ai_cg_jump_lazily(par, NO_LABEL, lex_line(par));

		lex_peek(par);
        ai_cg_mark_label(par, label1, lex_line(par));
        scan_stat(par);

        ai_cg_mark_label(par, label2, lex_line(par));
	}
	else {
        ai_cg_mark_label(par, label1, lex_line(par));
	}
}

static void scan_while_stat(Parser* par) {
    Scope scope;
    ai_cg_block_start(par, &scope, lex_line(par), JMP_PROP_BREAK | JMP_PROP_CONTINUE);

    lex_skip(par);

    scan_label_for_scope(par);

    lex_check_skip(par, TK_LBK);
	a_line line = lex_line(par);

	a_u32 label1 = ai_cg_mark_label(par, NO_LABEL, line);

    Expr ep[1];
    scan_expr(par, ep);
	a_u32 label2 = ai_cg_test_true(par, ep, line);
    ai_cg_wild(par, ep);

    lex_check_pair_right(par, TK_LBK, TK_RBK, line);
    lex_skip(par);

    scan_stat(par);
    ai_cg_jump_direct(par, label1, lex_line(par));

    ai_cg_mark_label(par, label2, lex_line(par));

	if (lex_test_skip(par, TK_else)) {
        scan_stat(par);
	}

    ai_cg_block_end(par, lex_line(par));
}

static void scan_loop_stat(Parser* par) {
    Scope scope;
    ai_cg_block_start(par, &scope, lex_line(par), JMP_PROP_BREAK | JMP_PROP_CONTINUE);

    lex_skip(par);

    scan_label_for_scope(par);

	a_u32 label = ai_cg_mark_label(par, NO_LABEL, lex_line(par));

    scan_stat(par);
    ai_cg_jump_direct(par, label, lex_line(par));

    ai_cg_block_end(par, lex_line(par));
}

static void scan_break_stat(Parser* par) {
    a_line line = lex_line(par);

    lex_skip(par);

    GStr* label = scan_label(par);
    ai_cg_break(par, label, line);

    lex_test_skip(par, TK_SEMI);
}

static void scan_continue_stat(Parser* par) {
    a_line line = lex_line(par);

    lex_skip(par);

    GStr* label = scan_label(par);
    ai_cg_continue(par, label, line);

    lex_test_skip(par, TK_SEMI);
}

static void scan_return_stat(Parser* par) {
	a_line line = lex_line(par);

    lex_skip(par);

    Expr e[1] = {};

	if (!lex_test_sep(par)) {
        scan_exprs(par, e, false);
	}

    lex_test_skip(par, TK_SEMI);
	ai_cg_return(par, e, line);
}

static a_noret par_error_dup_mod(Parser* par, a_i32 tk) {
    parse_error(par, "duplicate '%s' modifier", lex_line(par), ai_lex_tagname(tk));
}

static void scan_var_pattern(Parser* par, Pat* pat) {
    loop {
        switch (lex_peek(par)) {
            case TK_mut: {
                if (pat->fmut) par_error_dup_mod(par, TK_mut);
                pat->fmut = true;
                lex_skip(par);
                break;
            }
            case TK_use: {
                if (pat->fuse) par_error_dup_mod(par, TK_use);
                pat->fuse = true;
                lex_skip(par);
                break;
            }
            default: {
                pat->tag = PAT_VAR;
                pat->name = lex_check_ident(par);
                pat->line = lex_line(par);
                if (pat->fmut && pat->fuse) {
                    parse_error(par, "mutable variable cannot capture resource", pat->line);
                }
                return;
            }
        }
    }
}

static void scan_value_pat(Parser* par, PatInfo* info, Pat* up, Pat** slot, a_u32 tag) {
    Pat pat_desc = { };

    Pat* pat = &pat_desc;

    pat->up = up;
    up->num_sub += 1;
	*slot = pat;

	switch (tag) {
		case PAT_VARG:
		case PAT_TUPLE:
			goto branch_standard;
		default: unreachable();
	}

branch_standard:
	switch (lex_peek(par)) {
        case TK_IDENT:
        /* Variable modifiers */
        case TK_mut:
        case TK_use: {
            scan_var_pattern(par, pat);
			break;
		}
		case TK__: {
            lex_skip(par);
            up->fcpx = true;
            pat->tag = PAT_WILD;
            pat->line = lex_line(par);
			break;
		}
		case TK_LBK: {
            lex_skip(par);
            up->fcpx = true;
            pat->tag = PAT_TUPLE;
            pat->line = lex_line(par);
            pat->num_sub = 0;
			if (!lex_test_skip(par, TK_RBK)) {
				return scan_value_pat(par, info, pat, &pat->sub, PAT_TUPLE);
			}
			break;
		}
		case TK_LSQ: {
            lex_skip(par);
            up->fcpx = true;
            pat->tag = PAT_LIST;
            pat->line = lex_line(par);
            pat->num_sub = 0;
			if (!lex_test_skip(par, TK_RSQ)) {
				return scan_value_pat(par, info, pat, &pat->sub, PAT_LIST);
			}
			break;
		}
		case TK_LBR: {
            lex_skip(par);
            up->fcpx = true;
            pat->tag = PAT_TABLE;
            pat->line = lex_line(par);
            pat->num_sub = 0;
			if (!lex_test_skip(par, TK_RBR)) {
				return scan_value_pat(par, info, pat, &pat->sub, PAT_TABLE);
			}
			break;
		}
		default: goto error;
	}

	slot = &pat->sib;
	loop {
		switch (lex_peek(par)) {
			case TK_COMMA: {
                lex_skip(par);
				tag = up->tag; /* Recover goto label. */
				return scan_value_pat(par, info, up, slot, tag);
			}
			case TK_RBK: {
				if (up->tag != PAT_TUPLE) {
					if (up->tag == PAT_VARG)
						goto load_nils;
					goto error_unexpected;
				}
                lex_skip(par);
				break;
			}
			case TK_RSQ: {
				if (up->tag != PAT_LIST) {
					if (up->tag == PAT_VARG)
						goto load_nils;
					goto error_unexpected;
				}
                lex_skip(par);
				break;
			}
			case TK_RBR: {
				if (up->tag != PAT_TABLE) {
					if (up->tag == PAT_VARG)
						goto load_nils;
					goto error_unexpected;
				}
                lex_skip(par);
				break;
			}
			case TK_ASSIGN: {
				if (up->tag != PAT_VARG) {
					goto error;
				}
                return (*info->con)(par, &info->root, info->ctx);
			}
			default:
			load_nils: {
				return (*info->con)(par, &info->root, info->ctx);
			}
		}

		assume(up != null);
		slot = &up->sib;
        pat = up;
        up = up->up;
	}

error:
	lex_error_got(par, "malformed pattern");

error_unexpected:
	switch (up->tag) {
		case PAT_VARG:
			lex_error_got(par, "malformed pattern");
		case PAT_TUPLE:
            lex_error_bracket(par, '(', ')', up->line);
		case PAT_LIST:
            lex_error_bracket(par, '[', ']', up->line);
		case PAT_TABLE:
            lex_error_bracket(par, '{', '}', up->line);
		default:
			unreachable();
	}
}

static void scan_pat(Parser* par, void (*con)(Parser*, Pat*, a_usize), a_usize ctx) {
	PatInfo info = {
		.root = { .tag = PAT_VARG },
		.con = con,
		.ctx = ctx
	};
    scan_value_pat(par, &info, &info.root, &info.root.sub, PAT_VARG);
}

typedef struct {
    a_u32 label;
    a_line line;
} ForStat;

static void for_bind(Parser* par, Pat* pat, a_usize ctx) {
    ForStat* stat = int2ptr(ForStat, ctx);
    stat->label = ai_cg_bind_for(par, pat, stat->line);
}

static void scan_for_stat(Parser* par) {
	Scope scope = {};
	ForStat stat;

    Expr e[1];
	lex_skip(par);

	a_u32 line = lex_line(par);
    ai_cg_block_start(par, &scope, line, JMP_PROP_BREAK | JMP_PROP_CONTINUE);
	stat.line = line;
	
	lex_check_skip(par, TK_LBK);
    scan_expr(par, e);
	lex_check_pair_right(par, TK_LBK, TK_RBK, line);
    ai_cg_iter(par, e, line);

	a_u32 label = ai_cg_mark_label(par, NO_LABEL, line);

	a_u32 line2 = lex_line(par);
	lex_check_skip(par, TK_BAR);
    scan_pat(par, for_bind, ptr2int(&stat));
	lex_check_pair_right(par, TK_BAR, TK_BAR, line2);

    scan_stat(par);
    ai_cg_jump_direct(par, label, line);

    ai_cg_mark_label(par, stat.label, line);

    if (lex_test_skip(par, TK_else)) {
        scan_stat(par);
    }

    ai_cg_block_end(par, lex_line(par));

    ai_cg_wild(par, e);
}

static void let_bind(Parser* par, Pat* p, a_usize c) {
    a_line line = cast(a_line, c);

    if (lex_test_skip(par, TK_ASSIGN)) {
        Expr e[1];
        Pat* pat = p->sub;

        loop {
            ai_cg_bind_prepare(par, pat);
            scan_expr(par, e);
            ai_cg_bind_once(par, pat, e);

            pat = pat->sib;
            p->sub = pat;
            p->num_sub -= 1;

            if (!lex_test_skip(par, TK_COMMA))
                break;

            if (pat == null) {
                do {
                    scan_expr(par, e);
                    ai_cg_wild(par, e);
                }
                while (lex_test_skip(par, TK_COMMA));

                ai_cg_stack_compact(par);
                return;
            }
        }
    }
    else {
        ai_cg_bind_nils(par, p, line);
    }
    ai_cg_stack_compact(par);
}

static void scan_let_stat(Parser* par) {
	a_line line = lex_line(par);
    lex_skip(par);

	switch (lex_peek(par)) {
		case TK_fn: {
			lex_skip(par);
			goto scan_func;
		}
        case TK_use: {
            Expr e1[1];
            Expr e2[1];
            lex_skip(par);
            GStr* name = lex_check_ident(par);
            ai_cg_local(par, e1, name, (ExprMod) { }, line);

            lex_check_skip(par, TK_ASSIGN);

            scan_expr(par, e2);
            ai_cg_pin(par, e1, e2);
            ai_cg_tbc(par, e1, line);
            break;
        }
		case TK_IDENT: {
			if (lex_test2(par, TK_LBK)) {
                Expr e1[1];
                Expr e2[1];
                GStr* name;
			scan_func:
                name = lex_check_ident(par);
                ai_cg_local(par, e1, name, (ExprMod) { }, line);

                scan_function(par, e2, name, line);

                ai_cg_pin(par, e1, e2);
				break;
			}
			fallthrough;
		}
		default: {
            scan_pat(par, let_bind, line);
			break;
		}
	}
}

static void scan_fn_stat(Parser* par) {
	a_line line = lex_line(par);
    lex_skip(par);

    Expr en[1];
    Expr ef[1];

    /* TODO: Should check symbol conflict here? */
	GStr* name = lex_check_ident(par);

    if (lex_test_skip(par, TK_DOT)) {
        do {
            name = lex_check_ident(par);
            ai_cg_index_str(par, en, name, lex_line(par));
        }
        while (lex_test_skip(par, TK_DOT));
    }
    else {
        ai_cg_local(par, en, name, (ExprMod) { }, lex_line(par));
    }

    scan_function(par, ef, name, line);

    ai_cg_pin(par, en, ef);
}

static void scan_pub_stat(Parser* par) {
	a_line line = lex_line(par);
    lex_skip(par);

	if ((lex_test(par, TK_IDENT) && lex_test2(par, TK_LBK)) || lex_test_skip(par, TK_fn)) {
        Expr e1[1];
        Expr e2[1];
        GStr* name = lex_check_ident(par);
        ai_cg_export(par, e1, name, line);
        scan_function(par, e2, name, line);
        ai_cg_assign(par, e1, e2, line);
	}
	else {
        Expr e[1];
		GStr* name = lex_check_ident(par);
        ai_cg_export(par, e, name, line);
		if (lex_test_skip(par, TK_ASSIGN)) {
            Expr e2[1] = {};
            scan_expr(par, e2);
            ai_cg_assign(par, e, e2, line);
		}
	}
}

static void scan_stat_pack(Parser* par) {
	Scope scope;

	a_line line = lex_line(par);
    lex_skip(par);
    ai_cg_block_start(par, &scope, line, 0);

    scan_stats(par);

    lex_check_pair_right(par, TK_LBR, TK_RBR, line);

    ai_cg_block_end(par, lex_line(par));
}

static void scan_stat(Parser* par) {
	assume(par->scope->top_pin == par->scope->top_reg, "compute stack leaked.");
	switch (lex_peek(par)) {
		case TK_if: {
            scan_if_stat(par);
			break;
		}
        case TK_break: {
            scan_break_stat(par);
            break;
        }
        case TK_continue: {
            scan_continue_stat(par);
            break;
        }
		case TK_return: {
            scan_return_stat(par);
			break;
		}
		case TK_while: {
            scan_while_stat(par);
			break;
		}
		case TK_for: {
            scan_for_stat(par);
			break;
		}
		case TK_loop: {
            scan_loop_stat(par);
			break;
		}
		case TK_fn: {
            scan_fn_stat(par);
			break;
		}
		case TK_LBR: {
            scan_stat_pack(par);
			break;
		}
		case TK_pub: {
            scan_pub_stat(par);
			break;
		}
		default: {
            scan_assign_or_call(par);
			break;
		}
	}
}

static void scan_stats(Parser* par) {
	loop {
		switch (lex_peek(par)) {
			case TK_RBR:
			case TK_EOF:
				return;
			case TK_SEMI: {
                lex_skip(par);
				break;
			}
			case TK_let: {
                scan_let_stat(par);
				break;
			}
			case TK_return: {
                scan_return_stat(par);
				break;
			}
            case TK_break: {
                scan_break_stat(par);
                break;
            }
            case TK_continue: {
                scan_continue_stat(par);
                break;
            }
            case TK_do: {
                scan_do_stat(par);
                break;
            }
			case TK_if: {
                scan_if_stat(par);
				break;
			}
			case TK_while: {
                scan_while_stat(par);
				break;
			}
			case TK_for: {
                scan_for_stat(par);
				break;
			}
			case TK_loop: {
                scan_loop_stat(par);
				break;
			}
			case TK_fn: {
                scan_fn_stat(par);
				break;
			}
			case TK_pub: {
                scan_pub_stat(par);
				break;
			}
			default: {
                scan_assign_or_call(par);
				break;
			}
		}
	}
}

static void scan_eval_expr(Parser* par) {
    Expr e[1] = {};

	if (!lex_test_sep(par)) {
        scan_exprs(par, e, false);
		/* Unpack the return value if only return one value, and it can be unpacked. */
		if (e->mupk) {
            ai_cg_unpack(par, e, lex_line(par));
		}
	}

    lex_test_skip(par, TK_SEMI);
	ai_cg_return(par, e, 1);
}

static void scan_chunk(unused a_henv env, void* ctx) {
	Parser* par = ctx;
    ai_cg_chunk_start(par);

	if (unlikely(par->options & ALO_COMP_OPT_EVAL)) {
        scan_eval_expr(par);
	}
	else {
        scan_stats(par);
	}

	if (!lex_test(par, TK_EOF)) {
		lex_error_got(par, "statement expected");
	}

    ai_cg_chunk_end(par, lex_line(par));
}

a_msg ai_parse(a_henv env, a_ifun fun, void* ctx, char const* file, GStr* name, a_u32 options, GFun** pout) {
	Parser par;
    ai_cg_parse_init(env, fun, ctx, file, name, options, &par);
    par.pout = pout;

	return ai_env_catch(env, scan_chunk, par);
}

static GStr* api_get_str(a_henv env, a_ilen id) {
    Value const* v = api_roslot(env, id);
    return v != null ? v_as_str(*v) : null;
}

a_msg alo_compile(a_henv env, a_ifun fun, void* ctx,
                  a_ilen id_env, a_ilen id_name, char const* file,
                  a_flags options) {
    GFun* out;
    api_check_slot(env, 1);
    id_env = alo_absindex(env, id_env);

    GStr* name = api_get_str(env, id_name);

    a_msg msg = ai_parse(env, fun, ctx, file, name, options, &out);
    if (likely(msg == ALO_SOK)) {
        v_set_func(env, api_incr_stack(env), out);
        if (out->ncap > 0) {
            RcCap* cap = ai_cap_new(env);
            out->ref_caps[0] = cap;
            v_cpy(env, cap->ptr, api_rdslot(env, id_env));
        }
    }
    else {
        ai_env_pop_error(env, api_incr_stack(env));
    }
    return msg;
}