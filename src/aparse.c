/**
 *@file aparse.c
 */

#define aparse_c_
#define ALO_LIB

#include "abc.h"
#include "afun.h"
#include "aenv.h"
#include "afmt.h"
#include "aerr.h"

#include "aparse.h"

typedef struct Sym Sym;
typedef struct FnScope FnScope;
typedef struct Scope Scope;
typedef struct RichCapInfo RichCapInfo;

BUF_STRUCT_DECLARE(SymBuf, Sym);
BUF_STRUCT_DECLARE(ConstBuf, Value);
BUF_STRUCT_DECLARE(InsnBuf, a_insn);
BUF_STRUCT_DECLARE(LineInfoBuf, LineInfo);
BUF_STRUCT_DECLARE(LocalInfoBuf, LocalInfo);
BUF_STRUCT_DECLARE(CapInfoBuf, RichCapInfo, CapInfoBuf* _last);

struct Parser {
    union {
        Lexer _lex;
        a_henv _env;
    };
    union {
        InsnBuf _insns;
        struct {
            a_insn (*_code)[1];
            a_usize _head_label;
        };
    };
    a_u32 _options;
    a_u8 _scope_depth;
    SymBuf _syms;
    ConstBuf _consts; /* Constants. */
    LocalInfoBuf _locals;
    LineInfoBuf _lines;
    Buf _secs;
    GStr* _file; /* Source file name. */
    GStr* _name;
	GStr* _gname;
    Scope* _scope;
    FnScope* _fnscope;
    RefQueue _rq; /* Function prototype queue. */
    QBuf* _qbq; /* Queued string buffer queue, used for concatenate expression. */
};

#define lex(par) (&(par)->_lex)

#define NO_LABEL (~u32c(0))
#define NIL_SEC_REF (~u32c(0))

#define SCOPE_DEPTH_ENV u8c(0)
#define SCOPE_DEPTH_ROOT u8c(1)

char const* ai_par_file(Parser* par) {
    return par->_file != null ? str2ntstr(par->_file) : "<in>";
}

a_none ai_par_report(Parser* par, char const* fmt, ...) {
    va_list varg;
    va_start(varg, fmt);
    ai_err_raisevf(par->_env, ALO_ECHUNK, fmt, varg);
    va_end(varg);
}

/*=========================================================*/

typedef struct ExprDesc ExprDesc;
typedef struct ConExpr ConExpr;
typedef struct PatInfo PatInfo;

typedef ExprDesc* restrict InExpr;
typedef ExprDesc* restrict OutExpr;
typedef ExprDesc* restrict InoutExpr;

static void l_direct_jump(Parser* par, a_u32 label, a_line line);
static a_u32 l_lazy_jump(Parser* par, a_u32 label, a_line line);
static a_u32 l_mark_label(Parser* par, a_u32 label, a_line line);

static void expr_index_str(Parser* par, InoutExpr e, GStr* name, a_line line);
static void expr_discard(Parser* par, InExpr e);

/**
 ** Volatile expressions are expressions presumed to be destroyed across
 ** any unrelated operations. Nonvolatile expressions are required to
 ** retain the values across any operations.
 */
enum ExprTag {
	EXPR_UNIT,
/*==============================Constants===============================*/
	/**
	 ** Nil constant expression.
	 ** REPR: nil
	 */
	EXPR_NIL,
	/**
	 ** Boolean constant expression.
	 ** REPR: false/true
	 */
	EXPR_FALSE, EXPR_TRUE,
	/**
	 ** Integer constant expression.
	 ** REPR: _i
	 *@param _i the integer constant.
	 */
	EXPR_INT,
	/**
	 ** Float constant expression.
	 ** REPR: _n
	 *@param _n the float constant.
	 */
	EXPR_FLOAT,
	/**
	 ** String constant expression.
	 ** REPR: _s
	 *@param _s the string constant.
	 */
	EXPR_STR,
/*==========================Bind Expressions============================*/
	/**
	 ** The expression from a local variable.
	 ** REPR: R[_d1]
	 *@param _d1 the register index.
	 *@param _d2 the symbol index.
	 *@param _fval true if value needs drop.
	 *@param _fsym true if it is a named register.
	 */
	EXPR_REG,
	/**
	 ** The expression bind to a capture value.
	 ** REPR: C[_d1]
	 *@param _d1 the capture index.
	 *@param _d2 the symbol index.
	 *@param _fsym true if symbol exists.
	 */
	EXPR_CAP,
	/**
	 ** The reference of export symbol.
	 ** repr: _ENV[_s]
	 *@param _d1 the const index of variable name.
	 *@param _d2 the symbol of variable.
	 *@param _fsym if symbol exists.
	 */
	EXPR_GBL,
	/**
	 ** The expressions bind to unpacked arguments.
	 *@param _d1 the base register index.
	 *@param _fucf true if path is unreachable.
	 */
	EXPR_REGS, EXPR_VREGS,
/*===========================Lazy Expressions===========================*/
	/**
	 ** The value indexed expression. REPR: R[_d1][R[_d2]]
	 *@param _d1 the base register index.
	 *@param _d2 the key register index.
	 *@param _f1 true if base needs drop.
	 *@param _f2 true if key needs drop.
	 */
	EXPR_REF,
	/**
	 ** The integer indexed expression.
	 ** REPR: R[_d1][_d2]
	 *@param _d1 the base register index.
	 *@param _d2 the integer key.
	 */
	EXPR_REFI,
	/**
	 ** The constant indexed expression. REPR: R[_impl][K[_key]]
	 *@param _d1 the base register index.
	 *@param _d2 the key constant index.
	 */
	EXPR_REFK,
	EXPR_REFCK,
/*=========================Partial Expressions==========================*/
	/**
	 ** The partial evaluated expression.
	 ** The output is the register with index A of instruction.
	 ** REPR: R[_d1(a)]
	 *@param _d1 the label of instruction.
	 *@param _fupk true if expression can be unpacked.
	 */
	EXPR_DYN,
	/**
	 ** The partial evaluated expression.
	 ** The output is the register with index A of instruction.
	 ** REPR: R[_d1(a):_d1(a)+_d1(c)]
	 *@param _d1 the label of instruction.
	 *@param _fupk is always true.
	 */
	EXPR_VDYN,
	/**
	 ** The partial evaluated expression.
	 ** The output is the register with index A of instruction.
	 ** REPR: R[_d1(a)]
	 *@param _d1 the label of instruction.
	 *@param _fval true if value needs drop.
	 *@param _fupk is always true.
	 */
	EXPR_CALL,
	/**
	 ** The partial evaluated expression.
	 ** The output is the register with index A of instruction.
	 ** REPR: R[_d1(a)]
	 *@param _d1 the label of instruction.
	 *@param _fupk is always true.
	 */
	EXPR_VCALL,
	/**
	 ** The try expression. This is a volatile expression.
	 ** REPR: try { R[_try] } else { nil }
	 *@param _d1 the temporary register index.
	 *@param _d2 the label of jump instruction.
	 *@param _fval true if value needs drop.
	 *@param _fsym true if value is shared.
	 *@param _fucf true is path is unreachable.
	 */
	EXPR_REG_OR_NIL,
	/**
	 ** The try expression. This is a volatile expression.
	 ** REPR: try { R[_label(a)] } else { nil }
	 *@param _d1 the label of compute result instruction.
	 *@param _d2 the label of jump instruction.
	 */
	EXPR_DYN_OR_NIL,
	/**
	 ** The try expression with boolean type. This is a volatile expression.
	 ** REPR: try { true/false } else { false/true }
	 *@param _d2 the label of residual path.
	 */
	EXPR_FALSE_OR_TRUE, EXPR_TRUE_OR_FALSE,
	/**
	 ** The try expression with only residual part.
	 ** REPR: try { ! } else { false/true }
	 *@param _d2 the label of residual path.
	 */
	EXPR_RESIDUAL_TRUE, EXPR_RESIDUAL_FALSE,
	/**
	 ** The expressions bind to number of temporary registers.
	 *@param _d1 the base register index.
	 *@param _fval true if values need drop.
	 *@param _fucf true if path is unreachable.
	 */
	EXPR_NTMP, EXPR_VNTMP,
	/**
	 ** The expressions bind to number of temporary registers.
	 *@param _d1 the base register index.
	 *@param _fval true if values need drop.
	 *@param _fucf true if path is unreachable.
	 */
	EXPR_NTMPC, EXPR_VNTMPC,

	EXPR__MAX
};

enum PatKind {
	/**
	 ** Discard pattern:
	 ** The accepted value will be discarded.
	 */
	PAT_DROP,
	/**
	 ** Variable pattern:
	 ** The accepted value will be bind to a new variable.
	 */
	PAT_VAR,
	/**
	 ** Pin pattern:
	 ** The accepted value will be pin at a allocated register.
	 */
	PAT_PIN,
	/**
	 ** The variable length pattern:
	 ** Will accept multiple values. Those values will bind to child patterns.
	 ** This pattern cannot be a non-root pattern.
	 */
	PAT_VARG,
	PAT_TUPLE,
	PAT_LIST,
	PAT_TABLE,
};

static_assert(EXPR_DYN + 1 == EXPR_VDYN);
static_assert(EXPR_CALL + 1 == EXPR_VCALL);
static_assert(EXPR_NTMP + 1 == EXPR_VNTMP);
static_assert(EXPR_NTMPC + 1 == EXPR_VNTMPC);

struct ExprDesc {
	union {
		a_int _i;
		a_float _n;
		GStr* _s;
		struct {
			a_u32 _d1;
			a_u32 _d2;
		};
	};
	a_line _line;
	a_u8 _tag;
	union {
		a_u8 _flags;
		struct {
			a_u8 _fval: 1; /* Used for value drop mark. */
			a_u8 _fkey: 1; /* Used for key drop mark. */
			a_u8 _fsym: 1; /* Used for symbol mark or shared mark. */
			a_u8 _fupk: 1; /* Used for unpack-able mark. */
			a_u8 _fucf: 1; /* Used for unreachable control flow mark. */
		};
	};
};

typedef ExprDesc Expr[1];

#define expr_init(e,k,v...) quiet(*(e) = new(ExprDesc) { ._tag = (k), v })

static a_bool expr_has_multi_values(InExpr e) {
	a_u32 k = e->_tag;
	assume(k < EXPR__MAX, "invalid expression.");
	return ((1 << k) & (1 << EXPR_VCALL | 1 << EXPR_VDYN | 1 << EXPR_NTMP | 1 << EXPR_NTMPC | 1 << EXPR_VNTMP | 1 << EXPR_VNTMPC));
}

static a_bool expr_has_vararg_top(InExpr e) {
	a_u32 k = e->_tag;
	assume(k < EXPR__MAX, "invalid expression.");
	return ((1 << k) & (1 << EXPR_VCALL | 1 << EXPR_VDYN | 1 << EXPR_VNTMP | 1 << EXPR_VNTMPC));
}

static void expr_copy(OutExpr dst, InExpr src) {
	memcpy(dst, src, sizeof(Expr));
}

static void expr_unit(OutExpr e) {
	expr_init(e, EXPR_UNIT);
}

static void expr_tmp(OutExpr e, a_u32 reg, a_line line) {
	expr_init(e, EXPR_REG, ._d1 = reg, ._fval = true, ._fsym = false, ._line = line);
}

static void expr_val(OutExpr e, a_u32 reg, a_line line) {
	expr_init(e, EXPR_REG, ._d1 = reg, ._fval = false, ._fsym = false, ._line = line);
}

static void expr_dyn(OutExpr e, a_u32 label, a_line line) {
	expr_init(e, EXPR_DYN, ._d1 = label, ._line = line);
}

struct ConExpr {
	Expr _expr;
	QBuf _buf;
};

typedef struct Pat Pat;

struct Pat {
    Pat* _parent;
	union {
        Pat* _child;
        GStr* _name;
    };
	Pat* _sibling;
    a_usize _sec_ref;
    Expr _expr;

    a_line _line;
    a_u8 _nchild;
    a_u8 _kind;
    union {
        a_u8 _flags;
        struct {
            a_u8 _fcpx: 1;
            a_u8 _fdfl: 1;
        };
    };

    /* Used for register allocation. */
	a_u8 _tmp_pos;
	a_u8 _tmp_top;
	a_u8 _abs_bot;
	a_u8 _index; /* Index in enclosed pattern. */
};

struct PatInfo {
	Pat _root;
	void (*_con)(Parser*, Pat*, a_usize);
	a_usize _ctx;
};

enum SymKind {
	/**
	 ** Local variable.
	 *@param _index the register index.
	 */
	SYM_LOCAL,
	/**
	 ** The top capture value.
	 *@param _index the capture index in top scope.
	 */
	SYM_CAPTURE,
	/**
	 ** The exported variable.
	 */
	SYM_EXPORT,
	/**
	 ** The variable length arguments pass to function.
	 */
	SYM_VARARG,
};

typedef union {
    a_u16 _;
    struct {
        a_u16 _readonly: 1;
    };
} SymMods;

/**
 ** Storage compile-time metadata of named symbol in chunk.
 */
struct Sym {
	a_u8 _tag; /* The tag of symbol kind. */
	a_u8 _scope; /* The scope of symbol belongs to. */
    SymMods _mods; /* The modifiers of symbol. */
	a_u32 _index; /* Variant uses for different symbol tag. */
	GStr* _name; /* The symbol name. */
};

#define SCOPE_STRUCT_HEAD \
    Scope* _up;              \
	a_u8 _bot_reg; /* The bottom of scope. */ \
	a_u8 _top_ntr; /* Top of non-temporary section. */ \
	a_u8 _bot_fur; /* Bottom of fragmented section. */ \
	a_u8 _num_fur; /* Number of temporary register in fragmented section. */ \
	a_u8 _top_reg; /* The top of scope. */  \
	a_line _begin_line;      \
	a_u32 _begin_label;      \
	a_u32 _end_label;        \
	a_u32 _sym_off

struct Scope {
	SCOPE_STRUCT_HEAD;
};

struct RichCapInfo {
	a_u8 _scope; /* The depth of first captured scope. */
	a_u8 _src_index;
	GStr* _name;
};

struct FnScope {
	union {
		Scope _scope;
		struct {
			SCOPE_STRUCT_HEAD;
		};
	};
	FnScope* _fn_up;
	Scope* _top_scope;
	GProto** _base_subs;
	CapInfoBuf _caps;
	a_u32 _const_off;
	a_u32 _line_off;
	a_u32 _local_off;
	a_u32 _head_jump;
	a_u32 _head_land;
	a_line _head_jump_line;
	a_line _head_line;
	a_line _close_line;
    a_u16 _nsub;
	union {
		a_u8 _flags;
		struct {
			a_u8 _fpass: 1;
			a_u8 _fland: 1;
			a_u8 _fjump: 1;
			a_u8 _fclose: 1;
		};
	};
	a_u8 _nparam;
	a_u8 _max_reg;
};

typedef struct {
    a_u32 _ninsn;
    a_u32 _nline;
    a_u32 _rel_label;
    a_u8 _rel_reg_bot;
    a_u8 _rel_reg_top;
    a_insn _code[][1];
} SecHead;

struct SecRecDesc {
    a_u32 _line_off;
    a_u32 _head_label;
    a_u32 _head_jump;
    a_u32 _head_land;
    a_line _head_line;
    a_line _head_jump_line;
    a_line _close_line;
    a_u8 _reg_base;
    a_u8 _max_reg;
    a_u8 _flags;
};

typedef struct SecRecDesc SecRec[1];

/*=========================================================*/

enum {
	/* Mark register operand which is not determined. */
	R_DYN = 255,
	/* Mark for not determined count, default is 1. */
	N_DYN = 1,
	/* Mark unused operand. */
	DMB = 0
};

/* Variable length of arguments. */
#define VARARG cast(a_u32, -1)

/**
 ** Load the index for constant.
 *@return the index of constant in constant pool.
 */
static a_u32 const_index(Parser* par, Value val) {
	ConstBuf* consts = &par->_consts;
	a_u32 off = par->_fnscope->_const_off;
	for (a_u32 i = par->_fnscope->_const_off; i < consts->_len; ++i) {
		/*
		 * Since all literals which have the same format provided by compiler should
		 * have same binary data, use identity equality for comparison.
		 */
		if (v_trivial_equals_unchecked(consts->_ptr[i], val)) {
			return i - off;
		}
	}

	if (unlikely(par->_fnscope->_const_off + consts->_len == BC_MAX_BX + 1)) {
		ai_par_report(par, "too many constants.", 0);
	}

	return at_buf_put(par->_env, *consts, val, "constant") - off;
}

static Value const_at(Parser* par, a_u32 index) {
	return par->_consts._ptr[par->_fnscope->_const_off + index];
}

static a_bool const_is_str(Parser* par, a_u32 index) {
	return v_is_str(const_at(par, index));
}

#define INSN_NOP ((a_insn) 0)

static a_insn* code_at(Parser* par, a_u32 label) {
    return par->_code[label];
}

static a_u32 code_put(Parser* par, a_insn i) {
    return at_buf_put(par->_env, par->_insns, i, "code");
}

static void l_emit_line(Parser* par, a_line line) {
	FnScope* scope = par->_fnscope;
	if (scope->_head_line != line) {
		LineInfo info = new(LineInfo) {UINT32_MAX, line};
		a_u32 index = at_buf_put(par->_env, par->_lines, info, "line");
		if (index > scope->_line_off) { /* Settle end label for last line info. */
			par->_lines._ptr[index - 1]._end = par->_head_label;
		}
		scope->_head_line = line;
	}
}

static a_u32 l_emit_direct(Parser* restrict par, a_insn i, a_u32 line) {
	l_emit_line(par, line);
    return code_put(par, i);
}

static a_bool l_should_eval(Parser* par) {
	return likely(par->_fnscope->_fpass || par->_fnscope->_fland);
}

static a_i32 l_jump_diff(Parser* par, a_u32 from, a_u32 to, a_line line) {
    a_i32 diff = cast(a_i32, to - from - 1);
    if (unlikely(diff < BC_MIN_SAX || diff > BC_MAX_SAX)) {
        ai_par_error(par, "instruction jump out of bound.", line);
    }
    return diff;
}

static a_u32 l_emit_jump_direct(Parser* par, a_u32 label, a_line line) {
	a_i32 d = label != NO_LABEL ? l_jump_diff(par, par->_head_label, label, line) : -1;
	return l_emit_direct(par, bc_make_isax(BC_J, d), line);
}

static a_u32 l_next_jump(Parser* par, a_u32 label) {
	assume(label <= par->_head_label, "not valid label.");
	if (label == par->_head_label)
		return par->_fnscope->_head_land;

	a_insn* ip = code_at(par, label);
	assume(bc_load_op(ip) == BC_J);

	a_i32 disp = bc_load_sax(ip);
	return disp != -1 ? label + 1 + cast(a_u32, disp) : NO_LABEL;
}

static void l_redirect(Parser* par, a_u32 from, a_u32 to, a_line line) {
	a_insn* ip = par->_code[from];

	assume(bc_load_op(ip) == BC_J);

    bc_store_sax(ip, l_jump_diff(par, from, to, line));
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

static void l_flush_jump(Parser *par) {
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

static void l_redirect_leave(Parser* par, a_u32 label, a_insn i) {
	loop {
		a_u32 next = l_next_jump(par, label);
		bc_store(par->_code[label], i);
		if (next == NO_LABEL)
			break;
		label = next;
	}
}

/**
 ** Emit an instruction to leave current function.
 */
static void l_emit_leave(Parser* par, a_insn i, a_line line) {
	if (l_should_eval(par)) {
		FnScope* scope = par->_fnscope;

		l_flush_close(par);
        l_flush_jump(par);
		if (scope->_fland) {
			l_redirect_leave(par, scope->_head_land, i);
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
        l_flush_jump(par);
		l_flush_land(par, line);
		par->_fnscope->_fpass = true;
		return l_emit_direct(par, i, line);
	}
	return NO_LABEL;
}

#define l_emit_ia(par,i,a,l) l_emit(par, bc_make_ia(i, a), l)
#define l_emit_iax(par,i,a,l) l_emit(par, bc_make_iax(i, a), l)
#define l_emit_iab(par,i,a,b,l) l_emit(par, bc_make_iab(i, a, b), l)
#define l_emit_iac(par,i,a,c,l) l_emit(par, bc_make_iac(i, a, c), l)
#define l_emit_iabx(par,i,a,b,l) l_emit(par, bc_make_iabx(i, a, b), l)
#define l_emit_iasbx(par,i,a,b,l) l_emit(par, bc_make_iasbx(i, a, b), l)
#define l_emit_iabc(par,i,a,b,c,l) l_emit(par, bc_make_iabc(i, a, b, c), l)
#define l_emit_iabsc(par,i,a,b,c,l) l_emit(par, bc_make_iabsc(i, a, b, c), l)

static void l_emit_idb(Parser* par, a_u32 i, Expr e, a_u32 b, a_u32 line) {
	expr_dyn(e, l_emit_iab(par, i, R_DYN, b, line), line);
}

static void l_emit_idbx(Parser* par, a_u32 i, Expr e, a_u32 b, a_u32 line) {
	expr_dyn(e, l_emit_iabx(par, i, R_DYN, b, line), line);
}

static void l_emit_idbc(Parser* par, a_u32 i, Expr e, a_u32 b, a_u32 c, a_u32 line) {
	expr_dyn(e, l_emit_iabc(par, i, R_DYN, b, c, line), line);
}

static void l_emit_idbd(Parser* par, a_u32 i, Expr e, a_u32 b, a_u32 line) {
	expr_dyn(e, l_emit_iabc(par, i, R_DYN, b, N_DYN, line), line);
	e->_fupk = true; /* Mark unpackable. */
}

static void l_emit_idbsc(Parser* par, a_u32 i, Expr e, a_u32 b, a_i32 c, a_u32 line) {
	expr_dyn(e, l_emit_iabsc(par, i, R_DYN, b, c, line), line);
}

static a_u32 l_emit_k(Parser* par, a_u32 dst, Value val, a_line line) {
	a_u32 index = const_index(par, val);
	return l_emit(par, bc_make_iabx(BC_K, dst, index), line);
}

static a_u32 l_emit_kn(Parser* par, a_u32 dst, a_u32 len, a_line line) {
	assume(len > 0);
	if (par->_fnscope->_fpass && !par->_fnscope->_fland) {
		a_insn* ip = par->_code[par->_head_label - 1];
		if (bc_load_op(ip) == BC_KN) {
			a_u32 a = bc_load_a(ip);
			a_u32 c = bc_load_c(ip);
			a_u32 dst1 = dst;
			a_u32 dst2 = dst + len;
			a_u32 src1 = a;
			a_u32 src2 = a + c;
			if (!(dst2 < src1 || src2 < dst1)) {
				a = min(src1, dst1);
				c = max(src2, dst2) - a;
                bc_store_a(ip, a);
                bc_store_c(ip, c);
				return par->_head_label - 1;
			}
		}
	}
	return l_emit(par, bc_make_iac(BC_KN, dst, len), line);
}

static a_u32 l_emit_branch(Parser* par, a_insn i, a_u32 label, a_u32 line) {
	if (l_emit(par, i, line) != NO_LABEL) {
		return l_emit_jump_direct(par, label, line);
	}
	return NO_LABEL;
}

static a_u32 l_emit_aby(Parser* par, a_enum op, a_u32 a, a_u32 b, a_u32 c, a_u32 line) {
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

static void expr_const(OutExpr e, a_u32 val, a_line line) {
	assume(val == EXPR_NIL || val == EXPR_FALSE || val == EXPR_TRUE || val == EXPR_UNIT);
	expr_init(e, val, ._line = line);
}

static void expr_int(OutExpr e, a_int val, a_line line) {
	expr_init(e, EXPR_INT,
		._i = val,
		._line = line
	);
}

static void expr_float(OutExpr e, a_float val, a_line line) {
	expr_init(e, EXPR_FLOAT,
		._n = val,
		._line = line
	);
}

static void expr_str(OutExpr e, GStr* val, a_line line) {
	expr_init(e, EXPR_STR,
		._s = val,
		._line = line
	);
}

static void expr_func(Parser* par, OutExpr e, GProto* fun) {
	FnScope* scope = par->_fnscope;
	a_u16 index = scope->_nsub ++;
	l_emit_idbx(par, BC_LDF, e, index, fun->_dbg_lndef);
}

static a_u32 stack_alloc_succ(Parser* par, a_u32 num, a_u32 line) {
	Scope* scope = par->_scope;
	a_u32 reg = scope->_top_reg;
	scope->_top_reg += num;
	if (scope->_top_reg > par->_fnscope->_max_reg) {
		par->_fnscope->_max_reg = scope->_top_reg;
		if (reg > BC_MAX_A) {
			ai_par_error(par, "too many register used.", line);
		}
	}
	return reg;
}

static a_u32 stack_alloc(Parser* par, a_u32 line) {
	return stack_alloc_succ(par, 1, line);
}

static a_bool l_is_in_tmp(Parser* par, a_u32 reg) {
	return reg >= par->_scope->_top_ntr;
}

static void stack_check_free_used(Scope* scope) {
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
static void stack_realloc(Parser* par, a_u32 reg) {
	Scope* scope = par->_scope;
	assume(l_is_in_tmp(par, reg), "cannot reallocate a using register twice.");
	if (likely(scope->_top_reg == reg)) {
		scope->_top_reg += 1;
		assume(scope->_top_reg <= par->_fnscope->_max_reg);
	}
	else {
		if (reg == scope->_bot_fur) {
			scope->_bot_fur += 1;
		}
		scope->_num_fur -= 1;
		stack_check_free_used(scope);
	}
}

/**
 ** Free register from temporary value stack.
 ** The register can be freed with the different order with
 ** reversed order of allocation.
 *@param par the parser.
 *@param reg the temporary register.
 */
static void stack_free(Parser* par, a_u32 reg) {
	Scope* scope = par->_scope;
	assume(reg < scope->_top_reg && l_is_in_tmp(par, reg));
	/* The registers are likely freed with the reversed order of allocation. */
	if (likely(reg + 1 == scope->_top_reg)) {
		scope->_top_reg = reg;
	}
	else {
		/* Or, mark register position to top of stack into critical section. */
		if (scope->_num_fur == 0 || reg < scope->_bot_fur) {
			scope->_bot_fur = reg;
		}
		scope->_num_fur += 1;
	}
	stack_check_free_used(scope);
}

/**
 ** Free a set of registers from temporary value stack, the registers is allocated successively.
 ** The registers must be freed with the order of allocation.
 *@param par the parser.
 *@param reg the first temporary register to free.
 */
static void stack_free_succ(Parser* par, a_u32 reg) {
	Scope* scope = par->_scope;
	assume(l_is_in_tmp(par, reg));

	scope->_top_reg = reg;
	stack_check_free_used(scope);
}

/**
 ** Store temporary value in register to a variable in place, the register will
 ** be dropped until leave the scope.
 *@param par the parser.
 *@param reg the register to store.
 */
static void stack_store(Parser* par, a_u32 reg) {
	Scope* scope = par->_scope;
	assume(reg >= scope->_bot_fur && reg < scope->_top_reg, "cannot store register in place.");
	scope->_top_ntr = max(reg + 1, scope->_top_ntr);
}

/**
 ** Drop ownership (without value) for expression.
 *@param par the parser.
 *@param e the expression.
 */
static void expr_drop(Parser* par, InExpr e) {
	if (e->_fkey) {
		stack_free(par, e->_d2);
		e->_fkey = false;
	}
	if (e->_fval) {
		if (!expr_has_multi_values(e)) {
			stack_free(par, e->_d1);
		}
		else {
			stack_free_succ(par, e->_d1);
		}
		e->_fval = false;
	}
}

static void expr_to_dyn(Parser* par, InoutExpr e);
static void expr_to_top_tmp(Parser* par, InoutExpr e);
static void expr_to_tmp(Parser* par, InoutExpr e);
static void expr_to_reg(Parser* par, InoutExpr e);
static void expr_to_reg_or_const(Parser* par, InoutExpr e);
static void expr_pin_reg(Parser* par, InExpr e, a_u32 reg);
static void exprs_fix(Parser* par, InoutExpr e);
static void exprs_to_top_tmps(Parser* par, InoutExpr e);

static a_enum expr_test_true(Parser* par, InExpr e, a_u32* plabel, a_u32 line);
static a_enum expr_test_false(Parser* par, InExpr e, a_u32* plabel, a_u32 line);
static a_enum expr_test_nil(Parser* par, InExpr e, a_u32* plabel, a_u32 line);
static a_enum expr_test_not_nil(Parser* par, InExpr e, a_u32* plabel, a_u32 line);
static a_u32 expr_catch_nil_branch(Parser* par, InoutExpr e, a_u32 line);

static void expr_resolve(Parser* par, OutExpr e, a_u32 id);
static a_bool l_lookup_symbol(Parser* par, GStr* name, a_u32* pid);

static void l_capture_locally(Parser* par, FnScope* scope, Sym* sym, RichCapInfo* info) {
	quiet(scope);

	switch (sym->_tag) {
		case SYM_LOCAL: {
			info->_src_index = par->_locals._ptr[sym->_index]._reg; /* Get variable index. */
			break;
		}
		case SYM_CAPTURE: {
			info->_src_index = sym->_index;
			break;
		}
		default: unreachable();
	}
}

static a_u32 l_lookup_capture_internal(Parser* par, FnScope* scope, Sym* sym, a_u32 depth) {
	/* Find in captured values. */
	for (a_u32 i = 0; i < scope->_caps._len; ++i) {
		RichCapInfo* info = &scope->_caps._ptr[i];
		if (info->_name == sym->_name) {
			/* Already captured. */
			return i;
		}
	}

	/* Not found, create a new capture value. */
	RichCapInfo info = {
		._scope = sym->_scope,
		._name = sym->_name
	};
	if (sym->_scope >= depth - 1) {
		l_capture_locally(par, scope, sym, &info);
	}
	else { /* Capture recursively. */
		info._src_index = l_lookup_capture_internal(par, scope->_fn_up, sym,depth - 1);
	}
	return at_buf_put(par->_env, scope->_caps, info, "capture");
}

static a_u32 l_lookup_capture(Parser* par, Sym* sym) {
	return l_lookup_capture_internal(par, par->_fnscope, sym, par->_scope_depth);
}

static void expr_env(Parser* par, OutExpr e) {
	a_u32 id;
	a_bool res = l_lookup_symbol(par, par->_gname, &id);
	assume(res, "variable '_ENV' not defined.");
	expr_resolve(par, e, id);
}

static void expr_gvar(Parser* par, InoutExpr e) {
	a_u32 id = e->_d1;
	assume(e->_tag == EXPR_GBL, "not global variable.");
	expr_env(par, e);
	if (e->_tag == EXPR_CAP) {
		e->_tag = EXPR_REFCK;
		e->_d2 = id;
	}
	else {
		expr_to_reg(par, e);
		e->_tag = EXPR_REFK;
		e->_d2 = id;
	}
}

static void expr_resolve(Parser* par, OutExpr e, a_u32 id) {
	Sym* sym = &par->_syms._ptr[id];
	switch (sym->_tag) {
		case SYM_LOCAL: {
			if (par->_scope_depth == sym->_scope) {
				expr_init(e, EXPR_REG,
					._d1 = par->_locals._ptr[par->_fnscope->_local_off + sym->_index]._reg,
					._d2 = id,
					._fsym = true
				);
			}
			else {
				expr_init(e, EXPR_CAP,
					._d1 = l_lookup_capture(par, sym),
					._d2 = id,
					._fsym = true
				);
			}
			break;
		}
		case SYM_CAPTURE: {
			expr_init(e, EXPR_CAP,
				._d1 = l_lookup_capture(par, sym),
				._d2 = id,
				._fsym = true
			);
			break;
		}
		case SYM_EXPORT: {
			expr_init(e, EXPR_GBL,
				._d1 = const_index(par, v_of_obj(sym->_name)),
				._d2 = id,
				._fsym = true
			);
			break;
		}
		default: unreachable();
	}
}

static a_bool l_lookup_symbol(Parser* par, GStr* name, a_u32* pid) {
	SymBuf* syms = &par->_syms;
	for (a_u32 i = syms->_len; i > 0; --i) {
		Sym* sym = &syms->_ptr[i - 1];
		if (sym->_name == name) {
			*pid = i - 1;
			return true;
		}
	}
	return false;
}

/**
 ** Lookup symbol in global scope.
 *@param par the parser.
 *@param e the expression for output.
 *@param name the lookup name.
 *@param line the line number of name reference.
 */
static void expr_symbol(Parser* par, OutExpr e, GStr* name, a_line line) {
	a_u32 id;
	if (l_lookup_symbol(par, name, &id)) {
		expr_resolve(par, e, id);
	}
	else {
		expr_init(e, EXPR_GBL,
			._d1 = const_index(par, v_of_obj(name)),
			._fsym = false,
			._line = line
		);
	}
}

static void expr_index_str(Parser* par, InoutExpr e, GStr* name, a_line line) {
	a_u32 index = const_index(par, v_of_obj(name));
	if (e->_tag == EXPR_CAP) {
		e->_tag = EXPR_REFCK;
		e->_d2 = index;
	}
	else {
        expr_to_reg(par, e);
		e->_tag = EXPR_REFK;
		e->_d2 = index;
	}
	e->_line = line;
}

static void expr_lookup(Parser* par, InoutExpr e, GStr* name, a_line line) {
    expr_to_reg(par, e);
	expr_drop(par, e);

	a_u32 reg = stack_alloc_succ(par, 2, line);

    l_emit_aby(par, BC_LOOK, reg, e->_d1, const_index(par, v_of_obj(name)), line);

	expr_init(e, EXPR_NTMP,
		._d1 = reg,
		._fval = true,
		._line = line
	);
}

/**
 ** Make reference of indexed expression.
 *@param par the parser.
 *@param ev the view expression.
 *@param ek the key expression.
 *@param line the line of operation.
 */
static void expr_index(Parser* par, InoutExpr ev, InExpr ek, a_line line) {
	switch (ek->_tag) {
		case EXPR_INT: {
            expr_to_reg(par, ev);
			a_int val = ek->_i;
			if (val >= 0 && val <= BC_MAX_C) {
				ev->_tag = EXPR_REFI;
				ev->_d2 = cast(a_u32, val);
			}
			else {
				ev->_tag = EXPR_REFK;
				ev->_d2 = const_index(par, v_of_int(val));
			}
			ev->_line = line;
			break;
		}
		case EXPR_STR: {
			expr_index_str(par, ev, ek->_s, line);
			break;
		}
		default: {
			/* Handle by normal expression. */
            expr_to_reg(par, ev);
            expr_to_reg(par, ek);
			ev->_tag = EXPR_REF;
			ev->_d2 = ek->_d1;
			ev->_fkey = ek->_fval;
			ev->_line = line;
			break;
		}
	}
}

/**
 ** Invert the branch at [label-1] and merge residual branch to *plabel.
 */
static void branch_negate(Parser* par, a_u32* plabel, a_u32 label, a_line line) {
	if (label + 1 == par->_head_label) {
		/* Try to swap duality opcodes for */
		a_insn* ip = code_at(par, label - 1);

		if (insn_is_branch(ip)) {
			a_u32 op = bc_load_op(ip);
            bc_store_op(ip, op ^ 1);

			a_u32 label1 = l_next_jump(par, label);
			a_u32 label2 = par->_fnscope->_fland ? par->_fnscope->_head_land : NO_LABEL;

			l_mark_label(par, label1, line);

			l_merge_branch(par, plabel, label2, line);
			l_redirect(par, label, *plabel, line);
			*plabel = label;
			return;
		}
	}

	*plabel = l_lazy_jump(par, *plabel, line);
	l_mark_label(par, label, line);
}

static void branch_instantiate(Parser* par, InoutExpr e, a_u32 reg) {
	assume(e->_tag == EXPR_TRUE_OR_FALSE || e->_tag == EXPR_FALSE_OR_TRUE);
	assume(par->_head_label == e->_d2 + 1);
	assume(insn_is_branch(par->_code[e->_d2 - 1]));

	a_u32 label = l_next_jump(par, e->_d2);
	if (label != NO_LABEL) {
		l_redirect_chain(par, label, e->_d2, e->_line);
	}

	/* Swap last instruction. */
    bc_store(par->_code[par->_head_label - 1], bc_make_ia(e->_tag == EXPR_TRUE_OR_FALSE ? BC_BKF : BC_BKT, reg));
	l_emit_ia(par, e->_tag == EXPR_TRUE_OR_FALSE ? BC_KT : BC_KF, reg, e->_line);
}

static void expr_new_table(Parser* par, InoutExpr e, a_line line) {
	l_emit_idbx(par, BC_HNEW, e, 0, line);
}

static void expr_neg(Parser* par, InoutExpr e, a_line line) {
	switch (e->_tag) {
		case EXPR_INT: {
			e->_i = ai_op_neg_int(e->_i);
			e->_line = line;
			break;
		}
		case EXPR_FLOAT: {
			e->_n = ai_op_neg_float(e->_n);
			e->_line = line;
			break;
		}
		default: {
            expr_to_reg(par, e);
            expr_drop(par, e);
            l_emit_idb(par, BC_NEG, e, e->_d1, line);
			break;
		}
	}
}

static void expr_bit_inv(Parser* par, InoutExpr e, a_line line) {
	switch (e->_tag) {
		case EXPR_INT: {
			e->_i = ai_op_bnot_int(e->_i);
			e->_line = line;
			break;
		}
		default: {
            expr_to_reg(par, e);
            expr_drop(par, e);
            l_emit_idb(par, BC_NEG, e, e->_d1, line);
			break;
		}
	}
}

static void expr_not(Parser* par, InoutExpr e, a_line line) {
	switch (e->_tag) {
		case EXPR_NIL:
		case EXPR_FALSE: {
			e->_tag = EXPR_TRUE;
			e->_line = line;
			break;
		}
		case EXPR_TRUE:
		case EXPR_INT:
		case EXPR_FLOAT:
		case EXPR_STR: {
			e->_tag = EXPR_FALSE;
			e->_line = line;
			break;
		}
		case EXPR_RESIDUAL_FALSE: {
			e->_tag = EXPR_RESIDUAL_TRUE;
			e->_line = line;
			break;
		}
		case EXPR_RESIDUAL_TRUE: {
			e->_tag = EXPR_RESIDUAL_FALSE;
			e->_line = line;
			break;
		}
		case EXPR_TRUE_OR_FALSE: {
			e->_tag = EXPR_FALSE_OR_TRUE;
			e->_line = line;
			break;
		}
		case EXPR_FALSE_OR_TRUE: {
			e->_tag = EXPR_TRUE_OR_FALSE;
			e->_line = line;
			break;
		}
		default: {
            expr_to_reg(par, e);
            expr_drop(par, e);

            a_u32 label = l_emit_branch(par, bc_make_ia(BC_BNZ, e->_d1), NO_LABEL, line);

			expr_init(e, EXPR_TRUE_OR_FALSE,
				._d2 = label,
				._line = line
			);
			break;
		}
	}
}

static void expr_unbox(Parser* par, InoutExpr e, a_line line) {
    expr_to_reg(par, e);
    expr_drop(par, e);

    l_emit_idbd(par, BC_UNBOX, e, e->_d1, line);
}

static void expr_len(Parser* par, InoutExpr e, a_line line) {
    expr_to_reg(par, e);
    expr_drop(par, e);

    l_emit_idb(par, BC_LEN, e, e->_d1, line);
}

static void expr_iter(Parser* par, InoutExpr e, a_line line) {
	Scope* scope = par->_scope;

	expr_to_reg(par, e);
	expr_drop(par, e);

	a_u32 reg1 = e->_d1;

	assume(scope->_top_ntr == scope->_top_reg);

	a_u32 reg2 = stack_alloc_succ(par, 3, line);
	expr_init(e, EXPR_NTMP, ._d1 = reg2, ._fval = true);
	l_emit_iab(par, BC_ITER, reg2, reg1, line);

	scope->_top_ntr = scope->_top_reg;
}

static void expr_binary_left(Parser* par, InoutExpr e, a_enum op, a_line line) {
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
            expr_to_reg_or_const(par, e);
			break;
		}
		case OP_AND: {
			a_u32 label = NO_LABEL;
			a_u32 kind = expr_test_true(par, e, &label, line);
			expr_discard(par, e);
			e->_tag = kind;
			e->_d2 = label;
			break;
		}
		case OP_OR: {
			a_u32 label = NO_LABEL;
			a_u32 kind = expr_test_false(par, e, &label, line);
			expr_discard(par, e);
			e->_tag = kind;
			e->_d2 = label;
			break;
		}
		default: unreachable();
	}
}

static a_bool expr_are_ints(InExpr e1, a_int* i1, InExpr e2, a_int* i2) {
	if (e1->_tag == EXPR_INT && e2->_tag == EXPR_INT) {
		*i1 = e1->_i;
		*i2 = e2->_i;
		return true;
	}
	return false;
}

static a_bool expr_are_floats(InExpr e1, a_float* i1, InExpr e2, a_float* i2) {
	if (e1->_tag == EXPR_INT) {
		*i1 = cast(a_float, e1->_i);
	}
	else if (e1->_tag == EXPR_FLOAT) {
		*i1 = e1->_n;
	}
	else return false;
	if (e2->_tag == EXPR_INT) {
		*i2 = cast(a_float, e2->_i);
	}
	else if (e2->_tag == EXPR_FLOAT) {
		*i2 = e2->_n;
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
			e->_tag = EXPR_INT;
			e->_i = ai_op_bin_int(a, b, op);
			break;
		}
		case OP_DIV:
		case OP_MOD: {
			if (unlikely(b == 0)) {
				ai_par_error(par, "attempt to divide by 0.", line);
			}
			e->_tag = EXPR_INT;
			e->_i = ai_op_bin_int(a, b, op);
			break;
		}
		case OP_EQ:
		case OP_NE:
		case OP_LT:
		case OP_LE:
		case OP_GT:
		case OP_GE: {
			e->_tag = ai_op_cmp_int(a, b, op) ? EXPR_TRUE : EXPR_FALSE;
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
			e->_tag = EXPR_FLOAT;
			e->_n = ai_op_bin_float(a, b, op);
			break;
		}
		case OP_EQ:
		case OP_NE:
		case OP_LT:
		case OP_LE:
		case OP_GT:
		case OP_GE: {
			e->_tag = ai_op_cmp_float(a, b, op) ? EXPR_TRUE : EXPR_FALSE;
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
	if (e2->_tag == EXPR_INT && e2->_i >= BC_MIN_SBX && e2->_i <= BC_MAX_SBX) {
        expr_to_reg(par, e1);
        expr_drop(par, e1);
        l_emit_iasbx(par, bc1, e1->_d1, e2->_i, line);
		return false;
	}
	else if (e1->_tag == EXPR_INT && e1->_i >= BC_MIN_SBX && e1->_i <= BC_MAX_SBX) {
        expr_to_reg(par, e2);
        expr_drop(par, e2);
        l_emit_iasbx(par, bc2, e2->_d1, e1->_i, line);
		return false;
	}

	return true;
}

static void l_emit_bin(Parser* par, InoutExpr e1, InExpr e2, a_u32 bc, a_line line) {
    expr_to_reg(par, e2);
    expr_to_reg(par, e1);
    expr_drop(par, e1);
    expr_drop(par, e2);
    l_emit_idbc(par, bc, e1, e1->_d1, e2->_d1, line);
}

static void l_compare(Parser* par, InoutExpr e1, InExpr e2, a_u32 bc, a_line line) {
    expr_to_reg(par, e2);
    expr_to_reg(par, e1);
    expr_drop(par, e1);
    expr_drop(par, e2);
    l_emit_iab(par, bc, e1->_d1, e2->_d1, line);
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
static void expr_binary(Parser* par, InoutExpr e1, InExpr e2, a_enum op, a_line line) {
	switch (op) {
		case OP_ADD:
		case OP_SUB:
		case OP_MUL:
		case OP_DIV:
		case OP_MOD: {
			if (l_fold_const_int(par, e1, e2, op, line) || l_fold_const_float(par, e1, e2, op, line))
				return;
			if (e2->_tag == EXPR_INT && e2->_i >= BC_MIN_SC && e2->_tag <= BC_MAX_SC) {
                expr_to_reg(par, e1);
                expr_drop(par, e1);
                l_emit_idbsc(par, BC_ADDI + op - OP_ADD, e1, e1->_d1, e2->_i, line);
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
			if (e2->_tag == EXPR_INT && e2->_i >= BC_MIN_SC && e2->_tag <= BC_MAX_SC) {
                expr_to_reg(par, e1);
                expr_drop(par, e1);
                l_emit_idbsc(par, BC_SHLI + op - OP_SHL, e1, e1->_d1, e2->_i, line);
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
			expr_init(e1, EXPR_TRUE_OR_FALSE,
				._d2 = l_emit_jump_direct(par, NO_LABEL, line),
				._line = line
			);
			break;
		}
		case OP_AND: {
			switch (e1->_tag) {
				case EXPR_RESIDUAL_FALSE: {
					break;
				}
				case EXPR_TRUE: {
					e1->_d2 = NO_LABEL;
					fallthrough;
				}
				case EXPR_TRUE_OR_FALSE: {
					e1->_tag = expr_test_true(par, e2, &e1->_d2, line);
					expr_discard(par, e2);
					break;
				}
				default: unreachable();
			}
			break;
		}
		case OP_OR: {
			switch (e1->_tag) {
				case EXPR_RESIDUAL_TRUE: {
					break;
				}
				case EXPR_FALSE: {
					e1->_d2 = NO_LABEL;
					fallthrough;
				}
				case EXPR_FALSE_OR_TRUE: {
					e1->_tag = expr_test_false(par, e2, &e1->_d2, line);
					expr_discard(par, e2);
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

static void expr_phi(Parser* par, InoutExpr e1, InExpr e2, a_u32 label, a_line line) {
	if (e2->_fucf) {
		l_mark_label(par, label, line);
		return;
	}
	else if (e1->_fucf) {
		assume(label == NO_LABEL);
		expr_copy(e1, e2);
		return;
	}
	switch (e1->_tag) {
		case EXPR_DYN_OR_NIL: {
			a_u32 label2 = expr_catch_nil_branch(par, e2, line);
			l_merge_branch(par, &e1->_d2, label2, line);

            expr_to_tmp(par, e2);

			a_u32 reg = e2->_d1;
			l_merge_optR(par, e1->_d2, reg, line);

			l_mark_label(par, label, line);
			stack_realloc(par, reg);
			expr_tmp(e1, reg, line);
			break;
		}
		case EXPR_REG_OR_NIL: {
			a_u32 label2 = expr_catch_nil_branch(par, e2, line);
			l_merge_branch(par, &e1->_d2, label2, line);

			a_u32 reg = e1->_d1;
            expr_pin_reg(par, e2, reg);
			l_merge_optR(par, e1->_d2, reg, line);

			l_mark_label(par, label, line);
			stack_realloc(par, reg);
			expr_tmp(e1, reg, line);
			break;
		}
		case EXPR_REG: {
			a_u32 reg = e1->_d1;
			stack_realloc(par, reg);
            expr_pin_reg(par, e2, reg);
			l_mark_label(par, label, line);
			break;
		}
		case EXPR_DYN: {
			a_u32 reg = stack_alloc(par, line);
            expr_pin_reg(par, e1, reg);
            expr_pin_reg(par, e2, reg);
			l_mark_label(par, label, line);
			expr_tmp(e1, reg, line);
			break;
		}
		default: unreachable();
	}
}

static void expr_or_nil(Parser* par, InoutExpr e, a_u32* plabel, a_line line) {
	expr_test_not_nil(par, e, plabel, line);
}

static void expr_or_else(Parser* par, InoutExpr e, a_u32* plabel, a_line line) {
	a_u32 label = *plabel;
	if (!e->_fucf) {
		switch (e->_tag) {
			case EXPR_REG_OR_NIL: {
				if (e->_fsym) {
					e->_tag = EXPR_REG;
					a_u32 label2 = e->_d2;
					expr_to_dyn(par, e);
					e->_tag = EXPR_DYN_OR_NIL;
					e->_d2 = label2;
				}
				else if (e->_fval) {
					stack_free(par, e->_d1);
				}
				break;
			}
			case EXPR_REG: {
				if (e->_fsym) {
					expr_to_dyn(par, e);
				}
				else if (e->_fval) {
					/* Free stack for another branch used. */
					/* The ownership will still keep. */
					stack_free(par, e->_d1);
				}
				break;
			}
			default: {
				expr_to_dyn(par, e);
				break;
			}
		}
	}
	*plabel = l_lazy_jump(par, label, line);
	l_mark_label(par, label, line);
}

static void expr_phi_nil(Parser* par, InoutExpr e, a_u32 label, a_line line) {
	if (label == NO_LABEL)
		return;
	if (e->_fucf) {
		l_lazy_jump(par, label, line);
		l_mark_label(par, label, line);
		expr_const(e, EXPR_NIL, e->_line);
		return;
	}
	switch (e->_tag) {
		case EXPR_REG: {
			e->_tag = EXPR_REG_OR_NIL;
			e->_d2 = label;
			break;
		}
		default: {
            expr_to_dyn(par, e);
			fallthrough;
		}
		case EXPR_DYN: {
			e->_tag = EXPR_DYN_OR_NIL;
			e->_d2 = label;
			break;
		}
		case EXPR_REG_OR_NIL:
		case EXPR_DYN_OR_NIL: {
			l_merge_branch(par, &e->_d2, label, line);
			break;
		}
	}
	e->_line = line;
}

static void expr_or_ret(Parser* par, InoutExpr e, a_line line) {
	switch (e->_tag) {
		case EXPR_NIL: {
			l_emit_leave(par, bc_make_i(BC_RET0), line);
			e->_fucf = true;
			break;
		}
		case EXPR_FALSE:
		case EXPR_TRUE:
		case EXPR_INT:
		case EXPR_FLOAT:
		case EXPR_STR: {
			break;
		}
		case EXPR_REG_OR_NIL: {
			l_redirect_leave(par, e->_d2, bc_make_i(BC_RET0));
			l_emit(par, bc_make_ia(BC_BNN, e->_d1), line);
			l_emit_fast(par, bc_make_i(BC_RET0), line);

			e->_tag = EXPR_REG;
			break;
		}
		case EXPR_DYN_OR_NIL: {
			l_redirect_leave(par, e->_d2, bc_make_i(BC_RET0));

			e->_tag = EXPR_DYN;
			fallthrough;
		}
		case EXPR_DYN: {
			if (e->_fupk) {
				a_insn* ip = code_at(par, e->_d1);

				a_u32 reg = par->_scope->_top_reg;
				bc_store_op(ip, bc_load_op(ip) + 1);
				bc_store_a(ip, reg);
				bc_store_c(ip, DMB);

				l_emit(par, bc_make_ia(BC_BNN, reg), line);
				l_emit_fast(par, bc_make_ia(BC_RETM, reg), line);

				expr_init(e, EXPR_REG,
					._d1 = reg,
					._line = line
				);
			}
			else {
				expr_to_reg(par, e);

				l_emit(par, bc_make_ia(BC_BNN, e->_d1), line);
				l_emit_fast(par, bc_make_i(BC_RET0), line);
			}
			break;
		}
		case EXPR_CALL: {
			a_insn* ip = code_at(par, e->_d1);

			a_u32 reg = bc_load_a(ip);
			bc_store_op(ip, bc_load_op(ip) + 1);
			bc_store_c(ip, DMB);

			l_emit(par, bc_make_ia(BC_BNN, reg), line);
			l_emit_fast(par, bc_make_ia(BC_RETM, reg), line);

			expr_init(e, EXPR_REGS,
				._d1 = reg,
				._fupk = true,
				._line = line
			);
			break;
		}
		case EXPR_REGS: {
			a_u32 reg = e->_d1;

			l_emit(par, bc_make_ia(BC_BNN, reg), line);
			l_emit_fast(par, bc_make_ia(BC_RETM, reg), line);
			break;
		}
		default: {
			expr_to_reg(par, e);

			l_emit(par, bc_make_ia(BC_BNN, e->_d1), line);
			l_emit_fast(par, bc_make_i(BC_RET0), line);
			break;
		}
	}
}

static void exprs_push_left(Parser *par, InoutExpr es) {
    exprs_fix(par, es);
}

static void exprs_push(Parser *par, InoutExpr es, InExpr e) {
	if (es->_tag == EXPR_UNIT) {
		expr_copy(es, e);
		return;
	}
	else if (es->_tag != EXPR_NTMP && es->_tag != EXPR_NTMPC) {
        exprs_fix(par, es);
		assume(es->_tag == EXPR_NTMP || es->_tag == EXPR_NTMPC, "bad expression for push.");
	}

	es->_fucf |= e->_fucf;

	switch (e->_tag) {
		case EXPR_UNIT: {
			break;
		}
		case EXPR_VCALL: {
			a_insn* ip = par->_code[e->_d1];
			assume(bc_load_a(ip) == par->_scope->_top_reg, "can not place variable.");

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_c(ip, DMB);
			break;
		}
		case EXPR_VDYN: {
			a_insn* ip = par->_code[e->_d1];

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_a(ip, es->_d1);
            bc_store_c(ip, DMB);
			break;
		}
		default: {
            expr_to_top_tmp(par, e);
			break;
		}
	}
}

static void exprs_pop(Parser* par, InoutExpr es, InExpr e, a_line line) {
	if (es->_tag == EXPR_NTMP) {
		expr_tmp(e, par->_scope->_top_reg - 1, line);
	}
	else {
		assume(es->_tag != EXPR_NTMPC);
		expr_copy(e, es);
		expr_unit(es);
	}
}

static void expr_box_tuple(Parser* par, InoutExpr e, a_line line) {
    exprs_to_top_tmps(par, e);
    expr_drop(par, e);

	if (expr_has_vararg_top(e)) {
		l_emit_idb(par, BC_TNEWM, e, e->_d1, line);
	}
	else {
		l_emit_idbc(par, BC_TNEW, e, e->_d1, par->_scope->_top_reg - e->_d1, line);
	}
}

static void expr_box_list(Parser* par, InoutExpr e, a_line line) {
    exprs_fix(par, e);
	switch (e->_tag) {
		case EXPR_UNIT: {
			l_emit_idbx(par, BC_LNEW, e, 0, line);
			break;
		}
		case EXPR_NTMP: {
			a_u32 n = par->_scope->_top_reg - e->_d1;

            expr_drop(par, e);

            l_emit_idbc(par, BC_LBOX, e, e->_d1, n, line);
			break;
		}
		case EXPR_NTMPC: {
			a_u32 n = par->_scope->_top_reg - e->_d1 - 1;
			if (n > 0) {
				l_emit_iabc(par, BC_LPUSH, e->_d1, e->_d1 + 1, n, line);
				stack_free_succ(par, e->_d1 + 1);
			}
			expr_tmp(e, e->_d1, line);
			break;
		}
		default: unreachable();
	}
}

static void expr_call(Parser* par, InoutExpr es, a_line line) {
    exprs_to_top_tmps(par, es);

	a_u32 label;
	if (expr_has_vararg_top(es)) {
		label = l_emit_iabc(par, BC_CALLM, es->_d1, DMB, N_DYN, line);
	}
	else {
		label = l_emit_iabc(par, BC_CALL, es->_d1, par->_scope->_top_reg - es->_d1, N_DYN, line);
	}

    expr_drop(par, es);
    expr_init(es, EXPR_CALL,
		._d1 = label,
		._fupk = true,
		._line = line
	);
}

static void expr_return(Parser* par, InoutExpr e, a_line line) {
	l_clear_close(par);
	if (e->_tag == EXPR_REG) {
		l_emit_leave(par, bc_make_iab(BC_RET, e->_d1, 1), line);
	}
	else if (e->_tag == EXPR_VCALL) {
		a_insn* ip = code_at(par, e->_d1);
		a_enum op = bc_load_op(ip);

		assume(e->_d1 == par->_head_label - 1 && !par->_fnscope->_fjump, "not head label.");
		assume(op == BC_CALL || op == BC_CALLM, "not call operation.");

		bc_store_op(ip, op + (BC_TCALL - BC_CALL));
		bc_store_c(ip, 0);

		par->_fnscope->_fpass = false;
	}
	else {
        exprs_to_top_tmps(par, e);

		a_u32 len = par->_scope->_top_reg - e->_d1;

        expr_drop(par, e);

		if (expr_has_vararg_top(e)) {
			l_emit_leave(par, bc_make_i(BC_RETM), line);
		}
		else if (len == 0) {
			l_emit_leave(par, bc_make_i(BC_RET0), line);
		}
		else {
			l_emit_leave(par, bc_make_iab(BC_RET, e->_d1, len), line);
		}
	}
}

static void exprs_take(Parser* par, InoutExpr e, a_u32 n, a_line line) {
	assume(e->_tag == EXPR_VCALL || e->_tag == EXPR_VDYN, "not vararg expressions.");
	a_u32 reg = stack_alloc_succ(par, n, line);
	a_insn* ip = code_at(par, e->_d1);
	if (e->_tag != EXPR_VCALL) {
		bc_store_a(ip, reg);
	}
	else {
		assume(reg == bc_load_a(ip));
	}
	bc_store_c(ip, n);
	e->_fval = true;
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
static a_u32 exprs_trunc(Parser* par, InoutExpr ei, InoutExpr el, a_u32 n, a_line line) {
	assume(n > 0, "truncate nothing.");
	switch (ei->_tag) {
		case EXPR_UNIT: {
			if (expr_has_vararg_top(el)) {
				exprs_take(par, el, n, line);
				expr_tmp(el, par->_scope->_top_reg - 1, line);
				return 0;
			}
			return n - 1;
		}
		case EXPR_NTMP: {
			expr_drop(par, el);
			a_u32 m = par->_scope->_top_reg - ei->_d1;
			if (m >= n) {
				a_u32 top = ei->_d1 + n;
				stack_free_succ(par, top);
				expr_discard(par, el);
				expr_tmp(el, top - 1, line);
				return 0;
			}
			else if (expr_has_vararg_top(el)) {
				a_u32 top = ei->_d1 + n;
				exprs_take(par, el, n - m, line);
				expr_tmp(el, top - 1, line);
				return 0;
			}
			else {
				return n - m - 1;
			}
		}
		case EXPR_NTMPC: {
			exprs_push(par, ei, el);
			expr_box_list(par, ei, line);
			expr_init(el, EXPR_REFI,
				._d1 = ei->_d1,
				._d2 = n - 1,
				._fval = false
			);
			return 0;
		}
		default: {
			if (n > 1) {
				if (expr_has_vararg_top(el)) {
					exprs_take(par, el, n - 1, line);
					expr_tmp(el, par->_scope->_top_reg - 1, line);
					return 0;
				}
				return n - 2;
			}

			expr_discard(par, el);

			if (n > 0) {
				expr_copy(el, ei);
				expr_unit(ei);
			}
			else {
				expr_discard(par, ei);
			}
			return 0;
		}
	}
}

static a_bool l_try_fold_append(Parser* par, QBuf* buf, InExpr e) {
	switch (e->_tag) {
		case EXPR_INT: {
			at_fmt_puti(par->_env, buf, e->_i);
			return true;
		}
		case EXPR_FLOAT: {
			at_fmt_putf(par->_env, buf, e->_n);
			return true;
		}
		case EXPR_STR: {
			GStr* str = e->_s;
			at_buf_putls(par->_env, buf, str->_ptr, str->_len);
			return true;
		}
		default: {
			return false;
		}
	}
}

static GStr* buf_to_str(Parser* par, QBuf* buf) {
	GStr* str = ai_lex_to_str(&par->_lex, buf->_ptr, buf->_len);
	at_buf_clear(*buf);
	return str;
}

static void expr_concat(Parser* par, ConExpr* ce, InExpr e, a_line line) {
	if (l_try_fold_append(par, &ce->_buf, e)) {
		if (par->_qbq != &ce->_buf) {
			/* Link queue. */
			ce->_buf._last = par->_qbq;
			par->_qbq = &ce->_buf;
		}
	}
	else if (ce->_buf._len > 0) {
        expr_to_dyn(par, e); /* Drop used register. */

        exprs_to_top_tmps(par, ce->_expr);

		a_u32 reg = stack_alloc_succ(par, 2, line);
        expr_pin_reg(par, e, reg + 1);

		GStr* str = buf_to_str(par, &ce->_buf);
		l_emit_k(par, reg, v_of_obj(str), line);

		at_buf_clear(ce->_buf);
	}
	else {
		exprs_push(par, ce->_expr, e);
	}
}

static void expr_concat_end(Parser* par, ConExpr* ce, OutExpr e, a_line line) {
	if (ce->_expr->_tag == EXPR_UNIT) {
		expr_str(e, buf_to_str(par, &ce->_buf), line);
	}
	else {
        exprs_to_top_tmps(par, ce->_expr);
		if (ce->_buf._len > 0) {
			GStr* str = buf_to_str(par, &ce->_buf);
			a_u32 reg = stack_alloc(par, line);
			l_emit_k(par, reg, v_of_obj(str), line);
		}
        a_u32 base = ce->_expr->_d1;
		l_emit_idbc(par, BC_CAT, e, base, par->_scope->_top_reg - base, line);
        expr_drop(par, ce->_expr);
    }

	/* Check and drop_object string buffer. */
	if (par->_qbq == &ce->_buf) {
		at_buf_deinit(G(par->_env), ce->_buf);
		par->_qbq = ce->_buf._last;
	}
}

static void expr_unpack(Parser* par, InoutExpr e, a_line line) {
	if (!e->_fupk) {
		ai_par_error(par, "the expression cannot be unpack.", line);
	}
	e->_tag += 1;
	e->_fupk = false;
}

static a_insn l_leave_or_nop(Parser* par, a_u32 label) {
	if (unlikely(label == par->_head_label))
		return 0;
	a_insn* ip = par->_code[label];
	return insn_is_leave(ip) ? *ip : 0;
}

/**
 ** Jump to determined label.
 *@param par the parser.
 *@param label the label jump to.
 *@param line the line number.
 */
static void l_direct_jump(Parser* par, a_u32 label, a_line line) {
	if (l_should_eval(par)) {
        l_flush_jump(par);

		a_insn i = l_leave_or_nop(par, label);
		if (likely(i == 0)) {
			if (par->_fnscope->_fland) {
				l_redirect_chain(par, par->_fnscope->_head_land, label, line);
				l_clear_land(par);
			}
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

static a_u32 l_lazy_jump(Parser* par, a_u32 label, a_line line) {
	if (l_should_eval(par)) {
		l_flush_close(par); /* Force flush close. */
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

static a_u32 l_mark_label(Parser* par, a_u32 label, a_line line) {
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

static a_u32 expr_test(Parser* par, InoutExpr e, a_line line) {
	a_u32 label = NO_LABEL;
    expr_test_true(par, e, &label, line);
	return label;
}

/**
 ** Discard the result of the expression,
 ** also drop the ownership of it.
 * @param par the parser.
 * @param e the expression to be discarded.
 */
static void expr_discard(Parser* par, InExpr e) {
	switch (e->_tag) {
		case EXPR_DYN: {
			a_insn* ip = code_at(par, e->_d1);
			a_u32 reg = stack_alloc(par, e->_line);
            bc_store_a(ip, reg);
			stack_free(par, reg);

			e->_tag = EXPR_REG;
			e->_d1 = reg;
			e->_fupk = false;
			break;
		}
		case EXPR_CALL: {
			a_insn* ip = code_at(par, e->_d1);
            bc_store_c(ip, 0);
			expr_unit(e);
			break;
		}
		case EXPR_REG:
		case EXPR_REFI:
		case EXPR_REFK: {
            expr_drop(par, e);
            break;
		}
		default: {
			break;
		}
	}
}

static void sym_check_writable(Parser* par, InExpr e, a_line line) {
	if (e->_fsym) {
		a_u32 id = e->_d2;
		Sym* sym = &par->_syms._ptr[id];
		if (sym->_mods._readonly) {
			ai_par_error(par, "cannot assign to readonly variable %s.", line, str2ntstr(sym->_name));
		}
	}
}

static void expr_assign(Parser* par, InExpr e1, InExpr e2, a_line line) {
assign:
	switch (e1->_tag) {
		case EXPR_REG: {
			sym_check_writable(par, e1, line);
            expr_pin_reg(par, e2, e1->_d1);
			break;
		}
		case EXPR_CAP: {
			sym_check_writable(par, e1, line);
            expr_to_reg(par, e2);
			l_emit_iab(par, BC_STC, e1->_d1, e2->_d2, line);

            expr_drop(par, e2);
            break;
		}
		case EXPR_REF: {
            expr_to_reg(par, e2);
			l_emit_iabc(par, BC_SET, e2->_d1, e1->_d1, e1->_d2, line);

            expr_drop(par, e2);
            expr_drop(par, e1);
            break;
		}
		case EXPR_REFI: {
            expr_to_reg(par, e2);
			l_emit_iabsc(par, BC_SETI, e2->_d1, e1->_d1, cast(a_i32, e1->_d2), line);

            expr_drop(par, e2);
            expr_drop(par, e1);
            break;
		}
		case EXPR_REFK: {
            expr_to_reg(par, e2);
            l_emit_aby(par, BC_SETS, e2->_d1, e1->_d1, e1->_d2, line);

            expr_drop(par, e2);
            expr_drop(par, e1);
            break;
		}
		case EXPR_REFCK: {
            expr_to_reg(par, e2);

			if (const_is_str(par, e1->_d2)) {
                l_emit_aby(par, BC_CSETS, e2->_d1, e1->_d1, e1->_d2, line);
			}
			else {
				a_u32 reg = stack_alloc_succ(par, 2, line);
				a_u32 label = l_emit_iab(par, BC_LDC, reg, e1->_d1, line);
				if (likely(label != NO_LABEL)) {
					l_emit_fast(par, bc_make_iabx(BC_K, reg + 1, e1->_d2), line);
                    l_emit_aby(par, BC_SETS, e2->_d1, reg, reg + 1, line);
				}
				stack_free_succ(par, reg);
			}

            expr_drop(par, e2);
            break;
		}
		case EXPR_GBL: {
			if (e1->_fsym) {
				sym_check_writable(par, e1, line);
			}
			else if (!(par->_options & ALO_COMP_OPT_LOSSEN)) {
				GStr* name = v_as_str(const_at(par, e1->_d2));
				ai_par_error(par, "cannot assign to anonymous variable '%s'", e1->_line, str2ntstr(name));
			}
			expr_gvar(par, e1);
			goto assign;
		}
		default: {
			panic("cannot assign to the expression.");
		}
	}
}

static void expr_tbc(Parser* par, InExpr e, a_line line) {
    assume(e->_tag == EXPR_REG, "cannot mark a non-register storage variable to-be-closed.");
    l_emit_ia(par, BC_TBC, e->_d1, line);
}

static a_u32 syms_push(Parser* par, Sym sym) {
	return at_buf_put(par->_env, par->_syms, sym, "symbol");
}

static void sec_start(Parser* par, SecRec rec) {
    rec->_line_off = par->_lines._len;
    rec->_head_label = par->_head_label;
    rec->_head_line = par->_fnscope->_head_line;
    rec->_head_jump = par->_fnscope->_head_jump;
    rec->_head_jump_line = par->_fnscope->_head_jump_line;
    rec->_head_land = par->_fnscope->_head_land;
    rec->_close_line = par->_fnscope->_close_line;
    rec->_reg_base = par->_scope->_top_reg;
    rec->_max_reg = par->_fnscope->_max_reg;
    rec->_flags = par->_fnscope->_flags;
    par->_fnscope->_max_reg = par->_scope->_top_reg;

    par->_fnscope->_head_line = 0;
    par->_fnscope->_flags = 0;
    par->_fnscope->_fpass = true;
}

static a_u32 sec_record(Parser* par, SecRec rec) {
    l_flush_jump(par);
    if (par->_fnscope->_fland) {
        l_lazy_jump(par, NO_LABEL, par->_fnscope->_head_line);
        l_flush_jump(par);
    }

    SecHead head = {
        ._ninsn = par->_insns._len - rec->_head_label,
        ._nline = par->_lines._len - rec->_head_line,
        ._rel_label = rec->_head_label,
        ._rel_reg_bot = rec->_reg_base,
        ._rel_reg_top = par->_fnscope->_max_reg
    };

    if (head._ninsn == 0) return NIL_SEC_REF;

    a_usize size = offsetof(SecHead, _code)
            + sizeof(a_insn) * head._ninsn
            + sizeof(LineInfo) * head._nline;
    catch(at_buf_ncheck(par->_env, par->_secs, size), ai_buf_error, par->_env, "section");

    a_usize base = par->_secs._len;

    void* addr = par->_secs._ptr + base;

    memcpy(addr, &head, offsetof(SecHead, _code));
    addr += offsetof(SecHead, _code);

    memcpy(addr, par->_insns._ptr + rec->_head_label, sizeof(a_insn) * head._ninsn);
    addr += sizeof(a_insn) * head._ninsn;

    memcpy(addr, par->_lines._ptr + rec->_line_off, sizeof(LineInfo) * head._nline);
    addr += sizeof(LineInfo) * head._nline;

    assume(addr == par->_secs._ptr + base + size);

    par->_secs._len += size;

    par->_lines._len = rec->_line_off;
    par->_head_label = rec->_head_label;
    par->_fnscope->_head_line = rec->_head_line;
    par->_fnscope->_head_jump = rec->_head_jump;
    par->_fnscope->_head_jump_line = rec->_head_jump_line;
    par->_fnscope->_head_land = rec->_head_land;
    par->_fnscope->_close_line = rec->_close_line;
    par->_fnscope->_top_reg = rec->_reg_base;
    par->_fnscope->_max_reg = rec->_max_reg;

    return base;
}

static void l_emit_section(Parser* par, InoutExpr e, SecHead const* restrict sec, a_u32 abs_base, a_u32 line_call) {
    a_u32 reg_disp = abs_base - sec->_rel_reg_bot;
    a_u32 label_disp = par->_head_label - sec->_rel_label;

    LineInfo const* lines = sec->_nline > 0 ? cast(LineInfo const*, sec->_code + sec->_ninsn) : null;
    a_u32 line_info_index = 0;
    a_u32 l_next_line = sec->_nline > 0 ? lines[0]._end - sec->_rel_label : UINT32_MAX;
    a_u32 line = line_call;

    l_flush_close(par);

    for (a_u32 l = 0; l < sec->_ninsn; ++l) {
        if (l == l_next_line) {
            LineInfo info = lines[++line_info_index];
            line = info._lineno;
            l_next_line = info._end;
        }
        a_insn const* ip = sec->_code[l];
#define reloc(x) ({ a_u32 _r = bc_load_##x(ip); if (_r >= sec->_rel_reg_bot) { bc_store_##x(&i, _r + reg_disp); } })
        a_enum op = bc_load_op(ip);
        assume(op < BC__MAX, "bad opcode.");
        a_insn i = *ip;
        switch (ai_bc_reloc[op]) {
            case 0: {
                reloc(a);
                reloc(b);
                break;
            }
            case 1: {
                reloc(b);
                break;
            }
            case 2: {
                reloc(a);
                break;
            }
            case 3: {
                break;
            }
            case 4: {
                reloc(a);
                reloc(b);
                reloc(c);
                break;
            }
            case 5: {
                assume(op == BC_J, "not jump instruction.");
                a_u32 label = l + bc_load_sax(ip);
                if (l == 0) {
                    l_direct_jump(par, label + label_disp, line);
                }
                else if (label >= sec->_ninsn) {
//                    /* Redirect jump. */
//                    l_emit_jump_direct(par, label + label_disp, line);
                    panic("cross boundary jump not supported.");
                }
                break;
            }
            default: unreachable();
        }
        l_emit_direct(par, i, line);
#undef reloc
    }

    switch (e->_tag) {
        case EXPR_REG: {
            if (e->_d1 >= sec->_rel_reg_bot) {
                e->_d1 += reg_disp;
            }
            break;
        }
        case EXPR_NTMP:
        case EXPR_NTMPC: {
            assume(e->_d1 >= sec->_rel_reg_bot, "registers across stack boundary.");
            e->_d1 += reg_disp;
            break;
        }
        case EXPR_REG_OR_NIL: {
            if (e->_d1 >= sec->_rel_reg_bot) {
                e->_d1 += reg_disp;
            }
            e->_d2 += label_disp;
            break;
        }
        case EXPR_DYN_OR_NIL: {
            e->_d1 += label_disp;
            e->_d2 += label_disp;
            break;
        }
        case EXPR_TRUE_OR_FALSE:
        case EXPR_FALSE_OR_TRUE:
        case EXPR_RESIDUAL_FALSE:
        case EXPR_RESIDUAL_TRUE: {
            e->_d2 += label_disp;
            break;
        }
        default: {
            break;
        }
    }
}

static a_u32 sym_local(Parser* par, GStr* name, a_u32 reg, a_u32 begin_label, SymMods mods) {
	LocalInfo info = {
		._begin_label = begin_label - par->_fnscope->_begin_label,
		._end_label = NO_LABEL, /* Not determinted yet. */
		._name = name,
		._reg = reg
	};

	a_u32 index = at_buf_put(par->_env, par->_locals, info, "local variable");

	stack_store(par, reg);

	return syms_push(par, new(Sym) {
		._tag = SYM_LOCAL,
		._scope = par->_scope_depth,
		._mods = mods,
		._index = index,
		._name = name
	});
}

static void pat_bind_nils(Parser* par, Pat* p, a_line line) {
	Scope* scope = par->_scope;

	assume(p->_kind == PAT_VARG);

	/* If all node is bind. */
	if (p->_child == null)
		return;

	if (p->_fcpx) {
		ai_par_error(par, "nil binding is only available for plain pattern.", line);
	}

	assume(scope->_top_ntr == scope->_top_reg);
	a_u32 num = p->_nchild - p->_child->_index;
	a_u32 reg = stack_alloc_succ(par, num, line);
	a_u32 label = l_emit_kn(par, reg, num, line);
	for (Pat* pat = p->_child; pat != null; pat = pat->_sibling) {
        sym_local(par, pat->_name, reg++, label, new(SymMods) {});
	}

	scope->_top_ntr = scope->_top_reg;
}

static void pat_bind_with(Parser* par, Pat* pat, InExpr e, a_u32 base) {
    a_u32 reg = base + pat->_abs_bot + pat->_tmp_pos;
    if (pat->_expr->_tag != EXPR_UNIT && pat->_expr->_tag != EXPR_NIL) {
		assume(pat->_kind != PAT_VARG);

		a_u32 label = NO_LABEL;
        expr_test_nil(par, e, &label, pat->_line);

        if (pat->_sec_ref != NIL_SEC_REF) {
            SecHead* sec = cast(SecHead*, par->_secs._ptr + pat->_sec_ref);
            l_emit_section(par, pat->_expr, sec, reg, pat->_line);
        }

        expr_pin_reg(par, pat->_expr, reg);
        l_mark_label(par, label, pat->_line);
    }
	switch (pat->_kind) {
		case PAT_VARG: {
			a_u32 line = pat->_expr->_line;
			a_u32 num = pat->_nchild;

			assume(e->_tag == EXPR_VCALL || e->_tag == EXPR_VDYN || e->_tag == EXPR_VNTMP, "not vararg expressions.");

			if (e->_tag == EXPR_VNTMP) {
				l_emit_iabc(par, BC_TRIM, reg, DMB, num, line);
			}
			else {
				a_insn* ip = code_at(par, e->_d1);
				if (e->_tag != EXPR_VCALL) {
					bc_store_a(ip, reg);
				}
				else {
					assume(reg == bc_load_a(ip)); //TODO
				}
				bc_store_c(ip, num);
			}
			
			for (Pat* child = pat->_child; child != null; child = child->_sibling) {
				Expr e2;
				expr_val(e2, reg++, line); /* Discard ownership of value, drop them later. */
                pat_bind_with(par, child, e2, base);
			}
			break;
		}
		case PAT_DROP: {
			break;
		}
		case PAT_PIN: {
			expr_pin_reg(par, e, pat->_tmp_pos);
			break;
		}
		case PAT_VAR: {
            expr_pin_reg(par, e, reg);
            sym_local(par, pat->_name, reg, par->_head_label, new(SymMods) {});
			break;
		}
		case PAT_TUPLE: {
			a_u32 line = pat->_expr->_line;
			a_u32 num = pat->_nchild;
			l_emit_iabc(par, BC_UNBOX, reg, e->_d1, num /* TODO: variable length arguments? */, line);
			for (Pat* child = pat->_child; child != null; child = child->_sibling) {
				Expr e2;
				expr_val(e2, reg++, line);
                pat_bind_with(par, child, e2, base);
			}
			break;
		}
		default: panic("not implemented."); //TODO
	}
}

/**
 ** Compute relative slot indices of deconstructed values to be placed.
 *@param pat_root the deconstruction pattern.
 *@return the number of stack used to store variable.
 */
static a_u32 pat_compute(Pat* const pat_root) {
	Pat* pat = pat_root;
	a_u32 abs_top = 0;
	loop {
        pat->_abs_bot = abs_top;
        pat->_tmp_pos = 0;
		switch (pat->_kind) {
			case PAT_VAR: {
                pat->_tmp_top = 1;
				abs_top += 1;
				break;
			}
			case PAT_PIN: {
				pat->_tmp_top = 1;
				break;
			}
			case PAT_DROP: {
                pat->_tmp_top = 1;
				break;
			}
			case PAT_VARG:
			case PAT_TUPLE: {
                pat->_tmp_top = 0;
				if (pat->_child != null) {
                    pat = pat->_child;
					continue;
				}
				break;
			}
			default: {
				unreachable();
			}
		}

		/* Try to leave structured patterns. */
		loop {
			if (pat == pat_root)
				return abs_top;

			if (pat->_sibling != null)
				break;
			
			Pat* pat_up = pat->_parent;

			a_u32 used = (pat->_abs_bot - pat_up->_abs_bot) + pat->_tmp_pos;
            pat_up->_tmp_pos = used > pat->_index ? max(pat_up->_tmp_pos, used - pat->_index) : pat_up->_tmp_pos;
            pat_up->_tmp_top = max(pat_up->_tmp_pos + pat_up->_nchild, (pat->_abs_bot - pat_up->_abs_bot) + pat->_tmp_top);

            pat = pat_up;
		}

		Pat* pat_up = pat->_parent;
        pat_up->_tmp_pos = max(pat_up->_tmp_pos, (pat->_abs_bot - pat_up->_abs_bot) + pat->_tmp_top - pat->_index - 1);
        pat = pat->_sibling;
	}
}

static void pat_bind(Parser* par, Pat* pat, InExpr e) {
	assume(pat != null, "bind to nothing.");

    a_u32 abs_top = pat_compute(pat);

    expr_drop(par, e); /* Drop ownership but keep expression. */

    a_u32 reg = stack_alloc_succ(par, pat->_tmp_top, e->_line);
    pat_bind_with(par, pat, e, reg);
	assume(par->_scope->_top_ntr - abs_top == cast(a_u32, par->_scope->_top_reg - pat->_tmp_top), "bind compute incorrect.");
	stack_free_succ(par, par->_scope->_top_ntr);
}

static a_u32 for_bind_real(Parser* par, Pat* pat, a_line line) {
	assume(pat != null, "bind to nothing.");

	a_u32 reg_itr = par->_scope->_top_reg - 3; /* The index of iterator register. */

	Expr e;
	a_u32 label = l_emit_branch(par, bc_make_iabc(BC_FORG, R_DYN, reg_itr, N_DYN), NO_LABEL, line);
	expr_init(e, EXPR_VDYN, ._d1 = label - 1, ._fupk = true);

	pat_bind(par, pat, e);
	return label;
}

static void param_bind(Parser* par, Pat* pat, a_usize ctx) {
	a_line line = cast(a_line, ctx);
	
	Expr e;
	expr_init(e, EXPR_VNTMP, ._d1 = 0, ._fupk = true, ._line = line);

	pat_bind(par, pat, e);

	par->_fnscope->_nparam = par->_scope->_top_ntr;
}

typedef struct {
	a_u32 _label;
	a_line _line;
} ForStat;

static void for_bind(Parser* par, Pat* pat, a_usize ctx) {
	ForStat* stat = ptr_of(ForStat, ctx);
	stat->_label = for_bind_real(par, pat, stat->_line);
}

static void local_bind(Parser* par, OutExpr e, GStr* name, a_line line) {
	Scope* scope = par->_scope;
	assume(scope->_top_ntr == scope->_top_reg, "stack not balanced.");
	a_u32 reg = stack_alloc(par, line);
	a_u32 sym = sym_local(par, name, reg, par->_head_label, new(SymMods) {});
	expr_init(e, EXPR_REG,
		._d1 = reg,
		._d2 = sym,
		._fval = false,
		._fsym = true,
		._line = line
	);
	scope->_top_ntr = scope->_top_reg;
}

static void sym_export(Parser* par, OutExpr e, GStr* name, a_line line) {
	Scope* scope = par->_scope;
	assume(scope->_top_ntr == scope->_top_reg, "stack not balanced.");
	a_u32 id = syms_push(par, new(Sym) {
		._tag = SYM_EXPORT,
		._name = name,
		._scope = par->_scope_depth,
	});
	expr_init(e, EXPR_GBL,
		._d1 = const_index(par, v_of_obj(name)),
		._d2 = id,
		._fsym = true,
		._line = line
	);
}

/**
 ** Compact fragment section.
 *@param par the parser.
 */
static void stack_compact(Parser* par) {
	Scope* scope = par->_scope;
	assume(scope->_top_reg == scope->_top_ntr, "some temporary register is not freed.");
	scope->_num_fur = 0;
}

static void scope_push(Parser* par, Scope* scope, a_u32 reg, a_line line) {
	*scope = new(Scope) {
		._up = par->_scope,
		._bot_reg = reg,
		._top_ntr = reg,
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
		a_u32 label = par->_head_label - par->_fnscope->_begin_label;
		a_u32 local_off = par->_fnscope->_local_off;
		for (a_u32 i = bot; i < top; ++i) {
			Sym* sym = &par->_syms._ptr[i];
			switch (sym->_tag) {
				case SYM_LOCAL: {
					par->_locals._ptr[sym->_index - local_off]._end_label = label;
					break;
				}
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

static void scope_enter(Parser* par, Scope* scope, a_line line) {
	scope_push(par, scope, par->_scope->_top_reg, line);
}

static void scope_leave(Parser* par, a_line line) {
	assume(par->_scope->_up != null);
	scope_pop(par, line);
}

static void scope_leave_with(Parser* par, a_line line, InoutExpr e) {
	assume(par->_scope->_up != null);
	if (e->_tag == EXPR_REG && e->_d1 == par->_scope->_up->_top_reg) {
		scope_pop(par, line);

		a_u32 reg = stack_alloc(par, line);
		assume(reg == e->_d1);
		e->_fval = true; /* Recover drop marker. */
	}
	else {
        expr_to_dyn(par, e);
		scope_pop(par, line);
	}
}

static void fnscope_prologue(Parser* par, FnScope* fnscope, a_line line) {
	if (par->_scope_depth == UINT8_MAX) {
		ai_par_error(par, "function nested level overflow.", line);
	}

    code_put(par, INSN_NOP); /* Add barrier for instruction look ahead. */

	if (par->_fnscope != null) {
		par->_fnscope->_top_scope = par->_scope;
	}

	par->_scope = null;

	*fnscope = new(FnScope) {
		._fn_up = par->_fnscope,
		._base_subs = cast(GProto**, par->_rq._tail),
		._const_off = par->_consts._len,
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

static GProto* fnscope_epilogue(Parser* par, GStr* name, a_bool root, a_line line) {
	FnScope* scope = par->_fnscope;

	l_clear_close(par);
	l_emit_leave(par, bc_make_i(BC_RET0), line);

	scope_pop(par, line);

	ProtoDesc desc = {
		._nconst = par->_consts._len - scope->_const_off,
		._ninsn = par->_head_label - scope->_begin_label,
		._nsub = scope->_nsub,
		._nlocal = par->_locals._len - scope->_local_off,
		._ncap = scope->_caps._len,
		._nstack = scope->_max_reg,
		._nparam = scope->_nparam,
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
				._fup = cap_info->_scope != par->_scope_depth - 1
			};
			if (desc._flags._fdebug) {
				proto->_dbg_cap_names[i] = cap_info->_name;
			}
		}
	}
	run { /* Build sub function */
		GProto** src = scope->_base_subs;
		GProto** dst = proto->_subs;
		GProto* val = *src;

		GProto** const end = cast(GProto**, par->_rq._tail);
		par->_rq._tail = cast(a_gclist*, src);

		while (src != end) {
			*dst = val;
			*src = null;

			src = cast(GProto**, &val->_gnext);
			val = *src;
			dst += 1;
		}
	}
	
	proto->_name = name;
	if (desc._flags._fdebug) {
		proto->_dbg_file = par->_file;
		proto->_dbg_lndef = scope->_begin_line;
		proto->_dbg_lnldef = line;
	}

	at_buf_deinit(G(par->_env), scope->_caps);

	FnScope* up_scope = scope->_fn_up;

	par->_fnscope = up_scope;
	par->_scope = up_scope != null ? up_scope->_top_scope : null;
	par->_scope_depth -= 1;
	par->_head_label = scope->_begin_label - 1; /* Drop barrier. */
	par->_locals._len = scope->_local_off;
	par->_lines._len = scope->_line_off;
	par->_rq._tail = cast(a_gclist*, scope->_base_subs);

	rq_push(&par->_rq, proto);

	return proto;
}

static void proto_drop_recursive(a_henv env, GProto* proto) {
	for (a_u32 i = 0; i < proto->_nsub; ++i) {
        proto_drop_recursive(env, proto->_subs[i]);
	}
	ai_proto_drop(G(env), proto);
}

static void parser_close(Parser* par) {
    at_buf_deinit(G(par->_env), par->_insns);
	at_buf_deinit(G(par->_env), par->_consts);
	at_buf_deinit(G(par->_env), par->_lines);
	at_buf_deinit(G(par->_env), par->_locals);
	at_buf_deinit(G(par->_env), par->_syms);
    at_buf_deinit(G(par->_env), par->_secs);
	ai_lex_close(&par->_lex);

	gbl_unprotect(par->_env);
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
	rq_for(obj, &par->_rq) {
        proto_drop_recursive(par->_env, g_cast(GProto, obj));
	}
	/* Close linked buffers. */
	for (QBuf* qb = par->_qbq; qb != null; qb = qb->_last) {
		at_buf_deinit(G(par->_env), *qb);
	}
	for (FnScope* scope = par->_fnscope; scope != null; scope = scope->_fn_up) {
		at_buf_deinit(G(par->_env), scope->_caps);
	}
	/* Close parser. */
	parser_close(par);
}

#define ENV_NAME "_ENV"

static void parser_start(Parser* par) {
	gbl_protect(par->_env, parser_mark, parser_except, par);

	par->_gname = ai_lex_to_str(lex(par), ENV_NAME, sizeof(ENV_NAME) - 1);

	/* Add predefined '_ENV' name. */
	syms_push(par, new(Sym) {
		._tag = SYM_CAPTURE,
		._scope = SCOPE_DEPTH_ENV,
		._mods = {
			._readonly = true /* Predefined environment is always readonly variable. */
		},
		._index = 0,
		._name = par->_gname
	});
}

static void proto_register_recursive(a_henv env, GProto* proto) {
	assume(proto->_gnext == null, "duplicate root function.");
	ai_gc_register_object(env, proto);
	for (a_u32 i = 0; i < proto->_nsub; ++i) {
        proto_register_recursive(env, proto->_subs[i]);
	}
}

static GFun* func_build(Parser* par) {
	GProto* proto = g_cast(GProto, par->_rq._head); /* Get root prototype. */
    proto_register_recursive(par->_env, proto);
	parser_close(par);
	GFun* fun = proto->_cache;
	g_set_white(G(par->_env), fun);
	return fun;
}

/**
 ** Move value to the register without optimize.
 ** The A index of return label is the output register.
 *@param par the parser.
 *@param e the expression to move.
 *@param reg the destined register or a placeholder.
 *@return the label of movement instruction.
 */
static a_u32 expr_move_to(Parser* par, InExpr e, a_u32 reg) {
bind:
	switch (e->_tag) {
		case EXPR_NIL: {
			return l_emit_iabc(par, BC_KN, reg, DMB, 1, e->_line);
		}
		case EXPR_FALSE: {
			return l_emit_ia(par, BC_KF, reg, e->_line);
		}
		case EXPR_TRUE: {
			return l_emit_ia(par, BC_KT, reg, e->_line);
		}
		case EXPR_INT: {
			if (e->_i >= BC_MIN_SBX && e->_i <= BC_MAX_SBX) {
				return l_emit_iasbx(par, BC_KI, reg, e->_i, e->_line);
			}
			else {
				return l_emit_iabx(par, BC_K, reg, const_index(par, v_of_int(e->_i)), e->_line);
			}
		}
		case EXPR_FLOAT: {
			return l_emit_iabx(par, BC_K, reg, const_index(par, v_of_float(e->_n)), e->_line);
		}
		case EXPR_STR: {
			return l_emit_iabx(par, BC_K, reg, const_index(par, v_of_obj(e->_s)), e->_line);
		}
		case EXPR_CAP: {
			return l_emit_iab(par, BC_LDC, reg, e->_d1, e->_line);
		}
		case EXPR_REG: {
            return l_emit_iab(par, BC_MOV, reg, e->_d1, e->_line);
		}
		case EXPR_REGS: {
			l_emit_iac(par, BC_TRIM, e->_d1, 1, e->_line);
			return l_emit_iab(par, BC_MOV, reg, e->_d1, e->_line);
		}
		case EXPR_REF: {
            return l_emit_iabc(par, BC_GET, reg, e->_d1, e->_d2, e->_line);
		}
		case EXPR_REFI: {
            return l_emit_iabsc(par, BC_GETI, reg, e->_d1, cast(a_i32, e->_d2), e->_line);
		}
		case EXPR_REFK: {
            a_u32 k = e->_d2;
			if (const_is_str(par, k)) {
				return l_emit_aby(par, BC_GETS, reg, e->_d1, k, e->_line);
			}
			else {
				a_u32 reg2 = stack_alloc(par, e->_line);
				a_u32 label = l_emit(par, bc_make_iabx(BC_K, reg2, k), e->_line);
				if (likely(label != NO_LABEL)) {
					l_emit_fast(par, bc_make_iabc(BC_GET, reg, e->_d1, reg2), e->_line);
					label += 1;
				}
				stack_free(par, reg2);
				return label;
			}
			break;
		}
		case EXPR_REFCK: {
			a_u32 k = e->_d2;
			if (const_is_str(par, k)) {
				return l_emit_aby(par, BC_CGETS, reg, e->_d1, k, e->_line);
			}
			else {
				a_u32 reg2 = stack_alloc_succ(par, 2, e->_line);
				a_u32 label = l_emit(par, bc_make_iabx(BC_K, reg2, k), e->_line);
				if (likely(label != NO_LABEL)) {
					l_emit_fast(par, bc_make_iab(BC_LDC, reg2 + 1, e->_d1), e->_line);
					l_emit_fast(par, bc_make_iabc(BC_GET, reg, reg2 + 1, reg2), e->_line);
					label += 2;
				}
				stack_free_succ(par, reg2);
				return label;
			}
		}
		case EXPR_DYN: {
            bc_store_a(par->_code[e->_d1], reg);
			return e->_d1;
		}
		case EXPR_CALL: {
			a_insn* ip = par->_code[e->_d1];
			return l_emit_iab(par, BC_MOV, reg, bc_load_a(ip), e->_line);
		}
		case EXPR_FALSE_OR_TRUE:
		case EXPR_TRUE_OR_FALSE: {
			a_u32 reg2 = stack_alloc(par, e->_line);
            branch_instantiate(par, e, reg2);
			return l_emit_iab(par, BC_MOV, reg, reg2, e->_line);
		}
		case EXPR_REG_OR_NIL: {
			a_u32 reg2;
			if (e->_fsym) {
				assume(!e->_fval, "a shared value has ownership.");
				reg2 = stack_alloc(par, e->_line);
				l_emit_iab(par, BC_MOV, reg2, e->_d1, e->_line);
				stack_free(par, reg2);
				l_merge_optR(par, e->_d2, reg2, e->_line);
			}
			else {
				reg2 = e->_d1;
				l_merge_optR(par, e->_d2, reg2, e->_line);
			}
			return l_emit_iab(par, BC_MOV, reg, reg2, e->_line);
		}
		case EXPR_DYN_OR_NIL: {
			a_u32 reg2 = stack_alloc(par, e->_line);
            bc_store_a(par->_code[e->_d1], reg2);
			stack_free(par, reg2);
			l_merge_optR(par, e->_d2, reg2, e->_line);
			return l_emit_iab(par, BC_MOV, reg, reg2, e->_line);
		}
		case EXPR_GBL: {
			expr_gvar(par, e);
			goto bind;
		}
		default: {
			panic("cannot move expression with kind: %u.", e->_tag);
		}
	}
}

static void expr_move_to_if_need(Parser* par, InExpr e, a_u32 reg) {
	switch (e->_tag) {
		case EXPR_NIL: {
			l_emit_kn(par, reg, 1, e->_line);
			break;
		}
		case EXPR_REG: {
			if (e->_d1 != reg) {
                l_emit_iab(par, BC_MOV, reg, e->_d1, e->_line);
			}
			break;
		}
		case EXPR_REGS: {
			l_emit_iac(par, BC_TRIM, e->_d1, 1, e->_line);
			if (e->_d1 != reg) {
                l_emit_iab(par, BC_MOV, reg, e->_d1, e->_line);
			}
			break;
		}
		case EXPR_REG_OR_NIL: {
			if (e->_d1 != reg) {
				l_emit_iab(par, BC_MOV, reg, e->_d1, e->_line);
			}
			l_merge_optR(par, e->_d2, reg, e->_line);
			break;
		}
		case EXPR_DYN_OR_NIL: {
            bc_store_a(par->_code[e->_d1], reg);
			l_merge_optR(par, e->_d2, reg, e->_line);
			break;
		}
		case EXPR_FALSE_OR_TRUE:
		case EXPR_TRUE_OR_FALSE: {
            branch_instantiate(par, e, reg);
			break;
		}
		case EXPR_CALL: {
			a_u32 reg2 = bc_load_a(par->_code[e->_d1]);
			if (reg != reg2) {
				l_emit_iab(par, BC_MOV, reg, reg2, e->_line);
			}
			break;
		}
		default: {
            expr_move_to(par, e, reg);
			break;
		}
	}
}

static void expr_to_dyn(Parser* par, InoutExpr e) {
    if (e->_tag != EXPR_DYN) {
        expr_drop(par, e);
        a_u32 label = expr_move_to(par, e, R_DYN);
        expr_dyn(e, label, e->_line);
    }
}

/**
 ** Fix the result of expression to specific register.
 ** The expression is unavailable after fix.
 *@param par the parser.
 *@param e the computed expression.
 *@param reg the register index to bind result.
 */
static void expr_pin_reg(Parser* par, InExpr e, a_u32 reg) {
    expr_drop(par, e);
    expr_move_to_if_need(par, e, reg);
	expr_val(e, reg, e->_line);
}

static void expr_to_top_tmp(Parser* par, InoutExpr e) {
    expr_drop(par, e);
	a_u32 reg = stack_alloc(par, e->_line);
    expr_pin_reg(par, e, reg);
    e->_fval = true;
}

static void expr_to_reg(Parser* par, InoutExpr e) {
	switch (e->_tag) {
		case EXPR_REG: {
			break;
		}
		case EXPR_REGS: {
			l_emit_iac(par, BC_TRIM, e->_d1, 1, e->_line);

			e->_tag = EXPR_REG;
			break;
		}
		case EXPR_REG_OR_NIL: {
			a_u32 reg = e->_d1;
            expr_drop(par, e);
			if (!e->_fsym) {
				l_merge_optR(par, e->_d2, reg, e->_line);
				e->_tag = EXPR_REG;
			}
			else {
				a_u32 reg2 = stack_alloc(par, e->_line);
				l_emit_iab(par, BC_MOV, reg2, reg, e->_line);
				l_merge_optR(par, e->_d2, reg2, e->_line);
				expr_tmp(e, reg2, e->_line);
			}
			break;
		}
		case EXPR_CALL: {
			a_u32 reg = stack_alloc(par, e->_line);
			assume(reg == bc_load_a(par->_code[e->_d1]), "vararg are not in the top.");
			expr_tmp(e, reg, e->_line);
			break;
		}
		default: {
            expr_to_top_tmp(par, e);
			break;
		}
	}
}

static void expr_to_tmp(Parser* par, InoutExpr e) {
	if (e->_tag != EXPR_REG || !e->_fsym) {
        expr_to_top_tmp(par, e);
	}
}

static void expr_to_reg_or_const(Parser* par, InoutExpr e) {
	switch (e->_tag) {
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
            expr_to_reg(par, e);
			break;
		}
	}
}

static void exprs_fix(Parser* par, InoutExpr e) {
	switch (e->_tag) {
		case EXPR_VDYN: {
			a_insn* ip = code_at(par, e->_d1);

			a_u32 reg = stack_alloc(par, e->_line);

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_a(ip, reg);
            bc_store_c(ip, DMB);

			l_emit_iab(par, BC_LBOXM, reg, reg, e->_line);

			expr_init(e, EXPR_NTMPC,
				._d1 = reg,
				._fval = true,
				._fucf = e->_fucf
			);
			break;
		}
		case EXPR_VCALL: {
			a_insn* ip = par->_code[e->_d1];

			a_u32 reg = stack_alloc(par, e->_line);
			assume(reg == bc_load_a(ip), "not top of stack.");

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_c(ip, DMB);

			l_emit_iab(par, BC_LBOXM, reg, reg, e->_line);

			expr_init(e, EXPR_NTMPC,
				._d1 = reg,
				._fval = true,
				._fucf = e->_fucf
			);
			break;
		}
		case EXPR_VNTMP: {
			a_u32 bot = e->_d1;

			l_emit_iab(par, BC_LBOXM, bot, bot, e->_line);
			stack_free_succ(par, bot + 1);

			e->_tag = EXPR_NTMPC;
			break;
		}
		case EXPR_VNTMPC: {
			a_u32 col = e->_d1;
			a_u32 bot = col + 1;

			l_emit_iab(par, BC_LPUSHM, col, bot, e->_line);
			stack_free_succ(par, bot);
			break;
		}
		default: {
            exprs_to_top_tmps(par, e);
			break;
		}
	}
}

static void exprs_to_top_tmps(Parser* par, InoutExpr e) {
	switch (e->_tag) {
		case EXPR_UNIT: {
			e->_tag = EXPR_NTMP;
			e->_d1 = par->_scope->_top_reg;
			break;
		}
		case EXPR_NTMP:
		case EXPR_VNTMP: {
			break;
		}
		case EXPR_NTMPC: {
			a_u32 col = e->_d1;
			a_u32 bot = col + 1;
			a_u32 top = par->_scope->_top_reg;

			if (bot < top) {
				l_emit_iabc(par, BC_LPUSH, col, bot, top - bot, e->_line);
				stack_free_succ(par, bot);
			}

			l_emit_iab(par, BC_UNBOXV, col, col, e->_line);

			e->_tag = EXPR_VNTMP;
			break;
		}
		case EXPR_VNTMPC: {
			a_u32 col = e->_d1;
			a_u32 bot = col + 1;
			a_u32 top = par->_scope->_top_reg;

			if (bot < top) {
				l_emit_iab(par, BC_LPUSHM, col, bot, e->_line);
				stack_free_succ(par, bot);
			}

			l_emit_iab(par, BC_UNBOXV, col, col, e->_line);

			e->_tag = EXPR_NTMP;
			break;
		}
		case EXPR_VREGS: {
			e->_tag = EXPR_VNTMP;
			break;
		}
		case EXPR_VDYN: {
			a_insn* ip = code_at(par, e->_d1);
			a_u32 reg = par->_scope->_top_reg;

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_a(ip, reg);
            bc_store_c(ip, DMB);

			e->_tag = EXPR_VNTMP;
			e->_d1 = reg;
			break;
		}
		case EXPR_VCALL: {
			a_insn* ip = code_at(par, e->_d1);
			a_u32 reg = bc_load_a(ip);

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_c(ip, DMB);

			e->_tag = EXPR_VNTMP;
			e->_d1 = reg;
			break;
		}
		default: {
            expr_to_top_tmp(par, e);

            e->_tag = EXPR_NTMP;
			break;
		}
	}
}

static a_enum expr_test_true(Parser* par, InExpr e, a_u32* plabel, a_u32 line) {
	switch (e->_tag) {
		case EXPR_TRUE:
		case EXPR_INT:
		case EXPR_FLOAT:
		case EXPR_STR: {
			return EXPR_TRUE;
		}
		case EXPR_NIL:
		case EXPR_FALSE: {
			*plabel = l_lazy_jump(par, *plabel, line);
			e->_fucf = true;
			return EXPR_RESIDUAL_FALSE;
		}
		case EXPR_RESIDUAL_TRUE: {
			l_mark_label(par, e->_d2, line);
			e->_tag = EXPR_TRUE;
			e->_fucf = false;
			return EXPR_TRUE;
		}
		case EXPR_TRUE_OR_FALSE: {
			l_merge_branch(par, plabel, e->_d2, line);
			return EXPR_TRUE_OR_FALSE;
		}
		case EXPR_FALSE_OR_TRUE: {
            branch_negate(par, plabel, e->_d2, line);
			return EXPR_TRUE_OR_FALSE;
		}
		case EXPR_REG_OR_NIL: {
			l_merge_branch(par, plabel, e->_d2, line);
			e->_tag = EXPR_REG;
			*plabel = l_emit_branch(par, bc_make_ia(BC_BZ, e->_d1), *plabel, line);
			return EXPR_TRUE_OR_FALSE;
		}
		case EXPR_DYN_OR_NIL: {
			l_merge_branch(par, plabel, e->_d2, line);
			e->_tag = EXPR_DYN;
			fallthrough;
		}
		default: {
            expr_to_reg(par, e);
			assume(e->_tag == EXPR_REG);
			*plabel = l_emit_branch(par, bc_make_ia(BC_BZ, e->_d1), *plabel, line);
			return EXPR_TRUE_OR_FALSE;
		}
	}
}

static a_enum expr_test_false(Parser* par, InExpr e, a_u32* plabel, a_u32 line) {
	switch (e->_tag) {
		case EXPR_NIL:
		case EXPR_FALSE: {
			return EXPR_FALSE;
		}
		case EXPR_INT:
		case EXPR_FLOAT:
		case EXPR_STR:
		case EXPR_TRUE: {
			*plabel = l_lazy_jump(par, *plabel, line);
			e->_fucf = true;
			return EXPR_RESIDUAL_TRUE;
		}
		case EXPR_RESIDUAL_FALSE: {
			l_mark_label(par, e->_d2, line);
			e->_tag = EXPR_FALSE;
			e->_fucf = false;
			return EXPR_FALSE;
		}
		case EXPR_FALSE_OR_TRUE: {
			l_merge_branch(par, plabel, e->_d2, line);
			return EXPR_FALSE_OR_TRUE;
		}
		case EXPR_TRUE_OR_FALSE: {
            branch_negate(par, plabel, e->_d2, line);
			return EXPR_FALSE_OR_TRUE;
		}
		case EXPR_REG_OR_NIL: {
			*plabel = l_emit_branch(par, bc_make_ia(BC_BNZ, e->_d1), *plabel, line);
			l_merge_branch(par, plabel, e->_d2, line);
			e->_tag = EXPR_REG;
			return EXPR_TRUE_OR_FALSE;
		}
		case EXPR_DYN_OR_NIL: {
			l_merge_branch(par, plabel, e->_d2, line);
			e->_tag = EXPR_DYN;
			fallthrough;
		}
		default: {
            expr_to_reg(par, e);
			*plabel = l_emit_branch(par, bc_make_ia(BC_BNZ, e->_d1), *plabel, line);
			return EXPR_FALSE_OR_TRUE;
		}
	}
}

static a_enum expr_test_nil(Parser* par, InExpr e, a_u32* plabel, a_u32 line) {
	switch (e->_tag) {
		case EXPR_TRUE:
		case EXPR_INT:
		case EXPR_FLOAT:
		case EXPR_STR:
		case EXPR_FALSE: {
			*plabel = l_lazy_jump(par, *plabel, line);
			return EXPR_RESIDUAL_FALSE;
		}
		case EXPR_NIL: {
			return EXPR_TRUE;
		}
		case EXPR_REG_OR_NIL: {
			*plabel = l_emit_branch(par, bc_make_ia(BC_BN, e->_d1), *plabel, line);
			l_mark_label(par, e->_d2, line);

			e->_tag = EXPR_REG;
			return EXPR_TRUE_OR_FALSE;
		}
		case EXPR_DYN_OR_NIL: {
			a_u32 label = e->_d2;

			e->_tag = EXPR_DYN;
			expr_to_reg(par, e);

			assume(e->_tag == EXPR_REG);
			*plabel = l_emit_branch(par, bc_make_ia(BC_BN, e->_d1), *plabel, line);
			l_mark_label(par, label, line);

			return EXPR_TRUE_OR_FALSE;
		}
		default: {
            expr_to_reg(par, e);

			assume(e->_tag == EXPR_REG);
			*plabel = l_emit_branch(par, bc_make_ia(BC_BN, e->_d1), *plabel, line);

			return EXPR_TRUE_OR_FALSE;
		}
	}
}

static a_enum expr_test_not_nil(Parser* par, InExpr e, a_u32* plabel, a_u32 line) {
	switch (e->_tag) {
		case EXPR_TRUE:
		case EXPR_INT:
		case EXPR_FLOAT:
		case EXPR_STR:
		case EXPR_FALSE: {
			return EXPR_TRUE;
		}
		case EXPR_NIL: {
			*plabel = l_lazy_jump(par, *plabel, line);
			e->_fucf = true;
			return EXPR_RESIDUAL_FALSE;
		}
		case EXPR_REG_OR_NIL: {
			l_merge_branch(par, plabel, e->_d2, line);
			*plabel = l_emit_branch(par, bc_make_ia(BC_BNN, e->_d1), *plabel, line);

			e->_tag = EXPR_REG;
			return EXPR_TRUE_OR_FALSE;
		}
		case EXPR_DYN_OR_NIL: {
			l_merge_branch(par, plabel, e->_d2, line);

			e->_tag = EXPR_DYN;
			fallthrough;
		}
		default: {
            expr_to_reg(par, e);
			assume(e->_tag == EXPR_REG);
			*plabel = l_emit_branch(par, bc_make_ia(BC_BNN, e->_d1), *plabel, line);

			return EXPR_TRUE_OR_FALSE;
		}
	}
}

static a_u32 expr_catch_nil_branch(Parser* par, InoutExpr e, a_u32 line) {
	switch (e->_tag) {
		case EXPR_NIL: {
			a_u32 label = l_lazy_jump(par, NO_LABEL, line);
			e->_tag = EXPR_NIL;
			return label;
		}
		case EXPR_DYN_OR_NIL: {
			a_u32 label = e->_d2;
			e->_tag = EXPR_DYN;
			return label;
		}
		case EXPR_REG_OR_NIL: {
			a_u32 label = e->_d2;
			e->_tag = EXPR_REG;
			return label;
		}
		default: {
			return NO_LABEL;
		}
	}
}

/*=========================================================*/

#define lex_line(par) (lex(par)->_ahead[0]._line)
#define lex_token(par) (&lex(par)->_ahead[0])

static a_i32 lex_peek(Parser* par) {
	return ai_lex_peek(lex(par));
}

static a_i32 lex_peek2(Parser* par, a_u32 line) {
	return ai_lex_peek2(lex(par), line);
}

static void lex_sync(Parser* par) {
	lex_peek(par);
}

static void lex_skip(Parser* par) {
	lex_token(par)->_tag = TK__NONE;
}

static GStr* lex_ident(Parser* par) {
	Token* token = lex_token(par);
	assume(token->_tag == TK_IDENT);
	return token->_str;
}

static a_bool lex_test(Parser* par, a_i32 tk) {
	return lex_peek(par) == tk;
}

static a_i32 lex_forward(Parser* par) {
	return ai_lex_forward(lex(par));
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
    ai_par_error(par, fmt", got %s", lex_line(par), ##args, ai_lex_tkrepr(lex_token(par), _buf)); \
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

static a_none lex_error_bracket(Parser* par, a_i32 ltk, a_i32 rtk, a_line line) {
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

static void l_scan_atom(Parser* par, OutExpr e);
static void l_scan_expr(Parser* par, OutExpr e);
static void l_scan_expr_from_term(Parser* par, OutExpr e);
static void l_scan_exprs(Parser* par, InoutExpr es, a_bool exists);
static void l_scan_pattern(Parser* par, void (*con)(Parser*, Pat*, a_usize), a_usize ctx);
static void l_scan_stat(Parser* par);
static void l_scan_stats(Parser* par);

static void l_scan_tstring(Parser* par, OutExpr e) {
	ConExpr ce = {};
	a_u32 line = lex_line(par);
	lex_skip(par);

	loop {
		a_u32 line2 = lex_line(par);
		switch (lex_peek2(par, line)) {
			case TK_STRING: {
				expr_str(e, lex_token(par)->_str, line2);
                lex_skip(par);
				break;
			}
			case TK_TSESCAPE: {
                lex_skip(par);
                l_scan_atom(par, e);
				break;
			}
			case TK_TSEND: {
                lex_skip(par);
				expr_concat_end(par, &ce, e, lex_line(par));
				return;
			}
			default: unreachable();
		}
		expr_concat(par, &ce, e, line2);
	}
}

static void l_scan_function(Parser* par, OutExpr e, GStr* name, a_line line) {
	FnScope scope;

	fnscope_prologue(par, &scope, line);

	a_line line1 = lex_line(par);
	lex_check_skip(par, TK_LBK);
    if (!lex_test_skip(par, TK_RBK)) {
        l_scan_pattern(par, param_bind, line1);
        lex_check_pair_right(par, TK_LBK, TK_RBK, line1);
    }

	lex_sync(par);
	line = lex_line(par);

	switch (lex_peek(par)) {
		case TK_LBR: {
			lex_skip(par);
			l_scan_stats(par);
    		lex_check_pair_right(par, TK_LBR, TK_RBR, line);
			break;
		}
		case TK_ASSIGN: {
			lex_skip(par);
			l_scan_expr(par, e);
			expr_return(par, e, line);
			break;
		}
	}

	GProto* meta = fnscope_epilogue(par, name, false, lex_line(par));
	expr_func(par, e, meta);
}

static void l_scan_lambda(Parser* par, OutExpr e) {
	FnScope scope;

	fnscope_prologue(par, &scope, lex_line(par));

	if (!lex_test_skip(par, TK_BBAR)) {
		a_line line = lex_line(par);
		lex_check_skip(par, TK_BAR);
		l_scan_pattern(par, param_bind, scope._begin_line);
		lex_check_pair_right(par, TK_BAR, TK_BAR, line);
	}

	if (lex_test_skip(par, TK_LBR)) {
		a_line line = lex_line(par);

		l_scan_stats(par);
        lex_check_pair_right(par, TK_LBR, TK_RBR, line);
	}
	else {
		Expr e2;

		l_scan_expr(par, e2);
		expr_return(par, e2, scope._begin_line);
	}

	GProto* meta = fnscope_epilogue(par, null, false, lex_line(par));
	expr_func(par, e, meta);
}

static void l_scan_table_index_or_value(Parser* par, OutExpr e, a_int* pindex) {
    Expr e2;
    Expr e3;

    a_u32 line = lex_line(par);
    if (!lex_test_skip(par, TK_RSQ)) {
        l_scan_expr(par, e2);
        if (lex_test_skip(par, TK_RSQ)) {
            if (lex_test_skip(par, TK_ASSIGN) || lex_test_skip(par, TK_COLON)) {
				expr_index(par, e, e2, line);

                l_scan_expr(par, e3);

				expr_assign(par, e, e3, line);
                return;
            }
        }
        else {
            l_scan_exprs(par, e2, true);
            lex_check_pair_right(par, TK_LSQ, TK_RSQ, line);
        }
    }
    else {
		expr_unit(e2);
    }
	expr_box_list(par, e2, line);
    l_scan_expr_from_term(par, e2);

    a_int index = (*pindex)++;
	expr_int(e3, index, line);
	expr_index(par, e, e3, line);
	expr_assign(par, e, e2, line);
}

static void l_scan_table_constructor(Parser* par, OutExpr e) {
    a_u32 line = lex_line(par);
    Scope scope;
    Expr er;

    lex_skip(par);
    if (lex_test_skip(par, TK_RBR)) {
		expr_new_table(par, e, line);
        return;
    }

	scope_enter(par, &scope, line);

    local_bind(par, e, null, line);
	expr_new_table(par, er, line);
	expr_assign(par, e, er, line);

    a_int index = 0;

    do {
        expr_copy(er, e);

        lex_sync(par);
        if (lex_forward(par) == TK_ASSIGN || lex_forward(par) == TK_COLON) {
            Expr ek;
            Expr ev = {};

            a_line line2 = lex_line(par);

            if (lex_test(par, TK_IDENT)) {
				expr_index_str(par, er, lex_ident(par), line2);
				lex_skip(par);
            }
            else if (lex_test_skip(par, TK_INTEGER)) {
				expr_int(ek, lex_token(par)->_int, line2);
				expr_index(par, er, ek, line2);
            }
            else {
                lex_error_got(par, "table entry expected");
            }

            lex_sync(par);
            lex_skip(par);

            l_scan_expr(par, ev);

			expr_assign(par, er, ev, line2);
        }
        else if (lex_test_skip(par, TK_LSQ)) { /* List or setting by index */
            l_scan_table_index_or_value(par, er, &index);
        }
        else {
            Expr ek;
            Expr ev;

            l_scan_expr(par, ev);

			expr_int(ek, index++, line);
			expr_index(par, er, ek, line);
			expr_assign(par, er, ev, line);
        }
    }
    while (lex_test_skip(par, TK_COMMA) || lex_test_skip(par, TK_SEMI));

    lex_check_pair_right(par, TK_LBR, TK_RBR, line);

	scope_leave_with(par, lex_line(par), e);
}

static void l_scan_atom(Parser* par, OutExpr e) {
	switch (lex_peek(par)) {
		case TK_nil: {
			expr_const(e, EXPR_NIL, lex_line(par));
            lex_skip(par);
			break;
		}
		case TK_false: {
			expr_const(e, EXPR_FALSE, lex_line(par));
            lex_skip(par);
			break;
		}
		case TK_true: {
			expr_const(e, EXPR_TRUE, lex_line(par));
            lex_skip(par);
			break;
		}
		case TK_INTEGER: {
			expr_int(e, lex_token(par)->_int, lex_line(par));
            lex_skip(par);
			break;
		}
		case TK_FLOAT: {
			expr_float(e, lex_token(par)->_float, lex_line(par));
            lex_skip(par);
			break;
		}
		case TK_STRING: {
			expr_str(e, lex_token(par)->_str, lex_line(par));
            lex_skip(par);
			break;
		}
		case TK_TSBEGIN: {
			l_scan_tstring(par, e);
			break;
		}
		case TK_IDENT: {
			expr_symbol(par, e, lex_ident(par), lex_line(par));
            lex_skip(par);
			break;
		}
		case TK_LBK: {
			a_u32 line = lex_line(par);
            lex_skip(par);

			if (lex_test_skip(par, TK_RBK)) {
				expr_const(e, EXPR_UNIT, line);
				expr_box_tuple(par, e, line);
			}
			else {
				l_scan_expr(par, e);
				if (!lex_test_skip(par, TK_RBK)) {
					if (lex_test_skip(par, TK_COMMA) && !lex_test(par, TK_RBK)) {
						l_scan_exprs(par, e, true);
					}
                    lex_check_pair_right(par, TK_LBK, TK_RBK, line);
					expr_box_tuple(par, e, line);
				}
			}
			break;
		}
		case TK_LSQ: {
			a_u32 line = lex_line(par);
            lex_skip(par);
			if (!lex_test_skip(par, TK_RSQ)) {
				l_scan_exprs(par, e, false);
                lex_check_pair_right(par, TK_LSQ, TK_RSQ, line);
			}
			else {
				expr_const(e, EXPR_UNIT, line);
			}
			expr_box_list(par, e, line);
			break;
		}
		case TK_LBR: {
            l_scan_table_constructor(par, e);
			break;
		}
		default: {
			lex_error_got(par, "expression expected");
		}
	}
}

static void l_scan_term_suffix(Parser* par, InoutExpr e, a_line line) {
	a_u32 label = NO_LABEL;
	loop {
		switch (lex_peek(par)) {
			case TK_QDOT: {
				a_line line2 = lex_line(par);
                lex_skip(par);
				expr_or_nil(par, e, &label,  line2);
				GStr* name = lex_check_ident(par);
                if (lex_test_skip(par, TK_LBK)) {
                    expr_lookup(par, e, name, line2);

                    if (!lex_test_skip(par, TK_RBK)) {
                        l_scan_exprs(par, e, true);
                        lex_check_pair_right(par, TK_LBK, TK_RBK, line2);
                    }

                    expr_call(par, e, line2);
                }
                else {
                    expr_index_str(par, e, name, line2);
                }
				break;
			}
			case TK_BANG: {
				a_line line2 = lex_line(par);
                lex_skip(par);

				expr_or_ret(par, e, line2);
				break;
			}
			case TK_DOT: {
				a_line line2 = lex_line(par);
                lex_skip(par);
				GStr* name = lex_check_ident(par);
                if (lex_test_skip(par, TK_LBK)) {
                    expr_lookup(par, e, name, line2);

                    if (!lex_test_skip(par, TK_RBK)) {
                        l_scan_exprs(par, e, true);
                        lex_check_pair_right(par, TK_LBK, TK_RBK, line2);
                    }

                    expr_call(par, e, line2);
                }
                else {
                    expr_index_str(par, e, name, line2);
                }
				break;
			}
			case TK_LSQ: {
				Expr e2 = {};
				a_line line2 = lex_line(par);
                lex_skip(par);
				l_scan_expr(par, e2);
				expr_index(par, e, e2, line2);
				while (lex_test_skip(par, TK_COMMA)) {
					l_scan_expr(par, e2);
					expr_index(par, e, e2, line2);
				}
                lex_check_pair_right(par, TK_LSQ, TK_RSQ, line2);
				break;
			}
			case TK_LBK: {
				a_line line2 = lex_line(par);
                lex_skip(par);

				if (!lex_test_skip(par, TK_RBK)) {
					l_scan_exprs(par, e, true);
                    lex_check_pair_right(par, TK_LBK, TK_RBK, line2);
				}
				expr_call(par, e, line2);
				break;
			}
			default: {
				expr_phi_nil(par, e, label, line);
				return;
			}
		}
	}
}

static void l_scan_term(Parser* par, InoutExpr e) {
	if (lex_test(par, TK_BAR) || lex_test(par, TK_BBAR)) {
		l_scan_lambda(par, e);
	}
	else {
		a_line line = lex_line(par);
		l_scan_atom(par, e);
		l_scan_term_suffix(par, e, line);
	}
}

static void l_scan_term_with_prefix(Parser* par, InoutExpr e) {
    a_i32 tag = lex_peek(par);
    a_line line = lex_line(par);
	switch (tag) {
		case TK_PLUS: {
			//TODO should VM check type for this operation?
            lex_skip(par);
            l_scan_term_with_prefix(par, e);
			break;
		}
		case TK_MINUS: {
            lex_skip(par);
            l_scan_term_with_prefix(par, e);
			expr_neg(par, e, line);
			break;
		}
		case TK_TILDE: {
            lex_skip(par);
            l_scan_term_with_prefix(par, e);
			expr_bit_inv(par, e, line);
			break;
		}
		case TK_BANG: {
            lex_skip(par);
            l_scan_term_with_prefix(par, e);
			expr_not(par, e, line);
			break;
		}
		case TK_SHARP: {
            lex_skip(par);
            l_scan_term_with_prefix(par, e);
			expr_len(par, e, line);
			break;
		}
		case TK_STAR: {
            lex_skip(par);
            l_scan_term_with_prefix(par, e);
			expr_unbox(par, e, line);
			break;
		}
		case TK_fn: {
            lex_skip(par);
			l_scan_function(par, e, null, line);
			break;
		}
		default: {
            l_scan_term(par, e);
			break;
		}
	}
}

static void l_scan_terms(Parser *par, InoutExpr e) {
	l_scan_term_with_prefix(par, e);
	if (lex_test_skip(par, TK_TDOT)) {
		expr_unpack(par, e, lex_line(par));
	}
}

static a_u32 l_scan_arith_op(Parser* par) {
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

static void l_scan_arith_expr_leveled(Parser* par, InoutExpr e, a_u32 lv_min);

static void l_scan_arith_tail(Parser* par, InoutExpr e, a_u32 lv_min) {
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
	while ((op = l_scan_arith_op(par)) != OP__NOT_BIN) {
		lv = prios[op];
		if (!(lv_min <= lv && lv <= lv_max))
			break;

		Expr e2;
		a_line line = lex_line(par);
		expr_binary_left(par, e, op, line);
        lex_skip(par);
		l_scan_arith_expr_leveled(par, e2, lv + 1);
		expr_binary(par, e, e2, op, line);

		lv_max = lv;
	}
}

static void l_scan_arith_expr_leveled(Parser* par, InoutExpr e, a_u32 lv) {
	l_scan_terms(par, e);
	l_scan_arith_tail(par, e, lv);
}

#define l_scan_arith_expr(par,e) l_scan_arith_expr_leveled(par, e, 0)

static a_u32 l_scan_compare_op(Parser* par) {
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

static void l_scan_compare_expr(Parser* par, InoutExpr e);

static void l_scan_compare_tail(Parser* par, InoutExpr e) {
	a_u32 op1, op2;
	a_line line1;

	op1 = l_scan_compare_op(par);
	if (op1 == 0) return;

    lex_skip(par);
	line1 = lex_line(par);
	expr_binary_left(par, e, op1, line1);

	Expr e2;
	l_scan_arith_expr(par, e2);
	expr_binary(par, e, e2, op1, line1);

	op2 = l_scan_compare_op(par);
	while (op2 != OP__NOT_BIN) {
		a_u32 line2;

        lex_skip(par);
		line2 = lex_line(par);
		expr_binary_left(par, e, OP_AND, line2);
		expr_binary_left(par, e2, op2, line2);

		Expr e3;
		l_scan_arith_expr(par, e3);
		expr_binary(par, e2, e3, op2, line2);

		expr_discard(par, e3);
		expr_binary(par, e, e2, OP_AND, line2);

		op2 = l_scan_compare_op(par);
	}
}

static void l_scan_compare_expr(Parser* par, InoutExpr e) {
	l_scan_arith_expr(par, e);
	l_scan_compare_tail(par, e);
}

static a_u32 l_scan_relation_op(Parser* par) {
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
			switch (lex_forward(par)) {
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

static void l_scan_relation_tail(Parser* par, InoutExpr e) {
	a_u32 op = l_scan_relation_op(par);
	if (op != 0) {
		a_u32 line = lex_line(par);
        lex_skip(par);
		expr_binary_left(par, e, op, line);

		Expr e2;
		l_scan_compare_expr(par, e2);
		expr_binary(par, e, e2, op, line);
	}
}

static void l_scan_relation_expr(Parser* par, InoutExpr e) {
	l_scan_compare_expr(par, e);
	l_scan_relation_tail(par, e);
}

static void l_scan_concatenate_expr(Parser* par, InoutExpr e);

static void l_scan_concatenate_tail(Parser* par, InoutExpr e) {
	a_line line1 = lex_line(par);
	if (lex_test_skip(par, TK_BDOT)) {
		ConExpr ce = { };
		expr_concat(par, &ce, e, line1);
		do {
			a_u32 line2 = lex_line(par);
			l_scan_relation_expr(par, e);
			expr_concat(par, &ce, e, line2);
		}
		while (lex_test_skip(par, TK_BDOT));
		expr_concat_end(par, &ce, e, line1);
	}
}

static void l_scan_concatenate_expr(Parser* par, InoutExpr e) {
	l_scan_relation_expr(par, e);
	l_scan_concatenate_tail(par, e);
}

static void l_scan_conjunctive_tail(Parser* par, InoutExpr e) {
	while (lex_test(par, TK_BAMP)) {
		Expr e2;
		expr_binary_left(par, e, OP_AND, lex_line(par));
        lex_skip(par);
		l_scan_concatenate_expr(par, e2);
		expr_binary(par, e, e2, OP_AND, lex_line(par));
	}
}

static void l_scan_conjunctive_expr(Parser* par, InoutExpr e) {
	l_scan_concatenate_expr(par, e);
	l_scan_conjunctive_tail(par, e);
}

static void l_scan_disjunctive_expr(Parser* par, InoutExpr e);

static void l_scan_disjunctive_tail(Parser* par, InoutExpr e) {
	while (lex_test(par, TK_BBAR)) {
		Expr e2;
		expr_binary_left(par, e, OP_OR, lex_line(par));
        lex_skip(par);
		l_scan_conjunctive_expr(par, e2);
		expr_binary(par, e, e2, OP_OR, lex_line(par));
	}
}

static void l_scan_disjunctive_expr(Parser* par, InoutExpr e) {
	l_scan_conjunctive_expr(par, e);
	l_scan_disjunctive_tail(par, e);
}

static void l_scan_ternary_expr(Parser* par, InoutExpr e);

static void l_scan_ternary_tail(Parser* par, InoutExpr e) {
	switch (lex_peek(par)) {
		case TK_QUESTION: {
			a_line line = lex_line(par);
			a_u32 label1 = expr_test(par, e, line);

			lex_skip(par);

			expr_discard(par, e);
			l_scan_ternary_expr(par, e);

			lex_check_pair_right(par, TK_QUESTION, TK_COLON, line);

			a_u32 label2 = NO_LABEL;
			expr_or_else(par, e, &label2, line);

			Expr e2;
			l_mark_label(par, label1, line);
			l_scan_ternary_expr(par, e2);
			expr_phi(par, e, e2, label2, line);
			break;
		}
		case TK_ELVIS: {
			a_line line = lex_line(par);
			a_u32 label1 = expr_test(par, e, line);

			lex_skip(par);

			a_u32 label2 = NO_LABEL;
			expr_or_else(par, e, &label2, line);

			Expr e2;
			l_mark_label(par, label1, line);
			l_scan_ternary_expr(par, e2);
			expr_phi(par, e, e2, label2, line);
			break;
		}
		case TK_BQUESTION: {
			a_line line = lex_line(par);
			a_u32 label1 = NO_LABEL;
			expr_test_nil(par, e, &label1, line);

			lex_skip(par);

			a_u32 label2 = NO_LABEL;
			expr_or_else(par, e, &label2, line);

			Expr e2;
			l_mark_label(par, label1, line);
			l_scan_ternary_expr(par, e2);
			expr_phi(par, e, e2, label2, line);
			break;
		}
	}
}

static void l_scan_ternary_expr(Parser* par, InoutExpr e) {
	l_scan_disjunctive_expr(par, e);
	l_scan_ternary_tail(par, e);
}

static void l_scan_expr(Parser* par, InoutExpr e) {
	l_scan_ternary_expr(par, e);
}

static void l_scan_expr_from_term(Parser* par, OutExpr e) {
	l_scan_arith_tail(par, e, 0);
	l_scan_compare_tail(par, e);
	l_scan_relation_tail(par, e);
	l_scan_concatenate_tail(par, e);
	l_scan_conjunctive_tail(par, e);
	l_scan_disjunctive_tail(par, e);
	l_scan_ternary_tail(par, e);
}

static void l_scan_exprs(Parser* par, InoutExpr es, a_bool exists) {
	if (!exists) {
		l_scan_expr(par, es);
		if (!lex_test_skip(par, TK_COMMA))
			return;
	}

	do {
		Expr e;

		exprs_push_left(par, es);

		l_scan_expr(par, e);
		exprs_push(par, es, e);
	}
	while (lex_test_skip(par, TK_COMMA));
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
    lex_check_skip(par, TK_ASSIGN);
	a_u32 line = lex_line(par);

	Expr rhs = {};
	Expr rhs_last;

	l_scan_expr(par, rhs_last);
	if (lex_test_skip(par, TK_COMMA)) {
		expr_copy(rhs, rhs_last);
		exprs_push_left(par, rhs);
		loop {
			l_scan_expr(par, rhs_last);

			if (!lex_test_skip(par, TK_COMMA))
				break;

			exprs_push(par, rhs, rhs_last);
			exprs_push_left(par, rhs);
		}
	}

	a_u32 num_nil = exprs_trunc(par, rhs, rhs_last, count, line);

	LhsNode* node = tail;

	while (num_nil > 0) {
		Expr en;
		expr_const(en, EXPR_NIL, line);
		expr_assign(par, node->_expr, en, line);

		node = node->_last;
		assume(node != null);
		num_nil -= 1;
	}

	loop {
		expr_assign(par, node->_expr, rhs_last, line);
		if ((node = node->_last) == null)
			return;

		exprs_pop(par, rhs, rhs_last, line);
	}
}

static void l_scan_assign_lhs_tail(Parser* par, Lhs* lhs) {
	LhsNode n = {};

	if (lex_test_skip(par, TK_COMMA)) {
		n._last = lhs->_tail;

		lhs->_count += 1;
		lhs->_tail = &n;

        l_scan_term_with_prefix(par, n._expr);
		return l_scan_assign_lhs_tail(par, lhs);
	}

	l_scan_assign_rhs(par, lhs->_tail, lhs->_count + 1);
}

static void l_scan_assign_or_call(Parser* par) {
	Lhs lhs = {};

	a_line line = lex_line(par);
	l_scan_term(par, lhs._head._expr);
	if (lex_test(par, TK_COMMA) || lex_test_skip(par, TK_TDOT) || lex_test(par, TK_ASSIGN)) {
		lhs._tail = &lhs._head;
		l_scan_assign_lhs_tail(par, &lhs);
	}
	else if (lhs._head._expr->_tag != EXPR_CALL) {
		ai_par_error(par, "assignment or function call expected.", line);
	}

    lex_test_skip(par, TK_SEMI);
	expr_discard(par, lhs._head._expr);
}

static void l_scan_if_stat(Parser* par) {
	lex_skip(par);

	lex_check_skip(par, TK_LBK);
	a_u32 line = lex_line(par);

	Expr ep;
	l_scan_expr(par, ep);
	a_u32 label1 = expr_test(par, ep, line);
	expr_discard(par, ep);

	lex_check_pair_right(par, TK_LBK, TK_RBK, line);
	lex_skip(par);

	l_scan_stat(par);

	if (lex_test_skip(par, TK_else)) {
		a_u32 label2 = l_lazy_jump(par, NO_LABEL, lex_line(par));

		lex_sync(par);
		l_mark_label(par, label1, lex_line(par));
		l_scan_stat(par);

		l_mark_label(par, label2, lex_line(par));
	}
	else {
		l_mark_label(par, label1, lex_line(par));
	}
}

static void l_scan_while_stat(Parser* par) {
    lex_skip(par);

    lex_check_skip(par, TK_LBK);
	a_line line = lex_line(par);

	a_u32 label1 = l_mark_label(par, NO_LABEL, line);

	Expr ep;
	l_scan_expr(par, ep);
	a_u32 label2 = expr_test(par, ep, line);
	expr_discard(par, ep);

    lex_check_pair_right(par, TK_LBK, TK_RBK, line);
    lex_skip(par);

	l_scan_stat(par);
	l_direct_jump(par, label1, lex_line(par));

	l_mark_label(par, label2, lex_line(par));

	if (lex_test_skip(par, TK_else)) {
        lex_sync(par);
		l_scan_stat(par);
	}
}

static void l_scan_loop_stat(Parser* par) {
	lex_skip(par);

	a_u32 label = l_mark_label(par, NO_LABEL, lex_line(par));

	l_scan_stat(par);
	l_direct_jump(par, label, lex_line(par));
}

static void l_scan_return_stat(Parser* par) {
	a_line line = lex_line(par);

    lex_skip(par);

	Expr e = {};

	if (!lex_test_sep(par)) {
		l_scan_exprs(par, e, false);
	}

    lex_test_skip(par, TK_SEMI);
	expr_return(par, e, line);
}

static void l_scan_pattern_recursive(Parser* par, PatInfo* info, Pat* parent, Pat** slot, a_u32 tag) {
    Pat pat_desc = { ._sec_ref = NIL_SEC_REF };

    Pat* pat = &pat_desc;

    pat->_parent = parent;
    pat->_index = parent->_nchild;
	parent->_nchild += 1;
	*slot = pat;

	switch (tag) {
		case PAT_VARG:
		case PAT_TUPLE:
			goto branch_standard;
		default: unreachable();
	}

branch_standard:
	switch (lex_peek(par)) {
		case TK_IDENT: {
            pat->_kind = PAT_VAR;
            pat->_name = lex_ident(par);
            pat->_line = lex_line(par);
			lex_skip(par);
			break;
		}
		case TK__: {
            lex_skip(par);
			parent->_fcpx = true;
            pat->_kind = PAT_DROP;
            pat->_line = lex_line(par);
			break;
		}
		case TK_LBK: {
            lex_skip(par);
			parent->_fcpx = true;
            pat->_kind = PAT_TUPLE;
            pat->_line = lex_line(par);
            pat->_nchild = 0;
			if (!lex_test_skip(par, TK_RBK)) {
				return l_scan_pattern_recursive(par, info, pat, &pat->_child, PAT_TUPLE);
			}
			break;
		}
		case TK_LSQ: {
            lex_skip(par);
			parent->_fcpx = true;
            pat->_kind = PAT_LIST;
            pat->_line = lex_line(par);
            pat->_nchild = 0;
			if (!lex_test_skip(par, TK_RSQ)) {
				return l_scan_pattern_recursive(par, info, pat, &pat->_child, PAT_LIST);
			}
			break;
		}
		case TK_LBR: {
            lex_skip(par);
			parent->_fcpx = true;
            pat->_kind = PAT_TABLE;
            pat->_line = lex_line(par);
            pat->_nchild = 0;
			if (!lex_test_skip(par, TK_RBR)) {
				return l_scan_pattern_recursive(par, info, pat, &pat->_child, PAT_TABLE);
			}
			break;
		}
		default: goto error;
	}

	slot = &pat->_sibling;
	loop {
		switch (lex_peek(par)) {
			case TK_COMMA: {
                lex_skip(par);
				tag = parent->_kind; /* Recover goto label. */
				return l_scan_pattern_recursive(par, info, parent, slot, tag);
			}
			case TK_RBK: {
				if (parent->_kind != PAT_TUPLE) {
					if (parent->_kind == PAT_VARG)
						goto load_nils;
					goto error_unexpected;
				}
                lex_skip(par);
				break;
			}
			case TK_RSQ: {
				if (parent->_kind != PAT_LIST) {
					if (parent->_kind == PAT_VARG)
						goto load_nils;
					goto error_unexpected;
				}
                lex_skip(par);
				break;
			}
			case TK_RBR: {
				if (parent->_kind != PAT_TABLE) {
					if (parent->_kind == PAT_VARG)
						goto load_nils;
					goto error_unexpected;
				}
                lex_skip(par);
				break;
			}
			case TK_ASSIGN: {
                lex_skip(par);
				if (parent->_kind == PAT_VARG) {
					return (*info->_con)(par, &info->_root, info->_ctx);
				}
				else if (pat->_fdfl) {
					goto error;
				}
				else {
					SecRec rec;
					sec_start(par, rec);
					l_scan_expr(par, pat->_expr);
					pat->_sec_ref = sec_record(par, rec);
					pat->_fdfl = true;
					continue;
				}
			}
			default:
			load_nils: {
				return (*info->_con)(par, &info->_root, info->_ctx);
			}
		}

		assume(parent != null);
		slot = &parent->_sibling;
        pat = parent;
		parent = parent->_parent;
	}

error:
	lex_error_got(par, "malformed pattern");

error_unexpected:
	switch (parent->_kind) {
		case PAT_VARG:
			lex_error_got(par, "malformed pattern");
		case PAT_TUPLE:
            lex_error_bracket(par, '(', ')', parent->_expr->_line);
		case PAT_LIST:
            lex_error_bracket(par, '[', ']', parent->_expr->_line);
		case PAT_TABLE:
            lex_error_bracket(par, '{', '}', parent->_expr->_line);
		default:
			unreachable();
	}
}

static void l_scan_pattern(Parser* par, void (*con)(Parser*, Pat*, a_usize), a_usize ctx) {
	PatInfo info = {
		._root = { ._kind = PAT_VARG, ._sec_ref = NIL_SEC_REF },
		._con = con,
		._ctx = ctx
	};
	l_scan_pattern_recursive(par, &info, &info._root, &info._root._child, PAT_VARG);
}

static void l_scan_for_stat(Parser* par) {
	Scope scope = {};
	ForStat stat;

	Expr e;
	lex_skip(par);

	a_u32 line = lex_line(par);
	scope_enter(par, &scope, line);
	stat._line = line;
	
	lex_check_skip(par, TK_LBK);
	l_scan_expr(par, e);
	lex_check_pair_right(par, TK_LBK, TK_RBK, line);
	expr_iter(par, e, line);

	a_u32 label = l_mark_label(par, NO_LABEL, line);

	a_u32 line2 = lex_line(par);
	lex_check_skip(par, TK_BAR);
	l_scan_pattern(par, for_bind, addr_of(&stat));
	lex_check_pair_right(par, TK_BAR, TK_BAR, line2);

	l_scan_stat(par);
	l_direct_jump(par, label, line);

	l_mark_label(par, stat._label, line); /* TODO: use scoped label management. */
	scope_pop(par, line);
	
	expr_drop(par, e);
}

static void l_scan_let_stat2(Parser* par, Pat* p, a_usize c) {
	a_line line = cast(a_line, c);

	if (lex_test_skip(par, '=')) {
		Expr e;
		Pat* pat = p->_child;

		do {
			l_scan_expr(par, e);
    	    pat_bind(par, pat, e);
			
			pat = pat->_sibling;
		}
		while (lex_test_skip(par, TK_COMMA) && pat != null);

		if (pat == null) {
			/* If no pattern remains, discard remainding expressions */
			expr_discard(par, e);
			
			do {
				l_scan_expr(par, e);
				expr_discard(par, e);
			}
			while (lex_test_skip(par, TK_COMMA));
		}
	}

    pat_bind_nils(par, p, line);
    stack_compact(par);
}

static void l_scan_let_stat(Parser* par) {
	a_line line = lex_line(par);
    lex_skip(par);

	switch (lex_peek(par)) {
		case TK_fn: {
			lex_skip(par);
			goto scan_func;
		}
        case TK_use: {
            Expr e1, e2;
            lex_skip(par);
            GStr* name = lex_check_ident(par);
            local_bind(par, e1, name, line);

            lex_check_skip(par, TK_ASSIGN);

            l_scan_expr(par, e2);
            expr_or_ret(par, e2, line);
            expr_assign(par, e1, e2, line);
            expr_tbc(par, e1, line);
            break;
        }
		case TK_IDENT: {
			if (lex_forward(par) == TK_LBK) {
                Expr e1, e2;
                GStr* name;
			scan_func:
                name = lex_check_ident(par);
    	        local_bind(par, e1, name, line);

				l_scan_function(par, e2, name, line);
				expr_assign(par, e1, e2, line);
				break;
			}
			fallthrough;
		}
		default: {
			l_scan_pattern(par, l_scan_let_stat2, line);
			break;
		}
	}
}

static void l_scan_fun_def_stat(Parser* par) {
	a_line line = lex_line(par);
    lex_skip(par);

	Expr en;
	Expr ef;

	GStr* name = lex_check_ident(par);
	expr_symbol(par, en, name, lex_line(par));
	while (lex_test_skip(par, TK_DOT)) {
		name = lex_check_ident(par);
		expr_index_str(par, en, name, lex_line(par));
	}

	l_scan_function(par, ef, name, line);

	expr_assign(par, en, ef, line);
}

static void l_scan_pub_stat(Parser* par) {
	a_line line = lex_line(par);
    lex_skip(par);

	if ((lex_peek(par) == TK_IDENT && lex_forward(par) == TK_LBK) || lex_test_skip(par, TK_fn)) {
		Expr e1, e2;
		GStr* name = lex_check_ident(par);
        sym_export(par, e1, name, line);
		l_scan_function(par, e2, name, line);
		expr_assign(par, e1, e2, line);
	}
	else {
		Expr e;
		GStr* name = lex_check_ident(par);
        sym_export(par, e, name, line);
		if (lex_test_skip(par, TK_ASSIGN)) {
			Expr e2 = {};
			l_scan_expr(par, e2);
			expr_assign(par, e, e2, line);
		}
	}
}

static void l_scan_stat_pack(Parser* par) {
	Scope scope;

	a_line line = lex_line(par);
    lex_skip(par);
	scope_enter(par, &scope, line);

	l_scan_stats(par);

    lex_check_pair_right(par, TK_LBR, TK_RBR, line);

	scope_leave(par, lex_line(par));
}

static void l_scan_stat(Parser* par) {
	assume(par->_scope->_top_ntr == par->_scope->_top_reg, "compute stack leaked.");
	switch (lex_peek(par)) {
		case TK_if: {
			l_scan_if_stat(par);
			break;
		}
		case TK_return: {
			l_scan_return_stat(par);
			break;
		}
		case TK_while: {
			l_scan_while_stat(par);
			break;
		}
		case TK_for: {
			l_scan_for_stat(par);
			break;
		}
		case TK_loop: {
			l_scan_loop_stat(par);
			break;
		}
		case TK_fn: {
			l_scan_fun_def_stat(par);
			break;
		}
		case TK_LBR: {
			l_scan_stat_pack(par);
			break;
		}
		case TK_pub: {
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
		switch (lex_peek(par)) {
			case TK_RBR:
			case TK_EOF:
				return;
			case TK_SEMI: {
                lex_skip(par);
				break;
			}
			case TK_let: {
				l_scan_let_stat(par);
				break;
			}
			case TK_return: {
				l_scan_return_stat(par);
				break;
			}
			case TK_if: {
				l_scan_if_stat(par);
				break;
			}
			case TK_while: {
				l_scan_while_stat(par);
				break;
			}
			case TK_for: {
				l_scan_for_stat(par);
				break;
			}
			case TK_loop: {
				l_scan_loop_stat(par);
				break;
			}
			case TK_fn: {
				l_scan_fun_def_stat(par);
				break;
			}
			case TK_pub: {
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
	Expr e = {};

	if (!lex_test_sep(par)) {
		l_scan_exprs(par, e, false);
		/* Unpack the return value if only return one value and it can be unpacked. */
		if (e->_fupk) {
			expr_unpack(par, e, lex_line(par));
		}
	}

    lex_test_skip(par, TK_SEMI);
	expr_return(par, e, 1);
}

static void l_scan_root(unused a_henv env, void* ctx) {
	FnScope scope;

	Parser* par = ctx;
    parser_start(par);

	fnscope_prologue(par, &scope, 1);
	if (unlikely(par->_options & ALO_COMP_OPT_EVAL)) {
		l_scan_root_return_stat(par);
	}
	else {
		l_scan_stats(par);
	}

	if (!lex_test(par, TK_EOF)) {
		lex_error_got(par, "statement expected");
	}

	fnscope_epilogue(par, par->_name, true, lex_line(par));
}

static void parser_init(a_henv env, a_ifun fun, void* ctx, GStr* file, GStr* name, a_u32 options, Parser* par) {
	*par = new(Parser) {
		._options = options,
		._file = file,
		._name = name
	};
	ai_lex_init(env, &par->_lex, fun, ctx);
	rq_init(&par->_rq);
}

a_msg ai_parse(a_henv env, a_ifun fun, void* ctx, GStr* file, GStr* name, a_u32 options, GFun** pfun) {
	Parser par;
    parser_init(env, fun, ctx, file, name, options, &par);

	a_msg msg = ai_env_pcall(env, l_scan_root, &par);

	if (msg == ALO_SOK) {
		*pfun = func_build(&par);
	}

	return msg;
}