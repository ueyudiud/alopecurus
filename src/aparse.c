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
BUF_STRUCT_DECLARE(CapInfoBuf, RichCapInfo);

struct Parser {
    GOBJ_STRUCT_HEADER;
    union {
        Lexer lex;
        a_henv env;
    };
    union {
        InsnBuf insns[1];
        struct {
            a_insn (*code)[1];
            a_usize head_label;
        };
    };
    a_u32 options;
    a_u8 scope_depth;
    SymBuf syms[1];
    ConstBuf consts[1]; /* Constants. */
    LocalInfoBuf locals[1];
    LineInfoBuf lines[1];
    Buf secs[1];
    Buf sbuf[1];
    GStr* name; /* For dbg_name. */
	GStr* gvar_name;
    Scope* scope;
    FnScope* fscope;
    RefQueue rq; /* Function prototype queue. */
};

#define lex(par) (&(par)->lex)

#define parse_error(par,fmt,args...) ai_lex_error(lex(par), fmt, args)

#define NO_LABEL (~u32c(0))
#define NIL_SEC_REF (~u32c(0))

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
	 ** REPR: idat
	 *@param _i the integer constant.
	 */
	EXPR_INT,
	/**
	 ** Float constant expression.
	 ** REPR: ndat
	 *@param _n the float constant.
	 */
	EXPR_FLOAT,
	/**
	 ** String constant expression.
	 ** REPR: sdat
	 *@param _s the string constant.
	 */
	EXPR_STR,
/*==========================Bind Expressions============================*/
	/**
	 ** The expression from a local variable.
	 ** REPR: R[udat1]
	 *@param _d1 the register index.
	 *@param _d2 the symbol index.
	 *@param _fval true if value needs drop.
	 *@param _fsym true if it is a named register.
	 */
	EXPR_REG,
	/**
	 ** The expression bind to a capture value.
	 ** REPR: C[udat1]
	 *@param _d1 the capture index.
	 *@param _d2 the symbol index.
	 *@param _fsym true if symbol exists.
	 */
	EXPR_CAP,
	/**
	 ** The reference of export symbol.
	 ** repr: _ENV[sdat]
	 *@param _d1 the const index of variable dbg_name.
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
	 ** The value indexed expression. REPR: R[udat1][R[udat2]]
	 *@param _d1 the base register index.
	 *@param _d2 the key register index.
	 *@param _f1 true if base needs drop.
	 *@param _f2 true if key needs drop.
	 */
	EXPR_REF,
	/**
	 ** The integer indexed expression.
	 ** REPR: R[udat1][udat2]
	 *@param _d1 the base register index.
	 *@param _d2 the integer key.
	 */
	EXPR_REFI,
	/**
	 ** The constant indexed expression. REPR: R[_impl][K[key]]
	 *@param _d1 the base register index.
	 *@param _d2 the key constant index.
	 */
	EXPR_REFK,
	EXPR_REFCK,
/*=========================Partial Expressions==========================*/
	/**
	 ** The partial evaluated expression.
	 ** The output is the register with index A of instruction.
	 ** REPR: R[udat1(a)]
	 *@param _d1 the label of instruction.
	 *@param _fupk true if expression can be unpacked.
	 */
	EXPR_DYN,
	/**
	 ** The partial evaluated expression.
	 ** The output is the register with index A of instruction.
	 ** REPR: R[udat1(a):udat1(a)+udat1(c)]
	 *@param _d1 the label of instruction.
	 *@param _fupk is always true.
	 */
	EXPR_VDYN,
	/**
	 ** The partial evaluated expression.
	 ** The output is the register with index A of instruction.
	 ** REPR: R[udat1(a)]
	 *@param _d1 the label of instruction.
	 *@param _fval true if value needs drop.
	 *@param _fupk is always true.
	 */
	EXPR_CALL,
	/**
	 ** The partial evaluated expression.
	 ** The output is the register with index A of instruction.
	 ** REPR: R[udat1(a)]
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
	 ** REPR: try { R[label(a)] } else { nil }
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
		a_int idat;
		a_float ndat;
		GStr* sdat;
		struct { /* Universal data */
			a_u32 udat1;
			a_u32 udat2;
		};
	};
	a_line line;
	a_u8 tag;
	union {
		a_u8 flags;
		struct {
			a_u8 fval: 1; /* Used for value drop mark. */
			a_u8 fkey: 1; /* Used for key drop mark. */
			a_u8 fsym: 1; /* Used for symbol mark or shared mark. */
			a_u8 fupk: 1; /* Used for unpack-able mark. */
			a_u8 fucf: 1; /* Used for unreachable control flow mark. */
		};
	};
};

typedef ExprDesc Expr[1];

static a_bool expr_has_multi_values(InExpr e) {
	a_enum k = e->tag;
	assume(k < EXPR__MAX, "invalid expression.");
	return ((1 << k) & (1 << EXPR_VCALL | 1 << EXPR_VDYN | 1 << EXPR_NTMP | 1 << EXPR_NTMPC | 1 << EXPR_VNTMP | 1 << EXPR_VNTMPC));
}

static a_bool expr_has_vararg_top(InExpr e) {
    a_enum k = e->tag;
	assume(k < EXPR__MAX, "invalid expression.");
	return ((1 << k) & (1 << EXPR_VCALL | 1 << EXPR_VDYN | 1 << EXPR_VNTMP | 1 << EXPR_VNTMPC));
}

static void expr_copy(OutExpr dst, InExpr src) {
	memcpy(dst, src, sizeof(Expr));
}

static void expr_unit(OutExpr e) {
    init(e) {
        .tag = EXPR_UNIT
    };
}

static void expr_tmp(OutExpr e, a_u32 reg, a_line line) {
    init(e) {
        .tag = EXPR_REG,
        .udat1 = reg,
        .fval = true,
        .fsym = false,
        .line = line
    };
}

static void expr_val(OutExpr e, a_u32 reg, a_line line) {
    init(e) {
        .tag = EXPR_REG,
        .udat1 = reg,
        .fval = false,
        .fsym = false,
        .line = line
    };
}

static void expr_dyn(OutExpr e, a_u32 label, a_line line) {
    init(e) {
        .tag = EXPR_DYN,
        .udat1 = label,
        .line = line
    };
}

struct ConExpr {
	Expr expr;
    a_usize off;
};

typedef struct Pat Pat;

struct Pat {
    Pat* parent;
	union {
        Pat* child;
        GStr* name;
    };
	Pat* sibling;
    a_usize sec_ref;
    Expr expr;

    a_line line;
    a_u8 nchild;
    a_u8 kind;
    union {
        a_u8 flags;
        struct {
            a_u8 fmut: 1;
            a_u8 fuse: 1;
            a_u8 fcpx: 1;
            a_u8 fdfl: 1;
        };
    };

    /* Used for register allocation. */
	a_u8 tmp_pos;
	a_u8 tmp_top;
	a_u8 abs_bot;
	a_u8 index; /* Index in enclosed pattern. */
};

struct PatInfo {
	Pat root;
	void (*con)(Parser*, Pat*, a_usize);
	a_usize ctx;
};

enum SymKind {
	/**
	 ** Local variable.
	 *@param _index the register index.
	 */
	SYM_LOCAL,
	/**
	 ** The top capture value.
	 *@param _index the capture index in top as_scope.
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

enum SymStatus {
    SYM_STATUS_AVAILABLE,
    SYM_STATUS_UNINIT,
    SYM_STATUS_BORROWED,
    SYM_STATUS_MOVED,
    SYM_STATUS_DELETED
};

typedef union {
    a_u8 _;
    struct {
        a_u8 mmut: 1; /* Mark a variable is mutable. */
        a_u8 muse: 1; /* Mark a variable need drop after leave scope. */
    };
} SymMods;

/**
 ** Storage compile-time metadata of named symbol in chunk.
 */
struct Sym {
	a_u8 tag; /* The tag of symbol kind. */
	a_u8 scope; /* The as_scope of symbol belongs to. */
    a_u8 _status; /* The status of symbol. */
    SymMods mods; /* The modifiers of symbol. */
	a_u32 index; /* Variant uses for different symbol tag. */
	GStr* name; /* The symbol dbg_name. */
};

#define JMP_PROP_BREAK 0x01
#define JMP_PROP_CONTINUE 0x02
#define JMP_PROP_BOUND 0x04

#define SCOPE_STRUCT_HEAD \
    Scope* upscope;       \
    GStr* label_name;    \
	a_u8 bot_reg; /* The bottom of as_scope. */ \
	a_u8 top_ntr; /* Top of non-temporary section. */ \
	a_u8 bot_fur; /* Bottom of fragmented section. */ \
	a_u8 num_fur; /* Number of temporary register in fragmented section. */ \
	a_u8 top_reg; /* The top of as_scope. */  \
    a_u8 jmp_prop; /* The jump properties. */      \
	a_line begin_line;      \
	a_u32 begin_label;      \
	a_u32 end_label;        \
	a_u32 sym_off

struct Scope {
	SCOPE_STRUCT_HEAD;
};

struct RichCapInfo {
	a_u8 scope; /* The depth of first captured as_scope. */
	a_u8 src_index;
	GStr* name;
};

struct FnScope {
	union {
		Scope as_scope;
		struct {
			SCOPE_STRUCT_HEAD;
		};
	};
	FnScope* fupscope;
	GProto** base_subs;
    CapInfoBuf caps[1];

    a_u32 const_off;
	a_u32 line_off;
	a_u32 local_off;

	a_u32 head_jump;
	a_u32 head_land;

    a_line head_jump_line;
	a_line head_line;
	a_line close_line;

    a_u16 nsub;
	union {
		a_u8 flags;
		struct {
			a_u8 fpass: 1;
			a_u8 fland: 1;
			a_u8 fjump: 1;
			a_u8 fclose: 1;
		};
	};
	a_u8 nparam;
	a_u8 max_reg;
};

typedef struct {
    a_u32 ninsn;
    a_u32 nline;
    a_u32 rel_label;
    a_u8 rel_reg_bot;
    a_u8 rel_reg_top;
    a_insn code[];
} SecHead;

struct SecRecDesc {
    a_u32 line_off;
    a_u32 head_label;
    a_u32 head_jump;
    a_u32 head_land;
    a_line head_line;
    a_line head_jump_line;
    a_line close_line;
    a_u8 reg_base;
    a_u8 max_reg;
    a_u8 flags;
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

/**
 ** Get the index for constant, the value is distincted by trivial equality.
 *@return the index of constant in constant pool.
 */
static a_u32 const_index(Parser* par, Value val) {
	ConstBuf* consts = par->consts;
	a_u32 off = par->fscope->const_off;
	for (a_u32 i = par->fscope->const_off; i < consts->len; ++i) {
		/*
		 * Since all literals which have the same format provided by compiler should
		 * have same binary data, use identity equality for comparison.
		 */
		if (v_trivial_equals_unchecked(consts->ptr[i], val)) {
			return i - off;
		}
	}

	if (unlikely(par->fscope->const_off + consts->len == BC_MAX_BX + 1)) {
		parse_error(par, "too many constants.", 0);
	}

	return at_buf_push(par->env, consts, val, "constant") - off;
}

static Value const_at(Parser* par, a_u32 index) {
	return par->consts->ptr[par->fscope->const_off + index];
}

static a_bool const_is_str(Parser* par, a_u32 index) {
	return v_is_str(const_at(par, index));
}

#define INSN_NOP ((a_insn) 0)

static a_insn* code_at(Parser* par, a_u32 label) {
    return par->code[label];
}

static a_u32 code_put(Parser* par, a_insn i) {
    return at_buf_push(par->env, par->insns, i, "code");
}

static void l_emit_line(Parser* par, a_line line) {
	FnScope* scope = par->fscope;
	if (scope->head_line != line) {
		LineInfo info = {UINT32_MAX, line};
		a_u32 index = at_buf_push(par->env, par->lines, info, "line");
		if (index > scope->line_off) { /* Settle end label for last line info. */
			par->lines->ptr[index - 1].lend = par->head_label;
		}
		scope->head_line = line;
	}
}

static a_u32 l_emit_direct(Parser* restrict par, a_insn i, a_u32 line) {
	l_emit_line(par, line);
    return code_put(par, i);
}

static a_bool l_should_emit(Parser* par) {
	return likely(par->fscope->fpass || par->fscope->fland);
}

static a_i32 l_jump_diff(Parser* par, a_u32 from, a_u32 to, a_line line) {
    a_i32 diff = cast(a_i32, to - from - 1);
    if (unlikely(diff < BC_MIN_SAX || diff > BC_MAX_SAX)) {
        parse_error(par, "instruction jump out of bound.", line);
    }
    return diff;
}

static a_u32 l_emit_jump_direct(Parser* par, a_u32 label, a_line line) {
	a_i32 d = label != NO_LABEL ? l_jump_diff(par, par->head_label, label, line) : -1;
	return l_emit_direct(par, bc_make_isax(BC_J, d), line);
}

static a_u32 l_next_jump(Parser* par, a_u32 label) {
	assume(label <= par->head_label, "not valid label.");
	if (label == par->head_label)
		return par->fscope->head_land;

	a_insn* ip = code_at(par, label);
	assume(bc_load_op(ip) == BC_J);

	a_i32 disp = bc_load_sax(ip);
	return disp != -1 ? label + 1 + cast(a_u32, disp) : NO_LABEL;
}

static void l_redirect(Parser* par, a_u32 from, a_u32 to, a_line line) {
	a_insn* ip = par->code[from];

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
	par->fscope->fclose = false;
}

static void l_clear_jump(Parser* par) {
	par->fscope->fjump = false;
	par->fscope->head_jump = NO_LABEL;
}

static void l_clear_land(Parser* par) {
	par->fscope->fland = false;
	par->fscope->head_land = NO_LABEL;
}

static void l_emit_fast(Parser* par, a_insn i, a_line line) {
	if (par->fscope->fpass) {
		l_emit_direct(par, i, line);
	}
}

static void l_flush_close(Parser* par) {
	if (par->fscope->fclose) {
		l_emit_direct(par, bc_make_ia(BC_CLOSE, par->scope->top_ntr), par->fscope->close_line);
		l_clear_close(par);
	}
}

static void l_flush_jump(Parser *par) {
	if (par->fscope->fjump) {
		assume(par->fscope->fland); /* When jump is defined, the branch is reachable only if land is also defined. */
		/* Link to previous jump instruction. */
		l_emit_jump_direct(par, par->fscope->head_jump, par->fscope->head_jump_line);
		l_clear_jump(par);
	}
}

static void l_flush_land(Parser* par, a_line line) {
	if (par->fscope->fland) {
		l_redirect_chain(par, par->fscope->head_land, par->head_label, line);
		l_clear_land(par);
	}
}

static void l_redirect_leave(Parser* par, a_u32 label, a_insn i) {
	loop {
		a_u32 next = l_next_jump(par, label);
		bc_store(par->code[label], i);
		if (next == NO_LABEL)
			break;
		label = next;
	}
}

/**
 ** Emit an instruction to leave current function.
 */
static void l_emit_leave(Parser* par, a_insn i, a_line line) {
	if (l_should_emit(par)) {
		FnScope* scope = par->fscope;

		l_flush_close(par);
        l_flush_jump(par);
		if (scope->fland) {
			l_redirect_leave(par, scope->head_land, i);
			l_clear_land(par);
		}
		if (scope->fpass) {
			l_emit_direct(par, i, line);
		}
		scope->fpass = false;
	}
}

static a_u32 l_emit(Parser* par, a_insn i, a_line line) {
	if (l_should_emit(par)) {
		l_flush_close(par);
        l_flush_jump(par);
		l_flush_land(par, line);
		par->fscope->fpass = true;
		return l_emit_direct(par, i, line);
	}
	return NO_LABEL;
}

#define l_emit_ia(par,i,a,l) l_emit(par, bc_make_ia(i, a), l)
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
	e->fupk = true; /* Mark unpackable. */
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
	if (par->fscope->fpass && !par->fscope->fland) {
		a_insn* ip = par->code[par->head_label - 1];
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
				return par->head_label - 1;
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
	if (c <= BC_MAX_C) {
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
	else if (label1 != label2) {
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
    init(e) {
        .tag = val,
        .line = line
    };
}

static void expr_int(OutExpr e, a_int val, a_line line) {
	init(e) {
        .tag = EXPR_INT,
        .idat = val,
        .line = line
    };
}

static void expr_float(OutExpr e, a_float val, a_line line) {
    init(e) {
        .tag = EXPR_FLOAT,
        .ndat = val,
        .line = line
    };
}

static void expr_str(OutExpr e, GStr* val, a_line line) {
    init(e) {
        .tag = EXPR_STR,
        .sdat = val,
        .line = line
    };
}

static void expr_func(Parser* par, OutExpr e, GProto* fun) {
	FnScope* scope = par->fscope;
	a_u16 index = scope->nsub ++;
	l_emit_idbx(par, BC_LDF, e, index, fun->dbg_lndef);
}

static a_u32 stack_alloc_succ(Parser* par, a_u32 num, a_u32 line) {
	Scope* scope = par->scope;
	a_u32 reg = scope->top_reg;
	scope->top_reg += num;
	if (scope->top_reg > par->fscope->max_reg) {
		par->fscope->max_reg = scope->top_reg;
		if (reg > BC_MAX_A) {
			parse_error(par, "too many register used.", line);
		}
	}
	return reg;
}

static a_u32 stack_alloc(Parser* par, a_u32 line) {
	return stack_alloc_succ(par, 1, line);
}

static a_bool l_is_in_tmp(Parser* par, a_u32 reg) {
	return reg >= par->scope->top_ntr;
}

static void stack_check_free_used(Scope* scope) {
	/* Remove critical section if all registers in the section are fully freed. */
	if (unlikely(scope->num_fur > 0) && scope->bot_fur + scope->num_fur == scope->top_reg) {
		scope->top_reg = scope->bot_fur;
		scope->num_fur = 0;
	}
}

/**
 ** Reallocate the register which is already freed, used for phi operation.
 *@param par the parser.
 *@param reg the register to allocate.
 */
static void stack_realloc(Parser* par, a_u32 reg) {
	Scope* scope = par->scope;
	assume(l_is_in_tmp(par, reg), "cannot reallocate a using register twice.");
	if (scope->top_reg == reg) {
		scope->top_reg += 1;
		assume(scope->top_reg <= par->fscope->max_reg);
	}
	else {
		if (reg == scope->bot_fur) {
			scope->bot_fur += 1;
		}
		scope->num_fur -= 1;
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
	Scope* scope = par->scope;
	assume(reg < scope->top_reg && l_is_in_tmp(par, reg));
	/* The registers are likely freed with the reversed order of allocation. */
	if (reg + 1 == scope->top_reg) {
		scope->top_reg = reg;
	}
	else {
		/* Or, mark register position to top of stack into critical section. */
		if (scope->num_fur == 0 || reg < scope->bot_fur) {
			scope->bot_fur = reg;
		}
		scope->num_fur += 1;
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
	Scope* scope = par->scope;
	assume(l_is_in_tmp(par, reg));

	scope->top_reg = reg;
	stack_check_free_used(scope);
}

/**
 ** Store temporary value in register to a variable in place, the register will
 ** be dropped until leave the as_scope.
 *@param par the parser.
 *@param reg the register to store.
 */
static void stack_store(Parser* par, a_u32 reg) {
	Scope* scope = par->scope;
	assume(reg >= scope->bot_fur && reg < scope->top_reg, "cannot store register in place.");
	scope->top_ntr = max(reg + 1, scope->top_ntr);
}

/**
 ** Drop ownership (without value) for expression.
 *@param par the parser.
 *@param e the expression.
 */
static void expr_drop(Parser* par, InExpr e) {
	if (e->fkey) {
		stack_free(par, e->udat2);
		e->fkey = false;
	}
	if (e->fval) {
		if (!expr_has_multi_values(e)) {
			stack_free(par, e->udat1);
		}
		else {
			stack_free_succ(par, e->udat1);
		}
		e->fval = false;
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
static a_u32 l_lookup_symbol(Parser* par, GStr* name);

static void l_capture_locally(Parser* par, FnScope* scope, Sym* sym, RichCapInfo* info) {
	quiet(scope);

	switch (sym->tag) {
		case SYM_LOCAL: {
			info->src_index = par->locals->ptr[sym->index].reg; /* Get variable index. */
			break;
		}
		case SYM_CAPTURE: {
			info->src_index = sym->index;
			break;
		}
		default: unreachable();
	}
}

static a_u32 l_lookup_capture_internal(Parser* par, FnScope* fscope, Sym* sym, a_u32 depth) {
	/* Find in captured values. */
	for (a_u32 i = 0; i < fscope->caps->len; ++i) {
		RichCapInfo* info = &fscope->caps->ptr[i];
		if (info->name == sym->name) {
			/* Already captured. */
			return i;
		}
	}

	/* Not found, create a new capture value. */
	RichCapInfo info = {
		.scope = sym->scope,
		.name = sym->name
	};
	if (sym->scope >= depth - 1) {
		l_capture_locally(par, fscope, sym, &info);
	}
	else { /* Capture recursively. */
		info.src_index = l_lookup_capture_internal(par, fscope->fupscope, sym, depth - 1);
	}
	return at_buf_push(par->env, fscope->caps, info, "capture");
}

static a_u32 l_lookup_capture(Parser* par, Sym* sym) {
	return l_lookup_capture_internal(par, par->fscope, sym, par->scope_depth);
}

#define nosym UINT32_MAX

static void expr_env(Parser* par, OutExpr e) {
	a_u32 id = l_lookup_symbol(par, par->gvar_name);
    assume(id != nosym, "variable '_ENV' not defined.");
    expr_resolve(par, e, id);
}

static void expr_gvar(Parser* par, InoutExpr e, a_line line) {
	a_u32 id = e->udat1;
	assume(e->tag == EXPR_GBL, "not global variable.");
	expr_env(par, e);
	if (e->tag == EXPR_CAP) {
		e->tag = EXPR_REFCK;
		e->udat2 = id;
	}
	else {
		expr_to_reg(par, e);
		e->tag = EXPR_REFK;
		e->udat2 = id;
	}
    e->line = line;
}

static void expr_resolve(Parser* par, OutExpr e, a_u32 id) {
	Sym* sym = &par->syms->ptr[id];
	switch (sym->tag) {
        case SYM_LOCAL: {
            if (par->scope_depth == sym->scope) {
                init(e) {
                    .tag = EXPR_REG,
                    .udat1 = par->locals->ptr[sym->index].reg,
                    .udat2 = id,
                    .fsym = true
                };
            }
            else {
                init(e) {
                    .tag = EXPR_CAP,
                    .udat1 = l_lookup_capture(par, sym),
                    .udat2 = id,
                    .fsym = true
                };
            }
            break;
        }
        case SYM_CAPTURE: {
            init(e) {
                .tag = EXPR_CAP,
                .udat1 = l_lookup_capture(par, sym),
                .udat2 = id,
                .fsym = true
            };
            break;
        }
        case SYM_EXPORT: {
            init(e) {
                .udat1 = const_index(par, v_of_str(sym->name)),
                .udat2 = id,
                .fsym = true
            };
            break;
        }
		default: unreachable();
	}
}

static a_u32 l_lookup_symbol(Parser* par, GStr* name) {
	SymBuf* syms = par->syms;
	for (a_u32 i = syms->len; i > 0; --i) {
        a_u32 id = i - 1;
		Sym* sym = &syms->ptr[id];
		if (sym->name == name) {
            assume(id != nosym);
			return id;
		}
	}
	return nosym;
}

/**
 ** Lookup symbol in global as_scope.
 *@param par the parser.
 *@param e the expression for output.
 *@param name the lookup dbg_name.
 *@param line the line number of dbg_name reference.
 */
static void expr_symbol(Parser* par, OutExpr e, GStr* name, a_line line) {
	a_u32 id = l_lookup_symbol(par, name);
	if (id != nosym) {
		expr_resolve(par, e, id);
        e->line = line;
	}
	else {
        init(e) {
            .tag = EXPR_GBL,
            .udat1 = const_index(par, v_of_str(name)),
            .fsym = false,
            .line = line
        };
	}
}

static void expr_index_str(Parser* par, InoutExpr e, GStr* name, a_line line) {
	a_u32 index = const_index(par, v_of_str(name));
	if (e->tag == EXPR_CAP) {
		e->tag = EXPR_REFCK;
		e->udat2 = index;
	}
	else {
        expr_to_reg(par, e);
		e->tag = EXPR_REFK;
		e->udat2 = index;
	}
	e->line = line;
}

static void expr_lookup(Parser* par, InoutExpr e, GStr* name, a_line line) {
    expr_to_reg(par, e);
	expr_drop(par, e);

	a_u32 reg = stack_alloc_succ(par, 2, line);

    l_emit_aby(par, BC_LOOK, reg, e->udat1, const_index(par, v_of_str(name)), line);

    init(e) {
        .tag = EXPR_NTMP,
        .udat1 = reg,
        .fval = true,
        .line = line
    };
}

/**
 ** Make reference of indexed expression.
 *@param par the parser.
 *@param ev the view expression.
 *@param ek the key expression.
 *@param line the line of operation.
 */
static void expr_index(Parser* par, InoutExpr ev, InExpr ek, a_line line) {
	switch (ek->tag) {
		case EXPR_INT: {
            expr_to_reg(par, ev);
			a_int val = ek->idat;
			if (val >= 0 && val <= BC_MAX_C) {
				ev->tag = EXPR_REFI;
				ev->udat2 = cast(a_u32, val);
			}
			else {
				ev->tag = EXPR_REFK;
				ev->udat2 = const_index(par, v_of_int(val));
			}
			ev->line = line;
			break;
		}
		case EXPR_STR: {
			expr_index_str(par, ev, ek->sdat, line);
			break;
		}
		default: {
			/* Handle by normal expression. */
            expr_to_reg(par, ev);
            expr_to_reg(par, ek);
			ev->tag = EXPR_REF;
			ev->udat2 = ek->udat1;
			ev->fkey = ek->fval;
			ev->line = line;
			break;
		}
	}
}

/**
 ** Invert the branch at [label-1] and merge residual branch to *plabel.
 */
static void branch_negate(Parser* par, a_u32* plabel, a_u32 label, a_line line) {
	if (label + 1 == par->head_label) {
		/* Try to swap duality opcodes for */
		a_insn* ip = code_at(par, label - 1);

		if (insn_is_branch(ip)) {
			a_u32 op = bc_load_op(ip);
            bc_store_op(ip, op ^ 1);

			a_u32 label1 = l_next_jump(par, label);
			a_u32 label2 = par->fscope->fland ? par->fscope->head_land : NO_LABEL;

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
	assume(e->tag == EXPR_TRUE_OR_FALSE || e->tag == EXPR_FALSE_OR_TRUE);
	assume(par->head_label == e->udat2 + 1);
	assume(insn_is_branch(par->code[e->udat2 - 1]));

	a_u32 label = l_next_jump(par, e->udat2);
	if (label != NO_LABEL) {
		l_redirect_chain(par, label, e->udat2, e->line);
	}

	/* Swap last instruction. */
    bc_store(par->code[par->head_label - 1], bc_make_ia(e->tag == EXPR_TRUE_OR_FALSE ? BC_BKF : BC_BKT, reg));
	l_emit_ia(par, e->tag == EXPR_TRUE_OR_FALSE ? BC_KT : BC_KF, reg, e->line);
}

static void expr_new_table(Parser* par, InoutExpr e, a_line line) {
	l_emit_idbx(par, BC_HNEW, e, 0, line);
}

static void expr_neg(Parser* par, InoutExpr e, a_line line) {
	switch (e->tag) {
		case EXPR_INT: {
			e->idat = ai_op_neg_int(e->idat);
			e->line = line;
			break;
		}
		case EXPR_FLOAT: {
			e->ndat = ai_op_neg_float(e->ndat);
			e->line = line;
			break;
		}
		default: {
            expr_to_reg(par, e);
            expr_drop(par, e);
            l_emit_idb(par, BC_NEG, e, e->udat1, line);
			break;
		}
	}
}

static void expr_bit_inv(Parser* par, InoutExpr e, a_line line) {
	switch (e->tag) {
		case EXPR_INT: {
			e->idat = ai_op_bnot_int(e->idat);
			e->line = line;
			break;
		}
		default: {
            expr_to_reg(par, e);
            expr_drop(par, e);
            l_emit_idb(par, BC_NEG, e, e->udat1, line);
			break;
		}
	}
}

static void expr_not(Parser* par, InoutExpr e, a_line line) {
	switch (e->tag) {
		case EXPR_NIL:
		case EXPR_FALSE: {
			e->tag = EXPR_TRUE;
			e->line = line;
			break;
		}
		case EXPR_TRUE:
		case EXPR_INT:
		case EXPR_FLOAT:
		case EXPR_STR: {
			e->tag = EXPR_FALSE;
			e->line = line;
			break;
		}
		case EXPR_RESIDUAL_FALSE: {
			e->tag = EXPR_RESIDUAL_TRUE;
			e->line = line;
			break;
		}
		case EXPR_RESIDUAL_TRUE: {
			e->tag = EXPR_RESIDUAL_FALSE;
			e->line = line;
			break;
		}
		case EXPR_TRUE_OR_FALSE: {
			e->tag = EXPR_FALSE_OR_TRUE;
			e->line = line;
			break;
		}
		case EXPR_FALSE_OR_TRUE: {
			e->tag = EXPR_TRUE_OR_FALSE;
			e->line = line;
			break;
		}
		default: {
            expr_to_reg(par, e);
            expr_drop(par, e);

            a_u32 label = l_emit_branch(par, bc_make_ia(BC_BNZ, e->udat1), NO_LABEL, line);

            init(e) {
                .tag = EXPR_TRUE_OR_FALSE,
                .udat2 = label,
                .line = line
            };
			break;
		}
	}
}

static void expr_unbox(Parser* par, InoutExpr e, a_line line) {
    expr_to_reg(par, e);
    expr_drop(par, e);

    l_emit_idbd(par, BC_UNBOX, e, e->udat1, line);
}

static void expr_len(Parser* par, InoutExpr e, a_line line) {
    expr_to_reg(par, e);
    expr_drop(par, e);

    l_emit_idb(par, BC_LEN, e, e->udat1, line);
}

static void expr_iter(Parser* par, InoutExpr e, a_line line) {
	Scope* scope = par->scope;

	expr_to_reg(par, e);
	expr_drop(par, e);

	a_u32 reg1 = e->udat1;

	assume(scope->top_ntr == scope->top_reg);

	a_u32 reg2 = stack_alloc_succ(par, 3, line);

    init(e) {
        .tag = EXPR_NTMP,
        .udat1 = reg2,
        .fval = true
    };

	l_emit_iab(par, BC_ITER, reg2, reg1, line);

	scope->top_ntr = scope->top_reg;
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
			e->tag = kind;
			e->udat2 = label;
			break;
		}
		case OP_OR: {
			a_u32 label = NO_LABEL;
			a_u32 kind = expr_test_false(par, e, &label, line);
			expr_discard(par, e);
			e->tag = kind;
			e->udat2 = label;
			break;
		}
		default: unreachable();
	}
}

static a_bool expr_are_ints(InExpr e1, a_int* i1, InExpr e2, a_int* i2) {
	if (e1->tag == EXPR_INT && e2->tag == EXPR_INT) {
		*i1 = e1->idat;
		*i2 = e2->idat;
		return true;
	}
	return false;
}

static a_bool expr_are_floats(InExpr e1, a_float* i1, InExpr e2, a_float* i2) {
	if (e1->tag == EXPR_INT) {
		*i1 = cast(a_float, e1->idat);
	}
	else if (e1->tag == EXPR_FLOAT) {
		*i1 = e1->ndat;
	}
	else return false;
	if (e2->tag == EXPR_INT) {
		*i2 = cast(a_float, e2->idat);
	}
	else if (e2->tag == EXPR_FLOAT) {
		*i2 = e2->ndat;
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
			e->tag = EXPR_INT;
			e->idat = ai_op_bin_int(a, b, op);
			break;
		}
		case OP_DIV:
		case OP_MOD: {
			if (unlikely(b == 0)) {
				parse_error(par, "attempt to divide by 0.", line);
			}
			e->tag = EXPR_INT;
			e->idat = ai_op_bin_int(a, b, op);
			break;
		}
		case OP_EQ:
		case OP_NE:
		case OP_LT:
		case OP_LE:
		case OP_GT:
		case OP_GE: {
			e->tag = ai_op_cmp_int(a, b, op) ? EXPR_TRUE : EXPR_FALSE;
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
			e->tag = EXPR_FLOAT;
			e->ndat = ai_op_bin_float(a, b, op);
			break;
		}
		case OP_EQ:
		case OP_NE:
		case OP_LT:
		case OP_LE:
		case OP_GT:
		case OP_GE: {
			e->tag = ai_op_cmp_float(a, b, op) ? EXPR_TRUE : EXPR_FALSE;
			break;
		}
		default: unreachable();
	}
}

static a_bool l_fold_const_int(Parser* par, InoutExpr e1, InExpr e2, a_u32 op, a_line line) {
	a_int i1, i2;
	if (expr_are_ints(e1, &i1, e2, &i2)) {
		l_compute_int(par, i1, i2, e1, op, line);
		e1->line = line;
		return true;
	}
	return false;
}

static a_bool l_fold_const_float(Parser* par, InoutExpr e1, InExpr e2, a_u32 op, a_line line) {
	a_float f1, f2;
	if (expr_are_floats(e1, &f1, e2, &f2)) {
		l_compute_float(par->env, f1, f2, e1, op);
		e1->line = line;
		return true;
	}
	return false;
}

static a_bool l_compare_const(Parser* par, InExpr e1, InExpr e2, a_u32 bc1, a_u32 bc2, a_line line) {
	if (e2->tag == EXPR_INT && e2->idat >= BC_MIN_SBX && e2->idat <= BC_MAX_SBX) {
        expr_to_reg(par, e1);
        expr_drop(par, e1);
        l_emit_iasbx(par, bc1, e1->udat1, e2->idat, line);
		return false;
	}
	else if (e1->tag == EXPR_INT && e1->idat >= BC_MIN_SBX && e1->idat <= BC_MAX_SBX) {
        expr_to_reg(par, e2);
        expr_drop(par, e2);
        l_emit_iasbx(par, bc2, e2->udat1, e1->idat, line);
		return false;
	}

	return true;
}

static void l_emit_bin(Parser* par, InoutExpr e1, InExpr e2, a_u32 bc, a_line line) {
    expr_to_reg(par, e2);
    expr_to_reg(par, e1);
    expr_drop(par, e1);
    expr_drop(par, e2);
    l_emit_idbc(par, bc, e1, e1->udat1, e2->udat1, line);
}

static void l_compare(Parser* par, InoutExpr e1, InExpr e2, a_u32 bc, a_line line) {
    expr_to_reg(par, e2);
    expr_to_reg(par, e1);
    expr_drop(par, e1);
    expr_drop(par, e2);
    l_emit_iab(par, bc, e1->udat1, e2->udat1, line);
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
			if (e2->tag == EXPR_INT && e2->idat >= BC_MIN_SC && e2->tag <= BC_MAX_SC) {
                expr_to_reg(par, e1);
                expr_drop(par, e1);
                l_emit_idbsc(par, BC_ADDI + op - OP_ADD, e1, e1->udat1, e2->idat, line);
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
			if (e2->tag == EXPR_INT && e2->idat >= BC_MIN_SC && e2->tag <= BC_MAX_SC) {
                expr_to_reg(par, e1);
                expr_drop(par, e1);
                l_emit_idbsc(par, BC_SHLI + op - OP_SHL, e1, e1->udat1, e2->idat, line);
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
            init(e1) {
                .tag = EXPR_TRUE_OR_FALSE,
                .udat2 = l_emit_jump_direct(par, NO_LABEL, line),
                .line = line
            };
			break;
		}
		case OP_AND: {
			switch (e1->tag) {
				case EXPR_RESIDUAL_FALSE: {
					break;
				}
				case EXPR_TRUE: {
					e1->udat2 = NO_LABEL;
					fallthrough;
				}
				case EXPR_TRUE_OR_FALSE: {
					e1->tag = expr_test_true(par, e2, &e1->udat2, line);
					expr_discard(par, e2);
					break;
				}
				default: unreachable();
			}
			break;
		}
		case OP_OR: {
			switch (e1->tag) {
				case EXPR_RESIDUAL_TRUE: {
					break;
				}
				case EXPR_FALSE: {
					e1->udat2 = NO_LABEL;
					fallthrough;
				}
				case EXPR_FALSE_OR_TRUE: {
					e1->tag = expr_test_false(par, e2, &e1->udat2, line);
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
	if (e2->fucf) {
		l_mark_label(par, label, line);
		return;
	}
	else if (e1->fucf) {
		assume(label == NO_LABEL);
		expr_copy(e1, e2);
		return;
	}
	switch (e1->tag) {
		case EXPR_DYN_OR_NIL: {
			a_u32 label2 = expr_catch_nil_branch(par, e2, line);
			l_merge_branch(par, &e1->udat2, label2, line);

            expr_to_tmp(par, e2);

			a_u32 reg = e2->udat1;
			l_merge_optR(par, e1->udat2, reg, line);

			l_mark_label(par, label, line);
			stack_realloc(par, reg);
			expr_tmp(e1, reg, line);
			break;
		}
		case EXPR_REG_OR_NIL: {
			a_u32 label2 = expr_catch_nil_branch(par, e2, line);
			l_merge_branch(par, &e1->udat2, label2, line);

			a_u32 reg = e1->udat1;
            expr_pin_reg(par, e2, reg);
			l_merge_optR(par, e1->udat2, reg, line);

			l_mark_label(par, label, line);
			stack_realloc(par, reg);
			expr_tmp(e1, reg, line);
			break;
		}
		case EXPR_REG: {
			a_u32 reg = e1->udat1;
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
	if (!e->fucf) {
		switch (e->tag) {
			case EXPR_REG_OR_NIL: {
				if (e->fsym) {
					e->tag = EXPR_REG;
					a_u32 label2 = e->udat2;
					expr_to_dyn(par, e);
					e->tag = EXPR_DYN_OR_NIL;
					e->udat2 = label2;
				}
				else if (e->fval) {
					stack_free(par, e->udat1);
				}
				break;
			}
			case EXPR_REG: {
				if (e->fsym) {
					expr_to_dyn(par, e);
				}
				else if (e->fval) {
					/* Free stack for another branch used. */
					/* The ownership will still keep. */
					stack_free(par, e->udat1);
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
	if (e->fucf) {
		l_lazy_jump(par, label, line);
		l_mark_label(par, label, line);
		expr_const(e, EXPR_NIL, e->line);
		return;
	}
	switch (e->tag) {
		case EXPR_REG: {
			e->tag = EXPR_REG_OR_NIL;
			e->udat2 = label;
			break;
		}
		default: {
            expr_to_dyn(par, e);
			fallthrough;
		}
		case EXPR_DYN: {
			e->tag = EXPR_DYN_OR_NIL;
			e->udat2 = label;
			break;
		}
		case EXPR_REG_OR_NIL:
		case EXPR_DYN_OR_NIL: {
			l_merge_branch(par, &e->udat2, label, line);
			break;
		}
	}
	e->line = line;
}

static void expr_or_ret(Parser* par, InoutExpr e, a_line line) {
	switch (e->tag) {
		case EXPR_NIL: {
			l_emit_leave(par, bc_make_i(BC_RET0), line);
			e->fucf = true;
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
			l_redirect_leave(par, e->udat2, bc_make_i(BC_RET0));
			l_emit(par, bc_make_ia(BC_BNN, e->udat1), line);
			l_emit_fast(par, bc_make_i(BC_RET0), line);

			e->tag = EXPR_REG;
			break;
		}
		case EXPR_DYN_OR_NIL: {
			l_redirect_leave(par, e->udat2, bc_make_i(BC_RET0));

			e->tag = EXPR_DYN;
			fallthrough;
		}
		case EXPR_DYN: {
			if (e->fupk) {
				a_insn* ip = code_at(par, e->udat1);

				a_u32 reg = par->scope->top_reg;
				bc_store_op(ip, bc_load_op(ip) + 1);
				bc_store_a(ip, reg);
				bc_store_c(ip, DMB);

				l_emit(par, bc_make_ia(BC_BNN, reg), line);
				l_emit_fast(par, bc_make_ia(BC_RETM, reg), line);

                init(e) {
                    .tag = EXPR_REG,
                    .udat1 = reg,
                    .line = line
                };
			}
			else {
				expr_to_reg(par, e);

				l_emit(par, bc_make_ia(BC_BNN, e->udat1), line);
				l_emit_fast(par, bc_make_i(BC_RET0), line);
			}
			break;
		}
		case EXPR_CALL: {
			a_insn* ip = code_at(par, e->udat1);

			a_u32 reg = bc_load_a(ip);
			bc_store_op(ip, bc_load_op(ip) + 1);
			bc_store_c(ip, DMB);

			l_emit(par, bc_make_ia(BC_BNN, reg), line);
			l_emit_fast(par, bc_make_ia(BC_RETM, reg), line);

            init(e) {
                .tag = EXPR_REGS,
                .udat1 = reg,
                .fupk = true,
                .line = line
            };
			break;
		}
		case EXPR_REGS: {
			a_u32 reg = e->udat1;

			l_emit(par, bc_make_ia(BC_BNN, reg), line);
			l_emit_fast(par, bc_make_ia(BC_RETM, reg), line);
			break;
		}
		default: {
			expr_to_reg(par, e);

			l_emit(par, bc_make_ia(BC_BNN, e->udat1), line);
			l_emit_fast(par, bc_make_i(BC_RET0), line);
			break;
		}
	}
}

static void exprs_push_left(Parser *par, InoutExpr es) {
    exprs_fix(par, es);
}

static void exprs_push(Parser *par, InoutExpr es, InExpr e) {
	if (es->tag == EXPR_UNIT) {
		expr_copy(es, e);
		return;
	}
	else if (es->tag != EXPR_NTMP && es->tag != EXPR_NTMPC) {
        exprs_fix(par, es);
		assume(es->tag == EXPR_NTMP || es->tag == EXPR_NTMPC, "bad expression for push.");
	}

	es->fucf |= e->fucf;

	switch (e->tag) {
		case EXPR_UNIT: {
			break;
		}
		case EXPR_VCALL: {
			a_insn* ip = par->code[e->udat1];
			assume(bc_load_a(ip) == par->scope->top_reg, "can not place variable.");

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_c(ip, DMB);

            es->tag = EXPR_VNTMP;
			break;
		}
		case EXPR_VDYN: {
			a_insn* ip = par->code[e->udat1];

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_a(ip, par->scope->top_reg);
            bc_store_c(ip, DMB);

            es->tag = EXPR_VNTMP;
			break;
		}
		default: {
            expr_to_top_tmp(par, e);
			break;
		}
	}
}

static void exprs_pop(Parser* par, InoutExpr es, InExpr e, a_line line) {
	if (es->tag == EXPR_NTMP) {
		expr_tmp(e, par->scope->top_reg - 1, line);
	}
	else {
		assume(es->tag != EXPR_NTMPC);
		expr_copy(e, es);
		expr_unit(es);
	}
}

static void expr_box_tuple(Parser* par, InoutExpr e, a_line line) {
    exprs_to_top_tmps(par, e);

    a_u32 reg = e->udat1;

	if (expr_has_vararg_top(e)) {
		l_emit_idb(par, BC_TNEWM, e, reg, line);
	}
	else {
		l_emit_idbc(par, BC_TNEW, e, reg, par->scope->top_reg - reg, line);
	}

    stack_free_succ(par, reg);
}

static void expr_box_list(Parser* par, InoutExpr e, a_line line) {
    exprs_fix(par, e);
	switch (e->tag) {
		case EXPR_UNIT: {
			l_emit_idbx(par, BC_LNEW, e, 0, line);
			break;
		}
		case EXPR_NTMP: {
			a_u32 n = par->scope->top_reg - e->udat1;

            expr_drop(par, e);

            l_emit_idbc(par, BC_LBOX, e, e->udat1, n, line);
			break;
		}
		case EXPR_NTMPC: {
			a_u32 n = par->scope->top_reg - e->udat1 - 1;
			if (n > 0) {
				l_emit_iabc(par, BC_LPUSH, e->udat1, e->udat1 + 1, n, line);
				stack_free_succ(par, e->udat1 + 1);
			}
			expr_tmp(e, e->udat1, line);
			break;
		}
		default: unreachable();
	}
}

static void expr_call(Parser* par, InoutExpr es, a_line line) {
    exprs_to_top_tmps(par, es);

	a_u32 label;
	if (expr_has_vararg_top(es)) {
		label = l_emit_iabc(par, BC_CALLM, es->udat1, DMB, N_DYN, line);
	}
	else {
		label = l_emit_iabc(par, BC_CALL, es->udat1, par->scope->top_reg - es->udat1, N_DYN, line);
	}

    expr_drop(par, es);

    init(es) {
        .tag = EXPR_CALL,
        .udat1 = label,
        .fupk = true,
        .line = line
    };
}

static void expr_return(Parser* par, InoutExpr e, a_line line) {
	l_clear_close(par);
	if (e->tag == EXPR_REG) {
		l_emit_leave(par, bc_make_iab(BC_RET, e->udat1, 1), line);
	}
	else if (e->tag == EXPR_VCALL) {
		a_insn* ip = code_at(par, e->udat1);
		a_enum op = bc_load_op(ip);

		assume(e->udat1 == par->head_label - 1 && !par->fscope->fjump, "not head label.");
		assume(op == BC_CALL || op == BC_CALLM, "not call operation.");

		bc_store_op(ip, op + (BC_TCALL - BC_CALL));
		bc_store_c(ip, 0);

		par->fscope->fpass = false;
	}
	else {
        exprs_to_top_tmps(par, e);

		a_u32 len = par->scope->top_reg - e->udat1;

        expr_drop(par, e);

		if (expr_has_vararg_top(e)) {
			l_emit_leave(par, bc_make_i(BC_RETM), line);
		}
		else if (len == 0) {
			l_emit_leave(par, bc_make_i(BC_RET0), line);
		}
		else {
			l_emit_leave(par, bc_make_iab(BC_RET, e->udat1, len), line);
		}
	}
}

static void exprs_take(Parser* par, InoutExpr e, a_u32 n, a_line line) {
	assume(e->tag == EXPR_VCALL || e->tag == EXPR_VDYN, "not vararg expressions.");
	a_u32 reg = stack_alloc_succ(par, n, line);
	a_insn* ip = code_at(par, e->udat1);
	if (e->tag != EXPR_VCALL) {
		bc_store_a(ip, reg);
	}
	else {
		assume(reg == bc_load_a(ip));
	}
	bc_store_c(ip, n);
	e->fval = true;
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
	switch (ei->tag) {
		case EXPR_UNIT: {
			if (expr_has_vararg_top(el)) {
				exprs_take(par, el, n, line);
				expr_tmp(el, par->scope->top_reg - 1, line);
				return 0;
			}
			return n - 1;
		}
		case EXPR_NTMP: {
			expr_drop(par, el);
			a_u32 m = par->scope->top_reg - ei->udat1;
			if (m >= n) {
				a_u32 top = ei->udat1 + n;
				stack_free_succ(par, top);
				expr_discard(par, el);
				expr_tmp(el, top - 1, line);
				return 0;
			}
			else if (expr_has_vararg_top(el)) {
				a_u32 top = ei->udat1 + n;
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
            init(el) {
                .tag = EXPR_REFI,
                .udat1 = ei->udat1,
                .udat2 = n - 1,
                .fval = false
            };
			return 0;
		}
		default: {
			if (n > 1) {
				if (expr_has_vararg_top(el)) {
					exprs_take(par, el, n - 1, line);
					expr_tmp(el, par->scope->top_reg - 1, line);
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

static a_bool l_try_fold_append(Parser* par, InExpr e) {
	switch (e->tag) {
		case EXPR_INT: {
			at_fmt_puti(par->env, par->sbuf, e->idat);
			return true;
		}
		case EXPR_FLOAT: {
			at_fmt_putf(par->env, par->sbuf, e->ndat);
			return true;
		}
		case EXPR_STR: {
			GStr* str = e->sdat;
			at_buf_putls(par->env, par->sbuf, str->ptr, str->len);
			return true;
		}
		default: {
			return false;
		}
	}
}

static GStr* buf_to_str(Parser* par, ConExpr* ce) {
    Buf* buf = par->sbuf;
	GStr* str = ai_lex_to_str(lex(par), buf->ptr + ce->off, buf->len - ce->off);
	at_buf_clear(buf);
	return str;
}

static void expr_concat(Parser* par, ConExpr* ce, InExpr e, a_line line) {
    if (!l_try_fold_append(par, e)) {
        if (par->sbuf->len > ce->off) {
            expr_to_dyn(par, e); /* Drop used register. */

            exprs_to_top_tmps(par, ce->expr);

            a_u32 reg = stack_alloc_succ(par, 2, line);
            expr_pin_reg(par, e, reg + 1);

            GStr* str = buf_to_str(par, ce);
            l_emit_k(par, reg, v_of_str(str), line);
        }
        else {
            exprs_push(par, ce->expr, e);
        }
    }
}

static void expr_concat_end(Parser* par, ConExpr* ce, OutExpr e, a_line line) {
	if (ce->expr->tag == EXPR_UNIT) {
		expr_str(e, buf_to_str(par, ce), line);
	}
	else {
        exprs_to_top_tmps(par, ce->expr);
		if (par->sbuf->len > ce->off) {
			GStr* str = buf_to_str(par, ce);
			a_u32 reg = stack_alloc(par, line);
			l_emit_k(par, reg, v_of_str(str), line);
		}
        a_u32 base = ce->expr->udat1;
		l_emit_idbc(par, BC_CAT, e, base, par->scope->top_reg - base, line);
        expr_drop(par, ce->expr);
    }
}

static void expr_unpack(Parser* par, InoutExpr e, a_line line) {
	if (!e->fupk) {
		parse_error(par, "the expression cannot be unpack.", line);
	}
	e->tag += 1;
	e->fupk = false;
}

static a_insn l_leave_or_nop(Parser* par, a_u32 label) {
	if (unlikely(label == par->head_label))
		return 0;
	a_insn* ip = par->code[label];
	return insn_is_leave(ip) ? *ip : 0;
}

/**
 ** Jump to determined label.
 *@param par the parser.
 *@param label the label jump to.
 *@param line the line number.
 */
static void l_direct_jump(Parser* par, a_u32 label, a_line line) {
	if (l_should_emit(par)) {
        l_flush_jump(par);

		a_insn i = l_leave_or_nop(par, label);
		if (i == 0) {
			if (par->fscope->fland) {
				l_redirect_chain(par, par->fscope->head_land, label, line);
				l_clear_land(par);
			}
			if (par->fscope->fpass) {
				l_emit_jump_direct(par, label, line);
				par->fscope->fpass = false;
			}
		}
		else {
			l_emit_leave(par, i, line);
		}
	}
}

static a_u32 l_lazy_jump(Parser* par, a_u32 label, a_line line) {
	if (l_should_emit(par)) {
		l_flush_close(par); /* Force flush close. */
		if (likely(par->fscope->fpass)) {
			if (par->fscope->head_jump == NO_LABEL || label > par->fscope->head_jump) {
				par->fscope->head_jump_line = line;
			}
			par->fscope->fpass = false;
			par->fscope->fjump = true;
			par->fscope->head_jump = label;
			l_merge_branch(par, &par->fscope->head_jump, par->fscope->head_land, line);
			l_clear_land(par);
			return par->head_label; /* Return next instruction as pseudo label. */
		}
		else {
			assume(par->fscope->fland);
			l_merge_branch(par, &label, par->fscope->head_land, line);
			l_clear_land(par);
			return label;
		}
	}
	return label;
}

static a_u32 l_mark_label(Parser* par, a_u32 label, a_line line) {
	if (label != NO_LABEL) {
		if (label != par->head_label) { /* If from label is not pseudo head label, merge with head jump label. */
			par->fscope->fland = true;
			l_merge_branch(par, &par->fscope->head_land, label, line);
		}
		else {
			/* Pseudo head jump. */
			assume(par->fscope->fjump && !par->fscope->fpass);
			par->fscope->fpass = true;
			par->fscope->fland = par->fscope->head_jump != NO_LABEL;
			l_merge_branch(par, &par->fscope->head_land, par->fscope->head_jump, line);
			l_clear_jump(par);
		}
	}
	return par->head_label;
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
	switch (e->tag) {
		case EXPR_DYN: {
			a_insn* ip = code_at(par, e->udat1);
			a_u32 reg = stack_alloc(par, e->line);
            bc_store_a(ip, reg);
			stack_free(par, reg);

			e->tag = EXPR_REG;
			e->udat1 = reg;
			e->fupk = false;
			break;
		}
		case EXPR_CALL: {
			a_insn* ip = code_at(par, e->udat1);
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
	if (e->fsym) {
		a_u32 id = e->udat2;
		Sym* sym = &par->syms->ptr[id];
		if (!sym->mods.mmut) {
			parse_error(par, "cannot assign to readonly variable %s.", line, str2ntstr(sym->name));
		}
	}
}

static void expr_pin(Parser* par, InExpr e1, InExpr e2) {
    assume(e1->tag == EXPR_REG, "value already initialized.");
    expr_pin_reg(par, e2, e1->udat1);
}

static void expr_write(Parser* par, InExpr e1, InExpr e2, a_line line) {
assign:
	switch (e1->tag) {
		case EXPR_REG: {
			sym_check_writable(par, e1, line);
            expr_pin(par, e1, e2);
			break;
		}
		case EXPR_CAP: {
			sym_check_writable(par, e1, line);
            expr_to_reg(par, e2);
			l_emit_iab(par, BC_STC, e1->udat1, e2->udat2, line);

            expr_drop(par, e2);
            break;
		}
		case EXPR_REF: {
            expr_to_reg(par, e2);
			l_emit_iabc(par, BC_SET, e2->udat1, e1->udat1, e1->udat2, line);

            expr_drop(par, e2);
            expr_drop(par, e1);
            break;
		}
		case EXPR_REFI: {
            expr_to_reg(par, e2);
			l_emit_iabsc(par, BC_SETI, e2->udat1, e1->udat1, cast(a_i32, e1->udat2), line);

            expr_drop(par, e2);
            expr_drop(par, e1);
            break;
		}
		case EXPR_REFK: {
            expr_to_reg(par, e2);
            l_emit_aby(par, BC_SETS, e2->udat1, e1->udat1, e1->udat2, line);

            expr_drop(par, e2);
            expr_drop(par, e1);
            break;
		}
		case EXPR_REFCK: {
            expr_to_reg(par, e2);

			if (const_is_str(par, e1->udat2)) {
                l_emit_aby(par, BC_CSETS, e2->udat1, e1->udat1, e1->udat2, line);
			}
			else {
				a_u32 reg = stack_alloc_succ(par, 2, line);
				a_u32 label = l_emit_iab(par, BC_LDC, reg, e1->udat1, line);
				if (label != NO_LABEL) {
					l_emit_fast(par, bc_make_iabx(BC_K, reg + 1, e1->udat2), line);
                    l_emit_aby(par, BC_SETS, e2->udat1, reg, reg + 1, line);
				}
				stack_free_succ(par, reg);
			}

            expr_drop(par, e2);
            break;
		}
		case EXPR_GBL: {
			if (e1->fsym) {
				sym_check_writable(par, e1, line);
			}
			else if (!(par->options & ALO_COMP_OPT_LOSSEN)) {
				GStr* name = v_as_str(const_at(par, e1->udat2));
				parse_error(par, "cannot assign to anonymous variable '%s'", line, str2ntstr(name));
			}
			expr_gvar(par, e1, line);
			goto assign;
		}
		default: {
			panic("cannot assign to the expression.");
		}
	}
}

static void expr_tbc(Parser* par, InExpr e, a_line line) {
    assume(e->tag == EXPR_REG, "cannot mark a non-register storage variable to-be-closed.");
    l_emit_ia(par, BC_TBC, e->udat1, line);
}

static a_u32 syms_push(Parser* par, Sym sym) {
	return at_buf_push(par->env, par->syms, sym, "symbol");
}

static void sec_start(Parser* par, SecRec rec) {
    rec->line_off = par->lines->len;
    rec->head_label = par->head_label;
    rec->head_line = par->fscope->head_line;
    rec->head_jump = par->fscope->head_jump;
    rec->head_jump_line = par->fscope->head_jump_line;
    rec->head_land = par->fscope->head_land;
    rec->close_line = par->fscope->close_line;
    rec->reg_base = par->scope->top_reg;
    rec->max_reg = par->fscope->max_reg;
    rec->flags = par->fscope->flags;
    par->fscope->max_reg = par->scope->top_reg;

    par->fscope->head_line = 0;
    par->fscope->flags = 0;
    par->fscope->fpass = true;
}

static a_u32 sec_record(Parser* par, SecRec rec) {
    l_flush_jump(par);
    if (par->fscope->fland) {
        l_lazy_jump(par, NO_LABEL, par->fscope->head_line);
        l_flush_jump(par);
    }

    SecHead head = {
        .ninsn = par->insns->len - rec->head_label,
        .nline = par->lines->len - rec->head_line,
        .rel_label = rec->head_label,
        .rel_reg_bot = rec->reg_base,
        .rel_reg_top = par->fscope->max_reg
    };

    if (head.ninsn == 0) return NIL_SEC_REF;

    a_usize size = offsetof(SecHead, code)
            + sizeof(a_insn) * head.ninsn
            + sizeof(LineInfo) * head.nline;

    catch (at_buf_ncheck(par->env, par->secs, size), msg) {
        ai_buf_error(par->env, msg, "section");
    }

    a_usize base = par->secs->len;

    void* addr = par->secs->ptr + base;

    memcpy(addr, &head, offsetof(SecHead, code));
    addr += offsetof(SecHead, code);

    memcpy(addr, par->insns->ptr + rec->head_label, sizeof(a_insn) * head.ninsn);
    addr += sizeof(a_insn) * head.ninsn;

    memcpy(addr, par->lines->ptr + rec->line_off, sizeof(LineInfo) * head.nline);
    addr += sizeof(LineInfo) * head.nline;

    assume(addr == par->secs->ptr + base + size);

    par->secs->len += size;

    par->lines->len = rec->line_off;
    par->head_label = rec->head_label;
    par->fscope->head_line = rec->head_line;
    par->fscope->head_jump = rec->head_jump;
    par->fscope->head_jump_line = rec->head_jump_line;
    par->fscope->head_land = rec->head_land;
    par->fscope->close_line = rec->close_line;
    par->fscope->top_reg = rec->reg_base;
    par->fscope->max_reg = rec->max_reg;

    return base;
}

static void sec_emit(Parser* par, InoutExpr e, SecHead const* restrict sec, a_u32 abs_base, a_u32 line_call) {
    a_u32 reg_disp = abs_base - sec->rel_reg_bot;
    a_u32 label_disp = par->head_label - sec->rel_label;

    LineInfo const* lines = sec->nline > 0 ? cast(LineInfo const*, &sec->code[sec->ninsn]) : null;
    a_u32 line_info_index = 0;
    a_u32 id_next_line = sec->nline > 0 ? lines[0].lend - sec->rel_label : UINT32_MAX;
    a_u32 line = line_call;

    l_flush_close(par);

    for (a_u32 l = 0; l < sec->ninsn; ++l) {
        if (l == id_next_line) {
            LineInfo info = lines[++line_info_index];
            line = info.line;
            id_next_line = info.lend;
        }
        a_insn const* ip = &sec->code[l];
#define reloc(x) ({ a_u32 _r = bc_load_##x(ip); if (_r >= sec->rel_reg_bot) { bc_store_##x(&i, _r + reg_disp); } })
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
                else if (label >= sec->ninsn) {
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

    switch (e->tag) {
        case EXPR_REG: {
            if (e->udat1 >= sec->rel_reg_bot) {
                e->udat1 += reg_disp;
            }
            break;
        }
        case EXPR_NTMP:
        case EXPR_NTMPC: {
            assume(e->udat1 >= sec->rel_reg_bot, "registers across stack boundary.");
            e->udat1 += reg_disp;
            break;
        }
        case EXPR_REG_OR_NIL: {
            if (e->udat1 >= sec->rel_reg_bot) {
                e->udat1 += reg_disp;
            }
            e->udat2 += label_disp;
            break;
        }
        case EXPR_DYN_OR_NIL: {
            e->udat1 += label_disp;
            e->udat2 += label_disp;
            break;
        }
        case EXPR_TRUE_OR_FALSE:
        case EXPR_FALSE_OR_TRUE:
        case EXPR_RESIDUAL_FALSE:
        case EXPR_RESIDUAL_TRUE: {
            e->udat2 += label_disp;
            break;
        }
        default: {
            break;
        }
    }
}

static a_u32 sym_local(Parser* par, GStr* name, a_u32 reg, a_u32 begin_label, SymMods mods) {
	LocalInfo info = {
		.lbegin = begin_label - par->fscope->begin_label,
		.lend = NO_LABEL, /* Not determined yet. */
		.name = name,
		.reg = reg
	};

	a_u32 index = at_buf_push(par->env, par->locals, info, "local variable");

	stack_store(par, reg);

	return syms_push(par, (Sym) {
		.tag = SYM_LOCAL,
		.scope = par->scope_depth,
		.mods = mods,
		.index = index,
		.name = name
	});
}

static void pat_bind_nils(Parser* par, Pat* pat, a_line line) {
	Scope* scope = par->scope;

	assume(pat->kind == PAT_VARG);

	/* If all node is bind. */
	if (pat->child == null)
		return;

	if (pat->fcpx) {
		parse_error(par, "nil binding is only available for plain pattern.", line);
	}

	assume(scope->top_ntr == scope->top_reg);
	a_u32 num = pat->nchild - pat->child->index;
	a_u32 reg = stack_alloc_succ(par, num, line);
	a_u32 label = l_emit_kn(par, reg, num, line);
	for (Pat* pat_child = pat->child; pat_child != null; pat_child = pat_child->sibling) {
        sym_local(par, pat_child->name, reg++, label, (SymMods) {
            .mmut = pat_child->fmut
        });
	}

	scope->top_ntr = scope->top_reg;
}

static void pat_bind_with(Parser* par, Pat* pat, InExpr e, a_u32 base) {
    a_u32 reg = base + pat->abs_bot + pat->tmp_pos;
    if (pat->expr->tag != EXPR_UNIT && pat->expr->tag != EXPR_NIL) {
		assume(pat->kind != PAT_VARG);

		a_u32 label = NO_LABEL;
        expr_test_nil(par, e, &label, pat->line);

        if (pat->sec_ref != NIL_SEC_REF) {
            SecHead* sec = cast(SecHead*, par->secs->ptr + pat->sec_ref);
            sec_emit(par, pat->expr, sec, reg, pat->line);
        }

        expr_pin_reg(par, pat->expr, reg);
        l_mark_label(par, label, pat->line);
    }
	switch (pat->kind) {
		case PAT_VARG: {
			a_u32 line = pat->expr->line;
			a_u32 num = pat->nchild;

			assume(e->tag == EXPR_VCALL || e->tag == EXPR_VDYN || e->tag == EXPR_VNTMP, "not vararg expressions.");

			if (e->tag == EXPR_VNTMP) {
				l_emit_iabc(par, BC_TRIM, reg, DMB, num, line);
			}
			else {
				a_insn* ip = code_at(par, e->udat1);
				if (e->tag != EXPR_VCALL) {
					bc_store_a(ip, reg);
				}
				else {
					assume(reg == bc_load_a(ip)); //TODO
				}
				bc_store_c(ip, num);
			}
			
			for (Pat* child = pat->child; child != null; child = child->sibling) {
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
			expr_pin_reg(par, e, pat->tmp_pos);
			break;
		}
		case PAT_VAR: {
            expr_pin_reg(par, e, reg);
            sym_local(par, pat->name, reg, par->head_label, (SymMods) {
                .mmut = pat->fmut
            });
			break;
		}
		case PAT_TUPLE: {
			a_u32 line = pat->expr->line;
			a_u32 num = pat->nchild;
			l_emit_iabc(par, BC_UNBOX, reg, e->udat1, num /* TODO: variable length arguments? */, line);
			for (Pat* child = pat->child; child != null; child = child->sibling) {
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
        pat->abs_bot = abs_top;
        pat->tmp_pos = 0;
		switch (pat->kind) {
			case PAT_VAR: {
                pat->tmp_top = 1;
				abs_top += 1;
				break;
			}
			case PAT_PIN: {
				pat->tmp_top = 1;
				break;
			}
			case PAT_DROP: {
                pat->tmp_top = 1;
				break;
			}
			case PAT_VARG:
			case PAT_TUPLE: {
                pat->tmp_top = 0;
				if (pat->child != null) {
                    pat = pat->child;
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

			if (pat->sibling != null)
				break;
			
			Pat* pat_up = pat->parent;

			a_u32 used = (pat->abs_bot - pat_up->abs_bot) + pat->tmp_pos;
            pat_up->tmp_pos = used > pat->index ? max(pat_up->tmp_pos, used - pat->index) : pat_up->tmp_pos;
            pat_up->tmp_top = max(pat_up->tmp_pos + pat_up->nchild, (pat->abs_bot - pat_up->abs_bot) + pat->tmp_top);

            pat = pat_up;
		}

		Pat* pat_up = pat->parent;
        pat_up->tmp_pos = max(pat_up->tmp_pos, (pat->abs_bot - pat_up->abs_bot) + pat->tmp_top - pat->index - 1);
        pat = pat->sibling;
	}
}

static void pat_bind(Parser* par, Pat* pat, InExpr e) {
	assume(pat != null, "bind to nothing.");

    a_u32 abs_top = pat_compute(pat);

    expr_drop(par, e); /* Drop ownership but keep expression. */

    a_u32 reg = stack_alloc_succ(par, pat->tmp_top, e->line);
    pat_bind_with(par, pat, e, reg);
	assume(par->scope->top_ntr - abs_top == cast(a_u32, par->scope->top_reg - pat->tmp_top), "bind compute incorrect.");
	stack_free_succ(par, par->scope->top_ntr);
}

static a_u32 for_bind_real(Parser* par, Pat* pat, a_line line) {
	assume(pat != null, "bind to nothing.");

	a_u32 reg_itr = par->scope->top_reg - 3; /* The index of iterator register. */

	Expr e;
	a_u32 label = l_emit_branch(par, bc_make_iabc(BC_NEXTG, R_DYN, reg_itr, N_DYN), NO_LABEL, line);

    init(e) {
        .tag = EXPR_VDYN,
        .udat1 = label - 1,
        .fupk = true
    };

	pat_bind(par, pat, e);
	return label;
}

static void param_bind(Parser* par, Pat* pat, a_usize ctx) {
	a_line line = cast(a_line, ctx);
	
	Expr e;

    init(e) {
        .tag = EXPR_VNTMP,
        .udat1 = 0,
        .fupk = true,
        .line = line
    };

	pat_bind(par, pat, e);

	par->fscope->nparam = par->scope->top_ntr;
}

typedef struct {
	a_u32 label;
	a_line line;
} ForStat;

static void for_bind(Parser* par, Pat* pat, a_usize ctx) {
	ForStat* stat = int2ptr(ForStat, ctx);
	stat->label = for_bind_real(par, pat, stat->line);
}

static void local_alloc(Parser* par, OutExpr e, GStr* name, a_line line) {
	Scope* scope = par->scope;
	assume(scope->top_ntr == scope->top_reg, "stack not balanced.");
	a_u32 reg = stack_alloc(par, line);
	a_u32 sym = sym_local(par, name, reg, par->head_label, (SymMods) { });

    init(e) {
        .tag = EXPR_REG,
        .udat1 = reg,
        .udat2 = sym,
        .fval = false,
        .fsym = true,
        .line = line
    };

	scope->top_ntr = scope->top_reg;
}

static void sym_export(Parser* par, OutExpr e, GStr* name, a_line line) {
	Scope* scope = par->scope;
	assume(scope->top_ntr == scope->top_reg, "stack not balanced.");
	a_u32 id = syms_push(par, (Sym) {
		.tag = SYM_EXPORT,
		.name = name,
		.scope = par->scope_depth,
	});
	init(e) {
        .tag = EXPR_GBL,
        .udat1 = const_index(par, v_of_str(name)),
        .udat2 = id,
        .fsym = true,
        .line = line
    };
}

/**
 ** Compact fragment section.
 *@param par the parser.
 */
static void stack_compact(Parser* par) {
	Scope* scope = par->scope;
	assume(scope->top_reg == scope->top_ntr, "some temporary register is not freed.");
	scope->num_fur = 0;
}

static void scope_push(Parser* par, Scope* scope, a_u32 reg, a_line line) {
    init(scope) {
        .upscope = par->scope,
        .bot_reg = reg,
        .top_ntr = reg,
        .top_reg = reg,
        .bot_fur = reg,
        .num_fur = 0,
        .begin_line = line,
        .begin_label = par->head_label,
        .end_label = NO_LABEL,
        .sym_off = par->syms->len
    };
	par->scope = scope;
}

static void scope_pop(Parser* par, a_line line, a_bool leave) {
	Scope* scope = par->scope;
	Scope* upscope = scope->upscope;
	par->scope = upscope;
	run {
		a_u32 bot = scope->sym_off;
		a_u32 top = par->syms->len;
		a_u32 label = par->head_label - par->fscope->begin_label;
		for (a_u32 i = bot; i < top; ++i) {
			Sym* sym = &par->syms->ptr[i];
			switch (sym->tag) {
				case SYM_LOCAL: {
                    LocalInfo* info = &par->locals->ptr[sym->index];
					info->lend = label;
					break;
				}
			}
		}
		par->syms->len = bot;
	}
	scope->end_label = l_mark_label(par, scope->end_label, line);
	if (leave && scope->top_ntr != upscope->top_ntr) {
		par->fscope->fclose = true;
		par->fscope->close_line = line;
	}
}

static void scope_enter(Parser* par, Scope* scope, a_line line, a_u32 jmp_prop) {
	scope_push(par, scope, par->scope->top_reg, line);
    scope->jmp_prop = jmp_prop;
}

static void scope_leave(Parser* par, a_line line) {
	assume(par->scope->upscope != null);
	scope_pop(par, line, true);
}

static void scope_leave_with(Parser* par, a_line line, InoutExpr e) {
	assume(par->scope->upscope != null);
    /* TODO: need ensure top value is still alive */
	if (e->tag == EXPR_REG && e->udat1 == par->scope->upscope->top_reg) {
		scope_pop(par, line, true);

		a_u32 reg = stack_alloc(par, line);
		assume(reg == e->udat1);
		e->fval = true; /* Recover drop marker. */
	}
	else {
        expr_to_dyn(par, e);
		scope_pop(par, line, true);
	}
}

static void scope_break(Parser* par, GStr* label, a_line line) {
    if (label == null) {
        for (Scope* scope = par->scope; !(scope->jmp_prop & JMP_PROP_BOUND); scope = scope->upscope) {
            if (scope->jmp_prop & JMP_PROP_BREAK) {
                scope->end_label = l_lazy_jump(par, scope->end_label, line);
                return;
            }
        }
        parse_error(par, "no block for break statement", line);
    }
    else {
        for (Scope* scope = par->scope; !(scope->jmp_prop & JMP_PROP_BOUND); scope = scope->upscope) {
            if (scope->label_name == label) {
                if (scope->jmp_prop & JMP_PROP_BREAK) {
                    scope->end_label = l_lazy_jump(par, scope->end_label, line);
                    return;
                }
                parse_error(par, "cannot break at label '%s'", line, str2ntstr(label));
            }
        }
        parse_error(par, "label '%s' not found", line, str2ntstr(label));
    }
}

static void scope_continue(Parser* par, GStr* label, a_line line) {
    if (label == null) {
        for (Scope* scope = par->scope; !(scope->jmp_prop & JMP_PROP_BOUND); scope = scope->upscope) {
            if (scope->jmp_prop & JMP_PROP_CONTINUE) {
                scope->begin_label = l_lazy_jump(par, scope->end_label, line);
                return;
            }
        }
        parse_error(par, "no block for continue statement", line);
    }
    else {
        for (Scope* scope = par->scope; !(scope->jmp_prop & JMP_PROP_BOUND); scope = scope->upscope) {
            if (scope->label_name == label) {
                if (scope->jmp_prop & JMP_PROP_CONTINUE) {
                    scope->begin_label = l_lazy_jump(par, scope->end_label, line);
                    return;
                }
                parse_error(par, "cannot continue at label '%s'", line, str2ntstr(label));
            }
        }
        parse_error(par, "label '%s' not found", line, str2ntstr(label));
    }
}

static void fscope_prologue(Parser* par, FnScope* fscope, a_line line) {
	if (par->scope_depth == UINT8_MAX) {
		parse_error(par, "function nested level overflow.", line);
	}

    code_put(par, INSN_NOP); /* Add barrier for instruction look ahead. */

    init(fscope) {
        .fupscope = par->fscope,
        .base_subs = cast(GProto**, par->rq.tail),
        .const_off = par->consts->len,
        .line_off = par->lines->len,
        .local_off = par->locals->len,
        .head_jump = NO_LABEL,
        .head_land = NO_LABEL,
        .fpass = true,
        .fland = false,
        .fjump = false
    };
	scope_push(par, &fscope->as_scope, 0, line);
    fscope->as_scope.jmp_prop = JMP_PROP_BOUND;

	par->fscope = fscope;
	par->scope_depth += 1;
}

static GProto* fscope_epilogue(Parser* par, GStr* name, a_line line) {
	FnScope* fscope = par->fscope;

	l_clear_close(par);
	l_emit_leave(par, bc_make_i(BC_RET0), line);

	scope_pop(par, line, false);

	ProtoDesc desc = {
		.nconst = par->consts->len - fscope->const_off,
		.ninsn = par->head_label - fscope->begin_label,
		.nsub = fscope->nsub,
		.nlocal = par->locals->len - fscope->local_off,
		.ncap = fscope->caps->len,
		.nstack = fscope->max_reg,
		.nparam = fscope->nparam,
		.nline = par->lines->len - fscope->line_off,
		.flags =
            (!(par->options & ALO_COMP_OPT_STRIP_DEBUG) ? FUN_FLAG_DEBUG : 0) |
            (fscope->fupscope == null ? FUN_FLAG_UNIQUE : 0)
	};

	GProto* proto = ai_proto_alloc(par->env, &desc);
	if (proto == null) {
		ai_mem_nomem(par->env);
	}

	memcpy(proto->consts, par->consts->ptr + fscope->const_off, sizeof(Value) * desc.nconst);
	memcpy(proto->code, par->code + fscope->begin_label, sizeof(a_insn) * desc.ninsn);
	if (par->options & ALO_COMP_OPT_STRIP_DEBUG) {
		proto->dbg_lndef = fscope->begin_line;
		proto->dbg_lnldef = line;
		memcpy(proto->dbg_lines, par->lines->ptr + fscope->line_off, sizeof(LineInfo) * desc.nline);
		memcpy(proto->dbg_locals, par->locals->ptr + fscope->local_off, sizeof(LocalInfo) * desc.nlocal);
	}
	run {
		for (a_u32 i = 0; i < desc.ncap; ++i) {
			RichCapInfo* cap_info = &fscope->caps->ptr[i];
            init(&proto->caps[i]) {
				.reg = cap_info->src_index,
				.fup = cap_info->scope != par->scope_depth - 1
			};
			if (desc.flags & FUN_FLAG_DEBUG) {
				proto->dbg_cap_names[i] = cap_info->name;
			}
		}
	}
	run { /* Build sub function */
		GProto** src = fscope->base_subs;
		GProto** dst = proto->subs;
		GProto* val = *src;

		GProto** const end = cast(GProto**, par->rq.tail);
		par->rq.tail = cast(a_gclist*, src);

		while (src != end) {
			*dst = val;
			*src = null;

			src = cast(GProto**, &val->gnext);
			val = *src;
			dst += 1;
		}
	}
	
	proto->dbg_name = name;
	if (desc.flags & FUN_FLAG_DEBUG) {
		proto->dbg_file = from_member(GStr, ptr, par->lex.file);
		proto->dbg_lndef = fscope->begin_line;
		proto->dbg_lnldef = line;
	}

	at_buf_deinit(G(par->env), fscope->caps);

    par->scope = fscope->upscope;
	par->fscope = fscope->fupscope;
	par->scope_depth -= 1;
	par->head_label = fscope->begin_label - 1; /* Drop barrier. */
	par->locals->len = fscope->local_off;
	par->lines->len = fscope->line_off;
	par->rq.tail = cast(a_gclist*, fscope->base_subs);

	rq_push(&par->rq, proto);

	return proto;
}

static void proto_drop_recursive(a_henv env, GProto* proto) {
	for (a_u32 i = 0; i < proto->nsub; ++i) {
        proto_drop_recursive(env, proto->subs[i]);
	}
	ai_proto_drop(G(env), proto);
}

static void parser_close(Parser* par) {
    Global* gbl = G(par->env);
    at_buf_deinit(gbl, par->insns);
	at_buf_deinit(gbl, par->consts);
	at_buf_deinit(gbl, par->lines);
	at_buf_deinit(gbl, par->locals);
	at_buf_deinit(gbl, par->syms);
    at_buf_deinit(gbl, par->secs);
    at_buf_deinit(gbl, par->sbuf);
	ai_lex_close(&par->lex);
}

static void parser_mark(Global* gbl, void* ctx) {
	Parser* par = ctx;
	run {
		StrSet* set = &par->lex.strs;
        if (set->ptr != null) {
            for (a_u32 i = 0; i <= set->hmask; ++i) {
                GStr* str = set->ptr[i];
                if (str != null) {
                    ai_gc_trace_mark(gbl, str);
                }
            }
        }
	}
    g_set_stack_white(par);
}

static void parser_except(a_henv env, void* ctx, unused a_msg msg) {
	Parser* par = ctx;
	assume(env == par->env);
	/* Destroy queued prototypes. */
	rq_for(obj, &par->rq) {
        proto_drop_recursive(par->env, g_as(GProto, obj));
	}
	for (FnScope* scope = par->fscope; scope != null; scope = scope->fupscope) {
		at_buf_deinit(G(par->env), scope->caps);
	}
	/* Close parser. */
	parser_close(par);
}

#define ENV_NAME "_ENV"

static void parser_start(Parser* par) {
    ai_lex_open(lex(par), par->options);

	par->gvar_name = ai_lex_to_str(lex(par), ENV_NAME, sizeof(ENV_NAME) - 1);

	/* Add predefined '_ENV' dbg_name. */
	syms_push(par, (Sym) {
		.tag = SYM_CAPTURE,
		.scope = 0,
		.mods = {
			.mmut = false /* Predefined environment is always readonly variable. */
		},
		.index = 0,
		.name = par->gvar_name
	});
}

static void proto_register_recursive(a_henv env, GProto* proto) {
    if (proto->flags & FUN_FLAG_UNIQUE) {
        assume(proto->cache != null, "no unique instance given");
        ai_gc_register_object(env, proto->cache);
    }
    else {
        assume(proto->gnext == null, "children function not collected");
        ai_gc_register_object(env, proto);
    }
	for (a_u32 i = 0; i < proto->nsub; ++i) {
        proto_register_recursive(env, proto->subs[i]);
	}
}

static GFun* func_build(Parser* par) {
	GProto* proto = g_as(GProto, par->rq.head); /* Get root prototype. */
    proto_register_recursive(par->env, proto);
	parser_close(par);

    GFun* fun = proto->cache;
	g_set_white(G(par->env), fun);

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
	switch (e->tag) {
		case EXPR_NIL: {
			return l_emit_iabc(par, BC_KN, reg, DMB, 1, e->line);
		}
		case EXPR_FALSE: {
			return l_emit_ia(par, BC_KF, reg, e->line);
		}
		case EXPR_TRUE: {
			return l_emit_ia(par, BC_KT, reg, e->line);
		}
		case EXPR_INT: {
			if (e->idat >= BC_MIN_SBX && e->idat <= BC_MAX_SBX) {
				return l_emit_iasbx(par, BC_KI, reg, e->idat, e->line);
			}
			else {
				return l_emit_iabx(par, BC_K, reg, const_index(par, v_of_int(e->idat)), e->line);
			}
		}
		case EXPR_FLOAT: {
			return l_emit_iabx(par, BC_K, reg, const_index(par, v_of_float(e->ndat)), e->line);
		}
		case EXPR_STR: {
			return l_emit_iabx(par, BC_K, reg, const_index(par, v_of_str(e->sdat)), e->line);
		}
		case EXPR_CAP: {
			return l_emit_iab(par, BC_LDC, reg, e->udat1, e->line);
		}
		case EXPR_REG: {
            return l_emit_iab(par, BC_MOV, reg, e->udat1, e->line);
		}
		case EXPR_REGS: {
			l_emit_iac(par, BC_TRIM, e->udat1, 1, e->line);
			return l_emit_iab(par, BC_MOV, reg, e->udat1, e->line);
		}
		case EXPR_REF: {
            return l_emit_iabc(par, BC_GET, reg, e->udat1, e->udat2, e->line);
		}
		case EXPR_REFI: {
            return l_emit_iabsc(par, BC_GETI, reg, e->udat1, cast(a_i32, e->udat2), e->line);
		}
		case EXPR_REFK: {
            a_u32 k = e->udat2;
			if (const_is_str(par, k)) {
				return l_emit_aby(par, BC_GETS, reg, e->udat1, k, e->line);
			}
			else {
				a_u32 reg2 = stack_alloc(par, e->line);
				a_u32 label = l_emit(par, bc_make_iabx(BC_K, reg2, k), e->line);
				if (label != NO_LABEL) {
					l_emit_fast(par, bc_make_iabc(BC_GET, reg, e->udat1, reg2), e->line);
					label += 1;
				}
				stack_free(par, reg2);
				return label;
			}
			break;
		}
		case EXPR_REFCK: {
			a_u32 k = e->udat2;
			if (const_is_str(par, k)) {
				return l_emit_aby(par, BC_CGETS, reg, e->udat1, k, e->line);
			}
			else {
				a_u32 reg2 = stack_alloc_succ(par, 2, e->line);
				a_u32 label = l_emit(par, bc_make_iabx(BC_K, reg2, k), e->line);
				if (label != NO_LABEL) {
					l_emit_fast(par, bc_make_iab(BC_LDC, reg2 + 1, e->udat1), e->line);
					l_emit_fast(par, bc_make_iabc(BC_GET, reg, reg2 + 1, reg2), e->line);
					label += 2;
				}
				stack_free_succ(par, reg2);
				return label;
			}
		}
		case EXPR_DYN: {
            bc_store_a(par->code[e->udat1], reg);
			return e->udat1;
		}
		case EXPR_CALL: {
			a_insn* ip = par->code[e->udat1];
			return l_emit_iab(par, BC_MOV, reg, bc_load_a(ip), e->line);
		}
		case EXPR_FALSE_OR_TRUE:
		case EXPR_TRUE_OR_FALSE: {
			a_u32 reg2 = stack_alloc(par, e->line);
            branch_instantiate(par, e, reg2);
			return l_emit_iab(par, BC_MOV, reg, reg2, e->line);
		}
		case EXPR_REG_OR_NIL: {
			a_u32 reg2;
			if (e->fsym) {
				assume(!e->fval, "a shared value has ownership.");
				reg2 = stack_alloc(par, e->line);
				l_emit_iab(par, BC_MOV, reg2, e->udat1, e->line);
				stack_free(par, reg2);
				l_merge_optR(par, e->udat2, reg2, e->line);
			}
			else {
				reg2 = e->udat1;
				l_merge_optR(par, e->udat2, reg2, e->line);
			}
			return l_emit_iab(par, BC_MOV, reg, reg2, e->line);
		}
		case EXPR_DYN_OR_NIL: {
			a_u32 reg2 = stack_alloc(par, e->line);
            bc_store_a(par->code[e->udat1], reg2);
			stack_free(par, reg2);
			l_merge_optR(par, e->udat2, reg2, e->line);
			return l_emit_iab(par, BC_MOV, reg, reg2, e->line);
		}
		case EXPR_GBL: {
			expr_gvar(par, e, e->line);
			goto bind;
		}
		default: {
			panic("cannot move expression with kind: %u.", e->tag);
		}
	}
}

static void expr_move_to_if_need(Parser* par, InExpr e, a_u32 reg) {
	switch (e->tag) {
		case EXPR_NIL: {
			l_emit_kn(par, reg, 1, e->line);
			break;
		}
		case EXPR_REG: {
			if (e->udat1 != reg) {
                l_emit_iab(par, BC_MOV, reg, e->udat1, e->line);
			}
			break;
		}
		case EXPR_REGS: {
			l_emit_iac(par, BC_TRIM, e->udat1, 1, e->line);
			if (e->udat1 != reg) {
                l_emit_iab(par, BC_MOV, reg, e->udat1, e->line);
			}
			break;
		}
		case EXPR_REG_OR_NIL: {
			if (e->udat1 != reg) {
				l_emit_iab(par, BC_MOV, reg, e->udat1, e->line);
			}
			l_merge_optR(par, e->udat2, reg, e->line);
			break;
		}
		case EXPR_DYN_OR_NIL: {
            bc_store_a(par->code[e->udat1], reg);
			l_merge_optR(par, e->udat2, reg, e->line);
			break;
		}
		case EXPR_FALSE_OR_TRUE:
		case EXPR_TRUE_OR_FALSE: {
            branch_instantiate(par, e, reg);
			break;
		}
		case EXPR_CALL: {
			a_u32 reg2 = bc_load_a(par->code[e->udat1]);
			if (reg != reg2) {
				l_emit_iab(par, BC_MOV, reg, reg2, e->line);
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
    if (e->tag != EXPR_DYN) {
        expr_drop(par, e);
        a_u32 label = expr_move_to(par, e, R_DYN);
        expr_dyn(e, label, e->line);
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
	expr_val(e, reg, e->line);
}

static void expr_to_top_tmp(Parser* par, InoutExpr e) {
    expr_drop(par, e);
	a_u32 reg = stack_alloc(par, e->line);
    expr_pin_reg(par, e, reg);
    e->fval = true;
}

static void expr_to_reg(Parser* par, InoutExpr e) {
	switch (e->tag) {
		case EXPR_REG: {
			break;
		}
		case EXPR_REGS: {
			l_emit_iac(par, BC_TRIM, e->udat1, 1, e->line);

			e->tag = EXPR_REG;
			break;
		}
		case EXPR_REG_OR_NIL: {
			a_u32 reg = e->udat1;
            expr_drop(par, e);
			if (!e->fsym) {
				l_merge_optR(par, e->udat2, reg, e->line);
				e->tag = EXPR_REG;
			}
			else {
				a_u32 reg2 = stack_alloc(par, e->line);
				l_emit_iab(par, BC_MOV, reg2, reg, e->line);
				l_merge_optR(par, e->udat2, reg2, e->line);
				expr_tmp(e, reg2, e->line);
			}
			break;
		}
		case EXPR_CALL: {
			a_u32 reg = stack_alloc(par, e->line);
			assume(reg == bc_load_a(par->code[e->udat1]), "vararg are not in the top.");
			expr_tmp(e, reg, e->line);
			break;
		}
		default: {
            expr_to_top_tmp(par, e);
			break;
		}
	}
}

static void expr_to_tmp(Parser* par, InoutExpr e) {
	if (e->tag != EXPR_REG || !e->fsym) {
        expr_to_top_tmp(par, e);
	}
}

static void expr_to_reg_or_const(Parser* par, InoutExpr e) {
	switch (e->tag) {
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
	switch (e->tag) {
		case EXPR_VDYN: {
			a_insn* ip = code_at(par, e->udat1);

			a_u32 reg = stack_alloc(par, e->line);

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_a(ip, reg);
            bc_store_c(ip, DMB);

			l_emit_iab(par, BC_LBOXM, reg, reg, e->line);

			init(e) {
                .tag = EXPR_NTMPC,
                .udat1 = reg,
                .fval = true,
                .fucf = e->fucf
            };
			break;
		}
		case EXPR_VCALL: {
			a_insn* ip = par->code[e->udat1];

			a_u32 reg = stack_alloc(par, e->line);
			assume(reg == bc_load_a(ip), "not top of stack.");

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_c(ip, DMB);

			l_emit_iab(par, BC_LBOXM, reg, reg, e->line);

			init(e) {
                .tag = EXPR_NTMPC,
                .udat1 = reg,
                .fval = true,
                .fucf = e->fucf
            };
			break;
		}
		case EXPR_VNTMP: {
			a_u32 bot = e->udat1;

			l_emit_iab(par, BC_LBOXM, bot, bot, e->line);
			stack_free_succ(par, bot + 1);

			e->tag = EXPR_NTMPC;
			break;
		}
		case EXPR_VNTMPC: {
			a_u32 col = e->udat1;
			a_u32 bot = col + 1;

			l_emit_iab(par, BC_LPUSHM, col, bot, e->line);
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
	switch (e->tag) {
		case EXPR_UNIT: {
			e->tag = EXPR_NTMP;
			e->udat1 = par->scope->top_reg;
			break;
		}
		case EXPR_NTMP:
		case EXPR_VNTMP: {
			break;
		}
		case EXPR_NTMPC: {
			a_u32 col = e->udat1;
			a_u32 bot = col + 1;
			a_u32 top = par->scope->top_reg;

			if (bot < top) {
				l_emit_iabc(par, BC_LPUSH, col, bot, top - bot, e->line);
				stack_free_succ(par, bot);
			}

			l_emit_iab(par, BC_UNBOXV, col, col, e->line);

			e->tag = EXPR_VNTMP;
			break;
		}
		case EXPR_VNTMPC: {
			a_u32 col = e->udat1;
			a_u32 bot = col + 1;
			a_u32 top = par->scope->top_reg;

			if (bot < top) {
				l_emit_iab(par, BC_LPUSHM, col, bot, e->line);
				stack_free_succ(par, bot);
			}

			l_emit_iab(par, BC_UNBOXV, col, col, e->line);

			e->tag = EXPR_NTMP;
			break;
		}
		case EXPR_VREGS: {
			e->tag = EXPR_VNTMP;
			break;
		}
		case EXPR_VDYN: {
			a_insn* ip = code_at(par, e->udat1);
			a_u32 reg = par->scope->top_reg;

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_a(ip, reg);
            bc_store_c(ip, DMB);

			e->tag = EXPR_VNTMP;
			e->udat1 = reg;
			break;
		}
		case EXPR_VCALL: {
			a_insn* ip = code_at(par, e->udat1);
			a_u32 reg = bc_load_a(ip);

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_c(ip, DMB);

			e->tag = EXPR_VNTMP;
			e->udat1 = reg;
			break;
		}
		default: {
            expr_to_top_tmp(par, e);

            e->tag = EXPR_NTMP;
			break;
		}
	}
}

static a_enum expr_test_true(Parser* par, InExpr e, a_u32* plabel, a_u32 line) {
	switch (e->tag) {
		case EXPR_TRUE:
		case EXPR_INT:
		case EXPR_FLOAT:
		case EXPR_STR: {
			return EXPR_TRUE;
		}
		case EXPR_NIL:
		case EXPR_FALSE: {
			*plabel = l_lazy_jump(par, *plabel, line);
			e->fucf = true;
			return EXPR_RESIDUAL_FALSE;
		}
		case EXPR_RESIDUAL_TRUE: {
			l_mark_label(par, e->udat2, line);
			e->tag = EXPR_TRUE;
			e->fucf = false;
			return EXPR_TRUE;
		}
		case EXPR_TRUE_OR_FALSE: {
			l_merge_branch(par, plabel, e->udat2, line);
			return EXPR_TRUE_OR_FALSE;
		}
		case EXPR_FALSE_OR_TRUE: {
            branch_negate(par, plabel, e->udat2, line);
			return EXPR_TRUE_OR_FALSE;
		}
		case EXPR_REG_OR_NIL: {
			l_merge_branch(par, plabel, e->udat2, line);
			e->tag = EXPR_REG;
			*plabel = l_emit_branch(par, bc_make_ia(BC_BZ, e->udat1), *plabel, line);
			return EXPR_TRUE_OR_FALSE;
		}
		case EXPR_DYN_OR_NIL: {
			l_merge_branch(par, plabel, e->udat2, line);
			e->tag = EXPR_DYN;
			fallthrough;
		}
		default: {
            expr_to_reg(par, e);
			assume(e->tag == EXPR_REG);
			*plabel = l_emit_branch(par, bc_make_ia(BC_BZ, e->udat1), *plabel, line);
			return EXPR_TRUE_OR_FALSE;
		}
	}
}

static a_enum expr_test_false(Parser* par, InExpr e, a_u32* plabel, a_u32 line) {
	switch (e->tag) {
		case EXPR_NIL:
		case EXPR_FALSE: {
			return EXPR_FALSE;
		}
		case EXPR_INT:
		case EXPR_FLOAT:
		case EXPR_STR:
		case EXPR_TRUE: {
			*plabel = l_lazy_jump(par, *plabel, line);
			e->fucf = true;
			return EXPR_RESIDUAL_TRUE;
		}
		case EXPR_RESIDUAL_FALSE: {
			l_mark_label(par, e->udat2, line);
			e->tag = EXPR_FALSE;
			e->fucf = false;
			return EXPR_FALSE;
		}
		case EXPR_FALSE_OR_TRUE: {
			l_merge_branch(par, plabel, e->udat2, line);
			return EXPR_FALSE_OR_TRUE;
		}
		case EXPR_TRUE_OR_FALSE: {
            branch_negate(par, plabel, e->udat2, line);
			return EXPR_FALSE_OR_TRUE;
		}
		case EXPR_REG_OR_NIL: {
			*plabel = l_emit_branch(par, bc_make_ia(BC_BNZ, e->udat1), *plabel, line);
			l_merge_branch(par, plabel, e->udat2, line);
			e->tag = EXPR_REG;
			return EXPR_TRUE_OR_FALSE;
		}
		case EXPR_DYN_OR_NIL: {
			l_merge_branch(par, plabel, e->udat2, line);
			e->tag = EXPR_DYN;
			fallthrough;
		}
		default: {
            expr_to_reg(par, e);
			*plabel = l_emit_branch(par, bc_make_ia(BC_BNZ, e->udat1), *plabel, line);
			return EXPR_FALSE_OR_TRUE;
		}
	}
}

static a_enum expr_test_nil(Parser* par, InExpr e, a_u32* plabel, a_u32 line) {
	switch (e->tag) {
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
			*plabel = l_emit_branch(par, bc_make_ia(BC_BN, e->udat1), *plabel, line);
			l_mark_label(par, e->udat2, line);

			e->tag = EXPR_REG;
			return EXPR_TRUE_OR_FALSE;
		}
		case EXPR_DYN_OR_NIL: {
			a_u32 label = e->udat2;

			e->tag = EXPR_DYN;
			expr_to_reg(par, e);

			assume(e->tag == EXPR_REG);
			*plabel = l_emit_branch(par, bc_make_ia(BC_BN, e->udat1), *plabel, line);
			l_mark_label(par, label, line);

			return EXPR_TRUE_OR_FALSE;
		}
		default: {
            expr_to_reg(par, e);

			assume(e->tag == EXPR_REG);
			*plabel = l_emit_branch(par, bc_make_ia(BC_BN, e->udat1), *plabel, line);

			return EXPR_TRUE_OR_FALSE;
		}
	}
}

static a_enum expr_test_not_nil(Parser* par, InExpr e, a_u32* plabel, a_u32 line) {
	switch (e->tag) {
		case EXPR_TRUE:
		case EXPR_INT:
		case EXPR_FLOAT:
		case EXPR_STR:
		case EXPR_FALSE: {
			return EXPR_TRUE;
		}
		case EXPR_NIL: {
			*plabel = l_lazy_jump(par, *plabel, line);
			e->fucf = true;
			return EXPR_RESIDUAL_FALSE;
		}
		case EXPR_REG_OR_NIL: {
			l_merge_branch(par, plabel, e->udat2, line);
			*plabel = l_emit_branch(par, bc_make_ia(BC_BNN, e->udat1), *plabel, line);

			e->tag = EXPR_REG;
			return EXPR_TRUE_OR_FALSE;
		}
		case EXPR_DYN_OR_NIL: {
			l_merge_branch(par, plabel, e->udat2, line);

			e->tag = EXPR_DYN;
			fallthrough;
		}
		default: {
            expr_to_reg(par, e);
			assume(e->tag == EXPR_REG);
			*plabel = l_emit_branch(par, bc_make_ia(BC_BNN, e->udat1), *plabel, line);

			return EXPR_TRUE_OR_FALSE;
		}
	}
}

static a_u32 expr_catch_nil_branch(Parser* par, InoutExpr e, a_u32 line) {
	switch (e->tag) {
		case EXPR_NIL: {
			a_u32 label = l_lazy_jump(par, NO_LABEL, line);
			e->tag = EXPR_NIL;
			return label;
		}
		case EXPR_DYN_OR_NIL: {
			a_u32 label = e->udat2;
			e->tag = EXPR_DYN;
			return label;
		}
		case EXPR_REG_OR_NIL: {
			a_u32 label = e->udat2;
			e->tag = EXPR_REG;
			return label;
		}
		default: {
			return NO_LABEL;
		}
	}
}

/*=========================================================*/

#define lex_line(par) (lex(par)->ahead[0].line)
#define lex_token(par) (&lex(par)->ahead[0])

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

static void l_scan_atom(Parser* par, OutExpr e);
static void l_scan_expr(Parser* par, OutExpr e);
static void l_scan_expr_from_term(Parser* par, OutExpr e);
static void l_scan_exprs(Parser* par, InoutExpr es, a_bool exists);
static void l_scan_pattern(Parser* par, void (*con)(Parser*, Pat*, a_usize), a_usize ctx);
static void l_scan_stat(Parser* par);
static void l_scan_stats(Parser* par);

static GStr* l_scan_label(Parser* par) {
    return lex_test_skip(par, TK_COLON) ? lex_check_ident(par) : null;
}

static GStr* l_scan_label_for_scope(Parser* par) {
    GStr* name = l_scan_label(par);
    par->scope->label_name = name;
    return name;
}

static void l_scan_tstring(Parser* par, OutExpr e) {
	ConExpr ce = { .off = par->sbuf->len };
	a_u32 line = lex_line(par);
	lex_skip(par);

	loop {
		a_u32 line2 = lex_line(par);
		switch (lex_peek2(par, line)) {
			case TK_STRING: {
				expr_str(e, lex_token(par)->as_str, line2);
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

    fscope_prologue(par, &scope, line);

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
        default: lex_error_got(par, "function expected");
	}

	GProto* proto = fscope_epilogue(par, name, lex_line(par));
	expr_func(par, e, proto);
}

static void l_scan_lambda(Parser* par, OutExpr e) {
	FnScope scope;

    fscope_prologue(par, &scope, lex_line(par));

	if (!lex_test_skip(par, TK_BBAR)) {
		a_line line = lex_line(par);
		lex_check_skip(par, TK_BAR);
		l_scan_pattern(par, param_bind, scope.begin_line);
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
		expr_return(par, e2, scope.begin_line);
	}

	GProto* proto = fscope_epilogue(par, null, lex_line(par));
	expr_func(par, e, proto);
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

                expr_write(par, e, e3, line);
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
    expr_write(par, e, e2, line);
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

	scope_enter(par, &scope, line, 0);

    local_alloc(par, e, null, line);
	expr_new_table(par, er, line);
    expr_write(par, e, er, line);

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
				expr_int(ek, lex_token(par)->as_int, line2);
				expr_index(par, er, ek, line2);
            }
            else {
                lex_error_got(par, "table entry expected");
            }

            lex_sync(par);
            lex_skip(par);

            l_scan_expr(par, ev);

            expr_write(par, er, ev, line2);
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
            expr_write(par, er, ev, line);
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
			expr_str(e, lex_token(par)->as_str, lex_line(par));
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
	Expr expr;
	LhsNode* last;
};

struct Lhs {
	LhsNode head;
	LhsNode* tail;
	a_u32 count;
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
        expr_write(par, node->expr, en, line);

		node = node->last;
		assume(node != null);
		num_nil -= 1;
	}

	loop {
        expr_write(par, node->expr, rhs_last, line);
		if ((node = node->last) == null)
			return;

		exprs_pop(par, rhs, rhs_last, line);
	}
}

static void l_scan_assign_lhs_tail(Parser* par, Lhs* lhs) {
	LhsNode n = {};

	if (lex_test_skip(par, TK_COMMA)) {
		n.last = lhs->tail;

		lhs->count += 1;
		lhs->tail = &n;

        l_scan_term_with_prefix(par, n.expr);
		return l_scan_assign_lhs_tail(par, lhs);
	}

	l_scan_assign_rhs(par, lhs->tail, lhs->count + 1);
}

static void l_scan_assign_or_call(Parser* par) {
	Lhs lhs = {};

	a_line line = lex_line(par);
	l_scan_term(par, lhs.head.expr);
	if (lex_test(par, TK_COMMA) || lex_test_skip(par, TK_TDOT) || lex_test(par, TK_ASSIGN)) {
		lhs.tail = &lhs.head;
		l_scan_assign_lhs_tail(par, &lhs);
	}
	else if (lhs.head.expr->tag != EXPR_CALL) {
		parse_error(par, "assignment or function call expected.", line);
	}

    lex_test_skip(par, TK_SEMI);
	expr_discard(par, lhs.head.expr);
}

static void l_scan_do_stat(Parser* par) {
    Scope scope;
    lex_skip(par);

    lex_check_skip(par, TK_LBR);
    a_line line = lex_line(par);

    scope_enter(par, &scope, line, 0);
    l_scan_stats(par);

    lex_check_pair_right(par, TK_LBR, TK_RBR, line);
    scope_leave(par, lex_line(par));
}

static void l_scan_if_stat(Parser* par) {
	lex_skip(par);

	lex_check_skip(par, TK_LBK);
	a_line line = lex_line(par);

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
    Scope scope;
    scope_enter(par, &scope, lex_line(par), JMP_PROP_BREAK | JMP_PROP_CONTINUE);

    lex_skip(par);

    l_scan_label_for_scope(par);

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

    scope_leave(par, lex_line(par));
}

static void l_scan_loop_stat(Parser* par) {
    Scope scope;
    scope_enter(par, &scope, lex_line(par), JMP_PROP_BREAK | JMP_PROP_CONTINUE);

    lex_skip(par);

    l_scan_label_for_scope(par);

	a_u32 label = l_mark_label(par, NO_LABEL, lex_line(par));

	l_scan_stat(par);
	l_direct_jump(par, label, lex_line(par));

    scope_leave(par, lex_line(par));
}

static void l_scan_break_stat(Parser* par) {
    a_line line = lex_line(par);

    lex_skip(par);

    GStr* label = l_scan_label(par);
    scope_break(par, label, line);

    lex_test_skip(par, TK_SEMI);
}

static void l_scan_continue_stat(Parser* par) {
    a_line line = lex_line(par);

    lex_skip(par);

    GStr* label = l_scan_label(par);
    scope_continue(par, label, line);

    lex_test_skip(par, TK_SEMI);
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

static a_noret par_error_dup_mod(Parser* par, a_i32 tk) {
    parse_error(par, "duplicate '%s' modifier", lex_line(par), ai_lex_tagname(tk));
}

static void l_scan_var_pattern(Parser* par, Pat* pat) {
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
                pat->kind = PAT_VAR;
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

static void l_scan_pattern_recursive(Parser* par, PatInfo* info, Pat* parent, Pat** slot, a_u32 tag) {
    Pat pat_desc = { .sec_ref = NIL_SEC_REF };

    Pat* pat = &pat_desc;

    pat->parent = parent;
    pat->index = parent->nchild;
	parent->nchild += 1;
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
            l_scan_var_pattern(par, pat);
			break;
		}
		case TK__: {
            lex_skip(par);
			parent->fcpx = true;
            pat->kind = PAT_DROP;
            pat->line = lex_line(par);
			break;
		}
		case TK_LBK: {
            lex_skip(par);
			parent->fcpx = true;
            pat->kind = PAT_TUPLE;
            pat->line = lex_line(par);
            pat->nchild = 0;
			if (!lex_test_skip(par, TK_RBK)) {
				return l_scan_pattern_recursive(par, info, pat, &pat->child, PAT_TUPLE);
			}
			break;
		}
		case TK_LSQ: {
            lex_skip(par);
			parent->fcpx = true;
            pat->kind = PAT_LIST;
            pat->line = lex_line(par);
            pat->nchild = 0;
			if (!lex_test_skip(par, TK_RSQ)) {
				return l_scan_pattern_recursive(par, info, pat, &pat->child, PAT_LIST);
			}
			break;
		}
		case TK_LBR: {
            lex_skip(par);
			parent->fcpx = true;
            pat->kind = PAT_TABLE;
            pat->line = lex_line(par);
            pat->nchild = 0;
			if (!lex_test_skip(par, TK_RBR)) {
				return l_scan_pattern_recursive(par, info, pat, &pat->child, PAT_TABLE);
			}
			break;
		}
		default: goto error;
	}

	slot = &pat->sibling;
	loop {
		switch (lex_peek(par)) {
			case TK_COMMA: {
                lex_skip(par);
				tag = parent->kind; /* Recover goto label. */
				return l_scan_pattern_recursive(par, info, parent, slot, tag);
			}
			case TK_RBK: {
				if (parent->kind != PAT_TUPLE) {
					if (parent->kind == PAT_VARG)
						goto load_nils;
					goto error_unexpected;
				}
                lex_skip(par);
				break;
			}
			case TK_RSQ: {
				if (parent->kind != PAT_LIST) {
					if (parent->kind == PAT_VARG)
						goto load_nils;
					goto error_unexpected;
				}
                lex_skip(par);
				break;
			}
			case TK_RBR: {
				if (parent->kind != PAT_TABLE) {
					if (parent->kind == PAT_VARG)
						goto load_nils;
					goto error_unexpected;
				}
                lex_skip(par);
				break;
			}
			case TK_ASSIGN: {
				if (parent->kind == PAT_VARG) {
					return (*info->con)(par, &info->root, info->ctx);
				}
				else if (pat->fdfl) {
					goto error;
				}
                SecRec rec;

                lex_skip(par);

                sec_start(par, rec);
                l_scan_expr(par, pat->expr);

                pat->sec_ref = sec_record(par, rec);
                pat->fdfl = true;
                continue;
			}
			default:
			load_nils: {
				return (*info->con)(par, &info->root, info->ctx);
			}
		}

		assume(parent != null);
		slot = &parent->sibling;
        pat = parent;
		parent = parent->parent;
	}

error:
	lex_error_got(par, "malformed pattern");

error_unexpected:
	switch (parent->kind) {
		case PAT_VARG:
			lex_error_got(par, "malformed pattern");
		case PAT_TUPLE:
            lex_error_bracket(par, '(', ')', parent->expr->line);
		case PAT_LIST:
            lex_error_bracket(par, '[', ']', parent->expr->line);
		case PAT_TABLE:
            lex_error_bracket(par, '{', '}', parent->expr->line);
		default:
			unreachable();
	}
}

static void l_scan_pattern(Parser* par, void (*con)(Parser*, Pat*, a_usize), a_usize ctx) {
	PatInfo info = {
		.root = { .kind = PAT_VARG, .sec_ref = NIL_SEC_REF },
		.con = con,
		.ctx = ctx
	};
	l_scan_pattern_recursive(par, &info, &info.root, &info.root.child, PAT_VARG);
}

static void l_scan_for_stat(Parser* par) {
	Scope scope = {};
	ForStat stat;

	Expr e;
	lex_skip(par);

	a_u32 line = lex_line(par);
	scope_enter(par, &scope, line, JMP_PROP_BREAK | JMP_PROP_CONTINUE);
	stat.line = line;
	
	lex_check_skip(par, TK_LBK);
	l_scan_expr(par, e);
	lex_check_pair_right(par, TK_LBK, TK_RBK, line);
	expr_iter(par, e, line);

	a_u32 label = l_mark_label(par, NO_LABEL, line);

	a_u32 line2 = lex_line(par);
	lex_check_skip(par, TK_BAR);
	l_scan_pattern(par, for_bind, ptr2int(&stat));
	lex_check_pair_right(par, TK_BAR, TK_BAR, line2);

	l_scan_stat(par);
	l_direct_jump(par, label, line);

	l_mark_label(par, stat.label, line);

    if (lex_test_skip(par, TK_else)) {
        lex_sync(par);
        l_scan_stat(par);
    }

    scope_leave(par, lex_line(par));
	
	expr_drop(par, e);
}

static void l_scan_let_stat2(Parser* par, Pat* p, a_usize c) {
    a_line line = cast(a_line, c);

    if (lex_test_skip(par, TK_ASSIGN)) {
        Expr e;
        Pat* pat = p->child;

        loop {
            l_scan_expr(par, e);
            pat_bind(par, pat, e);

            pat = pat->sibling;
            p->child = pat;
            p->nchild -= 1;

            if (!lex_test_skip(par, TK_COMMA))
                break;

            if (pat == null) {
                do {
                    l_scan_expr(par, e);
                    expr_discard(par, e);
                }
                while (lex_test_skip(par, TK_COMMA));

                stack_compact(par);
                return;
            }
        }
    }
    else {
        pat_bind_nils(par, p, line);
    }
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
            local_alloc(par, e1, name, line);

            lex_check_skip(par, TK_ASSIGN);

            l_scan_expr(par, e2);
            expr_pin(par, e1, e2);
            expr_tbc(par, e1, line);
            break;
        }
		case TK_IDENT: {
			if (lex_forward(par) == TK_LBK) {
                Expr e1, e2;
                GStr* name;
			scan_func:
                name = lex_check_ident(par);
                local_alloc(par, e1, name, line);

				l_scan_function(par, e2, name, line);

                expr_pin(par, e1, e2);
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

    /* TODO: Should check symbol conflict here? */
	GStr* name = lex_check_ident(par);
    local_alloc(par, en, name, lex_line(par));

	while (lex_test_skip(par, TK_DOT)) {
		name = lex_check_ident(par);
		expr_index_str(par, en, name, lex_line(par));
	}

	l_scan_function(par, ef, name, line);

    expr_pin(par, en, ef);
}

static void l_scan_pub_stat(Parser* par) {
	a_line line = lex_line(par);
    lex_skip(par);

	if ((lex_test(par, TK_IDENT) && lex_forward(par) == TK_LBK) || lex_test_skip(par, TK_fn)) {
		Expr e1, e2;
		GStr* name = lex_check_ident(par);
        sym_export(par, e1, name, line);
		l_scan_function(par, e2, name, line);
        expr_write(par, e1, e2, line);
	}
	else {
		Expr e;
		GStr* name = lex_check_ident(par);
        sym_export(par, e, name, line);
		if (lex_test_skip(par, TK_ASSIGN)) {
			Expr e2 = {};
			l_scan_expr(par, e2);
            expr_write(par, e, e2, line);
		}
	}
}

static void l_scan_stat_pack(Parser* par) {
	Scope scope;

	a_line line = lex_line(par);
    lex_skip(par);
	scope_enter(par, &scope, line, 0);

	l_scan_stats(par);

    lex_check_pair_right(par, TK_LBR, TK_RBR, line);

	scope_leave(par, lex_line(par));
}

static void l_scan_stat(Parser* par) {
	assume(par->scope->top_ntr == par->scope->top_reg, "compute stack leaked.");
	switch (lex_peek(par)) {
		case TK_if: {
			l_scan_if_stat(par);
			break;
		}
        case TK_break: {
            l_scan_break_stat(par);
            break;
        }
        case TK_continue: {
            l_scan_continue_stat(par);
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
            case TK_break: {
                l_scan_break_stat(par);
                break;
            }
            case TK_continue: {
                l_scan_continue_stat(par);
                break;
            }
            case TK_do: {
                l_scan_do_stat(par);
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
		/* Unpack the return value if only return one value, and it can be unpacked. */
		if (e->fupk) {
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

    fscope_prologue(par, &scope, 1);
	if (unlikely(par->options & ALO_COMP_OPT_EVAL)) {
		l_scan_root_return_stat(par);
	}
	else {
		l_scan_stats(par);
	}

	if (!lex_test(par, TK_EOF)) {
		lex_error_got(par, "statement expected");
	}

    fscope_epilogue(par, par->name, lex_line(par));
}

static Impl const parser_impl = {
    .tag = ALO_TPTR,
    .flags = IMPL_FLAG_GREEDY_MARK | IMPL_FLAG_STACK_ALLOC,
    .mark = parser_mark
};

static void parser_init(a_henv env, a_ifun fun, void* ctx, char const* file, GStr* name, a_u32 options, Parser* par) {
    init(par) {
        .impl = &parser_impl,
        .tnext = WHITE_COLOR,
        .options = options,
        .name = name
    };
	ai_lex_init(env, &par->lex, fun, ctx, file);
	rq_init(&par->rq);
}

a_msg ai_parse(a_henv env, a_ifun fun, void* ctx, char const* file, GStr* name, a_u32 options, GFun** pfun) {
	Parser par;
    parser_init(env, fun, ctx, file, name, options, &par);

    Value* p = env->stack.top++;
    v_set_obj(env, p, &par);

	a_msg msg = ai_env_protect(env, l_scan_root, parser_except, &par);

    v_set_nil(--env->stack.top);

	if (msg == ALO_SOK) {
		*pfun = func_build(&par);
	}

	return msg;
}