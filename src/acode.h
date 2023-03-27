/**
 *@file acode.h
 */

#ifndef acode_h_
#define acode_h_

#include "aop.h"
#include "aparse.h"

typedef struct ExprBody ExprBody;
typedef struct ConExpr ConExpr;
typedef struct LetStat LetStat;

typedef ExprBody* restrict InExpr;
typedef ExprBody* restrict OutExpr;
typedef ExprBody* restrict InoutExpr;

intern void ai_code_never(Parser* par, OutExpr e, a_line line);
intern void ai_code_constK(Parser* par, OutExpr e, a_u32 val, a_line line);
intern void ai_code_constI(Parser* par, OutExpr e, a_int val, a_line line);
intern void ai_code_constF(Parser* par, OutExpr e, a_float val, a_line line);
intern void ai_code_constS(Parser* par, OutExpr e, GStr* val, a_line line);
intern void ai_code_loadfunc(Parser* par, OutExpr e, GProto* fun);
intern void ai_code_lookupG(Parser* par, OutExpr e, GStr* name, a_line line);

intern void ai_code_lookupS(Parser* par, InoutExpr e, GStr* name, a_line line);
intern void ai_code_index(Parser* par, InoutExpr ev, InExpr ek, a_line line);
intern void ai_code_new_list(Parser* par, InoutExpr e, a_line line);
intern void ai_code_unary(Parser* par, InoutExpr e, a_enum op, a_line line);
intern void ai_code_binary1(Parser* par, InoutExpr e, a_enum op, a_line line);
intern void ai_code_binary2(Parser* par, InoutExpr e1, InExpr e2, a_enum op, a_line line);
intern void ai_code_merge(Parser* par, InoutExpr e1, InExpr e2, a_u32 label, a_line line);
intern void ai_code_monad(Parser* par, InoutExpr e, a_u32* plabel, a_u32 op, a_line line);
intern a_u32 ai_code_testT(Parser* par, InoutExpr e, a_line line);
intern void ai_code_vararg1(Parser* par, InoutExpr es, a_enum op, a_line line);
intern void ai_code_vararg2(Parser* par, InoutExpr es, InoutExpr e, a_enum op, a_line line);
intern a_u32 ai_code_args_trunc(Parser* par, InoutExpr ei, InoutExpr el, a_u32 n, a_line line);
intern void ai_code_concat_next(Parser* par, ConExpr* ce, InExpr e, a_line line);
intern void ai_code_concat_end(Parser* par, ConExpr* ce, OutExpr e, a_line line);

intern void ai_code_unpack(Parser* par, InoutExpr e, a_line line);

intern void ai_code_gotoD(Parser* par, a_u32 label, a_line line);
intern a_u32 ai_code_gotoU(Parser* par, a_u32 label, a_line line);
intern a_u32 ai_code_label(Parser* par, a_u32 label, a_line line);
intern void ai_code_flush_jump(Parser* par, a_line line);

intern void ai_code_drop(Parser* par, InExpr e);
intern void ai_code_bind(Parser* par, InExpr e1, InExpr e2, a_line line);
intern void ai_code_let_nils(Parser* par, LetStat* s, a_line line);
intern a_bool ai_code_let_bind(Parser* par, LetStat* s, InExpr e);
intern void ai_code_local(Parser* par, OutExpr e, GStr* name, a_line line);
intern void ai_code_bind_param(Parser* par, GStr* name, a_line line);
intern void ai_code_compact(Parser* par);

intern void ai_code_enter(Parser* par, Scope* scope, a_line line);
intern void ai_code_leave(Parser* par, a_line line);
intern void ai_code_prologue(Parser* par, FnScope* fnscope, a_line line);
intern GProto* ai_code_epilogue(Parser* par, GStr* name, a_bool root, a_line line);
intern void ai_code_open(Parser* par);
intern GFun* ai_code_build_and_close(Parser* par);

/**
 ** Volatility:
 ** Volatile expressions are expressions presumed to be destroyed across
 ** any unrelated operations. Nonvolatile expressions are required to
 ** retain the values across any operations.
 */
enum ExprKind {
/*==============================Constants===============================*/
	/**
	 ** Unit expression.
	 ** REPR: unit
	 */
	EXPR_UNIT = 0x00,
	/**
	 ** Nil constant expression.
	 ** REPR: nil
	 */
	EXPR_NIL = 0x01,
	/**
	 ** Boolean constant expression.
	 ** REPR: false/true
	 */
	EXPR_FALSE = 0x02, EXPR_TRUE,
#define EXPR_BOOL(v) (EXPR_FALSE | (v))
	/**
	 ** Integer constant expression.
	 ** REPR: _int
	 *@param _int the integer constant.
	 */
	EXPR_INT = 0x04,
	/**
	 ** Float constant expression.
	 ** REPR: _float
	 *@param _float the float constant.
	 */
	EXPR_FLOAT = 0x05,
	/**
	 ** String constant expression.
	 ** REPR: _str
	 *@param _str the string constant.
	 */
	EXPR_STR = 0x06,
/*==========================Bind Expressions============================*/
	/**
	 ** The expression bind to a capture value.
	 ** REPR: C[_reg]
	 *@param _reg the capture register index.
	 */
	EXPR_CAP = 0x07,
	/**
	 ** The expression from a local variable.
	 ** REPR: R[_reg]
	 *@param _reg the register index.
	 *@param _sym the symbol index.
	 */
	EXPR_VAR = 0x08,
	/**
	 ** The expression bind to a temporary register.
	 ** REPR: R[_reg]
	 *@param _reg the register index.
	 */
	EXPR_TMP = 0x09,
	/**
	 ** Unreachable expression.
	 ** REPR: !
	 */
	EXPR_NEVER = 0x0B,
/*===========================Lazy Expressions===========================*/
	/**
	 ** The value indexed expression. REPR: R[_impl][R[_key]]
	 *@param _base the base register index.
	 *@param _key the key register index.
	 */
	EXPR_REF_ = 0x0C,
#define EXPR_REF(k,v) (EXPR_REF_ | (k) << 1 | (v))
#define EXPR_REFR_ALL EXPR_REF_ ... EXPR_REF_ + 3
	/**
	 ** The integer indexed expression.
	 ** REPR: R[_impl][_key]
	 *@param _base the base register index.
	 *@param _key the integer key.
	 */
	EXPR_REFI_ = 0x10,
#define EXPR_REFI(v) (EXPR_REFI_ | (v))
#define EXPR_REFI_ALL EXPR_REFI_ ... EXPR_REFI_ + 1
	/**
	 ** The constant indexed expression. REPR: R[_impl][K[_key]]
	 *@param _base the base register index.
	 *@param _key the key constant index.
	 */
	EXPR_REFK_ = 0x12,
#define EXPR_REFK(v) (EXPR_REFK_ | (v))
#define EXPR_REFK_ALL EXPR_REFK_ ... EXPR_REFK_ + 1
#define EXPR_REF_ALL EXPR_REF_ ... EXPR_REFK_ + 1
	EXPR_REFCK = 0x14,
/*=========================Partial Expressions==========================*/
	/**
	 ** The partial evaluated expression.
	 ** The output is the register with index A of instruction.
	 ** REPR: R[_label(a)]
	 *@param _label the label of instruction.
	 */
	EXPR_DYN_A = 0x15,
	/**
	 ** The expression of variable length arguments stored in sequence of
	 ** registers.
	 ** REPR: R[_label(a):]
	 *@param _label the label of instruction that generate vararg.
	 */
	EXPR_DYN_AC = 0x16,
	/**
	 ** The calling expression.
	 ** REPR: R[_label(a):_label(a)+_label(c)]
	 *@param _label the label of instruction.
	 */
	EXPR_DYN_C = 0x17,
	/**
	 ** The try expression. This is a volatile expression.
	 ** REPR: try { R[_try] } else { nil }
	 *@param _try the temporary register index.
	 *@param _residual the label of jump instruction.
	 */
	EXPR_TMP_OR_NIL = 0x18,
	/**
	 ** The try expression. This is a volatile expression.
	 ** REPR: try { R[_label(a)] } else { nil }
	 *@param _try the label of compute result instruction.
	 *@param _residual the label of jump instruction.
	 */
	EXPR_DYN_OR_NIL = 0x19,
	/**
	 ** The try expression with boolean type. This is a volatile expression.
	 ** REPR: try { true/false } else { false/true }
	 *@param _label the label of residual path.
	 */
	EXPR_TRY_TRUE = 0x1A, EXPR_TRY_FALSE,
	/**
	 ** The try expression with only residual part.
	 ** REPR: try { ! } else { false/true }
	 *@param _label the label of residual path.
	 */
	EXPR_RESIDUAL_FALSE = 0x1C, EXPR_RESIDUAL_TRUE,

	EXPR_NTMP = 0x1E, EXPR_NTMPC,
/*=========================Pattern Expressions==========================*/
	PAT_DROP,
	PAT_BIND,
	PAT_ROOT,
	PAT_TUPLE,
	PAT_LIST,
	PAT_TABLE
};

static_assert((EXPR_FALSE ^ 1) == EXPR_TRUE);
static_assert((EXPR_TRY_FALSE ^ 1) == EXPR_TRY_TRUE);
static_assert((EXPR_RESIDUAL_FALSE ^ 1) == EXPR_RESIDUAL_TRUE);

typedef struct {
	a_u8 _base;
	a_u8 _top;
	a_u8 _coll; /* The collector index. */
} Args;

struct ExprBody {
	a_u8 _kind;
	a_line _line;
	union {
		a_int _int;
		a_float _float;
		GStr* _str;
		struct {
			a_u32 _reg;
			a_u32 _sym;
		};
		a_u32 _label;
		struct {
			a_u32 _base;
			a_u32 _key;
		};
		struct {
			a_u32 _try;
			a_u32 _residual;
		};
		Args _args;
	};
};

typedef ExprBody Expr[1];

#define expr_init(e,k,v...) quiet(*(e) = new(ExprBody) { ._kind = (k), v })

always_inline void expr_copy(OutExpr dst, InExpr src) {
	*dst = *src;
}

always_inline void expr_never(OutExpr e, a_line line) {
	expr_init(e, EXPR_NEVER, ._line = line);
}

always_inline void expr_unit(OutExpr e) {
	expr_init(e, EXPR_UNIT);
}

always_inline void expr_var(OutExpr e, a_u32 reg, a_u32 sym, a_line line) {
	expr_init(e, EXPR_VAR, ._reg = reg, ._sym = sym, ._line = line);
}

always_inline void expr_tmp(OutExpr e, a_u32 reg, a_line line) {
	expr_init(e, EXPR_TMP, ._reg = reg, ._line = line);
}

always_inline void expr_dyn(OutExpr e, a_u32 label) {
	expr_init(e, EXPR_DYN_A, ._label = label);
}

always_inline a_bool expr_has_tmp_val(InExpr e) {
	a_u32 kind = e->_kind;
	switch (kind) {
		case EXPR_NEVER:
		case EXPR_VAR:
		case EXPR_TMP:
		case EXPR_REF_ALL:
			return kind & 0x1 ? true : false;
		default: panic("expression has no value.");
	}
}

always_inline a_bool expr_has_tmp_key(InExpr e) {
	a_u32 kind = e->_kind;
	switch (kind) {
		case EXPR_REFR_ALL:
			return kind & 0x2 ? true : false;
		default: panic("expression has no value.");
	}
}

struct ConExpr {
	Expr _expr;
	QBuf _buf;
};

typedef struct LetNode LetNode;

struct LetNode {
	LetNode* _child;
	LetNode* _sibling;
	LetNode* _parent;
	union {
		Expr _expr;
		struct {
			a_u8 _kind;
			union {
				a_u8 _flags;
				struct {
					a_u8 _faggr: 1;
				};
			};
			a_line _line;
			a_u32 _count;
		};
	};
	/* Used for register allocation. */
	a_u8 _rel_id;
	a_u8 _ntmp;
	a_u8 _nvar;
	a_u8 _index; /* Index in enclosed pattern. */
};

struct LetStat {
	LetNode _root;
	a_u32 _label_test;
	a_u32 _label_fail;
	a_u8 _ftest: 1;
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
};

enum {
	SYM_MOD_NONE = 0x0000,
	SYM_MOD_READONLY = 0x0001
};

struct Sym {
	a_u8 _kind;
	a_u8 _scope;
	a_u16 _mods;
	a_u32 _index;
	GStr* _name;
};

#define SCOPE_STRUCT_HEAD \
    Scope* _up;           \
	a_u8 _bot_reg;          \
	a_u8 _top_ntr; /* Top of non-temporary section. */ \
	a_u8 _bot_fur; /* Bottom of fragmented section. */ \
	a_u8 _num_fur; /* Number of temporary register in fragmented section. */ \
	a_u8 _top_reg;          \
	a_line _begin_line;      \
	a_u32 _begin_label;      \
	a_u32 _end_label;        \
	a_u32 _sym_off

struct Scope {
	SCOPE_STRUCT_HEAD;
};

struct RichCapInfo {
	a_u8 _scope; /* The depth of first captured scope. */
	a_u8 _sym_index; /* Qualified variable index. */
	a_u8 _src_index;
	a_u16 _mods;
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
		a_u8 _flow_flags;
		struct {
			a_u8 _fpass: 1;
			a_u8 _fland: 1;
			a_u8 _fjump: 1;
			a_u8 _fvarg: 1;
			a_u8 _fclose: 1;
		};
	};
	a_u8 _nparam;
	a_u8 _max_reg;
};

#endif /* acode_h_ */
