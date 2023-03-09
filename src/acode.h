/**
 *@file acode.h
 */

#ifndef acode_h_
#define acode_h_

#include "aop.h"
#include "aparse.h"

typedef struct Expr Expr;
typedef struct ExprPack ExprPack;
typedef struct ConExpr ConExpr;
typedef struct LetStat LetStat;

typedef Expr* restrict InExpr;
typedef Expr* restrict OutExpr;
typedef Expr* restrict InoutExpr;

intern void ai_code_never(Parser* par, OutExpr e, a_line line);
intern void ai_code_constK(Parser* par, OutExpr e, a_u32 val, a_line line);
intern void ai_code_constI(Parser* par, OutExpr e, a_int val, a_line line);
intern void ai_code_constF(Parser* par, OutExpr e, a_float val, a_line line);
intern void ai_code_constS(Parser* par, OutExpr e, GStr* val, a_line line);
intern void ai_code_loadfunc(Parser* par, OutExpr e, GProto* fun);
intern void ai_code_lookupG(Parser* par, OutExpr e, GStr* name, a_line line);

intern void ai_code_lookupS(Parser* par, InoutExpr e, GStr* name, a_line line);
intern void ai_code_index(Parser* par, InoutExpr ev, InExpr ek, a_line line);
intern void ai_code_new_tuple(Parser* par, InoutExpr e, a_line line);
intern void ai_code_new_list(Parser* par, InoutExpr e, a_line line);
intern void ai_code_unary(Parser* par, InoutExpr e, a_u32 op, a_line line);
intern void ai_code_binary1(Parser* par, InoutExpr e, a_u32 op, a_line line);
intern void ai_code_binary2(Parser* par, InoutExpr e1, InExpr e2, a_u32 op, a_line line);
intern void ai_code_merge(Parser* par, InoutExpr e1, InExpr e2, a_u32 label, a_line line);
intern void ai_code_monad(Parser* par, InoutExpr e, a_u32* plabel, a_u32 op, a_line line);
intern void ai_code_call(Parser* par, InExpr e, a_line line);
intern void ai_code_return(Parser* par, InExpr e, a_line line);
intern a_u32 ai_code_testT(Parser* par, InoutExpr e, a_line line);
intern a_bool ai_code_balance(Parser* par, InoutExpr es, InoutExpr e, a_u32 n, a_line line);
intern void ai_code_va_push(Parser* par, InoutExpr e1, InExpr e2, a_line line);
intern void ai_code_va_pop(Parser* par, InoutExpr es, InoutExpr e, a_line line);
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
intern void ai_code_discard(Parser* par);

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
	/**
	 ** Unit expression.
	 ** REPR: unit
	 */
	EXPR_UNIT = 0x00,
	/**
	 ** The sequence of temporary registers.
	 ** REPR: R[_impl:_impl+_len]
	 *@param _pack the register pack.
	 */
	EXPR_PACK = 0x01,
	/**
	 ** Boolean constant expression.
	 ** REPR: false/true
	 */
	EXPR_FALSE = 0x02, EXPR_TRUE,
#define EXPR_BOOL(v) (EXPR_FALSE | (v))
	/**
	 ** The try expression with boolean type. This is a volatile expression.
	 ** REPR: try { true/false } else { false/true }
	 *@param _label the label of residual path.
	 */
	EXPR_TRY_TRUE = 0x04, EXPR_TRY_FALSE,
	/**
	 ** The try expression with only residual part.
	 ** REPR: try { ! } else { false/true }
	 *@param _label the label of residual path.
	 */
	EXPR_RESIDUAL_FALSE = 0x06, EXPR_RESIDUAL_TRUE,
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
	 ** The expression bind to a capture value.
	 ** REPR: C[_reg]
	 *@param _reg the capture register index.
	 */
	EXPR_CAP = 0x0A,
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
	/**
	 ** The try expression. This is a volatile expression.
	 ** REPR: try { R[_try] } else { nil }
	 *@param _try the temporary register index.
	 *@param _residual the label of jump instruction.
	 */
	EXPR_TMP_OR_NIL = 0x15,
	/**
	 ** The try expression. This is a volatile expression.
	 ** REPR: try { R[_label(a)] } else { nil }
	 *@param _try the label of compute result instruction.
	 *@param _residual the label of jump instruction.
	 */
	EXPR_DYN_OR_NIL = 0x16,
/*==============================Constants===============================*/
	/**
	 ** Nil constant expression.
	 ** REPR: nil
	 */
	EXPR_NIL = 0x17,
	/**
	 ** Integer constant expression.
	 ** REPR: _int
	 *@param _int the integer constant.
	 */
	EXPR_INT = 0x18,
	/**
	 ** Float constant expression.
	 ** REPR: _float
	 *@param _float the float constant.
	 */
	EXPR_FLOAT = 0x19,
	/**
	 ** String constant expression.
	 ** REPR: _str
	 *@param _str the string constant.
	 */
	EXPR_STR = 0x1A,
/*=========================Partial Expressions==========================*/
	/**
	 ** The partial evaluated expression.
	 ** The output is the register with index A of instruction.
	 ** REPR: R[_label(a)]
	 *@param _label the label of instruction.
	 */
	EXPR_DYN = 0x1B,
	/**
	 ** The partial evaluated expression, used for function applying.
	 ** REPR: R[_label(a):_label(a)+_label(c)]
	 *@param _label the label of instruction.
	 */
	EXPR_VA_DYN = 0x1C,
	/**
	 ** The partial evaluated expression, used for function applying.
	 ** Different from EXPR_VA_DYN, variable a is immutable.
	 ** REPR: R[_label(a):_label(a)+_label(c)]
	 *@param _label the label of instruction.
	 */
	EXPR_VARG = 0x1D,
/*=========================Pattern Expressions==========================*/
	PAT_DROP,
	PAT_BIND,
	PAT_TUPLE,
	PAT_LIST,
	PAT_DICT
};

struct ExprPack {
	a_u32 _base;
	a_u32 _len;
};

struct Expr {
	a_u16 _kind;
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
		ExprPack _pack;
		struct {
			a_u32 _base;
			a_u32 _key;
		};
		struct {
			a_u32 _try;
			a_u32 _residual;
		};
	};
};

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
	ExprPack _head;
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
			a_u16 _kind;
			a_line _line;
			a_u32 _succ_tag; /* Successive tag. */
		};
	};
};

struct LetStat {
	LetNode* _head;
	a_u32 _label_test;
	a_u32 _label_fail;
	a_u32 _nnode;
	a_u32 _fvarg : 1;
	a_u32 _ftest : 1;
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
	a_u16 _bot_reg;          \
	a_u16 _top_ntr; /* Top of non-temporary section. */ \
	a_u16 _bot_fur; /* Bottom of fragmented section. */ \
	a_u16 _num_fur; /* Number of temporary register in fragmented section. */ \
	a_u16 _top_reg;          \
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
	a_u16 _max_reg;
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
};

#endif /* acode_h_ */
