/**
 *@file acode.h
 */

#ifndef acode_h_
#define acode_h_

#include "aparse.h"
#include "afun.h"

typedef struct Expr Expr;
typedef struct ExprPack ExprPack;
typedef struct ConExpr ConExpr;
typedef struct LetStat LetStat;

typedef Expr* InExpr;
typedef Expr* OutExpr;
typedef Expr* InoutExpr;

enum {
	OP__NONE    = 0x00,

	OP_ADD      = 0x01,
	OP_SUB      = 0x02,
	OP_MUL      = 0x03,
	OP_DIV      = 0x04,
	OP_MOD      = 0x05,
	OP_SHL      = 0x06,
	OP_SHR      = 0x07,
	OP_BIT_AND  = 0x08,
	OP_BIT_OR   = 0x09,
	OP_BIT_XOR  = 0x0A,
	OP_AND      = 0x0B,
	OP_OR       = 0x0C,

	OP_LT       = 0x10,
	OP_GE       = 0x11,
	OP_LE       = 0x12,
	OP_GT       = 0x13,
	OP_EQ       = 0x14,
	OP_NE       = 0x15,
	OP_IS       = 0x16,
	OP_IS_NOT   = 0x17,
	OP_AS       = 0x18,
	OP_AS_OR    = 0x19,
	OP_IN       = 0x1A,
	OP_NOT_IN   = 0x1B,

	OP_NEG      = 0x0D,
	OP_BIT_INV  = 0x0E,
	OP_NOT      = 0x0F,
	OP_UNBOX    = 0x1C,
	OP_UNPACK   = 0x34,

	OP_OPTION   = 0x1D,
	OP_MERGE    = 0x1E,
	OP_OR_ELSE  = 0x1F,

	OP_VA_PUSH  = 0x20,
	OP_VA_POP   = 0x21,
	OP_RETURN   = 0x22,

	OP_VA_FIT   = 0x30,
	OP_VA_FILL  = 0x31,
	OP_TNEW     = 0x32,
	OP_CALL     = 0x33
};

intern void ai_code_never(Parser* par, OutExpr e, a_u32 line);
intern void ai_code_constK(Parser* par, OutExpr e, a_u32 val, a_u32 line);
intern void ai_code_constI(Parser* par, OutExpr e, a_int val, a_u32 line);
intern void ai_code_constF(Parser* par, OutExpr e, a_float val, a_u32 line);
intern void ai_code_constS(Parser* par, OutExpr e, GStr* val, a_u32 line);
intern void ai_code_lookupU(Parser* par, OutExpr e, GStr* name, a_u32 line);

intern void ai_code_lookupC(Parser* par, InoutExpr e, GStr* name, a_u32 line);
intern void ai_code_index(Parser* par, InoutExpr ev, InExpr ek, a_u32 line);
intern void ai_code_unary(Parser* par, InoutExpr e, a_u32 op, a_u32 line);
intern void ai_code_binary1(Parser* par, InoutExpr e, a_u32 op, a_u32 line);
intern void ai_code_binary2(Parser* par, InoutExpr e1, InExpr e2, a_u32 op, a_u32 line);
intern void ai_code_merge(Parser* par, InoutExpr e1, InExpr e2, a_u32 label, a_u32 line);
intern void ai_code_monad(Parser* par, InoutExpr e, a_u32* plabel, a_u32 op, a_u32 line);
intern a_u32 ai_code_testT(Parser* par, InoutExpr e, a_u32 line);
intern void ai_code_multi(Parser* par, InoutExpr es, InoutExpr e, a_u32 op, a_u32 line);
intern a_bool ai_code_balance(Parser* par, InoutExpr es, InoutExpr e, a_u32 n, a_u32 line);
intern void ai_code_concat_next(Parser* par, ConExpr* ce, InExpr e, a_u32 line);
intern void ai_code_concat_end(Parser* par, ConExpr* ce, OutExpr e, a_u32 line);

intern void ai_code_gotoD(Parser* par, a_u32 label, a_u32 line);
intern a_u32 ai_code_gotoU(Parser* par, a_u32 label, a_u32 line);
intern a_u32 ai_code_label(Parser* par, a_u32 label, a_u32 line);
intern void ai_code_flush_jump(Parser* par, a_u32 line);

intern void ai_code_drop(Parser* par, InExpr e);
intern void ai_code_bind(Parser* par, InExpr e1, InExpr e2, a_u32 line);
intern void ai_code_let_init(Parser* par, LetStat* s);
intern void ai_code_let_push(Parser* par, LetStat* s, GStr* name);
intern void ai_code_let_nils(Parser* par, LetStat* s, a_u32 line);
intern a_bool ai_code_let_bind(Parser* par, LetStat* s, InExpr e);

intern void ai_code_enter(Parser* par, Scope* scope);
intern void ai_code_leave(Parser* par);
intern void ai_code_prologue(Parser* par, FnScope* fnscope);
intern GFunMeta* ai_code_epilogue(Parser* par, a_bool root, a_u32 line);
intern void ai_code_open(Parser* par);
intern GFun* ai_code_build(Parser* par);
intern void ai_code_close(Parser* par);

/**
 ** Volatility:
 ** Volatile expressions are expressions presumed to be destroyed across
 ** any unrelated operations. Nonvolatile expressions are required to
 ** retain the values across any operations.
 */
enum {
/*==========================Duality Expression==========================*/
	/**
	 ** Unit expression.
	 ** REPR: unit
	 */
	EXPR_UNIT,
	/**
	 ** Unreachable expression.
	 ** REPR: !
	 */
	EXPR_NEVER,
	/**
	 ** False constant expression.
	 ** REPR: false
	 */
	EXPR_FALSE,
	/**
	 ** True constant expression.
	 ** REPR: true
	 */
	EXPR_TRUE,
	/**
	 ** The try expression with boolean type. This is a volatile expression.
	 ** REPR: try { true } else { false }
	 *@param _label the label of residual path.
	 */
	EXPR_TRY_TF,
	/**
	 ** The try expression with boolean type. This is a volatile expression.
	 ** REPR: try { false } else { true }
	 *@param _label the label of residual path.
	 */
	EXPR_TRY_FT,
	/**
	 ** The try expression. This is a volatile expression.
	 ** REPR: try { R[_whent] } else { nil }
	 *@param _whent the register index.
	 *@param _whenf the label of jump instruction.
	 */
	EXPR_TRY_RN,
	/**
	 ** The try expression. This is a volatile expression.
	 ** REPR: try { R[_label(a)] } else { nil }
	 *@param _whent the label of compute result instruction.
	 *@param _whenf the label of jump instruction.
	 */
	EXPR_TRY_AN,
	/**
	 ** The try expression with only residual part.
	 ** REPR: try { ! } else { false }
	 *@param _label the label of residual path.
	 */
	EXPR_RESIDUAL_FALSE,
	/**
	 ** The try expression with only residual part.
	 ** REPR: try { ! } else { true }
	 *@param _label the label of residual path.
	 */
	EXPR_RESIDUAL_TRUE,
/*==============================Constants===============================*/
	/**
	 ** Nil constant expression.
	 ** REPR: nil
	 */
	EXPR_NIL,
	/**
	 ** Integer constant expression.
	 ** REPR: _int
	 *@param _int the integer constant.
	 */
	EXPR_INT,
	/**
	 ** Float constant expression.
	 ** REPR: _float
	 *@param _float the float constant.
	 */
	EXPR_FLOAT,
	/**
	 ** String constant expression.
	 ** REPR: _str
	 *@param _str the string constant.
	 */
	EXPR_STR,
/*===========================Lazy Expressions===========================*/
	EXPR_REF,
	/**
	 ** The integer indexed expression.
	 ** REPR: _base[_key]
	 *@param _base the base register index.
	 *@param _key the integer key.
	 */
	EXPR_REFI,
	EXPR_REFK,
	EXPR_CREFK,
/*=========================Partial Expressions==========================*/
	/**
	 ** The partial evaluated expression.
	 ** The output is the register with index A of instruction.
	 ** REPR: R[_label(a)]
	 *@param _label the label of instruction.
	 */
	EXPR_DST_A,
	/**
	 ** The partial evaluated expression, used for function applying.
	 ** REPR: R[_label(a):_label(a)+_label(c)]
	 *@param the label of instruction.
	 */
	EXPR_DST_AC,
	/**
	 ** The partial evaluated expression, used for function applying.
	 ** Different from EXPR_DST_AC, variable a is immutable.
	 ** REPR: R[_label(a):_label(a)+_label(c)]
	 *@param the label of instruction.
	 */
	EXPR_DST_C,
/*==========================Result Expressions==========================*/
	/**
	 ** The expression bind to a register.
	 ** REPR: R[_reg]
	 *@param _reg the register index.
	 */
	EXPR_REG,
	/**
	 ** The expression bind to a capture value.
	 ** REPR: C[_reg]
	 *@param _reg the capture register index.
	 */
	EXPR_CAP,
	/**
	 ** The variable length sequence of registers.
	 ** REPR: R[_base:_base+_len]
	 *@param _pack the register pack.
	 */
	EXPR_PACK
};

struct ExprPack {
	a_u32 _base;
	a_u32 _len;
};

struct Expr {
	a_u32 _kind;
	a_u32 _line;
	union {
		a_int _int;
		a_float _float;
		GStr* _str;
		a_u32 _reg;
		a_u32 _label;
		ExprPack _pack;
		struct {
			a_u32 _base;
			a_u32 _key;
		} _ref;
		struct {
			a_u32 _whent;
			a_u32 _whenf;
		} _cond;
	};
};

struct ConExpr {
	ExprPack _head;
	QBuf _buf;
};

struct LetStat {
	a_u32 _count;
	a_u32 _local_head;
	a_u32 _index;
};

enum {
	/**
	 ** Local variable.
	 *@param _index the register index.
	 */
	NAME_LOCAL
};

struct Name {
	a_u32 _kind;
	a_u32 _index;
	a_u16 _scope;
	GStr* _name;
};

#define SCOPE_STRUCT_HEAD \
    Scope* _up;           \
	a_u16 _bot_reg; \
	a_u16 _top_ntr; /* Top of non-temporary section. */ \
	a_u16 _bot_fur; /* Bottom of fragmented section. */ \
	a_u16 _num_fur; /* Number of temporary register in fragmented section. */ \
	a_u16 _top_reg; \
	a_u32 _begin_label; \
	a_u32 _end_label;        \
	a_u32 _bot_name

struct Scope {
	SCOPE_STRUCT_HEAD;
};

typedef struct ValBuf ValBuf;

struct ValBuf {
	Value* _dat;
	a_u32 _cap;
	a_u32 _len;
};

typedef struct LocalInfo LocalInfo;
typedef struct LocalInfos LocalInfos;
typedef struct CapInfo CapInfo;
typedef struct CapInfos CapInfos;

struct LocalInfo {
	GStr* _name;
	a_u32 _begin_label;
	a_u32 _end_label;
	a_u8 _reg;
};

struct LocalInfos {
	LocalInfo* _dat;
	a_u32 _cap;
	a_u32 _len;
};

struct CapInfo {
	a_u8 _scope; /* The depth of first captured scope. */
	a_u8 _iname; /* Qualified variable index. */
	a_u8 _index;
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
	ValBuf _consts; /* Constants. */
	LocalInfos _locals;
	CapInfo* _caps;
	GFunMeta** _base_subs;
	a_u16 _ncap;
	a_u16 _ccap;
	a_u16 _max_reg;
};

#endif /* acode_h_ */
