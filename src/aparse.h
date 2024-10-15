/**
 *@file aparse.h
 */

#ifndef aparse_h_
#define aparse_h_

#include "alex.h"

intern a_msg ai_parse(a_henv env, a_ifun fun, void* ctx, char const* file, GStr* name, a_u32 options, GFun** pfun);

typedef struct Sym Sym;
typedef struct FnScope FnScope;
typedef struct Scope Scope;
typedef struct RichCapInfo RichCapInfo;
typedef struct Parser Parser;

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
    GStr* name; /* For debug name. */
    GStr* gvar_name;
    Scope* scope;
    FnScope* fscope;
    RefQueue rq; /* Function prototype queue. */
};

#define lex(par) (&(par)->lex)

#define parse_error(par,fmt,args...) ai_lex_error(lex(par), fmt, args)

#define NO_LABEL (~u32c(0))
#define NIL_SEC_REF (~u32c(0))

typedef struct ExprDesc ExprDesc;
typedef struct ConExpr ConExpr;
typedef struct PatInfo PatInfo;

typedef ExprDesc* restrict InExpr;
typedef ExprDesc* restrict OutExpr;
typedef ExprDesc* restrict InoutExpr;

enum ConstTag {
    CONST_UNIT,
    /**
     ** Nil constant expression.
     */
    CONST_NIL,
    /**
     ** Boolean constant expression.
     */
    CONST_FALSE, CONST_TRUE,
    /**
     ** Integer constant expression.
     *@param as_int the integer constant.
     */
    CONST_INT,
    /**
     ** Float constant expression.
     *@param as_float the float constant.
     */
    CONST_FLOAT,
    /**
     ** String constant expression.
     *@param as_str the string constant.
     */
    CONST_STR,
    /* Count of constant variant */
    CONST__COUNT
};

typedef struct {
    a_int as_int;
    a_float as_float;
    GStr* as_str;
} UData;

typedef struct {
    UData dat;
    a_u8 tag;
} TData;

/**
 ** Volatile expressions are expressions presumed to be destroyed across
 ** any unrelated operations. Nonvolatile expressions are required to
 ** retain the values across any operations.
 */
enum ExprTag {
/*==========================Bind Expressions============================*/
    /**
     ** The expression from a local variable.
     ** REPR: R[udat1]
     *@param _d1 the register index.
     *@param _d2 the symbol index.
     *@param _fval true if value needs drop.
     *@param _fsym true if it is a named register.
     */
    EXPR_REG = CONST__COUNT,
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
        struct {
            union {
                a_int idat;
                a_float ndat;
                GStr* sdat;
                UData udat;
                struct { /* Universal data */
                    a_u32 udat1;
                    a_u32 udat2;
                };
            };
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
            a_line line;
        };
        TData tdat;
    };
};

typedef ExprDesc Expr[1];

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
     *@param index the register index.
     */
    SYM_LOCAL = CONST__COUNT,
    /**
     ** The top capture value.
     *@param index the capture index in top as_scope.
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
    union {
        struct {
            union {
                a_u32 index; /* Variant uses for different symbol tag. */
                UData udat;
            };
            a_u8 tag; /* The tag of symbol kind. */
            a_u8 scope; /* The as_scope of symbol belongs to. */
            a_u8 status; /* The status of symbol. */
            SymMods mods; /* The modifiers of symbol. */
        };
        TData tdat;
    };
    GStr* name; /* The symbol name. */
};

#define JMP_PROP_BREAK 0x01
#define JMP_PROP_CONTINUE 0x02
#define JMP_PROP_BOUND 0x04

#define SCOPE_STRUCT_HEAD \
    Scope* upscope;       \
    GStr* label_name;     \
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

#endif /* aparse_h_ */
