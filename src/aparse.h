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
typedef struct CapInfo CapInfo;
typedef struct Parser Parser;

BUF_STRUCT_DECLARE(SymBuf, Sym);
BUF_STRUCT_DECLARE(ConstBuf, Value);
BUF_STRUCT_DECLARE(InsnBuf, a_insn);
BUF_STRUCT_DECLARE(LineInfoBuf, LineInfo);
BUF_STRUCT_DECLARE(LocalInfoBuf, LocalInfo);
BUF_STRUCT_DECLARE(CapInfoBuf, CapInfo);

#define SCOPE_STRUCT_HEAD \
    Scope* upscope;       \
    GStr* label_name;     \
	a_u8 bot_reg; /* The bottom of scope. */ \
	a_u8 top_pin; /* Top of pinned register. */ \
	a_u8 top_reg; /* The top of as_scope. */ \
    a_u8 jmp_prop; /* The jump properties. */\
	a_line begin_line;      \
	a_u32 begin_label;      \
	a_u32 end_label;        \
	a_u32 sym_off

struct Scope {
    SCOPE_STRUCT_HEAD;
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

struct Parser {
    GOBJ_STRUCT_HEADER;
    union {
        Lexer lex;
        a_henv env;
    };
    union {
        InsnBuf insns;
        struct {
            a_insn (*code)[1];
            a_usize head_label;
        };
    };
    a_u32 options;
    a_u8 scope_depth;
    SymBuf syms;
    ConstBuf consts; /* Constants. */
    LocalInfoBuf locals;
    LineInfoBuf lines;
    Buf secs[1];
    Buf sbuf[1];
    GStr* name; /* For debug name. */
    Scope* scope;
    FnScope* fscope;
    RefQueue rq; /* Function prototype queue. */
    GFun** pout; /* Slot of output. */
    FnScope root_scope;
    a_u32 gvar_index;
};

#define lex(par) (&(par)->lex)

#define parse_error(par,fmt,args...) ai_lex_error(lex(par), fmt, args)

#define NO_LABEL (~u32c(0))
#define NIL_SEC_REF (~u32c(0))

#endif /* aparse_h_ */
