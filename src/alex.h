/**
 *@file alex.h
 */

#ifndef alex_h_
#define alex_h_

#include "aobj.h"
#include "abuf.h"
#include "aio.h"

typedef a_u32 a_line;

typedef struct Lexer Lexer;
typedef struct Token Token;

#define MAX_TOKEN_STR_BUF_SIZE 31

typedef char a_tkbuf[MAX_TOKEN_STR_BUF_SIZE + 1];

intern void ai_lex_init(a_henv env, Lexer* lex, a_ifun fun, void* ctx, char const* file);
intern void ai_lex_open(Lexer* lex, a_flags options);
intern void ai_lex_close(Lexer* lex);
intern char const* ai_lex_tagname(a_i32 tag);
intern char const* ai_lex_tkrepr(Token* tk, a_tkbuf buf);
intern a_noret ai_lex_report(Lexer* lex, char const* fmt, ...);
intern GStr* ai_lex_to_str(Lexer* lex, void const* src, a_usize len);
intern a_i32 ai_lex_forward(Lexer* lex);
intern a_i32 ai_lex_peek(Lexer* lex);
intern a_i32 ai_lex_peek2(Lexer* lex, a_u32 line);

#define ai_lex_error(lex,fmt,ln,args...) ai_lex_report(lex, "%s:%u: "fmt, (lex)->file, ln, ##args)

enum {
    TK__NONE,

    TK__FIRST,
    TK__STUB = TK__FIRST - 1,

#define STRDEF(n) TK_##n,
#define STRDEF2(n,r) TK_##n,
# include "asym/kw.h"
# include "asym/op.h"
#undef STRDEF
#undef STRDEF2

    TK__MAX
};

static_assert(cast(a_u32, TK_if) == cast(a_u32, STR_if));

struct Token {
    a_i32 tag;
    a_line line;
    union {
        a_int as_int;
        a_uint as_uint;
        a_float as_float;
        GStr* as_str;
    };
};

typedef struct {
	GStr** ptr;
    a_u32 len;
	a_u32 hmask;
} StrSet;

struct Lexer {
	union {
		ZIn in;
		a_henv env;
	};
    Buf buf[1];
    char const* file;
    a_line line;
    a_i32 _char; /* Next character. */
    Token ahead[2];
    StrSet strs;
};

#endif /* alex_h_ */
