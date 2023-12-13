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

intern void ai_lex_init(a_henv env, Lexer* lex, a_ifun fun, void* ctx);
intern void ai_lex_open(Lexer* lex);
intern void ai_lex_close(Lexer* lex);
intern char const* ai_lex_tagname(a_i32 tag);
intern char const* ai_lex_tkrepr(Token* tk, a_tkbuf buf);
intern GStr* ai_lex_to_str(Lexer* lex, void const* src, a_usize len);
intern a_i32 ai_lex_forward(Lexer* lex);
intern a_i32 ai_lex_peek(Lexer* lex);
intern a_i32 ai_lex_peek2(Lexer* lex, a_u32 line);

#define ai_lex_error(lex,fmt,args...) ai_par_error(cast(Parser*, lex), fmt, (lex)->_line, ##args)

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
    a_i32 _tag;
    a_line _line;
    union {
        a_int _int;
        a_uint _uint;
        a_float _float;
        GStr* _str;
    };
};

typedef struct {
	GStr** _ptr;
    a_u32 _len;
	a_u32 _hmask;
} StrSet;

struct Lexer {
	union {
		ZIn _in;
		a_henv _env;
	};
    Buf _buf[1];
    a_line _line;
    a_i32 _char; /* Next character. */
    Token _ahead[2];
    StrSet _strs;
};

#endif /* alex_h_ */
