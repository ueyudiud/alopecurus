/**
 *@file alex.h
 */

#ifndef alex_h_
#define alex_h_

#include "akw.h"
#include "aobj.h"
#include "abuf.h"
#include "aio.h"

typedef a_u32 a_line;

typedef struct Lexer Lexer;
typedef struct Token Token;
typedef struct LexScope LexScope;

#define MAX_TOKEN_STR_BUF_SIZE 31

typedef char a_tkbuf[MAX_TOKEN_STR_BUF_SIZE + 1];

intern void ai_lex_init(a_henv env, Lexer* lex, a_ifun fun, void* ctx);
intern void ai_lex_close(Lexer* lex);
intern char const* ai_lex_tagname(a_i32 tag);
intern char const* ai_lex_tkrepr(Token* tk, a_tkbuf buf);
intern GStr* ai_lex_to_str(Lexer* lex, void const* src, a_usize len);
intern a_i32 ai_lex_forward(Lexer* lex);
intern a_i32 ai_lex_peek(Lexer* lex);

#define ai_lex_error(lex,fmt,args...) ai_par_error(cast(Parser*, lex), fmt, (lex)->_line, ##args)

enum {
    TK__NONE,

#define TKDEF(id,repr) TK_##id,
	KW_LIST(TKDEF)
	OP_LIST(TKDEF)
#undef TKDEF
	TK_EOF,
	TK_IDENT,
	TK_INTEGER,
	TK_FLOAT,
	TK_STRING,
	TK_TSTRING,

    TK__MAX
};

enum {
    CHANNEL_NORMAL = 0x0,
    CHANNEL_ISTR_ESCAPE = 0x2,
    CHANNEL_ISTR_BODY = 0x3,
    CHANNEL_MISTR_ESCAPE = 0x4,
    CHANNEL_MISTR_BODY = 0x5
};

static_assert((CHANNEL_ISTR_BODY ^ 1) == CHANNEL_ISTR_ESCAPE);
static_assert((CHANNEL_MISTR_BODY ^ 1) == CHANNEL_MISTR_ESCAPE);

struct Token {
    a_i32 _tag;
    a_line _line;
    union {
        a_int _int;
        a_float _float;
        GStr* _str;
    };
};

typedef struct StrNode StrNode;

struct StrNode { 
    GStr* _str;
	a_x32 _hprev;
	a_x32 _hnext;
};

typedef struct {
	StrNode* _ptr;
    a_u32 _len;
	a_u32 _hmask;
	a_x32 _hfree;
} LexStrs;

struct LexScope {
    LexScope* _up;
    a_u32 _last_channel;
    a_u32 _begin_line;
    union {
        a_u32 _indent; /* For multiline string. */
    };
};

struct Lexer {
	union {
		ZIn _in;
		a_henv _env;
	};
    ByteBuf _buf;
    a_line _line;
    a_i32 _ch; /* Next character. */
    a_u32 _channel;
    Token _current;
	Token _forward;
    LexStrs _strs;
    LexScope* _scope;
    LexScope _scope0;
};

always_inline void ai_lex_push_scope(Lexer* lex, LexScope* scope) {
    scope->_up = lex->_scope;
    lex->_scope = scope;
}

always_inline void ai_lex_pop_scope(Lexer* lex) {
    lex->_scope = lex->_scope->_up;
	lex->_channel = lex->_scope->_last_channel;
}

#endif /* alex_h_ */
