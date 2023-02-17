/**
 *@file alex.h
 */

#ifndef alex_h_
#define alex_h_

#include "akw.h"
#include "abuf.h"
#include "alink.h"
#include "aobj.h"
#include "aio.h"

typedef a_u16 a_line;

typedef struct Lexer Lexer;
typedef struct Token Token;
typedef struct LexScope LexScope;

#define MAX_TOKEN_STR_BUF_SIZE 31

typedef a_byte a_tkbuf[MAX_TOKEN_STR_BUF_SIZE + 1];

intern void ai_lex_init(a_henv env, Lexer* lex, a_ifun fun, void* ctx);
intern void ai_lex_close(Lexer* lex);
intern char const* ai_lex_tagname(a_i32 tag);
intern char const* ai_lex_tkrepr(Token* tk, a_tkbuf buf);
intern GStr* ai_lex_tostr(Lexer* lex, void const* src, a_usize len);
intern a_i32 ai_lex_forward(Lexer* lex);
intern a_i32 ai_lex_peek(Lexer* lex);

#define lex_file(lex) (lex)->_file->_data

#define ai_lex_error(lex,fmt,args...) ai_par_report(from_member(Parser, _lex, lex), false, "%s:%u: "fmt, lex_file(lex), (lex)->_line, ##args)

enum {
    TK__NONE,

#define TKDEF(id,repr) TK_##id,
	KEYWORD_LIST(TKDEF)
	OPERATOR_LIST(TKDEF)
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
    CHANNEL_TSTR_ESCAPE = 0x2,
    CHANNEL_TSTR_BODY = 0x3,
    CHANNEL_MTSTR_ESCAPE = 0x4,
    CHANNEL_MTSTR_BODY = 0x5
};

struct Token {
    a_i16 _tag;
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
	LINK_DEF;
};

typedef struct {
    StrNode* _table;
    a_u32 _hfree;
    a_u32 _hmask;
    a_u32 _nstr;
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
    ZIn _in;
    Buf _buf;
    GStr* _file; /* Source file name. */
    a_line _line;
    a_i16 _ch; /* Next character. */
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
