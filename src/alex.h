/**
 *@file alex.h
 */

#ifndef alex_h_
#define alex_h_

#include "aio.h"
#include "abuf.h"
#include "aobj.h"

typedef struct Lexer Lexer;
typedef struct Token Token;
typedef struct LexScope LexScope;

#define KEYWORD_LIST(_) \
    _(_ENV, "_ENV")     \
    _(AS, "as")         \
    _(BREAK, "break")   \
    _(CONST, "const")   \
    _(CONTINUE, "continue") \
    _(DO, "do")         \
    _(ELSE, "else")     \
    _(EXPORT, "export") \
    _(FALSE, "false")   \
    _(FOR, "for")       \
    _(IF, "if")         \
    _(IMPORT, "import") \
    _(IN, "in")         \
    _(IS, "is")         \
    _(LET, "let")       \
    _(LOOP, "loop")     \
    _(MATCH, "match")   \
    _(NIL, "nil")       \
    _(TRUE, "true")     \
	_(WHILE, "while")

#define TOKEN_LIST(_) \
    _(EOF, "<eof>") \
    KEYWORD_LIST(_) \
    _(IDENT, "<identifier>") \
    _(INTEGER, "<integer>") \
    _(FLOAT, "<float>") \
    _(STRING, "<string>") \
    _(TSTRING, "<tstring>") \
    _(LBK, "'('") \
    _(RBK, "')'") \
    _(LSQ, "'['") \
    _(RSQ, "']'") \
    _(LBR, "'{'") \
    _(RBR, "'}'") \
    _(SHARP, "'#'") \
    _(AT, "'@'") \
    _(TILDE, "'~'") \
    _(COMMA, "','") \
    _(DOT, "'.'") \
    _(BDOT, "'..'") \
    _(TDOT, "'...'") \
    _(COLON, "':'") \
    _(BCOLON, "'::'") \
    _(PLUS, "'+'") \
    _(MINUS, "'-'") \
    _(STAR, "'*'") \
    _(LSLASH, "'/'") \
    _(PERCENT, "'%'") \
    _(ASSIGN, "'='") \
    _(EQ, "'=='") \
    _(NOT, "'!'") \
    _(NE, "'!='") \
    _(GT, "'>'") \
    _(GE, "'>='") \
    _(SHL, "'<<'") \
    _(LT, "'<'") \
    _(LE, "'<='") \
    _(SHR, "'>>'") \
    _(HAT, "'^'") \
    _(AMP, "'&'") \
    _(BAMP, "'&&'") \
    _(BAR, "'|'") \
    _(BBAR, "'||'") \
    _(QUESTION, "'?'") \
    _(QDOT, "'?.'") \
    _(ELVIS, "'?:'")

enum {
    TK__NONE,
#define TKDEF(id,repr) TK_##id,
    TOKEN_LIST(TKDEF)
#undef TKDEF
    TK__MAX
};

enum {
    CHANNEL_NORMAL = 0x0,
    CHANNEL_TSTR_ESCAPE = 0x2,
    CHANNEL_TSTR_BODY = 0x3,
    CHANNEL_MTSTR_ESCAPE = 0x4,
    CHANNEL_MTSTR_BODY = 0x5
};

intern void ai_lex_init(Lexer* lex, char const* fname);
intern a_none ai_lex_error_(Lexer* lex, char const* fmt, ...);
intern a_i32 ai_lex_forward(Lexer* lex);
intern a_i32 ai_lex_peek(Lexer* lex);

#define ai_lex_error(lex,fmt,args...) ai_lex_error_(lex, "%s:%u: "fmt, (lex)->_fname, (lex)->_line, ##args)

struct Token {
    a_i32 _tag;
    a_u32 _line;
    union {
        a_int _int;
        a_float _float;
        GStr* _str;
    };
};

typedef struct StrNode StrNode;

struct StrNode { 
    GStr* _str;
    a_u32 _prev;
    a_u32 _next;
};

typedef struct {
    StrNode* _table;
    a_u32 _hfree;
    a_u32 _hmask;
    a_u32 _nstr;
} Strs;

struct LexScope {
    LexScope* _up;
    a_u32 _last_channel;
    a_u32 _line;
    union {
        a_u32 _indent;
    };
};

struct Lexer {
    ZIn _in;
    Buf _buf;
    char const* _fname; /* Source file name. */
    a_u32 _line;
    a_i32 _ch;
    a_u32 _channel;
    Token _current;
	Token _forward;
    Strs _strs;
    LexScope* _scope;
    LexScope _scope0;
};

inline void ai_lex_push_scope(Lexer* lex, LexScope* scope) {
    scope->_up = lex->_scope;
    lex->_scope = scope;
}

inline void ai_lex_pop_scope(Lexer* lex) {
    lex->_scope = lex->_scope->_up;
}

#endif /* alex_h_ */
