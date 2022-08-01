/**
 *@file aparse.h
 */

#ifndef aparse_h_
#define aparse_h_

#include "agc.h"
#include "alex.h"

#define NO_LABEL (~u32c(0))

typedef struct Parser Parser;
typedef struct Scope Scope;
typedef struct FnScope FnScope;

intern a_msg ai_parse(a_henv env, a_ifun fun, void* ctx, char const* fname, a_u32 options, GFun** pfun);

typedef struct Name Name;
typedef struct Names Names;

typedef struct Line Line;
typedef struct Lines Lines;

struct Names {
	Name* _stack;
	a_u32 _top;
	a_u32 _cap;
};

struct Line {
	a_u32 _begin;
	a_u32 _line;
};

struct Lines {
	Line* _stack;
	a_u32 _top;
	a_u32 _cap;
};

struct Parser {
	union {
		Lexer _lex;
		a_henv _env;
	};
	a_insn* _insns;
	a_u32 _ninsn;
	a_u32 _cinsn;
	a_u32 _options;
	a_u32 _head_jump;
	a_u32 _head_jump_line;
	a_u32 _head_land;
	a_u32 _head_line;
	union {
		a_u32 _flags;
		struct {
			a_u32 _fpass: 1;
			a_u32 _fland: 1;
			a_u32 _fjump: 1;
		};
	};
	a_u16 _scope_depth;
	Names _names;
	Lines _lines;
	Scope* _scope;
	FnScope* _fnscope;
	RefQueue _rq;
};

#define SCOPE_DEPTH_ENV u8c(0)
#define SCOPE_DEPTH_ROOT u8c(1)

#define l_error(par,fmt,ln,args...) \
ai_lex_error_(&(par)->_lex, "%s:%u: "fmt, &(par)->_lex._fname, ln, ##args)

#endif /* aparse_h_ */
