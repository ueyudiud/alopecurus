/**
 *@file aparse.h
 */

#ifndef aparse_h_
#define aparse_h_

#include "agc.h"
#include "alex.h"
#include "afun.h"

#define NO_LABEL (~u32c(0))

typedef struct Parser Parser;
typedef struct Scope Scope;
typedef struct FnScope FnScope;

intern a_none ai_par_report(Parser* par, a_bool eof, char const* fmt, ...);
intern a_msg ai_parse(a_henv env, a_ifun fun, void* ctx, char const* fname, a_u32 options, GFun** pfun);

typedef struct Sym Sym;
typedef struct Syms Syms;

typedef struct CompCapInfo CompCapInfo;

BUF_STRUCT_DECLARE(InsnBuf, a_insn);
BUF_STRUCT_DECLARE(LineInfoBuf, LineInfo);
BUF_STRUCT_DECLARE(LocalInfoBuf, LocalInfo);
BUF_STRUCT_DECLARE(CapInfoBuf, CompCapInfo);

struct Syms {
	Sym* _stack;
	a_u32 _top;
	a_u32 _cap;
};

struct Parser {
	union {
		Lexer _lex;
		a_henv _env;
	};
	union {
		InsnBuf _insns;
		struct {
			a_insn* _code;
			a_usize _head_label;
		};
	};
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
			a_u32 _fvarg: 1;
		};
	};
	a_u16 _scope_depth;
	Syms _syms;
	LocalInfoBuf _locals;
	LineInfoBuf _lines;
	CapInfoBuf _captures;
	Scope* _scope;
	FnScope* _fnscope;
	RefQueue _rq; /* Function prototype queue. */
	QBuf* _qbq; /* Queued string buffer queue, used for concatenate expression. */
};

#define SCOPE_DEPTH_ENV u8c(0)
#define SCOPE_DEPTH_ROOT u8c(1)

#define par_err_f_arg(par,fmt) "%s: "fmt, (par)->_lex._fname
#define par_err_fl_arg(par,fmt,ln) par_err_f_arg(par, "%u: "fmt), ln

#define ai_par_error(par,fmt,ln,args...) ai_par_report(par, false, par_err_fl_arg(par,fmt,ln), ##args)

#endif /* aparse_h_ */
