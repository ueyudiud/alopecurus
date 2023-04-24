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

intern a_none ai_par_report(Parser* par, char const* fmt, ...);
intern a_msg ai_parse(a_henv env, a_ifun fun, void* ctx, GStr* file, GStr* name, a_u32 options, GFun** pfun);

typedef struct Sym Sym;

typedef struct RichCapInfo RichCapInfo;

BUF_STRUCT_DECLARE(SymBuf, Sym);
BUF_STRUCT_DECLARE(ConstBuf, Value);
BUF_STRUCT_DECLARE(InsnBuf, a_insn);
BUF_STRUCT_DECLARE(LineInfoBuf, LineInfo);
BUF_STRUCT_DECLARE(LocalInfoBuf, LocalInfo);
BUF_STRUCT_DECLARE(CapInfoBuf, RichCapInfo, CapInfoBuf* _last);

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
	a_u8 _scope_depth;
	SymBuf _syms;
	ConstBuf _consts; /* Constants. */
	LocalInfoBuf _locals;
	LineInfoBuf _lines;
	GStr* _file; /* Source file name. */
	GStr* _name;
	Scope* _scope;
	FnScope* _fnscope;
	RefQueue _rq; /* Function prototype queue. */
	QBuf* _qbq; /* Queued string buffer queue, used for concatenate expression. */
};

#define SCOPE_DEPTH_ENV u8c(0)
#define SCOPE_DEPTH_ROOT u8c(1)

always_inline char const* par_file(Parser* par) {
	return par->_file != null ? str2ntstr(par->_file) : "<in>";
}

#define ai_par_error(par,fmt,ln,args...) ai_par_report(par, "%s:%u: "fmt, par_file(par), ln, ##args)

#endif /* aparse_h_ */
