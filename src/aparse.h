/**
 *@file aparse.h
 */

#ifndef aparse_h_
#define aparse_h_

#include "agc.h"
#include "alex.h"
#include "afun.h"

typedef struct Parser Parser;

intern char const* ai_par_file(Parser* par);
intern a_none ai_par_report(Parser* par, char const* fmt, ...);
intern a_msg ai_parse(a_henv env, a_ifun fun, void* ctx, GStr* file, GStr* name, a_u32 options, GFun** pfun);

#define ai_par_error(par,fmt,ln,args...) ai_par_report(par, "%s:%u: "fmt, ai_par_file(par), ln, ##args)

#endif /* aparse_h_ */
