/**
 *@file aparse.h
 */

#ifndef aparse_h_
#define aparse_h_

#include "alex.h"
#include "afun.h"
#include "agc.h"

typedef struct Parser Parser;

intern a_msg ai_parse(a_henv env, a_ifun fun, void* ctx, char const* file, GStr* name, a_u32 options, GFun** pfun);

#endif /* aparse_h_ */
