/**
 *@file aload.h
 */

#ifndef aload_h_
#define aload_h_

#include "afun.h"

/* Binary value tags. */
#define LVTAG_NIL 0xff
#define LVTAG_FALSE 0xfe
#define LVTAG_TRUE 0xfd
#define LVTAG_INT 0xfc
#define LVTAG_FLOAT 0xfb
#define LVTAG_LSTR 0xfa

#define LVLSTR_LEN_BIAS LVTAG_LSTR

intern a_msg ai_fun_save(a_henv env, GFun* val, a_ofun fun, void* ctx, a_flags flags);
intern a_msg ai_fun_load(a_henv env, GFun** pval, a_ifun fun, void* ctx, a_flags flags);

#endif /* aload_h_ */
