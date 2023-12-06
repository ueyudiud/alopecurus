/**
 *@file aload.h
 */

#ifndef aload_h_
#define aload_h_

#include "afun.h"

/* Binary value tags. */
enum {
    LVTAG_NIL = 0xff,
    LVTAG_FALSE = 0xfe,
    LVTAG_TRUE = 0xfd,
    LVTAG_INT = 0xfc,
    LVTAG_FLOAT = 0xfb,
    LVTAG_LSTR = 0xfa,

    LVLSTR_LEN_BIAS = LVTAG_LSTR
};

intern a_msg ai_fun_save(a_henv env, GFun* val, a_ofun fun, void* ctx, a_flags flags);
intern a_msg ai_fun_load(a_henv env, GFun** pval, a_ifun fun, void* ctx, a_flags flags);

enum { CHUNK_HEADER_SIZE = 4 };

intern char const ai_fun_header[CHUNK_HEADER_SIZE];

#endif /* aload_h_ */
