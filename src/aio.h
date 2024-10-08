/**
 *@file aio.h
 */

#ifndef aio_h_
#define aio_h_

#include "adef.h"

typedef struct ZIn ZIn;
typedef struct ZOut ZOut;

intern void ai_io_iinit(a_henv env, a_ifun fun, void* ctx, ZIn* in);
intern a_i32 ai_io_igetc(ZIn* in);
intern a_msg ai_io_iget(ZIn* in, void* dst, a_usize len);

intern void ai_io_oinit(a_henv env, a_ofun fun, void* ctx, ZOut* out);
intern a_msg ai_io_oput(ZOut* out, void const* src, a_usize len);

struct ZIn {
    a_henv env;
    a_ifun fun;
    void* ctx;
    a_byte const* ptr;
    a_usize len;
    a_i32 err;
};

struct ZOut {
    a_henv env;
    a_ofun fun;
    void* ctx;
    a_i32 err;
};

#endif /* aio_h_ */
