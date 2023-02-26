/**
 *@file aio.h
 */

#ifndef aio_h_
#define aio_h_

#include "aobj.h"

typedef struct {
    a_henv _env;
    a_ifun _fun;
    void* _ctx;
    a_byte const* _ptr;
    a_usize _len;
	a_i32 _err;
} ZIn;

typedef struct {
    a_henv _env;
    a_ofun _fun;
    void* _ctx;
	a_i32 _err;
} ZOut;

intern void ai_io_iinit(a_henv env, a_ifun fun, void* ctx, ZIn* in);
intern a_i32 ai_io_igetc(ZIn* in);
intern a_msg ai_io_iget(ZIn* in, void* dst, a_usize len);

intern void ai_io_oinit(a_henv env, a_ofun fun, void* ctx, ZOut* out);
intern a_msg ai_io_oput(ZOut* out, void const* src, a_usize len);

#endif /* aio_h_ */
