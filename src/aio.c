/**
 *@file aio.c
 */

#define aio_c_

#include <string.h>

#include "amem.h"
#include "agc.h"

#include "aio.h"

void ai_io_iinit(a_henv env, a_ifun fun, void* ctx, ZIn* in) {
    *in = new(ZIn) {
        ._env =  env,
        ._fun =  fun,
        ._ctx =  ctx,
        ._ptr =  null,
        ._len =  0,
        ._err =  0
    };
}

static a_msg in_fetch(ZIn* in) {
    assume(in->_len == 0);
    a_isize err = (*in->_fun)(in->_env, in->_ctx, cast(void const**, &in->_ptr), &in->_len);
    in->_err = err;
    return likely(!err) ? ALO_SOK : ALO_EOUTER;
}

a_i32 ai_io_igetc(ZIn* in) {
    if (unlikely(in->_len == 0)) {
        check(in_fetch(in));
        if (in->_len == 0)
            return ALO_ESTMUF;
    }
    a_i32 ch = cast(a_i32, cast(a_u32, *in->_ptr));
    in->_ptr += 1;
    in->_len -= 1;
    return ch;
}

a_msg ai_io_iget(ZIn* in, void* dst, a_usize len) {
    if (unlikely(in->_len == 0))
        goto fetch;
    loop {
		if (in->_len >= len) {
			memcpy(dst, in->_ptr, len);
			in->_ptr += len;
			in->_len -= len;
			return ALO_SOK;
		}
		memcpy(dst, in->_ptr, in->_len);
		in->_len = 0;

	fetch:
		check(in_fetch(in));
		if (unlikely(in->_len == 0))
			return ALO_ESTMUF;
	}
}

void ai_io_oinit(a_henv env, a_ofun fun, void* ctx, ZOut* out) {
    out->_env = env;
    out->_fun = fun;
    out->_ctx = ctx;
    out->_err = 0;
}

a_msg ai_io_oput(ZOut* out, void const* src, a_usize len) {
    ptrdiff_t err = (*out->_fun)(out->_env, out->_ctx, src, len);
    out->_err = err;
    return likely(!err) ? ALO_SOK : ALO_EOUTER;
}
