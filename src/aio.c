/**
 *@file aio.c
 */

#define aio_c_
#define ALO_LIB

#include "aio.h"

void ai_io_iinit(a_henv env, a_ifun fun, void* ctx, ZIn* in) {
    init(in) {
        .env = env,
        .fun = fun,
        .ctx = ctx,
        .ptr = null,
        .len = 0,
        .err = 0
    };
}

static a_msg in_fetch(ZIn* in) {
    assume(in->len == 0);
    in->err = (*in->fun)(in->env, in->ctx, cast(void const**, &in->ptr), &in->len);
    return unlikely(in->err) ? ALO_EOUTER :
           unlikely(in->len == 0) ? ALO_EEMPTY :
           ALO_SOK;
}

a_i32 ai_io_igetc(ZIn* in) {
    if (unlikely(in->len == 0)) {
        try (in_fetch(in));
    }
    a_i32 ch = *in->ptr;
    in->ptr += 1;
    in->len -= 1;
    return ch;
}

a_msg ai_io_iget(ZIn* in, void* dst, a_usize len) {
    if (in->len == 0) {
        try (in_fetch(in));
    }
    loop {
		if (in->len >= len) {
			memcpy(dst, in->ptr, len);
			in->ptr += len;
			in->len -= len;
			return ALO_SOK;
		}
		memcpy(dst, in->ptr, in->len);
		in->len = 0;
		try (in_fetch(in));
	}
}

void ai_io_oinit(a_henv env, a_ofun fun, void* ctx, ZOut* out) {
    out->env = env;
    out->fun = fun;
    out->ctx = ctx;
    out->err = 0;
}

a_msg ai_io_oput(ZOut* out, void const* src, a_usize len) {
	a_i32 err = (*out->fun)(out->env, out->ctx, src, len);
    out->err = err;
    return likely(!err) ? ALO_SOK : ALO_EOUTER;
}
