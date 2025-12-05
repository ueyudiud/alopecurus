/**
 *@file aio.c
 */

#define aio_c_
#define ALO_LIB

#include "aio.h"

#include "aenv.h"

void ai_io_iinit(a_henv env, a_ifun fun, void* ctx, ZIn* in) {
    init(in) {
        .env = env,
        .fun = fun,
        .ctx = ctx,
        .ptr = nullptr,
        .len = 0
    };
}

static a_msg in_fetch(ZIn* in) {
    assume(in->len == 0);
    a_msg msg = (*in->fun)(in->env, in->ctx, cast(void const**, &in->ptr), &in->len);
    if (unlikely(msg != 0)) {
        ai_env_raise(in->env, ALO_EIO);
    }
    return unlikely(in->len == 0) ? ALO_EEMPTY : ALO_SOK;
}

a_i32 ai_io_igetc(ZIn* in) {
    if (unlikely(in->len == 0)) {
        a_msg msg = in_fetch(in);
        if (unlikely(msg != 0)) {
            return msg;
        }
    }
    a_i32 ch = *in->ptr;
    in->ptr += 1;
    in->len -= 1;
    return ch;
}

a_msg ai_io_iget(ZIn* in, void* dst, a_usize len) {
    a_msg rem = cast(a_msg, len);
    if (in->len == 0) {
        if (in_fetch(in) != ALO_SOK) {
            return rem;
        }
    }
    loop {
		if (in->len >= cast(a_usize, rem)) {
			memcpy(dst, in->ptr, rem);
			in->ptr += rem;
			in->len -= rem;
			return 0;
		}

		memcpy(dst, in->ptr, in->len);
        rem -= cast(a_msg, in->len);
		in->len = 0;
		if (in_fetch(in) != ALO_SOK) {
		    return rem;
		}
	}
}

void ai_io_oinit(a_henv env, a_ofun fun, void* ctx, ZOut* out) {
    out->env = env;
    out->fun = fun;
    out->ctx = ctx;
}

void ai_io_oput(ZOut* out, void const* src, a_usize len) {
	a_i32 err = (*out->fun)(out->env, out->ctx, src, len);
    if (unlikely(err != 0)) {
        ai_env_raise(out->env, ALO_EIO);
    }
}
