/**
 *@file abuf.c
 */

#define abuf_c_
#define ALO_LIB

#include <stdio.h>

#include "agc.h"
#include "aerr.h"

#include "abuf.h"

static VTable const buf_vtable;

a_msg ai_buf_nputfs_(a_henv env, Buf* buf, char const* fmt, ...) {
    va_list varg;
    va_start(varg, fmt);

    a_msg msg = ai_buf_nputvfs_(env, buf, fmt, varg);

    va_end(varg);
    return msg;
}

a_msg ai_buf_nputvfs_(a_henv env, Buf* buf, char const* fmt, va_list varg) {
    va_list varg2;
	a_usize rem = buf->_cap - buf->_len;

	va_copy(varg2, varg);

	a_usize len = vsnprintf(buf_end( buf), rem, fmt, varg);
	if (len + 1 > rem) {
		a_msg msg = ai_buf_ncheck(env, buf, len + 1, 1, SIZE_MAX);
		if (unlikely(msg != ALO_SOK)) {
			va_end(varg2);
			return msg;
		}
		vsnprintf(buf_end( buf), len + 1, fmt, varg2);
	}

	va_end(varg2);

	buf->_len += len;
	return ALO_SOK;
}

GBuf* ai_buf_new(a_henv env) {
	GBuf* self = ai_mem_gnew(env, GBuf, sizeof(GBuf));

	self->_vptr = &buf_vtable;
	at_buf_init(self);

	ai_gc_register_object(env, self);

	return self;
}

a_noret ai_buf_error(a_henv env, a_msg msg, char const* what) {
	if (msg == ALO_EINVAL) {
		ai_err_raisef(env, msg, "too many %s.", what);
	}
	else {
		assume(msg == ALO_ENOMEM);
		ai_mem_nomem(env);
	}
}

static void buf_mark(Global* gbl, a_hobj raw_self) {
	GBuf* self = g_cast(GBuf, raw_self);
	ai_gc_trace_work(gbl, sizeof(GBuf) + self->_cap);
}

static void buf_drop(Global* gbl, a_hobj raw_self) {
	GBuf* self = g_cast(GBuf, raw_self);
	at_buf_deinit(gbl, self);
	ai_mem_gdel(gbl, self, sizeof(GBuf));
}

static VTable const buf_vtable = {
	._stencil = V_STENCIL(T_USER),
    ._tag = ALO_TUSER,
	._slots = {
        [vfp_slot(drop)] = buf_drop,
        [vfp_slot(mark)] = buf_mark
	}
};
