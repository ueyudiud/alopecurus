/**
 *@file abuf.c
 */

#define abuf_c_
#define ALO_LIB

#include <stdio.h>

#include "agc.h"

#include "abuf.h"

static VTable const buf_vtable;

a_msg ai_buf_ncheck(a_henv env, Buf* buf, a_usize add) {
	if (add > buf->_cap - buf->_len) {
		a_usize old_cap = buf->_cap;
		a_usize new_cap = max(old_cap * 2, usizec(16));
		new_cap = max(new_cap, buf->_len + add);
		check(ai_buf_ngrow(env, buf, new_cap));
	}
	return ALO_SOK;
}

a_msg ai_buf_nputfs(a_henv env, void* buf, char const* fmt, ...) {
	va_list varg;
	va_start(varg, fmt);
	a_msg msg = ai_buf_nputvfs(env, buf, fmt, varg);
	va_end(varg);
	return msg;
}

a_msg ai_buf_nputvfs(a_henv env, void* raw_buf, char const* fmt, va_list varg) {
	Buf* buf = raw_buf;
	a_usize rem = buf->_cap - buf->_len;
	va_list varg2;
	va_copy(varg2, varg);
	a_usize len = vsnprintf(buf_fdst(buf), rem, fmt, varg);
	if (len + 1 > rem) {
		a_msg msg = ai_buf_ncheck(env, raw_buf, len + 1);
		if (msg != ALO_SOK) {
			va_end(varg2);
			return msg;
		}
		vsnprintf(buf_fdst(buf), len + 1, fmt, varg2);
	}
	va_end(varg2);
	buf->_len += len;
	return ALO_SOK;
}

GBuf* ai_buf_new(a_henv env) {
	GBuf* self = ai_mem_alloc(env, sizeof(GBuf));

	self->_vptr = &buf_vtable;
	ai_buf_init(self);

	ai_gc_register_object(env, self);

	return self;
}

static void buf_mark(Global* g, a_hobj raw_self) {
	GBuf* self = g_cast(GBuf, raw_self);
	ai_gc_trace_work(g, sizeof(GBuf) + self->_cap);
}

static void buf_drop(Global* g, a_hobj raw_self) {
	GBuf* self = g_cast(GBuf, raw_self);
	ai_buf_deinit(g, self);
	ai_mem_dealloc(g, self, sizeof(GBuf));
}

static VTable const buf_vtable = {
	._mask = V_MASKED_TAG(T_USER_TEQ),
	._iname = env_type_iname(_buf),
	._body = {
		vfp_def(drop, buf_drop),
		vfp_def(mark, buf_mark)
	}
};
