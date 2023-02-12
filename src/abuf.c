/**
 *@file abuf.c
 */

#define abuf_c_
#define ALO_LIB

#include <string.h>
#include <stdio.h>

#include "agbl.h"
#include "agc.h"

#include "abuf.h"

static a_msg l_check_capacity(a_henv env, RBuf* buf, a_usize expect) {
	if (expect >= buf->_cap) {
		a_usize old_cap = buf->_cap;
		a_usize new_cap = max(old_cap * 2, usizec(16));
		new_cap = max(new_cap, expect);
		ai_buf_resizex(env, buf, new_cap);
	}
	return ALO_SOK;
}

a_msg ai_buf_putsx_(a_henv env, void* buf, void const* src, a_usize len) {
	RBuf* rbuf = cast(RBuf*, buf);
	check(l_check_capacity(env, rbuf, rbuf->_len + len));
	memcpy(rbuf->_arr + rbuf->_len, src, len);
	rbuf->_len += len;
	return ALO_SOK;
}

a_msg ai_buf_putf_(a_henv env, void* buf, char const* fmt, ...) {
	va_list varg;
	va_start(varg, fmt);
	a_msg msg = ai_buf_putvf_(env, buf, fmt, varg);
	va_end(varg);
	return msg;
}

a_msg ai_buf_putvf_(a_henv env, void* buf, char const* fmt, va_list varg) {
	RBuf* rbuf = cast(RBuf*, buf);
	a_usize rem = rbuf->_cap - rbuf->_len;
	va_list varg2;
	va_copy(varg2, varg);
	a_usize len = vsnprintf(cast(char*, rbuf->_arr + rbuf->_len), rem, fmt, varg);
	a_msg msg = ALO_SOK;
	if (len + 1 > rem) {
		msg = l_check_capacity(env, buf, rem);
		if (msg == ALO_SOK) {
			vsnprintf(cast(char*, rbuf->_arr + rbuf->_len), len + 1, fmt, varg2);
		}
	}
	va_end(varg2);
	if (msg == ALO_SOK) {
		rbuf->_len += len;
	}
	return msg;
}

static a_usize l_ref_array_size(a_usize len) {
	return sizeof(GRefArray) + sizeof(a_hobj) * len;
}

GRefArray* ai_ref_array_new(a_henv env, a_usize len) {
	GRefArray* self = ai_mem_alloc(env, l_ref_array_size(len));

	self->_meta = &G(env)->_metas._ref_array;
	self->_len = len;
	memclr(self->_data, sizeof(a_hobj) * len);

	ai_gc_register_object(env, self);

	return self;
}

static void ref_array_splash(Global* g, GRefArray* self) {
	for (a_u32 i = 0; i < self->_len; ++i) {
		if (self->_data[i] != null) {
			ai_gc_trace_mark(g, self->_data[i]);
		}
	}

	g->_mem_work -= l_ref_array_size(self->_len);
}

static void ref_array_destruct(Global* g, GRefArray* self) {
	ai_mem_dealloc(g, self, l_ref_array_size(self->_len));
}

VTable const ai_ref_array_vtable = {
	._splash = fpcast(a_fp_splash, ref_array_splash),
	._destruct = fpcast(a_fp_destruct, ref_array_destruct)
};