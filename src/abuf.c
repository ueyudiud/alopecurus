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

static VTable const ref_array_vtable;

static a_msg buf_check(a_henv env, CBuf* buf, a_usize add) {
	if (add > buf->_cap - buf->_len) {
		a_usize old_cap = buf->_cap;
		a_usize new_cap = max(old_cap * 2, usizec(16));
		new_cap = max(new_cap, buf->_len + add);
		ai_buf_resizex(env, buf, new_cap);
	}
	return ALO_SOK;
}

a_msg ai_buf_putsx_(a_henv env, void* buf, void const* src, a_usize len) {
	CBuf* cbuf = buf;
	check(buf_check(env, cbuf, len));
	memcpy(cbuf->_arr + cbuf->_len, src, len);
	cbuf->_len += len;
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
	CBuf* cbuf = buf;
	a_usize rem = cbuf->_cap - cbuf->_len;
	va_list varg2;
	va_copy(varg2, varg);
	a_usize len = vsnprintf(cbuf->_arr + cbuf->_len, rem, fmt, varg);
	a_msg msg = ALO_SOK;
	if (len + 1 > rem) {
		msg = buf_check(env, buf, len + 1);
		if (msg == ALO_SOK) {
			vsnprintf(cbuf->_arr + cbuf->_len, len + 1, fmt, varg2);
		}
	}
	va_end(varg2);
	if (msg == ALO_SOK) {
		cbuf->_len += len;
	}
	return msg;
}

static a_usize ref_array_size(a_usize len) {
	return sizeof(GRefArray) + sizeof(a_hobj) * len;
}

GRefArray* ai_ref_array_new(a_henv env, a_usize len) {
	GRefArray* self = ai_mem_alloc(env, ref_array_size(len));

	self->_vtable = &ref_array_vtable;
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

	ai_gc_trace_work(g, ref_array_size(self->_len));
}

static void ref_array_delete(Global* g, GRefArray* self) {
	ai_mem_dealloc(g, self, ref_array_size(self->_len));
}

static VTable const ref_array_vtable = {
	._tid = T_USER_TEQ,
	._api_tag = ALO_TUSER,
	._flags = VTABLE_FLAG_IDENTITY_EQUAL,
	._name = "array",
	._splash = fpcast(a_fp_splash, ref_array_splash),
	._delete = fpcast(a_fp_delete, ref_array_delete)
};