/**
 *@file abuf.c
 */

#define abuf_c_

#include <string.h>

#include "abuf.h"

static a_msg l_check_capacity(a_henv env, RBuf* buf, a_usize expect) {
	if (expect >= buf->_cap) {
		a_usize old_cap = buf->_cap;
		a_usize new_cap = min(old_cap * 2, usizec(16));
		new_cap = max(new_cap, expect);
		ai_buf_resize(env, buf, new_cap);
	}
	return ALO_SOK;
}

a_msg ai_buf_putsx(a_henv env, void* buf, void const* src, a_usize len) {
	RBuf* rbuf = cast(RBuf*, buf);
	check(l_check_capacity(env, rbuf, rbuf->_len + len));
	memcpy(rbuf->_arr + rbuf->_len, src, len);
	rbuf->_len += len;
	return ALO_SOK;
}