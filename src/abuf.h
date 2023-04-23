/**
 *@file abuf.h
 */

#ifndef abuf_h_
#define abuf_h_

#include <string.h>

#include "amem.h"

intern a_msg ai_buf_nputfs(a_henv env, void* buf, char const* fmt, ...);
intern a_msg ai_buf_nputvfs(a_henv env, void* buf, char const* fmt, va_list varg);
intern GBuf* ai_buf_new(a_henv env);
intern a_none ai_buf_error(a_msg msg, a_henv env, char const *what);

always_inline void ai_buf_init(void* raw_buf) {
	Buf* buf = raw_buf;
	buf->_ptr = null;
	buf->_len = 0;
	buf->_cap = 0;
}

always_inline void ai_buf_drop_(Global* g, void* raw_buf, a_usize size) {
	Buf* buf = raw_buf;
	ai_mem_dealloc(g, buf->_ptr, buf->_cap * size);
	buf->_ptr = null;
	buf->_cap = 0;
}

#define ai_buf_drop(g,b) ai_buf_drop_(g, b, sizeof((b)->_ptr[0]))

#define buf_end(b) cast(char*, (b)->_ptr + (b)->_len)

#define buf_for(b,v) for (typeof((b)->_ptr) v = (b)->_ptr, _end_##v = (b)->_ptr + (b)->_len; v < _end_##v; v += 1)

always_inline a_msg ai_buf_ngrow_(a_henv env, void* raw_buf, a_usize new_cap, a_usize size) {
	Buf* buf = raw_buf;
	assume(buf->_cap <= SIZE_MAX / size);
    a_usize old_cap = buf->_cap;
    void* ptr = ai_mem_nrealloc(env, buf->_ptr, size * old_cap, size * new_cap);
    if (unlikely(ptr == null))
		return ALO_ENOMEM;
	buf->_ptr = ptr;
	buf->_cap = new_cap;
    return ALO_SOK;
}

always_inline a_msg ai_buf_nhint_(a_usize* pcap, a_usize len, a_usize add, a_usize lim) {
	a_usize old_cap = *pcap;
	assume(add > old_cap - len, "need not grow.");
	a_usize expect;
	if (checked_add_usize(len, add, &expect)) {
		return ALO_EINVAL;
	}
	a_usize new_cap;
	if (unlikely(expect >= lim / 2)) {
		if (expect > lim) {
			return ALO_EINVAL;
		}
		new_cap = lim;
	}
	else {
		new_cap = max(expect, old_cap * 2);
		new_cap = max(new_cap, usizec(8));
	}
	*pcap = new_cap;
	return ALO_SOK;
}

always_inline a_msg ai_buf_ncheck_(a_henv env, void* raw_buf, a_usize add, a_usize size, a_usize lim) {
	Buf* buf = raw_buf;
	if (add > buf->_cap - buf->_len) {
		a_usize cap = buf->_cap;
		try(ai_buf_nhint_(&cap, buf->_len, add, lim));
		try(ai_buf_ngrow_(env, buf, cap, size));
	}
	return ALO_SOK;
}

always_inline a_msg ai_buf_nputv_(a_henv env, void* raw_buf, void const* src, a_usize len, a_usize size, a_usize lim) {
	Buf* buf = raw_buf;
	try(ai_buf_ncheck_(env, raw_buf, len, size, lim));
	memcpy(buf->_ptr + size * buf->_len, src, len * size);
	buf->_len += len;
	return ALO_SOK;
}

#define ai_buf_ncheck(env,b,a) ai_buf_ncheck_(env, b, a, sizeof((b)->_ptr[0]), SIZE_MAX / sizeof((b)->_ptr[0]))
#define ai_buf_nputv(env,b,s,l) ai_buf_nputv_(env, b, s, l, sizeof((b)->_ptr[0]), SIZE_MAX / sizeof((b)->_ptr[0]))
#define ai_buf_nput(env,b,s) ({ typeof((b)->_ptr[0]) _va = (s); ai_buf_nputv(env, b, &_va, 1); })
#define ai_buf_nputls(env,b,s,l) ai_buf_nputv_(env, b, s, l, 1, SIZE_MAX)
#define ai_buf_nputs(env,b,s) ai_buf_nputls(env, b, s, strlen(s))

#define ai_buf_put(env,b,s,w) catch(ai_buf_nput(env, b, s), ai_buf_error, env, w)

always_inline void ai_buf_putls(a_henv env, void* buf, void const* src, a_usize len) {
	a_msg msg = ai_buf_nputls(env, buf, src, len);
	catch(msg, ai_buf_error, env, "char");
}

#define ai_buf_puts(env,b,s) ai_buf_putls(env, b, s, strlen(s))
#define ai_buf_tostr(env,b) ai_str_new(env, (b)->_ptr, (b)->_len)

#define ai_buf_reset(b) quiet((b)->_len = 0)

#endif /* abuf_h_ */
