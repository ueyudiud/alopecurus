/**
 *@file abuf.h
 */

#ifndef abuf_h_
#define abuf_h_

#include <string.h>

#include "amem.h"

typedef struct { a_usize _; } BufHeadMark[0];

#define BUF_STRUCT_DECLARE(n,t,e...) \
    typedef struct n n; \
    struct n { BufHeadMark _buf_head_mark; t* _ptr; a_usize _len; a_usize _cap; e; }

BUF_STRUCT_DECLARE(Buf, void);
BUF_STRUCT_DECLARE(ByteBuf, a_byte);
BUF_STRUCT_DECLARE(QBuf, a_byte, QBuf* _last);
BUF_STRUCT_DECLARE(GBuf, a_byte, GOBJ_STRUCT_HEADER);

typedef Buf* a_hbuf;

#define at_buf_cast(b) from_member(Buf, _buf_head_mark, &(b)._buf_head_mark)
#define at_buf_elem_type(b) typeof((b)._ptr[0])
#define at_buf_elem_size(b) sizeof((b)._ptr[0])
#define at_buf_max_len(b) (SIZE_MAX / at_buf_elem_size(b))

intern a_msg ai_buf_nputfs_(a_henv env, a_hbuf buf, char const* fmt, ...);
intern a_msg ai_buf_nputvfs_(a_henv env, a_hbuf buf, char const* fmt, va_list varg);
intern GBuf* ai_buf_new(a_henv env);
intern a_none ai_buf_error(a_msg msg, a_henv env, char const *what);

always_inline void ai_buf_init(a_hbuf buf) {
	buf->_ptr = null;
	buf->_len = 0;
	buf->_cap = 0;
}

#define at_buf_init(b) ai_buf_init(at_buf_cast(b))

always_inline void ai_buf_deinit(Global* g, a_hbuf buf, a_usize size) {
	ai_mem_dealloc(g, buf->_ptr, buf->_cap * size);
	buf->_ptr = null;
	buf->_cap = 0;
}

#define at_buf_deinit(g,b) ai_buf_deinit(g, at_buf_cast(b), at_buf_elem_size(b))

#define at_buf_for(b,v) for ( \
	typeof(at_buf_elem_type(b)*) v = (b)._ptr, \
	_end_##v = (b)._ptr + (b)._len; \
	v < _end_##v; \
	v += 1 \
)

always_inline a_msg ai_buf_ngrow(a_henv env, a_hbuf buf, a_usize new_cap, a_usize size) {
	assume(buf->_cap <= SIZE_MAX / size);
    a_usize old_cap = buf->_cap;
    void* ptr = ai_mem_nrealloc(env, buf->_ptr, size * old_cap, size * new_cap);
    if (unlikely(ptr == null))
		return ALO_ENOMEM;
	buf->_ptr = ptr;
	buf->_cap = new_cap;
    return ALO_SOK;
}

always_inline a_msg ai_buf_nhint(a_usize* pcap, a_usize len, a_usize add, a_usize lim) {
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

always_inline a_msg ai_buf_ncheck(a_henv env, a_hbuf buf, a_usize add, a_usize size, a_usize lim) {
	if (add > buf->_cap - buf->_len) {
		a_usize cap = buf->_cap;
		try(ai_buf_nhint(&cap, buf->_len, add, lim));
		try(ai_buf_ngrow(env, buf, cap, size));
	}
	return ALO_SOK;
}

always_inline a_msg ai_buf_nputv(a_henv env, a_hbuf buf, void const* src, a_usize len, a_usize size, a_usize lim) {
	try(ai_buf_ncheck(env, buf, len, size, lim));
	memcpy(buf->_ptr + size * buf->_len, src, len * size);
	buf->_len += len;
	return ALO_SOK;
}

#define at_buf_nhint(t,c,l,a) ai_buf_nhint(&(c), l, a, (SIZE_MAX / sizeof(t)))
#define at_buf_ncheck(env,b,a) ai_buf_ncheck(env, at_buf_cast(b), a, at_buf_elem_size(b), at_buf_max_len(b))
#define at_buf_nputv(env,b,s,l) ai_buf_nputv(env, at_buf_cast(b), s, l, at_buf_elem_size(b), at_buf_max_len(b))
#define at_buf_nput(env,b,s) ({ at_buf_elem_type(b) _va = (s); at_buf_nputv(env, b, &_va, 1); })

#define ai_buf_nputls(env,b,s,l) ai_buf_nputv(env, at_buf_cast(*(b)), s, l, 1, SIZE_MAX)
#define ai_buf_nputs(env,b,s) ai_buf_nputls(env, b, s, strlen(s))
#define ai_buf_nputfs(env,b,f,a...) ai_buf_nputfs_(env, at_buf_cast(*(b)), f, ##a)
#define ai_buf_nputvfs(env,b,f,v) ai_buf_nputvfs_(env, at_buf_cast(*(b)), f, v)

#define at_buf_put(env,b,v,w) ({ \
	typeof(b)* _pb = &(b);       \
    a_usize _bid = _pb->_len;    \
	catch(at_buf_nput(env, *_pb, v), ai_buf_error, env, w); \
    _bid;                        \
})

#define at_buf_pop(env,b) ({ \
	typeof(b)* _pb = &(b);   \
    assume(_pb->_len > 0, "pop empty buf."); \
    _pb->_ptr[--_pb->_len];  \
})

#define at_buf_putls(env,b,s,l) catch(ai_buf_nputls(env, b, s, l), ai_buf_error, env, "char")
#define at_buf_puts(env,b,s) at_buf_putls(env, b, s, strlen(s))
#define at_buf_tostr(env,b) ai_str_new(env, (b)->_ptr, (b)->_len)

#define at_buf_clear(b) quiet((b)._len = 0)

#endif /* abuf_h_ */
