/**
 *@file abuf.h
 */

#ifndef abuf_h_
#define abuf_h_

#include <string.h>

#include "amem.h"

typedef struct { a_usize _; } BufHeadMark[0];

#define BUF_STRUCT_BODY(t) BufHeadMark _buf_head_mark; t* _ptr; a_usize _len; a_usize _cap

#define BUF_STRUCT_DECLARE(n,t,e...) \
    typedef struct n n; \
    struct n { BUF_STRUCT_BODY(t); e; }

BUF_STRUCT_DECLARE(Buf, a_byte);

struct GBuf {
    GOBJ_STRUCT_HEADER;
    BUF_STRUCT_BODY(a_byte);
};

typedef Buf* a_hbuf;

#define buf_end(b) cast(void*, (b)->_ptr + (b)->_len)
#define buf_cast(b) from_member(Buf, _buf_head_mark, &(b)->_buf_head_mark)
#define buf_elem_type(b) typeof((b)->_ptr[0])
#define buf_elem_size(b) sizeof((b)->_ptr[0])
#define buf_max_len(b) (SIZE_MAX / buf_elem_size(b))

intern a_msg ai_buf_nputfs_(a_henv env, a_hbuf buf, char const* fmt, ...);
intern a_msg ai_buf_nputvfs_(a_henv env, a_hbuf buf, char const* fmt, va_list varg);
intern GBuf* ai_buf_new(a_henv env);
intern a_noret ai_buf_error(a_henv env, a_msg msg, char const* what);

always_inline void ai_buf_init_(a_hbuf buf) {
	buf->_ptr = null;
	buf->_len = 0;
	buf->_cap = 0;
}

#define at_buf_init(b) ai_buf_init_(buf_cast(b))

always_inline void ai_buf_deinit_(Global* gbl, a_hbuf buf, a_usize size) {
	ai_mem_dealloc(gbl, buf->_ptr, buf->_cap * size);
	buf->_ptr = null;
	buf->_cap = 0;
}

#define at_buf_deinit(gbl,b) ai_buf_deinit_(gbl, buf_cast(b), buf_elem_size(b))

#define at_buf_for(b,v) for ( \
	__auto_type               \
    v = (b)->_ptr,            \
	_end_##v =                \
    (b)->_ptr + (b)->_len;    \
	v < _end_##v;             \
	v += 1                    \
)

always_inline a_msg ai_buf_ngrow(a_henv env, a_hbuf buf, a_usize new_cap, a_usize size) {
    a_usize old_cap = buf->_cap;
    assume(old_cap <= SIZE_MAX / size && new_cap <= SIZE_MAX / size, "invalid capacity.");

    void* ptr = ai_mem_nrealloc(env, buf->_ptr, size * old_cap, size * new_cap);
    if (unlikely(ptr == null)) return ALO_ENOMEM;

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
		try (ai_buf_nhint(&cap, buf->_len, add, lim));
		try (ai_buf_ngrow(env, buf, cap, size));
	}
	return ALO_SOK;
}

always_inline a_msg ai_buf_nappend(a_henv env, a_hbuf buf, void const* src, a_usize len, a_usize size, a_usize lim) {
	try (ai_buf_ncheck(env, buf, len, size, lim));
	memcpy(buf->_ptr + size * buf->_len, src, len * size);
	buf->_len += len;
	return ALO_SOK;
}

#define at_buf_nhint(t,c,l,a) ai_buf_nhint(&(c), l, a, (SIZE_MAX / sizeof(t)))
#define at_buf_ncheck(env,b,a) ai_buf_ncheck(env, buf_cast(b), a, buf_elem_size(b), buf_max_len(b))
#define at_buf_nappend(env,b,s,l) ai_buf_nappend(env, buf_cast(b), s, l, buf_elem_size(b), buf_max_len(b))
#define at_buf_npush(env,b,v) ({ buf_elem_type(b) _va = (v); at_buf_nappend(env, b, &_va, 1); })

#define at_buf_push(env,b,v,w) ({ \
	__auto_type _b = b;           \
    a_usize _bid = _b->_len;      \
	catch (at_buf_npush(env, _b, v)) { ai_buf_error(env, _e, w); } \
    _bid;                         \
})

#define at_buf_pop(env,b) ({ \
	__auto_type _b = b;      \
    assume(_b->_len > 0, "pop empty buf."); \
    _b->_ptr[--_b->_len];    \
})

#define at_buf_clear(b) quiet((b)->_len = 0)

/* String buffer specific functions. */

#define ai_buf_nputls(env,b,s,l) at_buf_nappend(env, b, s, l)
#define ai_buf_nputs(env,b,s) ai_buf_nputls(env, b, s, strlen(s))
#define ai_buf_nputfs(env,b,f,a...) ai_buf_nputfs_(env, buf_cast(b), f, ##a)
#define ai_buf_nputvfs(env,b,f,v) ai_buf_nputvfs_(env, buf_cast(b), f, v)

#define at_buf_putls(env,b,s,l) catch (ai_buf_nputls(env, b, s, l), _msg) { ai_buf_error(env, _msg, "char"); }
#define at_buf_puts(env,b,s) at_buf_putls(env, b, s, strlen(s))
#define at_buf_putc(env,b,c) at_buf_push(env, b, c, "char")
#define at_buf_tostr(env,b) ai_str_get_or_new(env, (b)->_ptr, (b)->_len)

#endif /* abuf_h_ */
