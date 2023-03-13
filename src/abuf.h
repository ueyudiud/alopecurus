/**
 *@file abuf.h
 */

#ifndef abuf_h_
#define abuf_h_

#include <string.h>

#include "amem.h"

#define BUF_PTR_NAME _ptr
#define BUF_PTR_DEF(t) typeof(t)* BUF_PTR_NAME
#define BUF_PTR_REF BUF_PTR_NAME

#define BUF_LEN_NAME _len
#define BUF_LEN_REF BUF_LEN_NAME

intern a_msg ai_buf_nputi(a_henv env, void* raw_buf, a_int val);
intern a_msg ai_buf_nputf(a_henv env, void* raw_buf, a_float val);
intern a_msg ai_buf_ncheck(a_henv env, Buf* buf, a_usize add);
intern a_msg ai_buf_nputfs(a_henv env, void* buf, char const* fmt, ...);
intern a_msg ai_buf_nputvfs(a_henv env, void* raw_buf, char const* fmt, va_list varg);
intern GBuf* ai_buf_new(a_henv env);
intern void ai_buf_cats(a_henv env, GBuf* self, GStr* str);
intern GStr* ai_buf_tostr(a_henv env, GBuf* self);

#define BUF_STRUCT_DECLARE(n,t,e...) \
    typedef struct n n; \
    struct n { BUF_PTR_DEF(t); a_usize BUF_LEN_NAME; a_usize _cap; e; }

BUF_STRUCT_DECLARE(Buf, a_byte);
BUF_STRUCT_DECLARE(QBuf, a_byte, QBuf* _last);
BUF_STRUCT_DECLARE(GBuf, a_byte, GOBJ_STRUCT_HEADER);

#define buf_fdst(b) cast(char*, (b)->BUF_PTR_REF + (b)->BUF_LEN_REF)

always_inline a_msg ai_buf_ngrow(a_henv env, void* raw_buf, a_usize new_cap, a_usize size) {
	Buf* buf = raw_buf;
    if (unlikely(buf->_cap >= SIZE_MAX / size))
        return ALO_ENOMEM;
    a_usize old_cap = buf->_cap;
    void* ptr = ai_mem_nrealloc(env, buf->BUF_PTR_REF, size * old_cap, size * new_cap);
    if (unlikely(ptr == null)) {
		return ALO_ENOMEM;
	}
	buf->BUF_PTR_REF = ptr;
	buf->_cap = new_cap;
    return ALO_SOK;
}

always_inline a_msg ai_buf_nputls(a_henv env, void* raw_buf, void const* src, a_usize len) {
	Buf* buf = raw_buf;
	check(ai_buf_ncheck(env, buf, len));
	memcpy(buf_fdst(buf), src, len);
	buf->BUF_LEN_REF += len;
	return ALO_SOK;
}

#define ai_buf_nputs(env,b,s) ai_buf_nputls(env, b, s, strlen(s))

always_inline void ai_buf_init(void* raw_buf) {
	Buf* buf = raw_buf;
	buf->BUF_PTR_REF = null;
	buf->BUF_LEN_REF = 0;
	buf->_cap = 0;
}

always_inline void ai_buf_deinit(Global* g, void* raw_buf, a_usize size) {
	Buf* buf = raw_buf;
	ai_mem_dealloc(g, buf->BUF_PTR_REF, buf->_cap * size);
	buf->BUF_PTR_REF = null;
	buf->_cap = 0;
}

#define ai_buf_deinit(g,b) ai_buf_deinit(g, b, sizeof((b)->BUF_PTR_REF[0]))

#define ai_buf_ngrow(env,b,n) ai_buf_ngrow(env, b, n, sizeof((b)->BUF_PTR_REF[0]))

#define ai_buf_xput(env,b,v)  ({ \
    typeof(b) _buf = (b); \
    if (unlikely(_buf->BUF_LEN_REF == _buf->_cap)) { \
        check(ai_buf_ngrow(env, _buf, max(_buf->_cap * 2, 16))); \
    } \
    quiet(_buf->BUF_PTR_REF[_buf->BUF_LEN_REF ++] = (v)); \
})

#define ai_buf_xputls(env,b,s,l) check(ai_buf_nputls(env, b, s, l))
#define ai_buf_xputs(env,b,s) ai_buf_xputls(env, b, s, strlen(s))
#define ai_buf_xputvfs(env,b,f,v) check(ai_buf_nputvfs(env, b, f, v))
#define ai_buf_xputfs(env,b,f,a...) check(ai_buf_nputfs(env, b, f, ##a))

always_inline void ai_buf_putls(a_henv env, void* raw_buf, void const* src, a_usize len) {
	a_msg msg = ai_buf_nputls(env, raw_buf, src, len);
	if (unlikely(msg != ALO_SOK)) {
		assume(msg == ALO_ENOMEM);
		ai_mem_nomem(env);
	}
}

#define ai_buf_puts(env,b,s) ai_buf_putls(env, b, s, strlen(s))
#define ai_buf_tostr(env,b) ai_str_new(env, (b)->BUF_PTR_REF, (b)->BUF_LEN_REF)

#define ai_buf_reset(b) quiet((b)->BUF_LEN_REF = 0)

#endif /* abuf_h_ */
