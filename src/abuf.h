/**
 *@file abuf.h
 */

#ifndef abuf_h_
#define abuf_h_

#include <string.h>

#include "amem.h"

#define BUF_DATA_NAME _arr
#define BUF_DATA_DEF(t) typeof(t)* BUF_DATA_NAME
#define BUF_DATA_REF BUF_DATA_NAME

#define BUF_LEN_NAME _len
#define BUF_LEN_REF BUF_LEN_NAME

typedef struct GRefArray GRefArray;

intern a_msg ai_buf_putsx_(a_henv env, void* buf, void const* src, a_usize len);
intern a_msg ai_buf_putvf_(a_henv env, void* buf, char const* fmt, va_list varg);
intern a_msg ai_buf_putf_(a_henv env, void* buf, char const* fmt, ...);

intern GRefArray* ai_ref_array_new(a_henv env, a_usize len);

intern VTable const ai_ref_array_vtable;

struct GRefArray {
	GOBJ_STRUCT_HEADER;
	a_u32 _len;
	a_hobj _data[];
};

#define BUF_STRUCT_DECLARE(n,t,e...) \
    typedef struct n n; \
    struct n { BUF_DATA_DEF(t); a_usize BUF_LEN_NAME; a_usize _cap; e; }

BUF_STRUCT_DECLARE(Buf, a_byte);
BUF_STRUCT_DECLARE(QBuf, a_byte, QBuf* _last);
BUF_STRUCT_DECLARE(RBuf, void);

always_inline a_msg ai_buf_resize0_(a_henv env, void* buf, a_usize new_cap, a_usize size, a_usize limit) {
    RBuf* rbuf = cast(RBuf*, buf);
    if (unlikely(rbuf->_cap >= limit))
        return ALO_EINVAL;
    a_usize old_cap = rbuf->_cap;
    void* p = ai_mem_xrealloc(env, rbuf->BUF_DATA_REF, size * old_cap, size * new_cap);
    if (unlikely(p == null)) 
        return ALO_ENOMEM;
    rbuf->BUF_DATA_REF = p;
    rbuf->_cap = new_cap;
    return ALO_SOK;
}

always_inline void ai_buf_close_(Global* g, void* buf, a_usize size) {
	RBuf* rbuf = cast(RBuf*, buf);
	ai_mem_dealloc(g, rbuf->BUF_DATA_REF, rbuf->_cap * size);
	*rbuf = new(RBuf) {};
}

always_inline void ai_buf_copy_(void* dst, void* buf, a_usize size) {
	RBuf* rbuf = cast(RBuf*, buf);
	memcpy(dst, rbuf->BUF_DATA_REF, rbuf->_len * size);
}

#define ai_buf_open(b) (*(b) = new(typeof(*(b))) { .BUF_DATA_REF = null, .BUF_LEN_REF = 0, ._cap = 0 })
#define ai_buf_close(env,b) ai_buf_close_(G(env), b, sizeof((b)->_arr[0]))

#define ai_buf_resize_(env,b,n) ai_buf_resize0_(env, b, n, sizeof((b)->BUF_DATA_REF[0]), SIZE_MAX / sizeof((b)->BUF_DATA_REF[0]))
#define ai_buf_resizex(env,b,n) check(ai_buf_resize_(env, b, n))
#define ai_buf_resize(env,b,n) ({ if (ai_buf_resize_(env, b, n) != ALO_SOK) ai_mem_nomem(env); })

#define ai_buf_copy(dst,b) ai_buf_copy_(dst, b, sizeof((b)->_arr[0]))

#define ai_buf_put(env,b,v)  ({ \
    typeof(b) _buf = (b); \
    if (unlikely(_buf->_len == _buf->_cap)) { \
        ai_buf_resizex(env, _buf, max(_buf->_cap * 2, 16)); \
    } \
    quiet(_buf->BUF_DATA_REF[_buf->BUF_LEN_REF ++] = (v)); \
})

#define ai_buf_putls(env,b,s,l) check(ai_buf_putsx_(env, b, s, l))
#define ai_buf_puts(env,b,s) ai_buf_putls(env, b, s, strlen(s))
#define ai_buf_putvf(env,b,f,v) check(ai_buf_putvf_(env, b, f, v))
#define ai_buf_putf(env,b,f,a...) check(ai_buf_putf_(env, b, f, ##a))

#define ai_buf_reset(b) quiet((b)->_len = 0)

#endif /* abuf_h_ */
