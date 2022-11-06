/**
 *@file abuf.h
 */

#ifndef abuf_h_
#define abuf_h_

#include "amem.h"

#define BUF_STRUCT_DECLARE(n,t,e...) \
    typedef struct n n; \
    struct n { t* _arr; a_usize _len; a_usize _cap; e; }

BUF_STRUCT_DECLARE(Buf, a_byte);
BUF_STRUCT_DECLARE(QBuf, a_byte, QBuf* _last);

typedef struct {
    void* _arr;
    a_usize _len;
    a_usize _cap;
} RBuf;

#define ai_buf_init(b) (*(b) = new(typeof(*(b))) { _arr: null, _len: 0, _cap: 0 })

intern a_msg ai_buf_putsx(a_henv env, void* buf, void const* src, a_usize len);

inline a_msg ai_buf_resizex(a_henv env, void* buf, a_usize new_cap, a_usize size, a_usize limit) {
    RBuf* rbuf = cast(RBuf*, buf);
    if (unlikely(rbuf->_cap >= limit))
        return ALO_EINVAL;
    a_usize old_cap = rbuf->_cap;
    void* p = ai_mem_xrealloc(env, rbuf->_arr, size * old_cap, size * new_cap);
    if (unlikely(p == null)) 
        return ALO_ENOMEM;
    rbuf->_arr = p;
    rbuf->_cap = new_cap;
    return ALO_SOK;
}

inline void ai_buf_close(Global* g, void* buf) {
	RBuf* rbuf = cast(RBuf*, buf);
	ai_mem_dealloc(g, rbuf->_arr, rbuf->_cap);
	memclr(rbuf, sizeof(RBuf));
}

#define ai_buf_resize(env,b,n) check(ai_buf_resizex(env, b, n, sizeof((b)->_arr[0]), SIZE_MAX / sizeof((b)->_arr[0])))

#define ai_buf_put(env,b,v)  ({ \
    typeof(b) _b = (b); \
    if (unlikely(_b->_len == _b->_cap)) { \
        ai_buf_resize(env, _b, max(_b->_cap * 2, 16)); \
    } \
    quiet(_b->_arr[_b->_len ++] = (v)); \
})

#define ai_buf_puts(env,b,s,l) check(ai_buf_putsx(env, b, s, l))

#define ai_buf_reset(b) quiet((b)->_len = 0)

#endif /* abuf_h_ */
