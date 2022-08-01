/**
 *@file abuf.h
 */

#ifndef abuf_h_
#define abuf_h_

#include "amem.h"

#define BUF_STRUCT_DECLARE(n,t) \
    typedef struct n n; \
    struct n { t* _arr; a_usize _len; a_usize _cap; }; 

BUF_STRUCT_DECLARE(Buf, a_byte);

typedef struct {
    void* _arr;
    a_usize _len;
    a_usize _cap;
} RBuf;

#define ai_buf_init(b) (*(b) = new(typeof(*(b))) { _arr: null, _len: 0, _cap: 0 })

inline a_msg ai_buf_grow(a_henv env, void* buf, a_usize size, a_usize limit) {
    RBuf* rbuf = cast(RBuf*, buf);
    if (unlikely(rbuf->_cap >= limit))
        return ALO_EINVAL;
    a_usize old_cap = rbuf->_cap;
    a_usize new_cap = min(old_cap * 2, limit);
    void* p = ai_mem_xrealloc(env, rbuf->_arr, size * old_cap, size * new_cap);
    if (unlikely(p == null)) 
        return ALO_ENOMEM;
    rbuf->_arr = p;
    rbuf->_cap = new_cap;
    return ALO_SOK;
}

#define ai_buf_put(env,b,v,l)  ({ \
    typeof(b) _b = (b); \
    if (unlikely(_b->_len == _b->_cap)) { \
        check(ai_buf_grow(env, _b, sizeof(_b->_arr[0]), l)); \
    } \
    quiet(_b->_arr[_b->_len ++] = (v)); \
})

#endif /* abuf_h_ */
