/*
 * amem.h
 */

#ifndef amem_h_
#define amem_h_

#include "aobj.h"

intern a_none ai_mem_nomem(a_henv env);

intern void* ai_mem_alloc(a_henv env, a_usize sz);
intern void* ai_mem_realloc(a_henv env, void* blk_old, a_usize sz_old, a_usize sz_new);
intern void ai_mem_dealloc(Global* g, void* blk, a_usize sz);

intern void* ai_mem_xalloc(a_henv env, a_usize sz);
intern void* ai_mem_xrealloc(a_henv env, void* blk, a_usize sz_old, a_usize sz_new);
intern void ai_mem_xdealloc(Global* g, void* blk, a_usize sz);

inline void* ai_mem_nalloc(a_alloc const* af, void* ac, a_usize sz) {
	assume(sz > 0);
	return (*af->allocate)(ac, sz);
}

inline void* ai_mem_nrealloc(a_alloc const* af, void* ac, void* blk_old, a_usize sz_old, a_usize sz_new) {
	assume(blk_old != null && sz_old > 0);
	assume(sz_new > 0);
	return (*af->reallocate)(ac, blk_old, sz_old, sz_new);
}

inline void ai_mem_ndealloc(a_alloc const* af, void* ac, void* blk, a_usize sz) {
	assume(blk != null && sz > 0);
	(*af->deallocate)(ac, blk, sz);
}

#define ai_mem_vxnew(env,type,size) cast(typeof(type)*, ai_mem_xalloc(env, sizeof(type) * (size)))
#define ai_mem_vxgrow(env,vec,size_old,size_new) cast(typeof(vec), ai_mem_xrealloc(env, vec, sizeof((vec)[0]) * (size_old), sizeof((vec)[0]) * (size_new)))
#define ai_mem_vxdel(g,vec,size) ai_mem_xdealloc(g, vec, sizeof((vec)[0]) * (size))

#define ai_mem_vnew(env,type,size) cast(typeof(type)*, ai_mem_alloc(env, sizeof(type) * (size)))
#define ai_mem_vgrow(env,vec,size_old,size_new) cast(typeof(vec), ai_mem_realloc(env, vec, sizeof((vec)[0]) * (size_old), sizeof((vec)[0]) * (size_new)))
#define ai_mem_vdel(g,vec,size) ai_mem_dealloc(g, vec, sizeof((vec)[0]) * (size))

#endif /* amem_h_ */
