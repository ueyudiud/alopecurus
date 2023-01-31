/*
 * amem.c
 */

#define amem_c_

#include "aenv.h"
#include "agc.h"
#include "aerr.h"

#include "amem.h"

a_none ai_mem_nomem(a_henv env) {
	GStr* err = G(env)->_nomem_error;
	ai_err_raise(env, ALO_ENOMEM, likely(err != null) ? v_of_ref(err) : v_of_nil());
}

void* ai_mem_alloc(a_henv env, a_usize sz) {
#if ALO_STRICT_MEMORY_CHECK
	ai_gc_full_gc(env, true);
#endif
	void* blk = ai_mem_xalloc(env, sz);
	if (unlikely(blk == null)) {
		ai_gc_full_gc(env, true);
		blk = ai_mem_xalloc(env, sz);
		if (blk == null) {
			ai_mem_nomem(env);
		}
	}
	return blk;
}

void* ai_mem_realloc(a_henv env, void* blk_old, a_usize sz_old, a_usize sz_new) {
#if ALO_STRICT_MEMORY_CHECK
	ai_gc_full_gc(env, true);
#endif
	void* blk_new = ai_mem_xrealloc(env, blk_old, sz_old, sz_new);
	if (unlikely(blk_new == null)) {
		ai_gc_full_gc(env, true);
		blk_new = ai_mem_xrealloc(env, blk_old, sz_old, sz_new);
		if (blk_new == null) {
			ai_mem_nomem(env);
		}
	}
	return blk_new;
}

void ai_mem_dealloc(Global* g, void* blk, a_usize sz) {
	if (sz > 0) {
		assume(blk != null);
		ai_mem_xdealloc(g, blk, sz);
	}
}

void* ai_mem_xalloc(a_henv env, a_usize sz) {
	Global* g = G(env);
	void* blk = ai_mem_nalloc(&g->_af, g->_ac, sz);
	if (unlikely(blk == null)) return null;
	g->_mem_debt += cast(a_isize, sz);
	return blk;
}

void* ai_mem_xrealloc(a_henv env, void* blk_old, a_usize sz_old, a_usize sz_new) {
	if (sz_old == 0) {
		return ai_mem_xalloc(env, sz_new);
	}
	else {
		Global* g = G(env);
		void* blk_new = ai_mem_nrealloc(&g->_af, g->_ac, blk_old, sz_old, sz_new);
		if (unlikely(blk_new == null)) return null;
		g->_mem_debt += cast(a_isize, sz_new - sz_old);
		return blk_new;
	}
}

void ai_mem_xdealloc(Global* g, void* blk, a_usize sz) {
	ai_mem_ndealloc(&g->_af, g->_ac, blk, sz);
	g->_mem_debt -= cast(a_isize, sz);
}
