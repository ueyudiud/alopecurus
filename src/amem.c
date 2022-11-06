/*
 * amem.c
 */

#define amem_c_

#include "aenv.h"
#include "agc.h"

#include "amem.h"

a_none ai_mem_nomem(a_henv env) {
	GStr* error = G(env)->_nomem_error;
	if (likely(error != null)) {
		v_set(G(env), &env->_error, v_of_ref(error));
	}
	ai_env_raise(env, ALO_ENOMEM);
}

void* ai_mem_alloc(a_henv env, a_usize sz) {
	void* blk = ai_mem_xalloc(env, sz);
	if (unlikely(blk == null)) {
		ai_mem_nomem(env);
	}
	return blk;
}

void* ai_mem_realloc(a_henv env, void* blk_old, a_usize sz_old, a_usize sz_new) {
	void* blk_new = ai_mem_xrealloc(env, blk_old, sz_old, sz_new);
	if (unlikely(blk_new == null)) {
		ai_mem_nomem(env);
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
		g->_mem_debt += cast(a_isize, sz_new - sz_old);
		return blk_new;
	}
}

void ai_mem_xdealloc(Global* g, void* blk, a_usize sz) {
	ai_mem_ndealloc(&g->_af, g->_ac, blk, sz);
	g->_mem_debt -= cast(a_isize, sz);
}
