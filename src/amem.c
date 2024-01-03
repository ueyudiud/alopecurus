/**
 *@file amem.c
 */

#define amem_c_
#define ALO_LIB

#include "aenv.h"
#include "agc.h"
#include "aerr.h"

#include "amem.h"

a_noret ai_mem_nomem(a_henv env) {
	GStr* err = G(env)->nomem_error;
	ai_err_raise(env, ALO_ENOMEM, likely(err != null) ? v_of_str(err) : v_of_nil());
}

void* ai_mem_alloc(a_henv env, a_usize sz) {
#ifdef ALOI_CHECK_GC
	ai_gc_full_gc(env, true);
#endif
	void* blk = ai_mem_nalloc(env, sz);
	if (unlikely(blk == null)) {
		ai_gc_full_gc(env, true);
		blk = ai_mem_nalloc(env, sz);
		if (blk == null) {
			ai_mem_nomem(env);
		}
	}
	return blk;
}

void* ai_mem_realloc(a_henv env, void* blk_old, a_usize sz_old, a_usize sz_new) {
#ifdef ALOI_CHECK_GC
	ai_gc_full_gc(env, true);
#endif
	void* blk_new = ai_mem_nrealloc(env, blk_old, sz_old, sz_new);
	if (unlikely(blk_new == null)) {
		ai_gc_full_gc(env, true);
		blk_new = ai_mem_nrealloc(env, blk_old, sz_old, sz_new);
		if (blk_new == null) {
			ai_mem_nomem(env);
		}
	}
	return blk_new;
}

void ai_mem_dealloc(Global* gbl, void* blk, a_usize sz) {
	if (sz > 0) {
		assume(blk != null);
		ai_mem_ndealloc(gbl, blk, sz);
	}
}

void* ai_mem_nalloc(a_henv env, a_usize sz) {
	Global* gbl = G(env);
	void* blk = ai_mem_valloc(&gbl->alloc_, gbl->alloc_ctx, sz);
	if (unlikely(blk == null)) return null;
	gbl->mem_debt += cast(a_isize, sz);
	return blk;
}

void* ai_mem_nrealloc(a_henv env, void* blk_old, a_usize sz_old, a_usize sz_new) {
	if (sz_old == 0) {
		return ai_mem_nalloc(env, sz_new);
	}
	else {
		Global* gbl = G(env);
		void* blk_new = ai_mem_vrealloc(&gbl->alloc_, gbl->alloc_ctx, blk_old, sz_old, sz_new);
		if (unlikely(blk_new == null)) return null;
		gbl->mem_debt += cast(a_isize, sz_new - sz_old);
		return blk_new;
	}
}

void ai_mem_ndealloc(Global* gbl, void* blk, a_usize sz) {
	ai_mem_vdealloc(&gbl->alloc_, gbl->alloc_ctx, blk, sz);
	gbl->mem_debt -= cast(a_isize, sz);
}
