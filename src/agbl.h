/*
 * agbl.h
 *
 *  Created on: 2023/1/31
 *      Author: ueyudiud
 */

#ifndef agbl_h_
#define agbl_h_

#include "astr.h"
#include "astrx.h"
#include "afun.h"
#include "amod.h"
#include "aenv.h"

typedef struct {
	IStr** _table;
	a_usize _len;
	a_usize _hmask; /* Hash code mask. */
} IStrCache;

typedef void (*a_fp_gexecpt)(a_henv env, void* ctx, a_msg msg);
typedef void (*a_fp_gsplash)(Global* g, void* ctx);

struct Global {
	Alloc _af;
	void* _ac;
	a_hfun _hookf;
	a_hctx _hookc;
	a_henv _active;
	a_usize _mem_base;
	a_isize _mem_debt;
	a_isize _mem_work;
	a_usize _mem_estimate;
	a_gclist _gc_normal;
	a_gclist _gc_fixed;
	a_gclist _gc_closable;
	a_gclist _gc_toclose;
	a_gcnext* _gc_sweep;
	GStr* _nomem_error;
	Value _global;
	a_fp_gexecpt _gexecpt;
	a_fp_gsplash _gsplash;
	void* _gprotect_ctx;
	a_trmark _tr_gray;
	a_trmark _tr_regray;
	ModCache _mod_cache;
	IStrCache _istr_cache;
	a_hash _seed;
	a_u16 _gcpausemul;
	a_u16 _gcstepmul;
	a_u16 _flags;
	a_u8 _white_color;
	a_u8 _gcstep;
	volatile atomic_uint_fast8_t _hookm;
	GStr* _strx[STRX__END - 1];
};

#define ALO_HMSWAP 0x80

always_inline void ai_env_gprotect(a_henv env, a_fp_gsplash splash, a_fp_gexecpt except, void* ctx) {
	assume(splash != null || except != null);
	Global* g = G(env);
	g->_gsplash = splash;
	g->_gexecpt = except;
	g->_gprotect_ctx = ctx;
}

always_inline void ai_env_gprotect_clear(a_henv env) {
	Global* g = G(env);
	g->_gsplash = null;
	g->_gexecpt = null;
	g->_gprotect_ctx = null;
}

always_inline a_usize ai_env_mem_total(Global* g) {
	return g->_mem_base + cast(a_usize, g->_mem_debt);
}

always_inline GStr* ai_env_strx(Global* g, a_u32 tag) {
	return g->_strx[tag - 1];
}

#endif /* agbl_h_ */
