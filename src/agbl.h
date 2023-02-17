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
} IStrTable;

typedef struct {
	GMeta _nil;
	GMeta _bool;
	GMeta _int;
	GMeta _ptr;
	GMeta _dstr;
	GMeta _istr;
	GMeta _hstr;
	GMeta _tuple;
	GMeta _list;
	GMeta _table;
	GMeta _route;
	GMeta _cap;
	GMeta _ref_array;
	GMeta _mod;
	GMeta _mod_loader;
	GMeta _fmeta;
	GFunMeta _cfun;
} InternMetas;

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
	GStr* _strx[STRX__MAX - 1];
	Value _global;
	a_fp_gsplash _gsplash;
	void* _gsplash_ctx;
	a_trmark _tr_gray;
	a_trmark _tr_regray;
	InternMetas _metas;
	ModCache _mod_cache;
	IStrTable _istable;
	a_hash _seed;
	a_u16 _gcpausemul;
	a_u16 _gcstepmul;
	a_u16 _flags;
	a_u8 _white_color;
	a_u8 _gcstep;
	volatile atomic_uint_fast8_t _hookm;
};

always_inline a_bool g_is_route(Global* g, a_hobj v) {
	return v->_meta == &g->_metas._route;
}

always_inline a_bool v_is_route(Global* g, Value v) {
	return v_is_other(v) && g_is_route(g, v_as_obj(g, v));
}


#define ALO_HMSWAP 0x80

always_inline void ai_env_gsplash(a_henv env, a_fp_gsplash fun, void* ctx) {
	assume(fun != null);
	Global* g = G(env);
	g->_gsplash = fun;
	g->_gsplash_ctx = ctx;
}

always_inline void ai_env_gsplash_clear(a_henv env) {
	Global* g = G(env);
	g->_gsplash = null;
	g->_gsplash_ctx = null;
}

always_inline a_usize ai_env_mem_total(Global* g) {
	return g->_mem_base + cast(a_usize, g->_mem_debt);
}

always_inline GStr* ai_env_strx(Global* g, a_u32 tag) {
	return g->_strx[tag - 1];
}

#endif /* agbl_h_ */
