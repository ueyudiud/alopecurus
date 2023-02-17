/*
 * agc.c
 */

#define agc_c_
#define ALO_LIB

#include "amod.h"

#include "agc.h"

#define trmark_null cast(a_trmark, null)

#define GCUNIT 256

#ifndef ALOI_MINMEMWORK
# define ALOI_MINMEMWORK usizec(4096)
#endif

#ifndef ALOI_SWEEPCOST
# define ALOI_SWEEPCOST usizec(64)
#endif

#ifndef ALOI_CLOSECOST
# define ALOI_CLOSECOST usizec(256)
#endif

always_inline void join_gc_(a_gclist* list, a_hobj elem) {
	elem->_gnext = *list;
	*list = elem;
}

#define join_gc(list,elem) join_gc_(list, g_cast(GObj, elem))

always_inline a_hobj strip_gc(a_gclist* list) {
	a_hobj elem = *list;
	*list = elem->_gnext;
	return elem;
}

always_inline void flip_color(Global* g) {
	g->_white_color = cast(a_u8, other_color(g));
}

void ai_gc_register_object_(a_henv env, a_hobj obj) {
	Global* g = G(env);
	join_gc(&g->_gc_normal, obj);
	obj->_tnext = cast(a_trmark, g->_white_color);
}

void ai_gc_register_objects(a_henv env, RefQueue* rq) {
	Global* g = G(env);
	*rq->_tail = g->_gc_normal;
	g->_gc_normal = rq->_head;

#if defined(ALOI_DEBUG)
	rq_for(obj, rq) {
		assume(g_is_white(g, obj) || g_is_gray(obj) || g_is_black(obj));
	}
#endif
}

void ai_gc_fix_object_(a_henv env, a_hobj obj) {
	Global* g = G(env);
	assume(g->_gc_normal == obj);
	join_gc(&g->_gc_fixed, strip_gc(&g->_gc_normal));
}

void ai_gc_trace_mark_(Global* g, a_hobj obj) {
	if (g_is_white(g, obj) && obj->_meta->_vtable._splash != null) {
		join_trace(&g->_tr_gray, obj);
	}
}

always_inline void splash_object(Global* g, a_hobj obj) {
	a_fp_splash splash = obj->_meta->_vtable._splash;
	if (likely(splash != null)) {
		(*splash)(g, obj);
	}
}

always_inline void delete_object(Global* g, a_hobj obj) {
	a_fp_destruct destruct = obj->_meta->_vtable._destruct;
	if (destruct != null) {
		(*destruct)(g, obj);
	}
}

static void propagagte_once(Global* g, a_trmark* list) {
	splash_object(g, strip_trace(list));
}

static a_bool sweep_once(Global* g) {
	GObj* obj = strip_gc(g->_gc_sweep);
	if (g_is_other(g, obj)) {
		delete_object(g, obj);
		return true;
	}
	else {
		obj->_tnext = white_color(g);
		return false;
	}
}

static void close_once(Global* g) {
	GObj* obj = strip_gc(&g->_gc_closable);
	(void) obj; //TODO
	join_gc(&g->_gc_normal, obj);
}

static a_bool sweep_till_alive(Global* g) {
	while (*g->_gc_sweep != null && !sweep_once(g));
	return *g->_gc_sweep != null;
}

static a_bool propagate_work(Global* g, a_trmark* list) {
	while (*list != trmark_null) {
		propagagte_once(g, list);
		if (g->_mem_work < 0) return false;
	}
	return true;
}

static a_bool sweep_work(Global* g) {
	while (*g->_gc_sweep != null) {
		if (sweep_once(g)) {
			g->_mem_work -= ALOI_SWEEPCOST;
			if (g->_mem_work < 0)
				return false;
		}
	}
	return true;
}

static a_bool close_work(Global* g) {
	while (g->_gc_toclose != null) {
		close_once(g);
		g->_mem_work -= ALOI_CLOSECOST;
		if (g->_mem_work < 0) 
			return false;
	}
	return true;
}

static void propagate_all(Global* g, a_trmark* list) {
	while (*list != trmark_null) {
		propagagte_once(g, list);
	}
}

static void sweep_all(Global* g) {
	while (*g->_gc_sweep != null) {
		sweep_once(g);
	}
}

static void delete_all(Global* g, GObj** list) {
	while (*list != null) {
		GObj* obj = strip_gc(list);
		delete_object(g, obj);
	}
}

static void halt_propagate(Global* g, GObj** list) {
	a_trmark white = white_color(g);
	while (*list != null) {
		GObj* obj = *list;
		list = &obj->_gnext;
		if (g_is_other(g, obj)) {
			obj->_tnext = white;
		}
	}
}

static void begin_propagate(Global* g) {
	g->_tr_gray = trmark_null;
	g->_tr_regray = trmark_null;
	/* Mark nonvolatile root. */
	join_trace(&g->_tr_gray, gobj_cast(ai_env_mainof(g)));
	if (v_is_obj(g->_global)) {
		join_trace(&g->_tr_gray, v_as_obj(g, g->_global));
	}
	g->_gcstep = GCSTEP_PROPAGATE;
}

static void begin_sweep(Global* g) {
	g->_gc_sweep = &g->_gc_normal;
	sweep_till_alive(g);
	g->_gcstep = GCSTEP_SWEEP_NORMAL;
}

static a_bool begin_close(Global* g) {
	if (g->_gc_toclose == null) {
		g->_gcstep = GCSTEP_PAUSE;
		return true;
	}
	else {
		g->_gcstep = GCSTEP_CLOSE;
		return false;
	}
}

static void propagate_atomic(Global* g) {
	a_isize old_work = g->_mem_work;
	g->_gcstep = GCSTEP_PROPAGATE_ATOMIC;
	/* Mark volatile root. */
	ai_gc_trace_mark_(g, gobj_cast(g->_active));
	if (v_is_obj(g->_global)) {
		join_trace(&g->_tr_gray, v_as_obj(g, g->_global));
	}
	ai_mod_cache_splash(g, &g->_mod_cache);
	if (g->_gsplash != null) {
		(*g->_gsplash)(g, g->_gsplash_ctx);
	}
	propagate_all(g, &g->_tr_gray);

	g->_mem_estimate = ai_env_mem_total(g);
	
	a_trmark list = g->_tr_regray;
	g->_tr_regray = trmark_null;
	propagate_all(g, &list);

	g->_mem_work = 0;
	propagate_all(g, &g->_tr_gray);
	g->_mem_estimate += cast(a_usize, -g->_mem_work);

	flip_color(g);
	g->_mem_work = old_work;
}

static void sweep_atomic(Global* g) {
	g->_gcstep = GCSTEP_SWEEP_ATOMIC;
}

static a_bool run_incr_gc(Global* g) {
	switch (g->_gcstep) {
		case GCSTEP_PAUSE: {
			begin_propagate(g);
			fallthrough;
		}
		case GCSTEP_PROPAGATE: {
			if (!propagate_work(g, &g->_tr_gray)) 
				return false;
			fallthrough;
		}
		case GCSTEP_PROPAGATE_ATOMIC: {
			propagate_atomic(g);
			begin_sweep(g);
			fallthrough;
		}
		case GCSTEP_SWEEP_NORMAL: {
			if (!sweep_work(g)) 
				return false;
			fallthrough;
		}
		case GCSTEP_SWEEP_ATOMIC: {
			sweep_atomic(g);
			if (begin_close(g)) 
				return true;
			fallthrough;
		}
		case GCSTEP_CLOSE: {
			return close_work(g);
		}
		default: unreachable();
	}
}

static void run_full_gc(Global* g) {
	switch (g->_gcstep) {
		case GCSTEP_PROPAGATE: {
			flip_color(g);
			fallthrough;
		}
		case GCSTEP_SWEEP_NORMAL: {
			halt_propagate(g, &g->_gc_normal);
			fallthrough;
		}
		default: {
			begin_propagate(g);
			propagate_atomic(g);
			begin_sweep(g);
			sweep_all(g);
			sweep_atomic(g);
			break;
		}
	}
}

void ai_gc_set_debt(Global* g, a_isize debt) {
	a_usize total = ai_env_mem_total(g);
	g->_mem_base = total - debt;
	g->_mem_debt = debt;
}

static void compute_work(Global* g) {
	a_usize2 work = mul_usize(cast(a_usize, g->_mem_work) + cast(a_usize, g->_mem_debt), g->_gcstepmul) / GCUNIT;
	a_isize truncated_work = likely(work < ISIZE_MAX) ? cast(a_isize, work) : ISIZE_MAX;
	g->_mem_work = max(truncated_work, cast(a_isize, ALOI_MINMEMWORK));
}

static void compute_step_debt(Global* g, a_isize work) {
	if (unlikely(checked_mul_isize(work, 2, &work))) {
		work = ISIZE_MAX;
	}
	g->_mem_work = work;
	ai_gc_set_debt(g, -work);
}

static void compute_pause_debt(Global* g) {
	a_usize2 debt = mul_usize(g->_mem_estimate, g->_gcpausemul) / GCUNIT;
	a_isize truncated_debt = likely(debt < ISIZE_MAX) ? cast(a_isize, debt) : ISIZE_MAX;
	g->_mem_work = truncated_debt;
	ai_gc_set_debt(g, truncated_debt);
}

void ai_gc_incr_gc(a_henv env) {
	Global* g = G(env);
	assume(g->_mem_debt >= 0);
	assume(!(g->_flags & GLOBAL_FLAG_INCRGC));
	if (g->_flags & GLOBAL_FLAG_DISABLE_GC) return;
	a_isize work = g->_mem_work;
	g->_flags |= GLOBAL_FLAG_INCRGC;
	compute_work(g);
	g->_flags &= ~GLOBAL_FLAG_INCRGC;
	a_bool finish = run_incr_gc(g);
	if (finish) {
		compute_pause_debt(g);
	}
	else {
		compute_step_debt(g, work);
	}
}

void ai_gc_full_gc(a_henv env, a_bool emergency) {
	Global* g = G(env);
	a_u16 old_flags = g->_flags;
	g->_flags |= GLOBAL_FLAG_FULLGC;
	if (emergency) g->_flags |= GLOBAL_FLAG_PAUSE_CLOSE;
	run_full_gc(g);
	compute_pause_debt(g);
	g->_flags = old_flags;
}

void ai_gc_clean(Global* g) {
	delete_all(g, &g->_gc_closable);
	delete_all(g, &g->_gc_toclose);
	delete_all(g, &g->_gc_normal);
	delete_all(g, &g->_gc_fixed);
}