/*
 * agc.c
 */

#define agc_c_
#define ALO_LIB

#include "aenv.h"
#include "afun.h"
#include "atype.h"
#include "amem.h"

#include "agc.h"

#define trmark_null GRAY_NULL

#define GCUNIT 256

#ifndef ALOI_MIN_MEM_WORK
# define ALOI_MIN_MEM_WORK usizec(4096)
#endif

#ifndef ALOI_SWEEP_COST
# define ALOI_SWEEP_COST usizec(64)
#endif

#ifndef ALOI_CLOSE_COST
# define ALOI_CLOSE_COST usizec(256)
#endif

always_inline void join_gc(a_gclist* list, a_hobj elem) {
	elem->_gnext = *list;
	*list = elem;
}

#define join_gc(list,elem) join_gc(list, gobj_cast(elem))

always_inline a_hobj strip_gc(a_gclist* list) {
	a_hobj elem = *list;
	*list = elem->_gnext;
	return elem;
}

always_inline void flip_color(Global* g) {
	g->_white_color = cast(a_u8, other_color(g));
}

void ai_gc_register_object_(a_henv env, a_hobj obj) {
	g_vcheck(obj, drop); /* Only collectable object need be registered. */
	Global* g = G(env);
	join_gc(&g->_gc_normal, obj);
	g_set_white(g, obj);
}

void ai_gc_register_objects(a_henv env, RefQueue* rq) {
	Global* g = G(env);
	*rq->_tail = g->_gc_normal;
	g->_gc_normal = rq->_head;

#if ALO_DEBUG
	rq_for(obj, rq) {
		assume(!g_has_other_color(g, obj), "object is already dead.");
	}
#endif
}

void ai_gc_fix_object_(a_henv env, a_hobj obj) {
	Global* g = G(env);
	assume(g->_gc_normal == obj, "object not registered.");
	strip_gc(&g->_gc_normal);
	join_gc(&g->_gc_fixed, obj);
}

static void really_mark_object(Global* g, a_hobj obj) {
	/* Color object to black. */
	g_set_black(obj);
	/* Call mark virtual method. */
	g_vcall(g, obj, mark);
}

void ai_gc_trace_mark_(Global* g, a_hobj obj) {
	/* Tested in inline function. */
	assume(g_has_white_color(g, obj));
	/* Mark object lazily or greedily. */
	VTable const* vptr = obj->_vptr;
	if (vtable_has_flag(vptr, VTABLE_FLAG_GREEDY_MARK)) {
		g_set_gray(obj); /* Mark object to gray before propagation. */
		really_mark_object(g, obj);
		/* Else keep object as gray since it has no mark function to remark. */
	}
	else {
		g_vcheck(obj, mark);
		join_trace(&g->_tr_gray, obj);
	}
}

static void drop_object(Global* g, a_hobj obj) {
	/* Call drop virtual method */
	g_vcall(g, obj, drop);
}

static void propagate_once(Global* g, a_trmark* list) {
	a_hobj obj = strip_trace(list);
	really_mark_object(g, obj);
}

static a_bool sweep_once(Global* g, a_usize white) {
	GObj* obj = strip_gc(g->_gc_sweep);
	if (g_has_other_color(g, obj)) {
		drop_object(g, obj);
		return true;
	}
	else {
		obj->_tnext |= white;
		return false;
	}
}

static void close_once(Global* g) {
	GObj* obj = strip_gc(&g->_gc_toclose);
	g_vcall(g->_active, obj, close);
	join_gc(&g->_gc_normal, obj);
}

static a_bool sweep_till_alive(Global* g) {
	a_usize color = white_color(g);
	while (*g->_gc_sweep != null && !sweep_once(g, color));
	return *g->_gc_sweep != null;
}

static a_bool propagate_work(Global* g, a_trmark* list) {
	while (*list != trmark_null) {
		propagate_once(g, list);
		if (g->_mem_work < 0) return false;
	}
	return true;
}

static a_bool sweep_work(Global* g) {
	while (*g->_gc_sweep != null) {
		if (sweep_once(g, white_color(g))) {
			g->_mem_work -= ALOI_SWEEP_COST;
			if (g->_mem_work < 0)
				return false;
		}
	}
	return true;
}

static a_bool close_work(Global* g) {
	while (g->_gc_toclose != null) {
		close_once(g);
		g->_mem_work -= ALOI_CLOSE_COST;
		if (g->_mem_work < 0) 
			return false;
	}
	return true;
}

static void propagate_all(Global* g, a_trmark* list) {
	while (*list != trmark_null) {
		propagate_once(g, list);
	}
}

static void sweep_all(Global* g) {
	a_usize color = white_color(g);
	while (*g->_gc_sweep != null) {
		sweep_once(g, color);
	}
}

static void drop_all(Global* g, GObj** list) {
	while (*list != null) {
		GObj* obj = strip_gc(list);
		drop_object(g, obj);
	}
}

static void halt_propagate(Global* g, GObj** list) {
	a_trmark white = white_color(g);
	while (*list != null) {
		GObj* obj = *list;
		list = &obj->_gnext;
		if (g_has_other_color(g, obj)) {
			obj->_tnext = white;
		}
	}
}

static void begin_propagate(Global* g) {
	g->_tr_gray = trmark_null;
	g->_tr_regray = trmark_null;
	/* Mark nonvolatile root. */
	join_trace(&g->_tr_gray, ai_env_mroute(g));
	if (v_is_obj(g->_global)) {
		join_trace(&g->_tr_gray, v_as_obj(g->_global));
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
	g->_gcstep = GCSTEP_PROPAGATE_ATOMIC;
	/* Mark volatile root. */
	ai_gc_trace_mark(g, g->_active);
	if (v_is_obj(g->_global)) {
		join_trace(&g->_tr_gray, v_as_obj(g->_global));
	}
    ai_type_cache_mark(g, &g->_type_cache);
	if (g->_gmark != null) {
		(*g->_gmark)(g, g->_gctx);
	}
	propagate_all(g, &g->_tr_gray);

	a_isize old_work = g->_mem_work;
	a_trmark list = g->_tr_regray;
	g->_tr_regray = trmark_null;
	propagate_all(g, &list);

	propagate_all(g, &g->_tr_gray);

	g->_mem_estimate = gbl_mem_total(g);
	flip_color(g);
	g->_mem_work = old_work;
}

static void sweep_atomic(Global* g) {
	g->_gcstep = GCSTEP_SWEEP_ATOMIC;
	ai_cap_clean(g);
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
	a_usize total = gbl_mem_total(g);
	g->_mem_base = total - debt;
	g->_mem_debt = debt;
}

static void compute_work(Global* g) {
	a_usize2 work = mul_usize(cast(a_usize, g->_mem_work) + cast(a_usize, g->_mem_debt), g->_gcstepmul) / GCUNIT;
	a_isize truncated_work = likely(work < ISIZE_MAX) ? cast(a_isize, work) : ISIZE_MAX;
	g->_mem_work = max(truncated_work, cast(a_isize, ALOI_MIN_MEM_WORK));
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
	if (emergency) g->_flags |= GLOBAL_FLAG_EMERGENCYGC;
	run_full_gc(g);
	compute_pause_debt(g);
	g->_flags = old_flags;
}

void ai_gc_clean(Global* g) {
	drop_all(g, &g->_gc_closable);
	drop_all(g, &g->_gc_toclose);
	drop_all(g, &g->_gc_normal);
	drop_all(g, &g->_gc_fixed);
}