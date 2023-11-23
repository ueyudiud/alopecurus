/*
 * agc.c
 */

#define agc_c_
#define ALO_LIB

#include "aenv.h"
#include "afun.h"

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

always_inline void join_gc(a_gclist* list, a_gptr elem) {
	elem->_gnext = *list;
	*list = elem;
}

#define join_gc(list,elem) join_gc(list, gobj_cast(elem))

always_inline a_gptr strip_gc(a_gclist* list) {
	a_gptr elem = *list;
	*list = elem->_gnext;
	return elem;
}

always_inline void flip_color(Global* gbl) {
	gbl->_white_color = cast(a_u8, other_color(gbl));
}

void ai_gc_register_object_(a_henv env, a_gptr obj) {
	g_fetch(obj, drop); /* Only collectable object need be registered. */
	Global* gbl = G(env);
	join_gc(&gbl->_gc_normal, obj);
	g_set_white(gbl, obj);
}

void ai_gc_register_objects(a_henv env, RefQueue* rq) {
	Global* gbl = G(env);

#ifdef ALOI_CHECK_ASSUME
    rq_for(obj, rq) {
		assume(!g_has_other_color(gbl, obj), "object is already dead.");
	}
#endif

	*rq->_tail = gbl->_gc_normal;
	gbl->_gc_normal = rq->_head;
}

void ai_gc_fix_object_(a_henv env, a_gptr obj) {
	Global* gbl = G(env);
	assume(gbl->_gc_normal == obj, "object not registered.");
	strip_gc(&gbl->_gc_normal);
	join_gc(&gbl->_gc_fixed, obj);
}

static void really_mark_object(Global* gbl, a_gptr obj) {
	/* Color object to black. */
	g_set_black(obj);
	/* Call mark virtual method. */
    (*g_fetch(obj, mark))(gbl, obj);
}

void ai_gc_trace_mark_(Global* gbl, a_gptr obj) {
	/* Tested in inline function. */
	assume(g_has_white_color(gbl, obj));
	/* Mark object lazily or greedily. */
	VTable const* vptr = obj->_vptr;
	if (vtable_has_flag(vptr, VTABLE_FLAG_GREEDY_MARK)) {
		g_set_gray(obj); /* Mark object to gray before propagation. */
		really_mark_object(gbl, obj);
		/* Else keep object as gray since it has no mark function to remark. */
	}
	else {
		g_fetch(obj, mark);
		join_trace(&gbl->_tr_gray, obj);
	}
}

static void drop_object(Global* gbl, a_gptr obj) {
	/* Call drop virtual method */
    (*g_fetch(obj, drop))(gbl, obj);
}

static void propagate_once(Global* gbl, a_trmark* list) {
	a_gptr obj = strip_trace(list);
	really_mark_object(gbl, obj);
}

static a_bool sweep_once(Global* gbl) {
    a_gptr obj = *gbl->_gc_sweep;
	if (g_has_other_color(gbl, obj)) {
        *gbl->_gc_sweep = obj->_gnext;
		drop_object(gbl, obj);
		return true;
	}
	else {
        gbl->_gc_sweep = &obj->_gnext;
        g_set_white(gbl, obj);
		return false;
	}
}

static void close_once(Global* gbl) {
    a_gptr obj = strip_gc(&gbl->_gc_toclose);
    (*g_fetch(obj, close))(gbl->_active, obj);
	join_gc(&gbl->_gc_normal, obj);
}

static a_bool sweep_till_alive(Global* gbl) {
    do {
        if (*gbl->_gc_sweep == null) {
            return false;
        }
    }
    while (sweep_once(gbl));

	return true;
}

static a_bool propagate_work(Global* gbl, a_trmark* list) {
	while (*list != trmark_null) {
		propagate_once(gbl, list);
		if (gbl->_mem_work < 0) return false;
	}
	return true;
}

static a_bool sweep_work(Global* gbl) {
	while (*gbl->_gc_sweep != null) {
		if (sweep_once(gbl)) {
			gbl->_mem_work -= ALOI_SWEEP_COST;
			if (gbl->_mem_work < 0)
				return false;
		}
	}
	return true;
}

static a_bool close_work(Global* gbl) {
	while (gbl->_gc_toclose != null) {
		close_once(gbl);
		gbl->_mem_work -= ALOI_CLOSE_COST;
		if (gbl->_mem_work < 0)
			return false;
	}
	return true;
}

static void propagate_all(Global* gbl, a_trmark* list) {
	while (*list != trmark_null) {
		propagate_once(gbl, list);
	}
}

static void sweep_all(Global* gbl) {
	while (*gbl->_gc_sweep != null) {
        sweep_once(gbl);
	}
}

static void drop_all(Global* gbl, a_gptr* list) {
	while (*list != null) {
		a_gptr obj = strip_gc(list);
		drop_object(gbl, obj);
	}
}

static void halt_propagate(Global* gbl, a_gptr* list) {
	a_trmark white = white_color(gbl);
	while (*list != null) {
		a_gptr obj = *list;
		list = &obj->_gnext;
		if (g_has_other_color(gbl, obj)) {
			obj->_tnext = white;
		}
	}
}

static void begin_propagate(Global* gbl) {
	gbl->_tr_gray = trmark_null;
	gbl->_tr_regray = trmark_null;
	/* Mark nonvolatile root. */
	join_trace(&gbl->_tr_gray, ai_env_mroute(gbl));
	if (v_is_obj(gbl->_global)) {
		join_trace(&gbl->_tr_gray, v_as_obj(gbl->_global));
	}
    for (a_u32 i = 0; i < TYPE__COUNT; ++i) {
        join_trace(&gbl->_tr_gray, gbl->_types[i]);
    }
	gbl->_gcstep = GCSTEP_PROPAGATE;
}

static void begin_sweep(Global* gbl) {
	gbl->_gc_sweep = &gbl->_gc_normal;
	sweep_till_alive(gbl);
	gbl->_gcstep = GCSTEP_SWEEP_NORMAL;
}

static a_bool begin_close(Global* gbl) {
	if (gbl->_gc_toclose == null) {
		gbl->_gcstep = GCSTEP_PAUSE;
		return true;
	}
	else {
		gbl->_gcstep = GCSTEP_CLOSE;
		return false;
	}
}

static void propagate_atomic(Global* gbl) {
	gbl->_gcstep = GCSTEP_PROPAGATE_ATOMIC;
	/* Mark volatile root. */
	ai_gc_trace_mark(gbl, gbl->_active);
	if (v_is_obj(gbl->_global)) {
		ai_gc_trace_mark(gbl, v_as_obj(gbl->_global));
	}
	if (gbl->_gmark != null) {
		(*gbl->_gmark)(gbl, gbl->_gctx);
	}
	propagate_all(gbl, &gbl->_tr_gray);

	a_isize old_work = gbl->_mem_work;
	a_trmark list = gbl->_tr_regray;
	gbl->_tr_regray = trmark_null;
	propagate_all(gbl, &list);

	propagate_all(gbl, &gbl->_tr_gray);

	gbl->_mem_estimate = gbl_mem_total(gbl);
	flip_color(gbl);
	gbl->_mem_work = old_work;
}

static void sweep_atomic(Global* gbl) {
	gbl->_gcstep = GCSTEP_SWEEP_ATOMIC;
	ai_cap_clean(gbl);
}

static a_bool run_incr_gc(Global* gbl) {
	switch (gbl->_gcstep) {
		case GCSTEP_PAUSE: {
			begin_propagate(gbl);
			fallthrough;
		}
		case GCSTEP_PROPAGATE: {
			if (!propagate_work(gbl, &gbl->_tr_gray))
				return false;
			fallthrough;
		}
		case GCSTEP_PROPAGATE_ATOMIC: {
			propagate_atomic(gbl);
			begin_sweep(gbl);
			fallthrough;
		}
		case GCSTEP_SWEEP_NORMAL: {
			if (!sweep_work(gbl))
				return false;
			fallthrough;
		}
		case GCSTEP_SWEEP_ATOMIC: {
			sweep_atomic(gbl);
			if (begin_close(gbl))
				return true;
			fallthrough;
		}
		case GCSTEP_CLOSE: {
			return close_work(gbl);
		}
		default: unreachable();
	}
}

static void run_full_gc(Global* gbl) {
	switch (gbl->_gcstep) {
		case GCSTEP_PROPAGATE: {
			flip_color(gbl);
			fallthrough;
		}
		case GCSTEP_SWEEP_NORMAL: {
			halt_propagate(gbl, &gbl->_gc_normal);
			fallthrough;
		}
		default: {
			begin_propagate(gbl);
			propagate_atomic(gbl);
			begin_sweep(gbl);
			sweep_all(gbl);
			sweep_atomic(gbl);
			break;
		}
	}
}

void ai_gc_set_debt(Global* gbl, a_isize debt) {
	a_usize total = gbl_mem_total(gbl);
	gbl->_mem_base = total - debt;
	gbl->_mem_debt = debt;
}

static void compute_work(Global* gbl) {
#if ALO_M64
    a_usize work = (gbl->_mem_work + gbl->_mem_debt) * gbl->_gcstepmul / GCUNIT;
    /* We assume address space only use 47-bits, so maximum work will never overflow. */
    assume(work <= ISIZE_MAX);
#elif ALO_M32
    a_isize work;
    if (checked_mul_isize(gbl->_mem_work + gbl->_mem_debt, gbl->_gcstepmul, &work)) {
        work = ISIZE_MAX;
    }
    work /= GCUNIT;
#endif
	gbl->_mem_work = cast(a_isize, max(work, ALOI_MIN_MEM_WORK));
}

static void compute_step_debt(Global* gbl, a_isize work) {
	if (unlikely(checked_mul_isize(work, 2, &work))) {
		work = ISIZE_MAX;
	}
	gbl->_mem_work = work;
	ai_gc_set_debt(gbl, -work);
}

static void compute_pause_debt(Global* gbl) {
#if ALO_M64
    a_usize debt = gbl->_mem_estimate * gbl->_gcpausemul / GCUNIT;
    /* We assume address space only use 47-bits, so maximum work will never overflow. */
    assume(debt <= ISIZE_MAX);
#elif ALO_M32
    a_isize debt;
    if (checked_mul_isize(gbl->_mem_estimate, gbl->_gcpausemul, &work)) {
        work = ISIZE_MAX;
    }
    debt /= GCUNIT;
#endif
	gbl->_mem_work = cast(a_isize, debt);
	ai_gc_set_debt(gbl, cast(a_isize, debt));
}

void ai_gc_incr_gc(a_henv env) {
	Global* gbl = G(env);
	assume(gbl->_mem_debt >= 0);
	assume(!(gbl->_flags & GLOBAL_FLAG_INCRGC));
	if (gbl->_flags & GLOBAL_FLAG_DISABLE_GC) return;
	a_isize work = gbl->_mem_work;
	gbl->_flags |= GLOBAL_FLAG_INCRGC;
	compute_work(gbl);
	gbl->_flags &= ~GLOBAL_FLAG_INCRGC;
	a_bool finish = run_incr_gc(gbl);
	if (finish) {
		compute_pause_debt(gbl);
	}
	else {
		compute_step_debt(gbl, work);
	}
}

void ai_gc_full_gc(a_henv env, a_bool emergency) {
	Global* gbl = G(env);
	a_u16 old_flags = gbl->_flags;
	gbl->_flags |= GLOBAL_FLAG_FULLGC;
	if (emergency) gbl->_flags |= GLOBAL_FLAG_EMERGENCYGC;
	run_full_gc(gbl);
	compute_pause_debt(gbl);
	gbl->_flags = old_flags;
}

void ai_gc_clean(Global* gbl) {
	drop_all(gbl, &gbl->_gc_closable);
	drop_all(gbl, &gbl->_gc_toclose);
	drop_all(gbl, &gbl->_gc_normal);
	drop_all(gbl, &gbl->_gc_fixed);
}