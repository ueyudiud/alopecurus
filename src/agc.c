/**
 *@file agc.c
 */

#define agc_c_
#define ALO_LIB

#include "aenv.h"
#include "afun.h"

#include "agc.h"

#define trmark_null GRAY_NULL

#define GCUNITSIZE 256

#define GC_STOP_WORK (ISIZE_MIN / 2)

#ifndef ALOI_SWEEP_COST
# define ALOI_SWEEP_COST usizec(64)
#endif

#ifndef ALOI_CLOSE_COST
# define ALOI_CLOSE_COST usizec(256)
#endif

always_inline void join_gc(a_gclist* list, a_gptr elem) {
	elem->gnext = *list;
	*list = elem;
}

#define join_gc(list,elem) join_gc(list, g_as_obj(elem))

always_inline a_gptr strip_gc(a_gclist* list) {
	a_gptr elem = *list;
	*list = elem->gnext;
	return elem;
}

always_inline void flip_color(Global* gbl) {
	gbl->white_bit = cast(a_u8, other_color(gbl));
}

void ai_gc_register_normal_(a_henv env, a_gptr obj) {
	g_fetch(obj, drop); /* Only collectable object need be registered. */
	Global* gbl = G(env);
	join_gc(&gbl->gc_normal, obj);
	g_set_white(gbl, obj);
}

void ai_gc_register_normals(a_henv env, RefQueue* rq) {
	Global* gbl = G(env);

#ifdef ALOI_CHECK_ASSUME
    rq_for(obj, rq) {
		assume(!g_has_other_color(gbl, obj), "object is already dead.");
	}
#endif

	*rq->tail = gbl->gc_normal;
	gbl->gc_normal = rq->head;
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
	Impl const* impl = obj->impl;
	if (impl_has_flag(impl, IMPL_FLAG_GREEDY_MARK)) {
		g_set_gray(obj); /* Mark object to gray before propagation. */
		really_mark_object(gbl, obj);
		/* Else keep object as gray since it has no mark function to remark. */
	}
	else {
		g_fetch(obj, mark);
		join_trace(&gbl->tr_gray, obj);
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
    a_gptr obj = *gbl->gc_sweep;
	if (g_has_other_color(gbl, obj)) {
        *gbl->gc_sweep = obj->gnext;
		drop_object(gbl, obj);
		return true;
	}
	else {
        gbl->gc_sweep = &obj->gnext;
        g_set_white(gbl, obj);
		return false;
	}
}

static void close_once(Global* gbl) {
    a_gptr obj = strip_gc(&gbl->gc_toclose);
    (*g_fetch(obj, close))(gbl->active, obj);
	join_gc(&gbl->gc_normal, obj);
}

static a_bool sweep_till_alive(Global* gbl) {
    do {
        if (*gbl->gc_sweep == null) {
            return false;
        }
    }
    while (sweep_once(gbl));

	return true;
}

static a_bool propagate_work(Global* gbl, a_trmark* list) {
	while (*list != trmark_null) {
		propagate_once(gbl, list);
		if (gbl->mem_work < 0) return false;
	}
	return true;
}

static a_bool sweep_work(Global* gbl) {
	while (*gbl->gc_sweep != null) {
		if (sweep_once(gbl)) {
			gbl->mem_work -= ALOI_SWEEP_COST;
			if (gbl->mem_work < 0)
				return false;
		}
	}
	return true;
}

static a_bool close_work(Global* gbl) {
	while (gbl->gc_toclose != null) {
		close_once(gbl);
		gbl->mem_work -= ALOI_CLOSE_COST;
		if (gbl->mem_work < 0)
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
	while (*gbl->gc_sweep != null) {
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
		list = &obj->gnext;
		if (g_has_other_color(gbl, obj)) {
			obj->tnext = white;
		}
	}
}

static void begin_propagate(Global* gbl) {
	gbl->tr_gray = trmark_null;
	gbl->tr_regray = trmark_null;
	/* Mark nonvolatile root. */
	join_trace(&gbl->tr_gray, ai_env_mroute(gbl));
	if (v_is_obj(gbl->global_value)) {
		join_trace(&gbl->tr_gray, v_as_obj(gbl->global_value));
	}
    for (a_u32 i = 0; i < TYPE__COUNT; ++i) {
        join_trace(&gbl->tr_gray, gbl->fast_types[i]);
    }
	gbl->gcstep = GCSTEP_PROPAGATE;
}

static void begin_sweep(Global* gbl) {
	gbl->gc_sweep = &gbl->gc_normal;
	sweep_till_alive(gbl);
	gbl->gcstep = GCSTEP_SWEEP_NORMAL;
}

static a_bool begin_close(Global* gbl) {
	if (gbl->gc_toclose == null) {
		gbl->gcstep = GCSTEP_PAUSE;
		return true;
	}
	else {
		gbl->gcstep = GCSTEP_CLOSE;
		return false;
	}
}

static void propagate_atomic(Global* gbl) {
	gbl->gcstep = GCSTEP_PROPAGATE_ATOMIC;
	/* Mark volatile root. */
	ai_gc_trace_mark(gbl, gbl->active);
	if (v_is_obj(gbl->global_value)) {
		ai_gc_trace_mark(gbl, v_as_obj(gbl->global_value));
	}
	propagate_all(gbl, &gbl->tr_gray);

	a_isize old_work = gbl->mem_work;
	a_trmark list = gbl->tr_regray;
	gbl->tr_regray = trmark_null;
	propagate_all(gbl, &list);

	propagate_all(gbl, &gbl->tr_gray);

    ai_str_cache_shrink_if_need(gbl);

	gbl->mem_estimate = gbl_mem_total(gbl);
	flip_color(gbl);
	gbl->mem_work = old_work;
}

static void sweep_atomic(Global* gbl) {
	gbl->gcstep = GCSTEP_SWEEP_ATOMIC;
	ai_cap_clean(gbl);
}

static a_bool run_incr_gc(Global* gbl) {
	switch (gbl->gcstep) {
		case GCSTEP_PAUSE: {
			begin_propagate(gbl);
			fallthrough;
		}
		case GCSTEP_PROPAGATE: {
			if (!propagate_work(gbl, &gbl->tr_gray))
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
	switch (gbl->gcstep) {
		case GCSTEP_PROPAGATE: {
			flip_color(gbl);
			fallthrough;
		}
		case GCSTEP_SWEEP_NORMAL: {
			halt_propagate(gbl, &gbl->gc_normal);
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
	gbl->mem_base = total - debt;
	gbl->mem_debt = debt;
}

static void compute_work(Global* gbl) {
#if ALO_M64
    a_usize work = (gbl->mem_work + gbl->mem_debt) * gbl->gcstepmul / GCUNITSIZE;
    /* We assume address space only use 47-bits, so maximum work will never overflow. */
    assume(work <= ISIZE_MAX);
#elif ALO_M32
    a_isize work;
    if (ckd_mul(&work, gbl->mem_work + gbl->mem_debt, gbl->gcstepmul)) {
        work = ISIZE_MAX;
    }
    work /= GCUNITSIZE;
#endif
	gbl->mem_work = cast(a_isize, max(work, ALOI_MIN_GCSTEPSIZE));
}

static void compute_step_debt(Global* gbl, a_isize work) {
	if (unlikely(ckd_mul(&work, work, 2))) {
		work = ISIZE_MAX;
	}
	gbl->mem_work = work;
	ai_gc_set_debt(gbl, -work);
}

static void compute_pause_debt(Global* gbl) {
#if ALO_M64
    a_usize debt = gbl->mem_estimate * gbl->gcpausemul / GCUNITSIZE;
    /* We assume address space only use 47-bits, so maximum work will never overflow. */
    assume(debt <= ISIZE_MAX);
#elif ALO_M32
    a_isize debt;
    if (unlikely(ckd_mul(&work, gbl->mem_estimate, gbl->gcpausemul))) {
        work = ISIZE_MAX;
    }
    debt /= GCUNITSIZE;
#endif
	gbl->mem_work = cast(a_isize, debt);
	ai_gc_set_debt(gbl, cast(a_isize, debt));
}

void ai_gc_incr_gc(a_henv env) {
	Global* gbl = G(env);
	assume(gbl->mem_debt >= 0);
	assume(!gbl->gcflags.incremental, "incremental gc already start.");
	if (!gbl->gcflags.enable) {
        ai_gc_set_debt(gbl, GC_STOP_WORK);
        return;
    }
	a_isize work = gbl->mem_work;
    gbl->gcflags.incremental = true;
	compute_work(gbl);
    gbl->gcflags.incremental = false;
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
	GcFlags old_flags = gbl->gcflags;
    gbl->gcflags.full = true;
    gbl->gcflags.emergency = emergency;
	run_full_gc(gbl);
	compute_pause_debt(gbl);
	gbl->gcflags = old_flags;
}

void ai_gc_clean(Global* gbl) {
	drop_all(gbl, &gbl->gc_closable);
	drop_all(gbl, &gbl->gc_toclose);
	drop_all(gbl, &gbl->gc_normal);
}