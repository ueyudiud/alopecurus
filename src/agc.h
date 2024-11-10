/**
 *@file agc.h
 */

#ifndef agc_h_
#define agc_h_

#include "aenv.h"

#ifndef ALOI_MIN_GCSTEPSIZE
# define ALOI_MIN_GCSTEPSIZE usizec(4096)
#endif

typedef struct {
	a_gclist head;
	a_gclist* tail;
} RefQueue;

always_inline void rq_init(RefQueue* rq) {
	rq->tail = &rq->head;
}

always_inline void rq_push(RefQueue* rq, a_gptr p) {
	*rq->tail = p;
	rq->tail = &p->gnext;
}

#define rq_push(rq,o) rq_push(rq, g_as_ref(o))

#define rq_for(v,rq) for ( \
	a_gptr *_p_##v = &(rq)->head, v; \
	_p_##v != (rq)->tail && ((v) = *_p_##v, true); \
	_p_##v = &(v)->gnext \
)

always_inline void g_set_white(Global* gbl, a_gptr o) {
    o->tnext = white_color(gbl);
}

#define g_set_white(gbl,o) g_set_white(gbl, g_as_ref(o))

always_inline void g_set_gray(a_gptr v) {
    v->tnext = GRAY_NULL;
}

#define g_set_gray(o) g_set_gray(g_as_ref(o))

always_inline void g_set_black(a_gptr o) {
    o->tnext = BLACK_COLOR;
}

#define g_set_black(o) g_set_black(g_as_ref(o))

always_inline void join_trace(a_trmark* list, a_gptr o) {
    o->tnext = *list;
	*list = ptr2int(o);
}

#define join_trace(list,o) join_trace(list, g_as_ref(o))

always_inline a_gptr strip_trace(a_trmark* list) {
    a_gptr o = int2ptr(GObj, *list);
	*list = o->tnext;
	return o;
}

intern void ai_gc_push_stack_(a_henv env, a_gptr obj);
intern void ai_gc_pop_stack(a_henv env);
intern void ai_gc_register_normal_(a_henv env, a_gptr obj);
intern void ai_gc_register_normals(a_henv env, RefQueue* rq);
intern void ai_gc_register_closable_(a_henv env, a_gptr obj);
intern void ai_gc_trace_mark_(Global* gbl, a_gptr obj);
intern void ai_gc_set_debt(Global* gbl, a_isize debt);
intern void ai_gc_incr_gc(a_henv env);
intern void ai_gc_full_gc(a_henv env, a_bool emergency);
intern void ai_gc_clean(Global* gbl);

#define ai_gc_push_stack(env,obj) ai_gc_push_stack_(env, g_as_ref(obj))
#define ai_gc_pop_stack(env,obj) quiet((env)->gc_stack = (obj)->gnext)
#define ai_gc_register_normal(env,obj) ai_gc_register_normal_(env, g_as_ref(obj))
#define ai_gc_register_closable(env,obj) ai_gc_register_closable_(env, g_as_ref(obj))

always_inline a_bool g_trace(Global* gbl, a_gptr obj) {
	if (g_has_white_color_within_assume_alive(gbl, obj)) {
		ai_gc_trace_mark_(gbl, obj);
        return true;
	}
    return false;
}

#define g_trace(gbl,obj) g_trace(gbl, g_as_ref(obj))

always_inline a_bool v_trace(Global* gbl, Value v) {
	return v_is_ref(v) && g_trace(gbl, v_as_ref(v));
}

#define ai_gc_should_run(gbl) unlikely((gbl)->mem_debt >= 0)

#ifdef ALOI_CHECK_GC
# define ai_gc_trigger_(env,pre,post) ({ ai_gc_set_debt(G(env), 0); pre; ai_gc_incr_gc(env); post; })
#else
# define ai_gc_trigger_(env,pre,post)  ({ if (ai_gc_should_run(G(env))) { pre; ai_gc_incr_gc(env); post; } })
#endif

#define ai_gc_trigger(env) ai_gc_trigger_(env, (void) 0, (void) 0)

always_inline void ai_gc_trace_work(Global* gbl, a_usize size) {
	gbl->mem_work -= cast(a_isize, size);
}

always_inline a_bool ai_gc_is_tracing(Global* gbl) {
    return gbl->gcstep <= GCSTEP_TRACE_ATOMIC;
}

always_inline a_bool ai_gc_is_sweeping(Global* gbl) {
	return gbl->gcstep >= GCSTEP_SWEEP_NORMAL && gbl->gcstep <= GCSTEP_SWEEP_ATOMIC;
}

always_inline void ai_gc_barrier_forward(a_henv env, a_gptr obj1, a_gptr obj2) {
	Global* gbl = G(env);
	if (g_has_black_color(obj1) && g_has_white_color_within_assume_alive(gbl, obj2)) {
		if (ai_gc_is_tracing(gbl)) {
			join_trace(&gbl->tr_gray, obj2);
		}
		else {
			assume(ai_gc_is_sweeping(gbl));
			g_set_gray(obj1);
		}
	}
}

#define ai_gc_barrier_forward(env,obj1,obj2) ai_gc_barrier_forward(env, g_as_ref(obj1), g_as_ref(obj2))

always_inline void ai_gc_barrier_forward_val(a_henv env, a_gptr obj, Value val) {
	if (v_is_ref(val)) {
		ai_gc_barrier_forward(env, obj, v_as_ref(val));
	}
}

#define ai_gc_barrier_forward_val(env,obj,val) ai_gc_barrier_forward_val(env, g_as_ref(obj), val)

always_inline void ai_gc_barrier_backward(a_henv env, a_gptr obj1, a_gptr obj2) {
	Global* gbl = G(env);
	if (g_has_black_color(obj1) && g_has_white_color_within_assume_alive(gbl, obj2)) {
		join_trace(&gbl->tr_regray, obj1);
	}
}

#define ai_gc_barrier_backward(env,obj1,obj2) ai_gc_barrier_backward(env, g_as_ref(obj1), g_as_ref(obj2))

always_inline void ai_gc_barrier_backward_val(a_henv env, a_gptr obj, Value val) {
	if (v_is_ref(val)) {
		ai_gc_barrier_backward(env, obj, v_as_ref(val));
	}
}

#define ai_gc_barrier_backward_val(env,obj,val) ai_gc_barrier_backward_val(env, g_as_ref(obj), val)

#endif /* agc_h_ */
