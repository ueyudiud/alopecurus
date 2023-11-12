/*
 * agc.h
 */

#ifndef agc_h_
#define agc_h_

#include "aobj.h"

typedef struct {
	a_gclist _head;
	a_gclist* _tail;
} RefQueue;

always_inline void rq_init(RefQueue* rq) {
	rq->_tail = &rq->_head;
}

always_inline void rq_push(RefQueue* rq, void* ref) {
	a_hobj obj = cast(a_hobj, ref);
	*rq->_tail = obj;
	rq->_tail = &obj->_gnext;
}

#define rq_for(v,rq) for ( \
	a_hobj *_p_##v = &(rq)->_head, v; \
	_p_##v != (rq)->_tail && ((v) = *_p_##v, true); \
	_p_##v = &(v)->_gnext \
)

#define GCSTEP_PROPAGATE u8c(0)
#define GCSTEP_PROPAGATE_ATOMIC u8c(1)
#define GCSTEP_SWEEP_NORMAL u8c(2)
#define GCSTEP_SWEEP_ATOMIC u8c(3)
#define GCSTEP_CLOSE u8c(4)
#define GCSTEP_PAUSE u8c(5)

#define WHITE1_COLOR 0x1
#define WHITE2_COLOR 0x2
#define BLACK_COLOR 0x4

#define GRAY_NULL ((a_trmark) 0)

always_inline a_trmark white_color(Global* gbl) {
    return cast(a_trmark, gbl->_white_color);
}

always_inline a_trmark other_color(Global* gbl) {
    return white_color(gbl) ^ (WHITE1_COLOR | WHITE2_COLOR);
}

always_inline a_bool g_has_black_color(a_hobj o) {
    return (o->_tnext & BLACK_COLOR) != 0;
}

#define g_has_black_color(o) g_has_black_color(gobj_cast(o))

always_inline a_bool g_has_gray_color(a_hobj o) {
    return (o->_tnext & (BLACK_COLOR | WHITE1_COLOR | WHITE2_COLOR)) == 0;
}

#define g_has_gray_color(o) g_has_gray_color(gobj_cast(o))

always_inline a_bool g_has_white_color(Global* gbl, a_hobj o) {
    return (o->_tnext & white_color(gbl)) != 0;
}

#define g_has_white_color(gbl,o) g_has_white_color(gbl, gobj_cast(o))

always_inline a_bool g_has_other_color(Global* gbl, a_hobj o) {
    return (o->_tnext & other_color(gbl)) != 0;
}

#define g_has_other_color(gbl,v) g_has_other_color(gbl, gobj_cast(v))

always_inline a_bool g_has_white_color_within_assume_alive(Global* gbl, a_hobj o) {
	assume(!g_has_other_color(gbl, o));
	return (o->_tnext & (WHITE1_COLOR | WHITE2_COLOR)) != 0;
}

#define g_has_white_color_within_assume_alive(gbl,o) g_has_white_color_within_assume_alive(gbl, gobj_cast(o))

always_inline void g_set_white(Global* gbl, a_hobj o) {
    o->_tnext = white_color(gbl);
}

#define g_set_white(gbl,o) g_set_white(gbl, gobj_cast(o))

always_inline void g_set_gray(a_hobj v) {
	v->_tnext = GRAY_NULL;
}

#define g_set_gray(o) g_set_gray(gobj_cast(o))

always_inline void g_set_black(a_hobj o) {
    o->_tnext = BLACK_COLOR;
}

#define g_set_black(o) g_set_black(gobj_cast(o))

always_inline void v_check_alive(a_henv env, Value v) {
	if (v_is_obj(v)) {
        a_hobj p = v_as_obj(v);
		a_usize stencil = v._ ^ p->_vptr->_stencil;
		assume((stencil & ~V_PAYLOAD_MASK) == 0 && !g_has_other_color(G(env), p));
	}
}

always_inline void join_trace(a_trmark* list, a_hobj o) {
    o->_tnext = *list;
	*list = ptr2int(o);
}

#define join_trace(list,o) join_trace(list, gobj_cast(o))

always_inline a_hobj strip_trace(a_trmark* list) {
    a_hobj o = int2ptr(GObj, *list);
	*list = o->_tnext;
	return o;
}

intern void ai_gc_register_object_(a_henv env, a_hobj obj);
intern void ai_gc_register_objects(a_henv env, RefQueue* rq);
intern void ai_gc_fix_object_(a_henv env, a_hobj obj);
intern void ai_gc_trace_mark_(Global* gbl, a_hobj obj);
intern void ai_gc_set_debt(Global* gbl, a_isize debt);
intern void ai_gc_incr_gc(a_henv env);
intern void ai_gc_full_gc(a_henv env, a_bool emergency);
intern void ai_gc_clean(Global* gbl);

#define ai_gc_register_object(env,obj) ai_gc_register_object_(env, gobj_cast(obj))
#define ai_gc_fix_object(env,obj) ai_gc_fix_object_(env, gobj_cast(obj))

always_inline void ai_gc_trace_mark(Global* gbl, a_hobj obj) {
	if (g_has_white_color_within_assume_alive(gbl, obj)) {
		ai_gc_trace_mark_(gbl, obj);
	}
}

#define ai_gc_trace_mark(gbl,obj) ai_gc_trace_mark(gbl, gobj_cast(obj))

always_inline void ai_gc_trace_mark_val(Global* gbl, Value v) {
	if (v_is_obj(v)) ai_gc_trace_mark(gbl, v_as_obj(v));
}

#define ai_gc_should_run(gbl) unlikely((gbl)->_mem_debt >= 0)

#ifdef ALOI_CHECK_GC
# define ai_gc_trigger_(env,pre,post) ({ ai_gc_set_debt(G(env), 0); pre; ai_gc_incr_gc(env); post; })
#else
# define ai_gc_trigger_(env,pre,post)  ({ if (ai_gc_should_run(G(env))) { pre; ai_gc_incr_gc(env); post; } })
#endif

#define ai_gc_trigger(env) ai_gc_trigger_(env, (void) 0, (void) 0)

always_inline void ai_gc_trace_work(Global* gbl, a_usize size) {
	gbl->_mem_work -= cast(a_isize, size);
}

always_inline a_bool ai_gc_is_tracing(Global* gbl) {
	return gbl->_gcstep <= GCSTEP_PROPAGATE_ATOMIC;
}

always_inline a_bool ai_gc_is_sweeping(Global* gbl) {
	return gbl->_gcstep >= GCSTEP_SWEEP_NORMAL && gbl->_gcstep <= GCSTEP_SWEEP_ATOMIC;
}

always_inline void ai_gc_barrier_forward(a_henv env, a_hobj obj1, a_hobj obj2) {
	Global* gbl = G(env);
	if (g_has_black_color(obj1) && g_has_white_color_within_assume_alive(gbl, obj2)) {
		if (ai_gc_is_tracing(gbl)) {
			join_trace(&gbl->_tr_gray, obj2);
		}
		else {
			assume(ai_gc_is_sweeping(gbl));
			g_set_gray(obj1);
		}
	}
}

#define ai_gc_barrier_forward(env,obj1,obj2) ai_gc_barrier_forward(env, gobj_cast(obj1), gobj_cast(obj2))

always_inline void ai_gc_barrier_forward_val(a_henv env, a_hobj obj, Value val) {
	if (v_is_obj(val)) {
		ai_gc_barrier_forward(env, obj, v_as_obj(val));
	}
}

#define ai_gc_barrier_forward_val(env,obj,val) ai_gc_barrier_forward_val(env, gobj_cast(obj), val)

always_inline void ai_gc_barrier_backward(a_henv env, a_hobj obj1, a_hobj obj2) {
	Global* gbl = G(env);
	if (g_has_black_color(obj1) && g_has_white_color_within_assume_alive(gbl, obj2)) {
		join_trace(&gbl->_tr_regray, obj1);
	}
}

#define ai_gc_barrier_backward(env,obj1,obj2) ai_gc_barrier_backward(env, gobj_cast(obj1), gobj_cast(obj2))

always_inline void ai_gc_barrier_backward_val(a_henv env, a_hobj obj, Value val) {
	if (v_is_obj(val)) {
		ai_gc_barrier_backward(env, obj, v_as_obj(val));
	}
}

#define ai_gc_barrier_backward_val(env,obj,val) ai_gc_barrier_backward_val(env, gobj_cast(obj), val)

#endif /* agc_h_ */
