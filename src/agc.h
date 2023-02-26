/*
 * agc.h
 */

#ifndef agc_h_
#define agc_h_

#include "agbl.h"

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

always_inline a_trmark white_color(Global* g) {
    return cast(a_trmark, g->_white_color);
}

always_inline a_trmark other_color(Global* g) {
    return white_color(g) ^ (WHITE1_COLOR | WHITE2_COLOR);
}

always_inline a_bool g_is_black(a_hobj v) {
    return (v->_tnext & BLACK_COLOR) != 0;
}

always_inline a_bool g_is_gray(a_hobj v) {
    return (v->_tnext & (BLACK_COLOR | WHITE1_COLOR | WHITE2_COLOR)) == 0;
}

always_inline a_bool g_is_white(Global* g, a_hobj v) {
    return (v->_tnext & white_color(g)) != 0;
}

always_inline a_bool g_is_other(Global* g, a_hobj v) {
    return (v->_tnext & other_color(g)) != 0;
}

always_inline a_bool g_is_white_with_assume_alive(Global* g, a_hobj v) {
	assume(g_is_white(g, v));
	return (v->_tnext & (WHITE1_COLOR | WHITE2_COLOR)) != 0;
}

always_inline void g_set_white(Global* g, a_hobj v) {
	v->_tnext = white_color(g);
}

always_inline void v_check_alive(Global* g, Value v) {
	if (v_is_obj(v)) {
		GObj* obj = v_as_obj(v);
		a_u32 tag = v_get_tag(v);
		assume((tag == obj->_vtable->_tid) && !g_is_other(g, obj));
	}
}

always_inline void join_trace_(a_trmark* list, GObj* elem) {
	elem->_tnext = *list;
	*list = addr_of(elem);
}

#define join_trace(list,elem) join_trace_(list, g_cast(GObj, elem))

always_inline GObj* strip_trace(a_trmark* list) {
	GObj* elem = ptr_of(GObj, *list);
	*list = elem->_tnext;
	return elem;
}

intern void ai_gc_register_object_(a_henv env, a_hobj obj);
intern void ai_gc_register_objects(a_henv env, RefQueue* rq);
intern void ai_gc_fix_object_(a_henv env, a_hobj obj);
intern void ai_gc_trace_mark_(Global* g, a_hobj obj);
intern void ai_gc_set_debt(Global* g, a_isize debt);
intern void ai_gc_incr_gc(a_henv env);
intern void ai_gc_full_gc(a_henv env, a_bool emergency);
intern void ai_gc_clean(Global* g);

#define ai_gc_register_object(env,obj) ai_gc_register_object_(env, gobj_cast(obj))
#define ai_gc_fix_object(env,obj) ai_gc_fix_object_(env, gobj_cast(obj))
#define ai_gc_trace_mark(g,obj) ai_gc_trace_mark_(g, gobj_cast(obj))

always_inline void ai_gc_trace_mark_val(Global* g, Value v) {
	if (v_is_obj(v)) ai_gc_trace_mark_(g, v_as_obj(v));
}

#define ai_gc_should_run(g) unlikely((g)->_mem_debt >= 0)

#if ALO_STRICT_MEMORY_CHECK
# define ai_gc_trigger_ext(env,pre,post) ({ ai_gc_set_debt(G(env), 0); pre; ai_gc_incr_gc(env); post; })
#else
# define ai_gc_trigger_ext(env,pre,post)  ({ if (ai_gc_should_run(G(env))) { pre; ai_gc_incr_gc(env); post; } })
#endif
#define ai_gc_trigger(env) ai_gc_trigger_ext(env, (void) 0, (void) 0)

always_inline void ai_gc_trace_work(Global* g, a_usize size) {
	g->_mem_work -= cast(a_isize, size);
}

always_inline void ai_gc_barrier_(a_henv env, a_hobj obj1, a_hobj obj2) {
	Global* g = G(env);
	if (g_is_black(obj1) && g_is_white_with_assume_alive(g, obj2)) {
		join_trace(&g->_tr_gray, obj2);
	}
}

#define ai_gc_barrier(env,obj1,obj2) ai_gc_barrier_(env, gobj_cast(obj1), gobj_cast(obj2))

always_inline void ai_gc_barrier_val_(a_henv env, GObj* obj, Value val) {
	if (v_is_obj(val)) {
		ai_gc_barrier_(env, obj, v_as_obj(val));
	}
}

#define ai_gc_barrier_val(env,obj,val) ai_gc_barrier_val_(env, gobj_cast(obj), val)

always_inline void ai_gc_barrier_back_(a_henv env, a_hobj obj1, a_hobj obj2) {
	Global* g = G(env);
	if (g_is_black(obj1) && g_is_white_with_assume_alive(g, obj2)) {
		join_trace(&g->_tr_regray, obj1);
	}
}

#define ai_gc_barrier_back(env,obj1,obj2) ai_gc_barrier_back_(env, gobj_cast(obj1), gobj_cast(obj2))

always_inline void ai_gc_barrier_back_val_(a_henv env, GObj* obj, Value val) {
	if (v_is_obj(val)) {
		ai_gc_barrier_back_(env, obj, v_as_obj(val));
	}
}

#define ai_gc_barrier_back_val(env,obj,val) ai_gc_barrier_back_val_(env, gobj_cast(obj), val)

#endif /* agc_h_ */
