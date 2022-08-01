/*
 * agc.h
 */

#ifndef agc_h_
#define agc_h_

#include "aenv.h"

typedef struct {
	a_gclist _head;
	a_gclist* _tail;
} RefQueue;

inline void rq_init(RefQueue* rq) {
	rq->_tail = &rq->_head;
}

inline void rq_push(RefQueue* rq, void* ref) {
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

inline a_trmark white_color(Global* g) {
    return cast(a_trmark, g->_white_color);
}

inline a_trmark other_color(Global* g) {
    return white_color(g) ^ (WHITE1_COLOR | WHITE2_COLOR);
}

inline a_bool g_is_black(a_hobj v) {
    return (v->_tnext & BLACK_COLOR) != 0;
}

inline a_bool g_is_gray(a_hobj v) {
    return (v->_tnext & (BLACK_COLOR | WHITE1_COLOR | WHITE2_COLOR)) == 0;
}

inline a_bool g_is_white(Global* g, a_hobj v) {
    return (v->_tnext & white_color(g)) != 0;
}

inline a_bool g_is_other(Global* g, a_hobj v) {
    return (v->_tnext & other_color(g)) != 0;
}

inline void g_set_white(Global* g, a_hobj v) {
	v->_tnext = white_color(g);
}

inline void v_check_alive(Global* g, Value const* v) {
	if (v_is_ref(v)) {
		GObj* obj = cast(GObj*, v_as_hnd(v));
		a_u32 tag = v_raw_tag(v);
		assume((tag == obj->_meta->_tid || (tag == T_OTHER && obj->_meta->_tid > T__MAX)) && !g_is_other(g, obj));
	}
}

inline void join_trace_(a_trmark* list, GObj* elem) {
	elem->_tnext = *list;
	*list = addr_of(elem);
}

#define join_trace(list,elem) join_trace_(list, downcast(GObj, elem))

inline GObj* strip_trace(a_trmark* list) {
	GObj* elem = ptr_of(GObj, *list);
	*list = elem->_tnext;
	return elem;
}

intern void ai_gc_register_object_(a_henv env, a_hobj obj);
intern void ai_gc_register_objects(a_henv env, RefQueue* rq);
intern void ai_gc_fix_object_(a_henv env, a_hobj obj);
intern void ai_gc_trace_mark_(Global* g, a_hobj obj);
intern void ai_gc_incr_gc(a_henv env);
intern void ai_gc_full_gc(a_henv env, a_bool emergency);
intern void ai_gc_clean(Global* g);

#define ai_gc_register_object(env,obj) ai_gc_register_object_(env, downcast(GObj, obj))
#define ai_gc_fix_object(env,obj) ai_gc_fix_object_(env, downcast(GObj, obj))
#define ai_gc_trace_mark(g,obj) ai_gc_trace_mark_(g, downcast(GObj, obj))

inline void ai_gc_trace_markv(Global* g, Value const* v) {
	if (v_is_ref(v)) ai_gc_trace_mark_(g, v_as_obj(g, v));
}

inline a_bool ai_gc_should_run(Global* g) {
    return g->_mem_debt >= 0;
}

#define ai_gc_trigger(env) ({ if (ai_gc_should_run((env)->_g)) ai_gc_incr_gc(env); })

#endif /* agc_h_ */
