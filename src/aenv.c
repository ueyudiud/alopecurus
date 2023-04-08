/**
 *@file aenv.c
 */

#define aenv_c_
#define ALO_LIB

#include "abc.h"
#include "astr.h"
#include "atuple.h"
#include "alist.h"
#include "atable.h"
#include "atype.h"
#include "actx.h"
#include "amem.h"
#include "agc.h"
#include "aerr.h"

#include "aenv.h"

/* Main route. */
typedef struct MRoute {
	Route _route;
	Global _global;
	a_byte _reserved[];
} MRoute;

static MRoute* route_main(Global* g) {
	return from_member(MRoute, _global, g);
}

static a_bool route_is_main(a_henv env) {
	return &route_main(G(env))->_route._body == env;
}

static a_bool route_is_active(a_henv env) {
	return G(env)->_active == env;
}

static Route* env2route(a_henv env) {
	return from_member(Route, _body, env);
}

static VTable const route_vtable;

static void route_new(GRoute* self, Global* g) {
	*self = new(GRoute) {
		._vptr = &route_vtable,
		._status = ALO_SYIELD,
		._flags = 0,
		._g = g,
		._from = null,
		._error = v_of_nil(),
		._frame = &self->_base_frame,
		._base_frame = new(Frame) {
			._prev = null,
			._pc = null
		}
	};
}

static a_bool route_init(a_henv env, GRoute* self) {
	if (ai_stk_init(env, &self->_stack)) return true;
	self->_base_frame._stack_bot = val2stk(env, self->_stack._base);
	return false;
}

static void route_destroy(Global* g, GRoute* self) {
	a_henv env = &route_main(g)->_route._body;
	ai_stk_deinit(g, &self->_stack);
	for (Frame* frame = self->_frame; frame != null; frame = frame->_prev) {
		RcCap* caps = frame->_caps;
		RcCap* cap;
		while ((cap = caps) != null) {
			ai_cap_hard_close(env, cap);
		}
	}
}

static void route_mark_stack(Global* g, GRoute* self) {
	Stack* stack = &self->_stack;
	Value* from = stack->_base;
	Value* const to =
			/* Mark object conservative in increasing GC: assume all object in the last frame will be freed. */
			g->_gcstep == GCSTEP_PROPAGATE_ATOMIC ?
			stack->_top :
			stk2val(self, self->_frame->_stack_bot);
	for (Value const* v = from; v < to; ++v) {
		ai_gc_trace_mark_val(g, *v);
	}
	if (g->_gcstep == GCSTEP_PROPAGATE_ATOMIC) {
		v_set_nil_ranged(stack->_top, stack->_limit); /* Clear no-marked stack slice. */
	}
	else if (!(g->_flags & GLOBAL_FLAG_EMERGENCYGC)) {
		ai_stk_shrink(self);
	}
#if ALO_STACK_INNER
	ai_gc_trace_work(g, stack->_alloc_size);
#endif
}

static void route_mark(Global* g, GRoute* self) {
	route_mark_stack(g, self);
	if (self->_from != null) {
		ai_gc_trace_mark(g, self->_from);
	}
	ai_gc_trace_work(g, sizeof(GRoute));

	join_trace(&g->_tr_regray, self);
}

static void route_drop(Global* g, GRoute* self) {
	assume(self->_status != ALO_SOK, "route is running.");
	Route* route = env2route(self);
	ai_ctx_close(route);
	route_destroy(g, self);
	ai_mem_dealloc(g, route, sizeof(Route));
}

static VTable const route_vtable = {
	._mask = V_MASKED_TAG(T_USER_TEQ),
	._iname = env_type_iname(_route),
	._body = {
		vfp_def(drop, route_drop),
		vfp_def(mark, route_mark)
	}
};

a_msg ai_env_resume(a_henv env, GRoute* self) {
	assume(self->_status == ALO_SYIELD && self->_from == null && route_is_active(env));
	self->_status = ALO_SOK;
	self->_from = env;
	return ai_ctx_resume(env2route(env), env2route(self));
}

void ai_env_yield(a_henv env) {
	assume(!route_is_main(env) && env->_status == ALO_SOK && route_is_active(env));
	a_henv from_env = env->_from;
	env->_status = ALO_SYIELD;
	env->_from = null;
	ai_ctx_yield(env2route(env), env2route(from_env), ALO_SOK);
}

a_msg ai_env_pcall(a_henv env, a_pfun pfun, void* pctx) {
	return ai_ctx_catch(env2route(env), pfun, pctx);
}

a_none ai_env_raise(a_henv env, a_msg msg) {
	ai_ctx_raise(env2route(env), msg);
}

GRoute* ai_env_new(a_henv env, a_usize stack_size) {
	Route* self = ai_mem_alloc(env, sizeof(Route));
	route_new(&self->_body, G(env));
	if (route_init(env, &self->_body)) {
		ai_mem_dealloc(G(env), self, sizeof(Route));
		ai_mem_nomem(env);
	}
	ai_gc_register_object(env, &self->_body);
	ai_ctx_open(self, stack_size);
	return &self->_body;
}

static void global_init(a_henv env, unused void* ctx) {
	MRoute* m = from_member(MRoute, _route, env2route(env));
	ai_str_boost(env);
	ai_obj_boost(env, m->_reserved);

	GTable* gtable = ai_table_new(env);
	v_set_obj(env, &G(env)->_global, gtable);
}

static a_usize sizeof_MRoute() {
	return sizeof(MRoute) + sizeof_IStr(0)
#define SIZE(id,name) + sizeof_IStr(sizeof(name) - 1)
		KW_LIST(SIZE)
		TM_LIST(SIZE)
#undef SIZE
			;
	;
}

a_msg alo_create(a_alloc const* af, void* ac, a_henv* penv) {
	a_usize size = sizeof_MRoute();
	MRoute* mr = ai_mem_valloc(af, ac, size);
	if (mr == null) return ALO_ENOMEM;

	Global* g = &mr->_global;
	Route* route = &mr->_route;
	a_henv env = &route->_body;

	mr->_global = new(Global) {
		._af = *af,
		._ac = ac,
		._active = env,
		._gc_normal = null,
		._gc_closable = null,
		._gc_toclose = null,
		._gc_sweep = null,
		._mem_base = size,
		._mem_debt = 0,
		._flags = GLOBAL_FLAG_DISABLE_GC,
		._gcpausemul = ALOI_DFL_GCPAUSEMUL,
		._gcstepmul = ALOI_DFL_GCSTEPMUL,
		._white_color = WHITE1_COLOR,
		._gcstep = GCSTEP_PAUSE,
		._global = v_of_nil(),
		._hookm = ALO_HMNONE
	};
	
	route_new(env, g);

	/* Intialize context and stack of route. */
	route->_ctx = new(RCtx) { };
	if (route_init(env, env)) return ALO_ENOMEM;

	/* Initialize remaining components. */
	a_msg msg = ai_env_pcall(env, global_init, null);

	if (unlikely(msg != ALO_SOK)) {
		alo_destroy(env);
		return msg;
	}

	*penv = env;
	return ALO_SOK;
}

void alo_destroy(a_henv env) {
	Global* g = G(env);
	MRoute* mr = route_main(g);

	if (!route_is_main(env)) {
		/* Swap user stack to ensure stack is keeping. */
		ai_ctx_yield(env2route(env), &mr->_route, ALO_SEXIT);
	}

	env->_status = ALO_SEXIT; /* Mark status to exit. */

	/* Clean resources. */
	ai_gc_clean(g);
	ai_str_clean(g);
	ai_type_clean(g);
	route_destroy(g, &mr->_route._body);

	assume(gbl_mem_total(g) == sizeof_MRoute(), "memory leak.");
	ai_mem_vdealloc(&g->_af, g->_ac, mr, sizeof_MRoute());
}

a_henv ai_env_mainof(Global* g) {
	return &route_main(g)->_route._body;
}
