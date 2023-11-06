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
	GRoute _route;
	Global _global;
	a_byte _reserved[];
} MRoute;

static MRoute* g_mroute(Global* g) {
	return from_member(MRoute, _global, g);
}

static a_bool route_is_main(a_henv env) {
	return ai_env_mroute(G(env)) == env;
}

static a_bool route_is_active(a_henv env) {
	return G(env)->_active == env;
}

static VTable const route_vtable;

static void route_new(GRoute* self, Global* g) {
    init(self) {
        ._vptr = &route_vtable,
        ._status = ALO_SYIELD,
        ._flags = 0,
        ._g = g,
        ._error = v_of_nil(),
        ._frame = &self->_base_frame,
        ._base_frame = { }
    };
}

static a_bool route_init(a_henv env, GRoute* self) {
	if (ai_stk_init(env, &self->_stack)) return true;
	self->_base_frame._stack_bot = val2stk(env, self->_stack._base);
	return false;
}

static void route_close(Global* g, GRoute* self) {
	a_henv env = ai_env_mroute(g);
	RcCap* caps = self->_open_caps;
	RcCap* cap;
	while ((cap = caps) != null) {
		caps = cap->_next;
		ai_cap_close(env, cap);
	}
}

static void route_destroy(Global* g, GRoute* self) {
	route_close(g, self);
	ai_stk_deinit(g, &self->_stack);
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
	ai_gc_trace_work(g, sizeof(GRoute));

	route_mark_stack(g, self);
	if (self->_from != null) {
		ai_gc_trace_mark(g, self->_from);
	}

	join_trace(&g->_tr_regray, self);
}

static void route_drop(Global* g, GRoute* self) {
	assume(self->_status != ALO_SOK, "route is running.");
	ai_ctx_close(self);
	route_destroy(g, self);
	ai_mem_vdel(g, self, sizeof(GRoute));
}

static VTable const route_vtable = {
	._stencil = V_STENCIL(T_USER),
	._flags = VTABLE_FLAG_NONE,
    ._type_ref = g_type_ref(_route),
	._slots = {
        [vfp_slot(drop)] = route_drop,
        [vfp_slot(mark)] = route_mark
	}
};

a_msg ai_env_resume(a_henv env, GRoute* self) {
	assume(self->_status == ALO_SYIELD && self->_from == null && route_is_active(env));
	self->_status = ALO_SOK;
	self->_from = env;
	return ai_ctx_jump(self, env, ALO_SOK);
}

void ai_env_yield(a_henv env) {
	assume(!route_is_main(env) && env->_status == ALO_SOK && route_is_active(env));
	a_henv caller = env->_from;
	env->_status = ALO_SYIELD;
	env->_from = null;
	ai_ctx_jump(caller, env, ALO_SOK);
}

a_msg ai_env_pcall(a_henv env, a_pfun pfun, void* pctx) {
	return ai_ctx_catch(env, pfun, pctx);
}

a_none ai_env_raise(a_henv env, a_msg msg) {
	ai_ctx_raise(env, msg);
}

GRoute* ai_env_new(a_henv env, a_usize stack_size) {
	GRoute* self = ai_mem_gnew(env, GRoute, sizeof(GRoute));

	route_new(self, G(env));

    a_msg msg = ai_ctx_open(self, stack_size);
	if (msg != ALO_SOK)
		goto nomem1;
	if (route_init(env, self))
		goto nomem2;

    ai_gc_register_object(env, self);
	return self;

nomem2:
	ai_ctx_close(self);
nomem1:
	ai_mem_dealloc(G(env), self, sizeof(GRoute));
	ai_mem_nomem(env);
}

static void global_init(a_henv env, unused void* ctx) {
	MRoute* m = from_member(MRoute, _route, env);

    ai_str_boost(env, m->_reserved);
    ai_type_boost(env);

    GType* gbl = ai_type_new(env, g_str(env, STR_EMPTY), null, 0);
    v_set_obj(env, &G(env)->_global, gbl);
}

static a_usize sizeof_MRoute() {
	a_usize size = sizeof(MRoute) + sizeof(GcHead) + sizeof_GStr(0)
#define STRDEF(n) + sizeof(GcHead) + sizeof_GStr(sizeof(#n) - 1)
# include "asym/kw.h"
# include "asym/tm.h"
# include "asym/pt.h"
#undef STRDEF
	;
	return size;
}

a_msg alo_create(alo_Alloc const* af, void* ac, a_henv* penv) {
	a_usize size = sizeof_MRoute();
	MRoute* mr = ai_mem_valloc(af, ac, size);
	if (mr == null) return ALO_ENOMEM;

	Global* g = &mr->_global;
	GRoute* env = &mr->_route;

    init(g) {
        ._af = *af,
        ._ac = ac,
        ._active = env,
        ._mem_base = size,
        ._flags = GLOBAL_FLAG_DISABLE_GC,
        ._gcpausemul = ALOI_DFL_GCPAUSEMUL,
        ._gcstepmul = ALOI_DFL_GCSTEPMUL,
        ._white_color = WHITE1_COLOR,
        ._gcstep = GCSTEP_PAUSE,
        ._global = v_of_nil(),
        ._hookm = ALO_HMNONE
    };
	
	route_new(env, g);

	/* Initialize context and stack of route. */
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
	MRoute* mroute = g_mroute(g);

	if (!route_is_main(env)) {
		a_henv menv = &mroute->_route;
		/* Swap stack sensible fields. */
        swap(menv->_rctx, env->_rctx);
        swap(menv->_rctx_alloc, env->_rctx_alloc);
        swap(menv->_pctx, env->_pctx);
		swap(menv->_from, env->_from);
	}

	/* Clean resources. */
	route_close(g, &mroute->_route);
	ai_gc_clean(g);
	ai_str_clean(g);
    ai_type_clean(g);
	ai_cap_clean(g);

	assume(gbl_mem_total(g) == sizeof_MRoute(), "memory leak.");
	ai_mem_vdealloc(&g->_af, g->_ac, mroute, sizeof_MRoute());
}

a_henv ai_env_mroute(Global* g) {
	return &g_mroute(g)->_route;
}
