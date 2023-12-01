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
#include "afun.h"
#include "atype.h"
#include "actx.h"
#include "amem.h"
#include "agc.h"

#include "aenv.h"

/* Main route. */
typedef struct MRoute {
	GRoute _route;
	Global _global;
	a_byte _reserved[];
} MRoute;

static MRoute* g_mroute(Global* gbl) {
	return from_member(MRoute, _global, gbl);
}

static a_bool route_is_main(a_henv env) {
	return ai_env_mroute(G(env)) == env;
}

static a_bool route_is_active(a_henv env) {
	return G(env)->_active == env;
}

static VTable const route_vtable;

static void route_new(GRoute* self, Global* gbl) {
    init(self) {
        ._vptr = &route_vtable,
        ._status = ALO_SYIELD,
        ._flags = 0,
        ._global = gbl,
        ._error = v_of_nil(),
        ._frame = &self->_base_frame,
        ._base_frame = { }
    };
}

static a_bool route_init(a_henv env, GRoute* self) {
	catch (ai_stk_init(env, &self->_stack)) {
        return true;
    }
	self->_base_frame._stack_bot = val2stk(env, self->_stack._base);
	return false;
}

static void route_close(Global* gbl, GRoute* self) {
	a_henv env = ai_env_mroute(gbl);
	RcCap* caps = self->_open_caps;
	RcCap* cap;
	while ((cap = caps) != null) {
		caps = cap->_next;
		ai_cap_close(env, cap);
	}
}

static void route_destroy(Global* gbl, GRoute* self) {
	route_close(gbl, self);
	ai_stk_deinit(gbl, &self->_stack);
}

static void route_mark_stack(Global* gbl, GRoute* self) {
	Stack* stack = &self->_stack;
	Value* from = stack->_base;
	Value* const to =
			/* Mark object conservative in increasing GC: assume all object in the last frame will be freed. */
			gbl->_gcstep == GCSTEP_PROPAGATE_ATOMIC ?
			stack->_top :
			stk2val(self, self->_frame->_stack_bot);
	for (Value const* v = from; v < to; ++v) {
		ai_gc_trace_mark_val(gbl, *v);
	}
	if (gbl->_gcstep == GCSTEP_PROPAGATE_ATOMIC) {
		v_set_nil_ranged(stack->_top, stack->_limit); /* Clear no-marked stack slice. */
	}
	else if (!(gbl->_flags & GLOBAL_FLAG_EMERGENCYGC)) {
		ai_stk_shrink(self);
	}
#if ALO_STACK_INNER
	ai_gc_trace_work(gbl, stack->_alloc_size);
#endif
}

static void route_mark(Global* gbl, GRoute* self) {
	ai_gc_trace_work(gbl, route_size());

	route_mark_stack(gbl, self);
	if (self->_from != null) {
		ai_gc_trace_mark(gbl, self->_from);
	}

	join_trace(&gbl->_tr_regray, self);
}

static void route_drop(Global* gbl, GRoute* self) {
	assume(self->_status != ALO_SOK, "route is running.");
	ai_ctx_close(self);
	route_destroy(gbl, self);
	ai_mem_dealloc(gbl, self, route_size());
}

static VTable const route_vtable = {
	._stencil = V_STENCIL(T_USER),
    ._tag = ALO_TROUTE,
	._flags = VTABLE_FLAG_NONE,
    ._type_ref = g_type_ref(ALO_TROUTE),
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

a_noret ai_env_raise(a_henv env, a_msg msg) {
	ai_ctx_raise(env, msg);
}

GRoute* ai_env_new(a_henv env, a_usize stack_size) {
	GRoute* self = ai_mem_alloc(env, route_size());

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
	ai_mem_dealloc(G(env), self, route_size());
	ai_mem_nomem(env);
}

static void global_init(a_henv env, unused void* ctx) {
	MRoute* m = from_member(MRoute, _route, env);

    ai_str_boost1(env, m->_reserved);
    ai_type_boost(env);
    ai_str_boost2(env);

    GMod* gbl = ai_mod_new(env, 0);
    v_set_obj(env, &G(env)->_global, gbl);
}

static a_usize mroute_size() {
	a_usize size = sizeof(MRoute) + str_size(0)
#define STRDEF(n) + str_size(sizeof(#n) - 1)
# include "asym/kw.h"
# include "asym/tm.h"
# include "asym/pt.h"
#undef STRDEF
	;
	return size;
}

a_msg alo_create(alo_Alloc const* af, void* ac, a_henv* penv) {
	a_usize size = mroute_size();
	MRoute* mr = ai_mem_valloc(af, ac, size);
	if (mr == null) return ALO_ENOMEM;

	Global* gbl = &mr->_global;
	GRoute* env = &mr->_route;

    init(gbl) {
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
	
	route_new(env, gbl);

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
	Global* gbl = G(env);
	MRoute* mroute = g_mroute(gbl);

	if (!route_is_main(env)) {
		a_henv menv = &mroute->_route;
		/* Swap stack sensible fields. */
        swap(menv->_rctx, env->_rctx);
        swap(menv->_rctx_alloc, env->_rctx_alloc);
        swap(menv->_pctx, env->_pctx);
		swap(menv->_from, env->_from);
	}

	/* Clean resources. */
	route_close(gbl, &mroute->_route);
	ai_gc_clean(gbl);
	ai_str_clean(gbl);
    ai_type_clean(gbl);
	ai_cap_clean(gbl);

#ifndef ALOI_USE_VALGRIND
    /* Avoid used memory check if foreign memcheck tools is enabled. */
	assume(gbl_mem_total(gbl) == mroute_size(), "memory leak.");
#endif
	ai_mem_vdealloc(&gbl->_af, gbl->_ac, mroute, mroute_size());
}

a_henv ai_env_mroute(Global* gbl) {
	return &g_mroute(gbl)->_route;
}
