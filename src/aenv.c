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
	GRoute route;
	Global global;
	a_byte reserved[];
} MRoute;

static MRoute* g_mroute(Global* gbl) {
	return from_member(MRoute, global, gbl);
}

static a_bool route_is_main(a_henv env) {
	return ai_env_mroute(G(env)) == env;
}

static a_bool route_is_active(a_henv env) {
	return G(env)->active == env;
}

static VTable const route_vtable;

static void route_new(GRoute* self, Global* gbl) {
    init(self) {
        .vptr = &route_vtable,
        .status = ALO_SYIELD,
        .flags = 0,
        .global = gbl,
        .error = v_of_nil(),
        .frame = &self->base_frame,
        .base_frame = { }
    };
}

static a_bool route_init(a_henv env, GRoute* self) {
	try (ai_stk_init(env, &self->stack));
	self->base_frame.stack_bot = val2stk(env, self->stack.base);
	return false;
}

static void route_close(Global* gbl, GRoute* self) {
	a_henv env = ai_env_mroute(gbl);
	RcCap* caps = self->open_caps;
	RcCap* cap;
	while ((cap = caps) != null) {
		caps = cap->next;
		ai_cap_close(env, cap);
	}
}

static void route_destroy(Global* gbl, GRoute* self) {
	route_close(gbl, self);
	ai_stk_deinit(gbl, &self->stack);
}

static void route_mark_stack(Global* gbl, GRoute* self) {
	Stack* stack = &self->stack;
	Value* from = stack->base;
	Value* const to =
			/* Mark object conservative in increasing GC: assume all object in the last frame will be freed. */
			gbl->gcstep == GCSTEP_PROPAGATE_ATOMIC ?
			stack->top :
			stk2val(self, self->frame->stack_bot);
	for (Value const* v = from; v < to; ++v) {
		ai_gc_trace_mark_val(gbl, *v);
	}
	if (gbl->gcstep == GCSTEP_PROPAGATE_ATOMIC) {
		v_set_nil_ranged(stack->top, stack->limit); /* Clear no-marked stack slice. */
	}
	else if (!(gbl->flags & GLOBAL_FLAG_EMERGENCYGC)) {
		ai_stk_shrink(self);
	}
#if ALO_STACK_INNER
	ai_gc_trace_work(gbl, stack->alloc_size);
#endif
}

static void route_mark(Global* gbl, GRoute* self) {
	ai_gc_trace_work(gbl, route_size());

	route_mark_stack(gbl, self);
	if (self->caller != null) {
		ai_gc_trace_mark(gbl, self->caller);
	}

	join_trace(&gbl->tr_regray, self);
}

static void route_drop(Global* gbl, GRoute* self) {
	assume(self->status != ALO_SOK, "route is running.");
	ai_ctx_close(self);
	route_destroy(gbl, self);
	ai_mem_dealloc(gbl, self, route_size());
}

static VTable const route_vtable = {
    .tag = ALO_TROUTE,
	.flags = VTABLE_FLAG_NONE,
    .type_ref = g_type_ref(ALO_TROUTE),
	.impl = {
        .drop = cast(void const*, route_drop),
        .mark = cast(void const*, route_mark)
	}
};

a_msg ai_env_resume(a_henv env, GRoute* self) {
	assume(self->status == ALO_SYIELD && self->caller == null && route_is_active(env));
	self->status = ALO_SOK;
	self->caller = env;
	return ai_ctx_jump(self, env, ALO_SOK);
}

void ai_env_yield(a_henv env) {
	assume(!route_is_main(env) && env->status == ALO_SOK && route_is_active(env));
	a_henv caller = env->caller;
	env->status = ALO_SYIELD;
	env->caller = null;
	ai_ctx_jump(caller, env, ALO_SOK);
}

a_msg ai_env_pcall(a_henv env, a_pfun pfun, void* pctx, Value* errf) {
    StkPtr old_errf = env->errf;
    env->errf = errf != null ? val2stk(env, errf) : 0;
	a_msg msg = ai_ctx_catch(env, pfun, pctx);
    env->errf = old_errf;
    return msg;
}

a_noret ai_env_raise(a_henv env, a_msg msg) {
	ai_ctx_raise(env, msg);
}

GRoute* ai_env_new(a_henv env, a_usize stack_size) {
	GRoute* self = ai_mem_alloc(env, route_size());

	route_new(self, G(env));

    catch (ai_ctx_open(self, stack_size)) {
        goto nomem1;
    }
    catch (route_init(env, self)) {
        goto nomem2;
    }

    ai_gc_register_object(env, self);
	return self;

nomem2:
	ai_ctx_close(self);
nomem1:
	ai_mem_dealloc(G(env), self, route_size());
	ai_mem_nomem(env);
}

static void global_init(a_henv env, unused void* ctx) {
	MRoute* m = from_member(MRoute, route, env);

    ai_str_boost1(env, m->reserved);
    ai_type_boost(env);
    ai_str_boost2(env);

    GMod* gbl = ai_mod_new(env, 0);
    v_set_mod(env, &G(env)->global_value, gbl);
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

	Global* gbl = &mr->global;
	GRoute* env = &mr->route;

    init(gbl) {
        .alloc_ = *af,
        .alloc_ctx = ac,
        .active = env,
        .mem_base = size,
        .flags = GLOBAL_FLAG_DISABLE_GC,
        .gcpausemul = ALOI_DFL_GCPAUSEMUL,
        .gcstepmul = ALOI_DFL_GCSTEPMUL,
        .white_bit = WHITE1_COLOR,
        .gcstep = GCSTEP_PAUSE,
        .global_value = v_of_nil(),
        .hookm = ALO_HMNONE
    };
	
	route_new(env, gbl);

	/* Initialize context and stack of route. */
	if (route_init(env, env)) return ALO_ENOMEM;

	/* Initialize remaining components. */
	catch (ai_env_pcall(env, global_init, null, null), msg) {
        alo_destroy(env);
        return msg;
    }

    env->status = ALO_SOK;
	*penv = env;
	return ALO_SOK;
}

void alo_destroy(a_henv env) {
	Global* gbl = G(env);
	MRoute* mroute = g_mroute(gbl);

	if (!route_is_main(env)) {
		a_henv menv = &mroute->route;
		/* Swap stack sensible fields. */
        swap(menv->rctx, env->rctx);
        swap(menv->rctx_alloc, env->rctx_alloc);
        swap(menv->pctx, env->pctx);
		swap(menv->caller, env->caller);
	}

	/* Clean resources. */
	route_close(gbl, &mroute->route);
	ai_gc_clean(gbl);
	ai_str_clean(gbl);
    ai_type_clean(gbl);
	ai_cap_clean(gbl);

#ifndef ALOI_USE_VALGRIND
    /* Avoid used memory check if foreign memcheck tools is enabled. */
	assume(gbl_mem_total(gbl) == mroute_size(), "memory leak.");
#endif
	ai_mem_vdealloc(&gbl->alloc_, gbl->alloc_ctx, mroute, mroute_size());
}

a_henv ai_env_mroute(Global* gbl) {
	return &g_mroute(gbl)->route;
}
