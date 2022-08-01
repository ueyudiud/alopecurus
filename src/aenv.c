/*
 * aenv.c
 */

#define aenv_c_

#include "astr.h"
#include "atuple.h"
#include "alist.h"
#include "actx.h"
#include "amem.h"
#include "agc.h"
#include "aerr.h"
#include "astrx.h"

#include "aenv.h"

/* Main route. */
typedef struct MRoute {
	Route _route;
	Global _global;
	GStr* _strx[STRX__MAX - 1];
	a_byte _strx_reserved[STRX_RESERVE_SPACE];
} MRoute;

inline MRoute* main_route(Global* g) {
	return from_member(MRoute, _global, g);
}

inline a_bool in_main_route(a_henv env) {
	return &main_route(G(env))->_route._body == env;
}

inline Route* env2route(a_henv env) {
	return from_member(Route, _body, env);
}

static void route_new(GRoute* self, Global* g) {
	self->_status = ALO_SYIELD;
	self->_flags = 0;
	self->_g = g;
	self->_from = null;
	v_setx(&self->_error, v_of_nil());
	self->_stack = new(Stack) { };
	self->_frame = &self->_base_frame;
	self->_base_frame = new(Frame) { };
}

static a_bool route_create(a_henv env, GRoute* self) {
	Stack* stack = &self->_stack;
	Value* base = ai_mem_vxnew(env, Value, ALOI_INIT_STACKSIZE);
	if (base == null) return true;
	stack->_base = base;
	stack->_bot = base;
	stack->_top = base;
	stack->_limit = base + ALOI_INIT_STACKSIZE - RESERVED_STACKSIZE;
#ifdef ALOI_STRICT_STACK_CHECK
	self->_base_frame._bound = base + ALOI_INIT_CFRAMESIZE;
#endif
	return false;
}

static void route_destroy(Global* g, GRoute* self) {
	Stack* stack = &self->_stack;
	ai_mem_vxdel(g, stack->_base, stack->_limit - stack->_base + RESERVED_STACKSIZE);
}

static void route_destruct(Global* g, GRoute* self) {
	Route* route = env2route(self);
	ai_ctx_close(route);
	route_destroy(g, self);
	ai_mem_dealloc(g, route, sizeof(Route));
}

static void route_splash(Global* g, GRoute* self) {
	Stack* stack = &self->_stack;
	Value* from = stack->_base;
	Value* const to = stack->_top;
	for (Value const* v = from; v < to; ++v) {
		ai_gc_trace_markv(g, v);
	}
	g->_mem_work -= cast(a_isize, sizeof(GRoute) + sizeof(Value) * (stack->_limit - from + RESERVED_STACKSIZE));

	if (self == g->_active) {
		join_trace(&g->_tr_regray, self);
	}
}

a_msg ai_env_resume(a_henv env, GRoute* self) {
	assume(self->_status == ALO_SYIELD && self->_from == null);
	self->_status = ALO_SOK;
	self->_from = env;
	return ai_ctx_swap(env2route(env), env2route(self));
}

void ai_env_yield(a_henv env) {
	assume(!in_main_route(env) && env->_status == ALO_SOK);
	a_henv from_env = env->_from;
	env->_status = ALO_SYIELD;
	env->_from = null;
	ai_ctx_swapx(env2route(env), env2route(from_env), ALO_SOK);
}

a_msg ai_env_protect(a_henv env, a_pfun pfun, void* pctx) {
	return ai_ctx_catch(env2route(env), pfun, pctx);
}

a_none ai_env_raise(a_henv env, a_msg msg) {
	ai_ctx_raise(env2route(env), msg);
}

GRoute* ai_env_new(a_henv env, a_usize stack_size) {
	Route* self = ai_mem_alloc(env, sizeof(Route));
	route_new(&self->_body, G(env));
	self->_body._meta = &G(env)->_metas._route;
	if (route_create(env, &self->_body)) {
		ai_mem_dealloc(G(env), self, sizeof(Route));
		ai_mem_nomem(env);
	}
	ai_gc_register_object(env, &self->_body);
	ai_ctx_open(self, stack_size);
	return &self->_body;
}

static void global_init(Global* g) {
	g->_metas._dstr = new(GMeta) {
		_tid: T_ISTR,
		_flags: GTYPE_FLAG_IDENTITY_EQUAL,
		_splash: null,
		_destruct: null
	};
	g->_metas._istr = new(GMeta) {
		_tid: T_ISTR,
		_flags: GTYPE_FLAG_IDENTITY_EQUAL,
		_splash: fpcast(a_fp_splash, ai_istr_splash),
		_destruct: fpcast(a_fp_destruct, ai_istr_destruct)
	};
	g->_metas._hstr = new(GMeta) {
		_tid: T_HSTR,
		_flags: 0,
		_splash: fpcast(a_fp_splash, ai_hstr_splash),
		_destruct: fpcast(a_fp_destruct, ai_hstr_destruct)
	};
	g->_metas._tuple = new(GMeta) {
		_tid: T_TUPLE,
		_flags: 0,
		_splash: fpcast(a_fp_splash, ai_tuple_splash),
		_destruct: fpcast(a_fp_destruct, ai_tuple_destruct)
	};
	g->_metas._list = new(GMeta) {
		_tid: T_LIST,
		_flags: 0,
		_splash: fpcast(a_fp_splash, ai_list_splash),
		_destruct: fpcast(a_fp_destruct, ai_list_destruct)
	};
	g->_metas._route = new(GMeta) {
		_tid: T_ROUTE,
		_flags: GTYPE_FLAG_IDENTITY_EQUAL,
		_splash: fpcast(a_fp_splash, route_splash),
		_destruct: fpcast(a_fp_destruct, route_destruct)
	};
}

static void global_postinit(a_henv env, unused void* ctx) {
	MRoute* m = cast(MRoute*, env);
	ai_str_boost(env);
	ai_strx_open(env, m->_strx_reserved, m->_strx);
}

a_msg alo_create(a_alloc const* af, void* ac, a_henv* penv) {
	MRoute* mr = ai_mem_nalloc(af, ac, sizeof(MRoute));
	if (mr == null) return ALO_ENOMEM;

	Global* g = &mr->_global;
	Route* route = &mr->_route;
	a_henv env = &route->_body;

	mr->_global = new(Global) {
		_af: *af,
		_ac: ac,
		_active: env,
		_gc_normal: null,
		_gc_closable: null,
		_gc_toclose: null,
		_gc_sweep: null,
		_mem_base: sizeof(MRoute),
		_mem_debt: 0,
		_flags: GLOBAL_FLAG_DISABLE_GC,
		_gcpausemul: ALOI_DFL_GCPAUSEMUL,
		_gcstepmul: ALOI_DFL_GCSTEPMUL,
		_white_color: WHITE1_COLOR,
		_gcstep: GCSTEP_PAUSE
	};
	
	route_new(env, g);

	/* Initialize metadata of primitive types. */
	global_init(g);
	env->_meta = &g->_metas._route;

	/* Intialize context and stack of route. */
	route->_ctx = new(RCtx) { };
	if (route_create(env, env)) return ALO_ENOMEM;

	/* Initialize remaining components. */
	a_msg msg = ai_env_protect(env, cast(a_pfun, global_postinit), null);

	if (unlikely(msg != ALO_SOK)) {
		alo_destroy(env);
		return msg;
	}

	*penv = env;
	return ALO_SOK;
}

void alo_destroy(a_henv env) {
	Global* g = G(env);
	MRoute* mr = main_route(g);

	if (!in_main_route(env)) {
		/* Swap user stack to ensure stack is keeping. */
		swap(env2route(env)->_ctx, mr->_route._ctx);
	}

	/* Clean resources. */
	ai_gc_clean(g);
	ai_str_clean(g);
	route_destroy(g, &mr->_route._body);

	assume(ai_env_mem_total(g) == sizeof(MRoute), "memory leak");
	ai_mem_ndealloc(&g->_af, g->_ac, mr, sizeof(MRoute));
}

a_henv ai_env_mainof(Global* g) {
	return &main_route(g)->_route._body;
}

static a_isize grow_stack(a_henv env, a_usize size_new) {
	a_usize size_old = env->_stack._limit - env->_stack._base + RESERVED_STACKSIZE;
	Value* stack_old = env->_stack._base;
	Value* stack_new;
#ifdef ALOI_STRICT_MEMORY_CHECK
	stack_new = ai_mem_vnew(env, Value, size_new);
	memcpy(stack_new, stack_old, sizeof(Value) * (env->_stack._top - env->_stack._base));
	ai_mem_vxdel(env, stack_old, size_old);
#else
	stack_new = ai_mem_vgrow(env, env->_stack._base, size_old, size_new);
#endif
	a_isize diff = stack_new - stack_old;
	env->_stack._base = stack_new;
	env->_stack._bot += diff;
	env->_stack._top += diff;
	env->_stack._limit = stack_new + size_new - RESERVED_STACKSIZE;
	return diff * cast(a_isize, sizeof(Value));
}

#define MAX_VISIBLE_STACK_SIZE (ALOI_MAX_STACKSIZE - RESERVED_STACKSIZE)
#define MIN_GROW_STACK_SIZE 64

static a_none raise_stkof(a_henv env, a_bool wrap) {
	ai_err_raise(env, ALO_ESTKOF, "stack overflow");
}

a_isize ai_env_grow_stack(a_henv env, Value* top) {
	a_usize current_size = env->_stack._limit - env->_stack._base;
	a_usize expect_size = top - env->_stack._base;
	assume(expect_size > current_size);
	if (unlikely(expect_size > MAX_VISIBLE_STACK_SIZE)) {
		if (current_size == MAX_VISIBLE_STACK_SIZE + OVERFLOW_STACKSIZE)
			return 2;
		grow_stack(env, ALOI_MAX_STACKSIZE + OVERFLOW_STACKSIZE);
		return 1;
	}
	current_size = min(max(current_size * 2, expect_size + MIN_GROW_STACK_SIZE), MAX_VISIBLE_STACK_SIZE);

	return grow_stack(env, current_size + RESERVED_STACKSIZE);
}

a_isize ai_env_check_stack(a_henv env, Value* top) {
	if (top > env->_stack._limit) {
		a_isize diff = ai_env_grow_stack(env, top);
		if (diff & 0x3) {
			raise_stkof(env, (diff & 0x2) != 0);
		}
		return diff;
	}
	return 0;
}
