/*
 * aenv.h
 */

#ifndef aenv_h_
#define aenv_h_

#include <stdatomic.h>

#include "actx.h"
#include "astr.h"
#include "atype.h"
#include "astk.h"

#define GLOBAL_FLAG_INCRGC u16c(0x0001)
#define GLOBAL_FLAG_FULLGC u16c(0x0002)
#define GLOBAL_FLAG_EMERGENCYGC u16c(0x0004)
#define GLOBAL_FLAG_DISABLE_GC u16c(0x0008)

intern GRoute* ai_env_new(a_henv env, a_usize stack_size);
intern a_henv ai_env_mroute(Global* gbl);
intern a_msg ai_env_resume(a_henv env, GRoute* self);
intern void ai_env_yield(a_henv env);
intern a_msg ai_env_protect(a_henv env, a_pfun pfun, a_efun efun, void* ctx);
intern a_noret ai_env_raise(a_henv env, a_msg msg);

#define FRAME_FLAG_NONE 0x00

#define FRAME_FLAG_TRIM_RET 0x01
#define FRAME_FLAG_TAIL_CALL 0x02
#define FRAME_FLAG_META_CALL 0x04

struct Frame {
    Frame* prev;
    a_insn const* pc;
    Value* stack_bot;
    Value* stack_dst;
    a_u32 num_ret;
    a_u8 flags;
#ifdef ALOI_CHECK_API
    StkPtr stack_limit;
#endif
};

typedef void* a_rctx;

struct alo_Env {
    GOBJ_STRUCT_HEADER;
    Global* global;
    a_rctx rctx;
    void* rctx_alloc;
    GRoute* caller;
    Frame* frame;
    Stack stack;
    a_efun errf;
    void* errc;
    Value error;
    a_u16 flags;
    a_u8 status;
    PCtx pctx;
    RcCap* open_caps;
    Frame base_frame;
};

/* Some offset used in assembly, make sure the value is correct. */
#if ALO_M64
static_assert(offsetof(GRoute, rctx) == 0x20);
static_assert(offsetof(GRoute, rctx_alloc) == 0x28);
static_assert(offsetof(GRoute, caller) == 0x30);
static_assert(offsetof(GRoute, stack.base) == 0x40);
#else
static_assert(offsetof(GRoute, rctx) == 0x10);
static_assert(offsetof(GRoute, rctx_alloc) == 0x14);
static_assert(offsetof(GRoute, caller) == 0x18);
static_assert(offsetof(GRoute, stack.base) == 0x20);
#endif

/**
 ** Get global context from environment.
 */
#define G(env) ((env)->global)

typedef struct {
    GStr** ptr;
    a_u32 len;
    a_u32 hmask; /* Hash code mask. */
} StrCache;

enum { TYPE__COUNT = ALO_TUSER };

struct Global {
    Alloc alloc_;
    void* alloc_ctx;
    a_hfun hook_;
    a_hctx hook_ctx;
    a_cfun panic_;
    a_henv active;
    a_usize mem_base;
    a_isize mem_debt;
    a_isize mem_work;
    a_usize mem_estimate;
    a_gclist gc_normal;
    a_gclist gc_fixed;
    a_gclist gc_closable;
    a_gclist gc_toclose;
    a_gcnext* gc_sweep;
    RcCap* cap_cache;
    a_trmark tr_gray;
    a_trmark tr_regray;
    GStr* nomem_error;
    StrCache str_cache;
    Value global_value;
    a_hash seed;
    a_u16 gcpausemul;
    a_u16 gcstepmul;
    a_u16 flags;
    a_u8 white_bit;
    a_u8 gcstep;
    volatile atomic_uint_fast8_t hookm;
    GStr* fast_strs[STR__COUNT];
    GType fast_types[TYPE__COUNT][1];
};

#define RFLAG_COUNT_VARARG UINT8_MAX

#define ALO_HMSWAP 0x80

always_inline a_usize gbl_mem_total(Global* gbl) {
    return gbl->mem_base + cast(a_usize, gbl->mem_debt);
}

always_inline GStr* g_str(a_henv env, a_u32 tag) {
    return G(env)->fast_strs[tag];
}

#define g_type(env,f) (G(env)->fast_types[f])
#define g_type_ref(f) offsetof(Global, fast_types[f])

#define g_is_route(o) g_is(o, ALO_TROUTE)

always_inline a_bool v_is_route(Value v) {
    return v_is(v, T_OTHER) && g_is_route(v_as_obj(v));
}

always_inline GRoute* v_as_route(Value v) {
    assume(v_is_route(v), "not route");
    return g_as(GRoute, v_as_obj(v));
}

always_inline Value v_of_route(GRoute* o) {
    assume(g_is_route(o), "invalid instance.");
    return v_of_obj_(o, T_OTHER);
}

always_inline void v_set_route(a_henv env, Value* d, GRoute* o) {
    Value v = v_of_route(o);
    v_set(env, d, v);
}

#define route_size() sizeof(GRoute)

always_inline GType* g_typeof(a_henv env, a_gptr o) {
    a_u32 tag = o->impl->tag;
    return tag < TYPE__COUNT ? g_type(env, tag) : g_as(GType, from_member(GIType, body, o->impl));
}

#define g_typeof(env,p) g_typeof(env, g_as_obj(p))

always_inline char const* g_nameof(a_henv env, a_gptr p) {
    return str2ntstr(g_typeof(env, p)->name);
}

#define g_nameof(env,p) g_nameof(env, g_as_obj(p))

always_inline GType* v_typeof(a_henv env, Value v) {
    switch (v_get_tag(v)) {
        case T_NIL: return g_type(env, ALO_TNIL);
        case T_FALSE:
        case T_TRUE: return g_type(env, ALO_TBOOL);
        case T_INT: return g_type(env, ALO_TINT);
        case T_PTR: return g_type(env, ALO_TPTR);
        case T_OBJ: return g_typeof(env, v_as_obj(v));
        case T_FLOAT: return g_type(env, ALO_TFLOAT);
        default: unreachable();
    }
}

always_inline char const* v_nameof(a_henv env, Value v) {
    switch (v_get_tag(v)) {
        case T_NIL: return "nil";
        case T_FALSE:
        case T_TRUE: return "bool";
        case T_INT: return "int";
        case T_PTR: return "ptr";
        case T_OBJ: return g_nameof(env, v_as_obj(v));
        case T_FLOAT: return "float";
        default: unreachable();
    }
}

#ifndef ALOI_DFL_GCSTEPMUL
# define ALOI_DFL_GCSTEPMUL usizec(384)
#endif

#ifndef ALOI_DFL_GCPAUSEMUL
# define ALOI_DFL_GCPAUSEMUL usizec(512)
#endif

#ifndef ALOI_INIT_CFRAMESIZE
# define ALOI_INIT_CFRAMESIZE usizec(8)
#endif

always_inline void check_in_stack(a_henv env, Value* v) {
	assume(v >= env->stack.base && v < env->stack.limit + RESERVED_STACK_SIZE, "not stack value pointer.");
}

always_inline StkPtr val2stk(a_henv env, Value* v) {
	check_in_stack(env, v);
#if ALO_STACK_RELOC
	return addr_diff(v, env->stack._impl);
#else
	quiet(env);
	return v;
#endif
}

always_inline Value* stk2val(a_henv env, StkPtr p) {
	Value* v;
#if ALO_STACK_RELOC
	v = ptr_disp(Value, env->stack.impl, p);
#else
	quiet(env);
	v = p;
#endif
	check_in_stack(env, v);
	return v;
}

always_inline Value* ai_stk_bot(a_henv env) {
	return stk2val(env, env->frame->stack_bot);
}

always_inline a_isize ai_stk_check(a_henv env, Value* top) {
	if (top > env->stack.limit) {
		a_isize diff = ai_stk_grow(env, top);
		if (diff & STACK_GROW_FAILED) {
			ai_stk_overflow(env, diff);
		}
		return diff;
	}
	return 0;
}

always_inline void ai_env_pop_error(a_henv env, Value* d) {
	v_cpy(env, d, &env->error);
	v_set_nil(&env->error);
}

enum {
    GCSTEP_PROPAGATE,
    GCSTEP_PROPAGATE_ATOMIC,
    GCSTEP_SWEEP_NORMAL,
    GCSTEP_SWEEP_ATOMIC,
    GCSTEP_CLOSE,
    GCSTEP_PAUSE
};

#define WHITE1_COLOR 0x1
#define WHITE2_COLOR 0x2
#define WHITE_COLOR (WHITE1_COLOR|WHITE2_COLOR)
#define BLACK_COLOR 0x4

#define GRAY_NULL ((a_trmark) 0)

always_inline a_trmark white_color(Global* gbl) {
    return cast(a_trmark, gbl->white_bit);
}

always_inline a_trmark other_color(Global* gbl) {
    return white_color(gbl) ^ (WHITE1_COLOR | WHITE2_COLOR);
}

always_inline a_bool g_has_black_color(a_gptr o) {
    return (o->tnext & BLACK_COLOR) != 0;
}

#define g_has_black_color(o) g_has_black_color(g_as_obj(o))

always_inline a_bool g_has_gray_color(a_gptr o) {
    return (o->tnext & (BLACK_COLOR | WHITE1_COLOR | WHITE2_COLOR)) == 0;
}

#define g_has_gray_color(o) g_has_gray_color(g_as_obj(o))

always_inline a_bool g_has_white_color(Global* gbl, a_gptr o) {
    return (o->tnext & white_color(gbl)) != 0;
}

#define g_has_white_color(gbl,o) g_has_white_color(gbl, g_as_obj(o))

always_inline a_bool g_has_other_color(Global* gbl, a_gptr o) {
    return (o->tnext & other_color(gbl)) != 0;
}

#define g_has_other_color(gbl,v) g_has_other_color(gbl, g_as_obj(v))

always_inline a_bool g_has_valid_color(Global* gbl, a_gptr o) {
    return !g_has_other_color(gbl, o) || impl_has_flag(o->impl, IMPL_FLAG_STACK_ALLOC);
}

always_inline a_bool g_has_white_color_within_assume_alive(Global* gbl, a_gptr o) {
    assume(g_has_valid_color(gbl, o));
    return (o->tnext & (WHITE1_COLOR | WHITE2_COLOR)) != 0;
}

#define g_has_white_color_within_assume_alive(gbl,o) g_has_white_color_within_assume_alive(gbl, g_as_obj(o))

always_inline void v_check_alive(a_henv env, Value v) {
    if (v_is_obj(v)) {
        a_gptr p = v_as_obj(v);
        assume(g_has_valid_color(G(env), p));
    }
}

#endif /* aenv_h_ */
