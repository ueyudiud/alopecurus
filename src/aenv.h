/**
 *@file aenv.h
 */

#ifndef aenv_h_
#define aenv_h_

#include <stdatomic.h>

#include "actx.h"
#include "astr.h"
#include "atype.h"
#include "astk.h"

intern GRoute* ai_env_new(a_henv env, a_usize stack_size);
intern a_henv ai_env_mroute(Global* gbl);
intern a_msg ai_env_resume(a_henv env, GRoute* self);
intern void ai_env_yield(a_henv env);
intern a_msg ai_env_protect(a_henv env, a_pfun pfun, a_efun efun, void* ctx);
intern a_msg ai_env_catch_(a_henv env, a_pfun pfun, a_gptr ctx);
intern a_noret ai_env_raise(a_henv env, a_msg msg);

#define ai_env_catch(env,fun,ctx) ai_env_catch_(env, fun, g_as_ref(&(ctx)))

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
    a_gclist gc_stack;
    a_efun errf;
    void* errc;
    Value error;
    a_u16 flags;
    a_i8 status;
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
 ** Get global from context.
 */
#define G(env) cast(Global*, (env)->global)

typedef struct {
    GStr** ptr;
    a_u32 len;
    a_u32 hmask; /* Hash code mask. */
} StrCache;

enum { PTYPE_COUNT = ALO_TUSER };

typedef struct {
    a_u8 enable: 1;
    a_u8 incremental: 1;
    a_u8 full: 1;
    a_u8 emergency: 1;
} GcFlags;

struct Global {
    a_byte global[0];
    char const* version;
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
    a_gclist gc_closable;
    a_gclist gc_toclose;
    a_gcnext* gc_sweep;
    RcCap* cap_cache;
    a_trmark tr_gray;
    a_trmark tr_regray;
    a_trmark tr_weak;
    a_trmark tr_blur;
    a_trmark tr_phantom;
    GStr* nomem_error;
    StrCache str_cache;
    Value global_value;
    a_hash seed;
    a_u16 gcpausemul;
    a_u16 gcstepmul;
    GcFlags gcflags;
    a_u8 white_bit;
    a_u8 gcstep;
    volatile atomic_uint_fast8_t hookm;
    GStr* fast_strs[STR__COUNT];
    GBType fast_types[PTYPE_COUNT];
};

static_assert(offsetof(Global, version) == 0); /* 'version' must be head of global. */

#define RFLAG_COUNT_VARARG UINT8_MAX

#define ALO_HMSWAP 0x80

always_inline a_usize gbl_mem_total(Global* gbl) {
    return gbl->mem_base + cast(a_usize, gbl->mem_debt);
}

always_inline GStr* g_str(a_henv env, a_u32 tag) {
    return G(env)->fast_strs[tag];
}

#define g_ptype(env,f) g_as(GType, &G(env)->fast_types[f])

#define g_is_route(o) g_is(o, ALO_TROUTE)

always_inline a_bool v_is_route(Value v) {
    return v_is(v, T_OTHER) && g_is_route(v_as_ref(v));
}

always_inline GRoute* v_as_route(Value v) {
    assume(v_is_route(v), "not route");
    return g_as(GRoute, v_as_ref(v));
}

always_inline Value v_of_route(GRoute* o) {
    assume(g_is_route(o), "invalid instance.");
    return v_of_ref(o, T_OTHER);
}

always_inline void v_set_route(a_henv env, Value* d, GRoute* o) {
    Value v = v_of_route(o);
    v_set(env, d, v);
}

#define route_size() sizeof(GRoute)

always_inline GType* g_type(Global* gbl, a_gptr o) {
    Klass const* k = g_klass(o);
    a_u32 tag = k->tag;
    if (likely(tag < PTYPE_COUNT)) {
        return g_ptype(gbl, tag);
    }
    return g_as(GType, from_member(GUType, body, k));
}

#define g_type(env,p) g_type(G(env), g_as_ref(p))

always_inline char const* g_name(unused Global* gbl, a_gptr p) {
    return g_klass(p)->name ?: "user";
}

#define g_name(env,p) g_name(G(env), g_as_ref(p))

always_inline GType* v_type(Global* gbl, Value v) {
    switch (v_get_tag(v)) {
        case T_NIL: return g_ptype(gbl, ALO_TNIL);
        case T_FALSE:
        case T_TRUE: return g_ptype(gbl, ALO_TBOOL);
        case T_INT: return g_ptype(gbl, ALO_TINT);
        case T_PTR: return g_ptype(gbl, ALO_TPTR);
        case T_OBJ: return g_type(gbl, v_as_ref(v));
        case T_FLOAT: return g_ptype(gbl, ALO_TFLOAT);
        default: unreachable();
    }
}

#define v_type(env,v) v_type(G(env), v)

always_inline char const* v_name(Global* gbl, Value v) {
    switch (v_get_tag(v)) {
        case T_NIL: return "nil";
        case T_FALSE:
        case T_TRUE: return "bool";
        case T_INT: return "int";
        case T_PTR: return "ptr";
        case T_OBJ: return g_name(gbl, v_as_ref(v));
        case T_FLOAT: return "float";
        default: unreachable();
    }
}

#define v_name(env,v) v_name(G(env), v)

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
	v = addr_add(Value, env->stack.impl, p);
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
    GCSTEP_TRACE,
    GCSTEP_TRACE_ATOMIC,
    GCSTEP_TRACE_WEAK,
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

#define g_has_black_color(o) g_has_black_color(g_as_ref(o))

always_inline a_bool g_has_gray_color(a_gptr o) {
    return (o->tnext & (BLACK_COLOR | WHITE1_COLOR | WHITE2_COLOR)) == 0;
}

#define g_has_gray_color(o) g_has_gray_color(g_as_ref(o))

always_inline a_bool g_has_white_color(Global* gbl, a_gptr o) {
    return (o->tnext & white_color(gbl)) != 0;
}

#define g_has_white_color(gbl,o) g_has_white_color(gbl, g_as_ref(o))

always_inline a_bool g_has_other_color(Global* gbl, a_gptr o) {
    return (o->tnext & other_color(gbl)) != 0;
}

#define g_has_other_color(gbl,v) g_has_other_color(gbl, g_as_ref(v))

always_inline a_bool g_has_valid_color(Global* gbl, a_gptr o) {
    return !g_has_other_color(gbl, o);
}

always_inline a_bool g_has_white_color_within_assume_alive(Global* gbl, a_gptr o) {
    assume(g_has_valid_color(gbl, o));
    return (o->tnext & (WHITE1_COLOR | WHITE2_COLOR)) != 0;
}

always_inline a_bool v_is_alive(Global* gbl, Value v) {
    if (!v_is_ref(v) || v_is_str(v)) {
        return true;
    }
    a_gptr o = v_as_ref(v);
    if (v_is(v, T_OTHER) && k_has_flag(g_klass(o), KLASS_FLAG_VALUE)) {
        return true;
    }
    return !g_has_white_color_within_assume_alive(gbl, o);
}

always_inline void v_check_alive(a_henv env, Value v) {
    if (v_is_ref(v)) {
        a_gptr p = v_as_ref(v);
        assume(g_has_valid_color(G(env), p));
    }
}

#endif /* aenv_h_ */
