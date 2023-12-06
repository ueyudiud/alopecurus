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
intern a_msg ai_env_pcall(a_henv env, a_pfun pfun, void* pctx);
intern a_noret ai_env_raise(a_henv env, a_msg msg);

#define FRAME_FLAG_NONE 0x00

#define FRAME_FLAG_TRIM_RET 0x01
#define FRAME_FLAG_TAIL_CALL 0x02
#define FRAME_FLAG_META_CALL 0x04

struct Frame {
    Frame* _prev;
    a_insn const* _pc;
    Value* _stack_bot;
    Value* _stack_dst;
    a_u32 _num_ret;
    a_u8 _flags;
#ifdef ALOI_CHECK_API
    StkPtr _stack_limit;
#endif
};

typedef void* a_rctx;

struct alo_Env {
    GOBJ_STRUCT_HEADER;
    Global* _global;
    a_rctx _rctx;
    void* _rctx_alloc;
    GRoute* _from;
    Frame* _frame;
    Stack _stack;
    Value _error;
    a_u16 _flags;
    a_u8 _status;
    PCtx _pctx;
    RcCap* _open_caps;
    Frame _base_frame;
};

/* Some offset used in assembly, make sure the value is correct. */
#if ALO_M64
static_assert(offsetof(GRoute, _rctx) == 0x20);
static_assert(offsetof(GRoute, _rctx_alloc) == 0x28);
static_assert(offsetof(GRoute, _from) == 0x30);
static_assert(offsetof(GRoute, _stack._base) == 0x40);
#else
static_assert(offsetof(GRoute, _rctx) == 0x10);
static_assert(offsetof(GRoute, _rctx_alloc) == 0x14);
static_assert(offsetof(GRoute, _from) == 0x18);
static_assert(offsetof(GRoute, _stack._base) == 0x20);
#endif

/**
 ** Get global context from environment.
 */
#define G(env) ((env)->_global)

typedef struct {
    GStr** _table;
    a_usize _len;
    a_usize _hmask; /* Hash code mask. */
} StrCache;

typedef void (*a_fp_gexecpt)(a_henv env, void* ctx, a_msg msg);
typedef void (*a_fp_gmark)(Global* gbl, void* ctx);

enum { TYPE__COUNT = ALO_TUSER };

struct Global {
    Alloc _af;
    void* _ac;
    a_hfun _hookf;
    a_hctx _hookc;
    a_cfun _panic;
    a_henv _active;
    a_usize _mem_base;
    a_isize _mem_debt;
    a_isize _mem_work;
    a_usize _mem_estimate;
    a_gclist _gc_normal;
    a_gclist _gc_fixed;
    a_gclist _gc_closable;
    a_gclist _gc_toclose;
    a_gcnext* _gc_sweep;
    RcCap* _cap_cache;
    Value _global;
    a_fp_gexecpt _gexecpt;
    a_fp_gmark _gmark;
    void* _gctx;
    a_trmark _tr_gray;
    a_trmark _tr_regray;
    GStr* _nomem_error;
    StrCache _str_cache;
    a_hash _seed;
    a_u16 _gcpausemul;
    a_u16 _gcstepmul;
    a_u16 _flags;
    a_u8 _white_color;
    a_u8 _gcstep;
    volatile atomic_uint_fast8_t _hookm;
    GStr* _names[STR__COUNT];
    GType _types[TYPE__COUNT][1];
};

#define RFLAG_COUNT_VARARG UINT8_MAX

#define ALO_HMSWAP 0x80

always_inline void gbl_protect(a_henv env, a_fp_gmark mark, a_fp_gexecpt except, void* ctx) {
    assume(mark != null || except != null, "no protect function given.");
    Global* gbl = G(env);
    gbl->_gmark = mark;
    gbl->_gexecpt = except;
    gbl->_gctx = ctx;
}

always_inline void gbl_unprotect(a_henv env) {
    Global* gbl = G(env);
    gbl->_gmark = null;
    gbl->_gexecpt = null;
    gbl->_gctx = null;
}

always_inline a_usize gbl_mem_total(Global* gbl) {
    return gbl->_mem_base + cast(a_usize, gbl->_mem_debt);
}

always_inline GStr* g_str(a_henv env, a_u32 tag) {
    return G(env)->_names[tag];
}

#define g_type(env,f) (G(env)->_types[f])
#define g_type_ref(f) offsetof(Global, _types[f])

always_inline a_bool g_is_route(a_gptr p) {
    return p->_vptr->_tag == ALO_TROUTE;
}

always_inline a_bool v_is_route(Value v) {
    return v_is(v, T_USER) && g_is_route(v_as_obj(v));
}

always_inline GRoute* v_as_route(Value v) {
    assume(v_is_route(v), "not route");
    return g_cast(GRoute, v_as_obj(v));
}

#define route_size() sizeof(GRoute)

always_inline GType* g_typeof(a_henv env, a_gptr p) {
    a_usize type_ref = p->_vptr->_type_ref;
    return likely(type_ref) != 0 ?
           ptr_disp(GType, G(env), p->_vptr->_type_ref) :
           g_type(env, ALO_TPTR);
}

#define g_typeof(env,p) g_typeof(env, gobj_cast(p))

always_inline char const* g_nameof(a_henv env, a_gptr p) {
    return str2ntstr(g_typeof(env, p)->_name);
}

#define g_nameof(env,p) g_nameof(env, gobj_cast(p))

always_inline GType* v_typeof(a_henv env, Value v) {
    if (v_is_float(v)) {
        return g_type(env, ALO_TFLOAT);
    }
    else if (!v_is_obj(v)) {
        switch (v_get_tag(v)) {
            case T_NIL:
                return g_type(env, ALO_TNIL);
            case T_FALSE:
            case T_TRUE:
                return g_type(env, ALO_TBOOL);
            case T_INT:
                return g_type(env, ALO_TINT);
            case T_PTR:
                return g_type(env, ALO_TPTR);
            default:
                panic("bad type tag.");
        }
    }
    else {
        return g_typeof(env, v_as_obj(v));
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
	assume(v >= env->_stack._base && v < env->_stack._limit + RESERVED_STACK_SIZE, "not stack value pointer.");
}

always_inline StkPtr val2stk(a_henv env, Value* v) {
	check_in_stack(env, v);
#if ALO_STACK_RELOC
	return addr_diff(v, env->_stack._impl);
#else
	quiet(env);
	return v;
#endif
}

always_inline Value* stk2val(a_henv env, StkPtr p) {
	Value* v;
#if ALO_STACK_RELOC
	v = ptr_disp(Value, env->_stack._impl, p);
#else
	quiet(env);
	v = p;
#endif
	check_in_stack(env, v);
	return v;
}

always_inline Value* ai_stk_bot(a_henv env) {
	return stk2val(env, env->_frame->_stack_bot);
}

always_inline a_isize ai_stk_check(a_henv env, Value* top) {
	if (top > env->_stack._limit) {
		a_isize diff = ai_stk_grow(env, top);
		if (diff & STACK_GROW_FAILED) {
			ai_stk_overflow(env, diff);
		}
		return diff;
	}
	return 0;
}

always_inline void ai_env_pop_error(a_henv env, Value* d) {
	v_cpy(env, d, &env->_error);
	v_set_nil(&env->_error);
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
#define BLACK_COLOR 0x4

#define GRAY_NULL ((a_trmark) 0)

always_inline a_trmark white_color(Global* gbl) {
    return cast(a_trmark, gbl->_white_color);
}

always_inline a_trmark other_color(Global* gbl) {
    return white_color(gbl) ^ (WHITE1_COLOR | WHITE2_COLOR);
}

always_inline a_bool g_has_black_color(a_gptr o) {
    return (o->_tnext & BLACK_COLOR) != 0;
}

#define g_has_black_color(o) g_has_black_color(gobj_cast(o))

always_inline a_bool g_has_gray_color(a_gptr o) {
    return (o->_tnext & (BLACK_COLOR | WHITE1_COLOR | WHITE2_COLOR)) == 0;
}

#define g_has_gray_color(o) g_has_gray_color(gobj_cast(o))

always_inline a_bool g_has_white_color(Global* gbl, a_gptr o) {
    return (o->_tnext & white_color(gbl)) != 0;
}

#define g_has_white_color(gbl,o) g_has_white_color(gbl, gobj_cast(o))

always_inline a_bool g_has_other_color(Global* gbl, a_gptr o) {
    return (o->_tnext & other_color(gbl)) != 0;
}

#define g_has_other_color(gbl,v) g_has_other_color(gbl, gobj_cast(v))

always_inline a_bool g_has_white_color_within_assume_alive(Global* gbl, a_gptr o) {
    assume(!g_has_other_color(gbl, o));
    return (o->_tnext & (WHITE1_COLOR | WHITE2_COLOR)) != 0;
}

#define g_has_white_color_within_assume_alive(gbl,o) g_has_white_color_within_assume_alive(gbl, gobj_cast(o))

always_inline void v_check_alive(a_henv env, Value v) {
    if (v_is_obj(v)) {
        a_gptr p = v_as_obj(v);
        a_u64 stencil = v._ ^ p->_vptr->_stencil;
        assume((stencil & V_TAG_MASK) == 0 && !g_has_other_color(G(env), p));
    }
}

#endif /* aenv_h_ */
