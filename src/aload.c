/**
 *@file aload.c
 */

#define aload_c_
#define ALO_LIB

#include "aio.h"
#include "astr.h"
#include "aenv.h"
#include "amem.h"
#include "agc.h"

#include "aload.h"

typedef struct InCtx InCtx;

struct InCtx {
	union {
		a_henv _env;
		ZIn _in;
	};
    a_flags _flags;
    RefQueue _rq;
};

#define l_in(ic,p,l) try(ai_io_iget(&(ic)->_in, p, l))
#define l_get(ic,t) ({ t _v0; l_in(ic, &_v0, sizeof(t));  _v0; })
#define l_getv(ic,p,l) l_in(ic, p, sizeof((p)[0]) * (l))

#define l_getvi(ic,t) ({ \
    t _v = 0; \
    a_byte _b = 0; \
    while (unlikely(((_b = l_get(ic, a_byte)) & u8c(0x80)) != 0)) { \
        _v = (_v << 7) | (cast(t, _b) & u8c(0x7f)); \
    } \
    _v << 7 | _b; \
})

#define l_gets(ic,l) ({ GStr* _v; try(ai_str_load((ic)->_env, cast(a_sbfun, ai_io_iget), l, &(ic)->_in, &_v)); _v; })

static a_msg l_load_const(InCtx* ic, Value* v) {
    a_u8 tag;
    switch (tag = l_get(ic, a_u8)) {
        case LVTAG_NIL: {
            v_set_nil(v);
            break;
        }
        case LVTAG_FALSE: {
            v_set_bool(v, false);
            break;
        }
        case LVTAG_TRUE: {
            v_set_bool(v, true);
            break;
        }
        case LVTAG_INT: {
            a_int val = l_get(ic, a_int);
            v_set_int(v, val);
            break;
        }
        case LVTAG_FLOAT: {
            a_float val = l_get(ic, a_float);
            v_set_float(v, val);
            break;
        }
        case LVTAG_LSTR: {
            a_u32 len = l_getvi(ic, a_u32) + LVLSTR_LEN_BIAS;
            GStr* val = l_gets(ic, len);
            v_set_obj(ic->_in._env, v, val);
            break;
        }
        default: { /* For short string */
            GStr* val = l_gets(ic, tag);
            v_set_obj(ic->_in._env, v, val);
            break;
        }
    }
    return ALO_SOK;
}

static a_msg l_load_info(InCtx* ic, ProtoDesc* info, a_bool root) {
    init(info) {
        ._nconst = l_getvi(ic, a_u32),
        ._ninsn = l_getvi(ic, a_u32),
        ._nsub = l_getvi(ic, a_u32),
        ._nlocal = l_getvi(ic, a_u16),
        ._nline = l_getvi(ic, a_u16),
        ._ncap = l_get(ic, a_u8),
        ._nstack = l_get(ic, a_u8),
        ._flags = l_get(ic, a_u16) | (ic->_flags << 16)
    };
	if (root && !(info->_flags & FUN_FLAG_UNIQUE))
		return ALO_EINVAL;
    return ALO_SOK;
}

static a_msg l_load_sub(InCtx* ic, GProto** pf);

static a_msg l_load_meta(InCtx* ic, ProtoDesc const* info, GProto* meta) {
    /* Noexcept code until here, add meta into object list. */
	rq_push(&ic->_rq, meta);
    
    for (a_u32 i = 0; i < info->_nconst; ++i) {
        try(l_load_const(ic, &meta->_consts[i]));
    }
    l_getv(ic, meta->_code, info->_ninsn);
    for (a_u16 i = 0; i < info->_nsub; ++i) {
        try(l_load_sub(ic, &meta->_subs[i]));
    }
    
    return ALO_SOK;
}

static a_msg l_load_sub(InCtx* ic, GProto** pf) {
    /* Load function header */
    ProtoDesc info;
    try (l_load_info(ic, &info, false));

    GProto* meta = ai_proto_xalloc(ic->_env, &info);
    if (meta == null) return ALO_ENOMEM;

    try (l_load_meta(ic, &info, meta));
    *pf = meta;
    return ALO_SOK;
}

static a_msg l_load_root(InCtx* ic) {
    /* Load function header */
    ProtoDesc info;
    try (l_load_info(ic, &info, true));

	GProto* meta = ai_proto_xalloc(ic->_env, &info);
	if (meta == null) return ALO_ENOMEM;

    try (l_load_meta(ic, &info, meta));
    
    return ALO_SOK;
}

static a_msg l_load(InCtx* ic) {
    try (l_load_root(ic));
	ai_gc_register_objects(ic->_in._env, &ic->_rq);
    return ALO_SOK;
}

static void l_splash(Global* gbl, void* ctx) {
    InCtx* ic = ctx;
	rq_for (obj, &ic->_rq) {
		GProto* meta = g_cast(GProto, obj);
		for (a_u32 i = 0; i < meta->_nconst; ++i) {
			ai_gc_trace_mark_val(gbl, meta->_consts[i]);
		}
	}
}

static void l_release(InCtx* ic) {
    Global* gbl = G(ic->_env);
	rq_for (obj, &ic->_rq) {
		GProto* meta = g_cast(GProto, obj);
		ai_mem_ndealloc(gbl, meta, meta->_size);
	}
}

a_msg ai_fun_load(a_henv env, GFun** pval, a_ifun fun, void* ctx, a_flags flags) {
    InCtx ic = { ._flags = flags };
	rq_init(&ic._rq);
    ai_io_iinit(env, fun, ctx, &ic._in);

	gbl_protect(env, l_splash, null, &ic);
    a_msg msg = l_load(&ic);
	gbl_unprotect(env);
    
    if (likely(msg == ALO_SOK)) {
        *pval = g_cast(GProto, ic._rq._head)->_cache;
    }
    else {
        l_release(&ic);
    }
    return msg;
}