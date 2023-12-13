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
#include "aerr.h"

#include "aload.h"

char const ai_fun_header[CHUNK_HEADER_SIZE] = { '\x04', 'A', 'l', 'o' };

typedef struct InCtx InCtx;

struct InCtx {
    GOBJ_STRUCT_HEADER;
	union {
		a_henv _env;
		ZIn _in;
	};
    a_flags _flags;
    RefQueue _rq;
};

static void load_report(InCtx* ic, a_msg msg) {
    if (msg == ALO_EEMPTY) {
        ai_err_raisef(ic->_env, ALO_ECHUNK, "unexpected ending of chunk");
    }
    else {
        assume(msg == ALO_EOUTER);
        ai_err_raisef(ic->_env, msg, "foreign error, code: %d", ic->_in._err);
    }
}

static void load_input(InCtx* ic, void* dst, a_usize len) {
    catch (ai_io_iget(&ic->_in, dst, len), msg) {
        load_report(ic, msg);
    }
}

#define l_in(ic,p,l) load_input(ic, p, l)
#define l_get(ic,t) ({ t _v0; l_in(ic, &_v0, sizeof(t));  _v0; })
#define l_getv(ic,p,l) l_in(ic, p, sizeof((p)[0]) * (l))
/* Get var sized int */
#define l_getvi(ic,t) ({ \
    t _v = 0; \
    a_byte _b = 0; \
    while (unlikely(((_b = l_get(ic, a_byte)) & u8c(0x80)) != 0)) { \
        _v = (_v << 7) | (cast(t, _b) & u8c(0x7f)); \
    } \
    _v << 7 | _b; \
})

static GStr* l_gets(InCtx* ic, a_usize len) {
    GStr* str;
    catch (ai_str_load(ic->_env, cast(a_sbfun, ai_io_iget), len, &ic->_in, &str), msg) {
        load_report(ic, msg);
    }
    return str;
}

static void load_const(InCtx* ic, Value* v) {
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
}

static void load_info(InCtx* ic, ProtoDesc* info, a_bool root) {
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
	if (root && !(info->_flags & FUN_FLAG_UNIQUE)) {
        ai_err_raisef(ic->_env, ALO_ECHUNK, "chunk data verification failed");
    }
}

static GProto* load_proto(InCtx* ic, a_bool root) {
    ProtoDesc info;

    /* Load prototype header */
    load_info(ic, &info, root);

    GProto* proto = ai_proto_alloc(ic->_env, &info);

    /* Noexcept code until here, add proto into object list. */
	rq_push(&ic->_rq, proto);
    
    for (a_u32 i = 0; i < info._nconst; ++i) {
        load_const(ic, &proto->_consts[i]);
    }
    l_getv(ic, proto->_code, info._ninsn);
    for (a_u16 i = 0; i < info._nsub; ++i) {
        proto->_subs[i] = load_proto(ic, false);
    }

    return proto;
}

static void load_verify(InCtx* ic) {
    /* Check chunk header */
    char header[CHUNK_HEADER_SIZE];
    l_getv(ic, header, CHUNK_HEADER_SIZE);
    if (memcmp(header, ai_fun_header, CHUNK_HEADER_SIZE) != 0) {
        ai_err_raisef(ic->_env, ALO_ECHUNK, "not chunk file");
    }
    /* Check version and variant */
    a_u16 variant = l_get(ic, a_u16);
    if (variant != ALO_VARIANT) {
        ai_err_raisef(ic->_env, ALO_ECHUNK, "chunk file variant mismatched, expect %u, got %u", ALO_VARIANT, variant);
    }
    a_u16 version = l_get(ic, a_u16);
    if (version != ALO_VERSION_NUMBER) {
        ai_err_raisef(ic->_env, ALO_ECHUNK, "chunk file version mismatched, expect %u, got %u", ALO_VERSION_NUMBER, version);
    }
}

static void load_chunk(unused a_henv env, void* ctx) {
    InCtx* ic = ctx;
    /* Verify chunk head */
    load_verify(ic);
    /* Load root prototype */
    load_proto(ic, true);
    /* Register prototypes into GC */
	ai_gc_register_objects(ic->_in._env, &ic->_rq);
}

static void load_mark(Global* gbl, void* ctx) {
    InCtx* ic = ctx;
	rq_for (obj, &ic->_rq) {
		GProto* meta = g_cast(GProto, obj);
		for (a_u32 i = 0; i < meta->_nconst; ++i) {
			ai_gc_trace_mark_val(gbl, meta->_consts[i]);
		}
	}
    g_set_stack_white(ic);
}

static void load_except(a_henv env, InCtx* ic, unused a_msg msg) {
    Global* gbl = G(env);
	rq_for (obj, &ic->_rq) {
		GProto* meta = g_cast(GProto, obj);
		ai_mem_ndealloc(gbl, meta, meta->_size);
	}
}

static VTable const load_vtable = {
    ._stencil = V_STENCIL(T_USER),
    ._flags = VTABLE_FLAG_GREEDY_MARK | VTABLE_FLAG_STACK_ALLOC,
    ._slots = {
        [vfp_slot(mark)] = load_mark,
        [vfp_slot(except)] = load_except
    }
};

a_msg ai_fun_load(a_henv env, GFun** pval, a_ifun fun, void* ctx, a_flags flags) {
    InCtx ic = {
        ._vptr = &load_vtable,
        ._flags = flags
    };
    g_set_stack_white(&ic);
	rq_init(&ic._rq);
    ai_io_iinit(env, fun, ctx, &ic._in);

    Value* p = env->_stack._top++;
    v_set_obj(env, p, &ic);

    a_msg msg = ai_env_pcall(env, load_chunk, &ic, p);

    v_set_nil(--env->_stack._top);
    
    if (likely(msg == ALO_SOK)) {
        *pval = g_cast(GProto, ic._rq._head)->_cache;
    }
    return msg;
}