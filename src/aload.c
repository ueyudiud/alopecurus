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
        ZIn in;
		a_henv env;
	};
    a_flags flags;
    RefQueue rq;
};

static void load_report(InCtx* ic, a_msg msg) {
    if (msg == ALO_EEMPTY) {
        ai_err_raisef(ic->env, ALO_ECHUNK, "unexpected ending of chunk");
    }
    else {
        assume(msg == ALO_EOUTER);
        ai_err_raisef(ic->env, msg, "foreign error, code: %d", ic->in.err);
    }
}

static void load_input(InCtx* ic, void* dst, a_usize len) {
    catch (ai_io_iget(&ic->in, dst, len), msg) {
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
    catch (ai_str_load(ic->env, cast(a_sbfun, ai_io_iget), len, &ic->in, &str), msg) {
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
            v_set_str(ic->in.env, v, val);
            break;
        }
        default: { /* For short string */
            GStr* val = l_gets(ic, tag);
            v_set_str(ic->in.env, v, val);
            break;
        }
    }
}

static void load_info(InCtx* ic, ProtoDesc* info, a_bool root) {
    init(info) {
        .nconst = l_getvi(ic, a_u32),
        .ninsn = l_getvi(ic, a_u32),
        .nsub = l_getvi(ic, a_u32),
        .nlocal = l_getvi(ic, a_u16),
        .nline = l_getvi(ic, a_u16),
        .ncap = l_get(ic, a_u8),
        .nstack = l_get(ic, a_u8),
        .flags = l_get(ic, a_u16) | (ic->flags << 16)
    };
	if (root && !(info->flags & FUN_FLAG_UNIQUE)) {
        ai_err_raisef(ic->env, ALO_ECHUNK, "chunk data verification failed");
    }
}

static GProto* load_proto(InCtx* ic, a_bool root) {
    ProtoDesc info;

    /* Load prototype header */
    load_info(ic, &info, root);

    GProto* proto = ai_proto_alloc(ic->env, &info);

    /* Noexcept code until here, add proto into object list. */
	rq_push(&ic->rq, proto);
    
    for (a_u32 i = 0; i < info.nconst; ++i) {
        load_const(ic, &proto->consts[i]);
    }
    l_getv(ic, proto->code, info.ninsn);
    for (a_u16 i = 0; i < info.nsub; ++i) {
        proto->subs[i] = load_proto(ic, false);
    }

    return proto;
}

static void load_verify(InCtx* ic) {
    /* Check chunk header */
    char header[CHUNK_HEADER_SIZE];
    l_getv(ic, header, CHUNK_HEADER_SIZE);
    if (memcmp(header, ai_fun_header, CHUNK_HEADER_SIZE) != 0) {
        ai_err_raisef(ic->env, ALO_ECHUNK, "not chunk file");
    }
    /* Check version and variant */
    a_u16 variant = l_get(ic, a_u16);
    if (variant != ALO_VARIANT) {
        ai_err_raisef(ic->env, ALO_ECHUNK, "chunk file variant mismatched, expect %u, got %u", ALO_VARIANT, variant);
    }
    a_u16 version = l_get(ic, a_u16);
    if (version != ALO_VERSION_NUMBER) {
        ai_err_raisef(ic->env, ALO_ECHUNK, "chunk file version mismatched, expect %u, got %u", ALO_VERSION_NUMBER, version);
    }
}

static void load_chunk(unused a_henv env, void* ctx) {
    InCtx* ic = ctx;
    /* Verify chunk head */
    load_verify(ic);
    /* Load root prototype */
    load_proto(ic, true);
    /* Register prototypes into GC */
    ai_gc_register_normals(ic->in.env, &ic->rq);
}

static void load_mark(Global* gbl, InCtx* ctx) {
	rq_for (obj, &ctx->rq) {
		GProto* meta = g_as(GProto, obj);
		for (a_u32 i = 0; i < meta->nconst; ++i) {
            v_trace(gbl, meta->consts[i]);
		}
	}
}

static void load_except(a_henv env, void* ctx, unused a_msg msg) {
    InCtx* ic = ctx;
    Global* gbl = G(env);
	rq_for (obj, &ic->rq) {
		GProto* meta = g_as(GProto, obj);
		ai_mem_ndealloc(gbl, meta, meta->size);
	}
}

static KStack const load_klass = {
    .tag = ALO_TUSER,
    .flags = KLASS_FLAG_PLAIN,
    .name = null,
    .mark = load_mark,
    .catch = load_except
};

a_msg ai_fun_load(a_henv env, GFun** pval, a_ifun fun, void* ctx, a_flags flags) {
    InCtx ic = {
        .klass = &load_klass,
        .flags = flags
    };
	rq_init(&ic.rq);
    ai_io_iinit(env, fun, ctx, &ic.in);

    a_msg msg = ai_env_catch(env, load_chunk, ic);

    if (likely(msg == ALO_SOK)) {
        *pval = g_as(GProto, ic.rq.head)->cache;
    }
    return msg;
}