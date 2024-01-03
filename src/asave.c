/**
 *@file asave.c
 */

#define asave_c_
#define ALO_LIB

#include "aio.h"
#include "aenv.h"

#include "aload.h"

typedef struct OutCtx OutCtx;

struct OutCtx {
	union {
		ZOut out;
		a_henv _env;
	};
    a_flags flags;
};

#define l_out(oc,p,l) try (ai_io_oput(&(oc)->out, p, l))
#define l_put(oc,t,v) ({ t _v = cast(t, v); l_out(oc, &_v, sizeof(t)); })
#define l_putv(oc,v,l) ({ typeof(&(v)[0]) _v = (v); l_out(oc, &_v, sizeof(_v[0]) * (l)); })
/* Put var sized int. */
#define l_putvi(oc,t,v) ({ \
    a_byte _b[(sizeof(v) * 8 + 6) / 7]; \
    a_byte* _p = _b + sizeof(_b) - 1; \
    t _v = cast(t, v); \
    while (_v & ~cast(t, 0x7f)) { \
        *(_p--) = cast(a_byte, _v & cast(t, 0x7f)) | u8c(0x80); \
        _v >>= 7; \
    } \
    *_p = cast(a_byte, _v); \
    l_out(oc, _p, _b + sizeof(_b) - _p); \
})

static a_msg l_save_const(OutCtx* oc, Value v) {
    GStr* val;
    switch (v_get_tag(v)) {
        case T_NIL: {
            l_put(oc, a_u8, LVTAG_NIL);
            break;
        }
        case T_FALSE: {
            l_put(oc, a_u8, LVTAG_FALSE);
            break;
        }
        case T_TRUE: {
            l_put(oc, a_u8, LVTAG_TRUE);
            break;
        }
        case T_INT: {
            l_put(oc, a_u8, LVTAG_INT);
            break;
        }
        case T_STR: {
            val = v_as_str(v);
            if (likely(val->len <= LVLSTR_LEN_BIAS)) {
                l_put(oc, a_u8, val->len);
                l_putv(oc, val->ptr, val->len);
            }
            else {
                l_put(oc, a_u8, LVTAG_LSTR);
                l_putvi(oc, a_u32, val->len - LVLSTR_LEN_BIAS);
                l_putv(oc, val->ptr, val->len);
            }
            break;
        }
        default: {
            l_put(oc, a_u8, LVTAG_FLOAT);
            l_put(oc, a_float, v_as_float(v));
            break;
        }
    }
    return ALO_SOK;
}

static a_msg l_save_proto(OutCtx* oc, GProto* proto) {
    l_putvi(oc, a_u32, proto->nconst);
    l_putvi(oc, a_u32, proto->ninsn);
    l_putvi(oc, a_u16, proto->nsub);
    l_putvi(oc, a_u16, proto->nlocal);
    l_put(oc, a_u8, proto->ncap);
    l_put(oc, a_u8, proto->nstack);
	l_put(oc, a_u16, proto->flags);
    for (a_u32 i = 0; i < proto->nconst; ++i) {
        try (l_save_const(oc, proto->consts[i]));
    }
    l_putv(oc, proto->code, proto->ninsn);
    for (a_u32 i = 0; i < proto->nsub; ++i) {
        try (l_save_proto(oc, proto->subs[i]));
    }
    return ALO_SOK;
}

static a_msg l_save(OutCtx* oc, GProto* proto) {
    l_putv(oc, ai_fun_header, CHUNK_HEADER_SIZE);
    l_put(oc, a_u16, ALO_VARIANT);
    l_put(oc, a_u16, ALO_VERSION_NUMBER);
    return l_save_proto(oc, proto);
}

a_msg ai_fun_save(a_henv env, GFun* val, a_ofun fun, void* ctx, a_flags flags) {
    OutCtx oc = { .flags =  flags };
    ai_io_oinit(env, fun, ctx, &oc.out);
    return l_save(&oc, val->proto);
}
