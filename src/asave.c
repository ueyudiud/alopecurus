/**
 *@file asave.c
 */

#define asave_c_
#define ALO_LIB

#include "aio.h"
#include "aenv.h"
#include "agc.h"

#include "aload.h"

typedef struct OutCtx OutCtx;

struct OutCtx {
	union {
		ZOut _out;
		a_henv _env;
	};
    a_flags _flags;
};

#define l_out(oc,p,l) check(ai_io_oput(&(oc)->_out, p, l))
#define l_put(oc,t,v) ({ t _v = cast(t, v); l_out(oc, &_v, sizeof(t)); })
#define l_putv(oc,v,l) ({ typeof(&(v)[0]) _v = (v); l_out(oc, &_v, sizeof(_v[0]) * (l)); })

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
        case T_ISTR: {
            val = v_as_str(v);
        save_sstr:
            l_put(oc, a_u8, val->_len);
            l_putv(oc, val->_data, val->_len);
            break;
        }
        case T_HSTR: {
            val = v_as_str(v);
            if (likely(val->_len <= LVLSTR_LEN_BIAS)) 
                goto save_sstr;
            l_put(oc, a_u8, LVTAG_LSTR);
            l_putvi(oc, a_u32, val->_len - LVLSTR_LEN_BIAS);
            l_putv(oc, val->_data, val->_len);
            break;
        }
        default: {
            assume(v_is_float(v));
            l_put(oc, a_u8, LVTAG_FLOAT);
            l_put(oc, a_float, v_as_float(v));
            break;
        }
    }
    return ALO_SOK;
}

static a_msg l_save(OutCtx* oc, GProto* proto, a_bool root) {
    l_putvi(oc, a_u32, proto->_nconst);
    l_putvi(oc, a_u32, proto->_ninsn);
    l_putvi(oc, a_u16, proto->_nsub);
    l_putvi(oc, a_u16, proto->_nlocal);
    l_put(oc, a_u8, proto->_ncap);
    l_put(oc, a_u8, proto->_nstack);
	ProtoFlags flags = {
		._fdebug = true,
		._froot = root
	};
	l_put(oc, ProtoFlags, flags);
    for (a_u32 i = 0; i < proto->_nconst; ++i) {
        check(l_save_const(oc, proto->_consts[i]));
    }
    l_putv(oc, proto->_code, proto->_ninsn);
    for (a_u32 i = 0; i < proto->_nsub; ++i) {
        check(l_save(oc, proto->_subs[i], false));
    }
    return ALO_SOK;
}

a_msg ai_fun_save(a_henv env, GFun* val, a_ofun fun, void* ctx, a_flags flags) {
    OutCtx oc = { ._flags =  flags };
    ai_io_oinit(env, fun, ctx, &oc._out);
    return l_save(&oc, val->_proto, true);
}
