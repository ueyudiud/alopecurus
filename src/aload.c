/**
 *@file aload.c
 */

#define aload_c_

#include <string.h>

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

#define l_in(ic,p,l) check(ai_io_iget(&(ic)->_in, p, l))
#define l_get(ic,t) ({ t _v; l_in(ic, &_v, sizeof(t));  _v; })
#define l_getv(ic,p,l) l_in(ic, p, sizeof((p)[0]) * (l))

#define l_getvi(ic,t) ({ \
    t _v = 0; \
    a_byte _b = 0; \
    while (unlikely(((_b = l_get(ic, a_byte)) & u8c(0x80)) != 0)) { \
        _v = (_v << 7) | (cast(t, _b) & u8c(0x7f)); \
    } \
    _v << 7 | _b; \
})

static a_msg l_geths(InCtx* ic, a_u32 len, GStr** pstr) {
    assume(len > ALOI_SHTSTR_THRESHOLD);
    Global* g = G(ic->_env);
    GStr* str = ai_mem_xalloc(ic->_in._env, sizeof(GStr) + len);
    if (str == null) return ALO_ENOMEM;
    str->_meta = &g->_metas._hstr;
    str->_len = len;
    a_msg msg = ai_io_iget(&ic->_in, str->_data, len);
    if (msg != ALO_SOK) {
        ai_mem_xdealloc(g, str, sizeof(GStr) + len);
        if (msg == ALO_ESTMUF) {
            msg = ALO_EINVAL;
        }
        return msg;
    }
    str->_data[len] = '\0';
    str->_hash = ai_str_hashof(g->_seed, str->_data, len);
    ai_gc_register_object(ic->_in._env, str);
	*pstr = str;
    return ALO_SOK;
}

static a_msg l_load_const(InCtx* ic, Value* v) {
    a_u8 tag;
    switch (tag = l_get(ic, a_u8)) {
        case LVTAG_NIL: {
            v_setx(v, v_of_nil());
            break;
        }
        case LVTAG_FALSE: {
            v_setx(v, v_of_bool(false));
            break;
        }
        case LVTAG_TRUE: {
            v_setx(v, v_of_bool(true));
            break;
        }
        case LVTAG_INT: {
            a_int val = l_get(ic, a_int);
            v_setx(v, v_of_int(val));
            break;
        }
        case LVTAG_FLOAT: {
            a_float val = l_get(ic, a_float);
            v_setx(v, v_of_float(val));
            break;
        }
        case LVTAG_LSTR: {
            a_u32 len = l_getvi(ic, a_u32) + LVLSTR_LEN_BIAS;
            GStr* val;
            check(l_geths(ic, len, &val));
            v_setx(v, v_of_ref(val));
            break;
        }
        default: { /* For short string */
            a_byte buf[256];
            l_getv(ic, buf, tag);
            GStr* val = ai_str_create(ic->_in._env, buf, tag);
            v_set(G(ic->_in._env), v, v_of_ref(val));
            break;
        }
    }
    return ALO_SOK;
}

static a_msg l_load_info(InCtx* ic, FnCreateInfo* info) {
    info->_nconst = l_get(ic, a_u32);
    info->_ninsn = l_get(ic, a_u32);
    info->_nsub = l_get(ic, a_u16);
    info->_nlocal = l_get(ic, a_u16);
    info->_ncap = l_get(ic, a_u8);
    info->_nstack = l_get(ic, a_u8);
	fninfo_hint_size(info);
    return ALO_SOK;
}

static a_msg l_load_sub(InCtx* ic, GFunMeta** pf);

static a_msg l_load_meta(InCtx* ic, FnCreateInfo const* info, GFunMeta* meta) {
    /* Noexcept code until here, add meta into object list. */
	rq_push(&ic->_rq, meta);
    
    for (a_u32 i = 0; i < info->_nconst; ++i) {
        check(l_load_const(ic, &meta->_consts[i]));
    }
    l_getv(ic, meta->_insns, info->_ninsn);
    for (a_u16 i = 0; i < info->_nsub; ++i) {
        check(l_load_sub(ic, &meta->_subs[i]));
    }
    
    return ALO_SOK;
}

static a_msg l_load_sub(InCtx* ic, GFunMeta** pf) {
    /* Load function header */
    FnCreateInfo info;
    check(l_load_info(ic, &info));

    GFunMeta* meta = ai_fun_xalloc(ic->_env, &info);
    if (meta == null) return ALO_ENOMEM;

    check(l_load_meta(ic, &info, meta));
    *pf = meta;
    return ALO_SOK;
}

static a_msg l_load_root(InCtx* ic) {
    /* Load function header */
    FnCreateInfo info;
    check(l_load_info(ic, &info));

	a_usize meta_size = info._size;
    a_usize fun_size = sizeof(GFun) + sizeof(Value) * info._ncap;
    info._size += fun_size;

	GFunMeta* meta = ai_fun_xalloc(ic->_env, &info);
	if (meta == null) return ALO_ENOMEM;

	GFun* fun = cast(GFun*, addr_of(meta) + meta_size);
	meta->_cache = fun;

    fun->_meta = downcast(GMeta, meta);
    fun->_len = info._ncap;

    check(l_load_meta(ic, &info, meta));
    
    return ALO_SOK;
}

static a_msg l_load(InCtx* ic) {
    check(l_load_root(ic));
	ai_gc_register_objects(ic->_in._env, &ic->_rq);
    return ALO_SOK;
}

static void l_splash(Global* g, void* ctx) {
    InCtx* ic = ctx;
	rq_for(obj, &ic->_rq) {
		GFunMeta* meta = downcast(GFunMeta, obj);
		for (a_u32 i = 0; i < meta->_len; ++i) {
			ai_gc_trace_markv(g, &meta->_consts[i]);
		}
	}
}

static void l_release(InCtx* ic) {
    Global* g = G(ic->_env);
	rq_for(obj, &ic->_rq) {
		GFunMeta* meta = downcast(GFunMeta, obj);
		ai_mem_xdealloc(g, meta, meta->_len);
	}
}

a_msg ai_fun_load(a_henv env, GFun** pval, a_ifun fun, void* ctx, a_flags flags) {
    InCtx ic = { ._flags =  flags };
	rq_init(&ic._rq);
    ai_io_iinit(env, fun, ctx, &ic._in);

	ai_env_gsplash(env, l_splash, &ic);
    a_msg msg = l_load(&ic);
	ai_env_gsplash_clear(env);
    
    if (likely(msg == ALO_SOK)) {
        *pval = downcast(GFunMeta, ic._rq._head)->_cache;
    }
    else {
        l_release(&ic);
    }
    return msg;
}