/**
 *@file astrlib.c
 */

#define astrlib_c_
#define ALO_LIB

#include <ctype.h>

#include "abuf.h"
#include "astr.h"
#include "agc.h"
#include "aapi.h"

#include "alo.h"
#include "aauxlib.h"
#include "astrlib.h"

typedef struct {
    a_lstr sub;
    a_lstr pat;
    a_lstr rep;
} Sub;

static Buf* l_newbuf(a_henv env) {
    return cast(Buf*, aloL_newbuf(env));
}

static a_lstr ntstr2lstr(char const* str) {
    return (a_lstr) { str, str != null ? strlen(str) : 0 };
}

static char const* lstrstr(a_lstr sub, a_lstr pat) {
    assume(pat.len > 0, "pattern string length == 0");

    if (sub.len < pat.len)
        return null;

    char ch = pat.ptr[0];
    char const* begin;
    char const* match_end = sub.ptr + sub.len - pat.len;

    while ((begin = memchr(sub.ptr, ch, sub.len)) != null && begin <= match_end) {
        if (memcmp(begin + 1, pat.ptr + 1, pat.len - 1) == 0) {
            return begin;
        }
    }

    return null;
}

static void l_replace(a_henv env, Sub* ctx, Buf* buf) {
    if (ctx->pat.len <= ctx->rep.len) {
        at_buf_check(env, buf, ctx->sub.len);
    }

    char const* beg;
    a_lstr str = ctx->sub;

    while ((beg = lstrstr(str, ctx->pat)) != null) {
        at_buf_putls(env, buf, str.ptr, beg - str.ptr);
        at_buf_putls(env, buf, ctx->rep.ptr, ctx->rep.len);

        a_usize step = beg - str.ptr + ctx->pat.len;
        str.ptr += step;
        str.len -= step;
    }

    at_buf_putls(env, buf, str.ptr, ctx->sub.ptr + ctx->sub.len - str.ptr);
}

char const* aloS_replace(a_henv env, char const* sub, char const* pat, char const* rep) {
    Sub ctx = {
        .sub = ntstr2lstr(sub),
        .pat = ntstr2lstr(pat),
        .rep = ntstr2lstr(rep)
    };

    api_check_slot(env, 2);

    Buf* buf = l_newbuf(env);
    l_replace(env, &ctx, buf);

    char const* out = alo_pushstr(env, buf->ptr, buf->len);
    alo_erase(env, -2, 1); /* Drop buffer. */
    return out;
}

static a_lstr check_lstr(a_henv env, a_ilen id) {
    a_lstr str;
    str.ptr = aloL_checklstr(env, id, &str.len);
    return str;
}

static a_lstr opt_lstr_(a_henv env, a_ilen id, a_lstr dfl) {
    a_lstr str;
    str.ptr = aloL_optlstr(env, id, &str.len);
    if (str.ptr == null) {
        str = dfl;
    }
    return str;
}

#define opt_lstr(env,id,dfl) opt_lstr_(env, id, (a_lstr) { ""dfl, sizeof(dfl) - 1 })

static char const* l_memfind(a_lstr str, a_lstr pat) {
    if (pat.len == 0)
        return pat.ptr;
    if (pat.len > str.len)
        return null;

    char const* pi;
    int ch = cast(int, *pat.ptr);

    pat.len -= 1;
    str.len -= pat.len;
    while (str.len > 0 && (pi = memchr(str.ptr, ch, str.len)) != null) {
        pi += 1;
        if (memcmp(pi, pat.ptr + 1, pat.len) == 0)
            return pi - 1;
        str.len -= addr_diff(pi, str.ptr);
        str.ptr = pi;
    }
    return null;  /* not found */
}

static a_msg str_find(a_henv env) {
    a_lstr str = check_lstr(env, 0);
    a_lstr pat = check_lstr(env, 1);

    void const* found = l_memfind(str, pat);

    if (found != null) {
        a_isize off = addr_diff(found, str.ptr);
        alo_pushint(env, cast(a_i32, off));
        alo_pushint(env, cast(a_i32, off + pat.len));
        return 2;
    }

    return 0;
}

static a_msg str_startswith(a_henv env) {
    a_lstr str = check_lstr(env, 0);
    a_lstr pat = check_lstr(env, 1);
    alo_pushbool(env, str.len >= pat.len && memcmp(str.ptr, pat.ptr, pat.len) == 0);
    return 1;
}

static a_msg str_endswith(a_henv env) {
    a_lstr str = check_lstr(env, 0);
    a_lstr pat = check_lstr(env, 1);
    alo_pushbool(env, str.len >= pat.len && memcmp(str.ptr + str.len - pat.len, pat.ptr, pat.len) == 0);
    return 1;
}

static a_msg str_replace(a_henv env) {
    Sub ctx;

    ctx.sub = check_lstr(env, 0);
    ctx.pat = check_lstr(env, 1);
    ctx.rep = check_lstr(env, 2);

    char const* beg = lstrstr(ctx.sub, ctx.pat);
    if (beg != null) {
        alo_settop(env, 3);

        Buf* buf = l_newbuf(env);
        at_buf_putls(env, buf, ctx.sub.ptr, beg - ctx.sub.ptr);
        at_buf_putls(env, buf, ctx.rep.ptr, ctx.rep.len);

        a_usize step = beg - ctx.sub.ptr + ctx.pat.len;
        ctx.sub.ptr += step;
        ctx.sub.len -= step;
        l_replace(env, &ctx, buf);

        alo_pushstr(env, buf->ptr, buf->len);
    }
    else {
        alo_settop(env, 1);
    }
    return 1;
}

static a_msg str_split(a_henv env) {
    a_lstr str = check_lstr(env, 0);
    a_lstr pat = check_lstr(env, 1);
    a_bool strip = aloL_optbool(env, 2, false);

    alo_settop(env, 2);
    alo_newlist(env, 0);

    char const* ptr = l_memfind(str, pat);
    if (ptr != null) {
        do {
            a_usize len = addr_diff(ptr, str.ptr);

            if (len > 0 || !strip) {
                alo_pushstr(env, str.ptr, len);
                alo_put(env, 2);
            }

            str.len -= len + pat.len;
            str.ptr += len + pat.len;
        }
        while ((ptr = l_memfind(str, pat)) != null);

        if (str.len > 0 || !strip) {
            alo_pushstr(env, str.ptr, str.len);
            alo_put(env, 2);
        }
    }
    else {
        alo_push(env, 0);
        alo_put(env, 2);
    }

    return 1;
}

static a_msg str_trim(a_henv env) {
    a_lstr str = check_lstr(env, 0);

    char const* p = str.ptr;
    char const* q = str.ptr + str.len;

    while (p < q) {
        if (!isspace(*p))
            goto backward;
        p += 1;
    }

    v_set_str(env, api_incr_stack(env), g_str(env, STR_EMPTY));
    goto finish;

backward:
    while (p < q) {
        if (!isspace(*(q - 1)))
            break;
        q -= 1;
    }

    assume(p < q);

    if (p == str.ptr && q == str.ptr + str.len) {
        alo_settop(env, 1);
    }
    else {
        alo_pushstr(env, p, q - p);
    }

finish:
    return 1;
}

typedef struct {
    a_lstr str;
    a_lstr sep;
    a_u32 rep;
} Rep;

static a_msg l_repeat(void* rctx, void* dst, unused a_usize len) {
    Rep* ctx = rctx;

    memcpy(dst, ctx->str.ptr, ctx->str.len);
    dst += ctx->str.len;

    for (a_u32 i = 1; i < ctx->rep; ++i) {
        memcpy(dst, ctx->sep.ptr, ctx->sep.len);
        dst += ctx->sep.len;

        memcpy(dst, ctx->str.ptr, ctx->str.len);
        dst += ctx->str.len;
    }

    return ALO_SOK;
}

static a_bool get_rep_len(a_usize* l, a_usize a, a_usize b, a_usize r) {
    a_usize al = try_mul(a, r);
    a_usize bl = try_mul(b, r - 1);
    return ckd_add(l, al, bl);
}

static a_msg str_repeat(a_henv env) {
    Rep ctx = {
        .str = check_lstr(env, 0),
        .rep = cast(a_u32, aloL_checkint(env, 1)),
        .sep = opt_lstr(env, 2, "")
    };

    if (ctx.rep == 0) {
        v_set_str(env, api_incr_stack(env), g_str(env, STR_EMPTY));
    }
    else {
        GStr* out;

        a_usize len;
        if (get_rep_len(&len, ctx.str.len, ctx.sep.len, ctx.rep)) {
            alo_pushntstr(env, "result string too large.");
            alo_raise(env);
        }

        a_msg msg = ai_str_load(env, l_repeat, len, &ctx, &out);
        assume(msg == ALO_SOK);

        v_set_str(env, api_incr_stack(env), out);
        ai_gc_trigger(env);
    }

    return 1;
}

void aloopen_str(a_henv env) {
    static aloL_Entry const bindings[] = {
        { "__look__", null },
        { "__mul__", str_repeat },
        { "find", str_find },
        { "repeat", str_repeat },
        { "replace", str_replace },
        { "startswith", str_startswith },
        { "endswith", str_endswith },
        { "split", str_split },
        { "trim", str_trim }
    };

    alo_pushptype(env, ALO_TSTR);
    aloL_putalls(env, -1, bindings);

    alo_push(env, -1);
    aloL_puts(env, -2, "__look__");
}