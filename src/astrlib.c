/**
 *@file astrlib.c
 */

#define astrlib_c_
#define ALO_LIB

#include <ctype.h>

#include "abuf.h"
#include "astr.h"
#include "aapi.h"

#include "alo.h"
#include "aauxlib.h"
#include "astrlib.h"

typedef struct {
    a_lstr sub;
    a_lstr pat;
    a_lstr rep;
} RepCtx;

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

static void l_replace(a_henv env, RepCtx* ctx, Buf* buf) {
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
    RepCtx ctx = {
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

static a_msg str_replace(a_henv env) {
    RepCtx ctx;

    ctx.sub.ptr = aloL_checklstr(env, 0, &ctx.sub.len);
    ctx.pat.ptr = aloL_checklstr(env, 1, &ctx.pat.len);
    ctx.rep.ptr = aloL_checklstr(env, 2, &ctx.rep.len);

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

static a_msg str_trim(a_henv env) {
    a_lstr str;
    str.ptr = aloL_checklstr(env, 0, &str.len);

    char const* p = str.ptr;
    char const* q = str.ptr + str.len;

    while (p < q) {
        if (!isspace(*p))
            goto backward;
        p += 1;
    }

    v_set_obj(env, api_incr_stack(env), g_str(env, STR_EMPTY));

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

    return 1;
}

void aloopen_str(a_henv env) {
    static aloL_Entry const bindings[] = {
        { "__look__", null },
        { "replace", str_replace },
        { "trim", str_trim }
    };

    alo_pushptype(env, ALO_TSTR);
    aloL_putalls(env, -1, bindings);

    alo_push(env, -1);
    aloL_puts(env, -2, "__look__");
}