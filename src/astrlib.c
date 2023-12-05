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
    a_lstr _sub;
    a_lstr _pat;
    a_lstr _rep;
} RepCtx;

static Buf* l_newbuf(a_henv env) {
    return cast(Buf*, aloL_newbuf(env));
}

static a_lstr ntstr2lstr(char const* str) {
    return (a_lstr) { str, str != null ? strlen(str) : 0 };
}

static char const* lstrstr(a_lstr sub, a_lstr pat) {
    assume(pat._len > 0, "pattern string length == 0");

    if (sub._len < pat._len)
        return null;

    char ch = pat._ptr[0];
    char const* begin;
    char const* match_end = sub._ptr + sub._len - pat._len;

    while ((begin = memchr(sub._ptr, ch, sub._len)) != null && begin <= match_end) {
        if (memcmp(begin + 1, pat._ptr + 1, pat._len - 1) == 0) {
            return begin;
        }
    }

    return null;
}

static void l_replace(a_henv env, RepCtx* ctx, Buf* buf) {
    if (ctx->_pat._len <= ctx->_rep._len) {
        at_buf_check(env, buf, ctx->_sub._len);
    }

    char const* beg;
    a_lstr str = ctx->_sub;

    while ((beg = lstrstr(str, ctx->_pat)) != null) {
        at_buf_putls(env, buf, str._ptr, beg - str._ptr);
        at_buf_putls(env, buf, ctx->_rep._ptr, ctx->_rep._len);

        a_usize step = beg - str._ptr + ctx->_pat._len;
        str._ptr += step;
        str._len -= step;
    }

    at_buf_putls(env, buf, str._ptr, ctx->_sub._ptr + ctx->_sub._len - str._ptr);
}

char const* aloS_replace(a_henv env, char const* sub, char const* pat, char const* rep) {
    RepCtx ctx = {
        ._sub = ntstr2lstr(sub),
        ._pat = ntstr2lstr(pat),
        ._rep = ntstr2lstr(rep)
    };

    api_check_slot(env, 2);

    Buf* buf = l_newbuf(env);
    l_replace(env, &ctx, buf);

    char const* out = alo_pushstr(env, buf->_ptr, buf->_len);
    alo_erase(env, -2, 1); /* Drop buffer. */
    return out;
}

static a_msg str_replace(a_henv env) {
    RepCtx ctx;

    ctx._sub._ptr = aloL_checklstr(env, 0, &ctx._sub._len);
    ctx._pat._ptr = aloL_checklstr(env, 1, &ctx._pat._len);
    ctx._rep._ptr = aloL_checklstr(env, 2, &ctx._rep._len);

    char const* beg = lstrstr(ctx._sub, ctx._pat);
    if (beg != null) {
        alo_settop(env, 3);

        Buf* buf = l_newbuf(env);
        at_buf_putls(env, buf, ctx._sub._ptr, beg - ctx._sub._ptr);
        at_buf_putls(env, buf, ctx._rep._ptr, ctx._rep._len);

        a_usize step = beg - ctx._sub._ptr + ctx._pat._len;
        ctx._sub._ptr += step;
        ctx._sub._len -= step;
        l_replace(env, &ctx, buf);

        alo_pushstr(env, buf->_ptr, buf->_len);
    }
    else {
        alo_settop(env, 1);
    }
    return 1;
}

static a_msg str_trim(a_henv env) {
    a_lstr str;
    str._ptr = aloL_checklstr(env, 0, &str._len);

    char const* p = str._ptr;
    char const* q = str._ptr + str._len;

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

    if (p == str._ptr && q == str._ptr + str._len) {
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