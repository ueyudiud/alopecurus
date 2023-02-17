/**
 *@file alex.c
 */

#define alex_c_
#define ALO_LIB

#include <math.h>
#include <stdio.h>

#include "aenv.h"
#include "aerr.h"
#include "acode.h"
#include "astrx.h"
#include "afmt.h"
#include "aparse.h"

#include "alex.h"

always_inline void l_switch_scope(Lexer* lex, a_u32 channel) {
	lex->_scope->_last_channel = lex->_channel;
	lex->_scope->_begin_line = lex->_line;
	lex->_channel = channel;
}

always_inline a_i32 l_pollx(Lexer* lex) {
    a_i32 ch = lex->_ch;
    lex->_ch = ai_io_igetc(&lex->_in);
    return ch;
}

static void l_unwind(Lexer* lex, a_i32 ch) {
    assume(ch >= 0);
    lex->_in._ptr -= 1;
    lex->_in._len += 1;
    lex->_ch = ch;
}

#define l_peekx(lex) ((lex)->_ch)

static a_none l_foreign_error(Lexer* lex) {
	ai_par_report(from_member(Parser, _lex, lex), false, "%s: IO error. code: %tx", lex->_in._err);
}

static a_i32 l_poll(Lexer* lex) {
	a_i32 ch = l_pollx(lex);
	if (unlikely(ch < ALO_ESTMUF)) {
		l_foreign_error(lex);
	}
	return ch;
}

static a_i32 l_peek(Lexer* lex) {
	a_i32 ch = l_peekx(lex);
	if (unlikely(ch < ALO_ESTMUF)) {
		l_foreign_error(lex);
	}
	return ch;
}

#define l_skip(lex) l_pollx(lex)

#define l_testx(lex,ch) (l_peekx(lex) == (ch))
#define l_testskip(lex,ch) (l_testx(lex, ch) && (l_pollx(lex), true))

static a_i32 c_bdigit(a_i32 ch) {
    switch (ch) {
        case '0' ... '1': return ch - '0';
        default: return -1;
    }
}

static a_i32 c_ddigit(a_i32 ch) {
    switch (ch) {
        case '0' ... '9': return ch - '0';
        default: return -1;
    }
}

static a_i32 c_hdigit(a_i32 ch) {
    switch (ch) {
        case '0' ... '9': return ch - '0';
        case 'a' ... 'f': return ch - 'a' + 10;
        case 'A' ... 'F': return ch - 'A' + 10;
        default: return -1;
    }
}

static a_bool c_isihead(a_i32 ch) {
    switch (ch) {
        case 'a' ... 'z':
        case 'A' ... 'Z':
        case '_':
            return true;
        default:
            return false;
    }
}

static a_bool c_isibody(a_i32 ch) {
    switch (ch) {
        case 'a' ... 'z':
        case 'A' ... 'Z':
        case '0' ... '9':
        case '_':
            return true;
        default:
            return false;
    }
}

always_inline a_msg l_bputx(Lexer* lex, a_i32 ch) {
    ai_buf_put(lex->_in._env, &lex->_buf, ch);
	return ALO_SOK;
}

static void l_bput(Lexer* lex, a_i32 ch) {
	a_msg msg = l_bputx(lex, ch);
	if (unlikely(msg != ALO_SOK)) {
		if (msg == ALO_EINVAL) {
			ai_lex_error(lex, "token too long.");
		}
		else {
			ai_mem_nomem(lex->_in._env);
		}
	}
}

static a_u32 strs_index(LexStrs* strs, a_hash hash) {
	return ((hash - 1) & strs->_hmask) + 1;
}

static a_u32 strs_next_free(LexStrs* strs) {
    a_u32 head = strs->_hfree;
    StrNode* node = &strs->_table[head];
	while (node->_str != null) {
		node = &strs->_table[++head];
	}
    strs->_hfree = head + 1;
    return head;
}

static void strs_add(LexStrs* strs, GStr* str) {
    assume(strs->_hfree <= strs->_hmask);
    a_u32 index = strs_index(strs, str->_hash);
    StrNode* node = &strs->_table[index];
    if (node->_str == null) {
        *node = new(StrNode) {str, ai_link_new() };
    }
    else {
        a_u32 index2 = strs_next_free(strs);
        StrNode* node2 = &strs->_table[index2];
        if (ai_link_prev(node) != null) {
			ai_link_move(node, node2);
            *node = new(StrNode) { str, ai_link_new() };
        }
        else {
			node2->_str = str;
			ai_link_insert_after(node, node2);
        }
    }
}

static void strs_grow(a_henv env, LexStrs* strs) {
    a_usize old_cap = strs->_hmask + 1;
    a_usize new_cap = old_cap * 2;
    StrNode* old_table = strs->_table;
    StrNode* new_table = ai_mem_vnew(env, StrNode, new_cap);

    strs->_hmask = new_cap - 1;
    strs->_hfree = 0;
    strs->_table = new_table;
	memclr(new_table, sizeof(StrNode) * new_cap);

    for (a_usize i = 0; i < old_cap; ++i) {
        GStr* str = old_table[i]._str;
        if (str != null) {
            strs_add(strs, str);
        }
    }

	ai_mem_vdel(G(env), old_table, old_cap);
}

static void strs_close(a_henv env, LexStrs* strs) {
	if (strs->_table != null) {
		ai_mem_vdel(G(env), strs->_table, strs->_hmask + 1);
	}
}

static GStr* l_tostr(Lexer* lex) {
    Buf* buf = &lex->_buf;
    GStr* str = ai_lex_tostr(lex, buf->_arr, buf->_len);
	ai_buf_reset(&lex->_buf);
    return str;
}

#define STRS_INIT_CAP 64

void ai_lex_init(a_henv env, Lexer* lex, a_ifun fun, void* ctx) {
	ai_io_iinit(env, fun, ctx, &lex->_in);
    lex->_file = null;
    lex->_line = 1;
    lex->_current = new(Token) {};
	lex->_channel = CHANNEL_NORMAL;
	lex->_scope = &lex->_scope0;
	lex->_scope0 = new(LexScope) {
		_up: null
	};
    lex->_strs = new(LexStrs) {
        _table: ai_mem_vnew(lex->_in._env, StrNode, STRS_INIT_CAP),
        _hmask: STRS_INIT_CAP - 1,
        _hfree: 0,
        _nstr: 0
    };
	memclr(lex->_strs._table, STRS_INIT_CAP * sizeof(StrNode));
    l_pollx(lex);
}

void ai_lex_close(Lexer* lex) {
	strs_close(lex->_in._env, &lex->_strs);
	ai_buf_close(lex->_in._env, &lex->_buf);
}

char const* ai_lex_tagname(a_i32 tag) {
	switch (tag) {
#define CASE_KW(id,repr) case TK_##id: return strx_raw_kw(id);
		KEYWORD_LIST(CASE_KW)
#undef CASE_KW
#define CASE_OP(id,repr) case TK_##id: return "'"repr"'";
		OPERATOR_LIST(CASE_OP)
#undef CASE_OP
		case TK_EOF: return "<eof>";
		case TK_IDENT: return "<identifier>";
		case TK_INTEGER: return "<integer>";
		case TK_FLOAT: return "<float>";
		case TK_STRING: return "<string>";
		case TK_TSTRING: return "<tstring>";
		default: unreachable();
	}
}

char const* ai_lex_tkrepr(Token* tk, a_tkbuf buf) {
    switch (tk->_tag) {
		case TK_IDENT: {
			return ai_str_tocstr(tk->_str);
		}
		case TK_INTEGER: {
			a_usize len = ai_fmt_i2s(buf + MAX_TOKEN_STR_BUF_SIZE, tk->_int);
			buf[MAX_TOKEN_STR_BUF_SIZE] = '\0';
			return cast(char const*, buf + MAX_TOKEN_STR_BUF_SIZE - len);
		}
		case TK_FLOAT: {
			a_usize len = ai_fmt_f2s(buf + MAX_TOKEN_STR_BUF_SIZE, tk->_float);
			buf[MAX_TOKEN_STR_BUF_SIZE] = '\0';
			return cast(char const*, buf + MAX_TOKEN_STR_BUF_SIZE - len);
		}
		case TK_STRING: {
			GStr* str = tk->_str;
			char* src = cast(char*, buf);
			if (str->_len > 16) { /* Hidden string content if it is too long. */
				sprintf(src, "<string with %u bytes>", str->_len);
			}
			else {
				sprintf(src, "\"%s\"", ai_str_tocstr(str));
			}
			return src;
		}
		default: {
			return ai_lex_tagname(tk->_tag);
		}
	}
}

GStr* ai_lex_tostr(Lexer* lex, void const* src, a_usize len) {
	a_henv env = lex->_in._env;
	LexStrs* strs = &lex->_strs;
	a_hash hash = ai_str_hashof(G(env)->_seed, src, len);

	StrNode* node = &strs->_table[strs_index(strs, hash)];
	if (node->_str != null) {
		do {
			if (node->_str->_hash == hash && ai_str_requals(node->_str, src, len)) {
				lex->_buf._len = 0;
				return node->_str;
			}
		}
		while ((node = ai_link_next(node)) != null);
	}

	if (strs->_nstr == strs->_hmask) {
		strs_grow(env, strs);
	}

	GStr* str = ai_str_create(env, src, len);
	strs_add(strs, str);
	strs->_nstr += 1;
	return str;
}

static a_i32 l_skip_line(Lexer* lex) {
    loop {
		switch (l_pollx(lex)) {
			case ALO_ESTMUF:
				return TK_EOF;
			case ALO_EOUTER:
				return ALO_EOUTER;
			case '\r': {
				l_testskip(lex, '\n');
				fallthrough;
			}
			case '\n': {
				lex->_line += 1;
				return TK__NONE;
			}
			default:
				break;
		}
	}
}

static a_i32 l_scan_ident(Lexer* lex, Token* tk) {
    while (c_isibody(l_peekx(lex))) {
        l_bput(lex, l_pollx(lex));
    }
    GStr* str = l_tostr(lex);
    tk->_str = str;
    return strx_iskw(str) ? strx_totk(str) : TK_IDENT;
}

static a_float l_10pow(a_i32 i) {
    static a_float const table[] = {
        1e1,
        1e2,
        1e4,
        1e8,
        1e16,
        1e32,
        1e64,
        1e128,
        1e256
    };
    a_bool neg = i < 0;
    if (neg) {
        i = -i;
    }
    a_u32 n = cast(a_u32, i);
    a_u32 m = 1;
    a_u32 l = 0;
    a_float f = 1.0;
    while (m <= n) {
        if (n & m) {
            f *= table[l];
        }
		m <<= 1;
        l += 1;
    }
    return neg ? 1.0 / f : f;
}

static a_i32 l_scan_number(Lexer* lex, Token* tk, a_i32 sign, a_i32 ch) {
    a_i32 e; /* Exponent count. */
    a_i32 eb;
    a_i32 j;
    a_u64 n;
    a_bool sep = false;

/* Check end seperator. */
#define check_end_sep() if (sep) goto error_bad_format

/* Check gap between number and other identifiers. */
#define check_gap() if (c_isibody(ch)) goto error_no_gap;

/* Scan seperator. */
#define scan_sep() do { \
    if (ch == '\'') { \
        if (sep) goto error_bad_format; \
        sep = true; \
        l_skip(lex); \
        continue; \
    } \
} while (false)

/* Scan digit sequence. */
#define scan_digits(d,e...) do { \
    ch = l_peekx(lex); \
    j = d(ch); \
    if (unlikely(j < 0)) { scan_sep(); break; } \
    l_skip(lex); \
    sep = false; \
    (e); \
} while (true)

    if (ch == '0') {
        a_u32 i;
        switch (ch = l_peekx(lex)) {
            case '.': {
                ch = l_peekx(lex);
                j = c_ddigit(ch);
                if (j > 0) {
                    e = 0;
					n = 0;
                    goto float_frag_base;
                }
                else if (j == 0) {
                    eb = 1;
                    loop {
                        ch = l_peekx(lex);
                        if (ch != '0') {
                            scan_sep();
                            break;
                        }
                        eb += 1;
                    }
                    j = c_ddigit(ch);
                    if (j >= 0) {
						n = 0;
						goto float_frag;
					}
                    tk->_float = 0.0;
                    return TK_FLOAT;
                }
                l_unwind(lex, '.');
                tk->_int = 0;
                return TK_INTEGER;
            }
            case 'x': case 'X': {
                l_skip(lex);
                scan_digits(c_hdigit, {
                    a_u32 t;
                    if (unlikely(checked_mul_u32(i, 16, &t))) {
                        n = cast(a_u64, i) * 16 + j;
                        eb = 0;
                        goto float_hex_head;
                    }
                    i = t + j;
                });

                if (l_testskip(lex, '.')) {
                    n = cast(a_u64, cast(a_i64, i));
                    ch = l_peekx(lex);
                    j = c_hdigit(ch);
                    if (j >= 0) goto float_hex_frag;
                    l_unwind(lex, '.');
                }

                check_gap();
                tk->_int = sign >= 0 ? cast(a_i32, i) : -cast(a_i32, i);
                return TK_INTEGER;
            }
            float_hex_head: {
                scan_digits(c_ddigit, {
                    n = n * 16 + j;
                    if (unlikely(n >= u64c(0x1000000000000))) {
                        eb = 1;
                        goto float_hex_ignore_int;
                    }
                });

                check_end_sep();
                if (unlikely(!l_testskip(lex, '.')))
                    goto error_overflow;

            float_hex_frag: /* Fragment part. */
                eb = 0;
                sep = true;
                scan_digits(c_ddigit, {
                    n = n * 16 + j;
                    eb -= 1;
                    if (unlikely(n >= u64c(0x1000000000000)))
                        goto float_hex_ignore_frag;
                });
                goto float_hex_exp;

            float_hex_ignore_int:
                scan_digits(c_hdigit, eb += 1);

                check_end_sep();
                if (unlikely(!l_testskip(lex, '.')))
                    goto error_overflow;
                ch = l_pollx(lex);
                if (unlikely(c_hdigit(ch) < 0))
                    goto error_overflow;
            
            float_hex_ignore_frag:
                scan_digits(c_hdigit, { /* Ignore digits. */ });

            float_hex_exp:
                check_end_sep();
                if (!(ch == 'p' || ch == 'P')) 
                    goto error_bad_format;
                l_skip(lex);
                e = 0;
                if (l_testskip(lex, '-')) {
                    scan_digits(c_ddigit, {
                        if (unlikely(checked_mul_i32(e, 10, &e) || checked_sub_i32(e, j, &e))) 
                            goto error_overflow;
                    });
                }
                else {
                    l_testskip(lex, '+');
                    scan_digits(c_ddigit, {
                        if (unlikely(checked_mul_i32(e, 10, &e) || checked_add_i32(e, j, &e))) 
                            goto error_overflow;
                    });
                }
                if (unlikely(checked_add_i32(e, eb, &e)))
                    goto error_overflow;
                
                a_float f = ldexp(cast(a_float, n), e * 4);
                if (!isfinite(f)) goto error_overflow;
                tk->_float = f;
                return TK_FLOAT;
            }
            case 'b': case 'B': {
                l_skip(lex);
                scan_digits(c_bdigit, {
                    a_u32 t;
                    if (unlikely(checked_mul_u32(i, 2, &t)))
                        goto error_overflow;
                    i = t + j;
                });
                check_gap();
                tk->_int = cast(a_int, i);
                return TK_INTEGER;
            }
            default: {
                check_gap();
                tk->_int = 0;
                return TK_INTEGER;
            }
        }
    }
    else {
        switch (sign) {
            case 0: { /* Unsigned number. */
                a_u32 i = c_ddigit(ch);
                scan_digits(c_ddigit, {
                    a_u32 t;
                    if (unlikely(checked_mul_u32(i, 10, &t) || checked_add_u32(t, j, &t))) {
                        n = cast(a_u64, i) * 10 + j;
                        sign = 1;
                        goto float_head;
                    }
                    i = t;
                });
                check_end_sep();
                if (l_testskip(lex, '.')) {
                    n = cast(a_u64, i);
                    ch = l_peekx(lex);
                    j = c_ddigit(ch);
                    if (j >= 0) goto float_frag_base;
                    l_unwind(lex, '.');
                }
                else {
                    check_gap();
                }
                tk->_int = cast(a_int, i);
                return TK_INTEGER;
            }
            case 1: { /* Positive number. */
                a_i32 i = c_ddigit(ch);
                scan_digits(c_ddigit, {
                    a_i32 t;
                    if (unlikely(checked_mul_i32(i, 10, &t) || checked_add_i32(t, j, &t))) {
                        n = cast(a_u64, cast(a_i64, i)) * 10 + j;
                        goto float_head;
                    }
                    i = t;
                });
                check_end_sep();
                if (l_testskip(lex, '.')) {
                    n = cast(a_u64, cast(a_i64, i));
                    ch = l_peekx(lex);
                    j = c_ddigit(ch);
                    if (j >= 0) goto float_frag_base;
                    l_unwind(lex, '.');
                }
                else {
                    check_gap();
                }
                tk->_int = cast(a_int, i);
                return TK_INTEGER;
            }
            case -1: { /* Negative number. */
                a_i32 i = -c_ddigit(ch);
                scan_digits(c_ddigit, {
                    a_i32 t;
                    if (unlikely(checked_mul_i32(i, 10, &t) || checked_sub_i32(t, j, &t))) {
                        n = cast(a_u64, -cast(a_i64, i)) * 10 + j;
                        goto float_head;
                    }
                    i = t;
                });
                check_end_sep();
                if (l_testskip(lex, '.')) {
                    n = cast(a_u64, -cast(a_i64, i));
                    ch = l_peekx(lex);
                    j = c_ddigit(ch);
                    if (j >= 0) goto float_frag_base;
                    l_unwind(lex, '.');
                }
                else {
                    check_gap();
                }
                tk->_int = cast(a_int, i);
                return TK_INTEGER;
            }
            float_head: {
                scan_digits(c_ddigit, {
                    n = n * 10 + j;
                    if (unlikely(n >= u64c(10000000000000000)))
                        goto float_ignore_int;
                });

                check_end_sep();
                if (unlikely(!l_testskip(lex, '.')))
                    goto error_overflow;
                ch = l_peekx(lex);
                j = c_ddigit(ch);
                if (unlikely(j < 0))
                    goto error_overflow;

			float_frag_base:
                eb = -1; /* For first digit. */

            float_frag: /* Fragment part. */
                l_skip(lex);
                n = n * 10 + j;
                scan_digits(c_ddigit, {
                    n = n * 10 + j;
                    eb -= 1;
                    if (unlikely(n >= u64c(10000000000000000)))
                        goto float_ignore_frag;
                });
                goto float_exp;
                
            float_ignore_int:
                eb = 1;
                scan_digits(c_ddigit, eb += 1);

                check_end_sep();
                if (unlikely(!l_testskip(lex, '.')))
                    goto error_overflow;
                ch = l_pollx(lex);
                if (unlikely(c_ddigit(ch) < 0))
                    goto error_overflow;
            
            float_ignore_frag:
                scan_digits(c_ddigit, { /* Ignore digits. */ });

            float_exp:
                if (ch == 'e' || ch == 'E') { /* Exponent part. */
                    l_skip(lex);
                    e = 0;
                    if (l_testskip(lex, '-')) {
                        scan_digits(c_ddigit, {
                            if (unlikely(checked_mul_i32(e, 10, &e) || checked_sub_i32(e, j, &e))) 
                                goto error_overflow;
                        });
                    }
                    else {
                        l_testskip(lex, '+');
                        scan_digits(c_ddigit, {
                            if (unlikely(checked_mul_i32(e, 10, &e) || checked_add_i32(e, j, &e))) 
                                goto error_overflow;
                        });
                    }
                    if (unlikely(checked_add_i32(e, eb, &e)))
                        goto error_overflow;
                }
                else {
                    e = eb;
                }
                a_float f = cast(a_float, n) * l_10pow(e);
                if (!isfinite(f)) goto error_overflow;
                check_gap();
                tk->_float = f;
                return TK_FLOAT;
            }
            default: unreachable();
        }
    }

error_overflow:
    ai_lex_error(lex, "number overflow.");

error_bad_format:
    ai_lex_error(lex, "bad number format.");

error_no_gap:
    ai_lex_error(lex, "seperator expected between number and other identifier.");

#undef check_end_sep
#undef check_gap
#undef scan_sep
#undef scan_digits
}

static a_i32 l_scan_edigit(Lexer* lex) {
    a_i32 i = c_hdigit(l_poll(lex));
    if (i < 0) ai_lex_error(lex, "bad escape character.");
    return i;
}

static void l_scan_echar(Lexer* lex) {
    a_i32 ch = l_poll(lex);
	if (ch == '\\') {
		switch (l_poll(lex)) {
			case 'b': {
				l_bput(lex, '\b');
				break;
			}
			case 'f': {
				l_bput(lex, '\f');
				break;
			}
			case 'n' : {
				l_bput(lex, '\n');
				break;
			}
			case 'r' : {
				l_bput(lex, '\r');
				break;
			}
			case 't' : {
				l_bput(lex, '\t');
				break;
			}
			case 'v' : {
				l_bput(lex, '\v');
				break;
			}
			case '\\': {
				l_bput(lex, '\\');
				break;
			}
			case '\'': {
				l_bput(lex, '\'');
				break;
			}
			case '\"': {
				l_bput(lex, '\"');
				break;
			}
			case '`' : {
				l_bput(lex, '`');
				break;
			}
			case '$' : {
				l_bput(lex, '$');
				break;
			}
			case '0' : {
				l_bput(lex, '\0');
				break;
			}
			case 'x': {
				a_i32 a = l_scan_edigit(lex);
				a_i32 b = l_scan_edigit(lex);
				l_bput(lex, a << 4 | b);
				break;
			}
			case 'u': {
				a_i32 a = l_scan_edigit(lex);
				a_i32 b = l_scan_edigit(lex);
				a_i32 c = l_scan_edigit(lex);
				a_i32 d = l_scan_edigit(lex);
				ch = a << 12 | b << 8 | c << 4 | d;
				if (ch > 0xfff) {
					l_bput(lex, 0xe0 | ch >> 12);
					l_bput(lex, 0x80 | (ch >> 6 & 0x3f));
					l_bput(lex, 0x80 | (ch & 0x3f));
				}
				else if (ch > 0x7f) {
					l_bput(lex, 0xc0 | ch >> 6);
					l_bput(lex, 0x80 | (ch & 0x3f));
				}
				else {
					l_bput(lex, ch);
				}
				break;
			}
			case '\r': {
				l_testskip(lex, '\n');
				fallthrough;
			}
			case '\n': {
				lex->_line += 1;
				break;
			}
			default: ai_lex_error(lex, "bad escape character.");
		}
	}
	else {
		l_bput(lex, ch);
	}
}

#ifndef ALOI_HTAB_IDENT
# define ALOI_HTAB_IDENT 4
#endif

static a_bool l_scan_triple_quotes(Lexer* lex, a_i32 quote) {
	if (l_testskip(lex, quote)) {
		if (l_testskip(lex, quote)) {
			if (likely(l_testskip(lex, quote))) {
				return true;
			}
			l_bput(lex, quote);
		}
		l_bput(lex, quote);
	}
	return false;
}

static a_none l_error_unclosed(Lexer* lex, a_u32 line) {
	ai_lex_error(lex, "unclosed string. (from line %u)", line);
}

#define S_OK i32c(0)
#define S_LINE i32c(1)
#define S_TEXT i32c(3)

static a_i32 l_scan_multi_echar(Lexer* lex, a_i32 quote, a_u32 line) {
	switch (l_pollx(lex)) {
		case '\r': {
			l_testskip(lex, '\n');
			fallthrough;
		}
		case '\n': {
			lex->_line += 1;
			l_bput(lex, '\n');
			return S_LINE;
		}
		case ALO_ESTMUF:
			l_error_unclosed(lex, line);
		default: {
			if (l_scan_triple_quotes(lex, quote))
				return S_TEXT;
			l_scan_echar(lex);
			break;
		}
	}
	return S_OK;
}

static a_i32 l_scan_string_indent(Lexer* lex) {
	a_i32 ident = 0;
	loop {
		switch (l_peek(lex)) {
			case ' ': {
				ident += 1;
				break;
			}
			case '\t': {
				ident += ALOI_HTAB_IDENT;
				break;
			}
			default: {
				return ident;
			}
		}
	}
}

static a_bool l_skip_string_indent(Lexer* lex, a_u32 indent, a_i32 quote) {
	loop {
		switch (l_pollx(lex)) {
			case ' ': {
				indent -= 1;
				if (indent == 0) return false;
				break;
			}
			case '\t': {
				if (checked_sub_u32(indent, ALOI_HTAB_IDENT, &indent)) {
					if (unlikely(indent != 0))
						goto error_bad_indent;
					return false;
				}
				break;
			}
			case '\r': {
				l_testskip(lex, '\n');
				fallthrough;
			}
			case '\n': {
				lex->_line += 1;
				l_bputx(lex, '\n');
				return false;
			}
			default: {
				if (l_testskip(lex, quote) && l_testskip(lex, quote) && l_testskip(lex, quote))
					return true;
				goto error_bad_indent;
			}
		}
	}

error_bad_indent:
	ai_lex_error(lex, "bad indentation for multi-line string.");
}

static a_i32 l_scan_single_quoted_string(Lexer* lex, Token* tk) {
	a_u32 line = lex->_line;
	if (l_testskip(lex, '\'')) {
		if (l_testskip(lex, '\'')) {
			/* Multiline string. */
			if (l_testskip(lex, '\n')) {
				a_i32 indent = l_scan_string_indent(lex);
				check(indent);
				while (!l_skip_string_indent(lex, indent, '\'')) {
					a_i32 msg;
					while (!((msg = l_scan_multi_echar(lex, '\'', line)) & S_LINE));
					if (msg & S_TEXT) break;
				}
			}
			else {
				while (!(l_scan_multi_echar(lex, '\'', line) & S_TEXT));
			}
			tk->_str = l_tostr(lex);
		}
		else {
			tk->_str = ai_env_strx(G(lex->_in._env), STRX__EMPTY);
		}
	}
	else {
		while (!l_testskip(lex, '\'')) {
			switch (l_peek(lex)) {
				case ALO_ESTMUF:
				case '\r':
				case '\n':
					ai_lex_error(lex, "unclosed string.");
				default: {
					l_scan_echar(lex);
					break;
				}
			}
		}
		tk->_str = l_tostr(lex);
	}
	return TK_STRING;
}

static a_i32 l_scan_template_string_escape(Lexer* lex, Token* tk);

static a_i32 l_scan_template_string_body(Lexer* lex, Token* tk) {
	while (!l_testskip(lex, '\"')) {
		switch (l_peek(lex)) {
			case ALO_ESTMUF:
			case '\r':
			case '\n':
				ai_lex_error(lex, "unclosed string.");
			case '$': {
				if (lex->_buf._len != 0) {
					tk->_str = l_tostr(lex);
					return TK_TSTRING;
				}
				else {
					lex->_channel = CHANNEL_TSTR_ESCAPE;
					l_skip(lex);
					return l_scan_template_string_escape(lex, tk);
				}
			}
			default: {
				l_scan_echar(lex);
				break;
			}
		}
	}
	tk->_str = l_tostr(lex);
	lex->_channel = CHANNEL_NORMAL;
	return TK_STRING;
}

static a_i32 l_scan_multiline_template_string_body(Lexer* lex, Token* tk) {
	/* Multiline string. */
	a_u32 indent = lex->_scope->_indent;
	if (indent > 0) {
		while (!l_skip_string_indent(lex, indent, '\"')) {
			a_i32 msg;
			while (!((msg = l_scan_multi_echar(lex, '\"', lex->_scope->_begin_line)) & S_LINE));
			if (msg & S_TEXT) break;
		}
	}
	else {
		while (!(l_scan_multi_echar(lex, '\"', lex->_scope->_begin_line) & S_TEXT));
	}
	tk->_str = l_tostr(lex);
	lex->_channel = CHANNEL_NORMAL;
	return TK_STRING;
}

static a_i32 l_scan_double_quoted_string(Lexer* lex, Token* tk) {
	if (l_testskip(lex, '\"')) {
		if (l_testskip(lex, '\"')) {
			l_switch_scope(lex, CHANNEL_MTSTR_BODY);
			if (l_testskip(lex, '\n')) {
				a_i32 n = l_scan_string_indent(lex);
				check(n);
				lex->_scope->_indent = n;
			}
			return l_scan_multiline_template_string_body(lex, tk);
		}
		else {
			tk->_str = ai_env_strx(G(lex->_in._env), STRX__EMPTY);
			return TK_STRING;
		}
	}
	else {
		l_switch_scope(lex, CHANNEL_TSTR_BODY);
		return l_scan_template_string_body(lex, tk);
	}
}

static a_i32 l_scan_normal(Lexer* lex, Token* tk) {
    loop {
        a_i32 ch;
        switch (ch = l_poll(lex)) {
            case ALO_ESTMUF: {
                return TK_EOF;
            }
            case ' ':
            case '\t':
            case '\f': {
                break;
            }
            case '\r': {
                l_testskip(lex, '\n');
                fallthrough;
            }
            case '\n': {
                lex->_line += 1;
				tk->_line = lex->_line;
                break;
            }
            case '(': {
                return TK_LBK;
            }
            case ')': {
                return TK_RBK;
            }
            case '[': {
                return TK_LSQ;
            }
            case ']': {
                return TK_RSQ;
            }
            case '{': {
                return TK_LBR;
            }
            case '}': {
                return TK_RBR;
            }
            case '#': {
                return TK_SHARP;
            }
            case '@': {
                return TK_AT;
            }
            case '~': {
                return TK_TILDE;
            }
			case ',': {
				return TK_COMMA;
			}
			case ';': {
				return TK_SEMI;
			}
            case '.': {
                if (l_testskip(lex, '.')) {
                    if (l_testskip(lex, '.')) {
                        return TK_TDOT;
                    }
                    return TK_BDOT;
                }
                return TK_DOT;
            }
            case ':': {
                if (l_testskip(lex, ':')) {
                    return TK_BCOLON;
                }
                return TK_COLON;
            }
            case '+': {
                if (l_peekx(lex) >= '0' && l_peekx(lex) <= '9') {
                    return l_scan_number(lex, tk, 1, l_pollx(lex));
                }
                return TK_PLUS;
            }
            case '-': {
                if (l_testskip(lex, '-')) {
                    check(l_skip_line(lex));
                    break;
                }
                else if (l_peekx(lex) >= '0' && l_peekx(lex) <= '9') {
                    return l_scan_number(lex, tk, -1, l_pollx(lex));
                }
                return TK_MINUS;
            }
            case '*': {
                return TK_STAR;
            }
            case '/': {
                return TK_LSLASH;
            }
            case '%': {
                return TK_PERCENT;
            }
            case '^': {
                return TK_HAT;
            }
            case '=': {
                if (l_testskip(lex, '=')) {
                    return TK_EQ;
                }
                return TK_ASSIGN;
            }
            case '!': {
				if (l_testskip(lex, '=')) {
					return TK_NE;
				}
                return TK_NOT;
            }
            case '<': {
                if (l_testskip(lex, '<')) {
                    return TK_SHL;
                }
                else if (l_testskip(lex, '=')) {
                    return TK_LE;
                }
                return TK_LT;
            }
            case '>': {
                if (l_testskip(lex, '>')) {
                    return TK_SHR;
                }
                else if (l_testskip(lex, '=')) {
                    return TK_GE;
                }
                return TK_GT;
            }
            case '&': {
                if (l_testskip(lex, '&')) {
                    return TK_BAMP;
                }
                return TK_AMP;
            }
            case '|': {
                if (l_testskip(lex, '|')) {
                    return TK_BBAR;
                }
                return TK_BAR;
            }
            case '?': {
				if (l_testskip(lex, ':')) {
					return TK_ELVIS;
				}
				else if (l_testskip(lex, '.')) {
					return TK_QDOT;
				}
                return TK_QUESTION;
            }
            case 'a' ... 'z': 
            case 'A' ... 'Z': 
            case '_': {
                l_bput(lex, ch);
                return l_scan_ident(lex, tk);
            }
            case '0' ... '9': {
                return l_scan_number(lex, tk, 0, ch);
            }
            case '\'': {
                return l_scan_single_quoted_string(lex, tk);
            }
			case '\"': {
				return l_scan_double_quoted_string(lex, tk);
			}
            default: {
				ai_lex_error(lex, "invalid character, got '%c'", ch);
            }
        }
    }
}

static a_i32 l_scan_template_string_escape(Lexer* lex, Token* tk) {
	a_i32 ch = l_poll(lex);
	a_u32 channel = lex->_channel ^ 0x1; /* Load body channel. */
	switch (ch) {
		case '{': {
			lex->_channel = channel;
			l_switch_scope(lex, CHANNEL_NORMAL);
			return TK_LBR;
		}
		case 'a' ... 'z':
		case 'A' ... 'Z':
		case '_': {
			l_bputx(lex, ch);
			a_i32 t = l_scan_ident(lex, tk);
			lex->_channel = channel;
			return t;
		}
		case ALO_ESTMUF:
			return TK_EOF;
		default:
			ai_lex_error(lex, "bad escape string character.");
	}
}

static void l_scan(Lexer* lex, Token* tk) {
	tk->_line = lex->_line; /* Initialize line number. */
	if (likely(lex->_channel == CHANNEL_NORMAL)) { /* Fast path of scanner. */
		tk->_tag = l_scan_normal(lex, tk);
	}
	else {
		switch (lex->_channel) { /* Slow path of scanner. */
			case CHANNEL_TSTR_ESCAPE:
			case CHANNEL_MTSTR_ESCAPE: {
				tk->_tag = l_scan_template_string_escape(lex, tk);
				break;
			}
			case CHANNEL_TSTR_BODY: {
				tk->_tag = l_scan_template_string_body(lex, tk);
				break;
			}
			case CHANNEL_MTSTR_BODY: {
				tk->_tag = l_scan_multiline_template_string_body(lex, tk);
				break;
			}
			default: unreachable();
		}
	}
}

a_i32 ai_lex_forward(Lexer* lex) {
	assume(lex->_current._tag != TK__NONE, "cannot call forward() before poll current token.");
	if (lex->_forward._tag == TK__NONE) {
		l_scan(lex, &lex->_forward);
	}
	return lex->_forward._tag;
}

a_i32 ai_lex_peek(Lexer* lex) {
    if (lex->_current._tag == TK__NONE) {
		if (lex->_forward._tag != TK__NONE) {
			lex->_current = lex->_forward;
			lex->_forward._tag = TK__NONE;
		}
		else {
			l_scan(lex, &lex->_current);
		}
    }
    return lex->_current._tag;
}