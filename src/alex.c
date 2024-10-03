/**
 *@file alex.c
 */

#define alex_c_
#define ALO_LIB

#include <math.h>
#include <stdio.h>

#include "aenv.h"
#include "aerr.h"
#include "afmt.h"
#include "aparse.h"

#include "alex.h"

static a_noret l_foreign_error(Lexer* lex) {
	ai_err_raise(lex->env, ALO_EOUTER, v_of_int(lex->in.err));
}

always_inline a_i32 l_poll_unchecked(Lexer* lex) {
    a_i32 ch = lex->_char;
    lex->_char = ai_io_igetc(&lex->in);
    return ch;
}

static void l_back(Lexer* lex, a_i32 ch) {
    assume(ch >= 0, "cannot unwind error.");
    lex->in.ptr -= 1;
    lex->in.len += 1;
    lex->_char = ch;
}

#define l_peek(lex) ((lex)->_char)

static a_i32 l_poll(Lexer* lex) {
	a_i32 ch = l_poll_unchecked(lex);
	if (unlikely(ch == ALO_EOUTER)) {
		l_foreign_error(lex);
	}
	return ch;
}

#define l_skip(lex) quiet(l_poll(lex))

#define l_test(lex,ch) (l_peek(lex) == (ch))

#define l_test_skip(lex,ch) (l_test(lex, ch) && (l_poll_unchecked(lex), true))

static a_i32 c_bdigit(a_i32 ch) {
	if (ch >= '0' && ch <= '1') {
		return ch - '0';
	}
	else {
		return -1;
	}
}

static a_i32 c_ddigit(a_i32 ch) {
	if (ch >= '0' && ch <= '9') {
		return ch - '0';
	}
	else {
		return -1;
	}
}

static a_i32 c_xdigit(a_i32 ch) {
    switch (ch) {
        case '0' ... '9': return ch - '0';
        case 'a' ... 'f': return ch - 'a' + 10;
        case 'A' ... 'F': return ch - 'A' + 10;
        default: return -1;
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

static void l_bput(Lexer* lex, a_i32 ch) {
    at_buf_putc(lex->env, lex->buf, ch);
}

static a_bool strs_put(StrSet* set, GStr* str) {
    a_u32 index = str->hash & set->hmask;
    a_u32 perturb = str->hash;
    loop {
        GStr** slot = &set->ptr[index];
        if (*slot == str) {
            return false;
        }
        else if (*slot == null) {
            *slot = str;
            return true;
        }
        perturb >>= 5;
        index = (index * 5 + 1 + perturb) & set->hmask;
    }
}

static void strs_alloc_array(a_henv env, StrSet* self, a_usize cap) {
	GStr** ptr = ai_mem_vnew(env, GStr*, cap);
    memclr(ptr, sizeof(GStr*) * cap);

	self->hmask = cap - 1;
	self->ptr = ptr;
}

#define STRS_INIT_CAP 64

#if ALO_M64
# define STRS_MAX_CAP (u32c(1) << 31)
#else
# define STRS_MAX_CAP ((u32c(1) << 31) / sizeof(GStr*))
#endif

static void strs_grow(Lexer* lex, StrSet* self) {
    a_usize old_cap = self->hmask + 1;
	if (unlikely(old_cap == STRS_MAX_CAP)) {
		ai_lex_error(lex, "too many symbol and string in chunk.", lex->line);
	}

	a_usize new_cap = old_cap * 2;
    GStr** old_ptr = self->ptr;

	strs_alloc_array(lex->env, self, new_cap);

    for (a_usize i = 0; i < old_cap; ++i) {
        GStr* str = old_ptr[i];
        if (str != null) {
			a_bool succ = strs_put(self, str);
            assume(succ, "duplicate string.");
        }
    }

	ai_mem_vdel(G(lex->env), old_ptr, old_cap);
}

static void strs_close(a_henv env, StrSet* strs) {
	if (strs->ptr != null) {
		ai_mem_vdel(G(env), strs->ptr, strs->hmask + 1);
	}
}

static GStr* l_to_str(Lexer* lex) {
    Buf* buf = lex->buf;
    GStr* str = ai_lex_to_str(lex, buf->ptr, buf->len);
	at_buf_clear(lex->buf);
    return str;
}

void ai_lex_init(a_henv env, Lexer* lex, a_ifun fun, void* ctx, char const* file) {
	ai_io_iinit(env, fun, ctx, &lex->in);

    lex->file = file;
    lex->line = 1;

	lex->strs.len = 0;
}

void ai_lex_open(Lexer* lex, a_flags options) {
    strs_alloc_array(lex->env, &lex->strs, STRS_INIT_CAP);

    if (!(options & ALO_COMP_OPT_STRIP_DEBUG) && lex->file != null) {
        GStr* str = ai_lex_to_str(lex, lex->file, strlen(lex->file));
        lex->file = str2ntstr(str);
    }

    l_poll_unchecked(lex); /* scan next char */
}

void ai_lex_close(Lexer* lex) {
	strs_close(lex->env, &lex->strs);
	at_buf_deinit(G(lex->env), lex->buf);
}

char const* ai_lex_tagname(a_i32 tag) {
	static a_u16 const l_offs[] = {
#define STRDEF(n) [TK_##n] = STR_POS_##n,
#define STRDEF2(n,r) [TK_##n] = STR_POS_##n,
# include "asym/kw.h"
# include "asym/op.h"
#undef STRDEF
#undef STRDEF2
	};

	return &ai_str_interns[l_offs[tag]];
}

char const* ai_lex_tkrepr(Token* tk, a_tkbuf buf) {
    switch (tk->tag) {
		case TK_IDENT: {
			return str2ntstr(tk->as_str);
		}
		case TK_INTEGER: {
			a_usize len = ai_fmt_int2str(buf + MAX_TOKEN_STR_BUF_SIZE, tk->as_int);
			buf[MAX_TOKEN_STR_BUF_SIZE] = '\0';
			return &buf[MAX_TOKEN_STR_BUF_SIZE - len];
		}
		case TK_FLOAT: {
			a_usize len = ai_fmt_float2str(buf + MAX_TOKEN_STR_BUF_SIZE, tk->as_float);
			buf[MAX_TOKEN_STR_BUF_SIZE] = '\0';
			return &buf[MAX_TOKEN_STR_BUF_SIZE - len];
		}
		case TK_STRING: {
			GStr* str = tk->as_str;
			char* src = cast(char*, buf);
			if (str->len > 16) { /* Hidden string content if it is too long. */
				sprintf(src, "<string with %u bytes>", str->len);
			}
			else {
				sprintf(src, "\"%s\"", str2ntstr(str));
			}
			return src;
		}
		default: {
			return ai_lex_tagname(tk->tag);
		}
	}
}

a_noret ai_lex_report(Lexer* lex, char const* fmt, ...) {
    va_list varg;
    va_start(varg, fmt);
    ai_err_raisevf(lex->env, ALO_ECHUNK, fmt, varg);
    va_end(varg);
}

GStr* ai_lex_to_str(Lexer* lex, void const* src, a_usize len) {
	a_henv env = lex->env;
	StrSet* set = &lex->strs;

    GStr* str = ai_str_get_or_new(env, src, len);

    if (strs_put(set, str)) {
        set->len += 1;
        if (set->len > set->hmask * 3 / 4) {
            strs_grow(lex, set);
        }
    }

	return str;
}

static a_i32 l_skip_line(Lexer* lex) {
    loop {
		switch (l_poll_unchecked(lex)) {
			case ALO_EEMPTY:
				return TK_EOF;
			case ALO_EOUTER:
				return ALO_EOUTER;
			case '\r': {
				l_test_skip(lex, '\n');
				fallthrough;
			}
			case '\n': {
				lex->line += 1;
				return TK__NONE;
			}
			default: break;
		}
	}
}

/**
 ** Assume next token is an identifier or keyword.
 *@return the ID of next token.
 */
static a_i32 l_scan_ident(Lexer* lex, Token* tk) {
    while (c_isibody(l_peek(lex))) {
        l_bput(lex, l_poll_unchecked(lex));
    }
    GStr* str = l_to_str(lex);
    tk->as_str = str;
    return str_iskw(str) ? str_totk(str) : TK_IDENT;
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

enum {
    SIGN_NONE = 0,
    SIGN_POS = +1,
    SIGN_NEG = -1
};

/**
 ** Scan integer or float point number literal.
 *@param lex the lexer.
 *@param tk the output token.
 *@param sign the sign of number, 0 for unsigned number and 1 or -1 for positive or negative number.
 *@param ch the leading character.
 *@return the token kind.
 */
static a_i32 l_scan_number(Lexer* lex, Token* tk, a_i32 sign, a_i32 ch) {
    a_i32 e; /* Exponent count. */
    a_i32 eb;
    a_i32 j;
    a_u64 n;
    a_bool sep = false;

/* Check end seperator. */
#define check_end_sep() if (sep) goto error_format

/* Check gap between number and other identifiers. */
#define check_gap() if (c_isibody(ch)) goto error_no_gap;

/* Scan seperator. */
#define scan_sep() ({          \
    if (ch == '\'') {          \
        if (sep)               \
			goto error_format; \
        sep = true;            \
        l_skip(lex);           \
        if (l_test(lex, '\'')) \
			goto error_format; \
    }                          \
})

/* Scan digit sequence. */
#define scan_digits(d) while ( \
	ch = l_peek(lex),          \
	j = d(ch),                 \
	(j >= 0 ? (                \
		l_skip(lex),           \
		sep = false,           \
		true                   \
	) : (                      \
		scan_sep(),            \
		false                  \
	)))

    if (ch == '0') {
        a_u32 i = 0;
        switch (ch = l_peek(lex)) {
            case '.': {
                l_skip(lex);
                ch = l_peek(lex);
                j = c_ddigit(ch);
                if (j >= 0) {
                    e = 0;
                    n = 0;
                    goto float_frag;
                }
                l_back(lex, '.');
                tk->as_int = 0;
                return TK_INTEGER;
            }
            case 'x': case 'X': {
				l_skip(lex);
                scan_digits(c_xdigit) {
					a_u32 t;
					catch (ckd_mul(&t, i, 16)) {
                        /* Overflow cases, the literal can only be float value. */
						n = cast(a_u64, i) * 16 + j;
						eb = 0;
						goto float_hex_head;
					}
					i = t + j;
				}

                if (l_test_skip(lex, '.')) {
                    n = cast(a_u64, i);
                    ch = l_peek(lex);
                    j = c_xdigit(ch);
                    if (j >= 0) goto float_hex_frag;
                    l_back(lex, '.');
                }

                check_gap();
                tk->as_int = sign >= 0 ? bit_cast(a_i32, i) : -bit_cast(a_i32, i);
                return TK_INTEGER;
            }
            float_hex_head: {
                scan_digits(c_ddigit) {
					n = n * 16 + j;
					catch (n >= u64c(0x1000000000000)) {
						eb = 1;
						goto float_hex_ignore_int;
					}
				}

                check_end_sep();
                if (unlikely(!l_test_skip(lex, '.')))
                    goto error_overflow;

            float_hex_frag: /* Fragment part. */
                eb = 0;
                sep = true;
                scan_digits(c_ddigit) {
					n = n * 16 + j;
					eb -= 1;
					catch (n >= u64c(0x1000000000000)) {
                        goto float_hex_ignore_frag;
                    }
				}
                goto float_hex_exp;

            float_hex_ignore_int:
                scan_digits(c_xdigit) {
					eb += 1;
				}

                check_end_sep();
                if (!l_test_skip(lex, '.'))
                    goto error_overflow;
                ch = l_poll(lex);
                if (c_xdigit(ch) < 0)
                    goto error_overflow;
            
            float_hex_ignore_frag:
				/* Ignore digits. */
                scan_digits(c_xdigit);

            float_hex_exp:
                check_end_sep();
                if (!(ch == 'p' || ch == 'P')) 
                    goto error_format;
				l_skip(lex);
                e = 0;
                if (l_test_skip(lex, '-')) {
                    scan_digits(c_ddigit) {
						catch (ckd_mul(&e, e, 10) || ckd_sub(&e, e, j)) {
                            goto error_overflow;
                        }
					}
                }
                else {
                    l_test_skip(lex, '+');
                    scan_digits(c_ddigit) {
						catch (ckd_mul(&e, e, 10) || ckd_add(&e, e, j)) {
                            goto error_overflow;
                        }
					}
                }
                catch (ckd_add(&e, e, eb)) {
                    goto error_overflow;
                }
                
                a_float f = ldexp(cast(a_float, n), e * 4);
                if (unlikely(!isfinite(f))) goto error_overflow;
                tk->as_float = f;
                return TK_FLOAT;
            }
            case 'b': case 'B': {
				l_skip(lex);
                scan_digits(c_bdigit) {
					a_u32 t;
					catch (ckd_mul(&t, i, 2)) {
                        goto error_overflow;
                    }
					i = t + j;
				}
                check_gap();
                tk->as_int = cast(a_int, i);
                return TK_INTEGER;
            }
            default: {
                check_gap();
                tk->as_int = 0;
                return TK_INTEGER;
            }
        }
    }
    else {
        switch (sign) {
            case SIGN_NONE: { /* Unsigned number. */
                a_u32 i = c_ddigit(ch);
                scan_digits(c_ddigit) {
					a_u32 t;
                    catch (ckd_mul(&t, i, 10) || ckd_add(&t, t, j)) {
						n = cast(a_u64, i) * 10 + j;
						sign = 1;
						goto float_head;
					}
					i = t;
				}
                check_end_sep();
                if (l_test_skip(lex, '.')) {
                    n = cast(a_u64, i);
                    ch = l_peek(lex);
                    j = c_ddigit(ch);
                    if (j >= 0) goto float_frag;
                    l_back(lex, '.');
                }
                else {
                    check_gap();
                }
                tk->as_int = cast(a_int, i);
                return TK_INTEGER;
            }
            case SIGN_POS: { /* Positive number. */
                a_i32 i = c_ddigit(ch);
                scan_digits(c_ddigit) {
					a_i32 t;
					catch (ckd_mul(&t, i, 10) || ckd_add(&t, t, j)) {
						n = cast(a_u64, cast(a_i64, i)) * 10 + j;
						goto float_head;
					}
					i = t;
				}
                check_end_sep();
                if (l_test_skip(lex, '.')) {
                    n = cast(a_u64, cast(a_i64, i));
                    ch = l_peek(lex);
                    j = c_ddigit(ch);
                    if (j >= 0) goto float_frag;
                    l_back(lex, '.');
                }
                else {
                    check_gap();
                }
                tk->as_int = cast(a_int, i);
                return TK_INTEGER;
            }
            case SIGN_NEG: { /* Negative number. */
                a_i32 i = -c_ddigit(ch);
                scan_digits(c_ddigit) {
					a_i32 t;
					catch (ckd_mul(&t, i, 10) || ckd_sub(&t, t, j)) {
						n = cast(a_u64, -cast(a_i64, i)) * 10 + j;
						goto float_head;
					}
					i = t;
				}
                check_end_sep();
                if (l_test_skip(lex, '.')) {
                    n = cast(a_u64, -cast(a_i64, i));
                    ch = l_peek(lex);
                    j = c_ddigit(ch);
                    if (j >= 0) goto float_frag;
                    l_back(lex, '.');
                }
                else {
                    check_gap();
                }
                tk->as_int = cast(a_int, i);
                return TK_INTEGER;
            }
            float_head: {
                scan_digits(c_ddigit) {
					n = n * 10 + j;
					catch (n >= u64c(10000000000000000)) {
                        goto float_ignore_int;
                    }
				}

                check_end_sep();
                if (!l_test_skip(lex, '.'))
                    goto error_overflow;
                ch = l_peek(lex);
                j = c_ddigit(ch);
                if (j < 0)
                    goto error_overflow;

			float_frag:
                eb = -1; /* For first digit. */

				l_skip(lex);
                n = n * 10 + j;
                scan_digits(c_ddigit) {
					n = n * 10 + j;
					eb -= 1;
					catch (n >= u64c(10000000000000000)) {
                        goto float_ignore_frag;
                    }
				}
                goto float_exp;
                
            float_ignore_int:
                eb = 1;
                scan_digits(c_ddigit) {
					eb += 1;
				}

                check_end_sep();
                if (!l_test_skip(lex, '.'))
                    goto error_overflow;
                ch = l_poll(lex);
                if (c_ddigit(ch) < 0)
                    goto error_overflow;
            
            float_ignore_frag:
				/* Ignore digits. */
                scan_digits(c_ddigit);

            float_exp:
                if (ch == 'e' || ch == 'E') { /* Exponent part. */
					l_skip(lex);
                    e = 0;
                    if (l_test_skip(lex, '-')) {
                        scan_digits(c_ddigit) {
                            catch (ckd_mul(&e, e, 10) || ckd_sub(&e, e, j)) {
                                goto error_overflow;
                            }
						}
                    }
                    else {
                        l_test_skip(lex, '+');
                        scan_digits(c_ddigit) {
							catch (ckd_mul(&e, e, 10) || ckd_add(&e, e, j)) {
                                goto error_overflow;
                            }
						}
                    }
                    catch (ckd_add(&e, e, eb)) {
                        goto error_overflow;
                    }
                }
                else {
                    e = eb;
                }
                a_float f = cast(a_float, n) * l_10pow(e);
                if (sign == -1)
                    f = -f;
                if (unlikely(!isfinite(f)))
                    goto error_overflow;
                check_gap();
                tk->as_float = f;
                return TK_FLOAT;
            }
            default: unreachable();
        }
    }

error_overflow:
    ai_lex_error(lex, "number overflow.", lex->line);

error_format:
    ai_lex_error(lex, "bad number format.", lex->line);

error_no_gap:
    ai_lex_error(lex, "separator expected between number and other identifier.", lex->line);

#undef check_end_sep
#undef check_gap
#undef scan_sep
#undef scan_digits
}

static a_i32 l_scan_xdigit(Lexer* lex) {
    a_i32 i = c_xdigit(l_poll(lex));
    if (i < 0) ai_lex_error(lex, "bad escape character.", lex->line);
    return i;
}

static a_noret l_error_unclosed(Lexer* lex, a_u32 line) {
	if (lex->line == line) {
		ai_lex_error(lex, "unclosed string.", lex->line);
	}
	else {
		ai_lex_error(lex, "unclosed string. (from line %u)", lex->line, line);
	}
}

static void l_scan_sqchr(Lexer* lex, a_u32 line) {
	a_i32 ch = l_poll(lex);
	switch (ch) {
		case '\r': {
			l_test_skip(lex, '\n');
			fallthrough;
		}
		case '\n': {
			lex->line += 1;
			break;
		}
		case ALO_EEMPTY: {
			l_error_unclosed(lex, line);
			break;
		}
		default: {
			l_bput(lex, ch);
			break;
		}
	}
}

static a_i32 l_scan_sqstr(Lexer* lex, Token* tk) {
	a_u32 line = lex->line;
	while (!l_test_skip(lex, '\'')) {
		l_scan_sqchr(lex, line);
	}
	tk->as_str = l_to_str(lex);
	return TK_STRING;
}

static a_bool l_test_tesc_head(Lexer* lex) {
	a_i32 ch = l_peek(lex);
	switch (ch) {
		case '(':
		case '[':
		case '{':
		case 'a' ... 'z':
		case 'A' ... 'Z':
		case '_': 
			return true;
		default:
			return false;
	}
}

static a_i32 l_scan_dqchr(Lexer* lex, Token* tk, a_u32 line) {
    a_i32 ch = l_poll(lex);
	switch (ch) {
		case ALO_EEMPTY: {
			l_error_unclosed(lex, line);
		}
		case '\\': {
			switch (ch = l_poll(lex)) {
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
				case '$' : {
					l_bput(lex, '$');
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
				case '0' : {
					l_bput(lex, '\0');
					break;
				}
				case 'x': {
					a_i32 a = l_scan_xdigit(lex);
					a_i32 b = l_scan_xdigit(lex);
					l_bput(lex, a << 4 | b);
					break;
				}
				case 'u': {
					a_i32 a = l_scan_xdigit(lex);
					a_i32 b = l_scan_xdigit(lex);
					a_i32 c = l_scan_xdigit(lex);
					a_i32 d = l_scan_xdigit(lex);
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
					l_test_skip(lex, '\n');
					fallthrough;
				}
				case '\n': {
					lex->line += 1;
					break;
				}
				default: ai_lex_error(lex, "bad escape character '\\%c'.", lex->line, ch);
			}
			break;
		}
		case '$': {
			if (l_test_tesc_head(lex)) {
				if (lex->buf->len == 0) {
					return TK_TSESCAPE;
				}
				tk->as_str = l_to_str(lex);
				lex->ahead[1].tag = TK_TSESCAPE;
				return TK_STRING;
			}
			l_bput(lex, '$');
			break;
		}
		case '\r': {
			l_test_skip(lex, '\n');
			fallthrough;
		}
		case '\n': {
			lex->line += 1;
			l_bput(lex, '\n');
			break;
		}
		case '\"': {
			if (lex->buf->len == 0)
				return TK_TSEND;

			tk->as_str = l_to_str(lex);
			lex->ahead[1].tag = TK_TSEND;
			return TK_STRING;
		}
		default: {
			l_bput(lex, ch);
			break;
		}
	}
	return TK__NONE;
}

static a_i32 l_scan_dqstr(Lexer* lex, Token* tk, a_u32 line) {
	loop try (l_scan_dqchr(lex, tk, line));
}

static a_i32 l_scan_plain(Lexer* lex, Token* tk) {
	tk->line = lex->line;
    loop {
        a_i32 ch;
        switch (ch = l_poll(lex)) {
            case ALO_EEMPTY: {
                return TK_EOF;
            }
            case ' ':
            case '\t':
            case '\f': {
                break;
            }
            case '\r': {
                l_test_skip(lex, '\n');
                fallthrough;
            }
            case '\n': {
                lex->line += 1;
				tk->line = lex->line;
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
                if (l_test_skip(lex, '.')) {
                    if (l_test_skip(lex, '.')) {
                        return TK_TDOT;
                    }
                    return TK_BDOT;
                }
                return TK_DOT;
            }
            case ':': {
                return TK_COLON;
            }
            case '+': {
                if (l_test_skip(lex, '+')) {
					return TK_BPLUS;
				}
                else if (l_peek(lex) >= '0' && l_peek(lex) <= '9') {
                    return l_scan_number(lex, tk, SIGN_POS, l_poll_unchecked(lex));
                }
                return TK_PLUS;
            }
            case '-': {
                if (l_test_skip(lex, '-')) {
                    try (l_skip_line(lex));
                    break;
                }
                else if (l_peek(lex) >= '0' && l_peek(lex) <= '9') {
                    return l_scan_number(lex, tk, SIGN_NEG, l_poll_unchecked(lex));
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
                if (l_test_skip(lex, '=')) {
                    return TK_EQ;
                }
                return TK_ASSIGN;
            }
            case '!': {
				if (l_test_skip(lex, '=')) {
					return TK_NE;
				}
                return TK_BANG;
            }
            case '<': {
                if (l_test_skip(lex, '<')) {
                    return TK_SHL;
                }
                else if (l_test_skip(lex, '=')) {
                    return TK_LE;
                }
                return TK_LT;
            }
            case '>': {
                if (l_test_skip(lex, '>')) {
                    return TK_SHR;
                }
                else if (l_test_skip(lex, '=')) {
                    return TK_GE;
                }
                return TK_GT;
            }
            case '&': {
                if (l_test_skip(lex, '&')) {
                    return TK_BAMP;
                }
                return TK_AMP;
            }
            case '|': {
                if (l_test_skip(lex, '|')) {
                    return TK_BBAR;
                }
                return TK_BAR;
            }
            case '?': {
				if (l_test_skip(lex, '.')) {
					return TK_QDOT;
				}
				else if (l_test_skip(lex, ':')) {
					return TK_ELVIS;
				}
				else if (l_test_skip(lex, '?')) {
					return TK_BQUESTION;
				}
				else if (l_test_skip(lex, '-')) {
					if (l_test_skip(lex, '>')) {
						return TK_QARROW;
					}
                    l_back(lex, '-');
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
                return l_scan_number(lex, tk, SIGN_NONE, ch);
            }
            case '\'': {
                return l_scan_sqstr(lex, tk);
            }
			case '\"': {
				return TK_TSBEGIN;
			}
            default: {
				ai_lex_error(lex, "invalid character, got '%c'", lex->line, ch);
            }
        }
    }
}

static void l_scan(Lexer* lex, Token* tk) {
	tk->line = lex->line; /* Initialize line number. */
	tk->tag = l_scan_plain(lex, tk);
}

static void l_scan2(Lexer* lex, Token* tk, a_u32 line) {
	tk->line = lex->line;
	tk->tag = l_scan_dqstr(lex, tk, line);
}

a_i32 ai_lex_forward(Lexer* lex) {
	assume(lex->ahead[0].tag != TK__NONE, "cannot call forward() before poll current token.");
	if (lex->ahead[1].tag == TK__NONE) {
		l_scan(lex, &lex->ahead[1]);
	}
	return lex->ahead[1].tag;
}

a_i32 ai_lex_peek(Lexer* lex) {
    if (lex->ahead[0].tag == TK__NONE) {
		if (lex->ahead[1].tag != TK__NONE) {
			lex->ahead[0] = lex->ahead[1];
			lex->ahead[1].tag = TK__NONE;
		}
		else {
			l_scan(lex, &lex->ahead[0]);
		}
    }
    return lex->ahead[0].tag;
}

a_i32 ai_lex_peek2(Lexer* lex, a_u32 line) {
    if (lex->ahead[0].tag == TK__NONE) {
		if (lex->ahead[1].tag != TK__NONE) {
			lex->ahead[0] = lex->ahead[1];
			lex->ahead[1].tag = TK__NONE;
		}
		else {
			l_scan2(lex, &lex->ahead[0], line);
		}
    }
    return lex->ahead[0].tag;
}
