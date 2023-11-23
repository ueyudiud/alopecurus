/**
 *@file aintlib.c
 */

#define aintlib_c_
#define ALO_LIB

#include <stdlib.h>
#include <errno.h>
#include <ctype.h>

#include "aapi.h"

#include "alolib.h"

#define l_pushuint(env,v) alo_pushint(env, cast(a_int, v))
#define l_checkuint(env,id) cast(a_uint, aloL_checkint(env, id))

#define INT_BITS (sizeof(a_int) * CHAR_BIT)

static a_msg int___new__(a_henv env) {
    if (alo_tagof(env, 0) == ALO_EEMPTY) {
        alo_pushint(env, 0);
    }
    else {
        Value v = api_elem(env, 0);
        if (v_is_float(v)) {
            alo_pushint(env, cast(a_int, v_as_float(v)));
        }
        else switch (v_get_tag(v)) {
                case T_FALSE: {
                    alo_pushint(env, 0);
                    break;
                }
                case T_TRUE: {
                    alo_pushint(env, 1);
                    break;
                }
                case T_INT: {
                    alo_settop(env, 1);
                    break;
                }
                case T_STR: {
                    a_int radix = aloL_optint(env, 1, 0);
                    GStr* str = v_as_str(v);
                    char const* p = str2ntstr(str);
                    char* q; /* strtol() says end pointer should be mutable. */

                    if (isspace(p[0]))
                        goto einval;

                    int old_err = errno;
                    errno = 0;
                    a_int i = strtol(p, &q, radix);
                    int the_err = errno;
                    errno = old_err;

                    if (q != p + str->_len)
                        goto einval;
                    if (the_err == ERANGE)
                        goto erange;
                    alo_pushint(env, i);
                    break;

                einval:
                    aloL_argerror(env, 0, "cannot parse string to integer.");
                    break;

                erange:
                    aloL_argerror(env, 0, "integer out of range.");
                    break;
                }
                default: {
                    if (aloL_gettm(env, 0, "__int__") == ALO_EEMPTY) {
                        aloL_argerror(env, 0, "cannot convert to int.");
                    }
                    a_isize n = alo_insert(env, 0);
                    alo_call(env, n - 1, 1);
                    break;
                }
            }
    }
    return 1;
}

static a_msg int_rol(a_henv env) {
    a_uint i = l_checkuint(env, 0);
    a_int n = aloL_checkint(env, 1);

    n &= INT_BITS - 1;

    a_uint o = (i << n) | (i >> (INT_BITS - n));

    l_pushuint(env, o);
    return 1;
}

static a_msg int_ror(a_henv env) {
    a_uint i = l_checkuint(env, 0);
    a_int n = aloL_checkint(env, 1);

    n &= INT_BITS - 1;

    a_uint o = (i >> n) | (i << (INT_BITS - n));

    l_pushuint(env, o);
    return 1;
}

void aloopen_int(a_henv env) {
	static aloL_Entry const bindings[] = {
        { "rol", int_rol },
        { "ror", int_ror },
        { "__new__", int___new__ },
        { "__look__", null },
        { "MAX", null },
        { "MIN", null }
	};

    alo_pushptype(env, ALO_TINT);
    aloL_putalls(env, -1, bindings);

    alo_pushint(env, INT32_MAX);
    aloL_puts(env, -2, "MAX");

    alo_pushint(env, INT32_MIN);
    aloL_puts(env, -2, "MIN");

    alo_push(env, -1);
    aloL_puts(env, -2, "__look__");
}
