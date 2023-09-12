/**
 *@file aintlib.c
 */

#define aintlib_c_
#define ALO_LIB

#include <stdlib.h>
#include <errno.h>

#include "agc.h"
#include "avm.h"
#include "aapi.h"

#include "alolib.h"

static a_u32 int_new(a_henv env) {
    Value v = api_elem(env, 0);
    switch (v_get_tag(v)) {
        case T_NIL:
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
        case T_FLOAT: {
            alo_pushint(env, cast(a_int, v_as_float(v)));
            break;
        }
        case T_HSTR:
        case T_ISTR: {
            a_int radix = aloL_optint(env, 1, 0);
            GStr* str = v_as_str(v);
            char const* p = str2ntstr(str);
            char const* q;

            if (isspace(p[0]))
                goto einval;

            errno = 0;
            a_int i = strtol(p, &q, radix);
            if (p == q)
                goto einval;
            if (errno == ERANGE)
                goto erange;
            alo_pushint(env, i);
            break;

        einval:
            aloL_argerror(env, 0, "not number value.");
            break;

        erange:
            aloL_argerror(env, 0, "integer out of range.");
            break;
        }
        default: {
            alo_typeof(env, 0);
            break;
        }
    }
    return 1;
}

void aloopen_int(a_henv env) {
	static aloL_Entry const bindings[] = {
        { "__call__", int_new }
	};

    v_set_obj(env, api_incr_stack(env), &G(env)->_types._int);
	aloL_putfields(env, -1, bindings);
}
