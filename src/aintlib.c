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

static a_msg int_new(a_henv env) {
    Value v = api_elem(env, 1); //TODO value checking.
    if (v_is_float(v)) {
        alo_pushint(env, cast(a_int, v_as_float(v)));
    }
    else switch (v_get_tag(v)) {
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
            alo_settop(env, 2);
            break;
        }
        case T_STR: {
            a_int radix = aloL_optint(env, 2, 0);
            GStr* str = v_as_str(v);
            char const* p = str2ntstr(str);
            char* q; /* strtol() says end pointer should be mutable. */

            if (isspace(p[0]))
                goto einval;

            errno = 0;
            a_int i = strtol(p, &q, radix);
            if (q != p + str->_len)
                goto einval;
            if (errno == ERANGE)
                goto erange;
            alo_pushint(env, i);
            break;

        einval:
            aloL_argerror(env, 1, "cannot parse string to integer.");
            break;

        erange:
            aloL_argerror(env, 1, "integer out of range.");
            break;
        }
        default: {
            aloL_argerror(env, 1, "cannot convert to int."); //TODO __int__ meta call.
        }
    }
    return 1;
}

void aloopen_int(a_henv env) {
	static aloL_Entry const bindings[] = {
        { "__new__", int_new }
	};

    v_set_obj(env, api_incr_stack(env), &G(env)->_types._int);
	aloL_putfields(env, -1, bindings);
}
