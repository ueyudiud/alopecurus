/**
 *@file aintlib.c
 */

#define aintlib_c_
#define ALO_LIB

#include <stdlib.h>

#include "atable.h"
#include "agc.h"
#include "avm.h"
#include "aapi.h"

#include "alolib.h"

static a_msg int_new(a_henv env) {
    if (alo_tagof(env, 0) == ALO_EEMPTY) {
        alo_pushint(env, 0);
        return 1;
    }

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
            alo_settop(env, 2);
            break;
        }
        case T_STR: {
            a_int radix = aloL_optint(env, 1, 0);
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
            aloL_argerror(env, 0, "cannot parse string to integer.");
            break;

        erange:
            aloL_argerror(env, 0, "integer out of range.");
            break;
        }
        default: {
            aloL_argerror(env, 0, "cannot convert to int."); //TODO __int__ meta call.
        }
    }
    return 1;
}

void aloopen_int(a_henv env) {
	static aloL_Entry const bindings[] = {
        { "__new__", int_new },
        { "MAX", null },
        { "MIN", null },
	};

    GMeta* type = g_cast(GMeta, g_type(env, _int));

    v_set_obj(env, api_incr_stack(env), type);
	aloL_putfields(env, -1, bindings);

    ai_meta_usets(env, type, ai_str_newl(env, "MAX"), v_of_int(INT32_MAX));
    ai_meta_usets(env, type, ai_str_newl(env, "MIN"), v_of_int(INT32_MIN));

    ai_gc_trigger(env);
}
