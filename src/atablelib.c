/**
 *@file atablelib.c
 */

#define atablelib_c_
#define ALO_LIB

#include "atable.h"
#include "aapi.h"

#include "aauxlib.h"
#include "alolib.h"

static a_enum mode_from_str(a_henv env, a_ulen id, char const* str) {
    a_enum mode = REFERENCE_STRONG;
    while (*str) {
        switch (*(str++)) {
            case 'k': {
                mode |= REFERENCE_WEAKKEY;
                break;
            }
            case 'v': {
                mode |= REFERENCE_WEAK;
                break;
            }
            default: {
                aloL_argerror(env, id, "invalid table mode");
            }
        }
    }
    return mode;
}

static a_msg table___new__(a_henv env) {
    Value v = api_elem(env, 0);
    a_enum mode;
    if (v_is_str(v)) {
        mode = mode_from_str(env, 0, str2ntstr(v_as_str(v)));
        v = v_of_nil();
    }
    else {
        char const* mode_str = aloL_optstr(env, 1);
        if (mode_str != null) {
            mode = mode_from_str(env, 1, mode_str);
        }
    }
    switch (v_get_tag(v)) {
        case T_NIL: {
            alo_newtablex(env, 0, mode);
            break;
        }
        case T_INT: {
            alo_newtablex(env, cast(a_uint, v_as_int(v)), mode);
            break;
        }
        default: {
            aloL_argerror(env, 1, "initial size or collection expected."); //TODO iterable value support.
        }
    }
    return 1;
}

void aloopen_table(a_henv env) {
    static aloL_Entry const bindings[] = {
        { "__look__", null },
        { "__new__", table___new__ }
    };

    alo_pushptype(env, ALO_TTABLE);
    aloL_putalls(env, -1, bindings);

    alo_push(env, -1);
    aloL_puts(env, -2, "__look__");
}
