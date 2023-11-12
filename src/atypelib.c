/**
 *@file atypelib.c
 */

#define atypelib_c_
#define ALO_LIB

#include "aobj.h"
#include "atype.h"
#include "atable.h"
#include "agc.h"
#include "aapi.h"

#include "alo.h"
#include "aauxlib.h"
#include "alolib.h"

static a_msg type___call__(a_henv env) { /* Should this function write in script? */
    aloL_checktag(env, 0, ALO_TTYPE);

    GType* self = g_cast(GType, v_as_obj(api_elem(env, 0)));
    Value v;

    a_msg msg = ai_table_gets(env, type2mt(self), ai_str_from_ntstr(env, "__new__"), &v);
    if (msg == ALO_SOK) {
        Value* p = env->_frame->_stack_bot;
        a_usize n = env->_stack._top - p;

        v_set(env, p, v);

        alo_call(env, n - 1, 1);
    }
    else {
        //TODO No constructor, try default initializer.
        aloL_raisef(env, "no entry for '%s.__new__'", str2ntstr(self->_name));
    }

    return 1;
}

void aloopen_type(a_henv env) {
    static aloL_Entry const bindings[] = {
        {"__call__", type___call__ }
    };

    alo_pushptype(env, ALO_TTYPE);
    aloL_putall(env, -1, bindings);
}
