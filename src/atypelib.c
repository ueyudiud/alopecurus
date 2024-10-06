/**
 *@file atypelib.c
 */

#define atypelib_c_
#define ALO_LIB

#include "aobj.h"
#include "atype.h"
#include "agc.h"
#include "aapi.h"

#include "alo.h"
#include "aauxlib.h"
#include "alolib.h"

static a_msg type___call__(a_henv env) { /* Should this function write in script? */
    aloL_checktag(env, 0, ALO_TTYPE);

    if (aloL_gets(env, 0, "__new__") == ALO_EEMPTY) {
        GType* self = g_as(GType, v_as_obj(api_elem(env, 0)));
        aloL_raisef(env, "no entry for '%s.__new__'", str2ntstr(self->name));
    }

    alo_pop(env, 0);
    a_usize n = alo_stacksize(env);
    alo_call(env, n - 1, 1);
    return 1;
}

void aloopen_type(a_henv env) {
    static aloL_Entry const bindings[] = {
        { "__call__", type___call__ }
    };

    alo_pushptype(env, ALO_TTYPE);
    aloL_putalls(env, -1, bindings);
}
