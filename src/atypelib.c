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

static GType* check_type(a_henv env, a_ilen id) {
    aloL_checktag(env, id, ALO_TTYPE);
    return v_as_type(api_elem(env, id));
}

static a_msg type___call__(a_henv env) { /* Should this function write in script? */
    GType* self = check_type(env, 0);

    if (aloL_gets(env, 0, "__new__") == ALO_EEMPTY) {
        aloL_raisef(env, "method '%s.__new__' not found.", str2ntstr(self->name));
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
