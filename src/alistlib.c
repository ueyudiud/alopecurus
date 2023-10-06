/**
 *@file alistlib.c
 */

#define alistlib_c_
#define ALO_LIB

#include "aobj.h"
#include "alist.h"
#include "agc.h"
#include "aapi.h"

#include "alo.h"
#include "aauxlib.h"
#include "alolib.h"

static a_msg list___new__(a_henv env) { /* Should this function write in script? */
    if (alo_stacksize(env) == 0) {
        alo_newlist(env, 0);
    }
    else {
        Value v = api_elem(env, 0);
        if (v_is_float(v))
            goto error;
        switch (v_get_tag(v)) {
            case T_INT: {
                alo_newlist(env, cast(a_uint, v_as_int(v)));
                break;
            }
            case T_TUPLE: {
                GTuple* init_val = v_as_tuple(v);

                GList* out = ai_list_new(env);
                v_set_obj(env, api_incr_stack(env), out);
                ai_list_push_all(env, out, init_val->_ptr, init_val->_len);

                ai_gc_trigger(env);
                break;
            }
            case T_LIST: {
                GList* init_val = v_as_list(v);

                GList* out = ai_list_new(env);
                v_set_obj(env, api_incr_stack(env), out);
                ai_list_push_all(env, out, init_val->_ptr, init_val->_len);

                ai_gc_trigger(env);
                break;
            }
            default: {
            error:
                aloL_argerror(env, 1, "initial size or collection expected."); //TODO iterable value support.
            }
        }
    }
    return 1;
}

static a_msg list_get(a_henv env) {
    aloL_checktag(env, 0, ALO_TLIST);
    a_int k = aloL_checkint(env, 1);

    GList* self = v_as_list(api_elem(env, 0));
    Value v;

    a_msg msg = ai_list_ugeti(env, self, k, &v);
    if (msg == ALO_SOK) {
        v_set(env, api_incr_stack(env), v);
    }
    else {
        alo_settop(env, 3); /* or null, return nil or default value. */
    }

    return 1;
}

static a_msg list_push(a_henv env) {
    aloL_checktag(env, 0, ALO_TLIST);
    aloL_checkany(env, 1);

    GList* self = v_as_list(api_elem(env, 0));
    Value v = api_elem(env, 1);

    ai_list_push(env, self, v);

    return 0;
}

void aloopen_list(a_henv env) {
    static aloL_Entry const bindings[] = {
        {"__new__", list___new__ },
        { "get", list_get },
        { "push", list_push }
    };

    v_set_obj(env, api_incr_stack(env), g_type(env, _list));
    aloL_putfields(env, -1, bindings);
}
