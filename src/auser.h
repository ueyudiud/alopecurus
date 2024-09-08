/**
 *@file auser.h
 */

#ifndef auser_h_
#define auser_h_

#include "aobj.h"

struct GUser {
    GOBJ_STRUCT_HEADER;
};

#define g_is_user(o) g_is(o, ALO_TUSER)

always_inline a_bool v_is_user(Value v) {
    return v_is(v, T_OTHER) && g_is_user(v_as_obj(v));
}

always_inline GUser* v_as_user(Value v) {
    assume(v_is_user(v), "not userdata.");
    return g_as(GUser, v_as_obj(v));
}

#endif /* auser_h_ */
