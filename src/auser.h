/**
 *@file auser.h
 */

#ifndef auser_h_
#define auser_h_

#include "aobj.h"

struct GUser {
    GOBJ_STRUCT_HEADER;
};

#define v_is_user(v) v_is(v, T_USER)

always_inline GUser* v_as_user(Value v) {
    assume(v_is_user(v), "not userdata.");
    return g_cast(GUser, v_as_obj(v));
}

#endif /* auser_h_ */
