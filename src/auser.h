/**
 *@file auser.h
 */

#ifndef auser_h_
#define auser_h_

#include "atype.h"

intern GUser* ai_user_new(a_henv env, GUType* type);
intern GUser* ai_user_clone(a_henv env, GUser* self);

struct GUser {
    Value slot[0];
    GOBJ_STRUCT_HEADER;
    a_byte block[];
};

#define g_is_user(o) g_is(o, ALO_TUSER)

always_inline a_bool v_is_user(Value v) {
    return v_is(v, T_OTHER) && g_is_user(v_as_obj(v));
}

always_inline GUser* v_as_user(Value v) {
    assume(v_is_user(v), "not userdata.");
    return g_as(GUser, v_as_obj(v));
}

always_inline a_usize user_size(GUType* t) {
    return sizeof(GUType) + t->block_size + sizeof(Value) * t->num_slot;
}

#endif /* auser_h_ */
