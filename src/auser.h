/**
 *@file auser.h
 */

#ifndef auser_h_
#define auser_h_

#include "atype.h"

intern GUser* ai_user_new(a_henv env, GUType* type);
intern GUser* ai_user_clone(a_henv env, GUser* self);
intern void ai_user_mark(Global* gbl, a_gptr self);
intern void ai_user_close(Global* gbl, a_gptr self);
intern void ai_user_drop(Global* gbl, a_gptr self);

struct GUser {
    Value slot[0];
    GOBJ_STRUCT_HEADER;
    a_byte block[];
};

#define g_is_user(o) g_is(o, ALO_TUSER)

always_inline a_bool v_is_user(Value v) {
    return v_is(v, T_USER);
}

always_inline GUser* v_as_user(Value v) {
    assume(v_is_user(v), "not userdata.");
    return g_as(GUser, v_as_obj(v));
}

always_inline Value v_of_user(GUser* o) {
    assume(g_is_user(o));
    return v_of_obj_(o, T_USER);
}

always_inline void v_set_user(a_henv env, Value* d, GUser* o) {
    Value v = v_of_user(o);
    v_set(env, d, v);
}

always_inline a_usize user_size(GUType* t) {
    return sizeof(GUType) + t->block_size + sizeof(Value) * t->num_slot;
}

always_inline Value* user_slot(GUser* o, a_u32 i) {
    return o->slot - i;
}

#endif /* auser_h_ */
