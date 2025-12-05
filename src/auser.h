/**
 *@file auser.h
 */

#ifndef auser_h_
#define auser_h_

#include "atype.h"

ALO_PRIVATE GUser* ai_user_new(a_henv env, GUType* type);
ALO_PRIVATE GUser* ai_user_clone(a_henv env, GUser* self);
ALO_PRIVATE void ai_user_mark(Global* gbl, a_gptr self);
ALO_PRIVATE void ai_user_close(Global* gbl, a_gptr self);
ALO_PRIVATE void ai_user_drop(Global* gbl, a_gptr self);

struct GUser {
    Value slot[0];
    GOBJ_STRUCT_HEADER;
    a_byte block[];
};

#define g_is_user(o) g_is(o, ALO_TUSER)

ALO_INLINE a_bool v_is_user(Value v) {
    return v_is(v, T_USER);
}

ALO_INLINE GUser* v_as_user(Value v) {
    assume(v_is_user(v), "not userdata.");
    return g_as(GUser, v_as_ref(v));
}

ALO_INLINE Value v_of_user(GUser* o) {
    assume(g_is_user(o));
    return v_of_ref(o, T_USER);
}

ALO_INLINE void v_set_user(a_henv env, Value* d, GUser* o) {
    Value v = v_of_user(o);
    v_set(env, d, v);
}

ALO_INLINE a_usize user_size(GUType* t) {
    return sizeof(GUType) + t->block_size + sizeof(Value) * t->num_slot;
}

ALO_INLINE Value* user_slot(GUser* o, a_u32 i) {
    return o->slot - i;
}

#endif /* auser_h_ */
