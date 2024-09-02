/**
 *@file atype.h
 */

#ifndef atype_h_
#define atype_h_

#include "aobj.h"
#include "atable.h"
#include "amod.h"

intern GType* ai_type_new(a_henv env, GStr* name);
intern void ai_type_boost(a_henv env);
intern void ai_type_clean(Global* gbl);

/**
 ** Type.
 */
struct GType {
    GMOD_STRUCT_HEADER;
    /* Type metadata (Optional). */
    GStr* name;
};

#define FTM_BIT(tm) (u16c(1) << (tm))

#define mt_has_ftm(t,tm) (((t)->ftmz & FTM_BIT(tm)) == 0)

always_inline GMod* type2mt(GType* o) {
    return gmod_cast(o);
}

#define g_is_type(o) g_is(o, ALO_TTYPE)

always_inline a_bool v_is_type(Value v) {
    return v_is(v, T_META) && g_is_type(v_as_obj(v));
}

always_inline GType* v_as_type(Value v) {
    assume(v_is_type(v), "not type.");
    return g_cast(GType, v_as_obj(v));
}

always_inline Value v_of_type(GType* o) {
    assume(g_is_type(o), "invalid instance.");
    return v_of_obj_(o, T_META);
}

always_inline void v_set_type(a_henv env, Value* d, GType* o) {
    Value v = v_of_type(o);
    v_set(env, d, v);
}

#define type_size(e) align_to(sizeof(GType) + (e), sizeof(a_usize))

#endif /* atype_h_ */
