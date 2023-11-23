/**
 *@file atype.h
 */

#ifndef atype_h_
#define atype_h_

#include "aobj.h"
#include "atable.h"

intern GType* ai_type_new(a_henv env, GStr* name);
intern void ai_type_boost(a_henv env);
intern void ai_type_clean(Global* gbl);

/**
 ** Type.
 */
struct GType {
    GTABLE_STRUCT_HEADER;
    /* Type metadata (Optional). */
    GStr* _name;
};

#define FTM_BIT(tm) (u16c(1) << (tm))

#define mt_has_ftm(t,tm) (((t)->_tmz & FTM_BIT(tm)) == 0)

always_inline GTable* type2mt(GType* o) {
    return g_cast(GTable, o);
}

#define type_size(e) pad_to_raw(sizeof(GType) + (e), sizeof(a_usize))

#endif /* atype_h_ */
