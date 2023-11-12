/**
 *@file atype.h
 */

#ifndef atype_h_
#define atype_h_

#include "aobj.h"

intern GType* ai_type_new(a_henv env, GStr* name);
intern void ai_type_boost(a_henv env);
intern void ai_type_clean(Global* gbl);

#endif /* atype_h_ */
