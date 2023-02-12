/**
 *@file ameta.h
 */

#ifndef ameta_h_
#define ameta_h_

#include "adict.h"
#include "aobj.h"

typedef struct GUserMeta GUserMeta;

intern GUserMeta* ai_umeta_new(a_henv env, GMeta* proto);
intern Value const* ai_umeta_get(a_henv env, GUserMeta* self, GStr* key);
intern void ai_umeta_set(a_henv env, GUserMeta* self, GStr* key, Value value);

struct GUserMeta {
	GMETA_STRUCT_HEADER;
	GMeta* _proto; /* Prototype of value construct by this metadata. */
	a_u32 _cfield;
	a_u32 _field_hfree;
	Dict _field_indices;
	Value* _fields;
};

#endif /* ameta_h_ */
