/**
 *@file ameta.h
 */

#ifndef ameta_h_
#define ameta_h_

#include "aobj.h"

typedef struct GUserMeta GUserMeta;

struct GUserMeta {
	GMETA_STRUCT_HEADER;
	GMeta* _proto; /* Prototype of value construct by this metadata. */
	a_u32 _cfield;
	a_u32 _field_hfree;
	Value* _fields;
};

#endif /* ameta_h_ */
