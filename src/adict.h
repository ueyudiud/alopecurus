/**
 *@file adict.h
 */

#ifndef adict_h_
#define adict_h_

#include "aobj.h"

typedef struct Dict Dict;

intern void ai_dict_close(a_henv env, Dict* dict);
intern a_u32 ai_dict_push_in_place(Dict* dict, GStr* key, a_u32* hfree);
intern a_u32 ai_dict_push(a_henv env, Dict* dict, GStr* key, a_u32* hfree);
intern a_u32 const* ai_dict_find(Dict* dict, GStr* key);
intern void ai_dict_splash(Global* g, Dict* dict);

#define ai_dict_new() (new(Dict) { ._len = 0, ._mask = 0, ._arr = null })

typedef struct {
	GStr* _key;
	a_u32 _index;
	a_x32 _next;
} DNode;

struct Dict {
	a_u32 _len;
	a_u32 _mask;
	DNode* _arr;
};

#endif /* adict_h_ */
