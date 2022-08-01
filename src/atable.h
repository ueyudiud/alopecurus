/**
 *@file atable.h
 */

#ifndef atable_h_
#define atable_h_

#include "aobj.h"

typedef struct TNode TNode;

/**
 ** Linked hash table.
 */
struct GTable {
	GOBJ_STRUCT_HEADER;
	a_u32 _len;
	a_u32 _hmask; /* Hash mask. */
	TNode* _data; /* Biased data pointer. */
	a_u32 _lhead; /* Head node of linked list. */
	a_u32 _ltail; /* Tail node of linked list. */
	a_u32 _hfree; /* Last hash free slot. */
};

/**
 ** Table node.
 */
struct TNode {
	Value _value;
	Value _key;
	a_hash _hash;
	a_i32 _hnext;
	a_i32 _lprev;
	a_i32 _lnext;
};

intern GTable* ai_table_new(a_henv env);
intern void ai_table_hint(a_henv env, GTable* self, a_usize len);
intern Value const* ai_table_geti(a_henv env, GTable* self, a_int key);
intern Value const* ai_table_gets(a_henv env, GTable* self, a_lstr const* key);
intern void ai_table_splash(Global* g, GTable* self);
intern void ai_table_destruct(Global* g, GTable* self);

#endif /* atable_h_ */
