/**
 *@file atable.h
 */

#ifndef atable_h_
#define atable_h_

#include "alink.h"
#include "aobj.h"

typedef struct TNode TNode;

/**
 ** Linked hash table.
 */
struct GTable {
	GOBJ_STRUCT_HEADER;
	a_u32 _len;
	a_u32 _hmask; /* Hash mask. */
	BUF_DATA_DEF(TNode); /* Data pointer. */
	LHEAD_DEF; /* Head of linked list. */
	a_u32 _hfree; /* Last hash free slot. */
};

/**
 ** Table node.
 */
struct TNode {
	Value _value;
	Value _key;
	a_hash _hash;
	a_x32 _hnext;
	LINK_DEF;
};

intern GTable* ai_table_new(a_henv env);
intern void ai_table_hint(a_henv env, GTable* self, a_usize len);
intern Value const* ai_table_refi(a_henv env, GTable* self, a_int key);
intern Value const* ai_table_refs(a_henv env, GTable* self, a_lstr const* key);
intern Value const* ai_table_ref(a_henv env, GTable* self, Value key);
intern void ai_table_set(a_henv env, GTable* self, Value key, Value value);

intern VTable const ai_table_vtable;

#endif /* atable_h_ */
