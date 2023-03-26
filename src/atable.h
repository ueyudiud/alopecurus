/**
 *@file atable.h
 */

#ifndef atable_h_
#define atable_h_

#include "amap.h"
#include "aobj.h"

typedef struct TNode TNode;

intern GTable* ai_table_new(a_henv env);
intern void ai_table_hint(a_henv env, GTable* self, a_usize len);
intern Value const* ai_table_refi(a_henv env, GTable* self, a_int key);
intern Value const* ai_table_refs(a_henv env, GTable* self, a_lstr const* key);
intern Value const* ai_table_refis(a_henv env, GTable* self, GStr* key);
intern Value const* ai_table_ref(a_henv env, GTable* self, Value key);
intern Value ai_table_getis(a_henv env, GTable* self, GStr* key);
intern Value ai_table_get(a_henv env, GTable* self, Value key);
intern void ai_table_set(a_henv env, GTable* self, Value key, Value value);
intern void ai_table_emplaces(a_henv env, GTable* self, GStr* key, Value value);

#define GTABLE_STRUCT_HEADER \
	GOBJ_STRUCT_HEADER;         \
	a_u32 BUF_LEN_NAME;         \
	MAP_HMASK_DEF; /* Hash mask. */ \
	BUF_PTR_DEF(TNode); /* Data pointer. */ \
	LIST_LINK_DEF /* Head of linked list. */

/**
 ** Linked hash table.
 */
struct GTable {
	GTABLE_STRUCT_HEADER;
	a_u32 _hfree; /* Last hash free slot. */
};

static_assert(offsetof(GObj, _len) == offsetof(GTable, _len));

/**
 ** Table node.
 */
struct TNode {
	Value _value;
	Value _key;
	a_hash _hash;
	HLINK_NEXT_DEF;
	NODE_LINK_DEF;
};

always_inline a_bool tnode_is_empty(TNode* node) {
	return v_is_empty(node->_key);
}

always_inline a_bool g_is_table(a_hobj v) {
	return v->_vtable->_repr_id == REPR_TABLE;
}

always_inline GTable* g_as_table(a_hobj v) {
	assume(g_is_table(v));
	return g_cast(GTable, v);
}

always_inline GTable* v_as_table(Value v) {
	assume(v_is_table(v), "not table.");
	return g_as_table(v_as_obj(v));
}

#endif /* atable_h_ */
