/**
 *@file atable.h
 */

#ifndef atable_h_
#define atable_h_

#include "amap.h"
#include "aobj.h"

typedef struct GModLoader GModLoader;

typedef struct ModLoader ModLoader;
typedef struct ModCache ModCache;

intern GTable* ai_table_new(a_henv env);
intern void ai_table_hint(a_henv env, GTable* self, a_usize len);
intern Value const* ai_table_refi(a_henv env, GTable* self, a_int key);
intern Value const* ai_table_refs(a_henv env, GTable* self, a_lstr const* key);
intern Value const* ai_table_ref(a_henv env, GTable* self, Value key);
intern Value ai_table_getis(a_henv env, GTable* self, GStr* key);
intern Value ai_table_get(a_henv env, GTable* self, Value key);
intern void ai_table_set(a_henv env, GTable* self, Value key, Value value);

intern GMod* ai_mod_alloc(a_henv env, a_usize len);
intern void ai_mod_ready(a_henv env, GMod* self, VTable const* impl);
intern void ai_mod_emplace(a_henv env, GMod* self, GStr* key, Value value);
intern Value* ai_obj_vlookup_(a_henv env, VTable const* self, a_enum tm);

intern GMod* ai_mod_load(a_henv env, GModLoader* loader, GStr* name, a_bool load);
intern void ai_mod_cache(a_henv env, GModLoader* loader, GMod* mod);
intern void ai_mod_cache_mark(Global* g, ModCache* cache);
intern void ai_mod_clean(Global* g);

always_inline Value ai_obj_vlookup(a_henv env, Value val, a_enum tm) {
	if (!v_is_obj(val)) return v_of_nil();
	a_hobj obj = v_as_obj(val);
	VTable const* vtable = obj->_vtable;
	if (!(vtable->_flags & VTABLE_FLAG_VLOOKUP)) return v_of_nil();
	Value* v = ai_obj_vlookup_(env, vtable, tm);
	return v != null ? *v : v_of_nil();
}

typedef struct TNode TNode;

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

struct ModCache {
	GMod** _table;
	a_u32 _hmask;
	a_u32 _len;
};

struct ModLoader {
	GModLoader* _parent;
	ModCache _cache;
};

struct GModLoader {
	GOBJ_STRUCT_HEADER;
	ModLoader _body;
};

/**
 ** Module.
 */
struct alo_Mod {
	VTable _impl;
	union {
		struct {
			GTABLE_STRUCT_HEADER;
			a_u32 _mc; /* Modification counter. */
			a_u32 _rc; /* Reference counter. */
		};
		GTable _table;
	};
	GModLoader* _loader;
	GStr* _name;
	GMod* _next;
	TNode _data[];
};

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

always_inline a_bool g_is_mod(a_hobj v) {
	return g_test(v, T_MOD);
}

always_inline GMod* g_as_mod(a_hobj v) {
	assume(g_is_mod(v));
	return g_cast(GMod, v);
}

always_inline GMod* v_as_mod(Value v) {
	assume(v_is_mod(v), "not module.");
	return g_as_mod(v_as_obj(v));
}

#endif /* atable_h_ */
