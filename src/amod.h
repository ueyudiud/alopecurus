/**
 *@file amod.h
 */

#ifndef amod_h_
#define amod_h_

#include "adict.h"
#include "aobj.h"

typedef struct GModLoader GModLoader;
typedef struct ModLoader ModLoader;
typedef struct ModCache ModCache;

intern GMod* ai_mod_new(a_henv env, GStr* name, GStr** pkey, a_usize len);
intern a_i32 ai_mod_find(a_henv env, GMod* self, GStr* key);

intern GMod* ai_mod_load(a_henv env, GModLoader* loader, GStr* name, a_bool load);
intern void ai_mod_cache(a_henv env, GModLoader* loader, GMod* mod);
intern void ai_mod_cache_splash(Global* g, ModCache* cache);
intern void ai_mod_clean(Global* g);

intern VTable const ai_mod_vtable;

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

struct alo_Mod {
	GOBJ_STRUCT_HEADER;
	Dict _table;
	a_u32 _mc; /* Modification counter. */
	a_u32 _rc; /* Reference counter. */
	GModLoader* _loader;
	GStr* _name;
	GMod* _next;
	Value _values[0];
};

#endif /* amod_h_ */
