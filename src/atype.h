/**
 *@file atype.h
 */

#ifndef atype_h_
#define atype_h_

#include "aobj.h"

intern GType* ai_ctype_alloc(a_henv env, a_usize len);
intern void ai_type_ready(a_henv env, GType* self);
intern void ai_type_setis(a_henv env, GType* self, GStr* key, Value value);
intern Value ai_type_getis(a_henv env, GType* self, GStr* key);

intern GType* ai_type_load(a_henv env, GLoader* loader, GStr* name, a_bool load);
intern void ai_type_cache(a_henv env, GLoader* loader, GType* type);
intern void ai_type_cache_mark(Global* g, TypeCache* cache);
intern void ai_type_clean(Global* g);

always_inline Value ai_obj_vlooktm(a_henv env, Value v, a_enum tm) {
	GType* type = v_typeof(env, v);
	GStr* key = env_name(env, NAME_KW__BEGIN + tm);
	return ai_type_getis(env, type, key);
}

always_inline Value ai_obj_vlookftm(a_henv env, Value v, a_enum tm) {
	assume(tm <= TM__FAST_MAX, "cannot fast access.");
	GType* type = v_typeof(env, v);
	if (!type_has_method(type, tm)) return v_of_nil();
	GStr* key = env_name(env, NAME_KW__BEGIN + tm);
	return ai_type_getis(env, type, key);
}

#endif /* atype_h_ */
