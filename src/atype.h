/**
 *@file atype.h
 */

#ifndef atype_h_
#define atype_h_

#include "aobj.h"

intern GType* ai_type_alloc(a_henv env, a_usize len, a_vptr proto);
intern void ai_type_init(a_henv env, GType* self, a_tag tag, a_u32 nid);
intern GType* ai_atype_new(a_henv env);
intern void ai_type_hint(a_henv env, GType* self, a_usize len);
intern void ai_type_set(a_henv env, GType* self, Value key, Value value);
intern void ai_type_setis(a_henv env, GType* self, GStr* key, Value value);
intern Value ai_type_get(a_henv env, GType* self, Value key);
intern Value ai_type_getis(a_henv env, GType* self, GStr* key);

intern GType* ai_type_load(a_henv env, GLoader* loader, GStr* name, a_bool load);
intern void ai_type_cache(a_henv env, GLoader* loader, GType* type);
intern void ai_type_cache_mark(Global* g, TypeCache* cache);
intern void ai_type_clean(Global* g);

always_inline Value ai_obj_vlooktm(a_henv env, Value v, a_enum tm) {
	GType* type = v_typeof(env, v);
	GStr* key = env_name(env, NAME_TM__FIRST + tm);
	return ai_type_getis(env, type, key);
}

always_inline Value ai_obj_glookftm(a_henv env, a_hobj p, a_enum tm) {
	assume(tm <= TM__FAST_MAX, "cannot fast access.");
	GType* type = g_typeof(env, p);
	if (!type_has_method(type, tm)) return v_of_nil();
	GStr* key = env_name(env, NAME_TM__FIRST + tm);
	return ai_type_getis(env, type, key);
}

#define ai_obj_glookftm(env,p,tm) ai_obj_glookftm(env, gobj_cast(p), tm)

always_inline Value ai_obj_vlookftm(a_henv env, Value v, a_enum tm) {
	assume(tm <= TM__FAST_MAX, "cannot fast access.");
	GType* type = v_typeof(env, v);
	if (!type_has_method(type, tm)) return v_of_nil();
	GStr* key = env_name(env, NAME_TM__FIRST + tm);
	return ai_type_getis(env, type, key);
}

#endif /* atype_h_ */
