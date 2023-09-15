/**
 *@file amod.h
 */

#ifndef amod_h_
#define amod_h_

#include "aobj.h"

intern GMod* ai_mod_alloc(a_henv env, a_usize len, a_vptr proto);
intern void ai_mod_init(a_henv env, GMod* self, a_tag tag, a_u32 nid);
intern GMod* ai_amod_new(a_henv env);
intern void ai_mod_hint(a_henv env, GMod* self, a_usize len);
intern Value const* ai_mod_refls(a_henv env, GMod* self, char const* src, a_usize len);
intern Value ai_mod_get(a_henv env, GMod* self, Value key);
intern Value ai_mod_gets(a_henv env, GMod* self, GStr* key);
intern void ai_mod_set(a_henv env, GMod* self, Value key, Value value);
intern void ai_mod_sets(a_henv env, GMod* self, GStr* key, Value value);

intern GMod* ai_mod_load(a_henv env, GLoader* loader, GStr* name, a_bool load);
intern void ai_mod_cache(a_henv env, GLoader* loader, GMod* type);
intern void ai_mod_cache_mark(Global* g, ModCache* cache);
intern void ai_mod_boost(a_henv env);
intern void ai_mod_clean(Global* g);

always_inline Value ai_obj_vlooktm(a_henv env, Value v, a_enum tm) {
	GMod* type = v_typeof(env, v);
	GStr* key = env_int_str(env, STR_TM__FIRST + tm);
	return ai_mod_gets(env, type, key);
}

always_inline Value ai_obj_glookftm(a_henv env, a_hobj p, a_enum tm) {
	assume(tm <= TM__FAST_MAX, "cannot fast access.");
	GMod* type = g_typeof(env, p);
	if (!mod_has_method(type, tm)) return v_of_nil();
	GStr* key = env_int_str(env, STR_TM__FIRST + tm);
	return ai_mod_gets(env, type, key);
}

#define ai_obj_glookftm(env,p,tm) ai_obj_glookftm(env, gobj_cast(p), tm)

always_inline Value ai_obj_vlookftm(a_henv env, Value v, a_enum tm) {
	assume(tm <= TM__FAST_MAX, "cannot fast access.");
	GMod* type = v_typeof(env, v);
	if (!mod_has_method(type, tm)) return v_of_nil();
	GStr* key = env_int_str(env, STR_TM__FIRST + tm);
	return ai_mod_gets(env, type, key);
}

#endif /* amod_h_ */
