/**
 *@file ameta.h
 */

#ifndef ameta_h_
#define ameta_h_

#include "aobj.h"

intern GMeta* ai_meta_alloc(a_henv env, a_usize size, VTable const* vptr);
intern GMeta* ai_mod_new(a_henv env, GStr* name, GLoader* loader);

intern GMeta* ai_meta_look(a_henv env, GLoader* loader, GStr* name, a_bool load);
intern void ai_meta_cache(a_henv env, GLoader* loader, GMeta* meta);
intern void ai_meta_cache_mark(Global* g, MetaCache* cache);
intern void ai_meta_boost(a_henv env);
intern void ai_meta_clean(Global* g);

intern Value ai_meta_get(a_henv env, GMeta* self, Value vk);
intern void ai_meta_set(a_henv env, GMeta* self, Value vk, Value vv);
intern a_msg ai_meta_uget(a_henv env, GMeta* self, Value vk, Value* pv);
intern a_msg ai_meta_ugets(a_henv env, GMeta* self, GStr* k, Value* pv);
intern a_msg ai_meta_uset(a_henv env, GMeta* self, Value vk, Value vv);
intern a_msg ai_meta_usets(a_henv env, GMeta* self, GStr* k, Value vv);

always_inline Value ai_obj_vlooktm(a_henv env, Value v, a_enum tm) {
	GType* type = v_typeof(env, v);
	GStr* key = env_int_str(env, STR_TM__FIRST + tm);
	return ai_meta_get(env, g_cast(GMeta, type), v_of_obj(key));
}

always_inline Value ai_obj_glookftm(a_henv env, a_hobj p, a_enum tm) {
	assume(tm <= TM__FAST_MAX, "cannot fast lookup.");
	GType* type = g_typeof(env, p);
	if (!meta_has_tm(type, tm)) return v_of_nil();
	GStr* key = env_int_str(env, STR_TM__FIRST + tm);
    return ai_meta_get(env, g_cast(GMeta, type), v_of_obj(key));
}

#define ai_obj_glookftm(env,p,tm) ai_obj_glookftm(env, gobj_cast(p), tm)

always_inline Value ai_obj_vlookftm(a_henv env, Value v, a_enum tm) {
	assume(tm <= TM__FAST_MAX, "cannot fast lookup.");
	GType* type = v_typeof(env, v);
	if (!meta_has_tm(type, tm)) return v_of_nil();
	GStr* key = env_int_str(env, STR_TM__FIRST + tm);
    return ai_meta_get(env, g_cast(GMeta, type), v_of_obj(key));
}

#endif /* ameta_h_ */
