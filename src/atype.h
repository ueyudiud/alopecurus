/**
 *@file atype.h
 */

#ifndef atype_h_
#define atype_h_

#include "aobj.h"

#define TYPE_META_CAT_STATIC 0
#define TYPE_META_CAT_GETTER 1
#define TYPE_META_CAT_SETTER 2
#define TYPE_META_CAT_MEMBER 3

intern GType* ai_type_alloc(a_henv env, a_usize size);
intern GType* ai_stype_new(a_henv env, GStr* name, GLoader* loader);

intern Value ai_type_get(a_henv env, GType* self, Value vk);
intern void ai_type_set(a_henv env, GType* self, Value vk, Value vv);

intern a_msg ai_type_uget(a_henv env, GType* self, Value vk, Value* pv);
intern a_msg ai_type_ugets(a_henv env, GType* self, GStr* k, Value* pv);
intern a_msg ai_type_uset(a_henv env, GType* self, Value vk, Value vv);
intern a_msg ai_type_usets(a_henv env, GType* self, GStr* k, Value vv);

intern GType* ai_type_look(a_henv env, GLoader* loader, GStr* name, a_bool load);
intern void ai_type_cache(a_henv env, GLoader* loader, GType* type);
intern void ai_type_cache_mark(Global* g, TypeCache* cache);
intern void ai_type_boost(a_henv env);
intern void ai_type_clean(Global* g);

intern a_msg ai_obj_ulook(a_henv env, Value v, GStr* k, Value* pv);

always_inline a_msg ai_obj_ulooktm(a_henv env, Value v, a_enum tm, Value* pv) {
	GStr* k = g_str(env, STR_TM__FIRST + tm);
	return ai_obj_ulook(env, v, k, pv);
}

always_inline Value ai_obj_glookftm(a_henv env, a_hobj p, a_enum tm) {
	assume(tm <= TM__FAST_MAX, "cannot fast lookup.");
	GType* type = g_typeof(env, p);
	if (!type_has_ftm(type, tm)) return v_of_nil();
	GStr* key = g_str(env, STR_TM__FIRST + tm);
    return ai_type_get(env, type, v_of_obj(key));
}

#define ai_obj_glookftm(env,p,tm) ai_obj_glookftm(env, gobj_cast(p), tm)

always_inline Value ai_obj_vlookftm(a_henv env, Value v, a_enum tm) {
	assume(tm <= TM__FAST_MAX, "cannot fast lookup.");
	GType* type = v_typeof(env, v);
	if (!type_has_ftm(type, tm)) return v_of_nil();
	GStr* key = g_str(env, STR_TM__FIRST + tm);
    return ai_type_get(env, type, v_of_obj(key));
}

#endif /* atype_h_ */
