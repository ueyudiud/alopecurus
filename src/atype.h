/**
 *@file atype.h
 */

#ifndef atype_h_
#define atype_h_

#include "aobj.h"

intern GType* ai_type_new(a_henv env, GStr* name, GLoader* loader, a_usize extra);
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

#endif /* atype_h_ */
