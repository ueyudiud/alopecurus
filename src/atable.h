/**
 *@file atable.h
 */

#ifndef atable_h_
#define atable_h_

#include "aobj.h"
#include "agc.h"

intern GTable* ai_table_new(a_henv env);
intern void ai_table_grow(a_henv env, GTable* self, a_usize add);
intern void ai_table_hint(a_henv env, GTable* self, a_usize add);
intern a_bool ai_table_get(a_henv env, GTable* self, Value vk, Value* pv);
intern a_bool ai_table_geti(a_henv env, GTable* self, a_int k, Value* pv);
intern a_bool ai_table_gets(a_henv env, GTable* self, GStr* k, Value* pv);
intern a_bool ai_table_set(a_henv env, GTable* self, Value vk, Value vv);
intern Value* ai_table_refls(a_henv env, GTable* self, char const* ptr, a_usize len);
intern a_bool ai_table_del(a_henv env, GTable* self, Value vk);
intern a_msg ai_table_uset(a_henv env, GTable* self, Value vk, Value vv);
intern void ai_table_mark(Global* gbl, GTable* self);
intern void ai_table_clean(Global* gbl, GTable* self);

#endif /* atable_h_ */
