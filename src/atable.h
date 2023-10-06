/**
 *@file atable.h
 */

#ifndef atable_h_
#define atable_h_

#include "aobj.h"
#include "agc.h"

intern GTable* ai_table_new(a_henv env);
intern void ai_table_hint(a_henv env, GTable* self, a_usize len);
intern Value ai_table_get(a_henv env, GTable* self, Value vk);
intern Value ai_table_gets(a_henv env, GTable* self, GStr* k);
intern void ai_table_set(a_henv env, GTable* self, Value vk, Value vv);
intern void ai_table_put(a_henv env, GTable* self, Value vk, a_hash hash, Value vv);
intern a_bool ai_table_del(a_henv env, GTable* self, Value vk);
intern a_msg ai_table_ugeti(a_henv env, GTable* self, a_int k, Value* pv);
intern a_msg ai_table_uget(a_henv env, GTable* self, Value vk, Value* pv);
intern a_msg ai_table_uset(a_henv env, GTable* self, Value vk, Value vv);



#endif /* atable_h_ */
