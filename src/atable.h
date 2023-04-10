/**
 *@file atable.h
 */

#ifndef atable_h_
#define atable_h_

#include "aobj.h"

intern GTable* ai_table_new(a_henv env);
intern void ai_table_hint(a_henv env, GTable* self, a_usize len);
intern Value* ai_table_ref(a_henv env, GTable* self, Value key, a_u32* restrict phash);
intern Value const* ai_table_refi(a_henv env, GTable* self, a_int key);
intern Value const* ai_table_refs(a_henv env, GTable* self, a_lstr const* key);
intern Value const* ai_table_refis(a_henv env, GTable* self, GStr* key);
intern Value ai_table_getis(a_henv env, GTable* self, GStr* key);
intern Value ai_table_get(a_henv env, GTable* self, Value key);
intern void ai_table_set(a_henv env, GTable* self, Value key, Value value);
intern void ai_table_put(a_henv env, GTable* self, Value key, a_hash hash, Value value);
intern void ai_table_drop(Global* g, GTable* self);
intern void ai_table_mark(Global* g, GTable* self);

#endif /* atable_h_ */
