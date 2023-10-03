/**
 *@file atable.h
 */

#ifndef atable_h_
#define atable_h_

#include "aobj.h"
#include "agc.h"

intern GTable* ai_table_new(a_henv env);
intern void ai_table_hint(a_henv env, GTable* self, a_usize len);
intern Value* ai_table_ref(a_henv env, GTable* self, Value key, a_u32* restrict phash);
intern Value const* ai_table_refi(a_henv env, GTable* self, a_int key);
intern Value const* ai_table_refls(a_henv env, GTable* self, a_lstr const* key);
intern Value const* ai_table_refs(a_henv env, GTable* self, GStr* key);
intern Value ai_table_get(a_henv env, GTable* self, Value key);
intern Value ai_table_gets(a_henv env, GTable* self, GStr* key);
intern void ai_table_set(a_henv env, GTable* self, Value key, Value value);
intern void ai_table_put(a_henv env, GTable* self, Value key, a_hash hash, Value value);
intern void ai_table_drop(Global* g, GTable* self);
intern void ai_table_mark(Global* g, GTable* self);

inline a_msg ai_table_ugeti(a_henv env, GTable* self, a_int key, Value* pval) {
    Value const* psrc = ai_table_refi(env, self, key);
    if (psrc == null) return ALO_EEMPTY;
    v_cpy(env, pval, psrc);
    return ALO_SOK;
}

inline a_msg ai_table_uget(a_henv env, GTable* self, Value key, Value* pval) {
    a_hash hash;
    Value* psrc = ai_table_ref(env, self, key, &hash);
    if (psrc == null) return ALO_EEMPTY;
    v_cpy(env, pval, psrc);
    return ALO_SOK;
}

inline a_msg ai_table_uset(a_henv env, GTable* self, Value key, Value val) {
    a_hash hash;
    Value* pdst = ai_table_ref(env, self, key, &hash);
    
    if (pdst == null) {
        ai_table_put(env, self, key, hash, val);
        return ALO_SOK;
    }
    
    v_set(env, pdst, val);
    ai_gc_barrier_backward_val(env, self, val);
    return ALO_SOK;
}

#endif /* atable_h_ */
