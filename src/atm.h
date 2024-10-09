/**
 *@file atm.h
 */

#ifndef atm_h_
#define atm_h_

#include "aop.h"
#include "aobj.h"

intern a_bool ai_tm_hash(a_henv env, Value v, a_hash* ph);
intern a_bool ai_tm_equals(a_henv env, Value v1, Value v2, a_bool* pz);
intern a_bool ai_tm_get(a_henv env, Value v1, Value v2, Value* pv);
intern a_bool ai_tm_set(a_henv env, Value v1, Value v2, Value v3);
intern a_bool ai_tm_len(a_henv env, Value v, a_uint* pi);
intern a_bool ai_tm_look(a_henv env, Value v, GStr* k, Value* pv);
intern a_bool ai_tm_str(a_henv env, Value v, GStr** ps);
intern a_bool ai_tm_unary(a_henv env, a_enum tm, Value v, Value* pv);
intern a_bool ai_tm_binary(a_henv env, a_enum tm, Value v1, Value v2, Value* pv);
intern a_bool ai_tm_relation(a_henv env, a_enum tm, Value v1, Value v2, a_bool* pz);
intern a_bool ai_tm_precall(a_henv env, Value v, Value* pv);
intern void ai_tm_close(a_henv env, a_gptr o);

#endif /* atm_h_ */
