/**
 *@file ameta.h
 */

#ifndef ameta_h_
#define ameta_h_

#include "aobj.h"

intern a_bool ai_meta_hash(a_henv env, Value v, a_hash* ph);
intern a_bool ai_meta_equals(a_henv env, Value v1, Value v2, a_bool* pz);
intern a_bool ai_meta_get(a_henv env, Value v1, Value v2, Value* pv);
intern a_bool ai_meta_set(a_henv env, Value v1, Value v2, Value v3);
intern a_bool ai_meta_len(a_henv env, Value v, a_uint* pi);
intern a_bool ai_meta_str(a_henv env, Value v, GStr** ps);
intern a_bool ai_meta_unary(a_henv env, a_enum tm, Value v, Value* pv);
intern a_bool ai_meta_binary(a_henv env, a_enum tm, Value v1, Value v2, Value* pv);

#endif /* ameta_h_ */
