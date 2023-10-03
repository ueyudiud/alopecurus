/**
 *@file adict.h
 */

#ifndef adict_h_
#define adict_h_

#include "aobj.h"

intern void ai_dict_init(a_henv env, Dict* self);
intern void ai_dict_deinit(Global* g, Dict* self);
intern void ai_dict_mark(Global* g, Dict* self);
intern void ai_dict_hint(a_henv env, Dict* self, a_usize len);
intern a_msg ai_dict_uget(a_henv env, Dict* self, GStr* key, Value* pval);
intern a_msg ai_dict_uset(a_henv env, Dict* self, GStr* key, Value val, a_usize* pctx);
intern a_msg ai_dict_uput(a_henv env, Dict* self, GStr* key, Value val, a_usize* pctx);
intern void ai_dict_put_inplace(a_henv env, Dict* self, GStr* key, Value val);

#endif /* adict_h_ */
