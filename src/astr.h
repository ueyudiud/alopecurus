/**
 *@file astr.h
 */

#ifndef astr_h_
#define astr_h_

#include "aobj.h"
#include "aio.h"

#ifndef ALOI_INIT_SHTSTR_TABLE_CAPACITY
# define ALOI_INIT_SHTSTR_TABLE_CAPACITY 64
#endif

typedef a_msg (*a_sbfun)(void*, void*, a_usize);

intern a_hash ai_str_hashof(a_henv env, void const* src, a_usize len);
intern GStr* ai_str_new(a_henv env, void const* src, a_usize len);
intern a_msg ai_str_load(a_henv env, a_sbfun fun, a_usize len, void* ctx, GStr** pstr);
intern GStr* ai_str_format(a_henv env, char const* fmt, va_list varg);
intern a_bool ai_str_requals(GStr* self, void const* dat, a_usize len);
intern a_bool ai_str_equals(GStr* self, GStr* o);
intern void ai_str_boost(a_henv env, void* block);
intern void ai_str_clean(Global* g);

#define ai_str_newl(env,src) ai_str_new(env, ""src, sizeof(src) - sizeof(char))
#define ai_str_newc(env,src) ai_str_new(env, src, strlen(src))

intern char const ai_str_interns[STR__TOTAL_LEN];

#endif /* astr_h_ */
