/**
 *@file astr.h
 */

#ifndef astr_h_
#define astr_h_

#include "aobj.h"

#ifndef ALOI_INIT_SHTSTR_TABLE_CAPACITY
# define ALOI_INIT_SHTSTR_TABLE_CAPACITY 64
#endif

#ifndef ALOI_SHTSTR_THRESHOLD
# define ALOI_SHTSTR_THRESHOLD 40
#endif

typedef struct IStr IStr;

struct GStr {
	GOBJ_STRUCT_HEADER;
	a_u32 _len;
	a_hash _hash;
	a_byte _data[];
};

struct IStr {
    IStr* _next; /* Next node of overflow chain in intern table. */
    GStr _body;
};

intern a_hash ai_str_hashof(a_hash seed, void const* src, a_usize len);
intern GStr* ai_str_intern(a_henv env, void* blk, char const* src, a_usize len, a_u32 tag);
intern GStr* ai_str_new(a_henv env, void const* src, a_usize len, a_hash hash);
intern GStr* ai_istr_create(a_henv env, void const* src, a_usize len);
intern GStr* ai_str_create(a_henv env, void const* src, a_usize len);
intern GStr* ai_str_format(a_henv env, char const* fmt, ...);
intern GStr* ai_str_formatv(a_henv env, char const* fmt, va_list varg);
intern a_bool ai_str_requals(GStr* self, void const* dat, a_usize len);
intern a_bool ai_str_equals(GStr* self, GStr* other);
intern void ai_str_boost(a_henv env);
intern void ai_str_clean(Global* g);

#define ai_str_createl(env,src) ai_str_create(env, ""src, sizeof(src) - sizeof(char))

intern VTable const ai_dstr_vtable;
intern VTable const ai_istr_vtable;
intern VTable const ai_hstr_vtable;

#endif /* astr_h_ */
