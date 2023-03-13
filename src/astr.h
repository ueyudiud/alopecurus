/**
 *@file astr.h
 */

#ifndef astr_h_
#define astr_h_

#include "aio.h"
#include "aobj.h"

#ifndef ALOI_INIT_SHTSTR_TABLE_CAPACITY
# define ALOI_INIT_SHTSTR_TABLE_CAPACITY 64
#endif

typedef struct IStr IStr;

intern a_hash ai_str_hashof(a_hash seed, void const* src, a_usize len);
intern GStr* ai_str_intern(a_henv env, void* blk, char const* src, a_usize len, a_u32 tag);
intern GStr* ai_str_new(a_henv env, void const* src, a_usize len);
intern a_msg ai_str_load(a_henv env, ZIn* in, a_usize len, GStr** pstr);
intern GStr* ai_str_format(a_henv env, char const* fmt, va_list varg);
intern a_bool ai_str_requals(GStr* self, void const* dat, a_usize len);
intern a_bool ai_str_equals(GStr* self, GStr* other);
intern void ai_str_boost(a_henv env);
intern void ai_str_clean(Global* g);

#define ai_str_newl(env,src) ai_str_new(env, ""src, sizeof(src) - sizeof(char))

#define GSTR_STRUCT_HEADER \
	GOBJ_STRUCT_HEADER;       \
	a_u32 _len;               \
	a_hash _hash;             \
	a_byte _data[]

struct GStr {
	GSTR_STRUCT_HEADER;
};

static_assert(offsetof(GObj, _len) == offsetof(GStr, _len));

struct IStr {
	IStr* _cache_next; /* Next node of overflow chain in intern table. */
	union {
		GStr _body;
		struct {
			GSTR_STRUCT_HEADER;
		};
	};
};

#define ISTR_MAX_LEN 255

always_inline a_bool g_is_istr(a_hobj v) {
	return g_test(v, T_ISTR);
}

always_inline a_bool g_is_str(a_hobj v) {
	return v->_vtable->_repr_id == REPR_STR;
}

always_inline GStr* g_as_str(a_hobj v) {
	assume(g_is_str(v));
	return g_cast(GStr, v);
}

always_inline GStr* v_as_str(Value v) {
	assume(v_is_str(v), "not string.");
	return g_as_str(v_as_obj(v));
}

always_inline a_usize hstr_size(a_usize len) {
	return sizeof(GStr) + len + 1;
}

always_inline a_usize istr_size(a_usize len) {
	return sizeof(IStr) + len + 1;
}

always_inline char const* str2ntstr(GStr* self) {
	return cast(char const*, self->_data);
}

#endif /* astr_h_ */
