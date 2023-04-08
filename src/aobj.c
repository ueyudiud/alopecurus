/**
 *@file aobj.c
 */

#define aobj_c_
#define ALO_LIB

#include "astr.h"

#include "aobj.h"

char const ai_obj_type_names[][8] = {
	[T_NIL] = "nil",
	[T_FALSE] = "bool",
	[T_TRUE] = "bool",
	[T_INT] = "int",
	[T_PTR] = "ptr",
	[T_ISTR] = "str",
	[T_HSTR] = "str",
	[T_TUPLE] = "tuple",
	[T_LIST] = "list",
	[T_TABLE] = "table",
	[T_FUNC] = "func",
	[T_TYPE] = "type",
	[T_USER_TEQ] = "user",
	[T_USER_NEQ] = "user",
	[T_NAN] = "float",
	[T_FLOAT] = "float"
};

char const ai_obj_names[NAME_POS__MAX] = {
#define STR(id,name) name"\0"
	KW_LIST(STR)
	TM_LIST(STR)
#undef TMSTR
};

static void* l_intern(a_henv env, void* blk, GStr** dst, a_lstr str, a_u32 tag) {
	dst[tag] = ai_str_intern(env, blk, str._ptr, str._len, tag);
	return blk + sizeof_IStr(str._len);
}

void ai_obj_boost(a_henv env, void* blk) {
	GStr** dst = G(env)->_names;
	/* Intern empty string. */
	blk = l_intern(env, blk, dst, new(a_lstr) {null, 0}, NAME__EMPTY);

	/* Intern keywords. */
#define KWINT(id,name) blk = l_intern(env, blk, dst, name_str_kw(id), NAME_KW_##id);
	KW_LIST(KWINT)
#undef KWINT
#define TMINT(id,name) blk = l_intern(env, blk, dst, name_str_tm(id), NAME_TM_##id);
	TM_LIST(TMINT)
#undef TMINT
}