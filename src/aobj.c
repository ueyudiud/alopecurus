/**
 *@file aobj.c
 */

#define aobj_c_
#define ALO_LIB

#include "astr.h"
#include "atype.h"

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
	[T_AUSER] = "user",
	[T_CUSER] = "user",
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
	Global* g = G(env);

	GStr** dst = g->_names;
	/* Intern empty string. */
	blk = l_intern(env, blk, dst, new(a_lstr) {null, 0}, NAME__EMPTY);

	/* Intern keywords. */
#define KWINT(id,name) blk = l_intern(env, blk, dst, name_str_kw(id), NAME_KW_##id);
	KW_LIST(KWINT)
#undef KWINT
#define TMINT(id,name) blk = l_intern(env, blk, dst, name_str_tm(id), NAME_TM_##id);
	TM_LIST(TMINT)
#undef TMINT

	ai_type_init(env, &g->_types._nil, ALO_TNIL, NAME_KW_NIL);
	ai_type_init(env, &g->_types._bool, ALO_TBOOL, NAME__NORMAL);
	ai_type_init(env, &g->_types._int, ALO_TINT, NAME__NORMAL);
	ai_type_init(env, &g->_types._float, ALO_TFLOAT, NAME__NORMAL);
	ai_type_init(env, &g->_types._ptr, ALO_TPTR, NAME__NORMAL);
	ai_type_init(env, &g->_types._str, ALO_TSTR, NAME__NORMAL);
	ai_type_init(env, &g->_types._list, ALO_TLIST, NAME__NORMAL);
	ai_type_init(env, &g->_types._table, ALO_TTABLE, NAME__NORMAL);
	ai_type_init(env, &g->_types._route, ALO_TROUTE, NAME__NORMAL);
	ai_type_init(env, &g->_types._func, ALO_TFUNC, NAME__NORMAL);
	ai_type_init(env, &g->_types._type, ALO_TTYPE, NAME__NORMAL);
	ai_type_init(env, &g->_types._buf, ALO_TUSER, NAME__NORMAL);
}