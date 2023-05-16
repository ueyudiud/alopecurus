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

char const ai_obj_names[NAME_LEN] = {
#define AI_SYM(g,i,n) n"\0"
# include "asym/kw.h"
# include "asym/tm.h"
# include "asym/pt.h"
#undef AI_SYM
};

void ai_obj_boost(a_henv env, void* blk) {
	Global* g = G(env);

	static a_u16 const l_name_disp_table[NAME__COUNT][2] = {
		{ 0, 0 }, /* Intern empty string. */
#define AI_SYM(g,i,d) { NAME_POS_##g##_##i, NAME_EPOS_##g##_##i - NAME_POS_##g##_##i },
# include "asym/kw.h"
# include "asym/tm.h"
# include "asym/pt.h"
#undef AI_SYM
	};

	for (a_u32 i = 0; i < NAME__COUNT; ++i) {
		a_u16 const* dat = l_name_disp_table[i];
		a_u32 len = dat[1];
		g->_names[i] = ai_str_intern(env, blk, name_ptr(dat[0]), len, i);
		blk += sizeof_IStr(len);
	}

	ai_type_init(env, &g->_types._nil, ALO_TNIL, NAME_KW_NIL);
	ai_type_init(env, &g->_types._bool, ALO_TBOOL, NAME_PT_BOOL);
	ai_type_init(env, &g->_types._int, ALO_TINT, NAME_PT_INT);
	ai_type_init(env, &g->_types._float, ALO_TFLOAT, NAME_PT_FLOAT);
	ai_type_init(env, &g->_types._ptr, ALO_TPTR, NAME_PT_PTR);
	ai_type_init(env, &g->_types._str, ALO_TSTR, NAME_PT_STR);
	ai_type_init(env, &g->_types._tuple, ALO_TTUPLE, NAME_PT_TUPLE);
	ai_type_init(env, &g->_types._list, ALO_TLIST, NAME_PT_LIST);
	ai_type_init(env, &g->_types._table, ALO_TTABLE, NAME_PT_TABLE);
	ai_type_init(env, &g->_types._route, ALO_TROUTE, NAME_PT_ROUTE);
	ai_type_init(env, &g->_types._func, ALO_TFUNC, NAME_PT_FUNC);
	ai_type_init(env, &g->_types._type, ALO_TTYPE, NAME_PT_TYPE);
}