/**
 *@file aobj.c
 */

#define aobj_c_
#define ALO_LIB

#include "aobj.h"

char const ai_obj_typenames[][8] = {
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
	[T_MOD] = "mod",
	[T_USER_TEQ] = "user",
	[T_USER_NEQ] = "user",
	[T_NAN] = "float",
	[T_FLOAT] = "float"
};