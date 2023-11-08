/**
 *@file aobj.c
 */

#define aobj_c_
#define ALO_LIB

#include "aobj.h"

char const ai_obj_type_names[][8] = {
	[T_NIL] = "nil",
	[T_FALSE] = "bool",
	[T_TRUE] = "bool",
	[T_INT] = "int",
	[T_PTR] = "ptr",
	[T_STR] = "str",
	[T_TUPLE] = "tuple",
	[T_LIST] = "list",
	[T_TABLE] = "table",
	[T_FUNC] = "func",
	[T_TYPE] = "type",
	[T_USER] = "user",
	[T_NAN] = "float",
	[T_FLOAT] = "float"
};
