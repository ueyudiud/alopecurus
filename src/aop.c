/**
 *@file aop.c
 */

#define aop_c_
#define ALO_LIB

#include "aop.h"

char const* const ai_op_names[] = {
	[OP__NONE] = null,
	[OP_ADD] = "add",
	[OP_SUB] = "sub",
	[OP_MUL] = "mul",
	[OP_DIV] = "div",
	[OP_MOD] = "mod",
	[OP_SHL] = "shl",
	[OP_SHR] = "shr",
	[OP_BIT_AND] = "band",
	[OP_BIT_OR] = "bor",
	[OP_BIT_XOR] = "bxor",
	[OP_AND] = "and",
	[OP_OR] = "or",
	[OP_EQ] = "eq",
	[OP_NE] = "ne",
	[OP_LT] = "lt",
	[OP_LE] = "le",
	[OP_GT] = "gt",
	[OP_GE] = "ge"
};