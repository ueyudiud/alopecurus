/**
 *@file abc.c
 */

#define abc_c_
#define ALO_LIB

#include "abc.h"

char const* const ai_bc_names[BC__MAX] = {
#define BCNAME(id,name,a,b,c) name,
	ALO_BC_LIST(BCNAME)
#undef BCNAME
};