/**
 *@file abc.c
 */

#define abc_c_
#define ALO_LIB

#include "abc.h"

char const ai_bc_names[BC__MAX][8] = {
#define BCNAME(id,n,...) n,
	ALO_BC_LIST(BCNAME)
#undef BCNAME
};
