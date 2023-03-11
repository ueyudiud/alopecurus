/**
 *@file abc.c
 */

#define abc_c_
#define ALO_LIB

#include "abc.h"

char const* const ai_bc_names[BC__MAX] = {
#define BCNAME(id,n,f,a,b,c) n,
	ALO_BC_LIST(BCNAME)
#undef BCNAME
};

a_u8 const ai_bc_formats[] = {
#define BCFMT(id,n,f,a,b,c) M_cat(INSN_,f),
	ALO_BC_LIST(BCFMT)
#undef BCFMT
};