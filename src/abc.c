/**
 *@file abc.c
 */

#define abc_c_
#define ALO_LIB

#include "abc.h"

char const* const ai_bc_names[BC__MAX] = {
#define BCNAME(id,n,...) n,
	ALO_BC_LIST(BCNAME)
#undef BCNAME
};

a_u8 const ai_bc_fmts[BC__MAX] = {
#define BCFMT(id,n,f,...) M_cat(INSN_,f),
	ALO_BC_LIST(BCFMT)
#undef BCFMT
};

a_u8 const ai_bc_ctrls[BC__MAX] = {
#define BCCTRL(id,n,f,c,...) M_cat(INSN_c,c),
	ALO_BC_LIST(BCCTRL)
#undef BCCTRL
};