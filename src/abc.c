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

a_u8 const ai_bc_reloc[BC__MAX] = {
#define BCRELOC(id,n,a,b,c,ex) (q(c)) << 2 | (1 - (q(b))) << 1 | (1 - (q(a))),
#define q(x) 0
#define reg _) + (1
#define jmp _) + (1 - 5
	ALO_BC_LIST(BCRELOC)
#undef reg
#undef q
#undef BCRELOC
};
