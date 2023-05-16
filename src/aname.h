/**
 *@file aname.h
 */

#ifndef aname_h_
#define aname_h_

#include "adef.h"
#include "aop.h"

enum {
    NAME__EMPTY,
#define AI_SYM_PROLOGUE(g) NAME_##g##__FIRST, NAME_##g##__STUB1 = NAME_##g##__FIRST - 1,
#define AI_SYM_EPILOGUE(g) NAME_##g##__STUB2, NAME_##g##__LAST = NAME_##g##__STUB2 - 1,
#define AI_SYM(g,i,n) NAME_##g##_##i,
# include "asym/kw.h"
# include "asym/tm.h"
# include "asym/pt.h"
#undef AI_SYM_PROLOGUE
#undef AI_SYM_EPILOGUE
#undef AI_SYM

	NAME__COUNT
};

#define name_id(str) ((str)->_tnext >> 16)
#define name_iskw(str) (name_id(str) >= NAME_KW__FIRST && name_id(str) <= NAME_KW__LAST)
#define name_istm(str) (name_id(str) >= NAME_TM__FIRST && name_id(str) <= NAME_TM__LAST)
#define name_totk(str) (cast(a_i32, name_id(str)) + TK_IF - NAME_KW_IF)
#define name_totm(str) (cast(a_i32, name_id(str)) + TM_GET - NAME_TM_GET)

#define name_id_set(str,tag) quiet((str)->_tnext = cast(a_u64, tag) << 16)

enum {
#define AI_SYM(g,i,n) NAME_POS_##g##_##i, NAME_EPOS_##g##_##i = NAME_POS_##g##_##i + sizeof(n) - 1,
# include "asym/kw.h"
# include "asym/tm.h"
# include "asym/pt.h"
#undef AI_SYM
	NAME_LEN,

	NAME__DUMMY = ISIZE_MAX /* Pad enumeration to a_isize type. */
};

#define name_ptr(p) (&ai_obj_names[p])

#endif /* aname_h_ */
