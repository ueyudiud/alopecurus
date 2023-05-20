/**
 *@file astrs.h
 */

#ifndef aname_h_
#define aname_h_

#include "adef.h"
#include "aop.h"

enum {
    STR_EMPTY,
#define PROLOGUE(g) STR_##g##__FIRST, STR_##g##__STUB1 = STR_##g##__FIRST - 1,
#define EPILOGUE(g) STR_##g##__STUB2, STR_##g##__LAST = STR_##g##__STUB2 - 1,
#define STRDEF(n) STR_##n,
# include "asym/kw.h"
# include "asym/tm.h"
# include "asym/pt.h"
#undef PROLOGUE
#undef EPILOGUE
#undef STRDEF
#undef STRDEF2

	STR__COUNT
};

#define str_id(str) (cast(a_usize, (str)->_gnext) >> 48)
#define str_iskw(str) (str_id(str) >= STR_KW__FIRST && str_id(str) <= STR_KW__LAST)
#define str_istm(str) (str_id(str) >= STR_TM__FIRST && str_id(str) <= STR_TM__LAST)
#define str_totk(str) (cast(a_i32, str_id(str)) - STR_KW__FIRST + TK__FIRST)
#define str_totm(str) (cast(a_i32, str_id(str)) - STR_TM__FIRST)

#define str_id_set(str,id) quiet(*cast(a_usize*, &(str)->_gnext) = cast(a_usize, id) << 48)

enum {
	STR_POS_EMPTY,
#define EPILOGUE(g) STR_EPOS_##g, STR_EPOS_##g##__STUB2 = STR_EPOS_##g - 1,
#define STRDEF(n) STR_POS_##n, STR_EPOS_##n = STR_POS_##n + sizeof(#n) - 1,
#define STRDEF2(n,r) STR_POS_##n, STR_EPOS_##n = STR_POS_##n + sizeof(r) - 1,
# include "asym/kw.h"
# include "asym/tm.h"
# include "asym/pt.h"
# include "asym/op.h"
#undef EPILOGUE
#undef STRDEF
#undef STRDEF2
	STR_LEN,

	STR_POS__DUMMY = ISIZE_MAX /* Pad enumeration to a_isize type. */
};

#endif /* aname_h_ */
