/**
 *@file astrs.h
 */

#ifndef astrs_h_
#define astrs_h_

#include "adef.h"
#include "aop.h"
#include "asym.h"

enum {
#define SYMLIST SYMLIST_ISTRS
#define PROLOGUE(g) STR_##g##__FIRST, STR_##g##__STUB1 = STR_##g##__FIRST - 1,
#define EPILOGUE(g) STR_##g##__STUB2, STR_##g##__LAST = STR_##g##__STUB2 - 1,
#define SYMDEF(n,r) STR_##n,
#include "asym.h"
	STR__COUNT
};

#define str_id(str) (bit_cast(a_usize, (str)->gnext) >> 48)
#define str_iskw(str) (str_id(str) >= STR_KW__FIRST && str_id(str) <= STR_KW__LAST)
#define str_istm(str) (str_id(str) >= STR_TM__FIRST && str_id(str) <= STR_TM__LAST)
#define str_totk(str) (cast(a_i32, str_id(str)) - STR_KW__FIRST + TK__FIRST)
#define str_totm(str) (cast(a_i32, str_id(str)) - STR_TM__FIRST)

#define str_id_set(str,id) quiet((str)->gnext = bit_cast(a_gcnext, cast(a_usize, id) << 48))

enum {
#define SYMLIST SYMLIST_SSTRS
#define SYMDEF(n,r) STR_POS_##n, STR_EPOS_##n = STR_POS_##n + sizeof(r) - 1,
#include "asym.h"
    STR__TOTAL_LEN,

    STR_POS__DUMMY = ISIZE_MAX /* Pad enumeration to a_isize type. */
};

#endif /* astrs_h_ */
