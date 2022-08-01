/**
 *@file astrx.h
 */

#ifndef astrx_h_
#define astrx_h_

#include "alex.h"

enum {
    STRX__NORMAL,

    STRX__EMPTY,
    
    STRX_KW__BEGIN = STRX__EMPTY,
#define KWDEF(id,name) STRX_KW_##id,
    KEYWORD_LIST(KWDEF)
#undef KWDEF
    STRX_KW__END,
    STRX_KW__FIRST = STRX_KW__BEGIN + 1,
    STRX_KW__LAST = STRX_KW__END - 1,

    STRX__MAX = STRX_KW__END,

#define KWLEN(id,name) + (sizeof(name) - 1)
	STRX_RESERVE_SPACE = STRX__MAX * sizeof(IStr)
			KEYWORD_LIST(KWLEN) /* Add keyword size. */
#undef KWLEN
};

#define strx_id(str) ((str)->_tnext >> 32)
#define strx_iskw(str) (strx_id(str) >= STRX_KW__FIRST && strx_id(str) <= STRX_KW__LAST)
#define strx_totk(str) (cast(a_i32, strx_id(str)) + TK_IF - STRX_KW_IF)

#define strx_id_set(str,tag) quiet((str)->_tnext = cast(a_u64, tag) << 32)

intern void ai_strx_open(a_henv env, void* blk, GStr** dst);

#endif /* astrx_h_ */
