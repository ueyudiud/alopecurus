/**
 *@file astrx.h
 */

#ifndef astrx_h_
#define astrx_h_

#include "akw.h"
#include "aop.h"
#include "astr.h"

enum {
    STRX__NORMAL,

    STRX__EMPTY,
    
    STRX_KW__BEGIN = STRX__EMPTY,
#define KWDEF(id,name) M_cat(STRX_KW_,id),
    KEYWORD_LIST(KWDEF)
#undef KWDEF
    STRX_KW__END,
    STRX_KW__FIRST = STRX_KW__BEGIN + 1,
    STRX_KW__LAST = STRX_KW__END - 1,

	STRX_TM__BEGIN = STRX_KW__LAST,
#define TMDEF(id,name) M_cat(STRX_TM_,id),
	TM_LIST(TMDEF)
#undef TMDEF
	STRX_TM__END,
	STRX_TM__FIRST = STRX_TM__BEGIN + 1,
	STRX_TM__LAST = STRX_TM__END - 1,

    STRX__END = STRX_TM__END
};

#define strx_id(str) ((str)->_tnext >> 32)
#define strx_iskw(str) (strx_id(str) >= STRX_KW__FIRST && strx_id(str) <= STRX_KW__LAST)
#define strx_istm(str) (strx_id(str) >= STRX_TM__FIRST && strx_id(str) <= STRX_TM__LAST)
#define strx_totk(str) (cast(a_i32, strx_id(str)) + TK_IF - STRX_KW_IF)
#define strx_totm(str) (cast(a_i32, strx_id(str)) + TM_GET - STRX_TM_GET)

#define strx_id_set(str,tag) quiet((str)->_tnext = cast(a_u64, tag) << 32)

intern void ai_strx_boost(a_henv env, void* blk, GStr** dst);

enum {
#define KWPOS(id,name) STRX_POS_KW_##id, STRX_EPOS_KW_##id = STRX_POS_KW_##id + sizeof(name) - 1,
	KEYWORD_LIST(KWPOS)
#undef KWPOS
#define TMPOS(id,name) STRX_POS_TM_##id, STRX_EPOS_TM_##id = STRX_POS_TM_##id + sizeof(name) - 1,
	TM_LIST(TMPOS)
#undef TMPOS
	STRX_POS__MAX,

	STRX__DUMMY = ISIZE_MAX /* Pad enumeration to a_isize type. */
};

intern char const ai_strx_table[STRX_POS__MAX];

#define strx_raw(p) (&ai_strx_table[p])
#define strx_raw_kw(id) strx_raw(STRX_POS_KW_##id)
#define strx_raw_tm(id) strx_raw(STRX_POS_TM_##id)

__attribute__((__pure__))
always_inline a_usize ai_strx_reserve_space(void) {
	a_usize len = pad_to(istr_size(0), sizeof(a_usize)); /* For empty string. */
#define KWSIZE(id,name) len += pad_to(istr_size(sizeof(name) - 1), sizeof(a_usize));
	KEYWORD_LIST(KWSIZE)
#undef KWSIZE
#define TMSIZE(id,name) len += pad_to(istr_size(sizeof(name) - 1), sizeof(a_usize));
	TM_LIST(TMSIZE)
#undef TMSIZE
	return len;
}

#define STRX_RESERVE_SPACE ai_strx_reserve_space()

#endif /* astrx_h_ */
