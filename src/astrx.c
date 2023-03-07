/**
 *@file astrx.c
 */

#define astrx_c_
#define ALO_LIB

#include <string.h>

#include "astr.h"
#include "astrx.h"

char const ai_strx_table[STRX_POS__MAX] = {
#define KWSTR(id, name) name"\0"
	KEYWORD_LIST(KWSTR)
#undef KWSTR
#define TMSTR(id, name) name"\0"
	TM_LIST(TMSTR)
#undef TMSTR
};

static void* l_new_str(a_henv env, void* blk, GStr** dst, char const* src, a_u32 tag) {
	a_usize len = strlen(src);
	dst[tag] = ai_str_intern(env, blk, src, len, tag);
	return blk + pad_to(istr_size(len), sizeof(a_usize));
}

void ai_strx_boost(a_henv env, void* blk, GStr** dst) {
	/* Intern empty string. */
	blk = l_new_str(env, blk, dst, "", STRX__EMPTY);

	/* Intern keywords. */
#define KWINT(id,name) blk = l_new_str(env, blk, dst, strx_raw_kw(id), STRX_KW_##id);
	KEYWORD_LIST(KWINT)
#undef KWINT
#define TMINT(id,name) blk = l_new_str(env, blk, dst, strx_raw_tm(id), STRX_TM_##id);
	TM_LIST(TMINT)
#undef TMINT
}
