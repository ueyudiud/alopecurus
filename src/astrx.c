/**
 *@file astrx.c
 */

#define astrx_c_

#include "astr.h"
#include "astrx.h"

enum {
#define KWPOS(id,name) STRX_POS_KW_##id, STRX_EPOS_KW_##id = STRX_POS_KW_##id + sizeof(name) - 2,
	KEYWORD_LIST(KWPOS)
#undef KWPOS
	STRX_POS__MAX,

	STRX__DUMMY = INTPTR_MAX /* Pad enumeration to a_isize type. */
};

void ai_strx_open(a_henv env, void* blk, GStr** dst) {
	static char const _strs[STRX_POS__MAX] = {
			""
#define KWSTR(id, name) name
			KEYWORD_LIST(KWSTR)
#undef KWSTR
	};

	/* Intern empty string. */
	dst[STRX__EMPTY - 1] = ai_str_intern(env, blk, _strs, 0, STRX__EMPTY);
	blk += sizeof(IStr);

	/* Intern keywords. */
#define KWINT(id,name) \
	dst[STRX_KW_##id - 1] = ai_str_intern(env, blk, _strs + STRX_POS_KW_##id, sizeof(name) - 1, STRX_KW_##id); \
	blk += sizeof(IStr) + sizeof(name) - 1;
	KEYWORD_LIST(KWINT)
#undef KWINT
}
