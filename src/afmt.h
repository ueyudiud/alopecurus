/*
 * afmt.h
 *
 *  Created on: 2022/8/11
 *      Author: ueyudiud
 */

#ifndef afmt_h_
#define afmt_h_

#include "abuf.h"
#include "astr.h"

intern a_usize ai_fmt_int2str(char* p, a_int v);
intern void ai_fmt_puti(a_henv env, void* buf, a_int v);
intern a_usize ai_fmt_float2str(char* p, a_float v);
intern void ai_fmt_putf(a_henv env, void* buf, a_float v);
intern a_usize ai_fmt_ptr2str(char* p, void* v);
intern void ai_fmt_putp(a_henv env, void* buf, void* v);

#endif /* afmt_h_ */
