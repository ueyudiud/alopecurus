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

intern a_usize ai_fmt_i2s(a_byte* p, a_int v);
intern a_usize ai_fmt_f2s(a_byte* p, a_float v);
intern a_msg ai_fmt_puti(a_henv env, void* buf, a_int v);
intern a_msg ai_fmt_putf(a_henv env, void* buf, a_float v);

#endif /* afmt_h_ */
