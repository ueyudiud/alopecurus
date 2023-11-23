/**
 *@file afmt.h
 */

#ifndef afmt_h_
#define afmt_h_

#include "abuf.h"

intern a_usize ai_fmt_int2str(char* p, a_int v);
intern void ai_fmt_puti(a_henv env, Buf* buf, a_int v);
intern a_usize ai_fmt_float2str(char* p, a_float v);
intern void ai_fmt_putf(a_henv env, Buf* buf, a_float v);
intern a_usize ai_fmt_ptr2str(char* p, void* v);
intern void ai_fmt_putp(a_henv env, Buf* buf, void* v);

#define at_fmt_puti(env,b,v) ai_fmt_puti(env, buf_cast(b), v)
#define at_fmt_putf(env,b,v) ai_fmt_putf(env, buf_cast(b), v)
#define at_fmt_putp(env,b,v) ai_fmt_putp(env, buf_cast(b), v)

#endif /* afmt_h_ */
