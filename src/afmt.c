/**
 *@file afmt.c
 */

#define afmt_c_
#define ALO_LIB

#include <stdio.h>

#include "afmt.h"

static char const l_digits[] = "0123456789abcdef";

a_usize ai_fmt_int2str(char* p, a_int v) {
	a_bool neg;

	a_u32 rem;
	if (v < 0) {
		rem = cast(a_u32, -v);
		neg = true;
	}
	else {
		rem = cast(a_u32, v);
		neg = false;
	}

	char* q = p;

	do {
		*(--q) = cast(char, rem % 10 + '0');
	}
	while ((rem = rem / 10) > 0);

	if (neg) {
		*(--q) = '-';
	}

	return p - q;
}

void ai_fmt_puti(a_henv env, Buf* buf, a_int v) {
	char cs[16];
	a_usize len = ai_fmt_int2str(cs + 16, v);
	catch (ai_buf_nputls(env, buf, cs + 16 - len, len)) {
		ai_mem_nomem(env);
	}
}

a_usize ai_fmt_float2str(char* p, a_float v) {
	a_usize n = cast(a_usize, snprintf(null, 0, "%g", v));
	sprintf(cast(char*, p - n), "%g", v);
	return n;
}

void ai_fmt_putf(a_henv env, Buf* buf, a_float v) {
	char cs[32];
	a_usize len = ai_fmt_float2str(cs + 32, v);
	catch (ai_buf_nputls(env, buf, cs + 32 - len, len)) {
		ai_mem_nomem(env);
	}
}

#if ALO_M32
# define BUFF_SIZE_FOR_PTR 8
#else
# define BUFF_SIZE_FOR_PTR 12
#endif

a_usize ai_fmt_ptr2str(char* p, void* v) {
	char* q = p;
	a_usize addr = ptr2int(v);

	for (a_u32 i = 0; i < BUFF_SIZE_FOR_PTR; ++i) {
		*(--q) = l_digits[addr & 0xf];
		addr >>= 4;
	}
	return BUFF_SIZE_FOR_PTR;
}

void ai_fmt_putp(a_henv env, Buf* buf, void* v) {
	catch (ai_buf_ncheck(env, buf, BUFF_SIZE_FOR_PTR, 1, SIZE_MAX)) {
		ai_mem_nomem(env);
	}
	buf->len += ai_fmt_ptr2str(buf_end(buf) + BUFF_SIZE_FOR_PTR, v);
}