/*
 * afmt.c
 *
 *  Created on: 2022/8/11
 *      Author: ueyudiud
 */

#define afmt_c_

#include <stdio.h>

#include "afmt.h"

a_usize ai_fmt_i2s(a_byte* p, a_int v) {
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

	a_byte* q = p;

	do {
		*(--q) = rem % 10 + '0';
	}
	while ((rem = rem / 10) > 0);

	if (neg) {
		*(--q) = '-';
	}

	return p - q;
}

a_msg ai_fmt_puti(a_henv env, void* buf, a_int v) {
	a_byte cs[16];
	a_usize len = ai_fmt_i2s(cs + 16, v);
	ai_buf_puts(env, buf, cs + 16 - len, len);
	return ALO_SOK;
}

a_usize ai_fmt_f2s(a_byte* p, a_float v) {
	a_usize n = cast(a_usize, snprintf(null, 0, "%g", v));
	sprintf(cast(char*, p - n), "%g", v);
	return n;
}

a_msg ai_fmt_putf(a_henv env, void* buf, a_float v) {
	a_byte cs[32];
	a_usize len = ai_fmt_f2s(cs + 32, v);
	ai_buf_puts(env, buf, cs + 32 - len, len);
	return ALO_SOK;
}