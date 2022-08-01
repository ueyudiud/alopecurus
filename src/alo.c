/*
 * alo.c
 */

#include <stdio.h>
#include <string.h>

#include "alo.h"
#include "aaux.h"

#define S(s...) #s

int main(int argc, char const** argv) {
	(void) argc;
	(void) argv;
	alo_init();
	a_henv env = aloL_create();
	char const* source = S(
		x = a[1]
	);
	a_msg msg = aloL_readstr(env, source, strlen(source), "<main>", 0);
	if (msg != ALO_SOK) {
		printf("%s", alo_tostr(env, -1, NULL));
	}
	else {
		alo_dump(env, -1, 0);
	}
	alo_destroy(env);
	return 0;
}
