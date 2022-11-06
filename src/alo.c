/*
 * alo.c
 */

#include <stdio.h>
#include <string.h>
#include <signal.h>

#include "alo.h"
#include "aaux.h"

#define S(s...) #s

static int l_hook(a_henv env, a_msg msg, a_kctx ctx) {
	raise(SIGINT);
	return 0;
}

int main(int argc, char const** argv) {
	(void) argc;
	(void) argv;
	alo_init();
	a_henv env = aloL_create();
	alo_sethook(env, l_hook, 0, ALO_HMRAISE);
	char const* source = S(
		let a = 'Hello world!'
		if (a) {
			println(a)
		}
		return a, b
	);
	a_msg msg = aloL_readstr(env, source, strlen(source), "<main>", 0);
	if (msg != ALO_SOK) {
		fprintf(stderr, "%s", alo_tostr(env, -1, NULL));
	}
	else {
		alo_dump(env, -1, 0);
	}
	alo_destroy(env);
	return 0;
}
