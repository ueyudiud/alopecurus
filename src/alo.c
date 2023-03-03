/**
 *@file alo.c
 */

#define alo_c_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include "adef.h"

#include "alo.h"
#include "alolib.h"
#include "aauxlib.h"
#include "abaselib.h"

typedef struct InLine InLine;

static void l_hint(char const* msg) {
	fputs(msg, stdout);
	fflush(stdout);
}

static void l_report(char const* fmt, ...) {
	va_list varg;
	va_start(varg, fmt);
	vfprintf(stderr, fmt, varg);
	va_end(varg);
	fputc('\n', stderr);
	fflush(stderr);
}

static void l_hook(a_henv env, a_msg msg, unused a_hctx ctx) {
	if (msg < 0) {
		aloL_traceerror(env, ALO_STACK_INDEX_ERROR, 6);
	}
}

struct InLine {
	InLine* _next;
	a_usize _len;
	char _body[];
};

typedef struct {
	InLine* _head;
	InLine** _tail;
} InFull;

static InFull l_in;

static void l_free_input() {
	InLine* frag = l_in._head;
	while (frag != null) {
		InLine* next = frag->_next;
		free(frag);
		frag = next;
	}
}

#define LINE_BUF_SIZE 256

static a_msg l_read_line_frag(void) {
	char buf[LINE_BUF_SIZE];

	if (fgets(buf, LINE_BUF_SIZE, stdin) == null)
		return ALO_EIO;

	a_usize len = strlen(buf);

	InLine* frag = malloc(sizeof(InLine) + len);
	if (frag == null) return ALO_ENOMEM;

	frag->_next = null;
	frag->_len = len;
	memcpy(frag->_body, buf, len);
	*l_in._tail = frag;
	l_in._tail = &frag->_next;

	return buf[len - 1] == '\n' ? ALO_SEXIT : ALO_SOK;
}

static void l_read_line(a_henv env) {
	a_i32 msg = l_read_line_frag();
	if (msg < 0) goto error;

	while (msg == ALO_SOK) {
		msg = l_read_line_frag();
		if (msg < 0) goto error;
	}

	return;

error:
	l_free_input();
	aloL_errorf(env, "fail to read script.");
}

static a_i32 l_source_input(unused a_henv env, void* rctx, void const** pdst, a_usize* plen) {
	InLine** pline = rctx;
	InLine* line = *pline;
	if (line != null) {
		*pdst = line->_body;
		*plen = line->_len;
		*pline = line->_next;
	}
	else {
		*pdst = null;
		*plen = 0;
	}
	return 0;
}

static a_bool l_try_comp_console(a_henv env, a_bool expr, a_bool* psuccess) {
	InLine* ctx = l_in._head;

	/* stack: name */
	a_msg msg = alo_compile(
		env, 
		l_source_input, 
		&ctx, 
		ALO_STACK_INDEX_GLOBAL, 
		0,
		1,
		ALO_COMP_OPT_ALOC1 | (expr ? ALO_COMP_OPT_EVAL : 0));

	switch (msg) {
		case ALO_SOK: {
			*psuccess = true;
			return true;
		}
		case ALO_ECHUNK: {
			if (!expr) goto error;
			fallthrough;
		}
		case ALO_ESTMUF: {
			alo_settop(env, 2);
			return false;
		}
		default: {
		error:
			aloL_base_show(env, -1);
			aloi_show_newline();
			*psuccess = false;
			alo_settop(env, 1);
			return true;
		}
	}
}

static a_bool l_comp_console(a_henv env) {
	a_bool success;
	/* stack: <empty> */

	alo_pushlstr(env, "__main__");
	alo_pushlstr(env, "stdin");

	/* stack: file name */

	l_in = new(InFull) { ._head = null, ._tail = &l_in._head };

	l_hint("> ");
	l_read_line(env);
	if (l_try_comp_console(env, true, &success) || l_try_comp_console(env, false, &success))
		goto end;

	do {
		l_hint(">> ");
		l_read_line(env);
	}
	while (!l_try_comp_console(env, false, &success));

end:
	if (success) {
		alo_pop(env, 0); /* Drop arguments. */
		alo_settop(env, 1);
	}
	else {
		alo_settop(env, 0);
	}
	return success;
}

static void l_print_result(a_henv env) {
	a_u32 n = alo_stacksize(env);
	for (a_u32 i = 0; i < n; ++i) {
		if (i != 0) aloi_show("\t");
		aloL_base_show(env, i);
	}
	aloi_show_newline();
}

static void l_print_error(a_henv env) {
	switch (alo_tagof(env, -1)) {
		case ALO_TINT: {
			aloi_show("error code: %d\n", alo_toint(env, -1));
			break;
		}
		case ALO_TSTR: {
			aloi_show("%s\n", alo_tostr(env, -1));
			break;
		}
		default: {
			aloi_show("unidentified error.\n");
			break;
		}
	}
}

static a_none l_run_console(a_henv env) {
	loop {
		if (l_comp_console(env)) {
			a_msg msg = alo_pcall(env, 0, -1, 0);
			if (msg == ALO_SOK) {
				l_print_result(env);
			}
			else {
				l_print_error(env);
			}
			alo_settop(env, 0);
		}
	}
}

static void l_main(a_henv env) {
	aloL_openlibs(env);
	l_run_console(env);
}

int main(int argc, char const** argv) {
	(void) argc;
	(void) argv;
	alo_init();
	a_henv env = aloL_create();
	alo_sethook(env, l_hook, 0, ALO_HMRAISE);
	l_main(env);
	alo_destroy(env);
	return 0;
}
