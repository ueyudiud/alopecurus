/*
 * alo.c
 */

#define alo_c_

#ifdef ALOI_DEBUG
# define ALOI_STRICT_MEMORY_CHECK
#endif

#include <stdio.h>
#include <string.h>
#include <signal.h>

#include "alist.h"
#include "aerr.h"
#include "aparse.h"
#include "adump.h"
#include "aapi.h"

#include "alo.h"
#include "aaux.h"
#include "abaselib.h"

static void l_hint(char const* msg) {
	fputs(msg, stdout);
	fflush(stdout);
}

static void l_report(char const* msg) {
	fprintf(stderr, "%s\n", msg);
	fflush(stderr);
}

static int l_hook(unused a_henv env, unused a_msg msg, unused a_kctx ctx) {
	//raise(SIGINT);
	return 0;
}

#define LINE_BUF_SIZE 256

typedef struct {
	GList* _list;
	a_isize _index;
} ConsoleInputCtx;

static a_usize l_read_line_frag(a_henv env, GList* source, char* buf) {
	if (fgets(buf, LINE_BUF_SIZE, stdin) == null)
		return 0;

	ai_list_hint(env, source, 1);

	a_usize len = strlen(buf);
	GStr* str = ai_istr_create(env, buf, len);

	ai_list_insert(env, source, v_of_ref(str));
	return len;
}

static void l_read_line(a_henv env, GList* source) {
	char buf[LINE_BUF_SIZE];

	a_usize len = l_read_line_frag(env, source, buf);
	if (len == 0) goto error;

	while (buf[len - 1] != '\n') {
		len = l_read_line_frag(env, source, buf);
		if (len == 0)
			goto error;
	}

	return;

error:
	ai_err_raisef(env, ALO_EIO, "fail to read script.");
}

static ptrdiff_t l_source_input(a_henv env, void* rctx, void const** pdst, a_usize* plen) {
	ConsoleInputCtx* ctx = rctx;
	a_isize index = ctx->_index++;
	if (index == -1) {
		*pdst = "return ";
		*plen = sizeof("return ") - 1;
	}
	else if (index < ctx->_list->_len) {
		GStr* str = v_as_str(G(env), &ctx->_list->_data[index]);
		*pdst = str->_data;
		*plen = str->_len;
	}
	else {
		*pdst = null;
		*plen = 0;
	}
	return 0;
}

static a_bool l_try_compile_console(a_henv env, GList* source, a_bool expr, GFun** pfun) {
	ConsoleInputCtx ctx = { source, expr ? -1 : 0 };
	a_msg msg = ai_parse(env, l_source_input, &ctx, "<stdin>", ALO_COMP_OPT_CHECK_STMUF, pfun);
	switch (msg) {
		case ALO_SOK: {
			return true;
		}
		case ALO_ECHUNK: {
			if (!expr) {
				GStr* str = v_as_str(G(env), &env->_error);
				l_report(cast(char const*, str->_data));
				return true;
			}
			return false;
		}
		case ALO_ESTMUF: {
			return false;
		}
		default: {
			if (v_is_str(&env->_error)) {
				GStr* str = v_as_str(G(env), &env->_error);
				l_report(cast(char const*, str->_data));
			}
			return true;
		}
	}
}

static a_bool l_compile_console(a_henv env) {
	GList* list;
	GFun* fun = null;

	alo_newlist(env, 1);
	list = v_as_list(G(env), env->_stack._top - 1);

	l_hint("> ");
	l_read_line(env, list);
	if (l_try_compile_console(env, list, true, &fun) ||
			l_try_compile_console(env, list, false, &fun))
		goto end;

	do {
		l_hint(">> ");
		l_read_line(env, list);
	}
	while (!l_try_compile_console(env, list, false, &fun));

end:
	alo_settop(env, -1); /* Drop list. */
	a_bool success = fun != null;
	if (success) {
		v_set(G(env), api_incr_stack(env), v_of_ref(fun));
	}
	ai_gc_trigger(env);
	return success;
}

static void l_print_result(a_henv env) {
	a_u32 n = alo_stacksize(env);
	for (a_u32 i = 0; i < n; ++i) {
		aloL_base_show(env, i);
		if (i != 0) aloi_show("\t");
	}
	aloi_show("\n");
}

static a_none l_run_console(a_henv env) {
	loop {
		if (l_compile_console(env)) {
			alo_dump(env, -1, ALO_DUMP_OPT_CONST_POOL | ALO_DUMP_OPT_LOCAL | ALO_DUMP_OPT_LINE);
//			alo_call(env, 0, -1);
//			l_print_result(env);
			alo_settop(env, 0);
		}
	}
}

static void l_main(a_henv env) {
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
