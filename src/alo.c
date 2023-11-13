/**
 *@file alo.c
 */

#define alo_c_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "adef.h"

#include "alo.h"
#include "alolib.h"
#include "aauxlib.h"
#include "abaselib.h"

#ifndef aloi_stdin_is_tty
# if ALO_OS_WINDOWS
#  include <io.h>
#  define aloi_stdin_is_tty() _isatty(_fileno(stdin))
# elif ALO_OS_POSIX
#  include <unistd.h>
#  define aloi_stdin_is_tty() isatty(0) 
# else
#  define aloi_stdin_is_tty() true
# endif
#endif

#ifndef aloi_readline
# ifdef ALOI_USE_READLINE

#  include <readline/readline.h>
#  include <readline/history.h>

typedef char* LinePtr;

#  define aloi_readline(env,b,l,p) ({ quiet(env); ((b) = readline(p)) != null ? ((l) = strlen(b), ALO_SYIELD) : ALO_EOUTER; })
#  define aloi_saveline(env,l) (quiet(env), add_history(l))
#  define aloi_freeline(b) free(b)

# else

typedef char LinePtr[512];

static a_msg aloi_readline(a_henv env, LinePtr buf, a_usize* plen, char const* prompt) {
	quiet(env);
	if (prompt != null) {
		aloi_show("%s", prompt);
		aloi_show_flush();
	}
	if (fgets(buf, sizeof(LinePtr), stdin) == null)
		return ALO_EOUTER;
	a_usize len = strlen(buf);
	*plen = len;
	return likely(buf[len - 1] == '\n') ? ALO_SYIELD : ALO_SOK;
}

#  define aloi_readline(env,b,l,p) aloi_readline(env, b, &(l), p)
#  define aloi_saveline(env,l) (quiet(env), quiet(l))
#  define aloi_freeline(b) quiet(b)

# endif
#endif

typedef struct InLine InLine;

struct InLine {
	InLine* _next;
	a_usize _len;
	LinePtr _ptr;
};

typedef struct {
	InLine* _head;
	InLine** _tail;
	a_usize _len;
} InLines;

static InLines l_lines;

static void l_init_lines(void) {
    init(&l_lines) {
        ._head = null,
        ._tail = &l_lines._head
    };
}

static void l_deinit_lines(void) {
	InLine* frag = l_lines._head;
	while (frag != null) {
		InLine* next = frag->_next;
		aloi_freeline(frag->_ptr);
		free(frag);
		frag = next;
	}
}

static void l_close(void) {
	l_deinit_lines();
}

static a_msg l_open(void) {
	l_init_lines();
	try(alo_init());

	atexit(l_close); /* add exit handler. */
	return ALO_SOK;
}

static a_msg l_read_line_frag(a_henv env, char const* prompt) {
	InLine* frag = malloc(sizeof(InLine));
	if (frag == null) return ALO_ENOMEM;

	a_msg msg = aloi_readline(env, frag->_ptr, frag->_len, prompt);

	if (msg < 0) {
		free(frag);
		return msg;
	}

	frag->_next = null;
	*l_lines._tail = frag;
	l_lines._tail = &frag->_next;
	l_lines._len += frag->_len;

	return msg;
}

static a_msg l_read_line(a_henv env, char const* prompt) {
	try(l_read_line_frag(env, prompt));
	loop try(l_read_line_frag(env, null));
}

static void l_save_line(a_henv env) {
	char* lines = malloc(l_lines._len + 1);
	char* dst = lines;
	if (lines == null) return;
	for (InLine* line = l_lines._head; line != null; line = line->_next) {
		memcpy(dst, line->_ptr, line->_len);
		dst += line->_len;
	}
	dst[0] = '\0';
	aloi_saveline(env, lines);
	free(lines);
}

static a_i32 l_source_input(unused a_henv env, void* rctx, void const** pdst, a_usize* plen) {
	InLine** pline = rctx;
	InLine* line = *pline;
	if (line != null) {
		*pdst = line->_ptr;
		*plen = strlen(line->_ptr);
		*pline = line->_next;
	}
	else {
		*pdst = null;
		*plen = 0;
	}
	return 0;
}

#define EOF_MARK ", got <eof>"

static a_bool l_is_eof_error(a_henv env) {
	a_usize len;
	char const* ptr = alo_tolstr(env, -1, &len);
	return len >= sizeof(EOF_MARK) - 1 && strcmp(ptr + len - sizeof(EOF_MARK) + 1, EOF_MARK) == 0;
}

static void l_print_result(a_henv env, a_msg msg) {
	if (msg == ALO_SOK) {
		a_u32 n = alo_stacksize(env);
		for (a_u32 i = 0; i < n; ++i) {
			if (i != 0) aloi_show("\t");
			aloB_show(env, i);
		}
		aloi_show_newline();
	}
	else {
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
}

static a_msg l_try_comp_reps(a_henv env, char const* prompt, a_bool* peval) {
	a_msg msg = l_read_line(env, prompt);
	if (msg < 0) return msg;

	InLine* ctx;

again:
	ctx = l_lines._head;

	/* stack: name */
	msg = alo_compile(
		env,
		l_source_input,
		&ctx,
		ALO_STACK_INDEX_GLOBAL,
		0,
		1,
		ALO_COMP_OPT_LOSSEN | (*peval ? ALO_COMP_OPT_EVAL : 0));

	switch (msg) {
		case ALO_SOK: {
			return ALO_SYIELD;
		}
		case ALO_ECHUNK: {
			if (l_is_eof_error(env)) {
				alo_settop(env, 2);
				return ALO_SOK;
			}
			else if (*peval) {
				/* Try again within statement mode. */
				*peval = false;
				alo_settop(env, 2);
				goto again;
			}
			fallthrough;
		}
		default: {
			return msg;
		}
	}
}

static a_msg l_comp_reps(a_henv env, a_bool* peval) {
    alo_pushntstr(env, "__main__");
    alo_pushntstr(env, "stdin");

	try(l_try_comp_reps(env, "> ", peval));
	loop try(l_try_comp_reps(env, ">> ", peval));
}

static a_msg l_pcomp_reps(a_henv env) {
	a_bool eval = true;
	a_msg msg = l_comp_reps(env, &eval);

	l_save_line(env);
	l_deinit_lines();
	l_init_lines();

	if (msg != ALO_SYIELD)
		return msg;

	alo_pop(env, 0); /* Drop arguments. */
	alo_settop(env, 1);
	return ALO_SOK;
}

static a_msg l_pread_eval(a_henv env) {
	try (l_pcomp_reps(env));
	try (alo_pcall(env, 0, -1, 0));
	return ALO_SOK;
}

static a_noret l_run_repl(a_henv env) {
	loop {
		a_msg msg = l_pread_eval(env);
		l_print_result(env, msg);
		alo_settop(env, 0);
	}
}

#define OPT_EXECUTE u32c(1)
#define OPT_VERSION u32c(2)
#define OPT_HELP u32c(4)

#define ALO_HELP \
"Usage: alo [options]\n" \
"\t-h print help information\n" \
"\t-v print version only"

static a_enum l_opts;

static a_bool l_scan_opts(a_henv env, int argc, char const* const* argv) {
	l_opts = OPT_EXECUTE;

	for (int i = 1; i < argc; ++i) {
		char const* arg = argv[i];
		if (arg[0] == '-') {
			switch (arg[1]) {
				case 'v':
					l_opts |= OPT_VERSION;
					l_opts &= ~OPT_EXECUTE;
					break;
				case 'V':
					l_opts |= OPT_VERSION;
					break;
				case 'h':
					l_opts |= OPT_HELP;
					break;
				case '-':
					printf("Unknown option '%s'", arg);
					l_opts |= OPT_HELP;
					return true;
				case '\0':
					printf("Incomplete option '-'");
					l_opts |= OPT_HELP;
					return true;
				default:
					printf("Unknown option '-%c'", arg[1]);
					l_opts |= OPT_HELP;
					return true;
			}
		}
		else {
			printf("Unexpected option");
		}
	}
	return false;
}

static void l_main(a_henv env) {
	if ((l_opts & OPT_VERSION) || aloi_stdin_is_tty()) {
		aloi_show(ALO_COPYRIGHT"\n");
	}
	if (l_opts & OPT_HELP) {
		aloi_show(ALO_HELP);
	}
	if (l_opts & OPT_EXECUTE) {
		aloL_openlibs(env);
		l_run_repl(env);
	}
}

int main(int argc, char const* const argv[]) {
	if (l_open() != ALO_SOK) {
		return EXIT_FAILURE; /* fatal error. */
	}

	a_henv env = aloL_create();
	a_bool argerr = l_scan_opts(env, argc, argv);
	l_main(env);
	alo_destroy(env);
	
	return !argerr ? EXIT_SUCCESS : EXIT_FAILURE;
}
