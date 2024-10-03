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

typedef char* LineBuf;

#  define aloi_readline(env,b,l,p) ({ quiet(env); ((b) = readline(p)) != null ? ((l) = strlen(b), ALO_SYIELD) : ALO_EOUTER; })
#  define aloi_saveline(env,l) (quiet(env), add_history(l))
#  define aloi_freeline(b) free(b)

# else

typedef char LineBuf[512];

static a_msg aloi_readline(a_henv env, LineBuf buf, a_usize* plen, char const* prompt) {
	quiet(env);
	if (prompt != null) {
		aloi_show("%s", prompt);
		aloi_show_flush();
	}
	if (fgets(buf, sizeof(LineBuf), stdin) == null)
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
	InLine* next;
	a_usize len;
	LineBuf ptr;
};

typedef struct {
	InLine* head;
	InLine** tail;
	a_usize len;
} InLines;

static InLines l_lines;

static void l_init_lines(void) {
    init(&l_lines) {
        .head = null,
        .tail = &l_lines.head
    };
}

static void l_deinit_lines(void) {
	InLine* frag = l_lines.head;
	while (frag != null) {
		InLine* next = frag->next;
		aloi_freeline(frag->ptr);
		free(frag);
		frag = next;
	}
}

static void l_close(void) {
	l_deinit_lines();
}

static a_msg l_open(void) {
	l_init_lines();
	try (alo_init());

	atexit(l_close); /* add exit handler. */
	return ALO_SOK;
}

static a_msg l_read_frag(a_henv env, char const* prompt) {
	InLine* frag = malloc(sizeof(InLine));
	if (frag == null) return ALO_ENOMEM;

	a_msg msg = aloi_readline(env, frag->ptr, frag->len, prompt);

	if (msg < 0) {
		free(frag);
		return msg;
	}

	frag->next = null;
	*l_lines.tail = frag;
	l_lines.tail = &frag->next;
	l_lines.len += frag->len;

	return msg;
}

static a_msg l_read_line(a_henv env, char const* prompt) {
	try (l_read_frag(env, prompt));
	loop try (l_read_frag(env, null));
}

static void l_save_line(a_henv env) {
	char* lines = malloc(l_lines.len + 1);
	char* dst = lines;
	if (lines == null) return;
	for (InLine* line = l_lines.head; line != null; line = line->next) {
		memcpy(dst, line->ptr, line->len);
		dst += line->len;
	}
	dst[0] = '\0';
	aloi_saveline(env, lines);
	free(lines);
}

static a_i32 l_read_lines(unused a_henv env, void* rctx, void const** pdst, a_usize* plen) {
	InLine** pline = rctx;
	InLine* line = *pline;
	if (line != null) {
		*pdst = line->ptr;
		*plen = strlen(line->ptr);
		*pline = line->next;
	}
	else {
		*pdst = null;
		*plen = 0;
	}
	return 0;
}

#define EOF_MARK ", got <eof>"

static a_bool l_test_eof(a_henv env) {
	a_usize len;
	char const* ptr = alo_tolstr(env, -1, &len);
	return len >= sizeof(EOF_MARK) - 1 && strcmp(ptr + len - sizeof(EOF_MARK) + 1, EOF_MARK) == 0;
}

static a_msg l_print_error(a_henv env, a_msg msg) {
    catch (msg) {
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
    return msg;
}

static void l_print_result(a_henv env, a_msg msg) {
	if (msg == ALO_SOK) {
		a_ilen n = alo_stacksize(env);
		for (a_ilen i = 0; i < n; ++i) {
			if (i != 0) aloi_show("\t");
			aloB_show(env, i);
		}
		aloi_show_newline();
	}
	else {
        l_print_error(env, msg);
	}
}

static a_msg l_try_comp_reps(a_henv env, char const* prompt, a_bool* peval) {
	a_msg msg = l_read_line(env, prompt);
	if (msg < 0) return msg;

	InLine* ctx;

again:
	ctx = l_lines.head;

	/* stack: dbg_name */
	msg = alo_compile(
        env,
        l_read_lines,
        &ctx,
        ALO_STACK_INDEX_GLOBAL,
        0,
        "stdin",
        ALO_COMP_OPT_LOSSEN | (*peval ? ALO_COMP_OPT_EVAL : 0));

	switch (msg) {
		case ALO_SOK: {
			return ALO_SYIELD;
		}
		case ALO_ECHUNK: {
			if (l_test_eof(env)) {
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

	try (l_try_comp_reps(env, "> ", peval));
	loop try (l_try_comp_reps(env, ">> ", peval));
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

static a_msg l_stack_trace(a_henv env) {
    aloL_traceerror(env, env, 0, 1, 6);
    return 1;
}

static a_msg l_eval(a_henv env) {
    alo_newcfun(env, l_stack_trace, 0);
    alo_insert(env, 0);
    try (alo_pcall(env, 0, -1, 0));
    alo_erase(env, 0, 1);
    return ALO_SOK;
}

static a_msg l_pread_eval(a_henv env) {
	try (l_pcomp_reps(env));
	return l_eval(env);
}

static a_msg l_run_file(a_henv env, char const* file) {
    char const* fname = file[0] != '-' ? file : null;
    catch (aloL_compilef(env, fname, ALO_COMP_OPT_NOTHING), msg) {
        return l_print_error(env, msg);
    }
    a_msg msg = l_eval(env);
    return l_print_error(env, msg);
}

static a_noret l_run_repl(a_henv env) {
	loop {
		a_msg msg = l_pread_eval(env);
		l_print_result(env, msg);
		alo_settop(env, 0);
	}
}

#define OPT_I u32c(1)
#define OPT_V u32c(2)
#define OPT_H u32c(4)

#define ALO_HELP \
"Usage: alo [options] [script [args]]\n"            \
"Available options are:\n"                          \
"\t-h show help information\n"                      \
"\t-i into interactive mode after execute script\n" \
"\t-v show version information\n"                   \
"\t-- stop handling options\n"                      \
"\t-  stop handling options and execute stdin"

static a_enum l_opts;
static char const* l_script;

static a_bool l_scan_opts(a_henv env, int argc, char const* const* argv) {
	l_opts = 0;
    l_script = null;

    int i;

	for (i = 1; i < argc; ++i) {
		char const* arg = argv[i];
        if (arg[0] != '-') {
            l_script = arg;
            break;
        }
        else if (strcmp(arg, "-") == 0) {
            l_script = "-";
            goto push_args;
        }
        else if (strcmp(arg, "--") == 0) {
            goto push_args;
        }
        else if (strcmp(arg, "-h") == 0) {
            l_opts |= OPT_H;
        }
        else if (strcmp(arg, "-v") == 0) {
            l_opts |= OPT_V;
        }
        else if (strcmp(arg, "-i") == 0) {
            l_opts |= OPT_I;
        }
        else {
            printf("unrecognized option '%s'", arg);
            l_opts |= OPT_H;
            return true;
        }
	}

push_args:
    alo_newlist(env, argc - i);
    for (; i < argc; ++i) {
        alo_pushntstr(env, argv[i]);
        alo_put(env, -2);
    }
    aloL_puts(env, ALO_STACK_INDEX_GLOBAL, "args");

	return false;
}

static a_msg l_main(a_henv env) {
    a_int argc = aloL_checkint(env, 0);
    char const* const* argv = aloL_checkptr(env, 1);
    alo_settop(env, 0);

    catch (l_scan_opts(env, argc, argv)) {
        aloi_show(ALO_HELP);
        return 0;
    }

    if ((l_opts & OPT_V) || aloi_stdin_is_tty()) {
        aloi_show(ALO_COPYRIGHT"\n");
    }
    if (l_opts & OPT_H) {
        aloi_show(ALO_HELP);
    }
    aloL_openlibs(env);
    if (l_script != null) {
        catch (l_run_file(env, l_script)) {
            return 0;
        }
    }
    if (!((l_opts & OPT_V) || l_script != null) || (l_opts & OPT_I)) {
        l_run_repl(env);
    }
    return 0;
}

int main(int argc, char const* const argv[]) {
	catch (l_open()) {
		return EXIT_FAILURE; /* fatal error. */
	}

	a_henv env = aloL_create();
    alo_newcfun(env, l_main, 0);
    alo_pushint(env, argc);
    alo_pushptr(env, argv);
    a_msg msg = alo_pcall(env, 2, 0, ALO_STACK_INDEX_EMPTY);
	alo_destroy(env);
	
	return msg == ALO_SOK ? EXIT_SUCCESS : EXIT_FAILURE;
}
