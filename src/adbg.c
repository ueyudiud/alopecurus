/**
 *@file adbg.c
 */

#define adbg_c_
#define ALO_LIB

#include "afun.h"
#include "agc.h"
#include "aapi.h"

#include "adbg.h"

#if defined(ALOI_DEBUG)

#include <stdio.h>

a_none ai_dbg_panic(char const* fmt, ...) {
	va_list varg;
	va_start(varg, fmt);
	vfprintf(stderr, fmt, varg);
	fflush(stderr);
	va_end(varg);
	trap();
}

#endif

GFun* ai_dbg_get_func(a_henv env, Frame* frame) {
	Value* bot = ptr_disp(Value, env->_stack._base, frame->_stack_bot_diff);
	return bot > env->_stack._base ? v_as_func(G(env), bot[-1]) : null;
}

a_u32 ai_dbg_get_line(GFunMeta* meta, a_insn const* pc) {
	if (meta->_dbg_lines != null) {
		a_u32 disp = pc - meta->_code;
		a_u32 lo = 0;
		a_u32 hi = meta->_nline - 1;
		a_u32 mi;
		while (lo < hi) {
			mi = (lo + hi) >> 1;
			LineInfo* info = &meta->_dbg_lines[mi];
			if (disp >= info->_end) {
				lo = mi + 1;
			}
			else {
				hi = mi;
			}
		}
		return meta->_dbg_lines[lo]._lineno;
	}
	else {
		return 0;
	}
}

static void l_get_source(alo_Debug* dbg, GFun* func, a_insn* pc) {
	if (likely(func != null)) {
		GFunMeta* meta = g_cast(GFunMeta, func->_meta);
		dbg->file = meta->_dbg_file != null ? ai_str_tocstr(meta->_dbg_file) : null;
		dbg->line = ai_dbg_get_line(meta, pc);
	}
	else {
		dbg->file = null;
		dbg->line = 0;
	}
}

a_bool alo_debug(a_henv env, alo_Debug* dbg, a_enum n, a_flags w) {
	Frame** pframe = cast(Frame**, &dbg->_hnd);
	switch (n) {
		case ALO_DEBUG_START: {
			*pframe = env->_frame;
			return true;
		}
		case ALO_DEBUG_GET: {
			Frame* frame = *pframe;
			api_check(frame != null, "already reach to end.");
			GFun* func = ai_dbg_get_func(env, frame);
			if (w & ALO_DEBUG_GET_FLAG_SOURCE) {
				l_get_source(dbg, func, frame->_pc);
			}
			if (w & ALO_DEBUG_GET_FLAG_THEN_NEXT)
				goto get_next;
			return true;
		}
		case ALO_DEBUG_NEXT: {
		get_next:
			return (*pframe = (*pframe)->_prev) != null;
		}
		default: {
			api_panic("bad debug task.");
		}
	}
}