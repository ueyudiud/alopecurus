/**
 *@file adbg.c
 */

#define adbg_c_
#define ALO_LIB

#include "afun.h"
#include "agc.h"
#include "aapi.h"

#include "adbg.h"

#if ALO_DEBUG

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
	Value* bot = stk2val(env, frame->_stack_bot);
	return bot > env->_stack._base ? v_as_func(bot[-1]) : null;
}

a_u32 ai_dbg_get_line(GProto* proto, a_insn const* pc) {
	if (!(proto->_flags & FUN_FLAG_NATIVE) && proto->_dbg_lines != null) {
		a_u32 disp = pc - proto->_code;
		a_u32 lo = 0;
		a_u32 hi = proto->_nline - 1;
		a_u32 mi;
		while (lo < hi) {
			mi = (lo + hi) >> 1;
			LineInfo* info = &proto->_dbg_lines[mi];
			if (disp >= info->_end) {
				lo = mi + 1;
			}
			else {
				hi = mi;
			}
		}
		return proto->_dbg_lines[lo]._lineno;
	}
	else {
		return 0;
	}
}

static void l_get_source(alo_Debug* dbg, GFun* fun, a_insn const* pc) {
	if (likely(fun != null)) {
		GProto* proto = fun->_proto;
		dbg->file = proto->_dbg_file != null ? str2ntstr(proto->_dbg_file) : null;
		dbg->line = ai_dbg_get_line(proto, pc);
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
