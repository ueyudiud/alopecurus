/**
 *@file adbg.c
 */

#define adbg_c_
#define ALO_LIB

#include "afun.h"
#include "agc.h"
#include "aapi.h"

#include "adbg.h"

#if defined(ALOI_DEBUG) || defined(ALOI_CHECK_API)

#include <stdio.h>

ALO_NORETURN void ai_dbg_panic(char const* fmt, ...) {
	va_list varg;
	va_start(varg, fmt);
	vfprintf(stderr, fmt, varg);
	fflush(stderr);
	va_end(varg);
	trap();
}

#endif

GFun* ai_dbg_get_func(a_henv env, Frame* frame) {
	Value* bot = stk2val(env, frame->stack_bot);
	return bot > env->stack.base ? v_as_func(bot[-1]) : nullptr;
}

a_u32 ai_dbg_get_line(GProto* proto, a_insn const* pc) {
    if (proto->dbg_lines == nullptr)
        return 0;
    a_u32 disp = pc - proto->code;
    a_u32 lo = 0;
    a_u32 hi = proto->nline - 1;
    a_u32 mi;
    while (lo < hi) {
        mi = (lo + hi) >> 1;
        LineInfo* info = &proto->dbg_lines[mi];
        if (disp >= info->lend) {
            lo = mi + 1;
        } else {
            hi = mi;
        }
    }
    return proto->dbg_lines[lo].line;
}

static void l_get_source(alo_Debug* dbg, GFun* fun, a_insn const* pc) {
    if (fun == nullptr || fun->flags & FUN_FLAG_NATIVE) {
        dbg->file = nullptr;
        dbg->line = 0;
        return;
    }
    GProto* proto = fun->proto;
    dbg->file = proto->dbg_file != nullptr ? str2ntstr(proto->dbg_file) : nullptr;
    dbg->line = ai_dbg_get_line(proto, pc);
}

static Frame* l_load_frame(a_henv env, Frame** pframe, a_enum n) {
    Frame* frame;
    switch (n) {
        case ALO_DEBUG_HEAD: {
            frame = env->frame;
            break;
        }
        case ALO_DEBUG_NEXT: {
            frame = *pframe;
            *pframe = frame->prev;
            break;
        }
        case ALO_DEBUG_THIS: {
            frame = *pframe;
            break;
        }
        default: api_panic("bad load frame type.");
    }
    return frame;
}

a_bool alo_debug(a_henv env, alo_Debug* dbg, a_enum n, a_flags w) {
    Frame* frame = l_load_frame(env, cast(Frame**, &dbg->_frame), n);
    if (frame == nullptr) return false;

    GFun* p = ai_dbg_get_func(env, frame);
    if (w & ALO_DEBUG_FLAG_SOURCE) {
        l_get_source(dbg, p, frame->pc);
    }

    return true;
}
