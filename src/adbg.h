/*
 * adbg.h
 */

#ifndef adbg_h_
#define adbg_h_

#include "afun.h"
#include "aenv.h"

ALO_EXPORT GFun* ai_dbg_get_func(a_henv env, Frame* frame);
ALO_EXPORT a_u32 ai_dbg_get_line(GFunMeta* meta, a_insn const* pc);

#endif /* adbg_h_ */
