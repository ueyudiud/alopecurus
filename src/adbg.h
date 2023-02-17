/*
 * adbg.h
 */

#ifndef adbg_h_
#define adbg_h_

#include "afun.h"
#include "aenv.h"

ALO_EXPORT GFun* ai_dbg_get_func(a_henv env, Frame* frame);
ALO_EXPORT a_u32 ai_dbg_get_line(GFunMeta* meta, a_insn const* pc);

always_inline char const* ai_dbg_get_func_name(a_henv env, Frame* frame) {
	GFun* fun = ai_dbg_get_func(env, frame);
	if (fun == null) return null;
	GStr* name = g_cast(GFunMeta, fun->_meta)->_name;
	return name != null ? ai_str_tocstr(name) : null;
}

#endif /* adbg_h_ */
