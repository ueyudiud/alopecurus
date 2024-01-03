/*
 * adbg.h
 */

#ifndef adbg_h_
#define adbg_h_

#include "astr.h"
#include "afun.h"

ALO_EXPORT GFun* ai_dbg_get_func(a_henv env, Frame* frame);
ALO_EXPORT a_u32 ai_dbg_get_line(GProto* proto, a_insn const* pc);

always_inline char const* ai_dbg_get_func_name(a_henv env, Frame* frame) {
	GFun* fun = ai_dbg_get_func(env, frame);
	if (fun == null) return null;
	if (fun->flags & FUN_FLAG_NATIVE) {
		//TODO get native function dbg_name.
		return null;
	}
	GStr* name = fun->proto->dbg_name;
	return name != null ? str2ntstr(name) : null;
}

#endif /* adbg_h_ */
