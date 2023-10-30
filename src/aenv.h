/*
 * aenv.h
 */

#ifndef aenv_h_
#define aenv_h_

#include "astr.h"
#include "astk.h"
#include "afun.h"

#define GLOBAL_FLAG_INCRGC u16c(0x0001)
#define GLOBAL_FLAG_FULLGC u16c(0x0002)
#define GLOBAL_FLAG_EMERGENCYGC u16c(0x0004)
#define GLOBAL_FLAG_DISABLE_GC u16c(0x0008)

intern GRoute* ai_env_new(a_henv env, a_usize stack_size);
intern a_henv ai_env_mroute(Global* g);
intern a_msg ai_env_resume(a_henv env, GRoute* self);
intern void ai_env_yield(a_henv env);
intern a_msg ai_env_pcall(a_henv env, a_pfun pfun, void* pctx);
intern a_none ai_env_raise(a_henv env, a_msg msg);

#ifndef ALOI_DFL_GCSTEPMUL
# define ALOI_DFL_GCSTEPMUL usizec(384)
#endif

#ifndef ALOI_DFL_GCPAUSEMUL
# define ALOI_DFL_GCPAUSEMUL usizec(512)
#endif

#ifndef ALOI_INIT_CFRAMESIZE
# define ALOI_INIT_CFRAMESIZE usizec(8)
#endif

always_inline void check_in_stack(a_henv env, Value* v) {
	assume(v >= env->_stack._base && v < env->_stack._limit + RESERVED_STACK_SIZE, "not stack value pointer.");
}

always_inline StkPtr val2stk(a_henv env, Value* v) {
	check_in_stack(env, v);
#if ALO_STACK_RELOC
	return addr_diff(v, env->_stack._impl);
#else
	quiet(env);
	return v;
#endif
}

always_inline Value* stk2val(a_henv env, StkPtr p) {
	Value* v;
#if ALO_STACK_RELOC
	v = ptr_disp(Value, env->_stack._impl, p);
#else
	quiet(env);
	v = p;
#endif
	check_in_stack(env, v);
	return v;
}

always_inline Value* ai_stk_bot(a_henv env) {
	return stk2val(env, env->_frame->_stack_bot);
}

always_inline a_isize ai_stk_check(a_henv env, Value* top) {
	if (top > env->_stack._limit) {
		a_isize diff = ai_stk_grow(env, top);
		if (diff & STACK_GROW_FAILED) {
			ai_stk_overflow(env, diff);
		}
		return diff;
	}
	return 0;
}

always_inline void ai_env_pop_error(a_henv env, Value* d) {
	v_cpy(env, d, &env->_error);
	v_set_nil(&env->_error);
}

#endif /* aenv_h_ */
