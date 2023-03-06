/**
 *@file avm.h
 */

#ifndef avm_h_
#define avm_h_

#include "aenv.h"

intern a_u32 ai_vm_lock_hook(Global* g);
intern void ai_vm_hook(a_henv env, a_msg msg, a_u32 test);
intern a_hash ai_vm_hash(a_henv env, Value v);
intern a_bool ai_vm_equals(a_henv env, Value v1, Value v2);
intern Value ai_vm_call(a_henv env, Value* base, RFlags rflags);

always_inline Value* vm_push_args(a_henv env, Value const* src, a_usize len) {
	Value* dst = env->_stack._top;
	v_cpy_multi(env, dst, src, len);
	env->_stack._top = dst + len;
	return dst;
}

#define vm_push_args(env,args...) ({ Value const _vs[] = {args}; vm_push_args(env, _vs, sizeof(_vs) / sizeof(Value)); })

#endif /* avm_h_ */
