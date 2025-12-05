/**
 *@file avm.h
 */

#ifndef avm_h_
#define avm_h_

#include "aenv.h"

ALO_PRIVATE a_u32 ai_vm_lock_hook(Global* gbl);
ALO_PRIVATE void ai_vm_hook(a_henv env, a_msg msg, a_u32 test);
ALO_PRIVATE a_hash ai_vm_hash(a_henv env, Value v);
ALO_PRIVATE a_bool ai_vm_equals(a_henv env, Value v1, Value v2);
ALO_PRIVATE Value ai_vm_unary(a_henv env, Value v, a_enum op);
ALO_PRIVATE Value ai_vm_binary(a_henv env, Value v1, Value v2, a_enum op);
ALO_PRIVATE a_bool ai_vm_compare(a_henv env, Value v1, Value v2, a_enum op);
ALO_PRIVATE a_uint ai_vm_len(a_henv env, Value v);
ALO_PRIVATE Value ai_vm_get(a_henv env, Value v1, Value v2);
ALO_PRIVATE void ai_vm_set(a_henv env, Value v1, Value v2, Value v3);
ALO_PRIVATE void ai_vm_iter(a_henv env, Value* vs, Value v);
ALO_PRIVATE a_msg ai_vm_next(a_henv env, Value* vs);
ALO_PRIVATE void ai_vm_append(a_henv env, GBuf* buf, Value v);
ALO_PRIVATE void ai_vm_call(a_henv env, Value* base, a_i32 nret);
ALO_PRIVATE Value ai_vm_call_meta(a_henv env, Value* bot);

ALO_INLINE Value* vm_push_args(a_henv env, Value const* src, a_usize len) {
	Value* dst = env->stack.top;
	v_cpy_all(env, dst, src, len);
	env->stack.top = dst + len;
	return dst;
}

#define vm_push_args(env,...) ({ Value const _vs[] = {__VA_ARGS__}; vm_push_args(env, _vs, sizeof(_vs) / sizeof(Value)); })

#endif /* avm_h_ */
