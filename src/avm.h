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
intern void ai_vm_call(a_henv env, Value* base, RFlags rflags);

#endif /* avm_h_ */
