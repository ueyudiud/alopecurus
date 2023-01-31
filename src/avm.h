/**
 *@file avm.h
 */

#ifndef avm_h_
#define avm_h_

#include "aobj.h"

intern a_u32 ai_vm_lock_hook(Global* g);
intern void ai_vm_hook(a_henv env, a_msg msg);
intern void ai_vm_call(a_henv env, Value* base, RFlags rflags);

#endif /* avm_h_ */
