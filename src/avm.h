/**
 *@file avm.h
 */

#ifndef avm_h_
#define avm_h_

#include "aobj.h"

intern void ai_vm_hook(a_henv env, a_msg msg);
intern void ai_vm_call(a_henv env, Value* base, a_u32 rflags);

#endif /* avm_h_ */
