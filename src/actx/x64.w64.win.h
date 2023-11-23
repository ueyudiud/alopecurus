/**
 *@file actx/x64.w64.win.h
 */

#ifndef actx_h_
# error must include 'actx_h_'
#endif

#if ALO_OS_WINDOWS && ALO_ARCH_X64

#define actx_x64_w64_win_h_
#define actx_impl_h_

#include <windows.h>

/* Basic stack size alignment. */
#define PAGE_SIZE usizec(0x1000)

#define EXCEPTION_CODE_PREFIX 0xEA4BD800

struct PCtx {};

intern a_msg ai_ctx_catch_(a_henv env, a_pfun fun, void* ctx);

always_inline a_msg ai_ctx_catch(a_henv env, a_pfun fun, void* ctx) {
	asm volatile("movq %0, %%rsi"::"r"(env):"rsi");
	return ai_ctx_catch_(env, fun, ctx);
}

#endif
