/**
 *@file actx/x64.w64.win.h
 */

#ifndef actx_h_
# error must include 'actx_h_'
#endif

#if ALO_OS_WINDOWS && ALO_ARCH_X64

#define actx_x64_w64_win_h_
#define actx_impl_h_

/* Basic stack size alignment. */
#define PAGE_SIZE usizec(0x1000)

#define EXCEPTION_CODE_PREFIX 0xEA4BD800

struct PCtx {};

intern naked a_msg ai_ctx_catch0();
intern naked a_msg ai_ctx_jump0();

always_inline a_msg ai_ctx_catch(a_henv env, a_pfun fun, void* ctx) {
	register void* reg_env asm("rcx") = env;
	register void* reg_ctx asm("rdx") = ctx;
	register void* reg_fun asm("r8") = fun;
	a_msg reg_msg;

	asm volatile(
		"subq $0x20, %%rsp\n"
		"call %p[catch]\n"
		"addq $0x20, %%rsp\n":
		"=a"(reg_msg):
		"r"(reg_env),
		"r"(reg_ctx),
		"r"(reg_fun),
		[catch]"p"(ai_ctx_catch0));

	return reg_msg;
}

always_inline a_msg ai_ctx_jump(a_henv to, a_henv from, a_msg msg) {
	register void* reg_to asm("rcx") = to;
	register void* reg_from asm("rdx") = from;
	register a_msg reg_msg asm("rax") = msg;

	asm volatile(
		"movq %%rcx, %%rsi\n"
		"call %p[jump]"
		:
		"+a"(reg_msg):
		"r"(reg_to),
		"r"(reg_from),
		[jump]"p"(ai_ctx_jump0));

	return reg_msg;
}

#endif
