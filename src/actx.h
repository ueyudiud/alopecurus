/**
 *@file actx.h
 */

#ifndef actx_h_
#define actx_h_

#include "aarch.h"
#include "adef.h"

/* Basic stack size alignment. */
#define ALO_PAGE_SIZE usizec(0x1000)

typedef struct PCtx PCtx;

intern a_msg ai_ctx_init(void);
intern a_noret ai_ctx_raise(a_henv env, a_msg msg);
intern a_msg ai_ctx_open(a_henv env, a_usize stack_size);
intern void ai_ctx_close(a_henv env);

intern a_msg ai_ctx_catch(a_henv env, a_pfun fun, void* ctx);
intern a_msg ai_ctx_jump(a_henv to, a_henv from, a_msg msg);

#if ALO_OS_WINDOWS && ALO_ARCH_X64

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

#elif ALO_OS_POSIX && ALO_ARCH_X64

#include <setjmp.h>

typedef struct JStub JStub;

struct PCtx {
    JStub* stub;
};

struct JStub {
    jmp_buf jbuf;
    JStub* prev;
};

#else
# error unsupport architecture.
#endif

#endif /* actx_h_ */
