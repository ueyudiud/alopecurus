/**
 *@file actx.x64.win.h
 */

#ifndef actx_h_
# error must include 'actx_h_'
#endif

#include <windef.h>
#include <intrin.h>

#include "aenv.h"

/* Basic stack size alignment. */
#define PAGE_SIZE usizec(0x1000)

typedef ULONG_PTR a_gpr;
typedef union {
	struct { a_u64 _[2]; };
	M128A _win;
} a_fpr;

struct RCtx {
	void* _stack_alloc_base;
	void* _stack_base;
	void* _stack_limit;
	void* _except_list;
	a_gpr _rbx;
	a_gpr _rsp;
	a_gpr _rbp;
	a_gpr _rsi;
	a_gpr _rdi;
	a_gpr _r12;
	a_gpr _r13;
	a_gpr _r14;
	a_gpr _r15;
	a_gpr _rip;
	a_fpr _xmm6;
	a_fpr _xmm7;
	a_fpr _xmm8;
	a_fpr _xmm9;
	a_fpr _xmm10;
	a_fpr _xmm11;
	a_fpr _xmm12;
	a_fpr _xmm13;
	a_fpr _xmm14;
	a_fpr _xmm15;
	a_u64 _ssp;
	a_u32 _mxcsr;
	a_u16 _fctrl;
};

#define EXCEPTION_CODE_PREFIX 0xEA4BD800

typedef struct Route {
	GRoute _body;
	RCtx _ctx;
} Route;

intern a_none ai_ctx_jump(Route* from, Route* to);
intern a_msg ai_ctx_swap(Route* from, Route* to);
intern a_none ai_ctx_raise(Route* env, a_msg code);
intern a_msg ai_ctx_catch(Route* env, a_pfun fun, void* ctx);
intern a_msg ai_ctx_open(Route* env, a_usize stack_size);
intern void ai_ctx_close(Route* env);

inline void ai_ctx_swapx(Route* from, Route* to, a_msg msg) {
    register a_gpr p1 asm("rcx");
    register a_gpr p2 asm("rdx");
    register a_gpr p3 asm("rax");
    p1 = bcast(a_gpr, from);
    p2 = bcast(a_gpr, to);
    p3 = bcast(a_gpr, cast(a_isize, msg));
    asm("call %c0"::"p"(ai_ctx_swap), "r"(p1), "r"(p2), "r"(p3));
	unreachable();
}