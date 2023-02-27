/**
 *@file actx.h
 */

#ifndef actx_h_
#define actx_h_

#include "aarch.h"
#include "agbl.h"

typedef struct RCtx RCtx;
typedef struct Route Route;

#if ALO_OS_WINDOWS && ALO_ARCH_X64
# include "actx/x64.win.h"
#else
# error unsupported architecture.
#endif

intern a_msg ai_ctx_init(void);
intern a_none ai_ctx_jump(Route* from, Route* to);
intern a_msg ai_ctx_swap(Route* from, Route* to);
intern void ai_ctx_swapx(Route* from, Route* to, a_msg msg);
intern a_none ai_ctx_raise(Route* env, a_msg code);
intern a_msg ai_ctx_catch(Route* env, a_pfun fun, void* ctx);
intern a_msg ai_ctx_open(Route* env, a_usize stack_size);
intern void ai_ctx_close(Route* env);

/* Page allocation functions. */

intern void* ai_mem_nreserve(void* addr, a_usize size);
intern void* ai_mem_ncommit(void* addr, a_usize size, a_flags prot);
intern a_bool ai_mem_ndecommit(void* addr, a_usize size);
intern a_bool ai_mem_nrelease(void* addr, a_usize size);

#endif /* actx_h_ */
