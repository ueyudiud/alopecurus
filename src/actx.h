/**
 *@file actx.h
 */

#ifndef actx_h_
#define actx_h_

#include "aarch.h"
#include "adef.h"

typedef struct PCtx PCtx;

intern a_msg ai_ctx_init(void);
intern a_noret ai_ctx_start();
intern a_msg ai_ctx_jump(a_henv to, a_henv from, a_msg msg);
intern a_noret ai_ctx_raise(a_henv env, a_msg msg);
intern a_msg ai_ctx_catch(a_henv env, a_pfun fun, void* ctx);
intern a_msg ai_ctx_open(a_henv env, a_usize stack_size);
intern void ai_ctx_close(a_henv env);

#include "actx/x64.w64.win.h"
#include "actx/x64.sysv.posix.h"

#ifndef actx_impl_h_
# error unsupport architecture.
#endif

#endif /* actx_h_ */
