/**
 *@file actx.h
 */

#ifndef actx_h_
#define actx_h_

#include "aarch.h"

typedef struct RCtx RCtx;

#if ALO_OS_WINDOWS && ALO_ARCH_X64
# include "actx.x64.win.h"
#else
# error unsupported architecture.
#endif

#include "adef.h"

intern a_msg ai_ctx_init(void);

#endif /* actx_h_ */
