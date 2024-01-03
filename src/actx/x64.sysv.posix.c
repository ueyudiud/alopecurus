/**
 *@file actx/x64.sysv.posix.c
 */

#include "../actx.h"

#ifdef actx_x64_sysv_posix_h_

#define actx_x64_sysv_posix_c_
#define ALO_LIB

#include <fcntl.h>

#include "../aenv.h"

a_msg ai_ctx_init(void) {
    return ALO_SOK;
}

a_noret ai_ctx_raise(a_henv env, a_msg msg) {
    Global* gbl = G(env);
    if (env->pctx.stub != null) {
        longjmp(env->pctx.stub->jbuf, msg);
    }
    else if (env->caller != null) {
        env->status = msg;
        ai_ctx_jump(env->caller, env, msg);
        trap();
    }
    else {
        a_cfun panic = gbl->panic_;
        if (panic != null) {
            (*panic)(env);
        }
        trap();
    }
}

a_msg ai_ctx_catch(a_henv env, a_pfun pfun, void* pctx) {
    JStub stub = { .prev = env->pctx.stub };
    env->pctx.stub = &stub;
    a_msg msg = setjmp(stub.jbuf);
    if (msg == ALO_SOK) {
        (*pfun)(env, pctx);
    }
    env->pctx.stub = stub.prev;
    return msg;
}

a_msg ai_ctx_open(a_henv env, a_usize stack_size) {
    a_usize commit = pad_to(stack_size, PAGE_SIZE);

    void* addr = mmap(null, commit, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS|MAP_STACK, -1, 0);
    if (addr == MAP_FAILED) return ALO_ENOMEM;

	a_usize stack_base = ptr2int(addr + commit);

    ref_of(void*, stack_base - 0x00) = null;
    ref_of(void*, stack_base - 0x08) = null;
    ref_of(void*, stack_base - 0x10) = ai_ctx_start;
    env->rctx = int2ptr(void, stack_base - 0x18);
    env->rctx_alloc = addr;

    return ALO_SOK;
}

void ai_ctx_close(a_henv env) {
    munmap(env->rctx_alloc, 0);
}

#endif /* actx_x64_sysv_posix_h_ */

