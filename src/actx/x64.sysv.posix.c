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

a_none ai_ctx_raise(a_henv env, a_msg msg) {
    Global* gbl = G(env);
    if (gbl->_gexecpt != null) {
        (*gbl->_gexecpt)(env, gbl->_gctx, msg);
    }
    if (env->_pctx._stub != null) {
        longjmp(env->_pctx._stub->_jbuf, msg);
    }
    else if (env->_from != null) {
        env->_status = msg;
        ai_ctx_jump(env->_from, env, msg);
        trap();
    }
    else {
        a_cfun panic = gbl->_panic;
        if (panic != null) {
            (*panic)(env);
        }
        trap();
    }
}

a_msg ai_ctx_catch(a_henv env, a_pfun pfun, void* pctx) {
    JStub stub = { ._prev = env->_pctx._stub };
    env->_pctx._stub = &stub;
    a_msg msg = setjmp(stub._jbuf);
    if (msg == ALO_SOK) {
        (*pfun)(env, pctx);
    }
    env->_pctx._stub = stub._prev;
    return msg;
}

a_msg ai_ctx_open(a_henv env, a_usize stack_size) {
    a_usize commit = pad_to(stack_size, PAGE_SIZE);

    void* addr = mmap(null, commit, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS|MAP_STACK, -1, 0);
    if (addr == MAP_FAILED) return ALO_ENOMEM;

	void* stack_base = addr + commit;

    cast(void**, stack_base)[0] = null;
    cast(void**, stack_base)[-1] = null;
    cast(void**, stack_base)[-2] = ai_ctx_start;
    env->_rctx = stack_base - sizeof(void*) * 3;
    env->_rctx_alloc = addr;

    return ALO_SOK;
}

void ai_ctx_close(a_henv env) {
    munmap(env->_rctx_alloc, 0);
}

#endif /* actx_x64_sysv_posix_h_ */

