/**
 *@file actx/x64.sysv.posix.h
 */

#ifndef actx_h_
# error must include 'actx_h_'
#endif

#if ALO_OS_POSIX && ALO_ARCH_X64

#define actx_x64_sysv_posix_h_
#define actx_impl_h_

#include <sys/mman.h>
#include <setjmp.h>

typedef struct JStub JStub;

struct PCtx {
	JStub* _stub;
};

struct JStub {
	jmp_buf _jbuf;
	JStub* _prev;
};

/* Basic stack size alignment. */
#define PAGE_SIZE usizec(0x1000)

#endif