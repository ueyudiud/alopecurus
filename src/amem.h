/*
 * amem.h
 */

#ifndef amem_h_
#define amem_h_

#include "aobj.h"

intern a_noret ai_mem_nomem(a_henv env);

intern void* ai_mem_alloc(a_henv env, a_usize sz);
intern void* ai_mem_realloc(a_henv env, void* blk_old, a_usize sz_old, a_usize sz_new);
intern void ai_mem_dealloc(Global* gbl, void* blk, a_usize sz);

intern void* ai_mem_nalloc(a_henv env, a_usize sz);
intern void* ai_mem_nrealloc(a_henv env, void* blk_old, a_usize sz_old, a_usize sz_new);
intern void ai_mem_ndealloc(Global* gbl, void* blk, a_usize sz);

always_inline void* ai_mem_valloc(alo_Alloc const* af, void* ac, a_usize sz) {
	assume(sz > 0);
	return (*af->allocate)(ac, sz);
}

always_inline void* ai_mem_vrealloc(alo_Alloc const* af, void* ac, void* blk_old, a_usize sz_old, a_usize sz_new) {
	assume(blk_old != null && sz_old > 0);
	assume(sz_new > 0);
	return (*af->reallocate)(ac, blk_old, sz_old, sz_new);
}

always_inline void ai_mem_vdealloc(alo_Alloc const* af, void* ac, void* blk, a_usize sz) {
	assume(blk != null && sz > 0);
	(*af->deallocate)(ac, blk, sz);
}

#define ai_mem_vnnew(env,type,size) cast(typeof(type)*, ai_mem_nalloc(env, sizeof(type) * (size)))
#define ai_mem_vngrow(env,vec,size_old,size_new) cast(typeof(vec), ai_mem_nrealloc(env, vec, sizeof((vec)[0]) * (size_old), sizeof((vec)[0]) * (size_new)))
#define ai_mem_vndel(gbl,vec,size) ai_mem_xdealloc(gbl, vec, sizeof((vec)[0]) * (size))

#define ai_mem_vnew(env,type,size) cast(typeof(type)*, ai_mem_alloc(env, sizeof(type) * (size)))
#define ai_mem_vgrow(env,vec,size_old,size_new) cast(typeof(vec), ai_mem_realloc(env, vec, sizeof((vec)[0]) * (size_old), sizeof((vec)[0]) * (size_new)))
#define ai_mem_vdel(gbl,vec,size) ai_mem_dealloc(gbl, vec, sizeof((vec)[0]) * (size))

#define ai_mem_gnew(env,type,size,bias...) g_cast(type, g_biased(ai_mem_alloc(env, sizeof(GcHead) + (size)), ##bias))
#define ai_mem_gdel(gbl,obj,size,bias...) ai_mem_dealloc(gbl, g_unbiased(obj, ##bias), sizeof(GcHead) + (size))

/* Direct memory support. */

#if ALO_OS_WINDOWS

#include <memoryapi.h>

#define NCOMMIT_PROT_READWRITE PAGE_READWRITE
#define NCOMMIT_PROT_READEXEC PAGE_EXECUTE_READ

always_inline void* ai_mem_nreserve(void* hint, a_usize size) {
	return VirtualAlloc(hint, size, MEM_RESERVE, PAGE_NOACCESS);
}

always_inline a_bool ai_mem_ncommit(void* base, void* addr, a_usize size, a_flags prot) {
	quiet(base);
	void* addr2 = VirtualAlloc(addr, size, MEM_COMMIT, prot);
	assume(addr2 == addr || addr2 == null, "unexpected result.");
	return addr2 != null;
}

always_inline a_bool ai_mem_nprotect(void* base, void* addr, a_usize size, a_flags prot) {
	quiet(base);
	DWORD old_prot;
	return VirtualProtect(addr, size, prot, &old_prot);
}

always_inline a_bool ai_mem_ndecommit(void* base, void* addr, a_usize size) {
	quiet(base);
	return VirtualFree(addr, size, MEM_DECOMMIT);
}

always_inline a_bool ai_mem_nrelease(void* base, a_usize size) {
	quiet(size);
	return VirtualFree(base, 0, MEM_RELEASE);
}

#elif ALO_OS_POSIX

#include <sys/mman.h>

#if ALO_USE_VALGRIND
# include <valgrind/valgrind.h>
#endif

#define NCOMMIT_PROT_READWRITE (PROT_READ|PROT_WRITE)
#define NCOMMIT_PROT_READEXEC (PROT_READ|PROT_EXEC)

always_inline void* ai_mem_nreserve(void* hint, a_usize size) {
	void* base = mmap(hint, size, PROT_NONE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
#if ALO_USE_VALGRIND
	VALGRIND_CREATE_MEMPOOL(base, false, false);
#endif
	return base;
}

always_inline a_bool ai_mem_ncommit(void* base, void* addr, a_usize size, a_flags prot) {
	quiet(base);
	int ret = mprotect(addr, size, prot);
#if ALO_USE_VALGRIND
	a_usize page_count = size / PAGE_SIZE;
	for (a_usize i = 0; i < page_count; ++i) {
		VALGRIND_MEMPOOL_ALLOC(base, addr + PAGE_SIZE * i, PAGE_SIZE);
	}
#endif
	return ret == 0;
}

always_inline a_bool ai_mem_nprotect(void* base, void* addr, a_usize size, a_flags prot) {
	quiet(base);
	return mprotect(addr, size, prot) == 0;
}

always_inline a_bool ai_mem_ndecommit(void* base, void* addr, a_usize size) {
	quiet(base);
	int ret = mprotect(addr, size, PROT_NONE);
#if ALO_USE_VALGRIND
	a_usize page_count = size / PAGE_SIZE;
	for (a_usize i = 0; i < page_count; ++i) {
		VALGRIND_MEMPOOL_FREE(base, addr + PAGE_SIZE * i);
	}
#endif
	return ret == 0;
}

always_inline a_bool ai_mem_nrelease(void* base, a_usize size) {
	int ret = munmap(base, size);
#if ALO_USE_VALGRIND
	VALGRIND_DESTROY_MEMPOOL(base);
#endif
    return ret == 0;
}

#else

#define NCOMMIT_PROT_READWRITE 0
#define NCOMMIT_PROT_READEXEC 0

always_inline void* ai_mem_nreserve(unused void* hint, a_usize size) {
	return malloc(size);
}

always_inline a_bool ai_mem_ncommit(unused void* base, unused void* addr, unused a_usize size, unused a_flags prot) {
	return true;
}

always_inline a_bool ai_mem_nprotect(unused void* base, unused void* addr, unused a_usize size, unused a_flags prot) {
	return true;
}

always_inline a_bool ai_mem_ndecommit(unused void* base, unused void* addr, unused a_usize size) {
	return true;
}

always_inline a_bool ai_mem_nrelease(void* base, unused a_usize size) {
	free(base);
	return true;
}

#endif

#endif /* amem_h_ */
