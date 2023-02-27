/**
 *@file actx/x64.w64.win.c
 */

#define actx_x64_w64_win_c_
#define ALO_LIB

#include "../actx.h"
#include "../avm.h"

#define ext(n) M_cat(ext_,n) /* Avoid for type redefinition. */

typedef int NTSTATUS;

#define CTX_NVGPR_LIST(_) \
	_(rbx,Rbx) \
	_(rsp,Rsp) \
	_(rbp,Rbp) \
	_(rsi,Rsi) \
	_(rdi,Rdi) \
	_(r12,R12) \
	_(r13,R13) \
	_(r14,R14) \
	_(r15,R15)
#define CTX_NVFPR_LIST(_) \
	_(xmm6,Xmm6) \
	_(xmm7,Xmm7) \
	_(xmm8,Xmm8) \
	_(xmm9,Xmm9) \
	_(xmm10,Xmm10) \
	_(xmm11,Xmm11) \
	_(xmm12,Xmm12) \
	_(xmm13,Xmm13) \
	_(xmm14,Xmm14) \
	_(xmm15,Xmm15)

typedef struct ext(TEB) ext(TEB), *ext(PTEB);
typedef struct ext(PEB) ext(PEB), *ext(PPEB);
typedef struct ext(PROCESS_STACK_ALLOCATION_INFORMATION)
	ext(PROCESS_STACK_ALLOCATION_INFORMATION),
	*ext(PPROCESS_STACK_ALLOCATION_INFORMATION);

struct ext(TEB) {
	/* 0x0000 */ union {
		NT_TIB NtTib;
		struct {
			PEXCEPTION_REGISTRATION_RECORD ExceptionList;
      		PVOID StackBase;
      		PVOID StackLimit;
      		PVOID SubSystemTib;
      		union {
				PVOID FiberData;
				DWORD Version;
      		};
      		PVOID ArbitraryUserPointer;
      		PNT_TIB Self;
		};
	};
	/* 0x0038 */ PVOID _reserved1[5];
	/* 0x0060 */ ext(PPEB) ProcessEnvironmentBlock;
	/* 0x0068 */ PVOID _reserved2[642];
	/* 0x1478 */ PVOID DeallocationStack;
};

struct ext(PEB) {
	/* 0x0000 */ BYTE _reserved1[16];
	/* 0x0010 */ PVOID ImageBaseAddress;
};

#undef WINSYSAPI
#define WINSYSAPI __declspec(dllimport)

struct ext(PROCESS_STACK_ALLOCATION_INFORMATION) {
	SIZE_T ReserveSize;
	SIZE_T ZeroBits;
	PVOID StackBase;
};

typedef enum ext(PROCESS_INFORMATION_CLASS) {
	ProcessThreadStackAllocation = 41
} ext(PROCESS_INFORMATION_CLASS);

#define IMPORT_LIST(_) \
	_(NtSetInformationProcess, hntdll, NTSTATUS, (HANDLE, ext(PROCESS_INFORMATION_CLASS), PVOID, ULONG)) \
	_(NtRaiseException, hntdll, VOID, (PEXCEPTION_RECORD, PCONTEXT, BOOLEAN)) \
	_(RtlLookupFunctionEntry, hntdll, PRUNTIME_FUNCTION, (DWORD64, PDWORD64, PUNWIND_HISTORY_TABLE)) \
	_(RtlVirtualUnwind, hntdll, PEXCEPTION_ROUTINE, (DWORD, DWORD64, DWORD64, PRUNTIME_FUNCTION, PCONTEXT, PVOID*, PDWORD64, PKNONVOLATILE_CONTEXT_POINTERS)) \
	_(RtlImageNtHeader, hntdll, PIMAGE_NT_HEADERS, (PVOID))

#undef NtSetInformationProcess
#undef NtRaiseException
#undef RtlLookupFunctionEntry
#undef RtlVirtualUnwind
#undef RtlImageNtHeader

#if defined(ALOI_WIN_DYNAMIC_INIT)

WINSYSAPI HMODULE WINAPI GetModuleHandleA(LPCSTR);
WINSYSAPI FARPROC WINAPI GetProcAddress(HANDLE, LPCSTR);

#define CTX_DCL(name,owner,rtype,ptypes) static rtype (*WINAPI ext(name)) ptypes;
IMPORT_LIST(CTX_DCL)
#undef CTX_DCL

#define NtSetInformationProcess (*ext(NtSetInformationProcess))
#define NtRaiseException (*ext(NtRaiseException))
#define RtlLookupFunctionEntry (*ext(RtlLookupFunctionEntry))
#define RtlVirtualUnwind (*ext(RtlVirtualUnwind))
#define RtlImageNtHeader (*ext(RtlImageNtHeader))

#else

#define CTX_DCL(name,owner,rtype,ptypes) WINSYSAPI rtype WINAPI name ptypes;
IMPORT_LIST(CTX_DCL)
#undef CTX_DCL

#endif

#define NtCurrentProcess() cast(HANDLE, isizec(-1))
#define NtCurrentTeb() cast(ext(PTEB), __readgsqword(offsetof(ext(TEB), Self)))

WINSYSAPI void WINAPI RaiseException(DWORD, DWORD, DWORD, ULONG_PTR const*);

a_msg ai_ctx_init(void) {
#if defined(ALOI_WIN_DYNAMIC_INIT)
	HMODULE hntdll = GetModuleHandleA("ntdll.dll");
	if (hntdll == null) return ALO_EOUTER;

#define CTX_GET(name,mod,...) run { \
	void* _addr = cast(void*, GetProcAddress(mod, #name)); \
	if (_addr == null) return ALO_EOUTER; \
	ext(name) = _addr; \
}

	IMPORT_LIST(CTX_GET)
#undef CTX_GET

#endif

	return ALO_SOK;
}

#define PROC_NULL null

#define L_ENTER ".long_enter"

#define Fenv(f) "i"(addr_of(&cast(GRoute*, null)->f))
#define Fctx(f) "i"(addr_of(&cast(Route*, null)->_ctx.f))
#define Fgbl(f) "i"(addr_of(&cast(Global*, null)->f))
#define Fteb(f) "i"(addr_of(&cast(ext(PTEB), null)->f))

naked a_none ai_ctx_jump(unused Route* from, unused Route* to) {
	asm("jmp "L_ENTER);
}

naked a_msg ai_ctx_swap(unused Route* from, unused Route* to) {
	/* %rcx = caller, %rdx = callee */
	/* Save TEB context. */
	asm("movq %%gs:%c0, %%r8"::Fteb(DeallocationStack));
	asm("movq %%r8, %c0(%%rcx)"::Fctx(_stack_alloc_base)); /* callee->_stack_alloc_base = TEB->DeallocationStack */
	asm("movq %%gs:%c0, %%r8"::Fteb(StackBase));
	asm("movq %%r8, %c0(%%rcx)"::Fctx(_stack_base)); /* callee->_stack_base = TIB->StackBase */
	asm("movq %%gs:%c0, %%r8"::Fteb(ExceptionList));
	asm("movq %%r8, %c0(%%rcx)"::Fctx(_except_list)); /* callee->_except_list = TIB->ExceptionList */
	asm("movq %%gs:%c0, %%r8"::Fteb(StackLimit));
	asm("movq %%r8, %c0(%%rcx)"::Fctx(_stack_limit)); /* callee->_stack_limit = TIB->StackLimit */
	/* Save nonvolatile registers. */
	asm("movq (%rsp), %r8"); /* Virtual return. */
	asm("movq %%r8, %c0(%%rcx)"::Fctx(_rip));
	asm("addq $8, %rsp");
#define SAVE(n,f) asm("movq %%"#n", %c0(%%rcx)"::Fctx(_##n));
	CTX_NVGPR_LIST(SAVE) /* Save GPR */
#undef SAVE
#define SAVE(n,f) asm("movups %%"#n", %c0(%%rcx)"::Fctx(_##n));
	CTX_NVFPR_LIST(SAVE) /* Save FPR */
#undef SAVE
	asm("stmxcsr %c0(%%rcx)"::Fctx(_mxcsr));
	asm("fnclex");
	asm("fstcww %c0(%%rcx)"::Fctx(_fctrl));
	/* Enter into route. */
	asm(L_ENTER":");

	/* Check shadow pointer. */
	asm("movq %c0(%%rdx), %%r8"::Fctx(_ssp));
	asm("testq %r8, %r8");
	asm("je ._label1");
	asm("rdsspq %rbx");
	asm("rstorssp (%r8)");
	asm("movq %%rbx, %c0(%%rdx)"::Fctx(_ssp));
	asm("saveprevssp");
	asm("subq $-8, %rbx");
	asm("movq %%rbx, %c0(%%rcx)"::Fctx(_ssp));
	/* Load TEB context. */
	asm("._label1:");
	asm("movq %c0(%%rdx), %%r8"::Fctx(_stack_alloc_base));
	asm("movq %%r8, %%gs:%c0"::Fteb(DeallocationStack)); /* TEB->DeallocationStack = callee->_stack_alloc_base */
	asm("movq %c0(%%rdx), %%r8"::Fctx(_stack_base));
	asm("movq %%r8, %%gs:%c0"::Fteb(StackBase)); /* TIB->StackBase = callee->_stack_base */
	asm("movq %c0(%%rdx), %%r8"::Fctx(_stack_limit));
	asm("movq %%r8, %%gs:%c0"::Fteb(StackLimit)); /* TIB->StackLimit = callee->_stack_limit */
	asm("movq %c0(%%rdx), %%r8"::Fctx(_except_list));
	asm("movq %%r8, %%gs:%c0"::Fteb(ExceptionList)); /* TIB->ExceptionList = callee->_except_list */
	/* Load nonvolatile registers. */
#define LOAD(n,f) asm("movq %c0(%%rdx), %%"#n::Fctx(_##n));
	CTX_NVGPR_LIST(LOAD) /* Load GPR */
#undef LOAD
#define LOAD(n,f) asm("movups %c0(%%rdx), %%"#n::Fctx(_##n));
	CTX_NVFPR_LIST(LOAD) /* Load FPR */
#undef LOAD
	asm("ldmxcsr %c0(%%rdx)"::Fctx(_mxcsr));
	asm("fldcw %c0(%%rdx)"::Fctx(_fctrl));
	/* Switch active route. */
	asm("movq %c0(%%rdx), %%r8"::Fenv(_g));
	asm("movq %%rdx, %c0(%%r8)"::Fgbl(_active)); /* g->_active = callee */
	/* Jump. */
	asm("jmp *%c0(%%rdx)"::Fctx(_rip));
}

always_inline a_msg code2msg(DWORD code) {
	DWORD raw_msg = code ^ EXCEPTION_CODE_PREFIX;
	assume(raw_msg > 0);
	return raw_msg <= 0xff ? cast(a_msg, cast(a_i8, raw_msg)) : ALO_EOUTER;
}

static EXCEPTION_DISPOSITION start_except_hook(
		_In_ PEXCEPTION_RECORD ExceptionRecord,
		_In_ unused PVOID EstablisherFrame,
		_Inout_ PCONTEXT ContextRecord,
		_Inout_ PDISPATCHER_CONTEXT unused DispatcherContext) {
	Route* callee = ptr_of(Route, ContextRecord->Rbx);
	Route* caller = cast(Route*, callee->_body._from);

	if (!(ExceptionRecord->ExceptionFlags & (EXCEPTION_UNWINDING | EXCEPTION_EXIT_UNWIND))) {
		a_msg msg = code2msg(ExceptionRecord->ExceptionCode);
		if (msg != ALO_SOK) {
			asm volatile("jmp "L_ENTER:: "a"(msg), "c"(callee), "d"(caller));
		}
	}
 
	RCtx* ctx = &caller->_ctx;
#define LOAD(n,f) ContextRecord->f = ctx->_##n;
	CTX_NVGPR_LIST(LOAD)
#undef LOAD
#define LOAD(n,f) ContextRecord->f = ctx->_##n._win;
	CTX_NVFPR_LIST(LOAD)
#undef LOAD
	ContextRecord->Rip = ctx->_rip;
	ContextRecord->MxCsr = ctx->_mxcsr;
	ContextRecord->FltSave.ControlWord = ctx->_fctrl;

	ext(PTEB) teb = NtCurrentTeb();
	teb->NtTib.StackBase = ctx->_stack_base;
	teb->NtTib.StackLimit = ctx->_stack_limit;
	teb->NtTib.ExceptionList = ctx->_except_list;
	teb->DeallocationStack = ctx->_stack_alloc_base;

	NtRaiseException(ExceptionRecord, ContextRecord, TRUE);
	trap();
}

static never_inline void rmain(a_henv env) {
	assume(env->_stack._base == env->_stack._bot);
	ai_vm_call(env, env->_stack._base, new(RFlags) {
		._count = RFLAG_COUNT_VARARG
	});
}

static naked a_none rstart(void) {
	/* %rdx = callee */
	asm(".seh_handler %c0, @except"::"p"(start_except_hook));
	asm("movq %rdx, %rbx");
	asm("subq $0x8, %rsp");
	asm("call %c0"::"p"(rmain));
	asm("addq $0x8, %rsp");
	asm("movq %rbx, %rcx");
	asm("movq %c0(%%rbx), %%rdx"::"i"(offsetof(Route, _body._from)));
	asm("jmp "L_ENTER);
}

a_none ai_ctx_raise(unused Route* env, a_msg msg) {
	RaiseException(EXCEPTION_CODE_PREFIX | cast(DWORD, msg & 0xff), EXCEPTION_NONCONTINUABLE, 0, null);
	unreachable();
}

static EXCEPTION_DISPOSITION catch_except_hook(
		PEXCEPTION_RECORD ExceptionRecord,
		PVOID EstablisherFrame,
		PCONTEXT ContextRecord,
		PDISPATCHER_CONTEXT DispatcherContext) {
	a_msg msg = code2msg(ExceptionRecord->ExceptionCode);
	if (msg == ALO_SOK) return EXCEPTION_CONTINUE_SEARCH;

	a_henv env = &ptr_of(Route, ContextRecord->Rsi)->_body;
	Global* g = G(env);
	if (g->_gexecpt != null) {
		(*g->_gexecpt)(env, g->_gprotect_ctx, msg);
	}

	a_u32* prva = DispatcherContext->HandlerData;
	a_usize pc = DispatcherContext->ImageBase + *prva;
	RtlUnwind(EstablisherFrame, cast(PVOID, pc), ExceptionRecord, cast(PVOID, cast(a_isize, msg)));
	unreachable();
}

never_inline a_msg ai_ctx_catch(Route* env, a_pfun pfun, void* pctx) {
	asm(".seh_handler %c0, @except"::"p"(catch_except_hook));
	asm goto(
		".seh_handlerdata\n"
		".rva %c0\n"
		".long 0\n"
		".text"::::end_try);
	a_msg msg = ALO_SOK;

	asm("movq %0, %%rsi"::"r"(env):"rsi");
	(*pfun)(&env->_body, pctx);

	asm(""::"a"(msg));

end_try:
	asm volatile("":"=a"(msg));
	return msg;
}

a_msg ai_ctx_open(Route* route, a_usize stack_size) {
	PIMAGE_NT_HEADERS header = RtlImageNtHeader(NtCurrentTeb()->ProcessEnvironmentBlock->ImageBaseAddress);
	if (header == null) return ALO_EINVAL;

	a_usize commit = stack_size;
	a_usize reserve = header->OptionalHeader.SizeOfStackReserve;

	commit = (commit + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1);

	ext(PROCESS_STACK_ALLOCATION_INFORMATION) alloc_info = {
		.ReserveSize = max(reserve, commit),
		.ZeroBits = 0,
		.StackBase = null
	};

	NTSTATUS status = NtSetInformationProcess(NtCurrentProcess(), ProcessThreadStackAllocation, &alloc_info, sizeof(alloc_info));
	if (status != 0) return ALO_EINVAL;

	PVOID addr = alloc_info.StackBase;

	if (VirtualAlloc(addr, PAGE_SIZE, MEM_COMMIT, PAGE_NOACCESS) == null)
		return ALO_ENOMEM;
	if (VirtualAlloc(addr + PAGE_SIZE, PAGE_SIZE, MEM_COMMIT, PAGE_READWRITE | PAGE_GUARD) == null)
		return ALO_ENOMEM;
	if (VirtualAlloc(addr + PAGE_SIZE * 2, reserve - 2 * PAGE_SIZE, MEM_COMMIT, PAGE_READWRITE) == null)
		return ALO_ENOMEM;

	void* stack_base = alloc_info.StackBase + reserve;
	route->_ctx = new(RCtx) {
		._stack_alloc_base = alloc_info.StackBase,
		._stack_base = stack_base,
		._stack_limit = alloc_info.StackBase + 2 * PAGE_SIZE,
		._except_list = cast(void*, isizec(-1)),
		._rsp = cast(a_gpr, stack_base - 0x8),
		._rip = cast(a_gpr, rstart)
	};

	*cast(void**, route->_ctx._rsp) = PROC_NULL;

	return ALO_SOK;
}

static void l_unwind(Route* route) {
    DWORD64 EstablisherFrame;
    DWORD64 ImageBase;
	/* Load context. */
	CONTEXT Context = { };
#define CAPTURE(n,f) Context.f = route->_ctx._##n;
	CTX_NVGPR_LIST(CAPTURE)
#undef CAPTURE
#define CAPTURE(n,f) Context.f = route->_ctx._##n._win;
	CTX_NVFPR_LIST(CAPTURE)
#undef CAPTURE
	Context.MxCsr = route->_ctx._mxcsr;
	Context.FltSave.ControlWord = route->_ctx._fctrl;
	/* Initialize unwind history table. */
	UNWIND_HISTORY_TABLE UnwindHistoryTable = {};
	
	loop {
    	PRUNTIME_FUNCTION RuntimeFunction = RtlLookupFunctionEntry(
			Context.Rip, 
			&ImageBase, 
			&UnwindHistoryTable);
		KNONVOLATILE_CONTEXT_POINTERS NvContext = {};

		if (RuntimeFunction == null) {
			/* Unwinding leaf node. */
			Context.Rip = *ptr_of(DWORD64, Context.Rsp);
			Context.Rsp += sizeof(DWORD64);
		}
		else {
   			PVOID HandlerData;
			/* Unwinding branch node. */
			RtlVirtualUnwind(
				UNW_FLAG_NHANDLER,
				ImageBase,
				Context.Rip,
				RuntimeFunction,
				&Context,
				&HandlerData,
				&EstablisherFrame,
				&NvContext);
		}

		/* Stop if unwind to end. */
		if (cast(PVOID, Context.Rip) == PROC_NULL) break;
	}
}

void ai_ctx_close(Route* route) {
	l_unwind(route);
	VirtualFree(route->_ctx._stack_alloc_base, 0, MEM_RELEASE);
}

void* ai_mem_nreserve(void* addr, a_usize size) {
	return VirtualAlloc(addr, size, MEM_RESERVE, PAGE_NOACCESS);
}

void* ai_mem_ncommit(void* addr, a_usize size, a_flags prot) {
	return VirtualAlloc(addr, size, MEM_COMMIT, prot);
}

a_bool ai_mem_ndecommit(void* addr, a_usize size) {
	return VirtualFree(addr, size, MEM_DECOMMIT);
}

a_bool ai_mem_nrelease(void* addr, unused a_usize size) {
	return VirtualFree(addr, 0, MEM_RELEASE);
}
