/**
 *@file actx/x64.w64.win.c
 */

#include "../actx.h"

#ifdef actx_x64_w64_win_h_

#define actx_x64_w64_win_c_
#define ALO_LIB

#include <memoryapi.h>
#include <intrin.h>

#include "../aobj.h"
#include "../aenv.h"
#include "../aerr.h"

#define ext(n) M_cat(ext_,n) /* Avoid for type redefinition. */

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
	ext(dbg_name) = _addr; \
}

	IMPORT_LIST(CTX_GET)
#undef CTX_GET

#endif

	return ALO_SOK;
}

static a_msg code2msg(DWORD code) {
	DWORD raw_msg = code ^ EXCEPTION_CODE_PREFIX;
	assume(raw_msg > 0);
	return raw_msg <= 0xff ? cast(a_msg, cast(a_i8, raw_msg)) : ALO_SOK;
}

static void route_recover(void* rctx, PCONTEXT context, a_bool recover_stack) {
    a_usize frame = cast(a_usize, rctx);

    context->Xmm6 = ref_of(M128A, frame - 0x078);
    context->Xmm7 = ref_of(M128A, frame - 0x088);
    context->Xmm8 = ref_of(M128A, frame - 0x098);
    context->Xmm9 = ref_of(M128A, frame - 0x0A8);
    context->Xmm10 = ref_of(M128A, frame - 0x0B8);
    context->Xmm11 = ref_of(M128A, frame - 0x0C8);
    context->Xmm12 = ref_of(M128A, frame - 0x0D8);
    context->Xmm13 = ref_of(M128A, frame - 0x0E8);
    context->Xmm14 = ref_of(M128A, frame - 0x0F8);
    context->Xmm15 = ref_of(M128A, frame - 0x108);
    context->Rbp = ref_of(DWORD64, frame - 0x028);
    context->Rsp = frame + 0x008;
    context->Rbx = ref_of(DWORD64, frame - 0x030);
    context->Rsi = ref_of(DWORD64, frame - 0x038);
    context->Rdi = ref_of(DWORD64, frame - 0x040);
    context->R12 = ref_of(DWORD64, frame - 0x048);
    context->R13 = ref_of(DWORD64, frame - 0x050);
    context->R14 = ref_of(DWORD64, frame - 0x058);
    context->R15 = ref_of(DWORD64, frame - 0x060);
    context->Rip = ref_of(DWORD64, frame);
    context->MxCsr = ref_of(DWORD, frame - 0x068);
    context->FltSave.ControlWord = ref_of(WORD, frame - 0x06C);

    if (recover_stack) {
        ext(PTEB) teb = NtCurrentTeb();
        teb->NtTib.StackBase = ref_of(PVOID, frame - 0x020);
        teb->NtTib.StackLimit = ref_of(PVOID, frame - 0x018);
        teb->DeallocationStack = ref_of(PVOID, frame - 0x010);
    }
}

EXCEPTION_DISPOSITION ai_ctx_start_catch(
		_In_ PEXCEPTION_RECORD ExceptionRecord,
		_In_ unused PVOID EstablisherFrame,
		_Inout_ PCONTEXT ContextRecord,
		_Inout_ unused PDISPATCHER_CONTEXT DispatcherContext) {
	a_henv callee = cast(a_henv, ContextRecord->Rbx);
	a_henv caller = callee->caller;

	if (!(ExceptionRecord->ExceptionFlags & (EXCEPTION_UNWINDING | EXCEPTION_EXIT_UNWIND))) {
        a_msg msg = code2msg(ExceptionRecord->ExceptionCode);
        if (msg != ALO_SOK) {
            callee->status = msg;
            ai_ctx_jump(caller, callee, msg);
        }
    }

	route_recover(caller->frame, ContextRecord, TRUE);
	NtRaiseException(ExceptionRecord, ContextRecord, TRUE);
	trap();
}

a_noret ai_ctx_raise(unused a_henv env, a_msg msg) {
	RaiseException(EXCEPTION_CODE_PREFIX | cast(DWORD, msg & 0xff), EXCEPTION_NONCONTINUABLE, 0, null);
	unreachable();
}

static EXCEPTION_DISPOSITION catch_except_hook(
		PEXCEPTION_RECORD ExceptionRecord,
		unused PVOID EstablisherFrame,
		unused PCONTEXT ContextRecord,
		PDISPATCHER_CONTEXT DispatcherContext) {
	a_msg msg = code2msg(ExceptionRecord->ExceptionCode);
	if (msg == ALO_SOK) return EXCEPTION_CONTINUE_SEARCH;

	a_henv env = int2ptr(GRoute, DispatcherContext->ContextRecord->Rsi);
    ai_err_except(env, msg);

	a_usize target_ip = DispatcherContext->ImageBase + *((a_u32*) DispatcherContext->HandlerData);

	RtlUnwind(
			EstablisherFrame,
			cast(PVOID, target_ip),
			ExceptionRecord,
			cast(PVOID, cast(a_isize, msg)));
	unreachable();
}

never_inline a_msg ai_ctx_catch_(a_henv env, a_pfun pfun, void* pctx) {
	asm(".seh_handler %c0, @except"::"p"(catch_except_hook));
	asm goto(".seh_handlerdata\n"
		".rva .final\n"
		".text"::::final);
	a_msg msg = ALO_SOK;

	(*pfun)(env, pctx);

	asm(""::"a"(msg));
final:
	asm volatile(".final:":"=a"(msg));

	return msg;
}

a_msg ai_ctx_open(a_henv env, a_usize stack_size) {
	PIMAGE_NT_HEADERS header = RtlImageNtHeader(NtCurrentTeb()->ProcessEnvironmentBlock->ImageBaseAddress);
	if (header == null) return ALO_EINVAL;

	a_usize commit = align_to(stack_size, PAGE_SIZE);
	a_usize reserve = header->OptionalHeader.SizeOfStackReserve;

	PVOID addr;
#ifndef ALOI_WIN_NO_USE_UNDOCUMENT_API
	ext(PROCESS_STACK_ALLOCATION_INFORMATION) alloc_info = {
		.ReserveSize = max(reserve, commit),
		.ZeroBits = 0,
		.StackBase = null
	};

	NTSTATUS status = NtSetInformationProcess(NtCurrentProcess(), ProcessThreadStackAllocation, &alloc_info, sizeof(alloc_info));
	if (status != 0) return ALO_EINVAL;

	addr = alloc_info.StackBase;
#else
	addr = VirtualAlloc(NULL, reserve, MEM_RESERVE, PAGE_NOACCESS);
    if (addr == null) return ALO_EINVAL;
#endif

	if (VirtualAlloc(addr, PAGE_SIZE, MEM_COMMIT, PAGE_NOACCESS) == null)
		goto nomem;
	if (VirtualAlloc(addr + PAGE_SIZE, PAGE_SIZE, MEM_COMMIT, PAGE_READWRITE | PAGE_GUARD) == null)
		goto nomem;
	if (VirtualAlloc(addr + PAGE_SIZE * 2, reserve - 2 * PAGE_SIZE, MEM_COMMIT, PAGE_READWRITE) == null)
		goto nomem;

	a_usize stack_base = ptr2int(alloc_info.StackBase) + reserve;

    ref_of(void*, stack_base - 0x00) = null;
    ref_of(void*, stack_base - 0x08) = null;
    ref_of(void*, stack_base - 0x10) = ai_ctx_start;
    env->rctx = int2ptr(void, stack_base - 0x18);
	env->rctx_alloc = addr;

	return ALO_SOK;

nomem:
	VirtualFree(addr, 0, MEM_RELEASE);
	return ALO_ENOMEM;
}

static void route_unwind(a_henv env) {
    DWORD64 EstablisherFrame;
    DWORD64 ImageBase;
	/* Load context. */
	CONTEXT Context = { };

	route_recover(env->rctx, &Context, FALSE);

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
			Context.Rip = ref_of(DWORD64, Context.Rsp);
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
		if (Context.Rip == 0)
			break;
	}
}

void ai_ctx_close(a_henv env) {
	route_unwind(env);
	VirtualFree(env->rctx_alloc, 0, MEM_RELEASE);
}

#endif /* actx_x64_w64_win_h_ */