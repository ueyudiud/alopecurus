/**
 *@file actx/x64.w64.win.c
 */

#define actx_c_
#define actx_x64_w64_win_c_
#define ALO_LIB

#include "../actx.h"

#ifdef actx_x64_w64_win_h_

#include <windows.h>

#include "../aobj.h"
#include "../aenv.h"
#include "../avm.h"
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
	_(RtlImageNtHeader, hntdll, PIMAGE_NT_HEADERS, (PVOID))

#undef NtSetInformationProcess
#undef NtRaiseException
#undef RtlImageNtHeader

#if defined(ALOI_WIN_DYNAMIC_INIT)

WINSYSAPI HMODULE WINAPI GetModuleHandleA(LPCSTR);
WINSYSAPI FARPROC WINAPI GetProcAddress(HANDLE, LPCSTR);

#define CTX_DCL(name,owner,rtype,ptypes) static rtype (*WINAPI ext(name)) ptypes;
IMPORT_LIST(CTX_DCL)
#undef CTX_DCL

#define NtSetInformationProcess (*ext(NtSetInformationProcess))
#define NtRaiseException (*ext(NtRaiseException))
#define RtlImageNtHeader (*ext(RtlImageNtHeader))

#else

#define CTX_DCL(name,owner,rtype,ptypes) WINSYSAPI rtype WINAPI name ptypes;
IMPORT_LIST(CTX_DCL)
#undef CTX_DCL

#endif

#define NtCurrentProcess() cast(HANDLE, isizec(-1))

#undef NtCurrentTeb
always_inline ext(TEB)* ext(NtCurrentTeb)() {
	ext(TEB)* teb;
	asm("movq %%gs:(%c1), %0":"=r"(teb):"i"(offsetof(ext(TEB), Self)));
	return teb;
}
#define NtCurrentTeb ext(NtCurrentTeb)

static naked void ai_ctx_catch_unwind();
static naked a_msg ai_ctx_return();

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
		_In_ PVOID EstablisherFrame,
		_Inout_ PCONTEXT ContextRecord,
		_Inout_ PDISPATCHER_CONTEXT DispatcherContext) {
	quiet(EstablisherFrame);
	quiet(DispatcherContext);

	a_henv callee = cast(a_henv, ContextRecord->Rbx);
	a_henv caller = callee->caller;

	if (!(ExceptionRecord->ExceptionFlags & (EXCEPTION_UNWINDING | EXCEPTION_EXIT_UNWIND))) {
        a_msg msg = code2msg(ExceptionRecord->ExceptionCode);
        if (msg != ALO_SOK) {
            callee->status = cast(a_i8, msg);
            ai_ctx_jump(caller, callee, msg);
        }
    }

	route_recover(caller->frame, ContextRecord, TRUE);
	NtRaiseException(ExceptionRecord, ContextRecord, TRUE);
	trap();
}

naked a_msg ai_ctx_jump0() {
	/* load TEB */
	asm("movq %%gs:(%c0), %%r10"::"i"(offsetof(ext(TEB), Self)));

	/* save FLS */
	asm("movq %c0(%%r10), %%r8"::"i"(offsetof(ext(TEB), ArbitraryUserPointer)));
	asm("movq %%r8, -0x008(%%rsp)"::);
    /* save deallocation stack */
	asm("movq %c0(%%r10), %%r8"::"i"(offsetof(ext(TEB), DeallocationStack)));
	asm("movq %%r8, -0x010(%%rsp)"::);
	/* save stack limit */
	asm("movq %c0(%%r10), %%r8"::"i"(offsetof(ext(TEB), StackLimit)));
	asm("movq %%r8, -0x018(%%rsp)"::);
	/* save stack base */
	asm("movq %c0(%%r10), %%r8"::"i"(offsetof(ext(TEB), StackBase)));
	asm("movq %%r8, -0x020(%%rsp)"::);

	/* save GPR */
	asm("movq %%rbp, -0x028(%%rsp)"::);
	asm("movq %%rbx, -0x030(%%rsp)"::);
	asm("movq %%rsi, -0x038(%%rsp)"::);
	asm("movq %%rdi, -0x040(%%rsp)"::);
	asm("movq %%r12, -0x048(%%rsp)"::);
	asm("movq %%r13, -0x050(%%rsp)"::);
	asm("movq %%r14, -0x058(%%rsp)"::);
	asm("movq %%r15, -0x060(%%rsp)"::);

	/* save control word */
	asm("stmxcsr -0x068(%%rsp)"::);
	asm("fnstcw -0x06C(%%rsp)"::);

	/* save FPR */
	asm("movaps %%xmm6, -0x078(%%rsp)"::);
	asm("movaps %%xmm7, -0x088(%%rsp)"::);
	asm("movaps %%xmm8, -0x098(%%rsp)"::);
	asm("movaps %%xmm9, -0x0A8(%%rsp)"::);
	asm("movaps %%xmm10, -0x0B8(%%rsp)"::);
	asm("movaps %%xmm11, -0x0C8(%%rsp)"::);
	asm("movaps %%xmm12, -0x0D8(%%rsp)"::);
	asm("movaps %%xmm13, -0x0E8(%%rsp)"::);
	asm("movaps %%xmm14, -0x0F8(%%rsp)"::);
	asm("movaps %%xmm15, -0x108(%%rsp)"::);

	/* save context */
	asm("movq %%rsp, 0x20(%%rdx)"::);

	asm("%c0:"::"p"(ai_ctx_return));

	/* load context */
	asm("movq 0x20(%%rcx), %%rsp"::);

	/* load FPR */
	asm("movaps -0x078(%%rsp), %%xmm6"::);
	asm("movaps -0x088(%%rsp), %%xmm7"::);
	asm("movaps -0x098(%%rsp), %%xmm8"::);
	asm("movaps -0x0A8(%%rsp), %%xmm9"::);
	asm("movaps -0x0B8(%%rsp), %%xmm10"::);
	asm("movaps -0x0C8(%%rsp), %%xmm11"::);
	asm("movaps -0x0D8(%%rsp), %%xmm12"::);
	asm("movaps -0x0E8(%%rsp), %%xmm13"::);
	asm("movaps -0x0F8(%%rsp), %%xmm14"::);
	asm("movaps -0x108(%%rsp), %%xmm15"::);

	/* load Control Word */
	asm("ldmxcsr -0x068(%%rsp)"::);
	asm("fldcw -0x06C(%%rsp)"::);

	/* load GPR */
	asm("movq -0x028(%%rsp), %%rbp"::);
	asm("movq -0x030(%%rsp), %%rbx"::);
	asm("movq -0x038(%%rsp), %%rsi"::);
	asm("movq -0x040(%%rsp), %%rdi"::);
	asm("movq -0x048(%%rsp), %%r12"::);
	asm("movq -0x050(%%rsp), %%r13"::);
	asm("movq -0x058(%%rsp), %%r14"::);
	asm("movq -0x060(%%rsp), %%r15"::);

	/* load FLS */
	asm("movq -0x008(%%rsp), %%r8"::);
	asm("movq %%r8, %c0(%%r10)"::"i"(offsetof(ext(TEB), ArbitraryUserPointer)));
	/* load deallocation stack */
	asm("movq -0x010(%%rsp), %%r8"::);
	asm("movq %%r8, %c0(%%r10)"::"i"(offsetof(ext(TEB), DeallocationStack)));
	/* load stack limit */
	asm("movq -0x018(%%rsp), %%r8"::);
	asm("movq %%r8, %c0(%%r10)"::"i"(offsetof(ext(TEB), StackLimit)));
	/* load stack base */
	asm("movq -0x020(%%rsp), %%r8"::);
	asm("movq %%r8, %c0(%%r10)"::"i"(offsetof(ext(TEB), StackBase)));

	/* load return address */
	asm("popq %%r10"::);

	/* jump to caller route */
	asm("jmpq *%%r10"::);
	asm_unreachable();
}

a_noret ai_ctx_raise(a_henv env, a_msg msg) {
	DWORD code = EXCEPTION_CODE_PREFIX | cast(DWORD, msg & 0xff);
	RaiseException(code, 0, 0, null);
	/* no protector found, call panic handler */
	Global* g = G(env);
	if (g->panic_ != null) {
		(*g->panic_)(env);
	}
	/* no panic handler, abort process immediately */
	trap();
}

EXCEPTION_DISPOSITION alo_specific_handler(
		PEXCEPTION_RECORD ExceptionRecord,
		PVOID EstablisherFrame,
		PCONTEXT ContextRecord,
		PDISPATCHER_CONTEXT DispatcherContext) {
	a_henv env = ref_of(GRoute*, DispatcherContext->EstablisherFrame + 0x30);

	if (ExceptionRecord->ExceptionFlags & (EXCEPTION_UNWINDING | EXCEPTION_EXIT_UNWIND)) {
		return EXCEPTION_EXECUTE_HANDLER;
	}
	else {
		a_msg msg = code2msg(ExceptionRecord->ExceptionCode);
		if (msg == ALO_SOK) return EXCEPTION_CONTINUE_SEARCH;

		ai_err_except(env, msg);

		RtlUnwindEx(
				EstablisherFrame,
				ai_ctx_catch_unwind,
				ExceptionRecord,
				cast(PVOID, cast(a_isize, msg)),
				ContextRecord,
				DispatcherContext->HistoryTable);
		unreachable();
	}
}

naked a_msg ai_ctx_catch0() {
	asm(".seh_handler %c0, @except, @unwind"::"p"(alo_specific_handler));

	asm("subq $0x28, %%rsp"::);
	asm(".cfi_def_cfa_offset 0x30"::);

	asm("movq %%rcx, 0x30(%%rsp)"::);
	asm("callq *%r8");
	asm("xorq %%rax, %%rax"::);

	asm("%c0:"::"p"(ai_ctx_catch_unwind));
	asm("addq $0x28, %%rsp"::);
	asm(".cfi_def_cfa_offset 0x08"::);
	asm("ret"::);
	asm_unreachable();
}

static naked a_noret ai_ctx_start() {
	asm(".seh_handler %c0, @except"::"p"(ai_ctx_start_catch));

	/* save env */
	asm("movq %%rcx, %%rbx"::);

	/* do vm call */
	asm("movq 0x40(%%rcx), %%rdx"::"i"(offsetof(GRoute, rctx_alloc)));
	asm("movq $-1, %%r8"::);
	asm("call %c0"::"p"(ai_vm_call));

	asm("movq %%rbx, %%rdx"::);
	asm("movq 0x30(%%rbx), %%rcx"::);
	asm("xorq %%rax, %%rax"::);
	asm("jmp %c0"::"p"(ai_ctx_return));
	asm_unreachable();
}

a_msg ai_ctx_open(a_henv env, a_usize stack_size) {
	PIMAGE_NT_HEADERS header = RtlImageNtHeader(NtCurrentTeb()->ProcessEnvironmentBlock->ImageBaseAddress);
	if (header == null) return ALO_EINVAL;

	a_usize commit = align_forward(stack_size, PAGE_SIZE);
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