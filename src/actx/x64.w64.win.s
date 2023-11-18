    .file "x64.w64.win.s"
    .text

    .p2align 4,,15
    .globl  ai_ctx_jump
    .def    ai_ctx_jump; .scl 2; .type 32; .endef
    .seh_proc       ai_ctx_jump
ai_ctx_jump:
    .seh_endprologue

    /* load TEB */
    movq %gs:(0x30), %r10

    /* save FLS */
    movq 0x28(%r10), %rax
    movq %rax, -0x008(%rsp)
    /* save deallocation stack */
    movq 0x1478(%r10), %rax
    movq %rax, -0x010(%rsp)
    /* save stack limit */
    movq 0x0010(%r10), %rax
    movq %rax, -0x018(%rsp)
    /* save stack base */
    movq 0x0008(%r10), %rax
    movq %rax, -0x020(%rsp)

    /* save GPR */
    movq %rbp, -0x028(%rsp)
    movq %rbx, -0x030(%rsp)
    movq %rsi, -0x038(%rsp)
    movq %rdi, -0x040(%rsp)
    movq %r12, -0x048(%rsp)
    movq %r13, -0x050(%rsp)
    movq %r14, -0x058(%rsp)
    movq %r15, -0x060(%rsp)

    /* save Control Word */
    stmxcsr -0x068(%rsp)
    fnstcw -0x06C(%rsp)  

    /* save FPR */
    movaps %xmm6, -0x078(%rsp)
    movaps %xmm7, -0x088(%rsp)
    movaps %xmm8, -0x098(%rsp)
    movaps %xmm9, -0x0A8(%rsp)
    movaps %xmm10, -0x0B8(%rsp)
    movaps %xmm11, -0x0C8(%rsp)
    movaps %xmm12, -0x0D8(%rsp)
    movaps %xmm13, -0x0E8(%rsp)
    movaps %xmm14, -0x0F8(%rsp)
    movaps %xmm15, -0x108(%rsp)

    /* save context */
    movq %rsp, 0x20(%rdx)

    /* move result message */
    movq %r8, %rax

ai_ctx_return:
    /* load context */
    movq 0x20(%rcx), %rsp

    /* load FPR */
    movaps -0x078(%rsp), %xmm6
    movaps -0x088(%rsp), %xmm7
    movaps -0x098(%rsp), %xmm8
    movaps -0x0A8(%rsp), %xmm9
    movaps -0x0B8(%rsp), %xmm10
    movaps -0x0C8(%rsp), %xmm11
    movaps -0x0D8(%rsp), %xmm12
    movaps -0x0E8(%rsp), %xmm13
    movaps -0x0F8(%rsp), %xmm14
    movaps -0x108(%rsp), %xmm15

    /* load Control Word */
    ldmxcsr -0x068(%rsp)
    fldcw -0x06C(%rsp)

    /* load GPR */
    movq -0x028(%rsp), %rbp
    movq -0x030(%rsp), %rbx
    movq -0x038(%rsp), %rsi
    movq -0x040(%rsp), %rdi
    movq -0x048(%rsp), %r12
    movq -0x050(%rsp), %r13
    movq -0x058(%rsp), %r14
    movq -0x060(%rsp), %r15

    /* load FLS */
    movq -0x008(%rsp), %r8
    movq %r8, 0x28(%r10)
    /* load deallocation stack */
    movq -0x010(%rsp), %r8
    movq %r8, 0x1478(%r10)
    /* load stack limit */
    movq -0x018(%rsp), %r8
    movq %r8, 0x0010(%r10)
    /* load stack base */
    movq -0x020(%rsp), %r8
    movq %r8, 0x0008(%r10)
    
    /* load return address */
    popq %r10

    /* jump to caller route */
    jmpq *%r10

    .seh_endproc

    .p2align 4,,15
    .globl  ai_ctx_start
    .def    ai_ctx_start; .scl 2; .type 32; .endef
    .seh_proc       ai_ctx_start
    .seh_stackalloc 0x8
ai_ctx_start:
    .seh_endprologue

    .seh_handler ai_ctx_start_catch, @except

    /* save env */
    movq %rcx, %rbx

    /* do vm call */
    movq 0x40(%rcx), %rdx
    movq $-1, %r8
    call ai_vm_call

    /* do half jump */
    movq %rbx, %rdx
    movq 0x30(%rbx), %rcx
    xorq %rax, %rax
    jmp ai_ctx_return

    .seh_endproc
