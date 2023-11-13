    .file "x64.sysv.posix.s"
    .text

    .globl  ai_ctx_jump
    .type   ai_ctx_jump, @function
    .align  16
ai_ctx_jump:

    /* save GPR */
    movq %rbp, -0x008(%rsp)
    movq %rbx, -0x010(%rsp)
    movq %r12, -0x018(%rsp)
    movq %r13, -0x020(%rsp)
    movq %r14, -0x028(%rsp)
    movq %r15, -0x030(%rsp)

    /* save context */
    movq %rsp, 0x18(%rsi)

    /* move result message */
    movq %rdx, %rax

ai_ctx_return:
    /* load context */
    movq 0x18(%rdi), %rsp

    /* load GPR */
    movq -0x008(%rsp), %rbp
    movq -0x010(%rsp), %rbx
    movq -0x018(%rsp), %r12
    movq -0x020(%rsp), %r13
    movq -0x028(%rsp), %r14
    movq -0x030(%rsp), %r15

    /* load return address */
    popq %r10

    /* jump to caller route */
    jmpq *%r10

    .size ai_ctx_jump,.-ai_ctx_jump

    .globl  ai_ctx_start
    .type   ai_ctx_start, @function
    .align  16
ai_ctx_start:

    /* save env */
    movq %rdi, %rbx

    /* do vm call */
    movq 0x38(%rdi), %rsi
    movq $-1, %rdx
    call ai_vm_call

    /* do half jump */
    movq %rbx, %rsi
    movq 0x28(%rsi), %rdi
    xorq %rax, %rax
    jmp ai_ctx_return

    .size ai_ctx_start,.-ai_ctx_start
