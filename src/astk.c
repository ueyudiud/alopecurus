/**
 *@file astk.c
 */

#define astk_c_
#define ALO_LIB

#include "aenv.h"
#include "amem.h"
#include "aerr.h"
#include "actx.h"

#include "astk.h"

#if ALO_STACK_OUTER
# define STACK_GRANULARITY PAGE_SIZE
#else
# define STACK_GRANULARITY usizec(1024)
#endif

enum {
	INIT_STACK_SIZE = pad_to_raw(sizeof(Value) * (ALOI_INIT_STACKSIZE + RESERVED_STACK_SIZE), STACK_GRANULARITY),
	MAX_STACK_SIZE = pad_to_raw(sizeof(Value) * (ALOI_MAX_STACKSIZE + RESERVED_STACK_SIZE), STACK_GRANULARITY),
	MAX_OVERFLOWED_STACK_SIZE = pad_to_raw(sizeof(Value) * (ALOI_MAX_STACKSIZE + OVERFLOW_STACK_SIZE + RESERVED_STACK_SIZE), STACK_GRANULARITY)
};

a_bool ai_stk_init(a_henv env, Stack* stack) {
	Value* base;
	a_usize alloc_size = INIT_STACK_SIZE;

#if ALO_STACK_INNER
	base = ai_mem_nalloc(env, alloc_size);
	if (unlikely(base == null)) return true;
#else
	quiet(env);
	a_usize limit_page_size = MAX_OVERFLOWED_STACK_SIZE;
	base = ai_mem_nreserve(null, limit_page_size);
	if (unlikely(base == null)) return true;
	a_bool result = ai_mem_ncommit(base, base, alloc_size, NCOMMIT_PROT_READWRITE);
	if (unlikely(!result)) {
		ai_mem_nrelease(base, limit_page_size);
		return true;
	}
#endif

    init(stack) {
		.base = base,
		.limit = ptr_disp(Value, base, INIT_STACK_SIZE) - RESERVED_STACK_SIZE,
		.top = base,
		.alloc_size = alloc_size
	};

#ifdef ALOI_CHECK_API
    env->base_frame.stack_limit = val2stk(env, base + ALOI_INIT_CFRAME_STACKSIZE);
#endif

	return false;
}

#if ALO_STACK_RELOC
static void stack_reloc(a_henv env, a_isize diff) {
	Stack* stack = &env->stack;
	stack->top = ptr_disp(Value, stack->top, diff);

	for (Frame* frame = env->frame; frame != null; frame = frame->prev) {
		frame->stack_bot = ptr_disp(Value, frame->stack_bot, diff);
        frame->stack_dst = ptr_disp(Value, frame->stack_dst, diff);
	}

    for (RcCap* cap = env->open_caps; cap != null; cap = cap->next) {
        cap->ptr = ptr_disp(Value, cap->ptr, diff);
    }
}
#endif

static a_isize stack_grow(a_henv env, Stack* stack, a_usize size_new) {
	a_isize diff;

#if ALO_STACK_INNER
	a_usize size_old = stack->alloc_size;
	Value* stack_old = stack->base;
	Value* stack_new;
#if ALOI_MEMORY_CHECK
	stack_new = ai_mem_alloc(env, size_new);
	memcpy(stack_new, stack_old, addr_diff(stack->top, stack->base));
	ai_mem_dealloc(G(env), stack_old, size_old);
#else
	stack_new = ai_mem_realloc(env, stack_old, size_old, size_new);
#endif
	diff = addr_diff(stack_new, stack_old);
	stack->base = stack_new;
	stack->alloc_size = size_new;

	/* Relocate stack if base address is changed. */
	if (diff != 0) {
		stack_reloc(env, diff);
	}

	return diff;
#else
	if (likely(stack->alloc_size < sizeof(Value) * size_new)) {
		a_usize size_old = stack->alloc_size;
		assume(size_new > size_old, "grow nothing.");
		a_bool result = ai_mem_ncommit(stack->base, stack->base + size_old, size_new - size_old, NCOMMIT_PROT_READWRITE);
		if (unlikely(!result))
			ai_mem_nomem(env);
		stack->alloc_size = size_new;
	}
	diff = 0;
#endif

	stack->limit = stack->base + size_new - RESERVED_STACK_SIZE;

	return diff;
}

#define STACK_GROW_OVERFLOW isizec(2)

a_noret ai_stk_overflow(a_henv env, a_isize diff) {
	if (diff & STACK_GROW_OVERFLOW) {
		GStr* err = ai_str_from_ntstr(env, "stack overflow");
		v_set_obj(env, &env->error, err);
		ai_env_raise(env, ALO_ESTKOF);
	}
	else {
		ai_err_raisef(env, ALO_ESTKOF, "stack overflow");
	}
}

a_isize ai_stk_grow(a_henv env, Value* top) {
	Stack* stack = &env->stack;
	assume(top > stack->limit);
	a_usize current_size = addr_diff(stack->limit, stack->base);
	a_usize expect_size = addr_diff(top, stack->base);
	assume(expect_size > current_size);
	if (unlikely(expect_size > MAX_STACK_SIZE)) {
		if (current_size == MAX_OVERFLOWED_STACK_SIZE) {
			return STACK_GROW_FAILED | STACK_GROW_OVERFLOW;
		}
		stack_grow(env, stack, MAX_OVERFLOWED_STACK_SIZE);
		return STACK_GROW_FAILED;
	}

	a_usize size_new = pad_to(expect_size, STACK_GRANULARITY);
	size_new = max(size_new, current_size + STACK_GRANULARITY);
	size_new = min(size_new, MAX_STACK_SIZE);
	return stack_grow(env, stack, size_new);
}

void ai_stk_shrink(a_henv env) {
	Stack* stack = &env->stack;
	a_usize current_size = stack->alloc_size;
	a_usize used_size = stack->top - stack->base;
	if (unlikely(used_size <= current_size / 4 && current_size > STACK_GRANULARITY)) {
		a_usize size_new = current_size / 2;
		a_usize size_old = stack->alloc_size;
		assume(size_new < size_old, "shrink nothing.");
		ai_mem_ndealloc(G(env), stack->base + size_new, size_old - size_new);
	}
}

void ai_stk_deinit(Global* gbl, Stack* stack) {
#if ALO_STACK_INNER
	ai_mem_dealloc(gbl, stack->base, stack->alloc_size);
#else
	quiet(gbl);
	ai_mem_nrelease(stack->base, MAX_OVERFLOWED_STACK_SIZE);
#endif
}