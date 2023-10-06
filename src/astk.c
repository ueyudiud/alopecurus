/**
 *@file astk.c
 */

#define astk_c_
#define ALO_LIB

#include "aenv.h"
#include "amem.h"
#include "agc.h"
#include "aerr.h"
#include "actx.h"

#include "astk.h"

#if ALO_STACK_OUTER
# define STACK_SIZE_GRANULARITY PAGE_SIZE
#else
# define STACK_SIZE_GRANULARITY usizec(1024)
#endif

enum {
	INIT_STACK_SIZE = pad_to_raw(sizeof(Value) * (ALOI_INIT_STACKSIZE + RESERVED_STACK_SIZE), STACK_SIZE_GRANULARITY),
	MAX_STACK_SIZE = pad_to_raw(sizeof(Value) * (ALOI_MAX_STACKSIZE + RESERVED_STACK_SIZE), STACK_SIZE_GRANULARITY),
	MAX_OVERFLOWED_STACK_SIZE = pad_to_raw(sizeof(Value) * (ALOI_MAX_STACKSIZE + OVERFLOW_STACK_SIZE + RESERVED_STACK_SIZE), STACK_SIZE_GRANULARITY)
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

	*stack = new(Stack) {
		._base = base,
		._limit = ptr_disp(Value, base, INIT_STACK_SIZE) - RESERVED_STACK_SIZE,
		._top = base,
		._alloc_size = alloc_size
	};

# if ALO_STRICT_STACK_CHECK
	self->_base_frame._bound = stack->_base + ALOI_INIT_CFRAMESIZE;
# endif

	return false;
}

#if ALO_STACK_RELOC
static void stack_reloc(a_henv env, a_isize diff) {
	Stack* stack = &env->_stack;
	stack->_top = ptr_disp(Value, stack->_top, diff);

	for (Frame* frame = env->_frame; frame != null; frame = frame->_prev) {
		frame->_stack_bot = ptr_disp(Value, frame->_stack_bot, diff);
        frame->_stack_dst = ptr_disp(Value, frame->_stack_dst, diff);
	}

    for (RcCap* cap = env->_open_caps; cap != null; cap = cap->_next) {
        cap->_ptr = ptr_disp(Value, cap->_ptr, diff);
    }
}
#endif

static a_isize stack_grow(a_henv env, Stack* stack, a_usize size_new) {
	a_isize diff;

#if ALO_STACK_INNER
	a_usize size_old = stack->_alloc_size;
	Value* stack_old = stack->_base;
	Value* stack_new;
#if ALO_STRICT_MEMORY_CHECK
	stack_new = ai_mem_alloc(env, size_new);
	memcpy(stack_new, stack_old, ptr_diff(stack->_top, stack->_base));
	ai_mem_dealloc(G(env), stack_old, size_old);
#else
	stack_new = ai_mem_realloc(env, stack_old, size_old, size_new);
#endif
	diff = ptr_diff(stack_new, stack_old);
	stack->_base = stack_new;
	stack->_alloc_size = size_new;

	/* Relocate stack if base address is changed. */
	if (diff != 0) {
		stack_reloc(env, diff);
	}

	return diff;
#else
	if (likely(stack->_alloc_size < sizeof(Value) * size_new)) {
		a_usize size_old = stack->_alloc_size;
		assume(size_new > size_old, "grow nothing.");
		a_bool result = ai_mem_ncommit(stack->_base, stack->_base + size_old, size_new - size_old, NCOMMIT_PROT_READWRITE);
		if (unlikely(!result))
			ai_mem_nomem(env);
		stack->_alloc_size = size_new;
	}
	diff = 0;
#endif

	stack->_limit = stack->_base + size_new - RESERVED_STACK_SIZE;

	return diff;
}

#define STACK_GROW_OVERFLOW isizec(2)

a_none ai_stk_overflow(a_henv env, a_isize diff) {
	if (diff & STACK_GROW_OVERFLOW) {
		GStr* err = ai_str_newl(env, "stack overflow");
		v_set_obj(env, &env->_error, err);
		ai_env_raise(env, ALO_ESTKOF);
	}
	else {
		ai_err_raisef(env, ALO_ESTKOF, "stack overflow");
	}
}

a_isize ai_stk_grow(a_henv env, Value* top) {
	Stack* stack = &env->_stack;
	assume(top > stack->_limit);
	a_usize current_size = ptr_diff(stack->_limit, stack->_base);
	a_usize expect_size = ptr_diff(top, stack->_base);
	assume(expect_size > current_size);
	if (unlikely(expect_size > MAX_STACK_SIZE)) {
		if (current_size == MAX_OVERFLOWED_STACK_SIZE) {
			return STACK_GROW_FAILED | STACK_GROW_OVERFLOW;
		}
		stack_grow(env, stack, MAX_OVERFLOWED_STACK_SIZE);
		return STACK_GROW_FAILED;
	}

	a_usize size_new = pad_to(expect_size, STACK_SIZE_GRANULARITY);
	size_new = max(size_new, current_size + STACK_SIZE_GRANULARITY);
	size_new = min(size_new, MAX_STACK_SIZE);
	return stack_grow(env, stack, size_new);
}

void ai_stk_shrink(a_henv env) {
	Stack* stack = &env->_stack;
	a_usize current_size = stack->_alloc_size;
	a_usize used_size = stack->_top - stack->_base;
	if (unlikely(used_size <= current_size / 4 && current_size > STACK_SIZE_GRANULARITY)) {
		a_usize size_new = current_size / 2;
		a_usize size_old = stack->_alloc_size;
		assume(size_new < size_old, "shrink nothing.");
		ai_mem_ndealloc(G(env), stack->_base + size_new, size_old - size_new);
	}
}

void ai_stk_deinit(Global* g, Stack* stack) {
#if ALO_STACK_INNER
	ai_mem_dealloc(g, stack->_base, stack->_alloc_size);
#else
	quiet(g);
	ai_mem_nrelease(stack->_base, MAX_OVERFLOWED_STACK_SIZE);
#endif
}