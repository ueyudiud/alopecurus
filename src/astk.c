/*
 * astk.c
 *
 *  Created on: 2023/2/26
 *      Author: ueyudiud
 */

#define astk_c_
#define ALO_LIB

#include "aenv.h"
#include "amem.h"
#include "agc.h"
#include "aerr.h"
#include "actx.h"

#include "astk.h"

#define GROW_STACK_SIZE (4096 / sizeof(Value))

#if ALO_STACK_OUTER

static a_usize l_to_page_size(a_usize stack_size) {
	return (sizeof(Value) * stack_size + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1);
}

#endif

a_bool ai_stk_init(a_henv env, Stack* stack) {
	Value* base;

#if ALO_STACK_INNER
	base = ai_mem_vxnew(env, Value, INIT_STACK_SIZE_WITH_RESERVED);
	if (unlikely(base == null)) return true;
#else
	quiet(env);
	base = ai_mem_nreserve(null, l_to_page_size(MAX_STACK_SIZE_WITH_RESERVED));
	if (unlikely(base == null)) return true;
	a_usize init_page_size = l_to_page_size(INIT_STACK_SIZE_WITH_RESERVED);
	void* result = ai_mem_ncommit(base, init_page_size, CTX_VA_RW);
	if (unlikely(result == null)) {
		ai_mem_nrelease(base, l_to_page_size(MAX_STACK_SIZE_WITH_RESERVED));
		return true;
	}
	stack->_alloc_size = init_page_size;
#endif

	*stack = new(Stack) {
		._base = base,
		._limit = base + INIT_STACK_SIZE,
		._top = base
	};

# if ALO_STRICT_STACK_CHECK
	self->_base_frame._bound = stack->_base + ALOI_INIT_CFRAMESIZE;
# endif



	return false;
}

#if ALO_STACK_RELOC
static void stack_reloc(a_henv env, a_isize diff) {
	Stack* stack = &env->_stack;
	stack->_bot = ptr_disp(Value, stack->_bot, diff);
	stack->_top = ptr_disp(Value, stack->_top, diff);

	for (RcCap* cap = env->_frame->_caps; cap != null; cap = cap->_next) {
		cap->_ptr = ptr_disp(Value, cap->_ptr, diff);
	}
}
#endif

static a_isize stack_grow(a_henv env, Stack* stack, a_usize size_new) {
	a_isize diff;

#if ALO_STACK_INNER
	a_usize size_old = stack->_limit - stack->_base + RESERVED_STACK_SIZE;
	Value* stack_old = stack->_base;
	Value* stack_new;
#if ALO_STRICT_MEMORY_CHECK
	stack_new = ai_mem_vnew(env, Value, size_new);
	memcpy(stack_new, stack_old, sizeof(Value) * (stack->_top - stack->_base));
	ai_mem_vxdel(env, stack_old, size_old);
#else
	stack_new = ai_mem_vgrow(env, stack->_base, size_old, size_new);
#endif
	diff = ptr_diff(stack_new, stack_old);
	stack->_base = stack_new;

	/* Relocate stack if base address is changed. */
	if (diff != 0) {
		stack_reloc(env, diff);
	}

	return diff;
#else
	if (likely(stack->_alloc_size < sizeof(Value) * size_new)) {
		a_usize page_size_old = stack->_alloc_size;
		a_usize page_size_new = l_to_page_size(size_new);
		assume(page_size_old % PAGE_SIZE == 0, "bad page size.");
		assume(page_size_new > page_size_old, "grow nothing.");
		void* addr = stack->_base;
		void* result = ai_mem_ncommit(addr + page_size_old, page_size_new - page_size_old, CTX_VA_RW);
		if (unlikely(result == null))
			ai_mem_nomem(env);
		stack->_alloc_size = page_size_new;
	}
	diff = 0;
#endif

	stack->_limit = stack->_base + size_new - RESERVED_STACK_SIZE;

	return diff;
}

static a_none stack_overflow(a_henv env, a_bool again) {
	if (again) {
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
	a_usize current_size = stack->_limit - stack->_base;
	a_usize expect_size = top - stack->_base;
	assume(expect_size > current_size);
	if (unlikely(expect_size > MAX_STACK_SIZE)) {
		if (current_size == MAX_OVERFLOWED_STACK_SIZE)
			return GROW_STACK_FLAG_OF2;
		stack_grow(env, stack, MAX_OVERFLOWED_STACK_SIZE + RESERVED_STACK_SIZE);
		return GROW_STACK_FLAG_OF1;
	}
	current_size = max(current_size + GROW_STACK_SIZE, expect_size);
	current_size = min(current_size, MAX_STACK_SIZE);

	return stack_grow(env, stack, current_size + RESERVED_STACK_SIZE);
}

a_isize ai_stk_check(a_henv env, Value* top) {
	if (top > env->_stack._limit) {
		a_isize diff = ai_stk_grow(env, top);
		if (diff & (GROW_STACK_FLAG_OF1 | GROW_STACK_FLAG_OF2)) {
			stack_overflow(env, (diff & GROW_STACK_FLAG_OF2) != 0);
		}
		return diff;
	}
	return 0;
}

void ai_stk_deinit(Global* g, Stack* stack) {
#if ALO_STACK_INNER
	ai_mem_vxdel(g, stack->_base, stack->_limit - stack->_base + RESERVED_STACK_SIZE);
#else
	quiet(g);
	ai_mem_ndecommit(stack->_base, l_to_page_size(MAX_STACK_SIZE_WITH_RESERVED));
#endif
}