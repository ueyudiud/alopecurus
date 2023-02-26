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

a_bool ai_stk_init(a_henv env, Stack* stack) {
	Value* base;
#if ALO_STACK_DISABLE_MMAP
	base = ai_mem_vxnew(env, Value, INIT_STACK_SIZE_WITH_RESERVED);
	if (base == null) return true;
	stack->_base = base;
	stack->_limit = base + INIT_STACK_SIZE;
#else
	if (ai_ctx_stack_init(env, stack)) return true;
	base = stack->_base;
#endif
	stack->_bot = base;
	stack->_top = base;
# if ALO_STRICT_STACK_CHECK
	self->_base_frame._bound = stack->_base + ALOI_INIT_CFRAMESIZE;
# endif
	return false;
}

#if ALO_STACK_DISABLE_MMAP
static void stack_reloc(a_henv env, a_isize diff) {
	Stack* stack = &env->_stack;
	stack->_bot = ptr_disp(Value, stack->_bot, diff);
	stack->_top = ptr_disp(Value, stack->_top, diff);
	for (Capture* cap = env->_frame->_captures; cap != null; cap = cap->_next) {
		cap->_ptr = ptr_disp(Value, cap->_ptr, diff);
	}
}
#endif

static a_isize stack_grow(a_henv env, a_usize size_new) {
#if ALO_STACK_DISABLE_MMAP
	a_usize size_old = env->_stack._limit - env->_stack._base + RESERVED_STACK_SIZE;
	Value* stack_old = env->_stack._base;
	Value* stack_new;
#if ALO_STRICT_MEMORY_CHECK
	stack_new = ai_mem_vnew(env, Value, size_new);
	memcpy(stack_new, stack_old, sizeof(Value) * (env->_stack._top - env->_stack._base));
	ai_mem_vxdel(env, stack_old, size_old);
#else
	stack_new = ai_mem_vgrow(env, env->_stack._base, size_old, size_new);
#endif
	a_isize diff = ptr_diff(stack_new, stack_old);
	env->_stack._base = stack_new;
	env->_stack._limit = stack_new + size_new - RESERVED_STACK_SIZE;

	/* Relocate stack if base address is changed. */
	if (diff != 0) {
		stack_reloc(env, diff);
	}

	return diff;
#else
	if (unlikely(ai_ctx_stack_grow(env, &env->_stack, size_new)))
		ai_mem_nomem(env);
	return 0;
#endif
}

static a_none stack_overflow(a_henv env, a_bool again) {
	if (again) {
		GStr* err = ai_str_newl(env, "stack overflow");
		v_set(G(env), &env->_error, v_of_obj(err));
		ai_env_raise(env, ALO_ESTKOF);
	}
	else {
		ai_err_raisef(env, ALO_ESTKOF, "stack overflow");
	}
}

a_isize ai_stk_grow(a_henv env, Value* top) {
	a_usize current_size = env->_stack._limit - env->_stack._base;
	a_usize expect_size = top - env->_stack._base;
	assume(expect_size > current_size);
	if (unlikely(expect_size > MAX_STACK_SIZE)) {
		if (current_size == MAX_OVERFLOWED_STACK_SIZE)
			return GROW_STACK_FLAG_OF2;
		stack_grow(env, MAX_OVERFLOWED_STACK_SIZE);
		return GROW_STACK_FLAG_OF1;
	}
	current_size = max(current_size + GROW_STACK_SIZE, expect_size);
	current_size = min(current_size, MAX_STACK_SIZE);

	return stack_grow(env, current_size);
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
#if ALO_STACK_DISABLE_MMAP
	ai_mem_vxdel(g, stack->_base, stack->_limit - stack->_base + RESERVED_STACK_SIZE);
#else
	ai_ctx_stack_deinit(g, stack);
#endif
}