/**
 *@file astk.h
 */

#ifndef astk_h_
#define astk_h_

#include "aobj.h"

typedef struct Stack Stack;

intern a_bool ai_stk_init(a_henv env, Stack* stack);
intern a_isize ai_stk_grow(a_henv env, Value* top);
intern void ai_stk_shrink(a_henv env);
intern a_none ai_stk_overflow(a_henv env, a_bool again);
intern void ai_stk_deinit(Global* g, Stack* stack);

/* Reserve stack size for VM use. */
#define RESERVED_STACK_SIZE 5
/* Stack size for stack overflow error handling. */
#define OVERFLOW_STACK_SIZE 128

#ifndef ALOI_INIT_STACKSIZE
# define ALOI_INIT_STACKSIZE usizec(500)
#endif

#ifndef ALOI_INIT_CFRAME_STACKSIZE
# define ALOI_INIT_CFRAME_STACKSIZE usizec(16)
#endif

#ifndef ALOI_MAX_STACKSIZE
# define ALOI_MAX_STACKSIZE usizec(100000)
#endif

struct Stack {
	Value* _base;
	Value* _top;
	Value* _limit;
	a_usize _alloc_size; /* The actual allocate size.*/
};

#define GROW_STACK_FLAG_OF1 1
#define GROW_STACK_FLAG_OF2 2

always_inline a_isize ai_stk_check(a_henv env, Stack* stack, Value* top) {
	if (top > stack->_limit) {
		a_isize diff = ai_stk_grow(env, top);
		if (diff & (GROW_STACK_FLAG_OF1 | GROW_STACK_FLAG_OF2)) {
			ai_stk_overflow(env, (diff & GROW_STACK_FLAG_OF2) != 0);
		}
		return diff;
	}
	return 0;
}

#if ALO_STACK_RELOC
typedef a_isize StkPtr;
#else
typedef Value* StkPtr;
#endif

#endif /* astk_h_ */
