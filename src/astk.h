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
intern a_none ai_stk_overflow(a_henv env, a_isize diff);
intern void ai_stk_deinit(Global* g, Stack* stack);

#define STACK_GROW_FAILED isizec(1)

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

#if ALO_STACK_RELOC
typedef a_isize StkPtr;
#else
typedef Value* StkPtr;
#endif

#endif /* astk_h_ */
