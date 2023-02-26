/**
 *@file astk.h
 */

#ifndef astk_h_
#define astk_h_

#include "aobj.h"

typedef struct Stack Stack;

intern a_bool ai_stk_init(a_henv env, Stack* stack);
intern a_isize ai_stk_grow(a_henv env, Value* top);
intern a_isize ai_stk_check(a_henv env, Value* top);
intern void ai_stk_deinit(Global* g, Stack* stack);

/* Reserve stack size for VM use. */
#define RESERVED_STACK_SIZE 5
/* Stack size for stack overflow error handling. */
#define OVERFLOW_STACK_SIZE 128

#ifndef ALOI_INIT_STACKSIZE
# define ALOI_INIT_STACKSIZE usizec(256)
#endif

#ifndef ALOI_INIT_FRAME_STACKSIZE
# define ALOI_INIT_FRAME_STACKSIZE usizec(16)
#endif

#ifndef ALOI_MAX_STACKSIZE
# define ALOI_MAX_STACKSIZE usizec(100000)
#endif

#define GROW_STACK_FLAG_OF1 1
#define GROW_STACK_FLAG_OF2 2

#define INIT_STACK_SIZE ALOI_INIT_STACKSIZE
#define INIT_STACK_SIZE_WITH_RESERVED (INIT_STACK_SIZE + RESERVED_STACK_SIZE)
#define MAX_STACK_SIZE ALOI_MAX_STACKSIZE
#define MAX_OVERFLOWED_STACK_SIZE (MAX_STACK_SIZE + OVERFLOW_STACK_SIZE)
#define MAX_STACK_SIZE_WITH_RESERVED (MAX_STACK_SIZE + RESERVED_STACK_SIZE)

struct Stack {
	Value* _base;
	Value* _bot;
	Value* _top;
	Value* _limit;
};

#endif /* astk_h_ */
