/*
 * aenv.h
 */

#ifndef aenv_h_
#define aenv_h_

#include <stdatomic.h>

#include "astr.h"
#include "afun.h"

#define GLOBAL_FLAG_INCRGC u16c(0x0001)
#define GLOBAL_FLAG_FULLGC u16c(0x0002)
#define GLOBAL_FLAG_PAUSE_CLOSE u16c(0x0004)
#define GLOBAL_FLAG_DISABLE_GC u16c(0x0008)

typedef void (*a_pfun)(a_henv, void*);

intern GRoute* ai_env_new(a_henv env, a_usize stack_size);
intern a_henv ai_env_mainof(Global* g);
intern a_msg ai_env_resume(a_henv env, GRoute* self);
intern void ai_env_yield(a_henv env);
intern a_msg ai_env_protect(a_henv env, a_pfun pfun, void* pctx);

#define GROW_STACK_FLAG_OF1 1
#define GROW_STACK_FLAG_OF2 2

intern a_none ai_env_raise(a_henv env, a_msg msg);
intern a_isize ai_env_grow_stack(a_henv env, Value* top);
intern a_isize ai_env_check_stack(a_henv env, Value* top);

#ifndef ALOI_DFL_GCSTEPMUL
# define ALOI_DFL_GCSTEPMUL usizec(384)
#endif

#ifndef ALOI_DFL_GCPAUSEMUL
# define ALOI_DFL_GCPAUSEMUL usizec(512)
#endif

#ifndef ALOI_INIT_STACKSIZE
# define ALOI_INIT_STACKSIZE usizec(256)
#endif

#ifndef ALOI_INIT_CFRAMESIZE
# define ALOI_INIT_CFRAMESIZE usizec(8)
#endif

#ifndef ALOI_MAX_STACKSIZE
# define ALOI_MAX_STACKSIZE usizec(100000)
#endif

/* Reserve stack size for VM use. */
#define RESERVED_STACKSIZE 5
/* Stack size for stack overflow error handling. */
#define OVERFLOW_STACKSIZE 128

typedef struct Stack {
	Value* _base;
	Value* _bot;
	Value* _top;
	Value* _limit;
} Stack;

#define RFLAG_COUNT_VARARG 255

/* Flags of result. */
typedef struct {
	a_u16 _count: 8; /* Number of expected return count. */
} RFlags;

struct Frame {
	Frame* _prev;
	a_insn* _pc;
	a_isize _stack_bot_diff;
	Capture* _captures;
	RFlags _rflags;
	/* In strict stack checking mode, the API will use frame bound to check index range. */
#if ALO_STRICT_STACK_CHECK
	Value* _bound;
#endif
};

#define GROUTE_FLAGS_YIELDABLE u16c(0x0001)

struct alo_Env {
	GOBJ_STRUCT_HEADER;
	a_u8 _status;
	a_u16 _flags;
	Global* _g;
	GRoute* _from;
	Frame* _frame;
	Stack _stack;
	Value _error;
	Frame _base_frame;
};

#define G(env) ((env)->_g)

always_inline void ai_env_pop_error(a_henv env, Value* d) {
	v_cpy(G(env), d, &env->_error);
	env->_error = v_of_nil();
}

#endif /* aenv_h_ */