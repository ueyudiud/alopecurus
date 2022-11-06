/*
 * aenv.h
 */

#ifndef aenv_h_
#define aenv_h_

#include <stdatomic.h>

#include "astr.h"

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

typedef struct Frame Frame;

struct Frame {
	Frame* _prev;
	a_insn* _pc;
	union {
		a_u16 _rflags; /* Flags of result. */
		struct {
			a_u8 _nret; /* Number of expected return count. */
		};
	};
	/* In strict stack checking mode, the API will use frame bound to check index range. */
#ifdef ALOI_STRICT_STACK_CHECK
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

typedef struct {
	IStr** _table;
	a_usize _len;
	a_usize _hmask; /* Hash code mask. */
} IStrTable;

typedef struct {
	GMeta _nil;
	GMeta _bool;
	GMeta _int;
	GMeta _ptr;
	GMeta _dstr;
	GMeta _istr;
	GMeta _hstr;
	GMeta _tuple;
	GMeta _list;
	GMeta _table;
	GMeta _route;
	GMeta _fmeta;
} InternMetas;

#define GLOBAL_FLAG_INCRGC u16c(0x0001)
#define GLOBAL_FLAG_FULLGC u16c(0x0002)
#define GLOBAL_FLAG_PAUSE_CLOSE u16c(0x0004)
#define GLOBAL_FLAG_DISABLE_GC u16c(0x0008)

typedef void (*a_fp_gsplash)(Global* g, void* ctx);

struct Global {
	Alloc _af;
	void* _ac;
	a_kfun _hookf;
	a_kctx _hookc;
	a_henv _active;
	a_usize _mem_base;
	a_isize _mem_debt;
	a_isize _mem_work;
	a_usize _mem_estimate;
	a_gclist _gc_normal;
	a_gclist _gc_fixed;
	a_gclist _gc_closable;
	a_gclist _gc_toclose;
	a_gcnext* _gc_sweep;
	GStr* _nomem_error;
	a_fp_gsplash _gsplash;
	void* _gsplash_ctx;
	a_trmark _tr_gray;
	a_trmark _tr_regray;
	InternMetas _metas;
	IStrTable _istable;
	a_hash _seed;
	a_u16 _gcpausemul;
	a_u16 _gcstepmul;
	a_u16 _flags;
	a_u8 _white_color;
	a_u8 _gcstep;
	volatile atomic_uint_fast8_t _hookm;
};

#define ALO_HMSWAP 0x80

inline void ai_env_gsplash(a_henv env, a_fp_gsplash fun, void* ctx) {
	assume(fun != null);
	Global* g = G(env);
	g->_gsplash = fun;
	g->_gsplash_ctx = ctx;
}

inline void ai_env_gsplash_clear(a_henv env) {
	Global* g = G(env);
	g->_gsplash = null;
	g->_gsplash_ctx = null;
}

inline a_usize ai_env_mem_total(Global* g) {
	return g->_mem_base + cast(a_usize, g->_mem_debt);
}

inline GStr* ai_env_strx(Global* g, a_u32 tag) {
	return cast(GStr**, g + 1)[tag - 1];
}

typedef void (*a_pfun)(a_henv, void*);

intern GRoute* ai_env_new(a_henv env, a_usize stack_size);
intern a_henv ai_env_mainof(Global* g);
intern a_msg ai_env_resume(a_henv env, GRoute* self);
intern void ai_env_yield(a_henv env);
intern a_msg ai_env_protect(a_henv env, a_pfun pfun, void* pctx);
intern a_none ai_env_raise(a_henv env, a_msg msg);
intern a_isize ai_env_grow_stack(a_henv env, Value* top);
intern a_isize ai_env_check_stack(a_henv env, Value* top);

#endif /* aenv_h_ */
