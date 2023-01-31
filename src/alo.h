/*
 * alo.h
 */

#ifndef alo_h_
#define alo_h_

#include <stddef.h>
#include <stdarg.h>

#define ALO ALO_VERSION_NUMBER

#define ALO_MAJOR_VERSION 0
#define ALO_MINOR_VERSION 1
#define ALO_PATCH_VERSION 0

#define ALO_VERSION_NUMBER (ALO_MAJOR_VERSION * 100 + ALO_MINOR_VERSION)

typedef unsigned char a_byte;

#define ALO_EXPORT extern

typedef int a_msg;
typedef unsigned int a_enum;
typedef size_t a_flags;

#define ALO_TEMPTY (-1)

#define ALO_TNIL 0
#define ALO_TBOOL 1
#define ALO_TINT 2
#define ALO_TFLOAT 3
#define ALO_TPTR 4
#define ALO_TSTR 5
#define ALO_TTUPLE 6
#define ALO_TROUTE 9
#define ALO_TOTHER 10

#define ALO_EIO ((a_msg) -8) /* Foreign IO Error */
#define ALO_ECHUNK ((a_msg) -7) /* Text/Binary Chunk Error */
#define ALO_EOUTER ((a_msg) -6) /* External Error */
#define ALO_ESTKOF ((a_msg) -5) /* Stack Overflow */
#define ALO_EYIELD ((a_msg) -4) /* Route is Yield */
#define ALO_ENOMEM ((a_msg) -3) /* Out of Memory */
#define ALO_EINVAL ((a_msg) -2) /* Invalid Argument */
#define ALO_ESTMUF ((a_msg) -1) /* Stream Underflow */

#define ALO_SOK ((a_msg) 0)
#define ALO_SYIELD ((a_msg) 1) /* Route is Yield */
#define ALO_SDEAD ((a_msg) 2) /* Route is Dead */
#define ALO_SEXIT ((a_msg) 3) /* Exit Process */

/* Hook masks. */
#define ALO_HMNONE  0x0000
#define ALO_HMRAISE 0x0001 /* Raise Error */

typedef int a_bool;

/* VM integer number type. */
typedef int a_int;
/* VM float point number type. */
typedef double a_float;

/* Allocation function table. */
typedef struct alo_Alloc a_alloc;
/* Environment handle. */
typedef struct alo_Env* a_henv;

struct alo_Alloc {
	void* (*allocate)(void*, size_t);
	void* (*reallocate)(void*, void*, size_t, size_t);
	void (*deallocate)(void*, void*, size_t);
};

typedef int (*a_cfun)(a_henv env);

typedef ptrdiff_t a_kctx;
typedef int (*a_kfun)(a_henv env, a_msg msg, a_kctx ctx);

typedef ptrdiff_t (*a_ofun)(a_henv env, void* ctx, a_byte const* src, size_t len);
typedef ptrdiff_t (*a_ifun)(a_henv env, void* ctx, void const** pdst, size_t* plen);

ALO_EXPORT a_msg (alo_init)(void);
ALO_EXPORT a_msg (alo_create)(a_alloc const* af, void* ac, a_henv* penv);
ALO_EXPORT void (alo_destroy)(a_henv env);
ALO_EXPORT void (alo_sethook)(a_henv env, a_kfun kf, a_kctx kc, a_flags mask);

#define ALO_ATTR_VERSION 0x0001
#define ALO_ATTR_VARIANT 0x0002

ALO_EXPORT a_bool (alo_attri)(a_henv env, a_enum n, a_int* pi);

ALO_EXPORT size_t (alo_stacksize)(a_henv env);
ALO_EXPORT a_bool (alo_ensure)(a_henv env, size_t n);
ALO_EXPORT void (alo_settop)(a_henv env, ptrdiff_t n);
ALO_EXPORT ptrdiff_t (alo_absindex)(a_henv env, ptrdiff_t id);
ALO_EXPORT void (alo_push)(a_henv env, ptrdiff_t id);
ALO_EXPORT int (alo_pushex)(a_henv env, char const* sp, ...);
ALO_EXPORT int (alo_pushvex)(a_henv env, char const* sp, va_list varg);
ALO_EXPORT void (alo_pushnil)(a_henv env);
ALO_EXPORT void (alo_pushbool)(a_henv env, a_bool val);
ALO_EXPORT void (alo_pushint)(a_henv env, a_int val);
ALO_EXPORT void (alo_pushfloat)(a_henv env, a_float val);
ALO_EXPORT void (alo_pushptr)(a_henv env, void* val);
ALO_EXPORT char const* (alo_pushstr)(a_henv env, void const* src, size_t len);
ALO_EXPORT char const* (alo_pushfstr)(a_henv env, char const* fmt, ...);
ALO_EXPORT char const* (alo_pushvfstr)(a_henv env, char const* fmt, va_list varg);
ALO_EXPORT void (alo_pop)(a_henv env, ptrdiff_t id);
ALO_EXPORT void (alo_newtuple)(a_henv env, size_t n);
ALO_EXPORT void (alo_newlist)(a_henv env, size_t n);
ALO_EXPORT void (alo_newtable)(a_henv env, size_t n);
ALO_EXPORT void (alo_newcfun)(a_henv env, a_cfun f, size_t n);

#define alo_pushlstr(env,src) alo_pushstr(env, ""src, sizeof(src) - sizeof((src)[0]))

ALO_EXPORT size_t (alo_len)(a_henv env, ptrdiff_t id);
ALO_EXPORT int (alo_geti)(a_henv env, ptrdiff_t id, a_int key);
ALO_EXPORT void (alo_call)(a_henv env, size_t narg, ptrdiff_t nres);

ALO_EXPORT int (alo_tagof)(a_henv env, ptrdiff_t id);
ALO_EXPORT a_bool (alo_tobool)(a_henv env, ptrdiff_t id);
ALO_EXPORT a_int (alo_toint)(a_henv env, ptrdiff_t id);
ALO_EXPORT a_float (alo_tofloat)(a_henv env, ptrdiff_t id);
ALO_EXPORT char const* (alo_tostr)(a_henv env, ptrdiff_t id, size_t* plen);

ALO_EXPORT void (alo_fullgc)(a_henv env);

#define ALO_COMP_OPT_NOTHING 0x0
#define ALO_COMP_OPT_STATIC_LINK 0x1
#define ALO_COMP_OPT_STRICT_NAME 0x2
#define ALO_COMP_OPT_MODULE 0x4

#define ALO_COMP_OPT_DROP_DEBUG_LINE 0x100
#define ALO_COMP_OPT_DROP_DEBUG_NAME 0x200
#define ALO_COMP_OPT_DROP_DEBUG 0xff00

#define ALO_COMP_OPT_STOP_JIT 0x10000

#define ALO_COMP_OPT_CHECK_STMUF 0x1000000

ALO_EXPORT a_msg (alo_compile)(a_henv env, a_ifun fun, void* ctx, char const* name, unsigned int options);

#define ALO_DUMP_OPT_NOTHING 0x0
#define ALO_DUMP_OPT_CONST_POOL 0x1
#define ALO_DUMP_OPT_LOCAL 0x2
#define ALO_DUMP_OPT_LINE 0x4

ALO_EXPORT void (alo_dump)(a_henv env, ptrdiff_t id, unsigned int options);

#endif /* alo_h_ */
