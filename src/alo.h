/**
 *@file alo.h
 */

#ifndef alo_h_
#define alo_h_

#include <stddef.h>
#include <stdarg.h>
#include <stdint.h>

#define ALO ALO_VERSION_NUMBER

#define ALO_MAJOR_VERSION 0
#define ALO_MINOR_VERSION 1
#define ALO_PATCH_VERSION 0

#define ALO_VERSION_NUMBER (ALO_MAJOR_VERSION * 100 + ALO_MINOR_VERSION)

#define ALO_NAME "Alopecurus"
#define ALO_VERSION_STRING_BUILDER2(major,minor) ALO_NAME" "#major"."#minor
#define ALO_VERSION_STRING_BUILDER(major,minor) ALO_VERSION_STRING_BUILDER2(major,minor)
#define ALO_VERSION_STRING ALO_VERSION_STRING_BUILDER(ALO_MAJOR_VERSION,ALO_MINOR_VERSION)
#define ALO_VERSION_FULL_STRING_BUILDER2(major,minor,patch) ALO_VERSION_STRING_BUILDER2(major,minor)"."#patch
#define ALO_VERSION_FULL_STRING_BUILDER(major,minor,patch) ALO_VERSION_FULL_STRING_BUILDER2(major,minor,patch)
#define ALO_VERSION_FULL_STRING ALO_VERSION_FULL_STRING_BUILDER(ALO_MAJOR_VERSION,ALO_MINOR_VERSION,ALO_PATCH_VERSION)

#define ALO_COPYRIGHT ALO_VERSION_FULL_STRING" Copyright (C) 2022-2023 ueyudiud"

typedef unsigned char a_byte;

typedef int8_t a_i8;
typedef int16_t a_i16;
typedef int32_t a_i32;
typedef int64_t a_i64;
typedef ptrdiff_t a_isize;

typedef uint8_t a_u8;
typedef uint16_t a_u16;
typedef uint32_t a_u32;
typedef uint64_t a_u64;
typedef size_t a_usize;

typedef float a_f32;
typedef double a_f64;

#ifdef ALO_BUILD_AS_DLL
# ifdef ALO_LIB
#  define ALO_EXPORT __declspec(dllexport)
# else
#  define ALO_EXPORT __declspec(dllimport)
# endif
#else
# define ALO_EXPORT extern
#endif

#if __STDC_VERSION__ >= 201112L
# define ALO_NORETURN _Noreturn
#elif defined(__GNUC__)
# define ALO_NORETURN __attribute__((__noreturn__))
#else
# define ALO_NORETURN
#endif

typedef a_i32 a_msg;
typedef a_u32 a_enum;
typedef a_usize a_flags;
typedef a_i32 a_tag;

#define ALO_STACK_INDEX_ERROR (-1000)
#define ALO_STACK_INDEX_CAPTURE_BASE (-2000)
#define ALO_STACK_INDEX_GLOBAL (-3000)
#define ALO_STACK_INDEX_EMPTY (-4000)

#define ALO_STACK_INDEX_CAPTURE(n) (ALO_STACK_INDEX_CAPTURE_BASE + (n))

#define ALO_TEMPTY (-1)

#define ALO_TNIL 0
#define ALO_TBOOL 1
#define ALO_TINT 2
#define ALO_TFLOAT 3
#define ALO_TPTR 4
#define ALO_TSTR 5
#define ALO_TTUPLE 6
#define ALO_TLIST 7
#define ALO_TTABLE 8
#define ALO_TFUNC 9
#define ALO_TROUTE 10
#define ALO_TTYPE 11

#define ALO_TUSER 12

#define ALO_EIO ((a_msg) -9) /* Foreign IO Error */
#define ALO_ERAISE ((a_msg) -8) /* User Raised Error */
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
typedef a_i32 a_int;
typedef a_u32 a_uint;
/* VM float point number type. */
typedef a_f64 a_float;
/* Iterator for external used. */
typedef a_u32 a_ritr[1];

/* Allocation function table. */
typedef struct alo_Alloc a_alloc;
/* Environment handle. */
typedef struct alo_Env* a_henv;
/* Type handle. */
typedef struct alo_Type* a_htype;

struct alo_Alloc {
	void* (*allocate)(void*, a_usize);
	void* (*reallocate)(void*, void*, a_usize, a_usize);
	void (*deallocate)(void*, void*, a_usize);
};

typedef a_u32 (*a_cfun)(a_henv env);

typedef a_isize a_hctx;
typedef void (*a_hfun)(a_henv env, a_msg msg, a_hctx ctx);

typedef a_i32 (*a_ofun)(a_henv env, void* ctx, a_byte const* src, a_usize len);
typedef a_i32 (*a_ifun)(a_henv env, void* ctx, void const** pdst, a_usize* plen);

ALO_EXPORT a_msg (alo_init)(void);
ALO_EXPORT a_msg (alo_create)(a_alloc const* af, void* ac, a_henv* penv);
ALO_EXPORT void (alo_destroy)(a_henv env);
ALO_EXPORT void (alo_setpanic)(a_henv env, a_cfun f);
ALO_EXPORT void (alo_sethook)(a_henv env, a_hfun kf, a_hctx kc, a_flags mask);

#define ALO_ATTR_VERSION 0x0001
#define ALO_ATTR_VARIANT 0x0002

ALO_EXPORT a_bool (alo_attri)(a_henv env, a_enum n, a_int* pi);

ALO_EXPORT a_usize (alo_stacksize)(a_henv env);
ALO_EXPORT a_bool (alo_ensure)(a_henv env, a_usize n);
ALO_EXPORT void (alo_settop)(a_henv env, a_isize n);
ALO_EXPORT a_isize (alo_absindex)(a_henv env, a_isize id);
ALO_EXPORT void (alo_push)(a_henv env, a_isize id);
ALO_EXPORT a_tag (alo_pushex)(a_henv env, char const* sp, ...);
ALO_EXPORT a_tag (alo_pushvex)(a_henv env, char const* sp, va_list varg);
ALO_EXPORT void (alo_pushnil)(a_henv env);
ALO_EXPORT void (alo_pushbool)(a_henv env, a_bool val);
ALO_EXPORT void (alo_pushint)(a_henv env, a_int val);
ALO_EXPORT void (alo_pushfloat)(a_henv env, a_float val);
ALO_EXPORT void (alo_pushptr)(a_henv env, void* val);
ALO_EXPORT char const* (alo_pushstr)(a_henv env, void const* src, a_usize len);
ALO_EXPORT char const* (alo_pushntstr)(a_henv env, char const* src);
ALO_EXPORT char const* (alo_pushfstr)(a_henv env, char const* fmt, ...);
ALO_EXPORT char const* (alo_pushvfstr)(a_henv env, char const* fmt, va_list varg);
ALO_EXPORT void (alo_pushtype)(a_henv env, a_htype hnd);
ALO_EXPORT void (alo_pushroute)(a_henv env);
ALO_EXPORT void (alo_xmove)(a_henv src, a_henv dst, a_usize n);
ALO_EXPORT void (alo_pop)(a_henv env, a_isize id);
ALO_EXPORT void (alo_newtuple)(a_henv env, a_usize n);
ALO_EXPORT void (alo_newlist)(a_henv env, a_usize n);
ALO_EXPORT void (alo_newtable)(a_henv env, a_usize n);
ALO_EXPORT void (alo_newcfun)(a_henv env, a_cfun f, a_usize n);
ALO_EXPORT a_henv (alo_newroute)(a_henv env, a_usize ss);

#define alo_pushlstr(env,src) alo_pushstr(env, ""src, sizeof(src) - sizeof((src)[0]))

ALO_EXPORT a_usize (alo_rawlen)(a_henv env, a_isize id);
ALO_EXPORT a_tag (alo_rawgeti)(a_henv env, a_isize id, a_int key);
ALO_EXPORT void (alo_insert)(a_henv env, a_isize id);
ALO_EXPORT void (alo_call)(a_henv env, a_usize narg, a_isize nres);
ALO_EXPORT a_msg (alo_pcall)(a_henv env, a_usize narg, a_isize nres, a_usize nsav);
ALO_EXPORT ALO_NORETURN void (alo_raise)(a_henv env);
ALO_EXPORT a_msg (alo_resume)(a_henv env);
ALO_EXPORT void (alo_yield)(a_henv env);

ALO_EXPORT a_tag (alo_tagof)(a_henv env, a_isize id);
ALO_EXPORT a_bool (alo_tobool)(a_henv env, a_isize id);
ALO_EXPORT a_int (alo_toint)(a_henv env, a_isize id);
ALO_EXPORT a_float (alo_tofloat)(a_henv env, a_isize id);
ALO_EXPORT char const* (alo_tolstr)(a_henv env, a_isize id, a_usize* plen);

#define alo_tostr(env,id) alo_tolstr(env, id, NULL)

#define ALO_NEWTYPE_FLAG_STATIC 0x0001

/* Type operations. */
ALO_EXPORT void (alo_newtype)(a_henv env, char const* name, a_flags options);
ALO_EXPORT a_htype (alo_typeof)(a_henv env, a_isize id);
ALO_EXPORT char const* (alo_typename)(a_henv env, a_htype type);
ALO_EXPORT a_msg (alo_looktype)(a_henv env, char const* src, a_usize len);
ALO_EXPORT a_htype (alo_opentype)(a_henv env, a_isize id);
ALO_EXPORT void (alo_closetype)(a_henv env, a_htype hnd);

/* Table operations. */
ALO_EXPORT a_usize (alo_hnext)(a_henv env, a_isize id, a_ritr itr);
ALO_EXPORT a_bool (alo_hinsert)(a_henv env, a_isize id, a_ritr itr);
ALO_EXPORT a_bool (alo_hremove)(a_henv env, a_isize id, a_ritr itr);

ALO_EXPORT void (alo_fullgc)(a_henv env);

#define ALO_COMP_OPT_NOTHING 0x0
#define ALO_COMP_OPT_STATIC_LINK 0x1
#define ALO_COMP_OPT_STRICT_NAME 0x2
#define ALO_COMP_OPT_MODULE 0x4
#define ALO_COMP_OPT_CALL_EXACTLY_ONCE 0x8
#define ALO_COMP_OPT_STRIP_DEBUG 0x10

#define ALO_COMP_OPT_STOP_JIT 0x10000

ALO_EXPORT a_msg (alo_compile)(a_henv env, a_ifun fun, void* ctx, a_isize id_env,
							   a_isize id_name, a_isize id_file, a_flags options);

#define ALO_DUMP_OPT_NOTHING 0x0
#define ALO_DUMP_OPT_CONST_POOL 0x1
#define ALO_DUMP_OPT_LOCAL 0x2
#define ALO_DUMP_OPT_LINE 0x4

ALO_EXPORT void (alo_dump)(a_henv env, a_isize id, a_flags options);

typedef struct {
	unsigned char kind;
	char const* file;
	size_t line;
	char const* name;
	void* _hnd;
} alo_Debug;

#define ALO_DEBUG_START 1

#define ALO_DEBUG_GET 2
#define ALO_DEBUG_GET_FLAG_SOURCE 0x0001
#define ALO_DEBUG_GET_FLAG_THEN_NEXT 0x10000

#define ALO_DEBUG_NEXT 3

ALO_EXPORT a_bool (alo_debug)(a_henv env, alo_Debug* dbg, a_enum n, a_flags w);

#endif /* alo_h_ */
