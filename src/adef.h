/*
 * adef.h
 */

#ifndef adef_h_
#define adef_h_

#include <limits.h>

#include "aarch.h"
#include "alo.h"
#include "aauxlib.h"

#define M_false 0
#define M_true 1

#define M_cat0(a,b) a##b
#define M_cat(a,b) M_cat0(a,b)

#define M_str0(a) #a
#define M_str(a) M_str0(a)

#define ALO_COMP_OPT_ALOC1 0x1000000
#define ALO_COMP_OPT_EVAL 0x2000000

/**
 ** Debug aasm mode. If enabled, all assumption will be checked and panic if failed.
 */
#ifdef ALOI_DEBUG
# define ALO_DEBUG M_true
#else
# define ALO_DEBUG M_false
#endif

/**
 ** Check memory related function in strict mode.
 ** If enabled, the following strategy will be used:
 ** - the stack will be force to reallocate for each grow.
 ** - the full GC will be triggered in each allocation.
 */
#ifdef ALOI_STRICT_MEMORY_CHECK
# define ALO_STRICT_MEMORY_CHECK M_true
#else
# define ALO_STRICT_MEMORY_CHECK M_false
#endif

/**
 ** Check stack limit in C API.
 */
#ifdef ALOI_STRICT_STACK_CHECK
# define ALO_STRICT_STACK_CHECK M_true
#else
# define ALO_STRICT_STACK_CHECK M_false
#endif

/* Special attributes. */

#undef intern

#if defined(__ELF__)
# define intern __attribute__((__visibility__("hidden"))) extern
#else
# define intern extern
#endif

#undef always_inline
#undef never_inline
#undef naked
#undef unused

#define always_inline __attribute__((__always_inline__, __gnu_inline__)) extern __inline__
#define never_inline __attribute__((__noinline__))
#define naked __attribute__((__naked__))
#define unused __attribute__((__unused__))

/* Types. */
#define a_none ALO_NORETURN void

#if ALO_M64
typedef __int128_t a_isize2;
typedef __uint128_t a_usize2;
#else
typedef int64_t a_isize2;
typedef uint64_t a_usize2;
#endif

typedef struct {
	char const* _ptr;
	a_usize _len;
} a_lstr;

typedef struct { a_i32 _; } a_x32;

typedef a_u32 a_hash;
typedef a_u32 a_insn;

/* Literal function. */

#undef u8c
#undef u16c
#undef u32c
#undef u64c
#undef i8c
#undef i16c
#undef i32c
#undef i64c

#define u8c  UINT8_C
#define u16c UINT16_C
#define u32c UINT32_C
#define u64c UINT64_C
#define i8c  INT8_C
#define i16c INT16_C
#define i32c INT32_C
#define i64c INT64_C

#define usizec u64c
#define isizec i64c

#define ISIZE_MAX PTRDIFF_MAX
#define ISIZE_MIN PTRDIFF_MIN

/* Other literals. */

#undef null
#undef false
#undef true
#undef zero
#undef nil

#define null NULL
#define false ((a_bool) M_false)
#define true  (!false)
#define zero(t) ((t) {0})

#if __STDC_VERSION__ >= 201112L
# ifndef static_assert
#  define static_assert(e) _Static_assert(e, "assert failed.")
# endif
#else
# ifdef __COUNTER__
#  define ALO_ASSERT_NAME M_cat(ai_assert_,__COUNTER__)
# else
#  define ALO_ASSERT_NAME M_cat(ai_assert_,__LINE__)
# endif
# define static_assert(e) extern (ALO_ASSERT_NAME)(int assert_failed[(e) ? 1 : -1])
#endif

/* Special generic functors. */

#undef new
#undef cast
#undef bit_cast
#undef quiet
#undef null_of
#undef addr_of
#undef ptr_of
#undef ptr_diff
#undef ptr_disp
#undef from_member
#undef fallthrough

#define new(t) (t)
#define cast(t,e) ((t) (e))
#define bit_cast(t,e) ({ typeof(e) _e[sizeof(e) == sizeof(t) ? 1 : -1] = {e}; t _t; __builtin_memcpy(&_t, _e, sizeof(t)); _t; })
#define quiet(e...) ((void) ((void) 0, ##e))
#define null_of(t) ((t*) 0)
#define addr_of(e) cast(a_usize, e)
#define ptr_of(t,e) ({ typeof(e) _e = (e); quiet(&_e == zero(a_usize*)); cast(typeof(t)*, _e); })
#define ptr_diff(p,q) ({ void *_p = (p), *_q = (q); _p - _q; })
#define ptr_disp(t,p,d) ptr_of(t, addr_of(p) + (d))
#define from_member(t,f,v) ({ typeof(v) _v = (v); quiet(_v == null_of(typeof(((t*) 0)->f))); ptr_of(t, addr_of(_v) - offsetof(t, f)); })
#define fallthrough __attribute__((__fallthrough__))

/**
 ** Optional index type support.
 */

/**
 ** Internal used `nil` literal.
 ** Used in optional array based table reference to
 ** represent a empty value.
 */
#define x32c(l) wrap(i32c(l))

#define nil x32c(0)

#define unwrap_unsafe(e) ((e)._)
#define unwrap(e) ({ a_x32 _x = (e); assume(!is_nil(_x)); unwrap_unsafe(_x); })
#define wrap(v)  (new(a_x32) { v })

#define is_nil(e) (unwrap_unsafe(e) == unwrap_unsafe(nil))

/* Utility functions. */

#undef max
#undef min
#undef swap
#undef expect
#undef likely
#undef unlikely
#undef trap
#undef unreachable

#define max(a,b) ({ typeof(0 ? (a) : (b)) _a = (a), _b = (b); _a >= _b ? _a : _b; })
#define min(a,b) ({ typeof(0 ? (a) : (b)) _a = (a), _b = (b); _a <= _b ? _a : _b; })
#define swap(a,b) ({ typeof(a)* _a = &(a); typeof(b)* _b = &(b); quiet(_a == _b); typeof(a) _t; _t = *_a; *_a = *_b; quiet(*_b = _t); })
#define expect(e,v) __builtin_expect(e, v)
#define likely(e) expect(!!(e), true)
#define unlikely(e) expect(!!(e), false)
#define trap() __builtin_trap()
#define unreachable() __builtin_unreachable()

/* Error handling functions. */

#undef catch
#undef try

#define catch(e,f,a...) ({ typeof(e) _e = (e); if (unlikely(_e)) { f(_e, ##a); } })
#define try(e) catch(e, return)

#if ALO_DEBUG && defined(ALO_LIB)
intern a_none ai_dbg_panic(char const* fmt, ...);
# define panic(m...) ai_dbg_panic(""m)
#else
# define panic(...) unreachable()
#endif

#define assume(e,m...) ((void) (!!(e) || (panic(m), false)))

/* Special control flow. */

#undef run
#undef loop

#define run if (true)
#define loop while (true)

/* Extend arithmetic operations. */

#undef memclr

/**
 ** Fill memory with zero bits.
 */
#define memclr(dst,len) __builtin_memset(dst, 0, len)

always_inline a_usize2 mul_usize(a_usize a, a_usize b) {
    return cast(a_usize2, a) * b;
}

#define NUM_TYPE_LIST(_) \
    _(i8) \
    _(i16) \
    _(i32) \
    _(i64) \
    _(isize) \
    _(u8) \
    _(u16) \
    _(u32) \
    _(u64) \
    _(usize)

#define FUNDEF(t) \
    always_inline a_bool checked_add_##t(a_##t a, a_##t b, a_##t* d) { return __builtin_add_overflow(a, b, d); } \
    always_inline a_bool checked_sub_##t(a_##t a, a_##t b, a_##t* d) { return __builtin_sub_overflow(a, b, d); } \
    always_inline a_bool checked_mul_##t(a_##t a, a_##t b, a_##t* d) { return __builtin_mul_overflow(a, b, d); }

NUM_TYPE_LIST(FUNDEF)

#undef FUNDEF

#undef NUM_TYPE_LIST

always_inline a_u32 clz_usize(a_usize a) {
#if ALO_M64
	return __builtin_clzll(a);
#else
	return __builtin_clz(a);
#endif
}

always_inline a_usize ceil_pow2m1_usize(a_usize a) {
    return likely(a != 0) ? ~usizec(0) >> clz_usize(a) : 0;
}

#define pad_to_raw(s,g) (((s) + (g) - 1) & ~((g) - 1))

always_inline a_usize pad_to(a_usize size, a_usize granularity) {
	assume(granularity != 0 && (granularity & (granularity - 1)) == 0, "bad granularity.");
	return pad_to_raw(size, granularity);
}

#endif /* adef_h_ */
