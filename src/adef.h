/**
 *@file adef.h
 */

#ifndef adef_h_
#define adef_h_

#include <limits.h>
#include <string.h>

#include "acfg.h"
#include "alo.h"
#include "aauxlib.h"

/**
 ** The implement variant index.
 */
#define ALO_VARIANT 1

#define ALO_C11 (__STDC_VERSION__ >= 201112L)
#define ALO_C23 (__STDC_VERSION__ >= 202311L)

#define ALO_FALSE 0
#define ALO_TRUE 1

#define M_cat0(a,b) a##b
#define M_cat(a,b) M_cat0(a,b)

#define ALO_COMP_OPT_LOSSEN 0x1000000
#define ALO_COMP_OPT_EVAL 0x2000000

/* Special attributes. */

#undef intern

#if defined(__ELF__)
# define intern __attribute__((__visibility__("hidden"))) extern
#else
# define intern extern
#endif

#define always_inline __attribute__((__always_inline__, __gnu_inline__)) extern __inline__
#define never_inline __attribute__((__noinline__))
#define naked __attribute__((__naked__))
#define unused __attribute__((__unused__))

/* Types. */
#define a_noret ALO_NORETURN void

typedef struct {
	char const* ptr;
	a_usize len;
} a_lstr;

typedef a_u32 a_uint;

typedef a_u32 a_hash;
typedef a_u32 a_insn;

/* The protected function. */
typedef void (*a_pfun)(a_henv, void*);
/* The error catching function. */
typedef void (*a_efun)(a_henv, void*, a_msg);

/* Literal function. */

#define u8c  UINT8_C
#define u16c UINT16_C
#define u32c UINT32_C
#define u64c UINT64_C
#define i8c  INT8_C
#define i16c INT16_C
#define i32c INT32_C
#define i64c INT64_C

#if ALO_M64
# define usizec u64c
# define isizec i64c
#elif ALO_M32
# define usizec u32c
# define isizec i32c
#endif

#undef ISIZE_MAX
#undef ISIZE_MIN

#define ISIZE_MAX PTRDIFF_MAX
#define ISIZE_MIN PTRDIFF_MIN

/* Other literals. */

#define null NULL
#define false ((a_bool) ALO_FALSE)
#define true ((a_bool) ALO_TRUE)

#if ALO_C23
# define auto auto
#else
# define auto __auto_type
#endif

#if ALO_C11
# ifndef static_assert
#  define static_assert(e) _Static_assert(e, "assert failed.")
# endif
#else
# ifdef __COUNTER__
#  define ALO_ASSERT_NAME M_cat(ai_assert_,__COUNTER__)
# else
#  define ALO_ASSERT_NAME M_cat(ai_assert_,__LINE__)
# endif
# define static_assert(e) extern void (ALO_ASSERT_NAME)(int assert_failed[(e) ? 1 : -1])
#endif

/* Special control flow. */

#define run if (true)
#define loop while (true)

#if ALO_C23
# include <stdchkint.h>
# include <stdbit.h>
#endif

#undef max
#undef min

/* Special generic functors. */

#define init(p) *(p) = (typeof(*(p)))
#define cast(t,e) ((t) (e))
#define bit_cast(t,e) ({ auto _b = e; t _t; static_assert(sizeof(_b) == sizeof(t)); memcpy(&_t, &_b, sizeof(t)); _t; })
#define addr_diff(p,q) ({ void const *_p = (p), *_q = (q); _p - _q; })
#define int2ptr(t,a) ((typeof(t)*) (a_usize) {a})
#define ref_of(t,a) (*int2ptr(t, a))
#define ptr_disp(t,p,d) int2ptr(t, ptr2int(p) + (d))
#define from_member(t,f,v) int2ptr(t, ptr2int(v) - offsetof(t, f))
#define fallthrough __attribute__((__fallthrough__))

always_inline a_usize ptr2int(void const* p) {
    return bit_cast(a_usize, p);
}

#if ALO_C23
always_inline void quiet(...) {}
#else
always_inline void quiet_(unused int dummy, ...) {}
#define quiet(e...) quiet_(0, ##e)
#endif

/* Utility functions. */

#define max(a,b) ({ typeof(0 ? (a) : (b)) _a = (a), _b = (b); _a >= _b ? _a : _b; })
#define min(a,b) ({ typeof(0 ? (a) : (b)) _a = (a), _b = (b); _a <= _b ? _a : _b; })
#define swap(a,b) ({ auto _a = &(a); auto _b = &(b); quiet(_a == _b); typeof(a) _t; _t = *_a; *_a = *_b; quiet(*_b = _t); })
#define expect(e,v) __builtin_expect(e, v)
#define likely(e) expect(!!(e), true)
#define unlikely(e) expect(!!(e), false)

always_inline a_noret unreachable() {
    __builtin_unreachable();
}

always_inline a_noret trap() {
    __builtin_trap();
}

/* Error handling functions. */

#define catch_(e,n,...) for (auto n = (e); unlikely(n); n = 0)

/**
 ** Do the statement inside catch block if 'e' is not zero.
 ** The error is assumed happening rarely.
 */
#define catch(e,n...) catch_(e, ##n, _e)
/**
 ** Leave current function and return 'e' if 'e' is not zero.
 */
#define try(e) catch(e) { return _e; }

intern a_noret ai_dbg_panic(char const* fmt, ...);

#if defined(ALOI_CHECK_ASSUME)
# define assume_only(s...) s
#else
# define assume_only(...)
#endif

#if defined(ALOI_CHECK_ASSUME) && defined(ALO_LIB)
# define panic(m...) ai_dbg_panic(""m)
#else
# define panic(...) unreachable()
#endif

#define assume(e,m...) quiet((e) || (panic(m), false))

/**
 ** Fill memory with zero bits.
 */
always_inline void memclr(void* dst, a_usize len) {
    memset(dst, 0, len);
}

#if !ALO_C23
/* Compact with C23 standard function. */
# define ckd_add(d,a,b) __builtin_add_overflow(a, b, d)
# define ckd_sub(d,a,b) __builtin_sub_overflow(a, b, d)
# define ckd_mul(d,a,b) __builtin_mul_overflow(a, b, d)

# define count_leading_zero(a) __builtin_clz(a)
# define ceil_power_of_two(a) ((~((typeof(a)) 0) >> count_leading_zero(a)) + 1)
#else
/* Redirect to C23 defined function name. */
# define count_leading_zero(a) stdc_leading_zeros(a)
# define ceil_power_of_two(a) stdc_bit_ceil(a)
#endif

#define try_add(a,b) ({ typeof((a) + (b)) _c; try (ckd_add(&_c, a, b)); _c; })
#define try_sub(a,b) ({ typeof((a) - (b)) _c; try (ckd_sub(&_c, a, b)); _c; })
#define try_mul(a,b) ({ typeof((a) * (b)) _c; try (ckd_mul(&_c, a, b)); _c; })

#define align_to(s,g) (((s) + (g) - 1) & ~((g) - 1))

#endif /* adef_h_ */
