/*
 * adef.h
 */

#ifndef adef_h_
#define adef_h_

#include <limits.h>
#include <string.h>

#include "aarch.h"
#include "alo.h"
#include "aauxlib.h"

#define M_false 0
#define M_true 1

#define M_cat0(a,b) a##b
#define M_cat(a,b) M_cat0(a,b)

#define M_str0(a) #a
#define M_str(a) M_str0(a)

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
	char const* _ptr;
	a_usize _len;
} a_lstr;

typedef a_u32 a_uint;

typedef a_u32 a_hash;
typedef a_u32 a_insn;

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

#define ISIZE_MAX PTRDIFF_MAX
#define ISIZE_MIN PTRDIFF_MIN

/* Other literals. */

#define null NULL
#define false ((a_bool) M_false)
#define true ((a_bool) !false)

#if __STDC_VERSION__ >= 201112L
# define ALO_C11 M_true
#else
# define ALO_C11 M_false
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

/* Special generic functors. */

#define init(p) *(p) = (typeof(*(p)))
#define cast(t,e) ((t) (e))
#define bit_cast(t,e) ({ __auto_type _e = e; t _t; static_assert(sizeof(_e) == sizeof(t)); memcpy(&_t, &_e, sizeof(t)); _t; })
#define quiet(e...) ((void) ((void) 0, ##e))
#define ptr2int(p) ((a_usize) (void const*) {p})
#define addr_diff(p,q) ({ void *_p = (p), *_q = (q); _p - _q; })
#define int2ptr(t,a) ((typeof(t)*) (a_usize) {a})
#define ref_of(t,a) (*int2ptr(t, a))
#define ptr_disp(t,p,d) int2ptr(t, ptr2int(p) + (d))
#define from_member(t,f,v) ({ __auto_type _v = v; quiet(_v == &((t*) 0)->f); int2ptr(t, ptr2int(_v) - offsetof(t, f)); })
#define fallthrough __attribute__((__fallthrough__))

/* Utility functions. */

#define max(a,b) ({ typeof(0 ? (a) : (b)) _a = (a), _b = (b); _a >= _b ? _a : _b; })
#define min(a,b) ({ typeof(0 ? (a) : (b)) _a = (a), _b = (b); _a <= _b ? _a : _b; })
#define swap(a,b) ({ __auto_type _a = &(a); __auto_type _b = &(b); quiet(_a == _b); typeof(a) _t; _t = *_a; *_a = *_b; quiet(*_b = _t); })
#define expect(e,v) __builtin_expect(e, v)
#define likely(e) expect(!!(e), true)
#define unlikely(e) expect(!!(e), false)
#define trap() __builtin_trap()
#define unreachable() __builtin_unreachable()

/* Error handling functions. */

#define catch_(e,n,...) for (__auto_type n = (e); unlikely(n); n = 0)
#define catch(e,n...) catch_(e, ##n, _e)
#define try(e) catch(e) { return _e; }

intern a_noret ai_dbg_panic(char const* fmt, ...);

#if defined(ALOI_CHECK_ASSUME) && defined(ALO_LIB)
# define panic(m...) ai_dbg_panic(""m)
#else
# define panic(...) unreachable()
#endif

#define assume(e,m...) ((void) (!!(e) || (panic(m), false)))

/* Special control flow. */

#define run if (true)
#define loop while (true)

/**
 ** Fill memory with zero bits.
 */
#define memclr(dst,len) memset(dst, 0, len)

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

always_inline a_usize ceil_pow2_usize(a_usize a) {
    return (~usizec(0) >> clz_usize(a)) + 1;
}

#define pad_to_raw(s,g) (((s) + (g) - 1) & ~((g) - 1))

always_inline a_usize pad_to(a_usize size, a_usize granularity) {
	assume(granularity != 0 && (granularity & (granularity - 1)) == 0, "bad granularity.");
	return pad_to_raw(size, granularity);
}

#endif /* adef_h_ */
