/*
 * adef.h
 */

#ifndef adef_h_
#define adef_h_

#include <limits.h>

#include "aarch.h"
#include "alo.h"
#include "aaux.h"

#define M_cat(a,b) M_cat0(a,b)
#define M_cat0(a,b) a##b

#define M_str(a) M_str0(a)
#define M_str0(a) #a

#define M_sym(a) M_cat(a,__LINE__)

/* Special attributes. */

#undef intern

#if defined(__ELF__)
# define intern __attribute__((__visibility__("hidden"))) extern
#else
# define intern extern
#endif

#undef inline
#undef noinline
#undef naked
#undef unused

#define inline __attribute__((__always_inline__, __gnu_inline__)) extern inline
#define noinline __attribute__((__noinline__))
#define naked __attribute__((__naked__))
#define unused __attribute__((__unused__))

/* Types. */

#define a_none __attribute__((__noreturn__)) void

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

#define ISIZE_MAX INT64_MAX
#define ISIZE_MIN INT64_MIN

/* Other literals. */

#undef null
#undef false
#undef true
#undef zero

#define null NULL
#define false ((a_bool) 0)
#define true  (!false)
#define zero(t) ((t) {0})

/* Special generic functors. */

#undef new
#undef cast
#undef quiet
#undef addr_of
#undef ptr_of
#undef from_member
#undef fallthrough

#define new(t) (t)
#define cast(t,e) ((t) (e))
#define bcast(t,e) ({ typeof(e) _e[sizeof(e) == sizeof(t) ? 1 : -1] = {e}; *cast(typeof(t)*, &_e); })
#define fpcast(t,e) cast(t, cast(void*, e))
#define quiet(e...) ((void) ((void) 0, ##e))
#define addr_of(e) cast(a_usize, (quiet((typeof(*(e))*) null), e))
#define ptr_of(t,e) ({ typeof(e) _e = (e); quiet(&_e == zero(a_usize*)); cast(typeof(t)*, _e); })
#define from_member(t,f,v) ({ typeof(v) _v = (v); quiet(_v == &cast(typeof(t)*, null)->f); cast(typeof(t)*, cast(void*, v) - offsetof(t, f)); })
#define fallthrough __attribute__((__fallthrough__))

/* Utility functions. */

#undef max
#undef min
#undef swap
#undef expect
#undef likely
#undef unlikely
#undef check
#undef trap
#undef unreachable

#define max(a,b) ({ typeof(a) _a = (a); typeof(b) _b = (b); quiet(&_a == &_b); _a >= _b ? _a : _b; })
#define min(a,b) ({ typeof(a) _a = (a); typeof(b) _b = (b); quiet(&_a == &_b); _a <= _b ? _a : _b; })
#define swap(a,b) ({ typeof(a)* _a = &(a); typeof(b)* _b = &(b); quiet(_a == _b); typeof(a) _t; _t = *_a; *_a = *_b; quiet(*_b = _t); })
#define expect(e,v) __builtin_expect(e, v)
#define likely(e) expect(!!(e), true)
#define unlikely(e) expect(!!(e), false)
#define check(e) ({ typeof(e) _e = (e); if (unlikely(_e != ALO_SOK)) return _e; })
#define trap() __builtin_trap()
#define unreachable() __builtin_unreachable()

#if defined(ALOI_DEBUG)
intern a_none ai_dbg_panic(char const* fmt, ...);
# define panic(m...) ai_dbg_panic(""m)
#else
# define panic(...) unreachable()
#endif

#define assume(e,m...) (!!(e) || (panic(m), false))

/* Special control flow. */

#undef run
#undef loop
#undef defer

#define run if (true)
#define loop while (true)
#define defer(e) for (a_bool M_sym(__) = true; M_sym(__); quiet(e), M_sym(__) = false)

/* Extend arithmetic operations. */

#undef memclr

inline void* ai_def_memory_clear(void* dst, a_usize len) {
	return __builtin_memset(dst, 0, len);
}

/**
 ** Fill memory with zero bits.
 */
#define memclr(dst,len) ai_def_memory_clear(dst, len)

inline a_usize2 mul_usize(a_usize a, a_usize b) {
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
    inline a_bool checked_add_##t(a_##t a, a_##t b, a_##t* d) { return __builtin_add_overflow(a, b, d); } \
    inline a_bool checked_sub_##t(a_##t a, a_##t b, a_##t* d) { return __builtin_sub_overflow(a, b, d); } \
    inline a_bool checked_mul_##t(a_##t a, a_##t b, a_##t* d) { return __builtin_mul_overflow(a, b, d); }

NUM_TYPE_LIST(FUNDEF)

#undef FUNDEF

#undef NUM_TYPE_LIST

inline a_usize ceil_pow2m1_usize(a_usize a) {
    return ~usizec(0) >> __builtin_clz(a);
}

#endif /* adef_h_ */
