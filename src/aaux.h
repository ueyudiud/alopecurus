/*
 * aaux.h
 */

#ifndef aaux_h_
#define aaux_h_

#include <stdint.h>

#include "alo.h"

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

ALO_EXPORT a_henv aloL_create(void);

ALO_EXPORT a_msg aloL_readstr(a_henv env, char const* src, a_usize len, char const* name, a_u32 options);
ALO_EXPORT a_msg aloL_readfile(a_henv env, char const* fname, a_u32 options);

#endif /* aaux_h_ */
