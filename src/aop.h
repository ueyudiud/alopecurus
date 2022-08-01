/**
 *@file aop.h
 */

#ifndef aop_h_
#define aop_h_

#include "adef.h"
#include "aerr.h"

typedef a_u32 a_uint;

#ifdef aloi_op_neg_int
# define ai_op_neg_int(env,a) aloi_op_neg_int(env, a)
#else
# define ai_op_neg_int(env,a) (-(a))
#endif

#ifdef aloi_op_neg_float
# define ai_op_neg_float(env,a) aloi_op_neg_float(env, a)
#else
# define ai_op_neg_float(env,a) (-(a))
#endif

#ifdef aloi_op_bnot_int
# define ai_op_bnot_int(env,a) aloi_op_bnot_int(env, a)
#else
# define ai_op_bnot_int(env,a) cast(a_i32, ~cast(a_uint, a))
#endif

#ifdef aloi_op_add_int
# define ai_op_add_int(env,a,b) aloi_op_add_int(env, a, b)
#else
# define ai_op_add_int(env,a,b) ((a) + (b))
#endif

#ifdef aloi_op_add_float
# define ai_op_add_float(env,a,b) aloi_op_add_float(env, a, b)
#else
# define ai_op_add_float(env,a,b) ((a) + (b))
#endif

#ifdef aloi_op_sub_int
# define ai_op_sub_int(env,a,b) aloi_op_sub_int(env, a, b)
#else
# define ai_op_sub_int(env,a,b) ((a) - (b))
#endif

#ifdef aloi_op_sub_float
# define ai_op_sub_float(env,a,b) aloi_op_sub_float(env, a, b)
#else
# define ai_op_sub_float(env,a,b) ((a) - (b))
#endif

#ifdef aloi_op_mul_int
# define ai_op_mul_int(env,a,b) aloi_op_mul_int(env, a, b)
#else
# define ai_op_mul_int(env,a,b) ((a) * (b))
#endif

#ifdef aloi_op_mul_float
# define ai_op_mul_float(env,a,b) aloi_op_mul_float(env, a, b)
#else
# define ai_op_mul_float(env,a,b) ((a) * (b))
#endif

inline void ai_op_check_divisor(a_henv env, a_int b) {
	if (unlikely(b == 0)) {
		ai_err_raise(env, ALO_EINVAL, "attempt to divide 0.");
	}
}

#ifdef aloi_op_div_int
# define ai_op_div_int(env,a,b) aloi_op_div_int(env, a, b)
#else
inline a_int ai_op_div_int(a_henv env, a_int a, a_int b) {
	ai_op_check_divisor(env, b);
	return a / b;
}
#endif

#ifdef aloi_op_div_float
# define ai_op_div_float(env,a,b) aloi_op_div_float(env, a, b)
#else
# define ai_op_div_float(env,a,b) ((a) / (b))
#endif

#ifdef aloi_op_mod_int
# define ai_op_mod_int(env,a,b) aloi_op_mod_int(env, a, b)
#else
inline a_int ai_op_mod_int(a_henv env, a_int a, a_int b) {
	ai_op_check_divisor(env, b);
	switch (b) {
		case 1: return a;
		case -1: return -a;
		default: {
			a_int c = a % b;
			return (b ^ c) > 0 ? c : b + c;
		}
	}
}
#endif

#ifdef aloi_op_mod_float
# define ai_op_mod_float(env,a,b) aloi_op_div_float(a, b)
#else
# define ai_op_mod_float(env,a,b) __builtin_fmod(a, b)
#endif

#ifdef aloi_op_shl_int
# define ai_op_shl_int(env,a,b) aloi_op_shl_int(env, a, b)
#else
# define ai_op_shl_int(env,a,b) cast(a_i32, cast(a_u32, a) << cast(a_u32, b));
#endif

#ifdef aloi_op_shr_int
# define ai_op_shr_int(env,a,b) aloi_op_shr_int(env, a, b)
#else
# define ai_op_shr_int(env,a,b) cast(a_i32, cast(a_u32, a) >> cast(a_u32, b));
#endif

#ifdef aloi_op_band_int
# define ai_op_band_int(env,a,b) aloi_op_band_int(env, a, b)
#else
# define ai_op_band_int(env,a,b) cast(a_i32, cast(a_u32, a) & cast(a_u32, b));
#endif

#ifdef aloi_op_bor_int
# define ai_op_bor_int(env,a,b) aloi_op_band_int(env, a, b)
#else
# define ai_op_bor_int(env,a,b) cast(a_i32, cast(a_u32, a) | cast(a_u32, b));
#endif

#ifdef aloi_op_bxor_int
# define ai_op_bxor_int(env,a,b) aloi_op_band_int(env, a, b)
#else
# define ai_op_bxor_int(env,a,b) cast(a_i32, cast(a_u32, a) ^ cast(a_u32, b));
#endif

#endif /* aop_h_ */
