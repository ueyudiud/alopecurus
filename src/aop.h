/**
 *@file aop.h
 */

#ifndef aop_h_
#define aop_h_

#include "adef.h"
#include "aerr.h"

typedef a_u32 a_uint;

enum {
	OP__NONE    = 0x00,

	OP_ADD      = 0x01,
	OP_SUB      = 0x02,
	OP_MUL      = 0x03,
	OP_DIV      = 0x04,
	OP_MOD      = 0x05,
	OP_SHL      = 0x06,
	OP_SHR      = 0x07,
	OP_BIT_AND  = 0x08,
	OP_BIT_OR   = 0x09,
	OP_BIT_XOR  = 0x0A,
	OP_AND      = 0x0B,
	OP_OR       = 0x0C,

	OP_LT       = 0x10,
	OP_GE       = 0x11,
	OP_LE       = 0x12,
	OP_GT       = 0x13,
	OP_EQ       = 0x14,
	OP_NE       = 0x15,
	OP_IS       = 0x16,
	OP_IS_NOT   = 0x17,
	OP_AS       = 0x18,
	OP_AS_OR    = 0x19,
	OP_IN       = 0x1A,
	OP_NOT_IN   = 0x1B,

	OP_NEG      = 0x0D,
	OP_BIT_INV  = 0x0E,
	OP_NOT      = 0x0F,
	OP_UNBOX    = 0x1C,
	OP_LEN      = 0x20,
	OP_UNPACK   = 0x34,

	OP_OPTION   = 0x1D,
	OP_MERGE    = 0x1E,
	OP_OR_ELSE  = 0x1F,

	OP_VA_PUSH  = 0x20,
	OP_VA_POP   = 0x21,
	OP_RETURN   = 0x22,

	OP_VA_FIT   = 0x30,
	OP_VA_FILL  = 0x31,
	OP_TNEW     = 0x32,
	OP_CALL     = 0x33
};

#ifdef aloi_op_neg_int
# define ai_op_neg_int(a) aloi_op_neg_int(a)
#else
# define ai_op_neg_int(a) (-(a))
#endif

#ifdef aloi_op_neg_float
# define ai_op_neg_float(a) aloi_op_neg_float(a)
#else
# define ai_op_neg_float(a) (-(a))
#endif

#ifdef aloi_op_bnot_int
# define ai_op_bnot_int(a) aloi_op_bnot_int(a)
#else
# define ai_op_bnot_int(a) cast(a_int, ~cast(a_uint, a))
#endif

#ifdef aloi_op_add_int
# define ai_op_add_int(a,b) aloi_op_add_int(a, b)
#else
# define ai_op_add_int(a,b) ((a) + (b))
#endif

#ifdef aloi_op_add_float
# define ai_op_add_float(a,b) aloi_op_add_float(a, b)
#else
# define ai_op_add_float(a,b) ((a) + (b))
#endif

#ifdef aloi_op_sub_int
# define ai_op_sub_int(a,b) aloi_op_sub_int(a, b)
#else
# define ai_op_sub_int(a,b) ((a) - (b))
#endif

#ifdef aloi_op_sub_float
# define ai_op_sub_float(a,b) aloi_op_sub_float(a, b)
#else
# define ai_op_sub_float(a,b) ((a) - (b))
#endif

#ifdef aloi_op_mul_int
# define ai_op_mul_int(a,b) aloi_op_mul_int(a, b)
#else
# define ai_op_mul_int(a,b) ((a) * (b))
#endif

#ifdef aloi_op_mul_float
# define ai_op_mul_float(a,b) aloi_op_mul_float(a, b)
#else
# define ai_op_mul_float(a,b) ((a) * (b))
#endif

#ifdef aloi_op_div_int
# define ai_op_div_int(a,b) aloi_op_div_int(a, b)
#else
# define ai_op_div_int(a,b) ((a) / (b))
#endif

#ifdef aloi_op_div_float
# define ai_op_div_float(a,b) aloi_op_div_float(a, b)
#else
# define ai_op_div_float(a,b) ((a) / (b))
#endif

#ifdef aloi_op_mod_int
# define ai_op_mod_int(a,b) aloi_op_mod_int(a, b)
#else
# define ai_op_mod_int(a,b) ((a) % (b))
#endif

#ifdef aloi_op_mod_float
# define ai_op_mod_float(a,b) aloi_op_div_float(a, b)
#else
# define ai_op_mod_float(a,b) __builtin_fmod(a, b)
#endif

#ifdef aloi_op_shl_int
# define ai_op_shl_int(a,b) aloi_op_shl_int(a, b)
#else
# define ai_op_shl_int(a,b) cast(a_int, cast(a_uint, a) << cast(a_uint, b))
#endif

#ifdef aloi_op_shr_int
# define ai_op_shr_int(a,b) aloi_op_shr_int(a, b)
#else
# define ai_op_shr_int(a,b) cast(a_int, cast(a_uint, a) >> cast(a_uint, b))
#endif

#ifdef aloi_op_band_int
# define ai_op_band_int(a,b) aloi_op_band_int(a, b)
#else
# define ai_op_band_int(a,b) cast(a_int, cast(a_uint, a) & cast(a_uint, b))
#endif

#ifdef aloi_op_bor_int
# define ai_op_bor_int(a,b) aloi_op_band_int(a, b)
#else
# define ai_op_bor_int(a,b) cast(a_int, cast(a_uint, a) | cast(a_uint, b))
#endif

#ifdef aloi_op_bxor_int
# define ai_op_bxor_int(a,b) aloi_op_band_int(a, b)
#else
# define ai_op_bxor_int(a,b) cast(a_int, cast(a_uint, a) ^ cast(a_uint, b))
#endif

#ifdef aloi_op_eq_int
# define ai_op_eq_int(a,b) aloi_op_eq_int(a, b)
#else
# define ai_op_eq_int(a,b) ((a) == (b))
#endif

#ifdef aloi_op_eq_float
# define ai_op_eq_float(a,b) aloi_op_eq_float(a, b)
#else
# define ai_op_eq_float(a,b) ((a) == (b))
#endif

#ifdef aloi_op_lt_int
# define ai_op_lt_int(a,b) aloi_op_lt_int(a, b)
#else
# define ai_op_lt_int(a,b) ((a) < (b))
#endif

#ifdef aloi_op_lt_float
# define ai_op_lt_float(a,b) aloi_op_lt_float(a, b)
#else
# define ai_op_lt_float(a,b) ((a) < (b))
#endif

#ifdef aloi_op_le_int
# define ai_op_le_int(a,b) aloi_op_le_int(a, b)
#else
# define ai_op_le_int(a,b) ((a) <= (b))
#endif

#ifdef aloi_op_le_float
# define ai_op_le_float(a,b) aloi_op_le_float(a, b)
#else
# define ai_op_le_float(a,b) ((a) <= (b))
#endif

always_inline a_int ai_op_bin_int(a_int a, a_int b, a_enum op) {
	switch (op) {
		case OP_ADD:
			return ai_op_add_int(a, b);
		case OP_SUB:
			return ai_op_sub_int(a, b);
		case OP_MUL:
			return ai_op_mul_int(a, b);
		case OP_DIV:
			return ai_op_div_int(a, b);
		case OP_MOD:
			return ai_op_mod_int(a, b);
		case OP_SHL:
			return ai_op_shl_int(a, b);
		case OP_SHR:
			return ai_op_shr_int(a, b);
		case OP_BIT_AND:
			return ai_op_band_int(a, b);
		case OP_BIT_OR:
			return ai_op_bor_int(a, b);
		case OP_BIT_XOR:
			return ai_op_bxor_int(a, b);
		default:
			panic("bad operation.");
	}
}

always_inline a_float ai_op_bin_float(a_float a, a_float b, a_enum op) {
	switch (op) {
		case OP_ADD:
			return ai_op_add_float(a, b);
		case OP_SUB:
			return ai_op_sub_float(a, b);
		case OP_MUL:
			return ai_op_mul_float(a, b);
		case OP_DIV:
			return ai_op_div_float(a, b);
		case OP_MOD:
			return ai_op_mod_float(a, b);
		default:
			panic("bad operation.");
	}
}

always_inline a_bool ai_op_cmp_int(a_int a, a_int b, a_enum op) {
	switch (op) {
		case OP_EQ:
			return ai_op_eq_int(a, b);
		case OP_NE:
			return !ai_op_eq_int(a, b);
		case OP_LT:
			return ai_op_lt_int(a, b);
		case OP_LE:
			return ai_op_le_int(a, b);
		case OP_GT:
			return ai_op_lt_int(b, a);
		case OP_GE:
			return ai_op_le_int(b, a);
		default:
			panic("bad operation.");
	}
}

always_inline a_bool ai_op_cmp_float(a_float a, a_float b, a_enum op) {
	switch (op) {
		case OP_EQ:
			return ai_op_eq_float(a, b);
		case OP_NE:
			return !ai_op_eq_float(a, b);
		case OP_LT:
			return ai_op_lt_float(a, b);
		case OP_LE:
			return ai_op_le_float(a, b);
		case OP_GT:
			return ai_op_lt_float(b, a);
		case OP_GE:
			return ai_op_le_float(b, a);
		default:
			panic("bad operation.");
	}
}

intern char const* const ai_op_names[];

#endif /* aop_h_ */
