/**
 *@file aop.h
 */

#ifndef aop_h_
#define aop_h_

#include "adef.h"

enum {
#define SYMDEF(n,r) TM_##n,
    SYM_TM(SYMDEF, M_void, M_void)
#undef SYMDEF
    TM__LIMIT,
	TM__FAST_MAX = TM___eq__
};

enum UnaryOp {
    OP_NEG, OP_BNOT
};

enum BinaryOp {
	OP__NOT_BIN,
	/* Arithmetic operators */
	OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_MOD, OP_POW,
	/* Bitwise operators */
	OP_SHL, OP_SHR, OP_BIT_AND, OP_BIT_OR, OP_BIT_XOR,
	/* Compare operators */
	OP_EQ, OP_NE, OP_LT, OP_LE, OP_GT, OP_GE,
	/* Relation operations */
	OP_IS, OP_IS_NOT, OP_IN, OP_NOT_IN,
	/* Logical operators */
	OP_AND, OP_OR,
};

always_inline a_enum ai_op_bin2tm(a_enum op) {
	assume(op >= OP_ADD && op <= OP_BIT_XOR);
	return op - OP_ADD + TM___add__;
}

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

#ifdef aloi_op_pow_float
# define ai_op_pow_float(a,b) aloi_op_pow_float(a, b)
#else
# define ai_op_pow_float(a,b) __builtin_pow(a, b)
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
        case OP_POW:
            return ai_op_pow_float(a, b);
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

#endif /* aop_h_ */
