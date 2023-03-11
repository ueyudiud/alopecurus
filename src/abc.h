/**
 *@file abc.h
 */

#ifndef abc_h_
#define abc_h_

#include "adef.h"

#define BC_OFF_OP u32c(0)
#define BC_OFF_A u32c(8)
#define BC_OFF_B u32c(16)
#define BC_OFF_C u32c(24)

#define BC_MASK_OP (u32c(0xff) << BC_OFF_OP)
#define BC_MASK_A (u32c(0xff) << BC_OFF_A)
#define BC_MASK_AX (u32c(0xffffff) << BC_OFF_A)
#define BC_MASK_B (u32c(0xff) << BC_OFF_B)
#define BC_MASK_BX (u32c(0xffff) << BC_OFF_B)
#define BC_MASK_C (u32c(0xff) << BC_OFF_C)

#define BC_MAX_OP UINT8_MAX
#define BC_MAX_A UINT8_MAX
#define BC_MAX_AX ((u32c(1) << 24) - 1)
#define BC_MAX_SAX ((i32c(1) << 23) - 1)
#define BC_MIN_SAX (-(i32c(1) << 23))
#define BC_MAX_B UINT8_MAX
#define BC_MAX_BX UINT16_MAX
#define BC_MAX_SBX INT16_MAX
#define BC_MIN_SBX INT16_MIN
#define BC_MAX_C UINT8_MAX
#define BC_MAX_SC INT8_MAX
#define BC_MIN_SC INT8_MIN

#define bc_wrap_op(op) (cast(a_insn, cast(a_u8, op)) << BC_OFF_OP)
#define bc_wrap_a(a) (cast(a_insn, cast(a_u8, a)) << BC_OFF_A)
#define bc_wrap_ax(a) (cast(a_insn, a) << BC_OFF_A)
#define bc_wrap_sax(a) (cast(a_insn, cast(a_i32, a)) << BC_OFF_A)
#define bc_wrap_b(b) (cast(a_insn, cast(a_u8, b)) << BC_OFF_B)
#define bc_wrap_bx(b) (cast(a_insn, cast(a_u16, b)) << BC_OFF_B)
#define bc_wrap_sbx(b) (cast(a_insn, cast(a_u16, cast(a_i16, b))) << BC_OFF_B)
#define bc_wrap_c(c) (cast(a_insn, cast(a_u8, c)) << BC_OFF_C)
#define bc_wrap_sc(c) (cast(a_insn, cast(a_u8, cast(a_i8, c))) << BC_OFF_C)

#define bc_make_iabc(op,a,b,c) (bc_wrap_op(op) | bc_wrap_a(a) | bc_wrap_b(b) | bc_wrap_c(c))
#define bc_make_iabsc(op,a,b,c) (bc_wrap_op(op) | bc_wrap_a(a) | bc_wrap_b(b) | bc_wrap_sc(c))
#define bc_make_iab(op,a,b) (bc_wrap_op(op) | bc_wrap_a(a) | bc_wrap_b(b))
#define bc_make_iac(op,a,c) (bc_wrap_op(op) | bc_wrap_a(a) | bc_wrap_c(c))
#define bc_make_ia(op,a) (bc_wrap_op(op) | bc_wrap_a(a))
#define bc_make_i(op) bc_wrap_op(op)
#define bc_make_iabx(op,a,b) (bc_wrap_op(op) | bc_wrap_a(a) | bc_wrap_bx(b))
#define bc_make_iasbx(op,a,b) (bc_wrap_op(op) | bc_wrap_a(a) | bc_wrap_sbx(b))
#define bc_make_iax(op,a) (bc_wrap_op(op) | bc_wrap_ax(a))
#define bc_make_isax(op,a) (bc_wrap_op(op) | bc_wrap_sax(a))

#define bc_load_op(i) cast(a_u8, cast(a_u32, i) >> BC_OFF_OP)
#define bc_load_a(i) cast(a_u8, cast(a_u32, i) >> BC_OFF_A)
#define bc_load_ax(i) (cast(a_u32, i) >> BC_OFF_A)
#define bc_load_sax(i) (cast(a_i32, i) >> BC_OFF_A)
#define bc_load_b(i) cast(a_u8, cast(a_u32, i) >> BC_OFF_B)
#define bc_load_bx(i) cast(a_u16, cast(a_u32, i) >> BC_OFF_B)
#define bc_load_sbx(i) cast(a_i16, cast(a_i32, i) >> BC_OFF_B)
#define bc_load_c(i) cast(a_u8, cast(a_u32, i) >> BC_OFF_C)
#define bc_load_sc(i) cast(a_i8, cast(a_i32, i) >> BC_OFF_C)

always_inline void bc_swap_op(a_insn* i, a_u32 op) { *i = (*i & ~BC_MASK_OP) | bc_wrap_op(op); }
always_inline void bc_swap_a(a_insn* i, a_u32 a) { *i = (*i & ~BC_MASK_A) | bc_wrap_a(a); }
always_inline void bc_swap_ax(a_insn* i, a_u32 a) { *i = (*i & ~BC_MASK_AX) | bc_wrap_ax(a); }
always_inline void bc_swap_sax(a_insn* i, a_i32 a) { *i = (*i & ~BC_MASK_AX) | bc_wrap_sax(a); }
always_inline void bc_swap_b(a_insn* i, a_u32 b) { *i = (*i & ~BC_MASK_B) | bc_wrap_b(b); }
always_inline void bc_swap_bx(a_insn* i, a_u32 b) { *i = (*i & ~BC_MASK_BX) | bc_wrap_bx(b); }
always_inline void bc_swap_sbx(a_insn* i, a_u32 b) { *i = (*i & ~BC_MASK_BX) | bc_wrap_sbx(b); }
always_inline void bc_swap_c(a_insn* i, a_u32 c) { *i = (*i & ~BC_MASK_C) | bc_wrap_c(c); }
always_inline void bc_swap_sc(a_insn* i, a_i32 c) { *i = (*i & ~BC_MASK_C) | bc_wrap_sc(c); }

#define ALO_BC_LIST(_) \
/*        id,    name,   fmt,   a,   b,   c,    description                                  */ \
    _(   NOP,   "nop",     i, ___, ___, ___) /*                                              */ \
    _(   MOV,   "mov",   iAB, reg, reg, ___) /* R[a] := R[b]                                 */ \
/*=====================================Duality Opcodes=======================================*/ \
    _(    KF,    "kf",    iA, reg, ___, ___) /* R[a] := false                                */ \
    _(    KT,    "kt",    iA, reg, ___, ___) /* R[a] := true                                 */ \
    _(   BKF,   "bkf",    iA, reg, ___, ___) /* R[a] := false; pc += 1                       */ \
    _(   BKT,   "bkt",    iA, reg, ___, ___) /* R[a] := true; pc += 1                        */ \
    _(    BZ,    "bz",    iA, reg, ___, ___) /* if R[a] { pc := pc + 1 }                     */ \
    _(   BNZ,   "bnz",    iA, reg, ___, ___) /* if !R[a] { pc := pc + 1 }                    */ \
    _(   BEQ,   "beq",   iAB, reg, reg, ___) /* if R[a] == R[b] { pc := pc + 1 }             */ \
    _(   BNE,   "bne",   iAB, reg, reg, ___) /* if !(R[a] == R[b]) { pc := pc + 1 }          */ \
    _(   BLT,   "blt",   iAB, reg, reg, ___) /* if R[a] < R[b] { pc := pc + 1 }              */ \
    _(  BNLT,  "bnlt",   iAB, reg, reg, ___) /* if !(R[a] < R[b]) { pc := pc + 1 }           */ \
    _(   BLE,   "ble",   iAB, reg, reg, ___) /* if R[a] <= R[b] { pc := pc + 1 }             */ \
    _(  BNLE,  "bnle",   iAB, reg, reg, ___) /* if !(R[a] <= R[b]) { pc := pc + 1 }          */ \
    _(  BEQI,  "beqi", iAsBx, reg, val, val) /* if R[a] == int(b) { pc := pc + 1 }           */ \
    _(  BNEI,  "bnei", iAsBx, reg, val, val) /* if !(R[a] == int(b)) { pc := pc + 1 }        */ \
    _(  BLTI,  "blti", iAsBx, reg, val, val) /* if R[a] < int(b) { pc := pc + 1 }            */ \
    _( BNLTI, "bnlti", iAsBx, reg, val, val) /* if !(R[a] < int(b)) { pc := pc + 1 }         */ \
    _(  BLEI,  "blei", iAsBx, reg, val, val) /* if R[a] <= int(b) { pc := pc + 1 }           */ \
    _( BNLEI, "bnlei", iAsBx, reg, val, val) /* if !(R[a] <= int(b)) { pc := pc + 1 }        */ \
    _(  BGTI,  "bgti", iAsBx, reg, val, val) /* if R[a] > int(b) { pc := pc + 1 }            */ \
    _( BNGTI, "bngti", iAsBx, reg, val, val) /* if !(R[a] > int(b)) { pc := pc + 1 }         */ \
    _(  BGEI,  "bgei", iAsBx, reg, val, val) /* if R[a] >= int(b) { pc := pc + 1 }           */ \
    _( BNGEI, "bngei", iAsBx, reg, val, val) /* if !(R[a] >= int(b)) { pc := pc + 1 }        */ \
/*===========================================================================================*/ \
    _(   LDC,   "ldc",   iAB, reg, cap, ___) /* R[a] := *C[b]                                */ \
    _(   STC,   "stc",   iAB, cap, reg, ___) /* *C[a] := R[b]                                */ \
    _(    KN,    "kn",   iAC, reg, ___, num) /* R[a:a+c] := nil                              */ \
    _(    KI,    "ki", iAsBx, reg, val, val) /* R[a] := int(b)                               */ \
    _(     K,     "k",  iABx, reg, kst, kst) /* R[a] := K[b]                                 */ \
    _(   LDF,   "ldf",  iABx, reg, kst, kst) /* R[a] := func(F[b] )                          */ \
    _(  CMOV,  "cmov",   iAB, reg, cap, ___) /* R[a] := C[b]                                 */ \
    _(   GET,   "get",  iABC, reg, reg, reg) /* R[a] := R[b][R[c]]                           */ \
    _(  GETI,  "geti", iABsC, reg, reg, val) /* R[a] := R[b][int(c)]                         */ \
    _(  GETS,  "gets",  iABC, reg, reg, kst) /* R[a] := R[b][K[c]: str]                      */ \
    _( GETSX, "getsx", iABEx, reg, reg, ___) /* R[a] := R[b][K[ex]: str]                     */ \
    _( CGETS, "cgets",  iABC, reg, cap, kst) /* R[a] := C[b][K[c]: str]                      */ \
    _(CGETSX,"cgetsx", iABEx, reg, cap, ___) /* R[a] := C[b][K[ex]: str]                     */ \
    _(   SET,   "set",  iABC, reg, reg, reg) /* R[b][R[c]] := R[a]                           */ \
    _(  SETI,  "seti", iABsC, reg, reg, val) /* R[b][int(sc)] := R[a]                        */ \
    _(  SETS,  "sets",  iABC, reg, reg, val) /* R[b][K[c]] := R[a]                           */ \
    _( SETSX, "setsx", iABEx, reg, reg, ___) /* R[b][K[ex]] := R[a]                          */ \
    _(   NEG,   "neg",   iAB, reg, reg, ___) /* R[a] := -R[b]                                */ \
    _(   LEN,   "len",   iAB, reg, reg, ___) /* R[a] := #R[b]                                */ \
    _( UNBOX, "unbox",  iABC, reg, reg, num) /* R[a:a+c-1] := *R[b]                          */ \
    _(  TNEW,  "tnew",  iABC, reg, reg, num) /* R[a] := (R[b:b+c-1])                         */ \
    _(  LNEW,  "lnew",  iABx, reg, num, num) /* R[a] := [] (with size hint bx)               */ \
    _(   ADD,   "add",  iABC, reg, reg, reg) /* R[a] := R[b] + R[c]                          */ \
    _(   SUB,   "sub",  iABC, reg, reg, reg) /* R[a] := R[b] - R[c]                          */ \
    _(   MUL,   "mul",  iABC, reg, reg, reg) /* R[a] := R[b] * R[c]                          */ \
    _(   DIV,   "div",  iABC, reg, reg, reg) /* R[a] := R[b] / R[c]                          */ \
    _(   MOD,   "mod",  iABC, reg, reg, reg) /* R[a] := R[b] % R[c]                          */ \
    _(   SHL,   "shl",  iABC, reg, reg, reg) /* R[a] := R[b] << R[c]                         */ \
    _(   SHR,   "shr",  iABC, reg, reg, reg) /* R[a] := R[b] >> R[c]                         */ \
    _(  BAND,  "band",  iABC, reg, reg, reg) /* R[a] := R[b] & R[c]                          */ \
    _(   BOR,   "bor",  iABC, reg, reg, reg) /* R[a] := R[b] | R[c]                          */ \
    _(  BXOR,  "bxor",  iABC, reg, reg, reg) /* R[a] := R[b] ~ R[c]                          */ \
    _(  ADDI,  "addi", iABsC, reg, reg, val) /* R[a] := R[b] + int(c)                        */ \
    _(  SUBI,  "subi", iABsC, reg, reg, val) /* R[a] := R[b] - int(c)                        */ \
    _(  MULI,  "muli", iABsC, reg, reg, val) /* R[a] := R[b] * int(c)                        */ \
    _(  DIVI,  "divi", iABsC, reg, reg, val) /* R[a] := R[b] / int(c)                        */ \
    _(  MODI,  "modi", iABsC, reg, reg, val) /* R[a] := R[b] % int(c)                        */ \
    _(  SHLI,  "shli", iABsC, reg, reg, val) /* R[a] := R[b] << int(c)                       */ \
    _(  SHRI,  "shri", iABsC, reg, reg, val) /* R[a] := R[b] >> int(c)                       */ \
    _( BANDI, "bandi", iABsC, reg, reg, val) /* R[a] := R[b] & int(c)                        */ \
    _(  BORI,  "bori", iABsC, reg, reg, val) /* R[a] := R[b] | int(c)                        */ \
    _( BXORI, "bxori", iABsC, reg, reg, val) /* R[a] := R[b] ~ int(c)                        */ \
    _(  CALL,  "call",  iABC, reg, num, num) /* R[a:a+c-1] := R[a](R[a+1:a+b])               */ \
    _(   CAT,   "cat",  iABC, reg, reg, num) /* R[a] := concat(R[b:b+c-1])                   */ \
    _(     J,     "j",  isAx, off, off, off) /* pc := pc + a                                 */ \
    _( CLOSE, "close",    iA, reg, ___, ___) /* close(C[A:])                                 */ \
    _(   RET,   "ret",   iAB, reg, num, ___) /* return R[a:a+b+1]                            */ \
    _(  RETV,  "retv",    iA, reg, ___, ___) /* return R[a:]                                 */ \
    _(  RET1,  "ret1",    iA, reg, ___, ___) /* return R[a]                                  */ \
    _(  RET0,  "ret0",     i, ___, ___, ___) /* return                                       */ \
    _(    FC,    "fc",     i, ___, ___, ___) /* call C function                              */ \
    _(    EX,    "ex",   iAx, val, val, val) /*                                              */

enum OpCode {
#define BCNAME(id,n,f,a,b,c) BC_##id,
    ALO_BC_LIST(BCNAME)
#undef BCNAME
    BC__MAX
};

enum InsnFormat {
	INSN_i,
	INSN_iA,
	INSN_iAx,
	INSN_isAx,
	INSN_iAB,
	INSN_iABx,
	INSN_iAsBx,
	INSN_iABC,
	INSN_iABsC,
	INSN_iAC,
	INSN_iABEx
};

intern char const* const ai_bc_names[];
intern a_u8 const ai_bc_formats[];

always_inline a_bool bc_has_dual_op(a_enum op) {
	return op >= BC_KF && op <= BC_BNGEI;
}

always_inline a_bool bc_is_branch_op(a_enum op) {
	return op >= BC_BZ && op <= BC_BNGEI;
}

always_inline void insn_check(a_insn i) {
	a_enum op = bc_load_op(i);
	assume(op < BC__MAX, "bad opcode.");
	a_enum fmt = ai_bc_formats[op];
	switch (fmt) {
		case INSN_i: {
			assume(bc_load_ax(i) == 0, "bad operand.");
			break;
		}
		case INSN_iA: {
			assume(bc_load_bx(i) == 0, "bad operand.");
			break;
		}
		case INSN_iAB:
		case INSN_iABEx: {
			assume(bc_load_c(i) == 0, "bad operand.");
			break;
		}
		case INSN_iAC: {
			assume(bc_load_b(i) == 0, "bad operand.");
			break;
		}
		default: {
			break;
		}
	}
}

#endif /* abc_h_ */
