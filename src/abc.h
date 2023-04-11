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
/*        id,    name,   fmt, ctl,    description                                  */ \
    _(   MOV,   "mov",   iAB, ___) /* R[a] := R[b]                                 */ \
/*=================================================================================*/ \
    _(   LDC,   "ldc",   iAB, ___) /* R[a] := C[b]                                 */ \
    _(   STC,   "stc",   iAB, ___) /* C[a] := R[b]                                 */ \
/*=================================================================================*/ \
    _(    KN,    "kn",   iAC, ___) /* R[a:a+c] := nil                              */ \
    _(    KF,    "kf",    iA, ___) /* R[a] := false                                */ \
    _(    KT,    "kt",    iA, ___) /* R[a] := true                                 */ \
    _(    KI,    "ki", iAsBx, ___) /* R[a] := int(b)                               */ \
    _(     K,     "k",  iABx, ___) /* R[a] := K[b]                                 */ \
/*=================================================================================*/ \
    _(   BKF,   "bkf",    iA, ___) /* R[a] := false; pc += 1                       */ \
    _(   BKT,   "bkt",    iA, ___) /* R[a] := true; pc += 1                        */ \
    _(    BZ,    "bz",    iA, ___) /* if R[a] { pc := pc + 1 }                     */ \
    _(   BNZ,   "bnz",    iA, ___) /* if !R[a] { pc := pc + 1 }                    */ \
    _(   BEQ,   "beq",   iAB, ___) /* if R[a] == R[b] { pc := pc + 1 }             */ \
    _(   BNE,   "bne",   iAB, ___) /* if !(R[a] == R[b]) { pc := pc + 1 }          */ \
    _(   BLT,   "blt",   iAB, ___) /* if R[a] < R[b] { pc := pc + 1 }              */ \
    _(  BNLT,  "bnlt",   iAB, ___) /* if !(R[a] < R[b]) { pc := pc + 1 }           */ \
    _(   BLE,   "ble",   iAB, ___) /* if R[a] <= R[b] { pc := pc + 1 }             */ \
    _(  BNLE,  "bnle",   iAB, ___) /* if !(R[a] <= R[b]) { pc := pc + 1 }          */ \
    _(  BEQI,  "beqi", iAsBx, ___) /* if R[a] == int(b) { pc := pc + 1 }           */ \
    _(  BNEI,  "bnei", iAsBx, ___) /* if !(R[a] == int(b)) { pc := pc + 1 }        */ \
    _(  BLTI,  "blti", iAsBx, ___) /* if R[a] < int(b) { pc := pc + 1 }            */ \
    _( BNLTI, "bnlti", iAsBx, ___) /* if !(R[a] < int(b)) { pc := pc + 1 }         */ \
    _(  BLEI,  "blei", iAsBx, ___) /* if R[a] <= int(b) { pc := pc + 1 }           */ \
    _( BNLEI, "bnlei", iAsBx, ___) /* if !(R[a] <= int(b)) { pc := pc + 1 }        */ \
    _(  BGTI,  "bgti", iAsBx, ___) /* if R[a] > int(b) { pc := pc + 1 }            */ \
    _( BNGTI, "bngti", iAsBx, ___) /* if !(R[a] > int(b)) { pc := pc + 1 }         */ \
    _(  BGEI,  "bgei", iAsBx, ___) /* if R[a] >= int(b) { pc := pc + 1 }           */ \
    _( BNGEI, "bngei", iAsBx, ___) /* if !(R[a] >= int(b)) { pc := pc + 1 }        */ \
/*=================================================================================*/ \
    _(   LDF,   "ldf",  iABx, ___) /* R[a] := func(F[b])                           */ \
    _(  LOOK,  "look",  iABC, ___) /* R[a:a+2] := look(R[b], K[c]), R[b]           */ \
    _( LOOKX, "lookx", iABEx, ___) /* R[a:a+2] := look(R[b], K[ex]), R[b]          */ \
    _(   GET,   "get",  iABC, ___) /* R[a] := R[b][R[c]]                           */ \
    _(  GETI,  "geti", iABsC, ___) /* R[a] := R[b][int(c)]                         */ \
    _(  GETS,  "gets",  iABC, ___) /* R[a] := R[b][K[c]: str]                      */ \
    _( GETSX, "getsx", iABEx, ___) /* R[a] := R[b][K[ex]: str]                     */ \
    _( CGETS, "cgets",  iABC, ___) /* R[a] := C[b][K[c]: str]                      */ \
    _(CGETSX,"cgetsx", iABEx, ___) /* R[a] := C[b][K[ex]: str]                     */ \
    _(   SET,   "set",  iABC, ___) /* R[b][R[c]] := R[a]                           */ \
    _(  SETI,  "seti", iABsC, ___) /* R[b][int(sc)] := R[a]                        */ \
    _(  SETS,  "sets",  iABC, ___) /* R[b][K[c]] := R[a]                           */ \
    _( SETSX, "setsx", iABEx, ___) /* R[b][K[ex]] := R[a]                          */ \
    _( CSETS, "csets",  iABC, ___) /* C[b][K[ex]] := R[a]                          */ \
    _(CSETSX,"csetsx", iABEx, ___) /* C[b][K[ex]] := R[a]                          */ \
    _(   NEG,   "neg",   iAB, ___) /* R[a] := -R[b]                                */ \
    _(  BNOT,  "bnot",   iAB, ___) /* R[a] := ~R[b]                                */ \
    _(   LEN,   "len",   iAB, ___) /* R[a] := #R[b]                                */ \
    _( UNBOX, "unbox",  iABC, _2n) /* R[a:a+c] := *R[b]                            */ \
    _(UNBOXV,"unboxv",  iABC, _2v) /* R[a:] := *R[b]                               */ \
    _(  TNEW,  "tnew",  iABC, ___) /* R[a] := (R[b:b+c])                           */ \
    _( TNEWM, "tnewm",  iABC, v2_) /* R[a] := (R[b:])                              */ \
    _(  LNEW,  "lnew",  iABx, ___) /* R[a] := [] (with size hint b)                */ \
    _(  LBOX,  "lbox",  iABC, ___) /* R[a] := [R[b:b+c]]                           */ \
    _( LBOXM, "lboxm",   iAB, ___) /* R[a] := [R[b:]]                              */ \
    _( LPUSH, "lpush",  iABC, ___) /* (R[a]: list) ++= R[b:b+c])                   */ \
    _(LPUSHM,"lpushm",   iAB, v2_) /* (R[a]: list) ++= R[b:])                      */ \
    _(  HNEW,  "hnew",  iABx, ___) /* R[a] := {} (with size hint b)                */ \
    _(   ADD,   "add",  iABC, ___) /* R[a] := R[b] + R[c]                          */ \
    _(   SUB,   "sub",  iABC, ___) /* R[a] := R[b] - R[c]                          */ \
    _(   MUL,   "mul",  iABC, ___) /* R[a] := R[b] * R[c]                          */ \
    _(   DIV,   "div",  iABC, ___) /* R[a] := R[b] / R[c]                          */ \
    _(   MOD,   "mod",  iABC, ___) /* R[a] := R[b] % R[c]                          */ \
    _(   SHL,   "shl",  iABC, ___) /* R[a] := R[b] << R[c]                         */ \
    _(   SHR,   "shr",  iABC, ___) /* R[a] := R[b] >> R[c]                         */ \
    _(  BAND,  "band",  iABC, ___) /* R[a] := R[b] & R[c]                          */ \
    _(   BOR,   "bor",  iABC, ___) /* R[a] := R[b] | R[c]                          */ \
    _(  BXOR,  "bxor",  iABC, ___) /* R[a] := R[b] ~ R[c]                          */ \
    _(  ADDI,  "addi", iABsC, ___) /* R[a] := R[b] + int(c)                        */ \
    _(  SUBI,  "subi", iABsC, ___) /* R[a] := R[b] - int(c)                        */ \
    _(  MULI,  "muli", iABsC, ___) /* R[a] := R[b] * int(c)                        */ \
    _(  DIVI,  "divi", iABsC, ___) /* R[a] := R[b] / int(c)                        */ \
    _(  MODI,  "modi", iABsC, ___) /* R[a] := R[b] % int(c)                        */ \
    _(  SHLI,  "shli", iABsC, ___) /* R[a] := R[b] << int(c)                       */ \
    _(  SHRI,  "shri", iABsC, ___) /* R[a] := R[b] >> int(c)                       */ \
    _( BANDI, "bandi", iABsC, ___) /* R[a] := R[b] & int(c)                        */ \
    _(  BORI,  "bori", iABsC, ___) /* R[a] := R[b] | int(c)                        */ \
    _( BXORI, "bxori", iABsC, ___) /* R[a] := R[b] ~ int(c)                        */ \
    _(  CALL,  "call",  iABC, n2n) /* R[a:a+c] := R[a](R[a+1:a+b])                 */ \
    _( CALLV, "callv",   iAB, n2v) /* R[a:] := R[a](R[a+1:a+b])                    */ \
    _( CALLM, "callm",   iAC, v2n) /* R[a:a+c] := R[a](R[a+1:])                    */ \
    _(CALLMV,"callmv",    iA, v2v) /* R[a:] := R[a](R[a+1:])                       */ \
    _(   CAT,   "cat",  iABC, n2_) /* R[a] := concat(R[b:b+c])                     */ \
    _(  CATM,  "catm",  iABC, v2_) /* R[a] := concat(R[b:])                        */ \
    _(   TBC,   "tbc",    iA, ___) /* mark R[A] to be closed                       */ \
    _( CLOSE, "close",    iA, ___) /* close(C[A:])                                 */ \
    _(     J,     "j",  isAx, ___) /* pc := pc + a                                 */ \
    _(  RET0,  "ret0",     i, _2x) /* return                                       */ \
    _(  RET1,  "ret1",    iA, _2x) /* return R[a]                                  */ \
    _(   RET,   "ret",   iAB, n2x) /* return R[a:a+b+1]                            */ \
    _(  RETM,  "retm",    iA, v2x) /* return R[a:]                                 */ \
    _(    FC,    "fc",     i, ___) /* call C function                              */ \
    _(    EX,    "ex",   iAx, ___) /*                                              */

enum OpCode {
#define BCNAME(id,...) BC_##id,
    ALO_BC_LIST(BCNAME)
#undef BCNAME
    BC__MAX
};

static_assert((BC_KF ^ 1) == BC_KT);
static_assert((BC_BKF ^ 1) == BC_BKT);
static_assert((BC_BZ ^ 1) == BC_BNZ);
static_assert((BC_BEQ ^ 1) == BC_BNE);
static_assert((BC_BLT ^ 1) == BC_BNLT);
static_assert((BC_BLE ^ 1) == BC_BNLE);
static_assert((BC_BLTI ^ 1) == BC_BNLTI);
static_assert((BC_BLEI ^ 1) == BC_BNLEI);
static_assert((BC_BGTI ^ 1) == BC_BNGTI);
static_assert((BC_BGEI ^ 1) == BC_BNGEI);
static_assert(BC_GETS + 1 == BC_GETSX);
static_assert(BC_CGETS + 1 == BC_CGETSX);
static_assert(BC_SETS + 1 == BC_SETSX);
static_assert(BC_CSETS + 1 == BC_CSETSX);
static_assert(BC_UNBOX + 1 == BC_UNBOXV);
static_assert(BC_CALL + 1 == BC_CALLV);
static_assert(BC_CALLM + 1 == BC_CALLMV);

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

#define INSN_CTRL_TO_MARG 0x1
#define INSN_CTRL_TO_VARG 0x2
#define INSN_CTRL_FROM_MARG 0x4
#define INSN_CTRL_FROM_VARG 0x8
#define INSN_CTRL_LEAVE 0x10

enum InsnCtrl {
	INSN_c___ = 0,
	INSN_c_2n = INSN_CTRL_TO_MARG,
	INSN_c_2v = INSN_CTRL_TO_VARG,
	INSN_c_2x = INSN_CTRL_LEAVE,
	INSN_cn2_ = INSN_CTRL_FROM_MARG,
	INSN_cn2n = INSN_CTRL_FROM_MARG|INSN_CTRL_TO_MARG,
	INSN_cn2v = INSN_CTRL_FROM_MARG|INSN_CTRL_TO_VARG,
	INSN_cn2x = INSN_CTRL_FROM_MARG|INSN_CTRL_LEAVE,
	INSN_cv2_ = INSN_CTRL_FROM_VARG,
	INSN_cv2n = INSN_CTRL_FROM_VARG|INSN_CTRL_TO_MARG,
	INSN_cv2v = INSN_CTRL_FROM_VARG|INSN_CTRL_TO_VARG,
	INSN_cv2x = INSN_CTRL_FROM_VARG|INSN_CTRL_LEAVE,
};

intern char const* const ai_bc_names[];
intern a_u8 const ai_bc_fmts[];
intern a_u8 const ai_bc_ctrls[];

always_inline a_bool bc_has_dual(a_enum op) {
	return op >= BC_KF && op <= BC_BNGEI;
}

always_inline a_bool bc_is_branch(a_enum op) {
	return op >= BC_BZ && op <= BC_BNGEI;
}

always_inline a_bool bc_is_leave(a_enum op) {
	return op >= BC_RET0 && op <= BC_RETM;
}

always_inline a_bool bc_can_unpack(a_enum op) {
	a_u8 ctrl1 = ai_bc_ctrls[op];
	if (!(ctrl1 & INSN_CTRL_TO_MARG)) return false;
	assume(op + 1 <= BC__MAX && (ai_bc_ctrls[op + 1] & INSN_CTRL_TO_VARG), "bad control op.");
	return true;
}

always_inline void insn_check(a_insn i) {
	a_enum op = bc_load_op(i);
	assume(op < BC__MAX, "bad opcode.");
	a_enum fmt = ai_bc_fmts[op];
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
