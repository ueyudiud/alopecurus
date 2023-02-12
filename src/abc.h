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
#define bc_make_iab(op,a,b) bc_make_iabc(op, a, b, 0)
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

inline void bc_swap_op(a_insn* i, a_u32 op) { *i = (*i & ~BC_MASK_OP) | bc_wrap_op(op); }
inline void bc_swap_a(a_insn* i, a_u32 a) { *i = (*i & ~BC_MASK_A) | bc_wrap_a(a); }
inline void bc_swap_ax(a_insn* i, a_u32 a) { *i = (*i & ~BC_MASK_AX) | bc_wrap_ax(a); }
inline void bc_swap_sax(a_insn* i, a_i32 a) { *i = (*i & ~BC_MASK_AX) | bc_wrap_sax(a); }
inline void bc_swap_b(a_insn* i, a_u32 b) { *i = (*i & ~BC_MASK_B) | bc_wrap_b(b); }
inline void bc_swap_bx(a_insn* i, a_u32 b) { *i = (*i & ~BC_MASK_BX) | bc_wrap_bx(b); }
inline void bc_swap_sbx(a_insn* i, a_u32 b) { *i = (*i & ~BC_MASK_BX) | bc_wrap_sbx(b); }
inline void bc_swap_c(a_insn* i, a_u32 c) { *i = (*i & ~BC_MASK_C) | bc_wrap_c(c); }
inline void bc_swap_sc(a_insn* i, a_i32 c) { *i = (*i & ~BC_MASK_C) | bc_wrap_sc(c); }

#define ALO_BC_LIST(_) \
/*        id,    name,   a,   b,   c,    description                                         */ \
    _(   NOP,   "nop", ___, ___, ___) /*                                                     */ \
    _(   MOV,   "mov", reg, reg, ___) /* R[a] := R[b]                                        */ \
    _(   LDC,   "ldc", reg, cap, ___) /* R[a] := *C[b]                                       */ \
    _(   STC,   "stc", cap, reg, ___) /* *C[a] := R[b]                                       */ \
/*======================================Duality Opcodes======================================*/ \
    _(    KF,    "kf", reg, ___, ___) /* R[a] := false                                       */ \
    _(    KT,    "kt", reg, ___, ___) /* R[a] := true                                        */ \
    _(   BNZ,   "bnz", ___, reg, ___) /* if !R[b] { pc := pc + 1 }                           */ \
    _(    BZ,    "bz", ___, reg, ___) /* if R[b] { pc := pc + 1 }                            */ \
    _(   TNZ,   "tnz", reg, reg, ___) /* R[a] := if R[b] { false } else { true }             */ \
    _(    TZ,    "tz", reg, reg, ___) /* R[a] := if R[b] { true } else { false }             */ \
    _(   BEQ,   "beq", ___, reg, reg) /* if R[b] == R[c] { pc := pc + 1 }                    */ \
    _(   BNE,   "bne", ___, reg, reg) /* if !(R[b] == R[c]) { pc := pc + 1 }                 */ \
    _(   TEQ,   "teq", reg, reg, reg) /* R[a] := R[b] == R[c]                                */ \
    _(   TNE,   "tne", reg, reg, reg) /* R[a] := !(R[b] == R[c])                             */ \
    _(   BLT,   "blt", ___, reg, reg) /* if R[b] < R[c] { pc := pc + 1 }                     */ \
    _(  BNLT,  "bnlt", ___, reg, reg) /* if !(R[b] < R[c]) { pc := pc + 1 }                  */ \
    _(   TLT,   "tlt", reg, reg, reg) /* R[a] := R[b] < R[c]                                 */ \
    _(  TNLT,  "tnlt", reg, reg, reg) /* R[a] := !(R[b] < R[c])                              */ \
    _(   BLE,   "ble", ___, reg, reg) /* if R[b] <= R[c] { pc := pc + 1 }                    */ \
    _(  BNLE,  "bnle", ___, reg, reg) /* if !(R[b] <= R[c]) { pc := pc + 1 }                 */ \
    _(   TLE,   "tle", reg, reg, reg) /* R[a] := R[b] <= R[c]                                */ \
    _(  TNLE,  "tnle", reg, reg, reg) /* R[a] := !(R[b] <= R[c])                             */ \
    _(  BEQI,  "beqi", ___, reg, val) /* if R[b] == int(sc) { pc := pc + 1 }                 */ \
    _(  BNEI,  "bnei", ___, reg, val) /* if !(R[b] == int(sc)) { pc := pc + 1 }              */ \
    _(  TEQI,  "teqi", reg, reg, val) /* R[a] := R[b] == int(sc)                             */ \
    _(  TNEI,  "tnei", reg, reg, val) /* R[a] := !(R[b] == int(sc))                          */ \
    _(  BLTI,  "blti", ___, reg, val) /* if R[b] < int(sc) { pc := pc + 1 }                  */ \
    _( BNLTI, "bnlti", ___, reg, val) /* if !(R[b] < int(sc)) { pc := pc + 1 }               */ \
    _(  TLTI,  "tlti", reg, reg, val) /* R[a] := R[b] <= int(sc)                             */ \
    _( TNLTI, "tnlti", reg, reg, val) /* R[a] := !(R[b] <= int(sc))                          */ \
    _(  BLEI,  "blei", ___, reg, val) /* if R[b] <= int(sc) { pc := pc + 1 }                 */ \
    _( BNLEI, "bnlei", ___, reg, val) /* if !(R[b] <= int(sc)) { pc := pc + 1 }              */ \
    _(  TLEI,  "tlei", reg, reg, val) /* R[a] := R[b] <= int(sc)                             */ \
    _( TNLEI, "tnlei", reg, reg, val) /* R[a] := !(R[b] <= int(sc))                          */ \
    _(  BGTI,  "bgti", ___, reg, val) /* if R[b] > int(sc) { pc := pc + 1 }                  */ \
    _( BNGTI, "bngti", ___, reg, val) /* if !(R[b] > int(sc)) { pc := pc + 1 }               */ \
    _(  TGTI,  "tgti", reg, reg, val) /* R[a] := R[b] <= int(sc)                             */ \
    _( TNGTI, "tngti", reg, reg, val) /* R[a] := !(R[b] <= int(sc))                          */ \
    _(  BGEI,  "bgei", ___, reg, val) /* if R[b] >= int(sc) { pc := pc + 1 }                 */ \
    _( BNGEI, "bngei", ___, reg, val) /* if !(R[b] >= int(sc)) { pc := pc + 1 }              */ \
    _(  TGEI,  "tgei", reg, reg, val) /* R[a] := R[b] <= int(sc)                             */ \
    _( TNGEI, "tngei", reg, reg, val) /* R[a] := !(R[b] <= int(sc))                          */ \
/*===========================================================================================*/ \
    _(    KN,    "kn", reg, ___, off) /* R[a:a+c] := nil                                     */ \
    _(    KI,    "ki", reg, val, val) /* R[a] := int(sbx)                                    */ \
    _(     K,     "k", reg, kst, kst) /* R[a] := K[bx]                                       */ \
    _(  CMOV,  "cmov", reg, cap, ___) /* R[a] := C[b]                                        */ \
    _(   GET,   "get", reg, reg, reg) /* R[a] := R[b][R[c]]                                  */ \
    _(  GETI,  "geti", reg, reg, val) /* R[a] := R[b][int(c)]                                */ \
    _(  GETK,  "getk", reg, reg, kst) /* R[a] := R[b][K[c]]                                  */ \
    _( GETKX, "getkx", reg, reg, ___) /* R[a] := R[b][K[ex]]                                 */ \
    _( CGETK, "cgetk", reg, cap, kst) /* R[a] := C[b][K[c]]                                  */ \
    _(CGETKX,"cgetkx", reg, cap, ___) /* R[a] := C[b][K[ex]]                                 */ \
    _(   SET,   "set", reg, reg, reg) /* R[b][R[c]] := R[a]                                  */ \
    _(  SETI,  "seti", reg, reg, val) /* R[b][int(sc)] := R[a]                               */ \
    _(  SETK,  "setk", reg, reg, val) /* R[b][K[c]] := R[a]                                  */ \
    _( SETKX, "setkx", reg, reg, ___) /* R[b][K[ex]] := R[a]                                 */ \
    _(   NEG,   "neg", reg, reg, ___) /* R[a] := -R[b]                                       */ \
    _(   LEN,   "len", reg, reg, ___) /* R[a] := #R[b]                                       */ \
    _( UNBOX, "unbox", reg, reg, num) /* R[a:a+c-1] := *R[b]                                 */ \
    _(  TNEW,  "tnew", reg, reg, num) /* R[a] := (R[b:b+c-1])                                */ \
    _(   ADD,   "add", reg, reg, reg) /* R[a] := R[b] + R[c]                                 */ \
    _(   SUB,   "sub", reg, reg, reg) /* R[a] := R[b] - R[c]                                 */ \
    _(   MUL,   "mul", reg, reg, reg) /* R[a] := R[b] * R[c]                                 */ \
    _(   DIV,   "div", reg, reg, reg) /* R[a] := R[b] / R[c]                                 */ \
    _(   MOD,   "mod", reg, reg, reg) /* R[a] := R[b] % R[c]                                 */ \
    _(   SHL,   "shl", reg, reg, reg) /* R[a] := R[b] << R[c]                                */ \
    _(   SHR,   "shr", reg, reg, reg) /* R[a] := R[b] >> R[c]                                */ \
    _(  BAND,  "band", reg, reg, reg) /* R[a] := R[b] & R[c]                                 */ \
    _(   BOR,   "bor", reg, reg, reg) /* R[a] := R[b] | R[c]                                 */ \
    _(  BXOR,  "bxor", reg, reg, reg) /* R[a] := R[b] ~ R[c]                                 */ \
    _(  ADDI,  "addi", reg, reg, val) /* R[a] := R[b] + int(sc)                              */ \
    _(  SUBI,  "subi", reg, reg, val) /* R[a] := R[b] - int(sc)                              */ \
    _(  MULI,  "muli", reg, reg, val) /* R[a] := R[b] * int(sc)                              */ \
    _(  DIVI,  "divi", reg, reg, val) /* R[a] := R[b] / int(sc)                              */ \
    _(  MODI,  "modi", reg, reg, val) /* R[a] := R[b] % int(sc)                              */ \
    _(  SHLI,  "shli", reg, reg, val) /* R[a] := R[b] << int(sc)                             */ \
    _(  SHRI,  "shri", reg, reg, val) /* R[a] := R[b] >> int(sc)                             */ \
    _( BANDI, "bandi", reg, reg, val) /* R[a] := R[b] & int(sc)                              */ \
    _(  BORI,  "bori", reg, reg, val) /* R[a] := R[b] | int(sc)                              */ \
    _( BXORI, "bxori", reg, reg, val) /* R[a] := R[b] ~ int(sc)                              */ \
    _(  CALL,  "call", reg, num, num) /* R[a:a+c-1] := R[a](R[a+1:a+b])                      */ \
    _(   CAT,   "cat", reg, reg, num) /* R[a] := concat(R[b:b+c-1])                          */ \
    _(     J,     "j", off, off, off) /* pc := pc + sax                                      */ \
    _(   RET,   "ret", ___, reg, num) /* return R[b:b+c+1]                                   */ \
    _(    FC,    "fc", ___, cap, ___) /* call C function at C[b]                             */ \
    _(    EX,    "ex", val, val, val) /*                                                     */

enum {
#define BCNAME(id,name,a,b,c) BC_##id,
    ALO_BC_LIST(BCNAME)
#undef BCNAME
    BC__MAX
};

inline a_bool bc_has_dual_op(a_u32 op) {
	return op >= BC_KF && op <= BC_TNGEI;
}

inline a_bool bc_is_branch_op(a_u32 op) {
	return op >= BC_BNZ && op <= BC_TNGEI && ((op - BC_BNZ) & 2) == 0;
}


#endif /* abc_h_ */
