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

#define bc_load_op(ip) cast(a_u8, cast(a_u32, *(ip)) >> BC_OFF_OP)
#define bc_load_a(ip) cast(a_u8, cast(a_u32, *(ip)) >> BC_OFF_A)
#define bc_load_ax(ip) (cast(a_u32, *(ip)) >> BC_OFF_A)
#define bc_load_sax(ip) (cast(a_i32, *(ip)) >> BC_OFF_A)
#define bc_load_b(ip) cast(a_u8, cast(a_u32, *(ip)) >> BC_OFF_B)
#define bc_load_bx(ip) cast(a_u16, cast(a_u32, *(ip)) >> BC_OFF_B)
#define bc_load_sbx(ip) cast(a_i16, cast(a_i32, *(ip)) >> BC_OFF_B)
#define bc_load_c(ip) cast(a_u8, cast(a_u32, *(ip)) >> BC_OFF_C)
#define bc_load_sc(ip) cast(a_i8, cast(a_i32, *(ip)) >> BC_OFF_C)

always_inline a_insn bc_load(a_insn const* ip) { return *ip; }

always_inline void bc_store_op(a_insn* ip, a_u32 op) { *ip = (*ip & ~BC_MASK_OP) | bc_wrap_op(op); }
always_inline void bc_store_a(a_insn* ip, a_u32 a) { *ip = (*ip & ~BC_MASK_A) | bc_wrap_a(a); }
always_inline void bc_store_ax(a_insn* ip, a_u32 a) { *ip = (*ip & ~BC_MASK_AX) | bc_wrap_ax(a); }
always_inline void bc_store_sax(a_insn* ip, a_i32 a) { *ip = (*ip & ~BC_MASK_AX) | bc_wrap_sax(a); }
always_inline void bc_store_b(a_insn* ip, a_u32 b) { *ip = (*ip & ~BC_MASK_B) | bc_wrap_b(b); }
always_inline void bc_store_bx(a_insn* ip, a_u32 b) { *ip = (*ip & ~BC_MASK_BX) | bc_wrap_bx(b); }
always_inline void bc_store_sbx(a_insn* ip, a_u32 b) { *ip = (*ip & ~BC_MASK_BX) | bc_wrap_sbx(b); }
always_inline void bc_store_c(a_insn* ip, a_u32 c) { *ip = (*ip & ~BC_MASK_C) | bc_wrap_c(c); }
always_inline void bc_store_sc(a_insn* ip, a_i32 c) { *ip = (*ip & ~BC_MASK_C) | bc_wrap_sc(c); }

always_inline void bc_store(a_insn* ip, a_insn v) { *ip = v; }

#define ALO_BC_LIST(_) \
/*        id,    name,  A,  B,  C, Ex,    description                             */ \
    _(   MOV,   "mov",reg,reg,___,___) /* R[a] := R[b]                            */ \
/*================================================================================*/ \
    _(   LDC,   "ldc",reg,cap,___,___) /* R[a] := C[b]                            */ \
    _(   STC,   "stc",cap,reg,___,___) /* C[a] := R[b]                            */ \
/*================================================================================*/ \
    _(    KN,    "kn",reg,___,len,___) /* R[a:a+c] := nil                         */ \
    _(    KF,    "kf",reg,___,___,___) /* R[a] := false                           */ \
    _(    KT,    "kt",reg,___,___,___) /* R[a] := true                            */ \
    _(    KI,    "ki",reg,int,___,___) /* R[a] := int(b)                          */ \
    _(     K,     "k",reg,kst,___,___) /* R[a] := K[b]                            */ \
    _(    KX,    "kx",reg,___,___,kst) /* R[a] := K[ex]                           */ \
/*================================================================================*/ \
    _(     J,     "j",jmp,___,___,___) /* pc := pc + a                            */ \
/*================================================================================*/ \
    _(   BKF,   "bkf",reg,___,___,___) /* R[a] := false; pc += 1                  */ \
    _(   BKT,   "bkt",reg,___,___,___) /* R[a] := true; pc += 1                   */ \
    _(    BZ,    "bz",reg,___,___,___) /* if R[a] { pc := pc + 1 }                */ \
    _(   BNZ,   "bnz",reg,___,___,___) /* if !R[a] { pc := pc + 1 }               */ \
    _(    BN,    "bn",reg,___,___,___) /* if R[a] == nil { pc := pc + 1 }         */ \
    _(   BNN,   "bnn",reg,___,___,___) /* if R[a] != nil { pc := pc + 1 }         */ \
    _(   BEQ,   "beq",reg,___,___,___) /* if R[a] == R[b] { pc := pc + 1 }        */ \
    _(   BNE,   "bne",reg,___,___,___) /* if !(R[a] == R[b]) { pc := pc + 1 }     */ \
    _(   BLT,   "blt",reg,___,___,___) /* if R[a] < R[b] { pc := pc + 1 }         */ \
    _(  BNLT,  "bnlt",reg,___,___,___) /* if !(R[a] < R[b]) { pc := pc + 1 }      */ \
    _(   BLE,   "ble",reg,___,___,___) /* if R[a] <= R[b] { pc := pc + 1 }        */ \
    _(  BNLE,  "bnle",reg,___,___,___) /* if !(R[a] <= R[b]) { pc := pc + 1 }     */ \
    _(  BEQI,  "beqi",reg,int,___,___) /* if R[a] == int(b) { pc := pc + 1 }      */ \
    _(  BNEI,  "bnei",reg,int,___,___) /* if !(R[a] == int(b)) { pc := pc + 1 }   */ \
    _(  BLTI,  "blti",reg,int,___,___) /* if R[a] < int(b) { pc := pc + 1 }       */ \
    _( BNLTI, "bnlti",reg,int,___,___) /* if !(R[a] < int(b)) { pc := pc + 1 }    */ \
    _(  BLEI,  "blei",reg,int,___,___) /* if R[a] <= int(b) { pc := pc + 1 }      */ \
    _( BNLEI, "bnlei",reg,int,___,___) /* if !(R[a] <= int(b)) { pc := pc + 1 }   */ \
    _(  BGTI,  "bgti",reg,int,___,___) /* if R[a] > int(b) { pc := pc + 1 }       */ \
    _( BNGTI, "bngti",reg,int,___,___) /* if !(R[a] > int(b)) { pc := pc + 1 }    */ \
    _(  BGEI,  "bgei",reg,int,___,___) /* if R[a] >= int(b) { pc := pc + 1 }      */ \
    _( BNGEI, "bngei",reg,int,___,___) /* if !(R[a] >= int(b)) { pc := pc + 1 }   */ \
/*================================================================================*/ \
    _(  TRIM,  "trim",reg,___,len,___) /* R[a:a+c] := R[a:]                       */ \
    _(   LDF,   "ldf",reg,fun,___,___) /* R[a] := func(F[b])                      */ \
    _(  LOOK,  "look",reg,reg,kst,___) /* R[a:a+2] := look(R[b], K[c]), R[b]      */ \
    _( LOOKX, "lookx",reg,reg,___,kst) /* R[a:a+2] := look(R[b], K[ex]), R[b]     */ \
    _(   GET,   "get",reg,reg,reg,___) /* R[a] := R[b][R[c]]                      */ \
    _(  GETI,  "geti",reg,reg,int,___) /* R[a] := R[b][int(c)]                    */ \
    _(  GETS,  "gets",reg,reg,kst,___) /* R[a] := R[b][K[c]: str]                 */ \
    _( GETSX, "getsx",reg,reg,___,kst) /* R[a] := R[b][K[ex]: str]                */ \
    _( CGETS, "cgets",reg,cap,kst,___) /* R[a] := C[b][K[c]: str]                 */ \
    _(CGETSX,"cgetsx",reg,cap,___,kst) /* R[a] := C[b][K[ex]: str]                */ \
    _(   SET,   "set",reg,reg,reg,___) /* R[b][R[c]] := R[a]                      */ \
    _(  SETI,  "seti",reg,reg,int,___) /* R[b][int(sc)] := R[a]                   */ \
    _(  SETS,  "sets",reg,reg,kst,___) /* R[b][K[c]] := R[a]                      */ \
    _( SETSX, "setsx",reg,reg,___,kst) /* R[b][K[ex]] := R[a]                     */ \
    _( CSETS, "csets",reg,cap,kst,___) /* C[b][K[ex]] := R[a]                     */ \
    _(CSETSX,"csetsx",reg,cap,___,kst) /* C[b][K[ex]] := R[a]                     */ \
    _(   NEG,   "neg",reg,reg,___,___) /* R[a] := -R[b]                           */ \
    _(  BNOT,  "bnot",reg,reg,___,___) /* R[a] := ~R[b]                           */ \
    _(   LEN,   "len",reg,reg,___,___) /* R[a] := #R[b]                           */ \
    _( UNBOX, "unbox",reg,___,len,___) /* R[a:a+c] := *R[b]                       */ \
    _(UNBOXV,"unboxv",reg,___,___,___) /* R[a:] := *R[b]                          */ \
    _(  TNEW,  "tnew",reg,reg,len,___) /* R[a] := (R[b:b+c])                      */ \
    _( TNEWM, "tnewm",reg,reg,___,___) /* R[a] := (R[b:])                         */ \
    _(  LNEW,  "lnew",reg,ini,___,___) /* R[a] := [] (with size hint b)           */ \
    _(  LBOX,  "lbox",reg,reg,len,___) /* R[a] := [R[b:b+c]]                      */ \
    _( LBOXM, "lboxm",reg,reg,___,___) /* R[a] := [R[b:]]                         */ \
    _( LPUSH, "lpush",reg,reg,len,___) /* (R[a]: list) ++= R[b:b+c])              */ \
    _(LPUSHM,"lpushm",reg,reg,___,___) /* (R[a]: list) ++= R[b:])                 */ \
    _(  HNEW,  "hnew",reg,ini,___,___) /* R[a] := {} (with size hint b)           */ \
    _(   ADD,   "add",reg,reg,reg,___) /* R[a] := R[b] + R[c]                     */ \
    _(   SUB,   "sub",reg,reg,reg,___) /* R[a] := R[b] - R[c]                     */ \
    _(   MUL,   "mul",reg,reg,reg,___) /* R[a] := R[b] * R[c]                     */ \
    _(   DIV,   "div",reg,reg,reg,___) /* R[a] := R[b] / R[c]                     */ \
    _(   MOD,   "mod",reg,reg,reg,___) /* R[a] := R[b] % R[c]                     */ \
    _(   SHL,   "shl",reg,reg,reg,___) /* R[a] := R[b] << R[c]                    */ \
    _(   SHR,   "shr",reg,reg,reg,___) /* R[a] := R[b] >> R[c]                    */ \
    _(  BAND,  "band",reg,reg,reg,___) /* R[a] := R[b] & R[c]                     */ \
    _(   BOR,   "bor",reg,reg,reg,___) /* R[a] := R[b] | R[c]                     */ \
    _(  BXOR,  "bxor",reg,reg,reg,___) /* R[a] := R[b] ~ R[c]                     */ \
    _(  ADDI,  "addi",reg,reg,int,___) /* R[a] := R[b] + int(c)                   */ \
    _(  SUBI,  "subi",reg,reg,int,___) /* R[a] := R[b] - int(c)                   */ \
    _(  MULI,  "muli",reg,reg,int,___) /* R[a] := R[b] * int(c)                   */ \
    _(  DIVI,  "divi",reg,reg,int,___) /* R[a] := R[b] / int(c)                   */ \
    _(  MODI,  "modi",reg,reg,int,___) /* R[a] := R[b] % int(c)                   */ \
    _(  SHLI,  "shli",reg,reg,int,___) /* R[a] := R[b] << int(c)                  */ \
    _(  SHRI,  "shri",reg,reg,int,___) /* R[a] := R[b] >> int(c)                  */ \
    _( BANDI, "bandi",reg,reg,int,___) /* R[a] := R[b] & int(c)                   */ \
    _(  BORI,  "bori",reg,reg,int,___) /* R[a] := R[b] | int(c)                   */ \
    _( BXORI, "bxori",reg,reg,int,___) /* R[a] := R[b] ~ int(c)                   */ \
    _(  CALL,  "call",reg,len,len,___) /* R[a:a+c] := R[a](R[a+1:a+b])            */ \
    _( CALLV, "callv",reg,len,___,___) /* R[a:] := R[a](R[a+1:a+b])               */ \
    _( TCALL, "tcall",reg,len,len,len) /* return R[a:a+c], R[a+c](R[a+c+1:a+b])   */ \
    _( CALLM, "callm",reg,___,len,___) /* R[a:a+c] := R[a](R[a+1:])               */ \
    _(CALLMV,"callmv",reg,___,___,___) /* R[a:] := R[a](R[a+1:])                  */ \
    _(TCALLM,"tcallm",reg,len,___,len) /* return R[a:a+c], R[a+c](R[a+c+1:])      */ \
    _(   CAT,   "cat",reg,reg,len,___) /* R[a] := concat(R[b:b+c])                */ \
    _(  CATM,  "catm",reg,reg,___,___) /* R[a] := concat(R[b:])                   */ \
    _(   TBC,   "tbc",reg,___,___,___) /* mark R[A] to be closed                  */ \
    _( CLOSE, "close",reg,___,___,___) /* close(C[A:])                            */ \
    _(  RET0,  "ret0",___,___,___,___) /* return                                  */ \
    _(   RET,   "ret",reg,len,___,___) /* return R[a:a+b+1]                       */ \
    _(  RETM,  "retm",reg,___,___,___) /* return R[a:]                            */ \
    _(    EX,    "ex",ext,___,___,___) /*                                         */

enum OpCode {
#define BCNAME(id,...) BC_##id,
    ALO_BC_LIST(BCNAME)
#undef BCNAME
    BC__MAX
};

/* Constraints of opcodes value. */

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
static_assert(BC_CALL + 2 == BC_TCALL);
static_assert(BC_CALLM + 1 == BC_CALLMV);
static_assert(BC_CALLM + 2 == BC_TCALLM);

intern char const ai_bc_names[][8];
intern a_u8 const ai_bc_reloc[];

always_inline a_bool insn_is_branch(a_insn const* ip) {
    a_enum op = bc_load_op(ip);
	return op >= BC_BZ && op <= BC_BNGEI;
}

always_inline a_bool insn_is_leave(a_insn const* ip) {
    a_enum op = bc_load_op(ip);
	return op >= BC_RET0 && op <= BC_RETM;
}

#endif /* abc_h_ */
