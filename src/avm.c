/**
 *@file avm.c
 */

#define avm_c_
#define ALO_LIB

#include "abc.h"
#include "aop.h"
#include "atuple.h"
#include "alist.h"
#include "atable.h"
#include "afun.h"
#include "amod.h"
#include "agc.h"
#include "aapi.h"

#include "avm.h"

static void l_call_hook(a_henv env, a_msg msg, a_hfun fun, a_hctx ctx) {
	/* Call hook function. */
	(*fun)(env, msg, ctx);
}

a_u32 ai_vm_lock_hook(Global* g) {
	atomic_uint_fast8_t mask = g->_hookm;
	while (((mask & ALO_HMSWAP) != 0) | !atomic_compare_exchange_weak(&g->_hookm, &mask, ALO_HMSWAP));
	return mask;
}

void ai_vm_hook(a_henv env, a_msg msg, a_u32 test) {
	Global* g = env->_g;

	a_u32 mask = ai_vm_lock_hook(g);

	/* Load hook closure. */
	a_hfun fun = g->_hookf;
	a_hctx ctx = g->_hookc;

	/* Reset mask. */
	g->_hookm = mask;

	if (fun != null && (mask & test)) {
		l_call_hook(env, msg, fun, ctx);
	}
}

static a_none l_bad_op_err(a_henv env, a_u32 op) {
	ai_err_raisef(env, ALO_EINVAL, "bad value for %s operation.", ai_op_names[op]);
}

static a_none l_div_0_err(a_henv env) {
	ai_err_raisef(env, ALO_EINVAL, "attempt to divide by 0.");
}

a_hash ai_vm_hash(a_henv env, Value v) {
	if (likely(v_has_trivial_hash(v))) {
		return v_trivial_hash(v);
	}
	else if (likely(v_is_str(v))) {
		return v_as_str(v)->_hash;
	}
	else if (likely(v_is_tuple(v))) {
		return ai_tuple_hash(env, v_as_tuple(v));
	}
	else {
		GObj* obj = v_as_obj(v);
		a_fp_hash hash_fp = obj->_vtable->_hash;
		return hash_fp != null ? (*hash_fp)(env, obj) : v_trivial_hash(v);
	}
}

a_bool ai_vm_equals(a_henv env, Value v1, Value v2) {
	if (v_get_tag(v1) == v_get_tag(v2)) {
		if (v_is_float(v1)) {
			return ai_op_eq_float(v_as_float(v1), v_as_float(v2));
		}
		else if (v_has_trivial_equals(v1)) {
			return v_trivial_equals(v1, v2);
		}
		else if (likely(v_is_hstr(v1))) {
			return ai_str_equals(v_as_str(v1), v_as_str(v2));
		}
		else if (likely(v_is_tuple(v1))) {
			return ai_tuple_equals(env, v_as_tuple(v1), v_as_tuple(v2));
		}
		else {
			GObj* obj = v_as_obj(v1);
			a_fp_equals equals_fp = obj->_vtable->_equals;
			return equals_fp != null ? (*equals_fp)(env, obj, v2) : v_trivial_equals(v1, v2);
		}
	}
	else if (v_is_num(v1) && v_is_num(v2)) {
		return ai_op_eq_float(v_as_num(v1), v_as_num(v2));
	}
	else {
		return false;
	}
}

static Value l_vm_get(a_henv env, Value v1, Value v2) {
	if (unlikely(!v_is_obj(v1))) {
		goto bad_op;
	}
	a_hobj obj = v_as_obj(v1);
	a_fp_get get_fp = obj->_vtable->_get;
	if (unlikely(get_fp == null)) {
		goto bad_op;
	}
	return (*get_fp)(env, obj, v2);

bad_op:
	l_bad_op_err(env, OP_GET);
}

static void l_vm_set(a_henv env, Value v1, Value v2, Value v3) {
	if (unlikely(!v_is_obj(v1))) {
		goto bad_op;
	}
	a_hobj obj = v_as_obj(v1);
	a_fp_set set_fp = obj->_vtable->_set;
	if (unlikely(set_fp == null)) {
		goto bad_op;
	}
	return (*set_fp)(env, obj, v2, v3);

bad_op:
	l_bad_op_err(env, OP_GET);
}

static Value l_vm_bin(a_henv env, Value v1, Value v2, a_enum op) {
	if (!v_is_obj(v1)) {
		goto bad_op;
	}
	a_hobj obj = v_as_obj(v1);
	a_fp_binary binary_fp = obj->_vtable->_binary;
	if (unlikely(binary_fp == null)) {
		goto bad_op;
	}
	return (*binary_fp)(env, obj, v2, op);

bad_op:
	l_bad_op_err(env, op);
}

static a_bool l_vm_cmp(a_henv env, Value v1, Value v2, a_enum op) {
	if (v_is_int(v1) && v_is_int(v2)) {
		return ai_op_cmp_int(v_as_int(v1), v_as_int(v2), op);
	}
	else if (v_is_num(v1) && v_is_num(v2)) {
		return ai_op_cmp_float(v_as_num(v1), v_as_num(v2), op);
	}

	if (!v_is_obj(v1)) {
		goto bad_op;
	}
	a_hobj obj = v_as_obj(v1);
	a_fp_compare compare_fp = obj->_vtable->_compare;
	if (unlikely(compare_fp == null)) {
		goto bad_op;
	}
	return (*compare_fp)(env, obj, v2, op);

bad_op:
	l_bad_op_err(env, op);
}

static void l_move_ret(a_henv env, Value* dst, a_usize dst_len, Value* src, a_usize src_len) {
	assume(dst <= src);
	a_usize i = 0;
	a_usize mov_len = min(dst_len, src_len);

	while (i < mov_len) {
		v_cpy(env, dst, src);
		i += 1;
		dst += 1;
		src += 1;
	}

	while (i < dst_len) {
		v_set_nil(dst);
		i += 1;
		dst += 1;
	}

	env->_stack._top = dst;
}

void ai_vm_call(a_henv env, Value* base, RFlags rflags) {
	Frame frame;

	if (!v_is_func(*base))
		goto vm_meta_call;

	GFun* fun = v_as_func(*base);
	a_insn insn;
	Value const* K;

#define pc (frame._pc)
#if ALO_STACK_RELOC
 	Value* R;
# define R R
# define load_stack() quiet(R = env->_stack._bot)
# define reload_stack() load_stack()
#else
# define R stk2val(env, frame._stack_bot)
# define load_stack() ((void) 0)
# define reload_stack() ((void) 0)
#endif
#define check_gc() ai_gc_trigger_ext(env, (void) 0, reload_stack())

	base += 1;

	run {
		GProto* proto = fun->_proto;
		a_isize diff = ai_stk_check(env, base + proto->_nstack);
#if ALO_STACK_RELOC
		base = ptr_disp(Value, base, diff);
#else
		assume(diff == 0, "stack moved.");
#endif
		if (!(proto->_flags & FUN_FLAG_VARARG)) {
			env->_stack._top = base + proto->_nstack;
		}
		frame = new(Frame) {
			._prev = env->_frame,
			._stack_bot = val2stk(env, base),
			._caps = env->_frame != null ? env->_frame->_caps : null,
			._pc = proto->_code,
			._rflags = rflags
		};
		env->_frame = &frame;

		K = proto->_consts;
		load_stack();
	}

	loop {
		a_u32 bc;
		a_bool z;
		a_usize n, a, b, c;
		a_isize sa, sb, sc;
		Value vt;

		insn = *(pc++);
		a = bc_load_a(insn);
		bc = bc_load_op(insn);
		switch (bc) {
			case BC_NOP: {
				break;
			}
			case BC_MOV: {
				b = bc_load_b(insn);
				v_cpy(env, &R[a], &R[b]);
				break;
			}
			case BC_LDC: {
				b = bc_load_b(insn);
				RcCap* cap = fun->_caps[b]._rc;
				v_cpy(env, &R[a], cap->_ptr);
				break;
			}
			case BC_STC: {
				b = bc_load_b(insn);
				RcCap* cap = fun->_caps[a]._rc;
				v_cpy(env, cap->_ptr, &R[b]);
				ai_gc_barrier_back_val(env, fun, R[b]);
				break;
			}
			case BC_KN: {
				c = bc_load_c(insn);
				for (a_u32 i = 0; i < c; ++i) {
					v_set_nil(&R[a + i]);
				}
				break;
			}
			case BC_KF: {
				v_set_bool(&R[a], false);
				break;
			}
			case BC_KT: {
				v_set_bool(&R[a], true);
				break;
			}
			case BC_KI: {
				sb = bc_load_sbx(insn);
				v_set_int(&R[a], sb);
				break;
			}
			case BC_K: {
				b = bc_load_bx(insn);
				v_cpy(env, &R[a], &K[b]);
				break;
			}
			case BC_LDF: {
				b = bc_load_bx(insn);
				GFun* v = ai_fun_new(env, fun->_proto->_subs[b], &frame);
				v_set_obj(env, &R[a], v);
				check_gc();
				break;
			}
			case BC_TNEW: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				n = c != 0 ? c - 1 : cast(a_usize, env->_stack._top - &R[b]);
				GTuple* v = ai_tuple_new(env, &R[b], n);
				v_set_obj(env, &R[a], v);
				check_gc();
				break;
			}
			case BC_LNEW: {
				b = bc_load_bx(insn);
				GList* v = ai_list_new(env);
				v_set_obj(env, &R[a], v);
				ai_list_hint(env, v, b);
				check_gc();
				break;
			}
			case BC_GET: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				vt = l_vm_get(env, R[b], R[c]);
				v_set(env, &R[a], vt);
				break;
			}
			case BC_GETI: {
				b = bc_load_b(insn);
				sc = bc_load_sc(insn);
				vt = l_vm_get(env, R[b], v_of_int(sc));
				v_set(env, &R[a], vt);
				break;
			}
			case BC_GETS: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				vt = l_vm_get(env, R[b], K[c]);
				v_set(env, &R[a], vt);
				break;
			}
			case BC_GETSX: {
				b = bc_load_b(insn);
				c = bc_load_ax(*pc++);
				vt = l_vm_get(env, R[b], K[c]);
				v_set(env, &R[a], vt);
				break;
			}
			case BC_CGETS: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				vt = l_vm_get(env, fun->_caps[b]._imm, K[c]);
				v_set(env, &R[a], vt);
				break;
			}
			case BC_CGETSX: {
				b = bc_load_b(insn);
				c = bc_load_ax(*pc++);
				vt = l_vm_get(env, fun->_caps[b]._imm, K[c]);
				v_set(env, &R[a], vt);
				break;
			}
			case BC_SET: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				l_vm_set(env, R[b], R[c], R[a]);
				break;
			}
			case BC_SETI: {
				b = bc_load_b(insn);
				sc = bc_load_sc(insn);
				l_vm_set(env, R[b], v_of_int(sc), R[a]);
				break;
			}
			case BC_SETK: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				l_vm_set(env, R[b], K[c], R[a]);
				break;
			}
			case BC_SETKX: {
				b = bc_load_b(insn);
				c = bc_load_ax(*pc++);
				l_vm_set(env, R[b], K[c], R[a]);
				break;
			}
			case BC_ADD:
			case BC_SUB:
			case BC_MUL: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);

				a_u32 op = bc - BC_ADD + OP_ADD;
				Value vb = R[b];
				Value vc = R[c];

				if (v_is_int(vb) && v_is_int(vc)) {
					a_int val = ai_op_bin_int(v_as_int(vb), v_as_int(vc), op);
					v_set_int(&R[a], val);
				}
				else if (v_is_num(vb) && v_is_num(vc)) {
					a_float val = ai_op_bin_float(v_as_num(vb), v_as_num(vc), op);
					v_set_float(&R[a], val);
				}
				else {
					vt = l_vm_bin(env, vb, vc, op);
					reload_stack();
					v_set(env, &R[a], vt);
				}
				break;
			}
			case BC_DIV:
			case BC_MOD: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);

				a_u32 op = bc - BC_ADD + OP_ADD;
				Value vb = R[b];
				Value vc = R[c];

				if (v_is_int(vb) && v_is_int(vc)) {
					a_int ic = v_as_int(vc);
					if (unlikely(ic == 0)) {
						l_div_0_err(env);
					}
					a_int val = ai_op_bin_int(v_as_int(vb), ic, op);
					v_set_int(&R[a], val);
				}
				else if (v_is_num(vb) && v_is_num(vc)) {
					a_float val = ai_op_bin_float(v_as_num(vb), v_as_num(vc), op);
					v_set_float(&R[a], val);
				}
				else {
					vt = l_vm_bin(env, vb, vc, op);
					reload_stack();
					v_set(env, &R[a], vt);
				}
				break;
			}
			case BC_SHL:
			case BC_SHR:
			case BC_BAND:
			case BC_BOR:
			case BC_BXOR: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);

				a_u32 op = bc - BC_ADD + OP_ADD;
				Value vb = R[b];
				Value vc = R[c];

				if (v_is_int(vb) && v_is_int(vc)) {
					a_int val = ai_op_bin_int(v_as_int(vb), v_as_int(vc), op);
					v_set_int(&R[a], val);
				}
				else {
					vt = l_vm_bin(env, vb, vc, op);
					v_set(env, &R[a], vt);
				}
				break;
			}
			case BC_ADDI:
			case BC_SUBI:
			case BC_MULI: {
				b = bc_load_b(insn);
				sc = bc_load_sc(insn);

				a_u32 op = bc - BC_ADDI + OP_ADD;
				Value vb = R[b];
				a_int ic = cast(a_int, sc);

				if (v_is_int(vb)) {
					a_int val = ai_op_bin_int(v_as_int(vb), ic, op);
					v_set_int(&R[a], val);
				}
				else if (v_is_float(vb)) {
					a_float val = ai_op_bin_float(v_as_float(vb), ic, op);
					v_set_float(&R[a], val);
				}
				else {
					vt = l_vm_bin(env, vb, v_of_int(ic), op);
					reload_stack();
					v_set(env, &R[a], vt);
				}
				break;
			}
			case BC_DIVI:
			case BC_MODI: {
				b = bc_load_b(insn);
				sc = bc_load_sc(insn);

				a_u32 op = bc - BC_ADDI + OP_ADD;
				Value vb = R[b];
				a_int ic = cast(a_int, sc);

				if (v_is_int(vb)) {
					if (unlikely(ic == 0)) {
						l_div_0_err(env);
					}
					a_int val = ai_op_bin_int(v_as_int(vb), ic, op);
					v_set_int(&R[a], val);
				}
				else if (v_is_float(vb)) {
					a_float val = ai_op_bin_float(v_as_float(vb), ic, op);
					v_set_float(&R[a], val);
				}
				else {
					vt = l_vm_bin(env, vb, v_of_int(ic), op);
					reload_stack();
					v_set(env, &R[a], vt);
				}
				break;
			}
			case BC_SHLI:
			case BC_SHRI:
			case BC_BANDI:
			case BC_BORI:
			case BC_BXORI: {
				b = bc_load_b(insn);
				sc = bc_load_sc(insn);

				a_u32 op = bc - BC_ADDI + OP_ADD;
				Value vb = R[b];
				a_int ic = cast(a_int, sc);

				if (v_is_int(vb)) {
					a_int val = ai_op_bin_int(v_as_int(vb), ic, op);
					v_set_int(&R[a], val);
				}
				else {
					vt = l_vm_bin(env, vb, v_of_int(ic), op);
					reload_stack();
					v_set(env, &R[a], vt);
				}
				break;
			}
			case BC_BZ:
			case BC_BNZ:
			case BC_TZ:
			case BC_TNZ: {
				b = bc_load_b(insn);
				z = v_to_bool(R[b]);
				goto jump_or_test;
			}
			case BC_BEQ:
			case BC_BNE:
			case BC_TEQ:
			case BC_TNE: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				z = ai_vm_equals(env, R[b], R[c]);
				goto jump_or_test;
			}
			case BC_BLT:
			case BC_BNLT:
			case BC_TLT:
			case BC_TNLT: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				z = l_vm_cmp(env, R[b], R[c], OP_LT);
				goto jump_or_test;
			}
			case BC_BLE:
			case BC_BNLE:
			case BC_TLE:
			case BC_TNLE: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				z = l_vm_cmp(env, R[b], R[c], OP_LE);
				goto jump_or_test;
			}
			case BC_BEQI:
			case BC_BNEI:
			case BC_TEQI:
			case BC_TNEI: {
				b = bc_load_b(insn);
				sc = bc_load_sc(insn);
				z = ai_vm_equals(env, R[b], v_of_int(sc));
				goto jump_or_test;
			}
			case BC_BLTI:
			case BC_BNLTI:
			case BC_TLTI:
			case BC_TNLTI: {
				b = bc_load_b(insn);
				sc = bc_load_sc(insn);
				z = l_vm_cmp(env, R[b], v_of_int(sc), OP_LT);
				goto jump_or_test;
			}
			case BC_BLEI:
			case BC_BNLEI:
			case BC_TLEI:
			case BC_TNLEI: {
				b = bc_load_b(insn);
				sc = bc_load_sc(insn);
				z = l_vm_cmp(env, R[b], v_of_int(sc), OP_LE);
				goto jump_or_test;
			}
			case BC_BGTI:
			case BC_BNGTI:
			case BC_TGTI:
			case BC_TNGTI: {
				b = bc_load_b(insn);
				sc = bc_load_sc(insn);
				z = l_vm_cmp(env, v_of_int(sc), R[b], OP_LT);
				goto jump_or_test;
			}
			case BC_BGEI:
			case BC_BNGEI:
			case BC_TGEI:
			case BC_TNGEI: {
				b = bc_load_b(insn);
				sc = bc_load_sc(insn);
				z = l_vm_cmp(env, v_of_int(sc), R[b], OP_LE);
				goto jump_or_test;
			}
			jump_or_test: {
				z ^= cast(a_bool, bc & 1);
				if (!(bc & 2)) {
					pc += z;
				}
				else {
					v_set_bool(&R[a], z);
				}
				break;
			}
			case BC_J: {
				sa = bc_load_sax(insn);
				pc += sa;
				break;
			}
			case BC_CLOSE: {
				ai_cap_close(env, &frame._caps, &R[a]);
				break;
			}
			case BC_CALL: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				if (b != 0) {
					env->_stack._top = &R[a + b];
				}
				ai_vm_call(env, &R[a], new(RFlags) {
					._count = c != 0 ? c - 1 : RFLAG_COUNT_VARARG
				});
				reload_stack();
				if (c != 0) {
					env->_stack._top = &R[fun->_proto->_nstack];
				}
				break;
			}
			case BC_RET: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				a_usize m = c != 0 ? c - 1 : cast(a_usize, env->_stack._top - &R[b]);
				n = frame._rflags._count != RFLAG_COUNT_VARARG ? cast(a_usize, frame._rflags._count) : m;
				l_move_ret(env, R - 1, n, &R[b], m);
				goto vm_return;
			}
			case BC_FC: {
				a_cfun cf = bcast(a_cfun, fun->_caps[fun->_len - 1]);
				a_usize m = (*cf)(env);
				n = frame._rflags._count != RFLAG_COUNT_VARARG ? cast(a_usize, frame._rflags._count) : m;
				api_check_elem(env, m);
				l_move_ret(env, R - 1, n, env->_stack._top - m, m);
				goto vm_return;
			}
			default: {
				panic("bad opcode");
			}
		}
	}

vm_meta_call:
	panic("not implemented.");

vm_return:
	env->_frame = frame._prev;
	ai_cap_close(env, &frame._caps, R);
#undef pc
#undef R
}
