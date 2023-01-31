/**
 *@file avm.c
 */

#define avm_c_

#include "abc.h"
#include "aop.h"
#include "afun.h"
#include "amem.h"
#include "agc.h"
#include "aapi.h"

#include "avm.h"

static void l_call_hook(a_henv env, a_msg msg, a_kfun fun, a_kctx ctx) {
	/* Call hook function. */
	(*fun)(env, msg, ctx);
}

a_u32 ai_vm_lock_hook(Global* g) {
	atomic_uint_fast8_t mask = g->_hookm;
	while (((mask & ALO_HMSWAP) != 0) | !atomic_compare_exchange_weak(&g->_hookm, &mask, ALO_HMSWAP));
	return mask;
}

void ai_vm_hook(a_henv env, a_msg msg) {
	Global* g = env->_g;

	a_u32 mask = ai_vm_lock_hook(g);

	/* Load hook closure. */
	a_kfun fun = g->_hookf;
	a_kctx ctx = g->_hookc;

	/* Reset mask. */
	g->_hookm = mask;

	if (fun != null) {
		l_call_hook(env, msg, fun, ctx);
	}
}

static a_bool l_vm_equals(a_henv env, Value const* v1, Value const* v2, a_bool* r) {
	if (v_raw_tag(v1) == v_raw_tag(v2)) {
		switch (v_raw_tag(v1)) {
			case T_NIL:
			case T_FALSE:
			case T_TRUE:
				*r = true;
				return true;
			case T_INT:
				*r = ai_op_eq_int(env, v_as_int(v1), v_as_int(v2));
				return true;
			case T_PTR:
				*r = v_as_ptr(v1) == v_as_ptr(v2);
				return true;
			case T_TUPLE:
			case T_OTHER:
				//TODO
			case T_ISTR:
			case T_LIST:
			case T_TABLE:
			case T_FUNC:
			case T_ROUTE:
				*r = v_as_hnd(v1) == v_as_hnd(v2);
				return true;
			case T_HSTR:
				*r = ai_str_equals(v_as_str(G(env), v1), v_as_str(G(env), v2));
				return true;
			default:
				*r = ai_op_eq_float(env, v_as_float(v1), v_as_float(v2));
				return true;
		}
	}
	else if (v_is_num(v1) && v_is_num(v2)) {
		return ai_op_eq_float(env, v_as_num(v1), v_as_num(v2));
	}
	else {
		return false;
	}
}

static void l_move_ret(Global* g, Value* dst_ptr, a_usize dst_len, Value* src_ptr, a_usize src_len) {
	assume(dst_ptr <= src_ptr);
	a_usize i = 0;
	a_usize mov_len = min(dst_len, src_len);

	while (i < mov_len) {
		v_cpy(g, &dst_ptr[i], &src_ptr[i]);
		i += 1;
	}

	while (i < dst_len) {
		v_setx(&dst_ptr[i], v_of_nil());
		i += 1;
	}
}

void ai_vm_call(a_henv env, Value* base, RFlags rflags) {
	Frame frame = {
		._prev = env->_frame,
		._rflags = rflags
	};
	env->_frame = &frame;

	if (!v_is_func(base))
		goto vm_meta_call;

	GFun* fun = v_as_func(G(env), base);
	GFunMeta* fmeta = downcast(GFunMeta, fun->_meta);
	a_insn* pc = fmeta->_insns;
	a_insn insn;
	Value* R = base + 1;
	Value const* const K = fmeta->_consts;

	loop {
		insn = *(pc++);
		a_usize n, a, b, c;
		a_isize sa, sb, sc;
		a  = bc_load_a(insn);
		switch (bc_load_op(insn)) {
			case BC_NOP: {
				break;
			}
			case BC_MOV: {
				b = bc_load_b(insn);
				v_cpy(G(env), &R[a], &R[b]);
				break;
			}
			case BC_LDC: {
				b = bc_load_b(insn);
				GCap* cap = v_as_cap(G(env), &fun->_capval[b]);
				v_cpy(G(env), &R[a], cap->_ptr);
				break;
			}
			case BC_STC: {
				b = bc_load_b(insn);
				GCap* cap = v_as_cap(G(env), &fun->_capval[a]);
				v_cpy(G(env), cap->_ptr, &R[b]);
				break;
			}
			case BC_KN: {
				c = bc_load_c(insn);
				for (a_u32 i = 0; i < c; ++i) {
					v_setx(&R[a + i], v_of_nil());
				}
				break;
			}
			case BC_KF: {
				v_setx(&R[a], v_of_bool(false));
				break;
			}
			case BC_KT: {
				v_setx(&R[a], v_of_bool(true));
				break;
			}
			case BC_KI: {
				sb = bc_load_sbx(insn);
				v_setx(&R[a], v_of_int(sb));
				break;
			}
			case BC_K: {
				b = bc_load_bx(insn);
				v_cpy(G(env), &R[a], &K[b]);
				break;
			}
			case BC_BEQ: {
				a_bool r;
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				if (l_vm_equals(env, &R[b], &R[c], &r))
					goto vm_meta_call; //TODO
				pc += r;
				break;
			}
			case BC_BNE: {
				a_bool r;
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				if (l_vm_equals(env, &R[b], &R[c], &r))
					goto vm_meta_call; //TODO
				pc += !r;
				break;
			}
			case BC_TEQ: {
				a_bool r;
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				if (l_vm_equals(env, &R[b], &R[c], &r))
					goto vm_meta_call; //TODO
				v_setx(&R[a], v_of_bool(r));
				break;
			}
			case BC_TNE: {
				a_bool r;
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				if (l_vm_equals(env, &R[b], &R[c], &r))
					goto vm_meta_call; //TODO
				v_setx(&R[a], v_of_bool(!r));
				break;
			}
			case BC_J: {
				sa = bc_load_sax(insn);
				pc += sa;
				break;
			}
			case BC_RET: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				a_usize m = c != 0 ? c - 1 : cast(a_usize, env->_stack._top - &R[b]);
				n = frame._rflags._count != RFLAG_COUNT_VARARG ? cast(a_usize, frame._rflags._count) : m;
				l_move_ret(G(env), R - 1, n, &R[b], m);
				goto vm_return;
			}
			case BC_FC: {
				b = bc_load_b(insn);
				a_cfun cf = cast(a_cfun, cast(a_usize, fun->_capval[b]._u));
				a_usize m = (*cf)(env);
				n = frame._rflags._count != RFLAG_COUNT_VARARG ? cast(a_usize, frame._rflags._count) : m;
				api_check_elem(env, m);
				R = env->_stack._bot;
				l_move_ret(G(env), R - 1, n, env->_stack._top - m, m);
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
}
