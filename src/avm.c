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

a_hash ai_vm_hash(a_henv env, Value v) {
	switch (v_raw_tag(&v)) {
		case T_NIL:
			return 0;
		case T_FALSE:
			return v_bool_hash(false);
		case T_TRUE:
			return v_bool_hash(true);
		case T_INT:
			return v_int_hash(v_as_int(&v));
		case T_PTR:
		case T_LIST:
		case T_TABLE:
		case T_FUNC:
		case T_MOD:
			return v_hnd_hash(v_as_ptr(&v));
		case T_ISTR:
		case T_HSTR:
			return v_as_str(G(env), &v)->_hash;
		default:
			return v_float_hash(v_as_float(&v));
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
			case T_MOD:
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

static Value l_vm_get(a_henv env, Value v1, Value v2) {
	if (unlikely(!v_is_obj(&v1))) {
		goto bad_index;
	}
	a_hobj obj = v_as_obj(G(env), &v1);
	a_fp_get get = obj->_meta->_vtable._get;
	if (unlikely(get == null)) {
		goto bad_index;
	}
	return (*get)(env, obj, v2);

bad_index:
	ai_err_raisef(env, ALO_EINVAL, "bad index.");
}

static void l_move_ret(a_henv env, Value* dst, a_usize dst_len, Value* src, a_usize src_len) {
	assume(dst <= src);
	a_usize i = 0;
	a_usize mov_len = min(dst_len, src_len);

	while (i < mov_len) {
		v_cpy(G(env), dst, src);
		i += 1;
		dst += 1;
		src += 1;
	}

	while (i < dst_len) {
		v_setx(dst, v_of_nil());
		i += 1;
		dst += 1;
	}

	env->_stack._top = dst;
}

void ai_vm_call(a_henv env, Value* base, RFlags rflags) {
	Frame frame;

	if (!v_is_func(base))
		goto vm_meta_call;

	GFun* fun = v_as_func(G(env), base);
	a_insn insn;
	Value* R;
	Value const* K;

#define pc (frame._pc)
#define load_stack() quiet(R = env->_stack._bot)
#define check_gc() ai_gc_trigger_ext(env, (void) 0, load_stack())

	base += 1;

	run {
		GFunMeta* fmeta = g_cast(GFunMeta, fun->_meta);
		a_isize diff = ai_env_check_stack(env, base + fmeta->_nstack);
		env->_stack._bot = ptr_disp(Value, base, diff);
		if (!(fmeta->_flags & GFUNMETA_FLAG_VARARG)) {
			env->_stack._top = env->_stack._bot + fmeta->_nstack;
		}
		frame = new(Frame) {
			._prev = env->_frame,
			._stack_bot_diff = ptr_diff(env->_stack._bot, env->_stack._base),
			._pc = fmeta->_code,
			._rflags = rflags
		};
		env->_frame = &frame;

		K = fmeta->_consts;
		load_stack();
	}

	loop {
		insn = *(pc++);
		a_usize n, a, b, c;
		a_isize sa, sb, sc;
		Value vt;
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
			case BC_TNEW: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				n = c != 0 ? c - 1 : cast(a_usize, env->_stack._top - &R[b]);
				GTuple* v = ai_tuple_new(env, &R[b], n);
				v_set(G(env), &R[a], v_of_ref(v));
				check_gc();
				break;
			}
			case BC_GET: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				vt = l_vm_get(env, R[b], R[c]);
				v_set(G(env), &R[a], vt);
				break;
			}
			case BC_GETK: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				vt = l_vm_get(env, R[b], K[c]);
				v_set(G(env), &R[a], vt);
				break;
			}
			case BC_CGETK: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				vt = l_vm_get(env, fun->_capval[b], K[c]);
				v_set(G(env), &R[a], vt);
				break;
			}
			case BC_CGETKX: {
				b = bc_load_b(insn);
				c = bc_load_ax(*pc++);
				vt = l_vm_get(env, fun->_capval[b], K[c]);
				v_set(G(env), &R[a], vt);
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
			case BC_CALL: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				if (b != 0) {
					env->_stack._top = &R[a + b];
				}
				ai_vm_call(env, &R[a], new(RFlags) {
					._count = c != 0 ? c - 1 : RFLAG_COUNT_VARARG
				});
				load_stack();
				if (c != 0) {
					env->_stack._top = &R[g_cast(GFunMeta, fun->_meta)->_nstack];
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
				b = bc_load_b(insn);
				a_cfun cf = bcast(a_cfun, fun->_capval[b]);
				a_usize m = (*cf)(env);
				n = frame._rflags._count != RFLAG_COUNT_VARARG ? cast(a_usize, frame._rflags._count) : m;
				api_check_elem(env, m);
				R = env->_stack._bot;
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
	env->_stack._bot = ptr_disp(Value, env->_stack._base, env->_frame->_stack_bot_diff);
#undef pc
}
