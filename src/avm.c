/**
 *@file avm.c
 */

#define avm_c_

#include "abc.h"
#include "afun.h"
#include "aenv.h"
#include "amem.h"
#include "agc.h"

#include "avm.h"

static void l_call_hook(a_henv env, a_msg msg, a_kfun fun, a_kctx ctx) {
	/* Call hook function. */
	(*fun)(env, msg, ctx);
}

void ai_vm_hook(a_henv env, a_msg msg) {
	Global* g = env->_g;
	a_u32 mask = atomic_exchange(&g->_hookm, ALO_HMSWAP);
	if (!(mask & ALO_HMSWAP)) {
		/* Load hook closure. */
		a_kfun fun = g->_hookf;
		a_kctx ctx = g->_hookc;
		/* Reset mask. */
		g->_hookm = mask;

		if (fun != null) {
			l_call_hook(env, msg, fun, ctx);
		}
	}
}

inline void l_move_ret(Global* g, Value* dst_ptr, a_usize dst_len, Value* src_ptr, a_usize src_len) {
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

void ai_vm_call(a_henv env, Value* base, a_u32 rflags) {
	Frame frame;
	frame._prev = env->_frame;
	frame._rflags = rflags;
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
		switch (insn) {
			case BC_MOV: {
				b = bc_load_b(insn);
				v_cpy(G(env), &R[a], &R[b]);
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
			case BC_J: {
				sa = bc_load_sax(insn);
				pc += sa;
				break;
			}
			case BC_RET: {
				b = bc_load_b(insn);
				c = bc_load_c(insn);
				a_usize m = c != 0 ? c - 1 : cast(a_usize, env->_stack._top - &R[b]);
				n = frame._nret != 255 ? cast(a_usize, frame._nret) : m;
				l_move_ret(G(env), R - 1, n, &R[b], m);
				goto vm_return;
			}
			default: panic("bad opcode");
		}
	}

vm_meta_call:
	panic("not implemented.");

vm_return:
	env->_frame = frame._prev;
}
