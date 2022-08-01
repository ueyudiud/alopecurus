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
			case BC_RET0: {
				if (frame._nret != 255) {
					base = R - 1;
					for (a_usize i = 0; i < frame._nret; ++i) {
						v_setx(&base[i], v_of_nil());
					}
				}
				goto vm_return;
			}
			case BC_RETV: {
				b = bc_load_b(insn);
				a = env->_stack._top - &R[b];
				goto vm_ret;
			}
			case BC_RETN: {
				b = bc_load_b(insn);
			vm_ret:
				n = frame._nret;
				base = R - 1;
				if (n == 255 || a >= n) {
					if (n == 255)
						n = a;
					for (a_usize i = n; i > 0; --i) {
						v_cpy(G(env), &base[i - 1], &R[b + i - 1]);
					}
				}
				else {
					for (a_usize i = a; i > 0; --i) {
						v_cpy(G(env), &base[i - 1], &R[b + i - 1]);
					}
					for (a_usize i = a; i < n; ++i) {
						v_setx(&base[i], v_of_nil());
					}
				}
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
