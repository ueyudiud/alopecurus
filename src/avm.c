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
#include "afmt.h"
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

static a_none l_bad_tm_err(a_henv env, a_u32 tm) {
	GStr* tm_name = ai_env_strx(G(env), STRX_TM__FIRST + tm);
	ai_err_raisef(env, ALO_EINVAL, "'%s' method not found.", str2ntstr(tm_name));
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

static Value vm_meta_get(a_henv env, Value v1, Value v2) {
	if (!v_is_obj(v1)) {
		goto bad_op;
	}

	VTable const* vtable = v_as_obj(v1)->_vtable;
	if (!(vtable->_flags & VTABLE_FLAG_FAST_TM(TM_GET))) {
		goto bad_op;
	}

	Value const* pvf = ai_obj_vlookup_(env, vtable, TM_GET);
	assume(pvf != null);
	Value vf = *pvf;

	if (unlikely(v_is_nil(vf))) {
		goto bad_op;
	}

	Value* base = vm_push_args(env, vf, v1, v2);
	return ai_vm_call(env, base, RFLAGS_META_CALL);

bad_op:
	l_bad_tm_err(env, TM_GET);
}

static void vm_meta_set(a_henv env, Value v1, Value v2, Value v3) {
	if (!v_is_obj(v1)) {
		goto bad_op;
	}

	VTable const* vtable = v_as_obj(v1)->_vtable;
	if (!(vtable->_flags & VTABLE_FLAG_FAST_TM(TM_SET))) {
		goto bad_op;
	}

	Value const* pvf = ai_obj_vlookup_(env, vtable, TM_SET);
	assume(pvf != null);
	Value vf = *pvf;

	if (unlikely(v_is_nil(vf))) {
		goto bad_op;
	}

	Value* base = vm_push_args(env, vf, v1, v2, v3);
	ai_vm_call(env, base, RFLAGS_META_CALL);
	return;

bad_op:
	l_bad_tm_err(env, TM_SET);
}

static Value vm_meta_len(a_henv env, Value v1) {
	if (!v_is_obj(v1)) {
		goto bad_op;
	}

	a_hobj obj = v_as_obj(v1);
	VTable const* vtable = obj->_vtable;
	if (!(vtable->_flags & VTABLE_FLAG_FAST_TM(TM_LEN))) {
		if (vtable->_flags & VTABLE_FLAG_PLAIN_LEN) {
			return v_of_int(cast(a_int, obj->_len));
		}
		goto bad_op;
	}

	Value const* pvf = ai_obj_vlookup_(env, vtable, TM_LEN);
	assume(pvf != null);
	Value vf = *pvf;

	if (unlikely(v_is_nil(vf))) {
		goto bad_op;
	}

	Value* base = vm_push_args(env, vf, v1);
	return ai_vm_call(env, base, RFLAGS_META_CALL);

bad_op:
	l_bad_tm_err(env, TM_LEN);
}

static Value vm_meta_unbox(a_henv env, Value v1, Value* vb, a_usize n) {
	if (!v_is_obj(v1)) {
		goto bad_op;
	}

	Value vf = ai_obj_vlookup(env, v1, TM_UNBOX);

	if (unlikely(v_is_nil(vf))) {
		goto bad_op;
	}

	env->_stack._top = vb;
	Value* base = vm_push_args(env, vf, v1);
	return ai_vm_call(env, base, new(RFlags) { ._count = n });

bad_op:
	l_bad_tm_err(env, TM_UNBOX);
}

static Value vm_meta_unr(a_henv env, Value v1, a_enum tm) {
	Value vf = ai_obj_vlookup(env, v1, tm);

	if (v_is_nil(vf)) {
		l_bad_tm_err(env, tm);
	}

	Value* base = vm_push_args(env, vf, v1);
	return ai_vm_call(env, base, RFLAGS_META_CALL);
}

static Value vm_meta_bin(a_henv env, Value v1, Value v2, a_enum tm) {
	Value vf = ai_obj_vlookup(env, v1, tm);

	if (v_is_nil(vf)) {
		l_bad_tm_err(env, tm);
	}

	Value* base = vm_push_args(env, vf, v1, v2);
	return ai_vm_call(env, base, RFLAGS_META_CALL);
}

static a_bool vm_meta_cmp(a_henv env, Value v1, Value v2, a_enum tm) {
	Value vf = ai_obj_vlookup(env, v1, tm);

	if (v_is_nil(vf)) {
		l_bad_tm_err(env, tm);
	}

	Value* base = vm_push_args(env, vf, v1, v2);
	return v_to_bool(ai_vm_call(env, base, RFLAGS_META_CALL));
}

static GStr* vm_cat(a_henv env, Value* base, a_usize n) {
	a_usize i = 0;
	GStr* cache = null;
	Value v;

	run {
		while (i < n) {
			v = base[i++];
			if (likely(v_is_str(v))) {
				GStr* val = v_as_str(v);
				if (val->_len > 0) {
					cache = val;
					goto cached;
				}
			}
			else {
				switch (v_get_tag(v)) {
					case T_NIL: {
						cache = ai_env_strx(G(env), STRX_KW_NIL);
						i += 1;
						goto cached;
					}
					case T_FALSE: {
						cache = ai_env_strx(G(env), STRX_KW_FALSE);
						i += 1;
						goto cached;
					}
					case T_TRUE: {
						cache = ai_env_strx(G(env), STRX_KW_TRUE);
						i += 1;
						goto cached;
					}
					default: goto buffered;
				}
			}
		}
		return ai_env_strx(G(env), STRX__EMPTY); /* Empty string. */
	}

	run cached: {
		Value const str_empty = v_of_obj(ai_env_strx(G(env), STRX__EMPTY));
		while (i < n) {
			v = base[i++];
			if (!v_trivial_equals(v, str_empty))
				goto buffered;
		}
		return cache;
	}

	run buffered: { /* Cannot aasm string trivially, try to compute string size and create buffer. */
		GBuf* buf = ai_buf_new(env);
		vm_push_args(env, v_of_obj(buf));

		if (cache != null) {
			ai_buf_putls(env, buf, cache->_data, cache->_len);
		}

		loop {
			if (likely(v_is_str(v))) {
				GStr* val = v_as_str(v);
				ai_buf_putls(env, buf, val->_data, val->_len);
			}
			else switch (v_get_tag(v)) {
				case T_NIL: {
					ai_buf_puts(env, buf, "nil");
					break;
				}
				case T_FALSE: {
					ai_buf_puts(env, buf, "false");
					break;
				}
				case T_TRUE: {
					ai_buf_puts(env, buf, "true");
					break;
				}
				case T_INT: {
					if (unlikely(ai_fmt_nputi(env, buf, v_as_int(v))))
						ai_mem_nomem(env);
					break;
				}
				case T_PTR: {
					if (unlikely(ai_fmt_nputp(env, buf, v_as_ptr(v))))
						ai_mem_nomem(env);
					break;
				}
				case T__MIN_OBJ ... T__MAX_OBJ: {
					GObj* obj = v_as_obj(v);
					a_fp_tostr tostr_fp = obj->_vtable->_tostr;
					if (tostr_fp == null) {
						ai_err_raisef(env, ALO_EINVAL, "cannot convert %s to string.", obj->_vtable->_name);
					}
					(*tostr_fp)(env, obj, buf);
					break;
				}
				default: {
					if (unlikely(ai_fmt_nputf(env, buf, v_as_float(v))))
						ai_mem_nomem(env);
					break;
				}
			}
			if (i == n) break;
			v = base[i++];
		}

		GStr* result = ai_buf_tostr(env, buf);
		ai_buf_deinit(G(env), buf);
		env->_stack._top -= 1;

		return result;
	}
}

static void vm_close_above(a_henv env, RcCap** restrict caps, Value* ptr) {
	RcCap* cap;
	while ((cap = *caps) != null && cap->_ptr >= ptr) {
		*caps = cap->_next;
		ai_cap_soft_close(env, cap);
	}
}

static void v_mov_all_with_nil(a_henv env, Value* dst, a_usize dst_len, Value const* src, a_usize src_len) {
	a_usize i = 0;

	a_usize arg_len = min(dst_len, src_len);
	while (i < arg_len) {
		v_cpy(env, &dst[i], &src[i]);
		i += 1;
	}

	while (i < dst_len) {
		v_cpy(env, &dst[i], &src[i]);
		i += 1;
	}
}

static a_u32 vm_fetch_ex(a_insn const** ppc) {
	a_insn const* pc = *ppc;
	a_insn insn = *pc;
	*ppc = pc + 1;
	assume(bc_load_op(insn) == BC_EX, "not extra operand.");
	return bc_load_ax(insn);
}

/**
 ** Do 'call' operation on the stack.
 *@param env the environment.
 *@param base the base pointer of function and arguments.
 *@param rflags the return flags.
 *@return The first result.
 */
Value ai_vm_call(a_henv env, Value* base, RFlags rflags) {
	Frame frame;

#define pc (frame._pc)
#if ALO_STACK_RELOC
	Value* R;
# define R R
# define check_stack(top) ({ a_isize _d = ai_stk_check(env, top); base = ptr_disp(Value, R, _d); reload_stack(); })
# define reload_stack() quiet(R = env->_stack._bot)
#else
# define R frame._stack_bot
# define check_stack(top) ({ a_isize _d = ai_stk_check(env, top); assume(_d == 0, "stack moved."); reload_stack(); })
# define reload_stack() ((void) 0)
#endif
#define check_gc() ai_gc_trigger_ext(env, (void) 0, reload_stack())
#define adjust_top() quiet(env->_stack._top = &R[fun->_proto->_nstack])

	run { /* Check for function. */
		Value vf = *base;
		while (unlikely(!v_is_func(vf))) {
			vf = ai_obj_vlookup(env, vf, TM_CALL);
			if (v_is_nil(vf)) {
				l_bad_tm_err(env, TM_CALL);
			}

			for (Value* p = env->_stack._top; p > base; --p) {
				v_cpy(env, p, p - 1);
			}
			v_set(env, &base[0], vf);
			env->_stack._top += 1;

			check_stack(env->_stack._top);
		}
	}

	GFun* fun = v_as_func(*base);
	a_insn insn;
	Value const* K;

	base += 1;

	run {
		GProto* proto = fun->_proto;
		check_stack(base + proto->_nstack);
		if (!(proto->_flags & FUN_FLAG_VARARG)) {
			adjust_top();
		}
		frame = new(Frame) {
			._prev = env->_frame,
			._stack_bot = base,
			._caps = null,
			._pc = proto->_code,
			._rflags = rflags
		};
		env->_frame = &frame;

		K = proto->_consts;
	}

	loop {
		a_u32 bc;
		a_u32 a;

#define loadB() a_u32 b = bc_load_b(insn)
#define loadBx() a_u32 b = bc_load_bx(insn)
#define loadsBx() a_i16 b = bc_load_sbx(insn)
#define loadC() a_u32 c = bc_load_c(insn)
#define loadsC() a_i8 c = bc_load_sc(insn)
#define loadJ() a_i32 j = bc_load_sax(insn)
#define loadEx() a_u32 ex = vm_fetch_ex(&pc)

		insn = *(pc++);
		insn_check(insn);
		a = bc_load_a(insn);
		bc = bc_load_op(insn);
		switch (bc) {
			case BC_MOV: {
				loadB();

				v_cpy(env, &R[a], &R[b]);
				break;
			}
			case BC_LDC: {
				loadB();

				RcCap* cap = fun->_caps[b];
				v_cpy(env, &R[a], cap->_ptr);
				break;
			}
			case BC_STC: {
				loadB();

				RcCap* cap = fun->_caps[a];
				v_cpy(env, cap->_ptr, &R[b]);

				ai_gc_barrier_backward_val(env, fun, R[b]);
				break;
			}
			case BC_KN: {
				loadC();

				v_set_nil_ranged(&R[a], &R[a + c]);
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
			case BC_BKF: {
				v_set_bool(&R[a], false);
				pc += 1;
				break;
			}
			case BC_BKT: {
				v_set_bool(&R[a], true);
				pc += 1;
				break;
			}
			case BC_KI: {
				loadsBx();

				v_set_int(&R[a], b);
				break;
			}
			case BC_K: {
				loadBx();

				v_cpy(env, &R[a], &K[b]);
				break;
			}
			case BC_LDF: {
				loadB();

				GFun* v = ai_fun_new(env, fun->_proto->_subs[b], &frame);
				v_set_obj(env, &R[a], v);

				check_gc();
				break;
			}
			case BC_TNEW: {
				loadB();
				loadC();

				GTuple* v = ai_tuple_new(env, &R[b], c);
				v_set_obj(env, &R[a], v);

				check_gc();
				break;
			}
			case BC_TNEWM: {
				loadB();

				a_u32 n = env->_stack._top - &R[b];
				GTuple* v = ai_tuple_new(env, &R[b], n);
				v_set_obj(env, &R[a], v);

				check_gc();
				break;
			}
			case BC_LNEW: {
				loadBx();

				GList* val = ai_list_new(env);
				v_set_obj(env, &R[a], val);
				ai_list_hint(env, val, b);

				check_gc();
				break;
			}
			case BC_LBOXM: {
				loadB();

				a_u32 n = env->_stack._top - &R[b];

				GList* val = ai_list_new(env);
				v_set_obj(env, env->_stack._top, val);
				env->_stack._top += 1;

				ai_list_push_all(env, val, &R[b], n);
				v_set_obj(env, &R[a], val);
				check_gc();

				adjust_top();
				break;
			}
			case BC_LPUSH: {
				loadB();
				loadC();

				GList* val = v_as_list(R[a]);
				ai_list_push_all(env, val, &R[b], c);

				check_gc();
				break;
			}
			case BC_LPUSHM: {
				loadB();

				a_u32 n = env->_stack._top - &R[b];

				GList* val = v_as_list(R[a]);
				ai_list_push_all(env, val, &R[b], n);

				check_gc();
				break;
			}
			case BC_GET: {
				loadB();
				loadC();

				Value vb = R[b];
				Value vc = R[c];

				Value vt;
				if (v_is_tuple(vb)) {
					vt = ai_tuple_get(env, v_as_tuple(vb), vc);
				}
				else if (v_is_list(vb)) {
					vt = ai_list_get(env, v_as_list(vb), vc);
				}
				else if (v_is_table(vb)) {
					vt = ai_table_get(env, v_as_table(vb), vc);
				}
				else if (v_is_mod(vb)) {
					vt = ai_table_get(env, &v_as_mod(vb)->_table, vc);
				}
				else {
					vt = vm_meta_get(env, vb, vc);
					reload_stack();
				}
				v_set(env, &R[a], vt);
				break;
			}
			case BC_GETI: {
				loadB();
				loadsC();

				Value vb = R[b];

				Value vt;
				if (v_is_tuple(vb)) {
					vt = ai_tuple_geti(env, v_as_tuple(vb), c);
				}
				else if (v_is_list(vb)) {
					vt = ai_list_geti(env, v_as_list(vb), c);
				}
				else if (v_is_table(vb)) {
					vt = ai_table_get(env, v_as_table(vb), v_of_int(c));
				}
				else if (v_is_mod(vb)) {
					vt = ai_table_get(env, &v_as_mod(vb)->_table, v_of_int(c));
				}
				else {
					vt = vm_meta_get(env, vb, v_of_int(c));
					reload_stack();
				}
				v_set(env, &R[a], vt);
				break;
			}
			case BC_GETS: {
				loadB();
				loadC();

				Value vb = R[b];
				Value vc = K[c];

				Value vt;
				if (v_is_table(vb)) {
					vt = ai_table_getis(env, v_as_table(vb), v_as_str(vc));
				}
				else if (v_is_mod(vb)) {
					vt = ai_table_getis(env, &v_as_mod(vb)->_table, v_as_str(vc));
				}
				else if (unlikely(v_is_tuple(vb) || v_is_list(vb))) {
					ai_err_bad_index(env, v_as_obj(vb)->_vtable->_name, v_typename(vc));
				}
				else {
					vt = vm_meta_get(env, vb, vc);
					reload_stack();
				}
				v_set(env, &R[a], vt);
				break;
			}
			case BC_GETSX: {
				loadB();
				loadEx();

				Value vb = R[b];
				Value vc = K[ex];

				Value vt;
				if (v_is_table(vb)) {
					vt = ai_table_getis(env, v_as_table(vb), v_as_str(vc));
				}
				else if (v_is_mod(vb)) {
					vt = ai_table_getis(env, &v_as_mod(vb)->_table, v_as_str(vc));
				}
				else if (unlikely(v_is_tuple(vb) || v_is_list(vb))) {
					ai_err_bad_index(env, v_as_obj(vb)->_vtable->_name, v_typename(vc));
				}
				else {
					vt = vm_meta_get(env, vb, vc);
					reload_stack();
				}
				v_set(env, &R[a], vt);
				break;
			}
			case BC_CGETS: {
				loadB();
				loadC();

				Value vb = *fun->_caps[b]->_ptr;
				Value vc = K[c];

				Value vt;
				if (v_is_table(vb)) {
					vt = ai_table_getis(env, v_as_table(vb), v_as_str(vc));
				}
				else if (v_is_mod(vb)) {
					vt = ai_table_getis(env, &v_as_mod(vb)->_table, v_as_str(vc));
				}
				else if (unlikely(v_is_tuple(vb) || v_is_list(vb))) {
					ai_err_bad_index(env, v_as_obj(vb)->_vtable->_name, v_typename(vc));
				}
				else {
					vt = vm_meta_get(env, vb, vc);
					reload_stack();
				}
				v_set(env, &R[a], vt);
				break;
			}
			case BC_CGETSX: {
				loadB();
				loadEx();

				Value vb = *fun->_caps[b]->_ptr;
				Value vc = K[ex];

				Value vt;
				if (v_is_table(vb)) {
					vt = ai_table_getis(env, v_as_table(vb), v_as_str(vc));
				}
				else if (v_is_mod(vb)) {
					vt = ai_table_getis(env, &v_as_mod(vb)->_table, v_as_str(vc));
				}
				else if (unlikely(v_is_tuple(vb) || v_is_list(vb))) {
					ai_err_bad_index(env, v_as_obj(vb)->_vtable->_name, v_typename(vc));
				}
				else {
					vt = vm_meta_get(env, vb, vc);
					reload_stack();
				}
				v_set(env, &R[a], vt);
				break;
			}
			case BC_SET: {
				loadB();
				loadC();

				Value va = R[a];
				Value vb = R[b];
				Value vc = R[c];

				if (v_is_list(vb)) {
					ai_list_set(env, v_as_list(vb), vc, va);
				}
				else if (v_is_table(vb)) {
					ai_table_set(env, v_as_table(vb), vc, va);
				}
				else {
					vm_meta_set(env, R[b], R[c], R[a]);
					reload_stack();
				}
				break;
			}
			case BC_SETI: {
				loadB();
				loadsC();

				Value va = R[a];
				Value vb = R[b];

				if (v_is_list(vb)) {
					ai_list_seti(env, v_as_list(vb), c, va);
				}
				else if (v_is_table(vb)) {
					ai_table_set(env, v_as_table(vb), v_of_int(c), va);
				}
				else {
					vm_meta_set(env, R[b], v_of_int(c), R[a]);
					reload_stack();
				}
				break;
			}
			case BC_SETS: {
				loadB();
				loadC();

				Value va = R[a];
				Value vb = R[b];
				Value vc = K[c];

				if (v_is_table(vb)) {
					ai_table_set(env, v_as_table(vb), vc, va);
				}
				else if (unlikely(v_is_list(vb))) {
					ai_err_bad_index(env, v_as_obj(vb)->_vtable->_name, v_typename(vc));
				}
				else {
					vm_meta_set(env, vb, vc, va);
					reload_stack();
				}
				break;
			}
			case BC_SETSX: {
				loadB();
				loadEx();

				Value va = R[a];
				Value vb = R[b];
				Value vc = K[ex];

				if (v_is_table(vb)) {
					ai_table_set(env, v_as_table(vb), vc, va);
				}
				else if (unlikely(v_is_list(vb))) {
					ai_err_bad_index(env, v_as_obj(vb)->_vtable->_name, v_typename(vc));
				}
				else {
					vm_meta_set(env, vb, vc, va);
					reload_stack();
				}
				break;
			}
			case BC_CSETS: {
				loadB();
				loadC();

				Value va = R[a];
				Value vb = *fun->_caps[b]->_ptr;
				Value vc = K[c];

				if (v_is_table(vb)) {
					ai_table_set(env, v_as_table(vb), vc, va);
				}
				else if (unlikely(v_is_list(vb))) {
					ai_err_bad_index(env, v_as_obj(vb)->_vtable->_name, v_typename(vc));
				}
				else {
					vm_meta_set(env, vb, vc, va);
					reload_stack();
				}
				break;
			}
			case BC_CSETSX: {
				loadB();
				loadEx();

				Value va = R[a];
				Value vb = *fun->_caps[b]->_ptr;
				Value vc = K[ex];

				if (v_is_table(vb)) {
					ai_table_set(env, v_as_table(vb), vc, va);
				}
				else if (unlikely(v_is_list(vb))) {
					ai_err_bad_index(env, v_as_obj(vb)->_vtable->_name, v_typename(vc));
				}
				else {
					vm_meta_set(env, vb, vc, va);
					reload_stack();
				}
				break;
			}
			case BC_NEG: {
				loadB();

				Value vb = R[b];

				if (v_is_int(vb)) {
					a_int val = ai_op_neg_int(v_as_int(vb));
					v_set_int(&R[a], val);
				}
				else if (v_is_float(vb)) {
					a_float val = ai_op_neg_float(v_as_float(vb));
					v_set_float(&R[a], val);
				}
				else {
					Value vt = vm_meta_unr(env, vb, TM_NEG);
					reload_stack();
					v_set(env, &R[a], vt);
				}
				break;
			}
			case BC_BNOT: {
				loadB();

				Value vb = R[b];

				if (v_is_int(vb)) {
					a_int val = ai_op_bnot_int(v_as_int(vb));
					v_set_int(&R[a], val);
				}
				else {
					Value vt = vm_meta_unr(env, vb, TM_BIT_NOT);
					reload_stack();
					v_set(env, &R[a], vt);
				}
				break;
			}
			case BC_LEN: {
				loadB();

				Value vb = R[b];

				Value vt = vm_meta_len(env, vb);
				reload_stack();
				v_set(env, &R[a], vt);
				break;
			}
			case BC_UNBOX: {
				loadB();
				loadC();

				Value vb = R[b];

				if (v_is_tuple(vb)) {
					GTuple* val = v_as_tuple(vb);
					v_mov_all_with_nil(env, &R[a], c, val->_body, val->_len);
				}
				else if (v_is_list(vb)) {
					GList* val = v_as_list(vb);
					v_mov_all_with_nil(env, &R[a], c, val->_ptr, val->_len);
				}
				else {
					vm_meta_unbox(env, vb, &R[b], c);
					reload_stack();
					adjust_top();
				}
				break;
			}
			case BC_UNBOXV: {
				loadB();

				Value vb = R[b];

				if (v_is_tuple(vb)) {
					GTuple* val = v_as_tuple(vb);
					check_stack(&R[a] + val->_len);
					v_mov_all_with_nil(env, &R[a], RFLAG_COUNT_VARARG, val->_body, val->_len);
				}
				else if (v_is_list(vb)) {
					GList* val = v_as_list(vb);
					check_stack(&R[a] + val->_len);
					v_mov_all_with_nil(env, &R[a], RFLAG_COUNT_VARARG, val->_ptr, val->_len);
				}
				else {
					vm_meta_unbox(env, vb, &R[b], RFLAG_COUNT_VARARG);
					reload_stack();
				}
				break;
			}
			case BC_ADD:
			case BC_SUB:
			case BC_MUL: {
				loadB();
				loadC();

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
					Value vt = vm_meta_bin(env, vb, vc, bin_op2tm(op));
					reload_stack();
					v_set(env, &R[a], vt);
				}
				break;
			}
			case BC_DIV:
			case BC_MOD: {
				loadB();
				loadC();

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
					Value vt = vm_meta_bin(env, vb, vc, bin_op2tm(op));
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
				loadB();
				loadC();

				a_u32 op = bc - BC_ADD + OP_ADD;
				Value vb = R[b];
				Value vc = R[c];

				if (v_is_int(vb) && v_is_int(vc)) {
					a_int val = ai_op_bin_int(v_as_int(vb), v_as_int(vc), op);
					v_set_int(&R[a], val);
				}
				else {
					Value vt = vm_meta_bin(env, vb, vc, bin_op2tm(op));
					v_set(env, &R[a], vt);
				}
				break;
			}
			case BC_ADDI:
			case BC_SUBI:
			case BC_MULI: {
				loadB();
				loadsC();

				a_u32 op = bc - BC_ADDI + OP_ADD;
				Value vb = R[b];
				a_int ic = cast(a_int, c);

				if (v_is_int(vb)) {
					a_int val = ai_op_bin_int(v_as_int(vb), ic, op);
					v_set_int(&R[a], val);
				}
				else if (v_is_float(vb)) {
					a_float val = ai_op_bin_float(v_as_float(vb), ic, op);
					v_set_float(&R[a], val);
				}
				else {
					Value vt = vm_meta_bin(env, vb, v_of_int(ic), bin_op2tm(op));
					reload_stack();
					v_set(env, &R[a], vt);
				}
				break;
			}
			case BC_DIVI:
			case BC_MODI: {
				loadB();
				loadsC();

				a_u32 op = bc - BC_ADDI + OP_ADD;
				Value vb = R[b];
				a_int ic = cast(a_int, c);

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
					Value vt = vm_meta_bin(env, vb, v_of_int(ic), bin_op2tm(op));
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
				loadB();
				loadsC();

				a_u32 op = bc - BC_ADDI + OP_ADD;
				Value vb = R[b];
				a_int ic = cast(a_int, c);

				if (v_is_int(vb)) {
					a_int val = ai_op_bin_int(v_as_int(vb), ic, op);
					v_set_int(&R[a], val);
				}
				else {
					Value vt = vm_meta_bin(env, vb, v_of_int(ic), bin_op2tm(op));
					reload_stack();
					v_set(env, &R[a], vt);
				}
				break;
			}
			{ /* Begin of branch instructions. */
				a_bool z;
			case BC_BZ:
			case BC_BNZ: {
				z = v_to_bool(R[a]);

				goto vm_test;
			}
			case BC_BEQ:
			case BC_BNE: {
				loadB();

				z = ai_vm_equals(env, R[a], R[b]);

				goto vm_test;
			}
			case BC_BLT:
			case BC_BNLT: {
				loadB();

				Value va = R[a];
				Value vb = R[b];

				if (v_is_int(va) && v_is_int(vb)) {
					z = ai_op_cmp_int(v_as_int(va), v_as_int(vb), OP_LT);
				}
				else if (v_is_num(va) && v_is_num(vb)) {
					z = ai_op_cmp_float(v_as_num(va), v_as_num(vb), OP_LT);
				}
				else {
					z = vm_meta_cmp(env, va, vb, TM_LT);
				}

				goto vm_test;
			}
			case BC_BLE:
			case BC_BNLE: {
				loadB();

				Value va = R[a];
				Value vb = R[b];

				if (v_is_int(va) && v_is_int(vb)) {
					z = ai_op_cmp_int(v_as_int(va), v_as_int(vb), OP_LE);
				}
				else if (v_is_num(va) && v_is_num(vb)) {
					z = ai_op_cmp_float(v_as_num(va), v_as_num(vb), OP_LE);
				}
				else {
					z = vm_meta_cmp(env, va, vb, TM_LE);
				}

				goto vm_test;
			}
			case BC_BEQI:
			case BC_BNEI: {
				loadsBx();

				Value va = R[b];

				if (v_is_int(va)) {
					z = ai_op_cmp_int(v_as_int(va), b, OP_EQ);
				}
				else if (v_is_float(va)) {
					z = ai_op_cmp_float(v_as_float(va), b, OP_EQ);
				}
				else {
					z = false;
				}

				goto vm_test;
			}
			case BC_BLTI:
			case BC_BNLTI: {
				loadsBx();

				Value va = R[a];

				if (v_is_int(va)) {
					z = ai_op_cmp_int(v_as_int(va), b, OP_LT);
				}
				else if (v_is_float(va)) {
					z = ai_op_cmp_float(v_as_float(va), b, OP_LT);
				}
				else {
					z = vm_meta_cmp(env, va, v_of_int(b), TM_LT);
				}

				goto vm_test;
			}
			case BC_BLEI:
			case BC_BNLEI: {
				loadsBx();

				Value va = R[a];

				if (v_is_int(va)) {
					z = ai_op_cmp_int(v_as_int(va), b, OP_LE);
				}
				else if (v_is_float(va)) {
					z = ai_op_cmp_float(v_as_float(va), b, OP_LE);
				}
				else {
					z = vm_meta_cmp(env, va, v_of_int(b), TM_LE);
				}

				goto vm_test;
			}
			case BC_BGTI:
			case BC_BNGTI: {
				loadsBx();

				Value va = R[a];

				if (v_is_int(va)) {
					z = ai_op_cmp_int(v_as_int(va), b, OP_GT);
				}
				else if (v_is_float(va)) {
					z = ai_op_cmp_float(v_as_float(va), b, OP_GT);
				}
				else {
					z = vm_meta_cmp(env, v_of_int(b), va, TM_LT); //TODO
				}

				goto vm_test;
			}
			case BC_BGEI:
			case BC_BNGEI: {
				loadsBx();

				Value va = R[a];

				if (v_is_int(va)) {
					z = ai_op_cmp_int(v_as_int(va), b, OP_GE);
				}
				else if (v_is_float(va)) {
					z = ai_op_cmp_float(v_as_float(va), b, OP_GE);
				}
				else {
					z = vm_meta_cmp(env, v_of_int(b), va, TM_LE); //TODO
				}

				goto vm_test;
			}
			vm_test: {
				z ^= cast(a_bool, bc & 1);
				pc += z;
				break;
			}
			} /* End of branch instructions. */
			case BC_J: {
				loadJ();

				pc += j;
				break;
			}
			case BC_CLOSE: {
				vm_close_above(env, &frame._caps, &R[a]);
				break;
			}
			case BC_CALL: {
				loadB();
				loadC();

				env->_stack._top = &R[a + b];

				ai_vm_call(env, &R[a], new(RFlags) {
					._count = c
				});
				check_gc();

				adjust_top();
				break;
			}
			case BC_CALLV: {
				loadB();

				env->_stack._top = &R[a + b];

				ai_vm_call(env, &R[a], new(RFlags) {
					._count = RFLAG_COUNT_VARARG
				});
				check_gc();
				break;
			}
			case BC_CALLM: {
				loadC();

				ai_vm_call(env, &R[a], new(RFlags) {
					._count = c
				});
				check_gc();

				adjust_top();
				break;
			}
			case BC_CALLMV: {
				ai_vm_call(env, &R[a], new(RFlags) {
					._count = RFLAG_COUNT_VARARG
				});
				check_gc();
				break;
			}
			case BC_CAT: {
				loadB();
				loadC();

				GStr* st = vm_cat(env, &R[b], c != 0 ? c - 1 : cast(a_usize, env->_stack._top - &R[b]));
				v_set_obj(env, &R[a], st);

				check_gc();
				break;
			}
			{ /* Begin of return instructions. */
				Value ret;
				Value* top;
			case BC_RET: {
				loadB();

				ret = R[a];
				v_mov_all_with_nil(env, R - 1, frame._rflags._count, &R[a], b);
				top = R - 1 + b;

				goto vm_return;
			}
			case BC_RETM: {
				Value* p = &R[a];
				a_usize n = env->_stack._top - p;

				ret = p != env->_stack._top ? *p : v_of_nil();
				v_mov_all_with_nil(env, R - 1, frame._rflags._count, p, n);
				top = R - 1 + n;

				goto vm_return;
			}
			case BC_FC: {
				a_cfun cf = bcast(a_cfun, fun->_caps[fun->_len - 1]);

				a_u32 n = (*cf)(env);

				reload_stack();

				api_check_elem(env, n);
				v_mov_all_with_nil(env, R - 1, frame._rflags._count, env->_stack._top - n, n);
				top = R - 1 + n;

				goto vm_return;
			}
			case BC_RET1: {
				ret = R[a];
				v_cpy(env, &R[-1], &R[a]);
				top = R;

				goto vm_return;
			}
			case BC_RET0: {
				ret = v_of_nil();
				top = R - 1;

				goto vm_return;
			}
			vm_return: {
				run {
					RcCap* caps = frame._caps;
					RcCap* cap;
					while ((cap = caps) != null) {
						caps = cap->_next;
						ai_cap_soft_close(env, cap);
					}
				}
				if (frame._rflags._count == RFLAG_COUNT_VARARG) {
					env->_stack._top = top;
				}
				env->_frame = frame._prev;
				return ret;
			}
			} /* End of return instructions. */
			default: {
				panic("bad opcode");
			}
		}

#undef loadB
#undef loadBx
#undef loadsBx
#undef loadC
#undef loadsC
#undef loadEx
#undef loadJ
	}

#undef pc
#undef R
}
