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
#include "auser.h"
#include "atype.h"
#include "afmt.h"
#include "aenv.h"
#include "agc.h"
#include "aerr.h"
#include "aapi.h"

#include "avm.h"

typedef struct {
	Value const* _ptr;
	a_usize _len;
} ValueSlice;

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

static a_none l_div_0_err(a_henv env) {
	ai_err_raisef(env, ALO_EINVAL, "attempt to divide by 0.");
}

static Value vm_call_meta(a_henv env, Value* base);

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
		Value vf = ai_obj_vlookftm(env, v, TM___hash__);

		if (v_is_nil(vf)) {
			return v_trivial_hash_unchecked(v);
		}

		Value* base = vm_push_args(env, vf, v);

		Value vr = vm_call_meta(env, base);
		if (!v_is_int(vr)) ai_err_raisef(env, ALO_EINVAL, "result for '__hash__' should be int.");
		return cast(a_hash, v_as_int(vr));
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
	}
	else if (v_is_num(v1) && v_is_num(v2)) {
		return ai_op_eq_float(v_as_num(v1), v_as_num(v2));
	}

	Value vf = ai_obj_vlookftm(env, v1, TM___eq__);

	if (v_is_nil(vf)) {
		return v_trivial_equals_unchecked(v1, v2);
	}

	Value* base = vm_push_args(env, vf, v1, v2);
	return v_to_bool(vm_call_meta(env, base));
}

static Value vm_look(a_henv env, Value v, GStr* key) {
	GType* type = v_typeof(env, v);
	Value vf = ai_type_getis(env, type, key);
	if (v_is_nil(vf)) {
		ai_err_bad_look(env, v_nameof(env, v), key);
	}
	return vf;
}

Value ai_vm_meta_get(a_henv env, Value vf, Value v1, Value v2) {
	if (v_is_func(v1)) {
		Value* base = vm_push_args(env, vf, v1, v2);
		return vm_call_meta(env, base);
	}
	else {
		return ai_vm_get(env, vf, v2);
	}
}

Value ai_vm_get(a_henv env, Value v1, Value v2) {
	switch (v_get_tag(v1)) {
		case T_TUPLE:
			return ai_tuple_get(env, v_as_tuple(v1), v2);
		case T_LIST:
			return ai_list_get(env, v_as_list(v1), v2);
		case T_TABLE:
			return ai_table_get(env, v_as_table(v1), v2);
		case T_TYPE:
			return ai_type_get(env, v_as_type(v1), v2);
		case T_AUSER:
			return ai_auser_get(env, v_as_auser(v1), v2);
		default: {
			Value vf = ai_obj_vlookftm(env, v1, TM___get__);
			if (v_is_nil(vf)) ai_err_bad_tm(env, TM___get__);
			return ai_vm_meta_get(env, vf, v1, v2);
		}
	}
}

void ai_vm_meta_set(a_henv env, Value vf, Value v1, Value v2, Value v3) {
	if (v_is_func(v1)) {
		Value* base = vm_push_args(env, vf, v1, v2, v3);
		vm_call_meta(env, base);
	}
	else {
		ai_vm_set(env, vf, v2, v3);
	}
}

void ai_vm_set(a_henv env, Value v1, Value v2, Value v3) {
	if (v_is_list(v1)) {
		ai_list_set(env, v_as_list(v1), v2, v3);
	}
	else if (v_is_table(v1)) {
		ai_table_set(env, v_as_table(v1), v2, v3);
	}
	else if (v_is_type(v1)) {
		ai_type_set(env, v_as_type(v1), v2, v3);
	}
	else if (v_is_auser(v1)) {
		ai_auser_set(env, v_as_auser(v1), v2, v3);
	}
	else {
		Value vf = ai_obj_vlookftm(env, v1, TM___set__);
		if (v_is_nil(vf)) ai_err_bad_tm(env, TM___set__);
		ai_vm_meta_set(env, vf, v1, v2, v3);
	}
}

static Value vm_meta_len(a_henv env, Value v) {
	Value vf = ai_obj_vlookftm(env, v, TM___len__);
	if (v_is_nil(vf)) {
		if (v_is_auser(v)) {
			return v_of_int(v_as_auser(v)->_len);
		}
		else {
			ai_err_bad_tm(env, TM___len__);
		}
	}
	else {
		Value* base = vm_push_args(env, vf, v);
		Value vr = vm_call_meta(env, base);
		if (!v_is_int(vr)) {
			ai_err_raisef(env, ALO_EINVAL, "result for '__len__' should be int.");
		}
		return vr;
	}
}

static Value vm_len(a_henv env, Value v) {
	switch (v_get_tag(v)) {
		case T_ISTR:
		case T_HSTR:
			return v_of_int(v_as_str(v)->_len);
		case T_TUPLE:
			return v_of_int(v_as_tuple(v)->_len);
		case T_LIST:
			return v_of_int(v_as_list(v)->_len);
		case T_TABLE:
			return v_of_int(v_as_table(v)->_len);
		default:
			return vm_meta_len(env, v);
	}
}

static void vm_iter(a_henv env, Value* restrict vs, Value v) {
    switch (v_get_tag(v)) {
        case T_TUPLE: {
            v_set(env, &vs[0], v);
            v_set_int(&vs[2], 0);
            break;
        }
        case T_LIST: {
            v_set(env, &vs[0], v);
            v_set_int(&vs[2], 0);
            break;
        }
        case T_TABLE: {
            v_set(env, &vs[0], v);
            v_set_int(&vs[2], 0);
            break;
        }
        case T_TYPE: {
            v_set(env, &vs[0], v);
            v_set_int(&vs[2], 0);
            break;
        }
        default: {
            //TODO
            ai_err_bad_tm(env, TM___iter__);
        }
    }
}

static ValueSlice vm_next(a_henv env, Value* restrict vs, Value* vb) {
    Value vc = vb[0];
    Value* pi = &vb[2];
    switch (v_get_tag(vc)) {
        case T_TUPLE: {
            GTuple* p = v_as_tuple(vc);
            a_u32 i = cast(a_u32, v_as_int(*pi));
            if (cast(a_u32, i) >= p->_len)
                return new(ValueSlice) { null };
            
            v_set_int(pi, i + 1);
            env->_stack._top = vs;
            Value* vd = vm_push_args(env, p->_body[i]);
            return new(ValueSlice) { vd, 1 };
        }
        case T_LIST: {
            GList* p = v_as_list(vc);
            a_u32 i = cast(a_u32, v_as_int(*pi));
            if (i >= p->_len)
                return new(ValueSlice) { null };
            
            v_set_int(pi, i + 1);
            env->_stack._top = vs;
            Value* vd = vm_push_args(env, p->_ptr[i]);
            return new(ValueSlice) { vd, 1 };
        }
        case T_TABLE: {
            GTable* p = v_as_table(vc);
            a_u32 i = cast(a_u32, v_as_int(*pi));
            if (p->_len == 0 || unwrap_unsafe(p->_ptr->_link._prev) == cast(a_i32, i))
                return new(ValueSlice) { };

            i = unwrap(p->_ptr[i]._link._next);
            HNode* n = &p->_ptr[i];

            v_set_int(pi, i);
            env->_stack._top = vs;
            Value* vd = vm_push_args(env, n->_key, n->_value);
            return new(ValueSlice) { vd, 2 };
        }
        case T_TYPE: {
            GType* p = v_as_type(vc);
            a_u32 i = cast(a_u32, v_as_int(*pi));
            
            TDNode* n;
            loop {
                if (i > p->_hmask)
                    return new(ValueSlice) { };
                i += 1;
                n = &p->_ptr[i];
                if (n->_key != null)
                    break;
            }

            v_set_int(pi, i);
            env->_stack._top = vs;
            Value* vd = vm_push_args(env, v_of_obj(n->_key), p->_values[n->_index]);
            return new(ValueSlice) { vd, 2 };
        }
        default: {
            //TODO
            ai_err_bad_tm(env, TM___next__);
        }
    }
}

static Value vm_meta_unr(a_henv env, Value v1, a_enum tm) {
	Value vf = ai_obj_vlooktm(env, v1, tm);

	if (v_is_nil(vf)) {
		ai_err_bad_tm(env, tm);
	}

	Value* base = vm_push_args(env, vf, v1);
	return vm_call_meta(env, base);
}

static Value vm_meta_bin(a_henv env, Value v1, Value v2, a_enum tm) {
	Value vf = ai_obj_vlooktm(env, v1, tm);

	if (v_is_nil(vf)) {
		ai_err_bad_tm(env, tm);
	}

	Value* base = vm_push_args(env, vf, v1, v2);
	return vm_call_meta(env, base);
}

static a_bool vm_meta_cmp(a_henv env, Value v1, Value v2, a_enum tm) {
	Value vf = ai_obj_vlooktm(env, v1, tm);

	if (v_is_nil(vf)) {
		ai_err_bad_tm(env, tm);
	}

	Value* base = vm_push_args(env, vf, v1, v2);
	return v_to_bool(vm_call_meta(env, base));
}

static GStr* vm_cat(a_henv env, Value* base, a_usize n) {
	GBuf* buf = ai_buf_new(env);
	vm_push_args(env, v_of_obj(buf));

	for (a_usize i = 0; i < n; ++i) {
	    Value v = base[i];
		if (likely(v_is_str(v))) {
			GStr* val = v_as_str(v);
			at_buf_putls(env, buf, val->_data, val->_len);
		}
		else {
			switch (v_get_tag(v)) {
				case T_NIL: {
					at_buf_puts(env, buf, "nil");
					break;
				}
				case T_FALSE: {
					at_buf_puts(env, buf, "false");
					break;
				}
				case T_TRUE: {
					at_buf_puts(env, buf, "true");
					break;
				}
				case T_INT: {
					at_fmt_puti(env, buf, v_as_int(v));
					break;
				}
				case T_PTR: {
					at_fmt_putp(env, buf, v_as_ptr(v));
					break;
				}
				case T_HSTR:
				case T_ISTR: {
					GStr* str = v_as_str(v);
					at_buf_putls(env, buf, str->_data, str->_len);
					break;
				}
				case T_TUPLE:
				case T_LIST:
				case T_TABLE:
				case T_FUNC:
				case T_CUSER:
				case T_AUSER:
				case T_TYPE: {
					Value vf = ai_obj_vlooktm(env, v, TM___str__);
					if (v_is_nil(vf)) {
						ai_err_raisef(env, ALO_EINVAL, "cannot convert %s to string.", v_nameof(env, v));
					}
					Value* args = vm_push_args(env, vf, v);
					StkPtr bptr = val2stk(env, base);
					Value vs = vm_call_meta(env, args);
					base = stk2val(env, bptr);
					if (!v_is_str(vs)) {
						ai_err_raisef(env, ALO_EINVAL, "result for '__str__' should be string.");
					}
					GStr* str = v_as_str(v);
					at_buf_putls(env, buf, str->_data, str->_len);
					break;
				}
				default: {
					at_fmt_putf(env, buf, v_as_float(v));
					break;
				}
			}
		}
	}

	GStr* result = at_buf_tostr(env, buf);
	at_buf_deinit(G(env), *buf);

	return result;
}

static void v_mov_all_with_nil(a_henv env, Value* dst, a_usize dst_len, Value const* src, a_usize src_len) {
	a_usize i = 0;

	a_usize arg_len = min(dst_len, src_len);
	while (i < arg_len) {
		v_cpy(env, &dst[i], &src[i]);
		i += 1;
	}

	while (i < dst_len) {
		v_set_nil(&dst[i]);
		i += 1;
	}
}

static void v_mov_all(a_henv env, Value* dst, Value const* src, a_usize len) {
	assume(dst < src || len == 0, "bad order.");
	for (a_usize i = 0; i < len; ++i) {
		v_cpy(env, &dst[i], &src[i]);
	}
}

static a_u32 vm_fetch_ex(a_insn const** pc) {
	a_insn const* ip = *pc;
	*pc = ip + 1;
	assume(bc_load_op(ip) == BC_EX, "not extra operand.");
	return bc_load_ax(ip);
}

static ValueSlice vm_call_uncleared(a_henv env, Frame* frame);

static void vm_call_nret(a_henv env, Value* base, a_u32 nret) {
	Frame frame = {
		._env = env,
		._prev = env->_frame,
		._stack_bot = val2stk(env, base),
		._nret = nret
	};

	ValueSlice slice = vm_call_uncleared(env, &frame);
	env->_frame = frame._prev;

	base = stk2val(env, frame._stack_bot) - 1;

	ai_cap_close_above(env, base);
	v_mov_all_with_nil(env, base, nret, slice._ptr, slice._len);
	env->_stack._top = base + slice._len;
}

static void vm_call_vret(a_henv env, Value* base) {
	Frame frame = {
		._env = env,
		._prev = env->_frame,
		._stack_bot = val2stk(env, base),
		._fvret = true
	};

	ValueSlice slice = vm_call_uncleared(env, &frame);
	env->_frame = frame._prev;

	base = stk2val(env, frame._stack_bot) - 1;

	ai_cap_close_above(env, base);
	v_mov_all(env, base, slice._ptr, slice._len);
	env->_stack._top = base + slice._len;
}

static Value vm_call_meta(a_henv env, Value* base) {
	Frame frame = {
		._env = env,
		._prev = env->_frame,
		._stack_bot = val2stk(env, base),
		._fmeta = true
	};

	ValueSlice slice = vm_call_uncleared(env, &frame);
	env->_frame = frame._prev;

	base = stk2val(env, frame._stack_bot) - 1;

	ai_cap_close_above(env, base);

	env->_stack._top = base;

	return slice._len > 0 ? *slice._ptr : v_of_nil();
}

static ValueSlice vm_call_uncleared(a_henv env, Frame* frame) {
	GFun* fun;
	Value const* K;
#define pc (frame->_pc)
#if ALO_STACK_RELOC
    Value* R;
# define R R
# define check_stack(top) ({ a_isize _d = ai_stk_check(env, top); base = ptr_disp(Value, R, _d); reload_stack(); })
# define reload_stack() quiet(R = env->_stack._bot)
#else
# define R (frame->_stack_bot)
# define check_stack(top) ({ a_isize _d = ai_stk_check(env, top); assume(_d == 0, "stack moved."); reload_stack(); })
# define reload_stack() ((void) 0)
#endif
#define check_gc() ai_gc_trigger_ext(env, (void) 0, reload_stack())
#define adjust_top() quiet(env->_stack._top = &R[fun->_proto->_nstack])

	env->_frame = frame;

tail_call:
	R = stk2val(env, frame->_stack_bot);

	frame->_pc = null;

    run { /* Check for function. */
        Value vf = R[0];
        while (unlikely(!v_is_func(vf))) {
            vf = ai_obj_vlooktm(env, vf, TM___call__);
            if (v_is_nil(vf)) {
                ai_err_bad_tm(env, TM___call__);
            }

            for (Value* p = env->_stack._top; p > R; --p) {
                v_cpy(env, p, p - 1);
            }
            v_set(env, &R[0], vf);

            env->_stack._top += 1;

            check_stack(env->_stack._top);
        }
		fun = v_as_func(vf);
    }

	frame->_stack_bot = val2stk(env, R + 1);
	reload_stack();

	if (fun->_flags & FUN_FLAG_NATIVE) {
		check_stack(R + ALOI_INIT_CFRAME_STACKSIZE);

		a_u32 n = (*fun->_fptr)(env);
		api_check_elem(env, n);
		return new(ValueSlice) { env->_stack._top - n, n };
	}
	else {
		GProto* proto = fun->_proto;

		check_stack(R + proto->_nstack);

		frame->_pc = proto->_code;

		K = proto->_consts;
		if (!(proto->_flags & FUN_FLAG_VARARG)) {
			adjust_top();
		}
	}

    loop {
        a_u32 bc;
        a_u32 a;
        a_insn const* ip = pc++;

#define loadB() a_usize b = bc_load_b(ip)
#define loadBx() a_usize b = bc_load_bx(ip)
#define loadsBx() a_i16 b = bc_load_sbx(ip)
#define loadC() a_usize c = bc_load_c(ip)
#define loadsC() a_i8 c = bc_load_sc(ip)
#define loadJ() a_i32 j = bc_load_sax(ip)
#define loadEx() a_usize ex = vm_fetch_ex(&pc)

        a = bc_load_a(ip);
        bc = bc_load_op(ip);
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

                GFun* v = ai_fun_new(env, fun->_proto->_subs[b]);
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
            case BC_LBOX: {
                loadB();
                loadC();

                GList* val = ai_list_new(env);
                v_set_obj(env, env->_stack._top, val);
                env->_stack._top += 1;

                ai_list_push_all(env, val, &R[b], c);
                v_set_obj(env, &R[a], val);
                check_gc();

                adjust_top();
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
            case BC_HNEW: {
                loadBx();

                GTable* val = ai_table_new(env);
                v_set_obj(env, &R[a], val);
                ai_table_hint(env, val, b);

                check_gc();
                break;
            }
            case BC_LOOK: {
                loadB();
                loadC();

                Value vb = R[b];
                Value vc = K[c];

                Value vt = vm_look(env, vb, v_as_str(vc));
                v_set(env, &R[a], vt);
                v_set(env, &R[a + 1], vb);

                break;
            }
            case BC_LOOKX: {
                loadB();
                loadEx();

                Value vb = R[b];
                Value vc = K[ex];

                Value vt = vm_look(env, vb, v_as_str(vc));
                v_set(env, &R[a], vt);
                v_set(env, &R[a + 1], vb);

                break;
            }
            case BC_ITER: {
                loadB();

                Value vb = R[b];

                vm_iter(env, &R[a], vb);
                break;
            }
            case BC_FORG: {
                loadB();
                loadC();

                ValueSlice vs = vm_next(env, &R[a], &R[b]);
                if (vs._ptr != null) {
					v_mov_all_with_nil(env, &R[a], c, vs._ptr, vs._len);
                    adjust_top();
                    pc += 1;
                }
                break;
            }
            case BC_FORGV: {
                loadB();

                ValueSlice vs = vm_next(env, &R[a], &R[b]);
                if (vs._ptr != null) {
					v_mov_all_with_nil(env, &R[a], RFLAG_COUNT_VARARG, vs._ptr, vs._len);
                    pc += 1;
                }
                break;
            }
            case BC_GET: {
                loadB();
                loadC();

                Value vb = R[b];
                Value vc = R[c];

                Value vt = ai_vm_get(env, vb, vc);
                reload_stack();
                v_set(env, &R[a], vt);
                break;
            }
            case BC_GETI: {
                loadB();
                loadsC();

                Value vb = R[b];

                Value vt = ai_vm_get(env, vb, v_of_int(c));
                reload_stack();
                v_set(env, &R[a], vt);
                break;
            }
            case BC_GETS: {
                loadB();
                loadC();

                Value vb = R[b];
                Value vc = K[c];

                Value vt = ai_vm_get(env, vb, vc);
                reload_stack();
                v_set(env, &R[a], vt);
                break;
            }
            case BC_GETSX: {
                loadB();
                loadEx();

                Value vb = R[b];
                Value vc = K[ex];

                Value vt = ai_vm_get(env, vb, vc);
                reload_stack();
                v_set(env, &R[a], vt);
                break;
            }
            case BC_CGETS: {
                loadB();
                loadC();

                Value vb = *fun->_caps[b]->_ptr;
                Value vc = K[c];

                Value vt = ai_vm_get(env, vb, vc);
                reload_stack();
                v_set(env, &R[a], vt);
                break;
            }
            case BC_CGETSX: {
                loadB();
                loadEx();

                Value vb = *fun->_caps[b]->_ptr;
                Value vc = K[ex];

                Value vt = ai_vm_get(env, vb, vc);
                reload_stack();
                v_set(env, &R[a], vt);
                break;
            }
            case BC_SET: {
                loadB();
                loadC();

                Value va = R[a];
                Value vb = R[b];
                Value vc = R[c];

                ai_vm_set(env, vb, vc, va);
                reload_stack();
                break;
            }
            case BC_SETI: {
                loadB();
                loadsC();

                Value va = R[a];
                Value vb = R[b];

                ai_vm_set(env, vb, v_of_int(c), va);
                reload_stack();
                break;
            }
            case BC_SETS: {
                loadB();
                loadC();

                Value va = R[a];
                Value vb = R[b];
                Value vc = K[c];

                ai_vm_set(env, vb, vc, va);
                reload_stack();
                break;
            }
            case BC_SETSX: {
                loadB();
                loadEx();

                Value va = R[a];
                Value vb = R[b];
                Value vc = K[ex];

                ai_vm_set(env, vb, vc, va);
                reload_stack();
                break;
            }
            case BC_CSETS: {
                loadB();
                loadC();

                Value va = R[a];
                Value vb = *fun->_caps[b]->_ptr;
                Value vc = K[c];

                ai_vm_set(env, vb, vc, va);
                reload_stack();
                break;
            }
            case BC_CSETSX: {
                loadB();
                loadEx();

                Value va = R[a];
                Value vb = *fun->_caps[b]->_ptr;
                Value vc = K[ex];

                ai_vm_set(env, vb, vc, va);
                reload_stack();
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
                    Value vt = vm_meta_unr(env, vb, TM___neg__);
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
                    Value vt = vm_meta_unr(env, vb, TM___bnot__);
                    reload_stack();
                    v_set(env, &R[a], vt);
                }
                break;
            }
            case BC_LEN: {
                loadB();

                Value vb = R[b];

                Value vt = vm_len(env, vb);
                reload_stack();
                v_set(env, &R[a], vt);
                break;
            }
            case BC_UNBOX: { //TODO
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
					ai_err_raisef(env, ALO_EINVAL, "cannot unbox '%s' value.", v_nameof(env, vb));
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
					ai_err_raisef(env, ALO_EINVAL, "cannot unbox '%s' value.", v_nameof(env, vb));
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
                    Value vt = vm_meta_bin(env, vb, vc, ai_op_bin2tm(op));
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
                    Value vt = vm_meta_bin(env, vb, vc, ai_op_bin2tm(op));
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
                    Value vt = vm_meta_bin(env, vb, vc, ai_op_bin2tm(op));
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
                    Value vt = vm_meta_bin(env, vb, v_of_int(ic), ai_op_bin2tm(op));
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
                    Value vt = vm_meta_bin(env, vb, v_of_int(ic), ai_op_bin2tm(op));
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
                    Value vt = vm_meta_bin(env, vb, v_of_int(ic), ai_op_bin2tm(op));
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
                    case BC_BN:
                    case BC_BNN: {
                        z = v_is_nil(R[a]);

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
                            z = vm_meta_cmp(env, va, vb, TM___lt__);
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
                            z = vm_meta_cmp(env, va, vb, TM___le__);
                        }

                        goto vm_test;
                    }
                    case BC_BEQI:
                    case BC_BNEI: {
                        loadsBx();

                        Value va = R[a];

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
                            z = vm_meta_cmp(env, va, v_of_int(b), TM___lt__);
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
                            z = vm_meta_cmp(env, va, v_of_int(b), TM___le__);
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
                            z = vm_meta_cmp(env, v_of_int(b), va, TM___lt__); //TODO
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
                            z = vm_meta_cmp(env, v_of_int(b), va, TM___le__); //TODO
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
            case BC_TBC: {
                ai_cap_mark_tbc(env, &R[a]);
                break;
            }
            case BC_CLOSE: {
				ai_cap_close_above(env, &R[a]);
                break;
            }
            case BC_CALL: {
                loadB();
                loadC();

                env->_stack._top = &R[a + b];

				vm_call_nret(env, &R[a], c);
                check_gc();

                adjust_top();
                break;
            }
            case BC_CALLV: {
                loadB();

                env->_stack._top = &R[a + b];

				vm_call_vret(env, &R[a]);
                check_gc();
                break;
            }
            case BC_TCALL: {
                loadB();
                loadC();

				ai_cap_close_above(env, R - 1);
				v_mov_all(env, R - 1, &R[a], b);

                env->_stack._top = &R[b - 1];
                frame->_stack_bot = &R[c - 1];
				frame->_ftail = true;

                goto tail_call;
            }
            case BC_CALLM: {
                loadC();

				vm_call_nret(env, &R[a], c);
                check_gc();

                adjust_top();
                break;
            }
            case BC_CALLMV: {
				vm_call_vret(env, &R[a]);
                check_gc();
                break;
            }
            case BC_TCALLM: {
                loadC();

				a_u32 n = env->_stack._top - &R[a];

				ai_cap_close_above(env, R - 1);
				v_mov_all(env, R - 1, &R[a], n);

                env->_stack._top = &R[n - 1];
                frame->_stack_bot = &R[c - 1];
				frame->_ftail = true;

                goto tail_call;
            }
            case BC_TRIM: {
                loadC();

                assume(&R[a] <= env->_stack._top, "stack corrupt.");
                v_set_nil_ranged(env->_stack._top, &R[a + c]);

                adjust_top();
                break;
            }
            case BC_CAT: {
                loadB();
                loadC();

                env->_stack._top = &R[b + c];
                GStr* st = vm_cat(env, &R[b], c);
                v_set_obj(env, &R[a], st);
                adjust_top();

                check_gc();
                break;
            }
            case BC_RET: {
                loadB();

				Value* p = &R[a];

				return new(ValueSlice) { p, b };
            }
            case BC_RETM: {
				Value* p = &R[a];
                a_usize n = env->_stack._top - &R[a];

				return new(ValueSlice) { p, n };
            }
            case BC_RET0: {
				return new(ValueSlice) { null, 0 };
            }
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

/**
 ** Do 'call' operation on the stack.
 *@param env the environment.
 *@param base the base pointer of function and arguments.
 *@param nret the number of result expected, -1 for vararg.
 *@return The first result.
 */
void ai_vm_call(a_henv env, Value* base, a_i32 nret) {
	if (nret >= 0) {
		vm_call_nret(env, base, cast(a_u32, nret));
	}
	else {
		vm_call_vret(env, base);
	}
}
