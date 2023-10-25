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
    else if (unlikely(v_is_float(v))) {
        return v_float_hash(v);
    }
    else {
        Value vf = ai_obj_vlookftm(env, v, TM___hash__);

        if (v_is_nil(vf)) {
            return v_trivial_hash_unchecked(v);
        }

        Value* base = vm_push_args(env, vf, v);

        Value vr = ai_vm_call_meta(env, base);
        if (!v_is_int(vr)) ai_err_raisef(env, ALO_EINVAL, "result for '__hash__' should be int.");
        return cast(a_hash, v_as_int(vr));
    }
}

a_bool ai_vm_equals(a_henv env, Value v1, Value v2) {
	if (v_get_tag(v1) == v_get_tag(v2)) {
		if (likely(v_has_trivial_equals(v1))) {
			return v_trivial_equals(v1, v2);
		}
		else if (v_is_float(v1)) {
			return ai_op_eq_float(v_as_float(v1), v_as_float(v2));
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
	return v_to_bool(ai_vm_call_meta(env, base));
}

#define v_of_call() v_of_empty()
#define v_is_call(v) v_is_empty(v)

static void vm_look(a_henv env, Value v, GStr* k, Value* pv) {
    a_msg msg;
    Value vm;

    msg = ai_obj_vlook(env, v, k, &vm);

    if (msg == ALO_SOK) {
        v_set(env, &pv[0], vm);
        v_set(env, &pv[1], v);
        return;
    }

    msg = ai_vm_uget(env, v, v_of_obj(k), &vm);

    if (msg == ALO_SOK) {
        v_set(env, &pv[0], v_of_call());
        v_set(env, &pv[1], vm);
        return;
    }

    ai_err_bad_look(env, v_nameof(env, v), k); //TODO
}

Value ai_vm_get(a_henv env, Value v1, Value v2) {
	switch (v_get_tag(v1)) {
		case T_TUPLE: {
            return ai_tuple_get(env, v_as_tuple(v1), v2);
        }
		case T_LIST: {
            return ai_list_get(env, v_as_list(v1), v2);
        }
		case T_TABLE: {
            return ai_table_get(env, v_as_table(v1), v2);
        }
		case T_TYPE: {
            return ai_type_get(env, v_as_type(v1), v2);
        }
		case T_USER: {
            return v_vcall(env, v1, get, v2);
        }
        default: {
            ai_err_bad_tm(env, TM___get__);
        }
	}
}

a_msg ai_vm_uget(a_henv env, Value v1, Value v2, Value* pv) {
    switch (v_get_tag(v1)) {
        case T_TUPLE: {
            return ai_tuple_uget(env, v_as_tuple(v1), v2, pv);
        }
        case T_LIST: {
            return ai_list_uget(env, v_as_list(v1), v2, pv);
        }
        case T_TABLE: {
            return ai_table_uget(env, v_as_table(v1), v2, pv);
        }
        case T_TYPE: {
            return ai_type_uget(env, v_as_type(v1), v2, pv);
        }
        case T_USER: {
            GUser* p = v_as_user(v1);
            a_vfp(uget) uget = g_vfetch(p, uget);
            if (uget == null) return ALO_EXIMPL;
            return g_vcallp(env, p, uget, v2, pv);
        }
        default: {
            return ALO_EXIMPL;
        }
    }
}

void ai_vm_set(a_henv env, Value v1, Value v2, Value v3) {
    switch (v_get_tag(v1)) {
        case T_LIST: {
            return ai_list_set(env, v_as_list(v1), v2, v3);
        }
        case T_TABLE: {
            return ai_table_set(env, v_as_table(v1), v2, v3);
        }
        case T_TYPE: {
            return ai_type_set(env, v_as_type(v1), v2, v3);
        }
        case T_USER: {
            GUser* p = v_as_user(v1);
            a_vfp(set) set = g_vfetch(p, set);
            if (set == null) ai_err_bad_tm(env, TM___set__);
            g_vcallp(env, p, set, v2, v3);
            return;
        }
        default: {
            ai_err_bad_tm(env, TM___set__);
        }
    }
}

a_msg ai_vm_uset(a_henv env, Value v1, Value v2, Value v3) {
    switch (v_get_tag(v1)) {
        case T_LIST: {
            return ai_list_uset(env, v_as_list(v1), v2, v3);
        }
        case T_TABLE: {
            return ai_table_uset(env, v_as_table(v1), v2, v3);
        }
        case T_TYPE: {
            return ai_type_uset(env, v_as_type(v1), v2, v3);
        }
		case T_USER: {
			GUser* p = v_as_user(v1);
			a_vfp(uset) uset = g_vfetch(p, uset);
			if (uset == null) return ALO_EXIMPL;
			return g_vcallp(env, p, uset, v2, v3);
		}
        default: {
            return ALO_EXIMPL;
        }
    }
}

static Value vm_len(a_henv env, Value v) {
	switch (v_get_tag(v)) {
        case T_STR: {
            return v_of_int(v_as_str(v)->_len);
        }
        case T_TUPLE: {
            return v_of_int(v_as_tuple(v)->_len);
        }
        case T_LIST: {
            return v_of_int(v_as_list(v)->_len);
        }
        case T_TABLE: {
            return v_of_int(v_as_table(v)->_len);
        }
        case T_USER: {
            return v_of_int(v_vcall(env, v, len));
        }
        default: {
            ai_err_bad_tm(env, TM___len__);
        }
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
            if (i >= p->_len)
                return (ValueSlice) { null, 0 };
            
            v_set_int(pi, cast(a_i32, i + 1));
            env->_stack._top = vs;
            Value* vd = vm_push_args(env, p->_ptr[i]);
            return (ValueSlice) { vd, 1 };
        }
        case T_LIST: {
            GList* p = v_as_list(vc);
            a_u32 i = cast(a_u32, v_as_int(*pi));
            if (i >= p->_len)
                return (ValueSlice) { null, 0 };
            
            v_set_int(pi, cast(a_i32, i + 1));
            env->_stack._top = vs;
            Value* vd = vm_push_args(env, p->_ptr[i]);
            return (ValueSlice) { vd, 1 };
        }
        case T_TABLE: {
            GTable* p = v_as_table(vc);
            a_u32 i = cast(a_u32, v_as_int(*pi));
            if (i == 0 && p->_len == 0)
                return (ValueSlice) { };

            i = p->_ptr[i]._lnext;
            if (i > p->_hmask)
                return (ValueSlice) { };

            TNode* n = &p->_ptr[i];

            v_set_int(pi, cast(a_i32, i + 1));
            env->_stack._top = vs;
            Value* vd = vm_push_args(env, n->_key, n->_value);
            return (ValueSlice) { vd, 2 };
        }
        case T_TYPE: {
            GType* p = v_as_type(vc);
            a_u32 i = cast(a_u32, v_as_int(*pi));
            if (p->_fields._len == 0 || i <= p->_fields._hmask)
                return (ValueSlice) { };

            DNode* n;
            while ((n = &p->_fields._ptr[i])->_key == null) {
                i += 1;
                if (i > p->_fields._hmask)
                    return (ValueSlice) { };
            }

            v_set_int(pi, cast(a_i32, i + 1));
            env->_stack._top = vs;
            Value* vd = vm_push_args(env, v_of_obj(n->_key), n->_value);
            return (ValueSlice) { vd, 2 };
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
	return ai_vm_call_meta(env, base);
}

static Value vm_meta_bin(a_henv env, Value v1, Value v2, a_enum tm) {
	Value vf = ai_obj_vlooktm(env, v1, tm);

	if (v_is_nil(vf)) {
		ai_err_bad_tm(env, tm);
	}

	Value* base = vm_push_args(env, vf, v1, v2);
	return ai_vm_call_meta(env, base);
}

static a_bool vm_meta_cmp(a_henv env, Value v1, Value v2, a_enum tm) {
	Value vf = ai_obj_vlooktm(env, v1, tm);

	if (v_is_nil(vf)) {
		ai_err_bad_tm(env, tm);
	}

	Value* base = vm_push_args(env, vf, v1, v2);
	return v_to_bool(ai_vm_call_meta(env, base));
}

static GStr* vm_cat(a_henv env, Value* base, a_usize n) {
	GBuf* buf = ai_buf_new(env);
	vm_push_args(env, v_of_obj(buf));

	for (a_usize i = 0; i < n; ++i) {
	    Value v = base[i];
		if (likely(v_is_str(v))) {
			GStr* val = v_as_str(v);
			at_buf_putls(env, buf, val->_ptr, val->_len);
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
				case T_STR: {
					GStr* str = v_as_str(v);
					at_buf_putls(env, buf, str->_ptr, str->_len);
					break;
				}
				case T_TUPLE:
				case T_LIST:
				case T_TABLE:
				case T_FUNC:
				case T_USER:
				case T_TYPE: {
					Value vf = ai_obj_vlooktm(env, v, TM___str__);
					if (v_is_nil(vf)) {
						ai_err_raisef(env, ALO_EINVAL, "cannot convert %s to string.", v_nameof(env, v));
					}
					Value* args = vm_push_args(env, vf, v);
					StkPtr bptr = val2stk(env, base);
					Value vs = ai_vm_call_meta(env, args);
					base = stk2val(env, bptr);
					if (!v_is_str(vs)) {
						ai_err_raisef(env, ALO_EINVAL, "result for '__str__' should be string.");
					}
					GStr* str = v_as_str(v);
					at_buf_putls(env, buf, str->_ptr, str->_len);
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

static a_msg vm_call(a_henv env, Value* dst, Value* bot, a_u32 num_ret, a_u32 flags) {
	GFun* fun;
	Value const* K;
    Frame frame_ = {};
    Frame* const frame = &frame_;

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

    frame->_prev = env->_frame;
    frame->_stack_bot = bot;
    frame->_stack_dst = dst;
    frame->_num_ret = num_ret;
    frame->_flags = flags;

	env->_frame = frame;

tail_call:
	R = frame->_stack_bot;

	frame->_pc = null;

    run { /* Check for function. */
        Value vf = R[0];
        if (v_is_call(vf)) { /* For static call from look. */
            frame->_stack_bot += 1; /* Elision call stub. */
            reload_stack();
            vf = R[0];
        }
        while (unlikely(!v_is_func(vf))) {
            vf = ai_obj_vlooktm(env, vf, TM___call__);
            if (v_is_nil(vf)) {
                ai_err_bad_tm(env, TM___call__);
            }

            if (R > frame->_stack_dst) {
                frame->_stack_bot -= 1;
                reload_stack();
                v_set(env, &R[0], vf);
            }
            else {
                for (Value* p = env->_stack._top; p > R; --p) {
                    v_cpy(env, p, p - 1);
                }
                v_set(env, &R[0], vf);

                env->_stack._top += 1;
            }

            check_stack(env->_stack._top);
        }
		fun = v_as_func(vf);
        if (R > frame->_stack_dst) {
            a_usize len = env->_stack._top - R;
            v_mov_all(env, frame->_stack_dst, R, len);
            env->_stack._top = frame->_stack_dst + len;
            frame->_stack_bot = frame->_stack_dst;
        }
    }

    assume(frame->_stack_bot == frame->_stack_dst, "unexpected stack pointer.");

	frame->_stack_bot += 1;
	reload_stack();

	if (fun->_flags & FUN_FLAG_NATIVE) {
		check_stack(R + ALOI_INIT_CFRAME_STACKSIZE);

		a_msg n = (*fun->_fptr)(env);
		if (unlikely(n < 0))
            unreachable(); /* TODO when error raised. */
        api_check_elem(env, n);

        Value* p = env->_stack._top - n;
        n = min(n, frame->_num_ret);

        ai_cap_close_above(env, frame->_stack_bot);
        v_mov_all(env, frame->_stack_dst, p, n);
        frame->_num_ret -= n;
        frame->_stack_dst += n;
		goto handle_return;
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
                if (b > 0) {
                    ai_table_grow(env, val, b);
                }

                check_gc();
                break;
            }
            case BC_LOOK: {
                loadB();
                loadC();

                Value vb = R[b];
                Value vc = K[c];

                vm_look(env, vb, v_as_str(vc), &R[a]);
                break;
            }
            case BC_LOOKX: {
                loadB();
                loadEx();

                Value vb = R[b];
                Value vc = K[ex];

                vm_look(env, vb, v_as_str(vc), &R[a]);
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
					v_mov_all_with_nil(env, &R[a], c, val->_ptr, val->_len);
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
					v_mov_all(env, &R[a], val->_ptr, val->_len);
                    env->_stack._top = &R[a + val->_len];
                }
                else if (v_is_list(vb)) {
                    GList* val = v_as_list(vb);
                    check_stack(&R[a] + val->_len);
					v_mov_all(env, &R[a], val->_ptr, val->_len);
                    env->_stack._top = &R[a + val->_len];
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

                vm_call(env, &R[a], &R[a], c, 0);
                check_gc();

                adjust_top();
                break;
            }
            case BC_CALLV: {
                loadB();

                env->_stack._top = &R[a + b];

                vm_call(env, &R[a], &R[a], UINT32_MAX, FRAME_FLAG_VLR);
                check_gc();
                break;
            }
            case BC_TCALL: {
                loadB();
                loadC();

                a_usize n = min(c, frame->_num_ret);

                ai_cap_close_above(env, frame->_stack_bot);
				v_mov_all(env, frame->_stack_dst, &R[a], n);
                frame->_stack_dst += n;
                frame->_num_ret -= n;

                env->_stack._top = &R[a + b];
                frame->_stack_bot = &R[a + c];

                goto handle_tail_call;
            }
            case BC_CALLM: {
                loadC();

				vm_call(env, &R[a], &R[a], c, 0);
                check_gc();

                adjust_top();
                break;
            }
            case BC_CALLMV: {
				vm_call(env, &R[a], &R[a], UINT32_MAX, FRAME_FLAG_VLR);
                check_gc();
                break;
            }
            case BC_TCALLM: {
                loadC();

                a_u32 b = env->_stack._top - &R[a];
				a_u32 n = min(c, frame->_num_ret);

                ai_cap_close_above(env, frame->_stack_bot);
				v_mov_all(env, frame->_stack_dst, &R[a], n);
                frame->_stack_dst += n;
                frame->_num_ret -= n;

                v_mov_all(env, frame->_stack_dst, &R[a + c], b - c);

                env->_stack._top = &R[a + b];
                frame->_stack_bot = &R[a + c];

                goto handle_tail_call;
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
                a_usize n = min(b, frame->_num_ret);

                ai_cap_close_above(env, frame->_stack_bot);
                v_mov_all(env, frame->_stack_dst, p, n);
                frame->_stack_dst += n;
                frame->_num_ret -= n;
                goto handle_return;
            }
            case BC_RETM: {
				Value* p = &R[a];
                a_usize n = env->_stack._top - &R[a];
                n = min(n, frame->_num_ret);

                ai_cap_close_above(env, frame->_stack_bot);
                v_mov_all(env, frame->_stack_dst, p, n);
                frame->_stack_dst += n;
                frame->_num_ret -= n;
				goto handle_return;
            }
            case BC_RET0: {
                ai_cap_close_above(env, frame->_stack_bot);
                goto handle_return;
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

handle_tail_call:
    frame->_flags |= FRAME_FLAG_TAIL;
    goto tail_call;

handle_return:
    env->_frame = frame->_prev;

    if (!(frame->_flags & FRAME_FLAG_VLR)) {
        Value* top = frame->_stack_dst + frame->_num_ret;
        v_set_nil_ranged(frame->_stack_dst, top);
        frame->_stack_dst = top;
    }
    env->_stack._top = frame->_stack_dst;
    return ALO_SOK;

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
		vm_call(env, base, base, cast(a_u32, nret), 0);
	}
	else {
		vm_call(env, base, base, UINT32_MAX, FRAME_FLAG_VLR);
	}
}

Value ai_vm_call_meta(a_henv env, Value* bot) {
    vm_call(env, bot, bot, 1, FRAME_FLAG_META);
    Value v = env->_stack._top[-1];
    env->_stack._top -= 1;
	return v;
}