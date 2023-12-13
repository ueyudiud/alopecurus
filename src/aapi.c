/*
 * aapi.c
 */

#define aapi_c_
#define ALO_LIB

#include "astr.h"
#include "atuple.h"
#include "alist.h"
#include "atable.h"
#include "afun.h"
#include "amod.h"
#include "atype.h"
#include "actx.h"
#include "agc.h"
#include "aerr.h"
#include "avm.h"
#include "aparse.h"

#include "aapi.h"

a_msg api_tagof(unused a_henv env, Value v) {
    switch (v_get_tag(v)) {
        case T_NIL:
            return ALO_TNIL;
        case T_FALSE:
        case T_TRUE:
            return ALO_TBOOL;
        case T_INT:
            return ALO_TINT;
        case T_PTR:
            return ALO_TPTR;
        case T_OBJ:
            return v_as_obj(v)->_vptr->_tag;
        case T_FLOAT:
            return ALO_TFLOAT;
        default:
            unreachable();
    }
}

/**
 ** Initialize library context, the function should be called before use library.
 ** This function returns initialization result message.
 ** Name other function of the library should called after library is initialized.
 ** Initialize library context twice is undefined behavior.
 ** 
 ** Return message:
 ** ALO_SOK: if initialization success.
 ** ALO_EOUTER: if initialization failed causes by host environment.
 ** 
 *@return a_msg runtime message.
 */
a_msg alo_init(void) {
	return ai_ctx_init();
}

/**
 ** Load integer attribute.
 *@param env the optional runtime environment.
 *@param n the attribute name.
 *@param pi the pointer to store result.
 *@return true if attribute name is valid and false for otherwise.
 */
a_bool alo_attri(unused a_henv env, a_enum n, a_i32* pi) {
	switch (n) {
		case ALO_ATTR_VERSION:
			*pi = ALO_VERSION_NUMBER;
			return true;
		case ALO_ATTR_VARIANT:
			*pi = ALO_VARIANT;
			return true;
		default:
			return false;
	}
}

/**
 ** Set panic behavior for the virtual machine.
 *@param env the environment.
 *@param f the panic function.
 */
void alo_setpanic(a_henv env, a_cfun f) {
	Global* gbl = G(env);
	gbl->_panic = f;
}

/**
 ** Set hook for VM. This function is thread safe.
 *@param env the runtime environment.
 *@param kf the hook function.
 *@param kc the hook context.
 *@param mask the hook mask.
 */
void alo_sethook(a_henv env, a_hfun kf, a_hctx kc, a_flags mask) {
	Global* gbl = G(env);

	/* Make sure null safe. */
	if (kf == null || mask == 0) {
		kf = null;
		mask = 0;
	}

	ai_vm_lock_hook(gbl);

	gbl->_hookf = kf;
	gbl->_hookc = kc;

	gbl->_hookm = mask;
}

/**
 ** Get VM stack size.
 *@param env the runtime environment.
 *@return the stack size.
 */
a_ilen alo_stacksize(a_henv env) {
	return env->_stack._top - ai_stk_bot(env);
}

/**
 ** Reserve enough slots of stack to carry values.
 *@param env the runtime environment.
 *@param n the number of reserved slots.
 *@return false if reserved success and true for otherwise.
 */
a_bool alo_ensure(a_henv env, a_ulen n) {
	Value* const expect = env->_stack._top + n;
	Value* const limit = api_stack_limit(env);
	if (expect > limit) {
#ifdef ALOI_CHECK_API
        if (expect > env->_stack._limit) {
#endif
            a_isize diff = ai_stk_grow(env, expect);

            if (diff & STACK_GROW_FAILED) {
                return true;
            }
#ifdef ALOI_CHECK_API
        }

        env->_frame->_stack_limit = val2stk(env, expect);
#endif
	}
	return false;
}

/**
 ** Truncate or pad stack to specific size.
 ** If new stack size is more than old stack size,
 ** the grown slots will be filled with nil values.
 *@param env the runtime environment.
 *@param n the stack index of top, only valid stack index is available.
 */
void alo_settop(a_henv env, a_ilen n) {
	Value* old_top = env->_stack._top;
	Value* new_top = n >= 0 ? ai_stk_bot(env) + n : old_top + n;

	api_check(new_top >= ai_stk_bot(env) && new_top <= api_stack_limit(env));

	v_set_nil_ranged(old_top, new_top);
	env->_stack._top = new_top;
}

#define MIN_NEG_STACK_INDEX (-255)

a_ilen alo_absindex(a_henv env, a_ilen id) {
	if (id < 0 && id >= MIN_NEG_STACK_INDEX) {
		Value* v = env->_stack._top + id;
		api_check(v >= ai_stk_bot(env));
		id = v - ai_stk_bot(env);
	}
	return id;
}

Value const* api_roslot(a_henv env, a_ilen id) {
	if (id >= 0) {
		Value const* bot = ai_stk_bot(env);
		if (id >= env->_stack._top - bot) 
			return null;
		return bot + id;
	}
	else if (id >= MIN_NEG_STACK_INDEX) {
		Value const* bot = ai_stk_bot(env);
		if (-id > env->_stack._top - bot)
			return null;
		return env->_stack._top + id;
	}
	else if (id == ALO_STACK_INDEX_ERROR) {
		return &env->_error;
	}
	else if (id >= ALO_STACK_INDEX_CAPTURE_BASE) {
		id -= ALO_STACK_INDEX_CAPTURE_BASE;
		GFun* fun = v_as_func(*stk2val(env, env->_frame->_stack_bot - 1));
		if (cast(a_u32, id) >= fun->_len)
			return null;
		return unlikely(fun->_flags & FUN_FLAG_NATIVE) ? &fun->_vals[id] : fun->_caps[id]->_ptr;
	}
	else if (id == ALO_STACK_INDEX_GLOBAL) {
		return &G(env)->_global;
	}
	return null;
}

Value const* api_rdslot(a_henv env, a_ilen id) {
	static Value const v_nil = v_of_nil();
	return api_roslot(env, id) ?: &v_nil;
}

Value api_elem(a_henv env, a_ilen id) {
	return *api_rdslot(env, id);
}

Value* api_wrslot(a_henv env, a_ilen id) {
	Value* v;
	if (id >= MIN_NEG_STACK_INDEX) {
		v = api_stack(env, id);
	}
	else if (id >= ALO_STACK_INDEX_CAPTURE_BASE) {
		id -= ALO_STACK_INDEX_CAPTURE_BASE;
		GFun* fun = v_as_func(*stk2val(env, env->_frame->_stack_bot - 1));
		api_check(cast(a_u32, id) < fun->_len);
		v = unlikely(fun->_flags & FUN_FLAG_NATIVE) ? &fun->_vals[id] : fun->_caps[id]->_ptr;
	}
	else {
		v = null;
		api_panic("bad slot index.");
	}
	return v;
}

Value* api_stack(a_henv env, a_ilen id) {
    Value* v;
    if (id >= 0) {
        v = ai_stk_bot(env) + id;
        api_check(v < env->_stack._top, "stack index out of bound.");
    }
    else if (id >= MIN_NEG_STACK_INDEX) {
        v = env->_stack._top + id;
        api_check(v >= ai_stk_bot(env), "stack index out of bound.");
    }
    return v;
}

void alo_push(a_henv env, a_ilen id) {
	Value v = api_elem(env, id);
	v_set(env, api_incr_stack(env), v);
}

/**
 ** Push value to top of stack. The value is addressing by interactive slot path.
 ** This function return type tag of pushed value.
 **
 *@param env the runtime environment.
 *@param sp the slot path.
 *@param ... parameters of the slot path.
 *@return int type tag of the value.
 */
a_msg alo_pushex(a_henv env, char const* sp, ...) {
	va_list varg;
	va_start(varg, sp);
	a_msg tag = alo_pushvex(env, sp, varg);
	va_end(varg);
	return tag;
}

/**
 ** Push value to top of stack. The value is addressing by interactive slot path.
 ** This function return type tag of pushed value.
 **
 *@param env the runtime environment.
 *@param sp the slot path.
 *@param varg the va_list of parameters of the slot path.
 *@return int type tag of the value.
 *@see alo_pushex
 */
a_msg alo_pushvex(a_henv env, char const* sp, va_list varg) {
	Value v;
	switch (*(sp++)) {
		case '\0': { /* Empty constant. */
			return ALO_EEMPTY;
		}
		case 'i': { /* Index addressing. */
			Value const* p = api_roslot(env, va_arg(varg, a_ilen));
			if (p == null) return ALO_EEMPTY;
			v_cpy(env, &v, p);
			break;
		}
		case 'g': {
			v_set(env, &v, G(env)->_global);
			break;
		}
		default: {
			api_panic("bad slot path");
		}
	}
	
	loop {
		switch (*(sp++)) {
			case '\0': { /* Terminate character. */
				v_set(env, api_incr_stack(env), v);
				return api_tagof(env, v);
			}
			case 'i': { /* Integer index. */
				a_int k = va_arg(varg, a_int);
				switch (v_get_tag(v)) {
					case T_TUPLE: {
						try (ai_tuple_ugeti(env, v_as_tuple(v), k, &v));
						break;
					}
					case T_LIST: {
						try (ai_list_ugeti(env, v_as_list(v), k, &v));
						break;
					}
					case T_TABLE: {
						try (ai_table_geti(env, v_as_table(v), k, &v));
						break;
					}
					default: {
						return ALO_EXIMPL;
					}
				}
				break;
			}
			default: {
				api_panic("bad slot path");
			}
		}
	}
}

/**
 ** Push a nil value to the top of stack.
 **
 *@param env the runtime environment.
 */
void alo_pushnil(a_henv env) {
	v_set_nil(api_incr_stack(env));
}

/**
 ** Push a boolean value to the top of stack.
 **
 *@param env the runtime environment.
 *@param val zero represent false value and true value for otherwise.
 */
void alo_pushbool(a_henv env, a_bool val) {
	v_set_bool(api_incr_stack(env), val);
}

void alo_pushint(a_henv env, a_int val) {
	v_set_int(api_incr_stack(env), val);
}

void alo_pushfloat(a_henv env, a_float val) {
	v_set_float(api_incr_stack(env), val);
}

void alo_pushptr(a_henv env, void const* val) {
	v_set_ptr(api_incr_stack(env), val);
}

char const* alo_pushstr(a_henv env, void const* src, a_usize len) {
    api_check(src != null || len == 0, "string source violates contract.");
	GStr* str = ai_str_get_or_new(env, src, len);
	v_set_str(env, api_incr_stack(env), str);
	ai_gc_trigger(env);
	return str2ntstr(str);
}

char const* alo_pushntstr(a_henv env, char const* src) {
    api_check(src != null, "string source violates contract.");
	return alo_pushstr(env, src, strlen(src));
}

char const* alo_pushfstr(a_henv env, char const* fmt, ...) {
	va_list varg;
	va_start(varg, fmt);
	char const* res = alo_pushvfstr(env, fmt, varg);
	va_end(varg);
	return res;
}

char const* alo_pushvfstr(a_henv env, char const* fmt, va_list varg) {
    api_check(fmt != null, "format string violates contract.");
	GStr* str = ai_str_format(env, fmt, varg);
	v_set_str(env, api_incr_stack(env), str);
	ai_gc_trigger(env);
	return str2ntstr(str);
}

void alo_pushptype(a_henv env, a_msg tag) {
    api_check(tag < TYPE__COUNT, "not primitive type tag: %d", tag);

    GType* t = g_type(env, tag);
    v_set_obj(env, api_incr_stack(env), t);
}

void alo_pushroute(a_henv env) {
	v_set_obj(env, api_incr_stack(env), env);
}

void alo_copy(a_henv env, a_ilen id_src, a_ilen id_dst) {
    Value const* s = api_rdslot(env, id_src);
    Value* d = api_wrslot(env, id_dst);
    v_cpy(env, d, s);
}

void alo_xmove(a_henv src, a_henv dst, a_ulen n) {
	api_check(src != dst, "same environment.");
	api_check(G(src) == G(dst), "not in same global context.");
	api_check_elem(src, n);
	api_check_slot(dst, n);
	v_cpy_all(src, dst->_stack._top, src->_stack._top - n, n);
	src->_stack._top -= n;
	dst->_stack._top += n;
}

void alo_pop(a_henv env, a_ilen id) {
	Value* d = api_wrslot(env, id);
	api_check(env->_stack._top - 1 != d, "bad pop index.");
	Value s = api_decr_stack(env);
	v_set(env, d, s);
}

a_ulen alo_rotate(a_henv env, a_ilen id, a_ilen n) {
    Value* p = api_stack(env, id);
    Value* r = env->_stack._top;
    Value* q = n >= 0 ? p + n : r + n;
    api_check(p <= q && q <= r, "rotate range out of bound");

    v_reverse(env, p, q);
    v_reverse(env, q, r);
    v_reverse(env, p, r);
    return cast(a_ulen, r - p);
}

a_ulen alo_erase(a_henv env, a_ilen id, a_ulen n) {
    Value* p = api_stack(env, id);
    Value* r = env->_stack._top;
    Value* q = p + n;
    api_check(p <= q && q <= r, "erase range out of bound");

    v_mov_all_fwd(env, p, q, r - q);

    a_ulen m = cast(a_ulen, r - q);
    env->_stack._top = p + m;
    return m;
}

void alo_newtuple(a_henv env, a_ulen n) {
	api_check_elem(env, n);
	GTuple* val = ai_tuple_new(env, env->_stack._top - n, n);
	env->_stack._top -= n;
	v_set_obj(env, api_incr_stack(env), val);
	ai_gc_trigger(env);
}

void alo_newlist(a_henv env, a_ulen n) {
	GList* val = ai_list_new(env);
	v_set_obj(env, api_incr_stack(env), val);
    ai_list_hint(env, val, n);
    ai_gc_trigger(env);
}

void alo_newtable(a_henv env, a_ulen n) {
	GTable* val = ai_table_new(env);
	v_set_obj(env, api_incr_stack(env), val);
	if (n > 0) {
        ai_table_grow(env, val, n);
    }
    ai_gc_trigger(env);
}

void alo_newcfun(a_henv env, a_cfun f, a_ulen n) {
	api_check_elem(env, n);
	GFun* val = ai_cfun_create(env, f, n, env->_stack._top - n);
	env->_stack._top -= n;
	v_set_obj(env, api_incr_stack(env), val);
	ai_gc_trigger(env);
}

a_henv alo_newroute(a_henv env, a_usize ss) {
	GRoute* val = ai_env_new(env, ss);
	v_set_obj(env, api_incr_stack(env), val);
	ai_gc_trigger(env);
	return val;
}

void* alo_newmod(a_henv env, a_usize s) {
    GMod* val = ai_mod_new(env, s);
    v_set_obj(env, api_incr_stack(env), val);
    ai_gc_trigger(env);
    return val->_extra;
}

a_msg alo_compute(a_henv env, a_enum op) {
    switch (op) {
        case ALO_OPADD ... ALO_OPBXOR: {
            api_check_elem(env, 2);
            Value v1 = env->_stack._top[-2];
            Value v2 = env->_stack._top[-1];

            Value v = ai_vm_binary(env, v1, v2, op - ALO_OPADD + OP_ADD);
            env->_stack._top -= 2;

            v_set(env, api_incr_stack(env), v);
            return api_tagof(env, v);
        }
        case ALO_OPNEG ... ALO_OPBNOT: {
            api_check_elem(env, 1);
            Value v1 = api_pre_decr_stack(env);

            Value v = ai_vm_unary(env, v1, op - ALO_OPNEG + OP_NEG);
            api_post_decr_stack(env);

            v_set(env, api_incr_stack(env), v);
            return api_tagof(env, v);
        }
        default: api_panic("bad opcode for alo_compute: %u", op);
    }
}

a_bool alo_compare(a_henv env, a_ilen id1, a_ilen id2, a_enum op) {
    Value v1 = api_elem(env, id1);
    Value v2 = api_elem(env, id2);
    switch (op) {
        case ALO_OPEQ: {
            return ai_vm_equals(env, v1, v2);
        }
        case ALO_OPLT ... ALO_OPLE: {
            return ai_vm_compare(env, v1, v2, op - ALO_OPLT + OP_LT);
        }
        default: api_panic("bad opcode for alo_compare: %u", op);
    }
}

a_msg alo_get(a_henv env, a_ilen id) {
    api_check_elem(env, 1);

    Value v = api_elem(env, id);
    Value vk = env->_stack._top[-1];

    Value vv = ai_vm_get(env, v, vk);

    v_set(env, &env->_stack._top[-1], vv);

    return api_tagof(env, vv);
}

void alo_set(a_henv env, a_ilen id) {
    api_check_elem(env, 2);

    Value v = api_elem(env, id);
    Value vv = env->_stack._top[-1];
    Value vk = env->_stack._top[-2];

    ai_vm_set(env, v, vk, vv);

    env->_stack._top -= 2;
}

/**
 ** Get length of value without call meta method.
 **
 *@param env the runtime environment.
 *@param id the value index.
 *@return a non-negative number for the length of object, or 0 for method not implemented.
 */
a_int alo_rawlen(a_henv env, a_ilen id) {
	Value v = api_elem(env, id);

	switch (v_get_tag(v)) {
		case T_TUPLE: {
			GTuple* p = v_as_tuple(v);
			return cast(a_int, p->_len);
		}
		case T_LIST: {
			GList* p = v_as_list(v);
            return cast(a_int, p->_len);
		}
		case T_TABLE: {
			GTable* p = v_as_table(v);
            return cast(a_int, p->_len);
		}
		default: {
			return 0;
		}
	}
}

a_msg alo_rawgeti(a_henv env, a_ilen id, a_int key) {
	api_check_elem(env, 1);

	Value v = api_elem(env, id);

	switch (v_get_tag(v)) {
		case T_TUPLE: {
			GTuple* p = v_as_tuple(v);
			try (ai_tuple_ugeti(env, p, key, &v));
			break;
		}
		case T_LIST: {
			GList* p = v_as_list(v);
			try (ai_list_ugeti(env, p, key, &v));
			break;
		}
		case T_TABLE: {
			GTable* p = v_as_table(v);
			try (ai_table_geti(env, p, key, &v));
			break;
		}
		default: {
			return ALO_EXIMPL;
		}
	}

	v_set(env, api_incr_stack(env), v);
	return api_tagof(env, v);
}

a_msg alo_rawget(a_henv env, a_ilen id) {
	api_check_elem(env, 1);

	Value v = api_elem(env, id);
	Value vk = api_decr_stack(env);

    switch (v_get_tag(v)) {
        case T_TUPLE: {
            GTuple* p = v_as_tuple(v);
            try (ai_tuple_uget(env, p, vk, &v));
            break;
        }
        case T_LIST: {
            GList* p = v_as_list(v);
            try (ai_list_uget(env, p, vk, &v));
            break;
        }
        case T_TABLE: {
            GTable* p = v_as_table(v);
            try (ai_table_get(env, p, vk, &v));
            break;
        }
        default: {
            return ALO_EXIMPL;
        }
    }

	v_set(env, api_incr_stack(env), v);
	return api_tagof(env, v);
}

a_msg alo_rawset(a_henv env, a_ilen id) {
	api_check_elem(env, 2);

	Value v = api_elem(env, id);
	Value vk = env->_stack._top[-2];
    Value vv = env->_stack._top[-1];

    switch (v_get_tag(v)) {
        case T_LIST: {
            GList* p = v_as_list(v);
            try (ai_list_uset(env, p, vk, vv));
            break;
        }
        case T_TABLE: {
            GTable* p = v_as_table(v);
            try (ai_table_uset(env, p, vk, vv));
            break;
        }
        default: {
            return ALO_EXIMPL;
        }
    }

    env->_stack._top -= 2;
    return ALO_SOK;
}

void alo_put(a_henv env, a_ilen id) {
	Value v1 = api_elem(env, id);
	api_check(v_is_list(v1));
	Value v2 = api_pre_decr_stack(env);
	ai_list_push(env, v_as_list(v1), v2);
	api_post_decr_stack(env);
	ai_gc_trigger(env);
}

static void l_call(a_henv env, a_ulen narg, a_ilen nres) {
	Value* base = env->_stack._top - narg - 1;

	ai_vm_call(env, base, nres);
}

void alo_call(a_henv env, a_ulen narg, a_ilen nres) {
	api_check(nres < 256, "too much result expected.");
	api_check_elem(env, narg + 1);
	if (nres > 0) api_check_slot(env, nres);
	l_call(env, narg, cast(a_i32, nres));
}

typedef struct {
	a_u32 _narg;
	a_i32 _nres;
} PCallCtx;

static void l_pcall(a_henv env, void* rctx) {
	PCallCtx* ctx = cast(PCallCtx*, rctx);
	l_call(env, ctx->_narg, ctx->_nres);
}

a_msg alo_pcall(a_henv env, a_ulen narg, a_ilen nres, a_ilen id_errf) {
    api_check(env->_status == ALO_SOK, "cannot call on a non-normal route");
	api_check(nres < 256, "result count overflow");
    api_check_elem(env, narg + 1);

    PCallCtx ctx = {
		._narg = narg,
		._nres = cast(a_i32, nres)
	};

    Frame* frame = env->_frame;
    Value* v_errf = id_errf != ALO_STACK_INDEX_EMPTY ? api_stack(env, id_errf) : null;

    StkPtr p_bot = val2stk(env, env->_stack._top - (narg + 1));

    catch (ai_env_pcall(env, l_pcall, &ctx, v_errf), msg) {
        env->_frame = frame; /* Recover frame. */

        Value* bot = stk2val(env, p_bot);
        env->_stack._top = bot;
        ai_cap_close_above(env, bot);
        ai_env_pop_error(env, api_incr_stack(env));
        return msg;
    }

	return ALO_SOK;
}

void alo_raise(a_henv env) {
	ai_err_raise(env, ALO_ERAISE, api_decr_stack(env));
}

a_msg alo_resume(a_henv env) {
	api_check(env->_status == ALO_SYIELD, "cannot resume the route.");
	return ai_env_resume(G(env)->_active, env);
}

void alo_yield(a_henv env) {
	api_check(env->_status == ALO_SOK, "cannot yield the route.");
	ai_env_yield(env);
}

a_bool alo_fattrz(a_henv env, a_enum n) {
	quiet(env);
	switch (n) {
		case ALO_FATTR_YIELD:
			return true;
        case ALO_FATTR_ASYNC:
            return false;
		default:
			return false;
	}
}

a_msg alo_tagof(a_henv env, a_ilen id) {
	Value const* slot = api_roslot(env, id);
	return slot != null ? api_tagof(env, *slot) : ALO_EEMPTY;
}

a_bool alo_tobool(a_henv env, a_ilen id) {
	Value v = api_elem(env, id);
	return v_to_bool(v);
}

a_int alo_toint(a_henv env, a_ilen id) {
	Value v = api_elem(env, id);
    if (likely(v_is_int(v))) {
        return v_as_int(v);
    }
    else if (v_is_num(v)) {
        return cast(a_int, v_as_float(v));
    }
    else {
        api_panic("cannot convert to integer");
    }
}

a_float alo_tofloat(a_henv env, a_ilen id) {
	Value v = api_elem(env, id);
    if (likely(v_is_num(v))) {
        return v_as_num(v);
    }
    else {
        api_panic("cannot convert to float");
    }
}

/**
 ** Convert value into string value.
 ** The slot will be replaced by converted string value.
 **
 * @param env the runtime environment.
 * @param id the index of slot to convert.
 * @param plen the optional pointer to get length of string.
 * @return the pointer of string.
 */
char const* alo_tolstr(a_henv env, a_ilen id, a_usize* plen) {
	Value v = api_elem(env, id);
	api_check(v_is_str(v), "cannot convert to string");
	GStr* p = v_as_str(v);
	if (plen != null) {
		*plen = p->_len;
	}
	return str2ntstr(p);
}

void* alo_toptr(a_henv env, a_ilen id) {
    Value v = api_elem(env, id);
    switch (v_get_tag(v)) {
        case T_NIL: {
            return null;
        }
        case T_PTR: {
            return v_as_ptr(v);
        }
        case T_MOD: {
            GMod* mod = v_as_mod(v);
            return mod->_extra;
        }
        default: {
            api_panic("cannot cast to pointer");
            return null;
        }
    }
}

a_henv alo_toroute(a_henv env, a_ilen id) {
    Value v = api_elem(env, id);
    api_check(v_is_route(v), "not route");
    return v_as_route(v);
}

void alo_typeof(a_henv env, a_ilen id) {
    api_check_slot(env, 1);
    Value v = api_elem(env, id);
    v_set_obj(env, api_incr_stack(env), v_typeof(env, v));
}

void alo_newtype(a_henv env, char const* n, a_flags flags) {
	api_check_slot(env, 1);

    Value* pv = api_incr_stack(env);

    GStr* name = ai_str_from_ntstr(env, n);
    v_set_str(env, pv, name);

	GType* self = ai_type_new(env, name);
	v_set_obj(env, pv, self);

	ai_gc_trigger(env);
}

void alo_fullgc(a_henv env) {
	ai_gc_full_gc(env, false);
}

static GStr* l_get_str(a_henv env, a_ilen id) {
	Value const* v = api_roslot(env, id);
	return v != null ? v_as_str(*v) : null;
}

a_msg alo_compile(a_henv env, a_ifun fun, void* ctx,
                  a_ilen id_env, a_ilen id_name, a_ilen id_file,
                  a_flags options) {
	GFun* out;
	api_check_slot(env, 1);
	id_env = alo_absindex(env, id_env);

	GStr* file = l_get_str(env, id_file);
	GStr* name = l_get_str(env, id_name);

	a_msg msg = ai_parse(env, fun, ctx, file, name, options, &out);
	if (likely(msg == ALO_SOK)) {
		v_set_obj(env, api_incr_stack(env), out);
		if (out->_len > 0) {
            RcCap* cap = ai_cap_new(env);
            out->_caps[0] = cap;
            v_cpy(env, cap->_ptr, api_rdslot(env, id_env));
		}
	}
	else {
		ai_env_pop_error(env, api_incr_stack(env));
	}
	return msg;
}

char const ai_api_tagname[][8] = {
	[ALO_TNIL] = "nil",
	[ALO_TBOOL] = "bool",
	[ALO_TINT] = "int",
	[ALO_TPTR] = "ptr",
	[ALO_TFLOAT] = "float",
	[ALO_TSTR] = "str",
	[ALO_TTUPLE] = "tuple",
	[ALO_TLIST] = "list",
	[ALO_TTABLE] = "table",
	[ALO_TFUNC] = "func",
	[ALO_TTYPE] = "type",
	[ALO_TUSER] = "user"
};
