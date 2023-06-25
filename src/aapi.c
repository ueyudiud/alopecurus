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
#include "auser.h"
#include "atype.h"
#include "actx.h"
#include "agc.h"
#include "aerr.h"
#include "avm.h"
#include "aparse.h"

#include "aapi.h"

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
			*pi = 1;
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
	Global* g = G(env);
	g->_panic = f;
}

/**
 ** Set hook for VM. This function is thread safe.
 *@param env the runtime environment.
 *@param kf the hook function.
 *@param kc the hook context.
 *@param mask the hook mask.
 */
void alo_sethook(a_henv env, a_hfun kf, a_hctx kc, a_flags mask) {
	Global* g = G(env);

	/* Make sure null safe. */
	if (kf == null || mask == 0) {
		kf = null;
		mask = 0;
	}

	ai_vm_lock_hook(g);

	g->_hookf = kf;
	g->_hookc = kc;

	g->_hookm = mask;
}

/**
 ** Get VM stack size.
 *@param env the runtime environment.
 *@return the stack size.
 */
a_usize alo_stacksize(a_henv env) {
	return env->_stack._top - ai_stk_bot(env);
}

/**
 ** Reserve enough slots of stack to carry values.
 *@param env the runtime environment.
 *@param n the number of reserved slots.
 *@return true if reserved success and false for otherwise.
 */
a_bool alo_ensure(a_henv env, a_usize n) {
	Value* const expect = env->_stack._top + n;
	Value* const limit = env->_stack._limit;
	if (expect > limit) {
		a_isize diff = ai_stk_grow(env, expect);

		if (diff & STACK_GROW_FAILED) {
			return false;
		}

#if ALO_STRICT_STACK_CHECK
		env->_frame->_bound = ptr_disp(Value, expect, diff);
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
void alo_settop(a_henv env, a_isize n) {
	Value* old_top = env->_stack._top;
	Value* new_top = n >= 0 ? ai_stk_bot(env) + n : old_top + n;

	api_check(new_top >= ai_stk_bot(env) && new_top <= api_stack_limit(env));

	v_set_nil_ranged(old_top, new_top);
	env->_stack._top = new_top;
}

#define MIN_NEG_STACK_INDEX (-255)

a_isize alo_absindex(a_henv env, a_isize id) {
	if (id < 0 && id >= MIN_NEG_STACK_INDEX) {
		Value* v = env->_stack._top + id;
		api_check(v >= ai_stk_bot(env));
		id = v - ai_stk_bot(env);
	}
	return id;
}

Value const* api_roslot(a_henv env, a_isize id) {
	Value const* v;
	if (id >= 0) {
		v = ai_stk_bot(env) + id;
		if (v >= env->_stack._top) 
			return null;
	}
	else if (id >= MIN_NEG_STACK_INDEX) {
		v = env->_stack._top + id;
		if (v < ai_stk_bot(env))
			return null;
	}
	else if (id == ALO_STACK_INDEX_ERROR) {
		v = &env->_error;
	}
	else if (id >= ALO_STACK_INDEX_CAPTURE_BASE) {
		id -= ALO_STACK_INDEX_CAPTURE_BASE;
		GFun* fun = v_as_func(*stk2val(env, env->_frame->_stack_bot - 1));
		if (id >= fun->_len)
			return null;
		v = unlikely(fun->_flags & FUN_FLAG_NATIVE) ? &fun->_vals[id] : fun->_caps[id]->_ptr;
	}
	else if (id == ALO_STACK_INDEX_GLOBAL) {
		v = &G(env)->_global;
	}
	else {
		return null;
	}
	return v;
}

Value const* api_rdslot(a_henv env, a_isize id) {
	static Value const v_nil = v_of_nil();
	return api_roslot(env, id) ?: &v_nil;
}

Value api_elem(a_henv env, a_isize id) {
	return *api_rdslot(env, id);
}

Value* api_wrslot(a_henv env, a_isize id) {
	Value* v;
	if (id >= 0) {
		v = ai_stk_bot(env) + id;
		api_check(v < env->_stack._top);
	}
	else if (id >= MIN_NEG_STACK_INDEX) {
		v = env->_stack._top + id;
		api_check(v >= ai_stk_bot(env));
	}
	else if (id == ALO_STACK_INDEX_ERROR) {
		v = &env->_error;
	}
	else if (id >= ALO_STACK_INDEX_CAPTURE_BASE) {
		id -= ALO_STACK_INDEX_CAPTURE_BASE;
		GFun* fun = v_as_func(*stk2val(env, env->_frame->_stack_bot - 1));
		api_check(id < fun->_len);
		v = unlikely(fun->_flags & FUN_FLAG_NATIVE) ? &fun->_vals[id] : fun->_caps[id]->_ptr;
	}
	else {
		v = null;
		api_panic("bad stack index.");
	}
	return v;
}

static a_tag tag_of(a_henv env, Value v) {
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
		case T__MIN_OBJ ... T__MAX_OBJ:
			return v_typeof(env, v)->_tag;
		default:
			return ALO_TFLOAT;
	}
}

void alo_push(a_henv env, a_isize id) {
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
a_tag alo_pushex(a_henv env, char const* sp, ...) {
	va_list varg;
	va_start(varg, sp);
	a_enum tag = alo_pushvex(env, sp, varg);
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
a_tag alo_pushvex(a_henv env, char const* sp, va_list varg) {
	Value v;
	switch (*(sp++)) {
		case '\0': { /* Empty constant. */
			return ALO_TEMPTY;
		}
		case 'i': { /* Index addressing. */
			Value const* p = api_roslot(env, va_arg(varg, a_isize));
			if (p == null) return ALO_TEMPTY;
			v = *p;
			break;
		}
		case 'b': { /* Frame base addressing. */
			a_usize index = va_arg(varg, a_usize);
			Value const* p = ai_stk_bot(env) + index;
			if (p >= env->_stack._top) return ALO_TEMPTY;
			v = *p;
			break;
		}
		default: 
			api_panic("bad slot path");
	}
	loop {
		switch (*(sp++)) {
			case '\0': { /* Terminate character. */
				v_set(env, api_incr_stack(env), v);
				return tag_of(env, v);
			}
			case 'i': { /* Integer index. */
				a_isize index = va_arg(varg, a_isize);
				Value const* p;
				switch (v_get_tag(v)) {
					case T_TUPLE: {
						GTuple* value = v_as_tuple(v);
						p = ai_tuple_refi(env, value, index);
						break;
					}
					case T_LIST: {
						GList* value = v_as_list(v);
						p = ai_list_refi(env, value, index);
						break;
					}
					default: {
						p = null;
						break;
					}
				}
				if (p == null) return ALO_TEMPTY;
				v = *p;
				break;
			}
			default: {
				api_panic("bad slot path");
			}
		}
	}
}

void alo_pushnil(a_henv env) {
	v_set_nil(api_incr_stack(env));
}

void alo_pushbool(a_henv env, a_bool val) {
	v_set_bool(api_incr_stack(env), val);
}

void alo_pushint(a_henv env, a_int val) {
	v_set_int(api_incr_stack(env), val);
}

void alo_pushfloat(a_henv env, a_float val) {
	v_set_float(api_incr_stack(env), val);
}

void alo_pushptr(a_henv env, void* val) {
	v_set_ptr(api_incr_stack(env), val);
}

char const* alo_pushstr(a_henv env, void const* src, a_usize len) {
	GStr* val = ai_str_new(env, src, len);
	v_set_obj(env, api_incr_stack(env), val);
	ai_gc_trigger(env);
	return str2ntstr(val);
}

char const* alo_pushntstr(a_henv env, char const* src) {
	api_check(src != null, "string is null.");
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
	GStr* val = ai_str_format(env, fmt, varg);
	v_set_obj(env, api_incr_stack(env), val);
	ai_gc_trigger(env);
	return str2ntstr(val);
}

void alo_pushtype(a_henv env, a_htype hnd) {
	api_check(hnd->_nref > 0, "type not referenced by API.");
	v_set_obj(env, api_incr_stack(env), hnd);
}

void alo_pushroute(a_henv env) {
	v_set_obj(env, api_incr_stack(env), env);
}

void alo_xmove(a_henv src, a_henv dst, a_usize n) {
	api_check(src != dst, "same environment.");
	api_check_elem(src, n);
	api_check_slot(dst, n);
	v_cpy_all(src, dst->_stack._top, src->_stack._top - n, n);
	src->_stack._top -= n;
	dst->_stack._top += n;
}

void alo_pop(a_henv env, a_isize id) {
	Value* d = api_wrslot(env, id);
	api_check(env->_stack._top - 1 != d, "bad pop index.");
	Value s = api_decr_stack(env);
	v_set(env, d, s);
}

void alo_newtuple(a_henv env, a_usize n) {
	api_check_elem(env, n);
	GTuple* val = ai_tuple_new(env, env->_stack._top - n, n);
	env->_stack._top -= n;
	v_set_obj(env, api_incr_stack(env), val);
	ai_gc_trigger(env);
}

void alo_newlist(a_henv env, a_usize n) {
	GList* val = ai_list_new(env);
	v_set_obj(env, api_incr_stack(env), val);
	ai_gc_trigger(env);
	ai_list_hint(env, val, n);
}

void alo_newtable(a_henv env, a_usize n) {
	GTable* val = ai_table_new(env);
	v_set_obj(env, api_incr_stack(env), val);
	ai_gc_trigger(env);
	ai_table_hint(env, val, n);
}

void alo_newcfun(a_henv env, a_cfun f, a_usize n) {
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

a_usize alo_rawlen(a_henv env, a_isize id) {
	Value v = api_elem(env, id);
	switch (v_get_tag(v)) {
		case T_TUPLE: {
			GTuple* value = v_as_tuple(v);
			return value->_len;
		}
		case T_LIST: {
			GList* value = v_as_list(v);
			return value->_len;
		}
		case T_TABLE: {
			GTable* value = v_as_table(v);
			return value->_len;
		}
		case T_TYPE: {
			GType* value = v_as_type(v);
			return value->_len;
		}
		case T_AUSER: {
			GAUser* value = v_as_auser(v);
			return value->_len;
		}
		default: {
			api_panic("unsupported operation.");
		}
	}
}

a_tag alo_rawgeti(a_henv env, a_isize id, a_int key) {
	api_check_elem(env, 1);

	Value v = api_elem(env, id);

	switch (v_get_tag(v)) {
		case T_TUPLE: {
			GTuple* value = v_as_tuple(v);
			Value const* pv = ai_tuple_refi(env, value, key);
			if (pv == null) return ALO_TEMPTY;
			v = *pv;
			break;
		}
		case T_LIST: {
			GList* value = v_as_list(v);
			Value const* pv = ai_list_refi(env, value, key);
			if (pv == null) return ALO_TEMPTY;
			v = *pv;
			break;
		}
		case T_TABLE: {
			GTable* value = v_as_table(v);
			Value const* pv = ai_table_refi(env, value, key);
			if (pv == null) return ALO_TEMPTY;
			v = *pv;
			break;
		}
		case T_AUSER: {
			return ALO_TEMPTY;
		}
		default: {
			api_panic("bad value for 'geti' operation.");
		}
	}

	v_set(env, api_incr_stack(env), v);
	return tag_of(env, v);
}

a_tag alo_rawget(a_henv env, a_isize id) {
	api_check_elem(env, 1);

	Value v = api_elem(env, id);
	Value vk = api_decr_stack(env);

	switch (v_get_tag(v)) {
		case T_TUPLE: {
			GTuple* value = v_as_tuple(v);
			v = ai_tuple_get(env, value, vk);
			break;
		}
		case T_LIST: {
			GList* value = v_as_list(v);
			v = ai_list_get(env, value, vk);
			break;
		}
		case T_TABLE: {
			GTable* value = v_as_table(v);
			v = ai_table_get(env, value, vk);
			break;
		}
		case T_AUSER: {
			GAUser* value = v_as_auser(v);
			v = ai_auser_get(env, value, vk);
			break;
		}
		default: {
			api_panic("bad value for 'get' operation.");
		}
	}

	v_set(env, api_incr_stack(env), v);
	return tag_of(env, v);
}

void alo_insert(a_henv env, a_isize id) {
	Value v1 = api_elem(env, id);
	api_check(v_is_list(v1));
	Value v2 = api_pre_decr_stack(env);
	ai_list_push(env, v_as_list(v1), v2);
	api_post_decr_stack(env);
	ai_gc_trigger(env);
}

static void l_call(a_henv env, a_u32 narg, a_i32 nres) {
	Value* base = env->_stack._top - narg - 1;

	ai_vm_call(env, base, nres);
}

void alo_call(a_henv env, a_usize narg, a_isize nres) {
	api_check(nres < 256, "bad result count.");
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

a_msg alo_pcall(a_henv env, a_usize narg, a_isize nres, a_usize nsav) {
	api_check(nres < 256, "bad result count.");
	api_check_elem(env, max(nsav + 1, narg + 1));
	PCallCtx ctx = {
		._narg = narg,
		._nres = cast(a_i32, nres)
	};
	Frame* frame = env->_frame;
	a_msg msg = ai_env_pcall(env, l_pcall, &ctx);
	if (unlikely(msg != ALO_SOK)) {
		env->_frame = frame; /* Recover frame. */

		Value* bot = ai_stk_bot(env) + nsav;
		env->_stack._top = bot;
		ai_cap_close_above(env, bot);
		ai_env_pop_error(env, api_incr_stack(env));
	}
	return msg;
}

void alo_raise(a_henv env) {
	ai_err_raise(env, ALO_EOUTER, api_decr_stack(env));
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
	switch (n) {
		case ALO_FATTR_YIELD:
			return true;
		default:
			return false;
	}
}

a_tag alo_tagof(a_henv env, a_isize id) {
	Value const* slot = api_roslot(env, id);
	return slot != null ? tag_of(env, *slot) : ALO_TEMPTY;
}

a_bool alo_tobool(a_henv env, a_isize id) {
	Value v = api_elem(env, id);
	return v_to_bool(v);
}

a_int alo_toint(a_henv env, a_isize id) {
	Value v = api_elem(env, id);
	switch (expect(v_get_tag(v), T_INT)) {
		case T_NIL:
		case T_FALSE:
			return 0;
		case T_TRUE:
			return 1;
		case T_INT:
			return v_as_int(v);
		case T_FLOAT:
			return cast(a_int, v_as_float(v));
		default:
			api_panic("cannot cast to int");
	}
}

a_float alo_tofloat(a_henv env, a_isize id) {
	Value v = api_elem(env, id);
	switch (expect(v_get_tag(v), T_FLOAT)) {
		case T_NIL:
		case T_FALSE:
			return 0;
		case T_TRUE:
			return 1;
		case T_INT:
			return cast(a_float, v_as_int(v));
		case T_FLOAT:
			return v_as_float(v);
		default:
			api_panic("cannot cast to float");
	}
}

char const* alo_tolstr(a_henv env, a_isize id, a_usize* plen) {
	Value v = api_elem(env, id);
	api_check(v_is_str(v), "cannot cast to string");
	GStr* val = v_as_str(v);
	if (plen != null) {
		*plen = cast(a_usize, val->_len);
	}
	return str2ntstr(val);
}

static a_htype l_use_type(GType* type) {
	if (unlikely(type->_nref == UINT32_MAX))
		return null;
	type->_nref += 1;
	return type;
}

void alo_newtype(a_henv env, char const* name, a_flags options) {
	api_check_slot(env, 1);

	GType* type = options & ALO_NEWTYPE_FLAG_STATIC ? ai_type_alloc(env, 0, null) : ai_atype_new(env);
	v_set_obj(env, api_incr_stack(env), type);

	if (name != null) {
		type->_name = ai_str_new(env, name, strlen(name));
	}

	ai_gc_trigger(env);
}

a_htype alo_typeof(a_henv env, a_isize id) {
	Value const* pv = api_roslot(env, id);
	if (pv != null) {
		GType* type = v_typeof(env, *pv);
		return l_use_type(type);
	}
	return null;
}

char const* alo_typename(unused a_henv env, a_htype type) {
	return type != null ? str2ntstr(type->_name) : "empty";
}

a_htype alo_opentype(a_henv env, a_isize id) {
	Value v = api_elem(env, id);
	if (likely(v_is_type(v))) {
		GType* type = v_as_type(v);
		return l_use_type(type);
	}
	return null;
}

void alo_closetype(unused a_henv env, a_htype type) {
	if (type != null) {
		api_check(type->_nref > 0, "type not referenced by API.");
		type->_nref -= 1;
	}
}

void alo_fullgc(a_henv env) {
	ai_gc_full_gc(env, false);
}

static GStr* l_get_str(a_henv env, a_isize id) {
	Value const* v = api_roslot(env, id);
	return v != null ? v_as_str(*v) : null;
}

a_msg alo_compile(a_henv env, a_ifun fun, void* ctx,
				  a_isize id_env, a_isize id_name, a_isize id_file,
				  a_flags options) {
	GFun* out;
	api_check_slot(env, 1);
	id_env = alo_absindex(env, id_env);

	GStr* file = l_get_str(env, id_file);
	GStr* name = l_get_str(env, id_name);

	a_msg msg = ai_parse(env, fun, ctx, file, name, options, &out);
	if (likely(msg == ALO_SOK)) {
		v_set_obj(env, api_incr_stack(env), out);
		if (out->_proto->_ncap > 0) {
			v_cpy(env, out->_caps[0]->_ptr, api_rdslot(env, id_env));
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
