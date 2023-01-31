/*
 * aapi.c
 */

#define aapi_c_

#include "astr.h"
#include "atuple.h"
#include "alist.h"
#include "atable.h"
#include "afun.h"
#include "actx.h"
#include "agc.h"
#include "avm.h"
#include "aparse.h"
#include "adump.h"

#include "aapi.h"

/**
 ** Initialize library context, the function should be called before use library.
 ** This function returns initialization result message.
 ** Any other function of the library should called after library is initialized.
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
a_bool alo_attri(unused a_henv env, a_enum n, a_int* pi) {
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
 ** Set hook for VM. This function is thread safe.
 *@param env the runtime environment.
 *@param kf the hook function.
 *@param kc the hook context.
 *@param mask the hook mask.
 */
void alo_sethook(a_henv env, a_kfun kf, a_kctx kc, a_flags mask) {
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
	return env->_stack._top - env->_stack._bot;
}

a_bool alo_ensure(a_henv env, size_t n) {
	Value* require = env->_stack._bot + n;
	if (require > env->_stack._top) {
		a_isize diff = ai_env_grow_stack(env, require);
		if (diff & (GROW_STACK_FLAG_OF1 | GROW_STACK_FLAG_OF2)) return false;
#if ALO_STRICT_STACK_CHECK
		env->_frame->_bound += diff;
#endif
	}
	return true;
}

void alo_settop(a_henv env, ptrdiff_t n) {
	Value* v = n >= 0 ? env->_stack._bot + n : env->_stack._top + n;
	api_check(v >= env->_stack._bot && v <= api_stack_limit(env));
	Value* u = env->_stack._top;
	while (u < v) { /* Fill nil value if stack grows. */
		v_setx(u++, v_of_nil());
	}
	env->_stack._top = v;
}

ptrdiff_t alo_absindex(a_henv env, ptrdiff_t id) {
	if (id < 0) {
		Value* v = env->_stack._top + id;
		api_check(v >= env->_stack._bot);
		id = v - env->_stack._bot;
	}
	return id;
}

Value const* api_roslot(a_henv env, a_isize id) {
	Value const* v;
	if (id >= 0) {
		v = env->_stack._bot + id;
		if (v >= env->_stack._top) 
			return null;
	}
	else {
		v = env->_stack._top + id;
		if (v < env->_stack._bot) 
			return null;
	}
	return v;
}

Value const* api_rdslot(a_henv env, a_isize id) {
	static Value const v_nil = v_of_nil();
	return api_roslot(env, id) ?: &v_nil;
}

Value* api_wrslot(a_henv env, a_isize id) {
	Value* v;
	if (id >= 0) {
		v = env->_stack._bot + id;
		api_check(v < env->_stack._top);
	}
	else {
		v = env->_stack._top + id;
		api_check(v >= env->_stack._bot);
	}
	return v;
}

static int tag_of(Value const* v) {
	switch (v_raw_tag(v)) {
		case T_NIL: 
			return ALO_TNIL;
		case T_FALSE:
		case T_TRUE: 
			return ALO_TBOOL;
		case T_INT:
			return ALO_TINT;
		case T_PTR:
			return ALO_TPTR;
		case T_ISTR:
		case T_HSTR:
			return ALO_TSTR;
		case T_TUPLE:
			return ALO_TTUPLE;
		case T_ROUTE:
			return ALO_TROUTE;
		default:
			return ALO_TFLOAT;
	}
}

void alo_push(a_henv env, ptrdiff_t id) {
	Value const* slot = api_rdslot(env, id);
	v_cpy(G(env), api_incr_stack(env), slot);
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
int alo_pushex(a_henv env, char const* sp, ...) {
	va_list varg;
	va_start(varg, sp);
	int tag = alo_pushvex(env, sp, varg);
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
int alo_pushvex(a_henv env, char const* sp, va_list varg) {
	Value const* v;
	switch (*(sp++)) {
		case '\0': { /* Empty constant. */
			return ALO_TEMPTY;
		}
		case 'i': { /* Index addressing. */
			v = api_roslot(env, va_arg(varg, ptrdiff_t));
			if (v == null) return ALO_TEMPTY;
			break;
		}
		case 'b': { /* Frame base addressing. */
			a_usize index = va_arg(varg, a_usize);
			v = env->_stack._bot + index;
			if (v >= env->_stack._top) return ALO_TEMPTY;
			break;
		}
		default: 
			api_panic("bad slot path");
	}
	loop {
		switch (*(sp++)) {
			case '\0': { /* Terminate character. */
				v_cpy(G(env), api_incr_stack(env), v);
				return tag_of(v);
			}
			case 'i': { /* Integer index. */
				a_isize index = va_arg(varg, a_isize);
				switch (v_raw_tag(v)) {
					case T_TUPLE: {
						GTuple* value = v_as_tuple(G(env), v);
						v = ai_tuple_geti(env, value, index);
						break;
					}
					case T_LIST: {
						GList* value = v_as_list(G(env), v);
						v = ai_list_geti(env, value, index);
						break;
					}
					default: {
						v = null;
						break;
					}
				}
				if (v == null) return ALO_TEMPTY;
				break;
			}
			default:
				api_panic("bad slot path");
		}
	}
}

void alo_pushnil(a_henv env) {
	v_setx(api_incr_stack(env), v_of_nil());
}

void alo_pushbool(a_henv env, a_bool val) {
	v_setx(api_incr_stack(env), v_of_bool(val));
}

void alo_pushint(a_henv env, a_int val) {
	v_setx(api_incr_stack(env), v_of_int(val));
}

void alo_pushfloat(a_henv env, a_float val) {
	v_setx(api_incr_stack(env), v_of_float(val));
}

void alo_pushptr(a_henv env, void* val) {
	v_setx(api_incr_stack(env), v_of_ptr(val));
}

char const* alo_pushstr(a_henv env, void const* src, size_t len) {
	GStr* val = ai_str_create(env, src, len);
	v_setx(api_incr_stack(env), v_of_ref(val));
	ai_gc_trigger(env);
	return cast(char const*, val->_data);
}

char const* alo_pushfstr(a_henv env, char const* fmt, ...) {
	va_list varg;
	va_start(varg, fmt);
	char const* res = alo_pushvfstr(env, fmt, varg);
	va_end(varg);
	return res;
}

char const* alo_pushvfstr(a_henv env, char const* fmt, va_list varg) {
	GStr* val = ai_str_formatv(env, fmt, varg);
	v_setx(api_incr_stack(env), v_of_ref(val));
	ai_gc_trigger(env);
	return cast(char const*, val->_data);
}

void alo_pop(a_henv env, ptrdiff_t id) {
	Value* d = api_wrslot(env, id);
	Value const* s = api_decr_stack(env);
	assume(s != d);
	v_cpy(G(env), d, s);
}

void alo_newtuple(a_henv env, size_t n) {
	api_check_elem(env, n);
	GTuple* val = ai_tuple_new(env, env->_stack._top - n, n);
	env->_stack._top -= n;
	v_set(G(env), api_incr_stack(env), v_of_ref(val));
	ai_gc_trigger(env);
}

void alo_newlist(a_henv env, size_t n) {
	GList* val = ai_list_new(env);
	v_set(G(env), api_incr_stack(env), v_of_ref(val));
	ai_gc_trigger(env);
	ai_list_hint(env, val, n);
}

void alo_newtable(a_henv env, size_t n) {
	GTable* val = ai_table_new(env);
	v_set(G(env), api_incr_stack(env), v_of_ref(val));
	ai_gc_trigger(env);
	ai_table_hint(env, val, n);
}

void alo_newcfun(a_henv env, a_cfun f, size_t n) {
	api_check_slot(env, 1);
	api_check_elem(env, n);
	GFun* val = ai_cfun_create(env, f, n, env->_stack._top - n);
	env->_stack._top -= n;
	v_set(G(env), api_incr_stack(env), v_of_ref(val));
	ai_gc_trigger(env);
}

size_t alo_len(a_henv env, ptrdiff_t id) {
	Value const* v = api_rdslot(env, id);
	switch (v_raw_tag(v)) {
		case T_TUPLE: {
			GTuple* value = v_as_tuple(G(env), v);
			return cast(a_usize, value->_len);
		}
		case T_LIST: {
			GList* value = v_as_list(G(env), v);
			return cast(a_usize, value->_len);
		}
		case T_TABLE: {
			GTable* value = v_as_table(G(env), v);
			return cast(a_usize, value->_len);
		}
		default:
			api_panic("unsupported operation.");
	}
}

int alo_geti(a_henv env, ptrdiff_t id, a_int key) {
	Value const* v = api_rdslot(env, id);
	api_check_elem(env, 1);
	switch (v_raw_tag(v)) {
		case T_TUPLE: {
			GTuple* value = v_as_tuple(G(env), v);
			v = ai_tuple_geti(env, value, key);
			break;
		}
		case T_LIST: {
			GList* value = v_as_list(G(env), v);
			v = ai_list_geti(env, value, key);
			break;
		}
		case T_TABLE: {
			GTable* value = v_as_table(G(env), v);
			v = ai_table_geti(env, value, key);
			break;
		}
		default:
			api_panic("unsupported operation.");
	}
	if (v != null) v_cpy(G(env), api_incr_stack(env), v);
	return v != null ? tag_of(v) : ALO_TEMPTY;
}

void alo_call(a_henv env, size_t narg, ptrdiff_t nres) {
	api_check(nres < 256, "bad result count.");
	api_check_elem(env, narg + 1);
	if (nres > 0) api_check_slot(env, nres);
	ai_vm_call(env, env->_stack._top - narg - 1, new(RFlags) {
		._count = nres < 0 ? RFLAG_COUNT_VARARG : nres
	});
}

int alo_tagof(a_henv env, ptrdiff_t id) {
	Value const* slot = api_roslot(env, id);
	return slot != null ? tag_of(slot) : ALO_TEMPTY;
}

a_bool alo_tobool(a_henv env, ptrdiff_t id) {
	Value const* v = api_rdslot(env, id);
	return v_to_bool(v);
}

a_int alo_toint(a_henv env, ptrdiff_t id) {
	Value const* v = api_rdslot(env, id);
	switch (expect(v_raw_tag(v), T_INT)) {
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

a_float alo_tofloat(a_henv env, ptrdiff_t id) {
	Value const* v = api_rdslot(env, id);
	switch (expect(v_raw_tag(v), T_FLOAT)) {
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

char const* alo_tostr(a_henv env, ptrdiff_t id, size_t* plen) {
	Value const* v = api_rdslot(env, id);
	if (likely(v_is_str(v))) {
		GStr* value = v_as_str(G(env), v);
		if (plen != null) {
			*plen = cast(a_usize, value->_len);
		}
		return cast(char const*, value->_data);
	}
	api_panic("cannot cast to string");
}

void alo_fullgc(a_henv env) {
	ai_gc_full_gc(env, false);
}

a_msg alo_compile(a_henv env, a_ifun fun, void* ctx, char const* name, unsigned int options) {
	GFun* out;
	api_check_slot(env, 1);
	a_msg msg = ai_parse(env, fun, ctx, name, options, &out);
	if (likely(msg == ALO_SOK)) {
		v_set(G(env), api_incr_stack(env), v_of_ref(out));
	}
	else {
		v_set(G(env), api_incr_stack(env), env->_error);
		v_setx(&env->_error, v_of_nil());
	}
	return msg;
}

void alo_dump(a_henv env, ptrdiff_t id, unsigned int options) {
	Value const* v = api_rdslot(env, id);
	if (likely(v_is_func(v))) {
		GFun* fun = v_as_func(G(env), v);
		ai_dump_print(env, downcast(GFunMeta, fun->_meta), options);
		return;
	}
	api_panic("unable to dump.");
}