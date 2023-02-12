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
#include "actx.h"
#include "agc.h"
#include "aerr.h"
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
	return env->_stack._top - env->_stack._bot;
}

a_bool alo_ensure(a_henv env, a_usize n) {
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

void alo_settop(a_henv env, a_isize n) {
	Value* v = n >= 0 ? env->_stack._bot + n : env->_stack._top + n;
	api_check(v >= env->_stack._bot && v <= api_stack_limit(env));
	Value* u = env->_stack._top;
	while (u < v) { /* Fill nil value if stack grows. */
		v_setx(u++, v_of_nil());
	}
	env->_stack._top = v;
}

#define MIN_NEG_STACK_INDEX (-255)

a_isize alo_absindex(a_henv env, a_isize id) {
	if (id < 0 && id >= MIN_NEG_STACK_INDEX) {
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
	else if (id >= MIN_NEG_STACK_INDEX) {
		v = env->_stack._top + id;
		if (v < env->_stack._bot) 
			return null;
	}
	else if (id == ALO_STACK_INDEX_GLOBAL) {
		v = &G(env)->_global;
	}
	else if (id == ALO_STACK_INDEX_ERROR) {
		v = &env->_error;
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

Value* api_wrslot(a_henv env, a_isize id) {
	Value* v;
	if (id >= 0) {
		v = env->_stack._bot + id;
		api_check(v < env->_stack._top);
	}
	else if (id >= MIN_NEG_STACK_INDEX) {
		v = env->_stack._top + id;
		api_check(v >= env->_stack._bot);
	}
	else if (id == ALO_STACK_INDEX_ERROR) {
		v = &env->_error;
	}
	else {
		v = null;
		api_panic("bad stack index.");
	}
	return v;
}

static a_tag tag_of(a_henv env, Value const* v) {
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
		case T_LIST:
			return ALO_TLIST;
		case T_TABLE:
			return ALO_TTABLE;
		case T_MOD:
			return ALO_TMOD;
		case T_OTHER:
			return ALO_TOTHER;
		default:
			if (v_is_route(G(env), v))
				return ALO_TROUTE;
			return ALO_TFLOAT;
	}
}

void alo_push(a_henv env, a_isize id) {
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
	Value const* v;
	switch (*(sp++)) {
		case '\0': { /* Empty constant. */
			return ALO_TEMPTY;
		}
		case 'i': { /* Index addressing. */
			v = api_roslot(env, va_arg(varg, a_isize));
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
				return tag_of(env, v);
			}
			case 'i': { /* Integer index. */
				a_isize index = va_arg(varg, a_isize);
				switch (v_raw_tag(v)) {
					case T_TUPLE: {
						GTuple* value = v_as_tuple(G(env), v);
						v = ai_tuple_refi(env, value, index);
						break;
					}
					case T_LIST: {
						GList* value = v_as_list(G(env), v);
						v = ai_list_refi(env, value, index);
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

char const* alo_pushstr(a_henv env, void const* src, a_usize len) {
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

void alo_pop(a_henv env, a_isize id) {
	Value* d = api_wrslot(env, id);
	api_check(env->_stack._top - 1 != d, "bad pop index.");
	Value s = api_decr_stack(env);
	v_set(G(env), d, s);
}

void alo_newtuple(a_henv env, a_usize n) {
	api_check_elem(env, n);
	GTuple* val = ai_tuple_new(env, env->_stack._top - n, n);
	env->_stack._top -= n;
	v_set(G(env), api_incr_stack(env), v_of_ref(val));
	ai_gc_trigger(env);
}

void alo_newlist(a_henv env, a_usize n) {
	GList* val = ai_list_new(env);
	v_set(G(env), api_incr_stack(env), v_of_ref(val));
	ai_gc_trigger(env);
	ai_list_hint(env, val, n);
}

void alo_newtable(a_henv env, a_usize n) {
	GTable* val = ai_table_new(env);
	v_set(G(env), api_incr_stack(env), v_of_ref(val));
	ai_gc_trigger(env);
	ai_table_hint(env, val, n);
}

void alo_newcfun(a_henv env, a_cfun f, a_usize n) {
	api_check_slot(env, 1);
	api_check_elem(env, n);
	GFun* val = ai_cfun_create(env, f, n, env->_stack._top - n);
	env->_stack._top -= n;
	v_set(G(env), api_incr_stack(env), v_of_ref(val));
	ai_gc_trigger(env);
}

a_usize alo_len(a_henv env, a_isize id) {
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

a_tag alo_geti(a_henv env, a_isize id, a_int key) {
	Value const* v = api_rdslot(env, id);
	api_check_elem(env, 1);
	switch (v_raw_tag(v)) {
		case T_TUPLE: {
			GTuple* value = v_as_tuple(G(env), v);
			v = ai_tuple_refi(env, value, key);
			break;
		}
		case T_LIST: {
			GList* value = v_as_list(G(env), v);
			v = ai_list_refi(env, value, key);
			break;
		}
		case T_TABLE: {
			GTable* value = v_as_table(G(env), v);
			v = ai_table_refi(env, value, key);
			break;
		}
		default:
			api_panic("unsupported operation.");
	}
	if (v != null) v_cpy(G(env), api_incr_stack(env), v);
	return v != null ? tag_of(env, v) : ALO_TEMPTY;
}

void alo_insert(a_henv env, a_isize id) {
	Value const* v1 = api_rdslot(env, id);
	api_check(v_is_list(v1));
	Value v2 = api_pre_decr_stack(env);
	ai_list_insert(env, v_as_list(G(env), v1), v2);
	api_post_decr_stack(env);
	ai_gc_trigger(env);
}

static void l_call(a_henv env, a_u32 narg, a_i32 nres) {
	ai_vm_call(env, env->_stack._top - narg - 1, new(RFlags) {
		._count = nres < 0 ? RFLAG_COUNT_VARARG : nres
	});
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
	a_isize stack_diff = ptr_diff(env->_stack._bot, env->_stack._base);
	a_msg msg = ai_env_protect(env, l_pcall, &ctx);
	if (unlikely(msg != ALO_SOK)) {
		env->_frame = frame; /* Recover frame. */
		env->_stack._bot = ptr_disp(Value, env->_stack._base, stack_diff);
		env->_stack._top = env->_stack._bot + nsav;
		ai_env_pop_error(env, api_incr_stack(env));
	}
	return msg;
}

void alo_raise(a_henv env) {
	ai_err_raise(env, ALO_EOUTER, api_decr_stack(env));
}

a_tag alo_tagof(a_henv env, a_isize id) {
	Value const* slot = api_roslot(env, id);
	return slot != null ? tag_of(env, slot) : ALO_TEMPTY;
}

a_bool alo_tobool(a_henv env, a_isize id) {
	Value const* v = api_rdslot(env, id);
	return v_to_bool(v);
}

a_int alo_toint(a_henv env, a_isize id) {
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

a_float alo_tofloat(a_henv env, a_isize id) {
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

char const* alo_tostr(a_henv env, a_isize id, a_usize* plen) {
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

a_hmod alo_openmod(a_henv env, a_isize id) {
	Value const* v = api_rdslot(env, id);
	if (likely(v_is_mod(v))) {
		GMod* mod = v_as_mod(G(env), v);
		if (unlikely(mod->_rc == UINT32_MAX))
			return null;
		mod->_rc += 1;
		return mod;
	}
	return null;
}

void alo_closemod(unused a_henv env, a_hmod mod) {
	api_check(mod->_rc > 0, "bad module reference count.");
	mod->_rc -= 1;
}

void alo_fullgc(a_henv env) {
	ai_gc_full_gc(env, false);
}

static GStr* l_get_str(a_henv env, a_isize id) {
	Value const* v = api_roslot(env, id);
	return v != null ? v_as_str(G(env), v) : null;
}

a_msg alo_compile(a_henv env, a_ifun fun, void* ctx,
				  a_isize id_env, a_isize id_name, a_isize id_file,
				  a_flags options) {
	GFun* out;
	api_check_slot(env, 1);
	id_env = alo_absindex(env, id_env);
	a_msg msg = ai_parse(env, fun, ctx, l_get_str(env, id_file), l_get_str(env, id_name), options, &out);
	if (likely(msg == ALO_SOK)) {
		v_set(G(env), api_incr_stack(env), v_of_ref(out));
		if (g_cast(GFunMeta, out->_meta)->_ncap > 0) {
			Value* vcap = &out->_capval[0];
			if (!(options & ALO_COMP_OPT_CONST_ENV))
				vcap = v_as_cap(G(env), vcap)->_ptr;
			v_cpy(G(env), vcap, api_rdslot(env, id_env));
		}
	}
	else {
		ai_env_pop_error(env, api_incr_stack(env));
	}
	return msg;
}

void alo_dump(a_henv env, a_isize id, a_flags options) {
	Value const* v = api_rdslot(env, id);
	if (likely(v_is_func(v))) {
		GFun* fun = v_as_func(G(env), v);
		ai_dump_print(env, g_cast(GFunMeta, fun->_meta), options);
		return;
	}
	api_panic("unable to dump.");
}