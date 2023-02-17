/**
 *@file abaselib.c
 */

#define abaselib_c_
#define ALO_LIB

#include <stdio.h>

#include "alink.h"
#include "atuple.h"
#include "alist.h"
#include "atable.h"
#include "agc.h"
#include "aapi.h"

#include "alolib.h"
#include "abaselib.h"

#define MAX_SHOW_LEN 16
#define MAX_SHOW_DEPTH 16

static void l_show_impl(Global* g, Value v, a_u32 depth) {
	switch (v_raw_tag(v)) {
		case T_NIL: {
			aloi_show("nil");
			break;
		}
		case T_FALSE: {
			aloi_show("false");
			break;
		}
		case T_TRUE: {
			aloi_show("true");
			break;
		}
		case T_INT: {
			aloi_show("%d", v_as_int(v));
			break;
		}
		case T_PTR: {
			aloi_show("%p", v_as_ptr(v));
			break;
		}
		case T_HSTR:
		case T_ISTR: {
			GStr* val = v_as_str(g, v);
			aloi_show("%s", ai_str_tocstr(val));
			break;
		}
		case T_TUPLE: {
			if (depth >= MAX_SHOW_DEPTH) {
				aloi_show("(...)");
			}
			else {
				GTuple* val = v_as_tuple(g, v);
				a_u32 n = min(val->_len, MAX_SHOW_LEN);
				if (val->_len > 0) {
					aloi_show("(");
					l_show_impl(g, val->_body[0], depth + 1);
					for (a_u32 i = 1; i < n; ++i) {
						aloi_show(", ");
						l_show_impl(g, val->_body[i], depth + 1);
					}
					if (val->_len > MAX_SHOW_LEN) {
						aloi_show(", ...");
					}
					aloi_show(")");
				}
				else {
					aloi_show("()");
				}
			}
			break;
		}
		case T_LIST: {
			if (depth >= MAX_SHOW_DEPTH) {
				aloi_show("[...]");
			}
			else {
				GList* val = v_as_list(g, v);
				a_u32 n = min(val->_len, MAX_SHOW_LEN);
				if (val->_len > 0) {
					aloi_show("[");
					l_show_impl(g, val->_data[0], depth + 1);
					for (a_u32 i = 1; i < n; ++i) {
						aloi_show(", ");
						l_show_impl(g, val->_data[i], depth + 1);
					}
					if (val->_len > MAX_SHOW_LEN) {
						aloi_show(", ...");
					}
					aloi_show("]");
				}
				else {
					aloi_show("[]");
				}
			}
			break;
		}
		case T_TABLE: {
			if (depth >= MAX_SHOW_DEPTH) {
				aloi_show("{...}");
			}
			else {
				GTable* val = v_as_table(g, v);
				a_u32 n = min(val->_len, MAX_SHOW_LEN);
				if (val->_len > 0) {
					aloi_show("{");
					TNode* itr = ai_link_first(val);
					l_show_impl(g, itr->_key, depth + 1);
					aloi_show(" -> ");
					l_show_impl(g, itr->_value, depth + 1);
					while (--n > 0) {
						assume(itr != null);
						itr = ai_link_next(itr);
						aloi_show(", ");
						l_show_impl(g, itr->_key, depth + 1);
						aloi_show(" -> ");
						l_show_impl(g, itr->_value, depth + 1);
					}
					if (val->_len > MAX_SHOW_LEN) {
						aloi_show(", ...");
					}
					aloi_show("}");
				}
				else {
					aloi_show("{}");
				}
			}
			break;
		}
		case T_FUNC: {
			aloi_show("<func:%p>", v_as_hnd(v));
			break;
		}
		case T_MOD: {
			aloi_show("<mod:%s>", ai_str_tocstr(v_as_mod(g, v)->_name));
			break;
		}
		case T_OTHER: {
			aloi_show("<%p>", v_as_hnd(v));
			break;
		}
		default: {
			aloi_show("%.6g", v_as_float(v));
			break;
		}
	}
}

/**
 ** Show value in standard console with the default format in the specific slot.
 *@param env the runtime environment.
 *@param id the slot id.
 */
void aloL_base_show(a_henv env, a_isize id) {
	Value const* v = api_roslot(env, id);
	api_check(v != null, "bad slot id.");
	l_show_impl(G(env), *v, 0);
}

static a_u32 base_print(a_henv env) {
	a_usize len = alo_stacksize(env);
	for (a_usize id = 0; id < len; ++id) {
		if (id != 0) aloi_show("\t");
		aloL_base_show(env, cast(a_isize, id));
	}
	aloi_show_newline();
	return 0;
}

static a_u32 base_assert(a_henv env) {
	a_u32 n = alo_stacksize(env);
	if (alo_tobool(env, 0)) {
		return n;
	}
	else {
		if (n == 1) {
			alo_pushlstr(env, "assertion failed!");
		}
		alo_settop(env, 2);
		alo_raise(env);
		return 0;
	}
}

void aloopen_base(a_henv env) {
	static aloL_Binding bindings[] = {
		{ "print", base_print },
		{ "assert", base_assert }
	};

	aloL_newmod(env, ALO_LIB_BASE_NAME, bindings);
}