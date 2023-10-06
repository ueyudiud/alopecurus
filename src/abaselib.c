/**
 *@file abaselib.c
 */

#define abaselib_c_
#define ALO_LIB

#include <stdio.h>

#include "atuple.h"
#include "ameta.h"
#include "auser.h"
#include "agc.h"
#include "aapi.h"

#include "alolib.h"
#include "abaselib.h"

#define MAX_SHOW_LEN 16
#define MAX_SHOW_DEPTH 16

static void l_show_impl(a_henv env, Value v, a_u32 depth) {
	switch (v_get_tag(v)) {
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
		case T_STR: {
			GStr* val = v_as_str(v);
			aloi_show("%s", str2ntstr(val));
			break;
		}
		case T_TUPLE: {
			if (depth >= MAX_SHOW_DEPTH) {
				aloi_show("(...)");
			}
			else {
				GTuple* val = v_as_tuple(v);
				a_u32 n = min(val->_len, MAX_SHOW_LEN);
				if (val->_len > 0) {
					aloi_show("(");
					l_show_impl(env, val->_ptr[0], depth + 1);
					for (a_u32 i = 1; i < n; ++i) {
						aloi_show(", ");
						l_show_impl(env, val->_ptr[i], depth + 1);
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
				GList* val = v_as_list(v);
				a_u32 n = min(val->_len, MAX_SHOW_LEN);
				if (val->_len > 0) {
					aloi_show("[");
					l_show_impl(env, val->_ptr[0], depth + 1);
					for (a_u32 i = 1; i < n; ++i) {
						aloi_show(", ");
						l_show_impl(env, val->_ptr[i], depth + 1);
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
                GTable *val = v_as_table(v);
                if (val->_len == 0) {
                    aloi_show("{}");
                }
                else {
                    a_u32 n = MAX_SHOW_LEN;
                    aloi_show("{");
                    a_bool tail = false;
                    TNode *node;
                    for (a_x32 itr = val->_ptr->_link._next; !is_nil(itr); itr = node->_link._next) {
                        node = &val->_ptr[unwrap(itr)];
                        if (tail) {
                            aloi_show(", ");
                        }
                        else {
                            tail = true;
                        }
                        l_show_impl(env, node->_key, depth + 1);
                        aloi_show(" -> ");
                        l_show_impl(env, node->_value, depth + 1);
                        if (--n == 0) {
                            aloi_show(", ...");
                            break;
                        }
                    }
                    aloi_show("}");
                }
            }
			break;
		}
		case T_FUNC: {
			aloi_show("<func:%p>", v_as_obj(v));
			break;
		}
		case T_META: {
			aloi_show("<meta:%s>", str2ntstr(v_as_meta(v)->_name));
			break;
		}
		case T_USER: {
			aloi_show("<%s:%p>", v_nameof(env, v), v_as_obj(v));
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
void aloB_show(a_henv env, a_isize id) {
	Value const* v = api_roslot(env, id);
	api_check(v != null, "bad slot id.");
	l_show_impl(env, *v, 0);
}

static a_msg base_print(a_henv env) {
	a_usize len = alo_stacksize(env);
	for (a_usize id = 0; id < len; ++id) {
		if (id != 0) aloi_show("\t");
		aloB_show(env, cast(a_isize, id));
	}
	aloi_show_newline();
	return 0;
}

#define ERROR_DEFAULT_LEVEL 0
#define ERROR_DEFAULT_LIMIT 6

static a_msg base_error(a_henv env) {
    a_int level = aloL_optint(env, 1, ERROR_DEFAULT_LEVEL);
    a_int limit = aloL_optint(env, 2, ERROR_DEFAULT_LIMIT);

    alo_settop(env, 1);
    aloL_traceerror(env, 0, cast(a_usize, max(level, 0) + 1), limit >= 0 ? cast(a_usize, limit) : SIZE_MAX);
    alo_raise(env);
}

static a_msg base_assert(a_henv env) {
	a_usize n = alo_stacksize(env);
	if (alo_tobool(env, 0)) {
		return cast(a_msg, n);
	}
	else {
		if (n == 1) {
			alo_pushlstr(env, "assertion failed!");
		}
		alo_settop(env, 2);
        aloL_traceerror(env, 1, 1, 6);
		alo_raise(env);
	}
}

static a_msg base_typeof(a_henv env) {
	Value v = api_elem(env, 0);
	alo_settop(env, 1);
	v_set_obj(env, api_wrslot(env, 0), v_typeof(env, v));
	return 1;
}

void aloopen_base(a_henv env) {
	static aloL_Entry const bindings[] = {
		{ "assert", base_assert },
        { "error", base_error },
		{ "print", base_print },
		{ "typeof", base_typeof },
		{ "_VER", null }
	};

    alo_push(env, ALO_STACK_INDEX_GLOBAL);
	aloL_putfields(env, -1, bindings);

	GMeta* meta = v_as_meta(api_elem(env, -1));

	ai_meta_set(env, meta, v_of_obj(ai_str_newl(env, "_VER")), v_of_int(ALO_VERSION_NUMBER));

	ai_gc_trigger(env);
}