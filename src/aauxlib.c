/**
 *@file aauxlib.c
 */

#define aauxlib_c_
#define ALO_LIB

#include <stdlib.h>
#include <fcntl.h>

#include "abuf.h"
#include "astr.h"
#include "atable.h"
#include "afun.h"
#include "atype.h"
#include "aenv.h"
#include "agc.h"
#include "adbg.h"
#include "aapi.h"

#include "alolib.h"
#include "aauxlib.h"

static void* aux_alloc(unused void* ctx, a_usize sz) {
	return malloc(sz);
}

static void* aux_realloc(unused void* ctx, void* blk, unused a_usize sz_old, a_usize sz_new) {
	return realloc(blk, sz_new);
}

static void aux_dealloc(unused void* ctx, void* blk, unused a_usize sz) {
	free(blk);
}

a_henv aloL_create(void) {
	a_alloc af = {
		.allocate = aux_alloc,
		.reallocate = aux_realloc,
		.deallocate = aux_dealloc
	};
	a_henv env;
	a_msg msg = alo_create(&af, null, &env);
	return msg == ALO_SOK ? env : null;
}

void aloL_argerror(a_henv env, a_usize id, char const* what) {
	char const* name = ai_dbg_get_func_name(env, env->_frame);
	if (name == null) return aloL_errorf(env, "bad argument #%tu, %s", id, what);
	return aloL_errorf(env, "bad argument #%tu to '%s', %s", id, name, what);
}

static char const* l_typename(a_henv env, a_isize id) {
	Value const* p = api_roslot(env, id);
	return likely(p != null) ? v_nameof(env, *p) : "empty";
}

void aloL_typeerror(a_henv env, a_usize id, char const* name) {
	char const* what = alo_pushfstr(env, "'%s' expected, got '%s'", name, l_typename(env, cast(a_isize, id)));
	aloL_argerror(env, id, what);
}

void aloL_checktag(a_henv env, a_usize id, a_tag tag) {
	api_check(tag >= ALO_TEMPTY && tag <= ALO_TUSER, "bad type tag.");
	if (alo_tagof(env, cast(a_isize, id)) != tag) {
		aloL_typeerror(env, id, ai_api_tagname[tag]);
	}
}

static a_i32 l_read_str(unused a_henv env, void* rctx, void const** pdst, size_t* plen) {
	a_lstr* ctx = rctx;
	*pdst = ctx->_ptr;
	*plen = ctx->_len;
	ctx->_ptr = null;
	ctx->_len = 0;
	return 0;
}

typedef int a_fno;

typedef struct {
	a_byte _buffer[256];
	a_fno _handle;
} FileReadCtx;

static a_i32 l_read_file(unused a_henv env, void* rctx, void const** pdst, size_t* plen) {
	FileReadCtx* ctx = rctx;
	int count = read(ctx->_handle, ctx->_buffer, sizeof(ctx->_buffer));
	if (count < 0) return count;
	*pdst = ctx->_buffer;
	*plen = cast(a_usize, count);
	return 0;
}

a_msg aloL_compiles(a_henv env, char const* src, a_usize len, char const* fname, a_u32 options) {
	a_lstr str = {src, len};
	alo_pushstr(env, fname, strlen(fname));
	a_msg msg = alo_compile(env, l_read_str, &str, ALO_STACK_INDEX_GLOBAL, ALO_STACK_INDEX_EMPTY, -1, options);
	alo_pop(env, -2);
	return msg;
}

a_msg aloL_compilef(a_henv env, char const* fname, a_u32 options) {
	a_fno handle = open(fname, O_RDONLY);
	if (handle < 0) return ALO_EIO;

	FileReadCtx ctx;
	ctx._handle = handle;

	alo_pushstr(env, fname, strlen(fname));
	a_msg msg = alo_compile(env, l_read_file, &ctx, ALO_STACK_INDEX_GLOBAL, ALO_STACK_INDEX_EMPTY, -1, options);
	alo_pop(env, -2);

	close(ctx._handle);
	return msg;
}

void aloL_errorf(a_henv env, char const* fmt, ...) {
	va_list varg;
	va_start(varg, fmt);
	alo_pushvfstr(env, fmt, varg);
	va_end(varg);
	alo_raise(env);
}

static a_msg l_wrap_error(a_henv env, a_isize id, a_usize limit, Buf* buf) {
	a_bool head = true;
	Value* err = api_wrslot(env, id);
	for (Frame* frame = env->_frame; frame->_prev != null; frame = frame->_prev) {
		GFun* fun = ai_dbg_get_func(env, frame);
		assume(fun != null);
		GProto* proto = fun->_proto;
		char const* file = proto->_dbg_file != null ? str2ntstr(proto->_dbg_file) : null;
		a_insn const* pc = head ? frame->_pc : frame->_pc - 1;
		a_u32 line = ai_dbg_get_line(proto, pc);
		if (head) {
			if (file != null) {
				ai_buf_xputfs(env, buf, "%s:%u: ", file, line);
			}
			switch (v_get_tag(*err)) {
				case T_HSTR:
				case T_ISTR: {
					GStr* str = v_as_str(*err);
					ai_buf_xputls(env, buf, str->_data, str->_len);
					break;
				}
				case T_INT: {
					a_u32 code = cast(a_u32, v_as_int(*err));
					ai_buf_xputfs(env, buf, "error code: %08x", code);
					break;
				}
				default: {
					return ALO_EINVAL;
				}
			}
			if (limit == 0) break;
			ai_buf_xputs(env, buf, "\nstack trace:");
			head = false;
		}
		ai_buf_xputs(env, buf, "\n\t");
		if (limit-- == 0) {
			ai_buf_xputs(env, buf, "...");
			break;
		}
		if (line != 0) {
			assume(file != null);
			ai_buf_xputfs(env, buf, "at %s:%u", file, line);
		}
		else {
			ai_buf_xputfs(env, buf, "at %s", file ?: "?");
		}
		if (proto->_name != null) {
			ai_buf_xputfs(env, buf, " (%s)", str2ntstr(proto->_name));
		}
	}
	GStr* str = ai_str_new(env, buf->_ptr, buf->_len);
	v_set_obj(env, err, str);
	return ALO_SOK;
}

a_msg aloL_traceerror(a_henv env, a_isize id, a_usize limit) {
	if (env->_frame->_prev != null) {
		Buf buf;
		ai_buf_init(&buf);
		a_msg msg = l_wrap_error(env, id, limit, &buf);
		ai_buf_deinit(G(env), &buf);
		return msg;
	}

	return ALO_EINVAL;
}

void aloL_newmod_(a_henv env, char const* name, aloL_Binding const* bs, a_usize nb) {
	api_check_slot(env, 1);

	GType* mod = ai_ctype_alloc(env, nb);
	v_set_obj(env, api_incr_stack(env), mod);

	mod->_name = ai_str_new(env, name, strlen(name));

	for (a_usize i = 0; i < nb; ++i) {
		aloL_Binding const* b = &bs[i];
		assume(b->name != null);

		GStr* key = ai_str_new(env, b->name, strlen(b->name));

		if (b->fptr != null) {
			GFun* fun = ai_cfun_create(env, b->fptr, 0, null);
			ai_type_setis(env, mod, key, v_of_obj(fun));
		}
	}

	ai_type_ready(env, mod);

	ai_gc_trigger(env);
}

typedef struct {
	char const* _name;
	void (*_init)(a_henv);
} LibEntry;

static void l_open_lib(a_henv env, LibEntry const* entry) {
	(*entry->_init)(env);
	a_htype type = v_as_type(api_elem(env, -1));
	ai_type_cache(env, null, type);
	ai_table_set(env, v_as_table(G(env)->_global), v_of_obj(type->_name), v_of_obj(type));
	api_decr_stack(env);
}

void aloL_openlibs(a_henv env) {
	static LibEntry const entries[] = {
		{ ALO_LIB_BASE_NAME, aloopen_base },
		{ ALO_LIB_DEBUG_NAME, aloopen_debug },
		{ ALO_LIB_SYS_NAME, aloopen_sys }
	};

	for (a_usize i = 0; i < sizeof(entries) / sizeof(LibEntry); ++i) {
		l_open_lib(env, &entries[i]);
	}
}
