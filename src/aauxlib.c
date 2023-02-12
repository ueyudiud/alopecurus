/**
 *@file aauxlib.c
 */

#define aauxlib_c_
#define ALO_LIB

#include <stdlib.h>
#include <fcntl.h>

#include "abuf.h"
#include "atable.h"
#include "afun.h"
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
		GFunMeta* meta = g_cast(GFunMeta, fun->_meta);
		char const* file = meta->_dbg_file != null ? cast(char const*, meta->_dbg_file->_data) : null;
		a_u32 line = ai_dbg_get_line(meta, frame->_pc);
		if (head) {
			if (file != null) {
				ai_buf_putf(env, buf, "%s:%u: ", file, line);
			}
			switch (v_raw_tag(err)) {
				case T_HSTR:
				case T_ISTR: {
					GStr* str = v_as_str(G(env), err);
					ai_buf_putls(env, buf, str->_data, str->_len);
					break;
				}
				case T_INT: {
					a_u32 code = cast(a_u32, v_as_int(err));
					ai_buf_putf(env, buf, "error code: %08x", code);
					break;
				}
				default: {
					return ALO_EINVAL;
				}
			}
			if (limit == 0) break;
			ai_buf_puts(env, buf, "\nstack trace:");
			head = false;
		}
		ai_buf_puts(env, buf, "\n\t");
		if (limit-- == 0) {
			ai_buf_puts(env, buf, "...");
			break;
		}
		if (line != 0) {
			assume(file != null);
			ai_buf_putf(env, buf, "at %s:%u", file, line);
		}
		else {
			ai_buf_putf(env, buf, "at %s", file ?: "?");
		}
		if (meta->_dbg_name != null) {
			ai_buf_putf(env, buf, " (%s)", meta->_dbg_name->_data);
		}
	}
	GStr* str = ai_str_create(env, buf->_arr, buf->_len);
	v_set(G(env), err, v_of_ref(str));
	return ALO_SOK;
}

a_msg aloL_traceerror(a_henv env, a_isize id, a_usize limit) {
	if (env->_frame->_prev != null) {
		Buf buf;
		ai_buf_open(&buf);

		a_msg msg = l_wrap_error(env, id, limit, &buf);
		ai_buf_close(env, &buf);

		return msg;
	}

	return ALO_EINVAL;
}

void aloL_newmod_(a_henv env, char const* name, aloL_Binding const* bs, a_usize nb) {
	api_check_slot(env, 1);

	GRefArray* refs = ai_ref_array_new(env, nb + 1);
	v_set(G(env), api_incr_stack(env), v_of_ref(refs));

	GStr* name_ref = ai_str_create(env, name, strlen(name));
	refs->_data[0] = gobj_cast(name_ref);
	for (a_usize i = 0; i < nb; ++i) {
		char const* field_name = bs[i].name;
		refs->_data[i + 1] = gobj_cast(ai_str_create(env, field_name, strlen(field_name)));
	}

	GMod* mod = ai_mod_new(env, name_ref, cast(GStr**, &refs->_data[1]), nb);
	v_set(G(env), api_wrslot(env, -1), v_of_ref(mod));

	for (a_u32 i = 0; i < nb; ++i) {
		a_cfun fptr = bs[i].fptr;
		if (fptr != null) {
			GFun* fun = ai_cfun_create(env, fptr, 0, null);
			v_set(G(env), &mod->_values[i], v_of_ref(fun));
		}
	}

	ai_gc_trigger(env);
}

typedef struct {
	char const* _name;
	void (*_init)(a_henv);
} LibEntry;

static void l_open_lib(a_henv env, LibEntry const* entry) {
	(*entry->_init)(env);
	a_hmod mod = alo_openmod(env, -1);
	ai_mod_cache(env, null, mod);
	ai_table_set(env, v_as_table(G(env), &G(env)->_global), v_of_ref(mod->_name), v_of_ref(mod));
	api_decr_stack(env);
}

void aloL_openlibs(a_henv env) {
	static LibEntry const entries[] = {
		{ ALO_LIB_BASE_NAME, aloopen_base },
		{ ALO_LIB_SYS_NAME, aloopen_sys }
	};

	for (a_usize i = 0; i < sizeof(entries) / sizeof(LibEntry); ++i) {
		l_open_lib(env, &entries[i]);
	}
}
