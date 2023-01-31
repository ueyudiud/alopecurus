/*
 * aaux.c
 */

#define aaux_c_

#include <stdlib.h>
#include <fcntl.h>

#include "adef.h"

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

static ptrdiff_t l_read_str(a_henv env, void* rctx, void const** pdst, size_t* plen) {
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

static ptrdiff_t l_read_file(unused a_henv env, void* rctx, void const** pdst, size_t* plen) {
	FileReadCtx* ctx = rctx;
	int count = read(ctx->_handle, ctx->_buffer, sizeof(ctx->_buffer));
	if (count < 0) return count;
	*pdst = ctx->_buffer;
	*plen = cast(a_usize, count);
	return 0;
}

a_msg aloL_readstr(a_henv env, char const* src, a_usize len, char const* name, a_u32 options) {
	a_lstr str = {src, len};
	return alo_compile(env, l_read_str, &str, name, options);
}

a_msg aloL_readfile(a_henv env, char const* fname, a_u32 options) {
	a_fno handle = open(fname, O_RDONLY);
	if (handle < 0) return ALO_EIO;

	FileReadCtx ctx;
	ctx._handle = handle;

	a_msg msg = alo_compile(env, l_read_file, &ctx, fname, options);

	close(ctx._handle);
	return msg;
}
