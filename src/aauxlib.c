/**
 *@file aauxlib.c
 */

#define aauxlib_c_
#define ALO_LIB

#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>

#include "abuf.h"
#include "astr.h"
#include "amod.h"
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
	alo_Alloc af = {
		.allocate = aux_alloc,
		.reallocate = aux_realloc,
		.deallocate = aux_dealloc
	};
	a_henv env;
	a_msg msg = alo_create(&af, null, &env);
	return msg == ALO_SOK ? env : null;
}

void aloL_argerror(a_henv env, a_ulen id, char const* what) {
	char const* name = ai_dbg_get_func_name(env, env->_frame);
	if (name == null) aloL_raisef(env, "bad argument #%tu, %s", id, what);
	aloL_raisef(env, "bad argument #%tu to '%s', %s", id, name, what);
}

static char const* l_typename(a_henv env, a_ilen id) {
	Value const* p = api_roslot(env, id);
	return likely(p != null) ? v_nameof(env, *p) : "empty";
}

void aloL_typeerror(a_henv env, a_ulen id, char const* name) {
	char const* what = alo_pushfstr(env, "'%s' expected, got '%s'", name, l_typename(env, cast(a_ilen, id)));
	aloL_argerror(env, id, what);
}

#define aloL_tagof(env,id) alo_tagof(env, cast(a_isize, id))

void aloL_checktag(a_henv env, a_ulen id, a_msg tag) {
	api_check(tag >= ALO_TNIL && tag <= ALO_TUSER, "bad type tag.");
	if (aloL_tagof(env, id) != tag) {
		aloL_typeerror(env, id, ai_api_tagname[tag]);
	}
}

a_msg aloL_checkany(a_henv env, a_ulen id) {
    a_msg tag = aloL_tagof(env, id);
    if (tag == ALO_EEMPTY) {
        aloL_argerror(env, id, "value expected");
    }
    return tag;
}

a_int aloL_checkint(a_henv env, a_ulen id) {
	Value v = api_elem(env, cast(a_isize, id));
	if (!v_is_int(v)) {
		aloL_typeerror(env, id, ai_api_tagname[ALO_TINT]);
	}
	return v_as_int(v);
}

a_float aloL_checknum(a_henv env, a_ulen id) {
	Value v = api_elem(env, cast(a_isize, id));
	if (!v_is_num(v)) {
		aloL_typeerror(env, id, ai_api_tagname[ALO_TFLOAT]);
	}
	return v_as_num(v);
}

char const* aloL_checklstr(a_henv env, a_ulen id, a_usize* plen) {
	Value v = api_elem(env, cast(a_isize, id));
	if (!v_is_str(v)) {
		aloL_typeerror(env, id, ai_api_tagname[ALO_TSTR]);
	}
	GStr* str = v_as_str(v);
	if (plen != null) {
		*plen = str->_len;
	}
	return str2ntstr(str);
}

a_bool aloL_optbool(a_henv env, a_ulen id, a_bool dfl) {
    Value v = api_elem(env, cast(a_ilen, id));
    return !v_is_nil(v) ? v_to_bool(v) : dfl;
}

a_bool aloL_optint_(a_henv env, a_ulen id, a_int* pval) {
	Value v = api_elem(env, cast(a_ilen, id));
	if (!v_is_int(v)) {
		if (v_is_nil(v)) {
			return false;
		}
		aloL_typeerror(env, id, ai_api_tagname[ALO_TINT]);
	}
	*pval = v_as_int(v);
	return true;
}

a_bool aloL_optnum_(a_henv env, a_ulen id, a_float* pval) {
	Value v = api_elem(env, cast(a_ilen, id));
	if (!v_is_num(v)) {
		if (v_is_nil(v)) {
			return false;
		}
		aloL_typeerror(env, id, ai_api_tagname[ALO_TFLOAT]);
	}
	*pval = v_as_num(v);
	return true;
}

char const* aloL_optlstr(a_henv env, a_ulen id, a_usize* plen) {
	Value v = api_elem(env, cast(a_usize, id));
	if (!v_is_str(v)) {
		if (v_is_nil(v)) {
			return null;
		}
		aloL_typeerror(env, id, ai_api_tagname[ALO_TSTR]);
	}
	GStr* str = v_as_str(v);
	if (plen != null) {
		*plen = str->_len;
	}
	return str2ntstr(str);
}

a_msg aloL_resultcx(a_henv env, a_bool stat, int err, char const* what) {
	if (stat) {
		alo_pushbool(env, true);
		return 1;
	}
	else {
		aloL_pushfail(env);
		if (what != null) {
			alo_pushfstr(env, "%s: %s", what, strerror(err));
		}
		else {
			alo_pushntstr(env, strerror(err));
		}
		alo_pushint(env, err);
		return 3;
	}
}

#if ALO_OS_POSIX

# include <sys/wait.h>
# define l_inspect_result(stat) \
	if (WIFEXITED(stat)) { stat = WEXITSTATUS(stat); } \
	else if (WIFSIGNALED(stat)) { stat = WTERMSIG(stat); what = "signal"; }

#else

# define l_inspect_result(stat) quiet()

#endif

a_msg aloL_resulte(a_henv env, a_i32 stat) {
	char const* what = "exit";
	int err = errno;
	if (stat != 0 && err != 0) {
		return aloL_resultcx(env, false, err, null);
	}
	else {
		l_inspect_result(stat);
		if (*what == 'e' && stat == 0) {
			alo_pushbool(env, true);
		}
		else {
			aloL_pushfail(env);
		}
		alo_pushntstr(env, what);
		alo_pushint(env, stat);
		return 3;
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
	char _buffer[256];
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
	a_lstr str = {src, len };
	alo_pushntstr(env, fname);
	a_msg msg = alo_compile(env, l_read_str, &str, ALO_STACK_INDEX_GLOBAL, ALO_STACK_INDEX_EMPTY, -1, options);
	alo_pop(env, -2);
	return msg;
}

a_msg aloL_compilef(a_henv env, char const* fname, a_u32 options) {
	a_fno handle = open(fname, O_RDONLY);
	if (handle < 0) return ALO_EEMPTY;

	FileReadCtx ctx;
	ctx._handle = handle;

	alo_pushntstr(env, fname);
	a_msg msg = alo_compile(env, l_read_file, &ctx, ALO_STACK_INDEX_GLOBAL, ALO_STACK_INDEX_EMPTY, -1, options);
	alo_pop(env, -2);

	close(ctx._handle);
	return msg;
}

void aloL_raisef(a_henv env, char const* fmt, ...) {
	va_list varg;
	va_start(varg, fmt);
	alo_pushvfstr(env, fmt, varg);
	va_end(varg);
	alo_raise(env);
}

typedef struct {
	char const* _name;
	char const* _file;
	a_u32 _line;
} Trace;

#define TRACE_UNKNOWN_FILE "?"
#define TRACE_NATIVE_FILE "[C]"

static void trace_fill(a_henv env, Frame* frame, Trace* trace) {
	GFun* fun = ai_dbg_get_func(env, frame);
	assume(fun != null, "cannot trace root frame.");
	if (!(fun->_flags & FUN_FLAG_NATIVE)) {
		GProto* proto = fun->_proto;
		trace->_name = proto->_name != null ? str2ntstr(proto->_name) : null;
		trace->_file = proto->_dbg_file != null ? str2ntstr(proto->_dbg_file) : TRACE_UNKNOWN_FILE;
		trace->_line = ai_dbg_get_line(proto, frame->_pc - 1);
	}
	else {
		trace->_name = null;
		trace->_file = TRACE_NATIVE_FILE;
		trace->_line = 0;
	}
}

static Frame* l_frame_at(a_henv env, a_usize level) {
    Frame* frame = env->_frame;

    loop {
        if (frame->_prev == null) {
            return null;
        }
        else if (level == 0) {
            return frame;
        }

        frame = frame->_prev;
        level -= 1;
    }
}

static a_msg l_wrap_error(a_henv env, a_ilen id, a_usize level, a_usize limit, Buf* buf) {
    Trace trace;
	Value* err = api_wrslot(env, id);
    Frame* frame = l_frame_at(env, level);
    if (frame == null) return ALO_EINVAL;

    trace_fill(env, frame, &trace);

    if (trace._file != null) {
        if (trace._line != 0) {
            try (ai_buf_nputfs(env, buf, "%s:%u: ", trace._file, trace._line));
        }
        else {
            try (ai_buf_nputfs(env, buf, "%s: ", trace._file));
        }
    }

    switch (v_get_tag(*err)) {
        case T_STR: {
            GStr* str = v_as_str(*err);
            try (ai_buf_nputls(env, buf, str->_ptr, str->_len));
            break;
        }
        case T_INT: {
            a_u32 code = cast(a_u32, v_as_int(*err));
            try (ai_buf_nputfs(env, buf, "error code: %08x", code));
            break;
        }
        default: {
            return ALO_EINVAL;
        }
    }

    if (limit > 0) {
        try (ai_buf_nputs(env, buf, "\nstack trace:\n\t"));
        loop {
            if (trace._line != 0) {
                try (ai_buf_nputfs(env, buf, "at %s:%u", trace._file, trace._line));
            }
            else {
                try (ai_buf_nputfs(env, buf, "at %s", trace._file));
            }

            if (trace._name != null) {
                try (ai_buf_nputfs(env, buf, " (%s)", trace._name));
            }

			if (limit-- == 0) {
				try (ai_buf_nputs(env, buf, "\n\t..."));
				break;
			}
			else if (frame->_flags & FRAME_FLAG_TAIL_CALL) {
				try (ai_buf_nputs(env, buf, "\n\t... (tail call)"));
				if (limit > 1) {
					limit -= 1;
				}
			}

            frame = frame->_prev;
            if (frame->_prev == null)
                break;
            trace_fill(env, frame, &trace);

			try (ai_buf_nputs(env, buf, "\n\t"));
        }
    }

	GStr* str = ai_str_get_or_new(env, buf->_ptr, buf->_len);
	v_set_str(env, err, str);
	return ALO_SOK;
}

a_msg aloL_traceerror(a_henv env, a_ilen id, a_usize level, a_usize limit) {
	if (env->_frame->_prev != null) {
		Buf buf[1];
		at_buf_init(buf);
		a_msg msg = l_wrap_error(env, id, level, limit, buf_cast(buf));
		at_buf_deinit(G(env), buf);
		return msg;
	}

	return ALO_EINVAL;
}

a_msg aloL_gets(a_henv env, a_ilen id, char const* s) {
    Value v = api_elem(env, id);
    api_check(v_is_mod(v), "module expected.");

    GMod* o = v_as_mod(v);

    catch (ai_mod_getls(env, o, s, strlen(s), &v)) {
        return ALO_EEMPTY;
    }

    v_set(env, api_incr_stack(env), v);

    return api_tagof(env, v);
}

a_msg aloL_gettm(a_henv env, a_ilen id, char const* s) {
    Value v = api_elem(env, id);

    GType* o = v_typeof(env, v);

    catch (ai_mod_getls(env, type2mt(o), s, strlen(s), &v)) {
        return ALO_EEMPTY;
    }

    v_set(env, api_incr_stack(env), v);

    return api_tagof(env, v);
}

void aloL_puts(a_henv env, a_ilen id, char const* s) {
    Value v = api_elem(env, id);
    api_check(v_is_mod(v), "module expected.");

    GMod* o = v_as_mod(v);

    Value* p = ai_mod_refls(env, o, s, strlen(s));

    v = api_decr_stack(env);
    v_set(env, p, v);

    ai_gc_barrier_backward_val(env, o, v);

    ai_gc_trigger(env);
}

void aloL_putalls_(a_henv env, a_ilen id, aloL_Entry const* es, a_usize ne) {
	Value v = api_elem(env, id);
	api_check(v_is_mod(v), "module expected.");

	GMod* o = v_as_mod(v);

	for (a_usize i = 0; i < ne; ++i) {
		aloL_Entry const* e = &es[i];
		assume(e->name != null, "missing field name.");

        Value* slot = ai_mod_refls(env, o, e->name, strlen(e->name));
        v_set_nil(slot);

		if (e->fptr != null) {
			GFun* fun = ai_cfun_create(env, e->fptr, 0, null);
            v_set_obj(env, slot, fun);
            ai_gc_barrier_backward(env, o, fun);
        }
	}

	ai_gc_trigger(env);
}

typedef struct {
    GOBJ_STRUCT_HEADER;
    a_usize _size;
    a_byte _body[];
} GBlock;

#define block_size(s) pad_to(sizeof(GBlock) + (s), sizeof(a_usize))

static void block_drop(Global* gbl, GBlock* self) {
    ai_mem_dealloc(gbl, self, block_size(self->_size));
}

static void block_mark(Global* gbl, GBlock* self) {
    ai_gc_trace_work(gbl, block_size(self->_size));
}

static VTable const block_vtable = {
    ._stencil = V_STENCIL(T_USER),
    ._tag = ALO_TUSER,
    ._slots = {
        [vfp_slot(drop)] = block_drop,
        [vfp_slot(mark)] = block_mark
    }
};

void* aloL_newblk(a_henv env, a_usize s) {
    api_check_slot(env, 1);

    GBlock* self = ai_mem_alloc(env, block_size(s));
    self->_vptr = &block_vtable;
    self->_size = s;
    ai_gc_register_object(env, self);

    v_set_obj(env, api_incr_stack(env), self);

    return self->_body;
}

static_assert(offsetof(aloL_Buf, ptr) == offsetof(Buf, _ptr));
static_assert(offsetof(aloL_Buf, len) == offsetof(Buf, _len));
static_assert(offsetof(aloL_Buf, cap) == offsetof(Buf, _cap));

static GBuf* from_buff(a_henv env, aloL_Buf* raw) {
    GBuf* self = from_member(GBuf, _buf_head_mark, cast(BufHeadMark*, raw));
    v_check_alive(env, v_of_obj(self));
    return self;
}

aloL_Buf* aloL_newbuf(a_henv env) {
    api_check_slot(env, 1);

    GBuf* self = ai_buf_new(env);
    v_set_obj(env, api_incr_stack(env), self);
    return cast(aloL_Buf*, self->_buf_head_mark);
}

void aloL_bufhint(a_henv env, aloL_Buf* b, a_usize a) {
    GBuf* self = from_buff(env, b);
    at_buf_check(env, self, a);
}

void aloL_bufpush(a_henv env, aloL_Buf* b) {
    GBuf* self = from_buff(env, b);
    GStr* str = v_as_str(api_pre_decr_stack(env));
    at_buf_putls(env, self, str->_ptr, str->_len);
    api_post_decr_stack(env);
}

void aloL_bufstr(a_henv env, aloL_Buf* b) {
    GBuf* self = from_buff(env, b);
    alo_pushstr(env, self->_ptr, self->_len);
}

typedef struct {
	char const* _name;
	void (*_init)(a_henv);
} LibEntry;

static void l_open_lib(a_henv env, LibEntry const* entry) {
    alo_pushntstr(env, entry->_name);
	(*entry->_init)(env);
    alo_set(env, ALO_STACK_INDEX_GLOBAL);
}

void aloL_openlibs(a_henv env) {
	static LibEntry const entries[] = {
		{ ALO_LIB_BASE_NAME, aloopen_base },
        { ALO_LIB_MOD_NAME, aloopen_mod },
        { ALO_LIB_TYPE_NAME, aloopen_type },
        { ALO_LIB_STR_NAME, aloopen_str },
        { ALO_LIB_LIST_NAME, aloopen_list },
		{ ALO_LIB_DEBUG_NAME, aloopen_debug },
		{ ALO_LIB_INT_NAME, aloopen_int },
		{ ALO_LIB_SYS_NAME, aloopen_sys },
        { ALO_LIB_LOAD_NAME, aloopen_load }
	};

	for (a_usize i = 0; i < sizeof(entries) / sizeof(LibEntry); ++i) {
		l_open_lib(env, &entries[i]);
	}
}
