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

/**
 ** Get API slot index from absolute stack index.
 */
static a_ilen uid2sid(a_henv env, a_ulen id) {
    if (id >= env->stack.top - ai_stk_bot(env)) {
        /* Return empty stack index if overflow */
        return ALO_STACK_INDEX_EMPTY;
    }
    return cast(a_ilen, id);
}

void aloL_argerror(a_henv env, a_ulen id, char const* what) {
	char const* name = ai_dbg_get_func_name(env, env->frame);
    a_ilen sid = uid2sid(env, id);
	if (name == null) aloL_raisef(env, "bad argument #%u, %s", sid, what);
	aloL_raisef(env, "bad argument #%u to '%s', %s", sid, name, what);
}

void aloL_typeerror(a_henv env, a_ulen id, char const* name) {
    a_ilen sid = uid2sid(env, id);
	char const* what = alo_pushfstr(env, "'%s' expected, got '%s'", name, v_nameof(env, api_elem(env, sid)));
	aloL_argerror(env, sid, what);
}

void aloL_checktag(a_henv env, a_ulen id, a_msg tag) {
	api_check(tag >= ALO_TNIL && tag <= ALO_TUSER, "bad type tag.");
	if (alo_tagof(env, uid2sid(env, id)) != tag) {
		aloL_typeerror(env, id, ai_api_tagname[tag]);
	}
}

a_msg aloL_checkany(a_henv env, a_ulen id) {
    a_msg tag = alo_tagof(env, uid2sid(env, id));
    if (tag == ALO_EEMPTY) {
        aloL_argerror(env, id, "value expected");
    }
    return tag;
}

a_int aloL_checkint(a_henv env, a_ulen id) {
	Value v = api_elem(env, uid2sid(env, id));
	if (!v_is_int(v)) {
		aloL_typeerror(env, id, ai_api_tagname[ALO_TINT]);
	}
	return v_as_int(v);
}

a_float aloL_checknum(a_henv env, a_ulen id) {
	Value v = api_elem(env, uid2sid(env, id));
	if (!v_is_num(v)) {
		aloL_typeerror(env, id, ai_api_tagname[ALO_TFLOAT]);
	}
	return v_as_num(v);
}

void* aloL_checkptr(a_henv env, a_ulen id) {
    Value v = api_elem(env, uid2sid(env, id));
    if (!v_is_ptr(v) && !v_is_nil(v)) {
        aloL_typeerror(env, id, ai_api_tagname[ALO_TPTR]);
    }
    return v_is_ptr(v) ? v_as_ptr(v) : null;
}

char const* aloL_checklstr(a_henv env, a_ulen id, a_usize* plen) {
	Value v = api_elem(env, uid2sid(env, id));
	if (!v_is_str(v)) {
		aloL_typeerror(env, id, ai_api_tagname[ALO_TSTR]);
	}
	GStr* str = v_as_str(v);
	if (plen != null) {
		*plen = str->len;
	}
	return str2ntstr(str);
}

a_bool aloL_optbool(a_henv env, a_ulen id, a_bool dfl) {
    Value v = api_elem(env, uid2sid(env, id));
    return !v_is_nil(v) ? v_to_bool(v) : dfl;
}

a_bool aloL_optint_(a_henv env, a_ulen id, a_int* pval) {
	Value v = api_elem(env, uid2sid(env, id));
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
	Value v = api_elem(env, uid2sid(env, id));
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
	Value v = api_elem(env, uid2sid(env, id));
	if (!v_is_str(v)) {
		if (v_is_nil(v)) {
			return null;
		}
		aloL_typeerror(env, id, ai_api_tagname[ALO_TSTR]);
	}
	GStr* str = v_as_str(v);
	if (plen != null) {
		*plen = str->len;
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

typedef struct {
    char const* ptr;
    a_usize len;
} ReadStrCtx;

static a_i32 l_read_str(unused a_henv env, void* rctx, void const** pdst, size_t* plen) {
    ReadStrCtx* ctx = rctx;
	*pdst = ctx->ptr;
	*plen = ctx->len;
	ctx->ptr = null;
	ctx->len = 0;
	return 0;
}

a_msg aloL_compiles(a_henv env, char const* src, a_usize len, char const* fname, a_u32 options) {
    ReadStrCtx str = {
        src,
        len
    };
	alo_pushntstr(env, fname);
	a_msg msg = alo_compile(env, l_read_str, &str, ALO_STACK_INDEX_GLOBAL, ALO_STACK_INDEX_EMPTY, -1, options);
	alo_pop(env, -2);
	return msg;
}

typedef int a_hfile;

typedef struct {
    char buff[256];
    a_hfile file;
} FileReadCtx;

static a_i32 l_read_file(unused a_henv env, void* rctx, void const** pdst, size_t* plen) {
    FileReadCtx* ctx = rctx;
    int count = read(ctx->file, ctx->buff, sizeof(ctx->buff));
    if (count < 0) return count;
    *pdst = ctx->buff;
    *plen = cast(a_usize, count);
    return 0;
}

a_msg aloL_compilef(a_henv env, char const* fname, a_u32 options) {
    a_hfile file = fname != null ? open(fname, O_RDONLY) : STDIN_FILENO;
	if (file < 0) return ALO_EEMPTY;

	FileReadCtx ctx;
	ctx.file = file;

	alo_pushntstr(env, fname);
	a_msg msg = alo_compile(env, l_read_file, &ctx, ALO_STACK_INDEX_GLOBAL, ALO_STACK_INDEX_EMPTY, -1, options);
	alo_pop(env, -2);

    if (fname != null) close(ctx.file);
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
	char const* name;
	char const* file;
	a_u32 line;
} Trace;

#define TRACE_UNKNOWN_FILE "?"
#define TRACE_NATIVE_FILE "[C]"

static void trace_fill(a_henv env, Frame* frame, Trace* trace) {
	GFun* fun = ai_dbg_get_func(env, frame);
	assume(fun != null, "cannot trace root frame.");
	if (!(fun->flags & FUN_FLAG_NATIVE)) {
		GProto* proto = fun->proto;
		trace->name = proto->dbg_name != null ? str2ntstr(proto->dbg_name) : null;
		trace->file = proto->dbg_file != null ? str2ntstr(proto->dbg_file) : TRACE_UNKNOWN_FILE;
		trace->line = ai_dbg_get_line(proto, frame->pc - 1);
	}
	else {
		trace->name = null;
		trace->file = TRACE_NATIVE_FILE;
		trace->line = 0;
	}
}

static Frame* virtual_unwind(a_henv env, a_usize level) {
    Frame* frame = env->frame;

    loop {
        if (frame->prev == null) {
            return null;
        }
        else if (level == 0) {
            return frame;
        }

        frame = frame->prev;
        level -= 1;
    }
}

static a_msg l_wrap_error(a_henv env, a_ilen id, a_usize level, a_usize limit, Buf* buf) {
    Trace trace;
	Value* err = api_wrslot(env, id);
    Frame* frame = virtual_unwind(env, level);
    if (frame == null) return ALO_EINVAL;

    trace_fill(env, frame, &trace);

    if (trace.file != null) {
        if (trace.line != 0) {
            try (ai_buf_nputfs(env, buf, "%s:%u: ", trace.file, trace.line));
        }
        else {
            try (ai_buf_nputfs(env, buf, "%s: ", trace.file));
        }
    }

    switch (v_get_tag(*err)) {
        case T_STR: {
            GStr* str = v_as_str(*err);
            try (ai_buf_nputls(env, buf, str->ptr, str->len));
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
            if (trace.line != 0) {
                try (ai_buf_nputfs(env, buf, "at %s:%u", trace.file, trace.line));
            }
            else {
                try (ai_buf_nputfs(env, buf, "at %s", trace.file));
            }

            if (trace.name != null) {
                try (ai_buf_nputfs(env, buf, " (%s)", trace.name));
            }

			if (limit-- == 0) {
				try (ai_buf_nputs(env, buf, "\n\t..."));
				break;
			}
			else if (frame->flags & FRAME_FLAG_TAIL_CALL) {
				try (ai_buf_nputs(env, buf, "\n\t... (tail call)"));
				if (limit > 1) {
					limit -= 1;
				}
			}

            frame = frame->prev;
            if (frame->prev == null)
                break;
            trace_fill(env, frame, &trace);

			try (ai_buf_nputs(env, buf, "\n\t"));
        }
    }

	GStr* str = ai_str_get_or_new(env, buf->ptr, buf->len);
	v_set_str(env, err, str);
	return ALO_SOK;
}

a_msg aloL_traceerror(a_henv env, a_ilen id, a_usize level, a_usize limit) {
	if (env->frame->prev != null) {
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
		assume(e->name != null, "missing field dbg_name.");

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
    a_usize size;
    a_byte body[];
} GBlock;

#define block_size(s) align_to(sizeof(GBlock) + (s), sizeof(a_usize))

static void block_drop(Global* gbl, GBlock* self) {
    ai_mem_dealloc(gbl, self, block_size(self->size));
}

static void block_mark(Global* gbl, GBlock* self) {
    ai_gc_trace_work(gbl, block_size(self->size));
}

static VTable const block_vtable = {
    .stencil = V_STENCIL(T_USER),
    .tag = ALO_TUSER,
    .slots = {
        [vfp_slot(drop)] = block_drop,
        [vfp_slot(mark)] = block_mark
    }
};

void* aloL_newblk(a_henv env, a_usize s) {
    api_check_slot(env, 1);

    GBlock* self = ai_mem_alloc(env, block_size(s));
    self->vptr = &block_vtable;
    self->size = s;
    ai_gc_register_object(env, self);

    v_set_obj(env, api_incr_stack(env), self);

    return self->body;
}

static_assert(offsetof(aloL_Buf, ptr) == offsetof(Buf, ptr));
static_assert(offsetof(aloL_Buf, len) == offsetof(Buf, len));
static_assert(offsetof(aloL_Buf, cap) == offsetof(Buf, cap));

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
    at_buf_putls(env, self, str->ptr, str->len);
    api_post_decr_stack(env);
}

void aloL_bufstr(a_henv env, aloL_Buf* b) {
    GBuf* self = from_buff(env, b);
    alo_pushstr(env, self->ptr, self->len);
}

void aloL_openlibs(a_henv env) {
    typedef struct {
        char const* name;
        void (*init)(a_henv);
    } LibEntry;

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
		LibEntry const* entry = &entries[i];
        alo_pushntstr(env, entry->name);
        (*entry->init)(env);
        alo_set(env, ALO_STACK_INDEX_GLOBAL);
	}
}
