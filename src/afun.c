/**
 *@file afun.c
 */

#define afun_c_
#define ALO_LIB

#include "amem.h"
#include "agc.h"

#include "afun.h"

/**
 ** Hint function size with the minimum value.
 *@param info the information of function metadata.
 */
static a_usize fmeta_hint_size(FnMetaCreateInfo* info) {
	a_usize size = sizeof(GFunMeta) +
				   sizeof(Value) * info->_nconst +
				   sizeof(a_insn) * info->_ninsn +
				   sizeof(CapInfo) * info->_ncap +
				   sizeof(GFunMeta*) * info->_nsub;
	if (info->_flags._fline) {
		size += sizeof(LineInfo) * info->_nline;
	}
	else {
		size += sizeof(LineInfo);
	}
	if (info->_flags._fname) {
		size += sizeof(LocalInfo) * info->_nlocal +
				sizeof(GStr*) * info->_ncap;
	}
	if (info->_flags._froot) {
		assume(info->_ncap <= 1, "bad capture count.");
		size += sizeof(GFun);
		if (info->_ncap > 0) {
			size += sizeof(Value);
			if (!info->_flags._fconst_env) {
				size += sizeof(GCap);
			}
		}
	}
	return size;
}

GFunMeta* ai_fmeta_xalloc(a_henv env, FnMetaCreateInfo* info) {
	a_usize total_size = fmeta_hint_size(info);

    GFunMeta* self = ai_mem_xalloc(env, total_size);
    if (self == null) return null;
    memclr(self, total_size);

    self->_tid = T_FUNC;
    self->_meta = &G(env)->_metas._fmeta;
	self->_vtable = ai_fun_vtable;
    self->_len = total_size;
    self->_nconst = info->_nconst;
    self->_ninsn = info->_ninsn;
    self->_nsub = info->_nsub;
    self->_nlocal = info->_nlocal;
    self->_ncap = info->_ncap;
	self->_nline = info->_nline;
    self->_nstack = info->_nstack;

	a_usize addr = addr_of(self)
			+ sizeof(GFunMeta)
			+ sizeof(Value) * info->_nconst;

	self->_subs = ptr_of(GFunMeta*, addr);
	addr += sizeof(GFunMeta*) * info->_nsub;

	if (info->_flags._fline) {
		self->_lines = ptr_of(LineInfo, addr);
		addr += sizeof(LineInfo) * info->_nline;
	}
	else {
		self->_lines = ptr_of(LineInfo, addr);
		addr += sizeof(LineInfo);

		self->_lines[0] = new(LineInfo) { ._end = UINT32_MAX, ._lineno = 0 };
	}

	if (info->_flags._fname) {
		self->_locals = ptr_of(LocalInfo, addr);
		addr += sizeof(LocalInfo) * info->_nlocal;

		self->_cap_names = ptr_of(GStr*, addr);
		addr += sizeof(GStr*) * info->_ncap;
	}

	if (info->_flags._froot) {
		GFun* fun = ptr_of(GFun, addr);
		addr += sizeof(GFun);

		if (info->_ncap > 0) {
			addr += sizeof(Value);

			if (!info->_flags._fconst_env) {
				GCap* cap = ptr_of(GCap, addr);
				addr += sizeof(GCap);

				*cap = new(GCap) {
					._meta = &G(env)->_metas._cap,
					._ptr = &cap->_slot,
					._slot = v_of_nil()
				};

				v_set(G(env), &fun->_capval[0], v_of_ref(cap));
			}
			else {
				v_setx(&fun->_capval[0], v_of_nil());
			}
		}

		fun->_meta = g_cast(GMeta, self);
		fun->_len = info->_ncap;

		self->_cache = fun;
	}

	self->_caps = ptr_of(CapInfo, addr);
	addr += sizeof(CapInfo) * self->_ncap;

	self->_code = ptr_of(a_insn, addr);
	addr += sizeof(a_insn) * self->_ninsn;

	assume(addr_of(self) + total_size == addr);

	return self;
}

static GFun* l_fun_new(a_henv env, a_u32 ncap) {
	GFun* self = ai_mem_alloc(env, sizeof(GFun) + sizeof(Value) * ncap);
	self->_len = ncap;
	return self;
}

GFun* ai_cfun_create(a_henv env, a_cfun hnd, a_u32 ncap, Value const* pcap) {
	GFun* self = l_fun_new(env, ncap + 1);

	self->_meta = g_cast(GMeta, &G(env)->_metas._cfun);
	v_setx(self->_capval, bcast(Value, hnd));
	v_cpy_multi(G(env), self->_capval + 1, pcap, ncap);

	ai_gc_register_object(env, self);

	return self;
}

static a_bool cap_is_closed(GCap* self) {
	return self->_ptr == &self->_slot;
}

static void cap_splash(Global* g, GCap* self) {
	ai_gc_trace_markv(g, self->_ptr);
}

static void cap_destruct(Global* g, GCap* self) {
	assume(cap_is_closed(self));
	ai_mem_dealloc(g, self, sizeof(GCap));
}

static void fun_splash(Global* g, GFun* self) {
    ai_gc_trace_mark(g, self->_meta);
    a_usize ncap = self->_len;
    for (a_usize i = 0; i < ncap; ++i) {
        ai_gc_trace_markv(g, &self->_capval[i]);
    }
    g->_mem_work -= cast(a_isize, sizeof(GFun) + sizeof(Value) * self->_len);
}

static void fun_destruct(Global* g, GFun* self) {
    ai_mem_dealloc(g, self, sizeof(GFun) + sizeof(Value) * self->_len);
}

static void fmeta_splash(Global* g, GFunMeta* self) {
    for (a_u32 i = 0; i < self->_nconst; ++i) {
        ai_gc_trace_markv(g, &self->_consts[i]);
    }
	if (self->_dbg_file != null) {
		ai_gc_trace_mark(g, self->_dbg_file);
	}
	if (self->_locals != null) {
		for (a_u32 i = 0; i < self->_nlocal; ++i) {
			ai_gc_trace_mark(g, self->_locals[i]._name);
		}
		assume(self->_cap_names != null);
		for (a_u32 i = 0; i < self->_ncap; ++i) {
			ai_gc_trace_mark(g, self->_cap_names[i]);
		}
	}
    g->_mem_work -= self->_len;
}

void ai_fmeta_destruct(Global* g, GFunMeta* self) {
    ai_mem_dealloc(g, self, self->_len);
}

VTable const ai_cap_vtable = {
	._splash = fpcast(a_fp_splash, cap_splash),
	._destruct = fpcast(a_fp_destruct, cap_destruct)
};

VTable const ai_fun_vtable = {
	._splash = fpcast(a_fp_splash, fun_splash),
	._destruct = fpcast(a_fp_destruct, fun_destruct)
};

VTable const ai_fmeta_vtable = {
	._splash = fpcast(a_fp_splash, fmeta_splash),
	._destruct = fpcast(a_fp_destruct, ai_fmeta_destruct)
};