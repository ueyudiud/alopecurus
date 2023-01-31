/**
 *@file afun.c
 */

#define afun_c_

#include "amem.h"
#include "agc.h"

#include "afun.h"

GFunMeta* ai_fun_xalloc(a_henv env, FnCreateInfo* info) {
    GFunMeta* self = ai_mem_xalloc(env, info->_size);
    if (self == null) return null;
    memclr(self, info->_size);
    self->_tid = T_FUNC;
    self->_meta = &G(env)->_metas._fmeta;
    self->_splash = cast(a_fp_splash, ai_fun_meta_splash);
    self->_len = info->_size;
    self->_nconst = info->_nconst;
    self->_ninsn = info->_ninsn;
    self->_nsub = info->_nsub;
    self->_nlocal = info->_nlocal;
    self->_ncap = info->_ncap;
	self->_nline = info->_nline;
    self->_nstack = info->_nstack;

	a_usize addr = addr_of(self + 1);
	addr += sizeof(Value) * self->_nconst;

	self->_insns = ptr_of(a_insn, addr);
	addr += sizeof(a_insn) * self->_ninsn;

	self->_subs = ptr_of(GFunMeta*, addr);
	addr += sizeof(GFunMeta*) * self->_nsub;

	if (info->_debug._fline) {
		self->_lines = ptr_of(LineInfo, addr);
		addr += sizeof(LineInfo) * self->_nline;
	}
	else {
		self->_lines = ptr_of(LineInfo, addr);
		addr += sizeof(LineInfo);

		self->_lines[0] = new(LineInfo) { ._end = UINT32_MAX, ._lineno = 0 };
	}

	if (info->_debug._fname) {
		self->_locals = ptr_of(LocalInfo, addr);
		addr += sizeof(LocalInfo) * self->_nlocal;

		self->_cap_names = ptr_of(GStr*, addr);
		addr += sizeof(GStr*) * self->_ncap;
	}

	self->_caps = ptr_of(CapInfo, addr);

	return self;
}

static GFun* l_fun_new(a_henv env, a_u32 ncap) {
	GFun* fun = ai_mem_alloc(env, sizeof(GFun) + sizeof(Value) * ncap);
	fun->_len = ncap;
	return fun;
}

GFun* ai_cfun_create(a_henv env, a_cfun hnd, a_u32 ncap, Value const* pcap) {
	GFun* fun = l_fun_new(env, ncap + 1);
	v_setx(fun->_capval, new(Value) { ._u = cast(a_u64, cast(a_usize, hnd)) });
	v_bmcpy(G(env), fun->_capval + 1, pcap, ncap + 1);
	return fun;
}

void ai_fun_splash(Global* g, GFun* self) {
    ai_gc_trace_mark(g, self->_meta);
    a_usize ncap = self->_len;
    for (a_usize i = 0; i < ncap; ++i) {
        ai_gc_trace_markv(g, &self->_capval[i]);
    }
    g->_mem_work -= cast(a_isize, sizeof(GFun) + sizeof(Value) * self->_len);
}

void ai_fun_destruct(Global* g, GFun* self) {
    ai_mem_dealloc(g, self, sizeof(GFun) + sizeof(Value) * self->_len);
}

void ai_fun_meta_splash(Global* g, GFunMeta* self) {
    for (a_u32 i = 0; i < self->_nconst; ++i) {
        ai_gc_trace_markv(g, &self->_consts[i]);
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

void ai_fun_meta_destruct(Global* g, GFunMeta* self) {
    ai_mem_dealloc(g, self, self->_len);
}
