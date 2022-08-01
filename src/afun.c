/**
 *@file afun.c
 */

#define afun_c_

#include <string.h>

#include "amem.h"
#include "agc.h"

#include "afun.h"

GFunMeta* ai_fun_xalloc(a_henv env, FnInfo* info) {
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
    self->_nstack = info->_nstack;

	a_usize addr = addr_of(self + 1);

	addr += sizeof(Value) * self->_nconst;
	self->_insns = ptr_of(a_insn, addr);

	addr += sizeof(a_insn) * self->_ninsn;
	self->_subs = ptr_of(GFunMeta*, addr);

	return self;
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
    g->_mem_work -= self->_len;
}

void ai_fun_meta_destruct(Global* g, GFunMeta* self) {
    ai_mem_dealloc(g, self, self->_len);
}
