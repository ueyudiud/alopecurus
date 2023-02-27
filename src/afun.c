/**
 *@file afun.c
 */

#define afun_c_
#define ALO_LIB

#include "abc.h"
#include "amem.h"
#include "agc.h"

#include "afun.h"

static VTable const fun_vtable;
static VTable const proto_vtable;

static a_usize fun_size(a_usize ncap) {
	return sizeof(GFun) + sizeof(Value) * ncap;
}

static a_usize proto_size(ProtoDesc* desc) {
	a_usize size = sizeof(GProto) +
				   sizeof(Value) * desc->_nconst +
				   sizeof(a_insn) * desc->_ninsn +
				   sizeof(CapInfo) * desc->_ncap +
				   sizeof(GProto*) * desc->_nsub;
	if (desc->_flags._fdebug) {
		size += sizeof(LineInfo) * desc->_nline +
				sizeof(LocalInfo) * desc->_nlocal +
				sizeof(GStr*) * desc->_ncap;
	}
	else {
		size += sizeof(LineInfo);
	}
	if (desc->_flags._froot) {
		assume(desc->_ncap <= 1, "bad capture count.");
		size += fun_size(desc->_ncap);
	}
	return size;
}

GProto* ai_proto_xalloc(a_henv env, ProtoDesc* desc) {
	a_usize total_size = proto_size(desc);

    GProto* self = ai_mem_xalloc(env, total_size);
    if (self == null) return null;
    memclr(self, total_size);

	self->_vtable = &proto_vtable;
    self->_len = total_size;
    self->_nconst = desc->_nconst;
    self->_ninsn = desc->_ninsn;
    self->_nsub = desc->_nsub;
    self->_nlocal = desc->_nlocal;
    self->_ncap = desc->_ncap;
	self->_nline = desc->_nline;
    self->_nstack = desc->_nstack;

	a_usize addr = addr_of(self)
			+ sizeof(GProto)
			+ sizeof(Value) * desc->_nconst;

	self->_subs = ptr_of(GProto*, addr);
	addr += sizeof(GProto*) * desc->_nsub;

	if (desc->_flags._fdebug) {
		self->_dbg_lines = ptr_of(LineInfo, addr);
		addr += sizeof(LineInfo) * desc->_nline;

		self->_dbg_locals = ptr_of(LocalInfo, addr);
		addr += sizeof(LocalInfo) * desc->_nlocal;

		self->_dbg_cap_names = ptr_of(GStr*, addr);
		addr += sizeof(GStr*) * desc->_ncap;
	}
	else {
		self->_dbg_lndef = self->_dbg_lnldef = 0;

		self->_dbg_lines = ptr_of(LineInfo, addr);
		addr += sizeof(LineInfo);

		self->_dbg_lines[0] = new(LineInfo) { ._end = UINT32_MAX, ._lineno = 0 };
	}

	if (desc->_flags._froot) {
		GFun* fun = ptr_of(GFun, addr);
		addr += sizeof(GFun) + sizeof(Value) * desc->_ncap;

		for (a_u32 i = 0; i < desc->_ncap; ++i) {
			v_set_nil(&fun->_capval[i]);
		}

		fun->_vtable = &fun_vtable;
		fun->_proto = self;
		fun->_len = desc->_ncap;

		self->_cache = fun;
	}

	self->_caps = ptr_of(CapInfo, addr);
	addr += sizeof(CapInfo) * self->_ncap;

	self->_code = ptr_of(a_insn, addr);
	addr += sizeof(a_insn) * self->_ninsn;

	assume(addr_of(self) + total_size == addr);

	return self;
}

static GFun* fun_new(a_henv env, GProto* proto, a_u32 ncap) {
	GFun* self = ai_mem_alloc(env, fun_size(ncap));
	self->_vtable = &fun_vtable;
	self->_proto = proto;
	self->_len = ncap;
	return self;
}

GFun* ai_cfun_create(a_henv env, a_cfun hnd, a_u32 ncap, Value const* pcap) {
	static a_insn const codes[] = {
		bc_make_iabc(BC_FC, 0, 0, 0)
	};

	static GProto const proto = {
		._nstack = ALOI_INIT_FRAME_STACKSIZE,
		._nparam = 0,
		._flags = PROTO_FLAG_VARARG,
		._cache = null,
		._ninsn = 1,
		._code = cast(a_insn*, codes)
	};

	GFun* self = fun_new(env, cast(GProto*, &proto), ncap + 1);

	v_cpy_multi(env, self->_capval, pcap, ncap);
	v_setx(self->_capval + ncap, bcast(Value, hnd));

	ai_gc_register_object(env, self);

	return self;
}

static Value l_load_capture(a_henv env, CapInfo* info, Value* up, CapVal** now, Value* stack) {
	if (info->_fup) {
		return up[info->_reg];
	}
	else if (info->_fro) {
		return stack[info->_reg];
	}
	else {
		Value* dst = stack + info->_reg;
		CapVal* cap;
		while ((cap = *now) != null && cap->_ptr > dst) {
			now = &cap->_next;
		}
		if (cap == null || cap->_ptr < dst) {
			CapVal* cap2 = ai_mem_alloc(env, sizeof(CapVal));

			cap2->_ptr = dst;
			cap2->_next = cap;

			cap = cap2;
		}
		cap->_rc += 1;
		return v_of_cap(cap);
	}
}

GFun* ai_fun_new(a_henv env, GProto* proto, Frame* frame) {
	Value* base = env->_stack._bot;
	GFun* parent = v_as_func(base[-1]);
	CapInfo* infos = proto->_caps;

	a_u32 len = proto->_ncap;

	GFun* self = fun_new(env, proto, len);
	self->_vtable = &fun_vtable;
	for (a_u32 i = 0; i < len; ++i) {
		Value capval = l_load_capture(env, &infos[i], parent->_capval, &frame->_caps, base);
		v_setx(&self->_capval[i], capval);
	}

	ai_gc_register_object(env, self);

	return self;
}

static a_bool cap_is_closed(CapVal* self) {
	return self->_ptr == &self->_slot;
}

static void cap_close(a_henv env, CapVal* self) {
	v_cpy(env, &self->_slot, self->_ptr);
	self->_ptr = &self->_slot;
}

static void cap_delete(Global* g, CapVal* self) {
	ai_mem_dealloc(g, self, sizeof(CapVal));
}

static void fun_splash(Global* g, GFun* self) {
    ai_gc_trace_mark(g, self->_proto);
    a_usize len = self->_len;
    for (a_usize i = 0; i < len; ++i) {
		Value v = self->_capval[i];
		if (v_is_cap(v)) {
			ai_gc_trace_mark_val(g, *v_as_cap(v)->_ptr);
		}
		else {
			ai_gc_trace_mark_val(g, v);
		}
    }
	ai_gc_trace_work(g, sizeof(GFun) + sizeof(Value) * self->_len);
}

static void fun_delete(Global* g, GFun* self) {
	for (a_u32 i = 0; i < self->_len; ++i) {
		Value v = self->_capval[i];
		if (v_is_cap(v)) {
			CapVal* cap = v_as_cap(v);
			assume(cap->_rc > 0);
			if (--cap->_rc == 0 && cap_is_closed(cap)) {
				cap_delete(g, cap);
			}
		}
	}
    ai_mem_dealloc(g, self, sizeof(GFun) + sizeof(Value) * self->_len);
}

static void proto_splash(Global* g, GProto* self) {
    for (a_u32 i = 0; i < self->_nconst; ++i) {
		ai_gc_trace_mark_val(g, self->_consts[i]);
    }
	if (self->_dbg_file != null) {
		ai_gc_trace_mark(g, self->_dbg_file);
	}
	if (self->_dbg_locals != null) {
		for (a_u32 i = 0; i < self->_nlocal; ++i) {
			ai_gc_trace_mark(g, self->_dbg_locals[i]._name);
		}
		assume(self->_dbg_cap_names != null);
		for (a_u32 i = 0; i < self->_ncap; ++i) {
			ai_gc_trace_mark(g, self->_dbg_cap_names[i]);
		}
	}
	ai_gc_trace_work(g, self->_len);
}

void ai_proto_delete(Global* g, GProto* self) {
    ai_mem_dealloc(g, self, self->_len);
}

void ai_cap_close(a_henv env, CapVal** pcap, Value const* base) {
	CapVal* cap;
	while ((cap = *pcap) != null && cap->_ptr >= base) {
		CapVal* next = cap->_next;
		if (cap->_rc > 0) {
			cap_close(env, cap);
		}
		else {
			cap_delete(G(env), cap);
		}
		*pcap = next;
	}
}

static VTable const fun_vtable = {
	._tid = T_FUNC,
	._api_tag = ALO_TFUNC,
	._repr_id = REPR_FUNC,
	._flags = VTABLE_FLAG_IDENTITY_EQUAL,
	._name = "func",
	._splash = fpcast(a_fp_splash, fun_splash),
	._delete = fpcast(a_fp_delete, fun_delete)
};

static VTable const proto_vtable = {
	._tid = T_USER_TEQ,
	._api_tag = ALO_TUSER,
	._flags = VTABLE_FLAG_IDENTITY_EQUAL,
	._splash = fpcast(a_fp_splash, proto_splash),
	._delete = fpcast(a_fp_delete, ai_proto_delete)
};
