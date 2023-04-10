/**
 *@file afun.c
 */

#define afun_c_
#define ALO_LIB

#include "abc.h"
#include "aenv.h"
#include "amem.h"
#include "agc.h"

#include "afun.h"

static VTable const afun_vtable;
static VTable const cfun_vtable;
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
		size += fun_size(desc->_ncap) + desc->_ncap * sizeof(RcCap);
	}
	return size;
}

GProto* ai_proto_xalloc(a_henv env, ProtoDesc* desc) {
	a_usize total_size = proto_size(desc);

    GProto* self = ai_mem_nalloc(env, total_size);
    if (self == null) return null;
    memclr(self, total_size);

	self->_vptr = &proto_vtable;
    self->_size = total_size;
    self->_nconst = desc->_nconst;
    self->_ninsn = desc->_ninsn;
    self->_nsub = desc->_nsub;
    self->_nlocal = desc->_nlocal;
    self->_ncap = desc->_ncap;
	self->_nline = desc->_nline;
    self->_nstack = desc->_nstack;

	a_usize addr = addr_of(self) + sizeof(GProto);

	/* self->_subs = ptr_of(GProto*, addr); */
	addr += sizeof(GProto*) * desc->_nsub;

	self->_consts = ptr_of(Value, addr);
	addr += sizeof(Value) * desc->_nconst;

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
		addr += fun_size(desc->_ncap);

		fun->_vptr = &afun_vtable;
		fun->_proto = self;
		fun->_len = desc->_ncap;
		if (desc->_ncap > 0) {
			RcCap* cap = ptr_of(RcCap, addr);
			addr += sizeof(RcCap);

			cap->_ptr = &cap->_slot;
			cap->_rc_and_fopen = 4;

			fun->_caps[0] = cap;
		}

		self->_cache = fun;
	}

	self->_caps = ptr_of(CapInfo, addr);
	addr += sizeof(CapInfo) * self->_ncap;

	self->_code = ptr_of(a_insn, addr);
	addr += sizeof(a_insn) * self->_ninsn;

	assume(addr_of(self) + total_size == addr);

	return self;
}

static GFun* fun_new(a_henv env, VTable const* vptr, GProto* proto, a_u32 ncap) {
	GFun* self = ai_mem_alloc(env, fun_size(ncap));
	self->_vptr = vptr;
	self->_proto = proto;
	self->_len = ncap;
	self->_sym = 0;
	return self;
}

GFun* ai_cfun_create(a_henv env, a_cfun hnd, a_u32 ncap, Value const* pcap) {
	static a_insn const l_codes[] = {
		bc_make_i(BC_FC)
	};

	static GThinProto const l_proto = {
		._nstack = ALOI_INIT_CFRAME_STACKSIZE,
		._nparam = 0,
		._flags = FUN_FLAG_VARARG | FUN_FLAG_NATIVE,
		._code = cast(a_insn*, l_codes)
	};

	GFun* self = fun_new(env, &cfun_vtable, g_cast(GProto, &l_proto), ncap + 1);

	v_cpy_all(env, self->_vals, pcap, ncap);
	v_set_raw(self->_vals + ncap, bit_cast(Value, hnd));

	ai_gc_register_object(env, self);

	return self;
}

static RcCap* l_alloc_capture(a_henv env) {
	Global* g = G(env);
	RcCap* cap = g->_cap_cache;
	if (cap != null) {
		g->_cap_cache = cap->_next;
		return cap;
	}
	return ai_mem_alloc(env, sizeof(RcCap));
}

static RcCap* l_load_capture_at(a_henv env, RcCap** now, Value* dst) {
	RcCap* cap;
	while ((cap = *now) != null && cap->_ptr > dst) {
		now = &cap->_next;
	}
	if (cap == null || cap->_ptr < dst) {
		RcCap* self = l_alloc_capture(env);

		*self = new(RcCap) {
			._ptr = dst,
			._rc_and_fopen = 3,
			._next = cap,
			._flags = 0
		};

		*now = cap = self;
	}
	else {
		cap->_rc_and_fopen += 2;
	}
	return cap;
}

static RcCap* l_load_capture(a_henv env, CapInfo* info, RcCap** up, RcCap** now, Value* stack) {
	if (info->_fup) {
		return up[info->_reg];
	}
	else {
		return l_load_capture_at(env, now, stack + info->_reg);
	}
}

void ai_cap_mark_tbc(a_henv env, Frame* frame, Value* ptr) {
	RcCap* cap = l_load_capture_at(env, &frame->_caps, ptr);
	cap->_ftbc = true;
}

GFun* ai_fun_new(a_henv env, GProto* proto, Frame* frame) {
	Value* base = ai_stk_bot(env);
	GFun* parent = v_as_func(base[-1]);
	CapInfo* infos = proto->_caps;

	a_u32 len = proto->_ncap;

	GFun* self = fun_new(env, &afun_vtable, proto, len);

	for (a_u32 i = 0; i < len; ++i) {
		self->_caps[i] = l_load_capture(env, &infos[i], parent->_caps, &frame->_caps, base);
	}

	ai_gc_register_object(env, self);

	return self;
}

static a_bool cap_is_closed(RcCap* self) {
	return (self->_rc_and_fopen & 1) == 0;
}

static void cap_mark(Global* g, RcCap* self) {
	if (cap_is_closed(self) || g->_gcstep == GCSTEP_PROPAGATE_ATOMIC) {
		ai_gc_trace_mark_val(g, *self->_ptr);
	}
	else {
		self->_ftouch = true;
	}
}

void ai_cap_really_drop(Global* g, RcCap* self) {
	ai_mem_dealloc(g, self, sizeof(RcCap));
}

static void cap_drop(Global* g, RcCap* self) {
	self->_next = g->_cap_cache;
	g->_cap_cache = self;
}

static void cap_close_value(a_henv env, RcCap* self) {
	if (self->_ftbc) {
		a_hobj obj = v_as_obj(*self->_ptr);
		g_vcall(env, obj, close);
		self->_ftbc = false;
	}
}

static void cap_release(Global* g, RcCap* self) {
	assume(self->_rc_and_fopen >= 2);
	self->_rc_and_fopen -= 2;
	if (self->_rc_and_fopen == 0) {
		cap_close_value(ai_env_mainof(g), self);
		cap_drop(g, self);
	}
}

static void cap_close_internal(a_henv env, RcCap* restrict self) {
	v_cpy(env, &self->_slot, self->_ptr);
	self->_ptr = &self->_slot;

	if (self->_ftouch) {
		ai_gc_trace_mark_val(G(env), self->_slot);
	}
}

void ai_cap_soft_close(a_henv env, RcCap* self) {
	assume(!cap_is_closed(self));
	a_u32 count = self->_rc_and_fopen - 1;
	if (count > 0) {
		self->_rc_and_fopen = count;
		cap_close_internal(env, self);
	}
	else {
		cap_close_value(env, self);
		cap_drop(G(env), self);
	}
}

void ai_cap_hard_close(a_henv env, RcCap* self) {
	assume(!cap_is_closed(self));
	a_u32 count = self->_rc_and_fopen - 1;
	if (count > 0) {
		self->_rc_and_fopen = count;
		cap_close_internal(env, self);
	}
	else {
		cap_close_value(env, self);
		ai_cap_really_drop(G(env), self);
	}
}

static void afun_mark(Global* g, GFun* self) {
	ai_gc_trace_mark(g, self->_proto);
	a_u32 len = self->_len;
	for (a_u32 i = 0; i < len; ++i) {
		cap_mark(g, self->_caps[i]);
	}
	ai_gc_trace_work(g, fun_size(self->_len));
}

static void afun_drop(Global* g, GFun* self) {
	for (a_u32 i = 0; i < self->_len; ++i) {
		cap_release(g, self->_caps[i]);
	}
	ai_mem_dealloc(g, self, fun_size(self->_len));
}

static void cfun_mark(Global* g, GFun* self) {
	a_u32 len = self->_len;
	for (a_u32 i = 0; i < len - 1; ++i) {
		ai_gc_trace_mark_val(g, self->_vals[i]);
	}
	ai_gc_trace_work(g, fun_size(len));
}

static void cfun_drop(Global* g, GFun* self) {
	ai_mem_dealloc(g, self, fun_size(self->_len));
}

static void proto_mark(Global* g, GProto* self) {
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
	ai_gc_trace_work(g, self->_size);
}

void ai_proto_drop(Global* g, GProto* self) {
	ai_mem_dealloc(g, self, self->_size);
}

static VTable const afun_vtable = {
	._mask = V_MASKED_TAG(T_FUNC),
	._iname = env_type_iname(_func),
	._sname = "func",
	._base_size = sizeof(GFun),
	._elem_size = sizeof(RcCap*),
	._vfps = (a_vslot[]) {
		vfp_def(drop, afun_drop),
		vfp_def(mark, afun_mark)
	}
};

static VTable const cfun_vtable = {
	._mask = V_MASKED_TAG(T_FUNC),
	._iname = env_type_iname(_func),
	._sname = "func",
	._base_size = sizeof(GFun),
	._elem_size = sizeof(Value),
	._flags = VTABLE_FLAG_NONE,
	._vfps = (a_vslot[]) {
		vfp_def(drop, cfun_drop),
		vfp_def(mark, cfun_mark)
	}
};

static VTable const proto_vtable = {
	._mask = V_MASKED_TAG(T_CUSER),
	._base_size = 0,
	._elem_size = 1,
	._flags = VTABLE_FLAG_NONE,
	._vfps = (a_vslot[]) {
		vfp_def(drop, ai_proto_drop),
		vfp_def(mark, proto_mark)
	}
};
