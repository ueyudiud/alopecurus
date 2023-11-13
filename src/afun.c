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
static VTable const uniq_afun_vtable;
static VTable const cfun_vtable;
static VTable const proto_vtable;
static VTable const uniq_proto_vtable;

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
	if (desc->_flags._funiq) {
		size += fun_size(desc->_ncap);
	}
	return size;
}

GProto* ai_proto_xalloc(a_henv env, ProtoDesc* desc) {
	a_usize total_size = proto_size(desc);

    void* blk = ai_mem_nalloc(env, sizeof(GcHead) + total_size);
    if (blk == null) return null;
    memclr(blk, total_size);

    GProto* self = g_cast(GProto, g_biased(blk));
	self->_vptr = desc->_flags._funiq ? &uniq_proto_vtable : &proto_vtable;
    self->_size = total_size;
    self->_nconst = desc->_nconst;
    self->_ninsn = desc->_ninsn;
    self->_nsub = desc->_nsub;
    self->_nlocal = desc->_nlocal;
    self->_ncap = desc->_ncap;
	self->_nline = desc->_nline;
    self->_nstack = desc->_nstack;
	self->_nparam = desc->_nparam;

	a_usize addr = ptr2int(self) + sizeof(GProto);

	/* self->_subs = int2ptr(GProto*, addr); */
	addr += sizeof(GProto*) * desc->_nsub;

	self->_consts = int2ptr(Value, addr);
	addr += sizeof(Value) * desc->_nconst;

	if (desc->_flags._fdebug) {
		self->_dbg_lines = int2ptr(LineInfo, addr);
		addr += sizeof(LineInfo) * desc->_nline;

		self->_dbg_locals = int2ptr(LocalInfo, addr);
		addr += sizeof(LocalInfo) * desc->_nlocal;

		self->_dbg_cap_names = int2ptr(GStr*, addr);
		addr += sizeof(GStr*) * desc->_ncap;
	}
	else {
		self->_dbg_lndef = self->_dbg_lnldef = 0;

		self->_dbg_lines = int2ptr(LineInfo, addr);
		addr += sizeof(LineInfo);

        init(&self->_dbg_lines[0]) {
            ._end = UINT32_MAX,
            ._lineno = 0
        };
	}

	if (desc->_flags._funiq) {
        /* Initialize unique function for this prototype. */

		GFun* fun = int2ptr(GFun, addr);
		addr += fun_size(desc->_ncap);

		fun->_vptr = &uniq_afun_vtable;
		fun->_proto = self;
		fun->_len = desc->_ncap;

		self->_cache = fun;
	}

	self->_caps = int2ptr(CapInfo, addr);
	addr += sizeof(CapInfo) * self->_ncap;

	self->_code = int2ptr(a_insn, addr);
	addr += sizeof(a_insn) * self->_ninsn;

	assume(ptr2int(self) + total_size == addr);

	return self;
}

GFun* ai_cfun_create(a_henv env, a_cfun hnd, a_u32 ncap, Value const* pcap) {
	GFun* self = ai_mem_gnew(env, GFun, fun_size(ncap));

	self->_vptr = &cfun_vtable;
	self->_len = ncap;
	self->_fptr = hnd;
	self->_flags = FUN_FLAG_NATIVE | FUN_FLAG_VARARG;
	self->_fname = 0;

	v_cpy_all(env, self->_vals, pcap, ncap);

	ai_gc_register_object(env, self);

	return self;
}

static RcCap* cap_nalloc(a_henv env) {
	Global* gbl = G(env);
	RcCap* cap = gbl->_cap_cache;
	if (cap != null) {
		gbl->_cap_cache = cap->_next;
		return cap;
	}
	return ai_mem_nalloc(env, sizeof(RcCap));
}

static RcCap* cap_nload_from_stack(a_henv env, Value* pv) {
	/* Check stack value pointer. */
	check_in_stack(env, pv);
	v_check_alive(env, *pv);

	RcCap* cap;
	RcCap** now = &env->_open_caps;

	while ((cap = *now) != null && cap->_ptr > pv) {
		now = &cap->_next;
	}
	
	if (cap == null || cap->_ptr < pv) {
		RcCap* self = cap_nalloc(env);

        if (self == null) return null;

		self->_ptr = pv;
		self->_rc = 1;
		self->_next = cap;
		self->_flags = 0;

		*now = cap = self;
	}
	else {
		cap->_rc += 1;
	}
	return cap;
}

static RcCap* cap_load(a_henv env, CapInfo const* info, RcCap** up, Value* stack) {
	if (info->_fup) {
        RcCap* cap = up[info->_reg];
        cap->_rc += 1;
		return cap;
	}
	else {
		RcCap* cap = cap_nload_from_stack(env, &stack[info->_reg]);
        if (unlikely(cap == null)) ai_mem_nomem(env);
        return cap;
	}
}

static void v_close(a_henv env, Value v) {
    v_vcall(env, v, close);
}

RcCap* ai_cap_new(a_henv env) {
    RcCap* self = cap_nalloc(env);

    if (unlikely(self == null)) ai_mem_nomem(env);

    self->_ptr = &self->_slot;
    self->_rc = 1;

    return self;
}

void ai_cap_mark_tbc(a_henv env, Value* pv) {
	RcCap* cap = cap_nload_from_stack(env, pv);
    if (!cap->_ftbc) {
        if (unlikely(cap == null)) {
            /* Call close function immediately if cannot create capture. */
			v_close(env, *pv);
            ai_mem_nomem(env);
        }
        cap->_ftbc = true;
    }
}

GFun* ai_fun_new(a_henv env, GProto* proto) {
	Value* base = ai_stk_bot(env);
	GFun* parent = v_as_func(base[-1]);
	CapInfo* infos = proto->_caps;

	a_u32 len = proto->_ncap;

	GFun* self = ai_mem_gnew(env, GFun, fun_size(len));

	self->_vptr = &afun_vtable;
	self->_proto = proto;
	self->_len = len;
	self->_flags = proto->_flags;
	self->_fname = 0;

	memclr(self->_caps, sizeof(RcCap*) * len);

	for (a_u32 i = 0; i < len; ++i) {
		self->_caps[i] = cap_load(env, &infos[i], parent->_caps, base);
	}

	ai_gc_register_object(env, self);

	return self;
}

static a_bool cap_is_closed(RcCap* self) {
	return self->_ptr == &self->_slot;
}

static void cap_mark(Global* gbl, RcCap* self) {
	if (cap_is_closed(self) || gbl->_gcstep == GCSTEP_PROPAGATE_ATOMIC) {
		ai_gc_trace_mark_val(gbl, *self->_ptr);
	}
	else {
		self->_ftouch = true;
	}
}

void ai_cap_really_drop(Global* gbl, RcCap* self) {
	ai_mem_dealloc(gbl, self, sizeof(RcCap));
}

static void cap_drop(Global* gbl, RcCap* self) {
	self->_next = gbl->_cap_cache;
	gbl->_cap_cache = self;
}

static void cap_close_value(a_henv env, RcCap* self) {
	if (self->_ftbc) {
		v_close(env, *self->_ptr);
		self->_ftbc = false;
	}
}

static void cap_release(Global* gbl, RcCap* self) {
	assume(self->_rc > 0);
	if (--self->_rc == 0 && cap_is_closed(self)) {
		cap_close_value(ai_env_mroute(gbl), self);
		cap_drop(gbl, self);
	}
}

static void cap_close_internal(a_henv env, RcCap* restrict self) {
	v_cpy(env, &self->_slot, self->_ptr);
	self->_ptr = &self->_slot;

	if (self->_ftouch) {
		ai_gc_trace_mark_val(G(env), self->_slot);
	}
}

void ai_cap_close(a_henv env, RcCap* self) {
	assume(!cap_is_closed(self), "cannot close capture again.");
	if (self->_rc > 0) {
		cap_close_internal(env, self);
	}
	else {
		cap_close_value(env, self);
		cap_drop(G(env), self);
	}
}

void ai_cap_close_above(a_henv env, Value* pv) {
	check_in_stack(env, pv);
	RcCap* caps = env->_open_caps;
	RcCap* cap;
	while ((cap = caps) != null && cap->_ptr >= pv) {
		caps = cap->_next;
		ai_cap_close(env, cap);
	}
	env->_open_caps = cap;
}

void ai_cap_clean(Global* gbl) {
	RcCap* cap = gbl->_cap_cache;
	gbl->_cap_cache = null;
	while (cap != null) {
		RcCap* next = cap->_next;
		ai_cap_really_drop(gbl, cap);
		cap = next;
	}
}

static void afun_drop(Global* gbl, GFun* self) {
	for (a_u32 i = 0; i < self->_len; ++i) {
		cap_release(gbl, self->_caps[i]);
	}
    ai_mem_gdel(gbl, self, fun_size(self->_len));
}

static void afun_mark(Global* gbl, GFun* self) {
	ai_gc_trace_mark(gbl, self->_proto);
	a_u32 len = self->_len;
	for (a_u32 i = 0; i < len; ++i) {
		cap_mark(gbl, self->_caps[i]);
	}
	ai_gc_trace_work(gbl, fun_size(self->_len));
}

static void uniq_afun_mark(Global* gbl, GFun* self) {
    ai_gc_trace_mark(gbl, self->_proto);
    a_u32 len = self->_len;
    for (a_u32 i = 0; i < len; ++i) {
        cap_mark(gbl, self->_caps[i]);
    }
}

static void cfun_drop(Global* gbl, GFun* self) {
    ai_mem_gdel(gbl, self, fun_size(self->_len));
}

static void cfun_mark(Global* gbl, GFun* self) {
	a_u32 len = self->_len;
	for (a_u32 i = 0; i < len; ++i) {
		ai_gc_trace_mark_val(gbl, self->_vals[i]);
	}
	ai_gc_trace_work(gbl, fun_size(len));
}

static void uniq_proto_drop(Global* gbl, GProto* self) {
    GFun* func = self->_cache;
    for (a_u32 i = 0; i < func->_len; ++i) {
        cap_release(gbl, func->_caps[i]);
    }
    ai_mem_gdel(gbl, self, self->_size);
}

void ai_proto_drop(Global* gbl, GProto* self) {
	ai_mem_gdel(gbl, self, self->_size);
}

static void proto_mark(Global* gbl, GProto* self) {
	for (a_u32 i = 0; i < self->_nconst; ++i) {
		ai_gc_trace_mark_val(gbl, self->_consts[i]);
	}
    for (a_u32 i = 0; i < self->_nsub; ++i) {
        ai_gc_trace_mark(gbl, self->_subs[i]);
    }
	if (self->_dbg_file != null) {
		ai_gc_trace_mark(gbl, self->_dbg_file);
	}
	if (self->_dbg_locals != null) {
		for (a_u32 i = 0; i < self->_nlocal; ++i) {
			ai_gc_trace_mark(gbl, self->_dbg_locals[i]._name);
		}
		assume(self->_dbg_cap_names != null);
		for (a_u32 i = 0; i < self->_ncap; ++i) {
			ai_gc_trace_mark(gbl, self->_dbg_cap_names[i]);
		}
	}
	ai_gc_trace_work(gbl, self->_size);
}

static VTable const afun_vtable = {
	._stencil = V_STENCIL(T_FUNC),
    ._tag = ALO_TFUNC,
    ._type_ref = g_type_ref(ALO_TFUNC),
	._slots = {
        [vfp_slot(drop)] = afun_drop,
        [vfp_slot(mark)] = afun_mark
	}
};

static VTable const uniq_afun_vtable = {
	._stencil = V_STENCIL(T_FUNC),
    ._tag = ALO_TFUNC,
    ._type_ref = g_type_ref(ALO_TFUNC),
	._slots = {
        [vfp_slot(mark)] = uniq_afun_mark
	}
};

static VTable const cfun_vtable = {
	._stencil = V_STENCIL(T_FUNC),
    ._tag = ALO_TFUNC,
    ._type_ref = g_type_ref(ALO_TFUNC),
	._slots = {
        [vfp_slot(drop)] = cfun_drop,
        [vfp_slot(mark)] = cfun_mark
	}
};

static VTable const proto_vtable = {
	._stencil = V_STENCIL(T_USER),
	._slots = {
        [vfp_slot(drop)] = ai_proto_drop,
        [vfp_slot(mark)] = proto_mark
	}
};

static VTable const uniq_proto_vtable = {
    ._stencil = V_STENCIL(T_USER),
    ._slots = {
        [vfp_slot(drop)] = uniq_proto_drop,
        [vfp_slot(mark)] = proto_mark
    }
};