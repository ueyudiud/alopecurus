/**
 *@file afun.c
 */

#define afun_c_
#define ALO_LIB

#include "abc.h"
#include "aenv.h"
#include "amem.h"
#include "agc.h"
#include "avm.h"

#include "afun.h"

static VTable const afun_vtable;
static VTable const uniq_afun_vtable;
static VTable const cfun_vtable;
static VTable const proto_vtable;

static a_usize fun_size(a_usize ncap) {
	return sizeof(GFun) + sizeof(Value) * ncap;
}

#define UNIQ_PROTO_OFFSET offsetof(GProto, size)

static a_usize proto_size_with_head(ProtoDesc const* desc) {
	a_usize size = sizeof(GProto) +
				   sizeof(Value) * desc->nconst +
				   sizeof(a_insn) * desc->ninsn +
				   sizeof(CapInfo) * desc->ncap +
				   sizeof(GProto*) * desc->nsub;
	if (desc->flags & FUN_FLAG_DEBUG) {
		size += sizeof(LineInfo) * desc->nline +
				sizeof(LocalInfo) * desc->nlocal +
				sizeof(GStr*) * desc->ncap;
	}
	else {
		size += sizeof(LineInfo);
	}
	if (desc->flags & FUN_FLAG_UNIQUE) {
		size += fun_size(desc->ncap) - UNIQ_PROTO_OFFSET;
	}
	return pad_to(size, sizeof(a_usize));
}

GProto* ai_proto_alloc(a_henv env, ProtoDesc const* desc) {
	a_usize total_size = proto_size_with_head(desc);
    void* blk = ai_mem_alloc(env, total_size);
    memclr(blk, total_size);

    GProto* self;

    if (desc->flags & FUN_FLAG_UNIQUE) {
        self = blk - UNIQ_PROTO_OFFSET;
    }
    else {
        self = blk;
        self->vptr = &proto_vtable;
    }

    self->size = total_size;
    self->flags = desc->flags;
    self->nconst = desc->nconst;
    self->ninsn = desc->ninsn;
    self->nsub = desc->nsub;
    self->nlocal = desc->nlocal;
    self->ncap = desc->ncap;
	self->nline = desc->nline;
    self->nstack = desc->nstack;
	self->nparam = desc->nparam;

	a_usize addr = ptr2int(self) + sizeof(GProto);

	/* self->subs = int2ptr(GProto*, addr); */
	addr += sizeof(GProto*) * desc->nsub;

	self->consts = int2ptr(Value, addr);
	addr += sizeof(Value) * desc->nconst;

	if (desc->flags & FUN_FLAG_DEBUG) {
		self->dbg_lines = int2ptr(LineInfo, addr);
		addr += sizeof(LineInfo) * desc->nline;

		self->dbg_locals = int2ptr(LocalInfo, addr);
		addr += sizeof(LocalInfo) * desc->nlocal;

		self->dbg_cap_names = int2ptr(GStr*, addr);
		addr += sizeof(GStr*) * desc->ncap;
	}
	else {
        self->dbg_lndef = self->dbg_lnldef = 0;

		self->dbg_lines = int2ptr(LineInfo, addr);
		addr += sizeof(LineInfo);

        init(&self->dbg_lines[0]) {
            .lend = UINT32_MAX,
            .line = 0
        };
	}

	if (desc->flags & FUN_FLAG_UNIQUE) {
        /* Initialize unique function for this prototype. */
		GFun* fun = int2ptr(GFun, addr);
		addr += fun_size(desc->ncap);

		fun->vptr = &uniq_afun_vtable;
		fun->proto = self;
		fun->ncap = desc->ncap;

		self->cache = fun;
	}

	self->caps = int2ptr(CapInfo, addr);
	addr += sizeof(CapInfo) * self->ncap;

	self->code = int2ptr(a_insn, addr);
	addr += sizeof(a_insn) * self->ninsn;

    addr = pad_to(addr, sizeof(a_usize));

	assume(ptr2int(blk) + total_size == addr);

	return self;
}

GFun* ai_cfun_create(a_henv env, a_cfun hnd, a_u32 ncap, Value const* pcap) {
	GFun* self = ai_mem_alloc(env, fun_size(ncap));

	self->vptr = &cfun_vtable;
	self->ncap = ncap;
	self->fptr = hnd;
	self->flags = FUN_FLAG_NATIVE | FUN_FLAG_VARARG;
	self->fname = 0;

	v_cpy_all(env, self->val_caps, pcap, ncap);

	ai_gc_register_object(env, self);

	return self;
}

static RcCap* cap_nalloc(a_henv env) {
	Global* gbl = G(env);
	RcCap* cap = gbl->cap_cache;
	if (cap != null) {
		gbl->cap_cache = cap->next;
		return cap;
	}
	return ai_mem_nalloc(env, sizeof(RcCap));
}

static RcCap* cap_nload_from_stack(a_henv env, Value* pv) {
	/* Check stack value pointer. */
	check_in_stack(env, pv);
	v_check_alive(env, *pv);

	RcCap* cap;
	RcCap** now = &env->open_caps;

	while ((cap = *now) != null && cap->ptr > pv) {
		now = &cap->next;
	}
	
	if (cap == null || cap->ptr < pv) {
		RcCap* self = cap_nalloc(env);

        if (self == null) return null;

        init(self) {
            .ptr = pv,
            .nref = 1,
            .next = cap
        };

		*now = cap = self;
	}
	else {
		cap->nref += 1;
	}
	return cap;
}

static RcCap* cap_load(a_henv env, CapInfo const* info, RcCap** up, Value* stack) {
	if (info->fup) {
        RcCap* cap = up[info->reg];
        cap->nref += 1;
		return cap;
	}
	else {
		RcCap* cap = cap_nload_from_stack(env, &stack[info->reg]);
        if (unlikely(cap == null)) ai_mem_nomem(env);
        return cap;
	}
}

static void v_close(a_henv env, Value v) {
    a_gptr p = v_as_obj(v);
    (*g_fetch(p, close))(env, p);
}

RcCap* ai_cap_new(a_henv env) {
    RcCap* self = cap_nalloc(env);

    if (unlikely(self == null)) ai_mem_nomem(env);

    init(self) {
        .ptr = &self->slot,
        .nref = 1,
        .slot = v_of_nil()
    };

    return self;
}

void ai_cap_mark_tbc(a_henv env, Value* pv) {
	RcCap* cap = cap_nload_from_stack(env, pv);
    if (!cap->ftbc) {
        if (unlikely(cap == null)) {
            /* Call close function immediately if cannot create capture. */
			v_close(env, *pv);
            ai_mem_nomem(env);
        }
        cap->ftbc = true;
    }
}

GFun* ai_fun_new(a_henv env, GProto* proto) {
	Value* base = ai_stk_bot(env);
	GFun* parent = v_as_func(base[-1]);
	CapInfo* infos = proto->caps;

	a_u32 len = proto->ncap;

	GFun* self = ai_mem_alloc(env, fun_size(len));

	self->vptr = &afun_vtable;
	self->proto = proto;
	self->ncap = len;
	self->flags = proto->flags;
	self->fname = 0;

	memclr(self->ref_caps, sizeof(RcCap*) * len);

	for (a_u32 i = 0; i < len; ++i) {
		self->ref_caps[i] = cap_load(env, &infos[i], parent->ref_caps, base);
	}

	ai_gc_register_object(env, self);

	return self;
}

static a_bool cap_is_closed(RcCap* self) {
	return self->ptr == &self->slot;
}

static void cap_mark(Global* gbl, RcCap* self) {
	if (cap_is_closed(self) || gbl->gcstep == GCSTEP_PROPAGATE_ATOMIC) {
		ai_gc_trace_mark_val(gbl, *self->ptr);
	}
	else {
		self->ftouch = true;
	}
}

void ai_cap_really_drop(Global* gbl, RcCap* self) {
	ai_mem_dealloc(gbl, self, sizeof(RcCap));
}

static void cap_drop(Global* gbl, RcCap* self) {
	self->next = gbl->cap_cache;
	gbl->cap_cache = self;
}

static void cap_close_value(a_henv env, RcCap* self) {
	if (self->ftbc) {
		v_close(env, *self->ptr);
		self->ftbc = false;
	}
}

static void cap_release(Global* gbl, RcCap* self) {
	assume(self->nref > 0);
	if (--self->nref == 0 && cap_is_closed(self)) {
		cap_close_value(ai_env_mroute(gbl), self);
		cap_drop(gbl, self);
	}
}

static void cap_close_internal(a_henv env, RcCap* restrict self) {
	v_cpy(env, &self->slot, self->ptr);
	self->ptr = &self->slot;

	if (self->ftouch) {
		ai_gc_trace_mark_val(G(env), self->slot);
	}
}

void ai_cap_close(a_henv env, RcCap* self) {
	assume(!cap_is_closed(self), "cannot close capture again.");
	if (self->nref > 0) {
		cap_close_internal(env, self);
	}
	else {
		cap_close_value(env, self);
		cap_drop(G(env), self);
	}
}

void ai_cap_close_above(a_henv env, Value* pv) {
	check_in_stack(env, pv);
	RcCap* caps = env->open_caps;
	RcCap* cap;
	while ((cap = caps) != null && cap->ptr >= pv) {
		caps = cap->next;
		ai_cap_close(env, cap);
	}
	env->open_caps = cap;
}

void ai_cap_clean(Global* gbl) {
	RcCap* cap = gbl->cap_cache;
	gbl->cap_cache = null;
	while (cap != null) {
		RcCap* next = cap->next;
		ai_cap_really_drop(gbl, cap);
		cap = next;
	}
}

static void proto_drop(Global* gbl, GProto* self) {
    ai_mem_dealloc(gbl, self, self->size);
}

static void proto_mark_body(Global* gbl, GProto* self) {
    for (a_u32 i = 0; i < self->nconst; ++i) {
        ai_gc_trace_mark_val(gbl, self->consts[i]);
    }
    for (a_u32 i = 0; i < self->nsub; ++i) {
        ai_gc_trace_mark(gbl, self->subs[i]);
    }
    if (self->dbg_name != null) {
        ai_gc_trace_mark(gbl, self->dbg_name);
    }
    if (self->dbg_file != null) {
        ai_gc_trace_mark(gbl, self->dbg_file);
    }
    if (self->dbg_locals != null) {
        for (a_u32 i = 0; i < self->nlocal; ++i) {
            LocalInfo* info = &self->dbg_locals[i];
            if (info->name != null) {
                ai_gc_trace_mark(gbl, info->name);
            }
        }
        assume(self->dbg_cap_names != null);
        for (a_u32 i = 0; i < self->ncap; ++i) {
            if (self->dbg_cap_names[i] != null) {
                ai_gc_trace_mark(gbl, self->dbg_cap_names[i]);
            }
        }
    }
    ai_gc_trace_work(gbl, self->size);
}

static void proto_mark(Global* gbl, GProto* self) {
    self->cache = null;
    proto_mark_body(gbl, self);
}

static void afun_drop(Global* gbl, GFun* self) {
    for (a_u32 i = 0; i < self->ncap; ++i) {
        cap_release(gbl, self->ref_caps[i]);
    }
    ai_mem_dealloc(gbl, self, fun_size(self->ncap));
}

static void afun_mark_body(Global* gbl, GFun* self) {
    a_u32 len = self->ncap;
    for (a_u32 i = 0; i < len; ++i) {
        cap_mark(gbl, self->ref_caps[i]);
    }
}

static void afun_mark(Global* gbl, GFun* self) {
    ai_gc_trace_mark(gbl, self->proto);
    afun_mark_body(gbl, self);
    ai_gc_trace_work(gbl, fun_size(self->ncap));
}

static void uniq_afun_drop(Global* gbl, GFun* self) {
    for (a_u32 i = 0; i < self->ncap; ++i) {
        cap_release(gbl, self->ref_caps[i]);
    }

    GProto* proto = self->proto;
    void* block = cast(void*, proto) + UNIQ_PROTO_OFFSET;
    ai_mem_dealloc(gbl, block, proto->size);
}

static void uniq_afun_mark(Global* gbl, GFun* self) {
    afun_mark_body(gbl, self);
    proto_mark_body(gbl, self->proto);
}

static void cfun_drop(Global* gbl, GFun* self) {
    ai_mem_dealloc(gbl, self, fun_size(self->ncap));
}

static void cfun_mark(Global* gbl, GFun* self) {
    a_u32 len = self->ncap;
    for (a_u32 i = 0; i < len; ++i) {
        ai_gc_trace_mark_val(gbl, self->val_caps[i]);
    }
    ai_gc_trace_work(gbl, fun_size(len));
}

static void fun_except(a_henv env, GFun* self, unused a_msg msg) {
    Value* bot = vm_push_args(env, v_of_obj(self), env->error);
    v_set_nil(&env->error);
    Value err = ai_vm_call_meta(env, bot);
    v_set(env, &env->error, err);
}

void ai_proto_drop(Global* gbl, GProto* self) {
    if (self->flags & FUN_FLAG_UNIQUE) {
        uniq_afun_drop(gbl, self->cache);
    }
    else {
        proto_drop(gbl, self);
    }
}

static VTable const afun_vtable = {
	.stencil = V_STENCIL(T_FUNC),
    .tag = ALO_TFUNC,
    .type_ref = g_type_ref(ALO_TFUNC),
	.slots = {
        [vfp_slot(drop)] = afun_drop,
        [vfp_slot(mark)] = afun_mark,
        [vfp_slot(except)] = fun_except
	}
};

static VTable const uniq_afun_vtable = {
	.stencil = V_STENCIL(T_FUNC),
    .tag = ALO_TFUNC,
    .type_ref = g_type_ref(ALO_TFUNC),
	.slots = {
        [vfp_slot(drop)] = uniq_afun_drop,
        [vfp_slot(mark)] = uniq_afun_mark,
        [vfp_slot(except)] = fun_except
	}
};

static VTable const cfun_vtable = {
	.stencil = V_STENCIL(T_FUNC),
    .tag = ALO_TFUNC,
    .type_ref = g_type_ref(ALO_TFUNC),
	.slots = {
        [vfp_slot(drop)] = cfun_drop,
        [vfp_slot(mark)] = cfun_mark,
        [vfp_slot(except)] = fun_except
	}
};

static VTable const proto_vtable = {
	.stencil = V_STENCIL(T_USER),
	.slots = {
        [vfp_slot(drop)] = proto_drop,
        [vfp_slot(mark)] = proto_mark
	}
};
