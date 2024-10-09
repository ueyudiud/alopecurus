/**
 *@file afun.h
 */

#ifndef afun_h_
#define afun_h_

#include "aobj.h"

typedef struct LocalInfo LocalInfo;
typedef struct CapInfo CapInfo;
typedef struct LineInfo LineInfo;
typedef struct ProtoDesc ProtoDesc;

intern GProto* ai_proto_alloc(a_henv env, ProtoDesc const* desc);
intern GFun* ai_cfun_create(a_henv env, a_cfun hnd, a_u32 ncap, Value const* pcap);
intern GFun* ai_fun_new(a_henv env, GProto *proto);
intern void ai_proto_drop(Global* gbl, GProto* self);
intern RcCap* ai_cap_new(a_henv env);
intern void ai_cap_mark_tbc(a_henv env, Value* p);
intern void ai_cap_really_drop(Global* gbl, RcCap* self);
intern void ai_cap_close(a_henv env, RcCap* self);
intern void ai_cap_close_above(a_henv env, Value* pv);
intern void ai_cap_clean(Global* gbl);

#define FUN_FLAG_VARARG u32c(0x00000001)
#define FUN_FLAG_NATIVE u32c(0x00000002)
#define FUN_FLAG_UNIQUE u32c(0x00000004)
#define FUN_FLAG_DEBUG  u32c(0x00010000)

/**
 ** The capture value using reference counter.
 */
struct RcCap {
	Value* ptr;
	a_u32 nref;
	union {
		a_u8 flags;
		struct {
			a_u8 ftouch: 1;
			a_u8 ftbc: 1;
		};
	};
	union {
		Value slot;
		RcCap* next;
	};
};

/**
 ** Function fixed sized information.
 */
struct ProtoDesc {
	a_u32 nconst;
	a_u32 ninsn;
    a_flags flags;
	a_u16 nsub;
	a_u16 nlocal;
	a_u16 nline;
	a_u8 ncap;
	a_u8 nstack;
	a_u8 nparam;
};

struct GFun {
    GOBJ_STRUCT_HEADER;
    a_u32 ncap;
    a_u16 flags;
    /* Function dbg_name. */
    a_u16 fname;
    union {
        a_cfun fptr;
        GProto* proto;
    };
    union {
        RcCap* ref_caps[0];
        Value val_caps[0];
    };
};

struct GProto {
    GOBJ_STRUCT_HEADER;
    a_u32 size;
    a_u16 flags;
    a_u8 nstack;
    a_u8 nparam;
    Value* consts;
    a_insn* code;
    a_u16 nconst;
    a_u16 ninsn;
    a_u16 nsub;
    a_u16 nlocal;
    a_u16 nline;
    a_u8 ncap;
    CapInfo* caps;
    GFun* cache;
    GStr* dbg_name;
    GStr* dbg_file;
    a_u32 dbg_lndef;
    a_u32 dbg_lnldef;
    LineInfo* dbg_lines;
    LocalInfo* dbg_locals;
    GStr** dbg_cap_names;
    GProto* subs[0];
};

struct LocalInfo {
    GStr* name;
    a_u32 lbegin;
    a_u32 lend;
    a_u8 reg;
};

struct CapInfo {
    union {
        a_u8 flags;
        struct {
            a_u8 fup: 1; /* Capture from upper closure. */
        };
    };
    a_u8 reg;
};

struct LineInfo {
    a_u32 lend;
    a_u32 line;
};

#define g_is_func(o) g_is(o, ALO_TFUNC)

#define v_is_func(v) v_is(v, T_FUNC)

always_inline GFun* v_as_func(Value v) {
    assume(v_is_func(v), "not function.");
    return g_as(GFun, v_as_obj(v));
}

always_inline Value v_of_func(GFun* o) {
    assume(g_is_func(o), "invalid instance.");
    return v_of_obj_(o, T_FUNC);
}

always_inline void v_set_func(a_henv env, Value* d, GFun* o) {
    Value v = v_of_func(o);
    v_set(env, d, v);
}

#endif /* afun_h_ */
