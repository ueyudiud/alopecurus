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

intern GProto* ai_proto_xalloc(a_henv env, ProtoDesc const* desc);
intern GFun* ai_cfun_create(a_henv env, a_cfun hnd, a_u32 ncap, Value const* pcap);
intern GFun* ai_fun_new(a_henv env, GProto *proto);
intern void ai_proto_drop(Global* gbl, GProto* self);
intern RcCap* ai_cap_new(a_henv env);
intern void ai_cap_mark_tbc(a_henv env, Value* pv);
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
	Value* _ptr;
	a_u32 _rc;
	union {
		a_u8 _flags;
		struct {
			a_u8 _ftouch: 1;
			a_u8 _ftbc: 1;
		};
	};
	union {
		Value _slot;
		RcCap* _next;
	};
};

typedef struct {
	a_u8 _fdebug: 1;
    /**
     ** The prototype create with prototype flag will also create
     ** a unique function.
     */
	a_u8 _funiq: 1;
} ProtoFlags;

/**
 ** Function fixed sized information.
 */
struct ProtoDesc {
	a_u32 _nconst;
	a_u32 _ninsn;
    a_flags _flags;
	a_u16 _nsub;
	a_u16 _nlocal;
	a_u16 _nline;
	a_u8 _ncap;
	a_u8 _nstack;
	a_u8 _nparam;
};

struct GFun {
    GOBJ_STRUCT_HEADER;
    a_u32 _len;
    a_u16 _flags;
    /* Function name. */
    a_u16 _fname;
    union {
        a_cfun _fptr;
        GProto* _proto;
    };
    union {
        RcCap* _caps[0];
        Value _vals[0];
    };
};

struct GProto {
    GOBJ_STRUCT_HEADER;
    a_u32 _size;
    a_u16 _flags;
    a_u8 _nstack;
    a_u8 _nparam;
    Value* _consts;
    a_insn* _code;
    a_u16 _nconst;
    a_u16 _ninsn;
    a_u16 _nsub;
    a_u16 _nlocal;
    a_u16 _nline;
    a_u8 _ncap;
    CapInfo* _caps;
    GFun* _cache;
    GStr* _name;
    GStr* _dbg_file;
    a_u32 _dbg_lndef;
    a_u32 _dbg_lnldef;
    LineInfo* _dbg_lines;
    LocalInfo* _dbg_locals;
    GStr** _dbg_cap_names;
    GProto* _subs[0];
};

struct LocalInfo {
    GStr* _name;
    a_u32 _begin_label;
    a_u32 _end_label;
    a_u8 _reg;
};

struct CapInfo {
    union {
        a_u8 _flags;
        struct {
            a_u8 _fup: 1; /* Capture from upper closure. */
        };
    };
    a_u8 _reg;
};

struct LineInfo {
    a_u32 _end;
    a_u32 _lineno;
};

#define v_is_func(v) v_is(v, T_FUNC)

always_inline GFun* v_as_func(Value v) {
    assume(v_is_func(v), "not function.");
    return g_cast(GFun, v_as_obj(v));
}

#endif /* afun_h_ */
