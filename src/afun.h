/**
 *@file afun.h
 */

#ifndef afun_h_
#define afun_h_

#include "aobj.h"

typedef struct GProto GProto;

typedef union CapVal CapVal;
typedef struct LocalInfo LocalInfo;
typedef struct CapInfo CapInfo;
typedef struct LineInfo LineInfo;
typedef struct ProtoDesc ProtoDesc;

intern GProto* ai_proto_xalloc(a_henv env, ProtoDesc* desc);
intern GFun* ai_cfun_create(a_henv env, a_cfun hnd, a_u32 ncap, Value const* pcap);
intern GFun* ai_fun_new(a_henv env, GProto* proto, Frame* frame);
intern void ai_proto_drop(Global* g, GProto* self);
intern void ai_cap_drop(Global* g, RcCap* cap);
intern void ai_cap_soft_close(a_henv env, RcCap* cap);
intern void ai_cap_hard_close(a_henv env, RcCap* cap);

#define FUN_FLAG_VARARG u16c(0x0001)
#define FUN_FLAG_NATIVE u16c(0x0002)

#define GPROTO_STRUCT_HEADER \
	GOBJ_STRUCT_HEADER;         \
	a_u32 _len;                 \
	a_u16 _flags;               \
	a_u8 _nstack;               \
	a_u8 _nparam;               \
	Value* _consts;             \
	a_insn* _code

typedef struct {
	GPROTO_STRUCT_HEADER;
} GThinProto;

struct GProto {
	GPROTO_STRUCT_HEADER;
	a_u16 _nconst;
	a_u16 _ninsn;
	a_u16 _nsub;
	a_u16 _nlocal;
	a_u16 _nline;
	a_u8 _ncap;
	CapInfo* _caps;
	GStr* _name;
	GStr* _dbg_file;
	a_u32 _dbg_lndef;
	a_u32 _dbg_lnldef;
	LineInfo* _dbg_lines;
	LocalInfo* _dbg_locals;
	GStr** _dbg_cap_names;
	GFun* _cache;
	GProto* _subs[0];
};

union CapVal {
	Value _imm; /* Immediate value. */
	RcCap* _rc; /* Shared rc value. */
};

struct GFun {
	GOBJ_STRUCT_HEADER;
	a_u32 _len;
	a_u16 _flags;
	a_u16 _sym; /* Function symbol. */
	GProto* _proto;
	union {
		CapVal _caps[0];
		Value _vals[0];
	};
};

/**
 ** The capture value using reference counter.
 */
struct RcCap {
	Value* _ptr;
	a_u32 _rc_and_fclose;
	a_u8 _touch;
	union {
		Value _slot;
		RcCap* _next;
	};
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
			a_u8 _frc: 1; /* Use RcCap to capture. */
		};
	};
	a_u8 _reg;
};

struct LineInfo {
	a_u32 _end;
	a_u32 _lineno;
};

typedef struct {
	a_u8 _fdebug: 1;
	a_u8 _froot: 1;
} ProtoFlags;

/**
 ** Function fixed sized information.
 */
struct ProtoDesc {
	a_u32 _nconst;
	a_u32 _ninsn;
	a_u16 _nsub;
	a_u16 _nlocal;
	a_u16 _nline;
	a_u8 _ncap;
	a_u8 _nstack;
	ProtoFlags _flags;
};

always_inline a_bool g_is_func(a_hobj v) {
	return v->_vtable->_repr_id == REPR_FUNC;
}

always_inline GFun* g_as_func(a_hobj v) {
	assume(g_is_func(v));
	return g_cast(GFun, v);
}

always_inline GFun* v_as_func(Value v) {
	assume(v_is_func(v), "not function.");
	return g_as_func(v_as_obj(v));
}

#endif /* afun_h_ */
