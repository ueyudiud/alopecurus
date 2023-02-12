/**
 *@file afun.h
 */

#ifndef afun_h_
#define afun_h_

#include "aobj.h"
#include "aenv.h"

typedef struct GFunMeta GFunMeta;

typedef struct LocalInfo LocalInfo;
typedef struct CapInfo CapInfo;
typedef struct LineInfo LineInfo;
typedef struct FnMetaCreateInfo FnMetaCreateInfo;

intern GFunMeta* ai_fmeta_xalloc(a_henv env, FnMetaCreateInfo* info);
intern GFun* ai_cfun_create(a_henv env, a_cfun hnd, a_u32 ncap, Value const* pcap);
intern void ai_fmeta_destruct(Global* g, GFunMeta* self);

intern VTable const ai_cap_vtable;
intern VTable const ai_fun_vtable;
intern VTable const ai_fmeta_vtable;

#define GFUNMETA_FLAG_VARARG u32c(0x0100)

struct GFunMeta {
	GMETA_STRUCT_HEADER;
	a_u16 _nconst;
	a_u16 _ninsn;
	a_u16 _nsub;
	a_u16 _nlocal;
	a_u16 _nline;
	a_u8 _ncap;
	a_u8 _nstack;
	a_insn* _code;
	CapInfo* _caps;
	GStr* _dbg_name;
	GStr* _dbg_file;
	a_u32 _dbg_lndef;
	a_u32 _dbg_lnldef;
	LineInfo* _lines;
	LocalInfo* _locals;
	GStr** _cap_names;
	GFunMeta** _subs;
	GFun* _cache;
	Value _consts[0];
};

struct GFun {
	GOBJ_STRUCT_HEADER;
	a_u32 _len;
	Value _capval[0];
};

struct GCap {
	GOBJ_STRUCT_HEADER;
	Value* _ptr;
	a_u32 _rc;
	union {
		Value _slot;
		GCap* _next;
	};
};

struct LocalInfo {
	GStr* _name;
	a_u32 _begin_label;
	a_u32 _end_label;
	a_u8 _reg;
};

struct CapInfo {
	a_u8 _up;
	a_u8 _reg;
};

struct LineInfo {
	a_u32 _end;
	a_u32 _lineno;
};

typedef struct {
	a_u8 _fline: 1;
	a_u8 _fname: 1;
	a_u8 _froot: 1;
	a_u8 _fconst_env: 1;
} FnMetaCreateFlags;

/**
 ** Function fixed sized information.
 */
struct FnMetaCreateInfo {
	a_u32 _nconst;
	a_u32 _ninsn;
	a_u16 _nsub;
	a_u16 _nlocal;
	a_u16 _nline;
	a_u8 _ncap;
	a_u8 _nstack;
	FnMetaCreateFlags _flags;
};

inline a_bool g_is_cap(Global* g, a_hobj v);

inline GCap* g_as_cap(Global* g, a_hobj v) {
	assume(g_is_cap(g, v));
	return g_cast(GCap, v);
}

#endif /* afun_h_ */
