/**
 *@file afun.h
 */

#ifndef afun_h_
#define afun_h_

#include "aobj.h"

typedef struct GFunMeta GFunMeta;

typedef struct LocalInfo LocalInfo;
typedef struct CapInfo CapInfo;
typedef struct LineInfo LineInfo;
typedef struct FnMetaCreateInfo FnMetaCreateInfo;

intern GFunMeta* ai_fmeta_xalloc(a_henv env, FnMetaCreateInfo* info);
intern GFun* ai_cfun_create(a_henv env, a_cfun hnd, a_u32 ncap, Value const* pcap);
intern GFun* ai_fun_new(a_henv env, GFunMeta* meta, Frame* frame);
intern void ai_fmeta_destruct(Global* g, GFunMeta* self);
intern void ai_cap_close(a_henv env, Capture* cap, Value const* base);

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
	a_u8 _nparam;
	a_u8 _nstack;
	a_insn* _code;
	CapInfo* _caps;
	GStr* _name;
	GStr* _dbg_file;
	a_u32 _dbg_lndef;
	a_u32 _dbg_lnldef;
	LineInfo* _dbg_lines;
	LocalInfo* _dbg_locals;
	GStr** _dbg_cap_names;
	GFunMeta** _subs;
	GFun* _cache;
	Value _consts[0];
};

struct GFun {
	GOBJ_STRUCT_HEADER;
	a_u32 _len;
	Value _capval[0];
};

struct Capture {
	Value* _ptr;
	a_u32 _rc;
	union {
		Value _slot;
		Capture* _next;
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
			a_u8 _fro: 2; /* Readonly capture. */
		};
	};
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

#endif /* afun_h_ */
