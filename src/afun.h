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
typedef struct FnCreateInfo FnCreateInfo;

intern GFunMeta* ai_fun_xalloc(a_henv env, FnCreateInfo* info);
intern GFun* ai_cfun_create(a_henv env, a_cfun hnd, a_u32 ncap, Value const* pcap);
intern void ai_fun_splash(Global* g, GFun* self);
intern void ai_fun_destruct(Global* g, GFun* self);
intern void ai_fun_meta_splash(Global* g, GFunMeta* self);
intern void ai_fun_meta_destruct(Global* g, GFunMeta* self);

#define FUN_META_CAP_DYN UINT8_MAX

struct GFunMeta {
	GMETA_STRUCT_HEADER;
	a_u16 _nconst;
	a_u16 _ninsn;
	a_u16 _nsub;
	a_u16 _nlocal;
	a_u16 _nline;
	a_u8 _ncap;
	a_u8 _nstack;
	a_insn* _insns;
	CapInfo* _caps;
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
} DebugOption;

/**
 ** Function fixed sized information.
 */
struct FnCreateInfo {
	a_usize _size;
	a_u32 _nconst;
	a_u32 _ninsn;
	a_u16 _nsub;
	a_u16 _nlocal;
	a_u16 _nline;
	a_u8 _ncap;
	a_u8 _nstack;
	DebugOption _debug;
};

inline a_bool g_is_cap(Global* g, a_hobj v);

inline GCap* g_as_cap(Global* g, a_hobj v) {
	assume(g_is_cap(g, v));
	return downcast(GCap, v);
}

/**
 ** Hint function size with the minimum value.
 *@param info the information of function metadata.
 */
inline void fninfo_hint_size(FnCreateInfo* info) {
	a_usize size = sizeof(GFunMeta) +
			sizeof(Value) * info->_nconst +
			sizeof(a_insn) * info->_ninsn +
			sizeof(CapInfo) * info->_ncap +
			sizeof(GFunMeta*) * info->_nsub;
	if (info->_debug._fline) {
		size += sizeof(LineInfo) * info->_nline;
	}
	else {
		size += sizeof(LineInfo);
	}
	if (info->_debug._fname) {
		size += sizeof(LocalInfo) * info->_nlocal +
				sizeof(GStr*) * info->_ncap;
	}
	info->_size = size;
}

#endif /* afun_h_ */
