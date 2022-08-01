/**
 *@file afun.h
 */

#ifndef afun_h_
#define afun_h_

#include "aobj.h"

typedef struct GFunMeta GFunMeta;

struct GFunMeta {
	GMETA_STRUCT_HEADER;
    a_u32 _nconst;
    a_u32 _ninsn;
    a_u16 _nsub;
    a_u16 _nlocal;
    a_u8 _ncap;
    a_u8 _nstack;
    a_insn* _insns;
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

typedef struct FnInfo FnInfo;

intern GFunMeta* ai_fun_xalloc(a_henv env, FnInfo* info);
intern void ai_fun_splash(Global* g, GFun* self);
intern void ai_fun_destruct(Global* g, GFun* self);
intern void ai_fun_meta_splash(Global* g, GFunMeta* self);
intern void ai_fun_meta_destruct(Global* g, GFunMeta* self);

/**
 ** Function fixed sized information.
 */
struct FnInfo {
	a_usize _size;
	a_u32 _nconst;
	a_u32 _ninsn;
	a_u16 _nsub;
	a_u16 _nlocal;
	a_u8 _ncap;
	a_u8 _nstack;
};

/**
 ** Hint function size with the minimum value.
 *@param info the information of function metadata.
 */
inline void fninfo_hint_size(FnInfo* info) {
	info->_size = sizeof(GFunMeta) +
			sizeof(Value) * info->_nconst +
			sizeof(a_insn) * info->_ninsn +
			sizeof(GFunMeta*) * info->_nsub;
}

#endif /* afun_h_ */
