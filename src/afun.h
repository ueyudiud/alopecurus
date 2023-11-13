/**
 *@file afun.h
 */

#ifndef afun_h_
#define afun_h_

#include "aobj.h"

intern GProto* ai_proto_xalloc(a_henv env, ProtoDesc* desc);
intern GFun* ai_cfun_create(a_henv env, a_cfun hnd, a_u32 ncap, Value const* pcap);
intern GFun* ai_fun_new(a_henv env, GProto *proto);
intern void ai_proto_drop(Global* gbl, GProto* self);
intern RcCap* ai_cap_new(a_henv env);
intern void ai_cap_mark_tbc(a_henv env, Value* pv);
intern void ai_cap_really_drop(Global* gbl, RcCap* self);
intern void ai_cap_close(a_henv env, RcCap* self);
intern void ai_cap_close_above(a_henv env, Value* pv);
intern void ai_cap_clean(Global* gbl);

#define FUN_FLAG_VARARG u16c(0x0001)
#define FUN_FLAG_NATIVE u16c(0x0002)

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
	a_u8 _funiq: 1;
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
	a_u8 _nparam;
	ProtoFlags _flags;
};

#endif /* afun_h_ */
