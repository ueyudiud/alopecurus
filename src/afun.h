/**
 *@file afun.h
 */

#ifndef afun_h_
#define afun_h_

#include "aobj.h"

intern GProto* ai_proto_xalloc(a_henv env, ProtoDesc* desc);
intern GFun* ai_cfun_create(a_henv env, a_cfun hnd, a_u32 ncap, Value const* pcap);
intern GFun* ai_fun_new(a_henv env, GProto* proto, Frame* frame);
intern void ai_proto_drop(Global* g, GProto* self);
intern void ai_cap_mark_tbc(a_henv env, Frame* frame, Value* ptr);
intern void ai_cap_really_drop(Global* g, RcCap* self);
intern void ai_cap_soft_close(a_henv env, RcCap* self);
intern void ai_cap_hard_close(a_henv env, RcCap* self);

#define FUN_FLAG_VARARG u16c(0x0001)
#define FUN_FLAG_NATIVE u16c(0x0002)

typedef struct {
	GPROTO_STRUCT_HEADER;
} GThinProto;

/**
 ** The capture value using reference counter.
 */
struct RcCap {
	Value* _ptr;
	a_u32 _rc_and_fopen;
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

#endif /* afun_h_ */
