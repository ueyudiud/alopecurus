/*
 * aobj.h
 */

#ifndef aobj_h_
#define aobj_h_

#include <stdatomic.h>
#include <math.h>

#include "adef.h"
#include "astrs.h"
#include "actx.h"

typedef struct GObj GObj;
typedef struct GStr GStr;
typedef struct GTuple GTuple;
typedef struct GList GList;
typedef struct GTable GTable;
typedef struct GFun GFun;
typedef struct alo_Type GType;
typedef struct alo_Env GRoute;
typedef struct GProto GProto;
typedef struct GBuf GBuf;
typedef struct GLoader GLoader;

typedef GTable GAUser;

/* Object pointer. */
typedef GObj* a_hobj;

/* GC support. */
typedef a_usize a_trmark;
typedef a_hobj a_gcnext;
typedef a_gcnext a_gclist;

typedef struct VTable VTable;
typedef struct IStr IStr;
typedef struct alo_Alloc Alloc;
typedef struct RcCap RcCap;
typedef struct Frame Frame;
typedef struct Stack Stack;
typedef struct Global Global;
typedef struct Loader Loader;
typedef struct TypeCache TypeCache;
typedef struct ByteBuf ByteBuf;

#define T_NIL u32c(0)
#define T_FALSE u32c(1)
#define T_TRUE u32c(2)
#define T_PTR u32c(3)
#define T_LIST u32c(4)
#define T_TABLE u32c(5)
#define T_TYPE u32c(6)
#define T_FUNC u32c(7)
#define T_ISTR u32c(8)
#define T_HSTR u32c(9)
#define T_TUPLE u32c(10)
#define T_AUSER u32c(11)
#define T_CUSER u32c(12)
#define T_INT u32c(14)
#define T_NAN u32c(15)

#define T__MIN_OBJ T_LIST
#define T__MAX_OBJ T_CUSER
#define T__MIN_NEQ T_HSTR
#define T__MAX_NEQ T_CUSER
#define T__MIN_NHH T_ISTR
#define T__MAX_NHH T_CUSER
#define T__MAX_FAST u32c(15)

#define T_FLOAT u32c(16)

#define g_cast(t,e) from_member(t, _obj_head_mark, &(e)->_obj_head_mark)
#define gobj_cast(e) cast(a_hobj, (e)->_obj_head_mark)

typedef struct { a_u64 _; } Value;

static_assert(sizeof(Value) == 8);

always_inline void v_check_alive(a_henv env, Value v);

#define V_TAG_MASK (~u64c(0) << 47)
#define V_PAYLOAD_MASK (~V_TAG_MASK)
#define V_INT_MASK (~(~u64c(0) << 32))

#define V_MASKED_TAG(t) (~cast(a_u64, t) << 47)
#define V_GET_TAG(r) (~(r) >> 47)
#define V_GET_PAYLOAD(r) ((r) & V_PAYLOAD_MASK)
#define V_IS(r,t) (((r) & V_TAG_MASK) == V_MASKED_TAG(t))

#define V_STRICT_NIL (~u64c(0))
#define V_EMPTY (~u64c(1))
#define V_FALSE V_MASKED_TAG(T_FALSE)
#define V_TRUE (V_MASKED_TAG(T_TRUE) | V_PAYLOAD_MASK)
#define V_FLOAT_MAX u64c(0xfff8000000000000)

static_assert(V_IS(V_STRICT_NIL, T_NIL));
static_assert(V_IS(V_EMPTY, T_NIL));
static_assert(V_IS(V_FALSE, T_FALSE));
static_assert(V_IS(V_TRUE, T_TRUE));
static_assert(V_TRUE + 1 == V_FALSE);
static_assert(V_IS(V_FLOAT_MAX, T_NAN));

always_inline a_u64 v_masked_tag(a_enum tag) {
	assume(tag <= T__MAX_FAST, "bad value tag.");
	return V_MASKED_TAG(tag);
}

always_inline a_u64 v_box_nan_raw(a_enum tag, a_u64 payload) {
	assume((payload & ~V_PAYLOAD_MASK) == 0, "bad value payload.");
	return v_masked_tag(tag) | payload;
}

always_inline a_u64 v_box_nan_raw_min(a_enum tag) {
	return v_masked_tag(tag);
}

always_inline a_u64 v_box_nan_raw_max(a_enum tag) {
	return v_masked_tag(tag) | V_PAYLOAD_MASK;
}

#define v_new(v) (new(Value) {v})

always_inline Value v_box_nan(a_enum tag, a_u64 payload) {
	return v_new(v_box_nan_raw(tag, payload));
}

#define v_get_tag(v) V_GET_TAG((v)._)

#define v_get_payload(v) V_GET_PAYLOAD((v)._)

#define v_is(v,t) V_IS((v)._, t)

#define v_is_in(v,tmn,tmx) ((v)._ - v_box_nan_raw_min(tmx) <= v_box_nan_raw_max(tmn) - v_box_nan_raw_min(tmx))

always_inline void v_set_raw(Value* d, Value v) {
	*d = v;
}

always_inline void v_set(a_henv env, Value* d, Value v) {
	v_set_raw(d, v);
	v_check_alive(env, v);
}

always_inline void v_cpy(a_henv env, Value* restrict d, Value const* restrict s) {
	v_set(env, d, *s);
}

always_inline void v_cpy_all(a_henv env, Value* restrict d, Value const* restrict s, a_usize n) {
	for (a_usize i = 0; i < n; ++i) {
		v_cpy(env, &d[i], &s[i]);
	}
}

/*=========================================================*
 * Primitives
 *=========================================================*/

#define v_is_nil(v) v_is(v, T_NIL)

#define v_is_strict_nil(v) ((v)._ == V_STRICT_NIL)

#define v_of_nil() v_new(V_STRICT_NIL)

always_inline void v_set_nil(Value* d) {
	v_set_raw(d, v_of_nil());
}

always_inline void v_set_nil_ranged(Value* l, Value* h) {
	for (Value* p = l; p < h; ++p) {
		v_set_nil(p);
	}
}

#define v_is_empty(v) ((v)._ == V_EMPTY)

#define v_of_empty() v_new(V_EMPTY)

#define v_is_bool(v) v_is_in(v, T_FALSE, T_TRUE)

#define v_to_bool(v) ((v)._ <= v_box_nan_raw_max(T_TRUE))

#define v_of_bool(v) v_new((v) ? V_TRUE : V_FALSE)

always_inline void v_set_bool(Value* d, a_bool v) {
	v_set_raw(d, v_of_bool(v));
}

#define v_is_int(v) v_is(v, T_INT)

always_inline a_int v_as_int(Value v) {
	assume(v_is_int(v), "not int value.");
	return cast(a_int, v._ & V_INT_MASK);
}

#define v_of_int(v) v_box_nan(T_INT, v)

always_inline void v_set_int(Value* d, a_int v) {
	v_set_raw(d, v_of_int(v));
}

#define v_is_float(v) ((v)._ <= v_box_nan_raw_max(T_NAN))

always_inline a_float v_as_float(Value v) {
	assume(v_is_float(v), "not float value.");
	return bit_cast(a_float, v._);
}

#define v_of_float(v) v_new(bit_cast(a_u64, v))

always_inline void v_set_float(Value* d, a_float v) {
	v_set_raw(d, v_of_float(v));
}

#define v_is_nan(v) v_is(v, T_NAN)

#define v_is_num(v) ((v)._ <= v_box_nan_raw_max(T_INT))

always_inline a_float v_as_num(Value v) {
	return v_is_int(v) ? v_as_int(v) : v_as_float(v);
}

#define v_is_ptr(v) v_is(v, T_PTR)

always_inline void* v_as_ptr(Value v) {
	assume(v_is_ptr(v), "not pointer.");
	return ptr_of(void, v_get_payload(v));
}

#define v_of_ptr(v) v_box_nan(T_PTR, addr_of(v))

always_inline void v_set_ptr(Value* d, void* v) {
	v_set_raw(d, v_of_ptr(v));
}

/*=========================================================*
 * Object
 *=========================================================*/

typedef struct { a_usize _; } ObjHeadMark[0];
typedef VTable const* a_vptr;
typedef void const* a_vslot;

#define GOBJ_STRUCT_HEADER ObjHeadMark _obj_head_mark; a_vptr _vptr; a_gcnext _gnext; a_trmark _tnext

struct GObj {
	GOBJ_STRUCT_HEADER;
	a_u32 _len;
};

/**
 ** The virtual table for type, used to dispatch internal methods.
 ** Primitive types do not have virtual table.
 */
struct VTable {
	/* The masked tag to the value from the pointer. */
	a_u64 _mask;
	/* The size and alignment information of object. */
	a_u16 _base_size;
	a_u8 _elem_size;
	/* The tag of layout. */
	a_u8 _tag;
	/* The properties of type. */
	a_u32 _flags;
	/* The unique indexed name of the type, equals to the displacement between type object and global. */
	a_usize _iname; /* 0 for no companion type. */
	/* The string name of the type. */
	char const* _sname;
	/* The virtual function pointers. */
	a_vslot const* _vfps;
};

#define v_is_obj(v) v_is_in(v, T__MIN_OBJ, T__MAX_OBJ)

always_inline GObj* v_as_obj(Value v) {
	assume(v_is_obj(v), "not object.");
	return ptr_of(GObj, v_get_payload(v));
}

always_inline Value v_of_obj(a_hobj v) {
	return v_new(v->_vptr->_mask | addr_of(v));
}

#define v_of_obj(v) v_of_obj(gobj_cast(v))

always_inline void v_set_obj(a_henv env, Value* d, a_hobj v) {
	v_set(env, d, v_of_obj(v));
}

#define v_set_obj(env,d,v) v_set_obj(env, d, gobj_cast(v))

/*=========================================================*
 * String
 *=========================================================*/

#define GSTR_STRUCT_HEADER \
	GOBJ_STRUCT_HEADER;    \
	a_u32 _len;            \
	a_hash _hash;          \
	a_byte _data[]

struct GStr {
	GSTR_STRUCT_HEADER;
};

struct IStr {
	IStr* _cache_next; /* Next node of overflow chain in intern table. */
	union {
		GStr _body;
		struct {
			GSTR_STRUCT_HEADER;
		};
	};
};

static_assert(offsetof(GObj, _len) == offsetof(GStr, _len));

#define v_is_istr(v) v_is(v, T_ISTR)

#define g_is_istr(p) ((p)->_vptr->_mask == V_MASKED_TAG(T_ISTR))

#define v_is_hstr(v) v_is(v, T_HSTR)

#define v_is_str(v) v_is_in(v, T_ISTR, T_HSTR)

always_inline GStr* v_as_str(Value v) {
	assume(v_is_str(v), "not string.");
	return g_cast(GStr, v_as_obj(v));
}

#define sizeof_HStr(l) pad_to_raw(sizeof(GStr) + (l) + 1, sizeof(a_usize))

#define sizeof_IStr(l) pad_to_raw(sizeof(IStr) + (l) + 1, sizeof(a_usize))

always_inline char const* str2ntstr(GStr* self) {
	return cast(char const*, self->_data);
}

/*=========================================================*
 * Tuple
 *=========================================================*/

struct GTuple {
	GOBJ_STRUCT_HEADER;
	a_u32 _len;
	a_hash _hash;
	Value _body[0];
};

static_assert(offsetof(GObj, _len) == offsetof(GTuple, _len));

#define v_is_tuple(v) v_is(v, T_TUPLE)

always_inline GTuple* v_as_tuple(Value v) {
	assume(v_is_tuple(v), "not tuple.");
	return g_cast(GTuple, v_as_obj(v));
}

#define sizeof_GTuple(l) (sizeof(GTuple) + sizeof(Value) * (l))

/*=========================================================*
 * List
 *=========================================================*/

struct GList {
	GOBJ_STRUCT_HEADER;
	a_u32 _len;
	a_u32 _cap;
	Value* _ptr;
};

static_assert(offsetof(GObj, _len) == offsetof(GList, _len));

#define v_is_list(v) v_is(v, T_LIST)

always_inline GList* v_as_list(Value v) {
	assume(v_is_list(v), "not list.");
	return g_cast(GList, v_as_obj(v));
}

/*=========================================================*
 * Table
 *=========================================================*/

typedef struct HNode HNode;
typedef struct HLink HLink;

struct HLink {
	a_x32 _prev;
	a_x32 _next;
};

/**
 ** Linked hash table.
 */
struct GTable {
	GOBJ_STRUCT_HEADER;
	a_u32 _len;
	a_u32 _hmask;
	HNode* _ptr; /* Data pointer. */
};

/**
 ** Table node.
 */
struct HNode {
	Value _value;
	Value _key;
	a_hash _hash;
	a_x32 _hnext;
	HLink _link;
};

static_assert(offsetof(GObj, _len) == offsetof(GTable, _len));

static_assert(offsetof(HNode, _hash) % sizeof(a_u64) == 0);

#define v_is_table(v) v_is(v, T_TABLE)

always_inline GTable* v_as_table(Value v) {
	assume(v_is_table(v), "not table.");
	return g_cast(GTable, v_as_obj(v));
}

#define hnode_is_empty(n) v_is_strict_nil((n)->_key)

/*=========================================================*
 * Function & Prototype
 *=========================================================*/

typedef struct LocalInfo LocalInfo;
typedef struct CapInfo CapInfo;
typedef struct LineInfo LineInfo;
typedef struct ProtoDesc ProtoDesc;

#define GFUN_STRUCT_HEADER \
	GOBJ_STRUCT_HEADER; \
	a_u32 _len; \
	a_u16 _flags; \
	a_u16 _fname /* Function name. */

struct GFun {
	GFUN_STRUCT_HEADER;
	union {
		a_cfun _fptr;
		GProto* _proto;
	};
	union {
		RcCap* _caps[0];
		Value _vals[0];
	};
};

#define GPROTO_STRUCT_HEADER \
	GOBJ_STRUCT_HEADER;         \
	a_u32 _size;                \
	a_u16 _flags;               \
	a_u8 _nstack;               \
	a_u8 _nparam;               \
	Value* _consts;             \
	a_insn* _code

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

/*=========================================================*
 * Userdata
 *=========================================================*/

#define v_is_auser(v) v_is(v, T_AUSER)

always_inline GAUser* v_as_auser(Value v) {
	assume(v_is_auser(v), "not userdata.");
	return g_cast(GAUser, v_as_obj(v));
}

#define v_is_cuser(v) v_is(v, T_CUSER)

#define v_is_user(v) v_is_in(v, T_AUSER, T_CUSER)

/*=========================================================*
 * Type
 *=========================================================*/

#define VTABLE_FLAG_NONE        u8c(0x00)
#define VTABLE_FLAG_GREEDY_MARK u8c(0x01)

#define vtable_has_flag(vt,f) (((vt)->_flags & (f)) != 0)

#define METHOD_LIST(_) \
	_( 0, drop  ,    void, Global* g                   ) \
	_( 1, mark  ,    void, Global* g                   ) \
	_( 2, close ,    void, a_henv env                  )

/* Method Slot Table. */
union MST {
#define DEF(i,n,r,p1,pn...) struct { a_vslot M_cat(_off_,n)[i]; r (*n)(p1, a_hobj, ##pn);  };
	METHOD_LIST(DEF)
#undef DEF
};

#define vfp_loc(f) (addr_of(&cast(union MST*, null)->f) / sizeof(a_vslot))
#define vfp_def(f,p) [vfp_loc(f)] = (p)

#define g_vcheck(p,f) ({ \
	a_vptr _v2 = (p)->_vptr; \
	a_vslot _f2 = _v2->_vfps[vfp_loc(f)]; \
	assume(_f2 != null, "method '%s."#f"' is null.", (_v2)->_sname); \
	cast(typeof(cast(union MST*, null)->f), _f2); \
})

#define g_vcall(r,p,f,a...) ({ \
	a_hobj _p = gobj_cast(p);  \
    (*g_vcheck(_p,f))(r, _p, ##a); \
})

typedef struct TDNode TDNode;

struct TDNode {
	GStr* _key;
	a_x32 _hnext;
	a_u32 _index;
};

/**
 ** Type (or Module).
 */
struct alo_Type {
	GOBJ_STRUCT_HEADER;
	a_u32 _size;

	a_u32 _sig; /* Type methods signature, changed when the order of existed fields changed. */
	a_u32 _nref; /* Reference counter. */

	a_u32 _len;
	a_u32 _hmask;

	a_u16 _flags;
	a_u8 _tag; /* The type tag. */

	GLoader* _loader;
	GStr* _name;
	TDNode* _ptr;
	Value* _values;

	GType* _next; /* Used for linked list in loader. */

	VTable _opt_vtbl[0];
};

struct TypeCache {
	GType** _table;
	a_u32 _hmask;
	a_u32 _len;
};

struct Loader {
	GLoader* _parent;
	TypeCache _cache;
};

struct GLoader {
	GOBJ_STRUCT_HEADER;
	Loader _body;
};

#define TYPE_FLAG_NONE u16c(0)
#define TYPE_FLAG_FAST_TM(tm) (u16c(1) << (tm))

#define v_is_type(v) v_is(v, T_TYPE)

always_inline GType* v_as_type(Value v) {
	assume(v_is_type(v), "not type.");
	return g_cast(GType, v_as_obj(v));
}

#define type_has_flag(t,f) (((t)->_flags & (f)) != 0)
#define type_has_method(t,tm) type_has_flag(t, TYPE_FLAG_FAST_TM(tm))

/*=========================================================*
 * Route
 *=========================================================*/

struct Stack {
	Value* _base;
	Value* _top;
	Value* _limit;
	a_usize _alloc_size; /* The actual allocate size.*/
};

#if ALO_STACK_RELOC
typedef a_isize StkPtr;
#else
typedef Value* StkPtr;
#endif

struct Frame {
	a_henv _env;
	Frame* _prev;
	a_insn const* _pc;
	StkPtr _stack_bot;
	StkPtr _bound; /* In strict stack checking mode, the API will use frame bound to check index range. */
	union {
		a_u8 _flags;
		struct {
			a_u8 _fvret: 1; /* Variable length result mark. */
			a_u8 _ftail: 1; /* Tail call mark. */
			a_u8 _fmeta: 1; /* Meta call mark. */
		};
	};
	a_u8 _nret;
};

typedef void* a_rctx;

struct alo_Env {
	GOBJ_STRUCT_HEADER;
	Global* _g;
	a_rctx _rctx;
	void* _rctx_alloc;
	GRoute* _from;
	Frame* _frame;
	Stack _stack;
	Value _error;
	a_u16 _flags;
	a_u8 _status;
	PCtx _pctx;
	RcCap* _open_caps;
	Frame _base_frame;
};

#if ALO_M64
static_assert(offsetof(GRoute, _rctx) == 0x20);
static_assert(offsetof(GRoute, _rctx_alloc) == 0x28);
static_assert(offsetof(GRoute, _from) == 0x30);
static_assert(offsetof(GRoute, _stack._base) == 0x40);
#else
static_assert(offsetof(GRoute, _rctx) == 0x10);
static_assert(offsetof(GRoute, _rctx_alloc) == 0x14);
static_assert(offsetof(GRoute, _from) == 0x18);
static_assert(offsetof(GRoute, _stack._base) == 0x20);
#endif

#define G(env) ((env)->_g)

/*=========================================================*
 * Global
 *=========================================================*/

typedef struct {
	IStr** _table;
	a_usize _len;
	a_usize _hmask; /* Hash code mask. */
} IStrCache;

typedef void (*a_fp_gexecpt)(a_henv env, void* ctx, a_msg msg);
typedef void (*a_fp_gmark)(Global* g, void* ctx);

struct Global {
	Alloc _af;
	void* _ac;
	a_hfun _hookf;
	a_hctx _hookc;
	a_cfun _panic;
	a_henv _active;
	a_usize _mem_base;
	a_isize _mem_debt;
	a_isize _mem_work;
	a_usize _mem_estimate;
	a_gclist _gc_normal;
	a_gclist _gc_fixed;
	a_gclist _gc_closable;
	a_gclist _gc_toclose;
	a_gcnext* _gc_sweep;
	RcCap* _cap_cache;
	Value _global;
	a_fp_gexecpt _gexecpt;
	a_fp_gmark _gmark;
	void* _gctx;
	a_trmark _tr_gray;
	a_trmark _tr_regray;
	GStr* _nomem_error;
	TypeCache _type_cache;
	IStrCache _istr_cache;
	a_hash _seed;
	a_u16 _gcpausemul;
	a_u16 _gcstepmul;
	a_u16 _flags;
	a_u8 _white_color;
	a_u8 _gcstep;
	volatile atomic_uint_fast8_t _hookm;
	GStr* _names[STR__COUNT];
	struct {
		GType _nil;
		GType _bool;
		GType _int;
		GType _ptr;
		GType _str;
		GType _tuple;
		GType _list;
		GType _table;
		GType _func;
		GType _type;
		GType _route;
		GType _float;
	} _types;
};

#define RFLAG_COUNT_VARARG UINT8_MAX

#define ALO_HMSWAP 0x80

always_inline void gbl_protect(a_henv env, a_fp_gmark mark, a_fp_gexecpt except, void* ctx) {
	assume(mark != null || except != null, "no protect function given.");
	Global* g = G(env);
	g->_gmark = mark;
	g->_gexecpt = except;
	g->_gctx = ctx;
}

always_inline void gbl_unprotect(a_henv env) {
	Global* g = G(env);
	g->_gmark = null;
	g->_gexecpt = null;
	g->_gctx = null;
}

always_inline a_usize gbl_mem_total(Global* g) {
	return g->_mem_base + cast(a_usize, g->_mem_debt);
}

always_inline GStr* env_int_str(a_henv env, a_u32 tag) {
	return G(env)->_names[tag - 1];
}

#define env_type(env,f) (&G(env)->_types.f)
#define env_type_iname(f) offsetof(Global, _types.f)

/*=========================================================*/

#define v_has_trivial_hash(v) (!v_is_in(v, T__MIN_NHH, T__MAX_NHH))

#define v_has_trivial_equals(v) (!v_is_in(v, T__MIN_NEQ, T__MAX_NEQ))

/* Identity hashcode. */
always_inline a_hash v_trivial_hash_unchecked(Value v) {
	a_u32 h = v._ * u32c(0xcc9e2d51);
	h = (h << 15) | (h >> 17);
	h *= u32c(0x1b873593);
	h ^= h >> 17;
	h *= u32c(0x85ebca6b);
	h ^= h >> 13;
	return h;
}

always_inline a_hash v_trivial_hash(Value v) {
	assume(v_has_trivial_hash(v), "no trivial hash.");
	return v_trivial_hash_unchecked(v);
}

/* Identity equality. */
#define v_trivial_equals_unchecked(v1,v2) ((v1)._ == (v2)._)

always_inline a_bool v_trivial_equals(Value v1, Value v2) {
	assume(v_has_trivial_equals(v1), "no trivial equals.");
	return v_trivial_equals_unchecked(v1, v2);
}

intern char const ai_obj_type_names[][8];

always_inline GType* g_typeof(a_henv env, a_hobj p) {
	assume(env->_vptr->_iname != 0, "object does not have type.");
	return ptr_disp(GType, G(env), p->_vptr->_iname);
}

always_inline char const* g_nameof(unused a_henv env, a_hobj p) {
	return p->_vptr->_sname;
}

always_inline GType* v_typeof(a_henv env, Value v) {
	if (v_is_float(v)) {
		return &G(env)->_types._float;
	}
	else if (!v_is_obj(v)) {
		switch (v_get_tag(v)) {
			case T_NIL:
				return env_type(env, _nil);
			case T_FALSE:
			case T_TRUE:
				return env_type(env, _bool);
			case T_INT:
				return env_type(env, _int);
			case T_PTR:
				return env_type(env, _ptr);
			default:
				panic("bad type tag.");
		}
	}
	else {
		return g_typeof(env, v_as_obj(v));
	}
}

always_inline char const* v_nameof(a_henv env, Value v) {
	if (v_is_float(v)) {
		return ai_obj_type_names[T_FLOAT];
	}
	else if (!v_is_user(v)) {
		return ai_obj_type_names[v_get_tag(v)];
	}
	else {
		return g_nameof(env, v_as_obj(v));
	}
}

#endif /* aobj_h_ */
