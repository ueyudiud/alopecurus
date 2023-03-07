/*
 * aobj.h
 */

#ifndef aobj_h_
#define aobj_h_

#include <math.h>

#include "adef.h"

typedef struct GObj GObj;
typedef struct GStr GStr;
typedef struct GTuple GTuple;
typedef struct GList GList;
typedef struct GTable GTable;
typedef struct GFun GFun;
typedef struct alo_Env GRoute;
typedef struct alo_Mod GMod;
typedef struct GBuf GBuf;

/* Object pointer. */
typedef GObj* a_hobj;

/* GC support. */
typedef a_usize a_trmark;
typedef a_hobj a_gcnext;
typedef a_gcnext a_gclist;

typedef struct RcCap RcCap;
typedef struct Frame Frame;
typedef struct Global Global;
typedef struct alo_Alloc Alloc;
typedef struct Buf Buf;

#define T_NIL u32c(0)
#define T_FALSE u32c(1)
#define T_TRUE u32c(2)
#define T_PTR u32c(3)
#define T_USER_TEQ u32c(4)
#define T_LIST u32c(5)
#define T_TABLE u32c(6)
#define T_MOD u32c(7)
#define T_FUNC u32c(8)
#define T_ISTR u32c(9)
#define T_HSTR u32c(10)
#define T_TUPLE u32c(11)
#define T_USER_NEQ u32c(12)
#define T_CAP u32c(13)
#define T_INT u32c(14)
#define T_NAN u32c(15)

#define T__MIN_OBJ T_USER_TEQ
#define T__MAX_OBJ T_USER_NEQ
#define T__MIN_NEQ T_HSTR
#define T__MAX_NEQ T_USER_NEQ
#define T__MIN_NHH T_ISTR
#define T__MAX_NHH T_USER_NEQ
#define T__MAX_FAST u32c(15)

#define T_FLOAT u32c(16)

enum {
	REPR_OPAQUE, /* The layout of structure is not observable for VM. */
	REPR_STR,
	REPR_TUPLE,
	REPR_LIST,
	REPR_TABLE,
	REPR_FUNC,

	REPR_EMPTY = UINT8_MAX /* The layout of struct is empty, use for trait to mixin. */
};

#define g_cast(t,e) from_member(t, _obj_head_mark, &(e)->_obj_head_mark)
#define gobj_cast(e) cast(a_hobj, (e)->_obj_head_mark)

typedef struct { a_u64 _; } Value;

typedef void (*a_fp_mark)(Global* g, a_hobj self);
typedef void (*a_fp_drop)(Global* g, a_hobj self);
typedef void (*a_fp_close)(a_henv env, a_hobj self);
typedef Value (*a_fp_vlook)(a_henv env, a_hobj self, a_enum op);
typedef Value (*a_fp_get)(a_henv env, a_hobj self, Value index);
typedef void (*a_fp_set)(a_henv env, a_hobj self, Value index, Value value);
typedef a_hash (*a_fp_hash)(a_henv env, a_hobj self);
typedef a_bool (*a_fp_equals)(a_henv env, a_hobj self, Value other);
typedef void (*a_fp_tostr)(a_henv env, a_hobj self, GBuf* buf);

typedef struct VTable VTable;

struct VTable {
	a_u8 _tid; /* The value type id. */
	a_u8 _api_tag; /* The tag for API. */
	a_u8 _repr_id; /* The layout index for object. */
	a_u16 _flags;
	char const* _name;
	VTable const* _impl;
	/* Resource management functions. */
	a_fp_mark _mark;
	a_fp_drop _drop;
	a_fp_close _close;
	/* Virtual functions. */
	a_fp_get _get;
	a_fp_set _set;
	a_fp_hash _hash;
	a_fp_equals _equals;
	a_fp_tostr _tostr;
};

typedef struct { a_usize _; } ObjHeadMark[0];

#define GOBJ_STRUCT_HEADER ObjHeadMark _obj_head_mark; a_gcnext _gnext; a_trmark _tnext; VTable const* _vtable

struct GObj {
	GOBJ_STRUCT_HEADER;
};

#define VTABLE_FLAG_NONE u16c(0)
#define VTABLE_FLAG_OVERRIDE(tm) (u32c(1) << (tm))
#define VTABLE_FLAG_PLAIN_MARK u16c(0x0100)
#define VTABLE_FLAG_FAST_LEN u16c(0x0200)
#define VTABLE_FLAG_VLOOKUP u16c(0x0400)

always_inline void v_check_alive(a_henv env, Value v);

#define V_PAYLOAD_MASK (~(~u64c(0) << 47))
#define V_INT_MASK (~(~u64c(0) << 32))

always_inline a_u64 v_box_nan_raw(a_enum tag, a_u64 payload) {
	assume(tag <= T__MAX_FAST, "bad value tag.");
	assume((payload & ~V_PAYLOAD_MASK) == 0, "bad value payload.");
	return ~cast(a_u64, tag) << 47 | payload;
}

always_inline a_u64 v_box_nan_raw_min(a_enum tag) {
	return v_box_nan_raw(tag, 0);
}

always_inline a_u64 v_box_nan_raw_max(a_enum tag) {
	return v_box_nan_raw(tag, V_PAYLOAD_MASK);
}

always_inline Value v_box_nan(a_enum tag, a_u64 payload) {
	return new(Value) { v_box_nan_raw(tag, payload) };
}

always_inline a_enum v_get_tag(Value v) {
	return ~v._ >> 47;
}

always_inline a_u64 v_get_payload(Value v) {
	return v._ & V_PAYLOAD_MASK;
}

#define V_STRICT_NIL ~u64c(0)
#define V_EMPTY ~u32c(1)

always_inline a_bool v_is_nil(Value v) {
	return v_get_tag(v) == T_NIL;
}

always_inline a_bool v_is_strict_nil(Value v) {
	return v._ == V_STRICT_NIL;
}

always_inline a_bool v_is_empty(Value v) {
	return v._ == V_EMPTY;
}

always_inline a_bool v_is_false(Value v) {
	return v_get_tag(v) == T_FALSE;
}

always_inline a_bool v_is_true(Value v) {
	return v_get_tag(v) == T_TRUE;
}

always_inline a_bool v_is_int(Value v) {
	return v_get_tag(v) == T_INT;
}

always_inline a_bool v_is_ptr(Value v) {
	return v_get_tag(v) == T_PTR;
}

always_inline a_bool v_is_istr(Value v) {
	return v_get_tag(v) == T_ISTR;
}

always_inline a_bool v_is_hstr(Value v) {
	return v_get_tag(v) == T_HSTR;
}

always_inline a_bool v_is_obj(Value v) {
	return v._ - v_box_nan_raw_min(T__MAX_OBJ) <= v_box_nan_raw_max(T__MIN_OBJ) - v_box_nan_raw_min(T__MAX_OBJ);
}

always_inline a_bool v_is_str(Value v) {
	return v._ - v_box_nan_raw_min(T_HSTR) <= v_box_nan_raw_max(T_ISTR) - v_box_nan_raw_min(T_HSTR);
}

always_inline a_bool v_is_tuple(Value v) {
	return v_get_tag(v) == T_TUPLE;
}

always_inline a_bool v_is_list(Value v) {
	return v_get_tag(v) == T_LIST;
}

always_inline a_bool v_is_table(Value v) {
	return v_get_tag(v) == T_TABLE;
}

always_inline a_bool v_is_func(Value v) {
	return v_get_tag(v) == T_FUNC;
}

always_inline a_bool v_is_mod(Value v) {
	return v_get_tag(v) == T_MOD;
}

always_inline a_bool v_is_cap(Value v) {
	return v_get_tag(v) == T_CAP;
}

always_inline a_bool v_is_float(Value v) {
	return v._ <= v_box_nan_raw_max(T_NAN);
}

always_inline a_bool v_is_num(Value v) {
	return v._ <= v_box_nan_raw_max(T_INT);
}

always_inline a_bool v_is_user(Value v) {
	return v_get_tag(v) == T_USER_NEQ || v_get_tag(v) == T_USER_TEQ;
}

always_inline a_bool v_to_bool(Value v) {
	return v._ <= v_box_nan_raw_max(T_TRUE);
}

always_inline a_int v_as_int(Value v) {
	assume(v_is_int(v), "not int value.");
	return cast(a_int, v._ & V_INT_MASK);
}

always_inline a_float v_as_float(Value v) {
	assume(v_is_float(v), "not float value.");
	return bcast(a_float, v._);
}

always_inline a_bool v_is_nan(Value v) {
	return v_is_float(v) && isnan(v_as_float(v));
}

always_inline a_float v_as_num(Value v) {
	return v_is_int(v) ? v_as_int(v) : v_as_float(v);
}

always_inline void* v_as_ptr_raw(Value v) {
	return ptr_of(void, v_get_payload(v));
}

always_inline void* v_as_ptr(Value v) {
	assume(v_is_ptr(v), "not pointer.");
	return v_as_ptr_raw(v);
}

always_inline GObj* v_as_obj(Value v) {
	assume(v_is_obj(v), "not object.");
	return cast(GObj*, v_as_ptr_raw(v));
}

always_inline RcCap* v_as_cap(Value v) {
	assume(v_is_cap(v));
	return cast(RcCap*, v_as_ptr_raw(v));
}

#define v_of_nil() (new(Value) { V_STRICT_NIL })
#define v_of_empty() (new(Value) { V_EMPTY })
#define v_of_bool(v) ((v) ? v_box_nan(T_TRUE, V_PAYLOAD_MASK) : v_box_nan(T_FALSE, 0))
#define v_of_int(v) v_box_nan(T_INT, v)
#define v_of_float(v) (new(Value) { ._ = bcast(a_u64, v) })
#define v_of_ptr(v) v_box_nan(T_PTR, addr_of(v))

always_inline Value v_of_obj(a_hobj v) {
	a_u32 id = cast(a_u32, v->_vtable->_tid);
	return v_box_nan(min(id, T__MAX_OBJ), addr_of(v));
}

#define v_of_obj(v) v_of_obj(gobj_cast(v))

always_inline Value v_of_cap(RcCap* v) {
	return v_box_nan(T_CAP, addr_of(v));
}

always_inline void v_setx(Value* d, Value v) {
	*d = v;
}

always_inline void v_set(a_henv env, Value* d, Value v) {
	*d = v;
	v_check_alive(env, v);
}

always_inline void v_set_nil(Value* d) {
	v_setx(d, v_of_nil());
}

always_inline void v_set_bool(Value* d, a_bool v) {
	v_setx(d, v_of_bool(v));
}

always_inline void v_set_int(Value* d, a_int v) {
	v_setx(d, v_of_int(v));
}

always_inline void v_set_float(Value* d, a_float v) {
	v_setx(d, v_of_float(v));
}

always_inline void v_set_obj(a_henv env, Value* d, a_hobj v) {
	v_set(env, d, v_of_obj(v));
}

#define v_set_obj(env,d,v) v_set_obj(env, d, gobj_cast(v))

always_inline void v_cpy(a_henv env, Value* d, Value const* s) {
	v_set(env, d, *s);
}

always_inline void v_cpy_multi(a_henv env, Value* restrict d, Value const* restrict s, a_usize n) {
	for (a_usize i = 0; i < n; ++i) {
		v_cpy(env, &d[i], &s[i]);
	}
}

always_inline a_bool v_has_trivial_hash(Value v) {
	return v._ - v_box_nan_raw_min(T__MAX_NHH) > v_box_nan_raw_max(T__MIN_NHH) - v_box_nan_raw_min(T__MAX_NHH);
}

always_inline a_bool v_has_trivial_equals(Value v) {
	return v._ - v_box_nan_raw_min(T__MAX_NEQ) > v_box_nan_raw_max(T__MIN_NEQ) - v_box_nan_raw_min(T__MAX_NEQ);
}

always_inline a_hash v_trivial_hash(Value v) {
	assume(v_has_trivial_hash(v), "bad hash.");
	return v._ >> 32 ^ v._;
}

/* Identity equality. */
always_inline a_bool v_trivial_equals(Value v1, Value v2) {
	assume(v_has_trivial_equals(v1) && v_has_trivial_equals(v2));
	return v1._ == v2._;
}

intern char const* const ai_obj_tag_name[];

#endif /* aobj_h_ */
