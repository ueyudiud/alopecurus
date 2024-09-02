/**
 *@file aobj.h
 */

#ifndef aobj_h_
#define aobj_h_

#include "adef.h"

typedef struct GObj GObj;
typedef struct GStr GStr;
typedef struct GTuple GTuple;
typedef struct GList GList;
typedef struct GTable GTable;
typedef struct GFun GFun;
typedef struct GUser GUser;
typedef struct alo_Env GRoute;
typedef struct GMod GMod;
typedef struct GType GType;
typedef struct GProto GProto;
typedef struct GBuf GBuf;

/* GC pointer. */
typedef GObj* a_gptr;

/* GC support. */
typedef a_usize a_trmark;
typedef a_gptr a_gcnext;
typedef a_gcnext a_gclist;

typedef struct VTable VTable;
typedef struct alo_Alloc Alloc;
typedef struct RcCap RcCap;
typedef struct Frame Frame;
typedef struct Stack Stack;
typedef struct PFrame PFrame;
typedef struct Global Global;

typedef struct Value Value;

struct Value { a_u64 _; };

static_assert(sizeof(Value) == 8);

#define VALUE_TAG_SHIFT 47

enum {
    T_NIL = 0,
    T_FALSE = 1,
    T_TRUE = 2,
    T_PTR = 3,
    T_LIST = 4,
    T_TABLE = 5,
    T_FUNC = 6,
    T_STR = 7,
    T_MOD = 8,
    T_TUPLE = 10,
    T_USER = 11,
    T_INT = 14,
    T_NAN = 15,

    T__MAX_FAST = T_NAN,

    T__MIN_OBJ = T_LIST,
    T__MAX_OBJ = T_USER,
    T__MIN_FLT = 16,
    T__MAX_FLT = UINT32_MAX,
    T__MIN_NEQ = T_TUPLE,
    T__MAX_NEQ = T_USER,
    T__MIN_NHH = T_TUPLE,
    T__MAX_NHH = T_USER,
};

#define T_OBJ T__MIN_OBJ ... T__MAX_OBJ

#define T_FLOAT T__MIN_FLT ... T__MAX_FLT

/* Defined in aenv.h */
always_inline void v_check_alive(a_henv env, Value v);

#define V_TAG_MASK (~u64c(0) << VALUE_TAG_SHIFT)
#define V_PAYLOAD_MASK (~V_TAG_MASK)
#define V_INT_MASK (~(~u64c(0) << 32))

#define V_STENCIL(t) (~cast(a_u64, t) << VALUE_TAG_SHIFT)
#define V_GET_TAG(r) (~(r) >> 47)
#define V_GET_PAYLOAD(r) ((r) & V_PAYLOAD_MASK)
#define V_IS(r,t) (((r) & V_TAG_MASK) == V_STENCIL(t))

always_inline a_u64 v_stencil(a_enum tag) {
    assume(tag <= T__MAX_FAST, "bad value tag.");
    return V_STENCIL(tag);
}

always_inline a_u64 v_box_nan_raw(a_enum tag, a_u64 payload) {
    assume((payload & ~V_PAYLOAD_MASK) == 0, "bad value payload.");
    return v_stencil(tag) | payload;
}

always_inline a_u64 v_box_nan_raw_min(a_enum tag) {
    return v_stencil(tag);
}

always_inline a_u64 v_box_nan_raw_max(a_enum tag) {
    return v_stencil(tag) | V_PAYLOAD_MASK;
}

#define v_new(v) ((Value) {v})

always_inline Value v_box_nan(a_enum tag, a_u64 payload) {
    return v_new(v_box_nan_raw(tag, payload));
}

#define v_get_tag(v) V_GET_TAG((v)._)

#define v_get_payload(v) V_GET_PAYLOAD((v)._)

#define v_is(v,t) V_IS((v)._, t)

#define v_in(v,tmn,tmx) ((v)._ - v_box_nan_raw_min(tmx) <= v_box_nan_raw_max(tmn) - v_box_nan_raw_min(tmx))

/*=========================================================*
 * Value to Value Operations
 *=========================================================*/

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
    assume(d + n < s || s + n < d, "copy regions overlapping.");
    for (a_usize i = 0; i < n; ++i) {
        v_cpy(env, &d[i], &s[i]);
    }
}

always_inline void v_mov_all_fwd(a_henv env, Value* d, Value const* s, a_usize n) {
    assume(d <= s, "move regions not at forward.");
    for (a_usize i = 0; i < n; ++i) {
        v_cpy(env, &d[i], &s[i]);
    }
}

always_inline void v_mov_all_bwd(a_henv env, Value* d, Value const* s, a_usize n) {
    assume(d >= s, "move regions not at backward.");
    for (a_usize i = n - 1; i < n; --i) {
        v_cpy(env, &d[i], &s[i]);
    }
}

always_inline void v_swap(a_henv env, Value* v1, Value* v2) {
    Value v;
    v_cpy(env, &v, v1);
    v_cpy(env, v1, v2);
    v_cpy(env, v2, &v);
}

always_inline void v_reverse(a_henv env, Value* vl, Value* vh) {
    vh -= 1;
    while (vl < vh) {
        v_swap(env, vl, vh);
        vl += 1;
        vh -= 1;
    }
}

/*=========================================================*
 * Nil & Controller
 *=========================================================*/

#define V_STRICT_NIL (~-(u64c(0) + ALO_SOK))
#define V_EMPTY      (~-(u64c(0) + ALO_EEMPTY))

static_assert(V_IS(V_STRICT_NIL, T_NIL));
static_assert(V_IS(V_EMPTY, T_NIL));

#define v_is_nil(v) v_is(v, T_NIL)

#define v_is_strict_nil(v) ((v)._ == V_STRICT_NIL)
#define v_is_empty(v) ((v)._ == V_EMPTY)

#define v_of_nil() v_new(V_STRICT_NIL)
#define v_of_empty() v_new(V_EMPTY)

always_inline void v_set_nil(Value* d) {
    v_set_raw(d, v_of_nil());
}

always_inline void v_set_nil_ranged(Value* l, Value* h) {
    for (Value* p = l; p < h; ++p) {
        v_set_nil(p);
    }
}

/*=========================================================*
 * Primitives
 *=========================================================*/

#define V_FALSE V_STENCIL(T_FALSE)
#define V_TRUE (V_STENCIL(T_TRUE) | V_PAYLOAD_MASK)
#define V_FLOAT_MAX u64c(0xfff8000000000000)

static_assert(V_IS(V_FALSE, T_FALSE));
static_assert(V_IS(V_TRUE, T_TRUE));
static_assert(V_TRUE + 1 == V_FALSE);
static_assert(V_IS(V_FLOAT_MAX, T_NAN));

#define v_is_bool(v) v_in(v, T_FALSE, T_TRUE)

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

#define v_of_int(v) v_box_nan(T_INT, cast(a_uint, v))

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
    return int2ptr(void, v_get_payload(v));
}

#define v_of_ptr(v) v_box_nan(T_PTR, ptr2int(v))

always_inline void v_set_ptr(Value* d, void const* v) {
    v_set_raw(d, v_of_ptr(v));
}

/*=========================================================*
 * Hash & Equality
 *=========================================================*/

#define v_has_trivial_hash(v) (!v_in(v, T__MIN_NHH, T__MAX_NHH))

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

always_inline a_hash v_float_hash(Value v) {
    a_float f = v_as_float(v);
    if (f == 0.0) return 0; /* Special case for 0.0 and -0.0 */
    return v_trivial_hash_unchecked(v);
}

always_inline Value v_float_key(Value v) {
    a_float f = v_as_float(v);
    if (f == 0.0) return v_of_float(0.0); /* Special case for 0.0 and -0.0 */
    return v;
}

#define v_has_trivial_equals(v) (!v_in(v, T__MIN_NEQ, T__MAX_NEQ))

/* Identity equality. */
#define v_trivial_equals_unchecked(v1,v2) ((v1)._ == (v2)._)

always_inline a_bool v_trivial_equals(Value v1, Value v2) {
    assume(v_has_trivial_equals(v1), "object does not have trivial equality.");
    return v_trivial_equals_unchecked(v1, v2);
}

/*=========================================================*
 * Object
 *=========================================================*/

struct ObjHead { a_u64 _; };

#define GOBJ_STRUCT_HEADER struct ObjHead _obj_head_mark[0]; a_gcnext gnext; VTable const* vptr; a_trmark tnext

struct GObj {
    GOBJ_STRUCT_HEADER;
};

#define VTABLE_METHOD_LIST(_) \
	_( 0, drop  ,    void, Global* gbl                                      ) \
	_( 1, mark  ,    void, Global* gbl                                      ) \
	_( 2, close ,    void, a_henv env                                       ) \
	_( 3, except,    void, a_henv env, a_msg msg                            )

/* Method Table. */
typedef struct {
#define DEF(i,n,r,p1,pn...) r (*n)(p1, a_gptr, ##pn);
    VTABLE_METHOD_LIST(DEF)
#undef DEF
} ImplTable;

/**
 ** The virtual table for type, used for fast dispatch.
 ** Primitive types do not have virtual table.
 */
struct VTable {
    /* The stencil for value representation. */
    a_u64 stencil;
    /* The API tag of object. */
    a_u32 tag;
    /* The flags for virtual table. */
    a_u32 flags;
    /* The metadata used to describe virtual table. */
    void const* meta;
    /* The handle of type object (optional). */
    a_usize type_ref;
    /* The implement table. */
    ImplTable impl;
};

#define VTABLE_FLAG_NONE        u8c(0x00)
#define VTABLE_FLAG_GREEDY_MARK u8c(0x01)
#define VTABLE_FLAG_STACK_ALLOC u8c(0x02)

#define vtable_has_flag(vt,f) (((vt)->flags & (f)) != 0)

#define g_impl(p) (&(p)->vptr->impl)

#define g_fetch(p,f) ({ \
	auto _f2 = g_impl(p)->f; \
	assume(_f2 != null, "method '"#f"' is null."); \
	_f2;                    \
})

#define g_cast(t,o) from_member(t, _obj_head_mark, &(o)->_obj_head_mark)
#define gobj_cast(o) g_cast(GObj, o)

#define obj_idx(k,l,f) ({ \
    a_int _k = k; a_uint _l = l; \
    a_uint _i = _k >= 0 ? cast(a_uint, _k) : cast(a_uint, _k) + _l; \
    if (unlikely(_i >= _l)) return f;   \
    _i;                 \
})

#define v_is_obj(v) v_in(v, T__MIN_OBJ, T__MAX_OBJ)

always_inline a_gptr v_as_obj(Value v) {
    assume(v_is_obj(v), "not object.");
    return int2ptr(GObj, v_get_payload(v));
}

always_inline Value v_of_obj(a_gptr o) {
    return v_new(o->vptr->stencil | ptr2int(o));
}

#define v_of_obj(o) v_of_obj(gobj_cast(o))

always_inline void v_set_obj(a_henv env, Value* d, a_gptr o) {
    v_set(env, d, v_of_obj(o));
}

#define v_set_obj(env,d,v) v_set_obj(env, d, gobj_cast(v))

#endif /* aobj_h_ */
