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
typedef union GType GType;
typedef struct GProto GProto;
typedef struct GBuf GBuf;

typedef struct Klass Klass;
typedef struct KLocal KLocal;
typedef struct KStack KStack;
typedef struct KHeap KHeap;
typedef struct KClose KClose;
typedef struct KWeak KWeak;

/* GC pointer. */
typedef GObj* a_gptr;

/* GC support. */
typedef a_usize a_trmark;
typedef a_gptr a_gcnext;
typedef a_gcnext a_gclist;

typedef struct Impl Impl;
typedef struct alo_Alloc Alloc;
typedef struct RcCap RcCap;
typedef struct Frame Frame;
typedef struct Stack Stack;
typedef struct Global Global;

typedef struct Value Value;

struct Value { a_u64 _; };

static_assert(sizeof(Value) == 8);

#define ALO_TBUF 14

enum {
    T_NIL = 0,
    T_FALSE = 1,
    T_TRUE = 2,
    T_PTR = 3,
    T_LIST = 4,
    T_TABLE = 5,
    T_FUNC = 6,
    T_STR = 7,
    T_TYPE = 8,
    T_TUPLE = 10,
    T_USER = 11,
    T_OTHER = 12,
    T_INT = 14,
    T_NAN = 15,

    T__MAX = T_NAN,

    T__MIN_REF = T_LIST,
    T__MAX_REF = T_OTHER,
    T__MIN_FLT = 16,
    T__MAX_FLT = UINT32_MAX,
    T__MIN_NEQ = T_TUPLE,
    T__MAX_NEQ = T_USER,
    T__MIN_NHH = T_TUPLE,
    T__MAX_NHH = T_USER,
};

#define T_OBJ T__MIN_REF ... T__MAX_REF

#define T_FLOAT T__MIN_FLT ... T__MAX_FLT

/* Defined in aenv.h */
always_inline a_bool v_is_alive(Global* gbl, Value v);

always_inline void v_check_alive(a_henv env, Value v);

#define V_TAG_SHIFT 47
#define V_TAG_MASK (~u64c(0) << V_TAG_SHIFT)
#define V_DATA_MASK (~V_TAG_MASK)
#define V_INT_MASK (~(~u64c(0) << 32))

#define V_STENCIL(t) (~cast(a_u64, t) << V_TAG_SHIFT)
#define V_GET_TAG(r) (~(r) >> 47)
#define V_GET_DATA(r) ((r) & V_DATA_MASK)
#define V_IS(r,t) (((r) & V_TAG_MASK) == V_STENCIL(t))

always_inline a_u64 v_stencil(a_enum tag) {
    assume(tag <= T__MAX, "bad value tag.");
    return V_STENCIL(tag);
}

always_inline a_u64 v_box_nan_raw(a_enum tag, a_u64 payload) {
    assume((payload & ~V_DATA_MASK) == 0, "bad value payload.");
    return v_stencil(tag) | payload;
}

always_inline a_u64 v_box_nan_raw_min(a_enum tag) {
    return v_stencil(tag);
}

always_inline a_u64 v_box_nan_raw_max(a_enum tag) {
    return v_stencil(tag) | V_DATA_MASK;
}

#define v_new(v) ((Value) {v})

always_inline Value v_box_nan(a_enum tag, a_u64 payload) {
    return v_new(v_box_nan_raw(tag, payload));
}

#define v_get_tag(v) V_GET_TAG((v)._)

#define v_get_data(v) V_GET_DATA((v)._)

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
    assume(d <= s, "move regions violate contract.");
    for (a_usize i = 0; i < n; ++i) {
        v_cpy(env, &d[i], &s[i]);
    }
}

always_inline void v_mov_all_bwd(a_henv env, Value* d, Value const* s, a_usize n) {
    assume(d >= s, "move regions violate contract.");
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
#define V_TRUE (V_STENCIL(T_TRUE) | V_DATA_MASK)
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
    return int2ptr(void, v_get_data(v));
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
always_inline a_hash v_trivial_hash(Value v) {
    a_u32 h = v._ ^ (v._ >> 32);
    h *= u32c(0x1000193);
    h += h << 13;
    h ^= h >> 7;
    h += h << 3;
    h ^= h >> 17;
    h += h << 5;
    return h;
}

always_inline a_hash v_float_hash(Value v) {
    a_float f = v_as_float(v);
    if (f == 0.0) return 0; /* Special case for 0.0 and -0.0 */
    return v_trivial_hash(v);
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
 * Reference
 *=========================================================*/

#define REFERENCE_STRONG  u32c(0x00)
#define REFERENCE_WEAK    u32c(0x01)
#define REFERENCE_WEAKKEY u32c(0x02)
#define REFERENCE_PHANTOM u32c(0x03)

#define GOBJ_STRUCT_HEADER \
    a_byte _obj_head[0];   \
    void const* klass;     \
    a_gcnext gnext;        \
    a_trmark tnext

struct GObj {
    GOBJ_STRUCT_HEADER;
};

#define KOBJ_STRUCT_HEADER \
    a_byte _kls_head[0];   \
    char const* name;      \
    a_u32 tag;             \
    a_u32 flags

#define KOBj_METHODS(_) \
    _(drop, void(Global* gbl, a_gptr self))

#define KHEAP_METHODS(_) \
    _(drop, void(Global* gbl, a_gptr self))

#define KSTACK_METHODS(_) \
    _(close, void(a_henv env, a_gptr self)) \
    _(close, void(a_henv env, a_gptr self)) \


#define KANY_METHODS(_) \
    _(drop, void(Global* gbl, a_gptr self)) \
    _(mark, void(Global* gbl, a_gptr self)) \
    _(close, void(a_henv env, a_gptr self)) \
    _(drain, void(Global* gbl, a_gptr self))

/**
 ** The virtual table for type, used for fast dispatch.
 ** Primitive types do not have virtual table.
 */
struct Klass {
    KOBJ_STRUCT_HEADER;
    /* GObj */
    void (*mark)(Global* gbl, a_gptr self);
    union {
        /* GStack */
        void (*catch)(a_henv gbl, a_gptr self, a_i32 msg);
        struct {
            /* GHeap */
            void (*drop)(Global* gbl, a_gptr self);
            union {
                /* GClose */
                void (*close)(a_henv env, a_gptr self);
                /* GWeak */
                void (*clear)(Global* gbl, a_gptr self);
            };
        };
    };
};

struct KLocal {
    KOBJ_STRUCT_HEADER;
    void const* mark;
};

struct KStack {
    KOBJ_STRUCT_HEADER;
    void const* mark;
    void const* catch;
};

struct KHeap {
    KOBJ_STRUCT_HEADER;
    void const* mark;
    void const* drop;
};

struct KClose {
    KOBJ_STRUCT_HEADER;
    void const* mark;
    void const* drop;
    void const* close;
};

struct KWeak {
    KOBJ_STRUCT_HEADER;
    void const* mark;
    void const* drop;
    void const* clear;
};

#define KLASS_FLAG_NONE    u8c(0x00)
/**
 ** The reference has plain data, which assumes:
 ** 1. the value has no managed field stored the value.
 */
#define KLASS_FLAG_PLAIN   u8c(0x01)
/**
 ** The reference will be regard as a 'value', which assumes:
 ** 1. the value has no reference identity in equality and hashing method.
 ** 2. the value will always regard as an alive value in weak reference and so on.
 ** 3. the value should be immutable.
 */
#define KLASS_FLAG_VALUE   u8c(0x02)
/**
 ** The reference is build by inner custom type constructor.
 */
#define KLASS_FLAG_BUILD   u8c(0x04)
/**
 ** THe reference with hidden should not exposed in API.
 */
#define KLASS_FLAG_HIDDEN  u8c(0x08)

#define k_has_flag(k,f) (((k)->flags & (f)) != 0)

#define k_eq(k1,k2) ((k1)->_kls_head == (k2)->_kls_head)

#define g_klass(o) cast(Klass const*, (o)->klass)

#define g_tag(o) (g_klass(o)->tag)

#define g_is(o,t) (g_tag(o) == (t))

#define g_fetch(o,f) ({ \
	auto _f2 = g_klass(o)->f; \
	assume(_f2 != null, "method '"#f"' is null."); \
	_f2;                    \
})

#define g_as(t,o) from_member(t, _obj_head, &(o)->_obj_head)
#define g_as_ref(o) g_as(GObj, o)

#define obj_idx(k,l,f) ({ \
    a_int _k = k; a_uint _l = l; \
    a_uint _i = _k >= 0 ? cast(a_uint, _k) : cast(a_uint, _k) + _l; \
    if (unlikely(_i >= _l)) return f;   \
    _i;                 \
})

#define v_is_ref(v) v_in(v, T__MIN_REF, T__MAX_REF)

always_inline a_gptr v_as_ref(Value v) {
    assume(v_is_ref(v), "not reference.");
    return int2ptr(GObj, v_get_data(v));
}

always_inline Value v_of_ref(a_gptr o, a_enum t) {
    return v_new(V_STENCIL(t) | ptr2int(o));
}

#define v_of_ref(o,t) v_of_ref(g_as_ref(o), t)

#define v_of_other(o) v_of_ref(o, T_OTHER)

always_inline void v_set_other(a_henv env, Value* d, a_gptr o) {
    v_set(env, d, v_of_other(o));
}

#define v_set_other(env,d,o) v_set_other(env, d, g_as_ref(o))

#endif /* aobj_h_ */
