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
typedef struct GMeta GMeta;
typedef struct alo_Mod GMod;

/* Object fptr. */
typedef GObj* a_hobj;

/* GC support. */
typedef a_usize a_trmark;
typedef a_hobj a_gcnext;
typedef a_gcnext a_gclist;

typedef struct Capture Capture;
typedef struct Frame Frame;
typedef struct Global Global;
typedef struct alo_Alloc Alloc;

#define T_NIL u32c(0)
#define T_FALSE u32c(1)
#define T_TRUE u32c(2)
#define T_PTR u32c(3)
#define T_ISTR u32c(4)
#define T_HSTR u32c(5)
#define T_TUPLE u32c(6)
#define T_LIST u32c(7)
#define T_TABLE u32c(8)
#define T_FUNC u32c(9)
#define T_MOD u32c(10)
#define T_OTHER u32c(11)
#define T_CAP u32c(12)
#define T_INT u32c(13)

#define T__MIN_REF T_ISTR
#define T__MAX_REF T_OTHER
#define T__MAX u32c(15)

#define T_FLOAT u32c(14)

#define LO_DFL_VALUE (~u32c(0))

#define g_cast(t,e) cast(typeof(t)*, (e)->_obj_head_mark)
#define gobj_cast(e) cast(a_hobj, (e)->_obj_head_mark)

typedef union Value {
	a_f64 _f;
	a_u64 _u;
	struct {
		union {
			a_u32 _l;
			a_i32 _i;
		};
		a_u32 _h;
	};
} Value;

struct ObjHeadMark_ {
	a_usize _; /* Make sure the head mark alignment equals to machine size. */
};

typedef struct ObjHeadMark_ ObjHeadMark[0];

#define GOBJ_STRUCT_HEADER ObjHeadMark _obj_head_mark; a_gcnext _gnext; a_trmark _tnext; GMeta* _meta

struct GObj {
	GOBJ_STRUCT_HEADER;
};

typedef void (*a_fp_splash)(Global* g, a_hobj self);
typedef void (*a_fp_destruct)(Global* g, a_hobj self);
typedef Value (*a_fp_get)(a_henv env, a_hobj self, Value index);

#define GMETA_FLAG_IDENTITY_EQUAL u16c(0x0001)
#define GTYPE_FLAG_FAST_LENGTH u16c(0x0002)

typedef struct {
	a_fp_splash _splash;
	a_fp_destruct _destruct;
	a_fp_get _get;
} VTable;

#define GMETA_STRUCT_HEADER \
	GOBJ_STRUCT_HEADER; \
	a_u32 _len; \
	a_u16 _tid; \
	a_u16 _flags; \
    VTable _vtable

struct GMeta {
	GMETA_STRUCT_HEADER;
};

always_inline a_bool g_is_str(a_hobj v) {
	return v->_meta->_tid == T_ISTR || v->_meta->_tid == T_HSTR;
}

always_inline GStr* g_as_str(a_hobj v) {
	assume(g_is_str(v));
	return g_cast(GStr, v);
}

always_inline a_bool g_is_tuple(a_hobj v) {
	return v->_meta->_tid == T_TUPLE;
}

always_inline GTuple* g_as_tuple(a_hobj v) {
	assume(g_is_tuple(v));
	return g_cast(GTuple, v);
}

always_inline a_bool g_is_list(a_hobj v) {
	return v->_meta->_tid == T_LIST;
}

always_inline GList* g_as_list(a_hobj v) {
	assume(g_is_list(v));
	return g_cast(GList, v);
}

always_inline a_bool g_is_table(a_hobj v) {
	return v->_meta->_tid == T_TABLE;
}

always_inline GTable* g_as_table(a_hobj v) {
	assume(g_is_table(v));
	return g_cast(GTable, v);
}

always_inline a_bool g_is_func(a_hobj v) {
	return v->_meta->_tid == T_FUNC;
}

always_inline GFun* g_as_func(a_hobj v) {
	assume(g_is_func(v));
	return g_cast(GFun, v);
}

always_inline a_bool g_is_mod(a_hobj v) {
	return v->_meta->_tid == T_MOD;
}

always_inline GMod* g_as_mod(a_hobj v) {
	assume(g_is_mod(v));
	return g_cast(GMod, v);
}

always_inline void v_check_alive(Global* g, Value const* v);

#define v_hi_mask(id) (~(id) << 15)

#define v_raw_tag(v) (~(v)->_h >> 15)

always_inline a_bool v_is_nil(Value const* v) {
	return ~v->_h != 0;
}

#define V_STRICT_NIL (cast(a_u64, v_hi_mask(T_NIL)) << 32 | LO_DFL_VALUE)
#define V_EMPTY (cast(a_u64, v_hi_mask(T_NIL)) << 32 | ~u32c(1))

always_inline a_bool v_is_strict_nil(Value const* v) {
	return v->_u == V_STRICT_NIL;
}

always_inline a_bool v_is_dead_key(Value const* v) {
	return v->_u == V_EMPTY;
}

always_inline a_bool v_is_false(Value const* v) {
	return v_raw_tag(v) == T_FALSE;
}

always_inline a_bool v_is_true(Value const* v) {
	return v_raw_tag(v) == T_TRUE;
}

always_inline a_bool v_is_int(Value const* v) {
	return v_raw_tag(v) == T_INT;
}

always_inline a_bool v_is_ptr(Value const* v) {
	return v_raw_tag(v) == T_PTR;
}

always_inline a_bool v_is_obj(Value const* v) {
	return v->_h - v_hi_mask(T_OTHER) < v_hi_mask(T_PTR) - v_hi_mask(T_OTHER);
}

always_inline a_bool v_is_str(Value const* v) {
	return v_raw_tag(v) == T_HSTR || v_raw_tag(v) == T_ISTR;
}

always_inline a_bool v_is_tuple(Value const* v) {
	return v_raw_tag(v) == T_TUPLE;
}

always_inline a_bool v_is_list(Value const* v) {
	return v_raw_tag(v) == T_LIST;
}

always_inline a_bool v_is_table(Value const* v) {
	return v_raw_tag(v) == T_TABLE;
}

always_inline a_bool v_is_func(Value const* v) {
	return v_raw_tag(v) == T_FUNC;
}

always_inline a_bool v_is_mod(Value const* v) {
	return v_raw_tag(v) == T_MOD;
}

always_inline a_bool v_is_cap(Value const* v) {
	return v_raw_tag(v) == T_CAP;
}

always_inline a_bool v_is_float(Value const* v) {
	return v->_h <= u32c(0xfff80000);
}

always_inline a_bool v_is_nan(Value const* v) {
	return v_is_float(v) && isnan(v->_f);
}

always_inline a_bool v_is_num(Value const* v) {
	return v->_h >= v_hi_mask(T_INT);
}

always_inline a_bool v_is_other(Value const* v) {
	return v_raw_tag(v) == T_OTHER;
}

always_inline a_bool v_to_bool(Value const* v) {
	return v->_h <= v_hi_mask(T_TRUE);
}

always_inline a_int v_as_int(Value const* v) {
	assume(v_is_int(v), "not int value.");
	return v->_i;
}

always_inline a_float v_as_float(Value const* v) {
	assume(v_is_float(v), "not float value.");
	return v->_f;
}

always_inline a_float v_as_num(Value const* v) {
	return v_is_int(v) ? v_as_int(v) : v_as_float(v);
}

always_inline void* v_as_hnd(Value const* v) {
	return ptr_of(void, v->_u & u64c(0x00007fffffffffff));
}

always_inline void* v_as_ptr(Value const* v) {
	assume(v_is_ptr(v), "not pointer.");
	return v_as_hnd(v);
}

always_inline GObj* v_as_obj(Global* g, Value const* v) {
	assume(v_is_obj(v));
	v_check_alive(g, v);
	return cast(GObj*, v_as_hnd(v));
}

always_inline GStr* v_as_str(Global* g, Value const* v) {
	assume(v_is_str(v));
	return g_as_str(v_as_obj(g, v));
}

always_inline GTuple* v_as_tuple(Global* g, Value const* v) {
	assume(v_is_tuple(v));
	return g_as_tuple(v_as_obj(g, v));
}

always_inline GList* v_as_list(Global* g, Value const* v) {
	assume(v_is_list(v));
	return g_as_list(v_as_obj(g, v));
}

always_inline GTable* v_as_table(Global* g, Value const* v) {
	assume(v_is_table(v));
	return g_as_table(v_as_obj(g, v));
}

always_inline GFun* v_as_func(Global* g, Value const* v) {
	assume(v_is_func(v));
	return g_as_func(v_as_obj(g, v));
}

always_inline GMod* v_as_mod(Global* g, Value const* v) {
	assume(v_is_mod(v));
	return g_as_mod(v_as_obj(g, v));
}

always_inline Capture* v_as_cap(Value const* v) {
	assume(v_is_cap(v));
	return cast(Capture*, v_as_hnd(v));
}

#define v_of_nil() (new(Value) { ._h = v_hi_mask(T_NIL), ._l = LO_DFL_VALUE })
#define v_of_empty() (new(Value) { ._u = V_EMPTY })
#define v_of_bool(v) (new(Value) { ._h = v_hi_mask((v) ? T_TRUE : T_FALSE), ._l = LO_DFL_VALUE })
#define v_of_int(v) (new(Value) { ._h = v_hi_mask(T_INT), ._i = (v) })
#define v_of_float(v) (new(Value) { ._f = (v) })
#define v_of_hnd(id,v) (new(Value) { ._u = cast(a_u64, v_hi_mask(id)) << 32 | addr_of(v) })
#define v_of_ptr(v) v_of_hnd(T_PTR, v)

always_inline Value v_of_obj_(a_hobj v) {
	a_u32 id = cast(a_u32, v->_meta->_tid);
	return v_of_hnd(min(id, T_OTHER), v);
}

#define v_of_obj(v) v_of_obj_(gobj_cast(v))

always_inline Value v_of_cap(Capture* v) {
	return v_of_hnd(T_CAP, v);
}

always_inline void v_setx(Value* d, Value v) {
	*d = v;
}

always_inline void v_set_nil(Value* d) {
	v_setx(d, v_of_nil());
}

always_inline void v_set_bool(Value* d, a_bool v) {
	v_setx(d, v_of_bool(v));
}

always_inline void v_cpy(Global* g, Value* d, Value const* s) {
	*d = *s;
	v_check_alive(g, d);
}

always_inline void v_cpy_multi(Global* g, Value* d, Value const* s, a_usize n) {
	for (a_usize i = 0; i < n; ++i) {
		v_cpy(g, &d[i], &s[i]);
	}
}

always_inline void v_set(Global* g, Value* d, Value v) {
	v_cpy(g, d, &v);
}

#define v_nil_hash() u32c(0)
#define v_bool_hash(v) ((v) ? u32c(32) : u32c(31))

always_inline a_hash v_int_hash(a_int v) {
	return cast(a_u32, v);
}

always_inline a_hash v_float_hash(a_float v) {
	a_u64 u = *cast(a_u64*, &v);
	return (u >> 32) ^ u ^ u32c(0x9f1b407d);
}

always_inline a_hash v_hnd_hash(void* v) {
	a_usize u = cast(a_usize, v);
#if ALO_M64
	return (u >> 32) ^ u ^ u32c(0x38a8eb01);
#else
	return u ^ 0x38a8eb01;
#endif
}

/* Identity equality. */
#define v_id_eq(v1,v2) ((v1)->_u == (v2)->_u)

intern char const* const ai_obj_tag_name[];

#endif /* aobj_h_ */
