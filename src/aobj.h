/*
 * aobj.h
 */

#ifndef aobj_h_
#define aobj_h_

#include "adef.h"

typedef struct GObj GObj;
typedef struct GStr GStr;
typedef struct GTuple GTuple;
typedef struct GList GList;
typedef struct GTable GTable;
typedef struct GCap GCap;
typedef struct GFun GFun;
typedef struct alo_Env GRoute;
typedef struct GMeta GMeta;

/* Object handle. */
typedef GObj* a_hobj;

/* GC support. */
typedef a_usize a_trmark;
typedef a_hobj a_gcnext;
typedef a_gcnext a_gclist;

typedef struct Global Global;
typedef struct alo_Alloc Alloc;

#define T_NIL u32c(0)
#define T_FALSE u32c(1)
#define T_TRUE u32c(2)
#define T_INT u32c(3)
#define T_PTR u32c(4)
#define T_ISTR u32c(5)
#define T_HSTR u32c(6)
#define T_TUPLE u32c(7)
#define T_LIST u32c(8)
#define T_TABLE u32c(9)
#define T_FUNC u32c(10)
#define T_ROUTE u32c(11)
#define T_OTHER u32c(12)

#define T__MAX T_OTHER

#define T_FLOAT u32c(13)

#define LO_DFL_VALUE (~u32c(0))

#define upcast(e) cast(a_hobj, e)
#define downcast(t,e) cast(typeof(t)*, e)

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

#define GOBJ_STRUCT_HEADER a_gcnext _gnext; a_trmark _tnext; GMeta* _meta

struct GObj {
	GOBJ_STRUCT_HEADER;
};

typedef void (*a_fp_splash)(Global* g, a_hobj self);
typedef void (*a_fp_destruct)(Global* g, a_hobj self);

#define GTYPE_FLAG_IDENTITY_EQUAL u16c(0x0001)
#define GTYPE_FLAG_FAST_LENGTH u16c(0x0002)

#define GMETA_STRUCT_HEADER \
	GOBJ_STRUCT_HEADER; \
	a_u16 _tid; \
	a_u16 _flags; \
	a_u32 _len; \
	a_fp_splash _splash; \
	a_fp_destruct _destruct

struct GMeta {
	GMETA_STRUCT_HEADER;
};

inline a_bool g_is_str(a_hobj v) {
	return v->_meta->_tid == T_ISTR || v->_meta->_tid == T_HSTR;
}

inline GStr* g_as_str(a_hobj v) {
	assume(g_is_str(v));
	return downcast(GStr, v);
}

inline a_bool g_is_tuple(a_hobj v) {
	return v->_meta->_tid == T_TUPLE;
}

inline GTuple* g_as_tuple(a_hobj v) {
	assume(g_is_tuple(v));
	return downcast(GTuple, v);
}

inline a_bool g_is_list(a_hobj v) {
	return v->_meta->_tid == T_LIST;
}

inline GList* g_as_list(a_hobj v) {
	assume(g_is_list(v));
	return downcast(GList, v);
}

inline a_bool g_is_table(a_hobj v) {
	return v->_meta->_tid == T_TABLE;
}

inline GTable* g_as_table(a_hobj v) {
	assume(g_is_table(v));
	return downcast(GTable, v);
}

inline a_bool g_is_func(a_hobj v) {
	return v->_meta->_tid == T_FUNC;
}

inline GFun* g_as_func(a_hobj v) {
	assume(g_is_func(v));
	return downcast(GFun, v);
}

inline void v_check_alive(Global* g, Value const* v);

#define v_hi_mask(id) (~(id) << 15)

#define v_raw_tag(v) (~(v)->_h >> 15)

inline a_bool v_is_nil(Value const* v) {
	return ~v->_h != 0;
}

#define V_STRICT_NIL (cast(a_u64, v_hi_mask(T_NIL)) << 32 | LO_DFL_VALUE)
#define V_DEAD_KEY (cast(a_u64, v_hi_mask(T_NIL)) << 32 | ~u32c(1))

inline a_bool v_is_strict_nil(Value const* v) {
	return v->_u == V_STRICT_NIL;
}

inline a_bool v_is_dead_key(Value const* v) {
	return v->_u == V_DEAD_KEY;
}

inline a_bool v_is_false(Value const* v) {
	return v_raw_tag(v) == T_FALSE;
}

inline a_bool v_is_true(Value const* v) {
	return v_raw_tag(v) == T_TRUE;
}

inline a_bool v_is_int(Value const* v) {
	return v_raw_tag(v) == T_INT;
}

inline a_bool v_is_ref(Value const* v) {
	return v->_h - v_hi_mask(T_OTHER) < v_hi_mask(T_PTR) - v_hi_mask(T_OTHER);
}

inline a_bool v_is_str(Value const* v) {
	return v_raw_tag(v) >> 1 == T_ISTR >> 1;
}

inline a_bool v_is_tuple(Value const* v) {
	return v_raw_tag(v) == T_TUPLE;
}

inline a_bool v_is_list(Value const* v) {
	return v_raw_tag(v) == T_LIST;
}

inline a_bool v_is_table(Value const* v) {
	return v_raw_tag(v) == T_TABLE;
}

inline a_bool v_is_func(Value const* v) {
	return v_raw_tag(v) == T_FUNC;
}

inline a_bool v_is_route(Value const* v) {
	return v_raw_tag(v) == T_ROUTE;
}

inline a_bool v_is_float(Value const* v) {
	return v->_h <= u32c(0xfff80000);
}

inline a_bool v_to_bool(Value const* v) {
	return v->_h <= v_hi_mask(T_TRUE);
}

inline a_int v_as_int(Value const* v) {
	assume(v_is_int(v), "not int value.");
	return v->_i;
}

inline a_float v_as_float(Value const* v) {
	assume(v_is_float(v), "not float value.");
	return v->_f;
}

inline void* v_as_hnd(Value const* v) {
	return ptr_of(void, v->_u & u64c(0x00007fffffffffff));
}

inline GObj* v_as_obj(Global* g, Value const* v) {
	assume(v_is_ref(v));
	v_check_alive(g, v);
	return cast(GObj*, v_as_hnd(v));
}

inline GStr* v_as_str(Global* g, Value const* v) {
	assume(v_is_str(v));
	return g_as_str(v_as_obj(g, v));
}

inline GTuple* v_as_tuple(Global* g, Value const* v) {
	assume(v_is_tuple(v));
	return g_as_tuple(v_as_obj(g, v));
}

inline GList* v_as_list(Global* g, Value const* v) {
	assume(v_is_list(v));
	return g_as_list(v_as_obj(g, v));
}

inline GTable* v_as_table(Global* g, Value const* v) {
	assume(v_is_table(v));
	return g_as_table(v_as_obj(g, v));
}

inline GFun* v_as_func(Global* g, Value const* v) {
	assume(v_is_func(v));
	return g_as_func(v_as_obj(g, v));
}

#define v_of_nil() (new(Value) { _h: v_hi_mask(T_NIL), _l: LO_DFL_VALUE })
#define v_of_dead_key() (new(Value) { _u: V_DEAD_KEY })
#define v_of_bool(v) (new(Value) { _h: v_hi_mask((v) ? T_TRUE : T_FALSE), _l: LO_DFL_VALUE })
#define v_of_int(v) (new(Value) { _h: v_hi_mask(T_INT), _i: (v) })
#define v_of_float(v) (new(Value) { _f: (v) })
#define v_of_hnd(id,v) (new(Value) { _u: cast(a_u64, v_hi_mask(id)) << 32 | addr_of(v) })
#define v_of_ptr(v) v_of_hnd(T_PTR, v)

inline Value v_of_ref(void* v) {
	GObj* o = upcast(v);
	a_u32 id = cast(a_u32, o->_meta->_tid);
	return v_of_hnd(min(id, T__MAX), o);
}

inline void v_cpy(Global* g, Value* d, Value const* s) {
	*d = *s;
	v_check_alive(g, d);
}

inline void v_setx(Value* d, Value v) {
	*d = v;
}

inline void v_set(Global* g, Value* d, Value v) {
	v_cpy(g, d, &v);
}

inline a_hash v_int_hash(a_int v) {
	return cast(a_u32, v);
}

/* Identity equality. */
#define v_id_eq(v1,v2) ((v1)->_u == (v2)->_u)

#endif /* aobj_h_ */
