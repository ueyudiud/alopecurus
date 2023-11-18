/**
 *@file atype.c
 */

#define atype_c_
#define ALO_LIB

#include "atable.h"
#include "aenv.h"
#include "amem.h"
#include "agc.h"

#include "atype.h"

static VTable const type_vtable;

GType* ai_type_new(a_henv env, GStr* name) {
    a_usize size = type_size(0);

    GType* self = ai_mem_alloc(env, size);
    memclr(self, size);

    self->_vptr = &type_vtable;
    self->_name = name;

    ai_gc_register_object(env, self);
    return self;
}

void ai_type_boost(a_henv env) {
    Global* gbl = G(env);

    static a_u8 const l_name_tags[] = {
        [ALO_TNIL] = STR_nil,
        [ALO_TBOOL] = STR_bool,
        [ALO_TINT] = STR_int,
        [ALO_TFLOAT] = STR_float,
        [ALO_TPTR] = STR_ptr,
        [ALO_TSTR] = STR_str,
        [ALO_TTUPLE] = STR_tuple,
        [ALO_TLIST] = STR_list,
        [ALO_TTABLE] = STR_table,
        [ALO_TFUNC] = STR_func,
        [ALO_TTYPE] = STR_type,
        [ALO_TROUTE] = STR_route
    };

    static_assert(sizeof(l_name_tags) == TYPE__COUNT);

    for (a_u32 i = 0; i < TYPE__COUNT; ++i) {
        init(gbl->_types[i]) {
            ._vptr = &type_vtable,
            ._name = g_str(env, l_name_tags[i])
        };
    }
}

static void type_clean(Global* gbl, GType* self) {
    ai_table_clean(gbl, type2mt(self));
}

void ai_type_clean(Global* gbl) {
    for (a_u32 i = 0; i < TYPE__COUNT; ++i) {
        type_clean(gbl, gbl->_types[i]);
    }
}

static void type_drop(Global* gbl, GType* self) {
    type_clean(gbl, self);
    ai_mem_dealloc(gbl, self, type_size(0));
}

static void type_mark(Global* gbl, GType* self) {
    ai_table_mark(gbl, type2mt(self));
    ai_gc_trace_mark(gbl, self->_name);
    ai_gc_trace_work(gbl, type_size(0));
}

static VTable const type_vtable = {
	._stencil = V_STENCIL(T_TABLE),
    ._tag = ALO_TTYPE,
    ._type_ref = g_type_ref(ALO_TTYPE),
	._slots = {
        [vfp_slot(drop)] = type_drop,
        [vfp_slot(mark)] = type_mark
	}
};
