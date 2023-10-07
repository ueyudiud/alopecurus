/**
 *@file auser.c
 */

#define auser_c_
#define ALO_LIB

#include "adict.h"
#include "ameta.h"
#include "amem.h"
#include "agc.h"
#include "avm.h"
#include "aerr.h"

#include "auser.h"

typedef struct {
	GOBJ_STRUCT_HEADER;
    Dict _keys;
} GRec;

static VTable const rec_vtable;

GUser* ai_rec_new(a_henv env, GUserType* type) {
	assume(type->_vtbl->_stencil == V_STENCIL(T_USER), "not type for auser.");
	GRec* self = ai_mem_alloc(env, sizeof(GRec));

    self->_vptr = type->_vtbl; /* Set virtual table. */
    ai_dict_init(env, &self->_keys);

    ai_gc_register_object(env, self);

	return g_cast(GUser, self);
}

static a_uint rec_len(unused a_henv env, GRec* self) {
    return self->_keys._len;
}

static Value rec_get(a_henv env, GRec* self, Value vk) {
	a_msg msg;
	Value v;

    if (v_is_str(vk)) {
        msg = ai_dict_uget(env, &self->_keys, v_as_str(vk), &v);
        if (msg == ALO_SOK)
            return v;
    }

	Value vf = ai_obj_glookftm(env, self, TM___get__);
	if (v_is_nil(vf)) {
		if (msg == ALO_EXIMPL)
			ai_err_bad_tm(env, TM___get__);
		return v_of_nil();
	}
	else if (v_is_func(vf)) {
		return ai_vm_call_meta(env, vm_push_args(env, vf, v_of_obj(self), vk));
	}
	else {
        ai_err_bad_key(env, g_nameof(env, self), v_nameof(env, vk));
	}
}

static void rec_set(a_henv env, GRec* self, Value vk, Value vv) {
	a_msg msg;
    a_usize ctx;

	Value vf = ai_obj_glookftm(env, self, TM___set__);
	if (v_is_func(vf)) {
		ai_vm_call(env, vm_push_args(env, vf, v_of_obj(self), vk, vv), 0);
        return;
	}
	else if (!v_is_nil(vf)) {
		return ai_vm_set(env, vf, vk, vv);
	}

    if (v_is_str(vk)) {
        msg = ai_dict_uset(env, &self->_keys, v_as_str(vk), vv, &ctx);

        if (msg == ALO_SOK)
            return;
    }

	vf = ai_obj_glookftm(env, self, TM___put__);
	if (!v_is_nil(vf)) {
        if (v_is_func(vf)) {
            ai_vm_call(env, vm_push_args(env, vf, v_of_obj(self), vk, vv), 0);
        }
        else {
            ai_vm_set(env, vf, vk, vv);
        }
	}

    if (v_is_str(vk)) {
        ctx = 0;
        msg = ai_dict_uput(env, &self->_keys, v_as_str(vk), vv, &ctx);

        if (msg == ALO_SOK)
            return;
    }

    ai_err_bad_tm(env, TM___get__);
}

static a_msg rec_uget(a_henv env, GRec* self, Value vk, Value* pv) {
    if (!v_is_str(vk)) return ALO_EINVAL;
    return ai_dict_uget(env, &self->_keys, v_as_str(vk), pv);
}

static a_msg rec_uset(a_henv env, GRec* self, Value vk, Value vv) {
    if (!v_is_str(vk)) return ALO_EINVAL;
    a_usize ctx = 0;
    return ai_dict_uset(env, &self->_keys, v_as_str(vk), vv, &ctx);
}

static void rec_mark(Global* g, GRec* self) {
	GType* type = g_typeof(g->_active, self);
	ai_gc_trace_mark(g, type);
	ai_dict_mark(g, &self->_keys);
	ai_gc_trace_work(g, sizeof(GMeta) - sizeof(GTable));
}

static void rec_drop(Global* g, GRec* self) {
    ai_dict_deinit(g, &self->_keys);
    ai_mem_dealloc(g, self, sizeof(GRec));
}

static VTable const rec_vtable = {
	._stencil = V_STENCIL(T_USER),
	._flags = VTABLE_FLAG_NONE,
	._slots = {
        [vfp_slot(drop)] = &rec_drop,
        [vfp_slot(mark)] = rec_mark,
        [vfp_slot(len)] = rec_len,
        [vfp_slot(get)] = rec_get,
        [vfp_slot(uget)] = rec_uget,
        [vfp_slot(set)] = rec_set,
        [vfp_slot(uset)] = rec_uset
	}
};