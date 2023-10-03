/**
 *@file auser.c
 */

#define auser_c_
#define ALO_LIB

#include "atable.h"
#include "ameta.h"
#include "agc.h"
#include "avm.h"
#include "aerr.h"

#include "auser.h"

typedef struct {
	GOBJ_STRUCT_HEADER;
	Value _raw;
	GMeta* _meta;
} GAUser;

GUser* ai_auser_new(a_henv env, GMeta* type) {
	assume(type->_opt_vtbl->_stencil == v_stencil(T_USER), "not type for auser.");
	GUser* self = g_cast(GUser, ai_table_new(env));
	self->_vptr = type->_opt_vtbl; /* Set type. */
	return self;
}

static Value auser_get(a_henv env, GAUser* self, Value vk) {
	a_msg msg;
	Value v;

	msg = ai_vm_uget(env, self->_raw, vk, &v);
	if (msg == ALO_SOK) 
		return v;
	else if (msg == ALO_EINVAL)
		ai_err_bad_get(env, str2ntstr(self->_meta->_uid), v_nameof(env, v));
	assume(msg == ALO_EEMPTY || msg == ALO_EBADOP, "unexpected message");

	Value vf = ai_obj_glookftm(env, self->_meta, TM___get__);
	if (v_is_nil(vf)) {
		if (msg == ALO_EBADOP)
			ai_err_bad_tm(env, TM___get__);
		return v_of_nil();
	}
	else if (v_is_func(vf)) {
		return ai_vm_call_meta(env, vm_push_args(env, vf, v_of_obj(self), vk));
	}
	else {
		return ai_vm_get(env, vf, vk);
	}
}

static void auser_set(a_henv env, GAUser* self, Value vk, Value vv) {
	a_msg msg;
	Value v;
    a_isize ctx;

	Value vf = ai_obj_glookftm(env, self->_meta, TM___set__);
	if (v_is_func(vf)) {
		ai_vm_call(env, vm_push_args(env, vf, v_of_obj(self), vk, vv), 0);
        return;
	}
	else if (!v_is_nil(vf)) {
		return ai_vm_set(env, vf, vk, vv);
	}

	msg = ai_vm_uset(env, self->_raw, vk, vv, &ctx);

	if (msg == ALO_SOK)
		return;
	else if (msg == ALO_EINVAL)
		ai_err_bad_get(env, str2ntstr(self->_meta->_uid), v_nameof(env, v));
	assume(msg == ALO_EEMPTY || msg == ALO_EBADOP, "unexpected message");

	vf = ai_obj_glookftm(env, self->_meta, TM___put__);
	if (v_is_nil(vf)) {
		if (msg == ALO_EBADOP)
			ai_err_bad_tm(env, TM___get__);
	}
	else if (v_is_func(vf)) {
		ai_vm_call(env, vm_push_args(env, vf, v_of_obj(self), vk, vv), 0);
	}
	else {
        ai_vm_set(env, vf, vk, vv);
	}
}

static void tuser_mark(Global* g, GTable* self) {
	GMeta* type = g_typeof(g->_active, self);
	ai_gc_trace_mark(g, type);
	ai_table_mark(g, self);
	ai_gc_trace_work(g, sizeof(GMeta) - sizeof(GTable));
}

VTable const ai_auser_vtable = {
	._stencil = V_STENCIL(T_USER),
	._uid = "obj",
	._flags = VTABLE_FLAG_NONE,
	._vfps = {
		vfp_def(drop, ai_table_drop),
		vfp_def(mark, tuser_mark),
		vfp_def(get, auser_get),
		vfp_def(set, auser_set)
	}
};