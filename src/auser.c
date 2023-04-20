/**
 *@file auser.c
 */

#define auser_c_
#define ALO_LIB

#include "atable.h"
#include "atype.h"
#include "agc.h"
#include "avm.h"
#include "aerr.h"

#include "auser.h"

GAUser* ai_auser_new(a_henv env, GType* type) {
	assume(type->_opt_vtbl->_mask == v_masked_tag(T_AUSER), "not type for auser.");
	GTable* self = ai_table_new(env);
	self->_vptr = type->_opt_vtbl; /* Set type. */
	return self;
}

Value ai_auser_get(a_henv env, GAUser* self, Value key) {
	a_hash hash;

	Value const* ref = ai_table_ref(env, self, key, &hash);
	if (ref != null) {
		return *ref;
	}

	Value vf = ai_obj_glookftm(env, self, TM_GET);
	if (v_is_nil(vf)) {
		return v_of_nil();
	}

	return ai_vm_meta_get(env, vf, v_of_obj(self), key);
}

void ai_auser_set(a_henv env, GAUser* self, Value key, Value value) {
	a_hash hash;

	Value* ref = ai_table_ref(env, self, key, &hash);
	if (ref != null) {
		v_set(env, ref, value);
		ai_gc_barrier_backward_val(env, self, value);
		return;
	}

	Value vf = ai_obj_glookftm(env, self, TM_SET);
	if (v_is_nil(vf)) {
		ai_table_hint(env, self, 1);
		return ai_table_put(env, self, key, hash, value);
	}

	return ai_vm_meta_set(env, vf, v_of_obj(self), key, value);
}

static void tuser_mark(Global* g, GTable* self) {
	GType* type = ptr_disp(GType, g, self->_vptr->_iname);
	ai_gc_trace_mark(g, type);
	ai_table_mark(g, self);
}

VTable const ai_auser_vtable = {
	._mask = V_MASKED_TAG(T_AUSER),
	._base_size = sizeof(GTable),
	._elem_size = 0,
	._flags = VTABLE_FLAG_NONE,
	._vfps = (a_vslot[]) {
		vfp_def(drop, ai_table_drop),
		vfp_def(mark, tuser_mark),
		vfp_def(close, null)
	}
};