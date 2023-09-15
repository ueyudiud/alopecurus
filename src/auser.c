/**
 *@file auser.c
 */

#define auser_c_
#define ALO_LIB

#include "atable.h"
#include "amod.h"
#include "agc.h"
#include "avm.h"
#include "aerr.h"

#include "auser.h"

GAUser* ai_auser_new(a_henv env, GMod* type) {
	assume(type->_opt_vtbl->_tag == v_masked_tag(T_AUSER), "not type for auser.");
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

	Value vf = ai_obj_glookftm(env, self, TM___get__);
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

	Value vf = ai_obj_glookftm(env, self, TM___set__);
	if (v_is_nil(vf)) {
		ai_table_hint(env, self, 1);
		return ai_table_put(env, self, key, hash, value);
	}

	return ai_vm_meta_set(env, vf, v_of_obj(self), key, value);
}

static void tuser_mark(Global* g, GTable* self) {
	GMod* type = ptr_disp(GMod, g, self->_vptr->_iname);
	ai_gc_trace_mark(g, type);
	ai_table_mark(g, self);
	ai_gc_trace_work(g, sizeof(GMod) - sizeof(GTable));
}

VImpl const ai_auser_vtable = {
	._tag = V_MASKED_TAG(T_AUSER),
	._sname = "obj",
	._flags = VTABLE_FLAG_NONE,
	._vfps = {
		vfp_def(drop, ai_table_drop),
		vfp_def(mark, tuser_mark)
	}
};