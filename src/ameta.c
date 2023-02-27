/**
 *@file ameta.c
 */

#define ameta_c_
#define ALO_LIB

#include "agbl.h"
#include "amem.h"
#include "agc.h"

#include "ameta.h"

GUserMeta* ai_umeta_new(a_henv env, GMeta* proto) {
	GUserMeta* self = ai_mem_alloc(env, sizeof(GUserMeta));

	self->_vtable = proto->_vtable;
	self->_proto = proto;
	self->_cfield = 0;
	self->_fields = null;
	self->_field_hfree = 0;
	self->_field_indices = ai_dict_new();

	return self;
}

Value const* ai_umeta_get(unused a_henv env, GUserMeta* self, GStr* key) {
	a_u32 const* pid = ai_dict_find(&self->_field_indices, key);
	return pid != null ? &self->_fields[*pid] : null;
}

static void umeta_check_capacity(a_henv env, GUserMeta* self) {
	if (self->_field_indices._len == self->_cfield) {
		a_usize old_cap = self->_cfield;
		a_usize new_cap = max(old_cap * 2, 4);
		self->_fields = ai_mem_vgrow(env, self->_fields, old_cap, new_cap);
		self->_cfield = new_cap;
	}
}

void ai_umeta_set(a_henv env, GUserMeta* self, GStr* key, Value value) {
	umeta_check_capacity(env, self);
	a_u32 index = ai_dict_push(env, &self->_field_indices, key, &self->_field_hfree);
	v_set(env, &self->_fields[index], value);
}