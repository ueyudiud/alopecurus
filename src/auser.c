/**
 *@file auser.c
 */

#define auser_c_
#define ALO_LIB

#include "auser.h"
#include "amem.h"
#include "agc.h"
#include "atm.h"

static GUser* user_alloc(a_henv env, GUType* type) {
    void* base = ai_mem_alloc(env, user_size(type));
    GUser* self = base + sizeof(Value) * type->num_slot;
    return self;
}

GUser* ai_user_new(a_henv env, GUType* type) {
    GUser* self = user_alloc(env, type);
    self->impl = &type->body;

    memclr(self->block, type->block_size);
    v_set_nil_ranged(self->slot - type->num_slot, self->slot);

    ai_gc_register_normal(env, self);

    return self;
}

GUser* ai_user_clone(a_henv env, GUser* proto) {
    GUType* type = from_member(GUType, body, proto->impl);
    GUser* self = user_alloc(env, type);
    self->impl = proto->impl;

    memcpy(self->block, proto->block, type->block_size);
    v_cpy_all(env, self->slot - type->num_slot, proto->slot - type->num_slot, type->num_slot);

    ai_gc_register_normal(env, self);

    return self;
}

void ai_user_mark(Global* gbl, a_gptr obj) {
    GUser* self = g_as(GUser, obj);
    GUType* type = g_type(gbl, self)->as_utype;
    for (a_u32 i = 0; i < type->num_slot; ++i) {
        ai_gc_trace_mark_val(gbl, *user_slot(self, i));
    }
    ai_gc_trace_work(gbl, user_size(type));
    ai_gc_trace_mark(gbl, g_type(gbl, obj));
}

void ai_user_drop(Global* gbl, a_gptr obj) {
    GUser* self = g_as(GUser, obj);
    GUType* type = g_type(gbl, self)->as_utype;
    ai_mem_dealloc(gbl, self->slot - type->num_slot, user_size(type));
}
