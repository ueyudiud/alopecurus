/**
 *@file alist.c
 */

#define alist_c_
#define ALO_LIB

#include "aop.h"
#include "amem.h"
#include "agc.h"
#include "aerr.h"

#include "alist.h"

static Impl const list_impl;

#if ALO_M64
# define LIST_MAX_CAP cast(a_u32, INT32_MAX)
#else
# define LIST_MAX_CAP ((UINT32_MAX - sizeof(GList)) / sizeof(Value))
#endif

GList* ai_list_new(a_henv env) {
    GList* self = ai_mem_alloc(env, list_size());

    self->impl = &list_impl;
    self->ptr = null;
    self->cap = 0;
    self->len = 0;

    ai_gc_register_normal(env, self);

    return self;
}

static a_bool list_grow(a_henv env, GList* self, a_ulen add) {
    a_ulen need = try_add(self->len, add);
    if (unlikely(need > LIST_MAX_CAP)) return true;

    assume(need >= self->cap, "not need to grow.");

    a_ulen old_cap = self->cap;
    a_ulen new_cap = max(old_cap * 2, need + 4);
    if (unlikely(new_cap > LIST_MAX_CAP)) {
        new_cap = LIST_MAX_CAP;
    }

    self->ptr = ai_mem_vgrow(env, self->ptr, old_cap, new_cap);
    self->cap = new_cap;

    return false;
}

void ai_list_grow(a_henv env, GList* self, a_usize add) {
    catch (list_grow(env, self, add)) {
        ai_err_raisef(env, ALO_EINVAL, "too many elements.");
    }
}

void ai_list_hint(a_henv env, GList* self, a_usize add) {
    if (add > self->cap - self->len) {
        ai_list_grow(env, self, add);
    }
}

void ai_list_push(a_henv env, GList* self, Value v) {
    ai_list_hint(env, self, 1);

    v_set(env, &self->ptr[self->len], v);
    self->len += 1;

	ai_gc_barrier_backward_val(env, self, v);
}

void ai_list_push_all(a_henv env, GList* self, Value const* src, a_usize len) {
    ai_list_hint(env, self, len);

    v_cpy_all(env, &self->ptr[self->len], src, len);
    self->len += len;

    /* We assume the elements in source has white value. */
	if (g_has_black_color(self)) {
		join_trace(&G(env)->tr_regray, self);
	}
}

Value ai_list_get(a_henv env, GList* self, Value vk) {
	if (unlikely(!v_is_int(vk))) {
        ai_err_bad_key(env, "list", v_name(env, vk));
	}
	return ai_list_geti(env, self, v_as_int(vk));
}

Value ai_list_geti(unused a_henv env, GList* self, a_int k) {
    a_u32 i = obj_idx(k, self->len, v_of_nil());
	return self->ptr[i];
}

void ai_list_set(a_henv env, GList* self, Value vk, Value vv) {
	if (unlikely(!v_is_int(vk))) {
        ai_err_bad_key(env, "list", v_name(env, vv));
	}
	ai_list_seti(env, self, v_as_int(vk), vv);
}

void ai_list_seti(a_henv env, GList* self, a_int key, Value value) {
    a_u32 i = obj_idx(key, self->len,
        ai_err_raisef(env, ALO_EINVAL, "list index out of bound.")
    );
	v_set(env, &self->ptr[i], value);
	ai_gc_barrier_forward_val(env, self, value);
}

a_msg ai_list_ugeti(a_henv env, GList* self, a_int k, Value* pv) {
    a_uint i = obj_idx(k, self->len, ALO_EEMPTY);
    v_set(env, pv, self->ptr[i]);
    return ALO_SOK;
}

a_msg ai_list_uget(a_henv env, GList* self, Value vk, Value* pv) {
    if (!v_is_int(vk)) return ALO_EINVAL;
    return ai_list_ugeti(env, self, v_as_int(vk), pv);
}

a_msg ai_list_useti(a_henv env, GList* self, a_int k, Value v) {
    a_uint i = obj_idx(k, self->len, ALO_EEMPTY);

    v_set(env, &self->ptr[i], v);

    ai_gc_barrier_backward_val(env, self, v);
    return ALO_SOK;
}

a_msg ai_list_uset(a_henv env, GList* self, Value vk, Value v) {
    if (!v_is_int(vk)) return ALO_EINVAL;
    return ai_list_useti(env, self, v_as_int(vk), v);
}

static void list_mark(Global* gbl, GList* self) {
    for (a_u32 i = 0; i < self->len; ++i) {
        ai_gc_trace_mark_val(gbl, self->ptr[i]);
    }
    ai_gc_trace_work(gbl, list_size() + sizeof(Value) * self->cap);
}

static void list_drop(Global* gbl, GList* self) {
    if (self->ptr != null) {
        ai_mem_vdel(gbl, self->ptr, self->cap);
    }
    ai_mem_dealloc(gbl, self, list_size());
}

static Impl const list_impl = {
    .tag = ALO_TLIST,
    .name = "list",
    .drop = list_drop,
    .mark = list_mark
};