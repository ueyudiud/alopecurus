/**
 *@file atable.c
 */

#define atable_c_
#define ALO_LIB

#include "aop.h"
#include "astr.h"
#include "amem.h"
#include "agc.h"
#include "aerr.h"
#include "avm.h"

#include "atable.h"

static VTable const table_vtable;

#if ALO_M64
# define TABLE_MAX_CAP (cast(a_usize, INT32_MAX) + 1)
#elif ALO_M32
# define TABLE_MAX_CAP ceil_power_of_two(INT32_MAX / sizeof(TNode))
#endif

enum {
    CTRL_INDEX = i32c(-1)
};

static TNode* bucket_alloc(a_henv env, a_u32 cap) {
    TNode* p = ai_mem_vnew(env, TNode, cap + 1);
    memset(p, -1, sizeof(TNode) * (cap + 1));
    return p + 1;
}

static void bucket_dealloc(Global* gbl, TNode* bucket, a_u32 cap) {
    ai_mem_vdel(gbl, bucket - 1, cap + 1);
}

GTable* ai_table_new(a_henv env) {
    GTable* self = ai_mem_alloc(env, table_size());

	self->_vptr = &table_vtable;
    self->_ptr = null;
    self->_hmask = 0;
    self->_len = 0;

	ai_gc_register_object(env, self);
    return self;
}

static TNode* table_node(GTable* self, a_i32 index) {
    assume(index == CTRL_INDEX || (index >= 0 && index <= cast(a_i32, self->_hmask)), "bad table node index");
    return &self->_ptr[index];
}

static TNode* table_ctrl_node(GTable* self) {
    return table_node(self, CTRL_INDEX);
}

static a_i32 table_hash_to_first_index(GTable* self, a_hash hash) {
    return cast(a_i32, hash & self->_hmask);
}

static a_bool table_is_head_of_hash_chain(GTable* self, a_i32 index, TNode* node) {
    return index == table_hash_to_first_index(self, node->_hash);
}

static void table_move_node(a_henv env, GTable* self, a_i32 index_src, a_i32 index_dst) {
    TNode* node_src = table_node(self, index_src);
    TNode* node_dst = table_node(self, index_dst);

    /* Copy key-value. */
    v_cpy(env, &node_dst->_key, &node_src->_key);
    v_cpy(env, &node_dst->_value, &node_src->_value);
    node_dst->_hash = node_src->_hash;
    node_dst->_hnext = node_src->_hnext;

    /* Copy linked information. */
    a_i32 index_prev = node_src->_lprev;
    a_i32 index_next = node_src->_lnext;
    TNode* node_prev = table_node(self, index_prev);
    TNode* node_next = table_node(self, index_next);

    node_dst->_lprev = index_prev;
    node_dst->_lnext = index_next;
    node_prev->_lnext = index_dst;
    node_next->_lprev = index_dst;
}

static a_i32 table_reserve_free(GTable* self, a_hash hash) {
    a_u32 hmask = self->_hmask;
    a_u32 probe = hash;

    hash += 1;

    loop {
        a_i32 index_free = cast(a_i32, hash & hmask);
        TNode* node_free = table_node(self, index_free);

        if (v_is_nil(node_free->_key))
            return index_free;

        probe >>= 5;
        hash = hash * 5 + probe + 1;
    }
}

static void table_redirect_from_hash_chain(GTable* self, a_i32 index_seek, a_hash hash, a_i32 index_new) {
    a_i32 index = table_hash_to_first_index(self, hash);
    assume(index != index_seek);

    loop {
        assume(index != CTRL_INDEX, "index is not in hash chain");
        TNode* node = table_node(self, index);
        if (node->_hnext == index_seek) {
            node->_hnext = index_new;
            break;
        }
        index = node->_hnext;
    }
}

static void table_join_before(GTable* self, a_i32 index_next, a_i32 index) {
    TNode* node_next = table_node(self, index_next);
    TNode* node_prev = table_node(self, node_next->_lprev);
    TNode* node = table_node(self, index);

    node->_lnext = index_next;
    node->_lprev = node_next->_lprev;
    node_prev->_lnext = index;
    node_next->_lprev = index;
}

static a_i32 table_emplace_backward(a_henv env, GTable* self, Value key, a_hash hash, Value value) {
    a_i32 index;
    TNode* node;

    a_i32 index_head = table_hash_to_first_index(self, hash);
    TNode* node_head = &self->_ptr[index_head];

    if (v_is_nil(node_head->_key)) {
        index = index_head;
        node = node_head;

        node->_hnext = CTRL_INDEX;
    }
    else {
        a_i32 index_free = table_reserve_free(self, index_head);
        TNode* node_free = table_node(self, index_free);

        if (table_is_head_of_hash_chain(self, index_head, node_head)) {
            index = index_free;
            node = node_free;

            node->_hnext = node_head->_hnext;
            node_head->_hnext = index;
        }
        else {
            table_redirect_from_hash_chain(self, index_head, node_head->_hash, index_free);
            table_move_node(env, self, index_head, index_free);

            index = index_head;
            node = node_head;

            node_head->_hnext = CTRL_INDEX;
        }
    }

    v_set(env, &node->_key, key);
    v_set(env, &node->_value, value);
    node->_hash = hash;
    table_join_before(self, CTRL_INDEX, index);

    return index;
}

static void table_resize(a_henv env, GTable* self, a_usize old_cap, a_usize new_cap) {
    assume(new_cap > 0, "resize nothing");

    TNode* old_ptr = self->_ptr;
    TNode* new_ptr = bucket_alloc(env, new_cap);

    self->_hmask = new_cap - 1;
    self->_ptr = new_ptr;

    if (old_ptr != null) {
        if (self->_len > 0) {
            TNode* node = &old_ptr[CTRL_INDEX];
            for (a_i32 index = node->_lnext; index >= 0; index = node->_lnext) {
                node = &old_ptr[index];
                table_emplace_backward(env, self, node->_key, node->_hash, node->_value);
            }
        }

        bucket_dealloc(G(env), old_ptr, old_cap);
    }
}

static a_usize table_capacity(GTable* self) {
    return (self->_hmask + usizec(1)) & ~usizec(1);
}

static a_bool table_grow_amortized(a_henv env, GTable* self, a_ulen add) {
    a_ulen need = try_add(self->_len, add);
    if (unlikely(need > TABLE_MAX_CAP)) return true;

    a_usize old_cap = table_capacity(self);
    a_usize new_cap = ceil_power_of_two(need);
    new_cap = max(new_cap, 4);
    assume(new_cap <= TABLE_MAX_CAP);

    table_resize(env, self, old_cap, new_cap);
    return false;
}

void ai_table_grow(a_henv env, GTable* self, a_ulen add) {
    catch (table_grow_amortized(env, self, add)) {
        ai_err_raisef(env, ALO_EINVAL, "too many elements.");
    }
}

void ai_table_hint(a_henv env, GTable* self, a_ulen add) {
    if (unlikely(add > table_capacity(self) - self->_len)) {
        ai_table_grow(env, self, add);
    }
}

static void table_erase(unused a_henv env, GTable* self, a_i32 index) {
    TNode* node = table_node(self, index);
    TNode* node_ctrl = table_ctrl_node(self);

    a_i32 index_prev = node->_lprev;
    a_i32 index_next = node->_lnext;

    TNode* node_prev = table_node(self, index_prev);
    TNode* node_next = table_node(self, index_next);

    table_redirect_from_hash_chain(self, index, node->_hash, node->_hnext);
    node->_hnext = node_ctrl->_hnext;
    node_ctrl->_hnext = index;

    node_prev->_lnext = index_next;
    node_next->_lprev = index_prev;
}

static a_bool table_find_with_trivial_equality(GTable* self, Value vk, a_hash hash, a_i32* pindex) {
    if (self->_len == 0) return true;

    a_i32 index = table_hash_to_first_index(self, hash);
    TNode* node = table_node(self, index);
    if (!table_is_head_of_hash_chain(self, index, node)) return true;

    loop {
        if (v_trivial_equals(vk, node->_key)) {
            *pindex = index;
            return false;
        }
        index = node->_hnext;
        if (index == CTRL_INDEX) {
            return true;
        }
        node = table_node(self, index);
    }
}

static a_bool table_find_with_generic_equality(a_henv env, GTable* self, Value vk, a_hash hash, a_i32* pindex) {
    if (self->_len == 0) return true;

    a_i32 index = table_hash_to_first_index(self, hash);
    TNode* node = table_node(self, index);
    if (!table_is_head_of_hash_chain(self, index, node)) return true;

    loop {
        if (ai_vm_equals(env, vk, node->_key)) {
            *pindex = index;
            return false;
        }
        index = node->_hnext;
        if (index == CTRL_INDEX) {
            return true;
        }
        node = table_node(self, index);
    }
}

static a_bool table_find(a_henv env, GTable* self, Value vk, a_hash* phash, a_i32* pindex) {
    if (likely(v_is_str(vk))) {
        *phash = v_as_str(vk)->_hash;
        return table_find_with_trivial_equality(self, vk, *phash, pindex);
    }
    else if (likely(v_has_trivial_equals(vk))) {
        if (v_is_nil(vk)) {
            return true;
        }
        *phash = v_trivial_hash(vk);
        return table_find_with_trivial_equality(self, vk, *phash, pindex);
    }
    else if (unlikely(v_is_float(vk))) {
        if (unlikely(v_is_nan(vk))) {
            return true;
        }
        vk = v_float_key(vk);
        *phash = v_float_hash(vk);
        return table_find_with_trivial_equality(self, vk, *phash, pindex);
    }
    else {
        *phash = ai_vm_hash(env, vk);
        return table_find_with_generic_equality(env, self, vk, *phash, pindex);
    }
}

union LstrFindResult {
    a_i32 _index;
    GStr* _key;
};

static a_bool table_find_lstr(a_henv env, GTable* self, char const* ptr, a_usize len, a_hash hash, union LstrFindResult* presult) {
    GStr* k = ai_str_get_or_null_with_hash(env, ptr, len, hash);
    if (k == null) {
        presult->_key = null;
        return true;
    }
    if (table_find_with_trivial_equality(self, v_of_str(k), hash, &presult->_index)) {
        presult->_key = k;
        return true;
    }
    return false;
}

a_bool ai_table_get(a_henv env, GTable* self, Value vk, Value* pv) {
    a_i32 index;
    a_hash hash;

    try (table_find(env, self, vk, &hash, &index));

    TNode* node = table_node(self, index);
    v_cpy(env, pv, &node->_value);
    return false;
}

a_bool ai_table_geti(a_henv env, GTable* self, a_int k, Value* pv) {
    a_i32 index;
    Value vk = v_of_int(k);

    try (table_find_with_trivial_equality(self, vk, v_trivial_hash(vk), &index));

    TNode* node = table_node(self, index);
    v_cpy(env, pv, &node->_value);
    return false;
}

a_bool ai_table_getls(a_henv env, GTable* self, char const* ptr, a_usize len, Value* pv) {
    union LstrFindResult result;
    a_hash hash = ai_str_hashof(env, ptr, len);

    try (table_find_lstr(env, self, ptr, len, hash, &result));

    TNode* node = table_node(self, result._index);
    v_set(env, pv, node->_value);
    return false;
}

a_bool ai_table_gets(unused a_henv env, GTable* self, GStr* k, Value* pv) {
    a_i32 index;
    Value vk = v_of_str(k);

    try (table_find_with_trivial_equality(self, vk, k->_hash, &index));

    TNode* node = table_node(self, index);
    v_cpy(env, pv, &node->_value);
    return false;
}

a_bool ai_table_set(a_henv env, GTable* self, Value vk, Value vv) {
    a_i32 index;
    a_hash hash;

    if (!table_find(env, self, vk, &hash, &index)) {
        v_set(env, &self->_ptr[index]._value, vv);
        return true;
    }
    else {
        ai_table_hint(env, self, 1);
        table_emplace_backward(env, self, vk, hash, vv);
        self->_len += 1;

        ai_gc_barrier_backward_val(env, self, vk);
        ai_gc_barrier_backward_val(env, self, vv);

        return false;
    }
}

Value* ai_table_refls(a_henv env, GTable* self, char const* ptr, a_usize len) {
    union LstrFindResult result;
    a_hash hash = ai_str_hashof(env, ptr, len);

    catch (table_find_lstr(env, self, ptr, len, hash, &result)) {
        ai_table_hint(env, self, 1);

        GStr* key = result._key ?: ai_str_new_with_hash(env, ptr, len, hash);
        result._index = table_emplace_backward(env, self, v_of_str(key), hash, v_of_nil());

        self->_len += 1;

        ai_gc_barrier_backward(env, self, key);

        break;
    }

    TNode* node = table_node(self, result._index);
    return &node->_value;
}

a_bool ai_table_del(a_henv env, GTable* self, Value vk) {
    a_i32 index;
    a_hash hash;

    try (table_find(env, self, vk, &hash, &index));

    table_erase(env, self, index);
    self->_len -= 1;
    return true;
}

a_msg ai_table_uset(a_henv env, GTable* self, Value vk, Value vv) {
    a_i32 index;
    a_hash hash;

    catch (table_find(env, self, vk, &hash, &index)) {
        return ALO_EEMPTY;
    }

    TNode* node = table_node(self, index);
    v_set(env, &node->_value, vv);
    return ALO_SOK;
}

static void table_drop(Global* gbl, GTable* self) {
    if (self->_ptr != null) {
        bucket_dealloc(gbl, self->_ptr, self->_hmask + 1);
    }
    ai_mem_dealloc(gbl, self, table_size());
}

static void table_mark(Global* gbl, GTable* self) {
	if (self->_ptr != null) {
        a_u32 cap = self->_hmask + 1;
		for (a_u32 i = 0; i < cap; ++i) {
			TNode* node = &self->_ptr[i];
			if (!v_is_nil(node->_key)) {
				ai_gc_trace_mark_val(gbl, node->_key);
				ai_gc_trace_mark_val(gbl, node->_value);
			}
		}
		ai_gc_trace_work(gbl, sizeof(TNode) * (cap + 1));
	}
	ai_gc_trace_work(gbl, table_size());
}

static VTable const table_vtable = {
	._stencil = V_STENCIL(T_TABLE),
    ._tag = ALO_TTABLE,
	._type_ref = g_type_ref(ALO_TTABLE),
    ._flags = VTABLE_FLAG_NONE,
	._slots = {
        [vfp_slot(drop)] = table_drop,
        [vfp_slot(mark)] = table_mark
	}
};