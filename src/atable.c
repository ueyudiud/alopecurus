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

static KWeak const table_klass[];

/**
 * Control Block
 *
 * A control block wraps a 7-bit hash tag and a 1-bit occupancy marker,
 * and is probed in groups of TCTRL_GROUP_WIDTH blocks.
 */
typedef a_u8 TCtrl;

typedef struct TCtrlGroup TCtrlGroup;

TCtrlGroup tctrl_load_group(TCtrl const* pctrl);

/* Get the bitmap of blocks which equal to the given one. */
a_u32 tctrl_match_group(TCtrlGroup group, TCtrl c);

#if (ALO_ARCH_X86 || ALO_ARCH_X64) && defined(__SSE2__)

# include <emmintrin.h>

#define TCTRL_EMPTY   u8c(0x00)
#define TCTRL_DEAD    u8c(0x01)
#define TCTRL_H2_MASK u32c(0x7f)
#define TCTRL_H2_MARK u32c(0x80)

#define TCTRL_GROUP_WIDTH u32c(16)

struct TCtrlGroup {
    __m128i value;
};

ALO_INLINE TCtrlGroup tctrl_load_group(TCtrl const* pctrl) {
    assume((ptr2int(pctrl) & (alignof(TCtrlGroup) - 1)) == 0, "unaligned control array");
    return (TCtrlGroup) { _mm_load_si128(cast(__m128i const*, pctrl)) };
}

ALO_INLINE a_u32 tctrl_match_group(TCtrlGroup group, TCtrl c) {
    return cast(a_u32, _mm_movemask_epi8(_mm_cmpeq_epi8(group.value, _mm_set1_epi8(cast(char, c)))));
}

#elif ALO_SIMD_GCC

#define TCTRL_EMPTY   u8c(0x00)
#define TCTRL_DEAD    u8c(0x01)
#define TCTRL_H2_MASK u32c(0x7f)
#define TCTRL_H2_MARK u32c(0x80)

#define TCTRL_GROUP_WIDTH 16

struct TCtrlGroup {
    TCtrl value __attribute__((vector_size(TCTRL_GROUP_WIDTH)));
};

static_assert(alignof(TCtrlGroup) <= TCTRL_GROUP_WIDTH);

ALO_INLINE TCtrlGroup tctrl_load_group(TCtrl const* pctrl) {
    assume((ptr2int(pctrl) & (alignof(TCtrlGroup) - 1)) == 0, "unaligned control array");
    return (TCtrlGroup) { *cast(TCtrlGroup const*, pctrl) };
}

ALO_INLINE a_u32 tctrl_match_group(TCtrlGroup group, TCtrl c) {
    a_u32 m = 0;
    for (a_u32 i = 0; i < TCTRL_GROUP_WIDTH; ++i) {
        if (group.value[i] == c) {
            m |= 1 << i;
        }
    }
    return m;
}

#else

#define TCTRL_EMPTY   u8c(0x00)
#define TCTRL_DEAD    u8c(0x01)
#define TCTRL_H2_MASK u32c(0x7f)
#define TCTRL_H2_MARK u32c(0x80)

#define TCTRL_GROUP_WIDTH __SIZEOF_SIZE_T__

struct TCtrlGroup {
    alignas(alignof(a_usize)) TCtrl value[sizeof(a_usize) / sizeof(TCtrl)];
};

ALO_INLINE TCtrlGroup tctrl_load_group(TCtrl const* pctrl) {
    return *cast(TCtrlGroup const*, pctrl);
}

/**
 ** Get the bitmap of blocks which equal to the given one.
 */
ALO_INLINE a_u32 tctrl_match_group(TCtrlGroup group, TCtrl c) {
    a_u32 m = 0;
    for (a_u32 i = 0; i < TCTRL_GROUP_WIDTH; ++i) {
        if (group.value[i] == c) {
            m |= 1 << i;
        }
    }
    return m;
}

#endif

/**
 ** Build a control block from the hash code, low 7 bits are the tag.
 */
ALO_INLINE TCtrl tctrl_make(a_hash hash) {
    return cast(TCtrl, (hash & TCTRL_H2_MASK) | TCTRL_H2_MARK);
}

#define TLINK_HEADER \
    alignas(alignof(TCtrlGroup)) a_byte align[0]; \
    a_i32 lprev;     \
    a_i32 lnext

/**
 * Table link.
 */
struct TLink {
    TLINK_HEADER;
};

/**
 ** Table node.
 */
typedef union {
    TLink link;
    struct {
        TLINK_HEADER;
        a_hash hash;
        Value key;
        Value value;
    };
} TNode;

/*
 * The load factor is limited to 7/8, so that every probe sequence can
 * always meet an empty control block before it wraps around.
 * The capacity is also limited so that node indices always fit in a_i32.
 */
#define TABLE_MAX_CAP (cast(a_ulen, INT32_MAX) / 8 * 7)

#define TABLE_MIN_CAP cast(a_ulen, TCTRL_GROUP_WIDTH)

#define CTRL_INDEX i32c(-1)

static a_ulen table_nonempty_capacity(GTable* self) {
    assume(self->hmask != 0, "table bucket is empty");
    return self->hmask + 1;
}

static a_ulen table_capacity(GTable* self) {
    return self->hmask != 0 ? table_nonempty_capacity(self) : 0;
}

static a_usize bucket_size(a_ulen cap) {
    return sizeof(TNode) * cap + sizeof(TLink) + sizeof(TCtrl) * cap;
}

static void* bucket_alloc(a_henv env, a_ulen cap) {
    a_usize node_size = sizeof(TNode) * cap + sizeof(TLink);
    a_usize size = bucket_size(cap);

    void* block = ai_mem_alloc(env, size);

    /* Fill nodes with -1 raw, which is the strict nil key for the iterator. */
    memset(block, -1, node_size);
    memclr(addr_add(TCtrl, block, node_size), sizeof(TCtrl) * cap);

    return addr_add(void, block, sizeof(TNode) * (cap - 1));
}

static void bucket_dealloc(Global* gbl, void* bucket, a_ulen cap) {
    ai_mem_dealloc(gbl, addr_sub(void, bucket, sizeof(TNode) * (cap - 1)), bucket_size(cap));
}

static TLink* bucket_link(TLink* ptr, a_ulen cap, a_i32 index) {
    assume(index == CTRL_INDEX || (index >= 0 && cast(a_u32, index) < cap), "index out of bound.");
    TNode* bucket = from_member(TNode, link, ptr);
    return &(bucket - index)->link;
}

static TNode* bucket_node(TLink* ptr, a_ulen cap, a_i32 index) {
    assume(index >= 0 && cast(a_u32, index) < cap, "index out of bound.");
    TNode* bucket = from_member(TNode, link, ptr);
    return bucket - index;
}

GTable* ai_table_new(a_henv env, a_enum kv) {
    assume(kv == REFERENCE_STRONG || kv == REFERENCE_WEAKKEY || kv == REFERENCE_WEAK || kv == REFERENCE_PHANTOM, "bad reference strategy.");

    GTable* self = ai_mem_alloc(env, table_size());

    static struct {
        TLink link;
        TCtrl ctrl[TCTRL_GROUP_WIDTH];
    } const g_empty = {};

	self->klass = &table_klass[kv];
    self->ptr = cast(TLink*, &g_empty);
    self->hmask = 0;
    self->len = 0;
    self->nempty = 0;

	ai_gc_register_normal(env, self);
    return self;
}

static TNode* table_node(GTable* self, a_ilen index) {
    return bucket_node(self->ptr, table_nonempty_capacity(self), index);
}

static a_i32 table_node_index(GTable* self, TNode* node) {
    assume(table_node(self, self->hmask) <= node && node <= table_node(self, 0), "node out of bound.");
    return cast(a_i32, cast(TNode*, self->ptr) - node);
}

/**
 ** Get the control block array, which begins at the end of the TNode array.
 ** The control node is 'ptr[1]', so the control block array starts right
 ** after it, which is also the end of the bucket block of nodes.
 */
static TCtrl* table_ctrls(GTable* self) {
    assume(self->ptr != nullptr, "table is not allocated.");
    return addr_add(TCtrl, self->ptr, sizeof(TNode) + sizeof(TLink));
}

static void table_join_before(GTable* self, a_i32 index_next, a_i32 index) {
    void* ptr = self->ptr;
    a_ulen cap = table_nonempty_capacity(self);
    TLink* node_next = bucket_link(ptr, cap, index_next);
    TLink* node_prev = bucket_link(ptr, cap, node_next->lprev);
    TNode* node = bucket_node(ptr, cap, index);

    node->lnext = index_next;
    node->lprev = node_next->lprev;
    node_prev->lnext = index;
    node_next->lprev = index;
}

/**
 ** Get the first group of the probe sequence of the given hash.
 ** The high bits select the group and the low 7 bits are the tag, so that
 ** the probe sequence and the tag are independent of each other.
 */
static a_u32 table_probe_start(GTable* self, a_hash hash) {
    return (hash >> 7) & self->hmask & ~(TCTRL_GROUP_WIDTH - 1);
}

/**
 ** Reserve a vacant slot for the given hash.
 ** The slot is either empty or the first deleted one in the probe sequence.
 */
static a_i32 table_reserve_slot(GTable* self, a_hash hash) {
    a_u32 mask = self->hmask;
    TCtrl* pctrl = table_ctrls(self);
    a_u32 group_index = table_probe_start(self, hash);
    a_i32 index_reuse = CTRL_INDEX;

    loop {
        TCtrlGroup group = tctrl_load_group(pctrl + group_index);

        a_u32 match = tctrl_match_group(group, TCTRL_DEAD);
        if (index_reuse == CTRL_INDEX && match != 0) {
            index_reuse = cast(a_i32, group_index + count_trailing_zero(match));
        }

        match = tctrl_match_group(group, TCTRL_EMPTY);
        if (match != 0) {
            a_u32 index = group_index + count_trailing_zero(match);
            return index_reuse != CTRL_INDEX ? index_reuse : cast(a_i32, index);
        }

        group_index = (group_index + TCTRL_GROUP_WIDTH) & mask;
    }
}

static a_i32 table_emplace_backward(a_henv env, GTable* self, Value key, a_hash hash, Value value) {
    a_i32 index = table_reserve_slot(self, hash);
    TNode* node = table_node(self, index);
    TCtrl* pctrl = table_ctrls(self) + index;

    assume(v_is_nil(node->key), "slot is occupied.");

    if (*pctrl == TCTRL_DEAD) {
        /* Reuse a tombstone, the count of empty slots is untouched. */
    }
    else {
        assume(*pctrl == TCTRL_EMPTY, "slot is not vacant.");
        self->nempty -= 1;
    }
    *pctrl = tctrl_make(hash);

    v_set(env, &node->key, key);
    v_set(env, &node->value, value);
    node->hash = hash;

    table_join_before(self, CTRL_INDEX, index);

    return index;
}

/**
 ** Get the capacity which is able to hold 'nuse' slots under the load factor.
 */
static a_ulen table_grown_capacity(a_ulen nuse) {
    a_ulen cap = TABLE_MIN_CAP;
    while (cap - cap / 8 < nuse)
        cap += cap;
    return cap;
}

static void table_resize(a_henv env, GTable* self, a_ulen old_cap, a_ulen new_cap) {
    assume(new_cap > 0, "resize nothing");

    void* old_ptr = self->ptr;
    void* new_ptr = bucket_alloc(env, new_cap);

    self->hmask = new_cap - 1;
    self->nempty = new_cap - new_cap / 8;
    self->ptr = new_ptr;

    if (old_cap != 0) {
        if (self->len > 0) {
            TLink* link = bucket_link(old_ptr, old_cap, CTRL_INDEX);
            for (a_i32 index = link->lnext; index >= 0; index = link->lnext) {
                TNode* node = bucket_node(old_ptr, old_cap, index);
                table_emplace_backward(env, self, node->key, node->hash, node->value);
                link = &node->link;
            }
        }

        bucket_dealloc(G(env), old_ptr, old_cap);
    }
}

static a_bool table_grow_amortized(a_henv env, GTable* self, a_ulen add) {
    a_ulen need = try_add(self->len, add);
    if (unlikely(need > TABLE_MAX_CAP))
        return true;

    a_ulen old_cap = table_capacity(self);
    a_ulen new_cap = table_grown_capacity(need);
    new_cap = max(new_cap, old_cap);

    table_resize(env, self, old_cap, new_cap);
    return false;
}

void ai_table_grow(a_henv env, GTable* self, a_ulen add) {
    if (table_grow_amortized(env, self, add)) {
        ai_err_raisef(env, ALO_EINVAL, "too many elements.");
    }
}

void ai_table_hint(a_henv env, GTable* self, a_ulen add) {
    if (unlikely(add > self->nempty)) {
        ai_table_grow(env, self, add);
    }
}

static void table_erase(GTable* self, a_i32 index) {
    TNode* node = table_node(self, index);

    a_i32 index_prev = node->lprev;
    a_i32 index_next = node->lnext;

    TNode* node_prev = table_node(self, index_prev);
    TNode* node_next = table_node(self, index_next);

    v_set_nil(&node->key);
    v_set_nil(&node->value);
    *(table_ctrls(self) + index) = TCTRL_DEAD;

    node_prev->lnext = index_next;
    node_next->lprev = index_prev;

    self->len -= 1;
}

static a_bool table_find_in(a_henv env, GTable* self, Value vk, a_hash hash, a_bool trivial, a_i32* pindex) {
    if (self->len == 0)
        return true;

    void* bucket = self->ptr;
    a_u32 mask = self->hmask;
    TCtrl* pctrl = table_ctrls(self);
    a_u32 group_index = table_probe_start(self, hash);
    TCtrl ctrl = tctrl_make(hash);

    loop {
        TCtrlGroup group = tctrl_load_group(pctrl + group_index);
        a_u32 match = tctrl_match_group(group, ctrl);

        while (match != 0) {
            a_u32 index = group_index + count_trailing_zero(match);
            TNode* node = bucket_node(bucket, mask + 1, index);

            a_bool equals = trivial
                    ? v_trivial_equals(vk, node->key)
                    : ai_vm_equals(env, vk, node->key);

            if (equals) {
                *pindex = cast(a_i32, index);
                return false;
            }

            match &= match - 1;
        }

        if (tctrl_match_group(group, TCTRL_EMPTY) != 0)
            return true;

        group_index = (group_index + TCTRL_GROUP_WIDTH) & mask;
    }
}

static a_bool table_find(a_henv env, GTable* self, Value vk, a_hash* phash, a_i32* pindex) {
    if (likely(v_is_str(vk))) {
        *phash = v_as_str(vk)->hash;
        return table_find_in(env, self, vk, *phash, true, pindex);
    }
    else if (likely(v_has_trivial_equals(vk))) {
        if (v_is_nil(vk))
            return true;
        *phash = v_trivial_hash(vk);
        return table_find_in(env, self, vk, *phash, true, pindex);
    }
    else if (unlikely(v_is_float(vk))) {
        if (unlikely(v_is_nan(vk)))
            return true;
        vk = v_float_key(vk);
        *phash = v_float_hash(vk);
        return table_find_in(env, self, vk, *phash, true, pindex);
    }
    else {
        *phash = ai_vm_hash(env, vk);
        return table_find_in(env, self, vk, *phash, false, pindex);
    }
}

union LstrFind {
    a_i32 index;
    GStr* key;
};

static a_bool table_find_lstr(a_henv env, GTable* self, char const* kptr, a_usize klen, a_hash hash, union LstrFind* out) {
    GStr* key = ai_str_get_or_nullptr_with_hash(env, kptr, klen, hash);
    if (key == nullptr) {
        out->key = nullptr;
        return true;
    }
    if (table_find_in(env, self, v_of_str(key), hash, true, &out->index)) {
        out->key = key;
        return true;
    }
    return false;
}

a_bool ai_table_get(a_henv env, GTable* self, Value vk, Value* pv) {
    a_i32 index;
    a_hash hash;

    try (table_find(env, self, vk, &hash, &index));

    TNode* node = table_node(self, index);
    v_cpy(env, pv, &node->value);
    return false;
}

a_bool ai_table_geti(a_henv env, GTable* self, a_int k, Value* pv) {
    a_i32 index;
    Value vk = v_of_int(k);

    try (table_find_in(env, self, vk, v_trivial_hash(vk), true, &index));

    TNode* node = table_node(self, index);
    v_cpy(env, pv, &node->value);
    return false;
}

a_bool ai_table_getls(a_henv env, GTable* self, char const* kptr, a_usize klen, Value* pv) {
    union LstrFind out;
    a_hash hash = ai_str_hashof(env, kptr, klen);

    try (table_find_lstr(env, self, kptr, klen, hash, &out));

    TNode* node = table_node(self, out.index);
    v_set(env, pv, node->value);
    return false;
}

a_bool ai_table_gets(a_henv env, GTable* self, GStr* k, Value* pv) {
    a_i32 index;
    Value vk = v_of_str(k);

    try (table_find_in(env, self, vk, k->hash, true, &index));

    TNode* node = table_node(self, index);
    v_cpy(env, pv, &node->value);
    return false;
}

a_bool ai_table_set(a_henv env, GTable* self, Value vk, Value vv) {
    a_i32 index;
    a_hash hash;

    if (!table_find(env, self, vk, &hash, &index)) {
        v_set(env, &table_node(self, index)->value, vv);
        return true;
    }
    else {
        ai_table_hint(env, self, 1);
        table_emplace_backward(env, self, vk, hash, vv);
        self->len += 1;

        ai_gc_barrier_backward_val(env, self, vk);
        ai_gc_barrier_backward_val(env, self, vv);

        return false;
    }
}

Value* ai_table_refls(a_henv env, GTable* self, char const* kptr, a_usize klen) {
    union LstrFind out;
    a_hash hash = ai_str_hashof(env, kptr, klen);

    if (table_find_lstr(env, self, kptr, klen, hash, &out)) {
        ai_table_hint(env, self, 1);

        GStr* key = out.key ?: ai_str_new_with_hash(env, kptr, klen, hash);
        out.index = table_emplace_backward(env, self, v_of_str(key), hash, v_of_nil());

        self->len += 1;

        ai_gc_barrier_backward(env, self, key);
    }

    TNode* node = table_node(self, out.index);
    return &node->value;
}

a_bool ai_table_del(a_henv env, GTable* self, Value vk) {
    a_i32 index;
    a_hash hash;

    try (table_find(env, self, vk, &hash, &index));

    table_erase(self, index);
    return true;
}

void ai_table_delr(a_henv env, GTable* self, Value* ref) {
    quiet(env);
    TNode* node = from_member(TNode, value, ref);
    a_i32 index = table_node_index(self, node);
    table_erase(self, index);
}

a_bool ai_table_next(a_henv env, GTable* self, Value* rk, a_int* pindex, Value* pv) {
    a_i32 index = *pindex;
    a_ulen cap = table_capacity(self);
    void* ptr = self->ptr;

    TLink* l = bucket_link(ptr, cap, CTRL_INDEX);
    if (index != CTRL_INDEX) {
        /* Check index validity, since the table may changed during iteration. */
        TNode* n = from_member(TNode, link, l);

        Value vk = *rk;
        if (!v_trivial_equals(vk, n->key)) {
            /* Failed, try to locate index again. */
            if (table_find_in(env, self, vk, ai_vm_hash(env, vk), true, &index)) {
                /* No key found, raise error here. */
                ai_err_raisef(env, ALO_EINVAL, "modified iterator.");
            }
            l = bucket_link(ptr, cap, index);
        }
    }

    index = l->lnext;
    if (index < 0)
        return true;
    *pindex = index;

    TNode* n = table_node(self, index);
    v_cpy(env, rk, &n->key);
    v_cpy(env, pv, &n->value);

    return false;
}

a_msg ai_table_uset(a_henv env, GTable* self, Value vk, Value vv) {
    a_i32 index;
    a_hash hash;

    if (table_find(env, self, vk, &hash, &index)) {
        return ALO_EEMPTY;
    }

    TNode* node = table_node(self, index);
    v_set(env, &node->value, vv);
    return ALO_SOK;
}

static void table_drop(Global* gbl, GTable* self) {
    if (self->hmask != 0) {
        bucket_dealloc(gbl, self->ptr, table_nonempty_capacity(self));
    }
    ai_mem_dealloc(gbl, self, table_size());
}

static void table_mark(Global* gbl, GTable* self) {
    if (self->len > 0) {
        a_u32 cap = table_nonempty_capacity(self);
        void* ptr = self->ptr;
		for (a_u32 i = 0; i < cap; ++i) {
            TNode* node = bucket_node(ptr, cap, i);
			if (!v_is_nil(node->key)) {
                v_trace(gbl, node->key);
                v_trace(gbl, node->value);
			}
		}
		ai_gc_trace_work(gbl, bucket_size(cap));
	}
	ai_gc_trace_work(gbl, table_size());
}

static void wtable_mark(Global* gbl, GTable* self) {
    if (self->len > 0) {
        a_bool has_weak = false;
        a_u32 cap = table_nonempty_capacity(self);
        void* ptr = self->ptr;
        for (a_u32 i = 0; i < cap; ++i) {
            TNode* node = bucket_node(ptr, cap, i);
            if (!v_is_nil(node->key)) {
                if (v_is_alive(gbl, node->value)) {
                    v_trace(gbl, node->key);
                }
                else {
                    has_weak = true;
                }
            }
        }
        if (has_weak) {
            if (gbl->gcstep == GCSTEP_TRACE) {
                join_trace(&gbl->tr_regray, self);
            }
            else {
                join_trace(&gbl->tr_weak, self);
            }
        }
        ai_gc_trace_work(gbl, bucket_size(cap));
    }
    ai_gc_trace_work(gbl, table_size());
}

static void wktable_mark(Global* gbl, GTable* self) {
    if (self->len > 0) {
        a_bool has_weak = false;
        a_bool has_blur = false;
        a_u32 cap = table_nonempty_capacity(self);
        void* ptr = self->ptr;
        for (a_u32 i = 0; i < cap; ++i) {
            TNode* node = bucket_node(ptr, cap, i);
            if (!v_is_nil(node->key)) {
                if (v_is_alive(gbl, node->key)) {
                    v_trace(gbl, node->value);
                }
                else {
                    has_weak = true;
                    if (!v_is_alive(gbl, node->value)) {
                        has_blur = true;
                    }
                }
            }
        }
        if (has_weak) {
            if (gbl->gcstep == GCSTEP_TRACE) {
                join_trace(&gbl->tr_regray, self);
            }
            else if (has_blur) {
                join_trace(&gbl->tr_blur, self);
            }
            else {
                join_trace(&gbl->tr_weak, self);
            }
        }
        ai_gc_trace_work(gbl, bucket_size(cap));
    }
    ai_gc_trace_work(gbl, table_size());
}

static void ptable_mark(Global* gbl, GTable* self) {
    if (self->len > 0) {
        a_bool has_weak_key = false;
        a_bool has_weak = false;
        a_bool has_blur = false;
        a_ulen cap = table_nonempty_capacity(self);
        void* ptr = self->ptr;
        for (a_u32 i = 0; i < cap; ++i) {
            TNode* node = bucket_node(ptr, cap, i);
            if (!v_is_nil(node->key)) {
                a_bool weak_key = !v_is_alive(gbl, node->key);
                a_bool weak_value = !v_is_alive(gbl, node->value);
                has_weak |= weak_key || weak_value;
                has_weak_key |= weak_key;
                has_blur |= weak_key && weak_value;
            }
        }
        if (has_weak) {
            if (gbl->gcstep == GCSTEP_TRACE) {
                join_trace(&gbl->tr_regray, self);
            }
            else if (has_blur) {
                join_trace(&gbl->tr_blur, self);
            }
            else if (has_weak_key) {
                join_trace(&gbl->tr_phantom, self);
            }
            else {
                join_trace(&gbl->tr_weak, self);
            }
        }
        ai_gc_trace_work(gbl, bucket_size(cap));
    }
    ai_gc_trace_work(gbl, table_size());
}

static void wtable_clear(Global* gbl, GTable* self) {
    if (self->len > 0) {
        a_u32 cap = table_nonempty_capacity(self);
        void* ptr = self->ptr;
        for (a_u32 i = 0; i < cap; ++i) {
            TNode* node = bucket_node(ptr, cap, i);
            if (!v_is_nil(node->key) && !v_is_alive(gbl, node->value)) {
                table_erase(self, cast(a_i32, i));
            }
        }
    }
}

static void ptable_clear(Global* gbl, GTable* self) {
    if (self->len > 0) {
        a_u32 cap = table_nonempty_capacity(self);
        void* ptr = self->ptr;
        a_bool trace_weak = gbl->gcstep == GCSTEP_TRACE_WEAK;
        for (a_u32 i = 0; i < cap; ++i) {
            TNode* node = bucket_node(ptr, cap, i);
            if (v_is_nil(node->key))
                continue;

            a_bool dead_key = !v_is_alive(gbl, node->key);
            a_bool dead_value = !v_is_alive(gbl, node->value);
            if (trace_weak ? dead_key || dead_value : !dead_key && dead_value) {
                table_erase(self, cast(a_i32, i));
            }
        }
    }
}

static KWeak const table_klass[] = {
    [REFERENCE_STRONG] = {
        .tag = ALO_TTABLE,
        .name = "table",
        .flags = KLASS_FLAG_NONE,
        .drop = table_drop,
        .mark = table_mark
    },
    [REFERENCE_WEAK] = {
        .tag = ALO_TTABLE,
        .name = "table",
        .flags = KLASS_FLAG_NONE,
        .drop = table_drop,
        .mark = wtable_mark,
        .clear = wtable_clear
    },
    [REFERENCE_WEAKKEY] = {
        .tag = ALO_TTABLE,
        .name = "table",
        .flags = KLASS_FLAG_NONE,
        .drop = table_drop,
        .mark = wktable_mark,
        .clear = ptable_clear
    },
    [REFERENCE_PHANTOM] = {
        .tag = ALO_TTABLE,
        .name = "table",
        .flags = KLASS_FLAG_NONE,
        .drop = table_drop,
        .mark = ptable_mark,
        .clear = ptable_clear
    }
};
