/**
 *@file atable.c
 */

#define atable_c_
#define ALO_LIB

#include "aop.h"
#include "astr.h"
#include "amem.h"
#include "agc.h"
#include "avm.h"
#include "aapi.h"

#include "atable.h"

typedef struct HBucket HBucket;

/**
 ** Table bucket.
 */
struct HBucket {
	a_hash _hmask;
	a_x32 _hfree;
	union {
		HLink _link;
		struct {
			a_x32 _llast;
			a_x32 _lfirst;
		};
	};
	HNode _nodes[];
};

static_assert(offsetof(HNode, _hash) - offsetof(HNode, _hash) == offsetof(HBucket, _hmask));
static_assert(offsetof(HNode, _hnext) - offsetof(HNode, _hash) == offsetof(HBucket, _hfree));
static_assert(offsetof(HNode, _link) - offsetof(HNode, _hash) == offsetof(HBucket, _link));
static_assert(sizeof(HNode) - offsetof(HNode, _hash) == offsetof(HBucket, _nodes));

#ifndef ALOI_TABLE_LOAD_FACTOR
# define ALOI_TABLE_LOAD_FACTOR 0.75
#endif

static VTable const table_vtable;

static a_usize bucket_size(a_usize cap) {
	return sizeof(HBucket) + sizeof(HNode) * cap;
}

static HBucket* table_bucket(GTable* self) {
	return from_member(HBucket, _nodes[-1], self->_ptr);
}

static HNode* bucket_init(HBucket* self, a_usize cap) {
	assume(cap > 0, "bucket is empty.");

	self->_hmask = cap - 1;
	self->_hfree = x32c(1);

	self->_lfirst = self->_llast = nil;

	for (a_u32 i = 0; i < cap; ++i) {
		HNode* node = &self->_nodes[i];
		v_set_nil(&node->_key);
		node->_link = new(HLink) {
			._prev = wrap(i),
			._next = i < cap - 1 ? wrap(i + 2) : nil
		};
	}
	return &self->_nodes[-1];
}

static HNode* bucket_index(HBucket* self, a_x32 id) {
	return &self->_nodes[unwrap_unsafe(id) - 1];
}

static void bucket_alloc(a_henv env, GTable* table, a_usize cap) {
	HBucket* self = ai_mem_alloc(env, bucket_size(cap));
	table->_ptr = bucket_init(self, cap);
	table->_hmask = cap - 1;
}

static void bucket_free(Global* g, HBucket* self) {
	ai_mem_dealloc(g, self, bucket_size(self->_hmask + 1));
}

GTable* ai_table_new(a_henv env) {
    GTable* self = ai_mem_alloc(env, sizeof(GTable));

	self->_vptr = &table_vtable;
	self->_ptr = 0;
	self->_len = 0;
	self->_hmask = 0;

	ai_gc_register_object(env, self);
    return self;
}

static HNode* table_hfirst(GTable* self, a_u32 hash) {
	return &self->_ptr[(hash & self->_hmask) + 1];
}

static a_bool hnode_is_hhead(GTable* table, HNode* node) {
    return !hnode_is_empty(node) && node == table_hfirst(table, node->_hash);
}

static void hnode_put(a_henv env, HNode* node, Value key, a_hash hash, Value value) {
    v_set(env, &node->_key, key);
    v_set(env, &node->_value, value);
    node->_hash = hash;
}

static void hnode_link(GTable* table, HNode* restrict node, a_x32 prev, a_x32 next) {
	HBucket* bucket = table_bucket(table);
	HNode* nodep = bucket_index(bucket, prev);
	HNode* noden = bucket_index(bucket, next);
	a_x32 id = wrap(node - table->_ptr);
	noden->_link._prev = nodep->_link._next = id;
	node->_link = new(HLink) { ._prev = prev, ._next = next };
}

/**
 ** Move entry and link data to another node.
 **
 *@param env the runtime environment.
 *@param noded the destination node.
 *@param nodes the source node.
 */
static void hnode_move(a_henv env, GTable* table, HNode* restrict noded, HNode* restrict nodes) {
	assume(noded != nodes, "cannot move inplace.");
	hnode_put(env, noded, nodes->_key, nodes->_hash, nodes->_value);
	hnode_link(table, noded, nodes->_link._prev, nodes->_link._next);
	noded->_hnext = nodes->_hnext;
}

/**
 ** Pop a node from free list.
 *@param self the table.
 *@return the free node.
 */
static HNode* table_pop_free(GTable* self) {
	HBucket* bucket = table_bucket(self);
	HNode* node = bucket_index(bucket, bucket->_hfree);
	assume(is_nil(node->_link._prev), "not head of free node.");

	bucket->_hfree = node->_link._next;
	HNode* noden = bucket_index(bucket, node->_link._next);
	noden->_link._prev = nil;

	assume(node != null && hnode_is_empty(node));
	return node;
}

static void table_unlink_free(GTable* self, HNode* node) {
	HBucket* bucket = table_bucket(self);

	a_x32 prev = node->_link._prev;
	a_x32 next = node->_link._next;

	if (is_nil(prev)) {
		bucket->_hfree = next;
	}
	else {
		bucket_index(bucket, prev)->_link._next = next;
	}
	if (!is_nil(next)) {
		bucket_index(bucket, next)->_link._prev = prev;
	}
}

/**
 ** Link node at end of linked list of table.
 *@param self the table.
 *@param node the node to be linked.
 */
static void table_link_tail(GTable* self, HNode* node) {
	hnode_link(self, node, self->_ptr->_link._prev, nil);
}

static HNode* table_get_hprev(GTable* self, HNode* node) {
	HNode* nodep = table_hfirst(self, node->_hash);
	loop {
		HNode* node1 = &self->_ptr[unwrap(nodep->_hnext)];
		if (node1 == node) {
			break;
		}
		nodep = node1;
	}
	return nodep;
}

static void table_put(a_henv env, GTable* self, Value key, a_hash hash, Value value) {
    HNode* nodeh = table_hfirst(self, hash);
    if (hnode_is_hhead(self, nodeh)) {
        /* Insert into hash list. */
        HNode* nodet = table_pop_free(self);

		/* Emplace node at another free node. */
        /*                       v
         * h -> n -> ... => h -> t -> n -> ...
         */
		hnode_put(env, nodet, key, hash, value);

		nodet->_hnext = nodeh->_hnext;
		nodeh->_hnext = wrap(nodet - self->_ptr);

		table_link_tail(self, nodet);
    }
    else {
		if (hnode_is_empty(nodeh)) {
			table_unlink_free(self, nodeh);
		}
		else {
			/* Move placed entry to other node. */
			/*                         v
			 * ... -> p -> h -> ... => h; ... -> p -> f -> ...
			 */
			HNode* nodef = table_pop_free(self);
			HNode* nodep = table_get_hprev(self, nodeh);
			hnode_move(env, self, nodef, nodeh);
			nodep->_hnext = wrap(nodef - self->_ptr);
		}
		/* Emplace node locally. */
		/*    v
		 * => h
		 */
		hnode_put(env, nodeh, key, hash, value);
        nodeh->_hnext = nil;
		table_link_tail(self, nodeh);
    }
}

void ai_table_hint(a_henv env, GTable* self, a_usize len) {
    a_usize old_cap = self->_hmask + 1;
    a_usize expect = self->_len + len;
    if (expect > cast(a_usize, old_cap * ALOI_TABLE_LOAD_FACTOR)) {
		a_usize new_cap = ceil_pow2m1_usize(cast(a_usize, ceil(expect / ALOI_TABLE_LOAD_FACTOR))) + 1;
		assume(expect <= new_cap && new_cap > 0);

		HBucket* bucket = self->_ptr != null ? table_bucket(self) : null;

		bucket_alloc(env, self, new_cap);

		if (bucket != null) {
			HNode* node;
			for (a_x32 i = bucket->_lfirst; !is_nil(i); i = node->_link._next) {
				node = bucket_index(bucket, i);
				table_put(env, self, node->_key, node->_hash, node->_value);
			}

			bucket_free(G(env), bucket);
		}
	}
}

static void table_remove(a_henv env, GTable* self, HNode* node) {
	assume(!hnode_is_empty(node), "cannot remove empty node.");

	HBucket* bucket = table_bucket(self);

	/* Remove from link */
	HLink link = node->_link;
	HNode* nodep = bucket_index(bucket, link._prev);
	HNode* noden = bucket_index(bucket, link._next);

	nodep->_link._next = link._next;
	noden->_link._prev = link._prev;

	/* Remove from hash */
	if (hnode_is_hhead(self, node)) {
		if (!is_nil(node->_hnext)) {
			HNode* nodeh = bucket_index(bucket, node->_hnext);
			hnode_move(env, self, node, nodeh);
		}
	}
	else {
		HNode* nodeh = table_get_hprev(self, node);
		nodeh->_hnext = node->_hnext;
	}

	/* Add to free list */
	node->_link = new(HLink) { ._prev = nil, ._next = bucket->_hfree };
	bucket->_hfree = wrap(node - self->_ptr);

	self->_len -= 1;
}

static Value* table_find_id(unused a_henv env, GTable* self, a_hash hash, Value key) {
	if (self->_len > 0) {
		HNode* node = table_hfirst(self, hash);
		if (!hnode_is_empty(node)) {
			do {
				if (v_trivial_equals(key, node->_key)) {
					return &node->_value;
				}
			}
			while (!is_nil(node->_hnext) && (node = &self->_ptr[unwrap(node->_hnext)], true));
		}
	}
	return null;
}

static Value* table_find_str(unused a_henv env, GTable* self, a_hash hash, a_lstr const* key) {
	if (self->_len > 0) {
		HNode* node = table_hfirst(self, hash);
		if (!hnode_is_empty(node)) {
			do {
				if (v_is_str(node->_key) && ai_str_requals(v_as_str(node->_key), key->_ptr, key->_len)) {
					return &node->_value;
				}
			}
			while (!is_nil(node->_hnext) && (node = &self->_ptr[unwrap(node->_hnext)], true));
		}
	}
	return null;
}

static Value* table_find_any(unused a_henv env, GTable* self, a_hash hash, Value key) {
	if (self->_len > 0) {
		HNode* node = table_hfirst(self, hash);
		if (!hnode_is_empty(node)) {
			do {
				if (ai_vm_equals(env, key, node->_key)) {
					return &node->_value;
				}
			}
			while (!is_nil(node->_hnext) && (node = &self->_ptr[unwrap(node->_hnext)], true));
		}
	}
	return null;
}

Value const* ai_table_refi(a_henv env, GTable* self, a_int key) {
    return table_find_id(env, self, v_trivial_hash(v_of_int(key)), v_of_int(key));
}

Value const* ai_table_refs(a_henv env, GTable* self, a_lstr const* key) {
    return table_find_str(env, self, ai_str_hashof(G(env)->_seed, key->_ptr, key->_len), key);
}

Value const* ai_table_refis(a_henv env, GTable* self, GStr* key) {
	return table_find_id(env, self, key->_hash, v_of_obj(key));
}

Value* ai_table_ref(a_henv env, GTable* self, Value key, a_u32* restrict phash) {
	if (likely(v_is_istr(key))) {
		GStr* str = v_as_str(key);
		*phash = str->_hash;
		return table_find_id(env, self, str->_hash, key);
	}
	else if (likely(v_is_hstr(key))) {
		GStr* str = v_as_str(key);
		*phash = str->_hash;
		a_lstr lstr = { ._ptr = str2ntstr(str), ._len = str->_len };
		return table_find_str(env, self, str->_hash, &lstr);
	}
	else if (unlikely(v_is_float(key))) {
		if (unlikely(v_is_nan(key))) {
			return null;
		}
		*phash = v_trivial_hash(key);
		return table_find_id(env, self, *phash, key);
	}
	else if (likely(v_has_trivial_equals(key))) {
		*phash = v_trivial_hash(key);
		return table_find_id(env, self, *phash, key);
	}
	else {
		*phash = ai_vm_hash(env, key);
		return table_find_any(env, self, *phash, key);
	}
}

static void api_check_hitr(GTable* self, a_ritr const itr) {
	a_u32 pos = itr[0];

	api_check(
			pos <= table_bucket(self)->_hmask &&
			(pos == 0 || !hnode_is_empty(&self->_ptr[pos])), "invalid table iterator.");
}

a_usize alo_hnext(a_henv env, a_isize id, a_ritr itr) {
	api_check_slot(env, 2);

	Value v = api_elem(env, id);
	api_check(v_is_table(v), "not table.");

	GTable* self = v_as_table(v);

	api_check_hitr(self, itr);

	HBucket* bucket = table_bucket(self);

	a_u32 pos = itr[0];

	/* Get next position. */
	HNode* prev = &bucket->_nodes[pos - 1];
	if (is_nil(prev->_link._next)) {
		/* Reach to end. */
		return 0;
	}

	pos = unwrap(prev->_link._next);

	itr[0] = pos; /* Store cursor. */

	HNode* node = &self->_ptr[pos - 1];
	v_set(env, api_incr_stack(env), node->_key);
	v_set(env, api_incr_stack(env), node->_value);

	return 2;
}

a_bool alo_hremove(a_henv env, a_isize id, a_ritr itr) {
	Value v = api_elem(env, id);
	api_check(v_is_table(v), "not table.");

	GTable* self = v_as_table(v);

	api_check_hitr(self, itr);

	a_u32 pos = itr[0];

	api_check(pos != 0, "no element need to remove.");

	HNode* node = &self->_ptr[pos];
	itr[0] = unwrap_unsafe(node->_link._prev);
	table_remove(env, self, node);

	return true;
}

Value ai_table_getis(a_henv env, GTable* self, GStr* key) {
	assume(g_is_istr(key), "not short string.");
	Value const* v = table_find_id(env, self, key->_hash, v_of_obj(key));
	return v != null ? *v : v_of_nil();
}

Value ai_table_get(a_henv env, GTable* self, Value key) {
	a_u32 hash;
	Value const* value = ai_table_ref(env, self, key, &hash);
	return value != null ? *value : v_of_nil();
}

void ai_table_set(a_henv env, GTable* self, Value key, Value value) {
	a_hash hash;
	Value* ref = ai_table_ref(env, self, key, &hash);
	if (ref != null) {
		v_set(env, ref, value);
		ai_gc_barrier_backward_val(env, self, value);
	}
	else {
		ai_table_hint(env, self, 1);
		ai_table_put(env, self, key, hash, value);
	}
}

void ai_table_put(a_henv env, GTable* self, Value key, a_hash hash, Value value) {
	assume(self->_ptr != null && self->_len + 1 <= (self->_hmask + 1) * ALOI_TABLE_LOAD_FACTOR, "need hint before emplace.");
	table_put(env, self, key, hash, value);
	ai_gc_barrier_backward_val(env, self, key);
	ai_gc_barrier_backward_val(env, self, value);
	self->_len += 1;
}

void ai_table_drop(Global* g, GTable* self) {
	if (self->_ptr != null) {
		bucket_free(g, table_bucket(self));
	}
	ai_mem_dealloc(g, self, sizeof(GTable));
}

void ai_table_mark(Global* g, GTable* self) {
	if (self->_ptr != null) {
		HBucket* bucket = table_bucket(self);
		for (a_u32 i = 0; i <= self->_hmask; ++i) {
			HNode *node = &bucket->_nodes[i];
			if (!hnode_is_empty(node)) {
				ai_gc_trace_mark_val(g, node->_key);
				ai_gc_trace_mark_val(g, node->_value);
			}
		}
		ai_gc_trace_work(g, bucket_size(bucket->_hmask + 1));
	}
}

static VTable const table_vtable = {
	._mask = V_MASKED_TAG(T_TABLE),
	._iname = env_type_iname(_table),
	._sname = "table",
	._base_size = sizeof(GTable),
	._elem_size = 0,
	._flags = VTABLE_FLAG_NONE,
	._vfps = (a_vslot[]) {
		vfp_def(drop, ai_table_drop),
		vfp_def(mark, ai_table_mark)
	}
};