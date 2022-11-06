/**
 *@file atable.c
 */

#define atable_c_

#include <math.h>
#include <string.h>

#include "astr.h"
#include "amem.h"
#include "agc.h"

#include "atable.h"

#ifndef ALOI_TABLE_LOAD_FACTOR
# define ALOI_TABLE_LOAD_FACTOR 0.75
#endif

#define NO_STR_NODE i32c(0)

GTable* ai_table_new(a_henv env) {
    GTable* self = ai_mem_alloc(env, sizeof(GTable));
    self->_len = 0;
    self->_hmask = 0;
    self->_data = null;
    self->_lhead = self->_ltail = nil;
    self->_hfree = nil;
    self->_meta = &G(env)->_metas._table;
    return self;
}

static a_bool tnode_is_hhead(GTable* table, TNode* node, a_hash hash) {
    return ((node->_hash ^ hash) & table->_hmask) == 0;
}

static void tnode_emplace_entry(a_henv env, TNode* node, Value key, a_hash hash, Value value) {
    v_set(G(env), &node->_key, key);
    v_set(G(env), &node->_value, value);
    node->_hash = hash;
}

/**
 ** Move entry and linked list part data, the hash part data will be ignored.
 **
 *@param env the runtime environment.
 *@param self the table.
 *@param noded the destination node.
 *@param nodes the source node.
 */
static void tnode_move(a_henv env, TNode* noded, TNode* nodes) {
    tnode_emplace_entry(env, noded, nodes->_key, nodes->_hash, nodes->_value);
	noded->_lnext = nodes->_lnext;
	noded->_lprev = nodes->_lprev;
}

static a_u32 table_pop_free(GTable* self) {
    a_u32 index = unwrap(self->_hfree);
	TNode* node = &self->_data[index];
	self->_hfree = node->_lnext;
	return index;
}

/**
 ** Link node at end of linked list of table.
 **
 *@param env the runtime environment.
 *@param self the table.
 *@param node the node to be linked.
 */
static void table_link_tail(GTable* self, a_u32 index, TNode* node) {
	assume(&self->_data[index] == node);
    if (self->_len > 0) {
        TNode* nodet = &self->_data[unwrap(self->_ltail)];
		node->_lprev = self->_ltail;
        nodet->_lnext = wrap(index);
    }
    else {
        node->_lprev = nil;
        self->_lhead = wrap(index);
    }
    node->_lnext = nil;
    self->_ltail = wrap(index);
}

static void table_emplace(a_henv env, GTable* self, Value key, a_hash hash, Value value) {
	a_u32 indexh = hash & self->_hmask;
    TNode* nodeh = &self->_data[indexh];
    if (tnode_is_hhead(self, nodeh, hash)) {
        /* Insert into hash list. */
		a_u32 indext = table_pop_free(self);
        TNode* nodet = &self->_data[indext];
        /*                       v
         * h -> n -> ... => h -> t -> n -> ...
         *           v
         * h => h -> t
         */
		nodeh->_hnext = wrap(indext);
		nodet->_hnext = nodeh->_hnext;
		table_link_tail(self, indext, nodet);
    }
    else if (v_is_dead_key(&nodeh->_key)) {
        /* Emplace locally. */
        /*        v
         * nil => h
         */
        tnode_emplace_entry(env, nodeh, key, hash, value);
        nodeh->_hnext = nil;
		table_link_tail(self, indexh, nodeh);
    }
    else {
        /* Move emplaced entry to other node. */
        /*                    v
         * ... -> h -> ... => h; ... -> f -> ...
         */
		a_u32 indexf = table_pop_free(self);
        TNode* nodef = &self->_data[indexf];
        tnode_move(env, nodef, nodeh);
        tnode_emplace_entry(env, nodef, key, hash, value);
        nodeh->_hnext = nil;
		table_link_tail(self, indexh, nodeh);
    }
}

void ai_table_hint(a_henv env, GTable* self, a_usize len) {
    a_usize old_cap = self->_hmask + 1;
    a_usize expect = self->_len + len;
    if (expect > cast(a_usize, old_cap * ALOI_TABLE_LOAD_FACTOR)) {
        a_usize new_hmask = ceil_pow2m1_usize(cast(a_usize, ceil(expect / ALOI_TABLE_LOAD_FACTOR)));
        a_usize new_cap = new_hmask + 1;
        assume(expect <= new_cap);
        
        TNode* old_nodes = self->_data;
        TNode* new_nodes = ai_mem_vnew(env, TNode, new_cap);
        self->_data = new_nodes;
        self->_hmask = new_hmask;
        self->_hfree = nil;

        for (a_u32 i = 1; i <= new_cap; ++i) {
			TNode* node = &new_nodes[i];
            v_setx(&node->_key, v_of_dead_key());
			node->_lprev = wrap(i - 1);
			node->_lnext = wrap(i + 1);
        }
		new_nodes[new_cap - 1]._lnext = nil;

		TNode* node;
		for (a_x32 i = self->_lhead; !is_nil(i); i = node->_lnext) {
			node = &self->_data[unwrap(i)];
			table_emplace(env, self, node->_key, node->_hash, node->_value);
		}

        ai_mem_vdel(G(env), old_nodes + 1, old_cap);
    }
}

typedef a_bool (*Pred)(a_henv env, void const* ctx, Value const* v);

static a_bool p_identity_equals(unused a_henv env, void const* ctx, Value const* v) {
    return cast(Value const*, ctx)->_u == v->_u;
}

static a_bool p_string_equals(unused a_henv env, void const* ctx, Value const* v) {
    a_lstr const* key = cast(a_lstr const*, ctx);
    if (v_is_str(v)) {
        GStr* str = v_as_str(G(env), v);
        return key->_len == str->_len && memcmp(key->_ptr, str->_data, key->_len) == 0;
    } 
    return false;
}

static TNode* hfindro(a_henv env, GTable* self, a_hash hash, Pred pred, void const* ctx) {
    a_u32 index = hash & self->_hmask;
    TNode* node = &self->_data[index];
    if (!v_is_dead_key(&node->_key) && tnode_is_hhead(self, node, hash)) {
        loop {
            if (node->_hash == hash && (*pred)(env, ctx, &node->_key))
                return node;
            if (is_nil(node->_hnext))
				break;
            node = &self->_data[unwrap(node->_hnext)];
        }
    }
    return null;
}

Value const* ai_table_geti(a_henv env, GTable* self, a_int key) {
    Value v = v_of_int(key);
    TNode* node = hfindro(env, self, v_int_hash(key), p_identity_equals, &v);
    return node != null ? &node->_value : null;
}

Value const* ai_table_gets(a_henv env, GTable* self, a_lstr const* key) {
    TNode* node = hfindro(env, self, ai_str_hashof(G(env)->_seed, key->_ptr, key->_len), p_string_equals, key);
    return node != null ? &node->_value : null;
}

void ai_table_splash(Global* g, GTable* self) {
    a_usize len = self->_hmask + 1;
    for (a_usize i = 0; i < len; ++i) {
        TNode* node = &self->_data[i];
        if (!v_is_dead_key(&node->_key)) {
            ai_gc_trace_markv(g, &node->_key);
            ai_gc_trace_markv(g, &node->_value);
        }
    }
    g->_mem_work -= sizeof(GTable) + sizeof(TNode) * len;
}

void ai_table_destruct(Global* g, GTable* self) {
	if (self->_data != null) {
		ai_mem_vdel(g, self->_data - 1, self->_hmask + 1);
	}
    ai_mem_dealloc(g, self, sizeof(GTable));
}