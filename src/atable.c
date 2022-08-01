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

#define NONODE i32c(0)

GTable* ai_table_new(a_henv env) {
    GTable* self = ai_mem_alloc(env, sizeof(GTable));
    self->_len = 0;
    self->_hmask = 0;
    self->_data = null;
    self->_lhead = self->_ltail = 0;
    self->_hfree = 0;
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
 *@param noded the destination node.
 *@param nodes the source node.
 */
static void tnode_move(a_henv env, TNode* noded, TNode* nodes) {
    tnode_emplace_entry(env, noded, nodes->_key, nodes->_hash, nodes->_value);
    a_isize diff = noded - nodes;
    a_isize lprev = nodes->_lprev;
    if (lprev != NONODE) {
        TNode* nodep = nodes + lprev;
        noded->_lprev = nodes->_lprev + diff;
        nodep->_lnext -= diff;
    }
    else {
        noded->_lprev = NONODE;
    }
    a_isize lnext = nodes->_lnext;
    if (lnext != NONODE) {
        TNode* noden = nodes + lnext;
        noded->_lnext = nodes->_lnext + diff;
        noden->_lnext -= diff;
    }
    else {
        noded->_lnext = NONODE;
    }
}

static TNode* table_next_free(GTable* self) {
    a_usize index = self->_hfree;
    loop {
        assume(index <= self->_hmask);
        TNode* node = &self->_data[index ++];
        if (v_is_dead_key(&node->_key)) {
            self->_hfree = index;
            return node;
        }
    }
}

/**
 ** Link node at end of linked list of table.
 **
 *@param env the runtime environment.
 *@param self the table.
 *@param node the node to be linked.
 */
static void table_link_after(GTable* self, TNode* node) {
    a_usize index = node - self->_data;
    if (self->_len > 0) {
        TNode* nodet = &self->_data[self->_ltail];
        nodet->_lnext = node - nodet;
        node->_lprev = nodet - node;
    }
    else {
        node->_lprev = NONODE;
        self->_lhead = index;
    }
    node->_lnext = NONODE;
    self->_ltail = index;
}

static void table_emplace(a_henv env, GTable* self, Value key, a_hash hash, Value value) {
    TNode* nodeh = &self->_data[hash & self->_hmask];
    if (tnode_is_hhead(self, nodeh, hash)) {
        /* Insert into hash list. */
        TNode* nodet = table_next_free(self);
        a_isize hnext = nodeh->_hnext;
        /*                       v
         * h -> n -> ... => h -> t -> n -> ...
         *           v
         * h => h -> t
         */
        if (hnext != NONODE) {
            TNode* noden = nodeh + hnext;
            nodet->_hnext = noden - nodet;
        }
        else {
            nodet->_hnext = NONODE;
        }
        nodeh->_hnext = nodet - nodeh;
        table_link_after(self, nodet);
    }
    else if (v_is_dead_key(&nodeh->_key)) {
        /* Emplace locally. */
        /*        v
         * nil => h
         */
        tnode_emplace_entry(env, nodeh, key, hash, value);
        nodeh->_hnext = NONODE;
        table_link_after(self, nodeh);
    }
    else {
        /* Move emplaced entry to other node. */
        /*                    v
         * ... -> h -> ... => h; ... -> f -> ...
         */
        TNode* nodef = table_next_free(self);
        tnode_move(env, nodef, nodeh);
        tnode_emplace_entry(env, nodef, key, hash, value);
        nodeh->_hnext = NONODE;
        table_link_after(self, nodeh);
    }
}

void ai_table_hint(a_henv env, GTable* self, a_usize len) {
    a_usize old_cap = self->_hmask + 1;
    a_usize expect = self->_len + len;
    if (expect > cast(a_usize, old_cap * ALOI_TABLE_LOAD_FACTOR)) {
        a_usize new_hmask = ceil_pow2m1_usize(cast(a_usize, ceil(expect / ALOI_TABLE_LOAD_FACTOR)));
        a_usize new_cap = new_hmask + 1;
        assume(expect <= new_cap);
        
        TNode* old_nodes = ai_mem_vnew(env, TNode, new_cap);
        TNode* new_nodes = self->_data;
        self->_data = new_nodes;
        self->_hmask = new_hmask;
        self->_hfree = 0;

        for (a_usize i = 0; i < new_cap; ++i) {
            v_setx(&new_nodes[i]._key, v_of_dead_key());
        }

        if (self->_len > 0) {
            TNode* node = &self->_data[self->_lhead];
            loop {
                table_emplace(env, self, node->_key, node->_hash, node->_value);
                a_i32 diff = node->_lnext;
                if (diff == NONODE) break;
                node += cast(a_isize, diff);
            }
        }

        ai_mem_vdel(G(env), old_nodes, old_cap);
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
            if (node->_hnext == 0) break;
            node += node->_hnext;
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
    ai_mem_vdel(g, self->_data, self->_hmask + 1);
    ai_mem_dealloc(g, self, sizeof(GTable));
}