/**
 *@file adict.h
 */

#define adict_h_

#include "aobj.h"

#ifndef TKey
# error TKey is required.
# define TKey GStr*
#endif

#ifndef Dict
# error Metas and Node required.

typedef struct {
    TKey _key;
    Value _value;
} Node;

/* For type hint. */
typedef struct {
    Node* _ptr;
    a_usize _len;
    a_usize _hmask;
} Metas;

# define node_is_empty(n) (true)
# define node_is_ended(n) (false)
# define node_match(n,k) ((n)->_key == (k))
# define node_init(n) quiet(n)

# define key_hash(k) ((k)->_hash)

#endif

#define ALO_DICT_LOAD_FACTOR 3 / 4

static void at_dict_init(Dict* self) {
    self->_ptr = null;
    self->_hmask = 0;
    self->_len = 0;
}

static void at_dict_deinit(Global* g, Dict* self) {
    if (self->_ptr != null) {
        ai_mem_vdel(g, self->_ptr, self->_hmask + 1);
    }
}

#ifndef at_dict_for

static a_bool at_dict_next_(Dict* self, a_u32* pi, Node** pn) {
    for (a_u32 i = *pi; i <= self->_hmask; ++i) {
        Node* n = &self->_ptr[i];
        if (!node_is_empty(n)) {
            *pi = i;
            *pn = n;
            return true;
        }
    }
    return false;
}


# define at_dict_for(self,n) for (a_u32 _i = 0; at_dict_next_(self, &_i, &(n)); _i += 1)

#endif

#define at_dict_for_hash(self,hash,n) \
    for (a_hash                    \
        _perturb = (hash),         \
        _mask = (self)->_hmask,    \
        n = _perturb;              \
        n &= _mask,                \
        true;                      \
        _perturb >>= 5,            \
        n = n * 5 + 1 + _perturb   \
    )

static a_u32 dict_reserve_inplace(Dict* self, a_hash hash) {
    at_dict_for_hash(self, hash, i) {
        Node* node = &self->_ptr[i];
        if (node_is_empty(node)) {
            return i;
        }
    }
}

#ifndef at_dict_emplace

static void at_dict_emplace(a_henv env, Dict* self, a_u32 index, TKey key, Value val) {
    Node* node = &self->_ptr[index];

    node->_key = key;
    v_set(env, &node->_value, val);
}

#endif

#ifndef at_dict_erase

static void at_dict_erase(unused a_henv env, Dict* self, a_u32 index) {
    Node* node = &self->_ptr[index];
    node_erase(node);
}

#endif

static void at_dict_resize(a_henv env, Dict* self, a_usize old_cap, a_usize new_cap) {
    assume(((new_cap - 1) & new_cap) == 0, "bad capacity.");

    Node* old_ptr = self->_ptr;
    Node* new_ptr = ai_mem_vnew(env, Node, new_cap);

    for (a_u32 i = 0; i < new_cap; ++i) {
        node_init(&new_ptr[i]);
    }

    Dict old_self = *self;

    self->_hmask = new_cap - 1;
    self->_ptr = new_ptr;

    if (old_ptr != null) {
        Node* node;

        at_dict_for(&old_self, node) {
            a_u32 index = dict_reserve_inplace(self, key_hash(node->_key));
            at_dict_emplace(env, self, index, node->_key, node->_value);
        }

        ai_mem_vdel(G(env), old_ptr, old_cap);
    }
}

#ifndef at_dict_grow_amortized

static a_bool at_dict_grow_amortized(a_henv env, Dict* self, a_usize addition) {
    a_usize need;
    try(checked_add_usize(self->_len, addition, &need));

    a_usize old_cap = (self->_hmask + 1) & ~usizec(1);
    a_usize new_cap = ceil_pow2_usize(need);
    if (need > new_cap * ALO_DICT_LOAD_FACTOR) new_cap <<= 1;
    new_cap = max(new_cap, 4);

    at_dict_resize(env, self, old_cap, new_cap);
    return false;
}

#endif

static a_bool at_dict_hint(a_henv env, Dict* self, a_usize addition) {
    if (addition > (self->_hmask + 1) * ALO_DICT_LOAD_FACTOR - self->_len) {
        try(at_dict_grow_amortized(env, self, addition));
    }
    return false;
}

#ifndef at_dict_find

static a_bool at_dict_find(unused a_henv env, Dict* self, TKey key, a_usize* pindex) {
    if (self->_len == 0)
        return false;

    at_dict_for_hash(self, key_hash(key), i) {
        Node* node = &self->_ptr[i];
        if (node_match(node, key)) {
            *pindex = i;
            return true;
        }
        if (node_is_ended(node)) {
            return false;
        }
    }
}

#endif

static Value* at_dict_get(unused a_henv env, Dict* self, TKey key) {
    a_usize id;
    return at_dict_find(env, self, key, &id) ? &self->_ptr[id]._value : null;
}

static a_bool at_dict_set(a_henv env, Dict* self, TKey key, Value val) {
    a_usize id;
    if (at_dict_find(env, self, key, &id)) {
        v_set(env, &self->_ptr[id]._value, val);
        return true;
    }
    return false;
}

static a_bool at_dict_put(a_henv env, Dict* self, TKey key, Value val) {
    try(at_dict_hint(env, self, 1));

    a_u32 index = dict_reserve_inplace(self, key_hash(key));
    at_dict_emplace(env, self, index, key, val);

    self->_len += 1;
    return false;
}

static a_bool at_dict_del(a_henv env, Dict* self, TKey key) {
    a_usize index;
    if (!at_dict_find(env, self, key, &index)) return false;

    at_dict_erase(env, self, index);

    self->_len -= 1;
    return true;
}

#undef Dict
#undef TKey

#undef node_is_empty
#undef node_is_ended
#undef node_match
#undef node_init

#undef key_hash
