/**
 *@file astr.c
 */

#define astr_c_

#include <stdio.h>
#include <string.h>

#include "aenv.h"
#include "amem.h"
#include "agc.h"
#include "astrx.h"

#include "astr.h"

/**
 ** Compute hash code for string, use FNV-1 like algorithm.
 ** Take 4 integers as signature of character sequence and compute its hash code.
 **
 *@param env the runtime environment.
 *@param src the source of data.
 *@param len the data length.
 *@return a_hash the hash code.
 */
a_hash ai_str_hashof(a_hash seed, void const* src, a_usize len) {
    a_hash hash = seed;
    a_byte const* l = src;
    a_usize step = 1 + (len >> 4);
    for (a_byte const* p = src + len - 1; p >= l; p -= step) {
        hash = (hash ^ cast(a_hash, *p)) * 0x1000193;
    }
    return hash;
}

static void grow_istable(a_henv env, IStrTable* istable) {
    a_usize old_size = istable->_hmask + 1;
    a_usize new_size = old_size * 2;

    IStr** table = ai_mem_vgrow(env, istable->_table, old_size, new_size);

    /* Move strings. */
    for (a_usize i = 0; i < old_size; ++i) {
        IStr** p = &table[i];
        IStr** q = &table[i + old_size];
        IStr* s = *p;
        while (s != null) {
            if (s->_body._hash & old_size) {
                *q = s;
                q = &s->_next;
            }
            else {
                *p = s;
                p = &s->_next;
            }
            s = s->_next;
        }
        *p = *q = null;
    }

    istable->_table = table;
    istable->_hmask = new_size - 1;
}

static IStr** istable_slot(IStrTable* istable, a_hash hash) {
    return &istable->_table[hash & istable->_hmask];
}

static void istable_emplace(IStrTable* istable, IStr* self) {
	IStr** slot = istable_slot(istable, self->_body._hash);
	self->_next = *slot;
	*slot = self;
	istable->_len += 1;
}

static void str_new(GStr* self, void const* src, a_usize len, a_hash hash) {
    self->_len = len;
    self->_hash = hash;
    memcpy(self->_data, src, len);
    self->_data[len] = '\0';
}

static IStr* istr_lookup(a_henv env, void const* src, a_usize len, a_hash hash) {
    Global* g = G(env);
    IStrTable* istable = &g->_istable;

    /* Try look string in intern table. */
    for (IStr* str = istable->_table[hash & istable->_hmask]; str != null; str = str->_next) {
        if (str->_body._hash == hash && likely(str->_body._len == len && memcmp(str->_body._data, src, len) == 0)) {
            /* Revive string object if it is dead. */
            if (unlikely(g_is_other(g, upcast(&str->_body)))) {
                str->_body._tnext = white_color(g);
            }
            return str;
        }
    }

    if (unlikely(istable->_len == istable->_hmask)) {
        grow_istable(env, istable);
    }

    /* String not found, create new string. */
    IStr* self = ai_mem_alloc(env, sizeof(IStr) + len);
    str_new(&self->_body, src, len, hash);
    self->_body._meta = &g->_metas._istr;
	istable_emplace(istable, self);
    ai_gc_register_object(env, &self->_body);
    return self;
}

static GStr* hstr_new(a_henv env, void const* src, a_usize len, a_hash hash) {
    GStr* self = ai_mem_alloc(env, sizeof(GStr) + len);
    str_new(self, src, len, hash);
    self->_meta = &G(env)->_metas._hstr;
    ai_gc_register_object(env, self);
    return self;
}

GStr* ai_str_intern(a_henv env, void* blk, char const* src, a_usize len, a_u32 tag) {
    Global* g = G(env);
    IStr* self = cast(IStr*, blk);
    a_hash hash = ai_str_hashof(g->_seed, src, len);
    str_new(&self->_body, src, len, hash);
	strx_id_set(&self->_body, tag);
	self->_body._gnext = null;
	self->_body._meta = &G(env)->_metas._dstr;
	istable_emplace(&g->_istable, self);
	return &self->_body;
}

GStr* ai_str_new(a_henv env, void const* src, a_usize len, a_hash hash) {
    assume(hash == ai_str_hashof(G(env)->_seed, src, len));
    return likely(len <= ALOI_SHTSTR_THRESHOLD) ? 
            &istr_lookup(env, src, len, hash)->_body : 
            hstr_new(env, src, len, hash);
}

GStr* ai_str_create(a_henv env, void const* src, a_usize len) {
    return ai_str_new(env, src, len, ai_str_hashof(G(env)->_seed, src, len));
}

GStr* ai_str_format(a_henv env, char const* fmt, ...) {
    va_list varg;
    va_start(varg, fmt);
    GStr* self = ai_str_formatv(env, fmt, varg);
    va_end(varg);
    return self;
}

GStr* ai_str_formatv(a_henv env, char const* fmt, va_list varg) {
    char buf[ALOI_SHTSTR_THRESHOLD + 1];
    va_list varg2;
    va_copy(varg2, varg);
    int len = vsnprintf(buf, sizeof(buf), fmt, varg);
    if (len <= ALOI_SHTSTR_THRESHOLD) {
        va_end(varg2);
        return &istr_lookup(env, buf, len, ai_str_hashof(G(env)->_seed, buf, len))->_body;
    }
    else {
        GStr* self = ai_mem_alloc(env, sizeof(GStr) + len);
        self->_len = len;
        vsprintf(cast(char*, self->_data), fmt, varg2);
        va_end(varg2);
        self->_hash = ai_str_hashof(G(env)->_seed, self->_data, len);
        self->_meta = &G(env)->_metas._hstr;
        ai_gc_register_object(env, self);
        return self;
    }
}

a_bool ai_str_requals(GStr* self, void const* dat, a_usize len) {
    return self->_len == len && memcmp(self->_data, dat, len) == 0;
}

void ai_str_boost(a_henv env) {
    Global* g = G(env);
    
    IStrTable* istable = &g->_istable;
    istable->_table = ai_mem_vnew(env, IStr*, ALOI_INIT_SHTSTR_TABLE_CAPACITY);
    istable->_hmask = ALOI_INIT_SHTSTR_TABLE_CAPACITY - 1;
    memset(istable->_table, 0, sizeof(IStr*) * ALOI_INIT_SHTSTR_TABLE_CAPACITY);
    
    g->_nomem_error = ai_str_createl(env, "out of memory.");
    ai_gc_fix_object(env, g->_nomem_error);
}

void ai_str_clean(Global* g) {
    IStrTable* istable = &g->_istable;
    assume(istable->_len == 0);
    ai_mem_vdel(g, istable->_table, istable->_hmask + 1);
}

void ai_istr_splash(Global* g, GStr* self) {
    g->_mem_work -= sizeof(IStr) + self->_len;
}

void ai_hstr_splash(Global* g, GStr* self) {
    g->_mem_work -= sizeof(GStr) + self->_len;
}

void ai_istr_destruct(Global* g, GStr* self) {
    IStrTable* istable = &g->_istable;
    IStr* self0 = from_member(IStr, _body, self);
    
    /* Remove string from intern table. */
    IStr** slot = istable_slot(istable, self->_hash);
    loop {
        IStr* str = *slot;
        assume(str != null);
        if (self0 == str) {
            *slot = self0->_next;
            break;
        }
        slot = &str->_next;
    }
    istable->_len -= 1;

    ai_mem_dealloc(g, self0, sizeof(IStr) + self->_len);
}

void ai_hstr_destruct(Global* g, GStr* self) {
    ai_mem_dealloc(g, self, sizeof(GStr) + self->_len);
}
