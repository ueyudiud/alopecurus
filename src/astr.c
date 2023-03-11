/**
 *@file astr.c
 */

#define astr_c_
#define ALO_LIB

#include <stdio.h>
#include <string.h>

#include "aop.h"
#include "aenv.h"
#include "amem.h"
#include "agc.h"
#include "astrx.h"

#include "astr.h"

static VTable const istr_vtable;
static VTable const hstr_vtable;

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
	if (len > 0) {
		a_byte const* l = src;
		a_usize step = 1 + (len >> 4);
		assume(src != null);
		for (a_byte const* p = src + len - 1; p >= l; p -= step) {
			hash = (hash ^ cast(a_hash, *p)) * 0x1000193;
		}
	}
    return hash;
}

static void cache_grow(a_henv env, IStrCache* cache) {
    a_usize old_size = cache->_hmask + 1;
    a_usize new_size = old_size * 2;

    IStr** table = ai_mem_vgrow(env, cache->_table, old_size, new_size);

    /* Move strings. */
    for (a_usize i = 0; i < old_size; ++i) {
        IStr** p = &table[i];
        IStr** q = &table[i + old_size];
        IStr* s = *p;
        while (s != null) {
            if (s->_body._hash & old_size) {
                *q = s;
                q = &s->_cache_next;
            }
            else {
                *p = s;
                p = &s->_cache_next;
            }
            s = s->_cache_next;
        }
        *p = *q = null;
    }

	cache->_table = table;
	cache->_hmask = new_size - 1;
}

static IStr** cache_head(IStrCache* cache, a_hash hash) {
    return &cache->_table[hash & cache->_hmask];
}

static void cache_emplace(IStrCache* cache, IStr* self) {
	IStr** slot = cache_head(cache, self->_body._hash);
	self->_cache_next = *slot;
	*slot = self;
	cache->_len += 1;
}

static GStr* hstr_alloc(a_henv env, a_usize len) {
	GStr* self = ai_mem_alloc(env, hstr_size(len));
	self->_vtable = &hstr_vtable;
	self->_len = len;
	self->_data[len] = '\0';
	return self;
}

static void istr_init(IStr* self, void const* src, a_usize len, a_hash hash) {
	self->_body._vtable = &istr_vtable;
    self->_body._len = len;
    self->_body._hash = hash;
    memcpy(self->_body._data, src, len);
    self->_body._data[len] = '\0';
}

static GStr* istr_get2(a_henv env, void const* src, a_usize len, a_hash hash) {
	assume(len <= ISTR_MAX_LEN);

    Global* g = G(env);
    IStrCache* istable = &g->_istr_cache;

    /* Try lookup string in intern table. */
    for (IStr* str = istable->_table[hash & istable->_hmask]; str != null; str = str->_cache_next) {
        if (str->_body._hash == hash && likely(str->_body._len == len && memcmp(str->_body._data, src, len) == 0)) {
            /* Revive string object if it is dead. */
            if (unlikely(g_has_other_color(g, gobj_cast(&str->_body)))) {
                str->_body._tnext = white_color(g);
            }
            return &str->_body;
        }
    }

    if (unlikely(istable->_len == istable->_hmask)) {
		cache_grow(env, istable);
    }

	/* String not found, create new string. */
	IStr* self = ai_mem_alloc(env, istr_size(len));
	istr_init(self, src, len, hash);
	cache_emplace(istable, self);
    ai_gc_register_object(env, &self->_body);
    return &self->_body;
}

static GStr* hstr_new(a_henv env, void const* src, a_usize len, a_hash hash) {
    GStr* self = hstr_alloc(env, len);
	memcpy(self->_data, src, len);
	self->_hash = hash;
    ai_gc_register_object(env, self);
    return self;
}

GStr* ai_str_intern(a_henv env, void* blk, char const* src, a_usize len, a_u32 tag) {
    Global* g = G(env);
    IStr* self = cast(IStr*, blk);
    a_hash hash = ai_str_hashof(g->_seed, src, len);

	self->_body._gnext = null;
	istr_init(self, src, len, hash);
	strx_id_set(&self->_body, tag);

	cache_emplace(&g->_istr_cache, self);
	return &self->_body;
}

static GStr* istr_get(a_henv env, void const* src, a_usize len) {
	a_hash hash = ai_str_hashof(G(env)->_seed, src, len);
	return istr_get2(env, src, len, hash);
}

GStr* ai_str_new(a_henv env, void const* src, a_usize len) {
    a_hash hash = ai_str_hashof(G(env)->_seed, src, len);
	return likely(len <= ISTR_MAX_LEN) ? istr_get2(env, src, len, hash) : hstr_new(env, src, len, hash);
}

a_msg ai_str_load(a_henv env, ZIn* in, a_usize len, GStr** pstr) {
	if (len <= ISTR_MAX_LEN) {
		char buf[ISTR_MAX_LEN + 1];
		check(ai_io_iget(in, buf, len));
		*pstr = istr_get(env, buf, len);
		return ALO_SOK;
	}
	else {
		GStr* self = ai_mem_alloc(env, hstr_size(len));
		a_msg msg = ai_io_iget(in, self->_data, len);
		if (unlikely(msg != ALO_SOK)) {
			ai_mem_dealloc(G(env), self, hstr_size(len));
			return msg;
		}
		self->_vtable = &hstr_vtable;
		self->_len = len;
		self->_hash = ai_str_hashof(G(env)->_seed, self->_data, len);
		ai_gc_register_object(env, self);
		*pstr = self;
		return ALO_SOK;
	}
}

GStr* ai_str_format(a_henv env, char const* fmt, va_list varg) {
    char buf[ISTR_MAX_LEN + 1];

    va_list varg2;
    va_copy(varg2, varg);
    a_usize len = cast(a_isize, vsnprintf(buf, sizeof(buf), fmt, varg2));
	va_end(varg2);

    if (len <= ISTR_MAX_LEN) {
        return istr_get(env, buf, len);
    }
    else {
        GStr* self = ai_mem_alloc(env, sizeof(GStr) + len);
		self->_vtable = &hstr_vtable;
        self->_len = len;
        vsprintf(cast(char*, self->_data), fmt, varg);
        self->_hash = ai_str_hashof(G(env)->_seed, self->_data, len);
        ai_gc_register_object(env, self);
        return self;
    }
}

a_bool ai_str_requals(GStr* self, void const* dat, a_usize len) {
    return self->_len == len && memcmp(self->_data, dat, len) == 0;
}

a_bool ai_str_equals(GStr* self, GStr* other) {
    return self == other || (self->_hash == other->_hash && ai_str_requals(self, self->_data, self->_len));
}

void ai_str_boost(a_henv env) {
    Global* g = G(env);
    
    IStrCache* cache = &g->_istr_cache;
	cache->_table = ai_mem_vnew(env, IStr*, ALOI_INIT_SHTSTR_TABLE_CAPACITY);
	cache->_hmask = ALOI_INIT_SHTSTR_TABLE_CAPACITY - 1;
    memset(cache->_table, 0, sizeof(IStr*) * ALOI_INIT_SHTSTR_TABLE_CAPACITY);
    
    g->_nomem_error = ai_str_newl(env, "out of memory.");
    ai_gc_fix_object(env, g->_nomem_error);
}

void ai_str_clean(Global* g) {
    IStrCache* cache = &g->_istr_cache;
    assume(cache->_len == STRX__END - 1);
    ai_mem_vdel(g, cache->_table, cache->_hmask + 1);
}

static void cache_remove(IStrCache* cache, IStr* str) {
	/* Remove string from intern table. */
	IStr** slot = cache_head(cache, str->_body._hash);
	loop {
		IStr* str1 = *slot;
		assume(str1 != null);
		if (str == str1) {
			*slot = str->_cache_next;
			break;
		}
		slot = &str->_cache_next;
	}
	cache->_len -= 1;
}

static void istr_drop(Global* g, a_hobj raw_self) {
	IStr* self = g_cast(IStr, raw_self);
	cache_remove(&g->_istr_cache, self);
	ai_mem_dealloc(g, self, istr_size(self->_body._len));
}

static void hstr_drop(Global* g, GStr* self) {
    ai_mem_dealloc(g, self, hstr_size(self->_len));
}

static void str_tostr(a_henv env, GStr* self, GBuf* buf) {
	ai_buf_putls(env, buf, self->_data, self->_len);
}

static VTable const istr_vtable = {
	._val_mask = V_MASKED_TAG(T_ISTR),
	._api_tag = ALO_TSTR,
	._repr_id = REPR_STR,
	._flags = VTABLE_FLAG_PLAIN_LEN | VTABLE_FLAG_PLAIN_MARK,
	._name = "str",
	._mark = null,
	._drop = istr_drop,
	._tostr = fpcast(a_fp_tostr, str_tostr)
};

static VTable const hstr_vtable = {
	._val_mask = V_MASKED_TAG(T_HSTR),
	._api_tag = ALO_TSTR,
	._repr_id = REPR_STR,
	._flags = VTABLE_FLAG_PLAIN_MARK,
	._name = "str",
	._mark = null,
	._drop = fpcast(a_fp_drop, hstr_drop),
	._tostr = fpcast(a_fp_tostr, str_tostr)
};