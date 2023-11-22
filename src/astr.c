/**
 *@file astr.c
 */

#define astr_c_
#define ALO_LIB

#include <stdio.h>
#include <string.h>

#include "aop.h"
#include "abuf.h"
#include "aenv.h"
#include "amem.h"
#include "agc.h"

#include "astr.h"

#ifndef ALOI_INIT_STR_CACHE_CAPACITY
# define ALOI_INIT_STR_CACHE_CAPACITY 64
#endif

static VTable const str_vtable;

/**
 ** Compute hash code for string, use FNV-1 like algorithm.
 ** Take 4 integers as signature of character sequence and compute its hash code.
 **
 *@param env the runtime environment, seed is providing from here.
 *@param src the source of data.
 *@param len the data length.
 *@return the hash code.
 */
a_hash ai_str_hashof(a_henv env, char const* src, a_usize len) {
    a_hash hash = G(env)->_seed;
	if (len > 0) {
		char const* l = src;
		a_usize step = 1 + (len >> 4);
		assume(src != null);
		for (char const* p = src + len - 1; p >= l; p -= step) {
			hash = (hash ^ cast(a_hash, *p)) * 0x1000193;
		}
	}
    return hash;
}

static void cache_grow(a_henv env, StrCache* cache) {
    a_usize old_cap = cache->_hmask + 1;
    a_usize new_cap = old_cap * 2;

    GStr** table = ai_mem_vgrow(env, cache->_table, old_cap, new_cap);

    /* Move strings. */
    for (a_usize i = 0; i < old_cap; ++i) {
        GStr** p = &table[i];
        GStr** q = &table[i + old_cap];
        GStr* s = *p;
        while (s != null) {
            if (s->_hash & old_cap) {
                *q = s;
                q = &s->_snext;
            }
            else {
                *p = s;
                p = &s->_snext;
            }
            s = s->_snext;
        }
        *p = *q = null;
    }

	cache->_table = table;
	cache->_hmask = new_cap - 1;
}

static void cache_hint(a_henv env, StrCache* cache) {
    if (unlikely(cache->_len == cache->_hmask)) {
		cache_grow(env, cache);
    }
}

static GStr** cache_head(StrCache* cache, a_hash hash) {
    return &cache->_table[hash & cache->_hmask];
}

static void cache_emplace_in_place(StrCache* cache, GStr* self) {
	GStr** slot = cache_head(cache, self->_hash);
	self->_snext = *slot;
	*slot = self;
	cache->_len += 1;
}

static GStr* str_alloc(a_henv env, a_usize len) {
	return ai_mem_alloc(env, str_size(len));
}

static void str_init(GStr* self, char const* src, a_usize len, a_hash hash) {
	self->_vptr = &str_vtable;
    self->_len = len;
    self->_hash = hash;
    memcpy(self->_ptr, src, sizeof(char) * len);
    self->_ptr[len] = '\0';
}

static GStr* str_get_or_null_with_hash(a_henv env, char const* src, a_usize len, a_hash hash) {
    Global* gbl = G(env);
    StrCache* cache = &gbl->_str_cache;

    /* Try lookup string in intern table. */
    for (GStr* str = cache->_table[hash & cache->_hmask]; str != null; str = str->_snext) {
        if (str->_hash == hash && likely(ai_str_requals(str, src, len))) {
            /* Revive string object if it is dead. */
            if (unlikely(g_has_other_color(gbl, str))) {
                g_set_white(gbl, str);
            }
            return str;
        }
    }

	return null;
}

GStr* ai_str_new_with_hash(a_henv env, char const* src, a_usize len, a_hash hash) {
    /* Force create new string into string cache. */
    StrCache* cache = &G(env)->_str_cache;
    cache_hint(env, cache);

    GStr* self = str_alloc(env, len);
    str_init(self, src, len, hash);

    cache_emplace_in_place(cache, self);
    ai_gc_register_object(env, self);
    return self;
}

static GStr* str_get_and_drop_buff_or_put(a_henv env, GStr* buff, a_usize len) {
	a_hash hash = ai_str_hashof(env, buff->_ptr, len);
	GStr* self = str_get_or_null_with_hash(env, buff->_ptr, len, hash);
	
	if (self != null) {
		ai_mem_dealloc(G(env), buff, str_size(len));
		return self;
	}

	self = buff;
	/* Complete all fields. */
	self->_vptr = &str_vtable;
	self->_len = len;
	self->_hash = hash;
	self->_ptr[len] = '\0';
	/* Covert string buffer into string. */
	StrCache* cache = &G(env)->_str_cache;
	cache_hint(env, cache);
	cache_emplace_in_place(cache, self);
	ai_gc_register_object(env, self);

	return self;
}

GStr* ai_str_get_or_null_with_hash(a_henv env, char const* src, a_usize len, a_hash hash) {
    return str_get_or_null_with_hash(env, src, len, hash);
}

GStr* ai_str_get_or_new_with_hash(a_henv env, char const* src, a_usize len, a_hash hash) {
    return ai_str_get_or_null_with_hash(env, src, len, hash) ?: ai_str_new_with_hash(env, src, len, hash);
}

GStr* ai_str_get_or_new(a_henv env, char const* src, a_usize len) {
    a_hash hash = ai_str_hashof(env, src, len);
    return ai_str_get_or_new_with_hash(env, src, len, hash);
}

#define MAX_STACK_BUFFER_SIZE (256-1)

a_msg ai_str_load(a_henv env, a_sbfun fun, a_usize len, void* ctx, GStr** pstr) {
	if (likely(len < MAX_STACK_BUFFER_SIZE)) {
        /* Allocate buffer on stack. */
        char buff[MAX_STACK_BUFFER_SIZE + 1];
		try ((*fun)(ctx, buff, len));
		*pstr = ai_str_get_or_new(env, buff, len);
		return ALO_SOK;
	}
	else {
        /* Allocate buffer on heap. */
		GStr* buff = str_alloc(env, len);
        catch ((*fun)(ctx, buff->_ptr, len), msg) {
            ai_mem_dealloc(G(env), buff, str_size(len));
            return msg;
        }
		
		*pstr = str_get_and_drop_buff_or_put(env, buff, len);
		return ALO_SOK;
	}
}

GStr* ai_str_format(a_henv env, char const* fmt, va_list varg) {
    char buf[MAX_STACK_BUFFER_SIZE + 1];

    va_list varg2;
    va_copy(varg2, varg);

    int len = vsnprintf(buf, sizeof(buf), fmt, varg2);
    assume(len >= 0, "catch format error.");
	
	va_end(varg2);

    if (len <= MAX_STACK_BUFFER_SIZE) {
        return ai_str_get_or_new(env, buf, len);
    }
    else {
        GStr* buff = str_alloc(env, len);
		
		vsprintf(buff->_ptr, fmt, varg);

		va_end(varg);
        
		return str_get_and_drop_buff_or_put(env, buff, len);
    }
}

a_bool ai_str_requals(GStr* self, void const* dat, a_usize len) {
    return self->_len == len && memcmp(self->_ptr, dat, len) == 0;
}

static void str_mark(Global* gbl, GStr* self) {
    ai_gc_trace_work(gbl, str_size(self->_len));
}

static void cache_remove(StrCache* cache, GStr* str) {
    /* Remove string from intern table. */
    GStr** slot = cache_head(cache, str->_hash);
    loop {
        GStr* str1 = *slot;
        assume(str1 != null);
        if (str == str1) {
            *slot = str->_snext;
            break;
        }
        slot = &str1->_snext;
    }
    cache->_len -= 1;
}

static void str_drop(Global* gbl, GStr* self) {
    cache_remove(&gbl->_str_cache, self);
    ai_mem_dealloc(gbl, self, str_size(self->_len));
}

void ai_str_boost1(a_henv env, void* block) {
    Global* gbl = G(env);

    run {
        /* Initialize builtin string first, but not put them into the cache. */

        static a_u8 const l_str_len[STR__COUNT] = {
            0, /* Intern empty string. */
#define STRDEF(n) sizeof(#n)-1,
# include "asym/kw.h"
# include "asym/tm.h"
# include "asym/pt.h"
#undef STRDEF
        };

        char const* src = ai_str_interns;

        for (a_u32 i = 0; i < STR__COUNT; ++i) {
            a_u32 len = l_str_len[i];

            GStr* self = block;
            a_hash hash = ai_str_hashof(env, src, len);

            g_set_gray(self);

            str_init(self, src, len, hash);
            str_id_set(self, i);

            gbl->_names[i] = self;
            block += str_size(len);
            src += len + 1;
        }
    }
}

void ai_str_boost2(a_henv env) {
    Global* gbl = G(env);

    StrCache* cache = &gbl->_str_cache;

    run {
        cache->_table = ai_mem_vnew(env, GStr*, ALOI_INIT_STR_CACHE_CAPACITY);
        cache->_hmask = ALOI_INIT_STR_CACHE_CAPACITY - 1;
        memclr(cache->_table, sizeof(GStr*) * ALOI_INIT_STR_CACHE_CAPACITY);
    }

    run {
        for (a_u32 i = 0; i < STR__COUNT; ++i) {
            cache_emplace_in_place(cache, gbl->_names[i]);
        }
    }

    run {
        gbl->_nomem_error = ai_str_from_ntstr(env, "out of memory.");
        ai_gc_fix_object(env, gbl->_nomem_error);
    }
}

void ai_str_clean(Global* gbl) {
    StrCache* cache = &gbl->_str_cache;
    assume(cache->_len == STR__COUNT, "string size not matched.");
    ai_mem_vdel(gbl, cache->_table, cache->_hmask + 1);
}

static VTable const str_vtable = {
    ._stencil = V_STENCIL(T_STR),
    ._tag = ALO_TSTR,
    ._flags = VTABLE_FLAG_GREEDY_MARK,
    ._type_ref = g_type_ref(ALO_TSTR),
    ._slots = {
        [vfp_slot(drop)] = str_drop,
        [vfp_slot(mark)] = str_mark
    }
};

char const ai_str_interns[] = {
#define STRDEF(n) "\0"#n
#define STRDEF2(n,r) "\0"r
# include "asym/kw.h"
# include "asym/tm.h"
# include "asym/pt.h"
# include "asym/op.h"
#undef STRDEF
#undef STRDEF2
};

static_assert(sizeof(ai_str_interns) == STR__TOTAL_LEN);
