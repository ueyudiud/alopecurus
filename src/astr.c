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
#include "aerr.h"

#include "astr.h"

#ifndef ALOI_INIT_STR_CACHE_CAPACITY
# define ALOI_INIT_STR_CACHE_CAPACITY 64
#endif

#if ALO_M64
# define MAX_STR_CACHE_CAPACITY (((a_usize) UINT32_MAX) + 1)
#else
# define MAX_STR_CACHE_CAPACITY ((usizec(1) << 31) / sizeof(GStr*))
#endif

static Impl const str_impl;

/**
 ** Compute hash code for string, use FNV-1 like algorithm.
 ** Take 4 integers as signature of character sequence and compute its hash code.
 **
 *@param env the runtime environment, seed is providing from here.
 *@param src the source of data.
 *@param len the data length.
 *@return the hash code.
 */
a_hash ai_str_hashof(a_henv env, a_lstr str) {
    a_hash hash = G(env)->seed;
	if (str.len > 0) {
		char const* begin = str.ptr;
		a_usize step = 1 + (str.len >> 4);
		assume(str.ptr != null);
		for (char const* p = str.ptr + str.len - 1; p >= begin; p -= step) {
			hash = (hash ^ cast(a_hash, *p)) * 0x1000193;
		}
	}
    return hash;
}

static void cache_grow(a_henv env, StrCache* cache) {
    a_usize old_cap = cast(a_usize, cache->hmask) + 1;
    a_usize new_cap = old_cap * 2;

    if (unlikely(new_cap > MAX_STR_CACHE_CAPACITY)) {
        ai_err_raisef(env, ALO_EINVAL, "too many strings.");
    }

    GStr** new_ptr = ai_mem_vgrow(env, cache->ptr, old_cap, new_cap);

    /* Move strings. */
    for (a_usize i = 0; i < old_cap; ++i) {
        GStr** p = &new_ptr[i];
        GStr** q = &new_ptr[i + old_cap];
        /* Split string in same slot in old array into two slots by hash. */
        GStr* s = *p;
        while (s != null) {
            if (s->hash & old_cap) {
                *q = s;
                q = &s->snext;
            }
            else {
                *p = s;
                p = &s->snext;
            }
            s = s->snext;
        }
        *p = *q = null;
    }

	cache->ptr = new_ptr;
	cache->hmask = new_cap - 1;
}

static void cache_shrink(Global* gbl, StrCache* cache) {
    a_usize old_cap = cast(a_usize, cache->hmask) + 1;
    a_usize new_cap = old_cap / 2;

    GStr** old_ptr = cache->ptr;

    /* Move strings. */
    for (a_usize i = 0; i < new_cap; ++i) {
        GStr** p = &old_ptr[i];
        GStr** q = &old_ptr[i + new_cap];
        /* Merge string from two slots into one slot. */
        GStr* s;
        while ((s = *p) != null) {
            p = &s->snext;
        }
        *p = *q;
    }

    cache->ptr = ai_mem_vshrink(gbl, old_ptr, old_cap, new_cap);
    cache->hmask = new_cap - 1;
}

static void cache_hint(a_henv env, StrCache* cache) {
    if (unlikely(cache->len == cache->hmask)) {
		cache_grow(env, cache);
    }
}

static GStr** cache_head(StrCache* cache, a_hash hash) {
    return &cache->ptr[hash & cache->hmask];
}

static void cache_emplace_in_place(StrCache* cache, GStr* self) {
	GStr** slot = cache_head(cache, self->hash);
	self->snext = *slot;
	*slot = self;
	cache->len += 1;
}

static GStr* str_alloc(a_henv env, a_usize len) {
	return ai_mem_alloc(env, str_size(len));
}

static void str_init(GStr* self, a_lstr src, a_hash hash) {
	self->impl = &str_impl;
    self->len = src.len;
    self->hash = hash;
    memcpy(self->ptr, src.ptr, sizeof(char) * src.len);
    self->ptr[src.len] = '\0';
}

static GStr* str_get_or_null_with_hash(a_henv env, a_lstr src, a_hash hash) {
    Global* gbl = G(env);
    StrCache* cache = &gbl->str_cache;

    /* Try lookup string in intern table. */
    for (GStr* str = cache->ptr[hash & cache->hmask]; str != null; str = str->snext) {
        if (str->hash == hash && str->len == src.len && memcmp(str->ptr, src.ptr, src.len) == 0) {
            /* Revive string object if it is dead. */
            if (unlikely(g_has_other_color(gbl, str))) {
                g_set_white(gbl, str);
            }
            return str;
        }
    }

	return null;
}

GStr* ai_str_new_with_hash(a_henv env, a_lstr src, a_hash hash) {
    /* Force create new string into string cache. */
    StrCache* cache = &G(env)->str_cache;
    cache_hint(env, cache);

    GStr* self = str_alloc(env, src.len);
    str_init(self, src, hash);

    cache_emplace_in_place(cache, self);
    ai_gc_register_normal(env, self);
    return self;
}

static GStr* str_get_and_drop_buff_or_put(a_henv env, GStr* buff, a_usize len) {
    a_lstr src = { buff->ptr, len };
	a_hash hash = ai_str_hashof(env, src);
	GStr* self = str_get_or_null_with_hash(env, src, hash);
	
	if (self != null) {
		ai_mem_dealloc(G(env), buff, str_size(len));
		return self;
	}

	self = buff;
	/* Complete all fields. */
	self->impl = &str_impl;
	self->len = len;
	self->hash = hash;
	self->ptr[len] = '\0';
	/* Covert string buffer into string. */
	StrCache* cache = &G(env)->str_cache;
	cache_hint(env, cache);
	cache_emplace_in_place(cache, self);
	ai_gc_register_normal(env, self);

	return self;
}

GStr* ai_str_get_or_null_with_hash(a_henv env, a_lstr src, a_hash hash) {
    return str_get_or_null_with_hash(env, src, hash);
}

GStr* ai_str_get_or_new_with_hash(a_henv env, a_lstr src, a_hash hash) {
    return ai_str_get_or_null_with_hash(env, src, hash) ?: ai_str_new_with_hash(env, src, hash);
}

GStr* ai_str_get_or_new(a_henv env, a_lstr src) {
    a_hash hash = ai_str_hashof(env, src);
    return ai_str_get_or_new_with_hash(env, src, hash);
}

#define MAX_STACK_BUFFER_SIZE (256-1)

a_msg ai_str_load(a_henv env, a_sbfun fun, a_usize len, void* ctx, GStr** pstr) {
	if (likely(len < MAX_STACK_BUFFER_SIZE)) {
        /* Allocate buffer on stack. */
        char buf[MAX_STACK_BUFFER_SIZE + 1];
		try ((*fun)(ctx, buf, len));
		*pstr = ai_str_get_or_new(env, (a_lstr) {buf, len });
		return ALO_SOK;
	}
	else {
        /* Allocate buffer on heap. */
		GStr* buf = str_alloc(env, len);
        catch ((*fun)(ctx, buf->ptr, len), msg) {
            ai_mem_dealloc(G(env), buf, str_size(len));
            return msg;
        }
		
		*pstr = str_get_and_drop_buff_or_put(env, buf, len);
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
        return ai_str_get_or_new(env, (a_lstr) { buf, len });
    }
    else {
        GStr* buff = str_alloc(env, len);
		
		vsprintf(buff->ptr, fmt, varg);

		va_end(varg);
        
		return str_get_and_drop_buff_or_put(env, buff, len);
    }
}

static void str_mark(Global* gbl, GStr* self) {
    ai_gc_trace_work(gbl, str_size(self->len));
}

static void cache_remove(StrCache* cache, GStr* str) {
    /* Remove string from intern table. */
    GStr** slot = cache_head(cache, str->hash);
    loop {
        GStr* str1 = *slot;
        assume(str1 != null);
        if (str == str1) {
            *slot = str->snext;
            break;
        }
        slot = &str1->snext;
    }
    cache->len -= 1;
}

static void str_drop(Global* gbl, GStr* self) {
    cache_remove(&gbl->str_cache, self);
    ai_mem_dealloc(gbl, self, str_size(self->len));
}

void ai_str_boost1(a_henv env, void* block) {
    Global* gbl = G(env);

    run {
        /* Initialize builtin string first, but not put them into the cache. */

        static a_u8 const l_str_len[STR__COUNT] = {
#define SYMDEF(n,r) sizeof(r)-1,
            SYM_ISTRS(SYMDEF, M_void, M_void)
#undef SYMDEF
        };

        char const* src = ai_str_interns;

        for (a_u32 i = 0; i < STR__COUNT; ++i) {
            a_u32 len = l_str_len[i];

            GStr* self = block;
            a_lstr str = { src, len };
            a_hash hash = ai_str_hashof(env, str);

            g_set_gray(self);

            str_init(self, str, hash);
            str_id_set(self, i);

            gbl->fast_strs[i] = self;
            block += str_size(len);
            src += len + 1;
        }
    }
}

void ai_str_boost2(a_henv env) {
    Global* gbl = G(env);

    StrCache* cache = &gbl->str_cache;

    run {
        cache->ptr = ai_mem_vnew(env, GStr*, ALOI_INIT_STR_CACHE_CAPACITY);
        cache->hmask = ALOI_INIT_STR_CACHE_CAPACITY - 1;
        memclr(cache->ptr, sizeof(GStr*) * ALOI_INIT_STR_CACHE_CAPACITY);
    }

    run {
        for (a_u32 i = 0; i < STR__COUNT; ++i) {
            cache_emplace_in_place(cache, gbl->fast_strs[i]);
        }
    }
}

void ai_str_cache_shrink_if_need(Global* gbl) {
    StrCache* cache = &gbl->str_cache;
    if (!gbl->gcflags.emergency && cache->len <= cache->hmask / 4) {
        cache_shrink(gbl, cache);
    }
}

void ai_str_clean(Global* gbl) {
    StrCache* cache = &gbl->str_cache;
    assume(cache->len == STR__COUNT, "string size not matched.");
    ai_mem_vdel(gbl, cache->ptr, cache->hmask + 1);
}

static Impl const str_impl = {
    .tag = ALO_TSTR,
    .flags = IMPL_FLAG_GREEDY_MARK,
    .name = "str",
    .drop = str_drop,
    .mark = str_mark
};

char const ai_str_interns[] = {
#define SYMDEF(n,r) r"\0"
    SYM_ALL(SYMDEF, M_void, M_void)
#undef SYMDEF
};

static_assert(sizeof(ai_str_interns) == STR__TOTAL_LEN);
