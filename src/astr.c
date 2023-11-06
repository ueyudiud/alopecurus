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
a_hash ai_str_hashof(a_henv env, void const* src, a_usize len) {
    a_hash hash = G(env)->_seed;
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

static void cache_grow(a_henv env, StrCache* cache) {
    a_usize old_size = cache->_hmask + 1;
    a_usize new_size = old_size * 2;

    GStr** table = ai_mem_vgrow(env, cache->_table, old_size, new_size);

    /* Move strings. */
    for (a_usize i = 0; i < old_size; ++i) {
        GStr** p = &table[i];
        GStr** q = &table[i + old_size];
        GStr* s = *p;
        while (s != null) {
            if (s->_hash & old_size) {
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
	cache->_hmask = new_size - 1;
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
	return ai_mem_gnew(env, GStr, sizeof_GStr(len));
}

static void str_init(GStr* self, void const* src, a_usize len, a_hash hash) {
	self->_vptr = &str_vtable;
    self->_len = len;
    self->_hash = hash;
    memcpy(self->_ptr, src, len);
    self->_ptr[len] = '\0';
}

static GStr* str_get_or_null_with_hash(a_henv env, void const* src, a_usize len, a_hash hash) {
    Global* g = G(env);
    StrCache* cache = &g->_str_cache;

    /* Try lookup string in intern table. */
    for (GStr* str = cache->_table[hash & cache->_hmask]; str != null; str = str->_snext) {
        if (str->_hash == hash && likely(str->_len == len && memcmp(str->_ptr, src, len) == 0)) {
            /* Revive string object if it is dead. */
            if (unlikely(g_has_other_color(g, str))) {
                str->_tnext = white_color(g);
            }
            return str;
        }
    }

	return null;
}

static GStr* str_get_or_new_with_hash(a_henv env, void const* src, a_usize len, a_hash hash) {
	GStr* self = str_get_or_null_with_hash(env, src, len, hash);

	if (self != null)
		return self;

	/* String not found, create new string. */
	StrCache* cache = &G(env)->_str_cache;
	cache_hint(env, cache);
	
	self = str_alloc(env, len);
    str_init(self, src, len, hash);

	cache_emplace_in_place(cache, self);
    ai_gc_register_object(env, self);
    return self;
}

static GStr* str_get_and_drop_buff_or_put(a_henv env, GStr* buff, a_usize len) {
	a_hash hash = ai_str_hashof(env, buff->_ptr, len);
	GStr* self = str_get_or_null_with_hash(env, buff->_ptr, len, hash);
	
	if (self != null) {
		ai_mem_gdel(G(env), buff, sizeof_GStr(len));
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

static GStr* str_get_or_new(a_henv env, void const* src, a_usize len) {
	a_hash hash = ai_str_hashof(env, src, len);
	return str_get_or_new_with_hash(env, src, len, hash);
}

GStr* ai_str_new(a_henv env, void const* src, a_usize len) {
	return str_get_or_new(env, src, len);
}

#define MAX_STACK_BUFFER_SIZE 255

a_msg ai_str_load(a_henv env, a_sbfun fun, a_usize len, void* ctx, GStr** pstr) {
	char buf[MAX_STACK_BUFFER_SIZE + 1];
	if (likely(len < MAX_STACK_BUFFER_SIZE)) {
		try((*fun)(ctx, buf, len));
		*pstr = ai_str_new(env, buf, len);
		return ALO_SOK;
	}
	else {
		GStr* buff = str_alloc(env, len); /* Allocate buffer to place data. */
		a_msg msg = (*fun)(ctx, buff->_ptr, len);
		
		if (unlikely(msg != ALO_SOK)) {
			ai_mem_gdel(G(env), buff, sizeof_GStr(len));
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
        return str_get_or_new(env, buf, len);
    }
    else {
        GStr* buff = str_alloc(env, len);
		
		vsprintf(cast(char*, buff->_ptr), fmt, varg);

		va_end(varg);
        
		return str_get_and_drop_buff_or_put(env, buff, len);
    }
}

a_bool ai_str_requals(GStr* self, void const* dat, a_usize len) {
    return self->_len == len && memcmp(self->_ptr, dat, len) == 0;
}

static void str_mark(Global* g, GStr* self) {
    ai_gc_trace_work(g, sizeof_GStr(self->_len));
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
        slot = &str->_snext;
    }
    cache->_len -= 1;
}

static void str_drop(Global* g, GStr* self) {
    cache_remove(&g->_str_cache, self);
    ai_mem_gdel(g, self, sizeof_GStr(self->_len));
}

void ai_str_boost(a_henv env, void* block) {
    Global* g = G(env);
    
    StrCache* cache = &g->_str_cache;

	run {
		cache->_table = ai_mem_vnew(env, GStr*, ALOI_INIT_SHTSTR_TABLE_CAPACITY);
		cache->_hmask = ALOI_INIT_SHTSTR_TABLE_CAPACITY - 1;
		memset(cache->_table, 0, sizeof(GStr*) * ALOI_INIT_SHTSTR_TABLE_CAPACITY);
	}

	run {
		g->_nomem_error = ai_str_newl(env, "out of memory.");
		ai_gc_fix_object(env, g->_nomem_error);
	}

	run {
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

			GStr* self = g_cast(GStr, g_biased(block));
			a_hash hash = ai_str_hashof(env, src, len);

			g_set_gray(self);

            str_init(self, src, len, hash);
			str_id_set(self, i);

			cache_emplace_in_place(cache, self);

			g->_names[i] = self;
			block += sizeof(GcHead) + sizeof_GStr(len);
			src += len + 1;
		}
	}
}

void ai_str_clean(Global* g) {
    StrCache* cache = &g->_str_cache;
    assume(cache->_len == STR__COUNT, "string size not matched.");
    ai_mem_vdel(g, cache->_table, cache->_hmask + 1);
}

static VTable const str_vtable = {
    ._stencil = V_STENCIL(T_STR),
    ._flags = VTABLE_FLAG_NONE,
    ._type_ref = g_type_ref(_str),
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
