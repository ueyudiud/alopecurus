/**
 *@file astr.c
 */

#define astr_c_
#define ALO_LIB

#include <stdio.h>
#include <string.h>

#include "aenv.h"
#include "amem.h"
#include "agc.h"
#include "astrx.h"

#include "astr.h"

static VTable const dstr_vtable;
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

static GStr* hstr_alloc(a_henv env, a_usize len) {
	GStr* self = ai_mem_alloc(env, hstr_size(len));
	self->_vtable = &hstr_vtable;
	self->_len = len;
	self->_data[len] = '\0';
	return self;
}

static void istr_init(IStr* self, void const* src, a_usize len, a_hash hash) {
    self->_body._len = len;
    self->_body._hash = hash;
    memcpy(self->_body._data, src, len);
    self->_body._data[len] = '\0';
}

static GStr* istr_of(a_henv env, void const* src, a_usize len, a_hash hash) {
    Global* g = G(env);
    IStrTable* istable = &g->_istable;

    /* Try lookup string in intern table. */
    for (IStr* str = istable->_table[hash & istable->_hmask]; str != null; str = str->_next) {
        if (str->_body._hash == hash && likely(str->_body._len == len && memcmp(str->_body._data, src, len) == 0)) {
            /* Revive string object if it is dead. */
            if (unlikely(g_is_other(g, gobj_cast(&str->_body)))) {
                str->_body._tnext = white_color(g);
            }
            return &str->_body;
        }
    }

    if (unlikely(istable->_len == istable->_hmask)) {
        grow_istable(env, istable);
    }

	/* String not found, create new string. */
	IStr* self = ai_mem_alloc(env, istr_size(len));
	self->_body._vtable = &istr_vtable;
	istr_init(self, src, len, hash);
	istable_emplace(istable, self);
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
	self->_body._vtable = &dstr_vtable;
	istr_init(self, src, len, hash);
	strx_id_set(&self->_body, tag);
	istable_emplace(&g->_istable, self);
	return &self->_body;
}

GStr* ai_str_new2(a_henv env, void const* src, a_usize len, a_hash hash) {
    assume(hash == ai_str_hashof(G(env)->_seed, src, len));
    return likely(len <= ALOI_SHTSTR_THRESHOLD) ?
		   istr_of(env, src, len, hash) :
		   hstr_new(env, src, len, hash);
}

GStr* ai_istr_new(a_henv env, void const* src, a_usize len) {
	a_hash hash = ai_str_hashof(G(env)->_seed, src, len);
	return istr_of(env, src, len, hash);
}

GStr* ai_str_new(a_henv env, void const* src, a_usize len) {
    a_hash hash = ai_str_hashof(G(env)->_seed, src, len);
	return ai_str_new2(env, src, len, hash);
}

a_msg ai_str_load(a_henv env, ZIn* in, a_usize len, GStr** pstr) {
	if (len <= ALOI_SHTSTR_THRESHOLD) {
		char buf[ALOI_SHTSTR_THRESHOLD];
		check(ai_io_iget(in, buf, len));
		*pstr = ai_istr_new(env, buf, len);
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
    char buf[ALOI_SHTSTR_THRESHOLD + 1];
    va_list varg2;
    va_copy(varg2, varg);
    a_usize len = cast(a_usize, cast(a_isize, vsnprintf(buf, sizeof(buf), fmt, varg)));
    if (len <= ALOI_SHTSTR_THRESHOLD) {
        va_end(varg2);
        return ai_istr_new(env, buf, len);
    }
    else {
        GStr* self = ai_mem_alloc(env, sizeof(GStr) + len);
		self->_vtable = &hstr_vtable;
        self->_len = len;
        vsprintf(cast(char*, self->_data), fmt, varg2);
        va_end(varg2);
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
    
    IStrTable* istable = &g->_istable;
    istable->_table = ai_mem_vnew(env, IStr*, ALOI_INIT_SHTSTR_TABLE_CAPACITY);
    istable->_hmask = ALOI_INIT_SHTSTR_TABLE_CAPACITY - 1;
    memset(istable->_table, 0, sizeof(IStr*) * ALOI_INIT_SHTSTR_TABLE_CAPACITY);
    
    g->_nomem_error = ai_str_newl(env, "out of memory.");
    ai_gc_fix_object(env, g->_nomem_error);
}

void ai_str_clean(Global* g) {
    IStrTable* istable = &g->_istable;
    assume(istable->_len == STRX__MAX - 1);
    ai_mem_vdel(g, istable->_table, istable->_hmask + 1);
}

static void istr_splash(Global* g, GStr* self) {
	ai_gc_trace_work(g, sizeof(IStr) + 1 + self->_len);
}

static void istr_delete(Global* g, GStr* self) {
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

	ai_mem_dealloc(g, self0, sizeof(IStr) + 1 + self->_len);
}

static void hstr_splash(Global* g, GStr* self) {
	ai_gc_trace_work(g, sizeof(GStr) + 1 + self->_len);
}

static void hstr_delete(Global* g, GStr* self) {
    ai_mem_dealloc(g, self, sizeof(GStr) + 1 + self->_len);
}

static VTable const dstr_vtable = {
	._tid = T_ISTR,
	._api_tag = ALO_TSTR,
	._repr_id = REPR_STR,
	._flags = VTABLE_FLAG_IDENTITY_EQUAL,
	._name = "str",
	._splash = null,
	._delete = null
};

static VTable const istr_vtable = {
	._tid = T_ISTR,
	._api_tag = ALO_TSTR,
	._repr_id = REPR_STR,
	._flags = VTABLE_FLAG_IDENTITY_EQUAL,
	._name = "str",
	._splash = fpcast(a_fp_splash, istr_splash),
	._delete = fpcast(a_fp_delete, istr_delete)
};

static VTable const hstr_vtable = {
	._tid = T_HSTR,
	._api_tag = ALO_TSTR,
	._repr_id = REPR_STR,
	._flags = VTABLE_FLAG_NONE,
	._name = "str",
	._splash = fpcast(a_fp_splash, hstr_splash),
	._delete = fpcast(a_fp_delete, hstr_delete)
};