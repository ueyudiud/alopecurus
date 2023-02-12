/**
 *@file aauxlib.h
 */

#ifndef aauxlib_h_
#define aauxlib_h_

#include "alo.h"

typedef struct aloL_Binding aloL_Binding;

ALO_EXPORT a_henv (aloL_create)(void);

ALO_EXPORT a_msg (aloL_compiles)(a_henv env, char const* src, a_usize len, char const* fname, a_u32 options);
ALO_EXPORT a_msg (aloL_compilef)(a_henv env, char const* fname, a_u32 options);

ALO_EXPORT void (aloL_errorf)(a_henv env, char const* fmt, ...);
ALO_EXPORT a_msg (aloL_traceerror)(a_henv env, a_isize id, a_usize limit);

ALO_EXPORT void (aloL_newmod_)(a_henv env, char const* name, aloL_Binding const* bs, a_usize nb);

#define aloL_newmod(env,name,bs) aloL_newmod_(env, name, bs, sizeof(bs) / sizeof(aloL_Binding))

struct aloL_Binding {
	char const* name; /* Entry name. */
	a_cfun fptr; /* Function pointer, null for create entry only. */
};

#endif /* aauxlib_h_ */
