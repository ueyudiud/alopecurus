/**
 *@file aauxlib.h
 */

#ifndef aauxlib_h_
#define aauxlib_h_

#include "alo.h"

typedef struct aloL_Entry aloL_Entry;

ALO_EXPORT a_henv (aloL_create)(void);

ALO_EXPORT void (aloL_argerror)(a_henv env, a_usize id, char const* what);
ALO_EXPORT void (aloL_typeerror)(a_henv env, a_usize id, char const* name);
ALO_EXPORT void (aloL_checktag)(a_henv env, a_usize id, a_msg tag);
ALO_EXPORT a_int (aloL_checkint)(a_henv env, a_usize id);
ALO_EXPORT a_float (aloL_checknum)(a_henv env, a_usize id);
ALO_EXPORT char const* (aloL_checklstr)(a_henv env, a_usize id, a_usize* plen);
ALO_EXPORT a_bool (aloL_optint_)(a_henv env, a_usize id, a_int* pval);
ALO_EXPORT a_bool (aloL_optnum_)(a_henv env, a_usize id, a_float* pval);
ALO_EXPORT char const* (aloL_optlstr)(a_henv env, a_usize id, a_usize* plen);

#define aloL_checkstr(env,id) aloL_checklstr(env, id, NULL)
#define aloL_optstr(env,id) aloL_optlstr(env, id, NULL)
#define aloL_optint(env,id,dfl) ({ a_int _v; aloL_optint_(env, id, &_v) ? _v : (dfl); })
#define aloL_optnum(env,id,dfl) ({ a_float _v; aloL_optnum_(env, id, &_v) ? _v : (dfl); })

ALO_EXPORT a_msg (aloL_resultcx)(a_henv env, a_bool stat, errno_t err, char const* what);
ALO_EXPORT a_msg (aloL_resulte)(a_henv env, a_i32 stat);

#define aloL_resultc(env,stat,what) aloL_resultcx(env, stat, errno, what)
#define aloL_pushfail(env) alo_pushnil(env)

ALO_EXPORT a_msg (aloL_compiles)(a_henv env, char const* src, a_usize len, char const* fname, a_u32 options);
ALO_EXPORT a_msg (aloL_compilef)(a_henv env, char const* fname, a_u32 options);

ALO_EXPORT void (aloL_raisef)(a_henv env, char const* fmt, ...);
ALO_EXPORT a_msg (aloL_traceerror)(a_henv env, a_isize id, a_usize level, a_usize limit);

ALO_EXPORT void (aloL_putfields_)(a_henv env, a_isize id, aloL_Entry const* bs, a_usize nb);
ALO_EXPORT void (aloL_newmod_)(a_henv env, char const* name, aloL_Entry const* bs, a_usize nb);

#define aloL_putfields(env,id,bs) aloL_putfields_(env, id, bs, sizeof(bs) / sizeof(aloL_Entry))
#define aloL_newmod(env,name,bs) aloL_newmod_(env, name, bs, sizeof(bs) / sizeof(aloL_Entry))

struct aloL_Entry {
	char const* name; /* Entry name. */
	a_cfun fptr; /* Function pointer, null for create entry only. */
};

#endif /* aauxlib_h_ */
