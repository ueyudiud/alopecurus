/**
 *@file aauxlib.h
 */

#ifndef aauxlib_h_
#define aauxlib_h_

#include "alo.h"

typedef struct aloL_Entry aloL_Entry;
typedef struct aloL_Buf {
    char* ptr;
    a_usize len;
    a_usize cap;
} aloL_Buf;

ALO_EXPORT a_henv (aloL_create)(void);

ALO_EXPORT ALO_NORETURN void (aloL_argerror)(a_henv env, a_ulen id, char const* what);
ALO_EXPORT ALO_NORETURN void (aloL_typeerror)(a_henv env, a_ulen id, char const* name);
ALO_EXPORT void (aloL_checktag)(a_henv env, a_ulen id, a_msg tag);
ALO_EXPORT a_msg (aloL_checkany)(a_henv env, a_ulen id);
ALO_EXPORT a_int (aloL_checkint)(a_henv env, a_ulen id);
ALO_EXPORT a_float (aloL_checknum)(a_henv env, a_ulen id);
ALO_EXPORT void* (aloL_checkptr)(a_henv env, a_ulen id);
ALO_EXPORT char const* (aloL_checklstr)(a_henv env, a_ulen id, a_usize* plen);
ALO_EXPORT a_u32 (aloL_checkenum)(a_henv env, a_ulen id, char const* const* es, char const* what);
ALO_EXPORT a_bool (aloL_optbool)(a_henv env, a_ulen id, a_bool dfl);
ALO_EXPORT a_bool (aloL_optint_)(a_henv env, a_ulen id, a_int* pval);
ALO_EXPORT a_bool (aloL_optnum_)(a_henv env, a_ulen id, a_float* pval);
ALO_EXPORT char const* (aloL_optlstr)(a_henv env, a_ulen id, a_usize* plen);
ALO_EXPORT a_u32 (aloL_optenum)(a_henv env, a_ulen id, char const* const* es, char const* what, a_u32 dfl);

#define aloL_checkstr(env,id) aloL_checklstr(env, id, NULL)
#define aloL_optstr(env,id) aloL_optlstr(env, id, NULL)
#define aloL_optint(env,id,dfl) ({ a_int _v; aloL_optint_(env, id, &_v) ? _v : (dfl); })
#define aloL_optnum(env,id,dfl) ({ a_float _v; aloL_optnum_(env, id, &_v) ? _v : (dfl); })

ALO_EXPORT a_msg (aloL_resultcx)(a_henv env, a_bool stat, int err, char const* what);
ALO_EXPORT a_msg (aloL_resulte)(a_henv env, a_i32 stat);

#define aloL_resultc(env,stat,what) aloL_resultcx(env, stat, errno, what)
#define aloL_pushfail(env) alo_pushnil(env)

ALO_EXPORT a_msg (aloL_compiles)(a_henv env, char const* src, a_usize len, char const* fname, a_u32 options);
ALO_EXPORT a_msg (aloL_compilef)(a_henv env, char const* fname, a_u32 options);

ALO_EXPORT ALO_NORETURN void (aloL_raisef)(a_henv env, char const* fmt, ...);
ALO_EXPORT a_msg (aloL_traceerror)(a_henv dst, a_henv src, a_ilen id, a_usize level, a_usize limit);

ALO_EXPORT a_msg (aloL_gets)(a_henv env, a_ilen id, char const* s);

ALO_EXPORT a_msg (aloL_gettm)(a_henv env, a_ilen id, char const* s);

ALO_EXPORT void (aloL_puts)(a_henv env, a_ilen id, char const* s);
ALO_EXPORT void (aloL_putalls_)(a_henv env, a_ilen id, aloL_Entry const* es, a_usize ne);

#define aloL_putalls(env,id,bs) aloL_putalls_(env, id, bs, sizeof(bs) / sizeof(aloL_Entry))

ALO_EXPORT void* (aloL_newblk)(a_henv env, a_usize s);
ALO_EXPORT aloL_Buf* (aloL_newbuf)(a_henv env);
ALO_EXPORT void* (aloL_bufhint)(a_henv env, aloL_Buf* b, a_usize a);
ALO_EXPORT void (aloL_bufpush)(a_henv env, aloL_Buf* b);
ALO_EXPORT void (aloL_bufstr)(a_henv env, aloL_Buf* b);

struct aloL_Entry {
	char const* name; /* Entry dbg_name. */
	a_cfun fptr; /* Function pointer, null for create entry only. */
};

#endif /* aauxlib_h_ */
