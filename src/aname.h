/**
 *@file aname.h
 */

#ifndef aname_h_
#define aname_h_

#include "adef.h"
#include "akw.h"
#include "aop.h"

#define PT_LIST(_) \
	_(BOOL, "bool") \
	_(INT, "int") \
	_(PTR, "ptr") \
	_(STR, "str") \
	_(TUPLE, "tuple") \
	_(LIST, "list") \
	_(TABLE, "table") \
	_(FUNC, "func") \
	_(ROUTE, "route") \
	_(user, "user")

enum {
    NAME__NORMAL,

    NAME__EMPTY,
    
    NAME_KW__BEGIN = NAME__EMPTY,
#define KWDEF(id,name) M_cat(NAME_KW_,id),
    KW_LIST(KWDEF)
#undef KWDEF
    NAME_KW__END,
    NAME_KW__FIRST = NAME_KW__BEGIN + 1,
    NAME_KW__LAST = NAME_KW__END - 1,

	NAME_TM__BEGIN = NAME_KW__LAST,
#define TMDEF(id,name) M_cat(NAME_TM_,id),
	TM_LIST(TMDEF)
#undef TMDEF
	NAME_TM__END,
	NAME_TM__FIRST = NAME_TM__BEGIN + 1,
	NAME_TM__LAST = NAME_TM__END - 1,

    NAME__END = NAME_TM__END
};

#define name_id(str) ((str)->_tnext >> 32)
#define name_iskw(str) (name_id(str) >= NAME_KW__FIRST && name_id(str) <= NAME_KW__LAST)
#define name_istm(str) (name_id(str) >= NAME_TM__FIRST && name_id(str) <= NAME_TM__LAST)
#define name_totk(str) (cast(a_i32, name_id(str)) + TK_IF - NAME_KW_IF)
#define name_totm(str) (cast(a_i32, name_id(str)) + TM_GET - NAME_TM_GET)

#define name_id_set(str,tag) quiet((str)->_tnext = cast(a_u64, tag) << 32)

enum {
#define KWPOS(id,name) NAME_POS_KW_##id, NAME_EPOS_KW_##id = NAME_POS_KW_##id + sizeof(name) - 1,
	KW_LIST(KWPOS)
#undef KWPOS
#define TMPOS(id,name) NAME_POS_TM_##id, NAME_EPOS_TM_##id = NAME_POS_TM_##id + sizeof(name) - 1,
	TM_LIST(TMPOS)
#undef TMPOS
	NAME_POS__MAX,

	NAME__DUMMY = ISIZE_MAX /* Pad enumeration to a_isize type. */
};

#define name_ptr(p) (&ai_obj_names[p])
#define name_str_kw(id) (new(a_lstr) { name_ptr(NAME_POS_KW_##id), NAME_EPOS_KW_##id - NAME_POS_KW_##id })
#define name_str_tm(id) (new(a_lstr) { name_ptr(NAME_POS_TM_##id), NAME_EPOS_TM_##id - NAME_POS_TM_##id })

#endif /* aname_h_ */
