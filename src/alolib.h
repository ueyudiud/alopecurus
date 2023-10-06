/**
 *@file alolib.h
 */

#ifndef alolib_h_
#define alolib_h_

#include "alo.h"

#define ALO_LIB_BASE_NAME "_G"
ALO_EXPORT void aloopen_base(a_henv env);

#define ALO_LIB_LIST_NAME "list"
ALO_EXPORT void aloopen_list(a_henv env);

#define ALO_LIB_TYPE_NAME "type"
ALO_EXPORT void aloopen_type(a_henv env);

#define ALO_LIB_DEBUG_NAME "debug"
ALO_EXPORT void aloopen_debug(a_henv env);

#define ALO_LIB_SYS_NAME "sys"
ALO_EXPORT void aloopen_sys(a_henv env);

ALO_EXPORT void aloL_openlibs(a_henv env);

#endif /* alolib_h_ */
