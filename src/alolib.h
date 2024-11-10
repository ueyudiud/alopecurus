/**
 *@file alolib.h
 */

#ifndef alolib_h_
#define alolib_h_

#include "alo.h"

#define ALO_LIB_BASE_NAME "_G"
ALO_EXPORT void aloopen_base(a_henv env);

#define ALO_LIB_INT_NAME "int"
ALO_EXPORT void aloopen_int(a_henv env);

#define ALO_LIB_STR_NAME "str"
ALO_EXPORT void aloopen_str(a_henv env);

#define ALO_LIB_LIST_NAME "list"
ALO_EXPORT void aloopen_list(a_henv env);

#define ALO_LIB_TABLE_NAME "table"
ALO_EXPORT void aloopen_table(a_henv env);

#define ALO_LIB_TYPE_NAME "type"
ALO_EXPORT void aloopen_type(a_henv env);

#define ALO_LIB_SYS_NAME "sys"
ALO_EXPORT void aloopen_sys(a_henv env);

#define ALO_LIB_IO_NAME "io"
ALO_EXPORT void aloopen_io(a_henv env);

#define ALO_LIB_DEBUG_NAME "debug"
ALO_EXPORT void aloopen_debug(a_henv env);

#define ALO_LIB_LOAD_NAME "lib"
ALO_EXPORT void aloopen_load(a_henv env);

ALO_EXPORT void aloL_openlibs(a_henv env);

#endif /* alolib_h_ */
