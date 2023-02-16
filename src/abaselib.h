/*
 * abaselib.h
 *
 *  Created on: 2023/1/31
 *      Author: ueyudiud
 */

#ifndef abaselib_h_
#define abaselib_h_

#include "alo.h"

#ifndef aloi_show
# include <stdio.h>
# define aloi_show(fmt,args...) quiet(printf(fmt, ##args))
# define aloi_show_flush() quiet(fflush(stdout))
# define aloi_show_newline() quiet(fputc('\n', stdout), fflush(stdout))
#endif

ALO_EXPORT void aloL_base_show(a_henv env, a_isize id);

#endif /* abaselib_h_ */
