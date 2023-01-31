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
# define aloi_show(fmt,args...) printf(fmt, ##args)
#endif

#ifndef aloi_show_flush
# define aloi_show_flush() fflush(stdout)
#endif

ALO_EXPORT void aloL_base_show(a_henv env, ptrdiff_t id);

#endif /* abaselib_h_ */
