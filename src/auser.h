/**
 *@file auser.h
 */

#ifndef auser_h_
#define auser_h_

#include "aobj.h"

intern VTable const ai_auser_vtable;

intern GUser* ai_auser_new(a_henv env, GMeta* type);

#endif /* auser_h_ */
