/**
 *@file auser.h
 */

#ifndef auser_h_
#define auser_h_

#include "aobj.h"

intern VImpl const ai_auser_vtable;

intern GAUser* ai_auser_new(a_henv env, GType* type);
intern Value ai_auser_get(a_henv env, GAUser* self, Value key);
intern void ai_auser_set(a_henv env, GAUser* self, Value key, Value value);

#endif /* auser_h_ */
