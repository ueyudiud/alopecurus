/**
 *@file amodlib.c
 */

#define amodlib_c_
#define ALO_LIB

#include "adef.h"
#include "atable.h"
#include "amod.h"
#include "agc.h"
#include "aapi.h"

#include "alo.h"
#include "aauxlib.h"
#include "alolib.h"

static a_msg mod___new__(a_henv env) {
    if (alo_stacksize(env) == 0) {
        alo_newmod(env, 0);
    }
    else {
        aloL_checktag(env, 0, ALO_TTABLE);
        alo_settop(env, 1);

        alo_newmod(env, 0);

        GTable* src = v_as_table(api_elem(env, 0));
        GMod* dst = v_as_mod(api_elem(env, 1));

        if (src->len > 0) {
            for (a_u32 i = 0; i <= src->hmask; ++i) {
                TNode* node = &src->ptr[i];
                if (!v_is_nil(node->key)) {
                    ai_mod_set(env, dst, node->key, node->value);
                }
            }
        }

        ai_gc_trigger(env);
    }
    return 1;
}

void aloopen_mod(a_henv env) {
    static aloL_Entry const bindings[] = {
        { "__new__", mod___new__ }
    };

    alo_pushptype(env, ALO_TMOD);
    aloL_putalls(env, -1, bindings);
}
