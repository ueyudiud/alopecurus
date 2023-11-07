/**
 *@file atm.c
 */

#define atm_c_
#define ALO_LIB

#include <string.h>

#include "atype.h"
#include "agc.h"
#include "aerr.h"
#include "avm.h"

#include "atm.h"

#define vm_call0(env,vf,va...) ai_vm_call(env, vm_push_args(env, vf, ##va), 0)
#define vm_call1(env,vf,va...) ai_vm_call_meta(env, vm_push_args(env, vf, ##va))

static a_bool type_look(a_henv env, GType* t, GStr* k, Value* pv) {
    catch (ai_type_ugets(env, t, k, pv)) {
        return true;
    }
    return false;
}

static a_bool type_lookt(a_henv env, GType* t, a_enum tm, Value* pv) {
    return type_look(env, t, g_str(env, STR_TM__FIRST + tm), pv);
}

static a_bool type_lookft(a_henv env, GType* t, a_enum tm, Value* pv) {
    assume(tm <= TM__FAST_MAX, "cannot fast lookup.");
    return type_has_ftm(t, tm) ? type_lookt(env, t, tm, pv) : true;
}

static a_bool meta_int(Value v, void* p) {
    if (v_is_int(v)) {
        a_int i = v_as_int(v);
        memcpy(p, &i, sizeof(a_int));
        return false;
    }
    return true;
}

a_bool ai_tm_hash(a_henv env, Value v, a_hash* ph) {
    a_hobj p = v_as_obj(v);

    Value vf;
    try (type_lookft(env, g_typeof(env, p), TM___hash__, &vf));

    Value vr = vm_call1(env, vf, v);
    if (!meta_int(vr, ph)) {
        ai_err_raisef(env, ALO_EINVAL, "result for '__hash__' should be int.");
    }
    return false;
}

a_bool ai_tm_equals(a_henv env, Value v1, Value v2, a_bool* pz) {
    a_hobj p = v_as_obj(v1);

    Value vf;
    try (type_lookft(env, g_typeof(env, p), TM___eq__, &vf));

    Value vr = vm_call1(env, vf, v1, v2);
    *pz = v_to_bool(vr);
    return false;
}

a_bool ai_tm_len(a_henv env, Value v, a_uint* pi) {
    a_hobj p = v_as_obj(v);

    Value vf;
    try (type_lookft(env, g_typeof(env, p), TM___len__, &vf));

    Value vr = vm_call1(env, vf, v);
    if (!meta_int(vr, pi)) {
        ai_err_raisef(env, ALO_EINVAL, "result for '__len__' should be int.");
    }
    return false;
}

a_bool ai_tm_look(a_henv env, Value v, GStr* k, Value* pv) {
    a_hobj p = v_as_obj(v);

    Value vf;
    try (type_lookft(env, g_typeof(env, p), TM___look__, &vf));

    if (v_is_func(vf)) {
        Value vr = vm_call1(env, vf, v, v_of_obj(k));
        v_set(env, pv, vr);
        return false;
    }
    else {
        v_set(env, pv, ai_vm_get(env, vf, v_of_obj(k)));
        return false;
    }
}

a_bool ai_tm_str(a_henv env, Value v, GStr** ps) {
    a_hobj p = v_as_obj(v);

    Value vf;
    try (type_lookt(env, g_typeof(env, p), TM___str__, &vf));

    if (v_is_str(vf)) {
        *ps = v_as_str(vf);
    }
    else {
        Value vs = vm_call1(env, vf, v);
        if (!v_is_str(vs)) {
            ai_err_raisef(env, ALO_EINVAL, "result for '__str__' should be string.");
        }
        *ps = v_as_str(v);
    }
    return false;
}

a_bool ai_tm_get(a_henv env, Value v1, Value v2, Value* pv) {
    a_hobj p = v_as_obj(v1);

    Value vf;
    try (type_lookft(env, g_typeof(env, p), TM___get__, &vf));

    Value vr = vm_call1(env, vf, v1, v2);
    v_set(env, pv, vr);
    return false;
}

a_bool ai_tm_set(a_henv env, Value v1, Value v2, Value v3) {
    a_hobj p = v_as_obj(v1);

    Value vf;
    try (type_lookft(env, g_typeof(env, p), TM___set__, &vf));

    vm_call0(env, vf, v1, v2, v3);
    return false;
}

a_bool ai_tm_unary(a_henv env, a_enum tm, Value v, Value* pv) {
    a_hobj p = v_as_obj(v);

    Value vf;
    try (type_lookt(env, g_typeof(env, p), tm, &vf));

    Value vr = vm_call1(env, vf, v);
    v_set(env, pv, vr);
    return false;
}

a_bool ai_tm_binary(a_henv env, a_enum tm, Value v1, Value v2, Value* pv) {
    a_hobj p = v_as_obj(v1);

    Value vf;
    try (type_lookt(env, g_typeof(env, p), tm, &vf));

    Value vr = vm_call1(env, vf, v1, v2);
    v_set(env, pv, vr);
    return false;
}

a_bool ai_tm_relation(a_henv env, a_enum tm, Value v1, Value v2, a_bool* pz) {
    a_hobj p = v_as_obj(v1);

    Value vf;
    try (type_lookt(env, g_typeof(env, p), tm, &vf));

    Value vr = vm_call1(env, vf, v1, v2);
    *pz = v_to_bool(vr);
    return false;
}

a_bool ai_tm_precall(a_henv env, Value v, Value* pv) {
    a_hobj p = v_as_obj(v);

    Value vf;
    try (type_lookt(env, g_typeof(env, p), TM___call__, &vf));

    v_set(env, pv, vf);
    return false;
}