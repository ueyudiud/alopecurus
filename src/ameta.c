/**
 *@file ameta.c
 */

#define ameta_c_
#define ALO_LIB

#include "atype.h"
#include "agc.h"
#include "aerr.h"
#include "avm.h"

#include "ameta.h"

#define vm_call0(env,vf,va...) ai_vm_call(env, vm_push_args(env, vf, ##va), 0)
#define vm_call1(env,vf,va...) ai_vm_call_meta(env, vm_push_args(env, vf, ##va))

static a_bool meta_int(Value v, void* p) {
    if (v_is_int(v)) {
        a_int i = v_as_int(v);
        memcpy(p, &i, sizeof(a_int));
        return false;
    }
    return true;
}

a_bool ai_meta_hash(a_henv env, Value v, a_hash* ph) {
    Value vf = ai_obj_glookftm(env, v_as_obj(v), TM___hash__);
    if (v_is_nil(vf)) return true;

    Value vr = vm_call1(env, vf, v);
    if (!meta_int(vr, ph)) {
        ai_err_raisef(env, ALO_EINVAL, "result for '__hash__' should be int.");
    }
    return false;
}

a_bool ai_meta_equals(a_henv env, Value v1, Value v2, a_bool* pz) {
    Value vf = ai_obj_vlookftm(env, v1, TM___eq__);
    if (v_is_nil(vf)) return true;

    Value vr = vm_call1(env, vf, v1, v2);
    *pz = v_to_bool(vr);
    return false;
}

a_bool ai_meta_len(a_henv env, Value v, a_uint* pi) {
    Value vf = ai_obj_vlookftm(env, v, TM___len__);
    if (v_is_nil(vf)) return true;

    Value vr = vm_call1(env, vf, v);
    if (!meta_int(vr, pi)) {
        ai_err_raisef(env, ALO_EINVAL, "result for '__len__' should be int.");
    }
    return false;
}

a_bool ai_meta_str(a_henv env, Value v, GStr** ps) {
    Value vf;
    try (ai_obj_ulooktm(env, v, TM___str__, &vf));

    Value vs = vm_call1(env, vf, v);
    if (!v_is_str(vs)) ai_err_raisef(env, ALO_EINVAL, "result for '__str__' should be string.");
    *ps = v_as_str(v);
    return false;
}

a_bool ai_meta_get(a_henv env, Value v1, Value v2, Value* pv) {
    Value vf = ai_obj_vlookftm(env, v1, TM___get__);
    try (v_is_nil(vf));

    Value vr = vm_call1(env, vf, v1, v2);
    v_set(env, pv, vr);
    return false;
}

a_bool ai_meta_set(a_henv env, Value v1, Value v2, Value v3) {
    Value vf = ai_obj_vlookftm(env, v1, TM___set__);
    try (v_is_nil(vf));

    vm_call0(env, vf, v1, v2, v3);
    return false;
}

a_bool ai_meta_unary(a_henv env, a_enum tm, Value v, Value* pv) {
    Value vf;
    try (ai_obj_ulooktm(env, v, tm, &vf));

    Value vr = vm_call1(env, vf, v);
    v_set(env, pv, vr);
    return false;
}

a_bool ai_meta_binary(a_henv env, a_enum tm, Value v1, Value v2, Value* pv) {
    Value vf;
    try (ai_obj_ulooktm(env, v1, tm, &vf));

    Value vr = vm_call1(env, vf, v1, v2);
    v_set(env, pv, vr);
    return false;
}

a_bool ai_meta_relation(a_henv env, a_enum tm, Value v1, Value v2, a_bool* pz) {
    Value vf;
    try (ai_obj_ulooktm(env, v1, tm, &vf));

    Value vr = vm_call1(env, vf, v1, v2);
    *pz = v_to_bool(vr);
    return false;
}