/**
 *@file atm.c
 */

#define atm_c_
#define ALO_LIB

#include "atable.h"
#include "afun.h"
#include "auser.h"
#include "agc.h"
#include "aerr.h"
#include "avm.h"

#include "atm.h"

#define vm_call0(env,vf,va...) ai_vm_call(env, vm_push_args(env, vf, ##va), 0)
#define vm_call1(env,vf,va...) ai_vm_call_meta(env, vm_push_args(env, vf, ##va))

static a_bool type_look(a_henv env, GType* type, GStr* k, Value* pv) {
    return ai_type_gets(env, type, k, pv);
}

static a_bool type_lookt(a_henv env, GType* type, a_enum tm, Value* pv) {
    assume(tm <= TM__LIMIT, "bad tag of method.");
    GStr* k = g_str(env, STR__TM_BEGIN + tm);
    return type_look(env, type, k, pv);
}

static a_bool type_lookft(a_henv env, GType* type, a_enum tm, Value* pv) {
    assume(tm <= TM__FAST_MAX, "cannot fast lookup tagged method.");
    if (!mt_has_ftm(type, tm)) return true;
    catch (type_lookt(env, type, tm, pv)) {
        type->ftmz |= FTM_BIT(tm);
        return true;
    }
    return false;
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
    a_gptr p = v_as_ref(v);

    Value vf;
    try (type_lookft(env, g_type(env, p), TM___hash__, &vf));

    Value vr = vm_call1(env, vf, v);
    if (!meta_int(vr, ph)) {
        ai_err_raisef(env, ALO_EINVAL, "result for '__hash__' should be int.");
    }
    return false;
}

a_bool ai_tm_equals(a_henv env, Value v1, Value v2, a_bool* pz) {
    a_gptr p = v_as_ref(v1);

    Value vf;
    try (type_lookft(env, g_type(env, p), TM___eq__, &vf));

    Value vr = vm_call1(env, vf, v1, v2);
    *pz = v_to_bool(vr);
    return false;
}

a_bool ai_tm_len(a_henv env, Value v, a_uint* pi) {
    a_gptr p = v_as_ref(v);

    Value vf;
    try (type_lookft(env, g_type(env, p), TM___len__, &vf));

    Value vr = vm_call1(env, vf, v);
    if (!meta_int(vr, pi)) {
        ai_err_raisef(env, ALO_EINVAL, "result for '__len__' should be int.");
    }

    return false;
}

a_bool ai_tm_look(a_henv env, Value v, GStr* k, Value* pv) {
    Value vf;
    try (type_lookft(env, v_type(env, v), TM___look__, &vf));

    if (v_is_func(vf)) {
        Value vr = vm_call1(env, vf, v, v_of_str(k));
        v_set(env, pv, vr);
    }
    else {
        v_set(env, pv, ai_vm_get(env, vf, v_of_str(k)));
    }

    return false;
}

a_bool ai_tm_str(a_henv env, Value v, GStr** ps) {
    a_gptr p = v_as_ref(v);

    Value vf;
    try (type_lookt(env, g_type(env, p), TM___str__, &vf));

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
    a_gptr p = v_as_ref(v1);

    Value vf;
    try (type_lookft(env, g_type(env, p), TM___get__, &vf));

    Value vr = vm_call1(env, vf, v1, v2);
    v_set(env, pv, vr);

    return false;
}

a_bool ai_tm_set(a_henv env, Value v1, Value v2, Value v3) {
    a_gptr p = v_as_ref(v1);

    Value vf;
    try (type_lookft(env, g_type(env, p), TM___set__, &vf));

    vm_call0(env, vf, v1, v2, v3);

    return false;
}

a_bool ai_tm_unary(a_henv env, a_enum tm, Value v, Value* pv) {
    Value vf;
    try (type_lookt(env, v_type(env, v), tm, &vf));

    Value vr = vm_call1(env, vf, v);
    v_set(env, pv, vr);

    return false;
}

a_bool ai_tm_binary(a_henv env, a_enum tm, Value v1, Value v2, Value* pv) {
    Value vf;
    try (type_lookt(env, v_type(env, v1), tm, &vf));

    Value vr = vm_call1(env, vf, v1, v2);
    v_set(env, pv, vr);

    return false;
}

a_bool ai_tm_relation(a_henv env, a_enum tm, Value v1, Value v2, a_bool* pz) {
    Value vf;
    try (type_lookt(env, v_type(env, v1), tm, &vf));

    Value vr = vm_call1(env, vf, v1, v2);
    *pz = v_to_bool(vr);

    return false;
}

a_bool ai_tm_precall(a_henv env, Value v, Value* pv) {
    Value vf;
    try (type_lookt(env, v_type(env, v), TM___call__, &vf));

    v_set(env, pv, vf);

    return false;
}

void ai_tm_close(a_henv env, a_gptr o) {
    assume(g_is_user(o) || g_is(o, T_OTHER), "unexpected close dispatch.");

    Value vf;
    GType* type = g_type(env, o);
    if (type_lookft(env, type, TM___close__, &vf))
        return;

    ai_vm_call_meta(env, vm_push_args(env, vf, v_of_ref(o, T_OTHER)));
}