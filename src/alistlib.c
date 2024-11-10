/**
 *@file alistlib.c
 */

#define alistlib_c_
#define ALO_LIB

#include "aop.h"
#include "aobj.h"
#include "atuple.h"
#include "alist.h"
#include "abuf.h"
#include "agc.h"
#include "avm.h"
#include "aapi.h"

#include "alo.h"
#include "aauxlib.h"
#include "alolib.h"

static GList* check_self(a_henv env) {
    aloL_checktag(env, 0, ALO_TLIST);
    return v_as_list(*api_stack(env, 0));
}

static a_msg list___new__(a_henv env) { /* Should this function write in script? */
    Value v = api_elem(env, 0);
    switch (v_get_tag(v)) {
        case T_NIL: {
            alo_newlist(env, 0);
            break;
        }
        case T_INT: {
            alo_newlist(env, cast(a_uint, v_as_int(v)));
            break;
        }
        case T_TUPLE: {
            GTuple* init_val = v_as_tuple(v);

            GList* out = ai_list_new(env);
            v_set_list(env, api_incr_stack(env), out);
            ai_list_push_all(env, out, init_val->ptr, init_val->len);

            ai_gc_trigger(env);
            break;
        }
        case T_LIST: {
            GList* init_val = v_as_list(v);

            GList* out = ai_list_new(env);
            v_set_list(env, api_incr_stack(env), out);
            ai_list_push_all(env, out, init_val->ptr, init_val->len);

            ai_gc_trigger(env);
            break;
        }
        default: {
            aloL_argerror(env, 1, "initial size or collection expected."); //TODO iterable value support.
        }
    }
    return 1;
}

static a_msg list_clear(a_henv env) {
    GList* self = check_self(env);

    self->len = 0;

    return 0;
}

static a_msg list_get(a_henv env) {
    GList* self = check_self(env);
    a_int k = aloL_checkint(env, 1);

    Value v;

    a_msg msg = ai_list_ugeti(env, self, k, &v);
    if (msg == ALO_SOK) {
        v_set(env, api_incr_stack(env), v);
    }
    else {
        alo_settop(env, 3); /* or null, return nil or default value. */
    }

    return 1;
}

static a_msg list_mkstr(a_henv env) {
    GList* self = check_self(env);

    a_lstr pre = {}, sep = {}, post = {};
    a_ilen n = alo_stacksize(env);
    switch (n) {
        case 1: {
            break;
        }
        case 2: {
            sep.ptr = aloL_checklstr(env, 1, &sep.len);
            break;
        }
        case 4: {
            pre.ptr = aloL_checklstr(env, 1, &pre.len);
            sep.ptr = aloL_checklstr(env, 2, &sep.len);
            post.ptr = aloL_checklstr(env, 3, &post.len);
            break;
        }
        default: {
            aloL_raisef(env, "bad argument count to list.mkstr, 1,2,4 expected, got %d", n);
        }
    }

    GBuf* buf = ai_buf_new(env);
    v_set_buf(env, api_incr_stack(env), buf);

    at_buf_putls(env, buf, pre.ptr, pre.len);
    for (a_ulen i = 0; i < self->len; ++i) {
        if (i != 0) at_buf_putls(env, buf, sep.ptr, sep.len);
        ai_vm_append(env, buf, self->ptr[i]);
    }
    at_buf_putls(env, buf, post.ptr, post.len);
    alo_pushstr(env, buf->ptr, buf->len);

    ai_gc_trigger(env);
    return 1;
}

static a_msg list_push(a_henv env) {
    GList* self = check_self(env);
    aloL_checkany(env, 1);

    Value v = api_elem(env, 1);

    ai_list_push(env, self, v);
    ai_gc_trigger(env);
    return 0;
}

static a_msg list_reverse(a_henv env) {
    GList* self = check_self(env);
    v_reverse(env, self->ptr, self->ptr + self->len);
    return 0;
}

static a_msg list_repeat(a_henv env) {
    GList* self = check_self(env);
    a_u32 n = aloL_checkint(env, 1);
    alo_settop(env, 1);

    a_u32 len;
    if (ckd_mul(&len, self->len, n)) {
        aloL_raisef(env, "too many elements");
    }

    alo_newlist(env, len);
    GList* result = v_as_list(api_elem(env, 1));
    for (a_u32 i = 0; i < n; ++i) {
        ai_list_push_all(env, result, self->ptr, self->len);
    }

    return 1;
}

/**
 ** list.sort implementation
 */

#define MIN_MERGE_SHIFT 5
#define MIN_MERGE (u32c(1) << MIN_MERGE_SHIFT)

#define MIN_GALLOP 7

#define MAX_RUN_STACK 49

/**
 ** Do 'less than' compare for sort function.
 *@see list_sort
 */
static a_bool sort_comp(a_henv env, Value v1, Value v2) {
    Value vf = env->frame->stack_bot[1];
    if (v_is_nil(vf)) {
        return ai_vm_compare(env, v1, v2, OP_LT);
    }
    else {
        Value v = ai_vm_call_meta(env, vm_push_args(env, vf, v1, v2));
        return v_to_bool(v);
    }
}

/**
 ** Count the longest ascending or descending initial sequence.
 ** If ptr[0] > ptr[1], try detecting an descending sequence,
 ** or else, try detecting an ascending sequence.
 **
 * @param env the runtime environment.
 * @param ptr the pointer of begin of sequence.
 * @param len the sequence length.
 * @return the ascending sequence length.
 */
static a_u32 count_run_and_make_ascending(a_henv env, Value* ptr, a_u32 len) {
    assume(len > 0, "count nothing.");

    if (len == 1)
        return 1;

    a_u32 run_hi = 2;

    if (sort_comp(env, ptr[1], ptr[0])) {
        while (run_hi < len && sort_comp(env, ptr[run_hi], ptr[run_hi - 1])) {
            run_hi += 1;
        }
        v_reverse(env, ptr, &ptr[run_hi]);
    }
    else {
        while (run_hi < len && !sort_comp(env, ptr[run_hi], ptr[run_hi - 1])) {
            run_hi += 1;
        }
    }

    return run_hi;
}

static a_u32 binary_backward_insert_point(a_henv env, Value v, Value* ptr, a_u32 begin, a_u32 end) {
    a_u32 l = begin;
    a_u32 h = end;

    assume(l <= h);

    while (l < h) {
        a_u32 m = (l + h) >> 1;
        if (sort_comp(env, v, ptr[m])) {
            h = m;
        }
        else {
            l = m + 1;
        }
    }

    assume(l == h && begin <= l && l <= end);
    return l;
}

static a_u32 binary_forward_insert_point(a_henv env, Value v, Value* ptr, a_u32 begin, a_u32 end) {
    a_u32 l = begin;
    a_u32 h = end;

    assume(l <= h);

    while (l < h) {
        a_u32 m = (l + h) >> 1;
        if (sort_comp(env, ptr[m], v)) {
            l = m + 1;
        }
        else {
            h = m;
        }
    }

    assume(l == h && begin <= l && l <= end);
    return l;
}

static void binary_sort(a_henv env, Value* ptr, a_u32 init, a_u32 len) {
    for (a_u32 i = init; i < len; ++i) {
        Value v = ptr[i];
        a_u32 j = binary_backward_insert_point(env, v, ptr, 0, i);
        v_mov_all_bwd(env, &ptr[j + 1], &ptr[j], i - j);
        v_set(env, &ptr[j], v);
    }
}

/**
 ** Returns the minimum run length for sorting.
 */
static a_u32 get_min_run_len(a_u32 len) {
    a_u32 head_shift = count_leading_zero(len) - MIN_MERGE_SHIFT;
    a_u32 head = len >> head_shift;
    a_u32 tail_mask = ~((~usizec(0)) << head_shift);
    a_u32 tail = (len & tail_mask) != 0 ? 1 : 0;
    return head + tail;
}

static a_u32 gallop_right_forward(a_henv env, Value v, Value* const ptr, a_u32 len) {
    a_u32 last_off = 0;
    a_u32 off = 1;

    /* Find first off that v < ptr[off] (or bound to limit) */
    while (off < len && !sort_comp(env, v, ptr[off])) {
        last_off = off;
        off = (off << 1) + 1;
    }

    return binary_forward_insert_point(env, v, ptr, last_off, min(off, len));
}

static a_u32 gallop_right_backward(a_henv env, Value v, Value* const ptr, a_u32 len) {
    a_u32 last_off = 0;
    a_u32 off = 1;

    /* Find first off that v < ptr[off] (or bound to limit) */
    while (off < len && sort_comp(env, v, ptr[len - off])) {
        last_off = off;
        off = (off << 1) + 1;
    }

    return binary_forward_insert_point(env, v, ptr, len - min(off, len), len - last_off);
}

static a_u32 gallop_left_forward(a_henv env, Value v, Value* ptr, a_u32 len) {
    a_u32 last_off = 0;
    a_u32 off = 1;

    /* Find first off that v < ptr[off] (or bound to limit) */
    while (off < len && !sort_comp(env, v, ptr[off])) {
        last_off = off;
        off = (off << 1) + 1;
    }

    return binary_backward_insert_point(env, v, ptr, last_off, min(off, len));
}

static a_u32 gallop_left_backward(a_henv env, Value v, Value* const ptr, a_u32 len) {
    a_u32 last_off = 0;
    a_u32 off = 1;

    /* Find first off that v < ptr[off] (or bound to limit) */
    while (off < len && sort_comp(env, v, ptr[len - off])) {
        last_off = off;
        off = (off << 1) + 1;
    }

    return binary_backward_insert_point(env, v, ptr, len - min(off, len), len - last_off);
}

static void merge_run_forward(a_henv env, Value* ptr, a_u32 len1_and_off, a_u32 len2, a_u32* p_init_gallop) {
    GList* tmp_list = v_as_list(*api_stack(env, 2));
    assume(len1_and_off <= tmp_list->cap && tmp_list->len == 0);

    Value* tmp_ptr = tmp_list->ptr;
    v_cpy_all(env, tmp_ptr, &ptr[0], len1_and_off);
    tmp_list->len = len1_and_off;

    a_u32 dst = 0;
    a_u32 src1 = 0;
    a_u32 len1 = len1_and_off;
    a_u32 src2 = len1_and_off;

    v_cpy(env, &ptr[dst++], &ptr[src2++]);
    if (--len2 == 0) {
        v_cpy_all(env, &ptr[dst], tmp_ptr, len1);
        tmp_list->len = 0;
        return;
    }
    if (len1 == 1) {
        v_mov_all_fwd(env, &ptr[dst], &ptr[src2], len2);
        v_cpy(env, &ptr[dst + len2], tmp_ptr);
        tmp_list->len = 0;
        return;
    }

    a_u32 min_gallop = *p_init_gallop;
    loop {
        a_u32 cnt1 = 0;
        a_u32 cnt2 = 0;

        do {
            assume(len1 > 0 && len2 > 1);
            if (sort_comp(env, ptr[src2], tmp_ptr[src1])) {
                v_cpy(env, &ptr[dst++], &ptr[src2++]);
                cnt2 += 1;
                cnt1 = 0;
                if (--len2 == 0)
                    goto seq2_end;
            }
            else {
                v_cpy(env, &ptr[dst++], &tmp_ptr[src1++]);
                cnt1 += 1;
                cnt2 = 0;
                if (--len1 == 1)
                    goto seq1_end;
            }
        }
        while ((cnt1 | cnt2) < min_gallop);

        /* Gallop start. */
        do {
            assume(len1 > 0 && len2 > 1);
            cnt1 = gallop_right_forward(env, ptr[src2], &tmp_ptr[src1], len1);
            if (cnt1 > 0) {
                v_cpy_all(env, &ptr[dst], &tmp_ptr[src1], cnt1);
                dst += cnt1;
                src1 += cnt1;
                len1 -= cnt1;
                if (len1 <= 1)
                    goto seq1_end;
            }
            v_cpy(env, &ptr[dst++], &ptr[src2++]);
            if (--len2 == 0)
                goto seq2_end;

            cnt2 = gallop_left_forward(env, tmp_ptr[src1], &ptr[src2], len2);
            if (cnt2 > 0) {
                v_mov_all_fwd(env, &ptr[dst], &ptr[src2], cnt2);
                dst += cnt2;
                src2 += cnt2;
                len2 -= cnt2;
                if (len2 == 0)
                    goto seq2_end;
            }

            v_cpy(env, &ptr[dst++], &tmp_ptr[src1++]);
            if (--len1 == 1)
                goto seq1_end;

            min_gallop -= 1;
        }
        while ((cnt1 >= MIN_GALLOP) | (cnt2 >= MIN_GALLOP));

        min_gallop = min(min_gallop, 0) + 2;
    }

    run seq1_end: {
        if (unlikely(len1 == 0)) {
            aloL_raisef(env, "comparison method violates its contract");
        }
        assume(len2 > 0);
        *p_init_gallop = max(min_gallop, 1);
        v_mov_all_fwd(env, &ptr[dst], &ptr[src2], len2);
        v_cpy(env, &ptr[dst + len2], &tmp_ptr[src1]);
        tmp_list->len = 0;
        return;
    }

    run seq2_end: {
        assume(len1 > 1);
        *p_init_gallop = max(min_gallop, 1);
        v_cpy_all(env, &ptr[dst], &tmp_ptr[src1], len1);
        tmp_list->len = 0;
        return;
    }
}

static void merge_run_backward(a_henv env, Value* ptr, a_u32 len1_and_off, a_u32 len2, a_u32* p_init_gallop) {
    GList* tmp_list = v_as_list(*api_stack(env, 2));
    assume(len2 <= tmp_list->len);

    Value* tmp_ptr = tmp_list->ptr;
    v_cpy_all(env, tmp_ptr, &ptr[len1_and_off], len2);
    tmp_list->len = len2;

    a_u32 dst = len1_and_off + len2 - 1;
    a_u32 src1 = len1_and_off - 1;
    a_u32 len1 = len1_and_off;
    a_u32 src2 = len2 - 1;

    v_cpy(env, &ptr[dst--], &ptr[src1--]);
    if (--len1 == 0) {
        v_cpy_all(env, &ptr[dst - (len2 - 1)], tmp_ptr, len2);
        tmp_list->len = 0;
        return;
    }
    if (len2 == 1) {
        v_mov_all_bwd(env, &ptr[dst - (len2 - 1)], &ptr[src1 - (len1 - 1)], len1);
        v_cpy(env, &ptr[dst - len2], tmp_ptr);
        tmp_list->len = 0;
        return;
    }

    a_u32 min_gallop = *p_init_gallop;
    loop {
        a_u32 cnt1 = 0;
        a_u32 cnt2 = 0;

        do {
            assume(len1 > 0 && len2 > 1);
            if (sort_comp(env, tmp_ptr[src2], ptr[src1])) {
                v_cpy(env, &ptr[dst--], &tmp_ptr[src1--]);
                cnt1 += 1;
                cnt2 = 0;
                if (--len1 == 0)
                    goto seq1_end;
            }
            else {
                v_cpy(env, &ptr[dst--], &ptr[src2--]);
                cnt2 += 1;
                cnt1 = 0;
                if (--len2 == 1)
                    goto seq2_end;
            }
        }
        while ((cnt1 | cnt2) < min_gallop);

        /* Gallop start. */
        do {
            assume(len1 > 0 && len2 > 1);
            cnt1 = len1 - gallop_right_backward(env, tmp_ptr[src2], &ptr[0], len1);
            if (cnt1 > 0) {
                dst -= cnt1;
                src1 -= cnt1;
                len1 -= cnt1;
                v_cpy_all(env, &ptr[dst + 1], &ptr[src1 + 1], cnt1);
                if (len1 == 0)
                    goto seq1_end;
            }
            v_cpy(env, &ptr[dst--], &ptr[src2--]);
            if (--len2 == 1)
                goto seq2_end;

            cnt2 = len2 - gallop_left_backward(env, ptr[src1], &tmp_ptr[src2], len2);
            if (cnt2 > 0) {
                dst -= cnt2;
                src2 -= cnt2;
                len2 -= cnt2;
                v_cpy_all(env, &ptr[dst + 1], &tmp_ptr[src2 + 1], cnt2);
                if (len2 <= 1)
                    goto seq2_end;
            }

            v_cpy(env, &ptr[dst--], &ptr[src1--]);
            if (--len1 == 0)
                goto seq1_end;

            min_gallop -= 1;
        }
        while ((cnt1 >= MIN_GALLOP) | (cnt2 >= MIN_GALLOP));

        min_gallop = min(min_gallop, 0) + 2;
    }

    run seq2_end: {
        if (unlikely(len2 == 0)) {
            aloL_raisef(env, "comparison method violates its contract");
        }
        assume(len1 > 0);
        *p_init_gallop = max(min_gallop, 1);
        v_mov_all_bwd(env, &ptr[dst - (len1 - 1)], &ptr[src1 - (len1 - 1)], len1);
        v_cpy(env, &ptr[dst - len1], &tmp_ptr[src1 - len1]);
        tmp_list->len = 0;
        return;
    }

    run seq1_end: {
        assume(len2 > 0);
        *p_init_gallop = max(min_gallop, 1);
        v_cpy_all(env, &ptr[dst], &tmp_ptr[src2], len2);
        tmp_list->len = 0;
        return;
    }
}

static void merge_run(a_henv env, Value* ptr, a_u32 len1_and_off, a_u32 len2, a_u32* p_init_gallop) {
    a_u32 k;

    k = gallop_right_forward(env, ptr[len1_and_off], ptr, len1_and_off);
    ptr += k;
    len1_and_off -= k;
    if (len1_and_off == 0)
        return;

    len2 = gallop_left_backward(env, ptr[len1_and_off - 1], &ptr[len1_and_off], len2);
    if (len2 == 0) /* Should only happen when compare function don't satisfy 'less than' contract. */
        return;

    if (len1_and_off <= len2)
        merge_run_forward(env, ptr, len1_and_off, len2, p_init_gallop);
    else
        merge_run_backward(env, ptr, len1_and_off, len2, p_init_gallop);
}

static void tim_sort(a_henv env, Value* ptr, a_u32 len) {
    if (len < 2)
        return; /* The list with 0 or 1 length always sorted. */

    if (len < MIN_MERGE) {
        a_u32 run_len = count_run_and_make_ascending(env, ptr, len);
        binary_sort(env, ptr, run_len, len);
        return;
    }

    alo_newlist(env, len / 2); /* Allocate temporary sort space. */
    a_u32 min_run_len = get_min_run_len(len);

    a_u32 run_lens[MAX_RUN_STACK];
    a_u32 run_poses[MAX_RUN_STACK];
    a_u32 nrun = 0;
    a_u32 pos = 0;
    a_u32 rem = len;
    a_u32 init_gallop = MIN_GALLOP;

    do {
        a_u32 run_len = count_run_and_make_ascending(env, &ptr[pos], rem);

        if (run_len < min_run_len) {
            a_u32 force_run_len = max(run_len, rem);
            binary_sort(env, &ptr[pos], run_len, force_run_len);
            run_len = force_run_len;
        }

        run_poses[nrun] = pos;
        run_lens[nrun] = run_len;
        nrun += 1;

        /**
         ** Merge stack till it satisfies following rules:
         ** For all valid index i:
         ** run_lens[i] > run_lens[i + 1] + run_lens[i + 2]
         ** run_lens[i] >= run_lens[i + 1]
         **/
        while (nrun >= 2) {
            if (nrun >= 3 && run_lens[nrun - 3] <= run_lens[nrun - 2] + run_lens[nrun - 1]) {
                if (run_lens[nrun - 3] < run_lens[nrun - 1]) {
                    a_u32 len1 = run_lens[nrun - 3];
                    a_u32 len2 = run_lens[nrun - 2];
                    a_u32 pos1 = run_poses[nrun - 3];
                    run_lens[nrun - 3] = len1 + len2;
                    run_lens[nrun - 2] = run_lens[nrun - 1];
                    run_poses[nrun - 2] = run_poses[nrun - 1];
                    nrun -= 1;
                    merge_run(env, &ptr[pos1], len1, len2, &init_gallop);
                }
                else {
                    goto merge_head;
                }
            }
            else if (run_lens[nrun - 2] < run_lens[nrun - 1]) merge_head: {
                a_u32 len1 = run_lens[nrun - 2];
                a_u32 len2 = run_lens[nrun - 1];
                a_u32 pos1 = run_poses[nrun - 2];
                run_lens[nrun - 2] = len1 + len2;
                nrun -= 1;
                merge_run(env, &ptr[pos1], len1, len2, &init_gallop);
            }
            else {
                break;
            }
        }

        pos += run_len;
        rem -= run_len;
    }
    while (rem > 0);

    a_u32 len2 = run_lens[nrun - 1];
    while (nrun > 1) {
        a_u32 len1 = run_lens[nrun - 2];
        a_u32 pos1 = run_poses[nrun - 2];
        nrun -= 1;
        merge_run(env, &ptr[pos1], len1, len2, &init_gallop);
        len2 += len1;
    }
}

static a_msg list_sort(a_henv env) {
    GList* self = check_self(env);
    alo_settop(env, 2);

    tim_sort(env, self->ptr, self->len);

    alo_settop(env, 1);
    return 1;
}

void aloopen_list(a_henv env) {
    static aloL_Entry const bindings[] = {
        { "__look__", null },
        { "__new__", list___new__ },
        { "__mul__", list_repeat },
        { "clear", list_clear },
        { "get", list_get },
        { "mkstr", list_mkstr },
        { "push", list_push },
        { "repeat", list_repeat },
        { "reverse", list_reverse },
        { "sort", list_sort }
    };

    alo_pushptype(env, ALO_TLIST);
    aloL_putalls(env, -1, bindings);

    alo_push(env, -1);
    aloL_puts(env, -2, "__look__");
}
