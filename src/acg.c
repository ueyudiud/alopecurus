/**
 *@file acg.c
 */

#define acg_c_
#define ALO_LIB

#include "abc.h"
#include "astr.h"
#include "afmt.h"
#include "afun.h"
#include "aop.h"
#include "agc.h"

#include "acg.h"

typedef a_u32 a_reg;
typedef struct {
    a_reg bot;
    a_u32 len;
} a_regs;

#define SCOPE_ROOT u32c(0)

#define R_DYN u32c(0)
#define R_DMB UINT32_MAX
#define N_DYN u32c(1)
#define N_VLR UINT32_MAX

static void expr_reg(Expr* e, a_reg reg, a_line line) {
    assume(reg != R_DMB, "invalid register.");
    e->tag = EXPR_REG;
    e->udat1 = reg;
    e->line = line;
}

static void expr_dyn(Expr* e, a_u32 label, a_line line) {
    assume(label != NO_LABEL);
    e->tag = EXPR_DYN;
    e->udat1 = label;
    e->line = line;
}

/*=========================================================*/

enum {
    /* Mark unused operand. */
    DMB = 0
};

/*=========================================================*/

static a_bool expr_is_const(Expr* e) {
    return expr_match(e, EXPR_UNIT, EXPR_NIL, EXPR_FALSE, EXPR_TRUE, EXPR_INT, EXPR_FLOAT, EXPR_STR);
}

static a_bool expr_has_vararg_top(Expr* e) {
    a_enum k = e->tag;
    assume(k < EXPR__MAX, "invalid expression.");
    return ((u64c(1) << k) & (u64c(1) << EXPR_VCALL | u64c(1) << EXPR_VDYN | u64c(1) << EXPR_VNTMP | u64c(1) << EXPR_VNTMPC)) != 0;
}

/**
 ** Get the idx_sub for constant, the value is distinct by trivial equality.
 *@return the idx_sub of constant in constant pool.
 */
static a_u32 const_index(Parser* par, Value val) {
    ConstBuf* consts = &par->consts;
    a_u32 off = par->fscope->const_off;
    for (a_u32 i = off; i < consts->len; ++i) {
        /*
         * Since all literals which have the same format provided by compiler should
         * have same binary data, use identity equality for comparison.
         */
        if (v_trivial_equals(consts->ptr[i], val)) {
            return i - off;
        }
    }

    if (unlikely(consts->len - off == BC_MAX_BX + 1)) {
        parse_error(par, "too many constants.", 0);
    }

    return at_buf_push(par->env, consts, val, "constant") - off;
}

static Value const_at(Parser* par, a_u32 index) {
    return par->consts.ptr[par->fscope->const_off + index];
}

static a_bool const_is_str(Parser* par, a_u32 index) {
    return v_is_str(const_at(par, index));
}

#define INSN_NOP ((a_insn) 0)

static a_insn* code_at(Parser* par, a_u32 label) {
    assume(label != NO_LABEL);
    return par->code[label];
}

static a_u32 code_put(Parser* par, a_insn i) {
    return at_buf_push(par->env, &par->insns, i, "code");
}

static void emit_line(Parser* par, a_line line) {
    FnScope* scope = par->fscope;
    if (scope->head_line != line) {
        LineInfo info = {UINT32_MAX, line};
        a_u32 index = at_buf_push(par->env, &par->lines, info, "line");
        if (index > scope->line_off) { /* Settle end label for last line info. */
            par->lines.ptr[index - 1].lend = par->insns.len - par->fscope->begin_label;
        }
        scope->head_line = line;
    }
}

static a_u32 emit_direct(Parser* restrict par, a_insn i, a_u32 line) {
    emit_line(par, line);
    return code_put(par, i);
}

#define should_emit(par) (likely((par)->fscope->fpass || (par)->fscope->fland))

static a_i32 compute_jump_diff(Parser* par, a_u32 from, a_u32 to, a_line line) {
    a_i32 diff = cast(a_i32, to - from - 1);
    if (unlikely(diff < BC_MIN_SAX || diff > BC_MAX_SAX)) {
        parse_error(par, "jump range out of bound.", line);
    }
    return diff;
}

static a_u32 emit_jump_direct(Parser* par, a_u32 label, a_line line) {
    a_i32 d = label != NO_LABEL ? compute_jump_diff(par, par->head_label, label, line) : -1;
    return emit_direct(par, bc_make_isax(BC_J, d), line);
}

static a_u32 next_jump(Parser* par, a_u32 label) {
    assume(label <= par->head_label, "not valid label.");
    if (label == par->head_label)
        return par->fscope->head_land;

    a_insn* ip = code_at(par, label);
    assume(bc_load_op(ip) == BC_J);

    a_i32 disp = bc_load_sax(ip);
    return disp != -1 ? label + 1 + cast(a_u32, disp) : NO_LABEL;
}

static void redirect_jump(Parser* par, a_u32 from, a_u32 to, a_line line) {
    a_insn* ip = code_at(par, from);

    assume(bc_load_op(ip) == BC_J);

    bc_store_sax(ip, compute_jump_diff(par, from, to, line));
}

/**
 ** Redirect all jump instruction in jump chain to the current position.
 *@param par the parser.
 *@param from the unresolved jump chain.
 *@param to the destination.
 *@param line the line number for operation.
 */
static void redirect_jump_chain(Parser* par, a_u32 from, a_u32 to, a_line line) {
    loop {
        a_u32 next = next_jump(par, from);
        redirect_jump(par, from, to, line);
        if (next == NO_LABEL) break;
        from = next;
    }
}

static void clear_close(Parser* par) {
    par->fscope->fclose = false;
}

static void clear_jump(Parser* par) {
    par->fscope->fjump = false;
    par->fscope->head_jump = NO_LABEL;
}

static void clear_land(Parser* par) {
    par->fscope->fland = false;
    par->fscope->head_land = NO_LABEL;
}

static void emit_fast(Parser* par, a_insn i, a_line line) {
    if (par->fscope->fpass) {
        emit_direct(par, i, line);
    }
}

static void flush_close(Parser* par) {
    if (par->fscope->fclose) {
        emit_direct(par, bc_make_ia(BC_CLOSE, par->scope->top_pin), par->fscope->close_line);
        clear_close(par);
    }
}

static void flush_jump(Parser *par) {
    if (par->fscope->fjump) {
        assume(par->fscope->fland); /* When jump is defined, the branch is reachable only if land is also defined. */
        /* Link to previous jump instruction. */
        emit_jump_direct(par, par->fscope->head_jump, par->fscope->head_jump_line);
        clear_jump(par);
    }
}

static void flush_land(Parser* par, a_line line) {
    if (par->fscope->fland) {
        redirect_jump_chain(par, par->fscope->head_land, par->head_label, line);
        clear_land(par);
    }
}

static void redirect_leave(Parser* par, a_u32 label, a_insn i) {
    loop {
        a_u32 next = next_jump(par, label);
        bc_store(code_at(par, label), i);
        if (next == NO_LABEL)
            break;
        label = next;
    }
}

/**
 ** Emit an instruction to leave current function.
 */
static void emit_leave(Parser* par, a_insn i, a_line line) {
    if (should_emit(par)) {
        FnScope* scope = par->fscope;

        flush_close(par);
        flush_jump(par);
        if (scope->fland) {
            redirect_leave(par, scope->head_land, i);
            clear_land(par);
        }
        if (scope->fpass) {
            emit_direct(par, i, line);
        }
        scope->fpass = false;
    }
}

static a_u32 emit(Parser* par, a_insn i, a_line line) {
    if (should_emit(par)) {
        flush_close(par);
        flush_jump(par);
        flush_land(par, line);
        par->fscope->fpass = true;
        return emit_direct(par, i, line);
    }
    return NO_LABEL;
}

#define emit_ia(par,i,a,l) emit(par, bc_make_ia(i, a), l)
#define emit_iab(par,i,a,b,l) emit(par, bc_make_iab(i, a, b), l)
#define emit_iac(par,i,a,c,l) emit(par, bc_make_iac(i, a, c), l)
#define emit_iabx(par,i,a,b,l) emit(par, bc_make_iabx(i, a, b), l)
#define emit_iasbx(par,i,a,b,l) emit(par, bc_make_iasbx(i, a, b), l)
#define emit_iabc(par,i,a,b,c,l) emit(par, bc_make_iabc(i, a, b, c), l)
#define emit_iabsc(par,i,a,b,c,l) emit(par, bc_make_iabsc(i, a, b, c), l)

static void emit_idb(Parser* par, a_u32 i, Expr* e, a_u32 b, a_u32 line) {
    expr_dyn(e, emit_iab(par, i, R_DYN, b, line), line);
}

static void emit_idbx(Parser* par, a_u32 i, Expr* e, a_u32 b, a_u32 line) {
    expr_dyn(e, emit_iabx(par, i, R_DYN, b, line), line);
}

static void emit_idbc(Parser* par, a_u32 i, Expr* e, a_u32 b, a_u32 c, a_u32 line) {
    expr_dyn(e, emit_iabc(par, i, R_DYN, b, c, line), line);
}

static void emit_idbd(Parser* par, a_u32 i, Expr* e, a_u32 b, a_u32 line) {
    expr_dyn(e, emit_iabc(par, i, R_DYN, b, N_DYN, line), line);
    e->mupk = true; /* Mark unpackable. */
}

static void emit_idbsc(Parser* par, a_u32 i, Expr* e, a_u32 b, a_i32 c, a_u32 line) {
    expr_dyn(e, emit_iabsc(par, i, R_DYN, b, c, line), line);
}

static a_u32 emit_k(Parser* par, a_u32 dst, Value val, a_line line) {
    a_u32 index = const_index(par, val);
    return emit_iabx(par, BC_K, dst, index, line);
}

static a_u32 emit_kn(Parser* par, a_u32 dst, a_u32 len, a_line line) {
    assume(len > 0);
    if (par->fscope->fpass && !par->fscope->fland) {
        a_insn* ip = code_at(par, par->head_label - 1);
        if (bc_load_op(ip) == BC_KN) {
            a_u32 a = bc_load_a(ip);
            a_u32 c = bc_load_c(ip);
            a_u32 dst1 = dst;
            a_u32 dst2 = dst + len;
            a_u32 src1 = a;
            a_u32 src2 = a + c;
            if (!(dst2 < src1 || src2 < dst1)) {
                a = min(src1, dst1);
                c = max(src2, dst2) - a;
                bc_store_a(ip, a);
                bc_store_c(ip, c);
                return par->head_label - 1;
            }
        }
    }
    return emit_iac(par, BC_KN, dst, len, line);
}

static a_u32 emit_branch(Parser* par, a_insn i, a_u32 label, a_u32 line) {
    if (emit(par, i, line) != NO_LABEL) {
        return emit_jump_direct(par, label, line);
    }
    return NO_LABEL;
}

static a_u32 emit_aby(Parser* par, a_enum op, a_u32 a, a_u32 b, a_u32 c, a_u32 line) {
    if (c <= BC_MAX_C) {
        return emit_iabc(par, op, a, b, c, line);
    }
    else {
        a_u32 label = emit_iab(par, op, a, b, line);
        emit_fast(par, bc_make_iax(BC_EX, c), line);
        return label;
    }
}

static void merge_label(Parser* par, a_u32* plabel, a_u32 label2, a_u32 line) {
    if (label2 == NO_LABEL)
        return;

    a_u32 label1 = *plabel;
    if (label1 == NO_LABEL) {
        *plabel = label2;
    }
    else if (label1 != label2) {
        if (label1 < label2) {
            *plabel = label2;
            swap(label1, label2);
        }

        loop {
            a_u32 label3 = next_jump(par, label1);
            if (label3 == NO_LABEL) {
                redirect_jump(par, label1, label2, line);
                return;
            }
            else if (label3 > label2) {
                label1 = label3;
            }
            else if (label3 < label2) {
                redirect_jump(par, label1, label2, line);
                label1 = label2;
                label2 = label3;
            }
            else {
                return;
            }
        }
    }
}

static a_reg tmps_alloc(Parser* par, a_u32 num, a_u32 line) {
    Scope* scope = par->scope;
    a_u32 reg = scope->top_reg;
    scope->top_reg += num;
    if (scope->top_reg > par->fscope->max_reg) {
        par->fscope->max_reg = scope->top_reg;
        if (reg > BC_MAX_A) {
            parse_error(par, "too many register used.", line);
        }
    }
    return reg;
}

static a_reg tmp_alloc(Parser* par, a_u32 line) {
    return tmps_alloc(par, 1, line);
}

static a_bool reg_is_tmp(Parser* par, a_reg reg) {
    assume(reg < par->scope->top_reg || reg == R_DMB, "register not allocated.");
    return cast(a_i32, reg) >= par->scope->top_pin;
}

/**
 ** Reallocate the register which is already freed, used for phi operation.
 *@param par the parser.
 *@param reg the register to allocate.
 */
static void tmp_realloc(Parser* par, a_reg reg) {
    Scope* scope = par->scope;
    assume(scope->top_reg == reg, "cannot reallocate a using register twice.");
    scope->top_reg += 1;
    assume(scope->top_reg <= par->fscope->max_reg);
}

/**
 ** Free register from temporary value stack.
 ** The register can be freed with the different order with
 ** reversed order of allocation.
 *@param par the parser.
 *@param reg the temporary register.
 */
static void tmp_free(Parser* par, a_reg reg) {
    Scope* scope = par->scope;
    assume(reg + 1 == scope->top_reg && reg_is_tmp(par, reg));
    /* The registers are likely freed with the reversed order of allocation. */
    scope->top_reg = reg;
}

/**
 ** Free a set of registers from temporary value stack, the registers is allocated successively.
 ** The registers must be freed with the order of allocation.
 *@param par the parser.
 *@param reg the first temporary register to free.
 */
static void tmps_free(Parser* par, a_reg reg) {
    Scope* scope = par->scope;
    assume(reg <= par->scope->top_reg);
    scope->top_reg = reg;
}

static void tmps_pin(Parser* par, a_reg reg) {
    Scope* scope = par->scope;
    assume(reg <= scope->top_reg, "cannot store register in place.");
    scope->top_pin = reg;
}

/**
 ** Store temporary value in register to a variable in place, the register will
 ** be dropped until leave the scope.
 */
static void tmp_pin(Parser* par, a_reg reg) {
    Scope* scope = par->scope;
    assume(reg_is_tmp(par, reg) && reg == scope->top_pin, "cannot store register in place.");
    scope->top_pin = reg + 1;
}

/**
 ** Free register if it is temporary register.
 */
static void reg_free(Parser* par, a_reg reg) {
    if (reg_is_tmp(par, reg)) {
        tmp_free(par, reg);
    }
}

/**
 ** Free each register.
 */
static void reg2_free(Parser* par, a_reg reg1, a_reg reg2) {
    Scope* scope = par->scope;
    assume(reg1 != reg2 || !reg_is_tmp(par, reg1) || !reg_is_tmp(par, reg2));
    if (reg1 > reg2) {
        swap(reg1, reg2);
    }
    if (reg_is_tmp(par, reg1)) {
        assume(reg1 + 1 == reg2 && reg2 + 1 == scope->top_reg);
        scope->top_reg = reg1;
    }
    else if (reg_is_tmp(par, reg2)) {
        assume(reg2 + 1 == scope->top_reg);
        scope->top_reg = reg2;
    }
}

/**
 ** Drop allocated register.
 *@param par the parser.
 *@param e the expression.
 */
static void expr_drop(Parser* par, Expr* e) {
    switch (expr_tag(e)) {
        case EXPR_REG:
        case EXPR_REG_OR_NIL: {
            reg_free(par, e->udat1);
            break;
        }
        case EXPR_REF: {
            reg_free(par, e->udat2);
            reg_free(par, e->udat1);
            break;
        }
        case EXPR_VCALL:
        case EXPR_VDYN:
        case EXPR_NTMP:
        case EXPR_NTMPC:
        case EXPR_VNTMP:
        case EXPR_VNTMPC: {
            tmps_free(par, e->udat1);
            break;
        }
    }
}

static void eval_to(Parser* par, Expr* e, a_reg reg);
static a_reg eval_to_top_tmp(Parser* par, Expr* e);
static a_regs exprs_to_top_tmps(Parser* par, Expr* e);
static a_reg eval_to_tmp(Parser* par, Expr* e);
static a_reg eval_to_reg(Parser* par, Expr* e);
static a_reg eval_to_reg_or(Parser* par, Expr* e, a_reg reg);
static a_reg eval_to_reg_and_drop(Parser* par, Expr* e);

static void expr_to_dyn(Parser* par, Expr* e);

static void exprs_fix(Parser* par, Expr* e);

static void test_true(Parser* par, Expr* e, a_u32* plabel, a_u32 line);
static void test_false(Parser* par, Expr* e, a_u32* plabel, a_u32 line);
static a_u32 expr_catch_nil_branch(Parser* par, Expr* e, a_u32 line);

static void resolve_symbol(Parser* par, Expr* e, a_u32 id);

static void capture_locally(unused Parser* par, unused FnScope* scope, Sym* sym, CapInfo* info) {
    switch (sym->tag) {
        case EXPR_REG: {
            info->src_index = sym->udat1;
            break;
        }
        case EXPR_CAP: {
            info->src_index = sym->udat1;
            break;
        }
        default: unreachable();
    }
}

static a_u32 lookup_capture_within(Parser* par, FnScope* fscope, Sym* sym, a_u32 depth) {
    /* Find in captured values. */
    for (a_u32 i = 0; i < fscope->caps->len; ++i) {
        CapInfo* info = &fscope->caps->ptr[i];
        if (info->name == sym->name) {
            /* Already captured. */
            return i;
        }
    }

    /* Not found, create a new capture value. */
    CapInfo info = {
        .scope = sym->scope_depth,
        .name = sym->name
    };
    if (sym->scope_depth >= depth - 1) {
        capture_locally(par, fscope, sym, &info);
    }
    else { /* Capture recursively. */
        info.src_index = lookup_capture_within(par, fscope->fupscope, sym, depth - 1);
    }
    return at_buf_push(par->env, fscope->caps, info, "capture");
}

static a_u32 lookup_capture(Parser* par, Sym* sym) {
    return lookup_capture_within(par, par->fscope, sym, par->scope_depth);
}

#define nosym UINT32_MAX

static void expr_pub_to_index(Parser* par, Expr* e, a_line line) {
    a_u32 id = e->udat1;
    assume(e->tag == EXPR_PUB, "not global variable.");
    resolve_symbol(par, e, par->gvar_index);
    assume(e->tag == EXPR_CAP, "global variable not captured.");
    e->tag = EXPR_REFCK;
    e->udat1 = par->gvar_index;
    e->udat2 = id;
    e->line = line;
}

static void resolve_symbol(Parser* par, Expr* e, a_u32 id) {
    Sym* sym = &par->syms.ptr[id];
    if (sym->scope_depth != par->scope_depth) {
        switch (sym_tag(sym)) {
            case EXPR_REG:
            case EXPR_CAP: {
                e->tag = EXPR_CAP;
                e->mod = sym->mod;
                e->udat1 = lookup_capture(par, sym);
                return;
            }
            case EXPR_PUB: {
                expr_pub_to_index(par, e, e->line);
                return;
            }
            default: break;
        }
    }

    expr_copy(e, &sym->expr);
}

static a_u32 lookup_symbol(Parser* par, GStr* name) {
    SymBuf* syms = &par->syms;
    for (a_u32 i = syms->len; i > 0; --i) {
        a_u32 id = i - 1;
        Sym* sym = &syms->ptr[id];
        if (sym->name == name) {
            assume(id != nosym);
            return id;
        }
    }
    return nosym;
}

/**
 ** Lookup symbol in global scope_depth.
 *@param par the parser.
 *@param e the expression for output.
 *@param name the lookup name.
 *@param line the line number of name reference.
 */
void ai_cg_symbol(Parser* par, Expr* e, GStr* name, a_line line) {
    a_u32 id = lookup_symbol(par, name);
    if (id != nosym) {
        resolve_symbol(par, e, id);
        e->line = line;
    }
    else {
        e->tag = EXPR_PUB;
        e->udat1 = const_index(par, v_of_str(name));
        e->mod = (ExprMod) { };
        e->line = line;
    }
}

void ai_cg_index_int(Parser* par, Expr* e, a_int k, a_line line) {
    a_u32 reg = eval_to_reg(par, e);
    if (k >= 0 && k <= BC_MAX_C) {
        e->tag = EXPR_REFI;
        e->udat1 = reg;
        e->udat2 = cast(a_u32, k);
    }
    else {
        e->tag = EXPR_REFK;
        e->udat1 = reg;
        e->udat2 = const_index(par, v_of_int(k));
    }
    e->line = line;
}

void ai_cg_index_str(Parser* par, Expr* e, GStr* k, a_line line) {
    a_u32 index = const_index(par, v_of_str(k));
    if (expr_tag(e) == EXPR_CAP) {
        e->tag = EXPR_REFCK;
        e->udat2 = index;
    }
    else {
        a_u32 reg = eval_to_reg(par, e);
        e->tag = EXPR_REFK;
        e->udat1 = reg;
        e->udat2 = index;
    }
    e->line = line;
}

void ai_cg_lookup(Parser* par, Expr* e, GStr* k, a_line line) {
    a_u32 reg1 = eval_to_reg_and_drop(par, e);

    a_u32 reg2 = tmps_alloc(par, 2, line);

    emit_aby(par, BC_LOOK, reg2, reg1, const_index(par, v_of_str(k)), line);

    e->tag = EXPR_NTMP;
    e->udat1 = reg2;
    e->line = line;
}

/**
 ** Make reference of indexed expression.
 *@param par the parser.
 *@param ev the view expression.
 *@param ek the key expression.
 *@param line the line of operation.
 */
void ai_cg_index(Parser* par, Expr* ev, Expr* ek, a_line line) {
    switch (ek->tag) {
        case EXPR_INT: {
            ai_cg_index_int(par, ev, ek->idat, line);
            break;
        }
        case EXPR_STR: {
            ai_cg_index_str(par, ev, ek->sdat, line);
            break;
        }
        default: {
            /* Handle by normal expression. */
            a_u32 reg1 = eval_to_reg(par, ev);
            a_u32 reg2 = eval_to_reg(par, ek);
            ev->tag = EXPR_REF;
            ev->udat1 = reg1;
            ev->udat2 = reg2;
            ev->line = line;
            break;
        }
    }
}

/**
 ** Invert the branch at [label-1] and merge residual branch to *plabel.
 */
static void branch_negate(Parser* par, a_u32* plabel, a_u32 label, a_line line) {
    if (label + 1 == par->head_label) {
        /* Try to swap duality opcodes for */
        a_insn* ip = code_at(par, label - 1);

        if (insn_is_branch(ip)) {
            a_u32 op = bc_load_op(ip);
            bc_store_op(ip, op ^ 1);

            a_u32 label1 = next_jump(par, label);
            a_u32 label2 = par->fscope->fland ? par->fscope->head_land : NO_LABEL;

            ai_cg_mark_label(par, label1, line);

            merge_label(par, plabel, label2, line);
            redirect_jump(par, label, *plabel, line);
            *plabel = label;
            return;
        }
    }

    *plabel = ai_cg_jump_lazily(par, *plabel, line);
    ai_cg_mark_label(par, label, line);
}

static void branch_instantiate(Parser* par, Expr* e, a_u32 reg) {
    assume(e->tag == EXPR_TRUE_OR_FALSE || e->tag == EXPR_FALSE_OR_TRUE);
    if (should_emit(par)) {
        assume(par->head_label == e->udat2 + 1);
        assume(insn_is_branch(code_at(par, e->udat2 - 1)));

        a_u32 label = next_jump(par, e->udat2);
        if (label != NO_LABEL) {
            redirect_jump_chain(par, label, e->udat2, e->line);
        }

        /* Swap last instruction. */
        bc_store(code_at(par, par->head_label - 1), bc_make_ia(e->tag == EXPR_TRUE_OR_FALSE ? BC_BKF : BC_BKT, reg));
        emit_ia(par, e->tag == EXPR_TRUE_OR_FALSE ? BC_KT : BC_KF, reg, e->line);
    }
    else {
        emit_ia(par, e->tag == EXPR_TRUE_OR_FALSE ? BC_KF : BC_KT, reg, e->line);
    }
}

void ai_cg_table_new(Parser* par, Expr* e, a_line line) {
    emit_idbx(par, BC_HNEW, e, 0, line);
}

void ai_cg_neg(Parser* par, Expr* e, a_line line) {
    switch (e->tag) {
        case EXPR_UNIT: {
            assume(!should_emit(par));
            break;
        }
        case EXPR_INT: {
            e->idat = ai_op_neg_int(e->idat);
            e->line = line;
            break;
        }
        case EXPR_FLOAT: {
            e->fdat = ai_op_neg_float(e->fdat);
            e->line = line;
            break;
        }
        default: {
            a_u32 reg = eval_to_reg_and_drop(par, e);
            emit_idb(par, BC_NEG, e, reg, line);
            break;
        }
    }
}

void ai_cg_bit_inv(Parser* par, Expr* e, a_line line) {
    switch (e->tag) {
        case EXPR_UNIT: {
            assume(!should_emit(par));
            break;
        }
        case EXPR_INT: {
            e->idat = ai_op_bnot_int(e->idat);
            e->line = line;
            break;
        }
        default: {
            a_u32 reg = eval_to_reg(par, e);
            reg_free(par, reg);
            emit_idb(par, BC_NEG, e, reg, line);
            break;
        }
    }
}

void ai_cg_not(Parser* par, Expr* e, a_line line) {
    switch (e->tag) {
        case EXPR_UNIT: {
            assume(!should_emit(par));
            break;
        }
        case EXPR_NIL:
        case EXPR_FALSE: {
            expr_bool(e, true, line);
            break;
        }
        case EXPR_TRUE:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_STR: {
            expr_bool(e, false, line);
            break;
        }
        case EXPR_TRUE_OR_FALSE: {
            e->tag = EXPR_FALSE_OR_TRUE;
            e->line = line;
            break;
        }
        case EXPR_FALSE_OR_TRUE: {
            e->tag = EXPR_TRUE_OR_FALSE;
            e->line = line;
            break;
        }
        default: {
            a_u32 reg = eval_to_reg_and_drop(par, e);

            a_u32 label = emit_branch(par, bc_make_ia(BC_BNZ, reg), NO_LABEL, line);

            init(e) {
                .tag = EXPR_TRUE_OR_FALSE,
                .udat2 = label,
                .line = line
            };
            break;
        }
    }
}

void ai_cg_unbox(Parser* par, Expr* e, a_line line) {
    a_u32 reg = eval_to_reg_and_drop(par, e);
    emit_idbd(par, BC_UNBOX, e, reg, line);
}

void ai_cg_len(Parser* par, Expr* e, a_line line) {
    a_u32 reg = eval_to_reg_and_drop(par, e);
    emit_idb(par, BC_LEN, e, reg, line);
}

void ai_cg_iter(Parser* par, Expr* e, a_line line) {
    Scope* scope = par->scope;

    a_u32 reg1 = eval_to_reg_and_drop(par, e);

    assume(scope->top_pin == scope->top_reg);

    a_u32 reg2 = tmps_alloc(par, 3, line);

    init(e) {
        .tag = EXPR_NTMP,
        .udat1 = reg2
    };

    emit_iab(par, BC_ITER, reg2, reg1, line);

    scope->top_pin = scope->top_reg;
}

void ai_cg_binary_left(Parser* par, Expr* e, a_enum op, a_line line) {
    switch (op) {
        case OP_ADD:
        case OP_SUB:
        case OP_MUL:
        case OP_DIV:
        case OP_MOD:
        case OP_SHL:
        case OP_SHR:
        case OP_BIT_AND:
        case OP_BIT_OR:
        case OP_BIT_XOR:
        case OP_EQ:
        case OP_NE:
        case OP_LT:
        case OP_LE:
        case OP_GT:
        case OP_GE: {
            if (expr_is_const(e)) {
                return;
            }
            a_reg reg = eval_to_reg(par, e);
            expr_reg(e, reg, e->line);
            break;
        }
        case OP_AND: {
            a_u32 label = NO_LABEL;
            test_true(par, e, &label, line);
            ai_cg_wild(par, e);
            e->udat2 = label;
            break;
        }
        case OP_OR: {
            a_u32 label = NO_LABEL;
            test_false(par, e, &label, line);
            ai_cg_wild(par, e);
            e->udat2 = label;
            break;
        }
        default: unreachable();
    }
}

static a_bool data_are_ints(TConst const* d1, a_int* i1, TConst const* d2, a_int* i2) {
    if (d1->tag == EXPR_INT && d2->tag == EXPR_INT) {
        *i1 = d1->idat;
        *i2 = d2->idat;
        return true;
    }
    return false;
}

static a_bool data_are_floats(TConst const* d1, a_float* i1, TConst const* d2, a_float* i2) {
    if (d1->tag == EXPR_INT) {
        *i1 = cast(a_float, d1->idat);
    }
    else if (d1->tag == EXPR_FLOAT) {
        *i1 = d1->fdat;
    }
    else return false;
    if (d2->tag == EXPR_INT) {
        *i2 = cast(a_float, d2->idat);
    }
    else if (d2->tag == EXPR_FLOAT) {
        *i2 = d2->fdat;
    }
    else return false;
    return true;
}

static a_bool try_fold_int(Parser* par, TConst* d1, TConst* d2, a_u32 op, a_line line) {
    a_int i1, i2;
    if (data_are_ints(d1, &i1, d2, &i2)) {
        switch (op) {
            case OP_ADD:
            case OP_SUB:
            case OP_MUL:
            case OP_SHL:
            case OP_SHR:
            case OP_BIT_AND:
            case OP_BIT_OR:
            case OP_BIT_XOR: {
                d1->tag = EXPR_INT;
                d1->idat = ai_op_bin_int(i1, i2, op);
                break;
            }
            case OP_DIV:
            case OP_MOD: {
                if (unlikely(i2 == 0)) {
                    parse_error(par, "attempt to divide by 0.", line);
                }
                d1->tag = EXPR_INT;
                d1->idat = ai_op_bin_int(i1, i2, op);
                break;
            }
            case OP_EQ:
            case OP_NE:
            case OP_LT:
            case OP_LE:
            case OP_GT:
            case OP_GE: {
                d1->tag = ai_op_cmp_int(i1, i2, op) ? EXPR_TRUE : EXPR_FALSE;
                break;
            }
            default: unreachable();
        }
        return true;
    }
    return false;
}

static a_bool try_fold_float(unused Parser* par, TConst* d1, TConst* d2, a_u32 op, unused a_line line) {
    a_float f1, f2;
    if (data_are_floats(d1, &f1, d2, &f2)) {
        switch (op) {
            case OP_ADD:
            case OP_SUB:
            case OP_MUL:
            case OP_DIV:
            case OP_MOD:
            case OP_POW: {
                d1->tag = EXPR_FLOAT;
                d1->fdat = ai_op_bin_float(f1, f2, op);
                break;
            }
            case OP_EQ:
            case OP_NE:
            case OP_LT:
            case OP_LE:
            case OP_GT:
            case OP_GE: {
                d1->tag = ai_op_cmp_float(f1, f2, op) ? EXPR_TRUE : EXPR_FALSE;
                break;
            }
            default: unreachable();
        }
        return true;
    }
    return false;
}

static a_bool try_compare_int(Parser* par, Expr* e1, Expr* e2, a_u32 bc1, a_u32 bc2, a_line line) {
    if (e2->tag == EXPR_INT && e2->idat >= BC_MIN_SBX && e2->idat <= BC_MAX_SBX) {
        a_u32 reg1 = eval_to_reg_and_drop(par, e1);
        emit_iasbx(par, bc1, reg1, e2->idat, line);
        return false;
    }
    else if (e1->tag == EXPR_INT && e1->idat >= BC_MIN_SBX && e1->idat <= BC_MAX_SBX) {
        a_u32 reg2 = eval_to_reg_and_drop(par, e2);
        emit_iasbx(par, bc2, reg2, e1->idat, line);
        return false;
    }

    return true;
}

static void emit_binary(Parser* par, Expr* e1, Expr* e2, a_u32 bc, a_line line) {
    a_u32 reg2 = eval_to_reg(par, e2);
    a_u32 reg1 = eval_to_reg(par, e1);
    reg2_free(par, reg1, reg2);
    emit_idbc(par, bc, e1, reg1, reg2, line);
}

static void emit_compare(Parser* par, Expr* e1, Expr* e2, a_u32 bc, a_line line) {
    a_u32 reg2 = eval_to_reg(par, e2);
    a_u32 reg1 = eval_to_reg(par, e1);
    reg2_free(par, reg1, reg2);
    emit_iab(par, bc, reg1, reg2, line);
}

/**
 ** Post evaluate expression `e1 op e2` and bind result to e1.
 ** The left hand expression should be a constant or a non-volatile expression.
 *@param par the parser.
 *@param e1 the left hand sub expression.
 *@param e2 the right hand sub expression.
 *@param op the operation.
 *@param line the line number of the operation.
 */
void ai_cg_binary_right(Parser* par, Expr* e1, Expr* e2, a_enum op, a_line line) {
    switch (op) {
        case OP_ADD:
        case OP_SUB:
        case OP_MUL:
        case OP_DIV:
        case OP_MOD: {
            if (try_fold_int(par, &e1->tdat, &e2->tdat, op, line) || try_fold_float(par, &e1->tdat, &e2->tdat, op, line))
                return;
            if (e2->tag == EXPR_INT && e2->idat >= BC_MIN_SC && e2->tag <= BC_MAX_SC) {
                a_u32 reg1 = eval_to_reg_and_drop(par, e1);
                emit_idbsc(par, BC_ADDI + op - OP_ADD, e1, reg1, e2->idat, line);
            }
            else {
                emit_binary(par, e1, e2, BC_ADD + (op - OP_ADD), line);
            }
            break;
        }
        case OP_SHL:
        case OP_SHR:
        case OP_BIT_AND:
        case OP_BIT_OR:
        case OP_BIT_XOR: {
            if (try_fold_int(par, &e1->tdat, &e2->tdat, op, line))
                return;
            if (e2->tag == EXPR_INT && e2->idat >= BC_MIN_SC && e2->tag <= BC_MAX_SC) {
                a_u32 reg1 = eval_to_reg_and_drop(par, e1);
                emit_idbsc(par, BC_SHLI + op - OP_SHL, e1, reg1, e2->idat, line);
            }
            else {
                emit_binary(par, e1, e2, BC_ADD + (op - OP_ADD), line);
            }
            break;
        }
        case OP_EQ: {
            if (try_fold_int(par, &e1->tdat, &e2->tdat, op, line) || try_fold_float(par, &e1->tdat, &e2->tdat, op, line))
                return;
            if (try_compare_int(par, e1, e2, BC_BEQI, BC_BEQI, line)) {
                emit_compare(par, e1, e2, BC_BEQ, line);
            }

            goto try_true;
        }
        case OP_NE: {
            if (try_fold_int(par, &e1->tdat, &e2->tdat, op, line) || try_fold_float(par, &e1->tdat, &e2->tdat, op, line))
                return;
            if (try_compare_int(par, e1, e2, BC_BNEI, BC_BNEI, line)) {
                emit_compare(par, e1, e2, BC_BNE, line);
            }

            goto try_true;
        }
        case OP_LT: {
            if (try_fold_int(par, &e1->tdat, &e2->tdat, op, line) || try_fold_float(par, &e1->tdat, &e2->tdat, op, line))
                return;
            if (try_compare_int(par, e1, e2, BC_BLTI, BC_BGTI, line)) {
                emit_compare(par, e1, e2, BC_BLT, line);
            }
            goto try_true;
        }
        case OP_LE: {
            if (try_fold_int(par, &e1->tdat, &e2->tdat, op, line) || try_fold_float(par, &e1->tdat, &e2->tdat, op, line))
                return;
            if (try_compare_int(par, e1, e2, BC_BLEI, BC_BGEI, line)) {
                emit_compare(par, e1, e2, BC_BLE, line);
            }
            goto try_true;
        }
        case OP_GT: {
            if (try_fold_int(par, &e1->tdat, &e2->tdat, op, line) || try_fold_float(par, &e1->tdat, &e2->tdat, op, line))
                return;
            if (try_compare_int(par, e1, e2, BC_BGTI, BC_BLTI, line)) {
                emit_compare(par, e2, e1, BC_BLT, line);
            }
            goto try_true;
        }
        case OP_GE: {
            if (try_fold_int(par, &e1->tdat, &e2->tdat, op, line) || try_fold_float(par, &e1->tdat, &e2->tdat, op, line))
                return;
            if (try_compare_int(par, e1, e2, BC_BGEI, BC_BLEI, line)) {
                emit_compare(par, e2, e1, BC_BLE, line);
            }
            goto try_true;
        }
        try_true: {
            init(e1) {
                .tag = EXPR_TRUE_OR_FALSE,
                .udat2 = emit_jump_direct(par, NO_LABEL, line),
                .line = line
            };
            break;
        }
        case OP_AND: {
            test_true(par, e2, &e1->udat2, line);
            ai_cg_wild(par, e2);
            if (e1->udat2 == NO_LABEL) {
                e1->tag = EXPR_TRUE;
            }
            else {
                e1->tag = EXPR_TRUE_OR_FALSE;
            }
            break;
        }
        case OP_OR: {
            test_false(par, e2, &e1->udat2, line);
            ai_cg_wild(par, e2);
            if (e1->udat2 == NO_LABEL) {
                e1->tag = EXPR_FALSE;
            }
            else {
                e1->tag = EXPR_FALSE_OR_TRUE;
            }
            break;
        }
        default: unreachable();
    }
}

static void merge_opt_reg(Parser* par, a_u32 label, a_u32 reg, a_line line) {
    a_u32 label2 = ai_cg_jump_lazily(par, NO_LABEL, line);
    ai_cg_mark_label(par, label, line);
    emit_kn(par, reg, 1, line);
    ai_cg_mark_label(par, label2, line);
}

void ai_cg_merge(Parser* par, Expr* e1, Expr* e2, a_u32 label, a_line line) {
    switch (expr_tag(e1)) {
        case EXPR_UNIT: {
            assume(label == NO_LABEL);
            expr_copy(e1, e2);
            break;
        }
        case EXPR_DYN_OR_NIL: {
            a_u32 label2 = expr_catch_nil_branch(par, e2, line);
            merge_label(par, &e1->udat2, label2, line);

            a_u32 reg = eval_to_tmp(par, e2);
            merge_opt_reg(par, e1->udat2, reg, line);

            ai_cg_mark_label(par, label, line);
            tmp_realloc(par, reg);
            expr_reg(e1, reg, line);
            break;
        }
        case EXPR_REG_OR_NIL: {
            a_u32 label2 = expr_catch_nil_branch(par, e2, line);
            merge_label(par, &e1->udat2, label2, line);

            a_u32 reg = e1->udat1;
            eval_to(par, e2, reg);
            tmp_realloc(par, reg);
            merge_opt_reg(par, e1->udat2, reg, line);

            ai_cg_mark_label(par, label, line);
            expr_reg(e1, reg, line);
            break;
        }
        case EXPR_REG: {
            a_u32 reg = e1->udat1;
            assume(reg_is_tmp(par, reg), "temporary register expected.");
            eval_to(par, e2, reg);
            tmp_realloc(par, reg);

            ai_cg_mark_label(par, label, line);
            break;
        }
        case EXPR_DYN: {
            a_u32 reg = tmp_alloc(par, line);
            eval_to(par, e1, reg);
            eval_to(par, e2, reg);

            ai_cg_mark_label(par, label, line);
            expr_reg(e1, reg, line);
            break;
        }
        default: unreachable();
    }
}

void ai_cg_break_and_case(Parser* par, Expr* e, a_u32* plabel, a_line line) {
    a_u32 label = *plabel;
    switch (expr_tag(e)) {
        case EXPR_UNIT: {
            assume(!should_emit(par));
            break;
        }
        case EXPR_REG_OR_NIL: {
            if (!reg_is_tmp(par, e->udat1)) {
                e->tag = EXPR_REG;
                a_u32 label2 = e->udat2;
                expr_to_dyn(par, e);
                e->tag = EXPR_DYN_OR_NIL;
                e->udat2 = label2;
            }
            else {
                tmp_free(par, e->udat1);
            }
            break;
        }
        case EXPR_REG: {
            if (!reg_is_tmp(par, e->udat1)) {
                expr_to_dyn(par, e);
            }
            else {
                /* Free stack for another branch used. */
                /* The ownership will still keep. */
                tmp_free(par, e->udat1);
            }
            break;
        }
        default: {
            expr_to_dyn(par, e);
            break;
        }
    }
    *plabel = ai_cg_jump_lazily(par, label, line);
    ai_cg_mark_label(par, label, line);
}

void ai_cg_merge_nil(Parser* par, Expr* e, a_u32 label, a_line line) {
    if (label == NO_LABEL)
        return;
    switch (expr_tag(e)) {
        case EXPR_UNIT: {
            ai_cg_mark_label(par, label, line);
            expr_nil(e, e->line);
            break;
        }
        case EXPR_REG: {
            e->tag = EXPR_REG_OR_NIL;
            e->udat2 = label;
            break;
        }
        default: {
            expr_to_dyn(par, e);
            fallthrough;
        }
        case EXPR_DYN: {
            e->tag = EXPR_DYN_OR_NIL;
            e->udat2 = label;
            break;
        }
        case EXPR_REG_OR_NIL:
        case EXPR_DYN_OR_NIL: {
            merge_label(par, &e->udat2, label, line);
            break;
        }
    }
    e->line = line;
}

void ai_cg_check_not_nil(Parser* par, Expr* e, a_line line) {
    switch (e->tag) {
        case EXPR_NIL: {
            emit_leave(par, bc_make_i(BC_RET0), line);
            e->tag = EXPR_UNIT;
            break;
        }
        case EXPR_FALSE:
        case EXPR_TRUE:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_STR: {
            break;
        }
        case EXPR_REG_OR_NIL: {
            redirect_leave(par, e->udat2, bc_make_i(BC_RET0));
            emit_ia(par, BC_BNN, e->udat1, line);
            emit_fast(par, bc_make_i(BC_RET0), line);

            e->tag = EXPR_REG;
            break;
        }
        case EXPR_DYN_OR_NIL: {
            redirect_leave(par, e->udat2, bc_make_i(BC_RET0));

            expr_dyn(e, e->udat1, e->line);
            fallthrough;
        }
        case EXPR_DYN: {
            if (e->mupk) {
                a_insn* ip = code_at(par, e->udat1);

                a_u32 reg = par->scope->top_reg;
                bc_store_op(ip, bc_load_op(ip) + 1);
                bc_store_a(ip, reg);
                bc_store_c(ip, DMB);

                emit_ia(par, BC_BNN, reg, line);
                emit_fast(par, bc_make_ia(BC_RETM, reg), line);

                init(e) {
                    .tag = EXPR_REG,
                    .udat1 = reg,
                    .line = line
                };
            }
            else {
                a_u32 reg = eval_to_reg(par, e);

                emit_ia(par, BC_BNN, reg, line);
                emit_fast(par, bc_make_i(BC_RET0), line);
            }
            break;
        }
        case EXPR_CALL: {
            a_insn* ip = code_at(par, e->udat1);

            a_u32 reg = bc_load_a(ip);
            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_c(ip, DMB);

            emit_ia(par, BC_BNN, reg, line);
            emit_fast(par, bc_make_ia(BC_RETM, reg), line);

            init(e) {
                .tag = EXPR_REGS,
                .udat1 = reg,
                .mupk = true,
                .line = line
            };
            break;
        }
        case EXPR_REGS: {
            a_u32 reg = e->udat1;

            emit_ia(par, BC_BNN, reg, line);
            emit_fast(par, bc_make_ia(BC_RETM, reg), line);
            break;
        }
        default: {
            a_u32 reg = eval_to_reg(par, e);

            emit_ia(par, BC_BNN, reg, line);
            emit_fast(par, bc_make_i(BC_RET0), line);
            break;
        }
    }
}

void ai_cg_vec_init(unused Parser* par, Expr* es) {
    expr_unit(es);
}

void ai_cg_vec_push_left(Parser *par, Expr* es) {
    exprs_fix(par, es);
}

void ai_cg_vec_push_right(Parser *par, Expr* es, Expr* e) {
    if (expr_tag(es) == EXPR_UNIT) {
        expr_copy(es, e);
        return;
    }

    switch (expr_tag(e)) {
        case EXPR_UNIT: {
            assume(!should_emit(par));
            break;
        }
        case EXPR_VCALL: {
            a_insn* ip = code_at(par, e->udat1);
            assume(bc_load_a(ip) == par->scope->top_reg, "can not place variable.");

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_c(ip, DMB);

            es->tag = EXPR_VNTMP;
            break;
        }
        case EXPR_VDYN: {
            a_insn* ip = code_at(par, e->udat1);

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_a(ip, par->scope->top_reg);
            bc_store_c(ip, DMB);

            es->tag = EXPR_VNTMP;
            break;
        }
        default: {
            eval_to_top_tmp(par, e);
            break;
        }
    }
}

void ai_cg_vec_pop(Parser* par, Expr* es, Expr* e, a_line line) {
    if (es->tag == EXPR_NTMP) {
        expr_reg(e, par->scope->top_reg - 1, line);
    }
    else {
        assume(es->tag != EXPR_NTMPC);
        expr_copy(e, es);
        expr_unit(es);
    }
}

void ai_cg_tuple_box(Parser* par, Expr* e, a_line line) {
    a_regs regs = exprs_to_top_tmps(par, e);

    if (regs.len == N_VLR) {
        emit_idb(par, BC_TNEWM, e, regs.bot, line);
    }
    else {
        emit_idbc(par, BC_TNEW, e, regs.bot, regs.len, line);
    }

    tmps_free(par, regs.bot);
}

void ai_cg_list_box(Parser* par, Expr* e, a_line line) {
    exprs_fix(par, e);
    switch (e->tag) {
        case EXPR_UNIT: {
            emit_idbx(par, BC_LNEW, e, 0, line);
            break;
        }
        case EXPR_NTMP: {
            a_u32 n = par->scope->top_reg - e->udat1;

            expr_drop(par, e);

            emit_idbc(par, BC_LBOX, e, e->udat1, n, line);
            break;
        }
        case EXPR_NTMPC: {
            a_u32 n = par->scope->top_reg - e->udat1 - 1;
            if (n > 0) {
                emit_iabc(par, BC_LPUSH, e->udat1, e->udat1 + 1, n, line);
                tmps_free(par, e->udat1 + 1);
            }
            expr_reg(e, e->udat1, line);
            break;
        }
        default: unreachable();
    }
}

void ai_cg_call(Parser* par, Expr* e, a_line line) {
    a_regs regs = exprs_to_top_tmps(par, e);

    a_u32 label;
    if (regs.len == N_VLR) {
        label = emit_iac(par, BC_CALLM, regs.bot, N_DYN, line);
    }
    else {
        label = emit_iabc(par, BC_CALL, regs.bot, regs.len, N_DYN, line);
    }

    tmps_free(par, regs.bot);

    if (label != NO_LABEL) {
        e->tag = EXPR_CALL;
        e->udat1 = label;
        e->line = line;
    }
    else {
        e->tag = EXPR_UNIT;
    }
}

void ai_cg_return(Parser* par, Expr* e, a_line line) {
    clear_close(par);
    if (e->tag == EXPR_REG) {
        emit_leave(par, bc_make_iab(BC_RET, e->udat1, 1), line);
    }
    else if (e->tag == EXPR_VCALL) {
        a_insn* ip = code_at(par, e->udat1);
        a_enum op = bc_load_op(ip);

        assume(e->udat1 == par->head_label - 1 && !par->fscope->fjump, "not head label.");
        assume(op == BC_CALL || op == BC_CALLM, "not call operation.");

        bc_store_op(ip, op + (BC_TCALL - BC_CALL));
        bc_store_c(ip, 0);

        par->fscope->fpass = false;
    }
    else {
        a_regs regs = exprs_to_top_tmps(par, e);

        if (regs.len == N_VLR) {
            emit_leave(par, bc_make_ia(BC_RETM, regs.bot), line);
        }
        else if (regs.len == 0) {
            emit_leave(par, bc_make_i(BC_RET0), line);
        }
        else {
            emit_leave(par, bc_make_iab(BC_RET, regs.bot, regs.len), line);
        }

        tmps_free(par, regs.bot);
    }
}

static void exprs_take(Parser* par, Expr* e, a_u32 n, a_line line) {
    assume(e->tag == EXPR_VCALL || e->tag == EXPR_VDYN, "not vararg expressions.");
    a_u32 reg = tmps_alloc(par, n, line);
    a_insn* ip = code_at(par, e->udat1);
    if (e->tag != EXPR_VCALL) {
        bc_store_a(ip, reg);
    }
    else {
        assume(reg == bc_load_a(ip));
    }
    bc_store_c(ip, n);
}

/**
 ** Take expected argument.
 *@param par the parser.
 *@param ei the initial expressions, and become expression pack in result.
 *@param el the last expression, and become the last expression in result.
 *@param n the argument expected.
 *@param line the line number.
 *@return true if success and false for otherwise.
 */
a_u32 ai_cg_vec_trim(Parser* par, Expr* ei, Expr* el, a_u32 n, a_line line) {
    assume(n > 0, "truncate nothing.");
    switch (ei->tag) {
        case EXPR_UNIT: {
            if (expr_has_vararg_top(el)) {
                exprs_take(par, el, n, line);
                expr_reg(el, par->scope->top_reg - 1, line);
                return 0;
            }
            return n - 1;
        }
        case EXPR_NTMP: {
            expr_drop(par, el);
            a_u32 m = par->scope->top_reg - ei->udat1;
            if (m >= n) {
                a_u32 top = ei->udat1 + n;
                tmps_free(par, top);
                ai_cg_wild(par, el);
                expr_reg(el, top - 1, line);
                return 0;
            }
            else if (expr_has_vararg_top(el)) {
                a_u32 top = ei->udat1 + n;
                exprs_take(par, el, n - m, line);
                expr_reg(el, top - 1, line);
                return 0;
            }
            else {
                return n - m - 1;
            }
        }
        case EXPR_NTMPC: {
            ai_cg_vec_push_right(par, ei, el);
            ai_cg_list_box(par, ei, line);
            init(el) {
                .tag = EXPR_REFI,
                .udat1 = ei->udat1,
                .udat2 = n - 1
            };
            return 0;
        }
        default: {
            if (n > 1) {
                if (expr_has_vararg_top(el)) {
                    exprs_take(par, el, n - 1, line);
                    expr_reg(el, par->scope->top_reg - 1, line);
                    return 0;
                }
                return n - 2;
            }

            ai_cg_wild(par, el);

            if (n > 0) {
                expr_copy(el, ei);
                expr_unit(ei);
            }
            else {
                ai_cg_wild(par, ei);
            }
            return 0;
        }
    }
}

static a_bool try_fold_append(Parser* par, Expr* e) {
    switch (e->tag) {
        case EXPR_INT: {
            at_fmt_puti(par->env, par->sbuf, e->idat);
            return true;
        }
        case EXPR_FLOAT: {
            at_fmt_putf(par->env, par->sbuf, e->fdat);
            return true;
        }
        case EXPR_STR: {
            GStr* str = e->sdat;
            at_buf_putls(par->env, par->sbuf, str->ptr, str->len);
            return true;
        }
        default: {
            return false;
        }
    }
}

static GStr* buf_to_str(Parser* par, ConExpr* ce) {
    Buf* buf = par->sbuf;
    GStr* str = ai_lex_to_str(lex(par), (a_lstr) { buf->ptr + ce->off, buf->len - ce->off });
    at_buf_clear(buf);
    return str;
}

void ai_cg_str_init(unused Parser* par, ConExpr* ce, Expr* e, unused a_line line) {
    a_regs regs = exprs_to_top_tmps(par, e);
    ce->expr->tag = regs.len != N_VLR ? EXPR_NTMP : EXPR_NTMPC;
    ce->expr->udat1 = regs.bot;
    ce->expr->line = line;
    ce->off = par->sbuf->len;
}

void ai_cg_str_cat(Parser* par, ConExpr* ce, Expr* e, a_line line) {
    a_bool empty = par->sbuf->len == ce->off;
    if (try_fold_append(par, e)) {
        if (empty) {
            tmp_alloc(par, line);
        }
    }
    else {
        a_regs regs = exprs_to_top_tmps(par, ce->expr);
        if (!empty) {
            GStr* str = buf_to_str(par, ce);
            emit_k(par, regs.bot + regs.len - 1, v_of_str(str), line);
        }
        ai_cg_vec_push_right(par, ce->expr, e);
    }
}

void ai_cg_str_build(Parser* par, ConExpr* ce, Expr* e, a_line line) {
    if (ce->expr->tag == EXPR_UNIT) {
        expr_str(e, buf_to_str(par, ce), line);
    }
    else {
        a_regs regs = exprs_to_top_tmps(par, ce->expr);
        if (par->sbuf->len > ce->off) {
            GStr* str = buf_to_str(par, ce);
            emit_k(par, regs.bot + regs.len - 1, v_of_str(str), line);
        }
        if (regs.len != N_VLR) {
            emit_idbc(par, BC_CAT, e, regs.bot, regs.len, line);
        }
        else {
            emit_idb(par, BC_CATM, e, regs.bot, line);
        }
        tmps_free(par, regs.bot);
    }
}

void ai_cg_unpack(Parser* par, Expr* e, a_line line) {
    if (!e->mupk) {
        parse_error(par, "the expression cannot be unpack.", line);
    }
    e->tag += 1;
    e->mupk = false;
}

static a_insn fetch_leave_or_nop(Parser* par, a_u32 label) {
    if (unlikely(label == par->head_label))
        return INSN_NOP;
    a_insn* ip = code_at(par, label);
    return insn_is_leave(ip) ? *ip : INSN_NOP;
}

/**
 ** Jump to determined label.
 *@param par the parser.
 *@param label the label jump to.
 *@param line the line number.
 */
void ai_cg_jump_direct(Parser* par, a_u32 label, a_line line) {
    if (should_emit(par)) {
        flush_jump(par);

        a_insn i = fetch_leave_or_nop(par, label);
        if (i == INSN_NOP) {
            if (par->fscope->fland) {
                redirect_jump_chain(par, par->fscope->head_land, label, line);
                clear_land(par);
            }
            if (par->fscope->fpass) {
                emit_jump_direct(par, label, line);
                par->fscope->fpass = false;
            }
        }
        else {
            emit_leave(par, i, line);
        }
    }
}

a_u32 ai_cg_jump_lazily(Parser* par, a_u32 label, a_line line) {
    if (should_emit(par)) {
        flush_close(par); /* Force flush close. */
        if (likely(par->fscope->fpass)) {
            if (par->fscope->head_jump == NO_LABEL || label > par->fscope->head_jump) {
                par->fscope->head_jump_line = line;
            }
            par->fscope->fpass = false;
            par->fscope->fjump = true;
            par->fscope->head_jump = label;
            merge_label(par, &par->fscope->head_jump, par->fscope->head_land, line);
            clear_land(par);
            return par->head_label; /* Return next instruction as pseudo label. */
        }
        else {
            assume(par->fscope->fland);
            merge_label(par, &label, par->fscope->head_land, line);
            clear_land(par);
            return label;
        }
    }
    return label;
}

a_u32 ai_cg_mark_label(Parser* par, a_u32 label, a_line line) {
    if (label != NO_LABEL) {
        if (label != par->head_label) { /* If from label is not pseudo head label, merge with head jump label. */
            par->fscope->fland = true;
            merge_label(par, &par->fscope->head_land, label, line);
        }
        else {
            /* Pseudo head jump. */
            assume(par->fscope->fjump && !par->fscope->fpass);
            par->fscope->fpass = true;
            par->fscope->fland = par->fscope->head_jump != NO_LABEL;
            merge_label(par, &par->fscope->head_land, par->fscope->head_jump, line);
            clear_jump(par);
        }
    }
    return par->head_label;
}

a_u32 ai_cg_test_true(Parser* par, Expr* e, a_line line) {
    a_u32 label = NO_LABEL;
    test_true(par, e, &label, line);
    return label;
}

/**
 ** Discard the result of the expression,
 ** also drop the ownership of it.
 * @param par the parser.
 * @param e the expression to be discarded.
 */
void ai_cg_wild(Parser* par, Expr* e) {
    switch (e->tag) {
        case EXPR_DYN: {
            a_insn* ip = code_at(par, e->udat1);
            a_u32 reg = tmp_alloc(par, e->line);
            bc_store_a(ip, reg);
            tmp_free(par, reg);

            break;
        }
        case EXPR_CALL: {
            a_insn* ip = code_at(par, e->udat1);
            bc_store_c(ip, 0);
            break;
        }
        case EXPR_REG:
        case EXPR_REFI:
        case EXPR_REFK: {
            expr_drop(par, e);
            break;
        }
        default: {
            break;
        }
    }
}

static char const* sym_name(Parser* par, Expr* e) {
    switch (expr_tag(e)) {
        case EXPR_PUB: {
            GStr* name = v_as_str(const_at(par, e->udat1));
            return str2ntstr(name);
        }
        case EXPR_REG: {
            for (a_u32 i = par->syms.len; i > par->fscope->local_off; --i) {
                Sym* sym = &par->syms.ptr[i - 1];
                if (sym->tag == e->tag && sym->udat1 == e->udat1) {
                    return str2ntstr(sym->name);
                }
            }
            break;
        }
        case EXPR_CAP: {
            return str2ntstr(par->fscope->caps->ptr[e->udat1].name);
        }
    }
    return "?";
}

static void sym_check_writable(Parser* par, Expr* e, a_line line) {
    if (!e->mod.mmut) {
        parse_error(par, "cannot assign to readonly variable '%s'.", line, sym_name(par, e));
    }
}

void ai_cg_pin(Parser* par, Expr* e1, Expr* e2) {
    assume(e1->tag == EXPR_REG, "value already initialized.");
    eval_to(par, e2, e1->udat1);
}

void ai_cg_assign(Parser* par, Expr* e1, Expr* e2, a_line line) {
    switch (e1->tag) {
        case EXPR_REG: {
            sym_check_writable(par, e1, line);
            ai_cg_pin(par, e1, e2);
            break;
        }
        case EXPR_CAP: {
            sym_check_writable(par, e1, line);
            a_u32 reg = eval_to_reg_and_drop(par, e2);
            emit_iab(par, BC_STC, e1->udat1, reg, line);
            break;
        }
        case EXPR_REF: {
            a_u32 reg = eval_to_reg_and_drop(par, e2);
            emit_iabc(par, BC_SET, reg, e1->udat1, e1->udat2, line);

            expr_drop(par, e1);
            break;
        }
        case EXPR_REFI: {
            a_u32 reg = eval_to_reg_and_drop(par, e2);
            emit_iabsc(par, BC_SETI, reg, e1->udat1, cast(a_i32, e1->udat2), line);

            expr_drop(par, e1);
            break;
        }
        case EXPR_REFK: {
            a_u32 reg = eval_to_reg_and_drop(par, e2);
            emit_aby(par, BC_SETS, reg, e1->udat1, e1->udat2, line);

            expr_drop(par, e1);
            break;
        }
        case EXPR_PUB: {
            sym_check_writable(par, e1, line);
            if (!(par->options & ALO_COMP_OPT_LOSSEN)) {
                GStr* name = v_as_str(const_at(par, e1->udat2));
                parse_error(par, "cannot assign to anonymous variable '%s'", line, str2ntstr(name));
            }
            expr_pub_to_index(par, e1, line);
            fallthrough;
        }
        case EXPR_REFCK: {
            a_u32 reg2 = eval_to_reg(par, e2);

            emit_aby(par, BC_CSETS, reg2, e1->udat1, e1->udat2, line);

            expr_drop(par, e2);
            break;
        }
        default: {
            panic("cannot assign to the expression.");
        }
    }
}

void ai_cg_tbc(Parser* par, Expr* e, a_line line) {
    assume(e->tag == EXPR_REG, "cannot mark a non-register storage variable to-be-closed.");
    emit_ia(par, BC_TBC, e->udat1, line);
}

static a_u32 syms_push(Parser* par, Sym sym) {
    return at_buf_push(par->env, &par->syms, sym, "symbol");
}

static Expr* sym_local(Parser* par, GStr* name, a_u32 reg, a_u32 begin_label, ExprMod mod, a_line line) {
    LocalInfo info = {
        .lbegin = begin_label - par->fscope->begin_label,
        .lend = NO_LABEL, /* Not determined yet. */
        .name = name,
        .reg = reg
    };

    a_u32 idx = at_buf_push(par->env, &par->locals, info, "local variable");

    a_u32 sym = syms_push(par, (Sym) {
        .tag = EXPR_REG,
        .udat1 = reg,
        .udat2 = idx,
        .mod = mod,
        .scope_depth = par->scope_depth,
        .line = line,
        .name = name
    });

    return &par->syms.ptr[sym].expr;
}

void ai_cg_local(Parser* par, Expr* e, GStr* name, ExprMod mod, a_line line) {
    Scope* scope = par->scope;
    assume(scope->top_pin == scope->top_reg, "stack not balanced.");

    a_u32 reg = tmp_alloc(par, line);
    Expr* er = sym_local(par, name, reg, par->head_label, mod, line);
    tmp_pin(par, reg);
    expr_copy(e, er);
}

void ai_cg_export(Parser* par, Expr* e, GStr* name, a_line line) {
    Scope* scope = par->scope;
    assume(scope->top_pin == scope->top_reg, "stack not balanced.");
    Sym sym = {
        .tag = EXPR_PUB,
        .udat1 = const_index(par, v_of_str(name)),
        .mod = { .mmut = true },
        .scope_depth = 0, /* Scope depth always be root. */
        .name = name,
        .line = line
    };
    syms_push(par, sym);
    expr_copy(e, &sym.expr);
}

/**
 ** Compact fragment section.
 *@param par the parser.
 */
void ai_cg_stack_compact(Parser* par) {
    Scope* scope = par->scope;
    assume(scope->top_reg == scope->top_pin, "some temporary register is not freed.");
}

void ai_cg_bind_nils(Parser* par, Pat* pat, a_line line) {
    Scope* scope = par->scope;

    assume(pat->tag == PAT_VARG);

    /* If all node is bind. */
    if (pat->sub == null)
        return;

    if (pat->fcpx) {
        parse_error(par, "nil binding is only available for plain pattern.", line);
    }

    assume(scope->top_pin == scope->top_reg);
    a_u32 num = pat->num_sub;
    a_u32 reg = tmps_alloc(par, num, line);
    a_u32 label = emit_kn(par, reg, num, line);
    for (Pat* pat_child = pat->sub; pat_child != null; pat_child = pat_child->sib) {
        sym_local(par, pat_child->name, reg++, label, (ExprMod) {
            .mmut = pat_child->fmut
        }, pat_child->line);
    }

    scope->top_pin = scope->top_reg;
}

static a_u32 pat_bind_with(Parser* par, Pat* pat, a_u32 bot, Expr* src) {
    switch (pat->tag) {
        case PAT_WILD: {
            return bot;
        }
        case PAT_VAR: {
            eval_to(par, src, bot);
            sym_local(par, pat->name, bot, par->head_label, (ExprMod) {
                .mmut = pat->fmut,
                .muse = pat->fuse
            }, pat->line);
            return bot + 1;
        }
        case PAT_TUPLE: {
            a_u32 line = pat->line;
            a_u32 num = pat->num_sub;
            a_u32 top = bot + pat->num_bwd;
            a_u32 src_reg = eval_to_reg_or(par, src, top);
            if (pat->fcpx && num > 2) {
                emit_iab(par, BC_UNBOXT, top, src_reg, line);

                Pat* sub = pat->sub;
                a_u32 sub_idx = 0;

                while (sub != null) {
                    emit_idbc(par, BC_GETI, src, top, sub_idx, sub->line);
                    bot = pat_bind_with(par, sub, bot, src);

                    sub = sub->sib;
                    sub_idx += 1;
                }
            }
            else {
                emit_iabc(par, BC_UNBOX, top, src_reg, num /* TODO: variable length arguments? */, line);
                for (Pat* sub = pat->sub; sub != null; sub = sub->sib) {
                    expr_reg(src, top, sub->line);
                    bot = pat_bind_with(par, sub, bot, src);
                    top += 1;
                }
            }
            return bot;
        }
        default: panic("not implemented."); //TODO
    }
}

/**
 ** Compute relative slot indices of deconstructed values to be placed.
 *@param pat the deconstruction pattern.
 *@return the reserved stack registers allocation size for pattern.
 */
static a_u32 pat_valloc(Parser* par, Pat* pat) {
    pat->num_bwd = 0;
    pat->num_fwd = 0;
    switch (pat->tag) {
        case PAT_VAR: {
            return 1;
        }
        case PAT_WILD: {
            return 0;
        }
        case PAT_VARG:
        case PAT_TUPLE: {
            Pat* sub = pat->sub;
            a_u32 sub_idx = 0;
            a_u32 num_pin = 0;

            pat->num_bwd = pat->num_sub;
            pat->num_fwd = pat->num_sub;

            loop {
                a_u32 sub_pin = pat_valloc(par, sub);
                a_u32 sub_use = num_pin + sub->num_bwd + sub->num_fwd;
                num_pin += sub_pin;

                a_u32 num_tail = pat->num_sub - sub_idx - 1;
                pat->num_bwd = max(pat->num_bwd, sub_use + sub->num_bwd);
                if (sub->sib == null)
                    break;

                sub = sub->sib;
                sub_idx += 1;
            }

            pat->num_bwd -= pat->num_sub;
            return num_pin;
        }
        default: unreachable();
    }
}

void ai_cg_bind_prepare(Parser* par, Pat* pat) {
    pat_valloc(par, pat);
    tmps_alloc(par, pat->num_bwd, pat->line);
}

void ai_cg_bind_once(Parser* par, Pat* pat, Expr* e) {
    assume(pat != null, "bind to nothing.");

    if (pat->tag == PAT_VAR && !pat->fmut && e->tag <= CONST__MAX) { /* Try fold constant. */
        syms_push(par, (Sym) {
            .tag = e->tag,
            .kdat = e->kdat,
            .name = pat->name,
            .scope_depth = par->scope_depth,
        });
    }
    else {
        a_u32 old_top = par->scope->top_reg - pat->num_bwd;
        a_u32 new_top = pat_bind_with(par, pat, old_top, e);
        tmps_pin(par, new_top - old_top);
        tmps_free(par, par->scope->top_reg - new_top);
    }
}

a_u32 ai_cg_bind_for(Parser* par, Pat* pat, a_line line) {
    assume(pat != null, "bind to nothing.");

    a_u32 reg_itr = par->scope->top_reg - 3; /* The iterator register. */

    Expr e[1];
    a_u32 label = emit_branch(par, bc_make_iabc(BC_NEXTG, R_DYN, reg_itr, N_DYN), NO_LABEL, line);

    init(e) {
        .tag = EXPR_VDYN,
        .udat1 = label - 1
    };

    ai_cg_bind_prepare(par, pat);
    ai_cg_bind_once(par, pat, e);
    return label;
}

void ai_cg_bind_args(Parser* par, Pat* pat, a_usize ctx) {
    a_line line = cast(a_line, ctx);

    a_u32 num_sub = pat->num_sub;
    emit_iac(par, BC_TRIM, 0, num_sub, line); // TODO: support variable argument

    Scope* scope = par->scope;
    scope->top_reg = num_sub;
    scope->top_pin = num_sub;
    par->fscope->nparam = num_sub;

    Pat* sub = pat->sub;
    a_u32 sub_idx = 0;

    Expr e;

    while (sub != null) {
        if (sub->tag == PAT_VAR) {
            sym_local(par, sub->name, sub_idx, par->head_label, (ExprMod) {
                .mmut = pat->fmut,
                .muse = pat->fuse
            }, line);
        }
        else {
            pat_valloc(par, sub);
            expr_reg(&e, sub_idx, pat->line);
            scope->top_reg = pat_bind_with(par, sub, scope->top_reg, &e);
        }

        sub = sub->sib;
        sub_idx += 1;
    }
    scope->top_pin = scope->top_reg;
}

static void scope_push(Parser* par, Scope* scope, a_u32 base, a_line line) {
    init(scope) {
        .upscope = par->scope,
        .bot_reg = base,
        .top_pin = base,
        .top_reg = base,
        .begin_line = line,
        .begin_label = par->head_label,
        .end_label = NO_LABEL,
        .sym_off = par->syms.len
    };
    par->scope = scope;
}

static void scope_pop(Parser* par, a_line line, a_bool leave) {
    Scope* scope = par->scope;
    Scope* upscope = scope->upscope;
    par->scope = upscope;
    run {
        a_u32 bot = scope->sym_off;
        a_u32 top = par->syms.len;
        a_u32 label = par->head_label - par->fscope->begin_label;
        for (a_u32 i = bot; i < top; ++i) {
            Sym* sym = &par->syms.ptr[i];
            switch (sym->tag) {
                case EXPR_REG: {
                    LocalInfo* info = &par->locals.ptr[sym->udat2];
                    info->lend = label;
                    break;
                }
            }
        }
        par->syms.len = bot;
    }
    scope->end_label = ai_cg_mark_label(par, scope->end_label, line);
    if (leave && scope->top_pin != upscope->top_pin) {
        par->fscope->fclose = true;
        par->fscope->close_line = line;
    }
}

void ai_cg_block_start(Parser* par, Scope* scope, a_line line, a_u32 jmp_prop) {
    scope_push(par, scope, par->scope->top_reg, line);
    scope->jmp_prop = jmp_prop;
}

void ai_cg_block_end(Parser* par, a_line line) {
    assume(par->scope->upscope != null);
    scope_pop(par, line, true);
}

void ai_cg_block_end_with(Parser* par, a_line line, Expr* e) {
    assume(par->scope->upscope != null);
    /* TODO: need ensure top value is still alive */
    if (e->tag == EXPR_REG && e->udat1 == par->scope->upscope->top_reg) {
        scope_pop(par, line, true);

        a_u32 reg = tmp_alloc(par, line);
        assume(reg == e->udat1);
        /* Recover drop marker. */
    }
    else {
        expr_to_dyn(par, e);
        scope_pop(par, line, true);
    }
}

void ai_cg_break(Parser* par, GStr* label, a_line line) {
    if (label == null) {
        for (Scope* scope = par->scope; !(scope->jmp_prop & JMP_PROP_BOUND); scope = scope->upscope) {
            if (scope->jmp_prop & JMP_PROP_BREAK) {
                scope->end_label = ai_cg_jump_lazily(par, scope->end_label, line);
                return;
            }
        }
        parse_error(par, "no block for break statement", line);
    }
    else {
        for (Scope* scope = par->scope; !(scope->jmp_prop & JMP_PROP_BOUND); scope = scope->upscope) {
            if (scope->label_name == label) {
                if (scope->jmp_prop & JMP_PROP_BREAK) {
                    scope->end_label = ai_cg_jump_lazily(par, scope->end_label, line);
                    return;
                }
                parse_error(par, "cannot break at label '%s'", line, str2ntstr(label));
            }
        }
        parse_error(par, "label '%s' not found", line, str2ntstr(label));
    }
}

void ai_cg_continue(Parser* par, GStr* label, a_line line) {
    if (label == null) {
        for (Scope* scope = par->scope; !(scope->jmp_prop & JMP_PROP_BOUND); scope = scope->upscope) {
            if (scope->jmp_prop & JMP_PROP_CONTINUE) {
                scope->begin_label = ai_cg_jump_lazily(par, scope->end_label, line);
                return;
            }
        }
        parse_error(par, "no block for continue statement", line);
    }
    else {
        for (Scope* scope = par->scope; !(scope->jmp_prop & JMP_PROP_BOUND); scope = scope->upscope) {
            if (scope->label_name == label) {
                if (scope->jmp_prop & JMP_PROP_CONTINUE) {
                    scope->begin_label = ai_cg_jump_lazily(par, scope->end_label, line);
                    return;
                }
                parse_error(par, "cannot continue at label '%s'", line, str2ntstr(label));
            }
        }
        parse_error(par, "label '%s' not found", line, str2ntstr(label));
    }
}

void ai_cg_func_start(Parser* par, FnScope* fscope, a_line line) {
    if (unlikely(par->scope_depth == UINT8_MAX)) {
        parse_error(par, "function nested level overflow.", line);
    }

    code_put(par, INSN_NOP); /* Add barrier for instruction look ahead. */

    init(fscope) {
        .fupscope = par->fscope,
        .base_subs = cast(GProto**, par->rq.tail),
        .const_off = par->consts.len,
        .line_off = par->lines.len,
        .local_off = par->locals.len,
        .head_jump = NO_LABEL,
        .head_land = NO_LABEL,
        .fpass = true,
        .fland = false,
        .fjump = false
    };
    scope_push(par, &fscope->as_scope, 0, line);
    fscope->as_scope.jmp_prop = JMP_PROP_BOUND;

    par->fscope = fscope;
    par->scope_depth += 1;
}

static GProto* func_end(Parser* par, GStr* name, a_line line) {
    FnScope* fscope = par->fscope;

    clear_close(par);
    emit_leave(par, bc_make_i(BC_RET0), line);

    scope_pop(par, line, false);

    ProtoDesc desc = {
        .nconst = par->consts.len - fscope->const_off,
        .ninsn = par->head_label - fscope->begin_label,
        .nsub = fscope->nsub,
        .nlocal = par->locals.len - fscope->local_off,
        .ncap = fscope->caps->len,
        .nstack = fscope->max_reg,
        .nparam = fscope->nparam,
        .nline = par->lines.len - fscope->line_off,
        .flags =
        (!(par->options & ALO_COMP_OPT_STRIP_DEBUG) ? FUN_FLAG_DEBUG : 0) |
        (fscope->fupscope == null ? FUN_FLAG_UNIQUE : 0)
    };

    GProto* proto = ai_proto_alloc(par->env, &desc);
    if (proto == null) {
        ai_mem_nomem(par->env);
    }

    memcpy(proto->consts, par->consts.ptr + fscope->const_off, sizeof(Value) * desc.nconst);
    memcpy(proto->code, par->insns.ptr + fscope->begin_label, sizeof(a_insn) * desc.ninsn);
    if (!(par->options & ALO_COMP_OPT_STRIP_DEBUG)) {
        proto->dbg_lndef = fscope->begin_line;
        proto->dbg_lnldef = line;
        memcpy(proto->dbg_lines, par->lines.ptr + fscope->line_off, sizeof(LineInfo) * desc.nline);
        memcpy(proto->dbg_locals, par->locals.ptr + fscope->local_off, sizeof(LocalInfo) * desc.nlocal);
    }
    run {
        for (a_u32 i = 0; i < desc.ncap; ++i) {
            CapInfo* cap_info = &fscope->caps->ptr[i];
            init(&proto->caps[i]) {
                .reg = cap_info->src_index,
                .fup = cap_info->scope != par->scope_depth - 1
            };
            if (desc.flags & FUN_FLAG_DEBUG) {
                proto->dbg_cap_names[i] = cap_info->name;
            }
        }
    }
    run { /* Build sub function */
        GProto** src = fscope->base_subs;
        GProto** dst = proto->subs;
        GProto* val = *src;

        GProto** const end = cast(GProto**, par->rq.tail);
        par->rq.tail = cast(a_gclist*, src);

        while (src != end) {
            *dst = val;
            *src = null;

            src = cast(GProto**, &val->gnext);
            val = *src;
            dst += 1;
        }
    }

    proto->dbg_name = name;
    if (desc.flags & FUN_FLAG_DEBUG) {
        proto->dbg_file = from_member(GStr, ptr, par->lex.file);
        proto->dbg_lndef = fscope->begin_line;
        proto->dbg_lnldef = line;
    }

    at_buf_deinit(G(par->env), fscope->caps);

    par->scope = fscope->upscope;
    par->fscope = fscope->fupscope;
    par->scope_depth -= 1;
    par->head_label = fscope->begin_label - 1; /* Drop barrier. */
    par->locals.len = fscope->local_off;
    par->lines.len = fscope->line_off;
    par->rq.tail = cast(a_gclist*, fscope->base_subs);

    rq_push(&par->rq, proto);

    return proto;
}

void ai_cg_func_end(Parser* par, Expr* e, GStr* name, a_line line) {
    GProto* fun = func_end(par, name, line);
    FnScope* scope = par->fscope;
    a_u16 index = scope->nsub ++;
    emit_idbx(par, BC_LDF, e, index, fun->dbg_lndef);
}

static void protos_drop(a_henv env, GProto* proto) {
    for (a_u32 i = 0; i < proto->nsub; ++i) {
        protos_drop(env, proto->subs[i]);
    }
    ai_proto_drop(G(env), proto);
}

static void parser_close(Parser* par) {
    Global* gbl = G(par->env);
    at_buf_deinit(gbl, &par->insns);
    at_buf_deinit(gbl, &par->consts);
    at_buf_deinit(gbl, &par->lines);
    at_buf_deinit(gbl, &par->locals);
    at_buf_deinit(gbl, &par->syms);
    at_buf_deinit(gbl, par->secs);
    at_buf_deinit(gbl, par->sbuf);
    ai_lex_close(&par->lex);
}

static void parser_mark(Global* gbl, void* ctx) {
    Parser* par = ctx;
    run {
        StrSet* set = &par->lex.strs;
        if (set->ptr != null) {
            for (a_u32 i = 0; i <= set->hmask; ++i) {
                GStr* str = set->ptr[i];
                if (str != null) {
                    g_trace(gbl, str);
                }
            }
        }
    }
}

static void parser_except(a_henv env, void* ctx, unused a_msg msg) {
    Parser* par = ctx;
    assume(env == par->env);
    /* Destroy queued prototypes. */
    rq_for(obj, &par->rq) {
        protos_drop(par->env, g_as(GProto, obj));
    }
    for (FnScope* scope = par->fscope; scope != null; scope = scope->fupscope) {
        at_buf_deinit(G(par->env), scope->caps);
    }
    /* Close parser. */
    parser_close(par);
}

static KStack const parse_klass = {
    .tag = ALO_TUSER,
    .flags = KLASS_FLAG_PLAIN | KLASS_FLAG_HIDDEN,
    .name = null,
    .mark = parser_mark,
    .catch = parser_except
};

void ai_cg_parse_init(a_henv env, a_ifun fun, void* ctx, char const* file, GStr* name, a_u32 options, Parser* par) {
    init(par) {
        .klass = &parse_klass,
        .options = options,
        .name = name,
        .scope_depth = SCOPE_ROOT
    };
    ai_lex_init(env, &par->lex, fun, ctx, file);
    rq_init(&par->rq);
}

#define ENV_NAME "_ENV"

void ai_cg_chunk_start(Parser* par) {
    ai_lex_open(lex(par), par->options);

    GStr* gvar_name = ai_lex_to_str(lex(par), nt2lstr(ENV_NAME));

    /* Add predefined environment name. */
    par->gvar_index = syms_push(par, (Sym) {
        .tag = EXPR_CAP,
        .scope_depth = par->scope_depth,
        .mod = {
            .mmut = false /* Predefined environment is always readonly variable. */
        },
        .udat1 = 0,
        .name = gvar_name
    });

    ai_cg_func_start(par, &par->root_scope, 1);
}

static void register_protos(a_henv env, GProto* proto) {
    if (proto->flags & FUN_FLAG_UNIQUE) {
        assume(proto->cache != null, "no unique instance given");
        ai_gc_register_normal(env, proto->cache);
    }
    else {
        assume(proto->gnext == null, "children function not collected");
        ai_gc_register_normal(env, proto);
    }
    for (a_u32 i = 0; i < proto->nsub; ++i) {
        register_protos(env, proto->subs[i]);
    }
}

void ai_cg_chunk_end(Parser* par, a_line line) {
    GProto* proto = func_end(par, par->name, line);
    register_protos(par->env, proto);
    parser_close(par);

    GFun* fun = proto->cache;
    g_set_white(G(par->env), fun);
    *par->pout = fun;
}

/**
 ** Evaluate expression and emit an instruction explicitly.
 *@param par the parser.
 *@param e the expression to move.
 *@param reg the destined register or a placeholder.
 *@return the label of movement instruction.
 */
static a_u32 eval_to_explicit(Parser* par, Expr* e, a_u32 reg) {
    switch (e->tag) {
        case EXPR_UNIT: {
            assume(!should_emit(par));
            return NO_LABEL;
        }
        case EXPR_NIL: {
            return emit_iabc(par, BC_KN, reg, DMB, 1, e->line);
        }
        case EXPR_FALSE: {
            return emit_ia(par, BC_KF, reg, e->line);
        }
        case EXPR_TRUE: {
            return emit_ia(par, BC_KT, reg, e->line);
        }
        case EXPR_INT: {
            if (e->idat >= BC_MIN_SBX && e->idat <= BC_MAX_SBX) {
                return emit_iasbx(par, BC_KI, reg, e->idat, e->line);
            }
            else {
                return emit_iabx(par, BC_K, reg, const_index(par, v_of_int(e->idat)), e->line);
            }
        }
        case EXPR_FLOAT: {
            return emit_iabx(par, BC_K, reg, const_index(par, v_of_float(e->fdat)), e->line);
        }
        case EXPR_STR: {
            return emit_iabx(par, BC_K, reg, const_index(par, v_of_str(e->sdat)), e->line);
        }
        case EXPR_CAP: {
            return emit_iab(par, BC_LDC, reg, e->udat1, e->line);
        }
        case EXPR_REG: {
            return emit_iab(par, BC_MOV, reg, e->udat1, e->line);
        }
        case EXPR_REGS: {
            emit_iac(par, BC_TRIM, e->udat1, 1, e->line);
            return emit_iab(par, BC_MOV, reg, e->udat1, e->line);
        }
        case EXPR_REF: {
            return emit_iabc(par, BC_GET, reg, e->udat1, e->udat2, e->line);
        }
        case EXPR_REFI: {
            return emit_iabsc(par, BC_GETI, reg, e->udat1, cast(a_i32, e->udat2), e->line);
        }
        case EXPR_REFK: {
            a_u32 k = e->udat2;
            if (const_is_str(par, k)) {
                return emit_aby(par, BC_GETS, reg, e->udat1, k, e->line);
            }
            else {
                a_u32 reg2 = tmp_alloc(par, e->line);
                a_u32 label = emit(par, bc_make_iabx(BC_K, reg2, k), e->line);
                if (label != NO_LABEL) {
                    emit_fast(par, bc_make_iabc(BC_GET, reg, e->udat1, reg2), e->line);
                    label += 1;
                }
                tmp_free(par, reg2);
                return label;
            }
        }
        case EXPR_PUB: {
            expr_pub_to_index(par, e, e->line);
            fallthrough;
        }
        case EXPR_REFCK: {
            a_u32 k = e->udat2;
            assume(const_is_str(par, k), "not string index.");
            return emit_aby(par, BC_CGETS, reg, e->udat1, k, e->line);
        }
        case EXPR_DYN: {
            bc_store_a(code_at(par, e->udat1), reg);
            return e->udat1;
        }
        case EXPR_CALL: {
            a_insn* ip = code_at(par, e->udat1);
            return emit_iab(par, BC_MOV, reg, bc_load_a(ip), e->line);
        }
        case EXPR_FALSE_OR_TRUE:
        case EXPR_TRUE_OR_FALSE: {
            a_u32 reg2 = tmp_alloc(par, e->line);
            branch_instantiate(par, e, reg2);
            return emit_iab(par, BC_MOV, reg, reg2, e->line);
        }
        case EXPR_REG_OR_NIL: {
            a_u32 reg2;
            if (reg_is_tmp(par, e->udat1)) {
                reg2 = e->udat1;
            }
            else {
                reg2 = tmp_alloc(par, e->line);
                emit_iab(par, BC_MOV, reg2, e->udat1, e->line);
                tmp_free(par, reg2);
            }
            merge_opt_reg(par, e->udat2, reg2, e->line);
            return emit_iab(par, BC_MOV, reg, reg2, e->line);
        }
        case EXPR_DYN_OR_NIL: {
            a_u32 reg2 = tmp_alloc(par, e->line);
            bc_store_a(code_at(par, e->udat1), reg2);
            tmp_free(par, reg2);
            merge_opt_reg(par, e->udat2, reg2, e->line);
            return emit_iab(par, BC_MOV, reg, reg2, e->line);
        }
        default: {
            panic("cannot move expression with tag: %u.", e->tag);
        }
    }
}

static void eval_to(Parser* par, Expr* e, a_u32 reg) {
    switch (e->tag) {
        case EXPR_NIL: {
            emit_kn(par, reg, 1, e->line);
            break;
        }
        case EXPR_REG: {
            if (e->udat1 != reg) {
                emit_iab(par, BC_MOV, reg, e->udat1, e->line);
            }
            break;
        }
        case EXPR_REGS: {
            emit_iac(par, BC_TRIM, e->udat1, 1, e->line);
            if (e->udat1 != reg) {
                emit_iab(par, BC_MOV, reg, e->udat1, e->line);
            }
            break;
        }
        case EXPR_REG_OR_NIL: {
            if (e->udat1 != reg) {
                emit_iab(par, BC_MOV, reg, e->udat1, e->line);
            }
            merge_opt_reg(par, e->udat2, reg, e->line);
            break;
        }
        case EXPR_DYN_OR_NIL: {
            bc_store_a(code_at(par, e->udat1), reg);
            merge_opt_reg(par, e->udat2, reg, e->line);
            break;
        }
        case EXPR_FALSE_OR_TRUE:
        case EXPR_TRUE_OR_FALSE: {
            branch_instantiate(par, e, reg);
            break;
        }
        case EXPR_CALL: {
            a_u32 reg2 = bc_load_a(code_at(par, e->udat1));
            if (reg != reg2) {
                emit_iab(par, BC_MOV, reg, reg2, e->line);
            }
            break;
        }
        default: {
            eval_to_explicit(par, e, reg);
            break;
        }
    }
}

static a_reg eval_to_top_tmp(Parser* par, Expr* e) {
    expr_drop(par, e);
    a_u32 reg = tmp_alloc(par, e->line);
    eval_to(par, e, reg);
    return reg;
}

static a_reg eval_to_reg(Parser* par, Expr* e) {
    switch (expr_tag(e)) {
        case EXPR_UNIT: {
            return R_DMB;
        }
        case EXPR_REG: {
            return e->udat1;
        }
        case EXPR_REGS: {
            emit_iac(par, BC_TRIM, e->udat1, 1, e->line);
            return e->udat1;
        }
        case EXPR_REG_OR_NIL: {
            a_u32 reg = e->udat1;
            if (reg_is_tmp(par, reg)) {
                merge_opt_reg(par, e->udat2, reg, e->line);
                return reg;
            }
            else {
                a_u32 reg2 = tmp_alloc(par, e->line);
                emit_iab(par, BC_MOV, reg2, reg, e->line);
                merge_opt_reg(par, e->udat2, reg2, e->line);
                return reg2;
            }
            break;
        }
        case EXPR_CALL: {
            a_u32 reg = tmp_alloc(par, e->line);
            assume(reg == bc_load_a(code_at(par, e->udat1)), "vararg are not in the top.");
            return reg;
        }
        default: {
            return eval_to_top_tmp(par, e);
        }
    }
}

static a_reg eval_to_reg_or(Parser* par, Expr* e, a_reg reg) {
    if (expr_tag(e) == EXPR_REG) {
        return e->udat1;
    }
    eval_to(par, e, reg);
    return reg;
}

static a_reg eval_to_reg_and_drop(Parser* par, Expr* e) {
    a_u32 reg = eval_to_reg(par, e);
    reg_free(par, reg);
    return reg;
}

static a_reg eval_to_tmp(Parser* par, Expr* e) {
    if (expr_tag(e) == EXPR_REG && !reg_is_tmp(par, e->udat1)) {
        return e->udat1;
    }
    return eval_to_top_tmp(par, e);
}

static void expr_to_dyn(Parser* par, Expr* e) {
    if (!expr_match(e, EXPR_UNIT, EXPR_DYN)) {
        expr_drop(par, e);
        a_u32 label = eval_to_explicit(par, e, R_DYN);
        if (label != NO_LABEL) {
            expr_dyn(e, label, e->line);
        }
        else {
            e->tag = EXPR_UNIT;
        }
    }
}

static void exprs_fix(Parser* par, Expr* e) {
    switch (e->tag) {
        case EXPR_UNIT: {
            break;
        }
        case EXPR_VDYN: {
            a_insn* ip = code_at(par, e->udat1);

            a_u32 reg = tmp_alloc(par, e->line);

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_a(ip, reg);
            bc_store_c(ip, DMB);

            emit_iab(par, BC_LBOXM, reg, reg, e->line);

            init(e) {
                .tag = EXPR_NTMPC,
                .udat1 = reg
            };
            break;
        }
        case EXPR_VCALL: {
            a_insn* ip = code_at(par, e->udat1);

            a_u32 reg = tmp_alloc(par, e->line);
            assume(reg == bc_load_a(ip), "not top of stack.");

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_c(ip, DMB);

            emit_iab(par, BC_LBOXM, reg, reg, e->line);

            init(e) {
                .tag = EXPR_NTMPC,
                .udat1 = reg
            };
            break;
        }
        case EXPR_VNTMP: {
            a_u32 bot = e->udat1;

            emit_iab(par, BC_LBOXM, bot, bot, e->line);
            tmps_free(par, bot + 1);

            e->tag = EXPR_NTMPC;
            break;
        }
        case EXPR_VNTMPC: {
            a_u32 col = e->udat1;
            a_u32 bot = col + 1;

            emit_iab(par, BC_LPUSHM, col, bot, e->line);
            tmps_free(par, bot);
            break;
        }
        default: {
            a_regs regs = exprs_to_top_tmps(par, e);
            e->tag = regs.len != N_VLR ? EXPR_NTMP : EXPR_NTMPC;
            e->udat1 = regs.bot;
            break;
        }
    }
}

static a_regs exprs_to_top_tmps(Parser* par, Expr* e) {
    switch (e->tag) {
        case EXPR_UNIT: {
            return (a_regs) { par->scope->top_reg, 0 };
        }
        case EXPR_NTMP: {
            return (a_regs) { e->udat1, par->scope->top_reg - e->udat1 };
        }
        case EXPR_VNTMP: {
            return (a_regs) { e->udat1, N_VLR };
        }
        case EXPR_NTMPC: {
            a_u32 col = e->udat1;
            a_u32 bot = col + 1;
            a_u32 top = par->scope->top_reg;

            if (bot < top) {
                emit_iabc(par, BC_LPUSH, col, bot, top - bot, e->line);
                tmps_free(par, bot);
            }

            emit_iab(par, BC_UNBOXV, col, col, e->line);

            return (a_regs) { e->udat1, N_VLR };
        }
        case EXPR_VNTMPC: {
            a_u32 col = e->udat1;
            a_u32 bot = col + 1;
            a_u32 top = par->scope->top_reg;

            if (bot < top) {
                emit_iab(par, BC_LPUSHM, col, bot, e->line);
                tmps_free(par, bot);
            }

            emit_iab(par, BC_UNBOXV, col, col, e->line);

            return (a_regs) { e->udat1, N_VLR };
        }
        case EXPR_VDYN: {
            a_insn* ip = code_at(par, e->udat1);
            a_u32 reg = par->scope->top_reg;

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_a(ip, reg);
            bc_store_c(ip, DMB);

            return (a_regs) { reg, N_VLR };
        }
        case EXPR_VCALL: {
            a_insn* ip = code_at(par, e->udat1);
            a_u32 reg = bc_load_a(ip);

            bc_store_op(ip, bc_load_op(ip) + 1);
            bc_store_c(ip, DMB);

            return (a_regs) { reg, N_VLR };
        }
        default: {
            a_u32 reg = eval_to_top_tmp(par, e);
            return (a_regs) { reg, 1 };
        }
    }
}

static void test_true(Parser* par, Expr* e, a_u32* plabel, a_u32 line) {
    switch (expr_tag(e)) {
        case EXPR_UNIT:
        case EXPR_TRUE:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_STR: {
            break;
        }
        case EXPR_NIL:
        case EXPR_FALSE: {
            *plabel = ai_cg_jump_lazily(par, *plabel, line);
            break;
        }
        case EXPR_TRUE_OR_FALSE: {
            merge_label(par, plabel, e->udat2, line);
            break;
        }
        case EXPR_FALSE_OR_TRUE: {
            branch_negate(par, plabel, e->udat2, line);
            break;
        }
        case EXPR_REG_OR_NIL: {
            merge_label(par, plabel, e->udat2, line);
            e->tag = EXPR_REG;
            *plabel = emit_branch(par, bc_make_ia(BC_BZ, e->udat1), *plabel, line);
            break;
        }
        case EXPR_DYN_OR_NIL: {
            merge_label(par, plabel, e->udat2, line);
            expr_dyn(e, e->udat1, e->line);
            fallthrough;
        }
        default: {
            a_reg reg = eval_to_reg(par, e);
            expr_reg(e, reg, line);
            *plabel = emit_branch(par, bc_make_ia(BC_BZ, e->udat1), *plabel, line);
            break;
        }
    }
}

static void test_false(Parser* par, Expr* e, a_u32* plabel, a_u32 line) {
    switch (expr_tag(e)) {
        case EXPR_UNIT:
        case EXPR_NIL:
        case EXPR_FALSE: {
            break;
        }
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_STR:
        case EXPR_TRUE: {
            *plabel = ai_cg_jump_lazily(par, *plabel, line);
            break;
        }
        case EXPR_FALSE_OR_TRUE: {
            merge_label(par, plabel, e->udat2, line);
            break;
        }
        case EXPR_TRUE_OR_FALSE: {
            branch_negate(par, plabel, e->udat2, line);
            break;
        }
        case EXPR_REG_OR_NIL: {
            *plabel = emit_branch(par, bc_make_ia(BC_BNZ, e->udat1), *plabel, line);
            merge_label(par, plabel, e->udat2, line);
            e->tag = EXPR_REG;
            break;
        }
        case EXPR_DYN_OR_NIL: {
            merge_label(par, plabel, e->udat2, line);
            expr_dyn(e, e->udat1, e->line);
            fallthrough;
        }
        default: {
            a_reg reg = eval_to_reg(par, e);
            expr_reg(e, reg, line);
            *plabel = emit_branch(par, bc_make_ia(BC_BNZ, e->udat1), *plabel, line);
            break;
        }
    }
}

void ai_cg_test_eq_nil(Parser* par, Expr* e, a_u32* plabel, a_u32 line) {
    switch (e->tag) {
        case EXPR_TRUE:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_STR:
        case EXPR_FALSE: {
            *plabel = ai_cg_jump_lazily(par, *plabel, line);
            break;
        }
        case EXPR_NIL: {
            break;
        }
        case EXPR_REG_OR_NIL: {
            *plabel = emit_branch(par, bc_make_ia(BC_BN, e->udat1), *plabel, line);
            ai_cg_mark_label(par, e->udat2, line);

            e->tag = EXPR_REG;
            break;
        }
        case EXPR_DYN_OR_NIL: {
            a_u32 label = e->udat2;

            expr_dyn(e, e->udat1, e->line);
            eval_to_reg(par, e);

            assume(e->tag == EXPR_REG);
            *plabel = emit_branch(par, bc_make_ia(BC_BN, e->udat1), *plabel, line);
            ai_cg_mark_label(par, label, line);

            break;
        }
        default: {
            eval_to_reg(par, e);

            assume(e->tag == EXPR_REG);
            *plabel = emit_branch(par, bc_make_ia(BC_BN, e->udat1), *plabel, line);

            break;
        }
    }
}

void ai_cg_test_ne_nil(Parser* par, Expr* e, a_u32* plabel, a_u32 line) {
    switch (e->tag) {
        case EXPR_TRUE:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_STR:
        case EXPR_FALSE: {
            break;
        }
        case EXPR_NIL: {
            *plabel = ai_cg_jump_lazily(par, *plabel, line);
            e->tag = EXPR_UNIT;
            break;
        }
        case EXPR_REG_OR_NIL: {
            merge_label(par, plabel, e->udat2, line);
            *plabel = emit_branch(par, bc_make_ia(BC_BNN, e->udat1), *plabel, line);

            e->tag = EXPR_REG;
            break;
        }
        case EXPR_DYN_OR_NIL: {
            merge_label(par, plabel, e->udat2, line);

            expr_dyn(e, e->udat1, e->line);
            fallthrough;
        }
        default: {
            eval_to_reg(par, e);
            assume(e->tag == EXPR_REG);
            *plabel = emit_branch(par, bc_make_ia(BC_BNN, e->udat1), *plabel, line);
            break;
        }
    }
}

static a_u32 expr_catch_nil_branch(Parser* par, Expr* e, a_u32 line) {
    switch (e->tag) {
        case EXPR_NIL: {
            a_u32 label = ai_cg_jump_lazily(par, NO_LABEL, line);
            e->tag = EXPR_NIL;
            return label;
        }
        case EXPR_DYN_OR_NIL: {
            a_u32 label = e->udat2;
            expr_dyn(e, e->udat1, e->line);
            return label;
        }
        case EXPR_REG_OR_NIL: {
            a_u32 label = e->udat2;
            e->tag = EXPR_REG;
            return label;
        }
        default: {
            return NO_LABEL;
        }
    }
}
