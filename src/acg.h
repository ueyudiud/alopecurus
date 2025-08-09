/**
 *@file acg.h
 */

#ifndef acg_h_
#define acg_h_

#include "aparse.h"

typedef struct ExprMod ExprMod;
typedef struct Expr Expr;
typedef struct ConExpr ConExpr;
typedef struct Pat Pat;
typedef struct PatInfo PatInfo;

intern a_u32 ai_cg_mark_label(Parser* par, a_u32 label, a_line line);
intern void ai_cg_jump_direct(Parser* par, a_u32 label, a_line line);
intern a_u32 ai_cg_jump_lazily(Parser* par, a_u32 label, a_line line);

intern void ai_cg_local(Parser* par, Expr* e, GStr* name, ExprMod mod, a_line line);
intern void ai_cg_symbol(Parser* par, Expr* e, GStr* name, a_line line);
intern void ai_cg_table_new(Parser* par, Expr* e, a_line line);
intern void ai_cg_tuple_box(Parser* par, Expr* e, a_line line);
intern void ai_cg_list_box(Parser* par, Expr* e, a_line line);
intern void ai_cg_bind_prepare(Parser* par, Pat* pat);
intern void ai_cg_bind_once(Parser* par, Pat* pat, Expr* e);
intern void ai_cg_bind_args(Parser* par, Pat* pat, a_usize ctx);
intern void ai_cg_bind_nils(Parser* par, Pat* pat, a_line line);
intern void ai_cg_stack_compact(Parser* par);
intern a_u32 ai_cg_bind_for(Parser* par, Pat* pat, a_line line);
intern void ai_cg_pin(Parser* par, Expr* e1, Expr* e2);

intern void ai_cg_tbc(Parser* par, Expr* e, a_line line);
intern void ai_cg_neg(Parser* par, Expr* e, a_line line);
intern void ai_cg_bit_inv(Parser* par, Expr* e, a_line line);
intern void ai_cg_not(Parser* par, Expr* e, a_line line);
intern void ai_cg_len(Parser* par, Expr* e, a_line line);
intern void ai_cg_iter(Parser* par, Expr* e, a_line line);
intern void ai_cg_binary_left(Parser* par, Expr* e, a_enum op, a_line line);
intern void ai_cg_binary_right(Parser* par, Expr* e1, Expr* e2, a_enum op, a_line line);
intern void ai_cg_index_int(Parser* par, Expr* e, a_int k, a_line line);
intern void ai_cg_index_str(Parser* par, Expr* e, GStr* k, a_line line);
intern void ai_cg_index(Parser* par, Expr* ev, Expr* ek, a_line line);
intern void ai_cg_lookup(Parser* par, Expr* e, GStr* k, a_line line);
intern void ai_cg_call(Parser* par, Expr* e, a_line line);
intern void ai_cg_unbox(Parser* par, Expr* e, a_line line);
intern void ai_cg_unpack(Parser* par, Expr* e, a_line line);
intern void ai_cg_str_init(Parser* par, ConExpr* ce, Expr* e, a_line line);
intern void ai_cg_str_cat(Parser* par, ConExpr* ce, Expr* e, a_line line);
intern void ai_cg_str_build(Parser* par, ConExpr* ce, Expr* e, a_line line);
intern void ai_cg_vec_init(Parser* par, Expr* es);
intern void ai_cg_vec_push_left(Parser *par, Expr* es);
intern void ai_cg_vec_push_right(Parser *par, Expr* es, Expr* e);
intern void ai_cg_vec_pop(Parser* par, Expr* es, Expr* e, a_line line);
intern a_u32 ai_cg_vec_trim(Parser* par, Expr* ei, Expr* el, a_u32 n, a_line line);

intern a_u32 ai_cg_test_true(Parser* par, Expr* e, a_line line);
intern void ai_cg_test_eq_nil(Parser* par, Expr* e, a_u32* plabel, a_u32 line);
intern void ai_cg_test_ne_nil(Parser* par, Expr* e, a_u32* plabel, a_u32 line);
intern void ai_cg_check_not_nil(Parser* par, Expr* e, a_line line);
intern void ai_cg_merge(Parser* par, Expr* e1, Expr* e2, a_u32 label, a_line line);
intern void ai_cg_merge_nil(Parser* par, Expr* e, a_u32 label, a_line line);

intern void ai_cg_wild(Parser* par, Expr* e);
intern void ai_cg_assign(Parser* par, Expr* e1, Expr* e2, a_line line);
intern void ai_cg_return(Parser* par, Expr* e, a_line line);
intern void ai_cg_break(Parser* par, GStr* label, a_line line);
intern void ai_cg_break_and_case(Parser* par, Expr* e, a_u32* plabel, a_line line);
intern void ai_cg_continue(Parser* par, GStr* label, a_line line);
intern void ai_cg_export(Parser* par, Expr* e, GStr* name, a_line line);

intern void ai_cg_parse_init(a_henv env, a_ifun fun, void* ctx, char const* file, GStr* name, a_u32 options, Parser* par);
intern void ai_cg_chunk_start(Parser* par);
intern void ai_cg_chunk_end(Parser* par, a_line line);
intern void ai_cg_func_start(Parser* par, FnScope* fscope, a_line line);
intern void ai_cg_func_end(Parser* par, Expr* expr, GStr* name, a_line line);
intern void ai_cg_block_start(Parser* par, Scope* scope, a_line line, a_u32 jmp_prop);
intern void ai_cg_block_end(Parser* par, a_line line);
intern void ai_cg_block_end_with(Parser* par, a_line line, Expr* e);

enum ETag {
    EXPR_UNIT,
    /* Constants */
    EXPR_NIL,
    EXPR_FALSE,
    EXPR_TRUE,
    EXPR_INT,
    EXPR_FLOAT,
    EXPR_STR,
    /* Symbols */
    EXPR_REG,
    EXPR_CAP,
    EXPR_PUB,
    EXPR_VARG,
    EXPR_REGS,
    EXPR_REF,
    EXPR_REFI,
    EXPR_REFK,
    EXPR_REFCK,
    EXPR_DYN,
    EXPR_VDYN,
    EXPR_CALL,
    EXPR_VCALL,

    EXPR_REG_OR_NIL,
    EXPR_DYN_OR_NIL,

    EXPR_FALSE_OR_TRUE,
    EXPR_TRUE_OR_FALSE,

    EXPR_NTMP,
    EXPR_VNTMP,
    EXPR_NTMPC,
    EXPR_VNTMPC,

    CONST__MAX = EXPR_STR,
    SYM__MAX = EXPR_PUB,
    EXPR__MAX = EXPR_VNTMPC,
};

enum PTag {
    /**
     ** Wildcard pattern:
     ** The accepted value will be discarded.
     */
    PAT_WILD,
    /**
     ** Variable pattern:
     ** The accepted value will be bind to a new variable.
     */
    PAT_VAR,
    /**
     ** The variable length pattern:
     ** Will accept multiple values. Those values will bind to sub patterns.
     ** This pattern cannot be a non-root pattern.
     */
    PAT_VARG,
    PAT_TUPLE,
    PAT_LIST,
    PAT_TABLE,
};

static_assert(EXPR_DYN + 1 == EXPR_VDYN);
static_assert(EXPR_CALL + 1 == EXPR_VCALL);
static_assert(EXPR_NTMP + 1 == EXPR_VNTMP);
static_assert(EXPR_NTMPC + 1 == EXPR_VNTMPC);

#define UDATA_UNION \
    a_int idat;     \
    a_float fdat;   \
    GStr* sdat

#define EXPR_MOD_STRUCT \
    a_u8 mmut: 1;       \
    a_u8 muse: 1;       \
    a_u8 mupk: 1

typedef union {
    UDATA_UNION;
} Const;

typedef struct {
    union {
        UDATA_UNION;
        Const kdat;
    };
    a_u8 tag;
} TConst;

struct ExprMod {
    EXPR_MOD_STRUCT;
};

struct Expr {
    union {
        struct {
            union {
                UDATA_UNION;
                Const kdat;
                struct { /* Universal data */
                    a_u32 udat1;
                    a_u32 udat2;
                };
            };
            a_u8 tag;
            union {
                struct {
                    EXPR_MOD_STRUCT;
                };
                ExprMod mod;
            };
            a_line line;
        };
        TConst tdat;
    };
};

/**
 ** Storage compile-time metadata of named symbol in chunk.
 */
struct Sym {
    union {
        Expr expr;
        struct {
            union {
                struct {
                    a_u32 udat1;
                    a_u32 udat2;
                };
                Const kdat;
            };
            a_u8 tag; /* The tag of symbol tag. */
            union {
                struct {
                    EXPR_MOD_STRUCT;
                };
                ExprMod mod;
            };
            a_u8 scope_depth; /* The scope_depth of symbol belongs to. */
            a_line line;
        };
        TConst tdat;
    };
    GStr* name; /* The symbol name. */
};

struct ConExpr {
    Expr expr[1];
    a_usize off;
};

struct Pat {
    Pat* up;
    Pat* sib;
    union {
        Pat* sub;
        GStr* name;
    };

    a_line line;
    a_u8 tag;
    a_u8 num_sub;
    a_u8 num_bwd;
    a_u8 num_fwd;
    union {
        a_u8 flags;
        struct {
            a_u8 fmut: 1;
            a_u8 fuse: 1;
            a_u8 fcpx: 1;
            a_u8 fdfl: 1;
        };
    };

};

struct PatInfo {
    Pat root;
    void (*con)(Parser*, Pat*, a_usize);
    a_usize ctx;
};

#define JMP_PROP_BREAK 0x01
#define JMP_PROP_CONTINUE 0x02
#define JMP_PROP_BOUND 0x04

struct CapInfo {
    a_u8 scope; /* The depth of first captured as_scope. */
    a_u8 src_index;
    GStr* name;
};

always_inline void expr_copy(Expr* dst, Expr* src) {
    memcpy(dst, src, sizeof(Expr));
}

always_inline void expr_unit(Expr* e) {
    e->tag = EXPR_UNIT;
}

always_inline void expr_nil(Expr* e, a_line line) {
    e->tag = EXPR_NIL;
    e->line = line;
}

always_inline void expr_bool(Expr* e, a_bool val, a_line line) {
    e->tag = val ? EXPR_TRUE : EXPR_FALSE;
    e->line = line;
}

always_inline void expr_int(Expr* e, a_int val, a_line line) {
    e->tag = EXPR_INT;
    e->idat = val;
    e->line = line;
}

always_inline void expr_float(Expr* e, a_float val, a_line line) {
    e->tag = EXPR_FLOAT;
    e->fdat = val;
    e->line = line;
}

always_inline void expr_str(Expr* e, GStr* val, a_line line) {
    e->tag = EXPR_STR;
    e->sdat = val;
    e->line = line;
}

always_inline a_enum sym_tag(Sym* sym) {
    a_enum tag = sym->tag;
    assume(tag <= SYM__MAX, "bad sym tag.");
    return tag;
}

always_inline a_enum expr_tag(Expr* e) {
    a_enum tag = e->tag;
    assume(tag <= EXPR__MAX, "bad expr tag.");
    return tag;
}

#define EXPR_TAG_MASK(t) (u64c(1) << (t))

always_inline a_u64 expr_tag_mask(a_enum const src[], a_usize len) {
    a_u64 mask = 0;
    for (a_usize i = 0; i < len; ++i) {
        mask |= EXPR_TAG_MASK(src[i]);
    }
    return mask;
}

#define expr_match(e,ts...) ((EXPR_TAG_MASK(expr_tag(e)) & expr_tag_mask((a_enum[]) { ts }, sizeof((a_enum[]) { ts }) / sizeof(a_enum))) != 0)

#endif /* acg_h_ */
