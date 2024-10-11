/**
 *@file asym.h
 */

#ifndef asym_h_
#define asym_h_

#define SYM_NIL(_d,_b,_e) \
_b(NIL)                   \
_d(NIL, "")               \
_e(NIL)

#define SYM_KW(_d,_b,_e) \
_b(KW)                   \
_d(as      , "as"      ) \
_d(break   , "break"   ) \
_d(case    , "case"    ) \
_d(const   , "const"   ) \
_d(continue, "continue") \
_d(do      , "do"      ) \
_d(else    , "else"    ) \
_d(false   , "false"   ) \
_d(fn      , "fn"      ) \
_d(for     , "for"     ) \
_d(if      , "if"      ) \
_d(import  , "import"  ) \
_d(in      , "in"      ) \
_d(is      , "is"      ) \
_d(let     , "let"     ) \
_d(loop    , "loop"    ) \
_d(match   , "match"   ) \
_d(mut     , "mut"     ) \
_d(nil     , "nil"     ) \
_d(pub     , "pub"     ) \
_d(return  , "return"  ) \
_d(true    , "true"    ) \
_d(try     , "try"     ) \
_d(use     , "use"     ) \
_d(while   , "while"   ) \
_d(_       , "_"       ) \
_e(KW)

#define SYM_TM(_d,_b,_e) \
_b(TM)                   \
_d(__get__  , "__get__"  ) \
_d(__set__  , "__set__"  ) \
_d(__look__ , "__look__" ) \
_d(__len__  , "__len__"  ) \
_d(__close__, "__close__") \
_d(__hash__ , "__hash__" ) \
_d(__eq__   , "__eq__"   ) \
_d(__lt__   , "__lt__"   ) \
_d(__le__   , "__le__"   ) \
_d(__in__   , "__in__"   ) \
_d(__add__  , "__add__"  ) \
_d(__sub__  , "__sub__"  ) \
_d(__mul__  , "__mul__"  ) \
_d(__div__  , "__div__"  ) \
_d(__mod__  , "__mod__"  ) \
_d(__shl__  , "__shl__"  ) \
_d(__shr__  , "__shr__"  ) \
_d(__band__ , "__band__" ) \
_d(__bor__  , "__bor__"  ) \
_d(__bxor__ , "__bxor__" ) \
_d(__neg__  , "__neg__"  ) \
_d(__bnot__ , "__bnot__" ) \
_d(__call__ , "__call__" ) \
_d(__iter__ , "__iter__" ) \
_d(__next__ , "__next__" ) \
_d(__str__  , "__str__"  ) \
_e(TM)

#define SYM_EM(_d,_b,_e) \
_b(EM)                   \
_d(NOMEM    , "out of memory.") \
_e(EM)

#define SYM_OP(_d,_b,_e) \
_b(OP)                   \
_d(LBK      , "'('"          ) \
_d(RBK      , "')'"          ) \
_d(LSQ      , "'['"          ) \
_d(RSQ      , "']'"          ) \
_d(LBR      , "'{'"          ) \
_d(RBR      , "'}'"          ) \
_d(SHARP    , "'#'"          ) \
_d(AT       , "'@'"          ) \
_d(TILDE    , "'~'"          ) \
_d(COMMA    , "','"          ) \
_d(SEMI     , "';'"          ) \
_d(DOT      , "'.'"          ) \
_d(BDOT     , "'..'"         ) \
_d(TDOT     , "'...'"        ) \
_d(COLON    , "':'"          ) \
_d(PLUS     , "'+'"          ) \
_d(BPLUS    , "'++'"         ) \
_d(MINUS    , "'-'"          ) \
_d(STAR     , "'*'"          ) \
_d(LSLASH   , "'/'"          ) \
_d(PERCENT  , "'%'"          ) \
_d(ASSIGN   , "'='"          ) \
_d(EQ       , "'=='"         ) \
_d(BANG     , "'!'"          ) \
_d(BBANG    , "'!!'"         ) \
_d(NE       , "'!='"         ) \
_d(GT       , "'>'"          ) \
_d(GE       , "'>='"         ) \
_d(SHL      , "'<<'"         ) \
_d(LT       , "'<'"          ) \
_d(LE       , "'<='"         ) \
_d(SHR      , "'>>'"         ) \
_d(HAT      , "'^'"          ) \
_d(AMP      , "'&'"          ) \
_d(BAMP     , "'&&'"         ) \
_d(BAR      , "'|'"          ) \
_d(BBAR     , "'||'"         ) \
_d(QUESTION , "'?'"          ) \
_d(BQUESTION, "'??\'"        ) \
_d(QDOT     , "'?.'"         ) \
_d(ELVIS    , "'?:'"         ) \
_d(ARROW    , "'->'"         ) \
_d(QARROW   , "'?->'"        ) \
_d(IDENT    , "<ident>"      ) \
_d(INTEGER  , "<integer>"    ) \
_d(FLOAT    , "<float>"      ) \
_d(STRING   , "<string>"     ) \
_d(TSBEGIN  , "<string>"     ) \
_d(TSESCAPE , "'$'"          ) \
_d(TSEND    , "<string>"     ) \
_d(EOF      , "<eof>"        ) \
_e(OP)

#define SYM_ALL(_d,_b,_e) \
SYM_NIL(_d,_b,_e)         \
SYM_KW(_d,_b,_e)          \
SYM_TM(_d,_b,_e)          \
SYM_EM(_d,_b,_e)          \
SYM_OP(_d,_b,_e)

#define SYM_ISTRS(_d,_b,_e) \
SYM_NIL(_d,_b,_e)           \
SYM_KW(_d,_b,_e)            \
SYM_TM(_d,_b,_e)            \
SYM_EM(_d,_b,_e)

#define SYM_TOKEN(_d,_b,_e) \
SYM_KW(_d,_b,_e)            \
SYM_OP(_d,_b,_e)

enum {
#define SYMDEF(n,r) STR_##n,
#define SYMBEGIN(g) STR__##g##_BEGIN,STR__##g##_STUB1 = STR__##g##_BEGIN - 1,
#define SYMEND(g) STR__##g##_END,STR__##g##_STUB2 = STR__##g##_END - 1,
    SYM_ISTRS(SYMDEF,SYMBEGIN,SYMEND)
#undef SYMDEF
#undef SYMBEGIN
#undef SYMEND
    STR__COUNT
};

enum {
    TK__NONE,
#define SYMDEF(n,r) TK_##n,
#define SYMBEGIN(g) TK__##g##_BEGIN,TK__##g##_STUB1 = TK__##g##_BEGIN - 1,
#define SYMEND(g) TK__##g##_END,TK__##g##_STUB2 = TK__##g##_END - 1,
    SYM_TOKEN(SYMDEF, SYMBEGIN, SYMEND)
#undef SYMDEF
#undef SYMBEGIN
#undef SYMEND
    TK__MAX
};

#endif /* asym_h_ */
