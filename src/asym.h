/**
 *@file asym.h
 */

#ifndef asym_h_
#define asym_h_

/* Symbol collection flags. */
#define SYM_EMPTY 0x01
#define SYM_KW 0x02 /* Keyword */
#define SYM_TM 0x04 /* Tagged method */
#define SYM_EM 0x08 /* Error message */
#define SYM_OP 0x10 /* Operator */

/* Symbol collection bundle flags. */
#define SYMLIST_SSTRS (SYM_EMPTY | SYM_KW | SYM_TM | SYM_EM | SYM_OP)
#define SYMLIST_ISTRS (SYM_EMPTY | SYM_KW | SYM_TM | SYM_EM)
#define SYMLIST_TOKEN (SYM_KW | SYM_OP)

enum {
#define SYMLIST SYMLIST_ISTRS
#define PROLOGUE(g) STR_##g##__FIRST, STR_##g##__STUB1 = STR_##g##__FIRST - 1,
#define EPILOGUE(g) STR_##g##__STUB2, STR_##g##__LAST = STR_##g##__STUB2 - 1,
#define SYMDEF(n,r) STR_##n,
#include "asym.h"
    STR__COUNT
};

enum {
    TK__NONE,
#define SYMLIST SYMLIST_TOKEN
#define PROLOGUE(g) TK_##g##__FIRST, TK_##g##__STUB1 = TK_##g##__FIRST - 1,
#define EPILOGUE(g) TK_##g##__STUB2, TK_##g##__LAST = TK_##g##__STUB2 - 1,
#define SYMDEF(n,r) TK_##n,
#include "asym.h"
    TK__MAX
};

#endif /* asym_h_ */

#if defined(SYMDEF)

#define SYMDEF1(n,...) SYMDEF(__VA_ARGS__##n, #n)
#define SYMDEF2 SYMDEF

#ifndef PROLOGUE
# define PROLOGUE(...)
#endif

#ifndef EPILOGUE
# define EPILOGUE(...)
#endif

#if SYMLIST & SYM_EMPTY
PROLOGUE(EMPTY)

SYMDEF2(EMPTY, "")

EPILOGUE(EMPTY)
#endif

#if SYMLIST & SYM_KW
PROLOGUE(KW)

SYMDEF1(as      )
SYMDEF1(break   )
SYMDEF1(case    )
SYMDEF1(const   )
SYMDEF1(continue)
SYMDEF1(do      )
SYMDEF1(else    )
SYMDEF1(false   )
SYMDEF1(fn      )
SYMDEF1(for     )
SYMDEF1(if      )
SYMDEF1(import  )
SYMDEF1(in      )
SYMDEF1(is      )
SYMDEF1(let     )
SYMDEF1(loop    )
SYMDEF1(match   )
SYMDEF1(mut     )
SYMDEF1(nil     )
SYMDEF1(pub     )
SYMDEF1(return  )
SYMDEF1(true    )
SYMDEF1(try     )
SYMDEF1(use     )
SYMDEF1(while   )
SYMDEF1(_       )

EPILOGUE(KW)
#endif

#if SYMLIST & SYM_TM
PROLOGUE(TM)

SYMDEF1(__get__  )
SYMDEF1(__set__  )
SYMDEF1(__look__ )
SYMDEF1(__len__  )
SYMDEF1(__close__)
SYMDEF1(__hash__ )
SYMDEF1(__eq__   )
SYMDEF1(__lt__   )
SYMDEF1(__le__   )
SYMDEF1(__in__   )
SYMDEF1(__add__  )
SYMDEF1(__sub__  )
SYMDEF1(__mul__  )
SYMDEF1(__div__  )
SYMDEF1(__mod__  )
SYMDEF1(__shl__  )
SYMDEF1(__shr__  )
SYMDEF1(__band__ )
SYMDEF1(__bor__  )
SYMDEF1(__bxor__ )
SYMDEF1(__neg__  )
SYMDEF1(__bnot__ )
SYMDEF1(__call__ )
SYMDEF1(__iter__ )
SYMDEF1(__next__ )
SYMDEF1(__str__  )

EPILOGUE(TM)
#endif

#if SYMLIST & SYM_OP
PROLOGUE(OP)

SYMDEF2(LBK      , "'('"          )
SYMDEF2(RBK      , "')'"          )
SYMDEF2(LSQ      , "'['"          )
SYMDEF2(RSQ      , "']'"          )
SYMDEF2(LBR      , "'{'"          )
SYMDEF2(RBR      , "'}'"          )
SYMDEF2(SHARP    , "'#'"          )
SYMDEF2(AT       , "'@'"          )
SYMDEF2(TILDE    , "'~'"          )
SYMDEF2(COMMA    , "','"          )
SYMDEF2(SEMI     , "';'"          )
SYMDEF2(DOT      , "'.'"          )
SYMDEF2(BDOT     , "'..'"         )
SYMDEF2(TDOT     , "'...'"        )
SYMDEF2(COLON    , "':'"          )
SYMDEF2(PLUS     , "'+'"          )
SYMDEF2(BPLUS    , "'++'"         )
SYMDEF2(MINUS    , "'-'"          )
SYMDEF2(STAR     , "'*'"          )
SYMDEF2(LSLASH   , "'/'"          )
SYMDEF2(PERCENT  , "'%'"          )
SYMDEF2(ASSIGN   , "'='"          )
SYMDEF2(EQ       , "'=='"         )
SYMDEF2(BANG     , "'!'"          )
SYMDEF2(BBANG    , "'!!'"         )
SYMDEF2(NE       , "'!='"         )
SYMDEF2(GT       , "'>'"          )
SYMDEF2(GE       , "'>='"         )
SYMDEF2(SHL      , "'<<'"         )
SYMDEF2(LT       , "'<'"          )
SYMDEF2(LE       , "'<='"         )
SYMDEF2(SHR      , "'>>'"         )
SYMDEF2(HAT      , "'^'"          )
SYMDEF2(AMP      , "'&'"          )
SYMDEF2(BAMP     , "'&&'"         )
SYMDEF2(BAR      , "'|'"          )
SYMDEF2(BBAR     , "'||'"         )
SYMDEF2(QUESTION , "'?'"          )
SYMDEF2(BQUESTION, "'??\'"        )
SYMDEF2(QDOT     , "'?.'"         )
SYMDEF2(ELVIS    , "'?:'"         )
SYMDEF2(ARROW    , "'->'"         )
SYMDEF2(QARROW   , "'?->'"        )
SYMDEF2(IDENT    , "<ident>"      )
SYMDEF2(INTEGER  , "<integer>"    )
SYMDEF2(FLOAT    , "<float>"      )
SYMDEF2(STRING   , "<string>"     )
/* For template string. */
SYMDEF2(TSBEGIN  , "<string>"     )
SYMDEF2(TSESCAPE , "'$'"          )
SYMDEF2(TSEND    , "<string>"     )
SYMDEF2(EOF      , "<eof>"        )

EPILOGUE(OP)
#endif

#undef SYMDEF1
#undef SYMDEF2
#undef SYMDEF
#undef PROLOGUE
#undef EPILOGUE
#undef SYMLIST

#endif
