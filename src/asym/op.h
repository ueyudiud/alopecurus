/**
 *@file asym/op.h
 */

#ifndef STRDEF2 /* Make file analyzer quiet. */
# define STRDEF2(...)
#endif

#ifdef PROLOGUE
PROLOGUE(OP)
#endif

STRDEF2(LBK      , "'('"          )
STRDEF2(RBK      , "')'"          )
STRDEF2(LSQ      , "'['"          )
STRDEF2(RSQ      , "']'"          )
STRDEF2(LBR      , "'{'"          )
STRDEF2(RBR      , "'}'"          )
STRDEF2(SHARP    , "'#'"          )
STRDEF2(AT       , "'@'"          )
STRDEF2(TILDE    , "'~'"          )
STRDEF2(COMMA    , "','"          )
STRDEF2(SEMI     , "';'"          )
STRDEF2(DOT      , "'.'"          )
STRDEF2(BDOT     , "'..'"         )
STRDEF2(TDOT     , "'...'"        )
STRDEF2(COLON    , "':'"          )
STRDEF2(BCOLON   , "'::'"         )
STRDEF2(PLUS     , "'+'"          )
STRDEF2(MINUS    , "'-'"          )
STRDEF2(STAR     , "'*'"          )
STRDEF2(LSLASH   , "'/'"          )
STRDEF2(PERCENT  , "'%'"          )
STRDEF2(ASSIGN   , "'='"          )
STRDEF2(EQ       , "'=='"         )
STRDEF2(BANG     , "'!'"          )
STRDEF2(BBANG    , "'!!'"         )
STRDEF2(NE       , "'!='"         )
STRDEF2(GT       , "'>'"          )
STRDEF2(GE       , "'>='"         )
STRDEF2(SHL      , "'<<'"         )
STRDEF2(LT       , "'<'"          )
STRDEF2(LE       , "'<='"         )
STRDEF2(SHR      , "'>>'"         )
STRDEF2(HAT      , "'^'"          )
STRDEF2(AMP      , "'&'"          )
STRDEF2(BAMP     , "'&&'"         )
STRDEF2(BAR      , "'|'"          )
STRDEF2(BBAR     , "'||'"         )
STRDEF2(QUESTION , "'?'"          )
STRDEF2(BQUESTION, "'??\'"        )
STRDEF2(QDOT     , "'?.'"         )
STRDEF2(ELVIS    , "'?:'"         )
STRDEF2(ARROW    , "'->'"         )
STRDEF2(QARROW   , "'?->'"        )
STRDEF2(IDENT    , "<ident>"      )
STRDEF2(INTEGER  , "<integer>"    )
STRDEF2(FLOAT    , "<float>"      )
STRDEF2(STRING   , "<string>"     )
/* For template string. */
STRDEF2(TSBEGIN  , "<string>"     )
STRDEF2(TSESCAPE , "'$'"          )
STRDEF2(TSEND    , "<string>"     )
STRDEF2(EOF      , "<eof>"        )

#ifdef EPILOGUE
EPILOGUE(OP)
#endif
