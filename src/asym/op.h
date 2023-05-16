/**
 *@file asym/op.h
 */

#ifndef AI_SYM /* Make file analyzer quiet. */
# define AI_SYM(...)
#endif

#ifdef AI_SYM_PROLOGUE
AI_SYM_PROLOGUE(OP)
#endif

AI_SYM(OP, LBK      , "("  )
AI_SYM(OP, RBK      , ")"  )
AI_SYM(OP, LSQ      , "["  )
AI_SYM(OP, RSQ      , "]"  )
AI_SYM(OP, LBR      , "{"  )
AI_SYM(OP, RBR      , "}"  )
AI_SYM(OP, SHARP    , "#"  )
AI_SYM(OP, AT       , "@"  )
AI_SYM(OP, TILDE    , "~"  )
AI_SYM(OP, COMMA    , ","  )
AI_SYM(OP, SEMI     , ";"  )
AI_SYM(OP, DOT      , "."  )
AI_SYM(OP, BDOT     , ".." )
AI_SYM(OP, TDOT     , "...")
AI_SYM(OP, COLON    , ":"  )
AI_SYM(OP, BCOLON   , "::" )
AI_SYM(OP, PLUS     , "+"  )
AI_SYM(OP, MINUS    , "-"  )
AI_SYM(OP, STAR     , "*"  )
AI_SYM(OP, LSLASH   , "/"  )
AI_SYM(OP, RSLASH   , "\\" )
AI_SYM(OP, PERCENT  , "%"  )
AI_SYM(OP, ASSIGN   , "="  )
AI_SYM(OP, EQ       , "==" )
AI_SYM(OP, BANG     , "!"  )
AI_SYM(OP, BBANG    , "!!" )
AI_SYM(OP, NE       , "!=" )
AI_SYM(OP, GT       , ">"  )
AI_SYM(OP, GE       , ">=" )
AI_SYM(OP, SHL      , "<<" )
AI_SYM(OP, LT       , "<"  )
AI_SYM(OP, LE       , "<=" )
AI_SYM(OP, SHR      , ">>" )
AI_SYM(OP, HAT      , "^"  )
AI_SYM(OP, AMP      , "&"  )
AI_SYM(OP, BAMP     , "&&" )
AI_SYM(OP, BAR      , "|"  )
AI_SYM(OP, BBAR     , "||" )
AI_SYM(OP, QUESTION , "?"  )
AI_SYM(OP, BQUESTION, "??" )
AI_SYM(OP, QDOT     , "?." )
AI_SYM(OP, ELVIS    , "?:" )

#ifdef AI_SYM_EPILOGUE
AI_SYM_EPILOGUE(OP)
#endif
