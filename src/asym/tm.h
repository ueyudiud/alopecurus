/**
 *@file asym/tm.h
 */

#ifndef AI_SYM /* Make file analyzer quiet. */
# define AI_SYM(...)
#endif

#ifdef AI_SYM_PROLOGUE
AI_SYM_PROLOGUE(TM)
#endif

AI_SYM(TM, GET    , "__get__"  )
AI_SYM(TM, SET    , "__set__"  )
AI_SYM(TM, LEN    , "__len__"  )
AI_SYM(TM, CLOSE  , "__close__")
AI_SYM(TM, HASH   , "__hash__" )
AI_SYM(TM, EQ     , "__eq__"   )
AI_SYM(TM, LT     , "__lt__"   )
AI_SYM(TM, LE     , "__le__"   )
AI_SYM(TM, IN     , "__in__"   )
AI_SYM(TM, ADD    , "__add__"  )
AI_SYM(TM, SUB    , "__sub__"  )
AI_SYM(TM, MUL    , "__mul__"  )
AI_SYM(TM, DIV    , "__div__"  )
AI_SYM(TM, MOD    , "__mod__"  )
AI_SYM(TM, SHL    , "__shl__"  )
AI_SYM(TM, SHR    , "__shr__"  )
AI_SYM(TM, BIT_AND, "__band__" )
AI_SYM(TM, BIT_OR , "__bor__"  )
AI_SYM(TM, BIT_XOR, "__bxor__" )
AI_SYM(TM, NEG    , "__neg__"  )
AI_SYM(TM, BIT_NOT, "__bnot__" )
AI_SYM(TM, CALL   , "__call__" )
AI_SYM(TM, STR    , "__str__"  )

#ifdef AI_SYM_EPILOGUE
AI_SYM_EPILOGUE(TM)
#endif
