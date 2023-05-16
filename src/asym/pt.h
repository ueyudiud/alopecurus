/**
 *@file asym/pt.h
 */

#ifndef AI_SYM /* Make file analyzer quiet. */
# define AI_SYM(...)
#endif

#ifdef AI_SYM_PROLOGUE
AI_SYM_PROLOGUE(PT)
#endif

AI_SYM(PT, BOOL , "bool" )
AI_SYM(PT, INT  , "int"  )
AI_SYM(PT, FLOAT, "float")
AI_SYM(PT, PTR  , "ptr"  )
AI_SYM(PT, STR  , "str"  )
AI_SYM(PT, TUPLE, "tuple")
AI_SYM(PT, LIST , "list" )
AI_SYM(PT, TABLE, "table")
AI_SYM(PT, FUNC , "func" )
AI_SYM(PT, ROUTE, "route")
AI_SYM(PT, TYPE , "type" )
AI_SYM(PT, USER , "user" )

#ifdef AI_SYM_EPILOGUE
AI_SYM_EPILOGUE(PT)
#endif
