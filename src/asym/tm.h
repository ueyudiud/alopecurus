/**
 *@file asym/tm.h
 */

#ifndef STRDEF /* Make file analyzer quiet. */
# define STRDEF(...)
#endif

#ifdef PROLOGUE
PROLOGUE(TM)
#endif

STRDEF(__get__  )
STRDEF(__set__  )
STRDEF(__look__ )
STRDEF(__len__  )
STRDEF(__close__)
STRDEF(__hash__ )
STRDEF(__eq__   )
STRDEF(__lt__   )
STRDEF(__le__   )
STRDEF(__in__   )
STRDEF(__add__  )
STRDEF(__sub__  )
STRDEF(__mul__  )
STRDEF(__div__  )
STRDEF(__mod__  )
STRDEF(__shl__  )
STRDEF(__shr__  )
STRDEF(__band__ )
STRDEF(__bor__  )
STRDEF(__bxor__ )
STRDEF(__neg__  )
STRDEF(__bnot__ )
STRDEF(__call__ )
STRDEF(__iter__ )
STRDEF(__next__ )
STRDEF(__str__  )

#ifdef EPILOGUE
EPILOGUE(TM)
#endif
