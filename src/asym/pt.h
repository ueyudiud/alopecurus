/**
 *@file asym/pt.h
 */

#ifndef STRDEF /* Make file analyzer quiet. */
# define STRDEF(...)
#endif

#ifdef PROLOGUE
PROLOGUE(PT)
#endif

/* nil is already defined in kw.h */
STRDEF(bool )
STRDEF(int  )
STRDEF(float)
STRDEF(ptr  )
STRDEF(str  )
STRDEF(tuple)
STRDEF(list )
STRDEF(table)
STRDEF(func )
STRDEF(route)
STRDEF(type )
STRDEF(mod  )

#ifdef EPILOGUE
EPILOGUE(PT)
#endif
