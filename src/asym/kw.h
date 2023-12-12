/**
 *@file asym/kw.h
 */

#ifndef STRDEF /* Make file analyzer quiet. */
# define STRDEF(...)
#endif

#ifdef PROLOGUE
PROLOGUE(KW)
#endif

STRDEF(as      )
STRDEF(break   )
STRDEF(case    )
STRDEF(const   )
STRDEF(continue)
STRDEF(do      )
STRDEF(else    )
STRDEF(false   )
STRDEF(fn      )
STRDEF(for     )
STRDEF(if      )
STRDEF(import  )
STRDEF(in      )
STRDEF(is      )
STRDEF(let     )
STRDEF(loop    )
STRDEF(match   )
STRDEF(mut     )
STRDEF(nil     )
STRDEF(pub     )
STRDEF(return  )
STRDEF(true    )
STRDEF(try     )
STRDEF(use     )
STRDEF(while   )
STRDEF(_       )

#ifdef EPILOGUE
EPILOGUE(KW)
#endif
