/**
 *@file acfg.h
 */

#ifndef acfg_h_
#define acfg_h_

#include "aarch.h"
#include "alo.h"

#define ALO_VDIR ALO_VERSION_STRING(".") ALO_DIR_SEP

#define ALO_FILE_MARK "?"
#define ALO_EXEC_DIR "!"

#if ALO_OS_WINDOWS

#define ALO_SHDIR "!\\..\\share\\alo\\" ALO_VDIR

#define ALO_ALIB_PATH \
    "!\\alo\\"ALO_FILE_MARK".alo;" \
    "!\\alo\\"ALO_FILE_MARK"\\mod.alo;" \
    "!\\"ALO_FILE_MARK".alo;"      \
    "!\\"ALO_FILE_MARK"\\mod.alo;" \
    ALO_SHDIR ALO_FILE_MARK".alo;" \
    ALO_SHDIR ALO_FILE_MARK"\\mod.alo;" \
    ".\\"ALO_FILE_MARK".alo;"      \
    ".\\"ALO_FILE_MARK"\\mod.alo"

#define ALO_CLIB_PATH \
    "!\\"ALO_FILE_MARK".dll;" \
    "!\\..\\lib\\alo\\"ALO_VDIR ALO_FILE_MARK".dll;" \
    ".\\"ALO_FILE_MARK".dll"

#elif ALO_OS_POSIX

#define ALO_LDIR "/usr/local/lib/alo"ALO_VDIR
#define ALO_SHDIR "/usr/local/share/alo"ALO_VDIR

#define ALO_ALIB_PATH \
    ALO_LDIR ALO_FILE_MARK".alo:"  \
    ALO_LDIR ALO_FILE_MARK"/mod.alo:" \
    ALO_SHDIR ALO_FILE_MARK".alo:" \
    ALO_SHDIR ALO_FILE_MARK"/mod.alo:"\
    "./"ALO_FILE_MARK".alo:"       \
    "./"ALO_FILE_MARK"/mod.alo"

#define ALO_CLIB_PATH \
    ALO_LDIR ALO_FILE_MARK".so:" \
    "./"ALO_FILE_MARK".dll"

#else
# error file paths need configure manually.
#endif

#endif /* acfg_h_ */
