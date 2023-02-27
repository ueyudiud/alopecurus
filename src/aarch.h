/*
 * aarch.h
 */

#ifndef aarch_h_
#define aarch_h_

#define ALO_ENUM_ARCH_X86 1
#define ALO_ENUM_ARCH_X64 2
#define ALO_ENUM_ARCH_ARM 3
#define ALO_ENUM_ARCH_AARCH64 4

#define ALO_ENUM_OS_OTHER 0
#define ALO_ENUM_OS_WINDOWS 1
#define ALO_ENUM_OS_LINUX 2
#define ALO_ENUM_OS_OSX 3

#ifndef ALOI_ARCH
# if defined(__i386) || defined(__i386__)
#  define ALO_ARCH ALO_ENUM_ARCH_X86
# elif defined(__x86_64) || defined(__x86_64__)
#  define ALO_ARCH ALO_ENUM_ARCH_X64
# elif defined(__arm) && defined(__arm__)
#  define ALO_ARCH ALO_ENUM_ARCH_ARM
# elif defined(__aarch) && defined(__aarch64__)
#  define ALO_ARCH ALO_ENUM_ARCH_AARCH64
# else
#  define ALO_ARCH ALO_ENUM_OS_OTHER
# endif
#else
# define ALO_ARCH ALOI_ARCH
#endif

#define ALO_ARCH_X64 (ALO_ARCH == ALO_ENUM_ARCH_X64)

#ifndef ALOI_OS
# if defined(_WIN32)
#  define ALO_OS ALO_ENUM_OS_WINDOWS
# elif defined(__linux__)
#  define ALO_OS ALO_ENUM_OS_LINUX
# elif defined(__MACH__) && defined(__APPLE__)
#  define ALO_OS ALO_ENUM_OS_OSX
# else
#  define ALO_OS ALO_ENUM_OS_OTHER
# endif
#else
# define ALO_OS ALOI_OS
#endif

#define ALO_OS_WINDOWS (ALO_OS == ALO_ENUM_OS_WINDOWS)
#define ALO_OS_POSIX (ALO_OS >= ALO_ENUM_OS_LINUX)

#if ALO_ARCH_X64
# define ALO_BITS 64
#else
# error "Unknwon architecture."
#endif

#define ALO_M32 (ALO_BITS == 32)
#define ALO_M64 (ALO_BITS == 64)

#define ALO_ENUM_STACK_INNER 0
#define ALO_ENUM_STACK_MMAP 1

#if !defined(ALOI_STACK_DISABLE_MMAP) && (ALO_OS_WINDOWS || ALO_OS_POSIX)
# define ALO_STACK ALO_ENUM_STACK_MMAP
#else
# define ALO_STACK ALO_ENUM_STACK_INNER
#endif

#define ALO_STACK_INNER (ALO_STACK == ALO_ENUM_STACK_INNER)
#define ALO_STACK_MMAP (ALO_STACK == ALO_ENUM_STACK_MMAP)
#define ALO_STACK_OUTER (ALO_STACK != ALO_ENUM_STACK_INNER)

#define ALO_STACK_RELOC (ALO_STACK == ALO_STACK_INNER)

#endif /* aarch_h_ */
