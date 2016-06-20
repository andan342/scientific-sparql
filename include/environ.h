/*****************************************************************************
 * AMOS2
 *
 * Author: (c) 2011 Tore Risch, UDBL
 * $RCSfile: environ.h,v $
 * $Revision: 1.22 $ $Date: 2015/06/21 20:54:49 $
 * $State: Exp $ $Locker:  $
 *
 * Description: Macros to handle different compilation environments
 *
 ****************************************************************************/

#ifndef _environ_h_
#define _environ_h_

//-----------------------------
#if defined(__cplusplus)
    #define EXTLANG extern "C"
#else
    #define EXTLANG extern
#endif
//-----------------------------
#if defined(WIN32)
    #include <io.h>
    #define NT 1
#elif defined(LINUX)
    #define O_BINARY 0
#else 
    #error Unsupported operating system
#endif
//-----------------------------
#if defined(KERNEL_DLL) // Compiling kernel DLL
    #define EXPORT __declspec(dllexport)
    #define EXTERN EXPORT
#elif defined(_DLL) || defined(_USRDLL) // Compiling DLL on top of kernel DLL
    #define EXPORT __declspec(dllexport)
    #define EXTERN EXTLANG __declspec(dllimport)
#elif defined(WIN32) // Compiling non-DLL under Windows
    #define EXPORT
    #define EXTERN EXTLANG __declspec(dllimport)
#elif defined(LINUX)
    #define EXPORT
    #define EXTERN EXTLANG
#else 
    #error Unsupported operating system
#endif
//-----------------------------
#define LONGCHARS 30

#ifdef NT
// Win32
    #include <malloc.h>
    #define INLINE __inline
    #define LONGINT __int64
    #define LongIntToString(x, buff)sprintf(buff,"%I64d",x)
    #define LONGINTMIN _I64_MIN
    #define LONGINTMAX _I64_MAX
    #define ATOI64 _atoi64 
    #define ZU "%u"
    #define ZI "%d"
    #define THREADLOCAL   __declspec( thread )
    #if _MSC_VER >= 1800 
    #define isatty(x)(x<1)
    #define fileno _fileno
    #define strdup _strdup
    #define unlink _unlink
    #define getcwd _getcwd
    #define chdir _chdir
    #define read _read
    #define write _write
    #define open _open
    #define close _close
    #endif
#else
// Unix
    #include <stdint.h>
    #define INLINE static inline
    #define LONGINT long long
    #define LongIntToString(x, buff)sprintf(buff,"%lld",x)
    #define LONGINTMIN LLONG_MIN
    #define LONGINTMAX LLONG_MAX
    #define ATOI64 atoll 

    #if UINTPTR_MAX == 0xffffffffffffffff
// 64 bits Unix
      #define B64 1
      #define ZU "%zu"
      #define ZI "%lld"
    #else
// 32 bits Unix
      #define ZU "%lu"
      #define ZI "%d"
    #define THREADLOCAL  __thread
    #endif
#endif
//-----------------------------

#endif
