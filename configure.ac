#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT([Poly/ML],[5.7],[polyml AT polyml DOT org],[polyml])
AM_INIT_AUTOMAKE
AC_PREREQ(2.65)
# libtoolize recommends this line.
AC_CONFIG_MACRO_DIR([m4])

ac_debug_mode="no"

AC_ARG_ENABLE([debug],
   [  --enable-debug          Compiles without optimisation for debugging ],
   [ac_debug_mode="yes"])

if test "$ac_debug_mode" != "yes"; then
    # Default to maximum optimisation. -O2 is not good enough.
        # Set CCASFLAGS to empty so that it doesn't get set to CFLAGS.
        #   The -g option on assembler causes problems on Sparc/Solaris 10.
        # test X || Y  is equivalent to  if !X then Y
        test "${CFLAGS+set}" = set || CFLAGS="-O3"
        test "${CXXFLAGS+set}" = set || CXXFLAGS="-O3"
        test "${CCASFLAGS+set}" = set || CCASFLAGS=""
else
        test "${CFLAGS+set}" = set || CFLAGS="-g"
        test "${CXXFLAGS+set}" = set || CXXFLAGS="-g"
        test "${CCASFLAGS+set}" = set || CCASFLAGS=""
fi

AC_CANONICAL_HOST
# Set the OS flag.  This should not really be required but is still used in
# a few places for OS-specific quirks that can't be handled by more specific tests.
# Check also for mingw in which case we want to build native Windows.
poly_native_windows=no
# If we are building on cygwin or mingw we need to give the -no-defined flag to
# build a DLL.  We also have to use Windows calling conventions rather than
# SysV on 64-bit.
# On mingw we need the -mthreads flag and this must be included in CFLAGS and
# CXXFLAGS before the LIBTOOL macros.
# When building native Windows set the flag to build the Unicode version and
# the API version to at least Windows Vista to allow condition variables.
poly_no_undefined=no
poly_use_windowscc=no
case "${host_os}" in
    darwin*)
        AC_SUBST([OSFLAG], [-DMACOSX])
        ;;
    sunos* | solaris*)
        AC_SUBST([OSFLAG], [-DSOLARIS])
        ;;
    *mingw*)
        AC_SUBST([OSFLAG], ["-DUNICODE -D_UNICODE -D_WIN32_WINNT=0x600"])
        AC_CHECK_TOOL(WINDRES, windres)
        poly_native_windows=yes
        poly_no_undefined=yes
        poly_use_windowscc=yes
        CFLAGS="$CFLAGS -mthreads"
        CXXFLAGS="$CXXFLAGS -mthreads"
        ;;
    *cygwin*)
        poly_no_undefined=yes
        poly_use_windowscc=yes
        if test "$ac_cv_c_compiler_gnu" = "yes"; then
            if $CC -dumpmachine | grep -q mingw; then
                AC_SUBST([OSFLAG], ["-DUNICODE -D_UNICODE -D_WIN32_WINNT=0x600"])
                poly_native_windows=yes
                CFLAGS="$CFLAGS -mthreads"
                CXXFLAGS="$CXXFLAGS -mthreads"
            fi
        fi
        ;;
esac

# Enable/Disable the GUI in Windows.  Should this only be present if
# building on mingw?
AC_ARG_ENABLE([windows-gui],
        [AS_HELP_STRING([--enable-windows-gui],
            [create a GUI in Windows. If this is disabled use a Windows console. @<:@default=yes@:>@])],
        [case "${enableval}" in
           yes) enablegui=true ;;
           no)  enablegui=false ;;
           *) AC_MSG_ERROR([bad value ${enableval} for --enable-windows-gui]) ;;
        esac],
        [enablegui=true])
AM_CONDITIONAL([WINDOWSGUI], [test x$enablegui = xtrue])

# libpolyml can be a DLL but libpolymain can't.
# Disable shared libraries by default.  It complicates installation to "non-standard" places.
LT_INIT([win32-dll disable-shared])
AM_MAINTAINER_MODE


# Check we're in the right directory
AC_CONFIG_SRCDIR([polyexports.h])
AC_CONFIG_HEADER([config.h])


# Checks for programs.
AC_PROG_CXX
# The following check was supposed to check that there was actually a
# C++ compiler but doesn't work properly if CXX is set by the user.
#AC_CHECK_PROG(check_cpp, $CXX, "yes", "no")
#if test "$check_cpp" != "yes"; then
#    AC_MSG_ERROR([No C++ compiler found.  Unable to build Poly/ML.])
#fi

AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_CPP

AM_PROG_AS

# Activate large file mode if needed
AC_SYS_LARGEFILE

# Checks for libraries.
AC_CHECK_LIB(gcc, main)
AC_CHECK_LIB(gcc_s, main)
AC_CHECK_LIB(stdc++, main)

# These can sometimes be in the standard libraries
AC_SEARCH_LIBS([dlopen], [dl dld])
AC_SEARCH_LIBS([floor], [m])

# The next two are only used with mingw
AC_CHECK_LIB(ws2_32, main)
AC_CHECK_LIB(gdi32, main)

## External names on Win64.  They have no leading underscores as per
## the X64 ABI published by MS.  Earlier versions of GCC (anything
## prior to 4.5.0) were faulty.
LT_SYS_SYMBOL_USCORE
if test x$sys_symbol_underscore = xyes; then
    AC_DEFINE(SYMBOLS_REQUIRE_UNDERSCORE, [1], [Defined if external symbols are prefixed by underscores])
fi

# Solaris needs -lsocket, -lnsl and -lrt
AC_SEARCH_LIBS([gethostbyname], [nsl])
AC_SEARCH_LIBS([getsockopt], [socket])
AC_SEARCH_LIBS([sem_wait], [rt])

# Check for X and Motif headers and libraries
AC_PATH_X

if test "x${with_x}" = "xyes"; then

    AC_DEFINE([WITH_XWINDOWS], [1], [Define if the X-Windows interface should be built])

    if test "$x_includes" != "" ; then
    if test  "$x_includes" != "NONE" ; then
        CFLAGS="$CFLAGS -I$x_includes"
        CXXFLAGS="$CXXFLAGS -I$x_includes"
        CPPFLAGS="$CPPFLAGS -I$x_includes"
    fi
    fi
    if test "$x_libraries" != "" ; then
    if test "$x_libraries" != "NONE" ; then
        LIBS="-L$x_libraries $LIBS"
    fi
    fi
    AC_CHECK_LIB(X11, XCreateGC)
    AC_CHECK_LIB(Xt, XtMalloc)
    AC_CHECK_LIB(Xext, XextAddDisplay)

    if test "$xm_includes" != "" ; then
    if test "$xm_includes" != "NONE" ; then
        CFLAGS="$CFLAGS -I$xm_includes"
        CXXFLAGS="$CXXFLAGS -I$xm_includes"
        CPPFLAGS="$CPPFLAGS -I$xm_includes"
    fi
    fi
    if test "$xm_libraries" != "" ; then
    if test "$xm_libraries" != "NONE" ; then
        LIBS="-L$xm_libraries $LIBS"
    fi
    fi
    AC_CHECK_LIB(Xm, XmGetDestination)

fi

# TODO: May need AC_PATH_XTRA for Solaris

# Check for headers

AC_FUNC_ALLOCA
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([stdio.h time.h fcntl.h float.h limits.h locale.h malloc.h netdb.h netinet/in.h stddef.h])
AC_CHECK_HEADERS([stdlib.h string.h sys/file.h sys/ioctl.h sys/param.h sys/socket.h sys/systeminfo.h])
AC_CHECK_HEADERS([sys/time.h unistd.h values.h dlfcn.h signal.h ucontext.h])
AC_CHECK_HEADERS([assert.h ctype.h direct.h errno.h excpt.h fenv.h fpu_control.h grp.h])
AC_CHECK_HEADERS([ieeefp.h io.h math.h memory.h netinet/tcp.h poll.h pwd.h siginfo.h])
AC_CHECK_HEADERS([stdarg.h sys/errno.h sys/filio.h sys/mman.h sys/resource.h])
AC_CHECK_HEADERS([sys/signal.h sys/sockio.h sys/stat.h termios.h sys/termios.h sys/times.h])
AC_CHECK_HEADERS([sys/types.h sys/uio.h sys/un.h sys/utsname.h sys/select.h sys/sysctl.h])
AC_CHECK_HEADERS([sys/elf_SPARC.h sys/elf_386.h sys/elf_amd64.h asm/elf.h])
AC_CHECK_HEADERS([windows.h tchar.h semaphore.h])

# Only check for the X headers if the user said --with-x.
if test "${with_x+set}" = set; then
    AC_CHECK_HEADERS([X11/Xlib.h Xm/Xm.h])
fi

PKG_PROG_PKG_CONFIG

# Check for GMP
AC_ARG_WITH([gmp],
            [AS_HELP_STRING([--with-gmp],
              [use the GMP library for arbitrary precision arithmetic @<:@default=check@:>@])],
            [],
            [with_gmp=check])

# If we want GMP check that the library and headers are installed.
if test "x$with_gmp" != "xno"; then
    AC_CHECK_LIB([gmp], [__gmpn_tdiv_qr],
        [AC_DEFINE([HAVE_LIBGMP], [1],
              [Define to 1 if you have libgmp])
         [LIBS="-lgmp $LIBS"]
         AC_CHECK_HEADER([gmp.h],
             [AC_DEFINE([HAVE_GMP_H], [1],
                  [Define to 1 if you have the gmp.h header file])],
             [if test "x$with_gmp" != "xcheck"; then
                  AC_MSG_FAILURE(
                      [--with-gmp was given, but gmp.h header file is not installed])
              fi
             ])
        ],
        [if test "x$with_gmp" != "xcheck"; then
            AC_MSG_FAILURE(
                [--with-gmp was given, but gmp library (version 4 or later) is not installed])
         fi
        ])
fi

# libffi
# libffi must be configured even if we are not building with it so that things like "make dist" work.
AC_CONFIG_SUBDIRS([libffi])
# Use the internal version unless --with-system-libffi is given.
AC_ARG_WITH([system-libffi],
            [AS_HELP_STRING([--with-system-libffi],
              [use the version of libffi installed on your system rather than the version supplied with poly @<:@default=no@:>@])],
            [],
            [with_system_libffi=no])

# Libffi uses pkg-config.
if test "x$with_system_libffi" = "xyes"; then
    PKG_CHECK_MODULES([FFI], [libffi],
        [LIBS="$FFI_LIBS $LIBS" CFLAGS="$FFI_CFLAGS $CFLAGS"],
        [AC_CHECK_LIB([ffi], [ffi_prep_closure_loc],
             [
                 [LIBS="-lffi $LIBS"]
                 AC_CHECK_HEADER([ffi.h], [],
                     [ AC_MSG_FAILURE([--with-system-libffi was given, but ffi.h header file cannot be found]) ])
             ],
             [AC_MSG_FAILURE([--with-system-libffi was given, but the ffi library is not installed])]
       )
       ]
    )
else
# Use internal libffi
    CFLAGS="$CFLAGS -I../libffi/include"
    CXXFLAGS="$CXXFLAGS -I../libffi/include"
fi

AM_CONDITIONAL([INTERNAL_LIBFFI], [test "x$with_system_libffi" != "xyes"])


# Check for pthreads.  Mainly, this allows a single-threaded build on OSes such as Mac OS X
# that don't have proper multi-threaded profiling.
AC_ARG_WITH([threads],
            [AS_HELP_STRING([--with-threads],
              [use the pthread library for multi-threading @<:@default=check@:>@])],
            [],
            [with_threads=check])

# If we want pthreads check that the library and headers are installed.
# On Android pthread_create is in the standard library
if test "x$with_threads" != "xno"; then
    AC_SEARCH_LIBS([pthread_create], [pthread],
        [AC_DEFINE([HAVE_LIBPTHREAD], [1],
              [Define to 1 if you have the `pthread' library (-lpthread).])
         AC_CHECK_HEADER([pthread.h],
             [AC_DEFINE([HAVE_PTHREAD_H], [1],
                  [Define to 1 if you have the <pthread.h> header file.])],
             [if test "x$with_threads" != "xcheck"; then
                  AC_MSG_FAILURE(
                      [--with-threads was given, but pthread.h header file is not installed])
              fi
             ])
        ],
        [if test "x$with_threads" != "xcheck"; then
            AC_MSG_FAILURE(
                [--with-threads was given, but pthread library is not installed])
         fi
        ])
fi


# Find out which type of object code exporter to use.
# If we have winnt use PECOFF.  This really only applies to cygwin here.
# If we have elf.h use ELF.
# If we have mach-o/reloc.h use Mach-O
# Otherwise use the C source code exporter.
AC_CHECK_TYPES([IMAGE_FILE_HEADER],
    [AC_DEFINE([HAVE_PECOFF], [], [Define to 1 if you have the PE/COFF types.])]
        [polyexport=pecoff],
    [AC_CHECK_HEADER([elf.h],
        [AC_DEFINE([HAVE_ELF_H], [], [Define to 1 if you have the <elf.h> header file.])]
            [polyexport=elf],
              [AC_CHECK_HEADER([mach-o/reloc.h],
                  [AC_DEFINE([HAVE_MACH_O_RELOC_H], [], [Define to 1 if you have the <mach-o/reloc.h> header file.])]
                          [polyexport=macho],
                      [AC_CHECK_HEADERS([elf_abi.h machine/reloc.h],
                          [AC_DEFINE([HAVE_ELF_ABI_H], [], [Define to 1 if you have <elf_abi.h> and <machine/reloc.h> header files.])]
                              [polyexport=elf] )]
                )]
    )],
    [#include <windows.h>]
)

AM_CONDITIONAL([EXPPECOFF], [test "$polyexport" = pecoff])
AM_CONDITIONAL([EXPELF], [test "$polyexport" = elf])
AM_CONDITIONAL([EXPMACHO], [test "$polyexport" = macho])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_TYPE_INT16_T
AC_TYPE_UINT16_T
AC_TYPE_INT32_T
AC_TYPE_UINT32_T
AC_TYPE_INT64_T
AC_TYPE_UINT64_T
AC_TYPE_INTPTR_T
AC_TYPE_UINTPTR_T
AC_TYPE_UID_T
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_HEADER_TIME
AC_STRUCT_TM

# Check for the various sub-second fields of the stat structure.
AC_CHECK_MEMBERS([struct stat.st_atim, struct stat.st_atimespec,
  struct stat.st_atimensec, struct stat.st_atime_n, struct stat.st_uatime])

# Mac OS X, at any rate, needs signal.h to be included first.
AC_CHECK_TYPES([ucontext_t], , , [#include "signal.h"
#include "ucontext.h"])
AC_CHECK_TYPES([struct sigcontext, stack_t, sighandler_t, sig_t], , ,[#include "signal.h"])
AC_CHECK_TYPES([socklen_t],,,[#include "sys/types.h"
#include "sys/socket.h"])

AC_CHECK_TYPES([SYSTEM_LOGICAL_PROCESSOR_INFORMATION],,,[#include "windows.h"])

AC_CHECK_TYPES(long long)
AC_CHECK_TYPES(ssize_t)

AC_CHECK_SIZEOF(void*)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long long)

AC_C_BIGENDIAN

# Checks for library functions.
AC_FUNC_CHOWN
AC_FUNC_CLOSEDIR_VOID
AC_FUNC_ERROR_AT_LINE
AC_FUNC_FORK
AC_FUNC_GETGROUPS
AC_FUNC_GETPGRP
AC_PROG_GCC_TRADITIONAL
AC_FUNC_LSTAT
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_SELECT_ARGTYPES
AC_FUNC_STAT
AC_FUNC_STRFTIME
AC_FUNC_STRTOD
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([bzero dup2 floor ftruncate getcwd gethostbyaddr gethostname gettimeofday memmove])
AC_CHECK_FUNCS([memset mkdir mkfifo munmap pathconf pow realpath rmdir select setlocale socket sqrt strdup strerror])
AC_CHECK_FUNCS([uname dlopen getrusage strtod dtoa sigaction mkstemp getpagesize sigaltstack mmap mprotect])
## There does not seem to be a declaration for fpsetmask in mingw64.
AC_CHECK_DECLS([fpsetmask], [], [], [[#include <ieeefp.h>]])
AC_CHECK_FUNCS([sysctl sysctlbyname])
AC_CHECK_FUNCS([localtime_r gmtime_r])
AC_CHECK_FUNCS([ctermid tcdrain])

# Where are the registers?
#Linux:
AC_CHECK_MEMBERS([mcontext_t.gregs, mcontext_t.regs, mcontext_t.mc_esp],,,[#include "ucontext.h"])
#Mac OS X:
AC_CHECK_MEMBERS([struct mcontext.ss, struct __darwin_mcontext.ss, struct __darwin_mcontext.__ss,
   struct __darwin_mcontext32.ss, struct __darwin_mcontext32.__ss,
   struct __darwin_mcontext64.ss, struct __darwin_mcontext64.__ss],,,
   [#include "signal.h"
#include "ucontext.h"])

# FreeBSD includes a sun_len member in struct sockaddr_un
AC_CHECK_MEMBERS([struct sockaddr_un.sun_len],,,
   [#include <sys/un.h>])

# Additional checks. AC_CANONICAL_HOST has already been called.
#AC_CANONICAL_HOST

# Check for .note.GNU-stack support, used for marking the stack as non-executable.
AC_MSG_CHECKING([whether as supports .note.GNU-stack])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[__asm__(".section .note.GNU-stack,\"\",@progbits");]])],
    [AC_MSG_RESULT([yes])] [AC_DEFINE([HAVE_GNU_STACK], [1],
        [Define to 1 if you have .note.GNU-stack support in the assembler.])],
    [AC_MSG_RESULT([no])])

# This option enables the native code generator.  More precisely it allows
# the byte code interpreter to be built on X86.

AC_ARG_ENABLE([native-codegeneration],
        [AS_HELP_STRING([--disable-native-codegeneration],
            [disable the native code generator and use the slow byte code interpreter instead.])],
        [case "${enableval}" in
           no)  with_portable=yes ;;
           yes) with_portable=no ;;
           *) AC_MSG_ERROR([bad value ${enableval} for --enable-native-codegeneration]) ;;
        esac],
        [with_portable=check])

case "${host_cpu}" in
      i[[3456]]86*)
            AC_DEFINE([HOSTARCHITECTURE_X86], [1], [Define if the host is an X86 (32-bit)])
            polyarch=i386
            ;;
      x86_64* | amd64*)
            if test X"$ac_cv_sizeof_voidp" = X8; then
                AC_DEFINE([HOSTARCHITECTURE_X86_64], [1], [Define if the host is an X86 (64-bit)])
                polyarch=x86_64
            else
                AC_DEFINE([HOSTARCHITECTURE_X32], [1], [Define if the host is an X86 (32-bit ABI, 64-bit processor)])
                polyarch=interpret
            fi
            ;;
      sparc64*)
            AC_DEFINE([HOSTARCHITECTURE_SPARC64], [1], [Define if the host is a Sparc (64-bit)])
            polyarch=interpret
            ;;
      sparc*)
            AC_DEFINE([HOSTARCHITECTURE_SPARC], [1], [Define if the host is a Sparc (32-bit)])
            polyarch=interpret
            ;;
      powerpc64* | ppc64*)
            AC_DEFINE([HOSTARCHITECTURE_PPC64], [1], [Define if the host is a PowerPC (64-bit)])
            polyarch=interpret
            ;;
      power* | ppc*)
            AC_DEFINE([HOSTARCHITECTURE_PPC], [1], [Define if the host is a PowerPC (32-bit)])
            polyarch=interpret
            ;;
      arm*)
            AC_DEFINE([HOSTARCHITECTURE_ARM], [1], [Define if the host is an ARM (32-bit)])
            polyarch=interpret
            ;;
      aarch64*)
            AC_DEFINE([HOSTARCHITECTURE_AARCH64], [1], [Define if the host is an ARM (64-bit)])
            polyarch=interpret
            ;;
      hppa*)
            AC_DEFINE([HOSTARCHITECTURE_HPPA], [1], [Define if the host is an HP PA-RISC (32-bit)])
            polyarch=interpret
            ;;
      ia64*)
            AC_DEFINE([HOSTARCHITECTURE_IA64], [1], [Define if the host is an Itanium])
            polyarch=interpret
            ;;
      m68k*)
            AC_DEFINE([HOSTARCHITECTURE_M68K], [1], [Define if the host is a Motorola 68000])
            polyarch=interpret
            ;;
      mips64*)
            AC_DEFINE([HOSTARCHITECTURE_MIPS64], [1], [Define if the host is a MIPS (64-bit)])
            polyarch=interpret
            ;;
      mips*)
            AC_DEFINE([HOSTARCHITECTURE_MIPS], [1], [Define if the host is a MIPS (32-bit)])
            polyarch=interpret
            ;;
      s390x*)
            AC_DEFINE([HOSTARCHITECTURE_S390X], [1], [Define if the host is an S/390 (64-bit)])
            polyarch=interpret
            ;;
      s390*)
            AC_DEFINE([HOSTARCHITECTURE_S390], [1], [Define if the host is an S/390 (32-bit)])
            polyarch=interpret
            ;;
      sh*)
            AC_DEFINE([HOSTARCHITECTURE_SH], [1], [Define if the host is a SuperH (32-bit)])
            polyarch=interpret
            ;;
      alpha*)
            AC_DEFINE([HOSTARCHITECTURE_ALPHA], [1], [Define if the host is an Alpha (64-bit)])
            polyarch=interpret
            # GCC defaults to non-conforming floating-point, and does not respect the rounding mode
            # in the floating-point control register, so we force it to conform to IEEE and use the
            # dynamic suffix on the floating-point instructions it produces.
            CFLAGS="$CFLAGS -mieee -mfp-rounding-mode=d"
            CXXFLAGS="$CXXFLAGS -mieee -mfp-rounding-mode=d"
            ;;
      *) AC_MSG_ERROR([Poly/ML is not supported for this architecture]) ;;
esac

# If we explicitly asked to use the interpreter set the architecture to interpreted.
if test "x$with_portable" = "xyes" ; then
    if test "x$polyarch" != "xinterpret" ; then
        AC_MSG_WARN(
            [*******You have disabled native code generation.  Are you really sure you want to do that?*******])
    fi
    polyarch=interpret
fi

# If we asked not to use the interpreter check we have native code support.
if test "x$with_portable" = "xno" ; then
    if test "x$polyarch" = "xinterpret" ; then
        AC_MSG_ERROR(
            [--enable-native-codegeneration was given but native code is not supported on this platform])
    fi
fi

# Set the define if this is the interpreter.  This is no longer actually used.
if test "x$polyarch" = "xinterpret" ; then
       AC_DEFINE([INTERPRETED], [1], [Define if using the interpreter] )
fi

# Put this test at the end where it's less likely to be missed.
# If we're compiling on Cygwin (and mingw?) and /usr/bin/file is not present
# the link step will produce some strange warning messages of the form:
# "Warning: linker path does not have real file for library -lXXX".  I think
# that's really a bug in autoconf but to explain what's happening to the user
# add a test here.
if test "$lt_cv_file_magic_cmd" = "func_win32_libid";
then
    if test \! -x /usr/bin/file;
    then
        echo ""
        echo "*** Warning: You are building Poly/ML on Cygwin/Mingw but '/usr/bin/file' cannot be found."
        echo "*** You can still go ahead and build Poly/ML but libpolyml will not be built as a"
        echo "*** shared library and you may get strange warning messages from the linker step."
        echo "*** Install the 'file' package to correct this problem."
        echo ""
    fi
fi

AM_CONDITIONAL([ARCHI386], [test "$polyarch" = i386])
AM_CONDITIONAL([ARCHX86_64], [test "$polyarch" = x86_64])
AM_CONDITIONAL([ARCHINTERPRET], [test "$polyarch" = interpret -a X"$ac_cv_sizeof_voidp" = X4])
AM_CONDITIONAL([ARCHINTERPRET64], [test "$polyarch" = interpret -a X"$ac_cv_sizeof_voidp" = X8])
AM_CONDITIONAL([WINDOWS64CALLS], [test "$poly_use_windowscc" = yes -a "$polyarch" = x86_64])

AM_CONDITIONAL([NATIVE_WINDOWS], [test "$poly_native_windows" = yes])
AM_CONDITIONAL([NO_UNDEFINED], [test "$poly_no_undefined" = yes])

# Test whether this is a git directory and set the version if possible
AC_CHECK_PROG([gitinstalled], [git], [yes], [no])
if test X"$gitinstalled" = "Xyes" -a -d ".git"; then
    GIT_VERSION='-DGIT_VERSION=\"$(shell git describe --tags --always)\"'
    AC_SUBST(GIT_VERSION)
fi

# Strip -fdebug-prefix-map= from CFLAGS; it's meaningless for users of polyc,
# and hurts reproducibility.
polyc_CFLAGS=
for cflag in $CFLAGS; do
    cflag="${cflag##-fdebug-prefix-map=*}"
    if test -n "$cflag"; then
        if test -n "$polyc_CFLAGS"; then
            polyc_CFLAGS="$polyc_CFLAGS $cflag"
        else
            polyc_CFLAGS="$cflag"
        fi
    fi
done
AC_SUBST([polyc_CFLAGS], ["$polyc_CFLAGS"])

# Modules directory
AC_ARG_WITH([moduledir],
  [AS_HELP_STRING([--with-moduledir=DIR], [directory for Poly/ML modules])],
  [moduledir=$withval],
  [moduledir="\${libdir}/polyml/modules"])
AC_SUBST([moduledir], [$moduledir])

# Control whether to build the basis library with arbitrary precision as the default int
AC_ARG_ENABLE([intinf-as-int],
    [AS_HELP_STRING([--enable-intinf-as-int], [set arbitrary precision as the default int type])],
    [case "${enableval}" in
       no) intisintinf=no ;;
       yes) intisintinf=yes ;;
       *) AC_MSG_ERROR([bad value ${enableval} for --enable-intinf-as-int]) ;;
        esac],
        [intisintinf=no])

AM_CONDITIONAL([INTINFISINT], [test "$intisintinf" = "yes"])

# These are needed for building in a separate build directory, as they are
# referenced from exportPoly.sml.
AC_CONFIG_COMMANDS([basis], [test -e basis || ln -sf ${ac_top_srcdir}/basis .])
AC_CONFIG_COMMANDS([mlsource], [test -e mlsource || ln -sf ${ac_top_srcdir}/mlsource .])

AC_CONFIG_FILES([Makefile libpolyml/Makefile libpolyml/polyml.pc libpolymain/Makefile modules/Makefile modules/IntInfAsInt/Makefile])
AC_CONFIG_FILES([polyc], [chmod +x polyc])
AC_OUTPUT
