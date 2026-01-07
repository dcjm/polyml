#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT([Poly/ML],[5.9.2],[polyml AT polyml DOT org],[polyml])
AM_INIT_AUTOMAKE
AC_PREREQ([2.72])
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

# If the compiler defines _WIN32 we're building for native Windows otherwise we're
# building for something else.
AC_CHECK_DECL([_WIN32], [poly_native_windows=yes], [poly_native_windows=no])

# If we are building on cygwin or mingw we need to give the -no-defined flag to
# build a DLL.  We also have to use Windows calling conventions rather than
# SysV on 64-bit.
poly_use_windowscc=no
poly_need_macosopt=no
case "${host_os}" in
    darwin*)
        AC_SUBST([OSFLAG], [-DMACOSX])
        ;;
    sunos* | solaris*)
        AC_SUBST([OSFLAG], [-DSOLARIS])
        ;;
    haiku*)
        # Haiku error codes are negative by default; defining
        # B_USE_POSITIVE_POSIX_ERRORS translates the constants on header
        # files, and posix_error_mapper takes care of ensuring error
        # codes are positive at runtime.
        AC_CHECK_LIB([posix_error_mapper], [_errnop],
            [AC_SUBST([OSFLAG], [-DB_USE_POSITIVE_POSIX_ERRORS])
             LIBS="-lposix_error_mapper $LIBS"],
            [AC_MSG_FAILURE([Cannot link against posix_error_mapper])])
        ;;
    *mingw* | *cygwin*)
        poly_no_undefined=yes
        poly_use_windowscc=yes
        ;;
esac

# libpolyml can be a DLL but libpolymain can't.
# Enable shared libraries by default.  It complicates installation a bit if the
# the library is installed to a non-standard location but simplifies polyc.
LT_INIT([win32-dll])
AM_MAINTAINER_MODE


# Check we're in the right directory
AC_CONFIG_SRCDIR([polyexports.h])
AC_CONFIG_HEADERS([config.h])


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
AC_CHECK_LIB(stdc++, main)

# These can sometimes be in the standard libraries
AC_SEARCH_LIBS([dlopen], [dl dld])
AC_SEARCH_LIBS([floor], [m])

## External names on Win64.  They have no leading underscores as per
## the X64 ABI published by MS.  Earlier versions of GCC (anything
## prior to 4.5.0) were faulty.
LT_SYS_SYMBOL_USCORE
if test x$sys_symbol_underscore = xyes; then
    AC_DEFINE(SYMBOLS_REQUIRE_UNDERSCORE, [1], [Defined if external symbols are prefixed by underscores])
fi

# Check for headers

AC_FUNC_ALLOCA
AC_HEADER_DIRENT
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([stdio.h time.h fcntl.h float.h limits.h locale.h malloc.h netdb.h netinet/in.h stddef.h])
AC_CHECK_HEADERS([stdlib.h string.h sys/file.h sys/ioctl.h sys/param.h sys/socket.h sys/systeminfo.h])
AC_CHECK_HEADERS([sys/time.h unistd.h values.h dlfcn.h signal.h ucontext.h])
AC_CHECK_HEADERS([assert.h ctype.h direct.h errno.h excpt.h fenv.h fpu_control.h grp.h])
AC_CHECK_HEADERS([ieeefp.h io.h math.h memory.h netinet/tcp.h arpa/inet.h poll.h pwd.h siginfo.h])
AC_CHECK_HEADERS([stdarg.h sys/errno.h sys/filio.h sys/mman.h sys/resource.h])
AC_CHECK_HEADERS([sys/sockio.h sys/stat.h termios.h sys/times.h])
AC_CHECK_HEADERS([sys/types.h sys/uio.h sys/un.h sys/utsname.h sys/select.h sys/sysctl.h])
AC_CHECK_HEADERS([sys/elf_SPARC.h sys/elf_386.h sys/elf_amd64.h machine/reloc.h i386/elf_machdep.h])
AC_CHECK_HEADERS([mach-o/x86_64/reloc.h mach-o/arm64/reloc.h private/system/arch/x86_64/arch_elf.h])
AC_CHECK_HEADERS([windows.h tchar.h semaphore.h])
AC_CHECK_HEADERS([stdint.h inttypes.h])

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

# Special configuration for Windows or Unix.
poly_windows_enablegui=false

if test "x$poly_native_windows" = xyes; then
    # The next two are only used with mingw.  We mustn't include ws2_32 in Cygwin64 because
    # the "select" function gets used instead of Cygwin's own.
    AC_CHECK_LIB(ws2_32, main)
    AC_CHECK_LIB(gdi32, main)

    CFLAGS="$CFLAGS -mthreads"
    CXXFLAGS="$CXXFLAGS -mthreads"
    AC_SUBST([OSFLAG], ["-DUNICODE -D_UNICODE -D_WIN32_WINNT=0x600"])
    AC_CHECK_TOOL(WINDRES, windres)

    # Enable/Disable the GUI in Windows.
    AC_ARG_ENABLE([windows-gui],
        [AS_HELP_STRING([--enable-windows-gui],
            [create a GUI in Windows. If this is disabled use a Windows console. @<:@default=yes@:>@])],
        [case "${enableval}" in
            yes) poly_windows_enablegui=true ;;
            no)  poly_windows_enablegui=false ;;
            *) AC_MSG_ERROR([bad value ${enableval} for --enable-windows-gui]) ;;
        esac],
        [poly_windows_enablegui=true])

else
    # Unix or similar e.g. Cygwin.  We need pthreads.
    # On Android pthread_create is in the standard library
    AC_SEARCH_LIBS([pthread_create], [pthread],
        [AC_DEFINE([HAVE_LIBPTHREAD], [1],
              [Define to 1 if you have the `pthread' library (-lpthread).])
         AC_CHECK_HEADER([pthread.h],
             [AC_DEFINE([HAVE_PTHREAD_H], [1],
                  [Define to 1 if you have the <pthread.h> header file.])],
             [
                  AC_MSG_FAILURE([pthread.h header file is not installed])
             ])
        ],
        [
            AC_MSG_FAILURE([pthread library is not installed])
        ])

    # Solaris needs -lsocket, -lnsl and -lrt, Haiku needs -lnetwork
    AC_SEARCH_LIBS([gethostbyname], [nsl])
    AC_SEARCH_LIBS([getsockopt], [socket network])
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

fi
# End of Windows/Unix configuration.

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
AC_CHECK_TYPES([ptrdiff_t], [],
  [AC_DEFINE_UNQUOTED([ptrdiff_t], [int],
     [Define to `int' if <sys/types.h> does not define.])])


AC_CHECK_SIZEOF(void*)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long long)
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(float)

AC_C_BIGENDIAN

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_GETGROUPS
AC_FUNC_GETPGRP
AC_FUNC_SELECT_ARGTYPES
AC_FUNC_STAT
AC_FUNC_STRTOD
AC_CHECK_FUNCS([dlopen strtod dtoa getpagesize sigaltstack mmap mkstemp])
## There does not seem to be a declaration for fpsetmask in mingw64.
AC_CHECK_DECLS([fpsetmask], [], [], [[#include <ieeefp.h>]])
AC_CHECK_FUNCS([sysctl sysctlbyname])
AC_CHECK_FUNCS([localtime_r gmtime_r])
AC_CHECK_FUNCS([ctermid tcdrain])
AC_CHECK_FUNCS([_ftelli64])
AC_CHECK_FUNCS([pthread_jit_write_protect_np])

# Where are the registers when we get a signal?  Used in time profiling.
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

# Check which CPU we're building for.  Can we use a native pre-built compiler
# or do we need to fall back to the interpreter?  Most of these settings are to tweak
# the ELF exporter.

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
      aarch64* | arm*)
            # MacOS seems to return "arm" on 64-bit ARM.
            if test X"$ac_cv_sizeof_voidp" = X8; then
                AC_DEFINE([HOSTARCHITECTURE_AARCH64], [1], [Define if the host is an ARM (64-bit)])
                polyarch=aarch64
            else
                AC_DEFINE([HOSTARCHITECTURE_ARM], [1], [Define if the host is an ARM (32-bit)])
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
      riscv32)
            AC_DEFINE([HOSTARCHITECTURE_RISCV32], [1], [Define if the host is a RISC-V (32-bit)])
            polyarch=interpret
            ;;
      riscv64)
            AC_DEFINE([HOSTARCHITECTURE_RISCV64], [1], [Define if the host is a RISC-V (64-bit)])
            polyarch=interpret
            ;;
      loongarch64)
            AC_DEFINE([HOSTARCHITECTURE_LOONGARCH64], [1], [Define if the host is a LoongArch (64-bit)])
            polyarch=interpret
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

# Check for libffi only if we're building the interpreted version
if test "x$polyarch" = "xinterpret" ; then
    AC_CHECK_LIB([ffi], [ffi_prep_closure_loc])
    AC_CHECK_HEADERS([ffi.h])
fi

if test  X"$ac_cv_sizeof_voidp" = X8 ; then
    bootstrap64="yes"
else
    bootstrap64="no"
fi

# Build 32-bit in 64-bits.  This is only allowed when building on native 64-bit X86.
AC_ARG_ENABLE([compact32bit],
        [AS_HELP_STRING([--enable-compact32bit],
            [use 32-bit values rather than native 64-bits.])])
AC_ARG_ENABLE([compact32bit_large],
	[AS_HELP_STRING([--enable-compact32bit_large],
            [use 32-bit values with a larger heap size.])])


if test "x$enable_compact32bit_large" = "xyes"; then
    if test X"$ac_cv_sizeof_voidp" = X8 ; then
        AC_DEFINE([POLYML32IN64], [4], [Define if this should use 32-bit values in 64-bit architectures and set to the unit size])
        bootstrap64="no"
    else
        AC_MSG_ERROR([--enable-compact32bit is only available on a 64-bit architecture])
    fi
elif test "x$enable_compact32bit" = "xyes"; then
    if test X"$ac_cv_sizeof_voidp" = X8 ; then
        AC_DEFINE([POLYML32IN64], [2], [Define if this should use 32-bit values in 64-bit architectures and set to the unit size])
        bootstrap64="no"
    else
        AC_MSG_ERROR([--enable-compact32bit is only available on a 64-bit architecture])
    fi
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
AM_CONDITIONAL([ARCHARM_64], [test "$polyarch" = aarch64])
AM_CONDITIONAL([ARCHINTERPRET], [test "$polyarch" = interpret])
# Are we bootstrapping from the 32-bit or 64-bit pre-built compiler?
AM_CONDITIONAL([BOOT64], [test "$bootstrap64" = yes])
# If we are targeting Windows rather than *nix we need the pre=built compiler with Windows conventions.
AM_CONDITIONAL([WINDOWSCALLCONV], [test "$poly_use_windowscc" = yes])

# This is true if we are building for native Windows rather than Cygwin
AM_CONDITIONAL([NATIVE_WINDOWS], [test "$poly_native_windows" = yes])
AM_CONDITIONAL([NO_UNDEFINED], [test "$poly_no_undefined" = yes])
AM_CONDITIONAL([WINDOWSGUI], [test x$poly_windows_enablegui = xtrue])

# If we're building only the static version of libpolyml
# then polyc and polyml.pc have to include the dependent libraries.
dependentlibs=""
if test "${enable_shared}" != yes; then
    dependentlibs=${LIBS}
fi

AC_SUBST([dependentlibs], ["$dependentlibs"])

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

# Control whether to use emulation rather than calls for long arbitrary precision operations
AC_ARG_ENABLE([compact-intinf],
    [AS_HELP_STRING([--enable-compact-intinf], [use a compact format for long arbitrary precision operations])],
    [case "${enableval}" in
       no) compactintinf=no ;;
       yes) compactintinf=yes ;;
       *) AC_MSG_ERROR([bad value ${enableval} for --enable-compact-intinf]) ;;
        esac],
        [compactintinf=no])

AM_CONDITIONAL([COMPACTINTINF], [test "$compactintinf" = "yes"])

# These are needed for building in a separate build directory, as they are
# referenced from exportPoly.sml.
AC_CONFIG_COMMANDS([basis], [test -e basis || ln -sf ${ac_top_srcdir}/basis .])
AC_CONFIG_COMMANDS([mlsource], [test -e mlsource || ln -sf ${ac_top_srcdir}/mlsource .])

AC_CONFIG_FILES([Makefile libpolyml/Makefile libpolyml/polyml.pc libpolymain/Makefile modules/Makefile modules/IntInfAsInt/Makefile])
AC_CONFIG_FILES([polyc], [chmod +x polyc])

AC_OUTPUT
