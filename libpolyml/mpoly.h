/*
    Title:  exports signature for mpoly.c

    Copyright (c) 2000-7
        Cambridge University Technical Services Limited

    Further development copyright David C.J. Matthews 2001-12, 2015, 2019

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/
#ifndef _MPOLY_H_DEFINED
#define _MPOLY_H_DEFINED

#ifdef HAVE_TCHAR_H
#include <tchar.h>
#else
typedef char TCHAR;
#endif

#include "noreturn.h"
#include "memmgr.h"

extern struct _userOptions {
    unsigned    user_arg_count;
    TCHAR       **user_arg_strings;
    const TCHAR *programName;
    unsigned    gcthreads;    // Number of threads to use for gc
} userOptions;

class PolyWord;

NORETURNFN(extern void finish(int n));

extern char *RTSArgHelp(void);

extern ModuleId exportSignature;

#if (defined(_WIN32))
extern int polymain(int argc, TCHAR **argv, exportDescription *exports);
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

// Outout streams.  These are the same as stdOut and stdErr in Unix but
// may be redirected in Windows.
extern FILE *polyStdout, *polyStderr;

#endif /* _MPOLY_H_DEFINED */
