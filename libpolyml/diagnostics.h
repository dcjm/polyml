/*
    Title:  Header for diagnostics.c

    Copyright (c) 2011-12, 2015, 2017 David C.J. Matthews

    Copyright (c) 2000
        Cambridge University Technical Services Limited

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

#ifndef _DIAGNOSTICS_H_USED
#define _DIAGNOSTICS_H_USED
#include "noreturn.h"
#include "globals.h"

#ifdef HAVE_TCHAR_H
#include <tchar.h>
#else
typedef char TCHAR;
#endif

NORETURNFN(extern void Exit(const char *, ...));
NORETURNFN(extern void Crash(const char *, ...));

NORETURNFN(extern void ExitWithError(const char *, int err));

extern void SetLogFile(const TCHAR *fileName);
extern void Log(const char *, ...);
extern void LogSize(POLYUNSIGNED wordSize);

extern unsigned    debugOptions; // debugging  flags

// Values for debugging flags
#define DEBUG_CHECK_OBJECTS 0x0001      // Check that addresses are valid.
#define DEBUG_GC            0x0002      // Summary GC debugging output
#define DEBUG_MEMMGR        0x0004      // Memory manager output
#define DEBUG_GC_DETAIL     0x0008      // Detailed GC debugging output
#define DEBUG_THREADS       0x0010      // Thread-related output
#define DEBUG_GCTASKS       0x0020      // GC task farm
#define DEBUG_HEAPSIZE      0x0040      // Heap resizing
#define DEBUG_X             0x0080      // X-Windows debugging
#define DEBUG_SHARING       0x0100      // Diagnostics for share-common-data
#define DEBUG_CONTENTION    0x0200      // Information about contended locks
#define DEBUG_RTSCALLS      0x0400      // Information about run-time calls. Not currently used.
#define DEBUG_GC_ENHANCED   0x0800      // Intermediate level GC output
#define DEBUG_SAVING        0x1000      // Saving state and exporting

#endif
