/*
    Title:  Header for diagnostics.c

    Copyright (c) 2011-12, 2015 David C.J. Matthews

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
#define DEBUG_CHECK_OBJECTS 0x001       // Check that addresses are valid.
#define DEBUG_GC            0x002       // Summary GC debugging output
#define DEBUG_MEMMGR        0x004       // Memory manager output
#define DEBUG_GC_DETAIL     0x008       // Detailed GC debugging output
#define DEBUG_THREADS       0x010       // Thread-related output
#define DEBUG_GCTASKS       0x020       // GC task farm
#define DEBUG_HEAPSIZE      0x040       // Heap resizing
#define DEBUG_X             0x080       // X-Windows debugging
#define DEBUG_SHARING       0x100       // Diagnostics for share-common-data
#define DEBUG_CONTENTION    0x200       // Information about contended locks
#define DEBUG_RTSCALLS      0x400       // Information about run-time calls.
#define DEBUG_GC_ENHANCED   0x800       // Intermediate level GC output

#endif
