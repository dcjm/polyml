/*
    Title:  run_time.h

    Copyright (c) 2000-9
        Cambridge University Technical Services Limited
    Further development Copyright David C.J. Matthews 2016

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

#ifndef _RUNTIME_H_DEFINED
#define _RUNTIME_H_DEFINED 1

#include "globals.h" // PolyWord, PolyObject etc
#include "noreturn.h"

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class TaskData;

// Exceptions thrown by C++ code.  Indicates that the caller should not return normally.
// They now result in ML exceptions.

class IOException {
public:
    IOException() { }
};

// This exception is used in the exporter and sharedata code.  It is
// converted into an ML exception at the outer level.
class MemoryException {
public:
    MemoryException() {}
};

// A request to kill the thread raises this exception.
// This allows IO operations to handle this and unwind.
class KillException {
public:
    KillException() {}
};

/* storage allocation functions */
extern PolyObject *alloc(TaskData *taskData, POLYUNSIGNED words, unsigned flags = 0);
extern Handle alloc_and_save(TaskData *taskData, POLYUNSIGNED words, unsigned flags = 0);

extern Handle makeList(TaskData *taskData, int count, char *p, int size, void *arg,
                       Handle (mkEntry)(TaskData *, void*, char*));

/* exceptions and interrupts */
NORETURNFN(extern void raise_exception(TaskData *taskData, int id, Handle arg));
NORETURNFN(extern void raise_exception0(TaskData *taskData, int id));
NORETURNFN(extern void raise_exception_string(TaskData *taskData, int id, const char *str));
NORETURNFN(extern void raise_fail(TaskData *taskData, const char *errmsg));

// Raise OS.SysCall(OS.errorMsg err, SOME err)
NORETURNFN(extern void raiseSyscallError(TaskData *taskData, int err));
// Raise OS.SysCall(msg, NONE)
NORETURNFN(extern void raiseSyscallMessage(TaskData *taskData, const char *errmsg));
// This was the previous version.  The errmsg argument is ignored unless err is zero.
NORETURNFN(extern void raise_syscall(TaskData *taskData, const char *errmsg, int err));

Handle make_exn(TaskData *taskData, int id, Handle arg);

// Check to see that there is space in the stack.  May GC and may raise a C++ exception.
extern void CheckAndGrowStack(TaskData *mdTaskData, POLYUNSIGNED minSize);

extern Handle errorMsg(TaskData *taskData, int err);

// Create fixed precision values.
extern Handle Make_fixed_precision(TaskData *taskData, long);
extern Handle Make_fixed_precision(TaskData *taskData, unsigned long);

extern Handle Make_fixed_precision(TaskData *taskData, int);
extern Handle Make_fixed_precision(TaskData *taskData, unsigned);

#ifdef HAVE_LONG_LONG
extern Handle Make_fixed_precision(TaskData *taskData, long long);
extern Handle Make_fixed_precision(TaskData *taskData, unsigned long long);
#endif

extern Handle Make_sysword(TaskData *taskData, uintptr_t p);

extern struct _entrypts runTimeEPT[];

#endif /* _RUNTIME_H_DEFINED */
