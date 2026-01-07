/*
    Title:  run_time.h

    Copyright (c) 2000-9
        Cambridge University Technical Services Limited
    Further development Copyright David C.J. Matthews 2016-17, 2025

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
extern PolyObject *alloc(TaskData *taskData, uintptr_t words, unsigned flags = 0);
extern Handle alloc_and_save(TaskData *taskData, uintptr_t words, unsigned flags = 0);

extern Handle makeList(TaskData *taskData, int count, char *p, int size, void *arg,
                       Handle (mkEntry)(TaskData *, void*, char*));

// Exceptions without an argument e.g. Size and Overflow
NORETURNFN(extern void raiseException0WithLocation(TaskData *taskData, int id, const char *file, int line));
#define raise_exception0(taskData, id) raiseException0WithLocation(taskData, id, __FILE__, __LINE__)

// Exceptions with a string argument e.g. Foreign
NORETURNFN(extern void raiseExceptionStringWithLocation(TaskData *taskData, int id, const char *str, const char *file, int line));
#define raise_exception_string(taskData, id, str) raiseExceptionStringWithLocation(taskData, id, str, __FILE__, __LINE__)

// Fail exception
NORETURNFN(extern void raiseExceptionFailWithLocation(TaskData *taskData, const char *str, const char *file, int line));
#define raise_fail(taskData, errmsg) raiseExceptionFailWithLocation(taskData, errmsg, __FILE__, __LINE__)

// Syscall exception.  The errmsg argument is ignored and replaced with the standard string unless err is zero.
NORETURNFN(extern void raiseSycallWithLocation(TaskData *taskData, const char *errmsg, int err, const char *file, int line));
#define raise_syscall(taskData, errMsg, err) raiseSycallWithLocation(taskData, errMsg, err, __FILE__, __LINE__)

// Set the exception packet as the result of a bad::alloc exception.
// Does not throw a further C++ exception.
extern void setMemoryExceptionWithLocation(TaskData* taskData, const char* file, int line);
#define setMemoryException(taskData) setMemoryExceptionWithLocation(taskData, __FILE__, __LINE__)

// Construct an exception packet for future use
poly_exn *makeExceptionPacket(TaskData *taskData, int id);

// Check to see that there is space in the stack.  May GC and may raise a C++ exception.
extern void CheckAndGrowStack(TaskData *mdTaskData, uintptr_t minSize);

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
extern Handle MakeVolatileWord(TaskData *taskData, void *p);
extern Handle MakeVolatileWord(TaskData *taskData, uintptr_t p);

extern struct _entrypts runTimeEPT[];

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

// Helper class to close files on exit.
class AutoClose {
public:
    AutoClose(FILE* f = 0) : m_file(f) {}
    ~AutoClose() { if (m_file) ::fclose(m_file); }

    operator FILE* () { return m_file; }
    FILE* operator = (FILE* p) { return (m_file = p); }

private:
    FILE* m_file;
};

#endif /* _RUNTIME_H_DEFINED */
