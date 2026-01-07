/*
    Title:      Run-time system.
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further work copyright David C. J. Matthews 2009, 2012, 2015-18, 2025

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

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#include "globals.h"
#include "gc.h"
#include "mpoly.h"
#include "arb.h"
#include "diagnostics.h"
#include "processes.h"
#include "profiling.h"
#include "run_time.h"
#include "sys.h"
#include "polystring.h"
#include "save_vec.h"
#include "rtsentry.h"
#include "memmgr.h"

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyFullGC(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyIsBigEndian();
}

#define SAVE(x) taskData->saveVec.push(x)
#define SIZEOF(x) (sizeof(x)/sizeof(PolyWord))


// This is the storage allocator for allocating heap objects in the RTS.
PolyObject *alloc(TaskData *taskData, uintptr_t data_words, unsigned flags)
/* Allocate a number of words. */
{
    // Check the size.  This might possibly happen with a long string.
    if (data_words > MAX_OBJECT_SIZE)
        raise_exception0(taskData, EXC_size);

    POLYUNSIGNED words = (POLYUNSIGNED)data_words + 1;
    
    if (profileMode == kProfileStoreAllocation)
        taskData->addProfileCount(words);

    PolyWord *foundSpace = processes->FindAllocationSpace(taskData, words, false);
    if (foundSpace == 0)
    {
        // Failed - the thread is set to raise an exception.
        throw IOException();
    }

    PolyObject *pObj = (PolyObject*)(foundSpace + 1);
    pObj->SetLengthWord((POLYUNSIGNED)data_words, flags);
    
    // Must initialise object here, because GC doesn't clean store.
    // Is this necessary any more?  This used to be necessary when we used
    // structural equality and wanted to make sure that unused bytes were cleared.
    // N.B.  This sets the store to zero NOT TAGGED(0).
    for (POLYUNSIGNED i = 0; i < data_words; i++) pObj->Set(i, PolyWord::FromUnsigned(0));
    return pObj;
}

Handle alloc_and_save(TaskData *taskData, uintptr_t size, unsigned flags)
/* Allocate and save the result on the vector. */
{
    return taskData->saveVec.push(alloc(taskData, size, flags));
}

POLYUNSIGNED PolyFullGC(POLYUNSIGNED threadId)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();

    try {
        // Can this raise an exception e.g. if there is insufficient memory?
        FullGC(taskData);
    } catch (...) { } // If an ML exception is raised

    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned(); // Returns unit.
}


/******************************************************************************/
/*                                                                            */
/*      Error Messages                                                        */
/*                                                                            */
/******************************************************************************/


// Return the handle to a string error message.  This will return
// something like "Unknown error" from strerror if it doesn't match
// anything.
Handle errorMsg(TaskData *taskData, int err)
{
#if (defined(_WIN32))
    LPTSTR lpMsg = NULL;
    TCHAR *p;
    if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM |
            FORMAT_MESSAGE_ALLOCATE_BUFFER |
            FORMAT_MESSAGE_IGNORE_INSERTS,
            NULL, (DWORD)err, 0, (LPTSTR)&lpMsg, 1, NULL) > 0)
    {
        /* The message is returned with CRLF at the end.  Remove them. */
        for (p = lpMsg; *p != '\0' && *p != '\n' && *p != '\r'; p++);
        *p = '\0';
        Handle res = SAVE(C_string_to_Poly(taskData, lpMsg));
        LocalFree(lpMsg);
        return res;
    }
#endif
    // Unix and unknown Windows errors.
    return SAVE(C_string_to_Poly(taskData, strerror(err)));
}

#define DEREFEXNHANDLE(_x)       ((poly_exn *)DEREFHANDLE(_x))

static Handle make_exn(TaskData *taskData, int id, Handle arg, const char *fileName, int lineNo)
{
    const char *exName;
    switch (id) {
    case EXC_interrupt: exName = "Interrupt"; break;
    case EXC_syserr: exName = "SysErr"; break;
    case EXC_size: exName = "Size"; break;
    case EXC_overflow: exName = "Overflow"; break;
    case EXC_divide: exName = "Div"; break;
    case EXC_conversion: exName = "Conversion"; break;
    case EXC_XWindows: exName = "XWindows"; break;
    case EXC_subscript: exName = "Subscript"; break;
    case EXC_foreign: exName = "Foreign"; break;
    case EXC_Fail: exName = "Fail"; break;
    case EXC_thread: exName = "Thread"; break;
    default: ASSERT(0); exName = "Unknown"; // Shouldn't happen.
    }
   
    Handle pushed_name = SAVE(C_string_to_Poly(taskData, exName));
    
    Handle exnHandle = alloc_and_save(taskData, SIZEOF(poly_exn));
    Handle location;
    // The location data in an exception packet is either "NoLocation" (tagged 0)
    // or the address of a record.
    if (fileName == 0)
        location = taskData->saveVec.push(TAGGED(0));
    else
    {
        Handle file = taskData->saveVec.push(C_string_to_Poly(taskData, fileName));
        Handle line = Make_fixed_precision(taskData, lineNo);
        location = alloc_and_save(taskData, 5);
        location->WordP()->Set(0, file->Word());     // file
        location->WordP()->Set(1, line->Word());     // startLine
        location->WordP()->Set(2, line->Word());     // endLine
        location->WordP()->Set(3, TAGGED(0));        // startPosition
        location->WordP()->Set(4, TAGGED(0));        // endPosition
    }
    
    DEREFEXNHANDLE(exnHandle)->ex_id   = TAGGED(id);
    DEREFEXNHANDLE(exnHandle)->ex_name = pushed_name->Word();
    DEREFEXNHANDLE(exnHandle)->arg     = arg->Word();
    DEREFEXNHANDLE(exnHandle)->ex_location = location->Word();

    return exnHandle;
}

// Create an exception packet, e.g. Interrupt, for later use.  This does not have a
// location.
poly_exn *makeExceptionPacket(TaskData *taskData, int id)
{
    Handle exn = make_exn(taskData, id, taskData->saveVec.push(TAGGED(0)), 0, 0);
    return DEREFEXNHANDLE(exn);
}

static NORETURNFN(void raise_exception(TaskData *taskData, int id, Handle arg, const char *file, int line));

void raise_exception(TaskData *taskData, int id, Handle arg, const char *file, int line)
/* Raise an exception with no arguments. */
{
    Handle exn = make_exn(taskData, id, arg, file, line);
    taskData->SetException(DEREFEXNHANDLE(exn));
    throw IOException(); /* Return to Poly code immediately. */
    /*NOTREACHED*/
}

void raiseException0WithLocation(TaskData *taskData, int id, const char *file, int line)
/* Raise an exception with no arguments. */
{
    raise_exception(taskData, id, SAVE(TAGGED(0)), file, line);
    /*NOTREACHED*/
}

void raiseExceptionStringWithLocation(TaskData *taskData, int id, const char *str, const char *file, int line)
/* Raise an exception with a C string as the argument. */
{
    raise_exception(taskData, id, SAVE(C_string_to_Poly(taskData, str)), file, line);
    /*NOTREACHED*/
}

// This is called via a macro that puts in the file name and line number.
void raiseSycallWithLocation(TaskData *taskData, const char *errmsg, int err, const char *file, int line)
{
    if (err == 0)
    {
        Handle pushed_option = SAVE(NONE_VALUE); /* NONE */
        Handle pushed_name = SAVE(C_string_to_Poly(taskData, errmsg));
        Handle pair = alloc_and_save(taskData, 2);
        DEREFHANDLE(pair)->Set(0, pushed_name->Word());
        DEREFHANDLE(pair)->Set(1, pushed_option->Word());

        raise_exception(taskData, EXC_syserr, pair, file, line);
    }
    else
    {
        Handle errornum = Make_sysword(taskData, err);
        Handle pushed_option = alloc_and_save(taskData, 1);
        DEREFHANDLE(pushed_option)->Set(0, errornum->Word()); /* SOME err */
        Handle pushed_name = errorMsg(taskData, err); // Generate the string.
        Handle pair = alloc_and_save(taskData, 2);
        DEREFHANDLE(pair)->Set(0, pushed_name->Word());
        DEREFHANDLE(pair)->Set(1, pushed_option->Word());

        raise_exception(taskData, EXC_syserr, pair, file, line);
    }
}

void raiseExceptionFailWithLocation(TaskData *taskData, const char *str, const char *file, int line)
{
    raiseExceptionStringWithLocation(taskData, EXC_Fail, str, file, line);
}

// Set the exception packet as the result of a bad::alloc exception.
// Does not throw a further C++ exception.
void setMemoryExceptionWithLocation(TaskData* taskData, const char* file, int line)
{
    Handle str = SAVE(C_string_to_Poly(taskData, "Insufficient Memory: C++ allocation failed"));
    Handle exn = make_exn(taskData, EXC_Fail, str, file, line);
    taskData->SetException(DEREFEXNHANDLE(exn));
}

/* "Polymorphic" function to generate a list. */
Handle makeList(TaskData *taskData, int count, char *p, int size, void *arg,
                       Handle (mkEntry)(TaskData *, void*, char*))
{
    Handle saved = taskData->saveVec.mark();
    Handle list = SAVE(ListNull);
    /* Start from the end of the list. */
    p += count*size;
    while (count > 0)
    {
        Handle value, next;
        p -= size; /* Back up to the last entry. */
        value = mkEntry(taskData, arg, p);
        next  = alloc_and_save(taskData, SIZEOF(ML_Cons_Cell));

        DEREFLISTHANDLE(next)->h = value->Word();
        DEREFLISTHANDLE(next)->t = list->Word();

        taskData->saveVec.reset(saved);
        list = SAVE(next->Word());
        count--;
    }
    return list;
}

void CheckAndGrowStack(TaskData *taskData, uintptr_t minSize)
/* Expands the current stack if it has grown. We cannot shrink a stack segment
   when it grows smaller because the frame is checked only at the beginning of
   a function to ensure that there is enough space for the maximum that can
   be allocated. */
{
    /* Get current size of new stack segment. */
    uintptr_t old_len = taskData->stack->spaceSize();

    if (old_len >= minSize) return; /* Ok with present size. */

    // If it is too small double its size.
    uintptr_t new_len; /* New size */
    for (new_len = old_len; new_len < minSize; new_len *= 2);
    uintptr_t limitSize = getPolyUnsigned(taskData, taskData->threadObject->mlStackSize);

    // Do not grow the stack if its size is already too big.
    if ((limitSize != 0 && old_len >= limitSize) || ! gMem.GrowOrShrinkStack(taskData, new_len))
    {
        /* Cannot expand the stack any further. */
        extern FILE *polyStderr;
        fprintf(polyStderr, "Warning - Unable to increase stack - interrupting thread\n");
        if (debugOptions & DEBUG_THREADS)
            Log("THREAD: Unable to grow stack for thread %p from %lu to %lu\n", taskData, old_len, new_len);
        // We really should do this only if the thread is handling interrupts
        // asynchronously.  On the other hand what else do we do?
        taskData->SetException(processes->GetInterrupt());
    }
    else
    {
        if (debugOptions & DEBUG_THREADS)
            Log("THREAD: Growing stack for thread %p from %lu to %lu\n", taskData, old_len, new_len);
    }
}

Handle Make_fixed_precision(TaskData *taskData, int val)
{
#if (SIZEOF_INT >= SIZEOF_POLYWORD)
    // This range check may produce a warning if int is 32 bits and PolyWord is 64-bits.
    if (val > MAXTAGGED || val < -MAXTAGGED-1)
        raise_exception0(taskData, EXC_overflow);
#endif
    return taskData->saveVec.push(TAGGED(val));
}

Handle Make_fixed_precision(TaskData *taskData, unsigned uval)
{
#if (SIZEOF_INT >= SIZEOF_POLYWORD)
    if (uval > MAXTAGGED)
        raise_exception0(taskData, EXC_overflow);
#endif
    return taskData->saveVec.push(TAGGED(uval));
}

Handle Make_fixed_precision(TaskData *taskData, long val)
{
    if (val > MAXTAGGED || val < -MAXTAGGED-1)
        raise_exception0(taskData, EXC_overflow);
    return taskData->saveVec.push(TAGGED(val));
}

Handle Make_fixed_precision(TaskData *taskData, unsigned long uval)
{
    if (uval > MAXTAGGED)
        raise_exception0(taskData, EXC_overflow);
    return taskData->saveVec.push(TAGGED(uval));
}

#ifdef HAVE_LONG_LONG
Handle Make_fixed_precision(TaskData *taskData, long long val)
{
    if (val > MAXTAGGED || val < -MAXTAGGED-1)
        raise_exception0(taskData, EXC_overflow);
    return taskData->saveVec.push(TAGGED((POLYSIGNED)val));
}

Handle Make_fixed_precision(TaskData *taskData, unsigned long long uval)
{
    if (uval > MAXTAGGED)
        raise_exception0(taskData, EXC_overflow);
    return taskData->saveVec.push(TAGGED((POLYUNSIGNED)uval));
}
#endif

Handle Make_sysword(TaskData *taskData, uintptr_t p)
{
    Handle result = alloc_and_save(taskData, sizeof(uintptr_t)/sizeof(PolyWord), F_BYTE_OBJ);
    *(uintptr_t*)(result->Word().AsCodePtr()) = p;
    return result;
}

// A volatile ref is used for data that is not valid in a different session.
// When loaded from a saved state it is cleared to zero.
Handle MakeVolatileWord(TaskData *taskData, void *p)
{
    Handle result = alloc_and_save(taskData,
            WORDS(SIZEOF_VOIDP), F_BYTE_OBJ | F_WEAK_BIT | F_MUTABLE_BIT | F_NO_OVERWRITE);
    *(void**)(result->Word().AsCodePtr()) = p;
    return result;
}

Handle MakeVolatileWord(TaskData *taskData, uintptr_t p)
{
    return MakeVolatileWord(taskData, (void*)p);
}

// This is used to determine the endian-ness that Poly/ML is running under.
// It's really only needed for the interpreter.  In particular the pre-built
// compiler may be running under either byte order and has to check at
// run-time.
POLYUNSIGNED PolyIsBigEndian()
{
#ifdef WORDS_BIGENDIAN
    return TAGGED(1).AsUnsigned();
#else
    return TAGGED(0).AsUnsigned();
#endif
}

struct _entrypts runTimeEPT[] =
{
    { "PolyFullGC",                     (polyRTSFunction)&PolyFullGC},
    { "PolyIsBigEndian",                (polyRTSFunction)&PolyIsBigEndian},

    { NULL, NULL} // End of list.
};
