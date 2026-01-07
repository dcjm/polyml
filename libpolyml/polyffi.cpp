/*
    Title:  New Foreign Function Interface

    Copyright (c) 2015, 2018, 2019  David C.J. Matthews

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

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "globals.h"
// TODO: Do we need this??
// We need to include globals.h before <new> in mingw64 otherwise
// it messes up POLYUFMT/POLYSFMT.

#include <new>

#include "arb.h"
#include "save_vec.h"
#include "polyffi.h"
#include "run_time.h"
#include "sys.h"
#include "processes.h"
#include "polystring.h"

#if (defined(_WIN32))
#include <windows.h>
#include "winstartup.h" /* For hApplicationInstance. */
#endif

#include "scanaddrs.h"
#include "diagnostics.h"
#include "rts_module.h"
#include "rtsentry.h"

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySizeFloat();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySizeDouble();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySizeShort();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySizeInt();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySizeLong();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySizeLonglong();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySizeSsize();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySizeSize();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySizePtrdiff();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySizeIntptr();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySizeUintptr();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyFFIGetError(POLYUNSIGNED addr);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyFFISetError(POLYUNSIGNED err);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyFFICreateExtFn(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyFFICreateExtData(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL void PolyFFICallbackException(POLYUNSIGNED exnMessage);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyFFIMalloc(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyFFIFree(POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyFFILoadLibrary(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyFFILoadExecutable(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyFFIUnloadLibrary(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyFFIGetSymbolAddress(POLYUNSIGNED threadId, POLYUNSIGNED moduleAddress, POLYUNSIGNED symbolName);
}
static Handle toSysWord(TaskData *taskData, void *p)
{
    return Make_sysword(taskData, (uintptr_t)p);
}

// Malloc memory - Needs to allocate the SysWord.word value on the heap.
POLYUNSIGNED PolyFFIMalloc(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        POLYUNSIGNED size = getPolyUnsigned(taskData, PolyWord::FromUnsigned(arg));
        result = toSysWord(taskData, malloc(size));
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Free memory.  Not currently used: freed memory is just added back to the free list.
POLYUNSIGNED PolyFFIFree(POLYUNSIGNED arg)
{
    void* mem = *(void**)(PolyWord::FromUnsigned(arg).AsObjPtr());
    free(mem);
    return TAGGED(0).AsUnsigned();
}

POLYUNSIGNED PolyFFILoadLibrary(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        TempString libName(PolyWord::FromUnsigned(arg));
#if (defined(_WIN32))
        HINSTANCE lib = LoadLibrary(libName);
        if (lib == NULL)
        {
            char buf[256];
#if (defined(UNICODE))
            _snprintf(buf, sizeof(buf), "Loading <%S> failed. Error %lu", (LPCTSTR)libName, GetLastError());
#else
            _snprintf(buf, sizeof(buf), "Loading <%s> failed. Error %lu", (const char*)libName, GetLastError());
#endif
            buf[sizeof(buf) - 1] = 0; // Terminate just in case
            raise_exception_string(taskData, EXC_foreign, buf);
        }
#else
        void* lib = dlopen(libName, RTLD_LAZY);
        if (lib == NULL)
        {
            char buf[256];
            snprintf(buf, sizeof(buf), "Loading <%s> failed: %s", (const char*)libName, dlerror());
            buf[sizeof(buf) - 1] = 0; // Terminate just in case
            raise_exception_string(taskData, EXC_foreign, buf);
        }
#endif
        result = toSysWord(taskData, lib);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Get the address of the executable as a library.
POLYUNSIGNED PolyFFILoadExecutable(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
#if (defined(_WIN32))
        HINSTANCE lib = hApplicationInstance;
#else
        void* lib = dlopen(NULL, RTLD_LAZY);
        if (lib == NULL)
        {
            char buf[256];
            snprintf(buf, sizeof(buf), "Loading address of executable failed: %s", dlerror());
            buf[sizeof(buf) - 1] = 0; // Terminate just in case
            raise_exception_string(taskData, EXC_foreign, buf);
        }
#endif
        result = toSysWord(taskData, lib);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Unload library - Is this actually going to be used?
POLYUNSIGNED PolyFFIUnloadLibrary(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();

    try {
#if (defined(_WIN32))
        HMODULE hMod = *(HMODULE*)(PolyWord::FromUnsigned(arg).AsObjPtr());
        if (!FreeLibrary(hMod))
            raise_syscall(taskData, "FreeLibrary failed", GetLastError());
#else
        void* lib = *(void**)(PolyWord::FromUnsigned(arg).AsObjPtr());
        if (dlclose(lib) != 0)
        {
            char buf[256];
            snprintf(buf, sizeof(buf), "dlclose failed: %s", dlerror());
            buf[sizeof(buf) - 1] = 0; // Terminate just in case
            raise_exception_string(taskData, EXC_foreign, buf);
        }
#endif
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

// Load the address of a symbol from a library.
POLYUNSIGNED PolyFFIGetSymbolAddress(POLYUNSIGNED threadId, POLYUNSIGNED moduleAddress, POLYUNSIGNED symbolName)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        TempCString symName(PolyWord::FromUnsigned(symbolName));
#if (defined(_WIN32))
        HMODULE hMod = *(HMODULE*)(PolyWord::FromUnsigned(moduleAddress).AsObjPtr());
        void* sym = (void*)GetProcAddress(hMod, symName);
        if (sym == NULL)
        {
            char buf[256];
            _snprintf(buf, sizeof(buf), "Loading symbol <%s> failed. Error %lu", (LPCSTR)symName, GetLastError());
            buf[sizeof(buf) - 1] = 0; // Terminate just in case
            raise_exception_string(taskData, EXC_foreign, buf);
        }
#else
        void* lib = *(void**)(PolyWord::FromUnsigned(moduleAddress).AsObjPtr());
        void* sym = dlsym(lib, symName);
        if (sym == NULL)
        {
            char buf[256];
            snprintf(buf, sizeof(buf), "load_sym <%s> : %s", (const char*)symName, dlerror());
            buf[sizeof(buf) - 1] = 0; // Terminate just in case
            raise_exception_string(taskData, EXC_foreign, buf);
        }
#endif
        result = toSysWord(taskData, sym);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// These functions are needed in the compiler
POLYUNSIGNED PolySizeFloat()
{
    return TAGGED((POLYSIGNED)sizeof(float)).AsUnsigned();
}

POLYUNSIGNED PolySizeDouble()
{
    return TAGGED((POLYSIGNED)sizeof(double)).AsUnsigned();
}

POLYUNSIGNED PolySizeShort()
{
    return TAGGED((POLYSIGNED)sizeof(short)).AsUnsigned();
}

POLYUNSIGNED PolySizeInt()
{
    return TAGGED((POLYSIGNED)sizeof(int)).AsUnsigned();
}

POLYUNSIGNED PolySizeLong()
{
    return TAGGED((POLYSIGNED)sizeof(long)).AsUnsigned();
}

POLYUNSIGNED PolySizeLonglong()
{
    return TAGGED((POLYSIGNED)sizeof(long long)).AsUnsigned();
}

POLYUNSIGNED PolySizeSsize()
{
    return TAGGED((POLYSIGNED)sizeof(ssize_t)).AsUnsigned();
}

POLYUNSIGNED PolySizeSize()
{
    return TAGGED((POLYSIGNED)sizeof(size_t)).AsUnsigned();
}

POLYUNSIGNED PolySizePtrdiff()
{
    return TAGGED((POLYSIGNED)sizeof(ptrdiff_t)).AsUnsigned();
}

POLYUNSIGNED PolySizeIntptr()
{
    return TAGGED((POLYSIGNED)sizeof(intptr_t)).AsUnsigned();
}

POLYUNSIGNED PolySizeUintptr()
{
    return TAGGED((POLYSIGNED)sizeof(uintptr_t)).AsUnsigned();
}

// Get either errno or GetLastError
POLYUNSIGNED PolyFFIGetError(POLYUNSIGNED addr)
{
#if (defined(_WIN32))
    PolyWord::FromUnsigned(addr).AsObjPtr()->Set(0, PolyWord::FromUnsigned(GetLastError()));
#else
    PolyWord::FromUnsigned(addr).AsObjPtr()->Set(0, PolyWord::FromUnsigned((POLYUNSIGNED)errno));
#endif
    return 0;
}

// The argument is a SysWord.word value i.e. the address of a byte cell.
POLYUNSIGNED PolyFFISetError(POLYUNSIGNED err)
{
#if (defined(_WIN32))
    SetLastError((DWORD)(PolyWord::FromUnsigned(err).AsObjPtr()->Get(0).AsUnsigned()));
#else
    errno = PolyWord::FromUnsigned(err).AsObjPtr()->Get(0).AsSigned();
#endif
    return 0;
}

// Create an external function reference.  The value returned has space for
// an address followed by the name of the external symbol.  Because the
// address comes at the beginning it can be used in the same way as the
// SysWord value returned by the get-symbol call from a library.
POLYUNSIGNED PolyFFICreateExtFn(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = creatEntryPointObject(taskData, pushedArg, true);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Create an external reference to data.  On a small number of platforms
// different forms of relocation are needed for data and for functions.
POLYUNSIGNED PolyFFICreateExtData(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = creatEntryPointObject(taskData, pushedArg, false);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}


// Called if a callback raises an exception.  There's nothing we
// can do because we don't have anything to pass back to C.
void PolyFFICallbackException(POLYUNSIGNED exnMessage)
{
    TempCString exception(PolyWord::FromUnsigned(exnMessage));
    Crash("An ML function called from foreign code raised an exception: (%s).  Unable to continue.", (const char *)exception);
}

struct _entrypts polyFFIEPT[] =
{
    { "PolySizeFloat",                  (polyRTSFunction)&PolySizeFloat},
    { "PolySizeDouble",                 (polyRTSFunction)&PolySizeDouble},
    { "PolySizeShort",                  (polyRTSFunction)&PolySizeShort},
    { "PolySizeInt",                    (polyRTSFunction)&PolySizeInt},
    { "PolySizeLong",                   (polyRTSFunction)&PolySizeLong},
    { "PolySizeLonglong",               (polyRTSFunction)&PolySizeLonglong},
    { "PolySizeSsize",                  (polyRTSFunction)&PolySizeSsize},
    { "PolySizeSize",                   (polyRTSFunction)&PolySizeSize},
    { "PolySizePtrdiff",                (polyRTSFunction)&PolySizePtrdiff},
    { "PolySizeIntptr",                 (polyRTSFunction)&PolySizeIntptr},
    { "PolySizeUintptr",                (polyRTSFunction)&PolySizeUintptr},
    { "PolyFFIGetError",                (polyRTSFunction)&PolyFFIGetError},
    { "PolyFFISetError",                (polyRTSFunction)&PolyFFISetError},
    { "PolyFFICreateExtFn",             (polyRTSFunction)&PolyFFICreateExtFn},
    { "PolyFFICreateExtData",           (polyRTSFunction)&PolyFFICreateExtData },
    { "PolyFFICallbackException",       (polyRTSFunction)&PolyFFICallbackException },
    { "PolyFFIMalloc",                  (polyRTSFunction)&PolyFFIMalloc },
    { "PolyFFIFree",                    (polyRTSFunction)&PolyFFIFree },
    { "PolyFFILoadLibrary",             (polyRTSFunction)&PolyFFILoadLibrary },
    { "PolyFFILoadExecutable",          (polyRTSFunction)&PolyFFILoadExecutable },
    { "PolyFFIUnloadLibrary",           (polyRTSFunction)&PolyFFIUnloadLibrary },
    { "PolyFFIGetSymbolAddress",        (polyRTSFunction)&PolyFFIGetSymbolAddress },

    { NULL, NULL} // End of list.
};

