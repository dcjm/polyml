/*
    Title:  rtsentry.cpp - Entry points to the run-time system

    Copyright (c) 2016, 2017, 2025 David C. J. Matthews

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

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)

#else
#define ASSERT(x)
#endif

#include "globals.h"
#include "rtsentry.h"
#include "save_vec.h"
#include "processes.h"
#include "run_time.h"
#include "polystring.h"
#include "arb.h"
#include "basicio.h"
#include "polyffi.h"
#include "xwindows.h"
#include "os_specific.h"
#include "timing.h"
#include "sighandler.h"
#include "sharedata.h"
#include "run_time.h"
#include "reals.h"
#include "profiling.h"
#include "processes.h"
#include "process_env.h"
#include "poly_specific.h"
#include "objsize.h"
#include "network.h"
#include "machine_dep.h"
#include "exporter.h"
#include "statistics.h"
#include "savestate.h"
#include "bytecode.h"
#include "modules.h"

extern struct _entrypts rtsCallEPT[];

static entrypts entryPointTable[] =
{
    rtsCallEPT,
    arbitraryPrecisionEPT,
    basicIOEPT,
    polyFFIEPT,
    xwindowsEPT,
    osSpecificEPT,
    timingEPT,
    sigHandlerEPT,
    shareDataEPT,
    runTimeEPT,
    realsEPT,
    profilingEPT,
    processesEPT,
    processEnvEPT,
    polySpecificEPT,
    objSizeEPT,
    networkingEPT,
    exporterEPT,
    statisticsEPT,
    savestateEPT,
    machineSpecificEPT,
    byteCodeEPT,
    modulesEPT,
    NULL
};

extern "C" {
#ifdef _MSC_VER
    __declspec(dllexport)
#endif
    POLYUNSIGNED PolyCreateEntryPointObject(POLYUNSIGNED threadId, POLYUNSIGNED arg);
};

// Create an entry point containing the address of the entry and the
// string name.  Having the string in there allows us to export the entry.
Handle creatEntryPointObject(TaskData *taskData, Handle entryH, bool isFuncPtr)
{
    TempCString entryName(Poly_string_to_C_alloc(entryH->Word()));
    if ((const char *)entryName == 0) raise_syscall(taskData, "Insufficient memory", ENOMEM);
    // Create space for the address followed by the name as a C string.
    uintptr_t space = 1 + (strlen(entryName) + 1 + (isFuncPtr ? 0 : 1) + sizeof(polyRTSFunction*) - 1) / sizeof(PolyWord);
    // Allocate a byte, weak, mutable, no-overwrite cell.  It's not clear if
    // it actually needs to be mutable but if it is it needs to be no-overwrite.
    Handle refH = alloc_and_save(taskData, space, F_BYTE_OBJ|F_WEAK_BIT|F_MUTABLE_BIT|F_NO_OVERWRITE);
    PolyObject *p = refH->WordP();
    *(polyRTSFunction*)p = 0; // Clear it
    char *entryPtr = (char*)(p->AsBytePtr() + sizeof(polyRTSFunction*));
    if (! isFuncPtr) *entryPtr++ = 1; // Put in a type entry
    strcpy(entryPtr, entryName);
    return refH;
}

// Return the string entry point.
const char *getEntryPointName(PolyObject *p, bool *isFuncPtr)
{
    if (p->Length() <= sizeof(polyRTSFunction*)/sizeof(PolyWord)) return 0; // Doesn't contain an entry point
    const char *entryPtr = (const char*)(p->AsBytePtr() + sizeof(polyRTSFunction*));
    *isFuncPtr = *entryPtr != 1; // If the type is 1 it is a data entry point
    if (*entryPtr < ' ') entryPtr++; // Skip the type byte
    return entryPtr;
}

// Sets the address of the entry point in an entry point object.
bool setEntryPoint(PolyObject *p)
{
    if (p->Length() == 0) return false;
    *(polyRTSFunction*)p = 0; // Clear it by default
    if (p->Length() == 1) return false;
    const char *entryName = (const char*)(p->AsBytePtr()+sizeof(polyRTSFunction*));
    if (*entryName < ' ') entryName++; // Skip the type byte

    // Search the entry point table list.
    for (entrypts *ept=entryPointTable; *ept != NULL; ept++)
    {
        entrypts entryPtTable = *ept;
        if (entryPtTable != 0)
        {
            for (struct _entrypts *ep = entryPtTable; ep->entry != NULL; ep++)
            {
                if (strcmp(entryName, ep->name) == 0)
                {
                    polyRTSFunction entry = ep->entry;
                    *(polyRTSFunction*)p = entry;
                    return true;
                }
            }
        }
    }

    return false;
}

// External call
POLYUNSIGNED PolyCreateEntryPointObject(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = creatEntryPointObject(taskData, pushedArg, true /* Always functions */);
        if (!setEntryPoint(result->WordP()))
        {
            // Include the name of the symbol.  It's often helpful.
            char buff[100];
            strncpy(buff, "entry point not found: ", sizeof(buff) - 1);
            size_t length = strlen(buff);
            Poly_string_to_C(pushedArg->Word(), buff+ length, sizeof(buff) - length-1);
            raise_fail(taskData, buff);
        }
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

struct _entrypts rtsCallEPT[] =
{
    { "PolyCreateEntryPointObject",     (polyRTSFunction)&PolyCreateEntryPointObject},

    { NULL, NULL} // End of list.
};
