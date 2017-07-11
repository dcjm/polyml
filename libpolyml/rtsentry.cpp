/*
    Title:  rtsentry.cpp - Entry points to the run-time system

    Copyright (c) 2016 David C. J. Matthews

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
#include "foreign.h"
#include "machine_dep.h"
#include "exporter.h"

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
    foreignEPT,
    machineSpecificEPT,
    exporterEPT,
    NULL
};

extern "C" {
#ifdef _MSC_VER
    __declspec(dllexport)
#endif
    POLYUNSIGNED PolyCreateEntryPointObject(PolyObject *threadId, PolyWord arg);
};

// Create an entry point containing the address of the entry and the
// string name.  Having the string in there allows us to export the entry.
Handle creatEntryPointObject(TaskData *taskData, Handle entryH)
{
    TempCString entryName(Poly_string_to_C_alloc(entryH->WordP()));
    if ((const char *)entryName == 0) raise_syscall(taskData, "Insufficient memory", ENOMEM);
    // Create space for the address followed by the name as a C string.
    POLYUNSIGNED space = 1 + (strlen(entryName) + 1 + sizeof(PolyWord) - 1) / sizeof(PolyWord);
    // Allocate a byte, weak, mutable, no-overwrite cell.  It's not clear if
    // it actually needs to be mutable but if it is it needs to be no-overwrite.
    Handle refH = alloc_and_save(taskData, space, F_BYTE_OBJ|F_WEAK_BIT|F_MUTABLE_BIT|F_NO_OVERWRITE);
    strcpy((char*)(refH->WordP()->AsBytePtr() + sizeof(PolyWord)), entryName);
    if (! setEntryPoint(refH->WordP()))
        raise_fail(taskData, "entry point not found");
    return refH;
}

// Return the string entry point.
const char *getEntryPointName(PolyObject *p)
{
    if (p->Length() <= 1) return 0; // Doesn't contain an entry point
    return (const char *)(p->AsBytePtr() + sizeof(PolyWord));
}

// Sets the address of the entry point in an entry point object.
bool setEntryPoint(PolyObject *p)
{
    if (p->Length() == 0) return false;
    p->Set(0, PolyWord::FromSigned(0)); // Clear it by default
    if (p->Length() == 1) return false;
    const char *entryName = (const char*)(p->AsBytePtr()+sizeof(PolyWord));

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
POLYUNSIGNED PolyCreateEntryPointObject(PolyObject *threadId, PolyWord arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = creatEntryPointObject(taskData, pushedArg);
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
