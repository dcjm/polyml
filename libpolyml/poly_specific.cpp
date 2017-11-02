/*
    Title:  poly_specific.cpp - Poly/ML specific RTS calls.

    Copyright (c) 2006, 2015-17 David C. J. Matthews

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

/* This module is used for various run-time calls that are either in the
   PolyML structure or otherwise specific to Poly/ML. */

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "globals.h"
#include "poly_specific.h"
#include "arb.h"
#include "mpoly.h"
#include "sys.h"
#include "machine_dep.h"
#include "polystring.h"
#include "run_time.h"
#include "version.h"
#include "save_vec.h"
#include "exporter.h"
#include "version.h"
#include "sharedata.h"
#include "memmgr.h"
#include "processes.h"
#include "savestate.h"
#include "statistics.h"
#include "../polystatistics.h"
#include "gc.h"
#include "rtsentry.h"

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySpecificGeneral(PolyObject *threadId, PolyWord code, PolyWord arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetABI();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyLockMutableCode(PolyObject * threadId, PolyWord byteSeg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyCopyByteVecToCode(PolyObject *threadId, PolyWord byteVec);
}

#define SAVE(x) taskData->saveVec.push(x)

static const char *poly_runtime_system_copyright =
"Copyright (c) 2002-17 David C.J. Matthews, CUTS and contributors.";

#ifndef GIT_VERSION
#define GIT_VERSION             ""
#endif


Handle poly_dispatch_c(TaskData *taskData, Handle args, Handle code)
{
    unsigned c = get_C_unsigned(taskData, DEREFWORDHANDLE(code));
    switch (c)
    {
    case 9: // Return the GIT version if appropriate
        {
             return SAVE(C_string_to_Poly(taskData, GIT_VERSION));
        }

    case 10: // Return the RTS version string.
        {
            const char *version;
            switch (machineDependent->MachineArchitecture())
            {
            case MA_Interpreted:    version = "Portable-" TextVersion; break;
            case MA_I386:           version = "I386-" TextVersion; break;
            case MA_X86_64:         version = "X86_64-" TextVersion; break;
            default:                version = "Unknown-" TextVersion; break;
            }
            return SAVE(C_string_to_Poly(taskData, version));
        }

    case 11: // Return the RTS copyright string
        // Is this used??
        return SAVE(C_string_to_Poly(taskData, poly_runtime_system_copyright));

    case 12: // Return the architecture
        // Is this used??
        {
            const char *arch;
            switch (machineDependent->MachineArchitecture())
            {
            case MA_Interpreted:    arch = "Interpreted"; break;
            case MA_I386:           arch = "I386"; break;
            case MA_X86_64:         arch = "X86_64"; break;
            default:                arch = "Unknown"; break;
            }
            return SAVE(C_string_to_Poly(taskData, arch));
        }

    case 19: // Return the RTS argument help string.
        return SAVE(C_string_to_Poly(taskData, RTSArgHelp()));

    case 20: // Write a saved state file.
        return SaveState(taskData, args);

    case 21: // Load a saved state file and any ancestors.
        return LoadState(taskData, false, args);

    case 22: // Show the hierarchy.
        return ShowHierarchy(taskData);

    case 23: // Change the name of the immediate parent stored in a child
        return RenameParent(taskData, args);

    case 24: // Return the name of the immediate parent stored in a child
        return ShowParent(taskData, args);

    case 27: // Get number of user statistics available
        return Make_arbitrary_precision(taskData, N_PS_USER);

    case 28: // Set an entry in the user stats table.
        {
            unsigned index = get_C_unsigned(taskData, DEREFHANDLE(args)->Get(0));
            if (index >= N_PS_USER)
                raise_exception0(taskData, EXC_subscript);
            POLYSIGNED value = getPolySigned(taskData, DEREFHANDLE(args)->Get(1));
            globalStats.setUserCounter(index, value);
            Make_arbitrary_precision(taskData, 0);
        }

    case 29: // Get local statistics.
        return globalStats.getLocalStatistics(taskData);

    case 30: // Get remote statistics.  The argument is the process ID to get the statistics.
        return globalStats.getRemoteStatistics(taskData, getPolyUnsigned(taskData, DEREFHANDLE(args)));

    case 31: // Store a module
        return StoreModule(taskData, args);

    case 32: // Load a module
        return LoadModule(taskData, args);

    case 33: // Load hierarchy.  This provides a complete list of children and parents.
        return LoadState(taskData, true, args);

    case 34: // Return the system directory for modules.  This is configured differently
        // in Unix and in Windows.
#if (defined(MODULEDIR))
    return SAVE(C_string_to_Poly(taskData, MODULEDIR));
#elif (defined(_WIN32) && ! defined(__CYGWIN__))
        {
            // This registry key is configured when Poly/ML is installed using the installer.
            // It gives the path to the Poly/ML installation directory.  We return the
            // Modules subdirectory.
            HKEY hk;
            if (RegOpenKeyEx(HKEY_LOCAL_MACHINE,
                    _T("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\PolyML.exe"), 0,
                    KEY_QUERY_VALUE, &hk) == ERROR_SUCCESS)
            {
                DWORD valSize;
                if (RegQueryValueEx(hk, _T("Path"), 0, NULL, NULL, &valSize) == ERROR_SUCCESS)
                {
#define MODULEDIR _T("Modules")
                    TempString buff((TCHAR*)malloc(valSize + (_tcslen(MODULEDIR) + 1)*sizeof(TCHAR)));
                    DWORD dwType;
                    if (RegQueryValueEx(hk, _T("Path"), 0, &dwType, (LPBYTE)(LPTSTR)buff, &valSize) == ERROR_SUCCESS)
                    {
                        RegCloseKey(hk);
                        // The registry entry should end with a backslash.
                        _tcscat(buff, MODULEDIR);
                        return SAVE(C_string_to_Poly(taskData, buff));
                    }
                }
                RegCloseKey(hk);
            }
            return SAVE(C_string_to_Poly(taskData, ""));
        }
#else
        return SAVE(C_string_to_Poly(taskData, ""));
#endif

    case 106: // Lock a mutable code segment and return the executable address.
            // Legacy - used by bootstrap code only
        {
            PolyObject *codeObj = args->WordP();
            if (! codeObj->IsCodeObject() || ! codeObj->IsMutable())
                raise_fail(taskData, "Not mutable code area");
            POLYUNSIGNED segLength = codeObj->Length();
            codeObj->SetLengthWord(segLength, F_CODE_OBJ);
            machineDependent->FlushInstructionCache(codeObj, segLength * sizeof(PolyWord));
            // In the future it may be necessary to return a different address here.
            // N.B.  The code area should only have execute permission in the native
            // code version, not the interpreted version.
            return args; // Return the original address.
        }

    case 107: // Copy a byte segment into the code area and make it mutable code
              // Legacy - used by bootstrap code only
        {
            if (! args->WordP()->IsByteObject())
                raise_fail(taskData, "Not byte data area");
            while (true)
            {
                PolyObject *result = gMem.AllocCodeSpace(args->WordP());
                if (result != 0)
                    return taskData->saveVec.push(result);
                // Could not allocate - must GC.
                if (! QuickGC(taskData, args->WordP()->Length()))
                    raise_fail(taskData, "Insufficient memory");
            }
        }

    case 108:
        // Return the ABI.  For 64-bit we need to know if this is Windows.
        // Legacy - used by bootstrap code only
#if (SIZEOF_VOIDP == 8)
#ifdef _WIN32
        return taskData->saveVec.push(TAGGED(2));
#else
        return taskData->saveVec.push(TAGGED(1));
#endif
#else
        return taskData->saveVec.push(TAGGED(0));
#endif

    default:
        {
            char msg[100];
            sprintf(msg, "Unknown poly-specific function: %d", c);
            raise_exception_string(taskData, EXC_Fail, msg);
            return 0;
        }
    }
}

// General interface to poly-specific.  Ideally the various cases will be made into
// separate functions.
POLYUNSIGNED PolySpecificGeneral(PolyObject *threadId, PolyWord code, PolyWord arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedCode = taskData->saveVec.push(code);
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = poly_dispatch_c(taskData, pushedArg, pushedCode);
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Return the ABI - i.e. the calling conventions used when calling external functions.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetABI()
{
    // Return the ABI.  For 64-bit we need to know if this is Windows.
#if (SIZEOF_VOIDP == 8)
#ifdef _WIN32
    return TAGGED(2).AsUnsigned(); // 64-bit Windows
#else
    return TAGGED(1).AsUnsigned(); // 64-bit Unix
#endif
#else
    return TAGGED(0).AsUnsigned(); // 32-bit Unix and Windows
#endif
}

// Code generation - Code is initially allocated in a byte segment.  When all the
// values have been set apart from any addresses the byte segment is copied into
// a mutable code segment.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyCopyByteVecToCode(PolyObject * threadId, PolyWord byteVec)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(byteVec);
    PolyObject *result = 0;

    try {
        if (!pushedArg->WordP()->IsByteObject())
            raise_fail(taskData, "Not byte data area");
        do {
            result = gMem.AllocCodeSpace(pushedArg->WordP());
            if (result == 0)
            {
                // Could not allocate - must GC.
                if (!QuickGC(taskData, pushedArg->WordP()->Length()))
                    raise_fail(taskData, "Insufficient memory");
            }
        } while (result == 0);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return ((PolyWord)result).AsUnsigned();
}

// Code generation - Lock a mutable code segment and return the original address.
// Currently this does not allocate so other than the exception it could
// be a fast call.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyLockMutableCode(PolyObject * threadId, PolyWord byteSeg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(byteSeg);
    Handle result = 0;

    try {
        PolyObject *codeObj = pushedArg->WordP();
        if (!codeObj->IsCodeObject() || !codeObj->IsMutable())
            raise_fail(taskData, "Not mutable code area");
        POLYUNSIGNED segLength = codeObj->Length();
        codeObj->SetLengthWord(segLength, F_CODE_OBJ);
        // This is really a legacy of the PPC code-generator.
        machineDependent->FlushInstructionCache(codeObj, segLength * sizeof(PolyWord));
        // In the future it may be necessary to return a different address here.
        // N.B.  The code area should only have execute permission in the native
        // code version, not the interpreted version.
        result = pushedArg; // Return the original address.
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

struct _entrypts polySpecificEPT[] =
{
    { "PolySpecificGeneral",            (polyRTSFunction)&PolySpecificGeneral},
    { "PolyGetABI",                     (polyRTSFunction)&PolyGetABI },
    { "PolyCopyByteVecToCode",          (polyRTSFunction)&PolyCopyByteVecToCode },
    { "PolyLockMutableCode",            (polyRTSFunction)&PolyLockMutableCode },

    { NULL, NULL} // End of list.
};
