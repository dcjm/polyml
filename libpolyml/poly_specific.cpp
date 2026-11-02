/*
    Title:  poly_specific.cpp - Poly/ML specific RTS calls.

    Copyright (c) 2006, 2015-17, 2019, 2021, 2026 David C. J. Matthews

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
#include "version.h"
#include "memmgr.h"
#include "processes.h"
#include "gc.h"
#include "rtsentry.h"
#include "scanaddrs.h" // For SetConstantValue

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySpecificGeneral(POLYUNSIGNED threadId, POLYUNSIGNED code, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetABI();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyLockMutableClosure(POLYUNSIGNED threadId, POLYUNSIGNED closure);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyCopyByteVecToClosure(POLYUNSIGNED threadId, POLYUNSIGNED byteVec, POLYUNSIGNED closure);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySetCodeConstant(POLYUNSIGNED closure, POLYUNSIGNED offset, POLYUNSIGNED c, POLYUNSIGNED flags);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetCodeConstant(POLYUNSIGNED closure, POLYUNSIGNED offset, POLYUNSIGNED flags);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySetCodeByte(POLYUNSIGNED closure, POLYUNSIGNED offset, POLYUNSIGNED c);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetCodeByte(POLYUNSIGNED closure, POLYUNSIGNED offset);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySortArrayOfAddresses(POLYUNSIGNED array);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetHeapBase(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetC32UnitSize();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTest4(POLYUNSIGNED threadId, POLYUNSIGNED arg1, POLYUNSIGNED arg2, POLYUNSIGNED arg3, POLYUNSIGNED arg4);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTest5(POLYUNSIGNED threadId, POLYUNSIGNED arg1, POLYUNSIGNED arg2, POLYUNSIGNED arg3, POLYUNSIGNED arg4, POLYUNSIGNED arg5);
}

#define SAVE(x) taskData->saveVec.push(x)

#ifndef GIT_VERSION
#define GIT_VERSION             ""
#endif


Handle poly_dispatch_c(TaskData *taskData, Handle args, Handle code)
{
    unsigned c = get_C_unsigned(taskData, DEREFWORD(code));
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
            case MA_X86_64_32:      version = "X86_64_32-" TextVersion; break;
            case MA_Arm64:          version = "Arm64-" TextVersion; break;
            case MA_Arm64_32:       version = "Arm64_32-" TextVersion; break;
            default:                version = "Unknown-" TextVersion; break;
            }
            return SAVE(C_string_to_Poly(taskData, version));
        }

    case 12: // Return the architecture
        // Used in InitialPolyML.ML for PolyML.architecture
        {
            const char *arch;
            switch (machineDependent->MachineArchitecture())
            {
            case MA_Interpreted:    arch = "Interpreted"; break;
            case MA_I386:           arch = "I386"; break;
            case MA_X86_64:         arch = "X86_64"; break;
            case MA_X86_64_32:      arch = "X86_64_32"; break;
            case MA_Arm64:          arch = "Arm64"; break;
            case MA_Arm64_32:       arch = "Arm64_32"; break;
            default:                arch = "Unknown"; break;
            }
            return SAVE(C_string_to_Poly(taskData, arch));
        }

    case 19: // Return the RTS argument help string.
        return SAVE(C_string_to_Poly(taskData, RTSArgHelp()));

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
POLYUNSIGNED PolySpecificGeneral(POLYUNSIGNED threadId, POLYUNSIGNED code, POLYUNSIGNED arg)
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
#if (defined(_WIN32) || defined(__CYGWIN__))
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

// Copy the byte vector into code space.
POLYUNSIGNED PolyCopyByteVecToClosure(POLYUNSIGNED threadId, POLYUNSIGNED byteVec, POLYUNSIGNED closure)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedByteVec = taskData->saveVec.push(byteVec);
    Handle pushedClosure = taskData->saveVec.push(closure);
    PolyObject *result = 0;

#ifdef HAVE_PTHREAD_JIT_WRITE_PROTECT_NP
    pthread_jit_write_protect_np(false);
#endif

    try {
        if (!pushedByteVec->WordP()->IsByteObject())
            raise_fail(taskData, "Not byte data area");
        if (pushedClosure->WordP()->Length() != sizeof(PolyObject*)/sizeof(PolyWord))
            raise_fail(taskData, "Invalid closure size");
        if (!pushedClosure->WordP()->IsMutable())
            raise_fail(taskData, "Closure is not mutable");
        do {
            PolyObject *initCell = pushedByteVec->WordP();
            POLYUNSIGNED requiredSize = initCell->Length();
            result = gMem.AllocCodeSpace(requiredSize);
            if (result == 0)
            {
                // Could not allocate - must GC.
                if (!QuickGC(taskData, pushedByteVec->WordP()->Length()))
                    raise_fail(taskData, "Insufficient memory");
            }
            else memcpy(gMem.SpaceForObjectAddress(result)->writeAble((byte*)result), initCell, requiredSize * sizeof(PolyWord));
        } while (result == 0);
    }
    catch (...) {} // If an ML exception is raised

    // Store the code address in the closure.
    *((PolyObject**)pushedClosure->WordP()) = result;
    // Lock the closure.
    pushedClosure->WordP()->SetLengthWord(pushedClosure->WordP()->LengthWord() & ~_OBJ_MUTABLE_BIT);

#ifdef HAVE_PTHREAD_JIT_WRITE_PROTECT_NP
    pthread_jit_write_protect_np(true);
#endif

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

// Code generation - Lock a mutable code segment and return the original address.
// Currently this does not allocate so other than the exception it could
// be a fast call.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyLockMutableClosure(POLYUNSIGNED threadId, POLYUNSIGNED closure)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    PolyObject *codeObj = *(PolyObject**)(PolyWord::FromUnsigned(closure).AsObjPtr());

#ifdef HAVE_PTHREAD_JIT_WRITE_PROTECT_NP
    pthread_jit_write_protect_np(false);
#endif

    try {
        if (!codeObj->IsCodeObject() || !codeObj->IsMutable())
            raise_fail(taskData, "Not mutable code area");
        POLYUNSIGNED segLength = codeObj->Length();
        gMem.SpaceForObjectAddress(codeObj)->writeAble(codeObj)->SetLengthWord(segLength, F_CODE_OBJ);
        // Flush cache on ARM at least.
        machineDependent->FlushInstructionCache(codeObj, segLength * sizeof(PolyWord));
        // In the future it may be necessary to return a different address here.
        // N.B.  The code area should only have execute permission in the native
        // code version, not the interpreted version.
    }
    catch (...) {} // If an ML exception is raised

#ifdef HAVE_PTHREAD_JIT_WRITE_PROTECT_NP
    pthread_jit_write_protect_np(true);
#endif

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

// Set code constant.  This can be a fast call.
// This is in the RTS both because we pass a closure in here and cannot have
// code addresses in 32-in-64 and also because we need to ensure there is no
// possibility of a GC while the code is an inconsistent state.
POLYUNSIGNED PolySetCodeConstant(POLYUNSIGNED closure, POLYUNSIGNED offset, POLYUNSIGNED cWord, POLYUNSIGNED flags)
{
    byte *startCode;
#ifdef HAVE_PTHREAD_JIT_WRITE_PROTECT_NP
    pthread_jit_write_protect_np(false);
#endif

    // Previously we passed the code address in here and we need to
    // retain that for legacy code.  This is now the closure.
    if (PolyWord::FromUnsigned(closure).AsObjPtr()->IsCodeObject())
        startCode = PolyWord::FromUnsigned(closure).AsCodePtr();
    else startCode = *(POLYCODEPTR*)(PolyWord::FromUnsigned(closure).AsObjPtr());
    // startCode is the start of the code segment.
    // c will usually be an address.
    // offset is a byte offset
    byte* instrAddr = startCode + PolyWord::FromUnsigned(offset).UnTaggedUnsigned();
    byte* writeable = gMem.SpaceForAddress(instrAddr)->writeAble(instrAddr);
    switch (UNTAGGED(PolyWord::FromUnsigned(flags)))
    {
        case 0: // Absolute constant - size PolyWord
        {
            POLYUNSIGNED c = PolyWord::FromUnsigned(cWord).AsUnsigned();
#ifdef WORDS_BIGENDIAN
            // This is used to store constants in the constant area
            // on the interpreted version. 
            for (unsigned i = sizeof(PolyWord); i > 0; i--)
            {
                writeable[i-1] = (byte)(c & 255);
                c >>= 8;
            }
#else
            for (unsigned i = 0; i < sizeof(PolyWord); i++)
            {
                writeable[i] = (byte)(c & 255);
                c >>= 8;
            }
#endif
            break;
        }
        case 1: // Relative constant - X86 - size 4 bytes
        {
            // The offset is relative to the END of the constant.
            byte *target;
            // In 32-in-64 we pass in the closure address here
            // rather than the code address.
            if (PolyWord::FromUnsigned(cWord).AsObjPtr()->IsCodeObject())
                target = PolyWord::FromUnsigned(cWord).AsCodePtr();
            else target = *(POLYCODEPTR*)(PolyWord::FromUnsigned(cWord).AsObjPtr());
            size_t c = target - instrAddr - 4;
            for (unsigned i = 0; i < 4; i++)
            {
                writeable[i] = (byte)(c & 255);
                c >>= 8;
            }
            break;
        }
        case 2: // Absolute constant - size uintptr_t
            // This is the same as case 0 except in 32-in-64 when
            // it is an absolute address rather than an object pointer.
        {
            uintptr_t c = (uintptr_t)(PolyWord::FromUnsigned(cWord).AsObjPtr());
            for (unsigned i = 0; i < sizeof(uintptr_t); i++)
            {
                writeable[i] = (byte)(c & 255);
                c >>= 8;
            }
            break;
        }
        case 3: // ARM64 ADRP + LDR64
            // These don't actually put a constant into the code.  Instead
            // they set the instruction pair to an offset in the current code
            // segment.
        {
            uintptr_t c = (uintptr_t)startCode + PolyWord::FromUnsigned(cWord).UnTaggedUnsigned();
            ScanAddress::SetConstantValue(instrAddr, (PolyObject*)c, PROCESS_RELOC_ARM64ADRPLDR64);
            break;
        }
        case 4: // ARM64 ADRP + LDR32
        {
            uintptr_t c = (uintptr_t)startCode + PolyWord::FromUnsigned(cWord).UnTaggedUnsigned();
            ScanAddress::SetConstantValue(instrAddr, (PolyObject*)c, PROCESS_RELOC_ARM64ADRPLDR32);
            break;
        }
        case 5: // ARM64 ADRP + ADD
        {
            uintptr_t c = (uintptr_t)startCode + PolyWord::FromUnsigned(cWord).UnTaggedUnsigned();
            ScanAddress::SetConstantValue(instrAddr, (PolyObject*)c, PROCESS_RELOC_ARM64ADRPADD);
            break;
        }
    }

#ifdef HAVE_PTHREAD_JIT_WRITE_PROTECT_NP
    pthread_jit_write_protect_np(true);
#endif

    return TAGGED(0).AsUnsigned();
}

// Get a code constant.  This is only used for debugging.
POLYUNSIGNED PolyGetCodeConstant(POLYUNSIGNED closure, POLYUNSIGNED offset, POLYUNSIGNED flags)
{
    byte* pointer = *(POLYCODEPTR*)(PolyWord::FromUnsigned(closure).AsObjPtr());
    // offset is a byte offset
    pointer += PolyWord::FromUnsigned(offset).UnTaggedUnsigned();
    switch (UNTAGGED(PolyWord::FromUnsigned(flags)))
    {
    case 0: // Absolute constant - size PolyWord
    {
        POLYUNSIGNED c = 0;
#ifdef WORDS_BIGENDIAN
        for (unsigned i = 0; i < sizeof(PolyWord); i++)
            c = (c << 8) | pointer[i];
#else
        for (unsigned i = sizeof(PolyWord); i > 0; i--)
            c = (c << 8) | pointer[i-1];
#endif
        return c;
    }
    }
    // For the moment just handle that case.
    return TAGGED(0).AsUnsigned();
}

// Set a code byte.  This needs to be in the RTS because it uses the closure
POLYEXTERNALSYMBOL POLYUNSIGNED PolySetCodeByte(POLYUNSIGNED closure, POLYUNSIGNED offset, POLYUNSIGNED cWord)
{
    byte *pointer = *(POLYCODEPTR*)(PolyWord::FromUnsigned(closure).AsObjPtr());
    byte* writable = gMem.SpaceForAddress(pointer)->writeAble(pointer);
    writable[UNTAGGED_UNSIGNED(PolyWord::FromUnsigned(offset))] = (byte)UNTAGGED_UNSIGNED(PolyWord::FromUnsigned(cWord));
    return TAGGED(0).AsUnsigned();
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetCodeByte(POLYUNSIGNED closure, POLYUNSIGNED offset)
{
    byte *pointer = *(POLYCODEPTR*)(PolyWord::FromUnsigned(closure).AsObjPtr());
    return TAGGED(pointer[UNTAGGED_UNSIGNED(PolyWord::FromUnsigned(offset))]).AsUnsigned();
}

static int compare(const void *a, const void *b)
{
    PolyWord *av = (PolyWord*)a;
    PolyWord *bv = (PolyWord*)b;
    if ((*av).IsTagged() || (*bv).IsTagged()) return 0; // Shouldn't happen
    PolyObject *ao = (*av).AsObjPtr(), *bo = (*bv).AsObjPtr();
    if (ao->Length() < 1 || bo->Length() < 1) return 0; // Shouldn't happen
    if (ao->Get(0).AsUnsigned() < bo->Get(0).AsUnsigned())
        return -1;
    if (ao->Get(0).AsUnsigned() > bo->Get(0).AsUnsigned())
        return 1;
    return 0;
}

// Sort an array of addresses.  This is used in the code-generator to search for
// duplicates in the address area.  The argument is an array of pairs.  The first
// item of each pair is an address, the second is an identifier of some kind.
POLYEXTERNALSYMBOL POLYUNSIGNED PolySortArrayOfAddresses(POLYUNSIGNED array)
{
    if (!PolyWord::FromUnsigned(array).IsDataPtr()) return(TAGGED(0)).AsUnsigned();
    PolyObject *arrayP = PolyWord::FromUnsigned(array).AsObjPtr();
    POLYUNSIGNED numberOfItems = arrayP->Length();
    if (!arrayP->IsMutable()) return(TAGGED(0)).AsUnsigned();
    qsort(arrayP, numberOfItems, sizeof(PolyWord), compare);
    return (TAGGED(1)).AsUnsigned();
}

// Return the value of globalHeapBase as a SysWord value.
// This is used in just one place: when compiling an FFI callback stub in ARM 32-in-64.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetHeapBase(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle result = 0;

    try {
#ifdef POLYML32IN64
        result = Make_sysword(taskData, (uintptr_t)globalHeapBase);
#else
        result = Make_sysword(taskData, 0);
#endif
    }
    catch (...) {} // If an ML exception is raised

    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyTest4(POLYUNSIGNED threadId, POLYUNSIGNED arg1, POLYUNSIGNED arg2, POLYUNSIGNED arg3, POLYUNSIGNED arg4)
{
    switch (PolyWord::FromUnsigned(arg1).UnTaggedUnsigned())
    {
    case 1: return arg1;
    case 2: return arg2;
    case 3: return arg3;
    case 4: return arg4;
    default: return TAGGED(0).AsUnsigned();
    }
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyTest5(POLYUNSIGNED threadId, POLYUNSIGNED arg1, POLYUNSIGNED arg2, POLYUNSIGNED arg3, POLYUNSIGNED arg4, POLYUNSIGNED arg5)
{
    switch (PolyWord::FromUnsigned(arg1).UnTaggedUnsigned())
    {
    case 1: return arg1;
    case 2: return arg2;
    case 3: return arg3;
    case 4: return arg4;
    case 5: return arg5;
    default: return TAGGED(0).AsUnsigned();
    }

}

// Return the unit size for Compact 32 bit.  This is needed by the code-generator.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetC32UnitSize()
{
#ifdef POLYML32IN64
    return TAGGED(POLYML32IN64).AsUnsigned();
#else
    return TAGGED(0).AsUnsigned(); // Return zero
#endif
}


struct _entrypts polySpecificEPT[] =
{
    { "PolySpecificGeneral",            (polyRTSFunction)&PolySpecificGeneral},
    { "PolyGetABI",                     (polyRTSFunction)&PolyGetABI },
    { "PolyCopyByteVecToClosure",       (polyRTSFunction)&PolyCopyByteVecToClosure },
    { "PolyLockMutableClosure",         (polyRTSFunction)&PolyLockMutableClosure },
    { "PolySetCodeConstant",            (polyRTSFunction)&PolySetCodeConstant },
    { "PolyGetCodeConstant",            (polyRTSFunction)&PolyGetCodeConstant },
    { "PolySetCodeByte",                (polyRTSFunction)&PolySetCodeByte },
    { "PolyGetCodeByte",                (polyRTSFunction)&PolyGetCodeByte },
    { "PolySortArrayOfAddresses",       (polyRTSFunction)&PolySortArrayOfAddresses },
    { "PolyGetHeapBase",                (polyRTSFunction)&PolyGetHeapBase },
    { "PolyTest4",                      (polyRTSFunction)&PolyTest4 },
    { "PolyTest5",                      (polyRTSFunction)&PolyTest5 },
    { "PolyGetC32UnitSize",             (polyRTSFunction)&PolyGetC32UnitSize },

    { NULL, NULL} // End of list.
};
