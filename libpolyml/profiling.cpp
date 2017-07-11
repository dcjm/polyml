/*
    Title:      Profiling
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Copyright (c) 2000-7
        Cambridge University Technical Services Limited
    Further development copyright (c) David C.J. Matthews 2011, 2015

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

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#include "globals.h"
#include "arb.h"
#include "processes.h"
#include "polystring.h"
#include "profiling.h"
#include "save_vec.h"
#include "rts_module.h"
#include "memmgr.h"
#include "scanaddrs.h"
#include "locking.h"
#include "run_time.h"
#include "sys.h"
#include "rtsentry.h"

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyProfiling(PolyObject *threadId, PolyWord mode);
}

static POLYUNSIGNED mainThreadCounts[MTP_MAXENTRY];
static const char* const mainThreadText[MTP_MAXENTRY] =
{
    "UNKNOWN",
    "GARBAGE COLLECTION (sharing phase)",
    "GARBAGE COLLECTION (mark phase)",
    "GARBAGE COLLECTION (copy phase)",
    "GARBAGE COLLECTION (update phase)",
    "GARBAGE COLLECTION (minor collection)",
    "Common data sharing",
    "Exporting",
    "Saving state",
    "Loading saved state",
    "Profiling",
    "Setting signal handler",
    "Cygwin spawn",
    "Storing module",
    "Loading module"
};

// Entries for store profiling
enum _extraStore {
    EST_CODE = 0,
    EST_STRING,
    EST_BYTE,
    EST_WORD,
    EST_MUTABLE,
    EST_MUTABLEBYTE,
    EST_MAX_ENTRY
};

static POLYUNSIGNED extraStoreCounts[EST_MAX_ENTRY];
static const char * const extraStoreText[EST_MAX_ENTRY] =
{
    "Function code",
    "Strings",
    "Byte data (long precision ints etc)",
    "Unidentified word data",
    "Unidentified mutable data",
    "Mutable byte data (profiling counts)"
};

// Poly strings for "standard" counts.  These are generated from the C strings
// above the first time profiling is activated.
static PolyWord psRTSString[MTP_MAXENTRY], psExtraStrings[EST_MAX_ENTRY], psGCTotal;

ProfileMode profileMode;
// If we are just profiling a single thread, this is the thread data.
static TaskData *singleThreadProfile = 0;

typedef struct _PROFENTRY
{
    POLYUNSIGNED count;
    PolyWord functionName;
    struct _PROFENTRY *nextEntry;
} PROFENTRY, *PPROFENTRY;

class ProfileRequest: public MainThreadRequest
{
public:
    ProfileRequest(unsigned prof, TaskData *pTask):
        MainThreadRequest(MTP_PROFILING), mode(prof), pCallingThread(pTask), pTab(0), errorMessage(0) {}
    ~ProfileRequest();
    virtual void Perform();
    Handle extractAsList(TaskData *taskData);

private:
    void getResults(void);
    void getProfileResults(PolyWord *bottom, PolyWord *top);
    PPROFENTRY newProfileEntry(void);

private:
    unsigned mode;
    TaskData *pCallingThread;
    PPROFENTRY pTab;

public:
    const char *errorMessage;
};

ProfileRequest::~ProfileRequest()
{
    PPROFENTRY p = pTab;
    while (p != 0)
    {
        PPROFENTRY toFree = p;
        p = p->nextEntry;
        free(toFree); 
    }
}

// Lock to serialise updates of counts. Only used during update.
// Not required when we print the counts since there's only one thread
// running then.
static PLock countLock;

// Get the profile object associated with a piece of code.  Returns null if
// there isn't one, in particular if this is in the old format.
static PolyObject *getProfileObjectForCode(PolyObject *code)
{
    ASSERT(code->IsCodeObject());
    PolyWord *consts;
    POLYUNSIGNED constCount;
    code->GetConstSegmentForCode(consts, constCount);
    if (constCount < 3 || ! consts[2].IsDataPtr()) return 0;
    PolyObject *profObject = consts[2].AsObjPtr();
    if (profObject->IsMutable() && profObject->IsByteObject() && profObject->Length() == 1)
        return profObject;
    else return 0;
}

// Adds incr to the profile count for the function pointed at by
// pc or by one of its callers.
// This is called from a signal handler in the case of time profiling.
void add_count(TaskData *taskData, POLYCODEPTR fpc, PolyWord *sp, POLYUNSIGNED incr)
{

    /* The first PC may be valid even if it's not a code pointer */
    bool is_code = true;
    PolyWord pc = PolyWord::FromCodePtr(fpc);
    PolyWord *endStack = taskData->stack->top;
    
    /* First try the pc value we have been given - if that fails search down
       the stack to see if there is a return address we can use. */
    for (;;)
    {
        /* Get the address of the code segment from the code pointer */
        if (pc.IsCodePtr() || is_code)
        {   
            is_code = false;
            
            // Check that the pc value is within the heap.  It could be
            // in the assembly code.
            MemSpace *space = gMem.SpaceForAddress(pc.AsAddress());
            if (space != 0)
            {
                PolyObject *profObject = getProfileObjectForCode(ObjCodePtrToPtr(pc.AsCodePtr()));
                PLocker locker(&countLock);
                if (profObject)
                    profObject->Set(0, PolyWord::FromUnsigned(profObject->Get(0).AsUnsigned() + incr));
                return;
            }
            /* else just fall through and try next candidate address */
        } /* OBJ_IS_CODEPTR(pc) */
        
        /* Find next candidate address */
        if (sp < endStack)
            pc = *sp++;
        else /* Reached bottom of stack without finding valid code pointer */
        {
            PLocker locker(&countLock);
            mainThreadCounts[MTP_USER_CODE] += incr;
            return;
        }
    } /* loop "forever" */
    /*NOTREACHED*/
}


// newProfileEntry - Make a new entry in the list
PPROFENTRY ProfileRequest::newProfileEntry(void)
{
    PPROFENTRY newEntry = (PPROFENTRY)malloc(sizeof(PROFENTRY));
    if (newEntry == 0) { errorMessage = "Insufficient memory"; return 0; }
    newEntry->nextEntry = pTab;
    pTab = newEntry;
    return newEntry;
}

// We don't use ScanAddress here because we're only interested in the
// objects themselves not the addresses in them.
// We have to build the list of results in C memory rather than directly in
// ML memory because we can't allocate in ML memory in the root thread.
void ProfileRequest::getProfileResults(PolyWord *bottom, PolyWord *top)
{
    PolyWord *ptr = bottom;

    while (ptr < top)
    {
        ptr++; // Skip the length word
        PolyObject *obj = (PolyObject*)ptr;
        if (obj->ContainsForwardingPtr())
        {
            // It's a forwarding pointer - get the length from the new location
            while (obj->ContainsForwardingPtr())
                obj = obj->GetForwardingPtr();
            ASSERT(obj->ContainsNormalLengthWord());
            ptr += obj->Length();
        }
        else
        {
            ASSERT(obj->ContainsNormalLengthWord());

            if (obj->IsCodeObject())
            {
                PolyWord *firstConstant = obj->ConstPtrForCode();
                PolyWord name = firstConstant[0];
                PolyObject *profCount = getProfileObjectForCode(obj);
                if (profCount)
                {
                    POLYUNSIGNED count = profCount->Get(0).AsUnsigned();
                
                    if (count != 0)
                    {
                        if (name != TAGGED(0))
                        {
                            PPROFENTRY pEnt = newProfileEntry();
                            if (pEnt == 0) return;
                            pEnt->count = count;
                            pEnt->functionName = name;
                        }
                    
                        profCount->Set(0, PolyWord::FromUnsigned(0));
                    }
                }
            } /* code object */
            ptr += obj->Length();
        } /* else */
    } /* while */
}

void ProfileRequest::getResults(void)
// Print profiling information and reset profile counts.
{
    for (unsigned j = 0; j < gMem.npSpaces; j++)
    {
        MemSpace *space = gMem.pSpaces[j];
        // Permanent areas are filled with objects from the bottom.
        getProfileResults(space->bottom, space->top); // Bottom to top
    }
    for (unsigned j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *space = gMem.lSpaces[j];
        // Local areas only have objects from the allocation pointer to the top.
        getProfileResults(space->bottom, space->lowerAllocPtr);
        getProfileResults(space->upperAllocPtr, space->top);
    }

    {
        POLYUNSIGNED gc_count =
            mainThreadCounts[MTP_GCPHASESHARING]+
            mainThreadCounts[MTP_GCPHASEMARK]+
            mainThreadCounts[MTP_GCPHASECOMPACT] +
            mainThreadCounts[MTP_GCPHASEUPDATE] +
            mainThreadCounts[MTP_GCQUICK];
        if (gc_count)
        {
            PPROFENTRY pEnt = newProfileEntry();
            if (pEnt == 0) return; // Report insufficient memory?
            pEnt->count = gc_count;
            pEnt->functionName = psGCTotal;
        }
    }

    for (unsigned k = 0; k < MTP_MAXENTRY; k++)
    {
        if (mainThreadCounts[k])
        {
            PPROFENTRY pEnt = newProfileEntry();
            if (pEnt == 0) return; // Report insufficient memory?
            pEnt->count = mainThreadCounts[k];
            pEnt->functionName = psRTSString[k];
            mainThreadCounts[k] = 0;
        }
    }

    for (unsigned l = 0; l < EST_MAX_ENTRY; l++)
    {
        if (extraStoreCounts[l])
        {
            PPROFENTRY pEnt = newProfileEntry();
            if (pEnt == 0) return; // Report insufficient memory?
            pEnt->count = extraStoreCounts[l];
            pEnt->functionName = psExtraStrings[l];
            extraStoreCounts[l] = 0;
        }
    }
}

// Extract the accumulated results as an ML list of pairs of the count and the string.
Handle ProfileRequest::extractAsList(TaskData *taskData)
{
    Handle saved = taskData->saveVec.mark();
    Handle list = taskData->saveVec.push(ListNull);

    for (PPROFENTRY p = pTab; p != 0; p = p->nextEntry)
    {
        Handle pair = alloc_and_save(taskData, 2);
        Handle countValue = Make_arbitrary_precision(taskData, p->count);
        pair->WordP()->Set(0, countValue->Word());
        pair->WordP()->Set(1, p->functionName);
        Handle next  = alloc_and_save(taskData, sizeof(ML_Cons_Cell) / sizeof(PolyWord));
        DEREFLISTHANDLE(next)->h = pair->Word();
        DEREFLISTHANDLE(next)->t = DEREFLISTHANDLE(list);

        taskData->saveVec.reset(saved);
        list = taskData->saveVec.push(next->Word());
    }

    return list;
}

void handleProfileTrap(TaskData *taskData, SIGNALCONTEXT *context)
{
    if (singleThreadProfile != 0 && singleThreadProfile != taskData)
        return;

    /* If we are in the garbage-collector add the count to "gc_count"
        otherwise try to find out where we are. */
    if (mainThreadPhase == MTP_USER_CODE)
    {
        if (taskData)
        {
            PolyWord *sp;
            POLYCODEPTR pc;
            if (taskData->GetPCandSPFromContext(context, sp, pc))
                add_count(taskData, pc, sp, 1);
            else mainThreadCounts[MTP_USER_CODE]++;
        }
        else mainThreadCounts[MTP_USER_CODE]++;
        // On Mac OS X all virtual timer interrupts seem to be directed to the root thread
        // so all the counts will be "unknown".
    }
    else mainThreadCounts[mainThreadPhase]++;
}

// Called from the GC when allocation profiling is on.
void AddObjectProfile(PolyObject *obj)
{
    ASSERT(obj->ContainsNormalLengthWord());
    POLYUNSIGNED length = obj->Length();

    if (obj->IsWordObject() && OBJ_HAS_PROFILE(obj->LengthWord()))
    {
        // It has a profile pointer.  The last word should point to the
        // closure or code of the allocating function.  Add the size of this to the count.
        ASSERT(length != 0);
        PolyWord profWord = obj->Get(length-1);
        ASSERT(profWord.IsDataPtr());
        PolyObject *profObject = profWord.AsObjPtr();
        ASSERT(profObject->IsMutable() && profObject->IsByteObject() && profObject->Length() == 1);
        profObject->Set(0, PolyWord::FromUnsigned(profObject->Get(0).AsUnsigned() + length + 1));
    }
    // If it doesn't have a profile pointer add it to the appropriate count.
    else if (obj->IsMutable())
    {
        if (obj->IsByteObject())
            extraStoreCounts[EST_MUTABLEBYTE] += length+1;
        else extraStoreCounts[EST_MUTABLE] += length+1;
    }
    else if (obj->IsCodeObject())
        extraStoreCounts[EST_CODE] += length+1;
    else if (obj->IsByteObject())
    {
        // Try to separate strings from other byte data.  This is only
        // approximate.
        if (OBJ_IS_NEGATIVE(obj->LengthWord()))
            extraStoreCounts[EST_BYTE] += length+1;
        else
        {
            PolyStringObject *possString = (PolyStringObject*)obj;
            POLYUNSIGNED bytes = length * sizeof(PolyWord);
            // If the length of the string as given in the first word is sufficient
            // to fit in the exact number of words then it's probably a string.
            if (length >= 2 &&
                possString->length <= bytes - sizeof(POLYUNSIGNED) &&
                possString->length > bytes - 2 * sizeof(POLYUNSIGNED))
                    extraStoreCounts[EST_STRING] += length+1;
            else
            {
                extraStoreCounts[EST_BYTE] += length+1;
            }
        }
    }
    else extraStoreCounts[EST_WORD] += length+1;
}

// Called from ML to control profiling.
Handle profilerc(TaskData *taskData, Handle mode_handle)
/* Profiler - generates statistical profiles of the code.
   The parameter is an integer which determines the value to be profiled.
   When profiler is called it always resets the profiling and prints out any
   values which have been accumulated.
   If the parameter is 0 this is all it does, 
   if the parameter is 1 then it produces time profiling,
   if the parameter is 2 it produces store profiling.
   3 - arbitrary precision emulation traps. */
{
    unsigned mode = get_C_unsigned(taskData, DEREFWORDHANDLE(mode_handle));
    {
        // Create any strings we need.  We only need to do this once but
        // it must be done by a non-root thread since it needs a taskData object.
        // Don't bother locking.  At worst we'll create some garbage.
        for (unsigned k = 0; k < MTP_MAXENTRY; k++)
        {
            if (psRTSString[k] == TAGGED(0))
                psRTSString[k] = C_string_to_Poly(taskData, mainThreadText[k]);
        }
        for (unsigned k = 0; k < EST_MAX_ENTRY; k++)
        {
            if (psExtraStrings[k] == TAGGED(0))
                psExtraStrings[k] = C_string_to_Poly(taskData, extraStoreText[k]);
        }
        if (psGCTotal == TAGGED(0))
            psGCTotal = C_string_to_Poly(taskData, "GARBAGE COLLECTION (total)");
    }
    // All these actions are performed by the root thread.  Only profile
    // printing needs to be performed with all the threads stopped but it's
    // simpler to serialise all requests.
    ProfileRequest request(mode, taskData);
    processes->MakeRootRequest(taskData, &request);
    if (request.errorMessage != 0) raise_exception_string(taskData, EXC_Fail, request.errorMessage);
    return request.extractAsList(taskData);
}

POLYUNSIGNED PolyProfiling(PolyObject *threadId, PolyWord mode)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedMode = taskData->saveVec.push(mode);
    Handle result = 0;

    try {
        result = profilerc(taskData, pushedMode);
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// This is called from the root thread when all the ML threads have been paused.
void ProfileRequest::Perform()
{
    if (mode != kProfileOff && profileMode != kProfileOff)
    {
        // Profiling must be stopped first.
        errorMessage = "Profiling is currently active";
        return;
    }

    singleThreadProfile = 0; // Unless kProfileTimeThread is given this should be 0

    switch (mode)
    {
    case kProfileOff:
        // Turn off old profiling mechanism and print out accumulated results 
        profileMode = kProfileOff;
        processes->StopProfiling();
        getResults();
        break;

    case kProfileTimeThread:
        singleThreadProfile = pCallingThread;
        // And drop through to kProfileTime
      
    case kProfileTime:
        profileMode = kProfileTime;
        processes->StartProfiling();
        break;

    case kProfileStoreAllocation:
        profileMode = kProfileStoreAllocation;
        break;
        
    case kProfileEmulation:
        profileMode = kProfileEmulation;
        break;

    case kProfileLiveData:
        profileMode = kProfileLiveData;
        break;
 
    case kProfileLiveMutables:
        profileMode = kProfileLiveMutables;
        break;
       
    default: /* do nothing */
        break;
    }
    
} /* profilerc */

struct _entrypts profilingEPT[] =
{
    // Profiling
    { "PolyProfiling",                  (polyRTSFunction)&PolyProfiling},

    { NULL, NULL} // End of list.
};


class Profiling: public RtsModule
{
public:
    virtual void Init(void);
    virtual void GarbageCollect(ScanAddress *process);
};

// Declare this.  It will be automatically added to the table.
static Profiling profileModule;

void Profiling::Init(void)
{
    // Reset profiling counts.
    profileMode = kProfileOff;
    for (unsigned k = 0; k < MTP_MAXENTRY; k++) mainThreadCounts[k] = 0;
}

void Profiling::GarbageCollect(ScanAddress *process)
{
    // Process any strings in the table.
    for (unsigned k = 0; k < MTP_MAXENTRY; k++)
        process->ScanRuntimeWord(&psRTSString[k]);
    for (unsigned k = 0; k < EST_MAX_ENTRY; k++)
        process->ScanRuntimeWord(&psExtraStrings[k]);
    process->ScanRuntimeWord(&psGCTotal);
}
