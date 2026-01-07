/*
    Title:      Quick copying garbage collector

    Copyright (c) 2011-12, 2016-17, 2019, 2025 David C. J. Matthews

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
/*
This is a quick copying garbage collector that moves all the data out of
the allocation areas and into the mutable and immutable areas.  If either of
these has filled up it fails and a full garbage collection must be done.
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

#ifdef HAVE_STRING_H 
#include <string.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#include "globals.h"
#include "processes.h"
#include "gc.h"
#include "scanaddrs.h"
#include "check_objects.h"
#include "bitmap.h"
#include "memmgr.h"
#include "diagnostics.h"
#include "heapsizing.h"
#include "gctaskfarm.h"
#include "statistics.h"
#include "gc_progress.h"

// This protects access to the gMem.lSpace table.
static PLock localTableLock("Minor GC tables");

static bool succeeded = true;

class QuickGCScanner: public ScanAddress
{
public:
    QuickGCScanner(bool r): rootScan(r) {}
    virtual ~QuickGCScanner() {}

    // Overrides for ScanAddress class
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt);
    virtual PolyObject *ScanObjectAddress(PolyObject *base);
private:
    PolyObject *FindNewAddress(PolyObject *obj, POLYUNSIGNED L, LocalMemSpace *srcSpace);
    virtual LocalMemSpace *FindSpace(POLYUNSIGNED length, bool isMutable) = 0;
protected:
    bool objectCopied;
    bool rootScan;
};

class RootScanner: public QuickGCScanner
{
public:
    RootScanner(): QuickGCScanner(true), mutableSpace(0), immutableSpace(0) {}
private:
    virtual LocalMemSpace *FindSpace(POLYUNSIGNED length, bool isMutable);
    LocalMemSpace *mutableSpace, *immutableSpace;
};

class ThreadScanner: public QuickGCScanner
{
public:
    ThreadScanner(GCTaskId* id): QuickGCScanner(false), taskID(id), mutableSpace(0), immutableSpace(0),
        spaceTable(0), nOwnedSpaces(0) {}
    virtual ~ThreadScanner() { free(spaceTable); }

    void ScanOwnedAreas(void);
private:
    virtual LocalMemSpace *FindSpace(POLYUNSIGNED length, bool isMutable);
    bool TakeOwnership(LocalMemSpace *space);

    GCTaskId *taskID;
    LocalMemSpace *mutableSpace, *immutableSpace;
    LocalMemSpace **spaceTable;
    unsigned nOwnedSpaces;
};

// This is used when scanning code areas.  If there are no mutable cells left we can clear
// the mutable bit and we don't have to scan it again.
class CodeCheck: public ScanAddress
{
public:
    CodeCheck(): foundMutable(false) {}
    virtual PolyObject *ScanObjectAddress(PolyObject *base) { return base; }
    virtual void ScanAddressesInObject(PolyObject *base, POLYUNSIGNED lengthWord)
        { if (OBJ_IS_MUTABLE_OBJECT(lengthWord)) foundMutable = true;  }
    bool foundMutable;
};

// This uses the conditional exchange instruction to check and update
// the forwarding pointer.  It uses a lock prefix so that if another
// thread has updated it in the meantime it will not set it.
// Using the assembly code provides a very small speed-up so may not
// be worth-while. 
#if defined(_MSC_VER) && (_MSC_VER >= 1600)
// In later versions of MS C we can use the intrinsic.
// 1600 is Visual Studio 2010.  It may well work in older versions
#   include <intrin.h>
#   pragma intrinsic(_InterlockedCompareExchange)
#   if (SIZEOF_VOIDP == 8)
#       define InterlockedCompareExchange64 _InterlockedCompareExchange64
#   else
#       define InterlockedCompareExchange   _InterlockedCompareExchange
#   endif
#endif

#ifdef POLYML32IN64
typedef uint32_t ptrasint;
#else
typedef uintptr_t ptrasint;
#endif


static bool atomiclySetForwarding(LocalMemSpace *space, ptrasint *pt, ptrasint testVal, ptrasint update)
{
#ifdef _MSC_VER
# if (SIZEOF_POLYWORD == 8)
    LONGLONG *address = (LONGLONG*)(pt-1);
    uintptr_t result = InterlockedCompareExchange64(address, update, testVal);
    return result == testVal;
# else
    LONG *address = (LONG*)(pt-1);
    uintptr_t result = InterlockedCompareExchange(address, update, testVal);
    return result == testVal;
# endif
#elif (defined(__GNUC__))
    return __sync_bool_compare_and_swap(pt - 1, testVal, update);
#else
    // Fallback on other targets.
    PLocker lock(&space->spaceLock);
    if (pt[-1] == testVal)
    {
        pt[-1] = update;
        return true;
    }
    return false;
#endif
}

PolyObject *QuickGCScanner::FindNewAddress(PolyObject *obj, POLYUNSIGNED L, LocalMemSpace *srcSpace)
{
    bool isMutable = OBJ_IS_MUTABLE_OBJECT(L);
    POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L);
    LocalMemSpace *lSpace = FindSpace(n, isMutable);
    if (lSpace == 0)
        return 0; // Unable to move it.
    PolyObject *newObject = (PolyObject*)(lSpace->lowerAllocPtr+1);
    // It's possible that another thread may have actually copied the 
    // object since we loaded the length word so we check it again.
    // If this is a mutable we must ensure that checking the forwarding
    // pointer here and updating it if necessary is atomic.  We don't need
    // to do that for immutable data so there is a small chance that an
    // object may be copied twice.  That's not a problem for immutable data.
    // Also lock this if it's code.  This may not be necessary but code objects
    // are rare. Updating the addresses in code objects is complicated and
    // it's possible that there are assumptions somewhere that there's only one
    // copy.
    // Avoiding locking for immutables provides only a small speed-up so may not
    // be worth-while.
    if (isMutable || OBJ_IS_CODE_OBJECT(L))
    {
        if (! atomiclySetForwarding(srcSpace, (ptrasint*)obj, L, OBJ_SET_POINTER(newObject)))
        {
            newObject = obj->GetForwardingPtr();
            if (debugOptions & DEBUG_GC_DETAIL)
                Log("GC: Quick: %p %lu %u has already moved to %p\n", obj, n, GetTypeBits(L), newObject);
            objectCopied = false;
            return newObject;
        }
    }
    else
    {
        if (obj->ContainsForwardingPtr())
        {
            newObject = obj->GetForwardingPtr();
            if (debugOptions & DEBUG_GC_DETAIL)
                Log("GC: Quick: %p %lu %u has already moved to %p\n", obj, n, GetTypeBits(L), newObject);
            objectCopied = false;
            return newObject;
        }
        else obj->SetForwardingPtr(newObject);
    }
    lSpace->lowerAllocPtr += n+1;
#ifdef POLYML32IN64
    // Maintain the correct alignment of lowerAllocPtr
    while (lSpace->lowerAllocPtr < lSpace->upperAllocPtr && ((lSpace->lowerAllocPtr - (PolyWord*)0) & (POLYML32IN64 - 1)) != POLYML32IN64 - 1)
        *lSpace->lowerAllocPtr++ = PolyWord::FromUnsigned(0);
#endif
    CopyObjectToNewAddress(obj, newObject, L);
    objectCopied = true;
    return newObject;
}

// When scanning the roots we want to distribute the data among the immutable and mutable areas
// so that the work is distributed for the scanning threads.
LocalMemSpace *RootScanner::FindSpace(POLYUNSIGNED n, bool isMutable)
{
    LocalMemSpace *lSpace = isMutable ? mutableSpace : immutableSpace;

    if (lSpace != 0)
    {
        // See if there's space in the existing area.
        if (lSpace->freeSpace() > n /* At least n+1*/)
            return lSpace;
    }

    // Find the space with the largest free area.
    for (std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
    {
        LocalMemSpace *sp = *i;
        if (sp->isMutable == isMutable && !sp->allocationSpace &&
                (lSpace == 0 || sp->freeSpace() > lSpace->freeSpace()))
            lSpace = sp;
    }

    if (lSpace != 0 && lSpace->freeSpace() > n)
    {
        if (isMutable) mutableSpace = lSpace; else immutableSpace = lSpace;
        return lSpace;
    }

    return gHeapSizeParameters.AddSpaceInMinorGC(n+1, isMutable);
}

// When scanning within a thread we don't want to be searching the space table.
LocalMemSpace *ThreadScanner::FindSpace(POLYUNSIGNED n, bool isMutable)
{
    LocalMemSpace *lSpace = isMutable ? mutableSpace : immutableSpace;

    if (lSpace != 0)
    {
        // See if there's space in the existing area.
        if (lSpace->freeSpace() > n /* At least n+1*/)
            return lSpace;
    }

    for (unsigned i = 0; i < nOwnedSpaces; i++)
    {
        lSpace = spaceTable[i];
        if (lSpace->isMutable == isMutable &&
            ! lSpace->allocationSpace && lSpace->freeSpace() > n /* At least n+1*/)
        {
            if (n < 10)
            {
                // We use this space for further allocations unless we are trying to
                // allocate a "large" object.
                if (isMutable) mutableSpace = lSpace; else immutableSpace = lSpace;
            }
            return lSpace;
        }
    }

    PLocker l(&localTableLock);
    // Another thread may allocate a new area, reallocating gMem.lSpaces so we
    // we need a lock here.
    if (taskID != 0)
    {
        // See if we can take a space that is currently unused.
        for (std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
        {
            lSpace = *i;
            if (lSpace->spaceOwner == 0 && lSpace->isMutable == isMutable &&
                ! lSpace->allocationSpace && lSpace->freeSpace() > n /* At least n+1*/)
            {
                if (debugOptions & DEBUG_GC_ENHANCED)
                    Log("GC: Quick: Thread %p is taking ownership of space %p\n", taskID, lSpace);
                if (! TakeOwnership(lSpace))
                    return 0;
                return lSpace;
            }
        }
    }

    lSpace = gHeapSizeParameters.AddSpaceInMinorGC(n+1, isMutable);
    if (lSpace != 0 && TakeOwnership(lSpace))
        return lSpace;
    return 0;
}

// Copy all the objects.
POLYUNSIGNED QuickGCScanner::ScanAddressAt(PolyWord *pt)
{
    POLYUNSIGNED n = 1; // Set up the loop to process one word at *pt
    pt++;
    
    while (n-- != 0)
    {
        PolyWord val = *(--pt);
        if (! val.IsTagged())
        {
            LocalMemSpace *space = gMem.LocalSpaceForAddress(val.AsStackAddr()-1);

            // We only copy it if it is in a local allocation space and not in the
            // "overflow" area of data that could not copied by the last full GC.
            if (space != 0 && space->allocationSpace && val.AsAddress() <= space->upperAllocPtr)
            {
                // We shouldn't get code addresses since we handle code
                // segments separately so if this isn't an integer it must be an object address.
                ASSERT(OBJ_IS_DATAPTR(val));

                PolyObject *obj = val.AsObjPtr();
                // Load the length word without any interlock.  We can't assume that
                // another thread won't also copy this at the same time.
                POLYUNSIGNED L = obj->LengthWord();

                // Has it been moved already? N.B.  Another thread may be in the process of
                // moving it so the new object may not be fully copied.
                if (OBJ_IS_POINTER(L))
                    *pt = OBJ_GET_POINTER(L);
                else
                {
                    // We need to copy this object.
                    PolyObject *newObject = FindNewAddress(obj, L, space); // New address of object.

                    if (newObject == 0) { // Couldn't copy it - not enough space.
                        succeeded = false;

                        if (debugOptions & DEBUG_GC_DETAIL)
                            Log("GC: Quick: Insufficient space to move %p %lu %u\n",
                                obj, OBJ_OBJECT_LENGTH(L), GetTypeBits(L));

                        return 0;
                    }

                    *pt = newObject; // Update the pointer to the object
                    // N.B.  If another thread has just copied it "newObject" may actually
                    // be an address in another thread's space.  In that case "objectCopied"
                    // will be false.

                    if (debugOptions & DEBUG_GC_DETAIL)
                        Log("GC: Quick: %p %lu %u moved to %p\n", obj, OBJ_OBJECT_LENGTH(L), GetTypeBits(L), newObject);

                    // Stop now unless this is a simple word object we have been able to move.
                    // Also stop if we're just scanning the roots.
                    if (! rootScan && newObject != obj && ! OBJ_IS_MUTABLE_OBJECT(L) && 
                        GetTypeBits(L) == 0 && objectCopied)
                    {
                        // We can simply return zero in which case this performs a breadth-first scan.
                        // A breadth-first scan distributes the objects through the memory so
                        // to retain some degree of locality we try to copy some object pointed at
                        // by this one.  We work from the end back so that we follow the tail pointers
                        // for lists.
                        n = OBJ_OBJECT_LENGTH(L); // Object length
                        pt = (PolyWord*)newObject + n;
                    }
                }
            }
        }
    }
    // We've reached the end without finding a pointer to follow
    return 0;
}

// The initial entry to process the roots.  Also used when processing the addresses
// in objects that can't be handled by ScanAddressAt.
PolyObject *QuickGCScanner::ScanObjectAddress(PolyObject *base)
{
#ifdef POLYML32IN64
    // If this is a code address we can't turn it into a PolyWord.
    // Check that it's a local address.
    MemSpace *space = gMem.SpaceForObjectAddress(base);
    ASSERT(space != 0);
    if (space->spaceType != ST_LOCAL)
        return base;
#endif
    PolyWord val = base;
    // Scan this as an address.
    (void)QuickGCScanner::ScanAddressAt(&val);
    // Ignore the result of ScanAddressAt which is always zero and
    // just return the updated address.
    return val.AsObjPtr();
}

// Add this to the set of spaces we own.  Must be called with the
// localTableLock held.
bool ThreadScanner::TakeOwnership(LocalMemSpace *space)
{
    ASSERT(space->spaceOwner == 0);
    LocalMemSpace **v = (LocalMemSpace**)realloc(spaceTable, (nOwnedSpaces+1)*sizeof(LocalMemSpace*));
    if (v == 0)
        return false;
    spaceTable = v;
    space->spaceOwner = taskID;
    spaceTable[nOwnedSpaces++] = space;
    return true;
}

// Thread function to scan an area.  It scans the addresses in the region
// copying any objects from the allocation area into mutable or immutable
// areas it owns.  It then processes all the areas it owns until there
// are no further addresses to scan.
static void scanArea(GCTaskId *id, void *arg1, void *arg2)
{
    ThreadScanner marker(id);
    marker.ScanAddressesInRegion((PolyWord*)arg1, (PolyWord*)arg2);
    marker.ScanOwnedAreas();
}

void ThreadScanner::ScanOwnedAreas()
{
    while (true)
    {
        bool allDone = true;
        // We're finished when there is no unscanned data in any space we own.
        for (unsigned k = 0; k < nOwnedSpaces && allDone; k++)
        {
            LocalMemSpace *space = spaceTable[k];
            allDone = space->partialGCScan == space->lowerAllocPtr;
        }
        if (allDone)
            break;

        // Scan each area that has had data added to it.
        for (unsigned l = 0; l < nOwnedSpaces; l++)
        {
            LocalMemSpace *space = spaceTable[l];
            // Scan the area.  This may well result in more data being added
            while (space->partialGCScan < space->lowerAllocPtr)
            {
                // Is the queue draining?  If so it's probably worth creating
                // some spare work.
                if (gpTaskFarm->Draining() && gpTaskFarm->ThreadCount() > 1)
                {
                    PolyWord *mid =
                        space->partialGCScan + (space->lowerAllocPtr - space->partialGCScan)/2;
                    // Split the space in two.
                    PolyWord *p = space->partialGCScan;
                    while (p < mid)
                    {
#ifdef POLYML32IN64
                        if (((p-(PolyWord*)0) & (POLYML32IN64-1)) != POLYML32IN64 - 1)
                        {
                            p++; // Should be on the correct boundary 
                            continue;
                        }
#endif
                        PolyObject *o = (PolyObject*)(p+1);
                        ASSERT(o->ContainsNormalLengthWord());
                        p += o->Length()+1;
                    }
                    // Start a new task to scan the area up to the half-way point.
                    // Because we round up to the end of the next object we may
                    // include the whole area but that's probably better because
                    // we may have other areas to scan.
                    if (gpTaskFarm->AddWork(scanArea, space->partialGCScan, p))
                    {
                        space->partialGCScan = p;
                        if (space->lowerAllocPtr == space->partialGCScan)
                            break;
                    }
                }

#ifdef POLYML32IN64
                // Align so that the cell starts on the correct boundary
                if (((space->partialGCScan - (PolyWord*)0) & (POLYML32IN64 - 1)) != POLYML32IN64 - 1)
                {
                    space->partialGCScan++;
                    continue;
                }
#endif
                PolyObject* obj = (PolyObject*)(space->partialGCScan + 1);
                ASSERT(obj->ContainsNormalLengthWord());
                POLYUNSIGNED length = obj->Length();
                ASSERT(space->partialGCScan+length+1 <= space->lowerAllocPtr);
                space->partialGCScan += length+1;
                if (length != 0)
                    ScanAddressesInObject(obj);
                // If any thread has run out of space we should stop.
                if (! succeeded)
                    return;
            }
        }
    }
    // Release the spaces we're holding in case another thread wants to use them.
    // Take the table lock before releasing them.  This is necessary on the ARM
    // to act as a write barrier so that writes to the spaces and the space
    // structure itself are seen by another thread that takes ownership.
    PLocker l(&localTableLock);
    for (unsigned m = 0; m < nOwnedSpaces; m++)
    {
        LocalMemSpace *space = spaceTable[m];
        space->spaceOwner = 0;
    }
    nOwnedSpaces = 0;
}

bool RunQuickGC(const POLYUNSIGNED wordsRequiredToAllocate)
{
    // If the last minor GC took too long force a full GC.
    if (gHeapSizeParameters.RunMajorGCImmediately())
        return false;

    gHeapSizeParameters.RecordGCTime(HeapSizeParameters::GCTimeStart);
    globalStats.incCount(PSC_GC_PARTIALGC);
    mainThreadPhase = MTP_GCQUICK;
    succeeded = true;
    gcProgressBeginMinorGC();

    if (debugOptions & DEBUG_GC)
        Log("GC: Beginning quick GC\n");

    if (debugOptions & DEBUG_HEAPSIZE)
        gMem.ReportHeapSizes("Minor GC (before)");

    uintptr_t spaceBeforeGC = 0;

    for(std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
    {
        LocalMemSpace *lSpace = *i;
        ASSERT (lSpace->top >= lSpace->upperAllocPtr);
        ASSERT (lSpace->upperAllocPtr >= lSpace->lowerAllocPtr);
        ASSERT (lSpace->lowerAllocPtr >= lSpace->bottom);
        // Remember the top before we started this GC.  It's
        // only relevant for mutable areas.  It avoids us rescanning
        // objects that may have been added to the space as a result of
        // scanning another space.
        if (lSpace->isMutable)
            lSpace->partialGCTop = lSpace->upperAllocPtr;
        else lSpace->partialGCTop = lSpace->top;
        // If we're scanning a space this is where we start.
        // For immutable areas this only includes newly added
        // data but for mutable areas we have to scan data added
        // by previous partial GCs.
        if (lSpace->isMutable && ! lSpace->allocationSpace)
            lSpace->partialGCRootBase = lSpace->bottom;
        else lSpace->partialGCRootBase = lSpace->lowerAllocPtr;
        lSpace->spaceOwner = 0; // Not currently owned
        // Add up the space in the mutable and immutable areas
        if (! lSpace->allocationSpace)
            spaceBeforeGC += lSpace->allocatedSpace();
    }

    // First scan the roots, copying the data into the mutable and immutable areas.
    RootScanner rootScan;
    // Scan the permanent mutable areas.  This could be parallelised but it doesn't
    // appear to be worthwhile at the moment.
    for (std::vector<PermanentMemSpace*>::iterator i = gMem.pSpaces.begin(); i < gMem.pSpaces.end(); i++)
    {
        PermanentMemSpace *space = *i;
        if (space->isMutable && ! space->byteOnly)
            rootScan.ScanAddressesInRegion(space->bottom, space->top);
    }
    // Scan code spaces.  
    for (std::vector<CodeSpace *>::iterator i = gMem.cSpaces.begin(); i < gMem.cSpaces.end(); i++)
    {
        CodeSpace *space = *i;
        // Spaces are mutable if any object has been added to the area since the last GC.
        if (space->isMutable)
        {
            rootScan.ScanAddressesInRegion(space->bottom, space->top);
            // Check to see if any of the objects are still mutable.  If they are
            // we are still building the code and must rescan it on the next GC.
            // If there aren't we don't need to unless another code object is added.
            CodeCheck codeCheck;
            codeCheck.ScanAddressesInRegion(space->bottom, space->top);
            space->isMutable = codeCheck.foundMutable;
        }
    }

    // Scan RTS addresses.  This will include the thread stacks.
    GCModules(&rootScan);

    // At this point the immutable and mutable areas will have some root objects
    // in the space between partialGCRootBase (the old value of lowerAllocPtr) and
    // lowerAllocPtr.  These will contain the addresses of objects in the allocation
    // areas.  We need to scan these root objects and then any new objects we copy
    // until there are no objects left to scan.
    // We also need to scan local mutable areas since these are roots as well.
    // They have data between partialGCTop and top.  Parallelising this appears
    // to be a significant gain.
    // We have to be careful about the pointers here.  AddWorkOrRunNow begins
    // a thread immediately and so the scanning threads may be running while
    // we are still creating new tasks.  To avoid tripping up we use separate
    // pointers to the root objects rather than using lowerAllocPtr and
    // partialGCScan because these can be modified by the scanning tasks.
    // It's also possible for new spaces to be added to the table by the scanning
    // tasks while we are still adding tasks.  It is important that the values of
    // partialGCRootBase, partialGCRootTop and partialGCTop are properly initialised
    // for these new spaces.
    for (std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
    {
        LocalMemSpace *space = *i;
        space->partialGCRootTop = space->lowerAllocPtr; // Top of the roots
        space->partialGCScan = space->lowerAllocPtr; // Start of scanning for new data.
    }

    // Now start creating tasks.  From this point only a thread that owns a space
    // may read or modify lowerAllocPtr or partialGCScan.
    {
        unsigned l = 0;
        while (true)
        {
            LocalMemSpace *space;
            {
                // There is a chance that a thread that has already been forked may
                // allocate a new space and realloc gMem.lSpaces.  We have to drop
                // the lock before calling AddWorkOrRunNow in case we "run now".
                PLocker lock(&localTableLock);
                if (l >= gMem.lSpaces.size())
                    break;
                space = gMem.lSpaces[l++];
            }
            if (space->partialGCRootBase != space->partialGCRootTop)
                gpTaskFarm->AddWorkOrRunNow(scanArea, space->partialGCRootBase, space->partialGCRootTop);
            if (space->partialGCTop != space->top)
                gpTaskFarm->AddWorkOrRunNow(scanArea, space->partialGCTop, space->top);
        }
    }

    gpTaskFarm->WaitForCompletion();

    uintptr_t spaceAfterGC = 0;

    if (succeeded)
    {
        globalStats.setSize(PSS_AFTER_LAST_GC, 0);
        globalStats.setSize(PSS_ALLOCATION, 0);
        globalStats.setSize(PSS_ALLOCATION_FREE, 0);
        // If it succeeded the allocation areas are now empty.
        for(std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
        {
            LocalMemSpace *lSpace = *i;
            uintptr_t free;
            if (lSpace->allocationSpace)
            {
                lSpace->lowerAllocPtr = lSpace->bottom;
#ifdef POLYML32IN64
                for (unsigned i = 0; i < POLYML32IN64-1; i++)
                    *(lSpace->lowerAllocPtr++) = PolyWord::FromUnsigned(0);
#endif
                free = lSpace->freeSpace();
#ifdef FILL_UNUSED_MEMORY
                // This provides extra checking if we have dangling pointers
                memset(lSpace->bottom, 0xaa, (char*)lSpace->upperAllocPtr - (char*)lSpace->bottom);
#endif
                globalStats.incSize(PSS_ALLOCATION, free*sizeof(PolyWord));
                globalStats.incSize(PSS_ALLOCATION_FREE, free*sizeof(PolyWord));
            }
            else free = lSpace->freeSpace();

            if (debugOptions & DEBUG_GC_ENHANCED)
                Log("GC: %s space %p %" PRI_SIZET " free in %" PRI_SIZET " words %2.1f%% full\n", lSpace->spaceTypeString(),
                    lSpace, lSpace->freeSpace(), lSpace->spaceSize(),
                    ((float)lSpace->allocatedSpace()) * 100 / (float)lSpace->spaceSize());
            globalStats.incSize(PSS_AFTER_LAST_GC, free*sizeof(PolyWord));
            spaceAfterGC += lSpace->allocatedSpace();
        }

        if (! gMem.CheckForAllocation(wordsRequiredToAllocate))
            succeeded = false;
    }

    if (succeeded)
    {
        gHeapSizeParameters.RecordGCTime(HeapSizeParameters::GCTimeEnd);

        if (! gHeapSizeParameters.AdjustSizeAfterMinorGC(spaceAfterGC, spaceBeforeGC)) // Adjust the allocation size.
            return false; // If necessary trigger a full GC immediately
        gHeapSizeParameters.resetMinorTimingData();
        // Remove allocation spaces that are larger than the default
        // and any excess over the current size of the allocation area.
        gMem.RemoveExcessAllocation();

        if (debugOptions & DEBUG_HEAPSIZE)
            gMem.ReportHeapSizes("Minor GC (after)");

        if (debugOptions & DEBUG_GC)
            Log("GC: Completed successfully\n");

        CheckMemory();
    }
    else
    {
        // There was insufficient room to copy everything.  We will need to
        // run a full GC.
        gHeapSizeParameters.RecordGCTime(HeapSizeParameters::GCTimeEnd);
        if (debugOptions & DEBUG_GC)
            Log("GC: Quick GC failed\n");
    }

    return succeeded;
}
