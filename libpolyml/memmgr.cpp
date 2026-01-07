/*
    Title:  memmgr.cpp   Memory segment manager

    Copyright (c) 2006-7, 2011-12, 2016-18, 2025 David C. J. Matthews

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

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#include <stdio.h>

#include <new>

#include "globals.h"
#include "memmgr.h"
#include "osmem.h"
#include "scanaddrs.h"
#include "bitmap.h"
#include "mpoly.h"
#include "diagnostics.h"
#include "statistics.h"
#include "processes.h"
#include "machine_dep.h"


#ifdef POLYML32IN64
// This contains the address of the base of the heap.
PolyWord *globalHeapBase, *globalCodeBase;
#endif

// heap resizing policy option requested on command line
unsigned heapsizingOption = 0;

// If we are building for the interpreted version we don't need or want the
// code to be executable.
static const enum OSMem::_MemUsage executableCodeWhereNecessary =
#ifdef CODEISNOTEXECUTABLE
    OSMem::UsageData;
#else
    OSMem::UsageExecutableCode;
#endif

MemSpace::MemSpace(OSMem *alloc): SpaceTree(true)
{
    spaceType = ST_PERMANENT;
    isMutable = false;
    bottom = 0;
    top = 0;
    isCode = false;
    allocator = alloc;
    shadowSpace = 0;
}

MemSpace::~MemSpace()
{
    if (allocator != 0 && bottom != 0)
    {
        if (isCode)
            allocator->FreeCodeArea(bottom, shadowSpace, (char*)top - (char*)bottom);
        else allocator->FreeDataArea(bottom, (char*)top - (char*)bottom);
    }
}

MarkableSpace::MarkableSpace(OSMem *alloc): MemSpace(alloc), spaceLock("Local space")
{
}

LocalMemSpace::LocalMemSpace(OSMem *alloc): MarkableSpace(alloc)
{
    spaceType = ST_LOCAL;
    upperAllocPtr = lowerAllocPtr = 0;
    for (unsigned i = 0; i < NSTARTS; i++)
        start[i] = 0;
    start_index = 0;
    i_marked = m_marked = updated = 0;
    allocationSpace = false;
}

bool LocalMemSpace::InitSpace(PolyWord *heapSpace, uintptr_t size, bool mut)
{
    isMutable = mut;
    bottom = heapSpace;
    top = bottom + size;
    // Initialise all the fields.  The partial GC in particular relies on this.
    upperAllocPtr = partialGCTop = fullGCRescanStart = fullGCLowerLimit = lowestWeak = top;
    lowerAllocPtr = partialGCScan = partialGCRootBase = partialGCRootTop =
        fullGCRescanEnd = highestWeak = bottom;
#ifdef POLYML32IN64
    // The address must be on an odd-word boundary so that after the length
    // word is put in the actual cell address is on the correct boundary.
    for (unsigned i = 0; i < POLYML32IN64 - 1; i++)
        *lowerAllocPtr++ = PolyWord::FromUnsigned(0);
#endif
    spaceOwner = 0;

    allocationSpace = false;

    // Bitmap for the space.
    return bitmap.Create(size);
}

MemMgr::MemMgr(): allocLock("Memmgr alloc"), codeBitmapLock("Code bitmap")
{
    nextIndex = 0;
    reservedSpace = 0;
    nextAllocator = 0;
    defaultSpaceSize = 0;
    spaceBeforeMinorGC = 0;
    spaceForHeap = 0;
    currentAllocSpace = currentHeapSize = 0;
    defaultSpaceSize = 1024 * 1024 / sizeof(PolyWord); // 1Mbyte segments.
    spaceTree = new SpaceTreeTree;
}

MemMgr::~MemMgr()
{
    delete(spaceTree); // Have to do this before we delete the spaces.
    for (std::vector<PermanentMemSpace *>::iterator i = pSpaces.begin(); i < pSpaces.end(); i++)
        delete(*i);
    for (std::vector<LocalMemSpace*>::iterator i = lSpaces.begin(); i < lSpaces.end(); i++)
        delete(*i);
    for (std::vector<PermanentMemSpace *>::iterator i = eSpaces.begin(); i < eSpaces.end(); i++)
        delete(*i);
    for (std::vector<StackSpace *>::iterator i = sSpaces.begin(); i < sSpaces.end(); i++)
        delete(*i);
    for (std::vector<CodeSpace *>::iterator i = cSpaces.begin(); i < cSpaces.end(); i++)
        delete(*i);
}

bool MemMgr::Initialise()
{
#ifdef POLYML32IN64
    // Reserve a single 16/32G area but with no access.
    void *heapBase;
    if (!osHeapAlloc.Initialise(OSMem::UsageData, (size_t)8 * POLYML32IN64 * 1024 * 1024 * 1024, &heapBase))
        return false;
    globalHeapBase = (PolyWord*)heapBase;
    // Allocate a 4 gbyte area for the stacks.
    // It's important that the stack and code areas have addresses with
    // non-zero top 32-bits.
    if (!osStackAlloc.Initialise(OSMem::UsageStack, (size_t)4 * 1024 * 1024 * 1024, 0))
        return false;
#else
    if (!osHeapAlloc.Initialise(OSMem::UsageData) || !osStackAlloc.Initialise(OSMem::UsageStack))
        return false;
#endif
#if (defined(POLYML32IN64) || defined(HOSTARCHITECTURE_X86_64) || defined(HOSTARCHITECTURE_AARCH64))
    // Reserve a 2G area for the code.  N.B. If we need to use a shadow area this size may be reduced.
    void* codeBase;
    if (!osCodeAlloc.Initialise(executableCodeWhereNecessary,
        (size_t)2 * 1024 * 1024 * 1024, &codeBase))
        return false;
#ifdef POLYML32IN64
    globalCodeBase = (PolyWord*)codeBase;
#endif
    return true;
#else
    return osCodeAlloc.Initialise(executableCodeWhereNecessary);
#endif
}

// Create and initialise a new local space and add it to the table.
LocalMemSpace* MemMgr::NewLocalSpace(uintptr_t size, bool mut)
{
    try {
        LocalMemSpace *space = new LocalMemSpace(&osHeapAlloc);
        // Before trying to allocate the heap temporarily allocate the
        // reserved space.  This ensures that this much space will always
        // be available for C stacks and the C++ heap.
        void *reservation = 0;
        size_t rSpace = reservedSpace*sizeof(PolyWord);

        if (reservedSpace != 0) {
            reservation = osHeapAlloc.AllocateDataArea(rSpace);
            if (reservation == NULL) {
                // Insufficient space for the reservation.  Can't allocate this local space.
                if (debugOptions & DEBUG_MEMMGR)
                    Log("MMGR: New local %smutable space: insufficient reservation space\n", mut ? "": "im");
                delete space;
                return 0;
            }
        }

        // Allocate the heap itself.
        size_t iSpace = size * sizeof(PolyWord);
        PolyWord* heapSpace = (PolyWord*)osHeapAlloc.AllocateDataArea(iSpace);
        // The size may have been rounded up to a block boundary.
        size = iSpace / sizeof(PolyWord);
        bool success = heapSpace != 0 && space->InitSpace(heapSpace, size, mut) && AddLocalSpace(space);

        if (reservation != 0) osHeapAlloc.FreeDataArea(reservation, rSpace);
        if (success)
        {
            if (debugOptions & DEBUG_MEMMGR)
                Log("MMGR: New local %smutable space %p, size=%luk words, bottom=%p, top=%p\n", mut ? "": "im",
                    space, space->spaceSize()/1024, space->bottom, space->top);
            currentHeapSize += space->spaceSize();
            globalStats.setSize(PSS_TOTAL_HEAP, currentHeapSize * sizeof(PolyWord));
            return space;
        }

        // If something went wrong.
        delete space;
        if (debugOptions & DEBUG_MEMMGR)
            Log("MMGR: New local %smutable space: insufficient space\n", mut ? "": "im");
        return 0;
    }
    catch (std::bad_alloc&) {
        if (debugOptions & DEBUG_MEMMGR)
            Log("MMGR: New local %smutable space: \"new\" failed\n", mut ? "": "im");
        return 0;
    }
}

// Create a local space for initial allocation.
LocalMemSpace *MemMgr::CreateAllocationSpace(uintptr_t size)
{
    LocalMemSpace *result = NewLocalSpace(size, true);
    if (result) 
    {
        result->allocationSpace = true;
        currentAllocSpace += result->spaceSize();
        globalStats.incSize(PSS_ALLOCATION, result->spaceSize()*sizeof(PolyWord));
        globalStats.incSize(PSS_ALLOCATION_FREE, result->freeSpace()*sizeof(PolyWord));
    }
    return result;
}

// If an allocation space has a lot of data left in it after a GC, particularly 
// a single large object we should turn it into a local area.
void MemMgr::ConvertAllocationSpaceToLocal(LocalMemSpace *space)
{
    ASSERT(space->allocationSpace);
    space->allocationSpace = false;
    // Currently it is left as a mutable area but if the contents are all
    // immutable e.g. a large vector it could be better to turn it into an
    // immutable area.
    currentAllocSpace -= space->spaceSize();
}

// Add a local memory space to the table.
bool MemMgr::AddLocalSpace(LocalMemSpace *space)
{
    // Add to the table.
    // Update the B-tree.
    try {
        AddTree(space);
        // The entries in the local table are ordered so that the copy phase of the full
        // GC simply has to copy to an entry earlier in the table.  Immutable spaces come
        // first, followed by mutable spaces and finally allocation spaces.
        if (space->allocationSpace)
            lSpaces.push_back(space); // Just add at the end
        else if (space->isMutable)
        {
            // Add before the allocation spaces
            std::vector<LocalMemSpace*>::iterator i = lSpaces.begin();
            while (i != lSpaces.end() && ! (*i)->allocationSpace) i++;
            lSpaces.insert(i, space);
        }
        else
        {
            // Immutable space: Add before the mutable spaces
            std::vector<LocalMemSpace*>::iterator i = lSpaces.begin();
            while (i != lSpaces.end() && ! (*i)->isMutable) i++;
            lSpaces.insert(i, space);
        }
    }
    catch (std::bad_alloc&) {
        RemoveTree(space);
        return false;
    }
    return true;
}

// Create an entry for a permanent space that already exists in the executable.
PermanentMemSpace* MemMgr::PermanentSpaceFromExecutable(PolyWord *base, uintptr_t words,
                                             unsigned flags, unsigned index, ModuleId sourceModule)
{
    try {
        PermanentMemSpace *space = new PermanentMemSpace(0/* Not freed */);
        space->bottom = base;
        space->topPointer = space->top = space->bottom + words;
        space->spaceType = ST_PERMANENT;
        space->isMutable = flags & MTF_WRITEABLE ? true : false;
        space->noOverwrite = flags & MTF_NO_OVERWRITE ? true : false;
        space->byteOnly = flags & MTF_BYTES ? true : false;
        space->isCode = flags & MTF_EXECUTABLE ? true : false;
        space->index = index;
        space->moduleIdentifier = sourceModule;
        space->isWriteProtected = !space->isMutable;
        if (index >= nextIndex) nextIndex = index+1;

        // Extend the permanent memory table and add this space to it.
        try {
            AddTree(space);
            pSpaces.push_back(space);
        }
        catch (std::exception&) {
            RemoveTree(space);
            delete space;
            return 0;
        }
        return space;
    }
    catch (std::bad_alloc&) {
        return 0;
    }
}

PermanentMemSpace *MemMgr::AllocateNewPermanentSpace(uintptr_t byteSize, unsigned flags, unsigned index, ModuleId sourceModule)
{
    try {
        OSMem *alloc = flags & MTF_EXECUTABLE ? (OSMem*)&osCodeAlloc : (OSMem*)&osHeapAlloc;
        PermanentMemSpace *space = new PermanentMemSpace(alloc);
        size_t actualSize = byteSize;
        PolyWord* base;
        void* newShadow=0;
        if (flags & MTF_EXECUTABLE)
            base = (PolyWord*)alloc->AllocateCodeArea(actualSize, newShadow);
        else base = (PolyWord*)alloc->AllocateDataArea(actualSize);
        if (base == 0)
        {
            delete(space);
            return 0;
        }
        space->bottom = base;
        space->shadowSpace = (PolyWord*)newShadow;
        space->topPointer = space->top = space->bottom + actualSize/sizeof(PolyWord);
        space->spaceType = ST_PERMANENT;
        space->isMutable = flags & MTF_WRITEABLE ? true : false;
        space->noOverwrite = flags & MTF_NO_OVERWRITE ? true : false;
        space->byteOnly = flags & MTF_BYTES ? true : false;
        space->isCode = flags & MTF_EXECUTABLE ? true : false;
        space->index = index;
        space->moduleIdentifier = sourceModule;
        if (index >= nextIndex) nextIndex = index + 1;

        // Extend the permanent memory table and add this space to it.
        try {
            AddTree(space);
            pSpaces.push_back(space);
        }
        catch (std::exception&) {
            RemoveTree(space);
            delete space;
            return 0;
        }
        return space;
    }
    catch (std::bad_alloc&) {
        return 0;
    }
}

// Called to set the permissions on permanent spaces from the executable.
bool MemMgr::CompletePermanentSpaceAllocation(PermanentMemSpace *space)
{
    // Remove write access unless it is mutable.
    if (!space->isMutable)
    {
        if (space->isCode)
            osCodeAlloc.DisableWriteForCode(space->bottom, space->shadowSpace, (char*)space->top - (char*)space->bottom);
        else osHeapAlloc.EnableWrite(false, space->bottom, (char*)space->top - (char*)space->bottom);
        space->isWriteProtected = true;
    }
    return true;
}

// Delete a local space and remove it from the table.
void MemMgr::DeleteLocalSpace(std::vector<LocalMemSpace*>::iterator &iter)
{
    LocalMemSpace *sp = *iter;
    if (debugOptions & DEBUG_MEMMGR)
        Log("MMGR: Deleted local %s space %p at %p size %zu\n", sp->spaceTypeString(), sp, sp->bottom, sp->spaceSize());
    currentHeapSize -= sp->spaceSize();
    globalStats.setSize(PSS_TOTAL_HEAP, currentHeapSize * sizeof(PolyWord));
    if (sp->allocationSpace) currentAllocSpace -= sp->spaceSize();
    RemoveTree(sp);
    delete(sp);
    iter = lSpaces.erase(iter);
}

// Remove local areas that are now empty after a GC.
// It isn't clear if we always want to do this.
void MemMgr::RemoveEmptyLocals()
{
    for (std::vector<LocalMemSpace*>::iterator i = lSpaces.begin(); i < lSpaces.end(); )
    {
        LocalMemSpace *space = *i;
        if (space->isEmpty())
            DeleteLocalSpace(i);
        else i++;
    }
}

// Create and initialise a new export space and add it to the table.
PermanentMemSpace* MemMgr::NewExportSpace(uintptr_t size, bool mut, bool noOv, bool code)
{
    try {
        OSMem *alloc = code ? (OSMem*)&osCodeAlloc : (OSMem*)&osHeapAlloc;
        PermanentMemSpace *space = new PermanentMemSpace(alloc);
        space->spaceType = ST_EXPORT;
        space->isMutable = mut;
        space->noOverwrite = noOv;
        space->isCode = code;
        space->index = nextIndex++;
        // Allocate the memory itself.
        size_t iSpace = size*sizeof(PolyWord);
        if (code)
        {
            void* shadow;
            space->bottom = (PolyWord*)alloc->AllocateCodeArea(iSpace, shadow);
            if (space->bottom != 0)
                space->shadowSpace = (PolyWord*)shadow;
        }
        else space->bottom = (PolyWord*)alloc->AllocateDataArea(iSpace);

        if (space->bottom == 0)
        {
            delete space;
            if (debugOptions & DEBUG_MEMMGR)
                Log("MMGR: New export %smutable space: insufficient space\n", mut ? "" : "im");
            return 0;
        }

        // The size may have been rounded up to a block boundary.
        size = iSpace/sizeof(PolyWord);
        space->top = space->bottom + size;
        space->topPointer = space->bottom;
#ifdef POLYML32IN64
        // The address must be on an odd-word boundary so that after the length
        // word is put in the actual cell address is on the correct boundary.
        for (unsigned i = 0; i < POLYML32IN64 - 1; i++)
            *(space->writeAble(space->topPointer++)) = PolyWord::FromUnsigned(0);
#endif
        if (debugOptions & DEBUG_MEMMGR)
            Log("MMGR: New export %smutable %s%sspace %p, size=%luk words, bottom=%p, top=%p\n", mut ? "" : "im",
                noOv ? "no-overwrite " : "", code ? "code " : "", space,
                space->spaceSize() / 1024, space->bottom, space->top);

        // Add to the table.
        try {
            AddTree(space);
            eSpaces.push_back(space);
        }
        catch (std::exception&) {
            RemoveTree(space);
            delete space;
            if (debugOptions & DEBUG_MEMMGR)
                Log("MMGR: New export %smutable space: Adding to tree failed\n", mut ? "" : "im");
            return 0;
        }
        return space;
    }
    catch (std::bad_alloc&) {
        if (debugOptions & DEBUG_MEMMGR)
            Log("MMGR: New export %smutable space: \"new\" failed\n", mut ? "" : "im");
        return 0;
    }
}

void MemMgr::DeleteExportSpaces(void)
{
    for (std::vector<PermanentMemSpace *>::iterator i = eSpaces.begin(); i < eSpaces.end(); i++)
    {
        PermanentMemSpace *space = *i;
        RemoveTree(space);
        delete(space);
    }
    eSpaces.clear();
}

// If we have saved the state rather than exported a function we turn the exported
// spaces into permanent ones, removing existing permanent spaces at the same or
// lower level.
bool MemMgr::DemoteOldPermanentSpaces(ModuleId modId)
{
    // Save permanent spaces at a lower hierarchy.  Others are converted into
    // local spaces.  Most or all items will have been copied from these spaces
    // into an export space but there could be items reachable only from the stack.
    std::vector<PermanentMemSpace*>::iterator i = pSpaces.begin();
    while (i != pSpaces.end())
    {
        PermanentMemSpace* pSpace = *i;
        if (pSpace->moduleIdentifier != modId)
            i++;
        else
        {
            try {
                // Turn this into a local space or a code space
                // Remove this from the tree - AddLocalSpace will make an entry for the local version.
                RemoveTree(pSpace);

                if (pSpace->isCode)
                {
                    // Enable write access.  Permanent spaces are read-only.
 //                   osCodeAlloc.SetPermissions(pSpace->bottom, (char*)pSpace->top - (char*)pSpace->bottom,
 //                       PERMISSION_READ | PERMISSION_WRITE | PERMISSION_EXEC);
                    CodeSpace* space = new CodeSpace(pSpace->bottom, pSpace->shadowSpace, pSpace->spaceSize(), &osCodeAlloc);
                    if (!space->headerMap.Create(space->spaceSize()))
                    {
                        if (debugOptions & DEBUG_MEMMGR)
                            Log("MMGR: Unable to create header map for state space %p\n", pSpace);
                        return false;
                    }
                    if (!AddCodeSpace(space))
                    {
                        if (debugOptions & DEBUG_MEMMGR)
                            Log("MMGR: Unable to convert saved state space %p into code space\n", pSpace);
                        return false;
                    }
                    if (debugOptions & DEBUG_MEMMGR)
                        Log("MMGR: Converted saved state space %p into code space %p\n", pSpace, space);
                    // Set the bits in the header map.
                    for (PolyWord* ptr = space->bottom; ptr < space->top; )
                    {
                        PolyObject* obj = (PolyObject*)(ptr + 1);
                        // We may have forwarded this if this has been
                        // copied to the exported area. Restore the original length word.
                        if (obj->ContainsForwardingPtr())
                        {
#ifdef POLYML32IN64
                            PolyObject* forwardedTo = obj;
                            // This is relative to globalCodeBase not globalHeapBase
                            while (forwardedTo->ContainsForwardingPtr())
                                forwardedTo = (PolyObject*)(globalCodeBase + (((uintptr_t)(forwardedTo->LengthWord()) & ~_OBJ_TOMBSTONE_BIT) * POLYML32IN64));
#else
                            PolyObject* forwardedTo = obj->FollowForwardingChain();
#endif
                            obj->SetLengthWord(forwardedTo->LengthWord());
                        }
                        // Set the "start" bit if this is allocated.  It will be a byte seg if not.
                        if (obj->IsCodeObject())
                            space->headerMap.SetBit(ptr - space->bottom);
                        ASSERT(!obj->IsClosureObject());
                        ptr += obj->Length() + 1;
                    }
                }
                else
                {
                    // Enable write access.  Permanent spaces are read-only.
//                    osHeapAlloc.SetPermissions(pSpace->bottom, (char*)pSpace->top - (char*)pSpace->bottom,
//                        PERMISSION_READ | PERMISSION_WRITE);
                    LocalMemSpace* space = new LocalMemSpace(&osHeapAlloc);
                    space->top = pSpace->top;
                    // Space is allocated in local areas from the top down.  This area is full and
                    // all data is in the old generation.  The area can be recovered by a full GC.
                    space->bottom = space->upperAllocPtr = space->lowerAllocPtr =
                        space->fullGCLowerLimit = pSpace->bottom;
                    space->isMutable = pSpace->isMutable;
                    space->isCode = false;
                    if (!space->bitmap.Create(space->top - space->bottom) || !AddLocalSpace(space))
                    {
                        if (debugOptions & DEBUG_MEMMGR)
                            Log("MMGR: Unable to convert saved state space %p into local space\n", pSpace);
                        return false;
                    }

                    if (debugOptions & DEBUG_MEMMGR)
                        Log("MMGR: Converted saved state space %p into local %smutable space %p\n",
                            pSpace, pSpace->isMutable ? "im" : "", space);
                    currentHeapSize += space->spaceSize();
                    globalStats.setSize(PSS_TOTAL_HEAP, currentHeapSize * sizeof(PolyWord));
                }
                i = pSpaces.erase(i);
            }
            catch (std::bad_alloc&) {
                return false;
            }
        }
    }
    return true;
}

// Turn export spaces into permanent spaces.  Used for saved states and also for modules.
bool MemMgr::PromoteNewExportSpaces(ModuleId newModId)
{
    // Save newly exported spaces.
    for(std::vector<PermanentMemSpace *>::iterator j = eSpaces.begin(); j < eSpaces.end(); j++)
    {
        PermanentMemSpace *space = *j;
        space->spaceType = ST_PERMANENT;
        space->moduleIdentifier = newModId;
        // Put a dummy object to fill up the unused space.
        if (space->topPointer != space->top)
            FillUnusedSpace(space->writeAble(space->topPointer), space->top - space->topPointer);
        // Put in a dummy object to fill the rest of the space.
        pSpaces.push_back(space);
    }
    eSpaces.clear();

    return true;
}

// Return the space for a given index
PermanentMemSpace *MemMgr::SpaceForIndex(unsigned index, ModuleId modId)
{
    for (std::vector<PermanentMemSpace*>::iterator i = pSpaces.begin(); i < pSpaces.end(); i++)
    {
        PermanentMemSpace *space = *i;
        if (space->index == index && space->moduleIdentifier == modId)
            return space;
    }
    return NULL;
}

// In several places we assume that segments are filled with valid
// objects.  This fills unused memory with one or more "byte" objects.
void MemMgr::FillUnusedSpace(PolyWord *base, uintptr_t words)
{
    PolyWord *pDummy = base+1;
    while (words > 0)
    {
#ifdef POLYML32IN64
        // Make sure that any dummy object we insert is properly aligned. 
        if (((pDummy - (PolyWord*)0) & (POLYML32IN64 - 1)) != 0)
        {
            pDummy[-1] = PolyWord::FromUnsigned(0); // Put it in the length word
            pDummy++;
            words--;
            continue;
        }
#endif
        POLYUNSIGNED oSize;
        // If the space is larger than the maximum object size
        // we will need several objects.
        if (words > MAX_OBJECT_SIZE) oSize = MAX_OBJECT_SIZE;
        else oSize = (POLYUNSIGNED)(words-1);
        // Make this a byte object so it's always skipped.
        ((PolyObject*)pDummy)->SetLengthWord(oSize, F_BYTE_OBJ);
        words -= oSize+1;
        pDummy += oSize+1;
    }
}

// Allocate an area of the heap of at least minWords and at most maxWords.
// This is used both when allocating single objects (when minWords and maxWords
// are the same) and when allocating heap segments.  If there is insufficient
// space to satisfy the minimum it will return 0.
PolyWord *MemMgr::AllocHeapSpace(uintptr_t minWords, uintptr_t &maxWords, bool doAllocation)
{
    PLocker locker(&allocLock);
    // We try to distribute the allocations between the memory spaces
    // so that at the next GC we don't have all the most recent cells in
    // one space.  The most recent cells will be more likely to survive a
    // GC so distibuting them improves the load balance for a multi-thread GC.
    nextAllocator++;
    if (nextAllocator > gMem.lSpaces.size()) nextAllocator = 0;

    unsigned j = nextAllocator;
    for (std::vector<LocalMemSpace*>::iterator i = lSpaces.begin(); i < lSpaces.end(); i++)
    {
        if (j >= gMem.lSpaces.size()) j = 0;
        LocalMemSpace *space = gMem.lSpaces[j++];
        if (space->allocationSpace)
        {
            uintptr_t available = space->freeSpace();
            if (available > 0 && available >= minWords)
            {
                // Reduce the maximum value if we had less than that.
                if (available < maxWords) maxWords = available;
#ifdef POLYML32IN64
                // If necessary round down to an even boundary
                while (maxWords & (POLYML32IN64-1))
                {
                    maxWords--;
                    space->lowerAllocPtr[maxWords] = PolyWord::FromUnsigned(0);
                }
#endif
                PolyWord *result = space->lowerAllocPtr; // Return the address.
                if (doAllocation)
                    space->lowerAllocPtr += maxWords; // Allocate it.
                return result;
            }
        }
    }
    // There isn't space in the existing areas - can we create a new area?
    // The reason we don't have enough space could simply be that we want to
    // allocate an object larger than the default space size.  Try deleting
    // some other spaces to bring currentAllocSpace below spaceBeforeMinorGC - minWords.
    if (minWords > defaultSpaceSize && minWords < spaceBeforeMinorGC)
        RemoveExcessAllocation(spaceBeforeMinorGC - minWords);

    if (currentAllocSpace/* + minWords */ < spaceBeforeMinorGC)
    {
        // i.e. the current allocation space is less than the space allowed for the minor GC
        // but it may be that allocating this object will take us over the limit.  We allow
        // that to happen so that we can successfully allocate very large objects even if
        // we have a new GC very shortly.
        uintptr_t spaceSize = defaultSpaceSize;
#ifdef POLYML32IN64
        // When we create the allocation space we take one word so that the first
        // length word is on an odd-word boundary.  We need to allow for that otherwise
        // we may have available < minWords.
        if (minWords >= spaceSize) spaceSize = minWords + (POLYML32IN64-1); // If we really want a large space.
#else
        if (minWords > spaceSize) spaceSize = minWords; // If we really want a large space.
#endif
        LocalMemSpace *space = CreateAllocationSpace(spaceSize);
        if (space == 0) return 0; // Can't allocate it
        // Allocate our space in this new area.
        uintptr_t available = space->freeSpace();
        ASSERT(available >= minWords);
        if (available < maxWords)
        {
            maxWords = available;
#ifdef POLYML32IN64
            // If necessary round down to the correct boundary
            while (maxWords & (POLYML32IN64 - 1))
            {
                maxWords--;
                space->lowerAllocPtr[maxWords] = PolyWord::FromUnsigned(0);
            }
#endif
        }
        PolyWord *result = space->lowerAllocPtr; // Return the address.
        if (doAllocation)
            space->lowerAllocPtr += maxWords; // Allocate it.
        return result;
    }
    return 0; // There isn't space even for the minimum.
}

CodeSpace::CodeSpace(PolyWord *start, PolyWord *shadow, uintptr_t spaceSize, OSMem *alloc): MarkableSpace(alloc)
{
    bottom = start;
    shadowSpace = shadow;
    top = start+spaceSize;
    isMutable = true; // Make it mutable just in case.  This will cause it to be scanned.
    isCode = true;
    spaceType = ST_CODE;
#ifdef POLYML32IN64
    // Dummy words so that the cell itself, after the length word, is on the correct boundary.
    for (unsigned i = 0; i < POLYML32IN64-1; i++)
        *(writeAble(start++)) = PolyWord::FromUnsigned(0);
    largestFree = spaceSize - POLYML32IN64;
#else
    largestFree = spaceSize - 1;
#endif
    firstFree = start;
}

CodeSpace *MemMgr::NewCodeSpace(uintptr_t size)
{
    // Allocate a new area and add it at the end of the table.
    CodeSpace *allocSpace = 0;
    // Allocate a new mutable, code space. N.B.  This may round up "actualSize".
    size_t actualSize = size * sizeof(PolyWord);
    void* shadow;
    PolyWord *mem =
        (PolyWord*)osCodeAlloc.AllocateCodeArea(actualSize, shadow);
    if (mem != 0)
    {
        try {
            allocSpace = new CodeSpace(mem, (PolyWord*)shadow, actualSize / sizeof(PolyWord), &osCodeAlloc);
            allocSpace->shadowSpace = (PolyWord*)shadow;
            if (!allocSpace->headerMap.Create(allocSpace->spaceSize()))
            {
                delete allocSpace;
                allocSpace = 0;
            }
            else if (!AddCodeSpace(allocSpace))
            {
                delete allocSpace;
                allocSpace = 0;
            }
            else if (debugOptions & DEBUG_MEMMGR)
                Log("MMGR: New code space %p allocated at %p size %lu\n", allocSpace, allocSpace->bottom, allocSpace->spaceSize());
            // Put in a byte cell to mark the area as unallocated.
            FillUnusedSpace(allocSpace->writeAble(allocSpace->firstFree), allocSpace->top- allocSpace->firstFree);
        }
        catch (std::bad_alloc&)
        {
        }
        if (allocSpace == 0)
        {
            osCodeAlloc.FreeCodeArea(mem, shadow, actualSize);
            mem = 0;
        }
    }
    return allocSpace;
}

// Allocate memory for a piece of code.  This needs to be both mutable and executable,
// at least for native code.  The interpreted version need not (should not?) make the
// area executable.  It will not be executed until the mutable bit has been cleared.
// Once code is allocated it is not GCed or moved.
// initCell is a byte cell that is copied into the new code area.
PolyObject* MemMgr::AllocCodeSpace(POLYUNSIGNED requiredSize)
{
    PLocker locker(&codeSpaceLock);
    // Search the code spaces until we find a free area big enough.
    size_t i = 0;
    while (true)
    {
        if (i != cSpaces.size())
        {
            CodeSpace *space = cSpaces[i];
            if (space->largestFree >= requiredSize)
            {
                POLYUNSIGNED actualLargest = 0;
                while (space->firstFree < space->top)
                {
                    PolyObject *obj = (PolyObject*)(space->firstFree+1);
                    // Skip over allocated areas or free areas that are too small.
                    if (obj->IsCodeObject() || obj->Length() < 8)
                        space->firstFree += obj->Length()+1;
                    else break;
                }
                PolyWord *pt = space->firstFree;
                while (pt < space->top)
                {
                    PolyObject *obj = (PolyObject*)(pt+1);
                    POLYUNSIGNED length = obj->Length();
                    if (obj->IsByteObject())
                    {
                        if (length >= requiredSize)
                        {
                            // Free and large enough
                            PolyWord *next = pt+requiredSize+1;
                            POLYUNSIGNED spare = length - requiredSize;
                            if (spare != 0)
                                FillUnusedSpace(space->writeAble(next), spare);
                            space->isMutable = true; // Set this - it ensures the area is scanned on GC.
                            space->headerMap.SetBit(pt-space->bottom); // Set the "header" bit
                            // Set the length word of the code area and copy the byte cell in.
                            // The code bit must be set before the lock is released to ensure
                            // another thread doesn't reuse this.
                            space->writeAble(obj)->SetLengthWord(requiredSize,  F_CODE_OBJ|F_MUTABLE_BIT);
                            return obj;
                        }
                        else if (length >= actualLargest) actualLargest = length+1;
                    }
                    pt += length+1;
                }
                // Reached the end without finding what we wanted.  Update the largest size.
                space->largestFree = actualLargest;
            }
            i++; // Next area
        }
        else
        {
            // Allocate a new area and add it at the end of the table.
            uintptr_t spaceSize = requiredSize + 1;
#ifdef POLYML32IN64
            // We need to allow for the extra alignment words otherwise we
            // may allocate less than we need.
            spaceSize += POLYML32IN64-1;
#endif
            CodeSpace *allocSpace = NewCodeSpace(spaceSize);
            if (allocSpace == 0)
                return 0; // Try a GC.
            globalStats.incSize(PSS_CODE_SPACE, allocSpace->spaceSize() * sizeof(PolyWord));
        }
    }
}

// Remove code areas that are completely empty.  This is probably better than waiting to reuse them.
// It's particularly important if we reload a saved state because the code areas for old saved states
// are made into local code areas just in case they are currently in use or reachable.
void MemMgr::RemoveEmptyCodeAreas()
{
    for (std::vector<CodeSpace *>::iterator i = cSpaces.begin(); i != cSpaces.end(); )
    {
        CodeSpace *space = *i;
        PolyObject *start = (PolyObject *)(space->bottom+1);
        if (start->IsByteObject() && start->Length() == space->spaceSize()-1)
        {
            if (debugOptions & DEBUG_MEMMGR)
                Log("MMGR: Deleted code space %p at %p size %zu\n", space, space->bottom, space->spaceSize());
            globalStats.decSize(PSS_CODE_SPACE, space->spaceSize() * sizeof(PolyWord));
            // We have an empty cell that fills the whole space.
            RemoveTree(space);
            delete(space);
            i = cSpaces.erase(i);
        }
        else i++;
    }
}

// Add a code space to the tables.  Used both for newly compiled code and also demoted saved spaces.
bool MemMgr::AddCodeSpace(CodeSpace *space)
{
    try {
        AddTree(space);
        cSpaces.push_back(space);
    }
    catch (std::exception&) {
        RemoveTree(space);
        return false;
    }
    return true;
}

// Check that we have sufficient space for an allocation to succeed.
// Called from the GC to ensure that we will not get into an infinite
// loop trying to allocate, failing and garbage-collecting again.
bool MemMgr::CheckForAllocation(uintptr_t words)
{
    uintptr_t allocated = 0;
    return AllocHeapSpace(words, allocated, false) != 0;
}

// Adjust the allocation area by removing free areas so that the total
// size of the allocation area is less than the required value.  This
// is used after the quick GC and also if we need to allocate a large
// object.
void MemMgr::RemoveExcessAllocation(uintptr_t words)
{
    // First remove any non-standard allocation areas.
    for (std::vector<LocalMemSpace*>::iterator i = lSpaces.begin(); i < lSpaces.end();)
    {
        LocalMemSpace *space = *i;
        if (space->allocationSpace && space->isEmpty() &&
                space->spaceSize() != defaultSpaceSize)
            DeleteLocalSpace(i);
        else i++;
    }
    for (std::vector<LocalMemSpace*>::iterator i = lSpaces.begin(); currentAllocSpace > words && i < lSpaces.end(); )
    {
        LocalMemSpace *space = *i;
        if (space->allocationSpace && space->isEmpty())
            DeleteLocalSpace(i);
        else i++;
    }
}

// Return number of words free in all allocation spaces.
uintptr_t MemMgr::GetFreeAllocSpace()
{
    uintptr_t freeSpace = 0;
    PLocker lock(&allocLock);
    for (std::vector<LocalMemSpace*>::iterator i = lSpaces.begin(); i < lSpaces.end(); i++)
    {
        LocalMemSpace *space = *i;
        if (space->allocationSpace)
            freeSpace += space->freeSpace();
    }
    return freeSpace;
}

StackSpace *MemMgr::NewStackSpace(uintptr_t size)
{
    PLocker lock(&stackSpaceLock);

    try {
        StackSpace *space = new StackSpace(&osStackAlloc);
        size_t iSpace = size*sizeof(PolyWord);
        space->bottom = (PolyWord*)osStackAlloc.AllocateDataArea(iSpace);
        if (space->bottom == 0)
        {
            if (debugOptions & DEBUG_MEMMGR)
                Log("MMGR: New stack space: insufficient space\n");
            delete space;
            return 0;
        }

        // The size may have been rounded up to a block boundary.
        size = iSpace/sizeof(PolyWord);
        space->top = space->bottom + size;
        space->spaceType = ST_STACK;
        space->isMutable = true;

        // Add the stack space to the tree.  This ensures that operations such as
        // LocalSpaceForAddress will work for addresses within the stack.  We can
        // get them in the RTS with functions such as quot_rem and exception stack.
        // It's not clear whether they really appear in the GC.
        try {
            AddTree(space);
            sSpaces.push_back(space);
        }
        catch (std::exception&) {
            RemoveTree(space);
            delete space;
            return 0;
        }
        if (debugOptions & DEBUG_MEMMGR)
            Log("MMGR: New stack space %p allocated at %p size %lu\n", space, space->bottom, space->spaceSize());
        globalStats.incSize(PSS_STACK_SPACE, space->spaceSize() * sizeof(PolyWord));
        return space;
    }
    catch (std::bad_alloc&) {
        if (debugOptions & DEBUG_MEMMGR)
            Log("MMGR: New stack space: \"new\" failed\n");
        return 0;
    }
}

// If checkmem is given write protect the immutable areas except during a GC.
void MemMgr::ProtectImmutable(bool on)
{
    if (debugOptions & DEBUG_CHECK_OBJECTS)
    {
        for (std::vector<LocalMemSpace*>::iterator i = lSpaces.begin(); i < lSpaces.end(); i++)
        {
            LocalMemSpace *space = *i;
            if (!space->isMutable)
            {
                if (!space->isCode)
                    osHeapAlloc.EnableWrite(!on, space->bottom, (char*)space->top - (char*)space->bottom);
            }
        }
    }
}

bool MemMgr::GrowOrShrinkStack(TaskData *taskData, uintptr_t newSize)
{
    StackSpace *space = taskData->stack;
    size_t iSpace = newSize*sizeof(PolyWord);

    PolyWord *newSpace = (PolyWord*)osStackAlloc.AllocateDataArea(iSpace);
    if (newSpace == 0)
    {
        if (debugOptions & DEBUG_MEMMGR)
            Log("MMGR: Unable to change size of stack %p from %lu to %lu: insufficient space\n",
                space, space->spaceSize(), newSize);
        return false;
    }
    // The size may have been rounded up to a block boundary.
    newSize = iSpace/sizeof(PolyWord);
    try {
        AddTree(space, newSpace, newSpace+newSize);
    }
    catch (std::bad_alloc&) {
        RemoveTree(space, newSpace, newSpace+newSize);
        delete space;
        return 0;
    }
    taskData->CopyStackFrame(space->stack(), space->spaceSize(), (StackObject*)newSpace, newSize);
    if (debugOptions & DEBUG_MEMMGR)
        Log("MMGR: Size of stack %p changed from %lu to %lu at %p\n", space, space->spaceSize(), newSize, newSpace);
    globalStats.incSize(PSS_STACK_SPACE, (newSize - space->spaceSize()) * sizeof(PolyWord));
    RemoveTree(space); // Remove it BEFORE freeing the space - another thread may allocate it
    PolyWord *oldBottom = space->bottom;
    size_t oldSize = (char*)space->top - (char*)space->bottom;
    space->bottom = newSpace; // Switch this before freeing - We could get a profile trap during the free
    space->top = newSpace+newSize;
    osStackAlloc.FreeDataArea(oldBottom, oldSize);
    return true;
}


// Delete a stack when a thread has finished.
// This can be called by an ML thread so needs an interlock.
bool MemMgr::DeleteStackSpace(StackSpace *space)
{
    PLocker lock(&stackSpaceLock);

    for (std::vector<StackSpace *>::iterator i = sSpaces.begin(); i < sSpaces.end(); i++)
    {
        if (*i == space)
        {
            globalStats.decSize(PSS_STACK_SPACE, space->spaceSize() * sizeof(PolyWord));
            RemoveTree(space);
            delete space;
            sSpaces.erase(i);
            if (debugOptions & DEBUG_MEMMGR)
                Log("MMGR: Deleted stack space %p at %p size %zu\n", space, space->bottom, space->spaceSize());
            return true;
        }
    }
    ASSERT(false); // It should always be in the table.
    return false;
}

SpaceTreeTree::SpaceTreeTree(): SpaceTree(false)
{
    for (unsigned i = 0; i < 256; i++)
        tree[i] = 0;
}

SpaceTreeTree::~SpaceTreeTree()
{
    for (unsigned i = 0; i < 256; i++)
    {
        if (tree[i] && ! tree[i]->isSpace)
            delete(tree[i]);
    }
}

// Add and remove entries in the space tree.

void MemMgr::AddTree(MemSpace *space, PolyWord *startS, PolyWord *endS)
{
    // It isn't clear we need to lock here but it's probably sensible.
    PLocker lock(&spaceTreeLock);
    AddTreeRange(&spaceTree, space, (uintptr_t)startS, (uintptr_t)endS);
}

void MemMgr::RemoveTree(MemSpace *space, PolyWord *startS, PolyWord *endS)
{
    PLocker lock(&spaceTreeLock);
    RemoveTreeRange(&spaceTree, space, (uintptr_t)startS, (uintptr_t)endS);
}


void MemMgr::AddTreeRange(SpaceTree **tt, MemSpace *space, uintptr_t startS, uintptr_t endS)
{
    if (*tt == 0)
        *tt = new SpaceTreeTree;
    ASSERT(! (*tt)->isSpace);
    SpaceTreeTree *t = (SpaceTreeTree*)*tt;

    const unsigned shift = (sizeof(void*)-1) * 8; // Takes the high-order byte
    uintptr_t r = startS >> shift;
    ASSERT(r < 256);
    const uintptr_t s = endS == 0 ? 256 : endS >> shift;
    ASSERT(s >= r && s <= 256);

    if (r == s) // Wholly within this entry
        AddTreeRange(&(t->tree[r]), space, startS << 8, endS << 8);
    else
    {
        // Deal with any remainder at the start.
        if ((r << shift) != startS)
        {
            AddTreeRange(&(t->tree[r]), space, startS << 8, 0 /*End of range*/);
            r++;
        }
        // Whole entries.
        while (r < s)
        {
            ASSERT(t->tree[r] == 0);
            t->tree[r] = space;
            r++;
        }
        // Remainder at the end.
        if ((s << shift) != endS)
            AddTreeRange(&(t->tree[r]), space, 0, endS << 8);
    }
}

// Remove an entry from the tree for a range.  Strictly speaking we don't need the
// space argument here but it's useful as a check.
// This may be called to remove a partially installed structure if we have
// run out of space in AddTreeRange.
void MemMgr::RemoveTreeRange(SpaceTree **tt, MemSpace *space, uintptr_t startS, uintptr_t endS)
{
    SpaceTreeTree *t = (SpaceTreeTree*)*tt;
    if (t == 0)
        return; // This can only occur if we're recovering.
    ASSERT(! t->isSpace);
    const unsigned shift = (sizeof(void*)-1) * 8;
    uintptr_t r = startS >> shift;
    const uintptr_t s = endS == 0 ? 256 : endS >> shift;

    if (r == s)
        RemoveTreeRange(&(t->tree[r]), space, startS << 8, endS << 8);
    else
    {
        // Deal with any remainder at the start.
        if ((r << shift) != startS)
        {
            RemoveTreeRange(&(t->tree[r]), space, startS << 8, 0);
            r++;
        }
        // Whole entries.
        while (r < s)
        {
            ASSERT(t->tree[r] == space || t->tree[r] == 0 /* Recovery only */);
            t->tree[r] = 0;
            r++;
        }
        // Remainder at the end.
        if ((s << shift) != endS)
            RemoveTreeRange(&(t->tree[r]), space, 0, endS << 8);
    }
    // See if the whole vector is now empty.
    for (unsigned j = 0; j < 256; j++)
    {
        if (t->tree[j])
            return; // It's not empty - we're done.
    }
    delete(t);
    *tt = 0;
}

uintptr_t MemMgr::AllocatedInAlloc()
{
    uintptr_t inAlloc = 0;
    for (std::vector<LocalMemSpace*>::iterator i = lSpaces.begin(); i < lSpaces.end(); i++)
    {
        LocalMemSpace *sp = *i;
        if (sp->allocationSpace) inAlloc += sp->allocatedSpace();
    }
    return inAlloc;
}

// Report heap sizes and occupancy before and after GC
void MemMgr::ReportHeapSizes(const char *phase)
{
    uintptr_t alloc = 0, nonAlloc = 0, inAlloc = 0, inNonAlloc = 0;
    for (std::vector<LocalMemSpace*>::iterator i = lSpaces.begin(); i < lSpaces.end(); i++)
    {
        LocalMemSpace *sp = *i;
        if (sp->allocationSpace)
        {
            alloc += sp->spaceSize();
            inAlloc += sp->allocatedSpace();
        }
        else
        {
            nonAlloc += sp->spaceSize();
            inNonAlloc += sp->allocatedSpace();
        }
    }
    Log("Heap: %s Major heap used ", phase);
    LogSize(inNonAlloc); Log(" of ");
    LogSize(nonAlloc);
    Log(" (%1.0f%%). Alloc space used ", (float)inNonAlloc / (float)nonAlloc * 100.0F);
    LogSize(inAlloc); Log(" of ");
    LogSize(alloc);
    Log(" (%1.0f%%). Total space ", (float)inAlloc / (float)alloc * 100.0F);
    LogSize(spaceForHeap);
    Log(" %1.0f%% full.\n", (float)(inAlloc + inNonAlloc) / (float)spaceForHeap * 100.0F);
    Log("Heap: Local spaces %" PRI_SIZET ", permanent spaces %" PRI_SIZET ", code spaces %" PRI_SIZET ", stack spaces %" PRI_SIZET "\n",
        lSpaces.size(), pSpaces.size(), cSpaces.size(), sSpaces.size());
    uintptr_t cTotal = 0, cOccupied = 0;
    for (std::vector<CodeSpace*>::iterator c = cSpaces.begin(); c != cSpaces.end(); c++)
    {
        cTotal += (*c)->spaceSize();
        PolyWord *pt = (*c)->bottom;
        while (pt < (*c)->top)
        {
            pt++;
            PolyObject *obj = (PolyObject*)pt;
            if (obj->ContainsForwardingPtr())
            {
#ifdef POLYML32IN64
                // This is relative to globalCodeBase not globalHeapBase
                while (obj->ContainsForwardingPtr())
                    obj = (PolyObject*)(globalCodeBase + (((uintptr_t)(obj->LengthWord()) & ~_OBJ_TOMBSTONE_BIT) * POLYML32IN64));
#else
                obj = obj->FollowForwardingChain();
#endif
                pt += obj->Length();
            }
            else
            {
                if (obj->IsCodeObject())
                    cOccupied += obj->Length() + 1;
                pt += obj->Length();
            }
        }
    }
    Log("Heap: Code area: total "); LogSize(cTotal); Log(" occupied: "); LogSize(cOccupied); Log("\n");
    uintptr_t stackSpace = 0;
    for (std::vector<StackSpace*>::iterator s = sSpaces.begin(); s != sSpaces.end(); s++)
    {
        stackSpace += (*s)->spaceSize();
    }
    Log("Heap: Stack area: total "); LogSize(stackSpace); Log("\n");
}

// Profiling - Find a code object or return zero if not found.
// This can be called on a "user" thread.
PolyObject *MemMgr::FindCodeObject(const byte *addr)
{
    MemSpace *space = SpaceForAddress(addr);
    if (space == 0) return 0;
    Bitmap *profMap = 0;
    if (! space->isCode) return 0;
    if (space->spaceType == ST_CODE)
    {
        CodeSpace *cSpace = (CodeSpace*)space;
        profMap = &cSpace->headerMap;
    }
    else if (space->spaceType == ST_PERMANENT)
    {
        PermanentMemSpace *pSpace = (PermanentMemSpace*)space;
        profMap = &pSpace->profileCode;
    }
    else return 0; // Must be in code or permanent code.

    // For the permanent areas the header maps are created and initialised on demand.
    if (! profMap->Created())
    {
        PLocker lock(&codeBitmapLock);
        if (! profMap->Created()) // Second check now we've got the lock.
        {
            // Create the bitmap.  If it failed just say "not in this area"
            if (! profMap->Create(space->spaceSize()))
                return 0;
            // Set the first bit before releasing the lock.
            profMap->SetBit(0);
        }
    }

    // A bit is set if it is a length word.
    while ((uintptr_t)addr & (sizeof(POLYUNSIGNED)-1)) addr--; // Make it word aligned
    PolyWord *wordAddr = (PolyWord*)addr;
    // Work back to find the first set bit before this.
    // Normally we will find one but if we're looking up a value that
    // is actually an integer it might be in a piece of code that is now free.
    uintptr_t bitOffset = profMap->FindLastSet(wordAddr - space->bottom);
    if (space->spaceType == ST_CODE)
    {
        PolyWord *ptr = space->bottom+bitOffset;
        if (ptr >= space->top) return 0;
        // This will find the last non-free code cell or the first cell.
        // Return zero if the value was not actually in the cell or it wasn't code.
        PolyObject *obj = (PolyObject*)(ptr+1);
#ifdef POLYML32IN64
        PolyObject *lastObj = obj;
        // This is relative to globalCodeBase not globalHeapBase.
        while (lastObj->ContainsForwardingPtr())
            lastObj = (PolyObject*)(globalCodeBase + (((uintptr_t)(lastObj->LengthWord()) & ~_OBJ_TOMBSTONE_BIT) * POLYML32IN64));
#else
        PolyObject *lastObj = obj->FollowForwardingChain();
#endif
        // We normally replace forwarding pointers but when scanning to update
        // addresses after a saved state we may not have yet done that.
        if (wordAddr > ptr && wordAddr < ptr + 1 + lastObj->Length() && lastObj->IsCodeObject())
            return obj;
        else return 0;
    }
    // Permanent area - the bits are set on demand.
    // Now work forward, setting any bits if necessary.  We don't need a lock
    // because this is monotonic.
    for (;;)
    {
        PolyWord *ptr = space->bottom+bitOffset;
        if (ptr >= space->top) return 0;
        PolyObject *obj = (PolyObject*)(ptr+1);
        ASSERT(obj->ContainsNormalLengthWord());
        if (wordAddr > ptr && wordAddr < ptr + obj->Length())
            return obj;
        bitOffset += obj->Length()+1;
        profMap->SetBit(bitOffset);
    }
    return 0;
}

// Remove profiling bitmaps from permanent areas to free up memory.
void MemMgr::RemoveProfilingBitmaps()
{
    for (std::vector<PermanentMemSpace*>::iterator i = pSpaces.begin(); i < pSpaces.end(); i++)
        (*i)->profileCode.Destroy();
}

#ifdef POLYML32IN64DEBUG
POLYOBJECTPTR PolyWord::AddressToObjectPtr(void *address)
{
    ASSERT(address >= globalHeapBase);
    uintptr_t offset = (PolyWord*)address - globalHeapBase;
    ASSERT((offset & 1) == 0); // The tag bit should not be set
    offset = offset / (POLYML32IN64 / 2);
    ASSERT(offset < (uintptr_t)0x100000000);
    return (POLYOBJECTPTR)offset;
}
#endif

MemMgr gMem; // The one and only memory manager object

