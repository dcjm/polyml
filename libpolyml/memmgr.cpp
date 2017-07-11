/*
    Title:  memmgr.cpp   Memory segment manager

    Copyright (c) 2006-7, 2011-12, 2016 David C. J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
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

// heap resizing policy option requested on command line
unsigned heapsizingOption = 0;

MemSpace::MemSpace(): SpaceTree(true)
{
    spaceType = ST_PERMANENT;
    isMutable = false;
    bottom = 0;
    top = 0;
    isOwnSpace = false;
    isCode = false;
}

MemSpace::~MemSpace()
{
    if (isOwnSpace && bottom != 0)
        osMemoryManager->Free(bottom, (char*)top - (char*)bottom);
}

LocalMemSpace::LocalMemSpace(): spaceLock("Local space")
{
    spaceType = ST_LOCAL;
    upperAllocPtr = lowerAllocPtr = 0;
    for (unsigned i = 0; i < NSTARTS; i++)
        start[i] = 0;
    start_index = 0;
    i_marked = m_marked = updated = 0;
    allocationSpace = false;
}

bool LocalMemSpace::InitSpace(POLYUNSIGNED size, bool mut)
{
    isMutable = mut;

    // Allocate the heap itself.
    size_t iSpace = size*sizeof(PolyWord);
    bottom  =
        (PolyWord*)osMemoryManager->Allocate(iSpace, PERMISSION_READ|PERMISSION_WRITE);

    if (bottom == 0)
        return false;
    isOwnSpace = true; // Deallocate when we're finished.

    // The size may have been rounded up to a block boundary.
    size = iSpace/sizeof(PolyWord);

    top = bottom + size;
    // Initialise all the fields.  The partial GC in particular relies on this.
    upperAllocPtr = partialGCTop = fullGCRescanStart = fullGCLowerLimit = lowestWeak = top;
    lowerAllocPtr = partialGCScan = partialGCRootBase = partialGCRootTop =
        fullGCRescanEnd = highestWeak = bottom;
    spaceOwner = 0;

    allocationSpace = false;

    // Bitmap for the space.
    return bitmap.Create(size);
}

MemMgr::MemMgr(): allocLock("Memmgr alloc")
{
    npSpaces = nlSpaces = nsSpaces = ncSpaces = 0;
    pSpaces = 0;
    lSpaces = 0;
    eSpaces = 0;
    sSpaces = 0;
    cSpaces = 0;
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
    unsigned i;
    for (i = 0; i < npSpaces; i++)
        delete(pSpaces[i]);
    free(pSpaces);
    for (i = 0; i < nlSpaces; i++)
        delete(lSpaces[i]);
    free(lSpaces);
    for (i = 0; i < neSpaces; i++)
        delete(eSpaces[i]);
    free(eSpaces);
    for (i = 0; i < nsSpaces; i++)
        delete(sSpaces[i]);
    free(sSpaces);
    for (i = 0; i < ncSpaces; i++)
        delete(cSpaces[i]);
    free(cSpaces);
}

// Create and initialise a new local space and add it to the table.
LocalMemSpace* MemMgr::NewLocalSpace(POLYUNSIGNED size, bool mut)
{
    try {
        LocalMemSpace *space = new LocalMemSpace;
        // Before trying to allocate the heap temporarily allocate the
        // reserved space.  This ensures that this much space will always
        // be available for C stacks and the C++ heap.
        void *reservation = 0;
        size_t rSpace = reservedSpace*sizeof(PolyWord);

        if (reservedSpace != 0) {
            reservation = osMemoryManager->Allocate(rSpace, PERMISSION_READ);
            if (reservation == 0) {
                // Insufficient space for the reservation.  Can't allocate this local space.
                if (debugOptions & DEBUG_MEMMGR)
                    Log("MMGR: New local %smutable space: insufficient reservation space\n", mut ? "": "im");
                delete space;
                return 0;
            }
        }

        bool success = space->InitSpace(size, mut) && AddLocalSpace(space);
        if (reservation != 0) osMemoryManager->Free(reservation, rSpace);
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
LocalMemSpace *MemMgr::CreateAllocationSpace(POLYUNSIGNED size)
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
    LocalMemSpace **table = (LocalMemSpace **)realloc(lSpaces, (nlSpaces+1) * sizeof(LocalMemSpace *));
    if (table == 0) return false;
    lSpaces = table;
    // Update the B-tree.
    try {
        AddTree(space);
    }
    catch (std::bad_alloc&) {
        RemoveTree(space);
        return false;
    }
    // The entries in the local table are ordered so that the copy phase of the full
    // GC simply has to copy to an entry earlier in the table.  Immutable spaces come
    // first, followed by mutable spaces and finally allocation spaces.
    if (space->allocationSpace)
        lSpaces[nlSpaces++] = space; // Just add at the end
    else if (space->isMutable)
    {
        // Add before the allocation spaces
        unsigned s;
        for (s = nlSpaces; s > 0 && lSpaces[s-1]->allocationSpace; s--)
            lSpaces[s] = lSpaces[s-1];
        lSpaces[s] = space;
        nlSpaces++;
    }
    else
    {
        // Immutable space: Add before the mutable spaces
        unsigned s;
        for (s = nlSpaces; s > 0 && lSpaces[s-1]->isMutable; s--)
            lSpaces[s] = lSpaces[s-1];
        lSpaces[s] = space;
        nlSpaces++;
    }
    return true;
}


// Create an entry for a permanent space.
PermanentMemSpace* MemMgr::NewPermanentSpace(PolyWord *base, POLYUNSIGNED words,
                                             unsigned flags, unsigned index, unsigned hierarchy /*= 0*/)
{
    try {
        PermanentMemSpace *space = new PermanentMemSpace;
        space->bottom = base;
        space->topPointer = space->top = space->bottom + words;
        space->spaceType = ST_PERMANENT;
        space->isMutable = flags & MTF_WRITEABLE ? true : false;
        space->noOverwrite = flags & MTF_NO_OVERWRITE ? true : false;
        space->byteOnly = flags & MTF_BYTES ? true : false;
        space->isCode = flags & MTF_EXECUTABLE ? true : false;
        space->index = index;
        space->hierarchy = hierarchy;
        if (index >= nextIndex) nextIndex = index+1;

        // Extend the permanent memory table and add this space to it.
        PermanentMemSpace **table =
            (PermanentMemSpace **)realloc(pSpaces, (npSpaces+1) * sizeof(PermanentMemSpace *));
        if (table == 0)
        {
            delete space;
            return 0;
        }
        pSpaces = table;
        try {
            AddTree(space);
        }
        catch (std::bad_alloc&) {
            RemoveTree(space);
            delete space;
            return 0;
        }
        pSpaces[npSpaces++] = space;
        return space;
    }
    catch (std::bad_alloc&) {
        return 0;
    }
}

// Delete a local space and remove it from the table.
bool MemMgr::DeleteLocalSpace(LocalMemSpace *sp)
{
    for (unsigned i = 0; i < nlSpaces; i++)
    {
        if (lSpaces[i] == sp)
        {
            if (debugOptions & DEBUG_MEMMGR)
                Log("MMGR: Deleted local %s space %p\n", sp->spaceTypeString(), sp);
            currentHeapSize -= sp->spaceSize();
            globalStats.setSize(PSS_TOTAL_HEAP, currentHeapSize * sizeof(PolyWord));
            if (sp->allocationSpace) currentAllocSpace -= sp->spaceSize();
            RemoveTree(sp);
            delete sp;
            nlSpaces--;
            while (i < nlSpaces)
            {
                lSpaces[i] = lSpaces[i+1];
                i++;
            }
            return true;
        }
    }
    ASSERT(false); // It should always be in the table.
    return false;
}

// Remove local areas that are now empty after a GC.
// It isn't clear if we always want to do this.
void MemMgr::RemoveEmptyLocals()
{
    for (unsigned s = nlSpaces; s > 0; s--)
    {
        LocalMemSpace *space = lSpaces[s-1];
        if (space->allocatedSpace() == 0)
            DeleteLocalSpace(space);
    }
}

// Create and initialise a new export space and add it to the table.
PermanentMemSpace* MemMgr::NewExportSpace(POLYUNSIGNED size, bool mut, bool noOv, bool code)
{
    try {
        PermanentMemSpace *space = new PermanentMemSpace;
        space->spaceType = ST_EXPORT;
        space->isMutable = mut;
        space->noOverwrite = noOv;
        space->isCode = code;
        space->index = nextIndex++;
        // Allocate the memory itself.
        size_t iSpace = size*sizeof(PolyWord);
        space->bottom  =
            (PolyWord*)osMemoryManager->Allocate(iSpace, PERMISSION_READ|PERMISSION_WRITE|PERMISSION_EXEC);

        if (space->bottom == 0)
        {
            delete space;
            return 0;
        }
        space->isOwnSpace = true;
 
        // The size may have been rounded up to a block boundary.
        size = iSpace/sizeof(PolyWord);
        space->top = space->bottom + size;
        space->topPointer = space->bottom;

        // Add to the table.
        PermanentMemSpace **table = (PermanentMemSpace **)realloc(eSpaces, (neSpaces+1) * sizeof(PermanentMemSpace *));
        if (table == 0)
        {
            delete space;
            return 0;
        }
        eSpaces = table;
        try {
            AddTree(space);
        }
        catch (std::bad_alloc&) {
            RemoveTree(space);
            delete space;
            return 0;
        }
        eSpaces[neSpaces++] = space;
        return space;
    }
    catch (std::bad_alloc&) {
        return 0;
    }
}

void MemMgr::DeleteExportSpaces(void)
{
    while (neSpaces > 0)
    {
        PermanentMemSpace *space = eSpaces[--neSpaces];
        RemoveTree(space);
        delete(space);
    }
}

// If we have saved the state rather than exported a function we turn the exported
// spaces into permanent ones, removing existing permanent spaces at the same or
// lower level.
bool MemMgr::PromoteExportSpaces(unsigned hierarchy)
{
    // Create a new table big enough to hold all the permanent and export spaces
    PermanentMemSpace **pTable =
        (PermanentMemSpace **)calloc(npSpaces+neSpaces, sizeof(PermanentMemSpace *));
    if (pTable == 0) return false;
    unsigned newSpaces = 0;
    // Save permanent spaces at a lower hierarchy.  Others are converted into
    // local spaces.  Most or all items will have been copied from these spaces
    // into an export space but there could be items reachable only from the stack.
    for (unsigned i = 0; i < npSpaces; i++)
    {
        PermanentMemSpace *pSpace = pSpaces[i];
        if (pSpace->hierarchy < hierarchy)
            pTable[newSpaces++] = pSpace;
        else
        {
            try {
                // Turn this into a local space.
                // Remove this from the tree - AddLocalSpace will make an entry for the local version.
                RemoveTree(pSpace);
                LocalMemSpace *space = new LocalMemSpace;
                space->top = space->fullGCLowerLimit = pSpace->top;
                space->bottom = space->upperAllocPtr = space->lowerAllocPtr = pSpace->bottom;
                space->isMutable = pSpace->isMutable;
                space->isOwnSpace = true;
                if (! space->bitmap.Create(space->top-space->bottom) || ! AddLocalSpace(space))
                    return false;
                currentHeapSize += space->spaceSize();
                globalStats.setSize(PSS_TOTAL_HEAP, currentHeapSize * sizeof(PolyWord));
            }
            catch (std::bad_alloc&) {
                return false;
            }
        }
    }
    // Save newly exported spaces.
    for (unsigned j = 0; j < neSpaces; j++)
    {
        PermanentMemSpace *space = eSpaces[j];
        space->hierarchy = hierarchy; // Set the hierarchy of the new spaces.
        space->spaceType = ST_PERMANENT;
        // Put a dummy object to fill up the unused space.
        if (space->topPointer != space->top)
            FillUnusedSpace(space->topPointer, space->top - space->topPointer);
        // Put in a dummy object to fill the rest of the space.
        pTable[newSpaces++] = space;
    }
    neSpaces = 0;
    npSpaces = newSpaces;
    free(pSpaces);
    pSpaces = pTable;

    return true;
}


// Before we import a hierarchical saved state we need to turn any previously imported
// spaces into local spaces.
bool MemMgr::DemoteImportSpaces()
{
    // Create a new permanent space table.
    PermanentMemSpace **table =
        (PermanentMemSpace **)calloc(npSpaces, sizeof(PermanentMemSpace *));
    if (table == NULL) return false;
    unsigned newSpaces = 0;
    for (unsigned i = 0; i < npSpaces; i++)
    {
        PermanentMemSpace *pSpace = pSpaces[i];
        if (pSpace->hierarchy == 0) // Leave truly permanent spaces
            table[newSpaces++] = pSpace;
        else
        {
            try {
                // Turn this into a local space or a code space
                // Remove this from the tree - AddLocalSpace will make an entry for the local version.
                RemoveTree(pSpace);

                if (pSpace->isCode)
                {
                    CodeSpace *space = new CodeSpace;
                    space->top = pSpace->top;
                    // Space is allocated in local areas from the top down.  This area is full and
                    // all data is in the old generation.  The area can be recovered by a full GC.
                    space->bottom = pSpace->bottom;
                    space->isMutable = true; // Make it mutable just in case.  This will cause it to be scanned.
                    space->isOwnSpace = true;
                    space->isCode = true;
                    if (!AddCodeSpace(space))
                    {
                        if (debugOptions & DEBUG_MEMMGR)
                            Log("MMGR: Unable to convert saved state space %p into code space\n", pSpace);
                        return false;
                    }
                    if (debugOptions & DEBUG_MEMMGR)
                        Log("MMGR: Converted saved state space %p into code space %p\n", pSpace, space);
                }
                else
                {
                    LocalMemSpace *space = new LocalMemSpace;
                    space->top = pSpace->top;
                    // Space is allocated in local areas from the top down.  This area is full and
                    // all data is in the old generation.  The area can be recovered by a full GC.
                    space->bottom = space->upperAllocPtr = space->lowerAllocPtr =
                        space->fullGCLowerLimit = pSpace->bottom;
                    space->isMutable = pSpace->isMutable;
                    space->isOwnSpace = true;
                    space->isCode = false;
                    if (! space->bitmap.Create(space->top-space->bottom) || ! AddLocalSpace(space))
                    {
                        if (debugOptions & DEBUG_MEMMGR)
                            Log("MMGR: Unable to convert saved state space %p into local space\n", pSpace);
                        return false;
                    }
                    if (debugOptions & DEBUG_MEMMGR)
                        Log("MMGR: Converted saved state space %p into local %smutable space %p\n",
                                pSpace, pSpace->isMutable ? "im": "", space);
                    currentHeapSize += space->spaceSize();
                    globalStats.setSize(PSS_TOTAL_HEAP, currentHeapSize * sizeof(PolyWord));
                }
            }
            catch (std::bad_alloc&) {
                if (debugOptions & DEBUG_MEMMGR)
                    Log("MMGR: Unable to convert saved state space %p into local space (\"new\" failed)\n", pSpace);
                return false;
            }
        }
    }
    npSpaces = newSpaces;
    free(pSpaces);
    pSpaces = table;

    return true;
}

// Return the space for a given index
PermanentMemSpace *MemMgr::SpaceForIndex(unsigned index) const
{
    for (unsigned i = 0; i < npSpaces; i++)
    {
        PermanentMemSpace *space = pSpaces[i];
        if (space->index == index)
            return space;
    }
    return NULL;
}

// In several places we assume that segments are filled with valid
// objects.  This fills unused memory with one or more "byte" objects.
void MemMgr::FillUnusedSpace(PolyWord *base, POLYUNSIGNED words)
{
    PolyWord *pDummy = base+1;
    while (words > 0)
    {
        POLYUNSIGNED oSize = words;
        // If the space is larger than the maximum object size
        // we will need several objects.
        if (words > MAX_OBJECT_SIZE) oSize = MAX_OBJECT_SIZE;
        else oSize = words-1;
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
PolyWord *MemMgr::AllocHeapSpace(POLYUNSIGNED minWords, POLYUNSIGNED &maxWords, bool doAllocation)
{
    PLocker locker(&allocLock);
    // We try to distribute the allocations between the memory spaces
    // so that at the next GC we don't have all the most recent cells in
    // one space.  The most recent cells will be more likely to survive a
    // GC so distibuting them improves the load balance for a multi-thread GC.
    nextAllocator++;
    if (nextAllocator > gMem.nlSpaces) nextAllocator = 0;

    for (unsigned j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *space = gMem.lSpaces[(j + nextAllocator) % gMem.nlSpaces];
        if (space->allocationSpace)
        {
            POLYUNSIGNED available = space->freeSpace();
            if (available > 0 && available >= minWords)
            {
                // Reduce the maximum value if we had less than that.
                if (available < maxWords)
                    maxWords = available;
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
        POLYUNSIGNED spaceSize = defaultSpaceSize;
        if (minWords > spaceSize) spaceSize = minWords; // If we really want a large space.
        LocalMemSpace *space = CreateAllocationSpace(spaceSize);
        if (space == 0) return 0; // Can't allocate it
        // Allocate our space in this new area.
        POLYUNSIGNED available = space->freeSpace();
        ASSERT(available >= minWords);
        if (available < maxWords)
            maxWords = available;
        PolyWord *result = space->lowerAllocPtr; // Return the address.
        if (doAllocation)
            space->lowerAllocPtr += maxWords; // Allocate it.
        return result;
    }
    return 0; // There isn't space even for the minimum.
}

// Allocate memory for a piece of code.  This needs to be both mutable and executable,
// at least for native code.  The interpreted version need not (should not?) make the
// area executable.  It will not be executed until the mutable bit has been cleared.
// Once code is allocated it is not GCed or moved.
// N.B.  The argument size must include the length word; the result is a pointer
// to the base of the area or zero if it can't be allocated.
PolyWord *MemMgr::AllocCodeSpace(POLYUNSIGNED words)
{
    PLocker locker(&codeSpaceLock);
    CodeSpace *allocSpace = 0;
    for (unsigned j = 0; j < gMem.ncSpaces && allocSpace == 0; j++)
    {
        CodeSpace *space = gMem.cSpaces[j];
        if ((POLYUNSIGNED)(space->top - space->topPointer) >= words)
            allocSpace = space;
    }
    if (allocSpace == 0)
    {
        // Allocate a new mutable, code space.
        size_t actualSize = words * sizeof(PolyWord);
        PolyWord *mem  =
            (PolyWord*)osMemoryManager->Allocate(actualSize,
                            PERMISSION_READ|PERMISSION_WRITE|PERMISSION_EXEC);
        if (mem != 0)
        {
            try {
                allocSpace = new CodeSpace;
                allocSpace->bottom = allocSpace->topPointer = mem;
                allocSpace->top = allocSpace->bottom + actualSize / sizeof(PolyWord);
                allocSpace->spaceType = ST_CODE;
                allocSpace->isMutable = true;
                allocSpace->isCode = true;
                allocSpace->isOwnSpace = true;
                if (! AddCodeSpace(allocSpace))
                {
                    delete allocSpace;
                    allocSpace = 0;
                }
                else if (debugOptions & DEBUG_MEMMGR)
                    Log("MMGR: New code space %p allocated at %p size %lu\n", allocSpace, allocSpace->bottom, allocSpace->spaceSize());
            } catch (std::bad_alloc&)
            {
            }
            if (allocSpace == 0)
            {
                osMemoryManager->Free(mem, actualSize);
                mem = 0;
            }
        }
        if (allocSpace == 0)
            return 0; // Try a GC.
    }
    // Set the mutable flag.  This is cleared by the GC when it has been scanned.
    allocSpace->isMutable = true;
    PolyWord *result = allocSpace->topPointer; // Return the address.
    allocSpace->topPointer += words;
    if (allocSpace->topPointer != allocSpace->top)
        FillUnusedSpace(allocSpace->topPointer, allocSpace->top - allocSpace->topPointer);
    return result;
}

// Add a code space to the tables.  Used both for newly compiled code and also demoted saved spaces.
bool MemMgr::AddCodeSpace(CodeSpace *space)
{
    CodeSpace **table =
        (CodeSpace **)realloc(cSpaces, (ncSpaces+1) * sizeof(CodeSpace *));
    if (table == 0) return false;
    try {
        AddTree(space);
    }
    catch (std::bad_alloc&) {
        RemoveTree(space);
        return false;
    }
    cSpaces = table;
    cSpaces[ncSpaces++] = space;
    return true;
}

// Check that we have sufficient space for an allocation to succeed.
// Called from the GC to ensure that we will not get into an infinite
// loop trying to allocate, failing and garbage-collecting again.
bool MemMgr::CheckForAllocation(POLYUNSIGNED words)
{
    POLYUNSIGNED allocated = 0;
    return AllocHeapSpace(words, allocated, false) != 0;
}

// Adjust the allocation area by removing free areas so that the total
// size of the allocation area is less than the required value.  This
// is used after the quick GC and also if we need to allocate a large
// object.
void MemMgr::RemoveExcessAllocation(POLYUNSIGNED words)
{
    // First remove any non-standard allocation areas.
    unsigned i;
    for (i = nlSpaces; i > 0; i--)
    {
        LocalMemSpace *space = lSpaces[i-1];
        if (space->allocationSpace && space->allocatedSpace() == 0 &&
                space->spaceSize() != defaultSpaceSize)
            DeleteLocalSpace(space);
    }
    for (i = nlSpaces; currentAllocSpace > words && i > 0; i--)
    {
        LocalMemSpace *space = lSpaces[i-1];
        if (space->allocationSpace && space->allocatedSpace() == 0)
            DeleteLocalSpace(space);
    }
}

// Return number of words free in all allocation spaces.
POLYUNSIGNED MemMgr::GetFreeAllocSpace()
{
    POLYUNSIGNED freeSpace = 0;
    PLocker lock(&allocLock);
    for (unsigned j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *space = gMem.lSpaces[j];
        if (space->allocationSpace)
            freeSpace += space->freeSpace();
    }
    return freeSpace;
}

StackSpace *MemMgr::NewStackSpace(POLYUNSIGNED size)
{
    PLocker lock(&stackSpaceLock);

    try {
        StackSpace *space = new StackSpace;
        size_t iSpace = size*sizeof(PolyWord);
        space->bottom =
            (PolyWord*)osMemoryManager->Allocate(iSpace, PERMISSION_READ|PERMISSION_WRITE);
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

        // Extend the permanent memory table and add this space to it.
        StackSpace **table =
            (StackSpace **)realloc(sSpaces, (nsSpaces+1) * sizeof(StackSpace *));
        if (table == 0)
        {
            if (debugOptions & DEBUG_MEMMGR)
                Log("MMGR: New stack space: table realloc failed\n");
            delete space;
            return 0;
        }
        sSpaces = table;
        // Add the stack space to the tree.  This ensures that operations such as
        // LocalSpaceForAddress will work for addresses within the stack.  We can
        // get them in the RTS with functions such as quot_rem and exception stack.
        // It's not clear whether they really appear in the GC.
        try {
            AddTree(space);
        }
        catch (std::bad_alloc&) {
            RemoveTree(space);
            delete space;
            return 0;
        }
        sSpaces[nsSpaces++] = space;
        if (debugOptions & DEBUG_MEMMGR)
            Log("MMGR: New stack space %p allocated at %p size %lu\n", space, space->bottom, space->spaceSize());
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
        for (unsigned i = 0; i < nlSpaces; i++)
        {
            LocalMemSpace *space = lSpaces[i];
            if (! space->isMutable)
                osMemoryManager->SetPermissions(space->bottom, (char*)space->top - (char*)space->bottom,
                    on ? PERMISSION_READ|PERMISSION_EXEC : PERMISSION_READ|PERMISSION_EXEC|PERMISSION_WRITE);
        }
    }
}

bool MemMgr::GrowOrShrinkStack(TaskData *taskData, POLYUNSIGNED newSize)
{
    StackSpace *space = taskData->stack;
    size_t iSpace = newSize*sizeof(PolyWord);
    PolyWord *newSpace = (PolyWord*)osMemoryManager->Allocate(iSpace, PERMISSION_READ|PERMISSION_WRITE);
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
    RemoveTree(space); // Remove it BEFORE freeing the space - another thread may allocate it
    PolyWord *oldBottom = space->bottom;
    size_t oldSize = (char*)space->top - (char*)space->bottom;
    space->bottom = newSpace; // Switch this before freeing - We could get a profile trap during the free
    space->top = newSpace+newSize;
    osMemoryManager->Free(oldBottom, oldSize);
    return true;
}


// Delete a stack when a thread has finished.
// This can be called by an ML thread so needs an interlock.
bool MemMgr::DeleteStackSpace(StackSpace *space)
{
    PLocker lock(&stackSpaceLock);

    for (unsigned i = 0; i < nsSpaces; i++)
    {
        if (sSpaces[i] == space)
        {
            RemoveTree(space);
            delete space;
            nsSpaces--;
            while (i < nsSpaces)
            {
                sSpaces[i] = sSpaces[i+1];
                i++;
            }
            if (debugOptions & DEBUG_MEMMGR)
                Log("MMGR: Deleted stack space %p\n", space);
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

POLYUNSIGNED MemMgr::AllocatedInAlloc()
{
    POLYUNSIGNED inAlloc = 0;
    for (unsigned i = 0; i < nlSpaces; i++)
    {
        LocalMemSpace *sp = lSpaces[i];
        if (sp->allocationSpace) inAlloc += sp->allocatedSpace();
    }
    return inAlloc;
}

// Report heap sizes and occupancy before and after GC
void MemMgr::ReportHeapSizes(const char *phase)
{
    POLYUNSIGNED alloc = 0, nonAlloc = 0, inAlloc = 0, inNonAlloc = 0;
    for (unsigned i = 0; i < nlSpaces; i++)
    {
        LocalMemSpace *sp = lSpaces[i];
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
}

MemMgr gMem; // The one and only memory manager object

