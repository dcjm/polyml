/*
    Title:      Multi-Threaded Garbage Collector - Copy phase

    Copyright (c) 2010-12 David C. J. Matthews

    Based on the original garbage collector code
        Copyright 2000-2008
        Cambridge University Technical Services Limited

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
/*
This is the second, copy, phase of the garbage collector.  The previous,
mark, phase has identified all the live data and set the bits in the bit-maps.
This phase compacts the memory by copying cells from lower in the segment or
from other segments.  When a cell is copied the length-word is modified to be
a tomb-stone that gives the new location for the cell.  Cells can be copied to
areas of memory that are shown as free in the bit-maps and the destination area
is then marked as allocated.  Because there are tomb-stones left behind the original
location of a cell must remain as allocated and its space cannot be reused until the
GC is complete.

We copy cells in a defined order to avoid copying loops.
The ordering on the addresses is:
    Immutable areas (for immutable cells) (highest)
    Mutable areas
    Allocation areas (lowest)
Within each group a lower position in the gMem.lSpaces is higher
MemMgr::AddLocalSpace enters spaces gMem.lSpaces such that immutable
areas come before mutable areas which come before allocation areas
so this reduces to the order in that table.
Within a space higher addresses are higher.
So we try to copy data out of the allocation areas and to copy any
cells that are now immutable out of the mutable areas.  We try to copy
data out of higher numbered spaces in order to try to free them
completely and to compact data towards the top of a space if we
can't.

Once a thread has started copying into or out of an area it takes
ownership of the area and no other thread can use the area.  This
avoids 
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

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "globals.h"
#include "machine_dep.h"
#include "processes.h"
#include "gc.h"
#include "scanaddrs.h"
#include "bitmap.h"
#include "memmgr.h"
#include "gctaskfarm.h"
#include "locking.h"
#include "diagnostics.h"

static PLock copyLock("Copy");

// Search the area downwards looking for n consecutive free words.
// Return the address of the word if successful or 0 on failure.
// "limit" is the bit position of the bottom of the area or, if we're compacting an area,
// the bit position of the object we'd like to move to a higher address.
static inline PolyWord *FindFreeAndAllocate(LocalMemSpace *dst, uintptr_t limit, uintptr_t n)
{
    if (dst == 0) return 0; // No current space

    /* SPF's version of the start caching code. SPF 2/10/96 */
    // The idea of it is to avoid having to search over an area that is
    // already known not to have any spaces large enough for an object of
    // a given size.  Knowing that there is no space for an object of
    // size n implies that there is no space for anything of size larger
    // than n.  SPF's idea is that after finding the space in the bitmap
    // we update only the element for the size we are looking for rather
    // than everything larger.
    unsigned truncated_n = (unsigned)(n < NSTARTS ? n : NSTARTS - 1);

    // If we're looking for something larger than last time update
    // all the entries last time's size and this size.
    for (unsigned i = dst->start_index; i < truncated_n; i ++)
    {
        if (dst->start[i] < dst->start[i+1])
            dst->start[i+1] = dst->start[i];
    }

    dst->start_index = truncated_n;
    uintptr_t start = dst->start[truncated_n];
    if (start <= limit)
        return 0;

#ifdef POLYML32IN64
    // This is complicated.  We need the eventual address to be on the correct word boundary
    // which means the length word is on that boundary minus 1.  We might find an exact match that
    // fits this or we may need to keep looking.
    // n is the number of words including the length word
    uintptr_t free = start;
    uintptr_t m = (n + POLYML32IN64 - 1) & (-POLYML32IN64); // Round it up
    for (;;)
    {
        uintptr_t lastFree = free;
        free = dst->bitmap.FindFree(limit, free, m);
        if (free == lastFree) { free = start;  break; } // Not there - return with free = start
        if ((free & (POLYML32IN64 - 1)) == POLYML32IN64 - 1) break; // It's on the correct boundary
        free = free & (-POLYML32IN64); // Round down to the boundary.
        // See if the cell before the boundary is free so we can use it for the
        // length word.
        if (!dst->bitmap.TestBit(free - 1))
        {
            // The previous bit is free - we can use this.
            // This leaves a hole but we'll zero it during the update phase.
            free = free - 1;
            break;
        }
    }
#else
    // Look in the bitmap.  Returns "start" if it can't find space.
    POLYUNSIGNED free = dst->bitmap.FindFree(limit, start, n);
#endif

    // If we failed to allocate the space (free == start) we set this to
    // zero to indicate that there is no space for anything of this size
    // or larger.
    if (n < NSTARTS)
        dst->start[n] = free == start ? 0 : free;

    if (free == start)
        return 0;

    // Allocate the space.
    dst->bitmap.SetBits(free, n);

    PolyWord *newp = dst->wordAddr(free); /* New object address */

    // Update dst->upperAllocPtr, so the new object doesn't get trampled.
    if (newp < dst->upperAllocPtr)
        dst->upperAllocPtr = newp;

    return newp;
}

// Copy a cell to its new address.
void CopyObjectToNewAddress(PolyObject *srcAddress, PolyObject *destAddress, POLYUNSIGNED L)
{
    destAddress->SetLengthWord(L); /* copy length word */

    POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L);

    // Unroll loop for most common cases.
    switch (n)
    {
    default:
        memcpy(destAddress, srcAddress, n * sizeof(PolyWord));
        break;
    case 4:
        destAddress->Set(3, srcAddress->Get(3));
    case 3:
        destAddress->Set(2, srcAddress->Get(2));
    case 2:
        destAddress->Set(1, srcAddress->Get(1));
    case 1:
        destAddress->Set(0, srcAddress->Get(0));
    }
}

// Find the next space in the sequence.  It may return with the space unchanged if it
// is unable to find a suitable space.
static bool FindNextSpace(LocalMemSpace *src, LocalMemSpace **dst, bool isMutable, GCTaskId *id)
{
    std::vector<LocalMemSpace*>::iterator m = gMem.lSpaces.begin();
    // If we're compressing the space and it's full that's it.
    if (*dst == src)
        return false;
    if (*dst != 0)
    {
        // Find the next space after this
        while (*m != *dst) m++;
        m++;
    }
    for (; m < gMem.lSpaces.end(); m++) {
        LocalMemSpace *lSpace = *m;
        if (lSpace == src)
        {
            // The only possibility is to compact this area.
            ASSERT(!isMutable || src->isMutable);
            *dst = src;
            return true; // We already own it
        }
        if (lSpace->isMutable == isMutable && !lSpace->allocationSpace && lSpace->spaceOwner == 0)
        {
            // Now acquire the lock.  We have to retest spaceOwner with the lock held.
            PLocker lock(&copyLock);
            if (lSpace->spaceOwner == 0)
            {
                // Change the space.
                lSpace->spaceOwner = id;
                *dst = lSpace; // Return the space
                if (debugOptions & DEBUG_GC_ENHANCED)
                    Log("GC: Copy: copying %s cells from %p to %p\n",
                                isMutable ? "mutable" : "immutable", src, lSpace);
                return true;
            }
        }
    }
    return false;
}

// Copy objects from the source space into an earlier space or up within the
// current space.
static void copyAllData(GCTaskId *id, void * /*arg1*/, void * /*arg2*/)
{
    LocalMemSpace *mutableDest = 0, *immutableDest = 0;

    for (std::vector<LocalMemSpace*>::reverse_iterator i = gMem.lSpaces.rbegin(); i != gMem.lSpaces.rend(); i++)
    {
        LocalMemSpace *src = *i;

        if (src->spaceOwner == 0)
        {
            PLocker lock(&copyLock);
            if (src->spaceOwner == 0)
                src->spaceOwner = id;
            else continue;
        }
        else if (src->spaceOwner != id)
            continue;

        if (debugOptions & DEBUG_GC_ENHANCED)
            Log("GC: Copy: copying area %p (thread %p) %s \n", src, id, src->spaceTypeString());

        // We start at fullGCLowerLimit which is the lowest marked object in the heap
        // N.B.  It's essential that the first set bit at or above this corresponds
        // to the length word of a real object.
        uintptr_t  bitno   = src->wordNo(src->fullGCLowerLimit);
        // Set the limit to the top so we won't rescan this.  That can
        // only happen if copying takes a very short time and the same
        // thread runs multiple tasks.
        src->fullGCLowerLimit = src->top;

        // src->highest is the bit position that corresponds to the top of
        // generation we're copying.
        uintptr_t  highest = src->wordNo(src->top);

        for (;;)
        {
            if (bitno >= highest) break;

            /* SPF version; Invariant: 0 < highest - bitno */
            bitno += src->bitmap.CountZeroBits(bitno, highest - bitno);

            if (bitno >= highest) break;

            /* first set bit corresponds to the length word */
            PolyWord *old = src->wordAddr(bitno); /* Old object address */

            PolyObject *obj = (PolyObject*)(old+1);

            POLYUNSIGNED L = obj->LengthWord();
            ASSERT (OBJ_IS_LENGTH(L));

            POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L) + 1 ;/* Length of allocation (including length word) */
            bitno += n;

            // Find a mutable space for the mutable objects and an immutable space for
            // the immutables.  We copy objects into earlier spaces or within its own
            // space but we don't copy an object to a later space.  This avoids the
            // risk of copying an object multiple times.  Previously this copied objects
            // into later spaces but that doesn't work well if we have converted old
            // saved state segments into local areas.  It's much better to delete them
            // if possible.
            bool isMutable = OBJ_IS_MUTABLE_OBJECT(L);
            LocalMemSpace *destSpace = isMutable || immutableDest == 0 ? mutableDest : immutableDest;
            PolyWord *newp = FindFreeAndAllocate(destSpace, (src == destSpace) ? bitno : 0, n);
            if (newp == 0 && src != destSpace)
            {
                // See if we can find a different space.
                // N.B.  FindNextSpace side-effects mutableDest/immutableDest to give the next space.
                if (FindNextSpace(src, isMutable ? &mutableDest : &immutableDest, isMutable, id))
                {
                    bitno -= n; // Redo this object
                    continue;
                }
                // else just leave it
            }

            if (newp == 0) /* no room */
            {
                // We're not going to move this object
                // Update src->upperAllocPtr, so the old object doesn't get trampled.
                if (old < src->upperAllocPtr)
                    src->upperAllocPtr = old;

                // Previously this continued compressing to try to make space available
                // on the next GC.  Normally full GCs are infrequent so the chances are
                // that at the next GC other data will have been freed.  Just stop at
                // this point.
                // However if we're compressing a mutable area and there is immutable
                // data in it we should move those out because the mutable area is scanned
                // on every partial GC.
                if (! src->isMutable || src->i_marked == 0)
                    break;
            }
            else
            {
                PolyObject *destAddress = (PolyObject*)(newp+1);
                obj->SetForwardingPtr(destAddress);
                CopyObjectToNewAddress(obj, destAddress, L);

                if (debugOptions & DEBUG_GC_DETAIL)
                    Log("GC: Copy: %p %lu %u -> %p\n", obj, OBJ_OBJECT_LENGTH(L),
                                GetTypeBits(L), destAddress);
            }
        }

        if (mutableDest == src)
            mutableDest = 0;
        if (immutableDest == src)
            immutableDest = 0;
    }
}

void GCCopyPhase()
{
    mainThreadPhase = MTP_GCPHASECOMPACT;

    for(std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
    {
        LocalMemSpace *lSpace = *i;
        uintptr_t highest = lSpace->wordNo(lSpace->top);
        for (unsigned i = 0; i < NSTARTS; i++)
            lSpace->start[i] = highest;
        lSpace->start_index = NSTARTS - 1;
        lSpace->spaceOwner = 0;
        // Reset the allocation pointers. This puts garbage (and real data) below them.
        // At the end of the compaction the allocation pointer will point below the
        // lowest real data.
        lSpace->upperAllocPtr = lSpace->top;
    }

    // Copy the mutable data into a lower area if possible.
    if (gpTaskFarm->ThreadCount() == 0)
        copyAllData(globalTask, 0, 0);
    else
    {
        // We start as many tasks as we have threads.  If the amount of work to
        // be done is very small one thread could process more than one task.
        // Have to be careful because we use the task ID to decide which space
        // to scan.
        for (unsigned j = 0; j < gpTaskFarm->ThreadCount(); j++)
            gpTaskFarm->AddWorkOrRunNow(&copyAllData, 0, 0);
    }

    gpTaskFarm->WaitForCompletion();
}
