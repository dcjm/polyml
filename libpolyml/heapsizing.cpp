/*
    Title:  heapsizing.cpp - parameters to adjust heap size

    Copyright (c) Copyright David C.J. Matthews 2012, 2015, 2017, 2023, 2026

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
This module is intended to deal with heap sizing based on measurements of the time taken
in the GC compared with the application code.  Currently it is very basic.
This also provides GC timing information to the ML code as well as statistics and
debugging.
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h> // For sysconf
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_SYSCTL_H
#include <sys/sysctl.h>
#endif

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

#ifdef HAVE_MATH_H
#include <math.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#include "globals.h"
#include "arb.h"
#include "diagnostics.h"
#include "rts_module.h"
#include "timing.h"
#include "heapsizing.h"
#include "statistics.h"
#include "memmgr.h"

// The one and only parameter object
HeapSizeParameters gHeapSizeParameters;

#ifdef HAVE_WINDOWS_H
// There's no (documented) way to get the per-process hard page
// count in Windows.  Cygwin uses GetProcessMemoryInfo to return the
// value in ru_majflt but this is actually incorrect because it returns
// the soft page count not the hard page count.  We previously used the
// undocumented NtQuerySystemInformation call.
static long GetPaging(long)
{
    return 0;
}
#else
inline long GetPaging(long rusagePage)
{
    return rusagePage;
}
#endif

HeapSizeParameters::HeapSizeParameters()
{
    startPF = GetPaging(0);
    fullGCNextTime = false;
    performSharingPass = false;
    lastAllocationSucceeded = true;
    allocationFailedBeforeLastMajorGC = false;
    minHeapSize = 0;
    maxHeapSize = 0; // Unlimited
    lastFreeSpace = 0;
    pagingLimitSize = 0;
    highWaterMark = 0;
    sharingWordsRecovered = 0;
    cumulativeSharingSaving = 0;
    // Initial values until we've actually done a sharing pass.
    sharingRecoveryRate = 0.5; // The structure sharing recovers half the heap.
    sharingCostFactor = 2; // It doubles the cost
    allowGCSharing = false;
    currentSpaceUsed = 0;
    heapSizeAtStart = 0;
    lastMajorGCRatio = 0;
    majorGCPageFaults = minorGCPageFaults = minorGCsSinceMajor = 0;
    predictedRatio = userGCRatio = 0;
}

// These macros were originally in globals.h and used more generally.
// Since only K_to_words is used now this can be greatly simplified.
#define BITSPERWORD (sizeof(PolyWord)*8)
#define ROUNDUP_UNITS(m,n)   (((m) + (n) - 1) / (n))
#define ROUNDUP(m,n)   (ROUNDUP_UNITS(m,n) * (n))
#define K_to_words(k) ROUNDUP((k) * (1024 / sizeof(PolyWord)),BITSPERWORD)

// Returns physical memory size in bytes
static size_t GetPhysicalMemorySize(void);

// These are the maximum values for the number of words.
#if (SIZEOF_VOIDP == 4)
#   define MAXIMUMADDRESS   0x3fffffff /* 4Gbytes as words */
#elif defined(POLYML32IN64)
#   define MAXIMUMADDRESS   ((uintptr_t)0x80000000*POLYML32IN64-1) /* 16/32Gbytes as words */ 
#else
#   define MAXIMUMADDRESS   0x1fffffffffffffff
#endif

// Set the initial size based on any parameters specified on the command line.
// Any of these can be zero indicating they should default.
void HeapSizeParameters::SetHeapParameters(uintptr_t minsize, uintptr_t maxsize, uintptr_t initialsize, unsigned percent, bool gcshare)
{
    minHeapSize = K_to_words(minsize); // If these overflow assume the result will be zero
    maxHeapSize = K_to_words(maxsize);
    uintptr_t initialSize = K_to_words(initialsize);

//   uintptr_t memsize = GetPhysicalMemorySize() / sizeof(PolyWord);

    // If no maximum is given default it to 80% of the physical memory.
    // This allows some space for the OS and other things.
    // We now check maxsize so it should never exceed the maximum.
    if (maxHeapSize == 0 || maxHeapSize > MAXIMUMADDRESS)
    {
        maxHeapSize = MAXIMUMADDRESS;
        // But if this must not be smaller than the minimum size.
        if (maxHeapSize < minHeapSize) maxHeapSize = minHeapSize;
        if (maxHeapSize < initialSize) maxHeapSize = initialSize;
    }

    // The default minimum is zero; in practice the live data size.

    // The default initial size is the minimum if that has been provided,
    // otherwise 8M words.  There are applications that only require a small
    // heap and if we set the heap large to begin with we'll never do a
    // full GC and reduce it.
    if (initialSize == 0)
    {
        if (minHeapSize != 0)
            initialSize = minHeapSize;
        else initialSize = 8 * gMem.DefaultSpaceSize();
        // But not more than the maximum
        if (initialSize > maxHeapSize) initialSize = maxHeapSize;
    }
    // Together with the constraints on user settings that ensures this holds.
    ASSERT(initialSize >= minHeapSize && initialSize <= maxHeapSize);

    allowGCSharing = gcshare;

    // Initially we divide the space equally between the major and
    // minor heaps.  That means that there will definitely be space
    // for the first minor GC to copy its data.  This division can be
    // changed later on.
    gMem.SetSpaceForHeap(initialSize);
    gMem.SetSpaceBeforeMinorGC(initialSize/2);
    lastFreeSpace = initialSize;
    highWaterMark = initialSize;

    if (percent == 0)
        userGCRatio = 1.0 / 9.0; // Default to 10% GC to 90% application
    else
        userGCRatio = (float)percent / (float)(100 - percent);

    predictedRatio = lastMajorGCRatio = userGCRatio;

    if (debugOptions & DEBUG_HEAPSIZE)
    {
        Log("Heap: Initial settings: Initial heap ");
        LogSize(initialSize);
        Log(" minimum ");
        LogSize(minHeapSize);
        Log(" maximum ");
        LogSize(maxHeapSize);
        Log(" target ratio %f\n", userGCRatio);
    }
}

void HeapSizeParameters::SetReservation(uintptr_t rsize)
{
    gMem.SetReservation(K_to_words(rsize));
}

// Called in the minor GC if a GC thread needs to grow the heap.
// Returns zero if the heap cannot be grown. "space" is the space required for the
// object (and length field) in case this is larger than the default size.
LocalMemSpace *HeapSizeParameters::AddSpaceInMinorGC(uintptr_t space, bool isMutable)
{
    // See how much space is allocated to the major heap.
    uintptr_t spaceAllocated = gMem.CurrentHeapSize() - gMem.CurrentAllocSpace();

    // The new segment is either the default size or as large as
    // necessary for the object.
    uintptr_t spaceSize = gMem.DefaultSpaceSize();
#ifdef POLYML32IN64
    // When we allocate a space in NewLocalSpace we take one or more words to ensure
    // the that the first length word is on an odd-word boundary.
    // We need to add one here to ensure there is sufficient space to do that.
    // See AllocHeapSpace
    space += POLYML32IN64-1;
#endif
    if (space > spaceSize) spaceSize = space;

    // The heap may become fragmented in 32-in-64 so try a smaller allocation
    do {
        // We allow for extension if the total heap size after extending it
        // plus one allocation area of the default size would not be more
        // than the allowed heap size.
        if (spaceAllocated + spaceSize + gMem.DefaultSpaceSize() <= gMem.SpaceForHeap())
        {
            LocalMemSpace* sp = gMem.NewLocalSpace(spaceSize, isMutable); // Return the space or zero if it failed
            lastAllocationSucceeded = sp != 0;
            if (sp != 0) return sp;
        }
        spaceSize = spaceSize / 2;
    } while (spaceSize > space);

    // If this is the first time the allocation failed report it.
    if ((debugOptions & DEBUG_HEAPSIZE) && lastAllocationSucceeded)
    {
        Log("Heap: Allocation of new heap segment size ");
        LogSize(spaceSize);
        Log(" failed.  Limit reached?\n");
    }
    return 0; // Insufficient space
}

// Called in the major GC before the copy phase if the heap is more than
// 90% full.  This should improve the efficiency of copying.
LocalMemSpace *HeapSizeParameters::AddSpaceBeforeCopyPhase(bool isMutable)
{
    LocalMemSpace *sp = gMem.NewLocalSpace(gMem.DefaultSpaceSize(), isMutable);
    if (sp == 0 && (debugOptions & DEBUG_HEAPSIZE) && lastAllocationSucceeded)
        Log("Heap: Allocation of new heap segment failed.  Limit reached?\n");
    lastAllocationSucceeded = sp != 0;
    return sp;
}

// The steepness of the curve.
#define PAGINGCOSTSTEEPNESS 20.0
// The additional cost at the boundary
#define PAGINGCOSTFACTOR    3.0
// The number of pages at the boundary
#define PAGINGCOUNTFACTOR   1000.0

// Called at the end of collection.  This is where we should do the
// fine adjustment of the heap size to minimise the GC time.
// Growing the heap is just a matter of adjusting the limits.  We
// don't actually need to allocate the space here.
// See also adjustHeapSizeAfterMinorGC for adjustments after a minor GC.
void HeapSizeParameters::AdjustSizeAfterMajorGC(uintptr_t wordsRequired)
{
    // Cumulative times since the last major GC
    TIMEDATA gc, nonGc;
    gc.add(majorGCSystemCPU);
    gc.add(majorGCUserCPU);
    nonGc.add(majorNonGCSystemCPU);
    nonGc.add(majorNonGCUserCPU);

    if (highWaterMark < heapSizeAtStart) highWaterMark = heapSizeAtStart;

    uintptr_t heapSpace = gMem.SpaceForHeap() < highWaterMark ? gMem.SpaceForHeap() : highWaterMark;
    currentSpaceUsed = wordsRequired;
    for (std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
    {
        currentSpaceUsed += (*i)->allocatedSpace();
    }
    // N.B.  Normally currentSpaceUsed will be less than the size of the heap
    // except if wordsRequired is very large.

    // The times for all the minor GCs up to this.  The cost of this (major) GC
    // is actually in minorGCUserCPU/minorGCSystemCPU.
    TIMEDATA minorGC;
    minorGC.add(gc);
    minorGC.sub(minorGCUserCPU);
    minorGC.sub(minorGCSystemCPU);

    if (performSharingPass)
    {
        // We ran the sharing pass last time: calculate the actual recovery rate.
        uintptr_t originalSpaceUsed = currentSpaceUsed + sharingWordsRecovered;
        sharingRecoveryRate = (double)sharingWordsRecovered / (double)originalSpaceUsed;
        if (debugOptions & DEBUG_HEAPSIZE)
            Log("Heap: Sharing recovery rate was %0.3f and cost %0.3f seconds (%0.3f%% of total).\n",
                sharingRecoveryRate, sharingCPU.toSeconds(), sharingCPU.toSeconds() / gc.toSeconds());
        // The cost factor is the ratio of the cost of sharing to the cost without.
        sharingCostFactor = sharingCPU.toSeconds() / (gc.toSeconds() - sharingCPU.toSeconds());
        // Subtract the sharing cost from the GC cost because the initial estimate is
        // the cost without running the sharing pass.
        gc.sub(sharingCPU);
    }

    if (gc.toSeconds() != 0.0 && nonGc.toSeconds() != 0.0)
        lastMajorGCRatio = gc.toSeconds() / nonGc.toSeconds();

    if (debugOptions & DEBUG_HEAPSIZE)
    {
        uintptr_t currentFreeSpace = currentSpaceUsed > heapSpace ? 0: heapSpace - currentSpaceUsed;
        Log("Heap: GC cpu time %2.3f non-gc time %2.3f ratio %0.3f for free space ",
            gc.toSeconds(), nonGc.toSeconds(), lastMajorGCRatio);
        LogSize((lastFreeSpace + currentFreeSpace)/2);
        Log("\n");
        Log("Heap: GC real time %2.3f non-gc time %2.3f ratio %0.3f\n",
            majorGCReal.toSeconds(), majorNonGCReal.toSeconds(), majorGCReal.toSeconds()/majorNonGCReal.toSeconds());
        Log("Heap: Total of minor GCs %2.3f, %2.3f of total\n", minorGC.toSeconds(), minorGC.toSeconds() / gc.toSeconds());
    }

    // Calculate the paging threshold.
    if (pagingLimitSize != 0 && majorGCPageFaults == 0)
    {
        if (debugOptions & DEBUG_HEAPSIZE)
            Log("No paging seen so resetting pageLimitSize\n");
        pagingLimitSize = 0;
    }
    else if (pagingLimitSize != 0 || majorGCPageFaults != 0)
    {
        if (majorGCPageFaults == 0) majorGCPageFaults = 1; // Less than one
        // Some paging detected.  The expression here is the inverse of the one used to
        // compute the paging contribution in the cost function.
        double scaleFactor = 1.0 + log((double)majorGCPageFaults / PAGINGCOUNTFACTOR) / PAGINGCOSTSTEEPNESS;
        ASSERT(scaleFactor > 0.0);
        uintptr_t newLimit = (uintptr_t)((double)heapSpace / scaleFactor);
        if (pagingLimitSize == 0)
            pagingLimitSize = newLimit;
        else 
            pagingLimitSize = (newLimit + pagingLimitSize) / 2;
    }
    if (allocationFailedBeforeLastMajorGC)
    {
        // If the last allocation failed then we may well have reached the
        // maximum available memory.  Set the paging limit to be the current
        // heap size.  We want to avoid hitting the limit because typically
        // that happens when we try to extend the major heap in a minor GC
        // resulting in the minor GC failing and a major GC starting.
        if (pagingLimitSize == 0 || heapSizeAtStart < pagingLimitSize)
            pagingLimitSize = heapSizeAtStart;
    }
    if (pagingLimitSize != 0 && (debugOptions & DEBUG_HEAPSIZE))
    {
        Log("Heap: Paging threshold adjusted to ");
        LogSize(pagingLimitSize);
        Log(" with %ld page faults\n", majorGCPageFaults);
    }

    // Calculate the new heap size and the predicted cost.
    uintptr_t newHeapSize;
    double cost;
    bool atTarget = getCostAndSize(newHeapSize, wordsRequired, cost, false);
    // If we have been unable to allocate any more memory we may already
    // be at the limit.
    if (allocationFailedBeforeLastMajorGC && newHeapSize > heapSizeAtStart)
    {
        cost = costFunction(heapSizeAtStart, false, true);
        atTarget = false;
    }

    if (atTarget)
    {
        // We are at the target level.  We don't want to attempt sharing.
        performSharingPass = false;
        cumulativeSharingSaving = 0;
    }
    else if (allowGCSharing)
    {
        uintptr_t newHeapSizeWithSharing;
        double costWithSharing;
        // Get the cost and heap size if sharing was enabled.  If we are at the
        // limit, though, we need to work using the size we can achieve.
        if (!allocationFailedBeforeLastMajorGC)
            (void)getCostAndSize(newHeapSizeWithSharing, wordsRequired, costWithSharing, true);
        else
        {
            newHeapSizeWithSharing = heapSizeAtStart;
            costWithSharing = costFunction(heapSizeAtStart, true, true);
        }
        // Run the sharing pass if that would give a lower cost.
        // Subtract the cumulative saving that would have been made if the
        // sharing had been run before.  This is an estimate and depends on the
        // extent to which a reduction in the heap earlier would be carried through
        // to later GCs.
        cumulativeSharingSaving =
            cumulativeSharingSaving * ((double)currentSpaceUsed / (double)heapSpace);
        if (debugOptions & DEBUG_HEAPSIZE)
            Log("Heap: Cumulative sharing saving %0.2f\n", cumulativeSharingSaving);
        if (costWithSharing - cumulativeSharingSaving < cost)
        {
            // Run the sharing pass next time.
            performSharingPass = true;
            cumulativeSharingSaving = 0;
        }
        else
        {
            // Don't run the sharing pass next time
            performSharingPass = false;
            // Running a sharing pass reduces the heap for subsequent
            // runs.  Add this into the cost.
            double freeSharingCost = costFunction(newHeapSizeWithSharing, true, false);
            if (freeSharingCost < cost && freeSharingCost > userGCRatio)
            {
                if (debugOptions & DEBUG_HEAPSIZE)
                    Log("Heap: Previous sharing would have saved %0.2f\n", cost - freeSharingCost);
                cumulativeSharingSaving += cost - freeSharingCost;
            }
        }
    }
    else performSharingPass = false;

    if (debugOptions & DEBUG_HEAPSIZE)
    {
        if (performSharingPass)
            Log("Heap: Next full GC will enable the sharing pass\n");
        Log("Heap: Resizing from ");
        LogSize(gMem.SpaceForHeap());
        Log(" to ");
        LogSize(newHeapSize);
        Log(".  Estimated ratio %2.2f\n", cost);
    }
    // Set the sizes.
    gMem.SetSpaceForHeap(newHeapSize);
    // Set the minor space size.  It can potentially use the whole of the
    // rest of the available heap but there could be a problem if that exceeds
    // the available memory and causes paging.  We need to raise the limit carefully.
    // Also, if we use the whole of the heap we may not then be able to allocate
    // new areas in the major heap without going over the limit.  Restrict it to
    // half of the available heap.  Add the wordsRequired in case we're trying to
    // allocate something very big.
    uintptr_t nextLimit = highWaterMark + highWaterMark / 32 + wordsRequired;
    if (nextLimit > newHeapSize) nextLimit = newHeapSize;
    // gMem.CurrentHeapSize() is the live space size.
    if (gMem.CurrentHeapSize() > nextLimit)
    // This previously set the space to zero which resulted in a Run out of space
    // error.  Instead allow at least this allocation in the hope that everything
    // will sort itself out.
        gMem.SetSpaceBeforeMinorGC(wordsRequired);
    else gMem.SetSpaceBeforeMinorGC((nextLimit-gMem.CurrentHeapSize())/2);

    lastFreeSpace = newHeapSize - currentSpaceUsed;
    predictedRatio = cost;
}

// Called after a minor GC.  Currently does nothing.
// See also adjustHeapSize for adjustments after a major GC.
bool HeapSizeParameters::AdjustSizeAfterMinorGC(uintptr_t spaceAfterGC, uintptr_t spaceBeforeGC)
{
    uintptr_t spaceCopiedOut = spaceAfterGC-spaceBeforeGC;
    TIMEDATA gc, nonGC;
    minorGCsSinceMajor++;
    // Work out the ratio of GC to non-GC time since the last major GC. We can only adjust the heap size
    // at a major GC.  If we're now spending too much time in minor GCs we may need a major GC to adjust
    // the size.
    gc.add(minorGCSystemCPU);
    gc.add(minorGCUserCPU);
    nonGC.add(minorNonGCSystemCPU);
    nonGC.add(minorNonGCUserCPU);
    float g = gc.toSeconds() / nonGC.toSeconds();

    if (debugOptions & DEBUG_HEAPSIZE)
    {
        Log("Heap: Space before ");
        LogSize(spaceBeforeGC);
        Log(", space after ");
        LogSize(spaceAfterGC);
        Log("\n");
        Log("Heap: Minor resizing factors g = %f, recent pf = %ld, cumulative pf = %ld\n",
            g, minorGCPageFaults, majorGCPageFaults);
    }

    if (highWaterMark < gMem.CurrentHeapSize()) highWaterMark = gMem.CurrentHeapSize();

    uintptr_t nextLimit = highWaterMark + highWaterMark / 32;
    if (nextLimit > gMem.SpaceForHeap()) nextLimit = gMem.SpaceForHeap();

    // Set the space available for the allocation area to be the difference between the
    // total heap size and the allowed heap size together with as much space as we copied
    // on this GC.  That allows for the next minor GC to copy the same amount without
    // extending the heap.  If the next minor GC adds more than this the heap will be
    // extended and a corresponding amount deducted so that the heap shrinks again.
    uintptr_t currHeap = gMem.CurrentHeapSize();
    uintptr_t currAlloc = gMem.CurrentAllocSpace();
    uintptr_t nonAlloc = currHeap - currAlloc + spaceCopiedOut;
    // TODO: If we have limited the space to the high water mark + 1/32 but that is less
    // than we really need we should increase it further.
    uintptr_t allowedAlloc = nonAlloc >= nextLimit ? 0 : nextLimit - nonAlloc;
    // Normally the allocation area will be empty but if we've failed to copy
    // everything out, especially a big object, it may not be.
    uintptr_t allocatedInAlloc = gMem.AllocatedInAlloc();

    // If we hit the limit at the last major GC we have to be much more careful.
    // If the minor GC cannot allocate a major GC space when it needs it the minor
    // GC will fail immediately and a major GC will be started.  It's better to
    // risk doing more minor GCs than we need by making the allocation area smaller
    // rather than run out of space.
    if (allocationFailedBeforeLastMajorGC)
        allowedAlloc = allowedAlloc / 2;
    if (gMem.CurrentAllocSpace() - allocatedInAlloc != allowedAlloc)
    {
        if (debugOptions & DEBUG_HEAPSIZE)
        {
            Log("Heap: Adjusting space for allocation area from ");
            LogSize(gMem.SpaceBeforeMinorGC());
            Log(" to ");
            LogSize(allowedAlloc);
            Log("\n");
        }
        gMem.SetSpaceBeforeMinorGC(allowedAlloc);
        if (allowedAlloc < gMem.DefaultSpaceSize() * 2 || minorGCPageFaults > 100)
            return false; // Trigger full GC immediately.
     }

    // Trigger a full GC if the live data is very large or if we have exceeeded
    // the target ratio over several GCs (this smooths out small variations).
    if ((minorGCsSinceMajor > 4 && g > predictedRatio*0.8) || majorGCPageFaults > 100)
        fullGCNextTime = true;
    return true;
}

// Estimate the GC cost for a given heap size.  The result is the ratio of
// GC time to application time.
// This is really guesswork.
double HeapSizeParameters::costFunction(uintptr_t heapSize, bool withSharing, bool withSharingCost)
{
    uintptr_t heapSpace = gMem.SpaceForHeap() < highWaterMark ? gMem.SpaceForHeap() : highWaterMark;
    uintptr_t currentFreeSpace = heapSpace < currentSpaceUsed ? 0: heapSpace - currentSpaceUsed;
    uintptr_t averageFree = (lastFreeSpace + currentFreeSpace) / 2;
    uintptr_t spaceUsed = currentSpaceUsed; // N.B.  currentSpaceUsed includes the new space we want
    if (heapSize <= currentSpaceUsed)
        return 1.0E6;
    // If we run the sharing pass the live space will be smaller.
    if (withSharing)
        spaceUsed -= (uintptr_t)((double)currentSpaceUsed * sharingRecoveryRate);
    uintptr_t estimatedFree = heapSize - spaceUsed;
    // The cost scales as the inverse of the amount of free space.
    double result = lastMajorGCRatio * (double)averageFree / (double)estimatedFree;
    // If we run the sharing pass the GC cost will increase.
    if (withSharing && withSharingCost)
        result += result*sharingCostFactor;

    // The paging contribution depends on the page limit
    double pagingCost = 0.0;
    if (pagingLimitSize != 0)
    {
        double factor = ((double)heapSize - (double)pagingLimitSize) / (double)pagingLimitSize * PAGINGCOSTSTEEPNESS;
        pagingCost = PAGINGCOSTFACTOR * exp(factor);
        result += pagingCost;
    }

    if (debugOptions & DEBUG_HEAPSIZE)
    {
        Log("Heap: Cost for heap of size ");
        LogSize(heapSize);
        Log(" is %2.2f with paging contributing %2.2f with%s sharing pass.\n", result, pagingCost, withSharing ? "" : "out");
    }
    return result;
}

// Calculate the size for the minimum cost.  Returns true if this is bounded by
// the user GC ratio and false if we minimised the cost
// TODO: This could definitely be improved although it's not likely to contribute much to
// the overall cost of a GC.
bool HeapSizeParameters::getCostAndSize(uintptr_t &heapSize, uintptr_t wordsRequired, double &cost, bool withSharing)
{
    bool isBounded = false;
    uintptr_t heapSpace = gMem.SpaceForHeap() < highWaterMark ? gMem.SpaceForHeap() : highWaterMark;
    // Calculate a new heap size.  We allow a maximum doubling or halving of size.
    // It's probably more important to limit the increase in case we hit paging.
    // We add the words required to this.  That matters if we have a small heap but need
    // to allocate a very large array.
    uintptr_t sizeMax = heapSpace * 2 + wordsRequired;
    if (sizeMax > maxHeapSize) sizeMax = maxHeapSize;
    uintptr_t sizeMin = heapSpace / 2 + wordsRequired;
    if (sizeMin < minHeapSize) sizeMin = minHeapSize;
    // We mustn't reduce the heap size too far.  If the application does a lot
    // of work with few allocations and particularly if it calls PolyML.fullGC
    // explicitly we could attempt to shrink the heap below the current live data size.
    // Add 3*space size here.  We require 2* after a minor GC. Add 1 for rounding.
    uintptr_t minForAllocation = gMem.CurrentHeapSize() + gMem.DefaultSpaceSize() * 3;
    if (minForAllocation > maxHeapSize) minForAllocation = maxHeapSize;
    if (sizeMin < minForAllocation) sizeMin = minForAllocation;

    double costMin = costFunction(sizeMin, withSharing, true);
    if (costMin <= userGCRatio)
        // If the cost of the minimum is below or at the target we
        // use that and don't need to look further.
        isBounded = true;
    else
    {
        double costMax = costFunction(sizeMax, withSharing, true);
        while (sizeMax > sizeMin + gMem.DefaultSpaceSize())
        {
            uintptr_t sizeNext = (sizeMin + sizeMax) / 2;
            double cost = costFunction(sizeNext, withSharing, true);
            if (cost < userGCRatio)
                isBounded = true;
            if (cost < userGCRatio || (costMax > costMin && costMax > userGCRatio))
            {
                sizeMax = sizeNext;
                costMax = cost;
            }
            else
            {
                sizeMin = sizeNext;
                costMin = cost;
            }
            ASSERT(costMin >= userGCRatio);
        }
    }
    ASSERT(sizeMin >= minHeapSize && sizeMin <= maxHeapSize);
    // If we are bounded by the user GC ratio we actually return the size and cost
    // that is slightly above the user ratio.
    heapSize = sizeMin;
    cost = costMin;
    return isBounded;
}

bool HeapSizeParameters::RunMajorGCImmediately()
{
    if (fullGCNextTime)
    {
        fullGCNextTime = false;
        return true;
    }
    return false;
}


static bool GetLastStats(TIMEDATA &userTime, TIMEDATA &systemTime, TIMEDATA &realTime, long &pageCount)
{
#if (defined(_WIN32))
    FILETIME kt, ut;
    FILETIME ct, et; // Unused
    FILETIME rt;
    GetProcessTimes(GetCurrentProcess(), &ct, &et, &kt, &ut);
    GetSystemTimeAsFileTime(&rt);
    userTime = ut;
    systemTime = kt;
    realTime = rt;
    pageCount = GetPaging(0);
#else
    struct rusage rusage;
    if (getrusage(RUSAGE_SELF, &rusage) != 0)
        return false;
    userTime = rusage.ru_utime;
    systemTime = rusage.ru_stime;
    struct timeval tv;
    if (gettimeofday(&tv, NULL) != 0)
        return false;
    realTime = tv;
    pageCount = GetPaging(rusage.ru_majflt);
#endif
    return true;
}

void HeapSizeParameters::RecordAtStartOfMajorGC()
{
    heapSizeAtStart = gMem.CurrentHeapSize();
    allocationFailedBeforeLastMajorGC = !lastAllocationSucceeded;
}

// This function is called at the beginning and end of garbage
// collection to record the time used.
// This also reports the GC time if GC debugging is enabled.
void HeapSizeParameters::RecordGCTime(gcTime isEnd, const char *stage)
{
    switch (isEnd)
    {
    case GCTimeStart:
        {
            // Start of GC
            TIMEDATA userTime, systemTime, realTime;
            long pageCount;
            if (! GetLastStats(userTime, systemTime, realTime, pageCount))
                break;
            lastUsageU = userTime;
            lastUsageS = systemTime;
            lastRTime = realTime;
            userTime.sub(startUsageU);  // Times since the start
            systemTime.sub(startUsageS);
            realTime.sub(startRTime);
            if (debugOptions & DEBUG_GC)
                Log("GC: Non-GC time: CPU user: %0.3f system: %0.3f real: %0.3f page faults: %ld\n",
                    userTime.toSeconds(), systemTime.toSeconds(), realTime.toSeconds(), pageCount - startPF);
            minorNonGCUserCPU.add(userTime);
            majorNonGCUserCPU.add(userTime);
            minorNonGCSystemCPU.add(systemTime);
            majorNonGCSystemCPU.add(systemTime);
            minorNonGCReal.add(realTime);
            majorNonGCReal.add(realTime);
            startUsageU = lastUsageU;
            startUsageS = lastUsageS;
            startRTime = lastRTime;
            // Page faults in the application are included
            minorGCPageFaults += pageCount - startPF;
            majorGCPageFaults += pageCount - startPF;
            startPF = pageCount;
            break;
        }

    case GCTimeIntermediate:
        // Report intermediate GC time for debugging
        if (debugOptions & DEBUG_GC)
        {
            TIMEDATA userTime, systemTime, realTime;
            long pageCount;
            if (! GetLastStats(userTime, systemTime, realTime, pageCount))
                break;
            TIMEDATA nextU = userTime, nextS = systemTime, nextR = realTime;
            userTime.sub(lastUsageU);
            systemTime.sub(lastUsageS);
            realTime.sub(lastRTime);

            Log("GC: (%s) CPU user: %0.3f system: %0.3f real: %0.3f speed up %0.1f\n", stage, userTime.toSeconds(), 
                systemTime.toSeconds(), realTime.toSeconds(),
                realTime.toSeconds() == 0.0 ? 0.0 : (userTime.toSeconds() + systemTime.toSeconds()) / realTime.toSeconds());
            lastUsageU = nextU;
            lastUsageS = nextS;
            lastRTime = nextR;
        }
        break;

    case GCTimeEnd: // End of GC.
        {
            TIMEDATA userTime, systemTime, realTime;
            long pageCount;
            if (! GetLastStats(userTime, systemTime, realTime, pageCount))
                break;
            lastUsageU = userTime;
            lastUsageS = systemTime;
            lastRTime = realTime;

            userTime.sub(startUsageU);  // Times since the start
            systemTime.sub(startUsageS);
            realTime.sub(startRTime);

            totalGCUserCPU.add(userTime);
            totalGCSystemCPU.add(systemTime);
            totalGCReal.add(realTime);

            if (debugOptions & DEBUG_GC)
            {
                Log("GC: CPU user: %0.3f system: %0.3f real: %0.3f speed up %0.1f page faults %ld\n", userTime.toSeconds(), 
                    systemTime.toSeconds(), realTime.toSeconds(),
                    realTime.toSeconds() == 0.0 ? 0.0 : (userTime.toSeconds() + systemTime.toSeconds()) / realTime.toSeconds(),
                    pageCount - startPF);
            }
            minorGCUserCPU.add(userTime);
            majorGCUserCPU.add(userTime);
            minorGCSystemCPU.add(systemTime);
            majorGCSystemCPU.add(systemTime);
            minorGCReal.add(realTime);
            majorGCReal.add(realTime);
            startUsageU = lastUsageU;
            startUsageS = lastUsageS;
            startRTime = lastRTime;
            minorGCPageFaults += pageCount - startPF;
            majorGCPageFaults += pageCount - startPF;
            startPF = pageCount;
            globalStats.copyGCTimes(totalGCUserCPU, totalGCSystemCPU, totalGCReal);
        }
        break;
    }
}

// Record the recovery rate and cost after running the GC sharing pass.
// TODO: We should probably average these because if we've run a full
// sharing pass and then a full GC after the recovery rate will be zero.
void HeapSizeParameters::RecordSharingData(POLYUNSIGNED recovery)
{
    sharingWordsRecovered = recovery;
    TIMEDATA userTime, systemTime, realTime;
    long pageCount;
    if (! GetLastStats(userTime, systemTime, realTime, pageCount))
        return;
    userTime.sub(startUsageU);  // Times since the start
    systemTime.sub(startUsageS);
    sharingCPU = userTime;
    sharingCPU.add(systemTime);
}

Handle HeapSizeParameters::getGCUtime(TaskData *taskData) const
{
#if (defined(_WIN32))
    return Make_arb_from_Filetime(taskData, totalGCUserCPU);
#else
    return Make_arb_from_pair_scaled(taskData, ((struct timeval)totalGCUserCPU).tv_sec, ((struct timeval)totalGCUserCPU).tv_usec, 1000000);
#endif
}

Handle HeapSizeParameters::getGCStime(TaskData *taskData) const
{
#if (defined(_WIN32))
    return Make_arb_from_Filetime(taskData, totalGCSystemCPU);
#else
    return Make_arb_from_pair_scaled(taskData, ((struct timeval)totalGCSystemCPU).tv_sec, ((struct timeval)totalGCSystemCPU).tv_usec, 1000000);
#endif
}

void HeapSizeParameters::Init()
{
#if (defined(_WIN32))
    // Record an initial time of day to use as the basis of real timing
    FILETIME s;
    GetSystemTimeAsFileTime(&s);
#else
    struct timeval s;
    gettimeofday(&s, NULL);
#endif
    startTime = s;  // Overall start time
    startRTime = startTime; // Start of this non-gc phase

    resetMajorTimingData();
#if (defined(_WIN32))
    startPF = GetPaging(0);
#else
    startPF = GetPaging(0);
#endif
}

void HeapSizeParameters::Final()
{
    // Print the overall statistics
    if (debugOptions & (DEBUG_GC|DEBUG_HEAPSIZE))
    {
        TIMEDATA userTime, systemTime, realTime;
#if (defined(_WIN32))
        FILETIME kt, ut;
        FILETIME ct, et; // Unused
        FILETIME rt;
        GetProcessTimes(GetCurrentProcess(), &ct, &et, &kt, &ut);
        GetSystemTimeAsFileTime(&rt);
        userTime.add(ut);
        systemTime.add(kt);
        realTime.add(rt);
 #else
        struct rusage rusage;
        struct timeval tv;
        if (getrusage(RUSAGE_SELF, &rusage) != 0 || gettimeofday(&tv, NULL) != 0)
            return;
        userTime.add(rusage.ru_utime);
        systemTime.add(rusage.ru_stime);
        realTime.add(tv);
#endif
        realTime.sub(startTime);
        userTime.sub(totalGCUserCPU);
        systemTime.sub(totalGCSystemCPU);
        realTime.sub(totalGCReal);
        if (debugOptions & DEBUG_GC)
        {
            Log("GC (Total): Non-GC time: CPU user: %0.3f system: %0.3f real: %0.3f\n",
                userTime.toSeconds(), systemTime.toSeconds(), realTime.toSeconds());
            Log("GC (Total): GC time: CPU user: %0.3f system: %0.3f real: %0.3f\n",
                totalGCUserCPU.toSeconds(), totalGCSystemCPU.toSeconds(), totalGCReal.toSeconds());
        }
        if (debugOptions & DEBUG_HEAPSIZE)
        {
            TIMEDATA gc, nonGc;
            gc.add(totalGCUserCPU);
            gc.add(totalGCSystemCPU);
            nonGc.add(userTime);
            nonGc.add(systemTime);
            Log("Heap: Total CPU GC time %0.3fsecs,  Non-GC %0.3fsecs, ratio %0.3f\n",
                gc.toSeconds(), nonGc.toSeconds(), gc.toSeconds() / nonGc.toSeconds());
        }
    }
}


void HeapSizeParameters::resetMinorTimingData(void)
{
    minorNonGCUserCPU.fromSeconds(0);
    minorNonGCSystemCPU.fromSeconds(0);
    minorNonGCReal.fromSeconds(0);
    minorGCUserCPU.fromSeconds(0);
    minorGCSystemCPU.fromSeconds(0);
    minorGCReal.fromSeconds(0);
    minorGCPageFaults = 0;
}

void HeapSizeParameters::resetMajorTimingData(void)
{
    resetMinorTimingData();
    majorNonGCUserCPU.fromSeconds(0);
    majorNonGCSystemCPU.fromSeconds(0);
    majorNonGCReal.fromSeconds(0);
    majorGCUserCPU.fromSeconds(0);
    majorGCSystemCPU.fromSeconds(0);
    majorGCReal.fromSeconds(0);
    majorGCPageFaults = 0;
    minorGCsSinceMajor = 0;
}


class HeapSizing: public RtsModule
{
public:
    virtual void Init(void);
    virtual void Stop(void);
};

// Declare this.  It will be automatically added to the table.
static HeapSizing heapSizeModule;

void HeapSizing::Init(void)
{
    gHeapSizeParameters.Init();
}

void HeapSizing::Stop()
{
    gHeapSizeParameters.Final();
}

static size_t GetPhysicalMemorySize(void)
{
    size_t maxMem = (size_t)0-1; // Maximum unsigned value.
#if defined(HAVE_WINDOWS_H) // Windows including Cygwin
    {
        MEMORYSTATUSEX memStatEx;
        memset(&memStatEx, 0, sizeof(memStatEx));
        memStatEx.dwLength = sizeof(memStatEx);
        if (! GlobalMemoryStatusEx(&memStatEx))
            memStatEx.ullTotalPhys = 0; // Clobber any rubbish since it says it failed.
        if (memStatEx.ullTotalPhys) // If it's non-zero assume it succeeded
        {
            DWORDLONG dwlMax = maxMem;
            if (memStatEx.ullTotalPhys > dwlMax)
                return maxMem;
            else
                return (size_t)memStatEx.ullTotalPhys;
        }
    }

#endif
#if defined(_SC_PHYS_PAGES) && defined(_SC_PAGESIZE)
    {
        // Linux and Solaris.  This gives a silly value in Cygwin.
        long physPages      = sysconf(_SC_PHYS_PAGES);
        long physPagesize   = sysconf(_SC_PAGESIZE);
        if (physPages != -1 && physPagesize != -1)
        {
            unsigned long maxPages = maxMem / physPagesize;
            if ((unsigned long)physPages > maxPages)
                return maxMem;
            else // We've checked it won't overflow.
                return physPages*physPagesize;
        }
    }
#endif
#if defined(HAVE_SYSCTL) && defined(CTL_HW)
    // FreeBSD and Mac OS X.  It seems HW_MEMSIZE has been added to
    // Max OS X to return a 64-bit value.
#ifdef HW_MEMSIZE
    {
        static int mib[2] = { CTL_HW, HW_MEMSIZE };
        uint64_t physMem = 0;
        size_t len = sizeof(physMem);
        if (sysctl(mib, 2, &physMem, &len, NULL, 0) == 0 && len == sizeof(physMem))
        {
            if (physMem > (uint64_t)maxMem)
                return maxMem;
            else
                return (size_t)physMem;
        }
    }
#endif
#ifdef HW_PHYSMEM
    // If HW_MEMSIZE isn't there or the call failed try this.
    {
        static int mib[2] = { CTL_HW, HW_PHYSMEM };
        unsigned int physMem = 0;
        size_t len = sizeof(physMem);
        if (sysctl(mib, 2, &physMem, &len, NULL, 0) == 0 && len == sizeof(physMem))
        {
            if (physMem > maxMem)
                return maxMem;
            else
                return physMem;
        }
    }
#endif
#endif
    return 0; // Unable to determine
}

