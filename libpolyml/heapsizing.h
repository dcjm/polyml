/*
    Title:  heapsizing.h - parameters to adjust heap size

    Copyright (c) Copyright David C.J. Matthews 2012

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

#ifndef HEAPSIZING_H_INCLUDED
#define HEAPSIZING_H_INCLUDED 1

#include "timing.h"

class LocalMemSpace;

class HeapSizeParameters {
public:
    HeapSizeParameters();

    // Extract timing information for ML.
    Handle getGCUtime(TaskData *taskData) const;
    Handle getGCStime(TaskData *taskData) const;

    void SetHeapParameters(uintptr_t minsize, uintptr_t maxsize, uintptr_t initialsize, unsigned percent, bool gcsh);

    void SetReservation(uintptr_t rsize);

    // Called in the minor GC if a GC thread needs to grow the heap.
    // Returns zero if the heap cannot be grown.
    LocalMemSpace *AddSpaceInMinorGC(uintptr_t space, bool isMutable);

    // Called in the major GC before the copy phase if the heap is more than
    // 90% full.  This should improve the efficiency of copying.
    LocalMemSpace *AddSpaceBeforeCopyPhase(bool isMutable);

    bool PerformSharingPass() const { return performSharingPass; }
    void AdjustSizeAfterMajorGC(uintptr_t wordsRequired);
    bool AdjustSizeAfterMinorGC(uintptr_t spaceAfterGC, uintptr_t spaceBeforeGC);

    // Returns true if we should run a major GC at this point
    bool RunMajorGCImmediately();

    /* Called by the garbage collector at the beginning and
       end of garbage collection. */
    typedef enum __gcTime {
        GCTimeStart,
        GCTimeIntermediate,
        GCTimeEnd
    } gcTime;

    // These are called by the GC to record information about its progress.
    void RecordAtStartOfMajorGC();
    void RecordGCTime(gcTime isEnd, const char *stage = "");
    void RecordSharingData(POLYUNSIGNED recovery);
    
    void resetMinorTimingData(void);
    void resetMajorTimingData(void);

    void Init(void);
    void Final(void);

private:
    // Estimate the GC cost for a given heap size.  The result is the ratio of
    // GC time to application time.
    double costFunction(uintptr_t heapSize, bool withSharing, bool withSharingCost);

    bool getCostAndSize(uintptr_t &heapSize, uintptr_t wordsRequired, double &cost, bool withSharing);

    // Set if we should do a full GC next time instead of a minor GC.
    bool fullGCNextTime;

    // Whether to ever allow the GC sharing pass
    bool allowGCSharing;
    // Whether a sharing pass should be performed on the next GC
    bool performSharingPass;
    // The proportion of the total heap recovered by the sharing pass
    double sharingRecoveryRate;
    // The cost of doing the sharing as a proportion of the rest of the GC.
    double sharingCostFactor;
    // The actual number of words recovered in the last sharing pass
    POLYUNSIGNED sharingWordsRecovered;
    // The saving we would have made by enabling sharing in the past
    double cumulativeSharingSaving;

    // Maximum and minimum heap size as given by the user.
    uintptr_t minHeapSize, maxHeapSize;

    // Target GC cost requested by the user.
    double userGCRatio;
    // Actual ratio for the last major GC
    double lastMajorGCRatio;
    // Predicted ratio for the next GC
    double predictedRatio;

    uintptr_t lastFreeSpace, currentSpaceUsed;
    // Set to false if an allocation failed.  Indicates that
    // we may have reached some virtual memory limit.
    bool lastAllocationSucceeded;
    // Set to true if the last major GC may have hit the limit
    bool allocationFailedBeforeLastMajorGC;

    // The estimated boundary where the paging will become
    // a significant factor.
    uintptr_t pagingLimitSize;

    // The maximum size the heap has reached so far. 
    uintptr_t highWaterMark;

    // The heap size at the start of the current GC before any spaces have been deleted.
    uintptr_t heapSizeAtStart;

    // The start of the clock.
    TIMEDATA startTime;

    // Timing for the last minor or major GC
    TIMEDATA minorNonGCUserCPU;
    TIMEDATA minorNonGCSystemCPU;
    TIMEDATA minorNonGCReal;
    TIMEDATA minorGCUserCPU;
    TIMEDATA minorGCSystemCPU;
    TIMEDATA minorGCReal;
    long minorGCPageFaults;
    unsigned minorGCsSinceMajor;

    // Timing for all the minor GCs and the last major GC.
    // Reset after each major GC.
    TIMEDATA majorNonGCUserCPU;
    TIMEDATA majorNonGCSystemCPU;
    TIMEDATA majorNonGCReal;
    TIMEDATA majorGCUserCPU;
    TIMEDATA majorGCSystemCPU;
    TIMEDATA majorGCReal;
    long majorGCPageFaults;

    // Totals for all GCs
    TIMEDATA totalGCUserCPU;
    TIMEDATA totalGCSystemCPU;
    TIMEDATA totalGCReal;

    // The cost for the last sharing pass
    TIMEDATA sharingCPU;

    TIMEDATA startUsageU, startUsageS, lastUsageU, lastUsageS;
    TIMEDATA startRTime, lastRTime;
    long startPF;
};

extern HeapSizeParameters gHeapSizeParameters;
#endif
