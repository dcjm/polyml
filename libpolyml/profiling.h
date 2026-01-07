/*
    Title:  profiling.h

    Copyright (c) 2000
        Cambridge University Technical Services Limited
    Further development copyright (c) David C.J. Matthews 2011, 2-15

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

#ifndef _PROFILING_H_DEFINED
#define _PROFILING_H_DEFINED 1

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class TaskData;

// Current profiling mode
typedef enum {
    kProfileOff = 0,
    kProfileTime,
    kProfileStoreAllocation,
    kProfileEmulation,   // No longer used
    kProfileLiveData,
    kProfileLiveMutables,
    kProfileTimeThread,
    kProfileMutexContention
} ProfileMode;

extern ProfileMode profileMode;

#include "processes.h" // For SIGNALCONTEXT

// Handle a SIGVTALRM or the simulated equivalent in Windows.
extern void handleProfileTrap(TaskData *taskData, SIGNALCONTEXT *context);
// Add count.  Must not be called from a signal handler.
extern void addSynchronousCount(POLYCODEPTR pc, POLYUNSIGNED incr);
// Add one to the timing counter.  May occur at any time.
extern void incrementCountAsynch(POLYCODEPTR pc);
// Process the queue of profile pc values if we're time profiling.
// Only called by the main thread.
extern void processProfileQueue();

extern void AddObjectProfile(PolyObject *obj);

extern struct _entrypts profilingEPT[];

#endif /* _PROFILING_H_DEFINED */
