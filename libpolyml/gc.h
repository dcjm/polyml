/*
    Title:  gc.h - exports signature for gc.cpp

    Copyright (c) 2000-7
        Cambridge University Technical Services Limited

    Further development Copyright David C.J. Matthews 2010, 2019, 2020

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

#ifndef GC_H_INCLUDED
#define GC_H_INCLUDED

#include "globals.h" // For POLYUNSIGNED

class TaskData;

// Make a request for a full garbage collection.
extern void FullGC(TaskData *taskData);
// Make a request for a partial garbage collection.
extern bool QuickGC(TaskData *taskData, POLYUNSIGNED words_needed);
extern void CreateHeap();

extern void FullGCForShareCommonData(void);

extern bool convertedWeak;

// Multi-thread GC.
extern void initialiseMarkerTables();

// The task farm for the GC.  The threads are left waiting for the GC,
class GCTaskFarm;
extern GCTaskFarm *gpTaskFarm;

extern void CopyObjectToNewAddress(PolyObject *srcAddress, PolyObject *destAddress, POLYUNSIGNED L);

extern bool RunQuickGC(const POLYUNSIGNED wordsRequiredToAllocate);

// GC Phases.
extern void GCSharingPhase(void);
extern void GCMarkPhase(void);
extern void GCheckWeakRefs(void);
extern void GCCopyPhase(void);
extern void GCUpdatePhase(void);

#endif
