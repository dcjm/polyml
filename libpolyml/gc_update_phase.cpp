/*
    Title:      Multi-Threaded Garbage Collector - Update phase

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
This is the third, update, phase of the garbage collector.  The previous, copy,
phase will have moved cells in memory.  The update phase goes through all cells
that could contain an address of a cell that has been moved and looks for a
tomb-stone that contains its new location. 
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

#include "globals.h"
#include "run_time.h"
#include "processes.h"
#include "gc.h"
#include "scanaddrs.h"
#include "check_objects.h"
#include "bitmap.h"
#include "memmgr.h"
#include "gctaskfarm.h"
#include "diagnostics.h"

class MTGCProcessUpdate: public ScanAddress
{
public:
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt);
    virtual void ScanRuntimeAddress(PolyObject **pt, RtsStrength weak);
    virtual PolyObject *ScanObjectAddress(PolyObject *base);

    void UpdateObjectsInArea(LocalMemSpace *area);

private:
    static void UpdateAddress(PolyObject *&obj)
    {
        while (obj->ContainsForwardingPtr())
            obj = obj->GetForwardingPtr();
    }
};

/*********************************************************************/
/* This function is called in the update phase to update pointers to */
/* objects in the gc area that are in old mutable segments.          */
/*********************************************************************/
PolyObject *MTGCProcessUpdate::ScanObjectAddress(PolyObject *obj)
{
    LocalMemSpace *space = gMem.LocalSpaceForObjectAddress(obj);
    if (space != 0)
    {
        UpdateAddress(obj);
        ASSERT(obj->ContainsNormalLengthWord());
    }
    return obj;
}

void MTGCProcessUpdate::ScanRuntimeAddress(PolyObject **pt, RtsStrength/* weak*/)
/* weak is not used, but needed so type of the function is correct */
{
    PolyObject *obj = *pt;
    if (obj->ContainsForwardingPtr())
    {
        UpdateAddress(obj);
        *pt = obj;
    }
}  

// Update the addresses in a group of words.
POLYUNSIGNED MTGCProcessUpdate::ScanAddressAt(PolyWord *pt)
{
    PolyWord val = *pt;

    if (val.IsTagged())
        return 0;

    // It looked like it would be possible to simplify this code and
    // just call UpdateAddress on any address.  It seems to be
    // better to avoid unnecessary writes so we only store into
    // *pt if it has actually changed.

    PolyObject *obj = val.AsObjPtr();
    if (obj->ContainsForwardingPtr())
    {
        UpdateAddress(obj);
        *pt = obj;
    }
    return 0;
}

// Updates the addresses for objects in the area with the "allocated" bit set.
// It processes the area between area->pointer and the bit corresponding to area->highest.
// area->highest corresponds to gen_top i.e. we don't process older generations.
void MTGCProcessUpdate::UpdateObjectsInArea(LocalMemSpace *area)
{
    PolyWord *pt      = area->upperAllocPtr;
    uintptr_t   bitno   = area->wordNo(pt);
    uintptr_t   highest = area->wordNo(area->top);

    for (;;)
    {
        ASSERT(bitno <= highest);
        /* Zero unused words.  This is necessary so that
           ScanAddressesInRegion can work.  It requires the allocated
           area of memory to contain either objects with a valid length
           word or forwarding pointer or zeros.  We should only be
           zeroing words that we couldn't fill with real data so it
           shouldn't be too much.  Profiling showed that using dummy
           byte objects here didn't make a measurable difference,
        */
        while (bitno < highest && !area->bitmap.TestBit(bitno))
        {
            *pt++ = PolyWord::FromUnsigned(0);
            bitno++;
        }
        
        if (bitno == highest) {
            // Have reached the top of the area
            ASSERT(pt == area->top);
            break;
        }
        
        /* first set bit corresponds to the length word */
        pt++;
        PolyObject *obj = (PolyObject*)pt;
        POLYUNSIGNED L = obj->LengthWord();
        bitno++;
        
        if (obj->ContainsForwardingPtr())
        {
            // Skip over moved objects.  We have to find the new location to find
            // its length.
            UpdateAddress(obj);            
            POLYUNSIGNED length = obj->Length();
            pt    += length;
            bitno += length;
        }
        else // Contains real object
        {
            
            if (OBJ_IS_WORD_OBJECT(L))
            {
                POLYUNSIGNED length = OBJ_OBJECT_LENGTH(L);
                
                area->updated += length+1;
                
                while (length--)
                {
                    PolyWord val = *pt;

                    if (! val.IsTagged() && val != PolyWord::FromUnsigned(0))
                    {
                        PolyObject *obj = val.AsObjPtr();
                    
                        if (obj->ContainsForwardingPtr())
                        {
                            UpdateAddress(obj);
                            *pt = obj;
                        }
                    }
                    
                    pt++;
                    bitno++;
                }
            }
            
            else /* !OBJ_IS_WORD_OBJECT(L) */
            {
                POLYUNSIGNED length = OBJ_OBJECT_LENGTH(L);
                area->updated += length+1;
                ScanAddressesInObject(obj, L);
                pt    += length;
                bitno += length;
            } /* !OBJ_IS_WORD_OBJECT(L) */

            CheckObject(obj); // Can check it after it's been updated
        }  /* !OBJ_IS_POINTER(L) */
    } /* for loop */
}

// Task to update addresses in a local area.
static void updateLocalArea(GCTaskId*, void *arg1, void *arg2)
{
    MTGCProcessUpdate *processUpdate = (MTGCProcessUpdate *)arg1;
    LocalMemSpace *space = (LocalMemSpace *)arg2;
    if (debugOptions & DEBUG_GC_ENHANCED)
        Log("GC: Update local area %p\n", space);
    // Process the current generation for mutable or immutable areas.
    processUpdate->UpdateObjectsInArea(space);
    if (debugOptions & DEBUG_GC_ENHANCED)
        Log("GC: Completed local update for %p. %lu words updated\n", space, space->updated);
}

// Task to update addresses in a non-local area.
static void updateNonLocalMutableArea(GCTaskId*, void *arg1, void *arg2)
{
    MTGCProcessUpdate *processUpdate = (MTGCProcessUpdate *)arg1;
    MemSpace *space = (MemSpace *)arg2;
    if (debugOptions & DEBUG_GC_ENHANCED)
        Log("GC: Update non-local mutable area %p\n", space);
    processUpdate->ScanAddressesInRegion(space->bottom, space->top);
    if (debugOptions & DEBUG_GC_ENHANCED)
        Log("GC: Completed non-local mutable update for %p\n", space);
}

// Task to update addresses maintained by the RTS itself.
static void updateGCProcAddresses(GCTaskId*, void *arg1, void *)
{
    MTGCProcessUpdate *processUpdate = (MTGCProcessUpdate *)arg1;
    GCModules(processUpdate);
}

void GCUpdatePhase()
{
    /* Update phase */
    mainThreadPhase = MTP_GCPHASEUPDATE;
    
    /* Invariant: at most the first (gen_top - bottom) bits of each bitmap can be dirty here. */
    for(std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
        (*i)->updated = 0;

    // We can do the updates in parallel since they don't interfere at all.
    MTGCProcessUpdate processUpdate;

    // Process local areas.
    for (std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
    {
        LocalMemSpace *space = *i;
        // As well as updating the addresses this also clears the bitmaps.
        gpTaskFarm->AddWorkOrRunNow(&updateLocalArea, &processUpdate, space);
    }
    // Scan the permanent mutable areas and the code areas.
    for (std::vector<PermanentMemSpace*>::iterator i = gMem.pSpaces.begin(); i < gMem.pSpaces.end(); i++)
    {
        PermanentMemSpace *space = *i;
        if (space->isMutable && ! space->byteOnly)
            gpTaskFarm->AddWorkOrRunNow(&updateNonLocalMutableArea, &processUpdate, space);
    }
    for (std::vector<CodeSpace *>::iterator i = gMem.cSpaces.begin(); i < gMem.cSpaces.end(); i++)
    {
        CodeSpace *space = *i;
        gpTaskFarm->AddWorkOrRunNow(&updateNonLocalMutableArea, &processUpdate, space);
        // We could remove the mutable bit if there are no longer any mutable code objects
        // but it's easier to leave that to the minor GC.
    }

    // Update addresses in RTS modules.
    gpTaskFarm->AddWorkOrRunNow(&updateGCProcAddresses, &processUpdate, 0);
    // Wait for these to complete before proceeding.
    gpTaskFarm->WaitForCompletion();
}
