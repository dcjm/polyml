/*
    Title:      Multi-Threaded Garbage Collector - Check for weak references

    Copyright (c) 2010, 2012, 2015 David C. J. Matthews

    Based on the original garbage collector code
        Copyright 2000-2008
        Cambridge University Technical Services Limited

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
This is an intermediate phase in the GC that checks for weak references
that are no longer reachable.  It is performed after the first, mark, phase. 
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
#include "gc.h"
#include "scanaddrs.h"
#include "rts_module.h"
#include "memmgr.h"

class MTGCCheckWeakRef: public ScanAddress {
public:
    void ScanAreas(void);
private:
    virtual void ScanRuntimeAddress(PolyObject **pt, RtsStrength weak);
    // This has to be defined since it's virtual.
    virtual PolyObject *ScanObjectAddress(PolyObject *base) { return base; }
    virtual void ScanAddressesInObject(PolyObject *obj, POLYUNSIGNED lengthWord);
};

// This deals with weak references within the run-time system.
void MTGCCheckWeakRef::ScanRuntimeAddress(PolyObject **pt, RtsStrength weak)
{
    /* If the object has not been marked and this is only a weak reference */
    /* then the pointer is set to zero. This allows streams or windows     */
    /* to be closed if there is no other reference to them.                */
    
    PolyObject *val = *pt;
    PolyWord w = val;
    
    if (weak == STRENGTH_STRONG)
        return;

    LocalMemSpace *space = gMem.LocalSpaceForAddress(w.AsStackAddr()-1);
    if (space == 0)
        return; // Not in local area
    // If it hasn't been marked set it to zero.
    if (! space->bitmap.TestBit(space->wordNo(w.AsStackAddr())))
         *pt = 0;
}

// Deal with weak objects
void MTGCCheckWeakRef::ScanAddressesInObject(PolyObject *obj, POLYUNSIGNED L)
{
    if (! OBJ_IS_WEAKREF_OBJECT(L) || OBJ_IS_BYTE_OBJECT(L)) return; // Ignore Weak-Byte cells.
    ASSERT(OBJ_IS_MUTABLE_OBJECT(L)); // Should be a mutable.
    // See if any of the SOME objects contain unreferenced refs.
    POLYUNSIGNED length = OBJ_OBJECT_LENGTH(L);
    PolyWord *baseAddr = (PolyWord*)obj;
    for (POLYUNSIGNED i = 0; i < length; i++)
    {
        PolyWord someAddr = baseAddr[i];
        if (someAddr.IsDataPtr())
        {
            LocalMemSpace *someSpace = gMem.LocalSpaceForAddress(someAddr.AsStackAddr()-1);
            if (someSpace != 0)
            {
                PolyObject *someObj = someAddr.AsObjPtr();
                // If this is a weak object the SOME value may refer to an unreferenced
                // ref.  If so we have to set this entry to NONE.  For safety we also
                // set the contents of the SOME to TAGGED(0).
                ASSERT(someObj->Length() == 1 && someObj->IsWordObject()); // Should be a SOME node.
                PolyWord refAddress = someObj->Get(0);
                bool deleteRef = false;
                if (refAddress.IsTagged())
                    // If we have the same SOME cell referenced in two different places
                    // we will have overwritten the address with TAGGED(0) "For safety".
                    // We still need to overwrite the new reference to the SOME cell.
                    deleteRef = true; // We've overwritten it.
                else
                {
                    // Usual case: the contents of the SOME cell is the address of a ref.
                    LocalMemSpace *space = gMem.LocalSpaceForAddress(refAddress.AsStackAddr());
                    if (space != 0) // If the ref is permanent it's always there.
                    {
                        POLYUNSIGNED new_bitno = space->wordNo(refAddress.AsStackAddr());
                        // It wasn't marked so it's otherwise unreferenced.
                        if (! space->bitmap.TestBit(new_bitno)) deleteRef = true;
                    }
                }
                if (deleteRef)
                {
                        
                    baseAddr[i] = TAGGED(0); // Set it to NONE.
                    someObj->Set(0, TAGGED(0)); // For safety.
                    convertedWeak = true;
                }
            }
        }
    }
}

// We need to check any weak references both in the areas we are
// currently collecting and any other areas.  This actually checks
// weak refs in the area we're collecting even if they are not
// actually reachable any more.  N.B.  This differs from OpMutables
// because it also scans the area we're collecting.
void MTGCCheckWeakRef::ScanAreas(void)
{
    for (std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
    {
        LocalMemSpace *space = *i;
        if (space->isMutable)
            ScanAddressesInRegion(space->lowestWeak, space->highestWeak);
    }
    // Scan the permanent mutable areas.
    for (std::vector<PermanentMemSpace*>::iterator i = gMem.pSpaces.begin(); i < gMem.pSpaces.end(); i++)
    {
        MemSpace *space = *i;
        if (space->isMutable)
            ScanAddressesInRegion(space->lowestWeak, space->highestWeak);
    }
}


void GCheckWeakRefs()
{
    MTGCCheckWeakRef checkRef;
    GCModules(&checkRef);
    checkRef.ScanAreas();
}
