/*
    Title:      Validate addresses in objects.

    Copyright (c) 2006, 2012
        David C.J. Matthews

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


#include "globals.h"
#include "diagnostics.h"
#include "machine_dep.h"
#include "scanaddrs.h"
#include "memmgr.h"

#define INRANGE(val,start,end)\
  (start <= val && val < end)

static void CheckAddress(PolyWord *pt)
{
    MemSpace *space = gMem.SpaceForAddress(pt-1);
    if (space == 0)
    {
        Log("Check: Bad pointer %p (no space found)\n", pt);
        ASSERT(space != 0);
    }
    if (space->spaceType == ST_STACK) // This may not have valid length words.
        return;
    PolyObject *obj = (PolyObject*)pt;
    POLYUNSIGNED length = obj->Length();
    if (pt+length > space->top)
    {
        Log("Check: Bad pointer %p (space %p) length %" POLYUFMT "\n", pt, space, length);
        ASSERT(pt+length <= space->top);
    }
    if (space->spaceType == ST_LOCAL)
    {
        LocalMemSpace *lSpace = (LocalMemSpace*)space;
        if (!((pt > lSpace->bottom && pt+length <= lSpace->lowerAllocPtr) ||
            (pt > lSpace->upperAllocPtr && pt+length <= space->top)))
        {
            Log("Check: Bad pointer %p (space %p) length %" POLYUFMT " outside allocated area\n", pt, space, length);
            ASSERT((pt > lSpace->bottom && pt+length <= lSpace->lowerAllocPtr) ||
                (pt > lSpace->upperAllocPtr && pt+length <= space->top));
        }
    }
}

void DoCheck (const PolyWord pt)
{
    if (pt == PolyWord::FromUnsigned(0)) return;

    if (pt.IsTagged()) return;

    // It's possible that this could be a code address.
    // That can happen, at least when pushing a value onto the save vec
    // in a call to set_code_constant on X86/64
    if (pt.IsCodePtr())
        CheckAddress((PolyWord*)ObjCodePtrToPtr(pt.AsCodePtr()));
    else CheckAddress(pt.AsStackAddr());
} 

class ScanCheckAddress: public ScanAddress
{
public:
    virtual PolyObject *ScanObjectAddress(PolyObject *pt) { DoCheck(pt); return pt; }
};

void DoCheckObject (const PolyObject *base, POLYUNSIGNED L)
{

    PolyWord *pt  = (PolyWord*)base;
    CheckAddress(pt);
    MemSpace *space = gMem.SpaceForAddress(pt-1);
    if (space == 0)
        Crash ("Bad pointer 0x%08x found", pt);

    ASSERT (OBJ_IS_LENGTH(L));

    POLYUNSIGNED n   = OBJ_OBJECT_LENGTH(L);
    if (n == 0) return;

    ASSERT (n > 0);
    ASSERT(pt-1 >= space->bottom && pt+n <= space->top);

    byte flags = GetTypeBits(L);  /* discards GC flag and mutable bit */

    if (flags == F_BYTE_OBJ) /* possibly signed byte object */
        return; /* Nothing more to do */

    if (flags == F_CODE_OBJ) /* code object */
    {
        ScanCheckAddress checkAddr;
        /* We flush the instruction cache here in case we change any of the
          instructions when we update addresses. */
        machineDependent->FlushInstructionCache(pt, (n + 1) * sizeof(PolyWord));
        machineDependent->ScanConstantsWithinCode((PolyObject *)base, (PolyObject *)base, n, &checkAddr);
        /* Skip to the constants. */
        base->GetConstSegmentForCode(n, pt, n);
    }
    else ASSERT (flags == 0); /* ordinary word object */

    while (n--) DoCheck (*pt++);
}

void DoCheckPointer (const PolyWord pt)
{
    if (pt == PolyWord::FromUnsigned(0)) return;

    if (OBJ_IS_AN_INTEGER(pt)) return;

    DoCheck (pt);

    if (pt.IsDataPtr())
    {
        PolyObject *obj = pt.AsObjPtr();
        DoCheckObject (obj, obj->LengthWord());
    }
}

// Check all the objects in the memory.  Used to check the garbage collector
//
void DoCheckMemory()
{
    ScanCheckAddress memCheck;
    // Scan the local areas.
    for (unsigned i = 0; i < gMem.nlSpaces; i++)
    {
        LocalMemSpace *space = gMem.lSpaces[i];
        memCheck.ScanAddressesInRegion(space->bottom, space->lowerAllocPtr);
        memCheck.ScanAddressesInRegion(space->upperAllocPtr, space->top);
    }
    // Scan the permanent mutable areas.
    for (unsigned j = 0; j < gMem.npSpaces; j++)
    {
        PermanentMemSpace *space = gMem.pSpaces[j];
        if (space->isMutable && ! space->byteOnly)
            memCheck.ScanAddressesInRegion(space->bottom, space->top);
    }
}
