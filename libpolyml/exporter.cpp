/*
    Title:  exporter.cpp - Export a function as an object or C file

    Copyright (c) 2006-7, 2015, 2016-21, 2025 David C.J. Matthews

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
#define ASSERT(x) assert(x)

#else
#define ASSERT(x)
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#if (defined(_WIN32))
#include <tchar.h>
#else
#define _T(x) x
#define _tcslen strlen
#define _tcscmp strcmp
#define _tcscat strcat
#endif

#include "exporter.h"
#include "save_vec.h"
#include "polystring.h"
#include "run_time.h"
#include "osmem.h"
#include "scanaddrs.h"
#include "gc.h"
#include "machine_dep.h"
#include "diagnostics.h"
#include "memmgr.h"
#include "processes.h" // For IO_SPACING
#include "sys.h" // For EXC_Fail
#include "rtsentry.h"
#include "timing.h" // For getBuildTime

#include "pexport.h"

#include "../polyexports.h" // For MT_FLAGS

#ifdef HAVE_PECOFF
#include "pecoffexport.h"
#elif defined(HAVE_ELF_H) || defined(HAVE_ELF_ABI_H)
#include "elfexport.h"
#elif defined(HAVE_MACH_O_RELOC_H)
#include "machoexport.h"
#endif

#if (defined(_WIN32))
#define NOMEMORY ERROR_NOT_ENOUGH_MEMORY
#define ERRORNUMBER _doserrno
#else
#define NOMEMORY ENOMEM
#define ERRORNUMBER errno
#endif

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyExport(POLYUNSIGNED threadId, POLYUNSIGNED fileName, POLYUNSIGNED root);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyExportPortable(POLYUNSIGNED threadId, POLYUNSIGNED fileName, POLYUNSIGNED root);
}

/*
To export the function and everything reachable from it we need to copy
all the objects into a new area.  We leave tombstones in the original
objects by overwriting the length word.  That prevents us from copying an
object twice and breaks loops.  Once we've copied the objects we then
have to go back over the memory and turn the tombstones back into length
words.
*/

GraveYard::~GraveYard()
{
    free(graves);
}

// Used to calculate the space required for the ordinary mutables
// and the no-overwrite mutables.  They are interspersed in local space.
class MutSizes : public ScanAddress
{
public:
    MutSizes() : mutSize(0), noOverSize(0) {}

    virtual PolyObject *ScanObjectAddress(PolyObject *base) { return base; }// No Actually used

    virtual void ScanAddressesInObject(PolyObject *base, POLYUNSIGNED lengthWord)
    {
        const POLYUNSIGNED words = OBJ_OBJECT_LENGTH(lengthWord) + 1; // Include length word
        if (OBJ_IS_NO_OVERWRITE(lengthWord))
            noOverSize += words;
        else mutSize += words;
    }

    POLYUNSIGNED mutSize, noOverSize;
};

CopyScan::CopyScan(bool isExp /*=false*/)
{
    isExport = isExp;
    defaultImmSize = defaultMutSize = defaultCodeSize = defaultNoOverSize = 0;
    tombs = 0;
    graveYard = 0;
    hash_a = hash_b = hash_c = 0xdeadbeef;

    // Add extra randomness to the hash values.  This is still reproducible if SOURCE_DATE_EPOCH is set.
    hash_a += (uint32_t)getBuildTime();
    hash_b += sequenceNo++;

    hash_posn = 0;
}

uint32_t CopyScan::sequenceNo = 0;

void CopyScan::initialise()
{
    ASSERT(gMem.eSpaces.size() == 0);
    // Set the space sizes to a proportion of the space currently in use.
    // Computing these sizes is not obvious because CopyScan is used both
    // for export and for saved states.  For saved states in particular we
    // want to use a smaller size because they are retained after we save
    // the state and if we have many child saved states it's important not
    // to waste memory.
    if (isExport)
    {
        graveYard = new GraveYard[gMem.pSpaces.size()];
        if (graveYard == 0)
        {
            if (debugOptions & DEBUG_SAVING)
                Log("SAVE: Unable to allocate graveyard, size: %lu.\n", gMem.pSpaces.size());
            throw MemoryException();
        }
    }

    for (std::vector<PermanentMemSpace*>::iterator i = gMem.pSpaces.begin(); i < gMem.pSpaces.end(); i++)
    {
        PermanentMemSpace *space = *i;
        if (! dependencies[space->moduleIdentifier]) {
            // Include this if we're exporting (hierarchy=0) or if we're saving a state
            // and will include this in the new state.
            size_t size = (space->top-space->bottom)/4;
            if (space->noOverwrite)
                defaultNoOverSize += size;
            else if (space->isMutable)
                defaultMutSize += size;
            else if (space->isCode)
                defaultCodeSize += size;
            else
                defaultImmSize += size;
            if (space->isWriteProtected)
            {
                // We need a separate area for the tombstones because this is read-only
                graveYard[tombs].graves = (PolyWord*)calloc(space->spaceSize(), sizeof(PolyWord));
                if (graveYard[tombs].graves == 0)
                {
                    if (debugOptions & DEBUG_SAVING)
                        Log("SAVE: Unable to allocate graveyard for permanent space, size: %lu.\n",
                            space->spaceSize() * sizeof(PolyWord));
                    throw MemoryException();
                }
                if (debugOptions & DEBUG_SAVING)
                    Log("SAVE: Allocated graveyard for permanent space, %p size: %lu.\n",
                        graveYard[tombs].graves, space->spaceSize() * sizeof(PolyWord));
                graveYard[tombs].startAddr = space->bottom;
                graveYard[tombs].endAddr = space->top;
                tombs++;
            }
        }
    }
    for (std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
    {
        LocalMemSpace *space = *i;
        uintptr_t size = space->allocatedSpace();
        // It looks as though the mutable size generally gets
        // overestimated while the immutable size is correct.
        if (space->isMutable)
        {
            MutSizes sizeMut;
            sizeMut.ScanAddressesInRegion(space->bottom, space->lowerAllocPtr);
            sizeMut.ScanAddressesInRegion(space->upperAllocPtr, space->top);
            defaultNoOverSize += sizeMut.noOverSize / 4;
            defaultMutSize += sizeMut.mutSize / 4;
        }
        else
            defaultImmSize += size/2;
    }
    for (std::vector<CodeSpace *>::iterator i = gMem.cSpaces.begin(); i < gMem.cSpaces.end(); i++)
    {
        CodeSpace *space = *i;
        uintptr_t size = space->spaceSize();
        defaultCodeSize += size/2;
    }
    if (isExport)
    {
        // Minimum 1M words.
        if (defaultMutSize < 1024*1024) defaultMutSize = 1024*1024;
        if (defaultImmSize < 1024*1024) defaultImmSize = 1024*1024;
        if (defaultCodeSize < 1024*1024) defaultCodeSize = 1024*1024;
#ifdef MACOSX
        // Limit the segment size for Mac OS X.  The linker has a limit of 2^24 relocations
        // in a segment so this is a crude way of ensuring the limit isn't exceeded.
        // It's unlikely to be exceeded by the code itself.
        // Actually, from trial-and-error, the limit seems to be around 6M.
        if (defaultMutSize > 6 * 1024 * 1024) defaultMutSize = 6 * 1024 * 1024;
        if (defaultImmSize > 6 * 1024 * 1024) defaultImmSize = 6 * 1024 * 1024;
#endif
        if (defaultNoOverSize < 4096) defaultNoOverSize = 4096; // Except for the no-overwrite area
    }
    else
    {
        // Much smaller minimum sizes for saved states.
        if (defaultMutSize < 1024) defaultMutSize = 1024;
        if (defaultImmSize < 4096) defaultImmSize = 4096;
        if (defaultCodeSize < 4096) defaultCodeSize = 4096;
        if (defaultNoOverSize < 4096) defaultNoOverSize = 4096;
        // Set maximum sizes as well.  We may have insufficient contiguous space for
        // very large areas.
        if (defaultMutSize > 1024 * 1024) defaultMutSize = 1024 * 1024;
        if (defaultImmSize > 1024 * 1024) defaultImmSize = 1024 * 1024;
        if (defaultCodeSize > 1024 * 1024) defaultCodeSize = 1024 * 1024;
        if (defaultNoOverSize > 1024 * 1024) defaultNoOverSize = 1024 * 1024;
    }
    if (debugOptions & DEBUG_SAVING)
        Log("SAVE: Copyscan default sizes: Immutable: %" POLYUFMT ", Mutable: %" POLYUFMT ", Code: %" POLYUFMT ", No-overwrite %" POLYUFMT ".\n",
            defaultImmSize, defaultMutSize, defaultCodeSize, defaultNoOverSize);
}

CopyScan::~CopyScan()
{
    gMem.DeleteExportSpaces();
    if (graveYard)
        delete[](graveYard);
}


// This function is called for each address in an object
// once it has been copied to its new location.  We copy first
// then scan to update the addresses.
POLYUNSIGNED CopyScan::ScanAddressAt(PolyWord *pt)
{
    PolyWord val = *pt;
    // Ignore integers.
    if (IS_INT(val) || val == PolyWord::FromUnsigned(0))
        return 0;

    PolyObject *obj = val.AsObjPtr();
    POLYUNSIGNED l = ScanAddress(&obj);
    *pt = obj;
    return l;
}

// This function is called for each address in an object
// once it has been copied to its new location.  We copy first
// then scan to update the addresses.
POLYUNSIGNED CopyScan::ScanAddress(PolyObject **pt)
{
    PolyObject *obj = *pt;
    MemSpace *space = gMem.SpaceForObjectAddress(obj);
    ASSERT(space != 0);
    // We may sometimes get addresses that have already been updated
    // to point to the new area.  e.g. (only?) in the case of constants
    // that have been updated in ScanConstantsWithinCode.
    if (space->spaceType == ST_EXPORT)
        return 0;

    // If this is at a lower level than the hierarchy we are saving
    // then leave it untouched.
    if (space->spaceType == ST_PERMANENT)
    {
        PermanentMemSpace *pmSpace = (PermanentMemSpace*)space;
        if (dependencies[pmSpace->moduleIdentifier])
            return 0;
    }

    // Have we already scanned this?
    if (obj->ContainsForwardingPtr())
    {
        // Update the address to the new value.
#ifdef POLYML32IN64
        PolyObject *newAddr;
        if (space->isCode)
            newAddr = (PolyObject*)(globalCodeBase + (((uintptr_t)(obj->LengthWord()) & ~_OBJ_TOMBSTONE_BIT) * POLYML32IN64));
        else newAddr = obj->GetForwardingPtr();
#else
        PolyObject *newAddr = obj->GetForwardingPtr();
#endif
        *pt = newAddr;
        return 0; // No need to scan it again.
    }
    else if (space->spaceType == ST_PERMANENT)
    {
        // See if we have this in the grave-yard.
        for (unsigned i = 0; i < tombs; i++)
        {
            GraveYard *g = &graveYard[i];
            if ((PolyWord*)obj >= g->startAddr && (PolyWord*)obj < g->endAddr)
            {
                PolyWord *tombAddr = g->graves + ((PolyWord*)obj - g->startAddr);
                PolyObject *tombObject = (PolyObject*)tombAddr;
                if (tombObject->ContainsForwardingPtr())
                {
#ifdef POLYML32IN64
                    PolyObject *newAddr;
                    if (space->isCode)
                        newAddr = (PolyObject*)(globalCodeBase + (((uintptr_t)(tombObject->LengthWord()) & ~_OBJ_TOMBSTONE_BIT) * POLYML32IN64));
                    else newAddr = tombObject->GetForwardingPtr();
#else
                    PolyObject *newAddr = tombObject->GetForwardingPtr();
#endif
                    *pt = newAddr;
                    return 0;
                }
                break; // No need to look further
            }
        }
    }

    // No, we need to copy it.
    ASSERT(space->spaceType == ST_LOCAL || space->spaceType == ST_PERMANENT ||
        space->spaceType == ST_CODE);
    POLYUNSIGNED lengthWord = obj->LengthWord();
    POLYUNSIGNED originalLengthWord = lengthWord;
    POLYUNSIGNED words = OBJ_OBJECT_LENGTH(lengthWord);

    enum _newAddrType naType;
    if (obj->IsMutable())
    {
        if (obj->IsNoOverwriteObject()) naType = NANoOverwriteMutable; else naType = NAMutable;
    }
    else if (obj->IsCodeObject()) naType = NACode;
    else if (obj->IsByteObject()) naType = NAByte;
    else naType = NAWord;
    PolyObject* newObj;
#if((defined(HOSTARCHITECTURE_X86_64) || defined(HOSTARCHITECTURE_AARCH64)) && ! defined(POLYML32IN64) && !defined(CODEISNOTEXECUTABLE) && !defined(_WIN32))
    // SELinux, OpenBSD and Mac OS, at least on the ARM, require or prefer executable code segments without
    // absolute addresses: position-independent code.
    // That means the constant area cannot be in the same segment as the code.  We have to split the constant area
    // out and put it into the read-only, non-executable area.
    // Interpreted code and code for 32-in-64 aren't executable (32-in-64 code is copied during start-up).
    // We also don't need this on Windows, thankfully.
    if (obj->IsCodeObject() && isExport)
    {
        PolyWord* constPtr;
        POLYUNSIGNED numConsts;
        machineDependent->GetConstSegmentForCode(obj, constPtr, numConsts);
        // Newly generated code will have the constants included with the code
        // but if this is in the executable the constants will have been extracted before.
        bool constsWereIncluded = constPtr > (PolyWord*)obj && constPtr < ((PolyWord*)obj) + words;
        POLYUNSIGNED codeAreaSize = words;
        if (constsWereIncluded)
            codeAreaSize -= numConsts + 1;
        newObj = newAddressForObject(codeAreaSize, NACode);
        PolyObject* writableObj = gMem.SpaceForObjectAddress(newObj)->writeAble(newObj);
        writableObj->SetLengthWord(codeAreaSize, F_CODE_OBJ); // set length word
        lengthWord = newObj->LengthWord(); // Get the actual length word used
        memcpy(writableObj, obj, codeAreaSize * sizeof(PolyWord));
        PolyObject* newConsts = newAddressForObject(numConsts, NACodeConst);
        PolyObject* writableConsts = gMem.SpaceForObjectAddress(newConsts)->writeAble(newConsts);
        writableConsts->SetLengthWord(numConsts);
        memcpy(writableConsts, constPtr, numConsts * sizeof(PolyWord));
        machineDependent->SetAddressOfConstants(newObj, writableObj, codeAreaSize, (PolyWord*)newConsts);
    }
    else
#endif
    {
        newObj = newAddressForObject(words, naType);
        PolyObject* writAble = gMem.SpaceForObjectAddress(newObj)->writeAble(newObj);
        writAble->SetLengthWord(lengthWord); // copy length word

        if (isExport /* Exporting object module */ && obj->IsNoOverwriteObject() && ! obj->IsByteObject())
        {
            // These are not exported. They are used for special values e.g. mutexes
            // that should be set to 0/nil/NONE at start-up.
            // Weak+No-overwrite byte objects are used for entry points and volatiles
            // in the foreign-function interface and have to be treated specially.

            // Note: this must not be done when exporting a saved state because the
            // copied version is used as the local data for the rest of the session.
            for (POLYUNSIGNED i = 0; i < words; i++)
                writAble->Set(i, TAGGED(0));
        }
        else memcpy(writAble, obj, words * sizeof(PolyWord));
    }

    if (space->spaceType == ST_PERMANENT && ((PermanentMemSpace*)space)->isWriteProtected)
    {
        // The immutable permanent areas are read-only.
        unsigned m;
        for (m = 0; m < tombs; m++)
        {
            GraveYard *g = &graveYard[m];
            if ((PolyWord*)obj >= g->startAddr && (PolyWord*)obj < g->endAddr)
            {
                PolyWord *tombAddr = g->graves + ((PolyWord*)obj - g->startAddr);
                PolyObject *tombObject = (PolyObject*)tombAddr;
#ifdef POLYML32IN64
                if (naType == NACode)
                {
                    POLYUNSIGNED ll = (POLYUNSIGNED)((((PolyWord*)newObj - globalCodeBase) / POLYML32IN64) | _OBJ_TOMBSTONE_BIT);
                    tombObject->SetLengthWord(ll);
                }
                else tombObject->SetForwardingPtr(newObj);
#else
                tombObject->SetForwardingPtr(newObj);
#endif
                break; // No need to look further
            }
        }
        ASSERT(m < tombs); // Should be there.
    }
    else if (naType == NACode)
#ifdef POLYML32IN64
    // If this is a code address we can't use the usual forwarding pointer format.
    // Instead we have to compute the offset relative to the base of the code.
    {
        POLYUNSIGNED ll = (POLYUNSIGNED)((((PolyWord*)newObj - globalCodeBase) / POLYML32IN64) | _OBJ_TOMBSTONE_BIT);
        gMem.SpaceForObjectAddress(obj)->writeAble(obj)->SetLengthWord(ll);
    }
#else
        gMem.SpaceForObjectAddress(obj)->writeAble(obj)->SetForwardingPtr(newObj);
#endif
    else obj->SetForwardingPtr(newObj); // Put forwarding pointer in old object.

    if (naType == NACode)
    {
        // We should flush the instruction cache here since we will execute the code
        // at this location if this is a saved state.
        machineDependent->FlushInstructionCache(newObj, newObj->Length());
        // We have to update any relative addresses within the code
        // to take account of its new position.  We have to do that now
        // even though ScanAddressesInObject will do it again because this
        // is the only point where we have both the old and the new addresses.
        PolyWord *oldConstAddr;
        POLYUNSIGNED count;
        machineDependent->GetConstSegmentForCode(obj, OBJ_OBJECT_LENGTH(originalLengthWord), oldConstAddr, count);
        PolyWord *newConstAddr = machineDependent->ConstPtrForCode(newObj);
        machineDependent->ScanConstantsWithinCode(newObj, obj, newObj->Length(), newConstAddr, oldConstAddr, count, this);
    }
    *pt = newObj; // Update it to the newly copied object.

    // Add byte data and tagged values to the hash.  Don't include addresses since
    // these depend on storage allocation and may not be reproducible.
    if (newObj->IsByteObject())
    {
        for (POLYUNSIGNED i = 0; i < words; i++)
            addWordToHash(newObj->Get(i).AsUnsigned());
    }
    else if (newObj->IsWordObject())
    {
        for (POLYUNSIGNED i = 0; i < words; i++)
        {
            PolyWord p = newObj->Get(i);
            if (p.IsTagged())
                addWordToHash(p.AsUnsigned());
        }
    }

    return lengthWord;  // This new object needs to be scanned.
}

PolyObject* CopyScan::newAddressForObject(POLYUNSIGNED words, enum _newAddrType naType)
{
    PolyObject* newObj = 0;
    // Allocate a new address for the object.
    for (std::vector<PermanentMemSpace*>::iterator i = gMem.eSpaces.begin(); i < gMem.eSpaces.end(); i++)
    {
        PermanentMemSpace* space = *i;
        bool match = false;
        switch (naType)
        {
        case NAWord: match = !space->isMutable && !space->byteOnly && !space->isCode; break;
        case NAMutable: match = space->isMutable && !space->noOverwrite; break;
        case NANoOverwriteMutable: match = space->isMutable && space->noOverwrite; break;
        case NAByte: match = !space->isMutable && space->byteOnly; break;
        case NACode: match = !space->isMutable && space->isCode && !space->constArea; break;
        case NACodeConst: match = !space->isMutable && space->isCode && space->constArea; break;
        }
        if (match)
        {
            ASSERT(space->topPointer <= space->top && space->topPointer >= space->bottom);
            size_t spaceLeft = space->top - space->topPointer;
            if (spaceLeft > words)
            {
                newObj = (PolyObject*)(space->topPointer + 1);
                space->topPointer += words + 1;
#ifdef POLYML32IN64
                // Maintain the odd-word alignment of topPointer
                for(POLYUNSIGNED w = (words + 1) & (POLYML32IN64-1); w < POLYML32IN64 && space->topPointer < space->top; w++)
                {
                    *space->writeAble(space->topPointer) = PolyWord::FromUnsigned(0);
                    space->topPointer++;
                }
#endif
                break;
            }
        }
    }
    if (newObj == 0)
    {
        // Didn't find room in the existing spaces.  Create a new space.
        uintptr_t spaceWords;
        switch (naType)
        {
        case NAMutable: spaceWords = defaultMutSize; break;
        case NANoOverwriteMutable: spaceWords = defaultNoOverSize; break;
        case NACode: spaceWords = defaultCodeSize; break;
        case NACodeConst: spaceWords = defaultCodeSize; break;
        default: spaceWords = defaultImmSize;
        }
        if (spaceWords <= words)
            spaceWords = words + 1; // Make sure there's space for this object.
        PermanentMemSpace* space =
            gMem.NewExportSpace(spaceWords, naType == NAMutable || naType == NANoOverwriteMutable, naType == NANoOverwriteMutable,
                    naType == NACode || naType == NACodeConst);
        if (naType == NAByte) space->byteOnly = true;
        if (naType == NACodeConst) space->constArea = true;
        if (space == 0)
        {
            if (debugOptions & DEBUG_SAVING)
                Log("SAVE: Unable to allocate export space, size: %lu.\n", spaceWords);
            // Unable to allocate this.
            throw MemoryException();
        }
        newObj = (PolyObject*)(space->topPointer + 1);
        space->topPointer += words + 1;
#ifdef POLYML32IN64
        // Maintain the odd-word alignment of topPointer
        for (POLYUNSIGNED w = (words + 1) & (POLYML32IN64 - 1); w < POLYML32IN64 && space->topPointer < space->top; w++)
        {
            *space->writeAble(space->topPointer) = PolyWord::FromUnsigned(0);
            space->topPointer++;
        }
#endif
        ASSERT(space->topPointer <= space->top && space->topPointer >= space->bottom);
    }
    return newObj;
}

// The address of code in the code area.  We treat this as a normal heap cell.
// We will probably need to copy this and to process addresses within it.
POLYUNSIGNED CopyScan::ScanCodeAddressAt(PolyObject **pt)
{
    POLYUNSIGNED lengthWord = ScanAddress(pt);
    if (lengthWord)
        ScanAddressesInObject(*pt, lengthWord);
    return 0;
}

PolyObject *CopyScan::ScanObjectAddress(PolyObject *base)
{
    PolyWord val = base;
    // Scan this as an address. 
    POLYUNSIGNED lengthWord = CopyScan::ScanAddressAt(&val);
    if (lengthWord)
        ScanAddressesInObject(val.AsObjPtr(), lengthWord);
    return val.AsObjPtr();
}

// Hash code.  Modified from code by Bob Jenkins - https://burtleburtle.net/bob/c/lookup3.c

static inline uint32_t rot(uint32_t x, uint32_t k)
{
    return (x << k) | (x >> (32 - k));
}

void CopyScan::addToHash(uint32_t p)
{
    switch (hash_posn)
    {
    case 0:
        hash_a += p;
        hash_posn = 1;
        break;
    case 1:
        hash_b += p;
        hash_posn = 2;
        break;
    case 2:
        hash_c += p;
        hash_posn = 0;
        // Mix the values.
        hash_a -= hash_c;  hash_a ^= rot(hash_c, 4);  hash_c += hash_b;
        hash_b -= hash_a;  hash_b ^= rot(hash_a, 6);  hash_a += hash_c;
        hash_c -= hash_b;  hash_c ^= rot(hash_b, 8);  hash_b += hash_a;
        hash_a -= hash_c;  hash_a ^= rot(hash_c, 16);  hash_c += hash_b;
        hash_b -= hash_a;  hash_b ^= rot(hash_a, 19);  hash_a += hash_c;
        hash_c -= hash_b;  hash_c ^= rot(hash_b, 4);  hash_b += hash_a;
        break;
    }
}

void CopyScan::addWordToHash(POLYUNSIGNED p)
{
#if (SIZEOF_POLYWORD == 4)
    addToHash(p);
#else
    addToHash(p & 0xffffffff);
    addToHash((p >> 32) & 0xffffffff);
#endif
}

struct _moduleId CopyScan::extractHash()
{
    uint32_t a = hash_a, b = hash_b, c = hash_c;
    c ^= b; c -= rot(b, 14);
    a ^= c; a -= rot(c, 11);
    b ^= a; b -= rot(a, 25);
    c ^= b; c -= rot(b, 16);
    a ^= c; a -= rot(c, 4);
    b ^= a; b -= rot(a, 14);
    c ^= b; c -= rot(b, 24);
    return { hash_c, hash_b };
}

// Convert the forwarding pointers in a region back into length words.

// Generally if this object has a forwarding pointer that's
// because we've moved it into the export region.  We can,
// though, get multiple levels of forwarding if there is an object
// that has been shifted up by a garbage collection, leaving a forwarding
// pointer and then that object has been moved to the export region.
// We mustn't turn locally forwarded values back into ordinary objects
// because they could contain addresses that are no longer valid.
static POLYUNSIGNED FixObjLength(PolyObject *obj)
{
    if (obj->ContainsForwardingPtr())
    {
        PolyObject *forwardedTo;
#ifdef POLYML32IN64
        {
            MemSpace *space = gMem.SpaceForObjectAddress(obj);
            if (space->isCode)
                forwardedTo = (PolyObject*)(globalCodeBase + (((uintptr_t)(obj->LengthWord()) & ~_OBJ_TOMBSTONE_BIT) * POLYML32IN64));
            else forwardedTo = obj->GetForwardingPtr();
        }
#else
        forwardedTo = obj->GetForwardingPtr();
#endif
        POLYUNSIGNED length = FixObjLength(forwardedTo);
        MemSpace *space = gMem.SpaceForObjectAddress(forwardedTo);
        if (space->spaceType == ST_EXPORT)
        {
            // If this is a code object whose constant area has been split off we
            // need to add the length of the constant area.
            if (forwardedTo->IsCodeObject())
            {
                PolyWord* constPtr;
                POLYUNSIGNED numConsts;
                machineDependent->GetConstSegmentForCode(forwardedTo, constPtr, numConsts);
                if (!(constPtr > (PolyWord*)forwardedTo && constPtr < ((PolyWord*)forwardedTo) + OBJ_OBJECT_LENGTH(length)))
                    length += numConsts + 1;
            }
            gMem.SpaceForObjectAddress(obj)->writeAble(obj)->SetLengthWord(length);
        }
        return length;
    }
    else {
        ASSERT(obj->ContainsNormalLengthWord());
        return obj->LengthWord();
    }
}

static void FixForwarding(PolyWord *pt, size_t space)
{
    while (space)
    {
        pt++;
#ifdef POLYML32IN64
        if (((pt - (PolyWord*)0) & (POLYML32IN64 - 1)) != 0) // i.e. it's not an the required unit boundary
        {
            // Skip filler words needed to align to an even word
            space--;
            continue; // We've added 1 to pt so just loop.
        }
#endif
        PolyObject* obj = (PolyObject*)pt;
        size_t length = OBJ_OBJECT_LENGTH(FixObjLength(obj));
        pt += length;
        ASSERT(space > length);
        space -= length+1;
    }
}

class ExportRequest: public MainThreadRequest
{
public:
    ExportRequest(Handle root, Exporter *exp): MainThreadRequest(MTP_EXPORTING),
        exportRoot(root), exporter(exp) {}

    virtual void Perform() { exporter->RunExport(exportRoot->WordP()); }
    Handle exportRoot;
    Exporter *exporter;
};

static void exporter(TaskData *taskData, Handle fileName, Handle root, const TCHAR *extension, Exporter *exports)
{
    size_t extLen = _tcslen(extension);
    TempString fileNameBuff(Poly_string_to_T_alloc(fileName->Word(), extLen));
    if (fileNameBuff == NULL)
        raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    size_t length = _tcslen(fileNameBuff);

    // Does it already have the extension?  If not add it on.
    if (length < extLen || _tcscmp(fileNameBuff + length - extLen, extension) != 0)
        _tcscat(fileNameBuff, extension);
#if (defined(_WIN32) && defined(UNICODE))
    exports->exportFile = _wfopen(fileNameBuff, L"wb");
#else
    exports->exportFile = fopen(fileNameBuff, "wb");
#endif
    if (exports->exportFile == NULL)
        raise_syscall(taskData, "Cannot open export file", ERRORNUMBER);

    // Request a full GC  to reduce the size of fix-ups.
    FullGC(taskData);
    // Request the main thread to do the export.
    ExportRequest request(root, exports);
    processes->MakeRootRequest(taskData, &request);
    if (exports->errorMessage)
        raise_fail(taskData, exports->errorMessage);
}

// This is called by the initial thread to actually do the export.
void Exporter::RunExport(PolyObject *rootFunction)
{
    Exporter *exports = this;

    PolyObject *copiedRoot = 0;
    CopyScan copyScan(true); // Exclude the parent state if this is a module

    try {
        copyScan.initialise();
        // Copy the root and everything reachable from it into the temporary area.
        copiedRoot = copyScan.ScanObjectAddress(rootFunction);
    }
    catch (MemoryException &)
    {
        // If we ran out of memory.
        copiedRoot = 0;
    }

    // Reset the forwarding pointers so that the original data structure is in
    // local storage.
    CopyScan::revertToLocal();

    exportModId = copyScan.extractHash();

    // Reraise the exception after cleaning up the forwarding pointers.
    if (copiedRoot == 0)
    {
        exports->errorMessage = "Insufficient Memory";
        return;
    }

    // Copy the areas into the export object.
    size_t tableEntries = gMem.eSpaces.size();
    unsigned memEntry = 0;
    exports->memTable = new ExportMemTable[tableEntries];

    for (std::vector<PermanentMemSpace *>::iterator i = gMem.eSpaces.begin(); i < gMem.eSpaces.end(); i++)
    {
        ExportMemTable*entry = &exports->memTable[memEntry++];
        PermanentMemSpace *space = *i;
        entry->mtOriginalAddr = entry->mtCurrentAddr = space->bottom;
        entry->mtLength = (space->topPointer-space->bottom)*sizeof(PolyWord);
        entry->mtIndex = memEntry-1;
        entry->mtModId = exportModId;
        entry->mtFlags = 0;
        if (space->isMutable)
        {
            entry->mtFlags = MTF_WRITEABLE;
            if (space->noOverwrite) entry->mtFlags |= MTF_NO_OVERWRITE;
        }
        if (space->isCode && !space->constArea) entry->mtFlags |= MTF_EXECUTABLE;
        if (space->byteOnly) entry->mtFlags |= MTF_BYTES;
    }

    ASSERT(memEntry == tableEntries);
    exports->memTableEntries = memEntry;
    exports->rootFunction = copiedRoot;
    try {
        // This can raise MemoryException at least in PExport::exportStore. 
        exports->exportStore();
    }
    catch (MemoryException &) {
        exports->errorMessage = "Insufficient Memory";
    }
}


// Generate the address relative to the start of the segment.
void StateExport::setRelocationAddress(void* p, POLYUNSIGNED* reloc)
{
    unsigned area = findArea(p);
    POLYUNSIGNED offset = (POLYUNSIGNED)((char*)p - (char*)memTable[area].mtOriginalAddr);
    *reloc = offset;
}

PolyWord StateExport::createRelocation(PolyWord p, void* relocAddr)
{
#ifdef POLYML32IN64
    createActualRelocation(p.AsAddress(), relocAddr, PROCESS_RELOC_C32ADDR);
#else
    createActualRelocation(p.AsAddress(), relocAddr, PROCESS_RELOC_DIRECT);
#endif
    return p; // Don't change the contents
}

// Override for Exporter::relocateValue.  That function does not do anything for compact 32-bit because
// the operating system object module formats do not support a suitable relocation format.
void StateExport::relocateValue(PolyWord* pt)
{
    PolyWord q = *pt;
    if (IS_INT(q) || q == PolyWord::FromUnsigned(0)) {}
    else *gMem.SpaceForAddress(pt)->writeAble(pt) = createRelocation(*pt, pt);
}

// We need to override this since Exporter::relocateObject doesn't work for compact 32-bits
void StateExport::relocateObject(PolyObject* p)
{
    if (p->IsClosureObject())
    {
        POLYUNSIGNED length = p->Length();
        // The first word is an absolute code address.
        PolyWord* pt = p->Offset(0);
        void* addr = *(void**)pt;
        createActualRelocation(addr, pt, PROCESS_RELOC_DIRECT);

        for (POLYUNSIGNED i = sizeof(uintptr_t) / sizeof(PolyWord); i < length; i++) relocateValue(p->Offset(i));
    }
    // Exporter::relocateObject clears weak mutable byte refs.  We mustn't do that
    // because we're going to use the exported copy afterwards.  Instead
    // weak byte refs are cleared and external refs set after states
    // and modules are loaded: ClearVolatile class.
    else if (!p->IsByteObject())
        Exporter::relocateObject(p);
}


/* This is called for each constant within the code.
   Print a relocation entry for the word and return a value that means
   that the offset is saved in original word. */
void StateExport::ScanConstant(PolyObject* base, byte* addr, ScanRelocationKind code, intptr_t displacement)
{
    PolyObject* p = GetConstantValue(addr, code, displacement);

    if (p == 0)
        return;

    void* a = p;
    unsigned aArea = findArea(a);

    // We don't need a relocation if this is relative to the current segment
    // since the relative address will already be right.
    if (code == PROCESS_RELOC_I386RELATIVE && aArea == findArea(addr))
        return;

    // Set the value at the address to the offset relative to the symbol.
    createActualRelocation(a, addr, code);
}


// Functions called via the RTS call.
Handle exportNative(TaskData *taskData, Handle args)
{
#ifdef HAVE_PECOFF
    // Windows including Cygwin
#if (defined(_WIN32))
    const TCHAR *extension = _T(".obj"); // Windows
#else
    const char *extension = ".o"; // Cygwin
#endif
    PECOFFExport exports;
    exporter(taskData, taskData->saveVec.push(args->WordP()->Get(0)),
        taskData->saveVec.push(args->WordP()->Get(1)), extension, &exports);
#elif defined(HAVE_ELF_H) || defined(HAVE_ELF_ABI_H)
    // Most Unix including Linux, FreeBSD and Solaris.
    const char *extension = ".o";
    ELFExport exports;
    exporter(taskData, taskData->saveVec.push(args->WordP()->Get(0)),
        taskData->saveVec.push(args->WordP()->Get(1)), extension, &exports);
#elif defined(HAVE_MACH_O_RELOC_H)
    // Mac OS-X
    const char *extension = ".o";
    MachoExport exports;
    exporter(taskData, taskData->saveVec.push(args->WordP()->Get(0)),
        taskData->saveVec.push(args->WordP()->Get(1)), extension, &exports);
#else
    raise_exception_string (taskData, EXC_Fail, "Native export not available for this platform");
#endif
    return taskData->saveVec.push(TAGGED(0));
}

Handle exportPortable(TaskData *taskData, Handle args)
{
    PExport exports;
    exporter(taskData, taskData->saveVec.push(args->WordP()->Get(0)),
        taskData->saveVec.push(args->WordP()->Get(1)), _T(".txt"), &exports);
    return taskData->saveVec.push(TAGGED(0));
}

POLYUNSIGNED PolyExport(POLYUNSIGNED threadId, POLYUNSIGNED fileName, POLYUNSIGNED root)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedName = taskData->saveVec.push(fileName);
    Handle pushedRoot = taskData->saveVec.push(root);

    try {
#ifdef HAVE_PECOFF
        // Windows including Cygwin
#if (defined(_WIN32))
        const TCHAR *extension = _T(".obj"); // Windows
#else
        const char *extension = ".o"; // Cygwin
#endif
        PECOFFExport exports;
        exporter(taskData, pushedName, pushedRoot, extension, &exports);
#elif defined(HAVE_ELF_H) || defined(HAVE_ELF_ABI_H)
        // Most Unix including Linux, FreeBSD and Solaris.
        const char *extension = ".o";
        ELFExport exports;
        exporter(taskData, pushedName, pushedRoot, extension, &exports);
#elif defined(HAVE_MACH_O_RELOC_H)
        // Mac OS-X
        const char *extension = ".o";
        MachoExport exports;
        exporter(taskData, pushedName, pushedRoot, extension, &exports);
#else
        raise_exception_string (taskData, EXC_Fail, "Native export not available for this platform");
#endif
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned(); // Returns unit
}

POLYUNSIGNED PolyExportPortable(POLYUNSIGNED threadId, POLYUNSIGNED fileName, POLYUNSIGNED root)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedName = taskData->saveVec.push(fileName);
    Handle pushedRoot = taskData->saveVec.push(root);

    try {
        PExport exports;
        exporter(taskData, pushedName, pushedRoot, _T(".txt"), &exports);
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned(); // Returns unit
}


// Helper functions for exporting.  We need to produce relocation information
// and this code is common to every method.
Exporter::Exporter(): exportFile(NULL), errorMessage(0), errNumber(0), memTable(0), memTableEntries(0), rootFunction(0)
{
}

Exporter::~Exporter()
{
    delete[](memTable);
    if (exportFile)
        fclose(exportFile);
}

void Exporter::relocateValue(PolyWord *pt)
{
#ifndef POLYML32IN64
    PolyWord q = *pt;
    if (IS_INT(q) || q == PolyWord::FromUnsigned(0)) {}
    else createRelocation(pt);
#endif
}

void Exporter::createRelocation(PolyWord* pt)
{
    *gMem.SpaceForAddress(pt)->writeAble(pt) = createRelocation(*pt, pt);
}

// Check through the areas to see where the address is.  It must be
// in one of them.
unsigned Exporter::findArea(void *p)
{
    for (unsigned i = 0; i < memTableEntries; i++)
    {
        if (p > memTable[i].mtOriginalAddr &&
            p <= (char*)memTable[i].mtOriginalAddr + memTable[i].mtLength)
            return i;
    }
    { ASSERT(0); }
    return 0;
}

void Exporter::relocateObject(PolyObject *p)
{
    if (p->IsByteObject())
    {
        if (p->IsMutable() && p->IsWeakRefObject())
        {
            // Weak mutable byte refs are used for external references and
            // also in the FFI for non-persistent values.
            bool isFuncPtr = true;
            const char *entryName = getEntryPointName(p, &isFuncPtr);
            if (entryName != 0) addExternalReference(p, entryName, isFuncPtr);
            // Clear the first word of the data.
            ASSERT(p->Length() >= sizeof(uintptr_t)/sizeof(PolyWord));
            *(uintptr_t*)p = 0;
        }
    }
    else if (p->IsCodeObject())
    {
        POLYUNSIGNED constCount;
        PolyWord *cp;
        ASSERT(! p->IsMutable() );
        machineDependent->GetConstSegmentForCode(p, cp, constCount);
        /* Now the constants. */
        for (POLYUNSIGNED i = 0; i < constCount; i++) relocateValue(&(cp[i]));
    }
    else // Closure and ordinary objects
    {
        // TODO: This doesn't work properly for closures in compact 32-bit since it
        // treats the absolutely address of the code as two ordinary PolyWord values.
        // That doesn't matter when exporting to an object file since in that case
        // we never actually generate any relocations.
        POLYUNSIGNED length = p->Length();
        for (POLYUNSIGNED i = 0; i < length; i++) relocateValue(p->Offset(i));
    }
}

// CopyScan makes a copy of everything reachable from the root into export spaces leaving
// forwarding pointers in the original space.  This function reverts those forwarding pointers.
void CopyScan::fixPermanentAreas()
{
    for (std::vector<PermanentMemSpace*>::iterator i = gMem.pSpaces.begin(); i < gMem.pSpaces.end(); i++)
    {
        MemSpace* space = *i;
        // Permanent areas are filled with objects from the bottom.
        FixForwarding(space->bottom, space->top - space->bottom);
    }
}

void CopyScan::revertToLocal()
{
    // Fix the forwarding pointers.
    for (std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
    {
        LocalMemSpace* space = *i;
        // Local areas only have objects from the allocation pointer to the top.
        FixForwarding(space->bottom, space->lowerAllocPtr - space->bottom);
        FixForwarding(space->upperAllocPtr, space->top - space->upperAllocPtr);
    }
    fixPermanentAreas();
    for (std::vector<CodeSpace*>::iterator i = gMem.cSpaces.begin(); i < gMem.cSpaces.end(); i++)
    {
        MemSpace* space = *i;
        // Code areas are filled with objects from the bottom.
        FixForwarding(space->bottom, space->top - space->bottom);
    }
}


bool Exporter::checkedFwrite(const void* buffer, size_t size, size_t count)
{
    size_t written = fwrite(buffer, size, count, exportFile);
    if (written != count)
    {
        errNumber = ERRORNUMBER;
        errorMessage = "Error in fwrite"; // Will be set to the appropriate string in raise_syscall.
        return false;
    }
    return true;

}

ExportStringTable::ExportStringTable(): strings(0), stringSize(0), stringAvailable(0)
{
}

ExportStringTable::~ExportStringTable()
{
    free(strings);
}

// Add a string to the string table, growing it if necessary.
unsigned long ExportStringTable::makeEntry(const char *str)
{
    unsigned len = (unsigned)strlen(str);
    unsigned long entry = stringSize;
    if (stringSize + len + 1 > stringAvailable)
    {
        stringAvailable = stringAvailable+stringAvailable/2;
        if (stringAvailable < stringSize + len + 1)
            stringAvailable = stringSize + len + 1 + 500;
        char* newStrings = (char*)realloc(strings, stringAvailable);
        if (newStrings == 0)
        {
            if (debugOptions & DEBUG_SAVING)
                Log("SAVE: Unable to realloc string table, size: %lu.\n", stringAvailable);
            throw MemoryException();
        }
        else strings = newStrings;
     }
    strcpy(strings + stringSize, str);
    stringSize += len + 1;
    return entry;
}

struct _entrypts exporterEPT[] =
{
    { "PolyExport",                     (polyRTSFunction)&PolyExport},
    { "PolyExportPortable",             (polyRTSFunction)&PolyExportPortable},

    { NULL, NULL} // End of list.
};
