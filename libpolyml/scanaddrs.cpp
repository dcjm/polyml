/*
    Title:      Address scanner

    Copyright (c) 2006-8, 2012, 2019, 2021, 2025 David C.J. Matthews

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
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#include <new>

#include "globals.h"
#include "scanaddrs.h"
#include "machine_dep.h"
#include "diagnostics.h"
#include "memmgr.h"

// Process the value at a given location and update it as necessary.
POLYUNSIGNED ScanAddress::ScanAddressAt(PolyWord *pt)
{
    PolyWord val = *pt;
    PolyWord newVal = val;
    if (IS_INT(val) || val == PolyWord::FromUnsigned(0))
    {
        // We can get zeros in the constant area if we garbage collect
        //  while compiling some code. */
    }
    else
    {
        ASSERT(OBJ_IS_DATAPTR(val));
        // Any sort of address
        newVal = ScanObjectAddress(val.AsObjPtr());
    }
    if (newVal != val) // Only update if we need to.
        *pt = newVal;
    return 0;
}

// General purpose object processor,  Processes all the addresses in an object.
// Handles the various kinds of object that may contain addresses.
void ScanAddress::ScanAddressesInObject(PolyObject *obj, POLYUNSIGNED lengthWord)
{
    do
    {
        ASSERT (OBJ_IS_LENGTH(lengthWord));
    
        if (OBJ_IS_BYTE_OBJECT(lengthWord))
            return; /* Nothing more to do */
    
        POLYUNSIGNED length = OBJ_OBJECT_LENGTH(lengthWord);
        PolyWord *baseAddr = (PolyWord*)obj;
    
        if (OBJ_IS_CODE_OBJECT(lengthWord))
        {
            // Scan constants within the code.
            machineDependent->ScanConstantsWithinCode(obj, length, this);
            // Skip to the constants and get ready to scan them.
            machineDependent->GetConstSegmentForCode(obj, length, baseAddr, length);
            // Adjust to the read-write area if necessary.
            baseAddr = gMem.SpaceForAddress(baseAddr)->writeAble(baseAddr);
        }

        else if (OBJ_IS_CLOSURE_OBJECT(lengthWord))
        {
            // The first word is a code pointer so we need to treat it specially
            // but it is possible it hasn't yet been set.
            if ((*(uintptr_t*)baseAddr & 1) == 0)
            {
                POLYUNSIGNED lengthWord = ScanCodeAddressAt((PolyObject**)baseAddr); // N.B.  This could side-effect *baseAddr
                if (lengthWord != 0)
                    ScanAddressesInObject(*(PolyObject**)baseAddr, lengthWord);
            }
            baseAddr += sizeof(PolyObject*) / sizeof(PolyWord);
            length -= sizeof(PolyObject*) / sizeof(PolyWord);
        }

        PolyWord *endWord = baseAddr + length;

        // We want to minimise the actual recursion we perform so we try to
        // use tail recursion if we can.  We first scan from the end and
        // remove any words that don't need recursion.
        POLYUNSIGNED lastLengthWord = 0;
        while (endWord != baseAddr)
        {
            PolyWord wordAt = endWord[-1];
            if (IS_INT(wordAt) || wordAt == PolyWord::FromUnsigned(0))
                endWord--; // Don't need to look at this.
            else if ((lastLengthWord = ScanAddressAt(endWord-1)) != 0)
                // We need to process this one
                break;
            else endWord--; // We're not interested in this.
        }

        if (endWord == baseAddr)
            return; // We've done everything.

        // There is at least one word that needs to be processed, the
        // one at endWord-1.
        // Now process from the beginning forward to see if there are
        // any words before this that need to be handled.  This way we are more
        // likely to handle the head of a list by recursion and the
        // tail by looping (tail recursion).
        while (baseAddr < endWord-1)
        {
            PolyWord wordAt = *baseAddr;
            if (IS_INT(wordAt) || wordAt == PolyWord::FromUnsigned(0))
                baseAddr++; // Don't need to look at this.
            else
            {
                POLYUNSIGNED lengthWord = ScanAddressAt(baseAddr);
                if (lengthWord != 0)
                {
                    wordAt = *baseAddr; // Reload because it may have been side-effected
                     // We really have to process this recursively.
                    ASSERT(wordAt.IsDataPtr());
                    ScanAddressesInObject(wordAt.AsObjPtr(), lengthWord);
                    baseAddr++;
                }
                else baseAddr++;
            }
        }

        // Finally process the last word we found that has to be processed.
        // Do this by looping rather than recursion.
        PolyWord wordAt = *baseAddr; // Last word to do.
        // This must be an address 
        ASSERT(wordAt.IsDataPtr());
        obj = wordAt.AsObjPtr();

        lengthWord = lastLengthWord;

    } while(1);
}

void ScanAddress::ScanAddressesInRegion(PolyWord *region, PolyWord *end)
{
    PolyWord *pt = region;
    while (pt < end)
    {
#ifdef POLYML32IN64
        if (((pt - (PolyWord*)0) & (POLYML32IN64-1)) != POLYML32IN64 - 1)
        {
            // Skip any padding.  The length word should be in the last word of the unit.
            pt++;
            continue;
        }
#endif
        pt++; // Skip length word.
        // pt actually points AT the object here.
        PolyObject *obj = (PolyObject*)pt;
        if (obj->ContainsForwardingPtr())    /* skip over moved object */
        {
            // We can now get multiple forwarding pointers as a result
            // of applying ShareData repeatedly.  Perhaps we should
            // turn the forwarding pointers back into normal words in
            // an extra pass.
            obj = obj->FollowForwardingChain();
            ASSERT(obj->ContainsNormalLengthWord());
            pt += obj->Length();
        }
        else
        {
            ASSERT(obj->ContainsNormalLengthWord());
            POLYUNSIGNED length = obj->Length();
            if (pt+length > end)
                Crash("Malformed object at %p - length %lu\n", pt, length);
            if (length != 0)
                ScanAddressesInObject(obj);
            pt += length;
        }
    }
}

// Extract a constant from the code.
PolyObject *ScanAddress::GetConstantValue(byte *addressOfConstant, ScanRelocationKind code, intptr_t displacement)
{
    switch (code)
    {
    case PROCESS_RELOC_DIRECT: // Absolute address 
        {
            uintptr_t valu;
            byte *pt = addressOfConstant;
            if (pt[sizeof(uintptr_t)-1] & 0x80) valu = 0-1; else valu = 0;
            for (unsigned i = sizeof(uintptr_t); i > 0; i--)
                valu = (valu << 8) | pt[i-1];
            if (valu == 0 || PolyWord::FromUnsigned((POLYUNSIGNED)valu).IsTagged())
                return 0;
            else return (PolyObject*)valu;
        }
    case PROCESS_RELOC_I386RELATIVE:         // 32 bit relative address
        {
            POLYSIGNED disp;
            byte *pt = addressOfConstant;
            // Get the displacement. This is signed.
            if (pt[3] & 0x80) disp = -1; else disp = 0; // Set the sign just in case.
            for(unsigned i = 4; i > 0; i--) disp = (disp << 8) | pt[i-1];
            byte *absAddr = pt + disp + 4 + displacement; // The address is relative to AFTER the constant
            return (PolyObject*)absAddr;
        }
    case PROCESS_RELOC_ARM64ADRPLDR64:
    case PROCESS_RELOC_ARM64ADRPLDR32:
    case PROCESS_RELOC_ARM64ADRPADD:
    {
            // This is a pair of instructions.
            uint32_t* pt = (uint32_t*)addressOfConstant;
            uint32_t instr0 = fromARMInstr(pt[0]), instr1 = fromARMInstr(pt[1]);
            ASSERT((instr0 & 0x9f000000) == 0x90000000);
            int scale = code == PROCESS_RELOC_ARM64ADRPLDR64 ? 8 : code == PROCESS_RELOC_ARM64ADRPLDR32 ? 4 : 1;
            // ADRP: This is complicated. The offset is encoded in two parts.
            intptr_t disp = instr0 & 0x00800000 ? -1 : 0; // Sign bit
            disp = (disp << 19) + ((instr0 & 0x00ffffe0) >> 5); // Add in immhi
            disp = (disp << 2) + ((instr0 >> 29) & 3); // Add in immlo
            disp = disp << 12; // It's a page address
            // The second word is LDR or ADD with a 12-bit unsigned offset.
            // This is a scaled offset so the value is actually the offset / scale.
            disp += ((instr1 >> 10) & 0xfff) * scale;
            uintptr_t addr = (uintptr_t)addressOfConstant;
            addr = addr & -4096; // Clear the bottom 12 bits
            return (PolyObject*)(addr + disp);
        }
#ifdef POLYML32IN64
    case PROCESS_RELOC_C32ADDR:
    {
        uint32_t valu;
        byte* pt = addressOfConstant;
        if (pt[sizeof(uint32_t) - 1] & 0x80) valu = (uint32_t)0 - 1; else valu = 0;
        for (unsigned i = sizeof(uint32_t); i > 0; i--)
            valu = (valu << 8) | pt[i - 1];
        PolyWord wVal = PolyWord::FromUnsigned((POLYUNSIGNED)valu);
        if (valu == 0 || wVal.IsTagged())
            return 0;
        else return wVal.AsObjPtr();
    }
#endif
    default:
        ASSERT(false);
        return 0;
    }
}

// Store a constant value.  Also used with a patch table when importing a saved heap which has
// been exported using the C exporter.
void ScanAddress::SetConstantValue(byte *addressOfConstant, PolyObject *p, ScanRelocationKind code)
{
    MemSpace* space = gMem.SpaceForAddress(addressOfConstant);
    byte* addressToWrite = space->writeAble(addressOfConstant);
    switch (code)
    {
    case PROCESS_RELOC_DIRECT: // Absolute address
        {
            uintptr_t valu = (uintptr_t)p;
            for (unsigned i = 0; i < sizeof(uintptr_t); i++)
            {
                addressToWrite[i] = (byte)(valu & 255);
                valu >>= 8;
            }
        }
        break;
    case PROCESS_RELOC_I386RELATIVE:         // 32 bit relative address
        {
            // This offset may be positive or negative
            intptr_t newDisp = (byte*)p - addressOfConstant - 4;
#if (SIZEOF_VOIDP != 4)
            ASSERT(newDisp < (intptr_t)0x80000000 && newDisp >= -(intptr_t)0x80000000);
#endif
            for (unsigned i = 0; i < 4; i++) {
                addressToWrite[i] = (byte)(newDisp & 0xff);
                newDisp >>= 8;
            }
            // When we have shifted it 32-bits the result there should
            // be no significant bits left.
            ASSERT(newDisp == 0 || newDisp == -1);
        }
        break;
    case PROCESS_RELOC_ARM64ADRPLDR64:
    case PROCESS_RELOC_ARM64ADRPLDR32:
    case PROCESS_RELOC_ARM64ADRPADD:
    {
            // This is a pair of instructions.
            uint32_t* pt = (uint32_t*)addressOfConstant;
            uint32_t instr0 = fromARMInstr(pt[0]), instr1 = fromARMInstr(pt[1]);
            int scale = code == PROCESS_RELOC_ARM64ADRPLDR64 ? 8 : code == PROCESS_RELOC_ARM64ADRPLDR32 ? 4 : 1;
            intptr_t target = (intptr_t)p;
            // LDR: The offset we put in here is a number of 8-byte words relative to
            // the 4k-page.
            uint32_t* ptW = (uint32_t*)addressToWrite;
            ptW[1] = toARMInstr((instr1 & 0xffc003ff) | (((target & 0xfff) / scale) << 10));
            // ADRP - 4k page address relative to the instruction.
            intptr_t disp = (target >> 12) - ((intptr_t)addressOfConstant >> 12);
            ptW[0] = toARMInstr((instr0 & 0x9f00001f) | ((disp & 3) << 29) | (((disp >> 2) & 0x7ffff) << 5));
        break;
    }
#ifdef POLYML32IN64
    case PROCESS_RELOC_C32ADDR:
    {
        PolyWord pw(p); // Convert address to object reference
        uint32_t valu = (uint32_t)pw.AsUnsigned();
        for (unsigned i = 0; i < sizeof(uint32_t); i++)
        {
            addressToWrite[i] = (byte)(valu & 255);
            valu >>= 8;
        }
        break;
    }
#endif
    default:
        ASSERT(false);
    }
}

void ScanAddress::ScanConstant(PolyObject *base, byte *addressOfConstant, ScanRelocationKind code, intptr_t displacement)
{
    PolyObject *p = GetConstantValue(addressOfConstant, code, displacement);

    if (p != 0)
    {
        // If this is relative and the displacement is non-zero i.e. the
        // constant itself has moved, we may have to update it
        // even if it actually goes to the same place.
        PolyObject *oldValue = GetConstantValue(addressOfConstant, code, 0);
        // If this was a relative address we must have a code address.
        if (code == PROCESS_RELOC_I386RELATIVE)
            ScanCodeAddressAt(&p);
        else p = ScanObjectAddress(p);
        if (p != oldValue) // Update it if it has changed.
            SetConstantValue(addressOfConstant, p, code);
    }
}

void ScanAddress::ScanRuntimeWord(PolyWord *w)
{
    if (w->IsTagged()) {} // Don't need to do anything
    else {
        ASSERT(w->IsDataPtr());
        *w = ScanObjectAddress(w->AsObjPtr()); 
    }
}
