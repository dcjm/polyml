/*
    Title:  scanaddrs.h - Scan addresses in objects

    Copyright (c) 2006-8, 2012, 2015, 2018, 2021, 2025 David C.J. Matthews

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

#ifndef SCANADDRS_H_INCLUDED
#define SCANADDRS_H_INCLUDED

#include "globals.h"

// Type of relocations.
typedef enum {
    PROCESS_RELOC_DIRECT = 0,           // 32 or 64 bit address of target
    PROCESS_RELOC_I386RELATIVE,         // 32 or 64 bit relative address
    PROCESS_RELOC_ARM64ADRPLDR64,       // Relative address for ADRP/LDR (64-bit) pair
    PROCESS_RELOC_ARM64ADRPLDR32,       // Relative address for ADRP/LDR (32-bit) pair
    PROCESS_RELOC_ARM64ADRPADD,         // Relative address for ADRP/ADD pair
    // The relocations above have corresponding object module relocation types.
    // The next one does not and is only used within Poly/ML modules.
    PROCESS_RELOC_C32ADDR               // Compact 32-bit object identifier
} ScanRelocationKind;

class StackSpace;

class ScanAddress {
public:
    virtual ~ScanAddress() {} // Keeps gcc happy

protected:
    // Scan an address in the memory.  "pt" always points into an object.
    // It is not called with pt pointing at a C++ automatic variable.
    // Tagged integers have already been filtered out.
    // The result is the length word of the object to use if the object
    // is to be processed recursively or 0 if it should not be.
    // Default action - call ScanObjectAddress for the base object address of
    // the address.
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt);

    // As for ScanAddressAt except that the value is a pointer to the first word in a closure object.
    // In most cases we're just scanning the heap we don't need to do anything and we scan
    // the code area separately.
    virtual POLYUNSIGNED ScanCodeAddressAt(PolyObject **pt) { return 0; }

public:
    // The fundamental overridable for this class.  Takes the object address and returns
    // the updated address.  If nothing else is overridden everything eventually comes here.
    virtual PolyObject *ScanObjectAddress(PolyObject *base) = 0;// { return base; }

    typedef enum { STRENGTH_STRONG = 0, STRENGTH_WEAK = 1 } RtsStrength;

    // Scan an address in the run-time system.  This normally just applies ScanObjectAddress
    // but if this is a weak reference it can set *pt to NULL
    virtual void ScanRuntimeAddress(PolyObject **pt, RtsStrength weak)
        { *pt = ScanObjectAddress(*pt); }

    // Scan a word in the run-time system.  This is the preferred call for non-weak
    // references and deals with the general case of a word.
    void ScanRuntimeWord(PolyWord *w);

    // Process a constant within the code.
    // The default action is to call the DEFAULT ScanAddressAt NOT the virtual which means that it calls
    // ScanObjectAddress for the base address of the object referred to.
    // "displacement" is only used for relative addresses and is only non-zero when the code
    // has been moved.
    virtual void ScanConstant(PolyObject *base, byte *addressOfConstant, ScanRelocationKind code, intptr_t displacement=0);

    // Scan the objects in the region and process their addresses.  Applies ScanAddressesInObject
    // to each of the objects.  The "region" argument points AT the first length word.
    // Typically used to scan or update addresses in the mutable area.
    void ScanAddressesInRegion(PolyWord *region, PolyWord *endOfRegion);

    // General object processor.
    // If the object is a word object calls ScanAddressesAt for all the addresses.
    //
    // If the object is a code object calls ScanAddressesAt for the constant area and
    // calls (indirectly) ScanConstant, and by default ScanObjectAddress for addresses within
    // the code
    //
    // If the object is a stack calls ScanStackAddress which calls ScanObjectAddress for
    // addresses within the code.
    virtual void ScanAddressesInObject(PolyObject *base, POLYUNSIGNED lengthWord);

    void ScanAddressesInObject(PolyObject *base) { ScanAddressesInObject(base, base->LengthWord()); }

    // Create a relocation but don't adjust the address.  This is used for offsets from the
    // code area to the constant area.
    virtual void RelocateOnly(PolyObject* base, byte* addressOfConstant, ScanRelocationKind code) {}

    // Extract a constant from the code.
    static PolyObject *GetConstantValue(byte *addressOfConstant, ScanRelocationKind code, intptr_t displacement);

    // Store a constant in the code.
    static void SetConstantValue(byte *addressOfConstant, PolyObject *p, ScanRelocationKind code);
};

#endif
