/*
    Title:     Export memory as a PE/COFF object
    Author:    David C. J. Matthews.

    Copyright (c) 2006, 2013, 2016, 2020-1 David C. J. Matthews


    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR H PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef PECOFFEXPORT_H_INCLUDED
#define PECOFFEXPORT_H_INCLUDED

#include "scanaddrs.h" // For base class
#include "exporter.h"

#include <windows.h>
#include <winnt.h>

class PECOFFExport: public Exporter, public ScanAddress
{
public:
    PECOFFExport(): relocationCount(0), symbolNum(0) {}
public:
    virtual void exportStore(void);

private:
    // ScanAddress overrides
    virtual void ScanConstant(PolyObject *base, byte *addrOfConst, ScanRelocationKind code, intptr_t displacement);

    // At the moment we should only get calls to ScanConstant.
    virtual PolyObject *ScanObjectAddress(PolyObject *base) { return base; }
    void alignFile(int align);
    virtual void addExternalReference(void *addr, const char *name, bool isFuncPtr);
    virtual void RelocateOnly(PolyObject* base, byte* addressOfConstant, ScanRelocationKind code)
    {
        ScanConstant(base, addressOfConstant, code, 0);
    }

private:
    // Set the symbol in the relocation to the symbol for the target address
    // and return the offset relative to that symbol.
    POLYUNSIGNED setSymbolAndGetOffset(void* p, IMAGE_RELOCATION* reloc);
    // Set the VirtualAddrss field to the offset within the current segment
    // where the relocation must be applied.
    void setRelocationAddress(void *p, IMAGE_RELOCATION* reloc);
    PolyWord createRelocation(PolyWord p, void *relocAddr);
    void writeSymbol(const char *symbolName, __int32 value, int section, bool isExtern, int symType=0);

    void writeRelocation(const IMAGE_RELOCATION* reloc);
    unsigned relocationCount;

    ExportStringTable stringTable;

    // Table and count for external references.
    ExportStringTable externTable;
    unsigned symbolNum;

    // Copy of the first relocation in case we
    // have to overwrite it.
    IMAGE_RELOCATION firstRelocation;
};

#endif
