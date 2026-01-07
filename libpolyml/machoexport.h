/*
    Title:     Export memory as a Mach object file
    Author:    David C. J. Matthews.

    Copyright (c) 2006, 2016-18, 2020 David C. J. Matthews


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

#ifndef MachExport_H_INCLUDED
#define MachExport_H_INCLUDED

#include "config.h"

#include "scanaddrs.h" // For base class
#include "exporter.h"
#include <mach-o/reloc.h>


class MachoExport: public Exporter, public ScanAddress
{
public:
    MachoExport(): relocationCount(0), symbolNum(0) {}
public:
    virtual void exportStore(void);

private:
    // ScanAddress overrides
    virtual void ScanConstant(PolyObject *base, byte *addrOfConst, ScanRelocationKind code, intptr_t displacement);
    // At the moment we should only get calls to ScanConstant.
    virtual PolyObject *ScanObjectAddress(PolyObject *base) { return base; }
    virtual void addExternalReference(void *addr, const char *name, bool isFuncPtr);
    virtual void RelocateOnly(PolyObject* base, byte* addressOfConstant, ScanRelocationKind code)
    {
        ScanConstant(base, addressOfConstant, code, 0);
    }

private:
    void setRelocationAddress(void *p, int32_t *reloc);
    PolyWord createRelocation(PolyWord p, void *relocAddr);
    PolyWord writeRelocation(POLYUNSIGNED offset, void *relocAddr, unsigned symbolNumber, bool isExtern);
    void alignFile(int align);
    void createStructsRelocation(unsigned area, size_t offset);
    void adjustOffset(unsigned area, size_t &offset);

    unsigned relocationCount;
    ExportStringTable stringTable;

    // Table and count for external references.
    ExportStringTable externTable;
    unsigned symbolNum;
};

#endif
