/*
    Title:     Export memory as a PE/COFF object
    Author:    David C. J. Matthews.

    Copyright (c) 2006, 2013, 2016 David C. J. Matthews


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
    virtual void ScanConstant(PolyObject *base, byte *addrOfConst, ScanRelocationKind code);

    // At the moment we should only get calls to ScanConstant.
    virtual PolyObject *ScanObjectAddress(PolyObject *base) { return base; }
    void alignFile(int align);
    virtual void addExternalReference(void *addr, const char *name);

private:
    void setRelocationAddress(void *p, DWORD *reloc);
    PolyWord createRelocation(PolyWord p, void *relocAddr);
    void writeSymbol(const char *symbolName, __int32 value, int section, bool isExtern, int symType=0);

    unsigned relocationCount;

    ExportStringTable stringTable;

    // Table and count for external references.
    ExportStringTable externTable;
    unsigned symbolNum;
};

#endif
