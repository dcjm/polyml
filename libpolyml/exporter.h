/*
    Title:  exporter.h - Export a function as an object or C file

    Copyright (c) 2006, 2015-16 David C.J. Matthews

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

#ifndef EXPORTER_H_INCLUDED
#define EXPORTER_H_INCLUDED

#include "globals.h" // For PolyWord
#include "../polyexports.h" // For struct _memTableEntry

#ifdef HAVE_STDIO_H
#include <stdio.h> // For FILE
#endif

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class TaskData;

extern Handle exportNative(TaskData *mdTaskData, Handle args);
extern Handle exportPortable(TaskData *mdTaskData, Handle args);

// This is the base class for the exporters for the various object-code formats.
class Exporter
{
public:
    Exporter(unsigned int h=0);
    virtual ~Exporter();
    virtual void exportStore(void) = 0;

    // Called by the root thread to do the work.
    void RunExport(PolyObject *rootFunction);


protected:
    virtual PolyWord createRelocation(PolyWord p, void *relocAddr) = 0;
    void relocateValue(PolyWord *pt);
    void relocateObject(PolyObject *p);
    void createRelocation(PolyWord *pt) { *pt = createRelocation(*pt, pt); }
    unsigned findArea(void *p); // Find index of area that address is in.
    virtual void addExternalReference(void *p, const char *entryPoint) {}

public:
    FILE     *exportFile;
    const char *errorMessage;

protected:
    unsigned int hierarchy;
    struct _memTableEntry *memTable;
    unsigned memTableEntries;
    unsigned ioMemEntry; // The index corresponding to the IO area.
    unsigned ioSpacing;
    PolyObject *rootFunction; // Address of the root function.
    unsigned newAreas;
};

// The object-code exporters all use a similar string table format
// consisting of null-terminated C-strings.
class ExportStringTable
{
public:
    ExportStringTable();
    ~ExportStringTable();
    unsigned long makeEntry(const char *str);

    char *strings;
    unsigned long stringSize, stringAvailable;
};

#include "scanaddrs.h"

// Because permanent immutable areas are read-only we need to
// have somewhere else to hold the tomb-stones.
class GraveYard {
public:
    GraveYard() { graves = 0; }
    ~GraveYard();
    PolyWord *graves;
    PolyWord *startAddr, *endAddr;
};

class CopyScan: public ScanAddress
{
public:
    CopyScan(unsigned h=0);
    void initialise(bool isExport=true);
    ~CopyScan();
protected:
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt);
public:
    virtual PolyObject *ScanObjectAddress(PolyObject *base);

    // Default sizes of the segments.
    POLYUNSIGNED defaultImmSize, defaultCodeSize, defaultMutSize, defaultNoOverSize;
    unsigned hierarchy;

    GraveYard *graveYard;
    unsigned tombs;
};

extern struct _entrypts exporterEPT[];

#endif
