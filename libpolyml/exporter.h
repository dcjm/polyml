/*
    Title:  exporter.h - Export a function as an object or C file

    Copyright (c) 2006, 2015-17, 2020-21, 2025 David C.J. Matthews

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

#include "globals.h" // For PolyWord and also int types
#include "memmgr.h" // For ModuleId

#ifdef HAVE_STDIO_H
#include <stdio.h> // For FILE
#endif

#include <map>

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class TaskData;

extern Handle exportNative(TaskData *mdTaskData, Handle args);
extern Handle exportPortable(TaskData *mdTaskData, Handle args);

class ExportMemTable
{
public:
    ExportMemTable() : mtCurrentAddr(0), mtOriginalAddr(0), mtLength(0), 
        mtFlags(0), mtIndex(0) {
    }

    // This previously used the memTable in polyexports.h
    void* mtCurrentAddr;             // The address of the area of memory
    void* mtOriginalAddr;            // The original address, for saved states and 32-in-64.
    uintptr_t mtLength;              // The length in bytes of the area
    unsigned mtFlags;               // Flags describing the area.
    unsigned mtIndex;               // An index to identify permanent spaces.
    ModuleId mtModId;               // The source module
};

// This is the base class for the exporters for the various object-code formats.
class Exporter
{
public:
    Exporter();
    virtual ~Exporter();
    virtual void exportStore(void) = 0;

    // Called by the root thread to do the work.
    void RunExport(PolyObject *rootFunction);

    ModuleId getExportId() const { return exportModId; }

protected:
    virtual PolyWord createRelocation(PolyWord p, void *relocAddr) = 0;
    virtual void relocateValue(PolyWord *pt); // Overridden in ModuleExport
    virtual void relocateObject(PolyObject *p); // Overridden in ModuleExport
    void createRelocation(PolyWord *pt);
    unsigned findArea(void *p); // Find index of area that address is in.
    virtual void addExternalReference(void *p, const char *entryPoint, bool isFuncPtr) {}

    // Helper functions for subclasses.  These set errNumber and errorMessage if the
    // system calls fail.
    bool checkedFwrite(const void* buffer, size_t size, size_t count);

public:
    FILE     *exportFile;
    const char *errorMessage;
    int errNumber;

protected:
    ExportMemTable *memTable;
    unsigned memTableEntries;
    PolyObject *rootFunction; // Address of the root function.
    ModuleId exportModId;
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

class PermanentMemSpace;

class CopyScan: public ScanAddress
{
public:
    CopyScan(bool isExp=false);
    void initialise();
    ~CopyScan();
protected:
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt);
    // Have to follow pointers from closures into code.
    virtual POLYUNSIGNED ScanCodeAddressAt(PolyObject **pt);
    POLYUNSIGNED ScanAddress(PolyObject **pt);
private:
    enum _newAddrType {
        NAWord,
        NAMutable,
        NANoOverwriteMutable,
        NAByte,
        NACode,
        NACodeConst
    };

    PolyObject* newAddressForObject(POLYUNSIGNED words, enum _newAddrType naType);

    bool isExport;

public:
    virtual PolyObject *ScanObjectAddress(PolyObject *base);

private:
    // Default sizes of the segments.
    uintptr_t defaultImmSize, defaultCodeSize, defaultMutSize, defaultNoOverSize;

    GraveYard *graveYard;
    unsigned tombs;

public:
    // The space Ids for spaces that are dependencies.
    // This is empty when exporting to an object module since everything must be included.
    std::map<ModuleId, bool> dependencies;

private:
    // Local values for hash computation
    uint32_t hash_a, hash_b, hash_c;
    unsigned hash_posn;
    void addToHash(uint32_t v);
    void addWordToHash(POLYUNSIGNED p);
    static uint32_t sequenceNo;
public:
    struct _moduleId extractHash();
    // Restore forwarding pointers on permanent areas after copying
    static void fixPermanentAreas();
    // Restore forwarding pointers on local areas after copying.
    // This is done when exporting to object files or after a fault.
    // The alternative, with saved states and module, is to change references
    // to local cells so that they are moved into the permanent area.
    static void revertToLocal();
};

// This is a version of Exporter that is used to create relocations for both the
// state saver and module exporter.
class StateExport : public Exporter, public ScanAddress
{
public:
    StateExport() : relocationCount(0) {}
public:
    virtual void exportStore(void) override {} // Not used.

protected:
    // These have to be overridden to work properly for compact 32-bit
    virtual void relocateValue(PolyWord* pt) override;
    virtual void relocateObject(PolyObject* p) override;

private:
    // ScanAddress overrides
    virtual void ScanConstant(PolyObject* base, byte* addrOfConst, ScanRelocationKind code, intptr_t displacement) override;
    // At the moment we should only get calls to ScanConstant.
    virtual PolyObject* ScanObjectAddress(PolyObject* base) override { return base; }

protected:
    void setRelocationAddress(void* p, POLYUNSIGNED* reloc);
    PolyWord createRelocation(PolyWord p, void* relocAddr) override; // Override for Exporter
    virtual void createActualRelocation(void* addr, void* relocAddr, ScanRelocationKind kind) = 0;
    unsigned relocationCount;
};

extern struct _entrypts exporterEPT[];

#endif
