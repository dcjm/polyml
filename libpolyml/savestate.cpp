/*
    Title:  savestate.cpp - Save and Load state

    Copyright (c) 2007, 2015, 2017-19, 2021, 2025 David C.J. Matthews

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

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_WINDOWS_H
#include <windows.h> // For MAX_PATH
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h> // For MAX_PATH
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#include <string>

#if (defined(_WIN32))
#include <tchar.h>
#define ERRORNUMBER _doserrno
#define NOMEMORY ERROR_NOT_ENOUGH_MEMORY
#ifdef _UNICODE
#define std_tstring std::wstring
#else
#define std_tstring std::string
#endif
#else
typedef char TCHAR;
#define _T(x) x
#define _tfopen fopen
#define _tcscpy strcpy
#define _tcsdup strdup
#define _tcslen strlen
#define _fputtc fputc
#define _fputts fputs
#ifndef lstrcmpi
#define lstrcmpi strcasecmp
#endif
#define ERRORNUMBER errno
#define NOMEMORY ENOMEM
#define std_tstring std::string
#endif


#include "globals.h"
#include "savestate.h"
#include "processes.h"
#include "run_time.h"
#include "polystring.h"
#include "scanaddrs.h"
#include "arb.h"
#include "memmgr.h"
#include "mpoly.h" // For exportSignature
#include "exporter.h" // For CopyScan
#include "machine_dep.h"
#include "osmem.h"
#include "gc.h" // For FullGC.
#include "timing.h"
#include "rtsentry.h"
#include "check_objects.h"
#include "rtsentry.h"

#ifdef _MSC_VER
// Don't tell me about ISO C++ changes.
#pragma warning(disable:4996)
#endif

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySaveState(POLYUNSIGNED threadId, POLYUNSIGNED fileName, POLYUNSIGNED depth);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyLoadState(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyShowHierarchy(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyRenameParent(POLYUNSIGNED threadId, POLYUNSIGNED childName, POLYUNSIGNED parentName);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyShowParent(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyLoadHierarchy(POLYUNSIGNED threadId, POLYUNSIGNED arg);
}

// This is probably generally useful so may be moved into
// a general header file.
template<typename BASE> class AutoFree
{
public:
    AutoFree(BASE p = 0) : m_value(p) {}
    ~AutoFree() { free(m_value); }

    // Automatic conversions to the base type.
    operator BASE() { return m_value; }
    BASE operator = (BASE p) { return (m_value = p); }

private:
    BASE m_value;
};

#ifdef HAVE__FTELLI64
// fseek and ftell are only 32-bits in Windows.
#define off_t   __int64
#define fseek _fseeki64
#define ftell _ftelli64
#endif

/*
 *  Structure definitions for the saved state files.
 */

#define SAVEDSTATESIGNATURE "POLYSAVE"
#define SAVEDSTATEVERSION   3

// File header for a saved state file.  This appears as the first entry
// in the file.
typedef struct _savedStateHeader
{
    // These entries are primarily to check that we have a valid
    // saved state file before we try to interpret anything else.
    char        headerSignature[8];     // Should contain SAVEDSTATESIGNATURE
    unsigned    headerVersion;          // Should contain SAVEDSTATEVERSION
    unsigned    headerLength;           // Number of bytes in the header
    unsigned    segmentDescrLength;     // Number of bytes in a descriptor

    // These entries contain the real data.
    off_t       segmentDescr;           // Position of segment descriptor table
    unsigned    segmentDescrCount;      // Number of segment descriptors in the table
    off_t       stringTable;            // Pointer to the string table (zero if none)
    size_t      stringTableSize;        // Size of string table
    unsigned    parentNameEntry;        // Position of parent name in string table (0 if top)
    struct _moduleId      timeStamp;            // The time stamp for this file.
    struct _moduleId      parentTimeStamp;      // The time stamp for the parent.
    void       *originalBaseAddr;        // Original base address (32-in-64 only)
} SavedStateHeader;

// Entry for segment table.  This describes the segments on the disc that
// need to be loaded into memory.
typedef struct _savedStateSegmentDescr
{
    off_t       segmentData;            // Position of the segment data
    size_t      segmentSize;            // Size of the segment data
    off_t       relocations;            // Position of the relocation table
    unsigned    relocationCount;        // Number of entries in relocation table
    unsigned    relocationSize;         // Size of a relocation entry
    unsigned    segmentFlags;           // Segment flags (see SSF_ values)
    unsigned    segmentIndex;           // The index of this segment or the segment it overwrites
    struct _moduleId      moduleId;               // The module this came from.
} SavedStateSegmentDescr;

#define SSF_WRITABLE    1               // The segment contains mutable data
#define SSF_OVERWRITE   2               // The segment overwrites the data (mutable) in a parent.
#define SSF_NOOVERWRITE 4               // The segment must not be further overwritten
#define SSF_BYTES       8               // The segment contains only byte data
#define SSF_CODE        16              // The segment contains only code

typedef struct _relocationEntry
{
    // Each entry indicates a location that has to be set to an address.
    // The location to be set is determined by adding "relocAddress" to the base address of
    // this segment (the one to which these relocations apply) and the value to store
    // by adding "targetAddress" to the base address of the segment indicated by targetIndex/targetModId.
    POLYUNSIGNED    relocAddress;       // The (byte) offset in this segment that we will set
    POLYUNSIGNED    targetAddress;      // The value to add to the base of the destination segment
    unsigned        targetSegment;      // Index into the address table
    ScanRelocationKind relKind;         // The kind of relocation (processor dependent).
} RelocationEntry;

#define SAVE(x) taskData->saveVec.push(x)

/*
 *  Hierarchy table: contains information about last loaded or saved state.
 */

// Pointer to list of files loaded in last load.
// There's no need for a lock since the update is only made when all
// the ML threads have stopped.
class HierarchyTable
{
public:
    HierarchyTable(const TCHAR *file, struct _moduleId time):
      fileName(_tcsdup(file)), timeStamp(time) { }

    HierarchyTable() {}

    std_tstring         fileName;
    struct _moduleId    timeStamp;
};

std::vector <HierarchyTable> hierarchyTable;

// Test whether we're overwriting a parent of ourself.
#if (defined(_WIN32) || defined(__CYGWIN__))
static bool sameFile(const TCHAR *x, const TCHAR *y)
{
    HANDLE hXFile = INVALID_HANDLE_VALUE, hYFile = INVALID_HANDLE_VALUE;
    bool result = false;

    hXFile = CreateFile(x, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (hXFile == INVALID_HANDLE_VALUE) goto closeAndExit;
    hYFile = CreateFile(y, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (hYFile == INVALID_HANDLE_VALUE) goto closeAndExit;
    BY_HANDLE_FILE_INFORMATION fileInfoX, fileInfoY;
    if (! GetFileInformationByHandle(hXFile, &fileInfoX)) goto closeAndExit;
    if (! GetFileInformationByHandle(hYFile, &fileInfoY)) goto closeAndExit;

    result = fileInfoX.dwVolumeSerialNumber == fileInfoY.dwVolumeSerialNumber &&
        fileInfoX.nFileIndexLow == fileInfoY.nFileIndexLow &&
        fileInfoX.nFileIndexHigh == fileInfoY.nFileIndexHigh;

closeAndExit:
    if (hXFile != INVALID_HANDLE_VALUE) CloseHandle(hXFile);
    if (hYFile != INVALID_HANDLE_VALUE) CloseHandle(hYFile);
    return result;
}
#else
static bool sameFile(const char *x, const char *y)
{
    struct stat xStat, yStat;
    // If either file does not exist that's fine.
    if (stat(x, &xStat) != 0 || stat(y, &yStat) != 0)
        return false;
    return (xStat.st_dev == yStat.st_dev && xStat.st_ino == yStat.st_ino);
}
#endif

/*
 *  Saving state.
 */

 // Request to the main thread to save data.
class SaveRequest : public MainThreadRequest, public StateExport
{
public:
    SaveRequest(const TCHAR* name, unsigned h) : MainThreadRequest(MTP_SAVESTATE),
        fileName(name), newHierarchy(h), relocationCount(0) {
    }

    virtual void Perform();
    const TCHAR* fileName;
    unsigned newHierarchy;

public:
    virtual void exportStore(void) {} // Not used.

protected:
    void createActualRelocation(void* addr, void* relocAddr, ScanRelocationKind kind);
    unsigned relocationCount;
};

// Create a relocation entry for an address at a given location.
// This is currently never called in compact 32-bit mode because Exporter::relocateValue does
// not call createRelocation in 32-in-64 mode.
void SaveRequest::createActualRelocation(void *addr, void* relocAddr, ScanRelocationKind kind)
{
    RelocationEntry reloc;
    // Set the offset within the section we're scanning.
    setRelocationAddress(relocAddr, &reloc.relocAddress);

    unsigned addrArea = findArea(addr);
    reloc.targetAddress = (POLYUNSIGNED)((char*)addr - (char*)memTable[addrArea].mtOriginalAddr);
    reloc.targetSegment = addrArea;
    reloc.relKind = kind;
    fwrite(&reloc, sizeof(reloc), 1, exportFile);
    relocationCount++;
}

// This class is used to update references to objects that have moved.  If
// we have copied an object into the area to be exported we may still have references
// to it from the stack or from RTS data structures.  We have to ensure that these
// are updated.
// This is very similar to ProcessFixupAddress in sharedata.cpp
class SaveFixupAddress: public ScanAddress
{
protected:
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt);
    virtual POLYUNSIGNED ScanCodeAddressAt(PolyObject **pt) { *pt = ScanObjectAddress(*pt); return 0; }
    virtual PolyObject *ScanObjectAddress(PolyObject *base);

public:
    void ScanCodeSpace(CodeSpace *space);
};


POLYUNSIGNED SaveFixupAddress::ScanAddressAt(PolyWord *pt)
{
    PolyWord val = *pt;
    if (val.IsDataPtr() && val != PolyWord::FromUnsigned(0))
        *pt = ScanObjectAddress(val.AsObjPtr());
    return 0;
}

// Returns the new address if the argument is the address of an object that
// has moved, otherwise returns the original.
PolyObject *SaveFixupAddress::ScanObjectAddress(PolyObject *obj)
{
    if (obj->ContainsForwardingPtr()) // tombstone is a pointer to a moved object
    {
#ifdef POLYML32IN64
        MemSpace *space = gMem.SpaceForAddress((PolyWord*)obj - 1);
        PolyObject *newp;
        if (space->isCode)
            newp = (PolyObject*)(globalCodeBase + (((uintptr_t)(obj->LengthWord()) & ~_OBJ_TOMBSTONE_BIT) * POLYML32IN64));
        else newp = obj->GetForwardingPtr();
#else
        PolyObject *newp = obj->GetForwardingPtr();
#endif
        ASSERT (newp->ContainsNormalLengthWord());
        return newp;
    }
    
    ASSERT (obj->ContainsNormalLengthWord()); // object is not moved
    return obj;
}

// Fix up addresses in the code area.  Unlike ScanAddressesInRegion this updates
// cells that have been moved.  We need to do that because we may still have
// return addresses into those cells and we don't move return addresses.  We
// do want the code to see updated constant addresses.
void SaveFixupAddress::ScanCodeSpace(CodeSpace *space)
{
    for (PolyWord *pt = space->bottom; pt < space->top; )
    {
        pt++;
        PolyObject *obj = (PolyObject*)pt;
#ifdef POLYML32IN64
        PolyObject *dest = obj;
        while (dest->ContainsForwardingPtr())
        {
            MemSpace *space = gMem.SpaceForObjectAddress(dest);
            if (space->isCode)
                dest = (PolyObject*)(globalCodeBase + (((uintptr_t)(dest->LengthWord()) & ~_OBJ_TOMBSTONE_BIT) * POLYML32IN64));
            else dest = dest->GetForwardingPtr();
        }
#else
        PolyObject *dest = obj->FollowForwardingChain();
#endif
        POLYUNSIGNED length = dest->Length();
        if (length != 0)
            ScanAddressesInObject(obj, dest->LengthWord());
        pt += length;
    }
}

// Exported function also used by the modules system.
void switchLocalsToPermanent()
{
    // Update references to moved objects.
    SaveFixupAddress fixup;
    for (std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
    {
        LocalMemSpace* space = *i;
        fixup.ScanAddressesInRegion(space->bottom, space->lowerAllocPtr);
        fixup.ScanAddressesInRegion(space->upperAllocPtr, space->top);
    }
    for (std::vector<CodeSpace*>::iterator i = gMem.cSpaces.begin(); i < gMem.cSpaces.end(); i++)
        fixup.ScanCodeSpace(*i);

    GCModules(&fixup);

    // Restore the length words in the code areas.
    // Although we've updated any pointers to the start of the code we could have return addresses
    // pointing to the original code.  GCModules updates the stack but doesn't update return addresses.
    for (std::vector<CodeSpace*>::iterator i = gMem.cSpaces.begin(); i < gMem.cSpaces.end(); i++)
    {
        CodeSpace* space = *i;
        for (PolyWord* pt = space->bottom; pt < space->top; )
        {
            pt++;
            PolyObject* obj = (PolyObject*)pt;
            if (obj->ContainsForwardingPtr())
            {
#ifdef POLYML32IN64
                PolyObject* forwardedTo = obj;
                while (forwardedTo->ContainsForwardingPtr())
                    forwardedTo = (PolyObject*)(globalCodeBase + (((uintptr_t)(forwardedTo->LengthWord()) & ~_OBJ_TOMBSTONE_BIT) * POLYML32IN64));
#else
                PolyObject* forwardedTo = obj->FollowForwardingChain();
#endif
                POLYUNSIGNED lengthWord = forwardedTo->LengthWord();
                space->writeAble(obj)->SetLengthWord(lengthWord);
            }
            pt += obj->Length();
        }
    }
}

// Called by the root thread to actually save the state and write the file.
void SaveRequest::Perform()
{
    try {
        if (debugOptions & DEBUG_SAVING)
            Log("SAVE: Beginning saving state.\n");
        // Check that we aren't overwriting our own parent.
        for (unsigned q = 0; q < newHierarchy - 1; q++) {
            if (sameFile(hierarchyTable[q].fileName.c_str(), fileName))
            {
                errorMessage = "File being saved is used as a parent of this file";
                errNumber = 0;
                if (debugOptions & DEBUG_SAVING)
                    Log("SAVE: File being saved is used as a parent of this file.\n");
                return;
            }
        }

        // Open the file.  This could quite reasonably fail if the path is wrong.
        exportFile = _tfopen(fileName, _T("wb"));
        if (exportFile == NULL)
        {
            errorMessage = "Cannot open save file";
            errNumber = ERRORNUMBER;
            if (debugOptions & DEBUG_SAVING)
                Log("SAVE: Cannot open save file.\n");
            return;
        }

        // Scan over the permanent mutable area copying all reachable data that is
        // not in a lower hierarchy into new permanent segments.
        CopyScan copyScan;
        // Set the dependencies.
        copyScan.dependencies[exportSignature] = true; // Always include the executable
        // Include any existing states at a higher level
        for (unsigned i = 0; i < newHierarchy - 1; i++)
            copyScan.dependencies[hierarchyTable[i].timeStamp] = true;
        copyScan.initialise();

        bool success = true;
        try {
            for (std::vector<PermanentMemSpace*>::iterator i = gMem.pSpaces.begin(); i < gMem.pSpaces.end(); i++)
            {
                PermanentMemSpace* space = *i;
                if (space->isMutable && !space->noOverwrite && !space->byteOnly)
                {
                    if (debugOptions & DEBUG_SAVING)
                        Log("SAVE: Scanning permanent mutable area %p allocated at %p size %lu\n",
                            space, space->bottom, space->spaceSize());
                    copyScan.ScanAddressesInRegion(space->bottom, space->top);
                }
            }
            // We may have copied data out of modules.  Restore forwarding pointers in them.
            CopyScan::fixPermanentAreas();
        }
        catch (MemoryException&)
        {
            success = false;
            if (debugOptions & DEBUG_SAVING)
                Log("SAVE: Scan of permanent mutable area raised memory exception.\n");
        }

        // Copy the areas into the export object.  Make sufficient space for
        // the largest possible number of entries.
        memTable = new ExportMemTable[gMem.eSpaces.size() + gMem.pSpaces.size() + 1];
        unsigned memTableCount = 0;

        // Permanent spaces at higher level.  These have to have entries although
        // only the mutable entries will be written.
        for (std::vector<PermanentMemSpace*>::iterator i = gMem.pSpaces.begin(); i < gMem.pSpaces.end(); i++)
        {
            PermanentMemSpace* space = *i;
            if (copyScan.dependencies[space->moduleIdentifier])
            {
                ExportMemTable* entry = &memTable[memTableCount++];
                entry->mtOriginalAddr = entry->mtCurrentAddr = space->bottom;
                entry->mtLength = (space->topPointer - space->bottom) * sizeof(PolyWord);
                entry->mtIndex = space->index;
                entry->mtFlags = 0;
                entry->mtModId = space->moduleIdentifier;
                if (space->isMutable)
                {
                    entry->mtFlags |= MTF_WRITEABLE;
                    if (space->noOverwrite) entry->mtFlags |= MTF_NO_OVERWRITE;
                    if (space->byteOnly) entry->mtFlags |= MTF_BYTES;
                }
                if (space->isCode)
                    entry->mtFlags |= MTF_EXECUTABLE;
            }
        }
        unsigned permanentEntries = memTableCount; // Remember where new entries start.

        exportModId = copyScan.extractHash();

        // Newly created spaces.
        for (std::vector<PermanentMemSpace*>::iterator i = gMem.eSpaces.begin(); i < gMem.eSpaces.end(); i++)
        {
            ExportMemTable* entry = &memTable[memTableCount++];
            PermanentMemSpace* space = *i;
            entry->mtOriginalAddr = entry->mtCurrentAddr = space->bottom;
            entry->mtLength = (space->topPointer - space->bottom) * sizeof(PolyWord);
            entry->mtIndex = space->index;
            entry->mtFlags = 0;
            entry->mtModId = exportModId;
            if (space->isMutable)
            {
                entry->mtFlags |= MTF_WRITEABLE;
                if (space->noOverwrite) entry->mtFlags |= MTF_NO_OVERWRITE;
                if (space->byteOnly) entry->mtFlags |= MTF_BYTES;
            }
            if (space->isCode)
                entry->mtFlags |= MTF_EXECUTABLE;
        }

        memTableEntries = memTableCount;

        if (debugOptions & DEBUG_SAVING)
            Log("SAVE: Updating references to moved objects.\n");
        // We always switch to the permanent space even if there's been a
        // failure since it is possible to re-save.
        switchLocalsToPermanent();

        // Update the global memory space table.  Old segments at the same level
        // or lower are removed.  The new segments become permanent.
        // Try to promote the spaces even if we've had a failure because export
        // spaces are deleted in ~CopyScan and we may have already copied
        // some objects there.
        if (debugOptions & DEBUG_SAVING)
            Log("SAVE: Promoting export spaces to permanent spaces.\n");
        // Remove any deeper entries from the hierarchy table.
        for (unsigned i = newHierarchy - 1; i < hierarchyTable.size(); i++)
        {
            if (!gMem.DemoteOldPermanentSpaces(hierarchyTable[i].timeStamp))
            {
                errorMessage = "Out of Memory";
                errNumber = NOMEMORY;
                if (debugOptions & DEBUG_SAVING)
                    Log("SAVE: Unable to demote old spaces.\n");
            }
        }
        hierarchyTable.resize(newHierarchy - 1);

        if (!gMem.PromoteNewExportSpaces(exportModId) || !success)
        {
            errorMessage = "Out of Memory";
            errNumber = NOMEMORY;
            if (debugOptions & DEBUG_SAVING)
                Log("SAVE: Unable to promote export spaces.\n");
            return;
        }

        if (debugOptions & DEBUG_SAVING)
            Log("SAVE: Writing out data.\n");

        // TODO: We should handle write failures e.g. if the disc becomes full.

        // Write out the file header.
        SavedStateHeader saveHeader;
        memset(&saveHeader, 0, sizeof(saveHeader));
        saveHeader.headerLength = sizeof(saveHeader);
        memcpy(saveHeader.headerSignature,
            SAVEDSTATESIGNATURE, sizeof(saveHeader.headerSignature));
        saveHeader.headerVersion = SAVEDSTATEVERSION;
        saveHeader.segmentDescrLength = sizeof(SavedStateSegmentDescr);
        if (newHierarchy == 1)
            saveHeader.parentTimeStamp = exportSignature;
        else
        {
            saveHeader.parentTimeStamp = hierarchyTable[newHierarchy - 2].timeStamp;
            saveHeader.parentNameEntry = sizeof(TCHAR); // Always the first entry.
        }
        saveHeader.timeStamp = copyScan.extractHash();
        saveHeader.segmentDescrCount = memTableEntries; // One segment for each space.
#ifdef POLYML32IN64
        saveHeader.originalBaseAddr = globalHeapBase;
#endif
        // Write out the header.
        fwrite(&saveHeader, sizeof(saveHeader), 1, exportFile);

        // We need a segment header for each permanent area whether it is
        // actually in this file or not.
        std::vector<SavedStateSegmentDescr> descrs;
        descrs.reserve(memTableEntries);
        // Write out temporarily. Will be overwritten at the end.
        saveHeader.segmentDescr = ftell(exportFile);

        for (unsigned j = 0; j < memTableEntries; j++)
        {
            ExportMemTable* entry = &memTable[j];
            SavedStateSegmentDescr descr;
            memset(&descr, 0, sizeof(SavedStateSegmentDescr));
            descr.relocationSize = sizeof(RelocationEntry);
            descr.segmentIndex = (unsigned)entry->mtIndex;
            descr.segmentSize = entry->mtLength; // Set this even if we don't write it.
            descr.moduleId = entry->mtModId;
            if (entry->mtFlags & MTF_WRITEABLE)
            {
                descr.segmentFlags |= SSF_WRITABLE;
                if (entry->mtFlags & MTF_NO_OVERWRITE)
                    descr.segmentFlags |= SSF_NOOVERWRITE;
                if (j < permanentEntries && (entry->mtFlags & MTF_NO_OVERWRITE) == 0)
                    descr.segmentFlags |= SSF_OVERWRITE;
                if (entry->mtFlags & MTF_BYTES)
                    descr.segmentFlags |= SSF_BYTES;
            }
            if (entry->mtFlags & MTF_EXECUTABLE)
                descr.segmentFlags |= SSF_CODE;
            if(!checkedFwrite(&descr, sizeof(SavedStateSegmentDescr), 1))
                return;
            descrs.push_back(descr);
        }

        // Write out the relocations and the data.
        for (unsigned k = 1 /* Not IO area */; k < memTableEntries; k++)
        {
            ExportMemTable* entry = &memTable[k];
            // Write out the contents if this is new or if it is a normal, overwritable
            // mutable area.
            if (k >= permanentEntries ||
                (entry->mtFlags & (MTF_WRITEABLE | MTF_NO_OVERWRITE)) == MTF_WRITEABLE)
            {
                descrs[k].relocations = ftell(exportFile);
                // Have to write this out.
                relocationCount = 0;
                // Create the relocation table.
                char* start = (char*)entry->mtOriginalAddr;
                char* end = start + entry->mtLength;
                for (PolyWord* p = (PolyWord*)start; p < (PolyWord*)end; )
                {
                    p++;
                    PolyObject* obj = (PolyObject*)p;
                    POLYUNSIGNED length = obj->Length();
                    // Now include all relocations rather than trying to sort them out when loading.
                    if (length != 0 && obj->IsCodeObject())
                        machineDependent->ScanConstantsWithinCode(obj, this);
                    relocateObject(obj);
                    p += length;
                }
                descrs[k].relocationCount = relocationCount;
                // Write out the data.
                descrs[k].segmentData = ftell(exportFile);
                if (!checkedFwrite(entry->mtOriginalAddr, entry->mtLength, 1))
                    return;
            }
        }

        // If this is a child we need to write a string table containing the parent name.
        if (newHierarchy > 1)
        {
            saveHeader.stringTable = ftell(exportFile);
            _fputtc(0, exportFile); // First byte of string table is zero
            _fputts(hierarchyTable[newHierarchy - 2].fileName.c_str(), exportFile);
            _fputtc(0, exportFile); // A terminating null.
            saveHeader.stringTableSize = (_tcslen(hierarchyTable[newHierarchy - 2].fileName.c_str()) + 2) * sizeof(TCHAR);
        }

        if (ferror(exportFile))
        {
            errNumber = ERRORNUMBER;
            errorMessage = "Error while writing string table";
            return;
        }

        // Rewrite the header and the segment tables now they're complete.
        fseek(exportFile, 0, SEEK_SET);
        if (!checkedFwrite(&saveHeader, sizeof(saveHeader), 1))
            return;
        if (!checkedFwrite(descrs.data(), sizeof(SavedStateSegmentDescr), memTableEntries))
            return;

        if (debugOptions & DEBUG_SAVING)
            Log("SAVE: Writing complete.\n");

        // Add an entry to the hierarchy table for this file.
        hierarchyTable.push_back(HierarchyTable(fileName, saveHeader.timeStamp));

        CheckMemory();
    }

    catch (const std::bad_alloc&)
    {
        errorMessage = "Insufficient Memory: C++ allocation failed";
    }
}

// Write a saved state file.
POLYUNSIGNED PolySaveState(POLYUNSIGNED threadId, POLYUNSIGNED fileName, POLYUNSIGNED depth)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();

    try {
        TempString fileNameBuff(Poly_string_to_T_alloc(PolyWord::FromUnsigned(fileName)));
        // The value of depth is zero for top-level save so we need to add one for hierarchy.
        unsigned newHierarchy = get_C_unsigned(taskData, PolyWord::FromUnsigned(depth)) + 1;

        if (newHierarchy > hierarchyTable.size() + 1)
            raise_fail(taskData, "Depth must be no more than the current hierarchy plus one");

        // Request a full GC first.  The main reason is to avoid running out of memory as a
        // result of repeated saves.  Old export spaces are turned into local spaces and
        // the GC will delete them if they are completely empty
        FullGC(taskData);

        SaveRequest request(fileNameBuff, newHierarchy);
        processes->MakeRootRequest(taskData, &request);
        if (request.errorMessage)
            raise_syscall(taskData, request.errorMessage, request.errNumber);
    }
    catch (const std::bad_alloc&) { setMemoryException(taskData); }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

/*
 *  Loading saved state files.
 */

class StateLoader: public MainThreadRequest
{
public:
    StateLoader(bool isH, Handle files): MainThreadRequest(MTP_LOADSTATE),
        isHierarchy(isH), fileNameList(files), errorResult(0), errNumber(0) { }

    virtual void Perform(void);
    bool LoadFile(bool isInitial, ModuleId requiredStamp, PolyWord tail);
    bool isHierarchy;
    Handle fileNameList;
    const char *errorResult;
    // The fileName here is the last file loaded.  As well as using it
    // to load the name can also be printed out at the end to identify the
    // particular file in the hierarchy that failed.
    AutoFree<TCHAR*> fileName;
    int errNumber;
};

// Data needed for relocation during load.
class StateLoadData {
public:
    StateLoadData() : relocations(0), relocationCount(0), targetAddr(0) {}

    off_t       relocations;        // Copied from descriptor
    unsigned    relocationCount;
    PolyWord* targetAddr;           // Actual address of the segment
};

// Called by the main thread once all the ML threads have stopped.
void StateLoader::Perform(void)
{
    try {
        // Copy the first file name into the buffer.
        if (isHierarchy)
        {
            if (ML_Cons_Cell::IsNull(fileNameList->Word()))
            {
                errorResult = "Hierarchy list is empty";
                return;
            }
            ML_Cons_Cell* p = DEREFLISTHANDLE(fileNameList);
            fileName = Poly_string_to_T_alloc(p->h);
            if (fileName == NULL)
            {
                errorResult = "Insufficient memory";
                errNumber = NOMEMORY;
                return;
            }
            (void)LoadFile(true, ModuleId(), p->t);
        }
        else
        {
            fileName = Poly_string_to_T_alloc(fileNameList->Word());
            if (fileName == NULL)
            {
                errorResult = "Insufficient memory";
                errNumber = NOMEMORY;
                return;
            }
            (void)LoadFile(true, ModuleId(), TAGGED(0));
        }
    }
    catch (const std::bad_alloc&)
    {
        errorResult = "Insufficient Memory: C++ allocation failed";
    }
}

// Set the values of external references and clear the values of other weak byte refs.
void ClearVolatile::ScanAddressesInObject(PolyObject *base, POLYUNSIGNED lengthWord)
{
    if (OBJ_IS_MUTABLE_OBJECT(lengthWord) && OBJ_IS_NO_OVERWRITE(lengthWord))
    {
        if (OBJ_IS_BYTE_OBJECT(lengthWord))
        {
            if (OBJ_IS_WEAKREF_OBJECT(lengthWord))
            {
                POLYUNSIGNED len = OBJ_OBJECT_LENGTH(lengthWord);
                if (len >= sizeof(uintptr_t) / sizeof(PolyWord))
                    *((uintptr_t*)base) = 0;
                setEntryPoint(base);
            }
        }
        else
        {
            // Clear volatile refs
            POLYUNSIGNED len = OBJ_OBJECT_LENGTH(lengthWord);
            for (POLYUNSIGNED i = 0; i < len; i++)
                base->Set(i, TAGGED(0));
        }
    }
}

// Work around bug in Mac OS when reading into MAP_JIT memory.
size_t readData(void *ptr, size_t size, FILE *stream)
{
#ifndef MACOSX
    return fread(ptr, size, 1, stream);
#else
    char buff[1024];
    for (size_t s = 0; s < size; )
    {
        size_t unit = sizeof(buff);
        if (size - s < unit) unit = size-s;
        if (fread(buff, unit, 1, stream) != 1)
            return 0;
        memcpy((char*)ptr+s, buff, unit);
        s += unit;
    }
    return 1; // Succeeded
#endif
}

// Load a saved state file.  Calls itself to handle parent files.
bool StateLoader::LoadFile(bool isInitial, ModuleId requiredStamp, PolyWord tail)
{
    AutoFree<TCHAR*> thisFile(_tcsdup(fileName));

    AutoClose loadFile(_tfopen(fileName, _T("rb")));
    if ((FILE*)loadFile == NULL)
    {
        errorResult = "Cannot open load file";
        errNumber = ERRORNUMBER;
        return false;
    }

    SavedStateHeader header;
    // Read the header and check the signature.
    if (fread(&header, sizeof(SavedStateHeader), 1, loadFile) != 1)
    {
        errorResult = "Unable to load header";
        return false;
    }
    if (strncmp(header.headerSignature, SAVEDSTATESIGNATURE, sizeof(header.headerSignature)) != 0)
    {
        errorResult = "File is not a saved state";
        return false;
    }
    if (header.headerVersion != SAVEDSTATEVERSION ||
        header.headerLength != sizeof(SavedStateHeader) ||
        header.segmentDescrLength != sizeof(SavedStateSegmentDescr))
    {
        errorResult = "Unsupported version of saved state file";
        return false;
    }

    // Check that we have the required stamp before loading any children.
    // If a parent has been overwritten we could get a loop.
    if (! isInitial && ModuleId(header.timeStamp) != requiredStamp)
    {
        // Time-stamps don't match.
        errorResult = "The parent for this saved state does not match or has been changed";
        return false;
    }

    // Have verified that this is a reasonable saved state file.  If it isn't a
    // top-level file we have to load the parents first.
    if (header.parentNameEntry != 0)
    {
        if (isHierarchy)
        {
            // Take the file name from the list
            if (ML_Cons_Cell::IsNull(tail))
            {
                errorResult = "Missing parent name in argument list";
                return false;
            }
            ML_Cons_Cell *p = (ML_Cons_Cell *)tail.AsObjPtr();
            fileName = Poly_string_to_T_alloc(p->h);
            if (fileName == NULL)
            {
                errorResult = "Insufficient memory";
                errNumber = NOMEMORY;
                return false;
            }
            if (! LoadFile(false, header.parentTimeStamp, p->t))
                return false;
        }
        else
        {
            size_t toRead = header.stringTableSize-header.parentNameEntry;
            size_t elems = ((toRead + sizeof(TCHAR) - 1) / sizeof(TCHAR));
            // Always allow space for null terminator
            size_t roundedBytes = (elems + 1) * sizeof(TCHAR);
            TCHAR *newFileName = (TCHAR *)realloc(fileName, roundedBytes);
            if (newFileName == NULL)
            {
                errorResult = "Insufficient memory";
                errNumber = NOMEMORY;
                return false;
            }
            fileName = newFileName;

            if (header.parentNameEntry >= header.stringTableSize /* Bad entry */ ||
                fseek(loadFile, header.stringTable + header.parentNameEntry, SEEK_SET) != 0 ||
                fread(fileName, 1, toRead, loadFile) != toRead)
            {
                errorResult = "Unable to read parent file name";
                return false;
            }
            fileName[elems] = 0; // Should already be null-terminated, but just in case.

            if (! LoadFile(false, header.parentTimeStamp, TAGGED(0)))
                return false;
        }

        ASSERT(hierarchyTable.size() > 0);
    }
    else // Top-level file
    {
        if (isHierarchy && ! ML_Cons_Cell::IsNull(tail))
        {
            // There should be no further file names if this is really the top.
            errorResult = "Too many file names in the list";
            return false;
        }
        if (ModuleId(header.parentTimeStamp) != exportSignature)
        {
            // Time-stamp does not match executable.
            errorResult = 
                    "Saved state was exported from a different executable or the executable has changed";
            return false;
        }

        // Any existing spaces at this level or greater must be turned
        // into local spaces.  We may have references from the stack to objects that
        // have previously been imported but otherwise these spaces are no longer
        // needed.
        // Clean out the hierarchy table.
        for (std::vector<HierarchyTable>::iterator i = hierarchyTable.begin(); i != hierarchyTable.end(); i++)
            gMem.DemoteOldPermanentSpaces(i->timeStamp);

        hierarchyTable.clear();
    }

    // Now have a valid, matching saved state.
    // Load the segment descriptors.
    std::vector<SavedStateSegmentDescr> descrs;
    descrs.reserve(header.segmentDescrCount);
    if (fseek(loadFile, header.segmentDescr, SEEK_SET) != 0)
    {
        errorResult = "Unable to read segment descriptors";
        return false;
    }
    for (unsigned i = 0; i < header.segmentDescrCount; i++)
    {
        SavedStateSegmentDescr msd;
        if (fread(&msd, sizeof(SavedStateSegmentDescr), 1, loadFile) != 1)
        {
            errorResult = "Unable to read segment descriptors";
            return false;
        }
        descrs.push_back(msd);
    }

    std::vector<StateLoadData> loadData;
    loadData.reserve(header.segmentDescrCount);

    // Read in and create the new segments first.  If we have problems,
    // in particular if we have run out of memory, then it's easier to recover.  
    for (std::vector<SavedStateSegmentDescr>::iterator descr = descrs.begin(); descr < descrs.end(); descr++)
    {
        MemSpace *space = gMem.SpaceForIndex(descr->segmentIndex, descr->moduleId);
        StateLoadData load;
        load.relocationCount = descr->relocationCount;
        load.relocations = descr->relocations;
        if (space != NULL) // An existing segment.  If it's mutable we might be overwriting it.
            load.targetAddr = space->bottom;

        if (descr->segmentData == 0)
        { // No data - just an entry in the index.
            if (space == NULL/* ||
                descr->segmentSize != (size_t)((char*)space->top - (char*)space->bottom)*/)
            {
                errorResult = "Mismatch for existing memory space";
                return false;
            }
        }
        else if ((descr->segmentFlags & SSF_OVERWRITE) == 0)
        { // New segment.
            if (space != NULL)
            {
                errorResult = "Segment already exists";
                return false;
            }
            // Allocate memory for the new segment.
            unsigned mFlags =
                (descr->segmentFlags & SSF_WRITABLE ? MTF_WRITEABLE : 0) |
                (descr->segmentFlags & SSF_NOOVERWRITE ? MTF_NO_OVERWRITE : 0) |
                (descr->segmentFlags & SSF_BYTES ? MTF_BYTES : 0) |
                (descr->segmentFlags & SSF_CODE ? MTF_EXECUTABLE : 0);
            PermanentMemSpace *newSpace =
                gMem.AllocateNewPermanentSpace(descr->segmentSize, mFlags, descr->segmentIndex, descr->moduleId);
            if (newSpace == 0)
            {
                errorResult = "Unable to allocate memory";
                return false;
            }

            PolyWord *mem  = newSpace->bottom;
            PolyWord* writeAble = newSpace->writeAble(mem);
            if (fseek(loadFile, descr->segmentData, SEEK_SET) != 0)
            {
                errorResult = "Unable to seek segment";
                return false;
            }
            if (readData(writeAble, descr->segmentSize, loadFile) != 1)
            {
                errorResult = "Unable to read segment";
                return false;
            }
            // Fill unused space to the top of the area.
            gMem.FillUnusedSpace(writeAble +descr->segmentSize/sizeof(PolyWord),
                newSpace->spaceSize() - descr->segmentSize/sizeof(PolyWord));
            // Leave it writable until we've done the relocations.

            if (newSpace->noOverwrite)
            {
                ClearVolatile cwbr;
                cwbr.ScanAddressesInRegion(newSpace->bottom, newSpace->topPointer);
            }
            load.targetAddr = newSpace->bottom;
        }
        loadData.push_back(load);
    }

    // Now read in the mutable overwrites.  This overwrites the muable areas with the values from the saved state.
    for (std::vector<SavedStateSegmentDescr>::iterator descr = descrs.begin(); descr < descrs.end(); descr++)
    {
        if (descr->segmentFlags & SSF_OVERWRITE)
        {
            MemSpace* space = gMem.SpaceForIndex(descr->segmentIndex, descr->moduleId);
            if (fseek(loadFile, descr->segmentData, SEEK_SET) != 0 ||
                fread(space->bottom, descr->segmentSize, 1, loadFile) != 1)
            {
                errorResult = "Unable to read segment";
                return false;
            }
        }
    }

    // Now the relocations.
    for (std::vector<StateLoadData>::iterator i = loadData.begin(); i < loadData.end(); i++)
    {
        PolyWord* baseAddr = i->targetAddr;
        ASSERT(baseAddr != NULL); // We should have created it.
        // Process explicit relocations.
        // If we get errors just skip the error and continue rather than leave
        // everything in an unstable state.
        if (i->relocations)
        {
            if (fseek(loadFile, i->relocations, SEEK_SET) != 0)
            {
                errorResult = "Unable to read relocation segment";
                return false;
            }
            for (unsigned k = 0; k < i->relocationCount; k++)
            {
                RelocationEntry reloc;
                if (fread(&reloc, sizeof(reloc), 1, loadFile) != 1)
                {
                    errorResult = "Unable to read relocation segment";
                    return false;
                }
                byte *setAddress = (byte*)baseAddr + reloc.relocAddress;
                byte *targetAddress = (byte*)loadData[reloc.targetSegment].targetAddr + reloc.targetAddress;
                ScanAddress::SetConstantValue(setAddress, (PolyObject*)(targetAddress), reloc.relKind);
            }
        }
    }

    // Add an entry to the hierarchy table for this file.
    hierarchyTable.push_back(HierarchyTable(thisFile, header.timeStamp));

    return true; // Succeeded
}

static void LoadState(TaskData *taskData, bool isHierarchy, Handle hFileList)
// Load a saved state or a hierarchy.  
// hFileList is a list if this is a hierarchy and a single name if it is not.
{
    StateLoader loader(isHierarchy, hFileList);
    // Request the main thread to do the load.  This may set the error string if it failed.
    processes->MakeRootRequest(taskData, &loader);

    if (loader.errorResult != 0)
    {
        if (loader.errNumber == 0)
            raise_fail(taskData, loader.errorResult);
        else
        {
            AutoFree<char*> buff((char *)malloc(strlen(loader.errorResult) + 2 + _tcslen(loader.fileName) * sizeof(TCHAR) + 1));
#if (defined(_WIN32) && defined(UNICODE))
            sprintf(buff, "%s: %S", loader.errorResult, (TCHAR *)loader.fileName);
#else
            sprintf(buff, "%s: %s", loader.errorResult, (TCHAR *)loader.fileName);
#endif
            raise_syscall(taskData, buff, loader.errNumber);
        }
    }
}

// Load a saved state file and any ancestors.
POLYUNSIGNED PolyLoadState(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);

    try {
        LoadState(taskData, false, pushedArg);
    }
    catch (const std::bad_alloc&) { setMemoryException(taskData); }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

// Load hierarchy.  This provides a complete list of children and parents.
POLYUNSIGNED PolyLoadHierarchy(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);

    try {
         LoadState(taskData, true, pushedArg);
    }
    catch (const std::bad_alloc&) { setMemoryException(taskData); }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

/*
 *  Additional functions to provide information or change saved-state files.
 */

// These functions do not affect the global state so can be executed by
// the ML threads directly.

static Handle ShowHierarchy(TaskData *taskData)
// Return the list of files in the hierarchy.
{
    Handle saved = taskData->saveVec.mark();
    Handle list  = SAVE(ListNull);

    // Process this in reverse order.
    for (std::vector<HierarchyTable>::reverse_iterator i = hierarchyTable.rbegin(); i != hierarchyTable.rend(); i++)
    {
        Handle value = SAVE(C_string_to_Poly(taskData, i->fileName.c_str()));
        Handle next  = alloc_and_save(taskData, sizeof(ML_Cons_Cell)/sizeof(PolyWord));
        DEREFLISTHANDLE(next)->h = value->Word();
        DEREFLISTHANDLE(next)->t = list->Word();
        taskData->saveVec.reset(saved);
        list = SAVE(next->Word());
    }
    return list;
}

// Show the hierarchy.
POLYUNSIGNED PolyShowHierarchy(POLYUNSIGNED threadId)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = ShowHierarchy(taskData);
    }
    catch (const std::bad_alloc&) { setMemoryException(taskData); }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

static void RenameParent(TaskData *taskData, PolyWord childName, PolyWord parentName)
// Change the name of the immediate parent stored in a child
{
    // The name of the file to modify.
    AutoFree<TCHAR*> fileNameBuff(Poly_string_to_T_alloc(childName));
    if (fileNameBuff == NULL)
        raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    // The new parent name to insert.
    AutoFree<TCHAR*> parentNameBuff(Poly_string_to_T_alloc(parentName));
    if (parentNameBuff == NULL)
        raise_syscall(taskData, "Insufficient memory", NOMEMORY);

    AutoClose loadFile(_tfopen(fileNameBuff, _T("r+b"))); // Open for reading and writing
    if ((FILE*)loadFile == NULL)
    {
        AutoFree<char*> buff((char *)malloc(23 + _tcslen(fileNameBuff) * sizeof(TCHAR) + 1));
#if (defined(_WIN32) && defined(UNICODE))
        sprintf(buff, "Cannot open load file: %S", (TCHAR *)fileNameBuff);
#else
        sprintf(buff, "Cannot open load file: %s", (TCHAR *)fileNameBuff);
#endif
        raise_syscall(taskData, buff, ERRORNUMBER);
    }

    SavedStateHeader header;
    // Read the header and check the signature.
    if (fread(&header, sizeof(SavedStateHeader), 1, loadFile) != 1)
        raise_fail(taskData, "Unable to load header");

    if (strncmp(header.headerSignature, SAVEDSTATESIGNATURE, sizeof(header.headerSignature)) != 0)
        raise_fail(taskData, "File is not a saved state");

    if (header.headerVersion != SAVEDSTATEVERSION ||
        header.headerLength != sizeof(SavedStateHeader) ||
        header.segmentDescrLength != sizeof(SavedStateSegmentDescr))
    {
        raise_fail(taskData, "Unsupported version of saved state file");
    }

    // Does this actually have a parent?
    if (header.parentNameEntry == 0)
        raise_fail(taskData, "File does not have a parent");

    // At the moment the only entry in the string table is the parent
    // name so we can simply write a new one on the end of the file.
    // This makes the file grow slightly each time but it shouldn't be
    // significant.
    fseek(loadFile, 0, SEEK_END);
    header.stringTable = ftell(loadFile); // Remember where this is
    _fputtc(0, loadFile); // First byte of string table is zero
    _fputts(parentNameBuff, loadFile);
    _fputtc(0, loadFile); // A terminating null.
    header.stringTableSize = (_tcslen(parentNameBuff) + 2)*sizeof(TCHAR);

    // Now rewind and write the header with the revised string table.
    fseek(loadFile, 0, SEEK_SET);
    fwrite(&header, sizeof(header), 1, loadFile);
}

POLYUNSIGNED PolyRenameParent(POLYUNSIGNED threadId, POLYUNSIGNED childName, POLYUNSIGNED parentName)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();

    try {
        RenameParent(taskData, PolyWord::FromUnsigned(childName), PolyWord::FromUnsigned(parentName));
    }
    catch (const std::bad_alloc&) { setMemoryException(taskData); }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

static Handle ShowParent(TaskData *taskData, Handle hFileName)
// Return the name of the immediate parent stored in a child
{
    AutoFree<TCHAR*> fileNameBuff(Poly_string_to_T_alloc(hFileName->Word()));
    if (fileNameBuff == NULL)
        raise_syscall(taskData, "Insufficient memory", NOMEMORY);

    AutoClose loadFile(_tfopen(fileNameBuff, _T("rb")));
    if ((FILE*)loadFile == NULL)
    {
        AutoFree<char*> buff((char *)malloc(23 + _tcslen(fileNameBuff) * sizeof(TCHAR) + 1));
        if (buff == NULL)
            raise_syscall(taskData, "Insufficient memory", NOMEMORY);
#if (defined(_WIN32) && defined(UNICODE))
        sprintf(buff, "Cannot open load file: %S", (TCHAR *)fileNameBuff);
#else
        sprintf(buff, "Cannot open load file: %s", (TCHAR *)fileNameBuff);
#endif
        raise_syscall(taskData, buff, ERRORNUMBER);
    }

    SavedStateHeader header;
    // Read the header and check the signature.
    if (fread(&header, sizeof(SavedStateHeader), 1, loadFile) != 1)
        raise_fail(taskData, "Unable to load header");

    if (strncmp(header.headerSignature, SAVEDSTATESIGNATURE, sizeof(header.headerSignature)) != 0)
        raise_fail(taskData, "File is not a saved state");

    if (header.headerVersion != SAVEDSTATEVERSION ||
        header.headerLength != sizeof(SavedStateHeader) ||
        header.segmentDescrLength != sizeof(SavedStateSegmentDescr))
    {
        raise_fail(taskData, "Unsupported version of saved state file");
    }

    // Does this have a parent?
    if (header.parentNameEntry != 0)
    {
        size_t toRead = header.stringTableSize-header.parentNameEntry;
        size_t elems = ((toRead + sizeof(TCHAR) - 1) / sizeof(TCHAR));
        // Always allow space for null terminator
        size_t roundedBytes = (elems + 1) * sizeof(TCHAR);
        AutoFree<TCHAR*> parentFileName((TCHAR *)malloc(roundedBytes));
        if (parentFileName == NULL)
            raise_syscall(taskData, "Insufficient memory", NOMEMORY);

        if (header.parentNameEntry >= header.stringTableSize /* Bad entry */ ||
            fseek(loadFile, header.stringTable + header.parentNameEntry, SEEK_SET) != 0 ||
            fread(parentFileName, 1, toRead, loadFile) != toRead)
        {
            raise_fail(taskData, "Unable to read parent file name");
        }
        parentFileName[elems] = 0; // Should already be null-terminated, but just in case.
        // Convert the name into a Poly string and then build a "Some" value.
        // It's possible, although silly, to have the empty string as a parent name.
        Handle resVal = SAVE(C_string_to_Poly(taskData, parentFileName));
        Handle result = alloc_and_save(taskData, 1);
        DEREFHANDLE(result)->Set(0, resVal->Word());
        return result;
    }
    else return SAVE(NONE_VALUE);
}

// Return the name of the immediate parent stored in a child
POLYUNSIGNED PolyShowParent(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = ShowParent(taskData, pushedArg);
    }
    catch (const std::bad_alloc&) { setMemoryException(taskData); }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

struct _entrypts savestateEPT[] =
{
    { "PolySaveState",                  (polyRTSFunction)&PolySaveState },
    { "PolyLoadState",                  (polyRTSFunction)&PolyLoadState },
    { "PolyShowHierarchy",              (polyRTSFunction)&PolyShowHierarchy },
    { "PolyRenameParent",               (polyRTSFunction)&PolyRenameParent },
    { "PolyShowParent",                 (polyRTSFunction)&PolyShowParent },
    { "PolyLoadHierarchy",              (polyRTSFunction)&PolyLoadHierarchy },

    { NULL, NULL } // End of list.
};

