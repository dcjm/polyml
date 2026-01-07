/*
    Title:     Export memory as a PE/COFF object
    Author:    David C. J. Matthews.

    Copyright (c) 2006, 2011, 2016-18, 2020-21, 2025 David C. J. Matthews


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
#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#include <stdio.h>
#include <time.h>

#ifdef HAVE_STDDEF_H
#include <stddef.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif

#include <windows.h>

#include "globals.h"
#include "pecoffexport.h"
#include "machine_dep.h"
#include "scanaddrs.h"
#include "run_time.h"
#include "../polyexports.h"
#include "version.h"
#include "polystring.h"
#include "timing.h"

#ifdef _DEBUG
/* MS C defines _DEBUG for debug builds. */
#define DEBUG
#endif

#ifdef DEBUG
#define ASSERT(x) assert(x)
#else
#define ASSERT(x)
#endif

#if (defined(HOSTARCHITECTURE_X86))
#define DIRECT_WORD_RELOCATION      IMAGE_REL_I386_DIR32
#define RELATIVE_32BIT_RELOCATION   IMAGE_REL_I386_REL32
#define IMAGE_MACHINE_TYPE          IMAGE_FILE_MACHINE_I386
#elif (defined(HOSTARCHITECTURE_X86_64))
#define DIRECT_WORD_RELOCATION      IMAGE_REL_AMD64_ADDR64
#define RELATIVE_32BIT_RELOCATION   IMAGE_REL_AMD64_REL32
#define IMAGE_MACHINE_TYPE          IMAGE_FILE_MACHINE_AMD64
#elif (defined(HOSTARCHITECTURE_AARCH64))
#define DIRECT_WORD_RELOCATION      IMAGE_REL_ARM64_ADDR64
#define RELATIVE_32BIT_RELOCATION   IMAGE_REL_AMD64_REL32 // Leave for the moment
#define IMAGE_MACHINE_TYPE          IMAGE_FILE_MACHINE_ARM64
#else
#error "Unknown architecture: unable to configure exporter for PECOFF"
#endif

// ARM64 ADRP relocations have a maximum offset of 1Mbyte.
// We define symbols within each segment and make the offsets
// relative to those symbols.  This isn't necessary for X86
// but there's no harm in doing it there.
const unsigned SYMBOLSPACING = 0x100000;

void PECOFFExport::writeRelocation(const IMAGE_RELOCATION* reloc)
{
    fwrite(reloc, sizeof(*reloc), 1, exportFile);
    if (relocationCount == 0)
        firstRelocation = *reloc;
    relocationCount++;
}

void PECOFFExport::addExternalReference(void *relocAddr, const char *name, bool/* isFuncPtr*/)
{
    externTable.makeEntry(name);
    IMAGE_RELOCATION reloc;
    // Set the offset within the section we're scanning.
    setRelocationAddress(relocAddr, &reloc);
    reloc.SymbolTableIndex = symbolNum++;
    reloc.Type = DIRECT_WORD_RELOCATION;
    writeRelocation(&reloc);
}

// Set the VirtualAddrss field to the offset within the current segment
// where the relocation must be applied.
void PECOFFExport::setRelocationAddress(void *p, IMAGE_RELOCATION*reloc)
{
    unsigned area = findArea(p);
    DWORD offset = (DWORD)((char*)p - (char*)memTable[area].mtOriginalAddr);
    reloc->VirtualAddress = offset;
}

// Set the symbol in the relocation to the symbol for the target address
// and return the offset relative to that symbol.
POLYUNSIGNED PECOFFExport::setSymbolAndGetOffset(void* p, IMAGE_RELOCATION* reloc)
{
    unsigned area = findArea(p);
    POLYUNSIGNED symbol = 0;
    for (unsigned i = 0; i < area; i++)
        symbol += (memTable[i].mtLength + SYMBOLSPACING-1) / SYMBOLSPACING;
    POLYUNSIGNED offset = (POLYUNSIGNED)((char*)p - (char*)memTable[area].mtOriginalAddr);
    symbol += offset / SYMBOLSPACING;
    reloc->SymbolTableIndex = (DWORD)symbol;
    return offset % SYMBOLSPACING;
}

// Create a relocation entry for an address at a given location.
PolyWord PECOFFExport::createRelocation(PolyWord p, void *relocAddr)
{
    IMAGE_RELOCATION reloc;
    // Set the offset within the section we're scanning.
    setRelocationAddress(relocAddr, &reloc);
    void *addr = p.AsAddress();
    POLYUNSIGNED offset = setSymbolAndGetOffset(addr, &reloc);
    reloc.Type = DIRECT_WORD_RELOCATION;
    writeRelocation(&reloc);
    return PolyWord::FromUnsigned(offset);
}

#ifdef SYMBOLS_REQUIRE_UNDERSCORE
#define POLY_PREFIX_STRING "_"
#else
#define POLY_PREFIX_STRING ""
#endif

void PECOFFExport::writeSymbol(const char *symbolName, __int32 value, int section, bool isExtern, int symType)
{
    // On X86/32 we have to add an underscore to external symbols
    TempCString fullSymbol;
    fullSymbol = (char*)malloc(strlen(POLY_PREFIX_STRING) + strlen(symbolName) + 1);
    if (fullSymbol == 0) throw MemoryException();
    sprintf(fullSymbol, "%s%s", POLY_PREFIX_STRING, symbolName);
    IMAGE_SYMBOL symbol;
    memset(&symbol, 0, sizeof(symbol)); // Zero the unused part of the string
    // Short symbol names go in the entry, longer ones go in the string table.
    if (strlen(fullSymbol) <= 8)
        strcat((char*)symbol.N.ShortName, fullSymbol);
    else {
        symbol.N.Name.Short = 0;
        // We have to add 4 bytes because the first word written to the file is a length word.
        symbol.N.Name.Long = stringTable.makeEntry(fullSymbol) + sizeof(unsigned);
    }
    symbol.Value = value;
    symbol.SectionNumber = section;
    symbol.Type = symType;
    symbol.StorageClass = isExtern ? IMAGE_SYM_CLASS_EXTERNAL : IMAGE_SYM_CLASS_STATIC;
    fwrite(&symbol, sizeof(symbol), 1, exportFile);
}

/* This is called for each constant within the code. 
   Print a relocation entry for the word and return a value that means
   that the offset is saved in original word. */
void PECOFFExport::ScanConstant(PolyObject *base, byte *addr, ScanRelocationKind code, intptr_t displacement)
{
#ifndef POLYML32IN64
    IMAGE_RELOCATION reloc;
    PolyObject *p = GetConstantValue(addr, code, displacement);

    if (p == 0)
        return;

    setRelocationAddress(addr, &reloc);
    POLYUNSIGNED offset = setSymbolAndGetOffset(p, &reloc);

    switch (code)
    {
    case PROCESS_RELOC_DIRECT:
    {
        // The value we store here is the offset whichever relocation method
        // we're using.
        for (unsigned i = 0; i < sizeof(PolyWord); i++)
        {
            addr[i] = (byte)(offset & 0xff);
            offset >>= 8;
        }
        reloc.Type = DIRECT_WORD_RELOCATION;
        writeRelocation(&reloc);
        break;
    }

    case PROCESS_RELOC_I386RELATIVE:
    {
        // We don't need a relocation if this is relative to the current segment
        // since the relative address will already be right.
        if (findArea(p) == findArea(addr)) return;
        for (unsigned i = 0; i < 4; i++)
        {
            addr[i] = (byte)(offset & 0xff);
            offset >>= 8;
        }
        reloc.Type = RELATIVE_32BIT_RELOCATION;
        writeRelocation(&reloc);
        break;
    }

#if defined(HOSTARCHITECTURE_AARCH64)
    case PROCESS_RELOC_ARM64ADRPLDR64:
    case PROCESS_RELOC_ARM64ADRPLDR32:
    case PROCESS_RELOC_ARM64ADRPADD:
    {
        // The first word is the ADRP, the second is LDR or ADD
        setRelocationAddress(addr, &reloc);
        reloc.Type = IMAGE_REL_ARM64_PAGEBASE_REL21;
        writeRelocation(&reloc);
        setRelocationAddress(addr+4, &reloc);
        uint32_t* pt = (uint32_t*)addr;
        uint32_t instr1 = pt[1];
        int scale;
        if ((instr1 & 0xfbc00000) == 0xf9400000)
        {
            reloc.Type = IMAGE_REL_ARM64_PAGEOFFSET_12L;
            scale = 8; // LDR of 64-bit quantity
        }
        else if ((instr1 & 0xfbc00000) == 0xb9400000)
        {
            reloc.Type = IMAGE_REL_ARM64_PAGEOFFSET_12L;
            scale = 4; // LDR of 32-bit quantity
        }
        else if ((instr1 & 0xff800000) == 0x91000000)
        {
            reloc.Type = IMAGE_REL_ARM64_PAGEOFFSET_12A;
            scale = 1;
        }
        writeRelocation(&reloc);
        // There doesn't seem to be any documentation to say how to
        // fill in the target.  It turns out that the offset relative to
        // the symbol is encoded in the ADRP as a BYTE offset, despite the
        // final value, after relocation, being a page offset.  The low-order
        // bits of the offset are encoded in the LDR as a normal page offset.
        ASSERT(offset <= 0x1fffff);

        pt[0] = toARMInstr((fromARMInstr(pt[0]) & 0x9f00001f) | ((offset & 3) << 29) | ((offset >> 2) << 5));
        pt[1] = toARMInstr((fromARMInstr(pt[1]) & 0xffc003ff) | (((offset & 0xfff) / scale) << 10));
        break;
    }
#endif

    default:
        ASSERT(0); // Unknown code
    }

#endif
}

// Set the file alignment.
void PECOFFExport::alignFile(int align)
{
    char pad[32] = {0}; // Maximum alignment
    int offset = ftell(exportFile);
    if ((offset % align) == 0) return;
    fwrite(&pad, align - (offset % align), 1, exportFile);
}

void PECOFFExport::exportStore(void)
{
    PolyWord    *p;
    IMAGE_FILE_HEADER fhdr;
    IMAGE_SECTION_HEADER *sections = 0;
    IMAGE_RELOCATION reloc;
    // These are written out as the description of the data.
    exportDescription exports;
    time_t now = getBuildTime();

    sections = new IMAGE_SECTION_HEADER [memTableEntries+1]; // Plus one for the tables.

    // Write out initial values for the headers.  These are overwritten at the end.
    // File header
    memset(&fhdr, 0, sizeof(fhdr));
    fhdr.Machine = IMAGE_MACHINE_TYPE; // x86-64
    fhdr.NumberOfSections = memTableEntries+1; // One for each area plus one for the tables.
    fhdr.TimeDateStamp = (DWORD)now;
    //fhdr.NumberOfSymbols = memTableEntries+1; // One for each area plus "poly_exports"
    fwrite(&fhdr, sizeof(fhdr), 1, exportFile); // Write it for the moment.

    // External symbols are added after the memory table entries and "poly_exports".
    symbolNum = 0;

    // Section headers.
    for (unsigned i = 0; i < memTableEntries; i++)
    {
        symbolNum += (unsigned)((memTable[i].mtLength+ SYMBOLSPACING-1) / SYMBOLSPACING);

        memset(&sections[i], 0, sizeof(IMAGE_SECTION_HEADER));
        sections[i].SizeOfRawData = (DWORD)memTable[i].mtLength;
        sections[i].Characteristics = IMAGE_SCN_MEM_READ | IMAGE_SCN_ALIGN_8BYTES;

        if (memTable[i].mtFlags & MTF_WRITEABLE)
        {
            // Mutable data
            ASSERT(!(memTable[i].mtFlags & MTF_EXECUTABLE)); // Executable areas can't be writable.
            strcpy((char*)sections[i].Name, ".data");
            sections[i].Characteristics |= IMAGE_SCN_MEM_WRITE | IMAGE_SCN_CNT_INITIALIZED_DATA;
        }
#ifndef CODEISNOTEXECUTABLE
        // Not if we're building the interpreted version.
        else if (memTable[i].mtFlags & MTF_EXECUTABLE)
        {
            // Immutable data areas are marked as executable.
            strcpy((char*)sections[i].Name, ".text");
            sections[i].Characteristics |= IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE;
        }
#endif
        else
        {
            // Immutable data areas are marked as executable.
            strcpy((char*)sections[i].Name, ".rdata");
            sections[i].Characteristics |= IMAGE_SCN_CNT_INITIALIZED_DATA;
        }
    }
    unsigned polyExportSymbol = symbolNum++; // Symbol for polyexports

    // Extra section for the tables.
    memset(&sections[memTableEntries], 0, sizeof(IMAGE_SECTION_HEADER));
    sprintf((char*)sections[memTableEntries].Name, ".data");
    sections[memTableEntries].SizeOfRawData = sizeof(exports) + (memTableEntries+1)*sizeof(memoryTableEntry);
    // Don't need write access here but keep it for consistency with other .data sections
    sections[memTableEntries].Characteristics = 
        IMAGE_SCN_MEM_READ | IMAGE_SCN_ALIGN_8BYTES | IMAGE_SCN_MEM_WRITE | IMAGE_SCN_CNT_INITIALIZED_DATA;

    fwrite(sections, sizeof(IMAGE_SECTION_HEADER), memTableEntries+1, exportFile); // Write it for the moment.

    for (unsigned i = 0; i < memTableEntries; i++)
    {
        sections[i].PointerToRelocations = ftell(exportFile);
        relocationCount = 0;

        // Create the relocation table and turn all addresses into offsets.
        char *start = (char*)memTable[i].mtOriginalAddr;
        char *end = start + memTable[i].mtLength;
        for (p = (PolyWord*)start; p < (PolyWord*)end; )
        {
            p++;
            PolyObject *obj = (PolyObject*)p;
            POLYUNSIGNED length = obj->Length();
            if (length != 0 && obj->IsCodeObject())
            {
                POLYUNSIGNED constCount;
                PolyWord* cp;
                // Get the constant area pointer first because ScanConstantsWithinCode
                // may alter it.
                machineDependent->GetConstSegmentForCode(obj, cp, constCount);
                // Update any constants before processing the object
                // We need that for relative jumps/calls in X86/64.
                machineDependent->RelocateConstantsWithinCode(obj, this);
                if (cp > (PolyWord*)obj && cp < ((PolyWord*)obj) + length)
                {
                    // Process the constants if they're in the area but not if they've been moved.
                    for (POLYUNSIGNED i = 0; i < constCount; i++) relocateValue(&(cp[i]));
                }
            }
            else relocateObject(obj);
            p += length;
        }
            // If there are more than 64k relocations set this bit and set the value to 64k-1.
        if (relocationCount >= 65535) {
            // We're going to overwrite the first relocation so we have to write the
            // copy we saved here.
            writeRelocation(&firstRelocation); // Increments relocationCount
            sections[i].NumberOfRelocations = 65535;
            sections[i].Characteristics |= IMAGE_SCN_LNK_NRELOC_OVFL;
            // We have to go back and patch up the first (dummy) relocation entry
            // which contains the count.
            fseek(exportFile, sections[i].PointerToRelocations, SEEK_SET);
            memset(&reloc, 0, sizeof(reloc));
            reloc.RelocCount = relocationCount;
            fwrite(&reloc, sizeof(reloc), 1, exportFile);
            fseek(exportFile, 0, SEEK_END); // Return to the end of the file.
        }
        else sections[i].NumberOfRelocations = relocationCount;
    }

    // We don't need to handle relocation overflow here.
    sections[memTableEntries].PointerToRelocations = ftell(exportFile);
    relocationCount = 0;

    // Relocations for "exports" and "memTable";

    // Address of "memTable" within "exports". We can't use createRelocation because
    // the position of the relocation is not in either the mutable or the immutable area.
    reloc.Type = DIRECT_WORD_RELOCATION;
    reloc.SymbolTableIndex = polyExportSymbol; // Relative to poly_exports
    reloc.VirtualAddress = offsetof(exportDescription, memTable);
    fwrite(&reloc, sizeof(reloc), 1, exportFile);
    relocationCount++;

    // Address of "rootFunction" within "exports"
    reloc.Type = DIRECT_WORD_RELOCATION;
    POLYUNSIGNED rootOffset = setSymbolAndGetOffset(rootFunction, &reloc);
    reloc.VirtualAddress = offsetof(exportDescription, rootFunction);
    fwrite(&reloc, sizeof(reloc), 1, exportFile);
    relocationCount++;

    for (unsigned i = 0; i < memTableEntries; i++)
    {
        reloc.Type = DIRECT_WORD_RELOCATION;
        // Add 1 to the address before we start so that there is a valid address.
        setSymbolAndGetOffset((PolyWord*)memTable[i].mtCurrentAddr + 1, &reloc); // The offset is always zero.
        reloc.VirtualAddress =
            sizeof(exportDescription) + i * sizeof(memoryTableEntry) + offsetof(memoryTableEntry, mtCurrentAddr);
        fwrite(&reloc, sizeof(reloc), 1, exportFile);
        relocationCount++;
    }

    ASSERT(relocationCount < 65535); // Shouldn't get overflow!!
    sections[memTableEntries].NumberOfRelocations = relocationCount;

    // Now the binary data.
    for (unsigned i = 0; i < memTableEntries; i++)
    {
        sections[i].PointerToRawData = ftell(exportFile);
        fwrite(memTable[i].mtOriginalAddr, 1, memTable[i].mtLength, exportFile);
    }

    sections[memTableEntries].PointerToRawData = ftell(exportFile);
    memset(&exports, 0, sizeof(exports));
    exports.structLength = sizeof(exportDescription);
    exports.memTableSize = sizeof(memoryTableEntry);
    exports.memTableEntries = memTableEntries;
    exports.memTable = (memoryTableEntry *)sizeof(exports); // It follows immediately after this.
    exports.rootFunction = (void*)rootOffset; // Actual address is set by relocation
    exports.execIdentifier = exportModId;
    exports.architecture = machineDependent->MachineArchitecture();
    exports.rtsVersion = POLY_version_number;
#ifdef POLYML32IN64
    exports.originalBaseAddr = globalHeapBase;
#else
    exports.originalBaseAddr = 0; 
#endif

    // Set the address values to zero before we write.  They will always
    // be relative to their base symbol.
    for (unsigned i = 0; i < memTableEntries; i++)
        memTable[i].mtCurrentAddr = 0;

    fwrite(&exports, sizeof(exports), 1, exportFile);

    for (unsigned i = 0; i < memTableEntries; i++)
    {
        memoryTableEntry memt;
        memset(&memt, 0, sizeof(memt));
        memt.mtCurrentAddr = memTable[i].mtCurrentAddr;
        memt.mtOriginalAddr = memTable[i].mtOriginalAddr;
        memt.mtLength = memTable[i].mtLength;
        memt.mtFlags = memTable[i].mtFlags;
        fwrite(&memt, sizeof(memoryTableEntry), 1, exportFile);
    }
    // First the symbol table.  We have one entry for the exports and an additional
    // entry for each of the sections.
    fhdr.PointerToSymbolTable = ftell(exportFile);

    // The section numbers are one-based.  Zero indicates the "common" area.
    // First write symbols for each section and for poly_exports.
    // For each section write symbols at regular intervals.
    for (unsigned i = 0; i < memTableEntries; i++)
    {
        for (unsigned j = 0; j < (memTable[i].mtLength+SYMBOLSPACING-1) / SYMBOLSPACING; j++)
        {
            char buff[50];
            sprintf(buff, "area%0dp%0d", i, j);
            writeSymbol(buff, j * SYMBOLSPACING, i + 1, false);
        }
    }

    // Exported symbol for table. The symbol refers to extra segment.
    writeSymbol("poly_exports", 0, memTableEntries+1, true);

    // External references.
    for (unsigned i = 0; i < externTable.stringSize; i += (unsigned)strlen(externTable.strings+i) + 1)
        writeSymbol(externTable.strings+i, 0, 0, true, 0x20);

    fhdr.NumberOfSymbols = symbolNum;

    // The string table is written immediately after the symbols.
    // The length is included as the first word.
    unsigned strSize = stringTable.stringSize + sizeof(unsigned);
    fwrite(&strSize, sizeof(strSize), 1, exportFile);
    fwrite(stringTable.strings, stringTable.stringSize, 1, exportFile);

    // Rewind to rewrite the headers.
    fseek(exportFile, 0, SEEK_SET);
    fwrite(&fhdr, sizeof(fhdr), 1, exportFile);
    fwrite(sections,  sizeof(IMAGE_SECTION_HEADER), memTableEntries+1, exportFile);

    fclose(exportFile); exportFile = NULL;
    delete[](sections);
}

