/*
    Title:     Export memory as a PE/COFF object
    Author:    David C. J. Matthews.

    Copyright (c) 2006, 2011, 2016 David C. J. Matthews


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

#if (SIZEOF_VOIDP == 8)
#define DIRECT_WORD_RELOCATION      IMAGE_REL_AMD64_ADDR64
#define RELATIVE_32BIT_RELOCATION   IMAGE_REL_AMD64_REL32
#else
#define DIRECT_WORD_RELOCATION      IMAGE_REL_I386_DIR32
#define RELATIVE_32BIT_RELOCATION   IMAGE_REL_I386_REL32
#endif

void PECOFFExport::addExternalReference(void *relocAddr, const char *name)
{
    externTable.makeEntry(name);
    IMAGE_RELOCATION reloc;
    // Set the offset within the section we're scanning.
    setRelocationAddress(relocAddr, &reloc.VirtualAddress);
    reloc.SymbolTableIndex = symbolNum++;
    reloc.Type = DIRECT_WORD_RELOCATION;
    fwrite(&reloc, sizeof(reloc), 1, exportFile);
    relocationCount++;
}

// Generate the address relative to the start of the segment.
void PECOFFExport::setRelocationAddress(void *p, DWORD *reloc)
{
    unsigned area = findArea(p);
    DWORD offset = (DWORD)((char*)p - (char*)memTable[area].mtAddr);
    *reloc = offset;
}

// Create a relocation entry for an address at a given location.
PolyWord PECOFFExport::createRelocation(PolyWord p, void *relocAddr)
{
    IMAGE_RELOCATION reloc;
    // Set the offset within the section we're scanning.
    setRelocationAddress(relocAddr, &reloc.VirtualAddress);
    void *addr = p.AsAddress();
    unsigned addrArea = findArea(addr);
    POLYUNSIGNED offset = (char*)addr - (char*)memTable[addrArea].mtAddr;
    reloc.SymbolTableIndex = addrArea;
    reloc.Type = DIRECT_WORD_RELOCATION;
    fwrite(&reloc, sizeof(reloc), 1, exportFile);
    relocationCount++;
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
void PECOFFExport::ScanConstant(byte *addr, ScanRelocationKind code)
{
    IMAGE_RELOCATION reloc;
    PolyWord p = GetConstantValue(addr, code);

    if (IS_INT(p) || p == PolyWord::FromUnsigned(0))
        return;

    void *a = p.AsAddress();
    unsigned aArea = findArea(a);

    // We don't need a relocation if this is relative to the current segment
    // since the relative address will already be right.
    if (code == PROCESS_RELOC_I386RELATIVE && aArea == findArea(addr))
        return;

    setRelocationAddress(addr, &reloc.VirtualAddress);
    // Set the value at the address to the offset relative to the symbol.
    POLYUNSIGNED offset = (char*)a - (char*)memTable[aArea].mtAddr;
    reloc.SymbolTableIndex = aArea;

    // The value we store here is the offset whichever relocation method
    // we're using.
    unsigned maxSize = code == PROCESS_RELOC_I386RELATIVE ? 4: sizeof(PolyWord);
    for (unsigned i = 0; i < maxSize; i++)
    {
        addr[i] = (byte)(offset & 0xff);
        offset >>= 8;
    }

    if (code == PROCESS_RELOC_I386RELATIVE)
        reloc.Type = RELATIVE_32BIT_RELOCATION;
    else
        reloc.Type = DIRECT_WORD_RELOCATION;

    fwrite(&reloc, sizeof(reloc), 1, exportFile);
    relocationCount++;
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
    unsigned i;
    // These are written out as the description of the data.
    exportDescription exports;
    time_t now = getBuildTime();

    sections = new IMAGE_SECTION_HEADER [memTableEntries+1]; // Plus one for the tables.

    // Write out initial values for the headers.  These are overwritten at the end.
    // File header
    memset(&fhdr, 0, sizeof(fhdr));
#if (SIZEOF_VOIDP == 8)
    fhdr.Machine = IMAGE_FILE_MACHINE_AMD64; // x86-64
#else
    fhdr.Machine = IMAGE_FILE_MACHINE_I386; // i386
#endif
    fhdr.NumberOfSections = memTableEntries+1; // One for each area plus one for the tables.
    fhdr.TimeDateStamp = (DWORD)now;
    //fhdr.NumberOfSymbols = memTableEntries+1; // One for each area plus "poly_exports"
    fwrite(&fhdr, sizeof(fhdr), 1, exportFile); // Write it for the moment.

    // External symbols are added after the memory table entries and "poly_exports".
    symbolNum = memTableEntries+1; // The first external symbol

    // Section headers.
    for (i = 0; i < memTableEntries; i++)
    {
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
        else if (memTable[i].mtFlags & MTF_EXECUTABLE)
        {
            // Immutable data areas are marked as executable.
            strcpy((char*)sections[i].Name, ".text");
            sections[i].Characteristics |= IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE;
        }
        else
        {
            // Immutable data areas are marked as executable.
            strcpy((char*)sections[i].Name, ".rdata");
            sections[i].Characteristics |= IMAGE_SCN_CNT_INITIALIZED_DATA;
        }
    }
    // Extra section for the tables.
    memset(&sections[memTableEntries], 0, sizeof(IMAGE_SECTION_HEADER));
    sprintf((char*)sections[memTableEntries].Name, ".data");
    sections[memTableEntries].SizeOfRawData = sizeof(exports) + (memTableEntries+1)*sizeof(memoryTableEntry);
    // Don't need write access here but keep it for consistency with other .data sections
    sections[memTableEntries].Characteristics = 
        IMAGE_SCN_MEM_READ | IMAGE_SCN_ALIGN_8BYTES | IMAGE_SCN_MEM_WRITE | IMAGE_SCN_CNT_INITIALIZED_DATA;

    fwrite(sections, sizeof(IMAGE_SECTION_HEADER), memTableEntries+1, exportFile); // Write it for the moment.

    for (i = 0; i < memTableEntries; i++)
    {
        // Relocations.  The first entry is special and is only used if
        // we have more than 64k relocations.  It contains the number of relocations but is
        // otherwise ignored.
        sections[i].PointerToRelocations = ftell(exportFile);
        memset(&reloc, 0, sizeof(reloc));
        fwrite(&reloc, sizeof(reloc), 1, exportFile);
        relocationCount = 1;

        // Create the relocation table and turn all addresses into offsets.
        char *start = (char*)memTable[i].mtAddr;
        char *end = start + memTable[i].mtLength;
        for (p = (PolyWord*)start; p < (PolyWord*)end; )
        {
            p++;
            PolyObject *obj = (PolyObject*)p;
            POLYUNSIGNED length = obj->Length();
            // Update any constants before processing the object
            // We need that for relative jumps/calls in X86/64.
            if (length != 0 && obj->IsCodeObject())
                machineDependent->ScanConstantsWithinCode(obj, this);
            relocateObject(obj);
            p += length;
        }
            // If there are more than 64k relocations set this bit and set the value to 64k-1.
        if (relocationCount >= 65535) {
            sections[i].NumberOfRelocations = 65535;
            sections[i].Characteristics |= IMAGE_SCN_LNK_NRELOC_OVFL;
            // We have to go back and patch up the first (dummy) relocation entry
            // which contains the count.
            fseek(exportFile, sections[i].PointerToRelocations, SEEK_SET);
            memset(&reloc, 0, sizeof(reloc));
            reloc.VirtualAddress = relocationCount;
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
    reloc.SymbolTableIndex = memTableEntries; // Relative to poly_exports
    reloc.VirtualAddress = offsetof(exportDescription, memTable);
    fwrite(&reloc, sizeof(reloc), 1, exportFile);
    relocationCount++;

    // Address of "rootFunction" within "exports"
    reloc.Type = DIRECT_WORD_RELOCATION;
    unsigned rootAddrArea = findArea(rootFunction);
    reloc.SymbolTableIndex = rootAddrArea;
    reloc.VirtualAddress = offsetof(exportDescription, rootFunction);
    fwrite(&reloc, sizeof(reloc), 1, exportFile);
    relocationCount++;

    for (i = 0; i < memTableEntries; i++)
    {
        reloc.Type = DIRECT_WORD_RELOCATION;
        reloc.SymbolTableIndex = i; // Relative to base symbol
        reloc.VirtualAddress =
            sizeof(exportDescription) + i * sizeof(memoryTableEntry) + offsetof(memoryTableEntry, mtAddr);
        fwrite(&reloc, sizeof(reloc), 1, exportFile);
        relocationCount++;
    }

    ASSERT(relocationCount < 65535); // Shouldn't get overflow!!
    sections[memTableEntries].NumberOfRelocations = relocationCount;

    // Now the binary data.
    for (i = 0; i < memTableEntries; i++)
    {
        sections[i].PointerToRawData = ftell(exportFile);
        fwrite(memTable[i].mtAddr, 1, memTable[i].mtLength, exportFile);
    }

    sections[memTableEntries].PointerToRawData = ftell(exportFile);
    memset(&exports, 0, sizeof(exports));
    exports.structLength = sizeof(exportDescription);
    exports.memTableSize = sizeof(memoryTableEntry);
    exports.memTableEntries = memTableEntries;
    exports.memTable = (memoryTableEntry *)sizeof(exports); // It follows immediately after this.
    exports.rootFunction = (void*)((char*)rootFunction - (char*)memTable[rootAddrArea].mtAddr);
    exports.timeStamp = now;
    exports.architecture = machineDependent->MachineArchitecture();
    exports.rtsVersion = POLY_version_number;

    // Set the address values to zero before we write.  They will always
    // be relative to their base symbol.
    for (i = 0; i < memTableEntries; i++)
        memTable[i].mtAddr = 0;

    fwrite(&exports, sizeof(exports), 1, exportFile);
    fwrite(memTable, sizeof(memoryTableEntry), memTableEntries, exportFile);
    // First the symbol table.  We have one entry for the exports and an additional
    // entry for each of the sections.
    fhdr.PointerToSymbolTable = ftell(exportFile);

    // The section numbers are one-based.  Zero indicates the "common" area.
    // First write symbols for each section and for poly_exports.
    for (i = 0; i < memTableEntries; i++)
    {
        char buff[50];
        sprintf(buff, "area%0d", i);
        writeSymbol(buff, 0, i+1, false);
    }

    // Exported symbol for table.
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

