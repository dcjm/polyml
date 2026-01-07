/*
    Title:     Write out a database as a Mach object file
    Author:    David Matthews.

    Copyright (c) 2006-7, 2011-2, 2016-18, 2020-21, 2025 David C. J. Matthews

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

#include "config.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDDEF_H
#include <stddef.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x)
#endif

// If we haven't got the Mach header files we shouldn't be building this.
#include <mach-o/loader.h>
#include <mach-o/reloc.h>
#include <mach-o/nlist.h>

#ifdef HAVE_MACH_O_X86_64_RELOC_H
#include <mach-o/x86_64/reloc.h>
#endif

// Older versions of Mac OS didn't have this.
#ifdef HAVE_MACH_O_ARM64_RELOC_H
#include <mach-o/arm64/reloc.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif

#include "globals.h"

#include "diagnostics.h"
#include "sys.h"
#include "machine_dep.h"
#include "gc.h"
#include "mpoly.h"
#include "scanaddrs.h"
#include "machoexport.h"
#include "run_time.h"
#include "version.h"
#include "polystring.h"

// Mach-O seems to require each section to have a discrete virtual address range
// so we have to adjust various offsets to fit.
void MachoExport::adjustOffset(unsigned area, size_t &offset)
{
    // Add in the offset.  If sect is memTableEntries it's actually the
    // descriptors so doesn't have any additional offset.
    if (area != memTableEntries)
    {
        offset += sizeof(exportDescription)+sizeof(memoryTableEntry)*memTableEntries;
        for (unsigned i = 0; i < area; i++)
            offset += memTable[i].mtLength;
    }
}

void MachoExport::addExternalReference(void *relocAddr, const char *name, bool /*isFuncPtr*/)
{
    externTable.makeEntry(name);
    writeRelocation(0, relocAddr, symbolNum++, true);
}

// Generate the address relative to the start of the segment.
void MachoExport::setRelocationAddress(void *p, int32_t *reloc)
{
    unsigned area = findArea(p);
    size_t offset = (char*)p - (char*)memTable[area].mtOriginalAddr;
    *reloc = offset;
}

/* Get the index corresponding to an address. */
PolyWord MachoExport::createRelocation(PolyWord p, void *relocAddr)
{
    void *addr = p.AsAddress();
    unsigned addrArea = findArea(addr);
    size_t offset = (char*)addr - (char*)memTable[addrArea].mtOriginalAddr;
    adjustOffset(addrArea, offset);
    return writeRelocation(offset, relocAddr, addrArea+1 /* Sections count from 1 */, false);
}

PolyWord MachoExport::writeRelocation(POLYUNSIGNED offset, void *relocAddr, unsigned symbolNumber, bool isExtern)
{
    // It looks as though struct relocation_info entries are only used
    // with GENERIC_RELOC_VANILLA types.
    struct relocation_info relInfo;
    setRelocationAddress(relocAddr, &relInfo.r_address);
    relInfo.r_symbolnum = symbolNumber;
    relInfo.r_pcrel = 0;
#if (SIZEOF_VOIDP == 8)
    relInfo.r_length = 3; // 8 bytes
#else
    relInfo.r_length = 2; // 4 bytes
#endif
    relInfo.r_type = GENERIC_RELOC_VANILLA;
    relInfo.r_extern = isExtern ? 1 : 0;

    fwrite(&relInfo, sizeof(relInfo), 1, exportFile);
    relocationCount++;
    return PolyWord::FromUnsigned(offset);
}

/* This is called for each constant within the code. 
   Print a relocation entry for the word and return a value that means
   that the offset is saved in original word. */
void MachoExport::ScanConstant(PolyObject *base, byte *addr, ScanRelocationKind code, intptr_t displacement)
{
#ifndef POLYML32IN64
    PolyObject *p = GetConstantValue(addr, code, displacement);

    if (p == 0)
        return;

    void *a = p;
    unsigned aArea = findArea(a);

    // Set the value at the address to the offset relative to the symbol.
    size_t offset = (char*)a - (char*)memTable[aArea].mtOriginalAddr;

    switch (code)
    {
    case PROCESS_RELOC_DIRECT: // 32/64 bit address of target
        {
            struct relocation_info reloc;
            setRelocationAddress(addr, &reloc.r_address);
            reloc.r_symbolnum = aArea+1; // Section numbers start at 1
            reloc.r_pcrel = 0;
#if (SIZEOF_VOIDP == 8)
            reloc.r_length = 3; // 8 bytes
#else
            reloc.r_length = 2; // 4 bytes
#endif
            reloc.r_type = GENERIC_RELOC_VANILLA;
            reloc.r_extern = 0; // r_symbolnum is a section number.  It should be 1 if we make the IO area a common.
            // Adjust offset since this is section-relative
            adjustOffset(aArea, offset);

            for (unsigned i = 0; i < sizeof(PolyWord); i++)
            {
                addr[i] = (byte)(offset & 0xff);
                offset >>= 8;
            }
            fwrite(&reloc, sizeof(reloc), 1, exportFile);
            relocationCount++;
        }
        break;
#if (defined(HOSTARCHITECTURE_X86) || defined(HOSTARCHITECTURE_X86_64))
     case PROCESS_RELOC_I386RELATIVE:         // 32 bit relative address
        {
            unsigned addrArea = findArea(addr);
            // If it's in the same area we don't need a relocation because the
            // relative offset will be unchanged.
            if (addrArea != aArea)
            {
                struct relocation_info reloc;
                setRelocationAddress(addr, &reloc.r_address);
                reloc.r_symbolnum = aArea+1; // Section numbers start at 1
                reloc.r_pcrel = 1;
                reloc.r_length = 2; // 4 bytes
#if (defined(HOSTARCHITECTURE_X86_64))
                reloc.r_type = X86_64_RELOC_SIGNED; // Or X86_64_RELOC_BRANCH ?
#else
                reloc.r_type = GENERIC_RELOC_VANILLA;
#endif
                reloc.r_extern = 0; // r_symbolnum is a section number.
                fwrite(&reloc, sizeof(reloc), 1, exportFile);
                relocationCount++;

                // Adjust offset since this is section-relative
                adjustOffset(aArea, offset);
                size_t addrOffset = (char*)addr - (char*)memTable[addrArea].mtOriginalAddr;
                adjustOffset(addrArea, addrOffset);
                offset -= addrOffset + 4;

                for (unsigned i = 0; i < 4; i++)
                {
                    addr[i] = (byte)(offset & 0xff);
                    offset >>= 8;
                }
            }
        }
        break;
#endif
#if (defined(HOSTARCHITECTURE_AARCH64))
     case PROCESS_RELOC_ARM64ADRPLDR64:
     case PROCESS_RELOC_ARM64ADRPLDR32:
     case PROCESS_RELOC_ARM64ADRPADD:
     {
            // This seems to be completely undocumented and has been worked out
            // by some reverse-engineering and trial-and-error.  Have to use
            // symbol-relative addressing with the offset provided by a
            // ARM64_RELOC_ADDEND "relocation" that affects the next entry.
            struct relocation_info reloc;
            // We need four relocations here.
            // The first instruction is ADRP
            setRelocationAddress(addr, &reloc.r_address);
            reloc.r_pcrel = 0;
            reloc.r_length = 2; // 4 bytes
            reloc.r_type = ARM64_RELOC_ADDEND;
            reloc.r_symbolnum = offset; // Used for offset
            reloc.r_extern = 0;
            fwrite(&reloc, sizeof(reloc), 1, exportFile);
            relocationCount++;
            reloc.r_type = ARM64_RELOC_PAGE21;
            reloc.r_pcrel = 1;
            reloc.r_symbolnum = aArea+1; // Symbol number
            reloc.r_extern = 1;
            fwrite(&reloc, sizeof(reloc), 1, exportFile);
            relocationCount++;
            // The second instruction is LDR
            setRelocationAddress(addr+4, &reloc.r_address);
            reloc.r_pcrel = 0; // This is an absolute 12-bit value
            reloc.r_type = ARM64_RELOC_ADDEND;
            reloc.r_extern = 0;
            reloc.r_symbolnum = offset; // Used for offset
            fwrite(&reloc, sizeof(reloc), 1, exportFile);
            relocationCount++;
            reloc.r_type = ARM64_RELOC_PAGEOFF12;
            reloc.r_extern = 1;
            reloc.r_symbolnum = aArea+1; // Symbol number
            fwrite(&reloc, sizeof(reloc), 1, exportFile);
            relocationCount++;
            // Now we've found the target we can zero the data in the instructions.
            uint32_t* instrAddr = (uint32_t*)addr;
            instrAddr[0] = toARMInstr(fromARMInstr(instrAddr[0]) & 0x9f00001f);
            instrAddr[1] = toARMInstr(fromARMInstr(instrAddr[1]) & 0xffc003ff);
        }
        break;
#endif
        default:
            ASSERT(0); // Wrong type of relocation for this architecture.
    }
#endif
}

// Set the file alignment.
void MachoExport::alignFile(int align)
{
    char pad[32] = {0}; // Maximum alignment
    int offset = ftell(exportFile);
    if ((offset % align) == 0) return;
    fwrite(&pad, align - (offset % align), 1, exportFile);
}

void MachoExport::createStructsRelocation(unsigned sect, size_t offset)
{
    struct relocation_info reloc;
    reloc.r_address = offset;
    reloc.r_symbolnum = sect+1; // Section numbers start at 1
    reloc.r_pcrel = 0;
#if (SIZEOF_VOIDP == 8)
    reloc.r_length = 3; // 8 bytes
#else
    reloc.r_length = 2; // 4 bytes
#endif
    reloc.r_type = GENERIC_RELOC_VANILLA;
    reloc.r_extern = 0; // r_symbolnum is a section number.

    fwrite(&reloc, sizeof(reloc), 1, exportFile);
    relocationCount++;
}

void MachoExport::exportStore(void)
{
    PolyWord    *p;
#if (SIZEOF_VOIDP == 8)
    struct mach_header_64 fhdr;
    struct segment_command_64 sHdr;
    struct section_64 *sections = new section_64[memTableEntries+1];
    size_t sectionSize = sizeof(section_64);
#else
    struct mach_header fhdr;
    struct segment_command sHdr;
    struct section *sections = new section[memTableEntries+1];
    size_t sectionSize = sizeof(section);
#endif
    struct symtab_command symTab;
    unsigned i;

    // Write out initial values for the headers.  These are overwritten at the end.
    // File header
    memset(&fhdr, 0, sizeof(fhdr));
    fhdr.filetype = MH_OBJECT;
    fhdr.ncmds = 2; // One for the segment and one for the symbol table.
    fhdr.sizeofcmds = sizeof(sHdr) + sectionSize * (memTableEntries+1) + sizeof(symTab);
    fhdr.flags = 0;
    // The machine needs to match the machine we're compiling for
    // even if this is actually portable code.
#if (SIZEOF_VOIDP == 8)
    fhdr.magic = MH_MAGIC_64; // (0xfeedfacf) 64-bit magic number
#else
    fhdr.magic = MH_MAGIC; // Feed Face (0xfeedface)
#endif
#if defined(HOSTARCHITECTURE_X86)
    fhdr.cputype = CPU_TYPE_I386;
    fhdr.cpusubtype = CPU_SUBTYPE_I386_ALL;
#elif defined(HOSTARCHITECTURE_PPC)
    fhdr.cputype = CPU_TYPE_POWERPC;
    fhdr.cpusubtype = CPU_SUBTYPE_POWERPC_ALL;
#elif defined(HOSTARCHITECTURE_X86_64)
    fhdr.cputype = CPU_TYPE_X86_64;
    fhdr.cpusubtype = CPU_SUBTYPE_X86_64_ALL;
#elif defined(HOSTARCHITECTURE_AARCH64)
    fhdr.cputype = CPU_TYPE_ARM64;
    fhdr.cpusubtype = CPU_SUBTYPE_ARM64_ALL;
#else
#error "No support for exporting on this architecture"
#endif
    fwrite(&fhdr, sizeof(fhdr), 1, exportFile); // Write it for the moment.

    symbolNum = memTableEntries+1; // Symbols start after table entries and poly_exports

    // Segment header.
    memset(&sHdr, 0, sizeof(sHdr));
#if (SIZEOF_VOIDP == 8)
    sHdr.cmd = LC_SEGMENT_64;
#else
    sHdr.cmd = LC_SEGMENT;
#endif
    sHdr.nsects = memTableEntries+1; // One for each entry plus one for the tables.
    sHdr.cmdsize = sizeof(sHdr) + sectionSize * sHdr.nsects;
    // Add up the sections to give the file size
    sHdr.filesize = 0;
    for (i = 0; i < memTableEntries; i++)
        sHdr.filesize += memTable[i].mtLength; // Do we need any alignment?
    sHdr.filesize += sizeof(exportDescription) + memTableEntries * sizeof(memoryTableEntry);
    sHdr.vmsize = sHdr.filesize; // Set them the same since we don't have any "common" area.
    // sHdr.fileOff is set later.
    sHdr.maxprot = VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE;
    sHdr.initprot = VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE;
    sHdr.flags = 0;

    // Write it initially.
    fwrite(&sHdr, sizeof(sHdr), 1, exportFile);

    // Section header for each entry in the table
    POLYUNSIGNED sectAddr = sizeof(exportDescription)+sizeof(memoryTableEntry)*memTableEntries;
    for (i = 0; i < memTableEntries; i++)
    {
        memset(&(sections[i]), 0, sectionSize);

        if (memTable[i].mtFlags & MTF_WRITEABLE)
        {
            // Mutable areas
            ASSERT(!(memTable[i].mtFlags & MTF_EXECUTABLE)); // Executable areas can't be writable.
            sprintf(sections[i].sectname, "__data");
            sprintf(sections[i].segname, "__DATA");
            sections[i].flags = S_ATTR_LOC_RELOC | S_REGULAR;
        }
#ifndef CODEISNOTEXECUTABLE
        // Not if we're building the interpreted version.
        else if (memTable[i].mtFlags & MTF_EXECUTABLE)
        {
            sprintf(sections[i].sectname, "__text");
            sprintf(sections[i].segname, "__TEXT");
            sections[i].flags = S_ATTR_LOC_RELOC | S_ATTR_SOME_INSTRUCTIONS | S_REGULAR;
        }
#endif
        else
        {
            sprintf(sections[i].sectname, "__const");
            sprintf(sections[i].segname, "__DATA");
            sections[i].flags = S_ATTR_LOC_RELOC | S_REGULAR;
        }

        sections[i].addr = sectAddr;
        sections[i].size = memTable[i].mtLength;
        sectAddr += memTable[i].mtLength;
        //sections[i].offset is set later
        //sections[i].reloff is set later
        //sections[i].nreloc is set later
        sections[i].align = 3; // 8 byte alignment
        // theSection.size is set later

    }
    // For the tables.
    memset(&(sections[memTableEntries]), 0, sectionSize);
    sprintf(sections[memTableEntries].sectname, "__const");
    sprintf(sections[memTableEntries].segname, "__DATA");
    sections[memTableEntries].addr = 0;
    sections[memTableEntries].size = sizeof(exportDescription)+sizeof(memoryTableEntry)*memTableEntries;
    sections[memTableEntries].align = 3; // 8 byte alignment
    // theSection.size is set later
    sections[memTableEntries].flags = S_ATTR_LOC_RELOC | S_ATTR_SOME_INSTRUCTIONS | S_REGULAR;

    // Write them out for the moment.
    fwrite(sections, sectionSize * (memTableEntries+1), 1, exportFile);

    // Symbol table header.
    memset(&symTab, 0, sizeof(symTab));
    symTab.cmd = LC_SYMTAB;
    symTab.cmdsize = sizeof(symTab);
    //symTab.symoff is set later
    //symTab.nsyms is set later
    //symTab.stroff is set later
    //symTab.strsize is set later
    fwrite(&symTab, sizeof(symTab), 1, exportFile);

    // Create and write out the relocations.
    for (i = 0; i < memTableEntries; i++)
    {
        sections[i].reloff = ftell(exportFile);
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
        sections[i].nreloc = relocationCount;
    }

    // Additional relocations for the descriptors.
    sections[memTableEntries].reloff = ftell(exportFile);
    relocationCount = 0;

    // Address of "memTable" within "exports". We can't use createRelocation because
    // the position of the relocation is not in either the mutable or the immutable area.
    createStructsRelocation(memTableEntries, offsetof(exportDescription, memTable));

    // Address of "rootFunction" within "exports"
    unsigned rootAddrArea = findArea(rootFunction);
    size_t rootOffset = (char*)rootFunction - (char*)memTable[rootAddrArea].mtOriginalAddr;
    adjustOffset(rootAddrArea, rootOffset);
    createStructsRelocation(rootAddrArea, offsetof(exportDescription, rootFunction));

    // Addresses of the areas within memtable.
    for (i = 0; i < memTableEntries; i++)
    {
        createStructsRelocation(i,
            sizeof(exportDescription) + i * sizeof(memoryTableEntry) + offsetof(memoryTableEntry, mtCurrentAddr));
    }
    sections[memTableEntries].nreloc = relocationCount;

    // Write out the tables data.
    exportDescription exports;
    memset(&exports, 0, sizeof(exports));
    exports.structLength = sizeof(exportDescription);
    // Set the value to be the offset relative to the base of the area.  We have set a relocation
    // already which will add the base of the area.
    exports.rootFunction = (void*)rootOffset;
    exports.execIdentifier = exportModId;
    exports.architecture = machineDependent->MachineArchitecture();
    exports.rtsVersion = POLY_version_number;
#ifdef POLYML32IN64
    exports.originalBaseAddr = globalHeapBase;
#else
    exports.originalBaseAddr = 0;
#endif

    exports.memTableSize = sizeof(memoryTableEntry);
    exports.memTableEntries = memTableEntries;
    exports.memTable = (memoryTableEntry *)sizeof(exportDescription); // It follows immediately after this.

    sections[memTableEntries].offset = ftell(exportFile);
    fwrite(&exports, sizeof(exports), 1, exportFile);
    size_t addrOffset = sizeof(exports)+sizeof(memoryTableEntry)*memTableEntries;

    for (i = 0; i < memTableEntries; i++)
    {
        memoryTableEntry memt;
        memset(&memt, 0, sizeof(memt));
        memt.mtCurrentAddr = (void*)addrOffset; // Set this to the relative address.
        addrOffset += memTable[i].mtLength;
        memt.mtOriginalAddr = memTable[i].mtOriginalAddr;
        memt.mtLength = memTable[i].mtLength;
        memt.mtFlags = memTable[i].mtFlags;
        fwrite(&memt, sizeof(memoryTableEntry), 1, exportFile);
    }

    // Now the binary data.
    for (i = 0; i < memTableEntries; i++)
    {
        alignFile(4);
        sections[i].offset = ftell(exportFile);
        fwrite(memTable[i].mtOriginalAddr, 1, memTable[i].mtLength, exportFile);
    }

    // The symbol table.  The newer (15.0) Xtools linker seems to like this after the data.

    symTab.symoff = ftell(exportFile);

    stringTable.makeEntry(""); // Create a dummy string. Real strings must have offset > 0.

    // Global symbols: Just one.
    {
#if (SIZEOF_VOIDP == 8)
        struct nlist_64 symbol;
#else
        struct nlist symbol;
#endif
        memset(&symbol, 0, sizeof(symbol)); // Zero unused fields
        symbol.n_un.n_strx = stringTable.makeEntry("_poly_exports");
        symbol.n_type = N_EXT | N_SECT;
        symbol.n_sect = memTableEntries+1; // Sections count from 1.
        symbol.n_desc = REFERENCE_FLAG_DEFINED;
        fwrite(&symbol, sizeof(symbol), 1, exportFile);
    }

    // Create a symbol entry for each memTable entry.
    // ARM relocations need to be relative to an external.
    for (i = 0; i < memTableEntries; i++)
    {
#if (SIZEOF_VOIDP == 8)
        struct nlist_64 symbol;
#else
        struct nlist symbol;
#endif
        memset(&symbol, 0, sizeof(symbol)); // Zero unused fields
        char symName[100];
        sprintf(symName, "area%d", i);
        symbol.n_un.n_strx = stringTable.makeEntry(symName);
        symbol.n_type = N_SECT;
        symbol.n_sect = i+1; // Sections count from 1.
        symbol.n_desc = REFERENCE_FLAG_PRIVATE_DEFINED;
        symbol.n_value = 0;
        size_t offset = 0;
        adjustOffset(i, offset);
        symbol.n_value = offset;
        fwrite(&symbol, sizeof(symbol), 1, exportFile);
    }

    // External references.
    for (unsigned i = 0; i < externTable.stringSize; i += (unsigned)strlen(externTable.strings+i) + 1)
    {
        const char *symbolName = externTable.strings+i;
#if (SIZEOF_VOIDP == 8)
        struct nlist_64 symbol;
#else
        struct nlist symbol;
#endif
        memset(&symbol, 0, sizeof(symbol)); // Zero unused fields
        // Have to add an underscore to the symbols.
        TempCString fullSymbol;
        fullSymbol = (char*)malloc(strlen(symbolName) + 2);
        if (fullSymbol == 0) throw MemoryException();
        sprintf(fullSymbol, "_%s", symbolName);
        symbol.n_un.n_strx = stringTable.makeEntry(fullSymbol);
        symbol.n_type = N_EXT | N_UNDF;
        symbol.n_sect = NO_SECT;
        symbol.n_desc = REFERENCE_FLAG_UNDEFINED_NON_LAZY;
        fwrite(&symbol, sizeof(symbol), 1, exportFile);
    }

    symTab.nsyms = symbolNum;

    // The symbol name table
    symTab.stroff = ftell(exportFile);
    fwrite(stringTable.strings, stringTable.stringSize, 1, exportFile);
    symTab.strsize = stringTable.stringSize;
    alignFile(4);

    // Rewind to rewrite the headers with the actual offsets.
    rewind(exportFile);
    fwrite(&fhdr, sizeof(fhdr), 1, exportFile); // File header
    fwrite(&sHdr, sizeof(sHdr), 1, exportFile); // Segment header
    fwrite(sections, sectionSize * (memTableEntries+1), 1, exportFile); // Section headers
    fwrite(&symTab, sizeof(symTab), 1, exportFile); // Symbol table header
    fclose(exportFile); exportFile = NULL;

    delete[](sections);
}
