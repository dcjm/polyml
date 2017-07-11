/*
    Title:     Write out a database as a Mach object file
    Author:    David Matthews.

    Copyright (c) 2006-7, 2011-2, 2016 David C. J. Matthews

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

#ifdef HAVE_TIME_H
#include <time.h>
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
#include <mach-o/ppc/reloc.h>
#include <mach-o/x86_64/reloc.h>

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
#include "timing.h"

// Mach-O seems to require each section to have a discrete virtual address range
// so we have to adjust various offsets to fit.
void MachoExport::adjustOffset(unsigned area, POLYUNSIGNED &offset)
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

void MachoExport::addExternalReference(void *relocAddr, const char *name)
{
    externTable.makeEntry(name);
    writeRelocation(0, relocAddr, symbolNum++, true);
}

// Generate the address relative to the start of the segment.
void MachoExport::setRelocationAddress(void *p, int32_t *reloc)
{
    unsigned area = findArea(p);
    POLYUNSIGNED offset = (char*)p - (char*)memTable[area].mtAddr;
    *reloc = offset;
}

/* Get the index corresponding to an address. */
PolyWord MachoExport::createRelocation(PolyWord p, void *relocAddr)
{
    void *addr = p.AsAddress();
    unsigned addrArea = findArea(addr);
    POLYUNSIGNED offset = (char*)addr - (char*)memTable[addrArea].mtAddr;
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
    relInfo.r_type = X86_64_RELOC_UNSIGNED;
#else
    relInfo.r_length = 2; // 4 bytes
    relInfo.r_type = GENERIC_RELOC_VANILLA;
#endif
    relInfo.r_extern = isExtern ? 1 : 0;

    fwrite(&relInfo, sizeof(relInfo), 1, exportFile);
    relocationCount++;
    return PolyWord::FromUnsigned(offset);
}

/* This is called for each constant within the code. 
   Print a relocation entry for the word and return a value that means
   that the offset is saved in original word. */
void MachoExport::ScanConstant(PolyObject *base, byte *addr, ScanRelocationKind code)
{
    PolyWord p = GetConstantValue(addr, code);

    if (IS_INT(p) || p == PolyWord::FromUnsigned(0))
        return;

    void *a = p.AsAddress();
    unsigned aArea = findArea(a);

    // Set the value at the address to the offset relative to the symbol.
    POLYUNSIGNED offset = (char*)a - (char*)memTable[aArea].mtAddr;
    adjustOffset(aArea, offset);

    switch (code)
    {
    case PROCESS_RELOC_DIRECT: // 32 bit address of target
        {
            struct relocation_info reloc;
            setRelocationAddress(addr, &reloc.r_address);
            reloc.r_symbolnum = aArea+1; // Section numbers start at 1
            reloc.r_pcrel = 0;
#if (defined(HOSTARCHITECTURE_X86_64))
            reloc.r_length = 3; // 8 bytes
            reloc.r_type = X86_64_RELOC_UNSIGNED;
#else
            reloc.r_length = 2; // 4 bytes
            reloc.r_type = GENERIC_RELOC_VANILLA;
#endif
            reloc.r_extern = 0; // r_symbolnum is a section number.  It should be 1 if we make the IO area a common.

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
                reloc.r_type = X86_64_RELOC_SIGNED;
#else
                reloc.r_type = GENERIC_RELOC_VANILLA;
#endif
                reloc.r_extern = 0; // r_symbolnum is a section number.
                fwrite(&reloc, sizeof(reloc), 1, exportFile);
                relocationCount++;

                POLYUNSIGNED addrOffset = (char*)addr - (char*)memTable[addrArea].mtAddr;
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
        default:
            ASSERT(0); // Wrong type of relocation for this architecture.
    }
}

// Set the file alignment.
void MachoExport::alignFile(int align)
{
    char pad[32] = {0}; // Maximum alignment
    int offset = ftell(exportFile);
    if ((offset % align) == 0) return;
    fwrite(&pad, align - (offset % align), 1, exportFile);
}

void MachoExport::createStructsRelocation(unsigned sect, POLYUNSIGNED offset)
{
    struct relocation_info reloc;
    reloc.r_address = offset;
    reloc.r_symbolnum = sect+1; // Section numbers start at 1
    reloc.r_pcrel = 0;
#if (SIZEOF_VOIDP == 8)
    reloc.r_length = 3; // 8 bytes
    reloc.r_type = X86_64_RELOC_UNSIGNED;
#else
    reloc.r_length = 2; // 4 bytes
    reloc.r_type = GENERIC_RELOC_VANILLA;
#endif
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
#else
#error "No support for exporting on this architecture"
#endif
    fwrite(&fhdr, sizeof(fhdr), 1, exportFile); // Write it for the moment.

    symbolNum = 1; // The first symbol is poly_exports

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
        else if (memTable[i].mtFlags & MTF_EXECUTABLE)
        {
            sprintf(sections[i].sectname, "__text");
            sprintf(sections[i].segname, "__TEXT");
            sections[i].flags = S_ATTR_LOC_RELOC | S_ATTR_SOME_INSTRUCTIONS | S_REGULAR;
        }
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
        char *start = (char*)memTable[i].mtAddr;
        char *end = start + memTable[i].mtLength;
        for (p = (PolyWord*)start; p < (PolyWord*)end; )
        {
            p++;
            PolyObject *obj = (PolyObject*)p;
            POLYUNSIGNED length = obj->Length();
            if (length != 0 && obj->IsCodeObject())
                machineDependent->ScanConstantsWithinCode(obj, this);
            relocateObject(obj);
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
    POLYUNSIGNED rootOffset = (char*)rootFunction - (char*)memTable[rootAddrArea].mtAddr;
    adjustOffset(rootAddrArea, rootOffset);
    createStructsRelocation(rootAddrArea, offsetof(exportDescription, rootFunction));

    // Addresses of the areas within memtable.
    for (i = 0; i < memTableEntries; i++)
    {
        createStructsRelocation(i,
            sizeof(exportDescription) + i * sizeof(memoryTableEntry) + offsetof(memoryTableEntry, mtAddr));
    }
    sections[memTableEntries].nreloc = relocationCount;

    // The symbol table.

    symTab.symoff = ftell(exportFile);
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

    exportDescription exports;
    memset(&exports, 0, sizeof(exports));
    exports.structLength = sizeof(exportDescription);
    exports.memTableSize = sizeof(memoryTableEntry);
    exports.memTableEntries = memTableEntries;
    exports.memTable = (memoryTableEntry *)sizeof(exportDescription); // It follows immediately after this.
    // Set the value to be the offset relative to the base of the area.  We have set a relocation
    // already which will add the base of the area.
    exports.rootFunction = (void*)rootOffset;
    exports.timeStamp = getBuildTime();
    exports.architecture = machineDependent->MachineArchitecture();
    exports.rtsVersion = POLY_version_number;

    sections[memTableEntries].offset = ftell(exportFile);
    fwrite(&exports, sizeof(exports), 1, exportFile);
    POLYUNSIGNED addrOffset = sizeof(exports)+sizeof(memoryTableEntry)*memTableEntries;
    for (i = 0; i < memTableEntries; i++)
    {
        void *save = memTable[i].mtAddr;
        memTable[i].mtAddr = (void*)addrOffset; // Set this to the relative address.
        addrOffset += memTable[i].mtLength;
        fwrite(&memTable[i], sizeof(memoryTableEntry), 1, exportFile);
        memTable[i].mtAddr = save;
    }

    // Now the binary data.
    for (i = 0; i < memTableEntries; i++)
    {
        alignFile(4);
        sections[i].offset = ftell(exportFile);
        fwrite(memTable[i].mtAddr, 1, memTable[i].mtLength, exportFile);
    }
    // Rewind to rewrite the headers with the actual offsets.
    rewind(exportFile);
    fwrite(&fhdr, sizeof(fhdr), 1, exportFile); // File header
    fwrite(&sHdr, sizeof(sHdr), 1, exportFile); // Segment header
    fwrite(sections, sectionSize * (memTableEntries+1), 1, exportFile); // Section headers
    fwrite(&symTab, sizeof(symTab), 1, exportFile); // Symbol table header
    fclose(exportFile); exportFile = NULL;

    delete[](sections);
}
