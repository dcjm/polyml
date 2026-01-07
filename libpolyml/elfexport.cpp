/*
    Title:     Write out a database as an ELF object file
    Author:    David Matthews.

    Copyright (c) 2006-7, 2011, 2016-18, 2020-21, 2025 David C. J. Matthews

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

#ifdef HAVE_ELF_H
#include <elf.h>
#elif defined(HAVE_ELF_ABI_H)
#include <elf_abi.h>
#endif

#ifdef HAVE_MACHINE_RELOC_H
#include <machine/reloc.h>

#ifndef EM_X86_64
#define EM_X86_64 EM_AMD64
#endif

#if defined(HOSTARCHITECTURE_X86_64)

#ifndef R_386_PC32
#define R_386_PC32 R_X86_64_PC32
#endif

#ifndef R_386_32
#define R_386_32 R_X86_64_32
#endif

#ifndef R_X86_64_64
#define R_X86_64_64 R_X86_64_64
#endif

#endif /* HOSTARCHITECTURE_X86_64 */

#endif

// Solaris seems to put processor-specific constants in separate files
#ifdef HAVE_SYS_ELF_SPARC_H
#include <sys/elf_SPARC.h>
#endif
#ifdef HAVE_SYS_ELF_386_H
#include <sys/elf_386.h>
#endif
#ifdef HAVE_SYS_ELF_AMD64_H
#include <sys/elf_amd64.h>
#endif

// NetBSD relocation symbols
#ifdef HAVE_I386_ELF_MACHDEP_H
#include <i386/elf_machdep.h>
#endif

// Work around problems in NetBSD
#if (defined(R_AARCH_LDST64_ABS_LO12_NC) && !defined(R_AARCH64_LDST64_ABS_LO12_NC))
#define R_AARCH64_LDST64_ABS_LO12_NC R_AARCH_LDST64_ABS_LO12_NC
#endif
#if (defined(R_AARCH_LDST32_ABS_LO12_NC) && !defined(R_AARCH64_LDST32_ABS_LO12_NC))
#define R_AARCH64_LDST32_ABS_LO12_NC R_AARCH_LDST32_ABS_LO12_NC
#endif

// Haiku x86_64 relocation symbols
// The x86 ones are already defined on elf.h
#ifdef HAVE_PRIVATE_SYSTEM_ARCH_X86_64_ARCH_ELF_H
#include <private/system/arch/x86_64/arch_elf.h>
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
#include "elfexport.h"
#include "run_time.h"
#include "version.h"
#include "polystring.h"
#include "memmgr.h"


#define sym_last_local_sym sym_data_section

#if defined(HOSTARCHITECTURE_X86)
# define HOST_E_MACHINE EM_386
# define HOST_DIRECT_DATA_RELOC R_386_32
# define HOST_DIRECT_FPTR_RELOC R_386_32
# define USE_RELA 0
#elif defined(HOSTARCHITECTURE_PPC)
# define HOST_E_MACHINE EM_PPC
# define HOST_DIRECT_DATA_RELOC R_PPC_ADDR32
# define HOST_DIRECT_FPTR_RELOC R_PPC_ADDR32
# define USE_RELA 1
#elif defined(HOSTARCHITECTURE_PPC64)
# define HOST_E_MACHINE EM_PPC64
# define HOST_DIRECT_DATA_RELOC R_PPC64_ADDR64
# define HOST_DIRECT_FPTR_RELOC R_PPC64_ADDR64
# define USE_RELA 1
#elif defined(HOSTARCHITECTURE_S390)
# define HOST_E_MACHINE EM_S390
# define HOST_DIRECT_DATA_RELOC R_390_32
# define HOST_DIRECT_FPTR_RELOC R_390_32
# define USE_RELA 1
#elif defined(HOSTARCHITECTURE_S390X)
# define HOST_E_MACHINE EM_S390
# define HOST_DIRECT_DATA_RELOC R_390_64
# define HOST_DIRECT_FPTR_RELOC R_390_64
# define USE_RELA 1
#elif defined(HOSTARCHITECTURE_SH)
# define HOST_E_MACHINE EM_SH
# define HOST_DIRECT_DATA_RELOC R_SH_DIR32
# define HOST_DIRECT_FPTR_RELOC R_SH_DIR32
# define USE_RELA 1
#elif defined(HOSTARCHITECTURE_SPARC)
# define HOST_E_MACHINE EM_SPARC
# define HOST_DIRECT_DATA_RELOC R_SPARC_32
# define HOST_DIRECT_FPTR_RELOC R_SPARC_32
# define USE_RELA 1
/* Sparc/Solaris, at least 2.8, requires ELF32_Rela relocations.  For some reason,
   though, it adds the value in the location being relocated (as with ELF32_Rel
   relocations) as well as the addend. To be safe, whenever we use an ELF32_Rela
   relocation we always zero the location to be relocated. */
#elif defined(HOSTARCHITECTURE_SPARC64)
# define HOST_E_MACHINE EM_SPARCV9
# define HOST_DIRECT_DATA_RELOC R_SPARC_64
# define HOST_DIRECT_FPTR_RELOC R_SPARC_64
/* Use the most relaxed memory model. At link time, the most restrictive one is
   chosen, so it does no harm to be as permissive as possible here. */
# define HOST_E_FLAGS EF_SPARCV9_RMO
# define USE_RELA 1
#elif defined(HOSTARCHITECTURE_X86_64)
/* It seems Solaris/X86-64 only supports ELF64_Rela relocations.  It appears that
   Linux will support either so we now use Rela on X86-64. */
# define HOST_E_MACHINE EM_X86_64
# define HOST_DIRECT_DATA_RELOC R_X86_64_64
# define HOST_DIRECT_FPTR_RELOC R_X86_64_64
# define USE_RELA 1
#elif defined(HOSTARCHITECTURE_X32)
# define HOST_E_MACHINE EM_X86_64
# define HOST_DIRECT_DATA_RELOC R_X86_64_32
# define HOST_DIRECT_FPTR_RELOC R_X86_64_32
# define USE_RELA 1
#elif defined(HOSTARCHITECTURE_ARM)
# ifndef EF_ARM_EABI_VER4
#  define EF_ARM_EABI_VER4     0x04000000
# endif
// When linking ARM binaries the linker checks the ABI version.  We
// need to set the version to the same as the libraries.
// GCC currently uses version 4.
# define HOST_E_MACHINE EM_ARM
# define HOST_DIRECT_DATA_RELOC R_ARM_ABS32
# define HOST_DIRECT_FPTR_RELOC R_ARM_ABS32
# define USE_RELA 0
# define HOST_E_FLAGS EF_ARM_EABI_VER4
#elif defined(HOSTARCHITECTURE_HPPA)
# if defined(__hpux)
#  define HOST_OSABI ELFOSABI_HPUX
# elif defined(__NetBSD__)
#  define HOST_OSABI ELFOSABI_NETBSD
# elif defined(__linux__)
#  define HOST_OSABI ELFOSABI_GNU
# endif
# define HOST_E_MACHINE EM_PARISC
# define HOST_DIRECT_DATA_RELOC R_PARISC_DIR32
# define HOST_DIRECT_FPTR_RELOC R_PARISC_PLABEL32
# define HOST_E_FLAGS EFA_PARISC_1_0
# define USE_RELA 1
#elif defined(HOSTARCHITECTURE_IA64)
# define HOST_E_MACHINE EM_IA_64
# define HOST_DIRECT_DATA_RELOC R_IA64_DIR64LSB
# define HOST_DIRECT_FPTR_RELOC R_IA64_FPTR64LSB
# define HOST_E_FLAGS EF_IA_64_ABI64
# define USE_RELA 1
#elif defined(HOSTARCHITECTURE_AARCH64)
# define HOST_E_MACHINE EM_AARCH64
# define HOST_DIRECT_DATA_RELOC R_AARCH64_ABS64
# define HOST_DIRECT_FPTR_RELOC R_AARCH64_ABS64
# define USE_RELA 1
#elif defined(HOSTARCHITECTURE_M68K)
# define HOST_E_MACHINE EM_68K
# define HOST_DIRECT_DATA_RELOC R_68K_32
# define HOST_DIRECT_FPTR_RELOC R_68K_32
# define USE_RELA 1
#elif defined(HOSTARCHITECTURE_MIPS)
# define HOST_E_MACHINE EM_MIPS
# define HOST_DIRECT_DATA_RELOC R_MIPS_32
# define HOST_DIRECT_FPTR_RELOC R_MIPS_32
# ifdef __PIC__
#  define HOST_E_FLAGS EF_MIPS_CPIC
# endif
# define USE_RELA 1
#elif defined(HOSTARCHITECTURE_MIPS64)
# define HOST_E_MACHINE EM_MIPS
# define HOST_DIRECT_DATA_RELOC R_MIPS_64
# define HOST_DIRECT_FPTR_RELOC R_MIPS_64
# ifdef __PIC__
#  define HOST_E_FLAGS (EF_MIPS_ARCH_64 | EF_MIPS_CPIC)
# else
#  define HOST_E_FLAGS EF_MIPS_ARCH_64
# endif
# define USE_RELA 1
#elif defined(HOSTARCHITECTURE_ALPHA)
# define HOST_E_MACHINE EM_ALPHA
# define HOST_DIRECT_DATA_RELOC R_ALPHA_REFQUAD
# define HOST_DIRECT_FPTR_RELOC R_ALPHA_REFQUAD
# define USE_RELA 1
#elif defined(HOSTARCHITECTURE_RISCV32) || defined(HOSTARCHITECTURE_RISCV64)
# define HOST_E_MACHINE EM_RISCV
# if defined(HOSTARCHITECTURE_RISCV32)
#  define HOST_DIRECT_DATA_RELOC R_RISCV_32
#  define HOST_DIRECT_FPTR_RELOC R_RISCV_32
# else
#  define HOST_DIRECT_DATA_RELOC R_RISCV_64
#  define HOST_DIRECT_FPTR_RELOC R_RISCV_64
# endif
# if defined(__riscv_float_abi_soft)
#  define HOST_E_FLAGS_FLOAT_ABI EF_RISCV_FLOAT_ABI_SOFT
# elif defined(__riscv_float_abi_single)
#  define HOST_E_FLAGS_FLOAT_ABI EF_RISCV_FLOAT_ABI_SINGLE
# elif defined(__riscv_float_abi_double)
#  define HOST_E_FLAGS_FLOAT_ABI EF_RISCV_FLOAT_ABI_DOUBLE
# elif defined(__riscv_float_abi_quad)
#  define HOST_E_FLAGS_FLOAT_ABI EF_RISCV_FLOAT_ABI_QUAD
# else
#  error "Unknown RISC-V float ABI"
# endif
# ifdef __riscv_32e
#  define HOST_E_FLAGS_RVE __riscv_32e
# else
#  define HOST_E_FLAGS_RVE 0
# endif
# define HOST_E_FLAGS (HOST_E_FLAGS_FLOAT_ABI | HOST_E_FLAGS_RVE)
# define USE_RELA 1
#elif defined(HOSTARCHITECTURE_LOONGARCH64) 
#  define HOST_E_MACHINE EM_LOONGARCH
#  define HOST_DIRECT_DATA_RELOC R_LARCH_64
#  define HOST_DIRECT_FPTR_RELOC R_LARCH_64
# if defined(__loongarch_soft_float)
#  define HOST_E_FLAGS_FLOAT_ABI EF_LARCH_ABI_SOFT_FLOAT
# elif defined(__loongarch_single_float)
#  define HOST_E_FLAGS_FLOAT_ABI EF_LARCH_ABI_SINGLE_FLOAT
# elif defined(__loongarch_double_float)
#  define HOST_E_FLAGS_FLOAT_ABI EF_LARCH_ABI_DOUBLE_FLOAT
# else
#  error "Unknown LoongArch float ABI"
# endif
# define HOST_E_FLAGS (HOST_E_FLAGS_FLOAT_ABI | EF_LARCH_OBJABI_V1)
# define USE_RELA 1
#else
# error "No support for exporting on this architecture"
#endif

// The first two symbols are special:
// Zero is always special in ELF
// 1 is used for the data section
#define EXTRA_SYMBOLS   2
static unsigned AreaToSym(unsigned area) { return area+EXTRA_SYMBOLS; }

// Section table entries
enum {
    sect_initial = 0,
    sect_sectionnametable,
    sect_stringtable,
    // Data and relocation entries come in here.
    sect_data
    // Finally the symbol table
};

// Add an external reference to the RTS
void ELFExport::addExternalReference(void *relocAddr, const char *name, bool isFuncPtr)
{
    externTable.makeEntry(name);
    // The symbol is added after the memory table entries and poly_exports
    writeRelocation(0, relocAddr, symbolNum++, isFuncPtr);
}

// Generate the address relative to the start of the segment.
void ELFExport::setRelocationAddress(void *p, ElfXX_Addr *reloc)
{
    unsigned area = findArea(p);
    POLYUNSIGNED offset = (char*)p - (char*)memTable[area].mtOriginalAddr;
    *reloc = offset;
}

/* Get the index corresponding to an address. */
PolyWord ELFExport::createRelocation(PolyWord p, void *relocAddr)
{
    void *addr = p.AsAddress();
    unsigned addrArea = findArea(addr);
    POLYUNSIGNED offset = (char*)addr - (char*)memTable[addrArea].mtOriginalAddr;
    return writeRelocation(offset, relocAddr, AreaToSym(addrArea), false);
}

PolyWord ELFExport::writeRelocation(POLYUNSIGNED offset, void *relocAddr, unsigned symbolNum, bool isFuncPtr)
{
#if USE_RELA
    ElfXX_Rela reloc;
    reloc.r_addend = offset;
    offset = 0;
#else
    ElfXX_Rel reloc;
#endif
    // Set the offset within the section we're scanning.
    setRelocationAddress(relocAddr, &reloc.r_offset);
#ifdef HOSTARCHITECTURE_MIPS64
    reloc.r_sym = symbolNum;
    reloc.r_ssym = 0;
    reloc.r_type = isFuncPtr ? HOST_DIRECT_FPTR_RELOC : HOST_DIRECT_DATA_RELOC;
    reloc.r_type2 = 0;
    reloc.r_type3 = 0;
#else
    reloc.r_info = ELFXX_R_INFO(symbolNum, isFuncPtr ? HOST_DIRECT_FPTR_RELOC : HOST_DIRECT_DATA_RELOC);
#endif
    fwrite(&reloc, sizeof(reloc), 1, exportFile);
    relocationCount++;
    return PolyWord::FromUnsigned(offset);
}

/* This is called for each constant within the code. 
   Print a relocation entry for the word and return a value that means
   that the offset is saved in original word. */
void ELFExport::ScanConstant(PolyObject *base, byte *addr, ScanRelocationKind code, intptr_t displacement)
{
#ifndef POLYML32IN64
    PolyObject *p = GetConstantValue(addr, code, displacement);

    if (p == 0)
        return;

    void *a = p;
    unsigned aArea = findArea(a);

    // We don't need a relocation if this is relative to the current segment
    // since the relative address will already be right.
    if (code == PROCESS_RELOC_I386RELATIVE && aArea == findArea(addr))
        return;

    // Set the value at the address to the offset relative to the symbol.
    POLYUNSIGNED offset = (char*)a - (char*)memTable[aArea].mtOriginalAddr;

    switch (code)
    {
    case PROCESS_RELOC_DIRECT: // 32 or 64 bit address of target
        {
            PolyWord r = createRelocation(p, addr);
            POLYUNSIGNED w = r.AsUnsigned();
            for (unsigned i = 0; i < sizeof(PolyWord); i++)
            {
                addr[i] = (byte)(w & 0xff);
                w >>= 8;
            }
        }
        break;
#if(defined(HOSTARCHITECTURE_X86) || defined(HOSTARCHITECTURE_X86_64) || \
        defined(HOSTARCHITECTURE_X32))
#ifdef HOSTARCHITECTURE_X86
#define R_PC_RELATIVE R_386_PC32
#else
#define R_PC_RELATIVE R_X86_64_PC32
#endif
     case PROCESS_RELOC_I386RELATIVE:         // 32 bit relative address
        {
            // We seem to need to subtract 4 bytes to get the correct offset in ELF
            offset -= 4;
#if USE_RELA
            ElfXX_Rela reloc;
            reloc.r_addend = offset;
#else
            ElfXX_Rel reloc;
#endif
            setRelocationAddress(addr, &reloc.r_offset);
            reloc.r_info = ELFXX_R_INFO(AreaToSym(aArea), R_PC_RELATIVE);
            byte *writAble = gMem.SpaceForAddress(addr)->writeAble(addr);
#if USE_RELA
            // Clear the field.  Even though it's not supposed to be used with Rela the
            // Linux linker at least seems to add the value in here sometimes.
            memset(writAble, 0, 4);
#else
            for (unsigned i = 0; i < 4; i++)
            {
                writAble[i] = (byte)(offset & 0xff);
                offset >>= 8;
            }
#endif
            fwrite(&reloc, sizeof(reloc), 1, exportFile);
            relocationCount++;
        }
        break;
#endif
#if defined(HOSTARCHITECTURE_AARCH64)
     case PROCESS_RELOC_ARM64ADRPLDR64:
     case PROCESS_RELOC_ARM64ADRPLDR32:
     case PROCESS_RELOC_ARM64ADRPADD:
     {
             ElfXX_Rela reloc;
             reloc.r_addend = offset;
             setRelocationAddress(addr, &reloc.r_offset);
             reloc.r_info = ELFXX_R_INFO(AreaToSym(aArea), R_AARCH64_ADR_PREL_PG_HI21);
             fwrite(&reloc, sizeof(reloc), 1, exportFile);
             relocationCount++;
             setRelocationAddress(addr+4, &reloc.r_offset);
             int relType =
                 code == PROCESS_RELOC_ARM64ADRPLDR64 ? R_AARCH64_LDST64_ABS_LO12_NC :
                 code == PROCESS_RELOC_ARM64ADRPLDR32 ? R_AARCH64_LDST32_ABS_LO12_NC :
                     R_AARCH64_ADD_ABS_LO12_NC;
             reloc.r_info = ELFXX_R_INFO(AreaToSym(aArea), relType);
             fwrite(&reloc, sizeof(reloc), 1, exportFile);
             relocationCount++;
             // Clear the offsets within the instruction just in case 
             uint32_t* writAble = (uint32_t *)gMem.SpaceForAddress(addr)->writeAble(addr);
             writAble[0] = toARMInstr(fromARMInstr(writAble[0]) & 0x9f00001f);
             writAble[1] = toARMInstr(fromARMInstr(writAble[1]) & 0xffc003ff);
        }
        break;
#endif
        default:
            ASSERT(0); // Wrong type of relocation for this architecture.
    }
#endif
}

unsigned long ELFExport::makeStringTableEntry(const char *str, ExportStringTable *stab)
{
    if (str == NULL || str[0] == 0)
        return 0; // First entry is the null string.
    else
        return stab->makeEntry(str);
}

void ELFExport::writeSymbol(const char *symbolName, long value, long size, int binding, int sttype, int section)
{
    ElfXX_Sym symbol;
    memset(&symbol, 0, sizeof(symbol)); // Zero unused fields
    symbol.st_name = makeStringTableEntry(symbolName, &symStrings);
    symbol.st_value = value;
    symbol.st_size = size;
    symbol.st_info = ELFXX_ST_INFO(binding, sttype);
    symbol.st_other = 0;
    symbol.st_shndx = section;
    fwrite(&symbol, sizeof(symbol), 1, exportFile);
}

// Set the file alignment.
void ELFExport::alignFile(int align)
{
    char pad[32] = {0}; // Maximum alignment
    int offset = ftell(exportFile);
    if ((offset % align) == 0) return;
    fwrite(&pad, align - (offset % align), 1, exportFile);
}

void ELFExport::createStructsRelocation(unsigned sym, size_t offset, size_t addend)
{
#if USE_RELA
    ElfXX_Rela reloc;
    reloc.r_addend = addend;
#else
    ElfXX_Rel reloc;
#endif
    reloc.r_offset = offset;
#ifdef HOSTARCHITECTURE_MIPS64
    reloc.r_sym = sym;
    reloc.r_ssym = 0;
    reloc.r_type = HOST_DIRECT_DATA_RELOC;
    reloc.r_type2 = 0;
    reloc.r_type3 = 0;
#else
    reloc.r_info = ELFXX_R_INFO(sym, HOST_DIRECT_DATA_RELOC);
#endif
    fwrite(&reloc, sizeof(reloc), 1, exportFile);
    relocationCount++;
}

void ELFExport::exportStore(void)
{
    PolyWord    *p;
    ElfXX_Ehdr fhdr;
    ElfXX_Shdr *sections = 0;
#ifdef __linux__
    unsigned extraSections = 1; // Extra section for .note.GNU-stack
#else
    unsigned extraSections = 0;
#endif
    unsigned numSections = 0;
    for (unsigned j = 0; j < memTableEntries; j++)
    {
        if ((memTable[j].mtFlags & (MTF_BYTES|MTF_WRITEABLE)) == MTF_BYTES)
            numSections += 1;
        else numSections += 2;
    }
        // The symbol table comes at the end.
    unsigned sect_symtab = sect_data + numSections + 2;
    numSections += 6 + extraSections;
    
    // External symbols start after the memory table entries and "poly_exports".
    symbolNum = EXTRA_SYMBOLS+memTableEntries+1;

    // Both the string tables have an initial null entry.
    symStrings.makeEntry("");
    sectionStrings.makeEntry("");

    // Write out initial values for the headers.  These are overwritten at the end.
    // File header
    memset(&fhdr, 0, sizeof(fhdr));
    fhdr.e_ident[EI_MAG0] = 0x7f;
    fhdr.e_ident[EI_MAG1] = 'E';
    fhdr.e_ident[EI_MAG2] = 'L';
    fhdr.e_ident[EI_MAG3] = 'F';
    fhdr.e_ident[EI_CLASS] = ELFCLASSXX; // ELFCLASS32 or ELFCLASS64
    fhdr.e_ident[EI_VERSION] = EV_CURRENT;
#ifdef HOST_OSABI
    fhdr.e_ident[EI_OSABI] = HOST_OSABI;
#endif
    {
        union { unsigned long wrd; char chrs[sizeof(unsigned long)]; } endian;
        endian.wrd = 1;
        if (endian.chrs[0] == 0)
            fhdr.e_ident[EI_DATA] = ELFDATA2MSB; // Big endian
        else
            fhdr.e_ident[EI_DATA] = ELFDATA2LSB; // Little endian
    }
    fhdr.e_type = ET_REL;
    // The machine needs to match the machine we're compiling for
    // even if this is actually portable code.
    fhdr.e_machine = HOST_E_MACHINE;
#ifdef HOST_E_FLAGS
    fhdr.e_flags = HOST_E_FLAGS;
#endif
    fhdr.e_version = EV_CURRENT;
    fhdr.e_shoff = sizeof(fhdr); // Offset to section header - immediately follows
    fhdr.e_ehsize = sizeof(fhdr);
    fhdr.e_shentsize = sizeof(ElfXX_Shdr);
    fhdr.e_shnum = numSections;
    fhdr.e_shstrndx = sect_sectionnametable; // Section name table section index;
    fwrite(&fhdr, sizeof(fhdr), 1, exportFile); // Write it for the moment.

    sections = new ElfXX_Shdr[numSections];
    memset(sections, 0, sizeof(ElfXX_Shdr) * numSections); // Necessary?

    // Set up the section header but don't write it yet.

    // Section 0 - all zeros
    sections[sect_initial].sh_type = SHT_NULL;
    sections[sect_initial].sh_link = SHN_UNDEF;

    // Section name table.
    sections[sect_sectionnametable].sh_name = makeStringTableEntry(".shstrtab", &sectionStrings);
    sections[sect_sectionnametable].sh_type = SHT_STRTAB;
    sections[sect_sectionnametable].sh_addralign = sizeof(char);
    // sections[sect_sectionnametable].sh_offset is set later
    // sections[sect_sectionnametable].sh_size is set later

    // Symbol name table.
    sections[sect_stringtable].sh_name = makeStringTableEntry(".strtab", &sectionStrings);
    sections[sect_stringtable].sh_type = SHT_STRTAB;
    sections[sect_stringtable].sh_addralign = sizeof(char);
    // sections[sect_stringtable].sh_offset is set later
    // sections[sect_stringtable].sh_size is set later

    unsigned long dataName = makeStringTableEntry(".data", &sectionStrings);
    unsigned long dataRelName = makeStringTableEntry(USE_RELA ? ".rela.data" : ".rel.data", &sectionStrings);
#ifndef CODEISNOTEXECUTABLE
    unsigned long textName = makeStringTableEntry(".text", &sectionStrings);
    unsigned long textRelName = makeStringTableEntry(USE_RELA ? ".rela.text" : ".rel.text", &sectionStrings);
#endif
    // The Linux linker does not like relocations in the .rodata section and marks the executable
    // as containing text relocations.  Putting the data in a .data.rel.ro section seems to work.
    unsigned long relDataName = makeStringTableEntry(".data.rel.ro", &sectionStrings);
    unsigned long relDataRelName = makeStringTableEntry(USE_RELA ? ".rela.data.rel.ro" : ".rel.data.rel.ro", &sectionStrings);
    // Byte and other leaf data that do not require relocation can go in the .rodata section
    unsigned long nRelDataName = makeStringTableEntry(".rodata", &sectionStrings);

    // Main data sections.  Each one has a relocation section.
    unsigned s = sect_data;
    for (unsigned i=0; i < memTableEntries; i++)
    {
        sections[s].sh_addralign = 8; // 8-byte alignment
        sections[s].sh_type = SHT_PROGBITS;
        if (memTable[i].mtFlags & MTF_WRITEABLE)
        {
            // Mutable areas
            ASSERT(!(memTable[i].mtFlags & MTF_EXECUTABLE)); // Executable areas can't be writable.
            sections[s].sh_name = dataName;
            sections[s].sh_flags = SHF_WRITE | SHF_ALLOC;
            s++;
            // Mutable byte areas can contain external references so need relocation
            sections[s].sh_name = dataRelName; // Name of relocation section
        }
#ifndef CODEISNOTEXECUTABLE
        // Not if we're building the interpreted version.
        else if (memTable[i].mtFlags & MTF_EXECUTABLE)
        {
            // Code areas are marked as executable.
            sections[s].sh_name = textName;
            sections[s].sh_flags = SHF_ALLOC | SHF_EXECINSTR;
            s++;
            sections[s].sh_name = textRelName; // Name of relocation section
        }
#endif
        else if (memTable[i].mtFlags & MTF_BYTES)
        {
            // Data that does not require relocation.
            // Non-code immutable areas
            sections[s].sh_name = nRelDataName;
            sections[s].sh_flags = SHF_ALLOC;
            s++;
            continue; // Skip the relocation section for this
        }
        else
        {
            // Non-code immutable areas
            sections[s].sh_name = relDataName;
            // The .data.rel.ro has to be writable in order to be relocated.
            // It is set to read-only after relocation.
            sections[s].sh_flags = SHF_WRITE | SHF_ALLOC;
            s++;
            sections[s].sh_name = relDataRelName; // Name of relocation section
        }
        // sections[s].sh_size is set later
        // sections[s].sh_offset is set later.
        // sections[s].sh_size is set later.

        // Relocation section
        sections[s].sh_type = USE_RELA ? SHT_RELA : SHT_REL; // Contains relocation with/out explicit addends (ElfXX_Rel)
        sections[s].sh_link = sect_symtab; // Index to symbol table
        sections[s].sh_info = s-1; // Applies to the data section
        sections[s].sh_addralign = sizeof(long); // Align to a word
        sections[s].sh_entsize = USE_RELA ? sizeof(ElfXX_Rela) : sizeof(ElfXX_Rel);
        s++;
        // sections[s+1].sh_offset is set later.
        // sections[s+1].sh_size is set later.
    }

    // Table data - Poly tables that describe the memory layout.
    unsigned sect_table_data = s;

    sections[sect_table_data].sh_name = dataName;
    sections[sect_table_data].sh_type = SHT_PROGBITS;
    sections[sect_table_data].sh_flags = SHF_WRITE | SHF_ALLOC;
    sections[sect_table_data].sh_addralign = 8; // 8-byte alignment
    // Table relocation
    sections[sect_table_data+1].sh_name = dataRelName;
    sections[sect_table_data+1].sh_type = USE_RELA ? SHT_RELA : SHT_REL; // Contains relocation with/out explicit addends (ElfXX_Rel)
    sections[sect_table_data+1].sh_link = sect_symtab; // Index to symbol table
    sections[sect_table_data+1].sh_info = sect_table_data; // Applies to table section
    sections[sect_table_data+1].sh_addralign = sizeof(long); // Align to a word
    sections[sect_table_data+1].sh_entsize = USE_RELA ? sizeof(ElfXX_Rela) : sizeof(ElfXX_Rel);

    // Symbol table.
    sections[sect_symtab].sh_name = makeStringTableEntry(".symtab", &sectionStrings);
    sections[sect_symtab].sh_type = SHT_SYMTAB;
    sections[sect_symtab].sh_link = sect_stringtable; // String table to use
    sections[sect_symtab].sh_addralign = sizeof(long); // Align to a word
    sections[sect_symtab].sh_entsize = sizeof(ElfXX_Sym);
    // sections[sect_symtab].sh_info is set later
    // sections[sect_symtab].sh_size is set later
    // sections[sect_symtab].sh_offset is set later

#ifdef __linux__
    // Add a .note.GNU-stack section to indicate this does not require executable stack
    sections[numSections-1].sh_name = makeStringTableEntry(".note.GNU-stack", &sectionStrings);
    sections[numSections - 1].sh_type = SHT_PROGBITS;
#endif

    // Write the relocations.
    unsigned relocSection = sect_data;
    for (unsigned i = 0; i < memTableEntries; i++)
    {
        relocSection++;
        if ((memTable[i].mtFlags & (MTF_BYTES|MTF_WRITEABLE)) == MTF_BYTES)
            continue;
        alignFile(sections[relocSection].sh_addralign);
        sections[relocSection].sh_offset = ftell(exportFile);
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
        sections[relocSection].sh_size =
            relocationCount * (USE_RELA ? sizeof(ElfXX_Rela) : sizeof(ElfXX_Rel));
        relocSection++;
    }

    // Relocations for "exports" and "memTable";
    alignFile(sections[sect_table_data+1].sh_addralign);
    sections[sect_table_data+1].sh_offset = ftell(exportFile);
    relocationCount = 0;
    // TODO: This won't be needed if we put these in a separate section.
    POLYUNSIGNED areaSpace = 0;
    for (unsigned i = 0; i < memTableEntries; i++)
        areaSpace += memTable[i].mtLength;

    // Address of "memTable" within "exports". We can't use createRelocation because
    // the position of the relocation is not in either the mutable or the immutable area.
    size_t memTableOffset = sizeof(exportDescription); // It follows immediately after this.
    createStructsRelocation(AreaToSym(memTableEntries), offsetof(exportDescription, memTable), memTableOffset);

    // Address of "rootFunction" within "exports"
    unsigned rootAddrArea = findArea(rootFunction);
    size_t rootOffset = (char*)rootFunction - (char*)memTable[rootAddrArea].mtOriginalAddr;
    createStructsRelocation(AreaToSym(rootAddrArea), offsetof(exportDescription, rootFunction), rootOffset);

    // Addresses of the areas within memtable.
    for (unsigned i = 0; i < memTableEntries; i++)
    {
        createStructsRelocation(AreaToSym(i),
            sizeof(exportDescription) + i * sizeof(memoryTableEntry) + offsetof(memoryTableEntry, mtCurrentAddr),
            0 /* No offset relative to base symbol*/);
    }

    sections[sect_table_data+1].sh_size =
        relocationCount * (USE_RELA ? sizeof(ElfXX_Rela) : sizeof(ElfXX_Rel));

    // Now the symbol table.
    alignFile(sections[sect_symtab].sh_addralign);
    sections[sect_symtab].sh_offset = ftell(exportFile);
    writeSymbol("", 0, 0, 0, 0, 0); // Initial symbol
    // Write the local symbols first.
    writeSymbol("", 0, 0, STB_LOCAL, STT_SECTION, sect_data); // .data section

    // Create symbols for the address areas.  AreaToSym assumes these come first.
    s = sect_data;
    for (unsigned i = 0; i < memTableEntries; i++)
    {
        char buff[50];
        sprintf(buff, "area%1u", i);
        writeSymbol(buff, 0, 0, STB_LOCAL, STT_OBJECT, s);
        if ((memTable[i].mtFlags & (MTF_BYTES|MTF_WRITEABLE)) == MTF_BYTES)
            s += 1;
        else s += 2;
    }

    // Global symbols - Exported symbol for table.
    writeSymbol("poly_exports", 0, 
        sizeof(exportDescription)+sizeof(memoryTableEntry)*memTableEntries,
        STB_GLOBAL, STT_OBJECT, sect_table_data);
 
    // External references
    for (unsigned i = 0; i < externTable.stringSize; i += (unsigned)strlen(externTable.strings+i) + 1)
        writeSymbol(externTable.strings+i, 0, 0, STB_GLOBAL, STT_FUNC, SHN_UNDEF);

    sections[sect_symtab].sh_info = EXTRA_SYMBOLS+memTableEntries; // One more than last local sym
    sections[sect_symtab].sh_size = sizeof(ElfXX_Sym) * symbolNum;

    // Now the binary data.
    unsigned dataSection = sect_data;
    for (unsigned i = 0; i < memTableEntries; i++)
    {
        sections[dataSection].sh_size = memTable[i].mtLength;
        alignFile(sections[dataSection].sh_addralign);
        sections[dataSection].sh_offset = ftell(exportFile);
        fwrite(memTable[i].mtOriginalAddr, 1, memTable[i].mtLength, exportFile);
        if ((memTable[i].mtFlags & (MTF_BYTES|MTF_WRITEABLE)) == MTF_BYTES)
            dataSection += 1;
        else dataSection += 2;
    }

    exportDescription exports;
    memset(&exports, 0, sizeof(exports));
    exports.structLength = sizeof(exportDescription);
    exports.memTableSize = sizeof(memoryTableEntry);
    exports.memTableEntries = memTableEntries;
    exports.memTable = USE_RELA ? 0 : (memoryTableEntry *)memTableOffset;
    // Set the value to be the offset relative to the base of the area.  We have set a relocation
    // already which will add the base of the area.
    exports.rootFunction = USE_RELA ? 0 : (void*)rootOffset;
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

    // Now the binary data.
    alignFile(sections[sect_table_data].sh_addralign);
    sections[sect_table_data].sh_offset = ftell(exportFile);
    sections[sect_table_data].sh_size = sizeof(exportDescription) + memTableEntries*sizeof(memoryTableEntry);

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

    // The section name table
    sections[sect_sectionnametable].sh_offset = ftell(exportFile);
    fwrite(sectionStrings.strings, sectionStrings.stringSize, 1, exportFile);
    sections[sect_sectionnametable].sh_size = sectionStrings.stringSize;

    // The symbol name table
    sections[sect_stringtable].sh_offset = ftell(exportFile);
    fwrite(symStrings.strings, symStrings.stringSize, 1, exportFile);
    sections[sect_stringtable].sh_size = symStrings.stringSize;

    // Finally the section headers.
    alignFile(4);
    fhdr.e_shoff = ftell(exportFile);
    fwrite(sections, sizeof(ElfXX_Shdr) * numSections, 1, exportFile);

    // Rewind to rewrite the file header with the offset of the section headers.
    rewind(exportFile);
    fwrite(&fhdr, sizeof(fhdr), 1, exportFile);
    fclose(exportFile); exportFile = NULL;

    delete[]sections;
}
