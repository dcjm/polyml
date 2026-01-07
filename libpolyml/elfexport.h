/*
    Title:     Export memory as an ELF object file
    Author:    David C. J. Matthews.

    Copyright (c) 2006, 2016-17, 2020 David C. J. Matthews


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

#ifndef ELFExport_H_INCLUDED
#define ELFExport_H_INCLUDED

#include "config.h"

#include "scanaddrs.h" // For base class
#include "exporter.h"

#ifdef HAVE_ELF_H
#include <elf.h>
#endif

#ifdef HAVE_ELF_ABI_H
#include <elf_abi.h>
#endif

// Select 32 or 64 bit version depending on the word length
#if (SIZEOF_VOIDP == 8)
#define ElfXX_Addr  Elf64_Addr
#define ElfXX_Rel   Elf64_Rel
#define ElfXX_Rela  Elf64_Rela
#define ElfXX_Sym   Elf64_Sym
#define ElfXX_Ehdr  Elf64_Ehdr
#define ElfXX_Shdr  Elf64_Shdr
// Include a cast on the next line.  Linux includes this anyway but it's needed for FreeBSD.
#define ELFXX_R_INFO(_y, _z)    ELF64_R_INFO((Elf64_Xword)(_y), _z)
#define ELFXX_ST_INFO(_y, _z)   ELF64_ST_INFO(_y, _z)
#define ELFCLASSXX      ELFCLASS64
#else
#define ElfXX_Addr  Elf32_Addr
#define ElfXX_Rel   Elf32_Rel
#define ElfXX_Rela  Elf32_Rela
#define ElfXX_Sym   Elf32_Sym
#define ElfXX_Ehdr  Elf32_Ehdr
#define ElfXX_Shdr  Elf32_Shdr
#define ELFXX_R_INFO(_y, _z)    ELF32_R_INFO(_y, _z)
#define ELFXX_ST_INFO(_y, _z)   ELF32_ST_INFO(_y, _z)
#define ELFCLASSXX              ELFCLASS32
#endif

#ifdef HOSTARCHITECTURE_MIPS64
/* MIPS N64 ABI has a different Elf64_Rel/Rela layout */

typedef struct
{
  Elf64_Addr r_offset;      /* Address */
  Elf64_Word r_sym;         /* Symbol index */
  unsigned char r_ssym;     /* Special symbol */
  unsigned char r_type3;    /* Third relocation type */
  unsigned char r_type2;    /* Second relocation type */
  unsigned char r_type;     /* First relocation type */
} Elf64_Mips_Rel;

typedef struct
{
  Elf64_Addr r_offset;      /* Address */
  Elf64_Word r_sym;         /* Symbol index */
  unsigned char r_ssym;     /* Special symbol */
  unsigned char r_type3;    /* Third relocation type */
  unsigned char r_type2;    /* Second relocation type */
  unsigned char r_type;     /* First relocation type */
  Elf64_Sxword r_addend;    /* Addend */
} Elf64_Mips_Rela;

#undef  ElfXX_Rel
#define ElfXX_Rel   Elf64_Mips_Rel
#undef  ElfXX_Rela
#define ElfXX_Rela  Elf64_Mips_Rela
/* Elf64_Mips_Rel/Rela has no r_info, so this macro is meaningless */
#undef  ELFXX_R_INFO
#endif

class TaskData;

class ELFExport: public Exporter, public ScanAddress
{
public:
    ELFExport(): relocationCount(0), symbolNum(0) {}
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
    void setRelocationAddress(void *p, ElfXX_Addr *reloc);
    PolyWord createRelocation(PolyWord p, void *relocAddr);
    PolyWord writeRelocation(POLYUNSIGNED offset, void *relocAddr, unsigned symbolNum, bool isFuncPtr);
    void writeSymbol(const char *symbolName, long value, long size, int binding, int sttype, int section);
    unsigned long makeStringTableEntry(const char *str, ExportStringTable *stab);
    void alignFile(int align);
    void createStructsRelocation(unsigned area, size_t offset, size_t addend);

    unsigned relocationCount;

    // There are two tables - one is used for section names, the other for symbol names.
    ExportStringTable symStrings, sectionStrings;
    // Table and count for external references.
    ExportStringTable externTable;
    unsigned symbolNum;
};

#endif
