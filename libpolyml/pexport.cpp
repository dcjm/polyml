/*
    Title:     Export and import memory in a portable format
    Author:    David C. J. Matthews.

    Copyright (c) 2006-7, 2015-8, 2020-21, 2025 David C. J. Matthews


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

#ifdef HAVE_STDIO_H
#include <stdio.h>
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

#include "globals.h"
#include "pexport.h"
#include "machine_dep.h"
#include "scanaddrs.h"
#include "run_time.h"
#include "../polyexports.h"
#include "version.h"
#include "sys.h"
#include "polystring.h"
#include "memmgr.h"
#include "rtsentry.h"
#include "mpoly.h" // For polyStderr

/*
This file contains the code both to export the file and to import it
in a new session.
*/

PExport::PExport()
{
}

PExport::~PExport()
{
}


// Get the index corresponding to an address.
size_t PExport::getIndex(PolyObject *p)
{
    // Binary chop to find the index from the address.
    size_t lower = 0, upper = pMap.size();
    while (1)
    {
        ASSERT(lower < upper);
        size_t middle = (lower+upper)/2;
        ASSERT(middle < pMap.size());
        if (p < pMap[middle])
        {
            // Use lower to middle
            upper = middle; 
        }
        else if (p > pMap[middle])
        {
            // Use middle+1 to upper
            lower = middle+1;
        }
        else // Found it
            return middle;
    }
}

/* Get the index corresponding to an address. */
void PExport::printAddress(void *p)
{
    fprintf(exportFile, "@%" PRI_SIZET "", getIndex((PolyObject*)p));
}

void PExport::printValue(PolyWord q)
{
    if (IS_INT(q) || q == PolyWord::FromUnsigned(0))
        fprintf(exportFile, "%" POLYSFMT, UNTAGGED(q));
    else
        printAddress(q.AsAddress());
}

void PExport::printObject(PolyObject *p)
{
    POLYUNSIGNED length = p->Length();
    POLYUNSIGNED i;

    size_t myIndex = getIndex(p);

    fprintf(exportFile, "%" PRI_SIZET ":", myIndex);

    if (p->IsMutable())
        putc('M', exportFile);
    if (OBJ_IS_NEGATIVE(p->LengthWord()))
        putc('N', exportFile);
    if (OBJ_IS_WEAKREF_OBJECT(p->LengthWord()))
        putc('W', exportFile);
    if (OBJ_IS_NO_OVERWRITE(p->LengthWord()))
        putc('V', exportFile);

    if (p->IsByteObject())
    {
        if (p->IsMutable() && p->IsWeakRefObject() && p->Length() >= sizeof(uintptr_t) / sizeof(PolyWord))
        {
            // This is either an entry point or a weak ref used in the FFI.
            // Clear the first word
            if (p->Length() == sizeof(uintptr_t)/sizeof(PolyWord))
                putc('K', exportFile); // Weak ref
            else if (p->Length() > sizeof(uintptr_t) / sizeof(PolyWord))
            {
                // Entry point - C null-terminated string.
                putc('E', exportFile);
                const char* name = (char*)p + sizeof(uintptr_t);
                fprintf(exportFile, "%" PRI_SIZET "|%s", strlen(name), name);
                *(uintptr_t*)p = 0; // Entry point
            }
        }
        else
        {
            /* May be a string, a long format arbitrary precision
               number or a real number. */
            PolyStringObject* ps = (PolyStringObject*)p;
            /* This is not infallible but it seems to be good enough
               to detect the strings. */
            POLYUNSIGNED bytes = length * sizeof(PolyWord);
            if (length >= 2 &&
                ps->length <= bytes - sizeof(POLYUNSIGNED) &&
                ps->length > bytes - 2 * sizeof(POLYUNSIGNED))
            {
                /* Looks like a string. */
                fprintf(exportFile, "S%" POLYUFMT "|", ps->length);
                for (unsigned i = 0; i < ps->length; i++)
                {
                    char ch = ps->chars[i];
                    fprintf(exportFile, "%02x", ch & 0xff);
                }
            }
            else
            {
                /* Not a string. May be an arbitrary precision integer.
                   If the source and destination word lengths differ we
                   could find that some long-format arbitrary precision
                   numbers could be represented in the tagged short form
                   or vice-versa.  The former case might give rise to
                   errors because when comparing two arbitrary precision
                   numbers for equality we assume that they are not equal
                   if they have different representation.  The latter
                   case could be a problem because we wouldn't know whether
                   to convert the tagged form to long form, which would be
                   correct if the value has type "int" or to truncate it
                   which would be correct for "word".
                   It could also be a real number but that doesn't matter
                   if we recompile everything on the new machine.
                */
                byte* u = (byte*)p;
                putc('B', exportFile);
                fprintf(exportFile, "%" PRI_SIZET "|", length * sizeof(PolyWord));
                for (unsigned i = 0; i < (unsigned)(length * sizeof(PolyWord)); i++)
                {
                    fprintf(exportFile, "%02x", u[i]);
                }
            }
        }
    }
    else if (p->IsCodeObject())
    {
        POLYUNSIGNED constCount;
        PolyWord *cp;
        ASSERT(! p->IsMutable() );
        /* Work out the number of bytes in the code and the
           number of constants. */
        machineDependent->GetConstSegmentForCode(p, cp, constCount);
        /* The byte count is the length of the segment minus the
           number of constants minus one for the constant count.
           It includes the marker word, byte count, profile count
           and, on the X86/64 at least, any non-address constants.
           These are actually word values. */
        POLYUNSIGNED byteCount = (length - constCount - 2) * sizeof(PolyWord);
        fprintf(exportFile, "F%" POLYUFMT ",%" POLYUFMT "|", constCount, byteCount);

        // First the code.
        byte *u = (byte*)p;
        for (POLYUNSIGNED i = 0; i < byteCount; i++)
            fprintf(exportFile, "%02x", u[i]);

        putc('|', exportFile);
        // Now the constants.
        for (POLYUNSIGNED i = 0; i < constCount; i++)
        {
            printValue(cp[i]);
            if (i < constCount-1)
                putc(',', exportFile);
        }
        putc('|', exportFile);
        // Finally any constants in the code object.
        machineDependent->ScanConstantsWithinCode(p, this);
    }
    else // Ordinary objects, essentially tuples, or closures.
    {
        if (p->IsClosureObject())
        {
            POLYUNSIGNED nItems = length - sizeof(PolyObject*) / sizeof(PolyWord) + 1;
            fprintf(exportFile, "C%" POLYUFMT "|", nItems); // Number of items
        }
        else fprintf(exportFile, "O%" POLYUFMT "|", length);
        if (p->IsClosureObject())
        {
            // The first word is always a code address.
            printAddress(*(PolyObject**)p);
            i = sizeof(PolyObject*)/sizeof(PolyWord);
            if (i < length)
                putc(',', exportFile);
        }
        else i = 0;
        while (i < length)
        {
            printValue(p->Get(i));
            if (i < length-1)
                putc(',', exportFile);
            i++;
        }
    }
    fprintf(exportFile, "\n");
}

/* This is called for each constant within the code. 
   Print a relocation entry for the word and return a value that means
   that the offset is saved in original word. */
void PExport::ScanConstant(PolyObject *base, byte *addr, ScanRelocationKind code, intptr_t displacement)
{
    PolyObject *p = GetConstantValue(addr, code, displacement);
    if (p == 0) return; // Don't put in tagged constants

    // Put in the byte offset and the relocation type code.
    POLYUNSIGNED offset = (POLYUNSIGNED)(addr - (byte*)base);
    ASSERT (offset < base->Length() * sizeof(POLYUNSIGNED));
    fprintf(exportFile, "%" POLYUFMT ",%d,", (POLYUNSIGNED)(addr - (byte*)base), code);
    printAddress(p); // The value to plug in.
    fprintf(exportFile, " ");
}

void PExport::exportStore(void)
{
    // We want the entries in pMap to be in ascending
    // order of address to make searching easy so we need to process the areas
    // in order of increasing address, which may not be the order in memTable.
    std::vector<size_t> indexOrder;
    indexOrder.reserve(memTableEntries);

    for (size_t i = 0; i < memTableEntries; i++)
    {
        std::vector<size_t>::iterator it;
        for (it = indexOrder.begin(); it != indexOrder.end(); it++) {
            if (memTable[*it].mtOriginalAddr >= memTable[i].mtOriginalAddr)
                break;
        }
        indexOrder.insert(it, i);
    }

    // Process the area in order of ascending address.
    for (std::vector<size_t>::iterator i = indexOrder.begin(); i != indexOrder.end(); i++)
    {
        size_t index = *i;
        char *start = (char*)memTable[index].mtOriginalAddr;
        char *end = start + memTable[index].mtLength;
        for (PolyWord *p = (PolyWord*)start; p < (PolyWord*)end; )
        {
            p++;
            PolyObject *obj = (PolyObject*)p;
            POLYUNSIGNED length = obj->Length();
            pMap.push_back(obj);
            p += length;
        }
    }

    /* Start writing the information. */
    fprintf(exportFile, "Objects\t%" PRI_SIZET "\n", pMap.size());
    char arch = '?';
    switch (machineDependent->MachineArchitecture())
    {
    case MA_Interpreted:
        arch = 'I'; break;
    case MA_I386: case MA_X86_64: case MA_X86_64_32:
        arch = 'X'; break;
    case MA_Arm64: case MA_Arm64_32:
        arch = 'A'; break;
    }
    fprintf(exportFile, "Root\t%" PRI_SIZET " %c %u\n", getIndex(rootFunction), arch, (unsigned)sizeof(PolyWord));

    // Generate each of the areas.
    for (size_t i = 0; i < memTableEntries; i++)
    {
        char *start = (char*)memTable[i].mtOriginalAddr;
        char *end = start + memTable[i].mtLength;
        for (PolyWord *p = (PolyWord*)start; p < (PolyWord*)end; )
        {
            p++;
#ifdef POLYML32IN64
            // We may have filler cells to get the alignment right.
            // We mustn't try to print them.
            if ( ((p-(PolyWord*)0) & (POLYML32IN64-1)) != 0)
                continue;
#endif
            PolyObject* obj = (PolyObject*)p;
            POLYUNSIGNED length = obj->Length();
            printObject(obj);
            p += length;
        }
    }

    fclose(exportFile); exportFile = NULL;
}


/*
Import a portable export file and load it into memory.
Creates "permanent" address entries in the global memory table.
*/

class SpaceAlloc
{
public:
    SpaceAlloc(unsigned *indexCtr, unsigned perms, POLYUNSIGNED def);
    PolyObject *NewObj(POLYUNSIGNED objWords);

    size_t defaultSize;
    PermanentMemSpace *memSpace;
    size_t used;
    unsigned permissions;
    unsigned *spaceIndexCtr;

};

SpaceAlloc::SpaceAlloc(unsigned *indexCtr, unsigned perms, POLYUNSIGNED def)
{
    permissions = perms;
    defaultSize = def;
    memSpace = 0;
    used = 0;
    spaceIndexCtr = indexCtr;
}

// Allocate a new object.  May create a new space and add the old one to the permanent
// memory table if this is exhausted.
#ifndef POLYML32IN64
PolyObject *SpaceAlloc::NewObj(POLYUNSIGNED objWords)
{
    if (memSpace == 0 || memSpace->spaceSize() - used <= objWords)
    {
        // Need some more space.
        size_t size = defaultSize;
        if (size <= objWords)
            size = objWords+1;
        memSpace =
            gMem.AllocateNewPermanentSpace(size * sizeof(PolyWord), permissions, *spaceIndexCtr, ModuleId() /* No sig yet */);
        (*spaceIndexCtr)++;
        // The memory is writable until CompletePermanentSpaceAllocation is called
        if (memSpace == 0)
        {
            fprintf(polyStderr, "Unable to allocate memory\n");
            return 0;
        }
        used = 0;
    }
    ASSERT(memSpace->spaceSize() - used > objWords);
    PolyObject *newObj = (PolyObject*)(memSpace->bottom + used+1);
    used += objWords+1;
    return newObj;
}
#else
// With 32in64 we need to allocate on the correct boundary. 
PolyObject *SpaceAlloc::NewObj(POLYUNSIGNED objWords)
{
    size_t rounded = objWords;
    while (rounded % POLYML32IN64 != POLYML32IN64-1) rounded++;
    if (memSpace == 0 || memSpace->spaceSize() - used <= rounded)
    {
        // Need some more space.
        size_t size = defaultSize;
        if (size <= rounded)
            size = rounded + 1;
        memSpace = gMem.AllocateNewPermanentSpace(size * sizeof(PolyWord), permissions, *spaceIndexCtr, ModuleId());
        (*spaceIndexCtr)++;
        // The memory is writable until CompletePermanentSpaceAllocation is called
        if (memSpace == 0)
        {
            fprintf(stderr, "Unable to allocate memory\n");
            return 0;
        }
        // Zero initial words at the start and align to boundary-1
        used = 0;
        while (used < POLYML32IN64-1)
            memSpace->writeAble(memSpace->bottom)[used++] = PolyWord::FromUnsigned(0);
    }
    PolyObject *newObj = (PolyObject*)(memSpace->bottom + used + 1);
    // Clear any unused words at the end beyond the cell we're allocating.
    for (POLYUNSIGNED i = objWords; i < rounded; i++)
        memSpace->writeAble(newObj)->Set(i, PolyWord::FromUnsigned(0));
    used += rounded + 1; // Actual space we're allocating
    ASSERT(((uintptr_t)newObj & (POLYML32IN64 * sizeof(PolyWord) -1)) == 0);
    return newObj;
}
#endif

class PImport
{
public:
    PImport();
    ~PImport();
    bool DoImport(void);
    FILE *f;
    PolyObject *Root(void) { return objMap[nRoot]; }
private:
    bool ReadValue(PolyObject *p, POLYUNSIGNED i);
    bool GetValue(PolyWord *result);
    
    POLYUNSIGNED nObjects, nRoot;
    PolyObject **objMap;

    unsigned spaceIndex;

    SpaceAlloc mutSpace, immutSpace, codeSpace;
};

PImport::PImport():
    mutSpace(&spaceIndex, MTF_WRITEABLE, 1024*1024),
    immutSpace(&spaceIndex, 0, 1024*1024),
    codeSpace(&spaceIndex, MTF_EXECUTABLE, 1024 * 1024)
{
    f = NULL;
    objMap = 0;
    spaceIndex = 1;
}

PImport::~PImport()
{
    if (f)
        fclose(f);
    free(objMap);
}

bool PImport::GetValue(PolyWord *result)
{
    int ch = getc(f);
    if (ch == '@')
    {
        /* Address of an object. */
        POLYUNSIGNED obj;
        fscanf(f, "%" POLYUFMT, &obj);
        ASSERT(obj < nObjects);
        *result = objMap[obj];
    }
    else if ((ch >= '0' && ch <= '9') || ch == '-')
    {
        /* Tagged integer. */
        POLYSIGNED j;
        ungetc(ch, f);
        fscanf(f, "%" POLYSFMT, &j);
        /* The assertion may be false if we are porting to a machine
           with a shorter tagged representation. */
        ASSERT(j >= -MAXTAGGED-1 && j <= MAXTAGGED);
        *result = TAGGED(j);
    }
    else
    {
        fprintf(polyStderr, "Unexpected character in stream");
        return false;
    }
    return true;
}

/* Read a value and store it at the specified word. */
bool PImport::ReadValue(PolyObject *p, POLYUNSIGNED i)
{
    PolyWord result = TAGGED(0);
    if (GetValue(&result))
    {
        p->Set(i, result);
        return true;
    }
    else return false;
}

bool PImport::DoImport()
{
    int ch;
    POLYUNSIGNED objNo;

    ASSERT(gMem.pSpaces.size() == 0);
    ASSERT(gMem.eSpaces.size() == 0);

    ch = getc(f);
    ASSERT(ch == 'O'); /* Number of objects. */
    while (getc(f) != '\t') ;
    fscanf(f, "%" POLYUFMT, &nObjects);
    /* Create a mapping table. */
    objMap = (PolyObject**)calloc(nObjects, sizeof(PolyObject*));
    if (objMap == 0)
    {
        fprintf(polyStderr, "Unable to allocate memory\n");
        return false;
    }

    do
    {
        ch = getc(f);
    } while (ch == '\n');
    ASSERT(ch == 'R'); /* Root object number. */
    while (getc(f) != '\t') ;
    fscanf(f, "%" POLYUFMT, &nRoot);
    do { ch = getc(f); } while (ch == ' ' || ch == '\t');
    // Older versions did not have the architecture and word length.
    if (ch != '\r' && ch != '\n')
    {
        unsigned wordLength;
        while (ch == ' ' || ch == '\t') ch = getc(f);
        char arch = ch;
        ch = getc(f);
        fscanf(f, "%u", &wordLength);
        // If we're booting a native code version from interpreted
        // code we have to interpret.
        machineDependent->SetBootArchitecture(arch, wordLength);
    }

    /* Now the objects themselves. */
    while (1)
    {
        unsigned    objBits = 0;
        POLYUNSIGNED  nWords, nBytes;
        do
        {
            ch = getc(f);
        } while (ch == '\r' || ch == '\n');
        if (ch == EOF) break;
        ungetc(ch, f);
        fscanf(f, "%" POLYUFMT, &objNo);
        ch = getc(f);
        ASSERT(ch == ':');
        ASSERT(objNo < nObjects);

        /* Modifiers, MNVW. */
        do
        {
            ch = getc(f);
            if (ch == 'M') objBits |= F_MUTABLE_BIT;
            else if (ch == 'N') objBits |= F_NEGATIVE_BIT;
            if (ch == 'V') objBits |= F_NO_OVERWRITE;
            if (ch == 'W') objBits |= F_WEAK_BIT;
        } while (ch == 'M' || ch == 'N' || ch == 'V' || ch == 'W');

        /* Object type. */
        switch (ch)
        {
        case 'O': /* Simple object. */
            fscanf(f, "%" POLYUFMT, &nWords);
            break;

        case 'B': /* Byte segment. */
            objBits |= F_BYTE_OBJ;
            fscanf(f, "%" POLYUFMT, &nBytes);
            /* Round up to appropriate number of words. */
            nWords = (nBytes + sizeof(PolyWord) -1) / sizeof(PolyWord);
            break;

        case 'S': /* String. */
            objBits |= F_BYTE_OBJ;
            /* The length is the number of characters. */
            fscanf(f, "%" POLYUFMT, &nBytes);
            /* Round up to appropriate number of words.  Need to add
               one PolyWord for the length PolyWord.  */
            nWords = (nBytes + sizeof(PolyWord) -1) / sizeof(PolyWord) + 1;
            break;

        case 'F':
            objBits |= F_CODE_OBJ;
            /* Read the number of bytes of code and the number of words
               for constants. */
            fscanf(f, "%" POLYUFMT ",%" POLYUFMT, &nWords, &nBytes);
            nWords += 2; // Add two words for no of consts + offset.
            /* Add in the size of the code itself. */
            nWords += (nBytes + sizeof(PolyWord) -1) / sizeof(PolyWord);
            break;

        case 'C': // Closure
            objBits |= F_CLOSURE_OBJ;
            fscanf(f, "%" POLYUFMT, &nWords); // This is the number of items.
            nWords += sizeof(PolyObject*) / sizeof(PolyWord) - 1;
            break;

        case 'L': // Legacy closure
            objBits |= F_CLOSURE_OBJ;
            fscanf(f, "%" POLYUFMT, &nWords); // This was the number of words.
            break;

        case 'K': // Single weak reference
            nWords = sizeof(uintptr_t)/sizeof(PolyWord);
            objBits |= F_BYTE_OBJ;
            break;

        case 'E': // Entry point - address followed by string
            objBits |= F_BYTE_OBJ;
            // The length is the length of the string but it must be null-terminated
            fscanf(f, "%" POLYUFMT, &nBytes);
            // Add one uintptr_t plus one plus padding to an integral number of words.
            nWords = (nBytes + sizeof(uintptr_t) + sizeof(PolyWord)) / sizeof(PolyWord);
            break;

        default:
            fprintf(polyStderr, "Invalid object type\n");
            return false;
        }

        SpaceAlloc* alloc;
        if (objBits & F_MUTABLE_BIT)
            alloc = &mutSpace;
        else if ((objBits & 3) == F_CODE_OBJ)
            alloc = &codeSpace;
        else alloc = &immutSpace;
        PolyObject* p = alloc->NewObj(nWords);
        if (p == 0)
            return false;
        objMap[objNo] = p;
        /* Put in length PolyWord and flag bits. */
        alloc->memSpace->writeAble(p)->SetLengthWord(nWords, objBits);

        /* Skip the object contents. */
        while (getc(f) != '\n') ;
    }

    /* Second pass - fill in the contents. */
    fseek(f, 0, SEEK_SET);
    /* Skip the information at the start. */
    ch = getc(f);
    ASSERT(ch == 'O'); /* Number of objects. */
    while (getc(f) != '\n');
    ch = getc(f);
    ASSERT(ch == 'R'); /* Root object number. */
    while (getc(f) != '\n') ;

    while (1)
    {
        if (feof(f))
            break;
        fscanf(f, "%" POLYUFMT, &objNo);
        if (feof(f))
            break;
        ch = getc(f);
        ASSERT(ch == ':');
        ASSERT(objNo < nObjects);
        PolyObject * p = objMap[objNo];

        /* Modifiers, M or N. */
        do
        {
            ch = getc(f);
        } while (ch == 'M' || ch == 'N' || ch == 'V' || ch == 'W');

        /* Object type. */
        switch (ch)
        {
        case 'O': /* Simple object. */
        case 'C': // Closure
        case 'L': // Legacy closure
        {
            POLYUNSIGNED nWords;
            bool isClosure = ch == 'C' || ch == 'L';
            fscanf(f, "%" POLYUFMT, &nWords);
            if (ch == 'C') nWords += sizeof(PolyObject*) / sizeof(PolyWord) - 1;
            ch = getc(f);
            ASSERT(ch == '|');
            ASSERT(nWords == p->Length());

            POLYUNSIGNED i = 0;
            if (isClosure)
            {
                int ch = getc(f);
                // This should be an address
                if (ch != '@') return false; 
                POLYUNSIGNED obj;
                fscanf(f, "%" POLYUFMT, &obj);
                ASSERT(obj < nObjects);
                *(PolyObject**)p = objMap[obj];
                ch = getc(f);
                i = sizeof(PolyObject*) / sizeof(PolyWord);
            }

            while (i < nWords)
            {
                if (!ReadValue(p, i))
                    return false;
                ch = getc(f);
                ASSERT((ch == ',' && i < nWords - 1) ||
                    (ch == '\n' && i == nWords - 1));
                i++;
            }

            break;
        }

        case 'B': /* Byte segment. */
            {
                byte *u = (byte*)p;
                POLYUNSIGNED nBytes;
                fscanf(f, "%" POLYUFMT, &nBytes);
                ch = getc(f); ASSERT(ch == '|');
                for (POLYUNSIGNED i = 0; i < nBytes; i++)
                {
                    int n;
                    fscanf(f, "%02x", &n);
                    u[i] = n;
                }
                ch = getc(f);
                ASSERT(ch == '\n');
                // Legacy: If this is an entry point object set its value.
                if (p->IsMutable() && p->IsWeakRefObject() && p->Length() > sizeof(uintptr_t)/sizeof(PolyWord))
                {
                    bool loadEntryPt = setEntryPoint(p);
                    ASSERT(loadEntryPt);
                }
                break;
            }

        case 'S': /* String. */
            {
                PolyStringObject * ps = (PolyStringObject *)p;
                /* The length is the number of characters. */
                POLYUNSIGNED nBytes;
                fscanf(f, "%" POLYUFMT, &nBytes);
                ch = getc(f); ASSERT(ch == '|');
                ps->length = nBytes;
                for (POLYUNSIGNED i = 0; i < nBytes; i++)
                {
                    int n;
                    fscanf(f, "%02x", &n);
                    ps->chars[i] = n;
                }
                ch = getc(f);
                ASSERT(ch == '\n');
                break;
            }

//      case 'D':
        case 'F':
            {
                bool newForm = ch == 'F';
                POLYUNSIGNED length = p->Length();
                POLYUNSIGNED nWords, nBytes;
                MemSpace* space = gMem.SpaceForObjectAddress(p);
                PolyObject *wr = space->writeAble(p);
                byte* u = (byte*)wr;
                /* Read the number of bytes of code and the number of words
                   for constants. */
                fscanf(f, "%" POLYUFMT ",%" POLYUFMT, &nWords, &nBytes);
                /* Read the code. */
                ch = getc(f); ASSERT(ch == '|');
                for (POLYUNSIGNED i = 0; i < nBytes; i++)
                {
                    int n;
                    fscanf(f, "%02x", &n);
                    u[i] = n;
                }
                ch = getc(f);
                ASSERT(ch == '|');
                if (newForm)
                {
                    wr->Set(length - nWords - 2, PolyWord::FromUnsigned(nWords));
                    machineDependent->SetAddressOfConstants(p, wr, length, p->Offset(length - nWords - 1));
                }
                else wr->Set(length-1, PolyWord::FromUnsigned(nWords));
                /* Read in the constants. */
                for (POLYUNSIGNED i = 0; i < nWords; i++)
                {
                    if (! ReadValue(wr, i+length-nWords-1))
                        return false;
                    ch = getc(f);
                    ASSERT((ch == ',' && i < nWords-1) ||
                           ((ch == '\n' || ch == '|') && i == nWords-1));
                }
                // Read in any constants in the code.
                if (ch == '|')
                {
                    ch = getc(f);
                    while (ch != '\n')
                    {
                        ungetc(ch, f);
                        POLYUNSIGNED offset;
                        int code;
                        fscanf(f, "%" POLYUFMT ",%d", &offset, &code);
                        ch = getc(f);
                        ASSERT(ch == ',');
                        // This should be an address.
                        ch = getc(f);
                        if (ch == '@')
                        {
                            POLYUNSIGNED obj;
                            fscanf(f, "%" POLYUFMT, &obj);
                            ASSERT(obj < nObjects);
                            PolyObject *addr = objMap[obj];
                            byte *toPatch = (byte*)p + offset; // Pass the execute address here.
                            ScanAddress::SetConstantValue(toPatch, addr, (ScanRelocationKind)code);
                        }
                        else
                        {
                            // Previously we also included tagged constants but they are
                            // already in the code.
                            ungetc(ch, f);
                            PolyWord w;
                            if (!GetValue(&w))
                                return false;
                        }
                        do ch = getc(f); while (ch == ' ');
                    }
                }
                // Clear the mutable bit
                wr->SetLengthWord(p->Length(), F_CODE_OBJ);
                break;
            }

        case 'K':
            // Weak reference - must be zeroed
            *(uintptr_t*)p = 0;
            break;

        case 'E':
            // Entry point - address followed by string
            {
                // The length is the number of characters.
                *(uintptr_t*)p = 0;
                char* b = (char*)p + sizeof(uintptr_t);
                POLYUNSIGNED nBytes;
                fscanf(f, "%" POLYUFMT, &nBytes);
                ch = getc(f); ASSERT(ch == '|');
                for (POLYUNSIGNED i = 0; i < nBytes; i++)
                {
                    ch = getc(f);
                    *b++ = ch;
                }
                *b = 0;
                ch = getc(f);
                ASSERT(ch == '\n');
                bool loadEntryPt = setEntryPoint(p);
                ASSERT(loadEntryPt);
                break;
            }

        default:
            fprintf(polyStderr, "Invalid object type\n");
            return false;
        }
    }
    // There's no need to remove write permissions at this stage.
    return true;
}

// Import a file in the portable format and return a pointer to the root object.
PolyObject *ImportPortable(const TCHAR *fileName)
{
    PImport pImport;
#if (defined(_WIN32) && defined(UNICODE))
    pImport.f = _wfopen(fileName, L"r");
    if (pImport.f == 0)
    {
        fprintf(polyStderr, "Unable to open file: %S\n", fileName);
        return 0;
    }
#else
    pImport.f = fopen(fileName, "r");
    if (pImport.f == 0)
    {
        fprintf(polyStderr, "Unable to open file: %s\n", fileName);
        return 0;
    }
#endif
    if (pImport.DoImport())
        return pImport.Root();
    else
        return 0;
}
