/*
    Title:      Object size

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further development David C.J. Matthews 2016

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

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x)
#endif


#include "globals.h"
#include "arb.h"
#include "run_time.h"
#include "machine_dep.h"
#include "objsize.h"
#include "scanaddrs.h"
#include "polystring.h"
#include "save_vec.h"
#include "bitmap.h"
#include "memmgr.h"
#include "mpoly.h"
#include "processes.h"
#include "rtsentry.h"

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyObjSize(PolyObject *threadId, PolyWord obj);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyShowSize(PolyObject *threadId, PolyWord obj);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyObjProfile(PolyObject *threadId, PolyWord obj);
}

extern FILE *polyStdout;

#define MAX_PROF_LEN 100 // Profile lengths between 1 and this

class ProcessVisitAddresses: public ScanAddress
{
public:
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt) { return ShowWord(*pt); }
    virtual PolyObject *ScanObjectAddress(PolyObject *base);

    POLYUNSIGNED ShowWord(PolyWord w);
    ProcessVisitAddresses(bool show);
    ~ProcessVisitAddresses();

    VisitBitmap *FindBitmap(PolyWord p);
    void ShowBytes(PolyObject *start);
    void ShowCode(PolyObject *start);
    void ShowWords(PolyObject *start);

    POLYUNSIGNED total_length;
    bool     show_size;
    void     *io_bottom;
    void     *io_top;
    VisitBitmap  **bitmaps;
    unsigned   nBitmaps;
    // Counts of objects of each size for mutable and immutable data.
    unsigned   iprofile[MAX_PROF_LEN+1];
    unsigned   mprofile[MAX_PROF_LEN+1];
};

ProcessVisitAddresses::ProcessVisitAddresses(bool show)
{
    // Need to get the allocation lock here.  Another thread
    // could allocate new local areas resulting in gMem.nlSpaces
    // and gMem.lSpaces changing under our feet.
    PLocker lock(&gMem.allocLock);

    MemSpace *ioSpace = gMem.IoSpace();
    io_bottom    = ioSpace->bottom;
    io_top       = ioSpace->top;
    total_length = 0;
    show_size    = show;

    // Create a bitmap for each of the areas apart from the IO area
    nBitmaps = gMem.nlSpaces+gMem.npSpaces+gMem.ncSpaces; //
    bitmaps = new VisitBitmap*[nBitmaps];
    unsigned bm = 0;
    unsigned j;
    for (j = 0; j < gMem.npSpaces; j++)
    {
        MemSpace *space = gMem.pSpaces[j];
        // Permanent areas are filled with objects from the bottom.
        bitmaps[bm++] = new VisitBitmap(space->bottom, space->top);
    }
    for (j = 0; j < gMem.nlSpaces; j++)
    {
        LocalMemSpace *space = gMem.lSpaces[j];
        bitmaps[bm++] = new VisitBitmap(space->bottom, space->top);
    }
    for (j = 0; j < gMem.ncSpaces; j++)
    {
        CodeSpace *space = gMem.cSpaces[j];
        bitmaps[bm++] = new VisitBitmap(space->bottom, space->top);
    }
    ASSERT(bm == nBitmaps);

    // Clear the profile counts.
    for (unsigned i = 0; i < MAX_PROF_LEN+1; i++)
    {
        iprofile[i] = mprofile[i] = 0;
    }
}


ProcessVisitAddresses::~ProcessVisitAddresses()
{
    if (bitmaps)
    {
        for (unsigned i = 0; i < nBitmaps; i++)
            delete(bitmaps[i]);
        delete[](bitmaps);
    }
}

// Return the bitmap corresponding to the address or NULL if it isn't there.
VisitBitmap *ProcessVisitAddresses::FindBitmap(PolyWord p)
{
    for (unsigned i = 0; i < nBitmaps; i++)
    {
        VisitBitmap *bm = bitmaps[i];
        if (bm->InRange(p.AsStackAddr())) return bm;
    }
    return 0;
}

void ProcessVisitAddresses::ShowBytes(PolyObject *start)
{
    POLYUNSIGNED bytes = start->Length() * sizeof(PolyWord);
    char *array = (char *) start;

    putc('\n', polyStdout);

    if (start->IsMutable()) fprintf(polyStdout, "MUTABLE ");

    fprintf(polyStdout, "BYTES:%p:%" POLYUFMT "\n", array, bytes);

    POLYUNSIGNED i, n;
    for (i = 0, n = 0; n < bytes; n++)
    {
        fprintf(polyStdout, "%02x ",array[n] & 0xff);
        i++;
        if (i == 16)
        { 
            putc('\n', polyStdout);
            i = 0;
        }
    }

    if (i != 0) putc('\n', polyStdout);
}

#define MAXNAME 500

void ProcessVisitAddresses::ShowCode(PolyObject *start)
{
    POLYUNSIGNED length = start->Length();

    putc('\n', polyStdout);
    if (start->IsMutable()) fprintf(polyStdout, "MUTABLE ");

    char buffer[MAXNAME+1];
    PolyWord *consts = start->ConstPtrForCode();
    PolyWord string = consts[0];
            
    if (string == TAGGED(0))
        strcpy(buffer, "<not-named>");
    else
        (void) Poly_string_to_C(string, buffer, sizeof(buffer));

    fprintf(polyStdout, "CODE:%p:%" POLYUFMT " %s\n", start, length, buffer);

    POLYUNSIGNED i, n;
    for (i = 0, n = 0; n < length; n++)
    {
        if (i != 0) putc('\t', polyStdout);

        fprintf(polyStdout, "%8p ", start->Get(n).AsObjPtr());
        i++;
        if (i == 4)
        { 
            putc('\n', polyStdout);
            i = 0;
        }
    }

    if (i != 0) putc('\n', polyStdout);
}

void ProcessVisitAddresses::ShowWords(PolyObject *start)
{
    POLYUNSIGNED length = start->Length();
    
    putc('\n', polyStdout);
    if (start->IsMutable()) fprintf(polyStdout, "MUTABLE ");
    
    fprintf(polyStdout, "WORDS:%p:%" POLYUFMT "\n", start, length);
    
    POLYUNSIGNED i, n;
    for (i = 0, n = 0; n < length; n++)
    {
        if (i != 0)
            putc('\t', polyStdout);
        
        fprintf(polyStdout, "%8p ", start->Get(n).AsObjPtr());
        i++;
        if (i == 4)
        { 
            putc('\n', polyStdout);
            i = 0;
        }
    }
    
    if (i != 0)
        putc('\n', polyStdout);
}

// This is called initially to print the top-level object.
// Since we don't process stacks it probably doesn't get called elsewhere.
PolyObject *ProcessVisitAddresses::ScanObjectAddress(PolyObject *base)
{
    POLYUNSIGNED lengthWord = ShowWord(base);
    if (lengthWord)
        ScanAddressesInObject(base, lengthWord);
    return base;
}

// Handle the normal case.  Print the object at this word and
// return true is it must be handled recursively.
POLYUNSIGNED ProcessVisitAddresses::ShowWord(PolyWord w)
{
    
    if (IS_INT(w))
        return 0; /* not a pointer */
    
    if (w.AsAddress() >= io_bottom && w.AsAddress() < io_top)
        return 0; /* IO segment */
     
    if (w == PolyWord::FromUnsigned(0))
        return 0;
    
    VisitBitmap *bm    = FindBitmap(w);
    
    if (bm == 0)
    {
        fprintf(polyStdout, "Bad address " ZERO_X "%p found\n", w.AsObjPtr());
        return 0;
    }
    
    PolyObject *p;

    if (OBJ_IS_CODEPTR(w))
        p = ObjCodePtrToPtr(w.AsCodePtr()); /* find beginning of the code object */
    else p = w.AsObjPtr();
    
    /* Have we already visited this object? */
    if (bm->AlreadyVisited(p))
        return 0;
    
    bm->SetVisited(p);
    
    POLYUNSIGNED L = p->LengthWord();
    POLYUNSIGNED obj_length = OBJ_OBJECT_LENGTH(L);

    // Increment the appropriate size profile count.
    if (p->IsMutable())
    {
        if (obj_length > MAX_PROF_LEN)
            mprofile[MAX_PROF_LEN]++;
        else
            mprofile[obj_length]++;
    }
    else
    {
        if (obj_length > MAX_PROF_LEN)
            iprofile[MAX_PROF_LEN]++;
        else
            iprofile[obj_length]++;
    }
    
    total_length += obj_length + 1; /* total space needed for object */
    
    if (OBJ_IS_BYTE_OBJECT(L))
    {
        if (show_size)
            ShowBytes(p);
        return 0;
    }
    else if (OBJ_IS_CODE_OBJECT(L))
    {
        PolyWord *cp;
        POLYUNSIGNED const_count;
        p->GetConstSegmentForCode(cp, const_count);
        
        if (show_size)
            ShowCode(p);

        return L; // Process addresses in it.
     }
    else /* Word object */
    {
        if (show_size)
            ShowWords(p);
        return L; // Process addresses in it.
    }
}


Handle ObjSize(TaskData *taskData, Handle obj)
{
    ProcessVisitAddresses process(false);
    process.ScanObjectAddress(obj->WordP());
    return Make_arbitrary_precision(taskData, process.total_length);
}

Handle ShowSize(TaskData *taskData, Handle obj)
{
    ProcessVisitAddresses process(true);
    process.ScanObjectAddress(obj->WordP());
    fflush(polyStdout); /* We need this for Windows at least. */
    return Make_arbitrary_precision(taskData, process.total_length);
}

static void printfprof(unsigned *counts)
{
    for(unsigned i = 0; i < MAX_PROF_LEN+1; i++)
    {
        if (counts[i] != 0)
        {
            if (i == MAX_PROF_LEN)
                fprintf(polyStdout, ">%d\t%u\n", MAX_PROF_LEN, counts[i]);
            else
                fprintf(polyStdout, "%d\t%u\n", i, counts[i]);
        }
    }
}

Handle ObjProfile(TaskData *taskData, Handle obj)
{
    ProcessVisitAddresses process(false);
    process.ScanObjectAddress(obj->WordP());
    fprintf(polyStdout, "\nImmutable object sizes and counts\n");
    printfprof(process.iprofile);
    fprintf(polyStdout, "\nMutable object sizes and counts\n");
    printfprof(process.mprofile);
    fflush(polyStdout); /* We need this for Windows at least. */
    return Make_arbitrary_precision(taskData, process.total_length);
}

POLYUNSIGNED PolyObjSize(PolyObject *threadId, PolyWord obj)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();

    ProcessVisitAddresses process(false);
    process.ScanObjectAddress(obj.AsObjPtr());
    Handle result = Make_arbitrary_precision(taskData, process.total_length);

    taskData->PostRTSCall();
    return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyShowSize(PolyObject *threadId, PolyWord obj)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();

    ProcessVisitAddresses process(true);
    process.ScanObjectAddress(obj.AsObjPtr());
    fflush(polyStdout); /* We need this for Windows at least. */
    Handle result = Make_arbitrary_precision(taskData, process.total_length);

    taskData->PostRTSCall();
    return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyObjProfile(PolyObject *threadId, PolyWord obj)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();

    ProcessVisitAddresses process(false);
    process.ScanObjectAddress(obj.AsObjPtr());
    fprintf(polyStdout, "\nImmutable object sizes and counts\n");
    printfprof(process.iprofile);
    fprintf(polyStdout, "\nMutable object sizes and counts\n");
    printfprof(process.mprofile);
    fflush(polyStdout); /* We need this for Windows at least. */
    Handle result = Make_arbitrary_precision(taskData, process.total_length);

    taskData->PostRTSCall();
    return result->Word().AsUnsigned();
}

struct _entrypts objSizeEPT[] =
{
    { "PolyObjSize",                    (polyRTSFunction)&PolyObjSize},
    { "PolyShowSize",                   (polyRTSFunction)&PolyShowSize},
    { "PolyObjProfile",                 (polyRTSFunction)&PolyObjProfile},

    { NULL, NULL} // End of list.
};
