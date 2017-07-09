/*
    Title:      Multi-Threaded Garbage Collector - Data sharing phase

    Copyright (c) 2012 David C. J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/

/*  GC Sharing pass.
    This pass is invoked only if the heap sizing code detects that heap space
    is running very short because it adds a very considerable overhead to GC.
    It aims to reduce the size of the live data in a similar way to the data
    sharing function PolyML.shareCommonData by merging immutable cells that
    contain data that cannot be distinguished.

    This version of the code now does a deep structure merge in a similar
    way to the full sharing function. This code first does a full pass
    over the heap creating lists of cells that could possibly be merged.
    There are separate lists for byte and word objects up to a fixed
    size.  Larger objects and other objects are not considered. Because
    all the items in a list have the same length and type (flag bits)
    we can use the length word to link the items in the list.  A
    consequence of this is that positive long precision values can be
    shared but negative values cannot.  

    There is a sharing function that first distributes items into a
    hash table.  Then each hash table is sorted and as part of the
    sorting process cells with the same contents are merged.  One
    cell is chosen and the length words on the others are set to be
    forwarding pointers to the chosen cell.  Hashing allows for easy
    parallel processing.

    The structure sharing code works by first sharing the byte
    data which cannot contain pointers.  Then the word data is processed
    to separate out "tail" cells that contain only tagged integers or
    pointers to cells that either cannot be merged, such as mutables,
    or those that have already been processed, such as the byte data.
    Any pointers to shared data are updated to point to the merged cell.
    The tail cells are then sorted and shared using the sharing function
    and become part of the "processed" set.  This process is repeated to
    find cells that are now tails and so on.

    Compared with the full sharing code this is expensive since it
    requires repeated scans of the list of unprocessed cells.  In
    particular there may be cells that form loops (basically closures
    for mutually recusive functions) and if they are present they and
    anything that points directly or indirectly at them will never
    be removed from the list.  We stop when it appears that we are
    not making progress and simply do a final bit-wise share of the
    remainder.
*/
#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "globals.h"
#include "processes.h"
#include "gc.h"
#include "scanaddrs.h"
#include "bitmap.h"
#include "memmgr.h"
#include "diagnostics.h"
#include "gctaskfarm.h"
#include "heapsizing.h"

class ObjEntry
{
public:
    ObjEntry(): objList(0), objCount(0), shareCount(0) {}
    PolyObject *objList;
    POLYUNSIGNED objCount;
    POLYUNSIGNED shareCount;
};

// There is an instance of this class for each combination of size and
// word/byte.
class SortVector
{
public:
    SortVector(): totalCount(0), carryOver(0) {}

    void AddToVector(PolyObject *obj, POLYUNSIGNED length);

    void SortData(void);
    POLYUNSIGNED TotalCount() const { return totalCount; }
    POLYUNSIGNED CurrentCount() const { return baseObject.objCount; }
    POLYUNSIGNED Shared() const;
    void SetLengthWord(POLYUNSIGNED l) { lengthWord = l; }
    POLYUNSIGNED CarryOver() const { return carryOver; }

    static void hashAndSortAllTask(GCTaskId*, void *a, void *b);
    static void sharingTask(GCTaskId*, void *a, void *b);
    static void wordDataTask(GCTaskId*, void *a, void *b);

private:
    void sortList(PolyObject *head, POLYUNSIGNED nItems, POLYUNSIGNED &count);

    ObjEntry baseObject, processObjects[256];
    POLYUNSIGNED totalCount;
    POLYUNSIGNED lengthWord;
    POLYUNSIGNED carryOver;
};

POLYUNSIGNED SortVector::Shared() const
{
    // Add all the sharing counts
    POLYUNSIGNED shareCount = baseObject.shareCount;
    for (unsigned i = 0; i < 256; i++)
        shareCount += processObjects[i].shareCount;
    return shareCount;
}

void SortVector::AddToVector(PolyObject *obj, POLYUNSIGNED length)
{
    obj->SetShareChain(baseObject.objList);
    baseObject.objList = obj;
    baseObject.objCount++;
    totalCount++;
}

// The number of byte and word entries.
// Objects of up to and including this size are shared.
// Byte objects include strings so it is more likely that
// larger objects will share.  Word objects that share
// are much more likely to be 2 or 3 words.
#define NUM_BYTE_VECTORS    22
#define NUM_WORD_VECTORS    10

class GetSharing: public RecursiveScanWithStack
{
public:
    GetSharing();
    void SortData(void);
    static void shareByteData(GCTaskId *, void *, void *);
    static void shareWordData(GCTaskId *, void *, void *);
    static void shareRemainingWordData(GCTaskId *, void *, void *);

protected:
    virtual bool TestForScan(PolyWord *);
    virtual void MarkAsScanning(PolyObject *);
    virtual void StackOverflow(void) {} // Ignore stack overflow
    virtual void Completed(PolyObject *);

private:
    // The head of chains of cells of the same size
    SortVector byteVectors[NUM_BYTE_VECTORS];
    SortVector wordVectors[NUM_WORD_VECTORS];

    POLYUNSIGNED largeWordCount, largeByteCount, excludedCount;
};

GetSharing::GetSharing()
{
    for (unsigned i = 0; i < NUM_BYTE_VECTORS; i++)
        byteVectors[i].SetLengthWord((POLYUNSIGNED)(i+1) | _OBJ_BYTE_OBJ);

    for (unsigned j = 0; j < NUM_WORD_VECTORS; j++)
        wordVectors[j].SetLengthWord(j+1);

    largeWordCount = largeByteCount = excludedCount = 0;
}

bool GetSharing::TestForScan(PolyWord *pt)
{
    PolyWord p = *pt;
    ASSERT(p.IsDataPtr());
    PolyObject *obj = p.AsObjPtr();

    while (obj->ContainsForwardingPtr())
    {
        obj = obj->GetForwardingPtr();
        *pt = obj;
    }
    ASSERT(obj == (*pt).AsObjPtr());

    LocalMemSpace *space = gMem.LocalSpaceForAddress(obj-1);

    if (space == 0)
        return false; // Ignore it if it points to a permanent area

    if (space->bitmap.TestBit(space->wordNo((PolyWord*)obj)))
        return false;

    return true;
}

void GetSharing::MarkAsScanning(PolyObject *obj)
{
    ASSERT(obj->ContainsNormalLengthWord());
    LocalMemSpace *space = gMem.LocalSpaceForAddress(obj-1);
    space->bitmap.SetBit(space->wordNo((PolyWord*)obj));
}

void GetSharing::Completed(PolyObject *obj)
{
    POLYUNSIGNED L = obj->LengthWord();
    // We have tables for word objects and byte objects
    // We chain entries together using the length word so it
    // is important that we only do this for objects that
    // have no other bits in the header, such as the sign bit.
    if ((L & _OBJ_PRIVATE_FLAGS_MASK) == 0)
    {
        POLYUNSIGNED length = obj->Length();
        ASSERT(length != 0);
        if (length <= NUM_WORD_VECTORS)
            wordVectors[length-1].AddToVector(obj, length);
        else largeWordCount++;
    }
    else if ((L & _OBJ_PRIVATE_FLAGS_MASK) == _OBJ_BYTE_OBJ)
    {
        POLYUNSIGNED length = obj->Length();
        ASSERT(length != 0);
        if (length <= NUM_BYTE_VECTORS)
            byteVectors[length-1].AddToVector(obj, length);
        else largeByteCount++;
    }
    else if (! OBJ_IS_CODE_OBJECT(L) && ! OBJ_IS_MUTABLE_OBJECT(L))
        excludedCount++; // Code and mutables can't be shared - see what could be
}

// Quicksort the list to detect cells with the same content.  These are made
// to share and removed from further sorting.
void SortVector::sortList(PolyObject *head, POLYUNSIGNED nItems, POLYUNSIGNED &shareCount)
{
    while (nItems > 2)
    {
        size_t bytesToCompare = OBJ_OBJECT_LENGTH(lengthWord)*sizeof(PolyWord);
        PolyObject *median = head;
        head = head->GetShareChain();
        median->SetLengthWord(lengthWord);
        PolyObject *left = 0, *right = 0;
        POLYUNSIGNED leftCount = 0, rightCount = 0;
        while (head != 0)
        {
            PolyObject *next = head->GetShareChain();
            int res = memcmp(median, head, bytesToCompare);
            if (res == 0)
            {
                // Equal - they can share
                head->SetForwardingPtr(median);
                shareCount++;
            }
            else if (res < 0)
            {
                head->SetShareChain(left);
                left = head;
                leftCount++;
            }
            else
            {
                head->SetShareChain(right);
                right = head;
                rightCount++;
            }
            head = next;
        }
        // We can now drop the median and anything that shares with it.
        // Process the smaller partition recursively and the larger by
        // tail recursion.
        if (leftCount < rightCount)
        {
            sortList(left, leftCount, shareCount);
            head = right;
            nItems = rightCount;
        }
        else
        {
            sortList(right, rightCount, shareCount);
            head = left;
            nItems = leftCount;
        }
    }
    if (nItems == 1)
        head->SetLengthWord(lengthWord);
    else if (nItems == 2)
    {
        PolyObject *next = head->GetShareChain();
        head->SetLengthWord(lengthWord);
        if (memcmp(head, next, OBJ_OBJECT_LENGTH(lengthWord)*sizeof(PolyWord)) == 0)
        {
            next->SetForwardingPtr(head);
            shareCount++;
        }
        else next->SetLengthWord(lengthWord);
    }
}

void SortVector::sharingTask(GCTaskId*, void *a, void *b)
{
    SortVector *s = (SortVector *)a;
    ObjEntry *o = (ObjEntry*)b;
    s->sortList(o->objList, o->objCount, o->shareCount);
}

// Process one level of the word data.
// N.B.  The length words are updated without any locking.  This is safe
// because all length words are initially chain entries and a chain entry
// can be replaced by another chain entry, a forwarding pointer or a normal
// length word.  Forwarding pointers and normal length words are only ever
// set once.  There is a small chance that we could lose some sharing as a
// result of a race condition if a thread defers an object because it
// contains a pointer with a chain entry and later sees an otherwise
// equal object where another thread has replaced the chain with a
// normal address, adds it to the list for immediate processing and
// so never compares the two.
void SortVector::wordDataTask(GCTaskId*, void *a, void *)
{
    SortVector *s = (SortVector*)a;
    // Partition the objects between those that have pointers to objects that are
    // still to be processed and those that have been processed.
    if (s->baseObject.objList == 0)
        return;
    PolyObject *h = s->baseObject.objList;
    s->baseObject.objList = 0;
    s->baseObject.objCount = 0;
    POLYUNSIGNED words = OBJ_OBJECT_LENGTH(s->lengthWord);
    s->carryOver = 0;

    for (unsigned i = 0; i < 256; i++)
    {
        // Clear the entries in the hash table but not the sharing count.
        s->processObjects[i].objList = 0;
        s->processObjects[i].objCount = 0;
    }

    while (h != 0)
    {
        PolyObject *next = h->GetShareChain();
        bool deferred = false;
        for (POLYUNSIGNED i = 0; i < words; i++)
        {
            PolyWord w = h->Get(i);
            if (w.IsDataPtr())
            {
                PolyObject *p = w.AsObjPtr();
                // Update the addresses of objects that have been merged
                if (p->ContainsForwardingPtr())
                {
                    h->Set(i, p->GetForwardingPtr());
                    s->carryOver++;
                }
                else if (p->ContainsShareChain())
                {
                    // If it is still to be shared leave it
                    deferred = true;
                    break;
                }
            }
        }
        if (deferred)
        {
            // We can't do it yet: add it back to the list
            h->SetShareChain(s->baseObject.objList);
            s->baseObject.objList = h;
            s->baseObject.objCount++;
        }
        else
        {
            // Add it to the hash table.
            unsigned char hash = 0;
            for (POLYUNSIGNED i = 0; i < words*sizeof(PolyWord); i++)
                hash += h->AsBytePtr()[i];
            h->SetShareChain(s->processObjects[hash].objList);
            s->processObjects[hash].objList = h;
            s->processObjects[hash].objCount++;
        }
        h = next;
    }
    s->SortData();
}

// Sort the entries in the hash table.
void SortVector::SortData()
{
    for (unsigned j = 0; j < 256; j++)
    {
        ObjEntry *oentry = &processObjects[j];
        // Sort this entry.  If it's very small just process it now.
        switch (oentry->objCount)
        {
        case 0: break; // Nothing there

        case 1: // Singleton - just restore the length word
            oentry->objList->SetLengthWord(lengthWord);
            break;

        case 2:
            {
                // Two items - process now
                PolyObject *obj1 = oentry->objList;
                PolyObject *obj2 = obj1->GetShareChain();
                obj1->SetLengthWord(lengthWord);
                if (memcmp(obj1, obj2, OBJ_OBJECT_LENGTH(lengthWord)*sizeof(PolyWord)) == 0)
                {
                    obj2->SetForwardingPtr(obj1);
                    oentry->shareCount++;
                }
                else obj2->SetLengthWord(lengthWord);
                break;
            }

        default:
            gpTaskFarm->AddWorkOrRunNow(sharingTask, this, oentry);
        }
    }
}

void SortVector::hashAndSortAllTask(GCTaskId*, void *a, void *b)
{
    SortVector *s = (SortVector *)a;
    // Hash the contents of the base object then sort them.
    for (unsigned i = 0; i < 256; i++)
    {
        // Clear the entries in the hash table but not the sharing count.
        s->processObjects[i].objList = 0;
        s->processObjects[i].objCount = 0;
    }
    PolyObject *h = s->baseObject.objList;
    POLYUNSIGNED bytes = OBJ_OBJECT_LENGTH(s->lengthWord)*sizeof(PolyWord);
    while (h != 0)
    {
        PolyObject *next = h->GetShareChain();
        unsigned char hash = 0;
        for (POLYUNSIGNED j = 0; j < bytes; j++)
            hash += h->AsBytePtr()[j];
        h->SetShareChain(s->processObjects[hash].objList);
        s->processObjects[hash].objList = h;
        s->processObjects[hash].objCount++;
        h = next;
    }
    s->SortData();
}

// Look for sharing between byte data.  These cannot contain pointers
// so they can all be processed together.
void GetSharing::shareByteData(GCTaskId *, void *a, void *)
{
    GetSharing *s = (GetSharing*)a;
    for (unsigned i = 0; i < NUM_BYTE_VECTORS; i++)
    {
        if (s->byteVectors[i].CurrentCount() != 0)
            gpTaskFarm->AddWorkOrRunNow(SortVector::hashAndSortAllTask, &(s->byteVectors[i]), 0);
    }
}

// Process word data at this particular level
void GetSharing::shareWordData(GCTaskId *, void *a, void *)
{
    GetSharing *s = (GetSharing*)a;
    for (unsigned i = 0; i < NUM_WORD_VECTORS; i++)
    {
        if (s->wordVectors[i].CurrentCount() != 0)
            gpTaskFarm->AddWorkOrRunNow(SortVector::wordDataTask, &(s->wordVectors[i]), 0);
    }
}

// Share any entries left.
void GetSharing::shareRemainingWordData(GCTaskId *, void *a, void *)
{
    GetSharing *s = (GetSharing*)a;
    for (unsigned i = 0; i < NUM_WORD_VECTORS; i++)
    {
        if (s->wordVectors[i].CurrentCount() != 0)
            gpTaskFarm->AddWorkOrRunNow(SortVector::hashAndSortAllTask, &(s->wordVectors[i]), 0);
    }
}

void GetSharing::SortData()
{
    // First process the byte objects.  They cannot contain pointers.
    // We create a task to do this so that we never have more threads
    // running than given with --gcthreads.
    gpTaskFarm->AddWorkOrRunNow(shareByteData, this, 0);
    gpTaskFarm->WaitForCompletion();

    // Word data may contain pointers to other objects.  If an object
    // has been processed its header will contain either a normal length
    // word or a forwarding pointer if it shares.  We can process an
    // object if every word in it is either a tagged integer or an
    // address we have already processed.  This works provided there
    // are no loops so when we reach a stage where we are unable to
    // process anything we simply run a final scan on the remainder.
    // Loops can arise from the closures of mutually recursive functions.

    // Now process the word entries until we have nothing left apart from loops.
    POLYUNSIGNED lastCount = 0, lastShared = 0;
    for (unsigned n = 0; n < NUM_WORD_VECTORS; n++)
        lastCount += wordVectors[n].CurrentCount();

    for(unsigned pass = 1; lastCount != 0; pass++)
    {
        gpTaskFarm->AddWorkOrRunNow(shareWordData, this, 0);
        gpTaskFarm->WaitForCompletion();

        // At each stage check that we have removed some items
        // from the lists.
        POLYUNSIGNED postCount = 0, postShared = 0, carryOver = 0;
        for (unsigned i = 0; i < NUM_WORD_VECTORS; i++)
        {
            postCount += wordVectors[i].CurrentCount();
            postShared += wordVectors[i].Shared();
            carryOver += wordVectors[i].CarryOver();
        }

        if (debugOptions & DEBUG_GC)
            Log("GC: Share: Pass %u: %" POLYUFMT " removed (%1.1f%%) %" POLYUFMT " shared (%1.1f%%) %" POLYUFMT " remain. %" POLYUFMT " entries updated (%1.1f%%).\n",
                pass, lastCount-postCount, (double)(lastCount-postCount) / (double) lastCount * 100.0,
                postShared - lastShared, (double)(postShared - lastShared) / (double) (lastCount-postCount) * 100.0,
                postCount, carryOver, (double)carryOver / (double)(lastCount-postCount) * 100.0);

        // Condition for exiting the loop.  There are some heuristics here.
        // If we remove less than 10% in a pass it's probably not worth continuing
        // unless the carry over is large.  The "carry over" is the number of words updated as
        // a result of the last pass.  It represents the extra sharing we gained in this pass
        // as a result of the last pass.  If there are deep data structures that can be shared
        // we get better sharing with more passes.  If the data structures are shallow we will
        // get as much sharing by just running the final pass.  The first pass only carries
        // over any sharing from the byte objects so we need to run at least one more before
        // checking the carry over.
        if (pass > 1 && (lastCount - postCount) * 10 < lastCount && carryOver*2 < (lastCount-postCount))
            break;

        lastCount = postCount;
        lastShared = postShared;
    }

    // Process any remaining entries.  There may be loops.
    gpTaskFarm->AddWorkOrRunNow(shareRemainingWordData, this, 0);
    gpTaskFarm->WaitForCompletion();

    if (debugOptions & DEBUG_GC)
    {
        POLYUNSIGNED postShared = 0;
        for (unsigned i = 0; i < NUM_WORD_VECTORS; i++)
            postShared += wordVectors[i].Shared();
        if (debugOptions & DEBUG_GC)
            Log("GC: Share: Final pass %" POLYUFMT " removed %" POLYUFMT " shared (%1.1f%%).\n",
                lastCount, postShared - lastShared,
                (double)(postShared - lastShared) / (double) lastCount * 100.0);
    }

    // Calculate the totals.
    POLYUNSIGNED totalSize = 0, totalShared = 0, totalRecovered = 0;
    for (unsigned k = 0; k < NUM_BYTE_VECTORS; k++)
    {
        totalSize += byteVectors[k].TotalCount();
        POLYUNSIGNED shared = byteVectors[k].Shared();
        totalShared += shared;
        totalRecovered += shared * (k+2); // Add 2 because the 0th item is one word + length word.
        if (debugOptions & DEBUG_GC)
            Log("GC: Share: Byte objects of size %u: %" POLYUFMT " objects %" POLYUFMT " shared\n",
                k+1, byteVectors[k].TotalCount(), byteVectors[k].Shared());
    }

    for (unsigned l = 0; l < NUM_WORD_VECTORS; l++)
    {
        totalSize += wordVectors[l].TotalCount();
        POLYUNSIGNED shared = wordVectors[l].Shared();
        totalShared += shared;
        totalRecovered += shared * (l+2);
        if (debugOptions & DEBUG_GC)
            Log("GC: Share: Word objects of size %u: %" POLYUFMT " objects %" POLYUFMT " shared\n",
                l+1, wordVectors[l].TotalCount(), wordVectors[l].Shared());
    }

    if (debugOptions & DEBUG_GC)
    {
        Log("GC: Share: Total %" POLYUFMT " objects, %" POLYUFMT " shared (%1.0f%%).  %" POLYUFMT " words recovered.\n",
            totalSize, totalShared, (double)totalShared / (double)totalSize * 100.0, totalRecovered);
        Log("GC: Share: Excluding %" POLYUFMT " large word objects %" POLYUFMT " large byte objects and %" POLYUFMT " others\n",
            largeWordCount, largeByteCount, excludedCount);
    }

    gHeapSizeParameters.RecordSharingData(totalRecovered);
}

void GCSharingPhase(void)
{
    mainThreadPhase = MTP_GCPHASESHARING;

    GetSharing sharer;

    for (unsigned i = 0; i < gMem.nlSpaces; i++)
    {
        LocalMemSpace *lSpace = gMem.lSpaces[i];
        lSpace->bitmap.ClearBits(0, lSpace->spaceSize());
    }

    // Process the permanent mutable areas and the code areas
    for (unsigned j = 0; j < gMem.npSpaces; j++)
    {
        PermanentMemSpace *space = gMem.pSpaces[j];
        if (space->isMutable && ! space->byteOnly)
            sharer.ScanAddressesInRegion(space->bottom, space->top);
    }
    for (unsigned j = 0; j < gMem.ncSpaces; j++)
    {
        CodeSpace *space = gMem.cSpaces[j];
        // N.B.  We must not scan beyond topPointer here.  Above that is a byte
        // filler "cell" and the code here will attempt to share that possibly
        // replacing its length word with a forwarding pointer.
        sharer.ScanAddressesInRegion(space->bottom, space->topPointer);
    }

    // Process the RTS roots.
    GCModules(&sharer);

    gHeapSizeParameters.RecordGCTime(HeapSizeParameters::GCTimeIntermediate, "Table");

    // Sort and merge the data.
    sharer.SortData();

    gHeapSizeParameters.RecordGCTime(HeapSizeParameters::GCTimeIntermediate, "Sort");
}
