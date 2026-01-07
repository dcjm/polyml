/*
    Title:      Share common immutable data

    Copyright (c) 2000
        Cambridge University Technical Services Limited
    and David C. J. Matthews 2006, 2010-13, 2016-17, 2019

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

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
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

#include <vector>

#include "globals.h"
#include "save_vec.h"
#include "machine_dep.h"
#include "scanaddrs.h"
#include "run_time.h"
#include "sys.h"
#include "gc.h"
#include "rtsentry.h"
#include "memmgr.h"
#include "processes.h"
#include "gctaskfarm.h"
#include "diagnostics.h"
#include "sharedata.h"
#include "gc_progress.h"

/*
This code was largely written by Simon Finn as a database improver for the
memory-mapped persistent store version.  The aim is that where two immutable
objects (cells) contain the same data (i.e. where ML equality would say they
were equal) they should be merged so that only a single object is retained.

The basic algorithm works like this:
1. From the root, recursively process all objects and calculate a "depth"
   for each object.  Mutable data and code segments have depth 0 and
   cannot be merged.  Byte segments (e.g. strings and long-format arbitrary
   precision values) have depth 1.  Other cells have depths of 1 or greater,
   the depth being the maximum recursion depth until a byte segment or an
   object with depth 0 is reached.  Cycles of immutable data don't arise
   normally in ML but could be produced as a result of locking mutable objects.
   To avoid infinite recursion cycles are broken by setting the depth of an
   object to zero before processing it.  The depth of each object is stored
   in the length word of the object.  This ensures each object is processed
   once only.
2. Vectors are created containing objects of the same depth, from 1 to the
   maximum depth found.
3. We begin a loop starting at depth 1.
4. The length words are restored, replacing the depth count in the header.
5. The objects are sorted by their contents so bringing together objects
   with the same contents.  The contents are considered simply as
   uninterpreted bits.
6. The sorted vector is processed to find those objects that are actually
   bitwise equal.  One object is selected to be retained and other objects
   have their length words turned into tombstones pointing at the retained
   object.
7. Objects at the next depth are first processed to find pointers to objects
   that moved in the previous step (or that step with a lower depth).  The
   addresses are updated to point to the retained object.  The effect of this
   step is to ensure that now two objects that are equal in ML terms have
   identical contents.
   e.g. If we have
      val a = ("abc", "def") and b = ("abc", "def")
   then we will have merged the two occurrences of "abc" and "def" in the
   previous pass of level 1 objects.  This step ensures that the two cells
   containing the pairs both hold pointers to the same objects and so are
   bitwise equal.
8. Repeat with 4, 5 and 6 until all the levels have been processed.

Each object is processed once and at the end most of the objects have been
updated with the shared addresses.  We have to scan all the mutable and
code objects to update the addresses but also have to scan the immutables
because of the possibility of missing an update as a result of breaking a
loop (see SPF's comment below).
DCJM 3/8/06

This has been substantially updated while retaining the basic algorithm.
Sorting is now done in parallel by the GC task farm and the stack is
now in dynamic memory.  That avoids a possible segfault if the normal
C stack overflows.

A further problem is that the vectors can get very large and this
can cause problems if there is insufficient contiguous space.
The code has been modified to reduce the size of the vectors
at the cost of increasing the total memory requirement.
*/

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyShareCommonData(POLYUNSIGNED threadId, POLYUNSIGNED root);
}

// The depth is stored in the length field.  If the Weak bit is set but the Mutable bit
// is clear the value in the length word is a depth rather than a real length.
// The tombstone bit is zero.
// Previously "depth" values were encoded with the tombstone bit set but that isn't
// possible in 32-in-64 because we need 31 bits in a forwarding pointer.
inline bool OBJ_IS_DEPTH(POLYUNSIGNED L) { return (L & (_OBJ_WEAK_BIT| _OBJ_MUTABLE_BIT)) == _OBJ_WEAK_BIT; }
inline POLYUNSIGNED OBJ_GET_DEPTH(POLYUNSIGNED L) { return OBJ_OBJECT_LENGTH(L); }
inline POLYUNSIGNED OBJ_SET_DEPTH(POLYUNSIGNED n) { return n | _OBJ_WEAK_BIT; }

// The DepthVector type contains all the items of a particular depth.
// This is the abstract class.  There are variants for the case where all
// the cells have the same size and where they may vary.
class DepthVector {
public:
    DepthVector() : nitems(0), vsize(0), ptrVector(0) {}

    virtual ~DepthVector() { free(ptrVector);  }
    virtual POLYUNSIGNED MergeSameItems(void);
    virtual void Sort(void);
    virtual POLYUNSIGNED ItemCount(void) { return nitems; }

    virtual void AddToVector(POLYUNSIGNED L, PolyObject *pt) = 0;

    void FixLengthAndAddresses(ScanAddress *scan);

    virtual void RestoreForwardingPointers() = 0;

protected:
    POLYUNSIGNED    nitems;
    POLYUNSIGNED    vsize;
    PolyObject      **ptrVector;

    // This must only be called BEFORE sorting.  The pointer vector will be
    // modified by sorting but the length vector is not.
    virtual void RestoreLengthWords(void) = 0;

    static void SortRange(PolyObject * *first, PolyObject * *last);

    static int CompareItems(const PolyObject * const *a, const PolyObject * const *b);

    static int qsCompare(const void *a, const void *b)
    {
        return CompareItems((const PolyObject * const*)a, (const PolyObject *const *)b);
    }

    static void sortTask(GCTaskId*, void *s, void *l)
    {
        SortRange((PolyObject **)s, (PolyObject **)l);
    }
};

// DepthVector where the size needs to be held for each item.
class DepthVectorWithVariableLength: public DepthVector {
public:
    DepthVectorWithVariableLength() : lengthVector(0) {}
    virtual ~DepthVectorWithVariableLength() { free(lengthVector); }

    virtual void RestoreLengthWords(void);
    virtual void AddToVector(POLYUNSIGNED L, PolyObject *pt);
    virtual void RestoreForwardingPointers();

protected:
    POLYUNSIGNED    *lengthVector; // Same size as the ptrVector
};

class DepthVectorWithFixedLength : public DepthVector {
public:
    DepthVectorWithFixedLength(POLYUNSIGNED l) : length(l) {}

    virtual void RestoreLengthWords(void);
    virtual void AddToVector(POLYUNSIGNED L, PolyObject *pt);

    // It's safe to run this again for the fixed length vectors.
    virtual void RestoreForwardingPointers() { RestoreLengthWords(); }

protected:
    POLYUNSIGNED length;
};


// We have special vectors for the sizes from 1 to FIXEDLENGTHSIZE-1.
// Zero-sized and large objects go in depthVectorArray[0].
#define FIXEDLENGTHSIZE     10

class ShareDataClass {
public:
    ShareDataClass();
    ~ShareDataClass();

    bool RunShareData(PolyObject *root);
    void AddToVector(POLYUNSIGNED depth, POLYUNSIGNED length, PolyObject *pt);

private:
	std::vector<DepthVector*> depthVectorArray[FIXEDLENGTHSIZE];

    POLYUNSIGNED maxVectorSize;
};

ShareDataClass::ShareDataClass()
{
    maxVectorSize = 0;
}

ShareDataClass::~ShareDataClass()
{
    // Free the bitmaps associated with the permanent spaces.
    for (std::vector<PermanentMemSpace*>::iterator i = gMem.pSpaces.begin(); i < gMem.pSpaces.end(); i++)
       (*i)->shareBitmap.Destroy();

    // Free the depth vectors.
    for (unsigned i = 0; i < FIXEDLENGTHSIZE; i++)
    {
		for (std::vector <DepthVector*>::iterator j = depthVectorArray[i].begin(); j < depthVectorArray[i].end(); j++)
			delete(*j);
	}
}

// Grow the appropriate depth vector if necessary and add the item to it.
void ShareDataClass::AddToVector(POLYUNSIGNED depth, POLYUNSIGNED length, PolyObject *pt)
{
    // Select the appropriate vector.  Element zero is the variable length vector and is
    // also used for the, rare, zero length objects.
    std::vector<DepthVector*> *vectorToUse = &(depthVectorArray[length < FIXEDLENGTHSIZE ? length : 0]);

    if (depth >= maxVectorSize) maxVectorSize = depth+1;

	while (vectorToUse->size() <= depth)
	{
		try {
			if (length != 0 && length < FIXEDLENGTHSIZE)
				vectorToUse->push_back(new DepthVectorWithFixedLength(length));
			else vectorToUse->push_back(new DepthVectorWithVariableLength);
		}
		catch (std::bad_alloc&) {
			throw MemoryException();
		}
	}

    (*vectorToUse)[depth]->AddToVector(length, pt);
}

// Add an object to a depth vector
void DepthVectorWithVariableLength::AddToVector(POLYUNSIGNED L, PolyObject *pt)
{
    ASSERT (this->nitems <= this->vsize);

    if (this->nitems == this->vsize)
    {
        // The vector is full or has not yet been allocated.  Grow it by 50%.
        POLYUNSIGNED new_vsize = this->vsize + this->vsize / 2 + 1;
        if (new_vsize < 15)
            new_vsize = 15;

        // First the length vector.
        POLYUNSIGNED *newLength = (POLYUNSIGNED *)realloc(this->lengthVector, new_vsize * sizeof(POLYUNSIGNED));
        if (newLength == 0)
        {
            // The vectors can get large and we may not be able to grow them
            // particularly if the address space is limited in 32-bit mode.
            // Try again with just a small increase.
            new_vsize = this->vsize + 15;
            newLength = (POLYUNSIGNED *)realloc(this->lengthVector, new_vsize * sizeof(POLYUNSIGNED));
            // If that failed give up.
            if (newLength == 0)
                throw MemoryException();
        }

        PolyObject **newPtrVector = (PolyObject * *)realloc (this->ptrVector, new_vsize*sizeof(PolyObject *));

        if (newPtrVector == 0)
        {
            new_vsize = this->vsize + 15;
            newPtrVector = (PolyObject **)realloc (this->ptrVector, new_vsize*sizeof(PolyObject *));
            // If that failed give up.
            if (newPtrVector == 0)
                throw MemoryException();
        }

        this->lengthVector = newLength;
        this->ptrVector = newPtrVector;
        this->vsize  = new_vsize;
    }

    ASSERT (this->nitems < this->vsize);
    this->lengthVector[this->nitems]  = L;
    this->ptrVector[this->nitems] = pt;
    this->nitems++;
    ASSERT (this->nitems <= this->vsize);
}

// Add an object to a depth vector
void DepthVectorWithFixedLength::AddToVector(POLYUNSIGNED L, PolyObject *pt)
{
    ASSERT(this->nitems <= this->vsize);
    ASSERT(L == length);

    if (this->nitems == this->vsize)
    {
        // The vector is full or has not yet been allocated.  Grow it by 50%.
        POLYUNSIGNED new_vsize = this->vsize + this->vsize / 2 + 1;
        if (new_vsize < 15)
            new_vsize = 15;

        PolyObject **newPtrVector = (PolyObject * *)realloc(this->ptrVector, new_vsize * sizeof(PolyObject *));

        if (newPtrVector == 0)
        {
            new_vsize = this->vsize + 15;
            newPtrVector = (PolyObject **)realloc(this->ptrVector, new_vsize * sizeof(PolyObject *));
            // If that failed give up.
            if (newPtrVector == 0)
                throw MemoryException();
        }

        this->ptrVector = newPtrVector;
        this->vsize = new_vsize;
    }

    ASSERT(this->nitems < this->vsize);
    this->ptrVector[this->nitems] = pt;
    this->nitems++;
    ASSERT(this->nitems <= this->vsize);
}

// Comparison function used for sorting and also to test whether
// two cells can be merged.
int DepthVector::CompareItems(const PolyObject *const *a, const PolyObject *const *b)
{
    const PolyObject *x = *a;
    const PolyObject *y = *b;
    POLYUNSIGNED  lX = x->LengthWord();
    POLYUNSIGNED  lY = y->LengthWord();

//    ASSERT (OBJ_IS_LENGTH(lX));
//    ASSERT (OBJ_IS_LENGTH(lY));

    if (lX > lY) return  1; // These tests include the flag bits
    if (lX < lY) return -1;

    // Return simple bitwise equality.
    return memcmp(x, y, OBJ_OBJECT_LENGTH(lX)*sizeof(PolyWord));
}

// Merge cells with the same contents.
POLYUNSIGNED DepthVector::MergeSameItems()
{
    POLYUNSIGNED  N = this->nitems;
    POLYUNSIGNED  n = 0;
    POLYUNSIGNED  i = 0;

    while (i < N)
    {
        PolyObject *bestShare = 0; // Candidate to share.
        MemSpace *bestSpace = 0;

        POLYUNSIGNED j;
        for (j = i; j < N; j++)
        {
            ASSERT (OBJ_IS_LENGTH(ptrVector[i]->LengthWord()));
            // Search for identical objects.  Don't bother to compare it with itself.
            if (i != j && CompareItems (&ptrVector[i], &ptrVector[j]) != 0) break;
            // The order of sharing is significant.
            // Choose an object in the permanent memory if that is available.
            // This is necessary to retain the invariant that no object in
            // the permanent memory points to an object in the temporary heap.
            // (There may well be pointers to this object elsewhere in the permanent
            // heap).
            // Choose the lowest hierarchy value for preference since that
            // may reduce the size of saved state when resaving already saved
            // data.
            // If we can't find a permanent space choose a space that isn't
            // an allocation space.  Otherwise we could break the invariant
            // that immutable areas never point into the allocation area.
            MemSpace *space = gMem.SpaceForObjectAddress(ptrVector[j]);
            if (bestSpace == 0)
            {
                bestShare = ptrVector[j];
                bestSpace = space;
            }
            else if (bestSpace->spaceType == ST_LOCAL)
            {
                // Update if the current space is not an allocation space
                if (space->spaceType != ST_LOCAL || ! ((LocalMemSpace*)space)->allocationSpace)
                {
                    bestShare = ptrVector[j];
                    bestSpace = space;
                }
            }
        }
        POLYUNSIGNED k = j; // Remember the first object that didn't match.
        // For each identical object set all but the one we want to point to
        // the shared object.
        for (j = i; j < k; j++)
        {
            ASSERT (OBJ_IS_LENGTH(ptrVector[j]->LengthWord()));
            if (ptrVector[j] != bestShare)
            {
                ptrVector[j]->SetForwardingPtr(bestShare); /* an indirection */
                n++;
            }
        }
        i = k;
    }

    return n;
}

// Sort this vector
void DepthVector::Sort()
{
    if (nitems > 1)
    {
        SortRange(ptrVector, ptrVector + (nitems - 1));
        gpTaskFarm->WaitForCompletion();
    }

    // Check
//    for (POLYUNSIGNED i = 0; i < nitems-1; i++)
//       ASSERT(CompareItems(vector+i, vector+i+1) <= 0);
}

inline void swapItems(PolyObject * *i, PolyObject * *j)
{
    PolyObject * t = *i;
    *i = *j;
    *j = t;
}

// Simple parallel quick-sort.  "first" and "last" are the first
// and last items (inclusive) in the vector.
void DepthVector::SortRange(PolyObject * *first, PolyObject * *last)
{
    while (first < last)
    {
        if (last-first <= 100)
        {
            // Use the standard library function for small ranges.
            qsort(first, last-first+1, sizeof(PolyObject *), qsCompare);
            return;
        }
        // Select the best pivot from the first, last and middle item
        // by sorting these three items.  We use the middle item as
        // the pivot and since the first and last items are sorted
        // by this we can skip them when we start the partitioning.
        PolyObject * *middle = first + (last-first)/2;
        if (CompareItems(first, middle) > 0)
            swapItems(first, middle);
        if (CompareItems(middle, last) > 0)
        {
            swapItems(middle, last);
            if (CompareItems(first, middle) > 0)
                swapItems(first, middle);
        }

        // Partition the data about the pivot.  This divides the
        // vector into two partitions with all items <= pivot to
        // the left and all items >= pivot to the right.
        // Note: items equal to the pivot could be in either partition.
        PolyObject * *f = first+1;
        PolyObject * *l = last-1;

        do {
            // Find an item we have to move.  These loops will always
            // terminate because testing the middle with itself
            // will return == 0.
            while (CompareItems(f, middle/* pivot*/) < 0)
                f++;
            while (CompareItems(middle/* pivot*/, l) < 0)
                l--;
            // If we haven't finished we need to swap the items.
            if (f < l)
            {
                swapItems(f, l);
                // If one of these was the pivot item it will have moved to
                // the other position.
                if (middle == f)
                    middle = l;
                else if (middle == l)
                    middle = f;
                f++;
                l--;
            }
            else if (f == l)
            {
                f++;
                l--;
                break;
            }
        } while (f <= l);

        // Process the larger partition as a separate task or
        // by recursion and do the smaller partition by tail
        // recursion.
        if (l-first > last-f)
        {
            // Lower part is larger
            gpTaskFarm->AddWorkOrRunNow(sortTask, first, l);
            first = f;
        }
        else
        {
            // Upper part is larger
            gpTaskFarm->AddWorkOrRunNow(sortTask, f, last);
            last = l;
        }
    }
}

// Set the genuine length word.  This overwrites both depth words and forwarding pointers.
void DepthVectorWithVariableLength::RestoreLengthWords()
{
    for (POLYUNSIGNED i = 0; i < this->nitems; i++)
    {
        PolyObject* obj = ptrVector[i];
        obj = gMem.SpaceForObjectAddress(obj)->writeAble(obj); // This could be code.
        obj->SetLengthWord(lengthVector[i]); // restore genuine length word
    }
}
void DepthVectorWithFixedLength::RestoreLengthWords()
{
    for (POLYUNSIGNED i = 0; i < this->nitems; i++)
        ptrVector[i]->SetLengthWord(length); // restore genuine length word
}

// Fix up the length word.  Then update all addresses to their new location if
// we have shared the original destination of the address with something else.
void DepthVector::FixLengthAndAddresses(ScanAddress *scan)
{
    RestoreLengthWords();
    for (POLYUNSIGNED i = 0; i < this->nitems; i++)
    {
        // Fix up all addresses.
        scan->ScanAddressesInObject(ptrVector[i]);
    }
}

// Restore the original length words on forwarding pointers.
// After sorting the pointer vector and length vector are no longer
// matched so we have to follow the pointers.
void DepthVectorWithVariableLength::RestoreForwardingPointers()
{
    for (POLYUNSIGNED i = 0; i < this->nitems; i++)
    {
        PolyObject *obj = ptrVector[i];
        if (obj->ContainsForwardingPtr())
            obj->SetLengthWord(obj->GetForwardingPtr()->LengthWord());
    }
}

// This class is used in two places and is called to ensure that all
// object length words have been restored.
// Before we actually try to share the immutable objects at a particular depth it
// is called to update addresses in those objects to take account of
// sharing at lower depths.
// When all sharing is complete it is called to update the addresses in
// level zero objects, i.e. mutables and code.
class ProcessFixupAddress: public ScanAddress
{
protected:
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt);
    virtual POLYUNSIGNED ScanCodeAddressAt(PolyObject **pt);
    virtual PolyObject *ScanObjectAddress(PolyObject *base)
        { return GetNewAddress(base).AsObjPtr(); }
    PolyWord GetNewAddress(PolyWord old);
};

POLYUNSIGNED ProcessFixupAddress::ScanAddressAt(PolyWord *pt)
{
    *pt = GetNewAddress(*pt);
    return 0;
}

// Don't have to do anything for code since it isn't moved.
POLYUNSIGNED ProcessFixupAddress::ScanCodeAddressAt(PolyObject **pt)
{
    return 0;
}

// Returns the new address if the argument is the address of an object that
// has moved, otherwise returns the original.
PolyWord ProcessFixupAddress::GetNewAddress(PolyWord old)
{
    if (old.IsTagged() || old == PolyWord::FromUnsigned(0))
        return old; //  Nothing to do.

    ASSERT(old.IsDataPtr());

    PolyObject *obj = old.AsObjPtr();
    POLYUNSIGNED L = obj->LengthWord();

    if (obj->ContainsForwardingPtr()) // tombstone is a pointer to a shared object
    {
        PolyObject *newp = obj->GetForwardingPtr();
//        ASSERT (newp->ContainsNormalLengthWord());
        return newp;
    }

    // Generally each address will point to an object processed at a lower depth.
    // The exception is if we have a cycle and have assigned the rest of the
    // structure to a higher depth.
    // N.B. We return the original address here but this could actually share
    // with something else and not be retained.
    if (OBJ_IS_DEPTH(L))
        return old;

    ASSERT (obj->ContainsNormalLengthWord()); // object is not shared
    return old;
}

// This class is used to set up the depth vectors for sorting.  It subclasses ScanAddress
// in order to be able to use that for code objects since they are complicated but it
// handles all the other object types itself.  It scans them depth-first using an explicit stack.
class ProcessAddToVector: public ScanAddress
{
public:
    ProcessAddToVector(ShareDataClass *p): m_parent(p), addStack(0), stackSize(0), asp(0) {}

    ~ProcessAddToVector();

    // These are used when scanning code areas.  They return either
    // a length or a possibly updated address.
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt)
        { (void)AddPolyWordToDepthVectors(*pt); return 0; }
    virtual PolyObject *ScanObjectAddress(PolyObject *base)
        { (void)AddObjectToDepthVector(base); return base; }
    virtual POLYUNSIGNED ScanCodeAddressAt(PolyObject** pt)
        { *pt = ScanObjectAddress(*pt); return 0; }

    void ProcessRoot(PolyObject *root);

protected:
    // Process an address and return the "depth".
    POLYUNSIGNED AddPolyWordToDepthVectors(PolyWord old);
    POLYUNSIGNED AddObjectToDepthVector(PolyObject *obj);

    void PushToStack(PolyObject *obj);

    ShareDataClass *m_parent;
    PolyObject **addStack;
    unsigned stackSize;
    unsigned asp;
};

ProcessAddToVector::~ProcessAddToVector()
{
    // Normally the stack will be empty.  However if we have run out of
    // memory and thrown an exception we may well have items left.
    // We have to remove the mark bits otherwise it will mess up any
    // subsequent GC.
    for (unsigned i = 0; i < asp; i++)
    {
        PolyObject *obj = addStack[i];
        if (obj->LengthWord() & _OBJ_GC_MARK)
            obj->SetLengthWord(obj->LengthWord() & (~_OBJ_GC_MARK));
    }

    free(addStack); // Now free the stack
}

POLYUNSIGNED ProcessAddToVector::AddPolyWordToDepthVectors(PolyWord old)
{
    // If this is a tagged integer or an IO pointer that's simply a constant.
    if (old.IsTagged() || old == PolyWord::FromUnsigned(0))
        return 0;
    return AddObjectToDepthVector(old.AsObjPtr());
}

// Either adds an object to the stack or, if its depth is known, adds it
// to the depth vector and returns the depth.
// We use _OBJ_GC_MARK to detect when we have visited a cell but not yet
// computed the depth.  We have to be careful that this bit is removed
// before we finish in the case that we run out of memory and throw an
// exception.  PushToStack may throw the exception if the stack needs to
// grow.
POLYUNSIGNED ProcessAddToVector::AddObjectToDepthVector(PolyObject *obj)
{
    MemSpace *space = gMem.SpaceForObjectAddress(obj);
    if (space == 0)
        return 0;

    POLYUNSIGNED L = obj->LengthWord();

    if (OBJ_IS_DEPTH(L)) // tombstone contains genuine depth or 0.
        return OBJ_GET_DEPTH(L);

    if (obj->LengthWord() & _OBJ_GC_MARK)
        return 0; // Marked but not yet scanned. Circular structure.

    ASSERT (OBJ_IS_LENGTH(L));

    if (obj->IsMutable())
    {
        // Mutable data in the local or permanent areas.  Ignore byte objects or
        // word objects containing only ints.
        if (obj->IsWordObject())
        {
            bool containsAddress = false;
            for (POLYUNSIGNED j = 0; j < OBJ_OBJECT_LENGTH(L) && !containsAddress; j++)
                containsAddress = ! obj->Get(j).IsTagged();

            if (containsAddress)
            {
                // Add it to the vector so we will update any addresses it contains.
                m_parent->AddToVector(0, L, obj);
                // and follow any addresses to try to merge those.
                PushToStack(obj);
                obj->SetLengthWord(L | _OBJ_GC_MARK); // To prevent rescan
            }
            // If we don't add it to the vector we mustn't set _OBJ_GC_MARK.
        }
        return 0; // Level is zero
    }

    if (space->spaceType == ST_PERMANENT && ((PermanentMemSpace*)space)->isWriteProtected)
    {
        // Immutable data in the permanent area can't be merged
        // because it's read only.  We need to follow the addresses
        // because they may point to mutable areas containing data
        // that can be.  A typical case is the root function pointing
        // at the global name table containing new declarations.
        Bitmap *bm = &((PermanentMemSpace*)space)->shareBitmap;
        if (! bm->TestBit((PolyWord*)obj - space->bottom))
        {
            bm->SetBit((PolyWord*)obj - space->bottom);
            if (! obj->IsByteObject())
                PushToStack(obj);
        }
        return 0;
    }

    /* There's a problem sharing code objects if they have relative calls/jumps
       in them to other code.  The code of two functions may be identical (e.g.
       they both call functions 100 bytes ahead) and so they will appear the
       same but if the functions they jump to are different they are actually
       different.  For that reason we don't share code segments.  DCJM 4/1/01 */
    if (obj->IsCodeObject())
    {
        // We want to update addresses in the code segment.
        m_parent->AddToVector(0, L, obj);
        PushToStack(obj);
        gMem.SpaceForObjectAddress(obj)->writeAble(obj)->SetLengthWord(L | _OBJ_GC_MARK); // To prevent rescan

        return 0;
    }

    // Byte objects always have depth 1 and can't contain addresses.
    if (obj->IsByteObject())
    {
        m_parent->AddToVector (1, L, obj);// add to vector at correct depth
        obj->SetLengthWord(OBJ_SET_DEPTH(1));
        return 1;
    }

    ASSERT(OBJ_IS_WORD_OBJECT(L) || OBJ_IS_CLOSURE_OBJECT(L)); // That leaves immutable data objects.
    PushToStack(obj);
    obj->SetLengthWord(L | _OBJ_GC_MARK); // To prevent rescan

    return 0;
}

// Adds an object to the stack.
void ProcessAddToVector::PushToStack(PolyObject *obj)
{
    if (asp == stackSize)
    {
        if (addStack == 0)
        {
            addStack = (PolyObject**)malloc(sizeof(PolyObject*) * 100);
            if (addStack == 0) throw MemoryException();
            stackSize = 100;
        }
        else
        {
            unsigned newSize = stackSize+100;
            PolyObject** newStack = (PolyObject**)realloc(addStack, sizeof(PolyObject*) * newSize);
            if (newStack == 0) throw MemoryException();
            stackSize = newSize;
            addStack = newStack;
        }
    }

    ASSERT(asp < stackSize);

    addStack[asp++] = obj;
}

// Processes the root and anything reachable from it.  Addresses are added to the
// explicit stack if an object has not yet been processed.  Most of this function
// is about processing the stack.
void ProcessAddToVector::ProcessRoot(PolyObject *root)
{
    // Mark the initial object
    AddObjectToDepthVector(root);

    // Process the stack until it's empty.
    while (asp != 0)
    {
        // Pop it from the stack.
        PolyObject *obj = addStack[asp-1];

        if (obj->IsCodeObject())
        {
            // Code cells are now only found in the code area.
            /* There's a problem sharing code objects if they have relative calls/jumps
               in them to other code.  The code of two functions may be identical (e.g.
               they both call functions 100 bytes ahead) and so they will appear the
               same but if the functions they jump to are different they are actually
               different.  For that reason we don't share code segments.  DCJM 4/1/01 */
            asp--; // Pop it because we'll process it completely
            ScanAddressesInObject(obj);
            // If it's local set the depth with the value zero.  It has already been
            // added to the zero depth vector.
            if (obj->LengthWord() & _OBJ_GC_MARK)
                gMem.SpaceForObjectAddress(obj)->writeAble(obj)->SetLengthWord(OBJ_SET_DEPTH(0)); // Now scanned
        }

        else
        {
            POLYUNSIGNED length = obj->Length();
            PolyWord *pt = (PolyWord*)obj;
            unsigned osp = asp;

            if (obj->IsClosureObject())
            {
                // The first word of a closure is a code pointer.  We don't share code but
                // we do want to share anything reachable from the constants.
                AddObjectToDepthVector(*(PolyObject**)pt);
                pt += sizeof(PolyObject*) / sizeof(PolyWord);
                length -= sizeof(PolyObject*) / sizeof(PolyWord);
            }

            if (((obj->LengthWord() & _OBJ_GC_MARK) && !obj->IsMutable()))
            {
                // Immutable local objects.  These can be shared.  We need to compute the
                // depth by computing the maximum of the depth of all the addresses in it.
                POLYUNSIGNED depth = 0;
                while (length != 0 && osp == asp)
                {
                    POLYUNSIGNED d = AddPolyWordToDepthVectors(*pt);
                    if (d > depth) depth = d;
                    pt++;
                    length--;
                }

                if (osp == asp)
                {
                    // We've finished it
                    asp--; // Pop this item.
                    depth++; // One more for this object
                    obj->SetLengthWord(obj->LengthWord() & (~_OBJ_GC_MARK));
                    m_parent->AddToVector(depth, obj->LengthWord() & (~_OBJ_GC_MARK), obj);
                    obj->SetLengthWord(OBJ_SET_DEPTH(depth));
                }
            }
            else
            {
                // Mutable or non-local objects.  These have depth zero.  Local objects have
                // _OBJ_GC_MARK in their header.  Immutable permanent objects cannot be
                // modified so we don't set the depth.  Mutable objects are added to the
                // depth vectors even though they aren't shared so that they will be
                // updated if they point to immutables that have been shared.
                while (length != 0)
                {
                    if (!(*pt).IsTagged())
                    {
                        // If we've already pushed an address break now
                        if (osp != asp) break;
                        // Process the address and possibly push it
                        AddPolyWordToDepthVectors(*pt);
                    }
                    pt++;
                    length--;
                }

                if (length == 0)
                {
                    // We've finished it
                    if (osp != asp)
                    {
                        ASSERT(osp == asp - 1);
                        addStack[osp - 1] = addStack[osp];
                    }
                    asp--; // Pop this item.
                    if (obj->LengthWord() & _OBJ_GC_MARK)
                        obj->SetLengthWord(OBJ_SET_DEPTH(0));
                }
            }
        }
    }
}

// This is called by the root thread to do the work.
bool ShareDataClass::RunShareData(PolyObject *root)
{
    // We use a bitmap to indicate when we've visited an object to avoid
    // infinite recursion in cycles in the data.
    for (std::vector<PermanentMemSpace*>::iterator i = gMem.pSpaces.begin(); i < gMem.pSpaces.end(); i++)
    {
        PermanentMemSpace *space = *i;
        if (space->isWriteProtected)
        {
            if (! space->shareBitmap.Create(space->spaceSize()))
                return false;
        }
    }

    POLYUNSIGNED totalObjects = 0;
    POLYUNSIGNED totalShared  = 0;

    // Build the vectors from the immutable objects.
    bool success = true;

    try {
        ProcessAddToVector addToVector(this);
        addToVector.ProcessRoot(root);
    }
    catch (MemoryException &)
    {
        // If we ran out of memory we may still be able to process what we have.
        // That will also do any clean-up.
        success = false;
    }

    ProcessFixupAddress fixup;

    for (POLYUNSIGNED depth = 1; depth < maxVectorSize; depth++)
    {
        for (unsigned j = 0; j < FIXEDLENGTHSIZE; j++)
        {
            if (depth < depthVectorArray[j].size())
            {
                DepthVector *vec = depthVectorArray[j][depth];
                // Set the length word and update all addresses.
                vec->FixLengthAndAddresses(&fixup);

                vec->Sort();

                POLYUNSIGNED n = vec->MergeSameItems();

                if ((debugOptions & DEBUG_SHARING) && n > 0)
                    Log("Sharing: Level %4" POLYUFMT ", size %3u, Objects %6" POLYUFMT ", Shared %6" POLYUFMT " (%1.0f%%)\n",
                        depth, j, vec->ItemCount(), n, (float)n / (float)vec->ItemCount() * 100.0);

                totalObjects += vec->ItemCount();
                totalShared += n;
            }
        }
    }
    
    if (debugOptions & DEBUG_SHARING)
        Log("Sharing: Maximum level %4" POLYUFMT ",\n", maxVectorSize);

      /*
       At this stage, we have fixed up most but not all of the forwarding
       pointers. The ones that we haven't fixed up arise from situations
       such as the following:

               X -> Y <-> Z

       i.e. Y and Z form a loop, and X is isomorphic to Z. When we assigned
       the depths, we have to arbitrarily break the loop between Y and Z.
       Suppose Y is assigned to level 1, and Z is assigned to level 2.
       When we process level 1 and fixup Y, there's nothing to do, since
       Z is still an ordinary object. However when we process level 2,
       we find that X and Z are isomorphic so we arbitrarily choose one
       of them and turn it into a "tombstone" pointing at the other. If
       we change Z into the tombstone, then Y now contains a pointer
       that needs fixing up. That's why we need the second fixup pass.

       Note also that if we had broken the loop the other way, we would have
       assigned Z to level 1, Y to level 2 and X to level 3, so we would
       have missed the chance to share Z and X. Perhaps that's why running
       the program repeatedly sometimes finds extra things to share?

      SPF 26/1/95
    */

    /* We have updated the addresses in objects with non-zero level so they point to
       the single occurrence but we need to do the same with level 0 objects
       (mutables and code). */
    for (unsigned j = 0; j < FIXEDLENGTHSIZE; j++)
    {
        if (! depthVectorArray[j].empty())
        {
            DepthVector *v = depthVectorArray[j][0];
            // Log this because it could be very large.
            if (debugOptions & DEBUG_SHARING)
                Log("Sharing: Level %4" POLYUFMT ", size %3u, Objects %6" POLYUFMT "\n", 0ul, j, v->ItemCount());
            v->FixLengthAndAddresses(&fixup);
        }
    }
    /* Previously we made a complete scan over the memory updating any addresses so
       that if we have shared two substructures within our root we would also
       share any external pointers.  This has been removed but we have to
       reinstate the length words we've overwritten with forwarding pointers because
       there may be references to unshared objects from outside. */
    for (POLYUNSIGNED d = 1; d < maxVectorSize; d++)
    {
        for (unsigned j = 0; j < FIXEDLENGTHSIZE; j++)
        {
            if (d < depthVectorArray[j].size())
            {
                DepthVector *v = depthVectorArray[j][d];
                v->RestoreForwardingPointers();
            }
        }
    }

    if (debugOptions & DEBUG_SHARING)
        Log ("Sharing: Total Objects %6" POLYUFMT ", Total Shared %6" POLYUFMT " (%1.0f%%)\n",
            totalObjects, totalShared, (float)totalShared / (float)totalObjects * 100.0);

    return success; // Succeeded.
}

class ShareRequest: public MainThreadRequest
{
public:
    ShareRequest(Handle root): MainThreadRequest(MTP_SHARING), shareRoot(root), result(false) {}

    virtual void Perform()
    {
        ShareDataClass s; 
        // Do a full GC.  If we have a large heap the allocation of the vectors
        // can cause paging.  Doing this now reduces the heap and discards the
        // allocation spaces.  It may be overkill if we are applying the sharing
        // to a small root but generally it seems to be applied to the whole heap.
        FullGCForShareCommonData();
		gcProgressBeginOtherGC(); // Set the phase to "other" now the GC is complete.
        // Now do the sharing.
        result = s.RunShareData(shareRoot->WordP());
    }
    Handle shareRoot;
    bool result;
};


// ShareData.  This is the main entry point.
// Because this can recurse deeply it needs to be run by the main thread.
// Also it manipulates the heap in ways that could mess up other threads
// so we need to stop them before executing this.
void ShareData(TaskData *taskData, Handle root)
{
    if (! root->Word().IsDataPtr())
        return; // Nothing to do.  We could do handle a code pointer but it shouldn't occur.

    // Request the main thread to do the sharing.
    ShareRequest request(root);
    processes->MakeRootRequest(taskData, &request);

    // Raise an exception if it failed.
    if (! request.result)
        raise_exception_string(taskData, EXC_Fail, "Insufficient memory");
}

// RTS call entry.
POLYUNSIGNED PolyShareCommonData(POLYUNSIGNED threadId, POLYUNSIGNED root)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();

    try {
        if (!PolyWord::FromUnsigned(root).IsDataPtr())
            return TAGGED(0).AsUnsigned(); // Nothing to do.

        // Request the main thread to do the sharing.
        ShareRequest request(taskData->saveVec.push(root));
        processes->MakeRootRequest(taskData, &request);

        // Raise an exception if it failed.
        if (! request.result)
            raise_exception_string(taskData, EXC_Fail, "Insufficient memory");
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

struct _entrypts shareDataEPT[] =
{
    { "PolyShareCommonData",            (polyRTSFunction)&PolyShareCommonData},

    { NULL, NULL} // End of list.
};

