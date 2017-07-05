/*
    Title:  memmgr.h   Memory segment manager

    Copyright (c) 2006-8, 2010-12, 2016 David C. J. Matthews

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

#ifndef MEMMGR_H
#define MEMMGR_H

#include "bitmap.h"
#include "locking.h"

// utility conversion macros
#define Words_to_K(w) (w*sizeof(PolyWord))/1024
#define Words_to_M(w) (w*sizeof(PolyWord))/(1<<20)
#define B_to_M(b) (b/(1<<20))

class ScanAddress;
class GCTaskId;
class TaskData;

typedef enum {
    ST_IO,          // The io area forms an interface with the RTS
    ST_PERMANENT,   // Permanent areas are part of the object code
                    // Also loaded saved state.
    ST_LOCAL,       // Local heaps contain volatile data
    ST_EXPORT,      // Temporary export area
    ST_STACK,       // ML Stack for a thread
    ST_CODE         // Code created in the current run
} SpaceType;


// B-tree used in SpaceForAddress.  Leaves are MemSpaces.
class SpaceTree
{
public:
    SpaceTree(bool is): isSpace(is) { }
    virtual ~SpaceTree() {}

    bool isSpace;
};

// A non-leaf node in the B-tree
class SpaceTreeTree: public SpaceTree
{
public:
    SpaceTreeTree();
    virtual ~SpaceTreeTree();

    SpaceTree *tree[256];
};

// Base class for the various memory spaces.
class MemSpace: public SpaceTree
{
protected:
    MemSpace();
    virtual ~MemSpace();

public:
    SpaceType       spaceType;
    bool            isMutable;
    bool            isOwnSpace; // True if this has been allocated.
    bool            isCode;

    PolyWord        *bottom;    // Bottom of area
    PolyWord        *top;       // Top of area.
    
    POLYUNSIGNED spaceSize(void)const { return top-bottom; } // No of words

    // These next two are used in the GC to limit scanning for
    // weak refs.
    PolyWord        *lowestWeak, *highestWeak;

    // Used when printing debugging info
    virtual const char *spaceTypeString() { return isMutable ? "mutable" : "immutable"; }

    friend class MemMgr;
};

// Permanent memory space.  Either linked into the executable program or
// loaded from a saved state file.
class PermanentMemSpace: public MemSpace
{
protected:
    PermanentMemSpace(): index(0), hierarchy(0), noOverwrite(false),
        byteOnly(false), topPointer(0) {}
public:
    unsigned    index;      // An identifier for the space.  Used when saving and loading.
    unsigned    hierarchy;  // The hierarchy number: 0=from executable, 1=top level saved state, ...
    bool        noOverwrite; // Don't save this in deeper hierarchies.
    bool        byteOnly; // Only contains byte data - no need to scan for addresses.

    // When exporting or saving state we copy data into a new area.
    // This area grows upwards unlike the local areas that grow down.
    PolyWord    *topPointer;

    Bitmap      shareBitmap; // Used in sharedata

    friend class MemMgr;
};

#define NSTARTS 10

// Local areas can be garbage collected.
class LocalMemSpace: public MemSpace
{
protected:
    LocalMemSpace();
    virtual ~LocalMemSpace() {}
    bool InitSpace(POLYUNSIGNED size, bool mut);

public:
    // Allocation.  The minor GC allocates at the bottom of the areas while the
    // major GC and initial allocations are made at the top.  The reason for this
    // is that it's only possible to scan objects from the bottom up and the minor
    // GC combines scanning with allocation whereas the major GC compacts from the
    // bottom into the top of an area.
    PolyWord    *upperAllocPtr;   // Allocation pointer. Objects are allocated AFTER this.
    PolyWord    *lowerAllocPtr;   // Allocation pointer. Objects are allocated BEFORE this.

    PolyWord    *fullGCLowerLimit;// Lowest object in area before copying.
    PolyWord    *fullGCRescanStart; // Upper and lower limits for rescan during mark phase.
    PolyWord    *fullGCRescanEnd;
    PolyWord    *partialGCTop;    // Value of upperAllocPtr before the current partial GC.
    PolyWord    *partialGCScan;   // Scan pointer used in minor GC
    PolyWord    *partialGCRootBase; // Start of the root objects.
    PolyWord    *partialGCRootTop;// Value of lowerAllocPtr after the roots have been copied.
    PLock       spaceLock;        // Lock used to protect forwarding pointers
    GCTaskId    *spaceOwner;      // The thread that "owns" this space during a GC.

    Bitmap       bitmap;          /* bitmap with one bit for each word in the GC area. */
    bool         allocationSpace; // True if this is (mutable) space for initial allocation
    POLYUNSIGNED start[NSTARTS];  /* starting points for bit searches.                 */
    unsigned     start_index;     /* last index used to index start array              */
    POLYUNSIGNED i_marked;        /* count of immutable words marked.                  */
    POLYUNSIGNED m_marked;        /* count of mutable words marked.                    */
    POLYUNSIGNED updated;         /* count of words updated.                           */
    
    POLYUNSIGNED allocatedSpace(void)const // Words allocated
        { return (top-upperAllocPtr) + (lowerAllocPtr-bottom); }
    POLYUNSIGNED freeSpace(void)const // Words free
        { return upperAllocPtr-lowerAllocPtr; }

    virtual const char *spaceTypeString()
        { return allocationSpace ? "allocation" : MemSpace::spaceTypeString(); }

    // Used when converting to and from bit positions in the bitmap
    POLYUNSIGNED wordNo(PolyWord *pt) { return pt - bottom; }
    PolyWord *wordAddr(POLYUNSIGNED bitno) { return bottom + bitno; }

    friend class MemMgr;
};

class StackObject; // Abstract - Architecture specific

// Stack spaces.  These are managed by the thread module
class StackSpace: public MemSpace
{
public:
    StackSpace() { isOwnSpace = true; }

    StackObject *stack()const { return (StackObject *)bottom; }
};

// Code Space.  These contain local code created by the compiler.
class CodeSpace: public MemSpace
{
    public:
        CodeSpace() { isOwnSpace = true; }

    PolyWord *topPointer; // Allocation point
};

class MemMgr
{
public:
    MemMgr();
    ~MemMgr();

    // Create a local space for initial allocation.
    LocalMemSpace *CreateAllocationSpace(POLYUNSIGNED size);
    // Create and initialise a new local space and add it to the table.
    LocalMemSpace *NewLocalSpace(POLYUNSIGNED size, bool mut);
    // Create an entry for a permanent space.
    PermanentMemSpace *NewPermanentSpace(PolyWord *base, POLYUNSIGNED words,
        unsigned flags, unsigned index, unsigned hierarchy = 0);
    // Create an entry for the IO space.
    MemSpace   *InitIOSpace(PolyWord *base, POLYUNSIGNED words);
    // Delete a local space
    bool DeleteLocalSpace(LocalMemSpace *sp);

    // Allocate an area of the heap of at least minWords and at most maxWords.
    // This is used both when allocating single objects (when minWords and maxWords
    // are the same) and when allocating heap segments.  If there is insufficient
    // space to satisfy the minimum it will return 0.  Updates "maxWords" with
    // the space actually allocated
    PolyWord *AllocHeapSpace(POLYUNSIGNED minWords, POLYUNSIGNED &maxWords, bool doAllocation = true);
    PolyWord *AllocHeapSpace(POLYUNSIGNED words)
        { POLYUNSIGNED allocated = words; return AllocHeapSpace(words, allocated); }

    // Allocate space for code.  This is initially mutable to allow the code to be built.
    PolyWord *AllocCodeSpace(POLYUNSIGNED words);

    // Check that a subsequent allocation will succeed.  Called from the GC to ensure
    bool CheckForAllocation(POLYUNSIGNED words);

    // If an allocation space has a lot of data left in it, particularly a single
    // large object we should turn it into a local area.
    void ConvertAllocationSpaceToLocal(LocalMemSpace *space);

    // Allocate space for the initial stack for a thread.  The caller must
    // initialise the new stack.  Returns 0 if allocation fails.
    StackSpace *NewStackSpace(POLYUNSIGNED size);

    // Adjust the space for a stack.  Returns true if it succeeded.  If it failed
    // it leaves the stack untouched.
    bool GrowOrShrinkStack(TaskData *taskData, POLYUNSIGNED newSize);

    // Delete a stack when a thread has finished.
    bool DeleteStackSpace(StackSpace *space);

    // Create and delete export spaces
    PermanentMemSpace *NewExportSpace(POLYUNSIGNED size, bool mut, bool noOv, bool code);
    void DeleteExportSpaces(void);
    bool PromoteExportSpaces(unsigned hierarchy); // Turn export spaces into permanent spaces.
    bool DemoteImportSpaces(void); // Turn previously imported spaces into local.

    PermanentMemSpace *SpaceForIndex(unsigned index) const; // Return the space for a given index

    // As a debugging check, write protect the immutable areas apart from during the GC.
    void ProtectImmutable(bool on);

    // Find a space that contains a given address.  This is called for every cell
    // during a GC so needs to be fast.,
    // N.B.  This must be called on an address at the beginning or within the cell.
    // Generally that means with a pointer to the length word.  Pointing at the
    // first "data" word may give the wrong result if the length is zero.
    MemSpace *SpaceForAddress(const void *pt) const
    {
        uintptr_t t = (uintptr_t)pt;
        SpaceTree *tr = spaceTree;

        // Each level of the tree is either a leaf or a vector of trees.
        unsigned j = sizeof(void *)*8;
        for (;;)
        {
            if (tr == 0 || tr->isSpace)
                return (MemSpace*)tr;
            j -= 8;
            tr = ((SpaceTreeTree*)tr)->tree[(t >> j) & 0xff];
        }
        return 0;
    }

    // Find a local address for a space.
    // N.B.  The argument should generally be the length word.  See
    // comment on SpaceForAddress.
    LocalMemSpace *LocalSpaceForAddress(const void *pt) const
    {
        MemSpace *s = SpaceForAddress(pt);
        if (s != 0 && s->spaceType == ST_LOCAL)
            return (LocalMemSpace*)s;
        else return 0;
    }

    bool IsIOPointer(const void *pt) const { return pt >= ioSpace->bottom && pt < ioSpace->top; }

    void SetReservation(POLYUNSIGNED words) { reservedSpace = words; }

    // In several places we assume that segments are filled with valid
    // objects.  This fills unused memory with one or more "byte" objects.
    void FillUnusedSpace(PolyWord *base, POLYUNSIGNED words);

    // Return number of words of free space for stats.
    POLYUNSIGNED GetFreeAllocSpace();

    // Remove unused local areas.
    void RemoveEmptyLocals();

    // Remove unused allocation areas to reduce the space below the limit.
    void RemoveExcessAllocation(POLYUNSIGNED words);
    void RemoveExcessAllocation() { RemoveExcessAllocation(spaceBeforeMinorGC); }

    MemSpace *IoSpace() { return ioSpace; } // Return pointer to the IO space.

    MemSpace *ioSpace; // The IO space

    // Table for permanent spaces
    PermanentMemSpace **pSpaces;
    unsigned npSpaces;

    // Table for local spaces
    LocalMemSpace **lSpaces;
    unsigned nlSpaces;

    // Table for export spaces
    PermanentMemSpace **eSpaces;
    unsigned neSpaces;

    // Table for stack spaces
    StackSpace **sSpaces;
    unsigned nsSpaces;
    PLock stackSpaceLock;

    // Table for code spaces
    CodeSpace **cSpaces;
    unsigned ncSpaces;
    PLock codeSpaceLock;

    // Storage manager lock.
    PLock allocLock;

    unsigned nextIndex; // Used when allocating new permanent spaces.

    POLYUNSIGNED SpaceBeforeMinorGC() const { return spaceBeforeMinorGC; }
    POLYUNSIGNED SpaceForHeap() const { return spaceForHeap; }
    void SetSpaceBeforeMinorGC(POLYUNSIGNED minorSize) { spaceBeforeMinorGC = minorSize; }
    void SetSpaceForHeap(POLYUNSIGNED heapSize) { spaceForHeap = heapSize; }

    POLYUNSIGNED CurrentAllocSpace() { return currentAllocSpace; }
    POLYUNSIGNED AllocatedInAlloc();
    POLYUNSIGNED CurrentHeapSize() { return currentHeapSize; }

    POLYUNSIGNED DefaultSpaceSize() const { return defaultSpaceSize; }

    void ReportHeapSizes(const char *phase);

private:
    bool AddLocalSpace(LocalMemSpace *space);
    bool AddCodeSpace(CodeSpace *space);

    POLYUNSIGNED reservedSpace;
    unsigned nextAllocator;
    // The default size in words when creating new segments.
    POLYUNSIGNED defaultSpaceSize;
    // The number of words that can be used for initial allocation.
    POLYUNSIGNED spaceBeforeMinorGC;
    // The number of words that can be used for the heap
    POLYUNSIGNED spaceForHeap;
    // The current sizes of the allocation space and the total heap size.
    POLYUNSIGNED currentAllocSpace, currentHeapSize;
    // LocalSpaceForAddress is a hot-spot so we use a B-tree to convert addresses;
    SpaceTree *spaceTree;
    PLock spaceTreeLock;
    void AddTree(MemSpace *space) { AddTree(space, space->bottom, space->top); }
    void RemoveTree(MemSpace *space) { RemoveTree(space, space->bottom, space->top); }
    void AddTree(MemSpace *space, PolyWord *startS, PolyWord *endS);
    void RemoveTree(MemSpace *space, PolyWord *startS, PolyWord *endS);

    void AddTreeRange(SpaceTree **t, MemSpace *space, uintptr_t startS, uintptr_t endS);
    void RemoveTreeRange(SpaceTree **t, MemSpace *space, uintptr_t startS, uintptr_t endS);
};

extern MemMgr gMem;

#endif
