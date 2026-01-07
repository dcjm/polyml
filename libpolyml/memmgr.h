/*
    Title:  memmgr.h   Memory segment manager

    Copyright (c) 2006-8, 2010-12, 2016-18, 2020, 2021, 2025 David C. J. Matthews

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

#ifndef MEMMGR_H
#define MEMMGR_H

#include "bitmap.h"
#include "locking.h"
#include "osmem.h"
#include "../polyexports.h" // For struct _moduleId
#include <vector>

// utility conversion macros
#define Words_to_K(w) (w*sizeof(PolyWord))/1024
#define Words_to_M(w) (w*sizeof(PolyWord))/(1<<20)
#define B_to_M(b) (b/(1<<20))

class ScanAddress;
class GCTaskId;
class TaskData;

typedef enum {
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
    MemSpace(OSMem *alloc);
    virtual ~MemSpace();

public:
    SpaceType       spaceType;
    bool            isMutable;
    bool            isCode;

    PolyWord        *bottom;    // Bottom of area
    PolyWord        *top;       // Top of area.
    OSMem           *allocator; // Used to free the area.  May be null.

    PolyWord        *shadowSpace; // Extra writable area for code if necessary

    uintptr_t spaceSize(void)const { return top-bottom; } // No of words

    // These next two are used in the GC to limit scanning for
    // weak refs.
    PolyWord        *lowestWeak, *highestWeak;

    // Used when printing debugging info
    virtual const char *spaceTypeString() { return isMutable ? "mutable" : "immutable"; }

    // Return the writeable address if this is in read-only code.
    byte* writeAble(byte* p) {
        if (shadowSpace != 0)
            return (p - (byte*)bottom + (byte*)shadowSpace);
        else return p;
    }

    PolyWord* writeAble(PolyWord* p) {
        if (shadowSpace != 0)
            return (p - bottom + shadowSpace);
        else return p;
    }

    PolyObject* writeAble(PolyObject* p) {
        return (PolyObject*)writeAble((PolyWord *) p);
    }

    friend class MemMgr;
};

// Wrapper class for struct _moduleId.  polyexport.h needs to be compatible with C.
class ModuleId
{
public:
    ModuleId() : modId({ 0,0 }) {}
    ModuleId(uint32_t a, uint32_t b) : modId({ a,b }) {}
    ModuleId(struct _moduleId m) : modId(m) {}

    operator struct _moduleId() const { return modId; }

    ModuleId& operator=(const struct _moduleId& c)
    {
        modId = c;
        return *this;
    }

    bool operator==(const ModuleId& b) const
    {
        return modId.modA == b.modId.modA && modId.modB == b.modId.modB;
    }
    bool operator<(const ModuleId& b) const
    {
        return modId.modA < b.modId.modA || (modId.modA == b.modId.modA && modId.modB < b.modId.modB);
    }
    bool operator!=(const ModuleId& b) const
    {
        return !(*this == b);
    }

    struct _moduleId modId;
};


// Permanent memory space.  Either linked into the executable program or
// loaded from a saved state file or module.
class PermanentMemSpace: public MemSpace
{
protected:
    PermanentMemSpace(OSMem *alloc): MemSpace(alloc), index(0), isWriteProtected(false), noOverwrite(false),
        byteOnly(false), constArea(false), topPointer(0) {
    }

public:
    unsigned    index;      // An identifier for the space.  Used when saving and loading.
    bool        isWriteProtected; // Immutable spaces in the executable are really write protected
    bool        noOverwrite; // Don't save this in deeper hierarchies.
    bool        byteOnly; // Only contains byte data - no need to scan for addresses.
    bool        constArea; // Contains constants rather than code.  Special case for exporting PIE.

    // When exporting or saving state we copy data into a new area.
    // This area grows upwards unlike the local areas that grow down.
    PolyWord    *topPointer;

    Bitmap      shareBitmap; // Used in sharedata
    Bitmap      profileCode; // Used when profiling

    ModuleId    moduleIdentifier; // The identifier of the source module, usually the executable itself.

    friend class MemMgr;
};

#define NSTARTS 10

// Markable spaces are used as the base class for local heap
// spaces and code spaces.
class MarkableSpace: public MemSpace
{
protected:
    MarkableSpace(OSMem *alloc);
    virtual ~MarkableSpace() {}
public:
    PolyWord    *fullGCRescanStart; // Upper and lower limits for rescan during mark phase.
    PolyWord    *fullGCRescanEnd;
    PLock       spaceLock;        // Lock used to protect forwarding pointers
};

// Local areas can be garbage collected.
class LocalMemSpace: public MarkableSpace
{
protected:
    LocalMemSpace(OSMem *alloc);
    virtual ~LocalMemSpace() {}
    bool InitSpace(PolyWord *heapPtr, uintptr_t size, bool mut);

public:
    // Allocation.  The minor GC allocates at the bottom of the areas while the
    // major GC and initial allocations are made at the top.  The reason for this
    // is that it's only possible to scan objects from the bottom up and the minor
    // GC combines scanning with allocation whereas the major GC compacts from the
    // bottom into the top of an area.
    PolyWord    *upperAllocPtr;   // Allocation pointer. Objects are allocated AFTER this.
    PolyWord    *lowerAllocPtr;   // Allocation pointer. Objects are allocated BEFORE this.

    PolyWord    *fullGCLowerLimit;// Lowest object in area before copying.
    PolyWord    *partialGCTop;    // Value of upperAllocPtr before the current partial GC.
    PolyWord    *partialGCScan;   // Scan pointer used in minor GC
    PolyWord    *partialGCRootBase; // Start of the root objects.
    PolyWord    *partialGCRootTop;// Value of lowerAllocPtr after the roots have been copied.
    GCTaskId    *spaceOwner;      // The thread that "owns" this space during a GC.

    Bitmap       bitmap;          /* bitmap with one bit for each word in the GC area. */
    PLock        bitmapLock;      // Lock used in GC sharing pass.
    bool         allocationSpace; // True if this is (mutable) space for initial allocation
    uintptr_t start[NSTARTS];  /* starting points for bit searches.                 */
    unsigned     start_index;     /* last index used to index start array              */
    uintptr_t i_marked;        /* count of immutable words marked.                  */
    uintptr_t m_marked;        /* count of mutable words marked.                    */
    uintptr_t updated;         /* count of words updated.                           */

    uintptr_t allocatedSpace(void)const // Words allocated
        { return (top-upperAllocPtr) + (lowerAllocPtr-bottom); }
    uintptr_t freeSpace(void)const // Words free
        { return upperAllocPtr-lowerAllocPtr; }

#ifdef POLYML32IN64
    // We will generally set a zero cell for alignment.
    bool isEmpty(void)const { return allocatedSpace() <= POLYML32IN64-1; }
#else
    bool isEmpty(void)const { return allocatedSpace() == 0; }
#endif

    virtual const char *spaceTypeString()
        { return allocationSpace ? "allocation" : MemSpace::spaceTypeString(); }

    // Used when converting to and from bit positions in the bitmap
    uintptr_t wordNo(PolyWord *pt) { return pt - bottom; }
    PolyWord *wordAddr(uintptr_t bitno) { return bottom + bitno; }

    friend class MemMgr;
};

class StackObject; // Abstract - Architecture specific

// Stack spaces.  These are managed by the thread module
class StackSpace: public MemSpace
{
public:
    StackSpace(OSMem *alloc): MemSpace(alloc) { }

    StackObject *stack()const { return (StackObject *)bottom; }
};

// Code Space.  These contain local code created by the compiler.
class CodeSpace: public MarkableSpace
{
    public:
    CodeSpace(PolyWord *start, PolyWord *shadow, uintptr_t spaceSize, OSMem *alloc);

    Bitmap  headerMap; // Map to find the headers during GC or profiling.
    uintptr_t largestFree; // The largest free space in the area
    PolyWord *firstFree; // The start of the first free space in the area.
};

class MemMgr
{
public:
    MemMgr();
    ~MemMgr();
    bool Initialise();

    // Create a local space for initial allocation.
    LocalMemSpace *CreateAllocationSpace(uintptr_t size);
    // Create and initialise a new local space and add it to the table.
    LocalMemSpace *NewLocalSpace(uintptr_t size, bool mut);
    // Create an entry for a permanent space from the executable.
    PermanentMemSpace *PermanentSpaceFromExecutable(PolyWord *base, uintptr_t words,
        unsigned flags, unsigned index, ModuleId sourceModule);

    // Create a permanent space but allocate memory for it.
    // Sets bottom and top to the actual memory size.
    PermanentMemSpace *AllocateNewPermanentSpace(uintptr_t byteSize, unsigned flags, unsigned index, ModuleId sourceModule);
    // Called after an allocated permanent area has been filled in.
    bool CompletePermanentSpaceAllocation(PermanentMemSpace *space);

    // Delete a local space.  Takes the iterator position in lSpaces and returns the
    // iterator after deletion.
    void DeleteLocalSpace(std::vector<LocalMemSpace*>::iterator &iter);

    // Allocate an area of the heap of at least minWords and at most maxWords.
    // This is used both when allocating single objects (when minWords and maxWords
    // are the same) and when allocating heap segments.  If there is insufficient
    // space to satisfy the minimum it will return 0.  Updates "maxWords" with
    // the space actually allocated
    PolyWord *AllocHeapSpace(uintptr_t minWords, uintptr_t &maxWords, bool doAllocation = true);
    PolyWord *AllocHeapSpace(uintptr_t words)
        { uintptr_t allocated = words; return AllocHeapSpace(words, allocated); }

    CodeSpace *NewCodeSpace(uintptr_t size);
    // Allocate space for code.  This is initially mutable to allow the code to be built.
    PolyObject *AllocCodeSpace(POLYUNSIGNED size);

    // Check that a subsequent allocation will succeed.  Called from the GC to ensure
    bool CheckForAllocation(uintptr_t words);

    // If an allocation space has a lot of data left in it, particularly a single
    // large object we should turn it into a local area.
    void ConvertAllocationSpaceToLocal(LocalMemSpace *space);

    // Allocate space for the initial stack for a thread.  The caller must
    // initialise the new stack.  Returns 0 if allocation fails.
    StackSpace *NewStackSpace(uintptr_t size);

    // Adjust the space for a stack.  Returns true if it succeeded.  If it failed
    // it leaves the stack untouched.
    bool GrowOrShrinkStack(TaskData *taskData, uintptr_t newSize);

    // Delete a stack when a thread has finished.
    bool DeleteStackSpace(StackSpace *space);

    // Create and delete export spaces
    PermanentMemSpace *NewExportSpace(uintptr_t size, bool mut, bool noOv, bool code);
    void DeleteExportSpaces(void);
    bool PromoteNewExportSpaces(ModuleId newModId); // Turn export spaces into permanent spaces.
    bool DemoteOldPermanentSpaces(ModuleId modId); // Turn any old permanent spaces with this ID into local spaces.

    PermanentMemSpace *SpaceForIndex(unsigned index, ModuleId modId); // Return the space for a given index

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

    // SpaceForAddress must NOT be applied to a PolyObject *.  That's because
    // it works nearly all the time except when a zero-sized object is placed
    // at the end of page.  Then the base address will be the start of the
    // next page.
    void SpaceForAddress(PolyObject *pt) {}

    // Use this instead.
    MemSpace* SpaceForObjectAddress(PolyObject* pt)
        { return SpaceForAddress(((PolyWord*)pt) - 1); }

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

    // LocalSpaceForAddress must NOT be applied to PolyObject*.
    // See comment on SpaceForAddress above.
    void LocalSpaceForAddress(PolyObject* pt) {}

    LocalMemSpace* LocalSpaceForObjectAddress(PolyObject* pt)
        { return LocalSpaceForAddress(((PolyWord*)pt) - 1); }

    void SetReservation(uintptr_t words) { reservedSpace = words; }

    // In several places we assume that segments are filled with valid
    // objects.  This fills unused memory with one or more "byte" objects.
    void FillUnusedSpace(PolyWord *base, uintptr_t words);

    // Return number of words of free space for stats.
    uintptr_t GetFreeAllocSpace();

    // Remove unused local areas.
    void RemoveEmptyLocals();
    // Remove unused code areas.
    void RemoveEmptyCodeAreas();

    // Remove unused allocation areas to reduce the space below the limit.
    void RemoveExcessAllocation(uintptr_t words);
    void RemoveExcessAllocation() { RemoveExcessAllocation(spaceBeforeMinorGC); }

    // Table for permanent spaces
    std::vector<PermanentMemSpace *> pSpaces;

    // Table for local spaces
    std::vector<LocalMemSpace *> lSpaces;

    // Table for export spaces
    std::vector<PermanentMemSpace *> eSpaces;

    // Table for stack spaces
    std::vector<StackSpace *> sSpaces;
    PLock stackSpaceLock;

    // Table for code spaces
    std::vector<CodeSpace *> cSpaces;
    PLock codeSpaceLock;

    // Storage manager lock.
    PLock allocLock;

    // Lock for creating new bitmaps for code profiling
    PLock codeBitmapLock;

    unsigned nextIndex; // Used when allocating new permanent spaces.

    uintptr_t SpaceBeforeMinorGC() const { return spaceBeforeMinorGC; }
    uintptr_t SpaceForHeap() const { return spaceForHeap; }
    void SetSpaceBeforeMinorGC(uintptr_t minorSize) { spaceBeforeMinorGC = minorSize; }
    void SetSpaceForHeap(uintptr_t heapSize) { spaceForHeap = heapSize; }

    uintptr_t CurrentAllocSpace() { return currentAllocSpace; }
    uintptr_t AllocatedInAlloc();
    uintptr_t CurrentHeapSize() { return currentHeapSize; }

    uintptr_t DefaultSpaceSize() const { return defaultSpaceSize; }

    void ReportHeapSizes(const char *phase);

    // Profiling - Find a code object or return zero if not found.
    PolyObject *FindCodeObject(const byte *addr);
    // Profiling - Free bitmaps to indicate start of an object.
    void RemoveProfilingBitmaps();

private:
    bool AddLocalSpace(LocalMemSpace *space);
    bool AddCodeSpace(CodeSpace *space);

    uintptr_t reservedSpace;
    unsigned nextAllocator;
    // The default size in words when creating new segments.
    uintptr_t defaultSpaceSize;
    // The number of words that can be used for initial allocation.
    uintptr_t spaceBeforeMinorGC;
    // The number of words that can be used for the heap
    uintptr_t spaceForHeap;
    // The current sizes of the allocation space and the total heap size.
    uintptr_t currentAllocSpace, currentHeapSize;
    // LocalSpaceForAddress is a hot-spot so we use a B-tree to convert addresses;
    SpaceTree *spaceTree;
    PLock spaceTreeLock;
    void AddTree(MemSpace *space) { AddTree(space, space->bottom, space->top); }
    void RemoveTree(MemSpace *space) { RemoveTree(space, space->bottom, space->top); }
    void AddTree(MemSpace *space, PolyWord *startS, PolyWord *endS);
    void RemoveTree(MemSpace *space, PolyWord *startS, PolyWord *endS);

    void AddTreeRange(SpaceTree **t, MemSpace *space, uintptr_t startS, uintptr_t endS);
    void RemoveTreeRange(SpaceTree **t, MemSpace *space, uintptr_t startS, uintptr_t endS);

#ifdef POLYML32IN64
    OSMemInRegion osHeapAlloc, osStackAlloc, osCodeAlloc;
#else
    OSMemUnrestricted osHeapAlloc, osStackAlloc;
#if (defined(HOSTARCHITECTURE_X86_64) || defined(HOSTARCHITECTURE_AARCH64))
    // On the X86/64 and the ARM64 we need to put the code in a 2GB area.
    // This allows 32-bit relative displacements for branches in X86 and
    // also ensures that if we split the constant area for PIC the offsets
    // are within the range.
    OSMemInRegion osCodeAlloc;
#else
    OSMemUnrestricted osCodeAlloc;
#endif

#endif
};

extern MemMgr gMem;

#endif
