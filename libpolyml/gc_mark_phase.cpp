/*
    Title:      Multi-Threaded Garbage Collector - Mark phase

    Copyright (c) 2010-12, 2015-16, 2019 David C. J. Matthews

    Based on the original garbage collector code
        Copyright 2000-2008
        Cambridge University Technical Services Limited

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
/*
This is the first, mark, phase of the garbage collector.  It detects all
reachable cells in the area being collected.  At the end of the phase the
bit-maps associated with the areas will have ones for words belonging to cells
that must be retained and zeros for words that can be reused.

This is now multi-threaded.  The mark phase involves setting a bit in the header
of each live cell and then a pass over the memory building the bitmaps and clearing
this bit.  It is unfortunate that we cannot use the GC-bit that is used in
forwarding pointers but we may well have forwarded pointers left over from a
partially completed minor GC.  Using a bit in the header avoids the need for
locking since at worst it may involve two threads duplicating some marking.

The code ensures that each reachable cell is marked at least once but with
multiple threads a cell may be marked by more than once cell if the
memory is not fully up to date.  Each thread has a stack on which it
remembers cells that have been marked but not fully scanned.  If a
thread runs out of cells of its own to scan it can pick a pointer off
the stack of another thread and scan that.  The original thread will
still scan it some time later but it should find that the addresses
in it have all been marked and it can simply pop this off.  This is
all done without locking.  Stacks are only modified by the owning
thread and when they pop anything they write zero in its place.
Other threads only need to search for a zero to find if they are
at the top and if they get a pointer that has already been scanned
then this is safe.  The only assumption made about the memory is
that all the bits of a word are updated together so that a thread
will always read a value that is a valid pointer.

Many of the ideas are drawn from Flood, Detlefs, Shavit and Zhang 2001
"Parallel Garbage Collection for Shared Memory Multiprocessors".
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

#include "globals.h"
#include "processes.h"
#include "gc.h"
#include "scanaddrs.h"
#include "check_objects.h"
#include "bitmap.h"
#include "memmgr.h"
#include "diagnostics.h"
#include "gctaskfarm.h"
#include "profiling.h"
#include "heapsizing.h"

#define MARK_STACK_SIZE 3000
#define LARGECACHE_SIZE 20

class MTGCProcessMarkPointers: public ScanAddress
{
public:
    MTGCProcessMarkPointers();

    virtual void ScanRuntimeAddress(PolyObject **pt, RtsStrength weak);
    virtual PolyObject *ScanObjectAddress(PolyObject *base);

    virtual void ScanAddressesInObject(PolyObject *base, POLYUNSIGNED lengthWord);
    // Have to redefine this for some reason.
    void ScanAddressesInObject(PolyObject *base)
        { ScanAddressesInObject(base, base->LengthWord()); }

    virtual void ScanConstant(PolyObject *base, byte *addressOfConstant, ScanRelocationKind code, intptr_t displacement);
    // ScanCodeAddressAt should never be called.
    POLYUNSIGNED ScanCodeAddressAt(PolyObject **pt) { ASSERT(0); return 0; }

    static void MarkPointersTask(GCTaskId *, void *arg1, void *arg2);

    static void InitStatics(unsigned threads)
    {
        markStacks = new MTGCProcessMarkPointers[threads];
        nInUse = 0;
        nThreads = threads;
    }

    static void MarkRoots(void);
    static bool RescanForStackOverflow();

private:
    bool TestForScan(PolyWord *pt);
    void MarkAndTestForScan(PolyWord *pt);
    void Reset();

    void PushToStack(PolyObject *obj, PolyWord *currentPtr = 0)
    {
        // If we don't have all the threads running we start a new one but
        // only once we have several items on the stack.  Otherwise we
        // can end up creating a task that terminates almost immediately.
        if (nInUse >= nThreads || msp < 2 || ! ForkNew(obj))
        {
            if (msp < MARK_STACK_SIZE)
            {
                markStack[msp++] = obj;
                if (currentPtr != 0)
                {
                    locPtr++;
                    if (locPtr == LARGECACHE_SIZE) locPtr = 0;
                    largeObjectCache[locPtr].base = obj;
                    largeObjectCache[locPtr].current = currentPtr;
                }
            }
            else StackOverflow(obj);
        }
        // else the new task is processing it.
    }

    static void StackOverflow(PolyObject *obj);
    static bool ForkNew(PolyObject *obj);    

    PolyObject *markStack[MARK_STACK_SIZE];
    unsigned msp;
    bool active;

    // For the typical small cell it's easier just to rescan from the start
    // but that can be expensive for large cells.  This caches the offset for
    // large cells.
    static const POLYUNSIGNED largeObjectSize = 50;
    struct { PolyObject *base; PolyWord *current; } largeObjectCache[LARGECACHE_SIZE];
    unsigned locPtr;

    static MTGCProcessMarkPointers *markStacks;
protected:
    static unsigned nThreads, nInUse;
    static PLock stackLock;
};

// There is one mark-stack for each GC thread.  markStacks[0] is used by the
// main thread when marking the roots and rescanning after mark-stack overflow.
// Once that work is done markStacks[0] is released and is available for a
// worker thread.
MTGCProcessMarkPointers *MTGCProcessMarkPointers::markStacks;
unsigned MTGCProcessMarkPointers::nThreads, MTGCProcessMarkPointers::nInUse;
PLock MTGCProcessMarkPointers::stackLock("GC mark stack");

// It is possible to have two levels of forwarding because
// we could have a cell in the allocation area that has been moved
// to the immutable area and then shared with another cell.
inline PolyObject *FollowForwarding(PolyObject *obj)
{
    while (obj->ContainsForwardingPtr())
        obj = obj->GetForwardingPtr();
    return obj;
}

MTGCProcessMarkPointers::MTGCProcessMarkPointers(): msp(0), active(false), locPtr(0)
{
    // Clear the mark stack
    for (unsigned i = 0; i < MARK_STACK_SIZE; i++)
        markStack[i] = 0;
    // Clear the large object cache just to be sure.
    for (unsigned j = 0; j < LARGECACHE_SIZE; j++)
    {
        largeObjectCache[j].base = 0;
        largeObjectCache[j].current = 0;
    }
}

// Clear the state at the beginning of a new GC pass.
void MTGCProcessMarkPointers::Reset()
{
    locPtr = 0;
    //largeObjectCache[locPtr].base = 0;
    // Clear the cache completely just to be safe
    for (unsigned j = 0; j < LARGECACHE_SIZE; j++)
    {
        largeObjectCache[j].base = 0;
        largeObjectCache[j].current = 0;
    }

}

// Called when the stack has overflowed.  We need to include this
// in the range to be rescanned.
void MTGCProcessMarkPointers::StackOverflow(PolyObject *obj)
{
    MarkableSpace *space = (MarkableSpace*)gMem.SpaceForObjectAddress(obj);
    ASSERT(space != 0 && (space->spaceType == ST_LOCAL || space->spaceType == ST_CODE));
    PLocker lock(&space->spaceLock);
    // Have to include this in the range to rescan.
    if (space->fullGCRescanStart > ((PolyWord*)obj) - 1)
        space->fullGCRescanStart = ((PolyWord*)obj) - 1;
    POLYUNSIGNED n = obj->Length();
    if (space->fullGCRescanEnd < ((PolyWord*)obj) + n)
        space->fullGCRescanEnd = ((PolyWord*)obj) + n;
    ASSERT(obj->LengthWord() & _OBJ_GC_MARK); // Should have been marked.
    if (debugOptions & DEBUG_GC_ENHANCED)
        Log("GC: Mark: Stack overflow.  Rescan for %p\n", obj);
}

// Fork a new task.  Because we've checked nInUse without taking the lock
// we may find that we can no longer create a new task.
bool MTGCProcessMarkPointers::ForkNew(PolyObject *obj)
{
    MTGCProcessMarkPointers *marker = 0;
    {
        PLocker lock(&stackLock);
        if (nInUse == nThreads)
            return false;
        for (unsigned i = 0; i < nThreads; i++)
        {
            if (! markStacks[i].active)
            {
                marker = &markStacks[i];
                break;
            }
        }
        ASSERT(marker != 0);
        marker->active = true;
        nInUse++;
    }
    bool test = gpTaskFarm->AddWork(&MTGCProcessMarkPointers::MarkPointersTask, marker, obj);
    ASSERT(test);
    return true;
}

// Main marking task.  This is forked off initially to scan a specific object and
// anything reachable from it but once that has finished it tries to find objects
// on other stacks to scan.
void MTGCProcessMarkPointers::MarkPointersTask(GCTaskId *, void *arg1, void *arg2)
{
    MTGCProcessMarkPointers *marker = (MTGCProcessMarkPointers*)arg1;
    marker->Reset();

    marker->ScanAddressesInObject((PolyObject*)arg2);

    while (true)
    {
        // Look for a stack that has at least one item on it.
        MTGCProcessMarkPointers *steal = 0;
        for (unsigned i = 0; i < nThreads && steal == 0; i++)
        {
            if (markStacks[i].markStack[0] != 0)
                steal = &markStacks[i];
        }
        // We're finished if they're all done.
        if (steal == 0)
            break;
        // Look for items on this stack
        for (unsigned j = 0; j < MARK_STACK_SIZE; j++)
        {
            // Pick the item off the stack.
            // N.B. The owning thread may update this to zero
            // at any time.
            PolyObject *toSteal = steal->markStack[j];
            if (toSteal == 0) break; // Nothing more on the stack
            // The idea here is that the original thread pushed this
            // because there were at least two addresses it needed to
            // process.  It started down one branch but left the other.
            // Since it will have marked cells in the branch it has
            // followed this thread will start on the unprocessed
            // address(es).
            marker->ScanAddressesInObject(toSteal);
        }
    }

    PLocker lock(&stackLock);
    marker->active = false; // It's finished
    nInUse--;
    ASSERT(marker->markStack[0] == 0);
}

// Tests if this needs to be scanned.  It marks it if it has not been marked
// unless it has to be scanned.
bool MTGCProcessMarkPointers::TestForScan(PolyWord *pt)
{
    if ((*pt).IsTagged())
        return false;

    // This could contain a forwarding pointer if it points into an
    // allocation area and has been moved by the minor GC.
    // We have to be a little careful.  Another thread could also
    // be following any forwarding pointers here.  However it's safe
    // because they will update it with the same value.
    PolyObject *obj = (*pt).AsObjPtr();
    if (obj->ContainsForwardingPtr())
    {
        obj = FollowForwarding(obj);
        *pt = obj;
    }

    MemSpace *sp = gMem.SpaceForObjectAddress(obj);
    if (sp == 0 || (sp->spaceType != ST_LOCAL && sp->spaceType != ST_CODE))
        return false; // Ignore it if it points to a permanent area

    POLYUNSIGNED L = obj->LengthWord();
    if (L & _OBJ_GC_MARK)
        return false; // Already marked

    if (debugOptions & DEBUG_GC_DETAIL)
        Log("GC: Mark: %p %" POLYUFMT " %u\n", obj, OBJ_OBJECT_LENGTH(L), GetTypeBits(L));

    if (OBJ_IS_BYTE_OBJECT(L))
    {
        obj->SetLengthWord(L | _OBJ_GC_MARK); // Mark it
        return false; // We've done as much as we need
    }
    return true;
}

void MTGCProcessMarkPointers::MarkAndTestForScan(PolyWord *pt)
{
    if (TestForScan(pt))
    {
        PolyObject *obj = (*pt).AsObjPtr();
        obj->SetLengthWord(obj->LengthWord() | _OBJ_GC_MARK);
    }
}

// The initial entry to process the roots.  These may be RTS addresses or addresses in
// a thread stack.  Also called recursively to process the addresses of constants in
// code segments.  This is used in situations where a scanner may return the
// updated address of an object.
PolyObject *MTGCProcessMarkPointers::ScanObjectAddress(PolyObject *obj)
{
    MemSpace *sp = gMem.SpaceForAddress((PolyWord*)obj-1);

    if (!(sp->spaceType == ST_LOCAL || sp->spaceType == ST_CODE))
        return obj; // Ignore it if it points to a permanent area

    // We may have a forwarding pointer if this has been moved by the
    // minor GC.
    if (obj->ContainsForwardingPtr())
    {
        obj = FollowForwarding(obj);
        sp = gMem.SpaceForAddress((PolyWord*)obj - 1);
    }

    ASSERT(obj->ContainsNormalLengthWord());

    POLYUNSIGNED L = obj->LengthWord();
    if (L & _OBJ_GC_MARK)
        return obj; // Already marked
    sp->writeAble(obj)->SetLengthWord(L | _OBJ_GC_MARK); // Mark it

    if (profileMode == kProfileLiveData || (profileMode == kProfileLiveMutables && obj->IsMutable()))
        AddObjectProfile(obj);

    POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L);
    if (debugOptions & DEBUG_GC_DETAIL)
        Log("GC: Mark: %p %" POLYUFMT " %u\n", obj, n, GetTypeBits(L));

    if (OBJ_IS_BYTE_OBJECT(L))
        return obj;

    // If we already have something on the stack we must being called
    // recursively to process a constant in a code segment.  Just push
    // it on the stack and let the caller deal with it.
    if (msp != 0)
        PushToStack(obj); // Can't check this because it may have forwarding ptrs.
    else
    {
        // Normally a root but this can happen if we're following constants in code.
        // In that case we want to make sure that we don't recurse too deeply and
        // overflow the C stack.  Push the address to the stack before calling
        // ScanAddressesInObject so that if we come back here msp will be non-zero.
        // ScanAddressesInObject will empty the stack.
        PushToStack(obj);
        MTGCProcessMarkPointers::ScanAddressesInObject(obj, L);
        // We can only check after we've processed it because if we
        // have addresses left over from an incomplete partial GC they
        // may need to forwarded.
        CheckObject (obj);
    }

    return obj;
}

// These functions are only called with pointers held by the runtime system.
// Weak references can occur in the runtime system, eg. streams and windows.
// Weak references are not marked and so unreferenced streams and windows
// can be detected and closed.
void MTGCProcessMarkPointers::ScanRuntimeAddress(PolyObject **pt, RtsStrength weak)
{
    if (weak == STRENGTH_WEAK) return;
    *pt = ScanObjectAddress(*pt);
    CheckPointer (*pt); // Check it after any forwarding pointers have been followed.
}

// This is called via ScanAddressesInRegion to process the permanent mutables.  It is
// also called from ScanObjectAddress to process root addresses.
// It processes all the addresses reachable from the object.
// This is almost the same as RecursiveScan::ScanAddressesInObject. 
void MTGCProcessMarkPointers::ScanAddressesInObject(PolyObject *obj, POLYUNSIGNED lengthWord)
{
    if (OBJ_IS_BYTE_OBJECT(lengthWord))
        return;

    while (true)
    {
        ASSERT (OBJ_IS_LENGTH(lengthWord));

        POLYUNSIGNED length = OBJ_OBJECT_LENGTH(lengthWord);
        PolyWord *baseAddr = (PolyWord*)obj;
        PolyWord *endWord = baseAddr + length;

        if (OBJ_IS_WEAKREF_OBJECT(lengthWord))
        {
            // Special case.  
            ASSERT(OBJ_IS_MUTABLE_OBJECT(lengthWord)); // Should be a mutable.
            ASSERT(OBJ_IS_WORD_OBJECT(lengthWord)); // Should be a plain object.
            // We need to mark the "SOME" values in this object but we don't mark
            // the references contained within the "SOME".
            // Mark every word but ignore the result.
            for (POLYUNSIGNED i = 0; i < length; i++)
                (void)MarkAndTestForScan(baseAddr+i);
            // We've finished with this.
            endWord = baseAddr;
        }

        else if (OBJ_IS_CODE_OBJECT(lengthWord))
        {
            // Code addresses in the native code versions.
            // Closure cells are normal (word) objects and code addresses are normal addresses.
            // It's better to process the whole code object in one go.
            ScanAddress::ScanAddressesInObject(obj, lengthWord);
            endWord = baseAddr; // Finished
        }

        else if (OBJ_IS_CLOSURE_OBJECT(lengthWord))
        {
            // Closure cells in 32-in-64.
            // The first word is the absolute address of the code ...
            PolyObject *codeAddr = *(PolyObject**)obj;
            // except that it is possible we haven't yet set it.
            if (((uintptr_t)codeAddr & 1) == 0)
                ScanObjectAddress(codeAddr);
            // The rest is a normal tuple.
            baseAddr += sizeof(PolyObject*) / sizeof(PolyWord);
        }

        // If there are only two addresses in this cell that need to be
        // followed we follow them immediately and treat this cell as done.
        // If there are more than two we push the address of this cell on
        // the stack, follow the first address and then rescan it.  That way
        // list cells are processed once only but we don't overflow the
        // stack by pushing all the addresses in a very large vector.
        PolyObject *firstWord = 0;
        PolyObject *secondWord = 0;
        PolyWord *restartAddr = 0;

        if (obj == largeObjectCache[locPtr].base)
        {
            baseAddr = largeObjectCache[locPtr].current;
            ASSERT(baseAddr > (PolyWord*)obj && baseAddr < endWord);
            if (locPtr == 0) locPtr = LARGECACHE_SIZE - 1; else locPtr--;
        }

        while (baseAddr != endWord)
        {
            PolyWord wordAt = *baseAddr;

            if (wordAt.IsDataPtr() && wordAt != PolyWord::FromUnsigned(0))
            {
                // Normal address.  We can have words of all zeros at least in the
                // situation where we have a partially constructed code segment where
                // the constants at the end of the code have not yet been filled in.
                if (TestForScan(baseAddr))
                {
                    if (firstWord == 0)
                        firstWord = baseAddr->AsObjPtr();
                    else if (secondWord == 0)
                    {
                        // If we need to rescan because there are three or more words to do
                        // this is the place we need to restart (or the start of the cell if it's
                        // small).
                        restartAddr = baseAddr;
                        secondWord = baseAddr->AsObjPtr();
                    }
                    else break;  // More than two words.
                }
            }
            baseAddr++;
        }

        if (baseAddr != endWord)
            // Put this back on the stack while we process the first word
            PushToStack(obj, length < largeObjectSize ? 0 : restartAddr);
        else if (secondWord != 0)
        {
            // Mark it now because we will process it.
            PolyObject* writeAble = secondWord;
            if (secondWord->IsCodeObject())
                writeAble = gMem.SpaceForObjectAddress(secondWord)->writeAble(secondWord);
            writeAble->SetLengthWord(secondWord->LengthWord() | _OBJ_GC_MARK);
            // Put this on the stack.  If this is a list node we will be
            // pushing the tail.
            PushToStack(secondWord);
        }

        if (firstWord != 0)
        {
            // Mark it and process it immediately.
            PolyObject* writeAble = firstWord;
            if (firstWord->IsCodeObject())
                writeAble = gMem.SpaceForObjectAddress(firstWord)->writeAble(firstWord);
            writeAble->SetLengthWord(firstWord->LengthWord() | _OBJ_GC_MARK);
            obj = firstWord;
        }
        else if (msp == 0)
        {
            markStack[msp] = 0; // Really finished
            return;
        }
        else
        {
            // Clear the item above the top.  This really is finished.
            if (msp < MARK_STACK_SIZE) markStack[msp] = 0;
            // Pop the item from the stack but don't overwrite it yet.
            // This allows another thread to steal it if there really
            // is nothing else to do.  This is only really important
            // for large objects.
            obj = markStack[--msp]; // Pop something.
        }

        lengthWord = obj->LengthWord();
    }
}

// Process a constant within the code.  This is a direct copy of ScanAddress::ScanConstant
// with the addition of the locking.
void MTGCProcessMarkPointers::ScanConstant(PolyObject *base, byte *addressOfConstant, ScanRelocationKind code, intptr_t displacement)
{
    // If we have newly compiled code the constants may be in the
    // local heap.  MTGCProcessMarkPointers::ScanObjectAddress can
    // return an updated address for a local address if there is a
    // forwarding pointer.  
    // Constants can be aligned on any byte offset so another thread
    // scanning the same code could see an invalid address if it read
    // the constant while it was being updated.  We put a lock round
    // this just in case.
    MemSpace *space = gMem.SpaceForAddress(addressOfConstant);
    PLock *lock = 0;
    if (space->spaceType == ST_CODE)
        lock = &((CodeSpace*)space)->spaceLock;

    if (lock != 0)
        lock->Lock();
    PolyObject *p = GetConstantValue(addressOfConstant, code, displacement);
    if (lock != 0)
        lock->Unlock();

    if (p != 0)
    {
        PolyObject *newVal = ScanObjectAddress(p);
        if (newVal != p) // Update it if it has changed.
        {
            if (lock != 0)
                lock->Lock();
            SetConstantValue(addressOfConstant, newVal, code);
            if (lock != 0)
                lock->Unlock();
        }
    }
}

// Mark all the roots.  This is run in the main thread and has the effect
// of starting new tasks as the scanning runs.
void MTGCProcessMarkPointers::MarkRoots(void)
{
    ASSERT(nThreads >= 1);
    ASSERT(nInUse == 0);
    MTGCProcessMarkPointers *marker = &markStacks[0];
    marker->Reset();
    marker->active = true;
    nInUse = 1;

    // Scan the permanent mutable areas.
    for (std::vector<PermanentMemSpace*>::iterator i = gMem.pSpaces.begin(); i < gMem.pSpaces.end(); i++)
    {
        PermanentMemSpace *space = *i;
        if (space->isMutable && ! space->byteOnly)
            marker->ScanAddressesInRegion(space->bottom, space->top);
    }

    // Scan the RTS roots.
    GCModules(marker);

    ASSERT(marker->markStack[0] == 0);

    // When this has finished there may well be other tasks running.
    PLocker lock(&stackLock);
    marker->active = false;
    nInUse--;
}

// This class just allows us to use ScanAddress::ScanAddressesInRegion to call
// ScanAddressesInObject for each object in the region.
class Rescanner: public ScanAddress
{
public:
    Rescanner(MTGCProcessMarkPointers *marker): m_marker(marker) {}

    virtual void ScanAddressesInObject(PolyObject *obj, POLYUNSIGNED lengthWord)
    {
        // If it has previously been marked it is known to be reachable but
        // the contents may not have been scanned if the stack overflowed.
        if (lengthWord &_OBJ_GC_MARK)
            m_marker->ScanAddressesInObject(obj, lengthWord);
    }

    // Have to define this.
    virtual PolyObject *ScanObjectAddress(PolyObject *base) { ASSERT(false); return 0; }
    virtual POLYUNSIGNED ScanCodeAddressAt(PolyObject **pt) { ASSERT(false); return 0; }

    bool ScanSpace(MarkableSpace *space);
private:
    MTGCProcessMarkPointers *m_marker;
};

// Rescan any marked objects in the area between fullGCRescanStart and fullGCRescanEnd.
// N.B.  We may have threads already processing other areas and they could overflow
// their stacks and change fullGCRescanStart or fullGCRescanEnd.
bool Rescanner::ScanSpace(MarkableSpace *space)
{
    PolyWord *start, *end;
    {
        PLocker lock(&space->spaceLock);
        start = space->fullGCRescanStart;
        end = space->fullGCRescanEnd;
        space->fullGCRescanStart = space->top;
        space->fullGCRescanEnd = space->bottom;
    }
    if (start < end)
    {
        if (debugOptions & DEBUG_GC_ENHANCED)
            Log("GC: Mark: Rescanning from %p to %p\n", start, end);
        ScanAddressesInRegion(start, end);
        return true; // Require rescan
    }
    else return false;
}

// When the threads created by marking the roots have completed we need to check that
// the mark stack has not overflowed.  If it has we need to rescan.  This rescanning
// pass may result in a further overflow so if we find we have to rescan we repeat.
bool MTGCProcessMarkPointers::RescanForStackOverflow()
{
    ASSERT(nThreads >= 1);
    ASSERT(nInUse == 0);
    MTGCProcessMarkPointers *marker = &markStacks[0];
    marker->Reset();
    marker->active = true;
    nInUse = 1;
    bool rescan = false;
    Rescanner rescanner(marker);

    for (std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
    {
        if (rescanner.ScanSpace(*i))
            rescan = true;
    }
    for (std::vector<CodeSpace *>::iterator i = gMem.cSpaces.begin(); i < gMem.cSpaces.end(); i++)
    {
        if (rescanner.ScanSpace(*i))
            rescan = true;
    }
    {
        PLocker lock(&stackLock);
        nInUse--;
        marker->active = false;
    }
    return rescan;
}

static void SetBitmaps(LocalMemSpace *space, PolyWord *pt, PolyWord *top)
{
    while (pt < top)
    {
#ifdef POLYML32IN64
        if (((pt-(PolyWord*)0) & (POLYML32IN64-1)) != (POLYML32IN64-1)) // Align to length word
        {
            pt++;
            continue;
        }
#endif
        PolyObject *obj = (PolyObject*)++pt;
        // If it has been copied by a minor collection skip it
        if (obj->ContainsForwardingPtr())
        {
            obj = FollowForwarding(obj);
            ASSERT(obj->ContainsNormalLengthWord());
            pt += obj->Length();
        }
        else
        {
            POLYUNSIGNED L = obj->LengthWord();
            POLYUNSIGNED n = OBJ_OBJECT_LENGTH(L);
            if (L & _OBJ_GC_MARK)
            {
                obj->SetLengthWord(L & ~(_OBJ_GC_MARK));
                uintptr_t bitno = space->wordNo(pt);
                space->bitmap.SetBits(bitno - 1, n + 1);

                if (OBJ_IS_MUTABLE_OBJECT(L))
                    space->m_marked += n + 1;
                else
                    space->i_marked += n + 1;

                if ((PolyWord*)obj <= space->fullGCLowerLimit)
                    space->fullGCLowerLimit = (PolyWord*)obj-1;

                if (OBJ_IS_WEAKREF_OBJECT(L))
                {
                    // Add this to the limits for the containing area.
                    PolyWord *baseAddr = (PolyWord*)obj;
                    PolyWord *startAddr = baseAddr-1; // Must point AT length word.
                    PolyWord *endObject = baseAddr + n;
                    if (startAddr < space->lowestWeak) space->lowestWeak = startAddr;
                    if (endObject > space->highestWeak) space->highestWeak = endObject;
                }
            }
            pt += n;
        }
    }
}

static void CreateBitmapsTask(GCTaskId *, void *arg1, void *arg2)
{
    LocalMemSpace *lSpace = (LocalMemSpace *)arg1;
    lSpace->bitmap.ClearBits(0, lSpace->spaceSize());
    SetBitmaps(lSpace, lSpace->bottom, lSpace->top);
}

// Parallel task to check the marks on cells in the code area and
// turn them into byte areas if they are free.
static void CheckMarksOnCodeTask(GCTaskId *, void *arg1, void *arg2)
{
    CodeSpace *space = (CodeSpace*)arg1;
#ifdef POLYML32IN64
    PolyWord *pt = space->bottom + POLYML32IN64-1;
#else
    PolyWord *pt = space->bottom;
#endif
    PolyWord *lastFree = 0;
    POLYUNSIGNED lastFreeSpace = 0;
    space->largestFree = 0;
    space->firstFree = 0;
    while (pt < space->top)
    {
        PolyObject *obj = (PolyObject*)(pt+1);
        // There should not be forwarding pointers
        ASSERT(obj->ContainsNormalLengthWord());
        POLYUNSIGNED L = obj->LengthWord();
        POLYUNSIGNED length = OBJ_OBJECT_LENGTH(L);
        if (L & _OBJ_GC_MARK)
        {
            // It's marked - retain it.
            ASSERT(L & _OBJ_CODE_OBJ);
            space->writeAble(obj)->SetLengthWord(L & ~(_OBJ_GC_MARK)); // Clear the mark bit
            lastFree = 0;
            lastFreeSpace = 0;
        }
#ifdef POLYML32IN64
        else if (length == 0)
        {
            // We may have zero filler words to set the correct alignment.
            // Merge them into a previously free area otherwise leave
            // them if they're after something allocated.
            if (lastFree + lastFreeSpace == pt)
            {
                lastFreeSpace += length + 1;
                PolyObject *freeSpace = (PolyObject*)(lastFree + 1);
                space->writeAble(freeSpace)->SetLengthWord(lastFreeSpace - 1, F_BYTE_OBJ);
            }
        }
#endif
        else { // Turn it into a byte area i.e. free.  It may already be free.
            if (space->firstFree == 0) space->firstFree = pt;
            space->headerMap.ClearBit(pt-space->bottom); // Remove the "header" bit
            if (lastFree + lastFreeSpace == pt)
                // Merge free spaces.  Speeds up subsequent scans.
                lastFreeSpace += length + 1;
            else
            {
                lastFree = pt;
                lastFreeSpace = length + 1;
            }
            PolyObject *freeSpace = (PolyObject*)(lastFree+1);
            space->writeAble(freeSpace)->SetLengthWord(lastFreeSpace-1, F_BYTE_OBJ);
            if (lastFreeSpace > space->largestFree) space->largestFree = lastFreeSpace;
        }
        pt += length+1;
    }
}

void GCMarkPhase(void)
{
    mainThreadPhase = MTP_GCPHASEMARK;

    // Clear the mark counters and set the rescan limits.
    for(std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
    {
        LocalMemSpace *lSpace = *i;
        lSpace->i_marked = lSpace->m_marked = 0;
        lSpace->fullGCRescanStart = lSpace->top;
        lSpace->fullGCRescanEnd = lSpace->bottom;
    }
    for (std::vector<CodeSpace *>::iterator i = gMem.cSpaces.begin(); i < gMem.cSpaces.end(); i++)
    {
        CodeSpace *space = *i;
        space->fullGCRescanStart = space->top;
        space->fullGCRescanEnd = space->bottom;
    }
    
    MTGCProcessMarkPointers::MarkRoots();
    gpTaskFarm->WaitForCompletion();

    // Do we have to rescan because the mark stack overflowed?
    bool rescan;
    do {
        rescan = MTGCProcessMarkPointers::RescanForStackOverflow();
        gpTaskFarm->WaitForCompletion();
    } while(rescan);

    gHeapSizeParameters.RecordGCTime(HeapSizeParameters::GCTimeIntermediate, "Mark");

    // Turn the marks into bitmap entries.
    for (std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
        gpTaskFarm->AddWorkOrRunNow(&CreateBitmapsTask, *i, 0);

    // Process the code areas.
    for (std::vector<CodeSpace *>::iterator i = gMem.cSpaces.begin(); i < gMem.cSpaces.end(); i++)
        gpTaskFarm->AddWorkOrRunNow(&CheckMarksOnCodeTask, *i, 0);

    gpTaskFarm->WaitForCompletion(); // Wait for completion of the bitmaps

    gMem.RemoveEmptyCodeAreas();

    gHeapSizeParameters.RecordGCTime(HeapSizeParameters::GCTimeIntermediate, "Bitmap");

    uintptr_t totalLive = 0;
    for(std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
    {
        LocalMemSpace *lSpace = *i;
        if (! lSpace->isMutable) ASSERT(lSpace->m_marked == 0);
        totalLive += lSpace->m_marked + lSpace->i_marked;
        if (debugOptions & DEBUG_GC_ENHANCED)
            Log("GC: Mark: %s space %p: %" POLYUFMT " immutable words marked, %" POLYUFMT " mutable words marked\n",
                                lSpace->spaceTypeString(), lSpace,
                                lSpace->i_marked, lSpace->m_marked);
    }
    if (debugOptions & DEBUG_GC)
        Log("GC: Mark: Total live data %" POLYUFMT " words\n", totalLive);
}

// Set up the stacks.
void initialiseMarkerTables()
{
    unsigned threads = gpTaskFarm->ThreadCount();
    if (threads == 0) threads = 1;
    MTGCProcessMarkPointers::InitStatics(threads);
}
