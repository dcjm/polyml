/*
    Title:      Address scanner

    Copyright (c) 2006-8, 2012 David C.J. Matthews

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
#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#include <new>

#include "globals.h"
#include "scanaddrs.h"
#include "machine_dep.h"
#include "diagnostics.h"
#include "memmgr.h"

// Process the value at a given location and update it as necessary.
POLYUNSIGNED ScanAddress::ScanAddressAt(PolyWord *pt)
{
    PolyWord val = *pt;
    PolyWord newVal = val;
    if (IS_INT(val) || val == PolyWord::FromUnsigned(0))
    {
        // We can get zeros in the constant area if we garbage collect
        //  while compiling some code. */
    }
    else
    {
        ASSERT(OBJ_IS_DATAPTR(val));
        // Any sort of address
        newVal = ScanObjectAddress(val.AsObjPtr());
    }
    if (newVal != val) // Only update if we need to.
        *pt = newVal;
    return 0;
}

// General purpose object processor,  Processes all the addresses in an object.
// Handles the various kinds of object that may contain addresses.
void ScanAddress::ScanAddressesInObject(PolyObject *obj, POLYUNSIGNED lengthWord)
{
    do
    {
        ASSERT (OBJ_IS_LENGTH(lengthWord));
    
        if (OBJ_IS_BYTE_OBJECT(lengthWord))
            return; /* Nothing more to do */
    
        POLYUNSIGNED length = OBJ_OBJECT_LENGTH(lengthWord);
        PolyWord *baseAddr = (PolyWord*)obj;
    
        if (OBJ_IS_CODE_OBJECT(lengthWord))
        {
            // Scan constants within the code.
            machineDependent->ScanConstantsWithinCode(obj, obj, length, this);
        
            // Skip to the constants and get ready to scan them.
            obj->GetConstSegmentForCode(length, baseAddr, length);

        } // else it's a normal object,

        PolyWord *endWord = baseAddr + length;

        // We want to minimise the actual recursion we perform so we try to
        // use tail recursion if we can.  We first scan from the end and
        // remove any words that don't need recursion.
        POLYUNSIGNED lastLengthWord = 0;
        while (endWord != baseAddr)
        {
            PolyWord wordAt = endWord[-1];
            if (IS_INT(wordAt) || wordAt == PolyWord::FromUnsigned(0))
                endWord--; // Don't need to look at this.
            else if ((lastLengthWord = ScanAddressAt(endWord-1)) != 0)
                // We need to process this one
                break;
            else endWord--; // We're not interested in this.
        }

        if (endWord == baseAddr)
            return; // We've done everything.

        // There is at least one word that needs to be processed, the
        // one at endWord-1.
        // Now process from the beginning forward to see if there are
        // any words before this that need to be handled.  This way we are more
        // likely to handle the head of a list by recursion and the
        // tail by looping (tail recursion).
        while (baseAddr < endWord-1)
        {
            PolyWord wordAt = *baseAddr;
            if (IS_INT(wordAt) || wordAt == PolyWord::FromUnsigned(0))
                baseAddr++; // Don't need to look at this.
            else
            {
                POLYUNSIGNED lengthWord = ScanAddressAt(baseAddr);
                if (lengthWord != 0)
                {
                    wordAt = *baseAddr; // Reload because it may have been side-effected
                     // We really have to process this recursively.
                    ASSERT(wordAt.IsDataPtr());
                    ScanAddressesInObject(wordAt.AsObjPtr(), lengthWord);
                    baseAddr++;
                }
                else baseAddr++;
            }
        }

        // Finally process the last word we found that has to be processed.
        // Do this by looping rather than recursion.
        PolyWord wordAt = *baseAddr; // Last word to do.
        // This must be an address 
        ASSERT(wordAt.IsDataPtr());
        obj = wordAt.AsObjPtr();

        lengthWord = lastLengthWord;

    } while(1);
}

void ScanAddress::ScanAddressesInRegion(PolyWord *region, PolyWord *end)
{
    PolyWord *pt = region;
    while (pt < end)
    {
        pt++; // Skip length word.
        // pt actually points AT the object here.
        PolyObject *obj = (PolyObject*)pt;
        if (obj->ContainsForwardingPtr())    /* skip over moved object */
        {
            // We can now get multiple forwarding pointers as a result
            // of applying ShareData repeatedly.  Perhaps we should
            // turn the forwarding pointers back into normal words in
            // an extra pass.
            obj = obj->FollowForwardingChain();
            ASSERT(obj->ContainsNormalLengthWord());
            pt += obj->Length();
        }
        else
        {
            ASSERT(obj->ContainsNormalLengthWord());
            POLYUNSIGNED length = obj->Length();
            if (pt+length > end)
                Crash("Malformed object at %p - length %lu\n", pt, length);
            if (length != 0)
                ScanAddressesInObject(obj);
            pt += length;
        }
    }
}

// Extract a constant from the code.
PolyWord ScanAddress::GetConstantValue(byte *addressOfConstant, ScanRelocationKind code)
{
    switch (code)
    {
    case PROCESS_RELOC_DIRECT: // 32 or 64 bit address of target
        {
            POLYUNSIGNED valu;
            unsigned i;
            byte *pt = addressOfConstant;
            if (pt[3] & 0x80) valu = 0-1; else valu = 0;
            for (i = sizeof(PolyWord); i > 0; i--)
                valu = (valu << 8) | pt[i-1];

            return PolyWord::FromUnsigned(valu);
        }
    case PROCESS_RELOC_I386RELATIVE:         // 32 bit relative address
        {
            POLYSIGNED disp;
            byte *pt = addressOfConstant;
            // Get the displacement. This is signed.
            if (pt[3] & 0x80) disp = -1; else disp = 0; // Set the sign just in case.
            for(unsigned i = 4; i > 0; i--) disp = (disp << 8) | pt[i-1];

            byte *absAddr = pt + disp + 4; // The address is relative to AFTER the constant

            return PolyWord::FromCodePtr(absAddr);
        }
    default:
        ASSERT(false);
        return TAGGED(0);
    }
}

// Store a constant value.  Also used with a patch table when importing a saved heap which has
// been exported using the C exporter.
void ScanAddress::SetConstantValue(byte *addressOfConstant, PolyWord p, ScanRelocationKind code)
{

    switch (code)
    {
    case PROCESS_RELOC_DIRECT: // 32 or 64 bit address of target
        {
            POLYUNSIGNED valu = p.AsUnsigned();
            for (unsigned i = 0; i < sizeof(PolyWord); i++)
            {
                addressOfConstant[i] = (byte)(valu & 255); 
                valu >>= 8;
            }
        }
        break;
    case PROCESS_RELOC_I386RELATIVE:         // 32 bit relative address
        {
            POLYSIGNED newDisp = p.AsCodePtr() - addressOfConstant - 4;
#if (SIZEOF_VOIDP != 4)
            ASSERT(newDisp < 0x80000000 && newDisp >= -(POLYSIGNED)0x80000000);
#endif
            for (unsigned i = 0; i < 4; i++) {
                addressOfConstant[i] = (byte)(newDisp & 0xff);
                newDisp >>= 8;
            }
        }
        break;
    }
}

// The default action is to call the DEFAULT ScanAddressAt NOT the virtual which means that it calls
// ScanObjectAddress for the base address of the object referred to.
void ScanAddress::ScanConstant(PolyObject *base, byte *addressOfConstant, ScanRelocationKind code)
{
    PolyWord p = GetConstantValue(addressOfConstant, code);

    if (! IS_INT(p))
    {
        PolyWord oldValue = p;
        ScanAddress::ScanAddressAt(&p);
        if (p != oldValue) // Update it if it has changed.
            SetConstantValue(addressOfConstant, p, code);
    }
}

void ScanAddress::ScanRuntimeWord(PolyWord *w)
{
    if (w->IsTagged()) {} // Don't need to do anything
    else {
        ASSERT(w->IsDataPtr());
        *w = ScanObjectAddress(w->AsObjPtr()); 
    }
}

// This gets called in two circumstances.  It may be called for the roots
// in which case the stack will be empty and we want to process it completely
// or it is called for a constant address in which case it will have been
// called from RecursiveScan::ScanAddressesInObject and that can process
// any addresses.
PolyObject *RecursiveScan::ScanObjectAddress(PolyObject *obj)
{
    PolyWord pWord = obj;
    // Test to see if this needs to be scanned.
    // It may update the word.
    bool test = TestForScan(&pWord);
    obj = pWord.AsObjPtr();

    if (test)
    {
        MarkAsScanning(obj);
        if (obj->IsByteObject())
            Completed(obj); // Don't need to put it on the stack
        // If we already have something on the stack we must being called
        // recursively to process a constant in a code segment.  Just push
        // it on the stack and let the caller deal with it.
        else if (StackIsEmpty())
            RecursiveScan::ScanAddressesInObject(obj, obj->LengthWord());
        else
            PushToStack(obj, (PolyWord*)obj);
    }

    return obj;
}

// This is called via ScanAddressesInRegion to process the permanent mutables.  It is
// also called from ScanObjectAddress to process root addresses.
// It processes all the addresses reachable from the object.
void RecursiveScan::ScanAddressesInObject(PolyObject *obj, POLYUNSIGNED lengthWord)
{
    if (OBJ_IS_BYTE_OBJECT(lengthWord))
        return; // Ignore byte cells and don't call Completed on them

    PolyWord *baseAddr = (PolyWord*)obj;

    while (true)
    {
        ASSERT (OBJ_IS_LENGTH(lengthWord));

        // Get the length and base address.  N.B.  If this is a code segment
        // these will be side-effected by GetConstSegmentForCode.
        POLYUNSIGNED length = OBJ_OBJECT_LENGTH(lengthWord);

        if (OBJ_IS_CODE_OBJECT(lengthWord))
        {
            // It's better to process the whole code object in one go.
            ScanAddress::ScanAddressesInObject(obj, lengthWord);
            length = 0; // Finished
        }

        // else it's a normal object,

        // If there are only two addresses in this cell that need to be
        // followed we follow them immediately and treat this cell as done.
        // If there are more than two we push the address of this cell on
        // the stack, follow the first address and then rescan it.  That way
        // list cells are processed once only but we don't overflow the
        // stack by pushing all the addresses in a very large vector.
        PolyWord *endWord = (PolyWord*)obj + length;
        PolyObject *firstWord = 0;
        PolyObject *secondWord = 0;
        PolyWord *restartFrom = baseAddr;

        while (baseAddr != endWord)
        {
            PolyWord wordAt = *baseAddr;

            if (wordAt.IsDataPtr() && wordAt != PolyWord::FromUnsigned(0))
            {
                // Normal address.  We can have words of all zeros at least in the
                // situation where we have a partially constructed code segment where
                // the constants at the end of the code have not yet been filled in.
                if (TestForScan(baseAddr)) // Test value at baseAddr (may side-effect it)
                {
                    PolyObject *wObj = (*baseAddr).AsObjPtr();
                    if (wObj->IsByteObject())
                    {
                        // Can do this now - don't need to push it
                        MarkAsScanning(wObj);
                        Completed(wObj);
                    }
                    else if (firstWord == 0)
                    {
                        firstWord = wObj;
                        // We mark the word immediately.  We can have
                        // two words in an object that are the same
                        // and we don't want to process it again.
                        MarkAsScanning(firstWord);
                    }
                    else if (secondWord == 0)
                    {
                        secondWord = wObj;
                        restartFrom = baseAddr;
                    }
                    else break;  // More than two words.
                }
            }
            baseAddr++;
        }

        if (baseAddr == endWord)
        {
            // We have done everything except possibly firstWord and secondWord.
            Completed(obj);
            if (secondWord != 0)
            {
                MarkAsScanning(secondWord);
                // Put this on the stack.  If this is a list node we will be
                // pushing the tail.
                PushToStack(secondWord, (PolyWord*)secondWord);
            }
        }
        else // Put this back on the stack while we process the first word
            PushToStack(obj, restartFrom);

        if (firstWord != 0)
        {
            // Process it immediately.
            obj = firstWord;
            baseAddr = (PolyWord*)obj;
        }
        else if (StackIsEmpty())
            return;
        else
            PopFromStack(obj, baseAddr);

        lengthWord = obj->LengthWord();
    }
}

// The stack is allocated as a series of blocks chained together.
#define RSTACK_SEGMENT_SIZE 1000

class RScanStack {
public:
    RScanStack(): nextStack(0), lastStack(0), sp(0) {}
    ~RScanStack() { delete(nextStack); }

    RScanStack *nextStack;
    RScanStack *lastStack;
    unsigned sp;
    struct { PolyObject *obj; PolyWord *base; } stack[RSTACK_SEGMENT_SIZE];
};

RecursiveScanWithStack::~RecursiveScanWithStack()
{
    delete(stack);
}

bool RecursiveScanWithStack::StackIsEmpty(void)
{
    return stack == 0 || (stack->sp == 0 && stack->lastStack == 0);
}

void RecursiveScanWithStack::PushToStack(PolyObject *obj, PolyWord *base)
{
    if (stack == 0 || stack->sp == RSTACK_SEGMENT_SIZE)
    {
        if (stack != 0 && stack->nextStack != 0)
            stack = stack->nextStack;
        else
        {
            // Need a new segment
            try {
                RScanStack *s = new RScanStack;
                s->lastStack = stack;
                if (stack != 0)
                    stack->nextStack = s;
                stack = s;
            }
            catch (std::bad_alloc &) {
                StackOverflow();
                return;
            }
        }
    }
    stack->stack[stack->sp].obj = obj;
    stack->stack[stack->sp].base = base;
    stack->sp++;
}

void RecursiveScanWithStack::PopFromStack(PolyObject *&obj, PolyWord *&base)
{
    if (stack->sp == 0)
    {
        // Chain to the previous stack if any
        ASSERT(stack->lastStack != 0);
        // Before we do, delete any further one to free some memory
        delete(stack->nextStack);
        stack->nextStack = 0;
        stack = stack->lastStack;
        ASSERT(stack->sp == RSTACK_SEGMENT_SIZE);
    }
    --stack->sp;
    obj = stack->stack[stack->sp].obj;
    base = stack->stack[stack->sp].base;
}
