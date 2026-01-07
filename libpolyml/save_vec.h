/*
    Title:  save_vec.h - The save vector holds temporary values that may move as
    the result of a garbage collection.

    Copyright (c) 2006, 2010 David C.J. Matthews

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

#ifndef SAVE_VEC_H_DEFINED
#define SAVE_VEC_H_DEFINED

#include "globals.h" // For PolyWord 

/* A handle is the address of an element of save_vec  */
/* This element points at an element of the Poly heap */
/* The element is currently represented as a (PolyWord *) */

class SaveVecEntry {
public:
    SaveVecEntry(PolyWord w): m_Handle(w) {}
    SaveVecEntry(): m_Handle(PolyWord::FromUnsigned(0)) {} // Just used when initialising the vec

    PolyWord Word() { return m_Handle; }
    PolyObject *WordP() { return m_Handle.AsObjPtr(); }

private:
    PolyWord m_Handle;

    friend class SaveVec;
};

typedef SaveVecEntry *Handle;

#define DEREFWORD(_x)            ((_x)->Word())
#define DEREFHANDLE(_x)          ((_x)->WordP())
#define DEREFWORDHANDLE(_x)      ((_x)->WordP())


#define DEREFBYTEHANDLE(_x)      ((byte *)DEREFHANDLE(_x))
#define DEREFLISTHANDLE(_x)      ((ML_Cons_Cell *)DEREFHANDLE(_x))

class ScanAddress;

class SaveVec
{
public:
    SaveVec();
    ~SaveVec();

    // Clear the save vec at the start of an RTS call
    void init(void) { save_vec_addr = save_vec; }

    // Add a word to the save vec
    Handle push(PolyWord valu);

    // Overloading for common RTS case.
    Handle push(POLYUNSIGNED valu) {
        return push(PolyWord::FromUnsigned(valu));
    }

    // Mark a position
    Handle mark(void) { return save_vec_addr; }

    // Reset to the mark
    void reset(Handle mark);

    bool isValidHandle(Handle h) { return h >= save_vec && h < save_vec_addr; } // Check it is in the range.

    // Called by the garbage collector to scan and then update the addresses in the
    // vector.
    void gcScan(ScanAddress *process);
    
private:
    SaveVecEntry *save_vec;
    SaveVecEntry *save_vec_addr;
};

#endif

