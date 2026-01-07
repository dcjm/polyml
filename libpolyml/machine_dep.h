/*
    Title:  machine_dep.h - exports signature for machine_dep.c 

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further development Copyright 2020-21 David C. J. Matthews

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

#ifndef _MACHINE_DEP_H
#define _MACHINE_DEP_H

class ScanAddress;
class TaskData;
class SaveVecEntry;
typedef SaveVecEntry *Handle;
class StackSpace;

// Machine architecture values.
typedef enum {
    MA_Interpreted = 0,
    MA_I386,
    MA_X86_64,
    MA_X86_64_32,
    MA_Arm64,
    MA_Arm64_32
} Architectures;

// Machine-dependent module.
class MachineDependent {
public:
    virtual ~MachineDependent() {} // Keep the compiler happy

    // Create the machine-specific task data object.
    virtual TaskData *CreateTaskData(void) = 0;

    virtual unsigned InitialStackSize(void) { return 128; } // Initial size of a stack 
    // Must be > 40 (i.e. 2*min_stack_check) + base area in each stack frame

    // Find the start of the constant section for a piece of code.
    // This is the default version which uses the whole of the last word as a
    // byte offset.
    // Normally the constant area is located within the code object and the offset is a small
    // negative value.  When creating position-independent code we need to put the constants in a
    // separate area.  We have to use a relative offset to the constants rather than an absolute
    // address to ensure that the code is position-independent.
    virtual void GetConstSegmentForCode(PolyObject *obj, POLYUNSIGNED obj_length, PolyWord*& cp, POLYUNSIGNED& count) const
    {
        PolyWord* last_word = obj->Offset(obj_length - 1); // Last word in the code
        POLYSIGNED offset = last_word->AsSigned();
        cp = last_word + 1 + offset / sizeof(PolyWord);
        count = cp[-1].AsUnsigned();
    }
    void GetConstSegmentForCode(PolyObject* obj, PolyWord*& cp, POLYUNSIGNED& count) const
    {
        GetConstSegmentForCode(obj, obj->Length(), cp, count);
    }
    PolyWord* ConstPtrForCode(PolyObject* obj) const
    {
        PolyWord* cp; POLYUNSIGNED count;
        GetConstSegmentForCode(obj, cp, count);
        return cp;
    }

    /* ScanConstantsWithinCode - update addresses within a code segment.*/
    virtual void ScanConstantsWithinCode(PolyObject* addr, PolyObject* old, POLYUNSIGNED length,
        PolyWord* newConstAddr, PolyWord* oldConstAddr, POLYUNSIGNED numConsts, ScanAddress* process) {}

    void ScanConstantsWithinCode(PolyObject* addr, POLYUNSIGNED length, ScanAddress* process)
    {
        PolyWord* constAddr;
        POLYUNSIGNED count;
        GetConstSegmentForCode(addr, length, constAddr, count);
        ScanConstantsWithinCode(addr, addr, length, constAddr, constAddr, count, process);
    }

    void ScanConstantsWithinCode(PolyObject* addr, ScanAddress* process)
        { ScanConstantsWithinCode(addr, addr->Length(), process); } // Common case

    // Called to produce relocations.
    virtual void RelocateConstantsWithinCode(PolyObject* addr, ScanAddress* process)
        { ScanConstantsWithinCode(addr, process);  }

    // Set the address of the constant area.  The default is a relative byte offset.
    virtual void SetAddressOfConstants(PolyObject* objAddr, PolyObject* writable, POLYUNSIGNED length, PolyWord* constAddr)
    {
        POLYSIGNED offset = (POLYSIGNED)((constAddr - (PolyWord*)objAddr - length) * sizeof(PolyWord));
        writable->Set(length - 1, PolyWord::FromSigned(offset));
    }

    virtual void FlushInstructionCache(void *p, POLYUNSIGNED bytes) {}
    virtual Architectures MachineArchitecture(void) = 0;

    virtual void SetBootArchitecture(char arch, unsigned wordLength) {}

    // Update the global heap base in ARM64 32-in-64 FFI callbacks.
    // This is a very special case.
    virtual void UpdateGlobalHeapReference(PolyObject* addr) {}
};

extern MachineDependent *machineDependent;

extern struct _entrypts machineSpecificEPT[];

#endif /* _MACHINE_DEP_H */
