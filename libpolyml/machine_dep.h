/*
    Title:  machine_dep.h - exports signature for machine_dep.c 

    Copyright (c) 2000
        Cambridge University Technical Services Limited

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
    MA_X86_64
} Architectures;

// Machine-dependent module.
class MachineDependent {
public:
    virtual ~MachineDependent() {} // Keep the compiler happy

    // Create the machine-specific task data object.
    virtual TaskData *CreateTaskData(void) = 0;

    virtual unsigned InitialStackSize(void) { return 128; } // Initial size of a stack 
    // Must be > 40 (i.e. 2*min_stack_check) + base area in each stack frame

    /* ScanConstantsWithinCode - update addresses within a code segment.*/
    virtual void ScanConstantsWithinCode(PolyObject *addr, PolyObject *oldAddr, POLYUNSIGNED length, ScanAddress *process) {}
    void  ScanConstantsWithinCode(PolyObject *addr, ScanAddress *process)
        { ScanConstantsWithinCode(addr, addr, addr->Length(), process); } // Common case

    virtual void FlushInstructionCache(void *p, POLYUNSIGNED bytes) {}
    virtual Architectures MachineArchitecture(void) = 0; 
};

extern MachineDependent *machineDependent;

extern struct _entrypts machineSpecificEPT[];

#endif /* _MACHINE_DEP_H */
