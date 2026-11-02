/*
    Architecture independent wrapper for the byte-code interpreter.

    Copyright David C.J. Matthews 2020-21.

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

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "globals.h"
#include "machine_dep.h"
#include "sys.h"
#include "profiling.h"
#include "arb.h"
#include "processes.h"
#include "run_time.h"
#include "gc.h"
#include "diagnostics.h"
#include "polystring.h"
#include "save_vec.h"
#include "memmgr.h"
#include "scanaddrs.h"
#include "rtsentry.h"
#include "bytecode.h"

/* the amount of ML stack space to reserve for registers,
   C exception handling etc. The compiler requires us to
   reserve 2 stack-frames worth (2 * 20 words) plus whatever
   we require for the register save area. We actually reserve
   slightly more than this. SPF 3/3/97
*/
#define OVERFLOW_STACK_SIZE 50

class IntTaskData: public TaskData, ByteCodeInterpreter {
public:
    IntTaskData() : ByteCodeInterpreter(&taskSp, &sl) {}
    ~IntTaskData() {}

    virtual void GarbageCollect(ScanAddress *process);
    void ScanStackAddress(ScanAddress *process, stackItem& val, StackSpace *stack);
    virtual void EnterPolyCode(); // Start running ML

    virtual void SetException(poly_exn *exc) { exception_arg = exc;  }
    virtual void InterruptCode();

    // AddTimeProfileCount is used in time profiling.
    virtual bool AddTimeProfileCount(SIGNALCONTEXT *context);

    virtual void InitStackFrame(TaskData *newTask, Handle proc);

    virtual bool AtomicallyReleaseMutex(PolyObject* mutexp) {
        return InterpreterReleaseMutex(mutexp);
    }

    // Return the minimum space occupied by the stack.   Used when setting a limit.
    virtual uintptr_t currentStackSpace(void) const { return ((stackItem*)this->stack->top - this->taskSp) + OVERFLOW_STACK_SIZE; }

    virtual void addProfileCount(POLYUNSIGNED words) { addSynchronousCount(interpreterPc, words); }

    virtual void CopyStackFrame(StackObject *old_stack, uintptr_t old_length, StackObject *new_stack, uintptr_t new_length);

    PLock interruptLock;

    virtual void ClearExceptionPacket() { exception_arg = TAGGED(0); }
    virtual PolyWord GetExceptionPacket() { return exception_arg; }
    virtual stackItem* GetHandlerRegister() { return hr; }
    virtual void SetHandlerRegister(stackItem* newHr) { hr = newHr;  }
    virtual void HandleStackOverflow(uintptr_t space);

    stackItem       *taskSp; /* Stack pointer. */
    stackItem       *hr;
    PolyWord        exception_arg;
    stackItem       *sl; /* Stack limit register. */
};

class Interpreter : public MachineDependent {
public:
    Interpreter() {}

    // Create a task data object.
    virtual TaskData *CreateTaskData(void) { return new IntTaskData(); }
    virtual Architectures MachineArchitecture(void) { return MA_Interpreted; }
};

void IntTaskData::InitStackFrame(TaskData *parentTask, Handle proc)
/* Initialise stack frame. */
{
    StackSpace *space = this->stack;
    StackObject *stack = (StackObject *)space->stack();
    PolyObject *closure = DEREFWORDHANDLE(proc);
    uintptr_t stack_size = space->spaceSize() * sizeof(PolyWord) / sizeof(stackItem);
    this->taskSp = (stackItem*)stack + stack_size;
    this->hr = this->taskSp;
    *(--this->taskSp) = (PolyWord)closure; /* Closure address */
    this->interpreterPc = *(POLYCODEPTR*)closure;
    this->exception_arg = TAGGED(0); /* Used for exception argument. */
}

void IntTaskData::HandleStackOverflow(uintptr_t space)
{
    uintptr_t min_size = (this->stack->top - (PolyWord*)taskSp) + OVERFLOW_STACK_SIZE + space;
    try {
        // The stack check has failed.  This may either be because we really have
        // overflowed the stack or because the stack limit value has been adjusted
        // to result in a call here.
        CheckAndGrowStack(this, min_size);
    }
    catch (IOException&) {
        // We may get an exception while handling this if we run out of store
    }
    {
        PLocker l(&interruptLock);
        // Set the stack limit.  This clears any interrupt and also sets the
        // correct value if we've grown the stack.
        sl = (stackItem*)stack->bottom + OVERFLOW_STACK_SIZE;
    }

    try {
        processes->ProcessAsynchRequests(this);
        // Release and re-acquire use of the ML memory to allow another thread
        // to GC.
        processes->ThreadReleaseMLMemory(this);
        processes->ThreadUseMLMemory(this);
    }
    catch (IOException&) {
    }

}

void IntTaskData::GarbageCollect(ScanAddress *process)
{
    TaskData::GarbageCollect(process);
    ByteCodeInterpreter::GarbageCollect(process);

    if (exception_arg.IsDataPtr())
    {
        PolyObject* obj = exception_arg.AsObjPtr();
        obj = process->ScanObjectAddress(obj);
        exception_arg = obj;
    }

    if (stack != 0)
    {
        stackItem*stackPtr = this->taskSp;
        // Now the values on the stack.
        for (stackItem* q = stackPtr; q < (stackItem*)stack->top; q++)
            ScanStackAddress(process, *q, stack);
    }
}

// Process a value within the stack.
void IntTaskData::ScanStackAddress(ScanAddress *process, stackItem& stackItem, StackSpace *stack)
{
    // We may have return addresses on the stack which could look like
// tagged values.  Check whether the value is in the code area before
// checking whether it is untagged.
#ifdef POLYML32IN64
    // In 32-in-64 return addresses always have the top 32 bits non-zero. 
    if (stackItem.argValue < ((uintptr_t)1 << 32))
    {
        // It's either a tagged integer or an object pointer.
        if (stackItem.w().IsDataPtr())
        {
            PolyWord val = process->ScanObjectAddress(stackItem.w().AsObjPtr());
            stackItem = val;
        }
    }
    else
    {
        // Could be a code address or a stack address.
        MemSpace* space = gMem.SpaceForAddress(stackItem.codeAddr - 1);
        if (space == 0 || space->spaceType != ST_CODE) return;
        PolyObject* obj = gMem.FindCodeObject(stackItem.codeAddr);
        ASSERT(obj != 0);
        // Process the address of the start.  Don't update anything.
        process->ScanObjectAddress(obj);
    }
#else
    // The -1 here is because we may have a zero-sized cell in the last
    // word of a space.
    MemSpace* space = gMem.SpaceForAddress(stackItem.codeAddr - 1);
    if (space == 0) return; // In particular we may have one of the assembly code addresses.
    if (space->spaceType == ST_CODE)
    {
        PolyObject* obj = gMem.FindCodeObject(stackItem.codeAddr);
        // If it is actually an integer it might be outside a valid code object.
        if (obj == 0)
        {
            ASSERT(stackItem.w().IsTagged()); // It must be an integer
        }
        else // Process the address of the start.  Don't update anything.
            process->ScanObjectAddress(obj);
    }
    else if (space->spaceType == ST_LOCAL && stackItem.w().IsDataPtr())
        // Local values must be word addresses.
    {
        PolyWord val = process->ScanObjectAddress(stackItem.w().AsObjPtr());
        stackItem = val;
    }
#endif

}

// Copy a stack
void IntTaskData::CopyStackFrame(StackObject *old_stack, uintptr_t old_length, StackObject *new_stack, uintptr_t new_length)
{
    // The lengths are the number of PolyWords but a stack item is 8 bytes in 32-in-64.
    old_length = old_length / (sizeof(stackItem)/sizeof(PolyWord));
    new_length = new_length / (sizeof(stackItem)/sizeof(PolyWord));
    /* Moves a stack, updating all references within the stack */
    stackItem*old_base = (stackItem*)old_stack;
    stackItem*new_base = (stackItem*)new_stack;
    stackItem*old_top = old_base + old_length;

    /* Calculate the offset of the new stack from the old. If the frame is
    being extended objects in the new frame will be further up the stack
    than in the old one. */

    uintptr_t offset = new_base - old_base + new_length - old_length;
    stackItem *oldSp = this->taskSp;
    this->taskSp = oldSp + offset;
    this->hr = this->hr + offset;

    /* Skip the unused part of the stack. */

    uintptr_t i = oldSp - old_base;

    ASSERT(i <= old_length);

    i = old_length - i;

    stackItem *old = oldSp;
    stackItem *newp = this->taskSp;

    while (i--)
    {
        stackItem old_word = *old++;
        if (old_word.w().IsDataPtr() && old_word.stackAddr >= old_base && old_word.stackAddr <= old_top)
            old_word.stackAddr = old_word.stackAddr + offset;
        else if (old_word.w().IsDataPtr() && IsHeapAddress(old_word.stackAddr))
        {
            stackItem* addr = (stackItem*)old_word.w().AsStackAddr();
            if (addr >= old_base && addr <= old_top)
            {
                addr += offset;
                old_word = PolyWord::FromStackAddr((PolyWord*)addr);
            }
        }
        *newp++ = old_word;
    }
    ASSERT(old == ((stackItem*)old_stack) + old_length);
    ASSERT(newp == ((stackItem*)new_stack) + new_length);
}

void IntTaskData::EnterPolyCode()
/* Called from "main" to enter the code. */
{
    sl = (stackItem*)((PolyWord*)this->stack->stack() + OVERFLOW_STACK_SIZE);
    // Run the interpreter.  Currently this can never return.
    // Callbacks are not currently implemented which is the only case that could
    // occur in the pure interpreted version.
    (void)RunInterpreter(this);
    exitThread(this); // This thread is exiting.
    ASSERT(0); // We should never get here.
}

// This is called from a different thread so we have to be careful.
void IntTaskData::InterruptCode()
{
    PLocker l(&interruptLock);
    // Set the stack limit pointer to the top of the stack to cause
    // a trap when we next check for stack overflow.
    // We use a lock here to ensure that we always use the current value of the
    // stack.  The thread we're interrupting could be growing the stack at this point.
    if (stack != 0)
        sl = (stackItem*)(stack->top - 1);
}

bool IntTaskData::AddTimeProfileCount(SIGNALCONTEXT *context)
{
    if (interpreterPc != 0)
    {
        // See if the PC we've got is an ML code address.
        MemSpace *space = gMem.SpaceForAddress(interpreterPc);
        if (space != 0 && (space->spaceType == ST_CODE || space->spaceType == ST_PERMANENT))
        {
            incrementCountAsynch(interpreterPc);
            return true;
        }
    }
    return false;
}

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyInterpretedEnterIntMode();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyEndBootstrapMode(POLYUNSIGNED threadId, POLYUNSIGNED function);
}

// Do we require EnterInt instructions and if so for which architecture?
// 0 = > None; 1 => X86_32, 2 => X86_64. 3 => X86_32_in_64.
POLYUNSIGNED PolyInterpretedEnterIntMode()
{
    return TAGGED(0).AsUnsigned();
}

// This has no effect in the interpreted version.
POLYUNSIGNED PolyEndBootstrapMode(POLYUNSIGNED threadId, POLYUNSIGNED function)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle pushedFunction = taskData->saveVec.push(function);
    taskData->InitStackFrame(taskData, pushedFunction);
    taskData->EnterPolyCode();
    // Should never return.
    ASSERT(0);
    return TAGGED(0).AsUnsigned();
}

static Interpreter interpreterObject;

MachineDependent *machineDependent = &interpreterObject;

// No machine-specific calls in the interpreter.
struct _entrypts machineSpecificEPT[] =
{
    { "PolyInterpretedEnterIntMode",    (polyRTSFunction)&PolyInterpretedEnterIntMode },
    { "PolyEndBootstrapMode",           (polyRTSFunction)&PolyEndBootstrapMode },
    { NULL, NULL} // End of list.
};
