/*
    Title:      Lightweight process library
    Author:     David C.J. Matthews

    Copyright (c) 2007-8, 2012, 2015, 2017, 2019-21 David C.J. Matthews

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

#ifndef _PROCESSES_H_
#define _PROCESSES_H_

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#include "globals.h"
#include "rts_module.h"
#include "save_vec.h"

#include "noreturn.h"
#include "locking.h"

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class StackSpace;
class PolyWord;
class ScanAddress;
class MDTaskData;
class Exporter;
class StackObject;

#ifdef HAVE_WINDOWS_H
typedef void *HANDLE;
#endif

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#ifdef HAVE_UCONTEXT_H
#include <ucontext.h>
#endif

#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif

// SIGNALCONTEXT is the argument type that is passed to GetPCandSPFromContext
// to get the actual PC and SP in a profiling trap.
#if defined(HAVE_WINDOWS_H)
// First because it's used in both native Windows and Cygwin.
#include <windows.h>
#define SIGNALCONTEXT CONTEXT // This is the thread context.
#elif defined(HAVE_UCONTEXT_T)
#define SIGNALCONTEXT ucontext_t
#elif defined(HAVE_STRUCT_SIGCONTEXT)
#define SIGNALCONTEXT struct sigcontext
#else
#define SIGNALCONTEXT void
#endif

#define MIN_HEAP_SIZE   4096 // Minimum and initial heap segment size (words)

// This is the ML "thread identifier" object.  The fields
// are read and set by the ML code.
class ThreadObject: public PolyObject {
public:
    PolyWord    threadRef;  // Weak ref containing the address of the thread data. Not used by ML
    PolyWord    flags;  // Tagged integer containing flags indicating how interrupts
                        // are handled.  Set by ML but only by the thread itself
    PolyWord    threadLocal; // Head of a list of thread-local store items.
                        // Handled entirely by ML but only by the thread.
    PolyWord    requestCopy; // A tagged integer copy of the "requests" field.
                        // This is provided so that ML can easily test if there
                        // is an interrupt pending.
    PolyWord    mlStackSize; // A tagged integer with the maximum ML stack size in bytes
    PolyWord    debuggerSlots[4]; // These are used by the debugger.
};

// Other threads may make requests to a thread.
typedef enum {
    kRequestNone = 0, // Increasing severity
    kRequestInterrupt = 1,
    kRequestKill = 2
} ThreadRequests;

// Per-thread data.  This is subclassed for each architecture.
class TaskData {
public:
    TaskData();
    virtual ~TaskData();

    void FillUnusedSpace(void);
    virtual void GarbageCollect(ScanAddress *process);

    virtual void EnterPolyCode() = 0; // Start running ML

    virtual void InterruptCode() = 0;
    virtual bool AddTimeProfileCount(SIGNALCONTEXT *context) = 0;
    // Initialise the stack for a new thread.  The parent task object is passed in because any
    // allocation that needs to be made must be made in the parent.
    virtual void InitStackFrame(TaskData *parentTask, Handle proc) = 0;
    virtual void SetException(poly_exn *exc) = 0;

    // Atomically release a mutex, returning false if the mutex was previously
    // locked by more than one thread and we need to check for waiting threads.
    // This is used in waiting for a condition variable.  It is important that
    // the same atomic operations are used here as the code-generator uses,
    virtual bool AtomicallyReleaseMutex(PolyObject *mutexp) = 0;

    virtual void CopyStackFrame(StackObject *old_stack, uintptr_t old_length,
                                StackObject *new_stack, uintptr_t new_length) = 0;


    virtual uintptr_t currentStackSpace(void) const = 0;
    // Add a count to the local function if we are using store profiling.
    virtual void addProfileCount(POLYUNSIGNED words) = 0;

    // Functions called before and after an RTS call.
    virtual void PreRTSCall(void) { saveVec.init(); }
    virtual void PostRTSCall(void) {}

    SaveVec     saveVec;
    PolyWord    *allocPointer;  // Allocation pointer - decremented towards...
    PolyWord    *allocLimit;    // ... lower limit of allocation
    uintptr_t   allocSize;     // The preferred heap segment size
    unsigned    allocCount;     // The number of allocations since the last GC
    StackSpace  *stack;
    ThreadObject *threadObject;  // Pointer to the thread object.
    int         lastError;      // Last error from foreign code.
    void        *signalStack;  // Stack to handle interrupts (Unix only)

    // Get a TaskData pointer given the ML taskId.
    // This is called at the start of every RTS function that may allocate memory.
    // It is can be called safely to get the thread's own TaskData object without
    // a lock but any call to get the TaskData for another thread must take the
    // schedLock first in case the thread is exiting.
    static TaskData *FindTaskForId(PolyWord taskId) {
        return *(TaskData**)(((ThreadObject*)taskId.AsObjPtr())->threadRef.AsObjPtr());
    }

    // Overloading for usual RTS call case.
    static TaskData* FindTaskForId(POLYUNSIGNED taskId) {
        return FindTaskForId(PolyWord::FromUnsigned(taskId));
    }

private:
    // If a thread has to block it will block on this.
    PCondVar threadLock;
    // External requests made are stored here until they
    // can be actioned.
    ThreadRequests requests;
    // Pointer to the mutex when blocked. Set to NULL when it doesn't apply.
    PolyObject *blockMutex;
    // This is set to false when a thread blocks or enters foreign code,
    // While it is true the thread can manipulate ML memory so no other
    // thread can garbage collect.
    bool inMLHeap;

    // In Linux, at least, we need to run a separate timer in each thread
    bool runningProfileTimer;

#ifdef HAVE_WINDOWS_H
    LONGLONG lastCPUTime; // Used for profiling
#endif
public:
    bool threadExited;
private:
#ifdef HAVE_PTHREAD_H
    pthread_t threadId;
#endif
#ifdef HAVE_WINDOWS_H
public: // Because, on Cygwin, it's used in NewThreadFunction
    HANDLE threadHandle;
private:
#endif

    friend class Processes;
};

NORETURNFN(extern Handle exitThread(TaskData *mdTaskData));

class ScanAddress;

// Indicate what the main thread is doing if the profile
// timer goes off.
extern enum _mainThreadPhase {
    MTP_USER_CODE=0,
    MTP_GCPHASESHARING,
    MTP_GCPHASEMARK,
    MTP_GCPHASECOMPACT,
    MTP_GCPHASEUPDATE,
    MTP_GCQUICK,
    MTP_SHARING,
    MTP_EXPORTING,
    MTP_SAVESTATE,
    MTP_LOADSTATE,
    MTP_PROFILING,
    MTP_SIGHANDLER,
    MTP_CYGWINSPAWN,
    MTP_STOREMODULE,
    MTP_LOADMODULE,
    MTP_RELEASEMODULE,
    MTP_MAXENTRY
} mainThreadPhase;

// Data structure used for requests from a thread to the root
// thread.  These are GCs or similar.
class MainThreadRequest
{
public:
    MainThreadRequest (enum _mainThreadPhase phase): mtp(phase), completed(false) {}
    virtual ~MainThreadRequest () {} // Suppress silly GCC warning
    const enum _mainThreadPhase mtp;
    bool completed;
    virtual void Perform() = 0;
};

class PLock;

// Class to wait for a given time or for an event, whichever comes first.
//
// A pointer to this class or a subclass is passed to ThreadPauseForIO.
// Because a thread may be interrupted or killed by another ML thread we
// don't allow any thread to block indefinitely.  Instead whenever a
// thread wants to do an operation that may block we have it enter a
// loop that polls for the desired condition and if it is not ready it
// calls ThreadPauseForIO.  The default action is to block for a short
// period and then return so that the caller can poll again.  That can
// limit performance when, for example, reading from a pipe so where possible
// we use a sub-class that waits until either input is available or it times
// out, whichever comes first, using "select" in Unix or MsgWaitForMultipleObjects
// in Windows.
// During a call to Waiter::Wait the thread is set as "not using ML memory"
// so a GC can happen while this thread is blocked.
class Waiter
{
public:
    Waiter() {}
    virtual ~Waiter() {}
    virtual void Wait(unsigned maxMillisecs);
    static Waiter *defaultWaiter;
};

#ifdef _WIN32
class WaitHandle: public Waiter
{
public:
    WaitHandle(HANDLE h, unsigned maxWait): m_Handle(h), m_maxWait(maxWait) {}
    virtual void Wait(unsigned maxMillisecs);
private:
    HANDLE m_Handle;
    unsigned m_maxWait;
};

#else

// Unix: Wait until a file descriptor is available for input
class WaitInputFD: public Waiter
{
public:
    WaitInputFD(int fd): m_waitFD(fd) {}
    virtual void Wait(unsigned maxMillisecs);
private:
    int m_waitFD;
};
#endif


// External interface to the Process module.  These functions are all implemented
// by the Processes class.
class ProcessExternal
{
public:
    virtual ~ProcessExternal() {} // Defined to suppress a warning from GCC

    virtual TaskData *GetTaskDataForThread(void) = 0;
    virtual TaskData *CreateNewTaskData() = 0;
    // Request all ML threads to exit and set the result code.  Does not cause
    // the calling thread itself to exit since this may be called on the GUI thread.
    virtual void RequestProcessExit(int n) = 0;
    // Exit from this thread.
    virtual NORETURNFN(void ThreadExit(TaskData *taskData)) = 0;

    virtual void BroadcastInterrupt(void) = 0;

    virtual void BeginRootThread(PolyObject *rootFunction) = 0;

    // Called when a thread may block.  Returns some time later when perhaps
    // the input is available.
    virtual void ThreadPauseForIO(TaskData *taskData, Waiter *pWait) = 0;
    // As ThreadPauseForIO but when there is no stream
    virtual void ThreadPause(TaskData *taskData) { ThreadPauseForIO(taskData, Waiter::defaultWaiter); }

    // If a thread is blocking for some time it should release its use
    // of the ML memory.  That allows a GC. ThreadUseMLMemory returns true if
    // a GC was in progress.
    virtual void ThreadUseMLMemory(TaskData *taskData) = 0;
    virtual void ThreadReleaseMLMemory(TaskData *taskData) = 0;

    // Requests from the threads for actions that need to be performed by
    // the root thread.
    virtual void MakeRootRequest(TaskData *taskData, MainThreadRequest *request) = 0;

    // Deal with any interrupt or kill requests.
    virtual bool ProcessAsynchRequests(TaskData *taskData) = 0;
    // Process an interrupt request synchronously.
    virtual void TestSynchronousRequests(TaskData *taskData) = 0;
    // Process any events, synchronous or asynchronous.
    virtual void TestAnyEvents(TaskData *taskData) = 0;
    
    // Profiling control.
    virtual void StartProfiling(void) = 0;
    virtual void StopProfiling(void) = 0;
    
    // Find space for an object.  Returns a pointer to the start.  "words" must include
    // the length word and the result points at where the length word will go.
    // If the allocation succeeds it may update the allocation values in the taskData object.
    // If the heap is exhausted it may set this thread (or other threads) to raise an exception.
    virtual PolyWord *FindAllocationSpace(TaskData *taskData, POLYUNSIGNED words, bool alwaysInSeg) = 0;

    // Signal handling support.  The ML signal handler thread blocks until it is
    // woken up by the signal detection thread.
    virtual bool WaitForSignal(TaskData *taskData, PLock *sigLock) = 0;
    virtual void SignalArrived(void) = 0;

    virtual poly_exn* GetInterrupt(void) = 0;
};

// Return the number of processors.  Used when configuring multi-threaded GC.
extern unsigned NumberOfProcessors(void);
extern unsigned NumberOfPhysicalProcessors(void);

extern ProcessExternal *processes;

extern struct _entrypts processesEPT[];

#endif
