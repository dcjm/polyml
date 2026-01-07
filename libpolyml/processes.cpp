/*
    Title:      Thread functions
    Author:     David C.J. Matthews

    Copyright (c) 2007,2008,2013-15, 2017, 2019-21 David C.J. Matthews

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

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x)
#endif

#ifdef HAVE_PROCESS_H
#include <process.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h> // Want unistd for _SC_NPROCESSORS_ONLN at least
#endif

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

#if (!defined(_WIN32))
#include <pthread.h>
#endif

#ifdef HAVE_SYS_SYSCTL_H
// Used determine number of processors in Mac OS X.
#include <sys/sysctl.h>
#endif

#if (defined(_WIN32))
#include <tchar.h>
#endif

#include <new>
#include <vector>

/************************************************************************
 *
 * Include runtime headers
 *
 ************************************************************************/

#include "globals.h"
#include "gc.h"
#include "mpoly.h"
#include "arb.h"
#include "machine_dep.h"
#include "diagnostics.h"
#include "processes.h"
#include "run_time.h"
#include "sys.h"
#include "sighandler.h"
#include "scanaddrs.h"
#include "save_vec.h"
#include "rts_module.h"
#include "noreturn.h"
#include "memmgr.h"
#include "locking.h"
#include "profiling.h"
#include "sharedata.h"
#include "exporter.h"
#include "statistics.h"
#include "rtsentry.h"
#include "gc_progress.h"

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyThreadKillSelf(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyThreadMutexBlock(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyThreadMutexUnlock(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyThreadCondVarWait(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyThreadCondVarWaitUntil(POLYUNSIGNED threadId, POLYUNSIGNED lockArg, POLYUNSIGNED timeArg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyThreadCondVarWake(POLYUNSIGNED targetThread);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyThreadForkThread(POLYUNSIGNED threadId, POLYUNSIGNED function, POLYUNSIGNED attrs, POLYUNSIGNED stack);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyThreadIsActive(POLYUNSIGNED targetThread);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyThreadInterruptThread(POLYUNSIGNED targetThread);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyThreadKillThread(POLYUNSIGNED targetThread);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyThreadBroadcastInterrupt(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyThreadTestInterrupt(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyThreadNumProcessors();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyThreadNumPhysicalProcessors();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyThreadMaxStackSize(POLYUNSIGNED threadId, POLYUNSIGNED newSize);
}

#define SAVE(x) taskData->saveVec.push(x)
#define SIZEOF(x) (sizeof(x)/sizeof(PolyWord))

// These values are stored in the second word of thread id object as
// a tagged integer.  They may be set and read by the thread in the ML
// code.  
#define PFLAG_BROADCAST     1   // If set, accepts a broadcast
// How to handle interrrupts
#define PFLAG_IGNORE        0   // Ignore interrupts completely
#define PFLAG_SYNCH         2   // Handle synchronously
#define PFLAG_ASYNCH        4   // Handle asynchronously
#define PFLAG_ASYNCH_ONCE   6   // First handle asynchronously then switch to synch.
#define PFLAG_INTMASK       6   // Mask of the above bits

struct _entrypts processesEPT[] =
{
    { "PolyThreadKillSelf",             (polyRTSFunction)&PolyThreadKillSelf},
    { "PolyThreadMutexBlock",           (polyRTSFunction)&PolyThreadMutexBlock},
    { "PolyThreadMutexUnlock",          (polyRTSFunction)&PolyThreadMutexUnlock},
    { "PolyThreadCondVarWait",          (polyRTSFunction)&PolyThreadCondVarWait},
    { "PolyThreadCondVarWaitUntil",     (polyRTSFunction)&PolyThreadCondVarWaitUntil},
    { "PolyThreadCondVarWake",          (polyRTSFunction)&PolyThreadCondVarWake},
    { "PolyThreadForkThread",           (polyRTSFunction)&PolyThreadForkThread},
    { "PolyThreadIsActive",             (polyRTSFunction)&PolyThreadIsActive},
    { "PolyThreadInterruptThread",      (polyRTSFunction)&PolyThreadInterruptThread},
    { "PolyThreadKillThread",           (polyRTSFunction)&PolyThreadKillThread},
    { "PolyThreadBroadcastInterrupt",   (polyRTSFunction)&PolyThreadBroadcastInterrupt},
    { "PolyThreadTestInterrupt",        (polyRTSFunction)&PolyThreadTestInterrupt},
    { "PolyThreadNumProcessors",        (polyRTSFunction)&PolyThreadNumProcessors},
    { "PolyThreadNumPhysicalProcessors",(polyRTSFunction)&PolyThreadNumPhysicalProcessors},
    { "PolyThreadMaxStackSize",         (polyRTSFunction)&PolyThreadMaxStackSize},

    { NULL, NULL} // End of list.
};

class Processes: public ProcessExternal, public RtsModule
{
public:
    Processes();
    // RtsModule overrides
    virtual void Init(void);
    virtual void Stop(void);
    virtual void GarbageCollect(ScanAddress *process);
    virtual void ForkChild(void) { singleThreaded = true;  } // After a Unix fork this is single threaded
public:
    void BroadcastInterrupt(void);
    void BeginRootThread(PolyObject *rootFunction);
    void RequestProcessExit(int n); // Request all ML threads to exit and set the process result code.
    // Called when a thread has completed - doesn't return.
    virtual NORETURNFN(void ThreadExit(TaskData *taskData));

    // Called when a thread may block.  Returns some time later when perhaps
    // the input is available.
    virtual void ThreadPauseForIO(TaskData *taskData, Waiter *pWait);
    // Return the task data for the current thread.
    virtual TaskData *GetTaskDataForThread(void);
    // Create a new task data object for the current thread.
    virtual TaskData *CreateNewTaskData();
    // Create a new thread.  The "args" argument is only used for threads
    // created in the RTS by the signal handler.
    Handle ForkThread(TaskData *taskData, Handle threadFunction, PolyWord flags, PolyWord stacksize);
    // Process general RTS requests from ML.
    Handle ThreadDispatch(TaskData *taskData, Handle args, Handle code);

    virtual void ThreadUseMLMemory(TaskData *taskData); 
    virtual void ThreadReleaseMLMemory(TaskData *taskData);

    virtual poly_exn* GetInterrupt(void) { return interrupt_exn; }

    // If the schedule lock is already held we need to use these functions.
    void ThreadUseMLMemoryWithSchedLock(TaskData *taskData);
    void ThreadReleaseMLMemoryWithSchedLock(TaskData *taskData);

    // Requests from the threads for actions that need to be performed by
    // the root thread. Make the request and wait until it has completed.
    virtual void MakeRootRequest(TaskData *taskData, MainThreadRequest *request);

    // Deal with any interrupt or kill requests.
    virtual bool ProcessAsynchRequests(TaskData *taskData);
    // Process an interrupt request synchronously.
    virtual void TestSynchronousRequests(TaskData *taskData);
    // Process any events, synchronous or asynchronous.
    virtual void TestAnyEvents(TaskData *taskData);

    // Set a thread to be interrupted or killed.  Wakes up the
    // thread if necessary.  MUST be called with schedLock held.
    void MakeRequest(TaskData *p, ThreadRequests request);

    // Profiling control.
    virtual void StartProfiling(void);
    virtual void StopProfiling(void);

#ifdef HAVE_WINDOWS_H
    // Windows: Called every millisecond while profiling is on.
    void ProfileInterrupt(void);
#else
    // Unix: Start a profile timer for a thread.
    void StartProfilingTimer(void);
#endif
    // Memory allocation.  Tries to allocate space.  If the allocation succeeds it
    // may update the allocation values in the taskData object.  If the heap is exhausted
    // it may set this thread (or other threads) to raise an exception.
    PolyWord *FindAllocationSpace(TaskData *taskData, POLYUNSIGNED words, bool alwaysInSeg);

    // Get the task data value from the task reference.
    // The task data reference is a volatile ref containing the
    // address of the C++ task data.
    // N.B.  This is updated when the thread exits and the TaskData object
    // is deleted.
    TaskData *TaskForIdentifier(PolyObject *taskId) {
        return *(TaskData**)(((ThreadObject*)taskId)->threadRef.AsObjPtr());
    }

    // Signal handling support.  The ML signal handler thread blocks until it is
    // woken up by the signal detection thread.
    virtual bool WaitForSignal(TaskData *taskData, PLock *sigLock);
    virtual void SignalArrived(void);

    // Operations on mutexes
    void MutexBlock(TaskData *taskData, Handle hMutex);
    void MutexUnlock(TaskData *taskData, Handle hMutex);

    // Operations on condition variables.
    void WaitInfinite(TaskData *taskData, Handle hMutex);
    void WaitUntilTime(TaskData *taskData, Handle hMutex, Handle hTime);
    bool WakeThread(PolyObject *targetThread);

    // Generally, the system runs with multiple threads.  After a
    // fork, though, there is only one thread.
    bool singleThreaded;

    // Each thread has an entry in this vector.
    std::vector<TaskData*> taskArray;

    /* schedLock: This lock must be held when making scheduling decisions.
       It must also be held before adding items to taskArray, removing
       them or scanning the vector.
       It must also be held before deleting a TaskData object
       or using it in a thread other than the "owner"  */
    PLock schedLock;
#if (!defined(_WIN32))
    pthread_key_t tlsId;
#else
    DWORD tlsId;
#endif

    // We make an exception packet for Interrupt and store it here.
    // This exception can be raised if we run out of store so we need to
    // make sure we have the packet before we do.
    poly_exn *interrupt_exn;

    /* initialThreadWait: The initial thread waits on this for
       wake-ups from the ML threads requesting actions such as GC or
       close-down. */
    PCondVar initialThreadWait;
    // A requesting thread sets this to indicate the request.  This value
    // is only reset once the request has been satisfied.
    MainThreadRequest *threadRequest;

    PCondVar mlThreadWait;  // All the threads block on here until the request has completed.

    int exitResult;
    bool exitRequest;

#ifdef HAVE_WINDOWS_H /* Windows including Cygwin */
    // Used in profiling
    HANDLE hStopEvent; /* Signalled to stop all threads. */
    HANDLE profilingHd;
    HANDLE mainThreadHandle; // Handle for main thread
    LONGLONG lastCPUTime; // CPU used by main thread.
#endif

    TaskData *sigTask;  // Pointer to current signal task.
};

// Global process data.
static Processes processesModule;
ProcessExternal *processes = &processesModule;

Processes::Processes(): singleThreaded(false),
    schedLock("Scheduler"), interrupt_exn(0),
    threadRequest(0), exitResult(0), exitRequest(false), sigTask(0)
{
#ifdef HAVE_WINDOWS_H
    hStopEvent = NULL;
    profilingHd = NULL;
    lastCPUTime = 0;
    mainThreadHandle = NULL;
#endif
}

enum _mainThreadPhase mainThreadPhase = MTP_USER_CODE;

// Get the attribute flags.
static POLYUNSIGNED ThreadAttrs(TaskData *taskData)
{
    return UNTAGGED_UNSIGNED(taskData->threadObject->flags);
}

POLYUNSIGNED PolyThreadMutexBlock(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);

    if (profileMode == kProfileMutexContention)
        taskData->addProfileCount(1);

    try {
        processesModule.MutexBlock(taskData, pushedArg);
    }
    catch (KillException &) {
        processes->ThreadExit(taskData); // TestSynchronousRequests may test for kill
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

POLYUNSIGNED PolyThreadMutexUnlock(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);

    try {
        processesModule.MutexUnlock(taskData, pushedArg);
    }
    catch (KillException &) {
        processes->ThreadExit(taskData); // TestSynchronousRequests may test for kill
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

/* A mutex was locked i.e. the count was ~1 or less.  We will have set it to
  ~1. This code blocks if the count is still ~1.  It does actually return
  if another thread tries to lock the mutex and hasn't yet set the value
  to ~1 but that doesn't matter since whenever we return we simply try to
  get the lock again. */
void Processes::MutexBlock(TaskData *taskData, Handle hMutex)
{
    PLocker lock(&schedLock);
    // We have to check the value again with schedLock held rather than
    // simply waiting because otherwise the unlocking thread could have
    // set the variable back to 0 (unlocked) and signalled any waiters
    // before we actually got to wait.  
    if (UNTAGGED(DEREFHANDLE(hMutex)->Get(0)) > 1)
    {
        // Set this so we can see what we're blocked on.
        taskData->blockMutex = DEREFHANDLE(hMutex);
        // Now release the ML memory.  A GC can start.
        ThreadReleaseMLMemoryWithSchedLock(taskData);
        // Wait until we're woken up.  We mustn't block if we have been
        // interrupted, and are processing interrupts asynchronously, or
        // we've been killed.
        switch (taskData->requests)
        {
        case kRequestKill:
            // We've been killed.  Handle this later.
            break;
        case kRequestInterrupt:
            {
                // We've been interrupted.  
                POLYUNSIGNED attrs = ThreadAttrs(taskData) & PFLAG_INTMASK;
                if (attrs == PFLAG_ASYNCH || attrs == PFLAG_ASYNCH_ONCE)
                    break;
                // If we're ignoring interrupts or handling them synchronously
                // we don't do anything here.
            }
        case kRequestNone:
            globalStats.incCount(PSC_THREADS_WAIT_MUTEX);
            taskData->threadLock.Wait(&schedLock);
            globalStats.decCount(PSC_THREADS_WAIT_MUTEX);
        }
        taskData->blockMutex = 0; // No longer blocked.
        ThreadUseMLMemoryWithSchedLock(taskData);
    }
    // Test to see if we have been interrupted and if this thread
    // processes interrupts asynchronously we should raise an exception
    // immediately.  Perhaps we do that whenever we exit from the RTS.
}

/* Unlock a mutex.  Called after decrementing the count and discovering
   that at least one other thread has tried to lock it.  We may need
   to wake up threads that are blocked. */
void Processes::MutexUnlock(TaskData *taskData, Handle hMutex)
{
    // The caller has already set the variable to 1 (unlocked).
    // We need to acquire schedLock so that we can
    // be sure that any thread that is trying to lock sees either
    // the updated value (and so doesn't wait) or has successfully
    // waited on its threadLock (and so will be woken up).
    PLocker lock(&schedLock);
    // Unlock any waiters.
    for (std::vector<TaskData*>::iterator i = taskArray.begin(); i != taskArray.end(); i++)
    {
        TaskData *p = *i;
        // If the thread is blocked on this mutex we can signal the thread.
        if (p && p->blockMutex == DEREFHANDLE(hMutex))
            p->threadLock.Signal();
    }
}

POLYUNSIGNED PolyThreadCondVarWait(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);

    try {
        processesModule.WaitInfinite(taskData, pushedArg);
    }
    catch (KillException &) {
        processes->ThreadExit(taskData); // TestSynchronousRequests may test for kill
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

POLYUNSIGNED PolyThreadCondVarWaitUntil(POLYUNSIGNED threadId, POLYUNSIGNED lockArg, POLYUNSIGNED timeArg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedLockArg = taskData->saveVec.push(lockArg);
    Handle pushedTimeArg = taskData->saveVec.push(timeArg);

    try {
        processesModule.WaitUntilTime(taskData, pushedLockArg, pushedTimeArg);
    }
    catch (KillException &) {
        processes->ThreadExit(taskData); // TestSynchronousRequests may test for kill
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

// Atomically drop a mutex and wait for a wake up.
// It WILL NOT RAISE AN EXCEPTION unless it is set to handle exceptions
// asynchronously (which it shouldn't do if the ML caller code is correct).
// It may return as a result of any of the following:
//      an explicit wake up.
//      an interrupt, either direct or broadcast
//      a trap i.e. a request to handle an asynchronous event.
void Processes::WaitInfinite(TaskData *taskData, Handle hMutex)
{
    PLocker lock(&schedLock);
    // Atomically release the mutex.  This is atomic because we hold schedLock
    // so no other thread can call signal or broadcast.
    if (! taskData->AtomicallyReleaseMutex(hMutex->WordP()))
    {
        // The mutex was locked so we have to release any waiters.
        // Unlock any waiters.
        for (std::vector<TaskData*>::iterator i = taskArray.begin(); i != taskArray.end(); i++)
        {
            TaskData *p = *i;
            // If the thread is blocked on this mutex we can signal the thread.
            if (p && p->blockMutex == DEREFHANDLE(hMutex))
                p->threadLock.Signal();
        }
    }
    // Wait until we're woken up.  Don't block if we have been interrupted
    // or killed.
    if (taskData->requests == kRequestNone)
    {
        // Now release the ML memory.  A GC can start.
        ThreadReleaseMLMemoryWithSchedLock(taskData);
        globalStats.incCount(PSC_THREADS_WAIT_CONDVAR);
        taskData->threadLock.Wait(&schedLock);
        globalStats.decCount(PSC_THREADS_WAIT_CONDVAR);
        // We want to use the memory again.
        ThreadUseMLMemoryWithSchedLock(taskData);
    }
}

// Atomically drop a mutex and wait for a wake up or a time to wake up
void Processes::WaitUntilTime(TaskData *taskData, Handle hMutex, Handle hWakeTime)
{
    // Convert the time into the correct format for WaitUntil before acquiring
    // schedLock.  div_longc could do a GC which requires schedLock.
#if (defined(_WIN32))
    // On Windows it is the number of 100ns units since the epoch
    FILETIME tWake;
    getFileTimeFromArb(taskData, hWakeTime, &tWake);
#else
    // Unix style times.
    struct timespec tWake;
    // On Unix we represent times as a number of microseconds.
    Handle hMillion = Make_arbitrary_precision(taskData, 1000000);
    tWake.tv_sec =
        get_C_ulong(taskData, DEREFWORD(div_longc(taskData, hMillion, hWakeTime)));
    tWake.tv_nsec =
        1000*get_C_ulong(taskData, DEREFWORD(rem_longc(taskData, hMillion, hWakeTime)));
#endif
    PLocker lock(&schedLock);
    // Atomically release the mutex.  This is atomic because we hold schedLock
    // so no other thread can call signal or broadcast.
    if (!taskData->AtomicallyReleaseMutex(hMutex->WordP()))
    {
        // The mutex was locked so we have to release any waiters.
        // Unlock any waiters.
        for (std::vector<TaskData*>::iterator i = taskArray.begin(); i != taskArray.end(); i++)
        {
            TaskData *p = *i;
            // If the thread is blocked on this mutex we can signal the thread.
            if (p && p->blockMutex == DEREFHANDLE(hMutex))
                p->threadLock.Signal();
        }
    }
    // Wait until we're woken up.  Don't block if we have been interrupted
    // or killed.
    if (taskData->requests == kRequestNone)
    {
        // Now release the ML memory.  A GC can start.
        ThreadReleaseMLMemoryWithSchedLock(taskData);
        globalStats.incCount(PSC_THREADS_WAIT_CONDVAR);
        (void)taskData->threadLock.WaitUntil(&schedLock, &tWake);
        globalStats.decCount(PSC_THREADS_WAIT_CONDVAR);
        // We want to use the memory again.
        ThreadUseMLMemoryWithSchedLock(taskData);
    }
}

bool Processes::WakeThread(PolyObject *targetThread)
{
    bool result = false; // Default to failed.
    // Acquire the schedLock first.  This ensures that this is
    // atomic with respect to waiting.
    PLocker lock(&schedLock);
    TaskData *p = TaskForIdentifier(targetThread);
    if (p && p->threadObject == targetThread)
    {
        POLYUNSIGNED attrs = ThreadAttrs(p) & PFLAG_INTMASK;
        if (p->requests == kRequestNone ||
            (p->requests == kRequestInterrupt && attrs == PFLAG_IGNORE))
        {
            p->threadLock.Signal();
            result = true;
        }
    }
    return result;
}

POLYUNSIGNED PolyThreadCondVarWake(POLYUNSIGNED targetThread)
{
    if (processesModule.WakeThread(PolyWord::FromUnsigned(targetThread).AsObjPtr()))
        return TAGGED(1).AsUnsigned();
    else return TAGGED(0).AsUnsigned();
}

// Test if a thread is active.
POLYUNSIGNED PolyThreadIsActive(POLYUNSIGNED targetThread)
{
    // There's a race here: the thread may be exiting but since we're not doing
    // anything with the TaskData object we don't need a lock.
    TaskData *p = processesModule.TaskForIdentifier(PolyWord::FromUnsigned(targetThread).AsObjPtr());
    if (p != 0) return TAGGED(1).AsUnsigned();
    else return TAGGED(0).AsUnsigned();
}

// Send an interrupt to a specific thread
POLYUNSIGNED PolyThreadInterruptThread(POLYUNSIGNED targetThread)
{
    // Must lock here because the thread may be exiting.
    processesModule.schedLock.Lock();
    TaskData *p = processesModule.TaskForIdentifier(PolyWord::FromUnsigned(targetThread).AsObjPtr());
    if (p) processesModule.MakeRequest(p, kRequestInterrupt);
    processesModule.schedLock.Unlock();
    // If the thread cannot be identified return false.
    // The caller can then raise an exception
    if (p == 0) return TAGGED(0).AsUnsigned();
    else return TAGGED(1).AsUnsigned();
}

// Kill a specific thread
POLYUNSIGNED PolyThreadKillThread(POLYUNSIGNED targetThread)
{
    processesModule.schedLock.Lock();
    TaskData *p = processesModule.TaskForIdentifier(PolyWord::FromUnsigned(targetThread).AsObjPtr());
    if (p) processesModule.MakeRequest(p, kRequestKill);
    processesModule.schedLock.Unlock();
    // If the thread cannot be identified return false.
    // The caller can then raise an exception
    if (p == 0) return TAGGED(0).AsUnsigned();
    else return TAGGED(1).AsUnsigned();
}

POLYUNSIGNED PolyThreadBroadcastInterrupt(POLYUNSIGNED /*threadId*/)
{
    processesModule.BroadcastInterrupt();
    return TAGGED(0).AsUnsigned();
}

POLYUNSIGNED PolyThreadTestInterrupt(POLYUNSIGNED threadId)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();

    try {
        processesModule.TestSynchronousRequests(taskData);
        // Also process any asynchronous requests that may be pending.
        // These will be handled "soon" but if we have just switched from deferring
        // interrupts this guarantees that any deferred interrupts will be handled now.
        if (processesModule.ProcessAsynchRequests(taskData))
            throw IOException();
    }
    catch (KillException &) {
        processes->ThreadExit(taskData); // TestSynchronousRequests may test for kill
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

// Return the number of processors.
// Returns 1 if there is any problem.
POLYUNSIGNED PolyThreadNumProcessors(void)
{
    return TAGGED(NumberOfProcessors()).AsUnsigned();
}

// Return the number of physical processors.
// Returns 0 if there is any problem.
POLYUNSIGNED PolyThreadNumPhysicalProcessors(void)
{
    return TAGGED(NumberOfPhysicalProcessors()).AsUnsigned();
}

// Set the maximum stack size.
POLYUNSIGNED PolyThreadMaxStackSize(POLYUNSIGNED threadId, POLYUNSIGNED newSizeU)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    PolyWord newSize = PolyWord::FromUnsigned(newSizeU);

    try {
            taskData->threadObject->mlStackSize = newSize;
            if (newSize != TAGGED(0))
            {
                uintptr_t current = taskData->currentStackSpace(); // Current size in words
                uintptr_t newWords = getPolyUnsigned(taskData, newSize);
                if (current > newWords)
                    raise_exception0(taskData, EXC_interrupt);
            }
    }
    catch (KillException &) {
        processes->ThreadExit(taskData); // TestSynchronousRequests may test for kill
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

// Old dispatch function.  This is only required because the pre-built compiler
// may use some of these e.g. fork.
Handle Processes::ThreadDispatch(TaskData *taskData, Handle args, Handle code)
{
    unsigned c = get_C_unsigned(taskData, code->Word());
    TaskData *ptaskData = taskData;
    switch (c)
    {
    case 1:
        MutexBlock(taskData, args);
        return SAVE(TAGGED(0));

    case 2:
        MutexUnlock(taskData, args);
        return SAVE(TAGGED(0));

    case 7: // Fork a new thread.  The arguments are the function to run and the attributes.
            return ForkThread(ptaskData, SAVE(args->WordP()->Get(0)), args->WordP()->Get(1),
                        // For backwards compatibility we check the length here
                        args->WordP()->Length() <= 2 ? TAGGED(0) : args->WordP()->Get(2));

    case 10: // Broadcast an interrupt to all threads that are interested.
        BroadcastInterrupt();
        return SAVE(TAGGED(0));

    default:
        {
            char msg[100];
            snprintf(msg, sizeof(msg), "Unknown thread function: %u", c);
            raise_fail(taskData, msg);
            return 0;
        }
    }
}

// Fill unused allocation space with a dummy object to preserve the invariant
// that memory is always valid.
void TaskData::FillUnusedSpace(void)
{
    if (allocPointer > allocLimit)
        gMem.FillUnusedSpace(allocLimit, allocPointer-allocLimit); 
}


TaskData::TaskData(): allocPointer(0), allocLimit(0), allocSize(MIN_HEAP_SIZE), allocCount(0),
        stack(0), threadObject(0), signalStack(0),
        requests(kRequestNone), blockMutex(0), inMLHeap(false),
        runningProfileTimer(false)
{
#ifdef HAVE_WINDOWS_H
    lastCPUTime = 0;
#endif
#ifdef HAVE_WINDOWS_H
    threadHandle = 0;
#endif
    threadExited = false;
}

TaskData::~TaskData()
{
    if (signalStack) free(signalStack);
    if (stack) gMem.DeleteStackSpace(stack);
#ifdef HAVE_WINDOWS_H
    if (threadHandle) CloseHandle(threadHandle);
#endif
}

// Broadcast an interrupt to all relevant threads.
void Processes::BroadcastInterrupt(void)
{
    // If a thread is set to accept broadcast interrupts set it to
    // "interrupted".
    PLocker lock(&schedLock);
    for (std::vector<TaskData*>::iterator i = taskArray.begin(); i != taskArray.end(); i++)
    {
        TaskData *p = *i;
        if (p)
        {
            POLYUNSIGNED attrs = ThreadAttrs(p);
            if (attrs & PFLAG_BROADCAST)
                MakeRequest(p, kRequestInterrupt);
        }
    }
}

// Set the asynchronous request variable for the thread.  Must be called
// with the schedLock held.  Tries to wake the thread up if possible.
void Processes::MakeRequest(TaskData *p, ThreadRequests request)
{
    // We don't override a request to kill by an interrupt request.
    if (p->requests < request)
    {
        p->requests = request;
        p->InterruptCode();
        p->threadLock.Signal();
        // Set the value in the ML object as well so the ML code can see it
        p->threadObject->requestCopy = TAGGED(request);
    }
}

void Processes::ThreadExit(TaskData *taskData)
{
    if (debugOptions & DEBUG_THREADS)
        Log("THREAD: Thread %p exiting\n", taskData);

#if (!defined(_WIN32))
    // Block any profile interrupt from now on.  We're deleting the ML stack for this thread.
    sigset_t block_sigs;
    sigemptyset(&block_sigs);
    sigaddset(&block_sigs, SIGVTALRM);
    pthread_sigmask(SIG_BLOCK, &block_sigs, NULL);
    // Remove the thread-specific data since it's no
    // longer valid.
    pthread_setspecific(tlsId, 0);
#endif

    if (singleThreaded) finish(0);

    schedLock.Lock();
    ThreadReleaseMLMemoryWithSchedLock(taskData); // Allow a GC if it was waiting for us.
    taskData->threadExited = true;
    initialThreadWait.Signal(); // Tell it we've finished.
    schedLock.Unlock();
#if (!defined(_WIN32))
    pthread_exit(0);
#else
    ExitThread(0);
#endif
}

// These two functions are used for calls from outside where
// the lock has not yet been acquired.
void Processes::ThreadUseMLMemory(TaskData *taskData)
{
    // Trying to acquire the lock here may block if a GC is in progress
    PLocker lock(&schedLock);
    ThreadUseMLMemoryWithSchedLock(taskData);
}

void Processes::ThreadReleaseMLMemory(TaskData *taskData)
{
    PLocker lock(&schedLock);
    ThreadReleaseMLMemoryWithSchedLock(taskData);
}

// Called when a thread wants to resume using the ML heap.  That could
// be after a wait for some reason or after executing some foreign code.
// Since there could be a GC in progress already at this point we may either
// be blocked waiting to acquire schedLock or we may need to wait until
// we are woken up at the end of the GC.
void Processes::ThreadUseMLMemoryWithSchedLock(TaskData *taskData)
{
    TaskData *ptaskData = taskData;
    // If there is a request outstanding we have to wait for it to
    // complete.  We notify the root thread and wait for it.
    while (threadRequest != 0)
    {
        initialThreadWait.Signal();
        // Wait for the GC to happen
        mlThreadWait.Wait(&schedLock);
    }
    ASSERT(! ptaskData->inMLHeap);
    ptaskData->inMLHeap = true;
}

// Called to indicate that the thread has temporarily finished with the
// ML memory either because it is going to wait for something or because
// it is going to run foreign code.  If there is an outstanding GC request
// that can proceed.
void Processes::ThreadReleaseMLMemoryWithSchedLock(TaskData *taskData)
{
    TaskData *ptaskData = taskData;
    ASSERT(ptaskData->inMLHeap);
    ptaskData->inMLHeap = false;
    // Put a dummy object in any unused space.  This maintains the
    // invariant that the allocated area is filled with valid objects.
    ptaskData->FillUnusedSpace();
    //
    if (threadRequest != 0)
        initialThreadWait.Signal();
}


// Make a request to the root thread.
void Processes::MakeRootRequest(TaskData *taskData, MainThreadRequest *request)
{
    if (singleThreaded)
    {
        mainThreadPhase = request->mtp;
        ThreadReleaseMLMemoryWithSchedLock(taskData); // Primarily to call FillUnusedSpace
        request->Perform();
        ThreadUseMLMemoryWithSchedLock(taskData);
        mainThreadPhase = MTP_USER_CODE;
    }
    else
    {
        PLocker locker(&schedLock);

        // Wait for any other requests. 
        while (threadRequest != 0)
        {
            // Deal with any pending requests.
            ThreadReleaseMLMemoryWithSchedLock(taskData);
            ThreadUseMLMemoryWithSchedLock(taskData); // Drops schedLock while waiting.
        }
        // Now the other requests have been dealt with (and we have schedLock).
        request->completed = false;
        threadRequest = request;
        // Wait for it to complete.
        while (! request->completed)
        {
            ThreadReleaseMLMemoryWithSchedLock(taskData);
            ThreadUseMLMemoryWithSchedLock(taskData); // Drops schedLock while waiting.
        }
    }
}

// Find space for an object.  Returns a pointer to the start.  "words" must include
// the length word and the result points at where the length word will go.
PolyWord *Processes::FindAllocationSpace(TaskData *taskData, POLYUNSIGNED words, bool alwaysInSeg)
{
    bool triedInterrupt = false;
#ifdef POLYML32IN64
    words = (words + POLYML32IN64 - 1) & (-POLYML32IN64); // Must be on correct boundary.
#endif

    while (1)
    {
        // After a GC allocPointer and allocLimit are zero and when allocating the
        // heap segment we request a minimum of zero words.
        if (taskData->allocPointer != 0 && taskData->allocPointer >= taskData->allocLimit + words)
        {
            // There's space in the current segment,
            taskData->allocPointer -= words;
#ifdef POLYML32IN64
            // Zero the last words.  If we've rounded up the caller won't set them.
            for (unsigned i = 0; i < POLYML32IN64 && words != 0; i++)
            {
                taskData->allocPointer[--words] = PolyWord::FromUnsigned(0);
            }
            ASSERT(((taskData->allocPointer - (PolyWord*)0) & (POLYML32IN64 - 1)) == (POLYML32IN64 - 1)); // Must be odd-word aligned
#endif
            return taskData->allocPointer;
        }
        else // Insufficient space in this area. 
        {
            if (words > taskData->allocSize && ! alwaysInSeg)
            {
                // If the object we want is larger than the heap segment size
                // we allocate it separately rather than in the segment.
                PolyWord *foundSpace = gMem.AllocHeapSpace(words);
                if (foundSpace) return foundSpace;
            }
            else
            {
                // Fill in any unused space in the existing segment
                taskData->FillUnusedSpace();
                // Get another heap segment with enough space for this object.
                uintptr_t requestSpace = taskData->allocSize+words;
                uintptr_t spaceSize = requestSpace;
                // Get the space and update spaceSize with the actual size.
                PolyWord *space = gMem.AllocHeapSpace(words, spaceSize);
                if (space)
                {
                    // Double the allocation size for the next time if
                    // we succeeded in allocating the whole space.
                    taskData->allocCount++; 
                    if (spaceSize == requestSpace) taskData->allocSize = taskData->allocSize*2;
                    taskData->allocLimit = space;
                    taskData->allocPointer = space+spaceSize;
                    // Actually allocate the object
                    taskData->allocPointer -= words;
#ifdef POLYML32IN64
                    ASSERT(((taskData->allocPointer - (PolyWord*)0) & (POLYML32IN64 - 1)) == (POLYML32IN64 - 1)); // Must be odd-word aligned
#endif
                    return taskData->allocPointer;
                }
            }

            // It's possible that another thread has requested a GC in which case
            // we will have memory when that happens.  We don't want to start
            // another GC.
            if (! singleThreaded)
            {
                PLocker locker(&schedLock);
                if (threadRequest != 0)
                {
                    ThreadReleaseMLMemoryWithSchedLock(taskData);
                    ThreadUseMLMemoryWithSchedLock(taskData);
                    continue; // Try again
                }
            }

            // Try garbage-collecting.  If this failed return 0.
            if (! QuickGC(taskData, words))
            {
                extern FILE *polyStderr;
                if (! triedInterrupt)
                {
                    triedInterrupt = true;
                    fprintf(polyStderr,"Run out of store - interrupting threads\n");
                    if (debugOptions & DEBUG_THREADS)
                        Log("THREAD: Run out of store, interrupting threads\n");
                    BroadcastInterrupt();
                    try {
                        if (ProcessAsynchRequests(taskData))
                            return 0; // Has been interrupted.
                    }
                    catch(KillException &)
                    {
                        // The thread may have been killed.
                        ThreadExit(taskData);
                    }
                    // Not interrupted: pause this thread to allow for other
                    // interrupted threads to free something.
#if defined(_WIN32)
                    Sleep(5000);
#else
                    sleep(5);
#endif
                    // Try again.
                }
                else {
                    // That didn't work.  Exit.
                    fprintf(polyStderr,"Failed to recover - exiting\n");
                    RequestProcessExit(1); // Begins the shutdown process
                    ThreadExit(taskData); // And terminate this thread.
                }
             }
            // Try again.  There should be space now.
        }
    }
}

#ifdef _MSC_VER
// Don't tell me that exitThread has a non-void type.
#pragma warning(disable:4646)
#endif

Handle exitThread(TaskData *taskData)
/* A call to this is put on the stack of a new thread so when the
   thread function returns the thread goes away. */  
{
    processesModule.ThreadExit(taskData);
}

// Terminate the current thread.  Never returns.
POLYUNSIGNED PolyThreadKillSelf(POLYUNSIGNED threadId)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall(); // Possibly not needed since we never return
    processesModule.ThreadExit(taskData);
    return 0; 
}

/* Called when a thread is about to block, usually because of IO.
   If this is interruptable (currently only used for Posix functions)
   the process will be set to raise an exception if any signal is handled.
   It may also raise an exception if another thread has called
   broadcastInterrupt. */
void Processes::ThreadPauseForIO(TaskData *taskData, Waiter *pWait)
{
    TestAnyEvents(taskData); // Consider this a blocking call that may raise Interrupt
    ThreadReleaseMLMemory(taskData);
    globalStats.incCount(PSC_THREADS_WAIT_IO);
    pWait->Wait(1000); // Wait up to a second
    globalStats.decCount(PSC_THREADS_WAIT_IO);
    ThreadUseMLMemory(taskData);
    TestAnyEvents(taskData); // Check if we've been interrupted.
}

// Default waiter: simply wait for the time.  In Unix it may be woken
// up by a signal.
void Waiter::Wait(unsigned maxMillisecs)
{
    // Since this is used only when we can't monitor the source directly
    // we set this to 10ms so that we're not waiting too long.
    if (maxMillisecs > 10) maxMillisecs = 10;
#if (defined(_WIN32))
    Sleep(maxMillisecs);
#else
    // Unix
    fd_set read_fds, write_fds, except_fds;
    struct timeval toWait = { 0, 0 };
    toWait.tv_sec = maxMillisecs / 1000;
    toWait.tv_usec = (maxMillisecs % 1000) * 1000;
    FD_ZERO(&read_fds);
    FD_ZERO(&write_fds);
    FD_ZERO(&except_fds);
    select(FD_SETSIZE, &read_fds, &write_fds, &except_fds, &toWait);
#endif
}

static Waiter defWait;
Waiter *Waiter::defaultWaiter = &defWait;

#ifdef _WIN32
// Wait for the specified handle to be signalled.
void WaitHandle::Wait(unsigned maxMillisecs)
{
    // Wait until we get input or we're woken up.
    if (maxMillisecs > m_maxWait)
        maxMillisecs = m_maxWait;
    if (m_Handle == NULL)
        Sleep(maxMillisecs);
    else WaitForSingleObject(m_Handle, maxMillisecs);
}

#else

// Unix and Cygwin: Wait for a file descriptor on input.
void WaitInputFD::Wait(unsigned maxMillisecs)
{
    fd_set read_fds, write_fds, except_fds;
    struct timeval toWait = { 0, 0 };
    toWait.tv_sec = maxMillisecs / 1000;
    toWait.tv_usec = (maxMillisecs % 1000) * 1000;
    FD_ZERO(&read_fds);
    if (m_waitFD >= 0) FD_SET(m_waitFD, &read_fds);
    FD_ZERO(&write_fds);
    FD_ZERO(&except_fds);
    select(FD_SETSIZE, &read_fds, &write_fds, &except_fds, &toWait);
}
#endif

// Get the task data for the current thread.  This is held in
// thread-local storage.  Normally this is passed in taskData but
// in a few cases this isn't available.
TaskData *Processes::GetTaskDataForThread(void)
{
#if (!defined(_WIN32))
    return (TaskData *)pthread_getspecific(tlsId);
#else
    return (TaskData *)TlsGetValue(tlsId);
#endif
}

// Called to create a task data object in the current thread.
// This is currently only used if a thread created in foreign code calls
// a callback.
TaskData *Processes::CreateNewTaskData()
{
    TaskData *taskData = machineDependent->CreateTaskData();
#if defined(HAVE_WINDOWS_H)
    HANDLE thisProcess = GetCurrentProcess();
    DuplicateHandle(thisProcess, GetCurrentThread(), thisProcess, 
        &(taskData->threadHandle), THREAD_ALL_ACCESS, FALSE, 0);
#endif
    unsigned thrdIndex;

    {
        PLocker lock(&schedLock);
        // See if there's a spare entry in the array.
        for (thrdIndex = 0;
                thrdIndex < taskArray.size() && taskArray[thrdIndex] != 0;
                thrdIndex++);

        if (thrdIndex == taskArray.size()) // Need to expand the array
        {
            try {
                taskArray.push_back(taskData);
            } catch (std::bad_alloc&) {
                delete(taskData);
                throw MemoryException();
            }
        }
        else
        {
            taskArray[thrdIndex] = taskData;
        }
    }

    taskData->stack = gMem.NewStackSpace(machineDependent->InitialStackSize());
    if (taskData->stack == 0)
    {
        delete(taskData);
        throw MemoryException();
    }

    // TODO:  Check that there isn't a problem if we try to allocate
    // memory here and result in a GC.
    taskData->InitStackFrame(taskData, 0);

    ThreadUseMLMemory(taskData);

    // Make a thread reference to point to this taskData object.
    Handle threadRef = MakeVolatileWord(taskData, taskData);
    // Make a thread object.  Since it's in the thread table it can't be garbage collected.
    taskData->threadObject = (ThreadObject*)alloc(taskData, sizeof(ThreadObject)/sizeof(PolyWord), F_MUTABLE_BIT);
    taskData->threadObject->threadRef = threadRef->Word();
    taskData->threadObject->flags = TAGGED(PFLAG_SYNCH);
    taskData->threadObject->threadLocal = TAGGED(0); // Empty thread-local store
    taskData->threadObject->requestCopy = TAGGED(0); // Cleared interrupt state
    taskData->threadObject->mlStackSize = TAGGED(0); // Unlimited stack size
    for (unsigned i = 0; i < sizeof(taskData->threadObject->debuggerSlots)/sizeof(PolyWord); i++)
        taskData->threadObject->debuggerSlots[i] = TAGGED(0);

#if (!defined(_WIN32))
    initThreadSignals(taskData);
    pthread_setspecific(tlsId, taskData);
#else
    TlsSetValue(tlsId, taskData);
#endif
    globalStats.incCount(PSC_THREADS);

    return taskData;
}

// This function is run when a new thread has been forked.  The
// parameter is the taskData value for the new thread.  This function
// is also called directly for the main thread.
#if (!defined(_WIN32))
static void *NewThreadFunction(void *parameter)
{
    TaskData *taskData = (TaskData *)parameter;
#ifdef HAVE_WINDOWS_H
    // Cygwin: Get the Windows thread handle in case it's needed for profiling.
    HANDLE thisProcess = GetCurrentProcess();
    DuplicateHandle(thisProcess, GetCurrentThread(), thisProcess, 
        &(taskData->threadHandle), THREAD_ALL_ACCESS, FALSE, 0);
#endif
    initThreadSignals(taskData);
    pthread_setspecific(processesModule.tlsId, taskData);
    taskData->saveVec.init(); // Remove initial data
    globalStats.incCount(PSC_THREADS);
    processes->ThreadUseMLMemory(taskData);
    try {
        taskData->EnterPolyCode(); // Will normally (always?) call ExitThread.
    }
    catch (KillException &) {
        processesModule.ThreadExit(taskData);
    }

    return 0;
}
#else
static DWORD WINAPI NewThreadFunction(void *parameter)
{
    TaskData *taskData = (TaskData *)parameter;
    TlsSetValue(processesModule.tlsId, taskData);
    taskData->saveVec.init(); // Removal initial data
    globalStats.incCount(PSC_THREADS);
    processes->ThreadUseMLMemory(taskData);
    try {
        taskData->EnterPolyCode();
    }
    catch (KillException &) {
        processesModule.ThreadExit(taskData);
    }
    return 0;
}
#endif

// Sets up the initial thread from the root function.  This is run on
// the initial thread of the process so it will work if we don't
// have pthreads.
// When multithreading this thread also deals with all garbage-collection
// and similar operations and the ML threads send it requests to deal with
// that.  These require all the threads to pause until the operation is complete
// since they affect all memory but they are also sometimes highly recursive.
// On Mac OS X and on Linux if the stack limit is set to unlimited only the
// initial thread has a large stack and newly created threads have smaller
// stacks.  We need to make sure that any significant stack usage occurs only
// on the inital thread.
void Processes::BeginRootThread(PolyObject *rootFunction)
{
    int exitLoopCount = 100; // Maximum 100 * 400 ms.
    if (taskArray.size() < 1) {
        try {
            taskArray.push_back(0);
        } catch (std::bad_alloc&) {
            ::Exit("Unable to create the initial thread - insufficient memory");
        }
    }

    try {
        // We can't use ForkThread because we don't have a taskData object before we start
        TaskData *taskData = machineDependent->CreateTaskData();
        Handle threadRef = MakeVolatileWord(taskData, taskData);
        taskData->threadObject = (ThreadObject*)alloc(taskData, sizeof(ThreadObject) / sizeof(PolyWord), F_MUTABLE_BIT);
        taskData->threadObject->threadRef = threadRef->Word();
        // The initial thread is set to accept broadcast interrupt requests
        // and handle them synchronously.  This is for backwards compatibility.
        taskData->threadObject->flags = TAGGED(PFLAG_BROADCAST|PFLAG_ASYNCH); // Flags
        taskData->threadObject->threadLocal = TAGGED(0); // Empty thread-local store
        taskData->threadObject->requestCopy = TAGGED(0); // Cleared interrupt state
        taskData->threadObject->mlStackSize = TAGGED(0); // Unlimited stack size
        for (unsigned i = 0; i < sizeof(taskData->threadObject->debuggerSlots)/sizeof(PolyWord); i++)
            taskData->threadObject->debuggerSlots[i] = TAGGED(0);
#if defined(HAVE_WINDOWS_H)
        taskData->threadHandle = mainThreadHandle;
#endif
        taskArray[0] = taskData;

        taskData->stack = gMem.NewStackSpace(machineDependent->InitialStackSize());
        if (taskData->stack == 0)
            ::Exit("Unable to create the initial thread - insufficient memory");

        taskData->InitStackFrame(taskData, taskData->saveVec.push(rootFunction));

        // Create a packet for the Interrupt exception once so that we don't have to
        // allocate when we need to raise it.
        // We can only do this once the taskData object has been created.
        if (interrupt_exn == 0)
            interrupt_exn = makeExceptionPacket(taskData, EXC_interrupt);

        if (singleThreaded)
        {
            // If we don't have threading enter the code as if this were a new thread.
            // This will call finish so will never return.
            NewThreadFunction(taskData);
        }

        schedLock.Lock();
        int errorCode = 0;
#if (!defined(_WIN32))
        if (pthread_create(&taskData->threadId, NULL, NewThreadFunction, taskData) != 0)
            errorCode = errno;
#else
        taskData->threadHandle =
            CreateThread(NULL, 0, NewThreadFunction, taskData, 0, NULL);
        if (taskData->threadHandle == NULL) errorCode = GetLastError();
#endif
        if (errorCode != 0)
        {
            // Thread creation failed.
            taskArray[0] = 0;
            delete(taskData);
            ExitWithError("Unable to create initial thread:", errorCode);
        }

        if (debugOptions & DEBUG_THREADS)
            Log("THREAD: Forked initial root thread %p\n", taskData);
    }
    catch (std::bad_alloc &) {
        ::Exit("Unable to create the initial thread - insufficient memory");
    }

    // Wait until the threads terminate or make a request.
    // We only release schedLock while waiting.
    while (1)
    {
        // Look at the threads to see if they are running.
        bool allStopped = true;
        bool noUserThreads = true;
        bool signalThreadRunning = false;
        for (std::vector<TaskData*>::iterator i = taskArray.begin(); i != taskArray.end(); i++)
        {
            TaskData *p = *i;
            if (p)
            {
                if (p == sigTask) signalThreadRunning = true;
                else if (! p->threadExited) noUserThreads = false;

                if (p->inMLHeap)
                {
                    allStopped = false;
                    // It must be running - interrupt it if we are waiting.
                    if (threadRequest != 0) p->InterruptCode();
                }
                else if (p->threadExited) // Has the thread terminated?
                {
                    // Wait for it to actually stop then delete the task data.
#if (!defined(_WIN32))
                    pthread_join(p->threadId, NULL);
#else
                    WaitForSingleObject(p->threadHandle, INFINITE);
#endif
                    // The thread ref is no longer valid.
                    *(TaskData**)(p->threadObject->threadRef.AsObjPtr()) = 0;
                    delete(p); // Delete the task Data
                    *i = 0;
                    globalStats.decCount(PSC_THREADS);
                }
            }
        }
        if (noUserThreads)
        {
            // If all threads apart from the signal thread have exited then
            // we can finish but we must make sure that the signal thread has
            // exited before we finally finish and deallocate the memory.
            if (signalThreadRunning) exitRequest = true;
            else break; // Really no threads.
        }

        if (allStopped && threadRequest != 0)
        {
            mainThreadPhase = threadRequest->mtp;
            gcProgressBeginOtherGC(); // The default unless we're doing a GC.
            gMem.ProtectImmutable(false); // GC, sharing and export may all write to the immutable area
            threadRequest->Perform();
            gMem.ProtectImmutable(true);
            mainThreadPhase = MTP_USER_CODE;
            gcProgressReturnToML();
            threadRequest->completed = true;
            threadRequest = 0; // Allow a new request.
            mlThreadWait.Signal();
        }

        // Have we had a request to stop?  This may have happened while in the GC.
        if (exitRequest)
        {
            // Set this to kill the threads.
            for (std::vector<TaskData*>::iterator i = taskArray.begin(); i != taskArray.end(); i++)
            {
                TaskData *taskData = *i;
                if (taskData && taskData->requests != kRequestKill)
                    MakeRequest(taskData, kRequestKill);
            }
            // Leave exitRequest set so that if we're in the process of
            // creating a new thread we will request it to stop when the
            // taskData object has been added to the table.
        }

        // Now release schedLock and wait for a thread
        // to wake us up or for the timer to expire to update the statistics.
        if (! initialThreadWait.WaitFor(&schedLock, 400))
        {
            // We didn't receive a request in the last 400ms
            if (exitRequest)
            {
                if (--exitLoopCount < 0)
                {
                    // The loop count has expired and there is at least one thread that hasn't exited.
                    // Assume we've deadlocked.
#if defined(HAVE_WINDOWS_H)
                    ExitProcess(1);
#else
                    _exit(1); // Something is stuck.  Get out without calling destructors.
#endif
                }
            }
        }
        // Update the periodic stats.
        // Calculate the free memory.  We have to be careful here because although
        // we have the schedLock we don't have any lock that prevents a thread
        // from allocating a new segment.  Since these statistics are only
        // very rough it doesn't matter if there's a glitch.
        // One possibility would be see if the value of
        // gMem.GetFreeAllocSpace() has changed from what it was at the
        // start and recalculate if it has.
        // We also count the number of threads in ML code.  Taking the
        // lock in EnterPolyCode on every RTS call turned out to be
        // expensive.
        uintptr_t freeSpace = 0;
        unsigned threadsInML = 0;
        for (std::vector<TaskData*>::iterator j = taskArray.begin(); j != taskArray.end(); j++)
        {
            TaskData *taskData = *j;
            if (taskData)
            {
                // This gets the values last time it was in the RTS.
                PolyWord *limit = taskData->allocLimit, *ptr = taskData->allocPointer;
                if (limit < ptr && (uintptr_t)(ptr-limit) < taskData->allocSize)
                    freeSpace += ptr-limit;
                if (taskData->inMLHeap) threadsInML++;
            }
        }
        // Add the space in the allocation areas after calculating the sizes for the
        // threads in case a thread has allocated some more.
        freeSpace += gMem.GetFreeAllocSpace();
        globalStats.updatePeriodicStats(freeSpace, threadsInML);

        // Process the profile queue if necessary.
        processProfileQueue();
    }
    schedLock.Unlock();
    finish(exitResult); // Close everything down and exit.
}

// Create a new thread.  Returns the ML thread identifier object if it succeeds.
// May raise an exception.
Handle Processes::ForkThread(TaskData *taskData, Handle threadFunction, PolyWord flags, PolyWord stacksize)
{
    if (singleThreaded)
        raise_exception_string(taskData, EXC_thread, "Threads not available");

    try {
        // Create a taskData object for the new thread
        TaskData *newTaskData = machineDependent->CreateTaskData();
        // We allocate the thread object in the PARENT's space
        Handle threadRef = MakeVolatileWord(taskData, newTaskData);
        Handle threadId = alloc_and_save(taskData, sizeof(ThreadObject) / sizeof(PolyWord), F_MUTABLE_BIT);
        newTaskData->threadObject = (ThreadObject*)DEREFHANDLE(threadId);
        newTaskData->threadObject->threadRef = threadRef->Word();
        newTaskData->threadObject->flags = flags; // Flags
        newTaskData->threadObject->threadLocal = TAGGED(0); // Empty thread-local store
        newTaskData->threadObject->requestCopy = TAGGED(0); // Cleared interrupt state
        newTaskData->threadObject->mlStackSize = stacksize;
        for (unsigned i = 0; i < sizeof(newTaskData->threadObject->debuggerSlots)/sizeof(PolyWord); i++)
            newTaskData->threadObject->debuggerSlots[i] = TAGGED(0);

        unsigned thrdIndex;
        schedLock.Lock();
        // Before forking a new thread check to see whether we have been asked
        // to exit.  Processes::Exit sets the current set of threads to exit but won't
        // see a new thread.
        if (taskData->requests == kRequestKill)
        {
            schedLock.Unlock();
            // Raise an exception although the thread may exit before we get there.
            raise_exception_string(taskData, EXC_thread, "Thread is exiting");
        }

        // See if there's a spare entry in the array.
        for (thrdIndex = 0;
             thrdIndex < taskArray.size() && taskArray[thrdIndex] != 0;
             thrdIndex++);

        if (thrdIndex == taskArray.size()) // Need to expand the array
        {
            try {
                taskArray.push_back(newTaskData);
            } catch (std::bad_alloc&) {
                delete(newTaskData);
                schedLock.Unlock();
                raise_exception_string(taskData, EXC_thread, "Too many threads");
            }
        }
        else
        {
            taskArray[thrdIndex] = newTaskData;
        }
        schedLock.Unlock();

        newTaskData->stack = gMem.NewStackSpace(machineDependent->InitialStackSize());
        if (newTaskData->stack == 0)
        {
            delete(newTaskData);
            raise_exception_string(taskData, EXC_thread, "Unable to allocate thread stack");
        }

        // Allocate anything needed for the new stack in the parent's heap.
        // The child still has inMLHeap set so mustn't GC.
        newTaskData->InitStackFrame(taskData, threadFunction);

        // Now actually fork the thread.
        bool success = false;
        schedLock.Lock();
#if (!defined(_WIN32))
        success = pthread_create(&newTaskData->threadId, NULL, NewThreadFunction, newTaskData) == 0;
#else
        newTaskData->threadHandle =
            CreateThread(NULL, 0, NewThreadFunction, newTaskData, 0, NULL);
        success = newTaskData->threadHandle != NULL;
#endif
        if (success)
        {
            schedLock.Unlock();

            if (debugOptions & DEBUG_THREADS)
                Log("THREAD: Forking new thread %p from thread %p\n", newTaskData, taskData);

            return threadId;
        }
        // Thread creation failed.
        taskArray[thrdIndex] = 0;
        delete(newTaskData);
        schedLock.Unlock();

        if (debugOptions & DEBUG_THREADS)
            Log("THREAD: Fork from thread %p failed\n", taskData);

        raise_exception_string(taskData, EXC_thread, "Thread creation failed");
    }
    catch (std::bad_alloc &) {
            raise_exception_string(taskData, EXC_thread, "Insufficient memory");
    }
}

POLYUNSIGNED PolyThreadForkThread(POLYUNSIGNED threadId, POLYUNSIGNED function, POLYUNSIGNED attrs, POLYUNSIGNED stack)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedFunction = taskData->saveVec.push(function);
    Handle result = 0;

    try {
        result = processesModule.ForkThread(taskData, pushedFunction, PolyWord::FromUnsigned(attrs), PolyWord::FromUnsigned(stack));
    }
    catch (KillException &) {
        processes->ThreadExit(taskData); // TestSynchronousRequests may test for kill
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Deal with any interrupt or kill requests.
bool Processes::ProcessAsynchRequests(TaskData *taskData)
{
    bool wasInterrupted = false;
    TaskData *ptaskData = taskData;

    schedLock.Lock();

    switch (ptaskData->requests)
    {
    case kRequestNone:
        schedLock.Unlock();
        break;

    case kRequestInterrupt:
        {
            // Handle asynchronous interrupts only.
            // We've been interrupted.  
            POLYUNSIGNED attrs = ThreadAttrs(ptaskData);
            POLYUNSIGNED intBits = attrs & PFLAG_INTMASK;
            if (intBits == PFLAG_ASYNCH || intBits == PFLAG_ASYNCH_ONCE)
            {
                if (intBits == PFLAG_ASYNCH_ONCE)
                {
                    // Set this so from now on it's synchronous.
                    // This word is only ever set by the thread itself so
                    // we don't need to synchronise.
                    attrs = (attrs & (~PFLAG_INTMASK)) | PFLAG_SYNCH;
                    ptaskData->threadObject->flags = TAGGED(attrs);
                }
                ptaskData->requests = kRequestNone; // Clear this
                ptaskData->threadObject->requestCopy = TAGGED(0); // And in the ML copy
                schedLock.Unlock();
                // Don't actually throw the exception here.
                taskData->SetException(interrupt_exn);
                wasInterrupted = true;
            }
            else schedLock.Unlock();
        }
        break;

    case kRequestKill: // The thread has been asked to stop.
        schedLock.Unlock();
        throw KillException();
        // Doesn't return.
    }

#ifndef HAVE_WINDOWS_H
    // Start the profile timer if needed.
    if (profileMode == kProfileTime)
    {
        if (! ptaskData->runningProfileTimer)
        {
            ptaskData->runningProfileTimer = true;
            StartProfilingTimer();
        }
    }
    else ptaskData->runningProfileTimer = false;
    // The timer will be stopped next time it goes off.
#endif
    return wasInterrupted;
}

// If this thread is processing interrupts synchronously and has been
// interrupted clear the interrupt and raise the exception.  This is
// called from IO routines which may block.
void Processes::TestSynchronousRequests(TaskData *taskData)
{
    TaskData *ptaskData = taskData;
    schedLock.Lock();
    switch (ptaskData->requests)
    {
    case kRequestNone:
        schedLock.Unlock();
        break;

    case kRequestInterrupt:
        {
            // Handle synchronous interrupts only.
            // We've been interrupted.  
            POLYUNSIGNED attrs = ThreadAttrs(ptaskData);
            POLYUNSIGNED intBits = attrs & PFLAG_INTMASK;
            if (intBits == PFLAG_SYNCH)
            {
                ptaskData->requests = kRequestNone; // Clear this
                ptaskData->threadObject->requestCopy = TAGGED(0);
                schedLock.Unlock();
                taskData->SetException(interrupt_exn);
                throw IOException();
            }
            else schedLock.Unlock();
        }
        break;

    case kRequestKill: // The thread has been asked to stop.
        schedLock.Unlock();
        throw KillException();
        // Doesn't return.
    }
}

// Check for asynchronous or synchronous events
void Processes::TestAnyEvents(TaskData *taskData)
{
    TestSynchronousRequests(taskData);
    if (ProcessAsynchRequests(taskData))
        throw IOException();
}

// Request that the process should exit.
// This will usually be called from an ML thread as a result of
// a call to OS.Process.exit but on Windows it can be called from the GUI thread.
void Processes::RequestProcessExit(int n)
{
    if (singleThreaded)
        finish(n);

    exitResult = n;
    exitRequest = true;
    PLocker lock(&schedLock); // Lock so we know the main thread is waiting
    initialThreadWait.Signal(); // Wake it if it's sleeping.
}

#if !defined(HAVE_WINDOWS_H)
// N.B. This may be called either by an ML thread or by the main thread.
// On the main thread taskData will be null.
static void catchVTALRM(SIG_HANDLER_ARGS(sig, context))
{
    ASSERT(sig == SIGVTALRM);
    if (profileMode != kProfileTime)
    {
        // We stop the timer for this thread on the next signal after we end profile
        static struct itimerval stoptime = {{0, 0}, {0, 0}};
        /* Stop the timer */
        setitimer(ITIMER_VIRTUAL, & stoptime, NULL);
    }
    else {
        TaskData *taskData = processes->GetTaskDataForThread();
        handleProfileTrap(taskData, (SIGNALCONTEXT*)context);
    }
}

#else /* Windows including Cygwin */
// This runs as a separate thread.  Every millisecond it checks the CPU time used
// by each ML thread and increments the count for each thread that has used a
// millisecond of CPU time.

static bool testCPUtime(HANDLE hThread, LONGLONG &lastCPUTime)
{
    FILETIME cTime, eTime, kTime, uTime;
    // Try to get the thread CPU time if possible.  This isn't supported
    // in Windows 95/98 so if it fails we just include this thread anyway.
    if (GetThreadTimes(hThread, &cTime, &eTime, &kTime, &uTime))
    {
        LONGLONG totalTime = 0;
        LARGE_INTEGER li;
        li.LowPart = kTime.dwLowDateTime;
        li.HighPart = kTime.dwHighDateTime;
        totalTime += li.QuadPart;
        li.LowPart = uTime.dwLowDateTime;
        li.HighPart = uTime.dwHighDateTime;
        totalTime += li.QuadPart;
        if (totalTime - lastCPUTime >= 10000)
        {
            lastCPUTime = totalTime;
            return true;
        }
        return false;
    }
    else return true; // Failed to get thread time, maybe Win95.
 }


void Processes::ProfileInterrupt(void)
{               
    // Wait for millisecond or until the stop event is signalled.
    while (WaitForSingleObject(hStopEvent, 1) == WAIT_TIMEOUT)        
    {
        // We need to hold schedLock to examine the taskArray but
        // that is held during garbage collection.
        if (schedLock.Trylock())
        {
            for (std::vector<TaskData*>::iterator i = taskArray.begin(); i != taskArray.end(); i++)
            {
                TaskData *p = *i;
                if (p && p->threadHandle)
                {
                    if (testCPUtime(p->threadHandle, p->lastCPUTime))
                    {
                        CONTEXT context;
                        SuspendThread(p->threadHandle);
                        context.ContextFlags = CONTEXT_CONTROL; /* Get Eip and Esp */
                        if (GetThreadContext(p->threadHandle, &context))
                        {
                            handleProfileTrap(p, &context);
                        }
                        ResumeThread(p->threadHandle);
                    }
                }
            }
            schedLock.Unlock();
        }
        // Check the CPU time used by the main thread.  This is used for GC
        // so we need to check that as well.
        if (testCPUtime(mainThreadHandle, lastCPUTime))
            handleProfileTrap(NULL, NULL);
    }
}

DWORD WINAPI ProfilingTimer(LPVOID parm)
{
    processesModule.ProfileInterrupt();
    return 0;
}

#endif

// Profiling control.  Called by the root thread.
void Processes::StartProfiling(void)
{
#ifdef HAVE_WINDOWS_H
    DWORD threadId;
    extern FILE *polyStdout;
    if (profilingHd)
        return;
    ResetEvent(hStopEvent);
    profilingHd = CreateThread(NULL, 0, ProfilingTimer, NULL, 0, &threadId);
    if (profilingHd == NULL)
    {
        fputs("Creating ProfilingTimer thread failed.\n", polyStdout);
        return;
    }
    /* Give this a higher than normal priority so it pre-empts the main
       thread.  Without this it will tend only to be run when the main
       thread blocks for some reason. */
    SetThreadPriority(profilingHd, THREAD_PRIORITY_ABOVE_NORMAL);
#else
    // In Linux, at least, we need to run a timer in each thread.
    // We request each to enter the RTS so that it will start the timer.
    // Since this is being run by the main thread while all the ML threads
    // are paused this may not actually be necessary.
    for (std::vector<TaskData*>::iterator i = taskArray.begin(); i != taskArray.end(); i++)
    {
        TaskData *taskData = *i;
        if (taskData)
        {
            taskData->InterruptCode();
        }
    }
    StartProfilingTimer(); // Start the timer in the root thread.
#endif
}

void Processes::StopProfiling(void)
{
#ifdef HAVE_WINDOWS_H
    if (hStopEvent) SetEvent(hStopEvent);
    // Wait for the thread to stop
    if (profilingHd)
    {
        WaitForSingleObject(profilingHd, 10000);
        CloseHandle(profilingHd);
    }
    profilingHd = NULL;
#endif
}

// Called by the ML signal handling thread.  It blocks until a signal
// arrives.  There should only be a single thread waiting here.
bool Processes::WaitForSignal(TaskData *taskData, PLock *sigLock)
{
    TaskData *ptaskData = taskData;
    // We need to hold the signal lock until we have acquired schedLock.
    PLocker lock(&schedLock);
    sigLock->Unlock();
    if (sigTask != 0)
    {
        return false;
    }
    sigTask = ptaskData;

    if (ptaskData->requests == kRequestNone)
    {
        // Now release the ML memory.  A GC can start.
        ThreadReleaseMLMemoryWithSchedLock(ptaskData);
        globalStats.incCount(PSC_THREADS_WAIT_SIGNAL);
        ptaskData->threadLock.Wait(&schedLock);
        globalStats.decCount(PSC_THREADS_WAIT_SIGNAL);
        // We want to use the memory again.
        ThreadUseMLMemoryWithSchedLock(ptaskData);
    }

    sigTask = 0;
    return true;
}

// Called by the signal detection thread to wake up the signal handler
// thread.  Must be called AFTER releasing sigLock.
void Processes::SignalArrived(void)
{
    PLocker locker(&schedLock);
    if (sigTask)
        sigTask->threadLock.Signal();
}

#if (!defined(_WIN32))
// This is called when the thread exits in foreign code and
// ThreadExit has not been called.
static void threaddata_destructor(void *p)
{
    TaskData *pt = (TaskData *)p;
    pt->threadExited = true;
    // This doesn't actually wake the main thread and relies on the
    // regular check to release the task data.
}
#endif

void Processes::Init(void)
{
#if (!defined(_WIN32))
    pthread_key_create(&tlsId, threaddata_destructor);
#else
    tlsId = TlsAlloc();
#endif

#if defined(HAVE_WINDOWS_H) /* Windows including Cygwin. */
    // Create stop event for time profiling.
    hStopEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
    // Get the thread handle for this thread.
    HANDLE thisProcess = GetCurrentProcess();
    DuplicateHandle(thisProcess, GetCurrentThread(), thisProcess, 
        &mainThreadHandle, THREAD_ALL_ACCESS, FALSE, 0);
#else
    // Set up a signal handler.  This will be the same for all threads.
    markSignalInuse(SIGVTALRM);
    setSignalHandler(SIGVTALRM, catchVTALRM);
#endif
}

#ifndef HAVE_WINDOWS_H
// On Linux, at least, each thread needs to run this.
void Processes::StartProfilingTimer(void)
{
    // set virtual timer to go off every millisecond
    struct itimerval starttime;
    starttime.it_interval.tv_sec = starttime.it_value.tv_sec = 0;
    starttime.it_interval.tv_usec = starttime.it_value.tv_usec = 1000;
    setitimer(ITIMER_VIRTUAL,&starttime,NULL);
}
#endif

void Processes::Stop(void)
{     
#if (!defined(_WIN32))
    pthread_key_delete(tlsId);
#else
    TlsFree(tlsId);
#endif

#if defined(HAVE_WINDOWS_H)
    /* Stop the timer and profiling threads. */
    if (hStopEvent) SetEvent(hStopEvent);
    if (profilingHd)
    {
        WaitForSingleObject(profilingHd, 10000);
        CloseHandle(profilingHd);
        profilingHd = NULL;
    }
    if (hStopEvent) CloseHandle(hStopEvent);
    hStopEvent = NULL;
    if (mainThreadHandle) CloseHandle(mainThreadHandle);
    mainThreadHandle = NULL;
#else
    profileMode = kProfileOff;
    // Make sure the timer is not running
    struct itimerval stoptime;
    memset(&stoptime, 0, sizeof(stoptime));
    setitimer(ITIMER_VIRTUAL, &stoptime, NULL);
#endif
}

void Processes::GarbageCollect(ScanAddress *process)
/* Ensures that all the objects are retained and their addresses updated. */
{   
    /* The interrupt exn */
    if (interrupt_exn != 0) {
        PolyObject *p = interrupt_exn;
        process->ScanRuntimeAddress(&p, ScanAddress::STRENGTH_STRONG);
        interrupt_exn = (PolyException*)p;
    }
    for (std::vector<TaskData*>::iterator i = taskArray.begin(); i != taskArray.end(); i++)
    {
        if (*i)
            (*i)->GarbageCollect(process);
    }
}

void TaskData::GarbageCollect(ScanAddress *process)
{
    saveVec.gcScan(process);

    if (threadObject != 0)
    {
        PolyObject *p = threadObject;
        process->ScanRuntimeAddress(&p, ScanAddress::STRENGTH_STRONG);
        threadObject = (ThreadObject*)p;
    }
    if (blockMutex != 0)
        process->ScanRuntimeAddress(&blockMutex, ScanAddress::STRENGTH_STRONG);
    // The allocation spaces are no longer valid.
    allocPointer = 0;
    allocLimit = 0;
    // Divide the allocation size by four. If we have made a single allocation
    // since the last GC the size will have been doubled after the allocation.
    // On average for each thread, apart from the one that ran out of space
    // and requested the GC, half of the space will be unused so reducing by
    // four should give a good estimate for next time.
    if (allocCount != 0)
    { // Do this only once for each GC.
        allocCount = 0;
        allocSize = allocSize/4;
        if (allocSize < MIN_HEAP_SIZE)
            allocSize = MIN_HEAP_SIZE;
    }
}

// Return the number of processors.
extern unsigned NumberOfProcessors(void)
{
#if (defined(_WIN32))
        SYSTEM_INFO info;
        memset(&info, 0, sizeof(info));
        GetSystemInfo(&info);
        if (info.dwNumberOfProcessors == 0) // Just in case
            info.dwNumberOfProcessors = 1;
        return info.dwNumberOfProcessors;
#elif(defined(_SC_NPROCESSORS_ONLN))
        long res = sysconf(_SC_NPROCESSORS_ONLN);
        if (res <= 0) res = 1;
        return res;
#elif(defined(HAVE_SYSCTL) && defined(CTL_HW) && defined(HW_NCPU))
        static int mib[2] = { CTL_HW, HW_NCPU };
        int nCPU = 1;
        size_t len = sizeof(nCPU);
        if (sysctl(mib, 2, &nCPU, &len, NULL, 0) == 0 && len == sizeof(nCPU))
             return nCPU;
        else return 1;
#else
        // Can't determine.
        return 1;
#endif
}

// Return the number of physical processors.  If hyperthreading is
// enabled this returns less than NumberOfProcessors.  Returns zero if
// it cannot be determined.
// This can be used in Cygwin as well as native Windows.
#if (defined(HAVE_SYSTEM_LOGICAL_PROCESSOR_INFORMATION))
typedef BOOL (WINAPI *GETP)(SYSTEM_LOGICAL_PROCESSOR_INFORMATION*, PDWORD);

// Windows - use GetLogicalProcessorInformation if it's available.
static unsigned WinNumPhysicalProcessors(void)
{
    GETP getProcInfo = (GETP) GetProcAddress(GetModuleHandle(_T("kernel32")), "GetLogicalProcessorInformation");
    if (getProcInfo == 0) return 0;

    // It's there - use it.
    SYSTEM_LOGICAL_PROCESSOR_INFORMATION *buff = 0;
    DWORD space = 0;
    while (getProcInfo(buff, &space) == FALSE)
    {
        if (GetLastError() != ERROR_INSUFFICIENT_BUFFER)
        {
            free(buff);
            return 0;
        }
        free(buff);
        buff = (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION)malloc(space);
        if (buff == 0) return 0;
    }
    // Calculate the number of full entries in case it's truncated.
    unsigned nItems = space / sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION);
    unsigned numProcs = 0;
    for (unsigned i = 0; i < nItems; i++)
    {
        if (buff[i].Relationship == RelationProcessorCore)
            numProcs++;
    }
    free(buff);
    return numProcs;
}
#endif

// Read and parse /proc/cpuinfo
static unsigned LinuxNumPhysicalProcessors(void)
{
    // Find out the total.  This should be the maximum.
    unsigned nProcs = NumberOfProcessors();
    // If there's only one we don't need to check further.
    if (nProcs <= 1) return nProcs;
    long *cpus = (long*)calloc(nProcs, sizeof(long));
    if (cpus == 0) return 0;

    FILE *cpuInfo = fopen("/proc/cpuinfo", "r");
    if (cpuInfo == NULL) { free(cpus); return 0; }

    char line[40];
    unsigned count = 0;
    while (fgets(line, sizeof(line), cpuInfo) != NULL)
    {
        if (strncmp(line, "core id\t\t:", 10) == 0)
        {
            long n = strtol(line+10, NULL, 10);
            unsigned i = 0;
            // Skip this id if we've seen it already
            while (i < count && cpus[i] != n) i++;
            if (i == count) cpus[count++] = n;
        }
        if (strchr(line, '\n') == 0)
        {
            int ch;
            do { ch = getc(cpuInfo); } while (ch != '\n' && ch != EOF);
        }
    }

    fclose(cpuInfo);
    free(cpus);
    return count;
}

extern unsigned NumberOfPhysicalProcessors(void)
{
    unsigned numProcs = 0;
#if (defined(HAVE_SYSTEM_LOGICAL_PROCESSOR_INFORMATION))
    numProcs = WinNumPhysicalProcessors();
    if (numProcs != 0) return numProcs;
#endif
#if (defined(HAVE_SYSCTLBYNAME) && defined(HAVE_SYS_SYSCTL_H))
    // Mac OS X
    int nCores;
    size_t len = sizeof(nCores);
    if (sysctlbyname("hw.physicalcpu", &nCores, &len, NULL, 0) == 0)
        return (unsigned)nCores;
#endif
    numProcs = LinuxNumPhysicalProcessors();
    if (numProcs != 0) return numProcs;
    // Any other cases?
    return numProcs;
}
