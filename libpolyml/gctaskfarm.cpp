/*
    Title:      Task farm for Multi-Threaded Garbage Collector

    Copyright (c) 2010 David C. J. Matthews

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

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#include "gctaskfarm.h"
#include "diagnostics.h"
#include "timing.h"

static GCTaskId gTask;

GCTaskId *globalTask = &gTask;

GCTaskFarm::GCTaskFarm(): workLock("GC task farm work")
{
    queueSize = queueIn = queuedItems = 0;
    workQueue = 0;
    terminate = false;
    threadCount = activeThreadCount = 0;
#if (defined(HAVE_PTHREAD_H) || defined(HAVE_WINDOWS_H))
    threadHandles = 0;
#endif
}

GCTaskFarm::~GCTaskFarm()
{
    Terminate();
    free(workQueue);
#if (defined(HAVE_PTHREAD_H) || defined(HAVE_WINDOWS_H))
    free(threadHandles);
#endif
}


bool GCTaskFarm::Initialise(unsigned thrdCount, unsigned qSize)
{
    terminate = false;
    if (!waitForWork.Init(0, thrdCount)) return false;
    workQueue = (queue_entry*)calloc(qSize, sizeof(queue_entry));
    if (workQueue == 0) return false;
#if ((!defined(_WIN32) || defined(__CYGWIN__)) && defined(HAVE_PTHREAD_H))
    queueSize = qSize;
    threadHandles = (pthread_t*)calloc(thrdCount, sizeof(pthread_t));
    if (threadHandles == 0) return false;
#elif defined(HAVE_WINDOWS_H)
    queueSize = qSize;
    threadHandles = (HANDLE*)calloc(thrdCount, sizeof(HANDLE));
    if (threadHandles == 0) return false;
#else
    queueSize = 0;
#endif
    // Create the worker threads.
    for (unsigned i = 0; i < thrdCount; i++) {
        // Fork a thread
#if ((!defined(_WIN32) || defined(__CYGWIN__)) && defined(HAVE_PTHREAD_H))
        // Create a thread that isn't joinable since we don't want to wait
        // for it to finish.
        pthread_t pthreadId;
        bool isError = pthread_create(&pthreadId, NULL, WorkerThreadFunction, this) != 0;
        if (isError) break;
        threadHandles[threadCount++] = pthreadId;
#elif defined(HAVE_WINDOWS_H)
        DWORD dwThrdId; // Have to provide this although we don't use it.
        HANDLE threadHandle =
            CreateThread(NULL, 0, WorkerThreadFunction, this, 0, &dwThrdId);
        if (threadHandle == NULL) break;
        threadHandles[threadCount++] = threadHandle;
#endif
    }

    return true;
}

void GCTaskFarm::Terminate()
{
    terminate = true;
    // Increment the semaphore by the number of threads to release them all.
    for (unsigned i = 0; i < threadCount; i++) waitForWork.Signal();
    // Wait for the threads to terminate.
#if ((!defined(_WIN32) || defined(__CYGWIN__)) && defined(HAVE_PTHREAD_H))
    for (unsigned j = 0; j < threadCount; j++)
        pthread_join(threadHandles[j], NULL);
#elif defined(HAVE_WINDOWS_H)
    if (threadCount != 0)
        WaitForMultipleObjects(threadCount, threadHandles, TRUE, 10000);
#endif
}

// Add work to the queue.  Returns true if it succeeds.
bool GCTaskFarm::AddWork(gctask work, void *arg1, void *arg2)
{
    bool wantSignal = false;
    {
        PLocker l(&workLock);
        if (queuedItems == queueSize) return false; // Queue is full
        workQueue[queueIn].task = work;
        workQueue[queueIn].arg1 = arg1;
        workQueue[queueIn].arg2 = arg2;
        queueIn++;
        if (queueIn == queueSize) queueIn = 0;
        queuedItems++;
        wantSignal = queuedItems <= threadCount;
    }
    if (wantSignal) waitForWork.Signal();
    return true;
}

// Schedule this as a task or run it immediately if the queue is full.
void GCTaskFarm::AddWorkOrRunNow(gctask work, void *arg1, void *arg2)
{
    if (! AddWork(work, arg1, arg2))
        (*work)(globalTask, arg1, arg2);
}

void GCTaskFarm::ThreadFunction()
{
    GCTaskId myTaskId;
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    DWORD startActive = GetTickCount();
#else
    struct timeval startTime;
    gettimeofday(&startTime, NULL);
#endif
    workLock.Lock();
    activeThreadCount++;
    while (! terminate) {
        // Invariant: We have the lock and the activeThreadCount includes this thread.
        // Find some work.

        if (queuedItems > 0) { // There is work
            unsigned outPos;
            if (queuedItems > queueIn)
                outPos = queueIn+queueSize-queuedItems;
            else outPos = queueIn-queuedItems;
            gctask work = workQueue[outPos].task;
            void *arg1 = workQueue[outPos].arg1;
            void *arg2 = workQueue[outPos].arg2;
            workQueue[outPos].task = 0;
            queuedItems--;
            ASSERT(work != 0);
            workLock.Unlock();
            (*work)(&myTaskId, arg1, arg2);
            workLock.Lock();
        }
        else {
            activeThreadCount--; // We're no longer active
            // If there is no work and we're the last active thread signal the
            // main thread that the queue is empty
            bool wantSignal = activeThreadCount == 0;
            if (wantSignal)
                waitForCompletion.Signal();
            // Now release the lock.  In our Windows partial implementation of
            // condition vars we assume that signalling is done with the lock
            // still held.
            workLock.Unlock();

            if (debugOptions & DEBUG_GCTASKS)
            {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
                Log("GCTask: Thread %p blocking after %u milliseconds\n", &myTaskId,
                     GetTickCount() - startActive);
#else
                struct timeval endTime;
                gettimeofday(&endTime, NULL);
                subTimevals(&endTime, &startTime);
                Log("GCTask: Thread %p blocking after %0.4f seconds\n", &myTaskId,
                    (float)endTime.tv_sec + (float)endTime.tv_usec / 1.0E6);
#endif
            }

            if (terminate) return;
            // Block until there's work.
            waitForWork.Wait();
            // We've been woken up
            if (debugOptions & DEBUG_GCTASKS)
            {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
                startActive = GetTickCount();
#else
                gettimeofday(&startTime, NULL);
#endif
                Log("GCTask: Thread %p resuming\n", &myTaskId);
            }
            workLock.Lock();
            activeThreadCount++;
        }
    }
    activeThreadCount--;
    workLock.Unlock();
}

#if ((!defined(_WIN32) || defined(__CYGWIN__)) && defined(HAVE_PTHREAD_H))
void *GCTaskFarm::WorkerThreadFunction(void *parameter)
{
    GCTaskFarm *t = (GCTaskFarm *)parameter;
    t->ThreadFunction();
    return 0;
}
#elif defined(HAVE_WINDOWS_H)
DWORD WINAPI GCTaskFarm::WorkerThreadFunction(void *parameter)
{
    GCTaskFarm *t = (GCTaskFarm *)parameter;
    t->ThreadFunction();
    return 0;
}
#endif

// Wait until the queue is empty.
void GCTaskFarm::WaitForCompletion(void)
{
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    DWORD startWait;
    if (debugOptions & DEBUG_GCTASKS)
        startWait = GetTickCount();
#else
    struct timeval startWait;
    if (debugOptions & DEBUG_GCTASKS)
        gettimeofday(&startWait, NULL);
#endif
    workLock.Lock();
    while (activeThreadCount > 0 || queuedItems > 0)
        waitForCompletion.Wait(&workLock);
    workLock.Unlock();

    if (debugOptions & DEBUG_GCTASKS)
    {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        Log("GCTask: Threads completed after %u milliseconds\n", GetTickCount()-startWait);
#else
        struct timeval endWait;
        gettimeofday(&endWait, NULL);
        subTimevals(&endWait, &startWait);
        Log("GCTask: Threads completed after %0.4f seconds\n",
            (float)endWait.tv_sec + (float)endWait.tv_usec / 1.0E6);
#endif
    }
}
