/*
    Title:      Mutex and Condition Variable library.

    Copyright (c) 2007, 2012, 2015, 2019 David C. J. Matthews

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

#if (!defined(_WIN32))
// Configure requires pthread unless this is native Windows.
#include <pthread.h>
#else
#include <windows.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#if (defined(HAVE_SEMAPHORE_H) && !defined(_WIN32))
// Don't include semaphore.h on Mingw.  It's provided but doesn't compile.
#include <semaphore.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#include "locking.h"
#include "diagnostics.h"

// Report contended locks after this many attempts
#define LOCK_REPORT_COUNT   50

PLock::PLock(const char *n): lockName(n), lockCount(0)
{
#if (!defined(_WIN32))
    pthread_mutex_init(&lock, 0);
#else
    InitializeCriticalSection(&lock);
#endif
}

PLock::~PLock()
{
#if (!defined(_WIN32))
    pthread_mutex_destroy(&lock);
#else
    DeleteCriticalSection(&lock);
#endif
}

void PLock::Lock(void)
{
    if (debugOptions & DEBUG_CONTENTION)
    {
        // Report a heavily contended lock.
        if (Trylock())
            return;
        if (++lockCount > LOCK_REPORT_COUNT)
        {
            if (lockName != 0)
                Log("Lock: contention on lock: %s\n", lockName);
            else
                Log("Lock: contention on lock at %p\n", &lock);
            lockCount = 0;
        }
        // Drop through to a normal lock
    }
#if (!defined(_WIN32))
    pthread_mutex_lock(&lock);
#else
    EnterCriticalSection(&lock);
#endif
}

void PLock::Unlock(void)
{
#if (!defined(_WIN32))
    pthread_mutex_unlock(&lock);
#else
    LeaveCriticalSection(&lock);
#endif
}

bool PLock::Trylock(void)
{
#if (!defined(_WIN32))
    // Since we use normal mutexes this returns EBUSY if the
    // current thread owns the mutex.
    return pthread_mutex_trylock(&lock) != EBUSY;
#else
    // This is not implemented properly in Windows.  There is
    // TryEnterCriticalSection in Win NT and later but that
    // returns TRUE if the current thread owns the mutex.
   return TryEnterCriticalSection(&lock) == TRUE;
#endif
}

PCondVar::PCondVar()
{
#if (!defined(_WIN32))
    pthread_cond_init(&cond, NULL);
#else
    InitializeConditionVariable(&cond);
#endif
}

PCondVar::~PCondVar()
{
#if (!defined(_WIN32))
    pthread_cond_destroy(&cond);
#endif
}

// Wait indefinitely.  Drops the lock and reaquires it.
void PCondVar::Wait(PLock *pLock)
{
#if (!defined(_WIN32))
    pthread_cond_wait(&cond, &pLock->lock);
#else
    SleepConditionVariableCS(&cond, &pLock->lock, INFINITE);
#endif
}

// Wait until a specified absolute time.  Drops the lock and reaquires it.
#if (defined(_WIN32))
// Windows with Windows-style times
void PCondVar::WaitUntil(PLock *pLock, const FILETIME *time)
{
    FILETIME now;
    GetSystemTimeAsFileTime(&now);
    LARGE_INTEGER liNow, liTime;
    liNow.HighPart = now.dwHighDateTime;
    liNow.LowPart = now.dwLowDateTime;
    liTime.HighPart = time->dwHighDateTime;
    liTime.LowPart = time->dwLowDateTime;
    if (liNow.QuadPart >= liTime.QuadPart) // Already past the time
        return;
    DWORD toWait = (DWORD)((liTime.QuadPart - liNow.QuadPart) / (LONGLONG)10000);
    (void)WaitFor(pLock, toWait);
}
#else
// Unix-style times
void PCondVar::WaitUntil(PLock *pLock, const timespec *time)
{
    pthread_cond_timedwait(&cond, &pLock->lock, time);
}
#endif

// Wait for a number of milliseconds.  Used within the RTS.  Drops the lock and reaquires it.
// Returns true if the return was because the condition variable had been signalled.
// Returns false if the timeout expired or there was an error.
bool PCondVar::WaitFor(PLock *pLock, unsigned milliseconds)
{
#if (!defined(_WIN32))
    struct timespec waitTime;
    struct timeval tv;
    if (gettimeofday(&tv, NULL) != 0)
        return false;
    waitTime.tv_sec = tv.tv_sec + milliseconds / 1000;
    waitTime.tv_nsec = (tv.tv_usec + (milliseconds % 1000) * 1000) * 1000;
    if (waitTime.tv_nsec >= 1000*1000*1000)
    {
        waitTime.tv_nsec -= 1000*1000*1000;
        waitTime.tv_sec += 1;
    }
    return pthread_cond_timedwait(&cond, &pLock->lock, &waitTime) == 0;
#else
    // SleepConditionVariableCS returns zero on error or timeout.
    return SleepConditionVariableCS(&cond, &pLock->lock, milliseconds) != 0;
#endif
}

// Wake up all the waiting threads. 
void PCondVar::Signal(void)
{
#if (!defined(_WIN32))
    pthread_cond_broadcast(&cond);
#else
    WakeAllConditionVariable(&cond);
#endif
}


// Initialise a semphore.  Tries to create an unnamed semaphore if
// it can but tries a named semaphore if it can't.  Mac OS X only
// supports named semaphores.
// The semaphore is initialised with a count of zero.
PSemaphore::PSemaphore()
{
#if (!defined(_WIN32))
    sema = 0;
    isLocal = true;
#else
    sema = NULL;
#endif
}

PSemaphore::~PSemaphore()
{
#if (!defined(_WIN32))
#ifndef MACOSX
    if (sema && isLocal) sem_destroy(sema);
    else
#endif
        if (sema && !isLocal) sem_close(sema);
#else
    if (sema != NULL) CloseHandle(sema);
#endif
}

bool PSemaphore::Init(unsigned init, unsigned max)
{
#if (!defined(_WIN32))
#ifndef MACOSX
    isLocal = true;
    if (sem_init(&localSema, 0, init) == 0) {
        sema = &localSema;
        return true;
    }
#endif
#if (defined(__CYGWIN__))
    // Cygwin doesn't define sem_unlink but that doesn't matter
    // since sem_init works.
    sema = 0;
    return false;
#else
    isLocal = false;
    char semname[30];
    static int count=0;
    sprintf(semname, "poly%0d-%0d", (int)getpid(), count++);
    sema = sem_open(semname, O_CREAT|O_EXCL, 00666, init);
    if (sema == (sem_t*)SEM_FAILED) {
        sema = 0;
        return false;
    }
    sem_unlink(semname);
    return true;
#endif
#else
    sema = CreateSemaphore(NULL, init, max, NULL);
    return sema != NULL;
#endif
}

bool PSemaphore::Wait(void)
{
#if (!defined(_WIN32))
    // Wait until the semaphore is signalled.  A Unix signal may interrupt
    // it so we need to retry in that case.
    while (sem_wait(sema) == -1)
    {
        if (errno != EINTR)
            return false;
    }
    return true;
#else
    return WaitForSingleObject(sema, INFINITE) == WAIT_OBJECT_0;
#endif
}

void PSemaphore::Signal(void)
{
#if (!defined(_WIN32))
    sem_post(sema);
#else
    ReleaseSemaphore(sema, 1, NULL);
#endif
}



