/*
    Title:  statistics.cpp - Profiling statistics

    Copyright (c) 2011, 2013, 2015, 2019, 2020 David C.J. Matthews

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

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#if defined(HAVE_MMAP)
// How do we get the page size?
#ifndef HAVE_GETPAGESIZE
#ifdef _SC_PAGESIZE
#define getpagesize() sysconf(_SC_PAGESIZE)
#else
// If this fails we're stuck
#define getpagesize() PAGESIZE
#endif
#endif
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#if (defined(_WIN32))
#include <tchar.h>
#else
#define _T(x) x
#endif

#include <limits>
#ifdef max
#undef max
#endif

#include "run_time.h"
#include "sys.h"
#include "save_vec.h"
#include "rts_module.h"
#include "timing.h"
#include "polystring.h"
#include "processes.h"
#include "statistics.h"
#include "../polystatistics.h"
#include "rtsentry.h"
#include "arb.h"
#include "diagnostics.h"

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetUserStatsCount();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySetUserStat(POLYUNSIGNED threadId, POLYUNSIGNED index, POLYUNSIGNED value);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetLocalStats(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetRemoteStats(POLYUNSIGNED threadId, POLYUNSIGNED procId);
}

#define STATS_SPACE 4096 // Enough for all the statistics

#define ASN1_U_BOOL      1
#define ASN1_U_INT       2
#define ASN1_U_STRING    4
#define ASN1_U_NULL      5
#define ASN1_U_ENUM      10
#define ASN1_U_SEQUENCE  16

// For the moment we don't bother to interlock access to the statistics memory.
// Other processes only read the memory and at worst they may get a glitch in
// the values.

Statistics::Statistics(): accessLock("Statistics")
{
    statMemory = 0;
    memSize = 0;
    newPtr = 0;
    for (unsigned i = 0; i < N_PS_INTS; i++) counterAddrs[i] = 0;
    for (unsigned j = 0; j < N_PS_TIMES; j++) timeAddrs[j].secAddr = timeAddrs[j].usecAddr = 0;
    for (unsigned k = 0; k < N_PS_USER; k++) userAddrs[k] = 0;

    memset(&gcUserTime, 0, sizeof(gcUserTime));
    memset(&gcSystemTime, 0, sizeof(gcSystemTime));
    memset(&gcRealTime, 0, sizeof(gcRealTime));

#ifdef _WIN32
    // File mapping handle
    hFileMap  = NULL;
    exportStats = true; // Actually unused
#else
    mapFd = -1;
    mapFileName = 0;
    exportStats = false; // Don't export by default
#endif
    memSize = 0;
    statMemory = 0;
    newPtr = 0;
}

#ifdef _WIN32
// In Windows we always create shared memory for the statistics.
// If this fails just create local stats.
bool Statistics::createWindowsSharedStats()
{
    // Get the process ID to use in the shared memory name
    DWORD pid = ::GetCurrentProcessId();
    TCHAR shmName[MAX_PATH];
    wsprintf(shmName, _T(POLY_STATS_NAME) _T("%lu"), pid);

    // Create a piece of shared memory
    hFileMap = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE,
                                 0, STATS_SPACE, shmName);
    if (hFileMap == NULL) return false;

    // If it already exists it's the wrong one.
    if (GetLastError() == ERROR_ALREADY_EXISTS) 
    { 
        CloseHandle(hFileMap);
        hFileMap = NULL;
        return false;
    }

    statMemory = (unsigned char*)MapViewOfFile(hFileMap, FILE_MAP_ALL_ACCESS, 0, 0, STATS_SPACE);
    if (statMemory == NULL)
    {
        CloseHandle(hFileMap);
        hFileMap = NULL;
        return false;
    }
    memSize = STATS_SPACE;
    return true;
}
#endif

void Statistics::Init()
{
#ifdef _WIN32
    // Record an initial time of day to use as the basis of real timing
    GetSystemTimeAsFileTime(&startTime);
    createWindowsSharedStats();
#else
    // Record an initial time of day to use as the basis of real timing
    gettimeofday(&startTime, NULL);

    // On Unix we need to specify --exportstats but if we do and have a problem we exit.
    if (exportStats)
    {
        // Create the shared memory in the user's .polyml directory
        int pageSize = getpagesize();
        memSize = (STATS_SPACE + pageSize-1) & ~(pageSize-1);
        char* polyStatsDir = getenv("POLYSTATSDIR");
        if (!polyStatsDir || !createSharedStats(polyStatsDir, ""))
        {
            char* homeDir = getenv("HOME");
            if (homeDir == NULL)
                Exit("Unable to create shared statistics - HOME is not defined");
            if (!createSharedStats(homeDir, "/.polyml"))
                Exit("Unable to create shared statistics");
        }
    }
#endif
    if (statMemory == 0)
    {
        // If we just want the statistics locally.
        statMemory = (unsigned char*)calloc(STATS_SPACE, sizeof(unsigned char));
        if (statMemory == 0) return;
        memSize = STATS_SPACE;
    }
    
    // Set up the ASN1 structure in the statistics area.
    newPtr = statMemory;
    *newPtr++ = POLY_STATS_C_STATISTICS; // Context tag for statistics
    *newPtr++ = 0x82; // Extended length, 2 bytes
    *newPtr++ = 0x00; // Length is initially zero
    *newPtr++ = 0x00;

    addCounter(PSC_THREADS, POLY_STATS_ID_THREADS, "ThreadCount");
    addCounter(PSC_THREADS_IN_ML, POLY_STATS_ID_THREADS_IN_ML, "ThreadsInML");
    addCounter(PSC_THREADS_WAIT_IO, POLY_STATS_ID_THREADS_WAIT_IO, "ThreadsInIOWait");
    addCounter(PSC_THREADS_WAIT_MUTEX, POLY_STATS_ID_THREADS_WAIT_MUTEX, "ThreadsInMutexWait");
    addCounter(PSC_THREADS_WAIT_CONDVAR, POLY_STATS_ID_THREADS_WAIT_CONDVAR, "ThreadsInCondVarWait");
    addCounter(PSC_THREADS_WAIT_SIGNAL, POLY_STATS_ID_THREADS_WAIT_SIGNAL, "ThreadsInSignalWait");
    addCounter(PSC_GC_FULLGC, POLY_STATS_ID_GC_FULLGC, "FullGCCount");
    addCounter(PSC_GC_PARTIALGC, POLY_STATS_ID_GC_PARTIALGC, "PartialGCCount");
    addCounter(PSC_GC_SHARING, POLY_STATS_ID_GC_SHARING, "GCSharingCount");
    addCounter(PSC_GC_STATE, POLY_STATS_ID_GC_STATE, "GCState");
    addCounter(PSC_GC_PERCENT, POLY_STATS_ID_GC_PERCENT, "GCPercent");

    addSize(PSS_TOTAL_HEAP, POLY_STATS_ID_TOTAL_HEAP, "TotalHeap");
    addSize(PSS_AFTER_LAST_GC, POLY_STATS_ID_AFTER_LAST_GC, "HeapAfterLastGC");
    addSize(PSS_AFTER_LAST_FULLGC, POLY_STATS_ID_AFTER_LAST_FULLGC, "HeapAfterLastFullGC");
    addSize(PSS_ALLOCATION, POLY_STATS_ID_ALLOCATION, "AllocationSpace");
    addSize(PSS_ALLOCATION_FREE, POLY_STATS_ID_ALLOCATION_FREE, "AllocationSpaceFree");
    addSize(PSS_CODE_SPACE, POLY_STATS_ID_CODE_SPACE, "CodeSpace");
    addSize(PSS_STACK_SPACE, POLY_STATS_ID_STACK_SPACE, "StackSpace");

    addTime(PST_NONGC_UTIME, POLY_STATS_ID_NONGC_UTIME, "NonGCUserTime");
    addTime(PST_NONGC_STIME, POLY_STATS_ID_NONGC_STIME, "NonGCSystemTime");
    addTime(PST_GC_UTIME, POLY_STATS_ID_GC_UTIME, "GCUserTime");
    addTime(PST_GC_STIME, POLY_STATS_ID_GC_STIME, "GCSystemTime");
    addTime(PST_NONGC_RTIME, POLY_STATS_ID_NONGC_RTIME, "NonGCRealTime");
    addTime(PST_GC_RTIME, POLY_STATS_ID_GC_RTIME, "GCRealTime");

    addUser(0, POLY_STATS_ID_USER0, "UserCounter0");
    addUser(1, POLY_STATS_ID_USER1, "UserCounter1");
    addUser(2, POLY_STATS_ID_USER2, "UserCounter2");
    addUser(3, POLY_STATS_ID_USER3, "UserCounter3");
    addUser(4, POLY_STATS_ID_USER4, "UserCounter4");
    addUser(5, POLY_STATS_ID_USER5, "UserCounter5");
    addUser(6, POLY_STATS_ID_USER6, "UserCounter6");
    addUser(7, POLY_STATS_ID_USER7, "UserCounter7");
}

#ifndef _WIN32
// Try to create a shared memory file in the appropriate directory.
bool Statistics::createSharedStats(const char* baseName, const char* subDirName)
{
    size_t tMapSize = strlen(baseName) + strlen(subDirName) + strlen(POLY_STATS_NAME) + 100;
    TempCString tMapFileName((char*)malloc(tMapSize));
    // First construct the directory name because it may not exist.
    if (subDirName[0] != 0)
    {
         int cx = snprintf(tMapFileName, tMapSize, "%s%s", baseName, subDirName);
         if (cx < 0 || (size_t)cx >= tMapSize)
             return -1;
         mkdir(tMapFileName, 0777);
    }
    int cx = snprintf(tMapFileName, tMapSize, "%s%s/%s%d", baseName, subDirName, POLY_STATS_NAME, getpid());
    if (cx < 0 || (size_t)cx >= tMapSize)
        return -1;
    // Remove any existing file.  We're creating with 0444 so if there's an old one
    // left over from a previous crash we won't be able to reopen it.
    unlink(tMapFileName);
    // Open the file.
    mapFd = open(tMapFileName, O_RDWR | O_CREAT, 0444);
    if (mapFd == -1) return false;
    if (ftruncate(mapFd, memSize) == -1) return false;
    statMemory = (unsigned char*)mmap(0, memSize, PROT_READ | PROT_WRITE, MAP_SHARED, mapFd, 0);
    if (statMemory == MAP_FAILED) return false;
    memset(statMemory, 0, memSize);
    // Set the file name to this.
    mapFileName = tMapFileName;
    tMapFileName = 0;
    return true;
}

#endif

void Statistics::addCounter(int cEnum, unsigned statId, const char *name)
{
    // Tag header
    *newPtr++ = POLY_STATS_C_COUNTERSTAT;
    *newPtr++ = 0x00; // Initial length - overwritten at the end
    unsigned char *tagStart = newPtr;
    // First item - Id of this statistic - Implicit int
    *newPtr++ = POLY_STATS_C_IDENTIFIER;
    *newPtr++ = 0x01;
    ASSERT(statId < 128);
    *newPtr++ = statId;
    // Second item - The name
    size_t nameLength = strlen(name);
    ASSERT(nameLength < 125);
    *newPtr++ = POLY_STATS_C_NAME;
    *newPtr++ = (unsigned char)nameLength;
    for (unsigned i = 0; i < nameLength; i++) *newPtr++ = name[i];
    // Third item - the counter itself.
    // This, along with the other counters, is technically incorrect
    // for an ASN1 integer because it should not contain more than
    // one zero byte.
    *newPtr++ = POLY_STATS_C_COUNTER_VALUE;
    *newPtr++ = sizeof(POLYUNSIGNED);
    counterAddrs[cEnum] = newPtr; // This is the address
    for (unsigned j = 0; j < sizeof(POLYUNSIGNED); j++) *newPtr++ = 0;
    // Finally set the tag length and the overall size.
    size_t length = newPtr - tagStart;
    ASSERT(length < 128);
    tagStart[-1] = (unsigned char)length;
    // Set the overall size.
    length = newPtr-statMemory - 4;
    statMemory[2] = (length >> 8) & 0xff;
    statMemory[3] = length & 0xff;
}

void Statistics::addSize(int cEnum, unsigned statId, const char *name)
{
    // Tag header
    *newPtr++ = POLY_STATS_C_SIZESTAT;
    *newPtr++ = 0x00; // Initial length - overwritten at the end
    unsigned char *tagStart = newPtr;
    // First item - Id of this statistic - Implicit int
    *newPtr++ = POLY_STATS_C_IDENTIFIER;
    *newPtr++ = 0x01;
    ASSERT(statId < 128);
    *newPtr++ = statId;
    // Second item - The name
    size_t nameLength = strlen(name);
    ASSERT(nameLength < 125);
    *newPtr++ = POLY_STATS_C_NAME;
    *newPtr++ = (unsigned char)nameLength;
    for (unsigned i = 0; i < nameLength; i++) *newPtr++ = name[i];
    // Third item - the size value itself.  We have to allow one
    // byte extra to ensure that the value we encode is unsigned.
    unsigned bytes = sizeof(size_t) + 1;
    *newPtr++ = POLY_STATS_C_BYTE_COUNT;
    *newPtr++ = bytes;
    counterAddrs[cEnum] = newPtr; // This is the address
    for (unsigned j = 0; j < bytes; j++) *newPtr++ = 0;
    // Finally set the tag length and the overall size.
    size_t length = newPtr - tagStart;
    ASSERT(length < 128);
    tagStart[-1] = (unsigned char)length;
    // Set the overall size.
    length = newPtr-statMemory - 4;
    statMemory[2] = (length >> 8) & 0xff;
    statMemory[3] = length & 0xff;
}

void Statistics::addTime(int cEnum, unsigned statId, const char *name)
{
    // Tag header
    *newPtr++ = POLY_STATS_C_TIMESTAT;
    *newPtr++ = 0x00; // Initial length - overwritten at the end
    unsigned char *tagStart = newPtr;
    // First item - Id of this statistic - Implicit int
    *newPtr++ = POLY_STATS_C_IDENTIFIER;
    *newPtr++ = 0x01;
    ASSERT(statId < 128);
    *newPtr++ = statId;
    // Second item - The name
    size_t nameLength = strlen(name);
    ASSERT(nameLength < 125);
    *newPtr++ = POLY_STATS_C_NAME;
    *newPtr++ = (unsigned char)nameLength;
    for (unsigned i = 0; i < nameLength; i++) *newPtr++ = name[i];
    // Third item - the time.  Two four byte values.
    *newPtr++ = POLY_STATS_C_TIME;
    *newPtr++ = 12;
    *newPtr++ = POLY_STATS_C_SECONDS;
    *newPtr++ = 4;
    timeAddrs[cEnum].secAddr = newPtr; // This is the address
    for (unsigned j = 0; j < 4; j++) *newPtr++ = 0;
    *newPtr++ = POLY_STATS_C_MICROSECS;
    *newPtr++ = 4;
    timeAddrs[cEnum].usecAddr = newPtr; // This is the address
    for (unsigned k = 0; k < 4; k++) *newPtr++ = 0;
    // Finally set the tag length and the overall size.
    size_t length = newPtr - tagStart;
    ASSERT(length < 128);
    tagStart[-1] = (unsigned char)length;
    // Set the overall size.
    length = newPtr-statMemory - 4;
    statMemory[2] = (length >> 8) & 0xff;
    statMemory[3] = length & 0xff;
}

void Statistics::addUser(int n, unsigned statId, const char *name)
{
    // Tag header
    *newPtr++ = POLY_STATS_C_USERSTAT;
    *newPtr++ = 0x00; // Initial length - overwritten at the end
    unsigned char *tagStart = newPtr;
    // First item - Id of this statistic - Implicit int
    *newPtr++ = POLY_STATS_C_IDENTIFIER;
    *newPtr++ = 0x01;
    ASSERT(statId < 128);
    *newPtr++ = statId;
    // Second item - The name
    size_t nameLength = strlen(name);
    ASSERT(nameLength < 125);
    *newPtr++ = POLY_STATS_C_NAME;
    *newPtr++ = (unsigned char)nameLength;
    for (unsigned i = 0; i < nameLength; i++) *newPtr++ = name[i];
    // Third item - the counter itself.  For a user counter the value is a POLYSIGNED.
    *newPtr++ = POLY_STATS_C_COUNTER_VALUE;
    *newPtr++ = sizeof(POLYSIGNED);
    userAddrs[n] = newPtr; // This is the address
    for (unsigned j = 0; j < sizeof(POLYSIGNED); j++) *newPtr++ = 0;
    // Finally set the tag length and the overall size.
    size_t length = newPtr - tagStart;
    ASSERT(length < 128);
    tagStart[-1] = (unsigned char)length;
    // Set the overall size.
    length = newPtr-statMemory - 4;
    statMemory[2] = (length >> 8) & 0xff;
    statMemory[3] = length & 0xff;
}

Statistics::~Statistics()
{
#ifdef _WIN32
    if (hFileMap != NULL)
    {
        if (statMemory != NULL) ::UnmapViewOfFile(statMemory);
        ::CloseHandle(hFileMap);
        statMemory = NULL;
    }
#else
    if (mapFileName != 0)
    {
        if (statMemory != 0 && statMemory != MAP_FAILED) munmap(statMemory, memSize);
        if (mapFd != -1) close(mapFd);
        if (mapFileName != 0) unlink(mapFileName);
        free(mapFileName);
        statMemory = NULL;
    }
#endif
    if (statMemory)
        free(statMemory);
}

// Counters.  These are used for thread state so need interlocks
void Statistics::incCount(int which)
{
    if (statMemory && counterAddrs[which])
    {
        PLocker lock(&accessLock);
        unsigned length = counterAddrs[which][-1];
        while (length--)
        {
            if ((++counterAddrs[which][length]) != 0)
                break;
        }
    }
}

void Statistics::decCount(int which)
{
    if (statMemory && counterAddrs[which])
    {
        PLocker lock(&accessLock);
        unsigned length = counterAddrs[which][-1];
        while (length--)
        {
            if ((counterAddrs[which][length]--) != 0)
                break;
        }

    }
}

// This is only used for the GC progress which could really fit in a single byte.
void Statistics::setCount(int which, POLYUNSIGNED count)
{
    if (statMemory && counterAddrs[which])
    {
        PLocker lock(&accessLock);
        unsigned length = counterAddrs[which][-1];
        while (length--)
        {
            counterAddrs[which][length] = (unsigned char)(count & 0xff);
            count = count >> 8;
        }
    }
}

// Sizes.  Some of these are only set during GC so may not need interlocks
size_t Statistics::getSizeWithLock(int which)
{
    unsigned length = counterAddrs[which][-1];
    size_t result = 0;
    for (unsigned i = 0; i < length; i++)
        result = (result << 8) | counterAddrs[which][i];
    return result;
}

void Statistics::setSizeWithLock(int which, size_t s)
{
    unsigned length = counterAddrs[which][-1];
    while (length--)
    {
        counterAddrs[which][length] = (unsigned char)(s & 0xff);
        s = s >> 8;
    }
}

void Statistics::setSize(int which, size_t s)
{
    if (statMemory && counterAddrs[which])
    {
        PLocker lock(&accessLock);
        setSizeWithLock(which, s);
    }
}

void Statistics::incSize(int which, size_t s)
{
    if (statMemory && counterAddrs[which])
    {
        PLocker lock(&accessLock);
        setSizeWithLock(which, getSizeWithLock(which) + s);
    }
}

void Statistics::decSize(int which, size_t s)
{
    if (statMemory && counterAddrs[which])
    {
        PLocker lock(&accessLock);
        setSizeWithLock(which, getSizeWithLock(which) - s);
    }
}

size_t Statistics::getSize(int which)
{
    if (statMemory && counterAddrs[which])
    {
        PLocker lock(&accessLock);
        return getSizeWithLock(which);
    }
    else return 0;
}

void Statistics::setTimeValue(int which, unsigned long secs, unsigned long usecs)
{
    if (statMemory && timeAddrs[which].secAddr && timeAddrs[which].usecAddr)
    {
        PLocker lock(&accessLock); // Necessary ???
        unsigned sLength = timeAddrs[which].secAddr[-1];
        while (sLength--)
        {
            timeAddrs[which].secAddr[sLength] = (unsigned char)(secs & 0xff);
            secs = secs >> 8;
        }
        unsigned usLength = timeAddrs[which].usecAddr[-1];
        while (usLength--)
        {
            timeAddrs[which].usecAddr[usLength] = (unsigned char)(usecs & 0xff);
            usecs = usecs >> 8;
        }
    }
}

#if (defined(_WIN32))
// Native Windows
void Statistics::copyGCTimes(const FILETIME &gcUtime, const FILETIME &gcStime, const FILETIME &gcRtime)
{
    gcUserTime = gcUtime;
    gcSystemTime = gcStime;
    ULARGE_INTEGER li;
    li.LowPart = gcUtime.dwLowDateTime;
    li.HighPart = gcUtime.dwHighDateTime;
    setTimeValue(PST_GC_UTIME, (unsigned long)(li.QuadPart / 10000000), (unsigned long)((li.QuadPart / 10) % 1000000));
    li.LowPart = gcStime.dwLowDateTime;
    li.HighPart = gcStime.dwHighDateTime;
    setTimeValue(PST_GC_STIME, (unsigned long)(li.QuadPart / 10000000), (unsigned long)((li.QuadPart / 10) % 1000000));
    li.LowPart = gcRtime.dwLowDateTime;
    li.HighPart = gcRtime.dwHighDateTime;
    setTimeValue(PST_GC_RTIME, (unsigned long)(li.QuadPart / 10000000), (unsigned long)((li.QuadPart / 10) % 1000000));
}
#else
// Unix
void Statistics::copyGCTimes(const struct timeval &gcUtime, const struct timeval &gcStime, const struct timeval &gcRtime)
{
    gcUserTime = gcUtime;
    gcSystemTime = gcStime;
    setTimeValue(PST_GC_UTIME, gcUtime.tv_sec, gcUtime.tv_usec);
    setTimeValue(PST_GC_STIME, gcStime.tv_sec, gcStime.tv_usec);
    setTimeValue(PST_GC_RTIME, gcRtime.tv_sec, gcRtime.tv_usec);
}
#endif

// Update the statistics that are not otherwise copied.  Called from the
// root thread every second.
void Statistics::updatePeriodicStats(size_t freeWords, unsigned threadsInML)
{
    setSize(PSS_ALLOCATION_FREE, freeWords*sizeof(PolyWord));

#if (defined(_WIN32))
    FILETIME ct, et, st, ut, rt;
    GetProcessTimes(GetCurrentProcess(), &ct, &et, &st, &ut);
    GetSystemTimeAsFileTime(&rt);
    subFiletimes(&st, &gcSystemTime);
    subFiletimes(&ut, &gcUserTime);
    subFiletimes(&rt, &startTime);
    subFiletimes(&rt, &gcRealTime);
    ULARGE_INTEGER li;
    li.LowPart = ut.dwLowDateTime;
    li.HighPart = ut.dwHighDateTime;
    setTimeValue(PST_NONGC_UTIME, (unsigned long)(li.QuadPart / 10000000), (unsigned long)((li.QuadPart / 10) % 1000000));
    li.LowPart = st.dwLowDateTime;
    li.HighPart = st.dwHighDateTime;
    setTimeValue(PST_NONGC_STIME, (unsigned long)(li.QuadPart / 10000000), (unsigned long)((li.QuadPart / 10) % 1000000));
    li.LowPart = rt.dwLowDateTime;
    li.HighPart = rt.dwHighDateTime;
    setTimeValue(PST_NONGC_RTIME, (unsigned long)(li.QuadPart / 10000000), (unsigned long)((li.QuadPart / 10) % 1000000));
#else
    struct rusage usage;
    struct timeval tv;
    getrusage(RUSAGE_SELF, &usage);
    gettimeofday(&tv, NULL);
    subTimevals(&usage.ru_stime, &gcSystemTime);
    subTimevals(&usage.ru_utime, &gcUserTime);
    subTimevals(&tv, &startTime);
    subTimevals(&tv, &gcRealTime);
    setTimeValue(PST_NONGC_UTIME, usage.ru_utime.tv_sec,  usage.ru_utime.tv_usec);
    setTimeValue(PST_NONGC_STIME, usage.ru_stime.tv_sec,  usage.ru_stime.tv_usec);
    setTimeValue(PST_NONGC_RTIME, tv.tv_sec, tv.tv_usec);
#endif

    if (statMemory && counterAddrs[PSC_THREADS_IN_ML])
    {
        PLocker lock(&accessLock);
        unsigned length = counterAddrs[PSC_THREADS_IN_ML][-1];
        while (length--)
        {
            counterAddrs[PSC_THREADS_IN_ML][length] = (unsigned char)(threadsInML & 0xff);
            threadsInML = threadsInML >> 8;
        }
    }
}

void Statistics::setUserCounter(unsigned which, POLYSIGNED value)
{
    if (statMemory && userAddrs[which])
    {
        PLocker lock(&accessLock); // Not really needed
        // The ASN1 int is big-endian
        unsigned length = userAddrs[which][-1];
        while (length--)
        {
            userAddrs[which][length] = (unsigned char)value;
            value = value >> 8;
        }
    }
}

Handle Statistics::returnStatistics(TaskData *taskData, const unsigned char *stats, size_t size)
{
    // Just return the memory as a string i.e. Word8Vector.vector.
    return taskData->saveVec.push(C_string_to_Poly(taskData, (const char*)stats, size));
}

// Copy the local statistics into the buffer
Handle Statistics::getLocalStatistics(TaskData *taskData)
{
    if (statMemory == 0)
        raise_exception_string(taskData, EXC_Fail, "No statistics available");
    return returnStatistics(taskData, statMemory, memSize);
}

// Get statistics for a remote instance.  We don't do any locking 
Handle Statistics::getRemoteStatistics(TaskData *taskData, POLYUNSIGNED pid)
{
#ifdef _WIN32
    TCHAR shmName[MAX_PATH];
    wsprintf(shmName, _T(POLY_STATS_NAME) _T("%lu"), pid);
    HANDLE hRemMemory = OpenFileMapping(FILE_MAP_READ, FALSE, shmName);
    if (hRemMemory == NULL)
        raise_exception_string(taskData, EXC_Fail, "No statistics available");

    unsigned char *sMem = (unsigned char *)MapViewOfFile(hRemMemory, FILE_MAP_READ, 0, 0, 0);

    if (sMem == NULL)
    {
        CloseHandle(hRemMemory);
        raise_exception_string(taskData, EXC_Fail, "No statistics available");
    }
    // The size may not be the size of the statistics for this process
    // because we may be using a different version of Poly/ML.  It should
    // still be properly formatted ASN1.
    MEMORY_BASIC_INFORMATION memInfo;
    SIZE_T buffSize = VirtualQuery(sMem, &memInfo, sizeof(memInfo));
    if (buffSize == 0)
    {
        UnmapViewOfFile(sMem);
        CloseHandle(hRemMemory);
        raise_exception_string(taskData, EXC_Fail, "Unable to get statistics");
    }

    Handle result = returnStatistics(taskData, sMem, memInfo.RegionSize);

    UnmapViewOfFile(sMem);
    CloseHandle(hRemMemory);
    return result;
#else
    int remMapFd = -1;
    char* polyStatsDir = getenv("POLYSTATSDIR");
    if (polyStatsDir) remMapFd = openSharedStats(polyStatsDir, "", pid);
    if (remMapFd == -1) 
    {
        char* homeDir = getenv("HOME");
        if (homeDir) remMapFd = openSharedStats(homeDir, "/.polyml", pid);
    }
    if (remMapFd == -1)
        raise_exception_string(taskData, EXC_Fail, "No statistics available");

    struct stat statBuf;
    if (fstat(remMapFd, &statBuf) == -1)
    {
        close(remMapFd);
        raise_exception_string(taskData, EXC_Fail, "No statistics available");
    }

    TempCString statData((char*)malloc(statBuf.st_size));
    if (statData == NULL)
    {
        close(remMapFd);
        raise_exception_string(taskData, EXC_Fail, "No statistics available");
    }

    ssize_t haveRead = read(remMapFd, statData, statBuf.st_size);
    close(remMapFd);

    if (haveRead < 0)
        raise_exception_string(taskData, EXC_Fail, "No statistics available");

    return returnStatistics(taskData, (const unsigned char*)(const char *)statData, statBuf.st_size);
#endif
}

#ifndef _WIN32
// Try to open a shared statistics file
int Statistics::openSharedStats(const char* baseName, const char* subDirName, int pid)
{
    size_t remMapSize = strlen(baseName) + strlen(subDirName) + strlen(POLY_STATS_NAME) + 100;
    TempCString remMapFileName((char*)malloc(remMapSize));
    int cx = snprintf(remMapFileName, remMapSize, "%s%s/%s%d", baseName, subDirName, POLY_STATS_NAME, pid);
    if (cx < 0 || (size_t)cx >= remMapSize)
        return -1;
    // Open the file.
    return open(remMapFileName, O_RDONLY);
}
#endif

// Create the global statistics object.
Statistics globalStats;

POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetUserStatsCount()
{
    return TAGGED(N_PS_USER).AsUnsigned();
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolySetUserStat(POLYUNSIGNED threadId, POLYUNSIGNED indexVal, POLYUNSIGNED valueVal)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();

    try {
        unsigned index = get_C_unsigned(taskData, PolyWord::FromUnsigned(indexVal));
        if (index >= N_PS_USER)
            raise_exception0(taskData, EXC_subscript);
        POLYSIGNED value = getPolySigned(taskData, PolyWord::FromUnsigned(valueVal));
        globalStats.setUserCounter(index, value);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();

    return TAGGED(0).AsUnsigned();
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetLocalStats(POLYUNSIGNED threadId)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = globalStats.getLocalStatistics(taskData);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();

    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetRemoteStats(POLYUNSIGNED threadId, POLYUNSIGNED procId)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = globalStats.getRemoteStatistics(taskData, getPolyUnsigned(taskData, PolyWord::FromUnsigned(procId)));
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();

    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

struct _entrypts statisticsEPT[] =
{
    { "PolyGetUserStatsCount",            (polyRTSFunction)&PolyGetUserStatsCount },
    { "PolySetUserStat",                  (polyRTSFunction)&PolySetUserStat },
    { "PolyGetLocalStats",                (polyRTSFunction)&PolyGetLocalStats },
    { "PolyGetRemoteStats",               (polyRTSFunction)&PolyGetRemoteStats },

    { NULL, NULL } // End of list.
};
