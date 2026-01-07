/*
    Title:      Time functions.
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further development copyright David C.J. Matthews 2011,12,16,19-20

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

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
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

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
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
#define ASSERT(x) 0
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

#include <limits>
// Windows headers define min/max macros, which messes up trying to use std::numeric_limits<T>::min/max()
#ifdef min
#undef min
#endif
#ifdef max
#undef max
#endif

#include "locking.h"
#include "globals.h"
#include "arb.h"
#include "run_time.h"
#include "sys.h"
#include "timing.h"
#include "polystring.h"
#include "save_vec.h"
#include "rts_module.h"
#include "processes.h"
#include "heapsizing.h"
#include "rtsentry.h"
#include "mpoly.h" // For polyStderr

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTimingTicksPerMicroSec(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTimingGetNow(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTimingBaseYear(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTimingYearOffset(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTimingLocalOffset(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTimingSummerApplies(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTimingConvertDateStuct(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTimingGetUser(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTimingGetSystem(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTimingGetGCUser(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTimingGetReal(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTimingGetChildUser(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTimingGetChildSystem(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTimingGetGCSystem(POLYUNSIGNED threadId);
}

#if (defined(_WIN32))
/* Windows file times are 64-bit numbers representing times in
   tenths of a microsecond. */
#define TICKS_PER_MICROSECOND 10

#ifdef __GNUC__
#define SECSSINCE1601 11644473600LL
#else
#define SECSSINCE1601 11644473600
#endif

#else
/* For Unix return times in microseconds. */
#define TICKS_PER_MICROSECOND 1
#endif

/*
    The original Poly timing functions used a variety of timing bases
    (e.g. seconds, tenths of a second).  The old functions have been
    retained but the intention is to phase them out in favour of new
    functions.  Most of these are handled through the timing_dispatch
    function.

    The intention behind the timing functions is to make use of the
    arbitrary precision arithmetic to allow for a wider range of dates
    than the usual mktime range of 1970 to 2036.  We also want to handle
    more accurate timing than per second or per microsecond where the
    operating system provides it.
*/

#if (defined(_WIN32))
static FILETIME startTime;
#define StrToLL _strtoi64
#else
static struct timeval startTime;
#define StrToLL strtoll
#endif

#if(!(defined(HAVE_GMTIME_R) && defined(HAVE_LOCALTIME_R)))
// gmtime and localtime are not re-entrant so if we don't have the
// re-entrant versions we need to use a lock.
static PLock timeLock("Timing");
#endif

#define XSTR(X) STR(X)
#define STR(X)  #X


// Get ticks per microsecond.
POLYUNSIGNED PolyTimingTicksPerMicroSec(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = Make_arbitrary_precision(taskData, TICKS_PER_MICROSECOND);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Return time since the time base. */
POLYUNSIGNED PolyTimingGetNow(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
#if (defined(_WIN32))
        FILETIME ft;
        GetSystemTimeAsFileTime(&ft);
        result = Make_arb_from_Filetime(taskData, ft);
#else
        struct timeval tv;
        if (gettimeofday(&tv, NULL) != 0)
            raise_syscall(taskData, "gettimeofday failed", errno);
        result = Make_arb_from_pair_scaled(taskData, tv.tv_sec, tv.tv_usec, 1000000);
#endif
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Return the base year.  This is the year which corresponds to zero in the timing sequence. */
POLYUNSIGNED PolyTimingBaseYear(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
#if (defined(_WIN32))
        result = Make_arbitrary_precision(taskData, 1601);

#else
        result = Make_arbitrary_precision(taskData, 1970);
#endif
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* In both Windows and Unix the time base is 1st of January
    in the base year.  This function is provided just in case
    we are running on a system with a different base.  It
    returns the number of seconds after 1st January of the
    base year that corresponds to zero of the time base. */
POLYUNSIGNED PolyTimingYearOffset(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = Make_arbitrary_precision(taskData, 0);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Return the time offset which applied/will apply at the specified time (in seconds). */
POLYUNSIGNED PolyTimingLocalOffset(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        int localoff = 0;
        time_t theTime;
        int day = 0;
#if (defined(HAVE_GMTIME_R) || defined(HAVE_LOCALTIME_R))
        struct tm resultTime;
#endif
#if (defined(_WIN32))
        /* Although the offset is in seconds it is since 1601. */
        FILETIME ftSeconds; // Not really a file-time because it's a number of seconds.
        getFileTimeFromArb(taskData, pushedArg, &ftSeconds); /* May raise exception. */
        ULARGE_INTEGER   liTime;
        liTime.HighPart = ftSeconds.dwHighDateTime;
        liTime.LowPart = ftSeconds.dwLowDateTime;
        theTime = (long)(liTime.QuadPart - SECSSINCE1601);
#else
        theTime = get_C_long(taskData, DEREFWORD(pushedArg)); /* May raise exception. */
#endif

         {
#ifdef HAVE_GMTIME_R
             struct tm* loctime = gmtime_r(&theTime, &resultTime);
#else
             PLocker lock(&timeLock);
             struct tm* loctime = gmtime(&theTime);
#endif
             if (loctime == NULL) raise_exception0(taskData, EXC_size);
             localoff = (loctime->tm_hour * 60 + loctime->tm_min) * 60 + loctime->tm_sec;
             day = loctime->tm_yday;
         }

         {

#ifdef HAVE_LOCALTIME_R
             struct tm* loctime = localtime_r(&theTime, &resultTime);
#else
             PLocker lock(&timeLock);
             struct tm* loctime = localtime(&theTime);
#endif
             if (loctime == NULL) raise_exception0(taskData, EXC_size);
             localoff -= (loctime->tm_hour * 60 + loctime->tm_min) * 60 + loctime->tm_sec;
             if (loctime->tm_yday != day)
             {
                 // Different day - have to correct it.  We can assume that there
                 // is at most one day to correct.
                 if (day == loctime->tm_yday + 1 || (day == 0 && loctime->tm_yday >= 364))
                     localoff += 24 * 60 * 60;
                 else localoff -= 24 * 60 * 60;
             }
         }

        result = Make_arbitrary_precision(taskData, localoff);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Find out if Summer Time (daylight saving) was/will be in effect. */
POLYUNSIGNED PolyTimingSummerApplies(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        time_t theTime;
#if (defined(_WIN32))
        FILETIME ftSeconds; // Not really a file-time because it's a number of seconds.
        getFileTimeFromArb(taskData, pushedArg, &ftSeconds); /* May raise exception. */
        ULARGE_INTEGER   liTime;
        liTime.HighPart = ftSeconds.dwHighDateTime;
        liTime.LowPart = ftSeconds.dwLowDateTime;
        theTime = (long)(liTime.QuadPart - SECSSINCE1601);
#else
        theTime = get_C_long(taskData, DEREFWORD(pushedArg)); /* May raise exception. */
#endif
        int isDst = 0;
#ifdef HAVE_LOCALTIME_R
        struct tm resultTime;
        struct tm* loctime = localtime_r(&theTime, &resultTime);
        isDst = loctime->tm_isdst;
#else
        {
            PLocker lock(&timeLock);
            struct tm* loctime = localtime(&theTime);
            if (loctime == NULL) raise_exception0(taskData, EXC_size);
            isDst = loctime->tm_isdst;
        }
#endif
        result = Make_arbitrary_precision(taskData, isDst);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Call strftime.  It would be possible to do much of this in  ML except that it requires the current locale. */
POLYUNSIGNED PolyTimingConvertDateStuct(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        struct  tm time = {0}; // Must be zeroed because there are time-zone pointers on some platforms.
        char* format, buff[2048];
        Handle  resString;
        /* Get the format string. */
        format = Poly_string_to_C_alloc(DEREFHANDLE(pushedArg)->Get(0));

        /* Copy the time information. */
        time.tm_year = get_C_int(taskData, DEREFHANDLE(pushedArg)->Get(1)) - 1900;
        time.tm_mon = get_C_int(taskData, DEREFHANDLE(pushedArg)->Get(2));
        time.tm_mday = get_C_int(taskData, DEREFHANDLE(pushedArg)->Get(3));
        time.tm_hour = get_C_int(taskData, DEREFHANDLE(pushedArg)->Get(4));
        time.tm_min = get_C_int(taskData, DEREFHANDLE(pushedArg)->Get(5));
        time.tm_sec = get_C_int(taskData, DEREFHANDLE(pushedArg)->Get(6));
        time.tm_wday = get_C_int(taskData, DEREFHANDLE(pushedArg)->Get(7));
        time.tm_yday = get_C_int(taskData, DEREFHANDLE(pushedArg)->Get(8));
        time.tm_isdst = get_C_int(taskData, DEREFHANDLE(pushedArg)->Get(9));
#if (defined(_WIN32))
        _tzset(); /* Make sure we set the current locale. */
#else
        setlocale(LC_TIME, "");
#endif
        /* It would be better to dynamically allocate the string rather
           than use a fixed size but Unix unlike Windows does not distinguish
           between an error in the input and the buffer being too small. */
        if (strftime(buff, sizeof(buff), format, &time) <= 0)
        {
            /* Error */
            free(format);
            raise_exception0(taskData, EXC_size);
        }
        resString = taskData->saveVec.push(C_string_to_Poly(taskData, buff));
        free(format);
        result = resString;
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Return User CPU time since the start. */
POLYUNSIGNED PolyTimingGetUser(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
#if (defined(_WIN32))
        FILETIME ut, ct, et, kt;
        if (!GetProcessTimes(GetCurrentProcess(), &ct, &et, &kt, &ut))
            raise_syscall(taskData, "GetProcessTimes failed", GetLastError());
        result = Make_arb_from_Filetime(taskData, ut);
#else
        struct rusage rusage;
        if (getrusage(RUSAGE_SELF, &rusage) != 0)
            raise_syscall(taskData, "getrusage failed", errno);
        result = Make_arb_from_pair_scaled(taskData, rusage.ru_utime.tv_sec,
            rusage.ru_utime.tv_usec, 1000000);
#endif
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Return System CPU time since the start. */
POLYUNSIGNED PolyTimingGetSystem(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
#if (defined(_WIN32))
        FILETIME ct, et, kt, ut;
        if (!GetProcessTimes(GetCurrentProcess(), &ct, &et, &kt, &ut))
            raise_syscall(taskData, "GetProcessTimes failed", GetLastError());
        result = Make_arb_from_Filetime(taskData, kt);
#else
        struct rusage rusage;
        if (getrusage(RUSAGE_SELF, &rusage) != 0)
            raise_syscall(taskData, "getrusage failed", errno);
        result = Make_arb_from_pair_scaled(taskData, rusage.ru_stime.tv_sec,
            rusage.ru_stime.tv_usec, 1000000);
#endif
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Return GC time since the start. */
POLYUNSIGNED PolyTimingGetGCUser(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = gHeapSizeParameters.getGCUtime(taskData);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Return real time since the start. */
POLYUNSIGNED PolyTimingGetReal(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
#if (defined(_WIN32))
        FILETIME ft;
        GetSystemTimeAsFileTime(&ft);
        subFiletimes(&ft, &startTime);
        result = Make_arb_from_Filetime(taskData, ft);
#else
        struct timeval tv;
        if (gettimeofday(&tv, NULL) != 0)
            raise_syscall(taskData, "gettimeofday failed", errno);
        subTimevals(&tv, &startTime);
        result = Make_arb_from_pair_scaled(taskData, tv.tv_sec, tv.tv_usec, 1000000);
#endif
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Return User CPU time used by child processes.  (Posix only) */
POLYUNSIGNED PolyTimingGetChildUser(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
#if (defined(_WIN32))
        result = Make_arbitrary_precision(taskData, 0);
#else
        struct rusage rusage;
        if (getrusage(RUSAGE_CHILDREN, &rusage) != 0)
            raise_syscall(taskData, "getrusage failed", errno);
        result = Make_arb_from_pair_scaled(taskData, rusage.ru_utime.tv_sec,
            rusage.ru_utime.tv_usec, 1000000);
#endif
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Return System CPU time used by child processes. (Posix only) */
POLYUNSIGNED PolyTimingGetChildSystem(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
#if (defined(_WIN32))
        result = Make_arbitrary_precision(taskData, 0);
#else
        struct rusage rusage;
        if (getrusage(RUSAGE_CHILDREN, &rusage) != 0)
            raise_syscall(taskData, "getrusage failed", errno);
        result = Make_arb_from_pair_scaled(taskData, rusage.ru_stime.tv_sec,
            rusage.ru_stime.tv_usec, 1000000);
#endif
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Return GC system time since the start. */
POLYUNSIGNED PolyTimingGetGCSystem(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = gHeapSizeParameters.getGCStime(taskData);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

#ifdef _WIN32
void addFiletimes(FILETIME *result, const FILETIME *x)
{
    ULARGE_INTEGER liA, liB;
    liA.LowPart = result->dwLowDateTime;
    liA.HighPart = result->dwHighDateTime;
    liB.LowPart = x->dwLowDateTime;
    liB.HighPart = x->dwHighDateTime;
    liA.QuadPart += liB.QuadPart;
    result->dwLowDateTime = liA.LowPart;
    result->dwHighDateTime = liA.HighPart;
}

void subFiletimes(FILETIME *result, const FILETIME *x)
{
    ULARGE_INTEGER liA, liB;
    liA.LowPart = result->dwLowDateTime;
    liA.HighPart = result->dwHighDateTime;
    liB.LowPart = x->dwLowDateTime;
    liB.HighPart = x->dwHighDateTime;
    liA.QuadPart -= liB.QuadPart;
    result->dwLowDateTime = liA.LowPart;
    result->dwHighDateTime = liA.HighPart;
}

float filetimeToSeconds(const FILETIME *x)
{
    ULARGE_INTEGER ul;
    ul.LowPart = x->dwLowDateTime;
    ul.HighPart = x->dwHighDateTime;
    return (float)ul.QuadPart / (float)1.0E7;
}

void FileTimeTime::fromSeconds(unsigned u)
{
    ULARGE_INTEGER li;
    li.QuadPart = (ULONGLONG)u * TICKS_PER_MICROSECOND * 1000000;
    t.dwLowDateTime = li.LowPart;
    t.dwHighDateTime = li.HighPart;
}

void FileTimeTime::add(const FileTimeTime &f)
{
    addFiletimes(&t, &f.t);
}

void FileTimeTime::sub(const FileTimeTime &f)
{
    subFiletimes(&t, &f.t);
}

float FileTimeTime::toSeconds(void)
{
    return filetimeToSeconds(&t);
}

#endif

#ifdef HAVE_SYS_TIME_H
void addTimevals(struct timeval *result, const struct timeval *x)
{
    long uSecs = result->tv_usec + x->tv_usec;
    result->tv_sec += x->tv_sec;
    if (uSecs >= 1000000) { result->tv_sec++; uSecs -= 1000000; }
    result->tv_usec = uSecs;
}

void subTimevals(struct timeval *result, const struct timeval *x)
{
    long uSecs = result->tv_usec - x->tv_usec;
    result->tv_sec -= x->tv_sec;
    if (uSecs < 0) { result->tv_sec--; uSecs += 1000000; }
    result->tv_usec = uSecs;
}

float timevalToSeconds(const struct timeval *x)
{
    return (float)x->tv_sec + (float)x->tv_usec / 1.0E6;
}

void TimeValTime::add(const TimeValTime &f)
{
    addTimevals(&t, &f.t);
}

void TimeValTime::sub(const TimeValTime &f)
{
    subTimevals(&t, &f.t);
}

#endif


struct _entrypts timingEPT[] =
{
    { "PolyTimingTicksPerMicroSec",     (polyRTSFunction)&PolyTimingTicksPerMicroSec},
    { "PolyTimingGetNow",               (polyRTSFunction)&PolyTimingGetNow},
    { "PolyTimingBaseYear",             (polyRTSFunction)&PolyTimingBaseYear},
    { "PolyTimingYearOffset",           (polyRTSFunction)&PolyTimingYearOffset},
    { "PolyTimingLocalOffset",          (polyRTSFunction)&PolyTimingLocalOffset},
    { "PolyTimingSummerApplies",        (polyRTSFunction)&PolyTimingSummerApplies},
    { "PolyTimingConvertDateStuct",     (polyRTSFunction)&PolyTimingConvertDateStuct},
    { "PolyTimingGetUser",              (polyRTSFunction)&PolyTimingGetUser},
    { "PolyTimingGetSystem",            (polyRTSFunction)&PolyTimingGetSystem},
    { "PolyTimingGetGCUser",            (polyRTSFunction)&PolyTimingGetGCUser},
    { "PolyTimingGetReal",              (polyRTSFunction)&PolyTimingGetReal},
    { "PolyTimingGetChildUser",         (polyRTSFunction)&PolyTimingGetChildUser},
    { "PolyTimingGetChildSystem",       (polyRTSFunction)&PolyTimingGetChildSystem},
    { "PolyTimingGetGCSystem",          (polyRTSFunction)&PolyTimingGetGCSystem},

    { NULL, NULL} // End of list.
};

class Timing: public RtsModule
{
public:
    virtual void Init(void);
};

// Declare this.  It will be automatically added to the table.
static Timing timingModule;

void Timing::Init(void)
{
#if (defined(_WIN32))
    // Record an initial time of day to use as the basis of real timing
    GetSystemTimeAsFileTime(&startTime);
#else
    gettimeofday(&startTime, NULL);
#endif
}

time_t getBuildTime(void)
{
    char* source_date_epoch = getenv("SOURCE_DATE_EPOCH");
    if (source_date_epoch) {
        errno = 0;
        char* endptr;
        long long epoch = StrToLL(source_date_epoch, &endptr, 10);
        if ((errno == ERANGE && (epoch == LLONG_MIN || epoch == LLONG_MAX)) || (errno != 0 && epoch == 0)) {
            fprintf(polyStderr, "Environment variable $SOURCE_DATE_EPOCH: " XSTR(StrToLL) ": %s\n", strerror(errno));
            goto err;
        }
        if (endptr == source_date_epoch) {
            fprintf(polyStderr, "Environment variable $SOURCE_DATE_EPOCH: No digits were found: %s\n", endptr);
            goto err;
        }
        if (*endptr != '\0') {
            fprintf(polyStderr, "Environment variable $SOURCE_DATE_EPOCH: Trailing garbage: %s\n", endptr);
            goto err;
        }
        if (epoch < (long long)std::numeric_limits<time_t>::min()) {
            fprintf(polyStderr, "Environment variable $SOURCE_DATE_EPOCH: value must be greater than or equal to: %lld but was found to be: %lld\n", (long long)std::numeric_limits<time_t>::min(), epoch);
            goto err;
        }
        if (epoch > (long long)std::numeric_limits<time_t>::max()) {
            fprintf(polyStderr, "Environment variable $SOURCE_DATE_EPOCH: value must be smaller than or equal to: %lld but was found to be: %lld\n", (long long)std::numeric_limits<time_t>::max(), epoch);
            goto err;
        }
        return (time_t)epoch;
    }
err:
    return time(NULL);
}
