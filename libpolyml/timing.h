/*
    Title:  Header for time functions

    Copyright (c) 2000
        Cambridge University Technical Services Limited
    Further development Copyright David C.J. Matthews 2011-12, 16, 19, 2025.

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

#ifndef _TIMING_H_DEFINED
#define _TIMING_H_DEFINED 1

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

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class TaskData;

/* time functions etc */

// Define classes for operations on time values in Windows and Posix.

#ifdef _WIN32
class FileTimeTime {
public:
    FileTimeTime() { t.dwLowDateTime = t.dwHighDateTime = 0; }
    FileTimeTime(const FILETIME f) { t = f; }
    void fromSeconds(unsigned u);
    void add(const FileTimeTime &);
    void sub(const FileTimeTime &);
    float toSeconds(void);
    operator FILETIME() const { return t; }
protected:
    FILETIME t;
};
#endif

#ifdef HAVE_SYS_TIME_H
class TimeValTime {
public:
    TimeValTime() { t.tv_sec = 0; t.tv_usec = 0; }
    TimeValTime(const timeval f) { t = f; }
    void fromSeconds(unsigned u) { t.tv_sec = u; t.tv_usec = 0; }
    void add(const TimeValTime &);
    void sub(const TimeValTime &);
    float toSeconds(void) { return (float)t.tv_sec + (float)t.tv_usec / 1.0E6; }
    operator timeval() const { return t; }
protected:
    struct timeval t;
};
#endif


#ifdef _WIN32 /* Native windows */
#define TIMEDATA FileTimeTime
#else /* Unix and Cygwin. */
#define TIMEDATA TimeValTime
#endif

#ifdef HAVE_WINDOWS_H
extern void addFiletimes(FILETIME *result, const FILETIME *x);
extern void subFiletimes(FILETIME *result, const FILETIME *x);
extern float filetimeToSeconds(const FILETIME *x);
#endif

#ifdef HAVE_SYS_TIME_H
extern void addTimevals(struct timeval *result, const struct timeval *x);
extern void subTimevals(struct timeval *result, const struct timeval *x);
extern float timevalToSeconds(const struct timeval *x);
#endif

extern struct _entrypts timingEPT[];

extern time_t getBuildTime(void);

#endif
