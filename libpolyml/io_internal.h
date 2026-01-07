/*
    Title:      Data structures shared between basioio.c and network.c.

    Copyright (c) 2000, 2016, 2018-19 David C. J. Matthews
    Portions of this code are derived from the original stream io
    package copyright CUTS 1983-2000.

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

#ifndef IO_INTERNAL_H
#define IO_INTERNAL_H

// Bits to define tests and results in poll.
// These are the values passed to and from ML.
#define POLL_BIT_IN     1
#define POLL_BIT_OUT    2
#define POLL_BIT_PRI    4

// Return values from fileKind
#define FILEKIND_FILE   0
#define FILEKIND_DIR    1
#define FILEKIND_LINK   2
#define FILEKIND_TTY    3
#define FILEKIND_PIPE   4
#define FILEKIND_SKT    5
#define FILEKIND_DEV    6
#define FILEKIND_UNKNOWN 7
#define FILEKIND_ERROR  (-1)


#if (defined(_WIN32))

#include <WinSock2.h>
#include "locking.h" // For PLock

// Unlike Unix where select and poll can be used on both sockets and other
// streams, in Windows there is no single way of testing different sorts of
// streams.
class WinStreamBase
{
public:
    virtual ~WinStreamBase() {} // Quieten some warnings
    virtual int pollTest() { // Return the valid options for this descriptor
        return 0;
    }
    virtual int poll(TaskData *taskData, int test) { // Return the values set
        return 0;
    }
    // These are not currently used but could be used to poll
    // multiple sockets or streams.
    virtual SOCKET getSocket() {
        return INVALID_SOCKET;
    }
    virtual HANDLE getHandle() {
        return INVALID_HANDLE_VALUE;
    }
};

typedef enum { OPENREAD, OPENWRITE, OPENAPPEND } openMode;

// Abstract Windows stream
class WinStream : public WinStreamBase
{
public:
    virtual void closeEntry(TaskData *taskData) = 0;

    // Block for a short time until either input is possible, returning true,
    // or the time-out, which may be zero, has expired.
    virtual bool testForInput(TaskData *taskData, unsigned waitMilliSecs) = 0;

    // The same for output.
    virtual bool testForOutput(TaskData *taskData, unsigned waitMilliSecs) = 0;

    // These are really for backwards compatibility.
    virtual void waitUntilAvailable(TaskData *taskData);
    virtual void waitUntilOutputPossible(TaskData *taskData);

    virtual size_t readStream(TaskData *taskData, byte *base, size_t length) {
        unimplemented(taskData);
        return 0;
    }
    virtual uint64_t getPos(TaskData *taskData) {
        unimplemented(taskData);
        return 0;
    }
    virtual void setPos(TaskData *taskData, uint64_t pos) {
        unimplemented(taskData);
    }
    virtual uint64_t fileSize(TaskData *taskData) {
        unimplemented(taskData);
        return 0;
    }
    virtual size_t writeStream(TaskData *taskData, byte *base, size_t length) {
        unimplemented(taskData);
        return 0;
    }
    virtual int fileKind() = 0;
    static int fileTypeOfHandle(HANDLE hStream);

protected:
    void unimplemented(TaskData *taskData);
};

// Windows stream input using overlapped IO and the Windows calls.
class WinInOutStream : public WinStream
{
public:
    WinInOutStream();
    ~WinInOutStream();
    virtual void closeEntry(TaskData *taskData);
    virtual void openFile(TaskData * taskData, TCHAR *name, openMode mode, bool text);
    virtual size_t readStream(TaskData *taskData, byte *base, size_t length);

    virtual bool testForInput(TaskData *taskData, unsigned waitMilliSecs);
    virtual bool testForOutput(TaskData *taskData, unsigned waitMilliSecs);

    virtual uint64_t getPos(TaskData *taskData);
    virtual void setPos(TaskData *taskData, uint64_t pos);
    virtual uint64_t fileSize(TaskData *taskData);

    virtual size_t writeStream(TaskData *taskData, byte *base, size_t length);

    // Open on a handle.  This returns an error result rather than raising an exception
    virtual bool openHandle(HANDLE hndl, openMode mode, bool isText);

    virtual int fileKind() {
        return WinStream::fileTypeOfHandle(hStream);
    }

    virtual int pollTest() {
        // We can poll this to test for input.
        return isRead ? POLL_BIT_IN : POLL_BIT_OUT;
    }

    virtual int poll(TaskData *taskData, int test);

    virtual HANDLE getHandle() {
        return hEvent;
    }

protected:
    bool beginReading();
    void flushOut(TaskData *taskData);
    uint64_t getOverlappedPos() {
        return ((uint64_t)(overlap.OffsetHigh) << 32) + overlap.Offset;
    }
    void setOverlappedPos(uint64_t newPos) {
        overlap.Offset = (DWORD)newPos;
        overlap.OffsetHigh = (DWORD)(newPos >> 32);
    }

    bool isAvailable(TaskData *taskData);
    bool canOutput(TaskData *taskData);

protected:
    bool isRead;
    bool isText; // Remove CRs?
    byte *buffer;
    unsigned buffSize, currentInBuffer, currentPtr;
    bool endOfStream;
    HANDLE hStream;
    HANDLE hEvent;
    OVERLAPPED overlap;
    PLock lock;
};

// Create a new pipe.
extern void newPipeName(TCHAR *name);

#else

extern Handle wrapFileDescriptor(TaskData *taskData, int fd);
// Get a file descriptor and raise an exception if it is closed.
extern int getStreamFileDescriptor(TaskData *taskData, PolyWord strm);
extern int getStreamFileDescriptorWithoutCheck(PolyWord strm);

#endif

// This is used in both basicio and unix-specific
#if defined(HAVE_STRUCT_STAT_ST_ATIM)
# define STAT_SECS(stat,kind)    (stat)->st_##kind##tim.tv_sec
# define STAT_USECS(stat,kind) (((stat)->st_##kind##tim.tv_nsec + 500) / 1000)
#elif defined(HAVE_STRUCT_STAT_ST_ATIMENSEC)
# define STAT_SECS(stat,kind)    (stat)->st_##kind##time
# define STAT_USECS(stat,kind) (((stat)->st_##kind##timensec + 500) / 1000)
#elif defined(HAVE_STRUCT_STAT_ST_ATIMESPEC)
# define STAT_SECS(stat,kind)    (stat)->st_##kind##timespec.tv_sec
# define STAT_USECS(stat,kind) (((stat)->st_##kind##timespec.tv_nsec + 500) / 1000)
#elif defined(HAVE_STRUCT_STAT_ST_ATIME_N)
# define STAT_SECS(stat,kind)    (stat)->st_##kind##time
# define STAT_USECS(stat,kind) (((stat)->st_##kind##time_n + 500) / 1000)
#elif defined(HAVE_STRUCT_STAT_ST_UATIME)
# define STAT_SECS(stat,kind)    (stat)->st_##kind##time
# define STAT_USECS(stat,kind)   (stat)->st_u##kind##time
#else
# define STAT_SECS(stat,kind)    (stat)->st_##kind##time
# define STAT_USECS(stat,kind)   0
#endif

#endif
