/*
    Title:      Basic IO for Unix and similar

    Copyright (c) 2000, 2015-2020, 2022, 2025 David C. J. Matthews

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

/*
This module replaces the old stream IO based on stdio.  It works at a
lower level with the buffering being done in ML.
Sockets are generally dealt with in network.c but it is convenient to
use the same table for them particularly since it simplifies the
implementation of "poll".
Directory operations are also included in here.
DCJM May 2000. 
*/
#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#ifdef HAVE_IO_H
#include <io.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_POLL_H
#include <poll.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#ifdef HAVE_DIRECT_H
#include <direct.h>
#endif
#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif
#include <limits>
#include <climits> // For PATH_MAX

#ifndef INFTIM
#define INFTIM (-1)
#endif

#ifdef HAVE_DIRENT_H
# include <dirent.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
#else
# define dirent direct
# define NAMLEN(dirent) (dirent)->d_namlen
# if HAVE_SYS_NDIR_H
# include <sys/ndir.h>
# endif
# if HAVE_SYS_DIR_H
# include <sys/dir.h>
# endif
# if HAVE_NDIR_H
# include <ndir.h>
# endif
#endif

#include "globals.h"
#include "basicio.h"
#include "sys.h"
#include "gc.h"
#include "run_time.h"
#include "machine_dep.h"
#include "arb.h"
#include "processes.h"
#include "diagnostics.h"
#include "io_internal.h"
#include "scanaddrs.h"
#include "polystring.h"
#include "mpoly.h"
#include "save_vec.h"
#include "rts_module.h"
#include "locking.h"
#include "rtsentry.h"
#include "timing.h"


#define TOOMANYFILES EMFILE
#define NOMEMORY ENOMEM
#define STREAMCLOSED EBADF
#define FILEDOESNOTEXIST ENOENT
#define ERRORNUMBER errno


#ifndef O_ACCMODE
#define O_ACCMODE   (O_RDONLY|O_RDWR|O_WRONLY)
#endif

#define SAVE(x) taskData->saveVec.push(x)

#ifdef _MSC_VER
// Don't tell me about ISO C++ changes.
#pragma warning(disable:4996)
#endif

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyChDir(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyBasicIOGeneral(POLYUNSIGNED threadId, POLYUNSIGNED code, POLYUNSIGNED strm, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyPollIODescriptors(POLYUNSIGNED threadId, POLYUNSIGNED streamVec, POLYUNSIGNED bitVec, POLYUNSIGNED maxMillisecs);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyPosixCreatePersistentFD(POLYUNSIGNED threadId, POLYUNSIGNED fd);
}

static bool isAvailable(TaskData *taskData, int ioDesc)
{
#ifdef __CYGWIN__
      static struct timeval poll = {0,1};
#else
      static struct timeval poll = {0,0};
#endif
      fd_set read_fds;
      int selRes;
      FD_ZERO(&read_fds);
      FD_SET(ioDesc, &read_fds);

      /* If there is something there we can return. */
      selRes = select(FD_SETSIZE, &read_fds, NULL, NULL, &poll);
      if (selRes > 0) return true; /* Something waiting. */
      else if (selRes < 0 && errno != EINTR) // Maybe another thread closed descr
          raise_syscall(taskData, "select error", ERRORNUMBER);
      else return false;
}

// The strm argument is a volatile word containing the descriptor.
// Volatiles are set to zero on entry to indicate a closed descriptor.
// Zero is a valid descriptor but -1 is not so we add 1 when storing and
// subtract 1 when loading.
// N.B.  There are also persistent descriptors created with PolyPosixCreatePersistentFD
Handle wrapFileDescriptor(TaskData *taskData, int fd)
{
    return MakeVolatileWord(taskData, fd+1);
}

// Return a file descriptor or -1 if it is invalid.
int getStreamFileDescriptorWithoutCheck(PolyWord strm)
{
    return *(intptr_t*)(strm.AsObjPtr()) -1;
}

// Most of the time we want to raise an exception if the file descriptor
// has been closed although this could be left to the system call.
int getStreamFileDescriptor(TaskData *taskData, PolyWord strm)
{
    int descr = getStreamFileDescriptorWithoutCheck(strm);
    if (descr == -1) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
    return descr;
}

/* Open a file in the required mode. */
static Handle open_file(TaskData *taskData, Handle filename, int mode, int access, int isPosix)
{
    while (true) // Repeat only with certain kinds of errors
    {
        TempString cFileName(filename->Word()); // Get file name
        if (cFileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
        int stream = open(cFileName, mode, access);

        if (stream >= 0)
        {
            if (! isPosix)
            {
                /* Set the close-on-exec flag.  We don't set this if we are being
                   called from one of the low level functions in the Posix structure.
                   I assume that if someone is using those functions they know what
                   they're doing and would expect the behaviour to be close to that
                   of the underlying function. */
                fcntl(stream, F_SETFD, 1);
            }
            return wrapFileDescriptor(taskData, stream);
        }
        switch (errno)
        {
        case EINTR: // Just try the call.  Is it possible to block here indefinitely?
            continue;
        default:
            raise_syscall(taskData, "Cannot open", ERRORNUMBER);
            /*NOTREACHED*/
            return 0;
        }
    }
}

/* Close the stream unless it is stdin or stdout or already closed. */
static Handle close_file(TaskData *taskData, Handle stream)
{
    int descr = getStreamFileDescriptorWithoutCheck(stream->Word());
    // Don't close it if it's already closed or any of the standard streams 
    if (descr > 2)
    {
        close(descr);
        *(intptr_t*)(stream->WordP()) = 0; // Mark as closed
    }

    return Make_fixed_precision(taskData, 0);
}

static void waitForAvailableInput(TaskData *taskData, Handle stream)
{
    int fd = getStreamFileDescriptor(taskData, stream->Word());
    while (!isAvailable(taskData, fd))
    {
        WaitInputFD waiter(fd);
        processes->ThreadPauseForIO(taskData, &waiter);
    }
}

/* Read into an array. */
// We can't combine readArray and readString because we mustn't compute the
// destination of the data in readArray until after any GC.
static Handle readArray(TaskData *taskData, Handle stream, Handle args, bool/*isText*/)
{
    /* The isText argument is ignored in both Unix and Windows but
       is provided for future use.  Windows remembers the mode used
       when the file was opened to determine whether to translate
       CRLF into LF. */
    // We should check for interrupts even if we're not going to block.
    processes->TestAnyEvents(taskData);

    while (1) // Loop if interrupted.
    {
        // First test to see if we have input available.
        // These tests may result in a GC if another thread is running.
        // First test to see if we have input available.
        // These tests may result in a GC if another thread is running.
        waitForAvailableInput(taskData, stream);

        // We can now try to read without blocking.
        // Actually there's a race here in the unlikely situation that there
        // are multiple threads sharing the same low-level reader.  They could
        // both detect that input is available but only one may succeed in
        // reading without blocking.  This doesn't apply where the threads use
        // the higher-level IO interfaces in ML which have their own mutexes.
        int fd = getStreamFileDescriptor(taskData, stream->Word());
        byte *base = DEREFHANDLE(args)->Get(0).AsObjPtr()->AsBytePtr();
        POLYUNSIGNED offset = getPolyUnsigned(taskData, DEREFWORDHANDLE(args)->Get(1));
        size_t length = getPolyUnsigned(taskData, DEREFWORDHANDLE(args)->Get(2));
        ssize_t haveRead = read(fd, base + offset, length);
        if (haveRead >= 0)
            return Make_fixed_precision(taskData, haveRead); // Success.
        // If it failed because it was interrupted keep trying otherwise it's an error.
        if (errno != EINTR)
            raise_syscall(taskData, "Error while reading", ERRORNUMBER);
    }
}

/* Return input as a string. We don't actually need both readArray and
   readString but it's useful to have both to reduce unnecessary garbage.
   The IO library will construct one from the other but the higher levels
   choose the appropriate function depending on need. */
static Handle readString(TaskData *taskData, Handle stream, Handle args, bool/*isText*/)
{
    size_t length = getPolyUnsigned(taskData, DEREFWORD(args));
    // We should check for interrupts even if we're not going to block.
    processes->TestAnyEvents(taskData);

    while (1) // Loop if interrupted.
    {
        // First test to see if we have input available.
        // These tests may result in a GC if another thread is running.
        waitForAvailableInput(taskData, stream);

        // We can now try to read without blocking.
        int fd = getStreamFileDescriptor(taskData, stream->Word());
        // We previously allocated the buffer on the stack but that caused
        // problems with multi-threading at least on Mac OS X because of
        // stack exhaustion.  We limit the space to 100k. */
        if (length > 102400) length = 102400;
        byte *buff = (byte*)malloc(length);
        if (buff == 0) raise_syscall(taskData, "Unable to allocate buffer", NOMEMORY);
        ssize_t haveRead = read(fd, buff, length);
        if (haveRead >= 0)
        {
            Handle result = SAVE(C_string_to_Poly(taskData, (char*)buff, haveRead));
            free(buff);
            return result;
        }
        free(buff);
        // If it failed because it was interrupted keep trying otherwise it's an error.
        if (errno != EINTR)
            raise_syscall(taskData, "Error while reading", ERRORNUMBER);
    }
}

static Handle writeArray(TaskData *taskData, Handle stream, Handle args, bool/*isText*/)
{
    /* The isText argument is ignored in both Unix and Windows but
       is provided for future use.  Windows remembers the mode used
       when the file was opened to determine whether to translate
       LF into CRLF. */
    PolyWord base = DEREFWORDHANDLE(args)->Get(0);
    POLYUNSIGNED    offset = getPolyUnsigned(taskData, DEREFWORDHANDLE(args)->Get(1));
    size_t length = getPolyUnsigned(taskData, DEREFWORDHANDLE(args)->Get(2));
    int fd = getStreamFileDescriptor(taskData, stream->Word());
    /* We don't actually handle cases of blocking on output. */
    byte *toWrite = base.AsObjPtr()->AsBytePtr();
    ssize_t haveWritten = write(fd, toWrite+offset, length);
    if (haveWritten < 0) raise_syscall(taskData, "Error while writing", ERRORNUMBER);

    return Make_fixed_precision(taskData, haveWritten);
}

// Test whether we can write without blocking.  Returns false if it will block,
// true if it will not.
static bool canOutput(TaskData *taskData, Handle stream)
{
    int fd = getStreamFileDescriptor(taskData, stream->Word());

    /* Unix - use "select" to find out if output is possible. */
#ifdef __CYGWIN__
    static struct timeval poll = {0,1};
#else
    static struct timeval poll = {0,0};
#endif
    fd_set read_fds, write_fds, except_fds;
    int sel;
    FD_ZERO(&read_fds);
    FD_ZERO(&write_fds);
    FD_ZERO(&except_fds);
    FD_SET(fd, &write_fds);
    sel = select(FD_SETSIZE,&read_fds,&write_fds,&except_fds,&poll);
    if (sel < 0 && errno != EINTR)
        raise_syscall(taskData, "select failed", ERRORNUMBER);
    return sel > 0;
}

static long seekStream(TaskData *taskData, int fd, long pos, int origin)
{
    long lpos = lseek(fd, pos, origin);
    if (lpos < 0) raise_syscall(taskData, "Position error", ERRORNUMBER);
    return lpos;
}

/* Return the number of bytes available on the device.  Works only for
   files since it is meaningless for other devices. */
static Handle bytesAvailable(TaskData *taskData, Handle stream)
{
    int fd = getStreamFileDescriptor(taskData, stream->Word());
    /* Remember our original position, seek to the end, then seek back. */
    long original = seekStream(taskData, fd, 0L, SEEK_CUR);
    long endOfStream = seekStream(taskData, fd, 0L, SEEK_END);
    if (seekStream(taskData, fd, original, SEEK_SET) != original)
        raise_syscall(taskData, "Position error", ERRORNUMBER);
    return Make_fixed_precision(taskData, endOfStream-original);
}

static Handle fileKind(TaskData *taskData, Handle stream)
{
    int fd = getStreamFileDescriptor(taskData, stream->Word());
    struct stat statBuff;
    if (fstat(fd, &statBuff) < 0) raise_syscall(taskData, "Stat failed", ERRORNUMBER);
    switch (statBuff.st_mode & S_IFMT)
    {
    case S_IFIFO:
        return Make_fixed_precision(taskData, FILEKIND_PIPE);
    case S_IFCHR:
    case S_IFBLK:
        if (isatty(fd))
            return Make_fixed_precision(taskData, FILEKIND_TTY);
        else return Make_fixed_precision(taskData, FILEKIND_DEV);
    case S_IFDIR:
        return Make_fixed_precision(taskData, FILEKIND_DIR);
    case S_IFREG:
        return Make_fixed_precision(taskData, FILEKIND_FILE);
    case S_IFLNK:
        return Make_fixed_precision(taskData, FILEKIND_LINK);
    case S_IFSOCK:
        return Make_fixed_precision(taskData, FILEKIND_SKT);
    default:
        return Make_fixed_precision(taskData, -1);
    }
}

/* Find out what polling options, if any, are allowed on this
   file descriptor.  We assume that polling is allowed on all
   descriptors, either for reading or writing depending on how
   the stream was opened. */
Handle pollTest(TaskData *taskData, Handle stream)
{
    // How do we test this?  Assume all of them.
    return Make_fixed_precision(taskData, POLL_BIT_IN|POLL_BIT_OUT|POLL_BIT_PRI);
}

// Do the polling.  Takes a vector of io descriptors, a vector of bits to test
// and a time to wait and returns a vector of results.

class WaitPoll: public Waiter{
public:
    WaitPoll(POLYUNSIGNED nDesc, struct pollfd *fds, unsigned maxMillisecs);
    virtual void Wait(unsigned maxMillisecs);
    int PollResult(void) { return pollResult; }
    int PollError(void) { return errorResult; }

private:
    int pollResult;
    int errorResult;
    unsigned maxTime;
    struct pollfd *fdVec;
    POLYUNSIGNED nDescr;
};

WaitPoll::WaitPoll(POLYUNSIGNED nDesc, struct pollfd *fds, unsigned maxMillisecs)
{
    maxTime = maxMillisecs;
    pollResult = 0;
    errorResult = 0;
    nDescr = nDesc;
    fdVec = fds;
}

void WaitPoll::Wait(unsigned maxMillisecs)
{
    // N.B. We use this for OS.Process.sleep with empty descriptor list.
    if (maxTime < maxMillisecs) maxMillisecs = maxTime;
    pollResult = poll(fdVec, nDescr, maxMillisecs);
    if (pollResult < 0) errorResult = ERRORNUMBER;
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyPollIODescriptors(POLYUNSIGNED threadId, POLYUNSIGNED streamVector, POLYUNSIGNED bitVector, POLYUNSIGNED maxMillisecs)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    POLYUNSIGNED maxMilliseconds = PolyWord::FromUnsigned(maxMillisecs).UnTaggedUnsigned();
    Handle result = 0;

    try {
        PolyObject  *strmVec = PolyWord::FromUnsigned(streamVector).AsObjPtr();
        PolyObject  *bitVec = PolyWord::FromUnsigned(bitVector).AsObjPtr();
        POLYUNSIGNED nDesc = strmVec->Length();
        ASSERT(nDesc == bitVec->Length());

        struct pollfd * fds = 0;

        if (nDesc > 0)
            fds = (struct pollfd *)alloca(nDesc * sizeof(struct pollfd));

        /* Set up the request vector. */
        for (unsigned i = 0; i < nDesc; i++)
        {
            fds[i].fd = getStreamFileDescriptor(taskData, strmVec->Get(i));
            POLYUNSIGNED bits = UNTAGGED(bitVec->Get(i));
            fds[i].events = 0;
            if (bits & POLL_BIT_IN) fds[i].events |= POLLIN; /* | POLLRDNORM??*/
            if (bits & POLL_BIT_OUT) fds[i].events |= POLLOUT;
            if (bits & POLL_BIT_PRI) fds[i].events |= POLLPRI;
            fds[i].revents = 0;
        }

        // Poll the descriptors.
        WaitPoll pollWait(nDesc, fds, maxMilliseconds);
        processes->ThreadPauseForIO(taskData, &pollWait);

        if (pollWait.PollResult() < 0)
            raise_syscall(taskData, "poll failed", pollWait.PollError());
        // Construct the result vectors.
        result = alloc_and_save(taskData, nDesc);
        for (unsigned i = 0; i < nDesc; i++)
        {
            int res = 0;
            if (fds[i].revents & POLLIN) res = POLL_BIT_IN;
            if (fds[i].revents & POLLOUT) res = POLL_BIT_OUT;
            if (fds[i].revents & POLLPRI) res = POLL_BIT_PRI;
            DEREFWORDHANDLE(result)->Set(i, TAGGED(res));
        }
    }
    catch (KillException &) {
        processes->ThreadExit(taskData); // TestAnyEvents may test for kill
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Directory functions.

static Handle openDirectory(TaskData *taskData, Handle dirname)
{
    TempString dirName(dirname->Word());
    if (dirName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    while (1)
    {
        DIR *dirp = opendir(dirName);
        if (dirp != NULL)
            return MakeVolatileWord(taskData, dirp);

        switch (errno)
        {
        case EINTR:
            continue; // Just retry the call.
        default:
            raise_syscall(taskData, "opendir failed", ERRORNUMBER);
        }
    }
}

/* Return the next entry from the directory, ignoring current and
parent arcs ("." and ".." in Windows and Unix) */
Handle readDirectory(TaskData *taskData, Handle stream)
{
    DIR *pDir = *(DIR**)(stream->WordP()); // In a Volatile
    if (pDir == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
    while (1)
    {
        struct dirent *dp = readdir(pDir);
        int len;
        if (dp == NULL) return taskData->saveVec.push(EmptyString(taskData));
        len = NAMLEN(dp);
        if (!((len == 1 && strncmp(dp->d_name, ".", 1) == 0) ||
            (len == 2 && strncmp(dp->d_name, "..", 2) == 0)))
            return SAVE(C_string_to_Poly(taskData, dp->d_name, len));
    }
}

Handle rewindDirectory(TaskData *taskData, Handle stream, Handle dirname)
{
    DIR *pDir = *(DIR**)(stream->WordP()); // In a Volatile
    if (pDir == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
    rewinddir(pDir);
    return Make_fixed_precision(taskData, 0);
}

static Handle closeDirectory(TaskData *taskData, Handle stream)
{
    DIR *pDir = *(DIR**)(stream->WordP()); // In a SysWord
    if (pDir != 0)
    {
        closedir(pDir);
        *((DIR**)stream->WordP()) = 0; // Clear this - no longer valid
    }
    return Make_fixed_precision(taskData, 0);
}

/* change_dirc - this is called directly and not via the dispatch
   function. */
static Handle change_dirc(TaskData *taskData, Handle name)
/* Change working directory. */
{
    TempString cDirName(name->Word());
    if (cDirName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    if (chdir(cDirName) != 0)
        raise_syscall(taskData, "chdir failed", ERRORNUMBER);
    return SAVE(TAGGED(0));
}

// External call
POLYUNSIGNED PolyChDir(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);

    try {
        (void)change_dirc(taskData, pushedArg);
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned(); // Result is unit
}


/* Test for a directory. */
Handle isDir(TaskData *taskData, Handle name)
{
    TempString cDirName(name->Word());
    if (cDirName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    struct stat fbuff;
    if (stat(cDirName, &fbuff) != 0)
        raise_syscall(taskData, "stat failed", ERRORNUMBER);
    if ((fbuff.st_mode & S_IFMT) == S_IFDIR)
        return Make_fixed_precision(taskData, 1);
    else return Make_fixed_precision(taskData, 0);
}

/* Get absolute canonical path name. */
Handle fullPath(TaskData *taskData, Handle filename)
{
    TempCString cFileName;
    /* Special case of an empty string. */
    if (PolyStringLength(filename->Word()) == 0) cFileName = strdup(".");
    else cFileName = Poly_string_to_C_alloc(filename->Word());
    if (cFileName == 0) raise_syscall(taskData, "Insufficient memory", ENOMEM);
    TempCString resBuf(realpath(cFileName, NULL));
    if (resBuf == NULL)
    {
#ifdef PATH_MAX
        if (errno == EINVAL)
        {
            // Some very old systems e.g. Solaris 10 don't support NULL for the second arg.
            resBuf = (char*)malloc(PATH_MAX);
            if (resBuf == NULL)
                raise_syscall(taskData, "Insufficient memory", ENOMEM);
            if (realpath(cFileName, resBuf) == NULL)
                raise_syscall(taskData, "realpath failed", errno);
        }
        else
#endif
            raise_syscall(taskData, "realpath failed", errno);
    }
    /* Some versions of Unix don't check the final component
        of a file.  To be consistent try doing a "stat" of
        the resulting string to check it exists. */
    struct stat fbuff;
    if (stat(resBuf, &fbuff) != 0)
        raise_syscall(taskData, "stat failed", errno);
    return(SAVE(C_string_to_Poly(taskData, resBuf)));
}

/* Get file modification time.  This returns the value in the
   time units and from the base date used by timing.c. c.f. filedatec */
Handle modTime(TaskData *taskData, Handle filename)
{
    TempString cFileName(filename->Word());
    if (cFileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    struct stat fbuff;
    if (stat(cFileName, &fbuff) != 0)
        raise_syscall(taskData, "stat failed", ERRORNUMBER);
    /* Convert to microseconds. */
    return Make_arb_from_pair_scaled(taskData, STAT_SECS(&fbuff,m),
                                        STAT_USECS(&fbuff,m), 1000000);
}

/* Get file size. */
Handle fileSize(TaskData *taskData, Handle filename)
{
    TempString cFileName(filename->Word());
    if (cFileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    struct stat fbuff;
    if (stat(cFileName, &fbuff) != 0)
        raise_syscall(taskData, "stat failed", ERRORNUMBER);
    return Make_arbitrary_precision(taskData, fbuff.st_size);
}

/* Set file modification and access times. */
Handle setTime(TaskData *taskData, Handle fileName, Handle fileTime)
{
    TempString cFileName(fileName->Word());
    if (cFileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    struct timeval times[2];
    /* We have a value in microseconds.  We need to split
        it into seconds and microseconds. */
    Handle hTime = fileTime;
    Handle hMillion = Make_arbitrary_precision(taskData, 1000000);
    /* N.B. Arguments to div_longc and rem_longc are in reverse order. */
    unsigned secs =
        get_C_ulong(taskData, DEREFWORD(div_longc(taskData, hMillion, hTime)));
    unsigned usecs =
        get_C_ulong(taskData, DEREFWORD(rem_longc(taskData, hMillion, hTime)));
    times[0].tv_sec = times[1].tv_sec = secs;
    times[0].tv_usec = times[1].tv_usec = usecs;
    if (utimes(cFileName, times) != 0)
        raise_syscall(taskData, "utimes failed", ERRORNUMBER);
    return Make_fixed_precision(taskData, 0);
}

/* Rename a file. */
Handle renameFile(TaskData *taskData, Handle oldFileName, Handle newFileName)
{
    TempString oldName(oldFileName->Word()), newName(newFileName->Word());
    if (oldName == 0 || newName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    if (rename(oldName, newName) != 0)
        raise_syscall(taskData, "rename failed", ERRORNUMBER);
    return Make_fixed_precision(taskData, 0);
}

/* Access right requests passed in from ML. */
#define FILE_ACCESS_READ    1
#define FILE_ACCESS_WRITE   2
#define FILE_ACCESS_EXECUTE 4

/* Get access rights to a file. */
Handle fileAccess(TaskData *taskData, Handle name, Handle rights)
{
    TempString fileName(name->Word());
    if (fileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    int rts = get_C_int(taskData, DEREFWORD(rights));
    int mode = 0;
    if (rts & FILE_ACCESS_READ) mode |= R_OK;
    if (rts & FILE_ACCESS_WRITE) mode |= W_OK;
    if (rts & FILE_ACCESS_EXECUTE) mode |= X_OK;
    if (mode == 0) mode = F_OK;
    /* Return true if access is allowed, otherwise false
        for any other error. */
    if (access(fileName, mode) == 0)
        return Make_fixed_precision(taskData, 1);
    else return Make_fixed_precision(taskData, 0);
}



/* IO_dispatchc.  Called from assembly code module. */
static Handle IO_dispatch_c(TaskData *taskData, Handle args, Handle strm, Handle code)
{
    unsigned c = get_C_unsigned(taskData, DEREFWORD(code));
    switch (c)
    {
    case 0: /* Return standard input */
        return wrapFileDescriptor(taskData, 0);
    case 1: /* Return standard output */
        return wrapFileDescriptor(taskData, 1);
    case 2: /* Return standard error */
       return wrapFileDescriptor(taskData, 2);
    case 3: /* Open file for text input. */
    case 4: /* Open file for binary input. */
        return open_file(taskData, args, O_RDONLY, 0666, 0);
    case 5: /* Open file for text output. */
    case 6: /* Open file for binary output. */
        return open_file(taskData, args, O_WRONLY | O_CREAT | O_TRUNC, 0666, 0);
    case 7: /* Close file */
        return close_file(taskData, strm);
    case 8: /* Read text into an array. */
        return readArray(taskData, strm, args, true);
    case 9: /* Read binary into an array. */
        return readArray(taskData, strm, args, false);
    case 10: /* Get text as a string. */
        return readString(taskData, strm, args, true);
    case 11: /* Write from memory into a text file. */
        return writeArray(taskData, strm, args, true);
    case 12: /* Write from memory into a binary file. */
        return writeArray(taskData, strm, args, false);
    case 13: /* Open text file for appending. */
        /* The IO library definition leaves it open whether this
           should use "append mode" or not.  */
    case 14: /* Open binary file for appending. */
        return open_file(taskData, args, O_WRONLY | O_CREAT | O_APPEND, 0666, 0);
    case 15: /* Return recommended buffer size. */
        // This is a guess but 4k seems reasonable.
        return Make_fixed_precision(taskData, 4096);

    case 16: /* See if we can get some input. */
        {
            int fd = getStreamFileDescriptor(taskData, strm->Word());
            return Make_fixed_precision(taskData, isAvailable(taskData, fd) ? 1 : 0);
        }

    case 17: /* Return the number of bytes available.  */
        return bytesAvailable(taskData, strm);

    case 18: /* Get position on stream. */
        {
            /* Get the current position in the stream.  This is used to test
               for the availability of random access so it should raise an
               exception if setFilePos or endFilePos would fail. */
            int fd = getStreamFileDescriptor(taskData, strm->Word());
            long pos = seekStream(taskData, fd, 0L, SEEK_CUR);
            return Make_arbitrary_precision(taskData, pos);
        }

    case 19: /* Seek to position on stream. */
        {
            long position = (long)get_C_long(taskData, DEREFWORD(args));
            int fd = getStreamFileDescriptor(taskData, strm->Word());
            (void)seekStream(taskData, fd, position, SEEK_SET);
            return Make_arbitrary_precision(taskData, 0);
        }

    case 20: /* Return position at end of stream. */
        {
            int fd = getStreamFileDescriptor(taskData, strm->Word());
            /* Remember our original position, seek to the end, then seek back. */
            long original = seekStream(taskData, fd, 0L, SEEK_CUR);
            long endOfStream = seekStream(taskData, fd, 0L, SEEK_END);
            if (seekStream(taskData, fd, original, SEEK_SET) != original)
                raise_syscall(taskData, "Position error", ERRORNUMBER);
            return Make_arbitrary_precision(taskData, endOfStream);
        }

    case 21: /* Get the kind of device underlying the stream. */
        return fileKind(taskData, strm);
    case 22: /* Return the polling options allowed on this descriptor. */
        return pollTest(taskData, strm);
//    case 23: /* Poll the descriptor, waiting forever. */
//        return pollDescriptors(taskData, args, 1);
//    case 24: /* Poll the descriptor, waiting for the time requested. */
//        return pollDescriptors(taskData, args, 0);
//    case 25: /* Poll the descriptor, returning immediately.*/
//        return pollDescriptors(taskData, args, 2);
    case 26: /* Get binary as a vector. */
        return readString(taskData, strm, args, false);

    case 27: /* Block until input is available. */
        // We should check for interrupts even if we're not going to block.
        processes->TestAnyEvents(taskData);
        waitForAvailableInput(taskData, strm);
        return Make_fixed_precision(taskData, 0);

    case 28: /* Test whether output is possible. */
        return Make_fixed_precision(taskData, canOutput(taskData, strm) ? 1:0);

    case 29: /* Block until output is possible. */
        // We should check for interrupts even if we're not going to block.
        processes->TestAnyEvents(taskData);
        while (true) {
            if (canOutput(taskData, strm))
                return Make_fixed_precision(taskData, 0);
            // Use the default waiter for the moment since we don't have
            // one to test for output.
            processes->ThreadPauseForIO(taskData, Waiter::defaultWaiter);
        }

        /* Functions added for Posix structure. */
    case 30: /* Return underlying file descriptor. */
        /* This is now also used internally to test for
           stdIn, stdOut and stdErr. */
        {
            int fd = getStreamFileDescriptor(taskData, strm->Word());
            return Make_fixed_precision(taskData, fd);
        }

    case 31: /* Make an entry for a given descriptor.  No longer used - previously used for Posix.FileSys.wordToFD. */
        {
            int ioDesc = get_C_int(taskData, DEREFWORD(args));
            return wrapFileDescriptor(taskData, ioDesc);
        }


    /* Directory functions. */
    case 50: /* Open a directory. */
        return openDirectory(taskData, args);

    case 51: /* Read a directory entry. */
        return readDirectory(taskData, strm);

    case 52: /* Close the directory */
        return closeDirectory(taskData, strm);

    case 53: /* Rewind the directory. */
        return rewindDirectory(taskData, strm, args);

    case 54: /* Get current working directory. */
        {
            size_t size = 4096;
            TempString string_buffer((char *)malloc(size * sizeof(char)));
            if (string_buffer == NULL) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            char *cwd;
            while ((cwd = getcwd(string_buffer, size)) == NULL && errno == ERANGE) {
                if (size > std::numeric_limits<size_t>::max() / 2) raise_fail(taskData, "getcwd needs too large a buffer");
                size *= 2;
                char *new_buf = (char *)realloc(string_buffer, size * sizeof(char));
                if (new_buf == NULL) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
                string_buffer = new_buf;
            }

            if (cwd == NULL)
               raise_syscall(taskData, "getcwd failed", ERRORNUMBER);
            return SAVE(C_string_to_Poly(taskData, cwd));
        }

    case 55: /* Create a new directory. */
        {
            TempString dirName(args->Word());
            if (dirName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            if (mkdir(dirName, 0777) != 0)
                raise_syscall(taskData, "mkdir failed", ERRORNUMBER);
            return Make_fixed_precision(taskData, 0);
        }

    case 56: /* Delete a directory. */
        {
            TempString dirName(args->Word());
            if (dirName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            if (rmdir(dirName) != 0)
                raise_syscall(taskData, "rmdir failed", ERRORNUMBER);
            return Make_fixed_precision(taskData, 0);
        }

    case 57: /* Test for directory. */
        return isDir(taskData, args);

    case 58: /* Test for symbolic link. */
        {
            TempString fileName(args->Word());
            if (fileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            struct stat fbuff;
            if (lstat(fileName, &fbuff) != 0)
                raise_syscall(taskData, "stat failed", ERRORNUMBER);
            return Make_fixed_precision(taskData, 
                    ((fbuff.st_mode & S_IFMT) == S_IFLNK) ? 1 : 0);
        }

    case 59: /* Read a symbolic link. */
        {
            int nLen;
            TempString linkName(args->Word());
            if (linkName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);

            size_t size = 4096;
            TempString resBuf((char *)malloc(size * sizeof(char)));
            if (resBuf == NULL) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            // nLen is signed, so cast size to ssize_t to perform signed
            // comparison, avoiding an infinite loop when nLen is -1.
            while ((nLen = readlink(linkName, resBuf, size)) >= (ssize_t) size) {
                size *= 2;
                if (size > std::numeric_limits<ssize_t>::max()) raise_fail(taskData, "readlink needs too large a buffer");
                char *newBuf = (char *)realloc(resBuf, size * sizeof(char));
                if (newBuf == NULL) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
                resBuf = newBuf;
            }
            if (nLen < 0) raise_syscall(taskData, "readlink failed", ERRORNUMBER);
            return(SAVE(C_string_to_Poly(taskData, resBuf, nLen)));
        }

    case 60: /* Return the full absolute path name. */
        return fullPath(taskData, args);

    case 61: /* Modification time. */
        return modTime(taskData, args);

    case 62: /* File size. */
        return fileSize(taskData, args);

    case 63: /* Set file time. */
        return setTime(taskData, strm, args);

    case 64: /* Delete a file. */
        {
            TempString fileName(args->Word());
            if (fileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            if (unlink(fileName) != 0)
                raise_syscall(taskData, "unlink failed", ERRORNUMBER);
            return Make_fixed_precision(taskData, 0);
        }

    case 65: /* rename a file. */
        return renameFile(taskData, strm, args);

    case 66: /* Get access rights. */
        return fileAccess(taskData, strm, args);

    case 67: /* Return a temporary file name. */
        {
            const char *template_subdir =  "/MLTEMPXXXXXX";
#ifdef P_tmpdir
            TempString buff((char *)malloc(strlen(P_tmpdir) + strlen(template_subdir) + 1));
            if (buff == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            strcpy(buff, P_tmpdir);
#else
            const char *tmpdir = "/tmp";
            TempString buff((char *)malloc(strlen(tmpdir) + strlen(template_subdir) + 1));
            if (buff == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            strcpy(buff, tmpdir);
#endif
            strcat(buff, template_subdir);
#if (defined(HAVE_MKSTEMP) && ! defined(UNICODE))
            // mkstemp is present in the Mingw64 headers but only as ANSI not Unicode.
            // Set the umask to mask out access by anyone else.
            // mkstemp generally does this anyway.
            mode_t oldMask = umask(0077);
            int fd = mkstemp(buff);
            int wasError = ERRORNUMBER;
            (void)umask(oldMask);
            if (fd != -1) close(fd);
            else raise_syscall(taskData, "mkstemp failed", wasError);
#else
            if (mktemp(buff) == 0)
                raise_syscall(taskData, "mktemp failed", ERRORNUMBER);
            int fd = open(buff, O_RDWR | O_CREAT | O_EXCL, 00600);
            if (fd != -1) close(fd);
            else raise_syscall(taskData, "Temporary file creation failed", ERRORNUMBER);
#endif
            Handle res = SAVE(C_string_to_Poly(taskData, buff));
            return res;
        }

    case 68: /* Get the file id. */
        {
            struct stat fbuff;
            TempString fileName(args->Word());
            if (fileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            if (stat(fileName, &fbuff) != 0)
                raise_syscall(taskData, "stat failed", ERRORNUMBER);
            /* Assume that inodes are always non-negative. */
            return Make_arbitrary_precision(taskData, fbuff.st_ino);
        }

    case 69: // Return an index for a token.  It is used in OS.IO.hash.
        // This is supposed to be well distributed for any 2^n but simply return
        // the stream number.
        return Make_fixed_precision(taskData, getStreamFileDescriptor(taskData, strm->Word()));

    case 70: /* Posix.FileSys.openf - open a file with given mode. */
        {
            Handle name = taskData->saveVec.push(DEREFWORDHANDLE(args)->Get(0));
            int mode = get_C_int(taskData, DEREFWORDHANDLE(args)->Get(1));
            return open_file(taskData, name, mode, 0666, 1);
        }

    case 71: /* Posix.FileSys.createf - create a file with given mode and access. */
        {
            Handle name = taskData->saveVec.push(DEREFWORDHANDLE(args)->Get(0));
            int mode = get_C_int(taskData, DEREFWORDHANDLE(args)->Get(1));
            int access = get_C_int(taskData, DEREFWORDHANDLE(args)->Get(2));
            return open_file(taskData, name, mode|O_CREAT, access, 1);
        }

    default:
        {
            char msg[100];
            snprintf(msg, sizeof(msg), "Unknown io function: %d", c);
            raise_exception_string(taskData, EXC_Fail, msg);
            return 0;
        }
    }
}

// General interface to IO.  Ideally the various cases will be made into
// separate functions.
POLYUNSIGNED PolyBasicIOGeneral(POLYUNSIGNED threadId, POLYUNSIGNED code, POLYUNSIGNED strm, POLYUNSIGNED arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedCode = taskData->saveVec.push(code);
    Handle pushedStrm = taskData->saveVec.push(strm);
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = IO_dispatch_c(taskData, pushedArg, pushedStrm, pushedCode);
    }
    catch (KillException &) {
        processes->ThreadExit(taskData); // TestAnyEvents may test for kill
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Create a persistent file descriptor value for Posix.FileSys.stdin etc.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyPosixCreatePersistentFD(POLYUNSIGNED threadId, POLYUNSIGNED fd)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = alloc_and_save(taskData,
            WORDS(SIZEOF_VOIDP), F_BYTE_OBJ | F_MUTABLE_BIT | F_NO_OVERWRITE);
        *(POLYSIGNED*)(result->Word().AsCodePtr()) = PolyWord::FromUnsigned(fd).UnTagged() + 1;
    }
    catch (...) { } // If an ML exception is raised - could have run out of memory

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();

}
struct _entrypts basicIOEPT[] =
{
    { "PolyChDir",                      (polyRTSFunction)&PolyChDir},
    { "PolyBasicIOGeneral",             (polyRTSFunction)&PolyBasicIOGeneral},
    { "PolyPollIODescriptors",          (polyRTSFunction)&PolyPollIODescriptors },
    { "PolyPosixCreatePersistentFD",    (polyRTSFunction)&PolyPosixCreatePersistentFD},

    { NULL, NULL} // End of list.
};
