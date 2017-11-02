/*
    Title:      Basic IO.

    Copyright (c) 2000, 2015, 2016 David C. J. Matthews

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

#if (defined(_WIN32) && ! defined(__CYGWIN__))
#include <winsock2.h>
#include <tchar.h>
#else
typedef char TCHAR;
#define _T(x) x
#define lstrcat strcat
#define _topen open
#define _tmktemp mktemp
#define _tcsdup strdup
#endif

#ifndef O_BINARY
#define O_BINARY    0 /* Not relevant. */
#endif
#ifndef INFTIM
#define INFTIM (-1)
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

#if (defined(_WIN32) && ! defined(__CYGWIN__))
#include "Console.h"
#define TOOMANYFILES ERROR_NO_MORE_FILES
#define NOMEMORY ERROR_NOT_ENOUGH_MEMORY
#define STREAMCLOSED ERROR_INVALID_HANDLE
#define FILEDOESNOTEXIST ERROR_FILE_NOT_FOUND
#define ERRORNUMBER _doserrno
#else
#define TOOMANYFILES EMFILE
#define NOMEMORY ENOMEM
#define STREAMCLOSED EBADF
#define FILEDOESNOTEXIST ENOENT
#define ERRORNUMBER errno
#endif

#ifndef O_ACCMODE
#define O_ACCMODE   (O_RDONLY|O_RDWR|O_WRONLY)
#endif

#define STREAMID(x) (DEREFSTREAMHANDLE(x)->streamNo)

#define SAVE(x) taskData->saveVec.push(x)

#ifdef _MSC_VER
// Don't tell me about ISO C++ changes.
#pragma warning(disable:4996)
#endif

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyChDir(PolyObject *threadId, PolyWord arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyBasicIOGeneral(PolyObject *threadId, PolyWord code, PolyWord strm, PolyWord arg);
}

/* Points to tokens which represent the streams and the stream itself. 
   For each stream a single word token is made containing the file 
   number, and its address is put in here. When the stream is closed 
   the entry is overwritten. Any further activity will be trapped 
   because the address in the vector will not be the same as the 
   address of the token. This also prevents streams other than stdin 
   and stdout from being made persistent. stdin, stdout and stderr are
   treated specially.  The tokens for them are entries in the
   interface vector and so can be made persistent. */
/*
I've tried various ways of getting asynchronous IO to work in a
consistent manner across different kinds of IO devices in Windows.
It is possible to pass some kinds of handles to WaitForMultipleObjects
but not all.  Anonymous pipes, for example, cannot be used in Windows 95
and don't seem to do what is expected in Windows NT (they return signalled
even when there is no input).  The console is even more of a mess. The
handle is signalled when there are any events (such as mouse movements)
available but these are ignored by ReadFile, which may then block.
Conversely using ReadFile to read less than a line causes the handle
to be unsignalled, supposedly meaning that no input is available, yet
ReadFile will return subsequent characters without blocking.  The eventual
solution was to replace the console completely.
DCJM May 2000 
*/

PIOSTRUCT basic_io_vector;
PLock ioLock; // Currently this just protects against two threads using the same entry

#if (defined(_WIN32) && ! defined(__CYGWIN__))
class WaitStream: public WaitHandle
{
public:
    WaitStream(PIOSTRUCT strm): WaitHandle(strm == NULL ? NULL : strm->hAvailable) {}
};

#else

class WaitStream: public WaitInputFD
{
public:
    WaitStream(PIOSTRUCT strm): WaitInputFD(strm == NULL ? -1 : strm->device.ioDesc) {}
};
#endif

#if (defined(_WIN32) && ! defined(__CYGWIN__))

/* Deal with the various cases to see if input is available. */
static bool isAvailable(TaskData *taskData, PIOSTRUCT strm)
{
    HANDLE  hFile = (HANDLE)_get_osfhandle(strm->device.ioDesc);

    if (isPipe(strm))
    {
        DWORD dwAvail;
        int err;
        if (PeekNamedPipe(hFile, NULL, 0, NULL, &dwAvail, NULL))
            return dwAvail != 0;
        err = GetLastError();
        /* Windows returns ERROR_BROKEN_PIPE on input whereas Unix
           only returns it on output and treats it as EOF.  We
           follow Unix here.  */
        if (err == ERROR_BROKEN_PIPE)
            return true; /* At EOF - will not block. */
        else raise_syscall(taskData, "PeekNamedPipe error", err);
        /*NOTREACHED*/
    }

    else if (isConsole(strm)) return isConsoleInput();

    else if (isDevice(strm))
        return WaitForSingleObject(hFile, 0) == WAIT_OBJECT_0;
    else
        /* File - We may be at end-of-file but we won't block. */
        return true;
}

#else
static bool isAvailable(TaskData *taskData, PIOSTRUCT strm)
{
#ifdef __CYGWIN__
      static struct timeval poll = {0,1};
#else
      static struct timeval poll = {0,0};
#endif
      fd_set read_fds;
      int selRes;
      FD_ZERO(&read_fds);
      FD_SET((int)strm->device.ioDesc, &read_fds);

      /* If there is something there we can return. */
      selRes = select(FD_SETSIZE, &read_fds, NULL, NULL, &poll);
      if (selRes > 0) return true; /* Something waiting. */
      else if (selRes < 0 && errno != EINTR) // Maybe another thread closed descr
          raise_syscall(taskData, "select error", ERRORNUMBER);
      else return false;
}

#endif

static POLYUNSIGNED max_streams;

/* If we try opening a stream and it fails with EMFILE (too many files
   open) we may be able to recover by garbage-collecting and closing some
   unreferenced streams.  This flag is set to indicate that we have had
   an EMFILE error and is cleared whenever a file is closed or opened
   successfully.  It prevents infinite looping if we really have too
   many files. */
bool emfileFlag = false;

/* Close a stream, either explicitly or as a result of detecting an
   unreferenced stream in the g.c.  Doesn't report any errors. */
void close_stream(PIOSTRUCT str)
{
    if (!isOpen(str)) return;
    if (isDirectory(str))
    {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        FindClose(str->device.directory.hFind);
#else
        closedir(str->device.ioDir);
#endif
    }
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    else if (isSocket(str))
    {
        closesocket(str->device.sock);
    }
    else if (isConsole(str)) return;
#endif
    else close(str->device.ioDesc);
    str->ioBits = 0;
    str->token = TAGGED(0);
    emfileFlag = false;
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    if (str->hAvailable) CloseHandle(str->hAvailable);
    str->hAvailable = NULL;
#endif
}

PIOSTRUCT get_stream(PolyWord stream_token)
/* Checks that the stream number is valid and returns the actual stream. 
   Returns NULL if the stream is closed. */
{
    POLYUNSIGNED stream_no;
    if (stream_token.IsTagged())
        stream_no = stream_token.UnTaggedUnsigned();
    else stream_no = ((StreamToken*)stream_token.AsObjPtr())->streamNo;

    if (stream_no >= max_streams)
        return 0;
    if (basic_io_vector[stream_no].token != stream_token)
    {
        // Backwards compatibility.  The persistent streams may either be
        // tagged values or IO entry pointers.
        if (stream_no >= 3)
            return 0;
    }
    if (! isOpen(&basic_io_vector[stream_no])) 
        return 0; 

    return &basic_io_vector[stream_no];
}

Handle make_stream_entry(TaskData *taskData)
// Find a free entry in the stream vector and return a token for it.  
{
    POLYUNSIGNED stream_no;

    ioLock.Lock();
    // Find an unused entry.
    for(stream_no = 0;
        stream_no < max_streams && basic_io_vector[stream_no].token != ClosedToken;
        stream_no++);
    
    /* Check we have enough space. */
    if (stream_no >= max_streams)
    { /* No space. */
        POLYUNSIGNED oldMax = max_streams;
        max_streams += max_streams/2;
        PIOSTRUCT newVector =
            (PIOSTRUCT)realloc(basic_io_vector, max_streams*sizeof(IOSTRUCT));
        if (newVector == NULL) return NULL;
        basic_io_vector = newVector;
        /* Clear the new space. */
        memset(basic_io_vector+oldMax, 0, (max_streams-oldMax)*sizeof(IOSTRUCT));
        for (POLYUNSIGNED i = oldMax; i < max_streams; i++)
            basic_io_vector[i].token = ClosedToken;
    }

    // Create the token.  This must be mutable not because it will be updated but
    // because we will use pointer-equality on it and the GC does not guarantee to
    // preserve pointer-equality for immutables.
    Handle str_token =
        alloc_and_save(taskData, (sizeof(StreamToken) + sizeof(PolyWord) - 1)/sizeof(PolyWord), 
                       F_BYTE_OBJ|F_MUTABLE_BIT);
    STREAMID(str_token) = stream_no;

    ASSERT(!isOpen(&basic_io_vector[stream_no]));
    /* Clear the entry then set the token. */
    memset(&basic_io_vector[stream_no], 0, sizeof(IOSTRUCT));
    basic_io_vector[stream_no].token = DEREFWORDHANDLE(str_token);

    ioLock.Unlock();
    
    return str_token;
}

/******************************************************************************/
/*                                                                            */
/*      free_stream_entry - utility function                                  */
/*                                                                            */
/******************************************************************************/
/* Free an entry in the stream vector - used when openstreamc grabs a
   stream vector entry, but then fails to open the associated file. 
   (This happens frequently when we are using the Poly make system.)
   If we don't recycle the stream vector entries immediately we quickly
   run out and must perform a full garbage collection to recover
   the unused ones. SPF 12/9/95
*/ 
void free_stream_entry(POLYUNSIGNED stream_no)
{
    ASSERT(stream_no < max_streams);

    ioLock.Lock();
    basic_io_vector[stream_no].token  = ClosedToken;
    basic_io_vector[stream_no].ioBits = 0;
    ioLock.Unlock();
}

#if (defined(_WIN32) && ! defined(__CYGWIN__))
// When testing for available input we need to do it differently depending on
// the kind of handle we have.
static int getFileType(int stream)
{
    if (stream == 0 && hOldStdin == INVALID_HANDLE_VALUE)
        /* If this is stdio and we're using our own console.*/
        return IO_BIT_GUI_CONSOLE;
    switch (GetFileType((HANDLE)_get_osfhandle(stream)))
    {
        case FILE_TYPE_PIPE: return IO_BIT_PIPE;
        case FILE_TYPE_CHAR: return IO_BIT_DEV;
        default: return 0;
    }
}
#endif

/* Open a file in the required mode. */
static Handle open_file(TaskData *taskData, Handle filename, int mode, int access, int isPosix)
{
    while (true) // Repeat only with certain kinds of errors
    {
        TempString cFileName(filename->Word()); // Get file name
        if (cFileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
        Handle str_token = make_stream_entry(taskData);
        if (str_token == NULL) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
        POLYUNSIGNED stream_no = STREAMID(str_token);
        int stream = _topen(cFileName, mode, access);

        if (stream >= 0)
        {
            PIOSTRUCT strm = &basic_io_vector[stream_no];
            strm->device.ioDesc = stream;
            strm->ioBits = IO_BIT_OPEN;
            if ((mode & O_ACCMODE) != O_WRONLY)
                strm->ioBits |= IO_BIT_READ;
            if ((mode & O_ACCMODE) != O_RDONLY)
                strm->ioBits |= IO_BIT_WRITE;
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            strm->ioBits |= getFileType(stream);
#else
            if (! isPosix)
            {
                /* Set the close-on-exec flag.  We don't set this if we are being
                   called from one of the low level functions in the Posix structure.
                   I assume that if someone is using those functions they know what
                   they're doing and would expect the behaviour to be close to that
                   of the underlying function. */
                fcntl(stream, F_SETFD, 1);
            }
#endif
            emfileFlag = false; /* Successful open. */
            return str_token;
        }

        free_stream_entry(stream_no);
        switch (errno)
        {
        case EINTR: // Just try the call.  Is it possible to block here indefinitely?
            continue;
        case EMFILE: /* too many open files */
            {
                if (emfileFlag) /* Previously had an EMFILE error. */
                    raise_syscall(taskData, "Cannot open", TOOMANYFILES);
                emfileFlag = true;
                FullGC(taskData); /* May clear emfileFlag if we close a file. */
                continue;
            }
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
    // Closed streams, stdin, stdout or stderr are all short ints.
    if (stream->Word().IsDataPtr())
    {
        PIOSTRUCT strm = get_stream(DEREFHANDLE(stream));
        if (strm != NULL && strm->token.IsTagged()) strm = NULL; // Backwards compatibility for stdin etc.
        // Ignore closed streams, stdin, stdout or stderr.
        if (strm != NULL) close_stream(strm);
    }

    return Make_fixed_precision(taskData, 0);
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
        PIOSTRUCT   strm;

        while (true) {
            strm = get_stream(DEREFHANDLE(stream));
            /* Raise an exception if the stream has been closed. */
            if (strm == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
            if (isAvailable(taskData, strm))
                break;
            WaitStream waiter(strm);
            processes->ThreadPauseForIO(taskData, &waiter);
        }

#if (defined(_WIN32) && ! defined(__CYGWIN__))
        if (strm->hAvailable != NULL) ResetEvent(strm->hAvailable);
#endif
        // We can now try to read without blocking.
        // Actually there's a race here in the unlikely situation that there
        // are multiple threads sharing the same low-level reader.  They could
        // both detect that input is available but only one may succeed in
        // reading without blocking.  This doesn't apply where the threads use
        // the higher-level IO interfaces in ML which have their own mutexes.
        int fd = strm->device.ioDesc;
        byte *base = DEREFHANDLE(args)->Get(0).AsObjPtr()->AsBytePtr();
        POLYUNSIGNED offset = getPolyUnsigned(taskData, DEREFWORDHANDLE(args)->Get(1));
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        unsigned length = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(2));
        int haveRead;
#else
        size_t length = getPolyUnsigned(taskData, DEREFWORDHANDLE(args)->Get(2));
        ssize_t haveRead;
#endif
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        if (isConsole(strm))
            haveRead = getConsoleInput((char*)base+offset, length);
        else
#endif
            // Unix and Windows other than console.
            haveRead = read(fd, base+offset, length);
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
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    int length = get_C_int(taskData, DEREFWORD(args));
    int haveRead;
#else
    size_t length = getPolyUnsigned(taskData, DEREFWORD(args));
    ssize_t haveRead;
#endif
    // We should check for interrupts even if we're not going to block.
    processes->TestAnyEvents(taskData);

    while (1) // Loop if interrupted.
    {
        // First test to see if we have input available.
        // These tests may result in a GC if another thread is running.
        PIOSTRUCT   strm;

        while (true) {
            strm = get_stream(DEREFHANDLE(stream));
            /* Raise an exception if the stream has been closed. */
            if (strm == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
            if (isAvailable(taskData, strm))
                break;
            WaitStream waiter(strm);
            processes->ThreadPauseForIO(taskData, &waiter);
        }

#if (defined(_WIN32) && ! defined(__CYGWIN__))
        if (strm->hAvailable != NULL) ResetEvent(strm->hAvailable);
#endif

        // We can now try to read without blocking.
        int fd = strm->device.ioDesc;
        // We previously allocated the buffer on the stack but that caused
        // problems with multi-threading at least on Mac OS X because of
        // stack exhaustion.  We limit the space to 100k. */
        if (length > 102400) length = 102400;
        byte *buff = (byte*)malloc(length);
        if (buff == 0) raise_syscall(taskData, "Unable to allocate buffer", NOMEMORY);

#if (defined(_WIN32) && ! defined(__CYGWIN__))
        if (isConsole(strm))
            haveRead = getConsoleInput((char*)buff, length);
        else
#endif
            // Unix and Windows other than console.
            haveRead = read(fd, buff, length);
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
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    unsigned length = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(2));
    int haveWritten;
#else
    size_t length = getPolyUnsigned(taskData, DEREFWORDHANDLE(args)->Get(2));
    ssize_t haveWritten;
#endif
    PIOSTRUCT       strm = get_stream(stream->WordP());
    byte    ch;
    /* Raise an exception if the stream has been closed. */
    if (strm == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);

    /* We don't actually handle cases of blocking on output. */
    byte *toWrite;
    if (IS_INT(base))
    {
        /* To allow this function to work on strings as well as
           vectors we have to be able to handle the special case of
           a single character string. */
        ch = (byte)(UNTAGGED(base));
        toWrite = &ch;
        offset = 0;
        length = 1;
    }
    else toWrite = base.AsObjPtr()->AsBytePtr();
    haveWritten = write(strm->device.ioDesc, toWrite+offset, length);
    if (haveWritten < 0) raise_syscall(taskData, "Error while writing", ERRORNUMBER);

    return Make_fixed_precision(taskData, haveWritten);
}

// Test whether we can write without blocking.  Returns false if it will block,
// true if it will not.
static bool canOutput(TaskData *taskData, Handle stream)
{
    PIOSTRUCT strm = get_stream(stream->WordP());
    if (strm == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);

#if (defined(_WIN32) && ! defined(__CYGWIN__))
    /* There's no way I can see of doing this in Windows. */
    return true;
#else
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
    FD_SET(strm->device.ioDesc, &write_fds);
    sel = select(FD_SETSIZE,&read_fds,&write_fds,&except_fds,&poll);
    if (sel < 0 && errno != EINTR)
        raise_syscall(taskData, "select failed", ERRORNUMBER);
    return sel > 0;
#endif
}

static long seekStream(TaskData *taskData, PIOSTRUCT strm, long pos, int origin)
{
    long lpos;
    lpos = lseek(strm->device.ioDesc, pos, origin);
    if (lpos < 0) raise_syscall(taskData, "Position error", ERRORNUMBER);
    return lpos;
}

/* Return the number of bytes available on the device.  Works only for
   files since it is meaningless for other devices. */
static Handle bytesAvailable(TaskData *taskData, Handle stream)
{
    PIOSTRUCT strm = get_stream(stream->WordP());
    if (strm == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);

    /* Remember our original position, seek to the end, then seek back. */
    long original = seekStream(taskData, strm, 0L, SEEK_CUR);
    long endOfStream = seekStream(taskData, strm, 0L, SEEK_END);
    if (seekStream(taskData, strm, original, SEEK_SET) != original) 
        raise_syscall(taskData, "Position error", ERRORNUMBER);
    return Make_fixed_precision(taskData, endOfStream-original);
}


#define FILEKIND_FILE   0
#define FILEKIND_DIR    1
#define FILEKIND_LINK   2
#define FILEKIND_TTY    3
#define FILEKIND_PIPE   4
#define FILEKIND_SKT    5
#define FILEKIND_DEV    6
#define FILEKIND_ERROR  (-1)

static Handle fileKind(TaskData *taskData, Handle stream)
{
    PIOSTRUCT strm = get_stream(stream->WordP());
    if (strm == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    {
        HANDLE hTest;
        if (strm->device.ioDesc == 0)
        {
            // Stdin is special.  The actual handle is to a pipe whether we are using our
            // own console or we were provided with a stdin.
            if (hOldStdin == INVALID_HANDLE_VALUE)
                return Make_fixed_precision(taskData, FILEKIND_TTY); // We've made our own console
            hTest = hOldStdin;
        }
        else hTest = (HANDLE)_get_osfhandle(strm->device.ioDesc);
        switch (GetFileType(hTest))
        {
        case FILE_TYPE_PIPE: return Make_fixed_precision(taskData, FILEKIND_PIPE);
        case FILE_TYPE_CHAR: return Make_fixed_precision(taskData, FILEKIND_TTY); // Or a device?
        default: return Make_fixed_precision(taskData, FILEKIND_FILE);
        }
    }
#else
    {
        struct stat statBuff;
        if (fstat(strm->device.ioDesc, &statBuff) < 0) raise_syscall(taskData, "Stat failed", ERRORNUMBER);
        switch (statBuff.st_mode & S_IFMT)
        {
        case S_IFIFO:
            return Make_fixed_precision(taskData, FILEKIND_PIPE);
        case S_IFCHR:
        case S_IFBLK:
            if (isatty(strm->device.ioDesc))
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
#endif
}

/* Polling.  For the moment this applies only to objects which can
   be opened in the file system.  It may need to be extended to sockets
   later.  */
#define POLL_BIT_IN     1
#define POLL_BIT_OUT    2
#define POLL_BIT_PRI    4
/* Find out what polling options, if any, are allowed on this
   file descriptor.  We assume that polling is allowed on all
   descriptors, either for reading or writing depending on how
   the stream was opened. */
Handle pollTest(TaskData *taskData, Handle stream)
{
    PIOSTRUCT strm = get_stream(stream->WordP());
    int nRes = 0;
    if (strm == NULL) return Make_fixed_precision(taskData, 0);
    /* Allow for the possibility of both being set in the future. */
    if (isRead(strm)) nRes |= POLL_BIT_IN;
    if (isWrite(strm)) nRes |= POLL_BIT_OUT;
        /* For the moment we don't allow POLL_BIT_PRI.  */
    return Make_fixed_precision(taskData, nRes);
}

/* Do the polling.  Takes a vector of io descriptors, a vector of bits to test
   and a time to wait and returns a vector of results. */
static Handle pollDescriptors(TaskData *taskData, Handle args, int blockType)
{
    TryAgain:
    PolyObject  *strmVec = DEREFHANDLE(args)->Get(0).AsObjPtr();
    PolyObject  *bitVec =  DEREFHANDLE(args)->Get(1).AsObjPtr();
    POLYUNSIGNED nDesc = strmVec->Length();
    ASSERT(nDesc ==  bitVec->Length());
    // We should check for interrupts even if we're not going to block.
    processes->TestAnyEvents(taskData);

    /* Simply do a non-blocking poll. */
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    {
        /* Record the results in this vector. */
        char *results = 0;
        int haveResult = 0;
        Handle  resVec;
        if (nDesc > 0)
        {
            results = (char*)alloca(nDesc);
            memset(results, 0, nDesc);
        }
        
        for (POLYUNSIGNED i = 0; i < nDesc; i++)
        {
            Handle marker = taskData->saveVec.mark();
            PIOSTRUCT strm = get_stream(strmVec->Get(i).AsObjPtr());
            taskData->saveVec.reset(marker);
            int bits = get_C_int(taskData, bitVec->Get(i));
            if (strm == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
            
            if (isSocket(strm))
            {
                SOCKET sock = strm->device.sock;
                if (bits & POLL_BIT_PRI)
                {
                    u_long atMark = 0;
                    ioctlsocket(sock, SIOCATMARK, &atMark);
                    if (atMark) { haveResult = 1; results[i] |= POLL_BIT_PRI; }
                }
                if (bits & (POLL_BIT_IN|POLL_BIT_OUT))
                {
                    FD_SET readFds, writeFds;
                    TIMEVAL poll = {0, 0};
                    FD_ZERO(&readFds); FD_ZERO(&writeFds);
                    if (bits & POLL_BIT_IN) FD_SET(sock, &readFds);
                    if (bits & POLL_BIT_OUT) FD_SET(sock, &writeFds);
                    if (select(FD_SETSIZE, &readFds, &writeFds, NULL, &poll) > 0)
                    {
                        haveResult = 1;
                        /* N.B. select only tells us about out-of-band data if
                        SO_OOBINLINE is FALSE. */
                        if (FD_ISSET(sock, &readFds)) results[i] |= POLL_BIT_IN;
                        if (FD_ISSET(sock, &writeFds)) results[i] |= POLL_BIT_OUT;
                    }
                }
            }
            else
            {
                if ((bits & POLL_BIT_IN) && isRead(strm) && isAvailable(taskData, strm))
                {
                    haveResult = 1;
                    results[i] |= POLL_BIT_IN;
                }
                if ((bits & POLL_BIT_OUT) && isWrite(strm))
                {
                    /* I don't know if there's any way to do this. */
                    if (WaitForSingleObject(
                        (HANDLE)_get_osfhandle(strm->device.ioDesc), 0) == WAIT_OBJECT_0)
                    {
                        haveResult = 1;
                        results[i] |= POLL_BIT_OUT;
                    }
                }
                /* PRIORITY doesn't make sense for anything but a socket. */
            }
        }
        if (haveResult == 0)
        {
            /* Poll failed - treat as time out. */
            switch (blockType)
            {
            case 0: /* Check the time out. */
                {
                    Handle hSave = taskData->saveVec.mark();
                    /* The time argument is an absolute time. */
                    FILETIME ftTime, ftNow;
                    /* Get the file time. */
                    getFileTimeFromArb(taskData, taskData->saveVec.push(DEREFHANDLE(args)->Get(2)), &ftTime);
                    GetSystemTimeAsFileTime(&ftNow);
                    taskData->saveVec.reset(hSave);
                    /* If the timeout time is earlier than the current time
                       we must return, otherwise we block. */
                    if (CompareFileTime(&ftTime, &ftNow) <= 0)
                        break; /* Return the empty set. */
                    /* else drop through and block. */
                }
            case 1: /* Block until one of the descriptors is ready. */
                processes->ThreadPause(taskData);
                goto TryAgain;
                /*NOTREACHED*/
            case 2: /* Just a simple poll - drop through. */
                break;
            }
        }
        /* Copy the results to a result vector. */
        resVec = alloc_and_save(taskData, nDesc);
        for (POLYUNSIGNED j = 0; j < nDesc; j++)
            (DEREFWORDHANDLE(resVec))->Set(j, TAGGED(results[j]));
        return resVec;
    }
#elif (! defined(HAVE_POLL_H))
    /* Unix but poll isn't provided, e.g. Mac OS X.  Implement in terms of "select" as far as we can. */
    {
        fd_set readFds, writeFds, exceptFds;
        struct timeval poll = {0, 0};
        int selectRes = 0;
        FD_ZERO(&readFds); FD_ZERO(&writeFds); FD_ZERO(&exceptFds);

        for (POLYUNSIGNED i = 0; i < nDesc; i++)
        {
            PIOSTRUCT strm = get_stream(strmVec->Get(i).AsObjPtr());
            int bits = UNTAGGED(bitVec->Get(i));
            if (strm == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
            if (bits & POLL_BIT_IN) FD_SET(strm->device.ioDesc, &readFds);
            if (bits & POLL_BIT_OUT) FD_SET(strm->device.ioDesc, &writeFds);
        }
        /* Simply check the status without blocking. */
        if (nDesc > 0) selectRes = select(FD_SETSIZE, &readFds, &writeFds, &exceptFds, &poll);
        if (selectRes < 0) raise_syscall(taskData, "select failed", ERRORNUMBER);
        /* What if nothing was ready? */
        if (selectRes == 0)
        {
            switch (blockType)
            {
            case 0: /* Check the timeout. */
                {
                    struct timeval tv;
                    /* We have a value in microseconds.  We need to split
                       it into seconds and microseconds. */
                    Handle hSave = taskData->saveVec.mark();
                    Handle hTime = SAVE(DEREFWORDHANDLE(args)->Get(2));
                    Handle hMillion = Make_arbitrary_precision(taskData, 1000000);
                    unsigned long secs =
                        get_C_ulong(taskData, DEREFWORDHANDLE(div_longc(taskData, hMillion, hTime)));
                    unsigned long usecs =
                        get_C_ulong(taskData, DEREFWORDHANDLE(rem_longc(taskData, hMillion, hTime)));
                    /* If the timeout time is earlier than the current time
                       we must return, otherwise we block. */
                    taskData->saveVec.reset(hSave);
                    if (gettimeofday(&tv, NULL) != 0)
                        raise_syscall(taskData, "gettimeofday failed", ERRORNUMBER);
                    if ((unsigned long)tv.tv_sec > secs ||
                        ((unsigned long)tv.tv_sec == secs && (unsigned long)tv.tv_usec >= usecs))
                        break;
                    /* else block. */
                }
            case 1: /* Block until one of the descriptors is ready. */
                processes->ThreadPause(taskData);
                goto TryAgain;
            case 2: /* Just a simple poll - drop through. */
                break;
            }
        }
        /* Copy the results. */
        if (nDesc == 0) return taskData->saveVec.push(EmptyString());
        /* Construct a result vector. */
        Handle resVec = alloc_and_save(taskData, nDesc);
        for (POLYUNSIGNED i = 0; i < nDesc; i++)
        {
            POLYUNSIGNED res = 0;
            POLYUNSIGNED bits = UNTAGGED(bitVec->Get(i));
            PIOSTRUCT strm = get_stream(strmVec->Get(i).AsObjPtr());
            if ((bits & POLL_BIT_IN) && FD_ISSET(strm->device.ioDesc, &readFds)) res |= POLL_BIT_IN;
            if ((bits & POLL_BIT_OUT) && FD_ISSET(strm->device.ioDesc, &writeFds)) res |= POLL_BIT_OUT;
            DEREFWORDHANDLE(resVec)->Set(i, TAGGED(res));
        }
        return resVec;
    }
#else
    /* Unix */
    {
        int pollRes = 0;
        struct pollfd * fds = 0;
        if (nDesc > 0)
            fds = (struct pollfd *)alloca(nDesc * sizeof(struct pollfd));
        
        /* Set up the request vector. */
        for (unsigned i = 0; i < nDesc; i++)
        {
            PIOSTRUCT strm = get_stream(strmVec->Get(i).AsObjPtr());
            POLYUNSIGNED bits = UNTAGGED(bitVec->Get(i));
            if (strm == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
            fds[i].fd = strm->device.ioDesc;
            fds[i].events = 0;
            if (bits & POLL_BIT_IN) fds[i].events |= POLLIN; /* | POLLRDNORM??*/
            if (bits & POLL_BIT_OUT) fds[i].events |= POLLOUT;
            if (bits & POLL_BIT_PRI) fds[i].events |= POLLPRI;
            fds[i].revents = 0;
        }
        /* Poll the descriptors. */
        if (nDesc > 0) pollRes = poll(fds, nDesc, 0);
        if (pollRes < 0) raise_syscall(taskData, "poll failed", ERRORNUMBER);
        /* What if nothing was ready? */
        if (pollRes == 0)
        {
            switch (blockType)
            {
            case 0: /* Check the timeout. */
                {
                    struct timeval tv;
                    /* We have a value in microseconds.  We need to split
                       it into seconds and microseconds. */
                    // We need to reset the savevec because we can come here repeatedly
                    Handle hSave = taskData->saveVec.mark();
                    Handle hTime = SAVE(DEREFWORDHANDLE(args)->Get(2));
                    Handle hMillion = Make_arbitrary_precision(taskData, 1000000);
                    unsigned long secs =
                        get_C_ulong(taskData, DEREFWORDHANDLE(div_longc(taskData, hMillion, hTime)));
                    unsigned long usecs =
                        get_C_ulong(taskData, DEREFWORDHANDLE(rem_longc(taskData, hMillion, hTime)));
                    taskData->saveVec.reset(hSave);
                    /* If the timeout time is earlier than the current time
                       we must return, otherwise we block. */
                    if (gettimeofday(&tv, NULL) != 0)
                        raise_syscall(taskData, "gettimeofday failed", ERRORNUMBER);
                    if ((unsigned long)tv.tv_sec > secs ||
                        ((unsigned long)tv.tv_sec == secs && (unsigned long)tv.tv_usec >= usecs))
                        break;
                    /* else block. */
                }
            case 1: /* Block until one of the descriptors is ready. */
                processes->ThreadPause(taskData);
                goto TryAgain;
            case 2: /* Just a simple poll - drop through. */
                break;
            }
        }
        /* Copy the results. */
        /* Construct a result vector. */
        Handle resVec = alloc_and_save(taskData, nDesc);
        for (unsigned i = 0; i < nDesc; i++)
        {
            int res = 0;
            if (fds[i].revents & POLLIN) res = POLL_BIT_IN;
            if (fds[i].revents & POLLOUT) res = POLL_BIT_OUT;
            if (fds[i].revents & POLLPRI) res = POLL_BIT_PRI;
            DEREFWORDHANDLE(resVec)->Set(i, TAGGED(res));
        }
        return resVec;
    }
#endif
}


/* Directory functions. */
/* Open a directory. */
static Handle openDirectory(TaskData *taskData, Handle dirname)
{
    while (1) // Only certain errors
    {
        Handle str_token = make_stream_entry(taskData);
        if (str_token == NULL) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
        POLYUNSIGNED stream_no    = STREAMID(str_token);
        PIOSTRUCT strm = &basic_io_vector[stream_no];
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        {
            // Get the directory name but add on two characters for the \* plus one for the NULL.
            POLYUNSIGNED length = PolyStringLength(dirname->Word());
            TempString dirName((TCHAR*)malloc((length + 3)*sizeof(TCHAR)));
            if (dirName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            Poly_string_to_C(dirname->Word(), dirName, length+2);
            // Tack on \* to the end so that we find all files in the directory.
            lstrcat(dirName, _T("\\*"));
            HANDLE hFind = FindFirstFile(dirName, &strm->device.directory.lastFind);
            if (hFind == INVALID_HANDLE_VALUE)
                raise_syscall(taskData, "FindFirstFile failed", GetLastError());
            strm->device.directory.hFind = hFind;
            /* There must be at least one file which matched. */
            strm->device.directory.fFindSucceeded = 1;
        }
#else
        TempString dirName(dirname->Word());
        if (dirName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
        DIR *dirp = opendir(dirName);
        if (dirp == NULL)
        {
            free_stream_entry(stream_no);
            switch (errno)
            {
            case EINTR:
                continue; // Just retry the call.
            case EMFILE:
                {
                    if (emfileFlag) /* Previously had an EMFILE error. */
                        raise_syscall(taskData, "Cannot open", TOOMANYFILES);
                    emfileFlag = true;
                    FullGC(taskData); /* May clear emfileFlag if we close a file. */
                    continue;
                }
            default:
                raise_syscall(taskData, "opendir failed", ERRORNUMBER);
            }
        }
        strm->device.ioDir = dirp;
#endif
        strm->ioBits = IO_BIT_OPEN | IO_BIT_DIR;
        return(str_token);
    }
}

/* Return the next entry from the directory, ignoring current and
   parent arcs ("." and ".." in Windows and Unix) */
Handle readDirectory(TaskData *taskData, Handle stream)
{
    PIOSTRUCT strm = get_stream(stream->WordP());
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    Handle result = NULL;
#endif
    /* Raise an exception if the stream has been closed. */
    if (strm == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    /* The next entry to read is already in the buffer. FindFirstFile
       both opens the directory and returns the first entry. If
       fFindSucceeded is false we have already reached the end. */
    if (! strm->device.directory.fFindSucceeded)
        return SAVE(EmptyString(taskData));
    while (result == NULL)
    {
        WIN32_FIND_DATA *pFind = &strm->device.directory.lastFind;
        if (!((pFind->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) &&
            (lstrcmp(pFind->cFileName, _T(".")) == 0 ||
             lstrcmp(pFind->cFileName, _T("..")) == 0)))
        {
            result = SAVE(C_string_to_Poly(taskData, pFind->cFileName));
        }
        /* Get the next entry. */
        if (! FindNextFile(strm->device.directory.hFind, pFind))
        {
            DWORD dwErr = GetLastError();
            if (dwErr == ERROR_NO_MORE_FILES)
            {
                strm->device.directory.fFindSucceeded = 0;
                if (result == NULL) return SAVE(EmptyString(taskData));
            }
        }
    }
    return result;
#else
    while (1)
    {
        struct dirent *dp = readdir(strm->device.ioDir);
        int len;
        if (dp == NULL) return taskData->saveVec.push(EmptyString(taskData));
        len = NAMLEN(dp);
        if (!((len == 1 && strncmp(dp->d_name, ".", 1) == 0) ||
              (len == 2 && strncmp(dp->d_name, "..", 2) == 0)))
            return SAVE(C_string_to_Poly(taskData, dp->d_name, len));
    }
#endif
}

Handle rewindDirectory(TaskData *taskData, Handle stream, Handle dirname)
{
    PIOSTRUCT strm = get_stream(stream->WordP());
    /* Raise an exception if the stream has been closed. */
    if (strm == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    {
        /* There's no rewind - close and reopen. */
        FindClose(strm->device.directory.hFind);
        strm->ioBits = 0;
        POLYUNSIGNED length = PolyStringLength(dirname->Word());
        TempString dirName((TCHAR*)malloc((length + 3)*sizeof(TCHAR)));
        if (dirName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
        Poly_string_to_C(dirname->Word(), dirName, length+2);
        // Tack on \* to the end so that we find all files in the directory.
        lstrcat(dirName, _T("\\*"));
        HANDLE hFind = FindFirstFile(dirName, &strm->device.directory.lastFind);
        if (hFind == INVALID_HANDLE_VALUE)
            raise_syscall(taskData, "FindFirstFile failed", GetLastError());
        strm->device.directory.hFind = hFind;
        /* There must be at least one file which matched. */
        strm->device.directory.fFindSucceeded = 1;
        strm->ioBits = IO_BIT_OPEN | IO_BIT_DIR;
    }
#else
    rewinddir(strm->device.ioDir);
#endif
    return Make_fixed_precision(taskData, 0);
}

/* change_dirc - this is called directly and not via the dispatch
   function. */
static Handle change_dirc(TaskData *taskData, Handle name)
/* Change working directory. */
{
    TempString cDirName(name->Word());
    if (cDirName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    if (SetCurrentDirectory(cDirName) == FALSE)
       raise_syscall(taskData, "SetCurrentDirectory failed", GetLastError());
#else
    if (chdir(cDirName) != 0)
        raise_syscall(taskData, "chdir failed", ERRORNUMBER);
#endif
    return SAVE(TAGGED(0));
}

// External call
POLYUNSIGNED PolyChDir(PolyObject *threadId, PolyWord arg)
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
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    {
        DWORD dwRes = GetFileAttributes(cDirName);
        if (dwRes == 0xFFFFFFFF)
            raise_syscall(taskData, "GetFileAttributes failed", GetLastError());
        if (dwRes & FILE_ATTRIBUTE_DIRECTORY)
            return Make_fixed_precision(taskData, 1);
        else return Make_fixed_precision(taskData, 0);
    }
#else
    {
        struct stat fbuff;
        if (stat(cDirName, &fbuff) != 0)
            raise_syscall(taskData, "stat failed", ERRORNUMBER);
        if ((fbuff.st_mode & S_IFMT) == S_IFDIR)
            return Make_fixed_precision(taskData, 1);
        else return Make_fixed_precision(taskData, 0);
    }
#endif
}

/* Get absolute canonical path name. */
Handle fullPath(TaskData *taskData, Handle filename)
{
    TempString cFileName;

    /* Special case of an empty string. */
    if (PolyStringLength(filename->Word()) == 0) cFileName = _tcsdup(_T("."));
    else cFileName = Poly_string_to_T_alloc(filename->Word());
    if (cFileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);

#if (defined(_WIN32) && ! defined(__CYGWIN__))
    {
        // Get the length
        DWORD dwRes = GetFullPathName(cFileName, 0, NULL, NULL);
        if (dwRes == 0)
            raise_syscall(taskData, "GetFullPathName failed", GetLastError());
        TempString resBuf((TCHAR*)malloc(dwRes * sizeof(TCHAR)));
        if (resBuf == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
        // When the length is enough the result is the length excluding the null
        DWORD dwRes1 = GetFullPathName(cFileName, dwRes, resBuf, NULL);
        if (dwRes1 == 0 || dwRes1 >= dwRes)
            raise_syscall(taskData, "GetFullPathName failed", GetLastError());
        /* Check that the file exists.  GetFullPathName doesn't do that. */
        dwRes = GetFileAttributes(resBuf);
        if (dwRes == 0xffffffff)
            raise_syscall(taskData, "File does not exist", FILEDOESNOTEXIST);
        return(SAVE(C_string_to_Poly(taskData, resBuf)));
    }
#else
    {
        TempCString resBuf(realpath(cFileName, NULL));
        if (resBuf == NULL)
            raise_syscall(taskData, "realpath failed", ERRORNUMBER);
        /* Some versions of Unix don't check the final component
           of a file.  To be consistent try doing a "stat" of
           the resulting string to check it exists. */
        struct stat fbuff;
        if (stat(resBuf, &fbuff) != 0)
            raise_syscall(taskData, "stat failed", ERRORNUMBER);
        return(SAVE(C_string_to_Poly(taskData, resBuf)));
    }
#endif
}

/* Get file modification time.  This returns the value in the
   time units and from the base date used by timing.c. c.f. filedatec */
Handle modTime(TaskData *taskData, Handle filename)
{
    TempString cFileName(filename->Word());
    if (cFileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    {
        /* There are two ways to get this information.
           We can either use GetFileTime if we are able
           to open the file for reading but if it is locked
           we won't be able to.  FindFirstFile is the other
           alternative.  We have to check that the file name
           does not contain '*' or '?' otherwise it will try
           to "glob" this, which isn't what we want here. */
        WIN32_FIND_DATA wFind;
        HANDLE hFind;
        const TCHAR *p;
        for(p = cFileName; *p; p++)
            if (*p == '*' || *p == '?')
                raise_syscall(taskData, "Invalid filename", STREAMCLOSED);
        hFind = FindFirstFile(cFileName, &wFind);
        if (hFind == INVALID_HANDLE_VALUE)
            raise_syscall(taskData, "FindFirstFile failed", GetLastError());
        FindClose(hFind);
        return Make_arb_from_Filetime(taskData, wFind.ftLastWriteTime);
    }
#else
    {
        struct stat fbuff;
        if (stat(cFileName, &fbuff) != 0)
            raise_syscall(taskData, "stat failed", ERRORNUMBER);
        /* Convert to microseconds. */
        return Make_arb_from_pair_scaled(taskData, STAT_SECS(&fbuff,m),
                                         STAT_USECS(&fbuff,m), 1000000);
    }
#endif
}

/* Get file size. */
Handle fileSize(TaskData *taskData, Handle filename)
{
    TempString cFileName(filename->Word());
    if (cFileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    {
        /* Similar to modTime*/
        WIN32_FIND_DATA wFind;
        HANDLE hFind;
        const TCHAR *p;
        for(p = cFileName; *p; p++)
            if (*p == '*' || *p == '?')
                raise_syscall(taskData, "Invalid filename", STREAMCLOSED);
        hFind = FindFirstFile(cFileName, &wFind);
        if (hFind == INVALID_HANDLE_VALUE)
            raise_syscall(taskData, "FindFirstFile failed", GetLastError());
        FindClose(hFind);
        return Make_arb_from_32bit_pair(taskData, wFind.nFileSizeHigh, wFind.nFileSizeLow);
    }
#else
    {
    struct stat fbuff;
    if (stat(cFileName, &fbuff) != 0)
        raise_syscall(taskData, "stat failed", ERRORNUMBER);
    return Make_arbitrary_precision(taskData, fbuff.st_size);
    }
#endif
}

/* Set file modification and access times. */
Handle setTime(TaskData *taskData, Handle fileName, Handle fileTime)
{
    TempString cFileName(fileName->Word());
    if (cFileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);

#if (defined(_WIN32) && ! defined(__CYGWIN__))
    /* The only way to set the time is to open the file and
       use SetFileTime. */
    {
        FILETIME ft;
        /* Get the file time. */
        getFileTimeFromArb(taskData, fileTime, &ft);
        /* Open an existing file with write access. We need that
           for SetFileTime. */
        HANDLE hFile = CreateFile(cFileName, GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
                    FILE_ATTRIBUTE_NORMAL, NULL);
        if (hFile == INVALID_HANDLE_VALUE)
            raise_syscall(taskData, "CreateFile failed", GetLastError());
        /* Set the file time. */
        if (!SetFileTime(hFile, NULL, &ft, &ft))
        {
            int nErr = GetLastError();
            CloseHandle(hFile);
            raise_syscall(taskData, "SetFileTime failed", nErr);
        }
        CloseHandle(hFile);
    }
#else
    {
        struct timeval times[2];
        /* We have a value in microseconds.  We need to split
           it into seconds and microseconds. */
        Handle hTime = fileTime;
        Handle hMillion = Make_arbitrary_precision(taskData, 1000000);
        /* N.B. Arguments to div_longc and rem_longc are in reverse order. */
        unsigned secs =
            get_C_ulong(taskData, DEREFWORDHANDLE(div_longc(taskData, hMillion, hTime)));
        unsigned usecs =
            get_C_ulong(taskData, DEREFWORDHANDLE(rem_longc(taskData, hMillion, hTime)));
        times[0].tv_sec = times[1].tv_sec = secs;
        times[0].tv_usec = times[1].tv_usec = usecs;
        if (utimes(cFileName, times) != 0)
            raise_syscall(taskData, "utimes failed", ERRORNUMBER);
    }
#endif
    return Make_fixed_precision(taskData, 0);
}

/* Rename a file. */
Handle renameFile(TaskData *taskData, Handle oldFileName, Handle newFileName)
{
    TempString oldName(oldFileName->Word()), newName(newFileName->Word());
    if (oldName == 0 || newName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    if (! MoveFileEx(oldName, newName, MOVEFILE_REPLACE_EXISTING))
        raise_syscall(taskData, "MoveFileEx failed", GetLastError());
#else
    if (rename(oldName, newName) != 0)
        raise_syscall(taskData, "rename failed", ERRORNUMBER);
#endif
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

#if (defined(_WIN32) && ! defined(__CYGWIN__))
    {
        /* Test whether the file is read-only.  This is, of course,
           not what was asked but getting anything more is really
           quite complicated.  I don't see how we can find out if
           a file is executable (maybe check if the extension is
           .exe, .com or .bat?).  It would be possible, in NT, to
           examine the access structures but that seems far too
           complicated.  Leave it for the moment. */
        DWORD dwRes = GetFileAttributes(fileName);
        if (dwRes == 0xffffffff)
            return Make_fixed_precision(taskData, 0);
        /* If we asked for write access but it is read-only we
           return false. */
        if ((dwRes & FILE_ATTRIBUTE_READONLY) &&
            (rts & FILE_ACCESS_WRITE))
            return Make_fixed_precision(taskData, 0);
        else return Make_fixed_precision(taskData, 1);
    }
#else
    {
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
#endif

}



/* IO_dispatchc.  Called from assembly code module. */
static Handle IO_dispatch_c(TaskData *taskData, Handle args, Handle strm, Handle code)
{
    unsigned c = get_C_unsigned(taskData, DEREFWORD(code));
    switch (c)
    {
    case 0: /* Return standard input */
        return SAVE(basic_io_vector[0].token);
    case 1: /* Return standard output */
        return SAVE(basic_io_vector[1].token);
    case 2: /* Return standard error */
        return SAVE(basic_io_vector[2].token);
    case 3: /* Open file for text input. */
        return open_file(taskData, args, O_RDONLY, 0666, 0);
    case 4: /* Open file for binary input. */
        return open_file(taskData, args, O_RDONLY | O_BINARY, 0666, 0);
    case 5: /* Open file for text output. */
        return open_file(taskData, args, O_WRONLY | O_CREAT | O_TRUNC, 0666, 0);
    case 6: /* Open file for binary output. */
        return open_file(taskData, args, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, 0666, 0);
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
        return open_file(taskData, args, O_WRONLY | O_CREAT | O_APPEND, 0666, 0);
    case 14: /* Open binary file for appending. */
        return open_file(taskData, args, O_WRONLY | O_CREAT | O_APPEND | O_BINARY, 0666, 0);
    case 15: /* Return recommended buffer size. */
        /* TODO: This should try to find a sensible number based on
           the stream handle passed in. Leave it at 1k for
           the moment. */
        /* Try increasing to 4k. */
        return Make_fixed_precision(taskData, /*1024*/4096);

    case 16: /* See if we can get some input. */
        {
            PIOSTRUCT str = get_stream(strm->WordP());
            if (str == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
            return Make_fixed_precision(taskData, isAvailable(taskData, str) ? 1 : 0);
        }

    case 17: /* Return the number of bytes available.  */
        return bytesAvailable(taskData, strm);

    case 18: /* Get position on stream. */
        {
            /* Get the current position in the stream.  This is used to test
               for the availability of random access so it should raise an
               exception if setFilePos or endFilePos would fail. */
            PIOSTRUCT str = get_stream(strm->WordP());
            if (str == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);

            long pos = seekStream(taskData, str, 0L, SEEK_CUR);
            return Make_arbitrary_precision(taskData, pos);
        }

    case 19: /* Seek to position on stream. */
        {
            long position = (long)get_C_long(taskData, DEREFWORD(args));
            PIOSTRUCT str = get_stream(strm->WordP());
            if (str == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);

            (void)seekStream(taskData, str, position, SEEK_SET);
            return Make_arbitrary_precision(taskData, 0);
        }

    case 20: /* Return position at end of stream. */
        {
            PIOSTRUCT str = get_stream(strm->WordP());
            if (str == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);

            /* Remember our original position, seek to the end, then seek back. */
            long original = seekStream(taskData, str, 0L, SEEK_CUR);
            long endOfStream = seekStream(taskData, str, 0L, SEEK_END);
            if (seekStream(taskData, str, original, SEEK_SET) != original) 
                raise_syscall(taskData, "Position error", ERRORNUMBER);
            return Make_arbitrary_precision(taskData, endOfStream);
        }

    case 21: /* Get the kind of device underlying the stream. */
        return fileKind(taskData, strm);
    case 22: /* Return the polling options allowed on this descriptor. */
        return pollTest(taskData, strm);
    case 23: /* Poll the descriptor, waiting forever. */
        return pollDescriptors(taskData, args, 1);
    case 24: /* Poll the descriptor, waiting for the time requested. */
        return pollDescriptors(taskData, args, 0);
    case 25: /* Poll the descriptor, returning immediately.*/
        return pollDescriptors(taskData, args, 2);
    case 26: /* Get binary as a vector. */
        return readString(taskData, strm, args, false);

    case 27: /* Block until input is available. */
        // We should check for interrupts even if we're not going to block.
        processes->TestAnyEvents(taskData);
        while (true) {
            PIOSTRUCT str = get_stream(strm->WordP());
            if (str == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
            if (isAvailable(taskData, str))
                return Make_fixed_precision(taskData, 0);
            WaitStream waiter(str);
            processes->ThreadPauseForIO(taskData, &waiter);
        }

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
            PIOSTRUCT str = get_stream(strm->WordP());
            if (str == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
            return Make_fixed_precision(taskData, str->device.ioDesc);
        }

    case 31: /* Make an entry for a given descriptor. */
        {
            int ioDesc = get_C_int(taskData, DEREFWORD(args));
            PIOSTRUCT str;
            /* First see if it's already in the table. */
            for (unsigned i = 0; i < max_streams; i++)
            {
                str = &(basic_io_vector[i]);
                if (str->token != ClosedToken && str->device.ioDesc == ioDesc)
                    return taskData->saveVec.push(str->token);
            }
            /* Have to make a new entry. */
            Handle str_token = make_stream_entry(taskData);
            if (str_token == NULL) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            POLYUNSIGNED stream_no    = STREAMID(str_token);
            str = &basic_io_vector[stream_no];
            str->device.ioDesc = get_C_int(taskData, DEREFWORD(args));
            /* We don't know whether it's open for read, write or even if
               it's open at all. */
            str->ioBits = IO_BIT_OPEN | IO_BIT_READ | IO_BIT_WRITE ;
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            str->ioBits |= getFileType(ioDesc);
#endif
            return str_token;
        }


    /* Directory functions. */
    case 50: /* Open a directory. */
        return openDirectory(taskData, args);

    case 51: /* Read a directory entry. */
        return readDirectory(taskData, strm);

    case 52: /* Close the directory */
        return close_file(taskData, strm);

    case 53: /* Rewind the directory. */
        return rewindDirectory(taskData, strm, args);

    case 54: /* Get current working directory. */
        {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            DWORD space = GetCurrentDirectory(0, NULL);
            if (space == 0)
               raise_syscall(taskData, "GetCurrentDirectory failed", GetLastError());
            TempString buff((TCHAR*)malloc(space * sizeof(TCHAR)));
            if (buff == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            if (GetCurrentDirectory(space, buff) == 0)
                raise_syscall(taskData, "GetCurrentDirectory failed", GetLastError());
            return SAVE(C_string_to_Poly(taskData, buff));
#else
            size_t size = 4096;
            TempString string_buffer((TCHAR *)malloc(size * sizeof(TCHAR)));
            if (string_buffer == NULL) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            TCHAR *cwd;
            while ((cwd = getcwd(string_buffer, size)) == NULL && errno == ERANGE) {
                if (size > std::numeric_limits<size_t>::max() / 2) raise_fail(taskData, "getcwd needs too large a buffer");
                size *= 2;
                TCHAR *new_buf = (TCHAR *)realloc(string_buffer, size * sizeof(TCHAR));
                if (new_buf == NULL) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
                string_buffer = new_buf;
            }

            if (cwd == NULL)
               raise_syscall(taskData, "getcwd failed", ERRORNUMBER);
            return SAVE(C_string_to_Poly(taskData, cwd));
#endif
        }

    case 55: /* Create a new directory. */
        {
            TempString dirName(args->Word());
            if (dirName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            if (! CreateDirectory(dirName, NULL))
               raise_syscall(taskData, "CreateDirectory failed", GetLastError());
#else
            if (mkdir(dirName, 0777) != 0)
                raise_syscall(taskData, "mkdir failed", ERRORNUMBER);
#endif

            return Make_fixed_precision(taskData, 0);
        }

    case 56: /* Delete a directory. */
        {
            TempString dirName(args->Word());
            if (dirName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            if (! RemoveDirectory(dirName))
               raise_syscall(taskData, "RemoveDirectory failed", GetLastError());
#else
            if (rmdir(dirName) != 0)
                raise_syscall(taskData, "rmdir failed", ERRORNUMBER);
#endif

            return Make_fixed_precision(taskData, 0);
        }

    case 57: /* Test for directory. */
        return isDir(taskData, args);

    case 58: /* Test for symbolic link. */
        {
            TempString fileName(args->Word());
            if (fileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            {
                DWORD dwRes = GetFileAttributes(fileName);
                if (dwRes == 0xFFFFFFFF)
                    raise_syscall(taskData, "GetFileAttributes failed", GetLastError());
                return Make_fixed_precision(taskData, (dwRes & FILE_ATTRIBUTE_REPARSE_POINT) ? 1:0);
            }
#else
            {
            struct stat fbuff;
                if (lstat(fileName, &fbuff) != 0)
                    raise_syscall(taskData, "stat failed", ERRORNUMBER);
                return Make_fixed_precision(taskData, 
                        ((fbuff.st_mode & S_IFMT) == S_IFLNK) ? 1 : 0);
            }
#endif
        }

    case 59: /* Read a symbolic link. */
        {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            // Windows has added symbolic links but reading the target is far from
            // straightforward.   It's probably not worth trying to implement this.
            raise_syscall(taskData, "Symbolic links are not implemented", 0);
            return taskData->saveVec.push(TAGGED(0)); /* To keep compiler happy. */
#else
            int nLen;
            TempString linkName(args->Word());
            if (linkName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);

            size_t size = 4096;
            TempString resBuf((TCHAR *)malloc(size * sizeof(TCHAR)));
            if (resBuf == NULL) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            // nLen is signed, so cast size to ssize_t to perform signed
            // comparison, avoiding an infinite loop when nLen is -1.
            while ((nLen = readlink(linkName, resBuf, size)) >= (ssize_t) size) {
                size *= 2;
                if (size > std::numeric_limits<ssize_t>::max()) raise_fail(taskData, "readlink needs too large a buffer");
                TCHAR *newBuf = (TCHAR *)realloc(resBuf, size * sizeof(TCHAR));
                if (newBuf == NULL) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
                resBuf = newBuf;
            }
            if (nLen < 0) raise_syscall(taskData, "readlink failed", ERRORNUMBER);
            return(SAVE(C_string_to_Poly(taskData, resBuf, nLen)));
#endif
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
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            if (! DeleteFile(fileName))
               raise_syscall(taskData, "DeleteFile failed", GetLastError());
#else
            if (unlink(fileName) != 0)
                raise_syscall(taskData, "unlink failed", ERRORNUMBER);
#endif

            return Make_fixed_precision(taskData, 0);
        }

    case 65: /* rename a file. */
        return renameFile(taskData, strm, args);

    case 66: /* Get access rights. */
        return fileAccess(taskData, strm, args);

    case 67: /* Return a temporary file name. */
        {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            DWORD dwSpace = GetTempPath(0, NULL);
            if (dwSpace == 0)
                raise_syscall(taskData, "GetTempPath failed", GetLastError());
            TempString buff((TCHAR*)malloc((dwSpace + 12)*sizeof(TCHAR)));
            if (buff == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            if (GetTempPath(dwSpace, buff) == 0)
                raise_syscall(taskData, "GetTempPath failed", GetLastError());
            lstrcat(buff, _T("MLTEMPXXXXXX"));
#else
            const char *template_subdir =  "/MLTEMPXXXXXX";
#ifdef P_tmpdir
            TempString buff((TCHAR *)malloc(strlen(P_tmpdir) + strlen(template_subdir) + 1));
            if (buff == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            strcpy(buff, P_tmpdir);
#else
            const char *tmpdir = "/tmp";
            TempString buff((TCHAR *)malloc(strlen(tmpdir) + strlen(template_subdir) + 1));
            if (buff == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            strcpy(buff, tmpdir);
#endif
            strcat(buff, template_subdir);
#endif

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
            if (_tmktemp(buff) == 0)
                raise_syscall(taskData, "mktemp failed", ERRORNUMBER);
            int fd = _topen(buff, O_RDWR | O_CREAT | O_EXCL, 00600);
            if (fd != -1) close(fd);
            else raise_syscall(taskData, "Temporary file creation failed", ERRORNUMBER);
#endif
            Handle res = SAVE(C_string_to_Poly(taskData, buff));
            return res;
        }

    case 68: /* Get the file id. */
        {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            /* This concept does not exist in Windows. */
            /* Return a negative number. This is interpreted
               as "not implemented". */
            return Make_fixed_precision(taskData, -1);
#else
            struct stat fbuff;
            TempString fileName(args->Word());
            if (fileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
            if (stat(fileName, &fbuff) != 0)
                raise_syscall(taskData, "stat failed", ERRORNUMBER);
            /* Assume that inodes are always non-negative. */
            return Make_arbitrary_precision(taskData, fbuff.st_ino);
#endif
        }

    case 69: /* Return an index for a token. */
        return Make_fixed_precision(taskData, STREAMID(strm));

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
            sprintf(msg, "Unknown io function: %d", c);
            raise_exception_string(taskData, EXC_Fail, msg);
            return 0;
        }
    }
}

// General interface to IO.  Ideally the various cases will be made into
// separate functions.
POLYUNSIGNED PolyBasicIOGeneral(PolyObject *threadId, PolyWord code, PolyWord strm, PolyWord arg)
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

struct _entrypts basicIOEPT[] =
{
    { "PolyChDir",                      (polyRTSFunction)&PolyChDir},
    { "PolyBasicIOGeneral",             (polyRTSFunction)&PolyBasicIOGeneral},

    { NULL, NULL} // End of list.
};

class BasicIO: public RtsModule
{
public:
    virtual void Init(void);
    virtual void Start(void);
    virtual void Stop(void);
    void GarbageCollect(ScanAddress *process);
};

// Declare this.  It will be automatically added to the table.
static BasicIO basicIOModule;

void BasicIO::Init(void)
{    
    max_streams = 20; // Initialise to the old Unix maximum. Will grow if necessary.
    /* A vector for the streams (initialised by calloc) */
    basic_io_vector = (PIOSTRUCT)calloc(max_streams, sizeof(IOSTRUCT));
    for (unsigned i = 0; i < max_streams; i++)
        basic_io_vector[i].token = ClosedToken;
}

void BasicIO::Start(void)
{
    basic_io_vector[0].token  = TAGGED(0);
    basic_io_vector[0].device.ioDesc = 0;
    basic_io_vector[0].ioBits = IO_BIT_OPEN | IO_BIT_READ;
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    basic_io_vector[0].ioBits |= getFileType(0);
    // Set this to a duplicate of the handle so it can be closed when we
    // close the stream.
    HANDLE hDup;
    if (DuplicateHandle(GetCurrentProcess(), hInputEvent, GetCurrentProcess(),
                        &hDup, 0, FALSE, DUPLICATE_SAME_ACCESS))
        basic_io_vector[0].hAvailable = hDup;
#endif

    basic_io_vector[1].token  = TAGGED(1);
    basic_io_vector[1].device.ioDesc = 1;
    basic_io_vector[1].ioBits = IO_BIT_OPEN | IO_BIT_WRITE;
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    basic_io_vector[1].ioBits |= getFileType(1);
#endif

    basic_io_vector[2].token  = TAGGED(2);
    basic_io_vector[2].device.ioDesc = 2;
    basic_io_vector[2].ioBits = IO_BIT_OPEN | IO_BIT_WRITE;
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    basic_io_vector[2].ioBits |= getFileType(2);
#endif
    return;
}

/* Release all resources.  Not strictly necessary since the OS should
   do this but probably a good idea. */
void BasicIO::Stop(void)
{
    if (basic_io_vector)
    {
        // Don't close the standard streams since we may need
        // stdout at least to produce final debugging output.
        for (unsigned i = 3; i < max_streams; i++)
        {
            if (isOpen(&basic_io_vector[i]))
                close_stream(&basic_io_vector[i]);
        }
        free(basic_io_vector);
    }
    basic_io_vector = NULL;
}

void BasicIO::GarbageCollect(ScanAddress *process)
/* Ensures that all the objects are retained and their addresses updated. */
{
    /* Entries in the file table. These are marked as weak references so may
       return 0 for unreferenced streams. */
    for(unsigned i = 0; i < max_streams; i++)
    {
        PIOSTRUCT str = &(basic_io_vector[i]);
        
        if (str->token.IsDataPtr())
        {
            PolyObject *token = str->token.AsObjPtr();
            process->ScanRuntimeAddress(&token, ScanAddress::STRENGTH_WEAK);
            
            /* Unreferenced streams may return zero. */ 
            if (token == 0 && isOpen(str))
                close_stream(str);
            str->token = token == 0 ? ClosedToken : token;
        }
    }
}
