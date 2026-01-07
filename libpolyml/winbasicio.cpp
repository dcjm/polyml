/*
    Title:      Basic IO for Windows.

    Copyright (c) 2000, 2015-2019 David C. J. Matthews

    This was split from the common code for Unix and Windows.

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

#include <winsock2.h>
#include <tchar.h>

#ifndef INFTIM
#define INFTIM (-1)
#endif

#include <new>

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
#include "winstartup.h"

#define NOMEMORY ERROR_NOT_ENOUGH_MEMORY
#define STREAMCLOSED ERROR_INVALID_HANDLE
#define FILEDOESNOTEXIST ERROR_FILE_NOT_FOUND
#define ERRORNUMBER _doserrno

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
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTestForInput(POLYUNSIGNED threadId, POLYUNSIGNED strm, POLYUNSIGNED waitMillisecs);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyTestForOutput(POLYUNSIGNED threadId, POLYUNSIGNED strm, POLYUNSIGNED waitMillisecs);
}

// References to the standard streams.  They are only needed if we are compiling
// the basis library and make a second call to get the standard streams.
static PolyObject *standardInputValue, *standardOutputValue, *standardErrorValue;

// Creates a new unique pipename in the appropriate format.
// Utility function provided for winguiconsole and windows_specific
void newPipeName(TCHAR *pipeName)
{
    static LONG pipenum = 0;
    wsprintf(pipeName, _T("\\\\.\\Pipe\\PolyPipe.%08x.%08x"), GetCurrentProcessId(), InterlockedIncrement(&pipenum));
}

int WinStream::fileTypeOfHandle(HANDLE hStream)
{
    switch (GetFileType(hStream))
    {
    case FILE_TYPE_PIPE: return FILEKIND_PIPE;
    case FILE_TYPE_CHAR: return FILEKIND_TTY;// Or a device?
    case FILE_TYPE_DISK: return FILEKIND_FILE;
    default:
        if (GetLastError() == 0)
            return FILEKIND_UNKNOWN; // Error or unknown.
        else return FILEKIND_ERROR;
    }
}

void WinStream::unimplemented(TaskData *taskData)
{
    // Called on the random access functions
    raise_syscall(taskData, "Position error", ERROR_NOT_SUPPORTED);
}

// Backwards compatibility.  This should now be done in ML.
void WinStream::waitUntilAvailable(TaskData *taskData)
{
    while (!testForInput(taskData, 1000))
    {
    }
}

void WinStream::waitUntilOutputPossible(TaskData *taskData)
{
    while (!testForOutput(taskData, 1000))
    {
    }
}


WinInOutStream::WinInOutStream()
{
    hStream = hEvent = INVALID_HANDLE_VALUE;
    buffer = 0;
    currentInBuffer = currentPtr = 0;
    endOfStream = false;
    buffSize = 4096; // Seems like a good number
    ZeroMemory(&overlap, sizeof(overlap));
    isText = false;
    isRead = true;
}

WinInOutStream::~WinInOutStream()
{
    free(buffer);
}

void WinInOutStream::openFile(TaskData * taskData, TCHAR *name, openMode mode, bool isT)
{
    isRead = mode == OPENREAD;
    isText = isT;
    ASSERT(hStream == INVALID_HANDLE_VALUE); // We should never reuse an object.
    buffer = (byte*)malloc(buffSize);
    if (buffer == 0)
        raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    // Create a manual reset event with state=signalled.  This means
    // that no operation is in progress.
    hEvent = CreateEvent(NULL, TRUE, TRUE, NULL);
    overlap.hEvent = hEvent;
    switch (mode)
    {
    case OPENREAD:
        hStream = CreateFile(name, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, NULL);
        break;
    case OPENWRITE:
        hStream = CreateFile(name, GENERIC_WRITE, FILE_SHARE_READ, NULL, CREATE_ALWAYS, FILE_FLAG_OVERLAPPED, NULL);
        break;
    case OPENAPPEND:
        hStream = CreateFile(name, GENERIC_WRITE, FILE_SHARE_READ, NULL, OPEN_ALWAYS, FILE_FLAG_OVERLAPPED, NULL);
        break;
    }
    if (hStream == INVALID_HANDLE_VALUE)
        raise_syscall(taskData, "CreateFile failed", GetLastError());
    // Start a read immediately so that there is something in the buffer.
    switch (mode)
    {
    case OPENREAD:
        if(!beginReading())
            raise_syscall(taskData, "Read failure", GetLastError());
        break;
    case OPENWRITE: break;
    case OPENAPPEND:
    {
        // We could use the special 0xfff... value in the overlapped structure for this
        // but that would mess up getPos/endPos.
        LARGE_INTEGER fileSize;
        if (!GetFileSizeEx(hStream, &fileSize))
            raise_syscall(taskData, "Stream is not a file", GetLastError());
        setOverlappedPos(fileSize.QuadPart);
    }
    break;
    }
}

// This is only used to set up standard output.
// Now also used for Windows.execute.
bool WinInOutStream::openHandle(HANDLE hndl, openMode mode, bool isT)
{
    // Need to check the handle.  It seems DuplicateHandle actually allows an invalid handle
    if (hndl == INVALID_HANDLE_VALUE)
    {
        SetLastError(ERROR_INVALID_HANDLE);
        return false;
    }
    isRead = mode == OPENREAD;
    isText = isT;
    ASSERT(hStream == INVALID_HANDLE_VALUE); // We should never reuse an object.
    buffer = (byte*)malloc(buffSize);
    if (buffer == 0)
    {
        SetLastError(NOMEMORY);
        return false;
    }
    hEvent = CreateEvent(NULL, TRUE, TRUE, NULL);
    overlap.hEvent = hEvent;
    // Duplicate the handle so we can safely close it.
    if (!DuplicateHandle(GetCurrentProcess(), hndl, GetCurrentProcess(), &hStream, 0, FALSE, DUPLICATE_SAME_ACCESS))
        return false;
    if (isRead)
        return beginReading();
    return true;
}

// Start reading.  This may complete immediately.
bool WinInOutStream::beginReading()
{
    if (!ReadFile(hStream, buffer, buffSize, NULL, &overlap))
    {
        switch (GetLastError())
        {
        case ERROR_HANDLE_EOF:
            // We get ERROR_BROKEN_PIPE as EOF on a pipe.
        case ERROR_BROKEN_PIPE:
            endOfStream = true;
        case ERROR_IO_PENDING:
            return true;
        default:
            return false;
        }
    }
    return true;
}

void WinInOutStream::closeEntry(TaskData *taskData)
{
    if (isRead)
    {
        if (WaitForSingleObject(hEvent, 0) == WAIT_TIMEOUT)
            // Something is in progress.
            CancelIoEx(hStream, &overlap);
    }
    else flushOut(taskData);

    PLocker locker(&lock);
    if (!CloseHandle(hStream))
        raise_syscall(taskData, "CloseHandle failed", GetLastError());
    hStream = INVALID_HANDLE_VALUE;
    CloseHandle(hEvent);
    hEvent = INVALID_HANDLE_VALUE;
}

// Make sure that everything has been written.
void WinInOutStream::flushOut(TaskData *taskData)
{
    while (currentInBuffer != 0)
    {
        // If currentInBuffer is not zero we have an operation in progress.
        waitUntilOutputPossible(taskData); // canOutput will test the result and may update currentInBuffer.
        // We may not have written everything so check and repeat if necessary.
        if (currentInBuffer != 0)
            writeStream(taskData, NULL, 0);
    }
}

size_t WinInOutStream::readStream(TaskData *taskData, byte *base, size_t length)
{
    PLocker locker(&lock);
    if (endOfStream) return 0;
    size_t copied = 0;
    // Copy as much as we can from the buffer.
    while (currentPtr < currentInBuffer && copied < length)
    {
        byte b = buffer[currentPtr++];
        // In text mode we want to return NL for CRNL.  Assume that this is
        // properly formatted and simply skip CRs.  It's not clear what to return
        // if it isn't properly formatted and the user can always open it as binary
        // and do the conversion.
        if (!isText || b != '\r')
            base[copied++] = b;
    }
    // If we have exhausted the buffer we start a new read.
    while (isText && currentPtr < currentInBuffer && buffer[currentPtr] == '\r')
        currentPtr++;
    if (currentInBuffer == currentPtr)
    {
        // We need to start a new read
        currentInBuffer = currentPtr = 0;
        if (!beginReading())
            raise_syscall(taskData, "Read failure", GetLastError());
    }
    return copied;
}

// This actually does most of the work.  In particular for text streams we may have a
// block that consists only of CRs.
bool WinInOutStream::isAvailable(TaskData *taskData)
{
    while (1)
    {
        {
            PLocker locker(&lock);
            // It is available if we have something in the buffer or we're at EOF
            if (currentInBuffer < currentPtr || endOfStream)
                return true;
            // We should have had a read in progress.
            DWORD bytesRead = 0;
            if (!GetOverlappedResult(hStream, &overlap, &bytesRead, FALSE))
            {
                DWORD err = GetLastError();
                switch (err)
                {
                case ERROR_HANDLE_EOF:
                case ERROR_BROKEN_PIPE:
                    // We've had EOF - That result is available
                    endOfStream = true;
                    return true;
                case ERROR_IO_INCOMPLETE:
                    // It's still in progress.
                    return false;
                default:
                    raise_syscall(taskData, "GetOverlappedResult failed", err);
                }
            }
            // The next read must be after this.
            setOverlappedPos(getOverlappedPos() + bytesRead);
            currentInBuffer = bytesRead;
            // If this is a text stream skip CRs.
            while (isText && currentPtr < currentInBuffer && buffer[currentPtr] == '\r')
                currentPtr++;
            // If we have some real data it can be read now
            if (currentPtr < currentInBuffer)
                return true;
        }
        // Try again.
        if (!beginReading()) // And loop
            raise_syscall(taskData, "Read failure", GetLastError());
    }
}

bool WinInOutStream::testForInput(TaskData *taskData, unsigned waitMilliSecs)
{
    if (isAvailable(taskData)) return true;
    if (waitMilliSecs != 0)
    {
        WaitHandle waiter(hEvent, waitMilliSecs);
        processes->ThreadPauseForIO(taskData, &waiter);
    }
    return false;
}

int WinInOutStream::poll(TaskData *taskData, int test)
{
    if (test & POLL_BIT_IN)
    {
        if (testForInput(taskData, 0))
            return POLL_BIT_IN;
    }
    if (test & POLL_BIT_OUT)
    {
        if (testForOutput(taskData, 0))
            return POLL_BIT_OUT;
    }

    return 0;
}

// Random access functions
uint64_t WinInOutStream::getPos(TaskData *taskData)
{
    if (GetFileType(hStream) != FILE_TYPE_DISK)
        raise_syscall(taskData, "Stream is not a file", ERROR_SEEK_ON_DEVICE);
    PLocker locker(&lock);
    if (isRead)
        return getOverlappedPos() - currentInBuffer + currentPtr;
    else return getOverlappedPos() + currentInBuffer;
}

void WinInOutStream::setPos(TaskData *taskData, uint64_t pos)
{
    if (GetFileType(hStream) != FILE_TYPE_DISK)
        raise_syscall(taskData, "Stream is not a file", ERROR_SEEK_ON_DEVICE);
    // Need to wait until any pending operation is complete.  If this is a write
    // we need to flush anything before changing the position.
    if (isRead)
    {
        while (WaitForSingleObject(hEvent, 0) == WAIT_TIMEOUT)
        {
            WaitHandle waiter(hEvent, 1000);
            processes->ThreadPauseForIO(taskData, &waiter);
        }
    }
    else flushOut(taskData);

    PLocker locker(&lock);
    setOverlappedPos(pos);
    // Discard any unread data and start reading at the new position.
    currentInBuffer = currentPtr = 0;
    endOfStream = false;
    if (isRead && !beginReading())
        raise_syscall(taskData, "Read failure", GetLastError());
}

uint64_t WinInOutStream::fileSize(TaskData *taskData)
{
    LARGE_INTEGER fileSize;
    if (!GetFileSizeEx(hStream, &fileSize))
        raise_syscall(taskData, "Stream is not a file", GetLastError());
    return fileSize.QuadPart;
}

bool WinInOutStream::canOutput(TaskData *taskData)
{
    if (isRead)
        unimplemented(taskData);

    PLocker locker(&lock);
    // If the buffer is empty we're fine.
    if (currentInBuffer == 0)
        return true;
    // Otherwise there is an operation in progress.  Has it finished?
    DWORD bytesWritten = 0;
    if (!GetOverlappedResult(hStream, &overlap, &bytesWritten, FALSE))
    {
        DWORD err = GetLastError();
        if (err == ERROR_IO_INCOMPLETE)
            return false;
        else raise_syscall(taskData, "GetOverlappedResult failed", err);
    }
    setOverlappedPos(getOverlappedPos() + bytesWritten);
    // If we haven't written everything copy down what we have left.
    if (bytesWritten < currentInBuffer)
        memmove(buffer, buffer + bytesWritten, currentInBuffer - bytesWritten);
    currentInBuffer -= bytesWritten;
    // This will then be written before anything else.
    return true;
}

bool WinInOutStream::testForOutput(TaskData *taskData, unsigned waitMilliSecs)
{
    if (canOutput(taskData)) return true;
    if (waitMilliSecs != 0)
    {
        WaitHandle waiter(hEvent, waitMilliSecs);
        processes->ThreadPauseForIO(taskData, &waiter);
    }
    return false;
}

// Write data.  N.B.  This is also used with zero data from closeEntry.
size_t WinInOutStream::writeStream(TaskData *taskData, byte *base, size_t length)
{
    if (isRead)
        unimplemented(taskData);

    PLocker locker(&lock);
    // Copy as much as we can into the buffer.
    size_t copied = 0;
    while (currentInBuffer < buffSize && copied < length)
    {
        if (isText && base[copied] == '\n')
        {
            // Put in a CR but make sure we've space for both.
            if (currentInBuffer == buffSize - 1)
                break; // Exit the loop with what we've done.
            buffer[currentInBuffer++] = '\r';
        }
        buffer[currentInBuffer++] = base[copied++];
    }

    // Write what's in the buffer.
    if (!WriteFile(hStream, buffer, currentInBuffer, NULL, &overlap))
    {
        DWORD dwErr = GetLastError();
        if (dwErr != ERROR_IO_PENDING)
            raise_syscall(taskData, "WriteFile failed", dwErr);
    }
    // Even if it actually succeeded we still pick up the result in canOutput.
    return copied; // Return what we copied.
}

/* Open a file in the required mode. */
static Handle openWinFile(TaskData *taskData, Handle filename, openMode mode, bool isAppend, bool isBinary)
{
    TempString cFileName(filename->Word()); // Get file name
    if (cFileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    try {
        WinInOutStream *stream = new WinInOutStream();
        stream->openFile(taskData, cFileName, mode, !isBinary);
        return MakeVolatileWord(taskData, stream);
    }
    catch (std::bad_alloc&) {
        raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    }
}

/* Read into an array. */
// We can't combine readArray and readString because we mustn't compute the
// destination of the data in readArray until after any GC.
static Handle readArray(TaskData *taskData, Handle stream, Handle args, bool/*isText*/)
{
    WinStream *strm = *(WinStream**)(stream->WordP());
    if (strm == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
    /* The isText argument is ignored in both Unix and Windows but
    is provided for future use.  Windows remembers the mode used
    when the file was opened to determine whether to translate
    CRLF into LF. */
    // We should check for interrupts even if we're not going to block.
    processes->TestAnyEvents(taskData);

    // First test to see if we have input available.
    // These tests may result in a GC if another thread is running.
    strm->waitUntilAvailable(taskData);

    // We can now try to read without blocking.
    // Actually there's a race here in the unlikely situation that there
    // are multiple threads sharing the same low-level reader.  They could
    // both detect that input is available but only one may succeed in
    // reading without blocking.  This doesn't apply where the threads use
    // the higher-level IO interfaces in ML which have their own mutexes.
    byte *base = DEREFHANDLE(args)->Get(0).AsObjPtr()->AsBytePtr();
    POLYUNSIGNED offset = getPolyUnsigned(taskData, DEREFWORDHANDLE(args)->Get(1));
    size_t length = getPolyUnsigned(taskData, DEREFWORDHANDLE(args)->Get(2));
    size_t haveRead = strm->readStream(taskData, base + offset, length);
    return Make_fixed_precision(taskData, haveRead); // Success.
}

/* Return input as a string. We don't actually need both readArray and
readString but it's useful to have both to reduce unnecessary garbage.
The IO library will construct one from the other but the higher levels
choose the appropriate function depending on need. */
static Handle readString(TaskData *taskData, Handle stream, Handle args, bool/*isText*/)
{
    size_t length = getPolyUnsigned(taskData, DEREFWORD(args));
    WinStream *strm = *(WinStream**)(stream->WordP());
    if (strm == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);

    // We should check for interrupts even if we're not going to block.
    processes->TestAnyEvents(taskData);

    // First test to see if we have input available.
    // These tests may result in a GC if another thread is running.
    strm->waitUntilAvailable(taskData);

    // We can now try to read without blocking.
    // We previously allocated the buffer on the stack but that caused
    // problems with multi-threading at least on Mac OS X because of
    // stack exhaustion.  We limit the space to 100k. */
    if (length > 102400) length = 102400;
    byte *buff = (byte*)malloc(length);
    if (buff == 0) raise_syscall(taskData, "Unable to allocate buffer", NOMEMORY);

    try {
        size_t haveRead = strm->readStream(taskData, buff, length);
        Handle result = SAVE(C_string_to_Poly(taskData, (char*)buff, haveRead));
        free(buff);
        return result;
    }
    catch (...) {
        free(buff);
        throw;
    }
}

static Handle writeArray(TaskData *taskData, Handle stream, Handle args, bool/*isText*/)
{
    // The isText argument is ignored in both Unix and Windows but
    // is provided for future use.  Windows remembers the mode used
    // when the file was opened to determine whether to translate
    // LF into CRLF.
    WinStream *strm = *(WinStream**)(stream->WordP());
    if (strm == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);

    // We should check for interrupts even if we're not going to block.
    processes->TestAnyEvents(taskData);
    strm->waitUntilOutputPossible(taskData);

    PolyWord base = DEREFWORDHANDLE(args)->Get(0);
    POLYUNSIGNED    offset = getPolyUnsigned(taskData, DEREFWORDHANDLE(args)->Get(1));
    size_t length = getPolyUnsigned(taskData, DEREFWORDHANDLE(args)->Get(2));
    /* We don't actually handle cases of blocking on output. */
    byte *toWrite = base.AsObjPtr()->AsBytePtr();
    size_t haveWritten = strm->writeStream(taskData, toWrite + offset, length);
    return Make_fixed_precision(taskData, haveWritten);
}

Handle pollTest(TaskData *taskData, Handle stream)
{
    WinStream *strm = *(WinStream**)(stream->WordP());
    return Make_fixed_precision(taskData, strm->pollTest());
}

// Poll file descriptors.  Also used with empty descriptors as OS.Process.sleep.
// Takes a vector of io descriptors, a vector of bits to test
// and a time to wait and returns a vector of results.
// Windows: This is messy because "select" only works on sockets.
// Do the best we can.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyPollIODescriptors(POLYUNSIGNED threadId, POLYUNSIGNED streamVector, POLYUNSIGNED bitVector, POLYUNSIGNED maxMillisecs)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    unsigned maxMilliseconds = (unsigned)PolyWord::FromUnsigned(maxMillisecs).UnTaggedUnsigned();
    Handle result = 0;

    try {
        PolyObject  *strmVec = PolyWord::FromUnsigned(streamVector).AsObjPtr();
        PolyObject  *bitVec = PolyWord::FromUnsigned(bitVector).AsObjPtr();
        POLYUNSIGNED nDesc = strmVec->Length();
        ASSERT(nDesc == bitVec->Length());
        // We should check for interrupts even if we're not going to block.
        processes->TestAnyEvents(taskData);

        /* Simply do a non-blocking poll. */
        /* Record the results in this vector. */
        char *results = 0;
        bool haveResult = false;
        if (nDesc > 0)
        {
            results = (char*)alloca(nDesc);
            memset(results, 0, nDesc);
        }

        for (POLYUNSIGNED i = 0; i < nDesc; i++)
        {
            WinStream *strm = *(WinStream**)(strmVec->Get(i).AsObjPtr());
            if (strm == NULL) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
            int bits = get_C_int(taskData, bitVec->Get(i));
            results[i] = strm->poll(taskData, bits);
            if (results[i] != 0)
                haveResult = true;
        }
        if (haveResult == 0 && maxMilliseconds != 0)
        {
            /* Poll failed - treat as time out. */
            WaitHandle waiter(NULL, maxMilliseconds);
            processes->ThreadPauseForIO(taskData, &waiter);
        }
        /* Copy the results to a result vector. */
        result = alloc_and_save(taskData, nDesc);
        for (POLYUNSIGNED j = 0; j < nDesc; j++)
            (DEREFWORDHANDLE(result))->Set(j, TAGGED(results[j]));
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
class WinDirData
{
public:
    HANDLE  hFind; /* FindFirstFile handle */
    WIN32_FIND_DATA lastFind;
    int fFindSucceeded;
};

static Handle openDirectory(TaskData *taskData, Handle dirname)
{
    // Get the directory name but add on two characters for the \* plus one for the NULL.
    POLYUNSIGNED length = PolyStringLength(dirname->Word());
    TempString dirName((TCHAR*)malloc((length + 3) * sizeof(TCHAR)));
    if (dirName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    Poly_string_to_C(dirname->Word(), dirName, length + 2);
    // Tack on \* to the end so that we find all files in the directory.
    lstrcat(dirName, _T("\\*"));
    WinDirData *pData = new WinDirData; // TODO: Handle failue
    HANDLE hFind = FindFirstFile(dirName, &pData->lastFind);
    if (hFind == INVALID_HANDLE_VALUE)
        raise_syscall(taskData, "FindFirstFile failed", GetLastError());
    pData->hFind = hFind;
    /* There must be at least one file which matched. */
    pData->fFindSucceeded = 1;
    return MakeVolatileWord(taskData, pData);
}


/* Return the next entry from the directory, ignoring current and
parent arcs ("." and ".." in Windows and Unix) */
Handle readDirectory(TaskData *taskData, Handle stream)
{
    WinDirData *pData = *(WinDirData**)(stream->WordP()); // In a Volatile
    if (pData == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
    Handle result = NULL;
    // The next entry to read is already in the buffer. FindFirstFile
    // both opens the directory and returns the first entry. If
    // fFindSucceeded is false we have already reached the end.
    if (!pData->fFindSucceeded)
        return SAVE(EmptyString(taskData));
    while (result == NULL)
    {
        WIN32_FIND_DATA *pFind = &(pData->lastFind);
        if (!((pFind->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) &&
            (lstrcmp(pFind->cFileName, _T(".")) == 0 ||
                lstrcmp(pFind->cFileName, _T("..")) == 0)))
        {
            result = SAVE(C_string_to_Poly(taskData, pFind->cFileName));
        }
        /* Get the next entry. */
        if (!FindNextFile(pData->hFind, pFind))
        {
            DWORD dwErr = GetLastError();
            if (dwErr == ERROR_NO_MORE_FILES)
            {
                pData->fFindSucceeded = 0;
                if (result == NULL) return SAVE(EmptyString(taskData));
            }
        }
    }
    return result;
}

Handle rewindDirectory(TaskData *taskData, Handle stream, Handle dirname)
{
    WinDirData *pData = *(WinDirData**)(stream->WordP()); // In a SysWord
    if (pData == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
    // There's no rewind - close and reopen.
    FindClose(pData->hFind);
    POLYUNSIGNED length = PolyStringLength(dirname->Word());
    TempString dirName((TCHAR*)malloc((length + 3) * sizeof(TCHAR)));
    if (dirName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    Poly_string_to_C(dirname->Word(), dirName, length + 2);
    // Tack on \* to the end so that we find all files in the directory.
    lstrcat(dirName, _T("\\*"));
    HANDLE hFind = FindFirstFile(dirName, &(pData->lastFind));
    if (hFind == INVALID_HANDLE_VALUE)
        raise_syscall(taskData, "FindFirstFile failed", GetLastError());
    pData->hFind = hFind;
    /* There must be at least one file which matched. */
    pData->fFindSucceeded = 1;
    return Make_fixed_precision(taskData, 0);
}

static Handle closeDirectory(TaskData *taskData, Handle stream)
{
    WinDirData *pData = *(WinDirData**)(stream->WordP()); // In a SysWord
    if (pData != 0)
    {
        FindClose(pData->hFind);
        delete(pData);
        *((WinDirData**)stream->WordP()) = 0; // Clear this - no longer valid
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
    if (SetCurrentDirectory(cDirName) == FALSE)
       raise_syscall(taskData, "SetCurrentDirectory failed", GetLastError());
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
    DWORD dwRes = GetFileAttributes(cDirName);
    if (dwRes == 0xFFFFFFFF)
        raise_syscall(taskData, "GetFileAttributes failed", GetLastError());
    if (dwRes & FILE_ATTRIBUTE_DIRECTORY)
        return Make_fixed_precision(taskData, 1);
    else return Make_fixed_precision(taskData, 0);
}

/* Get absolute canonical path name. */
Handle fullPath(TaskData *taskData, Handle filename)
{
    TempString cFileName;

    /* Special case of an empty string. */
    if (PolyStringLength(filename->Word()) == 0) cFileName = _tcsdup(_T("."));
    else cFileName = Poly_string_to_T_alloc(filename->Word());
    if (cFileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);

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

/* Get file modification time.  This returns the value in the
   time units and from the base date used by timing.c. c.f. filedatec */
Handle modTime(TaskData *taskData, Handle filename)
{
    TempString cFileName(filename->Word());
    if (cFileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    /* There are several ways to get this information.  The best is to
    *  open the file and use GetFileTime.  Other possibilities are
    *  GetFileAttributeEx and FindFirstFile.  If the path is a
    *  symlink GetFileAttributeEx returns the times for the
    *  symlink itself.  The definition of OS.FileSys.modTime
    *  doesn't actually say which we should return but the Unix
    *  version uses stat rather than lstat.
    */
    DWORD dwRes = GetFileAttributes(cFileName);
    if (dwRes == 0xFFFFFFFF)
        raise_syscall(taskData, "GetFileAttributes failed", GetLastError());
    HANDLE hFile = CreateFile(cFileName, GENERIC_READ, FILE_SHARE_READ,
        NULL, OPEN_EXISTING, dwRes & FILE_ATTRIBUTE_DIRECTORY ? FILE_FLAG_BACKUP_SEMANTICS : 0, NULL);
    if (hFile == INVALID_HANDLE_VALUE)
        raise_syscall(taskData, "CreateFile failed", GetLastError());
    FILETIME lastWrite;
    if (! GetFileTime(hFile, NULL, NULL, &lastWrite))
    {
        DWORD dwErr = GetLastError();
        CloseHandle(hFile);
        raise_syscall(taskData, "GetFileTime failed", dwErr);
    }
    CloseHandle(hFile);
    return Make_arb_from_Filetime(taskData, lastWrite);
}

/* Get file size. */
Handle fileSize(TaskData *taskData, Handle filename)
{
    TempString cFileName(filename->Word());
    if (cFileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    /* Similar to modTime*/
    DWORD dwRes = GetFileAttributes(cFileName);
    if (dwRes == 0xFFFFFFFF)
        raise_syscall(taskData, "GetFileAttributes failed", GetLastError());
    HANDLE hFile = CreateFile(cFileName, GENERIC_READ, FILE_SHARE_READ,
        NULL, OPEN_EXISTING, dwRes & FILE_ATTRIBUTE_DIRECTORY ? FILE_FLAG_BACKUP_SEMANTICS : 0, NULL);
    if (hFile == INVALID_HANDLE_VALUE)
        raise_syscall(taskData, "CreateFile failed", GetLastError());
    LARGE_INTEGER fileSize;
    if (!GetFileSizeEx(hFile, &fileSize))
    {
        DWORD dwErr = GetLastError();
        CloseHandle(hFile);
        raise_syscall(taskData, "GetFileSizeEx failed", dwErr);
    }
    CloseHandle(hFile);
    return Make_arb_from_32bit_pair(taskData, fileSize.HighPart, fileSize.LowPart);
}

/* Set file modification and access times. */
Handle setTime(TaskData *taskData, Handle fileName, Handle fileTime)
{
    TempString cFileName(fileName->Word());
    if (cFileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);

    // The only way to set the time is to open the file and use SetFileTime.
    FILETIME ft;
    /* Get the file time. */
    getFileTimeFromArb(taskData, fileTime, &ft);
    DWORD dwRes = GetFileAttributes(cFileName);
    if (dwRes == 0xFFFFFFFF)
        raise_syscall(taskData, "GetFileAttributes failed", GetLastError());
    // Open an existing file with write access. We need that for SetFileTime.
    // Directories require FILE_FLAG_BACKUP_SEMANTICS.
    HANDLE hFile = CreateFile(cFileName, GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
        dwRes & FILE_ATTRIBUTE_DIRECTORY ? FILE_FLAG_BACKUP_SEMANTICS : FILE_ATTRIBUTE_NORMAL, NULL);
    if (hFile == INVALID_HANDLE_VALUE)
        raise_syscall(taskData, "CreateFile failed", GetLastError());
    /* Set the file time. */
    if (!SetFileTime(hFile, NULL, &ft, &ft))
    {
        DWORD nErr = GetLastError();
        CloseHandle(hFile);
        raise_syscall(taskData, "SetFileTime failed", nErr);
    }
    CloseHandle(hFile);
    return Make_fixed_precision(taskData, 0);
}

/* Rename a file. */
Handle renameFile(TaskData *taskData, Handle oldFileName, Handle newFileName)
{
    TempString oldName(oldFileName->Word()), newName(newFileName->Word());
    if (oldName == 0 || newName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    if (! MoveFileEx(oldName, newName, MOVEFILE_REPLACE_EXISTING))
        raise_syscall(taskData, "MoveFileEx failed", GetLastError());
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



/* IO_dispatchc.  Called from assembly code module. */
static Handle IO_dispatch_c(TaskData *taskData, Handle args, Handle strm, Handle code)
{
    unsigned c = get_C_unsigned(taskData, DEREFWORD(code));
    switch (c)
    {
    case 0: // Return standard input. 
        // This and the next two are normally only called once during start-up.
        // The exception is when we build the basis library during bootstrap.
        // We need to maintain the invariant that each WinStream object is referenced
        // by precisely one volatile word in order to be able to delete it when we close it.
    {
        if (standardInputValue != 0) return taskData->saveVec.push(standardInputValue);
        Handle stdStrm = MakeVolatileWord(taskData, standardInput);
        standardInputValue = stdStrm->WordP();
        return stdStrm;
    }
    case 1: // Return standard output
    {
        if (standardOutputValue != 0) return taskData->saveVec.push(standardOutputValue);
        Handle stdStrm = MakeVolatileWord(taskData, standardOutput);
        standardOutputValue = stdStrm->WordP();
        return stdStrm;
    }
    case 2: // Return standard error
    {
        if (standardErrorValue != 0) return taskData->saveVec.push(standardErrorValue);
        Handle stdStrm = MakeVolatileWord(taskData, standardError);
        standardErrorValue = stdStrm->WordP();
        return stdStrm;
    }
    case 3: /* Open file for text input. */
        return openWinFile(taskData, args, OPENREAD, false, false);
    case 4: /* Open file for binary input. */
        return openWinFile(taskData, args, OPENREAD, false, true);
    case 5: /* Open file for text output. */
        return openWinFile(taskData, args, OPENWRITE, false, false);
    case 6: /* Open file for binary output. */
        return openWinFile(taskData, args, OPENWRITE, false, true);
    case 13: /* Open text file for appending. */
             /* The IO library definition leaves it open whether this
             should use "append mode" or not.  */
        return openWinFile(taskData, args, OPENWRITE, true, false);
    case 14: /* Open binary file for appending. */
        return openWinFile(taskData, args, OPENWRITE, true, true);
    case 7: /* Close file */
    {
        // During the bootstrap we will have old format references.
        if (strm->Word().IsTagged())
            return Make_fixed_precision(taskData, 0);
        WinStream *stream = *(WinStream **)(strm->WordP());
        // May already have been closed.
        if (stream != 0)
        {
            try {
                stream->closeEntry(taskData);
                delete(stream);
                *(WinStream **)(strm->WordP()) = 0;
            }
            catch (...) {
                // If there was an error and we've raised an exception.
                delete(stream);
                *(WinStream **)(strm->WordP()) = 0;
                throw;
            }
        }
        return Make_fixed_precision(taskData, 0);
    }
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
    case 15: /* Return recommended buffer size. */
             // This is a guess but 4k seems reasonable.
        return Make_fixed_precision(taskData, 4096);

    case 16: /* See if we can get some input. */
    {
        WinStream *stream = *(WinStream **)(strm->WordP());
        if (stream == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
        return Make_fixed_precision(taskData, stream->testForInput(taskData, 0) ? 1 : 0);
    }

    case 17: // Return the number of bytes available. PrimIO.avail.
    {
        WinStream *stream = *(WinStream**)(strm->WordP());
        if (stream == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
        uint64_t endOfStream = stream->fileSize(taskData); // May raise an exception if this isn't a file.
        uint64_t current = stream->getPos(taskData);
        return Make_fixed_precision(taskData, endOfStream - current);
    }

    case 18: // Get position on stream.  PrimIO.getPos
    {
        WinStream *stream = *(WinStream**)(strm->WordP());
        if (stream == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
        // Get the current position in the stream.  This is used to test
        // for the availability of random access so it should raise an
        // exception if setFilePos or endFilePos would fail. 
        return Make_arbitrary_precision(taskData, stream->getPos(taskData));
    }

    case 19: // Seek to position on stream.  PrimIO.setPos
    {
        WinStream *stream = *(WinStream**)(strm->WordP());
        if (stream == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
        // TODO: This doesn't necessarily return a 64-bit value.
        uint64_t position = (uint64_t)getPolyUnsigned(taskData, DEREFWORD(args));
        stream->setPos(taskData, position);
        return Make_arbitrary_precision(taskData, 0);
    }

    case 20: // Return position at end of stream.  PrimIO.endPos.
    {
        WinStream *stream = *(WinStream**)(strm->WordP());
        if (stream == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
        return Make_arbitrary_precision(taskData, stream->fileSize(taskData));
    }

    case 21: /* Get the kind of device underlying the stream. */
    {
        WinStream *stream = *(WinStream**)(strm->WordP());
        if (stream == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
        return Make_fixed_precision(taskData, stream->fileKind());
    }
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
    {
        WinStream *stream = *(WinStream **)(strm->WordP());
        if (stream == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
        // We should check for interrupts even if we're not going to block.
        processes->TestAnyEvents(taskData);
        stream->waitUntilAvailable(taskData);
        return Make_fixed_precision(taskData, 0);
    }

    case 28: /* Test whether output is possible. */
    {
        WinStream *stream = *(WinStream **)(strm->WordP());
        if (stream == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
        return Make_fixed_precision(taskData, stream->testForOutput(taskData, 0) ? 1 : 0);
    }

    case 29: /* Block until output is possible. */
    {
        WinStream *stream = *(WinStream **)(strm->WordP());
        if (stream == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
        // We should check for interrupts even if we're not going to block.
        processes->TestAnyEvents(taskData);
        stream->waitUntilOutputPossible(taskData);
        return Make_fixed_precision(taskData, 0);
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
        DWORD space = GetCurrentDirectory(0, NULL);
        if (space == 0)
            raise_syscall(taskData, "GetCurrentDirectory failed", GetLastError());
        TempString buff((TCHAR*)malloc(space * sizeof(TCHAR)));
        if (buff == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
        if (GetCurrentDirectory(space, buff) == 0)
            raise_syscall(taskData, "GetCurrentDirectory failed", GetLastError());
        return SAVE(C_string_to_Poly(taskData, buff));
    }

    case 55: /* Create a new directory. */
    {
        TempString dirName(args->Word());
        if (dirName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
        if (!CreateDirectory(dirName, NULL))
            raise_syscall(taskData, "CreateDirectory failed", GetLastError());
        return Make_fixed_precision(taskData, 0);
    }

    case 56: /* Delete a directory. */
    {
        TempString dirName(args->Word());
        if (dirName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
        if (!RemoveDirectory(dirName))
            raise_syscall(taskData, "RemoveDirectory failed", GetLastError());
        return Make_fixed_precision(taskData, 0);
    }

    case 57: /* Test for directory. */
        return isDir(taskData, args);

    case 58: /* Test for symbolic link. */
    {
        TempString fileName(args->Word());
        if (fileName == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
        DWORD dwRes = GetFileAttributes(fileName);
        if (dwRes == 0xFFFFFFFF)
            raise_syscall(taskData, "GetFileAttributes failed", GetLastError());
        return Make_fixed_precision(taskData, (dwRes & FILE_ATTRIBUTE_REPARSE_POINT) ? 1 : 0);
    }

    case 59: /* Read a symbolic link. */
    {
        // Windows has added symbolic links but reading the target is far from
        // straightforward.   It's probably not worth trying to implement this.
        raise_syscall(taskData, "Symbolic links are not implemented", 0);
        return taskData->saveVec.push(TAGGED(0)); /* To keep compiler happy. */
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
        if (!DeleteFile(fileName))
            raise_syscall(taskData, "DeleteFile failed", GetLastError());
        return Make_fixed_precision(taskData, 0);
    }

    case 65: /* rename a file. */
        return renameFile(taskData, strm, args);

    case 66: /* Get access rights. */
        return fileAccess(taskData, strm, args);

    case 67: /* Return a temporary file name. */
    {
        DWORD dwSpace = GetTempPath(0, NULL);
        if (dwSpace == 0)
            raise_syscall(taskData, "GetTempPath failed", GetLastError());
        TempString buff((TCHAR*)malloc((dwSpace + 12) * sizeof(TCHAR)));
        if (buff == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
        if (GetTempPath(dwSpace, buff) == 0)
            raise_syscall(taskData, "GetTempPath failed", GetLastError());
        lstrcat(buff, _T("MLTEMPXXXXXX"));
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
        /* This concept does not exist in Windows. */
        /* Return a negative number. This is interpreted
        as "not implemented". */
        return Make_fixed_precision(taskData, -1);
    }

    case 69:
    {
        // Return an index for a token.  It is used in OS.IO.hash.
        // This is supposed to be well distributed for any 2^n but simply return
        // the low order part of the object address.
        WinStream *stream = *(WinStream **)(strm->WordP());
        if (stream == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
        return Make_fixed_precision(taskData, (POLYUNSIGNED)((uintptr_t)(stream)) & 0xfffffff);
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
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyTestForInput(POLYUNSIGNED threadId, POLYUNSIGNED strm, POLYUNSIGNED waitMillisecs)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    bool result = false;

    try {
        WinStream *stream = *(WinStream **)(PolyWord::FromUnsigned(strm).AsObjPtr());
        if (stream == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
        result = stream->testForInput(taskData, (unsigned)PolyWord::FromUnsigned(waitMillisecs).UnTaggedUnsigned());
    }
    catch (KillException &) {
        processes->ThreadExit(taskData); // TestAnyEvents may test for kill
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(result? 1: 0).AsUnsigned();
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyTestForOutput(POLYUNSIGNED threadId, POLYUNSIGNED strm, POLYUNSIGNED waitMillisecs)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    bool result = false;

    try {
        WinStream *stream = *(WinStream **)(PolyWord::FromUnsigned(strm).AsObjPtr());
        if (stream == 0) raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
        result = stream->testForOutput(taskData, (unsigned)PolyWord::FromUnsigned(waitMillisecs).UnTaggedUnsigned());
    }
    catch (KillException &) {
        processes->ThreadExit(taskData); // TestAnyEvents may test for kill
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(result ? 1 : 0).AsUnsigned();
}

struct _entrypts basicIOEPT[] =
{
    { "PolyChDir",                      (polyRTSFunction)&PolyChDir },
    { "PolyBasicIOGeneral",             (polyRTSFunction)&PolyBasicIOGeneral },
    { "PolyPollIODescriptors",          (polyRTSFunction)&PolyPollIODescriptors },
    { "PolyTestForInput",               (polyRTSFunction)&PolyTestForInput },
    { "PolyTestForOutput",              (polyRTSFunction)&PolyTestForOutput },

    { NULL, NULL } // End of list.
};

class WinBasicIO : public RtsModule
{
public:
    virtual void Start(void);
    virtual void GarbageCollect(ScanAddress * /*process*/);
};

// Declare this.  It will be automatically added to the table.
static WinBasicIO basicIOModule;

void WinBasicIO::Start(void)
{
}

void WinBasicIO::GarbageCollect(ScanAddress *process)
{
    if (standardInputValue != 0)
        process->ScanRuntimeAddress(&standardInputValue, ScanAddress::STRENGTH_STRONG);
    if (standardOutputValue != 0)
        process->ScanRuntimeAddress(&standardOutputValue, ScanAddress::STRENGTH_STRONG);
    if (standardErrorValue != 0)
        process->ScanRuntimeAddress(&standardErrorValue, ScanAddress::STRENGTH_STRONG);
}