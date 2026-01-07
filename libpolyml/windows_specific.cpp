/*
    Title:      Operating Specific functions: Windows version.

    Copyright (c) 2000, 2015, 2018, 2019 David C. J. Matthews

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

#include <winsock2.h>
#include <windows.h>

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_TCHAR_H
#include <tchar.h>
#endif

#ifdef HAVE_IO_H
#include <io.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include <lmcons.h>

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#include <new>

#include "globals.h"
#include "arb.h"
#include "gc.h"
#include "run_time.h"
#include "io_internal.h"
#include "os_specific.h"
#include "sys.h"
#include "processes.h"
#include "winguiconsole.h"
#include "mpoly.h"
#include "diagnostics.h"
#include "scanaddrs.h"
#include "polystring.h"
#include "save_vec.h"
#include "rts_module.h"
#include "machine_dep.h"
#include "rtsentry.h"
#include "winstartup.h"

#define SAVE(x) taskData->saveVec.push(x)
#define SIZEOF(x) (sizeof(x)/sizeof(word))

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyWindowsExecute(POLYUNSIGNED threadId, POLYUNSIGNED command, POLYUNSIGNED argument);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyWindowsOpenProcessHandle(POLYUNSIGNED threadId, POLYUNSIGNED arg, POLYUNSIGNED isRead, POLYUNSIGNED isText);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyWindowsGetProcessResult(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyWindowsSimpleExecute(POLYUNSIGNED threadId, POLYUNSIGNED command, POLYUNSIGNED argument);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyWindowsDDEStartDialogue(POLYUNSIGNED threadId, POLYUNSIGNED service, POLYUNSIGNED topic);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyWindowsDDEExecute(POLYUNSIGNED threadId, POLYUNSIGNED info, POLYUNSIGNED commd);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyWindowsDDEClose(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetOSType();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyWindowsGetCodePage();
}

typedef struct {
    HANDLE hProcess, hInput, hOutput;
} PROCESSDATA;

// Start DDE dialogue.
POLYUNSIGNED PolyWindowsDDEStartDialogue(POLYUNSIGNED threadId, POLYUNSIGNED service, POLYUNSIGNED topic)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        TCHAR* serviceName = Poly_string_to_T_alloc(PolyWord::FromUnsigned(service));
        TCHAR* topicName = Poly_string_to_T_alloc(PolyWord::FromUnsigned(topic));
        /* Send a request to the main thread to do the work. */
        HCONV hcDDEConv = StartDDEConversation(serviceName, topicName);
        free(serviceName); free(topicName);
        if (hcDDEConv == 0) raise_syscall(taskData, "DdeConnect failed", 0);
        // Create an entry to return the conversation.
        result = MakeVolatileWord(taskData, hcDDEConv);

    }
    catch (KillException&) {
        processes->ThreadExit(taskData); // Call 1005 may test for kill
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Send DDE execute request.
POLYUNSIGNED PolyWindowsDDEExecute(POLYUNSIGNED threadId, POLYUNSIGNED info, POLYUNSIGNED commd)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    LRESULT res = 0;

    try {
        HCONV hcDDEConv = *(HCONV*)(PolyWord::FromUnsigned(info).AsObjPtr());
        if (hcDDEConv == 0) raise_syscall(taskData, "DDE Conversation is closed", 0);
        char* command = Poly_string_to_C_alloc(PolyWord::FromUnsigned(commd));
        // Send a request to the main thread to do the work.
        // The result is -1 if an error, 0 if busy, 1 if success
        res = ExecuteDDE(command, hcDDEConv);
        free(command);
        if (res == -1) raise_syscall(taskData, "DdeClientTransaction failed", 0);

    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    return TAGGED(res == 1 ? 1 : 0).AsUnsigned();
}

POLYUNSIGNED PolyWindowsDDEClose(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();

    try {
        HCONV hcDDEConv = *(HCONV*)(PolyWord::FromUnsigned(arg).AsObjPtr());
        if (hcDDEConv != 0)
        {
            CloseDDEConversation(hcDDEConv);
            *(void**)(PolyWord::FromUnsigned(arg).AsObjPtr()) = 0;
        }
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

POLYUNSIGNED PolyGetOSType()
{
    return TAGGED(1).AsUnsigned(); // Return 1 for Windows
}

// Return the current code page set by the --codepage argument.
// This allows Unicode conversions to use same conversions as everything else.
POLYUNSIGNED PolyWindowsGetCodePage()
{
#if defined(UNICODE)
    return TAGGED(codePage).AsUnsigned();
#else
    return TAGGED(CP_ACP).AsUnsigned();
#endif
}


/*
The Windows version of this is more complicated than the Unix version because
we can't manipulate the pipe handles in the child process.  Everything has to be
set up in the parent.  As with Unix we create two pipes and pass one end of each
pipe to the child.  The end we pass to the child is "inheritable" (i.e. duplicated
in the child as with Unix file descriptors) while the ends we keep in the parent
are non-inheritable (i.e. not duplicated in the child). 
DCJM: December 1999.
This now uses overlapped IO for the streams.
*/
static Handle execute(TaskData *taskData, PolyWord command, PolyWord argument)
{
    LPCSTR lpszError = "";
    HANDLE hWriteToChild = INVALID_HANDLE_VALUE,
           hReadFromParent = INVALID_HANDLE_VALUE,
           hWriteToParent = INVALID_HANDLE_VALUE,
           hReadFromChild = INVALID_HANDLE_VALUE;
    STARTUPINFO startupInfo;
    PROCESS_INFORMATION processInfo;
    PROCESSDATA *pProcData = 0;

    LPTSTR commandName = Poly_string_to_T_alloc(command);
    LPTSTR arguments = Poly_string_to_T_alloc(argument);

    TCHAR toChildPipeName[MAX_PATH], fromChildPipeName[MAX_PATH];
    newPipeName(toChildPipeName);
    newPipeName(fromChildPipeName);
    // Create the pipes as inheritable handles.  These will be passed to the child.
    SECURITY_ATTRIBUTES secure;
    secure.nLength = sizeof(SECURITY_ATTRIBUTES);
    secure.lpSecurityDescriptor = NULL;
    secure.bInheritHandle = TRUE;
    hReadFromParent =
        CreateNamedPipe(toChildPipeName, PIPE_ACCESS_INBOUND | FILE_FLAG_FIRST_PIPE_INSTANCE,
            PIPE_READMODE_BYTE | PIPE_WAIT | PIPE_REJECT_REMOTE_CLIENTS, 1, 4096, 4096, 0, &secure);
    if (hReadFromParent == INVALID_HANDLE_VALUE)
    {
        lpszError = "CreateNamedPipe failed";
        goto error;
    }
    hWriteToChild =
        CreateFile(toChildPipeName, GENERIC_WRITE, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OVERLAPPED, NULL);
    if (hWriteToChild == INVALID_HANDLE_VALUE)
    {
        lpszError = "CreateFile failed";
        goto error;
    }
    hWriteToParent =
        CreateNamedPipe(fromChildPipeName, PIPE_ACCESS_OUTBOUND | FILE_FLAG_FIRST_PIPE_INSTANCE,
            PIPE_READMODE_BYTE | PIPE_WAIT | PIPE_REJECT_REMOTE_CLIENTS, 1, 4096, 4096, 0, &secure);
    if (hWriteToParent == INVALID_HANDLE_VALUE)
    {
        lpszError = "CreateNamedPipe failed";
        goto error;
    }
    hReadFromChild =
        CreateFile(fromChildPipeName, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OVERLAPPED, NULL);
    if (hReadFromChild == INVALID_HANDLE_VALUE)
    {
        lpszError = "CreateFile failed";
        goto error;
    }

    // Create a STARTUPINFO structure in which to pass the pipes as stdin
    // and stdout to the new process.
    memset(&startupInfo, 0, sizeof(startupInfo));
    startupInfo.cb = sizeof(startupInfo);
    startupInfo.dwFlags = STARTF_USESTDHANDLES;
    startupInfo.hStdInput = hReadFromParent;
    startupInfo.hStdOutput = hWriteToParent;
    // What should we do about the stderr?  For the moment, inherit the original.
    startupInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);

    // Treat the empty string as NULL.  This is non-standard.
    if (!CreateProcess(commandName[0] == 0 ? NULL: commandName,
            arguments[0] == 0 ? NULL: arguments, // Command line
            NULL, NULL, TRUE, // Security attributes. Inherit handles
            CREATE_NO_WINDOW, // creation flags
            NULL, NULL, // Inherit our environment and directory
            &startupInfo,
            &processInfo)) {
        lpszError = "Could not create process";
        goto error;
    }
    pProcData = (PROCESSDATA *)malloc(sizeof(PROCESSDATA));
    if (pProcData == 0)
    {
        lpszError = "Insufficient memory";
        SetLastError(ERROR_NOT_ENOUGH_MEMORY);
        goto error;
    }

    pProcData->hProcess = processInfo.hProcess;
    pProcData->hInput = hReadFromChild;
    pProcData->hOutput = hWriteToChild;

    // Everything has gone well - remove what we don't want
    free(commandName);
    free(arguments);
    /* Close thread handle since we don't need it. */
    CloseHandle(processInfo.hThread);
    /* Close the sides of the pipes we don't use in the parent. */
    CloseHandle(hReadFromParent);
    CloseHandle(hWriteToParent);

    return(MakeVolatileWord(taskData, pProcData));

error:
    {
        int err = GetLastError();
        free(commandName);
        free(arguments);
        free(pProcData);
        // Close all the pipe handles.
        if (hWriteToChild != INVALID_HANDLE_VALUE) CloseHandle(hWriteToChild);
        if (hReadFromParent != INVALID_HANDLE_VALUE) CloseHandle(hReadFromParent);
        if (hWriteToParent != INVALID_HANDLE_VALUE) CloseHandle(hWriteToParent);
        if (hReadFromChild != INVALID_HANDLE_VALUE) CloseHandle(hReadFromChild);
        raise_syscall(taskData, lpszError, err);
        return NULL; // Never reached.
    }
}

// Execute a command.
POLYUNSIGNED PolyWindowsExecute(POLYUNSIGNED threadId, POLYUNSIGNED command, POLYUNSIGNED argument)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = execute(taskData, PolyWord::FromUnsigned(command), PolyWord::FromUnsigned(argument));
    }
    catch (KillException&) {
        processes->ThreadExit(taskData); // Call 1005 may test for kill
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

static Handle simpleExecute(TaskData *taskData, PolyWord command, PolyWord argument)
{
    HANDLE hNull = INVALID_HANDLE_VALUE;
    PROCESS_INFORMATION processInfo;
    TCHAR *commandName = Poly_string_to_T_alloc(command);
    TCHAR *arguments = Poly_string_to_T_alloc(argument);

    STARTUPINFO startupInfo;
    // Open a handle to NUL for input and output.
    hNull = CreateFile(_T("NUL"), GENERIC_READ|GENERIC_WRITE,
                FILE_SHARE_READ|FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
                FILE_ATTRIBUTE_NORMAL, NULL);

    // Create a STARTUPINFO structure in which to pass hNULL as stdin
    // and stdout to the new process.
    // TODO: The handles should really be open on "NUL".
    memset(&startupInfo, 0, sizeof(startupInfo));
    startupInfo.cb = sizeof(startupInfo);
    startupInfo.dwFlags = STARTF_USESTDHANDLES;
    startupInfo.hStdInput = hNull;
    startupInfo.hStdOutput = hNull;
    startupInfo.hStdError = hNull;
    STARTUPINFO *start = &startupInfo;

    // Treat the empty string as NULL.  This is non-standard.
    if (!CreateProcess(commandName[0] == 0 ? NULL : commandName,
            arguments[0] == 0 ? NULL : arguments, // Command line
            NULL, NULL, // Security attributes
            TRUE, CREATE_NO_WINDOW, // Inherit handles, creation flags
            NULL, NULL, // Inherit our environment and directory
            start,
            &processInfo)) {
        int nErr = GetLastError();
        // Clean up
        free(commandName);
        free(arguments);
        CloseHandle(hNull);
        raise_syscall(taskData, "CreateProcess failed", nErr);
    }

    free(commandName);
    free(arguments);
    /* Close thread handle since we don't need it. */
    CloseHandle(processInfo.hThread);
#ifndef _WIN32_WCE
    CloseHandle(hNull); // We no longer need this
#endif

    PROCESSDATA *pProcData = (PROCESSDATA *)malloc(sizeof(PROCESSDATA));
    if (pProcData == 0)
        raise_syscall(taskData, "Insufficient memory", ERROR_NOT_ENOUGH_MEMORY);

    pProcData->hProcess = processInfo.hProcess;
    // We only use the process handle entry.
    pProcData->hInput = INVALID_HANDLE_VALUE;
    pProcData->hOutput = INVALID_HANDLE_VALUE;

    return(MakeVolatileWord(taskData, pProcData));
}

POLYUNSIGNED PolyWindowsSimpleExecute(POLYUNSIGNED threadId, POLYUNSIGNED command, POLYUNSIGNED argument)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = simpleExecute(taskData, PolyWord::FromUnsigned(command), PolyWord::FromUnsigned(argument));
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Return a stream, either text or binary, connected to an open process. */
POLYUNSIGNED PolyWindowsOpenProcessHandle(POLYUNSIGNED threadId, POLYUNSIGNED arg, POLYUNSIGNED isRead, POLYUNSIGNED isText)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        PROCESSDATA* hnd = *(PROCESSDATA * *)(PolyWord::FromUnsigned(arg).AsObjPtr());
        if (hnd == 0)
            raise_syscall(taskData, "Process is closed", ERROR_INVALID_HANDLE);
        // We allow multiple streams on the handles.  Since they are duplicated by openHandle that's safe.
        // A consequence is that closing the stream does not close the pipe as far as the child is
        // concerned.  That only happens when we close the final handle in reap.
        try
        {
            WinInOutStream* stream = new WinInOutStream;
            bool fResult;
            if (PolyWord::FromUnsigned(isRead).UnTagged())
                fResult = stream->openHandle(hnd->hInput, OPENREAD, PolyWord::FromUnsigned(isText).UnTagged());
            else fResult = stream->openHandle(hnd->hOutput, OPENWRITE, PolyWord::FromUnsigned(isText).UnTagged());
            if (!fResult)
            {
                delete(stream);
                raise_syscall(taskData, "openHandle failed", GetLastError());
            }

            result = MakeVolatileWord(taskData, stream);
        }
        catch (std::bad_alloc&)
        {
            raise_syscall(taskData, "Insufficient memory", ERROR_NOT_ENOUGH_MEMORY);
        }

    }
    catch (KillException&) {
        processes->ThreadExit(taskData); // Call 1005 may test for kill
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Get result of process. */
POLYUNSIGNED PolyWindowsGetProcessResult(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        PROCESSDATA* hnd = *(PROCESSDATA * *)(PolyWord::FromUnsigned(arg).AsObjPtr());
        *(PROCESSDATA * *)(PolyWord::FromUnsigned(arg).AsObjPtr()) = 0; // Mark as inaccessible.
        if (hnd == 0)
            raise_syscall(taskData, "Process is closed", ERROR_INVALID_HANDLE);
        // Close the streams. Either of them may have been
        // passed to the stream package.
        if (hnd->hInput != INVALID_HANDLE_VALUE)
            CloseHandle(hnd->hInput);
        hnd->hInput = INVALID_HANDLE_VALUE;
        if (hnd->hOutput != INVALID_HANDLE_VALUE)
            CloseHandle(hnd->hOutput);
        hnd->hOutput = INVALID_HANDLE_VALUE;

        // See if it's finished.
        while (true) {
            DWORD dwResult;
            if (GetExitCodeProcess(hnd->hProcess, &dwResult) == 0)
                raise_syscall(taskData, "GetExitCodeProcess failed", GetLastError());
            if (dwResult != STILL_ACTIVE) {
                // Finished - return the result.
                // Remove the process object.  The result is cached in ML.
                free(hnd);
                result = Make_fixed_precision(taskData, dwResult);
                break;
            }
            // Block and try again.
            WaitHandle waiter(hnd->hProcess, 1000);
            processes->ThreadPauseForIO(taskData, &waiter);
        }

    }
    catch (KillException&) {
        processes->ThreadExit(taskData); // May test for kill
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

struct _entrypts osSpecificEPT[] =
{
    { "PolyGetOSType",                  (polyRTSFunction)&PolyGetOSType},
    { "PolyWindowsExecute",             (polyRTSFunction)& PolyWindowsExecute},
    { "PolyWindowsOpenProcessHandle",   (polyRTSFunction)& PolyWindowsOpenProcessHandle},
    { "PolyWindowsGetProcessResult",    (polyRTSFunction)& PolyWindowsGetProcessResult},
    { "PolyWindowsSimpleExecute",       (polyRTSFunction)& PolyWindowsSimpleExecute},
    { "PolyWindowsDDEStartDialogue",    (polyRTSFunction)& PolyWindowsDDEStartDialogue},
    { "PolyWindowsDDEExecute",          (polyRTSFunction)& PolyWindowsDDEExecute},
    { "PolyWindowsDDEClose",            (polyRTSFunction)& PolyWindowsDDEClose},
    { "PolyWindowsGetCodePage",         (polyRTSFunction)& PolyWindowsGetCodePage},

    { NULL, NULL} // End of list.
};
