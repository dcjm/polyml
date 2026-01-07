/*
    Title:      Process environment.

    Copyright (c) 2000-8, 2016-17, 2020, 2025

        David C. J. Matthews

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

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#if (defined(__CYGWIN__) || defined(_WIN32))
#include <process.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

// Include this next before errors.h since in WinCE at least the winsock errors are defined there.
#if (defined(_WIN32))
#include <winsock2.h>
#include <tchar.h>
#define NOMEMORY        ERROR_NOT_ENOUGH_MEMORY
#undef ENOMEM
#else
typedef char TCHAR;
#define _tgetenv getenv
#define NOMEMORY ENOMEM
#endif

#include "globals.h"
#include "sys.h"
#include "run_time.h"
#include "process_env.h"
#include "arb.h"
#include "mpoly.h"
#include "gc.h"
#include "scanaddrs.h"
#include "polystring.h"
#include "save_vec.h"
#include "process_env.h"
#include "rts_module.h"
#include "machine_dep.h"
#include "processes.h"
#include "locking.h"
#include "errors.h"
#include "rtsentry.h"
#include "version.h"

extern "C" {
    POLYEXTERNALSYMBOL void PolyFinish(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL void PolyTerminate(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyProcessEnvGeneral(POLYUNSIGNED threadId, POLYUNSIGNED code, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyProcessEnvErrorName(POLYUNSIGNED threadId, POLYUNSIGNED syserr);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyProcessEnvErrorMessage(POLYUNSIGNED threadId, POLYUNSIGNED syserr);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyProcessEnvErrorFromString(POLYUNSIGNED threadId, POLYUNSIGNED string);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetMaxAllocationSize();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetMaxStringSize();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetPolyVersionNumber();
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetFunctionName(POLYUNSIGNED threadId, POLYUNSIGNED fnAddr);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetProcessName(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetCommandlineArguments(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetEnv(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetEnvironment(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyProcessEnvSuccessValue(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyProcessEnvFailureValue(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyProcessEnvSystem(POLYUNSIGNED threadId, POLYUNSIGNED arg);
}

#define SAVE(x) taskData->saveVec.push(x)
#define ALLOC(n) alloc_and_save(taskData, n)

#if (defined(_WIN32))
#define ISPATHSEPARATOR(c)  ((c) == '\\' || (c) == '/')
#define DEFAULTSEPARATOR    "\\"
#else
#define ISPATHSEPARATOR(c)  ((c) == '/')
#define DEFAULTSEPARATOR    "/"
#endif

#ifdef _MSC_VER
// Don't tell me about ISO C++ changes.
#pragma warning(disable:4996)
#endif

// "environ" is declared in the headers on some systems but not all.
// Oddly, declaring it within process_env_dispatch_c causes problems
// on mingw where "environ" is actually a function.
#if __APPLE__
// On Mac OS X there may be problems accessing environ directly.
#include <crt_externs.h>
#define environ (*_NSGetEnviron())
#else
extern char **environ;
#endif

#ifdef __CYGWIN__
// Cygwin requires spawnvp to avoid the significant overhead of vfork
// but it doesn't seem to be thread-safe.  Run it on the main thread
// to be sure.
class CygwinSpawnRequest: public MainThreadRequest
{
public:
    CygwinSpawnRequest(char **argv): MainThreadRequest(MTP_CYGWINSPAWN), spawnArgv(argv) {}

    virtual void Perform();
    char **spawnArgv;
    int pid;
};

void CygwinSpawnRequest::Perform()
{
    pid = spawnvp(_P_NOWAIT, "/bin/sh", spawnArgv);
}

#endif

// These are now just legacy calls.
static Handle process_env_dispatch_c(TaskData *taskData, Handle args, Handle code)
{
    unsigned c = get_C_unsigned(taskData, DEREFWORD(code));
    switch (c)
    {
    case 1: /* Return the argument list. */
        // This is used in the pre-built compilers.
        return convert_string_list(taskData, userOptions.user_arg_count, userOptions.user_arg_strings);

    default:
        {
            char msg[100];
            snprintf(msg, sizeof(msg), "Unknown environment function: %d", c);
            raise_exception_string(taskData, EXC_Fail, msg);
            return 0;
        }
    }
}

// General interface to process-env.  Ideally the various cases will be made into
// separate functions.
POLYUNSIGNED PolyProcessEnvGeneral(POLYUNSIGNED threadId, POLYUNSIGNED code, POLYUNSIGNED arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedCode = taskData->saveVec.push(code);
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = process_env_dispatch_c(taskData, pushedArg, pushedCode);
    }
    catch (KillException &) {
        processes->ThreadExit(taskData); // May test for kill
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Terminate normally with a result code.
void PolyFinish(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    int i = get_C_int(taskData, PolyWord::FromUnsigned(arg));
    // Cause the other threads to exit and set the result code.
    processes->RequestProcessExit(i);
    // Exit this thread
    processes->ThreadExit(taskData); // Doesn't return.
}

// Terminate without running the atExit list or flushing buffers
void PolyTerminate(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    int i = get_C_int(taskData, PolyWord::FromUnsigned(arg));
    _exit(i); // Doesn't return.
}

// Get the name of a numeric error message.
POLYUNSIGNED PolyProcessEnvErrorName(POLYUNSIGNED threadId, POLYUNSIGNED syserr)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        int e = (int)PolyWord::FromUnsigned(syserr).AsObjPtr()->Get(0).AsSigned();
        // First look to see if we have the name in the error table. They should generally all be there.
        const char *errorMsg = stringFromErrorCode(e);
        if (errorMsg != NULL)
            result = taskData->saveVec.push(C_string_to_Poly(taskData, errorMsg));
        else
        { // If it isn't in the table.
            char buff[40];
            snprintf(buff, sizeof(buff), "ERROR%0d", e);
            result = taskData->saveVec.push(C_string_to_Poly(taskData, buff));
        }
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Get the explanatory message for an error. */
POLYUNSIGNED PolyProcessEnvErrorMessage(POLYUNSIGNED threadId, POLYUNSIGNED syserr)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = errorMsg(taskData, (int)PolyWord::FromUnsigned(syserr).AsObjPtr()->Get(0).AsSigned());
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Try to convert an error string to an error number.
POLYUNSIGNED PolyProcessEnvErrorFromString(POLYUNSIGNED threadId, POLYUNSIGNED string)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        char buff[40];
        // Get the string.
        Poly_string_to_C(PolyWord::FromUnsigned(string), buff, sizeof(buff));
        // Look the string up in the table.
        int err = 0;
        if (errorCodeFromString(buff, &err))
            result = Make_sysword(taskData, err);
        else if (strncmp(buff, "ERROR", 5) == 0)
        // If we don't find it then it may have been a constructed error name.
            result = Make_sysword(taskData, atoi(buff+5));
        else result = Make_sysword(taskData, 0); // Return 0w0 if it isn't there.

    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Return the maximum size of a cell that can be allocated on the heap.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetMaxAllocationSize()
{
    return TAGGED(MAX_OBJECT_SIZE).AsUnsigned();
}

// Return the maximum string size (in bytes).
// It is the maximum number of bytes in a segment less one word for the length field.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetMaxStringSize()
{
    return TAGGED((MAX_OBJECT_SIZE) * sizeof(PolyWord) - sizeof(PolyWord)).AsUnsigned();
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetPolyVersionNumber()
{
    return TAGGED(POLY_version_number).AsUnsigned();
}

// Return the function name associated with a piece of compiled code.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetFunctionName(POLYUNSIGNED threadId, POLYUNSIGNED fnAddr)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        if (PolyWord::FromUnsigned(fnAddr).IsTagged()) raise_fail(taskData, "Not a code pointer");
        PolyObject *pt = PolyWord::FromUnsigned(fnAddr).AsObjPtr();
        // In 32-in-64 this may be a closure and the first word is the absolute address of the code.
        if (pt->IsClosureObject())
        {
            // It may not be set yet.
            pt = *(PolyObject**)pt;
            if (((uintptr_t)pt & 1) == 1) raise_fail(taskData, "Not a code pointer");
        }
        if (pt->IsCodeObject()) /* Should now be a code object. */
        {
            /* Compiled code.  This is the first constant in the constant area. */
            PolyWord *codePt = machineDependent->ConstPtrForCode(pt);
            PolyWord name = codePt[0];
            /* May be zero indicating an anonymous segment - return null string. */
            if (name == PolyWord::FromUnsigned(0))
                result = taskData->saveVec.push(C_string_to_Poly(taskData, ""));
            else result = taskData->saveVec.push(name);
        }
        else raise_fail(taskData, "Not a code pointer");
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}


// Get the command line process name.
POLYUNSIGNED PolyGetProcessName(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = taskData->saveVec.push(C_string_to_Poly(taskData, userOptions.programName));
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Get the command line arguments.
POLYUNSIGNED PolyGetCommandlineArguments(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = convert_string_list(taskData, userOptions.user_arg_count, userOptions.user_arg_strings);
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

/* Return a string from the environment. */
POLYUNSIGNED PolyGetEnv(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        TempString buff(pushedArg->Word());
        if (buff == 0)
            raise_syscall(taskData, "Insufficient memory", NOMEMORY);
        TCHAR * res = _tgetenv(buff);
        if (res == NULL)
            raise_syscall(taskData, "Not Found", 0);
        result = taskData->saveVec.push(C_string_to_Poly(taskData, res));
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

// Return the whole environment.  Only available in Posix.ProcEnv.
POLYUNSIGNED PolyGetEnvironment(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        /* Count the environment strings */
        int env_count = 0;
        while (environ[env_count] != NULL) env_count++;
        result = convert_string_list(taskData, env_count, environ);
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

/* Return the success value. */
POLYUNSIGNED PolyProcessEnvSuccessValue(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = Make_fixed_precision(taskData, EXIT_SUCCESS);
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

/* Return a failure value. */
POLYUNSIGNED PolyProcessEnvFailureValue(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = Make_fixed_precision(taskData, EXIT_FAILURE);
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

/* Run command. */
POLYUNSIGNED PolyProcessEnvSystem(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        TempString buff(pushedArg->Word());
        if (buff == 0) raise_syscall(taskData, "Insufficient memory", NOMEMORY);
        int res = -1;
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        // Windows.
        TCHAR * argv[4];
        argv[0] = _tgetenv(_T("COMSPEC")); // Default CLI.
        if (argv[0] == 0) argv[0] = (TCHAR*)_T("cmd.exe"); // Win NT etc.
        argv[1] = (TCHAR*)_T("/c");
        argv[2] = buff;
        argv[3] = NULL;
        // If _P_NOWAIT is given the result is the process handle.
        // spawnvp does any necessary path searching if argv[0]
        // does not contain a full path.
        intptr_t pid = _tspawnvp(_P_NOWAIT, argv[0], argv);
        if (pid == -1)
            raise_syscall(taskData, "Function system failed", errno);
#else
        // Cygwin and Unix
        char* argv[4];
        argv[0] = (char*)"sh";
        argv[1] = (char*)"-c";
        argv[2] = buff;
        argv[3] = NULL;
#if (defined(__CYGWIN__))
        CygwinSpawnRequest request(argv);
        processes->MakeRootRequest(taskData, &request);
        int pid = request.pid;
        if (pid < 0)
            raise_syscall(taskData, "Function system failed", errno);
#else
        // We need to break this down so that we can unblock signals in the
        // child process.
        // The Unix "system" function seems to set SIGINT and SIGQUIT to
        // SIG_IGN in the parent so that the wait will not be interrupted.
        // That may make sense in a single-threaded application but is
        // that right here?
        int pid = vfork();
        if (pid == -1)
            raise_syscall(taskData, "Function system failed", errno);
        else if (pid == 0)
        { // In child
            sigset_t sigset;
            sigemptyset(&sigset);
            sigprocmask(SIG_SETMASK, &sigset, 0);
            // Reset other signals?
            execv("/bin/sh", argv);
            _exit(1);
        }
#endif
#endif
        while (true)
        {
            try
            {
                // Test to see if the child has returned.
#if (defined(_WIN32) && ! defined(__CYGWIN__))
                DWORD dwWait = WaitForSingleObject((HANDLE)pid, 0);
                if (dwWait == WAIT_OBJECT_0)
                {
                    DWORD dwResult;
                    BOOL fResult = GetExitCodeProcess((HANDLE)pid, &dwResult);
                    if (!fResult)
                        raise_syscall(taskData, "Function system failed", GetLastError());
                    CloseHandle((HANDLE)pid);
                    result = Make_fixed_precision(taskData, dwResult);
                    break;
                }
                else if (dwWait == WAIT_FAILED)
                    raise_syscall(taskData, "Function system failed", GetLastError());
                else
                {
                    // Wait for the process to exit or for the timeout
                    WaitHandle waiter((HANDLE)pid, 1000);
                    processes->ThreadPauseForIO(taskData, &waiter);
                }
#else
                int wRes = waitpid(pid, &res, WNOHANG);
                if (wRes > 0)
                    break;
                else if (wRes < 0)
                {
                    raise_syscall(taskData, "Function system failed", errno);
                }
                // In Unix the best we can do is wait.  This may be interrupted
                // by SIGCHLD depending on where signals are processed.
                // One possibility is for the main thread to somehow wake-up
                // the thread when it processes a SIGCHLD.
                else processes->ThreadPause(taskData);
#endif
            }
            catch (...)
            {
                // Either IOException or KillException.
                // We're abandoning the wait.  This will leave
                // a zombie in Unix.
#if (defined(_WIN32) && ! defined(__CYGWIN__))
                CloseHandle((HANDLE)pid);
#endif
                throw;
            }
        }
        result = Make_fixed_precision(taskData, res);

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

struct _entrypts processEnvEPT[] =
{
    { "PolyFinish",                     (polyRTSFunction)&PolyFinish},
    { "PolyTerminate",                  (polyRTSFunction)&PolyTerminate},
    { "PolyProcessEnvGeneral",          (polyRTSFunction)&PolyProcessEnvGeneral},
    { "PolyProcessEnvErrorName",        (polyRTSFunction)&PolyProcessEnvErrorName},
    { "PolyProcessEnvErrorMessage",     (polyRTSFunction)&PolyProcessEnvErrorMessage},
    { "PolyProcessEnvErrorFromString",  (polyRTSFunction)&PolyProcessEnvErrorFromString},
    { "PolyGetMaxAllocationSize",       (polyRTSFunction)&PolyGetMaxAllocationSize },
    { "PolyGetMaxStringSize",           (polyRTSFunction)&PolyGetMaxStringSize },
    { "PolyGetPolyVersionNumber",       (polyRTSFunction)&PolyGetPolyVersionNumber },
    { "PolyGetFunctionName",            (polyRTSFunction)&PolyGetFunctionName },
    { "PolyGetProcessName",             (polyRTSFunction)&PolyGetProcessName },
    { "PolyGetCommandlineArguments",    (polyRTSFunction)&PolyGetCommandlineArguments },
    { "PolyGetEnv",                     (polyRTSFunction)& PolyGetEnv },
    { "PolyGetEnvironment",             (polyRTSFunction)& PolyGetEnvironment },
    { "PolyProcessEnvSuccessValue",     (polyRTSFunction)& PolyProcessEnvSuccessValue },
    { "PolyProcessEnvFailureValue",     (polyRTSFunction)& PolyProcessEnvFailureValue },
    { "PolyProcessEnvSystem",           (polyRTSFunction)& PolyProcessEnvSystem },

    { NULL, NULL} // End of list.
};
