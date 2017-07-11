/*
    Title:      Process environment.
    Copyright (c) 2000-8, 2016
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
#if (defined(_WIN32) && ! defined(__CYGWIN__))
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
    POLYEXTERNALSYMBOL void PolyFinish(PolyObject *threadId, PolyWord arg);
    POLYEXTERNALSYMBOL void PolyTerminate(PolyObject *threadId, PolyWord arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyProcessEnvGeneral(PolyObject *threadId, PolyWord code, PolyWord arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyProcessEnvErrorName(PolyObject *threadId, PolyWord syserr);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyProcessEnvErrorMessage(PolyObject *threadId, PolyWord syserr);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyProcessEnvErrorFromString(PolyObject *threadId, PolyWord string);
}

#define SAVE(x) mdTaskData->saveVec.push(x)
#define ALLOC(n) alloc_and_save(mdTaskData, n)

#if (defined(_WIN32) && ! defined(__CYGWIN__))
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

/* Functions registered with atExit are added to this list. */
static PolyWord at_exit_list = TAGGED(0);
/* Once "exit" is called this flag is set and no further
   calls to atExit are allowed. */
static bool exiting = false;

static PLock atExitLock; // Thread lock for above.

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

static Handle process_env_dispatch_c(TaskData *mdTaskData, Handle args, Handle code)
{
    unsigned c = get_C_unsigned(mdTaskData, DEREFWORDHANDLE(code));
    switch (c)
    {
    case 0: /* Return the program name. */
        return SAVE(C_string_to_Poly(mdTaskData, userOptions.programName));

    case 1: /* Return the argument list. */
        return convert_string_list(mdTaskData, userOptions.user_arg_count, userOptions.user_arg_strings);

    case 14: /* Return a string from the environment. */
        {
            TempString buff(args->Word());
            if (buff == 0) raise_syscall(mdTaskData, "Insufficient memory", NOMEMORY);
            TCHAR *res = _tgetenv(buff);
            if (res == NULL) raise_syscall(mdTaskData, "Not Found", 0);
            else return SAVE(C_string_to_Poly(mdTaskData, res));
        }

    case 21: // Return the whole environment.  Only available in Posix.ProcEnv.
        {
            /* Count the environment strings */
            int env_count = 0;
            while (environ[env_count] != NULL) env_count++;
            return convert_string_list(mdTaskData, env_count, environ);
        }

    case 15: /* Return the success value. */
        return Make_fixed_precision(mdTaskData, EXIT_SUCCESS);

    case 16: /* Return a failure value. */
        return Make_fixed_precision(mdTaskData, EXIT_FAILURE);

    case 17: /* Run command. */
        {
            TempString buff(args->Word());
            if (buff == 0) raise_syscall(mdTaskData, "Insufficient memory", NOMEMORY);
            int res = -1;
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            // Windows.
            TCHAR *argv[4];
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
                raise_syscall(mdTaskData, "Function system failed", errno);
#else
            // Cygwin and Unix
            char *argv[4];
            argv[0] = (char*)"sh";
            argv[1] = (char*)"-c";
            argv[2] = buff;
            argv[3] = NULL;
#if (defined(__CYGWIN__))
            CygwinSpawnRequest request(argv);
            processes->MakeRootRequest(mdTaskData, &request);
            int pid = request.pid;
            if (pid < 0)
                raise_syscall(mdTaskData, "Function system failed", errno);
#else
            // We need to break this down so that we can unblock signals in the
            // child process.
            // The Unix "system" function seems to set SIGINT and SIGQUIT to
            // SIG_IGN in the parent so that the wait will not be interrupted.
            // That may make sense in a single-threaded application but is
            // that right here?
            int pid = vfork();
            if (pid == -1)
                raise_syscall(mdTaskData, "Function system failed", errno);
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
                    switch (WaitForSingleObject((HANDLE)pid, 0))
                    {
                    case WAIT_OBJECT_0:
                        {
                            DWORD result;
                            BOOL fResult = GetExitCodeProcess((HANDLE)pid, &result);
                            if (! fResult)
                                raise_syscall(mdTaskData, "Function system failed", GetLastError());
                            CloseHandle((HANDLE)pid);
                            return Make_fixed_precision(mdTaskData, result);
                        }
                    case WAIT_FAILED:
                        raise_syscall(mdTaskData, "Function system failed", GetLastError());
                    }
                    // Wait for the process to exit or for the timeout
                    WaitHandle waiter((HANDLE)pid);
                    processes->ThreadPauseForIO(mdTaskData, &waiter);
#else
                    int wRes = waitpid(pid, &res, WNOHANG);
                    if (wRes > 0)
                        break;
                    else if (wRes < 0)
                    {
                        raise_syscall(mdTaskData, "Function system failed", errno);
                    }
                    // In Unix the best we can do is wait.  This may be interrupted
                    // by SIGCHLD depending on where signals are processed.
                    // One possibility is for the main thread to somehow wake-up
                    // the thread when it processes a SIGCHLD.
                    processes->ThreadPause(mdTaskData);
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
            return Make_fixed_precision(mdTaskData, res);
        }

    case 18: /* Register function to run at exit. */
        {
            PLocker locker(&atExitLock);
            if (! exiting)
            {
                PolyObject *cell = alloc(mdTaskData, 2);
                cell->Set(0, at_exit_list);
                cell->Set(1, DEREFWORD(args));
                at_exit_list = cell;
            }
            return Make_fixed_precision(mdTaskData, 0);
        }

    case 19: /* Return the next function in the atExit list and set the
                "exiting" flag to true. */
        {
            PLocker locker(&atExitLock);
            Handle res;
            exiting = true; /* Ignore further calls to atExit. */
            if (at_exit_list == TAGGED(0))
                raise_syscall(mdTaskData, "List is empty", 0);
            PolyObject *cell = at_exit_list.AsObjPtr();
            res = SAVE(cell->Get(1));
            at_exit_list = cell->Get(0);
            return res;
        }

    case 20: /* Terminate without running the atExit list or flushing buffers. */
        {
            /* I don't like terminating without some sort of clean up
               but we'll do it this way for the moment. */
            int i = get_C_int(mdTaskData, DEREFWORDHANDLE(args));
            _exit(i);
        }

        /************ Error codes **************/



        /************ Directory/file paths **************/

    case 5: /* Return the string representing the current arc. */
        return SAVE(C_string_to_Poly(mdTaskData, "."));

    case 6: /* Return the string representing the parent arc. */
        /* I don't know that this exists in MacOS. */
        return SAVE(C_string_to_Poly(mdTaskData, ".."));

    case 7: /* Return the string representing the directory separator. */
        return SAVE(C_string_to_Poly(mdTaskData, DEFAULTSEPARATOR));

    case 8: /* Test the character to see if it matches a separator. */
        {
            int e = get_C_int(mdTaskData, DEREFWORDHANDLE(args));
            if (ISPATHSEPARATOR(e))
                return Make_fixed_precision(mdTaskData, 1);
            else return Make_fixed_precision(mdTaskData, 0);
        }

    case 9: /* Are names case-sensitive? */
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        /* Windows - no. */
        return Make_fixed_precision(mdTaskData, 0);
#else
        /* Unix - yes. */
        return Make_fixed_precision(mdTaskData, 1);
#endif

        // These are no longer used.  The code is handled entirely in ML.
    case 10: /* Are empty arcs redundant? */
        /* Unix and Windows - yes. */
        return Make_fixed_precision(mdTaskData, 1);

    case 11: /* Match the volume name part of a path. */
        {
            const TCHAR *volName = NULL;
            int  isAbs = 0;
            int  toRemove = 0;
            PolyWord path = DEREFHANDLE(args);
            /* This examines the start of a string and determines
               how much of it represents the volume name and returns
               the number of characters to remove, the volume name
               and whether it is absolute.
               One would assume that if there is a volume name then it
               is absolute but there is a peculiar form in Windows/DOS
               (e.g. A:b\c) which means the file b\c relative to the
               currently selected directory on the volume A.
            */
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            TempString buff(path);
            if (buff == 0) raise_syscall(mdTaskData, "Insufficient memory", NOMEMORY);
            size_t length = _tcslen(buff);
            if (length >= 2 && buff[1] == ':')
            { /* Volume name? */
                if (length >= 3 && ISPATHSEPARATOR(buff[2]))
                {
                    /* Absolute path. */
                    toRemove = 3; isAbs = 1;
                }
                else { toRemove = 2; isAbs = 0; }
                volName = buff; buff[2] = '\0';
            }
            else if (length > 3 &&
                     ISPATHSEPARATOR(buff[0]) &&
                     ISPATHSEPARATOR(buff[1]) &&
                     ! ISPATHSEPARATOR(buff[2]))
            { /* UNC name? */
                int i;
                /* Skip the server name. */
                for (i = 3; buff[i] != 0 && !ISPATHSEPARATOR(buff[i]); i++);
                if (ISPATHSEPARATOR(buff[i]))
                {
                    i++;
                    /* Skip the share name. */
                    for (; buff[i] != 0 && !ISPATHSEPARATOR(buff[i]); i++);
                    toRemove = i;
                    if (buff[i] != 0) toRemove++;
                    isAbs = 1;
                    volName = buff;
                    buff[i] = '\0';
                }
            }
            else if (ISPATHSEPARATOR(buff[0]))
                /* \a\b strictly speaking is relative to the
                   current drive.  It's much easier to treat it
                   as absolute. */
                { toRemove = 1; isAbs = 1; volName = _T(""); }
#else
            /* Unix - much simpler. */
            char toTest = 0;
            if (IS_INT(path)) toTest = UNTAGGED(path);
            else {
                PolyStringObject * ps = (PolyStringObject *)path.AsObjPtr();
                if (ps->length > 1) toTest = ps->chars[0];
            }
            if (ISPATHSEPARATOR(toTest))
                { toRemove = 1; isAbs = 1; volName = ""; }
#endif
            /* Construct the result. */
            {
                Handle sVol = SAVE(C_string_to_Poly(mdTaskData, volName));
                Handle sRes = ALLOC(3);
                DEREFWORDHANDLE(sRes)->Set(0, TAGGED(toRemove));
                DEREFHANDLE(sRes)->Set(1, DEREFWORDHANDLE(sVol));
                DEREFWORDHANDLE(sRes)->Set(2, TAGGED(isAbs));
                return sRes;
            }
        }

    case 12: /* Construct a name from a volume and whether it is
                absolute. */
        {
            unsigned isAbs = get_C_unsigned(mdTaskData, DEREFHANDLE(args)->Get(1));
            PolyWord volName = DEREFHANDLE(args)->Get(0);
            /* In Unix the volume name will always be empty. */
            if (isAbs == 0)
                return SAVE(volName);
            /* N.B. The arguments to strconcatc are in reverse. */
            else return strconcatc(mdTaskData,
                                   SAVE(C_string_to_Poly(mdTaskData, DEFAULTSEPARATOR)),
                                   SAVE(volName));
        }

    case 13: /* Is the string a valid file name? */
        {
            PolyWord volName = DEREFWORD(args);
            // First check for NULL.  This is not allowed in either Unix or Windows.
            if (IS_INT(volName))
            {
                if (volName == TAGGED(0))
                    return Make_fixed_precision(mdTaskData, 0);
            }
            else
            {
                PolyStringObject * volume = (PolyStringObject *)(volName.AsObjPtr());
                for (POLYUNSIGNED i = 0; i < volume->length; i++)
                {
                    if (volume->chars[i] == '\0')
                        return Make_fixed_precision(mdTaskData, 0);
                }
            }
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            // We need to look for certain invalid characters but only after
            // we've converted it to Unicode if necessary.
            TempString name(volName);
            for (const TCHAR *p = name; *p != 0; p++)
            {
                switch (*p)
                {
                case '<': case '>': case ':': case '"': 
                case '\\': case '|': case '?': case '*': case '\0':
#if (0)
                // This currently breaks the build.
                case '/':
#endif
                    return Make_fixed_precision(mdTaskData, 0);
                }
                if (*p >= 0 && *p <= 31) return Make_fixed_precision(mdTaskData, 0);
            }
            // Should we check for special names such as aux, con, prn ??
            return Make_fixed_precision(mdTaskData, 1);
#else
            // That's all we need for Unix.
            // TODO: Check for /.  It's invalid in a file name arc.
            return Make_fixed_precision(mdTaskData, 1);
#endif
        }

        // These were supposed to have been moved to poly-specific but don't seem to have been.
    case 100: /* Return the maximum word segment size. */
            return mdTaskData->saveVec.push(TAGGED(MAX_OBJECT_SIZE));
    case 101: /* Return the maximum string size (in bytes).
                 It is the maximum number of bytes in a segment
                 less one word for the length field. */
            return mdTaskData->saveVec.push(TAGGED((MAX_OBJECT_SIZE)*sizeof(PolyWord) - sizeof(PolyWord)));
    // This is no longer in use but still seems to be in the pre-built compilers.
    case 102: /* Test whether the supplied address is in the io area. */
            return Make_arbitrary_precision(mdTaskData, 0);
 
    case 104: return Make_arbitrary_precision(mdTaskData, POLY_version_number);
 
    case 105: /* Get the name of the function. */
        {
            PolyObject *pt = DEREFWORDHANDLE(args);
            if (pt->IsCodeObject()) /* Should now be a code object. */ 
            {
                /* Compiled code.  This is the first constant in the constant area. */
                PolyWord *codePt = pt->ConstPtrForCode();
                PolyWord name = codePt[0];
                /* May be zero indicating an anonymous segment - return null string. */
                if (name == PolyWord::FromUnsigned(0))
                    return SAVE(C_string_to_Poly(mdTaskData, ""));
                else return SAVE(name);
            }
            else raise_syscall(mdTaskData, "Not a code pointer", 0);
        }

    default:
        {
            char msg[100];
            sprintf(msg, "Unknown environment function: %d", c);
            raise_exception_string(mdTaskData, EXC_Fail, msg);
            return 0;
        }
    }
}

// General interface to process-env.  Ideally the various cases will be made into
// separate functions.
POLYUNSIGNED PolyProcessEnvGeneral(PolyObject *threadId, PolyWord code, PolyWord arg)
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
void PolyFinish(PolyObject *threadId, PolyWord arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    int i = get_C_int(taskData, arg);
    // Cause the other threads to exit.
    processes->Exit(i);
    // Exit this thread
    processes->ThreadExit(taskData); // Doesn't return.
}

// Terminate without running the atExit list or flushing buffers
void PolyTerminate(PolyObject *threadId, PolyWord arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    int i = get_C_int(taskData, arg);
    _exit(i); // Doesn't return.
}

// Get the name of a numeric error message.
POLYUNSIGNED PolyProcessEnvErrorName(PolyObject *threadId, PolyWord syserr)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        int e = (int)syserr.AsObjPtr()->Get(0).AsSigned();
        // First look to see if we have the name in the error table. They should generally all be there.
        const char *errorMsg = stringFromErrorCode(e);
        if (errorMsg != NULL)
            result = taskData->saveVec.push(C_string_to_Poly(taskData, errorMsg));
        else
        { // If it isn't in the table.
            char buff[40];
            sprintf(buff, "ERROR%0d", e);
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
POLYUNSIGNED PolyProcessEnvErrorMessage(PolyObject *threadId, PolyWord syserr)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = errorMsg(taskData, (int)syserr.AsObjPtr()->Get(0).AsSigned());
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Try to convert an error string to an error number.
POLYUNSIGNED PolyProcessEnvErrorFromString(PolyObject *threadId, PolyWord string)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        char buff[40];
        // Get the string.
        Poly_string_to_C(string, buff, sizeof(buff));
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

struct _entrypts processEnvEPT[] =
{
    { "PolyFinish",                     (polyRTSFunction)&PolyFinish},
    { "PolyTerminate",                  (polyRTSFunction)&PolyTerminate},
    { "PolyProcessEnvGeneral",          (polyRTSFunction)&PolyProcessEnvGeneral},
    { "PolyProcessEnvErrorName",        (polyRTSFunction)&PolyProcessEnvErrorName},
    { "PolyProcessEnvErrorMessage",     (polyRTSFunction)&PolyProcessEnvErrorMessage},
    { "PolyProcessEnvErrorFromString",  (polyRTSFunction)&PolyProcessEnvErrorFromString},

    { NULL, NULL} // End of list.
};

class ProcessEnvModule: public RtsModule
{
public:
    void GarbageCollect(ScanAddress *process);
};

// Declare this.  It will be automatically added to the table.
static ProcessEnvModule processModule;

void ProcessEnvModule::GarbageCollect(ScanAddress *process)
/* Ensures that all the objects are retained and their addresses updated. */
{
    if (at_exit_list.IsDataPtr())
    {
        PolyObject *obj = at_exit_list.AsObjPtr();
        process->ScanRuntimeAddress(&obj, ScanAddress::STRENGTH_STRONG);
        at_exit_list = obj;
    }
}
