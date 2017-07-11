/*
    Title:      Operating Specific functions: Windows version.

    Copyright (c) 2000, 2015 David C. J. Matthews

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


#include "globals.h"
#include "arb.h"
#include "gc.h"
#include "run_time.h"
#include "io_internal.h"
#include "os_specific.h"
#include "sys.h"
#include "processes.h"
#include "Console.h"
#include "mpoly.h"
#include "diagnostics.h"
#include "scanaddrs.h"
#include "polystring.h"
#include "save_vec.h"
#include "rts_module.h"
#include "machine_dep.h"
#include "rtsentry.h"

#define STREAMID(x) (DEREFSTREAMHANDLE(x)->streamNo)

#define SAVE(x) taskData->saveVec.push(x)
#define SIZEOF(x) (sizeof(x)/sizeof(word))

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyOSSpecificGeneral(PolyObject *threadId, PolyWord code, PolyWord arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyGetOSType();
}

typedef enum
{
    HE_UNUSED,
    HE_REGISTRY,
    HE_PROCESS,
    HE_DDECONVERSATION
} HANDENTRYTYPE;

/* Table of open handles.
   This is modelled after the IO table in basicio.c and performs a
   similar function.  Each resource has an entry in here and there
   is a token which is an ML object.  The token is simply a word
   containing the index in the table.  It is the token itself which
   represents the entry within the ML world.  The token is checked
   against the entry whenever it is used since it is possible for
   the tokens to be persistent although the corresponding entry in
   the table will not make sense in a subsequent session.  This table
   also allows us to garbage-collect entries since if the token becomes
   unreachable we know that there is no longer a reference to the
   entry within ML.
*/
typedef struct {
    PolyObject *token; /* pointer into ML heap */
    HANDENTRYTYPE  entryType; /* Type of handle */

    // Only some of these are used in particular cases
    // but using a union here complicates things.
    HKEY    hKey; /* Registry key. */
    /* Process and IO channels. */
    HANDLE hProcess, hInput, hOutput, hEvent;
    PolyWord readToken, writeToken;
    HCONV hcDDEConv; /* DDE Conversation. */
} HANDLETAB, *PHANDLETAB;


static PHANDLETAB handleTable;
static POLYUNSIGNED maxHandleTab;

/* General "close" function which may be called from the
   garbage-collector. */
static void close_handle(PHANDLETAB pTab)
{
    switch (pTab->entryType)
    {
    case HE_REGISTRY:
        RegCloseKey(pTab->hKey);
        break;

    case HE_PROCESS:
        if (pTab->hProcess)
            CloseHandle(pTab->hProcess);
        if (pTab->hInput != INVALID_HANDLE_VALUE)
            CloseHandle(pTab->hInput);
        if (pTab->hOutput != INVALID_HANDLE_VALUE)
            CloseHandle(pTab->hOutput);
        if (pTab->hEvent)
            CloseHandle(pTab->hEvent);
        break;

    case HE_DDECONVERSATION:
        CloseDDEConversation(pTab->hcDDEConv);
        break;
    case HE_UNUSED:
        break; // Avoid warnings
    }
    pTab->token = 0;
    pTab->entryType = HE_UNUSED;
}

static PHANDLETAB get_handle(PolyWord token, HANDENTRYTYPE heType)
{
    StreamToken *handle_token = (StreamToken*)token.AsObjPtr();
    POLYUNSIGNED  handle_no    = handle_token->streamNo;

    if (handle_no >= maxHandleTab ||
        handleTable[handle_no].token != handle_token ||
        handleTable[handle_no].entryType != heType) 
        return 0;

    return &handleTable[handle_no];
}

static Handle make_handle_entry(TaskData *taskData)
{
    unsigned handle_no;
    Handle str_token;
    bool have_collected = false;

    do {
        for(handle_no = 0;
            handle_no < maxHandleTab && handleTable[handle_no].token != 0;
            handle_no++);
            
        /* Check we have enough space. */
        if (handle_no >= maxHandleTab)
        { /* No space. */
           /* See if we have unreferenced streams. */
            if (! have_collected)
            {
                FullGC(taskData);
                have_collected = true;
            }
            else /* No space - expand vector. */
            {
                POLYUNSIGNED oldMax = maxHandleTab;
                maxHandleTab += maxHandleTab/2;
                void *p = realloc(handleTable, maxHandleTab*sizeof(HANDLETAB));
                // If there's insufficient memory leave the old table.
                if (p == 0) raise_syscall(taskData, "Insufficient memory", ERROR_NOT_ENOUGH_MEMORY);
                handleTable = (PHANDLETAB)p;
                /* Clear the new space. */
                memset(handleTable+oldMax, 0, (maxHandleTab-oldMax)*sizeof(HANDLETAB));
            }
        }
    } while (handle_no >= maxHandleTab);
     
    str_token = alloc_and_save(taskData, 1, F_BYTE_OBJ);
    STREAMID(str_token) = handle_no;

    /* Clear the entry then set the token. */
    memset(&handleTable[handle_no], 0, sizeof(HANDLETAB));
    handleTable[handle_no].token = DEREFWORDHANDLE(str_token);
    return str_token;
}

static Handle execute(TaskData *taskData, Handle pname);
static Handle simpleExecute(TaskData *taskData, Handle args);
static Handle openProcessHandle(TaskData *taskData, Handle args, BOOL fIsRead, BOOL fIsText);
static Handle openRegistryKey(TaskData *taskData, Handle args, HKEY hkParent);
static Handle createRegistryKey(TaskData *taskData, Handle args, HKEY hkParent);
static Handle queryRegistryKey(TaskData *taskData, Handle args, HKEY hkParent);
static Handle setRegistryKey(TaskData *taskData, Handle args, HKEY hkParent);
static Handle deleteRegistryKey(TaskData *taskData, Handle args, HKEY hkParent);
static Handle deleteRegistryValue(TaskData *taskData, Handle args, HKEY hkParent);
static Handle enumerateRegistry(TaskData *taskData, Handle args, HKEY hkey, BOOL isKey);

// Vector of constants returned by call1006
static POLYUNSIGNED winConstVec[] =
{
    KEY_ALL_ACCESS, // 0
    KEY_CREATE_LINK,
    KEY_CREATE_SUB_KEY,
    KEY_ENUMERATE_SUB_KEYS,
    KEY_EXECUTE,
    KEY_NOTIFY,
    KEY_QUERY_VALUE,
    KEY_READ,
    KEY_SET_VALUE,
    KEY_WRITE, // 9

    STATUS_ACCESS_VIOLATION, // 10
    STATUS_ARRAY_BOUNDS_EXCEEDED,
    STATUS_BREAKPOINT,
    STATUS_CONTROL_C_EXIT,
    STATUS_DATATYPE_MISALIGNMENT,
    STATUS_FLOAT_DENORMAL_OPERAND,
    STATUS_FLOAT_DIVIDE_BY_ZERO,
    STATUS_FLOAT_INEXACT_RESULT,
    STATUS_FLOAT_INVALID_OPERATION,
    STATUS_FLOAT_OVERFLOW,
    STATUS_FLOAT_STACK_CHECK,
    STATUS_FLOAT_UNDERFLOW,
    STATUS_GUARD_PAGE_VIOLATION,
    STATUS_INTEGER_DIVIDE_BY_ZERO,
    STATUS_INTEGER_OVERFLOW,
    STATUS_ILLEGAL_INSTRUCTION,
    STATUS_INVALID_DISPOSITION,
#ifdef STATUS_INVALID_HANDLE
    STATUS_INVALID_HANDLE,
#else
    0, // Not defined in Win CE
#endif
    STATUS_IN_PAGE_ERROR,
    STATUS_NONCONTINUABLE_EXCEPTION,
    STATUS_PENDING,
    STATUS_PRIVILEGED_INSTRUCTION,
    STATUS_SINGLE_STEP,
    STATUS_STACK_OVERFLOW,
    STATUS_TIMEOUT,
    STATUS_USER_APC, // 35

    VER_PLATFORM_WIN32s, // 36
    VER_PLATFORM_WIN32_WINDOWS,
    VER_PLATFORM_WIN32_NT, // 38
    // VER_PLATFORM_WIN32_CE is only defined in the Windows CE headers 
#ifdef VER_PLATFORM_WIN32_CE
    VER_PLATFORM_WIN32_CE, // 39
#else
    3, // 39
#endif
};

HKEY hkPredefinedKeyTab[] =
{
    HKEY_CLASSES_ROOT,
    HKEY_CURRENT_USER,
    HKEY_LOCAL_MACHINE,
    HKEY_USERS,
#ifdef HKEY_PERFORMANCE_DATA
    HKEY_PERFORMANCE_DATA,
#else
    0, // Not defined in Win CE
#endif
#ifdef HKEY_CURRENT_CONFIG
    HKEY_CURRENT_CONFIG,
#else
    0,
#endif
#ifdef HKEY_DYN_DATA
    HKEY_DYN_DATA
#else
    0
#endif
};


Handle OS_spec_dispatch_c(TaskData *taskData, Handle args, Handle code)
{
    unsigned c = get_C_unsigned(taskData, DEREFWORD(code));
    switch (c)
    {
    case 0: /* Return our OS type.  Not in any structure. */
        return Make_fixed_precision(taskData, 1); /* 1 for Windows. */

        /* Windows-specific functions. */
    case 1000: /* execute */
        return execute(taskData, args);

    case 1001: /* Get input stream as text. */
        return openProcessHandle(taskData, args, TRUE, TRUE);

    case 1002: /* Get output stream as text. */
        return openProcessHandle(taskData, args, FALSE, TRUE);

    case 1003: /* Get input stream as binary. */
        return openProcessHandle(taskData, args, TRUE, FALSE);

    case 1004: /* Get output stream as binary. */
        return openProcessHandle(taskData, args, FALSE, FALSE);

    case 1005: /* Get result of process. */
        {
            PHANDLETAB hnd = get_handle(DEREFWORD(args), HE_PROCESS);
            if (hnd == 0)
                raise_syscall(taskData, "Process is closed", ERROR_INVALID_HANDLE);
            // Close the streams. Either of them may have been
            // passed to the stream package.
            if (hnd->hInput != INVALID_HANDLE_VALUE)
                CloseHandle(hnd->hInput);
            hnd->hInput = INVALID_HANDLE_VALUE;
            if (hnd->hEvent)
                CloseHandle(hnd->hEvent);
            hnd->hEvent = NULL;
            if (hnd->readToken.IsDataPtr())
            {
                PIOSTRUCT strm =
                    get_stream(hnd->readToken);
                if (strm != NULL) close_stream(strm);
            }
            hnd->readToken = ClosedToken;
            if (hnd->hOutput != INVALID_HANDLE_VALUE)
                CloseHandle(hnd->hOutput);
            hnd->hOutput = INVALID_HANDLE_VALUE;
            if (hnd->writeToken.IsDataPtr())
            {
                PIOSTRUCT strm =
                    get_stream(hnd->writeToken);
                if (strm != NULL) close_stream(strm);
            }
            hnd->writeToken = ClosedToken;

            // See if it's finished.
            while (true) {
                DWORD dwResult;
                if (GetExitCodeProcess(hnd->hProcess, &dwResult) == 0)
                    raise_syscall(taskData, "GetExitCodeProcess failed", GetLastError());
                if (dwResult != STILL_ACTIVE) {
                    /* Finished - return the result. */
                    /* Note: we haven't closed the handle because we might want to ask
                       for the result again.  We only close it when we've garbage-collected
                       the token.  Doing this runs the risk of running out of handles.
                       Maybe change it and remember the result in ML. */
                    return Make_fixed_precision(taskData, dwResult);
                }
                // Block and try again.
                WaitHandle waiter(hnd->hProcess);
                processes->ThreadPauseForIO(taskData, &waiter);
            }
        }

    case 1006: /* Return a constant. */
        {
            unsigned i = get_C_unsigned(taskData, DEREFWORD(args));
            if (i >= sizeof(winConstVec)/sizeof(winConstVec[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return Make_arbitrary_precision(taskData, winConstVec[i]);
        }

        /* Registry functions. */
    case 1007: // Open a key within one of the roots.
        {
            unsigned keyIndex = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(0));
            // This should only ever happen as a result of a fault in
            // the Windows structure.
            if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return openRegistryKey(taskData, args, hkPredefinedKeyTab[keyIndex]);
        }

    case 1008: // Open a subkey of an opened key.
        {
            PHANDLETAB hnd =
                get_handle(DEREFHANDLE(args)->Get(0), HE_REGISTRY);
            if (hnd == 0)
                raise_syscall(taskData, "Handle is closed", ERROR_INVALID_HANDLE);
            return openRegistryKey(taskData, args, hnd->hKey);
        }

    case 1009: // Create a subkey within one of the roots.
        {
            unsigned keyIndex = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(0));
            // This should only ever happen as a result of a fault in
            // the Windows structure.
            if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return createRegistryKey(taskData, args, hkPredefinedKeyTab[keyIndex]);
        }

    case 1010: // Create a subkey within an opened key.
        {
            PHANDLETAB hnd =
                get_handle(DEREFHANDLE(args)->Get(0), HE_REGISTRY);
            if (hnd == 0)
                raise_syscall(taskData, "Handle is closed", ERROR_INVALID_HANDLE);
            return createRegistryKey(taskData, args, hnd->hKey);
        }

    case 1011: // Close a registry handle.
        {
            PHANDLETAB hnd = get_handle(DEREFWORD(args), HE_REGISTRY);
            if (hnd != 0) close_handle(hnd);
            return Make_fixed_precision(taskData, 0);
        }

    case 1012: // Get a value
        {
            unsigned keyIndex = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(0));
            // This should only ever happen as a result of a fault in
            // the Windows structure.
            if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return queryRegistryKey(taskData, args, hkPredefinedKeyTab[keyIndex]);
        }

    case 1013: // Get a value
        {
            PHANDLETAB hnd =
                get_handle(DEREFHANDLE(args)->Get(0), HE_REGISTRY);
            if (hnd == 0)
                raise_syscall(taskData, "Handle is closed", ERROR_INVALID_HANDLE);
            return queryRegistryKey(taskData, args, hnd->hKey);
        }

    case 1014: // Delete a subkey
        {
            unsigned keyIndex = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(0));
            // This should only ever happen as a result of a fault in
            // the Windows structure.
            if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return deleteRegistryKey(taskData, args, hkPredefinedKeyTab[keyIndex]);
        }

    case 1015: // Delete a subkey
        {
            PHANDLETAB hnd =
                get_handle(DEREFHANDLE(args)->Get(0), HE_REGISTRY);
            if (hnd == 0)
                raise_syscall(taskData, "Handle is closed", ERROR_INVALID_HANDLE);
            return deleteRegistryKey(taskData, args, hnd->hKey);
        }

    case 1016: // Set a value
        {
            unsigned keyIndex = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(0));
            // This should only ever happen as a result of a fault in
            // the Windows structure.
            if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return setRegistryKey(taskData, args, hkPredefinedKeyTab[keyIndex]);
        }

    case 1017: // Set a value
        {
            PHANDLETAB hnd =
                get_handle(DEREFHANDLE(args)->Get(0), HE_REGISTRY);
            if (hnd == 0)
                raise_syscall(taskData, "Handle is closed", ERROR_INVALID_HANDLE);
            return setRegistryKey(taskData, args, hnd->hKey);
        }

    case 1018: // Enumerate a key in the predefined keys
        {
            unsigned keyIndex = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(0));
            if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return enumerateRegistry(taskData, args, hkPredefinedKeyTab[keyIndex], TRUE);
        }

    case 1019: // Enumerate a key in an opened key
        {
            PHANDLETAB hnd =
                get_handle(DEREFHANDLE(args)->Get(0), HE_REGISTRY);
            if (hnd == 0)
                raise_syscall(taskData, "Handle is closed", ERROR_INVALID_HANDLE);
            return enumerateRegistry(taskData, args, hnd->hKey, TRUE);
        }

    case 1020: // Enumerate a value in the predefined keys
        {
            unsigned keyIndex = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(0));
            if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return enumerateRegistry(taskData, args, hkPredefinedKeyTab[keyIndex], FALSE);
        }

    case 1021: // Enumerate a value in an opened key
        {
            PHANDLETAB hnd =
                get_handle(DEREFHANDLE(args)->Get(0), HE_REGISTRY);
            if (hnd == 0)
                raise_syscall(taskData, "Handle is closed", ERROR_INVALID_HANDLE);
            return enumerateRegistry(taskData, args, hnd->hKey, FALSE);
        }

    case 1022: // Delete a value
        {
            unsigned keyIndex = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(0));
            // This should only ever happen as a result of a fault in
            // the Windows structure.
            if (keyIndex >= sizeof(hkPredefinedKeyTab)/sizeof(hkPredefinedKeyTab[0]))
                raise_syscall(taskData, "Invalid index", 0);
            return deleteRegistryValue(taskData, args, hkPredefinedKeyTab[keyIndex]);
        }

    case 1023: // Delete a value
        {
            PHANDLETAB hnd =
                get_handle(DEREFHANDLE(args)->Get(0), HE_REGISTRY);
            if (hnd == 0)
                raise_syscall(taskData, "Handle is closed", ERROR_INVALID_HANDLE);
            return deleteRegistryValue(taskData, args, hnd->hKey);
        }


    case 1030: // Convert UTC time values to local time. -- No longer used??
        {
            FILETIME ftUTC, ftLocal;
            /* Get the file time. */
            getFileTimeFromArb(taskData, args, &ftUTC);
            if (! FileTimeToLocalFileTime(&ftUTC, &ftLocal))
                raise_syscall(taskData, "FileTimeToLocalFileTime failed", GetLastError());
            return Make_arb_from_Filetime(taskData, ftLocal);
        }

    case 1031: // Convert local time values to UTC. -- No longer used??
        {
            FILETIME ftUTC, ftLocal;
            /* Get the file time. */
            getFileTimeFromArb(taskData, args, &ftLocal);
            if (! LocalFileTimeToFileTime(&ftLocal, &ftUTC))
                raise_syscall(taskData, "LocalFileTimeToFileTime failed", GetLastError());
            return Make_arb_from_Filetime(taskData, ftUTC);
        }

    case 1032: // Get volume information.
        {
            TCHAR rootName[MAX_PATH], volName[MAX_PATH], sysName[MAX_PATH];
            DWORD dwVolSerial, dwMaxComponentLen, dwFlags;
            Handle volHandle, sysHandle, serialHandle, maxCompHandle;
            Handle resultHandle;
            POLYUNSIGNED length = Poly_string_to_C(DEREFWORD(args), rootName, MAX_PATH);
            if (length > MAX_PATH)
                raise_syscall(taskData, "Root name too long", ERROR_BAD_LENGTH);
            
            if (!GetVolumeInformation(rootName, volName, MAX_PATH,
                    &dwVolSerial, &dwMaxComponentLen, &dwFlags,
                    sysName, MAX_PATH))
                raise_syscall(taskData, "GetVolumeInformation failed", GetLastError());
            volHandle = SAVE(C_string_to_Poly(taskData, volName));
            sysHandle = SAVE(C_string_to_Poly(taskData, sysName));
            serialHandle = Make_arbitrary_precision(taskData, dwVolSerial);
            maxCompHandle = Make_arbitrary_precision(taskData, dwMaxComponentLen);
            resultHandle = alloc_and_save(taskData, 4);
            DEREFHANDLE(resultHandle)->Set(0, DEREFWORDHANDLE(volHandle));
            DEREFHANDLE(resultHandle)->Set(1, DEREFWORDHANDLE(sysHandle));
            DEREFHANDLE(resultHandle)->Set(2, DEREFWORDHANDLE(serialHandle));
            DEREFHANDLE(resultHandle)->Set(3, DEREFWORDHANDLE(maxCompHandle));
            return resultHandle;
        }

    case 1033:
        {
            TCHAR fileName[MAX_PATH], execName[MAX_PATH];
            POLYUNSIGNED length = Poly_string_to_C(DEREFWORD(args), fileName, MAX_PATH);
            HINSTANCE hInst;
            if (length > MAX_PATH)
                raise_syscall(taskData, "File name too long", ERROR_BAD_LENGTH);
            hInst = FindExecutable(fileName, NULL, execName);
            if ((POLYUNSIGNED)hInst <= 32)
            {
                int error = 0;
                switch ((POLYUNSIGNED)hInst) {
                case SE_ERR_FNF: error = ERROR_FILE_NOT_FOUND; break;
                case SE_ERR_PNF: error = ERROR_PATH_NOT_FOUND; break;
                case SE_ERR_ACCESSDENIED: error = ERROR_ACCESS_DENIED; break;
                case SE_ERR_OOM: error = ERROR_NOT_ENOUGH_MEMORY; break;
                case SE_ERR_NOASSOC: error = ERROR_NO_ASSOCIATION; break;
                }
                raise_syscall(taskData, "FindExecutable failed", error);
            }
            return SAVE(C_string_to_Poly(taskData, execName));
        }

    case 1034: // Open a document
        {
            SHELLEXECUTEINFO shellEx;
            memset(&shellEx, 0, sizeof(shellEx));
            shellEx.cbSize = sizeof(shellEx);
            shellEx.lpVerb = _T("open");
            shellEx.lpFile = Poly_string_to_T_alloc(DEREFWORD(args));
            shellEx.hwnd = hMainWindow;
            shellEx.nShow = SW_SHOWNORMAL;
            BOOL fRes = ShellExecuteEx(&shellEx);
            free((void*)shellEx.lpFile);
            if (! fRes)
                raise_syscall(taskData, "ShellExecuteEx failed", GetLastError());
            return Make_fixed_precision(taskData, 0);
        }

    case 1035: // Launch an application.
        {
            SHELLEXECUTEINFO shellEx;
            memset(&shellEx, 0, sizeof(shellEx));
            shellEx.cbSize = sizeof(shellEx);
            shellEx.lpVerb = _T("open");
            shellEx.lpFile = Poly_string_to_T_alloc(args->WordP()->Get(0));
            shellEx.lpParameters = Poly_string_to_T_alloc(args->WordP()->Get(1));
            shellEx.nShow = SW_SHOWNORMAL;
            BOOL fRes = ShellExecuteEx(&shellEx);
            free((void*)shellEx.lpFile);
            free((void*)shellEx.lpParameters);
            if (! fRes)
                raise_syscall(taskData, "ShellExecuteEx failed", GetLastError());
            return Make_fixed_precision(taskData, 0);
        }

    case 1036: // Does the process have its own console?
        return Make_fixed_precision(taskData, hMainWindow != NULL ? 1: 0);

    case 1037: // Simple execute.
        return simpleExecute(taskData, args);


        // DDE
    case 1038: // Start DDE dialogue.
        {
            Handle handToken;
            PHANDLETAB pTab;
            HCONV hcDDEConv;
            TCHAR *serviceName = Poly_string_to_T_alloc(args->WordP()->Get(0));
            TCHAR *topicName = Poly_string_to_T_alloc(args->WordP()->Get(1));
            /* Send a request to the main thread to do the work. */
            hcDDEConv = StartDDEConversation(serviceName, topicName);
            free(serviceName); free(topicName);
            if (hcDDEConv == 0) raise_syscall(taskData, "DdeConnect failed", 0);
            // Create an entry to return the conversation.
            handToken = make_handle_entry(taskData);
            pTab = &handleTable[STREAMID(handToken)];
            pTab->entryType = HE_DDECONVERSATION;
            pTab->hcDDEConv = hcDDEConv;
            return handToken;
        }

    case 1039: // Send DDE execute request.
        {
            PHANDLETAB hnd =
                get_handle(DEREFHANDLE(args)->Get(0), HE_DDECONVERSATION);
            LRESULT res;
            char *command;
            if (hnd == NULL)
            {
                raise_syscall(taskData, "DDE Conversation is closed", 0);
            }
            command = Poly_string_to_C_alloc(args->WordP()->Get(1));
            /* Send a request to the main thread to do the work. */
            res = ExecuteDDE(command, hnd->hcDDEConv);
            free(command);
            if (res == -1) raise_syscall(taskData, "DdeClientTransaction failed", 0);
            else return Make_arbitrary_precision(taskData, res);
        }

    case 1040: // Close a DDE conversation.
        {
            PHANDLETAB hnd = get_handle(args->Word(), HE_DDECONVERSATION);
            if (hnd != 0) close_handle(hnd);
            return Make_fixed_precision(taskData, 0);
        }


        // Configuration functions.
    case 1050: // Get version data
        {
            OSVERSIONINFO osver;
            ZeroMemory(&osver, sizeof(OSVERSIONINFO));
            osver.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
            // GetVersionEx is deprecated in Windows 8.1
            if (! GetVersionEx(&osver))
                raise_syscall(taskData, "GetVersionEx failed", GetLastError());
            Handle major = Make_fixed_precision(taskData, osver.dwMajorVersion);
            Handle minor = Make_fixed_precision(taskData, osver.dwMinorVersion);
            Handle build = Make_fixed_precision(taskData, osver.dwBuildNumber);
            Handle platform = Make_fixed_precision(taskData, osver.dwPlatformId);
            Handle version = SAVE(C_string_to_Poly(taskData, osver.szCSDVersion));
            Handle resVal = alloc_and_save(taskData, 5);
            DEREFHANDLE(resVal)->Set(0, DEREFWORDHANDLE(major));
            DEREFHANDLE(resVal)->Set(1, DEREFWORDHANDLE(minor));
            DEREFHANDLE(resVal)->Set(2, DEREFWORDHANDLE(build));
            DEREFHANDLE(resVal)->Set(3, DEREFWORDHANDLE(platform));
            DEREFHANDLE(resVal)->Set(4, DEREFWORDHANDLE(version));
            return resVal;
        }

    case 1051: // Get windows directory
        {
            TCHAR path[MAX_PATH+1];
            if (GetWindowsDirectory(path, sizeof(path)/sizeof(TCHAR)) == 0)
                raise_syscall(taskData, "GetWindowsDirectory failed", GetLastError());
            return SAVE(C_string_to_Poly(taskData, path));
        }

    case 1052: // Get system directory
        {
            TCHAR path[MAX_PATH+1];
            if (GetSystemDirectory(path, sizeof(path)/sizeof(TCHAR)) == 0)
                raise_syscall(taskData, "GetSystemDirectory failed", GetLastError());
            return SAVE(C_string_to_Poly(taskData, path));
        }

    case 1053: // Get computer name
        {
            TCHAR name[MAX_COMPUTERNAME_LENGTH +1];
            DWORD dwSize = MAX_COMPUTERNAME_LENGTH +1;
            if (GetComputerName(name, &dwSize) == 0)
                raise_syscall(taskData, "GetComputerName failed", GetLastError());
            return SAVE(C_string_to_Poly(taskData, name));
        }

    case 1054: // Get user name
        {
            TCHAR name[UNLEN +1];
            DWORD dwSize = UNLEN +1;
            if (GetUserName(name, &dwSize) == 0)
                raise_syscall(taskData, "GetUserName failed", GetLastError());
            return SAVE(C_string_to_Poly(taskData, name));
        }

    case 1100: // Get the error result from the last call.
               // This is saved when we make a call to a foreign function.
        {
            return(SAVE(TAGGED(taskData->lastError)));
        }

    case 1101: // Wait for a message.
        {
            HWND hwnd = *(HWND*)(DEREFWORDHANDLE(args)->Get(0).AsCodePtr());
            UINT wMsgFilterMin = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(1));
            UINT wMsgFilterMax = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(2));
            while (1)
            {
                MSG msg;
                processes->ThreadReleaseMLMemory(taskData);
                // N.B.  PeekMessage may directly call the window proc resulting in a
                // callback to ML.  For this to work a callback must not overwrite "args".
                BOOL result = PeekMessage(&msg, hwnd, wMsgFilterMin, wMsgFilterMax, PM_NOREMOVE);
                processes->ThreadUseMLMemory(taskData);
                if (result) return Make_fixed_precision(taskData, 0);
                // Pause until a message arrives.
                processes->ThreadPause(taskData);
            }
        }

    // case 1102: // Return the address of the window callback function.

    case 1103: // Return the application instance.
        {
            Handle result = alloc_and_save(taskData, 1, F_BYTE_OBJ);
            *(HINSTANCE*)(result->Word().AsCodePtr()) = hApplicationInstance;
            return result;
        }

    case 1104: // Return the main window handle
        {
            Handle result = alloc_and_save(taskData, 1, F_BYTE_OBJ);
            *(HWND*)(result->Word().AsCodePtr()) = hMainWindow;
            return result;
        }

//    case 1105: // Set the callback function

    default:
        {
            char msg[100];
            sprintf(msg, "Unknown windows-specific function: %d", c);
            raise_exception_string(taskData, EXC_Fail, msg);
            return 0;
        }
    }
}

// General interface to Windows OS-specific.  Ideally the various cases will be made into
// separate functions.
POLYUNSIGNED PolyOSSpecificGeneral(PolyObject *threadId, PolyWord code, PolyWord arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedCode = taskData->saveVec.push(code);
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        result = OS_spec_dispatch_c(taskData, pushedArg, pushedCode);
    }
    catch (KillException &) {
        processes->ThreadExit(taskData); // Call 1005 may test for kill
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset); // Ensure the save vec is reset
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyGetOSType()
{
    return TAGGED(1).AsUnsigned(); // Return 1 for Windows
}

/*
The Windows version of this is more complicated than the Unix version because
we can't manipulate the pipe handles in the child process.  Everything has to be
set up in the parent.  As with Unix we create two pipes and pass one end of each
pipe to the child.  The end we pass to the child is "inheritable" (i.e. duplicated
in the child as with Unix file descriptors) while the ends we keep in the parent
are non-inheritable (i.e. not duplicated in the child). 
DCJM: December 1999.
This is now further complicated to improve the performance.  In Unix we can pass
the file ID to "select" which will return immediately when input is available (we
ignore blocking on output at the moment).  That allows the ML process to respond
immediately.  There's no easy way to do that in Windows since the pipe handle is
signalled whether there is input available or not.  One possibility would be to
use overlapped IO but that requires using the ReadFile call directly and some
contortions to create a pipe with overlapped IO.  The other, taken here, is to
interpose a thread which can signal an event when input is available. 
*/
static Handle execute(TaskData *taskData, Handle args)
{
    LPCSTR lpszError = "";
    HANDLE hWriteToChild = INVALID_HANDLE_VALUE,
           hReadFromParent = INVALID_HANDLE_VALUE,
           hWriteToParent = INVALID_HANDLE_VALUE,
           hReadFromChild = INVALID_HANDLE_VALUE;
    HANDLE hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
    HANDLE hTemp;
    STARTUPINFO startupInfo;
    PROCESS_INFORMATION processInfo;
    Handle handToken = make_handle_entry(taskData);
    PHANDLETAB pTab = &handleTable[STREAMID(handToken)];

    LPTSTR commandName = Poly_string_to_T_alloc(args->WordP()->Get(0));
    LPTSTR arguments = Poly_string_to_T_alloc(args->WordP()->Get(1));

    // Create pipes for connection. Setting the security argument to NULL creates
    // the pipe handles as non-inheritable.  We have to make sure that the
    // child process does not inherit handles for the parent end of the
    // connection otherwise the pipe will remain open after the parent has
    // closed its end, causing the child process to sit around even after
    // the parent process has gone away.
    if (!CreatePipe(&hReadFromParent, &hWriteToChild, NULL, 0)) {
        lpszError = "Could not create pipe";
        goto error;
    }
    if (!CreatePipe(&hReadFromChild, &hWriteToParent, NULL, 0)) {
        lpszError = "Could not create pipe";
        goto error;
    }
    // Create the copying thread.
    hTemp = CreateCopyPipe(hReadFromChild, hEvent);
    if (hTemp == NULL) {
        lpszError = "Could not create pipe";
        goto error;
    }
    hReadFromChild = hTemp;
    // Convert the handles we want to pass to the child into inheritable
    // handles by duplicating and replacing them with the duplicates.
    if (! DuplicateHandle(GetCurrentProcess(), hWriteToParent, GetCurrentProcess(),
                          &hTemp, 0, TRUE, // inheritable
                          DUPLICATE_SAME_ACCESS )) {
        lpszError = "Could not create pipe";
        goto error;
    }
    CloseHandle(hWriteToParent);
    hWriteToParent = hTemp;
    if (! DuplicateHandle(GetCurrentProcess(), hReadFromParent, GetCurrentProcess(),
                          &hTemp, 0, TRUE, // inheritable
                          DUPLICATE_SAME_ACCESS )) {
        lpszError = "Could not create pipe";
        goto error;
    }
    CloseHandle(hReadFromParent);
    hReadFromParent = hTemp;

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

    free(commandName);
    free(arguments);
    /* Close thread handle since we don't need it. */
    CloseHandle(processInfo.hThread);
    /* Close the sides of the pipes we don't use in the parent. */
    CloseHandle(hReadFromParent);
    CloseHandle(hWriteToParent);
    pTab->entryType = HE_PROCESS;
    pTab->hProcess = processInfo.hProcess;
    pTab->hInput = hReadFromChild;
    pTab->hOutput = hWriteToChild;
    pTab->hEvent = hEvent;
    pTab->readToken = ClosedToken;
    pTab->writeToken = ClosedToken;

    return(handToken);

error:
    {
        int err = GetLastError();
        free(commandName);
        free(arguments);
        // Close all the pipe handles.
        if (hWriteToChild != INVALID_HANDLE_VALUE) CloseHandle(hWriteToChild);
        if (hReadFromParent != INVALID_HANDLE_VALUE) CloseHandle(hReadFromParent);
        if (hWriteToParent != INVALID_HANDLE_VALUE) CloseHandle(hWriteToParent);
        if (hReadFromChild != INVALID_HANDLE_VALUE) CloseHandle(hReadFromChild);
        if (hEvent) CloseHandle(hEvent);
        raise_syscall(taskData, lpszError, err);
        return NULL; // Never reached.
    }
}

static Handle simpleExecute(TaskData *taskData, Handle args)
{
    HANDLE hNull = INVALID_HANDLE_VALUE;
    PROCESS_INFORMATION processInfo;
    Handle handToken;
    PHANDLETAB pTab;

    TCHAR *commandName = Poly_string_to_T_alloc(args->WordP()->Get(0));
    TCHAR *arguments = Poly_string_to_T_alloc(args->WordP()->Get(1));

    STARTUPINFO startupInfo;
    // Open a handle to NUL for input and output.
    hNull = CreateFile(_T("NUL"), GENERIC_READ|GENERIC_WRITE,
                FILE_SHARE_READ|FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
                FILE_ATTRIBUTE_NORMAL, NULL);

    // Create a STARTUPINFO structure in which to pass hNULL as stdin
    // and stdout to the new process.
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

    handToken = make_handle_entry(taskData);
    pTab = &handleTable[STREAMID(handToken)];
    pTab->entryType = HE_PROCESS;
    pTab->hProcess = processInfo.hProcess;
    // We only use the process handle entry.
    pTab->hInput = INVALID_HANDLE_VALUE;
    pTab->hOutput = INVALID_HANDLE_VALUE;
    pTab->hEvent = NULL;
    pTab->readToken = ClosedToken;
    pTab->writeToken = ClosedToken;

    return(handToken);
}

/* Return a stream, either text or binary, connected to an open process. */
static Handle openProcessHandle(TaskData *taskData, Handle args, BOOL fIsRead, BOOL fIsText)
{
    PHANDLETAB hnd = get_handle(args->Word(), HE_PROCESS);
    HANDLE hStream;
    int mode = 0, ioBits = 0;
    if (hnd == 0)
        raise_syscall(taskData, "Process is closed", ERROR_INVALID_HANDLE);

    if (fIsRead) hStream = hnd->hInput;
    else hStream = hnd->hOutput;
    /* I had previously assumed that it wasn't possible to get the
       same stream twice.  The current basis library definition allows
       it but warns it may produce unpredictable results.  For the moment
       we don't allow it because we could get problems with closing
       the same handle twice. */
    if (hStream == INVALID_HANDLE_VALUE)
        raise_syscall(taskData, "Process is closed", ERROR_INVALID_HANDLE);

    if (fIsRead) { mode = _O_RDONLY; ioBits = IO_BIT_READ; }
    else { mode = 0; ioBits = IO_BIT_WRITE; }
    if (fIsText) mode |= _O_TEXT; else mode |= _O_BINARY;

    Handle str_token = make_stream_entry(taskData);
    if (str_token == NULL) raise_syscall(taskData, "Insufficient memory", ERROR_NOT_ENOUGH_MEMORY);
    PIOSTRUCT strm = &basic_io_vector[STREAMID(str_token)];
    strm->device.ioDesc = _open_osfhandle ((POLYSIGNED) hStream, mode);
    if (strm->device.ioDesc == -1)
        raise_syscall(taskData, "_open_osfhandle failed", _doserrno);
    strm->ioBits = ioBits | IO_BIT_OPEN | IO_BIT_PIPE;

    /* The responsibility for closing the handle is passed to
       the stream package.  We need to retain a pointer to the
       stream entry so that we can close the stream in "reap". */
    if (fIsRead)
    {
        hnd->hInput = INVALID_HANDLE_VALUE;
        hnd->readToken = strm->token;
        // Pass the "input available" event.
        strm->hAvailable = hnd->hEvent;
        hnd->hEvent = NULL;
    }
    else
    {
        hnd->hOutput = INVALID_HANDLE_VALUE;
        hnd->writeToken = strm->token;
    }

    return str_token;
}

// Open a registry key and make an entry in the table for it.
static Handle openRegistryKey(TaskData *taskData, Handle args, HKEY hkParent)
{
    TCHAR keyName[MAX_PATH];
    LONG lRes;
    Handle result;
    PHANDLETAB pTab;
    HKEY hkey;
    REGSAM sam = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(2));
    POLYUNSIGNED length = Poly_string_to_C(args->WordP()->Get(1), keyName, MAX_PATH);
    if (length > MAX_PATH)
        raise_syscall(taskData, "Key name too long", ERROR_BAD_LENGTH);

    // Try opening the key.
    lRes = RegOpenKeyEx(hkParent, keyName, 0, sam, &hkey);
    if (lRes != ERROR_SUCCESS)
        raise_syscall(taskData, "RegOpenKeyEx failed", lRes);

    // Make an entry in the table.
    result = make_handle_entry(taskData);
    pTab = &handleTable[STREAMID(result)];
    pTab->entryType = HE_REGISTRY;
    pTab->hKey = hkey;
    return result;
}

// Create a registry key and make an entry in the table for it.
static Handle createRegistryKey(TaskData *taskData, Handle args, HKEY hkParent)
{
    TCHAR keyName[MAX_PATH];
    LONG lRes;
    Handle keyResult, dispRes, pair;
    PHANDLETAB pTab;
    HKEY hkey;
    DWORD dwDisp;
    REGSAM sam = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(3));
    unsigned opt = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(2));
    POLYUNSIGNED length = Poly_string_to_C(args->WordP()->Get(1), keyName, MAX_PATH);
    if (length > MAX_PATH)
        raise_syscall(taskData, "Key name too long", ERROR_BAD_LENGTH);

    // Try opening the key.
    lRes = RegCreateKeyEx(hkParent, keyName, 0, NULL,
                opt ? REG_OPTION_NON_VOLATILE : REG_OPTION_VOLATILE,
                sam, NULL, &hkey, &dwDisp);
    if (lRes != ERROR_SUCCESS)
        raise_syscall(taskData, "RegCreateKeyEx failed", lRes);

    // Make an entry in the table.
    keyResult = make_handle_entry(taskData);
    pTab = &handleTable[STREAMID(keyResult)];
    pTab->entryType = HE_REGISTRY;
    pTab->hKey = hkey;
    // Record whether this was new or old.
    dispRes = Make_fixed_precision(taskData, dwDisp == REG_CREATED_NEW_KEY ? 0: 1);
    /* Return a pair of the disposition and the token. */
    pair = alloc_and_save(taskData, 2);
    DEREFHANDLE(pair)->Set(0, DEREFWORDHANDLE(dispRes));
    DEREFHANDLE(pair)->Set(1, DEREFWORDHANDLE(keyResult));
    return pair;
}

// Delete a key.  Note that in Windows NT (but not 95) this will fail if
// the key has subkeys.
static Handle deleteRegistryKey(TaskData *taskData, Handle args, HKEY hkParent)
{
    TCHAR keyName[MAX_PATH];
    LONG lRes;
    POLYUNSIGNED length = Poly_string_to_C(args->WordP()->Get(1), keyName, MAX_PATH);
    if (length > MAX_PATH)
        raise_syscall(taskData, "Key name too long", ERROR_BAD_LENGTH);

    // Try deleting the key.  
    lRes = RegDeleteKey(hkParent, keyName);
    if (lRes != ERROR_SUCCESS)
        /* Return the error. */
        raise_syscall(taskData, "RegDeleteKey failed", lRes);
    return Make_fixed_precision(taskData, 0);
}

static Handle deleteRegistryValue(TaskData *taskData, Handle args, HKEY hkParent)
{
    TCHAR keyName[MAX_PATH];
    LONG lRes;
    POLYUNSIGNED length = Poly_string_to_C(args->WordP()->Get(1).AsObjPtr(),
                    keyName, MAX_PATH);
    if (length > MAX_PATH)
        raise_syscall(taskData, "Key name too long", ERROR_BAD_LENGTH);

    // Try deleting the value.
    lRes = RegDeleteValue(hkParent, keyName);
    if (lRes != ERROR_SUCCESS)
        /* Return the original error. */
        raise_syscall(taskData, "RegDeleteValue failed", lRes);
    return Make_fixed_precision(taskData, 0);
}

static Handle queryRegistryKey(TaskData *taskData, Handle args, HKEY hkey)
{
    TCHAR valName[MAX_PATH];
    byte *keyValue = 0;
    LONG lRes;
    DWORD valSize;
    Handle result, resVal, resType;
    DWORD dwType;
    POLYUNSIGNED length = Poly_string_to_C(args->WordP()->Get(1), valName, MAX_PATH);
    if (length > MAX_PATH)
        raise_syscall(taskData, "Value name too long", ERROR_BAD_LENGTH);

    // How long is the entry?
    lRes = RegQueryValueEx(hkey, valName, 0, NULL, NULL, &valSize);
    // When opening HKEY_PERFORMANCE_DATA we don't get a sensible
    // answer here.
    if (lRes == ERROR_MORE_DATA) valSize = 1024; // Guess
    else if (lRes != ERROR_SUCCESS)
        raise_syscall(taskData, "RegQueryValueEx failed", lRes);
    // Allocate that much store and get the value.  We could
    // try reading directly into ML store to save copying but
    // it hardly seems worthwhile.
    // Note: It seems that valSize can be zero for some items.
    if (valSize == 0) resVal = SAVE(C_string_to_Poly(taskData, "", 0));
    else
    {
        do {
            byte *newAlloc = (byte*)realloc(keyValue, valSize);
            if (newAlloc == 0)
            {
                free(keyValue);
                raise_syscall(taskData, "Insufficient memory", ERROR_NOT_ENOUGH_MEMORY);
            }
            keyValue = newAlloc;
            lRes = RegQueryValueEx(hkey, valName, 0, &dwType, keyValue, &valSize);
            // In the special case of HKEY_PERFORMANCE_DATA we may need to keep
            // growing the buffer.
            if (lRes == ERROR_MORE_DATA) valSize = valSize + 1024;
        } while (lRes == ERROR_MORE_DATA);

        if (lRes != ERROR_SUCCESS)
        {
            free(keyValue);
            raise_syscall(taskData, "RegQueryValue failed", lRes);
        }
        // If we have a string we have to convert this to ANSI/utf-8.
        if (dwType == REG_SZ || dwType == REG_MULTI_SZ || dwType == REG_EXPAND_SZ)
            resVal = SAVE(C_string_to_Poly(taskData, (TCHAR*)keyValue, valSize / sizeof(TCHAR)));
        else resVal = SAVE(C_string_to_Poly(taskData, (char*)keyValue, valSize));
        free(keyValue);
    }

    /* Create a pair containing the type and the value. */
    resType = Make_fixed_precision(taskData, dwType);
    result = alloc_and_save(taskData, 2);
    DEREFHANDLE(result)->Set(0, DEREFWORDHANDLE(resType));
    DEREFHANDLE(result)->Set(1, DEREFWORDHANDLE(resVal));
    return result;
}

static Handle setRegistryKey(TaskData *taskData, Handle args, HKEY hkey)
{
    TCHAR valName[MAX_PATH];
    LONG lRes;
    PolyWord str = args->WordP()->Get(3);
    POLYUNSIGNED length = Poly_string_to_C(args->WordP()->Get(1), valName, MAX_PATH);
    DWORD dwType = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(2));
    if (length > MAX_PATH)
        raise_syscall(taskData, "Value name too long", ERROR_BAD_LENGTH);

    // The value is binary.  Strings will already have had a null added.
    if (IS_INT(str))
    {
        byte b = (byte)UNTAGGED(str);
        // Single byte value.
        lRes = RegSetValueEx(hkey, valName, 0, dwType, &b, 1);
    }
    else
    {
        PolyStringObject *ps = (PolyStringObject*)str.AsObjPtr();
        lRes = RegSetValueEx(hkey, valName, 0, dwType,
                            (CONST BYTE *)ps->chars, (DWORD)ps->length);
    }

    if (lRes != ERROR_SUCCESS)
        raise_syscall(taskData, "RegSetValue failed", lRes);

    return Make_fixed_precision(taskData, 0);
}

// Enumerate a key or a value.  Returns a string option containing NONE if
// no key/value could be found or SOME s where s is the name of the key/value.
static Handle enumerateRegistry(TaskData *taskData, Handle args, HKEY hkey, BOOL isKey)
{
    DWORD num = get_C_unsigned(taskData, DEREFWORDHANDLE(args)->Get(1));
    LONG lRes;
    TCHAR keyName[MAX_PATH];
    DWORD dwLength = sizeof(keyName)/sizeof(keyName[0]);
    Handle result, resVal;
    if (isKey)
    {
        FILETIME ftMod;
        lRes = RegEnumKeyEx(hkey, num, keyName, &dwLength, NULL, NULL, NULL, &ftMod);
        if (lRes != ERROR_SUCCESS && lRes != ERROR_NO_MORE_ITEMS)
            raise_syscall(taskData, "RegEnumKeyEx failed", lRes);
    }
    else
    {
        lRes = RegEnumValue(hkey, num, keyName, &dwLength, NULL, NULL, NULL, NULL);
        if (lRes != ERROR_SUCCESS && lRes != ERROR_NO_MORE_ITEMS)
            raise_syscall(taskData, "RegEnumValue failed", lRes);
    }
    if (lRes == ERROR_NO_MORE_ITEMS)
        return SAVE(NONE_VALUE); /* NONE. */
    resVal = SAVE(C_string_to_Poly(taskData, keyName));
    result = alloc_and_save(taskData, 1);
    DEREFHANDLE(result)->Set(0, DEREFWORDHANDLE(resVal));
    return result;
}

struct _entrypts osSpecificEPT[] =
{
    { "PolyGetOSType",                  (polyRTSFunction)&PolyGetOSType},
    { "PolyOSSpecificGeneral",          (polyRTSFunction)&PolyOSSpecificGeneral},

    { NULL, NULL} // End of list.
};

class WindowsModule: public RtsModule
{
public:
    virtual void Init(void);
    virtual void Stop(void);
    void GarbageCollect(ScanAddress *process);
};

// Declare this.  It will be automatically added to the table.
static WindowsModule windowsModule;

void WindowsModule::GarbageCollect(ScanAddress *process)
/* Ensures that all the objects are retained and their addresses updated. */
{
    POLYUNSIGNED i;
    /* Entries in the file table. These are marked as weak references so may
       return 0 for unreferenced streams. */
    for(i = 0; i < maxHandleTab; i++)
    {
        PHANDLETAB str = &(handleTable[i]);
        if (str->token != 0)
        {
            if (str->entryType == HE_PROCESS)
            {
                /* Update the references to opened streams but
                   do this only as weak references.  If the stream
                   has gone away then that's fine. */
                if (str->readToken.IsDataPtr())
                {
                    PolyObject *token = str->readToken.AsObjPtr();
                    process->ScanRuntimeAddress(&token, ScanAddress::STRENGTH_WEAK);
                    str->readToken = token == 0 ? ClosedToken : token;
                }
                if (str->writeToken.IsDataPtr())
                {
                    PolyObject *token = str->writeToken.AsObjPtr();
                    process->ScanRuntimeAddress(&token, ScanAddress::STRENGTH_WEAK);
                    str->writeToken = token == 0 ? ClosedToken : token;
                }
            }
            process->ScanRuntimeAddress(&str->token, ScanAddress::STRENGTH_WEAK);
            /* Unreferenced entries may return zero. */ 
            if (str->token == 0 && str->entryType != HE_UNUSED)
                close_handle(str);
        }
    }
}

void WindowsModule::Stop(void)
{
    if (handleTable)
    {
        for (POLYUNSIGNED i = 0; i < maxHandleTab; i++)
        {
            if (handleTable[i].token != 0)
                close_handle(&handleTable[i]);
        }
        free(handleTable);
    }
    handleTable = NULL;
}

void WindowsModule::Init(void)
{
    maxHandleTab = 5; /* Initialise to a small number. */
    /* A vector for the streams (initialised by calloc) */
    handleTable = (PHANDLETAB)calloc(maxHandleTab,sizeof(HANDLETAB));
}


