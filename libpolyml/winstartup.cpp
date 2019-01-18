/*
Title:      Poly/ML Start-up code.

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

// This was previously part of the Console and Winguiconsole file.

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

#ifdef HAVE_WINDOWS_H
#include <winsock2.h> // Include first to avoid conflicts
#include <windows.h>
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

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#include "PolyControl.h"
#include "mpoly.h"
#include "sighandler.h" // For RequestConsoleInterrupt
#include "winguiconsole.h"
#include "../polyexports.h"
#include "processes.h"
#include "io_internal.h"
#include "winstartup.h"


#ifdef _MSC_VER
// Don't tell me about ISO C++ changes.
#pragma warning(disable:4996)
#endif

HINSTANCE hApplicationInstance;     // Application instance (exported)

static HWND hDDEWindow;     // Window to handle DDE requests from ML thread.

static LPTSTR*  lpArgs = 0; // Argument list.
static int      nArgs = 0;
static int initDDEControl(const TCHAR *lpszName);
static void uninitDDEControl(void);
static DWORD dwDDEInstance;

// useConsole is read in diagnostics to indicate whether to
// put up a message box.
bool useConsole = false;

// The streams set by PolyWinMain.  These are used by winbasicio.
WinStream *standardInput, *standardOutput, *standardError;

// Default DDE service name.
#define POLYMLSERVICE   _T("PolyML")

#ifdef UNICODE
#define DDECODEPAGE CP_WINUNICODE
#else
#define DDECODEPAGE CP_WINANSI
#endif

/* Messages interpreted by the main window thread. */
//#define WM_ADDTEXT      WM_APP
#define WM_DDESTART     (WM_APP+1)
#define WM_DDESTOP      (WM_APP+2)
#define WM_DDEEXEC      (WM_APP+3)
#define WM_DDESERVINIT      (WM_APP+4)

/* DDE requests.  DDE uses an internal window for communication and so all
DDE operations on a particular instance handle have to be performed by
the same thread.  That thread also has to check and process the message
queue.  The previous version did this by having the ML thread make the
DDE calls and processed the message list in an "interrupt" routine.
That complicates the Windows interface so now the ML thread sends messages
to the main window thread to do the work. */
HCONV StartDDEConversation(TCHAR *serviceName, TCHAR *topicName)
{
    return (HCONV)SendMessage(hDDEWindow, WM_DDESTART, (WPARAM)serviceName, (LPARAM)topicName);
}

void CloseDDEConversation(HCONV hConv)
{
    SendMessage(hDDEWindow, WM_DDESTOP, 0, (LPARAM)hConv);
}

LRESULT ExecuteDDE(char *command, HCONV hConv)
{
    return SendMessage(hDDEWindow, WM_DDEEXEC, (WPARAM)hConv, (LPARAM)command);
}

// This is called by the main Poly/ML thread after the arguments have been processed.
void SetupDDEHandler(const TCHAR *lpszServiceName)
{
    SendMessage(hDDEWindow, WM_DDESERVINIT, 0, (LPARAM)lpszServiceName);
}

LRESULT CALLBACK DDEWndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch (uMsg)
    {
    case WM_DDESERVINIT:
        return initDDEControl((const TCHAR*)lParam);

    case WM_DDESTART:
    {
        HCONV hcDDEConv;
        HSZ hszServiceName, hszTopicName;
        TCHAR *serviceName = (TCHAR*)wParam;
        TCHAR *topicName = (TCHAR*)lParam;
        hszServiceName =
            DdeCreateStringHandle(dwDDEInstance, serviceName, DDECODEPAGE);
        hszTopicName =
            DdeCreateStringHandle(dwDDEInstance, topicName, DDECODEPAGE);
        hcDDEConv =
            DdeConnect(dwDDEInstance, hszServiceName, hszTopicName, NULL);
        DdeFreeStringHandle(dwDDEInstance, hszServiceName);
        DdeFreeStringHandle(dwDDEInstance, hszTopicName);
        if (hcDDEConv == 0)
        {
            // UINT nErr = DdeGetLastError(dwDDEInstance);
            return 0;
        }
        return (LRESULT)hcDDEConv;
    }

    case WM_DDESTOP:
        DdeDisconnect((HCONV)lParam);
        return 0;

    case WM_DDEEXEC:
    {
        HDDEDATA res;
        LPSTR   command = (LPSTR)lParam;
        res = DdeClientTransaction((LPBYTE)command, (DWORD)(strlen(command) + 1),
            (HCONV)wParam, 0L, 0, XTYP_EXECUTE, TIMEOUT_ASYNC, NULL);
        if (res != 0)
        {
            DdeFreeDataHandle(res);
            // Succeeded - return true;
            return 1;
        }
        else if (DdeGetLastError(dwDDEInstance) == DMLERR_BUSY)
            // If it's busy return false.
            return 0;
        else return -1; // An error
    }

    default:
        return DefWindowProc(hwnd, uMsg, wParam, lParam);
    }
}

static DWORD WINAPI MainThrdProc(LPVOID lpParameter)
// This thread simply continues with the rest of the ML
// initialistion.
{
    exportDescription *exports = (exportDescription *)lpParameter;
    return polymain(nArgs, lpArgs, exports);
}

// A wrapper for WinInOutStream for use with standard input that records the
// original file type.
class WinStdInPipeStream : public WinInOutStream
{
public:
    WinStdInPipeStream(int kind) : fKind(kind) {}
    virtual int fileKind() {
        return fKind;
    }
protected:
    int fKind;
};

// Thread to copy everything from the input to the output.
class CopyThread {
public:
    CopyThread() :
        hInput(INVALID_HANDLE_VALUE), hOutput(INVALID_HANDLE_VALUE) {}

    bool RunCopy(HANDLE hIn, HANDLE hOut);
private:
    ~CopyThread();

    void threadFunction(void);
    HANDLE hInput, hOutput;
    static DWORD WINAPI copyThread(LPVOID lpParameter);
};

CopyThread::~CopyThread()
{
    if (hOutput != INVALID_HANDLE_VALUE) CloseHandle(hOutput);
    if (hInput != INVALID_HANDLE_VALUE) CloseHandle(hInput);
}

// Static thread function.  Deletes the CopyThread object when the copying is complete.
// That closes the handles.
DWORD WINAPI CopyThread::copyThread(LPVOID lpParameter)
{
    CopyThread *cp = (CopyThread *)lpParameter;
    cp->threadFunction();
    delete cp;
    return 0;
}

void CopyThread::threadFunction()
{
    char buffer[4096];

    while (true) {
        DWORD dwRead;
        if (!ReadFile(hInput, buffer, sizeof(buffer), &dwRead, NULL))
            return;

        if (dwRead == 0) // End-of-stream
        {
            DWORD dwErr = GetLastError();
            // If we are reading from the (Windows) console and the user presses ctrl-C we
            // may get a ERROR_OPERATION_ABORTED error.
            if (dwErr == ERROR_OPERATION_ABORTED)
            {
                SetLastError(0); // Reset this.  We may have a normal EOF next.
                continue;
            }
            // Normal exit.  Indicate EOF
            return;
        }

        char *b = buffer;
        do {
            DWORD dwWritten;
            if (!WriteFile(hOutput, b, dwRead, &dwWritten, NULL))
                return;
            b += dwWritten;
            dwRead -= dwWritten;
        } while (dwRead != 0);
    }
}

// Set up the copying.  It closes the handles when it has finished.
bool CopyThread::RunCopy(HANDLE hIn, HANDLE hOut)
{
    DWORD dwInId;
    hInput = hIn;
    hOutput = hOut;
    HANDLE hInThread = CreateThread(NULL, 0, copyThread, this, 0, &dwInId);
    if (hInThread == NULL) return false;
    CloseHandle(hInThread);
    return true;
}

// Called with various control events if the input stream is a console.
static BOOL WINAPI CtrlHandler(DWORD dwCtrlType)
{
    if (dwCtrlType == CTRL_C_EVENT)
    {
        RequestConsoleInterrupt();
        return TRUE;
    }
    return FALSE;
}

// Main entry point.  Called from WinMain with a pointer to the ML code.
int PolyWinMain(
    HINSTANCE hInstance,
    HINSTANCE hPrevInstance,
    LPSTR lpCmdLineUnused,
    int nCmdShow,
    exportDescription *exports
)
{
    DWORD dwInId, dwRes;

    SetErrorMode(0); // Force a proper error report

    hApplicationInstance = hInstance;

    // If we already have standard input and standard output we
    // don't replace them, otherwise we create a window and pipes
    // to connect it.  We use _get_osfhandle here because that
    // checks for handles passed in in the STARTUPINFO as well as
    // those inherited as standard handles.
    HANDLE hStdInHandle = (HANDLE)_get_osfhandle(fileno(stdin));
    HANDLE hStdOutHandle = (HANDLE)_get_osfhandle(fileno(stdout));
    HANDLE hStdErrHandle = (HANDLE)_get_osfhandle(fileno(stderr));

    // If we don't have standard output we're going to create a console for output.
    // We also use this for input except if we've provided standard input but not standard ouput.
    useConsole = hStdOutHandle == INVALID_HANDLE_VALUE;

    // Do we have stdin?  If we do we need to create a pipe to buffer the input.
    if (hStdInHandle != INVALID_HANDLE_VALUE)
    {
        // We have to capture the original type here.  hStdInHandle itself will
        // be closed when we set the new stdin and hOldStdin could be closed by
        // the pipe input thread almost immediately if the input is a small file.
        int originalKind = WinStream::fileTypeOfHandle(hStdInHandle);

        TCHAR pipeName[MAX_PATH];
        newPipeName(pipeName);
        HANDLE hOutputPipe =
            CreateNamedPipe(pipeName, PIPE_ACCESS_OUTBOUND | FILE_FLAG_FIRST_PIPE_INSTANCE,
                PIPE_READMODE_BYTE | PIPE_WAIT | PIPE_REJECT_REMOTE_CLIENTS, 1, 4096, 4096, 0, NULL);
        if (hOutputPipe == INVALID_HANDLE_VALUE)
            return 1;

        HANDLE hNewStdin =
            CreateFile(pipeName, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OVERLAPPED, NULL);
        if (hNewStdin == INVALID_HANDLE_VALUE)
            return 1;
        // Copy everything from the original standard input to the pipe.
        CopyThread *copyStdin = new CopyThread;
        if (!copyStdin->RunCopy(hStdInHandle, hOutputPipe))
            return 1;

        SetConsoleCtrlHandler(CtrlHandler, TRUE); // May fail if there's no console.
                                                  // Leave the original stdIn.

        WinStdInPipeStream *stndIn = new WinStdInPipeStream(originalKind);
        stndIn->openHandle(hNewStdin, OPENREAD, true);
        standardInput = stndIn;
        CloseHandle(hNewStdin); // Duplicated
    }
    else
    {
        // There was no standard input.  If we didn't have standard output either and are using
        // our GUI console use that for input.  Otherwise open a stream on "NUL"
        if (useConsole)
            standardInput = createConsoleStream();
        else
        {
            WinInOutStream *inStream = new WinInOutStream;
            // We can't use openFile here because we don't have a taskData yet.
            HANDLE hNewStdin =
                CreateFile(_T("NUL"), GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OVERLAPPED, NULL);
            inStream->openHandle(hNewStdin, OPENREAD, true);
            standardInput = inStream;
        }
    }

    if (useConsole)
    {
        HANDLE hWriteToScreen = createConsoleWindow(nCmdShow);
        if (hWriteToScreen == INVALID_HANDLE_VALUE)
            return 1;
        // hWriteToScreen is used as stdout .
        // hReadFromML is what the screen thread reads from.
        WinInOutStream *stdOut = new WinInOutStream;
        // Safe since we duplicate the handle.
        stdOut->openHandle(hWriteToScreen, OPENWRITE, true);
        standardOutput = stdOut; // Pass to winbasicio
                                 // Replace the standard Windows handles.  This may be used in OS.Process.system.
        SetStdHandle(STD_OUTPUT_HANDLE, hWriteToScreen);
        // A few RTS modules use stdio for output, primarily objsize and diagnostics.
        // Doing this causes the stdIn package to close hWriteToScreen during shut-down.
        polyStdout = _fdopen(_open_osfhandle((INT_PTR)hWriteToScreen, _O_TEXT), "wt"); // == stdout

        if (hStdErrHandle == INVALID_HANDLE_VALUE)
        {
            // If we didn't have stderr write any stderr output to our console.
            WinInOutStream *stdErr = new WinInOutStream;
            stdErr->openHandle(hWriteToScreen, OPENWRITE, true);
            standardError = stdErr;
            HANDLE hStderr;
            // We definitely need a duplicate for stdio since it closes each stream on exit
            // We may also inherit it in OS.Process.system so make it inheritable.
            if (!DuplicateHandle(GetCurrentProcess(), hWriteToScreen, GetCurrentProcess(), &hStderr, 0, TRUE, DUPLICATE_SAME_ACCESS))
                return 1;
            SetStdHandle(STD_ERROR_HANDLE, hStderr);
            // Used in a few cases for diagnostics.
            polyStderr = _fdopen(_open_osfhandle((INT_PTR)hStderr, _O_TEXT), "wt");
        }
    }
    else
    {
        // Create a copy stream
        TCHAR pipeName[MAX_PATH];
        newPipeName(pipeName);
        HANDLE hInputPipe =
            CreateNamedPipe(pipeName, PIPE_ACCESS_INBOUND | FILE_FLAG_FIRST_PIPE_INSTANCE,
                PIPE_READMODE_BYTE | PIPE_WAIT | PIPE_REJECT_REMOTE_CLIENTS, 1, 4096, 4096, 0, NULL);
        if (hInputPipe == INVALID_HANDLE_VALUE)
            return 1;
        HANDLE hNewStdout =
            CreateFile(pipeName, GENERIC_WRITE, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OVERLAPPED, NULL);
        if (hNewStdout == INVALID_HANDLE_VALUE)
            return 1;
        // Copy everything from the original standard input to the pipe.
        CopyThread *copyStdout = new CopyThread;
        if (!copyStdout->RunCopy(hInputPipe, hStdOutHandle))
            return 1;
        WinInOutStream *standOut = new WinInOutStream;
        standOut->openHandle(hNewStdout, OPENWRITE, true);
        standardOutput = standOut;
        // Leave the existing stdout and use it for diagnostics if necessary.
        polyStdout = stdout;

        if (hStdErrHandle == INVALID_HANDLE_VALUE)
        {
            // We had a stdout but not stderr.  We could choose to direct stderr output
            // to the provided stdout but maybe that's not what the user wants.  Instead
            // we open one on NUL.
            polyStderr = fopen("NUL", "wt");
            HANDLE hStdErr = (HANDLE)_get_osfhandle(fileno(polyStderr));
            SetStdHandle(STD_ERROR_HANDLE, hStdErr);
            hStdErrHandle = hStdErr;
        }
        // Set up a stream for writes from ML.
        WinInOutStream *stdErr = new WinInOutStream;
        stdErr->openHandle(hStdErrHandle, OPENWRITE, true);
        standardError = stdErr;
    }

    // Set nArgs and lpArgs to the command line arguments.
    // Convert the command line into Unix-style arguments.  There isn't a
    // CommandLineToArgvA function so we have to use the Unicode version and
    // convert the results.
    {
        // Get the unicode args
        LPWSTR *uniArgs = CommandLineToArgvW(GetCommandLineW(), &nArgs);
#ifdef UNICODE
        lpArgs = uniArgs;
#else
        if (uniArgs != NULL)
        {
            lpArgs = (LPSTR*)calloc(nArgs, sizeof(LPSTR));
            if (lpArgs != 0)
            {
                for (int i = 0; i < nArgs; i++)
                {
                    // See how much space will be needed
                    int space =
                        WideCharToMultiByte(CP_ACP, 0, uniArgs[i], -1, NULL, 0, NULL, NULL);
                    if (space == 0) break; // Failed for some reason
                                           // Allocate the space then do the conversion
                    LPSTR buff = (LPSTR)malloc(space);
                    if (buff == 0) break;
                    int result =
                        WideCharToMultiByte(CP_ACP, 0, uniArgs[i], -1, buff, space, NULL, NULL);
                    if (result == 0) { free(buff); break; }
                    lpArgs[i] = buff;
                }
            }
            LocalFree(uniArgs);
        }
#endif
    }

    // Create an internal hidden window to handle DDE requests from the ML thread.
    {
        WNDCLASSEX wndClass;
        ATOM atClass;
        memset(&wndClass, 0, sizeof(wndClass));
        wndClass.cbSize = sizeof(wndClass);
        wndClass.lpfnWndProc = DDEWndProc;
        wndClass.hInstance = hInstance;
        wndClass.lpszClassName = _T("PolyMLDDEWindowClass");

        if ((atClass = RegisterClassEx(&wndClass)) == 0) return 1;

        hDDEWindow = CreateWindow(
            (LPTSTR)(intptr_t)atClass,
            _T("Poly/ML-DDE"),
            WS_OVERLAPPEDWINDOW,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            NULL,
            NULL,   // handle to menu or child-window identifier
            hInstance,
            NULL     // pointer to window-creation data
        );
    }

    // Call the main program to do the rest of the initialisation.
    HANDLE hMainThread = CreateThread(NULL, 0, MainThrdProc, exports, 0, &dwInId);

    // Enter the main message loop.
    while (MsgWaitForMultipleObjects(1, &hMainThread,
        FALSE, INFINITE, QS_ALLINPUT) == WAIT_OBJECT_0 + 1)
    {
        MSG Msg;
        while (PeekMessage(&Msg, NULL, 0, 0, PM_REMOVE))
        {
            TranslateMessage(&Msg);
            DispatchMessage(&Msg);
        }
    }

    if (!GetExitCodeThread(hMainThread, &dwRes)) dwRes = 0;

    uninitDDEControl();
    DestroyWindow(hDDEWindow);

    return dwRes;
}

HDDEDATA CALLBACK DdeCallback(UINT uType, UINT uFmt, HCONV hconv,
    HSZ hsz1, HSZ hsz2, HDDEDATA hdata,
    ULONG_PTR dwData1, ULONG_PTR dwData2)
{
    switch (uType)
    {
    case XTYP_CONNECT:
        // Client connecting.  For the moment we ignore
        // the topic.
        return (HDDEDATA)TRUE;

    case XTYP_EXECUTE:
    {
        // See what the message is.  The only ones we
        // handle are interrupt and terminate.
        TCHAR buff[256];
        buff[0] = 0;
        DdeGetData(hdata, (LPBYTE)buff, sizeof(buff), 0);
        if (lstrcmpi(buff, _T(POLYINTERRUPT)) == 0)
        {
            RequestConsoleInterrupt();
            return (HDDEDATA)DDE_FACK;
        }
        if (lstrcmpi(buff, _T(POLYTERMINATE)) == 0)
        {
            processes->RequestProcessExit(0);
            return (HDDEDATA)DDE_FACK;
        }
        return (HDDEDATA)DDE_FNOTPROCESSED;
    }

    default:
        return (HDDEDATA)NULL;
    }
}

static int initDDEControl(const TCHAR *lpszName)
{
    // Start the DDE service.  This receives remote requests.
    if (DdeInitialize(&dwDDEInstance, DdeCallback,
        APPCLASS_STANDARD | CBF_FAIL_ADVISES | CBF_FAIL_POKES |
        CBF_FAIL_REQUESTS | CBF_SKIP_ALLNOTIFICATIONS, 0)
        != DMLERR_NO_ERROR)
        return 0;

    // If we were given a service name we register that,
    // otherwise we use the default name.
    if (lpszName == 0) lpszName = POLYMLSERVICE;
    HSZ hszServiceName = DdeCreateStringHandle(dwDDEInstance, lpszName, DDECODEPAGE);
    if (hszServiceName == 0) return 0;

    DdeNameService(dwDDEInstance, hszServiceName, 0L, DNS_REGISTER);

    DdeFreeStringHandle(dwDDEInstance, hszServiceName);
    return 1;
}

static void uninitDDEControl(void)
{
    // Unregister our name(s).
    DdeNameService(dwDDEInstance, 0L, 0L, DNS_UNREGISTER);
    //  DdeAbandonTransaction(dwDDEInstance, 0L, 0L);
    // Unitialise DDE.
    DdeUninitialize(dwDDEInstance);
}
