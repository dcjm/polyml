/*
    Title:      Poly/ML Console Window.

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

#include "../resource.h"
#include "sighandler.h" // For RequestConsoleInterrupt
#include "processes.h"
#include "polystring.h" // For codepage
#include "io_internal.h"
#include "locking.h"
#include "winguiconsole.h"

/*
This module takes the place of the Windows console which
has a number of problems, apart from not being a pleasant
user interface.  It creates a main window containing an edit
control, which it has to sub-class so that we can receive all
the characters as they are typed.
I've written this in C using the direct Windows calls to make
it fairly independent of the compiler.  It would definitely be
simpler and cleaner written in C++ using MFC.
DCJM 30/5/2000.
*/

HWND hMainWindow = NULL; // Main window - exported.
extern HINSTANCE hApplicationInstance;     // Application instance (exported)
static HANDLE  hReadFromML; // Handles to pipe from ML thread
static WNDPROC  wpOrigEditProc; // Saved window proc.
static bool fAtEnd;         // True if we are at the end of the window
static HWND hEditWnd;       // Edit sub-window
static CHAR *pchInputBuffer; // Buffer to text read.
static int  nBuffLen;       // Length of input buffer.
static int  nNextPosn;      // Position to add input. (<= nBuffLen)
static int  nAvailable;     // Position of "committed" input (<= nNextPosn)
static int  nReadPosn;      // Position of last read (<= nAvailable)
static PLock iOInterlock;
static HANDLE hInputEvent;  // Signalled when input is available.

static int nInitialShow; // Value of nCmdShow passed in.
static bool isActive = false;

#ifdef UNICODE
#define DDECODEPAGE CP_WINUNICODE
#else
#define DDECODEPAGE CP_WINANSI
#endif


/* All addition is made at the end of the text so this function is
   called to find out if we're there. */
static void MoveToEnd(void)
{
    if (! fAtEnd)
    {
        // Make sure any text we add goes at the end.
        LRESULT dwEnd = SendMessage(hEditWnd, WM_GETTEXTLENGTH, 0, 0);
        SendMessage(hEditWnd, EM_SETSEL, dwEnd, dwEnd);
        fAtEnd = true;
    }
}

// Remove lines at the beginning until we have enough space.
static void CheckForScreenSpace(size_t nChars)
{
    // TODO: We could avoid these calls by remembering this information.
    size_t limit = SendMessage(hEditWnd, EM_GETLIMITTEXT, 0, 0);
    size_t size = SendMessage(hEditWnd, WM_GETTEXTLENGTH, 0, 0);
    if (nChars > limit)
    {
        SetWindowText(hEditWnd, _T(""));
        SendMessage(hEditWnd, EM_SETSEL, 1, 1); // Clear selection.
    }
    else if (nChars + size >= limit)
    {
        // We need to remove sufficient lines to make enough space.  Find the index of
        // the line that will create enough space and then delete to the start of the
        // next line.
        LRESULT lineNo = SendMessage(hEditWnd, EM_LINEFROMCHAR, nChars + size - limit, 0);
        LRESULT firstCh = SendMessage(hEditWnd, EM_LINEINDEX, lineNo + 1, 0);
        // Select the text we're going to remove
        SendMessage(hEditWnd, EM_SETSEL, 0, firstCh);
        // Use EM_REPLACESEL rather than WM_CLEAR since we don't want to undo.
        SendMessage(hEditWnd, EM_REPLACESEL, FALSE, (LPARAM)_T(""));
        fAtEnd = false; // Move the display to the end
        MoveToEnd();
    }
}

// Expand the buffer if necessary to allow room for
// additional characters.
static void CheckForBufferSpace(int nChars)
{
    BOOL fOverflow;
    if (nNextPosn >= nReadPosn)
        fOverflow = nNextPosn+nChars >= nReadPosn+nBuffLen;
    else fOverflow = nNextPosn+nChars >= nReadPosn;
    if (fOverflow)
    {
        int nOldLen = nBuffLen;
        // Need more space.
        nBuffLen = nBuffLen + nChars + nBuffLen/2;
        char *newBuffer = (char*)realloc(pchInputBuffer, nBuffLen);
        if (newBuffer == 0) return; // Not sure what to do here.
        pchInputBuffer = newBuffer;
        // Have to copy any data that has wrapped round to the
        // new area.
        if (nNextPosn < nReadPosn)
        {
            int nExtra = nBuffLen-nOldLen;
            if (nExtra >= nNextPosn)
            {
                // All the space before will fit in the new area.
                memcpy(pchInputBuffer+nOldLen, pchInputBuffer, nNextPosn);
            }
            else
            {
                // Some of the space before will fit but not all.
                memcpy(pchInputBuffer+nOldLen, pchInputBuffer, nExtra);
                memmove(pchInputBuffer, pchInputBuffer+nExtra, nNextPosn-nExtra);
            }
            // Adjust these pointers modulo the old and new lengths.
            if (nAvailable < nNextPosn) nAvailable += nOldLen;
            if (nAvailable >= nBuffLen) nAvailable -= nBuffLen;
            nNextPosn += nOldLen;
            if (nNextPosn >= nBuffLen) nNextPosn -= nBuffLen;
        }
    }
    ASSERT(nBuffLen >= 0 && nAvailable >= 0 && nNextPosn >= 0 && nAvailable < nBuffLen && nReadPosn < nBuffLen);
    if (nNextPosn > nReadPosn)
        ASSERT(nAvailable >= nReadPosn && nAvailable <= nNextPosn);
    else ASSERT((nNextPosn != nReadPosn &&
                 nAvailable <= nNextPosn) || nAvailable >= nReadPosn);
}

/* In order to be able to handle all the keys we need to
   sub-class the edit control.  */ 
static LRESULT APIENTRY EditSubclassProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) 
{
    switch (uMsg)
    {
    case WM_GETDLGCODE:
        return DLGC_WANTALLKEYS | DLGC_WANTCHARS;

    case WM_KEYDOWN:
        switch(wParam)
        {
        case VK_DELETE: // Ignore the delete key.  Beep perhaps?
            return 0;
        case VK_LEFT: // If we move the cursor we are probably not
        case VK_RIGHT: // at the end.
        case VK_UP:
        case VK_DOWN:
            fAtEnd = false;
        default:
            return CallWindowProc(wpOrigEditProc, hwnd, uMsg,  wParam, lParam);
        }

    case WM_CHAR:
        {
            LPARAM nRpt = lParam & 0xffff;
            PLocker locker(&iOInterlock);
            if (wParam == '\b')
            {
                // Delete the previous character(s).
                if (nNextPosn != nAvailable)
                {
                    int nCanRemove = 0;
                    while (nRpt-- > 0 && nNextPosn != nAvailable)
                    {
                        nCanRemove++;
                        if (nNextPosn == 0) nNextPosn = nBuffLen;
                        nNextPosn--;
                    }
                    lParam = (lParam & 0xffff0000) | nCanRemove;
                    return CallWindowProc(wpOrigEditProc, hwnd, uMsg,  wParam, lParam); 
                }
            }
            else if (wParam == 22) // Control-V
            {
                // Generate a Paste command.
                SendMessage(hMainWindow, WM_COMMAND, ID_EDIT_PASTE, 0);
            }
            else if (wParam == 3) // Control-C
            {
                // In Windows this has the effect of Copy but we also
                // want it to generate an interrupt.  I've chosen to
                // make it copy if there is any selection, otherwise to
                // generate an interrupt.  We'll have to see how this works.
                DWORD dwStart, dwEnd;
                SendMessage(hwnd, EM_GETSEL, (WPARAM)&dwStart, (LPARAM)&dwEnd);
                if (dwStart != dwEnd)
                {
                    SendMessage(hwnd, WM_COPY, 0, 0); 
                }
                else {
                    // Discard any type-ahead.
                    nNextPosn = nAvailable = nReadPosn = 0;
                    RequestConsoleInterrupt();
                }
            }
            else if (wParam >= ' ' || wParam == '\r' || wParam == '\t' ||
                     wParam == 4 /* ctrl-D */ || wParam == 26 /* ctrl-Z */)
            {
                CheckForBufferSpace((int)nRpt);
                CheckForScreenSpace(nRpt); // Make sure we have space on the screen.
                // Add the character(s) to the buffer.
                while (nRpt-- > 0)
                {
                    if (wParam == '\r')
                    {
                        pchInputBuffer[nNextPosn++] = '\n';
                        nAvailable = nNextPosn;
                        SetEvent(hInputEvent);
                    }
                    else if (wParam == 4 || wParam == 26)
                    {
                        // Treat either of these as EOF chars.
                        pchInputBuffer[nNextPosn++] = (CHAR)wParam;
                        nAvailable = nNextPosn;
                        SetEvent(hInputEvent);
                        wParam = 4;
                    }
                    else pchInputBuffer[nNextPosn++] = (CHAR)wParam;
                    if (nNextPosn == nBuffLen) nNextPosn = 0;
                    if (nAvailable == nBuffLen) nAvailable = 0;
                }
                MoveToEnd();
                // Add this to the window except if it's ctrl-Z or ctrl-D.
                if (wParam == 4 || wParam == 26) return 0;
                return CallWindowProc(wpOrigEditProc, hwnd, uMsg,  wParam, lParam); 
            }
            return 0;
        }

    case WM_DESTROY:
        {
            HFONT hFount;
            // Switch back to the old window proc just in case.
#ifdef _WIN64
            SetWindowLongPtr(hwnd, GWLP_WNDPROC, (INT_PTR)wpOrigEditProc);
            SetWindowLongPtr(hwnd, GWLP_USERDATA, 0);
#else
            SetWindowLong(hwnd, GWL_WNDPROC, (LONG)wpOrigEditProc);
            SetWindowLong(hwnd, GWL_USERDATA, 0);
#endif
            // Get the fount and delete it if it's not the default.
            hFount = (HFONT)SendMessage(hwnd, WM_GETFONT, 0, 0);
            if (hFount != NULL)
            {
                SendMessage(hwnd, WM_SETFONT, (WPARAM)NULL, FALSE);
                DeleteObject(hFount);
            }
            // Call the original to finish off.
            return CallWindowProc(wpOrigEditProc, hwnd, uMsg,  wParam, lParam); 
        }

    case WM_LBUTTONDOWN:
    case WM_RBUTTONDOWN:
    case WM_LBUTTONUP:
    case WM_RBUTTONUP:
    case EM_SETSEL:
        // Need to record that we may no longer be at the end of the text.
        fAtEnd = false;
        // Drop through to default.

    default:
        return CallWindowProc(wpOrigEditProc, hwnd, uMsg,  wParam, lParam); 
    }
} 

/* This function is only used when the "About Poly/ML" dialogue box is
   being displayed. */
static BOOL CALLBACK AboutProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch (uMsg)
    {
    case WM_INITDIALOG: return 1;
    case WM_COMMAND:
        if (wParam == IDOK)
        {
            EndDialog(hwndDlg, IDOK);
            return 1;
        }
    case WM_CLOSE:
        EndDialog(hwndDlg, IDOK);
        return 1;
    default: return 0;
    }
}

#ifdef UNICODE
#define CF_TEXTFORMAT   CF_UNICODETEXT
#else
#define CF_TEXTFORMAT   CF_TEXT
#endif

/* Messages interpreted by the main window thread. */
#define WM_ADDTEXT      WM_APP

/* This is the main window procedure. */
LRESULT CALLBACK WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch (uMsg) 
    { 
        case WM_CREATE:
            {
                hEditWnd = CreateWindow(_T("EDIT"), NULL,
                    WS_CHILD | WS_VISIBLE | WS_VSCROLL | WS_HSCROLL |
                    ES_LEFT | ES_MULTILINE | ES_AUTOVSCROLL | ES_AUTOHSCROLL, 
                    0, 0, 0, 0,
                    hwnd, 0, hApplicationInstance, NULL);
                if (hEditWnd == NULL) return -1; /* Failed */

                // Sub-class this so that we get the keys that are pressed.
                // Save the old window proc.
#ifdef _WIN64
                wpOrigEditProc =
                    (WNDPROC)GetWindowLongPtr(hEditWnd, GWLP_WNDPROC);
               // Set our new window proc.
                SetWindowLongPtr(hEditWnd, GWLP_WNDPROC, (INT_PTR)EditSubclassProc);
#else
                wpOrigEditProc =
                    (WNDPROC)GetWindowLong(hEditWnd, GWL_WNDPROC);
               // Set our new window proc.
                SetWindowLong(hEditWnd, GWL_WNDPROC, (LONG)EditSubclassProc);
#endif
                fAtEnd = true;

                // Get a 10 point Courier fount.
                HDC hDC = GetDC(hEditWnd);
                int nHeight = -MulDiv(10, GetDeviceCaps(hDC, LOGPIXELSY), 72);
                ReleaseDC(hEditWnd, hDC);
                HFONT hFont = CreateFont(nHeight, 0, 0, 0, FW_DONTCARE,
                                    FALSE, FALSE, FALSE, ANSI_CHARSET,
                                    OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
                                    DEFAULT_QUALITY, FIXED_PITCH | FF_MODERN,
                                    _T("Courier"));
                if (hFont) SendMessage(hEditWnd, WM_SETFONT, (WPARAM)hFont, 0);
 
                SetWindowText(hEditWnd, _T(""));
                return 0; /* Succeeded */
            }
 
        case WM_SETFOCUS:
            /* When the focus arrives at the parent set the focus on
               the edit window. */
            SetFocus(hEditWnd); 
            return 0; 

        case WM_SIZE:
            {
                LONG offset = 0;
                // Make the edit control the size of the window's client area.
                MoveWindow(hEditWnd, 0, offset, LOWORD(lParam), HIWORD(lParam)-offset, TRUE);
            }
            return 0;

        case WM_DESTROY:
            PostQuitMessage(0);
            return 0;

        case WM_COMMAND:
            switch(wParam)
            {
            case ID_EDIT_COPY:
                SendMessage(hEditWnd, WM_COPY, 0, 0); 
                return 0; 

            case ID_EDIT_PASTE:
                if (IsClipboardFormatAvailable(CF_TEXTFORMAT))
                {
                    // We need to check that we have enough space BEFORE we try pasting.
                    OpenClipboard(hEditWnd);
                    HANDLE hClip = GetClipboardData(CF_TEXTFORMAT);
                    if (hClip == NULL) return 0;
                    LPCTSTR lpszText = (LPCTSTR)GlobalLock(hClip);
                    if (lpszText == NULL) return 0;
                    CheckForScreenSpace(lstrlen(lpszText));
                    MoveToEnd();
                    // Add it to the screen.
                    SendMessage(hEditWnd, EM_REPLACESEL, FALSE, (LPARAM)lpszText);
                    // Add to the type-ahead.
                    PLocker locker(&iOInterlock);
                    // Check there's enough space.  This may be an
                    // over-estimate since we replace CRNL by NL.
                    CheckForBufferSpace(lstrlen(lpszText));
                    while (*lpszText)
                    {
                        // The data we're pasting contains CRNL as
                        // line separators.
                        if (lpszText[0] == '\r' && lpszText[1] == '\n')
                        {
                            pchInputBuffer[nNextPosn++] = '\n';
                            if (nNextPosn == nBuffLen) nNextPosn = 0;
                            nAvailable = nNextPosn;
                            lpszText += 2;
                        }
                        else {
                            pchInputBuffer[nNextPosn++] = (char)*lpszText++;
                            if (nNextPosn == nBuffLen) nNextPosn = 0;
                            if (lpszText[0] == 4 || lpszText[0] == 26)
                                nAvailable = nNextPosn; // EOF characters.
                        }
                    }
                    if (nAvailable != nReadPosn) SetEvent(hInputEvent);
                    GlobalUnlock(hClip);
                    CloseClipboard();
                }
                return 0; 

            case ID_HELP_ABOUT: 
                DialogBox(hApplicationInstance, MAKEINTRESOURCE(IDD_ABOUT_POLYML),
                    hwnd, (DLGPROC)AboutProc); 
                return 0;

            case ID_FILE_QUIT:
                if (MessageBox(hwnd, _T("Are you sure you want to quit?"),
                                     _T("Confirm Quit"), MB_OKCANCEL) == IDOK)
                    processes->RequestProcessExit(0);
                return 0;

            case ID_RUN_INTERRUPT:
                // Discard any type-ahead.
                nNextPosn = nAvailable = nReadPosn = 0;
                RequestConsoleInterrupt();
                return 0;

            default: return DefWindowProc(hwnd, uMsg, wParam, lParam);
            }

        case WM_CLOSE:
            if (MessageBox(hwnd, _T("Are you sure you want to quit?"),
                                 _T("Confirm Quit"), MB_OKCANCEL) == IDOK)
                processes->RequestProcessExit(0);
            return 0;

        case WM_ADDTEXT:
            // Request from the input thread to add some text.
            {
                // Remember the old selection and the original length.
                DWORD lrStart, lrEnd;
                SendMessage(hEditWnd, EM_GETSEL, (WPARAM)&lrStart, (LPARAM)&lrEnd);
                CheckForScreenSpace(lstrlen((TCHAR*)lParam));
                MoveToEnd();
                SendMessage(hEditWnd, EM_REPLACESEL, 0, lParam);
                return 0;
            }

        default:
            return DefWindowProc(hwnd, uMsg, wParam, lParam);
    } 
} 
 
static DWORD WINAPI InThrdProc(LPVOID lpParameter)
// This thread deals with input from the ML process.
{
    while (1)
    {
        CHAR buff[4096];
        DWORD dwRead;
        if (!ReadFile(hReadFromML, buff, sizeof(buff) - 1, &dwRead, NULL))
            return 0;
        buff[dwRead] = 0;
        if (! isActive) { ShowWindow(hMainWindow, nInitialShow); isActive = true; }
#ifdef UNICODE
        // We need to write Unicode here.  Convert it using the current code-page.
        int wlen = MultiByteToWideChar(codePage, 0, buff, -1, NULL, 0);
        if (wlen == 0) continue;
        WCHAR *wBuff = new WCHAR[wlen];
        wlen = MultiByteToWideChar(codePage, 0, buff, -1, wBuff, wlen);
        SendMessage(hMainWindow, WM_ADDTEXT, 0, (LPARAM)wBuff);
        delete[] wBuff;
#else
        SendMessage(hMainWindow, WM_ADDTEXT, 0, (LPARAM)buff);
#endif
    }
}

class WinGuiConsoleStream : public WinStream
{
public:
    WinGuiConsoleStream()  {}
    virtual bool testForInput(TaskData *taskData, unsigned waitMilliSecs);

    virtual bool testForOutput(TaskData *taskData, unsigned waitMilliSecs) {
        unimplemented(taskData);
        return false;
    }

    virtual size_t readStream(TaskData *taskData, byte *base, size_t length);

    virtual int fileKind() {
        return FILEKIND_TTY; // Treat it as a TTY i.e. an interactive input
    }

    virtual void closeEntry(TaskData *taskData) { } // Not closed

    virtual bool canOutput(TaskData *taskData) {
        return false;
    }
    virtual size_t writeStream(TaskData *taskData, byte *base, size_t length) {
        unimplemented(taskData);
        return 0;
    }
};

bool WinGuiConsoleStream::testForInput(TaskData *taskData, unsigned waitMilliSecs)
{
    if (!isActive) { ShowWindow(hMainWindow, nInitialShow); isActive = true; }
    {
        PLocker locker(&iOInterlock);
        if (nAvailable != nReadPosn) return true;
    }
    if (waitMilliSecs != 0)
    {
        WaitHandle waiter(hInputEvent, waitMilliSecs); // Global event
        processes->ThreadPauseForIO(taskData, &waiter);
    }
    return false; // It may actually be ready now.
}

size_t WinGuiConsoleStream::readStream(TaskData *taskData, byte *base, size_t length)
    /* Read characters from the input.  Only returns zero on EOF. */
{
    unsigned nRes = 0;
    if (!isActive) { ShowWindow(hMainWindow, nInitialShow); isActive = true; }
    PLocker locker(&iOInterlock);
    
    // Copy the available characters into the buffer.
    while (nReadPosn != nAvailable && length-- > 0)
    {
        char ch;
        ch = pchInputBuffer[nReadPosn];
        if (ch == 4 || ch == 26)
        {
            // EOF character.  We have to return this as
            // a separate buffer of size zero so if we've
            // already returned some characters we leave it till
            // next time.
            if (nRes == 0) if (++nReadPosn == nBuffLen) nReadPosn = 0;
            break;
        }
        base[nRes++] = ch;
        if (++nReadPosn == nBuffLen) nReadPosn = 0;
    }
    if (nAvailable == nReadPosn) ResetEvent(hInputEvent);

    return nRes;
}

HANDLE createConsoleWindow(int nCmdShow)
{
    WNDCLASSEX wndClass;
    ATOM atClass;
    // Allocate initial buffer space to maintain the invariants.
    hInputEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
    nBuffLen = 80;
    pchInputBuffer = (char*)malloc(nBuffLen);

    TCHAR pipeName[MAX_PATH];
    newPipeName(pipeName);
    hReadFromML =
        CreateNamedPipe(pipeName, PIPE_ACCESS_INBOUND | FILE_FLAG_FIRST_PIPE_INSTANCE,
            PIPE_READMODE_BYTE | PIPE_WAIT | PIPE_REJECT_REMOTE_CLIENTS, 1, 4096, 4096, 0, NULL);
    if (hReadFromML == INVALID_HANDLE_VALUE)
        return INVALID_HANDLE_VALUE;
    // We want to be able to inherit this handle.
    SECURITY_ATTRIBUTES secure;
    secure.nLength = sizeof(SECURITY_ATTRIBUTES);
    secure.lpSecurityDescriptor = NULL;
    secure.bInheritHandle = TRUE;
    HANDLE hWriteToScreen =
        CreateFile(pipeName, GENERIC_WRITE, 0, &secure, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OVERLAPPED, NULL);
    if (hWriteToScreen == INVALID_HANDLE_VALUE)
        return INVALID_HANDLE_VALUE;

    // Create a thread to manage the output from ML.
    DWORD dwInId;
    HANDLE hInThread = CreateThread(NULL, 0, InThrdProc, 0, 0, &dwInId);
    if (hInThread == NULL)
        return INVALID_HANDLE_VALUE;
    CloseHandle(hInThread);
    wndClass.cbSize = sizeof(wndClass);
    wndClass.style = 0;
    wndClass.lpfnWndProc = WndProc;
    wndClass.cbClsExtra = 0;
    wndClass.cbWndExtra = 0;
    wndClass.hInstance = hApplicationInstance;
    wndClass.hIcon = LoadIcon(hApplicationInstance, MAKEINTRESOURCE(IDI_ICON));
    wndClass.hCursor = NULL; // For the moment 
    wndClass.hbrBackground = NULL; // For the moment
    wndClass.lpszClassName = _T("PolyMLWindowClass");
    wndClass.lpszMenuName = MAKEINTRESOURCE(IDR_MENU);
    wndClass.hIconSm = NULL; // For the moment
    DWORD dwStyle = WS_OVERLAPPEDWINDOW;

    if ((atClass = RegisterClassEx(&wndClass)) == 0)
        return INVALID_HANDLE_VALUE;

    // Initially created invisible.
    hMainWindow = CreateWindow(
        (LPTSTR)(intptr_t)atClass,
        _T("Poly/ML"),
        dwStyle,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        NULL,
        NULL,   // handle to menu or child-window identifier
        hApplicationInstance,
        NULL     // pointer to window-creation data
    );

    if (hMainWindow == NULL)
        return INVALID_HANDLE_VALUE;

    // Save this setting and only apply it when we actually
    // read from or write to the main window.  That way if we are
    // actually using another window this will never get displayed.
    nInitialShow = nCmdShow;
    return hWriteToScreen;
}

WinStream *createConsoleStream()
{
    return new WinGuiConsoleStream();
}

void closeConsole()
{
    if (hInputEvent) CloseHandle(hInputEvent);
}
