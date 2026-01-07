/*
    Title:      Main program

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further development copyright David C.J. Matthews 2001-12, 2015, 2017-19, 2021, 2026

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

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
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

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#if (defined(_WIN32))
#include <tchar.h>
#else
#define _T(x) x
#define _tcslen strlen
#define _tcstol strtol
#define _tcsncmp strncmp
#define _tcschr strchr
#endif

#include "globals.h"
#include "sys.h"
#include "gc.h"
#include "heapsizing.h"
#include "run_time.h"
#include "machine_dep.h"
#include "version.h"
#include "diagnostics.h"
#include "processes.h"
#include "mpoly.h"
#include "scanaddrs.h"
#include "save_vec.h"
#include "../polyexports.h"
#include "memmgr.h"
#include "pexport.h"
#include "polystring.h"
#include "statistics.h"
#include "noreturn.h"

#if (defined(_WIN32))
#include "winstartup.h"
#include "winguiconsole.h"

static const TCHAR *lpszServiceName = 0; // DDE service name
#endif

FILE *polyStdout, *polyStderr; // Redirected in the Windows GUI

NORETURNFN(static void Usage(const char *message, ...));

static PolyObject* InitHeaderFromExport(struct _exportDescription* exports);

struct _userOptions userOptions;

ModuleId exportSignature;

enum {
    OPT_HEAPMIN,
    OPT_HEAPMAX,
    OPT_HEAPINIT,
    OPT_GCPERCENT,
    OPT_RESERVE,
    OPT_GCTHREADS,
    OPT_DEBUGOPTS,
    OPT_DEBUGFILE,
    OPT_DDESERVICE,
    OPT_CODEPAGE,
    OPT_REMOTESTATS,
    OPT_GCSHARING
};

static struct __argtab {
    const TCHAR *argName;
    const char *argHelp;
    unsigned argKey;
} argTable[] =
{
    { _T("-H"),             "Initial heap size (MB)",                               OPT_HEAPINIT },
    { _T("--minheap"),      "Minimum heap size (MB)",                               OPT_HEAPMIN },
    { _T("--maxheap"),      "Maximum heap size (MB)",                               OPT_HEAPMAX },
    { _T("--gcpercent"),    "Target percentage time in GC (1-99)",                  OPT_GCPERCENT },
    { _T("--stackspace"),   "Space to reserve for thread stacks and C++ heap(MB)",  OPT_RESERVE },
    { _T("--gcthreads"),    "Number of threads to use for garbage collection",      OPT_GCTHREADS },
    { _T("--debug"),        "Debug options: checkmem, gc, x",                       OPT_DEBUGOPTS },
    { _T("--logfile"),      "Logging file (default is to log to stdout)",           OPT_DEBUGFILE },
    { _T("--enablegcsharing"), "Allow the garbage collector to run the sharing pass if needed",  OPT_GCSHARING },
#if (defined(_WIN32))
#ifdef UNICODE
    { _T("--codepage"),     "Code-page to use for file-names etc in Windows",       OPT_CODEPAGE },
#endif
    { _T("-pServiceName"),  "DDE service name for remote interrupt in Windows",     OPT_DDESERVICE }
#else
    { _T("--exportstats"),  "Enable another process to read the statistics",        OPT_REMOTESTATS }
#endif
};

static struct __debugOpts {
    const TCHAR *optName;
    const char *optHelp;
    unsigned optKey;
} debugOptTable[] =
{
    { _T("checkmem"),           "Perform additional debugging checks on memory",    DEBUG_CHECK_OBJECTS },
    { _T("gc"),                 "Log summary garbage-collector information",        DEBUG_GC },
    { _T("gcenhanced"),         "Log enhanced garbage-collector information",       DEBUG_GC_ENHANCED },
    { _T("gcdetail"),           "Log detailed garbage-collector information",       DEBUG_GC_DETAIL },
    { _T("memmgr"),             "Memory manager information",                       DEBUG_MEMMGR },
    { _T("threads"),            "Thread related information",                       DEBUG_THREADS },
    { _T("gctasks"),            "Log multi-thread GC information",                  DEBUG_GCTASKS },
    { _T("heapsize"),           "Log heap resizing data",                           DEBUG_HEAPSIZE },
    { _T("x"),                  "Log X-windows information",                        DEBUG_X},
    { _T("sharing"),            "Information from PolyML.shareCommonData",          DEBUG_SHARING},
    { _T("locks"),              "Information about contended locks",                DEBUG_CONTENTION},
    { _T("rts"),                "General run-time system calls",                    DEBUG_RTSCALLS},
    { _T("saving"),             "Saving and loading state; exporting",              DEBUG_SAVING }
};

// Parse a parameter that is meant to be a size.  Returns the value as a number
// of kilobytes.
POLYUNSIGNED parseSize(const TCHAR *p, const TCHAR *arg)
{
    POLYUNSIGNED result = 0;
    if (*p < '0' || *p > '9')
        // There must be at least one digit
        Usage("Incomplete %s option\n", arg);
    while (true)
    {
        result = result*10 + *p++ - '0';
        if (*p == 0)
        {
            // The default is megabytes
            result *= 1024;
            break;
        }
        if (*p == 'G' || *p == 'g')
        {
            result *= 1024 * 1024;
            p++;
            break;
        }
        if (*p == 'M' || *p == 'm')
        {
            result *= 1024;
            p++;
            break;
        }
        if (*p == 'K' || *p == 'k')
        {
            p++;
            break;
        }
        if (*p < '0' || *p > '9')
            break;
    }
    if (*p != 0)
        Usage("Malformed %s option\n", arg);
    // The sizes must not exceed the possible heap size.
#ifdef POLYML32IN64
    if (result > 8 * POLYML32IN64 * 1024 * 1024)
        Usage("Value of %s option must not exceeed %uGbytes\n", arg, 8 * POLYML32IN64);
#elif (SIZEOF_VOIDP == 4)
    if (result > 4 * 1024 * 1024)
        Usage("Value of %s option must not exceeed 4Gbytes\n", arg);
#else
    // For completion only!
    if (result > (POLYUNSIGNED)8 * 1024 * 1024 * 1024 * 1024 * 1024)
        Usage("Value of %s option must not exceeed 8Ebytes\n", arg);
#endif
    return result;
}

/* In the Windows version this is called from WinMain in Console.c */
int polymain(int argc, TCHAR **argv, exportDescription *exports)
{
    POLYUNSIGNED minsize=0, maxsize=0, initsize=0;
    unsigned gcpercent=0;
    bool gcShare = false;
    /* Get arguments. */
    memset(&userOptions, 0, sizeof(userOptions)); /* Reset it */
    userOptions.gcthreads = 0; // Default multi-threaded

    if (polyStdout == 0) polyStdout = stdout;
    if (polyStderr == 0) polyStderr = stderr;

    // Get the program name for CommandLine.name.  This is allowed to be a full path or
    // just the last component so we return whatever the system provides.
    if (argc > 0) 
        userOptions.programName = argv[0];
    else
        userOptions.programName = _T(""); // Set it to a valid empty string
    
    TCHAR *importFileName = 0;
    debugOptions       = 0;

    userOptions.user_arg_count   = 0;
    userOptions.user_arg_strings = (TCHAR**)malloc(argc * sizeof(TCHAR*)); // Enough room for all of them

    // Process the argument list removing those recognised by the RTS and adding the
    // remainder to the user argument list.
    for (int i = 1; i < argc; i++)
    {
        if (argv[i][0] == '-')
        {
            bool argUsed = false;
            for (unsigned j = 0; j < sizeof(argTable)/sizeof(argTable[0]); j++)
            {
                size_t argl = _tcslen(argTable[j].argName);
                if (_tcsncmp(argv[i], argTable[j].argName, argl) == 0)
                {
                    const TCHAR *p = 0;
                    TCHAR *endp = 0;
                    if (argTable[j].argKey != OPT_REMOTESTATS && argTable[j].argKey != OPT_GCSHARING)
                    {
                        if (_tcslen(argv[i]) == argl)
                        { // If it has used all the argument pick the next
                            i++;
                            p = argv[i];
                        }
                        else
                        {
                            p = argv[i]+argl;
                             if (*p == '=') p++; // Skip an equals sign
                        }
                        if (i >= argc)
                            Usage("Incomplete %s option\n", argTable[j].argName);
                    }
                    switch (argTable[j].argKey)
                    {
                    case OPT_HEAPMIN:
                        minsize = parseSize(p, argTable[j].argName);
                        break;
                    case OPT_HEAPMAX:
                        maxsize = parseSize(p, argTable[j].argName);
                        break;
                    case OPT_HEAPINIT:
                        initsize = parseSize(p, argTable[j].argName);
                        break;
                    case OPT_GCPERCENT:
                        gcpercent = _tcstol(p, &endp, 10);
                        if (*endp != '\0') 
                            Usage("Malformed %s option\n", argTable[j].argName);
                        if (gcpercent < 1 || gcpercent > 99)
                        {
                            Usage("%s argument must be between 1 and 99\n", argTable[j].argName);
                            gcpercent = 0;
                        }
                        break;
                    case OPT_RESERVE:
                        {
                            POLYUNSIGNED reserve = parseSize(p, argTable[j].argName);
                            if (reserve != 0)
                                gHeapSizeParameters.SetReservation(reserve);
                            break;
                        }
                    case OPT_GCTHREADS:
                        userOptions.gcthreads = _tcstol(p, &endp, 10);
                        if (*endp != '\0') 
                            Usage("Incomplete %s option\n", argTable[j].argName);
                        break;
                    case OPT_DEBUGOPTS:
                        while (*p != '\0')
                        {
                            // Debug options are separated by commas
                            bool optFound = false;
                            const TCHAR *q = _tcschr(p, ',');
                            if (q == NULL) q = p+_tcslen(p);
                            for (unsigned k = 0; k < sizeof(debugOptTable)/sizeof(debugOptTable[0]); k++)
                            {
                                if (_tcslen(debugOptTable[k].optName) == (size_t)(q-p) &&
                                        _tcsncmp(p, debugOptTable[k].optName, q-p) == 0)
                                {
                                    debugOptions |= debugOptTable[k].optKey;
                                    optFound = true;
                                }
                            }
                            if (! optFound)
                                Usage("Unknown argument to --debug\n");
                            if (*q == ',') p = q+1; else p = q;
                        }
                        if (debugOptions & DEBUG_GC_DETAIL) debugOptions |= DEBUG_GC_ENHANCED;
                        if (debugOptions & DEBUG_GC_ENHANCED) debugOptions |= DEBUG_GC;
                        break;
                    case OPT_DEBUGFILE:
                        SetLogFile(p);
                        break;
#if (defined(_WIN32))
                    case OPT_DDESERVICE:
                        // Set the name for the DDE service.  This allows the caller to specify the
                        // service name to be used to send Interrupt "signals".
                        lpszServiceName = p;
                        break;
#if (defined(UNICODE))
                    case OPT_CODEPAGE:
                        if (! setWindowsCodePage(p))
                            Usage("Unknown argument to --codepage. Use code page number or CP_ACP, CP_UTF8.\n");
                        break;
#endif
#endif
                    case OPT_REMOTESTATS:
                        // If set we export the statistics on Unix.
                        globalStats.exportStats = true;
                        break;

                    case OPT_GCSHARING:
                        // If set allow the GC to run the expensive sharing pass
                        gcShare = true;
                        break;
                    }
                    argUsed = true;
                    break;
                }
            }
            if (! argUsed) // Add it to the user args.
                userOptions.user_arg_strings[userOptions.user_arg_count++] = argv[i];
        }
        else if (exports == 0 && importFileName == 0)
            importFileName = argv[i];
        else
            userOptions.user_arg_strings[userOptions.user_arg_count++] = argv[i];
    }

#ifdef __HAIKU__
    // On Haiku, select() checks whether the first argument is higher
    // than the current process' fd table, and errors out if not; so
    // we make sure it is at least FD_SETSIZE
    struct rlimit lim;

    if (getrlimit(RLIMIT_NOFILE, &lim) < 0)
        Usage("Unable to get file limit: %s\n", strerror(errno));

    if (lim.rlim_cur < FD_SETSIZE)
        lim.rlim_cur = FD_SETSIZE;

    if (setrlimit(RLIMIT_NOFILE, &lim) < 0)
        Usage("Unable to set file limit: %s\n", strerror(errno));
#endif

    if (!gMem.Initialise())
        Usage("Unable to initialise memory allocator\n");

    if (exports == 0 && importFileName == 0)
        Usage("Missing import file name\n");

    // If the maximum is provided it must be not less than the minimum.
    if (maxsize != 0 && maxsize < minsize)
        Usage("Minimum heap size must not be more than maximum size\n");
    // The initial size must be not more than the maximum
    if (maxsize != 0 && maxsize < initsize)
        Usage("Initial heap size must not be more than maximum size\n");
    // The initial size must be not less than the minimum
    if (initsize != 0 && initsize < minsize)
        Usage("Initial heap size must not be less than minimum size\n");

    if (userOptions.gcthreads == 0)
    {
        // If the gcthreads option is missing or zero the default is to try to
        // use as many threads as there are physical processors.  The result may
        // be zero in which case we use the number of processors.  Because memory
        // bandwidth is a limiting factor we want to avoid muliple GC threads on
        // hyperthreaded "processors".
        userOptions.gcthreads = NumberOfPhysicalProcessors();
        if (userOptions.gcthreads == 0)
            userOptions.gcthreads = NumberOfProcessors();
    }

    // Set the heap size if it has been provided otherwise use the default.
    gHeapSizeParameters.SetHeapParameters(minsize, maxsize, initsize, gcpercent, gcShare);

#if (defined(_WIN32))
    SetupDDEHandler(lpszServiceName); // Windows: Start the DDE handler now we processed any service name.
#endif

#ifdef HAVE_PTHREAD_JIT_WRITE_PROTECT_NP
    pthread_jit_write_protect_np(false); // This may well write to exec memory.
#endif

    // Initialise the run-time system before creating the heap.
    InitModules();
    CreateHeap();
    
    PolyObject *rootFunction = 0;

    if (exports != 0)
        rootFunction = InitHeaderFromExport(exports);
    else
    {
        if (importFileName != 0)
            rootFunction = ImportPortable(importFileName);
        if (rootFunction == 0)
            exit(1);
    }

    StartModules();
    
    // Set up the initial process to run the root function.
    processes->BeginRootThread(rootFunction);
    
    finish(0);
    
    /*NOTREACHED*/
    return 0; /* just to keep lint happy */
}

void Uninitialise(void)
// Close down everything and free all resources.  Stop any threads or timers.
{
    StopModules();
}

void finish (int n)
{
    // Make sure we don't get any interrupts once the destructors are
    // applied to globals or statics.
    Uninitialise();
#if (defined(_WIN32))
    ExitThread(n);
#else
    exit (n);
#endif
}

// Print a message and exit if an argument is malformed.
void Usage(const char *message, ...)
{
    va_list vl;
    fprintf(polyStdout, "\n");
    va_start(vl, message);
    vfprintf(polyStdout, message, vl);
    va_end(vl);

    for (unsigned j = 0; j < sizeof(argTable)/sizeof(argTable[0]); j++)
    {
#if (defined(_WIN32) && defined(UNICODE))
        fprintf(polyStdout, "%S <%s>\n", argTable[j].argName, argTable[j].argHelp);
#else
        fprintf(polyStdout, "%s <%s>\n", argTable[j].argName, argTable[j].argHelp);
#endif
    }
    fprintf(polyStdout, "Debug options:\n");
    for (unsigned k = 0; k < sizeof(debugOptTable)/sizeof(debugOptTable[0]); k++)
    {
#if (defined(_WIN32) && defined(UNICODE))
        fprintf(polyStdout, "%S <%s>\n", debugOptTable[k].optName, debugOptTable[k].optHelp);
#else
        fprintf(polyStdout, "%s <%s>\n", debugOptTable[k].optName, debugOptTable[k].optHelp);
#endif
    }
    fflush(polyStdout);
    
#if (defined(_WIN32))
    if (useConsole)
    {
        MessageBox(hMainWindow, _T("Poly/ML has exited"), _T("Poly/ML"), MB_OK);
    }
#endif
    exit (1);
}

// Return a string containing the argument names.  Can be printed out in response
// to a --help argument.  It is up to the ML application to do that since it may well
// want to produce information about any arguments it chooses to process.
char *RTSArgHelp(void)
{
    static char buff[2000];
    char *p = buff;
    for (unsigned j = 0; j < sizeof(argTable)/sizeof(argTable[0]); j++)
    {
#if (defined(_WIN32) && defined(UNICODE))
        int spaces = sprintf(p, "%S <%s>\n", argTable[j].argName, argTable[j].argHelp);
#else
        int spaces = sprintf(p, "%s <%s>\n", argTable[j].argName, argTable[j].argHelp);
#endif
        p += spaces;
    }
    {
        int spaces = sprintf(p, "Debug options:\n");
        p += spaces;
    }
    for (unsigned k = 0; k < sizeof(debugOptTable)/sizeof(debugOptTable[0]); k++)
    {
#if (defined(_WIN32) && defined(UNICODE))
        int spaces = sprintf(p, "%S <%s>\n", debugOptTable[k].optName, debugOptTable[k].optHelp);
#else
        int spaces = sprintf(p, "%s <%s>\n", debugOptTable[k].optName, debugOptTable[k].optHelp);
#endif
        p += spaces;
    }
    ASSERT((unsigned)(p - buff) < (unsigned)sizeof(buff));
    return buff;
}

#ifdef POLYML32IN64
// The operating system places the object code in memory at an arbitrary address.  That's
// fine for native address mode but doesn't work for compact 32-bit mode.  All the segments
// have to be copied into parts of the memory that will work with 32-bit object pointers.
// That requires relocating all the addresses.  This is further complicated because the
// operating system will not have relocated the object pointers from their original
// locations.

typedef struct _SegmentDescr
{
    unsigned segmentIndex;
    size_t segmentSize;
    byte* originalAddress;              // The base address when the segment was created.
    byte* newAddress;
} SegmentDescr;

// LoadRelocate was previously used when loading saved states
// to reduce the need for relocations.

// This class is used to relocate addresses in areas that have been loaded.
class LoadRelocate : public ScanAddress
{
public:
    LoadRelocate() : originalBaseAddr(0), relativeOffset(0) {
    }

    void RelocateObject(PolyObject* p);
    virtual PolyObject* ScanObjectAddress(PolyObject* base) { ASSERT(0); return base; } // Not used
    virtual void ScanConstant(PolyObject* base, byte* addressOfConstant, ScanRelocationKind code, intptr_t displacement);
    void RelocateAddressAt(PolyWord* pt);
    PolyObject* RelocateAddress(PolyObject* obj);

    PolyWord* originalBaseAddr;
    std::vector< SegmentDescr> descrs;

    intptr_t relativeOffset;
};

// Update the addresses in a group of words.
void LoadRelocate::RelocateAddressAt(PolyWord* pt)
{
    PolyWord val = *pt;
    if (!val.IsTagged())
        *gMem.SpaceForAddress(pt)->writeAble(pt) = RelocateAddress(val.AsObjPtr(originalBaseAddr));
}

PolyObject* LoadRelocate::RelocateAddress(PolyObject* obj)
{
    // Which segment is this address in?  N.B. The check is > the start and <= the end
    byte *t = (byte*)obj;
    for (std::vector<SegmentDescr>::iterator descr = descrs.begin(); descr < descrs.end(); descr++)
    {
        if (t > descr->originalAddress && t <= descr->originalAddress + descr->segmentSize)
            return (PolyObject*)(t - descr->originalAddress + descr->newAddress);
    }

    // It should be in one of the segments.
    ASSERT(0);
    return 0;
}

// This is based on Exporter::relocateObject but does the reverse.
// It attempts to adjust all the addresses in the object when it has
// been read in.
void LoadRelocate::RelocateObject(PolyObject* p)
{
    if (p->IsByteObject())
    {
    }
    else if (p->IsCodeObject())
    {
        POLYUNSIGNED constCount;
        PolyWord* cp;
        ASSERT(!p->IsMutable());
        machineDependent->GetConstSegmentForCode(p, cp, constCount);
        /* Now the constant area. */
        for (POLYUNSIGNED i = 0; i < constCount; i++) RelocateAddressAt(&(cp[i]));
        // Saved states and modules have relocation entries for constants in the code.
        // We can't use them when loading object files in 32-in-64 so have to process the
        // constants here.
        machineDependent->ScanConstantsWithinCode(p, this);
        // On 32-in-64 ARM we may have to update the global heap base in an FFI callback.
        machineDependent->UpdateGlobalHeapReference(p);
    }
    else if (p->IsClosureObject())
    {
        // The first word is the address of the code.
        POLYUNSIGNED length = p->Length();
        *(PolyObject**)p = RelocateAddress(*(PolyObject**)p);
        for (POLYUNSIGNED i = sizeof(PolyObject*) / sizeof(PolyWord); i < length; i++)
            RelocateAddressAt(p->Offset(i));
    }
    else /* Ordinary objects, essentially tuples. */
    {
        POLYUNSIGNED length = p->Length();
        for (POLYUNSIGNED i = 0; i < length; i++) RelocateAddressAt(p->Offset(i));
    }
}

// Update addresses as constants within the code.
void LoadRelocate::ScanConstant(PolyObject* base, byte* addressOfConstant, ScanRelocationKind code, intptr_t displacement)
{
    PolyObject* p = GetConstantValue(addressOfConstant, code, displacement);

    if (p != 0)
    {
        // Relative addresses are computed by adding the CURRENT address.
        // We have to convert them into addresses in original space before we
        // can relocate them.
        if (code == PROCESS_RELOC_I386RELATIVE)
            p = (PolyObject*)((PolyWord*)p + relativeOffset);
        PolyObject* newValue = RelocateAddress(p);
        SetConstantValue(addressOfConstant, newValue, code);
    }
}
#endif

PolyObject* InitHeaderFromExport(struct _exportDescription* exports)
{
    // Check the structure sizes stored in the export structure match the versions
    // used in this library.
    if (exports->structLength != sizeof(exportDescription) ||
        exports->memTableSize != sizeof(memoryTableEntry) ||
        exports->rtsVersion < FIRST_supported_version ||
        exports->rtsVersion > LAST_supported_version)
    {
#if (FIRST_supported_version == LAST_supported_version)
        Exit("The exported object file has version %0.2f but this library supports %0.2f",
            ((float)exports->rtsVersion) / 100.0,
            ((float)FIRST_supported_version) / 100.0);
#else
        Exit("The exported object file has version %0.2f but this library supports %0.2f-%0.2f",
            ((float)exports->rtsVersion) / 100.0,
            ((float)FIRST_supported_version) / 100.0,
            ((float)LAST_supported_version) / 100.0);
#endif
    }
    // We could also check the RTS version and the architecture.
    exportSignature = exports->execIdentifier; // Needed for load and save.

    memoryTableEntry* memTable = exports->memTable;
#ifdef POLYML32IN64
    // We need to copy this into the heap before beginning execution.
    LoadRelocate relocate;
    relocate.descrs.reserve(exports->memTableEntries);
    relocate.originalBaseAddr = (PolyWord*)exports->originalBaseAddr;

    PolyObject* root = 0;

    for (unsigned i = 0; i < exports->memTableEntries; i++)
    {
        PermanentMemSpace* newSpace =
            gMem.AllocateNewPermanentSpace(memTable[i].mtLength, (unsigned)memTable[i].mtFlags, i, exportSignature);
        if (newSpace == 0)
            Exit("Unable to initialise a permanent memory space");

        PolyWord* mem = newSpace->bottom;
        SegmentDescr descr;
        descr.segmentIndex = i;
        descr.originalAddress = (byte*)memTable[i].mtOriginalAddr;
        descr.segmentSize = memTable[i].mtLength;
        descr.newAddress = (byte*)mem;
        relocate.descrs.push_back(descr);

        memcpy(newSpace->writeAble(mem), memTable[i].mtCurrentAddr, memTable[i].mtLength);
        PolyWord* unused = mem + memTable[i].mtLength / sizeof(PolyWord);
        gMem.FillUnusedSpace(newSpace->writeAble(unused),
            newSpace->spaceSize() - memTable[i].mtLength / sizeof(PolyWord));

        if (newSpace == 0)
            Exit("Unable to initialise a permanent memory space");

        // Relocate the root function.
        if (exports->rootFunction >= memTable[i].mtCurrentAddr && exports->rootFunction < (char*)memTable[i].mtCurrentAddr + memTable[i].mtLength)
        {
            root = (PolyObject*)((char*)mem + ((char*)exports->rootFunction - (char*)memTable[i].mtCurrentAddr));
        }
    }

    // Now relocate the addresses
    for (unsigned j = 0; j < exports->memTableEntries; j++)
    {
        SegmentDescr* descr = &relocate.descrs[j];
        MemSpace* space = gMem.SpaceForIndex(descr->segmentIndex, exportSignature);
        // Any relative addresses have to be corrected by adding this.
        relocate.relativeOffset = (PolyWord*)descr->originalAddress - space->bottom;
        for (PolyWord* p = space->bottom; p < space->top; )
        {
            if ( ((p-(PolyWord*)0) & (POLYML32IN64 - 1)) != POLYML32IN64 - 1)
            {
                // Skip any padding.  The length word should be on the correct boundary.
                p++;
                continue;
            }
            p++;
            PolyObject* obj = (PolyObject*)p;
            POLYUNSIGNED length = obj->Length();
            relocate.RelocateObject(obj);
            p += length;
        }
    }

    // Set the final permissions.
    for (unsigned j = 0; j < exports->memTableEntries; j++)
    {
        PermanentMemSpace* space = gMem.SpaceForIndex(j, exportSignature);
        gMem.CompletePermanentSpaceAllocation(space);
    }

    return root;

#else
    for (unsigned i = 0; i < exports->memTableEntries; i++)
    {
        // Construct a new space for each of the entries.
        if (gMem.PermanentSpaceFromExecutable(
            (PolyWord*)memTable[i].mtCurrentAddr,
            memTable[i].mtLength / sizeof(PolyWord), (unsigned)memTable[i].mtFlags,
            i, exportSignature) == 0)
            Exit("Unable to initialise a permanent memory space");
    }
    return (PolyObject*)exports->rootFunction;
#endif
}
