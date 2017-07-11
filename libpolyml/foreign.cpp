/*
    Title:      Foreign function interface
    Author:     Nick Chapman

    Copyright (c) 2000-7
        Cambridge University Technical Services Limited
    Further development Copyright David C.J. Matthews 2008-2015.

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

#if (defined(_WIN32) || (defined(HAVE_DLOPEN)))
// Then we can use the foreign function interface.

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#if HAVE_ALLOCA_H
#include <alloca.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h> /* sleep */
#endif
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#else
// Earlier versions of Mac OS X had dlopen but no /usr/include/dlfcn.h
extern "C" {
     void *dlopen(const char *, int);
     int dlclose(void *);
     void *dlsym(void *, const char *);
     const char *dlerror(void);
}
#endif

#if (defined(_WIN32) && ! defined(__CYGWIN__))
#include <windows.h>
#include "Console.h" /* For hApplicationInstance. */
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#if (defined(_WIN32) && ! defined(__CYGWIN__))
#include <tchar.h>
#else
typedef char TCHAR;
#define _T(x) x
#endif

#include "globals.h"
// We need to include globals.h before <new> in mingw64 otherwise
// it messes up POLYUFMT/POLYSFMT.

#include <ffi.h>
#include <new>

#include "arb.h"
#include "reals.h"
#include "foreign.h"
#include "diagnostics.h"
#include "run_time.h"
#include "sys.h"
#include "machine_dep.h"
#include "mpoly.h"
#include "processes.h"
#include "scanaddrs.h"
#include "polystring.h"
#include "save_vec.h"
#include "rts_module.h"
#include "locking.h"
#include "rtsentry.h"

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyForeignGeneral(PolyObject *threadId, PolyWord code, PolyWord arg);
}

/**********************************************************************
 *
 *  Stuff...
 *   
 **********************************************************************/
    
#define DLOPENFLAGS 1

#define Head(p) (((ML_Cons_Cell*)(p).AsObjPtr())->h)
#define Tail(p) (((ML_Cons_Cell*)(p).AsObjPtr())->t)

#define UNHANDLE(h)     DEREFWORDHANDLE(h)
#define SAVE(x)         taskData->saveVec.push(x)


/**********************************************************************
 *
 *  Foreign Debug
 *   
 **********************************************************************/


static int foreign_debug = 0;

/* Debug levels:
   0 - (no-debug)
   1 - (debug)    Bug specific tracing.
   2 - (info)     Important info, e.g. the actual calls to foreign functions
   3 - (trace)    Trace every c-function
   4 - (mes)      Any old rubbish
   */

/*
   Level 1 can be used to track different bugs when they appear.
   For example, used it to watch Volatile GC when
   tracking `Invalid volatile' bug.
*/   


/***
 In each of the following macros, ARGS must be a bracketed arg list
 suitable for printf. This means usage will look like the following:
 (Note the double open/close brackets.)
 
       info(("example message with a parameter <%d>\n",parameter));
***/       

/* Give macro `__FUNCTION__' a defintion if we are not using GCC */
#if ! defined(__GNUC__) && !defined(__FUNCTION__)
#define __FUNCTION__        __FILE__ 
#endif

#define mesN(N,ARGS) \
    { \
      if (foreign_debug>=(N)) \
      { printf("%s:%4i (%s) ", __FILE__, __LINE__, __FUNCTION__); \
        printf ARGS; \
      } \
    }

#define show(ARGS) mesN(0,ARGS)
#define mes1(ARGS) mesN(1,ARGS)
#define mes2(ARGS) mesN(2,ARGS)
#define mes3(ARGS) mesN(3,ARGS)
#define mes4(ARGS) mesN(4,ARGS)


#define debug   mes1
#define info    mes2
#define trace   mes3
#define TRACE   mes3(("\n"))
#define mes     mes4


static Handle get_foreign_debug (TaskData *taskData, Handle ignored)
{
  TRACE;
  return Make_arbitrary_precision(taskData, foreign_debug);
}

static Handle set_foreign_debug (TaskData *taskData, Handle h)
{
  TRACE;
  foreign_debug = get_C_int(taskData, DEREFWORD(h));
  return h;
}
 

#define RAISE_EXN(string) raise_exception_string(taskData, EXC_foreign,(string))


typedef enum {
  Cchar    = 1,
  Cdouble  = 2,
  Cfloat   = 3,
  Cint     = 4,
  Clong    = 5,
  Cpointer = 6,
  Cshort   = 7,
  Cuint    = 8
} Ctype;


static const char *stringOfCtype(Ctype c)
{
  switch (c) {
  case Cchar    : return "Cchar";
  case Cdouble  : return "Cdouble";
  case Cfloat   : return "Cfloat";
  case Cint     : return "Cint";
  case Clong    : return "Clong";
  case Cpointer : return "Cpointer";
  case Cshort   : return "Cshort";
  case Cuint    : return "Cuint";
  default       : {
    static char buf[100];
    sprintf(buf,"Bad Ctype <%d>", c);
    return buf;
  }
  }
}


/**********************************************************************
 *
 * Volatiles ...
 * 
 **********************************************************************/

#define INITIAL_NUM_VOLS 200

class PolyVolData;

typedef struct {
    PolyVolData* ML_pointer;    // Pointer to ML token object.
    void* C_pointer;            // Pointer to C storage.
    bool Own_C_space;           // True if this is the owner of storage.
    void (*C_finaliser)(void*); // Pointer to finalisation function.
} Volatile;


static Volatile *vols;
static PLock volLock; // Mutex to protect vols.
// TODO: There is a theoretical risk of deadlock if any ML allocation is made while this
// lock is held.  An allocation can result in a GC which requires all threads to release
// ML memory but another thread could block waiting for the mutex.
// N.B. raising an exception involves an allocation.

// Index zero is special - this is always null.
#define VOL_NULL_INDEX 0
#define FIRST_VOL 1

static POLYUNSIGNED num_vols = 1;
static POLYUNSIGNED next_vol = FIRST_VOL;


/* This table contains all the callback functions that have been created.  Once a callback
   has been set up it remains in existence for the rest of the session. */
static struct _cbStructEntry {
    PolyWord    mlFunction;        /* The corresponding ML function. */
    PolyWord    argType;           /* The argument type information. */
    unsigned char *cFunction;       /* The C function "stub" code. */
} *callbackTable;
static unsigned callBackEntries = 0;
static PLock callbackTableLock; // Mutex to protect table.

/**********************************************************************
 *
 *  Volatile Allocation
 *   
 **********************************************************************/

/* With magic numbers */

// This is the ML object that represents a "vol"
class PolyVolData: public PolyObject {
public:
    POLYUNSIGNED    volmagic;
    POLYUNSIGNED    volindex;
};

#define VOL_BOX_SIZE            (sizeof(PolyVolData)/sizeof(PolyWord))
#define V_INDEX(v)              ((v))->volindex
#define V_MAGIC(v)              ((v))->volmagic
#define VOL_MAGIC_NUMBER        65169
#define MakeVolMagic(v)         V_MAGIC((v)) = VOL_MAGIC_NUMBER
#define IsVolMagic(v)           (V_MAGIC((v)) == VOL_MAGIC_NUMBER)

// Access to the vols table.  volLock must be held before any of these are
// called because another thread could realloc it.
#define ML_POINTER(v)           (vols[V_INDEX(v)].ML_pointer)
#define C_POINTER(v)            (vols[V_INDEX(v)].C_pointer)
#define OWN_C_SPACE(v)          (vols[V_INDEX(v)].Own_C_space)
#define FINALISER(v)            (vols[V_INDEX(v)].C_finaliser)

#define UNVOLHANDLE(_x)          ((PolyVolData*)DEREFHANDLE(_x))


/* Allocate a new "vol" in the table and in ML space and returns a pointer to the ML "vol". */
static Handle vol_alloc (TaskData *taskData)
{
    PolyVolData* v = (PolyVolData*)alloc(taskData, VOL_BOX_SIZE, F_MUTABLE_BIT|F_BYTE_OBJ);
    Handle result = SAVE(v);
    
    trace(("index=<%" POLYUFMT ">\n",next_vol));
    if (next_vol >= num_vols)
    {
        POLYUNSIGNED new_num_vols = (num_vols==0) ? INITIAL_NUM_VOLS : num_vols*2;
        info(("<%" POLYUFMT "> ---> <%" POLYUFMT ">\n", num_vols, new_num_vols));
        Volatile *new_vols = (Volatile*)realloc(vols, sizeof(Volatile)*new_num_vols);
        if (new_vols == NULL)
            RAISE_EXN("Can't Enlarge Volatile Array");
        vols = new_vols;
        num_vols = new_num_vols;
    }
    V_INDEX(v) = next_vol++;
    MakeVolMagic(v);
    ML_POINTER(v) = v;
    C_POINTER(v) = NULL;
    OWN_C_SPACE(v) = false; /* Does not own it. */
    FINALISER(v) = 0; /* None installed yet. */
    
    return result;
}

/* Allocate a new "vol" in the table which points to a C object of size "size". */
static Handle vol_alloc_with_c_space (TaskData *taskData, POLYUNSIGNED size)
{
    PLocker plocker(&volLock);
    Handle res = vol_alloc(taskData);
    trace(("size= %" POLYUFMT "\n",size));
    void *p = malloc(size);
    if (p == 0)
        RAISE_EXN("Insufficient memory");
    C_POINTER(UNVOLHANDLE(res)) = p;
    OWN_C_SPACE(UNVOLHANDLE(res)) = true;

    return res;
}


/**********************************************************************
 *
 *  Volatile Dereferencing --- A safe version of C_POINTER
 *   
 **********************************************************************/

/* Returns the C-pointer component corresponding to the "vol" argument. */
static void* DEREFVOL (TaskData *taskData, PolyWord v)
{ TRACE; {
    PLocker plocker(&volLock);
    PolyVolData *vol = (PolyVolData*)v.AsObjPtr();
    
    POLYUNSIGNED index = V_INDEX(vol);
    trace(("<%" POLYUFMT ">\n",index));
    
    if (!(IsVolMagic(vol))) {
        info (("Invalid volatile -- bad magic number, index=<%" POLYUFMT ">\n", index));
        RAISE_EXN("Bad volatile magic number");
    }

    // The Null vol is persistent and doesn't have an ML pointer
    if (index == VOL_NULL_INDEX) return vols[index].C_pointer;
    
    if (index < num_vols) {
        if (vols[index].ML_pointer == v.AsObjPtr()) {
            /* everything is okay */
            return vols[index].C_pointer;
            
        } else {
            info(("Invalid volatile -- backpointer is wrong <%" POLYUFMT ">: <%p> != <%p>\n",
                index, vol, (void *)vols[index].ML_pointer));
        }
    } else {
        info(("Invalid volatile -- no such vol index <%" POLYUFMT ">\n", index));
    }
    RAISE_EXN("Invalid volatile");
    /*NOTREACHED*/
    return 0;
}}

/**********************************************************************
 *
 *  Volatile Sanity Checking (debug only)
 *   
 **********************************************************************/
#if 0
static void sanity_check_vols(void)
{ TRACE; {
  POLYUNSIGNED v, v2;
  for (v=FIRST_VOL; v < next_vol; v++) {
    if (vols[v].ML_pointer == NULL) {
      trace(("vol <%lu> has nulled ML_pointer\n",v));
    } else {
      v2 = V_INDEX(vols[v].ML_pointer);
      if ( v2 != v ) {
    info(("vol <%d> has been corrupted to <%d>\n",v,v2));
      }
    }
  }
}}
#endif

/**********************************************************************
 *
 *  C Programming Primitives
 *   
 **********************************************************************/

/* This is roughly equivalent to calling malloc via the FFI, except that the vol it
   returns refers to the newly allocated store.  Typically we need a pointer to the
   store so we need to call address(alloc s). */
static Handle allocate (TaskData *taskData, Handle h)
{ TRACE; {
    POLYUNSIGNED size = getPolyUnsigned(taskData, DEREFWORD(h)); /* bytes */
    Handle space = vol_alloc_with_c_space(taskData, size);
    return space;
}}

/* Constructs a one-word C object whose value is the C-pointer of the argument. */
static Handle address (TaskData *taskData, Handle h)
{ TRACE; {
    Handle res = vol_alloc_with_c_space(taskData, sizeof(void*));
    void *addr = DEREFVOL(taskData, UNVOLHANDLE(h));
    PLocker plocker(&volLock);
    *(void**)C_POINTER(UNVOLHANDLE(res)) = addr;
    return res;
}}

/* Returns a vol containing the value at the address given in its argument. */
static Handle deref (TaskData *taskData, Handle h)
{ TRACE; {
    void *addr = DEREFVOL(taskData, UNVOLHANDLE(h));
    PLocker plocker(&volLock);
    Handle res = vol_alloc(taskData);
    C_POINTER(UNVOLHANDLE(res))= *(void**)addr;
    return res;
}}


/******
 * offset is intended for accessing structure components.
 * pointer_add can be written in terms offset,address & deref:
 *   pointer_add p n == address (offset (deref p) n)
 ******/
/* This seems odd to me (DCJM) but it makes sense when you realise that a pointer is
   a vol whose C-pointer value contains the address of the vol containing the storage.
   There's always one more level of indirection than you think.  DCJM 12/4/04. */
static Handle offset (TaskData *taskData, Handle h)
{ TRACE; {
    PolyWord structure  = UNHANDLE(h)->Get(0);
    char *addr = (char*)DEREFVOL(taskData, structure);
    PLocker plocker(&volLock);
    Handle res = vol_alloc(taskData);
    POLYSIGNED num_bytes = get_C_long(taskData, DEREFWORDHANDLE(h)->Get(1));
    
    C_POINTER(UNVOLHANDLE(res)) = addr + num_bytes;
    return res;
}}
  

static Handle assign (TaskData *taskData, Handle h)
{ TRACE; {
    
    PolyVolData *left  = (PolyVolData *)(UNHANDLE(h)->Get(0).AsObjPtr());
    PolyVolData *right = (PolyVolData *)(UNHANDLE(h)->Get(1).AsObjPtr());
    POLYSIGNED size = get_C_long(taskData, DEREFWORDHANDLE(h)->Get(2)); /* bytes */
    void *source = DEREFVOL(taskData, right);
    PLocker plocker(&volLock);
    void *dest = C_POINTER(left);
    memcpy(dest, source, size);

    return SAVE(TAGGED(0));
}}


static Handle c_sizeof (TaskData *taskData, Handle h)
{
    TRACE;
    PolyWord v = UNHANDLE(h);
    
    if (!(IS_INT(v))) // This should be handled within ML
        RAISE_EXN("sizeof for struct");

    Ctype ctype = (Ctype)UNTAGGED(v);
    trace(("<%s>\n", stringOfCtype(ctype)));
    switch (ctype) {
    case Cchar    : return Make_arbitrary_precision(taskData, sizeof(char));
    case Cdouble  : return Make_arbitrary_precision(taskData, sizeof(double));
    case Cfloat   : return Make_arbitrary_precision(taskData, sizeof(float));
    case Cint     : return Make_arbitrary_precision(taskData, sizeof(int));
    case Clong    : return Make_arbitrary_precision(taskData, sizeof(long));
    case Cpointer : return Make_arbitrary_precision(taskData, sizeof(void*));
    case Cshort   : return Make_arbitrary_precision(taskData, sizeof(short));
    case Cuint    : return Make_arbitrary_precision(taskData, sizeof(unsigned));
    default: {
        char buf[100];
        sprintf(buf, "Unknown ctype <%s>", stringOfCtype(ctype));
        RAISE_EXN(buf);
        /*NOTREACHED*/
        /* Keep -Wall happy */ return (Handle)0;
        }
    }
}


static Handle alignment (TaskData *taskData, Handle h)
{  
    TRACE;
    PolyWord v = UNHANDLE(h);
    
    if (!(IS_INT(v)))
        RAISE_EXN("alignment of structure"); 

    Ctype ctype = (Ctype)UNTAGGED(v);
    trace(("<%s>\n", stringOfCtype(ctype)));
    switch (ctype) {
#ifdef __GNUC__ 
    case Cchar    : return Make_arbitrary_precision(taskData, __alignof__(char));
    case Cdouble  : return Make_arbitrary_precision(taskData, __alignof__(double));
    case Cfloat   : return Make_arbitrary_precision(taskData, __alignof__(float));
    case Cint     : return Make_arbitrary_precision(taskData, __alignof__(int));
    case Clong    : return Make_arbitrary_precision(taskData, __alignof__(long));
    case Cpointer : return Make_arbitrary_precision(taskData, __alignof__(void*));
    case Cshort   : return Make_arbitrary_precision(taskData, __alignof__(short));
    case Cuint    : return Make_arbitrary_precision(taskData, __alignof__(unsigned));
#else
        /* Take a guess... */
        /* Use "sizeof" here.  DCJM 19/4/01. */
    case Cchar    : return Make_arbitrary_precision(taskData, sizeof(char));
    case Cdouble  : return Make_arbitrary_precision(taskData, sizeof(double));
    case Cfloat   : return Make_arbitrary_precision(taskData, sizeof(float));
    case Cint     : return Make_arbitrary_precision(taskData, sizeof(int));
    case Clong    : return Make_arbitrary_precision(taskData, sizeof(long));
    case Cpointer : return Make_arbitrary_precision(taskData, sizeof(void*));
    case Cshort   : return Make_arbitrary_precision(taskData, sizeof(short));
    case Cuint    : return Make_arbitrary_precision(taskData, sizeof(unsigned));
#endif  
    default: {
        char buf[100];
        sprintf(buf, "Unknown ctype <%s>", stringOfCtype(ctype));
        RAISE_EXN(buf);
        }
    }
    /*NOTREACHED*/
    /* Keep -Wall happy */ return (Handle)0;
}


/**********************************************************************
 *
 *  Volatile Garbage Collection
 *   
 **********************************************************************

During GC this function is called three times, with "op" taking the values
    1 -- MarkRuntimeObject
    2 -- ReferenceRuntimeObject
    3 -- UpdateRuntimeObject

Pass 1 is irrelevant. We call "op" (MarkRuntimeObject) with the second
parameter equal to 1.  This indicates the ML pointers held by the
array "vols" are weak references and so "op" does nothing and returns
immediately.

During Pass 2, the condition "(vols[from].ML_pointer != NULL)" is
always true, so we call "op" (ReferenceRuntimeObject) with the address
of each ML pointer cell contained in the array "vols".  Any cell that
holds a heap pointer that is not marked is overwritten by "op" to be
NULL. After the call to "op" we test if the cell has been nulled, if
so we deallocate the malloced space pointed to by the corresponding C
pointer.

Also, because the condition "(vols[from].ML_pointer != NULL)" is true
on every iteration of the loop, the counter "to" always keeps in step
with the loop counter "from". This prevents us from entering the code
guarded by the condition "(from>to)" (intended for pass 3).

During the Pass 3, we only call "op" (UpdateRuntimeObject) with the
address of those ML pointers that were not nulled on the previous
stage. The ML pointers are overwritten by "op" with their new location
in the heap.

Also, on this pass we compact the array "vols". The "to" counter is
only incremented when we encounter an ML pointer that is not NULL,
whereas the "from" counter is incremented on every iteration.  If
"from" > "to" we shift the pair of pointers (ML and C) leftwards in the
array from index "from" to index "to" . This requires us to update the
index held by the ML heap cell to be "to".

Finally we set "next_vol" to be "to", the index of the next volatile
to be allocated.  Although this occurs on every pass, it only has
effect on Pass 3.

**********************************************************************/

struct _entrypts foreignEPT[] =
{
    { "PolyForeignGeneral",             (polyRTSFunction)&PolyForeignGeneral},

    { NULL, NULL} // End of list.
};

class Foreign: public RtsModule
{
public:
    virtual void Init(void);
    virtual void Start(void);
    virtual void Stop(void);
    void GarbageCollect(ScanAddress *process);
};

// Declare this.  It will be automatically added to the table.
static Foreign foreignModule;


void Foreign::Init()
{
    vols = (Volatile*)malloc(sizeof(Volatile)*INITIAL_NUM_VOLS);
    ASSERT(vols != 0); // If this fails we won't be able to allocate the NULL.
    num_vols = INITIAL_NUM_VOLS;
}

void Foreign::Start()
{
    // Set up the NULL entry
    static void *nullValue = 0;
    vols[VOL_NULL_INDEX].ML_pointer = 0;
    vols[VOL_NULL_INDEX].C_pointer = &nullValue;
    vols[VOL_NULL_INDEX].Own_C_space = false; // Not freed
    vols[VOL_NULL_INDEX].C_finaliser = 0; // No finaliser
}

void Foreign::Stop()
{
    // Call finalisers that have not otherwise been called.
    for (POLYUNSIGNED k = FIRST_VOL; k < next_vol; k++)
    {
        if (vols[k].ML_pointer != NULL && vols[k].C_pointer != 0 && vols[k].C_finaliser)
            vols[k].C_finaliser(*(void**)vols[k].C_pointer);
    }
}

void Foreign::GarbageCollect(ScanAddress *process)
{ TRACE;
    // First pass: GC and relocate the ML pointers and detect unreferenced
    // entries.  Call any finalisers on them.  We have to do that before freeing
    // any "own" spaces because we may have set a finaliser on the contents of
    // something that is owned by an earlier vol (i.e. 2 vols have the same
    // C_pointer value).  The higher levels will ensure that we won't GC the
    // original value until the copy is also GCd.
    for (POLYUNSIGNED k = FIRST_VOL; k < next_vol; k++)
    {
        if (vols[k].ML_pointer != NULL)
        {
            PolyObject *p = vols[k].ML_pointer;
            process->ScanRuntimeAddress(&p, ScanAddress::STRENGTH_WEAK);
            vols[k].ML_pointer = (PolyVolData*)p;

            if (vols[k].ML_pointer == NULL && vols[k].C_finaliser)
            {
                trace(("Calling finaliser on <%" POLYUFMT ">\n", k));
                vols[k].C_finaliser(*(void**)vols[k].C_pointer);
            }
        }
    }

    // Then compact the table and free any "own" references.
    POLYUNSIGNED to,from;
    for (from=FIRST_VOL, to=FIRST_VOL; from < next_vol; from++)
    {
        mes(("to=<%" POLYUFMT "> from=<%" POLYUFMT ">\n",to,from));
        
        if (vols[from].ML_pointer == NULL)
        {
            if (vols[from].Own_C_space)
            {
                // Can now free this.            
                trace(("Freeing malloc space of <%" POLYUFMT ">\n",from));
                free(vols[from].C_pointer);
                vols[from].C_pointer = 0;
                vols[from].Own_C_space = false;
            }
        }
        else
        {
            if (from>to)
            {
                trace(("Shifting volatile <%" POLYUFMT "> ---> <%" POLYUFMT ">\n",from,to));
                vols[to] = vols[from];
                V_INDEX(vols[to].ML_pointer) = to;
            }
            to++;
        }
    }
    next_vol = to;
    
    /* Callback table.  Added DCJM 12/4/04.  We always process these as strong references.
    For the time being at any rate we treat these as permanent entries so that once a
    callback is set up it cannot be garbage-collected. */
    for (unsigned i = 0; i < callBackEntries; i++)
    {
        process->ScanRuntimeWord(&callbackTable[i].mlFunction);
        process->ScanRuntimeWord(&callbackTable[i].argType);
    }
}


/**********************************************************************
 *
 *  Load a Dynamic Library.
 *   
 **********************************************************************/

static Handle load_lib (TaskData *taskData, Handle string)
{
    TCHAR name[500];
    
    Poly_string_to_C(DEREFWORD(string), name, sizeof(name)/sizeof(TCHAR));

#if (defined(_WIN32) && defined(UNICODE))
    info(("<%S>\n", name));
#else
    info(("<%s>\n", name));
#endif
    // If the name is the null string use the current executable.
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    HINSTANCE lib;
    if (*name == 0) lib = hApplicationInstance;
    else lib = LoadLibrary(name);
    if (lib == NULL) 
    {
        char buf[256];
#if (defined(_WIN32) && defined(UNICODE))
        sprintf(buf, "load_lib <%S> : %lu", name, GetLastError());
#else
        sprintf(buf, "load_lib <%s> : %lu", name, GetLastError());
#endif
        RAISE_EXN(buf);
    }

    Handle res = vol_alloc_with_c_space(taskData, sizeof(void*));
    *(void**)DEREFVOL(taskData, UNHANDLE(res)) = lib;
    return res;

#else  /* UNIX version */
    void *lib = dlopen(*name == 0 ? NULL : name,DLOPENFLAGS);
    if (!lib)
    {
        char buf[256];
        sprintf(buf, "load_lib <%s> : %s", name, dlerror());
        RAISE_EXN(buf);
    }
    
    Handle res = vol_alloc_with_c_space(taskData, sizeof(void*));
    *(void**)DEREFVOL(taskData, UNHANDLE(res)) = lib;
    return res;
#endif
}


/**********************************************************************
 *
 *  Load Symbol from a Dynamic Library
 *   
 **********************************************************************/

static Handle load_sym (TaskData *taskData, Handle h)
{
    char name[500];
    
    Poly_string_to_C(DEREFHANDLE(h)->Get(1), name, sizeof(name));
    info(("<%s>\n", name));
    
#if (defined(_WIN32) && ! defined(__CYGWIN__))
    void *sym = (void*)GetProcAddress( *(HINSTANCE*)DEREFVOL(taskData, DEREFHANDLE(h)->Get(0)), name);
    
    if (sym == NULL) 
    {
        char buf[256];
        sprintf(buf, "load_sym <%s> : %lu", name, GetLastError());
        RAISE_EXN(buf);
    }

    Handle res = vol_alloc_with_c_space(taskData, sizeof(void*));
    *(void**)DEREFVOL(taskData, UNHANDLE(res)) = sym;
    return res;

#else /* UNIX version */
    void *sym = dlsym( *(void**)DEREFVOL(taskData, DEREFHANDLE(h)->Get(0)), name );
    
    if (!sym)
    {
        char buf[256];
        sprintf(buf, "load_sym <%s> : %s", name, dlerror());
        RAISE_EXN(buf);
    }

    Handle res = vol_alloc_with_c_space(taskData, sizeof(void*));
    *(void**)DEREFVOL(taskData, UNHANDLE(res)) = sym;
    return res;
#endif
}


static unsigned length_list (PolyWord p)
{
  TRACE; {
      return ML_Cons_Cell::IsNull(p) ? 0 : 1 + length_list (Tail(p));
  }
}

static ffi_type *ctypeToFfiType(TaskData *taskData, PolyWord conv)
{
    if (IS_INT(conv))
    {
        Ctype ctype = (Ctype)UNTAGGED(conv);
        info(("<%s>\n", stringOfCtype(ctype)));
        switch (ctype) {
            case Cchar: return &ffi_type_schar;
            case Cdouble: return &ffi_type_double;
            case Cfloat: return &ffi_type_float;
            case Cint: return &ffi_type_sint;
            case Clong: return &ffi_type_slong;
            case Cpointer: return &ffi_type_pointer;
            case Cshort: return &ffi_type_sshort;
            case Cuint: return &ffi_type_uint;
        }
        RAISE_EXN("Unknown ctype");
    }
    else
    {
        // Structure: this is a vector of ctypes.  This must be allocated dynamically.
        PolyObject *vec = conv.AsObjPtr();
        POLYUNSIGNED length = vec->Length();
        ffi_type **str = (ffi_type **)malloc((length+1)*sizeof(ffi_type *));
        if (str == NULL)
            RAISE_EXN("Insufficient memory");
        for (POLYUNSIGNED i = 0; i < length; i++)
            str[i] = ctypeToFfiType(taskData, vec->Get(i));
        str[length] = 0;
        ffi_type *result = (ffi_type *)malloc(sizeof(ffi_type));
        if (result == NULL)
            RAISE_EXN("Insufficient memory");
        result->size = 0;
        result->alignment = 0;
        result->type = FFI_TYPE_STRUCT;
        result->elements = str;
        return result;
    }
    return 0; // Suppress warning
}

// Free the structure elements of the type vectors.
static void freeTypeVec(ffi_type **vec, unsigned elements)
{
    for (unsigned i = 0; i < elements; i++)
    {
        ffi_type *t = vec[i];
        if (t->elements != 0)
        {
            unsigned elems = 0;
            while (t->elements[elems] != 0) elems++;
            freeTypeVec(t->elements, elems);
            free(t->elements);
        }
    }
}

typedef void (*ftype)(void);

static Handle call_sym (TaskData *taskData, Handle symH, Handle argsH, Handle retCtypeH)
{
    TRACE;
    ftype sym               = *(ftype*)DEREFVOL(taskData, symH->Word());
    PolyWord arg_list       = argsH->Word();
    unsigned num_args   = length_list(arg_list);
    ffi_cif cif;

    // Initialise the error vars to "no error".  If we have multiple worker
    // threads the previous value will depend on whatever that worker
    // did last and not necessarily on what this ML thread did.
#ifdef HAVE_ERRNO_H
    errno = 0;
#endif
#ifdef _WIN32
    SetLastError(0);
#endif

    ffi_type **arg_types = (ffi_type**)alloca(num_args * sizeof(ffi_type*));
    void **arg_values = (void**)alloca(num_args * sizeof(void*));
    
    // The argument list is a list of pairs.
    PolyWord p = arg_list;
    for (POLYUNSIGNED i=0; i<num_args; i++,p=Tail(p))
    {
        arg_values[i] = DEREFVOL(taskData, Head(p).AsObjPtr()->Get(1));
        arg_types[i] = ctypeToFfiType(taskData, Head(p).AsObjPtr()->Get(0));
    }

    ffi_type *result_type = ctypeToFfiType(taskData, retCtypeH->Word());

#if(defined(_WIN32) && ! defined(__GNUC__) && ! defined(_WIN64))
    const ffi_abi abi = FFI_STDCALL;
#else
    const ffi_abi abi = FFI_DEFAULT_ABI;
#endif

    if (ffi_prep_cif(&cif, abi, num_args, result_type, arg_types) != FFI_OK)
        RAISE_EXN("libffi error: ffi_prep_cif failed");

    // malloc memory for the result
    void *result = malloc(result_type->size);
    if (result == 0)
        RAISE_EXN("Insufficient memory to allocate space for result");

    processes->ThreadReleaseMLMemory(taskData);
    ffi_call(&cif, sym, result, arg_values);
    processes->ThreadUseMLMemory(taskData);

    // Allocate a vol for the result.  Don't do this before the
    // call in case we have a call-back and recursion.
    Handle res;
    {
        PLocker lock(&volLock);
        res = vol_alloc(taskData);
        C_POINTER(UNVOLHANDLE(res)) = result;
        OWN_C_SPACE(UNVOLHANDLE(res)) = true;
    }

    freeTypeVec(arg_types, num_args); // Free any structure entries
    freeTypeVec(&result_type, 1);

    // Record the last error result.  If this is Windows and
    // GetLastError returned a failure set that otherwise use
    // the value of errno.
#ifdef _WIN32
    int err = GetLastError();
    if (err != 0)
        taskData->lastError = -err;
    else
#endif
#ifdef HAVE_ERRNO_H
        taskData->lastError = errno;
#endif

    return res;
}

/**********************************************************************
 *
 *  Char Conversion
 *   
 **********************************************************************/

static Handle toCchar (TaskData *taskData, Handle h)
{
    char ch = (char)get_C_int(taskData, UNHANDLE(h));
    mes(("<%c>\n", ch));
    Handle res = vol_alloc_with_c_space(taskData, sizeof(char));
    *(char*)DEREFVOL(taskData, UNHANDLE(res)) = ch;
    return res;
}

static Handle fromCchar (TaskData *taskData, Handle h)
{
    char c = *(char*)DEREFVOL(taskData, UNHANDLE(h));
    mes(("<%c>\n", c));
    return Make_arbitrary_precision(taskData, c & 0xff);
}


/**********************************************************************
 *
 *  Double Conversion
 *   
 **********************************************************************/

static Handle toCdouble (TaskData *taskData, Handle h)
{
    double d = real_arg(h);
    mes(("<%f>\n", d));
    Handle res = vol_alloc_with_c_space(taskData, sizeof(double));
    *(double*)DEREFVOL(taskData, UNHANDLE(res)) = d;
    return res;
}

static Handle fromCdouble (TaskData *taskData, Handle h)
{
    double d = *(double*)DEREFVOL(taskData, UNHANDLE(h));
    mes(("<%f>\n", d));
    return real_result(taskData, d);
}


/**********************************************************************
 *
 *  Float Conversion
 *   
 **********************************************************************/

static Handle toCfloat (TaskData *taskData, Handle h)
{
    float f = (float)real_arg(h);
    mes(("<%f>\n", f));
    Handle res = vol_alloc_with_c_space(taskData, sizeof(float));
    *(float*)DEREFVOL(taskData, UNHANDLE(res)) = f;
    return res;
}

static Handle fromCfloat (TaskData *taskData, Handle h)
{
    float f = *(float*)DEREFVOL(taskData, UNHANDLE(h));
    mes(("<%f>\n", f));
    return real_result(taskData, (double)f);
}


/**********************************************************************
 *
 *  Int Conversion
 *   
 **********************************************************************/

static Handle toCint (TaskData *taskData, Handle h)
{
    int i = get_C_int(taskData, UNHANDLE(h));
    mes(("value = %d\n", i));
    Handle res = vol_alloc_with_c_space(taskData, sizeof(int));
    *(int*)DEREFVOL(taskData, UNHANDLE(res)) = i;
    return res;
}

static Handle fromCint (TaskData *taskData, Handle h)
{
    int i = *(int*)DEREFVOL(taskData, UNHANDLE(h));
    mes(("<%d>\n", i));
    return Make_arbitrary_precision(taskData, i);
}


/**********************************************************************
 *
 *  Long Conversion
 *   
 **********************************************************************/

static Handle toClong (TaskData *taskData, Handle h)
{
    long i = (long)get_C_long(taskData, UNHANDLE(h));
    mes(("value = %d\n", (int)i));
    Handle res = vol_alloc_with_c_space(taskData, sizeof(long));
    *(long*)DEREFVOL(taskData, UNHANDLE(res)) = i;
    return res;
}

static Handle fromClong (TaskData *taskData, Handle h)
{
    long i = *(long*)DEREFVOL(taskData, UNHANDLE(h));
    mes(("<%d>\n", (int)i));
    return Make_arbitrary_precision(taskData, i);
}


/**********************************************************************
 *
 *  Short Conversion
 *   
 **********************************************************************/

static Handle toCshort (TaskData *taskData, Handle h)
{
    short i = (short)get_C_long(taskData, UNHANDLE(h));
    mes(("<%d>\n", (int)i));
    Handle res = vol_alloc_with_c_space(taskData, sizeof(short));
    *(short*)DEREFVOL(taskData, UNHANDLE(res)) = i;
    return res;
}

static Handle fromCshort (TaskData *taskData, Handle h)
{
    short i = *(short*)DEREFVOL(taskData, UNHANDLE(h));
    mes(("<%d>\n", (int)i));
    return Make_arbitrary_precision(taskData, i);
}


/**********************************************************************
 *
 *  Unsigned int Conversion
 *   
 **********************************************************************/

static Handle toCuint (TaskData *taskData, Handle h)
{
    unsigned i = get_C_unsigned(taskData, UNHANDLE(h));
    mes(("value = %d\n", (int)i));
    Handle res = vol_alloc_with_c_space(taskData, sizeof(unsigned));
    *(unsigned*)DEREFVOL(taskData, UNHANDLE(res)) = i;
    return res;
}

static Handle fromCuint (TaskData *taskData, Handle h)
{
    unsigned i = *(unsigned*)DEREFVOL(taskData, UNHANDLE(h));
    mes(("<%d>\n", (int)i));
    return Make_arbitrary_precision(taskData, i);
}

/**********************************************************************
 *
 *  String Conversion
 *   
 **********************************************************************/
static Handle fillCstring (TaskData *taskData, Handle h)
{ TRACE; {
    POLYUNSIGNED size;
    PolyWord str = DEREFHANDLE(h)->Get(1);
    PolyVolData* cArg = (PolyVolData*)(DEREFHANDLE(h)->Get(0).AsObjPtr());

    if (str.IsTagged()) size = 1;
    else size = ((PolyStringObject*)str.AsObjPtr())->length;
    size += 1; // For the terminating zero

    Poly_string_to_C(str, (char*)DEREFVOL(taskData, cArg), size);
    PLocker plocker(&volLock);
    mes(("<%s>\n", (char*)C_POINTER(cArg)));
    return SAVE(TAGGED(0));
}}


static Handle toCstring (TaskData *taskData, Handle h)
{ TRACE; {
    POLYUNSIGNED size;
    {
        PolyWord str = DEREFWORD(h);
        if (str.IsTagged()) size = 1;
        else size = ((PolyStringObject*)str.AsObjPtr())->length;
    }
    size += 1; // For the terminating zero
    
    /* Allocate c-space for both the string & a pointer to the string,
    which is owned by the same vol. */
    Handle res = vol_alloc_with_c_space(taskData, sizeof(char*)+size);
    
    PLocker plocker(&volLock);
    /* Make the first word of the c-space point to the second word */
    *(void**)C_POINTER(UNVOLHANDLE(res)) = 1 + (void**)C_POINTER(UNVOLHANDLE(res));
    
    /* Copy the string into the c-space starting at the second word */
    Poly_string_to_C(DEREFWORD(h), (char*)(1 + (void**)C_POINTER(UNVOLHANDLE(res))), size);
    return res;
}}

static Handle fromCstring (TaskData *taskData, Handle h)
{ TRACE; {
    char* str = *(char**)DEREFVOL(taskData, UNHANDLE(h));
    mes(("<%s>\n", str));
    return SAVE(C_string_to_Poly(taskData, str));
}}

/* Byte vector functions.  The representation is the same as a string but
   the values may include zero bytes.  For fromCbytes the length
   therefore has to be passed as an argument.   Added DCJM 29/6/01. */
static Handle toCbytes (TaskData *taskData, Handle h)
{ TRACE; {
    POLYUNSIGNED size;
    {
        PolyWord str = DEREFWORD(h);
        if (str.IsTagged()) size = 1;
        else size = ((PolyStringObject*)str.AsObjPtr())->length;
    }
    // No terminating null here unlike strings
    
    /* Allocate c-space for both the string & a pointer to the string,
       which is owned by the same vol. */
    Handle res = vol_alloc_with_c_space(taskData, sizeof(char*)+size);
    PLocker plocker(&volLock);
    char  **p = (char**)C_POINTER(UNVOLHANDLE(res));
    
    /* Make the first word of the c-space point to the second word */
    *(char***)p = p + 1;
    
    /* Copy the string into the c-space starting at the second word */
    if (size == 1) **p = (char)UNTAGGED(DEREFHANDLE(h));
    else memcpy(*p, ((PolyStringObject*)h->WordP())->chars, size);
    
    return res;
}}

static Handle fromCbytes (TaskData *taskData, Handle h)
{ TRACE; {
    char* str = *(char**)DEREFVOL(taskData, DEREFHANDLE(h)->Get(0));
    size_t size = get_C_long(taskData, DEREFHANDLE(h)->Get(1));
    if (str == NULL) return SAVE(EmptyString(taskData));
    else return SAVE(C_string_to_Poly(taskData, str, size));
}}


/**********************************************************************
 *
 *  call_sym_and_convert
 *   
 **********************************************************************/

typedef enum {
    directedArg_In = 1,
    directedArg_Out = 2
} DirectedArgs;

/*
    datatype 'a union =
        Char    of string
      | Double  of real
      | Float   of real
      | Int     of int
      | Long    of int
      | Short   of int
      | String  of string
      | Vol     of 'a
*/
typedef enum {
    union_Char = 1,
    union_Double,
    union_Float,
    union_Int,
    union_Long,
    union_Short,
    union_String,
    union_Vol,
    union_Uint /* 9 */
} UnionTypes;


static Handle UNION_MAKE(TaskData *taskData, UnionTypes tag, Handle contents)
{
    Handle res = SAVE(alloc(taskData, 2));
    UNHANDLE(res)->Set(1, TAGGED(tag));
    DEREFHANDLE(res)->Set(0, UNHANDLE(contents));
    return res;
}

/*
    datatype 'ctype unionChoice =
        chooseChar
      | chooseDouble
      | chooseFloat
      | chooseInt
      | chooseLong
      | chooseShort
      | chooseString;
      | chooseVol of 'ctype
*/
typedef enum {
    choice_chooseChar = 1,
    choice_chooseDouble,
    choice_chooseFloat,
    choice_chooseInt,
    choice_chooseLong,
    choice_chooseShort,
    choice_chooseString,
    choice_chooseVol, /* not used, since boxed value is untagged */
    choice_chooseUint /* 9 */
} ChoiceTypes;

#define TAG(x)          (UNHANDLE(x)->Get(1))
#define CONTENTS(x)     (SAVE(UNHANDLE(x)->Get(0)))


#define LIST_ISNULL(x)  (ML_Cons_Cell::IsNull(UNHANDLE(x)))
#define LIST_HEAD(x)    (SAVE(Head(DEREFWORD(x))))
#define LIST_TAIL(x)    (SAVE(Tail(DEREFWORD(x))))
#define LIST_NULL       (SAVE(ListNull))

static Handle LIST_CONS (TaskData *taskData, Handle x,Handle xs)
{
    Handle res = SAVE(alloc(taskData, sizeof(ML_Cons_Cell)));
    Head(DEREFWORD(res)) = UNHANDLE(x);
    Tail(DEREFWORD(res)) = DEREFWORD(xs);
    return res;
}

#define TUPLE_GET1(x)  (SAVE(DEREFHANDLE(x)->Get(0)))
#define TUPLE_GET2(x)  (SAVE(DEREFHANDLE(x)->Get(1)))
#define TUPLE_GET3(x)  (SAVE(DEREFHANDLE(x)->Get(2)))

static Handle TUPLE_MAKE2 (TaskData *taskData, Handle x,Handle y)
{
    Handle res = SAVE(alloc(taskData, 2));
    DEREFHANDLE(res)->Set(0, DEREFWORD(x));
    DEREFHANDLE(res)->Set(1, DEREFWORD(y));
    return res;
}

/* For testing...
static Handle print_ctype_and_vol (Handle pair)
{

  word* ctype = UNHANDLE(TUPLE_GET1(pair));
  Handle vol  = TUPLE_GET2(pair);
  void* thing = DEREFVOL(taskData, UNHANDLE(vol));

  switch ((Ctype)UNTAGGED(ctype)) {
  case Cchar    : printf("Cchar <%c>\n",         *(char*)thing);     break;
  case Cdouble  : printf("Cdouble <%f>\n",       *(double*)thing);   break;
  case Cfloat   : printf("Cfloat <%f>\n",        *(float*)thing);    break;
  case Cint     : printf("Cint <%d>\n",          *(int*)thing);      break;
  case Clong    : printf("Clong <%ld>\n",        *(long*)thing);     break;
  case Cpointer : printf("Cpointer <%.3s>...\n", *(char**)thing);    break;
  case Cshort   : printf("Cshort <%d>\n",        *(short*)thing);    break;
  default       : {
    show(("Must be a Cstruct <%d>\n", get_C_long(taskData, ctype)));
  }}
  return pair;
}
...*/


static Handle union2vol_and_ctype (TaskData *taskData, Handle u)
{
    Handle contents = CONTENTS(u);
    switch (UNTAGGED(TAG(u))) {
    case union_Char:      return TUPLE_MAKE2(taskData, SAVE(TAGGED(Cchar)),    toCchar(taskData, contents));
    case union_Double:    return TUPLE_MAKE2(taskData, SAVE(TAGGED(Cdouble)),  toCdouble(taskData, contents));
    case union_Float:     return TUPLE_MAKE2(taskData, SAVE(TAGGED(Cfloat)),   toCfloat(taskData, contents));
    case union_Int:       return TUPLE_MAKE2(taskData, SAVE(TAGGED(Cint)),     toCint(taskData, contents));
    case union_Long:      return TUPLE_MAKE2(taskData, SAVE(TAGGED(Clong)),    toClong(taskData, contents));
    case union_Short:     return TUPLE_MAKE2(taskData, SAVE(TAGGED(Cshort)),   toCshort(taskData, contents));
    case union_String:    return TUPLE_MAKE2(taskData, SAVE(TAGGED(Cpointer)), toCstring(taskData, contents));
    case union_Uint:      return TUPLE_MAKE2(taskData, SAVE(TAGGED(Cuint)),    toCuint(taskData, contents));
    case union_Vol:       return contents;
        
    default:              RAISE_EXN ("Unknown union tag");
    }
    /*NOTREACHED*/
    return 0;
}


static Handle choice2ctype (TaskData *taskData, Handle choice)
{
    PolyWord either_tag_or_pointer = DEREFWORDHANDLE(choice);

    if (IS_INT(either_tag_or_pointer)) {
        switch (UNTAGGED(either_tag_or_pointer)) {
        case choice_chooseChar:       return SAVE(TAGGED(Cchar));
        case choice_chooseDouble:     return SAVE(TAGGED(Cdouble));
        case choice_chooseFloat:      return SAVE(TAGGED(Cfloat));
        case choice_chooseInt:        return SAVE(TAGGED(Cint));
        case choice_chooseLong:       return SAVE(TAGGED(Clong));
        case choice_chooseShort:      return SAVE(TAGGED(Cshort));
        case choice_chooseString:     return SAVE(TAGGED(Cpointer));
        case choice_chooseUint:       return SAVE(TAGGED(Cuint));
        default:                      RAISE_EXN ("Unknown choice type"); return 0;
        }
    }
    else /* It's a pointer */ return SAVE(either_tag_or_pointer.AsObjPtr()->Get(0));
}

  
static  Handle choice_and_vol2union (TaskData *taskData, Handle pair)
{
    Handle choice = TUPLE_GET1(pair);
    Handle vol = TUPLE_GET2(pair);
    PolyWord maybe_tag = DEREFWORDHANDLE(choice);
    if (IS_INT(maybe_tag)) {
        switch (UNTAGGED(maybe_tag)) {
        case choice_chooseChar:       return UNION_MAKE(taskData, union_Char,   fromCchar(taskData, vol));
        case choice_chooseDouble:     return UNION_MAKE(taskData, union_Double, fromCdouble(taskData, vol));
        case choice_chooseFloat:      return UNION_MAKE(taskData, union_Float,  fromCfloat(taskData, vol));
        case choice_chooseInt:        return UNION_MAKE(taskData, union_Int,    fromCint(taskData, vol));
        case choice_chooseLong:       return UNION_MAKE(taskData, union_Long,   fromClong(taskData, vol));
        case choice_chooseShort:      return UNION_MAKE(taskData, union_Short,  fromCshort(taskData, vol));
        case choice_chooseString:     return UNION_MAKE(taskData, union_String, fromCstring(taskData, vol));
        case choice_chooseUint:       return UNION_MAKE(taskData, union_Uint,   fromCuint(taskData, vol));
        default:                      RAISE_EXN ("Unknown choice type"); return 0;
        }
    }
    else  /* It's a pointer */ return UNION_MAKE(taskData, union_Vol,    vol);
}

  
/*
DCJM 7/4/04.  Based on reading the code, it seems as though the arguments to a function
can be either "in" or "out" parameters.  "in" parameters are passed as expected from ML
to C whereas "out" parameters have to be constructed from the type information supplied
and their values returned along with the result.
*/
static void mkArgs (TaskData *taskData, Handle xs, Handle &args, Handle &rets)
{
    TRACE;
    if (LIST_ISNULL(xs))
    {
        args = LIST_NULL;
        rets = LIST_NULL;
    }
    else
    {
        Handle x    = LIST_HEAD(xs);
        mkArgs(taskData, LIST_TAIL(xs), args, rets);
        
        switch (UNTAGGED(TAG(x))) {
        case directedArg_In:
            {
                // IN parameter - We have the type information and the value to pass.
                // Convert the value into a "vol".
                Handle vol_and_ctype = union2vol_and_ctype(taskData, CONTENTS(x));
                args = LIST_CONS(taskData, vol_and_ctype,args);
                return;
            }
        case directedArg_Out:
            {
                // OUT parameter - We have just the type information and have to construct
                // a "vol" to pass as the actual argument.
                Handle choice = CONTENTS(x);
                Handle ctype = choice2ctype(taskData, choice);
                Handle space = allocate(taskData, c_sizeof(taskData, ctype));
                Handle arg = TUPLE_MAKE2(taskData, SAVE(TAGGED(Cpointer)), address(taskData, space));
                Handle ret = TUPLE_MAKE2(taskData, choice,space);
            
                args = LIST_CONS(taskData, arg,args);
                rets = LIST_CONS(taskData, ret,rets);
                return;
            }
            
        default: RAISE_EXN ("Unknown directedArg tag\n");
        }
    }
    /*NOTREACHED*/
}

typedef Handle (*Handle2Handle)(TaskData *taskData, Handle);

static Handle map (TaskData *taskData,Handle2Handle f, Handle xs)
{
  return
    LIST_ISNULL(xs) ? LIST_NULL
                    : LIST_CONS(taskData,  f(taskData, LIST_HEAD(xs)), map(taskData, f,LIST_TAIL(xs)) );
}
/*
DCJM 7/4/04.  For reading the code it seems as though call_sym_and_convert takes three
arguments: sym, unionArgs and retChoice and returns a pair (x, y) as the result.
sym is simply the symbol to call.
unionArgs is a list of arguments to the function.  An argument can either be an "in" parameter
consisting of type info and a value or an "out" parameter in which case only the type info
is supplied.
retChoice is the type of the result.
The result of this function is a pair consisting of the value and a list of the "out" parameters.

The reason for using call_sym_and_convert rather than call_sym is that call_sym_and_convert takes
the function arguments as a list of a union of the ML values and converts them to the "vols" with
the C values as part of the call.  This avoids the multiple calls between ML and the RTS that
would be needed if each argument was converted separately, something that is needed for call_sym.
*/
static Handle call_sym_and_convert (TaskData *taskData, Handle triple)
{
    TRACE;

    Handle sym       = TUPLE_GET1(triple);
    Handle unionArgs = TUPLE_GET2(triple);
    Handle retChoice = TUPLE_GET3(triple);
    
    Handle args, rets;
    mkArgs(taskData, unionArgs, args, rets);
    Handle retCtype  = choice2ctype(taskData, retChoice);
    
    // If call_sym results in a callback the save vector will be reset and
    // all these handles will be overwritten.  We have to save them on a
    // separate stack per thread.
    Handle newStack = alloc_and_save(taskData, 3, 0);
    newStack->WordP()->Set(0, retChoice->Word());
    newStack->WordP()->Set(1, rets->Word());
    newStack->WordP()->Set(2, taskData->foreignStack);
    taskData->foreignStack = newStack->Word();
   
    /*map(print_ctype_and_vol,args);*/
    Handle vol = call_sym(taskData, sym,args,retCtype);

    // Pop the old values and put them back on the save vector
    retChoice = SAVE(taskData->foreignStack.AsObjPtr()->Get(0));
    rets = SAVE(taskData->foreignStack.AsObjPtr()->Get(1));
    taskData->foreignStack = taskData->foreignStack.AsObjPtr()->Get(2);
    
    return TUPLE_MAKE2 (taskData, choice_and_vol2union(taskData, TUPLE_MAKE2(taskData, retChoice, vol)),
              map(taskData, choice_and_vol2union, rets));
}

// This is the C function that will get control when any callback is made.  The "data"
// argument is the entry in the callback table for this callback.
static void callbackEntryPt(ffi_cif *cif, void *ret, void* args[], void *data)
{
    uintptr_t cbIndex = (uintptr_t)data;
    ASSERT(cbIndex < callBackEntries);
    struct _cbStructEntry *cbEntry = &callbackTable[cbIndex];
    // We should get the task data for the thread that is running this code.
    // If this thread has been created by the foreign code we will have to
    // create a new one here.
    TaskData *taskData = processes->GetTaskDataForThread();
    if (taskData == 0)
    {
        try {
            taskData = processes->CreateNewTaskData(0, 0, 0, TAGGED(0));
        }
        catch (std::bad_alloc &) {
            ::Exit("Unable to create thread data - insufficient memory");
        }
        catch (MemoryException &) {
            ::Exit("Unable to create thread data - insufficient memory");
        }
    }
    else processes->ThreadUseMLMemory(taskData);

    Handle mark = taskData->saveVec.mark();
    Handle h = SAVE(cbEntry->mlFunction);

    // Construct an ML argument list from the arguments.
    Handle saved = taskData->saveVec.mark();
    Handle mlArgs = SAVE(ListNull);
    for (unsigned i = cif->nargs; i > 0; i--)
    {
        ffi_type *argType = cif->arg_types[i-1];
        Handle value = vol_alloc_with_c_space(taskData, argType->size);
        memcpy(DEREFVOL(taskData, UNHANDLE(value)), args[i-1], argType->size);

        Handle next  = alloc_and_save(taskData, sizeof(ML_Cons_Cell)/sizeof(PolyWord));
        DEREFLISTHANDLE(next)->h = DEREFWORDHANDLE(value); 
        DEREFLISTHANDLE(next)->t = DEREFLISTHANDLE(mlArgs);

        taskData->saveVec.reset(saved);
        mlArgs = SAVE(DEREFHANDLE(next));
    }

    // Callbacks previously involved forking a new ML process.  They are
    // now handled on the caller's stack.
    Handle resultHandle = taskData->EnterCallbackFunction(h, mlArgs);

    PolyWord resultWord = UNHANDLE(resultHandle);
    taskData->saveVec.reset(mark);
    memcpy(ret, DEREFVOL(taskData, resultWord), cif->rtype->size);
    // Once we have copied the result into C memory we are free of the
    // ML memory and can allow any GC.
    processes->ThreadReleaseMLMemory(taskData);
}

// Creates a call-back entry.  Callbacks are never GCd because we don't know when
// the C code will be finished with them.
static Handle createCallbackFunction(TaskData *taskData, Handle triple, ffi_abi abi)
{
    TRACE;
    Handle argTypeList = TUPLE_GET1(triple);
    Handle cResultType = TUPLE_GET2(triple);
    Handle mlFunction = TUPLE_GET3(triple);

    PLocker pLocker(&callbackTableLock);

    // Make a new entry in the callback table.
    struct _cbStructEntry *newTable =
        (struct _cbStructEntry*)realloc(callbackTable, (callBackEntries+1)*sizeof(struct _cbStructEntry));
    if (newTable == 0)
        RAISE_EXN("Unable to allocate memory for callback table");
    callbackTable = newTable;
    callbackTable[callBackEntries].argType = UNHANDLE(argTypeList);
    callbackTable[callBackEntries].mlFunction = UNHANDLE(mlFunction);
    callbackTable[callBackEntries].cFunction = 0;

    void *resultFunction;
    ffi_closure *closure = (ffi_closure *)ffi_closure_alloc(sizeof(ffi_closure), &resultFunction);
    if (closure == 0)
        RAISE_EXN("Callbacks not implemented or insufficient memory");

    unsigned num_args = length_list(argTypeList->Word());
    ffi_type **arg_types = (ffi_type**)malloc(num_args * sizeof(ffi_type*));
    if (arg_types == 0)
        RAISE_EXN("Insufficient memory to allocate space for arguments");

    PolyWord p = argTypeList->Word();
    for (POLYUNSIGNED i=0; i<num_args; i++,p=Tail(p))
        arg_types[i] = ctypeToFfiType(taskData, Head(p));
    ffi_type *result_type = ctypeToFfiType(taskData, cResultType->Word());

    // The cif needs to be on the heap so that it is available in the callback.
    ffi_cif *cif = (ffi_cif *)malloc(sizeof(ffi_cif));
    if (cif == 0)
        RAISE_EXN("Insufficient memory to allocate space for cif");

    if (ffi_prep_cif(cif, abi, num_args, result_type, arg_types) != FFI_OK)
        RAISE_EXN("libffi error: ffi_prep_cif failed");

    // Pass the index into the callback table here rather than the address of the entry
    // because the table may move if we realloc.
    if (ffi_prep_closure_loc(closure, cif, callbackEntryPt, (void*)((uintptr_t)callBackEntries), resultFunction) != FFI_OK)
        RAISE_EXN("libffi error: ffi_prep_closure_loc failed");

    callbackTable[callBackEntries].cFunction = (unsigned char*)resultFunction;
    /* Construct a "vol" containing the pointer to the C function. */
    Handle res = vol_alloc_with_c_space(taskData, sizeof(void*));
    PLocker plocker(&volLock);
    *(unsigned char **)C_POINTER(UNVOLHANDLE(res)) = callbackTable[callBackEntries].cFunction;
    callBackEntries++;
    return res;
}

/* Create a callback using C calling conventions.  The calling function removes the
   arguments from the stack. */
static Handle toCfunction (TaskData *taskData, Handle triple)
{
    return createCallbackFunction(taskData, triple, FFI_DEFAULT_ABI);
}

/* Create a callback using Pascal/WINAPI/CALLBACK/__stdcall calling conventions.
   The CALLED function must remove the arguments from the stack before returning. */
static Handle toPascalfunction (TaskData *taskData, Handle triple)
{
#if(defined(_WIN32) && ! defined(_WIN64))    // We can't actually test for FFI_STDCALL here because it's a value in an enum not a define.
    return createCallbackFunction(taskData, triple, FFI_STDCALL);
#else
    RAISE_EXN("Pascal (stdcall) calling conventions are not supported on this platform");
    return (Handle)0;
#endif
}

typedef void   (*finalType)(void*);

// Set a finalisation function: A C function that is called when the Vol is freed by
// the GC.
static Handle set_final (TaskData *taskData, Handle pair)
{
    Handle symH       = TUPLE_GET1(pair);
    Handle volH       = TUPLE_GET2(pair);
    PolyVolData *vol  = (PolyVolData*)(UNHANDLE(volH));
    finalType f       = *(finalType*)DEREFVOL(taskData, symH->Word());
    FINALISER(vol)    = f;
    return SAVE(TAGGED(0));
}

// Return the NULL vol.  This is a persistent vol which always contains null.
// There is no associated back pointer.
static Handle getNull(TaskData *taskData, Handle)
{
    PolyVolData* v = (PolyVolData*)alloc(taskData, VOL_BOX_SIZE, F_MUTABLE_BIT|F_BYTE_OBJ);
    Handle result = SAVE(v);
    V_INDEX(v) = VOL_NULL_INDEX;
    MakeVolMagic(v);
    return result;
}

/**********************************************************************
 *
 *  Foreign Dispatch
 *   
 **********************************************************************/

typedef Handle(* type_hh_fun)(TaskData *taskData, Handle);

static type_hh_fun handlers[] =
{
  get_foreign_debug,
  set_foreign_debug,

  load_lib,
  load_sym,
  call_sym_and_convert,

  allocate,
  address,
  deref,
  offset,
  assign,
  c_sizeof,
  alignment,
  
  toCchar,
  fromCchar,

  toCdouble,
  fromCdouble,

  toCfloat,
  fromCfloat,

  toCint,
  fromCint,

  toClong,
  fromClong,

  toCshort,
  fromCshort,

  fillCstring,
  toCstring,
  fromCstring,

  toCuint,      /* Added DCJM 17/5/01. */
  fromCuint,        /* Added DCJM 17/5/01. */

  toCbytes,     /* Added DCJM 29/6/01. */
  fromCbytes,

  toCfunction,      /* Added DCJM 7/4/04. */
  toPascalfunction, /* Added DCJM 7/4/04. */

  set_final, /* Added DCJM 2/8/09. */

  getNull // Added DCJM 16/11/11.
};
    
#define NUM_HANDLERS ((int)(sizeof(handlers)/sizeof(type_hh_fun)))


static Handle foreign_dispatch_c (TaskData *taskData, Handle args, Handle fcode_h)
{
    int fcode = get_C_int(taskData, DEREFWORD(fcode_h));
    
    if (fcode < 0 || fcode >= NUM_HANDLERS) {
        char buf[100];
        sprintf(buf, "Unknown foreign dispatch code <%d>", fcode);
        RAISE_EXN(buf);
    }
    
    mes(("dispatch code = %d\n", fcode));
    /* dispatch to desired function */
    return (handlers[fcode])(taskData, args);
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyForeignGeneral(PolyObject *threadId, PolyWord code, PolyWord arg)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle pushedCode = taskData->saveVec.push(code);
    Handle result = 0;

    try {
        result = foreign_dispatch_c(taskData, pushedArg, pushedCode);
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

#else
// The foreign function interface isn't available.
#include "foreign.h"
#include "run_time.h"
#include "sys.h"

Handle foreign_dispatch_c (TaskData *taskData, Handle args, Handle fcode_h)
{
    raise_exception_string(taskData, EXC_foreign, "The foreign function interface is not available on this platform");
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyForeignGeneral(PolyObject *threadId, PolyWord /*code*/, PolyWord /*arg*/)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    taskData->PreRTSCall();

    try {
        raise_exception_string(taskData, EXC_foreign, "The foreign function interface is not available on this platform");
    } catch (...) { }  // Handle the C++ exception

    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned(); // Return unit since we're raising an exception
}

#endif

