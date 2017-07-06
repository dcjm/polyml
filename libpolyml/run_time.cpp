/*
    Title:      Run-time system.
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further work copyright David C. J. Matthews 2009, 2012, 2015-16

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

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#include "globals.h"
#include "gc.h"
#include "mpoly.h"
#include "arb.h"
#include "diagnostics.h"
#include "processes.h"
#include "profiling.h"
#include "run_time.h"
#include "sys.h"
#include "polystring.h"
#include "save_vec.h"
#include "rts_module.h"
#include "memmgr.h"

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyFullGC(PolyObject *threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyIsBigEndian();
}

#define SAVE(x) taskData->saveVec.push(x)
#define SIZEOF(x) (sizeof(x)/sizeof(PolyWord))

// used heavily by MD_init_interface_vector in machine_dep.c
void add_word_to_io_area (unsigned sysop, PolyWord val)
{
    ASSERT (sysop > 0 && sysop < 256);
    PolyWord *objAddr = IoEntry(sysop);
    objAddr[0] = val;
}

/******************************************************************************/
/*                                                                            */
/*      STORAGE ALLOCATION                                                    */
/*                                                                            */
/******************************************************************************/

// This is the storage allocator for allocating heap objects in the RTS.
PolyObject *alloc(TaskData *taskData, POLYUNSIGNED data_words, unsigned flags)
/* Allocate a number of words. */
{
    POLYUNSIGNED words = data_words + 1;
    
    if (profileMode == kProfileStoreAllocation)
        taskData->addAllocationProfileCount(words);

    PolyWord *foundSpace = processes->FindAllocationSpace(taskData, words, false);
    if (foundSpace == 0)
    {
        // Failed - the thread is set to raise an exception.
        throw IOException();
    }

    PolyObject *pObj = (PolyObject*)(foundSpace + 1);
    pObj->SetLengthWord(data_words, flags);
    
    // Must initialise object here, because GC doesn't clean store.
    // Is this necessary any more?  This used to be necessary when we used
    // structural equality and wanted to make sure that unused bytes were cleared.
    // N.B.  This sets the store to zero NOT TAGGED(0).
    for (POLYUNSIGNED i = 0; i < data_words; i++) pObj->Set(i, PolyWord::FromUnsigned(0));
    return pObj;
}

/******************************************************************************/
/*                                                                            */
/*      alloc_and_save - called by run-time system                            */
/*                                                                            */
/******************************************************************************/
Handle alloc_and_save(TaskData *taskData, POLYUNSIGNED size, unsigned flags)
/* Allocate and save the result on the vector. */
{
    return SAVE(alloc(taskData, size, flags));
}

/* CALL_IO0(full_gc_, NOIND) */
Handle full_gc_c(TaskData *taskData)
{
    FullGC(taskData);
    return SAVE(TAGGED(0));
}

POLYUNSIGNED PolyFullGC(PolyObject *threadId)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();

    try {
        // Can this raise an exception e.g. if there is insufficient memory?
        FullGC(taskData);
    } catch (...) { } // If an ML exception is raised

    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned(); // Returns unit.
}


/******************************************************************************/
/*                                                                            */
/*      Error Messages                                                        */
/*                                                                            */
/******************************************************************************/


// Return the handle to a string error message.  This will return
// something like "Unknown error" from strerror if it doesn't match
// anything.
Handle errorMsg(TaskData *taskData, int err)
{
#ifdef _WIN32
    /* In the Windows version we may have both errno values
       and also GetLastError values.  We convert the latter into
       negative values before returning them. */
    if (err < 0)
    {
        LPTSTR lpMsg = NULL;
        TCHAR *p;
        if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM |
                FORMAT_MESSAGE_ALLOCATE_BUFFER |
                FORMAT_MESSAGE_IGNORE_INSERTS,
                NULL, (DWORD)(-err), 0, (LPTSTR)&lpMsg, 1, NULL) > 0)
        {
            /* The message is returned with CRLF at the end.  Remove them. */
            for (p = lpMsg; *p != '\0' && *p != '\n' && *p != '\r'; p++);
            *p = '\0';
            Handle res = SAVE(C_string_to_Poly(taskData, lpMsg));
            LocalFree(lpMsg);
            return res;
        }
    }
#endif
    // Unix and unknown Windows errors.
    return SAVE(C_string_to_Poly(taskData, strerror(err)));
}


/******************************************************************************/
/*                                                                            */
/*      EXCEPTIONS                                                            */
/*                                                                            */
/******************************************************************************/

Handle make_exn(TaskData *taskData, int id, Handle arg)
{
    const char *exName;
    switch (id) {
    case EXC_interrupt: exName = "Interrupt"; break;
    case EXC_syserr: exName = "SysErr"; break;
    case EXC_size: exName = "Size"; break;
    case EXC_overflow: exName = "Overflow"; break;
    case EXC_underflow: exName = "Underflow"; break;
    case EXC_divide: exName = "Div"; break;
    case EXC_conversion: exName = "Conversion"; break;
    case EXC_XWindows: exName = "XWindows"; break;
    case EXC_subscript: exName = "Subscript"; break;
    case EXC_foreign: exName = "Foreign"; break;
    case EXC_Fail: exName = "Fail"; break;
    case EXC_thread: exName = "Thread"; break;
    case EXC_extrace: exName = "ExTrace"; break;
    default: ASSERT(0); exName = "Unknown"; // Shouldn't happen.
    }
   

    Handle pushed_name = SAVE(C_string_to_Poly(taskData, exName));
    
    Handle exnHandle = alloc_and_save(taskData, SIZEOF(poly_exn));
    
    DEREFEXNHANDLE(exnHandle)->ex_id   = TAGGED(id);
    DEREFEXNHANDLE(exnHandle)->ex_name = DEREFWORD(pushed_name);
    DEREFEXNHANDLE(exnHandle)->arg     = DEREFWORDHANDLE(arg);
    DEREFEXNHANDLE(exnHandle)->ex_location = TAGGED(0);

    return exnHandle;
}

/******************************************************************************/
/*                                                                            */
/*      raise_exception - called by run-time system                           */
/*                                                                            */
/******************************************************************************/
void raise_exception(TaskData *taskData, int id, Handle arg)
/* Raise an exception with no arguments. */
{
    Handle exn = make_exn(taskData, id, arg);
    /* N.B.  We must create the packet first BEFORE dereferencing the
       process handle just in case a GC while creating the packet
       moves the process and/or the stack. */
    taskData->SetException(DEREFEXNHANDLE(exn));
    throw IOException(); /* Return to Poly code immediately. */
    /*NOTREACHED*/
}


/******************************************************************************/
/*                                                                            */
/*      raise_exception0 - called by run-time system                          */
/*                                                                            */
/******************************************************************************/
void raise_exception0(TaskData *taskData, int id)
/* Raise an exception with no arguments. */
{
    raise_exception(taskData, id, SAVE(TAGGED(0)));
    /*NOTREACHED*/
}

/******************************************************************************/
/*                                                                            */
/*      raise_exception_string - called by run-time system                    */
/*                                                                            */
/******************************************************************************/
void raise_exception_string(TaskData *taskData, int id, const char *str)
/* Raise an exception with a C string as the argument. */
{
    raise_exception(taskData, id, SAVE(C_string_to_Poly(taskData, str)));
    /*NOTREACHED*/
}

// Raise a SysErr exception with a given error code.
// The string part must match the result of OS.errorMsg
void raiseSyscallError(TaskData *taskData, int err)
{
    Handle errornum = Make_fixed_precision(taskData, err);
    Handle pushed_option = alloc_and_save(taskData, 1);
    DEREFHANDLE(pushed_option)->Set(0, DEREFWORDHANDLE(errornum)); /* SOME err */
    Handle pushed_name = errorMsg(taskData, err); // Generate the string.
    Handle pair = alloc_and_save(taskData, 2);
    DEREFHANDLE(pair)->Set(0, DEREFWORDHANDLE(pushed_name));
    DEREFHANDLE(pair)->Set(1, DEREFWORDHANDLE(pushed_option));

    raise_exception(taskData, EXC_syserr, pair);
}

// Raise a SysErr exception which does not correspond to an error code.
void raiseSyscallMessage(TaskData *taskData, const char *errmsg)
{
    Handle pushed_option = SAVE(NONE_VALUE); /* NONE */
    Handle pushed_name = SAVE(C_string_to_Poly(taskData, errmsg));
    Handle pair = alloc_and_save(taskData, 2);
    DEREFHANDLE(pair)->Set(0, DEREFWORDHANDLE(pushed_name));
    DEREFHANDLE(pair)->Set(1, DEREFWORDHANDLE(pushed_option));

    raise_exception(taskData, EXC_syserr, pair);
}

// This was the previous version.  The errmsg argument is ignored unless err is zero.
// Calls to it should really be replaced with calls to either raiseSyscallMessage
// or raiseSyscallError but it's been left because there may be cases where errno
// actually contains zero.
void raise_syscall(TaskData *taskData, const char *errmsg, int err)
{
    if (err == 0) raiseSyscallMessage(taskData, errmsg);
    else raiseSyscallError(taskData, err);
}

// Raises a Fail exception.
void raise_fail(TaskData *taskData, const char *errmsg)
{
    raise_exception_string(taskData, EXC_Fail, errmsg);
}

/* "Polymorphic" function to generate a list. */
Handle makeList(TaskData *taskData, int count, char *p, int size, void *arg,
                       Handle (mkEntry)(TaskData *, void*, char*))
{
    Handle saved = taskData->saveVec.mark();
    Handle list = SAVE(ListNull);
    /* Start from the end of the list. */
    p += count*size;
    while (count > 0)
    {
        Handle value, next;
        p -= size; /* Back up to the last entry. */
        value = mkEntry(taskData, arg, p);
        next  = alloc_and_save(taskData, SIZEOF(ML_Cons_Cell));

        DEREFLISTHANDLE(next)->h = DEREFWORDHANDLE(value); 
        DEREFLISTHANDLE(next)->t = DEREFLISTHANDLE(list);

        taskData->saveVec.reset(saved);
        list = SAVE(DEREFHANDLE(next));
        count--;
    }
    return list;
}

// Return the address of the iovec entry for a given index.
Handle io_operation_c(TaskData *taskData, Handle entry)
{
    unsigned entryNo = get_C_unsigned(taskData, DEREFWORD(entry));
    if (entryNo >= POLY_SYS_vecsize)
        raise_exception0(taskData, EXC_subscript);
    return SAVE((PolyObject*)IoEntry(entryNo));
}

// In most cases the assembly coded version of this will handle the
// allocation.  The function can be called by the assembly code
// when it finds it has run out.  Using it avoids us having a
// return address into the assembly code.
Handle alloc_store_long_c(TaskData *taskData, Handle initial, Handle flags_handle, Handle size )
{
    unsigned flags = get_C_unsigned(taskData, DEREFWORD(flags_handle));
    POLYUNSIGNED usize = getPolyUnsigned(taskData, DEREFWORD(size));
    
    if (usize >= MAX_OBJECT_SIZE)
        raise_exception0(taskData, EXC_size);
    
    PolyObject *vector = alloc(taskData, usize, flags| F_MUTABLE_BIT);
    
    PolyWord value = DEREFWORD(initial);
    
    if (vector->IsByteObject()) {
        // Byte segments are supposed to be initialised only with zero
        if (value != TAGGED(0))
            raise_exception_string(taskData, EXC_Fail, "non-zero byte segment");
    }
    else if (value != PolyWord::FromUnsigned(0))  {
        for (POLYUNSIGNED i = 0; i < usize; i++)
            vector->Set(i, value);
    }
    
    return taskData->saveVec.push(vector);
}

void CheckAndGrowStack(TaskData *taskData, POLYUNSIGNED minSize)
/* Expands the current stack if it has grown. We cannot shrink a stack segment
   when it grows smaller because the frame is checked only at the beginning of
   a function to ensure that there is enough space for the maximum that can
   be allocated. */
{
    /* Get current size of new stack segment. */
    POLYUNSIGNED old_len = taskData->stack->spaceSize();

    if (old_len >= minSize) return; /* Ok with present size. */

    // If it is too small double its size.
    POLYUNSIGNED new_len; /* New size */
    for (new_len = old_len; new_len < minSize; new_len *= 2);
    POLYUNSIGNED limitSize = getPolyUnsigned(taskData, taskData->threadObject->mlStackSize);

    // Do not grow the stack if its size is already too big.
    if ((limitSize != 0 && old_len >= limitSize) || ! gMem.GrowOrShrinkStack(taskData, new_len))
    {
        /* Cannot expand the stack any further. */
        extern FILE *polyStderr;
        fprintf(polyStderr, "Warning - Unable to increase stack - interrupting thread\n");
        if (debugOptions & DEBUG_THREADS)
            Log("THREAD: Unable to grow stack for thread %p from %lu to %lu\n", taskData, old_len, new_len);
        // We really should do this only if the thread is handling interrupts
        // asynchronously.  On the other hand what else do we do?
        Handle exn = make_exn(taskData, EXC_interrupt, SAVE(TAGGED(0)));
        taskData->SetException(DEREFEXNHANDLE(exn));
    }
    else
    {
        if (debugOptions & DEBUG_THREADS)
            Log("THREAD: Growing stack for thread %p from %lu to %lu\n", taskData, old_len, new_len);
    }
}

Handle Make_fixed_precision(TaskData *taskData, int val)
{
    if (val > MAXTAGGED || val < -MAXTAGGED-1)
        raise_exception0(taskData, EXC_overflow);
    return taskData->saveVec.push(TAGGED(val));
}

Handle Make_fixed_precision(TaskData *taskData, unsigned uval)
{
    if (uval > MAXTAGGED)
        raise_exception0(taskData, EXC_overflow);
    return taskData->saveVec.push(TAGGED(uval));
}

Handle Make_fixed_precision(TaskData *taskData, long val)
{
    if (val > MAXTAGGED || val < -MAXTAGGED-1)
        raise_exception0(taskData, EXC_overflow);
    return taskData->saveVec.push(TAGGED(val));
}

Handle Make_fixed_precision(TaskData *taskData, unsigned long uval)
{
    if (uval > MAXTAGGED)
        raise_exception0(taskData, EXC_overflow);
    return taskData->saveVec.push(TAGGED(uval));
}

#if (SIZEOF_LONG_LONG != 0) && (SIZEOF_LONG_LONG <= SIZEOF_VOIDP)
Handle Make_fixed_precision(TaskData *taskData, long long val)
{
    if (val > MAXTAGGED || val < -MAXTAGGED-1)
        raise_exception0(taskData, EXC_overflow);
    return taskData->saveVec.push(TAGGED(val));
}

Handle Make_fixed_precision(TaskData *taskData, unsigned long long uval)
{
    if (uval > MAXTAGGED)
        raise_exception0(taskData, EXC_overflow);
    return taskData->saveVec.push(TAGGED(uval));
}
#endif

Handle Make_sysword(TaskData *taskData, uintptr_t p)
{
    Handle result = alloc_and_save(taskData, 1, F_BYTE_OBJ);
    *(uintptr_t*)(result->Word().AsCodePtr()) = p;
    return result;
}

static unsigned long rtsCallCounts[POLY_SYS_vecsize];

void IncrementRTSCallCount(unsigned ioFunction)
{
    if ((debugOptions & DEBUG_RTSCALLS) && ioFunction < POLY_SYS_vecsize)
        rtsCallCounts[ioFunction]++;
}

// This is used to determine the endian-ness that Poly/ML is running under.
// It's really only needed for the interpreter.  In particular the pre-built
// compiler may be running under either byte order and has to check at
// run-time.
POLYUNSIGNED PolyIsBigEndian()
{
#ifdef WORDS_BIGENDIAN
    return TAGGED(1).AsUnsigned();
#else
    return TAGGED(0).AsUnsigned();
#endif
}

static struct _entrypts entryPtTable[] =
{
    { "PolyFullGC",                     (polyRTSFunction)&PolyFullGC},
    { "PolyIsBigEndian",                (polyRTSFunction)&PolyIsBigEndian},

    { NULL, NULL} // End of list.
};

// This RTS module is defined purely to allow us to print the statistics.
class RTS: public RtsModule
{
public:
    virtual void Stop(void);
    virtual entrypts GetRTSCalls(void) { return entryPtTable; }
};

static RTS rtsModule;

static const char * const rtsName[POLY_SYS_vecsize] =
{
    "RTS Call   0",
    "SYS_exit",
    "RTS Call   2",
    "RTS Call   3",
    "RTS Call   4",
    "RTS Call   5",
    "RTS Call   6",
    "RTS Call   7",
    "RTS Call   8",
    "SYS_chdir",
    "RTS Call  10",
    "SYS_alloc_store",
    "SYS_alloc_unit",
    "RTS Call  13",
    "SYS_raisex",
    "SYS_get_length",
    "RTS Call  16",
    "SYS_get_flags",
    "RTS Call  18",
    "RTS Call  19",
    "RTS Call  20",
    "RTS Call  21",
    "RTS Call  22",
    "SYS_str_compare",
    "RTS Call  24",
    "RTS Call  25",
    "SYS_teststrgtr",
    "SYS_teststrlss",
    "SYS_teststrgeq",
    "SYS_teststrleq",
    "RTS Call  30",
    "RTS Call  31",
    "SYS_exception_trace_fn",
    "SYS_give_ex_trace_fn",
    "RTS Call  34",
    "RTS Call  35",
    "RTS Call  36",
    "RTS Call  37",
    "RTS Call  38",
    "RTS Call  39",
    "RTS Call  40",
    "RTS Call  41",
    "RTS Call  42",
    "RTS Call  43",
    "RTS Call  44",
    "RTS Call  45",
    "RTS Call  46",
    "SYS_lockseg",
    "SYS_emptystring", // Not actually a call
    "SYS_nullvector",  // Not actually a call
    "RTS Call  50",
    "SYS_network",
    "SYS_os_specific",
    "SYS_eq_longword",
    "SYS_neq_longword",
    "SYS_geq_longword",
    "SYS_leq_longword",
    "SYS_gt_longword",
    "SYS_lt_longword",
    "RTS Call  59",
    "RTS Call  60",
    "SYS_io_dispatch",
    "SYS_signal_handler",
    "RTS Call  63",
    "RTS Call  64",
    "RTS Call  65",
    "RTS Call  66",
    "RTS Call  67",
    "RTS Call  68",
    "SYS_atomic_reset",
    "SYS_atomic_incr",
    "SYS_atomic_decr",
    "SYS_thread_self",
    "SYS_thread_dispatch",
    "SYS_plus_longword",
    "SYS_minus_longword",
    "SYS_mul_longword",
    "SYS_div_longword",
    "SYS_mod_longword",
    "SYS_andb_longword",
    "SYS_orb_longword",
    "SYS_xorb_longword",
    "RTS Call  82",
    "RTS Call  83",
    "SYS_kill_self",
    "SYS_shift_left_longword",
    "SYS_shift_right_longword",
    "SYS_shift_right_arith_longword",
    "SYS_profiler",
    "SYS_longword_to_tagged",
    "SYS_signed_to_longword",
    "SYS_unsigned_to_longword",
    "SYS_full_gc",
    "SYS_stack_trace",
    "SYS_timing_dispatch",
    "RTS Call  95",
    "RTS Call  96",
    "RTS Call  97",
    "RTS Call  98",
    "SYS_objsize",
    "SYS_showsize",
    "SYS_equal_short_arb",
    "RTS Call 102",
    "RTS Call 103",
    "SYS_quotrem",
    "SYS_is_short",
    "SYS_aplus",
    "SYS_aminus",
    "SYS_amul",
    "SYS_adiv",
    "SYS_amod",
    "SYS_aneg",
    "SYS_xora",
    "SYS_equala",
    "SYS_ora",
    "SYS_anda",
    "RTS Call 116",
    "SYS_Real_str",
    "SYS_Real_geq",
    "SYS_Real_leq",
    "SYS_Real_gtr",
    "SYS_Real_lss",
    "SYS_Real_eq",
    "SYS_Real_neq",
    "SYS_Real_Dispatch",
    "SYS_Add_real",
    "SYS_Sub_real",
    "SYS_Mul_real",
    "SYS_Div_real",
    "SYS_Abs_real",
    "SYS_Neg_real",
    "RTS Call 131",
    "SYS_Repr_real",
    "SYS_conv_real",
    "SYS_real_to_int",
    "SYS_int_to_real",
    "SYS_sqrt_real",
    "SYS_sin_real",
    "SYS_cos_real",
    "SYS_arctan_real",
    "SYS_exp_real",
    "SYS_ln_real",
    "SYS_fixed_to_real",
    "RTS Call 143",
    "RTS Call 144",
    "RTS Call 145",
    "RTS Call 146",
    "RTS Call 147",
    "SYS_stdin",    // Not actually a call
    "SYS_stdout",   // Not actually a call
    "SYS_process_env",
    "SYS_set_string_length",
    "SYS_get_first_long_word",
    "SYS_poly_specific",
    "SYS_bytevec_eq",
    "RTS Call 155",
    "RTS Call 156",
    "RTS Call 157",
    "RTS Call 158",
    "RTS Call 159",
    "SYS_cmem_load_8",
    "SYS_cmem_load_16",
    "SYS_cmem_load_32",
    "SYS_cmem_load_64",
    "SYS_cmem_load_float",
    "SYS_cmem_load_double",
    "SYS_cmem_store_8",
    "SYS_cmem_store_16",
    "SYS_cmem_store_32",
    "SYS_cmem_store_64",
    "SYS_cmem_store_float",
    "SYS_cmem_store_double",
    "RTS Call 172",
    "RTS Call 173",
    "RTS Call 174",
    "RTS Call 175",
    "RTS Call 176",
    "RTS Call 177",
    "RTS Call 178",
    "RTS Call 179",
    "SYS_fixed_add",
    "SYS_fixed_sub",
    "SYS_fixed_mul",
    "SYS_fixed_quot",
    "SYS_fixed_rem",
    "SYS_fixed_div",
    "SYS_fixed_mod",
    "RTS Call 187",
    "RTS Call 188",
    "SYS_io_operation",
    "SYS_ffi",
    "RTS Call 191",
    "RTS Call 192",
    "RTS Call 193",
    "SYS_set_code_constant",
    "SYS_move_words",
    "SYS_shift_right_arith_word",
    "SYS_int_to_word",
    "SYS_move_bytes",
    "SYS_move_bytes_overlap",
    "SYS_code_flags",
    "SYS_shrink_stack",
    "SYS_stderr",
    "RTS Call 203",
    "SYS_callcode_tupled",
    "SYS_foreign_dispatch",
    "SYS_foreign_null",
    "RTS Call 207",
    "RTS Call 208",
    "SYS_XWindows",
    "RTS Call 210",
    "RTS Call 211",
    "RTS Call 212",
    "SYS_is_big_endian",
    "SYS_bytes_per_word",
    "SYS_offset_address",
    "SYS_shift_right_word",
    "SYS_word_neq",
    "SYS_not_bool",
    "SYS_fixed_geq",
    "SYS_fixed_leq",
    "SYS_fixed_gtr",
    "SYS_fixed_lss",
    "SYS_string_length",
    "RTS Call 224",
    "RTS Call 225",
    "RTS Call 226",
    "RTS Call 227",
    "SYS_touch_final",
    "SYS_int_eq",
    "SYS_int_neq",
    "SYS_int_geq",
    "SYS_int_leq",
    "SYS_int_gtr",
    "SYS_int_lss",
    "SYS_load_byte_immut",
    "SYS_load_word_immut",
    "RTS Call 237",
    "SYS_mul_word",
    "SYS_plus_word",
    "SYS_minus_word",
    "SYS_div_word",
    "SYS_or_word",
    "SYS_and_word",
    "SYS_xor_word",
    "SYS_shift_left_word",
    "SYS_mod_word",
    "SYS_word_geq",
    "SYS_word_leq",
    "SYS_word_gtr",
    "SYS_word_lss",
    "SYS_word_eq",
    "SYS_load_byte",
    "SYS_load_word",
    "SYS_assign_byte",
    "SYS_assign_word"
};

void RTS::Stop()
{
    if (debugOptions & DEBUG_RTSCALLS)
    {
        for (unsigned i = 0; i < POLY_SYS_vecsize; i++)
        {
            if (rtsCallCounts[i] != 0)
                Log("RTS: %s called %lu times\n", rtsName[i], rtsCallCounts[i]);
        }
    }
}
