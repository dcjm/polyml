/*
    Title:  Machine dependent code for i386 and X64 under Windows and Unix

    Copyright (c) 2000-7
        Cambridge University Technical Services Limited

    Further work copyright David C. J. Matthews 2011-16

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

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <stdio.h>

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#ifdef HAVE_ASSERT_H 
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#ifdef HAVE_STRING_H 
#include <string.h>
#endif

#if (defined(_WIN32) && ! defined(__CYGWIN__))
#include <windows.h>
#include <excpt.h>
#endif

#include "globals.h"
#include "run_time.h"
#include "mpoly.h"
#include "arb.h"
#include "diagnostics.h"
#include "processes.h"
#include "sys.h"
#include "profiling.h"
#include "sighandler.h"
#include "machine_dep.h"
#include "scanaddrs.h"
#include "gc.h"
#include "check_objects.h"
#include "save_vec.h"
#include "memmgr.h"
#include "reals.h"
#include "polystring.h"
#include "xwindows.h"
#include "objsize.h"
#include "foreign.h"
#include "process_env.h"
#include "basicio.h"
#include "network.h"
#include "os_specific.h"
#include "poly_specific.h"
#include "timing.h"
#include "polyffi.h"
#include "rtsentry.h"


/**********************************************************************
 *
 * Register usage:
 *
 *  %Reax: First argument to function.  Result of function call.
 *  %Rebx: Second argument to function.
 *  %Recx: General register
 *  %Redx: Closure pointer in call.
 *  %Rebp: Points to memory used for extra registers
 *  %Resi: General register.
 *  %Redi: General register.
 *  %Resp: Stack pointer.
 *  The following apply only on the X64
 *  %R8:   Third argument to function
 *  %R9:   Fourth argument to function
 *  %R10:  Fifth argument to function
 *  %R11:  General register
 *  %R12:  General register
 *  %R13:  General register
 *  %R14:  General register
 *  %R15:  Memory allocation pointer

 *
 **********************************************************************/

#ifdef HOSTARCHITECTURE_X86_64
struct fpSaveArea {
    double fpregister[7]; // Save area for xmm0-6
};
#else
// Structure of floating point save area.
// This is dictated by the hardware.
typedef byte fpregister[10];

struct fpSaveArea {
    unsigned short cw;
    unsigned short _unused0;
    unsigned short sw;
    unsigned short _unused1;
    unsigned short tw;
    unsigned short _unused2;
    unsigned fip;
    unsigned short fcs0;
    unsigned short _unused3;
    unsigned foo;
    unsigned short fcs1;
    unsigned short _unused4;
    fpregister registers[8];
};
#endif

/* the amount of ML stack space to reserve for registers,
   C exception handling etc. The compiler requires us to
   reserve 2 stack-frames worth (2 * 20 words). We actually reserve
   slightly more than this.
*/
#if (!defined(_WIN32) && !defined(HAVE_SIGALTSTACK))
// If we can't handle signals on a separate stack make sure there's space
// on the Poly stack.
#define OVERFLOW_STACK_SIZE (50+1024)
#else
#define OVERFLOW_STACK_SIZE 50
#endif

class X86TaskData;

// This is passed as the argument vector to X86AsmSwitchToPoly.
// The offsets are built into the assembly code and the code-generator.
// localMpointer and stackPtr are updated before control returns to C.
typedef struct _AssemblyArgs {
    PolyWord        *localMpointer;     // Allocation ptr + 1 word
    PolyWord        *handlerRegister;   // Current exception handler
    PolyWord        *localMbottom;      // Base of memory + 1 word
    PolyWord        *stackLimit;        // Lower limit of stack
    PolyWord        exceptionPacket;    // Set if there is an exception
    byte            requestCode;        // IO function to call.
    byte            unusedFlag;         // No longer used
    byte            returnReason;       // Reason for returning from ML.
    byte            fullRestore;        // 0 => clear registers, 1 => reload registers
    StackObject     *polyStack;         // Current stack base
    PolyObject      *threadId;          // My thread id.  Saves having to call into RTS for it.
    PolyWord        *stackPtr;          // Current stack pointer
    POLYCODEPTR     programCtr;
    // Saved registers, where applicable.
    PolyWord        p_rax;
    PolyWord        p_rbx;
    PolyWord        p_rcx;
    PolyWord        p_rdx;
    PolyWord        p_rsi;
    PolyWord        p_rdi;
#ifdef HOSTARCHITECTURE_X86_64
    PolyWord        p_r8;
    PolyWord        p_r9;
    PolyWord        p_r10;
    PolyWord        p_r11;
    PolyWord        p_r12;
    PolyWord        p_r13;
    PolyWord        p_r14;
#endif
    struct fpSaveArea p_fp;
} AssemblyArgs;

class X86TaskData: public TaskData {
public:
    X86TaskData();
    unsigned allocReg; // The register to take the allocated space.
    POLYUNSIGNED allocWords; // The words to allocate.
    Handle callBackResult;
    AssemblyArgs assemblyInterface;

    virtual void GarbageCollect(ScanAddress *process);
    void ScanStackAddress(ScanAddress *process, PolyWord &val, StackSpace *stack);
    virtual Handle EnterPolyCode(); // Start running ML
    virtual void InterruptCode();
    virtual bool GetPCandSPFromContext(SIGNALCONTEXT *context, PolyWord *&sp, POLYCODEPTR &pc);
    virtual void InitStackFrame(TaskData *parentTask, Handle proc, Handle arg);
    virtual void SetException(poly_exn *exc);

    // Release a mutex in exactly the same way as compiler code
    virtual Handle AtomicIncrement(Handle mutexp);
    virtual void AtomicReset(Handle mutexp);

    // Return the minimum space occupied by the stack.  Used when setting a limit.
    virtual POLYUNSIGNED currentStackSpace(void) const { return (this->stack->top - assemblyInterface.stackPtr) + OVERFLOW_STACK_SIZE; }

    virtual void addAllocationProfileCount(POLYUNSIGNED words)
    { add_count(this, regPC(), regSP(), words); }

    // PreRTSCall: After calling from ML to the RTS we need to save the current heap pointer
    virtual void PreRTSCall(void) { SaveMemRegisters(); }
    // PostRTSCall: Before returning we need to restore the heap pointer.
    // If there has been a GC in the RTS call we need to create a new heap area.
    virtual void PostRTSCall(void) { SetMemRegisters(); }

    virtual void CopyStackFrame(StackObject *old_stack, POLYUNSIGNED old_length, StackObject *new_stack, POLYUNSIGNED new_length);

    virtual Handle EnterCallbackFunction(Handle func, Handle args);

    int SwitchToPoly();

    void HeapOverflowTrap();

    void SetMemRegisters();
    void SaveMemRegisters();

    PolyWord *get_reg(int n);

    PolyWord *&regSP() { return assemblyInterface.stackPtr; }
    POLYCODEPTR &regPC() { return assemblyInterface.programCtr; }

    PolyWord &regAX() { return assemblyInterface.p_rax; }
    PolyWord &regBX() { return assemblyInterface.p_rbx; }
    PolyWord &regCX() { return assemblyInterface.p_rcx; }
    PolyWord &regDX() { return assemblyInterface.p_rdx; }
    PolyWord &regSI() { return assemblyInterface.p_rsi; }
    PolyWord &regDI() { return assemblyInterface.p_rdi; }
#ifdef HOSTARCHITECTURE_X86_64
    PolyWord &reg8() { return assemblyInterface.p_r8; }
    PolyWord &reg9() { return assemblyInterface.p_r9; }
    PolyWord &reg10() { return assemblyInterface.p_r10; }
    PolyWord &reg11() { return assemblyInterface.p_r11; }
    PolyWord &reg12() { return assemblyInterface.p_r12; }
    PolyWord &reg13() { return assemblyInterface.p_r13; }
    PolyWord &reg14() { return assemblyInterface.p_r14; }
#endif
};

class X86Dependent: public MachineDependent {
public:
    X86Dependent() {}

    // Create a task data object.
    virtual TaskData *CreateTaskData(void) { return new X86TaskData(); }

    virtual unsigned InitialStackSize(void) { return 128+OVERFLOW_STACK_SIZE; } // Initial size of a stack 
    virtual void InitInterfaceVector(void);
    virtual void ScanConstantsWithinCode(PolyObject *addr, PolyObject *oldAddr, POLYUNSIGNED length, ScanAddress *process);

    virtual Architectures MachineArchitecture(void)
#ifndef HOSTARCHITECTURE_X86_64
         { return MA_I386; }
#else /* HOSTARCHITECTURE_X86_64 */
         { return MA_X86_64; }
#endif /* HOSTARCHITECTURE_X86_64 */
};

// Values for the returnReason byte
enum RETURN_REASON {
    RETURN_IO_CALL = 0,
    RETURN_HEAP_OVERFLOW,
    RETURN_STACK_OVERFLOW,
    RETURN_STACK_OVERFLOWEX,
    RETURN_RAISE_DIV_NOW_UNUSED,
    RETURN_ARB_EMULATION_NOW_UNUSED,
    RETURN_CALLBACK_RETURN,
    RETURN_CALLBACK_EXCEPTION,
    RETURN_RAISE_OVERFLOW
};

extern "C" {

    // These are declared in the assembly code segment.
    void X86AsmSwitchToPoly(void *);

    extern int X86AsmKillSelf(void);
    extern int X86AsmCallbackReturn(void);
    extern int X86AsmCallbackException(void);
    extern int X86AsmPopArgAndClosure(void);

    POLYUNSIGNED X86AsmAtomicIncrement(PolyObject*);
    POLYUNSIGNED X86AsmAtomicDecrement(PolyObject*);

    // The entry points to assembly code functions.
    extern byte CallPOLY_SYS_exit, alloc_store, alloc_uninit, raisex,
        get_length_a, get_flags, str_compare, teststrgtr, teststrlss,
        teststrgeq, teststrleq, set_exception_trace, locksega, CallPOLY_SYS_network,
        CallPOLY_SYS_os_specific, eq_longword, geq_longword, leq_longword, gt_longword,
        lt_longword,  CallPOLY_SYS_io_dispatch, CallPOLY_SYS_signal_handler, atomic_reset, atomic_increment,
        atomic_decrement, thread_self, CallPOLY_SYS_thread_dispatch, plus_longword, minus_longword,
        mul_longword, div_longword, mod_longword, andb_longword, orb_longword, xorb_longword,
        CallPOLY_SYS_kill_self, shift_left_longword, shift_right_longword, shift_right_arith_longword,
        CallPOLY_SYS_profiler, longword_to_tagged, signed_to_longword, unsigned_to_longword,
        CallPOLY_SYS_full_gc, CallPOLY_SYS_timing_dispatch,
        quotrem_long, is_shorta, add_long, sub_long, mult_long, div_long, rem_long,
        neg_long, xor_long, equal_long, or_long, and_long, CallPOLY_SYS_Real_str, real_geq, real_leq,
        real_gtr, real_lss, real_eq, real_neq, CallPOLY_SYS_Real_Dispatch, real_add, real_sub, real_mul,
        real_div, real_abs, real_neg, CallPOLY_SYS_conv_real, CallPOLY_SYS_real_to_int, real_from_int,
        CallPOLY_SYS_sqrt_real, CallPOLY_SYS_sin_real, CallPOLY_SYS_cos_real, CallPOLY_SYS_arctan_real,
        CallPOLY_SYS_exp_real, CallPOLY_SYS_ln_real, CallPOLY_SYS_process_env, set_string_length_a,
        get_first_long_word_a, CallPOLY_SYS_poly_specific, bytevec_eq, cmem_load_asm_8, cmem_load_asm_16,
        cmem_load_asm_32, cmem_load_asm_float, cmem_load_asm_double, cmem_store_asm_8, cmem_store_asm_16,
        cmem_store_asm_32,  cmem_store_asm_float,  cmem_store_asm_double,  CallPOLY_SYS_io_operation,
        CallPOLY_SYS_ffi,  move_words, CallPOLY_SYS_set_code_constant, move_words, shift_right_arith_word,
        int_to_word,  move_bytes,
        callcodeTupled, CallPOLY_SYS_foreign_dispatch, CallPOLY_SYS_XWindows, is_big_endian,
        bytes_per_word,  offset_address,  shift_right_word,  not_bool,  string_length,
        touch_final,  int_geq,  int_leq,  int_gtr,  int_lss,  mul_word, plus_word, minus_word, 
        div_word, or_word, and_word, xor_word, shift_left_word, mod_word, word_geq, word_leq,
        word_gtr, word_lss, word_eq, load_byte, load_word, assign_byte, assign_word,
        fixed_geq, fixed_leq, fixed_gtr, fixed_lss, fixed_add, fixed_sub, fixed_mul,
        fixed_quot, fixed_rem, fixed_to_real, fixed_div, fixed_mod, CallPOLY_SYS_get_entry_point;
#ifdef HOSTARCHITECTURE_X86_64
        extern byte cmem_load_asm_64, cmem_store_asm_64;
#endif
};

// This vector could perfectly well be in x86asm.asm except that it would be more
// complicated to make the code position independent.  Mac OS X wants PIC for dynamic
// libraries.
static byte *entryPointVector[256] =
{
    0,
    &CallPOLY_SYS_exit, // 1
    0, // 2 is unused
    0, // 3 is unused
    0, // 4 is unused
    0, // 5 is unused
    0, // 6 is unused
    0, // 7 is unused
    0, // 8 is unused
    0, // 9
    &CallPOLY_SYS_get_entry_point, // 10
    &alloc_store, // 11
    &alloc_uninit, // 12
    0, // 13 is unused
    &raisex, // raisex = 14
    &get_length_a, // 15
    0, // 16 is unused
    &get_flags, // 17
    0, // 18 is no longer used
    0, // 19 is no longer used
    0, // 20 is no longer used
    0, // 21 is unused
    0, // 22 is unused
    &str_compare, // 23
    0, // 24 is unused
    0, // 25 is unused
    &teststrgtr, // 26
    &teststrlss, // 27
    &teststrgeq, // 28
    &teststrleq, // 29
    0, // 30
    0, // 31 is no longer used
    &set_exception_trace, // 32 - backwards compatibility only
    0, // 33 - exception trace
    0, // 34 is no longer used
    0, // 35 is no longer used
    0, // 36 is no longer used
    0, // 37 is unused
    0, // 38 is unused
    0, // 39 is unused
    0, // 40 is unused
    0, // 41 is unused
    0, // 42
    0, // 43
    0, // 44 is no longer used
    0, // 45 is no longer used
    0, // 46
    &locksega, // 47
    0, // nullorzero = 48
    0, // 49 is no longer used
    0, // 50 is no longer used
    &CallPOLY_SYS_network, // 51
    &CallPOLY_SYS_os_specific, // 52
    &eq_longword, // 53
    0, // 54 is no longer used
    &geq_longword, // 55
    &leq_longword, // 56
    &gt_longword, // 57
    &lt_longword, // 58
    0, // 59 is unused
    0, // 60 is unused
    &CallPOLY_SYS_io_dispatch, // 61
    &CallPOLY_SYS_signal_handler, // 62
    0, // 63 is unused
    0, // 64 is unused
    0, // 65 is unused
    0, // 66 is unused
    0, // 67 is unused
    0, // 68 is unused
    &atomic_reset, // 69
    &atomic_increment, // 70
    &atomic_decrement, // 71
    &thread_self, // 72
    &CallPOLY_SYS_thread_dispatch, // 73
    &plus_longword, // 74
    &minus_longword, // 75
    &mul_longword, // 76
    &div_longword, // 77
    &mod_longword, // 78
    &andb_longword, // 79
    &orb_longword, // 80
    &xorb_longword, // 81
    0, // 82 is unused
    0, // 83 is now unused
    &CallPOLY_SYS_kill_self, // 84
    &shift_left_longword, // 85
    &shift_right_longword, // 86
    &shift_right_arith_longword, // 87
    &CallPOLY_SYS_profiler, // 88
    &longword_to_tagged, // 89
    &signed_to_longword, // 90
    &unsigned_to_longword, // 91
    &CallPOLY_SYS_full_gc, // 92
    0, // 93
    &CallPOLY_SYS_timing_dispatch, // 94
    0, // 95 is unused
    0, // 96 is unused
    0, // 97 is unused
    0, // 98 is unused
    0, // 99 now unused
    0, // 100 now unused
    &word_eq, // 101.  This can be the same as 251
    0, // 102 is unused
    0, // 103 is unused
    &quotrem_long, // 104
    &is_shorta, // 105
    &add_long, // 106
    &sub_long, // 107
    &mult_long, // 108
    &div_long, // 109
    &rem_long, // 110
    &neg_long, // 111
    &xor_long, // 112
    &equal_long, // 113
    &or_long, // 114
    &and_long, // 115
    0, // 116 is unused
    &CallPOLY_SYS_Real_str, // 117
    &real_geq, // 118
    &real_leq, // 119
    &real_gtr, // 120
    &real_lss, // 121
    &real_eq, // 122
    &real_neq, // 123
    &CallPOLY_SYS_Real_Dispatch, // 124
    &real_add, // 125
    &real_sub, // 126
    &real_mul, // 127
    &real_div, // 128
    &real_abs, // 129
    &real_neg, // 130
    0, // 131 is unused
    0, // 132 is no longer used
    &CallPOLY_SYS_conv_real, // 133
    &CallPOLY_SYS_real_to_int, // 134
    &real_from_int, // 135
    &CallPOLY_SYS_sqrt_real, // 136
    &CallPOLY_SYS_sin_real, // 137
    &CallPOLY_SYS_cos_real, // 138
    &CallPOLY_SYS_arctan_real, // 139
    &CallPOLY_SYS_exp_real, // 140
    &CallPOLY_SYS_ln_real, // 141
    &fixed_to_real, // 142
    0, // 143 is unused
    0, // 144 is unused
    0, // 145 is unused
    0, // 146 is unused
    0, // 147 is unused
    0, // stdin = 148
    0, // stdout = 149
    &CallPOLY_SYS_process_env, // 150
    &set_string_length_a, // 151
    &get_first_long_word_a, // 152
    &CallPOLY_SYS_poly_specific, // 153
    &bytevec_eq, // 154
    0, // 155 is unused
    0, // 156 is unused
    0, // 157 is unused
    0, // 158 is unused
    0, // 159 is unused
    &cmem_load_asm_8, // 160
    &cmem_load_asm_16, // 161
    &cmem_load_asm_32, // 162
#ifdef HOSTARCHITECTURE_X86_64
    &cmem_load_asm_64, // 163
#else
    0, // 163
#endif
    &cmem_load_asm_float, // 164
    &cmem_load_asm_double, // 165
    &cmem_store_asm_8, // 166
    &cmem_store_asm_16, // 167
    &cmem_store_asm_32, // 168
#ifdef HOSTARCHITECTURE_X86_64
    &cmem_store_asm_64, // 169
#else
    0, // 169
#endif
    &cmem_store_asm_float, // 170
    &cmem_store_asm_double, // 171
    0, // 172 is unused
    0, // 173 is unused
    0, // 174 is unused
    0, // 175 is unused
    0, // 176 is unused
    0, // 177 is unused
    0, // 178 is unused
    0, // 179 is unused
    &fixed_add, // 180
    &fixed_sub, // 181
    &fixed_mul, // 182
    &fixed_quot, // 183
    &fixed_rem, // 184
    &fixed_div, // 185
    &fixed_mod, // 186
    0, // 187 is unused
    0, // 188 is unused
    &CallPOLY_SYS_io_operation, // 189
    &CallPOLY_SYS_ffi, // 190
    0, // 191 is no longer used
    0, // 192 is unused
    &move_words, // move_words_overlap = 193
    &CallPOLY_SYS_set_code_constant, // 194
    &move_words, // 195
    &shift_right_arith_word, // 196
    &int_to_word, // 197
    &move_bytes, // 198
    &move_bytes, // move_bytes_overlap = 199
    0, // 200
    0, // 201
    0, // stderr = 202
    0, // 203 now unused
    &callcodeTupled, // 204
    &CallPOLY_SYS_foreign_dispatch, // 205
    0, // 206 - foreign null
    0, // 207 is unused
    0, // 208 now unused
    &CallPOLY_SYS_XWindows, // 209
    0, // 210 is unused
    0, // 211 is unused
    0, // 212 is unused
    &is_big_endian, // 213
    &bytes_per_word, // 214
    &offset_address, // 215
    &shift_right_word, // 216
    0, // 217 is no longer used
    &not_bool, // 218
    &fixed_geq, // 219
    &fixed_leq, // 220
    &fixed_gtr, // 221
    &fixed_lss, // 222
    &string_length, // 223
    0, // 224 is unused
    0, // 225 is unused
    0, // 226 is unused
    0, // 227 is unused
    &touch_final, // 228
    0, // 229 is no longer used
    0, // 230 is no longer used
    &int_geq, // 231
    &int_leq, // 232
    &int_gtr, // 233
    &int_lss, // 234
    &load_byte, // load_byte_immut = 235
    &load_word, // load_word_immut = 236
    0, // 237 is unused
    &mul_word, // 238
    &plus_word, // 239
    &minus_word, // 240
    &div_word, // 241
    &or_word, // 242
    &and_word, // 243
    &xor_word, // 244
    &shift_left_word, // 245
    &mod_word, // 246
    &word_geq, // 247
    &word_leq, // 248
    &word_gtr, // 249
    &word_lss, // 250
    &word_eq, // 251
    &load_byte, // 252
    &load_word, // 253
    &assign_byte, // 254
    &assign_word
};

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolySetCodeConstant(byte *pointer, PolyWord offset, POLYUNSIGNED c, PolyWord flags);
}

X86TaskData::X86TaskData(): allocReg(0), allocWords(0)
{
    assemblyInterface.fullRestore = 1; // To force the floating point to 64-bit
}

void X86TaskData::GarbageCollect(ScanAddress *process)
{
    TaskData::GarbageCollect(process); // Process the parent first
    assemblyInterface.threadId = threadObject;

    if (stack != 0)
    {
        // Now the values on the stack.
        for (PolyWord *q = assemblyInterface.stackPtr; q < stack->top; q++)
            ScanStackAddress(process, *q, stack);
    }
}

// Process a value within the stack.
void X86TaskData::ScanStackAddress(ScanAddress *process, PolyWord &val, StackSpace *stack)
{
    if (! val.IsDataPtr()) return;

    MemSpace *space = gMem.LocalSpaceForAddress(val.AsStackAddr()-1);
    if (space != 0)
        val = process->ScanObjectAddress(val.AsObjPtr());
}


// Copy a stack
void X86TaskData::CopyStackFrame(StackObject *old_stack, POLYUNSIGNED old_length, StackObject *new_stack, POLYUNSIGNED new_length)
{
    /* Moves a stack, updating all references within the stack */
    PolyWord *old_base  = (PolyWord *)old_stack;
    PolyWord *new_base  = (PolyWord*)new_stack;
    PolyWord *old_top   = old_base + old_length;

    /* Calculate the offset of the new stack from the old. If the frame is
       being extended objects in the new frame will be further up the stack
       than in the old one. */

    POLYSIGNED offset = new_base - old_base + new_length - old_length;

    PolyWord *oldStackPtr = assemblyInterface.stackPtr;

    // Adjust the stack pointer and handler pointer since these point into the stack.
    assemblyInterface.stackPtr = assemblyInterface.stackPtr + offset;
    assemblyInterface.handlerRegister    = assemblyInterface.handlerRegister + offset;

    // We need to adjust any values on the stack that are pointers within the stack.
    // Skip the unused part of the stack.

    POLYUNSIGNED i = oldStackPtr - old_base;

    ASSERT (i <= old_length);

    i = old_length - i;

    PolyWord *old = oldStackPtr;
    PolyWord *newp= assemblyInterface.stackPtr;

    while (i--)
    {
        PolyWord old_word = *old++;
        if (old_word.IsTagged() || old_word.AsStackAddr() < old_base || old_word.AsStackAddr() >= old_top)
            *newp++ = old_word;
        else
            *newp++ = PolyWord::FromStackAddr(old_word.AsStackAddr() + offset);
    }
    ASSERT(old == ((PolyWord*)old_stack)+old_length);
    ASSERT(newp == ((PolyWord*)new_stack)+new_length);
}

// Set code constant.  This can be a fast call.  The only reason it is in the RTS is
// to ensure that there is no possibility of a GC while the individual bytes are being
// copied.
// At the moment this assumes we're dealing with a 32-bit constant on a 32-bit machine
// and a 64-bit constant on a 64-bit machine.

POLYUNSIGNED PolySetCodeConstant(byte *pointer, PolyWord offset, POLYUNSIGNED c, PolyWord flags)
{
    // pointer is the start of the code segment.
    // c will usually be an address.
    // offset is a byte offset
    pointer += offset.UnTaggedUnsigned();
    if (flags == TAGGED(1))
        c -= (POLYUNSIGNED)(pointer + sizeof(PolyWord)); // Relative address.  Relative to AFTER the pointer.
    // Store the value into the code.  It can be on an arbitrary alignment.
    for (unsigned i = 0; i < sizeof(PolyWord); i++)
    {
        pointer[i] = (byte)(c & 255); 
        c >>= 8;
    }
    return TAGGED(0).AsUnsigned();
}

// Legacy RTS call version.
static Handle set_code_constant(TaskData *taskData, Handle data, Handle constant, Handle offseth, Handle base)
{
    POLYUNSIGNED offset = getPolyUnsigned(taskData, DEREFWORD(offseth)); // Byte offset
    byte *pointer = DEREFWORD(base).AsCodePtr() + offset;
    POLYUNSIGNED flags = UNTAGGED(DEREFWORD(data));
    POLYUNSIGNED c = DEREFWORD(constant).AsUnsigned(); // N.B.  This may well really be an address.
    if (flags == 1)
        c -= (POLYUNSIGNED)(pointer + sizeof(PolyWord)); // Relative address.  Relative to AFTER the pointer.
    // Store the value into the code.  It can be on an arbitrary alignment.
    for (unsigned i = 0; i < sizeof(PolyWord); i++)
    {
        pointer[i] = (byte)(c & 255); 
        c >>= 8;
    }
    return taskData->saveVec.push(TAGGED(0));
}

// IO Functions called indirectly from assembly code.
static void CallIO0(X86TaskData *taskData, Handle (*ioFun)(TaskData *))
{
    // Set the return address now.  This could be changed if an exception is raised.
    taskData->regPC() = (*taskData->regSP()).AsCodePtr();
    Handle result = (*ioFun)(taskData);
    taskData->regAX() = result->Word();
    taskData->regSP()++;
}

static void CallIO1(X86TaskData *taskData, Handle (*ioFun)(TaskData *, Handle))
{
    taskData->regPC() = (*taskData->regSP()).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(taskData->regAX());
    Handle result = (*ioFun)(taskData, saved1);
    taskData->regAX() = result->Word();
    taskData->regSP()++; // Pop the return address.
}

static void CallIO2(X86TaskData *taskData, Handle (*ioFun)(TaskData *, Handle, Handle))
{
    taskData->regPC() = (*taskData->regSP()).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(taskData->regAX());
    Handle saved2 = taskData->saveVec.push(taskData->regBX());
    Handle result = (*ioFun)(taskData, saved2, saved1);
    taskData->regAX() = result->Word();
    taskData->regSP()++;
}

static void CallIO3(X86TaskData *taskData, Handle (*ioFun)(TaskData *, Handle, Handle, Handle))
{
    taskData->regPC() = (*taskData->regSP()).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(taskData->regAX());
    Handle saved2 = taskData->saveVec.push(taskData->regBX());
#ifndef HOSTARCHITECTURE_X86_64
    Handle saved3 = taskData->saveVec.push(taskData->regSP()[1]);
#else /* HOSTARCHITECTURE_X86_64 */
    Handle saved3 = taskData->saveVec.push(taskData->reg8());
#endif /* HOSTARCHITECTURE_X86_64 */
    Handle result = (*ioFun)(taskData, saved3, saved2, saved1);
    taskData->regAX() = result->Word();
#ifndef HOSTARCHITECTURE_X86_64
    taskData->regSP() += 2; // Pop the return address and a stack arg.
#else /* HOSTARCHITECTURE_X86_64 */
    taskData->regSP()++;
#endif /* HOSTARCHITECTURE_X86_64 */
}

static void CallIO4(X86TaskData *taskData, Handle (*ioFun)(TaskData *, Handle, Handle, Handle, Handle))
{
    taskData->regPC() = (*taskData->regSP()).AsCodePtr();
    Handle saved1 = taskData->saveVec.push(taskData->regAX());
    Handle saved2 = taskData->saveVec.push(taskData->regBX());
#ifndef HOSTARCHITECTURE_X86_64
    Handle saved3 = taskData->saveVec.push(taskData->regSP()[2]);
    Handle saved4 = taskData->saveVec.push(taskData->regSP()[1]);
#else /* HOSTARCHITECTURE_X86_64 */
    Handle saved3 = taskData->saveVec.push(taskData->reg8());
    Handle saved4 = taskData->saveVec.push(taskData->reg9());
#endif /* HOSTARCHITECTURE_X86_64 */
    Handle result = (*ioFun)(taskData, saved4, saved3, saved2, saved1);
    taskData->regAX() = result->Word();
#ifndef HOSTARCHITECTURE_X86_64
    taskData->regSP() += 3; // Pop the return address and two stack args.
#else /* HOSTARCHITECTURE_X86_64 */
    taskData->regSP()++;
#endif /* HOSTARCHITECTURE_X86_64 */
}

Handle X86TaskData::EnterPolyCode()
/* Called from "main" to enter the code. */
{
    Handle hOriginal = this->saveVec.mark(); // Set this up for the IO calls.
    while (1)
    {
        this->saveVec.reset(hOriginal); // Remove old RTS arguments and results.

        // Run the ML code and return with the function to call.
        this->inML = true;
        int ioFunction = SwitchToPoly();
        this->inML = false;

        if ((debugOptions & DEBUG_RTSCALLS))
            IncrementRTSCallCount(ioFunction);

        try {
            switch (ioFunction)
            {
            case -1:
                // We've been interrupted.  This usually involves simulating a
                // stack overflow so we could come here because of a genuine
                // stack overflow.
                // Previously this code was executed on every RTS call but there
                // were problems on Mac OS X at least with contention on schedLock.
                this->pendingInterrupt = false; // Clear this before we handle these
                // Process any asynchronous events i.e. interrupts or kill
                processes->ProcessAsynchRequests(this);
                // Release and re-acquire use of the ML memory to allow another thread
                // to GC.
                processes->ThreadReleaseMLMemory(this);
                processes->ThreadUseMLMemory(this);
                break;

            case -2: // A callback has returned.
                return callBackResult; // Return the saved value. Not used in the new interface.

            case POLY_SYS_exit:
                CallIO1(this, &finishc);
                break;

            case POLY_SYS_alloc_store:
                CallIO3(this, &alloc_store_long_c);
                break;

            case POLY_SYS_get_entry_point:
                CallIO1(this, &creatEntryPointObject);
                break;

            case POLY_SYS_profiler:
                CallIO1(this, &profilerc);
                break;

            case POLY_SYS_quotrem:
                CallIO3(this, &quot_rem_c);
                break;

            case POLY_SYS_aplus:
                CallIO2(this, &add_longc);
                break;

            case POLY_SYS_aminus:
                CallIO2(this, &sub_longc);
                break;

            case POLY_SYS_amul:
                CallIO2(this, &mult_longc);
                break;

            case POLY_SYS_adiv:
                CallIO2(this, &div_longc);
                break;

            case POLY_SYS_amod:
                CallIO2(this, &rem_longc);
                break;

            case POLY_SYS_aneg:
                CallIO1(this, &neg_longc);
                break;

            case POLY_SYS_equala:
                CallIO2(this, &equal_longc);
                break;

            case POLY_SYS_ora:
                CallIO2(this, &or_longc);
                break;

            case POLY_SYS_anda:
                CallIO2(this, &and_longc);
                break;

            case POLY_SYS_xora:
                CallIO2(this, &xor_longc);
                break;

            case POLY_SYS_Real_str:
                CallIO3(this, &Real_strc);
                break;

            case POLY_SYS_Real_Dispatch:
                CallIO2(this, &Real_dispatchc);
                break;

            case POLY_SYS_conv_real:
                CallIO1(this, &Real_convc);
                break;

            case POLY_SYS_real_to_int:
                CallIO1(this, &Real_intc);
                break;

            case POLY_SYS_int_to_real:
                CallIO1(this, &Real_from_arbitrary_precision);
                break;

            case POLY_SYS_sqrt_real:
                CallIO1(this, &Real_sqrtc);
                break;

            case POLY_SYS_sin_real:
                CallIO1(this, &Real_sinc);
                break;

            case POLY_SYS_cos_real:
                CallIO1(this, &Real_cosc);
                break;

            case POLY_SYS_arctan_real:
                CallIO1(this, &Real_arctanc);
                break;

            case POLY_SYS_exp_real:
                CallIO1(this, &Real_expc);
                break;

            case POLY_SYS_ln_real:
                CallIO1(this, &Real_lnc);
                break;

            case POLY_SYS_io_operation:
                CallIO1(this, &io_operation_c);
                break;

            case POLY_SYS_thread_dispatch:
                CallIO2(this, &ThreadDispatch);
                break;

            case POLY_SYS_int_geq:
                CallIO2(this, &ge_longc);
                break;

            case POLY_SYS_int_leq:
                CallIO2(this, &le_longc);
                break;

            case POLY_SYS_int_gtr:
                CallIO2(this, &gt_longc);
                break;

            case POLY_SYS_int_lss:
                CallIO2(this, &ls_longc);
                break;

            case POLY_SYS_timing_dispatch:
                CallIO2(this, &timing_dispatch_c);
                break;

            case POLY_SYS_XWindows:
                CallIO1(this, &XWindows_c);
                break;

            case POLY_SYS_full_gc:
                CallIO0(this, &full_gc_c);
                break;

            case POLY_SYS_foreign_dispatch:
                CallIO2(this, &foreign_dispatch_c);
                break;

            case POLY_SYS_ffi:
                CallIO2(this, &poly_ffi);
                break;

            case POLY_SYS_process_env:
                CallIO2(this, &process_env_dispatch_c);
                break;

            case POLY_SYS_poly_specific:
                CallIO2(this, &poly_dispatch_c);
                break;

            case POLY_SYS_set_code_constant:
                CallIO4(this, &set_code_constant);
                break;

            case POLY_SYS_io_dispatch:
                CallIO3(this, &IO_dispatch_c);
                break;

            case POLY_SYS_network:
                CallIO2(this, &Net_dispatch_c);
                break;

            case POLY_SYS_os_specific:
                CallIO2(this, &OS_spec_dispatch_c);
                break;

            case POLY_SYS_signal_handler:
                CallIO2(this, &Sig_dispatch_c);
                break;

            case POLY_SYS_kill_self:
                CallIO0(this, exitThread);
                break;

            default:
                Crash("Unknown io operation %d\n", ioFunction);
            }
        }
        catch (IOException &) {
        }
    }
}

// Run the current ML process.  X86AsmSwitchToPoly saves the C state so that
// whenever the ML requires assistance from the rest of the RTS it simply
// returns to C with the appropriate values set in assemblyInterface.requestCode and
// 

int X86TaskData::SwitchToPoly()
// (Re)-enter the Poly code from C.  Returns with the io function to call or
// -1 if we are responding to an interrupt.
{
    Handle mark = this->saveVec.mark();
    do
    {
        this->saveVec.reset(mark); // Remove old data e.g. from arbitrary precision.
        SetMemRegisters();

        X86AsmSwitchToPoly(&this->assemblyInterface);

        SaveMemRegisters(); // Update globals from the memory registers.

        // Handle any heap/stack overflows or arbitrary precision traps.
        switch (this->assemblyInterface.returnReason)
        {

        case RETURN_IO_CALL:
            return this->assemblyInterface.requestCode;

        case RETURN_HEAP_OVERFLOW:
            // The heap has overflowed.  Pop the return address into the program counter.
            // It may well not be a valid code address anyway.
            regPC() = (*(regSP())++).AsCodePtr();
            this->HeapOverflowTrap(); // Computes a value for allocWords only
            break;

        case RETURN_STACK_OVERFLOW:
            try {
                // The stack check has failed.  This may either be because we really have
                // overflowed the stack or because the stack limit value has been adjusted
                // to result in a call here.
                regPC() = (*regSP()++).AsCodePtr();
                POLYUNSIGNED min_size = this->stack->top - regSP() + OVERFLOW_STACK_SIZE;
                CheckAndGrowStack(this, min_size);
            }
            catch (IOException &) {
               // We may get an exception while handling this if we run out of store
            }
            return -1; // We're in a safe state to handle any interrupts.

        case RETURN_STACK_OVERFLOWEX:
            try {
                // Stack limit overflow.  If the required stack space is larger than
                // the fixed overflow size the code will calculate the limit in %EDI.
                // We need to extract that and the clear that register since it may
                // very well be outside the stack and therefore a "bad address".
                PolyWord *stackP = regDI().AsStackAddr();
                regDI() = TAGGED(0);
                regPC() = (*regSP()++).AsCodePtr();
                POLYUNSIGNED min_size = this->stack->top - stackP + OVERFLOW_STACK_SIZE;
                CheckAndGrowStack(this, min_size);
            }
            catch (IOException &) {
               // We may get an exception while handling this if we run out of store
            }
            return -1; // We're in a safe state to handle any interrupts.

        case RETURN_RAISE_OVERFLOW:
            try {
                // The primary reason for providing this is to allow the assembly code to
                // raise the overflow exception.  It's quite difficult for it to make an
                // exception packet otherwise.  It also provides a way to raise the
                // exception from compiled code.
                regPC() = (*regSP()++).AsCodePtr();
                raise_exception0(this, EXC_overflow);
            }
            catch (IOException &) {
                // Handle the C++ exception.
            }
            break;

        case RETURN_CALLBACK_RETURN:
            // Remove the extra exception handler we created in EnterCallbackFunction
            ASSERT(assemblyInterface.handlerRegister == regSP());
            regSP() += 1;
            assemblyInterface.handlerRegister = (*(regSP()++)).AsStackAddr(); // Restore the previous handler.
            this->callBackResult = this->saveVec.push(regAX()); // Argument to return is in RAX.
            regPC() = (*regSP()).AsCodePtr(); // Set the return address
            return -2;

        case RETURN_CALLBACK_EXCEPTION:
            // An ML callback has raised an exception.
            // It isn't possible to do anything here except abort.
            Crash("An ML function called from foreign code raised an exception.  Unable to continue.");

        default:
            Crash("Unknown return reason code %u", this->assemblyInterface.returnReason);
        }

    } while (1);
}

void X86TaskData::InitStackFrame(TaskData *parentTaskData, Handle proc, Handle arg)
/* Initialise stack frame. */
{
    StackSpace *space = this->stack;
    StackObject * newStack = space->stack();
    POLYUNSIGNED stack_size     = space->spaceSize();
    POLYUNSIGNED topStack = stack_size-5;
    assemblyInterface.stackPtr = (PolyWord*)newStack+topStack; 
    assemblyInterface.handlerRegister    = (PolyWord*)newStack+topStack+3;

    assemblyInterface.programCtr = (byte*)&X86AsmPopArgAndClosure;

    // Floating point save area.
    memset(&assemblyInterface.p_fp, 0, sizeof(struct fpSaveArea));
#ifndef HOSTARCHITECTURE_X86_64
    // Set the control word for 64-bit precision otherwise we get inconsistent results.
    assemblyInterface.p_fp.cw = 0x027f ; // Control word
    assemblyInterface.p_fp.tw = 0xffff; // Tag registers - all unused
#endif

    ((PolyWord*)newStack)[topStack] = DEREFWORDHANDLE(proc); // Closure
    ((PolyWord*)newStack)[topStack+1] = (arg == 0) ? TAGGED(0) : DEREFWORD(arg); // Argument
    /* We initialise the end of the stack with a sequence that will jump to
       kill_self whether the process ends with a normal return or by raising an
       exception.  A bit of this was added to fix a bug when stacks were objects
       on the heap and could be scanned by the GC. */
    ((PolyWord*)newStack)[topStack+4] = TAGGED(0); // Probably no longer needed
    // Set the default handler and return address to point to this code.
    PolyWord killJump(PolyWord::FromCodePtr((byte*)&X86AsmKillSelf));
    // Exception handler.
    ((PolyWord*)newStack)[topStack+3] = killJump;
    // Normal return address.  We need a separate entry on the stack from
    // the exception handler because it is possible that the code we are entering
    // may replace this entry with an argument.  The code-generator optimises tail-recursive
    // calls to functions with more args than the called function.
    ((PolyWord*)newStack)[topStack+2] = killJump;
}

// In Solaris-x86 the registers are named EIP and ESP.
#if (!defined(REG_EIP) && defined(EIP))
#define REG_EIP EIP
#endif
#if (!defined(REG_ESP) && defined(ESP))
#define REG_ESP ESP
#endif


// Get the PC and SP(stack) from a signal context.  This is needed for profiling.
// This version gets the actual sp and pc if we are in ML.
bool X86TaskData::GetPCandSPFromContext(SIGNALCONTEXT *context, PolyWord * &sp, POLYCODEPTR &pc)
{
    if (context == 0) return false;
// The tests for HAVE_UCONTEXT_T, HAVE_STRUCT_SIGCONTEXT and HAVE_WINDOWS_H need
// to follow the tests in processes.h.
#if defined(HAVE_WINDOWS_H)
#ifdef _WIN64
    sp = (PolyWord *)context->Rsp;
    pc = (POLYCODEPTR)context->Rip;
#else
    // Windows 32 including cygwin.
    sp = (PolyWord *)context->Esp;
    pc = (POLYCODEPTR)context->Eip;
#endif
#elif defined(HAVE_UCONTEXT_T)
#ifdef HAVE_MCONTEXT_T_GREGS
    // Linux
#ifndef HOSTARCHITECTURE_X86_64
    pc = (byte*)context->uc_mcontext.gregs[REG_EIP];
    sp = (PolyWord*)context->uc_mcontext.gregs[REG_ESP];
#else /* HOSTARCHITECTURE_X86_64 */
    pc = (byte*)context->uc_mcontext.gregs[REG_RIP];
    sp = (PolyWord*)context->uc_mcontext.gregs[REG_RSP];
#endif /* HOSTARCHITECTURE_X86_64 */
#elif defined(HAVE_MCONTEXT_T_MC_ESP)
   // FreeBSD
#ifndef HOSTARCHITECTURE_X86_64
    pc = (byte*)context->uc_mcontext.mc_eip;
    sp = (PolyWord*)context->uc_mcontext.mc_esp;
#else /* HOSTARCHITECTURE_X86_64 */
    pc = (byte*)context->uc_mcontext.mc_rip;
    sp = (PolyWord*)context->uc_mcontext.mc_rsp;
#endif /* HOSTARCHITECTURE_X86_64 */
#else
   // Mac OS X
#ifndef HOSTARCHITECTURE_X86_64
#if(defined(HAVE_STRUCT_MCONTEXT_SS)||defined(HAVE_STRUCT___DARWIN_MCONTEXT32_SS))
    pc = (byte*)context->uc_mcontext->ss.eip;
    sp = (PolyWord*)context->uc_mcontext->ss.esp;
#elif(defined(HAVE_STRUCT___DARWIN_MCONTEXT32___SS))
    pc = (byte*)context->uc_mcontext->__ss.__eip;
    sp = (PolyWord*)context->uc_mcontext->__ss.__esp;
#else
    return false;
#endif
#else /* HOSTARCHITECTURE_X86_64 */
#if(defined(HAVE_STRUCT_MCONTEXT_SS)||defined(HAVE_STRUCT___DARWIN_MCONTEXT64_SS))
    pc = (byte*)context->uc_mcontext->ss.rip;
    sp = (PolyWord*)context->uc_mcontext->ss.rsp;
#elif(defined(HAVE_STRUCT___DARWIN_MCONTEXT64___SS))
    pc = (byte*)context->uc_mcontext->__ss.__rip;
    sp = (PolyWord*)context->uc_mcontext->__ss.__rsp;
#else
    return false;
#endif
#endif /* HOSTARCHITECTURE_X86_64 */
#endif
#elif defined(HAVE_STRUCT_SIGCONTEXT)
#if defined(HOSTARCHITECTURE_X86_64) && defined(__OpenBSD__)
    // CPP defines missing in amd64/signal.h in OpenBSD
    pc = (byte*)context->sc_rip;
    sp = (PolyWord*)context->sc_rsp;
#else // !HOSTARCHITEXTURE_X86_64 || !defined(__OpenBSD__)
    pc = (byte*)context->sc_pc;
    sp = (PolyWord*)context->sc_sp;
#endif
#else
    // Can't get context.
    return false;
#endif
    // Check the sp value is in the current stack.
    if (sp >= this->stack->bottom && sp < this->stack->top)
        return true;
    else
        return false; // Bad stack pointer
}

void X86TaskData::InterruptCode()
{
    // Set the stack limit pointer to the top of the stack to cause
    // a trap when we next check for stack overflow.
    // SetMemRegisters actually does this anyway if "pendingInterrupt" is set but
    // it's safe to do this repeatedly.
    if (this->stack != 0) 
        this->assemblyInterface.stackLimit = this->stack->top-1;
    this->pendingInterrupt = true;
}

// This is called from SwitchToPoly before we enter the ML code.
void X86TaskData::SetMemRegisters()
{
    // Copy the current store limits into variables before we go into the assembly code.

    // If we haven't yet set the allocation area or we don't have enough we need
    // to create one (or a new one).
    if (this->allocPointer <= this->allocLimit + this->allocWords)
    {
        if (this->allocPointer < this->allocLimit)
            Crash ("Bad length in heap overflow trap");

        // Find some space to allocate in.  Updates taskData->allocPointer and
        // returns a pointer to the newly allocated space (if allocWords != 0)
        PolyWord *space =
            processes->FindAllocationSpace(this, this->allocWords, true);
        if (space == 0)
        {
            // We will now raise an exception instead of returning.
            // Set allocWords to zero so we don't set the allocation register
            // since that could be holding the exception packet.
            this->allocWords = 0;
        }
        // Undo the allocation just now.
        this->allocPointer += this->allocWords;
    }

    if (this->allocWords != 0)
    {
        // If we have had a heap trap we actually do the allocation here.
        // We will have already garbage collected and recovered sufficient space.
        // This also happens if we have just trapped because of store profiling.
        this->allocPointer -= this->allocWords; // Now allocate
        // Set the allocation register to this area.
        if (this->allocReg < 15)
            *(get_reg(this->allocReg)) =
                PolyWord::FromStackAddr(this->allocPointer + 1); /* remember: it's off-by-one */
        this->allocWords = 0;
    }

    // If we have run out of store, either just above or while allocating in the RTS,
    // allocPointer and allocLimit will have been set to zero as part of the GC.  We will
    // now be raising an exception which may free some store but we need to come back here
    // before we allocate anything.  The compiled code uses unsigned arithmetic to check for
    // heap overflow but only after subtracting the space required.  We need to make sure
    // that the values are still non-negative after substracting any object size.
    if (this->allocPointer == 0) this->allocPointer += MAX_OBJECT_SIZE;
    if (this->allocLimit == 0) this->allocLimit += MAX_OBJECT_SIZE;

    this->assemblyInterface.localMbottom = this->allocLimit + 1;
    this->assemblyInterface.localMpointer = this->allocPointer + 1;
    // If we are profiling store allocation we set mem_hl so that a trap
    // will be generated.
    if (profileMode == kProfileStoreAllocation)
        this->assemblyInterface.localMbottom = this->assemblyInterface.localMpointer;

    this->assemblyInterface.polyStack = this->stack->stack();
    // Whenever the ML code enters a function it checks that the stack pointer is above
    // this value.  The default is to set it to the top of the reserved area
    // but if we've had an interrupt we set it to the end of the stack.
    // InterruptCode may be called either when the thread is in the RTS or in ML code.
    if (this->pendingInterrupt) this->assemblyInterface.stackLimit = this->stack->top - 1;
    else this->assemblyInterface.stackLimit = this->stack->bottom + OVERFLOW_STACK_SIZE;
    this->assemblyInterface.requestCode = 0; // Clear these because only one will be set.
    this->assemblyInterface.returnReason = RETURN_IO_CALL;

    this->assemblyInterface.threadId = this->threadObject;
}

// This is called whenever we have returned from ML to C.
void X86TaskData::SaveMemRegisters()
{
    this->allocPointer = this->assemblyInterface.localMpointer - 1;
    this->allocWords = 0;
    // We need to restore all the registers if we are emulating an instruction or
    // are handling a heap or stack overflow.  For the moment we just consider
    // all cases apart from an RTS call.
    this->assemblyInterface.fullRestore = this->assemblyInterface.returnReason != 0 ? 1 : 0;
    this->assemblyInterface.exceptionPacket = TAGGED(0);
}

PolyWord *X86TaskData::get_reg(int n)
/* Returns a pointer to the register given by n. */
{
    switch (n) 
    {
    case 0: return &assemblyInterface.p_rax;
    case 1: return &assemblyInterface.p_rcx;
    case 2: return &assemblyInterface.p_rdx;
    case 3: return &assemblyInterface.p_rbx;
        // Should not have rsp or rbp.
    case 6: return &assemblyInterface.p_rsi;
    case 7: return &assemblyInterface.p_rdi;
#ifdef HOSTARCHITECTURE_X86_64
    case 8: return &assemblyInterface.p_r8;
    case 9: return &assemblyInterface.p_r9;
    case 10: return &assemblyInterface.p_r10;
    case 11: return &assemblyInterface.p_r11;
    case 12: return &assemblyInterface.p_r12;
    case 13: return &assemblyInterface.p_r13;
    case 14: return &assemblyInterface.p_r14;
    // R15 is the heap pointer so shouldn't occur here.
#endif /* HOSTARCHITECTURE_X86_64 */
    default: Crash("Unknown register %d at %p\n", n, regPC());
    }
}

// Called as a result of a heap overflow trap
void X86TaskData::HeapOverflowTrap()
{
    X86TaskData *mdTask = this;
    POLYUNSIGNED wordsNeeded = 0;
    // The next instruction, after any branches round forwarding pointers or pop
    // instructions, will be a store of register containing the adjusted heap pointer.
    // We need to find that register and the value in it in order to find out how big
    // the area we actually wanted is.  N.B.  The code-generator and assembly code
    // must generate the correct instruction sequence.
    byte *pcPtr = assemblyInterface.programCtr;
    while (true)
    {
        if (pcPtr[0] == 0xeb)
        {
            // Forwarding pointer
            if (pcPtr[1] >= 128) pcPtr += 256 - pcPtr[1] + 2;
            else pcPtr += pcPtr[1] + 2;
        }
        else if ((pcPtr[0] & 0xf8) == 0x58) // Pop instruction.
            pcPtr++;
        else if (pcPtr[0] == 0x41 && ((pcPtr[1] & 0xf8) == 0x58)) // Pop with Rex prefix
            pcPtr += 2;
        else break;
    }
#ifndef HOSTARCHITECTURE_X86_64
    // This should be movl REG,0[%ebp].
    ASSERT(pcPtr[0] == 0x89);
    mdTask->allocReg = (pcPtr[1] >> 3) & 7; // Remember this until we allocate the memory
    PolyWord *reg = get_reg(mdTask->allocReg);
    PolyWord reg_val = *reg;
    // The space we need is the difference between this register
    // and the current value of newptr.
    // The +1 here is because assemblyInterface.localMpointer is A.M.pointer +1.  The reason
    // is that after the allocation we have the register pointing at the address we will
    // actually use.
    wordsNeeded = (this->allocPointer - (PolyWord*)reg_val.AsAddress()) + 1;
    *reg = TAGGED(0); // Clear this - it's not a valid address.
    /* length in words, including length word */

    ASSERT (wordsNeeded <= (1<<24)); /* Max object size including length/flag word is 2^24 words.  */
#else /* HOSTARCHITECTURE_X86_64 */
    ASSERT(pcPtr[1] == 0x89 || pcPtr[1] == 0x8b);
    if (pcPtr[1] == 0x89)
    {
        // New (5.4) format.  This should be movq REG,%r15
        ASSERT(pcPtr[0] == 0x49 || pcPtr[0] == 0x4d);
        mdTask->allocReg = (pcPtr[2] >> 3) & 7; // Remember this until we allocate the memory
        if (pcPtr[0] & 0x4) mdTask->allocReg += 8;
    }
    else
    {
        // Alternative form of movq REG,%r15
        ASSERT(pcPtr[0] == 0x4c || pcPtr[0] == 0x4d);
        mdTask->allocReg = pcPtr[2] & 7; // Remember this until we allocate the memory
        if (pcPtr[0] & 0x1) mdTask->allocReg += 8;
    }
    PolyWord *reg = get_reg(this->allocReg);
    PolyWord reg_val = *reg;
    wordsNeeded = (this->allocPointer - (PolyWord*)reg_val.AsAddress()) + 1;
    *reg = TAGGED(0); // Clear this - it's not a valid address.
 #endif /* HOSTARCHITECTURE_X86_64 */
    if (profileMode == kProfileStoreAllocation)
        addAllocationProfileCount(wordsNeeded);

    mdTask->allocWords = wordsNeeded; // The actual allocation is done in SetMemRegisters.
}

// These macros build small pieces of assembly code for each io call.
// The code simply sets the requestCode value and jumps to
// X86AsmSaveStateAndReturn.  The address of these code pieces is
// stored in iovec.  Values in iovec are never looked at with the
// garbage collector so that's safe.

void X86Dependent::InitInterfaceVector(void)
{
    for (int i = 0; i < POLY_SYS_vecsize; i++)
    {
        if (entryPointVector[i] != 0)
            add_word_to_io_area(i, PolyWord::FromCodePtr(entryPointVector[i]));
    }
}

void X86TaskData::SetException(poly_exn *exc)
// Set up the stack to raise an exception.
{
    regDX() = (PolyObject*)IoEntry(POLY_SYS_raisex);
    regPC()  = regDX().AsObjPtr()->Get(0).AsCodePtr();
    regAX() = exc; /* put exception data into eax */
    assemblyInterface.exceptionPacket = exc; // Set for direct calls.
}

// Sets up a callback function on the current stack.  The present state is that
// the ML code has made a call in to foreign_dispatch.  We need to set the stack
// up so that we will enter the callback (as with CallCodeTupled) but when we return
// the result we enter callback_return. 
Handle X86TaskData::EnterCallbackFunction(Handle func, Handle args)
{
    // If we ever implement a light version of the FFI that allows a call to C
    // code without saving enough to allow allocation in C code we need to ensure
    // that this code doesn't do any allocation.  Essentially we need the values
    // in localMpointer and localMbottom to be valid across a call to C.  If we do
    // a callback the ML callback function would pick up the values saved in the
    // originating call.

    // Set up an exception handler so we will enter callBackException if there is an exception.
    *(--regSP()) = PolyWord::FromStackAddr(assemblyInterface.handlerRegister); // Create a special handler entry
    *(--regSP()) = PolyWord::FromCodePtr((byte*)&X86AsmCallbackException);
    assemblyInterface.handlerRegister = regSP();
    // Push the call to callBackReturn onto the stack as the return address.
    *(--regSP()) = PolyWord::FromCodePtr((byte*)&X86AsmCallbackReturn);
    // Set up the entry point of the callback.
    PolyObject *functToCall = func->WordP();
    regDX() = functToCall; // Closure address
    regAX() = args->Word();
    regPC() = functToCall->Get(0).AsCodePtr(); // First word of closure is entry pt.

    return EnterPolyCode();
}

// Decode and process an effective address.  There may
// be a constant address in here but in any case we need
// to decode it to work out where the next instruction starts.
// If this is an lea instruction any addresses are just constants
// so must not be treated as addresses.
static void skipea(byte **pt, ScanAddress *process, bool lea)
{
    unsigned int modrm = *((*pt)++);
    unsigned int md = modrm >> 6;
    unsigned int rm = modrm & 7;

    if (md == 3) { } /* Register. */
    else if (rm == 4)
    {
        /* s-i-b present. */
        unsigned int sib = *((*pt)++);

        if (md == 0)
        {
            if ((sib & 7) == 5) 
            {
                if (! lea) {
#ifndef HOSTARCHITECTURE_X86_64
                    process->ScanConstant(*pt, PROCESS_RELOC_DIRECT);
#endif /* HOSTARCHITECTURE_X86_64 */
                }
                (*pt) += 4;
            }
        }
        else if (md == 1) (*pt)++;
        else if (md == 2) (*pt) += 4;
    }
    else if (md == 0 && rm == 5)
    {
        if (!lea) {
#ifndef HOSTARCHITECTURE_X86_64
            /* Absolute address. */
            process->ScanConstant(*pt, PROCESS_RELOC_DIRECT);
#endif /* HOSTARCHITECTURE_X86_64 */
        }
        *pt += 4;
    }
    else
    {
        if (md == 1) *pt += 1;
        else if (md == 2) *pt += 4;
    }
}

/* Added to deal with constants within the
   code rather than in the constant area.  The constant
   area is still needed for the function name.
   DCJM 2/1/2001 
*/
void X86Dependent::ScanConstantsWithinCode(PolyObject *addr, PolyObject *old, POLYUNSIGNED length, ScanAddress *process)
{
    byte *pt = (byte*)addr;
    PolyWord *end = addr->Offset(length - 1);

    while (true)
    {
        // We've finished when this is word aligned and points to a zero word.
        if (((POLYUNSIGNED)pt & (0-sizeof(POLYUNSIGNED))) && ((PolyWord*)pt)->AsUnsigned() == 0)
            break;
        // Escape prefixes come before any Rex byte
        byte escPrefix = 0;
        if (*pt == 0xf2 || *pt == 0xf3 || *pt == 0x66)
            escPrefix = *pt++;
#ifdef HOSTARCHITECTURE_X86_64
        // REX prefixes.  Set this first.
        byte lastRex;
        if (*pt >= 0x40 && *pt <= 0x4f)
            lastRex = *pt++;
        else
            lastRex = 0;

        //printf("pt=%p *pt=%x\n", pt, *pt);

#endif /* HOSTARCHITECTURE_X86_64 */
        switch (*pt)
        {
        case 0x50: case 0x51: case 0x52: case 0x53:
        case 0x54: case 0x55: case 0x56: case 0x57: /* Push */
        case 0x58: case 0x59: case 0x5a: case 0x5b:
        case 0x5c: case 0x5d: case 0x5e: case 0x5f: /* Pop */
        case 0x90: /* nop */ case 0xc3: /* ret */
        case 0xf9: /* stc */ case 0xce: /* into */
        case 0xf0: /* lock. */ case 0xf3: /* rep/repe */
        case 0xa4: case 0xa5: case 0xaa: case 0xab: /* movs/stos */
        case 0xa6: /* cmpsb */ case 0x9e: /* sahf */ case 0x99: /* cqo/cdq */
            pt++; break;

        case 0x70: case 0x71: case 0x72: case 0x73: case 0x74: case 0x75: case 0x76: case 0x77:
        case 0x78: case 0x79: case 0x7a: case 0x7b: case 0x7c: case 0x7d: case 0x7e: case 0x7f:
        case 0xeb:
            /* short jumps. */
        case 0xcd: /* INT */
        case 0xa8: /* TEST_ACC8 */
        case 0x6a: /* PUSH_8 */
            pt += 2; break;

        case 0xc2: /* RET_16 */
            pt += 3; break;

        case 0x8d: /* leal. */
            pt++; skipea(&pt, process, true); break;

        case 0x03: case 0x0b: case 0x13: case 0x1b:
        case 0x23: case 0x2b: case 0x33: case 0x3b: /* Add r,ea etc. */
        case 0x88: /* MOVB_R_A */ case 0x89: /* MOVL_R_A */
        case 0x8b: /* MOVL_A_R */
        case 0x62: /* BOUNDL */
        case 0xff: /* Group5 */
        case 0xd1: /* Group2_1_A */
        case 0x8f: /* POP_A */
        case 0xd3: /* Group2_CL_A */
        case 0x87: // XCHNG
            pt++; skipea(&pt, process, false); break;

        case 0xf6: /* Group3_a */
            {
                int isTest = 0;
                pt++;
                /* The test instruction has an immediate operand. */
                if ((*pt & 0x38) == 0) isTest = 1;
                skipea(&pt, process, false);
                if (isTest) pt++;
                break;
            }

        case 0xf7: /* Group3_A */
            {
                int isTest = 0;
                pt++;
                /* The test instruction has an immediate operand. */
                if ((*pt & 0x38) == 0) isTest = 1;
                skipea(&pt, process, false);
                if (isTest) pt += 4;
                break;
            }

        case 0xc1: /* Group2_8_A */
        case 0xc6: /* MOVB_8_A */
        case 0x83: /* Group1_8_A */
        case 0x80: /* Group1_8_a */
            pt++; skipea(&pt, process, false); pt++; break;

        case 0x81: /* Group1_32_A */
            {
                pt ++;
#ifndef HOSTARCHITECTURE_X86_64
                unsigned opCode = *pt;
#endif /* HOSTARCHITECTURE_X86_64 */
                skipea(&pt, process, false);
                // Only check the 32 bit constant if this is a comparison.
                // For other operations this may be untagged and shouldn't be an address.
#ifndef HOSTARCHITECTURE_X86_64
                if ((opCode & 0x38) == 0x38)
                    process->ScanConstant(pt, PROCESS_RELOC_DIRECT);
#endif /* HOSTARCHITECTURE_X86_64 */
                pt += 4;
                break;
            }

        case 0xe8: case 0xe9:
            // Long jump and call.  These are used to call constant (known) functions
            // and also long jumps within the function.
            {
                pt++;
                POLYSIGNED disp = (pt[3] & 0x80) ? -1 : 0; // Set the sign just in case.
                for(unsigned i = 4; i > 0; i--)
                    disp = (disp << 8) | pt[i-1];
                byte *absAddr = pt + disp + 4; // The address is relative to AFTER the constant

                // If the new address is within the current piece of code we don't do anything
                if (absAddr >= (byte*)addr && absAddr < (byte*)end) {}
                else {
#ifdef HOSTARCHITECTURE_X86_64
                    ASSERT(sizeof(PolyWord) == 4); // Should only be used internally on x64
#endif /* HOSTARCHITECTURE_X86_64 */
                    if (addr != old)
                    {
                        // The old value of the displacement was relative to the old address before
                        // we copied this code segment.
                        // We have to correct it back to the original address.
                        absAddr = absAddr - (byte*)addr + (byte*)old;
                        // We have to correct the displacement for the new location and store
                        // that away before we call ScanConstant.
                        POLYSIGNED newDisp = absAddr - pt - 4;
                        for (unsigned i = 0; i < 4; i++)
                        {
                            pt[i] = (byte)(newDisp & 0xff);
                            newDisp >>= 8;
                        }
                    }
                    process->ScanConstant(pt, PROCESS_RELOC_I386RELATIVE);
                }
                pt += 4;
                break;
            }

        case 0xc7:/* MOVL_32_A */
            {
                pt++;
                if ((*pt & 0xc0) == 0x40 /* Byte offset or sib present */ &&
                    ((*pt & 7) != 4) /* But not sib present */ && pt[1] == 256-sizeof(PolyWord))
                {
                    /* We may use a move instruction to set the length
                       word on a new segment.  We mustn't try to treat this as a constant.  */
                    pt += 6; /* Skip the modrm byte, the offset and the constant. */
                }
                else
                {
                    skipea(&pt, process, false);
#ifndef HOSTARCHITECTURE_X86_64
                    process->ScanConstant(pt, PROCESS_RELOC_DIRECT);
#endif /* HOSTARCHITECTURE_X86_64 */
                    pt += 4;
                }
                break;
            }

        case 0xb8: case 0xb9: case 0xba: case 0xbb:
        case 0xbc: case 0xbd: case 0xbe: case 0xbf: /* MOVL_32_64_R */
            pt ++;
#ifdef HOSTARCHITECTURE_X86_64
            if ((lastRex & 8) == 0)
                pt += 4; // 32-bit mode on 64-bits.  Can this occur?
            else
#endif /* HOSTARCHITECTURE_X86_64 */
            {
                // 32 bits in 32-bit mode, 64-bits in 64-bit mode.
                process->ScanConstant(pt, PROCESS_RELOC_DIRECT);
                pt += sizeof(PolyWord);
            }
            break;

        case 0x68: /* PUSH_32 */
            pt ++;
#ifndef HOSTARCHITECTURE_X86_64
            process->ScanConstant(pt, PROCESS_RELOC_DIRECT);
#endif /* HOSTARCHITECTURE_X86_64 */
            pt += 4;
            break;

        case 0x0f: /* ESCAPE */
            {
                pt++;
                switch (*pt)
                {
                case 0xb6: /* movzl */
                case 0xb7: // movzw
                case 0xc1: /* xaddl */
                case 0xaf: // imul
                    pt++; skipea(&pt, process, false); break;

                case 0x80: case 0x81: case 0x82: case 0x83:
                case 0x84: case 0x85: case 0x86: case 0x87:
                case 0x88: case 0x89: case 0x8a: case 0x8b:
                case 0x8c: case 0x8d: case 0x8e: case 0x8f:
                    /* Conditional branches with 32-bit displacement. */
                    pt += 5; break;

                // These are SSE2 instructions
                case 0x10: case 0x11: case 0x58: case 0x5c: case 0x59: case 0x5e:
                case 0x2e: case 0x2a: case 0x54: case 0x57: case 0x5a: case 0xfa:
                    pt++; skipea(&pt, process, false); break;

                default: Crash("Unknown opcode %d at %p\n", *pt, pt);
                }
                break;
            }

        case 0xd8: case 0xd9: case 0xda: case 0xdb:
        case 0xdc: case 0xdd: case 0xde: case 0xdf: // Floating point escape instructions
            {
                pt++;
                if ((*pt & 0xe0) == 0xe0) pt++;
                else skipea(&pt, process, false);
                break;
            }

        default: Crash("Unknown opcode %d at %p\n", *pt, pt);
        }
    }
}

// Increment the value contained in the first word of the mutex.
Handle X86TaskData::AtomicIncrement(Handle mutexp)
{
    PolyObject *p = DEREFHANDLE(mutexp);
    POLYUNSIGNED result = X86AsmAtomicIncrement(p);
    return this->saveVec.push(PolyWord::FromUnsigned(result));
}

// Release a mutex.  Because the atomic increment and decrement
// use the hardware LOCK prefix we can simply set this to one.
void X86TaskData::AtomicReset(Handle mutexp)
{
    DEREFHANDLE(mutexp)->Set(0, TAGGED(1));
}

static X86Dependent x86Dependent;

MachineDependent *machineDependent = &x86Dependent;

static struct _entrypts entryPtTable[] =
{
    { "PolySetCodeConstant",              (polyRTSFunction)&PolySetCodeConstant},

    { NULL, NULL} // End of list.
};

class X86Module: public RtsModule
{
public:
    virtual entrypts GetRTSCalls(void) { return entryPtTable; }
};

// Declare this.  It will be automatically added to the table.
static X86Module timingModule;
