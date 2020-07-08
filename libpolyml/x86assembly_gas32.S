/*
   Title:  Assembly code routines for the poly system.
   Author:    David Matthews
   Copyright (c) David C. J. Matthews 2000-2020
 
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

/*
   This is the 32-bit Unix version of the assembly code file.
   There are separate versions of 32/64 and Windows (Intel syntax)
   and Unix (gas syntax).
*/

/*
 Registers used :-

  %%eax: First argument to function.  Result of function call.
  %%ebx: Second argument to function.
  %%ecx: General register
  %%edx: Closure pointer in call.
  %%ebp: Points to memory used for extra registers
  %%esi: General register.
  %%edi: General register.
  %%esp: Stack pointer.
*/


#include "config.h"
#ifdef SYMBOLS_REQUIRE_UNDERSCORE
#define EXTNAME(x)  _##x
#else
#define EXTNAME(x)  x
#endif

#
# Macro to begin the hand-coded functions
#

#ifdef MACOSX
#define GLOBAL .globl
#else
#define GLOBAL .global
#endif

#define INLINE_ROUTINE(id) \
GLOBAL EXTNAME(id); \
EXTNAME(id):

#define Fr_Size                     16

/* This is the argument vector passed in to X86AsmSwitchToPoly
   It is used to initialise the frame.  A few values are updated
   when ML returns. */
#define Arg_LocalMpointer       0x0
#define Arg_HandlerRegister     0x4
#define Arg_LocalMbottom        0x8
#define Arg_StackLimit          0xc
#define Arg_ExceptionPacket     0x10  /* Address of packet to raise */
#define Arg_RequestCode         0x14 /* Byte: Io function to call. */
#define Arg_ReturnReason        0x16  /* Byte: Reason for returning from ML. */
#define Arg_Pflags              0x17  /* Byte: Flags byte - used in arb prec compare */
#define Arg_SaveCStack          0x18  /* Save C Stack pointer */
#define Arg_ThreadId            0x1c  /* My thread id */
#define Arg_StackPtr            0x20  /* Stack Pointer */
#define Arg_SaveRAX             0x34
#define Arg_SaveRBX             0x38
#define Arg_SaveRCX             0x3c
#define Arg_SaveRDX             0x40
#define Arg_SaveRSI             0x44
#define Arg_SaveRDI             0x48
#define Arg_SaveFP              0x4c

#define RETURN_HEAP_OVERFLOW        1
#define RETURN_STACK_OVERFLOW       2
#define RETURN_STACK_OVERFLOWEX     3
#define RETURN_ARB_EMULATION        5
#define RETURN_CALLBACK_RETURN      6
#define RETURN_CALLBACK_EXCEPTION   7
#define RETURN_KILL_SELF            9

# Mark the stack as non-executable when supported
#ifdef HAVE_GNU_STACK
.section .note.GNU-stack, "", @progbits
#endif

#
# CODE STARTS HERE
#
    .text

#define CALL_EXTRA(index) \
        pushl %ecx; \
        movb  $index,Arg_ReturnReason(%ebp); \
        popl  %ecx; \
        jmp   SaveFullState;

/* Load the registers from the ML stack and jump to the code.
  This is used to start ML code.
  The argument is the address of the MemRegisters struct and goes into %rbp.
  This is the general code for switching control to ML.  There are a number of cases to consider:
  1.  Initial entry to root function or a new thread.  Needs to load EDX at least.
  2.  Normal return from an RTS call.  Could just do a simple return.
  3.  Exception raised in RTS call.
  4.  Callback from C to an ML function.  In effect this is a coroutine. Similar to 1.
  5.  Return from "trap" i.e. Heap/Stack overflow.  Stack-overflow can result in an exception
      either because the stack can't be grown or because Interrupt has been raised. */
INLINE_ROUTINE(X86AsmSwitchToPoly)
    pushl   %ebp                            # Standard entry sequence
    movl    8(%esp),%ebp                    # Address of argument vector
    pushl   %ebx
    pushl   %edi
    pushl   %esi                            # Push callee-save registers
    subl    $(Fr_Size-12),%esp              # Allocate frame
    movl    %esp,Arg_SaveCStack(%ebp)
    movl    Arg_StackPtr(%ebp),%esp
    movl    Arg_ExceptionPacket(%ebp),%eax
    cmpl    $1,%eax                             # Did we raise an exception?
    jnz     raisexlocal
    FRSTOR  Arg_SaveFP(%ebp)
    movb    Arg_Pflags(%ebp),%ah
    sahf
    movl    Arg_SaveRAX(%ebp),%eax              # Load the registers
    movl    Arg_SaveRBX(%ebp),%ebx              # Load the registers
    movl    Arg_SaveRCX(%ebp),%ecx
    movl    Arg_SaveRDX(%ebp),%edx
    movl    Arg_SaveRSI(%ebp),%esi
    movl    Arg_SaveRDI(%ebp),%edi
    cld                                     # Clear this just in case
    ret


/* Code to save the state and switch to C
   This saves the full register state. */
SaveFullState:
    movl    %eax,Arg_SaveRAX(%ebp)
    movl    %ebx,Arg_SaveRBX(%ebp)
    movl    %ecx,Arg_SaveRCX(%ebp)
    movl    %edx,Arg_SaveRDX(%ebp)
    movl    %esi,Arg_SaveRSI(%ebp)
    movl    %edi,Arg_SaveRDI(%ebp)
    fnsave  Arg_SaveFP(%ebp)                # Save FP state.  Also resets the state so...
    fldcw   Arg_SaveFP(%ebp)                # ...load because we need the same rounding mode in the RTS
    movl    %esp,Arg_StackPtr(%ebp)         # Save ML stack pointer
    movl    Arg_SaveCStack(%ebp),%esp       # Restore C stack pointer
    addl    $(Fr_Size-12),%esp
    popl    %esi
    popl    %edi
    popl    %ebx
    popl    %ebp
    ret

/* Used when entering new code.  The argument and closure are on the stack
   in case there is a GC before we enter the code. */
INLINE_ROUTINE(X86AsmPopArgAndClosure)
    popl    %edx
    popl    %eax
    jmp     *(%edx)

INLINE_ROUTINE(X86AsmRaiseException)
raisexlocal:
    movl    Arg_HandlerRegister(%ebp),%ecx    # Get next handler into %rcx
    jmp     *(%ecx)

# Additional assembly code routines

INLINE_ROUTINE(X86AsmCallExtraRETURN_HEAP_OVERFLOW)
    CALL_EXTRA(RETURN_HEAP_OVERFLOW)

INLINE_ROUTINE(X86AsmCallExtraRETURN_STACK_OVERFLOW)
    CALL_EXTRA(RETURN_STACK_OVERFLOW)

INLINE_ROUTINE(X86AsmCallExtraRETURN_STACK_OVERFLOWEX)
    CALL_EXTRA(RETURN_STACK_OVERFLOWEX)

# RTS call to kill the current thread. 
INLINE_ROUTINE(X86AsmKillSelf)
    CALL_EXTRA(RETURN_KILL_SELF)

INLINE_ROUTINE(X86AsmCallbackReturn)
    CALL_EXTRA(RETURN_CALLBACK_RETURN)

INLINE_ROUTINE(X86AsmCallbackException)
    CALL_EXTRA(RETURN_CALLBACK_EXCEPTION)

INLINE_ROUTINE(X86AsmCallExtraRETURN_ARB_EMULATION)
    CALL_EXTRA(RETURN_ARB_EMULATION)

# This implements atomic addition in the same way as atomic_increment
INLINE_ROUTINE(X86AsmAtomicIncrement)
#ifndef HOSTARCHITECTURE_X86_64
    movl    4(%esp),%eax
#else
    movl    %edi,%eax   # On X86_64 the argument is passed in %edi
#endif
# Use %ecx and %eax because they are volatile (unlike %ebx on X86/64/Unix)
    movl    $2,%ecx
    lock; xaddl %ecx,(%eax)
    addl    $2,%ecx
    movl    %ecx,%eax
    ret

