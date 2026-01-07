/*
   Title:  Assembly code routines for the poly system.
   Author:    David Matthews
   Copyright (c) David C. J. Matthews 2000-2021
 
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
#define Arg_TrapHandlerEntry    0x34
#define Arg_SaveRAX             0x38
#define Arg_SaveRBX             0x3c
#define Arg_SaveRCX             0x40
#define Arg_SaveRDX             0x44
#define Arg_SaveRSI             0x48
#define Arg_SaveRDI             0x4c
#define Arg_SaveFP              0x50

#define RETURN_HEAP_OVERFLOW        1
#define RETURN_STACK_OVERFLOW       2
#define RETURN_STACK_OVERFLOWEX     3
#define RETURN_ENTER_INTERPRETER    4
#define RETURN_ARB_EMULATION        5

# Mark the stack as non-executable when supported
#if (defined(__linux__) && defined(__ELF__))
.section .note.GNU-stack, "", @progbits
#endif

#
# CODE STARTS HERE
#
    .text

#define CALL_EXTRA(index) \
        movb  $index,Arg_ReturnReason(%ebp); \
        jmp   CallTrapHandler;

/*  Enter ML code.  This is now only ever used to start a new thread.
    It is probably unnecessary to save the callee-save regs or load the ML regs. */
INLINE_ROUTINE(X86AsmSwitchToPoly)
    pushl   %ebp                            # Standard entry sequence
    movl    8(%esp),%ebp                    # Address of argument vector
    pushl   %ebx
    pushl   %edi
    pushl   %esi                            # Push callee-save registers
    subl    $(Fr_Size-12),%esp              # Allocate frame
    movl    %esp,Arg_SaveCStack(%ebp)
    movl    Arg_StackPtr(%ebp),%esp
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
    jmp     *(%edx)


/* Save all the registers and enter the trap handler.
   It is probably unnecessary to save the FP state now. */
CallTrapHandler:
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
    subl    $12,%esp                        # Align stack ptr - GCC prefers this
    pushl   Arg_ThreadId(%ebp)
    calll   *Arg_TrapHandlerEntry(%ebp)
    addl    $16,%esp
    movl    Arg_StackPtr(%ebp),%esp
    movl    Arg_ExceptionPacket(%ebp),%eax
    cmpl    $1,%eax                             # Did we raise an exception?
    jnz     raisexlocal
    FRSTOR  Arg_SaveFP(%ebp)
    movl    Arg_SaveRAX(%ebp),%eax              # Load the registers
    movl    Arg_SaveRBX(%ebp),%ebx              # Load the registers
    movl    Arg_SaveRCX(%ebp),%ecx
    movl    Arg_SaveRDX(%ebp),%edx
    movl    Arg_SaveRSI(%ebp),%esi
    movl    Arg_SaveRDI(%ebp),%edi
    cld                                     # Clear this just in case
    ret

raisexlocal:
    movl    Arg_HandlerRegister(%ebp),%ecx    # Get next handler into %rcx
    jmp     *(%ecx)

INLINE_ROUTINE(X86AsmCallExtraRETURN_HEAP_OVERFLOW)
    CALL_EXTRA(RETURN_HEAP_OVERFLOW)

INLINE_ROUTINE(X86AsmCallExtraRETURN_STACK_OVERFLOW)
    CALL_EXTRA(RETURN_STACK_OVERFLOW)

INLINE_ROUTINE(X86AsmCallExtraRETURN_STACK_OVERFLOWEX)
    CALL_EXTRA(RETURN_STACK_OVERFLOWEX)
    
INLINE_ROUTINE(X86AsmCallExtraRETURN_ARB_EMULATION)
    CALL_EXTRA(RETURN_ARB_EMULATION)

INLINE_ROUTINE(X86AsmCallExtraRETURN_ENTER_INTERPRETER)
    CALL_EXTRA(RETURN_ENTER_INTERPRETER)

# Additional assembly code routines

# Fallback exchange if we can't use inline.
INLINE_ROUTINE(X86AsmAtomicExchange)
# Use %ecx and %eax because they are volatile (unlike %ebx on X86/64/Unix)
    movl    4(%esp),%ecx
    movl    8(%esp),%eax
    xchg    %eax,(%ecx)
    ret

