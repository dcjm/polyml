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
   This is the 64-bit gas version of the assembly code file.
   There are separate versions of 32/64 and MAMS (Intel syntax) and
   and GCC (gas syntax).
*/

/*
 Registers used :-

  %rax: First argument to function.  Result of function call.
  %rbx: Second argument to function.
  %rcx: General register
  %rdx: Closure pointer in call.
  %rbp: Points to memory used for extra registers
  %rsi: General register.
  %rdi: General register.
  %rsp: Stack pointer.
  %r8:   Third argument to function
  %r9:   Fourth argument to function
  %r10:  Fifth argument to function
  %r11:  General register
  %r12:  General register
  %r13:  General register
  %r14:  General register
  %r15:  Memory allocation pointer
*/


#include "config.h"
#ifdef SYMBOLS_REQUIRE_UNDERSCORE
#define EXTNAME(x)  _##x
#else
#define EXTNAME(x)  x
#endif

#ifdef __CYGWIN__
#define _WIN32 1
#endif

/* Macro to begin the hand-coded functions */
#ifdef MACOSX
#define GLOBAL .globl
#else
#define GLOBAL .global
#endif

#define INLINE_ROUTINE(id) \
GLOBAL EXTNAME(id); \
EXTNAME(id):

/* Extra entries on the C stack */
#define Fr_Size                 64         /* Must be multiple of 16 to get alignment correct */

/* This is the argument vector passed in to X86AsmSwitchToPoly
   It is used to initialise the frame.  A few values are updated
   when ML returns. */
#define Arg_LocalMpointer       0x0
#define Arg_HandlerRegister     0x8
#define Arg_LocalMbottom        0x10
#define Arg_StackLimit          0x18
#define Arg_ExceptionPacket     0x20  /* Address of packet to raise */
#define Arg_RequestCode         0x28  /* Byte: Io function to call. */
#define Arg_ReturnReason        0x2a  /* Byte: Reason for returning from ML. */
#define Arg_Pflags              0x2b  /* Byte: Flags byte - used in arb prec compare */
#define Arg_SaveCStack          0x30  /* Current stack base */
#define Arg_ThreadId            0x38  /* My thread id */
#define Arg_StackPtr            0x40  /* Stack Pointer */
#define Arg_SaveRAX             0x68
#define Arg_SaveRBX             0x70
#define Arg_SaveRCX             0x78
#define Arg_SaveRDX             0x80
#define Arg_SaveRSI             0x88
#define Arg_SaveRDI             0x90
#define Arg_SaveR8              0x98
#define Arg_SaveR9              0xa0
#define Arg_SaveR10             0xa8
#define Arg_SaveR11             0xb0
#define Arg_SaveR12             0xb8
#define Arg_SaveR13             0xc0
#define Arg_SaveR14             0xc8
#define Arg_SaveXMM0            0xd0
#define Arg_SaveXMM1            0xd8
#define Arg_SaveXMM2            0xe0
#define Arg_SaveXMM3            0xe8
#define Arg_SaveXMM4            0xf0
#define Arg_SaveXMM5            0xf8
#define Arg_SaveXMM6            0x100

/* IO function numbers.  These are functions that are called
   to handle special cases in this code */
#include "sys.h"

#define RETURN_HEAP_OVERFLOW        1
#define RETURN_STACK_OVERFLOW       2
#define RETURN_STACK_OVERFLOWEX     3
#define RETURN_ARB_EMULATION        5
#define RETURN_CALLBACK_RETURN      6
#define RETURN_CALLBACK_EXCEPTION   7
#define RETURN_RAISE_OVERFLOW       8
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
        pushq %rcx; \
        movb  $index,Arg_ReturnReason(%rbp); \
        popq  %rcx; \
        jmp   SaveFullState;


/* Load the registers from the ML stack and jump to the code. */
INLINE_ROUTINE(X86AsmSwitchToPoly)
    pushq   %rbp                            # Standard entry sequence
/* If we're compiling with Mingw we're using Windows calling conventions. */
#ifdef _WIN32
    movq    %rcx,%rbp                       # Argument is in %rcx
#else
    movq    %rdi,%rbp                       # Argument is in %rdi
#endif
    pushq   %rbx
    pushq   %r12
    pushq   %r13
    pushq   %r14
    pushq   %r15
#ifdef _WIN32
    pushq   %rdi                            # Callee save in Windows
    pushq   %rsi
    subq    $(Fr_Size-56),%rsp              # Argument is already in %rcx
#else
    subq    $(Fr_Size-40),%rsp
#endif
    movq    %rsp,Arg_SaveCStack(%rbp)

    movq    Arg_LocalMpointer(%rbp),%r15
    movq    Arg_StackPtr(%rbp),%rsp               # Set the new stack ptr

    movsd   Arg_SaveXMM0(%rbp),%xmm0              # Load the registers
    movsd   Arg_SaveXMM1(%rbp),%xmm1
    movsd   Arg_SaveXMM2(%rbp),%xmm2
    movsd   Arg_SaveXMM3(%rbp),%xmm3
    movsd   Arg_SaveXMM4(%rbp),%xmm4
    movsd   Arg_SaveXMM5(%rbp),%xmm5
    movsd   Arg_SaveXMM6(%rbp),%xmm6
    movq    Arg_SaveRBX(%rbp),%rbx
    movq    Arg_SaveRCX(%rbp),%rcx
    movq    Arg_SaveRDX(%rbp),%rdx
    movq    Arg_SaveRSI(%rbp),%rsi
    movq    Arg_SaveRDI(%rbp),%rdi
    movq    Arg_SaveR8(%rbp),%r8
    movq    Arg_SaveR9(%rbp),%r9
    movq    Arg_SaveR10(%rbp),%r10
    movq    Arg_SaveR11(%rbp),%r11
    movq    Arg_SaveR12(%rbp),%r12
    movq    Arg_SaveR13(%rbp),%r13
    movq    Arg_SaveR14(%rbp),%r14
    movq    Arg_ExceptionPacket(%rbp),%rax
    cmpq    $1,%rax                             # Did we raise an exception?
    jnz     raisexLocal
    movb    Arg_Pflags(%rbp),%ah
    sahf
    movq    Arg_SaveRAX(%rbp),%rax
    cld                                     # Clear this just in case
    ret

/* This is exactly the same as raisex but seems to be needed to work round a PIC problem. */
raisexLocal:
    movq    Arg_HandlerRegister(%rbp),%rcx    # Get next handler into %rcx
    jmp     *(%rcx)

/* Code to save the state and switch to C
   This saves the full register state. */
SaveFullState:
    movq    %rax,Arg_SaveRAX(%rbp)
    movq    %rbx,Arg_SaveRBX(%rbp)
    movq    %rcx,Arg_SaveRCX(%rbp)
    movq    %rdx,Arg_SaveRDX(%rbp)
    movq    %rsi,Arg_SaveRSI(%rbp)
    movq    %rdi,Arg_SaveRDI(%rbp)
    movsd   %xmm0,Arg_SaveXMM0(%rbp)
    movsd   %xmm1,Arg_SaveXMM1(%rbp)
    movsd   %xmm2,Arg_SaveXMM2(%rbp)
    movsd   %xmm3,Arg_SaveXMM3(%rbp)
    movsd   %xmm4,Arg_SaveXMM4(%rbp)
    movsd   %xmm5,Arg_SaveXMM5(%rbp)
    movsd   %xmm6,Arg_SaveXMM6(%rbp)
    movq    %r8,Arg_SaveR8(%rbp)
    movq    %r9,Arg_SaveR9(%rbp)
    movq    %r10,Arg_SaveR10(%rbp)
    movq    %r11,Arg_SaveR11(%rbp)
    movq    %r12,Arg_SaveR12(%rbp)
    movq    %r13,Arg_SaveR13(%rbp)
    movq    %r14,Arg_SaveR14(%rbp)
    movq    %rsp,Arg_StackPtr(%rbp)
    movq    %r15,Arg_LocalMpointer(%rbp)        # Save back heap pointer
    movq    Arg_SaveCStack(%rbp),%rsp           # Restore C stack pointer
#ifdef _WIN32
    addq    $(Fr_Size-56),%rsp
    popq    %rsi
    popq    %rdi
#else
    addq    $(Fr_Size-40),%rsp
#endif
    popq    %r15                                # Restore callee-save registers
    popq    %r14
    popq    %r13
    popq    %r12
    popq    %rbx
    popq    %rbp
    ret

/* Used when entering new code.  The argument and closure are on the stack
   in case there is a GC before we enter the code. */
INLINE_ROUTINE(X86AsmPopArgAndClosure)
    popq    %rdx
    popq    %rax
#ifdef POLYML32IN64
    jmp     *(%rbx,%rdx,4)
#else
    jmp     *(%rdx)
#endif

# This is used if the RTS sets up an exception.  It's probably no longer relevant.
INLINE_ROUTINE(X86AsmRaiseException)
    movq    Arg_HandlerRegister(%rbp),%rcx    # Get next handler into %rcx
    jmp     *(%rcx)
# Additional assembly code routines

# RTS call to kill the current thread. 
INLINE_ROUTINE(X86AsmKillSelf)
    CALL_EXTRA(RETURN_KILL_SELF)

INLINE_ROUTINE(X86AsmCallbackReturn)
    CALL_EXTRA(RETURN_CALLBACK_RETURN)

INLINE_ROUTINE(X86AsmCallbackException)
    CALL_EXTRA(RETURN_CALLBACK_EXCEPTION)

INLINE_ROUTINE(X86AsmCallExtraRETURN_HEAP_OVERFLOW)
    CALL_EXTRA(RETURN_HEAP_OVERFLOW)

INLINE_ROUTINE(X86AsmCallExtraRETURN_STACK_OVERFLOW)
    CALL_EXTRA(RETURN_STACK_OVERFLOW)

INLINE_ROUTINE(X86AsmCallExtraRETURN_STACK_OVERFLOWEX)
    CALL_EXTRA(RETURN_STACK_OVERFLOWEX)

INLINE_ROUTINE(X86AsmCallExtraRETURN_ARB_EMULATION)
    CALL_EXTRA(RETURN_ARB_EMULATION)

# This implements atomic addition in the same way as atomic_increment
INLINE_ROUTINE(X86AsmAtomicIncrement)
#ifdef _WIN32
    movq    %rcx,%rax       # On Windows the argument is passed in %rcx
#else
    movq    %rdi,%rax   # On X86_64 the argument is passed in %rdi
#endif
# Use %rcx and %rax because they are volatile (unlike %rbx on X86/64/Unix)
    movq    $2,%rcx
#ifdef POLYML32IN64
    lock xaddl %ecx,(%rax)      # Rax is an absolute address but this is only a word
#else
    lock xaddq %rcx,(%rax)
#endif
    addq    $2,%rcx
    movq    %rcx,%rax
    ret

