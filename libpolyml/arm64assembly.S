//
//  Assembly code for the ARM64 for Poly/ML
//  Author:    David Matthews
//  Copyright (c) David C. J. Matthews 2021
//
//  This library is free software; you can redistribute it and/or
//  modify it under the terms of the GNU Lesser General Public
//  License version 2.1 as published by the Free Software Foundation.
//  
//  This library is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  Lesser General Public License for more details.
//  
//  You should have received a copy of the GNU Lesser General Public
//  License along with this library// if not, write to the Free Software
//  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

// The syntax of directives in the GNU assembler and in the MS ARMASM
// are somewhat different.  ARMASMSYNTAX is defined in the VS project files. 

#ifdef ARMASMSYNTAX
#define LABEL(x) x
#else
#define LABEL(x) x:
#endif

#ifndef MACOSX
#ifdef ARMASMSYNTAX
    AREA |.text|, CODE, READONLY
#else
    .section .text,"x"
    .balign 4
#endif
#else
.align 4
#endif

#ifdef HAVE_CONFIG_H
// Specifically for POLYML32IN64
#include "config.h"
#endif

#ifdef SYMBOLS_REQUIRE_UNDERSCORE
#define EXTNAME(x)  _##x
#else
#define EXTNAME(x)  x
#endif



// Offsets into the assembly code interface
#define TrapHandlerEntry    32
#define HandlerRegister     40
#define ExceptionPacket     56
#define ThreadId            64
#define RegisterArray       72
#define FPRegisterArray     272
#define LocalMBottom        336
#define LocalMPointer       344
#define MLStackPointer      352
#define LinkRegister        360
#define EntryPoint          368
#define ReturnReason        376

#ifdef ARMASMSYNTAX
    EXPORT Arm64AsmEnterCompiledCode
Arm64AsmEnterCompiledCode PROC
#else
    .global EXTNAME(Arm64AsmEnterCompiledCode)
EXTNAME(Arm64AsmEnterCompiledCode):
#endif
// This is called once the thread has been initialised to run the ML code.
// It never returns.  The RTS may be entered either by a compiled RTS call
// or by a call to a "trap" function.
// We only need to load a subset of the registers.
    mov     x26,x0      // Copy the address of the assembly-code section into X26
    ldr     x0,[x26, RegisterArray]             // Argument
    ldr     x8,[x26, RegisterArray+8*8]         // Closure address
#ifdef POLYML32IN64
    ldr     x24,[x26, RegisterArray+24*8]     // Heap base address
    mov     x16,POLYML32IN64*2
    madd    x16,x16,x8,x24                      // Closure address as real address
    ldr     x16,[x16]
#else
    ldr     x16,[x8]                            // Code address - first word of closure
#endif
    ldr     x25,[x26, LocalMBottom]             // Limit of heap
    ldp     x27,x28,[x26, LocalMPointer]        // Allocation pointer and stack pointer
    ldr     x30,[x26, LinkRegister]             // Link register - always zero because we don't return

    br      x16                                 // Jump to code


#ifdef ARMASMSYNTAX
    ENDP
#endif

#ifdef ARMASMSYNTAX
    EXPORT Arm64AsmCallExtraRETURN_HEAP_OVERFLOW
Arm64AsmCallExtraRETURN_HEAP_OVERFLOW PROC
#else
    .global EXTNAME(Arm64AsmCallExtraRETURN_HEAP_OVERFLOW)
EXTNAME(Arm64AsmCallExtraRETURN_HEAP_OVERFLOW) :
#endif

    mov     x16, 1

    // Common code to call into the RTS
LABEL(trapHandle)
    strb    w16,[x26, ReturnReason]
    stp     x0,x1,[x26, RegisterArray]
    stp     x2,x3, [x26, RegisterArray+2*8]
    stp     x4,x5, [x26, RegisterArray + 4*8]
    stp     x6, x7, [x26, RegisterArray + 6*8]
    stp     x8, x9, [x26, RegisterArray + 8*8]
    stp     x10, x11, [x26, RegisterArray + 10*8]
    stp     x12, x13, [x26, RegisterArray + 12*8]
    stp     x14, x15, [x26, RegisterArray + 14*8]
    stp     x19, x20, [x26, RegisterArray + 19 * 8]
    stp     x21,x22,[x26, RegisterArray + 21 * 8]
    stp     x23,x24, [x26, RegisterArray + 23 * 8]
    stp     d0,d1,[x26, FPRegisterArray]
    stp     d2,d3,[x26, FPRegisterArray+2*8]
    stp     d4,d5,[x26, FPRegisterArray+4*8]
    stp     d6,d7,[x26, FPRegisterArray+6*8]
    str     x27,[x26,LocalMPointer]
    str     x28,[x26,MLStackPointer]
    str     x30,[x26,LinkRegister]
    ldr     x0,[x26,ThreadId]           // Pass the thread id as an argument so that we can get the task data
    ldr     x16,[x26,TrapHandlerEntry]
    blr     x16
    // Load the registers.  Even though some are callee-save the RTS may have updated them.
    // x26, though, should have been preserved.
    ldr     x1,[x26, RegisterArray+1*8]
    ldp     x2,x3, [x26, RegisterArray+2*8]
    ldp     x4,x5, [x26, RegisterArray + 4*8]
    ldp     x6, x7, [x26, RegisterArray + 6*8]
    ldp     x8, x9, [x26, RegisterArray + 8*8]
    ldp     x10, x11, [x26, RegisterArray + 10*8]
    ldp     x12, x13, [x26, RegisterArray + 12*8]
    ldp     x14, x15, [x26, RegisterArray + 14*8]
    ldp     x19, x20, [x26, RegisterArray + 19 * 8]
    ldp     x21,x22,[x26, RegisterArray + 21 * 8]
    ldp     x23,x24, [x26, RegisterArray + 23 * 8]
    ldp     d0,d1,[x26, FPRegisterArray]
    ldp     d2,d3,[x26, FPRegisterArray+2*8]
    ldp     d4,d5,[x26, FPRegisterArray+4*8]
    ldp     d6,d7,[x26, FPRegisterArray+6*8]
    ldr     x25,[x26, LocalMBottom]
    ldp     x27,x28,[x26,LocalMPointer]
    ldr     x30,[x26,LinkRegister]
    // Check whether we've raised an exception e.g. Interrupt
    ldr     x0,[x26,ExceptionPacket]
    cmp     x0,#1
    bne     raiseexcept
    ldr     x0,[x26, RegisterArray]
    ldr     x16,[x26,EntryPoint]        // Normally this will be x30 but not always
    br      x16

LABEL(raiseexcept)
    ldr     x28,[x26,HandlerRegister]   // Set the stack ptr to this
    ldr     x16,[x28]
    br      x16
#ifdef ARMASMSYNTAX
    ENDP
#endif
#ifdef ARMASMSYNTAX
    EXPORT Arm64AsmCallExtraRETURN_STACK_OVERFLOW
Arm64AsmCallExtraRETURN_STACK_OVERFLOW PROC
#else
    .global EXTNAME(Arm64AsmCallExtraRETURN_STACK_OVERFLOW)
EXTNAME(Arm64AsmCallExtraRETURN_STACK_OVERFLOW) :
#endif

    mov     x16, 2
    b       trapHandle

#ifdef ARMASMSYNTAX
    ENDP
#endif

#ifdef ARMASMSYNTAX
    EXPORT Arm64AsmCallExtraRETURN_STACK_OVERFLOWEX
Arm64AsmCallExtraRETURN_STACK_OVERFLOWEX PROC
#else
    .global EXTNAME(Arm64AsmCallExtraRETURN_STACK_OVERFLOWEX)
EXTNAME(Arm64AsmCallExtraRETURN_STACK_OVERFLOWEX) :
#endif

    mov     x16, 3
    b       trapHandle

#ifdef ARMASMSYNTAX
    ENDP
#endif
#ifdef ARMASMSYNTAX
    EXPORT Arm64AsmCallExtraRETURN_ENTER_INTERPRETER
Arm64AsmCallExtraRETURN_ENTER_INTERPRETER PROC
#else
    .global EXTNAME(Arm64AsmCallExtraRETURN_ENTER_INTERPRETER)
EXTNAME(Arm64AsmCallExtraRETURN_ENTER_INTERPRETER) :
#endif

    mov     x16,4
    b       trapHandle

#ifdef ARMASMSYNTAX
    ENDP
#endif



// POLYUNSIGNED Arm64AsmAtomicExchange(PolyObject*, POLYSIGNED);
// This is not actually used with the VS build.
#ifdef ARMASMSYNTAX
    EXPORT Arm64AsmAtomicExchange
Arm64AsmAtomicExchange PROC
#else
    .global EXTNAME(Arm64AsmAtomicExchange)
EXTNAME(Arm64AsmAtomicExchange):
#endif

// The easiest way to do this is with swpal  but that is only available
// in ARM 8.1 and above.  For the moment we use the old version.
//  swpal x0,xzr,[x0]
LABEL(aaea1)
    ldaxr   x3,[x0]
    stlxr   w4,xzr,[x0]
    cbnz    w4,aaea1
    dmb     ish
    mov     x0,x3
    ret

#ifdef ARMASMSYNTAX
    ENDP
    END
#endif
