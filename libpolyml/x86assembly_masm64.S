;
;  Title:  Assembly code routines for the poly system.
;  Author:    David Matthews
;  Copyright (c) David C. J. Matthews 2000-2020
;
;  This library is free software; you can redistribute it and/or
;  modify it under the terms of the GNU Lesser General Public
;  License version 2.1 as published by the Free Software Foundation.
;  
;  This library is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  Lesser General Public License for more details.
;  
;  You should have received a copy of the GNU Lesser General Public
;  License along with this library; if not, write to the Free Software
;  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;

;
; Registers used :-
;
;  rax:  First argument to function.  Result of function call.
;  rbx:  Second argument to function.
;  rcx:  General register
;  rdx:  Closure pointer in call.
;  rbp:  Points to memory used for extra registers
;  rsi:  General register.
;  rdi:  General register.
;  rsp:  Stack pointer.
;  r8:   Third argument to function
;  r9:   Fourth argument to function
;  r10:  Fifth argument to function
;  r11:  General register
;  r12:  General register
;  r13:  General register
;  r14:  General register
;  r15:  Memory allocation pointer

; Extra entries on the C stack
Fr_Size             EQU     64         ; Must be multiple of 16 to get alignment correct

; This is the argument vector passed in to X86AsmSwitchToPoly
; It is used to initialise the frame.  A few values are updated
; when ML returns.
ArgVector STRUCT
LocalMPointer       QWORD   ?
HandlerRegister     QWORD   ?
LocalMbottom        QWORD   ?
StackLimit          QWORD   ?
ExceptionPacket     QWORD   ?       ; Address of packet to raise
UnusedRequestCode   DB      ?       ; Byte: Io function to call.
UnusedFlag          DB      ?
ReturnReason        DB      ?       ; Byte: Reason for returning from ML.
Pflags              DB      ?       ; Byte: Flags to set - used in arbitrary precision
UnusedAlign         DWORD   ?
SaveCStack          QWORD   ?       ; Saved C stack frame
ThreadId            QWORD   ?       ; My thread id
StackPtr            QWORD   ?       ; Stack pointer
UnusedProgramCtr    QWORD   ?
HeapOverFlowCall    QWORD   ?
StackOverFlowCall   QWORD   ?
StackOverFlowCallEx QWORD   ?
SaveRAX             QWORD   ?
SaveRBX             QWORD   ?
SaveRCX             QWORD   ?
SaveRDX             QWORD   ?
SaveRSI             QWORD   ?
SaveRDI             QWORD   ?
SaveR8              QWORD   ?
SaveR9              QWORD   ?
SaveR10             QWORD   ?
SaveR11             QWORD   ?
SaveR12             QWORD   ?
SaveR13             QWORD   ?
SaveR14             QWORD   ?
SaveXMM0            QWORD   ?
SaveXMM1            QWORD   ?
SaveXMM2            QWORD   ?
SaveXMM3            QWORD   ?
SaveXMM4            QWORD   ?
SaveXMM5            QWORD   ?
SaveXMM6            QWORD   ?
ArgVector ENDS

RETURN_HEAP_OVERFLOW        EQU 1
RETURN_STACK_OVERFLOW       EQU 2
RETURN_STACK_OVERFLOWEX     EQU 3
RETURN_ARB_EMULATION        EQU 5
RETURN_CALLBACK_RETURN      EQU 6
RETURN_CALLBACK_EXCEPTION   EQU 7
RETURN_KILL_SELF            EQU 9

;
; CODE STARTS HERE
;
    .CODE

; Define standard call macro.

CALL_EXTRA  MACRO   index
    push    rcx
    mov     byte ptr [ArgVector.ReturnReason+rbp],index
    pop     rcx
    jmp     SaveFullState
ENDM


; Load the registers from the ML stack and jump to the code.
; This is used to start ML code.
; The argument is the address of the MemRegisters struct and goes into rbp.
; This is the general code for switching control to ML.  There are a number of cases to consider:
; 1.  Initial entry to root function or a new thread.  Needs to load EDX at least.
; 2.  Normal return from an RTS call.  Could just do a simple return.
; 3.  Exception raised in RTS call.
; 4.  Callback from C to an ML function.  In effect this is a coroutine. Similar to 1.
; 5.  Return from "trap" i.e. Heap/Stack overflow.  Stack-overflow can result in an exception
;     either because the stack can't be grown or because Interrupt has been raised.

; Switch from C code to ML.  This code uses the X86/64 Windows calling conventions.  It
; saves the callee-save registers.
; This does not set up a correct frame because we don't want to reserve a register for
; that.  RBP needs to be the original argument because we need to be able to modify
; the stack limit "register" from another thread in order to be able to interrupt
; this one.
X86AsmSwitchToPoly  PROC FRAME
    push    rbp                             ; Standard entry sequence
    push    rbx                             ; Save callee-save registers
    push    r12
    push    r13
    push    r14
    push    r15
    push    rdi                             ; Callee save in Windows
    push    rsi                             ; Strictly, we should also save xmm6
    .endprolog
    mov     rbp,rcx                         ; Move argument into rbp - this is definitely non-standard
    sub     rsp,(Fr_size-56)
    mov     [ArgVector.SaveCStack+rcx],rsp  ; Save the C stack pointer
    mov     r15,[ArgVector.LocalMpointer+rbp]
    mov     rsp,[ArgVector.StackPtr+rbp]
    movsd   xmm0,[ArgVector.SaveXMM0+rbp]
    movsd   xmm1,[ArgVector.SaveXMM1+rbp]
    movsd   xmm2,[ArgVector.SaveXMM2+rbp]
    movsd   xmm3,[ArgVector.SaveXMM3+rbp]
    movsd   xmm4,[ArgVector.SaveXMM4+rbp]
    movsd   xmm5,[ArgVector.SaveXMM5+rbp]
    movsd   xmm6,[ArgVector.SaveXMM6+rbp]
    mov     rbx,[ArgVector.SaveRBX+rbp]
    mov     rcx,[ArgVector.SaveRCX+rbp]
    mov     rdx,[ArgVector.SaveRDX+rbp]
    mov     rsi,[ArgVector.SaveRSI+rbp]
    mov     rdi,[ArgVector.SaveRDI+rbp]
    mov     r8,[ArgVector.SaveR8+rbp]
    mov     r9,[ArgVector.SaveR9+rbp]
    mov     r10,[ArgVector.SaveR10+rbp]
    mov     r11,[ArgVector.SaveR11+rbp]
    mov     r12,[ArgVector.SaveR12+rbp]
    mov     r13,[ArgVector.SaveR13+rbp]
    mov     r14,[ArgVector.SaveR14+rbp]
    mov     rax,[ArgVector.ExceptionPacket+rbp]
    cmp     rax,1                                           ; Did we raise an exception?
    jnz     raisex
    mov     ah,[ArgVector.Pflags+rbp]
    sahf
    mov     rax,[ArgVector.SaveRAX+rbp]
    cld                                     ; Clear this just in case
    ret

; Everything up to here is considered as part of the X86AsmSwitchToPoly proc
X86AsmSwitchToPoly ENDP

; Code to save the state and switch to C
; This saves the full register state.
SaveFullState:
    mov     [ArgVector.SaveRAX+rbp],rax
    mov     [ArgVector.SaveRBX+rbp],rbx
    mov     [ArgVector.SaveRCX+rbp],rcx
    mov     [ArgVector.SaveRDX+rbp],rdx
    mov     [ArgVector.SaveRSI+rbp],rsi
    mov     [ArgVector.SaveRDI+rbp],rdi
    movsd   [ArgVector.SaveXMM0+rbp],xmm0
    movsd   [ArgVector.SaveXMM1+rbp],xmm1
    movsd   [ArgVector.SaveXMM2+rbp],xmm2
    movsd   [ArgVector.SaveXMM3+rbp],xmm3
    movsd   [ArgVector.SaveXMM4+rbp],xmm4
    movsd   [ArgVector.SaveXMM5+rbp],xmm5
    movsd   [ArgVector.SaveXMM6+rbp],xmm6
    mov     [ArgVector.SaveR8+rbp],r8
    mov     [ArgVector.SaveR9+rbp],r9
    mov     [ArgVector.SaveR10+rbp],r10
    mov     [ArgVector.SaveR11+rbp],r11
    mov     [ArgVector.SaveR12+rbp],r12
    mov     [ArgVector.SaveR13+rbp],r13
    mov     [ArgVector.SaveR14+rbp],r14
    mov     [ArgVector.StackPtr+rbp],rsp
    mov     [ArgVector.LocalMpointer+rbp],r15       ; Save back heap pointer
    mov     rsp,[ArgVector.SaveCStack+rbp]          ; Restore C stack pointer
    add     rsp,(Fr_size-56)
    pop     rsi
    pop     rdi
    pop     r15                            ; Restore callee-save registers
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

;# Used when entering new code.  The argument and closure are on the stack
;# in case there is a GC before we enter the code.
PUBLIC X86AsmPopArgAndClosure
X86AsmPopArgAndClosure:
    pop     rdx
    pop     rax
#ifdef  POLYML32IN64
    jmp     qword ptr [rbx+rdx*4]
#else
    jmp     qword ptr [rdx]
#endif

;# This is used if the RTS sets up an exception.  It's probably no longer relevant.
PUBLIC  X86AsmRaiseException
X86AsmRaiseException:
raisex:
    mov     rcx,[ArgVector.HandlerRegister+rbp]
    jmp     qword ptr [rcx]

; RTS call to kill the current thread. 
PUBLIC  X86AsmKillSelf
X86AsmKillSelf:
    CALL_EXTRA RETURN_KILL_SELF

PUBLIC  X86AsmCallbackReturn
X86AsmCallbackReturn:
    CALL_EXTRA RETURN_CALLBACK_RETURN

PUBLIC  X86AsmCallbackException
X86AsmCallbackException:
    CALL_EXTRA RETURN_CALLBACK_EXCEPTION

; This implements atomic addition in the same way as atomic_increment
PUBLIC  X86AsmAtomicIncrement
X86AsmAtomicIncrement:
    mov     rax,rcx
; Use rcx and rax because they are volatile (unlike rbx on X86/64/Unix)
    mov     rcx,2
#ifdef  POLYML32IN64
    lock xadd [rax],ecx     ;# Rax is an absolute address but this is only a word
#else
    lock xadd [rax],rcx
#endif
    add     rcx,2
    mov     rax,rcx
    ret

CREATE_EXTRA_CALL MACRO index
PUBLIC  X86AsmCallExtra&index&
X86AsmCallExtra&index&:
    CALL_EXTRA index
    ENDM

CREATE_EXTRA_CALL RETURN_ARB_EMULATION
CREATE_EXTRA_CALL RETURN_HEAP_OVERFLOW
CREATE_EXTRA_CALL RETURN_STACK_OVERFLOW
CREATE_EXTRA_CALL RETURN_STACK_OVERFLOWEX

END
