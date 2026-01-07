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
TrapHandlerEntry    QWORD   ?
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
RETURN_ENTER_INTERPRETER    EQU 4
RETURN_ARB_EMULATION        EQU 5
;
; CODE STARTS HERE
;
    .CODE

; Define standard call macro.

CALL_EXTRA  MACRO   index
    mov     byte ptr [ArgVector.ReturnReason+rbp],index
    jmp     CallTrapHandler
ENDM


; Enter ML code.  This is now only ever used to start a new thread.
; It is probably unnecessary to save the callee-save regs or load the ML regs.
; This does not set up a correct frame because we do not want to reserve a register for
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
    mov     rax,[ArgVector.SaveRAX+rbp]
    cld                                     ; Clear this just in case
#ifdef POLYML32IN64
#if(POLYML32IN64==2)
    jmp     qword ptr [rbx+rdx*4]
#elif(POLYML32IN64==4)
    jmp     qword ptr [rbx+rdx*8]
#else
#error "If POLYML32IN64 is set it must be 2 or 4 on the X86"
#endif

#else
    jmp     qword ptr [rdx]
#endif

; Everything up to here is considered as part of the X86AsmSwitchToPoly proc
X86AsmSwitchToPoly ENDP

; Save all the registers and enter the trap handler.
; It is probably unnecessary to save the FP state now.
X86TrapHandler PROTO C

CallTrapHandler:
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
    mov     [ArgVector.StackPtr+rbp],rsp    ; Save ML stack pointer
    mov     [ArgVector.LocalMpointer+rbp],r15       ; Save back heap pointer
    mov     rsp,[ArgVector.SaveCStack+rbp]          ; Restore C stack pointer
    sub     rsp,32                          ; Create Windows save area
    mov     rcx,[ArgVector.ThreadId+rbp]
    call    [ArgVector.TrapHandlerEntry+rbp]
    add     rsp,32
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
    jnz     raisexcept
    mov     rax,[ArgVector.SaveRAX+rbp]
    cld                                     ; Clear this just in case
    ret

raisexcept:
    mov     rcx,[ArgVector.HandlerRegister+rbp]
    jmp     qword ptr [rcx]

CREATE_EXTRA_CALL MACRO index
PUBLIC  X86AsmCallExtra&index&
X86AsmCallExtra&index&:
    CALL_EXTRA index
    ENDM

CREATE_EXTRA_CALL RETURN_HEAP_OVERFLOW
CREATE_EXTRA_CALL RETURN_STACK_OVERFLOW
CREATE_EXTRA_CALL RETURN_STACK_OVERFLOWEX
CREATE_EXTRA_CALL RETURN_ARB_EMULATION
CREATE_EXTRA_CALL RETURN_ENTER_INTERPRETER

END
