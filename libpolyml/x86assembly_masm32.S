;
;  Title:  Assembly code routines for the poly system.
;  Author:    David Matthews
;  Copyright (c) David C. J. Matthews 2000-2020, 2027
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
;  eax: First argument to function.  Result of function call.
;  ebx: Second argument to function.
;  ecx: General register
;  edx: Closure pointer in call.
;  ebp: Points to memory used for extra registers
;  esi: General register.
;  edi: General register.
;  esp: Stack pointer.

.486
    .model  flat,c

Fr_Size             EQU     16		; Make this a multiple of 16 

; This is the argument vector passed in to X86AsmSwitchToPoly
; It is used to initialise the frame.  A few values are updated
; when ML returns.
ArgVector STRUCT
LocalMPointer       DWORD   ?
HandlerRegister     DWORD   ?
LocalMbottom        DWORD   ?
StackLimit          DWORD   ?
ExceptionPacket     DWORD   ?       ; Address of packet to raise
UnusedRequestCode   DB      ?       ; Byte: Io function to call.
UnusedFlag          DB      ?
ReturnReason        DB      ?       ; Byte: Reason for returning from ML.
Pflags              DB      ?       ; Byte: Flags to set - used in arbitrary precision
SaveCStack          DWORD   ?       ; Saved C stack pointer
ThreadId            DWORD   ?       ; My thread id
StackPtr            DWORD   ?       ; Stack pointer
EnterInterpreter    DWORD   ?
HeapOverFlowCall    DWORD   ?
StackOverFlowCall   DWORD   ?
StackOverFlowCallEx DWORD   ?
ArbEmulationCall    DWORD   ?
TrapHandlerEntry    DWORD   ?
SaveRAX             DWORD   ?
SaveRBX             DWORD   ?
SaveRCX             DWORD   ?
SaveRDX             DWORD   ?
SaveRSI             DWORD   ?
SaveRDI             DWORD   ?
SaveFP              WORD   ?         ; Actually larger
ArgVector ENDS


; Codes to indicate the reason for return.
RETURN_HEAP_OVERFLOW        EQU 1
RETURN_STACK_OVERFLOW       EQU 2
RETURN_STACK_OVERFLOWEX     EQU 3
RETURN_ARB_EMULATION        EQU 5
RETURN_CALLBACK_RETURN      EQU 6
RETURN_CALLBACK_EXCEPTION   EQU 7
RETURN_ENTER_INTERPRETER    EQU 4

;
; CODE STARTS HERE
;
    .CODE

; Enter ML code.  This is now only ever used to start a new thread.
; It is probably unnecessary to save the callee-save regs or load the ML regs.
PUBLIC  X86AsmSwitchToPoly
X86AsmSwitchToPoly:
    push    ebp                             ; Standard entry sequence
    mov     ebp,[8+esp]                     ; Address of argument vector
    push    ebx                             ; Push callee-save registers
    push    edi
    push    esi
    sub     esp,(Fr_size-12)                ; Allocate frame
    mov     [ArgVector.SaveCStack+ebp],esp
	mov     esp,[ArgVector.StackPtr+ebp]
    frstor  [ArgVector.SaveFP+ebp]
    mov     ah,[ArgVector.Pflags+ebp]
    sahf
    mov     eax,[ArgVector.SaveRAX+ebp]
    mov     ebx,[ArgVector.SaveRBX+ebp]
    mov     ecx,[ArgVector.SaveRCX+ebp]
    mov     edx,[ArgVector.SaveRDX+ebp]
    mov     esi,[ArgVector.SaveRSI+ebp]
    mov     edi,[ArgVector.SaveRDI+ebp]
    cld                                     ; Clear this just in case
    jmp     dword ptr [edx]

; Save all the registers and enter the trap handler.
; It is probably unnecessary to save the FP state now.
X86TrapHandler PROTO C

CallTrapHandler:
    mov     [ArgVector.SaveRAX+ebp],eax
    mov     [ArgVector.SaveRBX+ebp],ebx
    mov     [ArgVector.SaveRCX+ebp],ecx
    mov     [ArgVector.SaveRDX+ebp],edx
    mov     [ArgVector.SaveRSI+ebp],esi
    mov     [ArgVector.SaveRDI+ebp],edi
    FNSAVE  [ArgVector.SaveFP+ebp]          ; Save FP state.  Also resets the state so...
    FLDCW   [ArgVector.SaveFP+ebp]          ; ...load because we need the same rounding mode in the RTS
    mov     [ArgVector.StackPtr+ebp],esp    ; Save ML stack pointer
    mov     esp,[ArgVector.SaveCStack+ebp]  ; Restore C stack pointer
    sub     esp,12                          ; Align stack ptr - GCC prefers this
    push    [ArgVector.ThreadId+ebp]

    call    [ArgVector.TrapHandlerEntry+ebp]
    add     esp,16
	mov     esp,[ArgVector.StackPtr+ebp]
    mov     eax,[ArgVector.ExceptionPacket+ebp]
    cmp     eax,1                                           ; Did we raise an exception?
    jnz     raisexcept
    frstor  [ArgVector.SaveFP+ebp]
    mov     eax,[ArgVector.SaveRAX+ebp]
    mov     ebx,[ArgVector.SaveRBX+ebp]
    mov     ecx,[ArgVector.SaveRCX+ebp]
    mov     edx,[ArgVector.SaveRDX+ebp]
    mov     esi,[ArgVector.SaveRSI+ebp]
    mov     edi,[ArgVector.SaveRDI+ebp]
    cld                                     ; Clear this just in case
    ret
raisexcept:
    mov     ecx,[ArgVector.HandlerRegister+ebp]
    jmp     dword ptr [ecx]

; Define standard call macro.
; Defined as an Masm macro because there are multiple instructions.

CALL_EXTRA  MACRO   index
    mov     byte ptr [ArgVector.ReturnReason+ebp],index
    jmp     CallTrapHandler
ENDM

CREATE_EXTRA_CALL MACRO index
PUBLIC  X86AsmCallExtra&index&
X86AsmCallExtra&index&:
    CALL_EXTRA index
    ENDM


CREATE_EXTRA_CALL RETURN_ARB_EMULATION
CREATE_EXTRA_CALL RETURN_HEAP_OVERFLOW
CREATE_EXTRA_CALL RETURN_STACK_OVERFLOW
CREATE_EXTRA_CALL RETURN_STACK_OVERFLOWEX
CREATE_EXTRA_CALL RETURN_ENTER_INTERPRETER

END
