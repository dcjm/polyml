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
UnusedProgramCtr    DWORD   ?
HeapOverFlowCall    DWORD   ?
StackOverFlowCall   DWORD   ?
StackOverFlowCallEx DWORD   ?
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
RETURN_KILL_SELF            EQU 9

;
; CODE STARTS HERE
;
    .CODE

; Load the registers from the ML stack and jump to the code.
; This is used to start ML code.
; The argument is the address of the MemRegisters struct and goes into ebp.
; This is the general code for switching control to ML.  There are a number of cases to consider:
; 1.  Initial entry to root function or a new thread.  Needs to load EDX at least.
; 2.  Normal return from an RTS call.  Could just do a simple return.
; 3.  Exception raised in RTS call.
; 4.  Callback from C to an ML function.  In effect this is a coroutine. Similar to 1.
; 5.  Return from "trap" i.e. Heap/Stack overflow.  Stack-overflow can result in an exception
;     either because the stack can't be grown or because Interrupt has been raised.
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
    mov     eax,[ArgVector.ExceptionPacket+ebp]
    cmp     eax,1                                           ; Did we raise an exception?
    jnz     raisex
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
    ret

; Code to save the state and switch to C
; This saves the full register state.
SaveFullState:
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
    add     esp,(Fr_size-12)
    pop     esi                             ; Restore saved registers
    pop     edi
    pop     ebx
    pop     ebp
    ret

; Used when entering new code.  The argument and closure are on the stack
; in case there is a GC before we enter the code.
PUBLIC X86AsmPopArgAndClosure
X86AsmPopArgAndClosure:
    pop     edx
    pop     eax
    jmp     dword ptr [edx]

; This is used if the RTS sets up an exception.  It's probably no longer relevant.
PUBLIC  X86AsmRaiseException
X86AsmRaiseException:
raisex:
    mov     ecx,[ArgVector.HandlerRegister+ebp]
    jmp     dword ptr [ecx]

; Define standard call macro.
; Defined as an Masm macro because there are multiple instructions.

CALL_EXTRA  MACRO   index
    push    ecx
    mov     byte ptr [ArgVector.ReturnReason+ebp],index
    pop     ecx
    jmp     SaveFullState
ENDM

; Terminate the current thread
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
; N.B. It is called from the RTS so uses C linkage conventions.
PUBLIC  X86AsmAtomicIncrement
X86AsmAtomicIncrement:
    mov     eax,4[esp]
; Use ecx and eax because they are volatile (unlike ebx on X86/64/Unix)
    mov     ecx,2
    lock xadd [eax],ecx
    add     ecx,2
    mov     eax,ecx
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
