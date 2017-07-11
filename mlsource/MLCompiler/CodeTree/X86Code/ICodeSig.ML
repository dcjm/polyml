(*
    Signature for the high-level X86 code

    Copyright David C. J. Matthews 2016

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
*)

signature ICodeSig =
sig
    type machineWord = Address.machineWord
    type address = Address.address

    (* Registers. *)
    datatype genReg = GeneralReg of Word8.word * bool
    and fpReg = FloatingPtReg of Word8.word
    and xmmReg = SSE2Reg of Word8.word
    
    datatype reg =
        GenReg of genReg
    |   FPReg of fpReg
    |   XMMReg of xmmReg
    
    val regRepr: reg -> string
    val nReg:   reg -> int

    val isX64: bool and is32bit: LargeInt.int -> bool

    (* Should we use SSE2 or X87 floating point? *)
    datatype fpMode = FPModeSSE2 | FPModeX87
    val fpMode: fpMode

    val eax: genReg and ebx: genReg and ecx: genReg and edx: genReg
    and edi: genReg and esi: genReg and esp: genReg and ebp: genReg
    and r8:  genReg and r9:  genReg and r10: genReg and r11: genReg
    and r12: genReg and r13: genReg and r14: genReg and r15: genReg
    and fp0: fpReg and fp1: fpReg and fp2: fpReg and fp3: fpReg
    and fp4: fpReg and fp5: fpReg and fp6: fpReg and fp7: fpReg
    and xmm0:xmmReg and xmm1:xmmReg and xmm2:xmmReg and xmm3:xmmReg
    and xmm4:xmmReg and xmm5:xmmReg and xmm6:xmmReg

    datatype branchOps =
        JO | JNO | JE | JNE | JL | JGE | JLE | JG | JB | JNB | JNA | JA | JP | JNP
    and      arithOp = ADD | OR (*|ADC | SBB*) | AND | SUB | XOR | CMP
    and      shiftType = SHL | SHR | SAR

    datatype boxKind = BoxLargeWord | BoxFloat

    and      fpOps = FADD | FMUL | FCOM | FCOMP | FSUB | FSUBR | FDIV | FDIVR
    and      fpUnaryOps = FABS | FCHS | FLD1 | FLDZ
    and      sse2Operations =
        SSE2Move | SSE2Comp | SSE2Add | SSE2Sub | SSE2Mul | SSE2Div | SSE2Xor |
        SSE2And | SSE2MoveSingle | SSE2DoubleToFloat

    val memRegThreadSelf: int (* Copied from X86CodeSig *)

    datatype callKinds =
        Recursive
    |   ConstantCode of machineWord
    |   FullCall
    |   DirectReg of genReg

    datatype pregKind =
        PRegGeneral     (* A general register.  This can contain and address and be pushed to the stack. *)
    |   PRegUntagged    (* An untagged general register. Just used for short-term storage. Not valid on the stack. *)

    datatype abstract = PReg of int * pregKind (* A pseudo-register - an abstract register. *)

    datatype 'reg argument =
        RegisterArgument of 'reg
    |   AddressConstant of machineWord (* A constant that is an address. *)
    |   IntegerConstant of LargeInt.int (* A non-address constant.  Will usually be shifted and tagged. *)
    |   MemoryLocation of { base: 'reg, offset: int, index: 'reg memoryIndex } (* A memory location. *)
        (* Offset on the stack.  The "adjustment" is a temporary hack. *)
    |   StackLocation of { wordOffset: int, adjustment: int }

    and 'reg memoryIndex =
        NoMemIndex | MemIndex1 of 'reg | MemIndex2 of 'reg | MemIndex4 of 'reg | MemIndex8 of 'reg

    (* Kinds of moves.
       MoveWord - Move a whole word - 64-bits in 64-bit mode, 32-bits in 32-bit mode.
       MoveByte - When loading, load a byte and zero extend.
       Move16Bit - Used for C-memory loads and stores.  Zero extends on load.
       Move32Bit - Used for C-memory loads and stores in 64-bit mode.
       MoveFloat - Load and store a single-precision value
       MoveDouble - Load and store a double-precision value. *)
    datatype moveKind =
        MoveWord | MoveByte | Move16Bit | Move32Bit | MoveFloat | MoveDouble

    datatype iLabel = ILabel of int
    
    (* The reference to a condition code. *)
    datatype ccRef = CcRef of int

    datatype 'reg x86ICode =
        (* Move a value into a register. *)
        LoadArgument of { source: 'reg argument, dest: 'reg, kind: moveKind }
        
        (* Store a value into memory.  The source will usually be a register but could be
           a constant depending on the value. *)
    |   StoreArgument of { source: 'reg argument, base: 'reg, offset: int, index: 'reg memoryIndex, kind: moveKind }

        (* Load an entry from the "memory registers".  Used just for ThreadSelf. *)
    |   LoadMemReg of { offset: int, dest: 'reg }

        (* Exchange two registers. *)
    |   ExchangeRegisters of { regX: 'reg, regY: 'reg }

        (* Start of function.  Set the register arguments. *)
    |   BeginFunction of { regArgs: ('reg * reg) list }

        (* Call a function.  If the code address is a constant it is passed here.
           Otherwise the address is obtained by indirecting through rdx which has been loaded
           as one of the argument registers.  The result is stored in the destination register. *)
    |   FunctionCall of
            { callKind: callKinds, regArgs: ('reg argument * reg) list,
              stackArgs: 'reg argument list, dest: 'reg}

        (* Jump to a tail-recursive function.  This is similar to FunctionCall
           but complicated for stack arguments because the stack and the return
           address need to be overwritten.  We could actually include the
           return address among the stackArgs but leave that for the moment.
           stackAdjust is the number of words to remove (positive) or add
           (negative) to the stack before the call. *)
    |   TailRecursiveCall of
            { callKind: callKinds, regArgs: ('reg argument * reg) list,
              stackArgs: {src: 'reg argument, stack: int} list,
              returnAddr: {srcStack: int, stack: int},
              stackAdjust: int }

        (* Allocate a fixed sized piece of memory.  The size is the number of words
           required.  This sets the length word including the flags bits.
           saveRegs is the list of registers that need to be saved if we
           need to do a garbage collection. *)
    |   AllocateMemoryOperation of { size: int, flags: Word8.word, dest: 'reg, saveRegs: 'reg list }

        (* Allocate a piece of memory whose size is not known at compile-time.  The size
           argument is the number of words. *)
    |   AllocateMemoryVariable of { size: 'reg, dest: 'reg, saveRegs: 'reg list }

        (* Initialise a piece of memory.  N.B. The size is an untagged value containing
           the number of words.  This uses REP STOSL/Q so addr must be rdi, size must be
           rcx and init must be rax. *)
    |   InitialiseMem of { size: 'reg, addr: 'reg, init: 'reg }

        (* Signal that a tuple has been fully initialised.  Really a check in the
           low-level code-generator. *)
    |   InitialisationComplete

        (* Begin a loop.  A set of loop registers are initialised and the loop is entered.
           The loopLabel is the start of the loop and all jumps come back to it. *)
    |   StartLoop of { loopLabel: iLabel }

        (* End a loop.  This is a marker that is used at the higher levels.
           It doesn't generate any code. *)
    |   EndLoop of { loopLabel: iLabel, staticRegs: 'reg list }

        (* Within a loop the loop registers are updated from the source registers and
           a jump is made back to the containing StartLoop *)
    |   JumpLoop of
            { regArgs: ('reg argument * 'reg) list, stackArgs: ('reg argument * int) list,
              stackAdjust: int, loopLabel: iLabel, checkInterrupt: 'reg list option }

        (* Raise an exception.  The packet is always loaded into rax. *)
    |   RaiseExceptionPacket of { packet: 'reg argument }

        (* Reserve a contiguous area on the stack to receive a result tuple. *)
    |   ReserveContainer of { size: int, address: 'reg }

        (* Indexed case. *)
    |   IndexedCaseOperation of { testReg: 'reg, workReg: 'reg, cases: iLabel list, startValue: word }

        (* Lock a mutable cell by turning off the mutable bit. *)
    |   LockMutable of { addr: 'reg }

        (* Forward branches. *)
    |   ForwardJumpLabel of { label: iLabel, result: 'reg option }
    
    |   UnconditionalForwardJump of { label: iLabel }

        (* Conditional branch. *)
    |   ConditionalForwardJump of { ccRef: ccRef, condition: branchOps, label: iLabel }

        (* Compare two word values. *)
    |   WordComparison of { arg1: 'reg argument, arg2: 'reg argument, ccRef: ccRef }
    
        (* Exception handling.  - Set up an exception handler. *)
    |   PushExceptionHandler of { workReg: 'reg, handleStart: iLabel }

        (* End of a handled section.  Restore the previous handler. *)
    |   PopExceptionHandler of { resultReg: 'reg option, workReg: 'reg }

        (* Marks the start of a handler.  This sets the stack pointer and
           restores the old handler.  Sets the exception packet register. *) 
    |   BeginHandler of { handleStart: iLabel, packetReg: 'reg, workReg: 'reg }

        (* Return from the function. *)
    |   ReturnResultFromFunction of { resultReg: 'reg, numStackArgs: int }
    
        (* Arithmetic or logical operation.  These can set the condition codes. *)
    |   ArithmeticFunction of
            { oper: arithOp, resultReg: 'reg, operand1: 'reg argument, operand2: 'reg argument, ccRef: ccRef }

        (* Test the tag bit of a word.  Sets the Zero bit if the value is an address i.e. untagged. *)
    |   TestTagBit of { arg: 'reg argument, ccRef: ccRef }

        (* Push a value to the stack.  Added during translation phase. *)
    |   PushValue of { arg: 'reg argument }

        (* Remove items from the stack.  Added during translation phase. *)
    |   ResetStackPtr of { numWords: int }

        (* Tag a value by shifting and setting the tag bit. *)
    |   TagValue of { source: 'reg, dest: 'reg }

        (* Shift a value to remove the tag bit. *)
    |   UntagValue of { source: 'reg argument, dest: 'reg, isSigned: bool }

        (* This provides the LEA instruction which can be used for various sorts of arithmetic.
           The base register is optional in this case. *)
    |   LoadEffectiveAddress of { base: 'reg option, offset: int, index: 'reg memoryIndex, dest: 'reg }

        (* Shift a word by an amount that can either be a constant or a register. *)
    |   ShiftOperation of { shift: shiftType, resultReg: 'reg, operand: 'reg argument, shiftAmount: 'reg argument, ccRef: ccRef }

        (* Multiplication.  We can use signed multiplication for both fixed precision and word (unsigned)
           multiplication.  There are various forms of the instruction including a three-operand
           version. *)
    |   Multiplication of { resultReg: 'reg, operand1: 'reg argument, operand2: 'reg argument, ccRef: ccRef }

        (* Division.  This takes a register pair, always RDX:RAX, divides it by the operand register and
           puts the quotient in RAX and remainder in RDX.  At the abstract level we represent all of
           these by pRegs.  The divisor can be either a register or a memory location. *)
    |   Division of { isSigned: bool, dividend: 'reg, divisor: 'reg argument, quotient: 'reg, remainder: 'reg }

        (* Atomic exchange and addition.   This is executed with a lock prefix and is used
           for atomic increment and decrement for mutexes.
           Before the operation the source contains an increment.  After the operation
           the source contains the old value of the destination and the destination
           has been updated with its old value added to the increment.
           The destination is actually the word pointed at by "base". *)
    |   AtomicExchangeAndAdd of { base: 'reg, source: 'reg }

        (* Create a "box" of a single-word "byte" cell and store the source into it.
           This can be implemented using AllocateMemoryOperation but the idea is to
           allow the transform layer to recognise when a value is being boxed and
           then unboxed and remove unnecessary allocation. *)
    |   BoxValue of { boxKind: boxKind, source: 'reg, dest: 'reg, saveRegs: 'reg list }

        (* Compare two vectors of bytes and set the condition code on the result.
           In general vec1Addr and vec2Addr will be pointers inside memory cells
           so have to be untagged registers. *)
    |   CompareByteVectors of
            { vec1Addr: 'reg, vec2Addr: 'reg, length: 'reg, ccRef: ccRef }

        (* Move a block of bytes (isByteMove true) or words (isByteMove false).  The length is the
           number of items (bytes or words) to move. *)
    |   BlockMove of { srcAddr: 'reg, destAddr: 'reg, length: 'reg, isByteMove: bool }

        (* Floating point comparison. *)
    |   CompareFloatingPt of { arg1: 'reg argument, arg2: 'reg argument, ccRef: ccRef }

        (* The X87 FP unit does not generate condition codes directly.  We have to
           load the cc into RAX and test it there. *)
    |   X87FPGetCondition of { ccRef: ccRef, dest: 'reg }

        (* Binary floating point operations on the X87. *)
    |   X87FPArith of { opc: fpOps, resultReg: 'reg, arg1: 'reg argument, arg2: 'reg argument }

        (* Floating point operations: negate and set sign positive. *)
    |   X87FPUnaryOps of { fpOp: fpUnaryOps, dest: 'reg, source: 'reg argument }

        (* Load a fixed point value as a floating point value. *)
    |   FloatFixedInt of { dest: 'reg, source: 'reg argument }

        (* Binary floating point operations using SSE2 instructions. *)
    |   SSE2FPArith of { opc: sse2Operations, resultReg: 'reg, arg1: 'reg argument, arg2: 'reg argument }

    val codeAsX86Code: {icode: reg x86ICode list, maxLabels: int, stackRequired: int, inputRegisters: reg list,
            functionName: string, debugSwitches: Universal.universal list } -> address

    val printICodeConcrete: reg x86ICode * (string -> unit) -> unit
    and printICodeAbstract: abstract x86ICode * (string -> unit) -> unit
    
    val indexRegister: 'reg memoryIndex -> 'reg option
    
    (* Destinations used in move. *)
    datatype destinations = RegDest of reg | StackDest of int

    structure Sharing:
    sig
        type genReg         = genReg
        and  'reg argument  = 'reg argument
        and  iLabel         = iLabel
        and  'reg x86ICode  = 'reg x86ICode
        and  branchOps      = branchOps
        and  reg            = reg
        and abstract        = abstract
        and destinations    = destinations
    end
end;
