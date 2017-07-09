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

    val argThreadSelf: int (* Copied from X86CodeSgig *)

    datatype callKinds =
        Recursive
    |   ConstantCode of machineWord
    |   FullCall
    |   DirectReg of genReg

    datatype pregKind =
        PRegGeneral     (* A general register.  This can contain and address and be pushed to the stack. *)
    |   PRegUntagged    (* An untagged general register. Just used for short-term storage. Not valid on the stack. *)

    datatype argument =
        PReg of int * pregKind (* A pseudo-register - an abstract register. *)
    |   RealRegister of reg (* A real register. *)
    |   AddressConstant of machineWord (* A constant that is an address. *)
    |   IntegerConstant of LargeInt.int (* A non-address constant.  Will usually be shifted and tagged. *)
    |   MemoryLocation of { base: argument, offset: int, index: memoryIndex } (* A memory location.  Could be the stack. *)

    and memoryIndex =
        NoMemIndex | MemIndex1 of argument | MemIndex2 of argument | MemIndex4 of argument | MemIndex8 of argument

    (* Kinds of moves.
       WordToWord copies the whole value.  This is the "normal" move.
       ByteToByte overwrites only a byte.  This is used to store a byte.
       ByteToWordZeroExtend loads a byte and zero extends it to fill the
       destination.  This is used when loading a byte.
       Bit16ToWordZeroExtend is used when loading a 16-bit word from C-memory.
       Bit32ToWordZeroExtend is used in 64-bit mode to load a 32-bit word from C-memory.
       WordTo16Bit and WordTo32Bit are used for the corresponding stores. *)
    datatype moveKind =
        WordToWord | ByteToByte | ByteToWordZeroExtend |
        Bit16ToWordZeroExtend | Bit32ToWordZeroExtend | WordTo16Bit | WordTo32Bit

    datatype iLabel = ILabel of int
    
    (* The reference to a condition code. *)
    datatype ccRef = CcRef of int

    datatype x86ICode =
        (* Load, store and move, depending on the kinds of arguments.  Only certain combinations
           are supported. *)
        MoveArgument of { source: argument, dest: argument, kind: moveKind }

    |   MoveStackArgToPreg of { offset: int, dest: argument } (* Load an argument or the return address *)
    
        (* Call or jump to a function.  If the code address is a constant it is passed here.
           Otherwise the address is obtained by indirecting through rdx which has been loaded
           as one of the argument registers.  The result is stored in the destination register.
           If isTail is true this is tail-recursive and should be a jump. *)
    |   FunctionCall of
            { callKind: callKinds, regArgs: (argument * genReg) list,
              stackArgs: argument list, dest: argument, isTail: bool}

        (* Allocate a fixed sized piece of memory.  The size is the number of words
           required.  This sets the length word including the flags bits. *)
    |   AllocateMemoryOperation of { size: int, flags: Word8.word, dest: argument}

        (* Allocate a piece of memory whose size is not known at compile-time.  The size
           argument is the number of words.  If the initialiser is provided
           each word of the new memory is set to the value. *)
    |   AllocateMemoryVariable of { size: argument, flags: argument, dest: argument, initialiser: argument option }

        (* Signal that a tuple has been fully initialised.  Really a check in the
           low-level code-generator. *)
    |   InitialisationComplete of { dest: argument}

        (* Begin a loop.  A set of loop registers are initialised and the loop is entered.
           The loopLabel in all of these is for checking only, at least at the moment. *)
    |   StartLoop of { arguments: {source: argument, loopReg: argument} list, loopLabel: iLabel }

        (* End a loop.  Causes the loop stack to be popped. *)
    |   EndLoop of { loopLabel: iLabel }

        (* Within a loop the loop registers are updated from the source registers and
           a jump is made back to the containing StartLoop *)
    |   JumpLoop of { arguments: {source: argument, loopReg: argument} list, loopLabel: iLabel }
    
    |   RaiseExceptionPacket of { packet: argument }

        (* Reserve a contiguous area on the stack to receive a result tuple. *)
    |   ReserveContainer of { size: int, address: argument }

        (* Indexed case. *)
    |   IndexedCaseOperation of { testReg: argument, workReg: argument, cases: iLabel list, startValue: word }

        (* Lock a mutable cell by turning off the mutable bit. *)
    |   LockMutable of { addr: argument }

        (* Forward branches. *)
    |   ForwardJumpLabel of { label: iLabel, result: argument option }
    
    |   UnconditionalForwardJump of { label: iLabel }

        (* Conditional branch. *)
    |   ConditionalForwardJump of { ccRef: ccRef, condition: branchOps, label: iLabel }

        (* Compare two word values. *)
    |   WordComparison of { arg1: argument, arg2: argument, ccRef: ccRef }
    
        (* Exception handling.  - Set up an exception handler. *)
    |   PushExceptionHandler of { handlerAddr: argument, handleStart: iLabel }

        (* End of a handled section.  Restore the previous handler. *)
    |   PopExceptionHandler of { handlerAddr: argument, resultReg: argument, workReg: argument }

        (* Marks the start of a handler.  This sets the stack pointer and
           restores the old handler.  As a special case it must ensure that rax is
           preserved because that contains the exception packet. *) 
    |   BeginHandler of { handleStart: iLabel, workReg: argument }

        (* Return from the function. *)
    |   ReturnResultFromFunction of { resultReg: argument, numStackArgs: int }
    
        (* Arithmetic or logical operation.  These can set the condition codes. *)
    |   ArithmeticFunction of
            { oper: arithOp, resultReg: argument, operand1: argument, operand2: argument, ccRef: ccRef }

        (* Test the tag bit of a word.  Sets the Zero bit if the value is an address i.e. untagged. *)
    |   TestTagBit of { arg: argument, ccRef: ccRef }

        (* Push a value to the stack.  Added during translation phase. *)
    |   PushValue of { arg: argument }

        (* Remove items from the stack.  Added during translation phase. *)
    |   ResetStackPtr of { numWords: int }

        (* Tag a value by shifting and setting the tag bit. *)
    |   TagValue of { source: argument, dest: argument }

        (* Shift a value to remove the tag bit. *)
    |   UntagValue of { source: argument, dest: argument, isSigned: bool }

        (* This provides the LEA instruction which can be used for various sorts of arithmetic.
           The base register is optional in this case. *)
    |   LoadEffectiveAddress of { base: argument option, offset: int, index: memoryIndex, dest: argument }

        (* Shift a word by an amount that can either be a constant or a register. *)
    |   ShiftOperation of { shift: shiftType, resultReg: argument, operand: argument, shiftAmount: argument, ccRef: ccRef }

        (* Multiplication.  We can use signed multiplication for both fixed precision and word (unsigned)
           multiplication.  There are various forms of the instruction including a three-operand
           version. *)
    |   Multiplication of { resultReg: argument, operand1: argument, operand2: argument, ccRef: ccRef }

        (* Division.  This takes a register pair, always RDX:RAX, divides it by the operand register and
           puts the quotient in RAX and remainder in RDX.  At the abstract level we represent all of
           these by pRegs. *)
    |   Division of { isSigned: bool, dividend: argument, divisor: argument,
                      quotient: argument, remainder: argument }

        (* Atomic exchange and addition.   This is executed with a lock prefix and is used
           for atomic increment and decrement for mutexes.
           Before the operation the source contains an increment.  After the operation
           the source contains the old value of the destination and the destination
           has been updated with its old value added to the increment. *)
    |   AtomicExchangeAndAdd of { destAddr: argument, source: argument }

        (* Create a "box" of a single-word "byte" cell and store the source into it.
           This can be implemented using AllocateMemoryOperation but the idea is to
           allow the transform layer to recognise when a value is being boxed and
           then unboxed and remove unnecessary allocation. *)
    |   BoxLargeWord of { source: argument, dest: argument }

        (* Compare two vectors of bytes and set the condition code on the result.
           In general vec1Addr and vec2Addr will be pointers inside memory cells
           so have to be untagged registers. *)
    |   CompareByteVectors of
            { vec1Addr: argument, vec2Addr: argument, length: argument, ccRef: ccRef }

        (* Move a block of bytes (isByteMove true) or words (isByteMove false).  The length is the
           number of items (bytes or words) to move. *)
    |   BlockMove of { srcAddr: argument, destAddr: argument, length: argument, isByteMove: bool }

        (* Floating point comparison. *)
    |   CompareFloatingPt of { arg1: argument, arg2: argument, ccRef: ccRef }

        (* The X87 FP unit does not generate condition codes directly.  We have to
           load the cc into RAX and test it there. *)
    |   GetFloatingPtCondition of { ccRef: ccRef, dest: argument }

    val codeAsX86Code: {icode: x86ICode list, maxLabels: int, stackRequired: int, inputRegisters: genReg list,
            functionName: string, debugSwitches: Universal.universal list } -> address

    structure Sharing:
    sig
        type genReg         = genReg
        and  argument       = argument
        and  iLabel         = iLabel
        and  x86ICode       = x86ICode
        and  branchOps      = branchOps
    end
end;
