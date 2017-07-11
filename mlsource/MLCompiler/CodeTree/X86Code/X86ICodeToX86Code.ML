(*
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

functor X86ICodeToX86Code(

    structure X86CODE: X86CODESIG

    structure X86OPTIMISE:
    sig
        type operation
        type code
        type operations = operation list

        val optimise: code * operations -> operations

        structure Sharing:
        sig
            type operation = operation
            type code = code
        end
    end

    structure DEBUG: DEBUGSIG
    
    sharing X86CODE.Sharing = X86OPTIMISE.Sharing
): ICodeSig =
struct
    open X86CODE

    datatype fpMode = FPModeSSE2 | FPModeX87

    (* For the moment use SSE2 only on X86/64. Not all 32-bit processors support SSE2. *)
    val fpMode: fpMode = if isX64 then FPModeSSE2 else FPModeX87

    open Address

    datatype pregKind =
        PRegGeneral     (* A general register.  This can contain and address and be pushed to the stack. *)
    |   PRegUntagged    (* An untagged general register. Just used for short-term storage. Not valid on the stack. *)

    datatype abstract = PReg of int * pregKind (* A pseudo-register - an abstract register. *)

    datatype 'reg argument =
        RegisterArgument of 'reg
    |   AddressConstant of machineWord (* A constant that is an address. *)
    |   IntegerConstant of LargeInt.int (* A non-address constant.  Will usually be shifted and tagged. *)
    |   MemoryLocation of { base: 'reg, offset: int, index: 'reg memoryIndex } (* A memory location.  Could be the stack. *)
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
    and ccRef = CcRef of int

    datatype boxKind = BoxLargeWord | BoxFloat

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
    |   StartLoop of { regArgs: ('reg argument * 'reg) list, loopLabel: iLabel }

        (* End a loop.  This is a marker that is used at the higher levels.
           It doesn't generate any code. *)
    |   EndLoop of { loopLabel: iLabel, staticRegs: 'reg list }

        (* Within a loop the loop registers are updated from the source registers and
           a jump is made back to the containing StartLoop *)
    |   JumpLoop of { regArgs: ('reg argument * 'reg) list, stackArgs: ('reg argument * int) list, stackAdjust: int, loopLabel: iLabel }

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
 
    local

        fun printLabel(ILabel i, stream) = stream("L"^Int.toString i)
        
        fun printIndex(NoMemIndex, _, _) = ()
        |   printIndex(MemIndex1 i, stream, printReg) = (stream "["; printReg(i, stream); stream "*1]")
        |   printIndex(MemIndex2 i, stream, printReg) = (stream "["; printReg(i, stream); stream "*2]")
        |   printIndex(MemIndex4 i, stream, printReg) = (stream "["; printReg(i, stream); stream "*4]")
        |   printIndex(MemIndex8 i, stream, printReg) = (stream "["; printReg(i, stream); stream "*8]")

        fun printArg(RegisterArgument reg, stream, printReg) = printReg(reg, stream)
        |   printArg(AddressConstant m, stream, _) = stream(stringOfWord m)
        |   printArg(IntegerConstant i, stream, _) = stream(LargeInt.toString i)
        |   printArg(MemoryLocation{base, offset, index}, stream, printReg) =
            (
                stream(Int.toString offset ^ "(");
                printReg(base, stream);
                stream ")";
                printIndex(index, stream, printReg)
            )
        |   printArg(StackLocation{wordOffset, adjustment}, stream, _) =
                stream(Int.toString wordOffset ^ "+" ^ Int.toString adjustment ^ "(stackptr)")
            
        fun printSaves([], _, _) = ()
        |   printSaves([areg], printReg, stream) = printReg(areg, stream)
        |   printSaves(areg::more, printReg, stream) =
                (printReg(areg, stream); stream ","; printSaves(more, printReg, stream))

        fun printICode(LoadArgument{source, dest, kind}, stream, printReg) =
            (
                case kind of
                    MoveWord => stream "\tLoadWord\t"
                |   MoveByte => stream "\tLoadByte\t"
                |   Move16Bit => stream "\tLoad16Bit\t"
                |   Move32Bit => stream "\tLoad32Bit\t"
                |   MoveFloat => stream "\tLoadFloat\t"
                |   MoveDouble => stream "\tLoadDouble\t";
                printArg(source,  stream, printReg);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(StoreArgument{source, base, offset, index, kind}, stream, printReg) =
            (
                case kind of
                    MoveWord => stream "\tStoreWord\t"
                |   MoveByte => stream "\tStoreByte\t"
                |   Move16Bit => stream "\tStore16Bit\t"
                |   Move32Bit => stream "\tStore32Bit\t"
                |   MoveFloat => stream "\tStoreFloat\t"
                |   MoveDouble => stream "\tStoreDouble\t";
                printArg(source,  stream, printReg);
                stream " => ";
                stream(Int.toString offset ^ "(");
                printReg(base, stream);
                stream ")";
                printIndex(index, stream, printReg)
            )

        |   printICode(LoadMemReg { offset, dest}, stream, printReg) =
                ( stream "\tLoadMemReg\t"; stream(Int.toString offset); stream " => "; printReg(dest, stream) )

        |   printICode(ExchangeRegisters { regX, regY}, stream, printReg) =
                ( stream "\tExchangeRegs\t"; printReg(regX, stream); stream " <=> "; printReg(regY, stream) )

        |   printICode(BeginFunction {regArgs}, stream, printReg) =
            (
                stream "\tBeginFunction\t";
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printReg(arg, stream); stream " ")) regArgs
            )

        |   printICode(FunctionCall{callKind, regArgs, stackArgs, dest}, stream, printReg) =
            (
                stream "\tFunctionCall\t";
                case callKind of
                    Recursive => stream "recursive "
                |   ConstantCode m => (stream(stringOfWord m); stream " ")
                |   FullCall => ()
                |   DirectReg r => (stream(regRepr(GenReg r)); stream " ");
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printArg(arg, stream, printReg); stream " ")) regArgs;
                List.app(fn arg => (stream "p="; printArg(arg, stream, printReg); stream " ")) stackArgs;
                stream "=> "; printReg(dest, stream)
            )

        |   printICode(TailRecursiveCall{callKind, regArgs, stackArgs, returnAddr={srcStack, stack}, stackAdjust}, stream, printReg) =
            (
                stream "\tTailCall\t";
                case callKind of
                    Recursive => stream "recursive "
                |   ConstantCode m => (stream(stringOfWord m); stream " ")
                |   FullCall => ()
                |   DirectReg r => (stream(regRepr(GenReg r)); stream " ");
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printArg(arg, stream, printReg); stream " ")) regArgs;
                List.app(fn {src, stack} => (stream (Int.toString stack); stream "<="; printArg(src, stream, printReg); stream " ")) stackArgs;
                stream "ret "; stream(Int.toString stack); stream "<="; stream(Int.toString srcStack);
                stream "adj="; stream(Int.toString stackAdjust)
            )

        |   printICode(AllocateMemoryOperation{size, flags, dest, saveRegs}, stream, printReg) =
            (
                stream "\tAllocateMemory\t";
                stream(concat["s=", Int.toString size, ",f=", Word8.toString flags, " => "]);
                printReg(dest, stream);
                stream " save="; printSaves(saveRegs, printReg, stream)
            )

        |   printICode(AllocateMemoryVariable{size, dest, saveRegs}, stream, printReg) =
            (
                stream "\tAllocateMemory\t";
                stream "s="; printReg(size, stream);
                stream " => "; printReg(dest, stream);
                stream " save="; printSaves(saveRegs, printReg, stream)
            )

        |   printICode(InitialiseMem{size, addr, init}, stream, printReg) =
            (
                stream "\tInitialiseMem\t";
                stream "s="; printReg(size, stream);
                stream ",i="; printReg(init, stream);
                stream ",a="; printReg(addr, stream)
            )

        |   printICode(InitialisationComplete, stream, _) = stream "\tInitComplete"

        |   printICode(StartLoop{regArgs, loopLabel}, stream, printReg) =
            (
                printLabel(loopLabel, stream);
                stream ":\tStartLoop\t";
                List.app(
                    fn (source, loopReg) => (printReg(loopReg, stream); stream "="; printArg(source, stream, printReg); stream " ")
                    ) regArgs
            )

        |   printICode(EndLoop{loopLabel, ...}, stream, _) = (stream "\tEndLoop\t"; printLabel(loopLabel, stream))

        |   printICode(JumpLoop{regArgs, stackArgs, loopLabel, stackAdjust }, stream, printReg) =
            (
                stream "\tJumpLoop\t";
                List.app(
                    fn (source, loopReg) => (printReg(loopReg, stream); stream "="; printArg(source, stream, printReg); stream " ")
                    ) regArgs;
                List.app(
                    fn (source, stack) => (stream("sp" ^ Int.toString stack); stream "="; printArg(source, stream, printReg); stream " ")
                    ) stackArgs;
                printLabel(loopLabel, stream);
                stream " adj="; stream(Int.toString stackAdjust)
            )

        |   printICode(RaiseExceptionPacket{packet}, stream, printReg) = (stream "\tRaise\t"; printArg(packet, stream, printReg))

        |   printICode(ReserveContainer{size, address}, stream, printReg) =
            (stream "\tReserveContainer\t"; stream(Int.toString size); stream "=>"; printReg(address, stream))

        |   printICode(IndexedCaseOperation{testReg, workReg, cases, startValue}, stream, printReg) =
            (
                stream "\tIndexedCase\t";
                stream "test="; printReg(testReg, stream);
                stream "work="; printReg(workReg, stream);
                stream "from="; stream(Word.toString startValue);
                List.app(fn l => (stream " "; printLabel(l, stream))) cases
            )

        |   printICode(LockMutable{addr}, stream, printReg) = (stream "\tLockMutable\t"; printReg(addr, stream))
        
        |   printICode(ForwardJumpLabel{label, ...}, stream, _) = (printLabel(label, stream); stream ":\t\t")

        |   printICode(UnconditionalForwardJump{label}, stream, _) = (stream "\tJump\t"; printLabel(label, stream))

        |   printICode(ConditionalForwardJump{condition, label, ...}, stream, _) =
            (
                case condition of
                    JO => stream "\tJumpOverflow\t"
                |   JNO => stream "\tJumpNoOverflow\t"
                |   JE => stream "\tJumpEqual\t"
                |   JNE => stream "\tJumpNotEqual\t"
                |   JL => stream "\tJumpLessSigned\t"
                |   JGE => stream "\tJumpGeqSigned\t"
                |   JLE => stream "\tJumpLeqSigned\t"
                |   JG => stream "\tJumpGrtSigned\t"
                |   JB => stream "\tJumpLessUnsigned\t"
                |   JNB => stream "\tJumpGeqUnsigned\t"
                |   JNA => stream "\tJumpLeqUnsigned\t"
                |   JA => stream "\tJumpGrtUnsigned\t"
                |   JP => stream "\tJumpParitySet"
                |   JNP => stream "\tJumpParityClear\t";
                printLabel(label, stream)
            )

        |   printICode(WordComparison{arg1, arg2, ...}, stream, printReg) =
                (stream "\tWordComparison\t"; printArg(arg1, stream, printReg); stream ","; printArg(arg2, stream, printReg))

        |   printICode(PushExceptionHandler{workReg, handleStart}, stream, printReg) =
            (
                stream "\tPushExcHandler\t";
                printLabel(handleStart, stream);
                stream " with ";
                printReg(workReg, stream)
            )

        |   printICode(PopExceptionHandler{resultReg=_, workReg}, stream, printReg) =
            (
                stream "\tPopExceptionHandler\t";
                stream "with ";
                printReg(workReg, stream)
            )

        |   printICode(BeginHandler{handleStart, packetReg, workReg}, stream, printReg) =
            (
                printLabel(handleStart, stream);
                stream ":\tBeginHandler\t";
                printReg(packetReg, stream);
                stream " with ";
                printReg(workReg, stream)
            )

        |   printICode(ReturnResultFromFunction{resultReg, numStackArgs}, stream, printReg) =
                (stream "\tReturnFromFunction\t"; printReg(resultReg, stream); stream("," ^ Int.toString numStackArgs))

        |   printICode(ArithmeticFunction{oper, resultReg, operand1, operand2, ...}, stream, printReg) =
            (
                case oper of
                    ADD => stream "\tAdd\t"
                |   OR => stream "\tOrBits\t"
                |   AND => stream "\tAndBits\t"
                |   SUB => stream "\tSubtract\t"
                |   XOR => stream "\tExclusiveOrBits\t"
                |   CMP => stream "\tCompare\t";
                printArg(operand1, stream, printReg);
                stream ",";
                printArg(operand2, stream, printReg);
                stream " => ";
                printReg(resultReg, stream)
            )

        |   printICode(TestTagBit{arg, ...}, stream, printReg) = (stream "\tTestTagBit\t"; printArg(arg, stream, printReg))

        |   printICode(PushValue{arg}, stream, printReg) = (stream "\tPushValue\t"; printArg(arg, stream, printReg))

        |   printICode(ResetStackPtr{numWords}, stream, _) = (stream "\tResetStackPtr\t"; stream(Int.toString numWords))

        |   printICode(TagValue{source, dest}, stream, printReg) =
                (stream "\tTagValue\t"; printReg(source, stream); stream " => "; printReg(dest, stream))

        |   printICode(UntagValue{source, dest, isSigned=true}, stream, printReg) =
                (stream "\tUntagSigned\t"; printArg(source, stream, printReg); stream " => "; printReg(dest, stream))
        |   printICode(UntagValue{source, dest, isSigned=false}, stream, printReg) =
                (stream "\tUntagSigned\t"; printArg(source, stream, printReg); stream " => "; printReg(dest, stream))

        |   printICode(LoadEffectiveAddress{base, offset, index, dest}, stream, printReg) =
            (
                stream "\tLoadEffectiveAddr\t"; 
                stream(Int.toString offset ^ "(");
                case base of NONE => stream "_" | SOME b => printReg(b, stream);
                stream ")";
                printIndex(index, stream, printReg);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(ShiftOperation{shift, resultReg, operand, shiftAmount, ...}, stream, printReg) =
            (
                case shift of
                    SHL => stream "\tShiftLeft\t"
                |   SHR => stream "\tShiftRLogical\t"
                |   SAR => stream "\tShiftRArith\t";
                printArg(operand, stream, printReg); stream ",";
                printArg(shiftAmount, stream, printReg); stream " => ";
                printReg(resultReg, stream)
            )

        |   printICode(Multiplication{resultReg, operand1, operand2, ...}, stream, printReg) =
            (
                stream "\tMultiplication\t";
                printArg(operand1, stream, printReg);
                stream ",";
                printArg(operand2, stream, printReg);
                stream " => ";
                printReg(resultReg, stream)
            )

        |   printICode(Division{isSigned, dividend, divisor, quotient, remainder}, stream, printReg) =
            (
                stream "\tDivision"; stream(if isSigned then "Signed\t" else "Unsigned\t");
                printReg(dividend, stream); stream " by ";
                printArg(divisor, stream, printReg); stream " => ";
                printReg(quotient, stream); stream " rem ";
                printReg(remainder, stream)
            )

        |   printICode(AtomicExchangeAndAdd{base, source}, stream, printReg) =
            (
                stream "\tAtomicExchangeAndAdd\t";
                stream "addr=0("; printReg(base, stream);
                stream "),with="; printReg(source, stream)
            )

        |   printICode(BoxValue{boxKind, source, dest, saveRegs}, stream, printReg) =
            (
                case boxKind of BoxLargeWord => stream "\tBoxLarge\t" | BoxFloat => stream "\tBoxFloat\t";
                printReg(source, stream);
                stream " => ";
                printReg(dest, stream);
                stream " save="; printSaves(saveRegs, printReg, stream)
            )

        |   printICode(CompareByteVectors{vec1Addr, vec2Addr, length, ...}, stream, printReg) =
            (
                stream "\tCompareByteVectors\t";
                printReg(vec1Addr, stream); stream ",";
                printReg(vec2Addr, stream); stream ",";
                printReg(length, stream)
            )

        |   printICode(BlockMove{srcAddr, destAddr, length, isByteMove}, stream, printReg) =
            (
                stream(if isByteMove then "\tBlockByteMove\t" else "\tBlockWordMove\t");
                stream "src="; printReg(srcAddr, stream);
                stream ",dest="; printReg(destAddr, stream);
                stream ",len="; printReg(length, stream)
            )

        |   printICode(CompareFloatingPt{arg1, arg2, ...}, stream, printReg) =
                (stream "\tCompareFloatingPt\t"; printArg(arg1, stream, printReg); stream ","; printArg(arg2, stream, printReg))

        |   printICode(X87FPGetCondition{dest, ...}, stream, printReg) = (stream "\tX87FPGetCondition\t => "; printReg(dest, stream))

        |   printICode(X87FPArith{opc, resultReg, arg1, arg2}, stream, printReg) =
            (
                case opc of
                    FADD => stream "\tX87FPAdd\t"
                |   FMUL => stream "\tX87FPMul"
                |   FCOM => stream "\tX87FPCompare\t"
                |   FCOMP => stream "\tX87FPComparePop\t"
                |   FSUB => stream "\tX87FPSub\t"
                |   FSUBR => stream "\tX87FPRevSub\t"
                |   FDIV => stream "\tX87FPDiv\t"
                |   FDIVR => stream "\tX87FPRevDiv\t";
                printArg(arg1, stream, printReg); stream ",";
                printArg(arg2, stream, printReg); stream " => ";
                printReg(resultReg, stream)
            )
        
        |   printICode(X87FPUnaryOps{fpOp, dest, source}, stream, printReg) =
            (
                case fpOp of
                    FABS => stream "\tX87FPAbs\t"
                |   FCHS => stream "\tX87FPNegate\t"
                |   FLD1 => stream "\tX87FPLoad1\t"
                |   FLDZ => stream "\tX87FPLoad0\t";
                printArg(source, stream, printReg); stream " => ";
                printReg(dest, stream)
            )
        
        |   printICode(FloatFixedInt{dest, source}, stream, printReg) =
                (stream "\tFloatFixedInt\t"; printArg(source, stream, printReg); stream " => "; printReg(dest, stream))
        
        |   printICode(SSE2FPArith{opc, resultReg, arg1, arg2}, stream, printReg) =
            (
                case opc of
                    SSE2Move => stream "\tSSE2FPMove\t"
                |   SSE2Comp => stream "\tSSE2FPComp\t"
                |   SSE2Add => stream "\tSSE2FPAdd\t"
                |   SSE2Sub => stream "\tSSE2FPSub\t"
                |   SSE2Mul => stream "\tSSE2FPMul\t"
                |   SSE2Div => stream "\tSSE2FPDiv\t"
                |   SSE2Xor => stream "\tSSE2FPXor\t"
                |   SSE2And => stream "\tSSE2FPAnd\t"
                |   SSE2MoveSingle => stream "\tSSE2FPMoveSingle\t"
                |   SSE2DoubleToFloat => stream "\tSSE2FPDoubleToFloat\t";
                printArg(arg1, stream, printReg); stream ",";
                printArg(arg2, stream, printReg); stream " => ";
                printReg(resultReg, stream)
            )
            
        fun printConcreteReg(reg, stream) = stream(regRepr reg)

        and printAbstractReg(PReg(i, PRegGeneral), stream) = stream("G" ^ Int.toString i)
        |   printAbstractReg(PReg(i, PRegUntagged), stream) = stream("U" ^ Int.toString i)
    in
        fun printICodeConcrete(icode, stream) = printICode(icode, stream, printConcreteReg)
        and printICodeAbstract(icode, stream) = printICode(icode, stream, printAbstractReg)
    end

    (* We frequently just want to know the register. *)
    fun indexRegister NoMemIndex = NONE
    |   indexRegister (MemIndex1 r) = SOME r
    |   indexRegister (MemIndex2 r) = SOME r
    |   indexRegister (MemIndex4 r) = SOME r
    |   indexRegister (MemIndex8 r) = SOME r
    
    exception InternalError = Misc.InternalError

    (* Generate code from the ICode.  This assumes that all pseudo-registers have been replaced by
       real registers or locations.  Only certain patterns of arguments are accepted. *)
    fun codeAsX86Code{icode, maxLabels, stackRequired, inputRegisters: reg list, functionName, debugSwitches} =
    let
        (* The profile object is a single mutable with the F_bytes bit set. *)
        local
            val v = RunCall.allocateByteMemory(0w1, Word.fromLargeWord(Word8.toLargeWord(Word8.orb(F_mutable, F_bytes))))
            fun clear 0w0 = ()
            |   clear i = (assignByte(v, i-0w1, 0w0); clear (i-0w1))
            val () = clear(Word.fromInt wordSize)
        in
            val profileObject = toMachineWord v
        end
        (* Switch to indicate if we want to trace where live data has been allocated. *)
        val addAllocatingFunction =
            DEBUG.getParameter DEBUG.profileAllocationTag debugSwitches = 1
        
        fun asGenReg(GenReg r) = r
        |   asGenReg _ = raise InternalError "asGenReg"
        
        fun argAsGenReg(RegisterArgument(GenReg r)) = r
        |   argAsGenReg _ = raise InternalError "argAsGenReg"
        
        (* Labels.  Create an array and fill in the entries. *)
        datatype labelKind = NormalLabel of label | HandlerLab of addrs ref | NoLabel
        val labelArray = Array.array(maxLabels, NoLabel)

        fun addLabels(ForwardJumpLabel{label=ILabel labno, ...}) =
            (
                case Array.sub(labelArray, labno) of NoLabel => () | _ => raise InternalError "addLabels: redefined";
                Array.update(labelArray, labno, NormalLabel(mkLabel()))
            )
        |   addLabels(StartLoop{loopLabel=ILabel labno, ...}) =
            (
                case Array.sub(labelArray, labno) of NoLabel => () | _ => raise InternalError "addLabels: redefined";
                Array.update(labelArray, labno, NormalLabel(mkLabel()))
            )
        |   addLabels(PushExceptionHandler{ handleStart=ILabel labno, ... }) =
            (
                case Array.sub(labelArray, labno) of NoLabel => () | _ => raise InternalError "addLabels: redefined";
                Array.update(labelArray, labno, HandlerLab(ref addrZero))
            )
        |   addLabels _ = ()
        
        val () = List.app addLabels icode
        
        (* Look up a normal label. *)
        fun findLabelDef(ILabel labno) =
            case Array.sub(labelArray, labno) of
                NormalLabel l => l
            |   _ => raise InternalError "findLabel: label not defined"
        (* Look up a label and increment the reference count. *)
        fun findLabelRef lab =
            case findLabelDef lab of l as Labels{uses, ...} => (uses := !uses + 1; l)

        fun memoryAddressAsBaseOffset({offset, base=(GenReg baseReg), index}) =
            {base=baseReg, offset=offset, index=memoryIndexAsIndex index}
        |   memoryAddressAsBaseOffset _ = raise InternalError "memoryAddressAsBaseOffset"

        and memoryIndexAsIndex NoMemIndex = NoIndex
        |   memoryIndexAsIndex(MemIndex1((GenReg iReg))) = Index1 iReg
        |   memoryIndexAsIndex(MemIndex2((GenReg iReg))) = Index2 iReg
        |   memoryIndexAsIndex(MemIndex4((GenReg iReg))) = Index4 iReg
        |   memoryIndexAsIndex(MemIndex8((GenReg iReg))) = Index8 iReg
        |   memoryIndexAsIndex _ = raise InternalError "memoryIndexAsIndex"

        and sourceAsGenRegOrMem(RegisterArgument((GenReg r))) = RegisterArg r
        |   sourceAsGenRegOrMem(MemoryLocation{offset, base=(GenReg baseReg), index}) =
                MemoryArg{base=baseReg, offset=offset, index=memoryIndexAsIndex index}
        |   sourceAsGenRegOrMem(StackLocation{adjustment, wordOffset}) =
                MemoryArg{base=esp, offset=(adjustment+wordOffset)*wordSize, index=NoIndex}
        |   sourceAsGenRegOrMem(IntegerConstant v) = NonAddressConstArg v
        |   sourceAsGenRegOrMem(AddressConstant v) = AddressConstArg v
        |   sourceAsGenRegOrMem _ = raise InternalError "sourceAsGenRegOrMem"

        and sourceAsXMMRegOrMem(RegisterArgument((XMMReg r))) = RegisterArg r
        |   sourceAsXMMRegOrMem(MemoryLocation{offset, base=(GenReg baseReg), index}) =
                MemoryArg{base=baseReg, offset=offset, index=memoryIndexAsIndex index}
        |   sourceAsXMMRegOrMem(StackLocation{adjustment, wordOffset}) =
                MemoryArg{base=esp, offset=(adjustment+wordOffset)*wordSize, index=NoIndex}
        |   sourceAsXMMRegOrMem(IntegerConstant v) = NonAddressConstArg v
        |   sourceAsXMMRegOrMem(AddressConstant v) = AddressConstArg v
        |   sourceAsXMMRegOrMem _ = raise InternalError "sourceAsGenRegOrMem"

        (* Turn the icode into machine code.  This produces the code in reverse. *)
        fun codeGenICode([], code) = code

        |   codeGenICode(
                (* Load to a general register or move to a general register. *)
                LoadArgument{ source, dest=GenReg destReg, kind=MoveWord} :: rest, code) =
                    codeGenICode(rest, MoveToRegister { source=sourceAsGenRegOrMem source, output=destReg } :: code)

        |   codeGenICode(
                (* Load from memory. *)
                LoadArgument{ source=MemoryLocation mLoc, dest=GenReg destReg, kind=MoveByte} :: rest, code) =
                codeGenICode(rest, LoadNonWord{size=Size8Bit, source=memoryAddressAsBaseOffset mLoc, output=destReg} :: code)

        |   codeGenICode(
                (* Load from memory. *)
                LoadArgument{ source=MemoryLocation mLoc, dest=GenReg destReg, kind=Move16Bit} :: rest, code) =
                codeGenICode(rest, LoadNonWord{size=Size16Bit, source=memoryAddressAsBaseOffset mLoc, output=destReg} :: code)

        |   codeGenICode(
                (* Load from memory. *)
                LoadArgument{ source=MemoryLocation mLoc, dest=GenReg destReg, kind=Move32Bit} :: rest, code) =
                codeGenICode(rest, LoadNonWord{size=Size32Bit, source=memoryAddressAsBaseOffset mLoc, output=destReg} :: code)

                (* Store to memory *)
        |   codeGenICode(
                StoreArgument{ source=RegisterArgument(GenReg sourceReg), base, offset, index, kind=MoveWord} :: rest, code) =
                codeGenICode(rest,
                    StoreRegToMemory{toStore=sourceReg, address=memoryAddressAsBaseOffset{base=base, offset=offset, index=index}} :: code)

        |   codeGenICode(
                StoreArgument{ source=RegisterArgument(GenReg sourceReg), base, offset, index, kind=MoveByte} :: rest, code) =
                codeGenICode(rest,
                    StoreNonWord{size=Size8Bit, toStore=sourceReg, address=memoryAddressAsBaseOffset {base=base, offset=offset, index=index}} :: code)

        |   codeGenICode(
                StoreArgument{ source=RegisterArgument(GenReg sourceReg), base, offset, index, kind=Move16Bit} :: rest, code) =
                codeGenICode(rest,
                    StoreNonWord{size=Size16Bit, toStore=sourceReg, address=memoryAddressAsBaseOffset {base=base, offset=offset, index=index}} :: code)

        |   codeGenICode(
                StoreArgument{ source=RegisterArgument(GenReg sourceReg), base, offset, index, kind=Move32Bit} :: rest, code) =
                codeGenICode(rest,
                    StoreNonWord{size=Size32Bit, toStore=sourceReg, address=memoryAddressAsBaseOffset {base=base, offset=offset, index=index}} :: code)

                (* Store a short constant to memory *)
        |   codeGenICode(
                StoreArgument{ source=IntegerConstant srcValue, base, offset, index, kind=MoveWord} :: rest, code) =
                codeGenICode(rest,
                    StoreConstToMemory{toStore=srcValue, address=memoryAddressAsBaseOffset {base=base, offset=offset, index=index}} :: code)

        |   codeGenICode(
                StoreArgument{ source=IntegerConstant srcValue, base, offset, index, kind=MoveByte} :: rest, code) =
                codeGenICode(rest,
                    StoreNonWordConst{size=Size8Bit, toStore=srcValue, address=memoryAddressAsBaseOffset {base=base, offset=offset, index=index}} :: code)

                (* Store a long constant to memory *)
        |   codeGenICode(
                StoreArgument{ source=AddressConstant srcValue, base, offset, index, kind=MoveWord} :: rest, code) =
                codeGenICode(rest,
                    StoreLongConstToMemory{toStore=srcValue, address=memoryAddressAsBaseOffset {base=base, offset=offset, index=index}} :: code)

                (* Load a floating point value. *)
        |   codeGenICode(LoadArgument{source=MemoryLocation{offset, base=(GenReg baseReg), index},
                                dest=FPReg fpReg, kind=MoveDouble} :: rest, code) =
            let
                val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: Load FPReg <> fp0"
            in
                codeGenICode(rest, FPLoadFromMemory{ address={base=baseReg, offset=offset, index=memoryIndexAsIndex index}, precision=DoublePrecision } :: code)
            end

                (* Load or move from an XMM reg. *)
        |   codeGenICode(LoadArgument{source, dest=XMMReg xmmRegReg, kind=MoveDouble} :: rest, code) =
                codeGenICode(rest, XMMArith { opc= SSE2Move, source=sourceAsXMMRegOrMem source, output=xmmRegReg } :: code)

                (* Store a floating point value. *)
        |   codeGenICode(StoreArgument{source=RegisterArgument(FPReg fpReg),
                         offset, base=(GenReg baseReg), index, kind=MoveDouble} :: rest, code) =
            let
                val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: Store FPReg <> fp0"
            in
                codeGenICode(rest, FPStoreToMemory{ address={ base=baseReg, offset=offset, index=memoryIndexAsIndex index}, precision=DoublePrecision, andPop=true } :: code)
            end

        |   codeGenICode(StoreArgument{source=RegisterArgument(XMMReg xmmRegReg),
                         offset, base=(GenReg baseReg), index, kind=MoveDouble} :: rest, code) =
                codeGenICode(rest, XMMStoreToMemory { toStore=xmmRegReg, address={base=baseReg, offset=offset, index=memoryIndexAsIndex index}, precision=DoublePrecision } :: code)

                (* Load a floating point value. *)
        |   codeGenICode(LoadArgument{source=MemoryLocation{offset, base=(GenReg baseReg), index},
                                dest=FPReg fpReg, kind=MoveFloat} :: rest, code) =
            let
                val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: Load FPReg <> fp0"
            in
                codeGenICode(rest, FPLoadFromMemory{ address={ base=baseReg, offset=offset, index=memoryIndexAsIndex index }, precision=SinglePrecision } :: code)
            end

                (* Load or move from an XMM reg. *)
        |   codeGenICode(LoadArgument{source, dest=XMMReg xmmRegReg, kind=MoveFloat} :: rest, code) =
                codeGenICode(rest, XMMArith { opc= SSE2MoveSingle, source=sourceAsXMMRegOrMem source, output=xmmRegReg } :: code)

                (* Store a floating point value. *)
        |   codeGenICode(StoreArgument{source=RegisterArgument(FPReg fpReg),
                         offset, base=(GenReg baseReg), index, kind=MoveFloat} :: rest, code) =
            let
                val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: Store FPReg <> fp0"
            in
                codeGenICode(rest, FPStoreToMemory{address={ base=baseReg, offset=offset, index=memoryIndexAsIndex index}, precision=SinglePrecision, andPop=true } :: code)
            end

        |   codeGenICode(StoreArgument{source=RegisterArgument(XMMReg xmmRegReg),
                         offset, base=(GenReg baseReg), index, kind=MoveFloat} :: rest, code) =
                codeGenICode(rest, XMMStoreToMemory { toStore=xmmRegReg, address={base=baseReg, offset=offset, index=memoryIndexAsIndex index}, precision=SinglePrecision } :: code)

            (* Any other combinations are not allowed. *)
        |   codeGenICode(LoadArgument _ :: _, _) = raise InternalError "codeGenICode: LoadArgument"

        |   codeGenICode(StoreArgument _ :: _, _) = raise InternalError "codeGenICode: StoreArgument"

            (* This should have been transformed into MoveArgument. *)
        |   codeGenICode(LoadMemReg _ :: _, _) = raise InternalError "codeGenICode - LoadMemReg"

            (* Exchange two general registers. *)
        |   codeGenICode(ExchangeRegisters{ regX, regY} :: rest, code) =
                codeGenICode(rest, XChngRegisters { regX=asGenReg regX, regY=asGenReg regY } :: code)

        |   codeGenICode(BeginFunction _ :: rest, code) = codeGenICode(rest, code) (* Don't need to do anything. *)

        |   codeGenICode(FunctionCall {callKind, ...} :: rest, code) =
                codeGenICode(rest, CallFunction callKind :: code)

        |   codeGenICode(TailRecursiveCall {callKind, ...} :: rest, code) =
                codeGenICode(rest, JumpToFunction callKind :: code)

        |   codeGenICode(AllocateMemoryOperation { size, flags, dest, saveRegs} :: rest, code) =
            let
                val toReg = asGenReg dest
                val preserve = map asGenReg saveRegs

                (* Allocate memory.  N.B. Instructions are in reverse order. *)
                fun allocStore{size, flags, output, preserve} =
                if isX64 andalso flags <> 0w0
                then
                    [StoreNonWordConst{size=Size8Bit, toStore=Word8.toLargeInt flags, address={offset= ~1, base=output, index=NoIndex}},
                     StoreConstToMemory{toStore=LargeInt.fromInt size, address={offset= ~wordSize, base=output, index=NoIndex}},
                     AllocStore{size=size, output=output, saveRegs=preserve}]
                else
                let
                    val lengthWord = IntInf.orb(IntInf.fromInt size, IntInf.<<(Word8.toLargeInt flags, 0w24))
                in
                    [StoreConstToMemory{toStore=lengthWord, address={offset= ~wordSize, base=output, index=NoIndex}},
                     AllocStore{size=size, output=output, saveRegs=preserve}]
                end

                val allocCode =
                    (* If we need to add the profile object *)
                    if addAllocatingFunction
                    then
                        allocStore {size=size+1, flags=Word8.orb(flags, Address.F_profile), output=toReg, preserve=preserve} @
                            [StoreLongConstToMemory{ toStore=profileObject, address={base=toReg, offset=size*wordSize, index=NoIndex}}]
                    else allocStore {size=size, flags=flags, output=toReg, preserve=preserve}
            in
                codeGenICode(rest, allocCode @ code)
            end

        |   codeGenICode(AllocateMemoryVariable{ size, dest, saveRegs} :: rest, code) =
            let
                val sReg = asGenReg size and dReg = asGenReg dest
                val _ = sReg <> dReg
                            orelse raise InternalError "codeGenICode-AllocateMemoryVariable"
                val preserve = map asGenReg saveRegs

                val allocCode =
                [
                    (* Store it as the length field. *)
                    StoreRegToMemory{toStore=sReg,
                        address={base=dReg, offset= ~wordSize, index=NoIndex}},
                    (* Untag the length *)
                    ShiftConstant{ shiftType=SHR, output=sReg, shift=0w1},
                    (* Allocate the memory *)
                    AllocStoreVariable{ output=dReg, saveRegs=preserve},
                    (* Compute the number of bytes into dReg. The length in sReg is the number
                       of words as a tagged value so we need to multiply it, add wordSize to
                       include one word for the header then subtract the, multiplied, tag. *)
                    if wordSize = 4
                    then LoadAddress{output=dReg, base=NONE, offset=wordSize-2, index=Index2 sReg }
                    else LoadAddress{output=dReg, base=NONE, offset=wordSize-4, index=Index4 sReg }
                ]
            in
                codeGenICode(rest, allocCode @ code)
            end

        |   codeGenICode(InitialiseMem{size, addr, init} :: rest, code) =
            let
                val sReg = asGenReg size and iReg = asGenReg init and aReg = asGenReg addr
                (* Initialise the memory.  This requires that sReg = ecx, iReg = eax and aReg = edi. *)
                val _ = sReg = ecx orelse raise InternalError "codeGenICode: InitialiseMem"
                val _ = iReg = eax orelse raise InternalError "codeGenICode: InitialiseMem"
                val _ = aReg = edi orelse raise InternalError "codeGenICode: InitialiseMem"
            in
                codeGenICode(rest, RepeatOperation STOSL :: code)
            end

        |   codeGenICode(InitialisationComplete :: rest, code) =
                codeGenICode(rest, StoreInitialised :: code)

        |   codeGenICode(StartLoop {loopLabel, ...} :: rest, code) = (* Same as ForwardLabel. *)
                codeGenICode(rest, JumpLabel(findLabelDef loopLabel) :: code)

        |   codeGenICode(EndLoop _ :: rest, code) = (* Nothing to do here. *)
                codeGenICode(rest, code)

        |   codeGenICode(JumpLoop {loopLabel, ...} :: rest, code) =
                codeGenICode(rest, UncondBranch (findLabelRef loopLabel) :: code)
 
        |   codeGenICode(RaiseExceptionPacket _ :: rest, code) =
                codeGenICode(rest, RaiseException :: code)

        |   codeGenICode(IndexedCaseOperation { testReg, workReg, cases, startValue} :: rest, code) =
            let
                val rReg = asGenReg testReg and wReg = asGenReg workReg
                val _ = rReg <> wReg orelse raise InternalError "IndexedCaseOperation - same registers"
                val caseLabels = map findLabelRef cases
            in
                codeGenICode(rest, IndexedCase{testReg=rReg, workReg=wReg, min=startValue, cases=caseLabels} :: code)
            end

        |   codeGenICode(LockMutable { addr } :: rest, code) =
                codeGenICode(rest, LockMutableSegment (asGenReg addr) :: code)

        |   codeGenICode(ForwardJumpLabel { label, ... } :: rest, code) =
                codeGenICode(rest, JumpLabel(findLabelDef label) :: code)

        |   codeGenICode(UnconditionalForwardJump {label} :: rest, code) =
                codeGenICode(rest, UncondBranch(findLabelRef label) :: code)

        |   codeGenICode(ConditionalForwardJump {condition, label, ...} :: rest, code) =
                codeGenICode(rest, ConditionalBranch{test=condition, predict=PredictNeutral, label=findLabelRef label} :: code)

        |   codeGenICode(WordComparison {arg1=RegisterArgument(GenReg r), arg2, ...} :: rest, code) =
                codeGenICode(rest, ArithToGenReg {opc=CMP, output=r, source=sourceAsGenRegOrMem arg2} :: code)

        |   codeGenICode(WordComparison _ :: _, _) =
                raise InternalError "codeGenICode: TODO WordComparison"

        |   codeGenICode(PushExceptionHandler { workReg, handleStart=ILabel hStart, ... } :: rest, code) =
            let (* Set up an exception handler. *)
                (* Although we're pushing this to the stack we need to use LEA on the
                   X86/64 and some arithmetic on the X86/32.  We need a work reg for that. *)
                val handleReg = asGenReg workReg
                val labelRef =
                    case Array.sub(labelArray, hStart) of
                        HandlerLab addr => addr
                    |   _ => raise InternalError "codeGenICode: PushExceptionHandler not handler"
                (* Set up the handler by pushing the old handler to the stack, pushing the
                   entry point and setting the handler address to the current stack pointer. *)
            in
                codeGenICode(rest,
                    StoreRegToMemory{
                        toStore=esp, address={offset=memRegHandlerRegister, base=ebp, index=NoIndex}} ::
                    PushToStack(RegisterArg handleReg) ::
                    LoadHandlerAddress{ handlerLab=labelRef, output=handleReg} ::
                    PushToStack(MemoryArg{base=ebp, offset=memRegHandlerRegister, index=NoIndex}) :: code)
            end

        |   codeGenICode(PopExceptionHandler { workReg, ... } :: rest, code) =
            let (* Remove an exception handler if no exception was raised. *)
                val wReg = asGenReg workReg
            in
                (* The stack pointer has been adjusted to just above the two words that were stored
                   in PushExceptionHandler. *)
                codeGenICode(rest,
                    StoreRegToMemory{
                        toStore=wReg, address={offset=memRegHandlerRegister, base=ebp, index=NoIndex}} ::
                    PopR wReg ::
                    ResetStack 1 :: code)
            end
 
        |   codeGenICode(BeginHandler {handleStart=ILabel hStart, workReg, packetReg} :: rest, code) =
            let
                val _ = asGenReg packetReg = eax orelse raise InternalError "codeGenICode: BeginHandler"
                val wReg = asGenReg workReg
                val labelRef =
                    case Array.sub(labelArray, hStart) of
                        HandlerLab addr => addr
                    |   _ => raise InternalError "codeGenICode: BeginHandler not handler"
            in
                (* The code here is almost the same as PopExceptionHandler.  The only real difference
                   is that PopExceptionHandler needs to pass the result of executing the handled code
                   which could be in any register.  This code needs to transmit the exception packet
                   and that is always in rax. *)
                codeGenICode(rest,
                    StoreRegToMemory{
                        toStore=wReg, address={offset=memRegHandlerRegister, base=ebp, index=NoIndex}} ::
                    PopR wReg :: ResetStack 1 ::
                    MoveToRegister{ source=MemoryArg{base=ebp, offset=memRegHandlerRegister, index=NoIndex}, output=esp } ::
                    StartHandler{handlerLab=labelRef} :: code)
            end

        |   codeGenICode(ReturnResultFromFunction { numStackArgs, ... } :: rest, code) =
                codeGenICode(rest, ReturnFromFunction numStackArgs :: code)

        |   codeGenICode(
                ArithmeticFunction{
                    oper, resultReg=(GenReg resReg), operand1=RegisterArgument(GenReg op1Reg),
                    operand2, ...} :: rest, code) =
            let
                val _ = resReg = op1Reg orelse raise InternalError "codeGenICode: ArithmeticFunction"
            in
                codeGenICode(rest, ArithToGenReg { opc=oper, output=resReg, source=sourceAsGenRegOrMem operand2 } :: code)
            end

        |   codeGenICode(ArithmeticFunction _ :: _, _) =
                raise InternalError "codeGenICode: TODO codeGenICode - ArithmeticFunction"

        |   codeGenICode(TestTagBit {arg = RegisterArgument reg, ...} :: rest, code) =
                codeGenICode(rest, TestTagR(asGenReg reg) :: code)

        |   codeGenICode(TestTagBit {arg = StackLocation {adjustment, wordOffset}, ...} :: rest, code) =
                (* Since the X86 is little-endian we check the addressed byte. *)
                codeGenICode(rest, TestByteMem{base=esp, offset=(adjustment+wordOffset)*wordSize, bits=0w1} :: code)

        |   codeGenICode(TestTagBit {arg = MemoryLocation {offset, base, index=NoMemIndex}, ...} :: rest, code) =
                codeGenICode(rest, TestByteMem{base=asGenReg base, offset=offset, bits=0w1} :: code)

        |   codeGenICode(TestTagBit _ :: _, _) = raise InternalError "codeGenICode: TestTagBit"

        |   codeGenICode(PushValue { arg = RegisterArgument reg } :: rest, code) =
                codeGenICode(rest, PushToStack(RegisterArg(asGenReg reg)) :: code)

        |   codeGenICode(PushValue { arg = IntegerConstant v } :: rest, code) =
                codeGenICode(rest, PushToStack(NonAddressConstArg v) :: code)

        |   codeGenICode(PushValue { arg = AddressConstant v } :: rest, code) =
                codeGenICode(rest, PushToStack(AddressConstArg v) :: code)

        |   codeGenICode(PushValue { arg = StackLocation {adjustment, wordOffset} } :: rest, code) =
                (* The X86 manual says that the address is computed before the push. *)
                codeGenICode(rest, PushToStack(MemoryArg{base=esp, offset=(adjustment+wordOffset)*wordSize, index=NoIndex}) :: code)

        |   codeGenICode(PushValue { arg = MemoryLocation {offset, base, index} } :: rest, code) =
                codeGenICode(rest, PushToStack(MemoryArg{base=asGenReg base, offset=offset, index=memoryIndexAsIndex index}) :: code)

        |   codeGenICode(ResetStackPtr {numWords} :: rest, code) =
            (
                numWords >= 0 orelse raise InternalError "codeGenICode: ResetStackPtr - negative offset";
                codeGenICode(rest, ResetStack numWords :: code)
            )

        |   codeGenICode(TagValue _ :: _, _) =
                raise InternalError "codeGenICode: TODO TagValue"

        |   codeGenICode(UntagValue _ :: _, _) =
                raise InternalError "codeGenICode: TODO UntagValue"

        |   codeGenICode(LoadEffectiveAddress { base, offset, index, dest } :: rest, code) =
            let
                val bReg = Option.map asGenReg base
                val indexR = memoryIndexAsIndex index
            in
                codeGenICode(rest, LoadAddress{ output=asGenReg dest, offset=offset, base=bReg, index=indexR } :: code)
            end

        |   codeGenICode(
                ShiftOperation{ shift, resultReg, operand, shiftAmount=IntegerConstant shiftValue, ...} :: rest, code) =
            let
                val resReg = asGenReg resultReg and opReg = argAsGenReg operand
                val _ = resReg = opReg orelse raise InternalError "codeGenICode: ShiftOperation"
            in
                codeGenICode(rest, ShiftConstant{ shiftType=shift, output=resReg, shift=Word8.fromLargeInt shiftValue } :: code)
            end

        |   codeGenICode(ShiftOperation { shift, resultReg, operand, shiftAmount, ...} :: rest, code) =
            let
                val resReg = asGenReg resultReg and opReg = argAsGenReg operand
                val _ = resReg = opReg orelse raise InternalError "codeGenICode: ShiftOperation"
                (* The amount to shift must be in ecx.  The shift is masked to 5 or 6 bits so we have to
                   check for larger shift values at a higher level. *)
                val _ = argAsGenReg shiftAmount = ecx orelse raise InternalError "codeGenICode: ShiftOperation"
            in
                codeGenICode(rest, ShiftVariable{ shiftType=shift, output=resReg } :: code)
            end

        |   codeGenICode(Multiplication { resultReg, operand1, operand2=MemoryLocation{offset, base, index=NoMemIndex}, ... } :: rest, code) =
            let
                val resReg = asGenReg resultReg and op1Reg = argAsGenReg operand1 and baseReg = asGenReg base
                val _ = resReg = op1Reg orelse raise InternalError "codeGenICode: Multiplication"
            in
                codeGenICode(rest, MultiplyRM { base=baseReg, offset=offset, output=resReg } :: code)
            end

        |   codeGenICode(Multiplication { resultReg, operand1, operand2, ... } :: rest, code) =
            let
                val resReg = asGenReg resultReg and op1Reg = argAsGenReg operand1 and op2Reg = argAsGenReg operand2
                val _ = resReg = op1Reg orelse raise InternalError "codeGenICode: Multiplication"
            in
                codeGenICode(rest, MultiplyRR { source=op2Reg, output=resReg } :: code)
            end

        |   codeGenICode(Division { isSigned, dividend, divisor, quotient, remainder } :: rest, code) =
            let
                val dividendReg = asGenReg dividend and divisorReg = argAsGenReg divisor
                and quotientReg = asGenReg quotient and remainderReg = asGenReg remainder
                val _ = dividendReg = eax orelse raise InternalError "codeGenICode: Division"
                val _ = divisorReg <> eax andalso divisorReg <> edx orelse raise InternalError "codeGenICode: Division"
                val _ = quotientReg = eax orelse raise InternalError "codeGenICode: Division"
                val _ = remainderReg = edx orelse raise InternalError "codeGenICode: Division"
                (* rdx needs to be set to the high order part of the dividend.  For signed
                   division that means sign-extending rdx, for unsigned division we clear it. *)
                val setRDX =
                    if isSigned then SignExtendForDivide
                    else ArithToGenReg{ opc=XOR, output=edx, source=RegisterArg edx }
            in
                codeGenICode(rest, DivideAccR {arg=divisorReg, isSigned=isSigned} :: setRDX :: code)
            end

        |   codeGenICode(AtomicExchangeAndAdd{ base, source } :: rest, code) =
            let
                val baseReg = asGenReg base and outReg = asGenReg source
            in
                codeGenICode(rest, AtomicXAdd{base=baseReg, output=outReg} :: code)
            end

        |   codeGenICode(CompareByteVectors { vec1Addr, vec2Addr, length, ... } :: rest, code) =
            let
                (* The arguments must be in specific registers. *)
                val _ = asGenReg vec1Addr = esi orelse raise InternalError "CompareByteVectors: esi"
                val _ = asGenReg vec2Addr = edi orelse raise InternalError "CompareByteVectors: edi"
                val _ = asGenReg length = ecx orelse raise InternalError "CompareByteVectors: ecx"
            in
                codeGenICode(rest, RepeatOperation CMPSB :: code)
            end

        |   codeGenICode(BlockMove { srcAddr, destAddr, length, isByteMove } :: rest, code) =
            let
                (* The arguments must be in specific registers. *)
                val _ = asGenReg srcAddr = esi orelse raise InternalError "BlockMove: esi"
                val _ = asGenReg destAddr = edi orelse raise InternalError "BlockMove: edi"
                val _ = asGenReg length = ecx orelse raise InternalError "BlockMove: ecx"
            in
                codeGenICode(rest, RepeatOperation(if isByteMove then MOVSB else MOVSL) :: code)
            end

        |   codeGenICode(
                CompareFloatingPt {
                    arg1=RegisterArgument(FPReg fpReg),
                    arg2=MemoryLocation{offset, base=(GenReg baseReg), index=NoMemIndex}, ... } :: rest, code) =
            let
                val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: CompareFloatingPt not fp0"
                (* This currently pops the value. *)
                (*val _ = fpMode = FPModeX87 orelse raise InternalError "codeGenICode: FCOMP in SSE2 mode"*)
            in
                codeGenICode(rest, FPArithMemory{opc=FCOMP, base=baseReg, offset=offset} :: code)
            end

        |   codeGenICode(CompareFloatingPt {arg1=RegisterArgument(XMMReg xmmReg), arg2, ... } :: rest, code) =
                codeGenICode(rest, XMMArith { opc= SSE2Comp, output=xmmReg, source=sourceAsXMMRegOrMem arg2} :: code)

        |   codeGenICode(CompareFloatingPt _ :: _, _) =
                raise InternalError "codeGenICode: CompareFloatingPt: TODO"

        |   codeGenICode(X87FPGetCondition { dest, ... } :: rest, code) =
            let
                val _ = asGenReg dest = eax orelse raise InternalError "codeGenICode: GetFloatingPtCondition not eax"
                (* This currently pops the value. *)
                (*val _ = fpMode = FPModeX87 orelse raise InternalError "codeGenICode: FPStatusToEAX in SSE2 mode"*)
            in
                codeGenICode(rest, FPStatusToEAX :: code)
            end

        |   codeGenICode(
                X87FPArith {
                    opc, resultReg=(FPReg fpResReg), arg1=RegisterArgument(FPReg fpArgReg),
                    arg2=MemoryLocation{offset, base=(GenReg baseReg), index=NoMemIndex} } :: rest, code) =
            let
                val _ = fpResReg = fp0 orelse raise InternalError "codeGenICode: FloatingPointArith not fp0"
                val _ = fpArgReg = fp0 orelse raise InternalError "codeGenICode: FloatingPointArith not fp0"
            in
                codeGenICode(rest, FPArithMemory{opc=opc, base=baseReg, offset=offset} :: code)
            end

        |   codeGenICode(X87FPArith _ :: _, _) =
                raise InternalError "codeGenICode: X87FPArith: TODO"

        |   codeGenICode(
                SSE2FPArith {
                    opc, resultReg=(XMMReg xmmResReg), arg1=RegisterArgument(XMMReg xmmArgReg), arg2 } :: rest, code) =
            let
                val _ = xmmResReg = xmmArgReg orelse raise InternalError "codeGenICode: FloatingPointArith - different regs"
                (* xorpd and andpd require 128-bit arguments with 128-bit alignment. *)
                val _ =
                    case (opc, arg2) of
                        (SSE2Xor, RegisterArgument _) => ()
                    |   (SSE2Xor, _) => raise InternalError "codeGenICode - SSE2Xor not in register"
                    |   (SSE2And, RegisterArgument _) => ()
                    |   (SSE2And, _) => raise InternalError "codeGenICode - SSE2And not in register"
                    |   _ => ()
            in
                codeGenICode(rest, XMMArith{ opc=opc, output=xmmResReg, source=sourceAsXMMRegOrMem arg2} :: code)
            end

        |   codeGenICode(SSE2FPArith _ :: _, _) =
                raise InternalError "codeGenICode: SSE2FPArith: TODO"

        |   codeGenICode(X87FPUnaryOps {fpOp, dest=(FPReg fpResReg), source=RegisterArgument(FPReg fpArgReg)} :: rest, code) =
            let
                val _ = fpResReg = fp0 orelse raise InternalError "codeGenICode: X87FPUnaryOps not fp0"
                val _ = fpArgReg = fp0 orelse raise InternalError "codeGenICode: X87FPUnaryOps not fp0"
            in
                codeGenICode(rest, FPUnary fpOp :: code)
            end

        |   codeGenICode(X87FPUnaryOps _ :: _, _) =
                raise InternalError "codeGenICode: FloatingPointNeg: TODO"

        |   codeGenICode(FloatFixedInt { dest=(XMMReg xmmResReg), source=RegisterArgument(GenReg srcReg) } :: rest, code) =
                codeGenICode(rest, XMMConvertFromInt{ output=xmmResReg, source=srcReg} :: code)

        |   codeGenICode(FloatFixedInt { dest=(FPReg fpReg), source=MemoryLocation{base, offset, index=NoMemIndex} } :: rest, code) =
            let
                val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: FloatFixedInt not fp0"
            in
                codeGenICode(rest, FPLoadInt{ base=asGenReg base, offset=offset} :: code)
            end

        |   codeGenICode(FloatFixedInt _ :: _, _) =
                raise InternalError "codeGenICode: FloatFixedInt: TODO"

            (* ReserveContainer should have been removed by earlier passes. *)
        |   codeGenICode(ReserveContainer _ :: _, _) =
                raise InternalError "codeGenICode: ReserveContainer"
                
            (* BoxValue should have been removed by earlier passes. *)
        |   codeGenICode(BoxValue _ :: _, _) =
                raise InternalError "codeGenICode: BoxValue"

        (* The stack limit register is set at least twice this far from the
           end of the stack so we can simply compare the stack pointer with
           the stack limit register if we need less than this much. Setting
           it at twice this value means that functions that use up to this
           much stack and do not call any other functions do not need to
           check the stack at all. *)
        (* TODO: The only functions that don't check the stack are RTS functions
           and any hand-coded functions. *)
        val minStackCheck = 20
        
        (* Adds the constants onto the code, and copies the code into a new segment *)
        (* Prelude consists of stack checking code. *)
        fun testRegAndTrap(reg, entryPt) =
        let
            (* If we need to take the trap we save the argument and closure registers
               across the trap. *)
            val saveRegs = List.mapPartial(fn GenReg r => SOME r | _ => NONE) inputRegisters
            fun pushThenPop [] = [CallRTS entryPt]
            |   pushThenPop (aReg::regs) = PushToStack(RegisterArg aReg) :: (pushThenPop regs @ [PopR aReg])
            (* Normally we won't have a stack overflow so we will skip the check. *)
            fun condBranch(test, predict) =
            let
                val label as Labels{uses, ...} = mkLabel()
            in
                uses := 1;
                ([ConditionalBranch{test=test, predict=predict, label=label}], label)
            end
            val (skipCheck, skipCheckLab) = condBranch(JNB, PredictTaken)
        in
            [ArithToGenReg{ opc=CMP, output=reg, source=MemoryArg{offset=memRegStackLimit, base=ebp, index=NoIndex} }] @
            skipCheck @ pushThenPop saveRegs @ [JumpLabel skipCheckLab]
        end
        val preludeCode =
            if stackRequired >= minStackCheck
            then
            let
                (* Compute the necessary amount in edi and compare that. *)
                val stackByteAdjust = ~wordSize * stackRequired
                val testEdiCode =
                    testRegAndTrap (edi, memRegStackOverflowCallEx)
            in
                 [LoadAddress{output=edi, base=SOME esp, index=NoIndex, offset=stackByteAdjust}] @ testEdiCode
            end
     
            else testRegAndTrap (esp, memRegStackOverflowCall)
        val newCode = codeCreate (functionName, profileObject, debugSwitches) 

        val ops = codeGenICode(icode, [])
    in
        createCodeSegment(X86OPTIMISE.optimise(newCode, preludeCode @ List.rev ops), newCode)
    end
    
    datatype destinations = RegDest of reg | StackDest of int

    structure Sharing =
    struct
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
