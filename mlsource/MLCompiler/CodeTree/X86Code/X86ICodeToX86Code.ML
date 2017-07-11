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

    datatype argument =
        PReg of int * pregKind (* A pseudo-register - an abstract register. *)
    |   RealRegister of reg (* A real register. *)
    |   AddressConstant of machineWord (* A constant that is an address. *)
    |   IntegerConstant of LargeInt.int (* A non-address constant.  Will usually be shifted and tagged. *)
    |   MemoryLocation of { base: argument, offset: int, index: memoryIndex } (* A memory location.  Could be the stack. *)

    and memoryIndex =
        NoMemIndex | MemIndex1 of argument | MemIndex2 of argument | MemIndex4 of argument | MemIndex8 of argument

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

    datatype x86ICode =
        (* Load, store and move, depending on the kinds of arguments.  Only certain combinations
           are supported. *)
        MoveArgument of { source: argument, dest: argument, kind: moveKind }
    
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
    |   BoxValue of { boxKind: boxKind, source: argument, dest: argument }

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
    |   X87FPGetCondition of { ccRef: ccRef, dest: argument }

        (* Binary floating point operations on the X87. *)
    |   X87FPArith of
            { opc: fpOps, resultReg: argument, arg1: argument, arg2: argument }

        (* Floating point operations: negate and set sign positive. *)
    |   X87FPUnaryOps of { fpOp: fpUnaryOps, dest: argument, source: argument }

        (* Load a fixed point value as a floating point value. *)
    |   FloatFixedInt of { dest: argument, source: argument }

        (* Binary floating point operations using SSE2 instructions. *)
    |   SSE2FPArith of { opc: sse2Operations, resultReg: argument, arg1: argument, arg2: argument }
    
    
    local
        fun printArg(PReg(i, PRegGeneral), stream) = stream("G" ^ Int.toString i)
        |   printArg(PReg(i, PRegUntagged), stream) = stream("U" ^ Int.toString i)
        |   printArg(RealRegister r, stream) = stream(regRepr r)
        |   printArg(AddressConstant m, stream) = stream(stringOfWord m)
        |   printArg(IntegerConstant i, stream) = stream(LargeInt.toString i)
        |   printArg(MemoryLocation{base, offset, index}, stream) =
            (
                stream(Int.toString offset ^ "(");
                printArg(base, stream);
                stream ")";
                case index of
                    NoMemIndex => ()
                |   MemIndex1 i => (stream "["; printArg(i, stream); stream "*1]")
                |   MemIndex2 i => (stream "["; printArg(i, stream); stream "*2]")
                |   MemIndex4 i => (stream "["; printArg(i, stream); stream "*4]")
                |   MemIndex8 i => (stream "["; printArg(i, stream); stream "*8]")
            )
            
        fun printLabel(ILabel i, stream) = stream("L"^Int.toString i)
    in
        fun printICode(MoveArgument{source, dest, kind}, stream) =
            (
                case kind of
                    MoveWord => stream "\tMoveWord\t"
                |   MoveByte => stream "\tMoveByte\t"
                |   Move16Bit => stream "\tMove16Bit\t"
                |   Move32Bit => stream "\tMove32Bit\t"
                |   MoveFloat => stream "\tMoveFloat\t"
                |   MoveDouble => stream "\tMoveDouble\t";
                printArg(source, stream);
                stream " => ";
                printArg(dest, stream)
            )

        |   printICode(FunctionCall{callKind, regArgs, stackArgs, dest, isTail}, stream) =
            (
                if isTail then stream "\tTailCall\t"
                else stream "\tFunctionCall\t";
                case callKind of
                    Recursive => stream "recursive "
                |   ConstantCode m => (stream(stringOfWord m); stream " ")
                |   FullCall => ()
                |   DirectReg r => (stream(regRepr(GenReg r)); stream " ");
                List.app(fn (arg, r) => (stream(regRepr(GenReg r)); stream "="; printArg(arg, stream); stream " ")) regArgs;
                List.app(fn arg => (stream "p="; printArg(arg, stream); stream " ")) stackArgs;
                stream "=> "; printArg(dest, stream)
            )

        |   printICode(AllocateMemoryOperation{size, flags, dest}, stream) =
            (
                stream "\tAllocateMemory\t";
                stream(concat["s=", Int.toString size, ",f=", Word8.toString flags, " => "]);
                printArg(dest, stream)
            )

        |   printICode(AllocateMemoryVariable{size, flags, dest, initialiser}, stream) =
            (
                stream "\tAllocateMemory\t";
                stream "s="; printArg(size, stream);
                stream "f="; printArg(flags, stream);
                case initialiser of NONE => () | SOME init => (stream "i="; printArg(init, stream));
                stream " => "; printArg(dest, stream)
            )

        |   printICode(InitialisationComplete{dest}, stream) = (stream "\tInitComplete\t"; printArg(dest, stream))

        |   printICode(StartLoop{arguments, loopLabel}, stream) =
            (
                stream "\tStartLoop\t";
                List.app(
                    fn {source, loopReg} => (printArg(loopReg, stream); stream "="; printArg(source, stream); stream " ")
                    ) arguments;
                printLabel(loopLabel, stream)
            )

        |   printICode(EndLoop{loopLabel}, stream) = (stream "\tEndLoop\t"; printLabel(loopLabel, stream))

        |   printICode(JumpLoop{arguments, loopLabel}, stream) =
            (
                stream "\tJumpLoop\t";
                List.app(
                    fn {source, loopReg} => (printArg(loopReg, stream); stream "="; printArg(source, stream); stream " ")
                    ) arguments;
                printLabel(loopLabel, stream)
            )

        |   printICode(RaiseExceptionPacket{packet}, stream) = (stream "\tRaise\t"; printArg(packet, stream))

        |   printICode(ReserveContainer{size, address}, stream) =
            (stream "\tReserveContainer\t"; stream(Int.toString size); stream "=>"; printArg(address, stream))

        |   printICode(IndexedCaseOperation{testReg, workReg, cases, startValue}, stream) =
            (
                stream "\tIndexedCase\t";
                stream "test="; printArg(testReg, stream);
                stream "work="; printArg(workReg, stream);
                stream "from="; stream(Word.toString startValue);
                List.app(fn l => (stream " "; printLabel(l, stream))) cases
            )

        |   printICode(LockMutable{addr}, stream) = (stream "\tLockMutable\t"; printArg(addr, stream))
        
        |   printICode(ForwardJumpLabel{label, ...}, stream) = (printLabel(label, stream); stream ":\t\t")

        |   printICode(UnconditionalForwardJump{label}, stream) = (stream "\tJump\t"; printLabel(label, stream))

        |   printICode(ConditionalForwardJump{condition, label, ...}, stream) =
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

        |   printICode(WordComparison{arg1, arg2, ...}, stream) =
                (stream "\tWordComparison\t"; printArg(arg1, stream); stream ","; printArg(arg2, stream))

        |   printICode(PushExceptionHandler{...}, stream) = stream "\tPushExceptionHandler\t"
        |   printICode(PopExceptionHandler{...}, stream) = stream "\tPopExceptionHandler\t"
        |   printICode(BeginHandler{...}, stream) = stream "\tBeginHandler\t"

        |   printICode(ReturnResultFromFunction{resultReg, numStackArgs}, stream) =
                (stream "\tReturnFromFunction\t"; printArg(resultReg, stream); stream("," ^ Int.toString numStackArgs))

        |   printICode(ArithmeticFunction{oper, resultReg, operand1, operand2, ...}, stream) =
            (
                case oper of
                    ADD => stream "\tAdd\t"
                |   OR => stream "\tOrBits\t"
                |   AND => stream "\tAndBits\t"
                |   SUB => stream "\tSubtract\t"
                |   XOR => stream "\tExclusiveOrBits\t"
                |   CMP => stream "\tCompare\t";
                printArg(operand1, stream);
                stream ",";
                printArg(operand2, stream);
                stream " => ";
                printArg(resultReg, stream)
            )

        |   printICode(TestTagBit{...}, stream) = stream "\tTestTagBit\t"

        |   printICode(PushValue{arg}, stream) = (stream "\tPushValue\t"; printArg(arg, stream))

        |   printICode(ResetStackPtr{numWords}, stream) = (stream "\tResetStackPtr\t"; stream(Int.toString numWords))

        |   printICode(TagValue{source, dest}, stream) =
                (stream "\tTagValue\t"; printArg(source, stream); stream " => "; printArg(dest, stream))

        |   printICode(UntagValue{source, dest, isSigned=true}, stream) =
                (stream "\tUntagSigned\t"; printArg(source, stream); stream " => "; printArg(dest, stream))
        |   printICode(UntagValue{source, dest, isSigned=false}, stream) =
                (stream "\tUntagSigned\t"; printArg(source, stream); stream " => "; printArg(dest, stream))

        |   printICode(LoadEffectiveAddress{...}, stream) = stream "\tLoadEffectiveAddress\t"

        |   printICode(ShiftOperation{shift, resultReg, operand, shiftAmount, ...}, stream) =
            (
                case shift of
                    SHL => stream "\tShiftLeft\t"
                |   SHR => stream "\tShiftRLogical\t"
                |   SAR => stream "\tShiftRArith\t";
                printArg(operand, stream); stream ",";
                printArg(shiftAmount, stream); stream " => ";
                printArg(resultReg, stream)
            )

        |   printICode(Multiplication{resultReg, operand1, operand2, ...}, stream) =
            (
                stream "\tMultiplication\t";
                printArg(operand1, stream);
                stream ",";
                printArg(operand2, stream);
                stream " => ";
                printArg(resultReg, stream)
            )

        |   printICode(Division{isSigned, dividend, divisor, quotient, remainder}, stream) =
            (
                stream "\tDivision"; stream(if isSigned then "Signed\t" else "Unsigned\t");
                printArg(dividend, stream); stream " by ";
                printArg(divisor, stream); stream " => ";
                printArg(quotient, stream); stream " rem ";
                printArg(remainder, stream)
            )

        |   printICode(AtomicExchangeAndAdd{destAddr, source}, stream) =
            (
                stream "\tAtomicExchangeAndAdd\t";
                stream "addr="; printArg(destAddr, stream);
                stream ",with="; printArg(source, stream)
            )

        |   printICode(BoxValue{boxKind, source, dest}, stream) =
            (
                case boxKind of BoxLargeWord => stream "\tBoxLarge\t" | BoxFloat => stream "\tBoxFloat\t";
                printArg(source, stream);
                stream " => ";
                printArg(dest, stream)
            )

        |   printICode(CompareByteVectors{vec1Addr, vec2Addr, length, ...}, stream) =
            (
                stream "\tCompareByteVectors\t";
                printArg(vec1Addr, stream); stream ",";
                printArg(vec2Addr, stream); stream ",";
                printArg(length, stream)
            )

        |   printICode(BlockMove{srcAddr, destAddr, length, isByteMove}, stream) =
            (
                stream(if isByteMove then "\tBlockByteMove\t" else "\tBlockWordMove\t");
                stream "src="; printArg(srcAddr, stream);
                stream ",dest="; printArg(destAddr, stream);
                stream ",len="; printArg(length, stream)
            )

        |   printICode(CompareFloatingPt{arg1, arg2, ...}, stream) =
                (stream "\tCompareFloatingPt\t"; printArg(arg1, stream); stream ","; printArg(arg2, stream))

        |   printICode(X87FPGetCondition{dest, ...}, stream) = (stream "\tX87FPGetCondition\t => "; printArg(dest, stream))

        |   printICode(X87FPArith{opc, resultReg, arg1, arg2}, stream) =
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
                printArg(arg1, stream); stream ",";
                printArg(arg2, stream); stream " => ";
                printArg(resultReg, stream)
            )
        
        |   printICode(X87FPUnaryOps{fpOp, dest, source}, stream) =
            (
                case fpOp of
                    FABS => stream "\tX87FPAbs\t"
                |   FCHS => stream "\tX87FPNegate\t"
                |   FLD1 => stream "\tX87FPLoad1\t"
                |   FLDZ => stream "\tX87FPLoad0\t";
                printArg(source, stream); stream " => ";
                printArg(dest, stream)
            )
        
        |   printICode(FloatFixedInt{dest, source}, stream) =
                (stream "\tFloatFixedInt\t"; printArg(source, stream); stream " => "; printArg(dest, stream))
        
        |   printICode(SSE2FPArith{opc, resultReg, arg1, arg2}, stream) =
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
                printArg(arg1, stream); stream ",";
                printArg(arg2, stream); stream " => ";
                printArg(resultReg, stream)
            )
    end
    
    exception InternalError = Misc.InternalError

    (* Generate code from the ICode.  This assumes that all pseudo-registers have been replaced by
       real registers or locations.  Only certain patterns of arguments are accepted. *)
    fun codeAsX86Code{icode, maxLabels, stackRequired, inputRegisters, functionName, debugSwitches} =
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
        
        fun asGenReg(RealRegister(GenReg r)) = r
        |   asGenReg _ = raise InternalError "asGenReg"
        
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

        fun memoryAddressAsBaseOffset({offset, base=RealRegister(GenReg baseReg), index}) =
            {base=baseReg, offset=offset, index=memoryIndexAsIndex index}
        |   memoryAddressAsBaseOffset _ = raise InternalError "memoryAddressAsBaseOffset"

        and memoryIndexAsIndex NoMemIndex = NoIndex
        |   memoryIndexAsIndex(MemIndex1(RealRegister(GenReg iReg))) = Index1 iReg
        |   memoryIndexAsIndex(MemIndex2(RealRegister(GenReg iReg))) = Index2 iReg
        |   memoryIndexAsIndex(MemIndex4(RealRegister(GenReg iReg))) = Index4 iReg
        |   memoryIndexAsIndex(MemIndex8(RealRegister(GenReg iReg))) = Index8 iReg
        |   memoryIndexAsIndex _ = raise InternalError "memoryIndexAsIndex"

        and sourceAsGenRegOrMem(RealRegister(GenReg r)) = RegisterArg r
        |   sourceAsGenRegOrMem(MemoryLocation{offset, base=RealRegister(GenReg baseReg), index}) =
                MemoryArg{base=baseReg, offset=offset, index=memoryIndexAsIndex index}
        |   sourceAsGenRegOrMem(IntegerConstant v) = NonAddressConstArg v
        |   sourceAsGenRegOrMem(AddressConstant v) = AddressConstArg v
        |   sourceAsGenRegOrMem _ = raise InternalError "sourceAsGenRegOrMem"

        and sourceAsXMMRegOrMem(RealRegister(XMMReg r)) = RegisterArg r
        |   sourceAsXMMRegOrMem(MemoryLocation{offset, base=RealRegister(GenReg baseReg), index}) =
                MemoryArg{base=baseReg, offset=offset, index=memoryIndexAsIndex index}
        |   sourceAsXMMRegOrMem(IntegerConstant v) = NonAddressConstArg v
        |   sourceAsXMMRegOrMem(AddressConstant v) = AddressConstArg v
        |   sourceAsXMMRegOrMem _ = raise InternalError "sourceAsGenRegOrMem"

        (* Turn the icode into machine code.  This produces the code in reverse. *)
        fun codeGenICode([], code) = code

        |   codeGenICode(
                (* Load to a general register or move to a general register. *)
                MoveArgument{ source, dest=RealRegister(GenReg destReg), kind=MoveWord} :: rest, code) =
                    codeGenICode(rest, MoveToRegister { source=sourceAsGenRegOrMem source, output=destReg } :: code)

        |   codeGenICode(
                (* Load from memory. *)
                MoveArgument{ source=MemoryLocation mLoc, dest=RealRegister(GenReg destReg), kind=MoveByte} :: rest, code) =
                codeGenICode(rest, LoadNonWord{size=Size8Bit, source=memoryAddressAsBaseOffset mLoc, output=destReg} :: code)

        |   codeGenICode(
                (* Load from memory. *)
                MoveArgument{ source=MemoryLocation mLoc, dest=RealRegister(GenReg destReg), kind=Move16Bit} :: rest, code) =
                codeGenICode(rest, LoadNonWord{size=Size16Bit, source=memoryAddressAsBaseOffset mLoc, output=destReg} :: code)

        |   codeGenICode(
                (* Load from memory. *)
                MoveArgument{ source=MemoryLocation mLoc, dest=RealRegister(GenReg destReg), kind=Move32Bit} :: rest, code) =
                codeGenICode(rest, LoadNonWord{size=Size32Bit, source=memoryAddressAsBaseOffset mLoc, output=destReg} :: code)

                (* Store to memory *)
        |   codeGenICode(
                MoveArgument{ source=RealRegister(GenReg sourceReg), dest=MemoryLocation mLoc, kind=MoveWord} :: rest, code) =
                codeGenICode(rest,
                    StoreRegToMemory{toStore=sourceReg, address=memoryAddressAsBaseOffset mLoc} :: code)

        |   codeGenICode(
                MoveArgument{ source=RealRegister(GenReg sourceReg), dest=MemoryLocation mLoc, kind=MoveByte} :: rest, code) =
                codeGenICode(rest,
                    StoreNonWord{size=Size8Bit, toStore=sourceReg, address=memoryAddressAsBaseOffset mLoc} :: code)

        |   codeGenICode(
                MoveArgument{ source=RealRegister(GenReg sourceReg), dest=MemoryLocation mLoc, kind=Move16Bit} :: rest, code) =
                codeGenICode(rest,
                    StoreNonWord{size=Size16Bit, toStore=sourceReg, address=memoryAddressAsBaseOffset mLoc} :: code)

        |   codeGenICode(
                MoveArgument{ source=RealRegister(GenReg sourceReg), dest=MemoryLocation mLoc, kind=Move32Bit} :: rest, code) =
                codeGenICode(rest,
                    StoreNonWord{size=Size32Bit, toStore=sourceReg, address=memoryAddressAsBaseOffset mLoc} :: code)

                (* Store a short constant to memory *)
        |   codeGenICode(
                MoveArgument{ source=IntegerConstant srcValue, dest=MemoryLocation mLoc, kind=MoveWord} :: rest, code) =
                codeGenICode(rest,
                    StoreConstToMemory{toStore=srcValue, address=memoryAddressAsBaseOffset mLoc} :: code)

                (* Store a long constant to memory *)
        |   codeGenICode(
                MoveArgument{ source=AddressConstant srcValue, dest=MemoryLocation mLoc, kind=MoveWord} :: rest, code) =
                codeGenICode(rest,
                    StoreLongConstToMemory{toStore=srcValue, address=memoryAddressAsBaseOffset mLoc} :: code)

                (* Load a floating point value. *)
        |   codeGenICode(MoveArgument{source=MemoryLocation{offset, base=RealRegister(GenReg baseReg), index},
                                dest=RealRegister(FPReg fpReg), kind=MoveDouble} :: rest, code) =
            let
                val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: Load FPReg <> fp0"
            in
                codeGenICode(rest, FPLoadFromMemory{ address={base=baseReg, offset=offset, index=memoryIndexAsIndex index}, precision=DoublePrecision } :: code)
            end

                (* Load or move from an XMM reg. *)
        |   codeGenICode(MoveArgument{source, dest=RealRegister(XMMReg xmmRegReg), kind=MoveDouble} :: rest, code) =
                codeGenICode(rest, XMMArith { opc= SSE2Move, source=sourceAsXMMRegOrMem source, output=xmmRegReg } :: code)

                (* Store a floating point value. *)
        |   codeGenICode(MoveArgument{source=RealRegister(FPReg fpReg),
                         dest=MemoryLocation{offset, base=RealRegister(GenReg baseReg), index}, kind=MoveDouble} :: rest, code) =
            let
                val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: Store FPReg <> fp0"
            in
                codeGenICode(rest, FPStoreToMemory{ address={ base=baseReg, offset=offset, index=memoryIndexAsIndex index}, precision=DoublePrecision, andPop=true } :: code)
            end

        |   codeGenICode(MoveArgument{source=RealRegister(XMMReg xmmRegReg),
                         dest=MemoryLocation{offset, base=RealRegister(GenReg baseReg), index}, kind=MoveDouble} :: rest, code) =
                codeGenICode(rest, XMMStoreToMemory { toStore=xmmRegReg, address={base=baseReg, offset=offset, index=memoryIndexAsIndex index}, precision=DoublePrecision } :: code)

                (* Load a floating point value. *)
        |   codeGenICode(MoveArgument{source=MemoryLocation{offset, base=RealRegister(GenReg baseReg), index},
                                dest=RealRegister(FPReg fpReg), kind=MoveFloat} :: rest, code) =
            let
                val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: Load FPReg <> fp0"
            in
                codeGenICode(rest, FPLoadFromMemory{ address={ base=baseReg, offset=offset, index=memoryIndexAsIndex index }, precision=SinglePrecision } :: code)
            end

                (* Load or move from an XMM reg. *)
        |   codeGenICode(MoveArgument{source, dest=RealRegister(XMMReg xmmRegReg), kind=MoveFloat} :: rest, code) =
                codeGenICode(rest, XMMArith { opc= SSE2MoveSingle, source=sourceAsXMMRegOrMem source, output=xmmRegReg } :: code)

                (* Store a floating point value. *)
        |   codeGenICode(MoveArgument{source=RealRegister(FPReg fpReg),
                         dest=MemoryLocation{offset, base=RealRegister(GenReg baseReg), index}, kind=MoveFloat} :: rest, code) =
            let
                val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: Store FPReg <> fp0"
            in
                codeGenICode(rest, FPStoreToMemory{address={ base=baseReg, offset=offset, index=memoryIndexAsIndex index}, precision=SinglePrecision, andPop=true } :: code)
            end

        |   codeGenICode(MoveArgument{source=RealRegister(XMMReg xmmRegReg),
                         dest=MemoryLocation{offset, base=RealRegister(GenReg baseReg), index}, kind=MoveFloat} :: rest, code) =
                codeGenICode(rest, XMMStoreToMemory { toStore=xmmRegReg, address={base=baseReg, offset=offset, index=memoryIndexAsIndex index}, precision=SinglePrecision } :: code)

        |   codeGenICode(MoveArgument _ :: _, _) =
                raise InternalError "codeGenICode: TODO MoveArgument"

        |   codeGenICode(FunctionCall {isTail=false, callKind, ...} :: rest, code) =
                codeGenICode(rest, CallFunction callKind :: code)

        |   codeGenICode(FunctionCall {isTail=true, callKind, ...} :: rest, code) =
                codeGenICode(rest, JumpToFunction callKind :: code)

        |   codeGenICode(AllocateMemoryOperation { size, flags, dest} :: rest, code) =
            let
                val toReg = asGenReg dest

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
                        allocStore {size=size+1, flags=Word8.orb(flags, Address.F_profile), output=toReg, preserve=[]} @
                            [StoreLongConstToMemory{ toStore=profileObject, address={base=toReg, offset=size*wordSize, index=NoIndex}}]
                    else allocStore {size=size, flags=flags, output=toReg, preserve=[]}
            in
                codeGenICode(rest, allocCode @ code)
            end

        |   codeGenICode(AllocateMemoryVariable{ size, flags, dest, initialiser} :: rest, code) =
            let
                val sReg = asGenReg size and fReg = asGenReg flags and dReg = asGenReg dest
                val _ = sReg <> fReg andalso sReg <> dReg andalso fReg <> dReg
                            orelse raise InternalError "codeGenICode-AllocateMemoryVariable"

                val allocCode =
                [
                    (* Store in the top byte *)
                    StoreNonWord{size=Size8Bit, toStore=fReg, address={base=dReg, offset= ~1, index=NoIndex}},
                    (* Untag the flags. *)
                    ShiftConstant{ shiftType=SHR, output=fReg, shift=0w1},
                    (* Store it as the length field. *)
                    StoreRegToMemory{toStore=sReg,
                        address={base=dReg, offset= ~wordSize, index=NoIndex}},
                    (* Untag the length *)
                    ShiftConstant{ shiftType=SHR, output=sReg, shift=0w1},
                    (* Allocate the memory *)
                    AllocStoreVariable{ output=dReg, saveRegs=[]},
                    (* Compute the number of bytes into dReg. The length in sReg is the number
                       of words as a tagged value so we need to multiply it, add wordSize to
                       include one word for the header then subtract the, multiplied, tag. *)
                    if wordSize = 4
                    then LoadAddress{output=dReg, base=NONE, offset=wordSize-2, index=Index2 sReg }
                    else LoadAddress{output=dReg, base=NONE, offset=wordSize-4, index=Index4 sReg }
                ]
                
                (* Initialiser *)
                val initCode =
                    case initialiser of
                        NONE => [] (* Byte segments are not initialised - Should we clear last word? Length could be zero. *)
                    |   SOME init =>
                        let
                            (* Initialise the memory.  This requires that sReg = ecx, iReg = eax and edi is free. *)
                            val iReg = asGenReg init
                            val _ = sReg = ecx orelse raise InternalError "codeGenICode: AllocateMemoryVariable"
                            val _ = iReg = eax orelse raise InternalError "codeGenICode: AllocateMemoryVariable"
                        in
                            [
                                (* Initialise the memory.  This requires that sReg = ecx, iReg = eax and edi is free. *)
                                RepeatOperation STOSL,
                                (* Move the base address to edi.  It will be updated by the move. *)
                                MoveToRegister { source=RegisterArg dReg, output=edi }
                            ]
                        end
            in
                codeGenICode(rest, initCode @ allocCode @ code)
            end

        |   codeGenICode(InitialisationComplete _ :: rest, code) =
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

        |   codeGenICode(WordComparison {arg1=RealRegister(GenReg r), arg2, ...} :: rest, code) =
                codeGenICode(rest, ArithToGenReg {opc=CMP, output=r, source=sourceAsGenRegOrMem arg2} :: code)

        |   codeGenICode(WordComparison _ :: _, _) =
                raise InternalError "codeGenICode: TODO WordComparison"

        |   codeGenICode(PushExceptionHandler { handlerAddr, handleStart=ILabel hStart, ... } :: rest, code) =
            let (* Set up an exception handler. *)
                val handleReg = asGenReg handlerAddr
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
 
        |   codeGenICode(BeginHandler {handleStart=ILabel hStart, workReg} :: rest, code) =
            let
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
                    oper, resultReg=RealRegister(GenReg resReg), operand1=RealRegister(GenReg op1Reg),
                    operand2, ...} :: rest, code) =
            let
                val _ = resReg = op1Reg orelse raise InternalError "codeGenICode: ArithmeticFunction"
            in
                codeGenICode(rest, ArithToGenReg { opc=oper, output=resReg, source=sourceAsGenRegOrMem operand2 } :: code)
            end

        |   codeGenICode(ArithmeticFunction _ :: _, _) =
                raise InternalError "codeGenICode: TODO codeGenICode - ArithmeticFunction"

        |   codeGenICode(TestTagBit {arg, ...} :: rest, code) =
                codeGenICode(rest, TestTagR(asGenReg arg) :: code)

        |   codeGenICode(PushValue { arg = RealRegister(GenReg r) } :: rest, code) =
                codeGenICode(rest, PushToStack(RegisterArg r) :: code)

        |   codeGenICode(PushValue { arg = IntegerConstant v } :: rest, code) =
                codeGenICode(rest, PushToStack(NonAddressConstArg v) :: code)

        |   codeGenICode(PushValue _ :: _, _) =
                raise InternalError "codeGenICode: TODO PushValue"

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
                val resReg = asGenReg resultReg and opReg = asGenReg operand
                val _ = resReg = opReg orelse raise InternalError "codeGenICode: ShiftOperation"
            in
                codeGenICode(rest, ShiftConstant{ shiftType=shift, output=resReg, shift=Word8.fromLargeInt shiftValue } :: code)
            end

        |   codeGenICode(ShiftOperation { shift, resultReg, operand, shiftAmount, ...} :: rest, code) =
            let
                val resReg = asGenReg resultReg and opReg = asGenReg operand
                val _ = resReg = opReg orelse raise InternalError "codeGenICode: ShiftOperation"
                (* The amount to shift must be in ecx.  The shift is masked to 5 or 6 bits so we have to
                   check for larger shift values at a higher level. *)
                val _ = asGenReg shiftAmount = ecx orelse raise InternalError "codeGenICode: ShiftOperation"
            in
                codeGenICode(rest, ShiftVariable{ shiftType=shift, output=resReg } :: code)
            end

        |   codeGenICode(Multiplication { resultReg, operand1, operand2=MemoryLocation{offset, base, index=NoMemIndex}, ... } :: rest, code) =
            let
                val resReg = asGenReg resultReg and op1Reg = asGenReg operand1 and baseReg = asGenReg base
                val _ = resReg = op1Reg orelse raise InternalError "codeGenICode: Multiplication"
            in
                codeGenICode(rest, MultiplyRM { base=baseReg, offset=offset, output=resReg } :: code)
            end

        |   codeGenICode(Multiplication { resultReg, operand1, operand2, ... } :: rest, code) =
            let
                val resReg = asGenReg resultReg and op1Reg = asGenReg operand1 and op2Reg = asGenReg operand2
                val _ = resReg = op1Reg orelse raise InternalError "codeGenICode: Multiplication"
            in
                codeGenICode(rest, MultiplyRR { source=op2Reg, output=resReg } :: code)
            end

        |   codeGenICode(Division { isSigned, dividend, divisor, quotient, remainder } :: rest, code) =
            let
                val dividendReg = asGenReg dividend and divisorReg = asGenReg divisor
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

        |   codeGenICode(AtomicExchangeAndAdd{ destAddr, source } :: rest, code) =
            let
                val baseReg = asGenReg destAddr and outReg = asGenReg source
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
                    arg1=RealRegister(FPReg fpReg),
                    arg2=MemoryLocation{offset, base=RealRegister(GenReg baseReg), index=NoMemIndex}, ... } :: rest, code) =
            let
                val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: CompareFloatingPt not fp0"
                (* This currently pops the value. *)
                (*val _ = fpMode = FPModeX87 orelse raise InternalError "codeGenICode: FCOMP in SSE2 mode"*)
            in
                codeGenICode(rest, FPArithMemory{opc=FCOMP, base=baseReg, offset=offset} :: code)
            end

        |   codeGenICode(CompareFloatingPt {arg1=RealRegister(XMMReg xmmReg), arg2, ... } :: rest, code) =
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
                    opc, resultReg=RealRegister(FPReg fpResReg), arg1=RealRegister(FPReg fpArgReg),
                    arg2=MemoryLocation{offset, base=RealRegister(GenReg baseReg), index=NoMemIndex} } :: rest, code) =
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
                    opc, resultReg=RealRegister(XMMReg xmmResReg), arg1=RealRegister(XMMReg xmmArgReg), arg2 } :: rest, code) =
            let
                val _ = xmmResReg = xmmArgReg orelse raise InternalError "codeGenICode: FloatingPointArith - different regs"
            in
                codeGenICode(rest, XMMArith{ opc=opc, output=xmmResReg, source=sourceAsXMMRegOrMem arg2} :: code)
            end

        |   codeGenICode(SSE2FPArith _ :: _, _) =
                raise InternalError "codeGenICode: SSE2FPArith: TODO"

        |   codeGenICode(X87FPUnaryOps {fpOp, dest=RealRegister(FPReg fpResReg), source=RealRegister(FPReg fpArgReg)} :: rest, code) =
            let
                val _ = fpResReg = fp0 orelse raise InternalError "codeGenICode: X87FPUnaryOps not fp0"
                val _ = fpArgReg = fp0 orelse raise InternalError "codeGenICode: X87FPUnaryOps not fp0"
            in
                codeGenICode(rest, FPUnary fpOp :: code)
            end

        |   codeGenICode(X87FPUnaryOps _ :: _, _) =
                raise InternalError "codeGenICode: FloatingPointNeg: TODO"

        |   codeGenICode(FloatFixedInt { dest=RealRegister(XMMReg xmmResReg), source=RealRegister(GenReg srcReg) } :: rest, code) =
                codeGenICode(rest, XMMConvertFromInt{ output=xmmResReg, source=srcReg} :: code)

        |   codeGenICode(FloatFixedInt { dest=RealRegister(FPReg fpReg), source=MemoryLocation{base, offset, index=NoMemIndex} } :: rest, code) =
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
            val saveRegs = inputRegisters
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

    structure Sharing =
    struct
        type genReg         = genReg
        and  argument       = argument
        and  iLabel         = iLabel
        and  x86ICode       = x86ICode
        and  branchOps      = branchOps
        and  reg            = reg
    end

end;
