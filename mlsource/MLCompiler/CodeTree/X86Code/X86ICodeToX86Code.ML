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
       WordToWord copies the whole value.  This is the "normal" move.
       ByteToByte overwrites only a byte.  This is used to store a byte.
       ByteToWordZeroExtend loads a byte and zero extends it to fill the
       destination.  This is used when loading a byte. *)
    datatype moveKind = WordToWord | ByteToByte | ByteToWordZeroExtend

    datatype iLabel = ILabel of int
    (* The reference to a condition code. *)
    and ccRef = CcRef of int

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

        (* Allocate a tuple or a function closure. *)
    |   AllocateMemoryOperation of { size: int, isMutable: bool, dest: argument}

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

    
    exception InternalError = Misc.InternalError

    (* Generate code from the ICode.  This assumes that all pseudo-registers have been replaced by
       real registers or locations.  Only certain patterns of arguments are accepted. *)
    fun codeAsX86Code{icode, maxLabels, stackRequired, inputRegisters, functionName, debugSwitches} =
    let
        (* The profile object is a single mutable with the F_bytes bit set. *)
        val profileObject = toMachineWord (alloc(0w1, Word8.orb(F_mutable, F_bytes), toMachineWord 0w0))
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
            BaseOffset{base=baseReg, offset=offset, index=memoryIndexAsIndex index}
        |   memoryAddressAsBaseOffset _ = raise InternalError "memoryAddressAsBaseOffset"

        and memoryIndexAsIndex NoMemIndex = NoIndex
        |   memoryIndexAsIndex(MemIndex1(RealRegister(GenReg iReg))) = Index1 iReg
        |   memoryIndexAsIndex(MemIndex2(RealRegister(GenReg iReg))) = Index2 iReg
        |   memoryIndexAsIndex(MemIndex4(RealRegister(GenReg iReg))) = Index4 iReg
        |   memoryIndexAsIndex(MemIndex8(RealRegister(GenReg iReg))) = Index8 iReg
        |   memoryIndexAsIndex _ = raise InternalError "memoryIndexAsIndex"

        (* Turn the icode into machine code.  This produces the code in reverse. *)
        fun codeGenICode([], code) = code

        |   codeGenICode(
                (* Register-register move. *)
                MoveArgument{ source=RealRegister(GenReg sourceReg), dest=RealRegister(GenReg destReg), kind=WordToWord} :: rest, code) =
                    codeGenICode(rest, MoveRR { source=sourceReg, output=destReg } :: code)

        |   codeGenICode(
                (* Load from memory. *)
                MoveArgument{ source=MemoryLocation mLoc, dest=RealRegister(GenReg destReg), kind=WordToWord} :: rest, code) =
                codeGenICode(rest, LoadMemR{source=memoryAddressAsBaseOffset mLoc, output=destReg} :: code)

        |   codeGenICode(
                (* Load from memory. *)
                MoveArgument{ source=MemoryLocation mLoc, dest=RealRegister(GenReg destReg), kind=ByteToWordZeroExtend} :: rest, code) =
                codeGenICode(rest, LoadByteR{source=memoryAddressAsBaseOffset mLoc, output=destReg} :: code)

                (* Store to memory *)
        |   codeGenICode(
                MoveArgument{ source=RealRegister(GenReg sourceReg), dest=MemoryLocation mLoc, kind=WordToWord} :: rest, code) =
                codeGenICode(rest,
                    StoreRegToMemory{toStore=sourceReg, address=memoryAddressAsBaseOffset mLoc} :: code)

        |   codeGenICode(
                MoveArgument{ source=RealRegister(GenReg sourceReg), dest=MemoryLocation mLoc, kind=ByteToByte} :: rest, code) =
                codeGenICode(rest,
                    StoreByteRegToMemory{toStore=sourceReg, address=memoryAddressAsBaseOffset mLoc} :: code)

                (* Move a short constant to a register *)
        |   codeGenICode(MoveArgument {source=IntegerConstant srcValue, dest=RealRegister(GenReg destReg), kind=WordToWord} :: rest, code) =
                codeGenICode(rest, MoveConstR{source=srcValue, output=destReg} :: code)

                (* Move a long constant to a register *)
        |   codeGenICode(MoveArgument {source=AddressConstant srcValue, dest=RealRegister(GenReg destReg), kind=WordToWord} :: rest, code) =
                codeGenICode(rest, MoveLongConstR{source=srcValue, output=destReg} :: code)

                (* Store a short constant to memory *)
        |   codeGenICode(
                MoveArgument{ source=IntegerConstant srcValue, dest=MemoryLocation mLoc, kind=WordToWord} :: rest, code) =
                codeGenICode(rest,
                    StoreConstToMemory{toStore=srcValue, address=memoryAddressAsBaseOffset mLoc} :: code)

                (* Store a long constant to memory *)
        |   codeGenICode(
                MoveArgument{ source=AddressConstant srcValue, dest=MemoryLocation mLoc, kind=WordToWord} :: rest, code) =
                codeGenICode(rest,
                    StoreLongConstToMemory{toStore=srcValue, address=memoryAddressAsBaseOffset mLoc} :: code)

        |   codeGenICode(MoveArgument _ :: _, _) =
                raise InternalError "codeGenICode: TODO MoveArgument"

        |   codeGenICode(FunctionCall {isTail=false, callKind, ...} :: rest, code) =
                codeGenICode(rest, CallFunction callKind :: code)

        |   codeGenICode(FunctionCall {isTail=true, callKind, ...} :: rest, code) =
                codeGenICode(rest, JumpToFunction callKind :: code)

        |   codeGenICode(AllocateMemoryOperation { size, isMutable, dest} :: rest, code) =
            let
                val toReg = asGenReg dest
                val flag = if isMutable then Address.F_mutable else 0w0

                (* Allocate memory.  N.B. Instructions are in reverse order. *)
                fun allocStore{size, flags, output, preserve} =
                if isX64 andalso flags <> 0w0
                then
                    [StoreByteConstToMemory{toStore=flags, address=BaseOffset{offset= ~1, base=output, index=NoIndex}},
                     StoreConstToMemory{toStore=LargeInt.fromInt size, address=BaseOffset{offset= ~wordSize, base=output, index=NoIndex}},
                     AllocStore{size=size, output=output, saveRegs=preserve}]
                else
                let
                    val lengthWord = IntInf.orb(IntInf.fromInt size, IntInf.<<(Word8.toLargeInt flags, 0w24))
                in
                    [StoreConstToMemory{toStore=lengthWord, address=BaseOffset{offset= ~wordSize, base=output, index=NoIndex}},
                     AllocStore{size=size, output=output, saveRegs=preserve}]
                end

                val allocCode =
                    (* If we need to add the profile object *)
                    if addAllocatingFunction
                    then
                        allocStore {size=size+1, flags=Word8.orb(flag, Address.F_profile), output=toReg, preserve=[]} @
                            [StoreLongConstToMemory{ toStore=profileObject, address=BaseOffset{base=toReg, offset=size*wordSize, index=NoIndex}}]
                    else allocStore {size=size, flags=flag, output=toReg, preserve=[]}
            in
                codeGenICode(rest, allocCode @ code)
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

        |   codeGenICode(WordComparison {arg1 as RealRegister _, arg2=IntegerConstant testVal, ...} :: rest, code) =
                codeGenICode(rest, ArithRConst {opc=CMP, output=asGenReg arg1, source=testVal} :: code)

        |   codeGenICode(WordComparison {arg1 as RealRegister _, arg2=AddressConstant testVal, ...} :: rest, code) =
                codeGenICode(rest, ArithRLongConst {opc=CMP, output=asGenReg arg1, source=testVal} :: code)

        |   codeGenICode(WordComparison {arg1 as RealRegister _, arg2=MemoryLocation{offset, base, index=NoMemIndex}, ...} :: rest, code) =
                codeGenICode(rest, ArithRMem {opc=CMP, output=asGenReg arg1, offset=offset, base=asGenReg base} :: code)

        |   codeGenICode(WordComparison _ :: _, _) =
                raise InternalError "codeGenICode: TODO JumpOnWordComparison"

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
                        toStore=esp, address=BaseOffset{offset=memRegHandlerRegister, base=ebp, index=NoIndex}} ::
                    PushR handleReg ::
                    LoadHandlerAddress{ handlerLab=labelRef, output=handleReg} ::
                    PushMem{base=ebp, offset=memRegHandlerRegister} :: code)
            end

        |   codeGenICode(PopExceptionHandler { workReg, ... } :: rest, code) =
            let (* Remove an exception handler if no exception was raised. *)
                val wReg = asGenReg workReg
            in
                (* The stack pointer has been adjusted to just above the two words that were stored
                   in PushExceptionHandler. *)
                codeGenICode(rest,
                    StoreRegToMemory{
                        toStore=wReg, address=BaseOffset{offset=memRegHandlerRegister, base=ebp, index=NoIndex}} ::
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
                        toStore=wReg, address=BaseOffset{offset=memRegHandlerRegister, base=ebp, index=NoIndex}} ::
                    PopR wReg :: ResetStack 1 ::
                    LoadMemR{ source=BaseOffset{base=ebp, offset=memRegHandlerRegister, index=NoIndex}, output=esp } ::
                    StartHandler{handlerLab=labelRef} :: code)
            end

        |   codeGenICode(ReturnResultFromFunction { numStackArgs, ... } :: rest, code) =
                codeGenICode(rest, ReturnFromFunction numStackArgs :: code)

        |   codeGenICode(
                ArithmeticFunction{
                    oper, resultReg=RealRegister(GenReg resReg), operand1=RealRegister(GenReg op1Reg),
                    operand2=IntegerConstant op2Value, ...} :: rest, code) =
            let
                val _ = resReg = op1Reg orelse raise InternalError "codeGenICode: ArithmeticFunction"
            in
                codeGenICode(rest, ArithRConst { opc=oper, output=resReg, source=op2Value } :: code)
            end

        |   codeGenICode(
                ArithmeticFunction{
                    oper, resultReg=RealRegister(GenReg resReg), operand1=RealRegister(GenReg op1Reg),
                    operand2=RealRegister(GenReg op2Reg), ...} :: rest, code) =
            let
                val _ = resReg = op1Reg orelse raise InternalError "codeGenICode: ArithmeticFunction"
            in
                codeGenICode(rest, ArithRR { opc=oper, output=resReg, source=op2Reg } :: code)
            end

        |   codeGenICode(ArithmeticFunction _ :: _, _) =
                raise InternalError "codeGenICode: TODO codeGenICode - ArithmeticFunction"

        |   codeGenICode(TestTagBit {arg, ...} :: rest, code) =
                codeGenICode(rest, TestTagR(asGenReg arg) :: code)

        |   codeGenICode(PushValue { arg = RealRegister(GenReg r) } :: rest, code) =
                codeGenICode(rest, PushR r :: code)

        |   codeGenICode(PushValue { arg = IntegerConstant v } :: rest, code) =
                codeGenICode(rest, PushConst v :: code)

        |   codeGenICode(PushValue _ :: _, _) =
                raise InternalError "codeGenICode: TODO PushValue"

        |   codeGenICode(ResetStackPtr {numWords} :: rest, code) =
                codeGenICode(rest, ResetStack numWords :: code)

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
                ShiftOperation{
                    shift, resultReg=RealRegister(GenReg resReg), operand=RealRegister(GenReg opReg),
                    shiftAmount=IntegerConstant shiftValue, ...} :: rest, code) =
            let
                val _ = resReg = opReg orelse raise InternalError "codeGenICode: ArithmeticFunction"
            in
                codeGenICode(rest, ShiftConstant{ shiftType=shift, output=resReg, shift=Word8.fromLargeInt shiftValue } :: code)
            end

        |   codeGenICode(ShiftOperation _ :: _, _) =
                raise InternalError "codeGenICode: TODO ShiftOperation"

            (* MoveStackArgToPreg should have been removed by earlier passes. *)
        |   codeGenICode(MoveStackArgToPreg _ :: _, _) =
                raise InternalError "codeGenICode: MoveStackArgToPreg"

            (* ReserveContainer should have been removed by earlier passes. *)
        |   codeGenICode(ReserveContainer _ :: _, _) =
                raise InternalError "codeGenICode: ReserveContainer"

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
            |   pushThenPop (aReg::regs) = PushR aReg :: (pushThenPop regs @ [PopR aReg])
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
            [ArithRMem{ opc=CMP, output=reg, offset=memRegStackLimit, base=ebp }] @
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
    end

end;
