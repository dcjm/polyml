(*
    Copyright David C. J. Matthews 2016-19

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
        type closureRef

        val generateCode: {code: code, ops: operations, labelCount: int, resultClosure: closureRef } -> unit

        structure Sharing:
        sig
            type operation = operation
            type code = code
            type closureRef = closureRef
        end
    end

    structure DEBUG: DEBUGSIG
    
    structure ICODE: ICodeSig
    structure IDENTIFY: X86IDENTIFYREFSSIG
    structure INTSET: INTSETSIG
    structure PRETTY: PRETTYSIG

    structure STRONGLY:
        sig
            val stronglyConnectedComponents: {nodeAddress: 'a -> int, arcs: 'a -> int list } -> 'a list -> 'a list list
        end
    
    sharing X86CODE.Sharing = ICODE.Sharing = X86OPTIMISE.Sharing = IDENTIFY.Sharing = INTSET
): X86ICODEGENERATESIG =
struct
    open IDENTIFY
    open ICODE

    open X86CODE

    open Address
    
    exception InternalError = Misc.InternalError
    
    fun asGenReg(GenReg r) = r
    |   asGenReg _ = raise InternalError "asGenReg"
    
    and asFPReg(FPReg r) = r
    |   asFPReg _ = raise InternalError "asFPReg"
   
    and asXMMReg(XMMReg r) = r
    |   asXMMReg _ = raise InternalError "asXMMReg"

    (* tag a short constant *)
    fun tag c = 2 * c + 1

    local
        val regs =
            case targetArch of
                Native32Bit     => [edi, esi, edx, ecx, ebx, eax]
            |   Native64Bit     => [r14, r13, r12, r11, r10, r9, r8, edi, esi, edx, ecx, ebx, eax]
            |   ObjectId32Bit   => [r14, r13, r12, r11, r10, r9, r8, edi, esi, edx, ecx, eax]
    in
        val generalRegisters = List.map GenReg regs
    end

    fun opSizeToMove OpSize32 = Move32 | opSizeToMove OpSize64 = Move64

    fun icodeToX86Code{blocks, functionName, stackRequired, debugSwitches, allocatedRegisters, resultClosure, ...} =
    let        
        fun argAsGenReg(RegisterArg(GenReg r)) = r
        |   argAsGenReg _ = raise InternalError "argAsGenReg"

        fun sourceAsGenRegOrMem(RegisterArg(GenReg r)) = RegisterArg r
        |   sourceAsGenRegOrMem(MemoryArg{offset, base=baseReg, index}) =
                MemoryArg{base=baseReg, offset=offset, index=index}
        |   sourceAsGenRegOrMem(NonAddressConstArg v) = NonAddressConstArg v
        |   sourceAsGenRegOrMem(AddressConstArg v) = AddressConstArg v
        |   sourceAsGenRegOrMem _ = raise InternalError "sourceAsGenRegOrMem"

        and sourceAsXMMRegOrMem(RegisterArg(XMMReg r)) = RegisterArg r
        |   sourceAsXMMRegOrMem(MemoryArg{offset, base=baseReg, index}) =
                MemoryArg{base=baseReg, offset=offset, index=index}
        |   sourceAsXMMRegOrMem(NonAddressConstArg v) = NonAddressConstArg v
        |   sourceAsXMMRegOrMem(AddressConstArg v) = AddressConstArg v
        |   sourceAsXMMRegOrMem _ = raise InternalError "sourceAsGenRegOrMem"

        (* Moves and loads. *)
        fun llLoadArgument({ source, dest=GenReg destReg, kind=Move64Bit}, code) =
                Move { source=sourceAsGenRegOrMem source, destination=RegisterArg destReg, moveSize=Move64 } :: code

        |   llLoadArgument({ source=MemoryArg mLoc, dest=GenReg destReg, kind=MoveByte}, code) = (* Load from memory. *)
                Move{moveSize=Move8, source=MemoryArg mLoc, destination=RegisterArg destReg} :: code

        |   llLoadArgument({ source=MemoryArg mLoc, dest=GenReg destReg, kind=Move16Bit}, code) = (* Load from memory. *)
                Move{moveSize=Move16, source=MemoryArg mLoc, destination=RegisterArg destReg} :: code

        |   llLoadArgument({ source, dest=GenReg destReg, kind=Move32Bit}, code) = (* Load from memory. *)
                Move { source=sourceAsGenRegOrMem source, destination=RegisterArg destReg, moveSize=Move32 } :: code

                (* Load a floating point value. *)
        |   llLoadArgument({source=MemoryArg{offset, base=baseReg, index}, dest=FPReg fpReg, kind=MoveDouble}, code) =
                moveToOutputFP(fpReg,
                   FPLoadFromMemory{ address={base=baseReg, offset=offset, index=index}, precision=DoublePrecision } :: code)

        |   llLoadArgument({source=AddressConstArg addrConst, dest=FPReg fpReg, kind=MoveDouble}, code) =
                moveToOutputFP(fpReg, FPLoadFromConst{ constant= addrConst, precision=DoublePrecision } :: code)
        
        |   llLoadArgument({source=RegisterArg(FPReg fpSrc), dest=FPReg fpDest, kind=MoveDouble}, code) =
                (* Moving from one FP reg to another.  Even if we are moving from FP0 we still do a load
                   because FPStoreToFPReg adds one to the register number to account for one value on the
                   stack. *)
                moveToOutputFP(fpDest, FPLoadFromFPReg{source=fpSrc, lastRef=false} :: code)

                (* Load or move from an XMM reg. *)
        |   llLoadArgument({source, dest=XMMReg xmmRegReg, kind=MoveDouble}, code) =
                XMMArith { opc= SSE2MoveDouble, source=sourceAsXMMRegOrMem source, output=xmmRegReg } :: code

                (* Load a floating point value. *)
        |   llLoadArgument({source=MemoryArg{offset, base=baseReg, index}, dest=FPReg fpReg, kind=MoveFloat}, code) =
                moveToOutputFP(fpReg,
                    FPLoadFromMemory{ address={ base=baseReg, offset=offset, index=index }, precision=SinglePrecision } :: code)

        |   llLoadArgument({source=AddressConstArg addrConst, dest=FPReg fpReg, kind=MoveFloat}, code) =
                moveToOutputFP(fpReg, FPLoadFromConst{ constant= addrConst, precision=SinglePrecision } :: code)

                (* Load or move from an XMM reg. *)
        |   llLoadArgument({source, dest=XMMReg xmmRegReg, kind=MoveFloat}, code) =
                XMMArith { opc= SSE2MoveFloat, source=sourceAsXMMRegOrMem source, output=xmmRegReg } :: code

            (* Any other combinations are not allowed. *)
        |   llLoadArgument _ = raise InternalError "codeGenICode: LoadArgument"
        
        (* Unless the destination is FP0 we need to store and pop. *)
        and moveToOutputFP(fpDest, code) =
            if fpDest = fp0 then code
            else FPStoreToFPReg{output=fpDest, andPop=true} :: code

                (* Store to memory *)
        fun llStoreArgument{ source=RegisterArg(GenReg sourceReg), base, offset, index, kind=Move64Bit} =
                Move{source=RegisterArg sourceReg, destination=MemoryArg {base=base, offset=offset, index=index}, moveSize=opSizeToMove OpSize64}

        |   llStoreArgument{ source=RegisterArg(GenReg sourceReg), base, offset, index, kind=MoveByte} =
                Move{moveSize=Move8, source=RegisterArg sourceReg, destination=MemoryArg {base=base, offset=offset, index=index}} 

        |   llStoreArgument{ source=RegisterArg(GenReg sourceReg), base, offset, index, kind=Move16Bit} =
                Move{moveSize=Move16, source=RegisterArg sourceReg, destination=MemoryArg {base=base, offset=offset, index=index}}

        |   llStoreArgument{ source=RegisterArg(GenReg sourceReg), base, offset, index, kind=Move32Bit} =
                Move{source=RegisterArg sourceReg, destination=MemoryArg {base=base, offset=offset, index=index}, moveSize=opSizeToMove OpSize32}

                (* Store a short constant to memory *)
        |   llStoreArgument{ source=NonAddressConstArg srcValue, base, offset, index, kind=Move64Bit} =
                Move{source=NonAddressConstArg srcValue, destination=MemoryArg {base=base, offset=offset, index=index}, moveSize=Move64}
                
        |   llStoreArgument{ source=NonAddressConstArg srcValue, base, offset, index, kind=Move32Bit} =
                Move{source=NonAddressConstArg srcValue, destination=MemoryArg {base=base, offset=offset, index=index}, moveSize=Move32}

        |   llStoreArgument{ source=NonAddressConstArg srcValue, base, offset, index, kind=MoveByte} =
                Move{moveSize=Move8, source=NonAddressConstArg srcValue, destination=MemoryArg{base=base, offset=offset, index=index}}

                (* Store a long constant to memory *)
        |   llStoreArgument{ source=AddressConstArg srcValue, base, offset, index, kind} =
            (
                (* This Move must be of a polyWord size. *)
                case (kind, polyWordOpSize) of
                    (Move64Bit, OpSize64) => ()
                |   (Move32Bit, OpSize32) => ()
                |   _ => raise InternalError "Move of AddressConstArg";
                Move{moveSize=opSizeToMove polyWordOpSize, source=AddressConstArg srcValue, destination=MemoryArg {base=base, offset=offset, index=index}}
            )

                (* Store a floating point value. *)
        |   llStoreArgument{source=RegisterArg(FPReg fpReg), offset, base=baseReg, index, kind=MoveDouble} =
            let
                val _ = fpReg = fp0 orelse raise InternalError "llStoreArgument: Store FPReg <> fp0"
            in
                 FPStoreToMemory{ address={ base=baseReg, offset=offset, index=index}, precision=DoublePrecision, andPop=true }
            end

        |   llStoreArgument{source=RegisterArg(XMMReg xmmRegReg), offset, base=baseReg, index, kind=MoveDouble} =
                 XMMStoreToMemory { toStore=xmmRegReg, address={base=baseReg, offset=offset, index=index}, precision=DoublePrecision }

                (* Store a floating point value. *)
        |   llStoreArgument{source=RegisterArg(FPReg fpReg), offset, base=baseReg, index, kind=MoveFloat} =
            let
                val _ = fpReg = fp0 orelse raise InternalError "llStoreArgument: Store FPReg <> fp0"
            in
                 FPStoreToMemory{address={ base=baseReg, offset=offset, index=index}, precision=SinglePrecision, andPop=true }
            end

        |   llStoreArgument{source=RegisterArg(XMMReg xmmRegReg), offset, base=baseReg, index, kind=MoveFloat} =
                 XMMStoreToMemory { toStore=xmmRegReg, address={base=baseReg, offset=offset, index=index}, precision=SinglePrecision }

        |   llStoreArgument _ = raise InternalError "llStoreArgument: StoreArgument"
        
        val numBlocks = Vector.length blocks

        fun getAllocatedReg r = Vector.sub(allocatedRegisters, r)
        
        val getAllocatedGenReg = asGenReg o getAllocatedReg
        and getAllocatedFPReg = asFPReg o getAllocatedReg
        and getAllocatedXMMReg = asXMMReg o getAllocatedReg

        fun codeExtIndex NoMemIndex = NoIndex
        |   codeExtIndex(MemIndex1(PReg r)) = Index1(getAllocatedGenReg r)
        |   codeExtIndex(MemIndex2(PReg r)) = Index2(getAllocatedGenReg r)
        |   codeExtIndex(MemIndex4(PReg r)) = Index4(getAllocatedGenReg r)
        |   codeExtIndex(MemIndex8(PReg r)) = Index8(getAllocatedGenReg r)
        |   codeExtIndex ObjectIndex = raise InternalError "codeExtIndex: ObjectIndex"

        local
            fun codeExtArgument getReg (RegisterArgument(PReg r)) = RegisterArg(getReg r)
            |   codeExtArgument _ (AddressConstant m) = AddressConstArg m
            |   codeExtArgument _ (IntegerConstant i) = NonAddressConstArg i
            |   codeExtArgument _ (MemoryLocation{base=PReg bReg, offset, index=ObjectIndex, cache=NONE}) =
                    MemoryArg{base=ebx, index=Index4(getAllocatedGenReg bReg), offset=offset}
            |   codeExtArgument _ (MemoryLocation{base=PReg bReg, offset, index, cache=NONE}) =
                    MemoryArg{base=getAllocatedGenReg bReg, offset=offset, index=codeExtIndex index}
            |   codeExtArgument getReg (MemoryLocation{cache=SOME(PReg r), ...}) = RegisterArg(getReg r)
            |   codeExtArgument _ (StackLocation{wordOffset, cache=NONE, ...}) =
                    MemoryArg{base=esp, offset=wordOffset*Word.toInt nativeWordSize, index=NoIndex}
            |   codeExtArgument getReg (StackLocation{cache=SOME(PReg r), ...}) = RegisterArg(getReg r)
            |   codeExtArgument _ (ContainerAddr _) = raise InternalError "codeExtArgument - ContainerAddr"
        in
            val codeExtArgument = codeExtArgument getAllocatedReg
            and codeExtArgumentAsGenReg = codeExtArgument getAllocatedGenReg
            and codeExtArgumentAsFPReg = codeExtArgument getAllocatedFPReg
            and codeExtArgumentAsXMMReg = codeExtArgument getAllocatedXMMReg
        end

        fun codeCallKind Recursive = NonAddressConstArg 0 (* Jump to the start *)
        |   codeCallKind (ConstantCode v) = AddressConstArg v
        |   codeCallKind FullCall =
            (
                case targetArch of
                    ObjectId32Bit => MemoryArg{base=ebx, index=Index4 edx, offset=0}
                |   _ => MemoryArg{base=edx, index=NoIndex, offset=0}
            )

        (* Move unless the registers are the same. *)
        fun moveIfNecessary({src, dst, kind}, code) =
            if src = dst then code
            else llLoadArgument({source=RegisterArg src, dest=dst, kind=kind}, code)
        
        fun opSizeToIMove OpSize64 = Move64Bit
        |   opSizeToIMove OpSize32 = Move32Bit

        datatype llsource =
            StackSource of int
        |   OtherSource of reg regOrMemoryArg

        fun sourceToX86Code(OtherSource r) = r
        |   sourceToX86Code(StackSource wordOffset) = MemoryArg{base=esp, offset=wordOffset*Word.toInt nativeWordSize, index=NoIndex}

        local
            fun indexRegister NoIndex = NONE
            |   indexRegister (Index1 r) = SOME r
            |   indexRegister (Index2 r) = SOME r
            |   indexRegister (Index4 r) = SOME r
            |   indexRegister (Index8 r) = SOME r
            (* The registers are numbered from 0.  Choose values that don't conflict with
               the stack addresses. *)
            fun regNo r = ~1 - nReg r
            type node = {src: llsource, dst: destinations }
            
            fun nodeAddress({dst=RegDest r, ...}: node) = regNo r
            |   nodeAddress({dst=StackDest a, ...}) = a
            
            fun arcs({src=StackSource wordOffset, ...}: node) = [wordOffset]
            |   arcs{src=OtherSource(RegisterArg r), ...} = [regNo r]
            |   arcs{src=OtherSource(MemoryArg{base, index, ...}), ...} =
                    (case indexRegister index of NONE => [regNo(GenReg base)] | SOME r => [regNo(GenReg base), regNo(GenReg r)])
            |   arcs _ = []
        in
            val stronglyConnected = STRONGLY.stronglyConnectedComponents { nodeAddress=nodeAddress, arcs=arcs }
        end
        
        (* This is a general function for moving values into registers or to the stack
           where it is possible that the source values might also be in use as destinations.
           The stack is used for destinations only for tail recursive calls. *)
        fun moveMultipleValues(moves, workReg: reg option, code) =
        let
            val _ =
                if List.exists(fn {dst=StackDest _, ...} => true | _ => false) moves andalso not(isSome workReg) then raise InternalError "no work reg" else ()
 
            fun moveValues ([], code) = code (* We're done. *)

            |   moveValues (arguments, code) =
                let
                    (* stronglyConnectedComponents does two things.  It detects loops where
                       it's not possible to move items without breaking the loop but more
                       importantly it orders the dependencies so that if there are no loops we
                       can load the source and store it in the destination knowing that
                       we won't overwrite anything we might later need. *)
                    
                    val ordered = stronglyConnected arguments

                    fun isFPReg(GenReg _) = false
                    |   isFPReg(XMMReg _) = true
                    |   isFPReg(FPReg _) = true
                    
                    fun moveEachValue ([], code) = code

                    |   moveEachValue ([{dst=RegDest reg, src as OtherSource(RegisterArg r)}] :: rest, code) =
                            (* Source and dest are both regs - only move if they're different. *)
                            if r = reg
                            then moveEachValue(rest, code)
                            else moveEachValue(rest,
                                    llLoadArgument({source=sourceToX86Code src, dest=reg, kind=if isFPReg reg then MoveDouble else moveNativeWord}, code))

                    |   moveEachValue ([{dst=RegDest reg, src as StackSource _}] :: rest, code) =
                            (* If loading from the stack always use native word.  The value could be a stack address. *)
                            moveEachValue(rest, llLoadArgument({source=sourceToX86Code src, dest=reg, kind=moveNativeWord}, code))

                    |   moveEachValue ([{dst=RegDest reg, src}] :: rest, code) =
                            (* Load from store or a constant.  Have to use movePolyWord if it's an address constant. *)
                            moveEachValue(rest, llLoadArgument({source=sourceToX86Code src, dest=reg, kind=movePolyWord}, code))

                    |   moveEachValue ([{dst=StackDest _, src=OtherSource(MemoryArg _ )}] :: _, _) =
                            raise InternalError "moveEachValue - MemoryArgument"

                    |   moveEachValue ([{dst=StackDest addr, src as StackSource wordOffset}] :: rest, code) =
                            (* Copy a stack location - needs a load and store unless the address is the same. *)
                            if addr = wordOffset
                            then moveEachValue(rest, code)
                            else
                            let
                                val workReg = valOf workReg
                            in
                                moveEachValue(rest,
                                    llStoreArgument{source=RegisterArg workReg, base=esp, index=NoIndex,
                                                offset = addr*Word.toInt nativeWordSize, kind=moveNativeWord} ::
                                       llLoadArgument({source=sourceToX86Code src, dest=workReg, kind=moveNativeWord}, code))
                            end

                    |   moveEachValue ([{dst=StackDest addr, src}] :: rest, code) =
                            (* Store from a register or a constant. *)
                            moveEachValue(rest,
                                llStoreArgument{
                                    source=sourceToX86Code src, base=esp, index=NoIndex, offset = addr*Word.toInt nativeWordSize, kind=moveNativeWord} :: code)

                    |   moveEachValue((cycle as first :: _ :: _) :: rest, code) =
                        (* We have a cycle. *)
                        let
                            (* We need to exchange some of the arguments.  Doing an exchange here will
                               set the destination with the correct source.  However we have to process
                               every subsequent entry with the swapped registers.  That may well mean that
                               one of those entries becomes trivial.  Using XCHG means that we can move
                               N registers in N-1 exchanges.
                               We also need to rerun stronglyConnectedComponents on at least the rest of
                               this cycle.  It's easiest to flatten the rest and do everything. *)
                            (* Try to find either a register-register move or a register-stack move.
                               If not use the first.  If there's a stack-register move there will
                               also be a register-stack so we don't need to look for both. *)
                            val {dst=selectDst, src=selectSrc} =
                                case List.find(fn {src=OtherSource(RegisterArg _), dst=RegDest _} => true | _ => false) cycle of
                                    SOME found => found
                                |   _ =>
                                    (
                                        case List.find(fn {dst=RegDest _, ...} => true | _ => false) cycle of
                                            SOME found => found
                                        |   NONE => first
                                    )
                            (* This includes this entry but after the swap we'll eliminate it. *)
                            val flattened = List.foldl(fn (a, b) => a @ b) [] (cycle :: rest)
                            val destAsSource =
                                case selectDst of
                                    RegDest reg => OtherSource(RegisterArg reg)
                                |   StackDest s => StackSource s

                            (* Source is not an equality type.  We can't currently handle the
                               situation where the source is a memory location. *)
                            fun match(OtherSource(RegisterArg r1), OtherSource(RegisterArg r2)) = r1 = r2
                            |   match(StackSource s1, StackSource s2) = s1 = s2
                            |   match(OtherSource(MemoryArg _), _) = raise InternalError "moveEachValue: cycle"
                            |   match _ = false
                            
                            fun swapSources{src, dst} =
                                if match(src, selectSrc) then {src=destAsSource, dst=dst}
                                else if match(src, destAsSource) then {src=selectSrc, dst=dst}
                                else {src=src, dst=dst}
                            (* Try to use register to register exchange if we can.
                               A register-to-memory exchange involves a bus lock and we'd
                               like to avoid that. *)
                            val exchangeCode =
                                case (selectDst, selectSrc) of
                                    (RegDest(GenReg regA), OtherSource(RegisterArg(GenReg regB))) =>
                                        XChng { reg=regA, arg=RegisterArg regB, opSize=nativeWordOpSize } :: code

                                |   (RegDest(XMMReg regA), OtherSource(RegisterArg(XMMReg regB))) =>
                                        (* This is the only case where we can have a cycle with SSE2 regs.
                                           There are various ways of doing it but XORs are probably the easiest. *)
                                        XMMArith{opc=SSE2Xor, source=RegisterArg regA, output=regB} ::
                                        XMMArith{opc=SSE2Xor, source=RegisterArg regB, output=regA} ::
                                        XMMArith{opc=SSE2Xor, source=RegisterArg regA, output=regB} :: code

                                |   (RegDest _, OtherSource(RegisterArg _)) =>
                                        raise InternalError "moveEachValue: invalid register combination"
                                        
                                |   (RegDest regA, src as StackSource addr) =>
                                    let
                                        val workReg = valOf workReg
                                    in
                                        llStoreArgument{source=RegisterArg workReg, base=esp, index=NoIndex,
                                                offset = addr*Word.toInt nativeWordSize, kind=moveNativeWord} ::
                                            XChng { reg=asGenReg regA, arg=RegisterArg(asGenReg workReg), opSize=nativeWordOpSize } ::
                                                llLoadArgument({source=sourceToX86Code src, dest=workReg, kind=moveNativeWord}, code)
                                    end

                                |   (StackDest addr, OtherSource(RegisterArg regA)) =>
                                    let
                                        (* This doesn't actually occur because we always find the case above. *)
                                        val workReg = valOf workReg
                                    in
                                        llStoreArgument{source=RegisterArg workReg, base=esp, index=NoIndex,
                                                offset = addr*Word.toInt nativeWordSize, kind=moveNativeWord} ::
                                            XChng { reg=asGenReg regA, arg=RegisterArg (asGenReg workReg), opSize=nativeWordOpSize } ::
                                                llLoadArgument({
                                                    source=MemoryArg{base=esp, offset=addr*Word.toInt nativeWordSize, index=NoIndex}, dest=workReg, kind=moveNativeWord}, code)
                                    end

                                |   (StackDest addr1, StackSource addr2) =>
                                    let
                                        val workReg = valOf workReg
                                        (* This can still happen if we have argument registers that need to be
                                           loaded from stack locations and those argument registers happen to
                                           contain the values to be stored into those stack locations.
                                           e.g. ebx => S8; eax => S7; S8 => eax; S7 => eax.
                                           Eliminating the registers results in a cycle.
                                           It may be possible to avoid this by excluding the argument
                                           registers (eax; ebx; r8; r9; r10) from holding values in the
                                           area to be overwritten. *)
                                    in
                                        llStoreArgument{source=RegisterArg workReg, base=esp, index=NoIndex,
                                                offset = addr1*Word.toInt nativeWordSize, kind=moveNativeWord} ::
                                            XChng { reg=asGenReg workReg,
                                                arg=MemoryArg{base=esp, offset=addr2*Word.toInt nativeWordSize, index=NoIndex},
                                                opSize=nativeWordOpSize } ::
                                                llLoadArgument({
                                                    source=MemoryArg{base=esp, offset=addr1*Word.toInt nativeWordSize, index=NoIndex}, dest=workReg, kind=moveNativeWord}, code)
                                    end
                                
                                |   _ => raise InternalError "moveEachValue: cycle"
                                    
                        in
                            moveValues(List.map swapSources flattened, exchangeCode)
                        end

                    |   moveEachValue(([]) :: _, _) = (* This should not happen - avoid warning. *)
                            raise InternalError "moveEachValue - empty set"
                in
                    moveEachValue(ordered, code)
                end
        in
            moveValues(moves, code)
        end

        (* Where we have multiple specific registers as either source or
           destination there is the potential that a destination register
           if currently in use as a source. *) 
        fun moveMultipleRegisters(regPairList, code) =
        let
            val regPairsAsDests =
                List.map(fn {src, dst} => {src=OtherSource(RegisterArg src), dst=RegDest dst}) regPairList
        in
            moveMultipleValues(regPairsAsDests, NONE, code)
        end

        val outputLabelCount = ref 0
        val blockToLabelMap = Array.array(numBlocks, ~1)

        fun makeLabel() = Label{labelNo = ! outputLabelCount} before outputLabelCount := !outputLabelCount + 1
       
        fun getBlockLabel blockNo =
            case Array.sub(blockToLabelMap, blockNo) of
                ~1 =>
                let
                    val label as Label{labelNo} = makeLabel()
                    val () = Array.update(blockToLabelMap, blockNo, labelNo)
                in label end
            |   n => Label{labelNo=n}

        (* The profile object is a single mutable with the F_bytes bit set. *)
        local
            val v = RunCall.allocateByteMemory(0w1, Word.fromLargeWord(Word8.toLargeWord(Word8.orb(F_mutable, F_bytes))))
            fun clear 0w0 = ()
            |   clear i = (assignByte(v, i-0w1, 0w0); clear (i-0w1))
            val () = clear wordSize
        in
            val profileObject = toMachineWord v
        end
        (* Switch to indicate if we want to trace where live data has been allocated. *)
        val addAllocatingFunction =
            DEBUG.getParameter DEBUG.profileAllocationTag debugSwitches = 1

        fun llAllocateMemoryOperation ({ size, flags, dest, saveRegs}, code) =
        let
            val toReg = asGenReg dest
            val preserve = saveRegs

            (* Allocate memory.  N.B. Instructions are in reverse order. *)
            fun allocStore{size, flags, output, preserve} =
            if targetArch = Native64Bit andalso flags <> 0w0
            then
                [Move{moveSize=Move8, source=NonAddressConstArg(Word8.toLargeInt flags), destination=MemoryArg {offset= ~1, base=output, index=NoIndex}},
                 Move{source=NonAddressConstArg(LargeInt.fromInt size),
                      destination=MemoryArg {offset= ~ (Word.toInt wordSize), base=output, index=NoIndex},
                      moveSize=opSizeToMove polyWordOpSize},
                 AllocStore{size=size, output=output, saveRegs=preserve}]
            else
            let
                val lengthWord = IntInf.orb(IntInf.fromInt size, IntInf.<<(Word8.toLargeInt flags, 0w24))
            in
                [Move{source=NonAddressConstArg lengthWord, destination=MemoryArg {offset= ~ (Word.toInt wordSize), base=output, index=NoIndex},
                      moveSize=opSizeToMove polyWordOpSize},
                 AllocStore{size=size, output=output, saveRegs=preserve}]
            end

            val allocCode =
                (* If we need to add the profile object *)
                if addAllocatingFunction
                then
                    allocStore {size=size+1, flags=Word8.orb(flags, Address.F_profile), output=toReg, preserve=preserve} @
                        [Move{moveSize=opSizeToMove polyWordOpSize, source=AddressConstArg profileObject,
                              destination=MemoryArg {base=toReg, offset=size*Word.toInt wordSize, index=NoIndex}}]
                else allocStore {size=size, flags=flags, output=toReg, preserve=preserve}
            
            (* Convert to an object index if necessary. *)
            val convertToObjId =
                if targetArch = ObjectId32Bit
                then [ ShiftConstant{ shiftType=SHR, output=toReg, shift=0w2, opSize=OpSize64 },
                       ArithToGenReg{ opc=SUB, output=toReg, source=RegisterArg ebx, opSize=nativeWordOpSize } ]
                else []
        in
            convertToObjId @ allocCode @ code
        end

        (* Check the stack limit "register".  This is used both at the start of a function for genuine
           stack checking but also in a loop to check for an interrupt.  We need to save the registers
           even across an interrupt because it can be used if another thread wants a GC. *)
        fun testRegAndTrap(reg, entryPt, saveRegs) =
        let
            (* Normally we won't have a stack overflow so we will skip the check. *)
            val skipCheckLab = makeLabel()
        in
            (* Need it in reverse order. *)
            [
                JumpLabel skipCheckLab,
                CallRTS{rtsEntry=entryPt, saveRegs=saveRegs},
                ConditionalBranch{test=JNB, label=skipCheckLab},
                ArithToGenReg{ opc=CMP, output=reg, source=MemoryArg{offset=memRegStackLimit, base=ebp, index=NoIndex}, opSize=nativeWordOpSize }
            ]
        end
        
        local
            val numRegisters = Vector.length allocatedRegisters
            val uses = Array.array(numRegisters, false)
            fun used(PReg r) = Array.update(uses, r, true)
            fun isUsed(PReg r) = Array.sub(uses, r)

            (* Set the registers used by the sources.  This differs from getInstructionState in that we don't set
               the base register of a memory location to "used" if we can use the cache. *)
            fun argUses(RegisterArgument rarg) = used rarg
            |   argUses(MemoryLocation { cache=SOME cr, ...}) = used cr
            |   argUses(MemoryLocation { base, index, cache=NONE, ...}) = (used base; indexUses index)
            |   argUses(StackLocation { cache=SOME rarg, ...}) = used rarg
            |   argUses _ = ()
    
            and indexUses NoMemIndex = ()
            |   indexUses(MemIndex1 arg) = used arg
            |   indexUses(MemIndex2 arg) = used arg
            |   indexUses(MemIndex4 arg) = used arg
            |   indexUses(MemIndex8 arg) = used arg
            |   indexUses ObjectIndex = ()

            (* LoadArgument, TagValue, CopyToCache, UntagValue and BoxValue are eliminated if their destination
               is not used.  In that case their source are not used either. *)
            fun instructionUses(LoadArgument { source, dest, ...}) = if isUsed dest then argUses source else ()
            |   instructionUses(StoreArgument{ source, base, index, ...}) = (argUses source; used base; indexUses index)
            |   instructionUses(LoadMemReg _) = ()
            |   instructionUses(BeginFunction _) = ()
            |   instructionUses(FunctionCall{regArgs, stackArgs, ...}) = (List.app(argUses o #1) regArgs; List.app argUses stackArgs)
            |   instructionUses(TailRecursiveCall{regArgs, stackArgs, ...}) = (List.app(argUses o #1) regArgs; List.app(argUses o #src) stackArgs)
            |   instructionUses(AllocateMemoryOperation _) = ()
            |   instructionUses(AllocateMemoryVariable{size, ...}) = used size
            |   instructionUses(InitialiseMem{size, addr, init}) = (used size; used addr; used init)
            |   instructionUses(InitialisationComplete) = ()
            |   instructionUses(BeginLoop) = ()
            |   instructionUses(JumpLoop{regArgs, stackArgs, ...}) = (List.app(argUses o #1) regArgs; List.app(argUses o #1) stackArgs)
            |   instructionUses(RaiseExceptionPacket{packetReg}) = used packetReg
            |   instructionUses(ReserveContainer _) = ()
            |   instructionUses(IndexedCaseOperation{testReg, ...}) = used testReg
            |   instructionUses(LockMutable{addr}) = used addr
            |   instructionUses(WordComparison{arg1, arg2, ...}) = (used arg1; argUses arg2)
            |   instructionUses(CompareLiteral{arg1, ...}) = argUses arg1
            |   instructionUses(CompareByteMem{arg1={base, index, ...}, ...}) = (used base; indexUses index)
            |   instructionUses(PushExceptionHandler _) = ()
            |   instructionUses(PopExceptionHandler _) = ()
            |   instructionUses(BeginHandler _) = ()
            |   instructionUses(ReturnResultFromFunction{resultReg, ...}) = used resultReg
            |   instructionUses(ArithmeticFunction{operand1, operand2, ...}) = (used operand1; argUses operand2)
            |   instructionUses(TestTagBit{arg, ...}) = argUses arg
            |   instructionUses(PushValue {arg, ...}) = argUses arg
            |   instructionUses(CopyToCache{source, dest, ...}) = if isUsed dest then used source else ()
            |   instructionUses(ResetStackPtr _) = ()
            |   instructionUses(StoreToStack {source, ...}) = argUses source
            |   instructionUses(TagValue{source, dest, ...}) = if isUsed dest then used source else ()
            |   instructionUses(UntagValue{dest, cache=SOME cacheR, ...}) = if isUsed dest then used cacheR else ()
            |   instructionUses(UntagValue{source, dest, cache=NONE, ...}) = if isUsed dest then used source else ()
            |   instructionUses(LoadEffectiveAddress{base, index, ...}) = (case base of SOME bReg => used bReg | NONE => (); indexUses index)
            |   instructionUses(ShiftOperation{operand, shiftAmount, ...}) = (used operand; argUses shiftAmount)
            |   instructionUses(Multiplication{operand1, operand2, ...}) = (used operand1; argUses operand2)
            |   instructionUses(Division{dividend, divisor, ...}) = (used dividend; argUses divisor)
            |   instructionUses(AtomicExchangeAndAdd{base, source}) = (used base; used source)
            |   instructionUses(BoxValue{source, dest, ...}) = if isUsed dest then used source else ()
            |   instructionUses(CompareByteVectors{vec1Addr, vec2Addr, length, ...}) = (used vec1Addr; used vec2Addr; used length)
            |   instructionUses(BlockMove{srcAddr, destAddr, length, ...}) = (used srcAddr; used destAddr; used length)
            |   instructionUses(X87Compare{arg1, arg2, ...}) = (used arg1; argUses arg2)
            |   instructionUses(SSE2Compare{arg1, arg2, ...}) = (used arg1; argUses arg2)
            |   instructionUses(X87FPGetCondition _) = ()
            |   instructionUses(X87FPArith{arg1, arg2, ...}) = (used arg1; argUses arg2)
            |   instructionUses(X87FPUnaryOps{source, ...}) = used source
            |   instructionUses(X87Float{source, ...}) = argUses source
            |   instructionUses(SSE2Float{source, ...}) = argUses source
            |   instructionUses(SSE2FPUnary{source, ...}) = argUses source
            |   instructionUses(SSE2FPBinary{arg1, arg2, ...}) = (used arg1; argUses arg2)
            |   instructionUses(TagFloat{source, dest, ...}) = if isUsed dest then used source else ()
            |   instructionUses(UntagFloat{dest, cache=SOME cacheR, ...}) = if isUsed dest then used cacheR else ()
            |   instructionUses(UntagFloat{source, dest, cache=NONE, ...}) = if isUsed dest then argUses source else ()
            |   instructionUses(GetSSE2ControlReg _) = ()
            |   instructionUses(SetSSE2ControlReg{source}) = used source
            |   instructionUses(GetX87ControlReg _) = ()
            |   instructionUses(SetX87ControlReg{source}) = used source
            |   instructionUses(X87RealToInt{source, ...}) = used source
            |   instructionUses(SSE2RealToInt{source, ...}) = argUses source
            |   instructionUses(SignExtend32To64{source, dest}) = if isUsed dest then argUses source else ()
            |   instructionUses(TouchArgument{source}) = used source
            |   instructionUses(LongArbComparison{arg1, arg2, ...}) = (used arg1; argUses arg2)
            |   instructionUses(LongArbArithmetic{operand1, operand2, ...}) = (used operand1; argUses operand2)

            (* Depth-first scan. *)
            val visited = Array.array(numBlocks, false)

            fun processBlocks blockNo =
            if Array.sub(visited, blockNo)
            then ()  (* Done or currently being done. *)
            else
            let
                val () = Array.update(visited, blockNo, true)
                val ExtendedBasicBlock { flow, block,...} = Vector.sub(blocks, blockNo)
                val () =
                    (* Process the dependencies first. *)
                    case flow of
                        ExitCode => ()
                    |   Unconditional m => processBlocks m
                    |   Conditional {trueJump, falseJump, ...} =>
                            (processBlocks trueJump; processBlocks falseJump)
                    |   IndexedBr cases => List.app processBlocks cases
                    |   SetHandler{ handler, continue } =>
                            (processBlocks handler; processBlocks continue)
                    |   UnconditionalHandle _ => ()
                    |   ConditionalHandle { continue, ...} => processBlocks continue
                (* Now this block. *)
            in
                List.foldr(fn ({instr, ...}, ()) => instructionUses instr) () block
            end

        in
            val () = processBlocks 0
            val isUsed = isUsed
        end
        
        (* Return the register part of a cached item. *)
        fun decache(StackLocation{cache=SOME r, ...}) = RegisterArgument r
        |   decache(MemoryLocation{cache=SOME r, ...}) = RegisterArgument r
        |   decache arg = arg
        
        (* Only get the registers that are actually used. *)
        val getSaveRegs = List.mapPartial(fn (reg as PReg r) => if isUsed reg then SOME(getAllocatedGenReg r) else NONE)
                
        fun codeExtended _ ({instr=LoadArgument{source, dest as PReg dreg, kind}, ...}, code) =
            if not (isUsed dest)
            then code
            else
            let
                val realDestReg = getAllocatedReg dreg
            in
                case source of
                    RegisterArgument(PReg sreg) =>
                    (* Register to register move.  Try to use the same register for the source as the destination
                       to eliminate the instruction. *)
                        (* If the source is the same as the destination we don't need to do anything. *)
                        moveIfNecessary({src=getAllocatedReg sreg, dst=realDestReg, kind=kind}, code)

                |   MemoryLocation{cache=SOME(PReg sreg), ...} =>
                    (* This is also a register to register move but because the original load is from
                       memory it could be a byte or short precision value. *)
                    let
                        val moveKind =
                            case kind of
                                Move64Bit => Move64Bit
                            |   MoveByte => Move32Bit
                            |   Move16Bit => Move32Bit
                            |   Move32Bit => Move32Bit
                            |   MoveFloat => MoveFloat
                            |   MoveDouble => MoveDouble
                    in
                        moveIfNecessary({src=getAllocatedReg sreg, dst=realDestReg, kind=moveKind}, code)
                    end
                
                    (* TODO: Isn't this covered by codeExtArgument?  It looks like it was added in the
                       32-in-64 changes.  *)
                |   StackLocation{cache=SOME(PReg sreg), ...} =>
                        moveIfNecessary({src=getAllocatedReg sreg, dst=realDestReg, kind=kind}, code)

                |   source as StackLocation _ => (* Always use native loads from the stack. *)
                        llLoadArgument({source=codeExtArgument source, dest=realDestReg, kind=moveNativeWord}, code)

                |   source => (* Loads of constants or from an address. *)
                        llLoadArgument({source=codeExtArgument source, dest=realDestReg, kind=kind}, code)
            end

        |   codeExtended _ ({instr=StoreArgument{ source, base=PReg bReg, offset, index, kind, ... }, ...}, code) =
            let
                val (baseReg, indexVal) =
                    case index of
                        ObjectIndex => (ebx, Index4(getAllocatedGenReg bReg))
                    |   _ => (getAllocatedGenReg bReg, codeExtIndex index)
            in
                case (decache source, kind) of
                    (RegisterArgument(PReg sReg), MoveByte) =>
                    if targetArch <> Native32Bit
                    then
                        llStoreArgument{
                            source=codeExtArgument source, base=baseReg, offset=offset, index=indexVal, kind=MoveByte} :: code
                    else
                    (* This is complicated on X86/32.  We can't use edi or esi for the store registers.  Instead
                       we reserve ecx (see special case in "identify") and use that if we have to. *)
                    let
                        val realStoreReg = getAllocatedReg sReg
                        val (moveCode, storeReg) =
                            if realStoreReg = GenReg edi orelse realStoreReg = GenReg esi
                            then (moveIfNecessary({src=realStoreReg, dst=GenReg ecx, kind=moveNativeWord}, code), GenReg ecx)
                            else (code, realStoreReg)
                    in
                        llStoreArgument{
                            source=RegisterArg storeReg, base=baseReg, offset=offset, index=indexVal, kind=MoveByte} ::
                                moveCode
                    end
                    
                |   _ =>
                        llStoreArgument{
                            source=codeExtArgument source, base=baseReg, offset=offset, index=indexVal, kind=kind} :: code
            end

        |   codeExtended _ ({instr=LoadMemReg { offset, dest=PReg pr}, ...}, code) =
            (* Load from the "memory registers" pointed at by ebp. *)
            (* Currently only used to load the thread Id which is a Poly word. *)
                llLoadArgument({source=MemoryArg{base=ebp, offset=offset, index=NoIndex}, dest=getAllocatedReg pr, kind=movePolyWord}, code)

        |   codeExtended _ ({instr=BeginFunction{regArgs, ...}, ...}, code) =
            let
                val minStackCheck = 20
                val saveRegs = List.mapPartial(fn (_, GenReg r) => SOME r | _ => NONE) regArgs
                val preludeCode =
                    if stackRequired >= minStackCheck
                    then
                    let
                        (* Compute the necessary amount in edi and compare that. *)
                        val stackByteAdjust = ~ (Word.toInt nativeWordSize) * stackRequired
                        val testEdiCode =
                            testRegAndTrap (edi, StackOverflowCallEx, saveRegs)
                    in
                        (* N.B. In reverse order. *)
                        testEdiCode @ [LoadAddress{output=edi, base=SOME esp, index=NoIndex, offset=stackByteAdjust, opSize=nativeWordOpSize}]
                    end
     
                    else testRegAndTrap (esp, StackOverflowCall, saveRegs)

                val usedRegs = List.filter (isUsed o #1) regArgs
                fun mkPair(PReg pr, rr) = {src=rr,dst=getAllocatedReg pr}
                val regPairs = List.map mkPair usedRegs
            in
                moveMultipleRegisters(regPairs, preludeCode @ code)
            end

        |   codeExtended _ ({instr=TailRecursiveCall{callKind, regArgs=oRegArgs, stackArgs=oStackArgs, stackAdjust, currStackSize, workReg=PReg wReg}, ...}, code) =
            let
                val regArgs = List.map (fn (arg, reg) => (decache arg, reg)) oRegArgs
                and stackArgs = List.map(fn {src, stack } => {src=decache src, stack=stack}) oStackArgs
                val workReg = getAllocatedReg wReg
                
                (* We must leave stack entries as stack entries for the moment. *)
                fun codeArg(StackLocation{wordOffset, cache=NONE, ...}) = StackSource wordOffset
                |   codeArg arg = OtherSource(codeExtArgument arg)

                val extStackArgs = map (fn {stack, src} => {dst=StackDest(stack+currStackSize), src=codeArg src}) stackArgs
                val extRegArgs = map (fn (a, r) => {src=codeArg a, dst=RegDest r}) regArgs

                (* Tail recursive calls are complicated because we generally have to overwrite the existing stack.
                   That means storing the arguments in the right order to avoid overwriting a
                   value that we are using for a different argument. *)
                fun codeTailCall(arguments: {dst: destinations, src: llsource} list, stackAdjust, code) =
                if stackAdjust < 0
                then
                let
                    (* If the function we're calling takes more arguments on the stack than the
                       current function we will have to extend the stack.  Do that by pushing the
                       argument whose offset is at -1.  Then adjust all the offsets and repeat. *)
                    val {src=argM1, ...} = valOf(List.find(fn {dst=StackDest ~1, ...} => true | _ => false) arguments)
                    fun renumberArgs [] = []
                    |   renumberArgs ({dst=StackDest ~1, ...} :: args) = renumberArgs args (* Remove the one we've done. *)
                    |   renumberArgs ({dst, src} :: args) =
                        let
                            val newDest = case dst of StackDest d => StackDest(d+1) | regDest => regDest
                            val newSrc =
                                case src of
                                    StackSource wordOffset => StackSource(wordOffset+1)
                                |   other => other
                        in
                            {dst=newDest, src=newSrc} :: renumberArgs args
                        end
                in
                    codeTailCall(renumberArgs arguments, stackAdjust+1,
                        PushToStack(sourceAsGenRegOrMem(sourceToX86Code argM1)) :: code)
                end
                else
                let
                    val loadArgs = moveMultipleValues(arguments, SOME workReg, code)
                in
                    if stackAdjust = 0
                    then loadArgs
                    else ResetStack{numWords=stackAdjust, preserveCC=false} :: loadArgs
                end
            in
                JumpAddress(codeCallKind callKind) ::
                    codeTailCall(extStackArgs @ extRegArgs, stackAdjust+currStackSize, code)
            end

        |   codeExtended _ ({instr=FunctionCall{callKind, regArgs=oRegArgs, stackArgs=oStackArgs, dest=PReg dReg, realDest, saveRegs}, ...}, code) =
            let
                val regArgs = List.map (fn (arg, reg) => (decache arg, reg)) oRegArgs
                and stackArgs = List.map decache oStackArgs
                
                val destReg = getAllocatedReg dReg
                
                
                fun pushStackArgs ([], _, code) = code
                
                |   pushStackArgs (ContainerAddr {stackOffset, ...} ::args, argNum, code) =
                    let
                        val adjustedAddr = stackOffset+argNum
                        (* If there is an offset relative to rsp we need to add this in. *)
                        val addOffset =
                            if adjustedAddr = 0
                            then []
                            else [ArithMemConst{opc=ADD, address={offset=0, base=esp, index=NoIndex},
                                        source=LargeInt.fromInt(adjustedAddr*Word.toInt nativeWordSize), opSize=nativeWordOpSize}]
                    in
                        pushStackArgs(args, argNum+1, addOffset @ PushToStack(RegisterArg esp) :: code)
                    end
                    
                |   pushStackArgs (StackLocation {wordOffset, container, field, ...} ::args, argNum, code) =
                    let
                        (* Have to adjust the offsets of stack arguments. *)
                        val adjusted =
                            StackLocation{wordOffset=wordOffset+argNum, container=container, field=field+argNum,
                                                  cache=NONE}
                    in
                        pushStackArgs(args, argNum+1, PushToStack(codeExtArgumentAsGenReg adjusted) :: code)
                    end

                |   pushStackArgs (arg::args, argNum, code) =
                        pushStackArgs(args, argNum+1, PushToStack(codeExtArgumentAsGenReg arg) :: code)

                val pushedArgs = pushStackArgs(stackArgs, 0, code (* Initial code *))
                (* We have to adjust any stack offset to account for the arguments we've pushed. *)
                val numStackArgs = List.length stackArgs
                
                (* We don't currently allow the arguments to be memory locations and instead
                   force them into registers.  That may be simpler especially if we can get the
                   values directly into the required register. *)
                fun getRegArgs(RegisterArgument(PReg pr), reg) =
                        SOME{dst=reg, src=getAllocatedReg pr}
                |   getRegArgs(StackLocation {cache=SOME(PReg pr), ...}, reg) =
                        SOME{dst=reg, src=getAllocatedReg pr}
                |   getRegArgs(MemoryLocation _, _) = raise InternalError "FunctionCall - MemoryLocation"
                |   getRegArgs _ = NONE
                
                val loadRegArgs =
                    moveMultipleRegisters(List.mapPartial getRegArgs regArgs, pushedArgs)

                (* These are all items we can load without requiring a source register.
                   That includes loading from the stack. *)
                fun getConstArgs((AddressConstant m, reg), code) =
                        llLoadArgument({source=AddressConstArg m, dest=reg, kind=movePolyWord}, code)
                |   getConstArgs((IntegerConstant i, reg), code) =
                        llLoadArgument({source=NonAddressConstArg i, dest=reg, kind=movePolyWord}, code)
                |   getConstArgs((StackLocation { cache=SOME _, ...}, _), code) = code
                |   getConstArgs((StackLocation { wordOffset, ...}, reg), code) =
                        llLoadArgument({source=MemoryArg{offset=(wordOffset+numStackArgs)*Word.toInt nativeWordSize, base=esp, index=NoIndex},
                                          dest=reg, kind=moveNativeWord}, code)
                |   getConstArgs((ContainerAddr {stackOffset, ...}, reg), code) =
                        if stackOffset+numStackArgs = 0
                        then llLoadArgument({source=RegisterArg(GenReg esp), dest=reg, kind=moveNativeWord}, code)
                        else LoadAddress{ output=asGenReg reg, offset=(stackOffset+numStackArgs)*Word.toInt nativeWordSize, base=SOME esp,
                                          index=NoIndex, opSize=nativeWordOpSize } :: code
                |   getConstArgs((RegisterArgument _, _), code) = code
                |   getConstArgs((MemoryLocation _, _), code) = code
                val loadConstArgs = List.foldl getConstArgs loadRegArgs regArgs
                
                (* Push the registers before the call and pop them afterwards. *)
                fun makeSaves([], code) = CallAddress(codeCallKind callKind) :: code
                |   makeSaves(PReg reg::regs, code) =
                    let
                        val areg = getAllocatedGenReg reg
                        val _ = areg = eax andalso raise InternalError "codeExtended: eax in save regs"
                        val _ = if List.exists(fn (_, r) => r = GenReg areg) regArgs then raise InternalError "codeExtended: arg reg in save regs" else ()
                    in
                        PopR areg :: makeSaves(regs, PushToStack(RegisterArg areg) :: code)
                    end

            in
                moveIfNecessary({dst=destReg, src=realDest, kind=case realDest of GenReg _ => moveNativeWord | _ => MoveDouble},
                    makeSaves(saveRegs, loadConstArgs)) 
            end

        |   codeExtended _ ({instr=AllocateMemoryOperation{ size, flags, dest=PReg dReg, saveRegs}, ...}, code) =
            let
                val preserve = getSaveRegs saveRegs
            in
                llAllocateMemoryOperation({ size=size, flags=flags, dest=getAllocatedReg dReg, saveRegs=preserve}, code)
            end

        |   codeExtended _ ({instr=AllocateMemoryVariable{size=PReg size, dest=PReg dest, saveRegs}, ...}, code) =
            let
                (* Simple case - no initialiser. *)
                val saveRegs = getSaveRegs saveRegs
                val sReg = getAllocatedGenReg size and dReg = getAllocatedGenReg dest
                val _ = sReg <> dReg
                            orelse raise InternalError "codeGenICode-AllocateMemoryVariable"

                val allocCode =
                [
                    (* Store it as the length field. *)
                    Move{source=RegisterArg sReg, moveSize=opSizeToMove polyWordOpSize,
                         destination=MemoryArg {base=dReg, offset= ~ (Word.toInt wordSize), index=NoIndex}},
                    (* Untag the length *)
                    ShiftConstant{ shiftType=SHR, output=sReg, shift=0w1, opSize=polyWordOpSize},
                    (* Allocate the memory *)
                    AllocStoreVariable{ size=sReg, output=dReg, saveRegs=saveRegs}
                ]
                (* Convert to an object index if necessary. *)
                val convertToObjId =
                    if targetArch = ObjectId32Bit
                    then [ ShiftConstant{ shiftType=SHR, output=dReg, shift=0w2, opSize=OpSize64 },
                           ArithToGenReg{ opc=SUB, output=dReg, source=RegisterArg ebx, opSize=nativeWordOpSize } ]
                    else []
            in
                convertToObjId @ allocCode @ code
            end

        |   codeExtended _ ({instr=InitialiseMem{size=PReg sReg, addr=PReg aReg, init=PReg iReg}, ...}, code) =
                (* We are going to use rep stosl/q to set the memory.
                   That requires the length to be in ecx, the initialiser to be in eax and
                   the destination to be edi. *)
                RepeatOperation (if polyWordOpSize = OpSize64 then STOS64 else STOS32)::
                    moveIfNecessary({src=getAllocatedReg iReg, dst=GenReg eax, kind=moveNativeWord},
                        moveIfNecessary({src=getAllocatedReg aReg, dst=GenReg edi, kind=moveNativeWord},
                            moveIfNecessary({src=getAllocatedReg sReg, dst=GenReg ecx, kind=moveNativeWord}, code)))

        |   codeExtended _ ({instr=InitialisationComplete, ...}, code) = StoreInitialised :: code

        |   codeExtended _ ({instr=BeginLoop, ...}, code) = code

        |   codeExtended _ ({instr=JumpLoop{regArgs, stackArgs, checkInterrupt, workReg}, ...}, code) =
            let
                val workReg = Option.map (fn PReg r => getAllocatedReg r) workReg
                (* TODO: Make the sources and destinations "friends". *)
                (* We must leave stack entries as stack entries for the moment as with TailCall. *)
                fun codeArg(StackLocation{wordOffset, ...}) = StackSource wordOffset
                |   codeArg arg = OtherSource(codeExtArgument arg)
                val extStackArgs = map (fn (src, stack, _) => {dst=StackDest stack, src=codeArg src}) stackArgs
                val extRegArgs = map (fn (a, PReg r) => {src=codeArg a, dst=RegDest(getAllocatedReg r)}) regArgs
                val checkCode =
                    case checkInterrupt of
                        NONE => []
                    |   SOME saveRegs => testRegAndTrap (esp, StackOverflowCall, getSaveRegs saveRegs)
            in
                checkCode @ moveMultipleValues(extStackArgs @ extRegArgs, workReg, code)
            end

        |   codeExtended _ ({instr=RaiseExceptionPacket{ packetReg=PReg preg }, ...}, code) =
            let
                (* The argument must be put into rax. *)
                val _ = getAllocatedGenReg preg = eax orelse raise InternalError "codeExtended: RaiseExceptionPacket"
            in
                (* We need a work register here.  It can be any register other than eax since
                   we don't preserve registers across calls. *)
                RaiseException { workReg=ecx } :: code
            end

        |   codeExtended _ ({instr=ReserveContainer{size, ...}, ...}, code) =
                (* The memory must be cleared in case we have a GC. *)
                List.tabulate(size, fn _ => PushToStack(NonAddressConstArg(tag 0))) @ code

        |   codeExtended {flow} ({instr=IndexedCaseOperation{testReg=PReg tReg, workReg=PReg wReg}, ...}, code) =
            let
                val testReg = getAllocatedReg tReg
                val workReg = getAllocatedReg wReg
                val _ = testReg <> workReg orelse raise InternalError "IndexedCaseOperation - same registers"
                val rReg = asGenReg testReg and wReg = asGenReg workReg
                val _ = rReg <> wReg orelse raise InternalError "IndexedCaseOperation - same registers"
                (* This should only be within a block with an IndexedBr flow type. *)
                val cases =
                    case flow of IndexedBr cases => cases | _ => raise InternalError "codeGenICode: IndexedCaseOperation"
                val caseLabels = map getBlockLabel cases
                val startJumpTable = makeLabel()
                (* Compute the jump address.  The index is a tagged
                   integer so it is already multiplied by 2.  We need to
                   multiply by four to get the correct size. Subtract off the
                   shifted tag. *)
                val jumpSize = ref JumpSize8
            in
                JumpTable{cases=caseLabels, jumpSize=jumpSize} :: JumpLabel startJumpTable :: JumpAddress(RegisterArg wReg) ::
                    IndexedJumpCalc{ addrReg=wReg, indexReg=rReg, jumpSize=jumpSize } ::
                    LoadLabelAddress{label=startJumpTable, output=wReg} :: code
            end

        |   codeExtended _ ({instr=LockMutable{addr=PReg pr}, ...}, code) =
            let
                val (bReg, index) =
                    if targetArch = ObjectId32Bit
                    then (ebx, Index4(asGenReg(getAllocatedReg pr)))
                    else (asGenReg(getAllocatedReg pr), NoIndex)
            in
                (* Mask off the mutable bit. *)
                ArithByteMemConst{opc=AND, address={base=bReg, offset= ~1, index=index}, source=0wxff - F_mutable} :: code
            end

        |   codeExtended _ ({instr=WordComparison{ arg1=PReg pr, arg2, opSize, ... }, ...}, code) =
                ArithToGenReg {opc=CMP, output=asGenReg(getAllocatedReg pr), source=codeExtArgumentAsGenReg arg2, opSize=opSize} :: code

        |   codeExtended _ ({instr=CompareLiteral{ arg1, arg2, opSize, ... }, ...}, code) =
            (
                case decache arg1 of (* N.B. We MUST decache since we're assuming that the base reg is not used. *)
                    RegisterArgument(PReg pr) =>
                        ArithToGenReg {opc=CMP, output=asGenReg(getAllocatedReg pr), source=NonAddressConstArg arg2, opSize=opSize} :: code
                |   MemoryLocation{base=PReg br, offset, index=ObjectIndex, ...} =>
                        ArithMemConst{ opc=CMP,
                            address={offset=offset, base=ebx, index=Index4(asGenReg(getAllocatedReg br))}, source=arg2, opSize=opSize } :: code
                |   MemoryLocation{base=PReg br, index, offset, ...} =>
                        ArithMemConst{ opc=CMP,
                            address={offset=offset, base=asGenReg(getAllocatedReg br), index=codeExtIndex index}, source=arg2, opSize=opSize } :: code
                |   StackLocation{wordOffset, ...} =>
                        ArithMemConst{ opc=CMP, address={offset=wordOffset*Word.toInt nativeWordSize, base=esp, index=NoIndex}, source=arg2, opSize=opSize } :: code
                |   _ => raise InternalError "CompareLiteral"
            )

        |   codeExtended _ ({instr=CompareByteMem{ arg1={base=PReg br, offset, index}, arg2, ... }, ...}, code) =
            let
                val (bReg, index) =
                    case index of
                        ObjectIndex => (ebx, Index4(asGenReg(getAllocatedReg br)))
                    |   _ => (asGenReg(getAllocatedReg br), codeExtIndex index)
            in
                ArithByteMemConst{ opc=CMP, address={offset=offset, base=bReg, index=index}, source=arg2 } :: code
            end

            (* Set up an exception handler. *)
        |   codeExtended {flow} ({instr=PushExceptionHandler{workReg=PReg hReg}, ...}, code) =
            let (* Set up an exception handler. *)
                val workReg=getAllocatedReg hReg
                (* Although we're pushing this to the stack we need to use LEA on the
                   X86/64 and some arithmetic on the X86/32.  We need a work reg for that. *)
                val handleReg = asGenReg workReg
                (* This should only be within a block with a SetHandler flow type. *)
                val handleLabel =
                    case flow of
                        SetHandler{ handler, ...} => handler
                    |   _ => raise InternalError "codeGenICode: PushExceptionHandler"
                val labelRef = getBlockLabel handleLabel
                (* Set up the handler by pushing the old handler to the stack, pushing the
                   entry point and setting the handler address to the current stack pointer. *)
            in
                (
                    Move{source=RegisterArg esp, destination=MemoryArg {offset=memRegHandlerRegister, base=ebp, index=NoIndex},
                         moveSize=opSizeToMove nativeWordOpSize} ::
                    PushToStack(RegisterArg handleReg) ::
                    LoadLabelAddress{ label=labelRef, output=handleReg} ::
                    PushToStack(MemoryArg{base=ebp, offset=memRegHandlerRegister, index=NoIndex}) :: code)
            end

            (* Pop an exception handler at the end of a handled section.  Executed if no exception has been raised.
               This removes items from the stack. *)
        |   codeExtended _ ({instr=PopExceptionHandler{workReg=PReg wReg, ...}, ...}, code) =
            let
                val workReg = getAllocatedReg wReg
                val wReg = asGenReg workReg
            in
                (* The stack pointer has been adjusted to just above the two words that were stored
                   in PushExceptionHandler. *)
                (
                    Move{source=RegisterArg wReg, destination=MemoryArg {offset=memRegHandlerRegister, base=ebp, index=NoIndex},
                         moveSize=opSizeToMove nativeWordOpSize} ::
                    PopR wReg ::
                    ResetStack{numWords=1, preserveCC=false} :: code)
            end

            (* Start of a handler.  Sets the address associated with PushExceptionHandler and
               provides a register for the packet.*) 
        |   codeExtended _ ({instr=BeginHandler{packetReg=PReg pReg, workReg=PReg wReg}, ...}, code) =
            let
                (* The exception packet is in rax. *)
                val realPktReg = getAllocatedReg pReg
                val realWorkreg = getAllocatedGenReg wReg
                (* The code here is almost the same as PopExceptionHandler.  The only real difference
                   is that PopExceptionHandler needs to pass the result of executing the handled code
                   which could be in any register.  This code needs to transmit the exception packet
                   and that is always in rax. *)
                val beginHandleCode =
                    Move{source=RegisterArg realWorkreg, destination=MemoryArg {offset=memRegHandlerRegister, base=ebp, index=NoIndex},
                         moveSize=opSizeToMove nativeWordOpSize} ::
                    PopR realWorkreg :: ResetStack{numWords=1, preserveCC=false} ::
                    Move{ source=MemoryArg{base=ebp, offset=memRegHandlerRegister, index=NoIndex},
                          destination=RegisterArg esp, moveSize=opSizeToMove nativeWordOpSize } :: code
            in
                moveIfNecessary({src=GenReg eax, dst=realPktReg, kind=moveNativeWord }, beginHandleCode)
            end

        |   codeExtended _ ({instr=ReturnResultFromFunction { resultReg=PReg resReg, realReg, numStackArgs }, ...}, code) =
            let
                val resultReg = getAllocatedReg resReg
                (* If for some reason it's not in the right register we have to move it there. *)
            in
                ReturnFromFunction numStackArgs :: moveIfNecessary({src=resultReg, dst=realReg, kind=moveNativeWord}, code)
            end

        |   codeExtended _ ({instr=ArithmeticFunction{oper=SUB, resultReg=PReg resReg, operand1=PReg op1Reg,
                                            operand2, opSize, ...}, ...}, code) =
            (* Subtraction - this is special because it can only be done one way round.  The first argument must
               be in a register. *)
            let
                val realDestReg = getAllocatedReg resReg
                val realOp1Reg = getAllocatedReg op1Reg
            in
                ArithToGenReg { opc=SUB, output=asGenReg realDestReg, source=codeExtArgumentAsGenReg operand2, opSize=opSize } ::
                    moveIfNecessary({src=realOp1Reg, dst=realDestReg, kind=opSizeToIMove opSize}, code)
            end

        |   codeExtended _ ({instr=ArithmeticFunction{oper, resultReg=PReg resReg, operand1=PReg op1Reg, operand2, opSize, ...}, ...}, code) =
            (
                case decache operand2 of
                    RegisterArgument(PReg op2Reg) =>
                    (* Arithmetic operation with both arguments as registers.  These operations are all symmetric so
                       we can try to put either argument into the result reg and then do the operation on the other arg. *)
                    let
                        val realDestReg = getAllocatedGenReg resReg
                        val realOp1Reg = getAllocatedGenReg op1Reg
                        and realOp2Reg = getAllocatedGenReg op2Reg
                        val (operandReg, moveInstr) =
                            if realOp1Reg = realDestReg
                            then (realOp2Reg, code)
                            else if realOp2Reg = realDestReg
                            then (realOp1Reg, code)
                            else (realOp2Reg, Move{source=RegisterArg realOp1Reg, destination=RegisterArg realDestReg, moveSize=opSizeToMove opSize} :: code)
                    in
                        ArithToGenReg { opc=oper, output=realDestReg, source=RegisterArg operandReg, opSize=opSize } :: moveInstr
                    end

                |   operand2 =>
                    (* Arithmetic operation with the first argument in a register and the second a constant or memory location. *)
                    let
                        val realDestReg = getAllocatedReg resReg
                        val realOp1Reg = getAllocatedReg op1Reg
                        val op2Arg = codeExtArgumentAsGenReg operand2
                        (* If we couldn't put it in the result register we have to copy it there. *)
                    in
                        ArithToGenReg { opc=oper, output=asGenReg realDestReg, source=op2Arg, opSize=opSize } ::
                            moveIfNecessary({src=realOp1Reg, dst=realDestReg, kind=opSizeToIMove opSize}, code)
                    end
            )

        |   codeExtended _ ({instr=TestTagBit{arg, ...}, ...}, code) =
                TestByteBits{arg=codeExtArgumentAsGenReg arg, bits=0w1} :: code

        |   codeExtended _ ({instr=PushValue {arg, ...}, ...}, code) = PushToStack(codeExtArgumentAsGenReg arg) :: code

        |   codeExtended _ ({instr=CopyToCache{source=PReg sreg, dest as PReg dreg, kind}, ...}, code) =
            if not (isUsed dest)
            then code
            else
            let
                val realDestReg = getAllocatedReg dreg
                (* Get the source register using the current destination as a preference. *)
                val realSrcReg = getAllocatedReg sreg
            in
                (* If the source is the same as the destination we don't need to do anything. *)
                moveIfNecessary({src=realSrcReg, dst=realDestReg, kind=kind}, code)
            end

        |   codeExtended _ ({instr=ResetStackPtr {numWords, preserveCC}, ...}, code) =
            (
                numWords >= 0 orelse raise InternalError "codeGenICode: ResetStackPtr - negative offset";
                ResetStack{numWords=numWords, preserveCC=preserveCC} :: code
            )

        |   codeExtended _ ({instr=StoreToStack{ source, stackOffset, ... }, ...}, code) =
                llStoreArgument{
                    source=codeExtArgument source, base=esp, offset=stackOffset*Word.toInt nativeWordSize, index=NoIndex, kind=moveNativeWord} :: code

        |   codeExtended _ ({instr=TagValue{source=PReg srcReg, dest as PReg dReg, opSize, ...}, ...}, code) =
            if not (isUsed dest)
            then code
            else
            let
                val regResult = asGenReg(getAllocatedReg dReg)
                val realSReg = asGenReg(getAllocatedReg srcReg)
            in
                (* N.B. Using LEA with a base register and an index multiplier of 1 is shorter than
                   using no base register and a multiplier of two. *)
                (* TODO: If the value we're tagging is a byte or a 16-bit value we can use OpSize32 and possibly
                   save the Rex byte. *)
                LoadAddress{ output=regResult, offset=1, base=SOME realSReg, index=Index1 realSReg, opSize=opSize } :: code
            end

        |   codeExtended _ ({instr=UntagValue{dest as PReg dReg, cache=SOME(PReg cacheReg), opSize, ...}, ...}, code) =
            if not (isUsed dest)
            then code
            else moveIfNecessary({src=getAllocatedReg cacheReg, dst=getAllocatedReg dReg, kind=opSizeToIMove opSize}, code)

        |   codeExtended _ ({instr=UntagValue{source=PReg sReg, dest as PReg dReg, isSigned, opSize, ...}, ...}, code) =
            if not (isUsed dest)
            then code
            else
            let
                val regResult = getAllocatedReg dReg
                val realSReg = getAllocatedReg sReg
            in
                (* For most cases we're going to be using a 32-bit word if this is 32-in-64.  The exception
                   is when converting a word to a signed large-word.  *)
                ShiftConstant{ shiftType=if isSigned then SAR else SHR, output=asGenReg regResult, shift=0w1, opSize=opSize } ::
                    moveIfNecessary({src=realSReg, dst=regResult, kind=opSizeToIMove opSize}, code)
            end

        |   codeExtended _ ({instr=LoadEffectiveAddress{base, offset, index=ObjectIndex, dest=PReg dReg, opSize}, ...}, code) =
            let
                val destReg = asGenReg(getAllocatedReg dReg)
                val bReg =
                    case base of
                        SOME(PReg br) => asGenReg(getAllocatedReg br)
                    |   NONE => raise InternalError "LoadEffectiveAddress - ObjectIndex but no base"
            in
                LoadAddress{ output=destReg, offset=offset, base=SOME ebx, index=Index4 bReg, opSize=opSize } :: code
           end

        |   codeExtended _ ({instr=LoadEffectiveAddress{base, offset, index, dest=PReg dReg, opSize}, ...}, code) =
            let
                val destReg = asGenReg(getAllocatedReg dReg)
                val bReg =
                    case base of
                        SOME(PReg br) => SOME(asGenReg(getAllocatedReg br))
                    |   NONE => NONE
                val indexR = codeExtIndex index
            in
                LoadAddress{ output=destReg, offset=offset, base=bReg, index=indexR, opSize=opSize } :: code
            end

        |   codeExtended _ ({instr=ShiftOperation{shift, resultReg=PReg resReg, operand=PReg operReg, shiftAmount=IntegerConstant i, opSize, ...}, ...}, code) =
            let
                val realDestReg = getAllocatedReg resReg
                val realOpReg = getAllocatedReg operReg
            in
                ShiftConstant{ shiftType=shift, output=asGenReg realDestReg, shift=Word8.fromLargeInt i, opSize=opSize } ::
                    moveIfNecessary({src=realOpReg, dst=realDestReg, kind=opSizeToIMove opSize}, code)
            end

        |   codeExtended _ ({instr=ShiftOperation{shift, resultReg=PReg resReg, operand=PReg operReg,
                                        shiftAmount=RegisterArgument(PReg shiftReg), opSize, ...}, ...}, code) =
            let
                val realDestReg = getAllocatedReg resReg
                val realShiftReg = getAllocatedReg shiftReg
                val realOpReg = getAllocatedReg operReg
                (* We want the shift in ecx.  We may not have got it there but the register
                   should be free.  The shift is masked to 5 or 6 bits so we have to
                   check for larger shift values at a higher level.*)
            in
                ShiftVariable{ shiftType=shift, output=asGenReg realDestReg, opSize=opSize } ::
                    moveIfNecessary({src=realOpReg, dst=realDestReg, kind=opSizeToIMove opSize},
                        moveIfNecessary({src=realShiftReg, dst=GenReg ecx, kind=Move32Bit (* < 64*)}, code))
           end

        |   codeExtended _ ({instr=ShiftOperation _, ...}, _) = raise InternalError "codeExtended - ShiftOperation"

        |   codeExtended _ ({instr=
                Multiplication{resultReg=PReg resReg, operand1=PReg op1Reg,
                               operand2, opSize, ...}, ...}, code) =
            (
                case decache operand2 of
                    RegisterArgument(PReg op2Reg) =>
                    let
                        (* Treat exactly the same as ArithmeticFunction. *)
                        val realDestReg = getAllocatedGenReg resReg
                        val realOp1Reg = getAllocatedGenReg op1Reg
                        and realOp2Reg = getAllocatedGenReg op2Reg
                        val (operandReg, moveInstr) =
                            if realOp1Reg = realDestReg
                            then (realOp2Reg, code)
                            else if realOp2Reg = realDestReg
                            then (realOp1Reg, code)
                            else (realOp2Reg, Move{source=RegisterArg realOp1Reg, destination=RegisterArg realDestReg, moveSize=opSizeToMove opSize} :: code)
                    in
                        MultiplyR { source=RegisterArg operandReg, output=realDestReg, opSize=opSize } :: moveInstr
                    end
                |   operand2 =>
                    (* Multiply operation with the first argument in a register and the second a constant or memory location. *)
                    let
                        val realDestReg = getAllocatedReg resReg
                        val realOp1Reg = getAllocatedReg op1Reg
                        val op2Arg = codeExtArgumentAsGenReg operand2
                    in
                        MultiplyR { output=asGenReg realDestReg, source=op2Arg, opSize=opSize } ::
                            moveIfNecessary({src=realOp1Reg, dst=realDestReg, kind=opSizeToIMove opSize}, code)
                    end
            )

        |   codeExtended _ ({instr=Division{isSigned, dividend=PReg regDivid, divisor, quotient=PReg regQuot,
                                  remainder=PReg regRem, opSize}, ...}, code) =
            let
                (* TODO: This currently only supports the dividend in a register.  LargeWord division will
                   generally load the argument from a box so we could support a memory argument for that
                   case.  Word and integer values will always have to be detagged. *)
                (* Division is specific as to the registers.  The dividend must be eax, quotient is
                   eax and the remainder is edx. *)
                val realDiviReg = getAllocatedReg regDivid
                val realQuotReg = getAllocatedReg regQuot
                val realRemReg = getAllocatedReg regRem
                val divisorArg = codeExtArgument divisor
                val divisorReg = argAsGenReg divisorArg
                val _ = divisorReg <> eax andalso divisorReg <> edx orelse raise InternalError "codeGenICode: Division"
                (* rdx needs to be set to the high order part of the dividend.  For signed
                   division that means sign-extending rdx, for unsigned division we clear it.
                   We only need a 32-bit clear since the top 32-bits are cleared anyway. *)
                val setRDX =
                    if isSigned then SignExtendForDivide opSize
                    else ArithToGenReg{ opc=XOR, output=edx, source=RegisterArg edx, opSize=OpSize32 }
            in
                (* We may need to move one or more of the registers although normally that
                   won't be necessary.  Almost certainly only either the remainder or the
                   quotient will actually be used. *)
                moveMultipleRegisters([{src=GenReg eax, dst=realQuotReg}, {src=GenReg edx, dst=realRemReg}],
                    DivideAccR {arg=divisorReg, isSigned=isSigned, opSize=opSize} :: setRDX ::
                        moveIfNecessary({src=realDiviReg, dst=GenReg eax, kind=opSizeToIMove opSize}, code))
            end

        |   codeExtended _ ({instr=AtomicExchangeAndAdd{base=PReg bReg, source=PReg sReg}, ...}, code) =
            let
                val baseReg = asGenReg (getAllocatedReg bReg) and outReg = asGenReg (getAllocatedReg sReg)
                val address =
                    if targetArch = ObjectId32Bit
                    then {base=ebx, index=Index4 baseReg, offset=0}
                    else {base=baseReg, index=NoIndex, offset=0}
            in
                AtomicXAdd{address=address, output=outReg, opSize=polyWordOpSize} :: code
            end

        |   codeExtended _ ({instr=BoxValue{boxKind, source=PReg sReg, dest as PReg dReg, saveRegs}, ...}, code) =
            if not (isUsed dest)
            then code
            else
            let
                val preserve = getSaveRegs saveRegs
                val (srcReg, boxSize, moveKind) =
                    case boxKind of
                        BoxLargeWord => (getAllocatedReg sReg, Word.toInt(nativeWordSize div wordSize), moveNativeWord)
                    |   BoxX87Double => (getAllocatedReg sReg, Word.toInt(0w8 div wordSize), MoveDouble)
                    |   BoxX87Float => (getAllocatedReg sReg, Word.toInt(0w4 div wordSize), MoveFloat)
                    |   BoxSSE2Double => (getAllocatedReg sReg, Word.toInt(0w8 div wordSize), MoveDouble)
                    |   BoxSSE2Float => (getAllocatedReg sReg, Word.toInt(0w4 div wordSize), MoveFloat)
                val dstReg = getAllocatedReg dReg
                val (bReg, index) =
                    if targetArch = ObjectId32Bit
                    then (ebx, Index4(asGenReg dstReg))
                    else (asGenReg dstReg, NoIndex)
            in
                StoreInitialised ::
                    llStoreArgument{ source=RegisterArg srcReg, offset=0, base=bReg, index=index, kind=moveKind} ::
                        llAllocateMemoryOperation({ size=boxSize, flags=0wx1, dest=dstReg, saveRegs=preserve}, code)
            end

        |   codeExtended _ ({instr=CompareByteVectors{vec1Addr=PReg v1Reg, vec2Addr=PReg v2Reg, length=PReg lReg, ...}, ...}, code) =
                (* There's a complication here.  CompareByteVectors generates REPE CMPSB to compare
                   the vectors but the condition code is only set if CMPSB is executed at least
                   once.  If the value in RCX/ECX is zero it will never be executed and the
                   condition code will be unchanged.  We want the result to be "equal" in that
                   case so we need to ensure that is the case.  It's quite possible that the
                   condition code has just been set by shifting RCX/ECX to remove the tag in which
                   case it will have set "equal" if the value was zero.  We use CMP R/ECX,R/ECX which
                   is two bytes in 32-bit.
                   If we knew the length was non-zero (e.g. a constant) we could avoid this. *)
                RepeatOperation CMPS8 :: ArithToGenReg {opc=CMP, output=ecx, source=RegisterArg ecx, opSize=OpSize32} ::
                    moveIfNecessary({src=getAllocatedReg v1Reg, dst=GenReg esi, kind=moveNativeWord},
                        moveIfNecessary({src=getAllocatedReg v2Reg, dst=GenReg edi, kind=moveNativeWord},
                            moveIfNecessary({src=getAllocatedReg lReg, dst=GenReg ecx, kind=moveNativeWord}, code)))

        |   codeExtended _ ({instr=BlockMove{srcAddr=PReg sReg, destAddr=PReg dReg, length=PReg lReg, isByteMove}, ...}, code) =
                (* We may need to move these into the appropriate registers.  They have been reserved but it's
                   still possible the values could be in something else. *)
                RepeatOperation(if isByteMove then MOVS8 else if polyWordOpSize = OpSize64 then MOVS64 else MOVS32) ::
                    moveIfNecessary({src=getAllocatedReg sReg, dst=GenReg esi, kind=moveNativeWord},
                        moveIfNecessary({src=getAllocatedReg dReg, dst=GenReg edi, kind=moveNativeWord},
                            moveIfNecessary({src=getAllocatedReg lReg, dst=GenReg ecx, kind=moveNativeWord}, code)))

        |   codeExtended _ ({instr=X87Compare{arg1=PReg argReg, arg2, isDouble, ...}, ...}, code) =
            let
                val fpReg = getAllocatedFPReg argReg
                val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: CompareFloatingPt not fp0"
                (* This currently pops the value. *)
                val precision = if isDouble then DoublePrecision else SinglePrecision
            in
                case codeExtArgumentAsFPReg arg2 of
                    RegisterArg fpReg2 => FPArithR{opc=FCOMP, source=fpReg2} :: code
                |   MemoryArg{offset, base=baseReg, index=NoIndex} =>
                        FPArithMemory{opc=FCOMP, base=baseReg, offset=offset, precision=precision} :: code
                |   AddressConstArg const =>
                        FPArithConst{opc=FCOMP, source = const, precision=precision} :: code
                |   _ => raise InternalError "codeGenICode: CompareFloatingPt: TODO"
            end

        |   codeExtended _ ({instr=SSE2Compare{arg1=PReg argReg, arg2, isDouble, ...}, ...}, code) =
            let
                val xmmReg = getAllocatedXMMReg argReg
                val arg2Code = codeExtArgumentAsXMMReg arg2
            in
                XMMArith { opc= if isDouble then SSE2CompDouble else SSE2CompSingle, output=xmmReg, source=arg2Code} :: code
            end

        |   codeExtended _ ({instr=X87FPGetCondition{dest=PReg dReg, ...}, ...}, code) =
                moveIfNecessary({src=GenReg eax, dst=getAllocatedReg dReg, kind=Move32Bit},
                    FPStatusToEAX :: code)

        |   codeExtended _ ({instr=X87FPArith{opc, resultReg=PReg resReg, arg1=PReg op1Reg, arg2, isDouble}, ...}, code) =
            let
                val realDestReg = getAllocatedFPReg resReg
                val realOp1Reg = getAllocatedFPReg op1Reg
                val _ = realDestReg = fp0 orelse raise InternalError "codeGenICode: FloatingPointArith not fp0"
                val _ = realOp1Reg = fp0 orelse raise InternalError "codeGenICode: FloatingPointArith not fp0"
                val op2Arg = codeExtArgumentAsFPReg arg2
                val precision = if isDouble then DoublePrecision else SinglePrecision
            in
                case op2Arg of
                    MemoryArg{offset, base=baseReg, index=NoIndex} =>
                        FPArithMemory{opc=opc, base=baseReg, offset=offset, precision=precision} :: code
                |   AddressConstArg const =>
                        FPArithConst{opc=opc, source = const, precision=precision} :: code
                |   _ => raise InternalError "codeGenICode: X87FPArith: TODO"
            end
    
        |   codeExtended _ ({instr=X87FPUnaryOps{fpOp, dest=PReg resReg, source=PReg op1Reg}, ...}, code) =
            let
                val realDestReg = getAllocatedFPReg resReg
                val realOp1Reg = getAllocatedFPReg op1Reg
                val _ = realDestReg = fp0 orelse raise InternalError "codeGenICode: X87FPUnaryOps not fp0"
                val _ = realOp1Reg = fp0 orelse raise InternalError "codeGenICode: X87FPUnaryOps not fp0"
            in
                FPUnary fpOp :: code
            end

        |   codeExtended _ ({instr=X87Float{dest=PReg resReg, source}, ...}, code) =
            let
                val intSource = codeExtArgumentAsGenReg source
                val fpReg = getAllocatedFPReg resReg
                val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: FloatFixedInt not fp0"
            in
                (* This is complicated.  The integer value has to be in memory not in a
                   register so we have to push it to the stack and then make sure it is
                   popped afterwards.  Because it is untagged it is unsafe to leave it. *)
                ResetStack{numWords=1, preserveCC=false} :: FPLoadInt{ base=esp, offset=0, opSize=polyWordOpSize } :: PushToStack intSource :: code
            end

        |   codeExtended _ ({instr=SSE2Float{dest=PReg resReg, source}, ...}, code) =
            let
                val xmmResReg = getAllocatedXMMReg resReg
                val srcReg = case codeExtArgumentAsGenReg source of RegisterArg srcReg => srcReg | _ => raise InternalError "FloatFixedInt: not reg"
            in
                XMMConvertFromInt{ output=xmmResReg, source=srcReg, opSize=polyWordOpSize} :: code
            end

        |   codeExtended _ ({instr=SSE2FPUnary{opc, resultReg=PReg resReg, source}, ...}, code) =
            let
                val realDestReg = getAllocatedXMMReg resReg
                val opArg = codeExtArgumentAsXMMReg source
                val sse2Op =
                    case opc of
                        SSE2UDoubleToFloat  => SSE2DoubleToFloat
                    |   SSE2UFloatToDouble  => SSE2FloatToDouble
            in
                XMMArith{ opc=sse2Op, output=realDestReg, source=opArg} :: code
            end

        |   codeExtended _ ({instr=SSE2FPBinary{opc, resultReg=PReg resReg, arg1=PReg op1Reg, arg2}, ...}, code) =
            let
                val realDestReg = getAllocatedXMMReg resReg
                val realOp1Reg = getAllocatedXMMReg op1Reg
                val op2Arg = codeExtArgumentAsXMMReg arg2
                (* xorpd and andpd require 128-bit arguments with 128-bit alignment. *)
                val _ =
                    case (opc, op2Arg) of
                        (SSE2BXor, RegisterArg _) => ()
                    |   (SSE2BXor, _) => raise InternalError "codeGenICode - SSE2Xor not in register"
                    |   (SSE2BAnd, RegisterArg _) => ()
                    |   (SSE2BAnd, _) => raise InternalError "codeGenICode - SSE2And not in register"
                    |   _ => ()
                val doMove =
                    if realDestReg = realOp1Reg
                    then code
                    else XMMArith { opc=SSE2MoveDouble, source=RegisterArg realOp1Reg, output=realDestReg } :: code
                val sse2Op =
                    case opc of
                        SSE2BAddDouble  => SSE2AddDouble
                    |   SSE2BSubDouble  => SSE2SubDouble
                    |   SSE2BMulDouble  => SSE2MulDouble
                    |   SSE2BDivDouble  => SSE2DivDouble
                    |   SSE2BAddSingle  => SSE2AddSingle
                    |   SSE2BSubSingle  => SSE2SubSingle
                    |   SSE2BMulSingle  => SSE2MulSingle
                    |   SSE2BDivSingle  => SSE2DivSingle
                    |   SSE2BXor        => SSE2Xor
                    |   SSE2BAnd        => SSE2And
            in
                XMMArith{ opc=sse2Op, output=realDestReg, source=op2Arg} :: doMove
            end

        |   codeExtended _ ({instr=TagFloat{source=PReg srcReg, dest as PReg dReg, ...}, ...}, code) =
            if not (isUsed dest)
            then code
            else
            let
                val _ = targetArch = Native64Bit orelse raise InternalError "TagFloat: not 64-bit"
                (* Copy the value from an XMM reg into a general reg and tag it. *)
                val regResult = asGenReg(getAllocatedReg dReg)
                val realSReg = getAllocatedXMMReg srcReg
            in
                ArithToGenReg { opc=ADD, output=regResult, source=NonAddressConstArg 1, opSize=polyWordOpSize } ::
                ShiftConstant{ shiftType=SHL, output=regResult, shift=0w32, opSize=OpSize64} ::
                MoveXMMRegToGenReg { source = realSReg, output = regResult } :: code
            end

        |   codeExtended _ ({instr=UntagFloat{dest as PReg dReg, cache=SOME(PReg cacheReg), ...}, ...}, code) =
            if not (isUsed dest)
            then code
            else moveIfNecessary({src=getAllocatedReg cacheReg, dst=getAllocatedReg dReg, kind=MoveFloat}, code)

        |   codeExtended _ ({instr=UntagFloat{source, dest as PReg dReg, ...}, ...}, code) =
            if not (isUsed dest)
            then code
            else
            let
                val regResult = getAllocatedXMMReg dReg
            in
                case codeExtArgumentAsGenReg source of
                    RegisterArg realSReg =>
                        XMMShiftRight{ output=regResult, shift=0w4 (* Bytes - not bits *) } ::
                        MoveGenRegToXMMReg {source=realSReg, output=regResult} :: code
                |   MemoryArg{base, offset, index} =>
                        (* If the value is in memory we can just load the high order word. *)
                        XMMArith { opc=SSE2MoveFloat, source=MemoryArg{base=base, offset=offset+4, index=index}, output=regResult } :: code
                |   NonAddressConstArg ic =>
                        (* Shift down and then load from the non-constant area. *)
                        XMMArith { opc=SSE2MoveFloat, source=NonAddressConstArg(IntInf.~>>(ic, 0w32)), output=regResult } :: code
                |   _ => raise InternalError "UntagFloat - not register or memory"
            end

        |   codeExtended _ ({instr=GetSSE2ControlReg{dest=PReg dReg}, ...}, code) =
            let
                (* This has to work through memory.  Reserve one word on the stack, get the
                   MXCSR register into it and pop it to the register. *)
                val regResult = getAllocatedGenReg dReg
            in
                PopR regResult ::
                XMMStoreCSR{base=esp, offset=0, index=NoIndex } ::
                PushToStack(NonAddressConstArg 0) :: code
            end

        |   codeExtended _ ({instr=SetSSE2ControlReg{source=PReg sReg}, ...}, code) =
            let
                (* This has to work through memory.  Push the register to the stack,
                   store the value into the control register and remove it from the stack. *)
                val sourceReg = getAllocatedGenReg sReg
            in
                ResetStack{ numWords=1, preserveCC=false } ::
                XMMLoadCSR{base=esp, offset=0, index=NoIndex } ::
                PushToStack(RegisterArg sourceReg) :: code
            end

        |   codeExtended _ ({instr=GetX87ControlReg{dest=PReg dReg}, ...}, code) =
            let
                (* This has to work through memory.  Reserve one word on the stack, get the
                   X87 control register into it and pop it to the register. *)
                val regResult = getAllocatedGenReg dReg
            in
                PopR regResult ::
                FPStoreCtrlWord{base=esp, offset=0, index=NoIndex } ::
                PushToStack(NonAddressConstArg 0) :: code
            end

        |   codeExtended _ ({instr=SetX87ControlReg{source=PReg sReg}, ...}, code) =
            let
                (* This has to work through memory.  Push the register to the stack,
                   store the value into the control register and remove it from the stack. *)
                val sourceReg = getAllocatedGenReg sReg
            in
                ResetStack{ numWords=1, preserveCC=false } ::
                FPLoadCtrlWord{base=esp, offset=0, index=NoIndex } ::
                PushToStack(RegisterArg sourceReg) :: code
            end

        |   codeExtended _ ({instr=X87RealToInt{source=PReg sReg, dest=PReg dReg}, ...}, code) =
            let
                (* This has to work through memory.  Reserve one word on the stack,
                   convert the value into it and pop it to the register. *)
                val regResult = getAllocatedGenReg dReg
                val fpReg = getAllocatedFPReg sReg
                val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: CompareFloatingPt not fp0"
                (* This currently pops the value. *)
            in
                PopR regResult ::
                FPStoreInt{base=esp, offset=0, index=NoIndex } ::
                PushToStack(NonAddressConstArg 0) :: code
            end

        |   codeExtended _ ({instr=SSE2RealToInt{source, dest=PReg dReg, isDouble, isTruncate}, ...}, code) =
            let
                (* The source is either an XMM register or memory. *)
                val regResult = getAllocatedGenReg dReg
                val opArg = codeExtArgumentAsXMMReg source
            in
                XMMStoreInt {
                    source=opArg, precision=if isDouble then DoublePrecision else SinglePrecision,
                    output = regResult, isTruncate=isTruncate } :: code
            end

        |   codeExtended _ ({instr=SignExtend32To64{source, dest=PReg dReg}, ...}, code) =
            let
                val regResult = getAllocatedGenReg dReg
                val opArg = codeExtArgumentAsGenReg source
            in
                Move{moveSize=Move32X, source=opArg, destination=RegisterArg regResult } :: code
            end
        
        |   codeExtended _ ({instr=TouchArgument _, ...}, code) = code (* Don't need to do anything. *)

        |   codeExtended _ ({instr=LongArbComparison{ arg1=PReg pr, arg2, saveRegs, ... }, ...}, code) =
                ArithToGenReg {opc=CMP, output=asGenReg(getAllocatedReg pr), source=codeExtArgumentAsGenReg arg2, opSize=polyWordOpSize} ::
                    CallRTS{rtsEntry=ArbPrecisionEmulate, saveRegs=getSaveRegs saveRegs } ::code

        |   codeExtended _ ({instr=LongArbArithmetic{oper=SUB, resultReg=PReg resReg, operand1=PReg op1Reg,
                                            operand2, saveRegs, ...}, ...}, code) =
            (* Subtraction - this is special because it can only be done one way round.  The first argument must
               be in a register. *)
            let
                val realDestReg = getAllocatedReg resReg
                val realOp1Reg = getAllocatedReg op1Reg
            in
                ArithToGenReg { opc=SUB, output=asGenReg realDestReg, source=codeExtArgumentAsGenReg operand2, opSize=polyWordOpSize } ::
                    CallRTS{rtsEntry=ArbPrecisionEmulate, saveRegs=getSaveRegs saveRegs } ::
                    moveIfNecessary({src=realOp1Reg, dst=realDestReg, kind=opSizeToIMove polyWordOpSize}, code)
            end

        |   codeExtended _ ({instr=LongArbArithmetic{oper, resultReg=PReg resReg, operand1=PReg op1Reg, operand2, saveRegs, ...}, ...}, code) =
            (
                case decache operand2 of
                    RegisterArgument(PReg op2Reg) =>
                    (* Arithmetic operation with both arguments as registers.  These operations are all symmetric so
                       we can try to put either argument into the result reg and then do the operation on the other arg. *)
                    let
                        val realDestReg = getAllocatedGenReg resReg
                        val realOp1Reg = getAllocatedGenReg op1Reg
                        and realOp2Reg = getAllocatedGenReg op2Reg
                        val (operandReg, moveInstr) =
                            if realOp1Reg = realDestReg
                            then (realOp2Reg, code)
                            else if realOp2Reg = realDestReg
                            then (realOp1Reg, code)
                            else (realOp2Reg, Move{source=RegisterArg realOp1Reg, destination=RegisterArg realDestReg, moveSize=opSizeToMove polyWordOpSize} :: code)
                    in
                        ArithToGenReg { opc=oper, output=realDestReg, source=RegisterArg operandReg, opSize=polyWordOpSize } ::
                            CallRTS{rtsEntry=ArbPrecisionEmulate, saveRegs=getSaveRegs saveRegs } :: moveInstr
                    end

                |   operand2 =>
                    (* Arithmetic operation with the first argument in a register and the second a constant or memory location. *)
                    let
                        val realDestReg = getAllocatedReg resReg
                        val realOp1Reg = getAllocatedReg op1Reg
                        val op2Arg = codeExtArgumentAsGenReg operand2
                        (* If we couldn't put it in the result register we have to copy it there. *)
                    in
                        ArithToGenReg { opc=oper, output=asGenReg realDestReg, source=op2Arg, opSize=polyWordOpSize } ::
                            CallRTS{rtsEntry=ArbPrecisionEmulate, saveRegs=getSaveRegs saveRegs } ::
                            moveIfNecessary({src=realOp1Reg, dst=realDestReg, kind=opSizeToIMove polyWordOpSize}, code)
                    end
            )

        val newCode = codeCreate (functionName, profileObject, debugSwitches) 
        
        local
            (* processed - set to true when a block has been processed. *)
            val processed = Array.array(numBlocks, false)
            fun haveProcessed n = Array.sub(processed, n)
            
            (* Find the blocks that reference this one.  This isn't essential but
               allows us to try to generate blocks in the order of the control
               flow.  This in turn may allow us to use short branches rather
               than long ones. *)
            val labelRefs = Array.array(numBlocks, [])

            datatype flowCode =
                FlowCodeSimple of int
            |   FlowCodeCMove of {code: operation list, trueJump: int, falseJump: int}
            
            (* Process this recursively to set the references.  If we have
               unreachable blocks, perhaps because they've been merged, we
               don't want to include them in the reference counting.
               This shouldn't happen now that IdentifyReferences removes
               unreferenced blocks. *)
            fun setReferences fromLabel toLabel =
                case Array.sub(labelRefs, toLabel) of
                    [] => (* Not yet visited at all. *)
                    let
                        val ExtendedBasicBlock{ flow, ...} = Vector.sub(blocks, toLabel)
                        val refs =
                            case flow of
                                ExitCode => []
                            |   Unconditional lab => [lab]
                            |   Conditional{trueJump, falseJump, ... } => [trueJump, falseJump]
                            |   IndexedBr labs => labs
                            |   SetHandler { handler, continue } => [handler, continue]
                            |   UnconditionalHandle _ => []
                            |   ConditionalHandle { continue, ...} => [continue]

                        val () =
                            if fromLabel >= 0 then Array.update(labelRefs, toLabel, [fromLabel]) else ()
                    in
                        List.app (setReferences toLabel) refs
                    end
                    
                |   refs =>
                    (* We've visiting this at least once.  Just add us to the list. *)
                        Array.update(labelRefs, toLabel, fromLabel :: refs)
            
            val _ = setReferences 0 0
            
            (* Process the blocks.  We keep the "stack" explicit rather than using recursion
               because this allows us to select both arms of a conditional branch sooner. *)
            fun genCode(toDo, lastFlow, code) =
            case List.filter (not o haveProcessed) toDo of
                [] =>
                let
                    (* There's nothing left to do. We may need to add a final branch to the end. *)
                    val finalBranch =
                        case lastFlow of
                            ExitCode => []
                        |   IndexedBr _ => []
                        |   Unconditional dest => [UncondBranch(getBlockLabel dest)]
                        |   Conditional { condition, trueJump, falseJump, ...} =>
                                [
                                    UncondBranch(getBlockLabel falseJump),
                                    ConditionalBranch{test=condition, label=getBlockLabel trueJump}
                                ]
                        |   SetHandler { continue, ...} => [UncondBranch(getBlockLabel continue)]
                        |   UnconditionalHandle _ => []
                        |   ConditionalHandle { continue, ...} => [UncondBranch(getBlockLabel continue)]
                in
                    finalBranch @ code (* Done. *)
                end

            |   stillToDo as head :: _ =>
                let
                    local
                        (* Check the references.  If all the sources that lead up to this have
                           already been we won't have any backward jumps. *)
                        fun available dest = List.all haveProcessed (Array.sub(labelRefs, dest))

                        val continuation =
                            case lastFlow of
                                ExitCode => NONE
                            |   IndexedBr _ => NONE (* We could put the last branch in here. *)
                            |   Unconditional dest =>
                                    if not (haveProcessed dest) andalso available dest
                                    then SOME(FlowCodeSimple dest)
                                    else NONE
                            |   Conditional {trueJump, falseJump, condition, ...} =>
                                let
                                    (* Can we replace this with a SETCC or CMOV?  If both arms simply set
                                       a register to a value and either return or jump to the same location
                                       we can use a SETCC or a CMOV.  *)
                                    val ExtendedBasicBlock { flow=tFlow, block=tBlock, ...} = Vector.sub(blocks, trueJump)
                                    and ExtendedBasicBlock { flow=fFlow, block=fBlock, ...} = Vector.sub(blocks, falseJump)

                                    fun cmoveOrSetcc{condition, output, tSource=IntegerConstant trueValue, fSource=IntegerConstant falseValue, kind, code} =
                                        let (* Could use SETCC.  Only if we can use LEA for multiplication.  The result must be
                                               tagged so we will always have a multiplier. *)
                                            val (multiplier, fValue, testCondition) =
                                                if trueValue >= falseValue
                                                then (trueValue-falseValue, falseValue, condition)
                                                else (falseValue-trueValue, trueValue, invertTest condition)
                                            val destReg = asGenReg output
                                        in
                                            if not (targetArch = Native32Bit andalso (destReg=esi orelse destReg=edi))
                                                (* We can't use Setcc with esi or edi on native 32-bit. *)
                                               andalso (multiplier = 2 orelse multiplier = 4 orelse multiplier = 8)
                                               (* We're using LEA so can only be multiplying by 2, 4 or 8. *)
                                               andalso is32bit fValue (* and we're going to put this in the offset *)
                                            then
                                            let
                                                val effectiveOpSize =
                                                    (* We can generally use 32-bit LEA except if the result is negative. *)
                                                    if kind = Move32Bit orelse fValue >= 0 andalso fValue+multiplier <= 0x7fffffff
                                                    then OpSize32 else OpSize64
                                                val (index, base) =
                                                    case multiplier of
                                                        2 => (Index1 destReg, SOME destReg)
                                                    |   4 => (Index4 destReg, NONE)
                                                    |   8 => (Index8 destReg, NONE)
                                                    |   _ => (NoIndex, NONE)
                                                (* Try to put the instruction to zero the register before any compare.  We can do it
                                                   provided the register we're going to zero isn't used in the comparison. *)
                                                fun checkArg(RegisterArg r) = r <> destReg
                                                |   checkArg(MemoryArg mem) = checkMem mem
                                                |   checkArg _ = true
                                                
                                                and checkMem{base, index=NoIndex, ...} = base <> destReg
                                                |   checkMem{base, index=Index1 index, ...} = base <> destReg andalso index <> destReg
                                                |   checkMem{base, index=Index2 index, ...} = base <> destReg andalso index <> destReg
                                                |   checkMem{base, index=Index4 index, ...} = base <> destReg andalso index <> destReg
                                                |   checkMem{base, index=Index8 index, ...} = base <> destReg andalso index <> destReg

                                                val zeroReg = ArithToGenReg { opc=XOR, output=destReg, source=RegisterArg destReg, opSize=OpSize32 } 

                                                fun addXOR [] = NONE
                                                |   addXOR ((instr as ResetStack _) :: tl) =
                                                        (* If we can add the XOR before the ResetStack do so. *)
                                                        Option.map(fn code => instr :: code) (addXOR tl)
                                                |   addXOR ((instr as ArithToGenReg{output, source, ...}) :: tl) =
                                                        if output <> destReg andalso checkArg source
                                                        then SOME(instr :: zeroReg :: tl)
                                                        else NONE
                                                |   addXOR ((instr as ArithMemConst{address, ...}) :: tl) =
                                                        if checkMem address
                                                        then SOME(instr :: zeroReg :: tl)
                                                        else NONE
                                                |   addXOR ((instr as ArithByteMemConst{address, ...}) :: tl) =
                                                        if checkMem address
                                                        then SOME(instr :: zeroReg :: tl)
                                                        else NONE
                                                |   addXOR ((instr as XMMArith{source=MemoryArg mem, ...}) :: tl) =
                                                        if checkMem mem
                                                        then SOME(instr :: zeroReg :: tl)
                                                        else NONE
                                                |   addXOR ((instr as XMMArith _) :: tl) = SOME(instr :: zeroReg :: tl)
                                                |   addXOR ((instr as TestByteBits{arg, ...}) :: tl) =
                                                        if checkArg arg
                                                        then SOME(instr :: zeroReg :: tl)
                                                        else NONE
                                                |   addXOR ((instr as RepeatOperation CMPS8) :: tl) =
                                                        (* This uses edi, esi and ecx implicitly *)
                                                        if destReg <> esi andalso destReg <> edi andalso destReg <> ecx
                                                        then SOME(instr :: zeroReg :: tl)
                                                        else NONE
                                                    (* This seems to be just a conditional jump as a result of
                                                       testing the condition code twice in Real.== *)
                                                |   addXOR _ = NONE

                                                (* If we can't put the XOR before the instruction we need to either zero
                                                   it using a move which won't affect the CC or we use MOVZB to extend
                                                   the byte value to 32/64 bits. *)
                                                val loadAddr = LoadAddress{output=destReg, offset=Int.fromLarge fValue, base=base, index=index, opSize=effectiveOpSize}
                                                and setCond = SetCondition{output=destReg, test=testCondition}
                                                val code =
                                                    case addXOR code of
                                                        SOME withXOR => loadAddr :: setCond :: withXOR
                                                    |   NONE =>
                                                        loadAddr ::
                                                            (* We've already check that we're not using esi/edi on native 32-bits. *)
                                                            Move{destination=RegisterArg destReg, source=RegisterArg destReg, moveSize=Move8} :: setCond :: code
                                            in
                                                SOME code
                                            end
                                            else NONE
                                        end

                                        (* If either value is a memory location it isn't safe to load it.  The base address
                                           may not be valid if the condition does not hold. *)
                                    |   cmoveOrSetcc{tSource=MemoryLocation _, ...} = NONE
                                    |   cmoveOrSetcc{fSource=MemoryLocation _, ...} = NONE
                                        
                                    |   cmoveOrSetcc{condition, output, tSource, fSource, kind, code} =
                                        if targetArch = Native32Bit
                                        then NONE (* CMov doesn't work for constants. *)
                                        else
                                        let
                                            val output = asGenReg output
                                            val codeTrue = codeExtArgumentAsGenReg tSource
                                            and codeFalse = codeExtArgumentAsGenReg fSource
                                            val opSize =
                                                case kind of
                                                    Move32Bit => OpSize32
                                                |   Move64Bit => OpSize64
                                                | _ => raise InternalError "move size"
                                            (* One argument has to be loaded into a register first and the other
                                               is conditionally moved.  *)
                                            val loadFalseCmoveTrue =
                                                if (case codeFalse of RegisterArg regFalse => regFalse = output | _ => false)
                                                then true (* The false value is already in the right register. *)
                                                else if (case codeTrue of RegisterArg regTrue => regTrue = output | _ => false)
                                                then false (* The true value is in the right register - have to reverse. *)
                                                else if (case codeTrue of NonAddressConstArg _ => true | _ => false)
                                                then false (* The true value is a short constant.  If we use a CMOV we will have to put that
                                                              in the non-constant area and use a PC-relative reference.  Try to avoid it. *)
                                                else true
                                            fun cmov{codeLoad, codeMove, condition} =
                                            let
                                                val load =
                                                    case codeLoad of
                                                        RegisterArg regLoad =>
                                                            moveIfNecessary({src=GenReg regLoad, dst=GenReg output, kind=opSizeToIMove opSize}, code)
                                                    |   codeLoad =>
                                                            Move{source=codeLoad, destination=RegisterArg output, moveSize=opSizeToMove opSize} ::  code
                                            in
                                                CondMove{test=condition, output=output, source=codeMove, opSize=opSize} :: load
                                            end
                                        in
                                            if loadFalseCmoveTrue
                                            then SOME(cmov{codeLoad=codeFalse, codeMove=codeTrue, condition=condition})
                                            else SOME(cmov{codeLoad=codeTrue, codeMove=codeFalse, condition=invertTest condition})
                                        end
                                    
                                    val isPossSetCCOrCmov =
                                        if not (haveProcessed trueJump) andalso available trueJump
                                            andalso not (haveProcessed falseJump) andalso available falseJump
                                        then case (tFlow, fFlow, tBlock, fBlock) of
                                            (ExitCode,
                                             ExitCode,
                                             [{instr=LoadArgument{dest=PReg tReg, source=tSource, kind=kindT}, ...},
                                                {instr=ReturnResultFromFunction{resultReg=PReg resReg, realReg, numStackArgs, ...}, ...}],
                                             [{instr=LoadArgument{dest=PReg fReg, source=fSource, kind=kindF}, ...},
                                                {instr=ReturnResultFromFunction _, ...}]) =>
                                             (* The real register for the two sides should both be rax. *)
                                            let
                                                val realTReg = getAllocatedReg tReg and realFReg = getAllocatedReg fReg
                                            in
                                                if realTReg = realFReg andalso kindT = kindF andalso (kindT = Move32Bit orelse kindT = Move64Bit)
                                                then
                                                (
                                                    case cmoveOrSetcc{condition=condition, output=realTReg, tSource=tSource, fSource=fSource,
                                                                      kind=kindT, code=code} of
                                                        SOME code =>
                                                        let
                                                            val resultReg = getAllocatedReg resReg
                                                            val code =
                                                                ReturnFromFunction numStackArgs ::
                                                                    moveIfNecessary({src=resultReg, dst=realReg, kind=moveNativeWord}, code)
                                                        in
                                                            SOME{code=code, trueJump=trueJump, falseJump=falseJump}
                                                        end
                                                    |   NONE => NONE
                                               )
                                               else NONE
                                            end
                                        |   (Unconditional tDest,
                                             Unconditional fDest,
                                             [{instr=LoadArgument{dest=PReg tReg, source=tSource, kind=kindT}, ...}],
                                             [{instr=LoadArgument{dest=PReg fReg, source=fSource, kind=kindF}, ...}]) =>
                                            let
                                                val realTReg = getAllocatedReg tReg and realFReg = getAllocatedReg fReg
                                            in
                                                if tDest = fDest andalso realTReg = realFReg andalso kindT = kindF andalso (kindT = Move32Bit orelse kindT = Move64Bit)
                                                then
                                                (
                                                    case cmoveOrSetcc{condition=condition, output=realTReg, tSource=tSource, fSource=fSource,
                                                                      kind=kindT, code=code} of
                                                        SOME code => SOME{code=code, trueJump=trueJump, falseJump=falseJump}
                                                    |   NONE => NONE
                                                )
                                                else NONE
                                            end
                                        |   _ => NONE
                                        else NONE
                                in
                                    case isPossSetCCOrCmov of
                                        NONE =>
                                        (* We can usually choose either destination and in nearly all cases
                                           it won't matter.  The default branch is not to take forward jumps
                                           so if there is reason to believe that one branch is more likely
                                           we should follow that branch now and leave the other.  If we
                                           have JO/JNO we assume that overflow is unusual.  If one branch
                                           raises an exception we assume that that is unusual. *)
                                        let
                                            val (first, second) =
                                                case (condition, Vector.sub(blocks, falseJump)) of
                                                    (JNO, _) => (trueJump, falseJump)
                                                |   (_, ExtendedBasicBlock{ flow=ExitCode, block, ...}) =>
                                                        if List.exists(fn{instr=RaiseExceptionPacket _, ...} => true | _ => false) block
                                                        then (trueJump, falseJump)
                                                        else (falseJump, trueJump)
                                                |   _ => (falseJump, trueJump)
                                        in
                                            if not (haveProcessed first) andalso available first
                                            then SOME(FlowCodeSimple first)
                                            else if not (haveProcessed second) andalso available second
                                            then SOME(FlowCodeSimple second)
                                            else NONE
                                        end
                                    |   SOME args => SOME(FlowCodeCMove args)
                                end
                           |    SetHandler { continue, ... } =>
                                    (* We want the continuation if possible.  We'll need a
                                       branch round the handler so that won't help. *)
                                    if not (haveProcessed continue) andalso available continue
                                    then SOME(FlowCodeSimple continue)
                                    else NONE
                           |    UnconditionalHandle _ => NONE
                           |    ConditionalHandle _ => NONE
                    in
                        (* First choice - continue the existing block.
                           Second choice - the first item whose sources have all been
                           processed.
                           Third choice - something from the list. *)
                        val picked =
                            case continuation of
                                SOME c => c
                            |   NONE =>
                                    case List.find available stillToDo of
                                        SOME c => FlowCodeSimple c
                                    |   NONE => FlowCodeSimple head
                    end
                    
                in
                    case picked of
                        FlowCodeSimple picked =>
                        let
                            val () = Array.update(processed, picked, true)

                            (* Code to terminate the previous block. *)
                            val startCode =
                                case lastFlow of
                                    ExitCode => []
                                |   IndexedBr _ => []
                                |   UnconditionalHandle _ => []
                                |   Unconditional dest =>
                                        if dest = picked then [] else [UncondBranch(getBlockLabel dest)]
                                |   ConditionalHandle { continue, ...} =>
                                        if continue = picked then [] else [UncondBranch(getBlockLabel continue)]
                                |   SetHandler { continue, ... } =>
                                        if continue = picked then [] else [UncondBranch(getBlockLabel continue)]
                                |   Conditional { condition, trueJump, falseJump, ...} =>
                                    if picked = falseJump (* Usual case. *)
                                    then [ConditionalBranch{test=condition, label=getBlockLabel trueJump}]
                                    else if picked = trueJump
                                    then (* We have a jump to the true condition. Invert the jump.
                                            This is more than an optimisation.  Because this immediately precedes the
                                            true block we're not going to generate a label. *)
                                        [ConditionalBranch{test=invertTest condition, label=getBlockLabel falseJump}]
                                    else
                                    [
                                        UncondBranch(getBlockLabel falseJump),
                                        ConditionalBranch{test=condition, label=getBlockLabel trueJump}
                                    ]

                            (* Code-generate the body with the code we've done so far
                               at the end.  Add a label at the start if necessary. *)
                            local
                                (* If the previous block dropped through to this and this was
                                   the only reference then we don't need a label. *)
                                fun onlyJumpingHere (lab: int) =
                                    if lab <> picked then false
                                    else case Array.sub(labelRefs, picked) of
                                        [singleton] => singleton = lab
                                    |   _ => false
                    
                                val noLabel =
                                    case lastFlow of
                                        ExitCode => picked = 0 (* Unless this was the first block. *)
                                    |   Unconditional dest => onlyJumpingHere dest
                                    |   Conditional { trueJump, falseJump, ...} =>
                                            onlyJumpingHere trueJump orelse onlyJumpingHere falseJump
                                    |   IndexedBr _ => false
                                    |   SetHandler _ => false
                                    |   UnconditionalHandle _ => false
                                    |   ConditionalHandle { continue, ...} => onlyJumpingHere continue
                            in
                                val startLabel = if noLabel then [] else [JumpLabel(getBlockLabel picked)]
                            end

                            val ExtendedBasicBlock { flow, block, ...} = Vector.sub(blocks, picked)

                            local
                                fun genCodeBlock(instr, code) = codeExtended {flow=flow} (instr, code)
                            in
                                val bodyCode = List.foldl genCodeBlock (startLabel @ startCode @ code) block
                            end

                            val addSet =
                                case flow of
                                    ExitCode => []
                                |   IndexedBr cases => cases
                                |   Unconditional dest => [dest]
                                |   Conditional {trueJump, falseJump, ...} => [falseJump, trueJump]
                                |   SetHandler { handler, continue } => [handler, continue]
                                |   UnconditionalHandle _ => []
                                |   ConditionalHandle { continue, ...} => [continue]

                        in
                            genCode(addSet @ stillToDo, flow, bodyCode)
                        end
                
                    |   FlowCodeCMove{code, trueJump, falseJump} =>
                        let
                            (* We've generated a conditional move and possibly a return.  If the
                               trueJump and falseJump are only ever referenced from this block
                               they're done, otherwise we still need to do them. *)
                            val _ =
                                case Array.sub(labelRefs, trueJump) of
                                    [_] => Array.update(processed, trueJump, true)
                                |   _ => ()
                            val _ =
                                case Array.sub(labelRefs, falseJump) of
                                    [_] => Array.update(processed, falseJump, true)
                                |   _ => ()
                            val ExtendedBasicBlock { flow, ...} = Vector.sub(blocks, trueJump)
                            val addSet =
                                case flow of
                                    ExitCode => []
                                |   Unconditional dest => [dest]
                                |   _ => raise InternalError "FlowCodeCMove"
                        in
                            genCode(addSet @ stillToDo, flow, code)
                        end
                end
        in
            val ops = genCode([0], ExitCode, [])
        end
    in
        X86OPTIMISE.generateCode{code=newCode, ops=List.rev ops,
                                 labelCount= !outputLabelCount, resultClosure=resultClosure}
    end

    val nGenRegs = List.length generalRegisters

    structure Sharing =
    struct
        type intSet             = intSet
        and extendedBasicBlock  = extendedBasicBlock
        and regProperty         = regProperty
        and reg                 = reg
        and closureRef          = closureRef
    end

end;
