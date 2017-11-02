(*
    Copyright (c) 2016-17 David C.J. Matthews

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

functor X86ICodeIdentifyReferences(
    structure ICODE: ICodeSig
    structure DEBUG: DEBUGSIG
    structure INTSET: INTSETSIG
): X86IDENTIFYREFSSIG =
struct
    open ICODE
    open INTSET

    type regState =
    { 
        active: int, refs: int, pushState: bool, prop: regProperty
    }

    and conflictState =
    {
        conflicts: int list, realConflicts: reg list
    }

    (* CC states before and after.  The instruction may use the CC or ignore it. The only
       instruction to use the CC is X87FPGetCondition.  Conditional branches are handled at
       the block level.
       The result of executing the instruction may be to set the condition code to a
       defined state, an undefined state or leave it unchanged.
       N.B. Some "instructions" may involve a stack reset that could affect the CC. *)
    datatype outCCState = CCSet of ccRef | CCIndeterminate | CCUnchanged
    and inCCState = CCNeeded of ccRef | CCUnused
    
    datatype extendedBasicBlock =
        ExtendedBasicBlock of
        {
            block: {instr: x86ICode, current: intSet, active: intSet, kill: intSet } list,
            flow: controlFlow,
            locals: intSet, (* Defined and used entirely within the block. *)
            imports: intSet, (* Defined outside the block, used inside it, but not needed afterwards. *)
            exports: intSet, (* Defined within the block, possibly used inside, but used outside. *)
            passThrough: intSet, (* Active throughout the block. May be referred to by it but needed afterwards. *)
            initialStacks: intSet, (* Stack items required at the start i.e. imports+passThrough for stack items. *)
            inCCState: ccRef option, (* The state this block assumes.  If SOME _ all predecessors must set it. *)
            outCCState: ccRef option (* The condition code set by this block.  SOME _ if at least one successor needs it. *)
        }
    
    exception InternalError = Misc.InternalError

    (* Return the list of blocks that are the immediate successor of this. *)
    fun blockSuccessors(BasicBlock{flow, ...}) = successorBlocks flow

    (* Find the registers from an argument. *)
    fun argRegs(RegisterArgument rarg) = [rarg]
    |   argRegs(MemoryLocation { base, index, cache=SOME cr, ...}) = cr  :: base :: argIndex index
    |   argRegs(MemoryLocation { base, index, cache=NONE, ...}) = base :: argIndex index
    |   argRegs(StackLocation { cache=SOME rarg, ...}) = [rarg]
    |   argRegs _ = []
    
    and argIndex NoMemIndex = []
    |   argIndex(MemIndex1 arg) = [arg]
    |   argIndex(MemIndex2 arg) = [arg]
    |   argIndex(MemIndex4 arg) = [arg]
    |   argIndex(MemIndex8 arg) = [arg]

    fun argStacks(StackLocation { container, ...}) = [container]
    |   argStacks(ContainerAddr { container, ...}) = [container]
    |   argStacks _ = []

    (* Return the set of registers used by the instruction.
       sources are registers that must have values after the instruction.
       dests are registers that are given values or modified by the instruction. *)
    fun getInstructionState(LoadArgument { source, dest, ...}) =
        { sources=argRegs source, dests=[dest], sStacks=argStacks source, dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(StoreArgument{ source, base, index, ...}) =
            { sources=argRegs source @ [base] @ argIndex index, dests=[], sStacks=argStacks source, dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(LoadMemReg { dest, ...}) =
            { sources=[], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(BeginFunction {regArgs, stackArgs, ...}) =
            { sources=[], dests=map #1 regArgs, sStacks=[], dStacks=stackArgs, ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(FunctionCall{regArgs, stackArgs, dest, ...}) =
        let
            (* Non-tail-recursive.  Behaves as a normal reference to sources. *)
            fun getSources argSource =
            let
                val stackSources = List.foldl(fn (arg, srcs) => argSource arg @ srcs) [] stackArgs
                fun regSource((arg, _), srcs) = argSource arg @ srcs
            in
                List.foldl regSource stackSources regArgs
            end
        in
            { sources=getSources argRegs, dests=[dest], sStacks=getSources argStacks, dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }
        end

    |   getInstructionState(TailRecursiveCall{regArgs, stackArgs, workReg, ...}) =
        let
            (* Tail recursive call.  References the argument sources but exits. *)
            fun getSources argSource =
            let
                val stackSources = List.foldl(fn ({src, ...}, srcs) => argSource src @ srcs) [] stackArgs
                fun regSource((arg, _), srcs) = argSource arg @ srcs
            in
                List.foldl regSource stackSources regArgs
            end
        in
            { sources=getSources argRegs, dests=[workReg], sStacks=getSources argStacks, dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }
        end

    |   getInstructionState(AllocateMemoryOperation{dest, ...}) =
            { sources=[], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(AllocateMemoryVariable{size, dest, ...}) =
            { sources=[size], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(InitialiseMem{size, addr, init}) =
            { sources=[size, addr, init], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(InitialisationComplete) =
            (* This is just a marker.  It doesn't actually generate any code. *)
            { sources=[], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(JumpLoop{regArgs, stackArgs, workReg, ...}) =
        let
            fun getSources argSource =
            let
                val regSourceAsRegs =
                    List.foldl(fn ((source, _), srcs) => argSource source @ srcs) [] regArgs
            in
                List.foldl(fn ((source, _, _), srcs) => argSource source @ srcs) regSourceAsRegs stackArgs
            end
            val dests = case workReg of SOME r => [r] | NONE => []
        in
            { sources=getSources argRegs, dests=dests, sStacks=getSources argStacks, dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }
        end

    |   getInstructionState(RaiseExceptionPacket{packetReg}) =
            { sources=[packetReg], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(ReserveContainer{container, ...}) =
            { sources=[], dests=[], sStacks=[], dStacks=[container], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(IndexedCaseOperation{testReg, workReg, ...}) =
            { sources=[testReg], dests=[workReg], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(LockMutable{addr}) =
            { sources=[addr], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(WordComparison{arg1, arg2, ccRef, ...}) =
            { sources=arg1 :: argRegs arg2, dests=[], sStacks=argStacks arg2, dStacks=[], ccIn=CCUnused, ccOut=CCSet ccRef }

    |   getInstructionState(PushExceptionHandler{workReg, ...}) =
            { sources=[], dests=[workReg], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(PopExceptionHandler{ workReg }) =
            { sources=[], dests=[workReg], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(BeginHandler{ workReg, packetReg, ...}) =
            { sources=[], dests=[packetReg, workReg], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(ReturnResultFromFunction{resultReg, ...}) =
            { sources=[resultReg], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(ArithmeticFunction{resultReg, operand1, operand2, ccRef, ...}) =
            { sources=operand1 :: argRegs operand2, dests=[resultReg],
              sStacks=argStacks operand2, dStacks=[], ccIn=CCUnused, ccOut=CCSet ccRef }

    |   getInstructionState(TestTagBit{arg, ccRef, ...}) =
            { sources=argRegs arg, dests=[], sStacks=argStacks arg, dStacks=[], ccIn=CCUnused, ccOut=CCSet ccRef }

    |   getInstructionState(PushValue {arg, container, ...}) =
            { sources=argRegs arg, dests=[], sStacks=argStacks arg, dStacks=[container], ccIn=CCUnused, ccOut=CCUnchanged }
    
    |   getInstructionState(CopyToCache{source, dest, ...}) =
            { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged}

    |   getInstructionState(ResetStackPtr{preserveCC, ...}) =
            { sources=[], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused,
                ccOut=if preserveCC then CCUnchanged else CCIndeterminate }

    |   getInstructionState(StoreToStack {source, container, ...}) =
        (* Although this stores into the container it must already exist. *)
            { sources=argRegs source, dests=[], sStacks=container :: argStacks source, dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(TagValue{source, dest, ...}) =
            { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(UntagValue{source, dest, cache, ...}) =
            { sources=case cache of NONE => [source] | SOME cr => [cr, source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(LoadEffectiveAddress{base, index, dest, ...}) =
        let
            val bRegs =
                case base of NONE => [] | SOME bReg => [bReg]
            val iRegs = argIndex index
        in
            { sources=bRegs @ iRegs, dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }
        end

    |   getInstructionState(ShiftOperation{resultReg, operand, shiftAmount, ccRef, ...}) =
            { sources=operand :: argRegs shiftAmount, dests=[resultReg],
              sStacks=argStacks shiftAmount, dStacks=[], ccIn=CCUnused, ccOut=CCSet ccRef }

    |   getInstructionState(Multiplication{resultReg, operand1, operand2, ccRef, ...}) =
            { sources=operand1 :: argRegs operand2, dests=[resultReg],
              sStacks=argStacks operand2, dStacks=[], ccIn=CCUnused, ccOut=CCSet ccRef }

    |   getInstructionState(Division{dividend, divisor, quotient, remainder, ...}) =
            { sources=dividend :: argRegs divisor, dests=[quotient, remainder],
              sStacks=argStacks divisor, dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(AtomicExchangeAndAdd{base, source}) =
            { sources=[base, source], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(BoxValue{source, dest, ...}) =
            { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(CompareByteVectors{vec1Addr, vec2Addr, length, ccRef, ...}) =
            { sources=[vec1Addr, vec2Addr, length], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCSet ccRef }

    |   getInstructionState(BlockMove{srcAddr, destAddr, length, ...}) =
            { sources=[srcAddr, destAddr, length], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(X87Compare{arg1, arg2, ccRef, ...}) =
            { sources=arg1 :: argRegs arg2, dests=[], sStacks=argStacks arg2,
              dStacks=[], ccIn=CCUnused, ccOut=CCSet ccRef }

    |   getInstructionState(SSE2Compare{arg1, arg2, ccRef, ...}) =
            { sources=arg1 :: argRegs arg2, dests=[], sStacks=argStacks arg2,
              dStacks=[], ccIn=CCUnused, ccOut=CCSet ccRef }

    |   getInstructionState(X87FPGetCondition{dest, ccRef, ...}) =
            { sources=[], dests=[dest], sStacks=[], dStacks=[], ccIn=CCNeeded ccRef, ccOut=CCIndeterminate }

    |   getInstructionState(X87FPArith{resultReg, arg1, arg2, ...}) =
            { sources=arg1 :: argRegs arg2, dests=[resultReg],
              sStacks=argStacks arg2, dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(X87FPUnaryOps{dest, source, ...}) =
            { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(X87Float{dest, source}) =
            { sources=argRegs source, dests=[dest], sStacks=argStacks source, dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(SSE2Float{dest, source}) =
            { sources=argRegs source, dests=[dest], sStacks=argStacks source, dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(SSE2FPArith{resultReg, arg1, arg2, ...}) =
            { sources=arg1 :: argRegs arg2, dests=[resultReg],
              sStacks=argStacks arg2, dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    fun identifyRegisters(blockVector, pregProps): extendedBasicBlock vector * regState vector =
    let
        val maxPRegs = Vector.length pregProps
        val vectorLength = Vector.length blockVector
        (* Initial arrays - declarationArray is the set of registers given
           values by the block, importArray is the set of registers referenced by
           the block and not declared locally. *)
        val declarationArray = Array.array(vectorLength, emptySet)
        and importArray = Array.array(vectorLength, emptySet)
        val stackDecArray = Array.array(vectorLength, emptySet)
        and stackImportArray = Array.array(vectorLength, emptySet)
        
        (* References - this is used locally to see if a register is ever
           actually used and also included in the result which uses it as
           part of the choice of which register to spill. *)
        val regRefs = Array.array(maxPRegs, 0)
        (* Registers that must be pushed because they are required after
           a function call.  For cache registers this means "discard". *)
        and requirePushOrDiscard = Array.array(maxPRegs, false)

        fun incrRef r = Array.update(regRefs, r, Array.sub(regRefs, r)+1)
        
        (* Contains the, possibly filtered, code for each block. *)
        val resultCode = Array.array(vectorLength, NONE)
        
        val ccInStates = Array.array(vectorLength, CCUnused)
        and ccOutStates = Array.array(vectorLength, CCIndeterminate)
        
        (* First pass - for each block build up the sets of registers defined and
           used in the block.  We do this depth-first so that we can use "refs" to
           see if a register is used.  If this is an instruction that can be eliminated
           we don't need to generate it and can ignore any references it makes. *)
        local
            fun blockScan blockNo =
            if isSome(Array.sub(resultCode, blockNo)) then ()
            else
            let
                val () = Array.update(resultCode, blockNo, SOME []) (* Prevent looping. *)
                val thisBlock as BasicBlock { block, flow, ...} = Vector.sub(blockVector, blockNo)
                val successors = blockSuccessors thisBlock
                (* Visit everything reachable first. *)
                val () = List.app blockScan successors
                
                fun scanCode(instr, original as { code, decs, refs, sDecs, sRefs, occIn, occOut }) =
                let
                    val { sources, dests, sStacks=stackSrcs, dStacks=stackDests, ccIn, ccOut, ... } =
                        getInstructionState instr
                    val eliminateable =
                        case instr of
                            LoadArgument _ => true
                        |   TagValue _ => true
                        |   UntagValue _ => true
                        |   LoadEffectiveAddress _ => true
                        |   BoxValue _ => true
                        |   CopyToCache _ => true
                        |   LoadMemReg _ => true
                        |   _ => false
                    fun regNo(PReg i) = i
                    and stackNo(StackLoc{rno, ...}) = rno
                    val destRegNos = map regNo dests
                    and sourceRegNos = map regNo sources
                    val stackDestRegNos = map stackNo stackDests
                    and stackSourceRegNos = map stackNo stackSrcs
                    (* Check the CC if we need one and this sets it. The input CC is the earliest required CC
                       that is not set by this code. *)
                    val newInCC =
                        case (occIn, ccOut) of
                            (CCNeeded ccRIn, CCSet ccRout) =>
                                if ccRIn = ccRout then ccIn else raise InternalError "CCCheck failed"
                        |   (CCNeeded _, CCIndeterminate) => raise InternalError "CCCheck failed"
                        |   (_, CCUnchanged) => occIn
                        |   _ => ccIn
                    (* The output CC is the last CC set.  Tail instructions that don't change
                       the CC state are ignored until we reach an instruction that sets it. *)
                    val newOutCC = case occOut of CCUnchanged => ccOut | _ => occOut
                in
                    if eliminateable andalso
                        List.all(fn dReg => Array.sub(regRefs, dReg) = 0) destRegNos
                    then original (* Don't include this instruction. *)
                    else
                    let
                        (* Only mark the sources as referred after we know we're going to need this.
                           In that way we may eliminate the instruction that created this source. *)
                        val () = List.app incrRef sourceRegNos
                    in
                        { code = instr :: code, decs = union(listToSet destRegNos, decs), refs = union(listToSet sourceRegNos, refs),
                          sDecs = union(listToSet stackDestRegNos, sDecs), sRefs = union(listToSet stackSourceRegNos, sRefs),
                          occIn = newInCC, occOut = newOutCC}
                    end
                end
                
                (* If we have a conditional branch at the end we need the condition code.  It should either
                   be set here or in a preceding block. *)
                val inCC = case flow of Conditional { ccRef, ...} => CCNeeded ccRef | _ => CCUnused

                val { code, decs, refs, sDecs, sRefs, occIn, occOut, ... } =
                    List.foldr scanCode
                        {code=[], decs=emptySet, refs=emptySet, sDecs=emptySet, sRefs=emptySet, occIn=inCC, occOut=CCUnchanged} block
            in
                Array.update(declarationArray, blockNo, decs);
                (* refs includes local declarations. Remove before adding to the result. *)
                Array.update(importArray, blockNo, minus(refs, decs));
                Array.update(stackDecArray, blockNo, sDecs);
                Array.update(stackImportArray, blockNo, minus(sRefs, sDecs));
                Array.update(resultCode, blockNo, SOME code);
                Array.update(ccInStates, blockNo, occIn);
                Array.update(ccOutStates, blockNo, occOut)
            end
        in
            val () = blockScan 0 (* Start with the root block. *)
        end
        
        (* Second phase - Propagate reference information between the blocks.
           We need to consider loops here.  Do a depth-first scan marking each
           block.  If we find a loop we save the import information we've used.
           If when we come to process that block we find the import information
           is different we need to reprocess. *)
        (* Pass through array - values used in other blocks after this that
           are not declared in this block. *)
        val passThroughArray = Array.array(vectorLength, emptySet)
        val stackPassThroughArray = Array.array(vectorLength, emptySet)
        (* Exports - those of our declarations that are used in other blocks. *)
        val exportArray = Array.array(vectorLength, emptySet)
        val stackExportArray = Array.array(vectorLength, emptySet)
        (* If any one of the successors requires the CC then this is set.
           Otherwise we leave it as Unused. *)
        val ccRequiredOut = Array.array(vectorLength, CCUnused)
        local
            datatype loopData =
                Unprocessed | Processing | Processed | Looped of { regSet: intSet, stackSet: intSet, ccState: inCCState }
            
            fun reprocessLoop () =
            let
                val reprocess = ref false
                val loopArray = Array.array(vectorLength, Unprocessed)
            
                fun processBlocks blockNo =
                    case Array.sub(loopArray, blockNo) of
                        Processed => (* Already seen this by a different route. *)
                            {
                                regSet = union(Array.sub(passThroughArray, blockNo), Array.sub(importArray, blockNo)),
                                stackSet = union(Array.sub(stackPassThroughArray, blockNo), Array.sub(stackImportArray, blockNo)),
                                ccState = Array.sub(ccInStates, blockNo)
                            }
                    |   Looped s => s (* We've already seen this in a loop. *)
                    |   Processing => (* We have a loop. *)
                        let
                            (* Use the existing input array. *)
                            val inputs =
                            {
                                regSet = union(Array.sub(passThroughArray, blockNo), Array.sub(importArray, blockNo)),
                                stackSet = union(Array.sub(stackPassThroughArray, blockNo), Array.sub(stackImportArray, blockNo)),
                                ccState = Array.sub(ccInStates, blockNo)
                            }
                            val () = Array.update(loopArray, blockNo, Looped inputs)
                        in
                            inputs
                        end
                    |   Unprocessed => (* Normal case - not visited yet. *)
                        let
                            val () = Array.update(loopArray, blockNo, Processing)
                            val thisBlock = Vector.sub(blockVector, blockNo)
                            val ourDeclarations = Array.sub(declarationArray, blockNo)
                            and ourStackDeclarations = Array.sub(stackDecArray, blockNo)
                            val successors = blockSuccessors thisBlock

                            fun addSuccessor b =
                            let
                                val {regSet=theirImports, stackSet=theirStackImports, ccState=theirInState} = processBlocks b
                                (* Split the imports.  If a register is a local declaration then
                                   it becomes an export.  If it is not it becomes part of our
                                   passThrough. *)
                                val (addToExp, addToImp) =
                                    INTSET.partition (fn i => member(i, ourDeclarations)) theirImports
                                val (addToStackExp, addToStackImp) =
                                    INTSET.partition (fn i => member(i, ourStackDeclarations)) theirStackImports
                                (* Merge the input states from each of the successors. *)
                                val () =
                                    case (theirInState, Array.sub(ccRequiredOut, blockNo)) of
                                        (CCNeeded ts, CCNeeded req) =>
                                            if ts = req then () else raise InternalError "Mismatched states"
                                    |   (ts as CCNeeded _, _) => Array.update(ccRequiredOut, blockNo, ts)
                                    |   _ => ()
                            in
                                Array.update(exportArray, blockNo,
                                    union(Array.sub(exportArray, blockNo), addToExp));
                                Array.update(passThroughArray, blockNo,
                                    union(Array.sub(passThroughArray, blockNo), addToImp));
                                Array.update(stackExportArray, blockNo,
                                    union(Array.sub(stackExportArray, blockNo), addToStackExp));
                                Array.update(stackPassThroughArray, blockNo,
                                    union(Array.sub(stackPassThroughArray, blockNo), addToStackImp))
                            end
                            val () = List.app addSuccessor successors
                            val ourInputs =
                                union(Array.sub(passThroughArray, blockNo), Array.sub(importArray, blockNo))
                            val ourStackInputs =
                                union(Array.sub(stackPassThroughArray, blockNo), Array.sub(stackImportArray, blockNo))
                        in
                            (* Check that we supply the required state. *)
                            case (Array.sub(ccRequiredOut, blockNo), Array.sub(ccOutStates, blockNo)) of
                                (CCNeeded ccReq, CCSet ccSet) =>
                                    if ccReq = ccSet then () else raise InternalError "Mismatched cc states"
                            |   (CCNeeded _, CCIndeterminate) => raise InternalError "Mismatched cc states"
                            |   (cc as CCNeeded needOut, CCUnchanged) =>
                                (
                                    (* We pass through the state.  If we don't use the state then we
                                       need to set this as the input.  If we do use the state it must be
                                       the same. *)
                                    case Array.sub(ccInStates, blockNo) of
                                        CCUnused => Array.update(ccInStates, blockNo, cc)
                                    |   CCNeeded needIn =>
                                            if needOut = needIn then () else raise InternalError "Mismatched cc states"
                                )
                            |   _ => ();
                            (* Was this block used in a loop?  If so we should not be requiring a CC. *)
                            case Array.sub(loopArray, blockNo) of
                                Looped {regSet, stackSet, ...} =>
                                (
                                    case Array.sub(ccInStates, blockNo) of
                                        CCNeeded _ => raise InternalError "Looped state needs cc" | _ => ();
                                    if setToList regSet = setToList ourInputs andalso
                                        setToList stackSet = setToList ourStackInputs
                                    then ()
                                    else reprocess := true
                                )
                            |   _ => ();
                            Array.update(loopArray, blockNo, Processed);
                            { regSet = ourInputs, stackSet = ourStackInputs, ccState = Array.sub(ccInStates, blockNo) }
                        end

            in
                reprocess := false;
                processBlocks 0;
                if !reprocess then reprocessLoop () else ()
            end
        in
            val () = reprocessLoop ()
        end
        
        (* Third pass - Build the result list with the active registers for each
           instruction.  We don't include registers in the passThrough set since
           they are active throughout the block. *)
        local
            (* Number of instrs for which this is active.  We use this to try to select a
               register to push to the stack if we have too many.  Registers that have
               only a short lifetime are less likely to be pushed than those that are
               active longer. *)
            val regActive = Array.array(maxPRegs, 0)
            fun addActivity n r = Array.update(regActive, r, Array.sub(regActive, r)+n)
            
            fun createResultInstrs (passThrough, stackPassThrough)
                (instr, (tail, activeAfterThis, stackActiveAfterThis)) =
            let
                val { sources, dests, sStacks=stackSrcs, dStacks=stackDests, ... } = getInstructionState instr
                fun regNo(PReg i) = i
                fun stackNo(StackLoc{rno, ...}) = rno
                val destRegNos = map regNo dests
                and sourceRegNos = map regNo sources
                val destSet = listToSet destRegNos
                (* Remove any sources that are present in passThrough since
                   they are going to be active throughout the block. *)
                and sourceSet = minus(listToSet sourceRegNos, passThrough)
                val stackDestRegNos = map stackNo stackDests
                and stackSourceRegNos = map stackNo stackSrcs
                val stackDestSet = listToSet stackDestRegNos
                and stackSourceSet = minus(listToSet stackSourceRegNos, stackPassThrough)

                (* To compute the active set for the PREVIOUS instruction (we're processing from the
                   end back to the start) we remove any registers that have been given values in this
                   instruction and add anything that we are using in this instruction since they will
                   now need to have values. *)
                val afterRemoveDests = minus(activeAfterThis, destSet)
                val stackAfterRemoveDests = minus(stackActiveAfterThis, stackDestSet)
                val activeForPrevious = union(sourceSet, afterRemoveDests)
                val stackActiveForPrevious = union(stackSourceSet, stackAfterRemoveDests)
            
                (* The "active" set is the set of registers that need to be active DURING the
                   instruction.  It includes destinations, which will usually be in
                   "activeAfterThis", because there may be destinations that are not actually used
                   subsequently but still need a register.  That will also include work registers.
                   Usually sources aren't included if this is the last use but the
                   AllocateMemoryVariable "instruction" can't set the size after the memory is
                   allocated so the active set includes the source(s). *)
                val activeForInstr =
                    case instr of
                        FunctionCall _ => sourceSet (* Is this still needed? *)
                    |   TailRecursiveCall _ =>
                            (* Set the active set to the total set of registers we require including
                               the work register.  This ensures that we will spill as many registers
                               as we require when we look at the size of the active set. *)
                            union(sourceSet, destSet)
                    |   AllocateMemoryVariable _ => (* We can only set the size after the memory is allocated. *)
                            union(activeAfterThis, union(sourceSet, destSet))
                    |   BoxValue _ => (* We can only store the value in the box after the box is allocated. *)
                            union(activeAfterThis, union(sourceSet, destSet))
                    |   _ => union(activeAfterThis, destSet)
                
                val () = List.app(addActivity 1) (setToList activeForInstr)

                local
                    (* If we are allocating memory we have to save the current registers if
                       they could contain an address.  We mustn't push untagged registers
                       and we mustn't push the destination. *)
                    fun getSaveSet dReg =
                    let
                        val activeAfter = union(activeAfterThis, passThrough)
                        (* Remove any registers marked - must-not-push.  These are
                           registers holding non-address values.  They will actually
                           be saved by the RTS across any GC but not checked or
                           modified by the GC.
                           Exclude the result register. *)
                        fun getSave i =
                            if i = dReg
                            then NONE
                            else case Vector.sub(pregProps, i) of
                                RegPropGeneral => SOME(PReg i)
                            |   RegPropCacheTagged => SOME(PReg i)
                            |   _ => NONE
                    in
                        List.mapPartial getSave (setToList activeAfter)
                    end
                in
                    (* Sometimes we need to modify the instruction e.g. to include the set
                       of registers to save. *)
                    val convertedInstr =
                        case instr of
                            AllocateMemoryOperation{size, flags, dest, saveRegs=_} =>
                                AllocateMemoryOperation{size=size, flags=flags, dest=dest,
                                    saveRegs=getSaveSet(regNo dest)}

                        |   AllocateMemoryVariable{size, dest, saveRegs=_} =>
                                AllocateMemoryVariable{size=size, dest=dest, saveRegs=getSaveSet(regNo dest)}

                        |   BoxValue{source, dest, boxKind, saveRegs=_} =>
                                BoxValue{source=source, dest=dest, boxKind=boxKind,
                                    saveRegs=getSaveSet(regNo dest)}
                        
                        |   JumpLoop{regArgs, stackArgs, checkInterrupt, workReg, ...} =>
                            let
                                (* If we have to check for interrupts we must preserve registers across
                                   the RTS call. *)
                                fun getSave i =
                                    case Vector.sub(pregProps, i) of
                                    RegPropGeneral => SOME(PReg i)
                                |   RegPropCacheTagged => SOME(PReg i)
                                |   _ => NONE
                                val currentRegs = union(activeAfterThis, passThrough)
                                val check =
                                    case checkInterrupt of
                                        NONE => NONE
                                    |   SOME _ => SOME(List.mapPartial getSave (setToList currentRegs))
                            in
                                JumpLoop{regArgs=regArgs, stackArgs=stackArgs, checkInterrupt=check, workReg=workReg}
                            end

                        |   _ => instr
                end
                
                (* FunctionCall must mark all registers as "push". *)
                local
                    fun pushRegisters () =
                    let
                        val activeAfter = union(activeAfterThis, passThrough)
                        fun pushAllButDests i =
                            if List.exists(fn j => i=j) destRegNos
                            then ()
                            else case Vector.sub(pregProps, i) of
                                RegPropCacheTagged => raise InternalError "pushRegisters: cache reg"
                            |   RegPropCacheUntagged => raise InternalError "pushRegisters: cache reg"
                            |  _ => Array.update(requirePushOrDiscard, i, true)
                    in
                        (* We need to push everything active after this
                           except the result register. *)
                        List.app pushAllButDests (setToList activeAfter)
                    end
                in
                    val () =
                        case instr of
                            FunctionCall _ => pushRegisters ()
                            (* It should no longer be necessary to push across a handler but
                               there still seem to be cases that need it. *)
                        |   BeginHandler _ => pushRegisters ()
                        
                        |   CopyToCache {source=PReg srcReg, dest=PReg dstReg, ...} =>
                            (* If the source is a cache register marked as "must push" i.e. discard,
                               the destination must also be discarded i.e. not available. 
                               Note: the source could be a non-cache register marked for pushing. *)
                            (
                                case (Vector.sub(pregProps, srcReg), Array.sub(requirePushOrDiscard, srcReg)) of
                                    (RegPropCacheTagged, true) => Array.update(requirePushOrDiscard, dstReg, true)
                                |   (RegPropCacheUntagged, true) => Array.update(requirePushOrDiscard, dstReg, true)
                                |   _ => ()
                            )
                        |   _ => ()
                end
                
                (* Which entries are active in this instruction but not afterwards? *)
                val kill = union(minus(stackSourceSet, stackActiveAfterThis), minus(sourceSet, activeAfterThis))
            in
                ({instr=convertedInstr, active=activeForInstr, current=activeAfterThis, kill=kill} :: tail, activeForPrevious,
                 stackActiveForPrevious)
            end

            fun createResult blockNo =
            let
                val BasicBlock{ flow, ...} = Vector.sub(blockVector, blockNo)
                val declSet   = Array.sub(declarationArray, blockNo)
                and importSet = Array.sub(importArray, blockNo)
                and passSet   = Array.sub(passThroughArray, blockNo)
                and exportSet = Array.sub(exportArray, blockNo)
                and stackPassSet = Array.sub(stackPassThroughArray, blockNo)
                and stackImportSet = Array.sub(stackImportArray, blockNo)
                and stackExportSet = Array.sub(stackExportArray, blockNo)
                val filteredCode = getOpt(Array.sub(resultCode, blockNo), [])
                (* At the end of the block we should have the exports active. *)
                val (resultInstrs, _, _) = List.foldr (createResultInstrs (passSet, stackPassSet)) ([], exportSet, stackExportSet) filteredCode
                (* Set the active count for the pass through. *)
                val instrCount = List.length filteredCode
                val () = List.app(addActivity instrCount) (setToList passSet)
                val inCCState =
                    case Array.sub(ccInStates, blockNo) of CCNeeded s => SOME s | CCUnused => NONE
                val outCCState =
                    case Array.sub(ccRequiredOut, blockNo) of CCNeeded s => SOME s | CCUnused => NONE
            in
                ExtendedBasicBlock {
                    block = resultInstrs,
                    flow=flow,
                    locals = minus(declSet, exportSet),
                    imports = importSet,
                    exports = exportSet,
                    passThrough = passSet,
                    initialStacks = union(stackPassSet, stackImportSet),
                    inCCState = inCCState,
                    outCCState = outCCState
                }
            end
        in
            val resultBlocks = Vector.tabulate(vectorLength, createResult)
            val regActive = regActive
        end
        
        val registerState: regState vector =
            Vector.tabulate(maxPRegs,
                fn i => {
                    active = Array.sub(regActive, i),
                    refs = Array.sub(regRefs, i),
                    pushState = Array.sub(requirePushOrDiscard, i),
                    prop = Vector.sub(pregProps, i)
                }
            )
    in
        (resultBlocks, registerState)
    end

    (* Exported for use in GetConflictSets *)
    fun getInstructionRegisters instr =
    let
        val {sources, dests, ...} = getInstructionState instr
    in
        {sources=sources, dests=dests}
    end

    structure Sharing =
    struct
        type x86ICode           = x86ICode
        and reg                 = reg
        and preg                = preg
        and intSet              = intSet
        and basicBlock          = basicBlock
        and extendedBasicBlock  = extendedBasicBlock
        and controlFlow         = controlFlow
        and argument            = argument
        and memoryIndex         = memoryIndex
        and regProperty         = regProperty
        and ccRef               = ccRef
    end
end;
