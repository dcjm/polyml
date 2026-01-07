(*
    Copyright (c) 2021-3 David C.J. Matthews

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

functor Arm64IdentifyReferences(
    structure Arm64ICode: ARM64ICODE
    structure Debug: DEBUG
    structure IntSet: INTSET
): ARM64IDENTIFYREFERENCES =
struct
    open Arm64ICode
    open IntSet

    type regState =
    { 
        active: int, refs: int, pushState: bool, prop: regProperty
    }

    (* CC states before and after.  Currently no instruction uses the
       condition; conditional branches are handled at the block level.
       The result of executing the instruction may be to set the condition code to a
       defined state, an undefined state or leave it unchanged. *)
    datatype outCCState = CCSet of ccRef | CCIndeterminate | CCUnchanged
    and inCCState = CCNeeded of ccRef | CCUnused
    
    datatype extendedBasicBlock =
        ExtendedBasicBlock of
        {
            block: {instr: iCodeAbstract, current: intSet, active: intSet, kill: intSet } list,
            flow: controlFlow,
            locals: intSet, (* Defined and used entirely within the block. *)
            imports: intSet, (* Defined outside the block, used inside it, but not needed afterwards. *)
            exports: intSet, (* Defined within the block, possibly used inside, but used outside. *)
            passThrough: intSet, (* Active throughout the block. May be referred to by it but needed afterwards. *)
            loopRegs: intSet, (* Destination registers for a loop.  They will be updated by this block. *)
            initialStacks: intSet, (* Stack items required at the start i.e. imports+passThrough for stack items. *)
            inCCState: ccRef option, (* The state this block assumes.  If SOME _ all predecessors must set it. *)
            outCCState: ccRef option (* The condition code set by this block.  SOME _ if at least one successor needs it. *)
        }
    
    exception InternalError = Misc.InternalError

    (* Return the list of blocks that are the immediate successor of this. *)
    fun blockSuccessors(BasicBlock{flow, ...}) = successorBlocks flow

    fun getOptReg(SomeReg reg) = [reg] | getOptReg ZeroReg = []

    fun getInstructionState(MoveRegister { source, dest, ...}) =
        { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(LoadNonAddressConstant { dest, ...}) =
        { sources=[], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(LoadFPConstant { dest, ...}) =
        { sources=[], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(LoadAddressConstant { dest, ...}) =
        { sources=[], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(LoadWithConstantOffset { base, dest, ...}) =
        { sources=[base], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(LoadFPWithConstantOffset { base, dest, ...}) =
        { sources=[base], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(LoadWithIndexedOffset { base, dest, index, ...}) =
        { sources=[base, index], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(LoadFPWithIndexedOffset { base, dest, index, ...}) =
        { sources=[base, index], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(GetThreadId { dest, ...}) =
        { sources=[], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(ObjectIndexAddressToAbsolute { source, dest, ...}) =
        { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(AbsoluteToObjectIndex { source, dest, ...}) =
        { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(AllocateMemoryFixed { dest, ...}) =
        { sources=[], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(AllocateMemoryVariable{size, dest, ...}) =
            { sources=[size], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(InitialiseMem{size, addr, init}) =
            { sources=[size, addr, init], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(BeginLoop) =
            (* This is just a marker.  It doesn't actually generate any code. *)
            { sources=[], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(JumpLoop{regArgs, stackArgs, ...}) =
        let
            fun getSourceFromRegs({src=ArgInReg reg, ...}, (regArgs, stackArgs)) = (reg :: regArgs, stackArgs)
            |   getSourceFromRegs({src=ArgOnStack { container, ...}, ...}, (regArgs, stackArgs)) = (regArgs, container :: stackArgs)
            and getSourceFromStack({src=ArgInReg reg, ...}, (regArgs, stackArgs)) = (reg :: regArgs, stackArgs)
            |   getSourceFromStack({src=ArgOnStack { container, ...}, ...}, (regArgs, stackArgs)) = (regArgs, container :: stackArgs)
            val (regSources, stackSources) =
                List.foldl getSourceFromRegs (List.foldl getSourceFromStack ([], []) stackArgs) regArgs
        in
            { sources=regSources, dests=[], sStacks=stackSources, dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }
        end

    |   getInstructionState(StoreWithConstantOffset { base, source, ...}) =
        { sources=[source, base], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(StoreFPWithConstantOffset { base, source, ...}) =
        { sources=[source, base], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(StoreWithIndexedOffset { base, source, index, ...}) =
        { sources=[source, base, index], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(StoreFPWithIndexedOffset { base, source, index, ...}) =
        { sources=[source, base, index], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(AddSubImmediate{ source, dest, ccRef, ... }) =
        { sources=[source], dests=getOptReg dest, sStacks=[], dStacks=[],
          ccIn=CCUnused, ccOut=case ccRef of NONE => CCUnchanged | SOME cc => CCSet cc }

    |   getInstructionState(AddSubRegister{ base, shifted, dest, ccRef, ... }) =
        { sources=[base, shifted], dests=getOptReg dest, sStacks=[], dStacks=[],
          ccIn=CCUnused, ccOut=case ccRef of NONE => CCUnchanged | SOME cc => CCSet cc }

    |   getInstructionState(LogicalImmediate{ source, dest, ccRef, ... }) =
        { sources=[source], dests=getOptReg dest, sStacks=[], dStacks=[],
          ccIn=CCUnused, ccOut=case ccRef of NONE => CCUnchanged | SOME cc => CCSet cc }

    |   getInstructionState(LogicalRegister{ base, shifted, dest, ccRef, ... }) =
        { sources=[base, shifted], dests=getOptReg dest, sStacks=[], dStacks=[],
          ccIn=CCUnused, ccOut=case ccRef of NONE => CCUnchanged | SOME cc => CCSet cc }

    |   getInstructionState(ShiftRegister{ source, shift, dest, ... }) =
        { sources=[source, shift], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(CountLeadingZeroBits{ source, dest, ... }) =
        { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(Multiplication{ dest, sourceA, sourceM, sourceN, ... }) =
        { sources=getOptReg sourceA @ [sourceM, sourceN],
          dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(Division{ dest, dividend, divisor, ... }) =
        { sources=[dividend, divisor], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(BeginFunction {regArgs, fpRegArgs, stackArgs, ...}) =
            { sources=[], dests=map #1 regArgs @ map #1 fpRegArgs, sStacks=[], dStacks=stackArgs, ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(FunctionCall{regArgs, stackArgs, dests, fpRegArgs, fpDests, containers, ...}) =
        let
            (* Non-tail-recursive.  Behaves as a normal reference to sources. *)
            fun getSourceFromRegs((ArgInReg reg, _), (regArgs, stackArgs)) = (reg :: regArgs, stackArgs)
            |   getSourceFromRegs((ArgOnStack { container, ...}, _), (regArgs, stackArgs)) = (regArgs, container :: stackArgs)
            and getSourceFromStack(ArgInReg reg, (regArgs, stackArgs)) = (reg :: regArgs, stackArgs)
            |   getSourceFromStack(ArgOnStack { container, ...}, (regArgs, stackArgs)) = (regArgs, container :: stackArgs)
            val (argSources, stackSources) =
                List.foldl getSourceFromRegs (List.foldl getSourceFromStack ([], []) stackArgs) regArgs
            val fpArgSources = List.map #1 fpRegArgs
        in
            { sources=argSources @ fpArgSources, dests=List.map #1 dests @ List.map #1 fpDests, sStacks=stackSources @ containers, dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }
        end

    |   getInstructionState(TailRecursiveCall{regArgs, fpRegArgs, stackArgs, ...}) =
        let
            (* Tail recursive call.  References the argument sources but exits. *)
            fun getSourceFromRegs((ArgInReg reg, _), (regArgs, stackArgs)) = (reg :: regArgs, stackArgs)
            |   getSourceFromRegs((ArgOnStack { container, ...}, _), (regArgs, stackArgs)) = (regArgs, container :: stackArgs)
            and getSourceFromStack({src=ArgInReg reg, ...}, (regArgs, stackArgs)) = (reg :: regArgs, stackArgs)
            |   getSourceFromStack({src=ArgOnStack { container, ...}, ...}, (regArgs, stackArgs)) = (regArgs, container :: stackArgs)
            val (argSources, stackSources) =
                List.foldl getSourceFromRegs (List.foldl getSourceFromStack ([], []) stackArgs) regArgs
            val fpArgSources = List.map #1 fpRegArgs
        in
            { sources=argSources@fpArgSources, dests=[], sStacks=stackSources, dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }
        end

    |   getInstructionState(ReturnResultFromFunction{results, fpResults, returnReg, ...}) =
            { sources=returnReg :: List.map #1 results @ List.map #1 fpResults, dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(RaiseExceptionPacket{packetReg}) =
            { sources=[packetReg], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(PushToStack{ source, container, ... }) =
            { sources=[source], dests=[], sStacks=[], dStacks=[container], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(LoadStack{ dest, container, ... }) =
            { sources=[], dests=[dest], sStacks=[container], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(StoreToStack{ source, container, ... }) =
        (* Although this stores into the container it must already exist. *)
            { sources=[source], dests=[], sStacks=[container], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(ContainerAddress{ dest, container, ... }) =
            { sources=[], dests=[dest], sStacks=[container], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(ResetStackPtr _) =
            { sources=[], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(TagValue{source, dest, ...}) =
            { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(UntagValue{source, dest, ...}) =
            { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(BoxLarge{source, dest, ...}) =
            { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(UnboxLarge{source, dest, ...}) =
            { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(BoxTagFloat{source, dest, ...}) =
            { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(UnboxTagFloat{source, dest, ...}) =
            { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(LoadAcquire { base, dest, ...}) =
        { sources=[base], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(StoreRelease { base, source, ...}) =
        { sources=[source, base], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(BitFieldShift{ source, dest, ... }) =
        { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(BitFieldInsert{ source, destAsSource, dest, ... }) =
        { sources=[source, destAsSource], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(IndexedCaseOperation{ testReg }) =
        { sources=[testReg], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(PushExceptionHandler) =
        { sources=[], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(PopExceptionHandler) =
        { sources=[], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(BeginHandler{packetReg}) =
        (* The packet register is a destination since this provides its definition. *)
        { sources=[], dests=[packetReg], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(CompareByteVectors{vec1Addr, vec2Addr, length, ccRef, ...}) =
            { sources=[vec1Addr, vec2Addr, length], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCSet ccRef }

    |   getInstructionState(BlockMove{srcAddr, destAddr, length, ...}) =
            { sources=[srcAddr, destAddr, length], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCIndeterminate }

    |   getInstructionState(AddSubXSP{source, dest, ...}) =
            { sources=[source], dests=getOptReg dest, sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(TouchValue{source}) =
            { sources=[source], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(LoadAcquireExclusive{base, dest}) =
            { sources=[base], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(StoreReleaseExclusive{base, source, result}) =
            { sources=[base] @ getOptReg source, dests=[result], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(MemoryBarrier) =
            { sources=[], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(ConvertIntToFloat{ source, dest, ...}) =
            { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(ConvertFloatToInt{ source, dest, ...}) =
            { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(UnaryFloatingPt{ source, dest, ...}) =
            { sources=[source], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(BinaryFloatingPoint{ arg1, arg2, dest, ...}) =
            { sources=[arg1, arg2], dests=[dest], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(CompareFloatingPoint{ arg1, arg2, ccRef, ...}) =
            { sources=[arg1, arg2], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCSet ccRef }

    |   getInstructionState(CPUYield) =
            { sources=[], dests=[], sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    |   getInstructionState(AtomicOperation{ base, source, dest, ... }) =
            { sources=base :: getOptReg source, dests=getOptReg dest, sStacks=[], dStacks=[], ccIn=CCUnused, ccOut=CCUnchanged }

    (* These instructions can be eliminated if their register sources are not used.
       There may be other cases. *)
    fun eliminateable(MoveRegister _) = true
    |   eliminateable(LoadNonAddressConstant _) = true
    |   eliminateable(LoadAddressConstant _) = true
    |   eliminateable(LoadWithConstantOffset _) = true
    |   eliminateable(LoadWithIndexedOffset _) = true
    |   eliminateable(ObjectIndexAddressToAbsolute _) = true
    |   eliminateable(TagValue _) = true
    |   eliminateable(UntagValue _) = true
    |   eliminateable(BoxLarge _) = true
    |   eliminateable(UnboxLarge _) = true
    |   eliminateable(BoxTagFloat _) = true
    |   eliminateable(UnboxTagFloat _) = true

    |   eliminateable _ = false
   

    fun identifyRegs(blockVector, pregProps): extendedBasicBlock vector * regState vector =
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
        and localLoopRegArray = Array.array(vectorLength, emptySet)
        
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
                
                fun scanCode(instr, original as { code, decs, refs, sDecs, sRefs, occIn, occOut, loopRegs, ... }) =
                let
                    val { sources, dests, sStacks=stackSrcs, dStacks=stackDests, ccIn, ccOut, ... } =
                        getInstructionState instr
                    fun regNo(PReg i) = i
                    and stackNo(StackLoc{rno, ...}) = rno
                    val destRegNos = map regNo dests
                    and sourceRegNos = map regNo sources
                    val stackDestRegNos = map stackNo stackDests
                    and stackSourceRegNos = map stackNo stackSrcs
                    (* If this instruction requires a cc i.e. is SetToCondition or X87FPGetCondition we
                       need to set this as a requirement earlier.  If this sets the CC and it is the condition
                       we've been expecting we've satisfied it and can set the previous condition to Unused.
                       We could use this to decide if a comparison is no longer required.  That can only
                       happen in very specific circumstances e.g. some tests in Test176.ML so it's
                       not worthwhile. *)
                    val newInCC =
                        case (ccIn, ccOut, occIn) of
                            (cc as CCNeeded _, _, _) => cc (* This instr needs a particular cc. *)
                        |   (CCUnused, CCSet _, _) => CCUnused
                        |   (CCUnused, _, occIn) => occIn
                    (* If this instruction modifies the CC check to see if it is setting an requirement. *)
                    val _ =
                        case (occIn, ccOut) of
                            (CCNeeded ccRIn, CCSet ccRout) =>
                                if ccRIn = ccRout then () else raise InternalError "CCCheck failed"
                        |   (CCNeeded _, CCIndeterminate) => raise InternalError "CCCheck failed"
                        |   _ => ()
                    (* The output CC is the last CC set.  Tail instructions that don't change
                       the CC state are ignored until we reach an instruction that sets it. *)
                    val newOutCC = case occOut of CCUnchanged => ccOut | _ => occOut

                    val instrLoopRegs =
                        case instr of
                            JumpLoop{regArgs, ...} => listToSet (map (regNo o #dst) regArgs)
                        |   _ => emptySet
                in
                    if eliminateable instr andalso
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
                          occIn = newInCC, occOut = newOutCC, loopRegs = union(loopRegs, instrLoopRegs)}
                    end
                end
                
                (* If we have a conditional branch at the end we need the condition code.  It should either
                   be set here or in a preceding block. *)
                val inCC = case flow of Conditional { ccRef, ...} => CCNeeded ccRef | _ => CCUnused

                val { code, decs, refs, sDecs, sRefs, occIn, occOut, loopRegs, ... } =
                    List.foldr scanCode
                        {code=[], decs=emptySet, refs=emptySet, sDecs=emptySet, sRefs=emptySet, occIn=inCC, occOut=CCUnchanged, loopRegs=emptySet} block
            in
                Array.update(declarationArray, blockNo, decs);
                (* refs includes local declarations. Remove before adding to the result. *)
                Array.update(importArray, blockNo, minus(refs, decs));
                Array.update(localLoopRegArray, blockNo, loopRegs);
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
        (* Loop registers.  This contains the registers that are not exported
           from or passed through this block but are used subsequently as
           loop registers. *)
        val loopRegArray = Array.array(vectorLength, emptySet)
        val () = Array.copy{src=localLoopRegArray, dst=loopRegArray, di=0}
        (* If any one of the successors requires the CC then this is set.
           Otherwise we leave it as Unused. *)
        val ccRequiredOut = Array.array(vectorLength, CCUnused)
        local
            datatype loopData =
                Unprocessed | Processing | Processed
            |   Looped of { regSet: intSet, loopSet: intSet, stackSet: intSet, ccState: inCCState }
            
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
                                ccState = Array.sub(ccInStates, blockNo),
                                loopSet = Array.sub(loopRegArray, blockNo)
                            }
                    |   Looped s => s (* We've already seen this in a loop. *)
                    |   Processing => (* We have a loop. *)
                        let
                            (* Use the existing input array. *)
                            val inputs =
                            {
                                regSet = union(Array.sub(passThroughArray, blockNo), Array.sub(importArray, blockNo)),
                                stackSet = union(Array.sub(stackPassThroughArray, blockNo), Array.sub(stackImportArray, blockNo)),
                                ccState = Array.sub(ccInStates, blockNo),
                                loopSet = Array.sub(loopRegArray, blockNo)
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
                            and ourLocalLoopRegs = Array.sub(localLoopRegArray, blockNo)
                            val successors = blockSuccessors thisBlock

                            fun addSuccessor b =
                            let
                                val {regSet=theirImports, stackSet=theirStackImports, ccState=theirInState, loopSet=theirLoops} = processBlocks b
                                (* Remove loop regs from the imports if they are actually given new
                                   values by this block.  We don't want to pass the old loop regs through here. *)
                                val theirImports = minus(theirImports, ourLocalLoopRegs)
                                (* Split the imports.  If a register is a local declaration then
                                   it becomes an export.  If it is not it becomes part of our
                                   passThrough. *)
                                val (addToExp, addToImp) =
                                    IntSet.partition (fn i => member(i, ourDeclarations)) theirImports
                                val (addToStackExp, addToStackImp) =
                                    IntSet.partition (fn i => member(i, ourStackDeclarations)) theirStackImports
                                (* Merge the input states from each of the successors. *)
                                val () =
                                    case (theirInState, Array.sub(ccRequiredOut, blockNo)) of
                                        (CCNeeded ts, CCNeeded req) =>
                                            if ts = req then () else raise InternalError "Mismatched states"
                                    |   (ts as CCNeeded _, _) => Array.update(ccRequiredOut, blockNo, ts)
                                    |   _ => ()
                                (* Add loop registers to the set if they are not declared here.  The
                                   only place they are declared is at the entry to the loop so that
                                   stops them being propagated further. *)
                                val addToLoops = minus(theirLoops, ourDeclarations)
                            in
                                Array.update(exportArray, blockNo,
                                    union(Array.sub(exportArray, blockNo), addToExp));
                                Array.update(passThroughArray, blockNo,
                                    union(Array.sub(passThroughArray, blockNo), addToImp));
                                Array.update(stackExportArray, blockNo,
                                    union(Array.sub(stackExportArray, blockNo), addToStackExp));
                                Array.update(stackPassThroughArray, blockNo,
                                    union(Array.sub(stackPassThroughArray, blockNo), addToStackImp));
                                Array.update(loopRegArray, blockNo,
                                    union(Array.sub(loopRegArray, blockNo), addToLoops))
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
                            { regSet = ourInputs, stackSet = ourStackInputs,
                              ccState = Array.sub(ccInStates, blockNo), loopSet=Array.sub(loopRegArray, blockNo)}
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
            in
                (* Eliminate instructions if their results are not required.  The earlier check for this
                   will remove most cases but if we have duplicated a block we may have a register that
                   is required elsewhere but not in this particular branch.  *)
                if not(List.exists(fn PReg d => member(d, activeAfterThis)) dests) andalso eliminateable instr
                then (tail, activeAfterThis, stackActiveAfterThis)
                else
                let
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
                       subsequently but still need a register. *)
                    val activeForInstr =
                        case instr of
                            FunctionCall _ => sourceSet (* Is this still needed? *)
                        |   TailRecursiveCall _ =>
                                (* Set the active set to the total set of registers we require including
                                   the work register.  This ensures that we will spill as many registers
                                   as we require when we look at the size of the active set. *)
                                union(sourceSet, destSet)
                        |   BoxLarge _ =>
                                (* We can only store the value in the box after the box is allocated. *)
                                union(activeAfterThis, union(sourceSet, destSet))
                        |   BoxTagFloat _ =>
                                (* Since the source must be a V register and the destination an
                                   X register there isn't actually a problem here, but do this
                                   anyway. *)
                                union(activeAfterThis, union(sourceSet, destSet))
                        |   _ => union(activeAfterThis, destSet)
                
                    val () = List.app(addActivity 1) (setToList activeForInstr)

                    local
                        (* If we are allocating memory we have to save the current registers if
                           they could contain an address.  We mustn't push untagged registers
                           and we mustn't push the destination. *)
                        fun getSaveSet includeReg =
                        let
                            val activeAfter = union(activeAfterThis, passThrough)
                            (* The main save set contains registers that need to be updated
                               across a GC.  In addition we need to record any registers
                               containing non-address values.  These must not use X30 since
                               that will contain the return address.
                               Exclude the result register. *)
                            fun getSave i =
                                if includeReg i
                                then case Vector.sub(pregProps, i) of
                                    RegPropGeneral => SOME(PReg i)
                                |   RegPropCacheTagged => SOME(PReg i)
                                |   RegPropUntagged => NONE
                                |   RegPropStack _ => NONE
                                |   RegPropCacheUntagged => NONE
                                |   RegPropMultiple => raise InternalError "getSave: RegPropMultiple"
                                else NONE
                            and getNoSave i =
                                if includeReg i
                                then case Vector.sub(pregProps, i) of
                                    RegPropGeneral => NONE
                                |   RegPropCacheTagged => NONE
                                |   RegPropUntagged => SOME(PReg i)
                                |   RegPropStack _ => NONE
                                |   RegPropCacheUntagged => SOME(PReg i)
                                |   RegPropMultiple => raise InternalError "getSave: RegPropMultiple"
                                else NONE
                        in
                            {gcUpdate = List.mapPartial getSave (setToList activeAfter),
                             noUpdate = List.mapPartial getNoSave (setToList activeAfter)}
                        end
                    in
                        (* Sometimes we need to modify the instruction e.g. to include the set
                           of registers to save. *)
                        val convertedInstr =
                            case instr of
                                AllocateMemoryFixed{bytesRequired, dest, saveRegs=_} =>
                                    AllocateMemoryFixed{bytesRequired=bytesRequired, dest=dest,
                                        saveRegs=getSaveSet(fn i => i <> regNo dest)}

                            |   AllocateMemoryVariable{size, dest, saveRegs=_} =>
                                    AllocateMemoryVariable{size=size, dest=dest, saveRegs=getSaveSet(fn i => i <> regNo dest)}

                            |   BoxLarge{source, dest, saveRegs=_} =>
                                    BoxLarge{source=source, dest=dest, saveRegs=getSaveSet(fn i => i <> regNo dest)}

                            |   BoxTagFloat{source, dest, floatSize, saveRegs=_} =>
                                    BoxTagFloat{source=source, dest=dest, floatSize=floatSize,
                                        saveRegs=getSaveSet(fn i => i <> regNo dest)}
                       
                            |   JumpLoop{regArgs, stackArgs, checkInterrupt = SOME _, ...} =>
                                let
                                    (* If we have to check for interrupts we must preserve registers across
                                       the RTS call. *)
                                    fun getSave i =
                                        case Vector.sub(pregProps, i) of
                                        RegPropGeneral => SOME(PReg i)
                                    |   RegPropCacheTagged => SOME(PReg i)
                                    |   RegPropUntagged => NONE
                                    |   RegPropStack _ => NONE
                                    |   RegPropCacheUntagged => NONE
                                    |   RegPropMultiple => raise InternalError "getSave: RegPropMultiple"
                                    and getNoSave i =
                                        case Vector.sub(pregProps, i) of
                                        RegPropGeneral => NONE
                                    |   RegPropCacheTagged => NONE
                                    |   RegPropUntagged => SOME(PReg i)
                                    |   RegPropStack _ => NONE
                                    |   RegPropCacheUntagged => SOME(PReg i)
                                    |   RegPropMultiple => raise InternalError "getSave: RegPropMultiple"
                                    val currentRegs = union(activeAfterThis, passThrough)
                                    (* Have to include the loop registers.  These were previously included
                                       automatically because they were part of the import set. *)
                                    val regsToCheck = (map (regNo o #dst) regArgs @ setToList currentRegs)
                                    val check = {gcUpdate = List.mapPartial getSave regsToCheck, noUpdate = List.mapPartial getNoSave regsToCheck }
                                in
                                    JumpLoop{regArgs=regArgs, stackArgs=stackArgs, checkInterrupt=SOME check}
                                end

                            |   FunctionCall{regArgs, stackArgs=[], dests, fpRegArgs=[], fpDests=[], callKind as ConstantCode m,
                                             saveRegs=_, containers} =>
                                (* If this is arbitrary precision push the registers rather than marking them as "save".
                                   stringOfWord returns 'CODE "PolyAddArbitrary"' etc. *)
                                if (String.isSubstring "Arbitrary\"" (Address.stringOfWord m))
                                then
                                let
                                    val destRegs = List.map (regNo o #1) dests
                                    fun includeInSave i = not(List.exists(fn r => r=i) destRegs)
                                in
                                    FunctionCall{regArgs=regArgs, stackArgs=[], callKind=callKind, dests=dests,
                                        fpRegArgs=[], fpDests=[], containers=containers, saveRegs=getSaveSet includeInSave}
                                end
                                else instr

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
                                FunctionCall{ stackArgs=[], callKind=ConstantCode m, ...} =>
                                if (String.isSubstring "Arbitrary\"" (Address.stringOfWord m))
                                then ()
                                else pushRegisters ()
                            
                            |   FunctionCall _ => pushRegisters ()
                            
                                (* It should no longer be necessary to push across a handler but
                                   there still seem to be cases that need it. *)
                            (*|   BeginHandler _ => pushRegisters ()*)

                            |   _ => ()
                    end
                
                    (* Which entries are active in this instruction but not afterwards? *)
                    val kill = union(minus(stackSourceSet, stackActiveAfterThis), minus(sourceSet, activeAfterThis))
                in
                    ({instr=convertedInstr, active=activeForInstr, current=activeAfterThis, kill=kill} :: tail, activeForPrevious,
                     stackActiveForPrevious)
                end
            end

            fun createResult blockNo =
            let
                val BasicBlock{ flow, ...} = Vector.sub(blockVector, blockNo)
                val declSet   = Array.sub(declarationArray, blockNo)
                and importSet = Array.sub(importArray, blockNo)
                and passSet   = Array.sub(passThroughArray, blockNo)
                and loopSet   = Array.sub(loopRegArray, blockNo)
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
                    loopRegs = loopSet,
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
    (* Exported function.  First filter out unreferenced blocks then process the
       registers themselves. *)
    fun identifyRegisters(blockVector, pregProps) =
    let
        val vectorLength = Vector.length blockVector
        val mapArray = Array.array(vectorLength, NONE)
        and resArray = Array.array(vectorLength, NONE)
        val count = ref 0
        
        fun setReferences label =
            case Array.sub(mapArray, label) of
                NONE => (* Not yet visited *)
                let
                    val BasicBlock{flow, block} = Vector.sub(blockVector, label)
                    (* Create a new entry for it. *)
                    val newLabel = ! count before count := !count + 1
                    (* Add it to the map.  Any other references will use this
                       without reprocessing. *)
                    val () = Array.update(mapArray, label, SOME newLabel)
                    val newFlow =
                        case flow of
                            Unconditional l => Unconditional(setReferences l)
                        |   Conditional{trueJump, falseJump, ccRef, condition} =>
                                Conditional{trueJump=setReferences trueJump, falseJump=setReferences falseJump,
                                        ccRef=ccRef, condition=condition}
                        |   ExitCode => ExitCode
                        |   IndexedBr list => IndexedBr(map setReferences list)
                        |   SetHandler{handler, continue} =>
                                SetHandler{handler=setReferences handler, continue=setReferences continue}
                        |   UnconditionalHandle l => UnconditionalHandle(setReferences l)
                        |   ConditionalHandle{handler, continue} =>
                                ConditionalHandle{handler=setReferences handler, continue=setReferences continue}

                    val () = Array.update(resArray, newLabel, SOME(BasicBlock{flow=newFlow, block=block}))
                in
                    newLabel
                end
            |   SOME lab => lab
       
        val _ = setReferences 0
        
        val newBlockVector =
            Vector.tabulate(!count, fn i => valOf(Array.sub(resArray, i)))
    in
        identifyRegs(newBlockVector, pregProps)
    end

    (* Exported for use in GetConflictSets *)
    fun getInstructionRegisters instr =
    let
        val {sources, dests, ...} = getInstructionState instr
    in
        {sources=sources, dests=dests}
    end
    
    (* Exported for use in ICodeOptimise *)
    val getInstructionCC = #ccOut o getInstructionState

    structure Sharing =
    struct
        type ('genReg, 'optGenReg, 'fpReg) arm64ICode = ('genReg, 'optGenReg, 'fpReg) arm64ICode
        and preg                = preg
        and pregOrZero          = pregOrZero
        and intSet              = intSet
        and ('genReg, 'optGenReg, 'fpReg) basicBlock = ('genReg, 'optGenReg, 'fpReg) basicBlock
        and extendedBasicBlock  = extendedBasicBlock
        and controlFlow         = controlFlow
        and regProperty         = regProperty
        and ccRef               = ccRef
        and outCCState          = outCCState
    end
end;
