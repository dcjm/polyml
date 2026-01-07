(*
    Copyright David C. J. Matthews 2018-22

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

functor X86ICodeOptimise(
    structure X86ICode: X86ICODE
    structure IntSet: INTSET
    structure Identify: X86IDENTIFYREFERENCES
    structure X86Code: X86CODE (* For invertTest. *)
    structure Debug: DEBUG
    structure Pretty: PRETTY
    sharing X86ICode.Sharing = Identify.Sharing = IntSet = X86Code
): X86ICODEOPT =
struct
    open X86ICode
    open IntSet
    open Identify
    val InternalError = Misc.InternalError

    datatype optimise = Changed of basicBlock vector * regProperty vector | Unchanged
    
    (* Optimiser.
       This could incorporate optimisations done elsewhere.
       IdentifyReferences currently removes instructions that
       produce results in registers that are never used.

       PushRegisters deals with caching. Caching involves
       speculative changes that can be reversed if there is a need
       to spill registers.
       
       The optimiser currently deals with booleans and conditions
       and with moving memory loads into an instruction operand.
    *)
    
    (* This is a rewrite of the last instruction to set a boolean.
       This is almost always rewriting the next instruction.  The only
       possibility is that we have a ResetStackPtr in between. *)
    datatype boolRegRewrite =
        BRNone
        (* BRSetConditionToConstant - we have a comparison of two constant value.
           This will usually happen because we've duplicated a branch and
           set a register to a constant which we then compare. *)
    |   BRSetConditionToConstant of
            { srcCC: ccRef, signedCompare: order, unsignedCompare: order }

    fun optimiseICode{ code, pregProps, ccCount=_, debugSwitches=_ } =
    let
        val hasChanged = ref false
        val regCounter = ref(Vector.length pregProps)
        val regList = ref []
        fun newReg kind =
        (
            regList := kind :: ! regList;
            PReg (!regCounter)
        ) before regCounter := !regCounter + 1
        
        (* If this argument is a register and the register is mapped to a memory location, a constant
           or another register replace the value.  Memory locations are only replaced if this is
           the only use.  If there is more than one reference it's better to load it into a
           register and retain the register references.  *)
        fun replaceWithValue(arg as RegisterArgument (preg as PReg pregNo), kill, regMap, instrOpSize) =
        (
            case List.find(fn {dest, ... } => dest = preg) regMap of
                SOME { source as MemoryLocation _, opSize, ...} =>
                (
                    if member(pregNo, kill) andalso opSize = instrOpSize
                    then ( hasChanged := true; source )
                    else arg,
                    (* Filter this from the list.  If this is not the last
                       reference we want to use the register and if it is then
                       we don't need it any longer. *)
                    List.filter(fn {dest, ...} => dest <> preg) regMap
                )
           |    SOME { source, ...} =>
                (
                    source,
                    (* Filter it if it is the last reference. *)
                    if member(pregNo, kill)
                    then List.filter(fn {dest, ...} => dest <> preg) regMap
                    else regMap
                )
           |    NONE => (arg, regMap)
        )
        
        |   replaceWithValue(arg, _, regMap, _) = (arg, regMap)

        fun optimiseBlock processed (block, flow, outCCState) =
        let
            fun optCode([], brCond, regMap, code) = (code, brCond, regMap)

            |   optCode({instr=CompareLiteral{arg1, arg2, ccRef=ccRefOut, opSize}, kill, ...} :: rest,
                        _, regMap, code) =
                let
                    val (repArg1, memRefsOut) = replaceWithValue(arg1, kill, regMap, opSize)
                in
                    case repArg1 of
                        IntegerConstant test =>
                        (* CompareLiteral is put in by CodetreeToIcode to test a boolean value.  It can also
                           arise as the result of pattern matching on booleans or even by tests such as = true.
                           If the source register is now a constant we want to propagate the constant
                           condition. *)
                        let
                            (* This comparison reduces to a constant.  *)
                            val _ = hasChanged := true
                            (* Put in a replacement so that if we were previously testing ccRefOut
                               we should instead test ccRef. *)
                            val repl =
                                BRSetConditionToConstant{srcCC=ccRefOut, signedCompare=LargeInt.compare(test, arg2),
                                (* Unsigned tests.  We converted the values from Word to LargeInt.  We can therefore
                                   turn the tests back to Word for the unsigned comparisons. *)
                                    unsignedCompare=Word.compare(Word.fromLargeInt test, Word.fromLargeInt arg2)}
                            val _ = isSome outCCState andalso raise InternalError "optCode: CC exported"
                        in
                            optCode(rest, repl, memRefsOut, code)
                        end
                
                    |   repArg1 =>
                            optCode(rest, BRNone, memRefsOut,
                                CompareLiteral{arg1=repArg1, arg2=arg2, ccRef=ccRefOut, opSize=opSize}::code)
                end

            |   optCode({instr=LoadArgument{dest, source, kind=Move64Bit}, kill, ...} :: rest, inCond, regMap, code) =
                let
                    val (repSource, mapAfterReplace) = replaceWithValue(source, kill, regMap, OpSize64)
                    (* If the value is a constant or memory after replacement we include this. *)
                    val mapOut =
                        if (case repSource of MemoryLocation _ => true | IntegerConstant _ => true | _ => false)
                        then {dest=dest, source=repSource, opSize=OpSize64} :: mapAfterReplace
                        else mapAfterReplace
                    val outInstr = LoadArgument{dest=dest, source=repSource, kind=Move64Bit}
                in
                    optCode(rest, inCond, mapOut, outInstr::code)
                end

            |   optCode({instr=LoadArgument{dest, source, kind=Move32Bit}, kill, ...} :: rest, inCond, regMap, code) =
                let
                    val (repSource, mapAfterReplace) = replaceWithValue(source, kill, regMap, OpSize32)
                    val mapOut =
                        if (case repSource of MemoryLocation _ => true | IntegerConstant _ => true | _ => false)
                        then {dest=dest, source=repSource, opSize=OpSize32} :: mapAfterReplace
                        else mapAfterReplace
                    val outInstr = LoadArgument{dest=dest, source=repSource, kind=Move32Bit}
                in
                    optCode(rest, inCond, mapOut, outInstr::code)
                end

            |   optCode({instr as LoadArgument{dest, source as MemoryLocation _, kind} , ...} :: rest, inCond, regMap, code) =
                let
                    (* If we load a memory location add it to the list in case we can use it later. *)
                    val memRefsOut =
                        case kind of
                            Move64Bit => {dest=dest, source=source, opSize=OpSize64} :: regMap
                        |   Move32Bit => {dest=dest, source=source, opSize=OpSize32} :: regMap
                        |   _ => regMap
                in
                    optCode(rest, inCond, memRefsOut, instr::code)
                end

            |   optCode({instr as StoreArgument _, ...} :: rest, inCond, _, code) =
                    (* This may change a value in memory.  For safety remove everything. *)
                    optCode(rest, inCond, [], instr::code)

            |   optCode({instr as FunctionCall _, ...} :: rest, _, _, code) =
                    optCode(rest, BRNone, [], instr::code)                

            |   optCode({instr as BeginLoop, ...} :: rest, _, _, code) =
                    (* Any register value from outside the loop are not valid inside. *)
                    optCode(rest, BRNone, [], instr::code)                

            |   optCode({instr as JumpLoop _, ...} :: rest, _, _, code) =
                    (* Likewise at the end of the loop.  Not sure if this is essential. *)
                    optCode(rest, BRNone, [], instr::code)                

                (* These instructions could take memory operands.  This isn't the full set but the others are
                   rare or only take memory operands that refer to boxed memory. *)
            |   optCode({instr=WordComparison{arg1, arg2, ccRef, opSize}, kill, ...} :: rest, _, regMap, code) =
                let
                    (* Replace register reference with memory if possible. *)
                    val (source, memRefsOut) = replaceWithValue(arg2, kill, regMap, opSize)
                in
                    (* This affects the CC. *)
                    optCode(rest, BRNone, memRefsOut, WordComparison{arg1=arg1, arg2=source, ccRef=ccRef, opSize=opSize}::code)
                end

            |   optCode({instr=ArithmeticFunction{oper, resultReg, operand1, operand2, ccRef, opSize}, kill, ...} :: rest, _, regMap, code) =
                let
                    (* Replace register reference with memory if possible. *)
                    val (source, memRefsOut) = replaceWithValue(operand2, kill, regMap, opSize)
                in
                    (* This affects the CC. *)
                    optCode(rest, BRNone, memRefsOut,
                        ArithmeticFunction{oper=oper, resultReg=resultReg, operand1=operand1,
                                           operand2=source, ccRef=ccRef, opSize=opSize}::code)
                end

            |   optCode({instr as TestTagBit{arg, ccRef}, kill, ...} :: rest, _, regMap, code) =
                let
                    (* Replace register reference with memory.  In some circumstances it can try to
                       replace it with a constant.  Since we don't code-generate that case we
                       need to filter it out and retain the original register. *)
                    val (source, memRefsOut) = replaceWithValue(arg, kill, regMap, polyWordOpSize)
                    val resultInstr =
                        case source of
                            IntegerConstant _ => instr (* Use original *)
                        |   AddressConstant _ => instr
                        |   _ => TestTagBit{arg=source, ccRef=ccRef}
                in
                    (* This affects the CC. *)
                    optCode(rest, BRNone, memRefsOut, resultInstr::code)
                end

            |   optCode({instr=UntagFloat{source, dest, cache=_}, kill, ...} :: rest, _, regMap, code) =
                let
                    (* Replace register reference with memory if possible. *)
                    val (source, memRefsOut) = replaceWithValue(source, kill, regMap, polyWordOpSize)
                in
                    (* Not sure if this affects the CC but assume it might. *)
                    optCode(rest, BRNone, memRefsOut, UntagFloat{source=source, dest=dest, cache=NONE}::code)
                end

            |   optCode({instr, ...} :: rest, inCond, regMap, code) =
                let
                    (* If this instruction affects the CC the cached SetToCondition will no longer be valid. *)
                    val afterCond =
                        case getInstructionCC instr of
                            CCUnchanged => inCond
                        |   _ => BRNone
                in
                    optCode(rest, afterCond, regMap, instr::code)
                end

            val (blkCode, finalRepl, finalMap) = optCode(block, BRNone, [], processed)
        in
            case (flow, finalRepl) of
                (* We have a Condition and a change to the condition. *)
                (flow as Conditional{ccRef, condition, trueJump, falseJump},
                 BRSetConditionToConstant({srcCC, signedCompare, unsignedCompare, ...})) =>
                    if srcCC = ccRef
                    then
                    let
                        val testResult =
                            case (condition, signedCompare, unsignedCompare) of
                                (JE,    EQUAL,  _)   => true
                            |   (JE,    _,      _)   => false
                            |   (JNE,   EQUAL,  _)   => false
                            |   (JNE,   _,      _)   => true
                            |   (JL,    LESS,   _)   => true
                            |   (JL,    _,      _)   => false
                            |   (JG,    GREATER,_)   => true
                            |   (JG,    _,      _)   => false
                            |   (JLE,   GREATER,_)   => false
                            |   (JLE,   _,      _)   => true
                            |   (JGE,   LESS,   _)   => false
                            |   (JGE,   _,      _)   => true
                            |   (JB,    _, LESS  )   => true
                            |   (JB,    _,      _)   => false
                            |   (JA,    _,GREATER)   => true
                            |   (JA,    _,      _)   => false
                            |   (JNA,   _,GREATER)   => false
                            |   (JNA,   _,      _)   => true
                            |   (JNB,   _, LESS  )   => false
                            |   (JNB,   _,      _)   => true
                                (* The overflow and parity checks should never occur. *)
                            |   _   => raise InternalError "getCondResult: comparison"

                        val newFlow =
                            if testResult
                            then Unconditional trueJump
                            else Unconditional falseJump

                        val() = hasChanged := true
                    in
                        BasicBlock{flow=newFlow, block=List.rev blkCode}
                    end
                    else BasicBlock{flow=flow, block=List.rev blkCode}
              
            |   (flow as Unconditional jmp, _) =>
                let
                    val ExtendedBasicBlock{block=targetBlck, locals, exports, flow=targetFlow, outCCState=targetCC, ...} =
                        Vector.sub(code, jmp)
                    (* If the target is empty or is simply one or more Resets or a Return we're
                       better off merging this in rather than doing the jump.  We allow a single
                       Load  e.g. when loading a constant or moving a register.
                       If we have a CompareLiteral and we're comparing with a register in the map
                       that has been set to a constant we include that because the comparison will
                       then be reduced to a constant. *)
                    fun isSimple([], _, _) = true
                    |   isSimple ({instr=ResetStackPtr _, ...} :: instrs, moves, regMap) = isSimple(instrs, moves, regMap)
                    |   isSimple ({instr=ReturnResultFromFunction _, ...} :: instrs, moves, regMap) = isSimple(instrs, moves, regMap)
                    |   isSimple ({instr=RaiseExceptionPacket _, ...} :: instrs, moves, regMap) = isSimple(instrs, moves, regMap)
                    |   isSimple ({instr=LoadArgument{source=RegisterArgument preg, dest, kind=Move64Bit}, ...} :: instrs, moves, regMap) =
                        let
                            (* We frequently have a move of the original register into a new register before the test. *)
                            val newMap =
                                case List.find(fn {dest, ... } => dest = preg) regMap of
                                    SOME {source, ...} => {dest=dest, source=source, opSize=OpSize64} :: regMap
                                |   NONE => regMap
                        in
                            moves = 0 andalso isSimple(instrs, moves+1, newMap)
                        end
                    |   isSimple ({instr=LoadArgument{source=RegisterArgument preg, dest, kind=Move32Bit}, ...} :: instrs, moves, regMap) =
                        let
                            (* We frequently have a move of the original register into a new register before the test. *)
                            val newMap =
                                case List.find(fn {dest, ... } => dest = preg) regMap of
                                    SOME {source, ...} => {dest=dest, source=source, opSize=OpSize32} :: regMap
                                |   NONE => regMap
                        in
                            moves = 0 andalso isSimple(instrs, moves+1, newMap)
                        end
                    |   isSimple ({instr=LoadArgument _, ...} :: instrs, moves, regMap) = moves = 0 andalso isSimple(instrs, moves+1, regMap)
                    |   isSimple ({instr=CompareLiteral{arg1=RegisterArgument preg, ...}, ...} :: instrs, moves, regMap) =
                        let
                            val isReplace = List.find(fn {dest, ... } => dest = preg) regMap
                        in
                            case isReplace of
                                SOME {source=IntegerConstant _, ...} => isSimple(instrs, moves, regMap)
                            |   _ => false
                        end
                    |   isSimple _ = false
 
                in
                    (* Merge trivial blocks.  This previously also tried to merge non-trivial blocks if
                       they only had one reference but this ends up duplicating non-trivial code.  If we
                       have a trivial block that has multiple references but is the only reference to
                       a non-trivial block we can merge the non-trivial block into it.  That would
                       be fine except that at the same time we may merge this trivial block elsewhere. *)
                    (* The restriction that a block must only export "merge" registers is unfortunate
                       but necessary to avoid the situation where a non-merge register is defined at
                       multiple points and cannot be pushed to the stack.  This really isn't an issue
                       with blocks with unconditional branches but there are cases where we have
                       successive tests of the same condition and that results in local registers
                       being defined and then exported.  This occurs in, for example,
                       fun f x = if x > "abcde" then "yes" else "no"; *)
                    if isSimple(targetBlck, 0, finalMap) andalso
                            List.all (fn i => Vector.sub(pregProps, i) = RegPropMultiple) (setToList exports)
                    then
                    let
                        (* Copy the block, creating new registers for the locals. *)
                        val localMap = List.map (fn r => (PReg r, newReg(Vector.sub(pregProps, r)))) (setToList locals)
                        fun mapReg r = case List.find (fn (s, _) => r = s) localMap of SOME(_, s) => s | NONE => r
                        fun mapIndex(MemIndex1 r) = MemIndex1(mapReg r)
                        |   mapIndex(MemIndex2 r) = MemIndex2(mapReg r)
                        |   mapIndex(MemIndex4 r) = MemIndex4(mapReg r)
                        |   mapIndex(MemIndex8 r) = MemIndex8(mapReg r)
                        |   mapIndex index        = index
                        fun mapArg(RegisterArgument r) = RegisterArgument(mapReg r)
                        |   mapArg(MemoryLocation{base, offset, index, ...}) =
                                MemoryLocation{base=mapReg base, offset=offset, index=mapIndex index, cache=NONE}
                        |   mapArg arg = arg
                        fun mapInstr(instr as ResetStackPtr _) = instr
                        |   mapInstr(ReturnResultFromFunction{results, numStackArgs}) =
                                ReturnResultFromFunction{results=List.map(fn(pr, r) => (mapReg pr, r)) results, numStackArgs=numStackArgs}
                        |   mapInstr(RaiseExceptionPacket{packetReg}) =
                                RaiseExceptionPacket{packetReg=mapReg packetReg}
                        |   mapInstr(LoadArgument{source, dest, kind}) =
                                LoadArgument{source=mapArg source, dest=mapReg dest, kind=kind}
                        |   mapInstr(CompareLiteral{arg1, arg2, opSize, ccRef}) =
                                CompareLiteral{arg1=mapArg arg1, arg2=arg2, opSize=opSize, ccRef=ccRef}
                        |   mapInstr _ = raise InternalError "mapInstr: other instruction"
                        fun mapRegNo i = case(mapReg(PReg i)) of PReg r => r
                        (* Map the instructions and the sets although we only use the kill set. *)
                        fun mapCode{instr, current, active, kill} =
                            {instr=mapInstr instr, current=listToSet(map mapRegNo (setToList current)),
                             active=listToSet(map mapRegNo (setToList active)), kill=listToSet(map mapRegNo (setToList kill))}
                    in
                        hasChanged := true;
                        optimiseBlock blkCode(map mapCode targetBlck, targetFlow, targetCC)
                    end
                    else BasicBlock{flow=flow, block=List.rev blkCode}
                end

            |   (flow, _) => BasicBlock{flow=flow, block=List.rev blkCode}
        end
        
        fun optBlck(ExtendedBasicBlock{block, flow, outCCState, ...}) = optimiseBlock [] (block, flow, outCCState)
        val resVector = Vector.map optBlck code
    in
        if !hasChanged
        then
        let
            val extraRegs = List.rev(! regList)
            val props =
                if null extraRegs
                then pregProps
                else Vector.concat[pregProps, Vector.fromList extraRegs]
        in
            Changed(resVector, props)
        end
        else Unchanged
    end

    structure Sharing =
    struct
        type extendedBasicBlock = extendedBasicBlock
        and basicBlock = basicBlock
        and regProperty = regProperty
        and optimise = optimise
    end
end;
