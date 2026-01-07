(*
    Copyright David C. J. Matthews 2016-17, 2020-1

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

functor X86ICodeTransform(
    structure X86ICode: X86ICODE
    structure Debug: DEBUG
    structure Codegen: X86ICODEGENERATE
    structure Allocate: X86ALLOCATEREGISTERS
    structure Identify: X86IDENTIFYREFERENCES
    structure ConflictSets: X86GETCONFLICTSET
    structure PushRegisters: X86PUSHREGISTERS
    structure Optimise: X86ICODEOPT
    structure Pretty: PRETTY
    structure IntSet: INTSET
    sharing X86ICode.Sharing = Codegen.Sharing = Allocate.Sharing = Identify.Sharing =
            ConflictSets.Sharing = PushRegisters.Sharing = IntSet = Optimise.Sharing
) : X86ICODETRANSFORM
=
struct
    open X86ICode
    open Address
    open Identify
    open ConflictSets
    open PushRegisters
    open IntSet
    open Codegen
    open Allocate
    open Optimise
    
    exception InternalError = Misc.InternalError
    
    (* Find out the registers that need to be pushed to the stack, if any.
       We include those marked as "must push" because we need to save them across a
       function call or handler and also any we need to push because the set of active
       registers is more than the number of general registers we have.  This second case
       involves choosing suitable registers and is a first attempt to check we have enough
       registers.  We can also get a failure in codeExtended when we actually allocate
       the registers. *)
    fun spillRegisters(identified: extendedBasicBlock vector, regStates: regState vector) =
    let
        val maxPRegs = Vector.length regStates
        val pushArray = Array.array(maxPRegs, false)

        (* Mark anything already marked as "must push" unless it's already on the stack *)
        local
            fun checkPush(i, {pushState=true, ...}) = Array.update(pushArray, i, true)
            |   checkPush _ = ()
        in
            val () = Vector.appi checkPush regStates
        end
        
        (* Make a list of all the active sets ignoring those marked to be pushed.
           Do that first because we need to know how many sets each register is in. *)
        local
            fun addToActive(r, l) =
            (
                case Vector.sub(regStates, r) of
                    {prop=RegPropStack _, ...} => l
                |   _ => if Array.sub(pushArray, r) then l else r :: l
            )
        in
            fun nowActive regs = List.foldl addToActive [] regs
        end
            fun getBlockSets(ExtendedBasicBlock{block, passThrough, ...}, sets) =
            let
                fun getSets({active, ...}, l) =
                let
                    val set = nowActive(setToList(union(active, passThrough)))
                in
                    if List.length set > nGenRegs
                    then set :: l
                    else l
                end
            in
                List.foldl getSets sets block
            end

            val activeSets = Vector.foldl getBlockSets [] identified

    in
        if null activeSets then ()
        else
        let
            (* See how many times each register appears in a set. *)
            val activeIn = Array.array(maxPRegs, 0)
            val () =
                List.app (fn regs => List.app(fn r => Array.update(activeIn, r, Array.sub(activeIn, r)+1)) regs) activeSets
            (* We want to choose the best registers to spill. *)
            fun spillSomeRegs activeSet =
            let
                (* We may have already marked some of these to push. *)
                val currentActive = nowActive activeSet
                val regCount = List.length currentActive
                fun addCosts r =
                let
                    val {active, refs, prop, ...} = Vector.sub(regStates, r)
                in
                    case prop of
                        RegPropUntagged => (r, ~1, ~1)
                    |   RegPropStack _ => (r, ~1, ~1)
                    |   RegPropMultiple => (r, ~1, ~1)
                    |   _ => (r, Array.sub(activeIn, r), if refs = 0 then 0 else Int.quot(active, refs))
                end
                val withCosts = List.map addCosts currentActive
                (* Order so that the earlier items are those that appear in more sets and
                   if items appear in the same number of sets those that are active
                   longer come earlier. *)
                fun compare (_, in1, a1)  (_, in2, a2) = if in1 > in2 then true else if in1 < in2 then false else a1 > a2
                val sorted = Misc.quickSort compare withCosts

                fun markAsPush([], _) = ()
                |   markAsPush((reg, _, _) :: regs, n) =
                    if n <= 0
                    then ()
                    else
                    let
                        val {prop, ...} = Vector.sub(regStates, reg)
                        val _ = case prop of RegPropStack _ => raise InternalError "markAsPush" | _ => ()
                    in
                        Array.update(pushArray, reg, true);
                        markAsPush(regs, n-1)
                    end
            in
                markAsPush(sorted, regCount-nGenRegs)
            end
        in
            List.app spillSomeRegs activeSets
        end;
        (* Return the vector showing those that must be pushed. *)
        Array.vector pushArray
    end
     
    type triple = {instr: x86ICode, current: intSet, active: intSet}

    fun codeICodeFunctionToX86{blocks, functionName, pregProps, ccCount, debugSwitches, resultClosure, profileObject, ...} =
    let
       (*val maxPRegs = Vector.length pregProps*)
        val icodeTabs = [8, 20, 60]
        val wantPrintCode = Debug.getParameter Debug.icodeTag debugSwitches
        
        fun printCode identifiedCode =
            (* Print the code before the transformation. *)
            let
                val printStream = Pretty.getSimplePrinter(debugSwitches, icodeTabs)
            in
                printStream(functionName ^ "\n");
                printICodeAbstract(identifiedCode, printStream);
                printStream "\n"
            end
        
        fun printConflicts(regStates: conflictState vector) =
            let
                val printStream = Pretty.getSimplePrinter(debugSwitches, icodeTabs)

                fun printRegs([], _) = ()
                |   printRegs(_, 0) = printStream "..."
                |   printRegs([i], _) = printStream(Int.toString i)
                |   printRegs(i::l, n) = (printStream(Int.toString i ^ ","); printRegs(l, n-1))
                
                fun printRegData(i, { conflicts, ... }) =
                (
                    printStream (Int.toString i ^ "\t");
                    printStream ("Conflicts="); printRegs(setToList conflicts, 20);
                    printStream "\n"
                )
            in
                Vector.appi printRegData regStates
            end

        fun printRegisters(regAlloc: reg vector) =
        let
            val printStream = Pretty.getSimplePrinter(debugSwitches, icodeTabs)
            fun printRegAlloc(i, reg) = printStream (Int.toString i ^ "\t=> " ^ regRepr reg ^ "\n");
        in
            Vector.appi printRegAlloc regAlloc
        end
        
        (* Limit the number of passes. *)
        val maxOptimisePasses = 30
        val maxTotalPasses = maxOptimisePasses + 40

        fun processCode(basicBlocks: basicBlock vector, pregProps: regProperty vector, maxStack, passes, optPasses) =
        let
            (* This should only require a few passes. *)
            val _ = passes < maxTotalPasses orelse raise InternalError "Too many passes"
            val () =
                if wantPrintCode
                then printCode basicBlocks
                else ()
            (* First pass - identify register use patterns *)
            val (identified, regStates) = identifyRegisters(basicBlocks, pregProps)
            (* Try optimising.  This may not do anything in which case we can continue with
               the original code otherwise we need to reprocess. *)
            val tryOpt =
                if optPasses < maxOptimisePasses
                then optimiseICode{code=identified, pregProps=pregProps, ccCount=ccCount, debugSwitches=debugSwitches}
                else Unchanged
        in
            case tryOpt of
                Changed (postOptimise, postOpProps) => processCode(postOptimise, postOpProps, maxStack, passes, optPasses+1)

            |   Unchanged =>
                let
                    val regsToSpill = spillRegisters(identified, regStates)
                    val needPhase2 = Vector.exists(fn t => t) regsToSpill
                    val (needPhase2, regsToSpill) =
                        if needPhase2 orelse passes <> 0 then (needPhase2, regsToSpill)
                        else (true, Vector.tabulate(Vector.length pregProps, fn _ => false))
                in
                    if needPhase2
                    then
                    let
                        (* Push those registers we need to.  This also adds and renumbers pregs
                           and may add labels. *)
                        val {code=postPushCode, pregProps=regPropsPhase2, maxStack=maxStackPhase2} =
                            addRegisterPushes{code=identified, pushVec=regsToSpill, pregProps=pregProps, firstPass=passes=0}
                    in
                        (* And reprocess. *)
                        processCode(postPushCode, regPropsPhase2, maxStackPhase2, passes+1, optPasses)
                    end
                    else
                    let
                        val maxPRegs = Vector.length regStates
                
                        (* If we have been unable to allocate a register we need to spill something.
                           Choose a single register from each conflict set.  Since we've already checked
                           that the active sets are small enough this is really only required to deal
                           with special requirements e.g. esi/edi in block moves. *)
                        fun spillFromConflictSets conflictSets =
                        let
                            val maxPRegs = Vector.length regStates
                            val pushArray = Array.array(maxPRegs, false)
                    
                            fun selectARegisterToSpill active =
                            let
                                val regsToPick = setToList active
                            in
                                (* If we have already marked one of these to be pushed we don't
                                   need to do anything here. *)
                                if List.exists (fn r => Array.sub(pushArray, r)) regsToPick
                                then ()
                                else (* Choose something to push. *)
                                let
                                    fun chooseReg([], bestReg, _) = bestReg
                                    |   chooseReg(reg::regs, bestReg, bestCost) =
                                        let
                                            val {active, refs, prop, ...} = Vector.sub(regStates, reg)
                                            val cost = if refs = 0 then 0 else Int.quot(active, refs)
                                        in
                                            case prop of
                                                RegPropStack _ => chooseReg(regs, bestReg, bestCost)
                                            |   RegPropCacheUntagged => reg (* Pick the first cache reg. *)
                                            |   RegPropCacheTagged => reg (* Pick the first cache reg. *)
                                            |   RegPropMultiple => (* Must not pick a merge reg *)
                                                    chooseReg(regs, bestReg, bestCost)
                                            |   _ =>
                                                if cost >= bestCost
                                                then chooseReg(regs, reg, active)
                                                else chooseReg(regs, bestReg, bestCost)
                                        end
                                    val choice = chooseReg(regsToPick, ~1, 0)
                                    val _ = choice >= 0 orelse raise InternalError "chooseReg"
                                in
                                    Array.update(pushArray, choice, true)
                                end
                            end
                    
                            val () = List.app selectARegisterToSpill conflictSets
                        in
                            Array.vector pushArray
                        end
                
                        (* Now get the conflict sets. *)
                        val conflictSets = getConflictStates(identified, maxPRegs)
                        local
                            fun mapFromExtended(ExtendedBasicBlock{block, flow, ...}) =
                                BasicBlock{block=List.map #instr block, flow=flow}
                        in
                            val () =
                                if wantPrintCode
                                then (printCode(Vector.map mapFromExtended identified); printConflicts conflictSets)
                                else ()
                        end
                    in
                        case allocateRegisters {blocks=identified, regStates=conflictSets, regProps=pregProps } of
                            AllocateSuccess allocatedRegs =>
                                (
                                    if wantPrintCode then printRegisters allocatedRegs else ();
                                    icodeToX86Code{blocks=identified, functionName=functionName, allocatedRegisters=allocatedRegs,
                                               stackRequired=maxStack, debugSwitches=debugSwitches, resultClosure=resultClosure,
                                               profileObject=profileObject}
                                )
                       |    AllocateFailure fails =>
                            let
                                val regsToSpill = spillFromConflictSets fails
                                val {code=postPushCode, pregProps=pregPropsPhase2, maxStack=maxStackPhase2} =
                                    addRegisterPushes{code=identified, pushVec=regsToSpill, pregProps=pregProps, firstPass=false}
                            in
                                processCode(postPushCode, pregPropsPhase2, maxStackPhase2, passes+1, optPasses)
                            end
                    end
            end
        end

    in
        processCode(blocks, pregProps, 0 (* Should include handlers and containers. *), 0, 0)
    end
    structure Sharing =
    struct
        type preg       = preg
        and reg         = reg
        and basicBlock  = basicBlock
        and regProperty = regProperty
        and closureRef  = closureRef
    end
end;
