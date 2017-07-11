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

functor X86ICodeTransform(
    structure ICODE: ICodeSig
    structure DEBUG: DEBUGSIG
    structure IDENTIFY: X86IDENTIFYREFSSIG
    structure PRETTY: PRETTYSIG
    structure STRONGLY:
        sig
            type node
            val stronglyConnectedComponents: node list -> node list list
        end
        where type node = {src: ICODE.reg ICODE.argument, dst: ICODE.destinations}
    sharing ICODE.Sharing = IDENTIFY.Sharing
) : X86ICODETRANSFORMSIG
=
struct
    open ICODE
    open Address
    open IDENTIFY
    
    exception InternalError = Misc.InternalError

    (* tag a short constant *)
    fun tag c = 2 * c + 1

    val raxAsArg = (GenReg eax)
    and rcxAsArg = (GenReg ecx)
    
    val generalRegisters =
        List.map GenReg
        (if isX64
        then [r14, r13, r12, r11, r10, r9, r8, edi, esi, edx, ecx, ebx, eax]
        else [edi, esi, edx, ecx, ebx, eax])
    
    val floatingPtRegisters =
        case fpMode of
            FPModeSSE2 => List.map XMMReg [xmm6, xmm5, xmm4, xmm3, xmm2, xmm1, xmm0]
        |   FPModeX87 => List.map FPReg [fp0, fp1, fp2, fp3, fp4, fp5, fp6, fp7]

    (* Generally we have an offset in words and no index register. *)
    fun wordOffsetAddress(offset, baseReg) =
        MemoryLocation{offset=offset*wordSize, base=(GenReg baseReg), index=NoMemIndex}

    fun addRegisterPushes(code, pushVec: bool vector, maxPRegs, maxLabels) =
    let
        val labelCounter = ref maxLabels
        val pregCounter = ref 0
        val maxStack = ref 0
        
        (* Each preg in the input is mapped to either a new preg or the stack. *)
        datatype pregMapType = Unset | ToPReg of abstract | ToStack
        val pregMap = Array.array(maxPRegs, Unset)
        
        
        (* The stack contains both entries in the input code and entries added here. *)
        datatype stackEntry =
            NewEntry of {pregNo: int}
        |   ContainerEntry of { size: int }
        |   HandlerEntry
        
        type context =
        {
            oldCount: int,      (* The stack entries from the input code. *)
            newCount: int,      (* The total number of items on the stack. *)
            stack: stackEntry list
        }

        val labelState = Array.array(maxLabels, NONE: context option)

        val emptyContext: context = { oldCount=0, newCount=0, stack=[]}

        fun newPReg kind = PReg(!pregCounter before pregCounter := !pregCounter + 1, kind)
        
        fun getStackOffset(regNo: int, {stack, newCount, ...}: context) =
        let
            fun findEntry(offset, NewEntry {pregNo} :: tl) =
                if regNo = pregNo
                then offset
                else findEntry(offset+1, tl)
            |   findEntry(offset, ContainerEntry {size} :: tl) = findEntry(offset+size, tl)
            |   findEntry(offset, HandlerEntry :: tl) = findEntry(offset+2, tl) (* Occupies two words *)
            |   findEntry(_, []) = raise InternalError "findEntry - not found"
            val stackOffset = findEntry(0, stack)
            val _ = stackOffset < newCount orelse raise InternalError "getStackOffset - bad offset"
        in
            stackOffset
        end
        
        (* Adjust a stack offset from the old state to the new state. *)
        fun mapOldStackOffset(offset, {oldCount, newCount, stack, ...}: context) =
        let
            fun adjustStack(old, new, []) =
                let
                    val _ = old = offset-oldCount orelse raise InternalError "mapOldStackOffset - old"
                    val _ = new = newCount orelse raise InternalError "mapOldStackOffset - new"
                in
                    old+new
                end
            |   adjustStack(old, new, entry :: tl) =
                let
                    val _ = old < 0 andalso raise InternalError "adjustStack"
                in
                    case entry of
                        NewEntry _ => adjustStack(old, new+1, tl)
                    |   ContainerEntry {size} =>
                            if old = 0
                            then new
                            else adjustStack(old-size, new+size, tl)
                    |   HandlerEntry =>
                            if old = 0
                            then new
                            else adjustStack(old-2, new+2, tl)
                end
        in
            adjustStack(offset, 0, stack)
        end
        
        (* Map a source register.  This always loads the argument. *)
        fun mapSrcReg(PReg(n, _), context: context) =
            case Array.sub(pregMap, n) of
                Unset => raise InternalError "mapSrcReg - unset"
            |   ToPReg preg => (preg, [])
            |   ToStack =>
                let
                    (* Make a new untagged register. That will prevent us pushing it if
                       we have to spill registers. *)
                    val newReg = newPReg PRegUntagged
                in
                    (newReg,
                        [LoadArgument{source=StackLocation{wordOffset=getStackOffset(n, context), adjustment=0}, dest=newReg, kind=MoveWord}])
                end
       
        fun mapDestReg(PReg(n, kind), context: context as {stack, oldCount, newCount}) =
        let
            val currentLocation = Array.sub(pregMap, n)
        in
            if Vector.sub(pushVec, n)
            then
                let
                    (* This should not have been seen before. *)
                    val _ = case currentLocation of Unset => () | _ => raise InternalError "mapDestReg - already set"
                    val newReg = newPReg kind
                    val () = Array.update(pregMap, n, ToStack)
                    val newContext = { stack = NewEntry{pregNo=n}::stack, oldCount=oldCount, newCount=newCount+1}
                    val () = maxStack := Int.max(!maxStack, newCount+1)
                in
                    (newReg, [PushValue{arg=RegisterArgument newReg}], newContext)
                end
            else
                let
                (* See if we already have a number for it.  We may encounter the same preg
                   as a destination when returning the result from a conditional in which
                   case we have to use the same number.  We shouldn't have pushed it. *)
                    val newReg =
                        case currentLocation of
                            Unset =>
                            let
                                val newReg = newPReg kind
                                val () = Array.update(pregMap, n, ToPReg newReg)
                            in
                                newReg
                            end
                        |   ToPReg preg => preg
                        |   ToStack => raise InternalError "mapDestReg - already on stack"
                in
                    (newReg, [], context)
                end
        end
        
        (* A work register must be a normal register. *)
        fun mapWorkReg(PReg(n, kind)) =
        let
            val currentLocation = Array.sub(pregMap, n)
            val _ = Vector.sub(pushVec, n) andalso raise InternalError "mapWorkReg - MustPush"
        in
            case currentLocation of
                Unset =>
                let
                    val newReg = newPReg kind
                    val () = Array.update(pregMap, n, ToPReg newReg)
                in
                    newReg
                end
            |   ToPReg preg => preg
            |   ToStack => raise InternalError "mapWorkReg - on stack"
                
        end

        fun mapIndex(NoMemIndex, _) = (NoMemIndex, [])
        |   mapIndex(MemIndex1 r, context) =
                let val (sreg, c) = mapSrcReg(r, context) in (MemIndex1 sreg, c) end
        |   mapIndex(MemIndex2 r, context) =
                let val (sreg, c) = mapSrcReg(r, context) in (MemIndex2 sreg, c) end
        |   mapIndex(MemIndex4 r, context) =
                let val (sreg, c) = mapSrcReg(r, context) in (MemIndex4 sreg, c) end
        |   mapIndex(MemIndex8 r, context) =
                let val (sreg, c) = mapSrcReg(r, context) in (MemIndex8 sreg, c) end

        fun mapSource(RegisterArgument(PReg(r, _)), context: context) =
            (
                case Array.sub(pregMap, r) of
                    Unset => raise InternalError "mapSrcReg - unset"
                |   ToPReg preg => (RegisterArgument preg, [])
                |   ToStack => (StackLocation{wordOffset=getStackOffset(r, context), adjustment=0}, [])
            )

        |   mapSource(a as AddressConstant _, _) = (a, [])
        |   mapSource(i as IntegerConstant _, _) = (i, [])

        |   mapSource(MemoryLocation{base, offset, index}, context) =
            let
                val (baseReg, baseCode) = mapSrcReg(base, context)
                val (indexValue, indexCode) = mapIndex(index, context)
            in
                (MemoryLocation{base=baseReg, offset=offset, index=indexValue}, baseCode @ indexCode)
            end

        |   mapSource(StackLocation{wordOffset, adjustment}, context) =
                (StackLocation{wordOffset=mapOldStackOffset(wordOffset+adjustment, context), adjustment=0}, [])

        (* If the argument AFTER processing is a stack entry, then load it. *)
        fun loadStack(stack as StackLocation _) =
            let
                val newReg = newPReg PRegUntagged
            in
                (RegisterArgument newReg, [LoadArgument{source=stack, dest=newReg, kind=MoveWord}])
            end
        |   loadStack other = (other, [])


        local
            (* We don't need to save registers if we've pushed them.
               N.B.  If we ever cache registers we'll need to clear the cache. *)
            fun mapSaveReg (PReg(n, _), l) =
                case Array.sub(pregMap, n) of
                    Unset => raise InternalError "mapSaveReg - unset"
                |   ToPReg preg => preg :: l
                |   ToStack => l
        in
            val mapSaveRegs = List.foldl mapSaveReg [] 
        end
        
        (* Record the current state against the label.  In almost all cases the label
           will not previously have been set.  The one exception is that floating
           point comparisons sometimes have different jumps to check the parity bit
           and the zero bit. The context will be the same for both so it's safe. *)
        fun addNewLabel(ILabel lab, context) = Array.update(labelState, lab, SOME context)
        
        (* Rewrite the code, replacing any registers that need to be pushed with references to
           the stack.  The result is built up in reverse order and then reversed. *)
        fun pushRegisters([]: (abstract x86ICode * int list) list, code: abstract x86ICode list, _: context) = code
        
        |   pushRegisters((LoadArgument{source, dest=PReg(dReg, dKind), kind}, _) :: rest, code, context as {stack, oldCount, newCount}) =
            let
                val (sourceVal, sourceCode) = mapSource(source, context)
                (* If we have to push the value we don't have to first load it into a register. *)
                val currentLocation = Array.sub(pregMap, dReg)

                val (destCode, newContext) =
                    if Vector.sub(pushVec, dReg)
                    then
                        let
                            val _ = case currentLocation of Unset => () | _ => raise InternalError "LoadArgument - already set"
                            val () = Array.update(pregMap, dReg, ToStack)
                            val newContext = { stack = NewEntry{pregNo=dReg}::stack, oldCount=oldCount, newCount=newCount+1}
                            val () = maxStack := Int.max(!maxStack, newCount+1)
                        in
                            (PushValue{arg=sourceVal}, newContext)
                        end
                    else
                        let
                            val destReg =
                                case currentLocation of
                                    Unset =>
                                    let
                                        val newReg = newPReg dKind
                                        val () = Array.update(pregMap, dReg, ToPReg newReg)
                                    in
                                        newReg
                                    end
                                |   ToPReg preg => preg
                                |   ToStack => raise InternalError "LoadArgument - already on stack"
                        in
                            (LoadArgument{source=sourceVal, dest=destReg, kind=kind}, context)
                        end
            in
                pushRegisters(rest, destCode  :: sourceCode @ code, newContext)
            end

        |   pushRegisters((StoreArgument{source, offset, base, index, kind}, _) :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSource(source, context)
                (* We can't have a memory-memory store so we have to load the source if it's now the stack. *)
                val (loadedSource, loadCode) = loadStack sourceVal
                val (baseReg, baseCode) = mapSrcReg(base, context)
                val (indexValue, indexCode) = mapIndex(index, context)
            in
                pushRegisters(rest,
                    StoreArgument{source=loadedSource, base=baseReg, offset=offset, index=indexValue, kind=kind} ::
                        indexCode @ baseCode @ loadCode @ sourceCode @ code, context)
            end

        |   pushRegisters((LoadMemReg { offset, dest}, _) :: rest, code, context) =
            let
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ LoadMemReg { offset=offset, dest=destVal} :: code, newContext)
            end

        |   pushRegisters((ExchangeRegisters _, _) :: _, _, _) = (* This is added at the next level. *)
                raise InternalError "pushRegisters - ExchangeRegisters"

        |   pushRegisters((BeginFunction {regArgs}, _) :: rest, code, context) =
            let
                (* Push any registers that need to be pushed. *)
                fun pushReg((preg, rreg), (others, code, context)) =
                let
                    val (newReg, newCode, newContext) = mapDestReg(preg, context)
                in
                    ((newReg, rreg) :: others, newCode @ code, newContext)
                end
                val (newRegArgs, pushCode, newContext) = List.foldl pushReg ([], [], context) regArgs
            in
                pushRegisters(rest,
                    pushCode @ (BeginFunction {regArgs=newRegArgs} :: code), newContext)
            end

        |   pushRegisters((FunctionCall{callKind, regArgs, stackArgs, dest}, _) :: rest, code, context) =
            let
                (* It's possible that this could lead to having to spill registers in order
                   to load others.  Leave that problem for the moment. *)
                fun loadStackArg (arg, (otherLoads, otherArgs)) =
                let
                    val (argVal, loadCode) = mapSource(arg, context)
                in
                    (loadCode @ otherLoads, argVal :: otherArgs)
                end
                val (stackArgLoads, newStackArgs) = List.foldr loadStackArg ([], []) stackArgs

                fun loadRegArg ((arg, reg), (otherLoads, otherArgs)) =
                let
                    val (argVal, loadCode) = mapSource(arg, context)
                in
                    (loadCode @ otherLoads, (argVal, reg) :: otherArgs)
                end
                val (regArgLoads, newRegArgs) = List.foldr loadRegArg ([], []) regArgs
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @
                        FunctionCall{ callKind=callKind, regArgs=newRegArgs, stackArgs=newStackArgs, dest=destVal} ::
                            regArgLoads @ stackArgLoads @ code, newContext)
            end

        |   pushRegisters((TailRecursiveCall{callKind, regArgs, stackArgs, returnAddr={srcStack, stack}, stackAdjust}, _) :: rest, code,
                          context as {oldCount, newCount, ...}) =
            let
                (* We have to adjust all the stack offsets to account for anything we've pushed.
                   Note: we mustn't use mapOldStackOffset for destination stack offsets.
                   These are not actual source stack items but overwrite the stack. *)
                fun loadStackArg ({src, stack}, (otherLoads, otherArgs)) =
                let
                    val (argVal, loadCode) = mapSource(src, context)
                in
                    (loadCode @ otherLoads, {src=argVal, stack=stack+newCount-oldCount} :: otherArgs)
                end
                val (stackArgLoads, newStackArgs) = List.foldr loadStackArg ([], []) stackArgs
                
                fun loadRegArg ((arg, reg), (otherLoads, otherArgs)) =
                let
                    val (argVal, loadCode) = mapSource(arg, context)
                in
                    (loadCode @ otherLoads, (argVal, reg) :: otherArgs)
                end
                val (regArgLoads, newRegArgs) = List.foldr loadRegArg ([], []) regArgs

                val returnAddr = {srcStack=mapOldStackOffset(srcStack, context), stack=stack+newCount-oldCount}
                (* Because we're adjusting to the base of the stack we don't need to look at what we've
                   actually pushed. *) 
                val newStackAdjust = stackAdjust+newCount-oldCount
            in
                skipToLabel(rest,
                    TailRecursiveCall{ callKind=callKind, regArgs=newRegArgs,
                        stackArgs=newStackArgs, returnAddr=returnAddr, stackAdjust=newStackAdjust} ::
                            regArgLoads @ stackArgLoads @ code)
            end

        |   pushRegisters((AllocateMemoryOperation{size, flags, dest, saveRegs}, _) :: rest, code, context) =
            let
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ AllocateMemoryOperation{size=size, flags=flags, dest=destVal, saveRegs=mapSaveRegs saveRegs} :: code, newContext)
            end

        |   pushRegisters((AllocateMemoryVariable{size, dest, saveRegs}, _) :: rest, code, context) =
            let
                val (sizeVal, sizeCode) = mapSrcReg(size, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ AllocateMemoryVariable{size=sizeVal, dest=destVal, saveRegs=mapSaveRegs saveRegs} :: sizeCode @ code, newContext)
            end

        |   pushRegisters((InitialiseMem{size, addr, init}, _) :: rest, code, context) =
            let
                val (sizeVal, sizeCode) = mapSrcReg(size, context)
                val (addrVal, addrCode) = mapSrcReg(addr, context)
                val (initVal, initCode) = mapSrcReg(init, context)
            in
                pushRegisters(rest,
                    InitialiseMem{size=sizeVal, addr=addrVal, init=initVal} :: initCode @ addrCode @ sizeCode @ code, context)
            end

        |   pushRegisters((InitialisationComplete, _) :: rest, code, context) =
                pushRegisters(rest, InitialisationComplete :: code, context)

        |   pushRegisters((StartLoop{regArgs, loopLabel}, _) :: rest, code, context) =
            let
                (* Some of the loop registers may be marked to be pushed.  In that case we
                   don't include them in the regArgs list.  We have to split the list between
                   those that will be pushed and those that won't and process the ones that
                   will be pushed first. *)
                fun isAregArg (_, PReg(n, _)) = not(Vector.sub(pushVec, n))
                val (regArgsToDo, stackArgsToDo) = List.partition isAregArg regArgs

                fun loadStackArgs ([], ctxt) = ([], ctxt)
                |   loadStackArgs ((source, dest as PReg(n, _)) :: rest, ctxt) =
                    let
                        val (sourceVal, sourceCode) = mapSource(source, ctxt)
                        val (destVal, destCode, newContext) = mapDestReg(dest, ctxt)
                        val (codeRest, resContext) = loadStackArgs(rest, newContext)
                        val _ =
                            case Array.sub(pregMap, n) of
                                ToStack => () | _ => raise InternalError "pushRegisters - StartLoop - not stack"
                    in
                        (codeRest @ destCode @ [LoadArgument{source=sourceVal, dest=destVal, kind=MoveWord}] @ sourceCode, resContext)
                    end
                val (loadStackArgCode, stackArgContext) = loadStackArgs(stackArgsToDo, context)
                
                fun loadRegArgs ([], ctxt) = ([], [], ctxt)
                |   loadRegArgs ((source, dest as PReg(n, _)) :: rest, ctxt) =
                    let
                        val (sourceVal, sourceCode) = mapSource(source, ctxt)
                        val (_, destCode, newContext) = mapDestReg(dest, ctxt)
                        val (codeRest, otherRegArgs, resContext) = loadRegArgs(rest, newContext)
                    in
                        case Array.sub(pregMap, n) of
                            ToPReg newReg =>
                                (codeRest @ destCode @ sourceCode, (sourceVal, newReg) :: otherRegArgs, resContext)
                        |   _ => raise InternalError "pushRegisters - StartLoop - not reg"
                    end
                val (loadCode, newRegArgs, newContext) = loadRegArgs(regArgsToDo, stackArgContext)

                (* Set up the state.  Every jump has to reset to this.  Only the
                   stack pointer is needed. *)
                val () = addNewLabel(loopLabel, newContext)
            in
                pushRegisters(rest,
                    StartLoop{regArgs=newRegArgs, loopLabel=loopLabel} :: loadCode @ loadStackArgCode @ code, newContext)
            end

        |   pushRegisters((EndLoop{loopLabel, staticRegs}, _) :: rest, code, context) =
                (* We don't really need to map the static regs since we'll call "Identify" again after this. *)
                pushRegisters(rest, EndLoop{loopLabel=loopLabel, staticRegs=mapSaveRegs staticRegs} :: code, context)

        |   pushRegisters((JumpLoop{regArgs, stackArgs, loopLabel as ILabel lab, ...}, _) :: rest, code, context as {newCount, ...}) =
            let
                val {newCount=labNewCount, ...} = valOf(Array.sub(labelState, lab))
                fun getValues [] = ([], [], [])
                |   getValues ((source, PReg(n, _)) :: rest) =
                    let
                        val (otherRegArgs, otherStackArgs, otherCode) = getValues rest
                        val (sourceVal, sourceCode) = mapSource(source, context)
                    in
                        case Array.sub(pregMap, n) of
                            ToPReg lReg => ((sourceVal, lReg) :: otherRegArgs, otherStackArgs, sourceCode @ otherCode)
                        |   ToStack =>
                            let
                                val stackOff = getStackOffset(n, context)
                            in
                                (otherRegArgs, (sourceVal, stackOff) :: otherStackArgs, sourceCode @ otherCode)
                            end
                        |   Unset => raise InternalError "pushRegisters - JumpLoop - unset"
                    end
                val (newRegArguments, newStackArgs, sourceCode) = getValues regArgs
                fun loadStackArg((source, destStack), (otherLoads, otherArgs)) =
                let
                    val (sourceVal, sourceCode) = mapSource(source, context)
                in
                    (sourceCode @ otherLoads, (sourceVal, mapOldStackOffset(destStack, context)) :: otherArgs)
                end
                
                val (stackArgLoads, oldStackArgs) = List.foldr loadStackArg ([], []) stackArgs
                val newStackAdjust = newCount-labNewCount
            in
                skipToLabel(rest,
                    JumpLoop{ regArgs=newRegArguments, stackArgs=oldStackArgs @ newStackArgs, loopLabel=loopLabel,
                              stackAdjust=newStackAdjust} :: sourceCode @ stackArgLoads @ code)
            end

        |   pushRegisters((RaiseExceptionPacket{packet}, _) :: rest, code, context) =
            let
                val (packetVal, packetCode) = mapSource(packet, context)
            in
                skipToLabel(rest, RaiseExceptionPacket{packet=packetVal} :: packetCode @ code)
            end

        |   pushRegisters((ReserveContainer{size, address}, _) :: rest, code, {oldCount, newCount, stack}) =
            let
                val containerContext =
                    {oldCount=oldCount+size, newCount=newCount+size, stack=ContainerEntry{size=size}::stack}
                val (destVal, destCode, newContext) = mapDestReg(address, containerContext)
                val () = maxStack := Int.max(!maxStack, newCount+size)
            in
                pushRegisters(rest, destCode @ ReserveContainer{size=size, address=destVal} :: code, newContext)
            end

        |   pushRegisters((IndexedCaseOperation{testReg, workReg, cases, startValue}, _) :: rest, code, context) =
            let
                val (srcVal, srcCode) = mapSrcReg(testReg, context)
                val newWorkReg = mapWorkReg workReg
                (* Set the incoming state for each of the cases. *)
                val () = List.app (fn caseLab => addNewLabel(caseLab, context)) cases
            in
                (* This is an unconditional branch. *)
                skipToLabel(rest, IndexedCaseOperation{testReg=srcVal, workReg=newWorkReg, cases=cases, startValue=startValue} :: srcCode @ code)
            end

        |   pushRegisters((LockMutable{addr}, _) :: rest, code, context) =
            let
                val (addrVal, addrCode) = mapSrcReg(addr, context)
            in
                pushRegisters(rest, LockMutable{addr=addrVal} :: addrCode @ code, context)
            end
    
        |   pushRegisters((ForwardJumpLabel{label as ILabel lab, result}, _) :: rest, code,
                          context as {newCount=currentNewCount, stack=currentStack, ...}) =
            (* This is a forward jump label after other code.  We have to merge the states. *)
            (
                (* Get the state.  The label may never have been jumped to. *)
                case Array.sub(labelState, lab) of
                    NONE => pushRegisters(rest, code, context)
                |   SOME {newCount=jumpNewCount, stack=jumpStack, ...} =>
                    let
                        (* Merge the common entries. Stop as soon as we find a difference. *)
                        fun matchStacks((entry as NewEntry {pregNo=reg1}) :: tl1, NewEntry {pregNo=reg2} :: tl2, combined, old, new) =
                            if reg1 = reg2
                            then matchStacks(tl1, tl2, entry :: combined, old, new+1)
                            else {newCount=new, oldCount=old, stack=combined}

                        |   matchStacks((entry as ContainerEntry {size=size1}) :: tl1, ContainerEntry {size=size2} :: tl2, combined, old, new) =
                            if size1 = size2
                            then matchStacks(tl1, tl2, entry :: combined, old+size1, new+size1)
                            else {newCount=new, oldCount=old, stack=combined}
                            
                        |   matchStacks(HandlerEntry :: tl1, HandlerEntry :: tl2, combined, old, new) =
                                matchStacks(tl1, tl2, HandlerEntry :: combined, old+2, new+2)

                        |   matchStacks(_, _, combined, old, new) = {newCount=new, oldCount=old, stack=combined}

                        val newContext as {newCount=newSp, ...} = matchStacks(List.rev currentStack, List.rev jumpStack, [], 0, 0)

                        val adjustStack =
                            if newSp = currentNewCount
                            then []
                            else [ResetStackPtr{numWords=currentNewCount-newSp}]
                        (* If there is a result register it should not have been pushed to the stack. *)
                        val resultReg =
                            case result of
                                NONE => NONE
                            |   SOME(PReg(dReg, _)) =>
                                    case Array.sub(pregMap, dReg) of
                                        ToPReg preg => SOME preg
                                    |   _ => raise InternalError "pushRegisters merge register"
                        val resultCode =
                            if newSp = jumpNewCount
                            then (* We don't need to do anything to the incoming branch. *)
                                ForwardJumpLabel{label=label, result=resultReg} :: adjustStack  @ code
                            else (* We're going to have to adjust the stack pointer on the incoming branch.
                                    This requires an extra label and branch. *)
                            let
                                val extraLabel = ILabel(! labelCounter) before labelCounter := !labelCounter+1
                           in
                                ForwardJumpLabel{label=extraLabel, result=resultReg} ::
                                ResetStackPtr{numWords=jumpNewCount-newSp} ::
                                ForwardJumpLabel{label=label, result=resultReg} :: (* Come in here. *)
                                UnconditionalForwardJump{label=extraLabel} :: adjustStack  @ code
                            end
                    in
                        pushRegisters(rest, resultCode, newContext)
                    end
            )

        |   pushRegisters((UnconditionalForwardJump{label}, _) :: rest, code, context) =
            (
                (* Save the state.  Then enter the "unconditional transfer" state. *)
                addNewLabel(label, context);
                skipToLabel(rest, UnconditionalForwardJump{label=label} :: code)
            )

        |   pushRegisters((ConditionalForwardJump{condition, label, ccRef}, _) :: rest, code, context) =
            (
                (* Save the state but continue. *)
                addNewLabel(label, context);
                pushRegisters(rest, ConditionalForwardJump{condition=condition, label=label, ccRef=ccRef} :: code, context)
            )

        |   pushRegisters((WordComparison{arg1, arg2, ccRef}, _) :: rest, code, context) =
            let
                (* codeExtended expects the first argument to be in a register.
                   We may be able to do something clever if the second is actually in the
                   register but for the moment just force a load. *)
                val (op1Val, op1Code) = mapSource(arg1, context)
                val (loadedOp1, loadCode) = loadStack op1Val
                val (op2Val, op2Code) = mapSource(arg2, context)
            in
                pushRegisters(rest, WordComparison{arg1=loadedOp1, arg2=op2Val, ccRef=ccRef} :: op2Code @ loadCode @ op1Code @ code, context)
            end

        |   pushRegisters((PushExceptionHandler{workReg, handleStart}, _) :: rest, code, context as {oldCount, newCount, stack}) =
            let
                (* Save the state. This is a bit like a conditional jump *)
                val () = addNewLabel(handleStart, context)
                val newWorkReg = mapWorkReg workReg
                (* Add a handler entry to the stack. *)
                val newContext = {oldCount=oldCount+2, newCount=newCount+2, stack=HandlerEntry :: stack}
                val () = maxStack := Int.max(!maxStack, newCount+2)
            in
                pushRegisters(rest, PushExceptionHandler{workReg=newWorkReg, handleStart=handleStart} :: code, newContext)
            end

        |   pushRegisters((PopExceptionHandler{workReg, ...}, _) :: rest, code, {oldCount, newCount, stack}) =
            let
                val newWorkReg = mapWorkReg workReg
                (* Get the state after removing the handler. *)
                fun popContext ([], _, _) = raise InternalError "pushRegisters - pop handler"
                |   popContext (HandlerEntry :: tl, old, new) = {oldCount=old-2, newCount=new-2, stack=tl}
                |   popContext (ContainerEntry _ :: _, _, _) = raise InternalError "pushRegisters - pop handler - container"
                |   popContext (NewEntry _ :: tl, old, new) = popContext(tl, old, new-1)
                val newContext as { newCount=newNewCount, ...} = popContext(stack, oldCount, newCount)
                (* Reset the stack to just above the two words of the handler. *)
                val resetCode =
                    if newCount <> newNewCount+2
                    then [ResetStackPtr{numWords=newCount-newNewCount-2}]
                    else []
            in
                pushRegisters(rest, PopExceptionHandler{resultReg=NONE, workReg=newWorkReg} :: resetCode @ code, newContext)
            end

        |   pushRegisters((BeginHandler _, _) :: _, _, _) =
                (* This should only ever happen after an unconditional jump. *)
                raise InternalError "pushRegisters - BeginHandler"

        |   pushRegisters((ReturnResultFromFunction{resultReg, numStackArgs}, _) :: rest, code, context as {newCount, ...}) =
            let
                val (resultValue, loadResult) = mapSrcReg(resultReg, context)
                val resetCode =
                    if newCount = 0 then [] else [ResetStackPtr{numWords=newCount}]
            in
                skipToLabel(rest,
                    ReturnResultFromFunction{resultReg=resultValue, numStackArgs=numStackArgs} :: resetCode @ loadResult @ code)
            end

        |   pushRegisters((ArithmeticFunction{oper, resultReg, operand1, operand2, ccRef}, _) :: rest, code, context) =
            let
                (* codeExtended expects the first argument to be in a register.
                   We may be able to do something clever if the second is actually in the
                   register but for the moment just force a load. *)
                val (op1Val, op1Code) = mapSource(operand1, context)
                val (loadedOp1, loadCode) = loadStack op1Val
                val (op2Val, op2Code) = mapSource(operand2, context)
                val (destVal, destCode, newContext) = mapDestReg(resultReg, context)
            in
                pushRegisters(rest,
                    destCode @ ArithmeticFunction{oper=oper, resultReg=destVal, operand1=loadedOp1, operand2=op2Val, ccRef=ccRef} ::
                        op2Code @ loadCode @ op1Code @ code, newContext)
            end

        |   pushRegisters((TestTagBit{arg, ccRef}, _) :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSource(arg, context)
            in
                pushRegisters(rest, TestTagBit{arg=sourceVal, ccRef=ccRef} :: sourceCode @ code, context)
            end

        |   pushRegisters((PushValue{arg}, _) :: rest, code, context as {oldCount, newCount, stack}) =
            let
                val (sourceVal, sourceCode) = mapSource(arg, context)
                (* This was a push from a previous pass.  Treat as a container of size 1. *)
                val pushContext =
                    {oldCount=oldCount+1, newCount=newCount+1, stack=ContainerEntry{size=1}::stack}
            in
                pushRegisters(rest, PushValue{arg=sourceVal} :: sourceCode @ code, pushContext)
            end

        |   pushRegisters((ResetStackPtr{numWords}, _) :: rest, code, context as {newCount=oldSp, ...}) =
            (* This indicates that an old container entry is no longer required.  Remove it and anything
               else we've pushed since.  Because of the nesting anything pushed more recently should
               now be free. *)
            let
                fun removeItems(0, state) = state
                |   removeItems(_, {stack=[], ...}) = raise InternalError "removeItems - stack empty"
                |   removeItems(toRemove, {stack=ContainerEntry {size} :: tl, oldCount, newCount}) =
                        if toRemove < size then raise InternalError "removeItems - container size"
                        else removeItems(toRemove-size, {stack=tl, oldCount=oldCount-size, newCount=newCount-size})
                |   removeItems(_, {stack=HandlerEntry :: _, ...}) = raise InternalError "removeItems - handler"
                |   removeItems(toRemove, {stack=NewEntry _ :: tl, oldCount, newCount}) =
                        removeItems(toRemove, {stack=tl, oldCount=oldCount, newCount=newCount-1})
                val newContext as {newCount=newSp, ...} = removeItems(numWords, context)
            in
                pushRegisters(rest, ResetStackPtr{numWords=oldSp-newSp} :: code, newContext)
            end

        |   pushRegisters((TagValue{source, dest}, _) :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSrcReg(source, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest, destCode @ TagValue{source=sourceVal, dest=destVal} :: sourceCode @ code, newContext)
            end

        |   pushRegisters((UntagValue{source, dest, isSigned}, _) :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSource(source, context)
                val (loadedSource, loadCode) = loadStack sourceVal
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ UntagValue{source=loadedSource, dest=destVal, isSigned=isSigned} :: loadCode @ sourceCode @ code, newContext)
            end

        |   pushRegisters((LoadEffectiveAddress{base, offset, index, dest}, _) :: rest, code, context) =
            let
                val (baseVal, baseCode) =
                    case base of
                        NONE => (NONE, [])
                    |   SOME bReg =>
                            let val (newBReg, regCode) = mapSrcReg(bReg, context) in (SOME newBReg, regCode) end
                val (indexVal, indexCode) = mapIndex(index, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ LoadEffectiveAddress{base=baseVal, offset=offset, index=indexVal, dest=destVal} :: indexCode @ baseCode @ code, newContext)
            end

        |   pushRegisters((ShiftOperation{shift, resultReg, operand, shiftAmount, ccRef}, _) :: rest, code, context) =
            let
                val (opVal, opCode) = mapSource(operand, context)
                val (shiftVal, shiftCode) = mapSource(shiftAmount, context)
                val (destVal, destCode, newContext) = mapDestReg(resultReg, context)
            in
                pushRegisters(rest,
                    destCode @ ShiftOperation{shift=shift, resultReg=destVal, operand=opVal, shiftAmount=shiftVal, ccRef=ccRef} ::
                        shiftCode @ opCode @ code, newContext)
            end

        |   pushRegisters((Multiplication{resultReg, operand1, operand2, ccRef}, _) :: rest, code, context) =
            let
                val (op1Val, op1Code) = mapSource(operand1, context)
                val (op2Val, op2Code) = mapSource(operand2, context)
                val (destVal, destCode, newContext) = mapDestReg(resultReg, context)
            in
                pushRegisters(rest,
                    destCode @ Multiplication{resultReg=destVal, operand1=op1Val, operand2=op2Val, ccRef=ccRef} :: op2Code @ op1Code @ code, newContext)
            end

        |   pushRegisters((Division{isSigned, dividend, divisor, quotient, remainder}, _) :: rest, code, context) =
            let
                val (dividendVal, dividendCode) = mapSrcReg(dividend, context)
                val (divisorVal, divisorCode) = mapSource(divisor, context)
                val (quotVal, quotCode, quotContext) = mapDestReg(quotient, context)
                val (remVal, remCode, newContext) = mapDestReg(remainder, quotContext)
            in
                pushRegisters(rest,
                    remCode @ quotCode @
                        Division{isSigned=isSigned, dividend=dividendVal, divisor=divisorVal, quotient=quotVal, remainder=remVal} ::
                            divisorCode @ dividendCode @ code, newContext)
            end

        |   pushRegisters((AtomicExchangeAndAdd{base, source}, _) :: rest, code, context) =
            let
                val (baseVal, baseCode) = mapSrcReg(base, context)
                val (sourceVal, sourceCode) = mapSrcReg(source, context)
                (* The "source" is also a result and must be in a register.  It's an untagged reg
                   so it shouldn't have been marked as to be pushed. *)
                val _ = case sourceCode of [] => () | _ => raise InternalError "pushRegisters - AtomicExchangeAndAdd"
            in
                pushRegisters(rest, AtomicExchangeAndAdd{base=baseVal, source=sourceVal} :: baseCode @ code, context)
            end

        |   pushRegisters((BoxValue{boxKind, source, dest, saveRegs}, _) :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSrcReg(source, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ BoxValue{boxKind=boxKind, source=sourceVal, dest=destVal, saveRegs=mapSaveRegs saveRegs} :: sourceCode @ code, newContext)
            end

        |   pushRegisters((CompareByteVectors{vec1Addr, vec2Addr, length, ccRef}, _) :: rest, code, context) =
            let
                val (vec1Val, vec1Code) = mapSrcReg(vec1Addr, context)
                val (vec2Val, vec2Code) = mapSrcReg(vec2Addr, context)
                val (lengthVal, lengthCode) = mapSrcReg(length, context)
            in
                pushRegisters(rest,
                    CompareByteVectors{vec1Addr=vec1Val, vec2Addr=vec2Val, length=lengthVal, ccRef=ccRef} ::
                        lengthCode @ vec2Code @ vec1Code @ code, context)
            end

        |   pushRegisters((BlockMove{srcAddr, destAddr, length, isByteMove}, _) :: rest, code, context) =
            let
                val (srcVal, srcCode) = mapSrcReg(srcAddr, context)
                val (destVal, destCode) = mapSrcReg(destAddr, context)
                val (lengthVal, lengthCode) = mapSrcReg(length, context)
            in
                pushRegisters(rest,
                    BlockMove{srcAddr=srcVal, destAddr=destVal, length=lengthVal, isByteMove=isByteMove} ::
                        lengthCode @ destCode @ srcCode @ code, context)
            end

        |   pushRegisters((CompareFloatingPt{arg1, arg2, ccRef}, _) :: rest, code, context) =
            let
                val (arg1Val, arg1Code) = mapSource(arg1, context)
                val (arg2Val, arg2Code) = mapSource(arg2, context)
            in
                pushRegisters(rest, CompareFloatingPt{arg1=arg1Val, arg2=arg2Val, ccRef=ccRef} :: arg2Code @ arg1Code @ code, context)
            end

        |   pushRegisters((X87FPGetCondition{dest, ccRef}, _) :: rest, code, context) =
            let
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest, destCode @ X87FPGetCondition{dest=destVal, ccRef=ccRef} :: code, newContext)
            end

        |   pushRegisters((X87FPArith{opc, resultReg, arg1, arg2}, _) :: rest, code, context) =
            let
                val (arg1Val, arg1Code) = mapSource(arg1, context)
                val (arg2Val, arg2Code) = mapSource(arg2, context)
                val (destVal, destCode, newContext) = mapDestReg(resultReg, context)
            in
                pushRegisters(rest,
                    destCode @ X87FPArith{opc=opc, resultReg=destVal, arg1=arg1Val, arg2=arg2Val} ::
                        arg2Code @ arg1Code @ code, newContext)
            end
    
        |   pushRegisters((X87FPUnaryOps{fpOp, dest, source}, _) :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSource(source, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ X87FPUnaryOps{fpOp=fpOp, dest=destVal, source=sourceVal} :: sourceCode @ code, newContext)
            end

        |   pushRegisters((FloatFixedInt{dest, source}, _) :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSource(source, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest, destCode @ FloatFixedInt{dest=destVal, source=sourceVal} :: sourceCode @ code, newContext)
            end
    
        |   pushRegisters((SSE2FPArith{opc, resultReg, arg1, arg2}, _) :: rest, code, context) =
            let
                val (arg1Val, arg1Code) = mapSource(arg1, context)
                val (arg2Val, arg2Code) = mapSource(arg2, context)
                val (destVal, destCode, newContext) = mapDestReg(resultReg, context)
            in
                pushRegisters(rest,
                    destCode @ SSE2FPArith{opc=opc, resultReg=destVal, arg1=arg1Val, arg2=arg2Val} ::
                        arg2Code @ arg1Code @ code, newContext)
            end
        
        and skipToLabel([], code) = code

        |   skipToLabel((ForwardJumpLabel{label as ILabel lab, ...}, _) :: rest, code) =
            (* Label after unconditional transfer.  No need to merge the states. *)
            (
                (* Look at the state.  We may never have branched to this label.  This sometimes
                   happens with andalso/orelse.  In that case just continue. *)
                case Array.sub(labelState, lab) of
                    NONE => skipToLabel(rest, code)
                |   SOME state => pushRegisters(rest, ForwardJumpLabel{label=label, result=NONE} :: code, state)
            )
        
        |   skipToLabel((BeginHandler{handleStart as ILabel lab, packetReg, workReg, ...}, _) :: rest, code) =
                (* A handler should only ever occur after an unconditional transfer. *)
            let
                val context = valOf(Array.sub(labelState, lab))
                val newWorkReg = mapWorkReg workReg
                val (pktReg, pktCode, newContext) = mapDestReg(packetReg, context)
            in
                pushRegisters(rest, pktCode @ BeginHandler{handleStart=handleStart, packetReg=pktReg, workReg=newWorkReg} :: code, newContext)
            end
        
        |   skipToLabel((EndLoop{loopLabel, staticRegs}, _) :: rest, code) =
                (* Normally we will have at least one branch that exits the loop
                   but it could be that the loop is exited by raising an exception. *)
                skipToLabel(rest, EndLoop{loopLabel=loopLabel, staticRegs=mapSaveRegs staticRegs} :: code)

            (* Any other Exited conditions. Skip the instructions.  There are various situations where this
               can arise.  e.g. OS.Process.exit contains an infinite loop processing atExit functions
               until it eventually exits by calling a "stop" function. *)
        |   skipToLabel(_ :: rest, code) = skipToLabel(rest, code)

        val pushedCode = pushRegisters(code, [], emptyContext)
        val resultCode = List.rev pushedCode
    in
        (resultCode, !pregCounter, !labelCounter, !maxStack)
    end

    exception RegisterOverflow of int list

    fun codeAbstractToConcrete(identifiedCode: (abstract x86ICode * int list) list, regStates, maxPRegs) =
    let
        (* Allocated registers.  This is set to the real register that is used
           for a specific pseudo-register.  Once a register is allocated that
           is fixed. *)
        val allocatedRegs = Array.array(maxPRegs, NONE: reg option)
        
        (* Hint values.  The idea of hints is that by using a hinted register
           we may avoid an unnecessary move instruction.  realHints is set when
           a pseudo-register is going to be loaded from a specific register
           e.g. a register argument, or moved into one e.g. ecx for a shift.
           friends is set to the other pseudo-registers that may be associated
           with the pseudo-register.  E.g. the argument and destination of
           an arithmetic operation where choosing the same register for
           each may avoid a move. *)
        val realHints = Array.array(maxPRegs, NONE: reg option)
        val friends = Array.array(maxPRegs, []: int list)
        
        fun addRealHint(r, reg) =
            case Array.sub(realHints, r) of
                NONE => Array.update(realHints, r, SOME reg)
            |   SOME _ => ()
        
        fun addFriends(f1, f2) =
        let
            val {conflicts, ...} = Vector.sub(regStates, f1)
            
            fun addF (f, toF) =
                let
                    val currentFriends = Array.sub(friends, toF)
                in
                    if List.exists(fn i => i=f) currentFriends
                    then ()
                    else Array.update(friends, toF, f :: currentFriends)
                end
        in
            (* Add a friend if there's no conflict. *)
            if List.exists(fn i => i=f2) conflicts
            then ()
            else (addF(f1, f2); addF(f2, f1))
        end

        (* Find a real register for a preg.
           1.  If a register is already allocated use that.
           2.  Try the "preferred" register if one has been given.
           3.  Try the realHints value if there is one.
           4.  See if there is a "friend" that has an appropriate register
           5.  Look at all the registers and find one. *)
        fun findRegister(r, pref, regSet) =
            case Array.sub(allocatedRegs, r) of
                SOME reg => reg
            |   NONE =>
                let
                    val {conflicts, realConflicts, ...} = Vector.sub(regStates, r)
                    (* Find the registers we've already allocated that may conflict. *)
                    val conflictingRegs =
                        List.mapPartial(fn i => Array.sub(allocatedRegs, i)) conflicts @
                            realConflicts

                    fun isFree aReg = not (List.exists(fn i => i=aReg) conflictingRegs)

                    fun tryAReg NONE = NONE
                    |   tryAReg (somePref as SOME prefReg) =
                            if isFree prefReg
                            then (Array.update(allocatedRegs, r, somePref); somePref)
                            else NONE
                    
                    fun findAReg [] = raise RegisterOverflow conflicts
                    |   findAReg (reg::regs) =
                            if isFree reg then (Array.update(allocatedRegs, r, SOME reg); reg)
                            else findAReg regs

                    (* See if there is a friend that has a register already or a
                       hint.  Friends are registers that don't conflict and can
                       possibly avoid an extra move. *) 
                    fun findAFriend([], _) = NONE
                    |   findAFriend(friend :: tail, old) =
                        let
                            val possReg =
                                case Array.sub(allocatedRegs, friend) of
                                    v as SOME _ => tryAReg v
                                |   NONE => tryAReg(Array.sub(realHints, friend))
                        in
                            case possReg of
                                reg as SOME _ => reg
                            |   NONE =>
                                let
                                    (* Add a friend of a friend to the list if we haven't already
                                       seen it and it doesn't conflict. *)
                                    fun newFriend f =
                                        not(List.exists (fn n => n=f) old) andalso
                                            not(List.exists (fn n => n=f) conflicts)
                                    val fOfF = List.filter newFriend (Array.sub(friends, friend))
                                in
                                    findAFriend(tail @ fOfF, friend :: old)
                                end
                        end
                in
                    case tryAReg pref of
                        SOME r => r
                    |   NONE =>
                        (
                            case tryAReg (Array.sub(realHints, r)) of
                                SOME r => r
                            |   NONE =>
                                (
                                    case findAFriend(Array.sub(friends, r), []) of
                                        SOME r => r
                                        (* Look through the registers to find one that's free. *)
                                    |   NONE => findAReg regSet
                                )
                        )
                end

        fun findGeneralReg r = findRegister(r, NONE, generalRegisters)
        and findFloatReg r = findRegister(r, NONE, floatingPtRegisters)

        fun codeExtArgument(RegisterArgument(PReg(r, _)), regSet) = RegisterArgument(findRegister(r, NONE, regSet))
        |   codeExtArgument(AddressConstant m, _) = AddressConstant m
        |   codeExtArgument(IntegerConstant i, _) = IntegerConstant i
        |   codeExtArgument(MemoryLocation{base=PReg(bReg, _), offset, index}, _) =
                MemoryLocation{base=findGeneralReg bReg, offset=offset, index=codeExtIndex index}
        |   codeExtArgument(StackLocation{wordOffset, adjustment}, _) =
                MemoryLocation{base=GenReg esp, offset=(wordOffset+adjustment)*wordSize, index=NoMemIndex}
        
        and codeExtArgGeneral arg = codeExtArgument(arg, generalRegisters)
        and codeExtArgFloat arg = codeExtArgument(arg, floatingPtRegisters)

        and codeExtIndex NoMemIndex = NoMemIndex
        |   codeExtIndex(MemIndex1(PReg(r, _))) = MemIndex1(findGeneralReg r)
        |   codeExtIndex(MemIndex2(PReg(r, _))) = MemIndex2(findGeneralReg r)
        |   codeExtIndex(MemIndex4(PReg(r, _))) = MemIndex4(findGeneralReg r)
        |   codeExtIndex(MemIndex8(PReg(r, _))) = MemIndex8(findGeneralReg r)

        fun moveRR{src, dst} = LoadArgument{source=RegisterArgument src, dest=dst, kind=MoveWord}

        fun moveIfNecessary{src, dst} =
            if src = dst then [] else [moveRR{src=src, dst=dst}]
        
        (* This is a general function for moving values into registers or to the stack
           where it is possible that the source values might also be in use as destinations.
           The stack is used for destinations only for tail recursive calls. *)
        fun moveMultipleValues(moves, reservedRegs) =
        let
            fun getWorkRegister(reserved, rest) =
            let
                (* Find a free register.  We cannot use any reserved register, which
                   includes those we've loaded, or any source register that we
                   haven't yet used.  We start with all the registers, after filtering
                   those we can't use, and remove a source register from the list.  *)
                val filteredInUse =
                    List.filter (fn r => not(List.exists (fn i => i = r) reserved)) generalRegisters
                
                fun filterASet(_, []) = [] (* Stop if there's nothing there. *)
                |   filterASet([], regs) = regs
                |   filterASet({src=RegisterArgument srcReg, ...} :: rest, regs) =
                        filterASet(rest, List.filter(fn r => r <> srcReg) regs)
                |   filterASet({src=MemoryLocation _, ...} :: _, _) = raise InternalError "filterASet - Memory"
                |   filterASet(_ :: rest, regs) = filterASet(rest, regs)
                
                fun filterList(_, []) = []
                |   filterList([], regs) = regs
                |   filterList(set::rest, regs) = filterList(rest, filterASet(set, regs))
                
                (* So far it seems we always find a register this way if we need one.
                   Just in case, fall back to pushing a register, using it and popping it
                   again. *)
            in
                case filterList(rest, filteredInUse) of
                    workReg :: _ => SOME workReg
                |   [] => NONE
            end

            fun moveValues([], _) = [] (* We're done. *)

            |   moveValues(arguments, reservedRegs) =
                let
                    (* stronglyConnectedComponents does two things.  It detects loops where
                       it's not possible to move items without breaking the loop but more
                       importantly it orders the dependencies so that if there are no loops we
                       can load the source and store it in the destination knowing that
                       we won't overwrite anything we might later need. *)
                    val ordered = STRONGLY.stronglyConnectedComponents arguments
                    
                    fun moveEachValue ([], _) = []

                    |   moveEachValue ([{dst=RegDest reg, src as RegisterArgument r}] :: rest, reserved) =
                            (* Source and dest are both regs - only move if they're different. *)
                            if r = reg
                            then moveEachValue(rest, reg :: reserved)
                            else LoadArgument{source=src, dest=reg, kind=MoveWord} ::
                                    moveEachValue(rest, reg :: reserved)

                    |   moveEachValue ([{dst=RegDest reg, src}] :: rest, reserved) =
                            (* Load from store or a constant. *)
                            LoadArgument{source=src, dest=reg, kind=MoveWord} ::
                                moveEachValue(rest, reg :: reserved)

                    |   moveEachValue ([{dst=StackDest _, src=MemoryLocation _ }] :: _, _) =
                            raise InternalError "moveEachValue - MemoryArgument"

                    |   moveEachValue ([{dst=StackDest addr, src as StackLocation{ wordOffset, adjustment}}] :: rest, reserved) =
                            (* Copy a stack location - needs a load and store unless the address is the same. *)
                            if addr = wordOffset + adjustment
                            then moveEachValue(rest, reserved)
                            else
                            let
                                val loadAndStoreCode =
                                    case getWorkRegister(reserved, rest) of
                                        SOME workReg =>
                                            [LoadArgument{source=src, dest=workReg, kind=MoveWord},
                                             StoreArgument{
                                                    source=RegisterArgument workReg, base=GenReg esp, index=NoMemIndex,
                                                    offset = addr*wordSize, kind=MoveWord}]
                                    |   NONE =>
                                        (* So far it seems we always find a register this way if we need one.
                                           Just in case, fall back to pushing a register, using it and popping it
                                           again. *)
                                        let
                                            val workReg = GenReg eax
                                            val workRegAsArg = RegisterArgument workReg
                                            val destAddr = addr+1
                                            val newSrc =
                                                case src of
                                                    StackLocation{wordOffset, adjustment} =>
                                                        StackLocation{wordOffset=wordOffset+1, adjustment=adjustment}
                                                |   src => src
                                        in
                                            [
                                            PushValue{arg=workRegAsArg},
                                            LoadArgument{source=newSrc, dest=workReg, kind=MoveWord},
                                            StoreArgument{
                                                source=RegisterArgument workReg, base=GenReg esp, index=NoMemIndex,
                                                offset = destAddr*wordSize, kind=MoveWord},
                                            LoadArgument{source=StackLocation{wordOffset=0, adjustment=0}, dest=workReg, kind=MoveWord},
                                            ResetStackPtr{numWords=1}
                                            ]
                                        end
                            in
                                loadAndStoreCode @ moveEachValue(rest, reserved)
                            end

                    |   moveEachValue ([{dst=StackDest addr, src}] :: rest, reserved) =
                            (* Store from a register or a constant. *)
                            StoreArgument{
                                source=src, base=GenReg esp, index=NoMemIndex, offset = addr*wordSize, kind=MoveWord} ::
                                    moveEachValue(rest, reserved)

                    |   moveEachValue(({dst=RegDest regA, src=RegisterArgument regB} :: (cycle as _ :: _)) :: rest, reserved) =
                        (* Cycle with register arguments - swap the registers. *)
                        let
                            (* We need to exchange some of the arguments.  Doing an exchange here will
                               set the destination with the correct source.  However we have to process
                               every subsequent entry with the swapped registers.  That may well mean that
                               one of those entries becomes trivial.  Using XCHG means that we can move
                               N registers in N-1 exchanges.
                               We also need to rerun stronglyConnectedComponents on at least the rest of
                               this cycle.  It's easiest to flatten the rest and do everything. *)
                            val flattened = List.foldl(fn (a, b) => a @ b) [] (cycle :: rest)
                            fun swapRegs r = if r = regA then regB else if r = regB then regA else r
                            fun swapSources{src=RegisterArgument r, dst} =
                                    {src=RegisterArgument(swapRegs r), dst=dst}
                            |   swapSources{src=MemoryLocation _, ...} =
                                    raise InternalError "swapSources: MemoryLocation"
                            |   swapSources sd = sd
                        in
                            ExchangeRegisters{regX=regA, regY=regB} ::
                                moveValues(List.map swapSources flattened, regA :: reserved)
                        end

                    |   moveEachValue(cycle as ({src=originalSrc, ...} :: _ :: _) :: _, reserved) =
                        let
                            (* Anything else.  Break the cycle by getting a work register and
                               loading the source into that.  This is requires more instructions
                               than using an exchange but we don't want to use XCHG between
                               a register and memory.  That generates a memory lock and
                               we don't want the overhead. *)
                            val flattened = List.foldl(fn (a, b) => a @ b) [] cycle
                            val workReg =
                                case getWorkRegister(reserved, cycle) of
                                    SOME workReg => workReg
                                |   _ => raise RegisterOverflow []  (* Force an extra register spill here. *)
                            (* Replace any reference to the source by the new register. *)
                            val replace = RegisterArgument workReg
                            fun swapSources{src, dst} =
                            let
                                val newSource =
                                    case (src, originalSrc) of
                                        (RegisterArgument r1, RegisterArgument r2) =>
                                            if r1=r2 then replace else src
                                    |   (StackLocation{wordOffset=offset1, adjustment=adj1},
                                         StackLocation{wordOffset=offset2, adjustment=adj2}) =>
                                            if offset1+adj1 = offset2+adj2 then replace else src
                                    |   (MemoryLocation _, _) => raise InternalError "swapSources: memory loc"
                                    |   (_, MemoryLocation _) => raise InternalError "swapSources: memory loc"
                                    |   _ => src
                            in
                                {src=newSource, dst=dst}
                            end
                        in
                            LoadArgument{source=originalSrc, dest=workReg, kind=MoveWord} ::
                                (* Process the whole input including this instruction. Don't include
                                   the work reg as reserved.  It's present as a source now so won't
                                   be reused until we've copied it to the real destination. *)
                                moveValues(List.map swapSources flattened, reserved)
                        end

                    |   moveEachValue(([]) :: _, _) = (* This should not happen - avoid warning. *)
                            raise InternalError "moveEachValue - empty set"
                in
                    moveEachValue(ordered, reservedRegs)
                end
        in
            moveValues(moves, reservedRegs)
        end

        (* Where we have multiple specific registers as either source or
           destination there is the potential that a destination register
           if currently in use as a source. *) 
        fun moveMultipleRegisters regPairList =
        let
            val regPairsAsDests =
                List.map(fn {src, dst} => {src=RegisterArgument src, dst=RegDest dst}) regPairList
            (* We don't need a work register so this doesn't matter. *)
            val reserveRegs = generalRegisters (* Consider all registers to be in use. *)
        in
            moveMultipleValues(regPairsAsDests, reserveRegs)
        end

        (* Tail recursive calls are complicated because we generally have to overwrite the existing stack.
           That means storing the arguments in the right order to avoid overwriting a
           value that we are using for a different argument. *)
        type tailCopy = STRONGLY.node

        fun codeTailCall(callKind, arguments: tailCopy list, stackAdjust) =
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
                            StackLocation{wordOffset, adjustment} =>
                                StackLocation{wordOffset=wordOffset+1, adjustment=adjustment}
                        |   other => other
                in
                    {dst=newDest, src=newSrc} :: renumberArgs args
                end
        in
            PushValue{arg=argM1} :: codeTailCall(callKind, renumberArgs arguments, stackAdjust+1)
        end
        else
        let
            val loadArgs = moveMultipleValues(arguments, [] (* We only need argument registers. *))

            val adjustStack =
                if stackAdjust = 0
                then []
                else [ResetStackPtr{numWords=stackAdjust}]
        in
            loadArgs @ adjustStack @
            [TailRecursiveCall{regArgs=[], stackArgs=[], callKind=callKind, returnAddr={srcStack=0, stack=0}, stackAdjust=0}]
        end

        fun codeExtended([], _) = []

        |   codeExtended((LoadArgument{source=RegisterArgument(PReg(sreg, _)), dest=PReg(dreg, _), kind}, _) :: rest, context) =
            (* Register to register move.  Try to use the same register for the source as the destination
               to eliminate the instruction. *)
            let
                val () = addFriends (sreg, dreg)
                val code = codeExtended(rest, context)
            in
                case Array.sub(allocatedRegs, dreg) of
                    NONE => (* The result was never used *) code
                |   SOME realDestReg =>
                    let
                        val regset =
                            case kind of
                                MoveFloat => floatingPtRegisters
                            |   MoveDouble => floatingPtRegisters
                            |   _ => generalRegisters
                        (* Get the source register using the current destination as a preference. *)
                        val realSrcReg = findRegister(sreg, SOME realDestReg, regset)
                    in
                        (* If the source is the same as the destination we don't need to do anything. *)
                        moveIfNecessary{src=realSrcReg, dst=realDestReg} @ code
                    end
            end

        |   codeExtended((LoadArgument{source, dest=PReg(pr, _), kind}, _) :: rest, context) =
            (* Loads of constants or from an address. *)
            let
                val code = codeExtended(rest, context)
                val regSet =
                    case kind of MoveFloat => floatingPtRegisters | MoveDouble => floatingPtRegisters | _ => generalRegisters
            in
                (* If we don't have a register that means the result is never used. *)
                case Array.sub(allocatedRegs, pr) of
                    SOME regResult => LoadArgument{source=codeExtArgument(source, regSet), dest=regResult, kind=kind} :: code
                |   NONE => code
            end

        |   codeExtended((
                StoreArgument{
                    source as RegisterArgument(PReg(sReg, _)), base=PReg(bReg, _), offset, index, kind=MoveByte, ... }, _) :: rest, context) =
            if isX64
            then
            let
                val code = codeExtended(rest, context)
            in
                StoreArgument{
                    source=codeExtArgGeneral source, base=findGeneralReg bReg, offset=offset, index=codeExtIndex index, kind=MoveByte}
                        :: code
            end
            else
            (* This is complicated on X86/32.  We can't use edi or esi for the store registers.  Instead
               we reserve ecx (see special case in "identify") and use that if we have to. *)
            let
                val () = addRealHint(sReg, GenReg ecx)
                val code = codeExtended(rest, context)
                val realStoreReg = findRegister(sReg, SOME(GenReg ecx), generalRegisters)
                val (moveCode, storeReg) =
                    if realStoreReg = GenReg edi orelse realStoreReg = GenReg esi
                    then (moveIfNecessary{src=realStoreReg, dst=GenReg ecx}, GenReg ecx)
                    else ([], realStoreReg)
            in
                moveCode @
                (StoreArgument{
                    source=RegisterArgument storeReg, base=findGeneralReg bReg, offset=offset, index=codeExtIndex index, kind=MoveByte}
                        :: code)
            end

        |   codeExtended((StoreArgument{ source, base=PReg(bReg, _), offset, index, kind }, _) :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val regSet =
                    case kind of MoveFloat => floatingPtRegisters | MoveDouble => floatingPtRegisters | _ => generalRegisters
            in
                StoreArgument{
                    source=codeExtArgument(source, regSet), base=findGeneralReg bReg, offset=offset, index=codeExtIndex index, kind=kind}
                        :: code
            end

        |   codeExtended((LoadMemReg { offset, dest=PReg(pr, _)}, _) :: rest, context) =
            (* Load from the "memory registers" pointed at by ebp. *)
            let
                val code = codeExtended(rest, context)
            in
                (* If we don't have a register that means the result is never used. *)
                case Array.sub(allocatedRegs, pr) of
                    SOME regResult =>
                        LoadArgument{source=MemoryLocation{base=GenReg ebp, offset=offset, index=NoMemIndex}, dest=regResult, kind=MoveWord} :: code
                |   NONE => code
            end

        |   codeExtended((ExchangeRegisters _, _) :: _, _) =
                (* This is only generated in this pass. *)
                raise InternalError "codeExtended - ExchangeRegisters"

        |   codeExtended((BeginFunction{regArgs}, _) :: rest, context) =
            let
                val () = List.app (fn (PReg(pr, _), reg) => addRealHint(pr, reg)) regArgs
                val code = codeExtended(rest, context)

                (* Look up the register we need to move the argument into.
                   If there's no register then we don't use this argument and
                   can drop it. *)
                fun mkPair(PReg(pr, _), rr) =
                    case Array.sub(allocatedRegs, pr) of
                        NONE => NONE
                    |   SOME regResult => SOME{src=rr,dst=regResult}
                val regPairs = List.mapPartial mkPair regArgs
            in
                moveMultipleRegisters regPairs @ code
            end

        |   codeExtended((TailRecursiveCall{callKind, regArgs, stackArgs, returnAddr={srcStack, stack}, stackAdjust}, active) :: rest, context) =
            let
                val () = List.app (fn (RegisterArgument(PReg(pr, _)), reg) => addRealHint(pr, reg) | _ => ()) regArgs
                val code = codeExtended(rest, context)
                (* Add the return address as an extra argument.  This is a temporary hack because codeICode doesn't
                   do it that way. *)
                val returnEntry = {src=StackLocation{wordOffset=srcStack, adjustment=0}, dst=StackDest stack}
                (* We must leave stack entries as stack entries for the moment. *)
                fun codeArg(StackLocation stack) = StackLocation stack
                |   codeArg arg = codeExtArgGeneral arg

                val extStackArgs = map (fn {stack, src} => {dst=StackDest stack, src=codeArg src}) stackArgs
                val extRegArgs = map (fn (a, r) => {src=codeArg a, dst=RegDest r}) regArgs
                val codeTail =
                    codeTailCall(callKind, returnEntry :: extStackArgs @ extRegArgs, stackAdjust)
                        handle RegisterOverflow _ => raise RegisterOverflow active
            in
                codeTail @ code
            end

        |   codeExtended((FunctionCall{callKind, regArgs, stackArgs, dest=PReg(dReg, _)}, _) :: rest, context) =
            let
                val () = List.app (fn (RegisterArgument(PReg(pr, _)), reg) => addRealHint(pr, reg) | _ => ()) regArgs
                val () = addRealHint(dReg, GenReg eax)
                val code = codeExtended(rest, context)
                val destReg = findRegister(dReg, SOME(GenReg eax), generalRegisters)
                
                fun pushStackArgs ([], _) = []
                |   pushStackArgs (arg ::args, argNum) =
                    let
                        (* Have to adjust the offsets of stack arguments. *)
                        val adjusted =
                            case arg of
                                StackLocation {wordOffset, adjustment} =>
                                    StackLocation{wordOffset=wordOffset+argNum, adjustment=adjustment}
                            |   arg => arg
                    in
                        PushValue {arg=codeExtArgGeneral adjusted} :: pushStackArgs(args, argNum+1)
                    end
                val pushedArgs = pushStackArgs(stackArgs, 0)
                (* We have to adjust any stack offset to account for the arguments we've pushed. *)
                val numStackArgs = List.length stackArgs
                
                (* We don't currently allow the arguments to be memory locations and instead
                   force them into registers.  That may be simpler especially if we can get the
                   values directly into the required register. *)
                fun getRegArgs(RegisterArgument(PReg(pr, _)), reg) =
                        SOME{dst=reg, src=findRegister(pr, SOME reg, generalRegisters)}
                |   getRegArgs(MemoryLocation _, _) = raise InternalError "FunctionCall - MemoryLocation"
                |   getRegArgs _ = NONE
                val loadRegArgs = moveMultipleRegisters(List.mapPartial getRegArgs regArgs)

                (* These are all items we can load without requiring a source register.
                   That includes loading from the stack. *)
                fun getConstArgs(AddressConstant m, reg) =
                        SOME(LoadArgument{source=AddressConstant m, dest=reg, kind=MoveWord})
                |   getConstArgs(IntegerConstant i, reg) =
                        SOME(LoadArgument{source=IntegerConstant i, dest=reg, kind=MoveWord})
                |   getConstArgs(StackLocation { wordOffset, adjustment }, reg) =
                        SOME(LoadArgument{source=StackLocation{wordOffset=wordOffset+numStackArgs, adjustment=adjustment},
                                          dest=reg, kind=MoveWord})
                |   getConstArgs(RegisterArgument _, _) = NONE
                |   getConstArgs(MemoryLocation _, _) = NONE
                val loadConstArgs = List.mapPartial getConstArgs regArgs
            in
                pushedArgs @ loadRegArgs @ loadConstArgs @
                    (FunctionCall{regArgs=[], stackArgs=[], dest=raxAsArg, callKind=callKind} ::
                        moveIfNecessary{dst=destReg, src=GenReg eax}) @ code
            end

        |   codeExtended((AllocateMemoryOperation{ size, flags, dest=PReg(dReg, _), saveRegs}, _) :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val preserve = List.map(fn (PReg(r, _)) => findGeneralReg r) saveRegs
            in
                AllocateMemoryOperation{ size=size, flags=flags, dest=findGeneralReg dReg, saveRegs=preserve} :: code
            end

        |   codeExtended((AllocateMemoryVariable{size=PReg(sReg, _), dest=PReg(dReg, _), saveRegs}, _) :: rest, context) =
            let
                (* Simple case - no initialiser. *)
                val code = codeExtended(rest, context)
                val preserve = List.map(fn (PReg(r, _)) => findGeneralReg r) saveRegs
                val destReg = findGeneralReg dReg
            in
                AllocateMemoryVariable{size=findGeneralReg sReg, dest=destReg, saveRegs=preserve} :: code
            end

        |   codeExtended((InitialiseMem{size=PReg(sReg, _), addr=PReg(aReg, _), init=PReg(iReg, _)}, _) :: rest, context) =
            let
                (* We are going to use rep stosl/q to set the memory.
                   That requires the length to be in ecx, the initialiser to be in eax and
                   the destination to be edi. *)
                val () = addRealHint(aReg, GenReg edi)
                val () = addRealHint(iReg, GenReg eax)
                val () = addRealHint(sReg, GenReg ecx)
                val code = codeExtended(rest, context)
                val realAddrReg = findRegister(aReg, SOME(GenReg edi), generalRegisters)
                val realInitReg = findRegister(iReg, SOME(GenReg eax), generalRegisters)
                val realSizeReg = findRegister(sReg, SOME(GenReg ecx), generalRegisters)
            in
                moveMultipleRegisters[
                    {src=realInitReg, dst=GenReg eax}, {src=realSizeReg, dst=GenReg ecx}, {src=realAddrReg, dst=GenReg edi}] @
                    InitialiseMem{size=GenReg ecx, addr=GenReg edi, init=GenReg eax} :: code
            end

        |   codeExtended((InitialisationComplete, _) :: rest, context) =
                InitialisationComplete :: codeExtended(rest, context)

        |   codeExtended((StartLoop{regArgs, loopLabel}, _) :: rest, context) =
            let
                (* Make the loop arguments and the sources "friends". *)
                fun makeFriends (RegisterArgument(PReg(sReg, _)), PReg(dReg, _)) = addFriends(dReg, sReg)
                |   makeFriends _ = ()
                val () = List.app makeFriends regArgs
                val code = codeExtended(rest, context)
                val extRegArgs = map (fn (a, PReg(r, _)) => {src=codeExtArgGeneral a, dst=RegDest(findGeneralReg r)}) regArgs
            in
                moveMultipleValues(extRegArgs, generalRegisters) @ (StartLoop{regArgs=[], loopLabel=loopLabel} :: code)
            end

        |   codeExtended((EndLoop {staticRegs, ...}, _) :: rest, context) =
            let
                (* staticRegs in EndLoop is a list of the registers that are used within the loop
                   but aren't loop variables.  They have to retain their values throughout the loop
                   and are treated as having been last used after the loop.  We have to ensure
                   that registers are allocated for them before we enter the loop. *)
                val () = List.app(fn PReg(r, _) => (findRegister(r, NONE, generalRegisters); ())) staticRegs
            in
                codeExtended(rest, context)
            end

        |   codeExtended((JumpLoop{regArgs, stackArgs, loopLabel, stackAdjust}, _) :: rest, context) =
            let
                (* TODO: Make the sources and destinations "friends". *)
                val code = codeExtended(rest, context)
                val extStackArgs = map (fn (src, stack) => {dst=StackDest stack, src=codeExtArgGeneral src}) stackArgs
                val extRegArgs = map (fn (a, PReg(r, _)) => {src=codeExtArgGeneral a, dst=RegDest(findGeneralReg r)}) regArgs
                val resetCode =
                    if stackAdjust = 0 then [] else [ResetStackPtr{numWords=stackAdjust}]
            in
                moveMultipleValues(extStackArgs @ extRegArgs, generalRegisters) @ resetCode @
                    (JumpLoop{regArgs=[], stackArgs=[], loopLabel=loopLabel, stackAdjust = 0} :: code)
            end

        |   codeExtended((RaiseExceptionPacket{ packet=RegisterArgument(PReg(preg, _)) }, _) :: rest, context) =
            let
                val () = addRealHint(preg, GenReg eax)
                val code = codeExtended(rest, context)
                (* The argument must be put into rax. *)
                val argReg = findRegister(preg, SOME(GenReg eax), generalRegisters)
            in
                moveIfNecessary{src=argReg, dst=raxAsArg} @
                    RaiseExceptionPacket {packet=RegisterArgument raxAsArg } :: code
            end

        |   codeExtended((RaiseExceptionPacket{ packet }, _) :: rest, context) =
            let
                (* Because we're exiting we don't have to worry about conflicts - just load it. *)
                val code = codeExtended(rest, context)
            in
                LoadArgument{source=codeExtArgument(packet, generalRegisters), dest=raxAsArg, kind=MoveWord} ::
                    RaiseExceptionPacket {packet=RegisterArgument raxAsArg } :: code
            end

        |   codeExtended((ReserveContainer{size, address=PReg(aReg, _)}, _) :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val addrReg = findRegister(aReg, NONE, generalRegisters)
                (* The memory must be cleared in case we have a GC. *)
                val pushAll = List.tabulate(size, fn _ => PushValue{arg=IntegerConstant(tag 0)})
            in
                pushAll @ LoadArgument{source=RegisterArgument(GenReg esp), dest=addrReg, kind=MoveWord} :: code
            end

        |   codeExtended((IndexedCaseOperation{testReg=PReg(tReg, _), workReg=PReg(wReg, _), cases, startValue}, _) :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val testReg = findRegister(tReg, NONE, generalRegisters)
                val workReg = findRegister(wReg, NONE, generalRegisters)
                val _ = testReg <> workReg orelse raise InternalError "IndexedCaseOperation - same registers"
            in
                IndexedCaseOperation{ testReg=testReg, workReg=workReg, cases=cases, startValue=startValue} :: code
            end

        |   codeExtended((LockMutable{addr=PReg(pr, _)}, _) :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val argReg = findRegister(pr, NONE, generalRegisters)
            in
                LockMutable{addr=argReg} :: code
            end

        |   codeExtended((ForwardJumpLabel{ label, result=_ }, _) :: rest, context) =
                ForwardJumpLabel{ label=label, result=NONE } :: codeExtended(rest, context)

        |   codeExtended((UnconditionalForwardJump { label }, _) :: rest, context) =
                UnconditionalForwardJump { label=label } :: codeExtended(rest, context)

        |   codeExtended((ConditionalForwardJump{ ccRef, condition, label }, _) :: rest, context) =
                ConditionalForwardJump{ ccRef=ccRef, condition=condition, label=label } :: codeExtended(rest, context)

        |   codeExtended((WordComparison{ arg1 as RegisterArgument _, arg2, ccRef }, _) :: rest, context) =
            let
                val code = codeExtended(rest, context)
            in
                WordComparison{ arg1=codeExtArgGeneral arg1, arg2=codeExtArgGeneral arg2, ccRef=ccRef } :: code
            end

        |   codeExtended((WordComparison _, _) :: _, _) = raise InternalError "codeExtended - WordComparison"

            (* Set up an exception handler. *)
        |   codeExtended((PushExceptionHandler{workReg=PReg(hReg, _), handleStart}, _) :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val handleReg = findGeneralReg hReg
            in
                PushExceptionHandler{workReg=handleReg, handleStart=handleStart} :: code
            end

            (* Pop an exception handler at the end of a handled section.  Executed if no exception has been raised.
               This removes items from the stack. *)
        |   codeExtended((PopExceptionHandler{workReg=PReg(wReg, _), ...}, _) :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val realWork = findGeneralReg wReg
            in
                PopExceptionHandler{resultReg=NONE, workReg=realWork} :: code
            end

            (* Start of a handler.  Sets the address associated with PushExceptionHandler and
               provides a register for the packet.  There is a work register but we could
               use any register other than rax since we will have pushed anything we need. *) 
        |   codeExtended((BeginHandler{handleStart, packetReg=PReg(pReg, _), workReg=_}, _) :: rest, context) =
            let
                (* The exception packet is in rax. *)
                val () = addRealHint(pReg, GenReg eax)
                val code = codeExtended(rest, context)
                val realPktReg = findRegister(pReg, SOME(GenReg eax), generalRegisters)
            in
                BeginHandler{handleStart=handleStart, workReg=GenReg ebx, packetReg=GenReg eax} ::
                    (moveIfNecessary{src=GenReg eax, dst=realPktReg } @ code)
            end

        |   codeExtended((ReturnResultFromFunction { resultReg=PReg(resReg, _), numStackArgs }, _) :: rest, context) =
            let
                val () = addRealHint(resReg, GenReg eax)
                val code = codeExtended(rest, context)
                val resultReg = findRegister(resReg, SOME(GenReg eax), generalRegisters)
                (* If for some reason it's not in the right register we have to move it there. *)
                val moveCode = moveIfNecessary{src=resultReg, dst=raxAsArg}
            in
                moveCode @
                    (ReturnResultFromFunction{resultReg=raxAsArg, numStackArgs=numStackArgs} :: code)
            end

        |   codeExtended((ArithmeticFunction{oper=SUB, resultReg=PReg(resReg, _), operand1=RegisterArgument(PReg(op1Reg, _)),
                                            operand2, ccRef}, _) :: rest, context) =
            (* Subtraction - this is special because it can only be done one way round.  The first argument must
               be in a register. *)
            let
                val () = addFriends (resReg, op1Reg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                (* Try to put the argument into the same register as the result. *)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, generalRegisters)
                val op2Arg = codeExtArgGeneral operand2
                (* If we couldn't put it in the result register we have to copy it there. *)
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (ArithmeticFunction{oper=SUB, resultReg=realDestReg, operand1=RegisterArgument realDestReg, operand2=op2Arg, ccRef=ccRef} :: code)
            end

        |   codeExtended((ArithmeticFunction{oper=SUB, ...}, _) :: _, _) = raise InternalError "codeExtended - ArithmeticFunction"

        |   codeExtended((ArithmeticFunction{oper, resultReg=PReg(resReg, _), operand1=RegisterArgument(PReg(op1Reg, _)),
                                            operand2=RegisterArgument(PReg(op2Reg, _)), ccRef}, _) :: rest, context) =
            (* Arithmetic operation with both arguments as registers.  These operations are all symmetric so
               we can try to put either argument into the result reg and then do the operation on the other arg. *)
            let
                val () = addFriends (resReg, op1Reg)
                val () = addFriends (resReg, op2Reg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, generalRegisters)
                and realOp2Reg = findRegister(op2Reg, SOME realDestReg, generalRegisters)
                val (operandReg, moveInstr) =
                    if realOp1Reg = realDestReg
                    then (realOp2Reg, [])
                    else if realOp2Reg = realDestReg
                    then (realOp1Reg, [])
                    else (realOp2Reg, [moveRR{src=realOp1Reg, dst=realDestReg}])
            in
                moveInstr @
                    (ArithmeticFunction{oper=oper, resultReg=realDestReg, operand1=RegisterArgument realDestReg,
                                        operand2=RegisterArgument operandReg, ccRef=ccRef} :: code)
            end

        |   codeExtended((ArithmeticFunction{oper, resultReg=PReg(resReg, _), operand1=RegisterArgument(PReg(op1Reg, _)),
                                            operand2, ccRef}, _) :: rest, context) =
            (* Arithmetic operation with the first argument in a register and the second a constant or memory location. *)
            let
                val () = addFriends (resReg, op1Reg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, generalRegisters)
                val op2Arg = codeExtArgGeneral operand2
                (* If we couldn't put it in the result register we have to copy it there. *)
                (* TODO: Is there the potential for a problem?  We don't worry about a conflict
                   between the result register and the arguments.  What if the second argument is a memory
                   location with the result reg as a base or index? *)
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (ArithmeticFunction{oper=oper, resultReg=realDestReg, operand1=RegisterArgument realDestReg, operand2=op2Arg, ccRef=ccRef} :: code)
            end

        |   codeExtended((ArithmeticFunction{oper, resultReg=PReg(resReg, _), operand1,
                                            operand2=RegisterArgument(PReg(op2Reg, _)), ccRef}, _) :: rest, context) =
            (* Arithemtic operation with the second argument in a register and the first a constant or memory location. *)
            let
                val () = addFriends (resReg, op2Reg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val realOp2Reg = findRegister(op2Reg, SOME realDestReg, generalRegisters)
                val op1Arg = codeExtArgGeneral operand1
            in
                moveIfNecessary{src=realOp2Reg, dst=realDestReg} @
                    (ArithmeticFunction{oper=oper, resultReg=realDestReg, operand1=RegisterArgument realDestReg, operand2=op1Arg, ccRef=ccRef} :: code)
            end

        |   codeExtended((ArithmeticFunction _, _) :: _, _) =
                raise InternalError "codeExtended - ArithmeticFunction"

        |   codeExtended((TestTagBit{arg, ccRef}, _) :: rest, context) =
            let
                val code = codeExtended(rest, context)
            in
                TestTagBit{ arg=codeExtArgGeneral arg, ccRef=ccRef } :: code
            end

        |   codeExtended((PushValue {arg}, _) :: rest, context) =
            let
                val code = codeExtended(rest, context)
            in
                PushValue {arg=codeExtArgGeneral arg} :: code
            end

        |   codeExtended((ResetStackPtr {numWords}, _) :: rest, context) =
            (* This is needed to remove containers on the stack. *)
                ResetStackPtr{numWords=numWords} :: codeExtended(rest, context)

        |   codeExtended((TagValue{source=PReg(srcReg, _), dest=PReg(dReg, _)}, _) :: rest, context) =
            let
                val code = codeExtended(rest, context)
            in
                case Array.sub(allocatedRegs, dReg) of
                    SOME regResult =>
                        let
                            (* If we're using LEA to tag there's we can use any source register. *)
                            val realSReg = findRegister(srcReg, NONE, generalRegisters)
                        in
                            LoadEffectiveAddress { base=NONE, offset=1, index=MemIndex2 realSReg, dest=regResult }  :: code
                        end
                |   NONE => code
            end

        |   codeExtended((UntagValue{source=RegisterArgument(PReg(sReg, _)), dest=PReg(dReg, _), isSigned}, _) :: rest, context) =
            (* Always generates register argument at the moment.  TODO: This should really just take a single register arg. *)
            let
                val () = addFriends (sReg, dReg)
                val code = codeExtended(rest, context)
            in
                case Array.sub(allocatedRegs, dReg) of
                    SOME regResult =>
                        let
                            val realSReg = findRegister(sReg, SOME regResult, generalRegisters)
                        in
                            moveIfNecessary{src=realSReg, dst=regResult} @
                                (ShiftOperation{shift=if isSigned then SAR else SHR, resultReg=regResult, operand=RegisterArgument regResult,
                                    shiftAmount=IntegerConstant 1, ccRef=CcRef 0 } :: code)
                        end
                |   NONE => code
            end

        |   codeExtended((UntagValue _, _) :: _, _) =
                raise InternalError "UntagValue"

        |   codeExtended((LoadEffectiveAddress{base=SOME base, offset=0, index=NoMemIndex, dest}, active) :: rest, context) =
                (* This should be handled at the higher level. *)
                codeExtended((LoadArgument{source=RegisterArgument base, dest=dest, kind=MoveWord}, active) :: rest, context)

        |   codeExtended((LoadEffectiveAddress{base, offset, index, dest=PReg(dReg, _)}, _) :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val destReg = findGeneralReg dReg
                val bReg = case base of SOME(PReg(br, _)) => SOME(findGeneralReg br) | NONE => NONE
                val iReg = codeExtIndex index
            in
                LoadEffectiveAddress{base=bReg, offset=offset, index=iReg, dest=destReg} :: code
            end

        |   codeExtended((ShiftOperation{shift, resultReg=PReg(resReg, _), operand=RegisterArgument(PReg(operReg, _)), shiftAmount=IntegerConstant i, ccRef}, _) :: rest, context) =
            let
                val () = addFriends (resReg, operReg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val realOpReg = findRegister(operReg, SOME realDestReg, generalRegisters)
            in
                 moveIfNecessary{src=realOpReg, dst=realDestReg} @
                    (ShiftOperation{shift=shift, resultReg=realDestReg, operand=RegisterArgument realDestReg,
                                    shiftAmount=IntegerConstant i, ccRef=ccRef} :: code)
            end

        |   codeExtended((ShiftOperation{shift, resultReg=PReg(resReg, _), operand=RegisterArgument(PReg(operReg, _)),
                                        shiftAmount=RegisterArgument(PReg(shiftReg, _)), ccRef}, _) :: rest, context) =
            let
                val () = addFriends (resReg, operReg)
                val () = addRealHint(shiftReg, GenReg ecx)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val realShiftReg = findRegister(shiftReg, SOME(GenReg ecx), generalRegisters)
                val realOpReg = findRegister(operReg, SOME realDestReg, generalRegisters)
                (* We want the shift in ecx.  We may not have got it there but the register
                   should be free. *)
            in
                 moveIfNecessary{src=realOpReg, dst=realDestReg} @ moveIfNecessary{src=realShiftReg, dst=GenReg ecx} @
                    (ShiftOperation{shift=shift, resultReg=realDestReg, operand=RegisterArgument realDestReg,
                                    shiftAmount=RegisterArgument(GenReg ecx), ccRef=ccRef} :: code)
            end

        |   codeExtended((ShiftOperation _, _) :: _, _) = raise InternalError "codeExtended - ShiftOperation"

        |   codeExtended((
                Multiplication{resultReg=PReg(resReg, _), operand1=RegisterArgument(PReg(op1Reg, _)),
                               operand2=RegisterArgument(PReg(op2Reg, _)), ccRef}, _) :: rest, context) =
            let
                (* Treat exactly the same as ArithmeticFunction. *)
                val () = addFriends (resReg, op1Reg)
                val () = addFriends (resReg, op2Reg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, generalRegisters)
                and realOp2Reg = findRegister(op2Reg, SOME realDestReg, generalRegisters)
                val (operandReg, moveInstr) =
                    if realOp1Reg = realDestReg
                    then (realOp2Reg, [])
                    else if realOp2Reg = realDestReg
                    then (realOp1Reg, [])
                    else (realOp2Reg, [moveRR{src=realOp1Reg, dst=realDestReg}])
            in
                moveInstr @
                    (Multiplication{resultReg=realDestReg, operand1=RegisterArgument realDestReg,
                                        operand2=RegisterArgument operandReg, ccRef=ccRef} :: code)
            end

            (* We currently only generate the register/register case. *)
        |   codeExtended((Multiplication _, _) :: _, _) = raise InternalError "codeExtended - multiplication TODO"

        |   codeExtended((Division{isSigned, dividend=PReg(regDivid, _), divisor, quotient=PReg(regQuot, _),
                                  remainder=PReg(regRem, _)}, _) :: rest, context) =
            let
                (* Division is specific as to the registers.  The dividend must be eax, quotient is
                   eax and the remainder is edx. *)
                val () = addRealHint(regDivid, GenReg eax)
                val () = addRealHint(regQuot, GenReg eax)
                val () = addRealHint(regRem, GenReg edx)
                val code = codeExtended(rest, context)
                val realDiviReg = findRegister(regDivid, SOME(GenReg eax), generalRegisters)
                val realQuotReg = findRegister(regQuot, SOME(GenReg eax), generalRegisters)
                val realRemReg = findRegister(regRem, SOME(GenReg edx), generalRegisters)
                val divisorArg = codeExtArgGeneral divisor
            in
                (* We may need to move one or more of the registers although normally that
                   won't be necessary.  Almost certainly only either the remainder or the
                   quotient will actually be used. *)
                moveIfNecessary{src=realDiviReg, dst=GenReg eax} @
                    Division{isSigned=isSigned, dividend=GenReg eax, divisor=divisorArg,
                              quotient=GenReg eax, remainder=GenReg edx} ::
                    moveMultipleRegisters[{src=GenReg eax, dst=realQuotReg}, {src=GenReg edx, dst=realRemReg}] @ code
            end

        |   codeExtended((AtomicExchangeAndAdd{base=PReg(bReg, _), source=PReg(sReg, _)}, _) :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val srcReg = findGeneralReg sReg
                val baseReg = findGeneralReg bReg
            in
                AtomicExchangeAndAdd{base=baseReg, source=srcReg} :: code
            end

        |   codeExtended((BoxValue{boxKind, source=PReg(sReg, _), dest=PReg(dReg, _), saveRegs}, _) :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val preserve = List.map(fn (PReg(r, _)) => findGeneralReg r) saveRegs
                val (srcReg, boxSize, moveKind) =
                    case boxKind of
                        BoxLargeWord => (findGeneralReg sReg, 1, MoveWord)
                    |   BoxFloat => (findFloatReg sReg, Int.quot(8, wordSize), MoveDouble)
                val dstReg = findGeneralReg dReg
            in
                AllocateMemoryOperation{ size=boxSize, flags=0wx1, dest=dstReg, saveRegs=preserve} ::
                StoreArgument{ source=RegisterArgument srcReg, offset=0, base=dstReg, index=NoMemIndex, kind=moveKind} ::
                InitialisationComplete :: code
            end

        |   codeExtended((CompareByteVectors{vec1Addr=PReg(v1Reg, _), vec2Addr=PReg(v2Reg, _), length=PReg(lReg, _), ccRef}, _) :: rest, context) =
            let
                val () = addRealHint(v1Reg, GenReg esi)
                val () = addRealHint(v2Reg, GenReg edi)
                val () = addRealHint(lReg, GenReg ecx)
                val code = codeExtended(rest, context)
                val realV1Reg = findRegister(v1Reg, SOME(GenReg esi), generalRegisters)
                val realV2Reg = findRegister(v2Reg, SOME(GenReg edi), generalRegisters)
                val realLengthReg = findRegister(lReg, SOME(GenReg ecx), generalRegisters)
                (* There's a complication here.  CompareByteVectors generates REPE CMPSB to compare
                   the vectors but the condition code is only set if CMPSB is executed at least
                   once.  If the value in RCX/ECX is zero it will never be executed and the
                   condition code will be unchanged.  We want the result to be "equal" in that
                   case so we need to ensure that is the case.  It's quite possible that the
                   condition code has just been set by shifting RCX/ECX to remove the tag in which
                   case it will have set "equal" if the value was zero.  We use CMP R/ECX,R/ECX which
                   is two bytes in 32-bit but three in 64-bit.
                   If we knew the length was non-zero (e.g. a constant) we could avoid this. *)
            in
                moveIfNecessary{src=realV1Reg, dst=GenReg esi} @ moveIfNecessary{src=realV2Reg, dst=GenReg edi} @
                    moveIfNecessary{src=realLengthReg, dst=GenReg ecx} @
                    (WordComparison {arg1=RegisterArgument rcxAsArg, arg2=RegisterArgument rcxAsArg, ccRef=ccRef} ::
                     CompareByteVectors{vec1Addr=GenReg esi, vec2Addr=GenReg edi, length=GenReg ecx, ccRef=ccRef} :: code)
            end

        |   codeExtended((BlockMove{srcAddr=PReg(sReg, _), destAddr=PReg(dReg, _), length=PReg(lReg, _), isByteMove}, _) :: rest, context) =
            let
                val () = addRealHint(sReg, GenReg esi)
                val () = addRealHint(dReg, GenReg edi)
                val () = addRealHint(lReg, GenReg ecx)
                val code = codeExtended(rest, context)
                val realSrcReg = findRegister(sReg, SOME(GenReg esi), generalRegisters)
                val realDestReg = findRegister(dReg, SOME(GenReg edi), generalRegisters)
                val realLengthReg = findRegister(lReg, SOME(GenReg ecx), generalRegisters)
            in
                moveIfNecessary{src=realSrcReg, dst=GenReg esi} @ moveIfNecessary{src=realDestReg, dst=GenReg edi} @
                    moveIfNecessary{src=realLengthReg, dst=GenReg ecx} @
                    (BlockMove{srcAddr=GenReg esi, destAddr=GenReg edi, length=GenReg ecx, isByteMove=isByteMove} :: code)
            end

        |   codeExtended((CompareFloatingPt{arg1 as RegisterArgument _, arg2, ccRef}, _) :: rest, context) =
            let
                val code = codeExtended(rest, context)
            in
                CompareFloatingPt{ arg1=codeExtArgFloat arg1, arg2=codeExtArgFloat arg2, ccRef=ccRef } :: code
            end

        |   codeExtended((CompareFloatingPt _, _) :: _, _) = raise InternalError "codeExtended - CompareFloatingPt"

        |   codeExtended((X87FPGetCondition{dest=PReg(dReg, _), ccRef}, _) :: rest, context) =
            let
                (* We can only use RAX here. *)
                val () = addRealHint(dReg, GenReg eax)
                val code = codeExtended(rest, context)
                val destReg = findRegister(dReg, SOME(GenReg eax), generalRegisters)
            in
                X87FPGetCondition{dest=raxAsArg, ccRef=ccRef} ::
                    moveIfNecessary{dst=destReg, src=GenReg eax} @ code
            end

        |   codeExtended((X87FPArith{opc, resultReg=PReg(resReg, _), arg1=RegisterArgument(PReg(op1Reg, _)), arg2}, _) :: rest, context) =
            let
                val () = addFriends (resReg, op1Reg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, floatingPtRegisters)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, floatingPtRegisters)
                val op2Arg = codeExtArgFloat arg2
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (X87FPArith{opc=opc, resultReg=realDestReg, arg1=RegisterArgument realDestReg, arg2=op2Arg} :: code)
            end

        |   codeExtended((X87FPArith _, _) :: _, _) = raise InternalError "codeExtended - X87FPArith"
    
        |   codeExtended((X87FPUnaryOps{fpOp, dest=PReg(resReg, _), source=RegisterArgument(PReg(op1Reg, _))}, _) :: rest, context) =
            let
                val () = addFriends (resReg, op1Reg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, floatingPtRegisters)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, floatingPtRegisters)
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (X87FPUnaryOps{fpOp=fpOp, dest=realDestReg, source=RegisterArgument realDestReg} :: code)
            end

        |   codeExtended((X87FPUnaryOps _, _) :: _, _) = raise InternalError "codeExtended - X87FPArith"

        |   codeExtended((FloatFixedInt{dest=PReg(resReg, _), source}, _) :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val intSource = codeExtArgGeneral source
                val fpReg = findRegister(resReg, NONE, floatingPtRegisters)
                val floatCode =
                    case fpMode of
                        FPModeX87 => 
                            (* This is complicated.  The integer value has to be in memory not in a
                               register so we have to push it to the stack and then make sure it is
                               popped afterwards.  Because it is untagged it is unsafe to leave it. *)
                            [
                                PushValue{arg=intSource},
                                FloatFixedInt{dest=fpReg, source=wordOffsetAddress(0, esp)},
                                ResetStackPtr {numWords=1}
                            ]
                    |   FPModeSSE2 => [FloatFixedInt{dest=fpReg, source=intSource}]
            in
                floatCode @ code
            end
    
        |   codeExtended((SSE2FPArith{opc, resultReg=PReg(resReg, _), arg1=RegisterArgument(PReg(op1Reg, _)), arg2}, _) :: rest, context) =
            let
                val () = addFriends (resReg, op1Reg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, floatingPtRegisters)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, floatingPtRegisters)
                val op2Arg = codeExtArgFloat arg2
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (SSE2FPArith{opc=opc, resultReg=realDestReg, arg1=RegisterArgument realDestReg, arg2=op2Arg} :: code)
            end
    
        |   codeExtended((SSE2FPArith _, _) :: _, _) = raise InternalError "codeExtended - SSE2FPArith"

    in
        codeExtended(identifiedCode, [])
    end
    
    fun spillRegisters(instrsAndActive, regStates: regState vector, maxPRegs) =
    let
        val pushArray = Array.array(maxPRegs, false)
        (* Mark anything already marked as "must push" *)
        val () = Vector.appi(fn (i, {pushState=MustPush, ...}) => Array.update(pushArray, i, true) | _ => ()) regStates
        val nGenRegs = List.length generalRegisters
        (* Make a list of all the active sets ignoring those marked to be pushed.
           Do that first because we need to know how many sets each register is in. *)
        fun nowActive regs =
            List.foldl (fn (r, l) => if Array.sub(pushArray, r) then l else r :: l) [] regs
        fun getSets((_, active), l) =
        let
            val set = nowActive active
        in
            if List.length set > nGenRegs
            then set :: l
            else l
        end
        val activeSets = List.foldl getSets [] instrsAndActive
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
                    val {active, pushState, refs, ...} = Vector.sub(regStates, r)
                in
                    case pushState of
                        MustNotPush => (r, ~1, ~1)
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
                        val {pushState, ...} = Vector.sub(regStates, reg)
                        val _ = case pushState of MustNotPush => raise InternalError "markAsPush" | _ => ()
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

    fun codeICodeFunctionToX86{icode, functionName, maxLabels, maxPRegs, argRegsUsed, hasFullClosure, debugSwitches, ...} =
    let
        val icodeTabs = [8, 20, 60]
        
        fun printCodeAndStates(identifiedCode, regStates) =
            (* Print the code before the transformation. *)
            if DEBUG.getParameter DEBUG.icodeTag debugSwitches

            then
            let
                val printStream = PRETTY.getSimplePrinter(debugSwitches, icodeTabs)

                fun printRegs([], _) = ()
                |   printRegs(_, 0) = printStream "..."
                |   printRegs([i], _) = printStream(Int.toString i)
                |   printRegs(i::l, n) = (printStream(Int.toString i ^ ","); printRegs(l, n-1))

                fun printCode (c, _) =
                (
                    printICodeAbstract(c, printStream);
                    printStream "\n"
                )
                
                fun printRegData(i, { conflicts, defs, refs, pushState, ... }) =
                (
                    printStream (Int.toString i ^ "\t");
                    printStream ("Conflicts="); printRegs(conflicts, 20);
                    printStream (" Rfs=" ^ Int.toString refs);
                    printStream (" Defs=" ^ Int.toString defs);
                    case pushState of
                        MustPush => printStream " Must push" | MustNotPush => printStream " No push" | MayPush => ();
                    printStream "\n"
                )
            in
                printStream(functionName ^ "\n");
                List.app printCode identifiedCode;
                printStream "\n";
                Vector.appi printRegData regStates
            end
            else ()

        fun processCode(icode, maxPRegs, maxLabels, maxStack, passes) =
        let
            (* First pass - identify register use patterns *)
            val (identified, regStates) = identifyRegisters {icode=icode, maxPRegs=maxPRegs}
            val () = printCodeAndStates(identified, regStates)
            val regsToSpill = spillRegisters(identified, regStates, maxPRegs)
            val needPhase2 = Vector.exists(fn t => t) regsToSpill
        in
            if needPhase2
            then
            let
                (* Push those registers we need to.  This also adds and renumbers pregs
                   and may add labels. *)
                val (postPushCode, maxPRegsPhase2, maxLabelsPhase2, maxStackPhase2) =
                    addRegisterPushes(identified, regsToSpill, maxPRegs, maxLabels)
            in
                (* And reprocess. *)
                processCode(postPushCode, maxPRegsPhase2, maxLabelsPhase2, maxStackPhase2, passes+1)
            end
            else
            let
                fun selectARegisterToSpill active =
                let
                    fun chooseReg([], bestReg, _) = bestReg
                    |   chooseReg(reg::regs, bestReg, bestCost) =
                        let
                            val {active, pushState, refs, ...} = Vector.sub(regStates, reg)
                            val cost = if refs = 0 then 0 else Int.quot(active, refs)
                        in
                            case pushState of
                                MustNotPush => chooseReg(regs, bestReg, bestCost)
                            |   _ =>
                                if cost >= bestCost
                                then chooseReg(regs, reg, active)
                                else chooseReg(regs, bestReg, bestCost)
                        end
                    val choice = chooseReg(active, ~1, 0)
                    val _ = choice >= 0 orelse raise InternalError "chooseReg"
                in
                    Vector.tabulate(maxPRegs, fn i => i = choice)
                end
            in
                (codeAbstractToConcrete(identified, regStates, maxPRegs), maxLabels, maxStack)
                    handle RegisterOverflow activeSet =>
                    let
                        val regsToSpill = selectARegisterToSpill activeSet
                        val (postPushCode, maxPRegsPhase2, maxLabelsPhase2, maxStackPhase2) =
                            addRegisterPushes(identified, regsToSpill, maxPRegs, maxLabels)
                    in
                        processCode(postPushCode, maxPRegsPhase2, maxLabelsPhase2, maxStackPhase2, passes+1)
                    end
            end
        end

        val (postTransformCode, maxLabels, stackRequired) =
            processCode(icode, maxPRegs, maxLabels, 0 (* Should include handlers and containers. *), 0)

        val () =
            (* Print the code after the transformation. *)
            if DEBUG.getParameter DEBUG.icodeTag debugSwitches
            then
            let
                val printStream = PRETTY.getSimplePrinter(debugSwitches, icodeTabs)

                fun printCode c =
                ( printICodeConcrete(c, printStream); printStream "\n")
            in
                printStream(functionName ^ "\n");
                List.app printCode postTransformCode;
                printStream "\n"
            end
            else ()
    in
        (* Ccode-generate it. *)
        codeAsX86Code{icode=postTransformCode, maxLabels = maxLabels, stackRequired = stackRequired,
                      inputRegisters= argRegsUsed @ (if hasFullClosure then [GenReg edx] else []), debugSwitches=debugSwitches,
                      functionName=functionName}
    end

    structure Sharing =
    struct
        type 'reg x86ICode = 'reg x86ICode
        and abstract = abstract
        and reg   = reg
    end
end;
