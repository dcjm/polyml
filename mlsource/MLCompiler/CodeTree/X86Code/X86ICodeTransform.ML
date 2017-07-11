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

    fun argAsPReg(RegisterArgument(PReg(n, PRegGeneral))) = n | argAsPReg _ = raise InternalError "argAsPReg"
    and argAsUReg(RegisterArgument(PReg(n, PRegUntagged))) = n | argAsUReg _ = raise InternalError "argAsUReg"
    
    fun pRegNum(PReg(n, PRegGeneral)) = n | pRegNum _ = raise InternalError "pRegNum"
    and uRegNum(PReg(n, PRegUntagged)) = n | uRegNum _ = raise InternalError "uRegNum"

    fun loadMemory(reg, base, offset) =
        [MoveArgument{
            source=MemoryLocation{base=(GenReg base), offset=offset, index=NoMemIndex},
            dest=RegisterArgument(GenReg reg), kind=MoveWord}]
    and loadStack(reg, offset) = loadMemory(reg, esp, offset)
    and storeMemory(reg, base, offset) =
        [MoveArgument{
            source=RegisterArgument(GenReg reg),
            dest=MemoryLocation{base=(GenReg base), offset=offset, index=NoMemIndex}, kind=MoveWord}]

    val raxAsArg = (GenReg eax) and rbxAsArg = (GenReg ebx)
    and rcxAsArg = (GenReg ecx) and rsiAsArg = (GenReg esi)
    and rdxAsArg = (GenReg edx) and rdiAsArg = (GenReg edi)

    (* Generally we have an offset in words and no index register. *)
    fun wordOffsetAddress(offset, baseReg) =
        MemoryLocation{offset=offset*wordSize, base=(GenReg baseReg), index=NoMemIndex}

    (* Transform the version of the ICode generated from the codetree by replacing the pseudo-registers by
       real locations.  When this is complete it passes it on to the next phase for code-generation.
       This is currently very simple-minded and just uses rax as an accumulator and the
       stack for the rest. *)

    datatype pregLoc = InRax | OnStack of int
    
    (* The state of the condition codes. *)
    datatype ccState = CCIndeterminate | CCState of ccRef * (branchOps->branchOps)

    datatype state = NormalState of normalState | Exited
    withtype normalState =
        { valueStack : {preg: int, loc: pregLoc} list, ccState: ccState, untaggedRegs: {ureg: int, reg: reg} list }
    
    val initialState:normalState =
        {valueStack=[], ccState=CCIndeterminate, untaggedRegs=[]} (* Initial and final state. *)

    (* Return the location on the real stack immediately above the top item. *)
    local
        fun stackP [] = 0
        |   stackP ({loc=OnStack sp, ...} :: _) = sp+1
        |   stackP (_ :: stack) = stackP stack
    in
        fun stackPtr({valueStack, ...}: normalState) = stackP valueStack
    end

    local
        fun findE (_, []) = raise InternalError "findEntry: not found"
        |   findE (reg, {preg, loc} :: rest) = if reg = preg then loc else findE(reg, rest)
    in
        fun findEntry(reg, {valueStack, ...}) = findE(reg, valueStack)
    end

    fun loadSource(RegisterArgument reg, dest, state) = loadRegister(reg: abstract, dest, state)

    |   loadSource(AddressConstant a, dest, _) =
            [MoveArgument{ source=AddressConstant a, dest=RegisterArgument(GenReg dest), kind=MoveWord}]
    |   loadSource(IntegerConstant i, dest, _) =
            [MoveArgument{ source=IntegerConstant i, dest=RegisterArgument(GenReg dest), kind=MoveWord}]

    |   loadSource(MemoryLocation{ base, offset, index=NoMemIndex }, dest, state) =
            (* Use the destination register as a base address. *)
            MoveArgument {
                source=MemoryLocation {base=(GenReg dest), offset=offset, index=NoMemIndex },
                dest=RegisterArgument(GenReg dest), kind=MoveWord} ::
                    loadRegister(base, dest, state)

    |   loadSource _ = raise InternalError "loadSource"
    
    and loadRegister(PReg(src, PRegGeneral), dest, state) =
    (
        case findEntry (src, state) of
            InRax =>
                if dest = eax
                then []
                else [MoveArgument{ source=RegisterArgument(GenReg eax), dest=RegisterArgument(GenReg dest), kind=MoveWord}]
        |   OnStack s => loadStack(dest, (stackPtr state - s - 1) * wordSize)
    )
    |   loadRegister _ = raise InternalError "loadRegister"

    val loadSource =
        fn(arg, dest, state as {valueStack={preg=_, loc=InRax } :: _, ...}) =>
            if dest = eax then raise InternalError "loadSource: eax" else loadSource(arg, dest, state)
        |  (arg, dest, state) => loadSource(arg, dest, state)
   
    fun clearCC({valueStack, untaggedRegs, ...}:normalState): normalState =
        {valueStack=valueStack, ccState=CCIndeterminate, untaggedRegs=untaggedRegs}

    (* After we use an untagged value remove it from the state.  We only have
       a few registers and only use at most two untagged values before tagging them.
       For now, if we tag a value we clear the untagged set.  Division returns two
       tagged values but only one is actually used. *)
    fun removeAfterTag({valueStack, ccState, ...}, _) =
        {valueStack=valueStack, ccState=ccState, untaggedRegs=[]}

    fun codeICodeFunctionToX86{icode, functionName, maxLabels, maxPRegs, argRegsUsed, hasFullClosure, currentStackArgs, debugSwitches} =
    let
        (* We may need to create additional labels. *)
        val labelCounter = ref maxLabels
        fun newLabel() = ILabel(!labelCounter) before labelCounter := !labelCounter + 1

        val maxStack = ref 0 (* High-water mark *)

        (* If the top of the stack is in RAX push it otherwise do nothing. *)
        fun pushRax (state as {valueStack={preg, loc=InRax} :: stack, ccState, untaggedRegs }) =
            let
                val sp = stackPtr state
                val () = if sp >= !maxStack then maxStack := sp+1 else ()
            in
                (* Return the updated state with the value pushed.  Pushing does not affect condition codes. *)
                ({valueStack={preg=preg, loc=OnStack sp} :: stack, ccState=ccState, untaggedRegs=untaggedRegs},
                    [PushValue{arg=RegisterArgument raxAsArg}])
            end
        |   pushRax stack = (stack, [])

        (* Add a pseudo-register containing rax to the top of the stack. *)
        fun addRaxToState({valueStack, ccState, untaggedRegs}, destReg) =
            {valueStack={preg=destReg, loc=InRax} :: valueStack, ccState=ccState, untaggedRegs=untaggedRegs}

        (* Check that the top of the stack is the required pseudo-register and the value is in Rax.
           Returns the state with this item popped. *)
        fun checkTopInRax({valueStack={preg, loc=InRax } :: resultState, ccState, untaggedRegs}, checkReg) =
            if preg = checkReg
            then {valueStack=resultState, ccState=ccState, untaggedRegs=untaggedRegs}
            else raise InternalError "checkTopInRax"
        |   checkTopInRax _ = raise InternalError "checkTopInRax"

        fun loadToRax(state, arg) =
        let
            val (pushedState, pushCode) = pushRax state
            val load = loadSource(arg, eax, pushedState)
        in
            (pushedState, load @ pushCode)
        end
 
        (* Reset the stack to value in the original state except that
           the target register should be moved into the result state. *)
        fun resetStack(state, oldState, target) =
        let
            (* Check that the target is on the top of the stack and in rax. *)
            val _ = checkTopInRax(state, target)
            (* Check that the top of the old state is not in a register. *)
            val () =
                case oldState of
                    {valueStack={loc=InRax, ...} :: _, ...} => raise InternalError "resetStack"
                |   _ => ()
            val currentSp = stackPtr state
            and oldSp = stackPtr oldState
            val newState = addRaxToState(oldState, target)
            val code =
                if currentSp = oldSp then [] else [resetStackPtr(currentSp - oldSp)]
        in
            (* Currently ResetStackPtr uses arithmetic operations that affect the condition codes. *)
            (clearCC newState, code)
        end
        
        and resetStackPtr numWords =
            if numWords < 0 then raise InternalError "resetStackPtr"
            else ResetStackPtr{numWords=numWords}

        (* Find a real register for an untagged pseudo-register value. *)
        fun getUreg(iReg, {untaggedRegs, ...}: normalState) =
            case List.find (fn {ureg, ...} => ureg = iReg) untaggedRegs of
                SOME {reg as GenReg _, ...} => reg
            |   _ => raise InternalError "getUreg"

        fun getFPreg(iReg, {untaggedRegs, ...}: normalState) =
            case List.find (fn {ureg, ...} => ureg = iReg) untaggedRegs of
                SOME {reg as XMMReg _, ...} => reg
            |   SOME {reg as FPReg _, ...} => reg
            |   _ => raise InternalError "getFPreg"

        (* Get a new untagged register.  We only handle three at the moment. *)
        fun newUreg (iReg, {valueStack, untaggedRegs, ccState}: normalState) =
        let
            val newReg =
                case untaggedRegs of
                    [] => edx
                |   [_] => ecx
                |   [_, _] => esi
                |   _ => raise InternalError "newUreg"
        in
            ({valueStack=valueStack, ccState=ccState, untaggedRegs={ureg=iReg, reg=GenReg newReg}::untaggedRegs}, (GenReg newReg))
        end

        (* Process the code until the end.  The code list is built up in reverse order. *)
        (* If we reach the end we haven't exited or reached a JumpLoop so we just return the
           final state. *)
        fun codeICode{icode=[]: abstract x86ICode list, state: state, code: reg x86ICode list, ...} = (state, code: reg x86ICode list)

            (* Destination of a forward jump.  If the state was Exited this can
               continue.
               We should have the label in the list.
               We need to merge the states associated with dropping through and the
               state associated with the branch.  We can only adjust the state on the
               "drop through" side so if we need to change the state on the branch
               side we have to create a new branch. *)
        |   codeICode{icode=(ForwardJumpLabel{label, result}: abstract x86ICode) :: rest, state, code: reg x86ICode list, context as {forwardLabels, loopArgs, handlers}} =
            let
                (* Find the label in the list.  Remove it if it's there.  It may not be if
                   we never used it.  That could happen if we have a "drop-through" case in
                   an andalso/orelse or if we didn't generate a jump because we raised an
                   exception or made a tail-jump. *)
                fun findAndRemoveLabel(label: iLabel, (hd as (l, _)) :: tl) =
                    if label = l
                    then (SOME hd, tl)
                    else
                    let
                        val (found, newTail) = findAndRemoveLabel(label, tl)
                    in
                        (found, hd::newTail)
                    end
                |   findAndRemoveLabel _ = (NONE, [])
                val (labelCondition, newLabels) = findAndRemoveLabel(label, forwardLabels)
            in
                case (state, labelCondition) of
                    (state, NONE) => (* We didn't actually take a branch.  Discard the label and continue. *)
                        codeICode{icode=rest, state=state, code=code, context={forwardLabels=newLabels, loopArgs=loopArgs, handlers=handlers}}

                |   (Exited, SOME(_, labelState)) =>
                        (* There is no drop-through to consider because the preceding instruction
                           was an unconditional jump or something similar.  We can just set the state
                           and continue. *)
                        codeICode{icode=rest, state=NormalState labelState, code=ForwardJumpLabel{label=label, result=NONE} :: code,
                                  context={forwardLabels=newLabels, loopArgs=loopArgs, handlers=handlers}}

                |   (NormalState cstate, SOME(_, labelState)) =>
                    (* There was a drop-through.  Merge the states.  We can only actually change the
                       drop-through state i.e. before the label.  If the state on the incoming branch
                       needs to be changed we have to add an extra branch. *)
                    let
                        (* We should have the result register on both arms. *)
                        fun removeResult s =
                            case result of
                                NONE => s
                            |   SOME resArg => checkTopInRax(s, pRegNum resArg)
                            
                        val dropThrough = removeResult cstate and labState = removeResult labelState
                        (* After removing the top item the tails of the states will match up to the
                           point they diverged.  The common part is the resulting state. *)
                        fun matchStates([], _) = []
                        |   matchStates(_, []) = []
                        |   matchStates((hd as {loc=loc1, preg=preg1})::l1, {loc=loc2, preg=preg2}::l2) =
                            if preg1 <> preg2
                            then []
                            else if loc1 <> loc2
                            then raise InternalError "matchStates"
                                else hd :: matchStates(l1, l2)

                        val composite =
                            {valueStack=List.rev(matchStates(List.rev (#valueStack dropThrough), List.rev (#valueStack labState))),
                             ccState=CCIndeterminate, untaggedRegs= []}
                        (* Currently the only thing we need to adjust is the stack pointer. *)
                        val finalSp = stackPtr composite
                        and labelSp = stackPtr labelState

                        val (finalState, adjustSpOnDropThrough) =
                            case result of
                                NONE =>
                                let
                                    val currentSp = stackPtr cstate and finalSp = stackPtr composite
                                in
                                    (composite, if currentSp = finalSp then [] else [resetStackPtr(currentSp-finalSp)])
                                end
                            |   SOME resReg => resetStack(cstate, composite, pRegNum resReg)
                    in
                        if labelSp = finalSp
                        then (* We don't need to do anything to the incoming branch. *)
                        let
                            val newContext = {forwardLabels=newLabels, loopArgs=loopArgs, handlers=handlers}
                        in
                            codeICode{icode=rest, state=NormalState finalState,
                                      code=ForwardJumpLabel{label=label, result=NONE} :: adjustSpOnDropThrough @ code,
                                      context=newContext}
                        end
                        else (* We're going to have to adjust the stack pointer on the incoming branch.
                                We can't actually do that so instead we have to branch and merge again. *)
                        let
                            val extraLabel = newLabel()
                        in
                            codeICode{
                                (* Put in an unconditional jump followed by the label we're currently trying to merge in.
                                   Because it's following an unconditional jump the state will simply be set to the
                                   label state.  When we merge in again we will be able to adjust the stack pointer and
                                   it should all work. *)
                                icode=UnconditionalForwardJump{label=extraLabel} :: ForwardJumpLabel{label=label, result=result} ::
                                      ForwardJumpLabel{label=extraLabel, result=result} :: rest,
                                state=NormalState finalState, code=adjustSpOnDropThrough @ code,
                                context=context (* Original context - we haven't fixed up the label yet. *)}
                        end
                    end
            end

            (* Start of a handler.  This is similar to a ForwardLabel and should only occur in an Exited state.
               The handlers form a stack so the one we want should be on the top. *)
        |   codeICode{
                icode=(BeginHandler{handleStart, packetReg, ...}) :: rest, state=Exited, code,
                context={forwardLabels, loopArgs, handlers=(handleLab, handlerStartState) :: otherHandlers}} =
            let
                val _ = handleLab = handleStart orelse raise InternalError "codeICode: BeginHandler handler stack mismatch"
                val resultState = addRaxToState(handlerStartState, pRegNum packetReg)
            in
                codeICode{icode=rest, state=NormalState resultState,
                        code=BeginHandler{handleStart=handleStart, workReg=rbxAsArg, packetReg=raxAsArg} :: code,
                        context={forwardLabels=forwardLabels, loopArgs=loopArgs, handlers=otherHandlers}}
            end

        |   codeICode{icode=BeginHandler _ :: _, ...} =
                raise InternalError "codeICode: BeginHandler not preceded by jump or not in handler"
                
        |   codeICode{icode=EndLoop{loopLabel} :: rest, state, code, context={forwardLabels, loopArgs=(_, lab)::otherLoops, handlers}} =
            let
                val _ = lab = loopLabel orelse raise InternalError "codeICode: EndLoop - mismatched labels"
            in
                codeICode{icode=rest, state=state, code=code,
                        context={forwardLabels=forwardLabels, loopArgs=otherLoops, handlers=handlers}}
            end

        |   codeICode{icode=EndLoop _ :: _, ...} = raise InternalError "codeICode: EndLoop outside loop"

            (* Any other Exited conditions. Skip the instructions.  There are various situations where this
               can arise.  e.g. OS.Process.exit contains an infinite loop processing atExit functions
               until it eventually exits by calling a "stop" function. *)
        |   codeICode{icode=_ :: rest, state=Exited, code, context} =
               codeICode{icode=rest, state=Exited, code=code, context=context}

        |   codeICode{icode=(UnconditionalForwardJump{label}) :: rest, state=NormalState state, code, context={forwardLabels, loopArgs, handlers}} =
                codeICode{icode=rest, state=Exited, code=UnconditionalForwardJump{label=label} :: code,
                                      context={forwardLabels=(label, state) :: forwardLabels, loopArgs=loopArgs, handlers=handlers}}

        |   codeICode{icode=ConditionalForwardJump{ccRef, condition, label} :: rest,
                      state as NormalState labelState, code, context={forwardLabels, loopArgs, handlers}} =
            let
                (* Get the condition code.  It should be the state we set but we may have to change the
                   test if that wasn't the original. *)
                val testCondition =
                    case labelState of
                        {ccState=CCState(currentRef, condOp), ...} =>
                            if currentRef = ccRef then condOp condition
                            else raise InternalError "codeICode: condition ref incorrect"
                    |   _ => raise InternalError "codeICode: condition ref unset"
            in
                codeICode{icode=rest, state=state, code=ConditionalForwardJump{ ccRef=ccRef, condition=testCondition, label=label } :: code,
                                      context={forwardLabels=(label, labelState) :: forwardLabels, loopArgs=loopArgs, handlers=handlers}}
            end

            (* Word comparison. *)
        |   codeICode{icode=WordComparison{arg1 as RegisterArgument(PReg(_, PRegGeneral)), arg2=IntegerConstant i, ccRef} :: rest,
                      state=NormalState state, code, context} =
            let
                (* The value on the top of the stack should be in rax and the value we want to test. *)
                val (startState, loadCode) = loadToRax(state, arg1)
                (* The resulting state contains the condition we want to test. *)
                val checkState =
                    {valueStack= #valueStack startState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= []}
            in
                codeICode{icode=rest, state=NormalState checkState,
                          code=WordComparison{ arg1=RegisterArgument raxAsArg, arg2=IntegerConstant i, ccRef=ccRef } :: loadCode @ code, context=context}
            end

        |   codeICode{icode=WordComparison{arg1 as RegisterArgument(PReg(_, PRegGeneral)), arg2=AddressConstant a, ccRef} :: rest,
                      state=NormalState state, code, context} =
            let
                (* The value on the top of the stack should be in rax and the value we want to test. *)
                val (startState, loadCode) = loadToRax(state, arg1)
                val checkState = {valueStack= #valueStack startState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= []}
            in
                codeICode{icode=rest, state=NormalState checkState,
                          code=WordComparison{ arg1=RegisterArgument raxAsArg, arg2=AddressConstant a, ccRef=ccRef } :: loadCode @ code,
                          context=context}
            end

        |   codeICode{icode=WordComparison{arg1=RegisterArgument(PReg(testR1, PRegGeneral)), arg2 as RegisterArgument(PReg(_, PRegGeneral)), ccRef} :: rest,
                      state=NormalState state, code, context=context} =
            let
                (* The second argument should be on the top of the stack and in rax. *)
                val (startState, loadCode) = loadToRax(state, arg2)
                val secondArg =
                    case findEntry (testR1, startState) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: JumpOnWordComparison"
                (* We want the first argument in the register so that means the condition codes
                   we will test are reversed.. *)
                fun revCond JA  = JB
                |   revCond JNA = JNB
                |   revCond JB  = JA
                |   revCond JNB = JNA
                |   revCond JL  = JG
                |   revCond JLE = JGE
                |   revCond JG  = JL
                |   revCond JGE = JLE
                |   revCond t   = t
                val checkState = {valueStack= #valueStack startState, ccState=CCState(ccRef, revCond), untaggedRegs= []}
            in
                codeICode{icode=rest, state=NormalState checkState,
                          code=WordComparison{arg1=RegisterArgument raxAsArg, arg2=wordOffsetAddress(stackPtr startState - secondArg - 1, esp), ccRef=ccRef} ::
                               loadCode @ code,
                          context=context}
            end

        |   codeICode{
                icode=WordComparison{
                    arg1 as MemoryLocation{index=NoMemIndex, ...},
                    arg2=MemoryLocation{offset=offset2, base=bReg2, index=NoMemIndex}, ccRef} :: rest,
                state=NormalState state, code, context=context} =
                (* This is used for LargeWord comparisons. *)
            let
                val (startState as { valueStack, untaggedRegs, ...}, loadArg2Base) =
                    loadToRax(state, RegisterArgument bReg2)
                (* We use an untagged register temporarily. *)
                val newReg =
                    case untaggedRegs of
                        [] => edx
                    |   [_] => ecx
                    |   _ => raise InternalError "newUreg"
                val loadArg1 = loadSource(arg1, newReg, startState)
                val resultState = {valueStack= valueStack, ccState=CCState(ccRef, fn i=>i), untaggedRegs= []}
            in
                codeICode{icode=rest, state=NormalState resultState,
                          code=
                            WordComparison{arg1=RegisterArgument(GenReg newReg),
                                           arg2=MemoryLocation{offset=offset2, base=raxAsArg, index=NoMemIndex}, ccRef=ccRef} ::
                                loadArg1 @ loadArg2Base @ code,
                          context=context}
            end
            
        |   codeICode{icode=WordComparison _ :: _, ...} =
                raise InternalError "codeICode: JumpOnWordComparison TODO"

        |   codeICode{icode=TestTagBit{arg, ccRef} :: rest, state=NormalState state, code, context} =
            let
                (* The value on the top of the stack should be in rax and the value we want to test. *)
                val (startState, loadCode) = loadToRax(state, arg)
                val checkState = {valueStack= #valueStack startState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= []}
            in
                codeICode{icode=rest, state=NormalState checkState,
                    (* TestTagBit only sets the condition codes.  It doesn't affect the register. *)
                          code=TestTagBit{arg=RegisterArgument raxAsArg, ccRef=ccRef} :: loadCode @ code, context=context}
            end

            (* Set up an exception handler. *)
        |   codeICode{icode=PushExceptionHandler{ handlerAddr, handleStart } :: rest, state=NormalState state, code, context={forwardLabels, loopArgs, handlers}} =
            let
                val sHandle = pRegNum handlerAddr
                val (pushedState, pushCode) = pushRax state
                val sp = stackPtr pushedState
                val thisHandler = (handleStart, pushedState)
                val startState =
                    {valueStack={preg=sHandle, loc=OnStack(sp+1) (* Two words *)} :: #valueStack pushedState,
                     ccState=CCIndeterminate, untaggedRegs= []}
                val newContext = {forwardLabels=forwardLabels, loopArgs=loopArgs, handlers=thisHandler :: handlers}
            in
                codeICode{icode=rest, state=NormalState startState,
                          code=PushExceptionHandler{handlerAddr=raxAsArg, handleStart=handleStart} :: pushCode @ code, context=newContext}
            end

            (* Pop an exception handler at the end of a handled section.  Executed if no exception has been raised. *)
        |   codeICode{icode=PopExceptionHandler{ handlerAddr, resultReg, workReg=_ } :: rest, state=NormalState state, code, context} =
            let
                val resReg = pRegNum resultReg and handleAddr = pRegNum handlerAddr
                (* handlerStartState is the state BEFORE we pushed the handler. *)
                (* Search down the state until we find the handler. *)
                fun popState [] = raise InternalError "codeICode: PopExceptionHandler - not found"
                |   popState (hState as ({preg, ...} :: tail)) =
                        if preg = handleAddr then hState else popState tail
                val handlerStartState = {valueStack=popState(#valueStack state), ccState=CCIndeterminate, untaggedRegs= []}
                (* Reset to just above the handler. *)
                val (_, resetCode) = resetStack(state, handlerStartState, resReg)
                (* The result state is after the handler has been popped. *)
                val resultState =
                    addRaxToState({valueStack=tl(#valueStack handlerStartState), ccState=CCIndeterminate, untaggedRegs= []}, resReg)
                val workReg = (GenReg ebx) (* Must be different from the return register. *)
            in
                codeICode{icode=rest, state=NormalState resultState,
                          code=PopExceptionHandler{ handlerAddr=raxAsArg (* unused*), resultReg=raxAsArg (* unused*), workReg=workReg } :: resetCode @ code,
                          context=context}
            end

            (* MoveArgument - this is used for loads, stores and moves.  *)
            (* Create a new Preg containing the contents of another Preg.
               We only ever want the top item to be in RAX so we push the top of the stack. *)
        |   codeICode{
                icode=MoveArgument{source as RegisterArgument(PReg(_, PRegGeneral)), dest=RegisterArgument(PReg(destReg, PRegGeneral)), kind=MoveWord} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val load = loadSource(source, eax, pushedState)
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=load @ pushCode @ code, context=context}
            end
            
            (* Move a constant to a preg.  First push anything in RAX then load the constant into RAX. *)
        |   codeICode{icode=MoveArgument{source=AddressConstant srcValue, dest=RegisterArgument(PReg(destReg, PRegGeneral)), kind} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                (* The constant should be an address. *)
                val _ = isShort srcValue andalso raise InternalError "AddressConstant: short"
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=MoveArgument{source=AddressConstant srcValue, dest=RegisterArgument raxAsArg, kind=kind} :: pushCode @ code,
                    context=context}
            end

            (* Move a short constant to a preg.  First push anything in RAX then load the constant into RAX. *)
        |   codeICode{icode=MoveArgument{source=IntegerConstant i, dest=RegisterArgument(PReg(destReg, PRegGeneral)), kind} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=MoveArgument{source=IntegerConstant i, dest=RegisterArgument raxAsArg, kind=kind} :: pushCode @ code,
                    context=context}
            end

            (* Move a short constant to an untagged reg. *)
        |   codeICode{icode=MoveArgument{source=IntegerConstant i, dest=RegisterArgument(PReg(destReg, PRegUntagged)), kind} :: rest, state=NormalState state, code, context} =
            let
                val (resultState, resReg) = newUreg(destReg, state)
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code=MoveArgument{source=IntegerConstant i, dest=RegisterArgument resReg, kind=kind} :: code,
                    context=context}
            end

            (* Load a value from a piece of memory, typically the offset in a tuple. *)
        |   codeICode{
                icode=MoveArgument{ source=MemoryLocation{offset, base as PReg(_, PRegGeneral), index=NoMemIndex}, dest=RegisterArgument(PReg(destReg, dType)), kind} :: rest,
                state=NormalState state, code, context} =
            let
                (* This previously replaced the top.  Pushing the previous value is safer but means
                   values accumulate on the stack. *)
                val (pushedState, pushCode) = pushRax state
                val load = loadSource(RegisterArgument base, eax, pushedState)
                val (resultState, resultReg) =
                    case dType of
                        PRegGeneral => (addRaxToState(pushedState, destReg), raxAsArg)
                    |   PRegUntagged => newUreg(destReg, pushedState)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=MoveArgument{ source=MemoryLocation{offset=offset, base=raxAsArg, index=NoMemIndex}, dest=RegisterArgument resultReg, kind=kind} :: 
                        load @ pushCode @ code, context=context}
            end

        |   codeICode{
                icode=MoveArgument{ source=MemoryLocation{offset, base=PReg(bReg, PRegUntagged), index=NoMemIndex}, dest=RegisterArgument(PReg(destReg, dType)), kind} :: rest,
                state=NormalState(state as {untaggedRegs, ...}), code, context} =
            let
                (* CMem loads with a fixed offset *)
                val realBReg =
                    case List.find (fn {ureg, ...} => ureg = bReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val (resultState, resultReg) =
                    case (dType, kind, fpMode) of
                        (PRegUntagged, MoveFloat, FPModeX87) =>
                            ({valueStack= #valueStack state, ccState= #ccState state,
                                untaggedRegs =[{ureg=destReg, reg=FPReg fp0}]}, (FPReg fp0))
                    |   (PRegUntagged, MoveFloat, FPModeSSE2) =>
                            ({valueStack= #valueStack state, ccState= #ccState state,
                                untaggedRegs =[{ureg=destReg, reg=XMMReg xmm0}]}, (XMMReg xmm0))
                    |   (PRegUntagged, MoveDouble, FPModeX87) =>
                            ({valueStack= #valueStack state, ccState= #ccState state,
                                untaggedRegs =[{ureg=destReg, reg=FPReg fp0}]}, (FPReg fp0))
                    |   (PRegUntagged, MoveDouble, FPModeSSE2) =>
                            ({valueStack= #valueStack state, ccState= #ccState state,
                                untaggedRegs =[{ureg=destReg, reg=XMMReg xmm0}]}, (XMMReg xmm0))
                    |   (PRegGeneral, MoveFloat, _) => raise InternalError "MoveFloat to normal register"
                    |   (PRegGeneral, MoveDouble, _) => raise InternalError "MoveDouble to normal register"
                    |   (PRegGeneral, _, _) => (addRaxToState(state, destReg), raxAsArg)
                    |   (PRegUntagged, _, _) => newUreg(destReg, state)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=MoveArgument{ source=MemoryLocation{offset=offset, base=realBReg, index=NoMemIndex}, dest=RegisterArgument resultReg, kind=kind} :: 
                        code, context=context}
            end

        |   codeICode{
                icode=MoveArgument{ source=MemoryLocation {offset, base, index=MemIndex1(PReg(iReg, PRegUntagged))}, dest=RegisterArgument(PReg(destReg, dType)), kind} :: rest,
                state=NormalState (state as {untaggedRegs, ...}), code, context} =
            let
                (* The index is an untagged value. *)
                val realIReg =
                    case List.find (fn {ureg, ...} => ureg = iReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                (* The base may be a tagged or untagged register.  CMem loads extract the base address from
                   a LargeWord.word boxed value into an untagged register. *)
                val (realBReg, codeBreg) =
                    case base of
                        PReg(bReg, PRegGeneral) =>
                        let
                            val baseOnStack =
                                case findEntry (bReg, state) of
                                    OnStack s => s
                                |   _ => raise InternalError "codeICode: MoveArgument"
                            val code =
                                [MoveArgument{source=wordOffsetAddress(stackPtr state - baseOnStack - 1, esp), dest=RegisterArgument rbxAsArg, kind=MoveWord}]
                        in
                            (rbxAsArg, code)
                        end
                    |   PReg(bReg, PRegUntagged) =>
                        let
                            val realBReg =
                                case List.find (fn {ureg, ...} => ureg = bReg) untaggedRegs of
                                    SOME {reg as GenReg _, ...} => reg
                                |   _ => raise InternalError "codeICode: find tagged"
                        in
                            (realBReg, [])
                        end
                val (resultState, resultReg) =
                    case dType of
                        PRegGeneral => (addRaxToState(state, destReg), raxAsArg)
                    |   PRegUntagged =>
                            ({valueStack= #valueStack state, ccState= #ccState state,
                                untaggedRegs =[{ureg=destReg, reg=GenReg ecx}]}, rcxAsArg)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code =
                        MoveArgument{source=MemoryLocation {offset=offset, base=realBReg, index=MemIndex1 realIReg}, dest=RegisterArgument resultReg, kind=kind} ::
                        codeBreg @ code,
                    context=context}
            end

        |   codeICode{
                icode=MoveArgument{ source=MemoryLocation {offset, base, index}, dest=RegisterArgument(PReg(destReg, dType)), kind} :: rest,
                state=NormalState state, code, context} =
            let
                (* CMem operations will have pushed the index.  The easiest way to proceed is always
                   to do that. *)
                val (pushedState as { untaggedRegs, ...}, pushCode) = pushRax state
                val (iReg, indexed) =
                    case index of
                        MemIndex1 indexArg => (indexArg, MemIndex1 raxAsArg)
                    |   MemIndex2 indexArg => (indexArg, MemIndex2 raxAsArg)
                    |   MemIndex4 indexArg => (indexArg, MemIndex4 raxAsArg)
                    |   MemIndex8 indexArg => (indexArg, MemIndex8 raxAsArg)
                    |   _ => raise InternalError "codeICode: MemIndex"
                val (realBReg, codeBreg) =
                    case base of
                        PReg(_, PRegGeneral) => (rbxAsArg, loadSource(RegisterArgument base, ebx, pushedState))
                    |   PReg(bReg, PRegUntagged) =>
                        let
                            val realBReg =
                                case List.find (fn {ureg, ...} => ureg = bReg) untaggedRegs of
                                    SOME {reg as GenReg _, ...} => reg
                                |   _ => raise InternalError "codeICode: find tagged"
                        in
                            (realBReg, [])
                        end
                        
                val loadCode = codeBreg @ loadSource(RegisterArgument iReg, eax, pushedState)
                val (resultState, resultReg) =
                    case (dType, kind, fpMode) of
                        (PRegUntagged, MoveFloat, FPModeX87) =>
                            ({valueStack= #valueStack pushedState, ccState= #ccState pushedState,
                                untaggedRegs =[{ureg=destReg, reg=FPReg fp0}]}, (FPReg fp0))
                    |   (PRegUntagged, MoveFloat, FPModeSSE2) =>
                            ({valueStack= #valueStack pushedState, ccState= #ccState pushedState,
                                untaggedRegs =[{ureg=destReg, reg=XMMReg xmm0}]}, (XMMReg xmm0))
                    |   (PRegUntagged, MoveDouble, FPModeX87) =>
                            ({valueStack= #valueStack pushedState, ccState= #ccState pushedState,
                                untaggedRegs =[{ureg=destReg, reg=FPReg fp0}]}, (FPReg fp0))
                    |   (PRegUntagged, MoveDouble, FPModeSSE2) =>
                            ({valueStack= #valueStack pushedState, ccState= #ccState pushedState,
                                untaggedRegs =[{ureg=destReg, reg=XMMReg xmm0}]}, (XMMReg xmm0))
                    |   (PRegGeneral, MoveFloat, _) => raise InternalError "MoveFloat to normal register"
                    |   (PRegGeneral, MoveDouble, _) => raise InternalError "MoveDouble to normal register"
                    |   (PRegGeneral, _, _) => (addRaxToState(pushedState, destReg), raxAsArg)
                    |   (PRegUntagged, _, _) =>
                            ({valueStack= #valueStack pushedState, ccState= #ccState pushedState,
                                untaggedRegs =[{ureg=destReg, reg=GenReg ecx}]}, rcxAsArg)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code =
                        MoveArgument{source=MemoryLocation {offset=offset, base=realBReg, index=indexed}, dest=RegisterArgument resultReg, kind=kind} ::
                        loadCode @ pushCode @ code,
                    context=context}
            end

            (* Store a pseudo-register in a memory location. *)
        |   codeICode{
                icode=MoveArgument{
                        source=RegisterArgument(PReg(valueReg, PRegUntagged)),
                        dest=MemoryLocation{offset, base as PReg(_, PRegGeneral), index=NoMemIndex}, kind} :: rest,
                      state=NormalState state, code, context} =
            let
                val (pushedState as { valueStack, ccState, untaggedRegs}, pushCode) = pushRax state
                val baseOnStack = 
                    case findEntry (pRegNum base, pushedState) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val realVReg =
                    case List.find (fn {ureg, ...} => ureg = valueReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val loadBaseToEax = loadStack(eax, (stackPtr pushedState - baseOnStack -1) * wordSize)
                val store =
                    [MoveArgument{
                        source=RegisterArgument realVReg,
                        dest=MemoryLocation{base=(GenReg eax), offset=offset, index=NoMemIndex}, kind=kind}]
                val resultState = { valueStack= valueStack, ccState=ccState, untaggedRegs=[]}
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code=store @ loadBaseToEax @ pushCode @ code, context=context}
            end

            (* As above except the base register is in an untagged register.  This is used for CMem stores. *)
        |   codeICode{
                icode=MoveArgument{
                        source=RegisterArgument(PReg(valueReg, PRegUntagged)),
                        dest=MemoryLocation{offset, base=PReg(bReg, PRegUntagged), index=NoMemIndex}, kind} :: rest,
                      state=NormalState{untaggedRegs, valueStack, ccState, ...}, code, context} =
            let
                (* CMem loads with a fixed offset *)
                val realBReg =
                    case List.find (fn {ureg, ...} => ureg = bReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val realVReg =
                    case List.find (fn {ureg, ...} => ureg = valueReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val store =
                    [MoveArgument{
                        source=RegisterArgument realVReg,
                        dest=MemoryLocation{base=realBReg, offset=offset, index=NoMemIndex}, kind=kind}]
                val resultState = { valueStack= valueStack, ccState=ccState, untaggedRegs=[]}
            in
                codeICode{
                    icode=rest, state=NormalState resultState, code=store @ code, context=context}
            end

            (* Store an untagged register in a memory location.  This is currently used to store the string
               length word. *)
        |   codeICode{icode=MoveArgument{ source=RegisterArgument(PReg(srcReg, PRegGeneral)), dest=MemoryLocation{offset, base, index=NoMemIndex}, kind} :: rest,
                      state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val baseOnStack = 
                    case findEntry (pRegNum base, pushedState) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val valueOnStack =
                    case findEntry (srcReg, pushedState) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val loadValueToEbx = loadStack(ebx, (stackPtr pushedState - valueOnStack -1) * wordSize)
                val loadBaseToEax = loadStack(eax, (stackPtr pushedState - baseOnStack -1) * wordSize)
                (* Pop the value to store into ebx and store it in the tuple. *)
                val store =
                    [MoveArgument{
                        source=RegisterArgument(GenReg ebx),
                        dest=MemoryLocation{base=(GenReg eax), offset=offset, index=NoMemIndex}, kind=kind}]
            in
                codeICode{
                    icode=rest, state=NormalState pushedState,
                    code=store @ loadBaseToEax @ loadValueToEbx @ pushCode @ code, context=context}
            end

            (* Store a constant in memory. *)
        |   codeICode{icode=MoveArgument{ source=AddressConstant value, dest=MemoryLocation{offset, base, index=NoMemIndex}, kind} :: rest,
                      state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val baseOnStack = 
                    case findEntry (pRegNum base, pushedState) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val loadBaseToEax = loadStack(eax, (stackPtr pushedState - baseOnStack -1) * wordSize)
                val store =
                    [MoveArgument{
                        source=AddressConstant value,
                        dest=MemoryLocation{base=(GenReg eax), offset=offset, index=NoMemIndex}, kind=kind}]
            in
                codeICode{
                    icode=rest, state=NormalState pushedState,
                    code=store @ loadBaseToEax @ pushCode @ code, context=context}
            end

            (* Store a constant in memory. *)
        |   codeICode{icode=MoveArgument{ source=IntegerConstant value, dest=MemoryLocation{offset, base, index=NoMemIndex}, kind} :: rest,
                      state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val baseOnStack = 
                    case findEntry (pRegNum base, pushedState) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val loadBaseToEax = loadStack(eax, (stackPtr pushedState - baseOnStack -1) * wordSize)
                val store =
                    [MoveArgument{
                        source=IntegerConstant value,
                        dest=MemoryLocation{base=(GenReg eax), offset=offset, index=NoMemIndex}, kind=kind}]
            in
                codeICode{
                    icode=rest, state=NormalState pushedState,
                    code=store @ loadBaseToEax @ pushCode @ code, context=context}
            end

        |   codeICode{icode=MoveArgument{ source=RegisterArgument(PReg(valueReg, PRegGeneral)), dest=MemoryLocation {offset, base, index}, kind} :: rest,
                state=NormalState state, code, context} =
            let
                (* The top of the stack should be the value to store.  That will be in rax.
                   We load the base address into rcx and the index into rbx. *)
                val startState = checkTopInRax(state, valueReg)
                val (iReg, indexed) =
                    case index of
                        MemIndex1(PReg(r, PRegGeneral)) => (r, MemIndex1 rbxAsArg)
                    |   MemIndex2(PReg(r, PRegGeneral)) => (r, MemIndex2 rbxAsArg)
                    |   MemIndex4(PReg(r, PRegGeneral)) => (r, MemIndex4 rbxAsArg)
                    |   MemIndex8(PReg(r, PRegGeneral)) => (r, MemIndex8 rbxAsArg)
                    |   _ => raise InternalError "codeICode: MemIndex"
                val indexOnStack =
                    case findEntry (iReg, state) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val baseOnStack =
                    case findEntry (pRegNum base, state) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val rcxAsArg = (GenReg ecx)
            in
                codeICode{icode=rest, state=NormalState startState,
                    code =
                        MoveArgument{source=RegisterArgument raxAsArg, dest=MemoryLocation {offset=offset, base=rcxAsArg, index=indexed}, kind=kind} ::
                        MoveArgument{source=wordOffsetAddress(stackPtr state - baseOnStack - 1, esp), dest=RegisterArgument rcxAsArg, kind=MoveWord} ::
                        MoveArgument{source=wordOffsetAddress(stackPtr state - indexOnStack - 1, esp), dest=RegisterArgument rbxAsArg, kind=MoveWord} ::
                        code,
                    context=context}
            end

        |   codeICode{icode=
                       MoveArgument{ source=RegisterArgument(PReg(valueReg, PRegUntagged)),
                                     dest=MemoryLocation {offset, base, index=MemIndex1(PReg(iReg, PRegUntagged))}, kind} :: rest,
                    state=NormalState state, code, context} =
            let
                (* This is used for index stores of bytes.  N.B.  If we're storing a byte from a register
                   we must not use ESI or EDI for the value register in 32-bit mode.  That's because the
                   encoding actually refers to DH and BH.  In 64-bit mode the REX prefix overrides this. *)
                val ({ valueStack, ccState, untaggedRegs}, pushCode) = pushRax state
                (* The index is an untagged value. *)
                val realIReg =
                    case List.find (fn {ureg, ...} => ureg = iReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val realVReg =
                    case List.find (fn {ureg, ...} => ureg = valueReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                (* The base may be a tagged or untagged register.  CMem stores extract the base address from
                   a LargeWord.word boxed value into an untagged register. *)
                val (realBReg, codeBreg) =
                    case base of
                        PReg(bReg, PRegGeneral) =>
                        let
                            val baseOnStack =
                                case findEntry (bReg, state) of
                                    OnStack s => s
                                |   _ => raise InternalError "codeICode: MoveArgument"
                            val code =
                                [MoveArgument{source=wordOffsetAddress(stackPtr state - baseOnStack - 1, esp), dest=RegisterArgument rbxAsArg, kind=MoveWord}]
                        in
                            (rbxAsArg, code)
                        end
                    |   PReg(bReg, PRegUntagged) =>
                        let
                            val realBReg =
                                case List.find (fn {ureg, ...} => ureg = bReg) untaggedRegs of
                                    SOME {reg as GenReg _, ...} => reg
                                |   _ => raise InternalError "codeICode: find tagged"
                        in
                            (realBReg, [])
                        end
                val resultState = { valueStack= valueStack, ccState=ccState, untaggedRegs=[]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code =
                        MoveArgument{source=RegisterArgument realVReg, dest=MemoryLocation {offset=offset, base=realBReg, index=MemIndex1 realIReg}, kind=kind} ::
                        codeBreg @ pushCode @ code,
                    context=context}
            end

        |   codeICode{icode=
                       MoveArgument{ source=RegisterArgument(PReg(valueReg, PRegUntagged)), dest=MemoryLocation {offset, base, index}, kind} :: rest,
                    state=NormalState state, code, context} =
            (* Store an untagged value with an index value in a tagged register. This is used for CMem stores. *)
            let
                val (pushedState as { untaggedRegs, valueStack, ccState, ...}, pushCode) = pushRax state
                val (iReg, indexed) =
                    case index of
                        MemIndex1 indexArg => (indexArg, MemIndex1 raxAsArg)
                    |   MemIndex2 indexArg => (indexArg, MemIndex2 raxAsArg)
                    |   MemIndex4 indexArg => (indexArg, MemIndex4 raxAsArg)
                    |   MemIndex8 indexArg => (indexArg, MemIndex8 raxAsArg)
                    |   _ => raise InternalError "codeICode: MemIndex"
                (* The base address will be in an untagged register. *)
                val (realBReg, codeBreg) =
                    case base of
                        PReg(_, PRegGeneral) => (rbxAsArg, loadSource(RegisterArgument base, ebx, pushedState))
                    |   PReg(bReg, PRegUntagged) =>
                        let
                            val realBReg =
                                case List.find (fn {ureg, ...} => ureg = bReg) untaggedRegs of
                                    SOME {reg as GenReg _, ...} => reg
                                |   _ => raise InternalError "codeICode: find tagged"
                        in
                            (realBReg, [])
                        end
                val realVReg =
                    case List.find (fn {ureg, ...} => ureg = valueReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                        
                val loadCode = codeBreg @ loadSource(RegisterArgument iReg, eax, pushedState)
                val resultState = { valueStack= valueStack, ccState=ccState, untaggedRegs=[]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code =
                        MoveArgument{source=RegisterArgument realVReg, dest=MemoryLocation {offset=offset, base=realBReg, index=indexed}, kind=kind} ::
                        loadCode @ pushCode @ code,
                    context=context}
            end

         |   codeICode{icode=
                        MoveArgument{
                            source=MemoryLocation {offset=srcOffset, base=srcBase, index=NoMemIndex},
                            dest=MemoryLocation {offset, base=PReg(bReg, PRegUntagged), index=NoMemIndex}, kind} :: rest,
                    state=NormalState state, code, context} =
            let
                (* This is used to store floating point values into C memory.  The source will be a PReg and the base will
                   be an untagged register. *)
                val _ = kind = MoveFloat orelse kind = MoveDouble orelse raise InternalError "Move memory to memory - not float"
                val (pushedState as { untaggedRegs, valueStack, ccState, ...}, pushCode) = pushRax state 
                val loadArgAddr = loadSource(RegisterArgument srcBase, ebx, pushedState)
                val resultReg =
                    case fpMode of
                        FPModeX87 => (FPReg fp0)
                    |   FPModeSSE2 => (XMMReg xmm0)
                val loadToFpReg =
                    MoveArgument{ source=MemoryLocation{offset=srcOffset, base=rbxAsArg, index=NoMemIndex}, dest=RegisterArgument resultReg, kind=MoveDouble}
                (* The base address will be in an untagged register. *)
                val realBReg =
                    case List.find (fn {ureg, ...} => ureg = bReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val storeToMem =
                    MoveArgument { source=RegisterArgument resultReg, dest=MemoryLocation {offset=offset, base=realBReg, index=NoMemIndex}, kind=kind}
                val resultState = { valueStack= valueStack, ccState=ccState, untaggedRegs=[]}
                val cvtToFloat =
                    case (fpMode, kind) of
                        (FPModeSSE2, MoveFloat) =>
                            [SSE2FPArith { opc=SSE2DoubleToFloat, resultReg=resultReg, arg1=RegisterArgument resultReg, arg2=RegisterArgument resultReg }]
                    |   _ => []
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code =
                        storeToMem :: cvtToFloat @ (loadToFpReg :: loadArgAddr @ pushCode @ code),
                    context=context}
            end
           
        |   codeICode{icode=
                        MoveArgument{
                            source=MemoryLocation {offset=srcOffset, base=srcBase, index=NoMemIndex},
                            dest=MemoryLocation {offset, base=PReg(bReg, PRegUntagged), index}, kind} :: rest,
                    state=NormalState state, code, context} =
            let
                (* This is used to store floating point values into C memory.  The source will be a PReg and the base will
                   be an untagged register.  The index will be a tagged register. *)
                val _ = kind = MoveFloat orelse kind = MoveDouble orelse raise InternalError "Move memory to memory - not float"
                val (pushedState as { untaggedRegs, valueStack, ccState, ...}, pushCode) = pushRax state 
                val loadArgAddr = loadRegister(srcBase, ebx, pushedState)
                val resultReg =
                    case fpMode of
                        FPModeX87 => (FPReg fp0)
                    |   FPModeSSE2 => (XMMReg xmm0)
                val loadToFpReg =
                    MoveArgument{ source=MemoryLocation{offset=srcOffset, base=rbxAsArg, index=NoMemIndex}, dest=RegisterArgument resultReg, kind=MoveDouble}
                val (iReg, indexed) =
                    case index of
                        MemIndex1 indexArg => (indexArg, MemIndex1 raxAsArg)
                    |   MemIndex2 indexArg => (indexArg, MemIndex2 raxAsArg)
                    |   MemIndex4 indexArg => (indexArg, MemIndex4 raxAsArg)
                    |   MemIndex8 indexArg => (indexArg, MemIndex8 raxAsArg)
                    |   _ => raise InternalError "codeICode: MemIndex - move float"
                (* The base address will be in an untagged register. *)
                val realBReg =
                    case List.find (fn {ureg, ...} => ureg = bReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val cvtToFloat =
                    case (fpMode, kind) of
                        (FPModeSSE2, MoveFloat) =>
                            [SSE2FPArith { opc=SSE2DoubleToFloat, resultReg=resultReg, arg1=RegisterArgument resultReg, arg2=RegisterArgument resultReg }]
                    |   _ => []
                val storeToMem =
                    MoveArgument { source=RegisterArgument resultReg, dest=MemoryLocation {offset=offset, base=realBReg, index=indexed}, kind=kind}
                val resultState = { valueStack= valueStack, ccState=ccState, untaggedRegs=[]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code =
                        storeToMem :: cvtToFloat @ loadRegister(iReg, eax, pushedState) @ (loadToFpReg :: loadArgAddr @ pushCode @ code),
                    context=context}
            end

            (* Other combinations of move aren't used at the moment. *)
        |   codeICode{icode=MoveArgument _ :: _, ...} =
                raise InternalError "codeICode: MoveArgument"

        |   codeICode{icode=LoadStack{ offset, dest} :: rest, state=NormalState state, code, context} =
            (* Load from the stack *)
            let
                val destReg = pRegNum dest
                val (pushedState, pushCode) = pushRax state
                (* We have to adjust the offset. *)
                val load = loadStack(eax, stackPtr pushedState * wordSize  + offset)
            in
                codeICode{icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=load @ pushCode @ code, context=context}
            end

        |   codeICode{ icode=LoadMemReg{offset, dest} :: rest, state=NormalState state, code, context} =
            (* Load from ebp/rbp *)
            let
                val destReg = pRegNum dest
                val (pushedState, pushCode) = pushRax state
                val load = loadMemory(eax, ebp, offset)
            in
                (* We also generate loads from offsets in ebp. *)
                codeICode{icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=load @ pushCode @ code, context=context}
            end
        
        |   codeICode {
                icode=BeginFunction{regArgs} :: rest, state=NormalState state, code, context} =
            let
                fun pushRegister([], state , code) = (state, code)
                |   pushRegister((dest, srcReg)::others, state, code) =
                let
                    val destReg = pRegNum dest
                    val sp = stackPtr state
                    val {valueStack, ccState, untaggedRegs, ...} = state
                    val newState =
                        {valueStack={preg=destReg, loc=OnStack sp} :: valueStack, ccState=ccState, untaggedRegs=untaggedRegs}
                in
                    pushRegister(others, newState, PushValue{arg=RegisterArgument srcReg} :: code)
                end

                val (pushedState, pushCode) = pushRax state
                val (resultState, resultCode) = pushRegister(regArgs, pushedState, pushCode @ code)
            in
                codeICode{icode=rest, state=NormalState resultState, code=resultCode, context=context}
            end
            
        |   codeICode{icode=AllocateMemoryOperation{ size, flags, dest} :: rest, state=NormalState state, code, context} =
            let
                val destReg = pRegNum dest
                val (pushedState, pushCode) = pushRax state
                val resultState = clearCC(addRaxToState(pushedState, destReg)) (* Affects CC *)
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code=AllocateMemoryOperation{ size=size, flags=flags, dest=raxAsArg} :: pushCode @ code, context=context}
            end

        |   codeICode{icode=AllocateMemoryVariable{ size, flags, dest, initialiser=NONE} :: rest, state=NormalState state, code, context} =
            let
                (* Allocate memory without initialisation.  Used for byte segments that don't need to be initialised. *)
                val fReg = argAsPReg flags and destReg = pRegNum dest
                val startState = checkTopInRax(state, fReg) (* Should be in eax. *)
                val loadSize = loadSource(size, edx, startState)
                (* We put the size in rdx, the flags in rax and the result in rdi. *)
                val allocMem = [AllocateMemoryVariable{size=RegisterArgument rdxAsArg, flags=RegisterArgument raxAsArg, dest=rdiAsArg, initialiser=NONE}]
                val moveToRAX = [MoveArgument{source=RegisterArgument rdiAsArg, dest=RegisterArgument raxAsArg, kind=MoveWord}]
                val resultState = clearCC(addRaxToState(startState, destReg))
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code= moveToRAX @ allocMem @ loadSize @ code, context=context}
            end

        |   codeICode{icode=AllocateMemoryVariable{ size, flags, dest, initialiser=SOME init} :: rest,
                      state=NormalState state, code, context} =
            let
                (* Allocate memory and initialise.  This is needed for word segments.  We want to use
                   rep stosl/q to initialise the store so that requires the size to be in rcx, the
                   initialiser to be in rax and rdi to be available as a work register. *)
                val destReg = pRegNum dest and initReg = pRegNum init
                val startState = checkTopInRax(state, initReg) (* Should be in eax. *)
                val loadSize = loadSource(size, ecx, startState)
                val loadFlags = loadSource(flags, ebx, startState)
                val allocMem =
                    [AllocateMemoryVariable{size=RegisterArgument rcxAsArg, flags=RegisterArgument rbxAsArg, dest=rdxAsArg, initialiser=SOME raxAsArg}]
                val moveToRAX = [MoveArgument{source=RegisterArgument rdxAsArg, dest=RegisterArgument raxAsArg, kind=MoveWord}]
                val resultState = clearCC(addRaxToState(startState, destReg))
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code= moveToRAX @ allocMem @ loadFlags @ loadSize @ code, context=context}
            end

            (* Marks the end of setting the fields of a tuple.  This is used in the next
               level to avoid reordering a new allocation until a previously allocated
               section of memory has been fully initialised. *)
        |   codeICode{icode=(InitialisationComplete _) :: rest, state, code, context} =
                codeICode{icode=rest, state=state, code=InitialisationComplete{dest=raxAsArg (* unused *)} :: code, context=context}

            (* Function call. *)
        |   codeICode{icode=FunctionCall{ regArgs, stackArgs, dest, isTail=false, callKind} :: rest,
                      state=NormalState state, code, context} =
            let
                val destReg = pRegNum dest
                val (pushedState, pushCode) = pushRax state
                (* In an earlier version we assumed that the values were actually pushed to
                   the stack in sequence.  That's no longer true. Explicitly push the
                   stack arguments. *)
                local
                    fun pushAllArgs ([], state, code) = (state, code)
                    |   pushAllArgs (arg::args, state as {valueStack, ccState, untaggedRegs}, code) =
                        let
                            (* TODO: We can push constants and memory addresses directly without loading
                               them into a register. *)
                            val load = loadSource(arg, eax, state)
                            val sp = stackPtr state
                            val newState =
                                {valueStack={preg=0, loc=OnStack sp} :: valueStack, ccState=ccState, untaggedRegs=untaggedRegs}
                        in
                            pushAllArgs(args, newState, PushValue{arg=RegisterArgument raxAsArg} :: load @ code)
                        end
                in
                    val (afterArgs, argCode) = pushAllArgs (stackArgs, pushedState, [])
                end

                (* Move the register arguments into the correct real registers. *)
                val doLoad = List.foldl (fn ((pr, gr), l) => l @ loadSource(pr, gr, afterArgs)) [] regArgs
                (* After the call the stack arguments will have been removed by the caller. *)
                val returnState = addRaxToState({valueStack= #valueStack pushedState, ccState= CCIndeterminate, untaggedRegs=[]}, destReg)
                val functionCode =
                    FunctionCall{isTail=false, regArgs=[], stackArgs=[], dest=raxAsArg, callKind=callKind} ::
                        doLoad @ argCode @ pushCode @ code
            in
                codeICode{icode=rest, state=NormalState returnState, code=functionCode, context=context}
            end

            (* Jump to a function.  The arguments have to be moved into the space currently used
               by the arguments to this function.  That could result in values being overwritten
               so we have to save them, especially the return address. *)
        |   codeICode{icode=FunctionCall{ regArgs, stackArgs, dest= _, isTail=true, callKind} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val currentSp = stackPtr pushedState

                (* Load the earlier arguments into registers.  Do that first because
                   they're lower down the stack and we could overwrite them if we wait. *)
                (* Load until we've done all the arguments or all the registers whichever is earlier.
                   This includes the closure register. *)
                val doLoad = List.foldl (fn ((pr, gr), l) => l @ loadSource(pr, gr, pushedState)) [] regArgs

                (* Load the return address.  This could be overwritten by an argument. *)
                val loadReturn = loadStack(edi, currentSp * wordSize)

                (* Move the stack arguments into the space occupied by this function's args. *)
                val numArgsToMove = List.length stackArgs
                (* There's a bit of a nasty here.  If we have several constant arguments that we
                   haven't pushed to the stack we could find that argOffset is negative.  To avoid
                   that we push some zeros to line everything up. *)
                val (argOffset, extraPushes, currentState) =
                    if currentSp + currentStackArgs >= numArgsToMove
                    then (currentSp + currentStackArgs - numArgsToMove, [], pushedState)
                    else
                    let
                        val needExtras = numArgsToMove - (currentSp + currentStackArgs)
                        val extras = List.tabulate(needExtras, fn _ => PushValue { arg=IntegerConstant 0 })
                        and afterPush =
                            {valueStack= {loc=OnStack(currentSp+needExtras-1), preg=0} :: # valueStack pushedState, ccState=CCIndeterminate, untaggedRegs=[]}
                    in
                        (0, extras, afterPush)
                    end

                fun copyArgs (_, nil) = []
                |   copyArgs (n, r :: rl) =
                    let
                        (* N.B. This is reversed *)
                        val dest = MemoryLocation{base=(GenReg esp), offset=(argOffset+n)*wordSize, index=NoMemIndex}
                        (* If this is a 32-bit constant we can do a direct memory move. Othewise load
                           it into a register.  It might be easier to do some of this in the codetree
                           translation.  Treating all constants this way saves loading them into
                           "registers" in order to save the values. *)
                        val copy =
                            case r of
                                IntegerConstant value =>
                                    if is32bit value
                                    then [MoveArgument{source=IntegerConstant value, dest=dest, kind=MoveWord}]
                                    else [MoveArgument{source=RegisterArgument rsiAsArg, dest=dest, kind=MoveWord},
                                          MoveArgument{source=IntegerConstant value, dest=RegisterArgument rsiAsArg, kind=MoveWord}]
                            |   AddressConstant a =>
                                    if isX64
                                    then [MoveArgument{source=RegisterArgument rsiAsArg, dest=dest, kind=MoveWord},
                                          MoveArgument{source=AddressConstant a, dest=RegisterArgument rsiAsArg, kind=MoveWord}]
                                    else [MoveArgument{source=AddressConstant a, dest=dest, kind=MoveWord}]
                           |    r => MoveArgument{source=RegisterArgument rsiAsArg, dest=dest, kind=MoveWord} ::
                                        loadSource(r, esi, currentState)
                    in
                        copyArgs(n-1, rl) @ copy
                    end

                val moveArgs = copyArgs(numArgsToMove, stackArgs)
                val storeReturn = storeMemory(edi, esp, argOffset*wordSize)
                val functionCode =
                        FunctionCall{isTail=true, regArgs=[], stackArgs=[], dest=raxAsArg, callKind=callKind} ::
                                      resetStackPtr argOffset :: (storeReturn @ moveArgs @ extraPushes @ loadReturn @ doLoad @ pushCode @ code)
            in
                codeICode{icode=rest, state=Exited, code=functionCode, context=context}
            end

            (* Start of a loop.  The loop body contains one or more tail jumps to the start. *)
        |   codeICode{icode=StartLoop{arguments, loopLabel} :: rest, state=NormalState state, code,
                      context={forwardLabels, handlers, loopArgs}} =
            let
                (* The arguments have been evaluated to the stack.  Replace
                   the preg entries in the state with those of the loop variables. *)
                val (pushedState, pushCode) = pushRax state
                val numArgs = List.length arguments
                val headList = List.take(#valueStack pushedState, numArgs)
                fun repSourceWithDest({source, loopReg}, {preg, loc}) =
                     if argAsPReg source = preg then {preg=pRegNum loopReg, loc=loc} else raise InternalError "codeICode: StartLoop"
                val newHead = ListPair.mapEq repSourceWithDest (List.rev arguments, headList)
                val tailList = List.drop(#valueStack pushedState, numArgs)
                (* The CCstate is indeterminate because we may have looped. *)
                val bodyState = {valueStack=newHead @ tailList, ccState= CCIndeterminate, untaggedRegs= []}
                val newContext = {loopArgs=(bodyState, loopLabel) :: loopArgs, forwardLabels=forwardLabels, handlers=handlers}
            in
                codeICode{icode=rest, state=NormalState bodyState, code=StartLoop{arguments=[], loopLabel=loopLabel} :: pushCode @ code,
                          context=newContext}
            end

            (* A loop tail jump.  The arguments have been evaluated to the stack. *)
        |   codeICode{icode=(JumpLoop{arguments, loopLabel}) :: rest, state=NormalState state, code,
                      context=context as {loopArgs=(startState, lab) :: _, ...}} =
            let
                val _ = loopLabel = lab orelse raise InternalError "codeICode: JumpLoop - mismatched labels"
                val (pushedState, pushCode) = pushRax state
                (* Move each argument from the source to the loop variable.  This isn't the same as
                   MoveArgument because the destination already exists. *)
                fun moveArg{source, loopReg} =
                let
                    val load = loadSource(source, eax, pushedState)
                    val store =
                        case findEntry (pRegNum loopReg, pushedState) of
                            OnStack s => storeMemory(eax, esp, (stackPtr pushedState - s - 1) * wordSize)
                        |   InRax => raise InternalError "codeICode: JumpLoop"
                in
                    store @ load
                end
                val storeAll = List.foldl(fn (arg, l) => l @ moveArg arg) [] arguments
                val instrs =
                    JumpLoop{arguments=[], loopLabel=loopLabel} :: resetStackPtr(stackPtr pushedState - stackPtr startState) :: storeAll
            in
                codeICode{icode=rest, state=Exited, code=instrs @ pushCode @ code, context=context}
            end

        |   codeICode{icode=JumpLoop _ :: _, ...} =
                raise InternalError "codeICode: JumpLoop out of loop"

            (* Raise an exception using the value in the "packet" register as the exception packet.
               Since this exits we can ignore any further code and return the code and state. *)
        |   codeICode{icode=RaiseExceptionPacket{packet} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val load = loadSource(packet, eax, pushedState)
            in
                codeICode{icode=rest, state=Exited, code=RaiseExceptionPacket {packet=RegisterArgument raxAsArg } :: load @ pushCode @ code, context=context}
            end

            (* Reserve a contiguous area of memory on the stack.  This is used to receive the
               results of a tuple.  It may be set within the same function or the address
               may be passed into another function to allow it to return multiple results. *)
        |   codeICode{icode=ReserveContainer{size, address} :: rest, state=NormalState state, code, context} =
            let
                val addrReg = pRegNum address
                val (pushedState, pushCode) = pushRax state
                (* The memory must be cleared in case we have a GC. *)
                val pushAll = List.tabulate(size, fn _ => PushValue{arg=IntegerConstant(tag 0)})
                val sp = stackPtr pushedState
            in
                codeICode{icode=rest,
                    state=NormalState(
                            {valueStack={loc=InRax, preg=addrReg} :: {loc=OnStack(sp+size-1), preg= ~1} :: #valueStack pushedState,
                             ccState=CCIndeterminate, untaggedRegs= []}),
                    code=MoveArgument{source=RegisterArgument(GenReg esp), dest=RegisterArgument raxAsArg, kind=MoveWord} :: (pushAll @ pushCode) @ code, context=context}
            end

        |   codeICode{icode=IndexedCaseOperation{ testReg, workReg=_, cases, startValue} :: rest,
                      state=NormalState state, code, context={loopArgs, handlers, forwardLabels}} =
            let
                val tReg = pRegNum testReg
                (* We should have the value to test at the top of the stack. *)
                val startState = checkTopInRax(state, tReg)

                (* This behaves rather like an unconditional branch except that it can go to several places.
                   The case labels and the default label have to be added to the label list. *)
                val labelsFromCase =
                    map(fn ilab => (ilab, startState)) cases @ forwardLabels
            in
                codeICode{icode=rest, state=Exited,
                          code=IndexedCaseOperation{ testReg=raxAsArg, workReg=(GenReg ebx), cases=cases, startValue=startValue} :: code,
                          context={loopArgs=loopArgs, handlers=handlers, forwardLabels=labelsFromCase}}
            end

            (* Lock a mutable segment. *)
        |   codeICode{icode=LockMutable{ addr } :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val load = loadSource(addr, eax, pushedState)
                val lockCode = LockMutable {addr=RegisterArgument raxAsArg} :: (load @ pushCode)
                val resultState = clearCC pushedState (* May affect the CC *)
            in
                codeICode{icode=rest, state=NormalState resultState, code=lockCode @ code, context=context}
            end

       |    codeICode{icode=ReturnResultFromFunction{ resultReg, numStackArgs } :: rest, state=NormalState state, code, context} =
            let
                val resReg = pRegNum resultReg
                val (_, resetCode) = resetStack(state, initialState, resReg)
                val returnCode = ReturnResultFromFunction{resultReg=raxAsArg, numStackArgs=numStackArgs } :: resetCode
            in
                (* We may have more than one return point so we can't just stop here. *)
                codeICode{icode=rest, state=Exited, code=returnCode @ code, context=context}
            end

        |   codeICode{icode=ArithmeticFunction{ oper, resultReg=PReg(resReg, PRegGeneral), operand1 as RegisterArgument(PReg(_, PRegGeneral)), operand2=IntegerConstant i, ccRef } :: rest,
                      state=NormalState state, code, context} =
            let
                (* The result is a tagged register.  This is used when the constant operand can be untagged. *)
                val (pushedState, pushCode) = pushRax state
                val loadCode = loadSource(operand1, eax, pushedState)
                val resultState = addRaxToState(pushedState, resReg)
                val checkState =
                    {valueStack= #valueStack resultState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= #untaggedRegs resultState}
            in
                codeICode{icode=rest, state=NormalState checkState,
                    code=ArithmeticFunction { oper=oper, resultReg=raxAsArg, operand1=RegisterArgument raxAsArg, operand2=IntegerConstant i, ccRef=ccRef } :: loadCode @ pushCode @ code,
                    context=context}
            end

        |   codeICode{icode=ArithmeticFunction{ oper, resultReg=PReg(dReg, PRegUntagged), operand1 as RegisterArgument(PReg(_, PRegGeneral)), operand2=IntegerConstant i, ccRef } :: rest,
                      state=NormalState state, code, context} =
            let
                (* The result is an untagged register.  Typically used to untag a value before an operation when we
                   don't want to shift the argument. *)
                val (pushedState, pushCode) = pushRax state (* Simplifies things *)
                (* Find a spare untagged register. *)
                val {valueStack, untaggedRegs, ...} = pushedState
                val spareReg =
                    case untaggedRegs of
                        [] => edx
                    |   [_] => ecx
                    |   _ => raise InternalError "codeICode: ArithmeticFunction"
                val resReg = (GenReg spareReg)
                val loadCode = loadSource(operand1, spareReg, pushedState)
                val resultState =
                    {valueStack= valueStack, ccState= CCState(ccRef, fn i=>i), untaggedRegs ={ureg=dReg, reg=GenReg spareReg} :: untaggedRegs}
             in
                codeICode{icode=rest, state=NormalState resultState,
                    code=ArithmeticFunction { oper=oper, resultReg=resReg, operand1=RegisterArgument resReg, operand2=IntegerConstant i, ccRef=ccRef } :: loadCode @ pushCode @ code,
                    context=context}
            end

        |   codeICode{icode=ArithmeticFunction{ oper, resultReg=PReg(resReg, PRegGeneral), operand1=RegisterArgument(PReg(oper1Reg, PRegUntagged)), operand2=IntegerConstant i, ccRef } :: rest,
                      state=NormalState (state as {untaggedRegs, ...}), code, context} =
            let
                (* The result is a tagged register but the argument is untagged.  This is typically when we want to set the tag on a  value. *)
                val realSReg =
                    case List.find (fn {ureg, ...} => ureg = oper1Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: ArithmeticFunction"
                val (pushedState, pushCode) = pushRax state
                val addState = addRaxToState(pushedState, resReg)
                val resultState = removeAfterTag(addState, oper1Reg)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code= (* TODO: We could use LEA since this is normally (always?) ADD *)
                        MoveArgument{source=RegisterArgument realSReg, dest=RegisterArgument raxAsArg, kind=MoveWord} ::
                        ArithmeticFunction { oper=oper, resultReg=realSReg, operand1=RegisterArgument realSReg, operand2=IntegerConstant i, ccRef=ccRef } :: pushCode @ code,
                    context=context}
            end

        |   codeICode{icode=ArithmeticFunction{ oper, resultReg=PReg(resReg, PRegUntagged), operand1=RegisterArgument(PReg(oper1Reg, PRegUntagged)), operand2=IntegerConstant i, ccRef } :: rest,
                      state=NormalState ({valueStack, untaggedRegs, ...}), code, context} =
            let
                (* The result and argument are both untagged.  This is used in computing floating point comparisons. *)
                val realSReg =
                    case List.find (fn {ureg, ...} => ureg = oper1Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: ArithmeticFunction"
                (* Leave the untagged register where it was but add the condition code. *)
                val resultState = {valueStack=valueStack, ccState=CCState(ccRef, fn i => i), untaggedRegs=[{ureg=resReg, reg=realSReg}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        ArithmeticFunction { oper=oper, resultReg=realSReg, operand1=RegisterArgument realSReg, operand2=IntegerConstant i, ccRef=ccRef } :: code,
                    context=context}
            end

        |   codeICode{icode=ArithmeticFunction{ oper, resultReg, operand1=RegisterArgument(PReg(oper1Reg, PRegGeneral)), operand2 as RegisterArgument(PReg(_, PRegGeneral)), ccRef } :: rest,
                      state=NormalState state, code, context} =
            let
                (* The top item should be in RAX but that is the second argument.  That's all right for
                   addition but wrong for subtraction.  Move the first argument into RBX, load the second into
                   RAX and then do the operation. *)
                val resReg = pRegNum resultReg
                val (topState, loadArg2Base) = loadToRax(state, operand2)
                val secondArg =
                    case findEntry (oper1Reg, topState) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: ArithmeticFunction"
                val resultState = addRaxToState(topState, resReg)
                val checkState =
                    {valueStack= #valueStack resultState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= #untaggedRegs resultState}
            in
                codeICode{icode=rest, state=NormalState checkState,
                    code= (* N.B. In reverse order *)
                        ArithmeticFunction {
                            oper=oper, resultReg=raxAsArg, operand1=RegisterArgument raxAsArg, operand2=RegisterArgument(GenReg ebx), ccRef=ccRef } ::
                        MoveArgument
                            { source=MemoryLocation{offset=(stackPtr topState - secondArg - 1) * wordSize, base=(GenReg esp), index=NoMemIndex},
                              dest=RegisterArgument raxAsArg, kind=MoveWord } ::
                        MoveArgument { source=RegisterArgument raxAsArg, dest=RegisterArgument(GenReg ebx), kind=MoveWord} ::
                        loadArg2Base @ code,
                    context=context}
            end

        |   codeICode{
                icode=ArithmeticFunction{ oper, resultReg,
                    operand1 as MemoryLocation{index=NoMemIndex, ...},
                    operand2=MemoryLocation{offset=offset2, base=bReg2, index=NoMemIndex}, ccRef } :: rest,
                state=NormalState state, code, context} =
                (* This is used for LargeWord operations. *)
            let
                val startState = checkTopInRax(state, pRegNum bReg2)
                (* Load the first argument into an untagged register and use it as the result. *)
                val (resultState, resultReg) = newUreg(uRegNum resultReg, startState)
                val resReg = case resultReg of (GenReg r) => r | _ => raise Match
                val loadArg1 = loadSource(operand1, resReg, startState)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        ArithmeticFunction {
                            oper=oper, resultReg=resultReg, operand1=RegisterArgument resultReg,
                            operand2=MemoryLocation{offset=offset2, base=raxAsArg, index=NoMemIndex}, ccRef=ccRef } ::
                        loadArg1 @ code,
                    context=context}
            end

        |   codeICode{icode=ArithmeticFunction _ :: _, ...} =
                raise InternalError "TODO: CodeTransform ArithmeticFunction"

        |   codeICode{icode=TagValue { source, dest } :: rest,
                      state=NormalState (state as {untaggedRegs, ...}), code, context} =
            let
                val sReg = uRegNum source and destReg = pRegNum dest
                val realSReg =
                    case List.find (fn {ureg, ...} => ureg = sReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: TagValue"
                val (pushedState, pushCode) = pushRax state
                val addState = addRaxToState(pushedState, destReg)
                val resultState = removeAfterTag(addState, sReg)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=LoadEffectiveAddress { base=NONE, offset=1, index=MemIndex2 realSReg, dest=raxAsArg } :: pushCode @ code,
                    context=context}
            end

        |   codeICode{icode=UntagValue { source, dest, isSigned } :: rest, state=NormalState state, code, context} =
            let
                val dReg = uRegNum dest
                val (pushedState, pushCode) = pushRax state (* Simplifies things *)
                val (resultState, regAsArg) = newUreg(dReg, pushedState)
                val resReg = case regAsArg of (GenReg r) => r | _ => raise Match
                val loadCode = loadSource(source, resReg, pushedState)
                val resultState = clearCC resultState
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        ShiftOperation{shift=if isSigned then SAR else SHR, resultReg=regAsArg, operand=RegisterArgument regAsArg,
                            shiftAmount=IntegerConstant 1, ccRef=CcRef 0 } ::
                        loadCode @ pushCode @ code,
                    context=context}
            end

        |   codeICode{
                icode=LoadEffectiveAddress {
                    base=SOME baseSource, offset, index=MemIndex1(PReg(iReg, PRegUntagged)), dest=PReg(dReg, PRegUntagged)
                    } :: rest,
                state=NormalState state, code, context} =
            let
                (* This is used to compute the address for a CompareByteVectors instruction.
                   We have to reuse the untagged register for the result.  Use eax for the base. *)
                val (pushedState as { untaggedRegs, valueStack, ccState}, pushCode) = pushRax state (* Simplifies things *)
                val realIReg =
                    case List.find (fn {ureg, ...} => ureg = iReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: LoadEffectiveAddress"
                val loadCode = loadRegister(baseSource, eax, pushedState)
                val filteredUntagged =
                    List.filter (fn {ureg, ...} => ureg <> iReg) untaggedRegs
                val resultState =
                    {valueStack=valueStack, ccState=ccState, untaggedRegs={ureg=dReg, reg=realIReg}::filteredUntagged}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        LoadEffectiveAddress {base=SOME raxAsArg, offset=offset, index=MemIndex1(realIReg), dest=realIReg} ::
                        loadCode @ pushCode@code,
                    context=context}
            end

        |   codeICode{
                icode=LoadEffectiveAddress { base=SOME baseSource, offset, index=NoMemIndex, dest=PReg(dReg, PRegUntagged)} :: rest,
                state=NormalState state, code, context} =
            let
                (* Used to compute the base address for block operations.  The result is always an untagged register
                   because it points into a block of ML memory. *)
                val (pushedState, pushCode) = pushRax state (* Simplifies things *)
                val loadCode = loadRegister(baseSource, eax, pushedState)
                val (resultState, iReg) = newUreg(dReg, pushedState)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        LoadEffectiveAddress {base=SOME raxAsArg, offset=offset, index=NoMemIndex, dest=iReg} ::
                        loadCode @ pushCode@code,
                    context=context}
            end

        |   codeICode{
                icode=LoadEffectiveAddress { base=SOME baseSource, offset, index, dest=PReg(dReg, PRegUntagged)} :: rest,
                state=NormalState state, code, context} =
            let
                (* Used to compute the base address for block word moves.  Unlike the MemIndex1 case the index
                   register is not an untagged register. *)
                val (pushedState, pushCode) = pushRax state (* Simplifies things *)
                val (iReg, indexed) =
                    case index of
                        MemIndex2 src => (src, MemIndex2 raxAsArg)
                    |   MemIndex4 src => (src, MemIndex4 raxAsArg)
                    |   MemIndex8 src => (src, MemIndex8 raxAsArg)
                    |   _ => raise InternalError "codeICode: MemIndex"
                val loadCode = loadRegister(baseSource, ebx, pushedState) @ loadRegister(iReg, eax, pushedState)
                val (resultState, realDReg) = newUreg(dReg, pushedState)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        LoadEffectiveAddress {base=SOME rbxAsArg, offset=offset, index=indexed, dest=realDReg} ::
                        loadCode @ pushCode @ code,
                    context=context}
            end

(*        |   codeICode{icode=LoadEffectiveAddress { base=NONE, offset, index=MemIndex2(PReg iReg), dest } :: rest,
                      state=NormalState state, code, context} =
            (* Currently just used for tagging. *)
            let
                val resReg = argToPReg dest
                val topState = checkTopInRax(state, iReg)
                val resultState = addRaxToState(topState, resReg)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=LoadEffectiveAddress{ base=NONE, offset=offset, index=MemIndex2 raxAsArg, dest=raxAsArg } :: code,
                    context=context}
            end*)

        |   codeICode{icode=LoadEffectiveAddress _ :: _, ...} =
                raise InternalError "TODO: CodeTransform LoadEffectiveAddress"

        |   codeICode{icode=ShiftOperation {shift, resultReg, operand=RegisterArgument(PReg(operReg, PRegGeneral)), shiftAmount=IntegerConstant i, ccRef } :: rest,
                      state=NormalState state, code, context} =
            let
                val resReg = pRegNum resultReg
                val topState = checkTopInRax(state, operReg)
                val resultState = addRaxToState(topState, resReg)
                val checkState =
                    {valueStack= #valueStack resultState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= #untaggedRegs resultState}
            in
                codeICode{icode=rest, state=NormalState checkState,
                    code=ShiftOperation { shift=shift, resultReg=raxAsArg, operand=RegisterArgument raxAsArg, shiftAmount=IntegerConstant i, ccRef=ccRef } :: code,
                    context=context}
            end

        |   codeICode{icode=ShiftOperation {shift, resultReg, operand=RegisterArgument(PReg(operReg, PRegUntagged)), shiftAmount=RegisterArgument(PReg(shiftReg, PRegUntagged)), ccRef } :: rest,
                      state=NormalState ({untaggedRegs, valueStack, ...}), code, context} =
            let
                (* Shift by a variable.  This should be in rcx which is where we want it. *)
                val resReg = uRegNum resultReg
                val realOperReg =
                    case List.find (fn {ureg, ...} => ureg = operReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: ShiftOperation"
                val realShiftReg =
                    case List.find (fn {ureg, ...} => ureg = shiftReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: ShiftOperation"
                val _ = realShiftReg = GenReg ecx orelse raise InternalError "ShiftOperation: not ecx"
                val resultState =
                    {valueStack=valueStack, ccState=CCState(ccRef, fn i=>i), untaggedRegs =[{ureg=resReg, reg=realOperReg}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=ShiftOperation{ shift=shift, resultReg=realOperReg, operand=RegisterArgument realOperReg,
                                         shiftAmount=RegisterArgument realShiftReg, ccRef=ccRef} :: code, context=context}
            end

        |   codeICode{
                icode=ShiftOperation {shift, resultReg,
                    operand as MemoryLocation{index=NoMemIndex, ...},
                    shiftAmount=RegisterArgument(PReg(shiftReg, PRegUntagged)), ccRef } :: rest,
                state=NormalState (state as {untaggedRegs, valueStack, ...}), code, context} =
            let
                (* Shift by a variable.  This is used for LargeWord shifts.  The argument is boxed rather than untagged
                   so we will have loaded the shift amount, which is tagged, into edx.  *)
                val resReg = uRegNum resultReg
                val realShiftReg =
                    case List.find (fn {ureg, ...} => ureg = shiftReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: ShiftOperation"
                val _ = realShiftReg = GenReg edx orelse raise InternalError "ShiftOperation: it is ecx"
                (* Move the shift amount into ecx then load the value to be shifted into edx. *)
                val loadCode = loadSource(operand, edx, state)
                val resultState =
                    {valueStack=valueStack, ccState=CCState(ccRef, fn i=>i), untaggedRegs =[{ureg=resReg, reg=GenReg edx}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=ShiftOperation{ shift=shift, resultReg=rdxAsArg, operand=RegisterArgument rdxAsArg,
                                         shiftAmount=RegisterArgument rcxAsArg, ccRef=ccRef} ::
                         loadCode @
                         (MoveArgument{source=RegisterArgument rdxAsArg, dest=RegisterArgument rcxAsArg, kind=MoveWord} :: code), context=context}
            end

        |   codeICode{icode=ShiftOperation _ :: _, ...} =
                raise InternalError "TODO: CodeTransform ShiftOperation"

        |   codeICode{
                icode=Multiplication { resultReg,
                    operand1 as MemoryLocation{index=NoMemIndex, ...},
                    operand2=MemoryLocation{offset=offset2, base=bReg2, index=NoMemIndex}, ccRef } :: rest,
                state=NormalState state, code, context} =
                (* This is used for LargeWord multiplication.  We can do it in the same way as
                   arithmetic and logical operations. *)
            let
                val startState = checkTopInRax(state, pRegNum bReg2)
                (* Load the first argument into an untagged register and use it as the result. *)
                val (resultState, resultReg) = newUreg(uRegNum resultReg, startState)
                val resReg = case resultReg of (GenReg r) => r | _ => raise Match
                val loadArg1 = loadSource(operand1, resReg, startState)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        Multiplication { resultReg=resultReg, operand1=RegisterArgument resultReg,
                            operand2=MemoryLocation{offset=offset2, base=raxAsArg, index=NoMemIndex}, ccRef=ccRef } ::
                        loadArg1 @ code,
                    context=context}
            end

        |   codeICode{icode=Multiplication { resultReg, operand1, operand2, ccRef } :: rest,
                      state=NormalState ({untaggedRegs, valueStack, ...}), code, context} =
            let
                (* For the moment we only implement the case of multiplying two values in registers.
                   The X86 supports various other multiplication forms if one of the arguments is
                   a constant including one which takes three arguments. *)
                val resReg = uRegNum resultReg and op1Reg = argAsUReg operand1 and op2Reg = argAsUReg operand2
                val realOp1Reg =
                    case List.find (fn {ureg, ...} => ureg = op1Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: Multiplication"
                val realOp2Reg =
                    case List.find (fn {ureg, ...} => ureg = op2Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: Multiplication"
                (* We can replace both the untagged registers. *)
                val resultState =
                    {valueStack=valueStack, ccState=CCState(ccRef, fn i=>i), untaggedRegs =[{ureg=resReg, reg=realOp1Reg}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=Multiplication{ resultReg=realOp1Reg, operand1=RegisterArgument realOp1Reg,
                                         operand2=RegisterArgument realOp2Reg, ccRef=ccRef} :: code, context=context}
            end

        |   codeICode{
                icode=Division { isSigned,
                        dividend as MemoryLocation{index=NoMemIndex, ...},
                        divisor as MemoryLocation{index=NoMemIndex, ...}, quotient, remainder } :: rest,
                state=NormalState state, code, context} =
            let
                (* We need the dividend in Rax so we can't use it as the base reg for the divisor. *)
                val (pushState, pushCode) = pushRax state (* We need RAX. *)
                val loadDivisor = loadSource(divisor, ecx, pushState)
                val loadDividend = loadSource(dividend, eax, pushState)
                val resultState =
                    {valueStack= #valueStack pushState, ccState=CCIndeterminate,
                        untaggedRegs =[{ureg=uRegNum quotient, reg=GenReg eax}, {ureg=uRegNum remainder, reg=GenReg edx}]}
             in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        Division{ isSigned=isSigned, dividend=RegisterArgument raxAsArg, divisor=RegisterArgument rcxAsArg, quotient=raxAsArg, remainder=rdxAsArg} ::
                        loadDividend @ loadDivisor @ pushCode @ code, context=context}
            end

        |   codeICode{icode=Division { isSigned, dividend, divisor, quotient, remainder } :: rest,
                      state=NormalState state, code, context} =
            let
                val dividendReg = argAsUReg dividend and divisorReg = argAsUReg divisor
                val ({ untaggedRegs, valueStack, ...}, pushCode) = pushRax state (* We need RAX. *)
                val realDividend =
                    case List.find (fn {ureg, ...} => ureg = dividendReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: Division"
                val realDivisor =
                    case List.find (fn {ureg, ...} => ureg = divisorReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: Division"
                val resultState =
                    {valueStack=valueStack, ccState=CCIndeterminate,
                        untaggedRegs =[{ureg=uRegNum quotient, reg=GenReg eax}, {ureg=uRegNum remainder, reg=GenReg edx}]}
                (* The untagging code puts the first value into edx and the second into ecx.  We
                   have to move the dividend into eax. *)
                val _ = realDividend = GenReg edx orelse raise InternalError "codeICode: dividend"
                val _ = realDivisor = GenReg ecx orelse raise InternalError "codeICode: divisor"
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        Division{ isSigned=isSigned, dividend=RegisterArgument raxAsArg, divisor=RegisterArgument rcxAsArg, quotient=raxAsArg, remainder=rdxAsArg} ::
                        MoveArgument { source=RegisterArgument realDividend, dest=RegisterArgument raxAsArg, kind=MoveWord } :: pushCode @ code, context=context}
            end

        |   codeICode{icode=AtomicExchangeAndAdd{ destAddr, source } :: rest,
                      state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                (* Load the "destination" address into rax. *)
                val load = loadSource(destAddr, eax, pushedState)
                (* The "source" is an untagged register. *)
                val srcReg = getUreg(uRegNum source, pushedState)
            in
                codeICode{icode=rest, state=NormalState pushedState,
                    code=AtomicExchangeAndAdd{destAddr=RegisterArgument raxAsArg, source=srcReg} :: load @ pushCode @ code, context=context}
            end

        |   codeICode{icode=BoxValue{ boxKind=BoxLargeWord, dest, source } :: rest, state=NormalState state, code, context} =
            let
                (* Allocate a one word cell and store the untagged value into it. *)
                val destReg = pRegNum dest
                val srcReg = getUreg(uRegNum source, state)
                val realSource =
                    case srcReg of (GenReg r) => r | _ => raise InternalError "BoxLargeWord"
                val (newSrcReg, pushedState, pushCode) =
                    if realSource = eax
                    then
                    let
                        (* If the value we want to store is in eax i.e. the quotient, we
                           must move it to another register before the allocation. *)
                        val pushCode =
                            [MoveArgument{ source=RegisterArgument srcReg, dest=RegisterArgument rcxAsArg, kind=MoveWord}]
                    in
                        (rcxAsArg, state, pushCode)
                    end
                    else
                    let
                        val (pushedState, pushCode) = pushRax state
                    in
                        (srcReg, pushedState, pushCode)
                    end
                val resultState =
                    removeAfterTag(clearCC(addRaxToState(pushedState, destReg)) (* Affects CC *), srcReg)
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code=
                        InitialisationComplete{dest=raxAsArg} ::
                        MoveArgument{ source=RegisterArgument newSrcReg, dest=wordOffsetAddress(0, eax), kind=MoveWord} ::
                        AllocateMemoryOperation{ size=1, flags=0wx1, dest=raxAsArg} :: pushCode @ code, context=context}
            end

        |   codeICode{icode=BoxValue{ boxKind=BoxFloat, dest, source } :: rest, state=NormalState state, code, context} =
            let
                (* Allocate a one or two word cell and store the untagged value into it. *)
                val destReg = pRegNum dest
                val srcReg = getFPreg(uRegNum source, state)
                val (pushedState, pushCode) = pushRax state
                val resultState = removeAfterTag(clearCC(addRaxToState(pushedState, destReg)) (* Affects CC *), srcReg)
                val boxSize = Int.quot(8, wordSize)
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code=
                        InitialisationComplete{dest=raxAsArg} ::
                        MoveArgument{ source=RegisterArgument srcReg, dest=wordOffsetAddress(0, eax), kind=MoveDouble} ::
                        AllocateMemoryOperation{ size=boxSize, flags=0wx1, dest=raxAsArg} :: pushCode @ code, context=context}
            end

        |   codeICode{icode=CompareByteVectors { vec1Addr, vec2Addr, length, ccRef } :: rest, state=NormalState state, code, context} =
            let
                val {untaggedRegs, ...} = state
                val vec1Reg = uRegNum vec1Addr and vec2Reg = uRegNum vec2Addr
                val realVec1Reg =
                    case List.find (fn {ureg, ...} => ureg = vec1Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: CompareByteVectors"
                val realVec2Reg =
                    case List.find (fn {ureg, ...} => ureg = vec2Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: CompareByteVectors"
                (* Put the length into RCX. *)
                (* There's a complication here.  CompareByteVectors generates REPE CMPSB to compare
                   the vectors but the condition code is only set if CMPSB is executed at least
                   once.  If the value in RCX/ECX is zero it will never be executed and the
                   condition code will be unchanged.  We want the result to be "equal" in that
                   case so we need to ensure that is the case.  It's quite possible that the
                   condition code has just been set by shifting RCX/ECX to remove the tag in which
                   case it will have set "equal" if the value was zero.  We use CMP R/ECX,R/ECX which
                   is two bytes in 32-bit but three in 64-bit. *)
                val (lengthCode, setZeroFlag) =
                    case length of
                        IntegerConstant l =>
                            (MoveArgument{ source=IntegerConstant l, dest=RegisterArgument rcxAsArg, kind=MoveWord},
                             (* We don't need this except if the length is zero.  Really if we know
                                the length is zero we don't need this at all. *)
                             if l = 0 then [WordComparison {arg1=RegisterArgument rcxAsArg, arg2=RegisterArgument rcxAsArg, ccRef=ccRef}] else [])
                    |   length =>
                        let
                            val lenReg = argAsUReg length
                            val realLenReg =
                                case List.find (fn {ureg, ...} => ureg = lenReg) untaggedRegs of
                                    SOME {reg as GenReg _, ...} => reg
                                |   _ => raise InternalError "codeICode: CompareByteVectors"
                            val _ = realLenReg = GenReg esi orelse raise InternalError "codeICode: lenReg"
                        in
                            (MoveArgument{source=RegisterArgument realLenReg, dest=RegisterArgument rcxAsArg, kind=MoveWord},
                             [WordComparison {arg1=RegisterArgument rcxAsArg, arg2=RegisterArgument rcxAsArg, ccRef=ccRef}]) (* Set CC - any reg will do. *)
                        end
                (* These aren't in the registers we want so we have to shuffle them. *)
                val _ = realVec1Reg = GenReg edx orelse raise InternalError "codeICode: vec1Reg"
                val _ = realVec2Reg = GenReg ecx orelse raise InternalError "codeICode: vec2Reg"
                
                val resultState =
                    {valueStack= #valueStack state, ccState=CCState(ccRef, fn i=>i), untaggedRegs= []}
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code =
                        CompareByteVectors{vec1Addr=rsiAsArg, vec2Addr=rdiAsArg, length=RegisterArgument rcxAsArg, ccRef=ccRef} ::
                        setZeroFlag @ (
                        MoveArgument{source=RegisterArgument realVec1Reg, dest=RegisterArgument rsiAsArg, kind=MoveWord} ::
                        lengthCode ::
                        MoveArgument{source=RegisterArgument realVec2Reg, dest=RegisterArgument rdiAsArg, kind=MoveWord} :: code), context=context}
            end

        |   codeICode{icode=BlockMove { srcAddr, destAddr, length, isByteMove } :: rest, state=NormalState state, code, context} =
            let
                val {untaggedRegs, ...} = state
                val srcReg = uRegNum srcAddr and destReg = uRegNum destAddr and lenReg = argAsUReg length
                val realSrcReg =
                    case List.find (fn {ureg, ...} => ureg = srcReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: BlockMove"
                val realDestReg =
                    case List.find (fn {ureg, ...} => ureg = destReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: BlockMove"
                val realLenReg =
                    case List.find (fn {ureg, ...} => ureg = lenReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: BlockMove"
                (* These aren't in the registers we want so we have to shuffle them. *)
                val _ = realSrcReg = GenReg edx orelse raise InternalError "codeICode: vec1Reg"
                val _ = realDestReg = GenReg ecx orelse raise InternalError "codeICode: vec2Reg"
                val _ = realLenReg = GenReg esi orelse raise InternalError "codeICode: lenReg"
                val resultState =
                    {valueStack= #valueStack state, ccState=CCIndeterminate, untaggedRegs= []}
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code =
                        BlockMove{srcAddr=rsiAsArg, destAddr=rdiAsArg, length=RegisterArgument rcxAsArg, isByteMove=isByteMove} ::
                        MoveArgument{source=RegisterArgument realSrcReg, dest=RegisterArgument rsiAsArg, kind=MoveWord} ::
                        MoveArgument{source=RegisterArgument realLenReg, dest=RegisterArgument rcxAsArg, kind=MoveWord} ::
                        MoveArgument{source=RegisterArgument realDestReg, dest=RegisterArgument rdiAsArg, kind=MoveWord} :: code, context=context}
            end

        |   codeICode{
                icode=CompareFloatingPt{
                        arg1=MemoryLocation{offset=offset1, base=bReg1, index=NoMemIndex},
                        arg2=MemoryLocation{offset=offset2, base=bReg2, index=NoMemIndex}, ccRef} :: rest,
                      state=NormalState state, code, context=context} =
            let
                (* Load the first argument into an SSE2 register or the X87 stack.
                   Compare with the second. *)
                val (startState as { valueStack, ...}, arg2Code) = loadToRax(state, RegisterArgument bReg2)
                val arg1Code = loadRegister(bReg1, ebx, startState)
                val floatingPointReg =
                    case fpMode of
                        FPModeX87 => FPReg fp0
                    |   FPModeSSE2 => XMMReg xmm0
                val checkState = {valueStack= valueStack, ccState=CCState(ccRef, fn c => c), untaggedRegs= []}
            in
                codeICode{icode=rest, state=NormalState checkState,
                          code=
                            CompareFloatingPt{arg1=RegisterArgument floatingPointReg, arg2=MemoryLocation{offset=offset2, base=raxAsArg, index=NoMemIndex}, ccRef=ccRef} ::
                            MoveArgument{source=MemoryLocation{offset=offset1, base=rbxAsArg, index=NoMemIndex},
                                dest=RegisterArgument floatingPointReg, kind=MoveDouble} ::
                            arg1Code @ arg2Code @ code,
                          context=context}
            end

        |   codeICode{icode=CompareFloatingPt _ :: _, ...} =
                raise InternalError "codeICode: CompareFloatingPt"

        |   codeICode{
                icode=X87FPGetCondition{ ccRef, dest } :: rest, state=NormalState state, code, context=context} =
            let
                val destReg = uRegNum dest
                (* Check we have the condition. *)
                (* We can only use RAX here. *)
                val ({valueStack, ccState, ...}, pushCode) = pushRax state
                val () =
                    case ccState of
                        CCState(currentRef, _) =>
                            if currentRef = ccRef then ()
                            else raise InternalError "codeICode: condition ref incorrect"
                    |   _ => raise InternalError "codeICode: condition ref unset"
                val resultState =
                    {valueStack=valueStack, ccState=ccState, untaggedRegs=[{ureg=destReg, reg=GenReg eax}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code= X87FPGetCondition{ ccRef=ccRef, dest=raxAsArg } :: pushCode @ code,
                    context=context}
            end

        |   codeICode{
                icode=X87FPArith{
                        opc, resultReg,
                        arg1=MemoryLocation{offset=offset1, base=bReg1, index=NoMemIndex},
                        arg2=MemoryLocation{offset=offset2, base=bReg2, index=NoMemIndex}} :: rest,
                      state=NormalState state, code, context=context} =
            let
                (* The address of the first argument will be on the stack.  The second
                   will be in Rax. *)
                val startState as {valueStack, ...} = checkTopInRax(state, pRegNum bReg2)
                val loadArgAddr = loadRegister(bReg1, ebx, startState)

                val floatingPointReg = FPReg fp0
                val destReg = uRegNum resultReg
                val resultState =
                    {valueStack=valueStack, ccState=CCIndeterminate, untaggedRegs=[{ureg=destReg, reg=floatingPointReg}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                          code=
                            X87FPArith{
                                opc=opc, resultReg=floatingPointReg, arg1=RegisterArgument floatingPointReg,
                                arg2=MemoryLocation{offset=offset2, base=raxAsArg, index=NoMemIndex}} ::
                            MoveArgument{source=MemoryLocation{offset=offset1, base=rbxAsArg, index=NoMemIndex},
                                dest=RegisterArgument floatingPointReg, kind=MoveDouble} ::
                            loadArgAddr @ code,
                          context=context}
            end

        |   codeICode{icode=X87FPArith _ :: _, ...} =
                raise InternalError "codeICode: X87FPArith"

        |   codeICode{
                icode=SSE2FPArith{
                        opc, resultReg,
                        arg1=MemoryLocation{offset=offset1, base=bReg1, index=NoMemIndex},
                        arg2=MemoryLocation{offset=offset2, base=bReg2, index=NoMemIndex}} :: rest,
                      state=NormalState state, code, context=context} =
            let
                (* The address of the first argument will be on the stack.  The second
                   will be in Rax. *)
                val startState as {valueStack, ...} = checkTopInRax(state, pRegNum bReg2)
                val loadArgAddr = loadRegister(bReg1, ebx, startState)

                val floatingPointReg = XMMReg xmm0
                val destReg = uRegNum resultReg
                val resultState =
                    {valueStack=valueStack, ccState=CCIndeterminate, untaggedRegs=[{ureg=destReg, reg=floatingPointReg}]}
                (* We can't use this for the logical operations since they require 128-bit args. *)
                val () =
                    case opc of
                        SSE2Xor => raise InternalError "codeICode: SSE2FPArith - SSE2Xor"
                    |   SSE2And => raise InternalError "codeICode: SSE2FPArith - SSE2And"
                    |   _ => ()
            in
                codeICode{icode=rest, state=NormalState resultState,
                          code=
                            SSE2FPArith{
                                opc=opc, resultReg=floatingPointReg, arg1=RegisterArgument floatingPointReg,
                                arg2=MemoryLocation{offset=offset2, base=raxAsArg, index=NoMemIndex}} ::
                            MoveArgument{source=MemoryLocation{offset=offset1, base=rbxAsArg, index=NoMemIndex},
                                dest=RegisterArgument floatingPointReg, kind=MoveDouble} ::
                            loadArgAddr @ code,
                          context=context}
            end

        |   codeICode{
                icode=SSE2FPArith{
                        opc, resultReg,
                        arg1=MemoryLocation{offset=offset1, base=bReg1, index=NoMemIndex},
                        arg2=AddressConstant a} :: rest,
                      state=NormalState state, code, context=context} =
            let
                (* This is used for the logical operations to invert or mask the sign bit.
                   Both arguments need to be loaded into registers because any memory
                   argument must be a 128-bit value. *)
                val {valueStack, ...} = checkTopInRax(state, pRegNum bReg1)
                val floatingPointReg0 = XMMReg xmm0
                val floatingPointReg1 = XMMReg xmm1
                val destReg = uRegNum resultReg
                val resultState =
                    {valueStack=valueStack, ccState=CCIndeterminate, untaggedRegs=[{ureg=destReg, reg=floatingPointReg0}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                          code=
                            SSE2FPArith{
                                opc=opc, resultReg=floatingPointReg0, arg1=RegisterArgument floatingPointReg0,
                                arg2=RegisterArgument floatingPointReg1} ::
                            MoveArgument{source=AddressConstant a, dest=RegisterArgument floatingPointReg1, kind=MoveDouble} ::
                            MoveArgument{source=MemoryLocation{offset=offset1, base=raxAsArg, index=NoMemIndex},
                                dest=RegisterArgument floatingPointReg0, kind=MoveDouble} ::
                            code,
                          context=context}
            end

        |   codeICode{icode=SSE2FPArith _ :: _, ...} =
                raise InternalError "codeICode: SSE2FPArith"

        |   codeICode{
                icode=X87FPUnaryOps{ fpOp, dest, source=MemoryLocation{offset=offset, base=bReg, index=NoMemIndex}} :: rest,
                      state=NormalState state, code, context=context} =
            let
                (* The address of the argument will be on the stack. *)
                val {valueStack, ...} = checkTopInRax(state, pRegNum bReg)

                val floatingPointReg = FPReg fp0
                val destReg = uRegNum dest
                val resultState =
                    {valueStack=valueStack, ccState=CCIndeterminate, untaggedRegs=[{ureg=destReg, reg=floatingPointReg}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                          code=
                            X87FPUnaryOps{fpOp=fpOp, dest=floatingPointReg, source=RegisterArgument floatingPointReg} ::
                            MoveArgument{source=MemoryLocation{offset=offset, base=raxAsArg, index=NoMemIndex},
                                dest=RegisterArgument floatingPointReg, kind=MoveDouble} ::
                            code,
                          context=context}
            end

        |   codeICode{icode=X87FPUnaryOps _ :: _, ...} =
                raise InternalError "codeICode: X87FPUnaryOps"

        |   codeICode{icode=FloatFixedInt{ dest, source} :: rest, state=NormalState ({untaggedRegs, valueStack, ...}), code, context=context} =
            let
                (* The fixed point value should be in an untagged register. *)
                val op1Reg = argAsUReg source
                val realOpReg =
                    case List.find (fn {ureg, ...} => ureg = op1Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: Multiplication"
                val (floatingPointReg, floatCode) =
                    case fpMode of
                        FPModeX87 => 
                        let
                            (* This is complicated.  The integer value has to be in memory not in a
                               register so we have to push it to the stack and then make sure it is
                               popped afterwards.  Because it is untagged it is unsafe to leave it. *)
                            val fpReg = FPReg fp0
                            val fpCode =
                                [
                                    ResetStackPtr {numWords=1},
                                    FloatFixedInt{dest=fpReg, source=wordOffsetAddress(0, esp)},
                                    PushValue{arg=RegisterArgument realOpReg}
                                ]
                        in
                            (fpReg, fpCode)
                        end
                    |   FPModeSSE2 =>
                        let
                            val fpReg = XMMReg xmm0
                        in
                            (fpReg, [FloatFixedInt{dest=fpReg, source=RegisterArgument realOpReg}])
                        end
                val destReg = uRegNum dest
                val resultState =
                    {valueStack=valueStack, ccState=CCIndeterminate, untaggedRegs=[{ureg=destReg, reg=floatingPointReg}]}
            in
                codeICode{icode=rest, state=NormalState resultState, code=floatCode @ code,context=context}
            end


            (* These should not occur here.  They are only added during this phase. *)
        |   codeICode{icode=PushValue _ :: _, ...} = raise InternalError "codeICode: PushValue"
        |   codeICode{icode=ResetStackPtr _ :: _, ...} = raise InternalError "codeICode: ResetStackPtr"
        
        exception TooHard

        val (decoratedCode, regStates) = identifyRegisters {icode=icode, maxPRegs=maxPRegs}
        
        fun isActive(reg, set) = List.exists(fn i => i = reg) set
        
        fun findInState(pr, state) = #2(valOf(List.find(fn (r, _) => r = pr) state))
        fun removeFromState(pr, state) = List.filter(fn (r, _) => r <> pr) state

        fun codeExtended([], _) = ([], [])
        
        |   codeExtended((BeginFunction{regArgs=[(dest, srcReg)]}, active) :: rest, context) =
            let
                (* For the moment deal just with a single register. *)
                val pr = pRegNum dest
                val (state, code) = codeExtended(rest, context)
            in
                if isActive(pr, active)
                then
                let
                    (* See if we're using it from the register it's in.  Otherwise we need
                       to move it. *)
                    val regResult = findInState(pr, state)
                    val c =
                        if regResult = srcReg
                        then []
                        else [MoveArgument{source=RegisterArgument srcReg, dest=RegisterArgument regResult, kind=MoveWord}]
                in
                    (removeFromState(pr, state), c @ code)
                end
                else (state, code) (* Not needed. *)
            end

        |   codeExtended((MoveArgument{source=RegisterArgument(PReg(sreg, PRegGeneral)), dest=RegisterArgument(PReg(dreg, PRegGeneral)), kind=MoveWord}, active) :: rest, context) =
            let
                (* If the source register is active we're going to need both, or at least assume that for
                   the moment so we only ever associate a single preg with a real register. *)
                val (state, code) = codeExtended(rest, context)
            in
                if isActive(sreg, active)
                then
                let
                in
                    raise TooHard
                end
                else
                let
                    (* Since we're generating a new register and don't want the old one we can
                       use the same real register. *)
                    val regResult = findInState(dreg, state)
                    val newState = (sreg, regResult) :: removeFromState(dreg, state)
                in
                    (newState, code)
                end
            end

        |   codeExtended((MoveArgument{source=AddressConstant a, dest=RegisterArgument(PReg(pr, PRegGeneral)), kind=MoveWord}, active) :: rest, context) =
            let
                val (state, code) = codeExtended(rest, context)
            in
                if isActive(pr, active)
                then
                let
                    (* Find a register for this.  Because we're working backwards we should have this. *)
                    val regResult = findInState(pr, state)
                    val c = MoveArgument{source=AddressConstant a, dest=RegisterArgument regResult, kind=MoveWord}
                in
                    (removeFromState(pr, state), c :: code)
                end
                else (state, code) (* Not needed. *)
            end

        |   codeExtended((ReturnResultFromFunction { resultReg, numStackArgs }, _) :: rest, context) =
            let
                val (_, code) = codeExtended(rest, context)
                (* Say we want it in rax. *)
                val resultState = [(pRegNum resultReg, GenReg eax)]
                val c = ReturnResultFromFunction{resultReg=raxAsArg, numStackArgs=numStackArgs}
            in
                (resultState, c :: code)
            end

        |   codeExtended((ArithmeticFunction{ oper, resultReg=PReg(resReg, PRegGeneral), operand1=RegisterArgument(PReg(op1Reg, PRegGeneral)),
                                             operand2=IntegerConstant i, ccRef}, active) :: rest, context) =
            let (* The result is a tagged register.  This is used when the constant operand can be untagged. *)
                val (state, code) = codeExtended(rest, context)
            in
                (* If the operand register is not used after this instruction we can reuse the result register. *)
                if isActive(op1Reg, active)
                then raise TooHard
                else
                let
                    val regResult = findInState(resReg, state)
                    val c =
                        ArithmeticFunction{oper=oper, resultReg=regResult, operand1=RegisterArgument regResult,
                                           operand2=IntegerConstant i, ccRef=ccRef}
                    val newState = (op1Reg, regResult) :: removeFromState(resReg, state)
                in
                    (newState, c :: code)
                end
            end
        
        |   codeExtended _ = raise TooHard
        
        val icodeTabs = [8, 20, 60]
        
        val () =
            (* Print the code before the transformation. *)
            if DEBUG.getParameter DEBUG.icodeTag debugSwitches
            then
            let
                val printStream = PRETTY.getSimplePrinter(debugSwitches, icodeTabs)

                fun printRegs([], _) = ()
                |   printRegs(_, 0) = printStream "..."
                |   printRegs([i], _) = printStream(Int.toString i)
                |   printRegs(i::l, n) = (printStream(Int.toString i ^ ","); printRegs(l, n-1))

                fun printCode(c, regs) =
                (
                    printICodeAbstract(c, printStream);
                    printStream "\t[";
                    printRegs(regs, 10);
                    printStream "]\n"
                )
                
                fun printRegData(i, { active, defs, refs, sourceReal, destReal, pushState }) =
                (
                    printStream (Int.toString i ^ "\t");
                    printStream ("Active=" ^ Int.toString active);
                    printStream (" Rfs=" ^ Int.toString refs);
                    printStream (" Defs=" ^ Int.toString defs);
                    case sourceReal of SOME r => printStream(" From=" ^ regRepr r) | NONE => ();
                    case destReal of SOME r => printStream(" To=" ^ regRepr r) | NONE => ();
                    case pushState of
                        MustPush => printStream " Must push" | MustNotPush => printStream " No push" | MayPush => ();
                    printStream "\n"
                )
            in
                printStream(functionName ^ "\n");
                List.app printCode decoratedCode;
                printStream "\n";
                Vector.appi printRegData regStates
            end
            else ()

        val postTransformCode =
            let
                val (_, code) = codeExtended(decoratedCode, [])
                val _ = print("Done " ^ functionName ^ "!!!!\n")
            in
                code
            end
            handle TooHard =>
            let
                val (fnState, fnCode) =
                    codeICode{icode=icode, state=NormalState initialState, code=[], context={loopArgs=[], forwardLabels=[], handlers=[]}}
                val _ = case fnState of Exited => () | _ => raise InternalError "fnstate not exited"
            in
                List.rev fnCode (* Reverse the list *)
            end
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
        codeAsX86Code{icode=postTransformCode, maxLabels = !labelCounter, stackRequired = !maxStack,
                      inputRegisters= argRegsUsed @ (if hasFullClosure then [edx] else []), debugSwitches=debugSwitches,
                      functionName=functionName}
    end

    structure Sharing =
    struct
        type 'reg x86ICode = 'reg x86ICode
        and abstract = abstract
        and genReg   = genReg
    end
end;
