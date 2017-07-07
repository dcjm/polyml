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
) : X86ICODETRANSFORMSIG
=
struct
    open ICODE
    open Address
    
    exception InternalError = Misc.InternalError

    (* tag a short constant *)
    fun tag c = 2 * c + 1

    fun argToPReg(PReg(n, PRegGeneral)) = n | argToPReg _ = raise InternalError "argToPReg"
    and argToUReg(PReg(n, PRegUntagged)) = n | argToUReg _ = raise InternalError "argToUReg"

    fun loadMemory(reg, base, offset) =
        [MoveArgument{
            source=MemoryLocation{base=RealRegister(GenReg base), offset=offset, index=NoMemIndex},
            dest=RealRegister(GenReg reg), kind=WordToWord}]
    and loadStack(reg, offset) = loadMemory(reg, esp, offset)
    and storeMemory(reg, base, offset) =
        [MoveArgument{
            source=RealRegister(GenReg reg),
            dest=MemoryLocation{base=RealRegister(GenReg base), offset=offset, index=NoMemIndex}, kind=WordToWord}]

    val raxAsArg = RealRegister(GenReg eax) and rbxAsArg = RealRegister(GenReg ebx)
    and rcxAsArg = RealRegister(GenReg ecx) and rsiAsArg = RealRegister(GenReg esi)
    and rdxAsArg = RealRegister(GenReg edx) and rdiAsArg = RealRegister(GenReg edi)

    (* Generally we have an offset in words and no index register. *)
    fun wordOffsetAddress(offset, baseReg) =
        MemoryLocation{offset=offset*wordSize, base=RealRegister(GenReg baseReg), index=NoMemIndex}

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

    fun loadSource(PReg(src, PRegGeneral), dest, state) =
    (
        case findEntry (src, state) of
            InRax =>
                if dest = eax
                then []
                else [MoveArgument{ source=RealRegister(GenReg eax), dest=RealRegister(GenReg dest), kind=WordToWord}]
        |   OnStack s => loadStack(dest, (stackPtr state - s - 1) * wordSize)
    )

    |   loadSource(src as AddressConstant _, dest, _) =
            [MoveArgument{ source= src, dest=RealRegister(GenReg dest), kind=WordToWord}]
    |   loadSource(src as IntegerConstant _, dest, _) =
            [MoveArgument{ source= src, dest=RealRegister(GenReg dest), kind=WordToWord}]

    |   loadSource(MemoryLocation{ base, offset, index=NoMemIndex }, dest, state) =
            (* Use the destination register as a base address. *)
            MoveArgument {
                source=MemoryLocation {base=RealRegister(GenReg dest), offset=offset, index=NoMemIndex },
                dest=RealRegister(GenReg dest), kind=WordToWord} ::
                    loadSource(base, dest, state)

    |   loadSource _ = raise InternalError "loadSource"
   
    fun clearCC({valueStack, untaggedRegs, ...}:normalState): normalState =
        {valueStack=valueStack, ccState=CCIndeterminate, untaggedRegs=untaggedRegs}

    (* After we use an untagged value remove it from the state.  We only have
       a few registers and only use at most two untagged values before tagging them.
       For now, if we tag a value we clear the untagged set.  Division returns two
       tagged values but only one is actually used. *)
    fun removeAfterTag({valueStack, ccState, ...}, _) =
        {valueStack=valueStack, ccState=ccState, untaggedRegs=[]}

    fun codeICodeFunctionToX86{icode, functionName, maxLabels, maxPRegs= _, argRegsUsed, hasFullClosure, currentStackArgs, debugSwitches} =
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
                    [PushValue{arg=raxAsArg}])
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
                SOME {reg as GenReg _, ...} => RealRegister reg
            |   _ => raise InternalError "getUreg"

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
            ({valueStack=valueStack, ccState=ccState, untaggedRegs={ureg=iReg, reg=GenReg newReg}::untaggedRegs}, RealRegister(GenReg newReg))
        end

        (* Process the code until the end.  The code list is built up in reverse order. *)
        (* If we reach the end we haven't exited or reached a JumpLoop so we just return the
           final state. *)
        fun codeICode{icode=[], state: state, code, ...} = (state, code)

            (* Destination of a forward jump.  If the state was Exited this can
               continue.
               We should have the label in the list.
               We need to merge the states associated with dropping through and the
               state associated with the branch.  We can only adjust the state on the
               "drop through" side so if we need to change the state on the branch
               side we have to create a new branch. *)
        |   codeICode{icode=(instr as ForwardJumpLabel{label, result}) :: rest, state, code, context as {forwardLabels, loopArgs, handlers}} =
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
                        codeICode{icode=rest, state=NormalState labelState, code=instr :: code,
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
                            |   SOME resArg => checkTopInRax(s, argToPReg resArg)
                            
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
                            |   SOME resReg => resetStack(cstate, composite, argToPReg resReg)
                    in
                        if labelSp = finalSp
                        then (* We don't need to do anything to the incoming branch. *)
                        let
                            val newContext = {forwardLabels=newLabels, loopArgs=loopArgs, handlers=handlers}
                        in
                            codeICode{icode=rest, state=NormalState finalState,
                                      code=instr :: adjustSpOnDropThrough @ code,
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
                icode=(BeginHandler{handleStart, ...}) :: rest, state=Exited, code,
                context={forwardLabels, loopArgs, handlers=(handleLab, handlerStartState) :: otherHandlers}} =
            let
                val _ = handleLab = handleStart orelse raise InternalError "codeICode: BeginHandler handler stack mismatch"
            in
                codeICode{icode=rest, state=NormalState handlerStartState,
                        code=BeginHandler{handleStart=handleStart, workReg=RealRegister(GenReg ebx)} :: code,
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

        |   codeICode{icode=(instr as UnconditionalForwardJump{label}) :: rest, state=NormalState state, code, context={forwardLabels, loopArgs, handlers}} =
                codeICode{icode=rest, state=Exited, code=instr :: code,
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
        |   codeICode{icode=WordComparison{arg1=PReg(testR, PRegGeneral), arg2=(c as IntegerConstant _), ccRef} :: rest,
                      state=NormalState state, code, context} =
            let
                (* The value on the top of the stack should be in rax and the value we want to test. *)
                val startState = checkTopInRax(state, testR)
                (* The resulting state contains the condition we want to test. *)
                val checkState =
                    {valueStack= #valueStack startState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= []}
            in
                codeICode{icode=rest, state=NormalState checkState,
                          code=WordComparison{ arg1=raxAsArg, arg2=c, ccRef=ccRef } :: code, context=context}
            end

        |   codeICode{icode=WordComparison{arg1=PReg(testR, PRegGeneral), arg2=(c as AddressConstant _), ccRef} :: rest,
                      state=NormalState state, code, context} =
            let
                (* The value on the top of the stack should be in rax and the value we want to test. *)
                val startState = checkTopInRax(state, testR)
                val checkState = {valueStack= #valueStack startState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= []}
            in
                codeICode{icode=rest, state=NormalState checkState,
                          code=WordComparison{ arg1=raxAsArg, arg2=c, ccRef=ccRef } :: code,
                          context=context}
            end

        |   codeICode{icode=WordComparison{arg1=PReg(testR1, PRegGeneral), arg2=PReg(testR2, PRegGeneral), ccRef} :: rest,
                      state=NormalState state, code, context=context} =
            let
                (* The second argument should be on the top of the stack and in rax. *)
                val startState = checkTopInRax(state, testR2)
                val secondArg =
                    case findEntry (testR1, state) of
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
                          code=WordComparison{arg1=raxAsArg, arg2=wordOffsetAddress(stackPtr state - secondArg - 1, esp), ccRef=ccRef} :: code,
                          context=context}
            end

        |   codeICode{
                icode=WordComparison{
                    arg1 as MemoryLocation{index=NoMemIndex, ...},
                    arg2=MemoryLocation{offset=offset2, base=bReg2, index=NoMemIndex}, ccRef} :: rest,
                state=NormalState state, code, context=context} =
                (* This is used for LargeWord comparisons. *)
            let
                val startState as {untaggedRegs, valueStack, ...} = checkTopInRax(state, argToPReg bReg2)
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
                            WordComparison{arg1=RealRegister(GenReg newReg),
                                           arg2=MemoryLocation{offset=offset2, base=raxAsArg, index=NoMemIndex}, ccRef=ccRef} ::
                                loadArg1 @ code,
                          context=context}
            end
            
        |   codeICode{icode=WordComparison _ :: _, ...} =
                raise InternalError "codeICode: JumpOnWordComparison TODO"

        |   codeICode{icode=TestTagBit{arg, ccRef} :: rest, state=NormalState state, code, context} =
            let
                val argReg = argToPReg arg
                (* The value on the top of the stack should be in rax and the value we want to test. *)
                val startState = checkTopInRax(state, argReg)
                val checkState = {valueStack= #valueStack startState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= []}
            in
                codeICode{icode=rest, state=NormalState checkState,
                    (* TestTagBit only sets the condition codes.  It doesn't affect the register. *)
                          code=TestTagBit{arg=raxAsArg, ccRef=ccRef} :: code, context=context}
            end

            (* Set up an exception handler. *)
        |   codeICode{icode=PushExceptionHandler{ handlerAddr, handleStart } :: rest, state=NormalState state, code, context={forwardLabels, loopArgs, handlers}} =
            let
                val sHandle = argToPReg handlerAddr
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
                val resReg = argToPReg resultReg and handleAddr = argToPReg handlerAddr
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
                val workReg = RealRegister(GenReg ebx) (* Must be different from the return register. *)
            in
                codeICode{icode=rest, state=NormalState resultState,
                          code=PopExceptionHandler{ handlerAddr=handlerAddr, resultReg=resultReg, workReg=workReg } :: resetCode @ code,
                          context=context}
            end

            (* MoveArgument - this is used for loads, stores and moves.  *)
            (* Create a new Preg containing the contents of another Preg.
               We only ever want the top item to be in RAX so we push the top of the stack. *)
        |   codeICode{
                icode=MoveArgument{source as PReg(_, PRegGeneral), dest=PReg(destReg, PRegGeneral), kind=WordToWord} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val load = loadSource(source, eax, pushedState)
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=load @ pushCode @ code, context=context}
            end

        |   codeICode{
                icode=MoveArgument{source=RealRegister(GenReg srcReg), dest=PReg(destReg, PRegGeneral), kind=WordToWord} :: rest,
                state=NormalState state, code, context} =
            (* Move a specific register to the stack.  If it is anything but RAX we push it immediately. *)
            let
                val (pushedState, pushCode) = pushRax state
            in
                if srcReg = eax
                then codeICode{
                        icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                        code=pushCode @ code, context=context}
                else
                let
                    val sp = stackPtr pushedState
                    val {valueStack, ccState, untaggedRegs, ...} = pushedState
                in
                    codeICode{
                        icode=rest,
                        state=NormalState({valueStack={preg=destReg, loc=OnStack sp} :: valueStack, ccState=ccState, untaggedRegs=untaggedRegs}),
                        code=PushValue{arg=RealRegister(GenReg srcReg)} :: (pushCode @ code), context=context}
                end
            end
            
            (* Move a constant to a preg.  First push anything in RAX then load the constant into RAX. *)
        |   codeICode{icode=MoveArgument{source as AddressConstant srcValue, dest=PReg(destReg, PRegGeneral), kind} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                (* The constant should be an address. *)
                val _ = isShort srcValue andalso raise InternalError "AddressConstant: short"
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=MoveArgument{source=source, dest=raxAsArg, kind=kind} :: pushCode @ code,
                    context=context}
            end

            (* Move a short constant to a preg.  First push anything in RAX then load the constant into RAX. *)
        |   codeICode{icode=MoveArgument{source as IntegerConstant _, dest=PReg(destReg, PRegGeneral), kind} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=MoveArgument{source=source, dest=raxAsArg, kind=kind} :: pushCode @ code,
                    context=context}
            end

            (* Move a short constant to an untagged reg. *)
        |   codeICode{icode=MoveArgument{source as IntegerConstant _, dest=PReg(destReg, PRegUntagged), kind} :: rest, state=NormalState state, code, context} =
            let
                val (resultState, resReg) = newUreg(destReg, state)
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code=MoveArgument{source=source, dest=resReg, kind=kind} :: code,
                    context=context}
            end

            (* Load a value from a piece of memory, typically the offset in a tuple. *)
        |   codeICode{
                icode=MoveArgument{ source=MemoryLocation{offset, base as PReg(_, PRegGeneral), index=NoMemIndex}, dest=PReg(destReg, dType), kind} :: rest,
                state=NormalState state, code, context} =
            let
                (* This previously replaced the top.  Pushing the previous value is safer but means
                   values accumulate on the stack. *)
                val (pushedState, pushCode) = pushRax state
                val load = loadSource(base, eax, pushedState)
                val (resultState, resultReg) =
                    case dType of
                        PRegGeneral => (addRaxToState(pushedState, destReg), raxAsArg)
                    |   PRegUntagged => newUreg(destReg, pushedState)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=MoveArgument{ source=MemoryLocation{offset=offset, base=raxAsArg, index=NoMemIndex}, dest=resultReg, kind=kind} :: 
                        load @ pushCode @ code, context=context}
            end

        |   codeICode{
                icode=MoveArgument{ source=MemoryLocation{offset, base as RealRegister _, index=NoMemIndex}, dest=PReg(destReg, PRegGeneral), kind} :: rest,
                state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
            in
                (* We also generate loads from offsets in ebp. *)
                codeICode{icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=MoveArgument{ source=MemoryLocation{offset=offset, base=base, index=NoMemIndex}, dest=raxAsArg, kind=kind} :: pushCode @ code,
                    context=context}
            end

        |   codeICode{
                icode=MoveArgument{ source=MemoryLocation {offset, base, index=MemIndex1(PReg(iReg, PRegUntagged))}, dest=PReg(destReg, dType), kind} :: rest,
                state=NormalState (state as {untaggedRegs, ...}), code, context} =
            let
                (* The index is an untagged value. *)
                val realIReg =
                    case List.find (fn {ureg, ...} => ureg = iReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => RealRegister reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val baseOnStack =
                    case findEntry (argToPReg base, state) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val (resultState, resultReg) =
                    case dType of
                        PRegGeneral => (addRaxToState(state, destReg), raxAsArg)
                    |   PRegUntagged =>
                            ({valueStack= #valueStack state, ccState= #ccState state,
                                untaggedRegs =[{ureg=destReg, reg=GenReg ecx}]}, rcxAsArg)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code =
                        MoveArgument{source=MemoryLocation {offset=offset, base=rbxAsArg, index=MemIndex1 realIReg}, dest=resultReg, kind=kind} ::
                        MoveArgument{source=wordOffsetAddress(stackPtr state - baseOnStack - 1, esp), dest=rbxAsArg, kind=WordToWord} ::
                        code,
                    context=context}
            end

        |   codeICode{
                icode=MoveArgument{ source=MemoryLocation {offset, base, index}, dest=PReg(destReg, dType), kind} :: rest,
                state=NormalState state, code, context} =
            let
                (* The index value is going to be at the top of the stack. We replace this value with
                   the result.  The base address will be below it but we just leave it on the stack. *)
                val (iReg, indexed) =
                    case index of
                        MemIndex1(PReg(r, PRegGeneral)) => (r, MemIndex1 raxAsArg)
                    |   MemIndex2(PReg(r, PRegGeneral)) => (r, MemIndex2 raxAsArg)
                    |   MemIndex4(PReg(r, PRegGeneral)) => (r, MemIndex4 raxAsArg)
                    |   MemIndex8(PReg(r, PRegGeneral)) => (r, MemIndex8 raxAsArg)
                    |   _ => raise InternalError "codeICode: MemIndex"
                val startState = checkTopInRax(state, iReg)
                val baseOnStack =
                    case findEntry (argToPReg base, state) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val (resultState, resultReg) =
                    case dType of
                        PRegGeneral => (addRaxToState(startState, destReg), raxAsArg)
                    |   PRegUntagged =>
                            ({valueStack= #valueStack startState, ccState= #ccState startState,
                                untaggedRegs =[{ureg=destReg, reg=GenReg ecx}]}, rcxAsArg)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code =
                        MoveArgument{source=MemoryLocation {offset=offset, base=rbxAsArg, index=indexed}, dest=resultReg, kind=kind} ::
                        MoveArgument{source=wordOffsetAddress(stackPtr state - baseOnStack - 1, esp), dest=rbxAsArg, kind=WordToWord} ::
                        code,
                    context=context}
            end

            (* Store a pseudo-register in a memory location. *)
        |   codeICode{icode=MoveArgument{ source=PReg(valueReg, PRegUntagged), dest=MemoryLocation{offset, base, index=NoMemIndex}, kind} :: rest,
                      state=NormalState state, code, context} =
            let
                val (pushedState as { valueStack, ccState, untaggedRegs}, pushCode) = pushRax state
                val baseOnStack = 
                    case findEntry (argToPReg base, pushedState) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val realVReg =
                    case List.find (fn {ureg, ...} => ureg = valueReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => RealRegister reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val loadBaseToEax = loadStack(eax, (stackPtr pushedState - baseOnStack -1) * wordSize)
                val store =
                    [MoveArgument{
                        source=realVReg,
                        dest=MemoryLocation{base=RealRegister(GenReg eax), offset=offset, index=NoMemIndex}, kind=kind}]
                val resultState = { valueStack= valueStack, ccState=ccState, untaggedRegs=[]}
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code=store @ loadBaseToEax @ pushCode @ code, context=context}
            end

            (* Store an untagged register in a memory location.  This is currently used to store the string
               length word. *)
        |   codeICode{icode=MoveArgument{ source=PReg(srcReg, PRegGeneral), dest=MemoryLocation{offset, base, index=NoMemIndex}, kind} :: rest,
                      state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val baseOnStack = 
                    case findEntry (argToPReg base, pushedState) of
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
                        source=RealRegister(GenReg ebx),
                        dest=MemoryLocation{base=RealRegister(GenReg eax), offset=offset, index=NoMemIndex}, kind=kind}]
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
                    case findEntry (argToPReg base, pushedState) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val loadBaseToEax = loadStack(eax, (stackPtr pushedState - baseOnStack -1) * wordSize)
                val store =
                    [MoveArgument{
                        source=AddressConstant value,
                        dest=MemoryLocation{base=RealRegister(GenReg eax), offset=offset, index=NoMemIndex}, kind=kind}]
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
                    case findEntry (argToPReg base, pushedState) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val loadBaseToEax = loadStack(eax, (stackPtr pushedState - baseOnStack -1) * wordSize)
                val store =
                    [MoveArgument{
                        source=IntegerConstant value,
                        dest=MemoryLocation{base=RealRegister(GenReg eax), offset=offset, index=NoMemIndex}, kind=kind}]
            in
                codeICode{
                    icode=rest, state=NormalState pushedState,
                    code=store @ loadBaseToEax @ pushCode @ code, context=context}
            end

        |   codeICode{icode=MoveArgument{ source=PReg(valueReg, PRegGeneral), dest=MemoryLocation {offset, base, index}, kind} :: rest,
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
                    case findEntry (argToPReg base, state) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val rcxAsArg = RealRegister(GenReg ecx)
            in
                codeICode{icode=rest, state=NormalState startState,
                    code =
                        MoveArgument{source=raxAsArg, dest=MemoryLocation {offset=offset, base=rcxAsArg, index=indexed}, kind=kind} ::
                        MoveArgument{source=wordOffsetAddress(stackPtr state - baseOnStack - 1, esp), dest=rcxAsArg, kind=WordToWord} ::
                        MoveArgument{source=wordOffsetAddress(stackPtr state - indexOnStack - 1, esp), dest=rbxAsArg, kind=WordToWord} ::
                        code,
                    context=context}
            end

        |   codeICode{icode=
                       MoveArgument{ source=PReg(valueReg, PRegUntagged),
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
                        SOME {reg as GenReg _, ...} => RealRegister reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val realVReg =
                    case List.find (fn {ureg, ...} => ureg = valueReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => RealRegister reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val baseOnStack =
                    case findEntry (argToPReg base, state) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val resultState = { valueStack= valueStack, ccState=ccState, untaggedRegs=[]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code =
                        MoveArgument{source=realVReg, dest=MemoryLocation {offset=offset, base=raxAsArg, index=MemIndex1 realIReg}, kind=kind} ::
                        MoveArgument{source=wordOffsetAddress(stackPtr state - baseOnStack - 1, esp), dest=raxAsArg, kind=WordToWord} ::
                        pushCode @ code,
                    context=context}
            end

            (* Other combinations of move aren't used at the moment. *)
        |   codeICode{icode=MoveArgument _ :: _, ...} =
                raise InternalError "codeICode: MoveArgument"

            (* Load a value from the real stack.  Offset zero is the return address,
               subsequent values are arguments that were pushed by the caller. *)
        |   codeICode{icode=MoveStackArgToPreg{offset, dest} :: rest, state=NormalState state, code, context} =
            let
                val destReg = argToPReg dest
                val (pushedState, pushCode) = pushRax state
                val load = loadStack(eax, (stackPtr pushedState + offset) * wordSize)
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=load @ pushCode @ code, context=context}
            end
            
        |   codeICode{icode=AllocateMemoryOperation{ size, flags, dest} :: rest, state=NormalState state, code, context} =
            let
                val destReg = argToPReg dest
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
                val fReg = argToPReg flags and destReg = argToPReg dest
                val startState = checkTopInRax(state, fReg) (* Should be in eax. *)
                val loadSize = loadSource(size, edx, startState)
                (* We put the size in rdx, the flags in rax and the result in rdi. *)
                val allocMem = [AllocateMemoryVariable{size=rdxAsArg, flags=raxAsArg, dest=rdiAsArg, initialiser=NONE}]
                val moveToRAX = [MoveArgument{source=rdiAsArg, dest=raxAsArg, kind=WordToWord}]
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
                val destReg = argToPReg dest and initReg = argToPReg init
                val startState = checkTopInRax(state, initReg) (* Should be in eax. *)
                val loadSize = loadSource(size, ecx, startState)
                val loadFlags = loadSource(flags, ebx, startState)
                val allocMem = [AllocateMemoryVariable{size=rcxAsArg, flags=rbxAsArg, dest=rdxAsArg, initialiser=SOME raxAsArg}]
                val moveToRAX = [MoveArgument{source=rdxAsArg, dest=raxAsArg, kind=WordToWord}]
                val resultState = clearCC(addRaxToState(startState, destReg))
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code= moveToRAX @ allocMem @ loadFlags @ loadSize @ code, context=context}
            end

            (* Marks the end of setting the fields of a tuple.  This is used in the next
               level to avoid reordering a new allocation until a previously allocated
               section of memory has been fully initialised. *)
        |   codeICode{icode=(instr as InitialisationComplete _) :: rest, state, code, context} =
                codeICode{icode=rest, state=state, code=instr :: code, context=context}

            (* Function call. *)
        |   codeICode{icode=FunctionCall{ regArgs, stackArgs, dest, isTail=false, callKind} :: rest,
                      state=NormalState state, code, context} =
            let
                val destReg = argToPReg dest
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
                            pushAllArgs(args, newState, PushValue{arg=raxAsArg} :: load @ code)
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
                        val dest = MemoryLocation{base=RealRegister(GenReg esp), offset=(argOffset+n)*wordSize, index=NoMemIndex}
                        (* If this is a 32-bit constant we can do a direct memory move. Othewise load
                           it into a register.  It might be easier to do some of this in the codetree
                           translation.  Treating all constants this way saves loading them into
                           "registers" in order to save the values. *)
                        val copy =
                            case r of
                                c as IntegerConstant value =>
                                    if is32bit value
                                    then [MoveArgument{source=c, dest=dest, kind=WordToWord}]
                                    else [MoveArgument{source=rsiAsArg, dest=dest, kind=WordToWord},
                                          MoveArgument{source=c, dest=rsiAsArg, kind=WordToWord}]
                            |   c as AddressConstant _ =>
                                    if isX64
                                    then [MoveArgument{source=rsiAsArg, dest=dest, kind=WordToWord},
                                          MoveArgument{source=c, dest=rsiAsArg, kind=WordToWord}]
                                    else [MoveArgument{source=c, dest=dest, kind=WordToWord}]
                           |    r => MoveArgument{source=rsiAsArg, dest=dest, kind=WordToWord} ::
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
        |   codeICode{icode=(instr as StartLoop{arguments, loopLabel}) :: rest, state=NormalState state, code,
                      context={forwardLabels, handlers, loopArgs}} =
            let
                (* The arguments have been evaluated to the stack.  Replace
                   the preg entries in the state with those of the loop variables. *)
                val (pushedState, pushCode) = pushRax state
                val numArgs = List.length arguments
                val headList = List.take(#valueStack pushedState, numArgs)
                fun repSourceWithDest({source, loopReg}, {preg, loc}) =
                     if argToPReg source = preg then {preg=argToPReg loopReg, loc=loc} else raise InternalError "codeICode: StartLoop"
                val newHead = ListPair.mapEq repSourceWithDest (List.rev arguments, headList)
                val tailList = List.drop(#valueStack pushedState, numArgs)
                (* The CCstate is indeterminate because we may have looped. *)
                val bodyState = {valueStack=newHead @ tailList, ccState= CCIndeterminate, untaggedRegs= []}
                val newContext = {loopArgs=(bodyState, loopLabel) :: loopArgs, forwardLabels=forwardLabels, handlers=handlers}
            in
                codeICode{icode=rest, state=NormalState bodyState, code=instr :: pushCode @ code,
                          context=newContext}
            end

            (* A loop tail jump.  The arguments have been evaluated to the stack. *)
        |   codeICode{icode=(JumpLoop{arguments, loopLabel}) :: rest, state=NormalState state, code,
                      context=context as {loopArgs=(startState, lab) :: _, ...}} =
            let
                val _ = loopLabel = lab orelse raise InternalError "codeICode: JumpLoop - mismatched labels"
                (* Move each argument from the source to the loop variable.  This isn't the same as
                   MoveArgument because the destination already exists. *)
                fun moveArg{source, loopReg} =
                let
                    val load = loadSource(source, eax, state)
                    val store =
                        case findEntry (argToPReg loopReg, state) of
                            OnStack s => storeMemory(eax, esp, (stackPtr state - s - 1) * wordSize)
                        |   InRax => raise InternalError "codeICode: JumpLoop"
                in
                    store @ load
                end
                val storeAll = List.foldl(fn (arg, l) => l @ moveArg arg) [] arguments
                val instrs =
                    JumpLoop{arguments=[], loopLabel=loopLabel} :: resetStackPtr(stackPtr state - stackPtr startState) :: storeAll
            in
                codeICode{icode=rest, state=Exited, code=instrs @ code, context=context}
            end

        |   codeICode{icode=JumpLoop _ :: _, ...} =
                raise InternalError "codeICode: JumpLoop out of loop"

            (* Raise an exception using the value in the "packet" register as the exception packet.
               Since this exits we can ignore any further code and return the code and state. *)
        |   codeICode{icode=RaiseExceptionPacket{packet} :: rest, state=NormalState state, code, context} =
            let
                val load = loadSource(packet, eax, state)
            in
                codeICode{icode=rest, state=Exited, code=RaiseExceptionPacket {packet=raxAsArg } :: load @ code, context=context}
            end

            (* Reserve a contiguous area of memory on the stack.  This is used to receive the
               results of a tuple.  It may be set within the same function or the address
               may be passed into another function to allow it to return multiple results. *)
        |   codeICode{icode=ReserveContainer{size, address} :: rest, state=NormalState state, code, context} =
            let
                val addrReg = argToPReg address
                val (pushedState, pushCode) = pushRax state
                (* The memory must be cleared in case we have a GC. *)
                val pushAll = List.tabulate(size, fn _ => PushValue{arg=IntegerConstant(tag 0)})
                val sp = stackPtr pushedState
            in
                codeICode{icode=rest,
                    state=NormalState(
                            {valueStack={loc=InRax, preg=addrReg} :: {loc=OnStack(sp+size-1), preg= ~1} :: #valueStack pushedState,
                             ccState=CCIndeterminate, untaggedRegs= []}),
                    code=MoveArgument{source=RealRegister(GenReg esp), dest=raxAsArg, kind=WordToWord} :: (pushAll @ pushCode) @ code, context=context}
            end

        |   codeICode{icode=IndexedCaseOperation{ testReg, workReg=_, cases, startValue} :: rest,
                      state=NormalState state, code, context={loopArgs, handlers, forwardLabels}} =
            let
                val tReg = argToPReg testReg
                (* We should have the value to test at the top of the stack. *)
                val startState = checkTopInRax(state, tReg)

                (* This behaves rather like an unconditional branch except that it can go to several places.
                   The case labels and the default label have to be added to the label list. *)
                val labelsFromCase =
                    map(fn ilab => (ilab, startState)) cases @ forwardLabels
            in
                codeICode{icode=rest, state=Exited,
                          code=IndexedCaseOperation{ testReg=raxAsArg, workReg=RealRegister(GenReg ebx), cases=cases, startValue=startValue} :: code,
                          context={loopArgs=loopArgs, handlers=handlers, forwardLabels=labelsFromCase}}
            end

            (* Lock a mutable segment. *)
        |   codeICode{icode=LockMutable{ addr } :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val load = loadSource(addr, eax, pushedState)
                val lockCode = LockMutable {addr=raxAsArg} :: (load @ pushCode)
                val resultState = clearCC pushedState (* May affect the CC *)
            in
                codeICode{icode=rest, state=NormalState resultState, code=lockCode @ code, context=context}
            end

       |    codeICode{icode=ReturnResultFromFunction{ resultReg, numStackArgs } :: rest, state=NormalState state, code, context} =
            let
                val resReg = argToPReg resultReg
                val (_, resetCode) = resetStack(state, initialState, resReg)
                val returnCode = ReturnResultFromFunction{resultReg=raxAsArg, numStackArgs=numStackArgs } :: resetCode
            in
                (* We may have more than one return point so we can't just stop here. *)
                codeICode{icode=rest, state=Exited, code=returnCode @ code, context=context}
            end

        |   codeICode{icode=ArithmeticFunction{ oper, resultReg=PReg(resReg, PRegGeneral), operand1=PReg(oper1Reg, PRegGeneral), operand2 as IntegerConstant _, ccRef } :: rest,
                      state=NormalState state, code, context} =
            let
                (* The result is a tagged register.  This is used when the constant operand can be untagged. *)
                val topState = checkTopInRax(state, oper1Reg)
                val resultState = addRaxToState(topState, resReg)
                val checkState =
                    {valueStack= #valueStack resultState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= #untaggedRegs resultState}
            in
                codeICode{icode=rest, state=NormalState checkState,
                    code=ArithmeticFunction { oper=oper, resultReg=raxAsArg, operand1=raxAsArg, operand2=operand2, ccRef=ccRef } :: code,
                    context=context}
            end

        |   codeICode{icode=ArithmeticFunction{ oper, resultReg=PReg(dReg, PRegUntagged), operand1 as PReg(_, PRegGeneral), operand2 as IntegerConstant _, ccRef } :: rest,
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
                val resReg = RealRegister(GenReg spareReg)
                val loadCode = loadSource(operand1, spareReg, pushedState)
                val resultState =
                    {valueStack= valueStack, ccState= CCState(ccRef, fn i=>i), untaggedRegs ={ureg=dReg, reg=GenReg spareReg} :: untaggedRegs}
             in
                codeICode{icode=rest, state=NormalState resultState,
                    code=ArithmeticFunction { oper=oper, resultReg=resReg, operand1=resReg, operand2=operand2, ccRef=ccRef } :: loadCode @ pushCode @ code,
                    context=context}
            end

        |   codeICode{icode=ArithmeticFunction{ oper, resultReg=PReg(resReg, PRegGeneral), operand1=PReg(oper1Reg, PRegUntagged), operand2 as IntegerConstant _, ccRef } :: rest,
                      state=NormalState (state as {untaggedRegs, ...}), code, context} =
            let
                (* The result is a tagged register but the argument is untagged.  This is typically when we want to set the tag on a  value. *)
                val realSReg =
                    case List.find (fn {ureg, ...} => ureg = oper1Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => RealRegister reg
                    |   _ => raise InternalError "codeICode: ArithmeticFunction"
                val (pushedState, pushCode) = pushRax state
                val addState = addRaxToState(pushedState, resReg)
                val resultState = removeAfterTag(addState, oper1Reg)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code= (* TODO: We could use LEA since this is normally (always?) ADD *)
                        MoveArgument{source=realSReg, dest=raxAsArg, kind=WordToWord} ::
                        ArithmeticFunction { oper=oper, resultReg=realSReg, operand1=realSReg, operand2=operand2, ccRef=ccRef } :: pushCode @ code,
                    context=context}
            end

        |   codeICode{icode=ArithmeticFunction{ oper, resultReg, operand1=PReg(oper1Reg, PRegGeneral), operand2=PReg(oper2Reg, PRegGeneral), ccRef } :: rest,
                      state=NormalState state, code, context} =
            let
                (* The top item should be in RAX but that is the second argument.  That's all right for
                   addition but wrong for subtraction.  Move the first argument into RBX, load the second into
                   RAX and then do the operation. *)
                val resReg = argToPReg resultReg
                val topState = checkTopInRax(state, oper2Reg)
                val secondArg =
                    case findEntry (oper1Reg, state) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: ArithmeticFunction"
                val resultState = addRaxToState(topState, resReg)
                val checkState =
                    {valueStack= #valueStack resultState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= #untaggedRegs resultState}
            in
                codeICode{icode=rest, state=NormalState checkState,
                    code= (* N.B. In reverse order *)
                        ArithmeticFunction {
                            oper=oper, resultReg=raxAsArg, operand1=raxAsArg, operand2=RealRegister(GenReg ebx), ccRef=ccRef } ::
                        MoveArgument
                            { source=MemoryLocation{offset=(stackPtr state - secondArg - 1) * wordSize, base=RealRegister(GenReg esp), index=NoMemIndex},
                              dest=raxAsArg, kind=WordToWord } ::
                        MoveArgument { source=raxAsArg, dest=RealRegister(GenReg ebx), kind=WordToWord} ::
                        code,
                    context=context}
            end

        |   codeICode{
                icode=ArithmeticFunction{ oper, resultReg,
                    operand1 as MemoryLocation{index=NoMemIndex, ...},
                    operand2=MemoryLocation{offset=offset2, base=bReg2, index=NoMemIndex}, ccRef } :: rest,
                state=NormalState state, code, context} =
                (* This is used for LargeWord operations. *)
            let
                val startState = checkTopInRax(state, argToPReg bReg2)
                (* Load the first argument into an untagged register and use it as the result. *)
                val (resultState, resultReg) = newUreg(argToUReg resultReg, startState)
                val resReg = case resultReg of RealRegister(GenReg r) => r | _ => raise Match
                val loadArg1 = loadSource(operand1, resReg, startState)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        ArithmeticFunction {
                            oper=oper, resultReg=resultReg, operand1=resultReg,
                            operand2=MemoryLocation{offset=offset2, base=raxAsArg, index=NoMemIndex}, ccRef=ccRef } ::
                        loadArg1 @ code,
                    context=context}
            end

        |   codeICode{icode=ArithmeticFunction _ :: _, ...} =
                raise InternalError "TODO: CodeTransform ArithmeticFunction"

        |   codeICode{icode=TagValue { source, dest } :: rest,
                      state=NormalState (state as {untaggedRegs, ...}), code, context} =
            let
                val sReg = argToUReg source and destReg = argToPReg dest
                val realSReg =
                    case List.find (fn {ureg, ...} => ureg = sReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => RealRegister reg
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
                val dReg = argToUReg dest
                val (pushedState, pushCode) = pushRax state (* Simplifies things *)
                val (resultState, regAsArg) = newUreg(dReg, pushedState)
                val resReg = case regAsArg of RealRegister(GenReg r) => r | _ => raise Match
                val loadCode = loadSource(source, resReg, pushedState)
                val resultState = clearCC resultState
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        ShiftOperation{shift=if isSigned then SAR else SHR, resultReg=regAsArg, operand=regAsArg,
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
                val loadCode = loadSource(baseSource, eax, pushedState)
                val filteredUntagged =
                    List.filter (fn {ureg, ...} => ureg <> iReg) untaggedRegs
                val resultState =
                    {valueStack=valueStack, ccState=ccState, untaggedRegs={ureg=dReg, reg=realIReg}::filteredUntagged}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        LoadEffectiveAddress {base=SOME raxAsArg, offset=offset, index=MemIndex1(RealRegister realIReg), dest=RealRegister realIReg} ::
                        loadCode @ pushCode@code,
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

        |   codeICode{icode=ShiftOperation {shift, resultReg, operand=PReg(operReg, PRegGeneral), shiftAmount as IntegerConstant _, ccRef } :: rest,
                      state=NormalState state, code, context} =
            let
                val resReg = argToPReg resultReg
                val topState = checkTopInRax(state, operReg)
                val resultState = addRaxToState(topState, resReg)
                val checkState =
                    {valueStack= #valueStack resultState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= #untaggedRegs resultState}
            in
                codeICode{icode=rest, state=NormalState checkState,
                    code=ShiftOperation { shift=shift, resultReg=raxAsArg, operand=raxAsArg, shiftAmount=shiftAmount, ccRef=ccRef } :: code,
                    context=context}
            end

        |   codeICode{icode=ShiftOperation {shift, resultReg, operand=PReg(operReg, PRegUntagged), shiftAmount=PReg(shiftReg, PRegUntagged), ccRef } :: rest,
                      state=NormalState ({untaggedRegs, valueStack, ...}), code, context} =
            let
                (* Shift by a variable.  This should be in rcx which is where we want it. *)
                val resReg = argToUReg resultReg
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
                    code=ShiftOperation{ shift=shift, resultReg=RealRegister realOperReg, operand=RealRegister realOperReg,
                                         shiftAmount=RealRegister realShiftReg, ccRef=ccRef} :: code, context=context}
            end

        |   codeICode{
                icode=ShiftOperation {shift, resultReg,
                    operand as MemoryLocation{index=NoMemIndex, ...},
                    shiftAmount=PReg(shiftReg, PRegUntagged), ccRef } :: rest,
                state=NormalState (state as {untaggedRegs, valueStack, ...}), code, context} =
            let
                (* Shift by a variable.  This is used for LargeWord shifts.  The argument is boxed rather than untagged
                   so we will have loaded the shift amount, which is tagged, into edx.  *)
                val resReg = argToUReg resultReg
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
                    code=ShiftOperation{ shift=shift, resultReg=rdxAsArg, operand=rdxAsArg,
                                         shiftAmount=rcxAsArg, ccRef=ccRef} ::
                         loadCode @
                         (MoveArgument{source=rdxAsArg, dest=rcxAsArg, kind=WordToWord} :: code), context=context}
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
                val startState = checkTopInRax(state, argToPReg bReg2)
                (* Load the first argument into an untagged register and use it as the result. *)
                val (resultState, resultReg) = newUreg(argToUReg resultReg, startState)
                val resReg = case resultReg of RealRegister(GenReg r) => r | _ => raise Match
                val loadArg1 = loadSource(operand1, resReg, startState)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        Multiplication { resultReg=resultReg, operand1=resultReg,
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
                val resReg = argToUReg resultReg and op1Reg = argToUReg operand1 and op2Reg = argToUReg operand2
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
                    code=Multiplication{ resultReg=RealRegister realOp1Reg, operand1=RealRegister realOp1Reg,
                                         operand2=RealRegister realOp2Reg, ccRef=ccRef} :: code, context=context}
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
                        untaggedRegs =[{ureg=argToUReg quotient, reg=GenReg eax}, {ureg=argToUReg remainder, reg=GenReg edx}]}
             in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        Division{ isSigned=isSigned, dividend=raxAsArg, divisor=rcxAsArg, quotient=raxAsArg, remainder=rdxAsArg} ::
                        loadDividend @ loadDivisor @ pushCode @ code, context=context}
            end

        |   codeICode{icode=Division { isSigned, dividend, divisor, quotient, remainder } :: rest,
                      state=NormalState state, code, context} =
            let
                val dividendReg = argToUReg dividend and divisorReg = argToUReg divisor
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
                        untaggedRegs =[{ureg=argToUReg quotient, reg=GenReg eax}, {ureg=argToUReg remainder, reg=GenReg edx}]}
                (* The untagging code puts the first value into edx and the second into ecx.  We
                   have to move the dividend into eax. *)
                val _ = realDividend = GenReg edx orelse raise InternalError "codeICode: dividend"
                val _ = realDivisor = GenReg ecx orelse raise InternalError "codeICode: divisor"
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        Division{ isSigned=isSigned, dividend=raxAsArg, divisor=rcxAsArg, quotient=raxAsArg, remainder=rdxAsArg} ::
                        MoveArgument { source=RealRegister realDividend, dest=raxAsArg, kind=WordToWord } :: pushCode @ code, context=context}
            end

        |   codeICode{icode=AtomicExchangeAndAdd{ destAddr, source } :: rest,
                      state=NormalState state, code, context} =
            let
                (* Load the "destination" address into rax. *)
                val load = loadSource(destAddr, eax, state)
                (* The "source" is an untagged register. *)
                val srcReg = getUreg(argToUReg source, state)
            in
                codeICode{icode=rest, state=NormalState state,
                    code=AtomicExchangeAndAdd{destAddr=raxAsArg, source=srcReg} :: load @ code, context=context}
            end

        |   codeICode{icode=BoxLargeWord{ dest, source } :: rest, state=NormalState state, code, context} =
            let
                (* Allocate a one word cell and store the untagged value into it. *)
                val destReg = argToPReg dest
                val srcReg = getUreg(argToUReg source, state)
                val realSource =
                    case srcReg of RealRegister(GenReg r) => r | _ => raise InternalError "BoxLargeWord"
                val (newSrcReg, pushedState, pushCode) =
                    if realSource = eax
                    then
                    let
                        (* If the value we want to store is in eax i.e. the quotient, we
                           must move it to another register before the allocation. *)
                        val pushCode =
                            [MoveArgument{ source=srcReg, dest=rcxAsArg, kind=WordToWord}]
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
                        MoveArgument{ source=newSrcReg, dest=wordOffsetAddress(0, eax), kind=WordToWord} ::
                        AllocateMemoryOperation{ size=1, flags=0wx1, dest=raxAsArg} :: pushCode @ code, context=context}
            end

        |   codeICode{icode=CompareByteVectors { vec1Addr, vec2Addr, length, ccRef } :: rest, state=NormalState state, code, context} =
            let
                val {untaggedRegs, ...} = state
                val vec1Reg = argToUReg vec1Addr and vec2Reg = argToUReg vec2Addr and lenReg = argToUReg length
                val realVec1Reg =
                    case List.find (fn {ureg, ...} => ureg = vec1Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: CompareByteVectors"
                val realVec2Reg =
                    case List.find (fn {ureg, ...} => ureg = vec2Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: CompareByteVectors"
                val realLenReg =
                    case List.find (fn {ureg, ...} => ureg = lenReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: CompareByteVectors"
                (* These aren't in the registers we want so we have to shuffle them. *)
                val _ = realVec1Reg = GenReg edx orelse raise InternalError "codeICode: vec1Reg"
                val _ = realVec2Reg = GenReg ecx orelse raise InternalError "codeICode: vec2Reg"
                val _ = realLenReg = GenReg esi orelse raise InternalError "codeICode: lenReg"
                val resultState =
                    {valueStack= #valueStack state, ccState=CCState(ccRef, fn i=>i), untaggedRegs= []}
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code =
                        CompareByteVectors{vec1Addr=rsiAsArg, vec2Addr=rdiAsArg, length=rcxAsArg, ccRef=ccRef} ::
                        MoveArgument{source=RealRegister realVec1Reg, dest=rsiAsArg, kind=WordToWord} ::
                        MoveArgument{source=RealRegister realLenReg, dest=rcxAsArg, kind=WordToWord} ::
                        MoveArgument{source=RealRegister realVec2Reg, dest=rdiAsArg, kind=WordToWord} :: code, context=context}
            end

            (* These should not occur here.  They are only added during this phase. *)
        |   codeICode{icode=PushValue _ :: _, ...} = raise InternalError "codeICode: PushValue"
        |   codeICode{icode=ResetStackPtr _ :: _, ...} = raise InternalError "codeICode: ResetStackPtr"

        val (fnState, fnCode) = codeICode{icode=icode, state=NormalState initialState, code=[], context={loopArgs=[], forwardLabels=[], handlers=[]}}
        val _ = case fnState of Exited => () | _ => raise InternalError "fnstate not exited"
    in
        (* Reverse the list and then code-generate it. *)
        codeAsX86Code{icode=List.rev fnCode, maxLabels = !labelCounter, stackRequired = !maxStack,
                      inputRegisters= argRegsUsed @ (if hasFullClosure then [edx] else []), debugSwitches=debugSwitches,
                      functionName=functionName}
    end

    structure Sharing =
    struct
        type x86ICode = x86ICode
        and  genReg   = genReg
    end
end;
