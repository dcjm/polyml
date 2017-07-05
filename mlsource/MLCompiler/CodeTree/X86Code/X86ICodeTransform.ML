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

    fun argToPReg(PReg n) = n | argToPReg _ = raise InternalError "argToPReg"
    

    fun loadMemory(reg, base, offset) =
        [MoveArgument{source=MemoryLocation{base=RealRegister(GenReg base), offset=offset}, dest=RealRegister(GenReg reg)}]
    and loadStack(reg, offset) = loadMemory(reg, esp, offset)
    and storeMemory(reg, base, offset) =
        [MoveArgument{source=RealRegister(GenReg reg), dest=MemoryLocation{base=RealRegister(GenReg base), offset=offset}}]

    (* Transform the version of the ICode generated from the codetree by replacing the pseudo-registers by
       real locations.  When this is complete it passes it on to the next phase for code-generation.
       This is currently very simple-minded and just uses rax as an accumulator and the
       stack for the rest. *)

    fun codeICodeFunctionToX86{icode, functionName, maxLabels, maxPRegs= _, argRegsUsed, hasFullClosure, currentStackArgs, debugSwitches} =
    let
        (* We may need to create additional labels. *)
        val labelCounter = ref maxLabels
        fun newLabel() = ILabel(!labelCounter) before labelCounter := !labelCounter + 1

        val maxStack = ref 0 (* High-water mark *)

        datatype pregLoc = InRax | OnStack of int
        
        (* The state of the condition codes. *)
        datatype ccState = CCIndeterminate | CCState of ccRef * (branchOps->branchOps)

        datatype state = NormalState of normalState | Exited
        withtype normalState = { valueStack : {preg: int, loc: pregLoc} list, ccState: ccState }
        
        val initialState:normalState = {valueStack=[], ccState=CCIndeterminate} (* Initial and final state. *)

        val raxAsArg = RealRegister(GenReg eax)

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
    
        fun loadSource(source, dest, state) =
            case findEntry (source, state) of
                InRax =>
                    if dest = eax then [] else [MoveArgument{ source=RealRegister(GenReg eax), dest=RealRegister(GenReg dest)}]
            |   OnStack s => loadStack(dest, (stackPtr state - s - 1) * wordSize)

        (* If the top of the stack is in RAX push it otherwise do nothing. *)
        fun pushRax (state as {valueStack={preg, loc=InRax} :: stack, ccState }) =
            let
                val sp = stackPtr state
                val () = if sp >= !maxStack then maxStack := sp+1 else ()
            in
                (* Return the updated state with the value pushed.  Pushing does not affect condition codes. *)
                ({valueStack={preg=preg, loc=OnStack sp} :: stack, ccState=ccState}, [PushValue{arg=raxAsArg}])
            end
        |   pushRax stack = (stack, [])

        (* Add a pseudo-register containing rax to the top of the stack. *)
        fun addRaxToState({valueStack, ccState}, destReg) =
            {valueStack={preg=destReg, loc=InRax} :: valueStack, ccState=ccState}

        (* Check that the top of the stack is the required pseudo-register and the value is in Rax.
           Returns the state with this item popped. *)
        fun checkTopInRax({valueStack={preg, loc=InRax } :: resultState, ccState}, checkReg) =
            if preg = checkReg then {valueStack=resultState, ccState=ccState} else raise InternalError "checkTopInRax"
        |   checkTopInRax _ = raise InternalError "checkTopInRax"
        
        fun clearCC({valueStack, ...}:normalState): normalState = {valueStack=valueStack, ccState=CCIndeterminate}

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
                if currentSp = oldSp then [] else [ResetStackPtr{numWords=currentSp - oldSp}]
        in
            (* Currently ResetStackPtr uses arithmetic operations that affect the condition codes. *)
            (clearCC newState, code)
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
                             ccState=CCIndeterminate}
                        (* Currently the only thing we need to adjust is the stack pointer. *)
                        val finalSp = stackPtr composite
                        and labelSp = stackPtr labelState

                        val (finalState, adjustSpOnDropThrough) =
                            case result of
                                NONE =>
                                let
                                    val currentSp = stackPtr cstate and finalSp = stackPtr composite
                                in
                                    (composite, if currentSp = finalSp then [] else [ResetStackPtr{numWords=currentSp-finalSp}])
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
        |   codeICode{icode=WordComparison{arg1=PReg testR, arg2=(c as IntegerConstant _), ccRef} :: rest,
                      state=NormalState state, code, context} =
            let
                (* The value on the top of the stack should be in rax and the value we want to test. *)
                val startState = checkTopInRax(state, testR)
                (* The resulting state contains the condition we want to test. *)
                val checkState = {valueStack= #valueStack startState, ccState=CCState(ccRef, fn i=>i)}
            in
                codeICode{icode=rest, state=NormalState checkState,
                          code=WordComparison{ arg1=raxAsArg, arg2=c, ccRef=ccRef } :: code, context=context}
            end

        |   codeICode{icode=WordComparison{arg1=PReg testR, arg2=(c as AddressConstant _), ccRef} :: rest,
                      state=NormalState state, code, context} =
            let
                (* The value on the top of the stack should be in rax and the value we want to test. *)
                val startState = checkTopInRax(state, testR)
                val checkState = {valueStack= #valueStack startState, ccState=CCState(ccRef, fn i=>i)}
            in
                codeICode{icode=rest, state=NormalState checkState,
                          code=WordComparison{ arg1=raxAsArg, arg2=c, ccRef=ccRef } :: code,
                          context=context}
            end

        |   codeICode{icode=WordComparison{arg1=PReg testR1, arg2=PReg testR2, ccRef} :: rest,
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
                val checkState = {valueStack= #valueStack startState, ccState=CCState(ccRef, revCond)}
            in
                codeICode{icode=rest, state=NormalState checkState,
                          code=WordComparison{
                                arg1=raxAsArg,
                                arg2=MemoryLocation{offset=(stackPtr state - secondArg - 1) * wordSize, base=RealRegister(GenReg esp)},
                                ccRef=ccRef} :: code,
                          context=context}
            end
            
        |   codeICode{icode=WordComparison _ :: _, ...} =
                raise InternalError "codeICode: JumpOnWordComparison TODO"

        |   codeICode{icode=TestTagBit{arg, ccRef} :: rest, state=NormalState state, code, context} =
            let
                val argReg = argToPReg arg
                (* The value on the top of the stack should be in rax and the value we want to test. *)
                val startState = checkTopInRax(state, argReg)
                val checkState = {valueStack= #valueStack startState, ccState=CCState(ccRef, fn i=>i)}
            in
                codeICode{icode=rest, state=NormalState checkState,
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
                    {valueStack={preg=sHandle, loc=OnStack(sp+1) (* Two words *)} :: #valueStack pushedState, ccState=CCIndeterminate}
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
                val handlerStartState = {valueStack=popState(#valueStack state), ccState=CCIndeterminate}
                (* Reset to just above the handler. *)
                val (_, resetCode) = resetStack(state, handlerStartState, resReg)
                (* The result state is after the handler has been popped. *)
                val resultState = addRaxToState({valueStack=tl(#valueStack handlerStartState), ccState=CCIndeterminate}, resReg)
                val workReg = RealRegister(GenReg ebx) (* Must be different from the return register. *)
            in
                codeICode{icode=rest, state=NormalState resultState,
                          code=PopExceptionHandler{ handlerAddr=handlerAddr, resultReg=resultReg, workReg=workReg } :: resetCode @ code,
                          context=context}
            end

            (* MoveArgument - this is used for loads, stores and moves.  *)
            (* Create a new Preg containing the contents of another Preg.
               We only ever want the top item to be in RAX so we push the top of the stack. *)
        |   codeICode{icode=MoveArgument{source=PReg srcReg, dest=PReg destReg} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val load = loadSource(srcReg, eax, pushedState)
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=load @ pushCode @ code, context=context}
            end

        |   codeICode{icode=MoveArgument{source=RealRegister(GenReg srcReg), dest=PReg destReg} :: rest, state=NormalState state, code, context} =
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
                in
                    codeICode{
                        icode=rest,
                        state=NormalState({valueStack={preg=destReg, loc=OnStack sp} :: #valueStack pushedState, ccState= #ccState pushedState}),
                        code=PushValue{arg=RealRegister(GenReg srcReg)} :: (pushCode @ code), context=context}
                end
            end
            
            (* Move a constant to a preg.  First push anything in RAX then load the constant into RAX. *)
        |   codeICode{icode=MoveArgument{source as AddressConstant srcValue, dest=PReg destReg} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                (* The constant should be an address. *)
                val _ = isShort srcValue andalso raise InternalError "AddressConstant: short"
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=MoveArgument{source=source, dest=raxAsArg} :: pushCode @ code,
                    context=context}
            end

            (* Move a short constant to a preg.  First push anything in RAX then load the constant into RAX. *)
        |   codeICode{icode=MoveArgument{source as IntegerConstant _, dest=PReg destReg} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=MoveArgument{source=source, dest=raxAsArg} :: pushCode @ code,
                    context=context}
            end

            (* Load a value from a piece of memory, typically the offset in a tuple. *)
        |   codeICode{icode=MoveArgument{ source=MemoryLocation{offset, base}, dest=PReg destReg} :: rest, state=NormalState state, code, context} =
            let
                val bReg = argToPReg base
                (* Currently we always make a new preg for the base and that is
                   on the top of the state.  We can just remove it and replace it
                   with the result. *)
                val poppedState = checkTopInRax(state, bReg)
            in
                codeICode{icode=rest, state=NormalState(addRaxToState(poppedState, destReg)),
                    code=MoveArgument{ source=MemoryLocation{offset=offset, base=raxAsArg}, dest=raxAsArg} :: code, context=context}
            end

            (* Store a pseudo-register in a memory location. *)
        |   codeICode{icode=MoveArgument{ source=PReg srcReg, dest=MemoryLocation{offset, base}} :: rest, state=NormalState state, code, context} =
            let
                val bReg = argToPReg base
                (* We currently generate these so that the top of the state is the allocated
                   memory and the next item to store is immediately below it. *)
                val newState =
                    case #valueStack state of
                        {preg=tReg, loc=InRax} :: {preg=nReg, loc=OnStack _} :: state =>
                            if tReg = bReg andalso nReg = srcReg
                            then {valueStack=state, ccState= CCIndeterminate (* For the moment *)}
                            else raise InternalError "MoveArgument to MemoryLocation"
                    |   _ => raise InternalError "MoveArgument to MemoryLocation"
                (* Pop the value to store into ebx and store it in the tuple. *)
                val store = storeMemory(ebx, eax, offset) @ (ResetStackPtr{numWords=1} :: loadMemory(ebx, esp, 0))
            in
                codeICode{
                    (* We currently have the base in eax.  Leave it there because it may be used again. *)
                    icode=rest, state=NormalState(addRaxToState(newState, bReg)), code=store @ code, context=context}
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
            
        |   codeICode{icode=AllocateMemoryOperation{ size, isMutable, dest} :: rest, state=NormalState state, code, context} =
            let
                val destReg = argToPReg dest
                val (pushedState, pushCode) = pushRax state
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=AllocateMemoryOperation{ size=size, isMutable=isMutable, dest=raxAsArg} :: pushCode @ code, context=context}
            end

            (* Marks the end of setting the fields of a tuple.  This is used in the next
               level to avoid reordering a new allocation until a previously allocated
               section of memory has been fully initialised. *)
        |   codeICode{icode=(instr as InitialisationComplete _) :: rest, state, code, context} =
                codeICode{icode=rest, state=state, code=instr :: code, context=context}

            (* Function call. *)
        |   codeICode{icode=FunctionCall{ regArgs, stackArgs, dest, isTail=false} :: rest, state=NormalState state, code, context} =
            let
                val destReg = argToPReg dest
                val (pushedState, pushCode) = pushRax state
                (* We should find the stack arguments occupy the immediately preceding
                   entries on the stack.  If not we've got a problem. *)
                val numStackArgs = List.length stackArgs
                val currentSp = stackPtr pushedState

                fun checkStackArg ([], _) = ()
                |   checkStackArg (sa::args, num) =
                    let
                        val e = findEntry(argToPReg sa, pushedState)
                    in
                        case e of
                            InRax => raise InternalError "checkStackArg"
                        |   OnStack s => if s = num then () else raise InternalError "checkStackArg";
                        checkStackArg(args, num+1)
                    end
                val () = checkStackArg(stackArgs, currentSp-numStackArgs)
                
                (* Move the register arguments into the correct real registers. *)
                val doLoad = List.foldl (fn ((pr, gr), l) => l @ loadSource(argToPReg pr, gr, pushedState)) [] regArgs
                
                (* After the call the stack arguments will have been removed by the caller. *)
                (* We can just drop the stack args from the state.  We've checked them already.
                   Actually, we haven't checked that they were immediately preceding on this state list. *)
                val returnState = List.drop(#valueStack pushedState, numStackArgs)
                (* We haven't actually checked that the entries we're dropping really
                   are the register args. *)
                val poppedState = {valueStack=List.drop(returnState, List.length regArgs), ccState= CCIndeterminate}
                (* Add the result value in Rax to the state *)
                val (finalState, stackReset) =
                    resetStack(addRaxToState({valueStack=returnState, ccState= CCIndeterminate}, destReg), poppedState, destReg)
                val functionCode =
                    stackReset @ [FunctionCall{isTail=false, regArgs=[], stackArgs=[], dest=raxAsArg}] @ doLoad @ pushCode @ code
            in
                codeICode{icode=rest, state=NormalState finalState, code=functionCode, context=context}
            end

            (* Jump to a function.  The arguments have to be moved into the space currently used
               by the arguments to this function.  That could result in values being overwritten
               so we have to save them, especially the return address. *)
        |   codeICode{icode=FunctionCall{ regArgs, stackArgs, dest= _, isTail=true} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val currentSp = stackPtr pushedState

                (* Load the earlier arguments into registers.  Do that first because
                   they're lower down the stack and we could overwrite them if we wait. *)
                (* Load until we've done all the arguments or all the registers whichever is earlier.
                   This includes the closure register. *)
                val doLoad = List.foldl (fn ((pr, gr), l) => l @ loadSource(argToPReg pr, gr, pushedState)) [] regArgs
                
                (* Load the return address.  This could be overwritten by an argument. *)
                val loadReturn = loadStack(edi, currentSp * wordSize)

                (* Move the stack arguments into the space occupied by this function's args. *)
                val numArgsToMove = List.length stackArgs
                val argOffset = currentSp + currentStackArgs - numArgsToMove
                fun copyArgs (_, nil) = []
                |   copyArgs (n, r :: rl) =
                        (* N.B. This is reversed *)
                        copyArgs(n-1, rl) @ storeMemory(esi, esp, (argOffset+n)*wordSize) @ loadSource(argToPReg r, esi, pushedState)
                val moveArgs = copyArgs(numArgsToMove, stackArgs)
                val storeReturn = storeMemory(edi, esp, argOffset*wordSize)
                val functionCode =
                        FunctionCall{isTail=true, regArgs=[], stackArgs=[], dest=raxAsArg} ::
                                      ResetStackPtr {numWords=argOffset} :: (storeReturn @ moveArgs @ loadReturn @ doLoad @ pushCode @ code)
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
                val bodyState = {valueStack=newHead @ tailList, ccState= CCIndeterminate}
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
                    val load = loadSource(argToPReg source, eax, state)
                    val store =
                        case findEntry (argToPReg loopReg, state) of
                            OnStack s => storeMemory(eax, esp, (stackPtr state - s - 1) * wordSize)
                        |   InRax => raise InternalError "codeICode: JumpLoop"
                in
                    store @ load
                end
                val storeAll = List.foldl(fn (arg, l) => l @ moveArg arg) [] arguments
                val instrs =
                    JumpLoop{arguments=[], loopLabel=loopLabel} :: ResetStackPtr{numWords=stackPtr state - stackPtr startState} :: storeAll
            in
                codeICode{icode=rest, state=Exited, code=instrs @ code, context=context}
            end

        |   codeICode{icode=JumpLoop _ :: _, ...} =
                raise InternalError "codeICode: JumpLoop out of loop"

            (* Raise an exception using the value in the "packet" register as the exception packet.
               Since this exits we can ignore any further code and return the code and state. *)
        |   codeICode{icode=RaiseExceptionPacket{packet} :: rest, state=NormalState state, code, context} =
            let
                val pktReg = argToPReg packet
                val load = loadSource(pktReg, eax, state)
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
                             ccState=CCIndeterminate}),
                    code=MoveArgument{source=RealRegister(GenReg esp), dest=raxAsArg} :: (pushAll @ pushCode) @ code, context=context}
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
                val load = loadSource(argToPReg addr, eax, pushedState)
                val lockCode = LockMutable {addr=raxAsArg} :: (load @ pushCode)
            in
                codeICode{icode=rest, state=NormalState pushedState, code=lockCode @ code, context=context}
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

        |   codeICode{icode=ArithmeticFunction{ oper, resultReg, operand1=PReg oper1Reg, operand2 as IntegerConstant _ } :: rest,
                      state=NormalState state, code, context} =
            let
                val resReg = argToPReg resultReg
                val topState = checkTopInRax(state, oper1Reg)
                val resultState = addRaxToState(topState, resReg)
                val resCode = [ArithmeticFunction { oper=oper, resultReg=raxAsArg, operand1=raxAsArg, operand2=operand2 }]
            in
                codeICode{icode=rest, state=NormalState resultState, code=resCode @ code, context=context}
            end

        |   codeICode{icode=ArithmeticFunction _ :: _, ...} =
                raise InternalError "TODO: ArithmeticFunction"

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
