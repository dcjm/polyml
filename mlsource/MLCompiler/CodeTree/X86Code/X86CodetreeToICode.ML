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

functor X86CodetreeToICode(
    structure BACKENDTREE: BackendIntermediateCodeSig
    structure ICODE: ICodeSig
    structure DEBUG: DEBUGSIG
    structure X86FOREIGN: FOREIGNCALLSIG
    structure X86OLDCODEGEN : GENCODESIG
    sharing BACKENDTREE.Sharing = X86OLDCODEGEN.Sharing
    
): GENCODESIG =
struct
    open BACKENDTREE
    open Address
    open ICODE
    
    exception InternalError = Misc.InternalError

    val ioOp : int -> machineWord = RunCall.run_call1 RuntimeCalls.POLY_SYS_io_operation

    val argRegs = if isX64 then [eax, ebx, r8, r9, r10] else [eax, ebx]
    val numArgRegs = List.length argRegs

    (* tag a short constant *)
    fun tag c = 2 * c + 1

    fun codeFunctionToX86({body, localCount, name, argTypes, closure, ...}:bicLambdaForm, debugSwitches, closureOpt) =
    let
        val pregCounter = ref 0
        fun newPReg() = PReg(!pregCounter) before pregCounter := !pregCounter + 1
        val locToPregArray = Array.array(localCount, NONE: argument option)
        val labelCounter = ref 0
        fun newLabel() = ILabel(!labelCounter) before labelCounter := !labelCounter + 1

        val numFunctionArgs = List.length argTypes
        val argRegsUsed =
            if numFunctionArgs >= List.length argRegs
            then argRegs
            else List.take(argRegs, numFunctionArgs)

        (* Pseudo-regs for the result, the closure and the args that were passed in real regs. *)
        val resultTarget = newPReg()
        val closureRegAddr = newPReg()
        val argPRegs = map (fn _ => newPReg()) argRegsUsed

        val pushClosureReg =
            case closure of [] => [] | _ => [MoveArgument{source=RealRegister(GenReg edx), dest=closureRegAddr}]
        val pushArgs =
            ListPair.foldrEq (fn (r, m, l) => MoveArgument{source=RealRegister(GenReg r), dest=m} :: l) [] (argRegsUsed, argPRegs)
        (* Number of arguments on the stack and not in registers. *)
        val currentStackArgs = Int.max(0, numFunctionArgs - List.length argRegs)
        
        (* The return instruction.  This can be added on to various tails but there is always
           one at the end anyway. *)
        val returnInstruction =
            [ReturnResultFromFunction{resultReg=resultTarget, numStackArgs=currentStackArgs}]

        (* First pass - turn the ML code into ICode.  All targets are pseudo-registers. *)
        local
            fun codeToICode(BICNewenv (bindings, exp), context, isTail, target) =
                codeBindingsToICode(bindings, context) @ codeToICode(exp, context, isTail, target)
                
            |   codeToICode(BICConstnt(value, _), _, _, target) =
                let
                    val arg =
                        if isShort value
                        then IntegerConstant(tag(Word.toLargeIntX(toShort value)))
                        else AddressConstant value
                in
                    [MoveArgument{source=arg, dest=target}]
                end

            |   codeToICode(BICExtract(BICLoadLocal l), _, _, target) =
                    [MoveArgument{source=valOf(Array.sub(locToPregArray, l)), dest=target}]

            |   codeToICode(BICExtract(BICLoadArgument a), _, _, target) =
                    if a < numArgRegs
                    then (* It was originally in a register.  It's now in a preg. *)
                        [MoveArgument{source=List.nth(argPRegs, a), dest=target}]
                    else (* Pushed before call. *)
                        [MoveStackArgToPreg{offset=List.length argTypes - a, dest=target}]
            
            |   codeToICode(BICExtract(BICLoadClosure c), _, _, target) =
                let
                    val () = if c >= List.length closure then raise InternalError "BICExtract: closure" else ()
                    val baseReg = newPReg()
                    (* N.B.  We need to add one to the closure entry because zero is the code address. *)
                in
                    (* We could just use the closureRegAddr as the base but the code-generation
                       for MoveArgument with MemoryLocation as the source assumes that the base is on the top of the stack. *)
                    [
                        MoveArgument {source=closureRegAddr, dest=baseReg},
                        MoveArgument {source=MemoryLocation{offset=(c+1)*wordSize, base=baseReg}, dest=target}
                    ]
                end

            |   codeToICode(BICExtract BICLoadRecursive, _, _, target) =
                (
                    (* If the closure is empty we must use the constant.  We can't guarantee that
                       the caller will actually load the closure register if it knows the closure
                       is empty. *)
                    case closure of
                        [] => [MoveArgument{source=AddressConstant(toMachineWord(valOf closureOpt)), dest=target}]
                    |   _ => [MoveArgument {source=closureRegAddr, dest=target}]
                )
            
            |   codeToICode(BICField{base, offset}, context, _, target) =
                let
                    val baseR = newPReg()
                in
                    codeToICode(base, context, false, baseR) @
                        [MoveArgument{source=MemoryLocation{offset=offset*wordSize, base=baseR}, dest=target}]
                end
            
            |   codeToICode(BICEval {function, argList, ...}, context, isTail, target) =
                let
                    val numArgs = List.length argList
                    (* Create pregs for the closure and each argument. *)
                    val clPReg = newPReg()
                    val argPRegs = List.map (fn _ => newPReg()) argList
                    val codeArgs =
                        ListPair.foldlEq(fn (arg, pr, l) => l @ codeToICode(arg, context, false, pr)) []
                            (function :: List.map #1 argList, clPReg :: argPRegs)
                    (* Load the closure and the argument registers.  This returns the
                       shorter of the actual arguments and those in registers. *)
                    val regArgs = ListPair.zip (clPReg::argPRegs, edx :: argRegs)
                    (* The stack arguments are the later arguments in the list i.e. those
                       most recently pushed to the stack. *)
                    val stackArgs =
                        if numArgs > List.length argRegs
                        then List.drop(argPRegs, List.length argRegs)
                        else []
                in
                    codeArgs @ [FunctionCall{regArgs=regArgs, stackArgs=stackArgs, dest=target, isTail=isTail}]
                end

            |   codeToICode(BICBuiltIn(rtsNo, args), context, isTail, target) =
                    (* For the moment just call the RTS function. *)
                    codeToICode(
                        BICEval{function=BICConstnt(ioOp rtsNo, []),
                                argList = map (fn c => (c, GeneralType)) args, resultType=GeneralType}, context, isTail, target)

            |   codeToICode(BICLambda(lambda as { closure = [], ...}), _, _, target) =
                (* Empty closure - create a constant closure for any recursive calls. *)
                let
                    val closure = Address.alloc(0w1, Word8.orb (F_mutable, F_words), Address.toMachineWord 0w0)
                    val codeAddr = codeFunctionToX86(lambda, debugSwitches, SOME closure)
                    open Address
                in
                    assignWord(closure, 0w0, toMachineWord codeAddr);
                    lock closure;
                    [MoveArgument{source=AddressConstant(toMachineWord closure), dest=target}]
                end

            |   codeToICode(BICLambda(lambda as { closure, ...}), context, isTail, target) =
                (* Non-empty closure.  Ignore stack closure option at the moment. *)
                let
                    val codeAddr = codeFunctionToX86(lambda, debugSwitches, NONE)
                in
                    (* Treat it as a tuple with the code as the first field. *)
                    codeToICode(BICTuple(BICConstnt(toMachineWord codeAddr, []) :: map BICExtract closure), context, isTail, target)
                end

            |   codeToICode(BICCond(test, thenPt, elsePt), context, isTail, target) =
                let
                    val startElse = newLabel() and skipElse = newLabel()
                    val testCode = codeCondition(test, context, false, startElse)
                    (* Put the result in the target register. *)
                    val thenCode = codeToICode(thenPt, context, isTail, target)
                    val elseCode = codeToICode(elsePt, context, isTail, target)
                in
                    testCode @
                        thenCode @ [UnconditionalForwardJump{label=skipElse}, ForwardJumpLabel{label=startElse, result=NONE}] @
                        elseCode @ [ForwardJumpLabel{label=skipElse, result=SOME target}]
                end
            
            |   codeToICode(BICCase { cases, test, caseType, default}, context, isTail, target) =
                let
                    (* Sort the cases into ascending order.  It's possible that we may have
                       duplicates if this came from an if-then-else construction so we
                       need to retain the ordering for items with the same case label. *)
                    (* TODO: This should be done in the higher level. *)
                    local
                        val labelCount = List.length cases
                        (* Add an extra field before sorting which retains the ordering for
                           equal labels. *)
                        val ordered = ListPair.zipEq (cases, List.tabulate(labelCount, fn n=>n))
                        fun leq ((_, w1: word), n1: int) ((_, w2), n2) =
                            if w1 = w2 then n1 <= n2 else w1 < w2
                        val sorted = List.map #1 (Misc.quickSort leq ordered)
                        (* Filter out any duplicates. *)
                        fun filter [] = []
                        |   filter [p] = [p]
                        |   filter ((p as (_, lab1)) :: (q as (_, lab2)) :: tl) =
                                if lab1 = lab2
                                then p :: filter tl
                                else p :: filter (q :: tl)
                    in
                        val cases = filter sorted
                    end

                    val (isExhaustive, min, max) =
                        case caseType of
                            CaseTag max => (true, 0w0, max)
                        |   _ =>
                            let
                                val (_, aLabel) = hd cases
                                fun foldCases((_, w), (min, max)) = (Word.min(w, min), Word.max(w, max))
                                val (min, max) = List.foldl foldCases (aLabel, aLabel) cases
                            in
                                (false, min, max)
                            end

                    (* Create labels for each of the cases.  Fill in any gaps with entries that
                       will point to the default. *)
                    fun extendCase(indexVal, cl as ((c, caseValue) :: cps)) =
                        if indexVal = caseValue
                        then (newLabel(), SOME c, caseValue) :: extendCase(indexVal+0w1, cps)
                        else (newLabel(), NONE, indexVal) :: extendCase(indexVal+0w1, cl)

                    |   extendCase(indexVal, []) =
                        (* We may not be at the end if this came from a CaseTag *)
                            if indexVal > max
                            then []
                            else (newLabel(), NONE, indexVal) :: extendCase(indexVal+0w1, [])
                    
                    val fullCaseRange = extendCase(min, cases)

                    val testReg = newPReg() and workReg = newPReg()
                    val testCode = codeToICode(test, context, false, testReg)
                    (* Put in the default case.  Even when the case is exhaustive one entry is
                       always treated as a default and not included in the list of cases. *)
                    val defaultLabel = newLabel()

                    (* Generate the code for each of the cases and the default.  We need to put an
                       unconditional branch after each to skip the other cases.  *)
                    fun codeCases ((startLabel, SOME c, _) :: otherCases) =
                        let
                            (* Put in the case with a jump to the end of the sequence. *)
                            val exitLabel = newLabel()
                            val codeThis =
                                ForwardJumpLabel{label=startLabel, result=NONE} ::
                                    codeToICode(c, context, isTail, target) @ [UnconditionalForwardJump{label=exitLabel}]
                            val codeRest = codeCases otherCases
                        in
                            codeThis @ codeRest @ [ForwardJumpLabel{label=exitLabel, result=SOME target}]
                        end

                    |   codeCases((_, NONE, _) :: otherCases) = codeCases otherCases
                            
                    |   codeCases [] =
                        let
                            (* We need to add labels for all the gaps we filled and also for a "default" label for
                               the indexed-case instruction itself. *)
                            fun addDefault ((startLabel, NONE, _), l) = ForwardJumpLabel{label=startLabel, result=NONE} :: l
                            |   addDefault ((_, SOME _, _), l) = l
                            val defLabels =
                                List.foldl addDefault
                                    [ForwardJumpLabel{label=defaultLabel, result=NONE}] fullCaseRange
                        in
                            (* Put in the default.  Because this is the last we don't need to
                               jump round it. *)
                             defLabels @ codeToICode(default, context, isTail, target)
                        end
                        
                    val codedCases = codeCases fullCaseRange

                    val codeLabels = map (fn (lab, _, value) => (lab, value)) fullCaseRange
                in
                    testCode @ [IndexedCaseOperation{testReg=testReg, workReg=workReg, default=defaultLabel,
                                            cases=codeLabels, exhaustive=isExhaustive, min=min, max=max,
                                            arbitrary=caseType = CaseInt}] @ codedCases
                end
            
            |   codeToICode(BICBeginLoop {loop, arguments}, context, isTail, target) =
                let
                    (* First evaluate the initial values for the arguments to new pregs.
                       These are only used for the initial values. *)
                    val argPRegs = map(fn _ => newPReg()) arguments
                    val codeArgs =
                        ListPair.foldlEq(fn (({value, ...}, _), pr, l) => l @ codeToICode(value, context, false, pr)) []
                            (arguments, argPRegs)
                    (* Create loop arguments.  These are associated with the loop variables. *)
                    fun makeLoopReg ({addr, ...}, _) =
                        let val pr = newPReg() val () = Array.update(locToPregArray, addr, SOME pr) in pr end
                    val loopRegs = map makeLoopReg arguments
                    val loopLabel = newLabel()
                    val loopBody = codeToICode(loop, {loopArgs=SOME (loopRegs, loopLabel) }, isTail, target)
                    val args = ListPair.mapEq(fn (s, l) => {source=s, loopReg=l}) (argPRegs, loopRegs)
                in
                    codeArgs @ [StartLoop{arguments=args, loopLabel=loopLabel}] @ loopBody @ [EndLoop {loopLabel=loopLabel}]
                end
            
            |   codeToICode(BICLoop args, context as {loopArgs=SOME (loopRegs, loopLabel), ...}, _, _) =
                let
                    (* Registers to receive the evaluated arguments.  We can't put the
                       values into the loop variables yet because the values could depend
                       on the current values of the loop variables. *)
                    val argPRegs = map(fn _ => newPReg()) args
                    val codeArgs =
                        ListPair.foldlEq(fn ((arg, _), pr, l) => l @ codeToICode(arg, context, false, pr)) []
                            (args, argPRegs)
                    val jumpArgs = ListPair.mapEq(fn (s, l) => {source=s, loopReg=l}) (argPRegs, loopRegs)
                in
                    codeArgs @ [JumpLoop{arguments=jumpArgs, loopLabel=loopLabel}]
                end

            |   codeToICode(BICLoop _, {loopArgs=NONE, ...}, _, _) =
                    raise InternalError "BICLoop without BICBeginLoop"

            |   codeToICode(BICRaise exc, context, _, _) =
                let
                    val excReg = newPReg()
                in
                    codeToICode(exc, context, false, excReg) @ [RaiseExceptionPacket{packet=excReg}]
                end
            
            |   codeToICode(BICLdexc, _, _, target) = (* The argument is in eax. *)
                    [MoveArgument{source=RealRegister(GenReg eax), dest=target}]
            
            |   codeToICode(BICHandle {exp, handler}, context, isTail, target) =
                let
                    (* The expression cannot be treated as a tail because the handler has
                       to be removed after *)
                    val expCode = codeToICode(exp, context, false (* Not tail *), target)
                    val handleCode = codeToICode(handler, context, isTail, target)
                    val saveHandle = newPReg()
                    val handlerLab = newLabel()
                    val skipHandler = newLabel()
                in
                    [PushExceptionHandler{handlerAddr=saveHandle, handleStart=handlerLab}] @ expCode @
                    [PopExceptionHandler{handlerAddr=saveHandle, resultReg=target}, UnconditionalForwardJump{label=skipHandler},
                     BeginHandler{handleStart=handlerLab}] @ handleCode @ [ForwardJumpLabel{label=skipHandler, result=SOME target}]
                end
            
            |   codeToICode(BICTuple fields, context, _, target) =
                let
                    fun loadFields([], n) =
                            [AllocateMemoryToPreg{size=n, isMutable=false, dest=target}]
                    |   loadFields(f :: rest, n) =
                        let
                            val fReg = newPReg()
                            val code = codeToICode(f, context, false, fReg)
                            val restAndAlloc = loadFields(rest, n+1)
                            val storeValue = [MoveArgument{ source=fReg, dest=MemoryLocation{offset=n*wordSize, base=target} }]
                        in
                            code @ restAndAlloc @ storeValue
                        end
                in
                    loadFields(fields, 0) @ [InitialisationComplete{dest=target}]
                end
            
            |   codeToICode(BICContainer size , _, _, target) = [ReserveContainer{size=size, address=target}]

                (* Copy the source tuple into the container.
                   TODO: If the tuple is BICTuple we should copy the individual fields
                   and never create the heap tuple. *)
            |   codeToICode(BICSetContainer{container, tuple, filter}, context, _, target) =
                let
                    val containerTarget = newPReg() and tupleTarget = newPReg()
                    val codeContainer = codeToICode(container, context, false, containerTarget)
                    and codeTuple = codeToICode(tuple, context, false, tupleTarget)
                    
                    val filterLength = BoolVector.length filter

                    fun copyContainer(sourceWord, destWord) =
                        if sourceWord = filterLength
                        then []
                        else if BoolVector.sub(filter, sourceWord)
                        then
                        let
                            (* This is a little more complicated than we really need because the code for
                               MoveArgument from MemoryLocation and to MemoryLocation assume the arguments are in
                               specific locations on the stack. *)
                            val srcCopy = newPReg() and loadReg = newPReg() and destCopy = newPReg()
                            val srcCode = MoveArgument{source=tupleTarget, dest=srcCopy}
                            val load = MoveArgument{source=MemoryLocation{offset=sourceWord*wordSize, base=srcCopy}, dest=loadReg}
                            val destCode = MoveArgument{source=containerTarget, dest=destCopy}
                            val storeCode = MoveArgument{source=loadReg, dest=MemoryLocation{offset=destWord*wordSize, base=destCopy}}
                        in
                            srcCode :: load :: destCode :: storeCode :: copyContainer(sourceWord+1, destWord+1)
                        end
                        else copyContainer(sourceWord+1, destWord)
                in
                    (* Add a dummy result in here to keep everything neat. *)
                    codeContainer @ codeTuple @ copyContainer(0, 0) @ [MoveArgument{source=IntegerConstant(tag 0), dest=target}]
                end

            |   codeToICode(BICTagTest{test, tag, ...}, context, isTail, target) =
                let
                    (* Convert this into a simple equality function. *)
                    val code =
                        BICEval {
                            function = BICConstnt(ioOp RuntimeCalls.POLY_SYS_word_eq, []),
                            argList=[(test, GeneralType), (BICConstnt(Address.toMachineWord tag, []), GeneralType)],
                            resultType=GeneralType }
                in
                    codeToICode (code, context, isTail, target)
                end

            (* Process a list of bindings. *)
            and codeBindingsToICode(bindings, context) =
            let
                fun doBinding(BICDeclar{value, addr, ...}, l) =
                    let
                        val dest = newPReg()
                        val code = codeToICode(value, context, false, dest)
                        val () = Array.update(locToPregArray, addr, SOME dest)
                    in
                        l @ code
                    end

                |   doBinding(BICRecDecs [{lambda, addr, ...}], l) =
                    (* We shouldn't have single entries in RecDecs but it seems to occur at the moment. *)
                    let
                        val dest = newPReg()
                        val code = codeToICode(BICLambda lambda, context, false, dest)
                        val () = Array.update(locToPregArray, addr, SOME dest)
                    in
                        l @ code
                    end

                |   doBinding(BICRecDecs decs, l) =
                    let
                        val destRegs = map (fn _ => newPReg()) decs

                        (* First build the closures as mutable cells containing zeros.  Set the
                           entry in the address table to the register containing the address. *)
                        fun makeClosure({lambda={closure, ...}, addr, ...}, dest, c) =
                        let
                            val () = Array.update(locToPregArray, addr, SOME dest)
                            val sizeClosure = List.length closure + 1
                            fun clear n =
                                if n = sizeClosure
                                then [AllocateMemoryToPreg{size=sizeClosure, isMutable=true, dest=dest}]
                                else
                                let
                                    val d = newPReg()
                                in
                                    MoveArgument{source=IntegerConstant(tag 0), dest=d} ::
                                        (clear (n+1) @
                                            [MoveArgument{source=d, dest=MemoryLocation{offset=n*wordSize, base=dest}}])
                                end
                        in
                            c @ clear 0 @ [InitialisationComplete{dest=dest}]
                        end
                        
                        val allocClosures = ListPair.foldlEq makeClosure [] (decs, destRegs)
                        
                        fun setClosure({lambda as {closure, ...}, ...}, dest, l) =
                        let
                            val codeAddr = codeFunctionToX86(lambda, debugSwitches, NONE)
                            (* Basically the same as tuple except we load the address of
                               the closure we've made.  It's complicated because
                               MoveArgument to MemoryLocation assumes that the top of the stack is
                               the address of the allocated memory and the items below
                               are the values to store. *)
                            val dstCopy = newPReg()
                            fun loadFields([], _) = [MoveArgument{source=dest, dest=dstCopy}]
                            |   loadFields(f :: rest, n) =
                                let
                                    val fReg = newPReg()
                                    val code = codeToICode(f, context, false, fReg)
                                    val restAndAlloc = loadFields(rest, n+1)
                                    val storeValue = [MoveArgument{ source=fReg, dest=MemoryLocation{offset=n*wordSize, base=dstCopy} }]
                                in
                                    code @ restAndAlloc @ storeValue
                                end
                            val setFields = loadFields(BICConstnt(toMachineWord codeAddr, []) :: map BICExtract closure, 0)
                        in
                            l @ setFields @ [LockMutable{addr=dest}]
                        end
                        val setClosures = ListPair.foldlEq setClosure [] (decs, destRegs)
                    in
                        l @ allocClosures @ setClosures
                    end

                    (* This should be removed in due course.  The level above indicates the
                       last reference to a binding by adding these references. *)
                |   doBinding(BICNullBinding (BICExtract _), l) = l

                |   doBinding(BICNullBinding exp, l) =
                    let
                        val dest = newPReg()
                    in
                        l @ codeToICode(exp, context, false, dest) (* And discard result. *)
                    end
                in
                    List.foldl doBinding [] bindings
                end

                (* Code a branch condition.  This is really meant for andalso/orelse. *)
                and codeCondition(BICConstnt(value, _), _, jumpOn, jumpLabel) =
                    (* Constant - typically part of andalso/orelse.  Either an unconditional branch
                       or an unconditional drop-through. *)
                    if jumpOn = (toShort value <> 0w0)
                    then [UnconditionalForwardJump{label=jumpLabel}]
                    else []

                |   codeCondition(BICNewenv(decs, exp), context, jumpOn, jumpLabel) =
                        codeBindingsToICode(decs, context) @ codeCondition(exp, context, jumpOn, jumpLabel)

                |   codeCondition(BICTagTest{test, tag=tagValue, ...}, context, jumpOn, jumpLabel) =
                    let
                        val tagReg = newPReg()
                        val testCode = codeToICode(test, context, false, tagReg)
                    in
                        testCode @
                            [JumpOnWordComparison{arg1=tagReg, arg2=IntegerConstant(tag(Word.toLargeInt tagValue)),
                                   condition=if jumpOn then JE else JNE, label=jumpLabel}]
                    end

                |   codeCondition(BICCond (testPart, thenPart, elsePart), context, jumpOn, jumpLabel) =
                    let
                        val notTest = newLabel() and isThen = newLabel()
                        and notThen = newLabel() and notElse = newLabel()
                        (* Test the condition and jump to the else-part if this is false. *)
                        val testTest = codeCondition(testPart, context, false, notTest)
                        (* Test the then-part and jump if the condition we want holds.
                           We don't go to the final label yet. *)
                        val testThen = codeCondition(thenPart, context, jumpOn, isThen)
                        (* Test the else-part and jump on the inverse of the condition.
                           The destination of this jump is going to be the drop-through
                           case. *)
                        val testElse = codeCondition(elsePart, context, not jumpOn, notElse)
                    in
                        (* Now put this together. *)
                        testTest @ testThen @
                            (* Branch round the else-part and put in a label for the start of the else *)
                            [UnconditionalForwardJump{label=notThen}, ForwardJumpLabel {label=notTest, result=NONE}] @
                            testElse @
                            (* Add a label for the result of the then-part.  Because we branched
                               on the inverse of the test in the else-part we now have both the
                               conditions to take the branch.  Put in an unconditional branch
                               to the final label. *)
                            [ForwardJumpLabel {label=isThen, result=NONE}, UnconditionalForwardJump{label=jumpLabel},
                             (* And now the labels for the condition where we don't want to branch and want to
                                drop through. *)
                             ForwardJumpLabel {label=notElse, result=NONE}, ForwardJumpLabel{label=notThen, result=NONE}]
                    end

                    (* General case.  Load the value into a register and compare it with 1 (true) *)
                |   codeCondition(condition, context, jumpOn, jumpLabel) =
                    let
                        val testReg = newPReg()
                        val testCode = codeToICode(condition, context, false, testReg)
                    in
                        testCode @
                            [JumpOnWordComparison{arg1=testReg, arg2=IntegerConstant(tag 1),
                                   condition=if jumpOn then JE else JNE, label=jumpLabel}]
                    end
        in
            val icode = pushClosureReg @ pushArgs @
                codeToICode(body, {loopArgs=NONE}, true, resultTarget) @ returnInstruction
        end

    in
        codeICodeFunctionToX86{icode = icode, functionName = name, maxLabels = !labelCounter, maxPRegs = ! pregCounter,
            argRegsUsed = argRegsUsed, hasFullClosure = not (null closure), currentStackArgs = currentStackArgs,
            debugSwitches = debugSwitches}
    end

    fun gencodeLambda(lambda, debugSwitches, closure) =
    let
        open DEBUG Universal
        (*val debugSwitches =
            [tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
            tagInject assemblyCodeTag true] @ debugSwitches*)
        val codeAddr = codeFunctionToX86(lambda, debugSwitches, SOME closure)
        open Address
    in
        assignWord(closure, 0w0, toMachineWord codeAddr);
        lock closure
    end
    
    val gencodeLambda = X86OLDCODEGEN.gencodeLambda
    
    structure Foreign = X86FOREIGN
    
    structure Sharing =
    struct
        type backendIC = backendIC
        and  bicLoadForm = bicLoadForm
        and argumentType = argumentType
    end
    
end;
