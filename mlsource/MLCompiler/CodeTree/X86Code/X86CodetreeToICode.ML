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
    structure ICODETRANSFORM: X86ICODETRANSFORMSIG
    structure X86OLDCODEGEN : GENCODESIG
    sharing BACKENDTREE.Sharing = X86OLDCODEGEN.Sharing = ICODE.Sharing = ICODETRANSFORM.Sharing
    
): GENCODESIG =
struct
    open BACKENDTREE
    open Address
    open ICODE
    
    exception InternalError = Misc.InternalError

    val argRegs = if isX64 then [eax, ebx, r8, r9, r10] else [eax, ebx]
    val numArgRegs = List.length argRegs

    (* tag a short constant *)
    fun tag c = 2 * c + 1
  
    (* shift a short constant, but don't set tag bit *)
    fun semitag c = 2 * c;

    fun codeFunctionToX86({body, localCount, name, argTypes, closure, ...}:bicLambdaForm, debugSwitches, closureOpt) =
    let
        val pregCounter = ref 0
        fun newPReg() = PReg(!pregCounter) before pregCounter := !pregCounter + 1
        val locToPregArray = Array.array(localCount, NONE: argument option)
        val labelCounter = ref 0
        fun newLabel() = ILabel(!labelCounter) before labelCounter := !labelCounter + 1
        val ccRefCounter = ref 0
        fun newCCRef() = CcRef(!ccRefCounter) before ccRefCounter := !ccRefCounter + 1

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

        fun constantAsArgument value =
            if isShort value
            then IntegerConstant(tag(Word.toLargeIntX(toShort value)))
            else AddressConstant value

        (* Create the branch condition from the test, isSigned and jumpOn values.
           (In)equality tests are the same for signed and unsigned values. *)
        local
            open BuiltIns
        in
            fun testAsBranch(TestEqual,         _,      true)       = JE
            |   testAsBranch(TestEqual,         _,      false)      = JNE
            |   testAsBranch(TestNotEqual,      _,      true)       = JNE
            |   testAsBranch(TestNotEqual,      _,      false)      = JE
                (* Signed tests *)
            |   testAsBranch(TestLess,          true,   true)       = JL
            |   testAsBranch(TestLess,          true,   false)      = JGE
            |   testAsBranch(TestLessEqual,     true,   true)       = JLE
            |   testAsBranch(TestLessEqual,     true,   false)      = JG
            |   testAsBranch(TestGreater,       true,   true)       = JG
            |   testAsBranch(TestGreater,       true,   false)      = JLE
            |   testAsBranch(TestGreaterEqual,  true,   true)       = JGE
            |   testAsBranch(TestGreaterEqual,  true,   false)      = JL
                (* Unsigned tests *)
            |   testAsBranch(TestLess,          false,  true)       = JB
            |   testAsBranch(TestLess,          false,  false)      = JNB
            |   testAsBranch(TestLessEqual,     false,  true)       = JNA
            |   testAsBranch(TestLessEqual,     false,  false)      = JA
            |   testAsBranch(TestGreater,       false,  true)       = JA
            |   testAsBranch(TestGreater,       false,  false)      = JNA
            |   testAsBranch(TestGreaterEqual,  false,  true)       = JNB
            |   testAsBranch(TestGreaterEqual,  false,  false)      = JB
        end
        
        fun checkOverflow (ccRef, target) =
        let
            val label = newLabel() and excReg = newPReg()
        in
            [
                ConditionalForwardJump{ ccRef=ccRef, condition=JNO, label=label },
                MoveArgument{source=AddressConstant(toMachineWord(Overflow)), dest=excReg},
                RaiseExceptionPacket{packet=excReg},
                ForwardJumpLabel{ label=label, result=SOME target }
            ]
        end

        (* Main function to turn the codetree into ICode.  Optimisation is generally
           left to later passes.  This does detect tail recursion.  *)
        fun codeToICode(BICNewenv (bindings, exp), context, isTail, target) =
            codeBindingsToICode(bindings, context) @ codeToICode(exp, context, isTail, target)
            
        |   codeToICode(BICConstnt(value, _), _, _, target) =
                [MoveArgument{source=constantAsArgument value, dest=target}]

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

        |   codeToICode(BICBuiltIn0 _, _, _, _) =
                raise InternalError "TODO: BICBuiltIn0"

        |   codeToICode(BICBuiltIn1{oper=BuiltIns.NotBoolean, arg1}, context, _, target) =
            let
                val tReg = newPReg() and ccRef = newCCRef()
            in
                codeToICode(arg1, context, false, tReg) @
                    [ArithmeticFunction{oper=XOR, resultReg=target, operand1=tReg, operand2=IntegerConstant(semitag 1), ccRef=ccRef}]
            end

        |   codeToICode(instr as BICBuiltIn1{oper=BuiltIns.IsTaggedValue, ...}, context, isTail, target) =
                codeAsConditional(instr, context, isTail, target)

        |   codeToICode(instr as BICBuiltIn2{oper=BuiltIns.WordComparison _, ...}, context, isTail, target) =
                codeAsConditional(instr, context, isTail, target)

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithAdd, arg1, arg2=BICConstnt(value, _)}, context, _, target) =
            let
                val aReg1 = newPReg() and ccRef = newCCRef()
                (* If the argument is a constant we can subtract the tag beforehand.
                   This should always be a tagged value if the type is correct.  However it's possible for it not to
                   be if we have an arbitrary precision value.  There will be a run-time check that the value is
                   short and so this code will never be executed.  It will generally be edited out by the higher
                   level be we can't rely on that.  Because it's never executed we can just put in zero. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
            in
                codeToICode(arg1, context, false, aReg1) @
                    [ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg1, operand2=IntegerConstant constVal, ccRef=ccRef}] @
                    checkOverflow (ccRef, target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithAdd, arg1=BICConstnt(value, _), arg2}, context, _, target) =
            let
                val aReg2 = newPReg() and ccRef = newCCRef()
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
            in
                codeToICode(arg2, context, false, aReg2) @
                    [ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg2, operand2=IntegerConstant constVal, ccRef=ccRef}] @
                    checkOverflow (ccRef, target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithAdd, arg1, arg2}, context, _, target) =
            let
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newPReg() and ccRef = newCCRef()
            in
                codeToICode(arg1, context, false, aReg1) @ codeToICode(arg2, context, false, aReg2) @
                    (* Subtract the tag bit from the second argument, do the addition and check for overflow. *)
                    (* TODO: We should really do the detagging in the transform phase.  It can make a better choice of
                       the argument if one of the arguments is already untagged or if we have a constant argument. *)
                    [ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=aReg2, operand2=IntegerConstant 1, ccRef=newCCRef()},
                     ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg1, operand2=aReg3, ccRef=ccRef}] @
                    checkOverflow (ccRef, target)
            end

            (* Subtraction.  We can handle the special case of the second argument being a constant but not the first. *)
        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithSub, arg1, arg2=BICConstnt(value, _)}, context, _, target) =
            let
                val aReg1 = newPReg() and ccRef = newCCRef()
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
            in
                codeToICode(arg1, context, false, aReg1) @
                    [ArithmeticFunction{oper=SUB, resultReg=target, operand1=aReg1, operand2=IntegerConstant constVal, ccRef=ccRef}] @
                    checkOverflow (ccRef, target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithSub, arg1, arg2}, context, _, target) =
            let
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newPReg()
                val ccRef = newCCRef()
            in
                codeToICode(arg1, context, false, aReg1) @ codeToICode(arg2, context, false, aReg2) @
                    (* Do the subtraction, test for overflow and afterwards add in the tag bit. *)
                    [ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=aReg1, operand2=aReg2, ccRef=ccRef}] @
                    checkOverflow (ccRef, target) @
                    [ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg3, operand2=IntegerConstant 1, ccRef=newCCRef()}]
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.FixedPrecisionArith _, ...}, _, _, _) =
                raise InternalError "TODO: BICBuiltIn2: FixedPrecisionArith"

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1, arg2=BICConstnt(value, _)}, context, _, target) =
            let
                val aReg1 = newPReg()
                (* If the argument is a constant we can subtract the tag beforehand.  We shouldn't ever have
                   a long value here but because it might happen we check. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
            in
                codeToICode(arg1, context, false, aReg1) @
                    [ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg1, operand2=IntegerConstant constVal, ccRef = newCCRef()}]
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1=BICConstnt(value, _), arg2}, context, _, target) =
            let
                val aReg2 = newPReg()
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
            in
                codeToICode(arg2, context, false, aReg2) @
                    [ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg2, operand2=IntegerConstant constVal, ccRef = newCCRef()}]
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1, arg2}, context, _, target) =
            let
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newPReg()
            in
                codeToICode(arg1, context, false, aReg1) @ codeToICode(arg2, context, false, aReg2) @
                    (* Subtract the tag bit from the second argument and do the addition.  No need for overflow check. *)
                    [ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=aReg2, operand2=IntegerConstant 1, ccRef=newCCRef()},
                     ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg1, operand2=aReg3, ccRef=newCCRef()}]
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordArith BuiltIns.ArithSub, arg1, arg2=BICConstnt(value, _)}, context, _, target) =
            let
                val aReg1 = newPReg()
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
            in
                codeToICode(arg1, context, false, aReg1) @
                    [ArithmeticFunction{oper=SUB, resultReg=target, operand1=aReg1, operand2=IntegerConstant constVal, ccRef=newCCRef()}]
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordArith BuiltIns.ArithSub, arg1, arg2}, context, _, target) =
            let
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newPReg()
            in
                codeToICode(arg1, context, false, aReg1) @ codeToICode(arg2, context, false, aReg2) @
                    (* Do the subtraction and add in the tag bit.  This could be reordered if we have cascaded operations
                       since we don't need to check for overflow. *)
                    [ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=aReg1, operand2=aReg2, ccRef=newCCRef()},
                     ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg3, operand2=IntegerConstant 1, ccRef=newCCRef()}]
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordArith _, ...}, _, _, _) =
                raise InternalError "TODO: BICBuiltIn2: WordArith"

        |   codeToICode(BICBuiltIn3 _, _, _, _) =
                raise InternalError "TODO: BICBuiltIn3"

        |   codeToICode(BICBuiltIn4 _, _, _, _) =
                raise InternalError "TODO: BICBuiltIn4"

        |   codeToICode(BICBuiltIn5 _, _, _, _) =
                raise InternalError "TODO: BICBuiltIn5"

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
                
                (* Unless this is exhaustive we need to add some range checks.  These all
                   jump to the default case. *)
                val (rangeChecks, extraDefaults) =
                    if isExhaustive
                    then ([], [])
                    else
                    let
                        val (testTag, tagDefault) =
                            let
                                val defLab = newLabel() and tReg = newPReg() and ccRef = newCCRef()
                            in
                                ([
                                    MoveArgument {source=testReg, dest=tReg},
                                    TestTagBit{arg=tReg, ccRef=ccRef},
                                    (* Jump if the value is long. *)
                                    ConditionalForwardJump{ccRef=ccRef, condition=JE, label=defLab}
                                 ], [defLab])
                            end
                        val defLab1 = newLabel() and defLab2 = newLabel()
                        val tReg1 = newPReg() and tReg2 = newPReg()
                        val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                        val testCode =
                            [
                                MoveArgument {source=testReg, dest=tReg1},
                                WordComparison{arg1=tReg1, arg2=IntegerConstant(tag(Word.toLargeInt max)), ccRef=ccRef1},
                                ConditionalForwardJump{ccRef=ccRef1, condition=JA, label=defLab1},
                                MoveArgument {source=testReg, dest=tReg2},
                                WordComparison{arg1=tReg1, arg2=IntegerConstant(tag(Word.toLargeInt min)), ccRef=ccRef2},
                                ConditionalForwardJump{ccRef=ccRef2, condition=JB, label=defLab2}
                            ]
                    in
                        (testTag @ testCode, defLab1 :: defLab2 :: tagDefault)
                    end

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
                           the indexed-case instruction itself as well as any range checks. *)
                        fun addDefault ((startLabel, NONE, _), l) = ForwardJumpLabel{label=startLabel, result=NONE} :: l
                        |   addDefault ((_, SOME _, _), l) = l
                        fun asForward l = ForwardJumpLabel{label=l, result=NONE}
                        val dLabs = map asForward extraDefaults
                        val defLabels = List.foldl addDefault dLabs fullCaseRange
                    in
                        (* Put in the default.  Because this is the last we don't need to
                           jump round it. *)
                         defLabels @ codeToICode(default, context, isTail, target)
                    end
                    
                val codedCases = codeCases fullCaseRange

                val codeLabels = map (fn (lab, _, _) => lab) fullCaseRange
            in
                testCode @ rangeChecks @ [IndexedCaseOperation{testReg=testReg, workReg=workReg,
                                        cases=codeLabels, startValue=min}] @ codedCases
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
                PushExceptionHandler{handlerAddr=saveHandle, handleStart=handlerLab} :: expCode @
                [PopExceptionHandler{handlerAddr=saveHandle, resultReg=target, workReg=newPReg()},
                 UnconditionalForwardJump{label=skipHandler},
                 BeginHandler{handleStart=handlerLab, workReg=newPReg()}] @
                 handleCode @ [ForwardJumpLabel{label=skipHandler, result=SOME target}]
            end
        
        |   codeToICode(BICTuple fields, context, _, target) =
            let
                fun loadFields([], n) =
                        [AllocateMemoryOperation{size=n, isMutable=false, dest=target}]
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

        |   codeToICode(instr as BICTagTest _, context, isTail, target) =
                (* Better handled as a conditional *)
                codeAsConditional(instr, context, isTail, target)

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
                            then [AllocateMemoryOperation{size=sizeClosure, isMutable=true, dest=dest}]
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
            (* Check the "tag" word of a union (datatype).  N.B.  Not the same as testing the
               tag bit of a word. *)
            let
                val tagReg = newPReg() and ccRef = newCCRef()
                val testCode = codeToICode(test, context, false, tagReg)
            in
                testCode @
                    [WordComparison{arg1=tagReg, arg2=IntegerConstant(tag(Word.toLargeInt tagValue)), ccRef=ccRef},
                     ConditionalForwardJump{ccRef=ccRef,
                           condition=if jumpOn then JE else JNE, label=jumpLabel}]
            end

        |   codeCondition(BICBuiltIn1{oper=BuiltIns.NotBoolean, arg1}, context, jumpOn, jumpLabel) =
                (* If we have a "not" we can just invert the jump condition. *)
                codeCondition(arg1, context, not jumpOn, jumpLabel)

        |   codeCondition(BICBuiltIn1{oper=BuiltIns.IsTaggedValue, arg1}, context, jumpOn, jumpLabel) =
            let
                val testReg = newPReg() and ccRef = newCCRef()
                val testCode = codeToICode(arg1, context, false, testReg)
                (* Test the tag bit.  This sets the zero bit if the value is untagged. *)
            in
                testCode @
                    [TestTagBit{arg=testReg, ccRef=ccRef},
                     ConditionalForwardJump{ccRef=ccRef, condition=if jumpOn then JNE else JE, label=jumpLabel}]
            end

            (* Comparisons.  Because this is also used for pointer equality and even for exception matching it
               is perfectly possible that the argument could be an address. *)
        |   codeCondition(BICBuiltIn2{oper=BuiltIns.WordComparison{test, isSigned}, arg1, arg2=BICConstnt(arg2Value, _)},
                    context, jumpOn, jumpLabel) =
            let
                val testReg = newPReg() and ccRef = newCCRef()
                val testCode = codeToICode(arg1, context, false, testReg)
                val arg2Arg = constantAsArgument arg2Value
            in
                testCode @
                    [WordComparison{arg1=testReg, arg2=arg2Arg, ccRef=ccRef},
                     ConditionalForwardJump{ccRef=ccRef,
                           condition=testAsBranch(test, isSigned, jumpOn), label=jumpLabel}]
            end

(*        |   codeCondition(BICBuiltIn2{oper=BuiltIns.WordComparison{test, isSigned}, arg1=BICConstnt(arg1Value, _), arg2},
                    context, jumpOn, jumpLabel) =
            let
                val testReg = newPReg() and ccRef = newCCRef()
                val testCode = codeToICode(arg2, context, false, testReg)
                val arg1Arg = constantAsArgument arg1Value
            in
                testCode @
                    [WordComparison{arg1=arg1Arg, arg2=testReg, ccRef=ccRef},
                     ConditionalForwardJump{ccRef=ccRef,
                           condition=testAsBranch(test, isSigned, jumpOn), label=jumpLabel}]
            end*)

        |   codeCondition(BICBuiltIn2{oper=BuiltIns.WordComparison{test, isSigned}, arg1, arg2},
                    context, jumpOn, jumpLabel) =
            let
                val arg1Reg = newPReg() and arg2Reg = newPReg() and ccRef = newCCRef()
                val testCode =
                    codeToICode(arg1, context, false, arg1Reg) @
                    codeToICode(arg2, context, false, arg2Reg)
            in
                testCode @
                    [WordComparison{arg1=arg1Reg, arg2=arg2Reg, ccRef=ccRef},
                     ConditionalForwardJump{ccRef=ccRef,
                           condition=testAsBranch(test, isSigned, jumpOn), label=jumpLabel}]
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
                val testReg = newPReg() and ccRef = newCCRef()
                val testCode = codeToICode(condition, context, false, testReg)
            in
                testCode @
                    [WordComparison{arg1=testReg, arg2=IntegerConstant(tag 1), ccRef=ccRef},
                     ConditionalForwardJump{ccRef=ccRef,
                           condition=if jumpOn then JE else JNE, label=jumpLabel}]
            end

        (* Some operations that deliver boolean results are better coded as
           if condition then true else false *)
        and codeAsConditional(instr, context, isTail, target) =
            codeToICode(
                BICCond(instr, BICConstnt(toMachineWord 1, []), BICConstnt(toMachineWord 0, [])), context, isTail, target)

        val icode = pushClosureReg @ pushArgs @
            codeToICode(body, {loopArgs=NONE}, true, resultTarget) @ returnInstruction
      
        open ICODETRANSFORM
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
