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
    fun semitag c = 2 * c
 
    fun codeFunctionToX86({body, localCount, name, argTypes, closure, ...}:bicLambdaForm, debugSwitches, closureOpt) =
    let
        val pregCounter = ref 0
        fun newPReg() = PReg(!pregCounter, PRegGeneral) before pregCounter := !pregCounter + 1
        fun newUReg() = PReg(!pregCounter, PRegUntagged) before pregCounter := !pregCounter + 1
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
            case closure of [] => [] | _ => [MoveArgument{source=RealRegister(GenReg edx), dest=closureRegAddr, kind=WordToWord}]
        val pushArgs =
            ListPair.foldrEq (fn (r, m, l) => MoveArgument{source=RealRegister(GenReg r), dest=m, kind=WordToWord} :: l) [] (argRegsUsed, argPRegs)
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
    
        fun checkOverflow (ccRef, _ (* No longer used *)) =
        let
            val label = newLabel() and excReg = newPReg()
        in
            [
                ConditionalForwardJump{ ccRef=ccRef, condition=JNO, label=label },
                MoveArgument{source=AddressConstant(toMachineWord(Overflow)), dest=excReg, kind=WordToWord},
                RaiseExceptionPacket{packet=excReg},
                ForwardJumpLabel{ label=label, result=NONE }
            ]
        end
        
        (* Generally we have an offset in words and no index register. *)
        fun wordOffsetAddress(offset, baseReg) =
            MemoryLocation{offset=offset*wordSize, base=baseReg, index=NoMemIndex}
   
        (* The large-word operations all work on the value within the box pointed at
           by the register.  We generate all large-word operations using this even
           where the X86 instruction requires a register.  This allows the next level
           to optimise cases of cascaded instructions and avoid creating boxes for
           intermediate values. *)
        fun wordAt reg = wordOffsetAddress(0, reg)

        (* This controls what codeAsArgument returns.  Different instructions have different
           requirements.  If an option is set to false the value is instead loaded into a
           new preg.  "const32s" means that it will fit into 32-bits.  Any constant
           satisfies that on X86/32 but on the X86/64 we don't allow addresses because
           we can't be sure whether they will fit or not. *)
        type allowedArgument =
            { anyConstant: bool, const32s: bool, memAddr: bool, existingPreg: bool }
        val allowInMemMove = (* We can move a 32-bit constant into memory but not a long constant. *)
            { anyConstant=false, const32s=true, memAddr=false, existingPreg=true }

        datatype destination =
            SpecificPReg of argument
        |   NoResult
        |   Allowed of allowedArgument

        (* If a preg has been provided, use that, otherwise generate a new one. *)
        fun asTarget(SpecificPReg preg) = preg
        |   asTarget NoResult = newPReg()
        |   asTarget(Allowed _) = newPReg()

        fun moveIfNotAllowed(NoResult, code, arg) = (code, arg)

        |   moveIfNotAllowed(Allowed{anyConstant=true, ...}, code, arg as AddressConstant _) = (code, arg)
        
        |   moveIfNotAllowed(Allowed{anyConstant=true, ...}, code, arg as IntegerConstant _) = (code, arg)
        
        |   moveIfNotAllowed(dest as Allowed{const32s=true, ...}, code, arg as IntegerConstant value) =
            (* This is allowed if the value is within 32-bits *)
                if is32bit value
                then (code, arg)
                else moveToTarget(dest, code, arg)

        |   moveIfNotAllowed(dest as Allowed{const32s=true, ...}, code, arg as AddressConstant _) =
                if not isX64
                then (code, arg)
                else moveToTarget(dest, code, arg)

        |   moveIfNotAllowed(Allowed{existingPreg=true, ...}, code, arg as PReg _) = (code, arg)
        
        |   moveIfNotAllowed(Allowed{memAddr=true, ...}, code, arg as MemoryLocation _) = (code, arg)

        |   moveIfNotAllowed(dest, code, arg) = moveToTarget(dest, code, arg)

        and moveToTarget(dest, code, arg) =
            let
                val target = asTarget dest
            in
                (code @ [MoveArgument{source=arg, dest=target, kind=WordToWord}], target)
            end

        fun codeToICodeTarget(instr, context, isTail, target) =
        (* This is really for backwards compatibility.  *)
        let
            val (code, _) = codeToICode(instr, context, isTail, SpecificPReg target)
        in
            code
        end

        (* Main function to turn the codetree into ICode.  Optimisation is generally
           left to later passes.  This does detect tail recursion.  *)
        and codeToICode(BICNewenv (bindings, exp), context, isTail, target) =
            let
                val codeBindings = codeBindingsToICode(bindings, context)
                val (codeExp, result) = codeToICode(exp, context, isTail, target)
            in
                (codeBindings @ codeExp, result)
            end
            
        |   codeToICode(BICConstnt(value, _), _, _, destination) =
                moveIfNotAllowed(destination, [], constantAsArgument value)

        |   codeToICode(BICExtract(BICLoadLocal l), _, _, destination) =
                moveIfNotAllowed(destination, [], valOf(Array.sub(locToPregArray, l)))

        |   codeToICode(BICExtract(BICLoadArgument a), _, _, destination) =
            if a < numArgRegs
            then (* It was originally in a register.  It's now in a preg. *)
                moveIfNotAllowed(destination, [], List.nth(argPRegs, a))
            else (* Pushed before call. *)
            let
                val target = asTarget destination
            in
                ([MoveStackArgToPreg{offset=List.length argTypes - a, dest=target}], target)
            end
        
        |   codeToICode(BICExtract(BICLoadClosure c), _, _, destination) =
            (
                if c >= List.length closure then raise InternalError "BICExtract: closure" else ();
                (* N.B.  We need to add one to the closure entry because zero is the code address. *)
                moveIfNotAllowed(destination, [], wordOffsetAddress(c+1, closureRegAddr))
            )

        |   codeToICode(BICExtract BICLoadRecursive, _, _, destination) =
                (* If the closure is empty we must use the constant.  We can't guarantee that
                   the caller will actually load the closure register if it knows the closure
                   is empty. *)
                moveIfNotAllowed(destination, [],
                    case closure of
                        [] => AddressConstant(toMachineWord(valOf closureOpt))
                    |   _ => closureRegAddr)
        
        |   codeToICode(BICField{base, offset}, context, _, destination) =
            let
                val baseR = newPReg()
                val codeBase = codeToICodeTarget(base, context, false, baseR)
            in
                moveIfNotAllowed(destination, codeBase, wordOffsetAddress(offset, baseR))
            end

        |   codeToICode(BICEval {function, argList, ...}, context, isTail, destination) =
            let
                val target = asTarget destination
                val numArgs = List.length argList
                (* Create pregs for the closure and each argument. *)
                val clPReg = newPReg()
                (* If we have a constant closure we can go directly to the entry point.
                   If the closure is a single word we don't need to load the closure register. *)
                val (functionCode, closureEntry, callKind) =
                    case function of
                        BICConstnt(addr, _) =>
                        let
                            val addrAsAddr = toAddress addr
                            (* If this is a closure we're still compiling or if it's an address
                               of an io function (at the moment) we can't get the code address.
                               However if this is directly recursive we can use the recursive
                               convention. *)
                            val isRecursive =
                                case closureOpt of
                                    SOME a => wordEq(toMachineWord a, addr)
                                |   NONE => false
                        in
                            if isRecursive
                            then ([], [], Recursive)
                            else if flags addrAsAddr <> Address.F_words orelse isIoAddress addrAsAddr
                            then ([MoveArgument{source=AddressConstant addr, dest=clPReg, kind=WordToWord}],
                                      [(clPReg, edx)], FullCall)
                            else
                            let
                            
                                val addrLength = length addrAsAddr
                                val _ = addrLength >= 0w1 orelse raise InternalError "BICEval address"
                                val codeAddr = loadWord(addrAsAddr, 0w0)
                                val _ = isCode (toAddress codeAddr) orelse raise InternalError "BICEval address not code"
                            in
                                if addrLength = 0w1
                                then ([], [], ConstantCode codeAddr)
                                else ([MoveArgument{source=AddressConstant addr, dest=clPReg, kind=WordToWord}],
                                      [(clPReg, edx)], ConstantCode codeAddr)
                            end
                        end

                    |   BICExtract BICLoadRecursive =>
                        (
                            (* If the closure is empty we don't need to load rdx *)
                            case closure of
                                [] => ([], [], Recursive)
                            |   _ => ([MoveArgument {source=closureRegAddr, dest=clPReg, kind=WordToWord}], [(clPReg, edx)], Recursive)
                        )

                    |   function => (* General case. *)
                            (codeToICodeTarget(function, context, false, clPReg), [(clPReg, edx)], FullCall)
                (* Optimise arguments.  We have to be careful with tail-recursive functions because they
                   need to save any stack arguments that could be overwritten.  This is complicated
                   because we overwrite the stack before loading the register arguments.  In some
                   circumstances it could be safe but for the moment leave it. *)
                val allowDefer =
                    Allowed {anyConstant=true, const32s=true, memAddr=not isTail, existingPreg=not isTail }
                fun loadArgs [] = ([], [])
                |   loadArgs((arg, _)::args) =
                    let
                        val (c, r) = codeToICode(arg, context, false, allowDefer)
                        val (regs, code) = loadArgs args
                    in
                        (r::regs, c @ code)
                    end
                val (argPRegs, codeArgs) = loadArgs argList
                (* Load the argument registers.  This returns the
                   shorter of the actual arguments and those in registers. *)
                val regArgs = ListPair.zip (argPRegs, argRegs)
                (* The stack arguments are the later arguments in the list i.e. those
                   most recently pushed to the stack. *)
                val stackArgs =
                    if numArgs > List.length argRegs
                    then List.drop(argPRegs, List.length argRegs)
                    else []
            in
                (functionCode @ codeArgs @
                   [FunctionCall{regArgs=closureEntry @ regArgs, stackArgs=stackArgs, dest=target, isTail=isTail, callKind=callKind}], target)
            end

        |   codeToICode(BICBuiltIn0{oper=BuiltIns.CurrentThreadId}, _, _, destination) =
            (* Get the ID of the current thread. *)
            let
                val target = asTarget destination
                val argReg = newPReg() (* Should this be an untagged register? *)
                (* This is a duplicate of code in X86FOREIGNCALL. *)
                val memRegArgumentPtr =
                    case Compat560.polySpecificGeneral (108, 0) of
                        1 => (*X64Unix*) ~48
                    |   2 => (*X64Win*) 16
                    |   _ => (*X86_32*) 8
            in
                ([MoveArgument{
                    source=MemoryLocation{offset=memRegArgumentPtr, base=RealRegister(GenReg ebp), index=NoMemIndex},
                    dest=argReg, kind=WordToWord},
                 MoveArgument{
                    source=MemoryLocation{offset=argThreadSelf, base=argReg, index=NoMemIndex}, dest=target, kind=WordToWord}], target)
            end

        |   codeToICode(BICBuiltIn1{oper=BuiltIns.NotBoolean, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val tReg = newPReg() and ccRef = newCCRef()
            in
                (codeToICodeTarget(arg1, context, false, tReg) @
                    [ArithmeticFunction{oper=XOR, resultReg=target, operand1=tReg, operand2=IntegerConstant(semitag 1), ccRef=ccRef}], target)
            end

        |   codeToICode(instr as BICBuiltIn1{oper=BuiltIns.IsTaggedValue, ...}, context, isTail, destination) =
                codeAsConditional(instr, context, isTail, destination)

        |   codeToICode(BICBuiltIn1{oper=BuiltIns.MemoryCellLength, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val addrReg = newPReg() and argReg1 = newPReg() and argReg2 = newPReg() and argReg3 = newPReg()
                and ccRef1 = newCCRef() and ccRef2 = newCCRef() and ccRef3 = newCCRef()
                (* Get the length of a memory cell (heap object).  We need to mask out the
                   top byte containing the flags and to tag the result.  The mask is 56 bits on
                   64-bit which won't fit in an inline constant.  Since we have to shift it
                   anyway we might as well do this by shifts. *)
            in
                (codeToICodeTarget(arg1, context, false, addrReg) @
                [MoveArgument{source=wordOffsetAddress(~1, addrReg), dest=argReg1, kind=WordToWord},
                 ShiftOperation{shift=SHL, resultReg=argReg2, operand=argReg1, shiftAmount=IntegerConstant 8, ccRef=ccRef1 },
                 ShiftOperation{shift=SHR, resultReg=argReg3, operand=argReg2, shiftAmount=IntegerConstant 7 (* 8-tagshift*), ccRef=ccRef2 },
                 ArithmeticFunction{oper=OR, resultReg=target, operand1=argReg3, operand2=IntegerConstant 1, ccRef=ccRef3}], target)
            end

        |   codeToICode(BICBuiltIn1{oper=BuiltIns.MemoryCellFlags, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val addrReg = newPReg() and argReg1 = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, addrReg) @
                [MoveArgument{source=MemoryLocation{offset= ~1, base=addrReg, index=NoMemIndex}, dest=argReg1, kind=ByteToWordZeroExtend},
                 TagValue{ source=argReg1, dest=target }], target)
            end

        |   codeToICode(BICBuiltIn1{oper=BuiltIns.ClearMutableFlag, arg1}, context, _, destination) =
            let
                val addrReg = newPReg()
                val code =
                    codeToICodeTarget(arg1, context, false, addrReg) @ [LockMutable{addr=addrReg}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICBuiltIn1{oper=BuiltIns.StringLengthWord, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val addrReg = newPReg() and argReg1 = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, addrReg) @
                [MoveArgument{source=MemoryLocation{offset=0, base=addrReg, index=NoMemIndex}, dest=argReg1, kind=WordToWord},
                 TagValue{ source=argReg1, dest=target }], target)
            end

        |   codeToICode(BICBuiltIn1{oper=BuiltIns.AtomicIncrement, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val addrReg = newPReg() and incrReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, addrReg) @
                 [MoveArgument{source=IntegerConstant(semitag 1), dest=incrReg, kind=WordToWord},
                  AtomicExchangeAndAdd{ destAddr=addrReg, source=incrReg },
                  (* We want the result to be the new value but we've returned the old value. *)
                  ArithmeticFunction{oper=ADD, resultReg=target, operand1=incrReg, operand2=IntegerConstant(semitag 1), ccRef=newCCRef()}], target)
            end

        |   codeToICode(BICBuiltIn1{oper=BuiltIns.AtomicDecrement, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val addrReg = newPReg() and incrReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, addrReg) @
                 [MoveArgument{source=IntegerConstant(semitag ~1), dest=incrReg, kind=WordToWord},
                  AtomicExchangeAndAdd{ destAddr=addrReg, source=incrReg },
                  ArithmeticFunction{oper=SUB, resultReg=target, operand1=incrReg, operand2=IntegerConstant(semitag 1), ccRef=newCCRef()}], target)
            end

        |   codeToICode(BICBuiltIn1{oper=BuiltIns.AtomicReset, arg1}, context, _, destination) =
            let
                (* This is needed only for the interpreted version where we have a single real
                   mutex to interlock atomic increment and decrement.  We have to use the same
                   mutex to interlock clearing a mutex.  On the X86 we use hardware locking and
                   the hardware guarantees that an assignment of a word will be atomic. *)
                val addrReg = newPReg()
                val code =
                    codeToICodeTarget(arg1, context, false, addrReg) @ [LockMutable{addr=addrReg}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICBuiltIn1{oper=BuiltIns.LongWordToTagged, arg1}, context, _, destination) =
            let (* This is exactly the same as StringLengthWord at the moment.
                   TODO: introduce a new ICode entry so that the next stage can optimise
                   longword operations. *)
                val target = asTarget destination
                val addrReg = newPReg() and argReg1 = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, addrReg) @
                [MoveArgument{source=MemoryLocation{offset=0, base=addrReg, index=NoMemIndex}, dest=argReg1, kind=WordToWord},
                 TagValue{ source=argReg1, dest=target }], target)
            end

        |   codeToICode(BICBuiltIn1{oper=BuiltIns.SignedToLongWord, arg1}, context, _, destination) =
            let
                val addrReg = newPReg() and argReg1 = newPReg() and untagArg = newUReg()
                val code =
                    codeToICodeTarget(arg1, context, false, argReg1) @
                    [UntagValue{source=argReg1, dest=untagArg, isSigned=true},
                     BoxLargeWord{ source=untagArg, dest=addrReg}]
            in
                moveIfNotAllowed(destination, code, addrReg)
            end

        |   codeToICode(BICBuiltIn1{oper=BuiltIns.UnsignedToLongWord, arg1}, context, _, destination) =
            let
                val addrReg = newPReg() and argReg1 = newPReg() and untagArg = newUReg()
                val code =
                    codeToICodeTarget(arg1, context, false, argReg1) @
                    [UntagValue{source=argReg1, dest=untagArg, isSigned=false},
                     BoxLargeWord{ source=untagArg, dest=addrReg}]
            in
                moveIfNotAllowed(destination, code, addrReg)
            end

        |   codeToICode(instr as BICBuiltIn2{oper=BuiltIns.WordComparison _, ...}, context, isTail, destination) =
                codeAsConditional(instr, context, isTail, destination)

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithAdd, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
            let
                val target = asTarget destination
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
                (codeToICodeTarget(arg1, context, false, aReg1) @
                    [ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg1, operand2=IntegerConstant constVal, ccRef=ccRef}] @
                    checkOverflow (ccRef, target), target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithAdd, arg1=BICConstnt(value, _), arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg2 = newPReg() and ccRef = newCCRef()
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
            in
                (codeToICodeTarget(arg2, context, false, aReg2) @
                    [ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg2, operand2=IntegerConstant constVal, ccRef=ccRef}] @
                    checkOverflow (ccRef, target), target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithAdd, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newPReg() and ccRef = newCCRef()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Subtract the tag bit from the second argument, do the addition and check for overflow. *)
                    (* TODO: We should really do the detagging in the transform phase.  It can make a better choice of
                       the argument if one of the arguments is already untagged or if we have a constant argument. *)
                    [ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=aReg2, operand2=IntegerConstant 1, ccRef=newCCRef()},
                     ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg1, operand2=aReg3, ccRef=ccRef}] @
                    checkOverflow (ccRef, target), target)
            end

            (* Subtraction.  We can handle the special case of the second argument being a constant but not the first. *)
        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithSub, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and ccRef = newCCRef()
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @
                    [ArithmeticFunction{oper=SUB, resultReg=target, operand1=aReg1, operand2=IntegerConstant constVal, ccRef=ccRef}] @
                    checkOverflow (ccRef, target), target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithSub, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newPReg()
                val ccRef = newCCRef()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Do the subtraction, test for overflow and afterwards add in the tag bit. *)
                    [ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=aReg1, operand2=aReg2, ccRef=ccRef}] @
                    checkOverflow (ccRef, target) @
                    [ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg3, operand2=IntegerConstant 1, ccRef=newCCRef()}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithMult, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and arg1Untagged = newUReg()
                and arg2Untagged = newUReg() and resUntagged = newUReg()
                val mulCC = newCCRef()
                (* This is almost the same as the word operation except we use a signed shift and check for overflow. *)
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift one argument and subtract the tag from the other.  It's possible this could be reordered
                       if we have a value that is already untagged. *)
                    [UntagValue{source=aReg1, dest=arg1Untagged, isSigned=true (* Signed shift here. *)},
                     ArithmeticFunction{oper=SUB, resultReg=arg2Untagged, operand1=aReg2, operand2=IntegerConstant 1, ccRef=newCCRef()},
                     Multiplication{resultReg=resUntagged, operand1=arg1Untagged, operand2=arg2Untagged, ccRef=mulCC} ] @
                     checkOverflow(mulCC, target) @
                     [ArithmeticFunction{oper=ADD, resultReg=target, operand1=resUntagged, operand2=IntegerConstant 1, ccRef=newCCRef()}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithQuot, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift both of the arguments to remove the tags.  We don't test for zero here - that's done explicitly. *)
                    [UntagValue{source=aReg1, dest=arg1Untagged, isSigned=true},
                     UntagValue{source=aReg2, dest=arg2Untagged, isSigned=true},
                     Division { isSigned = true, dividend=arg1Untagged, divisor=arg2Untagged,
                                quotient=quotient, remainder=remainder },
                     TagValue { source=quotient, dest=target }], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithRem, arg1, arg2}, context, _, destination) =
            let
                (* Identical to Quot except that the result is the remainder. *)
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift both of the arguments to remove the tags. *)
                    [UntagValue{source=aReg1, dest=arg1Untagged, isSigned=true},
                     UntagValue{source=aReg2, dest=arg2Untagged, isSigned=true},
                     Division { isSigned = true, dividend=arg1Untagged, divisor=arg2Untagged,
                                quotient=quotient, remainder=remainder },
                     TagValue { source=remainder, dest=target }], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg()
                (* If the argument is a constant we can subtract the tag beforehand.
                   N.B. it is possible to have type-incorrect values in dead code. i.e. code that will
                   never be executed because of a run-time check.  *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @
                    [ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg1, operand2=IntegerConstant constVal, ccRef = newCCRef()}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1=BICConstnt(value, _), arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg2 = newPReg()
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
            in
                (codeToICodeTarget(arg2, context, false, aReg2) @
                    [ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg2, operand2=IntegerConstant constVal, ccRef = newCCRef()}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newPReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Subtract the tag bit from the second argument and do the addition.  No need for overflow check. *)
                    [ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=aReg2, operand2=IntegerConstant 1, ccRef=newCCRef()},
                     ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg1, operand2=aReg3, ccRef=newCCRef()}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordArith BuiltIns.ArithSub, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg()
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @
                    [ArithmeticFunction{oper=SUB, resultReg=target, operand1=aReg1, operand2=IntegerConstant constVal, ccRef=newCCRef()}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordArith BuiltIns.ArithSub, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newPReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Do the subtraction and add in the tag bit.  This could be reordered if we have cascaded operations
                       since we don't need to check for overflow. *)
                    [ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=aReg1, operand2=aReg2, ccRef=newCCRef()},
                     ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg3, operand2=IntegerConstant 1, ccRef=newCCRef()}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordArith BuiltIns.ArithMult, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and arg1Untagged = newUReg()
                and arg2Untagged = newUReg() and resUntagged = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift one argument and subtract the tag from the other.  It's possible this could be reordered
                       if we have a value that is already untagged. *)
                    [UntagValue{source=aReg1, dest=arg1Untagged, isSigned=false},
                     ArithmeticFunction{oper=SUB, resultReg=arg2Untagged, operand1=aReg2, operand2=IntegerConstant 1, ccRef=newCCRef()},
                     Multiplication{resultReg=resUntagged, operand1=arg1Untagged, operand2=arg2Untagged, ccRef=newCCRef()},
                     ArithmeticFunction{oper=ADD, resultReg=target, operand1=resUntagged, operand2=IntegerConstant 1, ccRef=newCCRef()}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordArith BuiltIns.ArithQuot, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift both of the arguments to remove the tags.  We don't test for zero here - that's done explicitly. *)
                    [UntagValue{source=aReg1, dest=arg1Untagged, isSigned=false},
                     UntagValue{source=aReg2, dest=arg2Untagged, isSigned=false},
                     Division { isSigned = false, dividend=arg1Untagged, divisor=arg2Untagged,
                                quotient=quotient, remainder=remainder },
                     TagValue { source=quotient, dest=target }], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordArith BuiltIns.ArithRem, arg1, arg2}, context, _, destination) =
            let
                (* Identical to Quot except that the result is the remainder. *)
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift both of the arguments to remove the tags. *)
                    [UntagValue{source=aReg1, dest=arg1Untagged, isSigned=false},
                     UntagValue{source=aReg2, dest=arg2Untagged, isSigned=false},
                     Division { isSigned = false, dividend=arg1Untagged, divisor=arg2Untagged,
                                quotient=quotient, remainder=remainder },
                     TagValue { source=remainder, dest=target }], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.SetStringLengthWord, arg1, arg2}, context, _, destination) =
            let
                val addrReg = newPReg() and lengthReg = newPReg()
                val untagLength = newUReg()
                val code =
                    codeToICodeTarget(arg1, context, false, addrReg) @ codeToICodeTarget(arg2, context, false, lengthReg) @
                        [UntagValue{source=lengthReg, dest=untagLength, isSigned=false},
                         MoveArgument {source=untagLength, dest=MemoryLocation{base=addrReg, offset=0, index=NoMemIndex}, kind=WordToWord}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordLogical BuiltIns.LogicalOr, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Or-ing preserves the tag bit. *)
                    [ArithmeticFunction{oper=OR, resultReg=target, operand1=aReg1, operand2=aReg2, ccRef=newCCRef()}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordLogical BuiltIns.LogicalAnd, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Since they're both tagged the result will be tagged. *)
                    [ArithmeticFunction{oper=AND, resultReg=target, operand1=aReg1, operand2=aReg2, ccRef=newCCRef()}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordLogical BuiltIns.LogicalXor, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newPReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* We need to restore the tag bit after the operation. *)
                    [ArithmeticFunction{oper=XOR, resultReg=aReg3, operand1=aReg1, operand2=aReg2, ccRef=newCCRef()},
                     ArithmeticFunction{oper=OR, resultReg=target, operand1=aReg3, operand2=IntegerConstant 1, ccRef=newCCRef()}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.WordShift shift, arg1, arg2}, context, _, destination) =
                (* N.B.  X86 shifts of greater than the word length mask the higher bits.  That isn't what ML wants
                   but that is dealt with at a higher level *)
            let
                open BuiltIns
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val aReg1Untagged = newUReg() and aReg2Untagged = newUReg() and resRegUntagged = newUReg()
                val shiftOp = case shift of ShiftLeft => SHL | ShiftRightLogical => SHR | ShiftRightArithmetic => SAR
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                 (* There are better ways of doing this but it will do for the moment.  We don't need to
                    remove the tag for right shifts provided we OR in a tag afterwards. *)
                 [UntagValue{source=aReg1, dest=aReg1Untagged, isSigned=shift=ShiftRightArithmetic},
                  UntagValue{source=aReg2, dest=aReg2Untagged, isSigned=false},
                  ShiftOperation{ shift=shiftOp, resultReg=resRegUntagged, operand=aReg1Untagged, shiftAmount=aReg2Untagged, ccRef=newCCRef() },
                  TagValue{source=resRegUntagged, dest=target}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.AllocateByteMemory, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                 [AllocateMemoryVariable{size=aReg1, flags=aReg2, dest=target, initialiser=NONE},
                 InitialisationComplete{dest=target}], target)
            end

        |   codeToICode(instr as BICBuiltIn2{oper=BuiltIns.LargeWordComparison _, ...}, context, isTail, destination) =
                codeAsConditional(instr, context, isTail, destination)

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.LargeWordArith BuiltIns.ArithAdd, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [ArithmeticFunction{oper=ADD, resultReg=aReg3, operand1=wordAt aReg1, operand2=wordAt aReg2, ccRef=newCCRef()},
                     BoxLargeWord{source=aReg3, dest=target}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.LargeWordArith BuiltIns.ArithSub, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Do the subtraction and add in the tag bit.  This could be reordered if we have cascaded operations
                       since we don't need to check for overflow. *)
                    [ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=wordAt aReg1, operand2=wordAt aReg2, ccRef=newCCRef()},
                     BoxLargeWord{source=aReg3, dest=target}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.LargeWordArith BuiltIns.ArithMult, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and resValue = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [Multiplication{resultReg=resValue, operand1=wordAt aReg1, operand2=wordAt aReg2, ccRef=newCCRef()},
                     BoxLargeWord{source=resValue, dest=target}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.LargeWordArith BuiltIns.ArithQuot, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* We don't test for zero here - that's done explicitly. *)
                    [Division { isSigned = false, dividend=wordAt aReg1, divisor=wordAt aReg2,
                                quotient=quotient, remainder=remainder },
                     BoxLargeWord{source=quotient, dest=target}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.LargeWordArith BuiltIns.ArithRem, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* We don't test for zero here - that's done explicitly. *)
                    [Division { isSigned = false, dividend=wordAt aReg1, divisor=wordAt aReg2,
                                quotient=quotient, remainder=remainder },
                     BoxLargeWord{source=remainder, dest=target}], target)
            end


        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.LargeWordLogical BuiltIns.LogicalOr, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [ArithmeticFunction{oper=OR, resultReg=aReg3, operand1=wordAt aReg1, operand2=wordAt aReg2, ccRef=newCCRef()},
                     BoxLargeWord{source=aReg3, dest=target}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.LargeWordLogical BuiltIns.LogicalAnd, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [ArithmeticFunction{oper=AND, resultReg=aReg3, operand1=wordAt aReg1, operand2=wordAt aReg2, ccRef=newCCRef()},
                     BoxLargeWord{source=aReg3, dest=target}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.LargeWordLogical BuiltIns.LogicalXor, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [ArithmeticFunction{oper=XOR, resultReg=aReg3, operand1=wordAt aReg1, operand2=wordAt aReg2, ccRef=newCCRef()},
                     BoxLargeWord{source=aReg3, dest=target}], target)
            end

        |   codeToICode(BICBuiltIn2 {oper=BuiltIns.LargeWordShift shift, arg1, arg2}, context, _, destination) =
                (* The shift is always a Word.word value i.e. tagged.  There is a check at the higher level
                   that the shift does not exceed 32/64 bits. *)
            let
                open BuiltIns
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val aReg2Untagged = newUReg() and aReg3 = newUReg()
                val shiftOp = case shift of ShiftLeft => SHL | ShiftRightLogical => SHR | ShiftRightArithmetic => SAR
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                 [UntagValue{source=aReg2, dest=aReg2Untagged, isSigned=false},
                  ShiftOperation{ shift=shiftOp, resultReg=aReg3, operand=wordAt aReg1, shiftAmount=aReg2Untagged, ccRef=newCCRef() },
                  BoxLargeWord{source=aReg3, dest=target}], target)
            end

        |   codeToICode(BICBuiltIn3 {oper=BuiltIns.AllocateWordMemory, arg1 as BICConstnt(length, _), arg2 as BICConstnt(flags, _), arg3}, context, _, destination) =
            (* Constant length and flags is used for ref.  We could handle other cases. *)
            if  isShort length andalso isShort flags andalso toShort length = 0w1
            then
            let
                val vecLength = Word.toInt(toShort length)
                val flagByte = Word8.fromLargeWord(Word.toLargeWord(toShort flags))
                val memAddr = newPReg() and valueReg = newPReg()
                fun initialise n =
                    MoveArgument{ source=valueReg, dest=wordOffsetAddress(n, memAddr), kind=WordToWord}
                val code =
                    codeToICodeTarget(arg3, context, false, valueReg) @
                    [AllocateMemoryOperation{size=vecLength, flags=flagByte, dest=memAddr}] @
                    List.tabulate(vecLength, initialise) @
                    [InitialisationComplete{dest=memAddr}]
            in
                moveIfNotAllowed(destination, code, memAddr)
            end
            else (* If it's longer use the full run-time form. *)
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newPReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                 codeToICodeTarget(arg3, context, false, aReg3) @
                 [AllocateMemoryVariable{size=aReg1, flags=aReg2, dest=target, initialiser=SOME aReg3},
                  InitialisationComplete{dest=target}], target)
            end

        |   codeToICode(BICBuiltIn3 {oper=BuiltIns.AllocateWordMemory, arg1, arg2, arg3}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newPReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                 codeToICodeTarget(arg3, context, false, aReg3) @
                 [AllocateMemoryVariable{size=aReg1, flags=aReg2, dest=target, initialiser=SOME aReg3},
                  InitialisationComplete{dest=target}], target)
            end

        |   codeToICode(BICBuiltIn4 _, _, _, _) =
                raise InternalError "TODO: BICBuiltIn4"

        |   codeToICode(instr as BICBuiltIn5{oper=BuiltIns.ByteVecEqual, ...}, context, isTail, destination) =
                codeAsConditional(instr, context, isTail, destination)

        |   codeToICode(BICBuiltIn5{oper=BuiltIns.ByteVecCompare, arg1, arg2, arg3, arg4, arg5}, context, _, destination) =
            let
                val target = asTarget destination
                val arg1Reg = newPReg() and arg2Reg = newPReg() and arg3Reg = newPReg()
                and arg4Reg = newPReg() and arg5Reg = newPReg()
                val untag3Reg = newUReg() and untag4Reg = newUReg()
                val vec1Reg = newUReg() and vec2Reg = newUReg() and lenReg = newUReg()
                val ccRef = newCCRef()
                val labLess = newLabel() and labGreater = newLabel()
                and exitLab1 = newLabel() and exitLab2 = newLabel()
            in
                (codeToICodeTarget(arg1, context, false, arg1Reg) @
                codeToICodeTarget(arg2, context, false, arg2Reg) @
                codeToICodeTarget(arg3, context, false, arg3Reg) @
                codeToICodeTarget(arg4, context, false, arg4Reg) @
                codeToICodeTarget(arg5, context, false, arg5Reg) @
                [UntagValue{source=arg3Reg, dest=untag3Reg, isSigned=false},
                 LoadEffectiveAddress{base=SOME arg1Reg, offset=0, index=MemIndex1 untag3Reg, dest=vec1Reg},
                 UntagValue{source=arg4Reg, dest=untag4Reg, isSigned=false},
                 LoadEffectiveAddress{base=SOME arg2Reg, offset=0, index=MemIndex1 untag4Reg, dest=vec2Reg},
                 UntagValue{source=arg5Reg, dest=lenReg, isSigned=false},
                 CompareByteVectors{ vec1Addr=vec1Reg, vec2Addr=vec2Reg, length=lenReg, ccRef=ccRef },
                 ConditionalForwardJump{ ccRef=ccRef, condition=JL, label=labLess },
                 ConditionalForwardJump{ ccRef=ccRef, condition=JG, label=labGreater },
                 MoveArgument{ source=IntegerConstant(tag 0), dest=target, kind=WordToWord },
                 UnconditionalForwardJump{ label=exitLab1 },
                 ForwardJumpLabel{ label=labLess, result=NONE },
                 MoveArgument{ source=IntegerConstant(tag ~1), dest=target, kind=WordToWord },
                 UnconditionalForwardJump{ label=exitLab2 },
                 ForwardJumpLabel{ label=labGreater, result=NONE },
                 MoveArgument{ source=IntegerConstant(tag 1), dest=target, kind=WordToWord },
                 ForwardJumpLabel{ label=exitLab1, result=SOME target },
                 ForwardJumpLabel{ label=exitLab2, result=SOME target }], target)
            end

        |   codeToICode(BICBuiltIn5{oper=BuiltIns.BlockMove{isByteMove}, arg1, arg2, arg3, arg4, arg5}, context, _, destination) =
            let
                val arg1Reg = newPReg() and arg2Reg = newPReg() and arg3Reg = newPReg()
                and arg4Reg = newPReg() and arg5Reg = newPReg()
                val untag3Reg = newUReg() and untag4Reg = newUReg()
                val vec1Reg = newUReg() and vec2Reg = newUReg() and lenReg = newUReg()
                val code =
                    codeToICodeTarget(arg1, context, false, arg1Reg) @
                    codeToICodeTarget(arg2, context, false, arg2Reg) @
                    codeToICodeTarget(arg3, context, false, arg3Reg) @
                    codeToICodeTarget(arg4, context, false, arg4Reg) @
                    codeToICodeTarget(arg5, context, false, arg5Reg) @
                    [UntagValue{source=arg3Reg, dest=untag3Reg, isSigned=false},
                     LoadEffectiveAddress{base=SOME arg1Reg, offset=0, index=MemIndex1 untag3Reg, dest=vec1Reg},
                     UntagValue{source=arg4Reg, dest=untag4Reg, isSigned=false},
                     LoadEffectiveAddress{base=SOME arg2Reg, offset=0, index=MemIndex1 untag4Reg, dest=vec2Reg},
                     UntagValue{source=arg5Reg, dest=lenReg, isSigned=false},
                     BlockMove{ srcAddr=vec1Reg, destAddr=vec2Reg, length=lenReg, isByteMove=isByteMove }]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICLambda(lambda as { closure = [], ...}), _, _, destination) =
            (* Empty closure - create a constant closure for any recursive calls. *)
            let
                val closure = Address.allocWordData(0w1, Word8.orb (F_mutable, F_words), Address.toMachineWord 0w0)
                val codeAddr = codeFunctionToX86(lambda, debugSwitches, SOME closure)
                open Address
            in
                assignWord(closure, 0w0, toMachineWord codeAddr);
                lock closure;
                moveIfNotAllowed(destination, [], AddressConstant(toMachineWord closure))
            end

        |   codeToICode(BICLambda(lambda as { closure, ...}), context, isTail, destination) =
            (* Non-empty closure.  Ignore stack closure option at the moment. *)
            let
                val codeAddr = codeFunctionToX86(lambda, debugSwitches, NONE)
            in
                (* Treat it as a tuple with the code as the first field. *)
                codeToICode(BICTuple(BICConstnt(toMachineWord codeAddr, []) :: map BICExtract closure), context, isTail, destination)
            end

        |   codeToICode(BICCond(test, thenPt, elsePt), context, isTail, NoResult) =
            (
                (* If we don't want the result but are only evaluating for side-effects we
                   may be able to optimise special cases. *)
                case (codeToICode(thenPt, context, isTail, NoResult),
                      codeToICode(elsePt, context, isTail, NoResult)) of
                    (([], _), (elseCode, _)) =>
                        let
                            val skipElse = newLabel()
                            val testCode = codeCondition(test, context, true (* Invert the test. *), skipElse)
                        in
                            (testCode @ elseCode @ [ForwardJumpLabel{label=skipElse, result=NONE}], (* Unit result *) IntegerConstant(tag 0))
                        end
                |   ((thenCode, _), ([], _)) =>
                        let
                            val skipThen = newLabel()
                            val testCode = codeCondition(test, context, false, skipThen)
                        in
                            (testCode @ thenCode @ [ForwardJumpLabel{label=skipThen, result=NONE}], (* Unit result *) IntegerConstant(tag 0))
                        end
                |   ((thenCode, _), (elseCode, _)) =>
                        let
                            val startElse = newLabel() and skipElse = newLabel()
                            val testCode = codeCondition(test, context, false, startElse)
                        in
                            (testCode @
                                thenCode @ [UnconditionalForwardJump{label=skipElse}, ForwardJumpLabel{label=startElse, result=NONE}] @
                                elseCode @ [ForwardJumpLabel{label=skipElse, result=NONE}], (* Unit result *) IntegerConstant(tag 0))
                        end
            )

        |   codeToICode(BICCond(test, thenPt, elsePt), context, isTail, destination) =
            let
                (* TODO: If the target is NoResult we want something different here. *)
                val target = asTarget destination
                val startElse = newLabel() and skipElse = newLabel()
                val testCode = codeCondition(test, context, false, startElse)
                (* Put the result in the target register. *)
                val thenCode = codeToICodeTarget(thenPt, context, isTail, target)
                val elseCode = codeToICodeTarget(elsePt, context, isTail, target)
            in
                (testCode @
                    thenCode @ [UnconditionalForwardJump{label=skipElse}, ForwardJumpLabel{label=startElse, result=NONE}] @
                    elseCode @ [ForwardJumpLabel{label=skipElse, result=SOME target}], target)
            end
        
        |   codeToICode(BICCase { cases, test, caseType, default}, context, isTail, destination) =
            let
                val target = asTarget destination
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
                val testCode = codeToICodeTarget(test, context, false, testReg)
                
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
                                    MoveArgument {source=testReg, dest=tReg, kind=WordToWord},
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
                                MoveArgument {source=testReg, dest=tReg1, kind=WordToWord},
                                WordComparison{arg1=tReg1, arg2=IntegerConstant(tag(Word.toLargeInt max)), ccRef=ccRef1},
                                ConditionalForwardJump{ccRef=ccRef1, condition=JA, label=defLab1},
                                MoveArgument {source=testReg, dest=tReg2, kind=WordToWord},
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
                                codeToICodeTarget(c, context, isTail, target) @ [UnconditionalForwardJump{label=exitLabel}]
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
                         defLabels @ codeToICodeTarget(default, context, isTail, target)
                    end
                    
                val codedCases = codeCases fullCaseRange

                val codeLabels = map (fn (lab, _, _) => lab) fullCaseRange
            in
                (testCode @ rangeChecks @ [IndexedCaseOperation{testReg=testReg, workReg=workReg,
                                        cases=codeLabels, startValue=min}] @ codedCases, target)
            end
        
        |   codeToICode(BICBeginLoop {loop, arguments}, context, isTail, destination) =
            let
                val target = asTarget destination
                (* First evaluate the initial values for the arguments to new pregs.
                   These are only used for the initial values. *)
                val argPRegs = map(fn _ => newPReg()) arguments
                val codeArgs =
                    ListPair.foldlEq(fn (({value, ...}, _), pr, l) => l @ codeToICodeTarget(value, context, false, pr)) []
                        (arguments, argPRegs)
                (* Create loop arguments.  These are associated with the loop variables. *)
                fun makeLoopReg ({addr, ...}, _) =
                    let val pr = newPReg() val () = Array.update(locToPregArray, addr, SOME pr) in pr end
                val loopRegs = map makeLoopReg arguments
                val loopLabel = newLabel()
                val loopBody = codeToICodeTarget(loop, {loopArgs=SOME (loopRegs, loopLabel) }, isTail, target)
                val args = ListPair.mapEq(fn (s, l) => {source=s, loopReg=l}) (argPRegs, loopRegs)
            in
                (codeArgs @ [StartLoop{arguments=args, loopLabel=loopLabel}] @ loopBody @ [EndLoop {loopLabel=loopLabel}], target)
            end
        
        |   codeToICode(BICLoop args, context as {loopArgs=SOME (loopRegs, loopLabel), ...}, _, destination) =
            let
                val target = asTarget destination
                (* Registers to receive the evaluated arguments.  We can't put the
                   values into the loop variables yet because the values could depend
                   on the current values of the loop variables. *)
                val argPRegs = map(fn _ => newPReg()) args
                val codeArgs =
                    ListPair.foldlEq(fn ((arg, _), pr, l) => l @ codeToICodeTarget(arg, context, false, pr)) []
                        (args, argPRegs)
                val jumpArgs = ListPair.mapEq(fn (s, l) => {source=s, loopReg=l}) (argPRegs, loopRegs)
            in
                (codeArgs @ [JumpLoop{arguments=jumpArgs, loopLabel=loopLabel}], target)
            end

        |   codeToICode(BICLoop _, {loopArgs=NONE, ...}, _, _) =
                raise InternalError "BICLoop without BICBeginLoop"

        |   codeToICode(BICRaise exc, context, _, destination) =
            let
                val excReg = newPReg()
            in
                (codeToICodeTarget(exc, context, false, excReg) @ [RaiseExceptionPacket{packet=excReg}], asTarget destination)
            end
        
        |   codeToICode(BICLdexc, _, _, destination) = (* The argument is in eax. *)
            let
                val target = asTarget destination
            in
                ([MoveArgument{source=RealRegister(GenReg eax), dest=target, kind=WordToWord}], target)
            end
        
        |   codeToICode(BICHandle {exp, handler}, context, isTail, destination) =
            let
                val target = asTarget destination
                (* The expression cannot be treated as a tail because the handler has
                   to be removed after *)
                val expCode = codeToICodeTarget(exp, context, false (* Not tail *), target)
                val handleCode = codeToICodeTarget(handler, context, isTail, target)
                val saveHandle = newPReg()
                val handlerLab = newLabel()
                val skipHandler = newLabel()
            in
                (PushExceptionHandler{handlerAddr=saveHandle, handleStart=handlerLab} :: expCode @
                [PopExceptionHandler{handlerAddr=saveHandle, resultReg=target, workReg=newPReg()},
                 UnconditionalForwardJump{label=skipHandler},
                 BeginHandler{handleStart=handlerLab, workReg=newPReg()}] @
                 handleCode @ [ForwardJumpLabel{label=skipHandler, result=SOME target}], target)
            end
        
        |   codeToICode(BICTuple fields, context, _, destination) =
            let
                (* We want the result address to be on the top of the stack when we've
                   finished.  Allocate the memory to a new register and copy it over to
                   the target when we've finished. *)
                val memAddr = newPReg()
                fun loadFields([], n) =
                        [AllocateMemoryOperation{size=n, flags=0w0, dest=memAddr}]
                |   loadFields(f :: rest, n) =
                    let
                        val (code, source) = codeToICode(f, context, false, Allowed allowInMemMove)
                        val restAndAlloc = loadFields(rest, n+1)
                        val storeValue = [MoveArgument{ source=source, dest=wordOffsetAddress(n, memAddr), kind=WordToWord}]
                    in
                        code @ restAndAlloc @ storeValue
                    end
                val code = loadFields(fields, 0) @ [InitialisationComplete{dest=memAddr}]
            in
                moveIfNotAllowed(destination, code, memAddr)
            end
        
        |   codeToICode(BICContainer size , _, _, destination) =
            let
                val target = asTarget destination
            in
                ([ReserveContainer{size=size, address=target}], target)
            end

            (* Copy the source tuple into the container.  If the source is a BICTuple we simply copy
               the fields.  That saves creating the tuple at compile time. *)
        |   codeToICode(BICSetContainer{container, tuple=BICTuple cl, filter}, context, _, destination) =
            let
                val containerTarget = newPReg()
                val codeContainer = codeToICodeTarget(container, context, false, containerTarget)
                (* In theory it's possible that the tuple could contain fields that are not
                   used but nevertheless need to be evaluated for their side-effects.
                   Create all the fields and push to the stack. *)
                fun codeAll [] = ([], [])
                |   codeAll(arg::args) =
                    let
                        val (c, r) = codeToICode(arg, context, false, Allowed allowInMemMove)
                        val (regs, code) = codeAll args
                    in
                        (r::regs, c @ code)
                    end
                val (pregs, codeFields) = codeAll cl

                val filterLength = BoolVector.length filter

                fun copyContainer([], _, _) = []
                |   copyContainer(srcReg :: otherRegs, sourceWord, destWord) =
                    if sourceWord < filterLength andalso BoolVector.sub(filter, sourceWord)
                    then
                        MoveArgument{source=srcReg, dest=wordOffsetAddress(destWord, containerTarget), kind=WordToWord} ::
                            copyContainer(otherRegs, sourceWord+1, destWord+1)

                    else copyContainer(otherRegs, sourceWord+1, destWord)
                    
                val code = codeContainer @ codeFields @ copyContainer(pregs, 0, 0)
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICSetContainer{container, tuple, filter}, context, _, destination) =
            let
                val containerTarget = newPReg() and tupleTarget = newPReg()
                val codeContainer = codeToICodeTarget(container, context, false, containerTarget)
                and codeTuple = codeToICodeTarget(tuple, context, false, tupleTarget)
                
                val filterLength = BoolVector.length filter

                fun copyContainer(sourceWord, destWord) =
                    if sourceWord = filterLength
                    then []
                    else if BoolVector.sub(filter, sourceWord)
                    then
                    let
                        val loadReg = newPReg()
                    in
                        MoveArgument{source=wordOffsetAddress(sourceWord, tupleTarget), dest=loadReg, kind=WordToWord} ::
                        MoveArgument{source=loadReg, dest=wordOffsetAddress(destWord, containerTarget), kind=WordToWord} ::
                            copyContainer(sourceWord+1, destWord+1)
                    end
                    else copyContainer(sourceWord+1, destWord)
                val code = codeContainer @ codeTuple @ copyContainer(0, 0)
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(instr as BICTagTest _, context, isTail, destination) =
                (* Better handled as a conditional *)
                codeAsConditional(instr, context, isTail, destination)

        |   codeToICode(BICLoadOperation {kind=LoadStoreMLWord _, address={base=arg1, index=NONE, offset}}, context, _, destination) =
            let
                val target = asTarget destination
                (* Constant offset.  This is very common since it's used for refs. *)
                (* TODO: We could also handle the case where the case where the base address is a constant
                   as a special case. *)
                val baseReg = newPReg()
                (* The offset should always be positive.  Loading the length word is a separate function. *)
                val offset = Word.toInt offset
            in
                (codeToICodeTarget(arg1, context, false, baseReg) @
                    [MoveArgument {source=MemoryLocation{offset=offset, base=baseReg, index=NoMemIndex}, dest=target, kind=WordToWord}], target)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreMLWord _, address={base=arg1, index=SOME arg2, offset}}, context, _, destination) =
            let
                val target = asTarget destination
                (* Variable offset.  We don't have to untag the index.  Instead we can adjust the
                   index factor and offset.  *)
                val baseReg = newPReg() and indexReg = newPReg()
                val source =
                    if isX64
                    then MemoryLocation{base=baseReg, offset= Word.toInt offset - 4, index= MemIndex4 indexReg}
                    else MemoryLocation{base=baseReg, offset= Word.toInt offset - 2, index= MemIndex2 indexReg}
            in
                (codeToICodeTarget(arg1, context, false, baseReg) @ codeToICodeTarget(arg2, context, false, indexReg) @
                     [MoveArgument {source=source, dest=target, kind=WordToWord}], target)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreMLByte _, address={base=arg1, index=NONE, offset}}, context, _, destination) =
            let
                val target = asTarget destination
                (* Constant offset - this is much less common than for LoadWord. *)
                val baseReg = newPReg() and untaggedResReg = newUReg()
                (* The offset should always be positive. *)
                val offset = Word.toInt offset
            in
                (codeToICodeTarget(arg1, context, false, baseReg) @
                    [MoveArgument {
                        source=MemoryLocation{offset= offset, base=baseReg, index=NoMemIndex},
                        dest=untaggedResReg, kind=ByteToWordZeroExtend},
                     TagValue {source=untaggedResReg, dest=target}], target)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreMLByte _, address={base=arg1, index=SOME arg2, offset}}, context, _, destination) =
            let
                val target = asTarget destination
                (* Variable offset.  Because the index is directly added to the address we have to
                   untag it.  *)
                val baseReg = newPReg() and indexReg = newPReg() and untaggedIndex = newUReg()
                val untaggedResReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, baseReg) @ codeToICodeTarget(arg2, context, false, indexReg) @
                    [UntagValue{source=indexReg, dest=untaggedIndex, isSigned=false},
                     MoveArgument {source=MemoryLocation{base=baseReg, offset=Word.toInt offset, index=MemIndex1 untaggedIndex}, dest=untaggedResReg, kind=ByteToWordZeroExtend},
                     TagValue {source=untaggedResReg, dest=target}], target)
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreMLWord _, address={base=arg1, index=NONE, offset}, value=arg3}, context, _, destination) =
            let
                val baseReg = newPReg() and valueReg = newPReg()
                (* Fixed offset.  *)
                val offset = Word.toInt offset
                val code =
                    codeToICodeTarget(arg1, context, false, baseReg) @ codeToICodeTarget(arg3, context, false, valueReg) @
                        [MoveArgument {source=valueReg, dest=MemoryLocation{offset=offset, base=baseReg, index=NoMemIndex}, kind=WordToWord}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreMLWord _, address={base=arg1, index=SOME arg2, offset}, value=arg3}, context, _, destination) =
            let
                val baseReg = newPReg() and indexReg = newPReg() and valueReg = newPReg()
                (* Variable offset.  We don't have to untag the index.  Instead we can adjust the
                   index factor and offset.  *)
                val destArg =
                    if isX64
                    then MemoryLocation{base=baseReg, offset=Word.toInt offset-4, index= MemIndex4 indexReg}
                    else MemoryLocation{base=baseReg, offset=Word.toInt offset-2, index= MemIndex2 indexReg}
                val code =
                    codeToICodeTarget(arg1, context, false, baseReg) @ codeToICodeTarget(arg2, context, false, indexReg) @
                        codeToICodeTarget(arg3, context, false, valueReg) @
                        [MoveArgument {source=valueReg, dest=destArg, kind=WordToWord}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreMLByte _, address={base=arg1, index=NONE, offset}, value=arg3}, context, _, destination) =
            let
                (* We have to untag the value to store. *)
                val baseReg = newPReg() and valueReg = newPReg() and valueReg1 = newUReg()
                val code =
                    codeToICodeTarget(arg1, context, false, baseReg) @
                    codeToICodeTarget(arg3, context, false, valueReg) @
                    [UntagValue{dest=valueReg1, source=valueReg, isSigned=false },
                     MoveArgument {source=valueReg1, dest=MemoryLocation{base=baseReg, offset=Word.toInt offset, index=NoMemIndex}, kind=ByteToByte}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreMLByte _, address={base=arg1, index=SOME arg2, offset}, value=arg3}, context, _, destination) =
            let
                (* We have to untag both the index and the value to store. *)
                val baseReg = newPReg() and indexReg = newPReg() and indexReg1 = newUReg()
                and valueReg = newPReg() and valueReg1 = newUReg()
                val code =
                    codeToICodeTarget(arg1, context, false, baseReg) @ codeToICodeTarget(arg2, context, false, indexReg) @
                    codeToICodeTarget(arg3, context, false, valueReg) @
                    [UntagValue{dest=valueReg1, source=valueReg, isSigned=false },
                     UntagValue{dest=indexReg1, source=indexReg, isSigned=false },
                     MoveArgument {source=valueReg1, dest=MemoryLocation{base=baseReg, offset=Word.toInt offset, index=MemIndex1 indexReg1}, kind=ByteToByte}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICBlockOperation _, context, _, destination) =
                raise InternalError "codeToICode: BICBlockOperation TODO"

        (* Process a list of bindings. *)
        and codeBindingsToICode(bindings, context) =
        let
            fun doBinding(BICDeclar{value, addr, ...}, l) =
                let
                    val dest = newPReg()
                    val code = codeToICodeTarget(value, context, false, dest)
                    val () = Array.update(locToPregArray, addr, SOME dest)
                in
                    l @ code
                end

            |   doBinding(BICRecDecs [{lambda, addr, ...}], l) =
                (* We shouldn't have single entries in RecDecs but it seems to occur at the moment. *)
                let
                    val dest = newPReg()
                    val code = codeToICodeTarget(BICLambda lambda, context, false, dest)
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
                            then [AllocateMemoryOperation{size=sizeClosure, flags=Address.F_mutable, dest=dest}]
                            else
                            let
                                val d = newPReg()
                            in
                                MoveArgument{source=IntegerConstant(tag 0), dest=d, kind=WordToWord} ::
                                    (clear (n+1) @
                                        [MoveArgument{source=d, dest=wordOffsetAddress(n, dest), kind=WordToWord}])
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
                        fun loadFields([], _) = [MoveArgument{source=dest, dest=dstCopy, kind=WordToWord}]
                        |   loadFields(f :: rest, n) =
                            let
                                val fReg = newPReg()
                                val code = codeToICodeTarget(f, context, false, fReg)
                                val restAndAlloc = loadFields(rest, n+1)
                                val storeValue =
                                    [MoveArgument{ source=fReg, dest=wordOffsetAddress(n, dstCopy), kind=WordToWord }]
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

            |   doBinding(BICNullBinding exp, l) =
                let
                    val (code, _) = codeToICode(exp, context, false, NoResult) (* And discard result. *)
                in
                    l @ code
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
                val testCode = codeToICodeTarget(test, context, false, tagReg)
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
                val testCode = codeToICodeTarget(arg1, context, false, testReg)
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
                val testCode = codeToICodeTarget(arg1, context, false, testReg)
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
                val testCode = codeToICodeTarget(arg2, context, false, testReg)
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
                    codeToICodeTarget(arg1, context, false, arg1Reg) @
                    codeToICodeTarget(arg2, context, false, arg2Reg)
            in
                testCode @
                    [WordComparison{arg1=arg1Reg, arg2=arg2Reg, ccRef=ccRef},
                     ConditionalForwardJump{ccRef=ccRef,
                           condition=testAsBranch(test, isSigned, jumpOn), label=jumpLabel}]
            end

        |   codeCondition(BICBuiltIn2{oper=BuiltIns.LargeWordComparison test, arg1, arg2},
                    context, jumpOn, jumpLabel) =
            let
                val arg1Reg = newPReg() and arg2Reg = newPReg() and ccRef = newCCRef()
                val testCode =
                    codeToICodeTarget(arg1, context, false, arg1Reg) @
                    codeToICodeTarget(arg2, context, false, arg2Reg)
            in
                testCode @
                    [WordComparison{arg1=wordAt arg1Reg, arg2=wordAt arg2Reg, ccRef=ccRef},
                     ConditionalForwardJump{ccRef=ccRef,
                           condition=testAsBranch(test, false, jumpOn), label=jumpLabel}]
            end

        |   codeCondition(BICBuiltIn5{oper=BuiltIns.ByteVecEqual, arg1, arg2, arg3, arg4, arg5},
                    context, jumpOn, jumpLabel) =
            let
                val arg1Reg = newPReg() and arg2Reg = newPReg() and arg3Reg = newPReg()
                and arg4Reg = newPReg() and arg5Reg = newPReg()
                val untag3Reg = newUReg() and untag4Reg = newUReg()
                val vec1Reg = newUReg() and vec2Reg = newUReg() and lenReg = newUReg()
                val ccRef = newCCRef()
            in
                codeToICodeTarget(arg1, context, false, arg1Reg) @
                codeToICodeTarget(arg2, context, false, arg2Reg) @
                codeToICodeTarget(arg3, context, false, arg3Reg) @
                codeToICodeTarget(arg4, context, false, arg4Reg) @
                codeToICodeTarget(arg5, context, false, arg5Reg) @
                [UntagValue{source=arg3Reg, dest=untag3Reg, isSigned=false},
                 LoadEffectiveAddress{base=SOME arg1Reg, offset=0, index=MemIndex1 untag3Reg, dest=vec1Reg},
                 UntagValue{source=arg4Reg, dest=untag4Reg, isSigned=false},
                 LoadEffectiveAddress{base=SOME arg2Reg, offset=0, index=MemIndex1 untag4Reg, dest=vec2Reg},
                 UntagValue{source=arg5Reg, dest=lenReg, isSigned=false},
                 CompareByteVectors{ vec1Addr=vec1Reg, vec2Addr=vec2Reg, length=lenReg, ccRef=ccRef },
                 ConditionalForwardJump{ccRef=ccRef,
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
                val testReg = newPReg() and ccRef = newCCRef()
                val testCode = codeToICodeTarget(condition, context, false, testReg)
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
            codeToICodeTarget(body, {loopArgs=NONE}, true, resultTarget) @ returnInstruction
      
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
