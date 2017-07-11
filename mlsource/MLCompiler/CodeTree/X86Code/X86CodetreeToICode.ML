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

    val argRegs = List.map GenReg (if isX64 then [eax, ebx, r8, r9, r10] else [eax, ebx])
    val numArgRegs = List.length argRegs

    (* tag a short constant *)
    fun tag c = 2 * c + 1
  
    (* shift a short constant, but don't set tag bit *)
    fun semitag c = 2 * c

    local
        open RunCall
        val F_mutable_bytes =  Word.fromLargeWord(Word8.toLargeWord(Word8.orb (F_mutable, F_bytes)))
        fun makeRealConst l =
        let
            val r = allocateByteMemory(0wx8 div bytesPerWord, F_mutable_bytes)
            fun setBytes([], _) = ()
            |   setBytes(hd::tl, n) = (storeByte(r, n, hd); setBytes(tl, n+0wx1))
            val () = setBytes(l, 0w0)
            val () = clearMutableBit r
        in
            r
        end
    in
        (* These are floating point constants used to change and mask the sign bit. *)
        val realSignBit: machineWord = makeRealConst [0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx80]
        and realAbsMask: machineWord = makeRealConst [0wxff, 0wxff, 0wxff, 0wxff, 0wxff, 0wxff, 0wxff, 0wx7f]
    end
 
    fun codeFunctionToX86({body, localCount, name, argTypes, closure, ...}:bicLambdaForm, debugSwitches, closureOpt) =
    let
        val pregCounter = ref 0
        fun newPReg() = PReg(!pregCounter, PRegGeneral) before pregCounter := !pregCounter + 1
        fun newUReg() = PReg(!pregCounter, PRegUntagged) before pregCounter := !pregCounter + 1
        val locToPregArray = Array.array(localCount, NONE: abstract option)
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

        local
            val clReg = case closure of [] => [] | _ => [(closureRegAddr, GenReg edx)]
            val argRegs = ListPair.zip (argPRegs, argRegsUsed)
        in
            val beginInstruction = BeginFunction { regArgs = clReg @ argRegs }
        end

        (* Number of arguments on the stack and not in registers. *)
        val currentStackArgs = Int.max(0, numFunctionArgs - List.length argRegs)
        
        (* The return instruction.  This can be added on to various tails but there is always
           one at the end anyway. *)
        fun returnInstruction({stackPtr, ...}, target) =
            (if stackPtr <> 0
            then [ResetStackPtr{numWords=stackPtr}]
            else []) @
            [ReturnResultFromFunction{resultReg=target, numStackArgs=currentStackArgs}]

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
            val label = newLabel()
        in
            [
                ConditionalForwardJump{ ccRef=ccRef, condition=JNO, label=label },
                RaiseExceptionPacket{packet=AddressConstant(toMachineWord(Overflow))},
                ForwardJumpLabel{ label=label, result=NONE }
            ]
        end
        
        (* Generally we have an offset in words and no index register. *)
        fun wordOffsetAddress(offset, baseReg: abstract): abstract argument =
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
        and allowInPReg =
            { anyConstant=false, const32s=false, memAddr=false, existingPreg=true }
        (* AllowDefer can be used to ensure that any side-effects are done before
           something else but otherwise we only evaluate afterwards. *)
        and allowDefer =
            { anyConstant=true, const32s=true, memAddr=true, existingPreg=true }

        datatype destination =
            SpecificPReg of abstract
        |   NoResult
        |   Allowed of allowedArgument

        (* If a preg has been provided, use that, otherwise generate a new one. *)
        fun asTarget(SpecificPReg preg) = preg
        |   asTarget NoResult = newPReg()
        |   asTarget(Allowed _) = newPReg()

        fun moveIfNotAllowed(NoResult, code, arg) = (code, arg, false)

        |   moveIfNotAllowed(Allowed{anyConstant=true, ...}, code, arg as AddressConstant _) = (code, arg, false)
        
        |   moveIfNotAllowed(Allowed{anyConstant=true, ...}, code, arg as IntegerConstant _) = (code, arg, false)
        
        |   moveIfNotAllowed(dest as Allowed{const32s=true, ...}, code, arg as IntegerConstant value) =
            (* This is allowed if the value is within 32-bits *)
                if is32bit value
                then (code, arg, false)
                else moveToTarget(dest, code, arg)

        |   moveIfNotAllowed(dest as Allowed{const32s=true, ...}, code, arg as AddressConstant _) =
                if not isX64
                then (code, arg, false)
                else moveToTarget(dest, code, arg)

        |   moveIfNotAllowed(Allowed{existingPreg=true, ...}, code, arg as RegisterArgument(PReg _)) = (code, arg, false)
        
        |   moveIfNotAllowed(Allowed{memAddr=true, ...}, code, arg as MemoryLocation _) = (code, arg, false)

        |   moveIfNotAllowed(dest, code, arg) = moveToTarget(dest, code, arg)

        and moveToTarget(dest, code, arg) =
            let
                val target = asTarget dest
            in
                (code @ [LoadArgument{source=arg, dest=target, kind=MoveWord}], RegisterArgument target, false)
            end

        fun codeToICodeTarget(instr, context, isTail, target) =
        (* This is really for backwards compatibility.  *)
        let
            val (code, _, _) = codeToICode(instr, context, isTail, SpecificPReg target)
        in
            code
        end
        
        and codeToPReg(instr, context) =
        let (* Many instructions require an argument in a register.  If it's already in a
               register use that rather than creating a new one. *)
            val (code, result, _) = codeToICode(instr, context, false, Allowed allowInPReg)
            val preg = case result of RegisterArgument pr => pr | _ => raise InternalError "codeToPReg"
        in
            (code, preg)
        end

        (* Main function to turn the codetree into ICode.  Optimisation is generally
           left to later passes.  This does detect tail recursion.  *)
        and codeToICode(BICNewenv (bindings, exp), context as {stackPtr=initialSp, ...} , isTail, target) =
            let
                (* Process a list of bindings.  We need to accumulate the space used by
                   any containers and reset the stack pointer at the end if necessary. *)
                fun doBindings([], context) = ([], context)
                    
                |   doBindings(BICDeclar{value, addr, ...} :: decs, context) =
                    let
                        val dest = newPReg()
                        val code = codeToICodeTarget(value, context, false, dest)
                        val () = Array.update(locToPregArray, addr, SOME dest)
                        val (rest, resContext) = doBindings(decs, context)
                    in
                        (code @ rest, resContext)
                    end

                |   doBindings(BICRecDecs [{lambda, addr, ...}] :: decs, context) =
                    (* We shouldn't have single entries in RecDecs but it seems to occur at the moment. *)
                    let
                        val dest = newPReg()
                        val code = codeToICodeTarget(BICLambda lambda, context, false, dest)
                        val () = Array.update(locToPregArray, addr, SOME dest)
                        val (rest, resContext) = doBindings(decs, context)
                    in
                        (code @ rest, resContext)
                    end

                |   doBindings(BICRecDecs recDecs :: decs, context) =
                    let
                        val destRegs = map (fn _ => newPReg()) recDecs

                        (* First build the closures as mutable cells containing zeros.  Set the
                           entry in the address table to the register containing the address. *)
                        fun makeClosure({lambda={closure, ...}, addr, ...}, dest, c) =
                        let
                            val () = Array.update(locToPregArray, addr, SOME dest)
                            val sizeClosure = List.length closure + 1
                            fun clear n =
                                if n = sizeClosure
                                then [AllocateMemoryOperation{size=sizeClosure, flags=Address.F_mutable, dest=dest, saveRegs=[]}]
                                else
                                    (clear (n+1) @
                                        [StoreArgument{source=IntegerConstant(tag 0), base=dest, offset=n*wordSize, index=NoMemIndex, kind=MoveWord}])
                        in
                            c @ clear 0 @ [InitialisationComplete]
                        end
                    
                        val allocClosures = ListPair.foldlEq makeClosure [] (recDecs, destRegs)
                    
                        fun setClosure({lambda as {closure, ...}, ...}, dest, l) =
                        let
                            val codeAddr = codeFunctionToX86(lambda, debugSwitches, NONE)
                            (* Basically the same as tuple except we load the address of
                               the closure we've made.  It's complicated because
                               StoreArgument to MemoryLocation assumes that the top of the stack is
                               the address of the allocated memory and the items below
                               are the values to store. *)
                            val dstCopy = newPReg()
                            fun loadFields([], _) = [LoadArgument{source=RegisterArgument dest, dest=dstCopy, kind=MoveWord}]
                            |   loadFields(f :: rest, n) =
                                let
                                    val (code, source, _) = codeToICode(f, context, false, Allowed allowInMemMove)
                                    val restAndAlloc = loadFields(rest, n+1)
                                    val storeValue =
                                        [StoreArgument{ source=source, base=dstCopy, offset=n*wordSize, index=NoMemIndex, kind=MoveWord }]
                                in
                                    code @ restAndAlloc @ storeValue
                                end
                            val setFields = loadFields(BICConstnt(toMachineWord codeAddr, []) :: map BICExtract closure, 0)
                        in
                            l @ setFields @ [LockMutable{addr=dest}]
                        end
                        val setClosures = ListPair.foldlEq setClosure [] (recDecs, destRegs)
                        
                        val (rest, resContext) = doBindings(decs, context)
                    in
                        (allocClosures @ setClosures @ rest, resContext)
                    end

                |   doBindings(BICNullBinding exp :: decs, context) =
                    let
                        val (code, _, _) = codeToICode(exp, context, false, NoResult) (* And discard result. *)
                        val (rest, resContext) = doBindings(decs, context)
                    in
                        (code @ rest, resContext)
                    end
       
                |   doBindings(BICDecContainer{ addr, size } :: decs, {loopArgs, stackPtr}) =
                    let
                        val dest = newPReg()
                        val () = Array.update(locToPregArray, addr, SOME dest)
                        val (rest, resContext) = doBindings(decs, {loopArgs=loopArgs, stackPtr=stackPtr+size})
                    in
                        (ReserveContainer{size=size, address=dest} :: rest, resContext)
                    end

                val (codeBindings, resContext as {stackPtr=finalSp, ...}) = doBindings(bindings, context)
                val (codeExp, result, haveExited) = codeToICode(exp, resContext, isTail, target)
                val _ = finalSp >= initialSp orelse raise InternalError "codeToICode - stack ptr"
                val adjustSp =
                    if initialSp = finalSp orelse haveExited
                    then []
                    else [ResetStackPtr{numWords=finalSp-initialSp}]
            in
                (codeBindings @ codeExp @ adjustSp, result, haveExited)
            end
            
        |   codeToICode(BICConstnt(value, _), _, _, destination) =
                moveIfNotAllowed(destination, [], constantAsArgument value)

        |   codeToICode(BICExtract(BICLoadLocal l), _, _, destination) =
                moveIfNotAllowed(destination, [], RegisterArgument(valOf(Array.sub(locToPregArray, l))))

        |   codeToICode(BICExtract(BICLoadArgument a), {stackPtr, ...}, _, destination) =
            if a < numArgRegs
            then (* It was originally in a register.  It's now in a preg. *)
                moveIfNotAllowed(destination, [], RegisterArgument(List.nth(argPRegs, a)))
            else (* Pushed before call. *)
            let
                val target = asTarget destination
            in
                ([LoadArgument{
                    source=StackLocation{wordOffset=List.length argTypes - a, adjustment=stackPtr}, dest=target, kind=MoveWord}],
                 RegisterArgument target, false)
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
                    |   _ => RegisterArgument closureRegAddr)
        
        |   codeToICode(BICField{base, offset}, context, _, destination) =
            let
                val (codeBase, baseR) = codeToPReg(base, context)
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
                            else if flags addrAsAddr <> Address.F_words
                            then ([LoadArgument{source=AddressConstant addr, dest=clPReg, kind=MoveWord}],
                                      [(RegisterArgument clPReg, GenReg edx)], FullCall)
                            else
                            let
                            
                                val addrLength = length addrAsAddr
                                val _ = addrLength >= 0w1 orelse raise InternalError "BICEval address"
                                val codeAddr = loadWord(addrAsAddr, 0w0)
                                val _ = isCode (toAddress codeAddr) orelse raise InternalError "BICEval address not code"
                            in
                                if addrLength = 0w1
                                then ([], [], ConstantCode codeAddr)
                                else ([LoadArgument{source=AddressConstant addr, dest=clPReg, kind=MoveWord}],
                                      [(RegisterArgument clPReg, GenReg edx)], ConstantCode codeAddr)
                            end
                        end

                    |   BICExtract BICLoadRecursive =>
                        (
                            (* If the closure is empty we don't need to load rdx *)
                            case closure of
                                [] => ([], [], Recursive)
                            |   _ => ([LoadArgument {source=RegisterArgument closureRegAddr, dest=clPReg, kind=MoveWord}], [(RegisterArgument clPReg, GenReg edx)], Recursive)
                        )

                    |   function => (* General case. *)
                            (codeToICodeTarget(function, context, false, clPReg), [(RegisterArgument clPReg, GenReg edx)], FullCall)
                (* Optimise arguments.  We have to be careful with tail-recursive functions because they
                   need to save any stack arguments that could be overwritten.  This is complicated
                   because we overwrite the stack before loading the register arguments.  In some
                   circumstances it could be safe but for the moment leave it.  This should be safe
                   in the new code-transform but not the old codeICode.
                   Currently we don't allow memory arguments at all.  There's the potential for
                   problems later.  Memory arguments could possibly lead to aliasing of the stack
                   if the memory actually refers to a container on the stack.  That would mess
                   up the code that ensures that stack arguments are stored in the right order.
                   Don't allow long constants in tail calls.  We may move arguments directly
                   to their stack location so we're dealing with a memory move there.  *)
                val allowDefer =
                    Allowed {anyConstant=not isTail, const32s=true, memAddr=false, existingPreg=not isTail }
                fun loadArgs [] = ([], [])
                |   loadArgs((arg, _)::args) =
                    let
                        val (c, r, _) = codeToICode(arg, context, false, allowDefer)
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
                val callInstr =
                    if isTail
                    then
                    let
                        val {stackPtr, ...} = context
                        (* The number of arguments currently on the stack. *)
                        val currentStackArgCount = Int.max(0, numFunctionArgs - numArgRegs)
                        val newStackArgCount = List.length stackArgs
                        (* The offset of the first argument or the return address if there are
                           no stack arguments.  N.B. We actually have currentStackArgCount+1
                           items on the stack including the return address.  Offsets can be
                           negative. *)
                        val firstArgumentAddr = stackPtr + currentStackArgCount
                        fun makeStackArgs([], _) = []
                        |   makeStackArgs(arg::args, offset) = {src=arg, stack=offset} :: makeStackArgs(args, offset-1)
                        val stackArgs = makeStackArgs(stackArgs, firstArgumentAddr)
                        (* The stack adjustment needed to compensate for any items that have been pushed
                           and the differences in the number of arguments.  May be positive or negative.
                           This is also the destination address of the return address so when we enter
                           the new function the return address will be the first item on the stack. *)
                        val stackAdjust = firstArgumentAddr - newStackArgCount
                    in
                        TailRecursiveCall{regArgs=closureEntry @ regArgs, stackArgs=stackArgs,
                                  returnAddr={srcStack=stackPtr, stack=stackAdjust}, stackAdjust = stackAdjust, callKind=callKind}
                    end
                    else FunctionCall{regArgs=closureEntry @ regArgs, stackArgs=stackArgs, dest=target, callKind=callKind}
            in
                (functionCode @ codeArgs @ [callInstr], RegisterArgument target, isTail (* We've exited if this was a tail jump *))
            end

        |   codeToICode(BICGetThreadId, _, _, destination) =
            (* Get the ID of the current thread. *)
            let
                val target = asTarget destination
            in
                ([LoadMemReg{offset=memRegThreadSelf, dest=target}], RegisterArgument target, false)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.NotBoolean, arg1}, context, _, destination) =
            let
                (* TODO: If the argument is something that will be a conditional we would be better off
                   generating a conditional here. *)
                val target = asTarget destination
                val ccRef = newCCRef()
                val (argCode, tReg) = codeToPReg(arg1, context)
            in
                (argCode @
                    [ArithmeticFunction{oper=XOR, resultReg=target, operand1=RegisterArgument tReg, operand2=IntegerConstant(semitag 1), ccRef=ccRef}], RegisterArgument target, false)
            end

        |   codeToICode(instr as BICUnary{oper=BuiltIns.IsTaggedValue, ...}, context, isTail, destination) =
                codeAsConditional(instr, context, isTail, destination)

        |   codeToICode(BICUnary{oper=BuiltIns.MemoryCellLength, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val argReg1 = newPReg() and argReg2 = newPReg() and argReg3 = newPReg()
                and ccRef1 = newCCRef() and ccRef2 = newCCRef() and ccRef3 = newCCRef()
                (* Get the length of a memory cell (heap object).  We need to mask out the
                   top byte containing the flags and to tag the result.  The mask is 56 bits on
                   64-bit which won't fit in an inline constant.  Since we have to shift it
                   anyway we might as well do this by shifts. *)
                val (argCode, addrReg) = codeToPReg(arg1, context)
            in
                (argCode @
                [LoadArgument{source=wordOffsetAddress(~1, addrReg), dest=argReg1, kind=MoveWord},
                 ShiftOperation{shift=SHL, resultReg=argReg2, operand=RegisterArgument argReg1, shiftAmount=IntegerConstant 8, ccRef=ccRef1 },
                 ShiftOperation{shift=SHR, resultReg=argReg3, operand=RegisterArgument argReg2, shiftAmount=IntegerConstant 7 (* 8-tagshift*), ccRef=ccRef2 },
                 ArithmeticFunction{oper=OR, resultReg=target, operand1=RegisterArgument argReg3, operand2=IntegerConstant 1, ccRef=ccRef3}], RegisterArgument target, false)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.MemoryCellFlags, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val argReg1 = newUReg()
                val (argCode, addrReg) = codeToPReg(arg1, context)
            in
                (argCode @
                [LoadArgument{source=MemoryLocation{offset= ~1, base=addrReg, index=NoMemIndex}, dest=argReg1, kind=MoveByte},
                 TagValue{ source=argReg1, dest=target }], RegisterArgument target, false)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.ClearMutableFlag, arg1}, context, _, destination) =
            let
                val (argCode, addrReg) = codeToPReg(arg1, context)
                val code = argCode @ [LockMutable{addr=addrReg}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICUnary{oper=BuiltIns.AtomicIncrement, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val incrReg = newUReg()
                val (argCode, addrReg) = codeToPReg(arg1, context)
            in
                (argCode @
                 [LoadArgument{source=IntegerConstant(semitag 1), dest=incrReg, kind=MoveWord},
                  AtomicExchangeAndAdd{ base=addrReg, source=incrReg },
                  (* We want the result to be the new value but we've returned the old value. *)
                  ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument incrReg, operand2=IntegerConstant(semitag 1), ccRef=newCCRef()}], RegisterArgument target, false)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.AtomicDecrement, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val incrReg = newUReg()
                val (argCode, addrReg) = codeToPReg(arg1, context)
            in
                (argCode @
                 [LoadArgument{source=IntegerConstant(semitag ~1), dest=incrReg, kind=MoveWord},
                  AtomicExchangeAndAdd{ base=addrReg, source=incrReg },
                  ArithmeticFunction{oper=SUB, resultReg=target, operand1=RegisterArgument incrReg, operand2=IntegerConstant(semitag 1), ccRef=newCCRef()}], RegisterArgument target, false)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.AtomicReset, arg1}, context, _, destination) =
            let
                (* This is needed only for the interpreted version where we have a single real
                   mutex to interlock atomic increment and decrement.  We have to use the same
                   mutex to interlock clearing a mutex.  On the X86 we use hardware locking and
                   the hardware guarantees that an assignment of a word will be atomic. *)
                val (argCode, addrReg) = codeToPReg(arg1, context)
                val code = argCode @ [LockMutable{addr=addrReg}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICUnary{oper=BuiltIns.LongWordToTagged, arg1}, context, _, destination) =
            let (* This is exactly the same as StringLengthWord at the moment.
                   TODO: introduce a new ICode entry so that the next stage can optimise
                   longword operations. *)
                val target = asTarget destination
                val argReg1 = newUReg()
                val (argCode, addrReg) = codeToPReg(arg1, context)
            in
                (argCode @
                [LoadArgument{source=MemoryLocation{offset=0, base=addrReg, index=NoMemIndex}, dest=argReg1, kind=MoveWord},
                 TagValue{ source=argReg1, dest=target }], RegisterArgument target, false)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.SignedToLongWord, arg1}, context, _, destination) =
            let
                val addrReg = newPReg() and argReg1 = newPReg() and untagArg = newUReg()
                val code =
                    codeToICodeTarget(arg1, context, false, argReg1) @
                    [UntagValue{source=RegisterArgument argReg1, dest=untagArg, isSigned=true},
                     BoxValue{boxKind=BoxLargeWord,  source=untagArg, dest=addrReg, saveRegs=[]}]
            in
                moveIfNotAllowed(destination, code, RegisterArgument addrReg)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.UnsignedToLongWord, arg1}, context, _, destination) =
            let
                val addrReg = newPReg() and argReg1 = newPReg() and untagArg = newUReg()
                val code =
                    codeToICodeTarget(arg1, context, false, argReg1) @
                    [UntagValue{source=RegisterArgument argReg1, dest=untagArg, isSigned=false},
                     BoxValue{boxKind=BoxLargeWord,  source=untagArg, dest=addrReg, saveRegs=[]}]
            in
                moveIfNotAllowed(destination, code, RegisterArgument addrReg)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.RealNeg, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and fpRegSrc = newUReg() and fpRegDest = newUReg() and sse2ConstReg = newUReg()
                (* The SSE2 code uses an SSE2 logical operation to flip the sign bit.  This
                   requires the values to be loaded into registers first because the logical
                   operations require 128-bit operands. *)
                val load = LoadArgument{source=wordAt aReg1, dest=fpRegSrc, kind=MoveDouble}
                val code =
                    case fpMode of
                        FPModeX87 =>
                            [X87FPUnaryOps{ fpOp=FCHS, dest=fpRegDest, source=RegisterArgument fpRegSrc}]
                    |   FPModeSSE2 =>
                            [LoadArgument{source=AddressConstant realSignBit, dest=sse2ConstReg, kind=MoveDouble},
                             SSE2FPArith{opc=SSE2Xor, resultReg=fpRegDest, arg1=RegisterArgument fpRegSrc, arg2=RegisterArgument sse2ConstReg}]
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @
                 [load] @ code @ [BoxValue{boxKind=BoxFloat, source=fpRegDest, dest=target, saveRegs=[]}], RegisterArgument target, false)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.RealAbs, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and fpRegSrc = newUReg() and fpRegDest = newUReg() and sse2ConstReg = newUReg()
                val load = LoadArgument{source=wordAt aReg1, dest=fpRegSrc, kind=MoveDouble}
                val code =
                    case fpMode of
                        FPModeX87 => [X87FPUnaryOps{ fpOp=FABS, dest=fpRegDest, source=RegisterArgument fpRegSrc}]
                    |   FPModeSSE2 =>
                            [LoadArgument{source=AddressConstant realAbsMask, dest=sse2ConstReg, kind=MoveDouble},
                            SSE2FPArith{opc=SSE2And, resultReg=fpRegDest, arg1=RegisterArgument fpRegSrc, arg2=RegisterArgument sse2ConstReg}]
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @
                 [load] @ code @ [BoxValue{boxKind=BoxFloat, source=fpRegDest, dest=target, saveRegs=[]}], RegisterArgument target, false)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.FloatFixedInt, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and untagReg = newUReg() and fpReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @
                 [UntagValue{source=RegisterArgument aReg1, dest=untagReg, isSigned=true},
                  FloatFixedInt{ dest=fpReg, source=RegisterArgument untagReg},
                  BoxValue{boxKind=BoxFloat, source=fpReg, dest=target, saveRegs=[]}], RegisterArgument target, false)
            end

        |   codeToICode(instr as BICBinary{oper=BuiltIns.WordComparison _, ...}, context, isTail, destination) =
                codeAsConditional(instr, context, isTail, destination)

        |   codeToICode(BICBinary {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithAdd, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
            let
                val target = asTarget destination
                val ccRef = newCCRef()
                (* If the argument is a constant we can subtract the tag beforehand.
                   This should always be a tagged value if the type is correct.  However it's possible for it not to
                   be if we have an arbitrary precision value.  There will be a run-time check that the value is
                   short and so this code will never be executed.  It will generally be edited out by the higher
                   level be we can't rely on that.  Because it's never executed we can just put in zero. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
            in
                (arg1Code @
                    [ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument aReg1, operand2=IntegerConstant constVal, ccRef=ccRef}] @
                    checkOverflow (ccRef, target), RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithAdd, arg1=BICConstnt(value, _), arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val ccRef = newCCRef()
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
            in
                (arg2Code @
                    [ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument aReg2, operand2=IntegerConstant constVal, ccRef=ccRef}] @
                    checkOverflow (ccRef, target), RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithAdd, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg3 = newPReg() and ccRef = newCCRef()
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
            in
                (arg1Code @ arg2Code @
                    (* Subtract the tag bit from the second argument, do the addition and check for overflow. *)
                    (* TODO: We should really do the detagging in the transform phase.  It can make a better choice of
                       the argument if one of the arguments is already untagged or if we have a constant argument. *)
                    [ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=RegisterArgument aReg2, operand2=IntegerConstant 1, ccRef=newCCRef()},
                     ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument aReg1, operand2=RegisterArgument aReg3, ccRef=ccRef}] @
                    checkOverflow (ccRef, target), RegisterArgument target, false)
            end

            (* Subtraction.  We can handle the special case of the second argument being a constant but not the first. *)
        |   codeToICode(BICBinary {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithSub, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
            let
                val target = asTarget destination
                val ccRef = newCCRef()
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
            in
                (arg1Code @
                    [ArithmeticFunction{oper=SUB, resultReg=target, operand1=RegisterArgument aReg1, operand2=IntegerConstant constVal, ccRef=ccRef}] @
                    checkOverflow (ccRef, target), RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithSub, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg3 = newPReg()
                val ccRef = newCCRef()
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
            in
                (arg1Code @ arg2Code @
                    (* Do the subtraction, test for overflow and afterwards add in the tag bit. *)
                    [ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=RegisterArgument aReg1, operand2=RegisterArgument aReg2, ccRef=ccRef}] @
                    checkOverflow (ccRef, target) @
                    [ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument aReg3, operand2=IntegerConstant 1, ccRef=newCCRef()}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithMult, arg1, arg2}, context, _, destination) =
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
                    [UntagValue{source=RegisterArgument aReg1, dest=arg1Untagged, isSigned=true (* Signed shift here. *)},
                     ArithmeticFunction{oper=SUB, resultReg=arg2Untagged, operand1=RegisterArgument aReg2, operand2=IntegerConstant 1, ccRef=newCCRef()},
                     Multiplication{resultReg=resUntagged, operand1=RegisterArgument arg1Untagged, operand2=RegisterArgument arg2Untagged, ccRef=mulCC} ] @
                     checkOverflow(mulCC, target) @
                     [ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument resUntagged, operand2=IntegerConstant 1, ccRef=newCCRef()}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithQuot, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift both of the arguments to remove the tags.  We don't test for zero here - that's done explicitly. *)
                    [UntagValue{source=RegisterArgument aReg1, dest=arg1Untagged, isSigned=true},
                     UntagValue{source=RegisterArgument aReg2, dest=arg2Untagged, isSigned=true},
                     Division { isSigned = true, dividend=arg1Untagged, divisor=RegisterArgument arg2Untagged,
                                quotient=quotient, remainder=remainder },
                     TagValue { source=quotient, dest=target }], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithRem, arg1, arg2}, context, _, destination) =
            let
                (* Identical to Quot except that the result is the remainder. *)
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift both of the arguments to remove the tags. *)
                    [UntagValue{source=RegisterArgument aReg1, dest=arg1Untagged, isSigned=true},
                     UntagValue{source=RegisterArgument aReg2, dest=arg2Untagged, isSigned=true},
                     Division { isSigned = true, dividend=arg1Untagged, divisor=RegisterArgument arg2Untagged,
                                quotient=quotient, remainder=remainder },
                     TagValue { source=remainder, dest=target }], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.FixedPrecisionArith _, ...}, _, _, _) =
                raise InternalError "codeToICode: FixedPrecisionArith - unimplemented operation"

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
            let
                val target = asTarget destination
                (* If the argument is a constant we can subtract the tag beforehand.
                   N.B. it is possible to have type-incorrect values in dead code. i.e. code that will
                   never be executed because of a run-time check.  *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
            in
                (arg1Code @
                    [ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument aReg1, operand2=IntegerConstant constVal, ccRef = newCCRef()}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1=BICConstnt(value, _), arg2}, context, _, destination) =
            let
                val target = asTarget destination
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
            in
                (arg2Code @
                    [ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument aReg2, operand2=IntegerConstant constVal, ccRef = newCCRef()}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg3 = newPReg()
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
            in
                (arg1Code @ arg2Code @
                    (* Subtract the tag bit from the second argument and do the addition.  No need for overflow check. *)
                    [ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=RegisterArgument aReg2, operand2=IntegerConstant 1, ccRef=newCCRef()},
                     ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument aReg1, operand2=RegisterArgument aReg3, ccRef=newCCRef()}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith BuiltIns.ArithSub, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
            let
                val target = asTarget destination
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
            in
                (arg1Code @
                    [ArithmeticFunction{oper=SUB, resultReg=target, operand1=RegisterArgument aReg1, operand2=IntegerConstant constVal, ccRef=newCCRef()}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith BuiltIns.ArithSub, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg3 = newPReg()
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
            in
                (arg1Code @ arg2Code @
                    (* Do the subtraction and add in the tag bit.  This could be reordered if we have cascaded operations
                       since we don't need to check for overflow. *)
                    [ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=RegisterArgument aReg1, operand2=RegisterArgument aReg2, ccRef=newCCRef()},
                     ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument aReg3, operand2=IntegerConstant 1, ccRef=newCCRef()}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith BuiltIns.ArithMult, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and arg1Untagged = newUReg()
                and arg2Untagged = newUReg() and resUntagged = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift one argument and subtract the tag from the other.  It's possible this could be reordered
                       if we have a value that is already untagged. *)
                    [UntagValue{source=RegisterArgument aReg1, dest=arg1Untagged, isSigned=false},
                     ArithmeticFunction{oper=SUB, resultReg=arg2Untagged, operand1=RegisterArgument aReg2, operand2=IntegerConstant 1, ccRef=newCCRef()},
                     Multiplication{resultReg=resUntagged, operand1=RegisterArgument arg1Untagged, operand2=RegisterArgument arg2Untagged, ccRef=newCCRef()},
                     ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument resUntagged, operand2=IntegerConstant 1, ccRef=newCCRef()}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith BuiltIns.ArithDiv, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift both of the arguments to remove the tags.  We don't test for zero here - that's done explicitly. *)
                    [UntagValue{source=RegisterArgument aReg1, dest=arg1Untagged, isSigned=false},
                     UntagValue{source=RegisterArgument aReg2, dest=arg2Untagged, isSigned=false},
                     Division { isSigned = false, dividend=arg1Untagged, divisor=RegisterArgument arg2Untagged,
                                quotient=quotient, remainder=remainder },
                     TagValue { source=quotient, dest=target }], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith BuiltIns.ArithMod, arg1, arg2}, context, _, destination) =
            let
                (* Identical to Quot except that the result is the remainder. *)
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift both of the arguments to remove the tags. *)
                    [UntagValue{source=RegisterArgument aReg1, dest=arg1Untagged, isSigned=false},
                     UntagValue{source=RegisterArgument aReg2, dest=arg2Untagged, isSigned=false},
                     Division { isSigned = false, dividend=arg1Untagged, divisor=RegisterArgument arg2Untagged,
                                quotient=quotient, remainder=remainder },
                     TagValue { source=remainder, dest=target }], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith _, ...}, _, _, _) =
                raise InternalError "codeToICode: WordArith - unimplemented operation"

        |   codeToICode(BICBinary {oper=BuiltIns.WordLogical BuiltIns.LogicalOr, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Or-ing preserves the tag bit. *)
                    [ArithmeticFunction{oper=OR, resultReg=target, operand1=RegisterArgument aReg1, operand2=RegisterArgument aReg2, ccRef=newCCRef()}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordLogical BuiltIns.LogicalAnd, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Since they're both tagged the result will be tagged. *)
                    [ArithmeticFunction{oper=AND, resultReg=target, operand1=RegisterArgument aReg1, operand2=RegisterArgument aReg2, ccRef=newCCRef()}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordLogical BuiltIns.LogicalXor, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newPReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* We need to restore the tag bit after the operation. *)
                    [ArithmeticFunction{oper=XOR, resultReg=aReg3, operand1=RegisterArgument aReg1, operand2=RegisterArgument aReg2, ccRef=newCCRef()},
                     ArithmeticFunction{oper=OR, resultReg=target, operand1=RegisterArgument aReg3, operand2=IntegerConstant 1, ccRef=newCCRef()}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordShift shift, arg1, arg2}, context, _, destination) =
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
                 [UntagValue{source=RegisterArgument aReg1, dest=aReg1Untagged, isSigned=shift=ShiftRightArithmetic},
                  UntagValue{source=RegisterArgument aReg2, dest=aReg2Untagged, isSigned=false},
                  ShiftOperation{ shift=shiftOp, resultReg=resRegUntagged, operand=RegisterArgument aReg1Untagged, shiftAmount=RegisterArgument aReg2Untagged, ccRef=newCCRef() },
                  TagValue{source=resRegUntagged, dest=target}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.AllocateByteMemory, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val sizeReg = newPReg() and baseReg = newPReg()
                val sizeCode = codeToICodeTarget(arg1, context, false, sizeReg)
                val (flagsCode, flagUntag, flagArg) = codeAsUntaggedValue(arg2, false, context)
            in
                (sizeCode @ flagsCode @
                 [AllocateMemoryVariable{size=sizeReg, dest=baseReg, saveRegs=[]}] @
                  flagUntag @
                  [StoreArgument{ source=flagArg, base=baseReg, offset= ~1, index=NoMemIndex, kind=MoveByte},
                  InitialisationComplete,
                  LoadArgument{ source=RegisterArgument baseReg, dest=target, kind=MoveWord}], RegisterArgument target, false)
            end

        |   codeToICode(instr as BICBinary{oper=BuiltIns.LargeWordComparison _, ...}, context, isTail, destination) =
                codeAsConditional(instr, context, isTail, destination)

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordArith BuiltIns.ArithAdd, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newUReg()
                val argReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord},
                     ArithmeticFunction{oper=ADD, resultReg=aReg3, operand1=RegisterArgument argReg, operand2=wordAt aReg2, ccRef=newCCRef()},
                     BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordArith BuiltIns.ArithSub, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newUReg()
                val argReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord},
                     ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=RegisterArgument argReg, operand2=wordAt aReg2, ccRef=newCCRef()},
                     BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordArith BuiltIns.ArithMult, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and resValue = newUReg()
                val argReg1 = newUReg() and argReg2 = newUReg()
                (* Temporarily load both arguments into untagged regs.  It's simpler. *)
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [LoadArgument{source=wordAt aReg1, dest=argReg1, kind=MoveWord},
                     LoadArgument{source=wordAt aReg2, dest=argReg2, kind=MoveWord},
                     Multiplication{resultReg=resValue, operand1=RegisterArgument argReg1, operand2=RegisterArgument argReg2, ccRef=newCCRef()},
                     BoxValue{boxKind=BoxLargeWord, source=resValue, dest=target, saveRegs=[]}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordArith BuiltIns.ArithDiv, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val quotient = newUReg() and remainder = newUReg()
                val dividendReg = newUReg() and divisorReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* We don't test for zero here - that's done explicitly. *)
                    [LoadArgument{source=wordAt aReg1, dest=dividendReg, kind=MoveWord},
                     LoadArgument{source=wordAt aReg2, dest=divisorReg, kind=MoveWord},
                     Division { isSigned = false, dividend=dividendReg, divisor=RegisterArgument divisorReg,
                                quotient=quotient, remainder=remainder },
                     BoxValue{boxKind=BoxLargeWord, source=quotient, dest=target, saveRegs=[]}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordArith BuiltIns.ArithMod, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val quotient = newUReg() and remainder = newUReg()
                val dividendReg = newUReg() and divisorReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* We don't test for zero here - that's done explicitly. *)
                    [LoadArgument{source=wordAt aReg1, dest=dividendReg, kind=MoveWord},
                     LoadArgument{source=wordAt aReg2, dest=divisorReg, kind=MoveWord},
                     Division { isSigned = false, dividend=dividendReg, divisor=RegisterArgument divisorReg,
                                quotient=quotient, remainder=remainder },
                     BoxValue{boxKind=BoxLargeWord, source=remainder, dest=target, saveRegs=[]}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordArith _, ...}, _, _, _) =
                raise InternalError "codeToICode: LargeWordArith - unimplemented operation"

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordLogical BuiltIns.LogicalOr, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newUReg()
                val argReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord},
                     ArithmeticFunction{oper=OR, resultReg=aReg3, operand1=RegisterArgument argReg, operand2=wordAt aReg2, ccRef=newCCRef()},
                     BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordLogical BuiltIns.LogicalAnd, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newUReg()
                val argReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord},
                     ArithmeticFunction{oper=AND, resultReg=aReg3, operand1=RegisterArgument argReg, operand2=wordAt aReg2, ccRef=newCCRef()},
                     BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordLogical BuiltIns.LogicalXor, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newUReg()
                val argReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord},
                     ArithmeticFunction{oper=XOR, resultReg=aReg3, operand1=RegisterArgument argReg, operand2=wordAt aReg2, ccRef=newCCRef()},
                     BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordShift shift, arg1, arg2}, context, _, destination) =
                (* The shift is always a Word.word value i.e. tagged.  There is a check at the higher level
                   that the shift does not exceed 32/64 bits. *)
            let
                open BuiltIns
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val aReg2Untagged = newUReg() and aReg3 = newUReg()
                val shiftOp = case shift of ShiftLeft => SHL | ShiftRightLogical => SHR | ShiftRightArithmetic => SAR
                val argReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                 [LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord},
                  UntagValue{source=RegisterArgument aReg2, dest=aReg2Untagged, isSigned=false},
                  ShiftOperation{ shift=shiftOp, resultReg=aReg3, operand=RegisterArgument argReg, shiftAmount=RegisterArgument aReg2Untagged, ccRef=newCCRef() },
                  BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]}], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.RealArith fpOper, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val fpRegSrc = newUReg() and fpRegDest = newUReg()
                val load = LoadArgument{source=wordAt aReg1, dest=fpRegSrc, kind=MoveDouble}
                open BuiltIns
                val arith =
                    case fpMode of
                        FPModeX87 =>
                        let
                            val fpOp =
                                case fpOper of
                                    ArithAdd => FADD
                                |   ArithSub => FSUB
                                |   ArithMult => FMUL
                                |   ArithDiv => FDIV
                                |   _ => raise InternalError "codeToICode: RealArith - unimplemented operation"
                        in
                            X87FPArith{ opc=fpOp, resultReg=fpRegDest, arg1=RegisterArgument fpRegSrc, arg2=wordAt aReg2}
                        end
                    |   FPModeSSE2 =>
                        let
                            val fpOp =
                                case fpOper of
                                    ArithAdd => SSE2Add
                                |   ArithSub => SSE2Sub
                                |   ArithMult => SSE2Mul
                                |   ArithDiv => SSE2Div
                                |   _ => raise InternalError "codeToICode: RealArith - unimplemented operation"
                        in
                            SSE2FPArith{ opc=fpOp, resultReg=fpRegDest, arg1=RegisterArgument fpRegSrc, arg2=wordAt aReg2}
                        end
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [load, arith, BoxValue{boxKind=BoxFloat, source=fpRegDest, dest=target, saveRegs=[]}], RegisterArgument target, false)
            end

        |   codeToICode(instr as BICBinary{oper=BuiltIns.RealComparison _, ...}, context, isTail, destination) =
                codeAsConditional(instr, context, isTail, destination)

        |   codeToICode(BICAllocateWordMemory {numWords as BICConstnt(length, _), flags as BICConstnt(flagValue, _), initial}, context, _, destination) =
            (* Constant length and flags is used for ref.  We could handle other cases. *)
            if  isShort length andalso isShort flagValue andalso toShort length = 0w1
            then
            let
                val target = asTarget destination (* Force a different register. *)
                val vecLength = Word.toInt(toShort length)
                val flagByte = Word8.fromLargeWord(Word.toLargeWord(toShort flagValue))
                val memAddr = newPReg() and valueReg = newPReg()
                fun initialise n =
                    StoreArgument{ source=RegisterArgument valueReg, offset=n*wordSize, base=memAddr, index=NoMemIndex, kind=MoveWord}
                val code =
                    codeToICodeTarget(initial, context, false, valueReg) @
                    [AllocateMemoryOperation{size=vecLength, flags=flagByte, dest=memAddr, saveRegs=[]}] @
                    List.tabulate(vecLength, initialise) @
                    [InitialisationComplete,
                     LoadArgument{source=RegisterArgument memAddr, dest=target, kind=MoveWord}]
            in
                (code, RegisterArgument target, false)
            end
            else (* If it's longer use the full run-time form. *)
                allocateMemoryVariable(numWords, flags, initial, context, destination)

        |   codeToICode(BICAllocateWordMemory {numWords, flags, initial}, context, _, destination) =
                allocateMemoryVariable(numWords, flags, initial, context, destination)

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
                   may be able to optimise special cases.  *)
                case (codeToICode(thenPt, context, isTail, NoResult),
                      codeToICode(elsePt, context, isTail, NoResult)) of
                    (([], _, _), (elseCode, _, _)) =>
                        let
                            val skipElse = newLabel()
                            val testCode = codeCondition(test, context, true (* Invert the test. *), skipElse)
                        in
                            (testCode @ elseCode @ [ForwardJumpLabel{label=skipElse, result=NONE}], (* Unit result *) IntegerConstant(tag 0), false)
                        end
                |   ((thenCode, _, _), ([], _, _)) =>
                        let
                            val skipThen = newLabel()
                            val testCode = codeCondition(test, context, false, skipThen)
                        in
                            (testCode @ thenCode @ [ForwardJumpLabel{label=skipThen, result=NONE}], (* Unit result *) IntegerConstant(tag 0), false)
                        end
                |   ((thenCode, _, _), (elseCode, _, _)) =>
                        let
                            val startElse = newLabel() and skipElse = newLabel()
                            val testCode = codeCondition(test, context, false, startElse)
                        in
                            (testCode @
                                thenCode @ [UnconditionalForwardJump{label=skipElse}, ForwardJumpLabel{label=startElse, result=NONE}] @
                                elseCode @ [ForwardJumpLabel{label=skipElse, result=NONE}], (* Unit result *) IntegerConstant(tag 0), false)
                        end
            )

        |   codeToICode(BICCond(test, thenPt, elsePt), context, isTail, destination) =
            let
                (* Because we may push the result onto the stack we have to create a new preg to
                   hold the result and then copy that to the final result. *)
                (* If this is a tail each arm will exit separately and neither will return a result. *)
                val target = asTarget destination
                val condResult = newPReg()
                val thenTarget = if isTail then newPReg() else condResult
                val startElse = newLabel()
                val testCode = codeCondition(test, context, false, startElse)
                (* Put the result in the target register. *)
                val (thenCode, _, thenExited) = codeToICode(thenPt, context, isTail, SpecificPReg thenTarget)
                (* Add a jump round the else-part except that if this is a tail we
                   return.  The then-part could have exited e.g. with a raise or a loop. *)
                val (exitThen, thenLabel, elseTarget) =
                    if thenExited then ([], [], target (* Can use original target. *))
                    else if isTail then (returnInstruction(context, thenTarget), [], newPReg())
                    else
                    let
                        val skipElse = newLabel()
                    in
                        ([UnconditionalForwardJump{label=skipElse}],
                         [ForwardJumpLabel{label=skipElse, result=SOME condResult},
                          LoadArgument{source=RegisterArgument condResult, dest=target, kind=MoveWord}],
                         condResult)
                    end
                val (elseCode, _, elseExited) = codeToICode(elsePt, context, isTail, SpecificPReg elseTarget)
                (* Add a return to the else-part if necessary so we will always exit on a tail. *)
                val exitElse =
                    if isTail andalso not elseExited then returnInstruction(context, elseTarget) else []
            in
                (testCode @
                    thenCode @ exitThen @ [ForwardJumpLabel{label=startElse, result=NONE}] @
                    elseCode @ exitElse @ thenLabel, RegisterArgument target, isTail orelse thenExited andalso elseExited)
            end
       
        |   codeToICode(BICCase { cases, test, caseType, default}, context, isTail, destination) =
            let
                (* We have to create a new preg for the result in case we need to push
                   it to the stack. *)
                val targetReg = newPReg()
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
                                    LoadArgument {source=RegisterArgument testReg, dest=tReg, kind=MoveWord},
                                    TestTagBit{arg=RegisterArgument tReg, ccRef=ccRef},
                                    (* Jump if the value is long. *)
                                    ConditionalForwardJump{ccRef=ccRef, condition=JE, label=defLab}
                                 ], [defLab])
                            end
                        val defLab1 = newLabel() and defLab2 = newLabel()
                        val tReg1 = newPReg() and tReg2 = newPReg()
                        val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                        val testCode =
                            [
                                LoadArgument {source=RegisterArgument testReg, dest=tReg1, kind=MoveWord},
                                WordComparison{arg1=RegisterArgument tReg1, arg2=IntegerConstant(tag(Word.toLargeInt max)), ccRef=ccRef1},
                                ConditionalForwardJump{ccRef=ccRef1, condition=JA, label=defLab1},
                                LoadArgument {source=RegisterArgument testReg, dest=tReg2, kind=MoveWord},
                                WordComparison{arg1=RegisterArgument tReg1, arg2=IntegerConstant(tag(Word.toLargeInt min)), ccRef=ccRef2},
                                ConditionalForwardJump{ccRef=ccRef2, condition=JB, label=defLab2}
                            ]
                    in
                        (testTag @ testCode, defLab1 :: defLab2 :: tagDefault)
                    end

                (* Generate the code for each of the cases and the default.  We need to put an
                   unconditional branch after each to skip the other cases.
                   TODO: This could be replaced by "returns" if we're at the tail. *)
                fun codeCases ((startLabel, SOME c, _) :: otherCases) =
                    let
                        val caseTarget = if isTail then newPReg() else targetReg
                        (* Put in the case with a jump to the end of the sequence. *)
                        val (codeThisCase, _, caseExited) = codeToICode(c, context, isTail, SpecificPReg caseTarget)
                        val (exitThisCase, exitLabel) =
                            if caseExited then ([], [])
                            else if isTail then (returnInstruction(context, caseTarget), [])
                            else
                            let
                                val exitLabel = newLabel()
                            in
                                ([UnconditionalForwardJump{label=exitLabel}],
                                 [ForwardJumpLabel{label=exitLabel, result=SOME targetReg}])
                            end
                        val codeRest = codeCases otherCases
                    in
                        ForwardJumpLabel{label=startLabel, result=NONE} ::
                            codeThisCase @ exitThisCase @ codeRest @ exitLabel
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
                        val defaultTarget = if isTail then newPReg() else targetReg
                        val (defaultCode, _, defaultExited) =
                            codeToICode(default, context, isTail, SpecificPReg defaultTarget)
                    in
                        (* Put in the default.  Because this is the last we don't need to
                           jump round it.  However if this is a tail and we haven't exited
                           we put in a return.  That way the case will always have
                           exited if this is a tail. *)
                         defLabels @ defaultCode @
                            (if isTail andalso not defaultExited then returnInstruction(context, defaultTarget) else [])
                    end
                    
                val codedCases = codeCases fullCaseRange

                val codeLabels = map (fn (lab, _, _) => lab) fullCaseRange
                (* We can now copy to the target.  If we need to push the result this load
                   will be converted into a push. *)
                val target = asTarget destination
                val copyToTarget =
                    if isTail then []
                    else [LoadArgument{source=RegisterArgument targetReg, dest=target, kind=MoveWord}]
            in
                (testCode @ rangeChecks @ [IndexedCaseOperation{testReg=testReg, workReg=workReg,
                                        cases=codeLabels, startValue=min}] @ codedCases @ copyToTarget,
                 RegisterArgument target, isTail (* We have always exited on a tail. *))
            end
        
        |   codeToICode(BICBeginLoop {loop, arguments}, context as { stackPtr, ...}, isTail, destination) =
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
                val (loopBody, _, loopExited) =
                    codeToICode(loop, {loopArgs=SOME (loopRegs, loopLabel, stackPtr), stackPtr=stackPtr }, isTail, SpecificPReg target)
                val args = ListPair.mapEq(fn (s, l) => (RegisterArgument s, l)) (argPRegs, loopRegs)
            in
                (codeArgs @ [StartLoop{regArgs=args, loopLabel=loopLabel}] @ loopBody @
                    [EndLoop {loopLabel=loopLabel, staticRegs=[]}], RegisterArgument target, loopExited)
            end
        
        |   codeToICode(BICLoop args, context as {loopArgs=SOME (loopRegs, loopLabel, loopSp), stackPtr, ...}, _, destination) =
            let
                val target = asTarget destination
                (* Registers to receive the evaluated arguments.  We can't put the
                   values into the loop variables yet because the values could depend
                   on the current values of the loop variables. *)
                val argPRegs = map(fn _ => newPReg()) args
                val codeArgs =
                    ListPair.foldlEq(fn ((arg, _), pr, l) => l @ codeToICodeTarget(arg, context, false, pr)) []
                        (args, argPRegs)
                val jumpArgs = ListPair.mapEq(fn (s, l) => (RegisterArgument s, l)) (argPRegs, loopRegs)
                (* If we've allocated a container in the loop we have to remove it before jumping back. *)
                val stackReset =
                    if loopSp = stackPtr then []
                    else [ResetStackPtr{numWords=stackPtr-loopSp}]
            in
                (codeArgs @ stackReset @ [JumpLoop{regArgs=jumpArgs, stackArgs=[], loopLabel=loopLabel, stackAdjust=0}], RegisterArgument target, true)
            end

        |   codeToICode(BICLoop _, {loopArgs=NONE, ...}, _, _) =
                raise InternalError "BICLoop without BICBeginLoop"

        |   codeToICode(BICRaise exc, context, _, destination) =
            let
                val (code, dest, _) = codeToICode(exc, context, false, Allowed allowDefer)
            in
                (code @ [RaiseExceptionPacket{packet=dest}], RegisterArgument(asTarget destination), true (* Always exits *))
            end

        |   codeToICode(BICHandle {exp, handler, exPacketAddr}, context as { stackPtr, loopArgs }, isTail, destination) =
            let
                (* As with BICCond and BICCase we need to create a new register for the
                   result in case we need to push it to the stack. *)
                val handleResult = newPReg()
                val (bodyTarget, handlerTarget) =
                    if isTail then (newPReg(), newPReg()) else (handleResult, handleResult)
                (* TODO: Even if we don't actually want a result we force one in here by
                   using "asTarget".  *)
                (* The expression cannot be treated as a tail because the handler has
                   to be removed after.  It may "exit" if it has raised an unconditional
                   exception.  If it has we mustn't generate a PopExceptionHandler because
                   there won't be any result for resultReg.
                   We need to add two words to the stack to account for the items pushed by
                   PushExceptionHandler. *)
                val (expCode, _, expExit) =
                    codeToICode(exp, {stackPtr=stackPtr+2, loopArgs=loopArgs}, false (* Not tail *), SpecificPReg bodyTarget)
                (* Make a register to hold the exception packet and put eax into it. *)
                val packetAddr = newPReg()
                val () = Array.update(locToPregArray, exPacketAddr, SOME packetAddr)
                val (handleCode, _, handleExit) =
                    codeToICode(handler, context, isTail, SpecificPReg handlerTarget)
                val handlerLab = newLabel()
                (* If this is the tail we can replace the jump at the end of the
                   handled code with returns.  If the handler has exited we don't need
                   a return there.  Otherwise we need to add an unconditional jump to
                   skip the handler. *)
                val target = asTarget destination
                val (atExpEnd, afterHandler) =
                    if isTail
                    then
                        (if expExit then []
                         else
                            PopExceptionHandler{resultReg=SOME bodyTarget, workReg=newPReg()} ::
                             returnInstruction(context, bodyTarget),
                         if handleExit then [] else returnInstruction(context, handlerTarget))
                    else
                    let
                        val skipHandler = newLabel()
                    in
                        (if expExit then []
                         else [PopExceptionHandler{resultReg=SOME bodyTarget, workReg=newPReg()}, 
                               UnconditionalForwardJump{label=skipHandler}],
                         [ForwardJumpLabel{label=skipHandler, result=SOME handleResult},
                          LoadArgument{source=RegisterArgument handleResult, dest=target, kind=MoveWord}])
                    end
            in
                (PushExceptionHandler{workReg=newPReg(), handleStart=handlerLab} :: expCode @ atExpEnd @
                [BeginHandler{handleStart=handlerLab, workReg=newPReg(), packetReg=packetAddr}] @
                 handleCode @ afterHandler, RegisterArgument target, isTail)
            end

        |   codeToICode(BICTuple fields, context, _, destination) =
            let
                (* TODO: This is a relic of the old fall-back code-generator.  It required
                   the result of a tuple to be at the top of the stack.  It should be changed. *)
                val target = asTarget destination (* Actually we want this. *)
                val memAddr = newPReg()
                fun loadFields([], n) =
                        [AllocateMemoryOperation{size=n, flags=0w0, dest=memAddr, saveRegs=[]}]
                |   loadFields(f :: rest, n) =
                    let
                        (* Defer the evaluation if possible.  We may have a constant that we can't move
                           directly but it's better to load it after the allocation otherwise we will
                           have to push the register if we need to GC. *)
                        val (code1, source1, _) = codeToICode(f, context, false, Allowed allowDefer)
                        val restAndAlloc = loadFields(rest, n+1)
                        val (code2, source, _)  = moveIfNotAllowed(Allowed allowInMemMove, [], source1)
                        val storeValue = [StoreArgument{ source=source, offset=n*wordSize, base=memAddr, index=NoMemIndex, kind=MoveWord}]
                    in
                        code1 @ restAndAlloc @ code2 @ storeValue
                    end
                val code = loadFields(fields, 0) @
                    [InitialisationComplete, LoadArgument{source=RegisterArgument memAddr, dest=target, kind=MoveWord}]
            in
                (code, RegisterArgument target, false)
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
                        val (c, r, _) = codeToICode(arg, context, false, Allowed allowInMemMove)
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
                        StoreArgument{source=srcReg, offset=destWord*wordSize, base=containerTarget, index=NoMemIndex, kind=MoveWord} ::
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
                        LoadArgument{source=wordOffsetAddress(sourceWord, tupleTarget), dest=loadReg, kind=MoveWord} ::
                        StoreArgument{source=RegisterArgument loadReg, offset=destWord*wordSize, base=containerTarget, index=NoMemIndex, kind=MoveWord} ::
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

        |   codeToICode(BICLoadOperation {kind=LoadStoreMLWord _, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeAddress(address, false, context)
            in
                (codeBaseIndex @ codeUntag @
                    [LoadArgument {source=MemoryLocation memLoc, dest=target, kind=MoveWord}], RegisterArgument target, false)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreMLByte _, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeAddress(address, true, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveByte},
                     TagValue {source=untaggedResReg, dest=target}], RegisterArgument target, false)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreC8, address}, context, _, destination) =
            let
                (* Load a byte from C memory.  This is almost exactly the same as LoadStoreMLByte except
                   that the base address is a LargeWord.word value. *)
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w1, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveByte},
                     TagValue {source=untaggedResReg, dest=target}], RegisterArgument target, false)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreC16, address}, context, _, destination) =
            let
                (* Load a 16-bit value from C memory. *)
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w2, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=Move16Bit},
                     TagValue {source=untaggedResReg, dest=target}], RegisterArgument target, false)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreC32, address}, context, _, destination) =
            let
                (* Load a 32-bit value from C memory.  If this is 64-bit mode we can tag it but
                   if this is 32-bit mode we need to box it. *)
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w4, context)
                val untaggedResReg = newUReg()
                val (boxTagCode, moveKind) =
                    if isX64
                    then (TagValue {source=untaggedResReg, dest=target}, Move32Bit)
                    else (BoxValue{boxKind=BoxLargeWord, source=untaggedResReg, dest=target, saveRegs=[]}, MoveWord)
            in
                (codeBaseIndex @ codeUntag @
                    [LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=moveKind}, boxTagCode], RegisterArgument target, false)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreC64, address}, context, _, destination) =
            let
                (* Load a 64-bit value from C memory.  This is only allowed in 64-bit mode.  The result
                   is a boxed value. *)
                val _ = isX64 orelse raise InternalError "codeToICode: BICLoadOperation LoadStoreC64 in 32-bit"
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w8, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveWord},
                     BoxValue{boxKind=BoxLargeWord, source=untaggedResReg, dest=target, saveRegs=[]}], RegisterArgument target, false)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreCFloat, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w4, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveFloat},
                     BoxValue{boxKind=BoxFloat, source=untaggedResReg, dest=target, saveRegs=[]}], RegisterArgument target, false)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreCDouble, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w8, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveDouble},
                     BoxValue{boxKind=BoxFloat, source=untaggedResReg, dest=target, saveRegs=[]}], RegisterArgument target, false)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreUntaggedUnsigned, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeAddress(address, false, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveWord},
                     TagValue {source=untaggedResReg, dest=target}], RegisterArgument target, false)
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreMLWord _, address, value}, context, _, destination) =
            let
                val (sourceCode, source, _) = codeToICode(value, context, false, Allowed allowInMemMove)
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeAddress(address, false, context)
                val code =
                    codeBaseIndex @ sourceCode @ codeUntag @
                        [StoreArgument {source=source, base=base, offset=offset, index=index, kind=MoveWord}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

         |   codeToICode(BICStoreOperation {kind=LoadStoreMLByte _, address, value=BICConstnt(constByte, _)}, context, _, destination) =
            let
                val valueToStore = if isShort constByte then toShort constByte else 0w0
                (* Storing a constant. *)
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeAddress(address, true, context)
                val code =
                    codeBaseIndex @ codeUntag @
                    [StoreArgument {source=IntegerConstant(Word.toLargeInt valueToStore), base=base, offset=offset, index=index, kind=MoveByte}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreMLByte _, address, value}, context, _, destination) =
            let
                (* We have to untag the value to store. *)
                val valueReg = newPReg() and valueReg1 = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeAddress(address, true, context)
                val code =
                    codeBaseIndex @
                    codeToICodeTarget(value, context, false, valueReg) @
                    [UntagValue{dest=valueReg1, source=RegisterArgument valueReg, isSigned=false }] @ codeUntag @
                    [StoreArgument {source=RegisterArgument valueReg1, base=base, offset=offset, index=index, kind=MoveByte}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreC8, address, value}, context, _, destination) =
            let
                (* Store a byte to C memory.  Almost exactly the same as LoadStoreMLByte. *)
                val valueReg = newPReg() and valueReg1 = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeCAddress(address, 0w1, context)
                val code =
                    codeBaseIndex @
                    codeToICodeTarget(value, context, false, valueReg) @
                    [UntagValue{dest=valueReg1, source=RegisterArgument valueReg, isSigned=false }] @ codeUntag @
                    [StoreArgument {source=RegisterArgument valueReg1, base=base, offset=offset, index=index, kind=MoveByte}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreC16, address, value}, context, _, destination) =
            let
                (* Store a 16-bit value to C memory. *)
                val valueReg = newPReg() and valueReg1 = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeCAddress(address, 0w2, context)
                val code =
                    codeBaseIndex @
                    codeToICodeTarget(value, context, false, valueReg) @
                    [UntagValue{dest=valueReg1, source=RegisterArgument valueReg, isSigned=false }] @ codeUntag @
                    [StoreArgument {source=RegisterArgument valueReg1, base=base, offset=offset, index=index, kind=Move16Bit}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreC32, address, value}, context, _, destination) =
            let
                (* Store a 32-bit value.  If this is 64-bit mode we untag it but if this is 32-bit mode we unbox it. *)
                val valueReg = newPReg() and valueReg1 = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeCAddress(address, 0w4, context)
                val untagUnbox =
                    if isX64
                    then [UntagValue{dest=valueReg1, source=RegisterArgument valueReg, isSigned=false }]
                    else [LoadArgument{source=wordAt valueReg, dest=valueReg1, kind=MoveWord}]
                val code =
                    codeBaseIndex @
                    codeToICodeTarget(value, context, false, valueReg) @ untagUnbox @ codeUntag @
                    [StoreArgument {source=RegisterArgument valueReg1, base=base, offset=offset, index=index, kind=Move32Bit}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreC64, address, value}, context, _, destination) =
            let
                (* Store a 64-bit value. *)
                val _ = isX64 orelse raise InternalError "codeToICode: BICStoreOperation LoadStoreC64 in 32-bit"
                val valueReg = newPReg() and valueReg1 = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeCAddress(address, 0w8, context)
                val code =
                    codeBaseIndex @
                    codeToICodeTarget(value, context, false, valueReg) @ codeUntag @
                    [LoadArgument{source=wordAt valueReg, dest=valueReg1, kind=MoveWord},
                     StoreArgument {source=RegisterArgument valueReg1, base=base, offset=offset, index=index, kind=MoveWord}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreCFloat, address, value}, context, _, destination) =
            let
                val valueReg = newPReg() and floatReg = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeCAddress(address, 0w4, context)
                val code =
                    codeBaseIndex @
                    codeToICodeTarget(value, context, false, valueReg) @ codeUntag @
                    [LoadArgument{source=wordAt valueReg, dest=floatReg, kind=MoveDouble},
                     StoreArgument {source=RegisterArgument floatReg, base=base, offset=offset, index=index, kind=MoveFloat}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreCDouble, address, value}, context, _, destination) =
            let
                val valueReg = newPReg() and floatReg = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeCAddress(address, 0w8, context)
                val code =
                    codeBaseIndex @
                    codeToICodeTarget(value, context, false, valueReg) @ codeUntag @
                    [LoadArgument{source=wordAt valueReg, dest=floatReg, kind=MoveDouble},
                     StoreArgument {source=RegisterArgument floatReg, base=base, offset=offset, index=index, kind=MoveDouble}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreUntaggedUnsigned, address, value}, context, _, destination) =
            let
                (* We have to untag the value to store. *)
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeAddress(address, false, context)
                (* See if it's a constant.  This is frequently used to set the last word of a string to zero. *)
                val storeAble =
                    case value of
                        BICConstnt(value, _) =>
                            if not(isShort value)
                            then NONE
                            else
                            let
                                val ival = Word.toLargeIntX(toShort value)
                            in
                                if is32bit ival then SOME ival else NONE
                            end
                    |   _ => NONE
                val (storeVal, valCode) =
                    case storeAble of
                        SOME value => (IntegerConstant value (* Leave untagged *), [])
                    |   NONE =>
                        let
                            val valueReg = newPReg() and valueReg1 = newUReg()
                        in
                            (RegisterArgument valueReg1,
                                codeToICodeTarget(value, context, false, valueReg) @
                                [UntagValue{dest=valueReg1, source=RegisterArgument valueReg, isSigned=false }])
                        end
                val code =
                    codeBaseIndex @ valCode @ codeUntag @
                    [StoreArgument {source=storeVal, base=base, offset=offset, index=index, kind=MoveWord}]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(instr as BICBlockOperation {kind=BlockOpEqualByte, ...}, context, isTail, destination) =
                codeAsConditional(instr, context, isTail, destination)

        |   codeToICode(BICBlockOperation {kind=BlockOpCompareByte, sourceLeft, destRight, length}, context, _, destination) =
            let
                (* TODO: If we have a short fixed length comparison we might well be better loading the value into
                   a register and comparing with memory. *)
                val target = asTarget destination
                val vec1Reg = newUReg() and vec2Reg = newUReg()
                val (leftCode, leftUntag, {base=leftBase, offset=leftOffset, index=leftIndex}) =
                    codeAddress(sourceLeft, true, context)
                val (rightCode, rightUntag, {base=rightBase, offset=rightOffset, index=rightIndex}) =
                    codeAddress(destRight, true, context)
                val ccRef = newCCRef()
                val labLess = newLabel() and labGreater = newLabel()
                and exitLab1 = newLabel() and exitLab2 = newLabel()
                
                val (lengthCode, lengthUntag, lengthArg) = codeAsUntaggedToReg(length, false (* unsigned *), context)

                val code =
                    leftCode @ rightCode @ lengthCode @
                    leftUntag @ [LoadEffectiveAddress{base=SOME leftBase, offset=leftOffset, index=leftIndex, dest=vec1Reg}] @
                    rightUntag @ [LoadEffectiveAddress{base=SOME rightBase, offset=rightOffset, index=rightIndex, dest=vec2Reg}] @
                    lengthUntag @
                    [CompareByteVectors{ vec1Addr=vec1Reg, vec2Addr=vec2Reg, length=lengthArg, ccRef=ccRef },
                     ConditionalForwardJump{ ccRef=ccRef, condition=JL, label=labLess },
                     ConditionalForwardJump{ ccRef=ccRef, condition=JG, label=labGreater },
                     LoadArgument{ source=IntegerConstant(tag 0), dest=target, kind=MoveWord },
                     UnconditionalForwardJump{ label=exitLab1 },
                     ForwardJumpLabel{ label=labLess, result=NONE },
                     LoadArgument{ source=IntegerConstant(tag ~1), dest=target, kind=MoveWord },
                     UnconditionalForwardJump{ label=exitLab2 },
                     ForwardJumpLabel{ label=labGreater, result=NONE },
                     LoadArgument{ source=IntegerConstant(tag 1), dest=target, kind=MoveWord },
                     ForwardJumpLabel{ label=exitLab1, result=SOME target },
                     ForwardJumpLabel{ label=exitLab2, result=SOME target }]
            in
                (code, RegisterArgument target, false)
            end

        |   codeToICode(BICBlockOperation {kind=BlockOpMove {isByteMove}, sourceLeft, destRight, length}, context, _, destination) =
            let
                (* TODO: If we have a short fixed length move we might well be better loading the value into
                   a register and storing it. *)
                val lengthReg = newPReg()
                val vec1Reg = newUReg() and vec2Reg = newUReg() and lenReg = newUReg()
                val (leftCode, leftUntag, {base=leftBase, offset=leftOffset, index=leftIndex}) =
                    codeAddress(sourceLeft, isByteMove, context)
                val (rightCode, rightUntag, {base=rightBase, offset=rightOffset, index=rightIndex}) =
                    codeAddress(destRight, isByteMove, context)
                val code =
                    leftCode @ rightCode @ codeToICodeTarget(length, context, false, lengthReg) @
                    leftUntag @ [LoadEffectiveAddress{base=SOME leftBase, offset=leftOffset, index=leftIndex, dest=vec1Reg}] @
                    rightUntag @ [LoadEffectiveAddress{base=SOME rightBase, offset=rightOffset, index=rightIndex, dest=vec2Reg}] @
                    [UntagValue{source=RegisterArgument lengthReg, dest=lenReg, isSigned=false},
                     BlockMove{ srcAddr=vec1Reg, destAddr=vec2Reg, length=lenReg, isByteMove=isByteMove }]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        (* Code a branch condition.  This is really meant for andalso/orelse. *)
        and codeCondition(BICConstnt(value, _), _, jumpOn, jumpLabel) =
            (* Constant - typically part of andalso/orelse.  Either an unconditional branch
               or an unconditional drop-through. *)
            if jumpOn = (toShort value <> 0w0)
            then [UnconditionalForwardJump{label=jumpLabel}]
            else []

        |   codeCondition(BICTagTest{test, tag=tagValue, ...}, context, jumpOn, jumpLabel) =
            (* Check the "tag" word of a union (datatype).  N.B.  Not the same as testing the
               tag bit of a word. *)
            let
                val ccRef = newCCRef()
                val (testCode, tagReg) = codeToPReg(test, context)
            in
                testCode @
                    [WordComparison{arg1=RegisterArgument tagReg, arg2=IntegerConstant(tag(Word.toLargeInt tagValue)), ccRef=ccRef},
                     ConditionalForwardJump{ccRef=ccRef,
                           condition=if jumpOn then JE else JNE, label=jumpLabel}]
            end

        |   codeCondition(BICUnary{oper=BuiltIns.NotBoolean, arg1}, context, jumpOn, jumpLabel) =
                (* If we have a "not" we can just invert the jump condition. *)
                codeCondition(arg1, context, not jumpOn, jumpLabel)

        |   codeCondition(BICUnary{oper=BuiltIns.IsTaggedValue, arg1}, context, jumpOn, jumpLabel) =
            let
                val ccRef = newCCRef()
                val (testCode, testReg) = codeToPReg(arg1, context)
                (* Test the tag bit.  This sets the zero bit if the value is untagged. *)
                (* TODO: The X86 supports tests with a memory argument so we don't have to
                   load it into a register.  That's not currently supported by the rest of
                   the code-generator. *)
            in
                testCode @
                    [TestTagBit{arg=RegisterArgument testReg, ccRef=ccRef},
                     ConditionalForwardJump{ccRef=ccRef, condition=if jumpOn then JNE else JE, label=jumpLabel}]
            end

            (* Comparisons.  Because this is also used for pointer equality and even for exception matching it
               is perfectly possible that the argument could be an address. *)
        |   codeCondition(BICBinary{oper=BuiltIns.WordComparison{test, isSigned}, arg1, arg2=BICConstnt(arg2Value, _)},
                    context, jumpOn, jumpLabel) =
            let
                val ccRef = newCCRef()
                val (testCode, testReg) = codeToPReg(arg1, context)
                val arg2Arg = constantAsArgument arg2Value
            in
                testCode @
                    [WordComparison{arg1=RegisterArgument testReg, arg2=arg2Arg, ccRef=ccRef},
                     ConditionalForwardJump{ccRef=ccRef,
                           condition=testAsBranch(test, isSigned, jumpOn), label=jumpLabel}]
            end

(*        |   codeCondition(BICBinary{oper=BuiltIns.WordComparison{test, isSigned}, arg1=BICConstnt(arg1Value, _), arg2},
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

        |   codeCondition(BICBinary{oper=BuiltIns.WordComparison{test, isSigned}, arg1, arg2},
                    context, jumpOn, jumpLabel) =
            let
                val ccRef = newCCRef()
                val (arg1Code, arg1Reg) = codeToPReg(arg1, context)
                val (arg2Code, arg2Reg) = codeToPReg(arg2, context)
            in
                arg1Code @ arg2Code @
                    [WordComparison{arg1=RegisterArgument arg1Reg, arg2=RegisterArgument arg2Reg, ccRef=ccRef},
                     ConditionalForwardJump{ccRef=ccRef,
                           condition=testAsBranch(test, isSigned, jumpOn), label=jumpLabel}]
            end

        |   codeCondition(BICBinary{oper=BuiltIns.LargeWordComparison test, arg1, arg2},
                    context, jumpOn, jumpLabel) =
            let
                val ccRef = newCCRef()
                val (arg1Code, arg1Reg) = codeToPReg(arg1, context)
                val (arg2Code, arg2Reg) = codeToPReg(arg2, context)
                val argReg = newUReg()
            in
                arg1Code @ arg2Code @
                    [LoadArgument{source=wordAt arg1Reg, dest=argReg, kind=MoveWord},
                     WordComparison{arg1=RegisterArgument argReg, arg2=wordAt arg2Reg, ccRef=ccRef},
                     ConditionalForwardJump{ccRef=ccRef,
                           condition=testAsBranch(test, false, jumpOn), label=jumpLabel}]
            end

           (* Floating point comparison.  This is complicated because we have different
              instruction sequences for SSE2 and X87.  We also have to get the handling
              of unordered (NaN) values right.  All the tests are treated as false
              if either argument is a NaN.  To combine that test with the other tests
              we sometimes have to reverse the comparison. *)

        |   codeCondition(BICBinary{oper=BuiltIns.RealComparison BuiltIns.TestEqual, arg1, arg2},
                    context, jumpOn, jumpLabel) =
            let
                val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                val fpReg = newUReg()
                val testReg1 = newUReg() and testReg2 = newUReg() and testReg3 = newUReg()
                val (arg1Code, arg1Reg) = codeToPReg(arg1, context)
                val (arg2Code, arg2Reg) = codeToPReg(arg2, context)
               (* If this is X87 we get the condition into RAX and test it there.  If
                   it is SSE2 we have to treat the unordered result (parity set) specially. *)
                val getResultCode =
                    case (fpMode, jumpOn) of
                        (FPModeX87, _) =>
                            [X87FPGetCondition { ccRef=ccRef1, dest=testReg1 },
                             ArithmeticFunction{
                                oper=AND, resultReg=testReg2, operand1=RegisterArgument testReg1, operand2=IntegerConstant 0x4400, ccRef=newCCRef() },
                             ArithmeticFunction{
                                oper=XOR, resultReg=testReg3, operand1=RegisterArgument testReg2, operand2=IntegerConstant 0x4000, ccRef=ccRef2 },
                             ConditionalForwardJump{ccRef=ccRef2, condition=if jumpOn then JE else JNE, label=jumpLabel}]
                    |   (FPModeSSE2, true) =>
                        let
                            val lab = newLabel()
                        in
                            [
                                ConditionalForwardJump{ccRef=ccRef1, condition=JP, label=lab},
                                ConditionalForwardJump{ccRef=ccRef1, condition=JE, label=jumpLabel},
                                ForwardJumpLabel{ label=lab, result=NONE }
                            ]
                        end
                   |    (FPModeSSE2, false) =>
                        [
                            ConditionalForwardJump{ccRef=ccRef1, condition=JP, label=jumpLabel},
                            ConditionalForwardJump{ccRef=ccRef1, condition=JNE, label=jumpLabel}
                        ]
            in
                arg1Code @ arg2Code @
                    (LoadArgument{source=wordAt arg1Reg, dest=fpReg, kind=MoveDouble} ::
                     CompareFloatingPt{arg1=RegisterArgument fpReg, arg2=wordAt arg2Reg, ccRef=ccRef1} :: getResultCode)
            end

        |   codeCondition(BICBinary{oper=BuiltIns.RealComparison BuiltIns.TestLess, arg1, arg2},
                    context, jumpOn, jumpLabel) =
            let
                val arg1Reg = newPReg() and arg2Reg = newPReg()
                val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                val fpReg = newUReg()
                val testReg1 = newUReg() and testReg2 = newUReg()
                val testCode =
                    codeToICodeTarget(arg1, context, false, arg1Reg) @
                    codeToICodeTarget(arg2, context, false, arg2Reg)
                val getResultCode =
                    case fpMode of
                        FPModeX87 =>
                            [X87FPGetCondition { ccRef=ccRef1, dest=testReg1 },
                             ArithmeticFunction{
                                oper=AND, resultReg=testReg2, operand1=RegisterArgument testReg1, operand2=IntegerConstant 0x4500, ccRef=ccRef2 },
                             ConditionalForwardJump{ccRef=ccRef2, condition=if jumpOn then JE else JNE, label=jumpLabel}]
                    |   FPModeSSE2 =>
                            [ConditionalForwardJump{ccRef=ccRef1, condition=if jumpOn then JA else JNA, label=jumpLabel}]
            in
                testCode @
                    [LoadArgument{source=wordAt arg2Reg, dest=fpReg, kind=MoveDouble},
                     CompareFloatingPt{arg1=RegisterArgument fpReg, arg2=wordAt arg1Reg, ccRef=ccRef1}] @ (* Reverse *)
                        getResultCode
            end

        |   codeCondition(BICBinary{oper=BuiltIns.RealComparison BuiltIns.TestGreater, arg1, arg2},
                    context, jumpOn, jumpLabel) =
            let
                val arg1Reg = newPReg() and arg2Reg = newPReg()
                val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                val fpReg = newUReg()
                val testReg1 = newUReg() and testReg2 = newUReg()
                val testCode =
                    codeToICodeTarget(arg1, context, false, arg1Reg) @
                    codeToICodeTarget(arg2, context, false, arg2Reg)
                val getResultCode =
                    case fpMode of
                        FPModeX87 =>
                            [X87FPGetCondition { ccRef=ccRef1, dest=testReg1 },
                             ArithmeticFunction{
                                oper=AND, resultReg=testReg2, operand1=RegisterArgument testReg1, operand2=IntegerConstant 0x4500, ccRef=ccRef2 },
                             ConditionalForwardJump{ccRef=ccRef2, condition=if jumpOn then JE else JNE, label=jumpLabel}]
                    |   FPModeSSE2 =>
                            [ConditionalForwardJump{ccRef=ccRef1, condition=if jumpOn then JA else JNA, label=jumpLabel}]
            in
                testCode @
                    [LoadArgument{source=wordAt arg1Reg, dest=fpReg, kind=MoveDouble},
                     CompareFloatingPt{arg1=RegisterArgument fpReg, arg2=wordAt arg2Reg, ccRef=ccRef1}] @ (* Not reversed. *)
                     getResultCode
            end

        |   codeCondition(BICBinary{oper=BuiltIns.RealComparison BuiltIns.TestLessEqual, arg1, arg2},
                    context, jumpOn, jumpLabel) =
            let
                val arg1Reg = newPReg() and arg2Reg = newPReg()
                val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                val fpReg = newUReg()
                val testReg1 = newUReg() and testReg2 = newUReg()
                val testCode =
                    codeToICodeTarget(arg1, context, false, arg1Reg) @
                    codeToICodeTarget(arg2, context, false, arg2Reg)
                val getResultCode =
                    case fpMode of
                        FPModeX87 =>
                            [X87FPGetCondition { ccRef=ccRef1, dest=testReg1 },
                             ArithmeticFunction{
                                oper=AND, resultReg=testReg2, operand1=RegisterArgument testReg1, operand2=IntegerConstant 0x500, ccRef=ccRef2 },
                             ConditionalForwardJump{ccRef=ccRef2, condition=if jumpOn then JE else JNE, label=jumpLabel}]
                    |   FPModeSSE2 =>
                            [ConditionalForwardJump{ccRef=ccRef1, condition=if jumpOn then JNB else JB, label=jumpLabel}]
            in
                testCode @
                    [LoadArgument{source=wordAt arg2Reg, dest=fpReg, kind=MoveDouble},
                     CompareFloatingPt{arg1=RegisterArgument fpReg, arg2=wordAt arg1Reg, ccRef=ccRef1}] @ (* Reverse *)
                     getResultCode
            end

        |   codeCondition(BICBinary{oper=BuiltIns.RealComparison BuiltIns.TestGreaterEqual, arg1, arg2},
                    context, jumpOn, jumpLabel) =
            let
                val arg1Reg = newPReg() and arg2Reg = newPReg()
                val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                val fpReg = newUReg()
                val testReg1 = newUReg() and testReg2 = newUReg()
                val testCode =
                    codeToICodeTarget(arg1, context, false, arg1Reg) @
                    codeToICodeTarget(arg2, context, false, arg2Reg)
                val getResultCode =
                    case fpMode of
                        FPModeX87 =>
                            [X87FPGetCondition { ccRef=ccRef1, dest=testReg1 },
                             ArithmeticFunction{
                                oper=AND, resultReg=testReg2, operand1=RegisterArgument testReg1, operand2=IntegerConstant 0x500, ccRef=ccRef2 },
                             ConditionalForwardJump{ccRef=ccRef2, condition=if jumpOn then JE else JNE, label=jumpLabel}]
                    |   FPModeSSE2 =>
                            [ConditionalForwardJump{ccRef=ccRef1, condition=if jumpOn then JNB else JB, label=jumpLabel}]
            in
                testCode @
                    [LoadArgument{source=wordAt arg1Reg, dest=fpReg, kind=MoveDouble},
                     CompareFloatingPt{arg1=RegisterArgument fpReg, arg2=wordAt arg2Reg, ccRef=ccRef1}] @ (* Not reversed. *)
                     getResultCode
            end

        |   codeCondition(
                BICBlockOperation{kind=BlockOpEqualByte, sourceLeft, destRight, length}, context, jumpOn, jumpLabel) =
            let
                val vec1Reg = newUReg() and vec2Reg = newUReg()
                val ccRef = newCCRef()
                val (leftCode, leftUntag, {base=leftBase, offset=leftOffset, index=leftIndex}) =
                    codeAddress(sourceLeft, true, context)
                val (rightCode, rightUntag, {base=rightBase, offset=rightOffset, index=rightIndex}) =
                    codeAddress(destRight, true, context)
                val (lengthCode, lengthUntag, lengthArg) = codeAsUntaggedToReg(length, false (* unsigned *), context)
            in
                leftCode @ rightCode @ lengthCode @
                leftUntag @ [LoadEffectiveAddress{base=SOME leftBase, offset=leftOffset, index=leftIndex, dest=vec1Reg}] @
                rightUntag @ [LoadEffectiveAddress{base=SOME rightBase, offset=rightOffset, index=rightIndex, dest=vec2Reg}] @
                lengthUntag @
                [CompareByteVectors{ vec1Addr=vec1Reg, vec2Addr=vec2Reg, length=lengthArg, ccRef=ccRef },
                 ConditionalForwardJump{ccRef=ccRef, condition=if jumpOn then JE else JNE, label=jumpLabel}]
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
                val ccRef = newCCRef()
                val (testCode, testReg) = codeToPReg(condition, context)
            in
                testCode @
                    [WordComparison{arg1=RegisterArgument testReg, arg2=IntegerConstant(tag 1), ccRef=ccRef},
                     ConditionalForwardJump{ccRef=ccRef,
                           condition=if jumpOn then JE else JNE, label=jumpLabel}]
            end

        (* Some operations that deliver boolean results are better coded as
           if condition then true else false *)
        and codeAsConditional(instr, context, isTail, target) =
            codeToICode(
                BICCond(instr, BICConstnt(toMachineWord 1, []), BICConstnt(toMachineWord 0, [])), context, isTail, target)

        (* Code an address.  The index is optional. *)
        and codeAddress({base, index=SOME index, offset}, true (* byte move *), context) =
            let
                (* Byte address with index.  The index needs to be untagged. *)
                val indexReg1 = newUReg()
                val (codeBase, baseReg) = codeToPReg(base, context)
                and (codeIndex, indexReg) = codeToPReg(index, context)
                val untagCode = [UntagValue{dest=indexReg1, source=RegisterArgument indexReg, isSigned=false }]
                val memResult = {base=baseReg, offset=Word.toInt offset, index=MemIndex1 indexReg1}
            in
                (codeBase @ codeIndex, untagCode, memResult)
            end

        |   codeAddress({base, index=SOME index, offset}, false (* word move *), context) =
            let
                (* Word address with index.  We can avoid untagging the index by adjusting the
                   multiplier and offset *) 
                val (codeBase, baseReg) = codeToPReg(base, context)
                and (codeIndex, indexReg) = codeToPReg(index, context)
                val memResult =
                    if isX64
                    then {base=baseReg, offset=Word.toInt offset-4, index=MemIndex4 indexReg}
                    else {base=baseReg, offset=Word.toInt offset-2, index=MemIndex2 indexReg}
            in
                (codeBase @ codeIndex, [], memResult)
            end

        |   codeAddress({base, index=NONE, offset}, _, context) =
            let
                val (codeBase, baseReg) = codeToPReg(base, context)
                val memResult = {offset=Word.toInt offset, base=baseReg, index=NoMemIndex}
            in
                (codeBase, [], memResult)
            end

        (* C-memory operations are slightly different.  The base address is a LargeWord.word value.
           The index is a byte index so may have to be untagged. *)
        and codeCAddress({base, index=SOME index, offset}, 0w1, context) =
            let
                (* Byte address with index.  The index needs to be untagged. *)
                val untaggedBaseReg = newUReg() and indexReg1 = newUReg()
                val (codeBase, baseReg) = codeToPReg(base, context)
                and (codeIndex, indexReg) = codeToPReg(index, context)
                val untagCode =
                    [LoadArgument{source=wordAt baseReg, dest=untaggedBaseReg, kind=MoveWord},
                     UntagValue{dest=indexReg1, source=RegisterArgument indexReg, isSigned=false }]
                val memResult = {base=untaggedBaseReg, offset=Word.toInt offset, index=MemIndex1 indexReg1}
            in
                (codeBase @ codeIndex, untagCode, memResult)
            end

        |   codeCAddress({base, index=SOME index, offset}, size, context) =
            let
                (* Non-byte address with index.  By using an appropriate multiplier we can avoid
                   having to untag the index. *)
                val untaggedBaseReg = newUReg()
                val (codeBase, baseReg) = codeToPReg(base, context)
                and (codeIndex, indexReg) = codeToPReg(index, context)
                val untagCode = [LoadArgument{source=wordAt baseReg, dest=untaggedBaseReg, kind=MoveWord}]
                val memResult =
                    case size of
                        0w2 => {base=untaggedBaseReg, offset=Word.toInt offset-1, index=MemIndex1 indexReg}
                    |   0w4 => {base=untaggedBaseReg, offset=Word.toInt offset-2, index=MemIndex2 indexReg}
                    |   0w8 => {base=untaggedBaseReg, offset=Word.toInt offset-4, index=MemIndex4 indexReg}
                    |   _ => raise InternalError "codeCAddress: unknown size"
            in
                (codeBase @ codeIndex, untagCode, memResult)
            end

        |   codeCAddress({base, index=NONE, offset}, _, context) =
            let
                val untaggedBaseReg = newUReg()
                val (codeBase, baseReg) = codeToPReg(base, context)
                val untagCode = [LoadArgument{source=wordAt baseReg, dest=untaggedBaseReg, kind=MoveWord}]
                val memResult = {offset=Word.toInt offset, base=untaggedBaseReg, index=NoMemIndex}
            in
                (codeBase, untagCode, memResult)
            end

        (* Return an untagged value.  If we have a constant just return it.  Otherwise
           return the code to evaluate the argument, the code to untag it and the
           reference to the untagged register. *)
        and codeAsUntaggedToReg(BICConstnt(value, _), isSigned, _) =
            let
                (* Should always be short except for unreachable code. *)
                val untagReg = newUReg()
                val cval = if isShort value then toShort value else 0w0
                val cArg = IntegerConstant(if isSigned then Word.toLargeIntX cval else Word.toLargeInt cval) (* Don't tag *)
                val untag = [LoadArgument{source=cArg, dest=untagReg, kind=MoveWord}]
            in
                ([], untag, untagReg) (* Don't tag. *)
            end
        |   codeAsUntaggedToReg(arg, isSigned, context) =
            let
                val untagReg = newUReg()
                val (code, srcReg) = codeToPReg(arg, context)
                val untag = [UntagValue{source=RegisterArgument srcReg, dest=untagReg, isSigned=isSigned}]
            in
                (code, untag, untagReg)
            end

        (* Return the argument as an untagged value.  We separate evaluating the argument from
           untagging because we may have to evaluate other arguments and that could involve a
           function call and we can't save the value to the stack after we've untagged it. *)
        and codeAsUntaggedValue(BICConstnt(value, _), isSigned, _) =
            let
                val cval = if isShort value then toShort value else 0w0
                val cArg = IntegerConstant(if isSigned then Word.toLargeIntX cval else Word.toLargeInt cval) (* Don't tag *)
            in
                ([], [], cArg)
            end
        |   codeAsUntaggedValue(arg, isSigned, context) =
            let
                val argReg = newPReg() and untagReg = newUReg()
                val code = codeToICodeTarget(arg, context, false, argReg)
                val untag = [UntagValue{source=RegisterArgument argReg, dest=untagReg, isSigned=isSigned}]
            in
                (code, untag, RegisterArgument untagReg)
            end

        (* Allocate memory.  This is used both for true variable length cells and also
           for longer constant length cells. *)
        and allocateMemoryVariable(numWords, flags, initial, context, destination) =
        let
            val target = asTarget destination
            (* With the exception of flagReg all these registers are modified by the code.
               So, we have to copy the size value into a new register. *)
            val sizeReg = newPReg() and initReg = newPReg()
            val sizeReg2 = newPReg()
            val untagSizeReg = newUReg() and initAddrReg = newPReg() and allocReg = newPReg()
            val sizeCode = codeToICodeTarget(numWords, context, false, sizeReg)
            and (flagsCode, flagUntag, flagArg) = codeAsUntaggedValue(flags, false, context)
            (* We're better off deferring the initialiser if possible.  If the value is
               a constant we don't have to save it. *)
            val (initCode, initResult, _) = codeToICode(initial, context, false, Allowed allowDefer)
         in
            (sizeCode @ flagsCode @ initCode
              @
             [(* We need to copy the size here because AllocateMemoryVariable modifies the
                 size in order to store the length word.  This is unfortunate especially as
                 we're going to untag it anyway. *)
              LoadArgument{source=RegisterArgument sizeReg, dest=sizeReg2, kind=MoveWord},
              AllocateMemoryVariable{size=sizeReg, dest=allocReg, saveRegs=[]}] @
              flagUntag @
              [StoreArgument{ source=flagArg, base=allocReg, offset= ~1, index=NoMemIndex, kind=MoveByte},
              (* We need to copy the address here because InitialiseMem modifies all its arguments. *)
              LoadArgument{source=RegisterArgument allocReg, dest=initAddrReg, kind=MoveWord},
              UntagValue{source=RegisterArgument sizeReg2, dest=untagSizeReg, isSigned=false},
              LoadArgument{source=initResult, dest=initReg, kind=MoveWord},
              InitialiseMem{size=untagSizeReg, init=initReg, addr=initAddrReg},
              InitialisationComplete,
              LoadArgument{source=RegisterArgument allocReg, dest=target, kind=MoveWord}], RegisterArgument target, false)
        end


        val bodyContext = {loopArgs=NONE, stackPtr=0}
        val (bodyCode, _, bodyExited) =
            codeToICode(body, bodyContext, true, SpecificPReg resultTarget)
        val icode = beginInstruction :: bodyCode @ (if bodyExited then [] else returnInstruction(bodyContext, resultTarget))
      
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
