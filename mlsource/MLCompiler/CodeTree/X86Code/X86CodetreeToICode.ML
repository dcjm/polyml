(*
    Copyright David C. J. Matthews 2016-17

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

    (* Reverse a list and append the second.  This is used a lot when converting
       between the reverse and forward list versions. e.g. codeToICode and codeToICodeRev *)
    fun revApp([], l) = l
    |   revApp(hd :: tl, l) = revApp(tl, hd :: l)
    
    
    datatype blockStruct =
        BlockSimple of x86ICode
    |   BlockExit of x86ICode
    |   BlockLabel of int
    |   BlockFlow of controlFlow
    |   BlockBegin of (preg * reg) list
    |   BlockRaiseAndHandle of x86ICode * int
    |   BlockOptionalHandle of {call: x86ICode, handler: int, label: int }

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

    (* Check that a large-word constant looks right and get the value as a large int*)
    fun largeWordConstant value =
        if isShort value then raise InternalError "largeWordConstant: invalid"
        else
        let
            val addr = toAddress value
        in
            if length addr <> 0w1 orelse flags addr <> F_bytes
            then raise InternalError "largeWordConstant: invalid"
            else ();
            LargeWord.toLargeInt(RunCall.unsafeCast addr)
        end

    fun codeFunctionToX86({body, localCount, name, argTypes, closure, ...}:bicLambdaForm, debugSwitches, closureOpt) =
    let
        (* Pseudo-registers are allocated sequentially and the properties added to the list. *)
        val pregCounter = ref 0
        val pregPropList = ref []
        
        fun newPReg() =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := RegPropGeneral :: !pregPropList
        in
            PReg regNo
        end
        
        and newUReg() =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := RegPropUntagged :: !pregPropList
        in
            PReg regNo
        end
        
        and newStackLoc size =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := RegPropStack size :: !pregPropList
        in
            StackLoc{size=size, rno=regNo}
        end
        
        datatype locationValue =
            NoLocation
        |   PregLocation of preg
        |   ContainerLocation of { container: stackLocn, stackOffset: int }

        val locToPregArray = Array.array(localCount, NoLocation)
        val labelCounter = ref 1 (* Start at 1.  Zero is used for the root. *)
        fun newLabel() = !labelCounter before labelCounter := !labelCounter + 1
        val ccRefCounter = ref 0
        fun newCCRef() = CcRef(!ccRefCounter) before ccRefCounter := !ccRefCounter + 1

        val returnAddressEntry = newStackLoc 1

        local
            val numFunctionArgs = List.length argTypes
            val maxRegArgs = List.length argRegs
            
            fun createArg i = {stackOffset=numFunctionArgs-(i+maxRegArgs), stackReg = newStackLoc 1 }
        in
            val (argRegsUsed, currentStackArgs) =
                if numFunctionArgs >= maxRegArgs
                then (argRegs, numFunctionArgs - List.length argRegs)
                else (List.take(argRegs, numFunctionArgs), 0)
            
            val stackArgVector = Vector.tabulate(currentStackArgs, createArg)
        end
        val stackArguments = Vector.foldr(fn ({stackReg, ...}, l) => stackReg :: l) [] stackArgVector

        (* Pseudo-regs for the result, the closure and the args that were passed in real regs. *)
        val resultTarget = newPReg()
        val closureRegAddr = newPReg()
        val argPRegs = map (fn _ => newPReg()) argRegsUsed

        local
            val clReg = case closure of [] => [] | _ => [(closureRegAddr, GenReg edx)]
            val argRegs = ListPair.zip (argPRegs, argRegsUsed)
        in
            val beginInstruction = BlockBegin(clReg @ argRegs)
        end
        
        (* The return instruction.  This can be added on to various tails but there is always
           one at the end anyway. *)
        fun returnInstruction({stackPtr, ...}, target, tailCode) =
            BlockExit(ReturnResultFromFunction{resultReg=target, numStackArgs=currentStackArgs}) ::
            (if stackPtr <> 0
            then BlockSimple(ResetStackPtr{numWords=stackPtr, preserveCC=false}) :: tailCode
            else tailCode)

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
            
            (* Switch the direction of a test if we turn  c op x into x op c. *)
            fun leftRightTest TestEqual         = TestEqual
            |   leftRightTest TestLess          = TestGreater
            |   leftRightTest TestLessEqual     = TestGreaterEqual
            |   leftRightTest TestGreater       = TestLess
            |   leftRightTest TestGreaterEqual  = TestLessEqual
        end

        (* Generally we have an offset in words and no index register. *)
        fun wordOffsetAddress(offset, baseReg: preg): argument =
            MemoryLocation{offset=offset*wordSize, base=baseReg, index=NoMemIndex, cache=NONE}
   
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
            SpecificPReg of preg
        |   NoResult
        |   Allowed of allowedArgument
        
        (* Context type. *)
        type context = { loopArgs: (preg list * int * int) option, stackPtr: int, currHandler: int option }

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
                (code @ [BlockSimple(LoadArgument{source=arg, dest=target, kind=MoveWord})], RegisterArgument target, false)
            end

        fun moveIfNotAllowedRev(NoResult, code, arg) = (code, arg, false)

        |   moveIfNotAllowedRev(Allowed{anyConstant=true, ...}, code, arg as AddressConstant _) = (code, arg, false)
        
        |   moveIfNotAllowedRev(Allowed{anyConstant=true, ...}, code, arg as IntegerConstant _) = (code, arg, false)
        
        |   moveIfNotAllowedRev(dest as Allowed{const32s=true, ...}, code, arg as IntegerConstant value) =
            (* This is allowed if the value is within 32-bits *)
                if is32bit value
                then (code, arg, false)
                else moveToTargetRev(dest, code, arg)

        |   moveIfNotAllowedRev(dest as Allowed{const32s=true, ...}, code, arg as AddressConstant _) =
                if not isX64
                then (code, arg, false)
                else moveToTargetRev(dest, code, arg)

        |   moveIfNotAllowedRev(Allowed{existingPreg=true, ...}, code, arg as RegisterArgument(PReg _)) = (code, arg, false)
        
        |   moveIfNotAllowedRev(Allowed{memAddr=true, ...}, code, arg as MemoryLocation _) = (code, arg, false)

        |   moveIfNotAllowedRev(dest, code, arg) = moveToTargetRev(dest, code, arg)

        and moveToTargetRev(dest, code, arg) =
            let
                val target = asTarget dest
            in
                (BlockSimple(LoadArgument{source=arg, dest=target, kind=MoveWord}) :: code, RegisterArgument target, false)
            end

        (* Use a move if there's no offset or index.  We could use an add if there's no index. *)
        and loadAddress{base, offset=0, index=NoMemIndex, dest} =
                LoadArgument{source=RegisterArgument base, dest=dest, kind=MoveWord}
        |   loadAddress{base, offset, index, dest} = LoadEffectiveAddress{base=SOME base, offset=offset, dest=dest, index=index}

        and codeToICodeTarget(instr, context: context, isTail, target) =
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
        
        and codeToPRegRev(instr, context, tailCode) =
        let (* Many instructions require an argument in a register.  If it's already in a
               register use that rather than creating a new one. *)
            val (code, result, _) = codeToICodeRev(instr, context, false, Allowed allowInPReg, tailCode)
            val preg = case result of RegisterArgument pr => pr | _ => raise InternalError "codeToPRegRev"
        in
            (code, preg)
        end
        
        and codeToICode(instr, context, isTail, destination) =
        let
            val (code, dest, haveExited) = codeToICodeRev(instr, context, isTail, destination, [])
        in
            (List.rev code, dest, haveExited)
        end
        
        (* Main function to turn the codetree into ICode.  Optimisation is generally
           left to later passes.  This does detect tail recursion.
           This builds the result up in reverse order.  There was an allocation hotspot in loadFields
           in the BICTuple case which was eliminated by building the list in reverse and then
           reversing the result.  It seems better to build the list in reverse generally but for
           the moment there are too many special cases to do everything. *)
        and codeToICodeRev(BICNewenv (bindings, exp), context: context as {stackPtr=initialSp, ...} , isTail, destination, tailCode) =
            let
                (* Process a list of bindings.  We need to accumulate the space used by
                   any containers and reset the stack pointer at the end if necessary. *)
                fun doBindings([], context, tailCode) = (tailCode, context)
                    
                |   doBindings(BICDeclar{value, addr, ...} :: decs, context, tailCode) =
                    let
                        val (code, dest) = codeToPRegRev(value, context, tailCode)
                        val () = Array.update(locToPregArray, addr, PregLocation dest)
                    in
                        doBindings(decs, context, code)
                    end

                |   doBindings(BICRecDecs [{lambda, addr, ...}] :: decs, context, tailCode) =
                    (* We shouldn't have single entries in RecDecs but it seems to occur at the moment. *)
                    let
                        val dest = newPReg()
                        val (code, _, _) = codeToICodeRev(BICLambda lambda, context, false, SpecificPReg dest, tailCode)
                        val () = Array.update(locToPregArray, addr, PregLocation dest)
                    in
                        doBindings(decs, context, code)
                    end

                |   doBindings(BICRecDecs recDecs :: decs, context, tailCode) =
                    let
                        val destRegs = map (fn _ => newPReg()) recDecs

                        (* First build the closures as mutable cells containing zeros.  Set the
                           entry in the address table to the register containing the address. *)
                        fun makeClosure({lambda={closure, ...}, addr, ...}, dest, c) =
                        let
                            val () = Array.update(locToPregArray, addr, PregLocation dest)
                            val sizeClosure = List.length closure + 1
                            fun clear n =
                                if n = sizeClosure
                                then [BlockSimple(AllocateMemoryOperation{size=sizeClosure, flags=Address.F_mutable, dest=dest, saveRegs=[]})]
                                else
                                    (clear (n+1) @
                                        [BlockSimple(
                                            StoreArgument{source=IntegerConstant(tag 0), base=dest, offset=n*wordSize, index=NoMemIndex,
                                                          kind=MoveWord, isMutable=false})])
                        in
                            c @ clear 0 @ [BlockSimple InitialisationComplete]
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
                            fun loadFields([], _) = [BlockSimple(LoadArgument{source=RegisterArgument dest, dest=dstCopy, kind=MoveWord})]
                            |   loadFields(f :: rest, n) =
                                let
                                    val (code, source, _) = codeToICode(f, context, false, Allowed allowInMemMove)
                                    val restAndAlloc = loadFields(rest, n+1)
                                    val storeValue =
                                        [BlockSimple(StoreArgument{ source=source, base=dstCopy, offset=n*wordSize, index=NoMemIndex, kind=MoveWord, isMutable=false })]
                                in
                                    code @ restAndAlloc @ storeValue
                                end
                            val setFields = loadFields(BICConstnt(toMachineWord codeAddr, []) :: map BICExtract closure, 0)
                        in
                            l @ setFields @ [BlockSimple(LockMutable{addr=dest})]
                        end
                        val setClosures = ListPair.foldlEq setClosure [] (recDecs, destRegs)
                        
                        val code = List.rev(allocClosures @ setClosures) 
                    in
                        doBindings(decs, context, code @ tailCode)
                    end

                |   doBindings(BICNullBinding exp :: decs, context, tailCode) =
                    let
                        val (code, _, _) = codeToICodeRev(exp, context, false, NoResult, tailCode) (* And discard result. *)
                    in
                        doBindings(decs, context, code)
                    end
       
                |   doBindings(BICDecContainer{ addr, size } :: decs, {loopArgs, stackPtr, currHandler}, tailCode) =
                    let
                        val containerReg = newStackLoc size
                        val () = Array.update(locToPregArray, addr,
                                    ContainerLocation{container=containerReg, stackOffset=stackPtr+size})
                    in
                        doBindings(decs, {loopArgs=loopArgs, stackPtr=stackPtr+size, currHandler=currHandler},
                            BlockSimple(ReserveContainer{size=size, container=containerReg}) :: tailCode)
                    end

                val (codeBindings, resContext as {stackPtr=finalSp, ...}) = doBindings(bindings, context, tailCode)
                (* If we have had a container we'll need to reset the stack *)
            in
                if initialSp <> finalSp
                then
                let
                    val _ = finalSp >= initialSp orelse raise InternalError "codeToICode - stack ptr"
                    val bodyReg = newPReg() and resultReg = asTarget destination
                    val (codeExp, result, haveExited) =
                        codeToICodeRev(exp, resContext, isTail, SpecificPReg bodyReg, codeBindings)
                    val afterAdjustSp =
                        if haveExited
                        then codeExp
                        else
                            BlockSimple(LoadArgument{source=result, dest=resultReg, kind=MoveWord}) ::
                            BlockSimple(ResetStackPtr{numWords=finalSp-initialSp, preserveCC=false}) :: codeExp
                in
                    (afterAdjustSp, RegisterArgument resultReg, haveExited)
                end
                else codeToICodeRev(exp, resContext, isTail, destination, codeBindings)
            end

        |   codeToICodeRev(BICConstnt(value, _), _, _, destination, tailCode) =
                moveIfNotAllowedRev(destination, tailCode, constantAsArgument value)

        |   codeToICodeRev(BICExtract(BICLoadLocal l), {stackPtr, ...}, _, destination, tailCode) =
            (
                case Array.sub(locToPregArray, l) of
                    NoLocation => raise InternalError "codeToICodeRev - local unset"
                |   PregLocation pr => moveIfNotAllowedRev(destination, tailCode, RegisterArgument pr)
                |   ContainerLocation{container, stackOffset} =>
                        (* This always returns a ContainerAddr whatever the "allowed". *)
                        (tailCode, ContainerAddr{container=container, stackOffset=stackPtr-stackOffset}, false)
            )

        |   codeToICodeRev(BICExtract(BICLoadArgument a), {stackPtr, ...}, _, destination, tailCode) =
            if a < numArgRegs
            then (* It was originally in a register.  It's now in a preg. *)
                moveIfNotAllowedRev(destination, tailCode, RegisterArgument(List.nth(argPRegs, a)))
            else (* Pushed before call. *)
            let
                val target = asTarget destination
                val {stackOffset, stackReg} = Vector.sub(stackArgVector, a-numArgRegs)
            in
                (BlockSimple(LoadArgument{
                    source=StackLocation{wordOffset=stackOffset+stackPtr, container=stackReg, field=0, cache=NONE},
                    dest=target, kind=MoveWord}) :: tailCode,
                 RegisterArgument target, false)
            end
        
        |   codeToICodeRev(BICExtract(BICLoadClosure c), _, _, destination, tailCode) =
            (
                if c >= List.length closure then raise InternalError "BICExtract: closure" else ();
                (* N.B.  We need to add one to the closure entry because zero is the code address. *)
                moveIfNotAllowedRev(destination, tailCode, wordOffsetAddress(c+1, closureRegAddr))
            )

        |   codeToICodeRev(BICExtract BICLoadRecursive, _, _, destination, tailCode) =
                (* If the closure is empty we must use the constant.  We can't guarantee that
                   the caller will actually load the closure register if it knows the closure
                   is empty. *)
                moveIfNotAllowedRev(destination, tailCode,
                    case closure of
                        [] => AddressConstant(toMachineWord(valOf closureOpt))
                    |   _ => RegisterArgument closureRegAddr)

        |   codeToICodeRev(BICField{base, offset}, context as { stackPtr, ...}, _, destination, tailCode) =
            let
                val (codeBase, baseEntry, _) = codeToICodeRev(base, context, false, Allowed allowInPReg, tailCode)
            in
                (* If this is a local container we extract the field. *)
                case baseEntry of
                    RegisterArgument baseR =>
                        moveIfNotAllowedRev(destination, codeBase, wordOffsetAddress(offset, baseR))
                |   ContainerAddr{container, stackOffset} =>
                    let
                        val target = asTarget destination
                        val finalOffset = stackPtr-stackOffset+offset
                        val _ = finalOffset >= 0 orelse raise InternalError "offset"
                    in
                        (BlockSimple(LoadArgument{
                            source=StackLocation{wordOffset=finalOffset, container=container, field=offset, cache=NONE},
                            dest=target, kind=MoveWord}) :: tailCode,
                        RegisterArgument target, false)
                    end
                |   _ =>   raise InternalError "codeToICodeRev-BICField"                      
            end

        |   codeToICodeRev(BICEval{function, argList, ...}, context as { currHandler, ...}, isTail, destination, tailCode) =
            let
                val target = asTarget destination
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
                            then (tailCode, [], Recursive)
                            else if flags addrAsAddr <> Address.F_words
                            then (BlockSimple(LoadArgument{source=AddressConstant addr, dest=clPReg, kind=MoveWord}) :: tailCode,
                                      [(RegisterArgument clPReg, GenReg edx)], FullCall)
                            else
                            let
                            
                                val addrLength = length addrAsAddr
                                val _ = addrLength >= 0w1 orelse raise InternalError "BICEval address"
                                val codeAddr = loadWord(addrAsAddr, 0w0)
                                val _ = isCode (toAddress codeAddr) orelse raise InternalError "BICEval address not code"
                            in
                                if addrLength = 0w1
                                then (tailCode, [], ConstantCode codeAddr)
                                else (BlockSimple(LoadArgument{source=AddressConstant addr, dest=clPReg, kind=MoveWord}) :: tailCode,
                                      [(RegisterArgument clPReg, GenReg edx)], ConstantCode codeAddr)
                            end
                        end

                    |   BICExtract BICLoadRecursive =>
                        (
                            (* If the closure is empty we don't need to load rdx *)
                            case closure of
                                [] => (tailCode, [], Recursive)
                            |   _ =>
                                    (BlockSimple(LoadArgument {source=RegisterArgument closureRegAddr, dest=clPReg, kind=MoveWord}) :: tailCode,
                                     [(RegisterArgument clPReg, GenReg edx)], Recursive)
                        )

                    |   function => (* General case. *)
                            (#1 (codeToICodeRev(function, context, false, SpecificPReg clPReg, tailCode)), [(RegisterArgument clPReg, GenReg edx)], FullCall)
                (* Optimise arguments.  We have to be careful with tail-recursive functions because they
                   need to save any stack arguments that could be overwritten.  This is complicated
                   because we overwrite the stack before loading the register arguments.  In some
                   circumstances it could be safe but for the moment leave it.  This should be safe
                   in the new code-transform but not the old codeICode.
                   Currently we don't allow memory arguments at all.  There's the potential for
                   problems later.  Memory arguments could possibly lead to aliasing of the stack
                   if the memory actually refers to a container on the stack.  That would mess
                   up the code that ensures that stack arguments are stored in the right order. *)
                (* We don't allow long constants in stack arguments to a tail-recursive call
                   because we may use a memory move to set them. *)
                val allowInStackArg =
                    Allowed {anyConstant=not isTail, const32s=true, memAddr=false, existingPreg=not isTail }
                and allowInRegArg =
                    Allowed {anyConstant=true, const32s=true, memAddr=false, existingPreg=not isTail }

                (* Load the first arguments into registers and the rest to the stack. *)
                fun loadArgs ([], _, tailCode) = (tailCode, [], [])
                |   loadArgs ((arg, _) :: args, argReg::argRegs, tailCode) =
                    let (* Register argument. *)
                        val (c, r, _) = codeToICodeRev(arg, context, false, allowInRegArg, tailCode)
                        val (code, regArgs, stackArgs) = loadArgs(args, argRegs, c)
                    in
                        (code, (r, argReg) :: regArgs, stackArgs)
                    end
                |   loadArgs ((arg, _) :: args, [], tailCode) =
                    let (* Stack argument. *)
                        val (c, r, _) = codeToICodeRev(arg, context, false, allowInStackArg, tailCode)
                        val (code, regArgs, stackArgs) = loadArgs(args, [], c)
                    in
                        (code, regArgs, r :: stackArgs)
                    end
                val (codeArgs, regArgs, stackArgs) = loadArgs(argList, argRegs, functionCode)
                
                val callInstr =
                    if isTail
                    then
                    let
                        val {stackPtr, ...} = context
                        (* The number of arguments currently on the stack. *)
                        val currentStackArgCount = currentStackArgs
                        val newStackArgCount = List.length stackArgs
                        (* The offset of the first argument or the return address if there are
                           no stack arguments.  N.B. We actually have currentStackArgCount+1
                           items on the stack including the return address.  Offsets can be
                           negative. *)
                        val stackOffset = stackPtr
                        val firstArgumentAddr = currentStackArgCount
                        fun makeStackArgs([], _) = []
                        |   makeStackArgs(arg::args, offset) = {src=arg, stack=offset} :: makeStackArgs(args, offset-1)
                        val stackArgs = makeStackArgs(stackArgs, firstArgumentAddr)
                        (* The stack adjustment needed to compensate for any items that have been pushed
                           and the differences in the number of arguments.  May be positive or negative.
                           This is also the destination address of the return address so when we enter
                           the new function the return address will be the first item on the stack. *)
                        val stackAdjust = firstArgumentAddr - newStackArgCount
                        (* Add an entry for the return address to the stack arguments. *)
                        val returnEntry =
                            {src=StackLocation{wordOffset=stackPtr, container=returnAddressEntry, field=0, cache=NONE}, stack=stackAdjust}
                        (* Because we're storing into the stack we may be overwriting values we want.  If the source of
                           any value is a stack location below the current stack pointer we load it except in the special
                           case where the destination is the same as the source (which is often the case with the return
                           address). *)
                        local
                            fun loadArgs [] = ([], [])
                            |   loadArgs (arg :: rest) =
                                let
                                    val (loadCode, loadedArgs) = loadArgs rest
                                in
                                    case arg of
                                        {src as StackLocation{wordOffset, ...}, stack} =>
                                            if wordOffset = stack+stackOffset (* Same location *)
                                                orelse stack+stackOffset < 0 (* Storing above current top of stack *)
                                                orelse stackOffset+wordOffset > ~ stackAdjust (* Above the last argument *)
                                            then (loadCode, arg :: loadedArgs)
                                            else
                                            let
                                                val preg = newPReg()
                                            in
                                                (BlockSimple(LoadArgument{source=src, dest=preg, kind=MoveWord}) :: loadCode,
                                                    {src=RegisterArgument preg, stack=stack} :: loadedArgs)
                                            end
                                    |   _ => (loadCode, arg :: loadedArgs)
                                end
                        in
                            val (loadStackArgs, loadedStackArgs) = loadArgs(returnEntry :: stackArgs)
                        end 
                    in
                        BlockExit(TailRecursiveCall{regArgs=closureEntry @ regArgs, stackArgs=loadedStackArgs,
                                  stackAdjust = stackAdjust, currStackSize=stackOffset, callKind=callKind, workReg=newPReg()}) ::
                                    loadStackArgs
                    end
                    else
                    let
                        val call = FunctionCall{regArgs=closureEntry @ regArgs, stackArgs=stackArgs, dest=target, callKind=callKind}
                    in
                        case currHandler of
                            NONE => [BlockSimple call]
                        |   SOME h => [BlockOptionalHandle{call=call, handler=h, label=newLabel()}]
                    end
            in
                (callInstr @ codeArgs, RegisterArgument target, isTail (* We've exited if this was a tail jump *))
            end

        |   codeToICodeRev(BICGetThreadId, _, _, destination, tailCode) =
            (* Get the ID of the current thread. *)
            let
                val target = asTarget destination
            in
                (BlockSimple(LoadMemReg{offset=memRegThreadSelf, dest=target}) :: tailCode, RegisterArgument target, false)
            end

        |   codeToICodeRev(BICUnary instr, context, isTail, destination, tailCode) =
            let
                val (code, dest, haveExited) = codeToICodeUnary(instr, context, isTail, destination)
            in
                (revApp(code, tailCode), dest, haveExited)
            end

        |   codeToICodeRev(BICBinary instr, context, isTail, destination, tailCode) =
            let
                val (code, dest, haveExited) = codeToICodeBinary(instr, context, isTail, destination)
            in
                (revApp(code, tailCode), dest, haveExited)
            end

        |   codeToICodeRev(BICArbitrary{oper, shortCond, arg1, arg2, longCall}, context, _, destination, tailCode) =
            let
                val startLong = newLabel() and resultLabel = newLabel()
                val target = asTarget destination
                val condResult = newPReg()
                (* Overflow check - if there's an overflow jump to the long precision case. *)
                fun jumpOnOverflow ccRef =
                let
                    val noOverFlow = newLabel()
                in
                    [BlockFlow(Conditional{ ccRef=ccRef, condition=JO, trueJump=startLong, falseJump=noOverFlow }),
                     BlockLabel noOverFlow]
                end
                val (longCode, _, _) = codeToICode(longCall, context, false, SpecificPReg condResult)
                
                     (* We could use a tail jump here if this is a tail. *)
                val (code, dest, haveExited) =
                (
                    (* Test the tag bits and skip to the long case if either is clear. *)
                    List.rev(codeConditionRev(shortCond, context, false, startLong, [])) @
                    (* Try evaluating as fixed precision and jump if we get an overflow. *)
                    codeFixedPrecisionArith(oper, arg1, arg2, context, condResult, jumpOnOverflow) @
                    (* If we haven't had an overflow jump to the result. *)
                    [BlockFlow(Unconditional resultLabel),
                     (* If we need to use the full long-precision call we come here. *)
                     BlockLabel startLong] @ longCode @
                    [BlockLabel resultLabel,
                     BlockSimple(LoadArgument{source=RegisterArgument condResult, dest=target, kind=MoveWord})],
                    RegisterArgument target, false)
            in
                (revApp(code, tailCode), dest, haveExited)
            end

        |   codeToICodeRev(BICAllocateWordMemory instr, context, isTail, destination, tailCode) =
            let
                val (code, dest, haveExited) = codeToICodeAllocate(instr, context, isTail, destination)
            in
                (revApp(code, tailCode), dest, haveExited)
            end

        |   codeToICodeRev(BICLambda(lambda as { closure = [], ...}), _, _, destination, tailCode) =
            (* Empty closure - create a constant closure for any recursive calls. *)
            let
                val closure = Address.allocWordData(0w1, Word8.orb (F_mutable, F_words), Address.toMachineWord 0w0)
                val codeAddr = codeFunctionToX86(lambda, debugSwitches, SOME closure)
                open Address
            in
                assignWord(closure, 0w0, toMachineWord codeAddr);
                lock closure;
                moveIfNotAllowedRev(destination, tailCode, AddressConstant(toMachineWord closure))
            end

        |   codeToICodeRev(BICLambda(lambda as { closure, ...}), context, isTail, destination, tailCode) =
            (* Non-empty closure.  Ignore stack closure option at the moment. *)
            let
                val codeAddr = codeFunctionToX86(lambda, debugSwitches, NONE)
            in
                (* Treat it as a tuple with the code as the first field. *)
                codeToICodeRev(BICTuple(BICConstnt(toMachineWord codeAddr, []) :: map BICExtract closure), context, isTail, destination, tailCode)
            end

        |   codeToICodeRev(BICCond(test, thenPt, elsePt), context, isTail, NoResult, tailCode) =
            let
                (* If we don't want the result but are only evaluating for side-effects we
                   may be able to optimise special cases.  This was easier in the forward
                   case but for now we don't bother and leave it to the lower levels. *)
                val startElse = newLabel() and skipElse = newLabel()
                val codeTest = codeConditionRev(test, context, false, startElse, tailCode)
                val (codeThen, _, _) =
                    codeToICodeRev(thenPt, context, isTail, NoResult, codeTest)
                val (codeElse, _, _) =
                     codeToICodeRev(elsePt, context, isTail, NoResult,
                        BlockLabel startElse ::
                        BlockFlow(Unconditional skipElse) :: codeThen)
            in
                (BlockLabel skipElse :: codeElse, (* Unit result *) IntegerConstant(tag 0), false)
            end

        |   codeToICodeRev(BICCond(test, thenPt, elsePt), context, isTail, destination, tailCode) =
            let
                (* Because we may push the result onto the stack we have to create a new preg to
                   hold the result and then copy that to the final result. *)
                (* If this is a tail each arm will exit separately and neither will return a result. *)
                val target = asTarget destination
                val condResult = newPReg()
                val thenTarget = if isTail then newPReg() else condResult
                val startElse = newLabel()
                val testCode = codeConditionRev(test, context, false, startElse, tailCode)
                
                (* Put the result in the target register. *)
                val (thenCode, _, thenExited) = codeToICodeRev(thenPt, context, isTail, SpecificPReg thenTarget, testCode)
                (* Add a jump round the else-part except that if this is a tail we
                   return.  The then-part could have exited e.g. with a raise or a loop. *)
                val (exitThen, thenLabel, elseTarget) =
                    if thenExited then (thenCode, [], target (* Can use original target. *))
                    else if isTail then (returnInstruction(context, thenTarget, thenCode), [], newPReg())
                    else
                    let
                        val skipElse = newLabel()
                    in
                        (BlockFlow(Unconditional skipElse) :: thenCode,
                         [BlockSimple(LoadArgument{source=RegisterArgument condResult, dest=target, kind=MoveWord}),
                          BlockLabel skipElse],
                         condResult)
                    end
                val (elseCode, _, elseExited) =
                    codeToICodeRev(elsePt, context, isTail, SpecificPReg elseTarget,
                        BlockLabel startElse :: exitThen)
                (* Add a return to the else-part if necessary so we will always exit on a tail. *)
                val exitElse =
                    if isTail andalso not elseExited
                    then returnInstruction(context, elseTarget, elseCode) else elseCode
            in
                (thenLabel @ exitElse, RegisterArgument target, isTail orelse thenExited andalso elseExited)
            end

        |   codeToICodeRev(BICCase { cases, test, default, isExhaustive, firstIndex}, context, isTail, destination, tailCode) =
            let
                (* We have to create a new preg for the result in case we need to push
                   it to the stack. *)
                val targetReg = newPReg()
                
                local
                    val initialTestReg = newPReg()
                    val (testCode, _, _) = codeToICodeRev(test, context, false, SpecificPReg initialTestReg, tailCode)
                    (* Subtract the minimum value so the value we're testing is always in the range of
                       (tagged) 0 to the maximum.  It is possible to adjust the value when computing the index
                       but that can lead to overflows during compilation if the minimum is very large or small.
                       We can ignore overflow and allow values to wrap round. *)
                in
                    val (testCode, testReg) =
                        if firstIndex = 0w0
                        then (testCode, initialTestReg)
                        else
                        let
                            val newTestReg = newPReg()
                            val subtract =
                                BlockSimple(ArithmeticFunction{oper=SUB, resultReg=newTestReg, operand1=initialTestReg,
                                                   operand2=IntegerConstant(semitag(Word.toLargeInt firstIndex)), ccRef=newCCRef()})
                        in
                            (subtract :: testCode, newTestReg)
                        end
                end

                val workReg = newPReg()
               
                (* Unless this is exhaustive we need to add a range check. *)
                val (rangeCheck, extraDefaults) =
                    if isExhaustive
                    then (testCode, [])
                    else
                    let
                        val defLab1 = newLabel() 
                        val tReg1 = newPReg()
                        val ccRef1 = newCCRef()
                        (* Since we've subtracted any minimum we only have to check whether the value is greater (unsigned)
                           than the maximum. *)
                        val numberOfCases = LargeInt.fromInt(List.length cases)
                        val continueLab = newLabel()
                        val testCode2 =
                                BlockLabel continueLab ::
                                BlockFlow(Conditional{ccRef=ccRef1, condition=JNB, trueJump=defLab1, falseJump=continueLab}) ::
                                BlockSimple(WordComparison{arg1=tReg1, arg2=IntegerConstant(tag numberOfCases), ccRef=ccRef1}) ::
                                BlockSimple(LoadArgument {source=RegisterArgument testReg, dest=tReg1, kind=MoveWord}) :: testCode
                    in
                        (testCode2, [defLab1])
                    end
                
                (* Make a label for each item in the list. *)
                val codeLabels = map (fn _ => newLabel()) cases
                
                (* Create an exit label in case it's needed. *)
                val labelForExit = if isTail then ~1 (* Illegal label. *) else newLabel()

                (* Generate the code for each of the cases and the default.  We need to put an
                   unconditional branch after each to skip the other cases.
                   TODO: This could be replaced by "returns" if we're at the tail. *)
                fun codeCases (SOME c :: otherCases, startLabel :: otherLabels, tailCode) =
                    let
                        val caseTarget = if isTail then newPReg() else targetReg
                        (* Put in the case with a jump to the end of the sequence. *)
                        val (codeThisCase, _, caseExited) =
                            codeToICodeRev(c, context, isTail, SpecificPReg caseTarget,
                                BlockLabel startLabel :: tailCode) 
                        val exitThisCase =
                            if caseExited then codeThisCase
                            else if isTail then returnInstruction(context, caseTarget, codeThisCase)
                            else BlockFlow(Unconditional labelForExit) :: codeThisCase
                    in
                        codeCases(otherCases, otherLabels, exitThisCase)
                    end

                |   codeCases(NONE :: otherCases, _ :: otherLabels, tailCode) = codeCases(otherCases, otherLabels, tailCode)
                        
                |   codeCases ([], [], tailCode) =
                    let
                        (* We need to add labels for all the gaps we filled and also for a "default" label for
                           the indexed-case instruction itself as well as any range checks. *)
                        fun addDefault (startLabel, NONE, l) = BlockLabel startLabel :: l
                        |   addDefault (_, SOME _, l) = l
                        fun asForward l = BlockLabel l
                        val dLabs = map asForward extraDefaults @ tailCode
                        val defLabels = ListPair.foldlEq addDefault dLabs (codeLabels, cases)
                        val defaultTarget = if isTail then newPReg() else targetReg
                        val (defaultCode, _, defaultExited) =
                            codeToICodeRev(default, context, isTail, SpecificPReg defaultTarget, defLabels)
                    in
                        (* Put in the default.  Because this is the last we don't need to
                           jump round it.  However if this is a tail and we haven't exited
                           we put in a return.  That way the case will always have
                           exited if this is a tail. *)
                         if isTail andalso not defaultExited
                         then returnInstruction(context, defaultTarget, defaultCode)
                         else defaultCode
                    end

                |   codeCases _ = raise InternalError "codeCases: mismatch"
                    
                val codedCases =
                    codeCases(cases, codeLabels,
                        BlockFlow(IndexedBr codeLabels) ::
                        BlockSimple(IndexedCaseOperation{testReg=testReg, workReg=workReg}) ::
                        rangeCheck)
                (* We can now copy to the target.  If we need to push the result this load
                   will be converted into a push. *)
                val target = asTarget destination
                val copyToTarget =
                    if isTail then codedCases
                    else BlockSimple(LoadArgument{source=RegisterArgument targetReg, dest=target, kind=MoveWord}) ::
                            BlockLabel labelForExit :: codedCases
            in
                (copyToTarget, RegisterArgument target, isTail (* We have always exited on a tail. *))
            end

        |   codeToICodeRev(BICBeginLoop {loop, arguments}, context as { stackPtr, currHandler, ...}, isTail, destination, tailCode) =
            let
                val target = asTarget destination
                
                fun codeArgs ([], tailCode) = ([], tailCode)
                |   codeArgs (({value, addr}, _) :: rest, tailCode) =
                    let
                        val pr = newPReg()
                        val () = Array.update(locToPregArray, addr, PregLocation pr)
                        val (code, _, _) = codeToICodeRev(value, context, false, SpecificPReg pr, tailCode)
                        val (pregs, othercode) = codeArgs(rest, code)
                    in
                        (pr::pregs, othercode)
                    end
                val (loopRegs, argCode) = codeArgs(arguments, tailCode)

                val loopLabel = newLabel()
                val (loopBody, _, loopExited) =
                    codeToICodeRev(loop, {loopArgs=SOME (loopRegs, loopLabel, stackPtr), stackPtr=stackPtr, currHandler=currHandler },
                            isTail, SpecificPReg target, BlockLabel loopLabel :: argCode)
            in
                (loopBody, RegisterArgument target, loopExited)
            end

        |   codeToICodeRev(BICLoop args, context as {loopArgs=SOME (loopRegs, loopLabel, loopSp), stackPtr, currHandler, ...}, _, destination, tailCode) =
            let
                val target = asTarget destination
                (* Registers to receive the evaluated arguments.  We can't put the
                   values into the loop variables yet because the values could depend
                   on the current values of the loop variables. *)
                val argPRegs = map(fn _ => newPReg()) args
                val codeArgs =
                    ListPair.foldlEq(fn ((arg, _), pr, l) =>
                        #1 (codeToICodeRev(arg, context, false, SpecificPReg pr, l))) tailCode
                        (args, argPRegs)
                val jumpArgs = ListPair.mapEq(fn (s, l) => (RegisterArgument s, l)) (argPRegs, loopRegs)
                (* If we've allocated a container in the loop we have to remove it before jumping back. *)
                val stackReset =
                    if loopSp = stackPtr then codeArgs
                    else BlockSimple(ResetStackPtr{numWords=stackPtr-loopSp, preserveCC=false}) :: codeArgs
                val jumpLoop = JumpLoop{regArgs=jumpArgs, stackArgs=[], checkInterrupt=SOME[], workReg=NONE}
                (* "checkInterrupt" could result in a Interrupt exception so we treat this like
                   a function call. *)
                val code =
                    case currHandler of
                        NONE => BlockFlow(Unconditional loopLabel) :: BlockSimple jumpLoop :: stackReset
                    |   SOME h => BlockOptionalHandle{call=jumpLoop, handler=h, label=loopLabel} :: stackReset
            in
                (code, RegisterArgument target, true)
            end

        |   codeToICodeRev(BICLoop _, {loopArgs=NONE, ...}, _, _, _) = raise InternalError "BICLoop without BICBeginLoop"


        |   codeToICodeRev(BICRaise exc, context as { currHandler, ...}, _, destination, tailCode) =
            let
                val packetReg = newPReg()
                val (code, _, _) =
                    codeToICodeRev(exc, context, false, SpecificPReg packetReg, tailCode)
                val raiseCode = RaiseExceptionPacket{packetReg=packetReg}
                val block =
                    case currHandler of
                        NONE => BlockExit raiseCode | SOME h => BlockRaiseAndHandle(raiseCode, h)
            in
                (block :: code, RegisterArgument(asTarget destination), true (* Always exits *))
            end

        |   codeToICodeRev(BICHandle{exp, handler, exPacketAddr}, context as { stackPtr, loopArgs, ... }, isTail, destination, tailCode) =
            let
                (* As with BICCond and BICCase we need to create a new register for the
                   result in case we need to push it to the stack. *)
                val handleResult = newPReg()
                val handlerLab = newLabel() and startHandling = newLabel()
                val (bodyTarget, handlerTarget) =
                    if isTail then (newPReg(), newPReg()) else (handleResult, handleResult)
                (* TODO: Even if we don't actually want a result we force one in here by
                   using "asTarget".  *)
                (* The expression cannot be treated as a tail because the handler has
                   to be removed after.  It may "exit" if it has raised an unconditional
                   exception.  If it has we mustn't generate a PopExceptionHandler because
                   there won't be any result for resultReg.
                   We need to add two words to the stack to account for the items pushed by
                   PushExceptionHandler.
                   We create an instruction to push the handler followed by a block fork to
                   the start of the code and, potentially the handler, then a label to start
                   the code that the handler is in effect for. *)
                val initialCode =
                    BlockLabel startHandling ::
                    BlockFlow(SetHandler{handler=handlerLab, continue=startHandling}) ::
                    BlockSimple(PushExceptionHandler{workReg=newPReg()}) :: tailCode
                val (expCode, _, expExit) =
                    codeToICodeRev(exp, {stackPtr=stackPtr+2, loopArgs=loopArgs, currHandler=SOME handlerLab},
                        false (* Not tail *), SpecificPReg bodyTarget, initialCode)
                (* If this is the tail we can replace the jump at the end of the
                   handled code with returns.  If the handler has exited we don't need
                   a return there.  Otherwise we need to add an unconditional jump to
                   skip the handler. *)
                val (atExpEnd, skipExpLabel) =
                    case (isTail, expExit) of
                        (true, true) => (* Tail and exited. *) (expCode, NONE)
                    |   (true, false) => (* Tail and not exited. *)
                            (returnInstruction(context, bodyTarget,
                                BlockSimple(PopExceptionHandler{workReg=newPReg()}) :: expCode), NONE)
                    |   (false, true) => (* Not tail but exited. *) (expCode, NONE)
                    |   (false, false) =>
                        let
                            val skipHandler = newLabel()
                        in
                            (BlockFlow(Unconditional skipHandler) ::
                             BlockSimple(PopExceptionHandler{workReg=newPReg()}) :: expCode, SOME skipHandler)
                        end
                (* Make a register to hold the exception packet and put eax into it. *)
                val packetAddr = newPReg()
                val () = Array.update(locToPregArray, exPacketAddr, PregLocation packetAddr)
                val (handleCode, _, handleExit) =
                    codeToICodeRev(handler, context, isTail, SpecificPReg handlerTarget,
                        BlockSimple(BeginHandler{workReg=newPReg(), packetReg=packetAddr}) :: BlockLabel handlerLab :: atExpEnd)
                val target = asTarget destination
                val afterHandler =
                    case (isTail, handleExit) of
                        (true, true) => (* Tail and exited. *) handleCode
                    |   (true, false) => (* Tail and not exited. *)
                            returnInstruction(context, handlerTarget, handleCode)
                    |   (false, _) => (* Not tail. *) handleCode
                
                val addLabel =
                    case skipExpLabel of
                        SOME lab => BlockLabel lab:: afterHandler
                    |   NONE => afterHandler
            in
                (BlockSimple(LoadArgument{source=RegisterArgument handleResult, dest=target, kind=MoveWord}) :: addLabel,
                    RegisterArgument target, isTail)
            end

        |   codeToICodeRev(BICTuple fields, context, _, destination, tailCode) =
            let
                (* TODO: This is a relic of the old fall-back code-generator.  It required
                   the result of a tuple to be at the top of the stack.  It should be changed. *)
                val target = asTarget destination (* Actually we want this. *)
                val memAddr = newPReg()
                fun loadFields([], n, tlCode) =
                        BlockSimple(AllocateMemoryOperation{size=n, flags=0w0, dest=memAddr, saveRegs=[]}) :: tlCode
                |   loadFields(f :: rest, n, tlCode) =
                    let
                        (* Defer the evaluation if possible.  We may have a constant that we can't move
                           directly but it's better to load it after the allocation otherwise we will
                           have to push the register if we need to GC. *)
                        val (code1, source1, _) = codeToICodeRev(f, context, false, Allowed allowDefer, tlCode)
                        val restAndAlloc = loadFields(rest, n+1, code1)
                        val (code2, source, _)  = moveIfNotAllowedRev(Allowed allowInMemMove, restAndAlloc, source1)
                        val storeValue = BlockSimple(StoreArgument{ source=source, offset=n*wordSize, base=memAddr, index=NoMemIndex, kind=MoveWord, isMutable=false})
                    in
                        storeValue :: code2
                    end
                val code =
                    BlockSimple InitialisationComplete ::
                        BlockSimple(LoadArgument{source=RegisterArgument memAddr, dest=target, kind=MoveWord}) ::
                        loadFields(fields, 0, tailCode)
            in
                (code, RegisterArgument target, false)
            end

            (* Copy the source tuple into the container.  There are important special cases for
               both the source tuple and the container.  If the source tuple is a BICTuple we have
               the fields and can store them without creating a tuple on the heap.  If the
               destination is a local container we can store directly into the stack. *)
        |   codeToICodeRev(BICSetContainer{container, tuple, filter}, context as {stackPtr, ...}, _, destination, tailCode) =
            let
                local
                    fun createStore containerReg (source, destWord) =
                        StoreArgument{source=source, offset=destWord*wordSize, base=containerReg, index=NoMemIndex, kind=MoveWord, isMutable=false}
                in
                    val (codeContainer, storeInstr) =
                        case container of
                            BICExtract(BICLoadLocal l) =>
                            (
                                case Array.sub(locToPregArray, l) of
                                    NoLocation => raise InternalError "codeToICodeRev - local unset"
                                |   PregLocation pr => (tailCode, createStore pr)
                                |   ContainerLocation{container, stackOffset} =>
                                    let
                                        fun storeToStack(source, destWord) =
                                            StoreToStack{source=source, container=container, field=destWord,
                                                stackOffset=stackPtr-stackOffset+destWord}
                                    in
                                        (tailCode, storeToStack)
                                    end
                            )
                        
                        |   _ =>
                            let
                                val containerTarget = newPReg()
                                val (codeContainer, _, _) =
                                    codeToICodeRev(container, context, false, SpecificPReg containerTarget, tailCode)
                            in
                                (codeContainer, createStore containerTarget)
                            end
                end
                
                val filterLength = BoolVector.length filter

                val code =
                    case tuple of
                        BICTuple cl =>
                        let
                            (* In theory it's possible that the tuple could contain fields that are not
                               used but nevertheless need to be evaluated for their side-effects.
                               Create all the fields and push to the stack. *)
                            fun codeField(arg, (regs, tailCode)) =
                            let
                                val (c, r, _) =
                                    codeToICodeRev(arg, context, false, Allowed allowInMemMove, tailCode)
                            in
                                (r :: regs, c)
                            end

                            val (pregsRev, codeFields) = List.foldl codeField ([], codeContainer) cl
                            val pregs = List.rev pregsRev

                            fun copyField(srcReg, (sourceWord, destWord, tailCode)) =
                                if sourceWord < filterLength andalso BoolVector.sub(filter, sourceWord)
                                then (sourceWord+1, destWord+1, BlockSimple(storeInstr(srcReg, destWord)) :: tailCode)
                                else (sourceWord+1, destWord, tailCode)
                            
                            val (_, _, resultCode) = List.foldl copyField (0, 0, codeFields) pregs
                        in
                            resultCode
                        end

                    |   tuple =>
                        let (* Copy a heap tuple.  It is possible that this is another container in which case
                               we must load the fields directly.  We mustn't load its address and then copy
                               because loading the address would be the last reference and might cause
                               the container to be reused prematurely. *)
                            val (codeTuple, loadField) =
                                case tuple of
                                    BICExtract(BICLoadLocal l) =>
                                    (
                                        case Array.sub(locToPregArray, l) of
                                            NoLocation => raise InternalError "codeToICodeRev - local unset"
                                        |   PregLocation pr => (codeContainer, fn sourceWord => wordOffsetAddress(sourceWord, pr))
                                        |   ContainerLocation{container, stackOffset} =>
                                            let
                                                fun getAddr sourceWord =
                                                    StackLocation{wordOffset=stackPtr-stackOffset+sourceWord, container=container,
                                                                  field=sourceWord, cache=NONE}
                                            in
                                                (codeContainer, getAddr)
                                            end
                                   )
                                |   _ =>
                                let
                                    val tupleTarget = newPReg()
                                    val (codeTuple, _, _) = codeToICodeRev(tuple, context, false, SpecificPReg tupleTarget, codeContainer)
                                    fun loadField sourceWord = wordOffsetAddress(sourceWord, tupleTarget)
                                in
                                    (codeTuple, loadField)
                                end

                            fun copyContainer(sourceWord, destWord, tailCode) =
                            if sourceWord = filterLength
                            then tailCode
                            else if BoolVector.sub(filter, sourceWord)
                            then
                            let
                                val loadReg = newPReg()
                                val code =
                                    BlockSimple(storeInstr(RegisterArgument loadReg, destWord)) ::
                                    BlockSimple(LoadArgument{source=loadField sourceWord, dest=loadReg, kind=MoveWord}) ::
                                    tailCode
                            in
                                copyContainer(sourceWord+1, destWord+1, code)
                            end
                            else copyContainer(sourceWord+1, destWord, tailCode)
                        in
                            copyContainer(0, 0, codeTuple)
                        end
            in
                moveIfNotAllowedRev(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeRev(instr as BICTagTest _, context, isTail, destination, tailCode) =
                (* Better handled as a conditional *)
                codeAsConditionalRev(instr, context, isTail, destination, tailCode)

        |   codeToICodeRev(BICLoadOperation instr, context, isTail, destination, tailCode) =
            let
                val (code, dest, haveExited) = codeToICodeLoad(instr, context, isTail, destination)
            in
                (revApp(code, tailCode), dest, haveExited)
            end

        |   codeToICodeRev(BICStoreOperation instr, context, isTail, destination, tailCode) =
            let
                val (code, dest, haveExited) = codeToICodeStore(instr, context, isTail, destination)
            in
                (revApp(code, tailCode), dest, haveExited)
            end

        |   codeToICodeRev(BICBlockOperation (instr as {kind=BlockOpEqualByte, ...}), context, isTail, destination, tailCode) =
                codeAsConditionalRev(BICBlockOperation instr, context, isTail, destination, tailCode)

        |   codeToICodeRev(BICBlockOperation instr, context, isTail, destination, tailCode) =
            let
                val (code, dest, haveExited) = codeToICodeBlock(instr, context, isTail, destination)
            in
                (revApp(code, tailCode), dest, haveExited)
            end

        and codeToICodeUnary({oper=BuiltIns.NotBoolean, arg1}, context, _, destination) =
            let
                (* TODO: If the argument is something that will be a conditional we would be better off
                   generating a conditional here. *)
                val target = asTarget destination
                val ccRef = newCCRef()
                val (argCode, tReg) = codeToPReg(arg1, context)
            in
                (argCode @
                    [BlockSimple(ArithmeticFunction{oper=XOR, resultReg=target, operand1=tReg, operand2=IntegerConstant(semitag 1), ccRef=ccRef})],
                        RegisterArgument target, false)
            end

        |   codeToICodeUnary(instr as {oper=BuiltIns.IsTaggedValue, ...}, context, isTail, destination) =
                codeAsConditional(BICUnary instr, context, isTail, destination)

        |   codeToICodeUnary({oper=BuiltIns.MemoryCellLength, arg1}, context, _, destination) =
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
                [BlockSimple(LoadArgument{source=wordOffsetAddress(~1, addrReg), dest=argReg1, kind=MoveWord}),
                 BlockSimple(ShiftOperation{shift=SHL, resultReg=argReg2, operand=argReg1, shiftAmount=IntegerConstant 8, ccRef=ccRef1 }),
                 BlockSimple(ShiftOperation{shift=SHR, resultReg=argReg3, operand=argReg2, shiftAmount=IntegerConstant 7 (* 8-tagshift*), ccRef=ccRef2 }),
                 BlockSimple(ArithmeticFunction{oper=OR, resultReg=target, operand1=argReg3, operand2=IntegerConstant 1, ccRef=ccRef3})], RegisterArgument target, false)
            end

        |   codeToICodeUnary({oper=BuiltIns.MemoryCellFlags, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val argReg1 = newUReg()
                val (argCode, addrReg) = codeToPReg(arg1, context)
            in
                (argCode @
                [BlockSimple(LoadArgument{source=MemoryLocation{offset= ~1, base=addrReg, index=NoMemIndex, cache=NONE}, dest=argReg1, kind=MoveByte}),
                 BlockSimple(TagValue{ source=argReg1, dest=target, isSigned=false })], RegisterArgument target, false)
            end

        |   codeToICodeUnary({oper=BuiltIns.ClearMutableFlag, arg1}, context, _, destination) =
            let
                val (argCode, addrReg) = codeToPReg(arg1, context)
                val code = argCode @ [BlockSimple(LockMutable{addr=addrReg})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeUnary({oper=BuiltIns.AtomicIncrement, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val incrReg = newUReg()
                val (argCode, addrReg) = codeToPReg(arg1, context)
            in
                (argCode @
                 [BlockSimple(LoadArgument{source=IntegerConstant(semitag 1), dest=incrReg, kind=MoveWord}),
                  BlockSimple(AtomicExchangeAndAdd{ base=addrReg, source=incrReg }),
                  (* We want the result to be the new value but we've returned the old value. *)
                  BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=incrReg, operand2=IntegerConstant(semitag 1), ccRef=newCCRef()})],
                    RegisterArgument target, false)
            end

        |   codeToICodeUnary({oper=BuiltIns.AtomicDecrement, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val incrReg = newUReg()
                val (argCode, addrReg) = codeToPReg(arg1, context)
            in
                (argCode @
                 [BlockSimple(LoadArgument{source=IntegerConstant(semitag ~1), dest=incrReg, kind=MoveWord}),
                  BlockSimple(AtomicExchangeAndAdd{ base=addrReg, source=incrReg }),
                  BlockSimple(ArithmeticFunction{oper=SUB, resultReg=target, operand1=incrReg, operand2=IntegerConstant(semitag 1), ccRef=newCCRef()})],
                    RegisterArgument target, false)
            end

        |   codeToICodeUnary({oper=BuiltIns.AtomicReset, arg1}, context, _, destination) =
            let
                (* This is needed only for the interpreted version where we have a single real
                   mutex to interlock atomic increment and decrement.  We have to use the same
                   mutex to interlock clearing a mutex.  On the X86 we use hardware locking and
                   the hardware guarantees that an assignment of a word will be atomic. *)
                val (argCode, addrReg) = codeToPReg(arg1, context)
                (* Store tagged 1 in the mutex.  This is the unlocked value. *)
                val code = argCode @
                    [BlockSimple(StoreArgument{source=IntegerConstant(tag 1), base=addrReg, index=NoMemIndex, offset=0, kind=MoveWord, isMutable=true})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeUnary({oper=BuiltIns.LongWordToTagged, arg1}, context, _, destination) =
            let (* This is exactly the same as StringLengthWord at the moment.
                   TODO: introduce a new ICode entry so that the next stage can optimise
                   longword operations. *)
                val target = asTarget destination
                val argReg1 = newUReg()
                val (argCode, addrReg) = codeToPReg(arg1, context)
            in
                (argCode @
                [BlockSimple(LoadArgument{source=MemoryLocation{offset=0, base=addrReg, index=NoMemIndex, cache=NONE}, dest=argReg1, kind=MoveWord}),
                 (* Is this signed or unsigned? *)
                 BlockSimple(TagValue{ source=argReg1, dest=target, isSigned=false })], RegisterArgument target, false)
            end

        |   codeToICodeUnary({oper=BuiltIns.SignedToLongWord, arg1}, context, _, destination) =
            let
                val addrReg = newPReg() and untagArg = newUReg()
                val (argCode, argReg1) = codeToPReg(arg1, context)
                val code =
                    argCode @
                    [BlockSimple(UntagValue{source=argReg1, dest=untagArg, isSigned=true, cache=NONE}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord,  source=untagArg, dest=addrReg, saveRegs=[]})]
            in
                moveIfNotAllowed(destination, code, RegisterArgument addrReg)
            end

        |   codeToICodeUnary({oper=BuiltIns.UnsignedToLongWord, arg1}, context, _, destination) =
            let
                val addrReg = newPReg() and untagArg = newUReg()
                val (argCode, argReg1) = codeToPReg(arg1, context)
                val code =
                    argCode @
                    [BlockSimple(UntagValue{source=argReg1, dest=untagArg, isSigned=false, cache=NONE}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord,  source=untagArg, dest=addrReg, saveRegs=[]})]
            in
                moveIfNotAllowed(destination, code, RegisterArgument addrReg)
            end

        |   codeToICodeUnary({oper=BuiltIns.RealNeg, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val fpRegSrc = newUReg() and fpRegDest = newUReg() and sse2ConstReg = newUReg()
                (* The SSE2 code uses an SSE2 logical operation to flip the sign bit.  This
                   requires the values to be loaded into registers first because the logical
                   operations require 128-bit operands. *)
                val (argCode, aReg1) = codeToPReg(arg1, context)
                val load = BlockSimple(LoadArgument{source=wordAt aReg1, dest=fpRegSrc, kind=MoveDouble})
                val code =
                    case fpMode of
                        FPModeX87 =>
                            [BlockSimple(X87FPUnaryOps{ fpOp=FCHS, dest=fpRegDest, source=fpRegSrc})]
                    |   FPModeSSE2 =>
                            [BlockSimple(LoadArgument{source=AddressConstant realSignBit, dest=sse2ConstReg, kind=MoveDouble}),
                             BlockSimple(SSE2FPArith{opc=SSE2Xor, resultReg=fpRegDest, arg1=fpRegSrc, arg2=RegisterArgument sse2ConstReg})]
                val boxFloat = case fpMode of FPModeX87 => BoxX87 | FPModeSSE2 => BoxSSE2
            in
                (argCode @
                 load :: code @ [BlockSimple(BoxValue{boxKind=boxFloat, source=fpRegDest, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeUnary({oper=BuiltIns.RealAbs, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val fpRegSrc = newUReg() and fpRegDest = newUReg() and sse2ConstReg = newUReg()
                val (argCode, aReg1) = codeToPReg(arg1, context)
                val load = BlockSimple(LoadArgument{source=wordAt aReg1, dest=fpRegSrc, kind=MoveDouble})
                val code =
                    case fpMode of
                        FPModeX87 => [BlockSimple(X87FPUnaryOps{ fpOp=FABS, dest=fpRegDest, source=fpRegSrc})]
                    |   FPModeSSE2 =>
                            [BlockSimple(LoadArgument{source=AddressConstant realAbsMask, dest=sse2ConstReg, kind=MoveDouble}),
                             BlockSimple(SSE2FPArith{opc=SSE2And, resultReg=fpRegDest, arg1=fpRegSrc, arg2=RegisterArgument sse2ConstReg})]
                val boxFloat = case fpMode of FPModeX87 => BoxX87 | FPModeSSE2 => BoxSSE2
            in
                (argCode @
                 load :: code @ [BlockSimple(BoxValue{boxKind=boxFloat, source=fpRegDest, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeUnary({oper=BuiltIns.FloatFixedInt, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val untagReg = newUReg() and fpReg = newUReg()
                val (argCode, aReg1) = codeToPReg(arg1, context)
                val floatOp = case fpMode of FPModeX87 => X87Float | FPModeSSE2 => SSE2Float
                val boxFloat = case fpMode of FPModeX87 => BoxX87 | FPModeSSE2 => BoxSSE2
            in
                (argCode @
                 [BlockSimple(UntagValue{source=aReg1, dest=untagReg, isSigned=true, cache=NONE}),
                  BlockSimple(floatOp{ dest=fpReg, source=RegisterArgument untagReg}),
                  BlockSimple(BoxValue{boxKind=boxFloat, source=fpReg, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        and codeToICodeBinary(instr as {oper=BuiltIns.WordComparison _, ...}, context, isTail, destination) =
                codeAsConditional(BICBinary instr, context, isTail, destination)

        |   codeToICodeBinary({oper=BuiltIns.FixedPrecisionArith oper, arg1, arg2}, context as {currHandler, ...}, _, destination) =
            let
                val target = asTarget destination
                fun checkOverflow ccRef =
                let
                    val overFlowLab = newLabel() and noOverflowLab = newLabel()
                    val packetReg = newPReg()
                    val raiseCode = RaiseExceptionPacket{packetReg=packetReg}
                in
                    [
                        BlockFlow(Conditional{ ccRef=ccRef, condition=JNO, trueJump=noOverflowLab, falseJump=overFlowLab }),
                        BlockLabel overFlowLab,
                        BlockSimple(LoadArgument{source=AddressConstant(toMachineWord(Overflow)), dest=packetReg, kind=MoveWord}),
                        case currHandler of NONE => BlockExit raiseCode | SOME h => BlockRaiseAndHandle(raiseCode, h),
                        BlockLabel noOverflowLab
                    ]
                end
            in
                (codeFixedPrecisionArith(oper, arg1, arg2, context, target, checkOverflow), RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
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
                    [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg1, operand2=IntegerConstant constVal, ccRef = newCCRef()})],
                        RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1=BICConstnt(value, _), arg2}, context, _, destination) =
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
                    [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg2, operand2=IntegerConstant constVal, ccRef = newCCRef()})],
                        RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                (* Use LEA to do the addition since we're not concerned with overflow.  This is shorter than
                   subtracting the tag and adding the values and also moves the result into the
                   appropriate register. *)
            in
                (arg1Code @ arg2Code @
                    [BlockSimple(LoadEffectiveAddress{base=SOME aReg1, offset= ~1, index=MemIndex1 aReg2, dest=target})],
                        RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.WordArith BuiltIns.ArithSub, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
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
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=target, operand1=aReg1, operand2=IntegerConstant constVal, ccRef=newCCRef()})],
                        RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.WordArith BuiltIns.ArithSub, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg3 = newPReg()
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
            in
                (arg1Code @ arg2Code @
                    (* Do the subtraction and add in the tag bit.  This could be reordered if we have cascaded operations
                       since we don't need to check for overflow. *)
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=aReg1, operand2=RegisterArgument aReg2, ccRef=newCCRef()}),
                     BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg3, operand2=IntegerConstant 1, ccRef=newCCRef()})], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.WordArith BuiltIns.ArithMult, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
                codeMultiplyConstantWord(arg1, context, destination, if isShort value then toShort value else 0w0)

        |   codeToICodeBinary({oper=BuiltIns.WordArith BuiltIns.ArithMult, arg1=BICConstnt(value, _), arg2}, context, _, destination) =
                codeMultiplyConstantWord(arg2, context, destination, if isShort value then toShort value else 0w0)

        |   codeToICodeBinary({oper=BuiltIns.WordArith BuiltIns.ArithMult, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val arg1Untagged = newUReg()
                and arg2Untagged = newUReg() and resUntagged = newUReg()
            in
                (arg1Code @ arg2Code @
                    (* Shift one argument and subtract the tag from the other.  It's possible this could be reordered
                       if we have a value that is already untagged. *)
                    [BlockSimple(UntagValue{source=aReg1, dest=arg1Untagged, isSigned=false, cache=NONE}),
                     BlockSimple(ArithmeticFunction{oper=SUB, resultReg=arg2Untagged, operand1=aReg2, operand2=IntegerConstant 1, ccRef=newCCRef()}),
                     BlockSimple(Multiplication{resultReg=resUntagged, operand1=arg1Untagged, operand2=RegisterArgument arg2Untagged, ccRef=newCCRef()}),
                     BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=resUntagged, operand2=IntegerConstant 1, ccRef=newCCRef()})],
                        RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.WordArith BuiltIns.ArithDiv, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                (arg1Code @ arg2Code @
                    (* Shift both of the arguments to remove the tags.  We don't test for zero here - that's done explicitly. *)
                    [BlockSimple(UntagValue{source=aReg1, dest=arg1Untagged, isSigned=false, cache=NONE}),
                     BlockSimple(UntagValue{source=aReg2, dest=arg2Untagged, isSigned=false, cache=NONE}),
                     BlockSimple(Division { isSigned = false, dividend=arg1Untagged, divisor=RegisterArgument arg2Untagged,
                                quotient=quotient, remainder=remainder }),
                     BlockSimple(TagValue { source=quotient, dest=target, isSigned=false })], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.WordArith BuiltIns.ArithMod, arg1, arg2}, context, _, destination) =
            let
                (* Identical to Quot except that the result is the remainder. *)
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                (arg1Code @ arg2Code @
                    (* Shift both of the arguments to remove the tags. *)
                    [BlockSimple(UntagValue{source=aReg1, dest=arg1Untagged, isSigned=false, cache=NONE}),
                     BlockSimple(UntagValue{source=aReg2, dest=arg2Untagged, isSigned=false, cache=NONE}),
                     BlockSimple(Division { isSigned = false, dividend=arg1Untagged, divisor=RegisterArgument arg2Untagged,
                                quotient=quotient, remainder=remainder }),
                     BlockSimple(TagValue { source=remainder, dest=target, isSigned=false })], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.WordArith _, ...}, _, _, _) =
                raise InternalError "codeToICodeNonRev: WordArith - unimplemented operation"

        |   codeToICodeBinary({oper=BuiltIns.WordLogical logOp, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg1Code, arg1Reg) = codeToPReg(arg1, context)
                (* Use a semitagged value for XOR.  This preserves the tag bit. *)
                val constVal =
                    if isShort value
                    then (case logOp of BuiltIns.LogicalXor => semitag | _ => tag) (Word.toLargeIntX(toShort value))
                    else 0
                val oper = case logOp of BuiltIns.LogicalOr => OR | BuiltIns.LogicalAnd => AND | BuiltIns.LogicalXor => XOR
            in
                (arg1Code @
                    [BlockSimple(ArithmeticFunction{oper=oper, resultReg=target, operand1=arg1Reg, operand2=IntegerConstant constVal, ccRef=newCCRef()})],
                        RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.WordLogical logOp, arg1=BICConstnt(value, _), arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg2Code, arg2Reg) = codeToPReg(arg2, context)
                (* Use a semitagged value for XOR.  This preserves the tag bit. *)
                val constVal =
                    if isShort value
                    then (case logOp of BuiltIns.LogicalXor => semitag | _ => tag) (Word.toLargeIntX(toShort value))
                    else 0
                val oper = case logOp of BuiltIns.LogicalOr => OR | BuiltIns.LogicalAnd => AND | BuiltIns.LogicalXor => XOR
            in
                (arg2Code @
                    [BlockSimple(ArithmeticFunction{oper=oper, resultReg=target, operand1=arg2Reg, operand2=IntegerConstant constVal, ccRef=newCCRef()})],
                        RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.WordLogical BuiltIns.LogicalOr, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg1Code, arg1Reg) = codeToPReg(arg1, context)
                val (arg2Code, arg2Reg) = codeToPReg(arg2, context)
            in
                (arg1Code @ arg2Code @
                    (* Or-ing preserves the tag bit. *)
                    [BlockSimple(ArithmeticFunction{oper=OR, resultReg=target, operand1=arg1Reg, operand2=RegisterArgument arg2Reg, ccRef=newCCRef()})], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.WordLogical BuiltIns.LogicalAnd, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg1Code, arg1Reg) = codeToPReg(arg1, context)
                val (arg2Code, arg2Reg) = codeToPReg(arg2, context)
            in
                (arg1Code @ arg2Code @
                    (* Since they're both tagged the result will be tagged. *)
                    [BlockSimple(ArithmeticFunction{oper=AND, resultReg=target, operand1=arg1Reg, operand2=RegisterArgument arg2Reg, ccRef=newCCRef()})], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.WordLogical BuiltIns.LogicalXor, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg1Code, arg1Reg) = codeToPReg(arg1, context)
                val (arg2Code, arg2Reg) = codeToPReg(arg2, context)
                val aReg3 = newPReg()
            in
                (arg1Code @ arg2Code @
                    (* We need to restore the tag bit after the operation. *)
                    [BlockSimple(ArithmeticFunction{oper=XOR, resultReg=aReg3, operand1=arg1Reg, operand2=RegisterArgument arg2Reg, ccRef=newCCRef()}),
                     BlockSimple(ArithmeticFunction{oper=OR, resultReg=target, operand1=aReg3, operand2=IntegerConstant 1, ccRef=newCCRef()})], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.WordShift BuiltIns.ShiftLeft, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
                (* Use the general case multiplication code.  This will use a shift except for small values.
                   It does detect special cases such as multiplication by 4 and 8 which can be implemented with LEA. *)
                codeMultiplyConstantWord(arg1, context, destination, if isShort value then Word.<<(0w1, toShort value) else 0w1)

        |   codeToICodeBinary({oper=BuiltIns.WordShift shift, arg1, arg2}, context, _, destination) =
                (* N.B.  X86 shifts of greater than the word length mask the higher bits.  That isn't what ML wants
                   but that is dealt with at a higher level *)
            let
                open BuiltIns
                val target = asTarget destination
                (* Load the value into an untagged register.  If this is a left shift we
                   need to clear the tag bit.  We don't need to do that for right shifts.  *)
                val argRegUntagged = newUReg()
                val arg1Code =
                    case arg1 of
                        BICConstnt(value, _) =>
                        let
                            (* Remove the tag bit.  This isn't required for right shifts. *)
                            val cnstntVal = if isShort value then semitag(Word.toLargeInt(toShort value)) else 1
                        in
                            [BlockSimple(LoadArgument{source=IntegerConstant cnstntVal, dest=argRegUntagged, kind=MoveWord})]
                        end
                    |   _ =>
                        let
                            val (arg1Code, arg1Reg) = codeToPReg(arg1, context)
                            val removeTag =
                                case shift of
                                    ShiftLeft =>
                                        ArithmeticFunction{oper=SUB, resultReg=argRegUntagged, operand1=arg1Reg,
                                                        operand2=IntegerConstant 1, ccRef=newCCRef()}
                                |   _ => LoadArgument{source=RegisterArgument arg1Reg, dest=argRegUntagged, kind=MoveWord}
                        in
                            arg1Code @ [BlockSimple removeTag]
                        end

                (* The shift amount can usefully be a constant. *)
                val (arg2Code, untag2Code, arg2Arg) = codeAsUntaggedValue(arg2, false, context)
                val resRegUntagged = newUReg()
                val shiftOp = case shift of ShiftLeft => SHL | ShiftRightLogical => SHR | ShiftRightArithmetic => SAR
            in
                (arg1Code @ arg2Code @ untag2Code @
                 [BlockSimple(ShiftOperation{ shift=shiftOp, resultReg=resRegUntagged, operand=argRegUntagged, shiftAmount=arg2Arg, ccRef=newCCRef() }),
                  (* Set the tag by ORing it in.  This will work whether or not a right shift has shifted a 1 into this position. *)
                  BlockSimple(
                    ArithmeticFunction{oper=OR, resultReg=target, operand1=resRegUntagged,
                                       operand2=IntegerConstant 1, ccRef=newCCRef()})], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.AllocateByteMemory, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val sizeReg = newPReg() and baseReg = newPReg()
                val sizeCode = codeToICodeTarget(arg1, context, false, sizeReg)
                val (flagsCode, flagUntag, flagArg) = codeAsUntaggedValue(arg2, false, context)
            in
                (sizeCode @ flagsCode @
                 [BlockSimple(AllocateMemoryVariable{size=sizeReg, dest=baseReg, saveRegs=[]})] @
                  flagUntag @
                  [BlockSimple(StoreArgument{ source=flagArg, base=baseReg, offset= ~1, index=NoMemIndex, kind=MoveByte, isMutable=false}),
                  BlockSimple InitialisationComplete,
                  BlockSimple(LoadArgument{ source=RegisterArgument baseReg, dest=target, kind=MoveWord})], RegisterArgument target, false)
            end

        |   codeToICodeBinary(instr as {oper=BuiltIns.LargeWordComparison _, ...}, context, isTail, destination) =
                codeAsConditional(BICBinary instr, context, isTail, destination)

        |   codeToICodeBinary({oper=BuiltIns.LargeWordArith BuiltIns.ArithAdd, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val aReg3 = newUReg()
                val argReg = newUReg()
                val constantValue = largeWordConstant value
            in
                (arg1Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord}),
                     BlockSimple(ArithmeticFunction{oper=ADD, resultReg=aReg3, operand1=argReg, operand2=IntegerConstant constantValue, ccRef=newCCRef()}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.LargeWordArith BuiltIns.ArithAdd, arg1=BICConstnt(value, _), arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val aReg3 = newUReg()
                val argReg = newUReg()
                val constantValue = largeWordConstant value
            in
                (arg2Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg2, dest=argReg, kind=MoveWord}),
                     BlockSimple(ArithmeticFunction{oper=ADD, resultReg=aReg3, operand1=argReg, operand2=IntegerConstant constantValue, ccRef=newCCRef()}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.LargeWordArith BuiltIns.ArithAdd, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val aReg3 = newUReg()
                val argReg = newUReg()
            in
                (arg1Code @ arg2Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord}),
                     BlockSimple(ArithmeticFunction{oper=ADD, resultReg=aReg3, operand1=argReg, operand2=wordAt aReg2, ccRef=newCCRef()}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.LargeWordArith BuiltIns.ArithSub, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val aReg3 = newUReg()
                val argReg = newUReg()
                val constantValue = largeWordConstant value
            in
                (arg1Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord}),
                     BlockSimple(ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=argReg, operand2=IntegerConstant constantValue, ccRef=newCCRef()}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.LargeWordArith BuiltIns.ArithSub, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val aReg3 = newUReg()
                val argReg = newUReg()
            in
                (arg1Code @ arg2Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord}),
                     BlockSimple(ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=argReg, operand2=wordAt aReg2, ccRef=newCCRef()}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.LargeWordArith BuiltIns.ArithMult, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val resValue = newUReg()
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val argReg1 = newUReg()
            in
                (arg1Code @ arg2Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg1, kind=MoveWord}),
                     BlockSimple(Multiplication{resultReg=resValue, operand1=argReg1, operand2=wordAt aReg2, ccRef=newCCRef()}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=resValue, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.LargeWordArith BuiltIns.ArithDiv, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val quotient = newUReg() and remainder = newUReg()
                val dividendReg = newUReg() and divisorReg = newUReg()
            in
                (arg1Code @ arg2Code @
                    (* We don't test for zero here - that's done explicitly. *)
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=dividendReg, kind=MoveWord}),
                     BlockSimple(LoadArgument{source=wordAt aReg2, dest=divisorReg, kind=MoveWord}),
                     BlockSimple(Division { isSigned = false, dividend=dividendReg, divisor=RegisterArgument divisorReg,
                                quotient=quotient, remainder=remainder }),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=quotient, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.LargeWordArith BuiltIns.ArithMod, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val quotient = newUReg() and remainder = newUReg()
                val dividendReg = newUReg() and divisorReg = newUReg()
            in
                (arg1Code @ arg2Code @
                    (* We don't test for zero here - that's done explicitly. *)
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=dividendReg, kind=MoveWord}),
                     BlockSimple(LoadArgument{source=wordAt aReg2, dest=divisorReg, kind=MoveWord}),
                     BlockSimple(Division { isSigned = false, dividend=dividendReg, divisor=RegisterArgument divisorReg,
                                quotient=quotient, remainder=remainder }),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=remainder, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.LargeWordArith _, ...}, _, _, _) =
                raise InternalError "codeToICodeNonRev: LargeWordArith - unimplemented operation"

        |   codeToICodeBinary({oper=BuiltIns.LargeWordLogical logOp, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val aReg3 = newUReg()
                val argReg = newUReg()
                val constantValue = largeWordConstant value
                val oper = case logOp of BuiltIns.LogicalOr => OR | BuiltIns.LogicalAnd => AND | BuiltIns.LogicalXor => XOR
            in
                (arg1Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord}),
                     BlockSimple(ArithmeticFunction{oper=oper, resultReg=aReg3, operand1=argReg, operand2=IntegerConstant constantValue, ccRef=newCCRef()}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.LargeWordLogical logOp, arg1=BICConstnt(value, _), arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val aReg3 = newUReg()
                val argReg = newUReg()
                val constantValue = largeWordConstant value
                val oper = case logOp of BuiltIns.LogicalOr => OR | BuiltIns.LogicalAnd => AND | BuiltIns.LogicalXor => XOR
            in
                (arg2Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg2, dest=argReg, kind=MoveWord}),
                     BlockSimple(ArithmeticFunction{oper=oper, resultReg=aReg3, operand1=argReg, operand2=IntegerConstant constantValue, ccRef=newCCRef()}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.LargeWordLogical logOp, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val aReg3 = newUReg()
                val argReg = newUReg()
               val oper = case logOp of BuiltIns.LogicalOr => OR | BuiltIns.LogicalAnd => AND | BuiltIns.LogicalXor => XOR
            in
                (arg1Code @ arg2Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord}),
                     BlockSimple(ArithmeticFunction{oper=oper, resultReg=aReg3, operand1=argReg, operand2=wordAt aReg2, ccRef=newCCRef()}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.LargeWordShift shift, arg1, arg2}, context, _, destination) =
                (* The shift is always a Word.word value i.e. tagged.  There is a check at the higher level
                   that the shift does not exceed 32/64 bits. *)
            let
                open BuiltIns
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, untag2Code, arg2Arg) = codeAsUntaggedValue(arg2, false, context)
                val aReg3 = newUReg()
                val shiftOp = case shift of ShiftLeft => SHL | ShiftRightLogical => SHR | ShiftRightArithmetic => SAR
                val argReg = newUReg()
            in
                (arg1Code @ arg2Code @ [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord})] @ untag2Code @
                 [BlockSimple(ShiftOperation{ shift=shiftOp, resultReg=aReg3, operand=argReg, shiftAmount=arg2Arg, ccRef=newCCRef() }),
                  BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeBinary({oper=BuiltIns.RealArith fpOper, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                and (arg2Code, aReg2) = codeToPReg(arg2, context)
                val fpRegSrc = newUReg() and fpRegDest = newUReg()
                val load = BlockSimple(LoadArgument{source=wordAt aReg1, dest=fpRegSrc, kind=MoveDouble})
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
                                |   _ => raise InternalError "codeToICodeNonRev: RealArith - unimplemented operation"
                        in
                            BlockSimple(X87FPArith{ opc=fpOp, resultReg=fpRegDest, arg1=fpRegSrc, arg2=wordAt aReg2})
                        end
                    |   FPModeSSE2 =>
                        let
                            val fpOp =
                                case fpOper of
                                    ArithAdd => SSE2Add
                                |   ArithSub => SSE2Sub
                                |   ArithMult => SSE2Mul
                                |   ArithDiv => SSE2Div
                                |   _ => raise InternalError "codeToICodeNonRev: RealArith - unimplemented operation"
                        in
                            BlockSimple(SSE2FPArith{ opc=fpOp, resultReg=fpRegDest, arg1=fpRegSrc, arg2=wordAt aReg2})
                        end
                val boxFloat = case fpMode of FPModeX87 => BoxX87 | FPModeSSE2 => BoxSSE2
            in
                (arg1Code @ arg2Code @
                    [load, arith, BlockSimple(BoxValue{boxKind=boxFloat, source=fpRegDest, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeBinary(instr as {oper=BuiltIns.RealComparison _, ...}, context, isTail, destination) =
                codeAsConditional(BICBinary instr, context, isTail, destination)
        
        (* Multiply tagged word by a constant.  We're not concerned with overflow so it's possible to use
           various short cuts. *)
        and codeMultiplyConstantWord(arg, context, destination, multiplier) =
        let
            val target = asTarget destination
            val (argCode, aReg) = codeToPReg(arg, context)
            
            val doMultiply =
                case multiplier of
                    0w0 => [BlockSimple(LoadArgument{source=IntegerConstant 1, dest=target, kind=MoveWord})]
                |   0w1 => [BlockSimple(LoadArgument{source=RegisterArgument aReg, dest=target, kind=MoveWord})]
                |   0w2 => [BlockSimple(LoadEffectiveAddress{base=SOME aReg, offset= ~1, index=MemIndex1 aReg, dest=target})]
                |   0w3 => [BlockSimple(LoadEffectiveAddress{base=SOME aReg, offset= ~2, index=MemIndex2 aReg, dest=target})]
                |   0w4 => [BlockSimple(LoadEffectiveAddress{base=NONE, offset= ~3, index=MemIndex4 aReg, dest=target})]
                |   0w5 => [BlockSimple(LoadEffectiveAddress{base=SOME aReg, offset= ~4, index=MemIndex4 aReg, dest=target})]
                |   0w8 => [BlockSimple(LoadEffectiveAddress{base=NONE, offset= ~7, index=MemIndex8 aReg, dest=target})]
                |   0w9 => [BlockSimple(LoadEffectiveAddress{base=SOME aReg, offset= ~8, index=MemIndex8 aReg, dest=target})]
                
                |   _ =>
                    let
                        val tReg = newUReg()
                        val tagCorrection = Word.toLargeInt multiplier - 1
                        fun getPower2 n =
                        let
                            fun p2 (n, l) =
                                if n = 0w1 then SOME l
                                else if Word.andb(n, 0w1) = 0w1 then NONE
                                else p2(Word.>>(n, 0w1), l+0w1)
                        in
                            if n = 0w0 then NONE else p2(n,0w0)
                        end
                        val multiply =
                            case getPower2 multiplier of
                                SOME power =>
                                    (* Shift it including the tag. *)
                                    BlockSimple(ShiftOperation{ shift=SHL, resultReg=tReg, operand=aReg,
                                        shiftAmount=IntegerConstant(Word.toLargeInt power), ccRef=newCCRef() })
                            |   NONE => (* Multiply including the tag. *)
                                    BlockSimple(Multiplication{resultReg=tReg, operand1=aReg,
                                        operand2=IntegerConstant(Word.toLargeInt multiplier), ccRef=newCCRef()})
                    in
                        [multiply,
                            BlockSimple(ArithmeticFunction{oper=SUB, resultReg=target, operand1=tReg,
                                operand2=IntegerConstant tagCorrection, ccRef=newCCRef()})]
                    end
        in
            
            (argCode @ doMultiply, RegisterArgument target, false)
        end

        and codeToICodeAllocate({numWords as BICConstnt(length, _), flags as BICConstnt(flagValue, _), initial}, context, _, destination) =
            (* Constant length and flags is used for ref.  We could handle other cases. *)
            if  isShort length andalso isShort flagValue andalso toShort length = 0w1
            then
            let
                val target = asTarget destination (* Force a different register. *)
                val vecLength = Word.toInt(toShort length)
                val flagByte = Word8.fromLargeWord(Word.toLargeWord(toShort flagValue))
                val memAddr = newPReg() and valueReg = newPReg()
                fun initialise n =
                    BlockSimple(StoreArgument{ source=RegisterArgument valueReg, offset=n*wordSize, base=memAddr, index=NoMemIndex, kind=MoveWord, isMutable=false})
                val code =
                    codeToICodeTarget(initial, context, false, valueReg) @
                    [BlockSimple(AllocateMemoryOperation{size=vecLength, flags=flagByte, dest=memAddr, saveRegs=[]})] @
                    List.tabulate(vecLength, initialise) @
                    [BlockSimple InitialisationComplete,
                     BlockSimple(LoadArgument{source=RegisterArgument memAddr, dest=target, kind=MoveWord})]
            in
                (code, RegisterArgument target, false)
            end
            else (* If it's longer use the full run-time form. *)
                allocateMemoryVariable(numWords, flags, initial, context, destination)

        |   codeToICodeAllocate({numWords, flags, initial}, context, _, destination) =
                allocateMemoryVariable(numWords, flags, initial, context, destination)


        and codeToICodeLoad({kind=LoadStoreMLWord _, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeAddress(address, false, context)
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument {source=MemoryLocation memLoc, dest=target, kind=MoveWord})], RegisterArgument target, false)
            end

        |   codeToICodeLoad({kind=LoadStoreMLByte _, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeAddress(address, true, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveByte}),
                     BlockSimple(TagValue {source=untaggedResReg, dest=target, isSigned=false})], RegisterArgument target, false)
            end

        |   codeToICodeLoad({kind=LoadStoreC8, address}, context, _, destination) =
            let
                (* Load a byte from C memory.  This is almost exactly the same as LoadStoreMLByte except
                   that the base address is a LargeWord.word value. *)
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w1, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveByte}),
                     BlockSimple(TagValue {source=untaggedResReg, dest=target, isSigned=false})], RegisterArgument target, false)
            end

        |   codeToICodeLoad({kind=LoadStoreC16, address}, context, _, destination) =
            let
                (* Load a 16-bit value from C memory. *)
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w2, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=Move16Bit}),
                     BlockSimple(TagValue {source=untaggedResReg, dest=target, isSigned=false})], RegisterArgument target, false)
            end

        |   codeToICodeLoad({kind=LoadStoreC32, address}, context, _, destination) =
            let
                (* Load a 32-bit value from C memory.  If this is 64-bit mode we can tag it but
                   if this is 32-bit mode we need to box it. *)
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w4, context)
                val untaggedResReg = newUReg()
                val (boxTagCode, moveKind) =
                    if isX64
                    then (BlockSimple(TagValue {source=untaggedResReg, dest=target, isSigned=false}), Move32Bit)
                    else (BlockSimple(BoxValue{boxKind=BoxLargeWord, source=untaggedResReg, dest=target, saveRegs=[]}), MoveWord)
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=moveKind}), boxTagCode], RegisterArgument target, false)
            end

        |   codeToICodeLoad({kind=LoadStoreC64, address}, context, _, destination) =
            let
                (* Load a 64-bit value from C memory.  This is only allowed in 64-bit mode.  The result
                   is a boxed value. *)
                val _ = isX64 orelse raise InternalError "codeToICodeNonRev: BICLoadOperation LoadStoreC64 in 32-bit"
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w8, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveWord}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=untaggedResReg, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeLoad({kind=LoadStoreCFloat, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w4, context)
                val untaggedResReg = newUReg()
                val boxFloat = case fpMode of FPModeX87 => BoxX87 | FPModeSSE2 => BoxSSE2
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveFloat}),
                     BlockSimple(BoxValue{boxKind=boxFloat, source=untaggedResReg, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeLoad({kind=LoadStoreCDouble, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w8, context)
                val untaggedResReg = newUReg()
                val boxFloat = case fpMode of FPModeX87 => BoxX87 | FPModeSSE2 => BoxSSE2
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveDouble}),
                     BlockSimple(BoxValue{boxKind=boxFloat, source=untaggedResReg, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeLoad({kind=LoadStoreUntaggedUnsigned, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeAddress(address, false, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveWord}),
                     BlockSimple(TagValue {source=untaggedResReg, dest=target, isSigned=false})], RegisterArgument target, false)
            end


        and codeToICodeStore({kind=LoadStoreMLWord _, address, value}, context, _, destination) =
            let
                val (sourceCode, source, _) = codeToICode(value, context, false, Allowed allowInMemMove)
                val (codeBaseIndex, codeUntag, {base, offset, index, ...}) = codeAddress(address, false, context)
                val code =
                    codeBaseIndex @ sourceCode @ codeUntag @
                        [BlockSimple(StoreArgument {source=source, base=base, offset=offset, index=index, kind=MoveWord, isMutable=true})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeStore({kind=LoadStoreMLByte _, address, value}, context, _, destination) =
            let
                val (codeBaseIndex, codeUntag, {base, offset, index, ...}) = codeAddress(address, true, context)
                (* We have to untag the value to store. *)
                val (valueCode, untagValue, valueArg) = codeAsUntaggedValue(value, false, context)
                val code =
                    codeBaseIndex @ valueCode @ untagValue @ codeUntag @
                    [BlockSimple(StoreArgument {source=valueArg, base=base, offset=offset, index=index, kind=MoveByte, isMutable=true})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeStore({kind=LoadStoreC8, address, value}, context, _, destination) =
            let
                (* Store a byte to C memory.  Almost exactly the same as LoadStoreMLByte. *)
                val (codeBaseIndex, codeUntag, {base, offset, index, ...}) = codeCAddress(address, 0w1, context)
                val (valueCode, untagValue, valueArg) = codeAsUntaggedValue(value, false, context)
                val code =
                    codeBaseIndex @ valueCode @ untagValue @ codeUntag @
                    [BlockSimple(StoreArgument {source=valueArg, base=base, offset=offset, index=index, kind=MoveByte, isMutable=true})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeStore({kind=LoadStoreC16, address, value}, context, _, destination) =
            let
                (* Store a 16-bit value to C memory. *)
                val (codeBaseIndex, codeUntag, {base, offset, index, ...}) = codeCAddress(address, 0w2, context)
                (* We don't currently implement 16-bit constant moves so this must always be in a reg. *)
                val (valueCode, untagValue, valueArg) = codeAsUntaggedToReg(value, false, context)
                val code =
                    codeBaseIndex @ valueCode @ untagValue @ codeUntag @
                    [BlockSimple(StoreArgument {source=RegisterArgument valueArg, base=base, offset=offset, index=index, kind=Move16Bit, isMutable=true})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeStore({kind=LoadStoreC32, address, value}, context, _, destination) =
                (* Store a 32-bit value.  If this is 64-bit mode we untag it but if this is 32-bit mode we unbox it. *)
            let
                val (codeBaseIndex, codeUntag, {base, offset, index, ...}) = codeCAddress(address, 0w4, context)

                val code =
                    if isX64
                    then
                    let
                        (* We don't currently implement 32-bit constant moves so this must always be in a reg. *)
                        val (valueCode, untagValue, valueArg) = codeAsUntaggedToReg(value, false, context)
                    in
                        codeBaseIndex @ valueCode @ untagValue @ codeUntag @
                        [BlockSimple(StoreArgument {source=RegisterArgument valueArg, base=base, offset=offset, index=index, kind=Move32Bit, isMutable=true})]
                    end
                    else
                    let
                        val (valueCode, valueReg) = codeToPReg(value, context)
                        val valueReg1 = newUReg()
                    in
                        codeBaseIndex @ valueCode @ BlockSimple(LoadArgument{source=wordAt valueReg, dest=valueReg1, kind=MoveWord}) :: codeUntag @
                        [BlockSimple(StoreArgument {source=RegisterArgument valueReg1, base=base, offset=offset, index=index, kind=Move32Bit, isMutable=true})]
                    end
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeStore({kind=LoadStoreC64, address, value}, context, _, destination) =
            let
                (* Store a 64-bit value. *)
                val _ = isX64 orelse raise InternalError "codeToICodeNonRev: BICStoreOperation LoadStoreC64 in 32-bit"
                val (valueCode, valueReg) = codeToPReg(value, context)
                val valueReg1 = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index, ...}) = codeCAddress(address, 0w8, context)
                val code =
                    codeBaseIndex @ valueCode @ codeUntag @
                    [BlockSimple(LoadArgument{source=wordAt valueReg, dest=valueReg1, kind=MoveWord}),
                     BlockSimple(StoreArgument {source=RegisterArgument valueReg1, base=base, offset=offset, index=index, kind=MoveWord, isMutable=true})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeStore({kind=LoadStoreCFloat, address, value}, context, _, destination) =
            let
                val floatReg = newUReg() and float2Reg = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index, ...}) = codeCAddress(address, 0w4, context)
                val (valueCode, valueReg) = codeToPReg(value, context)
                (* If we're using an SSE2 reg we have to convert it from double to single precision. *)
                val (storeReg, cvtCode) =
                    case fpMode of
                        FPModeSSE2 =>
                            (float2Reg,
                                [BlockSimple(SSE2FPArith{opc=SSE2DoubleToFloat, resultReg=float2Reg, arg1=floatReg, arg2=RegisterArgument floatReg})])
                    |   FPModeX87 => (floatReg, [])
                val code =
                    codeBaseIndex @ valueCode @ codeUntag @
                    BlockSimple(LoadArgument{source=wordAt valueReg, dest=floatReg, kind=MoveDouble}) :: cvtCode @
                    [BlockSimple(StoreArgument {source=RegisterArgument storeReg, base=base, offset=offset, index=index, kind=MoveFloat, isMutable=true})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeStore({kind=LoadStoreCDouble, address, value}, context, _, destination) =
            let
                val floatReg = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index, ...}) = codeCAddress(address, 0w8, context)
                val (valueCode, valueReg) = codeToPReg(value, context)
                val code =
                    codeBaseIndex @ valueCode @ codeUntag @
                    [BlockSimple(LoadArgument{source=wordAt valueReg, dest=floatReg, kind=MoveDouble}),
                     BlockSimple(StoreArgument {source=RegisterArgument floatReg, base=base, offset=offset, index=index, kind=MoveDouble, isMutable=true})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeStore({kind=LoadStoreUntaggedUnsigned, address, value}, context, _, destination) =
            let
                (* We have to untag the value to store. *)
                val (codeBaseIndex, codeUntag, {base, offset, index, ...}) = codeAddress(address, false, context)
                (* See if it's a constant.  This is frequently used to set the last word of a string to zero. *)
                (* We have to be a bit more careful on the X86.  We use moves to store constants that
                   can include addresses.  To avoid problems we only use a move if the value is
                   zero or odd and so looks like a tagged value. *)
                val storeAble =
                    case value of
                        BICConstnt(value, _) =>
                            if not(isShort value)
                            then NONE
                            else
                            let
                                val ival = Word.toLargeIntX(toShort value)
                            in
                                if isX64
                                then if is32bit ival then SOME ival else NONE
                                else if ival = 0 orelse ival mod 2 = 1 then SOME ival else NONE
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
                                [BlockSimple(UntagValue{dest=valueReg1, source=valueReg, isSigned=false, cache=NONE })])
                        end
                val code =
                    codeBaseIndex @ valCode @ codeUntag @
                    [BlockSimple(StoreArgument {source=storeVal, base=base, offset=offset, index=index, kind=MoveWord, isMutable=true})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end


        and codeToICodeBlock({kind=BlockOpCompareByte, sourceLeft, destRight, length}, context, _, destination) =
            let
                (* TODO: If we have a short fixed length comparison we might well be better loading the value into
                   a register and comparing with memory. *)
                val target = asTarget destination
                val vec1Reg = newUReg() and vec2Reg = newUReg()
                val (leftCode, leftUntag, {base=leftBase, offset=leftOffset, index=leftIndex, ...}) =
                    codeAddress(sourceLeft, true, context)
                val (rightCode, rightUntag, {base=rightBase, offset=rightOffset, index=rightIndex, ...}) =
                    codeAddress(destRight, true, context)
                val ccRef = newCCRef()
                val labLess = newLabel() and labGreater = newLabel() and exitLab = newLabel()
                val labNotLess = newLabel() and labNotGreater = newLabel()
                
                val (lengthCode, lengthUntag, lengthArg) = codeAsUntaggedToReg(length, false (* unsigned *), context)

                val code =
                    leftCode @ rightCode @ lengthCode @
                    leftUntag @ [BlockSimple(loadAddress{base=leftBase, offset=leftOffset, index=leftIndex, dest=vec1Reg})] @
                    rightUntag @ [BlockSimple(loadAddress{base=rightBase, offset=rightOffset, index=rightIndex, dest=vec2Reg})] @
                    lengthUntag @
                    [BlockSimple(CompareByteVectors{ vec1Addr=vec1Reg, vec2Addr=vec2Reg, length=lengthArg, ccRef=ccRef }),
                     (* N.B. These are unsigned comparisons. *)
                     BlockFlow(Conditional{ ccRef=ccRef, condition=JB, trueJump=labLess, falseJump=labNotLess }),
                     BlockLabel labNotLess,
                     BlockFlow(Conditional{ ccRef=ccRef, condition=JA, trueJump=labGreater, falseJump=labNotGreater }),
                     BlockLabel labNotGreater,
                     BlockSimple(LoadArgument{ source=IntegerConstant(tag 0), dest=target, kind=MoveWord }),
                     BlockFlow(Unconditional exitLab),
                     BlockLabel labLess,
                     BlockSimple(LoadArgument{ source=IntegerConstant(tag ~1), dest=target, kind=MoveWord }),
                     BlockFlow(Unconditional exitLab),
                     BlockLabel labGreater,
                     BlockSimple(LoadArgument{ source=IntegerConstant(tag 1), dest=target, kind=MoveWord }),
                     BlockLabel exitLab]
            in
                (code, RegisterArgument target, false)
            end

        |   codeToICodeBlock({kind=BlockOpMove {isByteMove}, sourceLeft, destRight, length}, context, _, destination) =
            let
                (* TODO: If we have a short fixed length move we might well be better loading the value into
                   a register and storing it. *)
                val lengthReg = newPReg()
                val vec1Reg = newUReg() and vec2Reg = newUReg() and lenReg = newUReg()
                val (leftCode, leftUntag, {base=leftBase, offset=leftOffset, index=leftIndex, ...}) =
                    codeAddress(sourceLeft, isByteMove, context)
                val (rightCode, rightUntag, {base=rightBase, offset=rightOffset, index=rightIndex, ...}) =
                    codeAddress(destRight, isByteMove, context)
                val code =
                    leftCode @ rightCode @ codeToICodeTarget(length, context, false, lengthReg) @
                    leftUntag @ [BlockSimple(loadAddress{base=leftBase, offset=leftOffset, index=leftIndex, dest=vec1Reg})] @
                    rightUntag @ [BlockSimple(loadAddress{base=rightBase, offset=rightOffset, index=rightIndex, dest=vec2Reg})] @
                    [BlockSimple(UntagValue{source=lengthReg, dest=lenReg, isSigned=false, cache=NONE}),
                     BlockSimple(BlockMove{ srcAddr=vec1Reg, destAddr=vec2Reg, length=lenReg, isByteMove=isByteMove })]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeBlock({kind=BlockOpEqualByte, ...}, _, _, _) =
                raise InternalError "codeToICodeBlock - BlockOpEqualByte" (* Already done *)
          
        and codeConditionRev(BICConstnt(value, _), _, jumpOn, jumpLabel, tailCode) =
            (* Constant - typically part of andalso/orelse.  Either an unconditional branch
               or an unconditional drop-through. *)
            if jumpOn = (toShort value <> 0w0)
            then BlockFlow(Unconditional jumpLabel) :: tailCode
            else tailCode

        |   codeConditionRev(BICTagTest{test, tag=tagValue, ...}, context, jumpOn, jumpLabel, tailCode) =
            (* Check the "tag" word of a union (datatype).  N.B.  Not the same as testing the
               tag bit of a word. *)
            let
                val ccRef = newCCRef()
                val (testCode, tagReg) = codeToPRegRev(test, context, tailCode)
                val noJumpLabel = newLabel()
            in
                BlockLabel noJumpLabel ::
                BlockFlow(Conditional{ccRef=ccRef,
                           condition=if jumpOn then JE else JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                BlockSimple(WordComparison{arg1=tagReg, arg2=IntegerConstant(tag(Word.toLargeInt tagValue)), ccRef=ccRef}) ::
                    testCode
            end

        |   codeConditionRev(BICUnary{oper=BuiltIns.NotBoolean, arg1}, context, jumpOn, jumpLabel, tailCode) =
                (* If we have a "not" we can just invert the jump condition. *)
                codeConditionRev(arg1, context, not jumpOn, jumpLabel, tailCode)

        |   codeConditionRev(BICUnary{oper=BuiltIns.IsTaggedValue, arg1}, context, jumpOn, jumpLabel, tailCode) =
            let
                val ccRef = newCCRef()
                val (testCode, testReg) = codeToPRegRev(arg1, context, tailCode)
                (* Test the tag bit.  This sets the zero bit if the value is untagged. *)
                (* TODO: The X86 supports tests with a memory argument so we don't have to
                   load it into a register.  That's not currently supported by the rest of
                   the code-generator. *)
                val noJumpLabel = newLabel()
            in
                BlockLabel noJumpLabel ::
                BlockFlow(Conditional{ccRef=ccRef, condition=if jumpOn then JNE else JE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                    BlockSimple(TestTagBit{arg=RegisterArgument testReg, ccRef=ccRef}) :: testCode
            end

            (* Comparisons.  Because this is also used for pointer equality and even for exception matching it
               is perfectly possible that the argument could be an address. *)
        |   codeConditionRev(BICBinary{oper=BuiltIns.WordComparison{test, isSigned}, arg1, arg2=BICConstnt(arg2Value, _)},
                    context, jumpOn, jumpLabel, tailCode) =
            let
                val ccRef = newCCRef()
                val (testCode, testReg) = codeToPRegRev(arg1, context, tailCode)
                val arg2Arg = constantAsArgument arg2Value
                val noJumpLabel = newLabel()
            in
                BlockLabel noJumpLabel ::
                BlockFlow(Conditional{ccRef=ccRef,
                           condition=testAsBranch(test, isSigned, jumpOn), trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                BlockSimple(WordComparison{arg1=testReg, arg2=arg2Arg, ccRef=ccRef}) :: testCode
            end
            
        |   codeConditionRev(BICBinary{oper=BuiltIns.WordComparison{test, isSigned}, arg1=BICConstnt(arg1Value, _), arg2},
                    context, jumpOn, jumpLabel, tailCode) =
            let
                (* If we have the constant first we need to reverse the test so the first argument is a register. *)
                val ccRef = newCCRef()
                val (testCode, testReg) = codeToPRegRev(arg2, context, tailCode)
                val arg1Arg = constantAsArgument arg1Value
                val noJumpLabel = newLabel()
            in
                BlockLabel noJumpLabel ::
                BlockFlow(Conditional{ccRef=ccRef,
                           condition=testAsBranch(leftRightTest test, isSigned, jumpOn), trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                BlockSimple(WordComparison{arg1=testReg, arg2=arg1Arg, ccRef=ccRef}) :: testCode
            end

        |   codeConditionRev(BICBinary{oper=BuiltIns.WordComparison{test, isSigned}, arg1, arg2},
                    context, jumpOn, jumpLabel, tailCode) =
            let
                val ccRef = newCCRef()
                val (arg1Code, arg1Reg) = codeToPRegRev(arg1, context, tailCode)
                val (arg2Code, arg2Reg) = codeToPRegRev(arg2, context, arg1Code)
                val noJumpLabel = newLabel()
            in
                BlockLabel noJumpLabel ::
                BlockFlow(Conditional{ccRef=ccRef,
                           condition=testAsBranch(test, isSigned, jumpOn), trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                BlockSimple(WordComparison{arg1=arg1Reg, arg2=RegisterArgument arg2Reg, ccRef=ccRef}) :: arg2Code
            end

        |   codeConditionRev(BICBinary{oper=BuiltIns.LargeWordComparison test, arg1, arg2},
                    context, jumpOn, jumpLabel, tailCode) =
            let
                val ccRef = newCCRef()
                val (arg1Code, arg1Reg) = codeToPRegRev(arg1, context, tailCode)
                (* In X64 we can extract the word from a constant and do the comparison
                   directly.  That can't be done in X86/32 because the value isn't tagged
                   and might look like an address.  The RTS scans for comparisons with
                   inline constant addresses. *)
                val (arg2Code, arg2Operand) =
                    if isX64
                    then
                    (
                        case arg2 of
                            BICConstnt(value, _) => (arg1Code, IntegerConstant(largeWordConstant value))
                        |   _ =>
                            let
                                val (code, reg) = codeToPRegRev(arg2, context, arg1Code)
                            in
                                (code, wordAt reg)
                            end
                    )
                    else
                    let
                        val (code, reg) = codeToPRegRev(arg2, context, arg1Code)
                    in
                        (code, wordAt reg)
                    end
                val argReg = newUReg()
                val noJumpLabel = newLabel()
            in
                BlockLabel noJumpLabel ::
                BlockFlow(Conditional{ccRef=ccRef,
                           condition=testAsBranch(test, false, jumpOn), trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                BlockSimple(WordComparison{arg1=argReg, arg2=arg2Operand, ccRef=ccRef}) ::
                BlockSimple(LoadArgument{source=wordAt arg1Reg, dest=argReg, kind=MoveWord}) :: arg2Code
            end

           (* Floating point comparison.  This is complicated because we have different
              instruction sequences for SSE2 and X87.  We also have to get the handling
              of unordered (NaN) values right.  All the tests are treated as false
              if either argument is a NaN.  To combine that test with the other tests
              we sometimes have to reverse the comparison. *)

        |   codeConditionRev(BICBinary{oper=BuiltIns.RealComparison BuiltIns.TestEqual, arg1, arg2},
                    context, jumpOn, jumpLabel, tailCode) =
            let
                val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                val fpReg = newUReg()
                val testReg1 = newUReg() and testReg2 = newUReg() and testReg3 = newUReg()
                val (arg1Code, arg1Reg) = codeToPRegRev(arg1, context, tailCode)
                val (arg2Code, arg2Reg) = codeToPRegRev(arg2, context, arg1Code)
               (* If this is X87 we get the condition into RAX and test it there.  If
                   it is SSE2 we have to treat the unordered result (parity set) specially. *)
                val noJumpLabel = newLabel()
            in
                case (fpMode, jumpOn) of
                    (FPModeX87, _) =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef2, condition=if jumpOn then JE else JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockSimple(ArithmeticFunction{
                            oper=XOR, resultReg=testReg3, operand1=testReg2, operand2=IntegerConstant 0x4000, ccRef=ccRef2 }) ::
                        BlockSimple(ArithmeticFunction{
                            oper=AND, resultReg=testReg2, operand1=testReg1, operand2=IntegerConstant 0x4400, ccRef=newCCRef() }) ::
                        BlockSimple(X87FPGetCondition { ccRef=ccRef1, dest=testReg1 }) ::
                        BlockSimple(X87Compare{arg1=fpReg, arg2=wordAt arg2Reg, ccRef=ccRef1}) ::
                        BlockSimple(LoadArgument{source=wordAt arg1Reg, dest=fpReg, kind=MoveDouble}) :: arg2Code
                |   (FPModeSSE2, true) =>
                    let
                        val noParityLabel = newLabel()
                    in
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef1, condition=JE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockLabel noParityLabel ::
                        (* Go to noJumpLabel if unordered and therefore not equal. *)
                        BlockFlow(Conditional{ccRef=ccRef1, condition=JP, trueJump=noJumpLabel, falseJump=noParityLabel}) ::
                        BlockSimple(SSE2Compare{arg1=fpReg, arg2=wordAt arg2Reg, ccRef=ccRef1}) ::
                        BlockSimple(LoadArgument{source=wordAt arg1Reg, dest=fpReg, kind=MoveDouble}) :: arg2Code
                    end
               |    (FPModeSSE2, false) =>
                    let
                        val noParityLabel = newLabel()
                    in
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef1, condition=JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockLabel noParityLabel ::
                        (* Go to jumpLabel if unordered and therefore not equal. *)
                        BlockFlow(Conditional{ccRef=ccRef1, condition=JP, trueJump=jumpLabel, falseJump=noParityLabel}) ::
                        BlockSimple(SSE2Compare{arg1=fpReg, arg2=wordAt arg2Reg, ccRef=ccRef1}) ::
                        BlockSimple(LoadArgument{source=wordAt arg1Reg, dest=fpReg, kind=MoveDouble}) :: arg2Code
                    end
            end

        |   codeConditionRev(BICBinary{oper=BuiltIns.RealComparison BuiltIns.TestLess, arg1, arg2},
                    context, jumpOn, jumpLabel, tailCode) =
            let
                val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                val fpReg = newUReg()
                val testReg1 = newUReg() and testReg2 = newUReg()
                val (arg1Code, arg1Reg) = codeToPRegRev(arg1, context, tailCode)
                val (arg2Code, arg2Reg) = codeToPRegRev(arg2, context, arg1Code)
                val noJumpLabel = newLabel()
            in
                case fpMode of
                    FPModeX87 =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef2, condition=if jumpOn then JE else JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockSimple(ArithmeticFunction{
                            oper=AND, resultReg=testReg2, operand1=testReg1, operand2=IntegerConstant 0x4500, ccRef=ccRef2 }) ::
                        BlockSimple(X87FPGetCondition { ccRef=ccRef1, dest=testReg1 }) ::
                        BlockSimple(X87Compare{arg1=fpReg, arg2=wordAt arg1Reg, ccRef=ccRef1}) :: (* Reverse *)
                        BlockSimple(LoadArgument{source=wordAt arg2Reg, dest=fpReg, kind=MoveDouble}) :: arg2Code
                |   FPModeSSE2 =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef1, condition=if jumpOn then JA else JNA, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockSimple(SSE2Compare{arg1=fpReg, arg2=wordAt arg1Reg, ccRef=ccRef1}) :: (* Reverse *)
                        BlockSimple(LoadArgument{source=wordAt arg2Reg, dest=fpReg, kind=MoveDouble}) :: arg2Code
            end

        |   codeConditionRev(BICBinary{oper=BuiltIns.RealComparison BuiltIns.TestGreater, arg1, arg2},
                    context, jumpOn, jumpLabel, tailCode) =
            let
                val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                val fpReg = newUReg()
                val testReg1 = newUReg() and testReg2 = newUReg()
                val (arg1Code, arg1Reg) = codeToPRegRev(arg1, context, tailCode)
                val (arg2Code, arg2Reg) = codeToPRegRev(arg2, context, arg1Code)
                val noJumpLabel = newLabel()
            in
                case fpMode of
                    FPModeX87 =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef2, condition=if jumpOn then JE else JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockSimple(ArithmeticFunction{
                            oper=AND, resultReg=testReg2, operand1=testReg1, operand2=IntegerConstant 0x4500, ccRef=ccRef2 }) ::
                        BlockSimple(X87FPGetCondition { ccRef=ccRef1, dest=testReg1 }) ::
                        BlockSimple(X87Compare{arg1=fpReg, arg2=wordAt arg2Reg, ccRef=ccRef1}) :: (* Not reversed. *)
                        BlockSimple(LoadArgument{source=wordAt arg1Reg, dest=fpReg, kind=MoveDouble}) :: arg2Code
                |   FPModeSSE2 =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef1, condition=if jumpOn then JA else JNA, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockSimple(SSE2Compare{arg1=fpReg, arg2=wordAt arg2Reg, ccRef=ccRef1}) :: (* Not reversed. *)
                        BlockSimple(LoadArgument{source=wordAt arg1Reg, dest=fpReg, kind=MoveDouble}) :: arg2Code
            end

        |   codeConditionRev(BICBinary{oper=BuiltIns.RealComparison BuiltIns.TestLessEqual, arg1, arg2},
                    context, jumpOn, jumpLabel, tailCode) =
            let
                val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                val fpReg = newUReg()
                val testReg1 = newUReg() and testReg2 = newUReg()
                val (arg1Code, arg1Reg) = codeToPRegRev(arg1, context, tailCode)
                val (arg2Code, arg2Reg) = codeToPRegRev(arg2, context, arg1Code)
                val noJumpLabel = newLabel()
            in
                case fpMode of
                    FPModeX87 =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef2, condition=if jumpOn then JE else JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockSimple(ArithmeticFunction{
                            oper=AND, resultReg=testReg2, operand1=testReg1, operand2=IntegerConstant 0x500, ccRef=ccRef2 }) ::
                        BlockSimple(X87FPGetCondition { ccRef=ccRef1, dest=testReg1 }) ::
                        BlockSimple(X87Compare{arg1=fpReg, arg2=wordAt arg1Reg, ccRef=ccRef1}) :: (* Reverse *)
                        BlockSimple(LoadArgument{source=wordAt arg2Reg, dest=fpReg, kind=MoveDouble}) :: arg2Code
                |   FPModeSSE2 =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef1, condition=if jumpOn then JNB else JB, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockSimple(SSE2Compare{arg1=fpReg, arg2=wordAt arg1Reg, ccRef=ccRef1}) :: (* Reverse *)
                        BlockSimple(LoadArgument{source=wordAt arg2Reg, dest=fpReg, kind=MoveDouble}) :: arg2Code
            end

        |   codeConditionRev(BICBinary{oper=BuiltIns.RealComparison BuiltIns.TestGreaterEqual, arg1, arg2},
                    context, jumpOn, jumpLabel, tailCode) =
            let
                val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                val fpReg = newUReg()
                val testReg1 = newUReg() and testReg2 = newUReg()
                val (arg1Code, arg1Reg) = codeToPRegRev(arg1, context, tailCode)
                val (arg2Code, arg2Reg) = codeToPRegRev(arg2, context, arg1Code)
                val noJumpLabel = newLabel()
            in
                case fpMode of
                    FPModeX87 =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef2, condition=if jumpOn then JE else JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockSimple(ArithmeticFunction{
                            oper=AND, resultReg=testReg2, operand1=testReg1, operand2=IntegerConstant 0x500, ccRef=ccRef2 }) ::
                        BlockSimple(X87FPGetCondition { ccRef=ccRef1, dest=testReg1 }) ::
                        BlockSimple(X87Compare{arg1=fpReg, arg2=wordAt arg2Reg, ccRef=ccRef1}) :: (* Not reversed. *)
                        BlockSimple(LoadArgument{source=wordAt arg1Reg, dest=fpReg, kind=MoveDouble}) :: arg2Code
                |   FPModeSSE2 =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef1, condition=if jumpOn then JNB else JB, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockSimple(SSE2Compare{arg1=fpReg, arg2=wordAt arg2Reg, ccRef=ccRef1}) :: (* Not reversed. *)
                        BlockSimple(LoadArgument{source=wordAt arg1Reg, dest=fpReg, kind=MoveDouble}) :: arg2Code
            end

        |   codeConditionRev(
                BICBlockOperation{kind=BlockOpEqualByte, sourceLeft, destRight, length}, context, jumpOn, jumpLabel, tailCode) =
            let
                val vec1Reg = newUReg() and vec2Reg = newUReg()
                val ccRef = newCCRef()
                val (leftCode, leftUntag, {base=leftBase, offset=leftOffset, index=leftIndex, ...}) =
                    codeAddressRev(sourceLeft, true, context, tailCode)
                val (rightCode, rightUntag, {base=rightBase, offset=rightOffset, index=rightIndex, ...}) =
                    codeAddressRev(destRight, true, context, leftCode)
                val (lengthCode, lengthUntag, lengthArg) = codeAsUntaggedToRegRev(length, false (* unsigned *), context, rightCode)
                val noJumpLabel = newLabel()
            in
                BlockLabel noJumpLabel ::
                BlockFlow(Conditional{ccRef=ccRef, condition=if jumpOn then JE else JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                BlockSimple(CompareByteVectors{ vec1Addr=vec1Reg, vec2Addr=vec2Reg, length=lengthArg, ccRef=ccRef }) ::
                lengthUntag @ BlockSimple(loadAddress{base=rightBase, offset=rightOffset, index=rightIndex, dest=vec2Reg}) ::
                rightUntag @ BlockSimple(loadAddress{base=leftBase, offset=leftOffset, index=leftIndex, dest=vec1Reg}) ::
                leftUntag @ lengthCode
            end

        |   codeConditionRev(BICCond (testPart, thenPart, elsePart), context, jumpOn, jumpLabel, tailCode) =
            let
                val notTest = newLabel() and isThen = newLabel() and notThenElse = newLabel()
                (* Test the condition and jump to the else-part if this is false. *)
                val testTest = codeConditionRev(testPart, context, false, notTest, tailCode)
                (* Test the then-part and jump if the condition we want holds.
                   We don't go to the final label yet. *)
                val testThen = codeConditionRev(thenPart, context, jumpOn, isThen, testTest)
                (* Test the else-part and jump on the inverse of the condition.
                   The destination of this jump is going to be the drop-through
                   case. *)
                (* Branch round the else-part and put in a label for the start of the else *)
                val testElse = codeConditionRev(elsePart, context, not jumpOn, notThenElse,
                    BlockLabel notTest ::
                    BlockFlow(Unconditional notThenElse) :: testThen)
            in
             (* And now the labels for the condition where we don't want to branch and want to
                drop through. *)
                BlockLabel notThenElse ::
                    (* Add a label for the result of the then-part.  Because we branched
                       on the inverse of the test in the else-part we now have both the
                       conditions to take the branch.  Put in an unconditional branch
                       to the final label. *)
                BlockFlow(Unconditional jumpLabel) :: BlockLabel isThen :: testElse
            end

            (* General case.  Load the value into a register and compare it with 1 (true) *)
        |   codeConditionRev(condition, context, jumpOn, jumpLabel, tailCode) =
            let
                val ccRef = newCCRef()
                val (testCode, testReg) = codeToPRegRev(condition, context, tailCode)
                val noJumpLabel = newLabel()
            in
                BlockLabel noJumpLabel ::
                BlockFlow(Conditional{ccRef=ccRef,
                           condition=if jumpOn then JE else JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                BlockSimple(WordComparison{arg1=testReg, arg2=IntegerConstant(tag 1), ccRef=ccRef}) ::
                testCode
            end

        (* Some operations that deliver boolean results are better coded as
           if condition then true else false *)
        and codeAsConditionalRev(instr, context, isTail, target, tailCode) =
            codeToICodeRev(
                BICCond(instr, BICConstnt(toMachineWord 1, []), BICConstnt(toMachineWord 0, [])), context, isTail, target, tailCode)

        and codeAsConditional(instr, context, isTail, target) =
        let
            val (code, dest, haveExited) =
                codeToICodeRev(BICCond(instr, BICConstnt(toMachineWord 1, []), BICConstnt(toMachineWord 0, [])),
                    context, isTail, target, [])
        in
            (List.rev code, dest, haveExited)
        end

        (* The fixed precision functions are also used for arbitrary precision but instead of raising Overflow we
           need to jump to the code that handles the long format. *)
        and codeFixedPrecisionArith(BuiltIns.ArithAdd, arg1, BICConstnt(value, _), context, target, onOverflow) =
            let
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
                arg1Code @
                    [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg1, operand2=IntegerConstant constVal, ccRef=ccRef})] @
                    onOverflow ccRef
            end

        |   codeFixedPrecisionArith(BuiltIns.ArithAdd, BICConstnt(value, _), arg2, context, target, onOverflow) =
            let
                val ccRef = newCCRef()
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
            in
                arg2Code @
                    [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg2, operand2=IntegerConstant constVal, ccRef=ccRef})] @
                    onOverflow ccRef
            end

        |   codeFixedPrecisionArith(BuiltIns.ArithAdd, arg1, arg2, context, target, onOverflow) =
            let
                val aReg3 = newPReg() and ccRef = newCCRef()
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
            in
                arg1Code @ arg2Code @
                    (* Subtract the tag bit from the second argument, do the addition and check for overflow. *)
                    (* TODO: We should really do the detagging in the transform phase.  It can make a better choice of
                       the argument if one of the arguments is already untagged or if we have a constant argument. *)
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=aReg2, operand2=IntegerConstant 1, ccRef=newCCRef()}),
                     BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg1, operand2=RegisterArgument aReg3, ccRef=ccRef})] @
                    onOverflow ccRef
            end

            (* Subtraction.  We can handle the special case of the second argument being a constant but not the first. *)
        |   codeFixedPrecisionArith(BuiltIns.ArithSub, arg1, BICConstnt(value, _), context, target, onOverflow) =
            let
                val ccRef = newCCRef()
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
            in
                arg1Code @
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=target, operand1=aReg1, operand2=IntegerConstant constVal, ccRef=ccRef})] @
                    onOverflow ccRef
            end

        |   codeFixedPrecisionArith(BuiltIns.ArithSub, arg1, arg2, context, target, onOverflow) =
            let
                val aReg3 = newPReg()
                val ccRef = newCCRef()
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
            in
                arg1Code @ arg2Code @
                    (* Do the subtraction, test for overflow and afterwards add in the tag bit. *)
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=aReg1, operand2=RegisterArgument aReg2, ccRef=ccRef})] @
                    onOverflow ccRef @
                    [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg3, operand2=IntegerConstant 1, ccRef=newCCRef()})]
            end

        |   codeFixedPrecisionArith(BuiltIns.ArithMult, arg1, BICConstnt(value, _), context, target, onOverflow) =
            let
                val aReg = newPReg() and argUntagged = newUReg()
                and resUntagged = newUReg()
                val mulCC = newCCRef()
                (* Is it better to untag the constant or the register argument? *)
                val constVal = if isShort value then Word.toLargeIntX(toShort value) else 0
            in
                codeToICodeTarget(arg1, context, false, aReg) @
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=argUntagged, operand1=aReg, operand2=IntegerConstant 1, ccRef=newCCRef()}),
                     BlockSimple(Multiplication{resultReg=resUntagged, operand1=argUntagged, operand2=IntegerConstant constVal, ccRef=mulCC} )] @
                     onOverflow mulCC @
                     [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=resUntagged, operand2=IntegerConstant 1, ccRef=newCCRef()})]
            end

        |   codeFixedPrecisionArith(BuiltIns.ArithMult, BICConstnt(value, _), arg2, context, target, onOverflow) =
            let
                val aReg = newPReg() and argUntagged = newUReg()
                and resUntagged = newUReg()
                val mulCC = newCCRef()
                (* Is it better to untag the constant or the register argument? *)
                val constVal = if isShort value then Word.toLargeIntX(toShort value) else 0
            in
                codeToICodeTarget(arg2, context, false, aReg) @
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=argUntagged, operand1=aReg, operand2=IntegerConstant 1, ccRef=newCCRef()}),
                     BlockSimple(Multiplication{resultReg=resUntagged, operand1=argUntagged, operand2=IntegerConstant constVal, ccRef=mulCC} )] @
                     onOverflow mulCC @
                     [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=resUntagged, operand2=IntegerConstant 1, ccRef=newCCRef()})]
            end

        |   codeFixedPrecisionArith(BuiltIns.ArithMult, arg1, arg2, context, target, onOverflow) =
            let
                val aReg1 = newPReg() and aReg2 = newPReg() and arg1Untagged = newUReg()
                and arg2Untagged = newUReg() and resUntagged = newUReg()
                val mulCC = newCCRef()
                (* This is almost the same as the word operation except we use a signed shift and check for overflow. *)
            in
                codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift one argument and subtract the tag from the other.  It's possible this could be reordered
                       if we have a value that is already untagged. *)
                    [BlockSimple(UntagValue{source=aReg1, dest=arg1Untagged, isSigned=true (* Signed shift here. *), cache=NONE}),
                     BlockSimple(ArithmeticFunction{oper=SUB, resultReg=arg2Untagged, operand1=aReg2, operand2=IntegerConstant 1, ccRef=newCCRef()}),
                     BlockSimple(Multiplication{resultReg=resUntagged, operand1=arg1Untagged, operand2=RegisterArgument arg2Untagged, ccRef=mulCC} )] @
                     onOverflow mulCC @
                     [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=resUntagged, operand2=IntegerConstant 1, ccRef=newCCRef()})]
            end

        |   codeFixedPrecisionArith(BuiltIns.ArithQuot, arg1, arg2, context, target, _) =
            let
                val aReg1 = newPReg() and aReg2 = newPReg()
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift both of the arguments to remove the tags.  We don't test for zero here - that's done explicitly. *)
                    [BlockSimple(UntagValue{source=aReg1, dest=arg1Untagged, isSigned=true, cache=NONE}),
                     BlockSimple(UntagValue{source=aReg2, dest=arg2Untagged, isSigned=true, cache=NONE}),
                     BlockSimple(Division { isSigned = true, dividend=arg1Untagged, divisor=RegisterArgument arg2Untagged,
                                quotient=quotient, remainder=remainder }),
                     BlockSimple(TagValue { source=quotient, dest=target, isSigned=true })]
            end

        |   codeFixedPrecisionArith(BuiltIns.ArithRem, arg1, arg2, context, target, _) =
            let
                (* Identical to Quot except that the result is the remainder. *)
                val aReg1 = newPReg() and aReg2 = newPReg()
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift both of the arguments to remove the tags. *)
                    [BlockSimple(UntagValue{source=aReg1, dest=arg1Untagged, isSigned=true, cache=NONE}),
                     BlockSimple(UntagValue{source=aReg2, dest=arg2Untagged, isSigned=true, cache=NONE}),
                     BlockSimple(Division { isSigned = true, dividend=arg1Untagged, divisor=RegisterArgument arg2Untagged,
                                quotient=quotient, remainder=remainder }),
                     BlockSimple(TagValue { source=remainder, dest=target, isSigned=true })]
            end

        |   codeFixedPrecisionArith(_, _, _, _, _, _) =
                raise InternalError "codeToICode: FixedPrecisionArith - unimplemented operation"

        (* Code an address.  The index is optional. *)
        and codeAddressRev({base, index=SOME index, offset}, true (* byte move *), context, tailCode) =
            let
                (* Byte address with index.  The index needs to be untagged. *)
                val indexReg1 = newUReg()
                val (codeBase, baseReg) = codeToPRegRev(base, context, tailCode)
                val (codeIndex, indexReg) = codeToPRegRev(index, context, codeBase)
                val untagCode = [BlockSimple(UntagValue{dest=indexReg1, source=indexReg, isSigned=false, cache=NONE })]
                val memResult = {base=baseReg, offset=Word.toInt offset, index=MemIndex1 indexReg1, cache=NONE}
            in
                (codeIndex, untagCode, memResult)
            end

        |   codeAddressRev({base, index=SOME index, offset}, false (* word move *), context, tailCode) =
            let
                (* Word address with index.  We can avoid untagging the index by adjusting the
                   multiplier and offset *) 
                val (codeBase, baseReg) = codeToPRegRev(base, context, tailCode)
                val (codeIndex, indexReg) = codeToPRegRev(index, context, codeBase)
                val memResult =
                    if isX64
                    then {base=baseReg, offset=Word.toInt offset-4, index=MemIndex4 indexReg, cache=NONE}
                    else {base=baseReg, offset=Word.toInt offset-2, index=MemIndex2 indexReg, cache=NONE}
            in
                (codeIndex, [], memResult)
            end

        |   codeAddressRev({base, index=NONE, offset}, _, context, tailCode) =
            let
                val (codeBase, baseReg) = codeToPRegRev(base, context, tailCode)
                val memResult = {offset=Word.toInt offset, base=baseReg, index=NoMemIndex, cache=NONE}
            in
                (codeBase, [], memResult)
            end

        and codeAddress(addr, isByte, context) =
        let
            val (code, untag, res) = codeAddressRev(addr, isByte, context, [])
        in
            (List.rev code, untag, res)
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
                    [BlockSimple(LoadArgument{source=wordAt baseReg, dest=untaggedBaseReg, kind=MoveWord}),
                     BlockSimple(UntagValue{dest=indexReg1, source=indexReg, isSigned=false, cache=NONE })]
                val memResult = {base=untaggedBaseReg, offset=Word.toInt offset, index=MemIndex1 indexReg1, cache=NONE}
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
                val untagCode = [BlockSimple(LoadArgument{source=wordAt baseReg, dest=untaggedBaseReg, kind=MoveWord})]
                val memResult =
                    case size of
                        0w2 => {base=untaggedBaseReg, offset=Word.toInt offset-1, index=MemIndex1 indexReg, cache=NONE}
                    |   0w4 => {base=untaggedBaseReg, offset=Word.toInt offset-2, index=MemIndex2 indexReg, cache=NONE}
                    |   0w8 => {base=untaggedBaseReg, offset=Word.toInt offset-4, index=MemIndex4 indexReg, cache=NONE}
                    |   _ => raise InternalError "codeCAddress: unknown size"
            in
                (codeBase @ codeIndex, untagCode, memResult)
            end

        |   codeCAddress({base, index=NONE, offset}, _, context) =
            let
                val untaggedBaseReg = newUReg()
                val (codeBase, baseReg) = codeToPReg(base, context)
                val untagCode = [BlockSimple(LoadArgument{source=wordAt baseReg, dest=untaggedBaseReg, kind=MoveWord})]
                val memResult = {offset=Word.toInt offset, base=untaggedBaseReg, index=NoMemIndex, cache=NONE}
            in
                (codeBase, untagCode, memResult)
            end

        (* Return an untagged value.  If we have a constant just return it.  Otherwise
           return the code to evaluate the argument, the code to untag it and the
           reference to the untagged register. *)
        and codeAsUntaggedToRegRev(BICConstnt(value, _), isSigned, _, tailCode) =
            let
                (* Should always be short except for unreachable code. *)
                val untagReg = newUReg()
                val cval = if isShort value then toShort value else 0w0
                val cArg = IntegerConstant(if isSigned then Word.toLargeIntX cval else Word.toLargeInt cval) (* Don't tag *)
                val untag = [BlockSimple(LoadArgument{source=cArg, dest=untagReg, kind=MoveWord})]
            in
                (tailCode, untag, untagReg) (* Don't tag. *)
            end
        |   codeAsUntaggedToRegRev(arg, isSigned, context, tailCode) =
            let
                val untagReg = newUReg()
                val (code, srcReg) = codeToPRegRev(arg, context, tailCode)
                val untag = [BlockSimple(UntagValue{source=srcReg, dest=untagReg, isSigned=isSigned, cache=NONE})]
            in
                (code, untag, untagReg)
            end

        and codeAsUntaggedToReg(arg, isSigned, context) =
        let
            val (code, untag, untagReg) = codeAsUntaggedToRegRev(arg, isSigned, context, [])
        in
            (List.rev code, untag, untagReg)
        end

        (* Return the argument as an untagged value.  We separate evaluating the argument from
           untagging because we may have to evaluate other arguments and that could involve a
           function call and we can't save the value to the stack after we've untagged it.
           Currently this is only used for byte values but we may have to be careful if
           we use it for word values on the X86.  Moving an untagged value into a register
           might look like loading a constant address. *)
        and codeAsUntaggedValue(BICConstnt(value, _), isSigned, _) =
            let
                val cval = if isShort value then toShort value else 0w0
                val cArg = IntegerConstant(if isSigned then Word.toLargeIntX cval else Word.toLargeInt cval) (* Don't tag *)
            in
                ([], [], cArg)
            end
        |   codeAsUntaggedValue(arg, isSigned, context) =
            let
                val untagReg = newUReg()
                val (code, argReg) = codeToPReg(arg, context)
                val untag = [BlockSimple(UntagValue{source=argReg, dest=untagReg, isSigned=isSigned, cache=NONE})]
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
              BlockSimple(LoadArgument{source=RegisterArgument sizeReg, dest=sizeReg2, kind=MoveWord}),
              BlockSimple(AllocateMemoryVariable{size=sizeReg, dest=allocReg, saveRegs=[]})] @
              flagUntag @
              [BlockSimple(StoreArgument{ source=flagArg, base=allocReg, offset= ~1, index=NoMemIndex, kind=MoveByte, isMutable=false}),
              (* We need to copy the address here because InitialiseMem modifies all its arguments. *)
              BlockSimple(LoadArgument{source=RegisterArgument allocReg, dest=initAddrReg, kind=MoveWord}),
              BlockSimple(UntagValue{source=sizeReg2, dest=untagSizeReg, isSigned=false, cache=NONE}),
              BlockSimple(LoadArgument{source=initResult, dest=initReg, kind=MoveWord}),
              BlockSimple(InitialiseMem{size=untagSizeReg, init=initReg, addr=initAddrReg}),
              BlockSimple InitialisationComplete,
              BlockSimple(LoadArgument{source=RegisterArgument allocReg, dest=target, kind=MoveWord})], RegisterArgument target, false)
        end

        (*Turn the codetree structure into icode. *)
        val bodyContext = {loopArgs=NONE, stackPtr=0, currHandler=NONE}
        val (bodyCode, _, bodyExited) =
            codeToICodeRev(body, bodyContext, true, SpecificPReg resultTarget, [beginInstruction])
        val icode = if bodyExited then bodyCode else returnInstruction(bodyContext, resultTarget, bodyCode)
        
        (* Turn the icode list into basic blocks.  The input list is in reverse so as part of
           this we reverse the list. *)
        local
            val resArray = Array.array(!labelCounter, BasicBlock{ block=[], flow=ExitCode })
            
            fun createEntry (blockNo, block, flow) =
                Array.update(resArray, blockNo, BasicBlock{ block=block, flow=flow})
            
            fun splitCode([], _, _) = 
                (* End of code.  We should have had a BeginFunction. *)
                raise InternalError "splitCode - no begin"
            
            |   splitCode(BlockBegin regArgs :: _, sinceLabel, flow) =
                    (* Final instruction.  Create the initial block and exit. *)
                    createEntry(0,
                        BeginFunction{regArgs=regArgs, stackArgs=stackArguments @ [returnAddressEntry]}::sinceLabel, flow)
            
            |   splitCode(BlockSimple instr :: rest, sinceLabel, flow) =
                    splitCode(rest, instr :: sinceLabel, flow)

            |   splitCode(BlockLabel label :: rest, sinceLabel, flow) =
                    (* Label - finish this block and start another. *)
                (
                    createEntry(label, sinceLabel, flow);
                    (* Default to a jump to this label.  That is used if we have
                       assumed a drop-through. *)
                    splitCode(rest, [], Unconditional label)
                )
            
            |   splitCode(BlockExit instr :: rest, _, _) =
                    splitCode(rest, [instr], ExitCode)

            |   splitCode(BlockFlow flow :: rest, _, _) =
                    splitCode(rest, [], flow)
            
            |   splitCode(BlockRaiseAndHandle(instr, handler) :: rest, _, _) =
                    splitCode(rest, [instr], UnconditionalHandle handler)

            |   splitCode(BlockOptionalHandle{call, handler, label} :: rest, sinceLabel, flow) =
                let
                    (* A function call within a handler.  This could go to the handler but
                       if there is no exception will go to the next instruction.
                       Also includes JumpLoop since the stack check could result in an
                       Interrupt exception. *)
                in
                    createEntry(label, sinceLabel, flow);
                    splitCode(rest, [call], ConditionalHandle{handler=handler, continue=label})
                end

        in
            val () = splitCode(icode, [], ExitCode)
            val resultVector = Array.vector resArray
        end
      
        open ICODETRANSFORM
        
        val pregProperties = Vector.fromList(List.rev(! pregPropList))
    in
        codeICodeFunctionToX86{blocks = resultVector, functionName = name, pregProps = pregProperties,
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
