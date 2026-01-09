(*
    Copyright David C. J. Matthews 2016-23

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
    structure BackEndTree: BACKENDINTERMEDIATECODE
    structure X86ICode: X86ICODE
    structure Debug: DEBUG
    structure X86Foreign: FOREIGNCALL
    structure ICodeTransform: X86ICODETRANSFORM
    structure CodeArray: CODEARRAY

    sharing X86ICode.Sharing = ICodeTransform.Sharing = CodeArray.Sharing
): GENCODE =
struct
    open BackEndTree
    open Address
    open X86ICode
    open CodeArray
 
    exception InternalError = Misc.InternalError

    local
        val regs =
            case targetArch of
                Native32Bit => [eax, ebx]
            |   Native64Bit => [eax, ebx, r8, r9, r10]
            |   ObjectId32Bit => [eax, esi, r8, r9, r10]
        val fpResult = case targetArch of Native32Bit => FPReg fp0 | _ => XMMReg xmm0
        val fpArgRegs = case targetArch of Native32Bit => [] | _ => [xmm0, xmm1, xmm2]
    in
        val generalArgRegs = List.map GenReg regs
        val floatingPtArgRegs = List.map XMMReg fpArgRegs
        fun resultReg GeneralType = GenReg eax
        |   resultReg DoubleFloatType = fpResult
        |   resultReg SingleFloatType = fpResult
        |   resultReg (ContainerType _) = GenReg eax (* Doesn't need a result. *)
    end

    (* If a container is larger than this it is passed on the stack. *)
    val smallContainerSize = case targetArch of Native32Bit => 2 | _ => 4

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
    |   BlockBegin of { regArgs: (preg * reg) list, stackArgs: stackLocn list }
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
        and floatSignBit: machineWord = makeRealConst [0wx00, 0wx00, 0wx00, 0wx80, 0wx00, 0wx00, 0wx00, 0wx00]
        and floatAbsMask: machineWord = makeRealConst [0wxff, 0wxff, 0wxff, 0wx7f, 0wx00, 0wx00, 0wx00, 0wx00]
    end

    datatype commutative = Commutative | NonCommutative

    (* Check that a large-word constant looks right and get the value as a large int*)
    fun largeWordConstant value =
        if isShort value then raise InternalError "largeWordConstant: invalid"
        else
        let
            val addr = toAddress value
        in
            if length addr <> nativeWordSize div wordSize orelse flags addr <> F_bytes
            then raise InternalError "largeWordConstant: invalid"
            else ();
            LargeWord.toLargeInt(RunCall.unsafeCast addr)
        end

    fun codeFunctionToX86({body, localCount, name, argTypes, resultType=fnResultType, closure, ...}:bicLambdaForm, debugSwitches, resultClosure) =
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
        
        and newMergeReg() =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := RegPropMultiple :: !pregPropList
        in
            PReg regNo
        end
        
        datatype locationValue =
            NoLocation
        |   PregLocation of preg
        |   StackContainer of { container: stackLocn, stackOffset: int }
        |   RegisterContainer of preg list

        val locToPregArray = Array.array(localCount, NoLocation)
        val labelCounter = ref 1 (* Start at 1.  Zero is used for the root. *)
        fun newLabel() = !labelCounter before labelCounter := !labelCounter + 1
        val ccRefCounter = ref 0
        fun newCCRef() = CcRef(!ccRefCounter) before ccRefCounter := !ccRefCounter + 1

        (* The profile object is a single mutable with the F_bytes bit set. *)
        val profileObject = createProfileObject()

        (* Switch to indicate if we want to trace where live data has been allocated. *)
        (* TODO: This should be used in AllocateMemoryOperation and BoxValue and possibly AllocateMemoryVariable. *)
        val addAllocatingFunction =
            Debug.getParameter Debug.profileAllocationTag debugSwitches = 1

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
            |   testAsBranch(TestUnordered,     _,      _)          = raise InternalError "TestUnordered"
            
            (* Switch the direction of a test if we turn  c op x into x op c. *)
            fun leftRightTest TestEqual         = TestEqual
            |   leftRightTest TestLess          = TestGreater
            |   leftRightTest TestLessEqual     = TestGreaterEqual
            |   leftRightTest TestGreater       = TestLess
            |   leftRightTest TestGreaterEqual  = TestLessEqual
            |   leftRightTest TestUnordered     = TestUnordered
        end
        
        (* Overflow check.  This raises Overflow if the overflow bit is set in the cc.  This generates
           a single block for the function unless there is a handler.
           As well as reducing the size of the code this also means that overflow checks are generally
           JO instructions to the end of the code.  Since the default branch prediction is not to take
           forward jumps this should improve prefetching on the normal, non-overflow, path. *)
        fun checkOverflow ({currHandler=NONE, overflowBlock=ref(SOME overFlowLab), ...}) ccRef =
            (* It's already been set and there's no surrounding handler - use this. *)
            let
                val noOverflowLab = newLabel()
            in
                [
                    BlockFlow(Conditional{ ccRef=ccRef, condition=JO, trueJump=overFlowLab, falseJump=noOverflowLab }),
                    BlockLabel noOverflowLab
                ]
            end

        |   checkOverflow ({currHandler=NONE, overflowBlock, ...}) ccRef =
            let
                (* *)
                val overFlowLab = newLabel() and noOverflowLab = newLabel()
                val packetReg = newPReg()
                val () = overflowBlock := SOME overFlowLab
            in
                [
                    BlockFlow(Conditional{ ccRef=ccRef, condition=JO, trueJump=overFlowLab, falseJump=noOverflowLab }),
                    BlockLabel overFlowLab,
                    BlockSimple(LoadArgument{source=AddressConstant(toMachineWord(Overflow)), dest=packetReg, kind=movePolyWord}),
                    BlockExit(RaiseExceptionPacket{packetReg=packetReg}),
                    BlockLabel noOverflowLab
                ]
            end

        |   checkOverflow ({currHandler=SOME h, ...}) ccRef =
            let
                val overFlowLab = newLabel() and noOverflowLab = newLabel()
                val packetReg = newPReg()
            in
                [
                    BlockFlow(Conditional{ ccRef=ccRef, condition=JO, trueJump=overFlowLab, falseJump=noOverflowLab }),
                    BlockLabel overFlowLab,
                    BlockSimple(LoadArgument{source=AddressConstant(toMachineWord(Overflow)), dest=packetReg, kind=movePolyWord}),
                    BlockRaiseAndHandle(RaiseExceptionPacket{packetReg=packetReg}, h),
                    BlockLabel noOverflowLab
                ]
            end
        
        fun setAndRestoreRounding (rndMode, doWithRounding) =
        let
            open IEEEReal
            val savedRnd = newUReg() and setRnd = newUReg()
        in
            case fpMode of
                FPModeX87 => [BlockSimple(GetX87ControlReg{dest=savedRnd})] @
                    (* Set the appropriate bits in the control word. *)
                    (case rndMode of
                        TO_NEAREST => (* The bits need to be zero - just mask them. *)
                            [BlockSimple(
                                ArithmeticFunction{oper=AND, resultReg=setRnd, operand1=savedRnd,
                                    operand2=IntegerConstant 0xf3ff, ccRef=newCCRef(), opSize=OpSize32})]
                    |   TO_NEGINF =>
                        let
                            val wrk = newUReg()
                        in
                            (* Mask the bits and set to 01 *)
                            [BlockSimple(
                                ArithmeticFunction{oper=AND, resultReg=wrk, operand1=savedRnd,
                                    operand2=IntegerConstant 0xf3ff, ccRef=newCCRef(), opSize=OpSize32}),
                             BlockSimple(
                                ArithmeticFunction{oper=OR, resultReg=setRnd, operand1=savedRnd,
                                    operand2=IntegerConstant 0x400, ccRef=newCCRef(), opSize=OpSize32})]
                        end
                    |   TO_POSINF =>
                        let
                            val wrk = newUReg()
                        in
                            (* Mask the bits and set to 10 *)
                            [BlockSimple(
                                ArithmeticFunction{oper=AND, resultReg=wrk, operand1=savedRnd,
                                    operand2=IntegerConstant 0xf3ff, ccRef=newCCRef(), opSize=OpSize32}),
                             BlockSimple(
                                ArithmeticFunction{oper=OR, resultReg=setRnd, operand1=savedRnd,
                                    operand2=IntegerConstant 0x800, ccRef=newCCRef(), opSize=OpSize32})]
                        end
                    |   TO_ZERO => (* The bits need to be one - just set them. *)
                            [BlockSimple(
                                ArithmeticFunction{oper=OR, resultReg=setRnd, operand1=savedRnd,
                                    operand2=IntegerConstant 0xc00, ccRef=newCCRef(), opSize=OpSize32})]) @
                    [BlockSimple(SetX87ControlReg{source=setRnd})] @
                    doWithRounding() @
                    (* Restore the original rounding. *)
                    [BlockSimple(SetX87ControlReg{source=savedRnd})]

            |   FPModeSSE2 => [BlockSimple(GetSSE2ControlReg{dest=savedRnd})] @
                    (* Set the appropriate bits in the control word. *)
                    (case rndMode of
                        TO_NEAREST => (* The bits need to be zero - just mask them. *)
                            [BlockSimple(
                                ArithmeticFunction{oper=AND, resultReg=setRnd, operand1=savedRnd,
                                    operand2=IntegerConstant 0xffff9fff, ccRef=newCCRef(), opSize=OpSize32})]
                    |   TO_NEGINF =>
                        let
                            val wrk = newUReg()
                        in
                            (* Mask the bits and set to 01 *)
                            [BlockSimple(
                                ArithmeticFunction{oper=AND, resultReg=wrk, operand1=savedRnd,
                                    operand2=IntegerConstant 0xffff9fff, ccRef=newCCRef(), opSize=OpSize32}),
                             BlockSimple(
                                ArithmeticFunction{oper=OR, resultReg=setRnd, operand1=savedRnd,
                                    operand2=IntegerConstant 0x2000, ccRef=newCCRef(), opSize=OpSize32})]
                        end
                    |   TO_POSINF =>
                        let
                            val wrk = newUReg()
                        in
                            (* Mask the bits and set to 10 *)
                            [BlockSimple(
                                ArithmeticFunction{oper=AND, resultReg=wrk, operand1=savedRnd,
                                    operand2=IntegerConstant 0xffff9fff, ccRef=newCCRef(), opSize=OpSize32}),
                             BlockSimple(
                                ArithmeticFunction{oper=OR, resultReg=setRnd, operand1=savedRnd,
                                    operand2=IntegerConstant 0x4000, ccRef=newCCRef(), opSize=OpSize32})]
                        end
                    |   TO_ZERO => (* The bits need to be one - just set them. *)
                            [BlockSimple(
                                ArithmeticFunction{oper=OR, resultReg=setRnd, operand1=savedRnd,
                                    operand2=IntegerConstant 0x6000, ccRef=newCCRef(), opSize=OpSize32})]) @
                    [BlockSimple(SetSSE2ControlReg{source=setRnd})] @
                    doWithRounding() @
                    [BlockSimple(SetSSE2ControlReg{source=savedRnd})]
        end

        (* Put a floating point value into a box or tag it so the value can be held in
           a general register. *)
        fun boxOrTagReal(srcReg, destReg, precision) =
            if precision = BuiltIns.PrecDouble orelse wordSize <> 0w8
            then
            let
                open BuiltIns
                val boxFloat =
                    case (fpMode, precision) of
                        (FPModeX87, PrecDouble) => BoxX87Double
                    |   (FPModeX87, PrecSingle) => BoxX87Float
                    |   (FPModeSSE2, PrecDouble) => BoxSSE2Double
                    |   (FPModeSSE2, PrecSingle) => BoxSSE2Float
            in
                [BlockSimple(BoxValue{boxKind=boxFloat, source=srcReg, dest=destReg, saveRegs=[]})]
            end
            else [BlockSimple(TagFloat{source=srcReg, dest=destReg})]

        
        (* Indicate that the base address is actually an object index where appropriate. *)
        val memIndexOrObject = case targetArch of ObjectId32Bit => ObjectIndex | _ => NoMemIndex

        (* Generally we have an offset in words and no index register. *)
        fun wordOffsetAddress(offset, baseReg: preg): argument =
            MemoryLocation{offset=offset*Word.toInt wordSize, base=baseReg, index=memIndexOrObject, cache=NONE}
   
        (* The large-word operations all work on the value within the box pointed at
           by the register.  We generate all large-word operations using this even
           where the X86 instruction requires a register.  This allows the next level
           to optimise cases of cascaded instructions and avoid creating boxes for
           intermediate values. *)
        fun wordAt reg = wordOffsetAddress(0, reg)

        val returnAddressEntry = newStackLoc 1

        datatype argLoc =
            ArgInReg of { realReg: reg, argReg: preg }
        |   ArgOnStack of { stackOffset: int, stackReg: stackLocn }
        |   ArgIsRegContainer of preg list

        (* Pseudo-regs for the result, the closure and the args that were passed in real regs. *)
        val resultTarget = newPReg()
        val closureRegAddr = newPReg()

        (* Create a map for the arguments indicating their register or stack location. *)
        local
            val containerRegs =
                case List.filter(fn ContainerType _ => true | _ => false) argTypes of
                    [] => NONE
                |   [ContainerType s] =>
                        if s <= smallContainerSize
                        then SOME(List.tabulate(s, fn _ => newMergeReg()))
                        else SOME [] (* Larger containers return their result on the stack. *)
                |   _ => raise InternalError "more than one container arg"

            (* Select the appropriate argument register depending on the argument type. *)
            fun argTypesToArgEntries([], _, _, _) = ([], [], [], [])

            |   argTypesToArgEntries(DoubleFloatType :: tl, gRegs, fpReg :: fpRegs, n) =
                let
                    val (argTypes, argCode, argRegs, stackArgs) = argTypesToArgEntries(tl, gRegs, fpRegs, n-1)
                    val pRegArg = newPReg() and uRegArg = newUReg()
                in
                    (ArgInReg{realReg=fpReg, argReg=pRegArg} :: argTypes,
                     boxOrTagReal(uRegArg, pRegArg, BuiltIns.PrecDouble) @ argCode, (uRegArg, fpReg) :: argRegs, stackArgs)
                end

            |   argTypesToArgEntries(SingleFloatType :: tl, gRegs, fpReg :: fpRegs, n) =
                let
                    val (argTypes, argCode, argRegs, stackArgs) = argTypesToArgEntries(tl, gRegs, fpRegs, n-1)
                    val pRegArg = newPReg() and uRegArg = newUReg()
                in
                    (ArgInReg{realReg=fpReg, argReg=pRegArg} :: argTypes,
                     boxOrTagReal(uRegArg, pRegArg, BuiltIns.PrecSingle) @ argCode, (uRegArg, fpReg) :: argRegs, stackArgs)
                end

            |   argTypesToArgEntries(ContainerType s :: tl, gRegs, fpRegs, n) =
                if s <= smallContainerSize
                then
                let
                    val (argTypes, argCode, argRegs, stackArgs) = argTypesToArgEntries(tl, gRegs, fpRegs, n-1)
                    val regs = valOf containerRegs
                in
                    (ArgIsRegContainer regs :: argTypes, argCode, argRegs, stackArgs)
                end
                (* The address of a larger container is passed as an argument *)
                else argTypesToArgEntries(GeneralType :: tl, gRegs, fpRegs, n)

            |   argTypesToArgEntries(_ :: tl, gReg :: gRegs, fpRegs, n) =
                (* This deals with general arguments but also with extra floating point arguments.
                   They are boxed as usual. *)
                let
                    val (argTypes, argCode, argRegs, stackArgs) =
                        argTypesToArgEntries(tl, gRegs, fpRegs, n-1)
                    val argReg=newPReg()
                in
                    (ArgInReg{realReg=gReg, argReg=argReg} :: argTypes, argCode, (argReg, gReg) :: argRegs, stackArgs)
                end

            |   argTypesToArgEntries(_ :: tl, [], fpRegs, n) =
                let
                    val (argTypes, argCode, argRegs, stackArgs) = argTypesToArgEntries(tl, [], fpRegs, n-1)
                    val stackLoc = newStackLoc 1
                in
                    (ArgOnStack {stackOffset=n, stackReg = stackLoc } :: argTypes, argCode, argRegs, stackLoc :: stackArgs)
                end

            val (argEntries, argCode, argRegs, stackArguments) =
                argTypesToArgEntries(argTypes, generalArgRegs, floatingPtArgRegs, List.length argTypes)
            val clReg = case closure of [] => [] | _ => [(closureRegAddr, GenReg edx)]
        in
            val argumentVector = Vector.fromList argEntries

            (* Start code for the function. *)
            val beginInstructions = argCode @
                [BlockBegin{regArgs=clReg @ argRegs, stackArgs=stackArguments @ [returnAddressEntry]}]

            (* The number of arguments on the stack.  Needed in return instrs and tail calls. *)
            val currentStackArgs = List.length stackArguments
            
            val containerResults = Option.map(fn regs => ListPair.zip(regs, generalArgRegs)) containerRegs
        end
        
        (* The return instruction.  This can be added on to various tails but there is always
           one at the end anyway. *)
        fun returnInstruction({stackPtr, ...}, target, tailCode) =
        let
            val (results, returnCode) =
                case (containerResults, fnResultType) of
                    (NONE, GeneralType) => ([(target, resultReg GeneralType)], [])
                |   (NONE, DoubleFloatType) =>
                    let
                        val resReg = newUReg()
                    in
                        ([(resReg, resultReg DoubleFloatType)],
                                [BlockSimple(LoadArgument{source=wordAt target, dest=resReg, kind=MoveDouble})])
                    end
                |   (NONE, SingleFloatType) =>
                    let
                        val resReg = newUReg()
                        val unpack =
                            if wordSize = 0w8
                            then BlockSimple(UntagFloat{source=RegisterArgument target, dest=resReg, cache=NONE})
                            else BlockSimple(LoadArgument{source=wordAt target, dest=resReg, kind=MoveFloat})
                    in
                        ([(resReg, resultReg SingleFloatType)], [unpack])
                    end
                |   (SOME cResult, GeneralType) => (cResult, [])
                |   _ => raise InternalError "returnInstruction: result type mismatch"
        in
            BlockExit(ReturnResultFromFunction{results=results(*[(resReg, resultReg fnResultType)]*), numStackArgs=currentStackArgs}) ::
                returnCode @
                (if stackPtr <> 0
                then BlockSimple(ResetStackPtr{numWords=stackPtr, preserveCC=false}) :: tailCode
                else tailCode)
        end

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
        type context =
            { loopArgs: (preg list * int * int) option, stackPtr: int, currHandler: int option,
              overflowBlock: int option ref }

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
                if targetArch = Native32Bit
                then (code, arg, false) (* We can store the address directly *)
                else moveToTarget(dest, code, arg)

        |   moveIfNotAllowed(Allowed{existingPreg=true, ...}, code, arg as RegisterArgument(PReg _)) = (code, arg, false)

        |   moveIfNotAllowed(Allowed{memAddr=true, ...}, code, arg as MemoryLocation _) = (code, arg, false)

        |   moveIfNotAllowed(dest, code, arg) = moveToTarget(dest, code, arg)

        and moveToTarget(dest, code, arg) =
            let
                val target = asTarget dest
                val moveSize =
                    case arg of
                        AddressConstant _ => movePolyWord
                    |   MemoryLocation _ => movePolyWord
                    |   _ => moveNativeWord
            in
                (code @ [BlockSimple(LoadArgument{source=arg, dest=target, kind=moveSize})], RegisterArgument target, false)
            end

        (* Create a bool result from a test by returning true or false. *)
        fun makeBoolResultRev(condition, ccRef, target, testCode) =
        let
            val trueLab = newLabel() and falseLab = newLabel() and mergeLab = newLabel()
            val mergeReg = newMergeReg()
        in
            BlockSimple(LoadArgument{dest=target, source=RegisterArgument mergeReg, kind=Move32Bit}) ::
            BlockLabel mergeLab ::
            BlockFlow(Unconditional mergeLab) ::
            BlockSimple(LoadArgument{dest=mergeReg, source=IntegerConstant(tag 0), kind=Move32Bit}) ::
            BlockLabel falseLab ::
            BlockFlow(Unconditional mergeLab) ::
            BlockSimple(LoadArgument{dest=mergeReg, source=IntegerConstant(tag 1), kind=Move32Bit}) ::
            BlockLabel trueLab ::
            BlockFlow(Conditional{ ccRef=ccRef, condition=condition, trueJump=trueLab, falseJump=falseLab }) ::
            testCode
        end

        (* See if we have a container and return the entry if present. *)
        datatype containerType =
            NoContainer | ContainerOnStack of { container: stackLocn, stackOffset: int } | ContainerInRegs of preg list

        fun getContainerIfPresent(BICExtract(BICLoadLocal l)) =
            (
                case Array.sub(locToPregArray, l) of
                    StackContainer container => ContainerOnStack container
                |   RegisterContainer rc => ContainerInRegs rc
                |   _ => NoContainer
            )
        |   getContainerIfPresent(BICExtract(BICLoadArgument a)) =
            (
                case Vector.sub(argumentVector, a) of
                    ArgIsRegContainer rc => ContainerInRegs rc
                |   _ => NoContainer
            )
            
        |   getContainerIfPresent _ = NoContainer


        fun moveIfNotAllowedRev(NoResult, code, arg) = (code, arg, false)

        |   moveIfNotAllowedRev(Allowed{anyConstant=true, ...}, code, arg as AddressConstant _) = (code, arg, false)
        
        |   moveIfNotAllowedRev(Allowed{anyConstant=true, ...}, code, arg as IntegerConstant _) = (code, arg, false)
        
        |   moveIfNotAllowedRev(dest as Allowed{const32s=true, ...}, code, arg as IntegerConstant value) =
            (* This is allowed if the value is within 32-bits *)
                if is32bit value
                then (code, arg, false)
                else moveToTargetRev(dest, code, arg)

        |   moveIfNotAllowedRev(dest as Allowed{const32s=true, ...}, code, arg as AddressConstant _) =
                if targetArch = Native32Bit
                then (code, arg, false)
                else moveToTargetRev(dest, code, arg)

        |   moveIfNotAllowedRev(Allowed{existingPreg=true, ...}, code, arg as RegisterArgument(PReg _)) = (code, arg, false)

        |   moveIfNotAllowedRev(Allowed{memAddr=true, ...}, code, arg as MemoryLocation _) = (code, arg, false)

        |   moveIfNotAllowedRev(dest, code, arg) = moveToTargetRev(dest, code, arg)

        and moveToTargetRev(dest, code, arg) =
            let
                val target = asTarget dest
                val moveSize =
                    case arg of
                        AddressConstant _ => movePolyWord
                    |   MemoryLocation _ => movePolyWord
                    |   _ => moveNativeWord
            in
                (BlockSimple(LoadArgument{source=arg, dest=target, kind=moveSize}) :: code, RegisterArgument target, false)
            end

        (* Allocate a fixed size cell with a reference to the profile object if we want
           to trace the location of live data.  Currently only used for tuples and for closures
           in native 32/64 bit. *)
        and allocateWithProfileRev(n, flags, memAddr, tlCode) =
            if addAllocatingFunction
            then
            let
                val restAndAlloc =
                    BlockSimple(AllocateMemoryOperation{size=n+1, flags=Word8.orb(flags, Address.F_profile), dest=memAddr, saveRegs=[]}) ::
                        tlCode
                val (code2, source, _) =
                    moveIfNotAllowedRev(Allowed allowInMemMove, restAndAlloc, AddressConstant profileObject)
                val storeValue =
                    BlockSimple(StoreArgument{ source=source, offset=n*Word.toInt wordSize, base=memAddr,
                        index=memIndexOrObject, kind=movePolyWord, isMutable=false})
            in
                storeValue :: code2
            end
            else BlockSimple(AllocateMemoryOperation{size=n, flags=flags, dest=memAddr, saveRegs=[]}) :: tlCode

        (* Use a move if there's no offset or index.  We could use an add if there's no index. *)
        and loadAddress{base, offset=0, index=NoMemIndex, dest} =
                LoadArgument{source=RegisterArgument base, dest=dest, kind=movePolyWord}
        |   loadAddress{base, offset, index, dest} =
                LoadEffectiveAddress{base=SOME base, offset=offset, dest=dest, index=index, opSize=nativeWordOpSize}

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
 
                |   doBindings(BICDeclar{value=BICExtract(BICLoadLocal l), addr, ...} :: decs, context, tailCode) =
                    let
                        (* Giving a new name to an existing entry.  This should have been removed
                           at a higher level but it doesn't always seem to be.  In particular we
                           must treat this specially if it's a container. *)
                        val original = Array.sub(locToPregArray, l)
                        val () = Array.update(locToPregArray, addr, original)
                    in
                        doBindings(decs, context, tailCode)
                    end

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
                            val sizeClosure = List.length closure + (if targetArch = ObjectId32Bit then 2 else 1)
                            open Address

                            fun clear n =
                                if n = sizeClosure
                                then [BlockSimple(AllocateMemoryOperation{size=sizeClosure,
                                        flags=if targetArch = ObjectId32Bit then Word8.orb(F_mutable, F_closure) else F_mutable, dest=dest, saveRegs=[]})]
                                else
                                    (clear (n+1) @
                                        [BlockSimple(
                                            StoreArgument{source=IntegerConstant(tag 0), base=dest, offset=n*Word.toInt wordSize, index=memIndexOrObject,
                                                          kind=movePolyWord, isMutable=false})])
                        in
                            c @ clear 0 @ [BlockSimple InitialisationComplete]
                        end
                    
                        val allocClosures = ListPair.foldlEq makeClosure [] (recDecs, destRegs)
                    
                        fun setClosure({lambda as {closure, ...}, ...}, dest, l) =
                        let
                            val clResult = makeConstantClosure()
                            val () = codeFunctionToX86(lambda, debugSwitches, clResult)
                            (* Basically the same as tuple except we load the address of the closure we've made. *)
                            fun loadFields([], _) = []
                            |   loadFields(f :: rest, n) =
                                let
                                    val (code, source, _) = codeToICode(BICExtract f, context, false, Allowed allowInMemMove)
                                    val storeValue =
                                        [BlockSimple(StoreArgument{ source=source, base=dest, offset=n*Word.toInt wordSize, index=memIndexOrObject, kind=movePolyWord, isMutable=false })]
                                in
                                    code @ storeValue @ loadFields(rest, n+1)
                                end
                            val setCodeAddress =
                                if targetArch = ObjectId32Bit
                                then
                                let (* We can't get the code address until run time. *)
                                    val codeReg = newUReg()
                                    val closureReg = newPReg()
                                in
                                    map BlockSimple
                                    [
                                        LoadArgument{ source=AddressConstant(toMachineWord clResult), dest=closureReg, kind=movePolyWord},
                                        LoadArgument{ source=MemoryLocation{offset=0, base=closureReg, index=ObjectIndex, cache=NONE},
                                            dest=codeReg, kind=Move64Bit},
                                        StoreArgument{ source=RegisterArgument codeReg, offset=0, base=dest, index=ObjectIndex,
                                            kind=moveNativeWord, isMutable=false}
                                    ]
                                end
                                else
                                let
                                    val codeAddr = codeAddressFromClosure clResult
                                    val (code, source, _) =
                                        moveIfNotAllowed(Allowed allowInMemMove, [], AddressConstant codeAddr)
                                in
                                    code @
                                        [BlockSimple(
                                            StoreArgument{ source=source, base=dest, offset=0, index=NoMemIndex, kind=movePolyWord, isMutable=false })]
                                end
                            val setFields =
                                setCodeAddress @ loadFields(closure, if targetArch = ObjectId32Bit then 2 else 1)
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
       
                |   doBindings(BICDecContainer{ addr, size } :: decs, {loopArgs, stackPtr, currHandler, overflowBlock}, tailCode) =
                    if size <= smallContainerSize
                    then
                    let
                        val regs = List.tabulate(size, fn _ => newMergeReg())
                        val () = Array.update(locToPregArray, addr, RegisterContainer regs)
                    in
                        doBindings(decs, context, tailCode)
                    end
                    else
                    let (* Larger container - reserve a portion of stack and zero it. *)
                         val containerReg = newStackLoc size
                        val () = Array.update(locToPregArray, addr,
                                    StackContainer{container=containerReg, stackOffset=stackPtr+size})
                    in
                        doBindings(decs,
                            {loopArgs=loopArgs, stackPtr=stackPtr+size, currHandler=currHandler, overflowBlock=overflowBlock},
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
                            BlockSimple(LoadArgument{source=result, dest=resultReg, kind=movePolyWord}) ::
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
                |   PregLocation preg => moveIfNotAllowedRev(destination, tailCode, RegisterArgument preg)
                |   StackContainer{container, stackOffset} =>
                        (* This always returns a ContainerAddr whatever the "allowed". *)
                        (tailCode, ContainerAddr{container=container, stackOffset=stackPtr-stackOffset}, false)
                |   RegisterContainer _ => raise InternalError "BICExtract local: reg container"
            )

        |   codeToICodeRev(BICExtract(BICLoadArgument a), {stackPtr, ...}, _, destination, tailCode) =
            (
                case Vector.sub(argumentVector, a) of
                    ArgInReg{argReg, ...} => (* It was originally in a register.  It's now in a preg. *)
                        moveIfNotAllowedRev(destination, tailCode, RegisterArgument argReg)
                |   ArgOnStack{stackOffset, stackReg} => (* Pushed before call. *)
                    let
                        val target = asTarget destination
                    in
                        (BlockSimple(LoadArgument{
                            source=StackLocation{wordOffset=stackOffset+stackPtr, container=stackReg, field=0, cache=NONE},
                            dest=target, kind=moveNativeWord}) :: tailCode,
                         RegisterArgument target, false)
                    end
                |   ArgIsRegContainer _ => raise InternalError "BICExtract argument: reg container"
            )
        
        |   codeToICodeRev(BICExtract(BICLoadClosure c), _, _, destination, tailCode) =
            let
                (* Add the number of words for the code address.  This is 1 in native but 2 in 32-in-64. *)
                val offset = case targetArch of ObjectId32Bit => c+2 | _ => c+1
            in
                if c >= List.length closure then raise InternalError "BICExtract: closure" else ();
                (* N.B.  We need to add one to the closure entry because zero is the code address. *)
                moveIfNotAllowedRev(destination, tailCode, wordOffsetAddress(offset, closureRegAddr))
            end

        |   codeToICodeRev(BICExtract BICLoadRecursive, _, _, destination, tailCode) =
                (* If the closure is empty we must use the constant.  We can't guarantee that
                   the caller will actually load the closure register if it knows the closure
                   is empty. *)
                moveIfNotAllowedRev(destination, tailCode,
                    case closure of
                        [] => AddressConstant(closureAsAddress resultClosure)
                    |   _ => RegisterArgument closureRegAddr)

        |   codeToICodeRev(BICField{base, offset}, context, _, destination, tailCode) =
            let
                val (codeBase, baseEntry, _) = codeToICodeRev(base, context, false, Allowed allowInPReg, tailCode)
            in
                (* This should not be used with a container. *)
                case baseEntry of
                    RegisterArgument baseR =>
                        moveIfNotAllowedRev(destination, codeBase, wordOffsetAddress(offset, baseR))
                |   _ =>   raise InternalError "codeToICodeRev-BICField"                      
            end

        |   codeToICodeRev(BICLoadContainer{base, offset}, context, _, destination, tailCode) =
            (
                case getContainerIfPresent base of
                    ContainerOnStack {container, stackOffset} =>
                    let
                        val target = asTarget destination
                        val finalOffset = stackOffset+offset
                        val _ = finalOffset >= 0 orelse raise InternalError "offset"
                    in
                        (BlockSimple(LoadArgument{
                            source=StackLocation{wordOffset=finalOffset, container=container, field=offset, cache=NONE},
                            dest=target, kind=moveNativeWord}) :: tailCode,
                        RegisterArgument target, false)
                    end

                |   NoContainer =>
                    let
                        val (codeBase, baseR) = codeToPRegRev(base, context, tailCode)
                        val multiplier = Word.toInt(nativeWordSize div wordSize)
                    in
                        moveIfNotAllowedRev(destination, codeBase, wordOffsetAddress(offset*multiplier, baseR))
                    end

                |   ContainerInRegs regs =>
                    let
                        (* Always copy this into a new register because the source will be a merge reg. *)
                        val target = asTarget destination
                    in
                        (BlockSimple(LoadArgument{source=RegisterArgument(List.nth(regs, offset)), dest=target, kind=movePolyWord}) :: tailCode,
                                RegisterArgument target, false)
                    end
            )

        |   codeToICodeRev(BICEval{function, argList, resultType, ...}, context as { currHandler, ...}, isTail, destination, tailCode) =
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
                            (* If this is a closure we're still compiling we can't get the code address.
                               However if this is directly recursive we can use the recursive
                               convention. *)
                        in
                            if wordEq(closureAsAddress resultClosure, addr)
                            then (tailCode, [], Recursive)
                            else if flags addrAsAddr <> Address.F_words andalso flags addrAsAddr <> Address.F_closure
                            then (BlockSimple(LoadArgument{source=AddressConstant addr, dest=clPReg, kind=movePolyWord}) :: tailCode,
                                      [(RegisterArgument clPReg, GenReg edx)], FullCall)
                            else if targetArch = ObjectId32Bit
                            then (* We can't actually load the code address here. *)
                            let
                                val addrLength = length addrAsAddr
                                val _ = addrLength >= 0w1 orelse raise InternalError "BICEval address"
                                val _ = flags addrAsAddr = Address.F_closure orelse raise InternalError "BICEval address not a closure"
                            in
                                if addrLength = 0w2
                                then (tailCode, [], ConstantCode addr)
                                else (BlockSimple(LoadArgument{source=AddressConstant addr, dest=clPReg, kind=movePolyWord}) :: tailCode,
                                      [(RegisterArgument clPReg, GenReg edx)], ConstantCode addr)
                            end
                            else (* Native 32 or 64-bits. *)
                            let
                                val addrLength = length addrAsAddr
                                val _ = addrLength >= 0w1 orelse raise InternalError "BICEval address"
                                val codeAddr = loadWord(addrAsAddr, 0w0)
                                val _ = isCode (toAddress codeAddr) orelse raise InternalError "BICEval address not code"
                            in
                                if addrLength = 0w1
                                then (tailCode, [], ConstantCode codeAddr)
                                else (BlockSimple(LoadArgument{source=AddressConstant addr, dest=clPReg, kind=movePolyWord}) :: tailCode,
                                      [(RegisterArgument clPReg, GenReg edx)], ConstantCode codeAddr)
                            end
                        end

                    |   BICExtract BICLoadRecursive =>
                        (
                            (* If the closure is empty we don't need to load rdx *)
                            case closure of
                                [] => (tailCode, [], Recursive)
                            |   _ =>
                                    (BlockSimple(LoadArgument {source=RegisterArgument closureRegAddr, dest=clPReg, kind=movePolyWord}) :: tailCode,
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
                   because we may use a memory move to set them.  We also don't allow them in
                   32-in-64 because we can't push an address constant. *)
                val allowInStackArg =
                    Allowed {anyConstant=not isTail andalso targetArch <> ObjectId32Bit,
                               const32s=true, memAddr=false, existingPreg=not isTail }
                and allowInRegArg =
                    Allowed {anyConstant=true, const32s=true, memAddr=false, existingPreg=not isTail }

                (* Load the first arguments into registers and the rest to the stack. *)
                fun loadArgs ([], _, _, tailCode) = (tailCode, [], [])

                |   loadArgs ((arg, DoubleFloatType) :: args, gRegs, fpReg :: fpRegs, tailCode) =
                    let (* Floating point register argument. *)
                        val (c, r) = codeToPRegRev(arg, context, tailCode)
                        val r1 = newUReg()
                        val c1 =
                            BlockSimple(LoadArgument{source=wordAt r, dest=r1, kind=MoveDouble}) :: c
                        val (code, regArgs, stackArgs) = loadArgs(args, gRegs, fpRegs, c1)
                    in
                        (code, (RegisterArgument r1, fpReg) :: regArgs, stackArgs)
                    end

                |   loadArgs ((arg, SingleFloatType) :: args, gRegs, fpReg :: fpRegs, tailCode) =
                    let (* Floating point register argument. *)
                        val (c, r) = codeToPRegRev(arg, context, tailCode)
                        val r1 = newUReg()
                        val c1 =
                            if wordSize = 0w8
                            then BlockSimple(UntagFloat{source=RegisterArgument r, dest=r1, cache=NONE}) :: c
                            else BlockSimple(LoadArgument{source=wordAt r, dest=r1, kind=MoveFloat}) :: c
                        val (code, regArgs, stackArgs) = loadArgs(args, gRegs, fpRegs, c1)
                    in
                        (code, (RegisterArgument r1, fpReg) :: regArgs, stackArgs)
                    end

                |   loadArgs ((arg, _) :: args, gReg::gRegs, fpRegs, tailCode) =
                    let (* General register argument. *)
                        val (c, r, _) = codeToICodeRev(arg, context, false, allowInRegArg, tailCode)
                        val (code, regArgs, stackArgs) = loadArgs(args, gRegs, fpRegs, c)
                    in
                        (code, (r, gReg) :: regArgs, stackArgs)
                    end

                |   loadArgs ((arg, _) :: args, [], fpRegs, tailCode) =
                    let (* Stack argument. *)
                        val (c, r, _) = codeToICodeRev(arg, context, false, allowInStackArg, tailCode)
                        val (code, regArgs, stackArgs) = loadArgs(args, [], fpRegs, c)
                    in
                        (code, regArgs, r :: stackArgs)
                    end

                fun isSmallContainer(ContainerType s) = s <= smallContainerSize | isSmallContainer _ = false

                val (codeArgs, regArgs, stackArgs) =
                    loadArgs(List.filter(not o isSmallContainer o #2) argList, generalArgRegs, floatingPtArgRegs, functionCode)
                
                (* If this is at the end of the function and the result types are the
                   same we can use a tail-recursive call. *)
                val tailCall = isTail andalso resultType = fnResultType
                
                val callCode =
                    if tailCall
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
                                                (BlockSimple(LoadArgument{source=src, dest=preg, kind=moveNativeWord}) :: loadCode,
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
                                    loadStackArgs @ codeArgs
                    end
                    else
                    let
                        (* See if there is a container argument. *)
                        val containerArg = List.find(fn (_, ContainerType _) => true | _ => false) argList

                        val containerValue =
                            case containerArg of
                                SOME(argVal, _) => getContainerIfPresent argVal
                            |   NONE => NoContainer
                        (* When a container is passed as an argument we put the address into a register.
                           Normally the container will be referenced after the call in order to extract
                           the values but if it's discarded we need to make sure it will continue to
                           be referenced at least as far as the call.  This isn't a problem for the X86
                           code-generator since container addresses are a form of the "argument"
                           datatype. *)
                        (* There's a further problem with register containers that seems to occur only with
                           the X86, perhaps because of the way registers are allocated from the end back.
                           We need to copy the container arguments to temporary normal registers and
                           later copy the results into the merge registers.  It seems to be something to
                           do with having a handler. *)
                        val (results, moveResult) =
                            case (containerValue, resultType) of
                                (ContainerInRegs regs, GeneralType) =>
                                let
                                    val newRegs = List.map (fn _ => newPReg()) regs
                                    val copyRegs =
                                        ListPair.mapEq(fn (sreg, dreg) =>
                                                BlockSimple(LoadArgument{ source=RegisterArgument sreg, dest=dreg, kind=movePolyWord })) (newRegs, regs)
                                in
                                    (ListPair.zip(newRegs, generalArgRegs),
                                        BlockSimple(LoadArgument{ source=IntegerConstant(tag 0), dest=target, kind=Move32Bit }) :: copyRegs)
                                end
                            |   (ContainerOnStack _, GeneralType) =>
                                    ([], [BlockSimple(LoadArgument{ source=IntegerConstant(tag 0), dest=target, kind=Move32Bit })])
                            |   (NoContainer, GeneralType) => ([(target, resultReg GeneralType)], [])
                            |   (NoContainer, DoubleFloatType) =>
                                let
                                    val fpRegDest = newUReg()
                                in
                                    ([(fpRegDest, resultReg DoubleFloatType)], boxOrTagReal(fpRegDest, target, BuiltIns.PrecDouble))
                                end
                            |   (NoContainer, SingleFloatType) =>
                                let
                                    val fpRegDest = newUReg()
                                in
                                    ([(fpRegDest, resultReg SingleFloatType)], boxOrTagReal(fpRegDest, target, BuiltIns.PrecSingle))
                                end
                            |   _ => raise InternalError "codeToICodeRev: BICEval result type"

                        val call =
                            FunctionCall{regArgs=closureEntry @ regArgs, stackArgs=stackArgs,
                                         results=results, callKind=callKind, saveRegs=[]}
                        val callBlock =
                            case currHandler of
                                NONE => BlockSimple call :: codeArgs
                            |   SOME h => BlockOptionalHandle{call=call, handler=h, label=newLabel()}  :: codeArgs
                    in
                        moveResult @ callBlock
                    end
            in
                (callCode, RegisterArgument target, tailCall (* We've exited if this was a tail jump *))
            end

        |   codeToICodeRev(BICNullary{oper=BuiltIns.GetCurrentThreadId}, _, _, destination, tailCode) =
            (* Get the ID of the current thread. *)
            let
                val target = asTarget destination
            in
                (BlockSimple(LoadMemReg{offset=memRegThreadSelf, dest=target, kind=movePolyWord}) :: tailCode, RegisterArgument target, false)
            end

        |   codeToICodeRev(BICNullary{oper=BuiltIns.CPUPause}, _, _, destination, tailCode) =
            (* Pause during spinlock phase of mutex locking. *)
                moveIfNotAllowedRev(destination, BlockSimple PauseCPU :: tailCode, (* Unit result *) IntegerConstant(tag 0))

        |   codeToICodeRev(BICNullary{oper=BuiltIns.CreateMutex}, _, _, destination, tailCode) =
            let
                (* Allocate memory for a mutex.  Use a native word as a mutable, weak, no-overwrite, byte cell
                   which is the same as a volatileRef. This ensures that it will always be cleared when it is
                   loaded even if it was locked when it was saved. *)
                val target = asTarget destination
                val flags = Word8.orb(F_mutable, Word8.orb(F_weak, Word8.orb(F_noOverwrite, F_bytes))) (* 0wx69 *)
            in
                (BlockSimple InitialisationComplete ::
                 BlockSimple(StoreArgument{source=IntegerConstant 0, base=target, offset=0,
                    index=memIndexOrObject, kind=moveNativeWord, isMutable=false }) ::
                 BlockSimple(AllocateMemoryOperation{size=Word.toInt(nativeWordSize div wordSize), flags=flags, dest=target, saveRegs=[]}) ::
                    tailCode, RegisterArgument target, false)
            end

        |   codeToICodeRev(BICUnary instr, context, isTail, destination, tailCode) =
                codeToICodeUnaryRev(instr, context, isTail, destination, tailCode)

        |   codeToICodeRev(BICBinary instr, context, isTail, destination, tailCode) =
                codeToICodeBinaryRev(instr, context, isTail, destination, tailCode)

        |   codeToICodeRev(BICArbitrary{oper, shortCond, arg1, arg2, longCall}, context, _, destination, tailCode) =
            let
                val startLong = newLabel() and resultLabel = newLabel()
                val target = asTarget destination
                val condResult = newMergeReg()
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
                     BlockSimple(LoadArgument{source=RegisterArgument condResult, dest=target, kind=movePolyWord})],
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
                val closure = makeConstantClosure()
                val () = codeFunctionToX86(lambda, debugSwitches, closure)
                (* Return the closure itself as the value. *)
            in
                moveIfNotAllowedRev(destination, tailCode, AddressConstant(closureAsAddress closure))
            end

        |   codeToICodeRev(BICLambda(lambda as { closure, ...}), context, isTail, destination, tailCode) =
            (* Non-empty closure.  Ignore stack closure option at the moment. *)
            let
                val closureRef = makeConstantClosure()
                val () = codeFunctionToX86(lambda, debugSwitches, closureRef)
            in
                if targetArch = ObjectId32Bit
                then
                let
                    val target = asTarget destination
                    val memAddr = newPReg()
                    fun loadFields([], n, tlCode) =
                        let
                            val codeReg = newUReg()
                            val closureReg = newPReg()
                        in
                            (* The code address occupies the first native word but we need to extract it at
                               run-time.  We don't currently have a way to have 64-bit constants. *)
                            BlockSimple(
                                StoreArgument{ source=RegisterArgument codeReg, offset=0, base=memAddr, index=ObjectIndex, kind=moveNativeWord, isMutable=false}) ::
                            BlockSimple(LoadArgument{ source=MemoryLocation{offset=0, base=closureReg, index=ObjectIndex, cache=NONE}, dest=codeReg, kind=Move64Bit}) ::
                            BlockSimple(LoadArgument{ source=AddressConstant(toMachineWord closureRef), dest=closureReg, kind=movePolyWord}) ::
                            BlockSimple(AllocateMemoryOperation{size=n, flags=F_closure, dest=memAddr, saveRegs=[]}) :: tlCode
                        end
                    |   loadFields(f :: rest, n, tlCode) =
                        let
                            (* Defer the evaluation if possible.  We may have a constant that we can't move
                               directly but it's better to load it after the allocation otherwise we will
                               have to push the register if we need to GC. *)
                            val (code1, source1, _) = codeToICodeRev(BICExtract f, context, false, Allowed allowDefer, tlCode)
                            val restAndAlloc = loadFields(rest, n+1, code1)
                            val (code2, source, _)  = moveIfNotAllowedRev(Allowed allowInMemMove, restAndAlloc, source1)
                            val storeValue =
                                BlockSimple(StoreArgument{ source=source, offset=n*Word.toInt wordSize, base=memAddr,
                                    index=ObjectIndex, kind=movePolyWord, isMutable=false})
                        in
                            storeValue :: code2
                        end
                    val code =
                        BlockSimple InitialisationComplete ::
                            BlockSimple(LoadArgument{source=RegisterArgument memAddr, dest=target, kind=movePolyWord}) ::
                            loadFields(closure, 2, tailCode)
                in
                    (code, RegisterArgument target, false)
                end
                (* Treat it as a tuple with the code as the first field. *)
                else codeToICodeRev(BICTuple(BICConstnt(codeAddressFromClosure closureRef, []) :: map BICExtract closure), context, isTail, destination, tailCode)
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
                val condResult = newMergeReg()
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
                         [BlockSimple(LoadArgument{source=RegisterArgument condResult, dest=target, kind=movePolyWord}),
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
                val targetReg = newMergeReg()
                
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
                                                   operand2=IntegerConstant(semitag(Word.toLargeInt firstIndex)), ccRef=newCCRef(),
                                                   opSize=polyWordOpSize})
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
                                BlockSimple(WordComparison{arg1=tReg1, arg2=IntegerConstant(tag numberOfCases), ccRef=ccRef1, opSize=polyWordOpSize}) ::
                                BlockSimple(LoadArgument {source=RegisterArgument testReg, dest=tReg1, kind=movePolyWord}) :: testCode
                    in
                        (testCode2, [defLab1])
                    end
                
                (* Make a label for each item in the list. *)
                val codeLabels = map (fn _ => newLabel()) cases
                
                (* Create an exit label in case it's needed. *)
                val labelForExit = if isTail then ~1 (* Illegal label. *) else newLabel()

                (* Generate the code for each of the cases and the default.  We need to put an
                   unconditional branch after each to skip the other cases. *)
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
                    else BlockSimple(LoadArgument{source=RegisterArgument targetReg, dest=target, kind=movePolyWord}) ::
                            BlockLabel labelForExit :: codedCases
            in
                (copyToTarget, RegisterArgument target, isTail (* We have always exited on a tail. *))
            end

        |   codeToICodeRev(BICBeginLoop {loop, arguments}, context as { stackPtr, currHandler, overflowBlock, ...},
                           isTail, destination, tailCode) =
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
                    codeToICodeRev(loop,
                        {loopArgs=SOME (loopRegs, loopLabel, stackPtr), stackPtr=stackPtr,
                         currHandler=currHandler, overflowBlock=overflowBlock },
                            isTail, SpecificPReg target, BlockLabel loopLabel :: BlockSimple BeginLoop :: argCode)
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

        |   codeToICodeRev(BICHandle{exp, handler, exPacketAddr}, context as { stackPtr, loopArgs, overflowBlock, ... }, isTail, destination, tailCode) =
            let
                (* As with BICCond and BICCase we need to create a new register for the
                   result in case we need to push it to the stack. *)
                val handleResult = newMergeReg()
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
                    codeToICodeRev(exp, {stackPtr=stackPtr+2, loopArgs=loopArgs, currHandler=SOME handlerLab, overflowBlock=overflowBlock},
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
                (BlockSimple(LoadArgument{source=RegisterArgument handleResult, dest=target, kind=movePolyWord}) :: addLabel,
                    RegisterArgument target, isTail)
            end

        |   codeToICodeRev(BICTuple fields, context, _, destination, tailCode) =
            let
                (* TODO: This is a relic of the old fall-back code-generator.  It required
                   the result of a tuple to be at the top of the stack.  It should be changed. *)
                val target = asTarget destination (* Actually we want this. *)
                val memAddr = newPReg()

                fun loadFields([], n, tlCode) = allocateWithProfileRev(n, 0w0, memAddr, tlCode)
                    
                |   loadFields(f :: rest, n, tlCode) =
                    let
                        (* Defer the evaluation if possible.  We may have a constant that we can't move
                           directly but it's better to load it after the allocation otherwise we will
                           have to push the register if we need to GC. *)
                        val (code1, source1, _) = codeToICodeRev(f, context, false, Allowed allowDefer, tlCode)
                        val restAndAlloc = loadFields(rest, n+1, code1)
                        val (code2, source, _)  = moveIfNotAllowedRev(Allowed allowInMemMove, restAndAlloc, source1)
                        val storeValue =
                            BlockSimple(StoreArgument{ source=source, offset=n*Word.toInt wordSize, base=memAddr,
                                index=memIndexOrObject, kind=movePolyWord, isMutable=false})
                    in
                        storeValue :: code2
                    end
                val code =
                    BlockSimple InitialisationComplete ::
                        BlockSimple(LoadArgument{source=RegisterArgument memAddr, dest=target, kind=movePolyWord}) ::
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
                val (codeContainer, storeInstr) =
                    case getContainerIfPresent container of
                        ContainerOnStack{container, stackOffset} =>
                        let
                            fun storeToStack(source, destWord) =
                                StoreToStack{source=source, container=container, field=destWord,
                                    stackOffset=stackPtr-stackOffset+destWord}
                        in
                            (tailCode, storeToStack)
                        end

                    |   ContainerInRegs regs =>
                        let
                            fun copy(source, destWord) =
                                LoadArgument{source=source, dest=List.nth(regs, destWord), kind=movePolyWord}
                        in
                            (tailCode, copy)
                        end

                    |   NoContainer => 
                        let
                            val containerTarget = newPReg()
                            val (codeContainer, _, _) =
                                codeToICodeRev(container, context, false, SpecificPReg containerTarget, tailCode)
                            fun createStore (source, destWord) =
                                StoreArgument{source=source, offset=destWord*Word.toInt nativeWordSize, base=containerTarget, index=NoMemIndex,
                                              kind=moveNativeWord, isMutable=false}
                        in
                            (codeContainer, createStore)
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
                        let (* Copy a heap tuple.  If the source is a stack tuple we need to copy it using
                               instructions that reference the container until the copy is complete.
                               Taking the address and then copying can result in the space used by
                               the container being reused for pushing registers after the address
                               has been taken but before the copy. *)
                            val (codeTuple, loadField) =
                                case getContainerIfPresent tuple of
                                    ContainerOnStack {container, stackOffset} =>
                                    let
                                        fun getAddr sourceWord =
                                            StackLocation{wordOffset=stackPtr-stackOffset+sourceWord, container=container,
                                                          field=sourceWord, cache=NONE}
                                    in
                                        (codeContainer, getAddr)
                                    end

                                |   ContainerInRegs regs =>
                                    let
                                        fun copyReg sourceWord = RegisterArgument(List.nth(regs, sourceWord))
                                    in
                                        (codeContainer, copyReg)
                                    end

                                |   NoContainer =>
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
                                    BlockSimple(LoadArgument{source=loadField sourceWord, dest=loadReg, kind=movePolyWord}) ::
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

        |   codeToICodeRev(BICTagTest{test, tag=tagValue, ...}, context, _, destination, tailCode) =
            (* Check the "tag" word of a union (datatype).  N.B.  Not the same as testing the
               tag bit of a word. *)
            let
                val ccRef = newCCRef()
                val memOrReg = { anyConstant=false, const32s=false, memAddr=true, existingPreg=true }
                val (testCode, tagArg, _) = codeToICodeRev(test, context, false, Allowed memOrReg, tailCode)
                val target = asTarget destination
            in
                (makeBoolResultRev(JE, ccRef, target,
                    (* Use CompareLiteral because the tag must fit in 32-bits. *)
                    BlockSimple(CompareLiteral{arg1=tagArg,
                            arg2=tag(Word.toLargeInt tagValue), opSize=polyWordOpSize, ccRef=ccRef}) :: testCode),
                    RegisterArgument target, false)
            end

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

        |   codeToICodeRev(BICBlockOperation ({kind=BlockOpEqualByte, sourceLeft, destRight, length}), context, _, destination, tailCode) =
            let
                val vec1Reg = newUReg() and vec2Reg = newUReg()
                val ccRef = newCCRef()
                val (leftCode, leftUntag, {base=leftBase, offset=leftOffset, index=leftIndex, ...}) =
                    codeAddressRev(sourceLeft, 0w1, context, tailCode)
                val (rightCode, rightUntag, {base=rightBase, offset=rightOffset, index=rightIndex, ...}) =
                    codeAddressRev(destRight, 0w1, context, leftCode)
                val (lengthCode, lengthUntag, lengthArg) = codeAsUntaggedToRegRev(length, false (* unsigned *), context, rightCode)
                val target = asTarget destination
                val code =
                    makeBoolResultRev(JE, ccRef, target,
                        BlockSimple(CompareByteVectors{ vec1Addr=vec1Reg, vec2Addr=vec2Reg, length=lengthArg, ccRef=ccRef }) ::
                        lengthUntag @ BlockSimple(loadAddress{base=rightBase, offset=rightOffset, index=rightIndex, dest=vec2Reg}) ::
                        rightUntag @ BlockSimple(loadAddress{base=leftBase, offset=leftOffset, index=leftIndex, dest=vec1Reg}) ::
                        leftUntag @ lengthCode)
            in
                (code, RegisterArgument target, false)
            end

        |   codeToICodeRev(BICBlockOperation instr, context, isTail, destination, tailCode) =
            let
                val (code, dest, haveExited) = codeToICodeBlock(instr, context, isTail, destination)
            in
                (revApp(code, tailCode), dest, haveExited)
            end

        and codeToICodeUnaryRev({oper=BuiltIns.NotBoolean, arg1}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val ccRef = newCCRef()
                val allow = Allowed {anyConstant=false, const32s=false, memAddr=true, existingPreg=true}
                val (argCode, testDest, _) = codeToICodeRev(arg1, context, false, allow, tailCode)
            in
                (* Test the argument and return a boolean result.  If either the argument is a condition
                   or the result is used in a test this will be better than using XOR. *)
                (makeBoolResultRev(JNE, ccRef, target,
                        BlockSimple(CompareLiteral{arg1=testDest, arg2=tag 1, opSize=polyWordOpSize, ccRef=ccRef}) ::
                            argCode), RegisterArgument target, false)
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.IsTaggedValue, arg1}, context, _, destination, tailCode) =
            let
                val ccRef = newCCRef()
                val memOrReg = { anyConstant=false, const32s=false, memAddr=true, existingPreg=true }
                val (testCode, testResult, _) = codeToICodeRev(arg1, context, false, Allowed memOrReg, tailCode)
                (* Test the tag bit.  This sets the zero bit if the value is untagged. *)
                val target = asTarget destination
            in
                (makeBoolResultRev(JNE, ccRef, target,
                    BlockSimple(TestTagBit{arg=testResult, ccRef=ccRef}) :: testCode), RegisterArgument target, false)
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.MemoryCellLength, arg1}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val argReg1 = newUReg() and argReg2 = newUReg() and argReg3 = newUReg() (* These are untagged until the tag is put in. *)
                and ccRef1 = newCCRef() and ccRef2 = newCCRef() and ccRef3 = newCCRef()
                (* Get the length of a memory cell (heap object).  We need to mask out the
                   top byte containing the flags and to tag the result.  The mask is 56 bits on
                   64-bit which won't fit in an inline constant.  Since we have to shift it
                   anyway we might as well do this by shifts. *)
                val (argCode, addrReg) = codeToPRegRev(arg1, context, tailCode)
            in
                (BlockSimple(ArithmeticFunction{oper=OR, resultReg=target, operand1=argReg3, operand2=IntegerConstant 1, ccRef=ccRef3, opSize=polyWordOpSize}) ::
                    BlockSimple(ShiftOperation{shift=SHR, resultReg=argReg3, operand=argReg2, shiftAmount=IntegerConstant 7 (* 8-tagshift*), ccRef=ccRef2, opSize=polyWordOpSize }) ::
                    BlockSimple(ShiftOperation{shift=SHL, resultReg=argReg2, operand=argReg1, shiftAmount=IntegerConstant 8, ccRef=ccRef1, opSize=polyWordOpSize }) ::
                    BlockSimple(LoadArgument{source=wordOffsetAddress(~1, addrReg), dest=argReg1, kind=movePolyWord}) :: argCode,
                RegisterArgument target, false)
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.MemoryCellFlags, arg1}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val argReg1 = newUReg()
                val (argCode, addrReg) = codeToPRegRev(arg1, context, tailCode)
            in
                (BlockSimple(TagValue{ source=argReg1, dest=target, isSigned=false, opSize=OpSize32 }) ::
                 BlockSimple(LoadArgument{source=MemoryLocation{offset= ~1, base=addrReg, index=memIndexOrObject, cache=NONE}, dest=argReg1, kind=MoveByte}) ::
                 argCode, RegisterArgument target, false)
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.ClearMutableFlag, arg1}, context, _, destination, tailCode) =
            let
                val (argCode, addrReg) = codeToPRegRev(arg1, context, tailCode)
            in
                moveIfNotAllowedRev(destination, BlockSimple(LockMutable{addr=addrReg}) :: argCode, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.LongWordToTagged, arg1}, context, _, destination, tailCode) =
            let (* This is exactly the same as StringLengthWord at the moment.
                   TODO: introduce a new ICode entry so that the next stage can optimise
                   longword operations. *)
                val target = asTarget destination
                val argReg1 = newUReg()
                val (argCode, addrReg) = codeToPRegRev(arg1, context, tailCode)
                val code =
                    BlockSimple(TagValue{ source=argReg1, dest=target, isSigned=false, opSize=polyWordOpSize }) ::
                    (* Use movePolyWord even on 32-in-64 since we're producing a 32-bit value anyway. *)
                    BlockSimple(LoadArgument{source=wordAt addrReg, dest=argReg1, kind=movePolyWord}) ::
                    argCode
            in
                (code, RegisterArgument target, false)
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.SignedToLongWord, arg1}, context, _, destination, tailCode) =
            let
                val addrReg = newPReg() and untagArg = newUReg()
                val (argCode, argReg1) = codeToPRegRev(arg1, context, tailCode)
                val (signExtend, sxReg) =
                    case targetArch of
                        ObjectId32Bit =>
                        let
                            val sReg = newUReg()
                        in
                            ([BlockSimple(SignExtend32To64{source=RegisterArgument argReg1, dest=sReg})], sReg)
                        end
                    |   _ => ([], argReg1)
                val code =
                    BlockSimple(BoxValue{boxKind=BoxLargeWord,  source=untagArg, dest=addrReg, saveRegs=[]}) ::
                    BlockSimple(UntagValue{source=sxReg, dest=untagArg, isSigned=true, cache=NONE, opSize=nativeWordOpSize}) ::
                    signExtend @ argCode
            in
                moveIfNotAllowedRev(destination, code, RegisterArgument addrReg)
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.UnsignedToLongWord, arg1}, context, _, destination, tailCode) =
            let
                val addrReg = newPReg() and untagArg = newUReg()
                val (argCode, argReg1) = codeToPRegRev(arg1, context, tailCode)
                val code =
                    BlockSimple(BoxValue{boxKind=BoxLargeWord,  source=untagArg, dest=addrReg, saveRegs=[]}) ::
                    (* We can just use a polyWord operation to untag the unsigned value. *)
                    BlockSimple(UntagValue{source=argReg1, dest=untagArg, isSigned=false, cache=NONE, opSize=polyWordOpSize}) ::
                    argCode
            in
                moveIfNotAllowedRev(destination, code, RegisterArgument addrReg)
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.RealNeg precision, arg1}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val fpRegSrc = newUReg() and fpRegDest = newUReg() and sse2ConstReg = newUReg()
                (* The SSE2 code uses an SSE2 logical operation to flip the sign bit.  This
                   requires the values to be loaded into registers first because the logical
                   operations require 128-bit operands. *)
                val (argCode, aReg1) = codeToPReg(arg1, context)
                (* Double precision values are always boxed and single precision values if they won't
                   fit in a word.  Otherwise we can using tagging. *)
                open BuiltIns
                val load =
                    if precision = PrecDouble
                    then BlockSimple(LoadArgument{source=wordAt aReg1, dest=fpRegSrc, kind=MoveDouble})
                    else if wordSize = 0w8
                    then BlockSimple(UntagFloat{source=RegisterArgument aReg1, dest=fpRegSrc, cache=NONE})
                    else BlockSimple(LoadArgument{source=wordAt aReg1, dest=fpRegSrc, kind=MoveFloat})
                val code =
                    case fpMode of
                        FPModeX87 =>
                            [BlockSimple(X87FPUnaryOps{ fpOp=FCHS, dest=fpRegDest, source=fpRegSrc})]
                    |   FPModeSSE2 =>
                        let
                            (* In single precision mode the sign bit is in the low 32-bits.  There
                               may be a better way to load it. *)
                            val signBit = if precision = PrecDouble then realSignBit else floatSignBit
                        in
                            [BlockSimple(LoadArgument{source=AddressConstant signBit, dest=sse2ConstReg, kind=MoveDouble}),
                             BlockSimple(SSE2FPBinary{opc=SSE2BXor, resultReg=fpRegDest, arg1=fpRegSrc, arg2=RegisterArgument sse2ConstReg})]
                        end
                val result = boxOrTagReal(fpRegDest, target, precision)
            in
                (revApp(argCode @ load :: code @ result, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.RealAbs precision, arg1}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val fpRegSrc = newUReg() and fpRegDest = newUReg() and sse2ConstReg = newUReg()
                val (argCode, aReg1) = codeToPReg(arg1, context)
                open BuiltIns
                val load =
                    if precision = PrecDouble
                    then BlockSimple(LoadArgument{source=wordAt aReg1, dest=fpRegSrc, kind=MoveDouble})
                    else if wordSize = 0w8
                    then BlockSimple(UntagFloat{source=RegisterArgument aReg1, dest=fpRegSrc, cache=NONE})
                    else BlockSimple(LoadArgument{source=wordAt aReg1, dest=fpRegSrc, kind=MoveFloat})
                val code =
                    case fpMode of
                        FPModeX87 => [BlockSimple(X87FPUnaryOps{ fpOp=FABS, dest=fpRegDest, source=fpRegSrc})]
                    |   FPModeSSE2 =>
                        let
                            val mask = if precision = PrecDouble then realAbsMask else floatAbsMask
                        in
                            [BlockSimple(LoadArgument{source=AddressConstant mask, dest=sse2ConstReg, kind=MoveDouble}),
                             BlockSimple(SSE2FPBinary{opc=SSE2BAnd, resultReg=fpRegDest, arg1=fpRegSrc, arg2=RegisterArgument sse2ConstReg})]
                        end
                val result = boxOrTagReal(fpRegDest, target, precision)
            in
                (revApp(argCode @ load :: code @ result, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.RealFixedInt precision, arg1}, context, _, destination, tailCode) =
            let
                open BuiltIns
                val target = asTarget destination
                val untagReg = newUReg() and fpReg = newUReg()
                val (argCode, aReg1) = codeToPReg(arg1, context)
                val floatOp =
                    case fpMode of
                        FPModeX87 => X87Float{ dest=fpReg, source=RegisterArgument untagReg}
                    |   FPModeSSE2 => SSE2IntToReal{ dest=fpReg, source=RegisterArgument untagReg, isDouble=precision=PrecDouble}
                val code = argCode @
                 [BlockSimple(UntagValue{source=aReg1, dest=untagReg, isSigned=true, cache=NONE, opSize=polyWordOpSize}),
                  BlockSimple floatOp] @
                  boxOrTagReal(fpReg, target, precision)
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.FloatToDouble, arg1}, context, _, destination, tailCode) =
            let
                (* Convert a single precision floating point value to double precision. *)
                val target = asTarget destination
                val fpReg = newUReg() and fpReg2 = newUReg()
                val (argCode, aReg1) = codeToPReg(arg1, context)
                (* MoveFloat always converts from single to double-precision. *)
                val unboxOrUntag =
                    case (fpMode, wordSize) of
                        (FPModeX87, _) => [BlockSimple(LoadArgument{source=wordAt aReg1, dest=fpReg2, kind=MoveFloat})]
                    |   (FPModeSSE2, 0w4) =>
                        [BlockSimple(LoadArgument{source=wordAt aReg1, dest=fpReg, kind=MoveFloat}),
                         BlockSimple(SSE2FPUnary{opc=SSE2UFloatToDouble, resultReg=fpReg2, source=RegisterArgument fpReg})]
                    |   (FPModeSSE2, _) =>
                        [BlockSimple(UntagFloat{source=RegisterArgument aReg1, dest=fpReg, cache=NONE}),
                         BlockSimple(SSE2FPUnary{opc=SSE2UFloatToDouble, resultReg=fpReg2, source=RegisterArgument fpReg})]
                val boxFloat = case fpMode of FPModeX87 => BoxX87Double | FPModeSSE2 => BoxSSE2Double
                val code = argCode @ unboxOrUntag @
                    [BlockSimple(BoxValue{boxKind=boxFloat, source=fpReg2, dest=target, saveRegs=[]})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.DoubleToFloat, arg1}, context, _, destination, tailCode) =
            let
                (* Convert a double precision value to a single precision using the current rounding
                   mode.  This is simpler than setting the rounding mode and then restoring it. *)
                val target = asTarget destination
                val fpReg = newUReg() and fpReg2 = newUReg()
                val (argCode, aReg1) = codeToPReg(arg1, context)
                (* In 32-bit mode we need to box the float.  In 64-bit mode we can tag it. *)
                val boxOrTag =
                    case fpMode of
                        FPModeX87 => [BlockSimple(BoxValue{boxKind=BoxX87Float, source=fpReg, dest=target, saveRegs=[]})]
                    |   FPModeSSE2 =>
                            BlockSimple(SSE2FPUnary{opc=SSE2UDoubleToFloat, resultReg=fpReg2, source=RegisterArgument fpReg}) ::
                                boxOrTagReal(fpReg2, target, BuiltIns.PrecSingle)
                val code = argCode @ [BlockSimple(LoadArgument{source=wordAt aReg1, dest=fpReg, kind=MoveDouble})] @ boxOrTag
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.RealToInt(precision, rndMode), arg1}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val chkOverflow = newCCRef()
                val convResult = newUReg() and wrkReg2 = newUReg()
                (* Convert a floating point value to an integer.  We need to raise overflow if the
                   result is out of range.  We first convert the value to 32/64 bits then tag it.
                   An overflow can happen either because the real number does not fit in 32/64
                   bits or if it is not a 31/63 bit value.  Fortunately, if the first conversion
                   fails the result is a value that causes an overflow when we try it shift it
                   so the check for overflow only needs to happen there.
                   There is an SSE2 instruction that implements truncation (round to zero)
                   directly but in other cases we need to set the rounding mode. *)
                val doConvert =
                    case (fpMode, precision) of
                        (FPModeX87, _) =>
                        let
                        
                            val fpReg = newUReg()
                            val (argCode, aReg) = codeToPReg(arg1, context)
                            fun doConvert() = [BlockSimple(X87RealToInt{source=fpReg, dest=convResult })]
                        in
                            argCode @
                                [BlockSimple(LoadArgument{source=wordAt aReg, dest=fpReg, kind=MoveDouble})] @
                                    setAndRestoreRounding(rndMode, doConvert)
                        end

                    |   (FPModeSSE2, BuiltIns.PrecDouble) =>
                        let
                            val (argCode, argReg) = codeToPReg(arg1, context)
                            fun doConvert() =
                                [BlockSimple(
                                    SSE2RealToInt{source=wordAt argReg, dest=convResult, isDouble=true,
                                        isTruncate = rndMode = IEEEReal.TO_ZERO }) ]
                        in
                            argCode @ (
                                case rndMode of
                                    IEEEReal.TO_ZERO => doConvert()
                                |   _ => setAndRestoreRounding(rndMode, doConvert))
                        end

                    |   (FPModeSSE2, BuiltIns.PrecSingle) =>
                        let
                            val (argCode, aReg) = codeToPReg(arg1, context)
                            val fpReg = newUReg()
                            fun doConvert() =
                                [BlockSimple(
                                    SSE2RealToInt{source=RegisterArgument fpReg, dest=convResult, isDouble=false,
                                        isTruncate = rndMode = IEEEReal.TO_ZERO })]
                        in
                            argCode @ [BlockSimple(UntagFloat{source=RegisterArgument aReg, dest=fpReg, cache=NONE})] @
                            (
                                case rndMode of
                                    IEEEReal.TO_ZERO => doConvert()
                                |   _ => setAndRestoreRounding(rndMode, doConvert)
                            )
                        end

                val checkAndTag =
                    BlockSimple(ShiftOperation{ shift=SHL, resultReg=wrkReg2, operand=convResult, shiftAmount=IntegerConstant 1, ccRef=chkOverflow, opSize=polyWordOpSize}) ::
                        checkOverflow context chkOverflow @
                        [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=wrkReg2, operand2=IntegerConstant 1, ccRef = newCCRef(), opSize=polyWordOpSize})]
            in
                (revApp(doConvert @ checkAndTag, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.TouchAddress, arg1}, context, _, destination, tailCode) =
            let
                (* Put the value in a register.  This is not entirely necessary but ensures that if the value is
                   a constant the constant will be included in the code. *)
                val (argCode, aReg) = codeToPRegRev(arg1, context, tailCode)
            in
                moveIfNotAllowedRev(destination, BlockSimple(TouchArgument{source=aReg}) :: argCode,
                        (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.AllocCStack, arg1}, context, _, destination, tailCode) =
            (* Allocate space on the C stack.  Assumes that the argument has already been aligned. *)
            let
                val target = asTarget destination
                val (argCode, untaggedArg) =
                    case arg1 of
                        BICConstnt(value, _) => (tailCode, IntegerConstant(Word.toLargeInt(toShort value)) (* Leave untagged *))
                    |   _ =>
                        let
                            val (argCode, aReg) = codeToPRegRev(arg1, context, tailCode)
                            val arg1Untagged = newUReg()
                        in
                            (
                                BlockSimple(UntagValue{source=aReg, dest=arg1Untagged, isSigned=false, cache=NONE, opSize=polyWordOpSize}) :: argCode,
                                RegisterArgument arg1Untagged
                            )
                        end
                val argReg1 = newUReg() and resReg1 = newUReg()
                val code =
                    BlockSimple(BoxValue{boxKind=BoxLargeWord, source=resReg1, dest=target, saveRegs=[]}) ::
                    BlockSimple(StoreMemReg{offset=memRegCStackPtr, source=resReg1, kind=moveNativeWord}) ::
                    BlockSimple(ArithmeticFunction{oper=SUB, resultReg=resReg1, operand1=argReg1, operand2=untaggedArg,
                            ccRef=newCCRef(), opSize=nativeWordOpSize}) ::
                    BlockSimple(LoadMemReg{offset=memRegCStackPtr, dest=argReg1, kind=moveNativeWord}) ::
                    argCode
            in
                (code, RegisterArgument target, false)
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.LockMutex, arg1}, context, _, destination, tailCode) =
            let
                (* Temporarily don't bother with the spin-lock. *)
                val incrReg = newUReg() and resultReg = newUReg() and ccRef = newCCRef()
                val (argCode, addrReg) = codeToPRegRev(arg1, context, tailCode)
                val target = asTarget destination
                val code =
                    BlockSimple(CompareLiteral{arg1=RegisterArgument resultReg, arg2=0, opSize=nativeWordOpSize, ccRef=ccRef }) ::
                    BlockSimple(AtomicExchangeAndAdd{base=addrReg, source=incrReg, resultReg=resultReg}) ::
                    BlockSimple(LoadArgument{source=IntegerConstant 1, dest=incrReg, kind=moveNativeWord}) ::
                    argCode
            in
                (makeBoolResultRev(JE, ccRef, target, code), RegisterArgument target, false)
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.TryLockMutex, arg1}, context, _, destination, tailCode) =
            let
                val toStoreReg = newUReg() and compareReg = newUReg() and resultReg = newUReg() and ccRef = newCCRef()
                val (argCode, addrReg) = codeToPRegRev(arg1, context, tailCode)
                val target = asTarget destination
                (* If the current value is zero (unlocked) set it to one (locked) otherwise do nothing. *)
                val code =
                    BlockSimple(AtomicCompareAndExchange{base=addrReg, compare=compareReg,
                                                         toStore=toStoreReg, resultReg=resultReg, ccRef=ccRef}) ::
                    BlockSimple(LoadArgument{source=IntegerConstant 0, dest=compareReg, kind=moveNativeWord}) ::
                    BlockSimple(LoadArgument{source=IntegerConstant 1, dest=toStoreReg, kind=moveNativeWord}) ::
                    argCode
            in
                (makeBoolResultRev(JE, ccRef, target, code), RegisterArgument target, false)
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.UnlockMutex, arg1}, context, _, destination, tailCode) =
            let (* Unlock a mutex - (atomically) exchange with 0 (unlock) and return true if the
                   previous value was 1 i.e. only this thread has locked it. *)
                val newValReg = newUReg() and resultReg = newUReg() and ccRef = newCCRef()
                val (argCode, addrReg) = codeToPRegRev(arg1, context, tailCode)
                val target = asTarget destination
                val code =
                    BlockSimple(CompareLiteral{arg1=RegisterArgument resultReg, arg2=1, opSize=nativeWordOpSize, ccRef=ccRef }) ::
                    BlockSimple(AtomicExchange{base=addrReg, source=newValReg, resultReg=resultReg}) ::
                    BlockSimple(LoadArgument{source=IntegerConstant 0, dest=newValReg, kind=moveNativeWord}) ::
                    argCode
            in
                (makeBoolResultRev(JE, ccRef, target, code), RegisterArgument target, false)
            end

        |   codeToICodeUnaryRev({oper=BuiltIns.Log2Word, arg1}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val argReg2 = newUReg()
                val (argCode, argReg1) = codeToPRegRev(arg1, context, tailCode)
                (* We compute the bit position using a tagged word so we have to subtract one from the result.
                   This can be combined with the tagging. *)
                val code =
                    BlockSimple(LoadEffectiveAddress{base=SOME argReg2, offset= ~1, index=MemIndex1 argReg2, dest=target, opSize=polyWordOpSize}) ::
                    BlockSimple(BitScanReverse{ source=RegisterArgument argReg1, dest=argReg2}) ::
                    argCode
            in
                (code, RegisterArgument target, false)
            end

        and codeToICodeBinaryRev({oper=BuiltIns.WordComparison{test, isSigned}, arg1, arg2=BICConstnt(arg2Value, _)}, context, _, destination, tailCode) =
            let
                (* Comparisons.  Because this is also used for pointer equality and even for exception matching it
                   is perfectly possible that the argument could be an address.
                   The higher levels used to generate this for pointer equality. *)
                val ccRef = newCCRef()
                val comparison =
                    (* If the argument is a tagged value that will fit in 32-bits we can use
                       the literal version.  Use toLargeIntX here because the value will be
                       sign-extended even if we're actually doing an unsigned comparison. *)
                    if isShort arg2Value andalso is32bit(tag(Word.toLargeIntX(toShort arg2Value)))
                    then
                    let
                        val allow = Allowed {anyConstant=false, const32s=false, memAddr=true, existingPreg=true}
                    in
                        (* We're often comparing with a character or a string length field that has to be
                           untagged.  In that case we can avoid loading it into a register and untagging it
                           by doing the comparison directly. *)
                        case arg1 of
                            BICLoadOperation{kind=LoadStoreUntaggedUnsigned, address} =>
                            let
                                val (codeBaseIndex, codeUntag, memLoc) = codeAddressRev(address, wordSize, context, tailCode)
                                val literal = Word.toLargeIntX(toShort arg2Value)
                            in
                                BlockSimple(CompareLiteral{arg1=MemoryLocation memLoc, arg2=literal, opSize=polyWordOpSize, ccRef=ccRef}) ::
                                    codeUntag @ codeBaseIndex
                            end
                        |   BICLoadOperation{kind=LoadStoreMLByte _, address} =>
                            let
                                val (codeBaseIndex, codeUntag, {base, index, offset, ...}) =
                                    codeAddressRev(address, 0w1, context, tailCode)
                                val _ = toShort arg2Value >= 0w0 andalso toShort arg2Value < 0w256
                                            orelse raise InternalError "Compare byte not a byte"
                                val literal = Word8.fromLargeWord(Word.toLargeWord(toShort arg2Value))
                            in
                                BlockSimple(CompareByteMem{arg1={base=base, index=index, offset=offset}, arg2=literal, ccRef=ccRef}) ::
                                    codeUntag @ codeBaseIndex
                            end
                        |   BICUnary({oper=BuiltIns.MemoryCellFlags, arg1}) =>
                                (* This occurs particularly in arbitrary precision comparisons. *)
                            let
                                val (baseCode, baseReg) = codeToPRegRev(arg1, context, tailCode)
                                val _ = toShort arg2Value >= 0w0 andalso toShort arg2Value < 0w256
                                            orelse raise InternalError "Compare memory cell not a byte"
                                val literal = Word8.fromLargeWord(Word.toLargeWord(toShort arg2Value))
                            in
                                BlockSimple(CompareByteMem{arg1={base=baseReg, index=memIndexOrObject, offset= ~1}, arg2=literal, ccRef=ccRef}) ::
                                    baseCode
                            end
                        |    _ =>
                            let
                                (* TODO: We could include rarer cases of tagging by looking at
                                   the code and seeing if it's a TagValue. *)
                                val (testCode, testDest, _) = codeToICodeRev(arg1, context, false, allow, tailCode)
                                val literal = tag(Word.toLargeIntX(toShort arg2Value))
                            in
                                BlockSimple(CompareLiteral{arg1=testDest, arg2=literal, opSize=polyWordOpSize, ccRef=ccRef}) ::
                                    testCode
                            end    
                    end
                    else (* Addresses or larger values.  We need to use a register comparison. *)
                    let
                        val (testCode, testReg) = codeToPRegRev(arg1, context, tailCode)
                        val arg2Arg = constantAsArgument arg2Value
                    in
                        BlockSimple(WordComparison{arg1=testReg, arg2=arg2Arg, ccRef=ccRef, opSize=polyWordOpSize}) ::
                            testCode
                    end
                val target = asTarget destination
            in
                (makeBoolResultRev(testAsBranch(test, isSigned, true), ccRef, target, comparison), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.WordComparison{test, isSigned}, arg1=BICConstnt(arg1Value, _), arg2}, context, _, destination, tailCode) =
            let
                (* If we have the constant first we need to reverse the test so the first argument is a register. *)
                val ccRef = newCCRef()
                val comparison =
                    if isShort arg1Value andalso is32bit(tag(Word.toLargeIntX(toShort arg1Value)))
                    then
                    let
                        val allow = Allowed {anyConstant=false, const32s=false, memAddr=true, existingPreg=true}
                        val (testCode, testDest, _) = codeToICodeRev(arg2, context, false, allow, tailCode)
                        val literal = tag(Word.toLargeIntX(toShort arg1Value))
                    in
                        BlockSimple(CompareLiteral{arg1=testDest, arg2=literal, opSize=polyWordOpSize, ccRef=ccRef}) ::
                            testCode
                    end
                    else (* Addresses or larger values.  We need to use a register comparison. *)
                    let
                        val (testCode, testReg) = codeToPRegRev(arg2, context, tailCode)
                        val arg1Arg = constantAsArgument arg1Value
                    in
                        BlockSimple(WordComparison{arg1=testReg, arg2=arg1Arg, ccRef=ccRef, opSize=polyWordOpSize}) ::
                            testCode
                    end

                val target = asTarget destination
            in
                (makeBoolResultRev(testAsBranch(leftRightTest test, isSigned, true), ccRef, target, comparison),
                     RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.WordComparison {test, isSigned}, arg1, arg2}, context, _, destination, tailCode) =
            let
                val ccRef = newCCRef()
                val memOrReg = { anyConstant=false, const32s=false, memAddr=true, existingPreg=true }
                val (arg1Code, arg1Result, _) = codeToICodeRev(arg1, context, false, Allowed memOrReg, tailCode)
                val (arg2Code, arg2Result, _) = codeToICodeRev(arg2, context, false, Allowed memOrReg, arg1Code)
                val target = asTarget destination
                val code =
                    case (arg1Result, arg2Result) of
                        (RegisterArgument arg1Reg, arg2Result) =>
                            makeBoolResultRev(testAsBranch(test, isSigned, true), ccRef, target,
                                BlockSimple(WordComparison{arg1=arg1Reg, arg2=arg2Result, ccRef=ccRef, opSize=polyWordOpSize}) ::
                                    arg2Code)
                    |   (arg1Result, RegisterArgument arg2Reg) =>
                            (* The second argument is in a register - switch the sense of the test. *)
                            makeBoolResultRev(testAsBranch(leftRightTest test, isSigned, true), ccRef, target,
                                BlockSimple(WordComparison{arg1=arg2Reg, arg2=arg1Result, ccRef=ccRef, opSize=polyWordOpSize}) ::
                                    arg2Code)
                    |   (arg1Result, arg2Result) =>
                        let (* Have to load an argument - pick the first. *)
                            val arg1Reg = newPReg()
                        in
                            makeBoolResultRev(testAsBranch(test, isSigned, true), ccRef, target,
                                BlockSimple(WordComparison{arg1=arg1Reg, arg2=arg2Result, ccRef=ccRef, opSize=polyWordOpSize}) ::
                                BlockSimple(LoadArgument{source=arg1Result, dest=arg1Reg, kind=movePolyWord})  :: arg2Code)
                        end
            in
                (code, RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.PointerEq, arg1, arg2}, context, isTail, destination, tailCode) =
                (* Equality of general values which can include pointers. This can be treated exactly as a word equality.
                   It has to be analysed differently for indexed cases. *)
                codeToICodeBinaryRev({oper=BuiltIns.WordComparison{test=BuiltIns.TestEqual, isSigned=false}, arg1=arg1, arg2=arg2},
                                      context, isTail, destination, tailCode)

        |   codeToICodeBinaryRev({oper=BuiltIns.FixedPrecisionArith oper, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val code = codeFixedPrecisionArith(oper, arg1, arg2, context, target, checkOverflow context)
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1, arg2=BICConstnt(value, _)}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                (* If the argument is a constant we can subtract the tag beforehand.
                   N.B. it is possible to have type-incorrect values in dead code. i.e. code that will
                   never be executed because of a run-time check.  *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
                val (arg1Code, aReg1) = codeToPRegRev(arg1, context, tailCode)
            in
                (BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg1, operand2=IntegerConstant constVal, ccRef = newCCRef(), opSize=polyWordOpSize}) ::
                    arg1Code, RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1=BICConstnt(value, _), arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
                val (arg2Code, aReg2) = codeToPRegRev(arg2, context, tailCode)
            in
                (BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg2, operand2=IntegerConstant constVal, ccRef = newCCRef(), opSize=polyWordOpSize}) ::
                    arg2Code, RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                (* Use LEA to do the addition since we're not concerned with overflow.  This is shorter than
                   subtracting the tag and adding the values and also moves the result into the
                   appropriate register. *)
                val code =
                    arg1Code @ arg2Code @
                    [BlockSimple(LoadEffectiveAddress{base=SOME aReg1, offset= ~1, index=MemIndex1 aReg2, dest=target, opSize=polyWordOpSize})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.WordArith BuiltIns.ArithSub, arg1, arg2=BICConstnt(value, _)}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
                val (arg1Code, aReg1) = codeToPRegRev(arg1, context, tailCode)
            in
                (BlockSimple(ArithmeticFunction{oper=SUB, resultReg=target, operand1=aReg1, operand2=IntegerConstant constVal, ccRef=newCCRef(), opSize=polyWordOpSize}) ::
                    arg1Code, RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.WordArith BuiltIns.ArithSub, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val aReg3 = newPReg()
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val code =
                    arg1Code @ arg2Code @
                    (* Do the subtraction and add in the tag bit.  This could be reordered if we have cascaded operations
                       since we don't need to check for overflow. *)
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=aReg1, operand2=RegisterArgument aReg2, ccRef=newCCRef(), opSize=polyWordOpSize}),
                     BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg3, operand2=IntegerConstant 1, ccRef=newCCRef(), opSize=polyWordOpSize})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.WordArith BuiltIns.ArithMult, arg1, arg2=BICConstnt(value, _)}, context, _, destination, tailCode) =
                codeMultiplyConstantWordRev(arg1, context, destination, if isShort value then toShort value else 0w0, tailCode)

        |   codeToICodeBinaryRev({oper=BuiltIns.WordArith BuiltIns.ArithMult, arg1=BICConstnt(value, _), arg2}, context, _, destination, tailCode) =
                codeMultiplyConstantWordRev(arg2, context, destination, if isShort value then toShort value else 0w0, tailCode)

        |   codeToICodeBinaryRev({oper=BuiltIns.WordArith BuiltIns.ArithMult, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val arg1Untagged = newUReg()
                and arg2Untagged = newUReg() and resUntagged = newUReg()
                val code =
                    arg1Code @ arg2Code @
                    (* Shift one argument and subtract the tag from the other.  It's possible this could be reordered
                       if we have a value that is already untagged. *)
                    [BlockSimple(UntagValue{source=aReg1, dest=arg1Untagged, isSigned=false, cache=NONE, opSize=polyWordOpSize}),
                     BlockSimple(ArithmeticFunction{oper=SUB, resultReg=arg2Untagged, operand1=aReg2, operand2=IntegerConstant 1, ccRef=newCCRef(), opSize=polyWordOpSize}),
                     BlockSimple(Multiplication{resultReg=resUntagged, operand1=arg1Untagged, operand2=RegisterArgument arg2Untagged, ccRef=newCCRef(), opSize=polyWordOpSize}),
                     BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=resUntagged, operand2=IntegerConstant 1, ccRef=newCCRef(), opSize=polyWordOpSize})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.WordArith BuiltIns.ArithDiv, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
                val code = arg1Code @ arg2Code @
                    (* Shift both of the arguments to remove the tags.  We don't test for zero here - that's done explicitly. *)
                    [BlockSimple(UntagValue{source=aReg1, dest=arg1Untagged, isSigned=false, cache=NONE, opSize=polyWordOpSize}),
                     BlockSimple(UntagValue{source=aReg2, dest=arg2Untagged, isSigned=false, cache=NONE, opSize=polyWordOpSize}),
                     BlockSimple(Division { isSigned = false, dividend=arg1Untagged, divisor=RegisterArgument arg2Untagged,
                                quotient=quotient, remainder=remainder, opSize=polyWordOpSize }),
                     BlockSimple(TagValue { source=quotient, dest=target, isSigned=false, opSize=polyWordOpSize })]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.WordArith BuiltIns.ArithMod, arg1, arg2}, context, _, destination, tailCode) =
            let
                (* Identical to Quot except that the result is the remainder. *)
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
                val code = arg1Code @ arg2Code @
                    (* Shift both of the arguments to remove the tags. *)
                    [BlockSimple(UntagValue{source=aReg1, dest=arg1Untagged, isSigned=false, cache=NONE, opSize=polyWordOpSize}),
                     BlockSimple(UntagValue{source=aReg2, dest=arg2Untagged, isSigned=false, cache=NONE, opSize=polyWordOpSize}),
                     BlockSimple(Division { isSigned = false, dividend=arg1Untagged, divisor=RegisterArgument arg2Untagged,
                                quotient=quotient, remainder=remainder, opSize=polyWordOpSize }),
                     BlockSimple(TagValue { source=remainder, dest=target, isSigned=false, opSize=polyWordOpSize })]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.WordArith _, ...}, _, _, _, _) =
                raise InternalError "codeToICodeNonRev: WordArith - unimplemented operation"

        |   codeToICodeBinaryRev({oper=BuiltIns.WordLogical logOp, arg1, arg2=BICConstnt(value, _)}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, arg1Reg) = codeToPReg(arg1, context)
                (* Use a semitagged value for XOR.  This preserves the tag bit.  Use toLargeIntX here because
                   the operations will sign-extend 32-bit values. *)
                val constVal =
                    if isShort value
                    then (case logOp of BuiltIns.LogicalXor => semitag | _ => tag) (Word.toLargeIntX(toShort value))
                    else 0
                val oper = case logOp of BuiltIns.LogicalOr => OR | BuiltIns.LogicalAnd => AND | BuiltIns.LogicalXor => XOR
                (* If we AND with a value that fits in 32-bits we can use a 32-bit operation. *)
                val opSize =
                    if logOp = BuiltIns.LogicalAnd andalso constVal <= 0xffffffff andalso constVal >= 0
                    then OpSize32 else polyWordOpSize
                val code =
                    arg1Code @
                    [BlockSimple(ArithmeticFunction{oper=oper, resultReg=target, operand1=arg1Reg, operand2=IntegerConstant constVal,
                                 ccRef=newCCRef(), opSize=opSize})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.WordLogical logOp, arg1=BICConstnt(value, _), arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg2Code, arg2Reg) = codeToPReg(arg2, context)
                (* Use a semitagged value for XOR.  This preserves the tag bit. *)
                val constVal =
                    if isShort value
                    then (case logOp of BuiltIns.LogicalXor => semitag | _ => tag) (Word.toLargeIntX(toShort value))
                    else 0
                val oper = case logOp of BuiltIns.LogicalOr => OR | BuiltIns.LogicalAnd => AND | BuiltIns.LogicalXor => XOR
                (* If we AND with a value that fits in 32-bits we can use a 32-bit operation. *)
                val opSize =
                    if logOp = BuiltIns.LogicalAnd andalso constVal <= 0xffffffff andalso constVal >= 0
                    then OpSize32 else polyWordOpSize
                val code =
                    arg2Code @
                    [BlockSimple(ArithmeticFunction{oper=oper, resultReg=target, operand1=arg2Reg, operand2=IntegerConstant constVal,
                                 ccRef=newCCRef(), opSize=opSize})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.WordLogical BuiltIns.LogicalOr, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, arg1Reg) = codeToPReg(arg1, context)
                val (arg2Code, arg2Reg) = codeToPReg(arg2, context)
                val code =
                    arg1Code @ arg2Code @
                    (* Or-ing preserves the tag bit. *)
                    [BlockSimple(ArithmeticFunction{oper=OR, resultReg=target, operand1=arg1Reg, operand2=RegisterArgument arg2Reg, ccRef=newCCRef(), opSize=polyWordOpSize})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.WordLogical BuiltIns.LogicalAnd, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, arg1Reg) = codeToPReg(arg1, context)
                val (arg2Code, arg2Reg) = codeToPReg(arg2, context)
                val code =
                    arg1Code @ arg2Code @
                    (* Since they're both tagged the result will be tagged. *)
                    [BlockSimple(ArithmeticFunction{oper=AND, resultReg=target, operand1=arg1Reg, operand2=RegisterArgument arg2Reg, ccRef=newCCRef(), opSize=polyWordOpSize})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.WordLogical BuiltIns.LogicalXor, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, arg1Reg) = codeToPReg(arg1, context)
                val (arg2Code, arg2Reg) = codeToPReg(arg2, context)
                val aReg3 = newPReg()
                val code = arg1Code @ arg2Code @
                    (* We need to restore the tag bit after the operation. *)
                    [BlockSimple(ArithmeticFunction{oper=XOR, resultReg=aReg3, operand1=arg1Reg, operand2=RegisterArgument arg2Reg, ccRef=newCCRef(), opSize=polyWordOpSize}),
                     BlockSimple(ArithmeticFunction{oper=OR, resultReg=target, operand1=aReg3, operand2=IntegerConstant 1, ccRef=newCCRef(), opSize=polyWordOpSize})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.WordShift BuiltIns.ShiftLeft, arg1, arg2=BICConstnt(value, _)}, context, _, destination, tailCode) =
                (* Use the general case multiplication code.  This will use a shift except for small values.
                   It does detect special cases such as multiplication by 4 and 8 which can be implemented with LEA. *)
                codeMultiplyConstantWordRev(arg1, context, destination, if isShort value then Word.<<(0w1, toShort value) else 0w1, tailCode)

        |   codeToICodeBinaryRev({oper=BuiltIns.WordShift shift, arg1, arg2}, context, _, destination, tailCode) =
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
                            [BlockSimple(LoadArgument{source=IntegerConstant cnstntVal, dest=argRegUntagged, kind=movePolyWord})]
                        end
                    |   _ =>
                        let
                            val (arg1Code, arg1Reg) = codeToPReg(arg1, context)
                            val removeTag =
                                case shift of
                                    ShiftLeft =>
                                        ArithmeticFunction{oper=SUB, resultReg=argRegUntagged, operand1=arg1Reg,
                                                        operand2=IntegerConstant 1, ccRef=newCCRef(), opSize=polyWordOpSize}
                                |   _ => LoadArgument{source=RegisterArgument arg1Reg, dest=argRegUntagged, kind=movePolyWord}
                        in
                            arg1Code @ [BlockSimple removeTag]
                        end

                (* The shift amount can usefully be a constant. *)
                val (arg2Code, untag2Code, arg2Arg) = codeAsUntaggedByte(arg2, false, context)
                val resRegUntagged = newUReg()
                val shiftOp = case shift of ShiftLeft => SHL | ShiftRightLogical => SHR | ShiftRightArithmetic => SAR
                val code = arg1Code @ arg2Code @ untag2Code @
                 [BlockSimple(ShiftOperation{ shift=shiftOp, resultReg=resRegUntagged, operand=argRegUntagged, shiftAmount=arg2Arg, ccRef=newCCRef(), opSize=polyWordOpSize }),
                  (* Set the tag by ORing it in.  This will work whether or not a right shift has shifted a 1 into this position. *)
                  BlockSimple(
                    ArithmeticFunction{oper=OR, resultReg=target, operand1=resRegUntagged,
                                       operand2=IntegerConstant 1, ccRef=newCCRef(), opSize=polyWordOpSize})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.AllocateByteMemory, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val sizeReg = newPReg() and baseReg = newPReg()
                val sizeCode = codeToICodeTarget(arg1, context, false, sizeReg)
                val (flagsCode, flagUntag, flagArg) = codeAsUntaggedByte(arg2, false, context)
                val code =sizeCode @ flagsCode @
                 [BlockSimple(AllocateMemoryVariable{size=sizeReg, dest=baseReg, saveRegs=[]})] @
                  flagUntag @
                  [BlockSimple(StoreArgument{ source=flagArg, base=baseReg, offset= ~1, index=memIndexOrObject, kind=MoveByte, isMutable=false}),
                  BlockSimple InitialisationComplete,
                  BlockSimple(LoadArgument{ source=RegisterArgument baseReg, dest=target, kind=movePolyWord})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.LargeWordComparison test, arg1, arg2}, context, _, destination, tailCode) =
            let
                val ccRef = newCCRef()
                val (arg1Code, arg1Reg) = codeToPRegRev(arg1, context, tailCode)
                (* In X64 we can extract the word from a constant and do the comparison
                   directly.  That can't be done in X86/32 because the value isn't tagged
                   and might look like an address.  The RTS scans for comparisons with
                   inline constant addresses. *)
                val (arg2Code, arg2Operand) =
                    if targetArch <> Native32Bit
                    then (* Native 64-bit or 32-in-64. *)
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
                val target = asTarget destination
                val code =
                    makeBoolResultRev(testAsBranch(test, false, true), ccRef, target,
                        BlockSimple(WordComparison{arg1=argReg, arg2=arg2Operand, ccRef=ccRef, opSize=nativeWordOpSize}) ::
                        BlockSimple(LoadArgument{source=wordAt arg1Reg, dest=argReg, kind=moveNativeWord}) :: arg2Code)
            in
                (code, RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.LargeWordArith BuiltIns.ArithAdd, arg1, arg2=BICConstnt(value, _)}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val aReg3 = newUReg()
                val argReg = newUReg()
                val constantValue = largeWordConstant value
                val code =arg1Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=moveNativeWord}),
                     BlockSimple(ArithmeticFunction{oper=ADD, resultReg=aReg3, operand1=argReg, operand2=IntegerConstant constantValue, ccRef=newCCRef(), opSize=nativeWordOpSize}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.LargeWordArith BuiltIns.ArithAdd, arg1=BICConstnt(value, _), arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val aReg3 = newUReg()
                val argReg = newUReg()
                val constantValue = largeWordConstant value
                val code = arg2Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg2, dest=argReg, kind=moveNativeWord}),
                     BlockSimple(ArithmeticFunction{oper=ADD, resultReg=aReg3, operand1=argReg, operand2=IntegerConstant constantValue, ccRef=newCCRef(), opSize=nativeWordOpSize}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.LargeWordArith BuiltIns.ArithAdd, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val aReg3 = newUReg()
                val argReg = newUReg()
                val code = arg1Code @ arg2Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=moveNativeWord}),
                     BlockSimple(ArithmeticFunction{oper=ADD, resultReg=aReg3, operand1=argReg, operand2=wordAt aReg2, ccRef=newCCRef(), opSize=nativeWordOpSize}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.LargeWordArith BuiltIns.ArithSub, arg1, arg2=BICConstnt(value, _)}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val aReg3 = newUReg()
                val argReg = newUReg()
                val constantValue = largeWordConstant value
                val code = arg1Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=moveNativeWord}),
                     BlockSimple(ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=argReg, operand2=IntegerConstant constantValue, ccRef=newCCRef(), opSize=nativeWordOpSize}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.LargeWordArith BuiltIns.ArithSub, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val aReg3 = newUReg()
                val argReg = newUReg()
                val code = arg1Code @ arg2Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=moveNativeWord}),
                     BlockSimple(ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=argReg, operand2=wordAt aReg2, ccRef=newCCRef(), opSize=nativeWordOpSize}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.LargeWordArith BuiltIns.ArithMult, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val resValue = newUReg()
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val argReg1 = newUReg()
                val code = arg1Code @ arg2Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg1, kind=moveNativeWord}),
                     BlockSimple(Multiplication{resultReg=resValue, operand1=argReg1, operand2=wordAt aReg2, ccRef=newCCRef(), opSize=nativeWordOpSize}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=resValue, dest=target, saveRegs=[]})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.LargeWordArith BuiltIns.ArithDiv, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val quotient = newUReg() and remainder = newUReg()
                val dividendReg = newUReg() and divisorReg = newUReg()
                val code = arg1Code @ arg2Code @
                    (* We don't test for zero here - that's done explicitly. *)
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=dividendReg, kind=moveNativeWord}),
                     BlockSimple(LoadArgument{source=wordAt aReg2, dest=divisorReg, kind=moveNativeWord}),
                     BlockSimple(Division { isSigned = false, dividend=dividendReg, divisor=RegisterArgument divisorReg,
                                quotient=quotient, remainder=remainder, opSize=nativeWordOpSize }),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=quotient, dest=target, saveRegs=[]})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.LargeWordArith BuiltIns.ArithMod, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val quotient = newUReg() and remainder = newUReg()
                val dividendReg = newUReg() and divisorReg = newUReg()
                val code = arg1Code @ arg2Code @
                    (* We don't test for zero here - that's done explicitly. *)
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=dividendReg, kind=moveNativeWord}),
                     BlockSimple(LoadArgument{source=wordAt aReg2, dest=divisorReg, kind=moveNativeWord}),
                     BlockSimple(Division { isSigned = false, dividend=dividendReg, divisor=RegisterArgument divisorReg,
                                quotient=quotient, remainder=remainder, opSize=nativeWordOpSize }),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=remainder, dest=target, saveRegs=[]})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.LargeWordArith _, ...}, _, _, _, _) =
                raise InternalError "codeToICodeNonRev: LargeWordArith - unimplemented operation"

        |   codeToICodeBinaryRev({oper=BuiltIns.LargeWordLogical logOp, arg1, arg2=BICConstnt(value, _)}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val aReg3 = newUReg()
                val argReg = newUReg()
                val constantValue = largeWordConstant value
                val oper = case logOp of BuiltIns.LogicalOr => OR | BuiltIns.LogicalAnd => AND | BuiltIns.LogicalXor => XOR
                (* If we AND with a value that fits in 32-bits we can use a 32-bit operation. *)
                val opSize =
                    if logOp = BuiltIns.LogicalAnd andalso constantValue <= 0xffffffff andalso constantValue >= 0
                    then OpSize32 else nativeWordOpSize
                val code = arg1Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=moveNativeWord}),
                     BlockSimple(ArithmeticFunction{oper=oper, resultReg=aReg3, operand1=argReg, operand2=IntegerConstant constantValue,
                                                    ccRef=newCCRef(), opSize=opSize}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.LargeWordLogical logOp, arg1=BICConstnt(value, _), arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val aReg3 = newUReg()
                val argReg = newUReg()
                val constantValue = largeWordConstant value
                val oper = case logOp of BuiltIns.LogicalOr => OR | BuiltIns.LogicalAnd => AND | BuiltIns.LogicalXor => XOR
                (* If we AND with a value that fits in 32-bits we can use a 32-bit operation. *)
                val opSize =
                    if logOp = BuiltIns.LogicalAnd andalso constantValue <= 0xffffffff andalso constantValue >= 0
                    then OpSize32 else nativeWordOpSize
                val code = arg2Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg2, dest=argReg, kind=moveNativeWord}),
                     BlockSimple(ArithmeticFunction{oper=oper, resultReg=aReg3, operand1=argReg, operand2=IntegerConstant constantValue,
                                                    ccRef=newCCRef(), opSize=opSize}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.LargeWordLogical logOp, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
                val aReg3 = newUReg()
                val argReg = newUReg()
                val oper = case logOp of BuiltIns.LogicalOr => OR | BuiltIns.LogicalAnd => AND | BuiltIns.LogicalXor => XOR
                val code = arg1Code @ arg2Code @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=moveNativeWord}),
                     BlockSimple(ArithmeticFunction{oper=oper, resultReg=aReg3, operand1=argReg, operand2=wordAt aReg2, ccRef=newCCRef(), opSize=nativeWordOpSize}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.LargeWordShift shift, arg1, arg2}, context, _, destination, tailCode) =
                (* The shift is always a Word.word value i.e. tagged.  There is a check at the higher level
                   that the shift does not exceed 32/64 bits. *)
            let
                open BuiltIns
                val target = asTarget destination
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, untag2Code, arg2Arg) = codeAsUntaggedByte(arg2, false, context)
                val aReg3 = newUReg()
                val shiftOp = case shift of ShiftLeft => SHL | ShiftRightLogical => SHR | ShiftRightArithmetic => SAR
                val argReg = newUReg()
                val code = arg1Code @ arg2Code @ [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=moveNativeWord})] @ untag2Code @
                 [BlockSimple(ShiftOperation{ shift=shiftOp, resultReg=aReg3, operand=argReg, shiftAmount=arg2Arg, ccRef=newCCRef(), opSize=nativeWordOpSize }),
                  BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})]
            in
                (revApp(code, tailCode), RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.RealArith(fpOpPrec as (fpOp, fpPrec)), arg1, arg2}, context, _, destination, tailCode) =
            let
                open BuiltIns
                val commutative =
                    case fpOp of
                        ArithSub => NonCommutative
                    |   ArithDiv => NonCommutative
                    |   ArithAdd => Commutative
                    |   ArithMult => Commutative
                    |   _ => raise InternalError "codeToICodeNonRev: RealArith - unimplemented operation"

                val (argCodeRev, fpRegSrc, arg2Value) = codeFPBinaryArgsRev(arg1, arg2, fpPrec, commutative, context, [])
                val argCode = List.rev argCodeRev
                
                val target = asTarget destination
                val fpRegDest = newUReg()
                val arith =
                    case fpMode of
                        FPModeX87 =>
                        let
                            val fpOp =
                                case fpOp of
                                    ArithAdd => FADD
                                |   ArithSub => FSUB
                                |   ArithMult => FMUL
                                |   ArithDiv => FDIV
                                |   _ => raise InternalError "codeToICodeNonRev: RealArith - unimplemented operation"
                            val isDouble = case fpPrec of PrecSingle => false |  PrecDouble => true
                        in
                            [BlockSimple(X87FPArith{ opc=fpOp, resultReg=fpRegDest, arg1=fpRegSrc, arg2=arg2Value, isDouble=isDouble})]
                        end
                    |   FPModeSSE2 =>
                        let
                            val fpOp =
                                case fpOpPrec of
                                    (ArithAdd, PrecSingle) => SSE2BAddSingle
                                |   (ArithSub, PrecSingle) => SSE2BSubSingle
                                |   (ArithMult, PrecSingle) => SSE2BMulSingle
                                |   (ArithDiv, PrecSingle) => SSE2BDivSingle
                                |   (ArithAdd, PrecDouble) => SSE2BAddDouble
                                |   (ArithSub, PrecDouble) => SSE2BSubDouble
                                |   (ArithMult, PrecDouble) => SSE2BMulDouble
                                |   (ArithDiv, PrecDouble) => SSE2BDivDouble
                                |   _ => raise InternalError "codeToICodeNonRev: RealArith - unimplemented operation"
                        in
                            [BlockSimple(SSE2FPBinary{ opc=fpOp, resultReg=fpRegDest, arg1=fpRegSrc, arg2=arg2Value})]
                        end
                (* Box or tag the result. *)
                val result = boxOrTagReal(fpRegDest, target, fpPrec)
            in
                (revApp(argCode @ arith @ result, tailCode), RegisterArgument target, false)
            end

           (* Floating point comparison.  This is complicated because we have different
              instruction sequences for SSE2 and X87.  We also have to get the handling
              of unordered (NaN) values right.  All the tests are treated as false
              if either argument is a NaN.  To combine that test with the other tests
              we sometimes have to reverse the comparison. *)
        |   codeToICodeBinaryRev({oper=BuiltIns.RealComparison(BuiltIns.TestEqual, precision), arg1, arg2}, context, _, destination, tailCode) =
            let
                (* Get the arguments.  It's commutative. *)
                val (arg2Code, fpReg, arg2Val) = codeFPBinaryArgsRev(arg1, arg2, precision, Commutative, context, tailCode)
                val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                val testReg1 = newUReg() and testReg2 = newUReg() and testReg3 = newUReg()
                (* If this is X87 we get the condition into RAX and test it there.  If
                   it is SSE2 we have to treat the unordered result (parity set) specially. *)
                val isDouble = precision = BuiltIns.PrecDouble
                val target = asTarget destination
                
                val code =
                case fpMode of
                    FPModeX87 =>
                        makeBoolResultRev(JE, ccRef2, target,
                            BlockSimple(ArithmeticFunction{
                                oper=XOR, resultReg=testReg3, operand1=testReg2, operand2=IntegerConstant 0x4000, ccRef=ccRef2, opSize=OpSize32 }) ::
                            BlockSimple(ArithmeticFunction{
                                oper=AND, resultReg=testReg2, operand1=testReg1, operand2=IntegerConstant 0x4400, ccRef=newCCRef(), opSize=OpSize32 }) ::
                            BlockSimple(X87FPGetCondition { ccRef=ccRef1, dest=testReg1 }) ::
                            BlockSimple(X87Compare{arg1=fpReg, arg2=arg2Val, ccRef=ccRef1, isDouble = isDouble}) ::
                            arg2Code)
                |   FPModeSSE2 =>
                    let
                        val noParityLabel = newLabel()
                        
                        val resultLabel = newLabel()
                        val falseLabel = newLabel()
                        val trueLabel = newLabel()
                        val mergeReg = newMergeReg()
                    in
                        BlockSimple(LoadArgument{ source=RegisterArgument mergeReg, dest=target, kind=Move32Bit }) ::
                        BlockLabel resultLabel ::
                        BlockFlow(Unconditional resultLabel) ::
                        (* Result is false if parity is set i.e. unordered or if unequal. *)
                        BlockSimple(LoadArgument{ source=IntegerConstant(tag 0), dest=mergeReg, kind=Move32Bit }) ::
                        BlockLabel falseLabel ::
                        BlockFlow(Unconditional resultLabel) ::
                        (* Result is true if it's ordered and equal. *)
                        BlockSimple(LoadArgument{ source=IntegerConstant(tag 1), dest=mergeReg, kind=Move32Bit }) ::
                        BlockLabel trueLabel ::
                        (* Not unordered - test the equality *)
                        BlockFlow(Conditional{ccRef=ccRef1, condition=JE, trueJump=trueLabel, falseJump=falseLabel}) ::
                        BlockLabel noParityLabel ::
                        (* Go to falseLabel if unordered and therefore not equal. *)
                        BlockFlow(Conditional{ccRef=ccRef1, condition=JP, trueJump=falseLabel, falseJump=noParityLabel}) ::
                        BlockSimple(SSE2Compare{arg1=fpReg, arg2=arg2Val, ccRef=ccRef1, isDouble = isDouble}) ::
                        arg2Code
                    end
            in
                (code, RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.RealComparison(BuiltIns.TestUnordered, precision), arg1, arg2}, context, _, destination, tailCode) =
            let
                (* The unordered test is really included because it is easy to implement and is the
                   simplest way of implementing isNan.  *)
                (* Get the arguments.  It's commutative. *)
                val (arg2Code, fpReg, arg2Val) = codeFPBinaryArgsRev(arg1, arg2, precision, Commutative, context, tailCode)
                val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                val testReg1 = newUReg() and testReg2 = newUReg() and testReg3 = newUReg()

               (* If this is X87 we get the condition into RAX and test it there.  If
                   it is SSE2 we have to treat the unordered result (parity set) specially. *)
                val isDouble = precision = BuiltIns.PrecDouble
                val target = asTarget destination
                val code =
                    case fpMode of
                        FPModeX87 =>
                            (* And with 0x4500.  We have to use XOR rather than CMP to avoid having an untagged constant comparison. *)
                            makeBoolResultRev(JE, ccRef2, target,
                                BlockSimple(ArithmeticFunction{
                                    oper=XOR, resultReg=testReg3, operand1=testReg2, operand2=IntegerConstant 0x4500, ccRef=ccRef2, opSize=OpSize32 }) ::
                                BlockSimple(ArithmeticFunction{
                                    oper=AND, resultReg=testReg2, operand1=testReg1, operand2=IntegerConstant 0x4500, ccRef=newCCRef(), opSize=OpSize32 }) ::
                                BlockSimple(X87FPGetCondition { ccRef=ccRef1, dest=testReg1 }) ::
                                BlockSimple(X87Compare{arg1=fpReg, arg2=arg2Val, ccRef=ccRef1, isDouble = isDouble}) ::
                                arg2Code)
                    |   FPModeSSE2 =>
                            makeBoolResultRev(JP, ccRef1, target,
                                BlockSimple(SSE2Compare{arg1=fpReg, arg2=arg2Val, ccRef=ccRef1, isDouble = isDouble}) ::
                                arg2Code)
            in
                (code, RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.RealComparison(comparison, precision), arg1, arg2}, context, _, destination, tailCode) =
            let
                (* Ordered comparisons are complicated because they are all defined to be false
                   if either argument is a NaN.  We have two different tests for a > b and a >= b
                   and implement a < b and a <= b by changing the order of the arguments. *)
                val (arg1Code, arg1Value) = codeFPArgument(arg1, precision, context, tailCode)
                val (arg2Code, arg2Value) = codeFPArgument(arg2, precision, context, arg1Code)
                
                val (regArg, opArg, isGeq) =
                    case comparison of
                        BuiltIns.TestGreater      => (arg1Value, arg2Value, false)
                    |   BuiltIns.TestLess         => (arg2Value, arg1Value, false) (* Reversed: a<b is b>a. *)
                    |   BuiltIns.TestGreaterEqual => (arg1Value, arg2Value, true)
                    |   BuiltIns.TestLessEqual    => (arg2Value, arg1Value, true)  (* Reversed: a<=b is b>=a. *)
                    |   _ => raise InternalError "RealComparison: unimplemented operation"
                
                (* Load the first operand into a register. *)
                val (fpReg, loadCode) =
                    case regArg of
                        RegisterArgument fpReg => (fpReg, arg2Code)
                    |   regArg =>
                        let
                            val fpReg = newUReg()
                            val moveOp =
                                case precision of
                                    BuiltIns.PrecDouble => MoveDouble | BuiltIns.PrecSingle => MoveFloat
                        in
                            (fpReg, BlockSimple(LoadArgument{source=regArg, dest=fpReg, kind=moveOp}) :: arg2Code)
                        end
                
                val isDouble = precision = BuiltIns.PrecDouble
                val target = asTarget destination

                val code =
                case fpMode of
                    FPModeX87 =>
                    let
                        val testReg1 = newUReg() and testReg2 = newUReg()
                        val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                        val testBits = if isGeq then 0x500 else 0x4500
                    in
                        makeBoolResultRev(JE, ccRef2, target,
                            BlockSimple(ArithmeticFunction{
                                oper=AND, resultReg=testReg2, operand1=testReg1, operand2=IntegerConstant testBits, ccRef=ccRef2, opSize=OpSize32 }) ::
                            BlockSimple(X87FPGetCondition { ccRef=ccRef1, dest=testReg1 }) ::
                            BlockSimple(X87Compare{arg1=fpReg, arg2=opArg, ccRef=ccRef1, isDouble = isDouble}) ::
                            loadCode)
                    end
                        
                |   FPModeSSE2 =>
                    let
                        val ccRef1 = newCCRef()
                        val condition = if isGeq then JNB (* >=, <= *) else  JA  (* >, < *)
                    in
                        makeBoolResultRev(condition, ccRef1, target,
                            BlockSimple(SSE2Compare{arg1=fpReg, arg2=opArg, ccRef=ccRef1, isDouble = isDouble}) :: loadCode)
                    end
            in
                (code, RegisterArgument target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.FreeCStack, arg1, arg2}, context, _, destination, tailCode) =
            (* Free space on the C stack by storing the address in the argument into the "memory register".
               This is a binary operation that takes the base address and the size.  The base address isn't
               used in this version.  *)
            let
                val (arg2Code, untaggedLength) =
                    case arg2 of
                        BICConstnt(value, _) => (tailCode, IntegerConstant(Word.toLargeInt(toShort value)) (* Leave untagged *))
                    |   _ =>
                        let
                            val (arg2Code, lengthReg) = codeToPRegRev(arg2, context, tailCode)
                            val lengthUntagged = newUReg()
                        in
                            (
                                BlockSimple(UntagValue{source=lengthReg, dest=lengthUntagged, isSigned=false, cache=NONE, opSize=polyWordOpSize}) :: arg2Code,
                                RegisterArgument lengthUntagged
                            )
                        end

                (* Evaluate the first argument for side-effects but discard it. *)
                val (arg1Code, _, _) = codeToICodeRev(arg1, context, false, Allowed allowDefer, arg2Code)
                val addrReg = newUReg() and resAddrReg = newUReg()
                val code =
                    BlockSimple(StoreMemReg{offset=memRegCStackPtr, source=resAddrReg, kind=moveNativeWord}) ::
                    BlockSimple(ArithmeticFunction{oper=ADD, resultReg=resAddrReg, operand1=addrReg, operand2=untaggedLength,
                                    ccRef=newCCRef(), opSize=nativeWordOpSize}) ::
                    BlockSimple(LoadMemReg{offset=memRegCStackPtr, dest=addrReg, kind=moveNativeWord}) ::
                    arg1Code
            in
                moveIfNotAllowedRev(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.GeneralEquality, ...}, _, _, _, _) =
                (* This should have been removed at a higher level. *)
                raise InternalError "codeToICodeBinaryRev: GeneralEquality"

        (* Multiply tagged word by a constant.  We're not concerned with overflow so it's possible to use
           various short cuts. *)
        and codeMultiplyConstantWordRev(arg, context, destination, multiplier, tailCode) =
        let
            val target = asTarget destination
            val (argCode, aReg) = codeToPReg(arg, context)
            
            val doMultiply =
                case multiplier of
                    0w0 => [BlockSimple(LoadArgument{source=IntegerConstant 1, dest=target, kind=movePolyWord})]
                |   0w1 => [BlockSimple(LoadArgument{source=RegisterArgument aReg, dest=target, kind=movePolyWord})]
                |   0w2 => [BlockSimple(LoadEffectiveAddress{base=SOME aReg, offset= ~1, index=MemIndex1 aReg, dest=target, opSize=polyWordOpSize})]
                |   0w3 => [BlockSimple(LoadEffectiveAddress{base=SOME aReg, offset= ~2, index=MemIndex2 aReg, dest=target, opSize=polyWordOpSize})]
                |   0w4 => [BlockSimple(LoadEffectiveAddress{base=NONE, offset= ~3, index=MemIndex4 aReg, dest=target, opSize=polyWordOpSize})]
                |   0w5 => [BlockSimple(LoadEffectiveAddress{base=SOME aReg, offset= ~4, index=MemIndex4 aReg, dest=target, opSize=polyWordOpSize})]
                |   0w8 => [BlockSimple(LoadEffectiveAddress{base=NONE, offset= ~7, index=MemIndex8 aReg, dest=target, opSize=polyWordOpSize})]
                |   0w9 => [BlockSimple(LoadEffectiveAddress{base=SOME aReg, offset= ~8, index=MemIndex8 aReg, dest=target, opSize=polyWordOpSize})]
                
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
                                        shiftAmount=IntegerConstant(Word.toLargeInt power), ccRef=newCCRef(), opSize=polyWordOpSize })
                            |   NONE => (* Multiply including the tag. *)
                                    BlockSimple(Multiplication{resultReg=tReg, operand1=aReg,
                                        operand2=IntegerConstant(Word.toLargeInt multiplier), ccRef=newCCRef(), opSize=polyWordOpSize})
                    in
                        [multiply,
                            BlockSimple(ArithmeticFunction{oper=SUB, resultReg=target, operand1=tReg,
                                operand2=IntegerConstant tagCorrection, ccRef=newCCRef(), opSize=polyWordOpSize})]
                    end
        in
            
            (revApp(argCode @ doMultiply, tailCode), RegisterArgument target, false)
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
                    BlockSimple(StoreArgument{ source=RegisterArgument valueReg, offset=n*Word.toInt wordSize, base=memAddr, index=memIndexOrObject, kind=movePolyWord, isMutable=false})
                val code =
                    codeToICodeTarget(initial, context, false, valueReg) @
                    [BlockSimple(AllocateMemoryOperation{size=vecLength, flags=flagByte, dest=memAddr, saveRegs=[]})] @
                    List.tabulate(vecLength, initialise) @
                    [BlockSimple InitialisationComplete,
                     BlockSimple(LoadArgument{source=RegisterArgument memAddr, dest=target, kind=movePolyWord})]
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
                val (codeBaseIndex, codeUntag, memLoc) = codeAddress(address, wordSize, context)
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument {source=MemoryLocation memLoc, dest=target, kind=movePolyWord})], RegisterArgument target, false)
            end

        |   codeToICodeLoad({kind=LoadStoreMLByte _, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeAddress(address, 0w1, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveByte}),
                     BlockSimple(TagValue {source=untaggedResReg, dest=target, isSigned=false, opSize=OpSize32})], RegisterArgument target, false)
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
                     BlockSimple(TagValue {source=untaggedResReg, dest=target, isSigned=false, opSize=OpSize32})], RegisterArgument target, false)
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
                     BlockSimple(TagValue {source=untaggedResReg, dest=target, isSigned=false, opSize=OpSize32})], RegisterArgument target, false)
            end

        |   codeToICodeLoad({kind=LoadStoreC32, address}, context, _, destination) =
            let
                (* Load a 32-bit value from C memory.  If this is 64-bit mode we can tag it but
                   if this is 32-bit mode we need to box it. *)
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w4, context)
                val untaggedResReg = newUReg()
                val boxTagCode =
                    if targetArch = Native64Bit
                    then BlockSimple(TagValue {source=untaggedResReg, dest=target, isSigned=false, opSize=OpSize64 (* It becomes 33 bits *)})
                    else BlockSimple(BoxValue{boxKind=BoxLargeWord, source=untaggedResReg, dest=target, saveRegs=[]})
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=Move32Bit}), boxTagCode], RegisterArgument target, false)
            end

        |   codeToICodeLoad({kind=LoadStoreC64, address}, context, _, destination) =
            let
                (* Load a 64-bit value from C memory.  This is only allowed in 64-bit mode.  The result
                   is a boxed value. *)
                val _ = targetArch <> Native32Bit orelse raise InternalError "codeToICodeNonRev: BICLoadOperation LoadStoreC64 in 32-bit"
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w8, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=Move64Bit}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=untaggedResReg, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeLoad({kind=LoadStoreCFloat, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w4, context)
                val untaggedResReg = newUReg()
                val boxFloat = case fpMode of FPModeX87 => BoxX87Double | FPModeSSE2 => BoxSSE2Double
                (* We need to convert the float into a double. *)
                val loadArg =
                    case fpMode of
                        FPModeX87 => BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveFloat})
                    |   FPModeSSE2 => BlockSimple(SSE2FPUnary { source=MemoryLocation memLoc, resultReg=untaggedResReg, opc=SSE2UFloatToDouble})
            in
                (codeBaseIndex @ codeUntag @
                    [loadArg,
                     BlockSimple(BoxValue{boxKind=boxFloat, source=untaggedResReg, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeLoad({kind=LoadStoreCDouble, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w8, context)
                val untaggedResReg = newUReg()
                val boxFloat = case fpMode of FPModeX87 => BoxX87Double | FPModeSSE2 => BoxSSE2Double
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveDouble}),
                     BlockSimple(BoxValue{boxKind=boxFloat, source=untaggedResReg, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeLoad({kind=LoadStoreUntaggedUnsigned, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeAddress(address, wordSize, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=movePolyWord}),
                     BlockSimple(TagValue {source=untaggedResReg, dest=target, isSigned=false, opSize=polyWordOpSize})], RegisterArgument target, false)
            end

        |   codeToICodeLoad({kind=LoadStorePolyWord _, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeAddress(address, wordSize, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=movePolyWord}),
                     BlockSimple(BoxValue {boxKind=BoxLargeWord, source=untaggedResReg, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICodeLoad({kind=LoadStoreNativeWord _, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeAddress(address, nativeWordSize, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=moveNativeWord}),
                     BlockSimple(BoxValue {boxKind=BoxLargeWord, source=untaggedResReg, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        and codeToICodeStore({kind=LoadStoreMLWord _, address, value}, context, _, destination) =
            let
                val (sourceCode, source, _) = codeToICode(value, context, false, Allowed allowInMemMove)
                val (codeBaseIndex, codeUntag, {base, offset, index, ...}) = codeAddress(address, wordSize, context)
                val code =
                    codeBaseIndex @ sourceCode @ codeUntag @
                        [BlockSimple(StoreArgument {source=source, base=base, offset=offset, index=index, kind=movePolyWord, isMutable=true})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeStore({kind=LoadStoreMLByte _, address, value}, context, _, destination) =
            let
                val (codeBaseIndex, codeUntag, {base, offset, index, ...}) = codeAddress(address, 0w1, context)
                (* We have to untag the value to store. *)
                val (valueCode, untagValue, valueArg) = codeAsUntaggedByte(value, false, context)
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
                val (valueCode, untagValue, valueArg) = codeAsUntaggedByte(value, false, context)
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
                    if targetArch = Native64Bit
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
                        codeBaseIndex @ valueCode @ BlockSimple(LoadArgument{source=wordAt valueReg, dest=valueReg1, kind=Move32Bit}) :: codeUntag @
                        [BlockSimple(StoreArgument {source=RegisterArgument valueReg1, base=base, offset=offset, index=index, kind=Move32Bit, isMutable=true})]
                    end
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeStore({kind=LoadStoreC64, address, value}, context, _, destination) =
            let
                (* Store a 64-bit value. *)
                val _ = targetArch <> Native32Bit orelse raise InternalError "codeToICodeNonRev: BICStoreOperation LoadStoreC64 in 32-bit"
                val (valueCode, valueReg) = codeToPReg(value, context)
                val valueReg1 = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index, ...}) = codeCAddress(address, 0w8, context)
                val code =
                    codeBaseIndex @ valueCode @ codeUntag @
                    [BlockSimple(LoadArgument{source=wordAt valueReg, dest=valueReg1, kind=Move64Bit}),
                     BlockSimple(StoreArgument {source=RegisterArgument valueReg1, base=base, offset=offset, index=index, kind=Move64Bit, isMutable=true})]
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
                                [BlockSimple(SSE2FPUnary{opc=SSE2UDoubleToFloat, resultReg=float2Reg, source=RegisterArgument floatReg})])
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
                val (codeBaseIndex, codeUntag, {base, offset, index, ...}) = codeAddress(address, wordSize, context)
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
                                if targetArch = Native64Bit
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
                                [BlockSimple(UntagValue{dest=valueReg1, source=valueReg, isSigned=false, cache=NONE, opSize=polyWordOpSize})])
                        end
                val code =
                    codeBaseIndex @ valCode @ codeUntag @
                    [BlockSimple(StoreArgument {source=storeVal, base=base, offset=offset, index=index, kind=movePolyWord, isMutable=true})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeStore({kind=LoadStorePolyWord _, address, value}, context, _, destination) =
            let
                (* Store a 32-bit value from a large-word. *)
                val (valueCode, valueReg) = codeToPReg(value, context)
                val valueReg1 = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index, ...}) = codeAddress(address, wordSize, context)
                val code =
                    codeBaseIndex @ valueCode @ codeUntag @
                    [BlockSimple(LoadArgument{source=wordAt valueReg, dest=valueReg1, kind=moveNativeWord}),
                     BlockSimple(StoreArgument {source=RegisterArgument valueReg1, base=base, offset=offset, index=index, kind=movePolyWord, isMutable=true})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICodeStore({kind=LoadStoreNativeWord _, address, value}, context, _, destination) =
            let
                (* Store a 32-bit value from a large-word. *)
                val (valueCode, valueReg) = codeToPReg(value, context)
                val valueReg1 = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index, ...}) = codeAddress(address, nativeWordSize, context)
                val code =
                    codeBaseIndex @ valueCode @ codeUntag @
                    [BlockSimple(LoadArgument{source=wordAt valueReg, dest=valueReg1, kind=moveNativeWord}),
                     BlockSimple(StoreArgument {source=RegisterArgument valueReg1, base=base, offset=offset, index=index, kind=moveNativeWord, isMutable=true})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        and codeToICodeBlock({kind=BlockOpCompareByte, sourceLeft, destRight, length}, context, _, destination) =
            let
                (* This is effectively a big-endian comparison since we compare the bytes until we
                   find an inequality. *)
                val target = asTarget destination
                val mergeResult = newMergeReg()
                val vec1Reg = newUReg() and vec2Reg = newUReg()
                val (leftCode, leftUntag, {base=leftBase, offset=leftOffset, index=leftIndex, ...}) =
                    codeAddress(sourceLeft, 0w1, context)
                val (rightCode, rightUntag, {base=rightBase, offset=rightOffset, index=rightIndex, ...}) =
                    codeAddress(destRight, 0w1, context)
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
                     BlockSimple(LoadArgument{ source=IntegerConstant(tag 0), dest=mergeResult, kind=movePolyWord }),
                     BlockFlow(Unconditional exitLab),
                     BlockLabel labLess,
                     BlockSimple(LoadArgument{ source=IntegerConstant(tag ~1), dest=mergeResult, kind=movePolyWord }),
                     BlockFlow(Unconditional exitLab),
                     BlockLabel labGreater,
                     BlockSimple(LoadArgument{ source=IntegerConstant(tag 1), dest=mergeResult, kind=movePolyWord }),
                     BlockLabel exitLab,
                     BlockSimple(LoadArgument{ source=RegisterArgument mergeResult, dest=target, kind=movePolyWord })]
            in
                (code, RegisterArgument target, false)
            end

        |   codeToICodeBlock({kind=BlockOpMove {isByteMove}, sourceLeft, destRight, length}, context, _, destination) =
            let
                (* Moves of 4 or 8 bytes can be done as word moves provided the alignment is correct.
                   Although this will work for strings it is really to handle moves between SysWord and
                   volatileRef in Foreign.Memory.  Moves of 1, 2 or 3 bytes or words are converted into a
                   sequence of byte or word moves. *)
                val isWordMove =
                    case (isByteMove, length) of
                        (true, BICConstnt(l, _)) =>
                        if not (isShort l) orelse (toShort l <> 0w4 andalso toShort l <> nativeWordSize)
                        then NONE
                        else
                        let
                            val leng = Word.toInt(toShort l)
                            val moveKind =
                                if toShort l = nativeWordSize
                                then moveNativeWord
                                else Move32Bit
                            val isLeftAligned =
                                case sourceLeft of
                                    {index=NONE, offset:int, ...} => offset mod leng = 0
                                |   _ => false
                            val isRightAligned =
                                case destRight of
                                    {index=NONE, offset: int, ...} => offset mod leng = 0
                                |   _ => false
                        in
                            if isLeftAligned andalso isRightAligned
                            then SOME moveKind
                            else NONE
                        end
                    |   _ => NONE
            in
                case isWordMove of
                    SOME moveKind =>
                    let
                        val (leftCode, leftUntag, leftMem) =
                            codeAddress(sourceLeft, if isByteMove then 0w1 else wordSize, context)
                        val (rightCode, rightUntag, {base, offset, index, ...}) =
                            codeAddress(destRight, if isByteMove then 0w1 else wordSize, context)
                        val untaggedResReg = newUReg()
                        val code =
                            leftCode @ rightCode @ leftUntag @ rightUntag @
                            [BlockSimple(LoadArgument { source=MemoryLocation leftMem, dest=untaggedResReg, kind=moveKind}),
                             BlockSimple(StoreArgument
                                {source=RegisterArgument untaggedResReg, base=base, offset=offset, index=index, kind=moveKind, isMutable=true})]
                    in
                        moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
                    end
                |   _ =>
                    let
                        val vec1Reg = newUReg() and vec2Reg = newUReg()
                        val (leftCode, leftUntag, {base=leftBase, offset=leftOffset, index=leftIndex, ...}) =
                            codeAddress(sourceLeft, if isByteMove then 0w1 else wordSize, context)
                        val (rightCode, rightUntag, {base=rightBase, offset=rightOffset, index=rightIndex, ...}) =
                            codeAddress(destRight, if isByteMove then 0w1 else wordSize, context)
                        val (lengthCode, lengthUntag, lengthArg) = codeAsUntaggedToReg(length, false (* unsigned *), context)
                        val code =
                            leftCode @ rightCode @ lengthCode @
                            leftUntag @ [BlockSimple(loadAddress{base=leftBase, offset=leftOffset, index=leftIndex, dest=vec1Reg})] @
                            rightUntag @ [BlockSimple(loadAddress{base=rightBase, offset=rightOffset, index=rightIndex, dest=vec2Reg})] @
                            lengthUntag @
                            [BlockSimple(BlockMove{ srcAddr=vec1Reg, destAddr=vec2Reg, length=lengthArg, isByteMove=isByteMove })]
                    in
                        moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
                    end
            end

        |   codeToICodeBlock({kind=BlockOpEqualByte, ...}, _, _, _) =
                (* TODO: Move the code from codeToICodeRev.  However, that is already reversed. *)
                raise InternalError "codeToICodeBlock - BlockOpEqualByte" (* Already done *)

        and codeConditionRev(condition, context, jumpOn, jumpLabel, tailCode) =
           (* General case.  Load the value into a register and compare it with 1 (true) *)
            let
                val ccRef = newCCRef()
                val (testCode, testReg) = codeToPRegRev(condition, context, tailCode)
                val noJumpLabel = newLabel()
            in
                BlockLabel noJumpLabel ::
                BlockFlow(Conditional{ccRef=ccRef,
                           condition=if jumpOn then JE else JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                BlockSimple(CompareLiteral{arg1=RegisterArgument testReg, arg2=tag 1, opSize=OpSize32, ccRef=ccRef}) ::
                testCode
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
                    [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg1, operand2=IntegerConstant constVal, ccRef=ccRef, opSize=polyWordOpSize})] @
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
                    [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg2, operand2=IntegerConstant constVal, ccRef=ccRef, opSize=polyWordOpSize})] @
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
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=aReg1, operand2=IntegerConstant 1, ccRef=newCCRef(), opSize=polyWordOpSize}),
                     BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg3, operand2=RegisterArgument aReg2, ccRef=ccRef, opSize=polyWordOpSize})] @
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
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=target, operand1=aReg1, operand2=IntegerConstant constVal, ccRef=ccRef, opSize=polyWordOpSize})] @
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
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=aReg1, operand2=RegisterArgument aReg2, ccRef=ccRef, opSize=polyWordOpSize})] @
                    onOverflow ccRef @
                    [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=aReg3, operand2=IntegerConstant 1, ccRef=newCCRef(), opSize=polyWordOpSize})]
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
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=argUntagged, operand1=aReg, operand2=IntegerConstant 1, ccRef=newCCRef(), opSize=polyWordOpSize}),
                     BlockSimple(Multiplication{resultReg=resUntagged, operand1=argUntagged, operand2=IntegerConstant constVal, ccRef=mulCC, opSize=polyWordOpSize} )] @
                     onOverflow mulCC @
                     [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=resUntagged, operand2=IntegerConstant 1, ccRef=newCCRef(), opSize=polyWordOpSize})]
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
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=argUntagged, operand1=aReg, operand2=IntegerConstant 1, ccRef=newCCRef(), opSize=polyWordOpSize}),
                     BlockSimple(Multiplication{resultReg=resUntagged, operand1=argUntagged, operand2=IntegerConstant constVal, ccRef=mulCC, opSize=polyWordOpSize} )] @
                     onOverflow mulCC @
                     [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=resUntagged, operand2=IntegerConstant 1, ccRef=newCCRef(), opSize=polyWordOpSize})]
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
                    [BlockSimple(UntagValue{source=aReg1, dest=arg1Untagged, isSigned=true (* Signed shift here. *), cache=NONE, opSize=polyWordOpSize}),
                     BlockSimple(ArithmeticFunction{oper=SUB, resultReg=arg2Untagged, operand1=aReg2, operand2=IntegerConstant 1, ccRef=newCCRef(), opSize=polyWordOpSize}),
                     BlockSimple(Multiplication{resultReg=resUntagged, operand1=arg1Untagged, operand2=RegisterArgument arg2Untagged, ccRef=mulCC, opSize=polyWordOpSize} )] @
                     onOverflow mulCC @
                     [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=resUntagged, operand2=IntegerConstant 1, ccRef=newCCRef(), opSize=polyWordOpSize})]
            end

        |   codeFixedPrecisionArith(BuiltIns.ArithQuot, arg1, arg2, context, target, _) =
            let
                val aReg1 = newPReg() and aReg2 = newPReg()
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift both of the arguments to remove the tags.  We don't test for zero here - that's done explicitly. *)
                    [BlockSimple(UntagValue{source=aReg1, dest=arg1Untagged, isSigned=true, cache=NONE, opSize=polyWordOpSize}),
                     BlockSimple(UntagValue{source=aReg2, dest=arg2Untagged, isSigned=true, cache=NONE, opSize=polyWordOpSize}),
                     BlockSimple(Division { isSigned = true, dividend=arg1Untagged, divisor=RegisterArgument arg2Untagged,
                                quotient=quotient, remainder=remainder, opSize=polyWordOpSize }),
                     BlockSimple(TagValue { source=quotient, dest=target, isSigned=true, opSize=polyWordOpSize})]
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
                    [BlockSimple(UntagValue{source=aReg1, dest=arg1Untagged, isSigned=true, cache=NONE, opSize=polyWordOpSize}),
                     BlockSimple(UntagValue{source=aReg2, dest=arg2Untagged, isSigned=true, cache=NONE, opSize=polyWordOpSize}),
                     BlockSimple(Division { isSigned = true, dividend=arg1Untagged, divisor=RegisterArgument arg2Untagged,
                                quotient=quotient, remainder=remainder, opSize=polyWordOpSize }),
                     BlockSimple(TagValue { source=remainder, dest=target, isSigned=true, opSize=polyWordOpSize})]
            end

        |   codeFixedPrecisionArith(_, _, _, _, _, _) =
                raise InternalError "codeToICode: FixedPrecisionArith - unimplemented operation"

        (* Generate code for floating point arguments where one of the arguments must be
           in a register.  If the first argument is in a register use that, if the second is
           in a register and it's commutative use that otherwise load the first argument
           into a register. *)
        and codeFPBinaryArgsRev(arg1, arg2, precision, commutative, context, tailCode) =
        let
            val (arg1Code, arg1Value) = codeFPArgument(arg1, precision, context, tailCode)
            val (arg2Code, arg2Value) = codeFPArgument(arg2, precision, context, arg1Code)
        in
            case (arg1Value, arg2Value, commutative) of
                (RegisterArgument fpReg, _, _) => (arg2Code, fpReg, arg2Value)

            |   (_, RegisterArgument fpReg, Commutative) => (arg2Code, fpReg, arg1Value)

            |   (arg1Val, _, _) =>
                let
                    val fpReg = newUReg()
                    val moveOp =
                        case precision of
                            BuiltIns.PrecDouble => MoveDouble | BuiltIns.PrecSingle => MoveFloat
                in
                    (BlockSimple(LoadArgument{source=arg1Val, dest=fpReg, kind=moveOp}) :: arg2Code, fpReg, arg2Value)
                end
        end

        (* Generate code to evaluate a floating point argument.  The aim of this code is to avoid
           the overhead of untagging a short-precision floating point value in memory. *)
        and codeFPArgument(BICConstnt(value, _), _, _, tailCode) =
            let
                val argVal =
                    (* Single precision constants in 64-bit mode are represented by the value
                       shifted left 32 bits.  A word is shifted left one bit so the result is 0w31. *)
                    if isShort value
                    then IntegerConstant(Word.toLargeInt(Word.>>(toShort value, 0w31)))
                    else AddressConstant value
            in
                (tailCode, argVal)
            end

        |   codeFPArgument(arg, precision, context, tailCode) =
        (
            case (precision, wordSize) of
                (BuiltIns.PrecSingle, 0w8) =>
                    (* If this is a single precision value and the word size is 8 the values are tagged.
                       If it is memory we can load the value directly from the high-order word. *)
                let
                    val memOrReg = { anyConstant=false, const32s=false, memAddr=true, existingPreg=true }
                    val (code, result, _) = codeToICodeRev(arg, context, false, Allowed memOrReg, tailCode)
                in
                    case result of
                        RegisterArgument argReg =>
                        let
                            val fpReg = newUReg()
                        in
                            (BlockSimple(UntagFloat{source=RegisterArgument argReg, dest=fpReg, cache=NONE}) :: code,
                                RegisterArgument fpReg)
                        end
                    |   MemoryLocation{offset, base, index, ...} =>
                            (code, MemoryLocation{offset=offset+4, base=base, index=index, cache=NONE})

                    |   _ => raise InternalError "codeFPArgument"
                end

            |   _ =>
                (* Otherwise the value is boxed. *)
                let
                    val (argCode, argReg) = codeToPRegRev(arg, context, tailCode)
                in
                    (argCode, wordAt argReg)
                end
        )

        (* Code an address.  The index is optional. *)
        and codeAddressRev({base, index=SOME index, offset}, 0w1 (* byte move *), context, tailCode) =
            let
                (* Byte address with index.  The index needs to be untagged. *)
                val indexReg1 = newUReg()
                val (codeBase, baseReg) = codeToPRegRev(base, context, tailCode)
                val (codeIndex, indexReg) = codeToPRegRev(index, context, codeBase)
                val untagCode = [BlockSimple(UntagValue{dest=indexReg1, source=indexReg, isSigned=false, cache=NONE, opSize=polyWordOpSize})]
                val (codeLoadAddr, realBase) =
                    if targetArch = ObjectId32Bit
                    then
                    let
                        val addrReg = newUReg()
                    in
                        ([BlockSimple(LoadEffectiveAddress{ base=SOME baseReg, offset=0, index=ObjectIndex, dest=addrReg, opSize=nativeWordOpSize})], addrReg)
                    end
                    else ([], baseReg)
                val memResult = {base=realBase, offset=offset, index=MemIndex1 indexReg1, cache=NONE}
            in
                (codeLoadAddr @ codeIndex, untagCode, memResult)
            end

        |   codeAddressRev({base, index=SOME index, offset}, scale (* word move *), context, tailCode) =
            let
                (* Word address with index.  We can avoid untagging the index by adjusting the
                   multiplier and offset *) 
                val (codeBase, baseReg) = codeToPRegRev(base, context, tailCode)
                val (codeIndex, indexReg) = codeToPRegRev(index, context, codeBase)
                val (codeLoadAddr, realBase) =
                    if targetArch = ObjectId32Bit
                    then
                    let
                        val addrReg = newUReg()
                    in
                        ([BlockSimple(LoadEffectiveAddress{ base=SOME baseReg, offset=0, index=ObjectIndex, dest=addrReg, opSize=nativeWordOpSize})], addrReg)
                    end
                    else ([], baseReg)
                val memResult =
                    case scale of
                        0w8 => {base=realBase, offset=offset-4, index=MemIndex4 indexReg, cache=NONE}
                    |   0w4 => {base=realBase, offset=offset-2, index=MemIndex2 indexReg, cache=NONE}
                    |   _ => raise InternalError "codeAddressRev: unknown scale"
            in
                (codeLoadAddr @ codeIndex, [], memResult)
            end

        |   codeAddressRev({base, index=NONE, offset}, _, context, tailCode) =
            let
                val (codeBase, baseReg) = codeToPRegRev(base, context, tailCode)
                val memResult = {offset=offset, base=baseReg, index=memIndexOrObject, cache=NONE}
            in
                (codeBase, [], memResult)
            end

        and codeAddress(addr, scale, context) =
        let
            val (code, untag, res) = codeAddressRev(addr, scale, context, [])
        in
            (List.rev code, untag, res)
        end

        (* C-memory operations are slightly different.  The base address is a LargeWord.word value.
           The index is a byte index so may have to be untagged. *)
        and codeCAddress({base, index=SOME index, offset}, 0w1, context) =
            let
                (* Byte address with index.  The index needs to be untagged. *)
                val untaggedBaseReg = newUReg()
                val (codeBase, baseReg) = codeToPReg(base, context)
                and (codeIndex, indexReg) = codeToPReg(index, context)
                (* The index needs to untagged and, if necessary, sign-extended to the native word size. *)
                val (untagCode, sxReg) =
                    if targetArch = ObjectId32Bit
                    then
                    let
                        val sReg1 = newUReg() and sReg2 = newUReg()
                    in
                        ([BlockSimple(SignExtend32To64{dest=sReg1, source=RegisterArgument indexReg}),
                          BlockSimple(UntagValue{dest=sReg2, source=sReg1, isSigned=true, cache=NONE, opSize=nativeWordOpSize})], sReg2)
                    end
                    else
                    let
                        val sReg = newUReg()
                    in
                        ([BlockSimple(UntagValue{dest=sReg, source=indexReg, isSigned=true, cache=NONE, opSize=nativeWordOpSize})], sReg)
                    end
                val loadCode = [BlockSimple(LoadArgument{source=wordAt baseReg, dest=untaggedBaseReg, kind=moveNativeWord})]
                val memResult = {base=untaggedBaseReg, offset=offset, index=MemIndex1 sxReg, cache=NONE}
            in
                (codeBase @ codeIndex, loadCode @ untagCode, memResult)
            end

        |   codeCAddress({base, index=SOME index, offset}, size, context) =
            let
                (* Non-byte address with index.  By using an appropriate multiplier we can avoid
                   having to untag the index. *)
                val untaggedBaseReg = newUReg()
                val (codeBase, baseReg) = codeToPReg(base, context)
                and (codeIndex, indexReg) = codeToPReg(index, context)
                (* The index is signed i.e. negative index values are legal.  We don't have
                   to do anything special on the native code versions but on 32-in-64 we need
                   to sign extend. *)
                val (untagCode, sxReg) =
                    if targetArch = ObjectId32Bit
                    then
                    let
                        val sReg = newUReg()
                    in
                        ([BlockSimple(SignExtend32To64{source=RegisterArgument indexReg, dest=sReg})], sReg)
                    end
                    else ([], indexReg)
                val loadCode = [BlockSimple(LoadArgument{source=wordAt baseReg, dest=untaggedBaseReg, kind=moveNativeWord})]
                val memResult =
                    case size of
                        0w2 => {base=untaggedBaseReg, offset=offset-1, index=MemIndex1 sxReg, cache=NONE}
                    |   0w4 => {base=untaggedBaseReg, offset=offset-2, index=MemIndex2 sxReg, cache=NONE}
                    |   0w8 => {base=untaggedBaseReg, offset=offset-4, index=MemIndex4 sxReg, cache=NONE}
                    |   _ => raise InternalError "codeCAddress: unknown size"
            in
                (codeBase @ codeIndex, loadCode @ untagCode, memResult)
            end

        |   codeCAddress({base, index=NONE, offset}, _, context) =
            let
                val untaggedBaseReg = newUReg()
                val (codeBase, baseReg) = codeToPReg(base, context)
                val untagCode = [BlockSimple(LoadArgument{source=wordAt baseReg, dest=untaggedBaseReg, kind=moveNativeWord})]
                val memResult = {offset=offset, base=untaggedBaseReg, index=NoMemIndex, cache=NONE}
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
                val untag = [BlockSimple(LoadArgument{source=cArg, dest=untagReg, kind=movePolyWord})]
            in
                (tailCode, untag, untagReg) (* Don't tag. *)
            end
        |   codeAsUntaggedToRegRev(arg, isSigned, context, tailCode) =
            let
                val untagReg = newUReg()
                val (code, srcReg) = codeToPRegRev(arg, context, tailCode)
                val untag = [BlockSimple(UntagValue{source=srcReg, dest=untagReg, isSigned=isSigned, cache=NONE, opSize=polyWordOpSize})]
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
        and codeAsUntaggedByte(BICConstnt(value, _), isSigned, _) =
            let
                val cval = if isShort value then toShort value else 0w0
                val cArg = IntegerConstant(if isSigned then Word.toLargeIntX cval else Word.toLargeInt cval) (* Don't tag *)
            in
                ([], [], cArg)
            end
        |   codeAsUntaggedByte(arg, isSigned, context) =
            let
                val untagReg = newUReg()
                val (code, argReg) = codeToPReg(arg, context)
                val untag = [BlockSimple(UntagValue{source=argReg, dest=untagReg, isSigned=isSigned, cache=NONE, opSize=OpSize32})]
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
            and (flagsCode, flagUntag, flagArg) = codeAsUntaggedByte(flags, false, context)
            (* We're better off deferring the initialiser if possible.  If the value is
               a constant we don't have to save it. *)
            val (initCode, initResult, _) = codeToICode(initial, context, false, Allowed allowDefer)
         in
            (sizeCode @ flagsCode @ initCode
              @
             [(* We need to copy the size here because AllocateMemoryVariable modifies the
                 size in order to store the length word.  This is unfortunate especially as
                 we're going to untag it anyway. *)
              BlockSimple(LoadArgument{source=RegisterArgument sizeReg, dest=sizeReg2, kind=movePolyWord}),
              BlockSimple(AllocateMemoryVariable{size=sizeReg, dest=allocReg, saveRegs=[]})] @
              flagUntag @
              [BlockSimple(StoreArgument{ source=flagArg, base=allocReg, offset= ~1, index=memIndexOrObject, kind=MoveByte, isMutable=false}),
              (* We need to copy the address here because InitialiseMem modifies all its arguments. *)
              BlockSimple(
                if targetArch = ObjectId32Bit
                then LoadEffectiveAddress{ base=SOME allocReg, offset=0, index=ObjectIndex, dest=initAddrReg, opSize=nativeWordOpSize}
                else LoadArgument{source=RegisterArgument allocReg, dest=initAddrReg, kind=movePolyWord}),
              BlockSimple(UntagValue{source=sizeReg2, dest=untagSizeReg, isSigned=false, cache=NONE, opSize=polyWordOpSize}),
              BlockSimple(LoadArgument{source=initResult, dest=initReg, kind=movePolyWord}),
              BlockSimple(InitialiseMem{size=untagSizeReg, init=initReg, addr=initAddrReg}),
              BlockSimple InitialisationComplete,
              BlockSimple(LoadArgument{source=RegisterArgument allocReg, dest=target, kind=movePolyWord})], RegisterArgument target, false)
        end

        (*Turn the codetree structure into icode. *)
        val bodyContext = {loopArgs=NONE, stackPtr=0, currHandler=NONE, overflowBlock=ref NONE}
        val (bodyCode, _, bodyExited) =
            codeToICodeRev(body, bodyContext, true, SpecificPReg resultTarget, beginInstructions)
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
            
            |   splitCode(BlockBegin args :: _, sinceLabel, flow) =
                    (* Final instruction.  Create the initial block and exit. *)
                    createEntry(0, BeginFunction args ::sinceLabel, flow)
            
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
      
        open ICodeTransform
        
        val pregProperties = Vector.fromList(List.rev(! pregPropList))
    in
        codeICodeFunctionToX86{blocks = resultVector, functionName = name, pregProps = pregProperties,
            ccCount= ! ccRefCounter, debugSwitches = debugSwitches, resultClosure = resultClosure,
            profileObject = profileObject}
    end

    fun gencodeLambda(lambda, debugSwitches, closure) =
    let
        open Debug Universal
        (*val debugSwitches =
            [tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
            tagInject assemblyCodeTag true] @ debugSwitches*)
    in
        codeFunctionToX86(lambda, debugSwitches, closure)
    end
    
    structure Foreign = X86Foreign
    
    structure Sharing =
    struct
        type backendIC = backendIC
        and  bicLoadForm = bicLoadForm
        and argumentType = argumentType
        and closureRef = closureRef
    end
    
end;
