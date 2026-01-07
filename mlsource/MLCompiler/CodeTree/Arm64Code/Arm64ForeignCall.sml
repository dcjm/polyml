(*
    Copyright (c) 2021-2, 2026 David C. J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    Licence version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public Licence for more details.
    
    You should have received a copy of the GNU Lesser General Public
    Licence along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

functor Arm64ForeignCall(
    structure CodeArray: CODEARRAY
    and       Arm64PreAssembly: ARM64PREASSEMBLY
    and       Debug: DEBUG

    sharing CodeArray.Sharing = Arm64PreAssembly.Sharing
): FOREIGNCALL
=
struct

    open CodeArray Arm64PreAssembly
    
    exception InternalError = Misc.InternalError
    and Foreign = Foreign.Foreign

    datatype fastArgs = FastArgFixed | FastArgDouble | FastArgFloat

    val makeEntryPoint: string -> machineWord = RunCall.rtsCallFull1 "PolyCreateEntryPointObject"

    val wordToWord8 = Word8.fromLarge o Word.toLarge

    (* Turn an index into an absolute address. *)
    fun indexToAbsoluteAddress(iReg, absReg) =
    case archType of
        ArchC32 shift =>
            [AddShiftedReg{regM=iReg, regN=X_Base32in64, regD=absReg, shift=ShiftLSL(wordToWord8 shift),
                opSize=OpSize64, setFlags=false}]
    |   ArchNative =>
            if iReg = absReg
            then []
            else [MoveXRegToXReg{sReg=iReg, dReg=absReg}]

    (* Call the RTS.  Previously this did not check for exceptions raised in the RTS and instead
       there was code added after each call.  Doing it after the call doesn't affect the time
       taken but makes the code larger especially as this is needed in every arbitrary precision
       operation.
       Currently we clear the RTS exception packet field before the call.  The field is
       cleared in "full" calls that may raise an exception but not in fast calls.  They
       may not raise an exception but the packet may not have been cleared from a
       previous call. *)

    fun rtsCallFastGeneral (functionName, debugSwitches) =
    let
        val entryPointAddr = makeEntryPoint functionName
        (* The maximum we currently have is five so we don't need to worry about stack args. *)

        val labelMaker = createLabelMaker()

        val noRTSException = createLabel labelMaker

        val instructions =
                (* Clear the RTS exception state. *)
                LoadNonAddr(X16, 0w1) ::
            [
                StoreRegScaled{regT=X16, regN=X_MLAssemblyInt, unitOffset=exceptionPacketOffset, loadType=Load64},
                (* Move X30 to X23, a callee-save register. *)
                (* Note: maybe we should push X24 just in case this is the only
                   reachable reference to the code. *)
                LogicalShiftedReg{regN=XZero, regM=X_LinkReg, regD=X23, shift=ShiftNone, logOp=LogOr, opSize=OpSize64, setFlags=false},
                LoadAddr(X16, entryPointAddr) (* Load entry point *)
            ] @ indexToAbsoluteAddress(X16, X16) @
            [
                LoadRegScaled{regT=X16, regN=X16, unitOffset=0, loadType=Load64}, (* Load the actual address. *)
                (* Store the current heap allocation pointer. *)
                StoreRegScaled{regT=X_MLHeapAllocPtr, regN=X_MLAssemblyInt, unitOffset=heapAllocPtrOffset, loadType=Load64},
                (* For the moment save and restore the ML stack pointer.  No RTS call should change
                   it and it's callee-save but just in case... *)
                StoreRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=mlStackPtrOffset, loadType=Load64},
                BranchReg{regD=X16, brRegType=BRRAndLink}, (* Call the function. *)
                (* Restore the ML stack pointer. *)
                LoadRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=mlStackPtrOffset, loadType=Load64},
                (* Load the heap allocation ptr and limit.  We could have GCed in the RTS call. *)
                LoadRegScaled{regT=X_MLHeapAllocPtr, regN=X_MLAssemblyInt, unitOffset=heapAllocPtrOffset, loadType=Load64},
                LoadRegScaled{regT=X_MLHeapLimit, regN=X_MLAssemblyInt, unitOffset=heapLimitPtrOffset, loadType=Load64},
                (* Check the RTS exception. *)
                LoadRegScaled{regT=X16, regN=X_MLAssemblyInt, unitOffset=exceptionPacketOffset, loadType=Load64},
                SubImmediate{regN=X16, regD=XZero, immed=0w1, shifted=false, setFlags=true, opSize=OpSize64},
                ConditionalBranch(CondEqual, noRTSException),
                    (* If it isn't then raise the exception. *)
                MoveXRegToXReg{sReg=X16, dReg=X0},
                LoadRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset, loadType=Load64},
                LoadRegScaled{regT=X16, regN=X_MLStackPtr, unitOffset=0, loadType=Load64},
                BranchReg{regD=X16, brRegType=BRRBranch},
                SetLabel noRTSException
            ] @
            [
                BranchReg{regD=X23, brRegType=BRRReturn}
            ]
        val closure = makeConstantClosure()
        val () = generateFinalCode{instrs=instructions, name=functionName, parameters=debugSwitches, resultClosure=closure,
                              profileObject=createProfileObject(), labelMaker=labelMaker}
    in
        closureAsAddress closure
    end

    (* Provided there are no more than eight fixed pt args and four floating pt args
       everything will be in the correct place.  These are only used in Initialise
       so the number of arguments is limited, currently six. *)
    fun rtsCallFast (functionName, nArgs, debugSwitches) =
        if nArgs > 8 then raise InternalError "rtsCallFast: more than 8 arguments"
        else rtsCallFastGeneral (functionName, debugSwitches)
    
    val rtsCallFastRealtoReal = rtsCallFastGeneral
    and rtsCallFastRealRealtoReal = rtsCallFastGeneral
    and rtsCallFastRealGeneraltoReal = rtsCallFastGeneral
    and rtsCallFastGeneraltoReal = rtsCallFastGeneral
    and rtsCallFastFloattoFloat = rtsCallFastGeneral
    and rtsCallFastFloatFloattoFloat = rtsCallFastGeneral
    and rtsCallFastFloatGeneraltoFloat = rtsCallFastGeneral
    and rtsCallFastGeneraltoFloat = rtsCallFastGeneral

    (* There is only one ABI value. *)
    datatype abi = ARM64Abi
    fun abiList () = [("default", ARM64Abi)]

    fun alignUp(s, align) = Word.andb(s + align-0w1, ~ align)
    
    val getThreadDataCall = makeEntryPoint "PolyArm64GetThreadData"

    (* This must match the type in Foreign.LowLevel.  Once this is bootstrapped we could use that
       type but note that this is the type we use within the compiler and we build Foreign.LowLevel
       AFTER compiling this. *)
    datatype cTypeForm =
        CTypeFloatingPt | CTypePointer | CTypeSignedInt | CTypeUnsignedInt
    |   CTypeStruct of cType list | CTypeVoid
    withtype cType = { typeForm: cTypeForm, align: word, size: word }

    (* Load a byte, halfword, word or long *)
    fun loadAlignedValue(reg, base, offset, size) =
    let
        val _ = offset mod size = 0w0 orelse raise InternalError "loadAlignedValue: not aligned"
        val loadOp =
            case size of
                0w8 => Load64
            |   0w4 => Load32
            |   0w2 => Load16
            |   0w1 => Load8
            |   _ => raise InternalError "loadAlignedValue: invalid length"
    in
        LoadRegScaled{regT=reg, regN=base, unitOffset=Word.toInt(offset div size), loadType=loadOp}
    end

    (* Store a register into upto 8 bytes.  Most values will involve a single store but odd-sized
       structs can require shifts and multiple stores.  N.B.  May modify the source register. *)
    and storeUpTo8(reg, base, offset, size) =
    let
        val storeOp =
            if size = 0w8 then Load64 else if size >= 0w4 then Load32
            else if size >= 0w2 then Load16 else Load8
    in
        [StoreRegUnscaled{regT=reg, regN=base, byteOffset=offset, loadType=storeOp, unscaledType=NoUpdate}]
    end @
    (
        if size = 0w6 orelse size = 0w7
        then
        [
            shiftConstant{direction=ShiftRightLogical, regN=reg, regD=reg, shift=0w32, opSize=OpSize64 },
            StoreRegUnscaled{regT=reg, regN=base, byteOffset=offset+4, loadType=Load16, unscaledType=NoUpdate}
        ]
        else []
    ) @
    (
        if size = 0w3 orelse size = 0w5 orelse size = 0w7
        then
        [
            shiftConstant{direction=ShiftRightLogical, regN=reg, regD=reg, shift=(size-0w1)*0w8, opSize=OpSize64 },
            StoreRegUnscaled{regT=reg, regN=base, byteOffset=offset+Word.toInt(size-0w1), loadType=Load8, unscaledType=NoUpdate}
        ]
        else []
    )

    (* Extract the elements of structures. *)
    fun unwrap(CTypeStruct ctypes, _) = List.foldr(fn({typeForm, size, ...}, l) => unwrap(typeForm, size) @ l) [] ctypes
    |   unwrap (ctype, size) = [(ctype, size)]

    (* Structures of up to four floating point values of the same precision are treated specially. *)
    datatype argClass =
        ArgClassHFA of Word8.word * bool (* 1 - 4 floating pt values *)
    |   ArgLargeStruct          (* > 16 bytes and not an HFA *)
    |   ArgSmall                (* Scalars or small structures *)

    fun classifyArg(ctype, size) =
        case unwrap (ctype, size) of
            [(CTypeFloatingPt, 0w4)] =>
                ArgClassHFA(0w1, false)
        |   [(CTypeFloatingPt, 0w4), (CTypeFloatingPt, 0w4)] =>
                ArgClassHFA(0w2, false)
        |   [(CTypeFloatingPt, 0w4), (CTypeFloatingPt, 0w4), (CTypeFloatingPt, 0w4)] =>
                ArgClassHFA(0w3, false)
        |   [(CTypeFloatingPt, 0w4), (CTypeFloatingPt, 0w4), (CTypeFloatingPt, 0w4), (CTypeFloatingPt, 0w4)] =>
                ArgClassHFA(0w4, false)
        |   [(CTypeFloatingPt, 0w8)] =>
                ArgClassHFA(0w1, true)
        |   [(CTypeFloatingPt, 0w8), (CTypeFloatingPt, 0w8)] =>
                ArgClassHFA(0w2, true)
        |   [(CTypeFloatingPt, 0w8), (CTypeFloatingPt, 0w8), (CTypeFloatingPt, 0w8)] =>
                ArgClassHFA(0w3, true)
        |   [(CTypeFloatingPt, 0w8), (CTypeFloatingPt, 0w8), (CTypeFloatingPt, 0w8), (CTypeFloatingPt, 0w8)] =>
                ArgClassHFA(0w4, true)
        |   _ =>
                if size > 0w16 then ArgLargeStruct
                else ArgSmall

    (* Can we load this in a single instruction? *)
    fun alignedLoadStore(_, 0w1) = true
    |   alignedLoadStore(addr, 0w2) = addr mod 0w2 = 0w0
    |   alignedLoadStore(addr, 0w4) = addr mod 0w4 = 0w0
    |   alignedLoadStore(addr, 0w8) = addr mod 0w8 = 0w0
    |   alignedLoadStore(addr, 0w16) = addr mod 0w8 = 0w0 (* Can use load-pair. *)
    |   alignedLoadStore _ = false

    (* This builds a piece of code that takes three arguments and returns a unit result.
       All three arguments are SysWord.word values i.e. ML addresses containing the address
       of the actual C value.
       The first argument (X0) is the address of the function to call.
       The second argument (X1) points to a struct that contains the argument(s) for the
       function.  The arguments have to be unpacked from the struct into the appropriate
       registers or to the C stack.
       The third argument (X2) points to a piece of memory to receive the result of the call.
       It may be empty if the function returns void.  It may only be as big as required
       for the result type. *)
    fun foreignCall(_: abi, args: cType list, result: cType): Address.machineWord =
    let
        val resultAreaPtr = X19 (* Unboxed value from X2 - This is callee save. *)
        val argPtrReg = X9 (* A scratch register that isn't used for arguments. *)
        val entryPtReg = X16 (* Contains the address of the function to call. *)
        val argWorkReg = X10 (* Used in loading arguments if necessary. *)
        and argWorkReg2 = X11
        and structSpacePtr = X12
        and argWorkReg3 = X13
        and argWorkReg4 = X14

        val labelMaker = createLabelMaker()

        fun loadArgs([], stackOffset, _, _, _, code, largeStructSpace) =
                (code, stackOffset, largeStructSpace)

        |   loadArgs(arg::args, stackOffset, argOffset, gRegNo, fpRegNo, code, largeStructSpace) =
            let
                val {size, align, typeForm, ...} =  arg
                val newArgOffset = alignUp(argOffset, align)
            in
                case classifyArg(typeForm, size) of
                    ArgClassHFA(numItems, isDouble) =>
                        if fpRegNo + numItems <= 0w8
                        then
                        let
                            val scale = if isDouble then 0w8 else 0w4
                            (* Load the values to the floating point registers. *)
                            fun loadFPRegs(0w0, _, _) = []
                            |   loadFPRegs(0w1, fpRegNo, offset) =
                                [LoadFPRegScaled{regT=VReg fpRegNo, regN=argPtrReg, unitOffset=offset, floatSize=if isDouble then Double64 else Float32}]
                            |   loadFPRegs(n, fpRegNo, offset) =
                                (LoadFPRegPair{regT1=VReg fpRegNo, regT2=VReg(fpRegNo+0w1), regN=argPtrReg, unitOffset=offset,
                                                floatSize=if isDouble then Double64 else Float32, unscaledType=NoUpdate} ::
                                        loadFPRegs(n-0w2, fpRegNo+0w2, offset+2))
                        in
                            loadArgs(args, stackOffset, newArgOffset+size, gRegNo, fpRegNo+numItems,
                                loadFPRegs(numItems, fpRegNo, Word.toInt(newArgOffset div scale)) @ code,
                                largeStructSpace)
                        end
                        else
                        let
                            (* If we have insufficient number of registers we discard any that are
                               left and push the argument to the stack. *)
                            (* The floating point value or structure is copied to the stack
                               as a contiguous area.  Use general registers to copy the data.
                               It could be on a 4-byte alignment.  In the typical case of a single
                               floating point value this will just be a single load and store. *)
                            fun copyData(0w0, _, _) = []
                            |   copyData(n, srcOffset, stackOffset) =
                                if isDouble
                                then LoadRegScaled{loadType=Load64, regT=argWorkReg2, regN=argPtrReg, unitOffset=srcOffset} ::
                                     StoreRegScaled{loadType=Load64, regT=argWorkReg2, regN=XSP, unitOffset=stackOffset} ::
                                     copyData(n-0w1, srcOffset+1, stackOffset+1)
                                else LoadRegScaled{loadType=Load32, regT=argWorkReg2, regN=argPtrReg, unitOffset=srcOffset} ::
                                     StoreRegScaled{loadType=Load32, regT=argWorkReg2, regN=XSP, unitOffset=stackOffset} ::
                                     copyData(n-0w1, srcOffset+1, stackOffset+1)

                            val copyToStack =
                                if isDouble
                                then copyData(numItems, Word.toInt(newArgOffset div 0w8), stackOffset)
                                else copyData(numItems, Word.toInt(newArgOffset div 0w4), stackOffset*2)
                            (* The overall size is rounded up to a multiple of 8 *)
                            val newStackOffset = stackOffset + Word.toInt(alignUp(size, 0w8) div 0w8)
                        in
                            loadArgs(args, newStackOffset, newArgOffset+size, gRegNo, 0w8,
                                copyToStack @ code, largeStructSpace)
                        end
             
                |   _ =>
                    let
                        (* Load an aligned argument into one or two registers or copy it to the stack. *)
                        fun loadArgumentValues(argSize, sourceOffset, sourceBase, newStructSpace, preCode) =
                            if gRegNo <= 0w6 orelse (size <= 0w8 andalso gRegNo <= 0w7)
                            then (* There are sufficient registers *)
                            let
                                val (loadInstr, nextGReg) =
                                    if argSize = 0w16
                                    then ([LoadRegPair{loadType=Load64, unscaledType=NoUpdate, regT1=XReg gRegNo, regT2=XReg(gRegNo+0w1),
                                                regN=sourceBase, unitOffset=Word.toInt(sourceOffset div 0w8)}], gRegNo+0w2)
                                    else ([loadAlignedValue(XReg gRegNo, sourceBase, sourceOffset, size)], gRegNo+0w1)
                            in
                                loadArgs(args, stackOffset, newArgOffset+size, nextGReg, fpRegNo,
                                        preCode @ loadInstr @ code, newStructSpace)
                            end
                            else if argSize = 0w16
                            then loadArgs(args, stackOffset+2, newArgOffset+size, 0w8, fpRegNo,
                                    preCode @
                                    LoadRegPair{loadType=Load64, unscaledType=NoUpdate, regT1=argWorkReg2, regT2=argWorkReg3, regN=sourceBase, unitOffset=Word.toInt(sourceOffset div 0w8)} ::
                                    StoreRegPair{loadType=Load64, unscaledType=NoUpdate, regT1=argWorkReg2, regT2=argWorkReg3, regN=XSP, unitOffset=stackOffset} :: code, newStructSpace)
                            else loadArgs(args, stackOffset+1, newArgOffset+size, 0w8, fpRegNo,
                                    preCode @ loadAlignedValue(argWorkReg2, sourceBase, sourceOffset, argSize) ::
                                    StoreRegScaled{loadType=Load64, regT=argWorkReg2, regN=XSP, unitOffset=stackOffset} :: code, newStructSpace)
                    in
                        if alignedLoadStore(newArgOffset, size)
                        then loadArgumentValues(size, newArgOffset, argPtrReg, largeStructSpace, [])

                        else (* General case.  Either a large structure or a small structure that
                                can't easily be loaded,  First copy it to the stack, and either pass
                                the address or load it once it's aligned. *)
                        let
                            val newStructSpace = alignUp(largeStructSpace + size, 0w16)
                            val loopLabel = createLabel labelMaker
                            (* The address of the area we're copying to is in argRegNo. *)
                            val argRegNo = if gRegNo < 0w8 then XReg gRegNo else argWorkReg
                            (* Copy from the end back to the start. *)
                            val copyToStructSpace =
                            [
                                AddImmediate{opSize=OpSize64, setFlags=false, regN=structSpacePtr, regD=argRegNo, immed=largeStructSpace, shifted=false}, 
                                AddImmediate{opSize=OpSize64, setFlags=false, regN=argRegNo, regD=argWorkReg2, immed=size, shifted=false}, (* End of dest area *)
                                AddImmediate{opSize=OpSize64, setFlags=false, regN=argPtrReg, regD=argWorkReg3, immed=newArgOffset+size, shifted=false}, (* end of source *)
                                SetLabel loopLabel,
                                LoadRegUnscaled{loadType=Load8, unscaledType=PreIndex, regT=argWorkReg4, regN=argWorkReg3, byteOffset= ~1},
                                StoreRegUnscaled{loadType=Load8, unscaledType=PreIndex, regT=argWorkReg4, regN=argWorkReg2, byteOffset= ~1},
                                SubShiftedReg{opSize=OpSize64, setFlags=true, regM=argWorkReg2, regN=argRegNo, regD=XZero, shift=ShiftNone}, (* At start? *)
                                ConditionalBranch(CondNotEqual, loopLabel)
                            ]
                        in
                            if size > 0w16
                            then (* Large struct - pass by reference *)
                            (
                                if gRegNo < 0w8
                                then loadArgs(args, stackOffset, newArgOffset+size, gRegNo+0w1, fpRegNo, copyToStructSpace @ code, newStructSpace)
                                else loadArgs(args, stackOffset+1, newArgOffset+size, 0w8, fpRegNo,
                                        copyToStructSpace @ StoreRegScaled{loadType=Load64, regT=argWorkReg, regN=XSP, unitOffset=stackOffset} :: code,
                                        newStructSpace)
                            )
                            else (* Small struct.  Since it's now in an area at least 16 bytes and properly aligned we can load it. *)
                                (* argRegNo points to where we copied it *)
                                loadArgumentValues(if size > 0w8 then 0w16 else 0w8, 0w0, argRegNo, newStructSpace, copyToStructSpace)
                        end
                    end
            end
    
        local
            val {size, typeForm, ...} =  result
            (* Store a result register into the result area.  In almost all cases
               this is very simple: the only complication is with structs of odd sizes. *)
            fun storeResult(reg, offset, size) = storeUpTo8(reg, resultAreaPtr, offset, size)
        in
            val (getResult, passArgAddress) =
                if typeForm = CTypeVoid
                then ([], false)
                else case classifyArg(typeForm, size) of
                    (* Floating point values are returned in s0-sn, d0-dn. *)
                    ArgClassHFA(numItems, isDouble) =>
                        let
                            fun storeFPRegs(0w0, _, _) = []
                            |   storeFPRegs(0w1, fpRegNo, offset) =
                                [StoreFPRegScaled{regT=VReg fpRegNo, regN=resultAreaPtr, unitOffset=offset, floatSize=if isDouble then Double64 else Float32}]
                            |   storeFPRegs(n, fpRegNo, offset) =
                                StoreFPRegPair{regT1=VReg fpRegNo, regT2=VReg(fpRegNo+0w1), regN=resultAreaPtr, unitOffset=offset,
                                        floatSize=if isDouble then Double64 else Float32, unscaledType=NoUpdate} ::
                                    storeFPRegs(n-0w2, fpRegNo+0w2, offset+2)
                        in
                            (storeFPRegs(numItems, 0w0 (* V0-Vn*), 0), false)
                        end

                |   ArgLargeStruct => ([], true) (* Structures larger than 16 bytes are passed by reference. *)

                |   _ =>
                        if size = 0w16
                        then ([StoreRegPair{loadType=Load64, unscaledType=NoUpdate, regT1=X0, regT2=X1, regN=resultAreaPtr, unitOffset=0}], false)
                        else if size > 0w8
                        then (StoreRegScaled{loadType=Load64, regT=X0, regN=resultAreaPtr, unitOffset=0} :: storeResult(X1, 8, size-0w8), false)
                        else (storeResult(X0, 0, size), false)
        end

        val (argCode, argStack, largeStructSpace) =
            loadArgs(args, 0, 0w0, 0w0, 0w0,
                    if passArgAddress (* If we have to pass the address of the result struct it goes in X8. *)
                    then [MoveXRegToXReg{sReg=resultAreaPtr, dReg=X8}]
                    else [], 0w0)

        val stackSpaceRequired = alignUp(Word.fromInt argStack * 0w8, 0w16) + largeStructSpace

        val instructions =
            [(* Push the return address to the stack.  We could put it in a callee-save register but
                there's a very small chance that this could be the last reference to a piece of code. *)
             StoreRegUnscaled{loadType=Load64, unscaledType=PreIndex, regT=X30, regN=X_MLStackPtr, byteOffset= ~8},
             (* Save heap ptr.  Needed in case we have a callback. *)
             StoreRegScaled{loadType=Load64, regT=X_MLHeapAllocPtr, regN=X_MLAssemblyInt, unitOffset=heapAllocPtrOffset}
            ] @ indexToAbsoluteAddress(X0, X0) @
             (* Load the entry point address. *)
             LoadRegScaled{loadType=Load64, regT=entryPtReg, regN=X0, unitOffset=0} ::
            (
                (* Unbox the address of the result area into a callee save resgister.  This is where
                   the result will be stored on return if it is anything other than a struct.
                   We have to put the C address in there now because an ML address wouldn't be updated
                   by a possible GC in a callback. *)
                if #typeForm(result) <> CTypeVoid
                then indexToAbsoluteAddress(X2, X2) @ [LoadRegScaled{loadType=Load64, regT=resultAreaPtr, regN=X2, unitOffset=0}]
                else []
            ) @
            [StoreRegScaled{loadType=Load64, regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=mlStackPtrOffset}] @ (* Save the stack pointer. *)
            (
                if stackSpaceRequired = 0w0
                then []
                else [SubImmediate{opSize=OpSize64, setFlags=false, regN=XSP, regD=XSP, immed=stackSpaceRequired, shifted=false}]
            ) @
            (
                (* If we need to copy a struct load a register with a pointer to the area for it. *)
                if largeStructSpace = 0w0
                then []
                else [AddImmediate{opSize=OpSize64, setFlags=false, regN=XSP, regD=structSpacePtr, immed=stackSpaceRequired-largeStructSpace, shifted=false}]
            ) @
            (
                (* The second argument is a SysWord containing the address of a malloced area of memory
                   with the actual arguments in it. *)
                if null args
                then []
                else indexToAbsoluteAddress(X1, X1) @ [LoadRegScaled{loadType=Load64, regT=argPtrReg, regN=X1, unitOffset=0}]
            ) @ argCode @
            [BranchReg{regD=X16, brRegType=BRRAndLink}] @ (* Call the function. *)
            (* Restore the C stack value in case it's been changed by a callback. *)
            (
                if stackSpaceRequired = 0w0
                then []
                else [AddImmediate{opSize=OpSize64, setFlags=false, regN=XSP, regD=XSP, immed=stackSpaceRequired, shifted=false}]
            ) @
            [
                (* Reload the ML stack pointer even though it's callee save.  If we've made a callback
                   the ML stack could have grown and so moved to a different address. *)
                LoadRegScaled{loadType=Load64, regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=mlStackPtrOffset},
                (* Load the heap allocation ptr and limit in case of a callback. *)
                LoadRegScaled{loadType=Load64, regT=X_MLHeapAllocPtr, regN=X_MLAssemblyInt, unitOffset=heapAllocPtrOffset},
                LoadRegScaled{loadType=Load64, regT=X_MLHeapLimit, regN=X_MLAssemblyInt, unitOffset=heapLimitPtrOffset}
            ] @ (* Store the result in the destination. *) getResult @
            (* Pop the return address and return. *)
            [ LoadRegUnscaled{regT=X30, regN=X_MLStackPtr, byteOffset= 8, loadType=Load64, unscaledType=PostIndex}, BranchReg{regD=X30,brRegType=BRRReturn} ]

        val functionName = "foreignCall"
        val debugSwitches =
            [(*Universal.tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
               Universal.tagInject Debug.assemblyCodeTag true*)]
        val closure = makeConstantClosure()
        val () = generateFinalCode{instrs=instructions, name=functionName, parameters=debugSwitches, resultClosure=closure,
                              profileObject=createProfileObject(), labelMaker=labelMaker}
    in
        closureAsAddress closure
    end

    (* Build a callback function.  The arguments are the abi, the list of argument types and the result type.
       The result is an ML function that takes an ML function, f, as its argument, registers it as a callback and
       returns the C function as its result.  When the C function is called the arguments are copied into
       temporary memory and the vector passed to f along with the address of the memory for the result.
       "f" stores the result in it when it returns and the result is then passed back as the result of the
       callback.
       N.B.  This returns a closure cell which contains the address of the code.  It can be used as a
       SysWord.word value except that while it exists the code will not be GCed.  *)
    fun buildCallBack(_: abi, args: cType list, result: cType): Address.machineWord =
    let
        val argWorkReg = X10 (* Used in loading arguments if necessary. *)
        and argWorkReg2 = X11
        and argWorkReg3 = X13
        and argWorkReg4 = X14
        
        val labelMaker = createLabelMaker()

        (* The stack contains a 32-byte result area then an aligned area for the arguments. *)

        (* Store the argument values to the structure that will be passed to the ML callback function. *)
        (* Note.  We've loaded the frame pointer with the original stack ptr-96 so we can
           access any stack arguments from that. *)
        fun moveArgs([], _, _, _, _, moveFromStack) = moveFromStack
        
        |   moveArgs(arg::args, stackSpace, argOffset, gRegNo, fpRegNo, moveFromStack) =
            let
                val {size, align, typeForm, ...} =  arg
                val newArgOffset = alignUp(argOffset, align)
            in
                case classifyArg(typeForm, size) of
                    ArgClassHFA(numItems, isDouble) =>
                        if fpRegNo + numItems <= 0w8
                        then
                        let
                            val scale = if isDouble then 0w8 else 0w4
                            (* Store the values from the FP registers. *)
                            fun storeFPRegs(0w0, _, _) = []
                            |   storeFPRegs(0w1, fpRegNo, offset) =
                                [StoreFPRegScaled{regT=VReg fpRegNo, regN=XSP, unitOffset=offset, floatSize=if isDouble then Double64 else Float32}]
                            |   storeFPRegs(n, fpRegNo, offset) =
                                StoreFPRegPair{regT1=VReg fpRegNo, regT2=VReg(fpRegNo+0w1), regN=XSP, unitOffset=offset,
                                                floatSize=if isDouble then Double64 else Float32, unscaledType=NoUpdate} ::
                                        storeFPRegs(n-0w2, fpRegNo+0w2, offset+2)
                        in
                            moveArgs(args, stackSpace, newArgOffset+size, gRegNo, fpRegNo+numItems,
                                storeFPRegs(numItems, fpRegNo, Word.toInt(newArgOffset div scale)) @ moveFromStack)
                        end
                        else
                        let
                            (* Load the arguments from the stack and store into the result area. *)
                            fun copyData(0w0, _, _) = []
                            |   copyData(n, dstOffset, stackOffset) =
                                if isDouble
                                then LoadRegScaled{loadType=Load64, regT=argWorkReg2, regN=X29, unitOffset=stackOffset} ::
                                     StoreRegScaled{loadType=Load64, regT=argWorkReg2, regN=XSP, unitOffset=dstOffset} ::
                                     copyData(n-0w1, dstOffset+1, stackOffset+1)
                                else LoadRegScaled{loadType=Load32, regT=argWorkReg2, regN=X29, unitOffset=stackOffset} ::
                                     StoreRegScaled{loadType=Load32, regT=argWorkReg2, regN=XSP, unitOffset=dstOffset} ::
                                     copyData(n-0w1, dstOffset+1, stackOffset+1)

                            val copyFromStack =
                                if isDouble
                                then copyData(numItems, Word.toInt(newArgOffset div 0w8), stackSpace)
                                else copyData(numItems, Word.toInt(newArgOffset div 0w4), stackSpace*2)
                            (* The overall size is rounded up to a multiple of 8 *)
                            val newStackOffset = stackSpace + Word.toInt(alignUp(size, 0w8) div 0w8)
                        in
                            moveArgs(args, newStackOffset, newArgOffset+size, gRegNo, 0w8,
                                copyFromStack @ moveFromStack)
                        end

                |   _ =>
                    if alignedLoadStore(newArgOffset, size) andalso
                        (gRegNo <= 0w6 orelse gRegNo = 0w7 andalso size <= 0w8)
                    then (* Usual case: argument passed in one or two registers. *)
                    (
                        if size > 0w8
                        then moveArgs(args, stackSpace, newArgOffset+size, gRegNo + 0w2, fpRegNo,
                                StoreRegPair{loadType=Load64, unscaledType=NoUpdate, regT1=XReg gRegNo, regT2=XReg(gRegNo+0w1), regN=XSP,
                                    unitOffset=Word.toInt(newArgOffset div 0w8)} :: moveFromStack)
                        else moveArgs(args, stackSpace, newArgOffset+size, gRegNo + 0w1, fpRegNo,
                                storeUpTo8(XReg gRegNo, XSP, Word.toInt newArgOffset, size) @ moveFromStack)
                    )
                    else
                    (* General case.  Store the argument registers if necessary and then use
                       a byte copy to copy into the argument area.  This sorts out any odd alignments or
                       lengths.  In some cases the source will be in memory already. *)
                    let
                        (* The source is either the register value or the value on the stack. *)
                        val (argRegNo, nextGReg, newStack, loadArg) =
                            if size > 0w16
                            then
                            (
                                if gRegNo < 0w8
                                then (XReg gRegNo, gRegNo + 0w1, stackSpace, [])
                                else (argWorkReg, 0w8, stackSpace+1, [LoadRegScaled{loadType=Load64, regT=argWorkReg, regN=X29, unitOffset=stackSpace}])
                            )
                            else
                            let
                                val regsNeeded = if size > 0w8 then 0w2 else 0w1
                            in
                                if gRegNo + regsNeeded <= 0w8
                                then (XReg gRegNo, gRegNo+regsNeeded, stackSpace,
                                        [if size > 0w8
                                         then StoreRegPair{loadType=Load64, unscaledType=NoUpdate, regT1=XReg gRegNo, regT2=XReg(gRegNo+0w1), regN=XSP, unitOffset=2}
                                         else StoreRegScaled{loadType=Load64, regT=XReg gRegNo, regN=XSP, unitOffset=2},
                                         AddImmediate{opSize=OpSize64, setFlags=false, regD=XReg gRegNo, regN=XSP, immed=0w16, shifted=false}])
                                else (* Being passed on the stack *)
                                    (argWorkReg, 0w8, stackSpace+Word8.toInt regsNeeded,
                                        [AddImmediate{opSize=OpSize64, setFlags=false, regD=argWorkReg, regN=X29, immed=Word.fromInt stackSpace*0w8, shifted=false}])
                            end

                        val loopLabel = createLabel labelMaker

                        val copyCode =
                        [ 
                            AddImmediate{opSize=OpSize64, setFlags=false, regN=argRegNo, regD=argWorkReg3, immed=size, shifted=false}, (* End of source area *)
                            AddImmediate{opSize=OpSize64, setFlags=false, regN=XSP, regD=argWorkReg2, immed=newArgOffset+size, shifted=false}, (* end of dest *)
                            SetLabel loopLabel,
                            LoadRegUnscaled{loadType=Load8, unscaledType=PreIndex, regT=argWorkReg4, regN=argWorkReg3, byteOffset= ~1},
                            StoreRegUnscaled{loadType=Load8, unscaledType=PreIndex, regT=argWorkReg4, regN=argWorkReg2, byteOffset= ~1},
                            SubShiftedReg{opSize=OpSize64, setFlags=true, regM=argWorkReg3, regN=argRegNo, regD=XZero, shift=ShiftNone}, (* At start? *)
                            ConditionalBranch(CondNotEqual, loopLabel)
                        ]
                    in
                        moveArgs(args, newStack, newArgOffset+size, nextGReg, fpRegNo, loadArg @ copyCode @ moveFromStack)
                    end
            end

        val copyArgsFromRegsAndStack =
            moveArgs(args, 12 (* Offset to first stack arg *), 0w32 (* Size of result area *), 0w0, 0w0, [])

        local
            fun getNextSize (arg, argOffset) =
            let val {size, align, ...} =  arg in alignUp(argOffset, align) + size end
        in
            val argumentSpace = alignUp(List.foldl getNextSize 0w0 args, 0w16)
        end

        local
            val {size, typeForm, ...} =  result
        in
            (* Load the results from the result area except that if we're
               passing the result structure by reference this is done by
               the caller.  Generally similar to how arguments are passed
               in a call. *)
            val (loadResults, resultByReference) =
                if typeForm = CTypeVoid
                then ([], false)
                else case classifyArg(typeForm, size) of
                    ArgClassHFA(numItems, isDouble) =>
                        let
                            (* Load the values to the floating point registers. *)
                            fun loadFPRegs(0w0, _, _) = []
                            |   loadFPRegs(0w1, fpRegNo, offset) =
                                [LoadFPRegScaled{regT=VReg fpRegNo, regN=XSP, unitOffset=offset, floatSize=if isDouble then Double64 else Float32}]
                            |   loadFPRegs(n, fpRegNo, offset) =
                                LoadFPRegPair{regT1=VReg fpRegNo, regT2=VReg(fpRegNo+0w1), regN=XSP, unitOffset=offset, unscaledType=NoUpdate,
                                                floatSize=if isDouble then Double64 else Float32} ::
                                        loadFPRegs(n-0w2, fpRegNo+0w2, offset+2)
                        in
                            (loadFPRegs(numItems, 0w0, 0 (* result area *)), false)
                        end

                |   ArgLargeStruct => ([], true) (* Structures larger than 16 bytes are passed by reference. *)

                |   _ =>
                    (* We've allocated a 32-byte area aligned onto a 16-byte boundary so
                       we can simply load one or two registers. *)
                    if size > 0w8
                    then ([LoadRegPair{loadType=Load64, unscaledType=NoUpdate, regT1=X0, regT2=X1, regN=XSP, unitOffset=0}], false)
                    else ([LoadRegScaled{loadType=Load64, regT=X0, regN=XSP, unitOffset=0}], false)
        end

        val instructions =
            [ (* Push LR, FP and the callee-save registers. *)
                StoreRegPair{loadType=Load64, unscaledType=PreIndex, regT1=X29, regT2=X30, regN=XSP, unitOffset= ~12},
                MoveXRegToXReg{sReg=XSP, dReg=X29},
                StoreRegPair{loadType=Load64, unscaledType=NoUpdate, regT1=X19, regT2=X20, regN=X29, unitOffset=2},
                StoreRegPair{loadType=Load64, unscaledType=NoUpdate, regT1=X21, regT2=X22, regN=X29, unitOffset=4},
                StoreRegPair{loadType=Load64, unscaledType=NoUpdate, regT1=X23, regT2=X24, regN=X29, unitOffset=6},
                StoreRegPair{loadType=Load64, unscaledType=NoUpdate, regT1=X25, regT2=X26, regN=X29, unitOffset=8},
                StoreRegPair{loadType=Load64, unscaledType=NoUpdate, regT1=X27, regT2=X28, regN=X29, unitOffset=10},
                (* Reserve space for the arguments and results. *)
                SubImmediate{opSize=OpSize64, setFlags=false, regN=XSP, regD=XSP, immed=argumentSpace+0w32, shifted=false},
                (* We passed the function we're calling in X9 but we need to move
                   it to a callee-save register before we call the RTS. *)
                MoveXRegToXReg{sReg=X9, dReg=X20}
            ] @
                (* Save X8 if we're going to need it. *)
            (if resultByReference then [StoreRegScaled{loadType=Load64, regT=X8, regN=XSP, unitOffset=0}] else []) @
            (* Now we've saved X24 we can move the global heap base into it. *)
            (case archType of ArchC32 _ => [MoveXRegToXReg{sReg=X10, dReg=X_Base32in64}] | ArchNative => []) @
            copyArgsFromRegsAndStack @
            [LoadAddr(X0, getThreadDataCall)] @
            (
                case archType of
                    ArchC32 shift =>
                        [AddShiftedReg{setFlags=false, opSize=OpSize64, regM=X0, regN=X_Base32in64, regD=X0,
                                       shift=ShiftLSL(wordToWord8 shift)}]
                |   ArchNative => []
            ) @
            [
                (* Call into the RTS to get the thread data ptr. *)
                LoadRegScaled{loadType=Load64, regT=X0, regN=X0, unitOffset=0},
                BranchReg{regD=X0, brRegType=BRRAndLink},
                MoveXRegToXReg{sReg=X0, dReg=X_MLAssemblyInt},
                (* Load the ML regs. *)
                LoadRegScaled{loadType=Load64, regT=X_MLHeapLimit, regN=X_MLAssemblyInt, unitOffset=heapLimitPtrOffset},
                LoadRegScaled{loadType=Load64, regT=X_MLHeapAllocPtr, regN=X_MLAssemblyInt, unitOffset=heapAllocPtrOffset},
                LoadRegScaled{loadType=Load64, regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=mlStackPtrOffset},
                (* Prepare the arguments.  They are both syswords so have to be boxed.
                   First load the address of the argument area which is after the
                   32-byte result area. *)
                AddImmediate{opSize=OpSize64, setFlags=false, regN=XSP, regD=X2, immed=0w32, shifted=false}
            ] @ List.rev(boxSysWord({source=X2, destination=X0, workReg=X3, saveRegs=[]}, [])) @ (* Address of arguments. *)
            (
                (* Result area pointer.  If we're returning by reference this is the original
                   value of X8 otherwise it's the address of the 32 bytes we've reserved. *)
                if resultByReference
                then [LoadRegScaled{loadType=Load64, regT=X2, regN=XSP, unitOffset=0}]
                else [MoveXRegToXReg{sReg=XSP, dReg=X2}]
            ) @ List.rev(boxSysWord({source=X2, destination=X1, workReg=X3, saveRegs=[]}, [])) @
                (* Put the ML closure pointer, originally in X9 now in X20, into the
                   ML closure pointer register, X8.  Then call the ML code. *)
            [MoveXRegToXReg{sReg=X20, dReg=X8}] @
            (
                case archType of
                    ArchC32 shift =>
                    [
                        AddShiftedReg{regM=X8, regN=X_Base32in64, regD=X16, shift=ShiftLSL(wordToWord8 shift),
                            opSize=OpSize64, setFlags=false},
                        LoadRegScaled{loadType=Load64, regT=X16, regN=X16, unitOffset=0}
                    ]
                |   ArchNative => [LoadRegScaled{loadType=Load64, regT=X16, regN=X8, unitOffset=0}]
            ) @
            [
                BranchReg{regD=X16, brRegType=BRRAndLink},
                (* Save the ML stack and heap pointers.  We could have allocated or
                   grown the stack.  The limit pointer is maintained by the RTS. *)
                StoreRegScaled{loadType=Load64, regT=X_MLHeapAllocPtr, regN=X_MLAssemblyInt, unitOffset=heapAllocPtrOffset},
                StoreRegScaled{loadType=Load64, regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=mlStackPtrOffset}
            ] @ loadResults @ (* Load the return values *)
            [
                (* Restore the callee-save registers and return. *)
                MoveXRegToXReg{sReg=X29, dReg=XSP},
                LoadRegPair{loadType=Load64, unscaledType=NoUpdate, regT1=X19, regT2=X20, regN=X29, unitOffset=2},
                LoadRegPair{loadType=Load64, unscaledType=NoUpdate, regT1=X21, regT2=X22, regN=X29, unitOffset=4},
                LoadRegPair{loadType=Load64, unscaledType=NoUpdate, regT1=X23, regT2=X24, regN=X29, unitOffset=6},
                LoadRegPair{loadType=Load64, unscaledType=NoUpdate, regT1=X25, regT2=X26, regN=X29, unitOffset=8},
                LoadRegPair{loadType=Load64, unscaledType=NoUpdate, regT1=X27, regT2=X28, regN=X29, unitOffset=10},
                LoadRegPair{loadType=Load64, unscaledType=PostIndex, regT1=X29, regT2=X30, regN=XSP, unitOffset=12},
                BranchReg{regD=X30, brRegType=BRRReturn}
            ]

        val functionName = "foreignCallBack(2)"
        val debugSwitches =
            [(*Universal.tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
               Universal.tagInject Debug.assemblyCodeTag true*)]

        val closure = makeConstantClosure()
        val () = generateFinalCode{instrs=instructions, name=functionName, parameters=debugSwitches,
                              resultClosure=closure, profileObject=createProfileObject(), labelMaker=labelMaker}
        val stage2Code = closureAsAddress closure
        
        fun resultFunction f =
        let
            (* Generate a small function to load the address of f into a register and then jump to stage2.
               The idea is that it should be possible to generate this eventually in a single RTS call.
               That could be done by using a version of this as a model. *)
            val instructions =
                case archType of
                    ArchC32 shift =>
                    (* Get the global heap base into X10. *)
                    [
                        LoadGlobalHeapBaseInCallback X10,
                        LoadAddr(X9, Address.toMachineWord f),
                        (* Have to load the actual address at run-time. *)
                        LoadAddr(X16, stage2Code),
                        AddShiftedReg{setFlags=false, opSize=OpSize64, regM=X16, regN=X10, regD=X16,
                            shift=ShiftLSL(wordToWord8 shift)},
                        LoadRegScaled{loadType=Load64, regT=X16, regN=X16, unitOffset=0},
                        BranchReg{regD=X16, brRegType=BRRBranch}
                    ]
                |   ArchNative =>
                    let
                        (* We can extract the actual code address in the native address version. *)
                        val codeAddress = Address.loadWord(Address.toAddress stage2Code, 0w0)
                    in
                        [
                            LoadAddr(X9, Address.toMachineWord f),
                            LoadAddr(X16, codeAddress),
                            BranchReg{regD=X16, brRegType=BRRBranch}
                        ]
                    end
            val functionName = "foreignCallBack(1)"
            val debugSwitches =
                [(*Universal.tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
                   Universal.tagInject Debug.assemblyCodeTag true*)]
            val closure = makeConstantClosure()
            val () = generateFinalCode{instrs=instructions, name=functionName, parameters=debugSwitches,
                                  resultClosure=closure, profileObject=createProfileObject(),
                                  labelMaker=createLabelMaker()}
            val res = closureAsAddress closure
            (*val _ = print("Address is " ^ (LargeWord.toString(RunCall.unsafeCast res)) ^ "\n")*)
        in
            res
        end
    in
        Address.toMachineWord resultFunction
    end

end;
