(*
    Copyright David C. J. Matthews 2021-3

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

functor Arm64ICodeToArm64Code(
    structure Arm64PreAssembly: ARM64PREASSEMBLY
    structure Debug: DEBUG
    structure Arm64ICode: ARM64ICODE
    structure Identify: ARM64IDENTIFYREFERENCES
    structure IntSet: INTSET
    structure Pretty: PRETTY

    structure Strongly:
        sig
            val stronglyConnectedComponents: {nodeAddress: 'a -> int, arcs: 'a -> int list } -> 'a list -> 'a list list
        end
    
    sharing Arm64PreAssembly.Sharing = Arm64ICode.Sharing = Identify.Sharing = IntSet
): ARM64ICODEGENERATE =
struct
    open Identify
    open Arm64ICode
    open Arm64PreAssembly
    open Address
    exception InternalError = Misc.InternalError

    (* Reversed cons and append to make the code easier to read. *)
    infix 5 <::> <@>
    fun tl <::> hd = hd :: tl
    (*and snd <@> fst = fst @ snd*)

    (* These aren't currently used for anything. *)
    val workReg1 = X16 and workReg2 = X17
    
    val wordToWord8 = Word8.fromLarge o Word.toLarge
    
    val is32in64 = case archType of ArchC32 _ => true | ArchNative => false

    fun icodeToArm64Code {blocks: basicBlockConcrete vector, functionName, stackRequired, debugSwitches, resultClosure, profileObject, ...} =
    let
        val numBlocks = Vector.length blocks

        (* Load from and store to stack. *)
        fun loadFromStack(destReg, wordOffset, code) =
            if wordOffset >= 4096
            then (LoadRegIndexed{regT=destReg, regN=X_MLStackPtr, regM=destReg, option=ExtUXTX ScaleOrShift, loadType=Load64}) ::
                    [LoadNonAddr(destReg, Word64.fromInt wordOffset)] @ code
            else (LoadRegScaled{regT=destReg, regN=X_MLStackPtr, unitOffset=wordOffset, loadType=Load64}) :: code

        and storeToStack(sourceReg, wordOffset, workReg, code) =
            if wordOffset >= 4096
            then (StoreRegIndexed{regT=sourceReg, regN=X_MLStackPtr, regM=workReg, option=ExtUXTX ScaleOrShift, loadType=Load64}) ::
                    [LoadNonAddr(workReg, Word64.fromInt wordOffset)] @ code
            else (StoreRegScaled{regT=sourceReg, regN=X_MLStackPtr, unitOffset=wordOffset, loadType=Load64}) :: code

        datatype srcAndDest = IsInReg of xReg | IsOnStack of int

        local
            (* The registers are numbered from 0.  Choose values that don't conflict with
               the stack addresses. *)
            fun regNo(XReg r) = ~1 - Word8.toInt r | regNo _ = ~1 - 31
            type node = {src: srcAndDest, dst: srcAndDest }
            
            fun nodeAddress({dst=IsInReg r, ...}: node) = regNo r
            |   nodeAddress({dst=IsOnStack a, ...}) = a
            
            fun arcs({src=IsOnStack wordOffset, ...}: node) = [wordOffset]
            |   arcs{src=IsInReg r, ...} = [regNo r]
        in
            val stronglyConnected = Strongly.stronglyConnectedComponents { nodeAddress=nodeAddress, arcs=arcs }
        end
        
        (* This is a general function for moving values into registers or to the stack
           where it is possible that the source values might also be in use as destinations.
           The stack is used for destinations only for tail recursive calls. *)
        fun moveMultipleValues(moves, code) =
        let
            fun moveValues ([], code) = code (* We're done. *)

            |   moveValues (arguments, code) =
                let
                    (* stronglyConnectedComponents does two things.  It detects loops where
                       it's not possible to move items without breaking the loop but more
                       importantly it orders the dependencies so that if there are no loops we
                       can load the source and store it in the destination knowing that
                       we won't overwrite anything we might later need. *)
                    
                    val ordered = stronglyConnected arguments
                    
                    fun loadIntoReg(IsInReg sReg, dReg, code) =
                            if sReg = dReg then code else (MoveXRegToXReg{sReg=sReg, dReg=dReg}) :: code
                    |   loadIntoReg(IsOnStack wordOffset, dReg, code) =
                            loadFromStack(dReg, wordOffset, code)

                    fun moveEachValue ([], code) = code

                    |   moveEachValue ([{dst=IsInReg dReg, src}] :: rest, code) =
                            moveEachValue(rest, loadIntoReg(src, dReg, code))

                    |   moveEachValue ([{dst=IsOnStack wordOffset, src=IsInReg sReg}] :: rest, code) =
                            (* Storing into the stack. *)
                            moveEachValue(rest, storeToStack(sReg, wordOffset, workReg1, code))

                    |   moveEachValue ([{dst=IsOnStack dstOffset, src=IsOnStack srcOffset}] :: rest, code) =
                            (* Copy a stack location - needs a load and store unless the address is the same. *)
                            if dstOffset = srcOffset
                            then moveEachValue(rest, code)
                            else moveEachValue(rest,
                                    storeToStack(workReg2, dstOffset, workReg1,
                                        loadFromStack(workReg2, srcOffset, code)))

                    |   moveEachValue((cycle as first :: _ :: _) :: rest, code) =
                        (* We have a cycle. *)
                        let
                            (* We need to exchange some of the arguments.  Doing an exchange here will
                               set the destination with the correct source.  However we have to process
                               every subsequent entry with the swapped registers.  That may well mean that
                               one of those entries becomes trivial.
                               We also need to rerun stronglyConnectedComponents on at least the rest of
                               this cycle.  It's easiest to flatten the rest and do everything. *)
                            (* Exchange the source and destination.  We don't have an exchange instruction
                               and there's a further complication.  We could be copying between stack locations
                               and their offsets could be > 4096.  Since we've only got two work registers
                               we need to use the hardware stack as an extra location. Stack-stack
                               exchange is very rare so the extra overhead to handle the general case
                               is worth it. *)
                            local
                                fun storeToDest(sReg, IsInReg dReg, _, code) =
                                        (MoveXRegToXReg{sReg=sReg, dReg=dReg}) :: code
                                |   storeToDest(sReg, IsOnStack wordOffset, work, code) =
                                        storeToStack(sReg, wordOffset, work, code)
                            in
                                fun exchange(IsInReg arg1Reg, arg2, code) =
                                    (MoveXRegToXReg{sReg=workReg2, dReg=arg1Reg}) ::
                                    storeToDest(arg1Reg, arg2, workReg1,
                                        loadIntoReg(arg2, workReg2, code))

                                |   exchange(arg1, IsInReg arg2Reg, code) =
                                    (MoveXRegToXReg{sReg=workReg2, dReg=arg2Reg}) ::
                                    storeToDest(arg2Reg, arg1, workReg1,
                                        loadIntoReg(arg1, workReg2, code))

                                |   exchange(arg1, arg2, code) =
                                    (* The hardware stack must be 16-byte aligned. *)
                                    storeToDest(workReg2, arg2, workReg1,
                                        (LoadRegUnscaled{regT=workReg2, regN=XSP, byteOffset=16, loadType=Load64, unscaledType=PostIndex}) ::
                                        storeToDest(workReg2, arg1, workReg1,
                                            loadIntoReg(arg2, workReg2,
                                                (StoreRegUnscaled{regT=workReg2, regN=XSP, byteOffset= ~16, loadType=Load64, unscaledType=PreIndex}) ::
                                                    loadIntoReg(arg1, workReg2, code))))
                            end
                            
                            (* Try to find either a register-register move or a register-stack move.
                               If not use the first.  If there's a stack-register move there will
                               also be a register-stack so we don't need to look for both. *)
                               
                            val {dst=selectDst, src=selectSrc} = first

                            (* This includes this entry but after the swap we'll eliminate it. *)
                            val flattened = List.foldl(fn (a, b) => a @ b) [] (cycle :: rest)
                            val destAsSource = selectDst

                            fun match(s1: srcAndDest, s2) = s1 = s2
                            
                            fun swapSources{src, dst} =
                                if match(src, selectSrc) then {src=destAsSource, dst=dst}
                                else if match(src, destAsSource) then {src=selectSrc, dst=dst}
                                else {src=src, dst=dst}

                            val exchangeCode = exchange(selectDst, selectSrc, code)                                    
                        in
                            moveValues(List.map swapSources flattened, exchangeCode)
                        end

                    |   moveEachValue(([]) :: _, _) = (* This should not happen - avoid warning. *)
                            raise InternalError "moveEachValue - empty set"
                in
                    moveEachValue(ordered, code)
                end
        in
            moveValues(moves, code)
        end

        (* Where we have multiple specific registers as either source or
           destination there is the potential that a destination register
           if currently in use as a source. *) 
        fun moveMultipleRegisters(regPairList, code) =
        let
            val regPairsAsDests =
                List.map(fn {src, dst} => {src=IsInReg src, dst=IsInReg dst}) regPairList
        in
            moveMultipleValues(regPairsAsDests, code)
        end

        (* Floating point registers can only be moved into other floating point registers but
           it is possible to have a cycle. *)
        fun moveMultipleFPRegisters(moves: {dst: vReg, src: vReg} list, code) =
        let
            local
                fun regNo(VReg r) = Word8.toInt r
                type node = {src: vReg, dst: vReg }
                fun nodeAddress({dst=r, ...}: node) = regNo r
                fun arcs{src=r, ...} = [regNo r]
            in
                val stronglyConnected = Strongly.stronglyConnectedComponents { nodeAddress=nodeAddress, arcs=arcs }
            end

            fun moveValues ([], code) = code (* We're done. *)

            |   moveValues (arguments, code) =
                let
                    val ordered = stronglyConnected arguments

                    fun moveEachValue ([], code) = code

                    |   moveEachValue ([{dst, src}] :: rest, code) =
                            moveEachValue(rest,
                                if src = dst then code else (FPUnaryOp{regN=src, regD=dst, fpOp=MoveDouble}) :: code)

                    |   moveEachValue((cycle as first :: _ :: _) :: rest, code) =
                        (* We have a cycle. *)
                        let
                            val {dst=selectDst: vReg, src=selectSrc: vReg} = first

                            (* This includes this entry but after the swap we'll eliminate it. *)
                            val flattened = List.foldl(fn (a, b) => a @ b) [] (cycle :: rest)
                            val destAsSource = selectDst
                            fun swapSources{src, dst} =
                                if src=selectSrc then {src=destAsSource, dst=dst}
                                else if src=destAsSource then {src=selectSrc, dst=dst}
                                else {src=src, dst=dst}

                            (* Exchange the values of two floating point registers.  There are
                               various ways to do this.  For the moment just use the hardware stack. *)
                            val exchangeCode =
                                code <::>
                                    StoreFPRegUnscaled{regT=selectDst, regN=XSP, byteOffset= ~16, floatSize=Double64, unscaledType=PreIndex} <::>
                                    FPUnaryOp{regN=selectSrc, regD=selectDst, fpOp=MoveDouble} <::>
                                    LoadFPRegUnscaled{regT=selectSrc, regN=XSP, byteOffset=16, floatSize=Double64, unscaledType=PostIndex}
                        in
                            moveValues(List.map swapSources flattened, exchangeCode)
                        end

                    |   moveEachValue(([]) :: _, _) = (* This should not happen - avoid warning. *)
                            raise InternalError "moveEachValue - empty set"
                in
                    moveEachValue(ordered, code)
                end
        in
            moveValues(moves, code)
        end



        fun moveIfNecessary({src, dst}, code) =
            if src = dst then code
            else MoveXRegToXReg{sReg=src, dReg=dst} :: code

        (* Add a constant word to the source register and put the result in the
           destination.  regW is used as a work register if necessary.  This is used
           both for addition and subtraction. *)
        fun addConstantWord({regS, regD, value=0w0, ...}, code) =
            if regS = regD then code else MoveXRegToXReg{sReg=regS, dReg=regD} :: code
    
        |   addConstantWord({regS, regD, regW, value}, code) =
            let
                (* If we have to load the constant it's better if the top 32-bits are
                   zero if possible. *)
                val (isSub, unsigned) =
                    if value > Word64.<<(0w1, 0w63)
                    then (true, ~ value)
                    else (false, value)
            in
                if unsigned < Word64.<<(0w1, 0w24)
                then (* We can put up to 24 in a shifted and an unshifted constant. *)
                let
                    val w = Word.fromLarge(Word64.toLarge unsigned)
                    val high = Word.andb(Word.>>(w, 0w12), 0wxfff)
                    val low = Word.andb(w, 0wxfff)
                    val addSub = if isSub then SubImmediate else AddImmediate
                in
                    if high <> 0w0
                    then
                    (
                        (if low <> 0w0 then [addSub{regN=regD, regD=regD, immed=low, shifted=false, setFlags=false, opSize=OpSize64}] else []) @
                        addSub{regN=regS, regD=regD, immed=high, shifted=true, setFlags=false, opSize=OpSize64} :: code
                    )
                    else addSub{regN=regS, regD=regD, immed=low, shifted=false, setFlags=false, opSize=OpSize64} :: code
                end
                else
                let
                    (* To minimise the constant and increase the chances that it
                       will fit in a single word look to see if we can shift it. *)
                    fun getShift(value, shift) =
                        if Word64.andb(value, 0w1) = 0w0
                        then getShift(Word64.>>(value, 0w1), shift+0w1)
                        else (value, shift)
                    val (shifted, shift) = getShift(unsigned, 0w0)
                in
                    code <::> LoadNonAddr(regW, shifted) <::>
                        (if isSub then SubShiftedReg else AddShiftedReg)
                            {regM=regW, regN=regS, regD=regD, shift=ShiftLSL shift, setFlags=false, opSize=OpSize64}
                end
            end

        val labelMaker = createLabelMaker()

        val startOfFunctionLabel = createLabel labelMaker (* Used for recursive calls/jumps *)

        val blockToLabelMap = Vector.tabulate(numBlocks, fn _ => createLabel labelMaker)

        fun getBlockLabel blockNo = Vector.sub(blockToLabelMap, blockNo)

        fun codeExtended _ (MoveRegister{source, dest, ...}, code) =
            moveIfNecessary({src=source, dst=dest}, code)

        |   codeExtended _ (LoadNonAddressConstant{source, dest}, code) =
                code <::> LoadNonAddr(dest, source)

        |   codeExtended _ (LoadFPConstant{source, dest, floatSize}, code) =
                code <::> LoadFPConst{dest=dest, value=source, floatSize=floatSize, work=workReg1}

        |   codeExtended _ (LoadAddressConstant{source, dest, ...}, code) =
                code <::> LoadAddr(dest, source)

        |   codeExtended _ (LoadWithConstantOffset{dest, base, byteOffset, loadType, ...}, code) =
            if byteOffset < 0
            then LoadRegUnscaled{regT=dest, regN=base, byteOffset=byteOffset,
                                        loadType=loadType, unscaledType=NoUpdate} :: code
            else
            let
                val unitOffset =
                    case loadType of
                        Load64 => Int.quot(byteOffset, 8)
                    |   Load32 => Int.quot(byteOffset, 4)
                    |   Load16 => Int.quot(byteOffset, 2)
                    |   Load8 => byteOffset
            in
                LoadRegScaled{regT=dest, regN=base, unitOffset=unitOffset, loadType=loadType} :: code
            end

        |   codeExtended _ (LoadFPWithConstantOffset{dest, base, byteOffset, floatSize, ...}, code) =
            if byteOffset < 0
            then (LoadFPRegUnscaled{regT=dest, regN=base, byteOffset=byteOffset,
                                        floatSize=floatSize, unscaledType=NoUpdate}) :: code
            else
            let
                val unitOffset = Int.quot(byteOffset, case floatSize of Float32 => 4 | Double64 => 8)
            in
                (LoadFPRegScaled{regT=dest, regN=base, unitOffset=unitOffset, floatSize=floatSize}) :: code
            end

        |   codeExtended _ (LoadWithIndexedOffset{dest, base, index, loadType, signExtendIndex, ...}, code) =
            let
                val baseReg = base and indexReg = index
                (* In 32-in-64 the index is a 32-bit value in the low order 32-bits.  It may be
                   signed if it is a C address. *)
                val scaleType = if is32in64 andalso signExtendIndex then ExtSXTW else ExtUXTX
                val scale = case loadType of Load8 => NoScale | _ => ScaleOrShift
            in
                (LoadRegIndexed{regT=dest, regN=baseReg, regM=indexReg, option=scaleType scale, loadType=loadType}) :: code
            end

        |   codeExtended _ (LoadFPWithIndexedOffset{dest, base, index, floatSize, signExtendIndex, ...}, code) =
            let
                val baseReg = base and indexReg = index
                (* In 32-in-64 the index is a 32-bit value in the low order 32-bits.  It may be
                   signed if it is a C address. *)
                val scaleType = if is32in64 andalso signExtendIndex then ExtSXTW else ExtUXTX
            in
                (LoadFPRegIndexed{regT=dest, regN=baseReg, regM=indexReg, option=scaleType ScaleOrShift, floatSize=floatSize}) :: code
            end

        |   codeExtended _ (GetThreadId { dest}, code) =
                (* Load the thread id.  This is always a 64-bit value. *)
                (LoadRegScaled{regT=dest, regN=X_MLAssemblyInt, unitOffset=threadIdOffset, loadType=Load64}) :: code

        |   codeExtended _ (ObjectIndexAddressToAbsolute{source, dest, ...}, code) =
            let
                val shift = case archType of ArchC32 shift => shift | ArchNative => raise InternalError "ObjectIndexAddressToAbsolute"
            in
                (AddShiftedReg{regM=source, regN=X_Base32in64, regD=dest,
                                      shift=ShiftLSL(wordToWord8 shift), setFlags=false, opSize=OpSize64}) :: code
            end

        |   codeExtended _ (AbsoluteToObjectIndex{source, dest, ...}, code) =
            let
                val destReg = dest
                val shift = case archType of ArchC32 shift => shift | ArchNative => raise InternalError "ObjectIndexAddressToAbsolute"
            in
                code <::>
                (SubShiftedReg{regM=X_Base32in64, regN=source, regD=destReg, shift=ShiftNone, setFlags=false, opSize=OpSize64}) <::>
                (shiftConstant{shift=shift, regN=destReg, regD=destReg, direction=ShiftRightLogical, opSize=OpSize64})
            end

        |   codeExtended _ (AllocateMemoryFixed{ bytesRequired, dest, saveRegs={gcUpdate=saveRegs, ...} }, code) =
                code <::> AllocateMemoryFixedSize{ bytes=Word.fromLarge bytesRequired, dest=dest, save=saveRegs, work=workReg1 }

        |   codeExtended _ (AllocateMemoryVariable{ size, dest, saveRegs={gcUpdate=saveRegs, ...} }, code) =
                code <::> AllocateMemoryVariableSize{ sizeReg=size, dest=dest, save=saveRegs, work=workReg1 }

        |   codeExtended _ (InitialiseMem{ size, addr, init}, code) =
            let
                val sizeReg = size
                and addrReg = addr
                and initReg = init
                val exitLabel = createLabel labelMaker and loopLabel = createLabel labelMaker
                (* This uses a loop to initialise.  It's possible the size is
                   zero so we have to check at the top of the loop. *)
                val (bShift, offset, loadType) = if is32in64 then (0w2, ~4, Load32) else (0w3, ~8, Load64)
            in
                code <::>
                (* Add the length in bytes so we point at the end. *)
                AddShiftedReg{regM=sizeReg, regN=addrReg, regD=workReg1, shift=ShiftLSL bShift, setFlags=false, opSize=OpSize64} <::>
                SetLabel loopLabel <::>
                (* Are we at the start? *)
                SubShiftedReg{regM=workReg1, regN=addrReg, regD=XZero, shift=ShiftNone, setFlags=true, opSize=OpSize64} <::>
                ConditionalBranch(CondEqual, exitLabel) <::>
                StoreRegUnscaled{regT=initReg, regN=workReg1, byteOffset=offset, loadType=loadType, unscaledType=PreIndex } <::>
                UnconditionalBranch loopLabel <::>
                SetLabel exitLabel
            end

        |   codeExtended _ (BeginLoop, code) = code

        |   codeExtended _ (JumpLoop{regArgs, stackArgs, checkInterrupt}, code) =
            let
                (* TODO: We could have a single list and use ArgOnStack and ArgInReg to distinguish. *)
                fun convertArg(ArgOnStack{wordOffset, ...}) = IsOnStack wordOffset
                |   convertArg(ArgInReg reg) = IsInReg(reg)
                val extStackArgs =
                    map (fn {wordOffset, src, ...} => {src=convertArg src, dst=IsOnStack wordOffset}) stackArgs
                val extRegArgs =
                    map (fn {dst, src} => {src=convertArg src, dst=convertArg(ArgInReg dst)}) regArgs
                val code2 = moveMultipleValues(extStackArgs @ extRegArgs, code)
            in
                case checkInterrupt of
                    NONE => code2
                |   SOME {gcUpdate=saveRegs, ...} =>
                    let
                        val skipCheck = createLabel labelMaker
                    in
                        code2 <::>
                        (* Put in stack-check code to allow this to be interrupted. *)
                        LoadRegScaled{regT=workReg1, regN=X_MLAssemblyInt, unitOffset=stackLimitOffset, loadType=Load64} <::>
                        SubShiftedReg{regM=workReg1, regN=X_MLStackPtr, regD=XZero, shift=ShiftNone, setFlags=true, opSize=OpSize64} <::>
                        ConditionalBranch(CondCarrySet, skipCheck) <::>
                        RTSTrap{rtsEntry=stackOverflowCallOffset, work=workReg1, save=saveRegs} <::>
                        SetLabel skipCheck
                    end
            end

        |   codeExtended _ (StoreWithConstantOffset{source, base, byteOffset, loadType, ...}, code) =
            if byteOffset < 0
            then (StoreRegUnscaled{regT=source, regN=base, byteOffset=byteOffset,
                                        loadType=loadType, unscaledType=NoUpdate}) :: code
            else
            let
                val unitOffset =
                    case loadType of
                        Load64 => Int.quot(byteOffset, 8)
                    |   Load32 => Int.quot(byteOffset, 4)
                    |   Load16 => Int.quot(byteOffset, 2)
                    |   Load8 => byteOffset
            in
                (StoreRegScaled{regT=source, regN=base, unitOffset=unitOffset, loadType=loadType}) :: code
            end

        |   codeExtended _ (StoreFPWithConstantOffset{source, base, byteOffset, floatSize, ...}, code) =
            if byteOffset < 0
            then (StoreFPRegUnscaled{regT=source, regN=base, byteOffset=byteOffset,
                                        floatSize=floatSize, unscaledType=NoUpdate}) :: code
            else
            let
                val unitOffset = Int.quot(byteOffset, case floatSize of Float32 => 4 | Double64 => 8)
            in
                (StoreFPRegScaled{regT=source, regN=base, unitOffset=unitOffset, floatSize=floatSize}) :: code
            end

        |   codeExtended _ (StoreWithIndexedOffset{source, base, index, loadType, signExtendIndex, ...}, code) =
            let
                val baseReg = base and indexReg = index
                (* In 32-in-64 the index is a 32-bit value in the low order 32-bits.  It may be
                   signed if it is a C address. *)
                val scaleType = if is32in64 andalso signExtendIndex then ExtSXTW else ExtUXTX
                val scale = case loadType of Load8 => NoScale | _ => ScaleOrShift
            in
                (StoreRegIndexed{regT=source, regN=baseReg, regM=indexReg, option=scaleType scale, loadType=loadType}) :: code
            end

        |   codeExtended _ (StoreFPWithIndexedOffset{source, base, index, floatSize, signExtendIndex, ...}, code) =
            let
                val baseReg = base and indexReg = index
                (* In 32-in-64 the index is a 32-bit value in the low order 32-bits.  It may be
                   signed if it is a C address. *)
                val scaleType = if is32in64 andalso signExtendIndex then ExtSXTW else ExtUXTX
            in
                (StoreFPRegIndexed{regT=source, regN=baseReg, regM=indexReg, option=scaleType ScaleOrShift, floatSize=floatSize}) :: code
            end

        |   codeExtended _ (AddSubImmediate{ source, dest, immed, isAdd, length, ccRef}, code) =
            let
                val destReg = dest
            in
                ((if isAdd then AddImmediate else SubImmediate)
                    {regN=source, regD=destReg, immed=immed, shifted=false, opSize=length, setFlags=isSome ccRef}) :: code
            end

        |   codeExtended _ (AddSubRegister{ base, shifted, dest, isAdd, length, ccRef, shift}, code) =
            let
                val destReg = dest
            in
                (
                    (if isAdd then AddShiftedReg else SubShiftedReg)
                        {regN=base, regM=shifted, regD=destReg, shift=shift, opSize=length, setFlags=isSome ccRef}) :: code
            end           

        |   codeExtended _ (LogicalImmediate{ source, dest, immed, logOp, length, ccRef}, code) =
            let
                val destReg = dest
            in
                (BitwiseLogical{regN=source, regD=destReg, bits=immed, opSize=length,
                                       logOp=logOp, setFlags=isSome ccRef}) :: code
            end

        |   codeExtended _ (LogicalRegister{ base, shifted, dest, logOp, length, ccRef, shift}, code) =
            let
                (* There are also versions of AND/OR/XOR which operate on a complement (NOT)
                   of the shifted register.  It's probably not worth looking for a use for them. *)
                val destReg = dest
            in
                (LogicalShiftedReg{regN=base, regM=shifted, regD=destReg,
                                          shift=shift, opSize=length, logOp=logOp, setFlags=isSome ccRef}) :: code
            end

        |   codeExtended _ (ShiftRegister{ direction, dest, source, shift, opSize }, code) =
                (ShiftRegisterVariable{regN=source, regM=shift, regD=dest,
                                              shiftDirection=direction, opSize=opSize}) :: code

        |   codeExtended _ (CountLeadingZeroBits{ dest, source, opSize }, code) =
                CountLeadingZeros{regN=source, regD=dest, opSize=opSize} :: code

        |   codeExtended _ (Multiplication{ kind, dest, sourceA, sourceM, sourceN }, code) =
            let
                val destReg = dest
                and srcAReg = sourceA
                and srcNReg = sourceN
                and srcMReg = sourceM
            in
                (MultiplyAndAddSub{regM=srcMReg, regN=srcNReg, regA=srcAReg, regD=destReg, multKind=kind}) :: code
            end

        |   codeExtended _ (Division{ isSigned, dest, dividend, divisor, opSize }, code) =
                (DivideRegs{regN=dividend, regM=divisor,
                                   regD=dest, isSigned=isSigned, opSize=opSize}) :: code

        |   codeExtended _ (BeginFunction{regArgs, fpRegArgs, ...}, code) =
            let
                val skipCheck = createLabel labelMaker
                val defaultWords = 10 (* This is wired into the RTS. *)
                val workRegister = workReg1
                val debugTrapAlways = false (* Can be set to true for debugging *)

                (* Test with either the stack-pointer or a high-water value.
                   The RTS assumes that X9 has been used as the high-water if it is
                   called through stackOverflowXCallOffset rather than stackOverflowCallOffset *)
                val (testReg, entryPt, code1) =
                    if stackRequired <= defaultWords
                    then (X_MLStackPtr, stackOverflowCallOffset, code)
                    else (X9, stackOverflowXCallOffset,
                            addConstantWord({regS=X_MLStackPtr, regD=X9, regW=workRegister,
                            value= ~ (Word64.fromLarge(Word.toLarge nativeWordSize)) * Word64.fromInt stackRequired}, code))

                (* Skip the RTS call if there is enough stack.  N.B. The RTS can modify the end-of-stack value
                   to force a trap here even if there is really enough stack. *)
                val code2 =
                    (if debugTrapAlways then []
                     else
                     [ConditionalBranch(CondCarrySet, skipCheck),
                      SubShiftedReg{regM=workRegister, regN=testReg, regD=XZero, shift=ShiftNone, setFlags=true, opSize=OpSize64}]) @
                    (* Load the end-of-stack value. *)
                    LoadRegScaled{regT=workRegister, regN=X_MLAssemblyInt, unitOffset=stackLimitOffset, loadType=Load64} :: code1

                val code3 =
                    code2 <::>
                        RTSTrap{rtsEntry=entryPt, work=workReg1, save=List.map #2 regArgs} <::>
                        SetLabel skipCheck

                fun mkPair(pr, rr) = {src=rr,dst=pr}
                val regPairs = List.map mkPair regArgs
                val fpRegPairs = List.map mkPair fpRegArgs
            in
                moveMultipleFPRegisters(fpRegPairs, moveMultipleRegisters(regPairs, code3))
            end

        |   codeExtended _ (TailRecursiveCall{callKind, regArgs, stackArgs, fpRegArgs, stackAdjust, currStackSize}, code) =
            let
                fun convertArg(ArgOnStack{wordOffset, ...}) = IsOnStack wordOffset
                |   convertArg(ArgInReg reg) = IsInReg(reg)

                val extStackArgs = map (fn {stack, src} => {dst=IsOnStack(stack+currStackSize), src=convertArg src}) stackArgs
                val extRegArgs = map (fn (a, r) => {src=convertArg a, dst=IsInReg r}) regArgs
                val extFPArgs = map (fn (a, r) => {src=a, dst=r}) fpRegArgs

                (* Tail recursive calls are complicated because we generally have to overwrite the existing stack.
                   That means storing the arguments in the right order to avoid overwriting a
                   value that we are using for a different argument. *)
                fun codeTailCall(arguments, stackAdjust, code) =
                    if stackAdjust < 0
                    then
                    let
                        (* If the function we're calling takes more arguments on the stack than the
                           current function we will have to extend the stack.  Do that by pushing the
                           argument whose offset is at -1.  Then adjust all the offsets and repeat. *)
                        val {src=argM1, ...} = valOf(List.find(fn {dst=IsOnStack ~1, ...} => true | _ => false) arguments)
                        fun renumberArgs [] = []
                        |   renumberArgs ({dst=IsOnStack ~1, ...} :: args) = renumberArgs args (* Remove the one we've done. *)
                        |   renumberArgs ({dst, src} :: args) =
                            let
                                val newDest = case dst of IsOnStack d => IsOnStack(d+1) | regDest => regDest
                                val newSrc =
                                    case src of
                                        IsOnStack wordOffset => IsOnStack(wordOffset+1)
                                    |   other => other
                            in
                                {dst=newDest, src=newSrc} :: renumberArgs args
                            end
                        val pushCode =
                            case argM1 of
                                IsOnStack wordOffset =>
                                    (StoreRegUnscaled{regT=workReg2, regN=X_MLStackPtr, byteOffset= ~8, unscaledType=PreIndex, loadType=Load64}) ::
                                        loadFromStack(workReg2, wordOffset, code)
                            |   IsInReg reg =>
                                    (StoreRegUnscaled{regT=reg, regN=X_MLStackPtr, byteOffset= ~8, unscaledType=PreIndex, loadType=Load64}) :: code
                                
                    in
                        codeTailCall(renumberArgs arguments, stackAdjust+1, pushCode)
                    end
                    else
                    let
                        val loadArgs =
                            moveMultipleFPRegisters(extFPArgs, moveMultipleValues(arguments, code))
                    in
                        if stackAdjust = 0
                        then loadArgs
                        else addConstantWord({regS=X_MLStackPtr, regD=X_MLStackPtr, regW=workReg1,
                                value=Word64.fromInt stackAdjust * Word.toLarge nativeWordSize}, loadArgs)
                    end
                val setArgumentsCode = codeTailCall(extStackArgs @ extRegArgs, stackAdjust+currStackSize, code)
                
                val jumpToFunctionCode =
                    case callKind of
                        Recursive => [(UnconditionalBranch startOfFunctionLabel)]
                    |   ConstantCode m =>
                        (
                            case archType of
                                ArchC32 _ => raise InternalError "ConstantCode" (* Not currently *)
                            |   ArchNative => [(BranchReg{regD=workReg1, brRegType=BRRBranch}), (LoadAddr(workReg1, m))]
                        )
                    |   FullCall =>
                        (
                            case archType of
                                ArchC32 shift =>
                                    [BranchReg{regD=workReg1, brRegType=BRRBranch},
                                     LoadRegScaled{regT=workReg1, regN=workReg1, unitOffset=0, loadType=Load64},
                                     AddShiftedReg{regM=X8, regN=X_Base32in64, regD=workReg1, shift=ShiftLSL(wordToWord8 shift),
                                        setFlags=false, opSize=OpSize64}]
                            |   ArchNative =>
                                    [BranchReg{regD=workReg1, brRegType=BRRBranch},
                                        LoadRegScaled{regT=workReg1, regN=X8, unitOffset=0, loadType=Load64}]
                        )
            in
                jumpToFunctionCode @ setArgumentsCode
            end

        |   codeExtended _ (FunctionCall{callKind, regArgs, stackArgs, dests, fpRegArgs, fpDests, saveRegs={gcUpdate=saveRegs, ...}, ...}, code) =
            let
                local
                    fun pushStackArgs ([], _, code) = code

                    |   pushStackArgs (ArgOnStack {wordOffset, ...} ::args, argNum, code) =
                        let
                            (* Have to adjust the offsets of stack arguments. *)
                            val adjustedOffset = wordOffset+argNum
                        in
                            pushStackArgs(args, argNum+1,
                                loadFromStack(workReg1, adjustedOffset, code) <::>
                                StoreRegUnscaled{regT=workReg1, regN=X_MLStackPtr, byteOffset= ~8, unscaledType=PreIndex, loadType=Load64})
                        end

                    |   pushStackArgs (ArgInReg reg ::args, argNum, code) =
                            pushStackArgs(args, argNum+1,
                                code <::>
                                (StoreRegUnscaled{regT=reg, regN=X_MLStackPtr, byteOffset= ~8,
                                                         unscaledType=PreIndex, loadType=Load64}))

                    val pushedArgs = pushStackArgs(stackArgs, 0, code (* Initial code *))

                    (* We have to adjust any stack offset to account for the arguments we've pushed. *)
                    val numStackArgs = List.length stackArgs

                    fun convertArg(ArgOnStack{wordOffset, ...}) = IsOnStack(wordOffset+numStackArgs)
                    |   convertArg(ArgInReg reg) = IsInReg(reg)

                in
                    val extRegArgs = map (fn (a, r) => {src=convertArg a, dst=IsInReg r}) regArgs
                    val extFPArgs = map (fn (a, r) => {src=a, dst=r}) fpRegArgs
                    val loadArgs = moveMultipleFPRegisters(extFPArgs, moveMultipleValues(extRegArgs, pushedArgs))
                end

                (* Push the registers before the call and pop them afterwards. *)
                fun makeSavesAndCall([], code) =
                    (
                        case callKind of
                            Recursive => code <::> (BranchAndLink startOfFunctionLabel)
                        |   ConstantCode m =>
                            (
                                case archType of
                                ArchC32 _ => raise InternalError "ConstantCode" (* Not currently *)
                            |   ArchNative => code <::> (LoadAddr(workReg1, m)) <::> (BranchReg{regD=workReg1, brRegType=BRRAndLink})
                            )
                        |   FullCall =>
                            (
                                case archType of
                                    ArchC32 shift => code <::>
                                        AddShiftedReg{regM=X8, regN=X_Base32in64, regD=workReg1, shift=ShiftLSL(wordToWord8 shift),
                                                      setFlags=false, opSize=OpSize64} <::>
                                        LoadRegScaled{regT=workReg1, regN=workReg1, unitOffset=0, loadType=Load64} <::>
                                        BranchReg{regD=workReg1, brRegType=BRRAndLink}
                                |   ArchNative =>
                                        code <::> LoadRegScaled{regT=workReg1, regN=X8, unitOffset=0, loadType=Load64} <::>
                                            BranchReg{regD=workReg1, brRegType=BRRAndLink}
                            )
                    )

                |   makeSavesAndCall(reg::regs, code) =
                    let
                        val areg = reg
                    in
                        makeSavesAndCall(regs,
                                code <::> StoreRegUnscaled{regT=areg, regN=X_MLStackPtr, byteOffset= ~8,
                                                           loadType=Load64, unscaledType=PreIndex}) <::>
                            LoadRegUnscaled{regT=areg, regN=X_MLStackPtr, byteOffset= 8, loadType=Load64, unscaledType=PostIndex}
                    end

                (* Results.  These go from the specific result register into the allocated register. *)
                val resultPairs = List.map(fn (pr, rr) => {src=rr,dst=pr}) dests
                val fpResultPairs = List.map(fn (pr, rr) => {src=rr,dst=pr}) fpDests
            in
                moveMultipleFPRegisters(fpResultPairs,
                    moveMultipleRegisters(resultPairs, makeSavesAndCall(saveRegs, loadArgs)))
            end

        |   codeExtended _ (ReturnResultFromFunction { results, fpResults, returnReg, numStackArgs }, code) =
            let
                fun resetStack(0, code) = code
                |   resetStack(nItems, code) =
                        addConstantWord({regS=X_MLStackPtr, regD=X_MLStackPtr, regW=X3,
                                        value=Word64.fromLarge(Word.toLarge nativeWordSize) * Word64.fromInt nItems}, code)
                (* Return results.  This goes from the allocated register into the specific register rr. *)
                val resultPairs = List.map(fn (pr, rr) => {src=pr,dst=rr}) results
                and fpResultPairs = List.map(fn (pr, rr) => {src=pr,dst=rr}) fpResults
            in
                BranchReg{regD=returnReg, brRegType=BRRReturn} :: resetStack(numStackArgs,
                    moveMultipleFPRegisters(fpResultPairs, moveMultipleRegisters(resultPairs, code)))
            end

        |   codeExtended _ (RaiseExceptionPacket{ packetReg }, code) =
                (* We need a work register here.  It can be any register other than X0 since
                   we don't preserve registers across calls. *)
                (* Copy the handler "register" into the stack pointer.  Then
                   jump to the address in the first word.  The second word is
                   the next handler.  This is set up in the handler.  We have a lot
                   more raises than handlers since most raises are exceptional conditions
                   such as overflow so it makes sense to minimise the code in each raise. *)
                moveIfNecessary({src=packetReg, dst=X0}, code) <::>
                LoadRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset, loadType=Load64} <::>
                LoadRegScaled{regT=workReg1, regN=X_MLStackPtr, unitOffset=0, loadType=Load64} <::>
                BranchReg{regD=workReg1, brRegType=BRRBranch }

        |   codeExtended _ (PushToStack{ source, copies, ... }, code) =
            let
                val reg = source
                val _ = copies > 0 orelse raise InternalError "PushToStack: copies<1"
                fun pushn(0, c) = c
                |   pushn(n, c) =
                        pushn(n-1,
                            (StoreRegUnscaled{regT=reg, regN=X_MLStackPtr, byteOffset= ~8, loadType=Load64, unscaledType=PreIndex}) :: c)
            in
                pushn(copies, code)
            end

        |   codeExtended _ (LoadStack{ dest, wordOffset, ... }, code) =
                loadFromStack(dest, wordOffset, code)

        |   codeExtended _ (StoreToStack{ source, stackOffset, ... }, code) =
                (* Store into the stack to set a field of a container.  Always 64-bits. *)
                storeToStack(source, stackOffset, workReg1, code)

        |   codeExtended _ (ContainerAddress{ dest, stackOffset, ... }, code) =
                (* Set the register to an offset in the stack. *)
            let
                val _ = stackOffset >= 0 orelse raise InternalError "codeGenICode: ContainerAddress - negative offset"
                val byteOffset = stackOffset * Word.toInt nativeWordSize
            in
                if byteOffset >= 4096
                then code <::> LoadNonAddr(dest, Word64.fromInt byteOffset) <::>
                        AddShiftedReg{regN=X_MLStackPtr, regM=dest, regD=dest, shift=ShiftNone, setFlags=false, opSize=OpSize64}
                else code <::> AddImmediate{regN=X_MLStackPtr, regD=dest, immed=Word.fromInt byteOffset,
                                          shifted=false, setFlags=false, opSize=OpSize64}
            end

        |   codeExtended _ (ResetStackPtr{ numWords, ... }, code) =
                addConstantWord({regS=X_MLStackPtr, regD=X_MLStackPtr, regW=workReg1,
                    value=Word64.fromInt numWords * Word.toLarge nativeWordSize}, code)

        |   codeExtended _ (TagValue{ source, dest, isSigned=_, opSize }, code) =
                (* Shift left by one bit and add one. *)
                code <::>
                    shiftConstant{ direction=ShiftLeft, regD=dest, regN=source, shift=0w1, opSize=opSize } <::>
                    BitwiseLogical{ bits=0w1, regN=dest, regD=dest, opSize=opSize, setFlags=false, logOp=LogOr}

        |   codeExtended _ (UntagValue{ source, dest, isSigned, opSize }, code) =
                code <::>
                    shiftConstant{ direction=if isSigned then ShiftRightArithmetic else ShiftRightLogical,
                                    regD=dest, regN=source, shift=0w1, opSize=opSize }

        |   codeExtended _ (BoxLarge{ source, dest, saveRegs={gcUpdate=saveRegs, ...} }, code) =
               boxSysWord({source=source, destination=dest,
                           workReg=workReg1, saveRegs=saveRegs}, code)

        |   codeExtended _ (UnboxLarge{ source, dest }, code) =
            let
                (* Unbox a large word.  The argument is a poly word. *)
                val destReg = dest
                and srcReg = source
            in
                case archType of
                    ArchC32 0w3 =>
                        LoadRegIndexed{regN=X_Base32in64, regM=source, regT=dest, option=ExtUXTX ScaleOrShift, loadType=Load64} :: code
                |   ArchC32 shift =>
                        LoadRegScaled{regT=destReg, regN=destReg, unitOffset=0, loadType=Load64} ::
                            AddShiftedReg{regM=srcReg, regN=X_Base32in64, regD=destReg, shift=ShiftLSL(wordToWord8 shift),
                                          setFlags=false, opSize=OpSize64} :: code
                |   ArchNative => LoadRegScaled{regT=destReg, regN=srcReg, unitOffset=0, loadType=Load64} :: code
            end

        |   codeExtended _ (BoxTagFloat{ floatSize=Double64, source, dest, saveRegs={gcUpdate=saveRegs, ...} }, code) =
                boxDouble({source=source, destination=dest,
                           workReg=workReg1, saveRegs=saveRegs}, code)

        |   codeExtended _ (BoxTagFloat{ floatSize=Float32, source, dest, saveRegs={gcUpdate=saveRegs, ...} }, code) =
            let
                val floatReg = source
                and fixedReg = dest
            in
                case archType of
                    ArchC32 _ =>
                        boxFloat({source=floatReg, destination=fixedReg, workReg=workReg1, saveRegs=saveRegs}, code)
                |   ArchNative =>
                        code <::>
                            MoveFPToGeneral{regN=floatReg, regD=fixedReg, floatSize=Float32} <::>
                            shiftConstant{ direction=ShiftLeft, shift=0w32, regN=fixedReg, regD=fixedReg, opSize=OpSize64} <::>
                            BitwiseLogical{ bits=0w1, regN=fixedReg, regD=fixedReg, opSize=OpSize64, setFlags=false, logOp=LogOr}
            end

        |   codeExtended _ (UnboxTagFloat { floatSize=Double64, source, dest }, code) =
            (
                case archType of
                    ArchC32 0w3 =>
                        code <::>
                            LoadFPRegIndexed{regN=X_Base32in64, regM=source, regT=dest, option=ExtUXTX ScaleOrShift, floatSize=Double64}                        
                |   ArchC32 shift =>
                        code <::>
                            AddShiftedReg{regM=source, regN=X_Base32in64, regD=workReg1, shift=ShiftLSL(wordToWord8 shift),
                                              setFlags=false, opSize=OpSize64} <::>
                            LoadFPRegScaled{regT=dest, regN=workReg1, unitOffset=0, floatSize=Double64}
                |   ArchNative => code <::> LoadFPRegScaled{regT=dest, regN=source, unitOffset=0, floatSize=Double64}
            )

        |   codeExtended _ (UnboxTagFloat { floatSize=Float32, source, dest }, code) =
            (
                (* This is tagged in native 64-bits.  In 32-in-64 we're loading 32-bits so we
                   can use an indexed load directly. *)
                case archType of
                    ArchC32 0w2 =>
                        code <::>
                            LoadFPRegIndexed{regN=X_Base32in64, regM=source, regT=dest, option=ExtUXTX ScaleOrShift, floatSize=Float32}
                |   ArchC32 shift =>
                        code <::>
                            AddShiftedReg{regM=source, regN=X_Base32in64, regD=workReg1, shift=ShiftLSL(wordToWord8 shift),
                                              setFlags=false, opSize=OpSize64} <::>
                            LoadFPRegScaled{regT=dest, regN=workReg1, unitOffset=0, floatSize=Float32}                    
                |   ArchNative =>
                        code <::>
                            shiftConstant{direction=ShiftRightLogical, shift=0w32, regN=source, regD=workReg1, opSize=OpSize64} <::>
                            MoveGeneralToFP{regN=workReg1, regD=dest, floatSize=Float32}
            )

        |   codeExtended _ (LoadAcquire{dest, base, loadType, ...}, code) =
                LoadAcquireReg{regT=dest, regN=base, loadType=loadType} :: code

        |   codeExtended _ (StoreRelease{source, base, loadType, ...}, code) =
                StoreReleaseReg{regT=source, regN=base, loadType=loadType} :: code

        |   codeExtended _ (BitFieldShift{ source, dest, isSigned, length, immr, imms }, code) =
                BitField{immr=immr, imms=imms, regN=source, regD=dest,
                         bitfieldKind=if isSigned then BFSigned else BFUnsigned, opSize=length} :: code

        |   codeExtended _ (BitFieldInsert{ source, destAsSource, dest, length, immr, imms }, code) =
            let
                (* If we're using BitFieldMove we retain some of the bits of the
                   destination.  The higher levels require us to treat that as a source.  *)
                val _ = source = dest andalso raise InternalError "codeExtended: bitfield: dest=source"
            in
                BitField{immr=immr, imms=imms, regN=source, regD=dest, bitfieldKind=BFInsert, opSize=length} ::
                    moveIfNecessary({src=destAsSource, dst=dest}, code)
            end

        |   codeExtended {flow} (IndexedCaseOperation{testReg}, code) =
            let
                (* testReg contains the original value after the lowest value has been subtracted.
                   Since both the original value and the lowest value were tagged it contains a
                   shifted but untagged value. *)
                (* This should only be within a block with an IndexedBr flow type. *)
                val cases =
                    case flow of IndexedBr cases => cases | _ => raise InternalError "codeGenICode: IndexedCaseOperation"
                val caseLabels = map getBlockLabel cases
                val tableLabel = createLabel labelMaker
            in
                code <::>
                    LoadLabelAddress(workReg1, tableLabel) <::>
                    (* Add the value shifted by one since it's already shifted. *)
                    AddShiftedReg{regN=workReg1, regD=workReg1, regM=testReg,
                                          shift=ShiftLSL 0w1, setFlags=false, opSize=OpSize64} <::>
                    BranchReg{regD=workReg1, brRegType=BRRBranch} <::>
                    BranchTable{ startLabel=tableLabel, brTable=caseLabels }
            end

        |   codeExtended {flow} (PushExceptionHandler, code) =
            let
                (* This should only be within a block with a SetHandler flow type. *)
                val handleLabel =
                    case flow of
                        SetHandler{ handler, ...} => handler
                    |   _ => raise InternalError "codeGenICode: PushExceptionHandler"
                val labelRef = getBlockLabel handleLabel
            in
                (* Push the old handler and the handler entry point and set the "current handler"
                   to point to the stack after we've pushed these. *)
                code <::>
                    LoadRegScaled{regT=workReg1, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset, loadType=Load64} <::>
                    LoadLabelAddress(workReg2, labelRef) <::>
                    StoreRegPair{regT1=workReg2, regT2=workReg1, regN=X_MLStackPtr, unitOffset= ~2, unscaledType=PreIndex, loadType=Load64} <::>
                    StoreRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset, loadType=Load64}
            end

        |   codeExtended _ (PopExceptionHandler, code) =
                (* Remove and discard the handler we've set up.  Pop the previous handler
                   and put into "current handler". *)
                code <::>
                    LoadRegPair{regT1=XZero, regT2=workReg2, regN=X_MLStackPtr, unitOffset=2, unscaledType=PostIndex, loadType=Load64} <::>
                    StoreRegScaled{regT=workReg2, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset, loadType=Load64}

        |   codeExtended _ (BeginHandler{packetReg}, code) =
            let
                val beginHandleCode =
                    code <::>
                        (* The exception raise code resets the stack pointer to the value in the exception handler
                           so this is probably redundant.  Leave it for the moment, *)
                        LoadRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset, loadType=Load64} <::>
                        LoadRegPair{regT1=XZero, regT2=workReg2, regN=X_MLStackPtr, unitOffset=2, unscaledType=PostIndex, loadType=Load64} <::>
                        StoreRegScaled{regT=workReg2, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset, loadType=Load64}
            in
                moveIfNecessary({src=X0, dst=packetReg }, beginHandleCode)
            end

        |   codeExtended _ (CompareByteVectors{vec1Addr, vec2Addr, length,  ...}, code) =
            let
                (* Construct a loop to compare two vectors of bytes. *)
                val vec1Reg = vec1Addr
                and vec2Reg = vec2Addr
                and lenReg = length
                val loopLabel = createLabel labelMaker and exitLabel = createLabel labelMaker
            in
                code <::>
                (* Set the CC to Equal before we start in case length = 0 *)
                SubShiftedReg{regM=lenReg, regN=lenReg, regD=XZero, shift=ShiftNone, setFlags=true, opSize=OpSize64} <::>
                SetLabel loopLabel <::> (* Start of loop *)
                CompareBranch{ test=lenReg, label=exitLabel, onZero=true, opSize=OpSize64} <::> (* Go to the end when len = zero *)
                (* Load the bytes for the comparison and increment each. *)
                LoadRegUnscaled{regT=workReg1, regN=vec1Reg, byteOffset=1, unscaledType=PostIndex, loadType=Load8} <::>
                LoadRegUnscaled{regT=workReg2, regN=vec2Reg, byteOffset=1, unscaledType=PostIndex, loadType=Load8} <::>            
                SubImmediate{regN=lenReg, regD=lenReg, immed=0w1, shifted=false, setFlags=false, opSize=OpSize64} <::> (* Decr len *)
                (* Compare *)
                SubShiftedReg{regM=workReg2, regN=workReg1, regD=XZero, shift=ShiftNone, setFlags=true, opSize=OpSize64} <::>
                ConditionalBranch(CondEqual, loopLabel) <::> (* Loop if they're equal *)
                SetLabel exitLabel
            end

        |   codeExtended _ (BlockMove{srcAddr, destAddr, length, isByteMove}, code) =
            let
                (* Construct a loop to move the data. *)
                val srcReg = srcAddr
                and destReg = destAddr
                and lenReg = length
                val loopLabel = createLabel labelMaker and exitLabel = createLabel labelMaker
                val (offset, loadType) = if isByteMove then (1, Load8) else if is32in64 then (4, Load32) else (8, Load64)
            in
                code <::> SetLabel loopLabel (* Start of loop *) <::> 
                CompareBranch{ test=lenReg, label=exitLabel, onZero=true, opSize=OpSize64} <::> (* Exit when length = 0 *)
                LoadRegUnscaled{regT=workReg1, regN=srcReg, byteOffset=offset, loadType=loadType, unscaledType=PostIndex} <::>
                StoreRegUnscaled{regT=workReg1, regN=destReg, byteOffset=offset, loadType=loadType, unscaledType=PostIndex} <::>
                SubImmediate{regN=lenReg, regD=lenReg, immed=0w1, shifted=false, setFlags=false, opSize=OpSize64} <::> (* Decr len *)
                UnconditionalBranch loopLabel <::> (* Back to the start *)
                SetLabel exitLabel
            end

        |   codeExtended _ (AddSubXSP{ source, dest, isAdd }, code) =
            let
                val allocFreeCode =
                    (if isAdd then AddExtendedReg else SubExtendedReg)
                        {regM=source, regN=XSP, regD=XSP, extend=ExtUXTX 0w0, setFlags=false, opSize=OpSize64} :: code
            in
                case dest of
                    XZero => allocFreeCode
                |   destReg =>
                        (* We have to use add here to get the SP into the destination instead of the usual move. *)
                        AddImmediate{regN=XSP, regD=destReg, immed=0w0, shifted=false, setFlags=false, opSize=OpSize64} ::
                            allocFreeCode
            end

        |   codeExtended _ (TouchValue _, code) = code (* Don't need to do anything now. *)

            (* Used in mutex operations. *)
        |   codeExtended _ (LoadAcquireExclusive{ base, dest }, code) =
                LoadAcquireExclusiveRegister{regN=base, regT=dest} :: code

        |   codeExtended _ (StoreReleaseExclusive{ base, source, result }, code) =
                StoreReleaseExclusiveRegister{regS=result,
                        regT=source, regN=base} :: code

        |   codeExtended _ (MemoryBarrier, code) = code <::> MemBarrier

        |   codeExtended _ (ConvertIntToFloat{ source, dest, srcSize, destSize}, code) =
                (CvtIntToFP{regN=source, regD=dest, floatSize=destSize, opSize=srcSize}) :: code

        |   codeExtended _ (ConvertFloatToInt{ source, dest, srcSize, destSize, rounding}, code) =
                (CvtFloatToInt{regN=source, regD=dest, round=rounding, floatSize=srcSize, opSize=destSize}) :: code

        |   codeExtended _ (UnaryFloatingPt{ source, dest, fpOp}, code) =
                (FPUnaryOp{regN=source, regD=dest, fpOp=fpOp}) :: code

        |   codeExtended _ (BinaryFloatingPoint{ arg1, arg2, dest, fpOp, opSize}, code) =
                (FPBinaryOp{regN=arg1, regM=arg2, regD=dest, floatSize=opSize, fpOp=fpOp}) :: code

        |   codeExtended _ (CompareFloatingPoint{ arg1, arg2, opSize, ...}, code) =
                (FPComparison{regN=arg1, regM=arg2, floatSize=opSize}) :: code

        |   codeExtended _ (CPUYield, code) = code <::> Yield

        |   codeExtended _ (AtomicOperation{ base, source, dest, atOp }, code) = AtomicExtension{regN=base, regS=source, regT=dest, atOp=atOp} :: code

        local
            (* processed - set to true when a block has been processed. *)
            val processed = Array.array(numBlocks, false)
            fun haveProcessed n = Array.sub(processed, n)
            
            (* Find the blocks that reference this one.  This isn't essential but
               allows us to try to generate blocks in the order of the control
               flow.  This in turn may allow us to use short branches rather
               than long ones. *)
            val labelRefs = Array.array(numBlocks, [])

            datatype flowCode =
                FlowCodeSimple of int
            |   FlowCodeCMove of {code: precode list, trueJump: int, falseJump: int}
            
            (* Process this recursively to set the references.  If we have
               unreachable blocks, perhaps because they've been merged, we
               don't want to include them in the reference counting.
               This shouldn't happen now that IdentifyReferences removes
               unreferenced blocks. *)
            fun setReferences fromLabel toLabel =
                case Array.sub(labelRefs, toLabel) of
                    [] => (* Not yet visited at all. *)
                    let
                        val BasicBlock{ flow, ...} = Vector.sub(blocks, toLabel)
                        val refs =
                            case flow of
                                ExitCode => []
                            |   Unconditional lab => [lab]
                            |   Conditional{trueJump, falseJump, ... } => [trueJump, falseJump]
                            |   IndexedBr labs => labs
                            |   SetHandler { handler, continue } => [handler, continue]
                            |   UnconditionalHandle _ => []
                            |   ConditionalHandle { continue, ...} => [continue]

                        val () =
                            if fromLabel >= 0 then Array.update(labelRefs, toLabel, [fromLabel]) else ()
                    in
                        List.app (setReferences toLabel) refs
                    end
                    
                |   refs =>
                    (* We've visiting this at least once.  Just add us to the list. *)
                        Array.update(labelRefs, toLabel, fromLabel :: refs)
            
            val _ = setReferences 0 0
            
            (* Process the blocks.  We keep the "stack" explicit rather than using recursion
               because this allows us to select both arms of a conditional branch sooner. *)
            fun genCode(toDo, lastFlow, code) =
            case List.filter (not o haveProcessed) toDo of
                [] =>
                let
                    (* There's nothing left to do. We may need to add a final branch to the end. *)
                    val finalBranch =
                        case lastFlow of
                            ExitCode => []
                        |   IndexedBr _ => []
                        |   Unconditional dest => [(UnconditionalBranch(getBlockLabel dest))]
                        |   Conditional { condition, trueJump, falseJump, ...} =>
                                [
                                    (UnconditionalBranch(getBlockLabel falseJump)),
                                    (ConditionalBranch(condition, getBlockLabel trueJump))
                                ]
                        |   SetHandler { continue, ...} => [(UnconditionalBranch(getBlockLabel continue))]
                        |   UnconditionalHandle _ => []
                        |   ConditionalHandle { continue, ...} => [(UnconditionalBranch(getBlockLabel continue))]
                in
                    finalBranch @ code (* Done. *)
                end

            |   stillToDo as head :: _ =>
                let
                    local
                        (* Check the references.  If all the sources that lead up to this have
                           already been we won't have any backward jumps. *)
                        fun available dest = List.all haveProcessed (Array.sub(labelRefs, dest))

                        val continuation =
                            case lastFlow of
                                ExitCode => NONE
                            |   IndexedBr _ => NONE (* We could put the last branch in here. *)
                            |   Unconditional dest =>
                                    if not (haveProcessed dest) andalso available dest
                                    then SOME(FlowCodeSimple dest)
                                    else NONE
                            |   Conditional {trueJump, falseJump, condition, ...} =>
                                let
                                    (* Can we replace this with a conditional set?  If both arms simply set
                                       a register to a value and either return or jump to the same location
                                       we can.  *)
                                    (* The most common case is producing a boolean result, 1 (tagged 0) for false and
                                       3 (tagged 1) for true.  We look for blocks that generate this and also
                                       functions that return this. *)
                                    val BasicBlock { flow=tFlow, block=tBlock, ...} = Vector.sub(blocks, trueJump)
                                    and BasicBlock { flow=fFlow, block=fBlock, ...} = Vector.sub(blocks, falseJump)

                                    datatype moveType = MoveReg of xReg | MoveAddr of machineWord | MoveNonAddr of Word64.word

                                    fun findMoveAndTail [] = (SOME NONE, [])
                                    |   findMoveAndTail (MoveRegister{source, dest} :: tl) = (SOME(SOME(dest, MoveReg source)), tl)
                                    |   findMoveAndTail (LoadAddressConstant{source, dest} :: tl) = (SOME(SOME(dest, MoveAddr source)), tl)
                                    |   findMoveAndTail (LoadNonAddressConstant{source, dest} :: tl) = (SOME(SOME(dest, MoveNonAddr source)), tl)
                                            (* The result could be in a different register which will need to be moved to X0. *)
                                    |   findMoveAndTail (tl as [ReturnResultFromFunction{results=[(resRegT, realRegT)], ...}]) = (SOME(SOME(realRegT, MoveReg resRegT)), tl)
                                    |   findMoveAndTail tl = (NONE, tl)

                                    val isPossSetCCOrCmov =
                                        if not (haveProcessed trueJump) andalso available trueJump
                                            andalso not (haveProcessed falseJump) andalso available falseJump
                                        then
                                        let
                                            (* If we have a conditional branch and each arm simply sets the
                                               result register to a constant or another register we can
                                               replace this with a conditional move.  Apart from reducing the size of
                                               the code this should reduce the chance of a processor pre-fetch stall.
                                               We may have this either with an internal branch or a return. *)
                                            fun makeCond(reg, sourceT, MoveNonAddr 0w1) =
                                                let
                                                    (* We can generate 0w1 using CSINC XZ. Because constants
                                                       are almost always tagged we don't bother with zero itself. *)
                                                    val (codeT, regT) =
                                                        case sourceT of
                                                            MoveReg regT => (code, regT)
                                                        |   MoveAddr addr => (code <::> LoadAddr(workReg1, addr), workReg1)
                                                        |   MoveNonAddr value => (code <::> LoadNonAddr(workReg1, value), workReg1)
                                               in
                                                    codeT <::>
                                                        ConditionalSet{regD=reg, regTrue=regT, regFalse=XZero, cond=condition,
                                                                       condSet=CondSetIncr, opSize=OpSize64}
                                                end
                                                
                                            |   makeCond(reg, MoveNonAddr 0w1, sourceF) =
                                                let
                                                    val (codeF, regF) =
                                                        case sourceF of
                                                            MoveReg regF => (code, regF)
                                                        |   MoveAddr addr => (code <::> LoadAddr(workReg2, addr), workReg2)
                                                        |   MoveNonAddr value => (code <::> LoadNonAddr(workReg2, value), workReg2)
                                                in
                                                    codeF <::>
                                                        ConditionalSet{regD=reg, regTrue=regF, regFalse=XZero, cond=invertTest condition,
                                                                               condSet=CondSetIncr, opSize=OpSize64}
                                                end

                                            |   makeCond(reg, sourceT, sourceF) =
                                                let
                                                    val (codeT, regT) =
                                                        case sourceT of
                                                            MoveReg regT => (code, regT)
                                                        |   MoveAddr addr => (code <::> LoadAddr(workReg1, addr), workReg1)
                                                        |   MoveNonAddr value => (code <::> LoadNonAddr(workReg1, value), workReg1)
                                                    val (codeF, regF) =
                                                        case sourceF of
                                                            MoveReg regF => (codeT, regF)
                                                        |   MoveAddr addr => (codeT <::> LoadAddr(workReg2, addr), workReg2)
                                                        |   MoveNonAddr value => (codeT <::> LoadNonAddr(workReg2, value), workReg2)
                                                in
                                                    codeF <::>
                                                        ConditionalSet{regD=reg, regTrue=regT, regFalse=regF, cond=condition,
                                                                               condSet=CondSet, opSize=OpSize64}
                                                end
                                        in
                                            case (tFlow, fFlow) of
                                                (Unconditional tDest, Unconditional fDest) =>
                                                if tDest <> fDest then NONE
                                                else
                                                let
                                                    val (tInstr, tTl) = findMoveAndTail tBlock
                                                    and (fInstr, fTl) = findMoveAndTail fBlock
                                                in
                                                    case (fTl, tTl, tInstr, fInstr) of
                                                        ([], [], SOME tIns, SOME fIns) => (* We should have at most one move. *)
                                                        let
                                                        in
                                                            case (tIns, fIns) of
                                                                (NONE, NONE) => NONE (* Both empty??*)
                                                            |   (SOME (regT, sourceT), NONE) =>
                                                                    SOME({code=makeCond(regT, sourceT, MoveReg regT), trueJump=trueJump, falseJump=falseJump})
                                                            |   (NONE, SOME (regF, sourceF)) =>
                                                                    SOME({code=makeCond(regF, MoveReg regF, sourceF), trueJump=trueJump, falseJump=falseJump})
                                                            |   (SOME (regT, sourceT), SOME (regF, sourceF)) =>
                                                                    if regT <> regF then NONE
                                                                    else SOME({code=makeCond(regF, sourceT, sourceF), trueJump=trueJump, falseJump=falseJump})
                                                        end
                                                                
                                                    |   _ => NONE
                                                end

                                            |   (ExitCode, ExitCode) =>
                                                let
                                                    val (tInstr, tTl) = findMoveAndTail tBlock
                                                    and (fInstr, fTl) = findMoveAndTail fBlock
                                                    (* We can use a conditional set if we have the same return after any move.
                                                       This is slightly simpler than the branch case because the result will
                                                       always end up in X0. *)
                                                in
                                                    case (tTl, fTl, tInstr, fInstr) of
                                                        ([ReturnResultFromFunction{results=[(_, realRegT)], fpResults=[], returnReg=retRegT, numStackArgs=nsT}],
                                                            [ReturnResultFromFunction{results=[(_, realRegF)], fpResults=[], returnReg=retRegF, numStackArgs=nsF}],
                                                            SOME(SOME (regT, sourceT)), SOME (SOME (regF, sourceF))) =>
                                                            if regT <> regF then NONE
                                                            else
                                                            let
                                                                val _ = realRegT = X0 andalso realRegF = X0 andalso retRegT = retRegF andalso nsT = nsF
                                                                            orelse raise InternalError "isPossSetCCOrCmov: return mismatch"
                                                                val condCode = makeCond(regF, sourceT, sourceF)
                                                                val withReset =
                                                                    if nsT = 0
                                                                    then condCode
                                                                    else addConstantWord({regS=X_MLStackPtr, regD=X_MLStackPtr, regW=X3,
                                                                            value=Word64.fromLarge(Word.toLarge nativeWordSize) * Word64.fromInt nsT}, condCode)
                                                                val withReturn =
                                                                    withReset <::> BranchReg{regD=retRegT, brRegType=BRRReturn}
                                                            in
                                                                SOME({code=withReturn, trueJump=trueJump, falseJump=falseJump})
                                                            end
                                                    |   _ => NONE
 
                                                end
                                        
                                            |   _ => NONE
                                        end

                                        else NONE
                                in
                                    case isPossSetCCOrCmov of
                                        NONE =>
                                        (* We can usually choose either destination and in nearly all cases
                                           it won't matter.  The ARM doesn't seem to define what happens if
                                           a conditional branch hasn't been seen before.  Assume it's the
                                           same as the X86 and that conditional forward branches aren't
                                           taken.  Arrange this so that something that raises an exception
                                           is assumed to be "unusual". *)
                                        let
                                            val (first, second) =
                                                case (condition, Vector.sub(blocks, falseJump)) of
                                                    (CondNoOverflow, _) => (trueJump, falseJump)
                                                |   (_, BasicBlock{ flow=ExitCode, block, ...}) =>
                                                        if List.exists(fn RaiseExceptionPacket _ => true | _ => false) block
                                                        then (trueJump, falseJump)
                                                        else (falseJump, trueJump)
                                                |   _ => (falseJump, trueJump)
                                        in
                                            if not (haveProcessed first) andalso available first
                                            then SOME(FlowCodeSimple first)
                                            else if not (haveProcessed second) andalso available second
                                            then SOME(FlowCodeSimple second)
                                            else NONE
                                        end
                                    |   SOME args => SOME(FlowCodeCMove args)
                                    end
                           |    SetHandler { continue, ... } =>
                                    (* We want the continuation if possible.  We'll need a
                                       branch round the handler so that won't help. *)
                                    if not (haveProcessed continue) andalso available continue
                                    then SOME(FlowCodeSimple continue)
                                    else NONE
                           |    UnconditionalHandle _ => NONE
                           |    ConditionalHandle _ => NONE
                    in
                        (* First choice - continue the existing block.
                           Second choice - the first item whose sources have all been
                           processed.
                           Third choice - something from the list. *)
                        val picked =
                            case continuation of
                                SOME c => c
                            |   NONE =>
                                    case List.find available stillToDo of
                                        SOME c => FlowCodeSimple c
                                    |   NONE => FlowCodeSimple head
                    end
                    
                in
                    case picked of
                        FlowCodeSimple picked =>
                        let
                            val () = Array.update(processed, picked, true)

                            (* Code to terminate the previous block. *)
                            val startCode =
                                case lastFlow of
                                    ExitCode => []
                                |   IndexedBr _ => []
                                |   UnconditionalHandle _ => []
                                |   Unconditional dest =>
                                        if dest = picked then [] else [(UnconditionalBranch(getBlockLabel dest))]
                                |   ConditionalHandle { continue, ...} =>
                                        if continue = picked then [] else [(UnconditionalBranch(getBlockLabel continue))]
                                |   SetHandler { continue, ... } =>
                                        if continue = picked then [] else [(UnconditionalBranch(getBlockLabel continue))]
                                |   Conditional { condition, trueJump, falseJump, ...} =>
                                    if picked = falseJump (* Usual case. *)
                                    then [(ConditionalBranch(condition, getBlockLabel trueJump))]
                                    else if picked = trueJump
                                    then (* We have a jump to the true condition. Invert the jump.
                                            This is more than an optimisation.  Because this immediately precedes the
                                            true block we're not going to generate a label. *)
                                        [(ConditionalBranch(invertTest condition, getBlockLabel falseJump))]
                                    else
                                    [
                                        (UnconditionalBranch(getBlockLabel falseJump)),
                                        (ConditionalBranch(condition, getBlockLabel trueJump))
                                    ]

                            (* Code-generate the body with the code we've done so far
                               at the end.  Add a label at the start if necessary. *)
                            local
                                (* If the previous block dropped through to this and this was
                                   the only reference then we don't need a label. *)
                                fun onlyJumpingHere (lab: int) =
                                    if lab <> picked then false
                                    else case Array.sub(labelRefs, picked) of
                                        [singleton] => singleton = lab
                                    |   _ => false
                    
                                val noLabel =
                                    case lastFlow of
                                        ExitCode => picked = 0 (* Unless this was the first block. *)
                                    |   Unconditional dest => onlyJumpingHere dest
                                    |   Conditional { trueJump, falseJump, ...} =>
                                            onlyJumpingHere trueJump orelse onlyJumpingHere falseJump
                                    |   IndexedBr _ => false
                                    |   SetHandler _ => false
                                    |   UnconditionalHandle _ => false
                                    |   ConditionalHandle { continue, ...} => onlyJumpingHere continue
                            in
                                val startLabel = if noLabel then [] else [(SetLabel(getBlockLabel picked))]
                            end

                            val BasicBlock { flow, block, ...} = Vector.sub(blocks, picked)

                            local
                                fun genCodeBlock(instr, code) = codeExtended {flow=flow} (instr, code)
                            in
                                val bodyCode = List.foldl genCodeBlock (startLabel @ startCode @ code) block
                            end

                            val addSet =
                                case flow of
                                    ExitCode => []
                                |   IndexedBr cases => cases
                                |   Unconditional dest => [dest]
                                |   Conditional {trueJump, falseJump, ...} => [falseJump, trueJump]
                                |   SetHandler { handler, continue } => [handler, continue]
                                |   UnconditionalHandle _ => []
                                |   ConditionalHandle { continue, ...} => [continue]

                        in
                            genCode(addSet @ stillToDo, flow, bodyCode)
                        end
                
                    |   FlowCodeCMove{code, trueJump, falseJump} =>
                        let
                            (* We've generated a conditional move and possibly a return.  If the
                               trueJump and falseJump are only ever referenced from this block
                               they're done, otherwise we still need to do them. *)
                            val _ =
                                case Array.sub(labelRefs, trueJump) of
                                    [_] => Array.update(processed, trueJump, true)
                                |   _ => ()
                            val _ =
                                case Array.sub(labelRefs, falseJump) of
                                    [_] => Array.update(processed, falseJump, true)
                                |   _ => ()
                            val BasicBlock { flow, ...} = Vector.sub(blocks, trueJump)
                            val addSet =
                                case flow of
                                    ExitCode => []
                                |   Unconditional dest => [dest]
                                |   _ => raise InternalError "FlowCodeCMove"
                        in
                            genCode(addSet @ stillToDo, flow, code)
                        end
                end
        in
            val ops = genCode([0], ExitCode, [(SetLabel startOfFunctionLabel)])
        end
    in
        generateFinalCode{instrs=List.rev ops, name=functionName, resultClosure=resultClosure,
                                   parameters=debugSwitches, profileObject=profileObject,  labelMaker= labelMaker}
    end

    structure Sharing =
    struct
        type ('genReg, 'optGenReg, 'fpReg) basicBlock = ('genReg, 'optGenReg, 'fpReg) basicBlock
        and xReg        = xReg
        and vReg        = vReg
        and closureRef  = closureRef
    end

end;
