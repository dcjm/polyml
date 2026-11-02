(*
    Copyright (c) 2021-3, 2026 David C. J. Matthews

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

(* The pre-assembly layer goes below the icode and allows peep-hole optimisation. *)

functor Arm64PreAssembly(

    structure Arm64Assembly: ARM64ASSEMBLY
    structure Debug: DEBUG
    structure Pretty: PRETTY

): ARM64PREASSEMBLY =
struct
    open Arm64Assembly
    
    exception InternalError = Misc.InternalError

    (* Reversed cons and append to make the code easier to read. *)
    infix 5 <::> <@>
    fun tl <::> hd = hd :: tl
    and snd <@> fst = fst @ snd

    (* Many of the datatypes are inherited from Arm64Assembly *)

    datatype loadType = Load64 | Load32 | Load16 | Load8
    and opSize = OpSize32 | OpSize64
    and logicalOp = LogAnd | LogOr | LogXor
    and floatSize = Float32 | Double64
    and shiftDirection = ShiftLeft | ShiftRightLogical | ShiftRightArithmetic
    and multKind =
        MultAdd32 | MultSub32 | MultAdd64 | MultSub64 |
        SignedMultAddLong (* 32bit*32bit + 64bit => 64Bit *) |
        SignedMultHigh (* High order part of 64bit*64Bit *)
    and fpUnary =
        NegFloat | NegDouble | AbsFloat | AbsDouble | ConvFloatToDble |
        ConvDbleToFloat | MoveDouble | MoveFloat
    and fpBinary = MultiplyFP | DivideFP | AddFP | SubtractFP
    and unscaledType = NoUpdate | PreIndex | PostIndex
    and condSet = CondSet | CondSetIncr | CondSetInvert | CondSetNegate
    and bitfieldKind = BFUnsigned | BFSigned | BFInsert
    and brRegType = BRRBranch | BRRAndLink | BRRReturn
        (* Some of the atomic operations added in 8.1 *)
    and atomicOp = LoadAddAL | LoadUmaxAL | SwapAL | LoadAddAcquire | LoadUMaxAcquire | SwapRelease

    datatype label = Label of int
    type labelMaker = int ref
    fun createLabelMaker() = ref 0
    fun createLabel(r as ref n) = Label n before r := n+1

    datatype precode =
        (* Basic instructions *)
        AddImmediate of {regN: xReg, regD: xReg, immed: word, shifted: bool, opSize: opSize, setFlags: bool}
    |   SubImmediate of {regN: xReg, regD: xReg, immed: word, shifted: bool, opSize: opSize, setFlags: bool}
    |   AddShiftedReg of {regM: xReg, regN: xReg, regD: xReg, shift: shiftType, opSize: opSize, setFlags: bool}
    |   SubShiftedReg of {regM: xReg, regN: xReg, regD: xReg, shift: shiftType, opSize: opSize, setFlags: bool}
    |   AddExtendedReg of {regM: xReg, regN: xReg, regD: xReg, extend: Word8.word extend, opSize: opSize, setFlags: bool}
    |   SubExtendedReg of {regM: xReg, regN: xReg, regD: xReg, extend: Word8.word extend, opSize: opSize, setFlags: bool}
    |   MultiplyAndAddSub of {regM: xReg, regN: xReg, regA: xReg, regD: xReg, multKind: multKind}
    |   DivideRegs of
            {regM: xReg, regN: xReg, regD: xReg, isSigned: bool, opSize: opSize}
    |   LogicalShiftedReg of
            {regM: xReg, regN: xReg, regD: xReg, shift: shiftType, logOp: logicalOp, opSize: opSize, setFlags: bool}
    |   LoadRegScaled of
            {regT: xReg, regN: xReg, unitOffset: int, loadType: loadType}
    |   LoadFPRegScaled of
            {regT: vReg, regN: xReg, unitOffset: int, floatSize: floatSize}
    |   StoreRegScaled of
            {regT: xReg, regN: xReg, unitOffset: int, loadType: loadType}
    |   StoreFPRegScaled of
            {regT: vReg, regN: xReg, unitOffset: int, floatSize: floatSize}
    |   LoadRegUnscaled of
            {regT: xReg, regN: xReg, byteOffset: int, loadType: loadType, unscaledType: unscaledType}
    |   StoreRegUnscaled of
            {regT: xReg, regN: xReg, byteOffset: int, loadType: loadType, unscaledType: unscaledType}
    |   LoadFPRegUnscaled of
            {regT: vReg, regN: xReg, byteOffset: int, floatSize: floatSize, unscaledType: unscaledType}
    |   StoreFPRegUnscaled of
            {regT: vReg, regN: xReg, byteOffset: int, floatSize: floatSize, unscaledType: unscaledType}
    |   LoadRegIndexed of {regT: xReg, regN: xReg, regM: xReg, loadType: loadType, option: scale extend}
    |   StoreRegIndexed of {regT: xReg, regN: xReg, regM: xReg, loadType: loadType, option: scale extend}
    |   LoadFPRegIndexed of {regT: vReg, regN: xReg, regM: xReg, floatSize: floatSize, option: scale extend}
    |   StoreFPRegIndexed of {regT: vReg, regN: xReg, regM: xReg, floatSize: floatSize, option: scale extend}
        (* LoadAcquire and StoreRelease are used for mutables. *)
    |   LoadAcquireReg of {regN: xReg, regT: xReg, loadType: loadType}
    |   StoreReleaseReg of {regN: xReg, regT: xReg, loadType: loadType}
        (* LoadAcquireExclusiveRegister and StoreReleaseExclusiveRegister are used for mutexes. *)
    |   LoadAcquireExclusiveRegister of {regN: xReg, regT: xReg}
    |   StoreReleaseExclusiveRegister of {regS: xReg, regT: xReg, regN: xReg}
    |   MemBarrier
        (* Additional atomic operations. *)
    |   AtomicExtension of { regT: xReg, regN: xReg, regS: xReg, atOp: atomicOp }
    |   LoadRegPair of
            { regT1: xReg, regT2: xReg, regN: xReg, unitOffset: int, loadType: loadType, unscaledType: unscaledType}
    |   StoreRegPair of
            { regT1: xReg, regT2: xReg, regN: xReg, unitOffset: int, loadType: loadType, unscaledType: unscaledType}
    |   LoadFPRegPair of
            { regT1: vReg, regT2: vReg, regN: xReg, unitOffset: int, floatSize: floatSize, unscaledType: unscaledType}
    |   StoreFPRegPair of
            { regT1: vReg, regT2: vReg, regN: xReg, unitOffset: int, floatSize: floatSize, unscaledType: unscaledType}
    |   ConditionalSet of
            {regD: xReg, regTrue: xReg, regFalse: xReg, cond: condition, condSet: condSet, opSize: opSize}
    |   BitField of {immr: word, imms: word, regN: xReg, regD: xReg, opSize: opSize, bitfieldKind: bitfieldKind}
    |   ShiftRegisterVariable of {regM: xReg, regN: xReg, regD: xReg, opSize: opSize, shiftDirection: shiftDirection}
    |   BitwiseLogical of { bits: Word64.word, regN: xReg, regD: xReg, opSize: opSize, setFlags: bool, logOp: logicalOp}
    |   CountLeadingZeros of {regN: xReg, regD: xReg, opSize: opSize}
        (* Floating point *)
    |   MoveGeneralToFP of { regN: xReg, regD: vReg, floatSize: floatSize}
    |   MoveFPToGeneral of {regN: vReg, regD: xReg, floatSize: floatSize}
    |   CvtIntToFP of { regN: xReg, regD: vReg, floatSize: floatSize, opSize: opSize}
    |   CvtFloatToInt of { round: IEEEReal.rounding_mode, regN: vReg, regD: xReg, floatSize: floatSize, opSize: opSize}
    |   FPBinaryOp of { regM: vReg, regN: vReg, regD: vReg, floatSize: floatSize, fpOp: fpBinary}
    |   FPComparison of { regM: vReg, regN: vReg, floatSize: floatSize}
    |   FPUnaryOp of {regN: vReg, regD: vReg, fpOp: fpUnary}
        (* Branches and Labels. *)
    |   SetLabel of label
    |   ConditionalBranch of condition * label
    |   UnconditionalBranch of label
    |   BranchAndLink of label
    |   BranchReg of {regD: xReg, brRegType: brRegType }
    |   LoadLabelAddress of xReg * label
    |   TestBitBranch of { test: xReg, bit: Word8.word, label: label, onZero: bool }
    |   CompareBranch of { test: xReg, label: label, onZero: bool, opSize: opSize }
        (* Composite instructions *)
    |   MoveXRegToXReg of {sReg: xReg, dReg: xReg}
    |   LoadNonAddr of xReg * Word64.word
    |   LoadFPConst of {dest: vReg, value: Word64.word, floatSize: floatSize, work: xReg}
    |   LoadAddr of xReg * machineWord
    |   RTSTrap of { rtsEntry: int, work: xReg, save: xReg list }
    |   AllocateMemoryFixedSize of { bytes: word, dest: xReg, save: xReg list, work: xReg }
    |   AllocateMemoryVariableSize of { sizeReg: xReg, dest: xReg, save: xReg list, work: xReg }
        (* Branch table for indexed case. startLabel is the address of the first label in
           the list.  The branch table is a sequence of unconditional branches. *)
    |   BranchTable of { startLabel: label, brTable: label list }
    |   LoadGlobalHeapBaseInCallback of xReg
    |   Yield

    (* Test whether a value is a power of two and return the shift if it is. *)
    fun powerTwo v =
        if LargeWord.andb(v, ~ v) = v
        then
        let
            fun shift(n: Word8.word, 0w1) = n
            |   shift(n, v) = shift(n+0w1, LargeWord.>>(v, 0w1))
        in
            SOME(shift(0w0, v))
        end
        else NONE



    (* Optimise the pre-assembler code and then generate the final code. *)
    fun generateFinalCode {instrs, name, parameters, resultClosure, profileObject, labelMaker=ref labelCount} =
    let
        val labelTargets = Array.tabulate(labelCount, fn i => (Arm64Assembly.createLabel(), i) )

        (* Follow the chain of forwarded labels. *)
        local
            fun forwardLab(labelNo, labels) =
            let
                val dest as (_, dNo) = Array.sub(labelTargets, labelNo)
            in
                if dNo = labelNo
                then dest
                (* This should not happen but just in case... *)
                else if List.exists(fn i => i = dNo) labels
                then raise InternalError "Infinite loop"
                else forwardLab(dNo, dNo::labels)
            end
        in
            fun getLabel labelNo = forwardLab(labelNo, [labelNo])
            val getLabelTarget = #1 o getLabel
        end                

        fun toAssembler([], code) = code

        |   toAssembler(AddImmediate{regN, regD, immed, shifted, opSize, setFlags} :: rest, code) =
            let
                val instr =
                    case (opSize, setFlags) of
                        (OpSize64, false) => addImmediate
                    |   (OpSize32, false) => addImmediate32
                    |   (OpSize64, true) => addSImmediate
                    |   (OpSize32, true) => addSImmediate32
            in
                toAssembler(rest, code <::> instr{regN=regN, regD=regD, immed=immed, shifted=shifted})
            end

        |   toAssembler(SubImmediate{regN, regD, immed, shifted, opSize, setFlags} :: rest, code) =
            let
                val instr =
                    case (opSize, setFlags) of
                        (OpSize64, false) => subImmediate
                    |   (OpSize32, false) => subImmediate32
                    |   (OpSize64, true) => subSImmediate
                    |   (OpSize32, true) => subSImmediate32
            in
                toAssembler(rest, code <::> instr{regN=regN, regD=regD, immed=immed, shifted=shifted})
            end

        |   toAssembler(AddShiftedReg{regM, regN, regD, shift, opSize, setFlags} :: rest, code) =
            let
                val instr =
                    case (opSize, setFlags) of
                        (OpSize64, false) => addShiftedReg
                    |   (OpSize32, false) => addShiftedReg32
                    |   (OpSize64, true) => addSShiftedReg
                    |   (OpSize32, true) => addSShiftedReg32
            in
                toAssembler(rest, code <::> instr{regM=regM, regN=regN, regD=regD, shift=shift})
            end

        |   toAssembler(SubShiftedReg{regM, regN, regD, shift, opSize, setFlags} :: rest, code) =
            let
                val instr =
                    case (opSize, setFlags) of
                        (OpSize64, false) => subShiftedReg
                    |   (OpSize32, false) => subShiftedReg32
                    |   (OpSize64, true) => subSShiftedReg
                    |   (OpSize32, true) => subSShiftedReg32
            in
                toAssembler(rest, code <::> instr{regM=regM, regN=regN, regD=regD, shift=shift})
            end

        |   toAssembler(AddExtendedReg{regM, regN, regD, extend, opSize, setFlags} :: rest, code) =
            (* Add/SubExtended are only used to access XSP. *)
            let
                val instr =
                    case (opSize, setFlags) of
                        (OpSize64, false) => addExtendedReg
                    |   (OpSize32, false) => raise InternalError "AddExtendedReg; 32"
                    |   (OpSize64, true) => addSExtendedReg
                    |   (OpSize32, true) => raise InternalError "AddExtendedReg; 32"
            in
                toAssembler(rest, code <::> instr{regM=regM, regN=regN, regD=regD, extend=extend})
            end

        |   toAssembler(SubExtendedReg{regM, regN, regD, extend, opSize, setFlags} :: rest, code) =
            let
                val instr =
                    case (opSize, setFlags) of
                        (OpSize64, false) => subExtendedReg
                    |   (OpSize32, false) => raise InternalError "AddExtendedReg; 32"
                    |   (OpSize64, true) => subSExtendedReg
                    |   (OpSize32, true) => raise InternalError "AddExtendedReg; 32"
            in
                toAssembler(rest, code <::> instr{regM=regM, regN=regN, regD=regD, extend=extend})
            end

        |   toAssembler(MultiplyAndAddSub{regM, regN, regA, regD, multKind} :: rest, code) =
            let
                val instr =
                    case multKind of
                        MultAdd32 => multiplyAndAdd32{regM=regM, regN=regN, regA=regA, regD=regD}
                    |   MultSub32 => multiplyAndSub32{regM=regM, regN=regN, regA=regA, regD=regD}
                    |   MultAdd64 => multiplyAndAdd{regM=regM, regN=regN, regA=regA, regD=regD}
                    |   MultSub64 => multiplyAndSub{regM=regM, regN=regN, regA=regA, regD=regD}
                    |   SignedMultAddLong => signedMultiplyAndAddLong{regM=regM, regN=regN, regA=regA, regD=regD}
                    |   SignedMultHigh => signedMultiplyHigh{regM=regM, regN=regN, regD=regD}
            in
                toAssembler(rest, code <::> instr)
            end

        |   toAssembler(DivideRegs{regM, regN, regD, isSigned, opSize} :: rest, code) =
            let
                val instr =
                    case (isSigned, opSize) of
                        (true, OpSize64) => signedDivide
                    |   (true, OpSize32) => signedDivide32
                    |   (false, OpSize64) => unsignedDivide
                    |   (false, OpSize32) => unsignedDivide32
            in
                toAssembler(rest, code <::> instr{regN=regN, regM=regM, regD=regD})
            end

        |   toAssembler(LogicalShiftedReg{regM, regN, regD, shift, logOp, opSize, setFlags} :: rest, code) =
            let
                val instr =
                    case (logOp, setFlags, opSize) of
                        (LogAnd, false, OpSize64) => andShiftedReg
                    |   (LogAnd, true, OpSize64) => andsShiftedReg
                    |   (LogOr, false, OpSize64) => orrShiftedReg
                    |   (LogXor, false, OpSize64) => eorShiftedReg

                    |   (LogAnd, false, OpSize32) => andShiftedReg32
                    |   (LogAnd, true, OpSize32) => andsShiftedReg32
                    |   (LogOr, false, OpSize32) => orrShiftedReg32
                    |   (LogXor, false, OpSize32) => eorShiftedReg32

                    |   _ => raise InternalError "setFlags not valid with OR or XOR"
                (* There are also versions of AND/OR/XOR which operate on a complement (NOT)
                   of the shifted register.  It's probably not worth looking for a use for them. *)
            in
                toAssembler(rest, code <::> instr{regN=regN, regM=regM, regD=regD, shift=shift})
            end

        |   toAssembler(LoadRegScaled{regT, regN, unitOffset, loadType} :: rest, code) =
            let
                val instr =
                    case loadType of
                        Load64 => loadRegScaled
                    |   Load32 => loadRegScaled32
                    |   Load16 => loadRegScaled16
                    |   Load8 => loadRegScaledByte
            in
                toAssembler(rest, code <::> instr{regT=regT, regN=regN, unitOffset=unitOffset})
            end

        |   toAssembler(StoreRegScaled{regT, regN, unitOffset, loadType} :: rest, code) =
            let
                val instr =
                    case loadType of
                        Load64 => storeRegScaled
                    |   Load32 => storeRegScaled32
                    |   Load16 => storeRegScaled16
                    |   Load8 => storeRegScaledByte
            in
                toAssembler(rest, code <::> instr{regT=regT, regN=regN, unitOffset=unitOffset})
            end

        |   toAssembler(LoadFPRegScaled{regT, regN, unitOffset, floatSize} :: rest, code) =
            let
                val instr =
                    case floatSize of
                        Float32 => loadRegScaledFloat
                    |   Double64 => loadRegScaledDouble
            in
                toAssembler(rest, code <::> instr{regT=regT, regN=regN, unitOffset=unitOffset})
            end

        |   toAssembler(StoreFPRegScaled{regT, regN, unitOffset, floatSize} :: rest, code) =
            let
                val instr =
                    case floatSize of
                        Float32 => storeRegScaledFloat
                    |   Double64 => storeRegScaledDouble
            in
                toAssembler(rest, code <::> instr{regT=regT, regN=regN, unitOffset=unitOffset})
            end

        |   toAssembler(LoadRegUnscaled{regT, regN, byteOffset, loadType, unscaledType} :: rest, code) =
            let
                val instr =
                    case (loadType, unscaledType) of
                        (Load64, NoUpdate) => loadRegUnscaled
                    |   (Load32, NoUpdate) => loadRegUnscaled32
                    |   (Load16, NoUpdate) => loadRegUnscaled16
                    |   (Load8, NoUpdate) => loadRegUnscaledByte
                    |   (Load64, PreIndex) => loadRegPreIndex
                    |   (Load32, PreIndex) => loadRegPreIndex32
                    |   (Load16, PreIndex) => raise InternalError "loadRegPreIndex16"
                    |   (Load8, PreIndex) => loadRegPreIndexByte
                    |   (Load64, PostIndex) => loadRegPostIndex
                    |   (Load32, PostIndex) => loadRegPostIndex32
                    |   (Load16, PostIndex) => raise InternalError "loadRegPostIndex16"
                    |   (Load8, PostIndex) => loadRegPostIndexByte
            in
                toAssembler(rest, code <::> instr{regT=regT, regN=regN, byteOffset=byteOffset})
            end

        |   toAssembler(LoadFPRegUnscaled{regT, regN, byteOffset, floatSize, unscaledType} :: rest, code) =
            let
                val instr =
                    case (floatSize, unscaledType) of
                        (Float32, NoUpdate) => loadRegUnscaledFloat
                    |   (Double64, NoUpdate) => loadRegUnscaledDouble
                    |   _ => raise InternalError "LoadFPRegUnscaled: pre/post indexed"
            in
                toAssembler(rest, code <::> instr{regT=regT, regN=regN, byteOffset=byteOffset})
            end

        |   toAssembler(StoreRegUnscaled{regT, regN, byteOffset, loadType, unscaledType} :: rest, code) =
            let
                val instr =
                    case (loadType, unscaledType) of
                        (Load64, NoUpdate) => storeRegUnscaled
                    |   (Load32, NoUpdate) => storeRegUnscaled32
                    |   (Load16, NoUpdate) => storeRegUnscaled16
                    |   (Load8, NoUpdate) => storeRegUnscaledByte
                    |   (Load64, PreIndex) => storeRegPreIndex
                    |   (Load32, PreIndex) => storeRegPreIndex32
                    |   (Load16, PreIndex) => raise InternalError "storeRegPreIndex16"
                    |   (Load8, PreIndex) => storeRegPreIndexByte
                    |   (Load64, PostIndex) => storeRegPostIndex
                    |   (Load32, PostIndex) => storeRegPostIndex32
                    |   (Load16, PostIndex) => raise InternalError "storeRegPostIndex16"
                    |   (Load8, PostIndex) => storeRegPostIndexByte
            in
                toAssembler(rest, code <::> instr{regT=regT, regN=regN, byteOffset=byteOffset})
            end

        |   toAssembler(StoreFPRegUnscaled{regT, regN, byteOffset, floatSize, unscaledType} :: rest, code) =
            let
                val instr =
                    case (floatSize, unscaledType) of
                        (Float32, NoUpdate) => storeRegUnscaledFloat
                    |   (Double64, NoUpdate) => storeRegUnscaledDouble
                    |   _ => raise InternalError "StoreFPRegUnscaled: pre/post indexed"
            in
                toAssembler(rest, code <::> instr{regT=regT, regN=regN, byteOffset=byteOffset})
            end

        |   toAssembler(LoadRegIndexed{regT, regN, regM, loadType, option} :: rest, code) =
            let
                val instr =
                    case loadType of
                        Load64 => loadRegIndexed
                    |   Load32 => loadRegIndexed32
                    |   Load16 => loadRegIndexed16
                    |   Load8 => loadRegIndexedByte
            in
                toAssembler(rest, code <::> instr{regT=regT, regN=regN, regM=regM, option=option})
            end

        |   toAssembler(StoreRegIndexed{regT, regN, regM, loadType, option} :: rest, code) =
            let
                val instr =
                    case loadType of
                        Load64 => storeRegIndexed
                    |   Load32 => storeRegIndexed32
                    |   Load16 => storeRegIndexed16
                    |   Load8 => storeRegIndexedByte
            in
                toAssembler(rest, code <::> instr{regT=regT, regN=regN, regM=regM, option=option})
            end

        |   toAssembler(LoadFPRegIndexed{regT, regN, regM, floatSize, option} :: rest, code) =
            let
                val instr =
                    case floatSize of
                        Float32 => loadRegIndexedFloat
                    |   Double64 => loadRegIndexedDouble
            in
                toAssembler(rest, code <::> instr{regT=regT, regN=regN, regM=regM, option=option})
            end

        |   toAssembler(StoreFPRegIndexed{regT, regN, regM, floatSize, option} :: rest, code) =
            let
                val instr =
                    case floatSize of
                        Float32 => storeRegIndexedFloat
                    |   Double64 => storeRegIndexedDouble
            in
                toAssembler(rest, code <::> instr{regT=regT, regN=regN, regM=regM, option=option})
            end

        |   toAssembler(LoadAcquireReg{regN, regT, loadType} :: rest, code) =
            let
                val loadInstr  =
                    case loadType of
                        Load64 => loadAcquire
                    |   Load32 => loadAcquire32
                    |   Load8 => loadAcquireByte
                    |   _ => raise InternalError "LoadAcquire: Unsupported size" (* Not used *)
            in
                toAssembler(rest, code <::> loadInstr{regT=regT, regN=regN})
            end

        |   toAssembler(StoreReleaseReg{regN, regT, loadType} :: rest, code) =
            let
                val storeInstr  =
                    case loadType of
                        Load64 => storeRelease
                    |   Load32 => storeRelease32
                    |   Load8 => storeReleaseByte
                    |   _ => raise InternalError "StoreRelease: Unsupported size" (* Not used *)
            in
                toAssembler(rest, code <::> storeInstr{regT=regT, regN=regN})
            end

        |   toAssembler(LoadAcquireExclusiveRegister{regN, regT} :: rest, code) =
                toAssembler(rest, code <::> loadAcquireExclusiveRegister{regN=regN, regT=regT})

        |   toAssembler(StoreReleaseExclusiveRegister{regN, regT, regS} :: rest, code) =
                toAssembler(rest, code <::> storeReleaseExclusiveRegister{regN=regN, regT=regT, regS=regS})

        |   toAssembler(MemBarrier :: rest, code) =
                toAssembler(rest, code <::> dmbIsh)

        |   toAssembler(AtomicExtension{ regT, regN, regS, atOp} :: rest, code) =
            let
                val instr =
                    case atOp of
                        LoadAddAL => loadAddAL | LoadUmaxAL => loadUMaxAL | SwapAL => swapAL
                    |   LoadAddAcquire => loadAddA | LoadUMaxAcquire => loadUMaxA | SwapRelease => swapL
            in
                toAssembler(rest, code <::> instr{regT=regT, regN=regN, regS=regS})
            end

        |   toAssembler(LoadRegPair{ regT1, regT2, regN, unitOffset, loadType, unscaledType} :: rest, code) =
            let
                val _ = regT1 <> regT2 orelse raise InternalError "LoadRegPair: same register"
                val instr =
                    case (loadType, unscaledType) of
                        (Load64, NoUpdate) => loadPairOffset
                    |   (Load64, PreIndex) => loadPairPreIndexed
                    |   (Load64, PostIndex) => loadPairPostIndexed
                    |   (Load32, NoUpdate) => loadPairOffset32
                    |   (Load32, PreIndex) => loadPairPreIndexed32
                    |   (Load32, PostIndex) => loadPairPostIndexed32
                    |   _ => raise InternalError "LoadRegPair: unimplemented"
            in
                toAssembler(rest, code <::> instr{regT1=regT1, regT2=regT2, regN=regN, unitOffset=unitOffset})
            end

        |   toAssembler(StoreRegPair{ regT1, regT2, regN, unitOffset, loadType, unscaledType} :: rest, code) =
            let
                val instr =
                    case (loadType, unscaledType) of
                        (Load64, NoUpdate) => storePairOffset
                    |   (Load64, PreIndex) => storePairPreIndexed
                    |   (Load64, PostIndex) => storePairPostIndexed
                    |   (Load32, NoUpdate) => storePairOffset32
                    |   (Load32, PreIndex) => storePairPreIndexed32
                    |   (Load32, PostIndex) => storePairPostIndexed32
                    |   _ => raise InternalError "StoreRegPair: unimplemented"
            in
                toAssembler(rest, code <::> instr{regT1=regT1, regT2=regT2, regN=regN, unitOffset=unitOffset})
            end

        |   toAssembler(LoadFPRegPair{ regT1, regT2, regN, unitOffset, floatSize, unscaledType} :: rest, code) =
            let
                val _ = regT1 <> regT2 orelse raise InternalError "LoadRegPair: same register"
                val instr =
                    case (floatSize, unscaledType) of
                        (Double64, NoUpdate) => loadPairOffsetDouble
                    |   (Double64, PreIndex) => loadPairPreIndexedDouble
                    |   (Double64, PostIndex) => loadPairPostIndexedDouble
                    |   (Float32, NoUpdate) => loadPairOffsetFloat
                    |   (Float32, PreIndex) => loadPairPreIndexedFloat
                    |   (Float32, PostIndex) => loadPairPostIndexedFloat
            in
                toAssembler(rest, code <::> instr{regT1=regT1, regT2=regT2, regN=regN, unitOffset=unitOffset})
            end

        |   toAssembler(StoreFPRegPair{ regT1, regT2, regN, unitOffset, floatSize, unscaledType} :: rest, code) =
            let
                val instr =
                    case (floatSize, unscaledType) of
                        (Double64, NoUpdate) => storePairOffsetDouble
                    |   (Double64, PreIndex) => storePairPreIndexedDouble
                    |   (Double64, PostIndex) => storePairPostIndexedDouble
                    |   (Float32, NoUpdate) => storePairOffsetFloat
                    |   (Float32, PreIndex) => storePairPreIndexedFloat
                    |   (Float32, PostIndex) => storePairPostIndexedFloat
            in
                toAssembler(rest, code <::> instr{regT1=regT1, regT2=regT2, regN=regN, unitOffset=unitOffset})
            end

        |   toAssembler(ConditionalSet{regD, regTrue, regFalse, cond, condSet, opSize} :: rest, code) =
            let
                val instr =
                    case (condSet, opSize) of
                        (CondSet, OpSize64) => conditionalSet
                    |   (CondSetIncr, OpSize64) => conditionalSetIncrement
                    |   (CondSetInvert, OpSize64) => conditionalSetInverted
                    |   (CondSetNegate, OpSize64) => conditionalSetNegated
                    |   (CondSet, OpSize32) => conditionalSet32
                    |   (CondSetIncr, OpSize32) => conditionalSetIncrement32
                    |   (CondSetInvert, OpSize32) => conditionalSetInverted32
                    |   (CondSetNegate, OpSize32) => conditionalSetNegated32
            in
                toAssembler(rest, code <::> instr{regD=regD, regTrue=regTrue, regFalse=regFalse, cond=cond})
            end

        |   toAssembler(BitField{immr, imms, regN, regD, opSize, bitfieldKind} :: rest, code) =
            let
                val bfInstr =
                    case (bitfieldKind, opSize) of
                        (BFSigned, OpSize64) => signedBitfieldMove64
                    |   (BFUnsigned, OpSize64) => unsignedBitfieldMove64
                    |   (BFInsert, OpSize64) => bitfieldMove64
                    |   (BFSigned, OpSize32) => signedBitfieldMove32
                    |   (BFUnsigned, OpSize32) => unsignedBitfieldMove32
                    |   (BFInsert, OpSize32) => bitfieldMove32
            in
                toAssembler(rest, code <::> bfInstr{immr=immr, imms=imms, regN=regN, regD=regD})
            end

        |   toAssembler(ShiftRegisterVariable{regM, regN, regD, opSize, shiftDirection} :: rest, code) =
            let
                val instr =
                    case (shiftDirection, opSize) of
                        (ShiftLeft, OpSize64) => logicalShiftLeftVariable
                    |   (ShiftLeft, OpSize32) => logicalShiftLeftVariable32
                    |   (ShiftRightLogical, OpSize64) => logicalShiftRightVariable
                    |   (ShiftRightLogical, OpSize32) => logicalShiftRightVariable32
                    |   (ShiftRightArithmetic, OpSize64) => arithmeticShiftRightVariable
                    |   (ShiftRightArithmetic, OpSize32) => arithmeticShiftRightVariable32
            in
                toAssembler(rest, code <::> instr{regN=regN, regM=regM, regD=regD})
            end

        |   toAssembler(BitwiseLogical{ bits, regN, regD, opSize, setFlags, logOp} :: rest, code) =
            let
                val instr =
                    case (logOp, setFlags, opSize) of
                        (LogAnd, false, OpSize64) => bitwiseAndImmediate
                    |   (LogAnd, true, OpSize64) => bitwiseAndSImmediate
                    |   (LogOr, false, OpSize64) => bitwiseOrImmediate
                    |   (LogXor, false, OpSize64) => bitwiseXorImmediate

                    |   (LogAnd, false, OpSize32) => bitwiseAndImmediate32
                    |   (LogAnd, true, OpSize32) => bitwiseAndSImmediate32
                    |   (LogOr, false, OpSize32) => bitwiseOrImmediate32
                    |   (LogXor, false, OpSize32) => bitwiseXorImmediate32

                    |   _ => raise InternalError "flags not valid with OR or XOR"
            in
                toAssembler(rest, code <::> instr{regN=regN, regD=regD, bits=bits})
            end

        |   toAssembler(CountLeadingZeros{ regN, regD, opSize=OpSize32} :: rest, code) =
                toAssembler(rest, code <::> countLeadingZeros32{regN=regN, regD=regD})

        |   toAssembler(CountLeadingZeros{ regN, regD, opSize=OpSize64} :: rest, code) =
                toAssembler(rest, code <::> countLeadingZeros{regN=regN, regD=regD})

        |   toAssembler(MoveGeneralToFP{ regN, regD, floatSize=Float32} :: rest, code) =
                toAssembler(rest, code <::> moveGeneralToFloat{regN=regN, regD=regD})
        |   toAssembler(MoveGeneralToFP{ regN, regD, floatSize=Double64} :: rest, code) =
                toAssembler(rest, code <::> moveGeneralToDouble{regN=regN, regD=regD})

        |   toAssembler(MoveFPToGeneral{ regN, regD, floatSize=Float32} :: rest, code) =
                toAssembler(rest, code <::> moveFloatToGeneral{regN=regN, regD=regD})
        |   toAssembler(MoveFPToGeneral{ regN, regD, floatSize=Double64} :: rest, code) =
                toAssembler(rest, code <::> moveDoubleToGeneral{regN=regN, regD=regD})

        |   toAssembler(CvtIntToFP{ regN, regD, floatSize, opSize} :: rest, code) =
            let
                val instr =
                    case (opSize, floatSize) of
                        (OpSize32, Float32) => convertInt32ToFloat
                    |   (OpSize64, Float32) => convertIntToFloat
                    |   (OpSize32, Double64) => convertInt32ToDouble
                    |   (OpSize64, Double64) => convertIntToDouble
            in
                toAssembler(rest, code <::> instr{regN=regN, regD=regD})
            end

        |   toAssembler(CvtFloatToInt{ round, regN, regD, floatSize, opSize} :: rest, code) =
            let
                val instr =
                    case (floatSize, opSize) of
                        (Float32, OpSize32) => convertFloatToInt32
                    |   (Float32, OpSize64) => convertFloatToInt
                    |   (Double64, OpSize32) => convertDoubleToInt32
                    |   (Double64, OpSize64) => convertDoubleToInt
            in
                toAssembler(rest, code <::> instr round {regN=regN, regD=regD})
            end

        |   toAssembler(FPBinaryOp{ regM, regN, regD, floatSize, fpOp} :: rest, code) =
            let
                val instr =
                    case (fpOp, floatSize) of
                        (MultiplyFP, Float32) => multiplyFloat
                    |   (DivideFP, Float32) => divideFloat
                    |   (AddFP, Float32) => addFloat
                    |   (SubtractFP, Float32) => subtractFloat
                    |   (MultiplyFP, Double64) => multiplyDouble
                    |   (DivideFP, Double64) => divideDouble
                    |   (AddFP, Double64) => addDouble
                    |   (SubtractFP, Double64) => subtractDouble
            in
                toAssembler(rest, code <::> instr {regN=regN, regM=regM, regD=regD})
            end

        |   toAssembler(FPComparison{ regM, regN, floatSize} :: rest, code) =
                toAssembler(rest, code <::> (case floatSize of Float32 => compareFloat | Double64 => compareDouble){regN=regN, regM=regM})

        |   toAssembler(FPUnaryOp{ regN, regD, fpOp} :: rest, code) =
            let
                val instr =
                    case fpOp of
                        NegFloat => negFloat | NegDouble => negDouble
                    |   AbsFloat => absFloat | AbsDouble => absDouble
                    |   ConvFloatToDble => convertFloatToDouble
                    |   ConvDbleToFloat => convertDoubleToFloat
                    |   MoveDouble => moveDoubleToDouble
                    |   MoveFloat => moveFloatToFloat
            in
                toAssembler(rest, code <::> instr {regN=regN, regD=regD})
            end

        |   toAssembler(SetLabel(Label lab) :: rest, code) = toAssembler(rest, code <::> setLabel(getLabelTarget lab))

        |   toAssembler(ConditionalBranch(cond, Label lab) :: rest, code) = toAssembler(rest, code <::> conditionalBranch(cond, getLabelTarget lab))

        |   toAssembler(UnconditionalBranch(Label lab) :: rest, code) = toAssembler(rest, code <::> unconditionalBranch(getLabelTarget lab))

        |   toAssembler(BranchAndLink(Label lab) :: rest, code) = toAssembler(rest, code <::> branchAndLink(getLabelTarget lab))

        |   toAssembler(BranchReg{regD, brRegType=BRRBranch} :: rest, code) = toAssembler(rest, code <::> branchRegister regD)
        |   toAssembler(BranchReg{regD, brRegType=BRRAndLink} :: rest, code) = toAssembler(rest, code <::> branchAndLinkReg regD)
        |   toAssembler(BranchReg{regD, brRegType=BRRReturn} :: rest, code) = toAssembler(rest, code <::> returnRegister regD)

        |   toAssembler(LoadLabelAddress(reg, Label lab) :: rest, code) = toAssembler(rest, code <::> loadLabelAddress(reg, getLabelTarget lab))

        |   toAssembler(TestBitBranch{ test, bit, label=Label lab, onZero } :: rest, code) =
                toAssembler(rest, code <::> (if onZero then testBitBranchZero else testBitBranchNonZero)(test, bit, getLabelTarget lab))

        |   toAssembler(CompareBranch{ test, label=Label lab, onZero, opSize } :: rest, code) =
            let
                val instr =
                    case (onZero, opSize) of
                        (true, OpSize64) => compareBranchZero
                    |   (false, OpSize64) => compareBranchNonZero
                    |   (true, OpSize32) => compareBranchZero32
                    |   (false, OpSize32) => compareBranchNonZero32
            in
                toAssembler(rest, code <::> instr(test, getLabelTarget lab))
            end

            (* Register-register moves - special case for XSP. *)
        |   toAssembler(MoveXRegToXReg{sReg=XSP, dReg} :: rest, code) =
                toAssembler(rest, code <::> addImmediate{regN=XSP, regD=dReg, immed=0w0, shifted=false})
        |   toAssembler(MoveXRegToXReg{sReg, dReg=XSP} :: rest, code) =
                toAssembler(rest, code <::> addImmediate{regN=sReg, regD=XSP, immed=0w0, shifted=false})
        |   toAssembler(MoveXRegToXReg{sReg, dReg} :: rest, code) =
                toAssembler(rest, code <::> orrShiftedReg{regN=XZero, regM=sReg, regD=dReg, shift=ShiftNone})

        |   toAssembler(LoadNonAddr(xReg, value) :: rest, code) =
            let
                (* Load a non-address constant.  Tries to use movz/movn/movk if
                   that can be done easily, othewise uses loadNonAddressConstant to
                   load the value from the non-address constant area. *)
                fun extW (v, h) = Word.andb(Word.fromLarge(LargeWord.>>(Word64.toLarge v, h*0w16)), 0wxffff)
                val hw0 = extW(value, 0w3) and hw1 = extW(value, 0w2)
                and hw2 = extW(value, 0w1) and hw3 = extW(value, 0w0)
                val nextCode =
                    if value < 0wx100000000
                    then
                    let
                        (* 32-bit constants can be loaded using at most a movz and movk but
                           various cases can be reduced since all 32-bit operations set
                           the top word to zero. *)
                        val hi = hw2
                        and lo = hw3
                    in
                        (* 32-bit constants can be loaded with at most a movz and a movk but
                           it may be that there is something shorter. *)
                        if hi = 0w0
                        then code <::> moveZero32{regD=xReg, immediate=lo, shift=0w0}
                        else if hi = 0wxffff
                        then code <::> moveNot32{regD=xReg, immediate=Word.xorb(0wxffff, lo), shift=0w0}
                        else if lo = 0w0
                        then code <::> moveZero32{regD=xReg, immediate=hi, shift=0w16}
                        else if isEncodableBitPattern(value, WordSize32)
                        then code <::> bitwiseOrImmediate32{bits=value, regN=XZero, regD=xReg}
                        else (* Have to use two instructions *)
                            code <::>
                                moveZero32{regD=xReg, immediate=lo, shift=0w0} <::>
                                moveKeep{regD=xReg, immediate=hi, shift=0w16}
                    end
                    else if isEncodableBitPattern(value, WordSize64)
                    then code <::> bitwiseOrImmediate{bits=value, regN=XZero, regD=xReg}
                    else if hw0 = 0wxffff andalso hw1 = 0wxffff andalso hw2 = 0wxffff
                    then code <::> moveNot{regD=xReg, immediate=Word.xorb(0wxffff, hw3), shift=0w0}
                    else if hw1 = 0w0 andalso hw2 = 0w0
                    then (* This is common for length words with a flags byte *)
                        code <::> moveZero32{regD=xReg, immediate=hw3, shift=0w0} <::>
                            moveKeep{regD=xReg, immediate=hw0, shift=0w48} 
                    else code <::> loadNonAddressConstant(xReg, value)
            in
                toAssembler(rest, nextCode)
            end

        |   toAssembler(LoadFPConst{dest, value, floatSize=Float32, work} :: rest, code) =
                toAssembler(rest, loadFloatConstant(dest, value, work)::code)

        |   toAssembler(LoadFPConst{dest, value, floatSize=Double64, work} :: rest, code) =
                toAssembler(rest, loadDoubleConstant(dest, value, work)::code)

        |   toAssembler(LoadAddr(dReg, source) :: rest, code) = toAssembler(rest, loadAddressConstant(dReg, source) :: code)

        |   toAssembler(RTSTrap{ rtsEntry, work, save } :: rest, code) =
            let
                (* Because X30 is used in the branchAndLink it has to be pushed
                   across any trap. *)
                val saveX30 = List.exists (fn r => r = X30) save
                val preserve = List.filter (fn r => r <> X30) save
            in
                toAssembler(rest,
                    code <@>
                        (if saveX30 then [storeRegPreIndex{regT=X30, regN=X_MLStackPtr, byteOffset= ~8}] else []) <::>
                    loadRegScaled{regT=work, regN=X_MLAssemblyInt, unitOffset=rtsEntry} <::>
                    branchAndLinkReg work <::>
                    registerMask preserve <@>
                    (if saveX30 then [loadRegPostIndex{regT=X30, regN=X_MLStackPtr, byteOffset= 8}] else [])
                )
            end

        |   toAssembler(AllocateMemoryFixedSize{ bytes, dest, save, work } :: rest, code) =
            let
                val _ =
                    case archType of
                        ArchC32 shift =>
                            let
                                val mask = Word.<<(0w2, shift) - 0w1
                            in
                                if Word.andb(bytes, mask) = 0w0 then () else raise InternalError "AllocateMemoryFixedSize"
                            end
                    |   ArchNative => ()
                val label = Arm64Assembly.createLabel()
                val saveX30 = List.exists (fn r => r = X30) save
                val preserve = List.filter (fn r => r <> X30) save

                val allocCode =
                    code <@>
                    (* Subtract the number of bytes required from the heap pointer. *)
                    (if bytes >= 0w4096
                    then [subShiftedReg{regM=work, regN=X_MLHeapAllocPtr, regD=dest, shift=ShiftNone},
                          loadNonAddressConstant(work, Word.toLarge bytes)]
                    else [subImmediate{regN=X_MLHeapAllocPtr, regD=dest, immed=bytes, shifted=false}]) <::>
                    (* Compare the result with the heap limit. *)
                    subSShiftedReg{regM=X_MLHeapLimit, regN=dest, regD=XZero, shift=ShiftNone} <::>
                    conditionalBranch(CondCarrySet, label) <@>
                        (if saveX30 then [storeRegPreIndex{regT=X30, regN=X_MLStackPtr, byteOffset= ~8}] else []) <::>
                    loadRegScaled{regT=work, regN=X_MLAssemblyInt, unitOffset=heapOverflowCallOffset} <::>
                    branchAndLinkReg work <::>
                    registerMask preserve <@>
                    (if saveX30 then [loadRegPostIndex{regT=X30, regN=X_MLStackPtr, byteOffset= 8}] else []) <::>
                    setLabel label <::>
                    (* Update the heap pointer. *)
                    orrShiftedReg{regN=XZero, regM=dest, regD=X_MLHeapAllocPtr, shift=ShiftNone}
            in
                toAssembler(rest, allocCode)
            end

        |   toAssembler(AllocateMemoryVariableSize{ sizeReg, dest, save, work } :: rest, code) =
            let
                val trapLabel = Arm64Assembly.createLabel() and noTrapLabel = Arm64Assembly.createLabel()
                val saveX30 = List.exists (fn r => r = X30) save
                val preserve = List.filter (fn r => r <> X30) save
            
                val allocCode =
                    (
                        (* Subtract the size into the result register.  Subtract a further word for
                           the length word and round down in 32-in-64. *)
                        case archType of
                            ArchC32 shift =>
                            let
                                val mask = ~(Word64.<<(0w2, shift)) (* Bytes *)
                            in
                                code <::>
                                    subShiftedReg{regM=sizeReg, regN=X_MLHeapAllocPtr, regD=dest, shift=ShiftLSL 0w2} <::>
                                    subImmediate{regN=dest, regD=dest, immed=0w4, shifted=false} <::>
                                    bitwiseAndImmediate{bits=mask, regN=dest, regD=dest}
                            end
                        |   ArchNative =>
                                code <::>
                                subShiftedReg{regM=sizeReg, regN=X_MLHeapAllocPtr, regD=dest, shift=ShiftLSL 0w3} <::>
                                subImmediate{regN=dest, regD=dest, immed=0w8, shifted=false}
                    ) <::>
                    (* Check against the limit. If the size is large enough it is possible that this could wrap round. 
                       To check for that we trap if either the result is less than the limit or if it is
                       now greater than the allocation pointer. *)
                    subSShiftedReg{regM=X_MLHeapLimit, regN=dest, regD=XZero, shift=ShiftNone} <::>
                    conditionalBranch(CondCarryClear, trapLabel) <::>
                    subSShiftedReg{regM=X_MLHeapAllocPtr, regN=dest, regD=XZero, shift=ShiftNone} <::>
                    conditionalBranch(CondCarryClear, noTrapLabel) <::>
                    setLabel trapLabel <@>
                        (if saveX30 then [storeRegPreIndex{regT=X30, regN=X_MLStackPtr, byteOffset= ~8}] else []) <::>
                    loadRegScaled{regT=work, regN=X_MLAssemblyInt, unitOffset=heapOverflowCallOffset} <::>
                    branchAndLinkReg work <::>
                    registerMask preserve <@>
                    (if saveX30 then [loadRegPostIndex{regT=X30, regN=X_MLStackPtr, byteOffset= 8}] else []) <::>
                    setLabel noTrapLabel <::>
                    (* Update the heap pointer. *)
                    orrShiftedReg{regN=XZero, regM=dest, regD=X_MLHeapAllocPtr, shift=ShiftNone}
            in
                toAssembler(rest, allocCode)
            end

        |   toAssembler(BranchTable{ startLabel=Label lab, brTable } :: rest, code) =
                toAssembler(rest,
                    List.foldl (fn (Label lab, code) => (unconditionalBranch(getLabelTarget lab)) :: code)
                        (code <::> setLabel(getLabelTarget lab)) brTable)

        |   toAssembler(LoadGlobalHeapBaseInCallback dest :: rest, code) =
                toAssembler(rest,
                    code <@> List.rev(loadGlobalHeapBaseInCallback dest))

        |   toAssembler(Yield :: rest, code) =
                toAssembler(rest, code <::> yield)

        (* Optimisation passes. *)
        fun isValidForPair(offset1, offset2) =
            let val v = Int.min(offset1, offset2) in v >= ~64 andalso v < 64 end

        fun forward([], list, rep) = reverse(list, [], rep)

        |   forward(SetLabel(Label srcLab) :: (ubr as UnconditionalBranch(Label destLab)) :: tl, list, _) =
            if srcLab = destLab
            (* We should never get this because there should always be a stack-check to
               allow a loop to be broken.  If that ever changes we need to retain the label. *)
            then raise InternalError "Infinite loop detected"
            else (* Mark this to forward to its destination. *)
            (
                Array.update(labelTargets, srcLab, getLabel destLab);
                forward(ubr :: tl, list, true)
            )

        |   forward(SetLabel(Label jmpLab1) :: (tl as SetLabel(Label jmpLab2) :: _), list, _) =
            (* Eliminate adjacent labels.  They complicate the other tests although they
               don't incur any run-time cost. *)
            (
                (* Any reference to the first label is forwarded to the second. *)
                Array.update(labelTargets, jmpLab1, getLabel jmpLab2);
                forward(tl, list, true)
            )

        |   forward((ubr as UnconditionalBranch(Label ubrLab)) :: (tl as SetLabel(Label jumpLab) :: _), list, rep) =
                (* Eliminate unconditional jumps to the next instruction. *)
            if ubrLab = jumpLab
            then forward(tl, list, true)
            else forward(tl, ubr :: list, rep)

        |   forward((cbr as ConditionalBranch(test, Label cbrLab)) :: (ubr as UnconditionalBranch(Label ubrLab)) ::
                    (tl as SetLabel(Label jumpLab) :: _), list, rep) =
            if cbrLab = jumpLab
            then (* We have a conditional branch followed by an unconditional branch followed by the destination of
                    the conditional branch.  Eliminate the unconditional branch by reversing the test.
                    This can often happen if one branch of an if-then-else has been reduced to zero
                    because the same register has been chosen for the input and output. *)
                forward(tl (* Leave the label just in case it's used elsewhere*),
                    ConditionalBranch(invertTest test, Label ubrLab) :: list, true)

            else forward(ubr :: tl, cbr :: list, rep)

        |   forward((load as LoadRegScaled{regT=regT1, regN=regN1, unitOffset=offset1, loadType=lt1}) ::
                     (tl1 as LoadRegScaled{regT=regT2, regN=regN2, unitOffset=offset2, loadType=lt2} ::tl2), list, rep) =
            (* Two adjacent loads - can this be converted to load-pair?  N.B.  We have to be careful about the
               sequence ldr x0,[x0]; ldr x1,[x0+8] which isn't the same at all. *)
            if regN1 = regN2 andalso regN1 <> regT1 andalso lt1 = lt2 andalso (offset2 = offset1 + 1 orelse offset2 = offset1 - 1) andalso
                (case lt1 of Load64 => true | Load32 => true | _ => false) andalso isValidForPair(offset1, offset2)
            then
            let
                val (reg1, reg2, offset) =
                    if offset1 < offset2 then (regT1, regT2, offset1) else (regT2, regT1, offset2)
            in
                forward(tl2,
                    LoadRegPair{ regT1=reg1, regT2=reg2, regN=regN1, unitOffset=offset, loadType=lt1, unscaledType=NoUpdate} :: list, true)
            end
            else forward(tl1, load :: list, rep)

        |   forward((store as StoreRegScaled{regT=regT1, regN=regN1, unitOffset=offset1, loadType=lt1}) ::
                     (tl1 as StoreRegScaled{regT=regT2, regN=regN2, unitOffset=offset2, loadType=lt2} ::tl2), list, rep) =
            (* Two adjacent stores - can this be converted to store-pair? *)
            if regN1 = regN2 andalso lt1 = lt2 andalso (offset2 = offset1 + 1 orelse offset2 = offset1 - 1) andalso
                (case lt1 of Load64 => true | Load32 => true | _ => false) andalso isValidForPair(offset1, offset2)
            then
            let
                val (reg1, reg2, offset) =
                    if offset1 < offset2 then (regT1, regT2, offset1) else (regT2, regT1, offset2)
            in
                forward(tl2,
                    StoreRegPair{ regT1=reg1, regT2=reg2, regN=regN1, unitOffset=offset, loadType=lt1, unscaledType=NoUpdate} :: list, true)
            end
            else forward(tl1, store :: list, rep)

        |   forward((store as StoreRegUnscaled{regT=regT1, regN=regN1, byteOffset= ~8, loadType=Load64, unscaledType=NoUpdate}) ::
                     (tl1 as StoreRegScaled{regT=regT2, regN=regN2, unitOffset=0, loadType=Load64} ::tl2), list, rep) =
            (* Common case - store length word and then the first word of the cell. *)
            if regN1 = regN2
            then forward(tl2,
                    StoreRegPair{ regT1=regT1, regT2=regT2, regN=regN1, unitOffset= ~1, loadType=Load64, unscaledType=NoUpdate} :: list, true)
            else forward(tl1, store :: list, rep)

        |   forward((store as StoreRegUnscaled{regT=regT1, regN=regN1, byteOffset= ~4, loadType=Load32, unscaledType=NoUpdate}) ::
                     (tl1 as StoreRegScaled{regT=regT2, regN=regN2, unitOffset=0, loadType=Load32} ::tl2), list, rep) =
            (* Common case - store length word and then the first word of the cell. *)
            if regN1 = regN2
            then forward(tl2,
                    StoreRegPair{ regT1=regT1, regT2=regT2, regN=regN1, unitOffset= ~1, loadType=Load32, unscaledType=NoUpdate} :: list, true)
            else forward(tl1, store :: list, rep)

        |   forward((store as StoreRegUnscaled{regT=regT1, regN=regN1, byteOffset= ~8, loadType=Load64, unscaledType=PreIndex}) ::
                     (tl1 as StoreRegUnscaled{regT=regT2, regN=regN2, byteOffset= ~8, loadType=Load64, unscaledType=PreIndex} :: tl2), list, rep) =
            (* Adjacent pushes T2 is in the lower address so the order is T2, T1.  The stack is always 64-bit aligned so
               this works on both native addressing and 32-in-64. *)
            if regN1 = regN2
            then forward(tl2,
                    StoreRegPair{ regT1=regT2, regT2=regT1, regN=regN1, unitOffset= ~2, loadType=Load64, unscaledType=PreIndex} :: list, true)
            else forward(tl1, store :: list, rep)

        |   forward((add1 as AddImmediate{regN=regN1, regD=regD1, immed=immed1, shifted=false, opSize=OpSize64, setFlags=false}) ::
                    (tl1 as AddImmediate{regN=regN2, regD=regD2, immed=immed2, shifted=false, opSize=OpSize64, setFlags=false}  ::tl2), list, rep) =
            (* Adjacent stack resets.  This can apply more generally but only if the result registers are the same.  If they're
               not we may need the intermediate result.  We put the result back into the input stream in case it can be combined
               with another stack reset. *)
            if regN2 = regD2 andalso regD1 = regD2 andalso immed2+immed1 < 0w4096
            then forward(AddImmediate{regN=regN1, regD=regD2, immed=immed2+immed1, shifted=false, opSize=OpSize64, setFlags=false} :: tl2, list, true)
            else forward(tl1, add1 :: list, rep)

        |   forward((bwl as BitwiseLogical{bits, regN, regD=XZero, logOp=LogAnd, opSize=_, setFlags=true}) ::
                    (tl1 as ConditionalBranch(cond, label) :: tl2), list, rep) =
            (* This occurs frequently as a either a test of the tag bit (bit 0) or a test of a
               boolean (bit 1).  Other cases don't currently occur because we don't recognise
               logical and followed by a comparison as a special case. *)
            (
                case (cond, powerTwo bits) of
                    (CondEqual, SOME shift) =>
                        forward(TestBitBranch{test=regN, bit=shift, label=label, onZero=true} :: tl2, list, true)
                |   (CondNotEqual, SOME shift) =>
                        forward(TestBitBranch{test=regN, bit=shift, label=label, onZero=false} :: tl2, list, true)
                |   _ => forward(tl1, bwl :: list, rep)
            )

        |   forward(hd :: tl, list, rep) = forward(tl, hd :: list, rep) 
        
        and reverse([], list, rep) = (list, rep)

        |   reverse((add as AddImmediate{regN=regN2, regD=regD2, immed, shifted=false, opSize=OpSize64, setFlags=false}) ::
                     (tl1 as LoadRegScaled{regT=regT1, regN=regN1, unitOffset=0, loadType=Load64} ::tl2), list, rep) =
            (* A stack reset occurring after a load.  This is usually the ML SP but can also occur with C memory ops.
               It might be possible to consider other cases. *)
            if regN1 = regD2 andalso regN2 = regD2 andalso regT1 <> regN1 andalso immed < 0w256
            then reverse(tl2,
                    LoadRegUnscaled{regT=regT1, regN=regN1, byteOffset=Word.toInt immed, loadType=Load64, unscaledType=PostIndex} :: list, true)
            else reverse(tl1, add :: list, rep)

        |   reverse((add as AddImmediate{regN=regN2, regD=regD2, immed, shifted=false, opSize=OpSize64, setFlags=false}) ::
                     (tl1 as LoadRegPair{regT1=regT1, regT2=regT2, regN=regN1, unitOffset=0, loadType=Load64, unscaledType=NoUpdate} ::tl2), list, rep) =
            (* A stack reset occurring after a load pair *)
            if regN1 = regD2 andalso regN2 = regD2 andalso regT1 <> regN1 andalso regT2 <> regN1 andalso immed < 0w64 * 0w8
            then reverse(tl2,
                    LoadRegPair{regT1=regT1, regT2=regT2, regN=regN1, unitOffset=Word.toInt(immed div 0w8),
                                loadType=Load64, unscaledType=PostIndex} :: list, true)
            else reverse(tl1, add :: list, rep)

        |   reverse(hd :: tl, list, rep) = reverse(tl, hd :: list, rep)

        (* Repeat scans through the code until there are no further changes. *)
        fun repeat ops =
            case forward(ops, [], false) of
                (list, false) => list
            |   (list, true) => repeat list

        val optimised = repeat instrs

    in
        generateCode{instrs=List.rev(toAssembler(optimised, [])), name=name, parameters=parameters,
                     resultClosure=resultClosure, profileObject=profileObject}
    end

    (* Constant shifts are encoded in the immr and imms fields of the bit-field instruction. *)
    fun shiftConstant{ direction, regD, regN, shift, opSize } =
    let
        val (bitfieldKind, immr, imms) =
            case (direction, opSize) of
                (ShiftLeft, OpSize64) => (BFUnsigned, Word.~ shift mod 0w64, 0w64-0w1-shift)
            |   (ShiftLeft, OpSize32) => (BFUnsigned, Word.~ shift mod 0w32, 0w32-0w1-shift)
            |   (ShiftRightLogical, OpSize64) => (BFUnsigned, shift, 0wx3f)
            |   (ShiftRightLogical, OpSize32) => (BFUnsigned, shift, 0wx1f)
            |   (ShiftRightArithmetic, OpSize64) => (BFSigned, shift, 0wx3f)
            |   (ShiftRightArithmetic, OpSize32) => (BFSigned, shift, 0wx1f)
    in
        BitField{ regN=regN, regD=regD, opSize=opSize, immr=immr, imms=imms, bitfieldKind=bitfieldKind }
    end

    (* These sequences are used both in the ML code-generator and in the FFI code so it
       is convenient to have them here and share the code. *)
    local
        fun allocateWords(fixedReg, workReg, words, regMask, code) =
        let
            val bytes =
                case archType of
                    ArchNative => (words + 0w1) * 0w8
                |   ArchC32 shift =>
                    let
                        (* Round up to unit size and add the length word *)
                        val unitSize = Word.<<(0w2, shift) (* bytes *)
                    in
                        Word.andb(words*0w4+unitSize-0w1+0w4, ~ unitSize)
                    end
            val (lengthWord, setLength, flagShift) =
                case archType of
                    ArchC32 _ => (~4, Load32, 0w24)
                |   ArchNative => (~8, Load64, 0w56)
        in
            code <::>
            AllocateMemoryFixedSize{ bytes=bytes, dest=fixedReg, save=regMask, work=X16 } <::>
            LoadNonAddr(workReg,
                    Word64.orb(Word.toLarge words, Word64.<<(Word64.fromLarge(Word8.toLarge Address.F_bytes), flagShift))) <::>
            (* Store the length word.  Have to use the unaligned version because offset is -ve. *)
            StoreRegUnscaled{regT=workReg, regN=fixedReg, byteOffset= lengthWord, loadType=setLength, unscaledType=NoUpdate}
        end

        fun absoluteAddressToIndex(reg, code) =
            case archType of
                ArchC32 shiftBits =>
                    code <::>
                    SubShiftedReg{regM=X_Base32in64, regN=reg, regD=reg, shift=ShiftNone, opSize=OpSize64, setFlags=false} <::>
                    shiftConstant{direction=ShiftRightLogical, regN=reg, regD=reg, shift=shiftBits, opSize=OpSize64}
                |   ArchNative => code
    in
        fun boxDouble({source, destination, workReg, saveRegs}, code) =
            absoluteAddressToIndex(destination,
                allocateWords(destination, workReg,
                    case archType of ArchC32 _ => 0w2 |	ArchNative => 0w1, saveRegs, code) <::>
                    StoreFPRegScaled{regT=source, regN=destination, unitOffset=0, floatSize=Double64})
                
        and boxSysWord({source, destination, workReg, saveRegs}, code) =
            absoluteAddressToIndex(destination,
                allocateWords(destination, workReg, case archType of ArchC32 _ => 0w2 | ArchNative => 0w1, saveRegs, code) <::>
                    StoreRegScaled{regT=source, regN=destination, unitOffset=0, loadType=Load64})

        and boxFloat({source, destination, workReg, saveRegs}, code) =
            absoluteAddressToIndex(destination, 
                allocateWords(destination, workReg, 0w1, saveRegs, code) <::>
                    StoreFPRegScaled{regT=source, regN=destination, unitOffset=0, floatSize=Float32})
    end

    structure Sharing =
    struct
        type closureRef = closureRef
        type loadType = loadType
        type opSize = opSize
        type logicalOp = logicalOp
        type floatSize = floatSize
        type shiftDirection = shiftDirection
        type multKind = multKind
        type fpUnary = fpUnary
        type fpBinary = fpBinary
        type unscaledType = unscaledType
        type condSet = condSet
        type bitfieldKind = bitfieldKind
        type brRegType = brRegType
        type precode = precode
        type xReg = xReg
        type vReg = vReg
        type label = label
        type labelMaker = labelMaker
        type condition = condition
        type shiftType = shiftType
        type wordSize = wordSize
        type 'a extend = 'a extend
        type scale = scale
        type instr = instr
        type atomicOp = atomicOp
    end

end;
