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
signature ARM64PREASSEMBLY =
sig
    type closureRef
    type machineWord = Address.machineWord

    (* XZero and XSP are both encoded as 31 but the interpretation
       depends on the instruction 
       The datatype definition is included here to allow for
       pattern matching on XSP and XZero. *)
    datatype xReg = XReg of Word8.word | XZero | XSP
    and vReg = VReg of Word8.word

    val X0:  xReg   and X1:  xReg   and X2:  xReg   and X3: xReg
    and X4:  xReg   and X5:  xReg   and X6:  xReg   and X7: xReg
    and X8:  xReg   and X9:  xReg   and X10: xReg   and X11: xReg
    and X12: xReg   and X13: xReg   and X14: xReg   and X15: xReg
    and X16: xReg   and X17: xReg   and X18: xReg   and X19: xReg
    and X20: xReg   and X21: xReg   and X22: xReg   and X23: xReg
    and X24: xReg   and X25: xReg   and X26: xReg   and X27: xReg
    and X28: xReg   and X29: xReg   and X30: xReg

    val X_MLHeapLimit: xReg (* ML Heap limit pointer *)
    and X_MLAssemblyInt: xReg (* ML assembly interface pointer. *)
    and X_MLHeapAllocPtr: xReg (* ML Heap allocation pointer. *)
    and X_MLStackPtr: xReg (* ML Stack pointer. *)
    and X_LinkReg: xReg (* Link reg - return address *)
    and X_Base32in64: xReg (* X24 is used for the heap base in 32-in-64. *)

    val V0:  vReg   and V1:  vReg   and V2:  vReg   and V3: vReg
    and V4:  vReg   and V5:  vReg   and V6:  vReg   and V7: vReg

    (* Condition for conditional branches etc. *)
    datatype condition =
        CondEqual            (* Z=1 *)
    |   CondNotEqual         (* Z=0 *)
    |   CondCarrySet         (* C=1 *)
    |   CondCarryClear       (* C=0 *)
    |   CondNegative         (* N=1 *)
    |   CondPositive         (* N=0 imcludes zero *)
    |   CondOverflow         (* V=1 *)
    |   CondNoOverflow       (* V=0 *)
    |   CondUnsignedHigher   (* C=1 && Z=0 *)
    |   CondUnsignedLowOrEq  (* ! (C=1 && Z=0) *)
    |   CondSignedGreaterEq  (* N=V *)
    |   CondSignedLess       (* N<>V *)
    |   CondSignedGreater    (* Z==0 && N=V *)
    |   CondSignedLessEq     (* !(Z==0 && N=V) *)

    val invertTest: condition -> condition (* i.e. jump when the condition is not true. *)
    val condToString: condition -> string

    datatype shiftType =
        ShiftLSL of Word8.word
    |   ShiftLSR of Word8.word
    |   ShiftASR of Word8.word
    |   ShiftNone

    datatype wordSize = WordSize32 | WordSize64

    datatype 'a extend =
        ExtUXTB of 'a (* Unsigned extend byte *)
    |   ExtUXTH of 'a (* Unsigned extend byte *)
    |   ExtUXTW of 'a (* Unsigned extend byte *)
    |   ExtUXTX of 'a (* Left shift *)
    |   ExtSXTB of 'a (* Sign extend byte *)
    |   ExtSXTH of 'a (* Sign extend halfword *)
    |   ExtSXTW of 'a (* Sign extend word *)
    |   ExtSXTX of 'a (* Left shift *)

    (* Load/store instructions have only a single bit for the shift.  For byte
       operations this is one bit shift; for others it scales by the size of
       the operand if set. *)
    datatype scale =
        ScaleOrShift
    |   NoScale

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

    type label and labelMaker
    val createLabelMaker: unit -> labelMaker
    and createLabel: labelMaker -> label

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
        (* Allocate memory - bytes includes the length word and rounding. *)
    |   AllocateMemoryFixedSize of { bytes: word, dest: xReg, save: xReg list, work: xReg }
        (* Allocate memory - sizeReg is number of ML words needed for cell. *)
    |   AllocateMemoryVariableSize of { sizeReg: xReg, dest: xReg, save: xReg list, work: xReg }
        (* Branch table for indexed case. startLabel is the address of the first label in
           the list.  The branch table is a sequence of unconditional branches. *)
    |   BranchTable of { startLabel: label, brTable: label list }
    |   LoadGlobalHeapBaseInCallback of xReg
    |   Yield

    (* Wrapper for BitField *)
    val shiftConstant: { direction: shiftDirection, regD: xReg, regN: xReg, shift: word, opSize: opSize } -> precode
    
    (* Convenient sequences.  N.B. These are in reverse order. *)
    val boxDouble:
        {source: vReg, destination: xReg, workReg: xReg, saveRegs: xReg list} * precode list -> precode list
    and boxFloat:
        {source: vReg, destination: xReg, workReg: xReg, saveRegs: xReg list} * precode list -> precode list
    and boxSysWord:
        {source: xReg, destination: xReg, workReg: xReg, saveRegs: xReg list} * precode list -> precode list

    (* Create the vector of code from the list of instructions and update the
       closure reference to point to it. *)
    val generateFinalCode:
        {instrs: precode list, name: string, parameters: Universal.universal list, resultClosure: closureRef,
         profileObject: machineWord, labelMaker: labelMaker} -> unit

    (* Offsets in the assembly code interface pointed at by X26
       These are in units of 64-bits NOT bytes. *)
    val heapOverflowCallOffset: int
    and stackOverflowCallOffset: int
    and stackOverflowXCallOffset: int
    and exceptionHandlerOffset: int
    and stackLimitOffset: int
    and threadIdOffset: int
    and heapLimitPtrOffset: int
    and heapAllocPtrOffset: int
    and mlStackPtrOffset: int
    and exceptionPacketOffset: int

	datatype archType = ArchNative | ArchC32 of word
	val archType: archType

    val isBigEndian: bool

    val isEncodableBitPattern: Word64.word * wordSize -> bool

    structure Sharing:
    sig
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
        type atomicOp = atomicOp
    end

end;
