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

signature ARM64ASSEMBLY =
sig
    type closureRef
    type instr
    type machineWord = Address.machineWord
    type labels

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

    (* Jump to the address in the register and put the address of the
       next instruction into X30. *)
    val branchAndLinkReg: xReg -> instr
    (* Jump to the address in the register. *)
    and branchRegister: xReg -> instr
    (* Jump to the address in the register and hint this is a return. *)
    and returnRegister: xReg -> instr

    (* Move an address constant to a register. *)
    val loadAddressConstant: xReg * machineWord -> instr
    (* Move a constant into a register that is not an address.
       The argument is the actual bit pattern to be copied.
       For tagged integers that means that the value must have
       been shifted and the tag bit set. *)
    and loadNonAddressConstant: xReg * Word64.word -> instr
    
    and loadFloatConstant: vReg  * Word64.word * xReg -> instr
    and loadDoubleConstant: vReg  * Word64.word * xReg -> instr

    (* Move a value into a register.  The immediate is 16-bits and the shift
       is 0, 16, 24, or 48.  moveKeep affect only the specific 16-bits and
       leaves the remainder unchanged. *)
    val moveNot32: {regD: xReg, immediate: word, shift: word} -> instr
    and moveZero32: {regD: xReg, immediate: word, shift: word} -> instr
    and moveKeep32: {regD: xReg, immediate: word, shift: word} -> instr
    val moveNot: {regD: xReg, immediate: word, shift: word} -> instr
    and moveZero: {regD: xReg, immediate: word, shift: word} -> instr
    and moveKeep: {regD: xReg, immediate: word, shift: word} -> instr

    (* Add/subtract an optionally shifted 12-bit immediate (i.e. constant) to/from a register.
       The constant is zero-extended. *)
    val addImmediate: {regN: xReg, regD: xReg, immed: word, shifted: bool} -> instr
    and addSImmediate: {regN: xReg, regD: xReg, immed: word, shifted: bool} -> instr
    and subImmediate: {regN: xReg, regD: xReg, immed: word, shifted: bool} -> instr
    and subSImmediate: {regN: xReg, regD: xReg, immed: word, shifted: bool} -> instr
    and addImmediate32: {regN: xReg, regD: xReg, immed: word, shifted: bool} -> instr
    and addSImmediate32: {regN: xReg, regD: xReg, immed: word, shifted: bool} -> instr
    and subImmediate32: {regN: xReg, regD: xReg, immed: word, shifted: bool} -> instr
    and subSImmediate32: {regN: xReg, regD: xReg, immed: word, shifted: bool} -> instr

    (* Add/subtract a shifted register, optionally setting the flags. *)
    val addShiftedReg: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and addSShiftedReg: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and subShiftedReg: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and subSShiftedReg: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and addShiftedReg32: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and addSShiftedReg32: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and subShiftedReg32: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and subSShiftedReg32: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    
    (* Add/subtract an extended register, optionally setting the flags. *)
    val addExtendedReg: {regM: xReg, regN: xReg, regD: xReg, extend: Word8.word extend} -> instr
    and addSExtendedReg: {regM: xReg, regN: xReg, regD: xReg, extend: Word8.word extend} -> instr
    and subExtendedReg: {regM: xReg, regN: xReg, regD: xReg, extend: Word8.word extend} -> instr
    and subSExtendedReg: {regM: xReg, regN: xReg, regD: xReg, extend: Word8.word extend} -> instr

    (* Multiplication *)
    (* regD = regA + regN * regM *)
    val multiplyAndAdd: {regM: xReg, regN: xReg, regA: xReg, regD: xReg} -> instr
    (* regD = regA - regN * regM *)
    and multiplyAndSub: {regM: xReg, regN: xReg, regA: xReg, regD: xReg} -> instr
    (* Return the high-order part of a signed multiplication. *)
    and signedMultiplyHigh: {regM: xReg, regN: xReg, regD: xReg} -> instr
    and multiplyAndAdd32: {regM: xReg, regN: xReg, regA: xReg, regD: xReg} -> instr
    and multiplyAndSub32: {regM: xReg, regN: xReg, regA: xReg, regD: xReg} -> instr
    (* Multiply two 32-bit quantities and add/subtract a 64-bit quantity. *)
    and signedMultiplyAndAddLong: {regM: xReg, regN: xReg, regA: xReg, regD: xReg} -> instr
    and signedMultiplyAndSubLong: {regM: xReg, regN: xReg, regA: xReg, regD: xReg} -> instr

    (* Division *)
    val unsignedDivide: {regM: xReg, regN: xReg, regD: xReg} -> instr
    and signedDivide: {regM: xReg, regN: xReg, regD: xReg} -> instr
    and unsignedDivide32: {regM: xReg, regN: xReg, regD: xReg} -> instr
    and signedDivide32: {regM: xReg, regN: xReg, regD: xReg} -> instr

    (* Single source operations. *)
    val countLeadingZeros: {regN: xReg, regD: xReg} -> instr
    and countLeadingZeros32: {regN: xReg, regD: xReg} -> instr

    (* Logical operations on a shifted register, optionally setting the flags. *)
    val andShiftedReg: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and orrShiftedReg: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and eorShiftedReg: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and andsShiftedReg: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and andShiftedReg32: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and orrShiftedReg32: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and eorShiftedReg32: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and andsShiftedReg32: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr

    (* Check whether a constant can be encoded. *)
    val isEncodableBitPattern: Word64.word * wordSize -> bool

    (* Load/Store an aligned word using a 12-bit offset.  The offset is in units
       of the size of the operand. *)
    val loadRegScaled: {regT: xReg, regN: xReg, unitOffset: int} -> instr
    and storeRegScaled: {regT: xReg, regN: xReg, unitOffset: int} -> instr
    and loadRegScaledByte: {regT: xReg, regN: xReg, unitOffset: int} -> instr
    and storeRegScaledByte: {regT: xReg, regN: xReg, unitOffset: int} -> instr
    and loadRegScaled16: {regT: xReg, regN: xReg, unitOffset: int} -> instr
    and storeRegScaled16: {regT: xReg, regN: xReg, unitOffset: int} -> instr
    and loadRegScaled32: {regT: xReg, regN: xReg, unitOffset: int} -> instr
    and storeRegScaled32: {regT: xReg, regN: xReg, unitOffset: int} -> instr
    and loadRegScaledDouble: {regT: vReg, regN: xReg, unitOffset: int} -> instr
    and storeRegScaledDouble: {regT: vReg, regN: xReg, unitOffset: int} -> instr
    and loadRegScaledFloat: {regT: vReg, regN: xReg, unitOffset: int} -> instr
    and storeRegScaledFloat: {regT: vReg, regN: xReg, unitOffset: int} -> instr

    (* Load/Store a value using a signed byte offset. *)
    val loadRegUnscaled: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and storeRegUnscaled: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and loadRegUnscaledByte: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and loadRegUnscaledSignedByteTo64: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and loadRegUnscaledSignedByteTo32: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and storeRegUnscaledByte: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and loadRegUnscaled16: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and loadRegUnscaledSigned16To64: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and loadRegUnscaledSigned16To32: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and storeRegUnscaled16: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and loadRegUnscaled32: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and loadRegUnscaledSigned32To64: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and storeRegUnscaled32: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and loadRegUnscaledFloat: {regT: vReg, regN: xReg, byteOffset: int} -> instr
    and storeRegUnscaledFloat: {regT: vReg, regN: xReg, byteOffset: int} -> instr
    and loadRegUnscaledDouble: {regT: vReg, regN: xReg, byteOffset: int} -> instr
    and storeRegUnscaledDouble: {regT: vReg, regN: xReg, byteOffset: int} -> instr

    (* Load/store with a register offset i.e. an index register. *)
    val loadRegIndexed: {regN: xReg, regM: xReg, regT: xReg, option: scale extend} -> instr
    and storeRegIndexed: {regN: xReg, regM: xReg, regT: xReg, option: scale extend} -> instr
    and loadRegIndexedByte: {regN: xReg, regM: xReg, regT: xReg, option: scale extend} -> instr
    and storeRegIndexedByte: {regN: xReg, regM: xReg, regT: xReg, option: scale extend} -> instr
    and loadRegIndexed16: {regN: xReg, regM: xReg, regT: xReg, option: scale extend} -> instr
    and storeRegIndexed16: {regN: xReg, regM: xReg, regT: xReg, option: scale extend} -> instr
    and loadRegIndexed32: {regN: xReg, regM: xReg, regT: xReg, option: scale extend} -> instr
    and storeRegIndexed32: {regN: xReg, regM: xReg, regT: xReg, option: scale extend} -> instr
    and loadRegIndexedFloat: {regN: xReg, regM: xReg, regT: vReg, option: scale extend} -> instr
    and storeRegIndexedFloat: {regN: xReg, regM: xReg, regT: vReg, option: scale extend} -> instr
    and loadRegIndexedDouble: {regN: xReg, regM: xReg, regT: vReg, option: scale extend} -> instr
    and storeRegIndexedDouble: {regN: xReg, regM: xReg, regT: vReg, option: scale extend} -> instr

    (* Load/Store a value using a signed byte offset and post-indexing (post-increment). *)
    (* The terminology is confusing. Pre-indexing means adding the offset into base address
       before loading the value, typically used for push, and post-index means using the
       original value of the base register as the address and adding in the offset after
       the value has been loaded, e.g. pop. *)
    val loadRegPostIndex: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and storeRegPostIndex: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and loadRegPostIndex32: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and storeRegPostIndex32: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and loadRegPostIndexByte: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and storeRegPostIndexByte: {regT: xReg, regN: xReg, byteOffset: int} -> instr

    (* Load/Store a value using a signed byte offset and pre-indexing (pre-increment). *)
    val loadRegPreIndex: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and storeRegPreIndex: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and loadRegPreIndex32: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and storeRegPreIndex32: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and loadRegPreIndexByte: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and storeRegPreIndexByte: {regT: xReg, regN: xReg, byteOffset: int} -> instr

    (* Loads and stores with special ordering. *)
    val loadAcquire: {regN: xReg, regT: xReg} -> instr
    and storeRelease: {regN: xReg, regT: xReg} -> instr
    and loadAcquire32: {regN: xReg, regT: xReg} -> instr
    and storeRelease32: {regN: xReg, regT: xReg} -> instr
    and loadAcquireByte: {regN: xReg, regT: xReg} -> instr
    and storeReleaseByte: {regN: xReg, regT: xReg} -> instr

    (* Load and store pairs of registers.  The offsets are signed scaled values. *)
    val storePairOffset: {regT1: xReg, regT2: xReg, regN: xReg, unitOffset: int} -> instr
    and loadPairOffset: {regT1: xReg, regT2: xReg, regN: xReg, unitOffset: int} -> instr
    and storePairPostIndexed: {regT1: xReg, regT2: xReg, regN: xReg, unitOffset: int} -> instr
    and loadPairPostIndexed: {regT1: xReg, regT2: xReg, regN: xReg, unitOffset: int} -> instr
    and storePairPreIndexed: {regT1: xReg, regT2: xReg, regN: xReg, unitOffset: int} -> instr
    and loadPairPreIndexed: {regT1: xReg, regT2: xReg, regN: xReg, unitOffset: int} -> instr

    and storePairOffset32: {regT1: xReg, regT2: xReg, regN: xReg, unitOffset: int} -> instr
    and loadPairOffset32: {regT1: xReg, regT2: xReg, regN: xReg, unitOffset: int} -> instr
    and storePairPostIndexed32: {regT1: xReg, regT2: xReg, regN: xReg, unitOffset: int} -> instr
    and loadPairPostIndexed32: {regT1: xReg, regT2: xReg, regN: xReg, unitOffset: int} -> instr
    and storePairPreIndexed32: {regT1: xReg, regT2: xReg, regN: xReg, unitOffset: int} -> instr
    and loadPairPreIndexed32: {regT1: xReg, regT2: xReg, regN: xReg, unitOffset: int} -> instr

    and storePairOffsetFloat: {regT1: vReg, regT2: vReg, regN: xReg, unitOffset: int} -> instr
    and loadPairOffsetFloat: {regT1: vReg, regT2: vReg, regN: xReg, unitOffset: int} -> instr
    and storePairPostIndexedFloat: {regT1: vReg, regT2: vReg, regN: xReg, unitOffset: int} -> instr
    and loadPairPostIndexedFloat: {regT1: vReg, regT2: vReg, regN: xReg, unitOffset: int} -> instr
    and storePairPreIndexedFloat: {regT1: vReg, regT2: vReg, regN: xReg, unitOffset: int} -> instr
    and loadPairPreIndexedFloat: {regT1: vReg, regT2: vReg, regN: xReg, unitOffset: int} -> instr

    and storePairOffsetDouble: {regT1: vReg, regT2: vReg, regN: xReg, unitOffset: int} -> instr
    and loadPairOffsetDouble: {regT1: vReg, regT2: vReg, regN: xReg, unitOffset: int} -> instr
    and storePairPostIndexedDouble: {regT1: vReg, regT2: vReg, regN: xReg, unitOffset: int} -> instr
    and loadPairPostIndexedDouble: {regT1: vReg, regT2: vReg, regN: xReg, unitOffset: int} -> instr
    and storePairPreIndexedDouble: {regT1: vReg, regT2: vReg, regN: xReg, unitOffset: int} -> instr
    and loadPairPreIndexedDouble: {regT1: vReg, regT2: vReg, regN: xReg, unitOffset: int} -> instr

    (* This word is put in after a call to the RTS trap-handler.  All the registers
       are saved and restored across a call to the trap-handler; the register
       mask contains those that may contain an address and so need to be scanned and
       possibly updated if there is a GC. *)
    val registerMask: xReg list -> instr

    (* Create a label. *)
    val createLabel: unit -> labels
    (* Put a label into the code. *)
    val setLabel: labels -> instr
    (* A conditional branch. *)
    val conditionalBranch: condition * labels -> instr
    (* Unconditional branch *)
    and unconditionalBranch: labels -> instr
    (* Unconditional branch and link. Only ever goes to the start of the function. *)
    and branchAndLink: labels -> instr
    (* Put the address of a label into a register - used for handlers and cases. *)
    and loadLabelAddress: xReg * labels -> instr
    (* Test a bit in a register and branch if zero/nonzero *)
    and testBitBranchZero: xReg * Word8.word * labels -> instr
    and testBitBranchNonZero: xReg * Word8.word * labels -> instr
    (* Compare a register with zero and branch if zero/nonzero *)
    and compareBranchZero: xReg * labels -> instr
    and compareBranchZero32: xReg * labels -> instr
    and compareBranchNonZero: xReg * labels -> instr
    and compareBranchNonZero32: xReg * labels -> instr

    (* Set the destination register to the value of the first reg if the
       condition is true otherwise to a, possibly modified, version of
       the second argument.  There are variants that set it unmodified,
       incremented, inverted and negated. *)
    val conditionalSet:
        {regD: xReg, regTrue: xReg, regFalse: xReg, cond: condition} -> instr
    and conditionalSetIncrement:
        {regD: xReg, regTrue: xReg, regFalse: xReg, cond: condition} -> instr
    and conditionalSetInverted:
        {regD: xReg, regTrue: xReg, regFalse: xReg, cond: condition} -> instr
    and conditionalSetNegated:
        {regD: xReg, regTrue: xReg, regFalse: xReg, cond: condition} -> instr
    and conditionalSet32:
        {regD: xReg, regTrue: xReg, regFalse: xReg, cond: condition} -> instr
    and conditionalSetIncrement32:
        {regD: xReg, regTrue: xReg, regFalse: xReg, cond: condition} -> instr
    and conditionalSetInverted32:
        {regD: xReg, regTrue: xReg, regFalse: xReg, cond: condition} -> instr
    and conditionalSetNegated32:
        {regD: xReg, regTrue: xReg, regFalse: xReg, cond: condition} -> instr

    (* General form of shift/bit extraction. *)
    val signedBitfieldMove32: {immr: word, imms: word, regN: xReg, regD: xReg} -> instr
    and bitfieldMove32: {immr: word, imms: word, regN: xReg, regD: xReg} -> instr
    and unsignedBitfieldMove32: {immr: word, imms: word, regN: xReg, regD: xReg} -> instr
    and signedBitfieldMove64: {immr: word, imms: word, regN: xReg, regD: xReg} -> instr
    and bitfieldMove64: {immr: word, imms: word, regN: xReg, regD: xReg} -> instr
    and unsignedBitfieldMove64: {immr: word, imms: word, regN: xReg, regD: xReg} -> instr

    (* Derived forms: Various shifts *)
    val logicalShiftLeft: {shift: word, regN: xReg, regD: xReg} -> instr
    and logicalShiftLeft32: {shift: word, regN: xReg, regD: xReg} -> instr
    and logicalShiftRight: {shift: word, regN: xReg, regD: xReg} -> instr
    and logicalShiftRight32: {shift: word, regN: xReg, regD: xReg} -> instr
    and arithmeticShiftRight: {shift: word, regN: xReg, regD: xReg} -> instr
    and arithmeticShiftRight32: {shift: word, regN: xReg, regD: xReg} -> instr
    (* Extract "width" least significant bits and place at offset "lsb" in the destination
       setting the rest of the register to zero. *)
    and unsignedBitfieldInsertinZeros: {lsb: word, width: word, regN: xReg, regD: xReg} -> instr
    and unsignedBitfieldInsertinZeros32: {lsb: word, width: word, regN: xReg, regD: xReg} -> instr
    (* Extract bits but leave the rest of the register unchanged.  Can be used
       to clear a specific range of bits by using XZero as the source. *)
    and bitfieldInsert: {lsb: word, width: word, regN: xReg, regD: xReg} -> instr
    and bitfieldInsert32: {lsb: word, width: word, regN: xReg, regD: xReg} -> instr
    (* Extract "width" bits starting from "lsb" in the source and place in the
       least significant bits of the destination, setting the high order bits to the
       sign bit. *)
    and signedBitfieldExtract: {lsb: word, width: word, regN: xReg, regD: xReg} -> instr

    (* Logical shift left Rd = Rn << (Rm mod 0w64) *)
    val logicalShiftLeftVariable: {regM: xReg, regN: xReg, regD: xReg} -> instr
    (* Logical shift right Rd = Rn >> (Rm mod 0w64) *)
    and logicalShiftRightVariable: {regM: xReg, regN: xReg, regD: xReg} -> instr
    (* Arithmetic shift right Rd = Rn ~>> (Rm mod 0w64) *)
    and arithmeticShiftRightVariable: {regM: xReg, regN: xReg, regD: xReg} -> instr
    and logicalShiftLeftVariable32: {regM: xReg, regN: xReg, regD: xReg} -> instr
    and logicalShiftRightVariable32: {regM: xReg, regN: xReg, regD: xReg} -> instr
    and arithmeticShiftRightVariable32: {regM: xReg, regN: xReg, regD: xReg} -> instr

    (* Logical operations on bit patterns.  The pattern must be valid.
       ANDS is an AND that also sets the flags, typically used for a test. *)
    val bitwiseAndImmediate: {bits: Word64.word, regN: xReg, regD: xReg} -> instr
    and bitwiseAndImmediate32: {bits: Word64.word, regN: xReg, regD: xReg} -> instr
    and bitwiseOrImmediate: {bits: Word64.word, regN: xReg, regD: xReg} -> instr
    and bitwiseOrImmediate32: {bits: Word64.word, regN: xReg, regD: xReg} -> instr
    and bitwiseXorImmediate: {bits: Word64.word, regN: xReg, regD: xReg} -> instr
    and bitwiseXorImmediate32: {bits: Word64.word, regN: xReg, regD: xReg} -> instr
    and bitwiseAndSImmediate: {bits: Word64.word, regN: xReg, regD: xReg} -> instr
    and bitwiseAndSImmediate32: {bits: Word64.word, regN: xReg, regD: xReg} -> instr

    (* Instructions involved in thread synchonisation. *)
    val yield: instr and dmbIsh: instr
    val loadAcquireExclusiveRegister: {regN: xReg, regT: xReg} -> instr
    val storeReleaseExclusiveRegister: {regN: xReg, regS: xReg, regT: xReg} -> instr

    (* Floating point moves and conversions.  Moves simply copy the bits.
       In all cases the integer argument is signed 64-bits. *)
    val moveGeneralToDouble: {regN: xReg, regD: vReg} -> instr
    and moveGeneralToFloat: {regN: xReg, regD: vReg} -> instr
    and moveDoubleToGeneral: {regN: vReg, regD: xReg} -> instr
    and moveFloatToGeneral: {regN: vReg, regD: xReg} -> instr
    and convertIntToDouble: {regN: xReg, regD: vReg} -> instr
    and convertIntToFloat: {regN: xReg, regD: vReg} -> instr
    and convertFloatToInt: IEEEReal.rounding_mode -> {regN: vReg, regD: xReg} -> instr
    and convertDoubleToInt: IEEEReal.rounding_mode -> {regN: vReg, regD: xReg} -> instr
    and convertInt32ToDouble: {regN: xReg, regD: vReg} -> instr
    and convertInt32ToFloat: {regN: xReg, regD: vReg} -> instr
    and convertFloatToInt32: IEEEReal.rounding_mode -> {regN: vReg, regD: xReg} -> instr
    and convertDoubleToInt32: IEEEReal.rounding_mode -> {regN: vReg, regD: xReg} -> instr
   
    (* Floating point operations. *)
    val multiplyFloat: {regM: vReg, regN: vReg, regD: vReg} -> instr
    and divideFloat: {regM: vReg, regN: vReg, regD: vReg} -> instr
    and addFloat: {regM: vReg, regN: vReg, regD: vReg} -> instr
    and subtractFloat: {regM: vReg, regN: vReg, regD: vReg} -> instr
    and multiplyDouble: {regM: vReg, regN: vReg, regD: vReg} -> instr
    and divideDouble: {regM: vReg, regN: vReg, regD: vReg} -> instr
    and addDouble: {regM: vReg, regN: vReg, regD: vReg} -> instr
    and subtractDouble: {regM: vReg, regN: vReg, regD: vReg} -> instr

    val compareFloat: {regM: vReg, regN: vReg} -> instr
    and compareDouble: {regM: vReg, regN: vReg} -> instr
    
    val moveFloatToFloat: {regN: vReg, regD: vReg} -> instr
    and absFloat: {regN: vReg, regD: vReg} -> instr
    and negFloat: {regN: vReg, regD: vReg} -> instr
    and convertFloatToDouble: {regN: vReg, regD: vReg} -> instr
    and moveDoubleToDouble: {regN: vReg, regD: vReg} -> instr
    and absDouble: {regN: vReg, regD: vReg} -> instr
    and negDouble: {regN: vReg, regD: vReg} -> instr
    and convertDoubleToFloat: {regN: vReg, regD: vReg} -> instr

    (* Some of the atomic operations added in 8.1 *)
    val loadAddAL: { regN: xReg, regT: xReg, regS: xReg } -> instr
    and loadUMaxAL: { regN: xReg, regT: xReg, regS: xReg } -> instr
    and swapAL: { regN: xReg, regT: xReg, regS: xReg } -> instr
    and loadAddA: { regN: xReg, regT: xReg, regS: xReg } -> instr
    and loadUMaxA: { regN: xReg, regT: xReg, regS: xReg } -> instr
    and swapL: { regN: xReg, regT: xReg, regS: xReg } -> instr
    
    (* Special hack for callbacks in 32-in-64.  Must appear as
       the first instructions in the callback. *)
    val loadGlobalHeapBaseInCallback: xReg -> instr list

    (* Create the vector of code from the list of instructions and update the
       closure reference to point to it. *)
    val generateCode:
        {instrs: instr list, name: string, parameters: Universal.universal list, resultClosure: closureRef,
         profileObject: machineWord} -> unit
 
    (* Offsets in the assembly code interface pointed at by X26
       These are in units of 64-bits NOT bytes. *)
    val heapOverflowCallOffset: int
    and stackOverflowCallOffset: int
    and stackOverflowXCallOffset: int
    and exceptionHandlerOffset: int
    and stackLimitOffset: int
    and exceptionPacketOffset: int
    and threadIdOffset: int
    and heapLimitPtrOffset: int
    and heapAllocPtrOffset: int
    and mlStackPtrOffset: int

	datatype archType = ArchNative | ArchC32 of word
	val archType: archType

    val isBigEndian: bool

    structure Sharing:
    sig
        type closureRef = closureRef
        type instr = instr
        type xReg = xReg
        type vReg = vReg
        type labels = labels
        type condition = condition
        type shiftType = shiftType
        type wordSize = wordSize
        type 'a extend = 'a extend
        type scale = scale
    end
end;
