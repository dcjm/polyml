(*
    Copyright David C. J. Matthews 2010, 2012, 2016-20

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

signature X86CODESIG =
sig
    type machineWord = Address.machineWord
    type short = Address.short
    type address = Address.address
    type closureRef

    type code

    (* Registers. *)
    datatype genReg = GeneralReg of Word8.word * bool
    and fpReg = FloatingPtReg of Word8.word
    and xmmReg = SSE2Reg of Word8.word
    
    datatype reg =
        GenReg of genReg
    |   FPReg of fpReg
    |   XMMReg of xmmReg

    val is32bit: LargeInt.int -> bool

    val eax: genReg and ebx: genReg and ecx: genReg and edx: genReg
    and edi: genReg and esi: genReg and esp: genReg and ebp: genReg
    and r8:  genReg and r9:  genReg and r10: genReg and r11: genReg
    and r12: genReg and r13: genReg and r14: genReg and r15: genReg
    and fp0: fpReg and fp1: fpReg and fp2: fpReg and fp3: fpReg
    and fp4: fpReg and fp5: fpReg and fp6: fpReg and fp7: fpReg
    and xmm0:xmmReg and xmm1:xmmReg and xmm2:xmmReg and xmm3:xmmReg
    and xmm4:xmmReg and xmm5:xmmReg and xmm6:xmmReg
    (* For vector indexing we provide a numbering for the registers. *)
    val regs:   int
    val regN:   int -> reg
    val nReg:   reg -> int

    val regRepr: reg -> string
    
    (* May be targeted at native 32-bit, native 64-bit or X86/64 with 32-bit words
       and addresses as object Ids. *)
    datatype targetArch = Native32Bit | Native64Bit | ObjectId32Bit
    val targetArch: targetArch

    type addrs
    val addrZero: addrs

    structure RegSet:
    sig
        eqtype regSet
        val singleton: reg -> regSet
        val allRegisters: regSet (* All registers: data, address, floating pt. *)
        val generalRegisters: regSet
        val floatingPtRegisters: regSet
        val sse2Registers: regSet
        val noRegisters: regSet
        val isAllRegs: regSet->bool
        val regSetUnion: regSet * regSet -> regSet
        val regSetIntersect: regSet * regSet -> regSet
        val listToSet: reg list -> regSet
        val setToList: regSet -> reg list
        val regSetMinus: regSet * regSet -> regSet
        val inSet: reg * regSet -> bool
        val cardinality: regSet -> int
        val regSetRepr: regSet -> string
        val oneOf: regSet -> reg
    end
    
    (* Size of operand.  OpSize64 is only valid in 64-bit mode. *)
    datatype opSize = OpSize32 | OpSize64

    datatype arithOp = ADD | OR (*|ADC | SBB*) | AND | SUB | XOR | CMP
    and      shiftType = SHL | SHR | SAR
    and      repOps = CMPS8 | MOVS8 | MOVS32 | STOS8 | STOS32 | MOVS64 | STOS64
    and      fpOps = FADD | FMUL | FCOM | FCOMP | FSUB | FSUBR | FDIV | FDIVR
    and      fpUnaryOps = FABS | FCHS | FLD1 | FLDZ
    and      branchOps =
                JO | JNO | JE | JNE | JL | JGE | JLE | JG | JB | JNB | JNA | JA | JP | JNP
    and sse2Operations =
        SSE2MoveDouble | SSE2MoveFloat | SSE2CompDouble | SSE2AddDouble |
        SSE2SubDouble | SSE2MulDouble | SSE2DivDouble |
        SSE2Xor | SSE2And | SSE2FloatToDouble | SSE2DoubleToFloat |
        SSE2CompSingle | SSE2AddSingle | SSE2SubSingle | SSE2MulSingle | SSE2DivSingle

    val invertTest: branchOps -> branchOps

    datatype label = Label of { labelNo: int }

    datatype indexType =
        NoIndex | Index1 of genReg | Index2 of genReg | Index4 of genReg | Index8 of genReg

    type memoryAddress = { base: genReg, offset: int, index: indexType }

    datatype 'reg regOrMemoryArg =
        RegisterArg of 'reg
    |   MemoryArg of memoryAddress
    |   NonAddressConstArg of LargeInt.int
    |   AddressConstArg of machineWord
    
    datatype moveSize = Move64 | Move32 | Move8 | Move16 | Move32X
    and fpSize = SinglePrecision | DoublePrecision

    datatype trapEntries =
        StackOverflowCall
    |   StackOverflowCallEx
    |   HeapOverflowCall
    |   ArbPrecisionEmulate

    datatype operation =
        Move of { source: genReg regOrMemoryArg, destination: genReg regOrMemoryArg, moveSize: moveSize }
    |   PushToStack of genReg regOrMemoryArg
    |   PopR of genReg
    |   ArithToGenReg of { opc: arithOp, output: genReg, source: genReg regOrMemoryArg, opSize: opSize }
    |   ArithMemConst of { opc: arithOp, address: memoryAddress, source: LargeInt.int, opSize: opSize }
    |   ArithMemLongConst of { opc: arithOp, address: memoryAddress, source: machineWord }
    |   ArithByteMemConst of { opc: arithOp, address: memoryAddress, source: Word8.word }
    |   ShiftConstant of { shiftType: shiftType, output: genReg, shift: Word8.word, opSize: opSize }
    |   ShiftVariable of { shiftType: shiftType, output: genReg, opSize: opSize } (* Shift amount is in ecx *)
    |   ConditionalBranch of { test: branchOps, label: label }
    |   SetCondition of { output: genReg, test: branchOps }
    |   LoadAddress of { output: genReg, offset: int, base: genReg option, index: indexType, opSize: opSize }
    |   TestByteBits of { arg: genReg regOrMemoryArg, bits: Word8.word }
    |   CallRTS of {rtsEntry: trapEntries, saveRegs: genReg list }
    |   AllocStore of { size: int, output: genReg, saveRegs: genReg list }
    |   AllocStoreVariable of { size: genReg, output: genReg, saveRegs: genReg list }
    |   StoreInitialised
    |   CallAddress of genReg regOrMemoryArg
    |   JumpAddress of genReg regOrMemoryArg
    |   ReturnFromFunction of int
    |   RaiseException of { workReg: genReg }
    |   UncondBranch of label
    |   ResetStack of { numWords: int, preserveCC: bool }
    |   JumpLabel of label
    |   LoadLabelAddress of { label: label, output: genReg }
    |   RepeatOperation of repOps
    |   DivideAccR of {arg: genReg, isSigned: bool, opSize: opSize }
    |   DivideAccM of {base: genReg, offset: int, isSigned: bool, opSize: opSize }
    |   AtomicXAdd of {address: memoryAddress, output: genReg, opSize: opSize }
    |   FPLoadFromMemory of { address: memoryAddress, precision: fpSize }
    |   FPLoadFromFPReg of { source: fpReg, lastRef: bool }
    |   FPLoadFromConst of { constant: machineWord, precision: fpSize }
    |   FPStoreToFPReg of { output: fpReg, andPop: bool }
    |   FPStoreToMemory of { address: memoryAddress, precision: fpSize, andPop: bool }
    |   FPArithR of { opc: fpOps, source: fpReg }
    |   FPArithConst of { opc: fpOps, source: machineWord, precision: fpSize }
    |   FPArithMemory of { opc: fpOps, base: genReg, offset: int, precision: fpSize }
    |   FPUnary of fpUnaryOps
    |   FPStatusToEAX
    |   FPLoadInt of { base: genReg, offset: int, opSize: opSize }
    |   FPFree of fpReg
    |   MultiplyR of { source: genReg regOrMemoryArg, output: genReg, opSize: opSize }
    |   XMMArith of { opc: sse2Operations, source: xmmReg regOrMemoryArg, output: xmmReg }
    |   XMMStoreToMemory of { toStore: xmmReg, address: memoryAddress, precision: fpSize }
    |   XMMConvertFromInt of { source: genReg, output: xmmReg, opSize: opSize }
    |   SignExtendForDivide of opSize
    |   XChng of { reg: genReg, arg: genReg regOrMemoryArg, opSize: opSize }
    |   Negative of { output: genReg, opSize: opSize }
    |   JumpTable of { cases: label list, jumpSize: jumpSize ref }
    |   IndexedJumpCalc of { addrReg: genReg, indexReg: genReg, jumpSize: jumpSize ref }
    |   MoveXMMRegToGenReg of { source: xmmReg, output: genReg }
    |   MoveGenRegToXMMReg of { source: genReg, output: xmmReg }
    |   XMMShiftRight of { output: xmmReg, shift: Word8.word }
    |   FPLoadCtrlWord of memoryAddress (* Load FP control word. *)
    |   FPStoreCtrlWord of memoryAddress (* Store FP control word. *)
    |   XMMLoadCSR of memoryAddress (* Load combined control/status word. *)
    |   XMMStoreCSR of memoryAddress (* Store combined control/status word. *)
    |   FPStoreInt of memoryAddress
    |   XMMStoreInt of { source: xmmReg regOrMemoryArg, output: genReg, precision: fpSize, isTruncate: bool }
    |   CondMove of { test: branchOps, output: genReg, source: genReg regOrMemoryArg, opSize: opSize }

    and jumpSize = JumpSize2 | JumpSize8

    type operations = operation list
    val printOperation: operation * (string -> unit) -> unit

    val codeCreate: string * machineWord * Universal.universal list -> code  (* makes the initial segment. *)
    
    (* Code generate operations and construct the final code. *)
    val generateCode: { ops: operations, code: code, labelCount: int, resultClosure: closureRef } -> unit

    val memRegLocalMPointer: int
    and memRegHandlerRegister: int
    and memRegLocalMbottom: int
    and memRegStackLimit: int
    and memRegExceptionPacket: int
    and memRegCStackPtr: int
    and memRegThreadSelf: int
    and memRegStackPtr: int

    (* Debugging controls and streams for optimiser. *)
    val lowLevelOptimise: code -> bool
    val printLowLevelCode: operation list * code -> unit

    structure Sharing:
    sig
        type code           = code
        and  reg            = reg
        and  genReg         = genReg
        and  fpReg          = fpReg
        and  addrs          = addrs
        and  operation      = operation
        and  regSet         = RegSet.regSet
        and  label          = label
        and  branchOps      = branchOps
        and  arithOp        = arithOp
        and  shiftType      = shiftType
        and  repOps         = repOps
        and  fpOps          = fpOps
        and  fpUnaryOps     = fpUnaryOps
        and  sse2Operations = sse2Operations
        and  opSize         = opSize
        and  closureRef     = closureRef
    end
end;
