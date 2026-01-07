(*
    Copyright (c) 2016-18, 2020-23 David C.J. Matthews

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

signature INTCODECONS =
sig
    type machineWord = Address.machineWord
    type address = Address.address
    type code
    type opcode
    type labels
    type closureRef

    val opcode_notBoolean: opcode
    val opcode_cellLength: opcode
    and opcode_cellFlags: opcode
    and opcode_clearMutable: opcode
    and opcode_createMutex: opcode
    and opcode_lockMutex: opcode
    and opcode_tryLockMutex: opcode
    and opcode_atomicReset: opcode
    and opcode_longWToTagged: opcode
    and opcode_signedToLongW: opcode
    and opcode_unsignedToLongW: opcode
    and opcode_realAbs: opcode
    and opcode_realNeg: opcode
    and opcode_fixedIntToReal: opcode
    and opcode_fixedIntToFloat: opcode
    and opcode_floatToReal: opcode
    and opcode_floatAbs: opcode
    and opcode_floatNeg: opcode
    
    val opcode_equalWord: opcode
    and opcode_lessSigned: opcode
    and opcode_lessUnsigned: opcode
    and opcode_lessEqSigned: opcode
    and opcode_lessEqUnsigned: opcode
    and opcode_greaterSigned: opcode
    and opcode_greaterUnsigned: opcode
    and opcode_greaterEqSigned: opcode
    and opcode_greaterEqUnsigned: opcode

    val opcode_fixedAdd: opcode
    val opcode_fixedSub: opcode
    val opcode_fixedMult: opcode
    val opcode_fixedQuot: opcode
    val opcode_fixedRem: opcode
    val opcode_fixedDiv: opcode
    val opcode_fixedMod: opcode
    val opcode_wordAdd: opcode
    val opcode_wordSub: opcode
    val opcode_wordMult: opcode
    val opcode_wordDiv: opcode
    val opcode_wordMod: opcode
    val opcode_wordAnd: opcode
    val opcode_wordOr: opcode
    val opcode_wordXor: opcode
    val opcode_wordShiftLeft: opcode
    val opcode_wordShiftRLog: opcode
    val opcode_wordShiftRArith: opcode
    val opcode_allocByteMem: opcode
    val opcode_lgWordEqual: opcode
    val opcode_lgWordLess: opcode
    val opcode_lgWordLessEq: opcode
    val opcode_lgWordGreater: opcode
    val opcode_lgWordGreaterEq: opcode
    val opcode_lgWordAdd: opcode
    val opcode_lgWordSub: opcode
    val opcode_lgWordMult: opcode
    val opcode_lgWordDiv: opcode
    val opcode_lgWordMod: opcode
    val opcode_lgWordAnd: opcode
    val opcode_lgWordOr: opcode
    val opcode_lgWordXor: opcode
    val opcode_lgWordShiftLeft: opcode
    val opcode_lgWordShiftRLog: opcode
    val opcode_lgWordShiftRArith: opcode
    val opcode_realEqual: opcode
    val opcode_realLess: opcode
    val opcode_realLessEq: opcode
    val opcode_realGreater: opcode
    val opcode_realGreaterEq: opcode
    val opcode_realUnordered: opcode
    val opcode_realAdd: opcode
    val opcode_realSub: opcode
    val opcode_realMult: opcode
    val opcode_realDiv: opcode
    val opcode_floatEqual: opcode
    val opcode_floatLess: opcode
    val opcode_floatLessEq: opcode
    val opcode_floatGreater: opcode
    val opcode_floatGreaterEq: opcode
    val opcode_floatUnordered: opcode
    val opcode_floatAdd: opcode
    val opcode_floatSub: opcode
    val opcode_floatMult: opcode
    val opcode_floatDiv: opcode
    val opcode_getThreadId: opcode
    val opcode_allocWordMemory: opcode
    val opcode_alloc_ref: opcode
    val opcode_loadMLWord: opcode
    val opcode_loadMLByte: opcode
    val opcode_loadC8: opcode
    val opcode_loadC16: opcode
    val opcode_loadC32: opcode
    val opcode_loadC64: opcode
    val opcode_loadCFloat: opcode
    val opcode_loadCDouble: opcode
    val opcode_loadUntagged: opcode
    val opcode_loadPolyWord: opcode
    val opcode_loadNativeWord: opcode
    val opcode_storeMLWord: opcode
    val opcode_storeMLByte: opcode
    val opcode_storeC8: opcode
    val opcode_storeC16: opcode
    val opcode_storeC32: opcode
    val opcode_storeC64: opcode
    val opcode_storeCFloat: opcode
    val opcode_storeCDouble: opcode
    val opcode_storeUntagged: opcode
    val opcode_storePolyWord: opcode
    val opcode_storeNativeWord: opcode
    val opcode_blockMoveWord: opcode
    val opcode_blockMoveByte: opcode
    val opcode_blockEqualByte: opcode
    val opcode_blockCompareByte: opcode
    val opcode_deleteHandler: opcode
    val opcode_allocCSpace: opcode
    val opcode_freeCSpace: opcode
    val opcode_arbAdd: opcode
    val opcode_arbSubtract: opcode
    val opcode_arbMultiply: opcode
    val opcode_log2Word: opcode

    val codeCreate: string * Universal.universal list -> code  (* makes the initial segment. *)

    (* GEN- routines all put a value at the instruction counter and add
      an appropriate amount to it. *)
      
   (* gen... - put instructions and their operands. *)
   val genCallClosure : code -> unit
   val genRaiseEx     : code -> unit
   val genLock        : code -> unit
   val genLdexc       : code -> unit
   val genPushHandler : code -> unit
      
   val genReturn      : int * code -> unit
   val genLocal       : int * code -> unit
   val genIndirect    : int * code -> unit
   val genSetStackVal : int * code -> unit
   val genCase        : int * code -> labels list
   val genTuple       : int * code -> unit
   val genTailCall    : int * int * code -> unit
   
   val genIndirectClosure:      { addr: int, item: int, code: code } -> unit
   and genIndirectContainer:    int * code -> unit
   and genMoveToContainer:      int * code -> unit
   and genMoveToMutClosure:     int * code -> unit
   and genClosure:              int * code -> unit
   
   val genDoubleToFloat: code -> unit
   and genRealToInt:   IEEEReal.rounding_mode * code -> unit
   and genFloatToInt:  IEEEReal.rounding_mode * code -> unit
   
   val genAllocMutableClosure: int * code -> unit

   val genRTSCallFast:    int * code -> unit
   val genRTSCallFastRealtoReal: code -> unit
   val genRTSCallFastRealRealtoReal: code -> unit
   val genRTSCallFastGeneraltoReal: code -> unit
   val genRTSCallFastRealGeneraltoReal: code -> unit
   val genRTSCallFastFloattoFloat: code -> unit
   val genRTSCallFastFloatFloattoFloat: code -> unit
   val genRTSCallFastGeneraltoFloat: code -> unit
   val genRTSCallFastFloatGeneraltoFloat: code -> unit
   
   val genOpcode: opcode * code -> unit

   (* genEnter instructions are only needed when machine-code routines
      can call interpreted routines or vice-versa. The enterInt instruction
      causes the interpreter to be entered and the argument indicates the
      reason. *)
      
   val genEnterIntCatch : code -> unit
   val genEnterIntCall  : code * int -> unit
      
   (* pushConst - Generates code to push a constant. *)
   val pushConst        : machineWord * code -> unit

   (* Create a container on the stack *)
   val genContainer : int * code -> unit

    (* copyCode - Finish up after compiling a function. *)
    val copyCode : {code: code, maxStack: int, numberOfArguments: int, resultClosure: closureRef } -> unit
   
   (* putBranchInstruction puts in an instruction which involves
      a forward reference. *)
   datatype jumpTypes = Jump | JumpBack | JumpFalse | JumpTrue | SetHandler
   val putBranchInstruction: jumpTypes * labels * code -> unit
   
   val createLabel: unit -> labels
   
   (* Define the position of a label. *)
   val setLabel: labels * code -> unit
   
   val resetStack: int * bool * code -> unit (* Set a pending reset *)
   
    val genEqualWordConst: word * code -> unit
    val genIsTagged: code -> unit
   
    structure Sharing:
    sig
        type code = code
        type opcode = opcode
        type labels = labels
        type closureRef = closureRef
    end
end ;
