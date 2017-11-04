(*
    Copyright (c) David C.J. Matthews 2015, 2016-17

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

signature X86OLDFIRSTSTAGESIG =
sig
    type code
    type ttab
    eqtype reg
    val regClosure:  reg
    val regStackPtr: reg
    datatype argType = ArgGeneral | ArgFP
    val argRegs: argType list -> reg option list
    val resultReg: argType -> reg
    type genReg

    datatype arithOp = ADD | OR (*|ADC | SBB*) | AND | SUB | XOR | CMP

    structure RegSet:
    sig
        eqtype regSet
        val singleton: reg -> regSet
        val allRegisters: regSet
        val generalRegisters: regSet
        val floatingPtRegisters: regSet
        val noRegisters: regSet
        val regSetUnion: regSet * regSet -> regSet
        val inSet: reg * regSet -> bool
        val listToSet: reg list -> regSet
    end

    datatype regHint = UseReg of RegSet.regSet | NoHint | NoResult
    type operation
    type operations = operation list
    type forwardLabel
    type backwardLabel

    val activeRegister: reg -> operations
    val returnFromFunction: int -> operations
    val resetStack: int -> operations
    val raiseException: operations
    val pushRegisterToStack: reg -> operations
    val pushCurrentHandler: operations
    val storeToHandler: reg -> operations
    val allocStore: { size: int, flags: Word8.word, output: reg, saveRegs: RegSet.regSet } -> operations
    val allocationComplete: operations
    val backJumpLabel: ttab -> operations * backwardLabel
    val forwardJumpLabel: forwardLabel -> operations
    val indexedCase:
            { ttab: ttab, testReg: reg, workReg: reg, minCase: word, maxCase: word,
              isArbitrary: bool, isExhaustive: bool } -> operations * forwardLabel list * forwardLabel
    
    datatype callKinds =
        Recursive
    |   ConstantCode of Address.machineWord
    |   FullCall
    |   DirectReg of genReg
    
    val callFunction: callKinds -> operations
    val jumpToFunction: callKinds  -> operations

    val codeCreate: string * Address.machineWord * Universal.universal list -> code  (* makes the initial segment. *)
    val copyCode: ttab * code * operations * int * bool * reg list -> Address.address

    type regSet = RegSet.regSet
    type machineWord = Address.machineWord
    type loopPush
    type addrs
    type savedState

    val ttabCreate: int * Universal.universal list -> ttab

    (* Register allocation *)
    val getRegister:    ttab * reg -> operation list
    val getRegisterInSet: ttab * regSet -> reg * operation list
    val freeRegister:   ttab * reg -> operation list
    val addRegUse:      ttab * reg -> operation list
    val removeRegistersFromCache: ttab * regSet -> operation list
    
    val findActiveRegisters: ttab -> regSet

    (* Stack handling *)
    eqtype stackIndex

    val noIndex: stackIndex

    (* Push entries *)
    val pushReg:      ttab * reg  -> stackIndex;
    val pushStack:    ttab * int  -> stackIndex
    val pushConst:    ttab * machineWord -> stackIndex
    val pushAllBut:   ttab * ((stackIndex -> unit) -> unit) * regSet -> operation list
    val pushNonArguments: ttab * stackIndex list * regSet -> reg list * operation list
    val pushSpecificEntry: ttab * stackIndex -> operation list
    val incsp:        ttab -> stackIndex;
    val decsp:        ttab*int -> unit;
    val reserveStackSpace: ttab * int -> stackIndex * operation list

    (* Code entries *)
    val loadEntryToSet:    ttab * stackIndex * regSet * bool -> reg * stackIndex * operation list
    val loadToSpecificReg: ttab * reg * stackIndex * bool -> stackIndex * operation list
    val lockRegister:      ttab * reg -> unit
    val unlockRegister:    ttab * reg -> operation list
    val loadIfArg:         ttab * stackIndex -> stackIndex * operation list
    val indirect:          int * stackIndex * ttab -> stackIndex * operation list
    val moveToVec:         stackIndex * stackIndex * int * ttab -> operation list
    val ensureNoAllocation: ttab * stackIndex -> stackIndex * operation list

    val removeStackEntry: ttab*stackIndex -> operation list

    val resetButReload:   ttab * int -> operation list
    val pushValueToStack: ttab * stackIndex * int -> stackIndex * operation list
    val storeInStack:     ttab * stackIndex * int -> stackIndex * operation list
    val realstackptr:     ttab -> int
    val maxstack:         ttab -> int
    val parameterInRegister: reg * int * ttab -> stackIndex
    val incrUseCount:     ttab * stackIndex * int -> operation list

    val setLifetime:      ttab * stackIndex * int -> unit

    type stackMark
    val markStack: ttab -> stackMark
    val unmarkStack: ttab * stackMark -> unit

    type labels;

    val noJump: labels
    val isEmptyLabel: labels -> bool

    datatype mergeResult = NoMerge | MergeIndex of stackIndex;

    val unconditionalBranch: mergeResult * ttab -> labels * operation list
    val makeLabels: mergeResult * forwardLabel * savedState -> labels
    val jumpBack: backwardLabel * ttab -> operation list

    val fixup: labels * ttab -> operation list
    val merge: labels * ttab * mergeResult * stackMark -> mergeResult * operation list

    type handler;

    val pushAddress: ttab * int -> stackIndex * handler * operation list
    val fixupH:      handler * int * ttab -> operation list
    val reloadHandler: ttab * stackIndex -> operation list

    val exiting: ttab -> unit
    val haveExited: ttab -> bool

    val saveState : ttab -> savedState
    val compareLoopStates: ttab * savedState * stackIndex list -> regSet * loopPush list
    val restoreLoopState: ttab * savedState * regSet * loopPush list -> operation list

    (* Temporary checking that the stack has been emptied. *)
    val checkBlockResult: ttab * mergeResult -> unit

    val addModifiedRegSet: ttab * regSet -> unit

    datatype argdest = ArgToRegister of reg | ArgToStack of int | ArgDiscard
    val getLoopDestinations: stackIndex list * ttab -> argdest list * operation list

    val callCode: stackIndex * bool * ttab -> operation list
    val jumpToCode: stackIndex * bool * ttab -> operation list

    val isConstant: stackIndex * ttab -> machineWord option
    val isRegister: stackIndex * ttab -> reg option
    val isContainer: stackIndex * ttab -> bool

    val createStackClosure: ttab * stackIndex list -> stackIndex * operation list
    val setRecursiveClosureEntry: stackIndex * stackIndex * int * ttab -> operation list
 
    val threadSelf: ttab * regHint -> operation list * mergeResult

    val vectorLength: stackIndex * ttab * regHint -> operation list * mergeResult
    and vectorFlags: stackIndex * ttab * regHint -> operation list * mergeResult
    and atomicIncrement: stackIndex * ttab * regHint -> operation list * mergeResult
    and atomicDecrement: stackIndex * ttab * regHint -> operation list * mergeResult
    and atomicReset: stackIndex * ttab * regHint -> operation list * mergeResult
    and lockVector: stackIndex * ttab * regHint -> operation list * mergeResult
    and absoluteReal: stackIndex * ttab * regHint -> operation list * mergeResult
    and negativeReal: stackIndex * ttab * regHint -> operation list * mergeResult
    and integerToReal: stackIndex * ttab * regHint -> operation list * mergeResult

    val testOverflow: ttab -> operation list (* Raise Overflow if the overflow flag is set. *)
    and branchOnOverflow: ttab -> labels * operation list
    
    val fixedPointOperation:
        {arg1: stackIndex, arg2: stackIndex, transtable: ttab, whereto: regHint,
         oper: arithOp, checkOverflow: unit -> operation list} -> (operation list * mergeResult)

    val multiplyFixed: {arg1: stackIndex, arg2: stackIndex, transtable: ttab, whereto: regHint,
                        checkOverflow: unit -> operation list} -> (operation list * mergeResult)
    and quotFixed: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and remFixed: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and upShiftWordConstant: stackIndex * word * ttab * regHint -> (operation list * mergeResult)
    and upShiftWordVariable: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and downShiftWordConstant: stackIndex * word * ttab * regHint -> (operation list * mergeResult)
    and downShiftWordVariable: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and downShiftWordArithmeticConstant: stackIndex * word * ttab * regHint -> (operation list * mergeResult)
    and downShiftWordArithmeticVariable: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and multiplyWord: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and divideWord: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and modulusWord: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and addReal: stackIndex * stackIndex * ttab * regHint -> operation list * mergeResult
    and subtractReal: stackIndex * stackIndex * ttab * regHint -> operation list * mergeResult
    and multiplyReal: stackIndex * stackIndex * ttab * regHint -> operation list * mergeResult
    and divideReal: stackIndex * stackIndex * ttab * regHint -> operation list * mergeResult
    and addLargeWord: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and subtractLargeWord: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and andLargeWord: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and orLargeWord: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and xorLargeWord: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and multiplyLargeWord: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and divideLargeWord: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and modulusLargeWord: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and upShiftLargeWordConstant: stackIndex * word * ttab * regHint -> (operation list * mergeResult)
    and upShiftLargeWordVariable: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and downShiftLargeWordConstant: stackIndex * word * ttab * regHint -> (operation list * mergeResult)
    and downShiftLargeWordVariable: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    and downShiftLargeWordArithmeticConstant: stackIndex * word * ttab * regHint -> (operation list * mergeResult)
    and downShiftLargeWordArithmeticVariable: stackIndex * stackIndex * ttab * regHint -> (operation list * mergeResult)
    
    val loadByte: {base: stackIndex, index: stackIndex, byteOffset: word, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)
    and loadWord: {base: stackIndex, index: stackIndex, byteOffset: word, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)
    and loadC8: {base: stackIndex, index: stackIndex, byteOffset: word, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)
    and loadC16: {base: stackIndex, index: stackIndex, byteOffset: word, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)
    and loadC32: {base: stackIndex, index: stackIndex, byteOffset: word, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)
    and loadC64: {base: stackIndex, index: stackIndex, byteOffset: word, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)
    and loadCfloat: {base: stackIndex, index: stackIndex, byteOffset: word, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)
    and loadCdouble: {base: stackIndex, index: stackIndex, byteOffset: word, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)
    and loadAndTag: {base: stackIndex, index: stackIndex, byteOffset: word, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)
    and storeByte: {base: stackIndex, index: stackIndex, byteOffset: word, toStore: stackIndex, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)
    and storeWord: {base: stackIndex, index: stackIndex, byteOffset: word, toStore: stackIndex, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)
    and storeC8: {base: stackIndex, index: stackIndex, byteOffset: word, toStore: stackIndex, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)
    and storeC16: {base: stackIndex, index: stackIndex, byteOffset: word, toStore: stackIndex, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)
    and storeC32: {base: stackIndex, index: stackIndex, byteOffset: word, toStore: stackIndex, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)
    and storeC64: {base: stackIndex, index: stackIndex, byteOffset: word, toStore: stackIndex, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)
    and storeCfloat: {base: stackIndex, index: stackIndex, byteOffset: word, toStore: stackIndex, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)
    and storeCdouble: {base: stackIndex, index: stackIndex, byteOffset: word, toStore: stackIndex, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)
    and storeUntagged: {base: stackIndex, index: stackIndex, byteOffset: word, toStore: stackIndex, transtable: ttab, whereto: regHint} ->
            (operation list * mergeResult)

    val allocateStoreSmallFixedSize: int * Word8.word * stackIndex * ttab * regHint -> operation list * mergeResult
    and allocStoreAndInitialise: stackIndex * stackIndex * stackIndex * ttab * regHint -> operation list * mergeResult
    and allocStoreUninitialised: stackIndex * stackIndex * ttab * regHint -> operation list * mergeResult

    val moveBytes:
        {src: {base:stackIndex, index: stackIndex, byteOffset: word},
         dst: {base:stackIndex, index: stackIndex, byteOffset: word},
         length: stackIndex, transtable: ttab, whereto: regHint} -> operation list * mergeResult
    and moveWords:
        {src: {base:stackIndex, index: stackIndex, byteOffset: word},
         dst: {base:stackIndex, index: stackIndex, byteOffset: word},
         length: stackIndex, transtable: ttab, whereto: regHint} -> operation list * mergeResult
    and byteVecComparison:
        {left: {base:stackIndex, index: stackIndex, byteOffset: word},
         right: {base:stackIndex, index: stackIndex, byteOffset: word},
         length: stackIndex, transtable: ttab, whereto: regHint} -> operation list * mergeResult
    
    val wordToLargeWord: bool * stackIndex * ttab * regHint -> operation list * mergeResult
 
    val notEqualWord: stackIndex * stackIndex * ttab -> labels * operation list
    and equalWord: stackIndex * stackIndex * ttab -> labels * operation list
    and greaterOrEqualWord: stackIndex * stackIndex * ttab -> labels * operation list
    and greaterThanWord: stackIndex * stackIndex * ttab -> labels * operation list
    and lessOrEqualWord: stackIndex * stackIndex * ttab -> labels * operation list
    and lessThanWord: stackIndex * stackIndex * ttab -> labels * operation list
    and greaterOrEqualFixed: stackIndex * stackIndex * ttab -> labels * operation list
    and greaterThanFixed: stackIndex * stackIndex * ttab -> labels * operation list
    and lessOrEqualFixed: stackIndex * stackIndex * ttab -> labels * operation list
    and lessThanFixed: stackIndex * stackIndex * ttab -> labels * operation list
    and notEqualReal: stackIndex * stackIndex * ttab -> labels * operation list
    and equalReal: stackIndex * stackIndex * ttab -> labels * operation list
    and greaterOrEqualReal: stackIndex * stackIndex * ttab -> labels * operation list
    and greaterThanReal: stackIndex * stackIndex * ttab -> labels * operation list
    and lessOrEqualReal: stackIndex * stackIndex * ttab -> labels * operation list
    and lessThanReal: stackIndex * stackIndex * ttab -> labels * operation list
    and notEqualLargeWord: stackIndex * stackIndex * ttab -> labels * operation list
    and equalLargeWord: stackIndex * stackIndex * ttab -> labels * operation list
    and greaterOrEqualLargeWord: stackIndex * stackIndex * ttab -> labels * operation list
    and greaterThanLargeWord: stackIndex * stackIndex * ttab -> labels * operation list
    and lessOrEqualLargeWord: stackIndex * stackIndex * ttab -> labels * operation list
    and lessThanLargeWord: stackIndex * stackIndex * ttab -> labels * operation list
 
    val testShortInt: stackIndex * ttab -> labels * operation list
    and testNotShortInt: stackIndex * ttab -> labels * operation list
    
    val testByteVecEq:
        {left: {base:stackIndex, index: stackIndex, byteOffset: word},
         right: {base:stackIndex, index: stackIndex, byteOffset: word},
         length: stackIndex, transtable: ttab} -> labels * operation list
    and testByteVecNe:
        {left: {base:stackIndex, index: stackIndex, byteOffset: word},
         right: {base:stackIndex, index: stackIndex, byteOffset: word},
         length: stackIndex, transtable: ttab} -> labels * operation list

    structure Sharing:
    sig
        type code           = code
        and  reg            = reg
        and  addrs          = addrs
        and  operation      = operation
        and  regHint        = regHint
        and  regSet         = RegSet.regSet
        and  backwardLabel  = backwardLabel
        and  forwardLabel  = forwardLabel
    end
end;
