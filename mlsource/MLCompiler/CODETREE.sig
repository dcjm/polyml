(*
    Copyright (c) 2012,13,15-22, 2025 David C.J. Matthews

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

signature CODETREE =
sig
    type machineWord
    type codetree
    type pretty
    type codeBinding
    
    type level

    datatype argumentType =
        GeneralType
    |   DoubleFloatType
    |   SingleFloatType
    |   ContainerType of int

    and loadStoreKind =
        LoadStoreMLWord of {isImmutable: bool}
    |   LoadStoreMLByte of {isImmutable: bool}
    |   LoadStoreC8
    |   LoadStoreC16
    |   LoadStoreC32
    |   LoadStoreC64
    |   LoadStoreCFloat
    |   LoadStoreCDouble
    |   LoadStoreUntaggedUnsigned
    |   LoadStorePolyWord of {isImmutable: bool}        (* Load/Store a PolyWord value to/from a large word *)
    |   LoadStoreNativeWord of {isImmutable: bool}      (* Load/Store a native word value to/from a large word *)

    and blockOpKind =
        BlockOpMove of {isByteMove: bool}
    |   BlockOpEqualByte
    |   BlockOpCompareByte
    
    structure BuiltIns: BUILTINS
 
    datatype arbPrecisionOps =
        ArbCompare of BuiltIns.testConditions
    |   ArbArith of BuiltIns.arithmeticOperations

    val CodeTrue:           codetree (* code for "true"  *)
    val CodeFalse:          codetree (* code for "false" *)
    val CodeZero:           codetree (* code for 0, nil etc. *)

    val mkFunction:
        {
            body: codetree, argTypes:argumentType list, resultType: argumentType,
            name: string, closure: codetree list, numLocals: int 
        } -> codetree
    val mkInlineFunction:
        {
            body: codetree, argTypes:argumentType list, resultType: argumentType,
            name: string, closure: codetree list, numLocals: int 
        } -> codetree
    val mkCall:             codetree * (codetree * argumentType) list * argumentType -> codetree

    val mkLoadLocal:        int -> codetree
    and mkLoadArgument:     int -> codetree
    and mkLoadClosure:      int -> codetree
    val loadRecursive:      codetree

    val mkConst:            machineWord -> codetree
    val mkInd:              int * codetree -> codetree
    val mkVarField:         int * codetree -> codetree
    val mkProc:             codetree * int * string * codetree list * int -> codetree
    val mkInlproc:          codetree * int * string * codetree list * int -> codetree
    val mkMacroProc:        codetree * int * string * codetree list * int -> codetree
    val mkIf:               codetree * codetree * codetree -> codetree
    val mkWhile:            codetree * codetree -> codetree
    val mkEnv:              codeBinding list * codetree -> codetree
    val mkStr:              string -> codetree
    val mkTuple:            codetree list -> codetree
    val mkDatatype:         codetree list -> codetree
    val mkRaise:            codetree -> codetree
    val mkCor:              codetree * codetree -> codetree
    val mkCand:             codetree * codetree -> codetree
    val mkHandle:           codetree * codetree * int -> codetree
    val mkEval:             codetree * codetree list -> codetree
    val identityFunction:   string -> codetree

    val mkSetContainer:     codetree * codetree * int -> codetree
    val mkTupleFromContainer: int * int -> codetree
    val mkTagTest:          codetree * word * word -> codetree
    val mkBeginLoop:        codetree * (int * codetree) list -> codetree
    val mkLoop:             codetree list -> codetree

    val mkDec:              int * codetree -> codeBinding
    val mkMutualDecs:       (int * codetree) list -> codeBinding
    val mkNullDec:          codetree -> codeBinding
    val mkContainer:        int * int * codetree -> codeBinding
    
    val mkNot:                codetree -> codetree
    val mkIsShort:            codetree -> codetree
    val mkEqualTaggedWord:    codetree * codetree -> codetree
    val mkEqualPointerOrWord: codetree * codetree -> codetree
    val equalTaggedWordFn:    codetree
    val equalPointerOrWordFn: codetree

    val decSequenceWithFinalExp: codeBinding list -> codetree

    val pretty:    codetree -> pretty
    val evalue:    codetree -> machineWord option
    val genCode:   codetree * Universal.universal list * int -> (unit -> codetree)

    (* Helper functions to build closure. *)
    val mkLoad:             int * level * level -> codetree
    and mkLoadParam:        int * level * level -> codetree
    val baseLevel:          level
    val newLevel:           level -> level
    val getClosure:         level -> codetree list
    val multipleUses: codetree * (unit -> int) * level -> {load: level -> codetree, dec: codeBinding list}
    
    val mkUnary: BuiltIns.unaryOps * codetree -> codetree
    and mkBinary: BuiltIns.binaryOps * codetree * codetree -> codetree

    val mkUnaryFn: BuiltIns.unaryOps -> codetree
    and mkBinaryFn: BuiltIns.binaryOps -> codetree
    and mkArbitraryFn: arbPrecisionOps -> codetree
    
    val getCurrentThreadId: codetree
    and getCurrentThreadIdFn: codetree
    and cpuPauseFn: codetree
    and createMutexFn: codetree

    val mkAllocateWordMemory: codetree * codetree * codetree -> codetree
    and mkAllocateWordMemoryFn: codetree

    (* Load and store operations.  At this level the first operand is the base address and
       the second is an index. *)
    val mkLoadOperation: loadStoreKind * codetree * codetree -> codetree
    val mkLoadOperationFn: loadStoreKind -> codetree
    val mkStoreOperation: loadStoreKind * codetree * codetree * codetree -> codetree
    val mkStoreOperationFn: loadStoreKind -> codetree
    val mkBlockOperation:
        {kind:blockOpKind, leftBase: codetree, rightBase: codetree, leftIndex: codetree,
         rightIndex: codetree, length: codetree} -> codetree
    val mkBlockOperationFn: blockOpKind -> codetree

    structure Foreign: FOREIGNCALL
    structure Sharing:
    sig
        type machineWord = machineWord
        type codetree    = codetree
        type pretty      = pretty
        type argumentType=argumentType
        type codeBinding     = codeBinding
        type level       = level
    end

end;

