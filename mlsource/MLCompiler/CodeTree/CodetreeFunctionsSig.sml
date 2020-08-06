(*
    Copyright (c) 2012,13,17,20 David C.J. Matthews

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

signature CodetreeFunctionsSig =
sig
    type codetree
    and codeBinding
    and loadForm
    and envSpecial

    type machineWord = Address.machineWord

    val mkLoadLocal: int -> codetree
    and mkLoadArgument: int -> codetree
    and mkLoadClosure: int -> codetree
 
    val mkEnv: codeBinding list * codetree -> codetree
    and mkInd: int * codetree -> codetree
    and mkVarField: int * codetree -> codetree
    and mkIndContainer: int * codetree -> codetree
    and mkTuple: codetree list -> codetree
    and mkDatatype: codetree list -> codetree

    val CodeFalse: codetree and CodeTrue: codetree and CodeZero: codetree

    val mkSetContainer:     codetree * codetree * BoolVector.vector -> codetree
    val mkTupleFromContainer: int * int -> codetree

    val decSequenceWithFinalExp: codeBinding list -> codetree
    
    val sideEffectFree: codetree -> bool
    and reorderable: codetree -> bool
    and sideEffectFreeRTSCall: int -> bool

    val makeConstVal: codetree -> codetree
    
    val evalue:    codetree -> machineWord option
    
    val findEntryInBlock: codetree * int * bool -> codetree
    
    val earlyRtsCall: int -> bool

    val partitionMutualBindings: codeBinding -> codeBinding list

    type createClosure
    val makeClosure: unit -> createClosure
    and addToClosure: createClosure -> loadForm -> loadForm
    and extractClosure: createClosure -> loadForm list

    val findInline: Universal.universal list -> envSpecial
    val setInline: envSpecial -> Universal.universal list -> Universal.universal list

    datatype inlineTest =
        TooBig
    |   NonRecursive
    |   TailRecursive of bool vector
    |   NonTailRecursive of bool vector

    val evaluateInlining: codetree * int * int -> inlineTest

    structure Sharing:
    sig
        type codetree = codetree
        and codeBinding = codeBinding
        and loadForm = loadForm
        and createClosure = createClosure
        and envSpecial = envSpecial
    end

end;
