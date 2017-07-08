(*
    Copyright (c) 2012, 2016 David C.J. Matthews

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

(* Intermediate code tree for the back end of the compiler. *)

signature BackendIntermediateCodeSig =
sig
    type machineWord = Address.machineWord

    datatype argumentType =
        GeneralType
    |   FloatingPtType
    
    structure BuiltIns: BUILTINS
    
    datatype backendIC =
        BICNewenv of bicCodeBinding list * backendIC (* Set of bindings with an expression. *)

    |   BICConstnt of machineWord * Universal.universal list (* Load a constant *)

    |   BICExtract of bicLoadForm (* Get a local variable, an argument or a closure value *)

    |   BICField of {base: backendIC, offset: int }
         (* Load a field from a tuple or record *)
    
    |   BICEval of (* Evaluate a function with an argument list. *)
        {
            function:  backendIC,
            argList:   (backendIC * argumentType) list,
            resultType: argumentType
        }

        (* Built-in functions. *)
    |   BICBuiltIn0 of {oper: BuiltIns.builtIn0Ops}
    |   BICBuiltIn1 of {oper: BuiltIns.builtIn1Ops, arg1: backendIC}
    |   BICBuiltIn2 of {oper: BuiltIns.builtIn2Ops, arg1: backendIC, arg2: backendIC}
    |   BICBuiltIn3 of {oper: BuiltIns.builtIn3Ops, arg1: backendIC, arg2: backendIC, arg3: backendIC}

    |   BICLambda of bicLambdaForm (* Lambda expressions. *)

    |   BICCond of backendIC * backendIC * backendIC (* If-then-else expression *)

    |   BICCase of (* Case expressions *)
        {
            cases   : (backendIC * word) list,
            test    : backendIC,
            caseType: caseType,
            default : backendIC
        }
    
    |   BICBeginLoop of (* Start of tail-recursive inline function. *)
        { loop: backendIC, arguments: (bicSimpleBinding * argumentType) list }

    |   BICLoop of (backendIC * argumentType) list (* Jump back to start of tail-recursive function. *)

    |   BICRaise of backendIC (* Raise an exception *)

    |   BICLdexc (* Load the exception (used at the start of a handler) *)

    |   BICHandle of (* Exception handler. *) { exp: backendIC, handler: backendIC }

    |   BICTuple of backendIC list (* Tuple *)

    |   BICContainer of int (* Create a container for a tuple on the stack. *)

    |   BICSetContainer of (* Copy a tuple to a container. *)
        {
            container: backendIC,
            tuple:     backendIC,
            filter:    BoolVector.vector
        }

    |   BICTagTest of { test: backendIC, tag: word, maxTag: word }

    |   BICLoadOperation of { kind: loadStoreKind, address: bicAddress }
    
    |   BICStoreOperation of { kind: loadStoreKind, address: bicAddress, value: backendIC }
    
    |   BICBlockOperation of
            { kind: blockOpKind, sourceLeft: bicAddress, destRight: bicAddress, length: backendIC }

    and bicCodeBinding =
        BICDeclar  of bicSimpleBinding (* Make a local declaration or push an argument *)
    |   BICRecDecs of { addr: int, lambda: bicLambdaForm } list (* Set of mutually recursive declarations. *)
    |   BICNullBinding of backendIC (* Just evaluate the expression and discard the result. *)

    and caseType =
        CaseInt
    |   CaseWord
    |   CaseTag of word

    and bicLoadForm =
        BICLoadLocal of int (* Local binding *)
    |   BICLoadArgument of int (* Argument - 0 is first arg etc.*)
    |   BICLoadClosure of int (* Closure - 0 is first closure item etc *)
    |   BICLoadRecursive (* Recursive call *)

    and loadStoreKind =
        LoadStoreMLWord of {isImmutable: bool}     (* Load/Store an ML word in an ML word cell. *)
    |   LoadStoreMLByte of {isImmutable: bool}     (* Load/Store a byte, tagging and untagging as appropriate, in an ML byte cell. *)
    |   LoadStoreC8         (* Load/Store C values - The base address is a boxed SysWord.word value. *)
    |   LoadStoreC16
    |   LoadStoreC32
    |   LoadStoreC64
    |   LoadStoreCFloat
    |   LoadStoreCDouble

    and blockOpKind =
        BlockOpMove of {isByteMove: bool}
    |   BlockOpEqualByte
    |   BlockOpCompareByte

    withtype bicSimpleBinding = 
    { (* Declare a value or push an argument. *)
        value:      backendIC,
        addr:       int
    }

    and bicLambdaForm =
    { (* Lambda expressions. *)
        body          : backendIC,
        name          : string,
        closure       : bicLoadForm list,
        argTypes      : argumentType list,
        resultType    : argumentType,
        localCount    : int,
        heapClosure   : bool
    }

    and bicAddress =
        (* Address form used in loads, store and block operations.  The base is an ML
           address if this is to/from ML memory or a (boxed) SysWord.word if it is
           to/from C memory.  The index is a value in units of the size of the item
           being loaded/stored and the offset is always in bytes. *)
        {base: backendIC, index: backendIC option, offset: word}

    type pretty
    val pretty : backendIC -> pretty
    
    val loadStoreKindRepr: loadStoreKind -> string
    and blockOpKindRepr: blockOpKind -> string

    structure CodeTags:
    sig
        val tupleTag: Universal.universal list list Universal.tag
        val mergeTupleProps:
            Universal.universal list * Universal.universal list -> Universal.universal list
    end

    structure Sharing:
    sig
        type backendIC = backendIC
        and  bicLoadForm = bicLoadForm
        and  caseType = caseType
        and  pretty = pretty
        and  argumentType = argumentType
        and  bicCodeBinding = bicCodeBinding
        and  bicSimpleBinding = bicSimpleBinding
        and  loadStoreKind = loadStoreKind
        and  blockOpKind = blockOpKind
        and  builtIn0Ops = BuiltIns.builtIn0Ops
        and  builtIn1Ops = BuiltIns.builtIn1Ops
        and  builtIn2Ops = BuiltIns.builtIn2Ops
        and  builtIn3Ops = BuiltIns.builtIn3Ops
    end

end;
