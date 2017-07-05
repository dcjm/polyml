(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Modified David C. J. Matthews 2008-2010, 2013, 2016

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

(* Signature for the basic codetree types and operations. *)
signature BaseCodeTreeSig =
sig
    type machineWord = Address.machineWord
    
    datatype inlineStatus =
        NonInline
    |   Inline

    datatype argumentType =
        GeneralType
    |   FloatingPtType
    
    structure BuiltIns: BUILTINS

    (* How variables are used.  Added and examined by the optimisation pass. *)
    datatype codeUse =
        UseGeneral (* Used in some other context. *)
    |   UseExport  (* Exported i.e. the result of a top-level binding. *)
    |   UseApply of codeUse list * codetree list
            (* Applied as a function - the list is where the result goes, the codetree list
               is the code that was used for each argument. *)
    |   UseField of int * codeUse list (* Selected as a field - the list is where the result goes *)

    and codetree =
        Newenv of codeBinding list * codetree (* Set of bindings with an expression. *)

    |   Constnt of machineWord * Universal.universal list (* Load a constant *)

    |   Extract of loadForm
    
    |   Indirect of {base: codetree, offset: int, isVariant: bool }
        (* Load a value from a heap record *)

    |   Eval of (* Evaluate a function with an argument list. *)
        {
            function:  codetree,
            argList:   (codetree * argumentType) list,
            resultType: argumentType
        }

        (* Built-in functions. *)
    |   BuiltIn0 of {oper: BuiltIns.builtIn0Ops}
    |   BuiltIn1 of {oper: BuiltIns.builtIn1Ops, arg1: codetree}
    |   BuiltIn2 of {oper: BuiltIns.builtIn2Ops, arg1: codetree, arg2: codetree}
    |   BuiltIn3 of {oper: BuiltIns.builtIn3Ops, arg1: codetree, arg2: codetree, arg3: codetree}
    |   BuiltIn4 of {oper: BuiltIns.builtIn4Ops, arg1: codetree, arg2: codetree, arg3: codetree, arg4: codetree}
    |   BuiltIn5 of {oper: BuiltIns.builtIn5Ops, arg1: codetree, arg2: codetree, arg3: codetree, arg4: codetree, arg5: codetree}

    |   Lambda of lambdaForm (* Lambda expressions. *)

    |   Cond of codetree * codetree * codetree (* If-statement *)
    
    |   BeginLoop of (* Start of tail-recursive inline function. *)
        { loop: codetree, arguments: (simpleBinding * argumentType) list }

    |   Loop of (codetree * argumentType) list (* Jump back to start of tail-recursive function. *)

    |   Raise of codetree (* Raise an exception *)

    |   Ldexc (* Load the exception (used at the start of a handler) *)
    
    |   Handle of (* Exception handler. *) { exp: codetree, handler: codetree }

    |   Tuple of { fields: codetree list, isVariant: bool } (* Tuples and datatypes *)

    |   SetContainer of { container: codetree, tuple: codetree, filter: BoolVector.vector}
         (* Copy a tuple to a container. *)

    |   TagTest of { test: codetree, tag: word, maxTag: word }

    and codeBinding =
        Declar  of simpleBinding (* Make a local declaration or push an argument *)
    |   RecDecs of { addr: int, lambda: lambdaForm, use: codeUse list } list (* Set of mutually recursive declarations. *)
    |   NullBinding of codetree (* Just evaluate the expression and discard the result. *)
    |   Container of { addr: int, use: codeUse list, size: int, setter: codetree }

    and loadForm =
        LoadArgument of int
    |   LoadLocal of int
    |   LoadClosure of int
    |   LoadRecursive

    (* When we look up an entry in the environment we get a pair of
       a "general" value, which is either a constant or a load, and
       an optional special value, which is either a tuple or an
       inline function.  Tuple entries are functions from an integer
       offset to one of these pairs; inline function entries are a
       lambda together with a map for the free variables. *)
    and envGeneral =
        EnvGenLoad of loadForm | EnvGenConst of machineWord * Universal.universal list

    and envSpecial =
        EnvSpecNone
    |   EnvSpecTuple of int * (int -> envGeneral * envSpecial)
    |   EnvSpecInlineFunction of lambdaForm * (int -> envGeneral * envSpecial)
    |   EnvSpecBuiltIn of int * codetree list

    withtype simpleBinding = 
    { (* Declare a value or push an argument. *)
        value:      codetree,
        addr:       int,
        use:        codeUse list
    }

    and lambdaForm =
    { (* Lambda expressions. *)
        body          : codetree,           (* The body of the function. *)
        isInline      : inlineStatus,       (* Whether it's inline - modified by optimiser *)
        name          : string,             (* Text name for profiling etc. *)
        closure       : loadForm list,      (* List of items for closure. *)
        argTypes      : (argumentType * codeUse list) list,  (* "Types" and usage of arguments. *)
        resultType    : argumentType,       (* Result "type" of the function. *)
        localCount    : int,                (* Maximum (+1) declaration address for locals. *)
        recUse        : codeUse list        (* Recursive use of the function *)
    }

    type pretty
    val pretty : codetree -> pretty

    val mapCodetree: (codetree -> codetree option) -> codetree -> codetree

    datatype foldControl = FOLD_DESCEND | FOLD_DONT_DESCEND
    val foldtree: (codetree * 'a -> 'a * foldControl) -> 'a -> codetree -> 'a

    structure CodeTags:
    sig
        val tupleTag: Universal.universal list list Universal.tag
        val inlineCodeTag: envSpecial Universal.tag
        val mergeTupleProps:
            Universal.universal list * Universal.universal list -> Universal.universal list
    end


    structure Sharing:
    sig
        type codetree = codetree
        and  pretty = pretty
        and  inlineStatus = inlineStatus
        and  argumentType = argumentType
        and  codeBinding = codeBinding
        and  simpleBinding = simpleBinding
        and  loadForm = loadForm
        and  envGeneral = envGeneral
        and  envSpecial = envSpecial
        and  codeUse = codeUse
        and  foldControl = foldControl
        and  builtIn0Ops = BuiltIns.builtIn0Ops
        and  builtIn1Ops = BuiltIns.builtIn1Ops
        and  builtIn2Ops = BuiltIns.builtIn2Ops
        and  builtIn3Ops = BuiltIns.builtIn3Ops
        and  builtIn4Ops = BuiltIns.builtIn4Ops
        and  builtIn5Ops = BuiltIns.builtIn5Ops
    end

end;
