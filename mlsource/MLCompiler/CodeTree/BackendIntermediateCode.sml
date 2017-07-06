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

structure BackendIntermediateCode: BackendIntermediateCodeSig =
struct
    open Address
    
    structure BuiltIns =
    struct
        datatype testConditions =
            TestEqual
        |   TestNotEqual
        |   TestLess
        |   TestLessEqual
        |   TestGreater
        |   TestGreaterEqual

        datatype arithmeticOperations =
            ArithAdd
        |   ArithSub
        |   ArithMult
        |   ArithQuot
        |   ArithRem

        datatype builtIn0Ops =
            CurrentThreadId

        and builtIn1Ops =
            NotBoolean
        |   IsTaggedValue
        |   MemoryCellLength
        |   MemoryCellFlags
        |   ClearMutableFlag
        |   StringLengthWord

        and builtIn2Ops =
            WordComparison of { test: testConditions, isSigned: bool }
        |   FixedPrecisionArith of arithmeticOperations
        |   WordArith of arithmeticOperations
        |   LoadWord of {isImmutable: bool }
        |   LoadByte of { isImmutable: bool }

        and builtIn3Ops =
            StoreWord
        |   StoreByte

        and builtIn4Ops =
            Built4PlaceHolder

        and builtIn5Ops =
            Built5PlaceHolder
        
        fun builtIn0Repr CurrentThreadId = "CurrentThreadId"

        and builtIn1Repr NotBoolean = "NotBoolean"
        |   builtIn1Repr IsTaggedValue = "IsTaggedValue"
        |   builtIn1Repr MemoryCellLength = "MemoryCellLength"
        |   builtIn1Repr MemoryCellFlags = "MemoryCellFlags"
        |   builtIn1Repr ClearMutableFlag = "ClearMutableFlag"
        |   builtIn1Repr StringLengthWord = "StringLengthWord"

        and builtIn2Repr (WordComparison{test, isSigned}) =
                "Test" ^ (testRepr test) ^ (if isSigned then "Signed" else "Unsigned")
        |   builtIn2Repr (FixedPrecisionArith arithOp) = (arithRepr arithOp) ^ "Fixed"
        |   builtIn2Repr (WordArith arithOp) =  (arithRepr arithOp) ^ "Word"
        |   builtIn2Repr (LoadWord {isImmutable=true}) =  "LoadWordImmutable"
        |   builtIn2Repr (LoadWord {isImmutable=false}) =  "LoadWordMutable"
        |   builtIn2Repr (LoadByte {isImmutable=true}) =  "LoadByteImmutable"
        |   builtIn2Repr (LoadByte {isImmutable=false}) =  "LoadByteMutable"
        
        and testRepr TestEqual          = "Equal"
        |   testRepr TestNotEqual       = "NotEqual"
        |   testRepr TestLess           = "Less"
        |   testRepr TestLessEqual      = "LessEqual"
        |   testRepr TestGreater        = "Greater"
        |   testRepr TestGreaterEqual   = "GreaterEqual"
        
        and arithRepr ArithAdd          = "Add"
        |   arithRepr ArithSub          = "Sub"
        |   arithRepr ArithMult         = "Mult"
        |   arithRepr ArithQuot         = "Quot"
        |   arithRepr ArithRem          = "Rem"

        and builtIn3Repr StoreWord = "StoreWord"
        |   builtIn3Repr StoreByte = "StoreByte"

        and builtIn4Repr Built4PlaceHolder = "Built4PlaceHolder"
        and builtIn5Repr Built5PlaceHolder = "Built5PlaceHolder"
    end

    datatype argumentType =
        GeneralType
    |   FloatingPtType
    
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
    |   BICBuiltIn4 of {oper: BuiltIns.builtIn4Ops, arg1: backendIC, arg2: backendIC, arg3: backendIC, arg4: backendIC}
    |   BICBuiltIn5 of {oper: BuiltIns.builtIn5Ops, arg1: backendIC, arg2: backendIC, arg3: backendIC, arg4: backendIC, arg5: backendIC}


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

    structure CodeTags =
    struct
        open Universal
        val tupleTag: universal list list tag = tag()

        fun splitProps _ [] = (NONE, [])
        |   splitProps tag (hd::tl) =
                if Universal.tagIs tag hd
                then (SOME hd, tl)
                else let val (p, l) = splitProps tag tl in (p, hd :: l) end

        fun mergeTupleProps(p, []) = p
        |   mergeTupleProps([], p) = p
        |   mergeTupleProps(m, n) =
            (
                case (splitProps tupleTag m, splitProps tupleTag n) of
                    ((SOME mp, ml), (SOME np, nl)) =>
                    let
                        val mpl = Universal.tagProject tupleTag mp
                        and npl = Universal.tagProject tupleTag np
                        val merge = ListPair.mapEq mergeTupleProps (mpl, npl)
                    in
                        Universal.tagInject tupleTag merge :: (ml @ nl)
                    end
                |   _ => m @ n
            )
    end

    open Pretty

    fun pList ([]: 'b list, _: string, _: 'b->pretty) = []
    |   pList ([h],    _, disp) = [disp h]
    |   pList (h::t, sep, disp) =
        PrettyBlock (0, false, [],
            [
                disp h,
                PrettyBreak (0, 0),
                PrettyString sep
            ]
        ) ::
        PrettyBreak (1, 0) ::
        pList (t, sep, disp)

    fun pretty (pt : backendIC) : pretty =
    let
        
        fun printList(start, lst, sep) : pretty =
            PrettyBlock (1, true, [],
                PrettyString (start ^ "(") ::
                pList(lst, sep, pretty) @
                [ PrettyBreak (0, 0), PrettyString (")") ]
            )

        fun prettyArgType GeneralType = PrettyString "G"
        |   prettyArgType FloatingPtType = PrettyString "F"
        
        fun prettyArg (c, t) =
                PrettyBlock(1, false, [], [pretty c, PrettyBreak (1, 0), prettyArgType t])

        fun prettyArgs(start, lst, sep) : pretty =
            PrettyBlock (1, true, [],
                PrettyString (start ^ "(") ::
                pList(lst, sep, prettyArg) @
                [ PrettyBreak (0, 0), PrettyString (")") ]
            )

    in
        case pt of
            BICEval {function, argList, resultType} =>
            let
                val prettyArgs =
                    PrettyBlock (1, true, [],
                        PrettyString ("$(") ::
                        pList(argList, ",", prettyArg) @
                        [ PrettyBreak (0, 0), PrettyString (")") ]
                    )
            in
                PrettyBlock (3, false, [],
                    [ pretty function, PrettyBreak(1, 0), prettyArgType resultType, PrettyBreak(1, 0), prettyArgs ]
                )
            end

        |   BICBuiltIn0 { oper } =>
                PrettyBlock (3, false, [],
                    [ PrettyString(BuiltIns.builtIn0Repr oper), PrettyBreak(1, 0), printList("", [], ",") ]
                )

        |   BICBuiltIn1 { oper, arg1 } =>
                PrettyBlock (3, false, [],
                    [ PrettyString(BuiltIns.builtIn1Repr oper), PrettyBreak(1, 0), printList("", [arg1], ",") ]
                )

        |   BICBuiltIn2 { oper, arg1, arg2 } =>
                PrettyBlock (3, false, [],
                    [ PrettyString(BuiltIns.builtIn2Repr oper), PrettyBreak(1, 0), printList("", [arg1, arg2], ",") ]
                )

        |   BICBuiltIn3 { oper, arg1, arg2, arg3 } =>
                PrettyBlock (3, false, [],
                    [ PrettyString(BuiltIns.builtIn3Repr oper), PrettyBreak(1, 0), printList("", [arg1, arg2, arg3], ",") ]
                )

        |   BICBuiltIn4 { oper, arg1, arg2, arg3, arg4 } =>
                PrettyBlock (3, false, [],
                    [ PrettyString(BuiltIns.builtIn4Repr oper), PrettyBreak(1, 0), printList("", [arg1, arg2, arg3, arg4], ",") ]
                )

        |   BICBuiltIn5 { oper, arg1, arg2, arg3, arg4, arg5 } =>
                PrettyBlock (3, false, [],
                    [ PrettyString(BuiltIns.builtIn5Repr oper), PrettyBreak(1, 0), printList("", [arg1, arg2, arg3, arg4, arg5], ",") ]
                )

        |   BICExtract (BICLoadLocal addr) =>
            let
                val str : string =
                    concat ["LOCAL(", Int.toString addr, ")"]
            in
                PrettyString str
            end
         
        |   BICExtract (BICLoadArgument addr) =>
            let
                val str : string =
                    concat ["PARAM(", Int.toString addr, ")"]
            in
                PrettyString str
            end

       |   BICExtract (BICLoadClosure addr) =>
            let
                val str : string =
                    concat ["CLOS(", Int.toString addr, ")"]
            in
                PrettyString str
            end

       |   BICExtract (BICLoadRecursive) =>
            let
                val str : string =
                    concat ["RECURSIVE(", ")"]
            in
                PrettyString str
            end

        |   BICField {base, offset} =>
            let
                val str = "INDIRECT(" ^ Int.toString offset ^ ", ";
            in
                PrettyBlock(0, false, [],
                    [ PrettyString str, pretty base, PrettyString ")" ]
                )
            end
        
        |   BICLambda {body, name, closure, argTypes,
                  heapClosure, resultType, localCount} =>
            let
                fun prettyArgTypes [] = []
                |   prettyArgTypes [last] = [prettyArgType last]
                |   prettyArgTypes (hd::tl) = prettyArgType hd :: PrettyBreak(1, 0) :: prettyArgTypes tl
            in
                PrettyBlock (1, true, [],
                    [
                        PrettyString ("LAMBDA("),
                        PrettyBreak (1, 0),
                        PrettyString name,
                        PrettyBreak (1, 0),
                        PrettyString ( "CL="  ^ Bool.toString heapClosure),
                        PrettyString (" LOCALS=" ^ Int.toString localCount),
                        PrettyBreak(1, 0),
                        PrettyBlock (1, false, [], PrettyString "ARGS=" :: prettyArgTypes argTypes),
                        PrettyBreak(1, 0),
                        PrettyBlock (1, false, [], [PrettyString "RES=", prettyArgType resultType]),
                        printList (" CLOS=", map BICExtract closure, ","),
                        PrettyBreak (1, 0),
                        pretty body,
                        PrettyString "){LAMBDA}"
                    ]
                )
            end
        
        |   BICConstnt (w, _) => PrettyString (stringOfWord w)
        
        |   BICCond (f, s, t) =>
            PrettyBlock (1, true, [],
                [
                    PrettyString "IF(",
                    pretty f,
                    PrettyString ", ",
                    PrettyBreak (0, 0),
                    pretty s,
                    PrettyString ", ",
                    PrettyBreak (0, 0),
                    pretty t,
                    PrettyBreak (0, 0),
                    PrettyString (")")
                ]
            )

        |   BICNewenv(decs, final) =>
            PrettyBlock (1, true, [],
                PrettyString ("BLOCK" ^ "(") ::
                pList(decs, ";", prettyBinding) @
                [ PrettyBreak (1, 0), pretty final, PrettyBreak (0, 0), PrettyString (")") ]
            )

        |   BICBeginLoop{loop=loopExp, arguments=args } =>
            let
                fun prettyArg (c, t) =
                    PrettyBlock(1, false, [],
                        [prettySimpleBinding c, PrettyBreak (1, 0), prettyArgType t])
            in
                PrettyBlock (3, false, [],
                    [
                        PrettyBlock (1, true, [],
                            PrettyString ("BEGINLOOP(") ::
                            pList(args, ",", prettyArg) @
                            [ PrettyBreak (0, 0), PrettyString (")") ]
                        ),
                        PrettyBreak (0, 0),
                        PrettyString "(",
                        PrettyBreak (0, 0),
                        pretty loopExp,
                        PrettyBreak (0, 0),
                        PrettyString ")"
                    ]
                )
            end
        
        |   BICLoop ptl => prettyArgs("LOOP", ptl, ",")
        
        |   BICRaise c =>
            PrettyBlock (1, true, [],
                [
                    PrettyString "RAISE(",
                    pretty c,
                    PrettyBreak (0, 0),
                    PrettyString (")")
                ]
            )
        
        |   BICHandle {exp, handler, ...} =>
            PrettyBlock (3, false, [],
                [
                    PrettyString "HANDLE(",
                    pretty exp,
                    PrettyString "WITH",
                    PrettyBreak (1, 0),
                    pretty handler,
                    PrettyString ")"
                ]
            )
        
        |   BICLdexc => PrettyString "LDEXC"
        
        |   BICCase {cases, test, default, caseType} =>
            PrettyBlock (1, true, [],
                PrettyString
                    (concat ["CASE ",
                        case caseType of CaseInt => "INT" | CaseWord => "WORD" | CaseTag n => "TAG " ^ Word.toString n,
                        " (" ]) ::
                pretty test ::
                PrettyBreak (1, 0) ::
                PrettyString "(" ::
                PrettyBreak (1, 0) ::
                pList(cases, ",",
                    fn (exp, label : word) =>
                        PrettyBlock (1, true, [],
                            [
                                PrettyString (Word.toString label ^ ":"),
                                PrettyBreak (1, 0),
                                pretty exp
                            ])
                    ) @
                [
                    PrettyBreak (1, 0),
                    PrettyBlock (1, false, [],
                        [
                            PrettyString "ELSE:",
                            PrettyBreak (1, 0),
                            pretty default
                        ]
                    ),
                    PrettyBreak (1, 0), 
                    PrettyString (") {"^"CASE"^"}")
                ]
            )
         
        |   BICTuple ptl => printList("RECCONSTR", ptl, ",")
        
        |   BICContainer size => PrettyString ("CONTAINER " ^ Int.toString size)
        
        |   BICSetContainer{container, tuple, filter} =>
            let
                val source = BoolVector.length filter
                val dest = BoolVector.foldl(fn (true, n) => n+1 | (false, n) => n) 0 filter
            in
                PrettyBlock (3, false, [],
                    [
                        PrettyString (concat["SETCONTAINER(", Int.toString dest, "/", Int.toString source, ", "]),
                        pretty container,
                        PrettyBreak (0, 0),
                        PrettyString ",",
                        PrettyBreak (1, 0),
                        pretty tuple,
                        PrettyBreak (0, 0),
                        PrettyString ")"
                    ]
                )
            end

        |   BICTagTest { test, tag, maxTag } =>
            PrettyBlock (3, false, [],
                [
                    PrettyString (concat["TAGTEST(", Word.toString tag, ", ", Word.toString maxTag, ","]),
                    PrettyBreak (1, 0),
                    pretty test,
                    PrettyBreak (0, 0),
                    PrettyString ")"
                ]
            )

        (* That list should be exhaustive! *)
    end (* pretty *)

    and prettyBinding(BICDeclar dec) = prettySimpleBinding dec
       
    |   prettyBinding(BICRecDecs ptl) =
        let
            fun prettyRDec {lambda, addr} =
            PrettyBlock (1, false, [],
                [
                    PrettyString (concat ["DECL #", Int.toString addr, "="]),
                    PrettyBreak (1, 0),
                    pretty(BICLambda lambda)
                ]
            )
        in
            PrettyBlock (1, true, [],
                PrettyString ("MUTUAL" ^ "(") ::
                pList(ptl, " AND ", prettyRDec) @
                [ PrettyBreak (0, 0), PrettyString (")") ]
            )
        end
    |   prettyBinding(BICNullBinding c) = pretty c

    and prettySimpleBinding{value, addr} =
        PrettyBlock (1, false, [],
            [
                PrettyString (concat ["DECL #", Int.toString addr, "="]),
                PrettyBreak (1, 0),
                pretty value
            ]
        )

    structure Sharing =
    struct
        type backendIC = backendIC
        and  bicLoadForm = bicLoadForm
        and  caseType = caseType
        and  pretty = pretty
        and  argumentType = argumentType
        and  bicCodeBinding = bicCodeBinding
        and  bicSimpleBinding = bicSimpleBinding
        and  builtIn0Ops = BuiltIns.builtIn0Ops
        and  builtIn1Ops = BuiltIns.builtIn1Ops
        and  builtIn2Ops = BuiltIns.builtIn2Ops
        and  builtIn3Ops = BuiltIns.builtIn3Ops
        and  builtIn4Ops = BuiltIns.builtIn4Ops
        and  builtIn5Ops = BuiltIns.builtIn5Ops
    end

end;
