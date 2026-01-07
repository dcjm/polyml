(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Modified David C. J. Matthews 2008-2010, 2013, 2015, 2017-22, 2025.

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

(* Basic code-tree data structure.  This was previously partly in GCODE.ML
   and partly in CODETREE.ML. *)

structure BaseCodeTree: BASECODETREE =
struct
    open Address

    datatype argumentType = datatype BackendIntermediateCode.argumentType
    datatype loadStoreKind = datatype BackendIntermediateCode.loadStoreKind
    datatype blockOpKind = datatype BackendIntermediateCode.blockOpKind

    structure BuiltIns = BackendIntermediateCode.BuiltIns
    
    datatype arbPrecisionOps =
        ArbCompare of BuiltIns.testConditions
    |   ArbArith of BuiltIns.arithmeticOperations

    datatype inlineStatus =
        DontInline
    |   InlineAlways
    |   SmallInline

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

    |   Extract of loadForm (* Get a local variable, an argument or a closure value *)
    
    |   Indirect of {base: codetree, offset: int, indKind: indKind }
         (* Load a value from the heap or the stack. *)
    
    |   Eval of (* Evaluate a function with an argument list. *)
        {
            function:  codetree,
            argList:   (codetree * argumentType) list,
            resultType: argumentType
        }

        (* Built-in functions. *)
    |   Nullary of {oper: BuiltIns.nullaryOps}
    |   Unary of {oper: BuiltIns.unaryOps, arg1: codetree}
    |   Binary of {oper: BuiltIns.binaryOps, arg1: codetree, arg2: codetree}

        (* Arbitrary precision operations.  This combines some conditionals
           with the operation.  shortCond is the condition that must be satisfied
           for the short precision operation to be executed.  longCall is called
           if either argument is long or the evaluation overflows. *)
    |   Arbitrary of
            {oper: arbPrecisionOps, shortCond: codetree, arg1: codetree, arg2: codetree, longCall: codetree}

    |   Lambda of lambdaForm (* Lambda expressions. *)

    |   Cond of codetree * codetree * codetree (* If-statement *)
    
    |   BeginLoop of (* Start of tail-recursive inline function. *)
        { loop: codetree, arguments: (simpleBinding * argumentType) list }

    |   Loop of (codetree * argumentType) list (* Jump back to start of tail-recursive function. *)

    |   Raise of codetree (* Raise an exception *)

    |   Handle of (* Exception handler. *) { exp: codetree, handler: codetree, exPacketAddr: int }

    |   Tuple of { fields: codetree list, isVariant: bool } (* Tuples and datatypes *)

    |   SetContainer of (* Copy a tuple to a container. *)
        {
            container: codetree,
            tuple:     codetree,
            filter:    BoolVector.vector
        }

    |   TagTest of { test: codetree, tag: word, maxTag: word }

    |   LoadOperation of { kind: loadStoreKind, address: codeAddress }
    
    |   StoreOperation of { kind: loadStoreKind, address: codeAddress, value: codetree }
    
    |   BlockOperation of
            { kind: blockOpKind, sourceLeft: codeAddress, destRight: codeAddress, length: codetree }

    |   AllocateWordMemory of {numWords: codetree, flags: codetree, initial: codetree}

    and codeBinding =
        Declar  of simpleBinding (* Make a local declaration or push an argument *)
    |   RecDecs of { addr: int, lambda: lambdaForm, use: codeUse list } list (* Set of mutually recursive declarations. *)
    |   NullBinding of codetree (* Just evaluate the expression and discard the result. *)
    |   Container of { addr: int, use: codeUse list, size: int, setter: codetree }
                (* Container: allocate a piece of stack space and set it to
                   the values from a tuple.  *)

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

    (* Special entries.  The type of both EnvSpecTuple and EnvSpecInlineFunction
       includes a function from int, the index, to the (general, special) pair
       rather than a list of either fields or closure entries.  The main
       reason is that if we have a function that contains a reference to, say a tuple,
       in its closure we can pass in a EnvSpecTuple entry with a function that
       only adds a field to the closure if the field is actually used.  Passing
       a list would require adding all the fields to the closure at the time
       the EnvSpecTuple was passed.
       EnvSpecBuiltInX are used for a small number of built-in functions which
       can be simplied if they occur in combination with others. *)
    and envSpecial =
        EnvSpecNone
    |   EnvSpecTuple of int * (int -> envGeneral * envSpecial)
    |   EnvSpecInlineFunction of lambdaForm * (int -> envGeneral * envSpecial)
    |   EnvSpecUnary of BuiltIns.unaryOps * codetree
    |   EnvSpecBinary of BuiltIns.binaryOps * codetree * codetree

    (* Indirection types.
       IndTuple is from a tuple so the field will always be present;
       IndVariant is from a datatype which may have other variants that do not have the field;
       IndContainer is from a container (a set of words on the stack). *)
    and indKind = IndTuple | IndVariant | IndContainer
    
    withtype simpleBinding = 
    { (* Declare a value or push an argument. *)
        value:      codetree,
        addr:       int,
        use:        codeUse list
    }
    
    and lambdaForm =
    { (* Lambda expressions. *)
        body          : codetree,
        isInline      : inlineStatus,
        name          : string,
        closure       : loadForm list,
        argTypes      : (argumentType * codeUse list) list,
        resultType    : argumentType,
        localCount    : int,
        recUse        : codeUse list
    }

    and codeAddress = {base: codetree, index: codetree option, offset: int}

    structure CodeTags =
    struct
        open Universal
        (* Import tags from back end *)
        open BackendIntermediateCode.CodeTags

        val inlineCodeTag: envSpecial tag = tag()
    end

    open Pretty

    (* Common cases. *)
    val space = PrettyBreak (1, 0)
    fun block l = PrettyBlock (0, false, [], l)
    val string = PrettyString

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

    fun pretty (pt : codetree) : pretty =
    let

        fun printList(start, lst, sep) : pretty =
            PrettyBlock (1, true, [],
                PrettyString (start ^ "(") ::
                pList(lst, sep, pretty) @
                [ PrettyBreak (0, 0), PrettyString (")") ]
            )

        fun genType GeneralType = []
        |   genType DoubleFloatType = [ space, string ":double" ]
        |   genType SingleFloatType = [ space, string ":float" ]
        |   genType (ContainerType n) = [ space, string ":multiple ", space, string(Int.toString n) ]

        fun prettyArg (c, t) =
            PrettyBlock(1, false, [], pretty c :: genType t)

        fun prettyArgs(start, lst, sep) : pretty =
            PrettyBlock (1, true, [],
                PrettyString (start ^ "(") ::
                pList(lst, sep, prettyArg) @
                [ PrettyBreak (0, 0), PrettyString (")") ]
            )

        fun prettyBuiltin(opers, arglist) =
                PrettyBlock (2, false, [],
                    [
                        PrettyString opers,
                        PrettyBreak(1, 2),
                        PrettyBlock(2, true, [],
                            [
                                printList("", arglist, ",")
                            ]
                        )
                    ]
                )

        fun prettyAddress({base, index, offset}: codeAddress): pretty =
        let
        in
            PrettyBlock (1, true, [],
                [
                    PrettyString "[", PrettyBreak (0, 3),
                    pretty base,
                    PrettyBreak (0, 0), PrettyString ",", PrettyBreak (1, 0), 
                    case index of NONE => PrettyString "-" | SOME i => pretty i,
                    PrettyBreak (0, 0), PrettyString ",", PrettyBreak (1, 0),
                    PrettyString(Int.toString offset), PrettyBreak (0, 0), PrettyString "]"
                ])
        end
    in
        case pt of
            Eval {function, argList, ...} =>
                PrettyBlock (2, false, [],
                    [
                        case function of
                            Extract _ => pretty function
                        |   Constnt _ => pretty function
                        |   _ => PrettyBlock(2, true, [],
                                    [
                                        string "(",
                                        PrettyBreak(0, 0),
                                        pretty function,
                                        PrettyBreak(0, 0),
                                        string ")"
                                    ]
                                 )
                        ,
                        PrettyBreak(1, 2),
                        PrettyBlock(2, true, [],
                            (
                                string "(" ::
                                PrettyBreak(0, 0) ::
                                pList(argList, ",", prettyArg) @
                                [PrettyBreak (0, 0), PrettyString (")")]
                            )
                        )
                    ]
                )

        |   Unary { oper, arg1 } =>
                prettyBuiltin(BuiltIns.unaryRepr oper, [arg1])

        |   Binary { oper, arg1, arg2 } =>
                prettyBuiltin(BuiltIns.binaryRepr oper, [arg1, arg2])

        |   Nullary { oper } => PrettyString(BuiltIns.nullaryRepr oper)

        |   Arbitrary { oper, shortCond, arg1, arg2, longCall } =>
            let
                val operName =
                    case oper of
                        ArbCompare test => BuiltIns.testRepr test
                    |   ArbArith arith => BuiltIns.arithRepr arith
            in
                prettyBuiltin(operName ^ "Arbitrary", [shortCond, arg1, arg2, longCall])
            end

        |   AllocateWordMemory { numWords, flags, initial } =>
                prettyBuiltin("AllocateWordMemory", [numWords, flags, initial])

        |   Extract(LoadArgument addr) => string ("Arg" ^ Int.toString addr)
        |   Extract(LoadLocal addr) => string ("Local" ^ Int.toString addr)
        |   Extract(LoadClosure addr) => string ("Closure" ^ Int.toString addr)
        |   Extract LoadRecursive => string "Recursive"

        |   Indirect {base, offset, indKind} =>
                PrettyBlock(2, false, [],
                    [
                        pretty base,
                        PrettyBreak(0, 2),
                        string(concat["[", Int.toString offset, "]",
                            case indKind of IndTuple => "" | IndVariant => "(*V*)" | IndContainer => "(*C*)"])
                    ]
                )
        
        | Lambda {body, isInline, name, closure, argTypes, localCount, recUse, resultType, ...} =>
            let
                val inl = 
                    case isInline of
                        DontInline   => ""
                    |   InlineAlways => "inline,"
                    |   SmallInline => "small,"
                fun printArgs(n, (t, u) :: rest) =
                    PrettyBlock(4, false, [],
                        [
                            string("Arg"^Int.toString n),
                            space,
                            prettyUses "" u
                        ] @ genType t @
                        (
                            if null rest then []
                            else [PrettyBreak(0,0), string ",", space]
                        )
                    ) :: printArgs(n+1, rest)
                |   printArgs(_, []) = []
            in
                PrettyBlock(2, true, [],
                [
                    PrettyBlock(4, false, [],
                    [
                        string "fn(",
                        space,
                        block(printArgs(0, argTypes)),
                        space,
                        string ")"] @ genType resultType @
                    [
                        space,
                        string "(*", space, string("\"" ^ name ^ "\""), space, string inl,
                        space, string(Int.toString localCount ^ " locals,"), space,
                        printList ("closure=", map Extract closure, ","),
                        space,
                        prettyUses "recursive use=" recUse,
                        space, string "*)"
                    ]),
                    PrettyBreak(1, 2),
                    pretty body
                ])
            end
        
        |   Constnt(w, m) =>
            if isShort w andalso toShort w = 0w0
            then
            (
                case List.find (Universal.tagIs CodeTags.inlineCodeTag) m of
                    SOME h =>
                    (
                        case Universal.tagProject CodeTags.inlineCodeTag h of
                            EnvSpecInlineFunction(lambda, _) => pretty(Lambda lambda)
                        |   _ => PrettyString (stringOfWord w)
                    )
                |   NONE => PrettyString (stringOfWord w)
            )
            else PrettyString (stringOfWord w)

        |   Cond (f, s, t) =>
            PrettyBlock (0, true, [],
                [
                    PrettyBlock(2, false, [], [string "if", space, pretty f]),
                    space,
                    PrettyBlock(2, false, [], [string "then", space, pretty s]),
                    space,
                    PrettyBlock(2, false, [], [string "else", space, pretty t])
                ]
            )

        | Newenv(decs, final) =>
            PrettyBlock (0, true, [],
                [
                    string "let",
                    PrettyBreak (1, 2),
                    PrettyBlock(2, true, [], pList(decs, ";", prettyBinding)),
                    space,
                    string "in",
                    PrettyBreak(1, 2),
                    PrettyBlock(2, true, [], [pretty final]),
                    space,
                    string "end"
                ]
            )

        | BeginLoop{loop=loopExp, arguments=args } =>
            let
                fun prettyArg (c, _) = prettySimpleBinding c
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
        
        |   Loop ptl => prettyArgs("LOOP", ptl, ",")
        
        |   Raise c =>
            PrettyBlock (1, true, [],
                [
                    PrettyString "RAISE(",
                    pretty c,
                    PrettyBreak (0, 0),
                    PrettyString (")")
                ]
            )

        |   Handle {exp, handler, exPacketAddr} =>
            PrettyBlock (3, false, [],
                [
                    PrettyString "exceptions(",
                    pretty exp,
                    PrettyBreak (1, 0),
                    PrettyBlock(3, false, [],
                        [
                            PrettyBlock(0, false, [],
                            [
                                PrettyString ("handle"), PrettyBreak(1, 3),
                                PrettyString ("Local" ^ Int.toString exPacketAddr ^ "=packet"), PrettyBreak(1, 3),
                                PrettyString ("in")
                            ]),
                            PrettyBreak (1, 0),
                            pretty handler
                        ]),
                    PrettyString ")"
                ]
            )
         
        |   Tuple { fields, isVariant } =>
                printList(if isVariant then "DATATYPE" else "TUPLE", fields, ",")

        |   SetContainer{container, tuple, filter} =>
            let
                val source = BoolVector.length filter
                val dest = BoolVector.foldl(fn (true, n) => n+1 | (false, n) => n) 0 filter
            in
                PrettyBlock (3, false, [],
                    [
                        string (concat["SETCONTAINER(", Int.toString dest, "/", Int.toString source, ", "]),
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

        |   TagTest { test, tag, maxTag } =>
            PrettyBlock (3, false, [],
                [
                    PrettyString (concat["TAGTEST(", Word.toString tag, ", ", Word.toString maxTag, ","]),
                    PrettyBreak (1, 0),
                    pretty test,
                    PrettyBreak (0, 0),
                    PrettyString ")"
                ]
            )

        |   LoadOperation{ kind, address } =>
            PrettyBlock (3, false, [],
                [
                    PrettyString("Load" ^ BackendIntermediateCode.loadStoreKindRepr kind),
                    PrettyBreak (1, 0),
                    prettyAddress address
                ]
            )

        |   StoreOperation{ kind, address, value } =>
            PrettyBlock (3, false, [],
                [
                    PrettyString("Store" ^ BackendIntermediateCode.loadStoreKindRepr kind),
                    PrettyBreak (1, 0),
                    prettyAddress address,
                    PrettyBreak (1, 0),
                    PrettyString "<=",
                    PrettyBreak (1, 0),
                    pretty value
                ]
            )

        |   BlockOperation{ kind, sourceLeft, destRight, length } =>
            PrettyBlock (3, false, [],
                [
                    PrettyString(BackendIntermediateCode.blockOpKindRepr kind ^ "("),
                    PrettyBreak (1, 0),
                    prettyAddress sourceLeft,
                    PrettyBreak (1, 0), PrettyString ",",
                    prettyAddress destRight,
                    PrettyBreak (1, 0), PrettyString ",",
                    pretty length,
                    PrettyBreak (1, 0), PrettyString ")"
                ]
            )

        (* That list should be exhaustive! *)
    end (* pretty *)

    and prettyBinding(Declar dec) = prettySimpleBinding dec
       
    |   prettyBinding(RecDecs ptl) =
        let
            fun prettyRDec {lambda, addr, use, ...} =
            block [
                    string ("Local" ^ Int.toString addr),
                    space,
                    string "(*",
                    prettyUses "" use,
                    space,
                    string "*)",
                    space,
                    string "=",
                    PrettyBreak (1, 2),
                    PrettyBlock (2, false, [], [pretty(Lambda lambda)])
                ]
        in
            PrettyBlock(0, true, [],
                string "val rec " ::
                pList(ptl, " and ", prettyRDec)
            )
        end
    |   prettyBinding(NullBinding c) = pretty c
    |   prettyBinding(Container{addr, use, size, setter}) =
            PrettyBlock(1, false, [],
                [
                    string ("val Local" ^ Int.toString addr),
                    space,
                    string "(*",
                    string "",
                    space,
                    prettyUses "" use,
                    space,
                    string "*)",
                    space,
                    string ("= Container " ^  Int.toString size),
                    space,
                    string "with",
                    space,
                    pretty setter
                ]
            )

    and prettySimpleBinding{value, addr, use, ...} =
        PrettyBlock (1, false, [],
            [
                string ("val Local" ^ Int.toString addr),
                space,
                string "(*",
                string "",
                space,
                prettyUses "" use,
                space,
                string "*)",
                space,
                string "=",
                PrettyBreak (1, 2),
                PrettyBlock (2, false, [], [pretty value])
            ]
        )

    and prettyUses prefix cl =
           PrettyBlock (1, true, [],
                PrettyString (prefix ^ "[") ::
                pList(cl, ",", prettyUsage) @
                [ PrettyBreak (0, 0), PrettyString ("]") ]
            )

    and prettyUsage UseGeneral = PrettyString "_"
    |   prettyUsage UseExport = PrettyString "Export"
    |   prettyUsage (UseApply (cl, al)) =
           PrettyBlock (1, true, [],
                string "(" ::
                pList(al, "|", fn _ => string "-") @
                string ")" ::
                space ::
                string "->" ::
                space ::
                string "(" ::
                pList(cl, "|", prettyUsage) @
                [ PrettyBreak (0, 0), string ")" ]
            )
    |   prettyUsage (UseField (n, cl)) =
           PrettyBlock (1, true, [],
                string ("UseField"^ Int.toString n ^ "[") ::
                pList(cl, ",", prettyUsage) @
                [ PrettyBreak (0, 0), string "]" ]
            )

    (* Mapping function to enable parts of the tree to be replaced. *)
    fun mapCodetree f code =
    let
        (* We use these functions to allow all nodes to be processed even if
           they are not full codetree nodes. *)
        fun deExtract(Extract l) = l | deExtract _ = raise Misc.InternalError "deExtract"
        fun deLambda (Lambda l) = l | deLambda _ = raise Misc.InternalError "deLambda"

        fun mapt (Newenv(decs, exp)) =
            let
                fun mapbinding(Declar{value, addr, use}) = Declar{value=mapCodetree f value, addr=addr, use=use}
                |   mapbinding(RecDecs l) =
                        RecDecs(map(fn {addr, lambda, use} =>
                            {addr=addr, use = use, lambda = deLambda(mapCodetree f (Lambda lambda))}) l)
                |   mapbinding(NullBinding exp) = NullBinding(mapCodetree f exp)
                |   mapbinding(Container{addr, use, size, setter}) =
                        Container{addr=addr, use=use, size=size, setter=mapCodetree f setter}
            in
                Newenv(map mapbinding decs, mapCodetree f exp)
            end
        |   mapt (c as Constnt _) = c
        |   mapt (e as Extract _) = e
        |   mapt (Indirect { base, offset, indKind }) =
                Indirect{ base = mapCodetree f base, offset = offset, indKind = indKind }
        |   mapt (Eval { function, argList, resultType }) =
                Eval {
                    function = mapCodetree f function, 
                    argList = map (fn(c, a) => (mapCodetree f c, a)) argList,
                    resultType = resultType
                }
        |   mapt(nullary as Nullary _) = nullary
        |   mapt(Unary { oper, arg1 }) =
                Unary { oper = oper, arg1 = mapCodetree f arg1 }
        |   mapt(Binary { oper, arg1, arg2 }) =
                Binary { oper = oper, arg1 = mapCodetree f arg1, arg2 = mapCodetree f arg2 }
        |   mapt(Arbitrary { oper, shortCond, arg1, arg2, longCall }) =
                Arbitrary {
                    oper = oper, shortCond = mapCodetree f shortCond, arg1 = mapCodetree f arg1,
                    arg2 = mapCodetree f arg2, longCall = mapCodetree f longCall }
        |   mapt(AllocateWordMemory { numWords, flags, initial }) =
                AllocateWordMemory { numWords = mapCodetree f numWords, flags = mapCodetree f flags, initial = mapCodetree f initial }
        |   mapt (Lambda { body, isInline, name, closure, argTypes, resultType, localCount, recUse }) =
                Lambda {
                    body = mapCodetree f body, isInline = isInline, name = name,
                    closure = map (deExtract o (mapCodetree f) o Extract) closure,
                    argTypes = argTypes, resultType = resultType, localCount = localCount,
                    recUse = recUse
                }
        |   mapt (Cond(i, t, e)) = Cond(mapCodetree f i, mapCodetree f t, mapCodetree f e)
        |   mapt (BeginLoop{loop, arguments}) =
                BeginLoop {
                    loop = mapCodetree f loop,
                    arguments = map(fn({value, addr, use}, t) => ({value=mapCodetree f value, addr=addr, use=use}, t)) arguments
                
                }
        |   mapt (Loop l) = Loop (map(fn(c, t) => (mapCodetree f c, t)) l)
        |   mapt (Raise r) = Raise(mapCodetree f r)
        |   mapt (Handle{exp, handler, exPacketAddr}) =
                Handle{exp=mapCodetree f exp, handler=mapCodetree f handler, exPacketAddr=exPacketAddr }
        |   mapt (Tuple { fields, isVariant} ) = Tuple { fields = map (mapCodetree f) fields, isVariant = isVariant }
        |   mapt (SetContainer{container, tuple, filter}) =
                SetContainer{
                    container = mapCodetree f container, tuple = mapCodetree f tuple, filter = filter }
        |   mapt (TagTest{test, tag, maxTag}) = TagTest{test = mapCodetree f test, tag = tag, maxTag = maxTag }
        |   mapt (LoadOperation{kind, address}) = LoadOperation{kind = kind, address = maptAddress address }
        |   mapt (StoreOperation{kind, address, value}) =
                    StoreOperation{kind = kind, address = maptAddress address, value=mapCodetree f value }
        |   mapt (BlockOperation{kind, sourceLeft, destRight, length}) =
                    BlockOperation{kind = kind, sourceLeft = maptAddress sourceLeft,
                                   destRight = maptAddress destRight, length=mapCodetree f length }
        
        and maptAddress({base, index, offset}: codeAddress): codeAddress =
            {base=mapCodetree f base, index=case index of NONE => NONE | SOME i => SOME(mapCodetree f i), offset=offset}
    in
        (* Apply f to node.  If it returns SOME c use that otherwise
           traverse the tree. *)
        case f code of
            SOME c => c
        |   NONE => mapt code
    end

    (* Fold a function over the tree.  f is applied to the node and the input
       value and returns an output and a flag.  If the flag is
       FOLD_DONT_DESCEND the output value is used and the code tree is not
       examined further.  Otherwise this function descends into the tree and
       folds over the subtree. *)
    datatype foldControl = FOLD_DESCEND | FOLD_DONT_DESCEND

    fun foldtree (f: codetree * 'a -> 'a * foldControl) (input: 'a) code =
    let
        fun ftree (Newenv(decs, exp), v) =
            let
                fun foldbinding(Declar{value, ...}, w) = foldtree f w value
                |   foldbinding(RecDecs l, w) =
                        foldl(fn ({lambda, ...}, x) => foldtree f x (Lambda lambda)) w l
                |   foldbinding(NullBinding exp, w) = foldtree f w exp
                |   foldbinding(Container{setter, ...}, w) = foldtree f w setter
            in
                foldtree f (foldl foldbinding v decs) exp
            end
        |   ftree (Constnt _, v) = v
        |   ftree (Extract _, v) = v
        |   ftree (Indirect{base, ...}, v) = foldtree f v base
        |   ftree (Eval { function, argList, ...}, v) =
                foldl(fn((c, _), w) => foldtree f w c) (foldtree f v function) argList
        |   ftree (Nullary _, v) = v
        |   ftree (Unary {arg1, ...}, v) = foldtree f v arg1
        |   ftree (Binary {arg1, arg2, ...}, v) = foldtree f (foldtree f v arg1) arg2
        |   ftree (Arbitrary {shortCond, arg1, arg2, longCall, ...}, v) =
                foldtree f (foldtree f (foldtree f (foldtree f v shortCond) arg1) arg2) longCall
        |   ftree (AllocateWordMemory {numWords, flags, initial}, v) = foldtree f (foldtree f (foldtree f v numWords) flags) initial
        |   ftree (Lambda { body, closure, ...}, v) =
                foldtree f (foldl (fn (c, w) => foldtree f w (Extract c)) v closure) body
        |   ftree (Cond(i, t, e), v) = foldtree f (foldtree f (foldtree f v i) t) e
        |   ftree (BeginLoop{loop, arguments, ...}, v) =
                foldtree f (foldl (fn (({value, ...}, _), w) => foldtree f w value) v arguments) loop
        |   ftree (Loop l, v) = foldl (fn ((c, _), w) => foldtree f w c) v l
        |   ftree (Raise r, v) = foldtree f v r
        |   ftree (Handle{exp, handler, ...}, v) = foldtree f (foldtree f v exp) handler
        |   ftree (Tuple { fields, ...}, v) = foldl (fn (c, w) => foldtree f w c) v fields
        |   ftree (SetContainer { container, tuple, ...}, v) = foldtree f (foldtree f v container) tuple
        |   ftree (TagTest{test, ...}, v) = foldtree f v test
        |   ftree (LoadOperation{address, ...}, v) = fAddress address v
        |   ftree (StoreOperation{address, value, ...}, v) = foldtree f (fAddress address v) value
        |   ftree (BlockOperation{sourceLeft, destRight, length, ...}, v) =
                    foldtree f (fAddress sourceLeft (fAddress destRight v)) length
        
        and fAddress {base, index=NONE, ...} v = foldtree f v base
        |   fAddress {base, index=SOME index, ...} v = foldtree f (foldtree f v base) index
    in
        case f (code, input) of
            (v, FOLD_DONT_DESCEND) => v
        |   (v, FOLD_DESCEND) => ftree(code, v)
    end


    structure Sharing =
    struct
        type codetree = codetree
        and  pretty = pretty
        and  inlineStatus = inlineStatus
        and  argumentType = argumentType
        and  loadStoreKind = loadStoreKind
        and  blockOpKind = blockOpKind
        and  codeBinding = codeBinding
        and  simpleBinding = simpleBinding
        and  loadForm = loadForm
        and  envGeneral = envGeneral
        and  envSpecial = envSpecial
        and  codeUse = codeUse
        and  foldControl = foldControl
        and  unaryOps = BuiltIns.unaryOps
        and  binaryOps = BuiltIns.binaryOps
        and  nullaryOps = BuiltIns.nullaryOps
        and  arbPrecisionOps = arbPrecisionOps
        and  testConditions = BuiltIns.testConditions
        and  arithmeticOperations = BuiltIns.arithmeticOperations
    end

end;
