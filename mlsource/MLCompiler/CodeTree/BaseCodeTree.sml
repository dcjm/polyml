(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Modified David C. J. Matthews 2008-2010, 2013, 2015

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

structure BaseCodeTree: BaseCodeTreeSig =
struct
    open Address

    datatype argumentType = datatype BackendIntermediateCode.argumentType
    
    structure BuiltIns = BackendIntermediateCode.BuiltIns

    datatype inlineStatus =
        NonInline
    |   Inline

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

    |   SetContainer of (* Copy a tuple to a container. *)
        {
            container: codetree,
            tuple:     codetree,
            filter:    BoolVector.vector
        }

    |   TagTest of { test: codetree, tag: word, maxTag: word }

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
    |   EnvSpecBuiltIn1 of BuiltIns.builtIn1Ops * codetree
    |   EnvSpecBuiltIn2 of BuiltIns.builtIn2Ops * codetree * codetree
    
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

        fun prettyArg (c, _) = pretty c

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
                                printList("", arglist, ","),
                                PrettyBreak (0, 0),
                                PrettyString (")")
                            ]
                        )
                    ]
                )
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

        |   BuiltIn0 { oper } =>
                prettyBuiltin(BuiltIns.builtIn0Repr oper, [])

        |   BuiltIn1 { oper, arg1 } =>
                prettyBuiltin(BuiltIns.builtIn1Repr oper, [arg1])

        |   BuiltIn2 { oper, arg1, arg2 } =>
                prettyBuiltin(BuiltIns.builtIn2Repr oper, [arg1, arg2])

        |   BuiltIn3 { oper, arg1, arg2, arg3 } =>
                prettyBuiltin(BuiltIns.builtIn3Repr oper, [arg1, arg2, arg3])

        |   BuiltIn4 { oper, arg1, arg2, arg3, arg4 } =>
                prettyBuiltin(BuiltIns.builtIn4Repr oper, [arg1, arg2, arg3, arg4])

        |   BuiltIn5 { oper, arg1, arg2, arg3, arg4, arg5 } =>
                prettyBuiltin(BuiltIns.builtIn5Repr oper, [arg1, arg2, arg3, arg4, arg5])

        |   Extract(LoadArgument addr) => string ("Arg" ^ Int.toString addr)
        |   Extract(LoadLocal addr) => string ("Local" ^ Int.toString addr)
        |   Extract(LoadClosure addr) => string ("Closure" ^ Int.toString addr)
        |   Extract LoadRecursive => string "Recursive"

        |   Indirect {base, offset, isVariant} =>
                PrettyBlock(2, false, [],
                    [
                        pretty base,
                        PrettyBreak(0, 2),
                        string(concat["[", Int.toString offset, "]", if isVariant then "(*V*)" else ""])
                    ]
                )
        
        | Lambda {body, isInline, name, closure, argTypes, localCount, recUse, ...} =>
            let
                val inl = 
                    case isInline of
                      NonInline   => ""
                    | Inline => "inline,"
                fun printArgs(n, (t, u) :: rest) =
                    PrettyBlock(4, false, [],
                        [
                            string("Arg"^Int.toString n),
                            space,
                            prettyUses "" u
                        ] @
                        (
                            case t of
                                GeneralType => []
                            |   FloatingPtType => [ space, string ":real" ]
                        ) @
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
                        string ")",
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

        |   Handle {exp, handler, ...} =>
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
        
        | Ldexc => PrettyString "LDEXC"
         
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
        |   mapt (Indirect { base, offset, isVariant }) =
                Indirect{ base = mapCodetree f base, offset = offset, isVariant = isVariant }
        |   mapt (Eval { function, argList, resultType }) =
                Eval {
                    function = mapCodetree f function, 
                    argList = map (fn(c, a) => (mapCodetree f c, a)) argList,
                    resultType = resultType
                }
        |   mapt(b as BuiltIn0 _) = b
        |   mapt(BuiltIn1 { oper, arg1 }) =
                BuiltIn1 { oper = oper, arg1 = mapCodetree f arg1 }
        |   mapt(BuiltIn2 { oper, arg1, arg2 }) =
                BuiltIn2 { oper = oper, arg1 = mapCodetree f arg1, arg2 = mapCodetree f arg2 }
        |   mapt(BuiltIn3 { oper, arg1, arg2, arg3 }) =
                BuiltIn3 { oper = oper, arg1 = mapCodetree f arg1, arg2 = mapCodetree f arg2, arg3 = mapCodetree f arg3 }
        |   mapt(BuiltIn4 { oper, arg1, arg2, arg3, arg4 }) =
                BuiltIn4 { oper = oper, arg1 = mapCodetree f arg1, arg2 = mapCodetree f arg2,
                           arg3 = mapCodetree f arg3, arg4 = mapCodetree f arg4 }
        |   mapt(BuiltIn5 { oper, arg1, arg2, arg3, arg4, arg5 }) =
                BuiltIn5 { oper = oper, arg1 = mapCodetree f arg1, arg2 = mapCodetree f arg2,
                           arg3 = mapCodetree f arg3, arg4 = mapCodetree f arg4, arg5 = mapCodetree f arg5 }
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
        |   mapt Ldexc = Ldexc
        |   mapt (Handle{exp, handler}) = Handle{exp=mapCodetree f exp, handler=mapCodetree f handler }
        |   mapt (Tuple { fields, isVariant} ) = Tuple { fields = map (mapCodetree f) fields, isVariant = isVariant }
        |   mapt (SetContainer{container, tuple, filter}) =
                SetContainer{
                    container = mapCodetree f container, tuple = mapCodetree f tuple, filter = filter }
        |   mapt (TagTest{test, tag, maxTag}) = TagTest{test = mapCodetree f test, tag = tag, maxTag = maxTag }
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
        |   ftree (BuiltIn0 _, v) = v
        |   ftree (BuiltIn1 {arg1, ...}, v) = foldtree f v arg1
        |   ftree (BuiltIn2 {arg1, arg2, ...}, v) = foldtree f (foldtree f v arg1) arg2
        |   ftree (BuiltIn3 {arg1, arg2, arg3, ...}, v) = foldtree f (foldtree f (foldtree f v arg1) arg2) arg3
        |   ftree (BuiltIn4 {arg1, arg2, arg3, arg4, ...}, v) =
                foldtree f (foldtree f (foldtree f (foldtree f v arg1) arg2) arg3) arg4
        |   ftree (BuiltIn5 {arg1, arg2, arg3, arg4, arg5, ...}, v) =
                foldtree f (foldtree f (foldtree f (foldtree f (foldtree f v arg1) arg2) arg3) arg4) arg5
        |   ftree (Lambda { body, closure, ...}, v) =
                foldtree f (foldl (fn (c, w) => foldtree f w (Extract c)) v closure) body
        |   ftree (Cond(i, t, e), v) = foldtree f (foldtree f (foldtree f v i) t) e
        |   ftree (BeginLoop{loop, arguments, ...}, v) =
                foldtree f (foldl (fn (({value, ...}, _), w) => foldtree f w value) v arguments) loop
        |   ftree (Loop l, v) = foldl (fn ((c, _), w) => foldtree f w c) v l
        |   ftree (Raise r, v) = foldtree f v r
        |   ftree (Ldexc, v) = v
        |   ftree (Handle{exp, handler}, v) = foldtree f (foldtree f v exp) handler
        |   ftree (Tuple { fields, ...}, v) = foldl (fn (c, w) => foldtree f w c) v fields
        |   ftree (SetContainer { container, tuple, ...}, v) = foldtree f (foldtree f v container) tuple
        |   ftree (TagTest{test, ...}, v) = foldtree f v test
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
