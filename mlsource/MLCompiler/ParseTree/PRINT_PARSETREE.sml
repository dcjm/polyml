(*
    Copyright (c) 2013, 2025 David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(*
    Derived from the original parse-tree

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further development:
    Copyright (c) 2000-13 David C.J. Matthews

    Title:      Parse Tree Structure and Operations.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985

*)

functor PRINT_PARSETREE (

    structure BASEPARSETREE : BaseParseTreeSig
    structure LEX : LEXSIG
    structure STRUCTVALS : STRUCTVALSIG;
    structure TYPETREE : TYPETREESIG
    structure VALUEOPS : VALUEOPSSIG;
    structure PRETTY : PRETTY

    structure MISC :
    sig
        exception InternalError of string;
    end;

    sharing LEX.Sharing = TYPETREE.Sharing = STRUCTVALS.Sharing
           = VALUEOPS.Sharing = PRETTY.Sharing
           = BASEPARSETREE.Sharing

): PrintParsetreeSig
=
struct 
    open LEX
    open STRUCTVALS
    open VALUEOPS
    open TYPETREE
    open PRETTY
    open BASEPARSETREE
    open MISC

    fun isEmptyTree           EmptyTree               = true | isEmptyTree _           = false;

  (* This pretty printer is used to format the parsetree
     for error messages (Error near ...) and also for
     debugging.  There is a quite different pretty printer
     in VALUEOPS that is used to format values produced as
     a result of compiling and executing an expression or
     declaration. *) 

    fun typeFromTypeParse(
            ParseTypeConstruction{ args, name, location, foundConstructor = ref constr, ...}) =
        let
            val argTypes = List.map typeFromTypeParse args
        in
            TypeConstruction {name = name, constr = constr,
                              args = argTypes, locations = [DeclaredAt location]}
        end

    |   typeFromTypeParse(ParseTypeProduct{ fields, ...}) =
            mkProductType(List.map typeFromTypeParse fields)
    
    |   typeFromTypeParse(ParseTypeFunction{ argType, resultType, ...}) =
            mkFunctionType(typeFromTypeParse argType, typeFromTypeParse resultType)
    
    |   typeFromTypeParse(ParseTypeLabelled{ fields, frozen, ...}) =
        let
            fun makeField((name, _), t, _) = mkLabelEntry(name, typeFromTypeParse t)        
        in
            mkLabelled(sortLabels(List.map makeField fields), frozen)
        end

    |   typeFromTypeParse(ParseTypeId{ types=ParseTypeFreeVar{typeVar=ref(SOME(typ as FreeTypeVar _)), ...}, ...}) = typ

    |   typeFromTypeParse(ParseTypeId{ types=ParseTypeFreeVar _, ...}) = raise InternalError "typeFromTypeParse: Not set"

    |   typeFromTypeParse(ParseTypeId{ types=ParseTypeBoundVar{index=TVIndex index, ...}, ...}) = createBoundVar false index

    |   typeFromTypeParse(ParseTypeId{ types=ParseTypeError, ...}) =
            TypeVar(makeTv{value=NONE, level=Generalisable, equality=false})
   
    |   typeFromTypeParse(ParseTypeBad) = BadType

    fun displayTypeParse(types, depth, env) = display(SimpleInstance(typeFromTypeParse types), depth, env)

    local
        fun printTypeVar(ParseTypeBoundVar{name, ...}, _, _) = PrettyString name
        |   printTypeVar(ParseTypeFreeVar{name, ...}, _, _) = PrettyString name
        |   printTypeVar(ParseTypeError, _, _) = PrettyString "'error"
    in
        fun displayTypeVariables (vars : parseTypeVar list, depth : FixedInt.int) =
          printTypeVariables printTypeVar (vars, depth, varNameSequence ())
    end

    fun printList (doPrint: 'a*FixedInt.int->pretty) (c: 'a list, separator, depth: FixedInt.int): pretty list =
        if depth <= 0 then [PrettyString "..."]
        else
        case c of
            []      => []
        |   [v]     => [doPrint (v, depth)]
        |   (v::vs) =>
                PrettyBlock (0, false, [],
                    [
                        doPrint (v, depth),
                        PrettyBreak
                           (if separator = "," orelse separator = ";" orelse separator = "" then 0 else 1, 0),
                        PrettyString separator
                    ]
                    ) ::
                PrettyBreak (1, 0) ::
                printList doPrint (vs, separator, depth - 1)
  
   (* Generates a pretty-printed representation of a piece of tree. *)
    fun displayParsetree (c      : parsetree, (* The value to print. *)
                   depth  : FixedInt.int) : pretty = (* The number of levels to display. *)
    let
        val displayList = printList displayParsetree
        and displayListWithBpts = printList (fn ((c,_), depth) => displayParsetree(c, depth))
        
        (* type bindings and datatype bindings are used in several cases *)
        fun printTypeBind (TypeBind{name, typeVars, decType, ...}, depth) =
            PrettyBlock (3, true, [],
                displayTypeVariables (typeVars, depth) @
                (
                    PrettyString name ::
                    (* The type may be missing if this is a signature. *)
                    (case decType of
                        NONE => []
                    |   SOME t =>
                            [
                                PrettyBreak (1, 0),
                                PrettyString "=",
                                PrettyBreak (1, 0),
                                displayTypeParse (t, depth, emptyTypeEnv)
                            ]
                    )
                )
            )

        and printDatatypeBind (DatatypeBind{name, typeVars, constrs, ...}, depth) =
            PrettyBlock (3, true, [],
                displayTypeVariables (typeVars, depth) @
                    (
                        PrettyString name ::
                        PrettyBreak (1, 0) ::
                        PrettyString "=" ::
                        PrettyBreak (1, 0) ::
                        printList printConstructor (constrs, "|", depth - 1)
                    )
                )

        and printConstructor ({constrName, constrArg, ...}, depth) =
            PrettyBlock (2, false, [],
                PrettyString constrName ::
                (
                    case constrArg of
                        NONE => []
                    |   SOME argType =>
                        [
                            PrettyBreak (1, 0),
                            PrettyString "of",
                            PrettyBreak (1, 0),
                            displayTypeParse (argType, depth, emptyTypeEnv)
                        ]
                )
            )
        
    in
      if depth <= 0 (* elide further text. *)
        then PrettyString "..."

      else case c of
      
        Ident {name, ...} =>
          PrettyString name
          
      | Literal{literal, converter=Value{name=convName, ...}, ...} =>
        let
            val lit =
                if convName = "convString"
                then concat["\"" , literal, "\""]
                else if convName = "convChar"
                then concat["#\"" , literal, "\""]
                else literal 
        in
            PrettyString lit
        end

      | Applic { f, arg = TupleTree{fields=[left, right], ...}, isInfix = true, ...} =>
            (* Infixed application. *)
            PrettyBlock (0, false, [],
                [
                    displayParsetree (left, depth - 1),
                    PrettyBreak (1, 0),
                    displayParsetree (f, depth), (* Just an identifier. *)
                    PrettyBreak (1, 0),
                    displayParsetree (right, depth - 1)
                ]
            )

      | Applic {f, arg, ...} => (* Function application. *)
            PrettyBlock (0, false, [],
                [
                    displayParsetree (f, depth - 1),
                    PrettyBreak (1, 0),
                    displayParsetree (arg, depth - 1)
                ]
            )

      | Cond {test, thenpt, elsept, ...} => (* if..then..else.. *)
            PrettyBlock (0, false, [],
                [
                    PrettyString "if",
                    PrettyBreak (1, 0),
                    displayParsetree (test, depth - 1),
                    PrettyBreak (1, 0),
                    PrettyString "then",
                    PrettyBreak (1, 0),
                    displayParsetree (thenpt, depth - 1),
                    PrettyBreak (1, 0),
                    PrettyString "else",
                    PrettyBreak (1, 0),
                    displayParsetree (elsept, depth - 1)
                ]
            )

      | TupleTree{fields, ...} =>
            PrettyBlock (3, true, [],
                (
                    PrettyString "(" ::
                    PrettyBreak (0, 0) ::
                    displayList (fields, ",", depth - 1)
                ) @ [PrettyBreak (0, 0), PrettyString ")"]
            )

      | ValDeclaration {dec, ...} =>
        let
            (* We can't use printList here because we don't want an
               "and" after a "rec". *)
            fun printValBind ([], _) = []

              | printValBind (ValBind{dec, exp, isRecursive, ...} :: rest, depth) =
                    if depth <= 0
                    then [PrettyString "..."]
                    else
                    let
                        val isRec =
                            if isRecursive then [PrettyString "rec" , PrettyBreak (1, 0)] else []
                        val pValBind =
                            PrettyBlock (3, false, [],
                                [
                                    displayParsetree (dec, depth - 1),
                                    PrettyBreak (1, 0),
                                    PrettyString "=",
                                    PrettyBreak (1, 0),
                                    displayParsetree (exp, depth - 1)
                                ]
                            )
                    in
                        case rest of
                            [] => isRec @ [pValBind]
                        |   _ => PrettyBlock (0, false, [], isRec @ [pValBind, PrettyBreak(1, 0), PrettyString "and"]) ::
                                      PrettyBreak(1, 0) :: printValBind(rest, depth-1)
                    end
        in
            PrettyBlock (3, true, [],
                PrettyString "val" ::
                PrettyBreak (1, 0) ::
                (* TODO: Display the explicit type variables. *)
                (* displayTypeVariables (explicit, depth); *)
                printValBind (dec, depth - 1)
            )
        end

      | FunDeclaration {dec, ...} =>
          let
            fun printfvalbind (FValBind{clauses, ...}, depth) =
                PrettyBlock(3, true, [], printList printClause (clauses, "|", depth - 1))
            and printClause (FValClause{dec, exp, ...}, depth) =
                PrettyBlock (3, true, [],
                    [
                        printDec (dec, depth - 1),
                        PrettyBreak (1, 0),
                        PrettyString "=",
                        PrettyBreak (1, 0),
                        displayParsetree (exp, depth - 1)
                    ]
                )
            and printDec(
                    { ident = { name, ... }, isInfix=true, args=[TupleTree{fields=[left, right], ...}], constraint }, depth) =
                (* Single infixed application. *)
                PrettyBlock (0, false, [],
                    [
                        displayParsetree (left, depth - 1),
                        PrettyBreak (1, 0),
                        PrettyString name,
                        PrettyBreak (1, 0),
                        displayParsetree (right, depth - 1)
                    ] @ printConstraint (constraint, depth-1)
                )
            |   printDec(
                    { ident = { name, ... }, isInfix=true,
                      args=TupleTree{fields=[left, right], ...} :: args, constraint }, depth) =
                (* Infixed application followed by other arguments. *)
                PrettyBlock (0, false, [],
                    [
                        PrettyString "(",
                        PrettyBreak (0, 0),
                        displayParsetree (left, depth - 1),
                        PrettyBreak (1, 0),
                        PrettyString name,
                        PrettyBreak (1, 0),
                        displayParsetree (right, depth - 1),
                        PrettyBreak (0, 0),
                        PrettyString ")"
                    ] @ displayList (args, "", depth - 1) @ printConstraint(constraint, depth-2)
                )
            |   printDec({ ident = { name, ...}, args, constraint, ... }, depth) =
                (* Prefixed application. *)
                PrettyBlock (0, false, [],
                    [ PrettyString name, PrettyBreak (1, 0) ] @
                        displayList (args, "", depth - 1) @ printConstraint(constraint, depth-2)
                )
            and printConstraint(NONE, _) = []
            |   printConstraint(SOME given, depth) =
                [
                    PrettyBreak (1, 0),
                    PrettyString ":",
                    PrettyBreak (1, 0),
                    displayTypeParse (given, depth, emptyTypeEnv)
                ]
         in
            PrettyBlock (3, true, [],
                PrettyString "fun" ::
                PrettyBreak (1, 0) ::
                (* TODO: Display the explicit type variables. *)
                (* displayTypeVariables (explicit, depth); *)
                printList printfvalbind (dec, "and", depth - 1)
            )
        end

      | OpenDec {decs, ...} =>
        let
            fun printStrName ({name, ...}: structureIdentForm, _) = PrettyString name
        in
            PrettyBlock (3, true, [],
                PrettyString "open" ::
                PrettyBreak (1, 0) ::
                printList printStrName (decs, "", depth - 1)
            )
        end

      | List {elements, ...} =>
            PrettyBlock (3, true, [],
                PrettyString "[" ::
                PrettyBreak (0, 0) ::
                displayList (elements, ",", depth - 1) @
                [PrettyBreak (0, 0), PrettyString "]" ]
            )

      | Constraint {value, given, ...} =>
            PrettyBlock (3, false, [],
                [
                    displayParsetree (value, depth - 1),
                    PrettyBreak (1, 0),
                    PrettyString ":",
                    PrettyBreak (1, 0),
                    displayTypeParse (given, depth, emptyTypeEnv)
                ]
            )

      | Layered {var, pattern, ...} =>
            PrettyBlock (3, true, [],
                [
                    displayParsetree (var, depth - 1),
                    PrettyBreak (1, 0),
                    PrettyString "as",
                    PrettyBreak (1, 0),
                    displayParsetree (pattern, depth - 1)
                ]
            )

      | Fn {matches, ...} =>
            PrettyBlock (3, true, [],
                PrettyString "fn" ::
                PrettyBreak (1, 0) ::
                printList displayMatch (matches, "|", depth - 1)
            )

      | Unit _ =>
            PrettyString "()"

      | WildCard _ =>
            PrettyString "_"

      | Localdec {isLocal, decs, body, ...} =>
            PrettyBlock (3, false, [],
                PrettyString (if isLocal then "local" else "let") ::
                PrettyBreak (1, 0) ::
                displayListWithBpts (decs, ";", depth - 1) @
                [PrettyBreak (1, 0), PrettyString "in", PrettyBreak (1, 0)] @
                displayListWithBpts (body, ";", depth - 1) @
                [PrettyBreak (1, 0), PrettyString "end"]
            )

      | TypeDeclaration(ptl, _) =>
        let
            (* This is used both for type bindings and also in signatures.
               In a signature we may have "eqtype". *)
            val typeString =
                case ptl of
                    TypeBind {isEqtype=true, ...} :: _ => "eqtype"
                |   _ => "type"
        in
            PrettyBlock (3, true, [],
                PrettyString typeString ::
                PrettyBreak (1, 0) ::
                printList printTypeBind (ptl, "and", depth - 1)
            )
        end

      | AbsDatatypeDeclaration {typelist, withtypes, isAbsType=false, ...} =>
            PrettyBlock (3, true, [],
                PrettyString "datatype" ::
                PrettyBreak (1, 0) ::
                printList printDatatypeBind (typelist, "and", depth - 1) @
                (
                    if null withtypes then []
                    else
                        PrettyBreak (1, 0) ::
                        PrettyString "withtype" ::
                        PrettyBreak (1, 0) ::
                        printList printTypeBind (withtypes, "and", depth - 1)
                 )
             )

      | DatatypeReplication {newType, oldType, ...} =>
            PrettyBlock (3, true, [],
                [
                    PrettyString "datatype",
                    PrettyBreak (1, 0),
                    PrettyString newType,
                    PrettyBreak (1, 0),
                    PrettyString "=",
                    PrettyBreak (1, 0),
                    PrettyString "datatype",
                    PrettyBreak (1, 0),
                    PrettyString oldType
                ]
            )

       | AbsDatatypeDeclaration {typelist, withtypes, declist, isAbsType=true, ...} =>
            PrettyBlock (3, true, [],
                PrettyString "abstype" ::
                PrettyBreak (1, 0) ::
                printList printDatatypeBind (typelist, "and", depth - 1) @
                [ PrettyBreak (1, 0) ] @
                (
                    if null withtypes then []
                    else
                        PrettyString "withtype" ::
                        PrettyBreak (1, 0) ::
                        printList printTypeBind (withtypes, "and", depth - 1) @
                        [PrettyBreak (1, 0)]
                ) @
                [
                    PrettyString "with",
                    PrettyBreak (1, 0),
                    PrettyBlock (3, true, [],
                        displayListWithBpts(declist, ";", depth - 1))
                ]
            )
                

      | ExpSeq(ptl, _) =>
            PrettyBlock (3, true, [],
                PrettyString "(" ::
                PrettyBreak (0, 0) ::
                displayListWithBpts (ptl, ";", depth - 1) @
                [ PrettyBreak (0, 0), PrettyString ")"]
            )

      | Directive {fix, tlist, ...} =>
        let
            val status =
                case fix of
                    Nonfix => PrettyString "nonfix"
                |   Infix prec =>
                        PrettyBlock(0, false, [],
                            [ PrettyString "infix", PrettyBreak (1, 0), PrettyString (Int.toString prec) ])
                |   InfixR prec =>
                        PrettyBlock(0, false, [],
                            [ PrettyString "infixr", PrettyBreak (1, 0), PrettyString (Int.toString prec) ])
        in
            PrettyBlock (3, true, [],
                status ::
                PrettyBreak (1, 0) ::
                printList (fn (name, _) => PrettyString name) (tlist, "", depth - 1)
            )
        end

      | ExDeclaration(pt, _) =>
          let
            fun printExBind (ExBind {name, ofType, previous, ...}, depth) =
                PrettyBlock (0, false, [],
                    PrettyString name ::
                    (case ofType of NONE => []
                        | SOME typeof =>
                        [
                            PrettyBreak (1, 0),
                            PrettyString "of",
                            PrettyBreak (1, 0),
                            displayTypeParse (typeof, depth, emptyTypeEnv)
                        ]
                    ) @
                    (if isEmptyTree previous then []
                    else 
                    [
                        PrettyBreak (1, 0),
                        PrettyString "=",
                        PrettyBreak (1, 0),
                        displayParsetree (previous, depth - 1)
                    ])
                )
         in
            PrettyBlock (3, true, [],
                PrettyString "exception" ::
                PrettyBreak (1, 0) ::
                printList printExBind (pt, "and", depth - 1)
            )
        end

      | Raise (pt, _) =>
            PrettyBlock (0, false, [],
                [
                    PrettyString "raise",
                    PrettyBreak (1, 0),
                    displayParsetree (pt, depth - 1)
                ]
            )

      | HandleTree {exp, hrules, ...} =>
            PrettyBlock (0, false, [],
                [
                    displayParsetree (exp, depth - 1),
                    PrettyBreak (1, 0),
                    PrettyBlock (3, true, [],
                        PrettyString "handle" ::
                        PrettyBreak (1, 0) ::
                        printList displayMatch (hrules, "|", depth - 1)
                    )
                ]
            )

      | While {test, body, ...} =>
            PrettyBlock (0, false, [],
                [
                    PrettyString "while",
                    PrettyBreak (1, 0),
                    displayParsetree (test, depth - 1),
                    PrettyBreak (1, 0),
                    PrettyString "do",
                    PrettyBreak (1, 0),
                    displayParsetree (body, depth - 1)
                ]
            )

      | Case {test, match, ...} =>
            PrettyBlock (3, true, [],
                PrettyBlock (0, false, [],
                    [
                        PrettyString "case",
                        PrettyBreak (1, 0),
                        displayParsetree (test, depth - 1),
                        PrettyBreak (1, 0),
                        PrettyString "of"
                    ]
                ) ::
                PrettyBreak (1, 0) ::
                printList displayMatch (match, "|", depth - 1)
            )

      | Andalso {first, second, ...} =>
            PrettyBlock (3, true, [],
                [
                    displayParsetree (first, depth - 1),
                    PrettyBreak (1, 0),
                    PrettyString "andalso",
                    PrettyBreak (1, 0),
                    displayParsetree (second, depth - 1)
                ]
            )

      | Orelse {first, second, ...} =>
            PrettyBlock (3, true, [],
                [
                    displayParsetree (first, depth - 1),
                    PrettyBreak (1, 0),
                    PrettyString "orelse",
                    PrettyBreak (1, 0),
                    displayParsetree (second, depth - 1)
                ]
            )

      | Labelled {recList, frozen, ...} =>
        let
            fun displayRecList (c, depth): pretty list =
            if depth <= 0 then [PrettyString "..."]
            else
              case c of
                []      => []
              | [{name, valOrPat, ...}]     =>
                    [
                        PrettyBlock (0, false, [],
                            [
                                PrettyString (name ^ " ="),
                                PrettyBreak (1, 0),
                                displayParsetree (valOrPat, depth - 1)
                            ]
                        )
                    ]
                | ({name, valOrPat, ...}::vs) =>
                    PrettyBlock (0, false, [],
                        [
                             PrettyBlock (0, false, [],
                                [
                                    PrettyString (name ^ " ="),
                                    PrettyBreak (1, 0),
                                    displayParsetree (valOrPat, depth - 1)
                                ]
                            ),
                            PrettyBreak (0, 0),
                            PrettyString ","
                        ]
                    ) ::
                    PrettyBreak (1, 0) ::
                    displayRecList (vs, depth - 1)
             (* end displayRecList *)
        in
            PrettyBlock (2, false, [],
                PrettyString "{" ::
                displayRecList (recList, depth - 1) @
                (if frozen then [PrettyString "}"]
                else [PrettyString (if null recList then "...}" else ", ...}")])
            )
        end

      | Selector {name, ...} =>
          PrettyString ("#" ^ name)

      | EmptyTree =>
         PrettyString "<Empty>"
         
      | Parenthesised(p, _) =>
            PrettyBlock(0, false, [],
                [
                    PrettyString "(",
                    PrettyBreak (0, 0),
                    displayParsetree (p, depth),
                    PrettyBreak (0, 0),
                    PrettyString ")"
                ]
            )
    
    end (* displayParsetree *)

    and displayMatch(MatchTree {vars, exp, ...}, depth) =
        PrettyBlock (0, false, [],
            [
                displayParsetree (vars, depth - 1),
                PrettyBreak (1, 0),
                PrettyString "=>",
                PrettyBreak (1, 0),
                displayParsetree (exp, depth - 1)
            ]
        )

    (* Error message routine.  Used in both pass 2 and pass 3. *)
    fun errorNear (lex, hard, near, line, message) =
    let
        val errorDepth = errorDepth lex
    in
    (* Puts out an error message and then prints the piece of tree. *)
        reportError lex
        {
            hard = hard,
            location = line,
            message = PrettyBlock (0, false, [], [PrettyString message]),
            context = SOME(displayParsetree (near, errorDepth))
        }
     end

    (* Types that can be shared. *)
    structure Sharing =
    struct
        type lexan = lexan
        and  parsetree = parsetree
        and  matchtree = matchtree
        and  pretty = pretty
        and  typeParsetree = typeParsetree
        and  types = types
        and  parseTypeVar = parseTypeVar
        and  typeId = typeId
        and  structVals = structVals
        and  typeConstrSet = typeConstrSet
    end

end;

