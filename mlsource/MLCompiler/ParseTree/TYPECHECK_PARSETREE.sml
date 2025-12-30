(*
    Copyright (c) 2013-2015, 2025 David C.J. Matthews

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


functor TYPECHECK_PARSETREE (

structure BASEPARSETREE : BaseParseTreeSig
structure PRINTTREE: PrintParsetreeSig
structure EXPORTTREE: ExportParsetreeSig
structure LEX : LEXSIG
structure CODETREE : CODETREE
structure STRUCTVALS : STRUCTVALSIG;
structure TYPETREE : TYPETREESIG
structure VALUEOPS : VALUEOPSSIG;
structure PRETTY : PRETTY
structure COPIER: COPIERSIG
structure DATATYPEREP: DATATYPEREPSIG

structure UTILITIES :
sig
    type lexan;
    type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }

    val noDuplicates: (string * 'a * 'a -> unit) -> 
                       { apply: (string * 'a -> unit) -> unit,
                         enter:  string * 'a -> unit,
                         lookup: string -> 'a option};

    val searchList: unit -> { apply: (string * 'a -> unit) -> unit,
                            enter:  string * 'a -> unit,
                            lookup: string -> 'a option };

    val checkForDots:  string * lexan * location -> unit;

    val splitString: string -> { first:string,second:string }

    structure Sharing:
    sig
        type lexan = lexan
    end
end

structure MISC :
sig  
    exception InternalError of string; (* compiler error *)

    val quickSort : ('a -> 'a -> bool) -> 'a list -> 'a list;

    val lookupDefault :  ('a -> 'b option) -> ('a -> 'b option) -> 'a -> 'b option
end

structure DEBUG: DEBUG

sharing LEX.Sharing = TYPETREE.Sharing = STRUCTVALS.Sharing = COPIER.Sharing
       = VALUEOPS.Sharing = UTILITIES.Sharing = PRETTY.Sharing
       = CODETREE.Sharing = DATATYPEREP.Sharing
       = BASEPARSETREE.Sharing = PRINTTREE.Sharing = EXPORTTREE.Sharing

): TypeCheckParsetreeSig =
   
struct

    local
        fun allValLongNamesFromStruct
            (STRUCTVALS.Struct {name = structName, signat, ...} : STRUCTVALS.structVals) : string list =
        let
            val longNames = ref []
            val tsvEnv : COPIER.tsvEnv = {
                enterType = fn _ => (),
                enterStruct = fn (_, structVal) =>
                    (longNames := List.map (fn s => structName ^ "." ^ s) (allValLongNamesFromStruct structVal) @ !longNames),
                enterVal = fn (name, _) =>
                    (longNames := structName ^ "." ^ name :: !longNames)
            }
            val () = COPIER.openSignature (signat, tsvEnv, structName ^ ".");
        in
            !longNames
        end
    in
        fun allValLongNamesWithPrefix prefix env =
        let
            fun longNamesFromStruct name =
                (case #lookupStruct env name of
                  NONE => []
                | SOME structVal => allValLongNamesFromStruct structVal)
        in
            List.filter (fn s => String.isPrefix prefix s) (#allValNames env () @ List.concat (List.map longNamesFromStruct (#allStructNames env ())))
        end
    end

    open MISC
    open LEX
    open CODETREE
    open STRUCTVALS
    open TYPETREE
    open VALUEOPS
    open UTILITIES
    open PRETTY
    open DATATYPEREP
  
    open BASEPARSETREE
    open PRINTTREE
    open EXPORTTREE


    val badInstance = SimpleInstance BadType
    and boolInstance = SimpleInstance boolType
    and boolStarBool = SimpleInstance(mkProductType[boolType, boolType])
    and unitInstance = SimpleInstance unitType
    (* Associates type constructors from the environment with type identifiers
       (NOT type variables) *)
    fun assignTypes (tp : typeParsetree,  lookupType : string * location -> typeConstrSet, lex : lexan) =
    let

        fun tcArity(TypeConstrs {identifier=TypeId{idKind=TypeFn{arity, ...},...}, ...}) = arity
        |   tcArity(TypeConstrs {identifier=TypeId{idKind=Bound{arity, ...},...}, ...}) = arity
        |   tcArity(TypeConstrs {identifier=TypeId{idKind=Free{arity, ...},...}, ...}) = arity

        fun typeFromTypeParse(ParseTypeConstruction{ args, name, location, foundConstructor, ...}) =
            let
                (* Assign constructor, then the parameters. *)
                val TypeConstrSet(constructor, _) = lookupType (name, location)
                val () =
                    (* Check that it has the correct arity. *)
                    case constructor of
                        TypeConstrs{name=tcName, ...} =>
                        let
                            val arity = tcArity constructor
                            val num = length args
                        in
                            if arity <> num andalso not (isUndefinedTypeConstr constructor)
                            then (* Give an error message *)
                            errorMessage (lex, location,
                                String.concat["Type constructor (", tcName,
                                    ") requires ", Int.toString arity, " type(s) not ",
                                    Int.toString num])
                            else foundConstructor := constructor
                        end
                val argTypes = List.map typeFromTypeParse args
            in
                if isUndefinedTypeConstr constructor
                then BadType
                else TypeConstruction {name = name, constr = constructor,
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

        |   typeFromTypeParse(ParseTypeId{ types=ParseTypeFreeVar{typeVar=ref NONE, ...}, ...}) = raise InternalError "assignTypes: free type not set"
        |   typeFromTypeParse(ParseTypeId{ types=ParseTypeFreeVar{typeVar=ref(SOME typ), ...}, ...}) = typ
        |   typeFromTypeParse(ParseTypeId{ types=ParseTypeBoundVar{index=TVIndex index, ...}, ...}) = createBoundVar false index
        |   typeFromTypeParse(ParseTypeId{ types=ParseTypeError, ...}) =
                TypeVar(makeTv{value=NONE, level=Generalisable, equality=false})

        |   typeFromTypeParse(ParseTypeBad) = BadType
    in
        typeFromTypeParse tp
    end

    fun getBoundTypeVar(ParseTypeBoundVar{index=TVIndex i, ...}) = createBoundVar false i
    |   getBoundTypeVar ParseTypeError = BadType
    |   getBoundTypeVar(ParseTypeFreeVar _) = raise InternalError "getBoundTypeVar: free"

    (* Set a free type variable to either an outer one or to the current level. *)
    fun setParseTypeVar(ParseTypeFreeVar{name, typeVar, equality, ...}, other, level) =
        let
            (* Should not have been previously set. *)
            val _ = case !typeVar of NONE => () | _ => raise InternalError "setParseTypeVar: already set"
            val toSet =
                case other of
                    SOME(ParseTypeFreeVar{typeVar=ref (SOME(otherTv as FreeTypeVar _)), ...}) => otherTv
                |   SOME _ => raise InternalError "setParseTypeVar: other not free"
                |   NONE => FreeTypeVar {name=name, equality=equality, level=NotGeneralisable level, uid=makeUniqueId()}
        in
            typeVar := SOME toSet
        end

    |   setParseTypeVar _ = raise InternalError "setParseTypeVar: not free"

    fun unitTree location = ParseTypeLabelled{ fields = [], frozen = true, location = location }

   (* Second pass of ML parse tree. *)
   
    (* This is pass 2 of the compiler. It walks over the parse tree
       generated by pass 1 and looks up identifiers to match them to
       declarations. It performs the type checking. "makeTypeId" is used
       to construct unique identifiers for types depending on the context
       (i.e. in a signature, structure or functor). *)
    fun pass2 (v, makeTypeId, env, lex, sigTypeIdMap) =
    let
        (* Returns a function which can be passed to unify or apply to
           print a bit of context info. *)
        fun foundNear v () =
        let
            val errorDepth = errorDepth lex
        in
            displayParsetree (v, errorDepth)
        end

      (* A simpler error message routine for lookup_... where the message
         does not involve pretty-printing anything. *)
      fun giveError (v, lex, line)  =
        fn message => errorNear (lex, true, v, line, message);

      fun checkForBuiltIn (name, v, lex, lineno, isConstr) =
      (* ML97 does not allow the standard constructors to be rebound and does
         not allow "it" to become a constructor. *)
         if name = "true" orelse name = "false" orelse name = "nil"
         orelse name = "::" orelse name = "ref" orelse (isConstr andalso name = "it")
         then errorNear(lex, true, v, lineno,
                     "Rebinding or specifying \"" ^ name ^ "\" is illegal")
         else ()

        (* Turn a result from unifyTypes into a pretty structure so that it
           can be included in a message. *)
        fun unifyErrorReport(lex, typeEnv) = unifyTypesErrorReport(lex, typeEnv, typeEnv, "unify")

        (* Error message for incompatible types.  Displays both expressions and their types. *)
        fun typeMismatch (title, left, right, detail, lex : lexan, location, moreInfo) =
        let
            val message =
                PrettyBlock(3, true, [],
                    [
                        PrettyString title,
                        PrettyBreak(1, 0), left,
                        PrettyBreak(1, 0), right,
                        PrettyBreak(1, 0),
                        PrettyBlock(0, false, [],
                            [PrettyString "Reason:", PrettyBreak(1, 3), detail])
                    ])
        in
            reportError lex
            {
                location = location,
                hard = true,
                message = message,
                context = SOME (moreInfo ())
            }
        end;

        (* Error message for single expressions with the wrong type. e.g. "if" not followed
           by a "bool". *)
        fun typeWrong (title, value, detail, lex : lexan, location, moreInfo) =
        let
            val message =
                PrettyBlock(3, true, [],
                    [
                        PrettyString title,
                        PrettyBreak(1, 0), value,
                        PrettyBreak(1, 0),
                        PrettyBlock(0, false, [],
                            [ PrettyString "Reason:", PrettyBreak(1, 3), detail])
                    ])
        in
            reportError lex
            {
                location = location,
                hard = true,
                message = message,
                context = SOME (moreInfo ())
            }
        end;

        (* Display a value and its type as part of an error message. *)
        fun valTypeMessage (lex, typeEnv) (title, value, valType) =
        let
            val errorDepth = errorDepth lex
        in
            PrettyBlock(3, false, [],
                [
                    PrettyString title,
                    PrettyBreak(1, 0),
                    displayParsetree (value, errorDepth),
                    PrettyBreak(1, 0),
                    PrettyString ":",
                    PrettyBreak(1, 0),
                    display(valType, 10000 (* All of it *), typeEnv)
                ])
        end

        fun matchTypeMessage (lex, typeEnv) (title, match, valType) =
        let
            val errorDepth = errorDepth lex
        in
            PrettyBlock(3, false, [],
                [
                    PrettyString title,
                    PrettyBreak(1, 0),
                    displayMatch (match, errorDepth),
                    PrettyBreak(1, 0),
                    PrettyString ":",
                    PrettyBreak(1, 0),
                    display(valType, 10000 (* All of it *), typeEnv)
                ])
        end

        (* Old error message and unification functions.  These will eventually be
           removed.  *)
        fun matchError 
            (error: matchResult, lex : lexan, location : LEX.location, moreInfo : unit -> pretty, typeEnv) : unit =
            reportError lex
            {
                location = location,
                hard = true,
                message = unifyErrorReport(lex, typeEnv) error,
                context = SOME (moreInfo ())
           }

        fun unify (alpha, beta, lex, location, moreInfo, typeEnv) =
            case unifyTypes (alpha, beta) of
                NONE => ()
            |   SOME error =>
                    matchError (error, lex, location, moreInfo, typeEnv)

        fun apply (f, arg: types, lex, location, moreInfo, typeEnv) =
            let  (* Make arg->'a, and unify with the function. *)
                val resType  = mkTypeVar (Generalisable, false)
                val fType    = mkFunctionType (arg, resType)
  
                (* This may involve more than just assigning the type to "ef". *)
                val () = unify (f, SimpleInstance fType, lex, location, moreInfo, typeEnv);
            in
                SimpleInstance resType (* The result is the type variable unified to the result. *)
            end

        (* These cases currently use the "apply" or "unify" and may need to be improved in
           order to produce better messages.
           apply:
              Literals.  The conversion functions are applied to the string literal.  In effect this produces the set
              of overloadings of the literal.  This should never produce an error message.
              Constructors in patterns to their args.
              "case": the patterns are "applied" to the value to be tested.

           unify:
              Layered patterns, to set the variable. Also checks the pattern against any explicit type.
              Handlers: the handling patterns are unified against a function from exn -> the result type of the
              expression being handled.
         *)

    fun stringsOfSearchList { apply: (string * 'a -> unit) -> unit, ...} () =
    let
        val v = ref []
        val () = apply (fn (s, _) => v := s :: !v)
    in
        !v
    end

    fun assignValues (level, letDepth, env, near, v): instanceType  =
    let
        val typeEnv =
        {
            lookupType = fn s => case #lookupType env s of NONE => NONE | SOME t => SOME(t, NONE),
            lookupStruct = fn s => case #lookupStruct env s of NONE => NONE | SOME t => SOME(t, NONE)
        }
         (* Process each item of the sequence and return the type of the
            last item. A default item is returned if the list is empty. *)
        fun assignSeq(env, depth, l, isExp) =
        let
            fun applyList last []       = last
            |   applyList _ ((h, _) :: t) =
                let
                    val expT: instanceType = assignValues(level, depth, env, v, h)
                    val _ =
                        if isExp andalso not (null t)
                        then (* It's not the last value and we're going to discard it *)
                        case checkDiscard(expT, lex) of
                            NONE => ()
                        |   SOME s => errorNear (lex, false, h, getLocation h, s)
                        else ()
                in
                    applyList expT t
                end
        in
            applyList badInstance l
        end

        fun tcIdentifier (TypeConstrs {identifier,...}) = identifier
        fun tcLocations  (TypeConstrs {locations, ...}) = locations

        val tcEquality = isEquality o tcIdentifier;
        fun tcSetEquality(tc, eq) = setEquality(tcIdentifier tc, eq)

        fun tsConstr(TypeConstrSet(ts, _)) = ts

        fun isConstructor (Value{class=Constructor _, ...}) = true
        |   isConstructor (Value{class=Exception _, ...})     = true
        |   isConstructor _                                  = false;

        (* Variables, constructors and fn are non-expansive.
           [] is a derived form of "nil" so must be included.
           Integer and string constants are also constructors.
           Constrained versions are also non-expansive. *)
        fun nonExpansive (Fn _)   = true
        |   nonExpansive (Ident _) = true
        |   nonExpansive (List{elements = [], ...}) = true
        |   nonExpansive (List{elements, ...}) =
                List.foldl (fn (v, a) => a andalso nonExpansive v) true elements
        |   nonExpansive (Constraint {value, ...}) = nonExpansive value
        |   nonExpansive (Literal _) = true
        |   nonExpansive (Unit _) = true
        |   nonExpansive (TupleTree{fields, ...}) = 
                List.foldl (fn (v, a) => a andalso nonExpansive v) true fields
        |   nonExpansive (Labelled{recList, ...}) =
                List.foldl (fn ({valOrPat, ...}, a) => a andalso nonExpansive valOrPat)
                        true recList (* Every element must be non-expansive *)
        |   nonExpansive (Applic{f, arg, ...}) =
                isNonRefConstructor f andalso nonExpansive arg
        |   nonExpansive (Selector _) = true (* derived from fn {..} => ...*)
        |   nonExpansive (Parenthesised(p, _)) = nonExpansive p
        |   nonExpansive _       = false

        (* An application is non-expansive only if it is a, possibly
           constrained, constructor which is not ref. *)
        and isNonRefConstructor (Ident {value=ref(v as Value{typeOf=ValueType(typeOf, _), ...}), ...}) =
                isConstructor v andalso not (isRefFunction typeOf)
        | isNonRefConstructor (Constraint {value, ...}) =
                isNonRefConstructor value
        | isNonRefConstructor (Parenthesised(p, _)) =
                isNonRefConstructor p
        | isNonRefConstructor _ = false

        (* Applies "assignValues" or "processPattern" to every element of a list and unifies the
           types. Returns a type variable if the list is empty.
           This is used for lists, function values (fn .. => ...),
           handlers and case expressions. *)
        fun assignList _ _ [] = SimpleInstance(mkTypeVar (Generalisable, false))
        |   assignList (processValue: 'a->instanceType, _, _) _ [single] = processValue single

        |   assignList (processValue: 'a->instanceType, displayValue, typeMsg)
                            (errorMsg, itemName, separator, location, near) (tlist as hd :: tl) =
            let
                val firstType = processValue hd

                fun printList (doPrint: 'a*FixedInt.int->pretty) (c: 'a list, separator, depth): pretty list =
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

                fun applyList(ty, _, []) = ty
                |   applyList(ty, n, h::t) =
                    let
                        val typ = processValue h
                    in
                        case unifyTypes (ty, typ) of
                            NONE => applyList(ty, n+1, t)
                        |   SOME report =>
                            let
                                (* We have a type error but we don't know which is correct.
                                   The previous items must have produced a consistent type
                                   otherwise we'd already have reported an error but we
                                   can't identify exactly where the error occurred. *)
                                val errorDepth = errorDepth lex
                                val previousValsAndType =
                                    PrettyBlock(3, false, [],
                                        [
                                            PrettyString (
                                                if n = 1 then itemName ^ " 1:"
                                                else itemName ^ "s 1-" ^ Int.toString n ^ ":"),
                                            PrettyBreak(1, 0),
                                            PrettyBlock(0, false, [],
                                                printList (*displayParsetree*)displayValue (List.take(tlist, n),
                                                separator, errorDepth)),
                                            PrettyBreak(1, 0),
                                            PrettyString ":",
                                            PrettyBreak(1, 0),
                                            display(ty, 10000 (* All of it *), typeEnv)
                                        ])
                            in
                                typeMismatch(errorMsg,
                                    previousValsAndType,
                                    (*valTypeMessage*)typeMsg(lex, typeEnv) (concat[itemName, " ", Int.toString(n+1), ":"], h, typ),
                                    unifyErrorReport(lex, typeEnv) report, lex, location, foundNear near);
                                (* Continue with "bad" which suppresses further error messages
                                   and return "bad" as the result. *)
                                applyList(badInstance, n+1, t)
                            end
                    end
            in
                applyList(firstType, 1, tl)
            end

        fun ptAssignTypes (t, near) =
            assignTypes
                (t,
                fn (s, line) => 
                    lookupTyp 
                        ({lookupType = #lookupType env, lookupStruct = #lookupStruct env},
                        s, giveError (near, lex, line)),
                lex);

        (* Makes a type for an instance of an identifier. *)
        fun instanceType(Value{typeOf=ValueType valType, ...}) = generalise valType
            (* The types of constructors and variables are copied 
               to create new instances of type variables. *)

        fun makeTypeConstructor (name, uid, locations) = TypeConstrs { name = name, identifier = uid, locations = locations }

        fun processPattern(pat as Ident {name, value, expType, location, possible, ...}, enterResult, level, notConst, mkVar, isRec) =
        (* Variable or nullary constructor. *)
            let
                (* Look up the name. If it is a constructor then use it,
                    otherwise return `undefined'. If it is a qualified name,
                    i.e. it contains a full-stop, we assume it is a constructor
                    and give an error message if it does not exist. *)
                (* In ML 97 recursive declarations such as val rec f = ...
                     override constructor status.  If this is a recursive declaration
                     we don't check for constructor status. *)
                val names   = splitString name;
                val nameVal as Value{class, ...} =
                    if isRec
                    then undefinedValue
                    else if #first names = ""
                    then (* Not qualified - may be a variable. *)
                        getOpt (#lookupVal env name, undefinedValue) 
          
                    else (* Qualified - cannot be a variable. *)
                        lookupValue
                            ("Constructor",
                            {lookupVal= #lookupVal env, lookupStruct= #lookupStruct env},
                            name,
                            giveError (pat, lex, location))

                (* Remember the possible names here. *)
                val () = possible := (fn () => allValLongNamesWithPrefix name env)

                val instanceType = 
                    (* If the result is a constructor use it. *)
                    if (case class of Exception _ => true | Constructor _ => true | _ => false)
                    then
                    (
                        if notConst
                        then
                        (
                            errorNear (lex, true, pat, location,
                                    "Identifier before `as' must not be a constructor.");
                            badInstance
                        )
                        else
                        (* Must be a nullary constructor otherwise it should
                           have been applied to something. *)
                        let
                            (* set this value in the record *)
                            val () = value := nameVal
                            val isNullary =
                                case class of
                                    Constructor{nullary, ...} => nullary
                                |   Exception{nullary, ...} => nullary
                                |   _ => true (* Should not happen *)
                        in
                            if isNullary then #1 (instanceType nameVal)
                            else
                            (
                                errorNear (lex, true, pat, location,
                                            "Constructor must be applied to an argument pattern.");
                                badInstance
                            )
                        end
                    )
                    (* If undefined or another variable, construct a new variable. *)
                    else
                    let
                        val props = [DeclaredAt location, SequenceNo (newBindingId lex)]
                        val var as Value{typeOf=ValueType(valTypeOf, _), ...} =
                            mkVar(name, ValueType(mkTypeVar (NotGeneralisable level, false), []), props)
                    in
                        checkForDots (name, lex, location); (* Must not be qualified *)
                        (* Must not be "true", "false" etc. *)
                        checkForBuiltIn (name, v, lex, location, false);
                        enterResult (name, var);
                        value := var;
                        SimpleInstance valTypeOf (* and return its type *)
                    end;
            in
                expType := (instanceType, []); (* Record the instance type.*)
                instanceType: instanceType
            end

        |   processPattern(pat as Literal{converter=Value{typeOf=ValueType valTypeOf, ...}, expType, location, ...}, _, _, _, _, _) =
            let
                (* Find out the overloadings on this converter and
                   construct an instance of it.  The converters are
                   all functions from string to the result type. *)
                val (instanceType, _) = generalise valTypeOf
                (* Apply the converter to string to get the type of the
                   literal. *)
                val instance =
                    apply(instanceType, stringType, lex, location, foundNear pat, typeEnv)
            in
                expType := instance; (* Record the instance type.*)
                instance
            end

        |   processPattern(pat as Applic {f = con, arg, location, expType, ...}, enterResult, level, notConst, mkVar, isRec) =
            let
                (* Apply the function to the argument and return the result. *)
                (* Function must be a constructor. *)
                val conType = 
                    case con of
                        Ident {name, value, location, expType, possible, ...} =>
                        let (* Look up the value and return the type. *)
                        
                            (* Remember the possible names here. *)
                            val () = possible := (fn () => allValLongNamesWithPrefix name env)

                            val constrVal =
                                lookupValue 
                                    ("Constructor",
                                    {lookupVal   = #lookupVal env, lookupStruct = #lookupStruct env},
                                    name, giveError (pat, lex, location));
                        in
                            if isConstructor constrVal
                            then
                            let
                                val instance as (cType, _) = instanceType constrVal
                            in
                                value := constrVal;
                                expType := instance; (* Record the instance type.*)
                                cType
                            end
                            else (* Undeclared or a variable. *)
                            (
                                if isUndefinedValue constrVal then ()
                                else errorNear (lex, true, pat, location, name ^ " is not a constructor");
                                badInstance
                            )
                        end
    
                    |   _ => (* con is not an Ident *)
                        (
                            errorNear (lex, true, pat, location,
                                "Constructor in a pattern was not an identifier");
                            badInstance
                        )

                val patType = processPattern(arg, enterResult, level, notConst, mkVar, isRec);
                (* Apply to the pattern type. *)
                val resultType = apply (conType, instanceToType patType, lex, location, foundNear pat, typeEnv)
            in
                expType := resultType; (* Record the instance type.*)
                resultType
            end (* Applic *)

        |   processPattern(TupleTree{fields, expType, ...}, enterResult, level, notConst, mkVar, isRec) =
            let
                val mapProcessPattern: parsetree list -> types list =
                    map (fn x => instanceToType(processPattern(x, enterResult, level, notConst, mkVar, isRec)));
                (* Construct the type obtained by mapping "processPattern"
                   onto each element of the tuple. *)
                val tupleType = mkProductType (mapProcessPattern fields)
            in
                expType := tupleType;
                SimpleInstance tupleType
            end

        |   processPattern(Labelled {recList, frozen, expType, ...}, enterResult, level, notConst, mkVar, isRec) =
            let (* Process each item in the list. *)

                fun mapLabels [] = []
                |   mapLabels ({name, valOrPat, expType, ...}::T) =
                    (* Type is a label entry with the label name
                       and the type of the pattern. *)
                    let
                        val ty = processPattern(valOrPat, enterResult, level, notConst, mkVar, isRec)
                    in
                        expType := instanceToType ty;
                        mkLabelEntry(name, instanceToType ty) :: mapLabels T
                    end;
                val patType = mkLabelled (sortLabels(mapLabels recList), frozen)
            in
                expType := patType;
                SimpleInstance patType
            end

        |   processPattern((aList as List{elements, location}), enterResult, level, notConst, mkVar, isRec) =
            let
                (* Applies "processPattern" to every element of a list and
                   unifies the types. Returns a type variable if the list
                   is empty *)
                fun processElement elem =
                    processPattern(elem, enterResult, level, notConst, mkVar, isRec)
                val elementType =
                    instanceToType(assignList (processElement, displayParsetree, valTypeMessage)
                        ("Elements in a list have different types.", "Item", ",", location, aList) elements)
            in
                if isBadType elementType
                then badInstance
                else SimpleInstance(mkTypeConstruction ("list", tsConstr listConstr, [elementType], [DeclaredAt inBasis]))
            end

        |   processPattern(aConstraint as Constraint {value, given, location}, enterResult, level, notConst, mkVar, isRec) =
            let
                val valType  = processPattern(value, enterResult, level, notConst, mkVar, isRec);
                val theType = SimpleInstance(ptAssignTypes(given, aConstraint))
            in
                (* These must be unifiable. *)
                case unifyTypes(valType, theType) of
                    NONE => () (* OK. *)
                |   SOME report =>
                        typeMismatch("Type mismatch in type constraint.",
                            valTypeMessage (lex, typeEnv) ("Value:", value, valType),
                            PrettyBlock(0, false, [],
                                [
                                    PrettyString "Constraint:",
                                    PrettyBreak(1, 0),
                                    display(theType, 10000 (* All of it *), typeEnv)
                                ]),
                            unifyErrorReport (lex, typeEnv) report,
                            lex, location, foundNear aConstraint);
                theType
            end

        |   processPattern(pat as Layered {var, pattern, location}, enterResult, level, notConst, mkVar, isRec) =
            let
                (* Unify the variable and the pattern - At this stage that simply
                 involves assigning the type of the pattern to the variable,
                 but it may result in more unification when the variable is
                 used *)
          
                (* The "variable" must be either id or id: ty but we have to
                 check that the id is not a constructor. *)
                val varType = processPattern(var,     enterResult, level, true, mkVar, isRec);
                val patType = processPattern(pattern, enterResult, level, notConst, mkVar, isRec)
                val () = unify (varType, patType, lex, location, foundNear pat, typeEnv);
            in
                varType
            end

        |   processPattern(Unit _, _, _, _, _, _) = unitInstance

        |   processPattern(WildCard _, _, _, _, _, _) = SimpleInstance (mkTypeVar (Generalisable, false))

        |   processPattern(Parenthesised(p, _), enterResult, level, notConst, mkVar, isRec) =
                processPattern(p, enterResult, level, notConst, mkVar, isRec)

        |   processPattern(_, _, _, _, _, _) = badInstance (* not a legal pattern *)


        and assValues near (Ident {name, value, expType, location, possible, ...}) =
            let
                val expValue =
                    lookupValue 
                        ("Value or constructor",
                            {lookupVal = #lookupVal env, lookupStruct = #lookupStruct env},
                            name, giveError (near, lex, location));
                (* Set the value and type found. *)
                val instanceTypeAndVars as (instanceType, _)  = instanceType expValue
            in
                (* Include this reference in the list of local references. *)
                case expValue of
                    Value { references=SOME{localRef, ...}, ...} =>
                        localRef := location :: ! localRef
                |   _ => ();
                expType := instanceTypeAndVars;
                value  := expValue;
                possible := (fn () => allValLongNamesWithPrefix name env);
                instanceType (* Result is the instance type. *)
            end

        |   assValues near (Literal{converter=Value{typeOf=ValueType valTypeOf, ...}, expType, location, ...}) =
            let
                (* Find out the overloadings on this converter and
                   construct an instance of it.  The converters are
                   all functions from string to the result type. *)
                val (instanceType, _) = generalise valTypeOf
                val instance =
                    apply(instanceType, stringType, lex, location, foundNear near, typeEnv)
            in
                expType := instance;
                instance
            end

        |   assValues near (Applic {f, arg, location, expType, ...}) =
            let
                (* Apply the function to the argument and return the result. *)
                val funType = assValues near f
                val argType = assValues near arg
                val funResType = mkTypeVar (Generalisable, false)
                val funArgType = mkTypeVar (Generalisable, false)
                val fType = mkFunctionType (funArgType, funResType)
                (* Test to see if we have a function. *)
            in
                case unifyTypes (SimpleInstance fType, funType) of
                    SOME _ =>
                    (
                        (* It's not a function. *)
                        typeMismatch("Type error in function application.",
                            valTypeMessage (lex, typeEnv) ("Function:", f, funType),
                            valTypeMessage (lex, typeEnv) ("Argument:", arg, argType),
                            PrettyString "Value being applied does not have a function type",
                            lex, location, foundNear near);
                        badInstance
                    ) 
                |   NONE =>
                    (
                        case unifyTypes (SimpleInstance funArgType, argType) of
                            NONE => ()
                        |   SOME report =>
                                typeMismatch("Type error in function application.",
                                    valTypeMessage (lex, typeEnv) ("Function:", f, funType),
                                    valTypeMessage (lex, typeEnv) ("Argument:", arg, argType),
                                    unifyErrorReport (lex, typeEnv) report, lex, location, foundNear near);
                        expType := SimpleInstance funResType; (* Preserve for browsing. *)
                        SimpleInstance funResType
                    )
            end

        |   assValues _ (v as Cond {test, thenpt, elsept, location, ...}) =
            let
                (* The test must be bool, and the then and else parts must be the
                   same. The result is either of these two once they have been
                   unified. *)
                val testType = assValues v test
                val thenType = assValues v thenpt
                val elseType = assValues v elsept
            in
                case unifyTypes(testType, boolInstance) of
                    NONE => ()
                |   SOME report =>
                        typeWrong("Condition in if-statement must have type bool.",
                            valTypeMessage (lex, typeEnv) ("If:", test, testType),
                            unifyErrorReport (lex, typeEnv) report, lex, location, foundNear v);

                case unifyTypes(thenType, elseType) of
                    NONE => thenType (* or equally elseType *)
                |   SOME report =>
                    (
                        typeMismatch("Type mismatch between then-part and else-part.",
                            valTypeMessage (lex, typeEnv) ("Then:", thenpt, thenType),
                            valTypeMessage (lex, typeEnv) ("Else:", elsept, elseType),
                            unifyErrorReport (lex, typeEnv) report, lex, location, foundNear v);
                        badInstance
                    )
            end

        |   assValues near (TupleTree{fields, expType, ...}) =
            let
                (* Construct the type obtained by mapping "assignValue" onto
                   each element of the tuple. *)
                val tupleType = mkProductType (map (instanceToType o assValues near) fields)
            in
                expType := tupleType;
                SimpleInstance tupleType
            end
          
        |   assValues _ (v as Labelled {recList, frozen, expType, ...}) =
            let
                (* Process each item in the list. *)              
                fun labEntryToLabType {name, valOrPat, expType, ...} =
                let
                    val ty = instanceToType(assValues v valOrPat)
                in
                    expType := ty;
                    {name = name, typeOf = ty }
                end
            
              val expressionType =
                mkLabelled 
                    (sortLabels (map labEntryToLabType recList), frozen) (* should always be true *)
            in
                expType := expressionType;
                SimpleInstance expressionType
            end

        |   assValues _ (Selector {typeof, ...}) =
              SimpleInstance typeof (* Already made. *)

        |   assValues _ (ValDeclaration {dec, explicit, implicit, ...}) =
                (assValDeclaration (dec, explicit, implicit); badInstance (* Should never be used. *))

        |   assValues _ (FunDeclaration fund) =
                (assFunDeclaration fund; badInstance (* Should never be used. *))

        |   assValues _ (v as OpenDec{decs=ptl, variables, structures, typeconstrs, ...}) =
                let
                    (* Go down the list of names opening the structures. *)
                    (* We have to be careful because open A B is not the same as
                       open A; open B if A contains a structure called B. *)
                    (* We accumulate the values so that we can produce debugging
                       information if we need to.  Note: we have to be careful if
                       we have the same name in multiple structures. *)
                    val valTable = HashTable.hashMake 10
                    and typeTable = HashTable.hashMake 10
                    and structTable = HashTable.hashMake 10
    
                    (* First get the structures... *)
                    fun findStructure ({name, location, value, ...}: structureIdentForm) =
                    let
                        val foundStruct =
                            lookupStructure
                                ("Structure", {lookupStruct = #lookupStruct env}, name,
                                    giveError (v, lex, location))
                        val () = value := foundStruct (* Remember in case we export. *)
                    in
                        case foundStruct of
                            SOME str => SOME(str, location)
                        |   NONE => NONE
                    end
        
                    val strs = List.mapPartial findStructure ptl
                        
                    (* Value and substructure entries in a structure will generally have
                       "Formal" access which simply gives the offset of the entry within
                       the parent structure.  We need to convert these into "Select"
                       entries to capture the address of the base structure. *)
                    fun copyEntries (str as Struct{locations, signat = sigTbl, name=strName, ...}, varLoc) =
                    let
                        val openLocs =
                        (* If we have a declaration location for the structure set this as the structure
                           location.  Add in here as the "open location". *)
                            case List.find (fn DeclaredAt _ => true | _ => false) locations of
                                SOME (DeclaredAt loc) => [StructureAt loc, OpenedAt varLoc]
                            |   _ => [OpenedAt varLoc]

                        (* Open the structure.  Formal entries are turned into Selected entries. *)
                        val _ =
                            COPIER.openSignature 
                            (sigTbl,
                            {
                                enterType   =
                                fn (name, TypeConstrSet(ty, valConstrs)) =>
                                    let
                                        (* We also have to turn the value constructors into
                                           "selected" entries in case we use datatype
                                           replication. Unlike with "include" in signatures,
                                           there's no guarantee that the constructors will also
                                           be part of the value environment. They could have
                                           been redefined. *)
                                        val newTypeSet =
                                            TypeConstrSet(ty,
                                                List.map (fn c => mkSelectedVar (c, str, openLocs)) valConstrs)
                                    in
                                        HashTable.hashSet(typeTable, name, newTypeSet);
                                        #enterType env (name, newTypeSet)
                                    end,
                                enterStruct =
                                fn (name, strVal) =>
                                    let
                                        val selectedStruct = 
                                            makeSelectedStruct (strVal, str, openLocs);
                                    in
                                        HashTable.hashSet(structTable, name, selectedStruct);
                                        #enterStruct env (name, selectedStruct)
                                    end,
                                enterVal    =
                                fn (name, value) =>
                                    let
                                        val selectedVar = 
                                            mkSelectedVar (value, str, openLocs);
                                    in
                                        HashTable.hashSet(valTable, name, selectedVar);
                                        #enterVal env (name, selectedVar)
                                    end
                            },
                            (* Add the structure we're opening here to the types of
                               the values.  The name will be removed in messages if the type
                               constructor is in scope but if it has been redefined we can
                               get an identifiable name. *)
                            strName ^ ".");
                    in
                        ()
                    end
    
                    (* ...then put them into the name space. *)
                    val () = List.app copyEntries strs;
                in
                    variables := HashTable.fold (fn (_, v, t) => v :: t) [] valTable;
                    structures := HashTable.fold (fn (_, v, t) => v :: t) [] structTable;
                    typeconstrs := HashTable.fold (fn (_, v, t) => v :: t) [] typeTable;
                    badInstance (* Does not return a type *)
                end
    
        |   assValues _ (v as TypeDeclaration(tlist, _)) =
            let (* This is either a type abbreviation in the core language, in a structure
                   or in a signature or it is a type specification in a signaure. *)
                fun messFn(name, _, new) = 
                    errorNear (lex, true, v, declaredAt(tcLocations new),
                        name ^ " has already been bound in this declaration");
               
                val newEnv = noDuplicates messFn;
              
                (* First match all the types on the right-hand sides. *)
                fun processTypeBody (TypeBind {decType = SOME decType, ...}) = SOME(ptAssignTypes(decType, v))
                |   processTypeBody _ = NONE (* Specification. *)
                
                val resTypes = List.map processTypeBody tlist;
              
                (* Can now declare the new types. *)
                fun processType (TypeBind {name, typeVars=tvcVars, isEqtype, nameLoc, tcon=tcRef, ...}, decType) =
                let
                    val typeVars = map getBoundTypeVar tvcVars
                    (* Construct a type constructor which is an alias of the
                       right-hand side of the declaration.  If we are effectively
                       giving a new name to a type constructor we use the same type
                       identifier.  This is needed to check "well-formedness" in signatures. *)
                    val props = [DeclaredAt nameLoc, SequenceNo (newBindingId lex)]

                    val tcon =
                        case decType of
                            NONE => (* Type specification *)
                            let
                                val description = { location = nameLoc, name = name, description = "" }
                            in
                                makeTypeConstructor (name,
                                    makeTypeId(isEqtype, false, (tvcVars, NONE), description), props)
                            end
                        |   SOME decT =>
                            (
                                case typeNameRebinding(typeVars, decT) of
                                    SOME typeId =>
                                        makeTypeConstructor (name, typeId, props)
                                |   NONE =>
                                    let
                                        val description = { location = nameLoc, name = name, description = "" }
                                    in
                                        makeTypeConstructor (name,
                                            makeTypeId(isEqtype, false, (tvcVars, decType), description), props)
                                    end
                            )
                in
                    checkForDots  (name, lex, nameLoc); (* Must not be qualified *)
                    #enter newEnv (name, tcon); (* Check for duplicates. *)
                    tcRef := TypeConstrSet(tcon, []);
                    #enterType env  (name, TypeConstrSet(tcon, []))  (* Put in the surrounding scope. *)
                end
                   
                val () = ListPair.app processType (tlist, resTypes);
            in
                badInstance (* Does not return a type *)
            end
        
        |   assValues _ (AbsDatatypeDeclaration absData) = assAbsData absData

        |   assValues near (v as DatatypeReplication{oldType, newType, oldLoc, newLoc, ...}) =
                  (* Adds both the type and the constructors to the
                   current environment. *)
              let
                (* Look up the type constructor in the environment. *)
                val oldTypeCons: typeConstrSet =
                    lookupTyp 
                         ({lookupType = #lookupType env, lookupStruct = #lookupStruct env},
                          oldType,
                          giveError (near, lex, oldLoc))

                (* Copy the datatype, converting any Formal constructors to Selected. *)
                local
                    (* If the type name was qualified (e.g. S.t) we need to find the
                       value constructors from the same structure. *)
                    val {first = namePrefix, ...} = splitString oldType;
                    val baseStruct =
                        if namePrefix = ""
                        then NONE
                        else lookupStructure("Structure", {lookupStruct = #lookupStruct env},
                                    namePrefix, giveError (v, lex, oldLoc))
                    val TypeConstrSet(tcons, tcConstructors) = oldTypeCons
                    val newName = newType
                    val locations = [DeclaredAt newLoc, SequenceNo (newBindingId lex)]
                    (* Create a new constructor with the same unique ID. *)
                    val typeID = tcIdentifier tcons
                    val newTypeCons = makeTypeConstructor(newName, typeID, locations)
    
                    (* Copy the value constructors. *)
                    fun copyAConstructor(Value{name=cName, typeOf=ValueType(typeOf, valTempls), class, access, ...}) =
                        let
                            (* Copy the types of value constructors replacing
                               occurrences of the old type with the new one.
                               This is not strictly necessary but improves printing.
                               e.g. local datatype X = A | B in datatype Y = datatype X end;
                               A; prints  A: Y rather than A: X *)
                            fun copyTypeCons (tcon : typeConstrs) : typeConstrs =
                                if sameTypeId(tcIdentifier tcon, typeID)
                                then newTypeCons
                                else tcon;
                            fun copyTyp (t : types) : types = copyType (t, copyTypeCons);
                            val newType = copyTyp typeOf
                            val newAccess =
                                case (access, baseStruct) of
                                    (* If we are opening a structure we must have a base structure
                                       and we turn Formal entries into Selected.  If we are replicating
                                       a datatype within a signature the original constructors will
                                       be Formal. *)
                                    (Formal addr, SOME(Struct{access, ...})) => Selected{base=access, addr=addr}
                                |    (Formal _, NONE) => access
                                |    _ => access; (* Probably already a global. *)
                        in
                            Value{name=cName, typeOf=ValueType(newType, valTempls), class=class, access=newAccess, locations=locations,
                                  references = NONE}
                        end

                in
                    val newValConstrs = map copyAConstructor tcConstructors
                    val newTypeCons = TypeConstrSet(newTypeCons, newValConstrs)
                end
            in
                (* This previously checked that it was a datatype but that's
                   not actually correct. *)
                (* Enter the value constrs in the environment. *)
                List.app (fn (c as Value{name, ...}) => (#enterVal env) (name, c)) newValConstrs;
                (* Add this type constructor to the environment. *)
                (#enterType env) (newType, newTypeCons);
                badInstance (* Does not return a type *)
            end

        |   assValues _ (aList as List{elements, location, ...}) =
            let
                val elementType =
                    assignList(assValues v, displayParsetree, valTypeMessage)
                        ("Elements in a list have different types.", "Item", ",", location, aList) elements
                val elementType = instanceToType elementType
            in
                if isBadType elementType
                then badInstance
                else SimpleInstance(mkTypeConstruction ("list", tsConstr listConstr, [elementType], [DeclaredAt inBasis]))
            end

        |   assValues near (v as Constraint {value, given, location}) =
            let
                val valType = assValues near value;
                val theType = ptAssignTypes(given, v)
            in
                (* These must be unifiable. *)
                case unifyTypes(valType, SimpleInstance theType) of
                    NONE => () (* OK. *)
                |   SOME report =>
                        typeMismatch("Type mismatch in type constraint.",
                            valTypeMessage (lex, typeEnv) ("Value:", value, valType),
                            PrettyBlock(0, false, [],
                                [
                                    PrettyString "Constraint:",
                                    PrettyBreak(1, 0),
                                    display(SimpleInstance theType, 10000 (* All of it *), typeEnv)
                                ]),
                            unifyErrorReport (lex, typeEnv) report,
                            lex, location, foundNear v);
                SimpleInstance theType
            end

        |   assValues _ (aFun as Fn {matches, location, ...}) =  (* Must unify the types of each of the alternatives.*)
            let
                val resType =
                    assignList(assMatchTree aFun, displayMatch, matchTypeMessage)
                        ("Clauses in fn expression have different types.", "Clause", "|", location, aFun) matches
            in
                resType
            end

        |   assValues _ (Unit _) = unitInstance

        |   assValues _ (Localdec {decs, body, isLocal, varsInBody, ...}) =
            let (* Local declarations or expressions. *)
              val newValEnv  = searchList();
              val newTypeEnv = searchList();
              val newStrEnv  = searchList();
              val newLetDepth = if isLocal then letDepth else letDepth+1;
              (* The environment for the local declarations. *)
              val localEnv =
                {
                   lookupVal     = lookupDefault (#lookup newValEnv)  (#lookupVal env),
                   lookupType    = lookupDefault (#lookup newTypeEnv) (#lookupType env),
                   lookupFix     = #lookupFix env,
                   (* This environment is needed if we open a 
                      structure which has sub-structures. *)
                   lookupStruct  = lookupDefault (#lookup newStrEnv) (#lookupStruct env),
                   lookupSig     = #lookupSig env,
                   lookupFunct   = #lookupFunct env,
                   lookupTvars   = #lookupTvars env,
                   enterVal      = #enter newValEnv,
                   enterType     = #enter newTypeEnv,
                  (* Fixity has already been dealt with in the parsing process.  The only reason
                     we deal with it here is to ensure that declarations are printed in the
                     correct order.  We simply need to make sure that local fixity declarations
                     are ignored. *)
                   enterFix      = fn _ => (),
                   enterStruct   = #enter newStrEnv,
                   enterSig      = #enterSig env,
                   enterFunct    = #enterFunct env,
                   allValNames   = fn () => (stringsOfSearchList newValEnv () @ #allValNames env ()),
                   allStructNames = fn () => #allStructNames env ()
                };
        
              (* Process the local declarations and discard the result. *)
              val _ = assignSeq(localEnv, newLetDepth, decs, false)
        
              (* This is the environment used for the body of the declaration.
                 Declarations are added both to the local environment and to
                 the surrounding scope. *)
              val bodyEnv =
                { 
                  (* Look-ups come from the local environment *)
                  lookupVal     = #lookupVal localEnv,
                  lookupType    = #lookupType localEnv,
                  lookupFix     = #lookupFix localEnv,
                  lookupStruct  = #lookupStruct localEnv,
                  lookupSig     = #lookupSig localEnv,
                  lookupFunct   = #lookupFunct localEnv,
                  lookupTvars   = #lookupTvars localEnv,
                  enterVal      =
                    fn (pair as (_, v)) =>
                      (varsInBody := v :: ! varsInBody;
                       #enter newValEnv pair;
                       #enterVal env      pair),
                  enterType     =
                    fn pair =>
                      (#enter newTypeEnv pair;
                       #enterType env      pair),
                  enterFix      = #enterFix env,
                  enterStruct   =
                    fn pair =>
                      (#enter newStrEnv pair;
                       #enterStruct env   pair),
                  enterSig      = #enterSig env,
                  enterFunct    = #enterFunct env,
                  allValNames   = #allValNames localEnv,
                  allStructNames = #allStructNames localEnv
                };
              (* Now the body, returning its result if it is an expression. *)
                val resType = assignSeq(bodyEnv, newLetDepth, body, not isLocal)
            in
                resType
            end (* LocalDec *)

        |   assValues _ (ExpSeq (ptl, _)) =
             (* A sequence of expressions separated by semicolons.
                Result is result of last expression. *)
              assignSeq (env, letDepth, ptl, true)

        |   assValues _ (v as ExDeclaration(tlist, _)) =
            let
                fun messFn(name, _, line) =
                    errorNear (lex, true, v, line,
                        name ^ " has already been bound in this declaration");
         
                (* Construct an environment to check for duplicate declarations.
                   Include the declaration location as the value. *)
                val dupEnv = noDuplicates messFn;
  
                fun processException (ExBind {name, previous, ofType, value, nameLoc, ...}) =
                let
                    (* Fill in any types.  If there was no type given the exception has type exn
                       otherwise it has type ty->exn. *)
                    val (oldType, nullary) =
                        case ofType of
                            NONE => (exnType, true)
                        |   SOME typeof => (mkFunctionType(ptAssignTypes(typeof, v), exnType), false)
                    val locations = [DeclaredAt nameLoc, SequenceNo (newBindingId lex)]
    
                    val exValue = 
                        case previous of 
                            EmptyTree => mkEx (name, ValueType(oldType, []), nullary, locations) (* Generative binding. *)
                                
                        |   Ident {name = prevName, value = prevValue, location, expType, possible, ...} =>
                            let 
                                (* ex = ex' i.e. a non-generative binding? *)
                                (* Match up the previous exception. *)
                                val prev as Value{typeOf = excType as ValueType(oldExcType, _), ...} = 
                                    lookupValue 
                                        ("Exception",
                                            {lookupVal= #lookupVal env,
                                            lookupStruct= #lookupStruct env},
                                            prevName,
                                            giveError (v, lex, location))
                                val nullary = (* Check that it is an exception *)
                                case prev of
                                    Value{class=Exception{nullary}, ...} => nullary
                                |    _ => (errorNear (lex, true, v, location, "(" ^ prevName ^ ") is not an exception."); true)

                            in
                                prevValue := prev; (* Set the value of the looked-up identifier. *)
                                expType := (SimpleInstance oldExcType, []); (* And remember the type. *)
                                possible := (fn () => allValLongNamesWithPrefix prevName env);
                                (* The result is an exception with the same type. *)
                                mkEx (name, excType, nullary, locations)
                            end
                        | _ =>
                            raise InternalError "processException: badly-formed parse-tree"
                in
                    (* Save this value. *)
                    value := exValue;
        
                    (* In the check environment *)
                    #enter dupEnv (name, nameLoc);
        
                    (* Must not be qualified *)
                    checkForDots (name, lex, nameLoc) : unit;
                    (* Must not be "true", "false" etc. *)
                    checkForBuiltIn (name, v, lex, nameLoc, true) : unit;
        
                    (* Put this exception into the env *)
                    #enterVal env (name, exValue) 
                end
  
                val () = List.app processException tlist;
            in
                badInstance
            end (* ExDeclaration *)
        
        |   assValues _ (v as Raise (pt, line)) =
            let
                val exType = assValues v pt
            in
                (* The exception value must have type exn. *)
                case unifyTypes(exType, SimpleInstance exnType) of
                    NONE => ()
                |   SOME report =>
                        typeWrong("Exception to be raised must have type exn.",
                            valTypeMessage (lex, typeEnv) ("Raise:", pt, exType),
                            unifyErrorReport (lex, typeEnv) report, lex, line, foundNear v);
                (* Matches anything *)
                SimpleInstance(mkTypeVar (Generalisable, false))
            end
  
        |   assValues _ (aHandler as HandleTree {exp, hrules, location, ...}) =
            let
                (* If the expression returns type E
                 the handler must be exn -> E *)
                val expType = assValues aHandler exp;
                (* Unify the handler with a function from exn -> expType *)
                val clauses =
                    assignList(assMatchTree aHandler, displayMatch, matchTypeMessage)
                        ("Clauses in handler have different types.", "Clause", "|", location, aHandler) hrules
                (* The result type of the handlers must match the result type of the expression being
                   handled and the arguments must all have type exn. *)
                val () = 
                    unify (clauses, SimpleInstance(mkFunctionType (exnType, instanceToType expType)), lex, location, foundNear v, typeEnv);
            in
                expType (* Result is expType. *)
            end

        |   assValues _ (v as While {test, body, location, ...}) =
            let
                val testType = assValues v test
            in
                (* Test must be bool. Result is unit *)
                case unifyTypes(testType, boolInstance) of
                    NONE => ()
                |   SOME report =>
                        typeWrong("Loop condition of while-expression must have type bool.",
                            valTypeMessage (lex, typeEnv) ("While:", test, testType),
                            unifyErrorReport (lex, typeEnv) report, lex, location, foundNear v);
                assValues v body; (* Result of body is discarded. *)
                unitInstance
            end

        |   assValues _ (aCase as Case {test, match, location, ...}) =
            let
                val funType =
                    assignList(assMatchTree aCase, displayMatch, matchTypeMessage)
                        ("Clauses in case have different types.", "Clause", "|", location, aCase) match;
                val argType = assValues aCase test;
                (* The matches constitute a function from the test type to
                   the result of the case statement, so we apply the match type
                   to the test. *)
            in
                apply (funType, instanceToType argType, lex, location, foundNear aCase, typeEnv)
            end

        |   assValues _ (anAndAlso as Andalso {first, second, location}) =
            let
                (* Both parts must be bool and the result is bool. *)
                fun mkTupleTree(fields, location) = TupleTree { fields=fields, location=location, expType = ref BadType }
                val pairArgs = mkTupleTree([first, second], location)
                val argTypes  = assValues anAndAlso pairArgs;
                val () =
                    case unifyTypes(argTypes, boolStarBool) of
                        NONE => ()
                    |   SOME report =>
                            typeWrong("Arguments of andalso must have type bool*bool.",
                                valTypeMessage (lex, typeEnv) ("Arguments:", pairArgs, argTypes),
                                unifyErrorReport (lex, typeEnv) report, lex, location, foundNear anAndAlso)
            in
                boolInstance
            end

        |   assValues _ (anOrElse as Orelse {first, second, location}) =
            let
                (* Both parts must be bool and the result is bool. *)
                fun mkTupleTree(fields, location) = TupleTree { fields=fields, location=location, expType = ref BadType }
                val pairArgs = mkTupleTree([first, second], location)
                val argTypes  = assValues anOrElse pairArgs;
                val () =
                    case unifyTypes(argTypes, boolStarBool) of
                        NONE => ()
                    |   SOME report =>
                            typeWrong("Arguments of orelse must have type bool*bool.",
                                valTypeMessage (lex, typeEnv) ("Arguments:", pairArgs, argTypes),
                                unifyErrorReport (lex, typeEnv) report, lex, location, foundNear anOrElse)
            in
                boolInstance
            end

        |   assValues _ (Directive { tlist, fix, ... }) =
                  (
                (* Infix declarations have already been processed by the parser.  We include
                   them here merely so that we get all declarations in the correct order. *)
                List.app (fn name => #enterFix env (name, FixStatus(name, fix))) tlist;
                badInstance
                )

        |   assValues _ (WildCard _) = (* Should never occur in an expression. *)
                  raise InternalError "assignTypes: wildcard found"

        |   assValues _ (Layered _) =
                  raise InternalError "assignTypes: layered pattern found"

        |   assValues _ EmptyTree =
                  raise InternalError "assignTypes: emptytree found"

        |   assValues near (Parenthesised(p, _)) = assValues near p

    
        and assMatchTree _ (MatchTree {vars, exp, resType, argType, ...}) =
            let 
              (* A match is a function from the pattern to the expression *)
              
              (* Process the pattern looking for variables. *)
        
               (* Construct a new environment for the variables. *)
              fun messFn(name, _, Value{locations, ...}) =  
                    errorNear (lex, true, v, declaredAt locations,
                        name ^ " has already been bound in this match");
              
              val newEnv   = noDuplicates messFn;
              val newLevel = level + 1;
              val decs     = processPattern(vars, #enter newEnv, newLevel, false, mkPattVar, false)
        
              (* The identifiers declared in the pattern are available in the
                 body of the function. *)
              val bodyEnv =
                {
                  lookupVal     = lookupDefault (#lookup newEnv) (#lookupVal env),
                  lookupType    = #lookupType env,
                  lookupFix     = #lookupFix env,
                  lookupStruct  = #lookupStruct env,
                  lookupSig     = #lookupSig env,
                  lookupFunct   = #lookupFunct env,
                  lookupTvars   = #lookupTvars env,
                  enterVal      = #enterVal env,
                  enterType     = #enterType env,
                  enterFix      = #enterFix env,
                  enterStruct   = #enterStruct env,
                  enterSig      = #enterSig env,
                  enterFunct    = #enterFunct env,
                  allValNames   =
                    fn () => (stringsOfSearchList newEnv () @ #allValNames env ()),
                  allStructNames = fn () => #allStructNames env ()
                };
        
              (* Now the body. *)
              val expType = assignValues(newLevel, letDepth, bodyEnv, v, exp);
            in
                (* Save these in case they're needed for the debugger. *)
                resType := instanceToType expType;
                argType := instanceToType decs;
                (* Result is a function from the type of the pattern to the type
                 of the body. This previously generalised the resulting type. Why? *)
                SimpleInstance(mkFunctionType (instanceToType decs, instanceToType expType))
            end (* MatchTree *)

        and assValDeclaration (valdecs: valbind list, explicit, implicit) =
        (* assignTypes for a val-declaration. *)
        let
            val newLevel = level + 1
      
            (* Set the scope of explicit type variables. *)
            val () = #apply explicit(fn (_, tv) => setParseTypeVar (tv, NONE, newLevel))

            (* For each implicit type variable associated with this value declaration,
               link it to any type variable with the same name in an outer
               scope.  The outer occurrence can be textually later. *)
            val () = 
                #apply implicit
                    (fn (name, tv: parseTypeVar) => setParseTypeVar(tv, #lookupTvars env name, newLevel))
            (* If it isn't there set the level of the type variable. *)

            (* Construct a new environment for the variables. *)
            val newEnv =
                noDuplicates
                (fn(name, _, Value{locations, ...}) =>
                    errorNear (lex, true, v, declaredAt locations,
                        name ^ " has already been bound in this declaration"));

            (* This environment is those identifiers declared by recursive bindings *)
            val recEnv = searchList ();

            (* If this is a recursive declaration we will have to find all
               the variables declared by the patterns in each binding before
               we can look at the bodies of the bindings. For simplicity we
               process the patterns first even if this is not recursive but
               arrange for the variables to be added to the environment
               after rather than before processing the bodies. The result of
               processing the patterns is a list of their types. Each item
               in the list must be unified with the type of the
               corresponding body. *)

            (* Process the patterns. *)
            local
                fun doVal (ValBind {dec, isRecursive, variables, ...}) =
                    let
                        fun enterVals(pair as (_, value)) =
                        (
                            #enter newEnv pair;
                            if isRecursive then #enter recEnv pair else ();
                            variables := value :: ! variables
                        )

                        val patType =
                            processPattern(dec, enterVals, newLevel, false, mkValVar, isRecursive);
                    in
                        patType
                    end;
                
            in
                val decs = List.map doVal (valdecs)
            end

            (* Now the bodies. *)
            local
                (* Check that the types match by going down the list of value
                   bindings and the list of types produced from the patterns,
                   and matching corresponding types. *)
                fun checkTypes (patType, (ValBind {dec, exp, line, isRecursive, ...})) =
                    let
                        val newEnv =
                        { (* If this is recursive we find the recursive names
                             and others in the surrounding scope. *)
                            lookupVal     = 
                                if isRecursive
                                then lookupDefault (#lookup recEnv) (#lookupVal env)
                                else #lookupVal env,
                            lookupType    = #lookupType env,
                            lookupFix     = #lookupFix env,
                            lookupStruct  = #lookupStruct env,
                            lookupSig     = #lookupSig env,
                            lookupFunct   = #lookupFunct env,
                            (* Extend the environment of type variables. *)
                            lookupTvars   =
                                lookupDefault (#lookup explicit)
                                    (lookupDefault (#lookup implicit) (#lookupTvars env)),
                            enterVal      = #enterVal env,
                            enterType     = #enterType env,
                            enterFix      = #enterFix env,
                            enterStruct   = #enterStruct env,
                            enterSig      = #enterSig env,
                            enterFunct    = #enterFunct env,
                            allValNames   =
                                if isRecursive
                                then fn () => (stringsOfSearchList recEnv () @ #allValNames env ())
                                else #allValNames env,
                            allStructNames = #allStructNames env
                        }

                        val expType = assignValues(newLevel, letDepth, newEnv, exp, exp);
            
                        val () =
                            case unifyTypes(patType, expType) of
                                NONE => () (* OK*)
                            |   SOME report =>
                                    typeMismatch("Pattern and expression have incompatible types.",
                                        valTypeMessage (lex, typeEnv) ("Pattern:", dec, patType),
                                        valTypeMessage (lex, typeEnv) ("Expression:", exp, expType),
                                        unifyErrorReport (lex, typeEnv) report, lex, line, foundNear v)
        
                        (* true if the expression is a possibly-constrained fn-expression.
                           It isn't clear whether a parenthesised expression is allowed here.
                           As often, the definition is informal.  On p8 of the ML97
                           definition it says "exp must be of the form fn match".  In ML90
                           it added "possibly constrained by one or more type expressions".
                           This is such a mess that I'm allowing both contraints and parentheses
                           here. *)
                        fun isConstrainedFn (Constraint {value, ...}) = isConstrainedFn value
                        |   isConstrainedFn (Fn _)  = true
                        |   isConstrainedFn (Parenthesised(p, _)) = isConstrainedFn p
                        |   isConstrainedFn _       = false;
                    in
                        (* Must check that the expression is of the form FN match. *)
                        (* N.B. the code generator assumes this is true. *)
                        if isRecursive andalso not (isConstrainedFn exp)
                        then errorNear (lex, true, v, line, 
                            "Recursive declaration is not of the form `fn match'")
                        else ()
                    end
            in
                val () = ListPair.app checkTypes (decs, valdecs)
            end
            
            (* Generalise the list and declare the new names into the surrounding environment. *)
            fun generaliseAndDeclare(ValBind{exp, variables=ref vars, line, ...}) =
            let
                fun genAndEnter(Value{name, typeOf=ValueType(typeOf, _), access, class, locations, references, ...}) =
                let
                    (* If this is a non-expansive context we can create a generic instance.  Otherwise the
                       original type variables are used.  In that case we need to check that there are
                       no references to explicit type variables. *)
                    val valType =
                        if nonExpansive exp
                        then
                        let
                            open DEBUG
                            val parameters = debugParams lex
                            val checkOverloadFlex = getParameter narrowOverloadFlexRecordTag parameters
                        in
                            allowGeneralisation(SimpleInstance typeOf, newLevel, checkOverloadFlex, giveError (v, lex, line))
                        end
                        else
                        (
                            if containsLocalFreeVariables(typeOf, newLevel)
                            then errorNear (lex, true, v, line, "Explicit type variables cannot be generalised")
                            else ();
                            (typeOf, [])
                        )
                    val newValue =
                        Value{name=name, typeOf=ValueType valType, access=access, class=class, locations=locations,
                              references=references}
                in
                    #enterVal env (name, newValue)
                end
            in
                List.app genAndEnter vars
            end
        in
            List.app generaliseAndDeclare valdecs
        end (* assValDeclaration *)

        and assFunDeclaration {dec=tlist: fvalbind list, explicit, implicit, ...} =
        (* Assigntypes for a fun-declaration. *)
        let
            val funLevel = level + 1; (* Level for function names. *)
      
            (* Set the scope of explicit type variables. *)
            val () =
                #apply explicit(fn (_, tv) => setParseTypeVar (tv, NONE, funLevel))

            (* For each implicit type variable associated with this value declaration,
               link it to any type variable with the same name in an outer
               scope. *)
            val () = 
                #apply implicit (fn (name, tv) => setParseTypeVar(tv, #lookupTvars env name, funLevel))
            (* If it isn't there set the level of the type variable. *)

            (* Construct a new environment for the variables. *)
            fun msgFn(name, _, Value{locations, ...}) = 
                errorNear (lex, true, v, declaredAt locations,
                    name ^ " has already been bound in this declaration");
           
            val newEnv = noDuplicates msgFn;
           
            (* Since this is a recursive declaration we must get the function
               names first. Because of the way they are parsed they are hidden
               as applications of the function to one or more patterns. There
               may be more than one clause in a function binding but each
               should declare the same function and have the same number of
               patterns. We need to know the number of patterns and the
               function name in the third pass so we save them in the
               function binding. *)

            local
                fun findNameAndPatts (FValBind {clauses = (FValClause {dec, line, ...}::_), numOfPatts, functVar, ...}) =
                let
                    (* Just look at the first clause for the moment. *)
                    val { ident = { name, location, ... }, ... } = dec;
                    (* Declare a new identifier with this name. *)
                    val locations = [DeclaredAt location, SequenceNo (newBindingId lex)]
                    val funVar =
                        mkValVar (name, ValueType(mkTypeVar (NotGeneralisable funLevel, false), []), locations)

                    val arity = case dec of { args, ...} => List.length args
                    val () = numOfPatts := arity;
                    val () =
                        (* Put the results onto the function binding. *)
                        if arity = 0
                        then errorNear (lex, true, v, line,
                                "Clausal function does not have any parameters.")
                        else ()
                in
                    (* Must not be qualified *)
                    checkForDots (name, lex, line);
                    (* Must not be "true", "false" etc. but may be "it" *)
                    checkForBuiltIn (name, v, lex, line, false);
                    functVar := funVar; (* Save the variable. *)
                    (* Enter it in the environment. *)
                    #enter newEnv (name, funVar)
                end
                |   findNameAndPatts _ = raise InternalError "findNameAndPatts: badly-formed parse-tree";

            in
                val () = List.app findNameAndPatts tlist
            end;

            local
                (* Can now process the clausal functions in the environment 
                   of the function names and using the information about
                   function name and number of patterns we have saved. *)
                fun processBinding
                    (fvalBind as FValBind {clauses, functVar=ref(Value{typeOf=ValueType(oldfuncVarType, _), name=functVarName, ...}), argType, resultType, location, ...}) =
                let
                    (* Each fun binding in the declaration may consist of several
                       clauses. Each must have the same function name, the same
                       number of patterns and a unifiable type. *)
                    (* The type information is built up from the bottom so that if there are
                       errors we can report them in the most appropriate place.
                       Build a type to be used for the function.  This will later be unified
                       with the type that we've already created for the function variable. *)
                    val funType = SimpleInstance(mkTypeVar(Generalisable, false))

                    fun processClause (clause as FValClause {dec, exp, line, ...}) =
                    let
                        val { ident = ident, args, constraint, ... } = dec

                        fun mkClausal(clauses, location) : fvalbind =
                           FValBind
                             { 
                               clauses    = clauses,
                               numOfPatts = ref 0,
                               functVar   = ref undefinedValue,
                               argType    = ref BadType,
                               resultType = ref BadType,
                               location   = location
                             }
    
                        fun mkFunDeclaration (dec, explicit, implicit, location) : parsetree =
                          FunDeclaration
                            {
                                dec=dec,
                                explicit = explicit,
                                implicit = implicit,
                                location = location
                            }

                        val clauseAsTree: parsetree =
                            (* This clause as a parsetree object for error messages. *)
                            mkFunDeclaration([mkClausal([clause], line)], explicit, implicit, line)
                        
                        val () = (* Set the type.  Only in case we look at the export tree. *)
                            #expType ident := oldfuncVarType

                        fun messFn (name, _, Value{locations, ...}) =
                            errorNear (lex, true, clauseAsTree, declaredAt locations,
                                name ^ " has already been bound in this clause.");
                        (* Construct a new environment for the variables in the patts. *)
                        val varEnv = noDuplicates messFn;
                        val varLevel = funLevel + 1; (* Level for variables. *)

                        (* Process the patterns. *)
                        val argTypeList =
                            List.map (fn arg =>
                                        processPattern(arg, #enter varEnv, varLevel, false, mkPattVar, false))
                                args
                        (* This list is used for the type of the helper function. *)
                        val () = argType :=
                            (case argTypeList of
                                [] => BadType (* error *)
                            |   [single] => instanceToType single
                            |   multiple => mkProductType(List.map instanceToType multiple))

                        (* The identifiers declared in the pattern are available in the
                           body of the function. Since it is recursive the function
                           names are also available. *)
                        val bodyEnv =
                        { 
                            lookupVal     = 
                                lookupDefault (#lookup varEnv)
                                    (lookupDefault (#lookup newEnv) (#lookupVal env)),
                            lookupType    = #lookupType env,
                            lookupFix     = #lookupFix env,
                            lookupStruct  = #lookupStruct env,
                            lookupSig     = #lookupSig env,
                            lookupFunct   = #lookupFunct env,
                            (* Extend the environment of type variables. *)
                            lookupTvars   =
                                lookupDefault (#lookup explicit)
                                    (lookupDefault (#lookup implicit) (#lookupTvars env)),
                            enterVal      = #enterVal env,
                            enterType     = #enterType env,
                            enterFix      = #enterFix env,
                            enterStruct   = #enterStruct env,
                            enterSig      = #enterSig env,
                            enterFunct    = #enterFunct env,
                            allValNames   =
                                fn () => (stringsOfSearchList varEnv () @
                                          stringsOfSearchList newEnv () @ #allValNames env ()),
                            allStructNames = fn () => #allStructNames env ()
                        };
           
                        (* Now the body. *)
                        val expTyp = assignValues(varLevel, letDepth, bodyEnv, exp, exp);
                        (* Check the expression type against any explicit type constraint. *)
                        (* Return the explicit constraint if possible.  For the purposes of
                           type checking this is identical to "expTyp" but if there is a
                           type abbreviation this will be used when printing the result.
                           .e.g type t = int * int; fun f ((x, y):t): t = (y, x); *)
                        val typeOfBody =
                            case constraint of
                                NONE => expTyp
                            |   SOME given =>
                                let
                                    val theType = SimpleInstance(ptAssignTypes(given, v))
                                in
                                    case unifyTypes(expTyp, theType) of
                                        NONE => () (* OK. *)
                                    |   SOME report =>
                                            typeMismatch("Body of fun binding does not match type constraint.",
                                                valTypeMessage (lex, typeEnv) ("Expression:", exp, expTyp),
                                                PrettyBlock(0, false, [],
                                                    [
                                                        PrettyString "Constraint:",
                                                        PrettyBreak(1, 0),
                                                        display(theType, 10000 (* All *), typeEnv)
                                                    ]),
                                                unifyErrorReport (lex, typeEnv) report,
                                                lex, line, foundNear clauseAsTree);
                                    theType
                                end
                        (* Remember the result type for the debugger. Actually this
                           assigns the result type for each clause in the fun but
                           they'll all be the same. *)
                        val () = resultType := instanceToType typeOfBody
                        (* The type of this clause is a function type. *)
                        val clauseType = List.foldr (fn (a, b) => SimpleInstance(mkFunctionType(instanceToType a, instanceToType b))) typeOfBody argTypeList
                        (* Unify this with the type we're using for the other clauses. *)
                        val () =
                            case unifyTypes(clauseType, funType) of
                                NONE => () (* OK. *)
                            |   SOME report =>
                                    typeMismatch("Type of clause does not match the type of previous clauses.",
                                        valTypeMessage (lex, typeEnv) ("Clause:", clauseAsTree, clauseType),
                                        PrettyBlock(0, false, [],
                                            [
                                                PrettyString "Other clauses:",
                                                PrettyBreak(1, 0),
                                                display(funType, 10000 (* All *), typeEnv)
                                            ]),
                                        unifyErrorReport (lex, typeEnv) report,
                                        lex, line, foundNear clauseAsTree)
                    in (* body of processClause *)
                        ()
                    end
                in (* body of processFun *)
                    List.app processClause clauses;
                    (* If this function makes any recursive references move those references from the
                       local list to the recursive list.  In that way if we're looking for whether a
                       function is actually referenced we'll only include it if it's referenced outside
                       or from another referenced function. *)
                    let
                        fun moveRefs(FValBind{functVar=ref(Value{references,...}), ...}) =
                        let
                            val {localRef as ref locals, recursiveRef, ...} = valOf references
                            val callerName = functVarName (* Name of referring function. *)
                        in
                            recursiveRef := List.map (fn r => (r, callerName)) locals @ !recursiveRef;
                            localRef := []
                        end
                    in
                        List.app moveRefs tlist
                    end;
                    (* Finally unify the function type with the type of the function variable.  If the
                       variable has not yet been used that will simply set its type but if it has been
                       used recursively it may have been given an incompatible type. *)
                    case unifyTypes(funType, SimpleInstance oldfuncVarType) of
                        NONE => () (* OK. *)
                    |   SOME report =>
                        let
                            fun mkFunDeclaration (dec, explicit, implicit, location) : parsetree =
                              FunDeclaration
                                {
                                    dec=dec,
                                    explicit = explicit,
                                    implicit = implicit,
                                    location = location
                                }

                            fun mkIdent (name, loc) : parsetree = 
                              Ident
                                {
                                  name   = name,
                                  expType = ref(badInstance, []),
                                  value  = ref undefinedValue,
                                  location = loc,
                                  possible = ref(fn () => [])
                                }
 
                            val fvalAsTree = mkFunDeclaration([fvalBind], explicit, implicit, location)
                        in
                            typeMismatch("Type of function does not match type of recursive application.",
                                valTypeMessage (lex, typeEnv) ("Function:", fvalAsTree, funType),
                                valTypeMessage (lex, typeEnv)
                                    ("Variable:", mkIdent(functVarName, location), SimpleInstance oldfuncVarType),
                                unifyErrorReport (lex, typeEnv) report,
                                lex, location, foundNear fvalAsTree)
                        end
                end
            in
                val () = List.app processBinding tlist
            end

            fun generaliseAndDeclare(
                    FValBind{functVar as ref(Value{typeOf=ValueType(typeOf, _), access, class, locations, name, references, ...}), location, ...}) =
            let
                open DEBUG
                val parameters = debugParams lex
                val checkOverloadFlex = getParameter narrowOverloadFlexRecordTag parameters
                val valType =
                    allowGeneralisation(SimpleInstance typeOf, funLevel, checkOverloadFlex, giveError (v, lex, location))
                val newValue =
                    Value{name=name, typeOf=ValueType valType, access=access, class=class, locations=locations,
                          references=references}
            in
                #enterVal env (name, newValue);
                functVar := newValue
            end
        in
            (* Now declare the new names into the surrounding environment,
               releasing the copy flags on the type variables. All fun
               bindings are non-expansive. *)
            List.app generaliseAndDeclare tlist
        end (* assFunDeclaration *)

        and assAbsData({typelist=typeList, withtypes, declist, equalityStatus, isAbsType=isAbs, ...}) =
        let
            (* A type declaration causes a type to be entered in the type
               environment, together with some constructors. *)
            fun messFn (name, _, TypeConstrSet(new, _)) = 
                errorNear (lex, true, v, declaredAt(tcLocations new),
                   name ^ " has already been bound in this declaration");

            val localTypeEnv = noDuplicates messFn;
      
            (* datatype and abstype declarations are both recursive so we can
               enter the type names into the environment during a first pass,
               and then process the value constructors during a second. *)
            fun enterType(typeConstr, typeName) =
            (
                checkForDots  (typeName, lex, declaredAt(tcLocations(tsConstr typeConstr))); (* Must not be qualified *)
                #enter localTypeEnv (typeName, typeConstr) (* Check for duplicates. *)
            );
       
            (* Make the type constructors and put them in a list. *)
            fun enterTcon (DatatypeBind {name, tcon, typeVars, nameLoc, ...}) =
            let
                (* Make a new ID.  If this is within a let declaration we always make
                   a free ID because it is purely local and can't be exported. *)
                val description = { location = nameLoc, name = name, description = "" }
                val arity = length typeVars
            
                val newId =
                    if letDepth = 0
                    then makeTypeId(false, true, (typeVars, NONE), description)
                    else makeFreeIdEqUpdate (arity, Local{addr = ref ~1, level = ref baseLevel}, false, description)
                val locations = [DeclaredAt nameLoc, SequenceNo (newBindingId lex)]
                val tc = makeTypeConstructor(name, newId, locations)
            in
                tcon := TypeConstrSet(tc, []);
                enterType(TypeConstrSet(tc, []), name);
                tc
            end
      
            val listOfTypes = map enterTcon typeList;

            local
                fun lookup(s, line) =
                    lookupTyp 
                        ({lookupType = lookupDefault(#lookup localTypeEnv) (#lookupType env),
                            lookupStruct = #lookupStruct env},
                        s, giveError (v, lex, line))
            in
                fun localAssignTypes decType = assignTypes (decType, lookup, lex)
            end

            (* First match all the types on the right-hand sides using the
               datatypes and the existing bindings. *)
            local
                fun processType (TypeBind {decType = SOME decType, ...}) = SOME(localAssignTypes decType)
                |   processType (TypeBind {decType = NONE, ...}) = NONE
            in
                val decTypes = List.map processType withtypes
            end;

            (* Can now enter the `withtypes'. *)
            fun enterWithType (TypeBind {name, typeVars=tcvVars, nameLoc, tcon=tcRef, ...}, decType) =
            let
                val description = { location = nameLoc, name = name, description = "" }
                (* Construct a type constructor which is an alias of the
                   right-hand side of the declaration. *)
                val locations = [DeclaredAt nameLoc, SequenceNo (newBindingId lex)]
                val tcon =
                    makeTypeConstructor (name,
                        makeTypeId(false, false, (tcvVars, decType), description), locations)
                val tset = TypeConstrSet(tcon, [])
            in
                tcRef := tset;
                enterType(tset, name); (* Checks for duplicates. *)
                #enterType env (name, tset); (* Put in the global environment. *)
                tset
            end

            val withTypeConstrSets = ListPair.map enterWithType (withtypes, decTypes)
        
            (* For the constructors *)
            fun messFn (name, _, Value{locations, ...}) =
                errorNear (lex, true, v, declaredAt locations,
                    name ^ " has already been used as a constructor in this type");
      
            val consEnv = noDuplicates messFn;
            val abstypeEnv = searchList ();
    
            (* Now process the types and generate the constructors. *)
            fun genValueConstrs (DatatypeBind {name, typeVars, constrs, nameLoc, tcon, ...}, typ) =
            let
                val numOfConstrs = length constrs;
                val typeVarsAsTypes = List.map getBoundTypeVar typeVars
                (* Default template for each type variable. *)
                val templates = List.map (fn _ => TemplPlain{ equality=false }) typeVars
        
                (* The new constructor applied to the type variables (if any) *)
                val locations = [DeclaredAt nameLoc, SequenceNo (newBindingId lex)]
                val resultType = mkTypeConstruction (name, typ, typeVarsAsTypes, locations)

                (* Sort the constructors by name.  This simplifies matching with
                   datatypes in signatures. *)
                fun leq {constrName=xname: string, ...} {constrName=yname, ...} = xname < yname;
                val sortedConstrs = quickSort leq constrs;

                fun processConstr ({constrName=name, constrArg, idLocn, constrVal, ...}) =
                let
                    val (constrType, isNullary) =
                        case constrArg of
                            NONE => (resultType, true)
                        |   SOME argtype =>
                                (mkFunctionType (localAssignTypes argtype, resultType), false)
                    val locations = [DeclaredAt idLocn, SequenceNo (newBindingId lex)]
                    val cons =
                        Value { name = name, typeOf = ValueType(constrType, templates),
                            access = Local{addr = ref ~1, level = ref baseLevel},
                            class = Constructor { nullary = isNullary, ofConstrs = numOfConstrs },
                            locations = locations, references = NONE }
        
                    (* Name must not be qualified *)
                    val () = checkForDots (name, lex, idLocn);
                    (* Must not be "true", "false" etc. *)
                    val () = checkForBuiltIn (name, v, lex, idLocn, true) : unit;
          
                    (* Put into the environment. *)
                    val () = #enter consEnv (name, cons)
                    
                    (* Link it in case we export the tree. *)
                    val () = constrVal := cons
                in    
                    cons
                end (* processConstr *)
                val tset = TypeConstrSet (typ, List.map processConstr sortedConstrs)
            in
                (* If this is an abstype enter the version with the constructors into
                   a local environment and a version without the constructors into the
                   global environment.  If it is a datatype enter the version with
                   constructors in the global environment. *)
                if isAbs
                then (#enter abstypeEnv (name, tset); #enterType env (name, TypeConstrSet(typ, [])))
                else #enterType env (name, tset);
                tcon := tset;
                tset
            end (* genValueConstrs *)

            val listOfTypeSets =
                ListPair.map genValueConstrs (typeList, listOfTypes) @ withTypeConstrSets

            (* Third pass - Check to see if equality testing is allowed for
               these types. *)
            val () = computeDatatypeEqualities(listOfTypeSets, sigTypeIdMap);

            (* If this is a datatype declaration the value constructors should be
               entered in the surrounding scope, otherwise (abstract type
               declaration) we evaluate the declarations in an environment
               containing the value constructors, but the value constructors
               themselves do not appear in the surrounding scope. *)
            val () =
                if not isAbs
                then #apply consEnv (#enterVal env)
                else
                let   (* Abstract type *)
                    (* The declarations have to be evaluated in an environment in
                       which the constructors have been declared. When an identifier
                       is looked up it may:
                       (a) be one of these new declarations, or else
                       (b) be a constructor from the type declarations, or else
                       (c) be outside the abstract type declaration.
                       New declarations are entered in the local environment so that
                       they can be found under (a) and in the surrounding environment
                       where they will be available after this declaration. *)
                    val decEnv =
                    let
                        val localEnv = searchList ();
                        fun enterValFn pair = (#enter localEnv pair; #enterVal env pair);
                        val lookupValFn = 
                            lookupDefault (#lookup localEnv)
                                (lookupDefault (#lookup consEnv) (#lookupVal env))
                        fun allValNames () = (stringsOfSearchList localEnv () @ stringsOfSearchList consEnv () @ #allValNames env ())
                        (* We also have to do something similar with types.  This is really
                           only for perverse cases where there is a datatype replication
                           inside the abstype. *)
                        fun enterTypeFn pair = (#enter abstypeEnv pair; #enterType env pair);
                        val lookupTypeFn = 
                            lookupDefault (#lookup abstypeEnv) (#lookupType env)
                    in
                        { 
                            lookupVal     = lookupValFn,
                            lookupType    = lookupTypeFn,
                            lookupFix     = #lookupFix env,
                            lookupStruct  = #lookupStruct env,
                            lookupSig     = #lookupSig env,
                            lookupFunct   = #lookupFunct env,
                            lookupTvars   = #lookupTvars env,
                            enterVal      = enterValFn,
                            enterType     = enterTypeFn,
                            enterFix      = #enterFix env,
                            enterStruct   = #enterStruct env,
                            enterSig      = #enterSig env,
                            enterFunct    = #enterFunct env,
                            allValNames   = allValNames,
                            allStructNames = #allStructNames env
                        }
                    end;
  
                in
                    (* Process the declarations, discarding the result. *)
                    assignSeq (decEnv, letDepth, declist, false);
                    (* Turn off equality outside the with..end block.  This is required by the
                       "Abs" function defined in section 4.9 of the ML Definition.
                       We need to record the equality status, though, because we need
                       to reinstate this for code-generation. *)
                    equalityStatus := List.map tcEquality listOfTypes;
                    List.app(fn tc => tcSetEquality (tc, false)) listOfTypes;
                    ()
                end;
        in
            badInstance (* Does not return a type *)
        end (* assAbsData *)
    in 
        assValues near v
    end (* assignValues *);

      val Env gEnv = env
      
      val env = 
          {
            lookupVal     = #lookupVal gEnv,
            lookupType    = #lookupType gEnv,
            lookupFix     = #lookupFix gEnv,
            lookupStruct  = #lookupStruct gEnv,
            lookupSig     = #lookupSig gEnv,
            lookupFunct   = #lookupFunct gEnv,
            lookupTvars   = fn _ => NONE,
            enterVal      = #enterVal gEnv,
            enterType     = #enterType gEnv,
            enterFix      = #enterFix gEnv,
            enterStruct   = #enterStruct gEnv,
            enterSig      = #enterSig gEnv,
            enterFunct    = #enterFunct gEnv,
            allValNames   = #allValNames gEnv,
            allStructNames = #allStructNames gEnv
          };
    in
      assignValues(1, 0, env, v, v)
    end (* pass2 *);

    (* Types that can be shared. *)
    structure Sharing =
    struct
        type parsetree = parsetree
        and  types = types
        and  parseTypeVar = parseTypeVar
        and  typeId = typeId
        and  lexan = lexan
        and  env = env
        and  instanceType = instanceType
        and  typeParsetree = typeParsetree
        and  typeConstrSet = typeConstrSet
    end

end;
