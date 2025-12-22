(*
    Copyright (c) 2013, 2016, 2025 David C.J. Matthews

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

functor EXPORT_PARSETREE (

    structure BASEPARSETREE : BaseParseTreeSig
    structure PRINTTREE: PrintParsetreeSig
    structure LEX : LEXSIG
    structure STRUCTVALS : STRUCTVALSIG
    structure EXPORTTREE: EXPORTTREESIG
    structure TYPETREE : TYPETREESIG
    structure DEBUGGER : DEBUGGER

    sharing LEX.Sharing = TYPETREE.Sharing = STRUCTVALS.Sharing
           = EXPORTTREE.Sharing = BASEPARSETREE.Sharing = PRINTTREE.Sharing
           = DEBUGGER.Sharing

): ExportParsetreeSig
=
struct 
    open LEX
    open STRUCTVALS
    open EXPORTTREE
    open TYPETREE
    open BASEPARSETREE
    open PRINTTREE

    (* Build an export tree from the parse tree. *)
    fun typeExportTree(navigation, p: typeParsetree) =
    let        
        val typeof = typeFromTypeParse p

        (* Common properties for navigation and printing. *)
        val commonProps =
            PTprint(fn d => display(SimpleInstance typeof, d, emptyTypeEnv)) ::
            PTtype typeof ::
            exportNavigationProps navigation

        fun asParent () = typeExportTree(navigation, p)
        
    in
        case p of
            ParseTypeConstruction{ location, nameLoc, args, argLoc, ...} =>
            let
                (* If the constructor has been bound return the declaration location.
                   We have to attach the declaration location in the right place if
                   this is a polytype e.g. if we have "int list" here we will have
                   the location for "list" which is the second item not the first. *)
                val (name, decLoc) =
                    case typeof of
                        TypeConstruction { constr=TypeConstrs{locations, ...}, name, ...} =>
                            (name, mapLocationProps locations)
                    |   _ => ("", []) (* Error? *)
                val navNameAndArgs =
                (* Separate cases for nullary, unary and higher type constructions. *)
                    case args of
                        [] => decLoc (* Singleton e.g. int *)
                    |   [oneArg] =>
                        let (* Single arg e.g. int list. *)
                            (* Navigate between the type constructor and the argument.
                               Since the arguments come before the constructor we go there first. *)
                            fun getArg () =
                                typeExportTree({parent=SOME asParent, previous=NONE, next=SOME getName}, oneArg)
                            and getName () =
                                getStringAsTree({parent=SOME asParent, previous=SOME getArg, next=NONE},
                                            name, nameLoc, decLoc)
                        in
                            [PTfirstChild getArg]
                        end
                    |   args =>
                        let (* Multiple arguments e.g. (int, string) pair *)
                            fun getArgs () =
                                (argLoc,
                                    exportList(typeExportTree, SOME getArgs) args @
                                        exportNavigationProps{parent=SOME asParent, previous=NONE, next=SOME getName})
                            and getName () =
                                getStringAsTree({parent=SOME asParent, previous=SOME getArgs, next=NONE},
                                            name, nameLoc, decLoc)
                        in
                            [PTfirstChild getArgs]
                        end
            in
                (location, navNameAndArgs @ commonProps)
            end

        |   ParseTypeProduct{ location, fields, ...} =>
                (location, exportList(typeExportTree, SOME asParent) fields @ commonProps)

        |   ParseTypeFunction{ location, argType, resultType, ...} =>
                (location, exportList(typeExportTree, SOME asParent) [argType, resultType] @ commonProps)

        |   ParseTypeLabelled{ location, fields, ...} =>
            let
                fun exportField(navigation, label as ((name, nameLoc), t, fullLoc)) =
                    let
                        (* The first position is the label, the second the type *)
                        fun asParent () = exportField (navigation, label)
                        fun getLab () =
                            getStringAsTree({parent=SOME asParent, next=SOME getType, previous=NONE},
                                name, nameLoc, [PTtype(typeFromTypeParse t)])
                        and getType () =
                            typeExportTree({parent=SOME asParent, previous=SOME getLab, next=NONE}, t)
                    in
                        (fullLoc, PTfirstChild getLab :: exportNavigationProps navigation)
                    end
            in
                (location, exportList(exportField, SOME asParent) fields @ commonProps)
            end

        |   ParseTypeId{ location, ...} =>
                (location, commonProps)

        |   ParseTypeBad =>
                (nullLocation, commonProps)
    end

    fun getExportTree(navigation, p: parsetree) =
    let
        (* Common properties for navigation and printing. *)
        val commonProps = exportNavigationProps navigation @ [PTprint(fn d => displayParsetree(p, d))]

        fun asParent () = getExportTree(navigation, p)

         (* Put all these into a common list.  That simplifies navigation between
            the various groups in abstypes and datatypes. *)
        datatype lType = DataT of datatypebind | TypeB of typebind | Decl of parsetree * breakPoint option ref
       
        (* Common code for datatypes, abstypes and type bindings. *)
        fun exportTypeBinding(navigation,
                this as DataT(DatatypeBind{name, nameLoc, fullLoc, constrs, tcon=ref(TypeConstrSet(TypeConstrs {locations, ...}, _)), ...})) =
            let
                fun asParent () = exportTypeBinding(navigation, this)
                (* Ignore any type variables before the type name. *)
                fun getName () =
                    getStringAsTree({parent=SOME asParent, previous=NONE, next=SOME getConstrs}, name, nameLoc, 
                                    definingLocationProps locations)
                and getConstrs () =
                    let
                        fun exportConstrs(navigation, {constrName, idLocn, constrVal=ref(Value{locations, ...}), ... }) =
                            (* TODO: the constructor type. *)
                            getStringAsTree(navigation, constrName, idLocn, definingLocationProps locations)
                    in
                        (fullLoc, (* TODO: We need a separate location for the constrs. *)
                            exportList(exportConstrs, SOME asParent) constrs @    
                                exportNavigationProps {parent=SOME asParent, previous=SOME getName, next=NONE})
                    end
            in
                (fullLoc, PTfirstChild getName :: exportNavigationProps navigation)
            end

        |   exportTypeBinding(navigation,
                this as TypeB(TypeBind{name, nameLoc, decType = SOME decType, fullLoc, tcon=ref(TypeConstrSet(TypeConstrs {locations, ...}, _)), ...})) =
            let
                fun asParent () = exportTypeBinding(navigation, this)
                (* Ignore any type variables before the type name. *)
                fun getName () =
                    getStringAsTree({parent=SOME asParent, previous=NONE, next=SOME getType}, name, nameLoc,
                        definingLocationProps locations)
                and getType () =
                    typeExportTree({parent=SOME asParent, previous=SOME getName, next=NONE}, decType)
            in
                (fullLoc, PTfirstChild getName :: exportNavigationProps navigation)
            end

           (* TypeBind is also used in a signature in which case decType could be NONE. *)
        |   exportTypeBinding(navigation,
                this as TypeB(TypeBind{name, nameLoc, decType = NONE, fullLoc, tcon=ref(TypeConstrSet(TypeConstrs {locations, ...}, _)), ...})) =
            let
                fun asParent () = exportTypeBinding(navigation, this)
                (* Ignore any type variables before the type name. *)
                (* Retain this as a child entry in case we decide to add the type vars later. *)
                fun getName () =
                    getStringAsTree({parent=SOME asParent, previous=NONE, next=NONE}, name, nameLoc,
                        definingLocationProps locations)
            in
                (fullLoc, PTfirstChild getName :: exportNavigationProps navigation)
            end

        |   exportTypeBinding(navigation, Decl dec) =
                (* Value declarations in an abstype. *) exportTreeWithBpt(navigation, dec)

        (* In a couple of cases we can have a breakpoint associated with an entry. *)
        and exportTreeWithBpt(nav, (p, ref NONE)) = getExportTree (nav, p)
        |   exportTreeWithBpt(nav, (p, ref (SOME bpt))) =
            let
                val (loc, props) = getExportTree (nav, p)
            in
                (loc, PTbreakPoint bpt :: props)
            end
        
        fun exportMatch(navigation,
                p as MatchTree{location, vars, exp, resType = ref rtype, argType = ref atype, breakPoint = ref bpt, ...}) =
        let
            fun asParent () = exportMatch(navigation, p)
            val debugProp =
                case bpt of
                    NONE => []
                |   SOME bpt => [PTbreakPoint bpt]
        in
            (location,
                [PTprint(fn d => displayMatch(p, d)), PTtype (mkFunctionType (atype, rtype))] @ 
                exportList(getExportTree, SOME asParent) [vars, exp] @
                exportNavigationProps navigation @ debugProp
                )
        end
    in
        case p of
            Ident{location, expType=ref(expType, _), value, possible, ...} =>
            let
                (* Include the type and declaration properties if these
                   have been set. *)
                val (decProp, references, possProp) =
                    case value of
                        ref (Value{name = "<undefined>", ...}) => ([], NONE, [PTcompletions (! possible ())])
                    |   ref (Value{locations, references, ...}) =>
                        let
                            (* If this is in a pattern it could be the defining location of the id.
                               It's complicated trying to find out exactly which is the defining location
                               so we check to see if this is the DeclaredAt location. *)
                            val locProps =
                                case List.find (fn DeclaredAt l => l = location | _ => false) locations of
                                    SOME _ => definingLocationProps locations
                                |   NONE => mapLocationProps locations
                        in
                            (locProps, references, [])
                        end
                val refProp =
                    case references of
                        NONE => []
                    |   SOME {exportedRef=ref exp, localRef=ref locals, recursiveRef=ref recs} =>
                            [PTreferences(exp, List.map #1 recs @ locals)]
            in
                (location, PTtype(instanceToType expType) :: decProp @ commonProps @ refProp @ possProp)
            end

        |   Literal {location, expType=ref expType, ...} => (location, PTtype(instanceToType expType) :: commonProps)

            (* Infixed application.  For the purposes of navigation we treat this as
               three entries in order. *)
        |   Applic{location, f, arg = TupleTree{fields=[left, right], ...}, isInfix = true, expType=ref expType, ...} =>
                (location,
                    PTtype(instanceToType expType) :: exportList(getExportTree, SOME asParent) [left, f, right] @ commonProps)

            (* Non-infixed application. *)
        |   Applic{location, f, arg, expType=ref expType, ...} =>
                (location, PTtype(instanceToType expType) :: exportList(getExportTree, SOME asParent) [f, arg] @ commonProps)

        |   Cond{location, test, thenpt, elsept, thenBreak, elseBreak, ...} =>
                (location,
                    exportList(exportTreeWithBpt, SOME asParent)
                        [(test, ref NONE), (thenpt, thenBreak), (elsept, elseBreak)] @ commonProps)

        |   TupleTree{fields, location, expType=ref expType, ...}=>
                (location, PTtype expType :: exportList(getExportTree, SOME asParent) fields @ commonProps)

        |   ValDeclaration{location, dec, ...}  =>
            let
                fun exportVB(navigation, vb as ValBind{dec, exp, line, ...}) =
                    let
                        val vbProps = exportNavigationProps navigation
                        (* First child should give the pattern *)
                        (* Second child should give the expression *)
                        fun exportThis () = exportVB(navigation, vb)
                        val asChild = exportList(getExportTree, SOME exportThis) [dec, exp]
                    in
                        (line, asChild @ vbProps)
                    end

                val expChild = exportList(exportVB, SOME asParent) dec
            in
                (* We need a special case for a top-level expression.  This has been converted
                   by the parser into val it = exp but the "val it = " takes up no space.
                   We need to go directly to the expression in that case. *)
                case dec of
                    [ValBind{dec=Ident{name="it", location=itLoc, ...}, exp, ...}]
                    => if #startPosition itLoc = #endPosition itLoc andalso
                          #startLine itLoc = #endLine itLoc
                       then getExportTree(navigation, exp)
                       else (location, expChild @ commonProps)
                | _ => (location, expChild @ commonProps)
            end

        |   FunDeclaration{location, dec, ...}  =>
            let
                (* It's easiest to put these all together into a single list. *)
                datatype funEntry =
                    FunIdent of { name: string, expType: types ref, location: location } * values
                |   FunPtree of parsetree
                |   FunConstraint of typeParsetree
                |   FunInfixed of funEntry list * location

                fun exportFunEntry(navigation, FunIdent({expType=ref expType, location, ...}, Value{references, locations, ...})) =
                    let
                        val refProp =
                            case references of
                                NONE => []
                            |   SOME {exportedRef=ref exp, localRef=ref locals, recursiveRef=ref recs} =>
                                    [PTreferences(exp, List.map #1 recs @ locals)] 
                    in
                        (location, refProp @ definingLocationProps locations @ (PTtype expType :: exportNavigationProps navigation))
                    end
                |   exportFunEntry(navigation, FunPtree pt) = getExportTree(navigation, pt)
                |   exportFunEntry(navigation, FunConstraint typ) = typeExportTree(navigation, typ)

                |   exportFunEntry(navigation, this as FunInfixed(inf, location)) =
                    let
                        fun asParent () = exportFunEntry(navigation, this)
                        val expChild = exportList(exportFunEntry, SOME asParent) inf
                    in
                        (location, expChild @ exportNavigationProps navigation)
                    end

                fun exportAClause(
                        FValClause{dec = {ident, isInfix, args, constraint}, exp, breakPoint = ref bpt, ...}, idVal, exportThis) =
                let
                    (* The effect of this is to have all the elements of the clause as
                       a single level except that if we have an infixed application of
                       the function (e.g. fun f o g = ...) then this is a subnode. *)
                    val funAndArgs =
                        case (isInfix, args) of
                            (true, TupleTree{fields=[left, right], location, ...} :: otherArgs) => (* Infixed. *)
                                FunInfixed([FunPtree left, FunIdent(ident, idVal), FunPtree right], location)
                                    :: map FunPtree otherArgs
                        |   (_, args) => (* Normal prefixed form. *)
                                FunIdent(ident, idVal) :: map FunPtree args

                    val constraint = case constraint of NONE => [] |SOME typ => [FunConstraint typ]
                    
                    val debugProp =
                        case bpt of
                            NONE => []
                        |   SOME bpt => [PTbreakPoint bpt]
                in
                    exportList(exportFunEntry, SOME exportThis) (funAndArgs @ constraint @ [FunPtree exp]) @ debugProp
                end

                fun exportFB(navigation,
                        fb as FValBind{clauses=[clause], location, functVar = ref idVal, ...}) =
                    (* If there's just one clause go straight to it.  Otherwise we have an
                       unnecessary level of navigation. *)
                    let
                        val fbProps = exportNavigationProps navigation
                        val asChild = exportAClause(clause, idVal, fn () => exportFB(navigation, fb))
                    in
                        (location, asChild @ fbProps)
                    end
                
                |   exportFB(navigation, fb as FValBind{clauses, location, functVar = ref idVal, ...}) =
                    let
                        val fbProps = exportNavigationProps navigation
                        (* Each child gives a clause. *)
                        (* First child should give the pattern *)
                        (* Second child should give the expression *)
                        fun exportThis () = exportFB(navigation, fb)
                        
                        fun exportClause(navigation, clause as FValClause{ line, ...}) =
                        let
                            val clProps = exportNavigationProps navigation
                            val asChild = exportAClause(clause, idVal, fn () => exportClause(navigation, clause))
                        in
                            (line, asChild @ clProps)    
                        end
                            
                        val asChild = exportList(exportClause, SOME exportThis) clauses
                    in
                        (location, asChild @ fbProps)
                    end

                val expChild = exportList(exportFB, SOME asParent) dec
            in
                (location, expChild @ commonProps)
            end

        |   OpenDec{location, decs, ...} =>
            let
                fun exportStructIdent(navigation, { value, location, ...} ) =
                    let
                        (* Include the declaration properties if it has been set. *)
                        val locProps =
                            case !value of
                                SOME(Struct{locations, ...}) => mapLocationProps locations
                            |   NONE => []
                        val siProps = exportNavigationProps navigation @ locProps
                    in
                        (location, siProps)
                    end

                val expChild = exportList(exportStructIdent, SOME asParent) decs
            in
                (location, expChild @ commonProps)
            end

        |   Constraint{location, value, given, ...} =>
            let
                (* The first position is the expression, the second the type *)
                fun getExpr () =
                    getExportTree({parent=SOME asParent, previous=NONE, next=SOME getType}, value)
                and getType () =
                    typeExportTree({parent=SOME asParent, previous=SOME getExpr, next=NONE}, given)
            in
                (location, PTfirstChild getExpr :: commonProps)
            end

        |   Layered{location, var, pattern, ...} =>
                (location, exportList(getExportTree, SOME asParent) [var, pattern] @ commonProps)

        |   Fn {matches, location, ...} =>
                (location, exportList(exportMatch, SOME asParent) matches @ commonProps)

        |   Localdec{location, decs, body, ...} =>
                (location, exportList(exportTreeWithBpt, SOME asParent) (decs @ body) @ commonProps)

        |   TypeDeclaration(tbl, location) =>
            let
                val allItems = List.map TypeB tbl
            in
                (location, exportList(exportTypeBinding, SOME asParent) allItems @ commonProps)
            end

        |   AbsDatatypeDeclaration { location, typelist, withtypes, declist, ... } =>
            let
                val allItems =
                    List.map DataT typelist @ List.map TypeB withtypes @ List.map Decl declist
            in
                (location, exportList(exportTypeBinding, SOME asParent) allItems @ commonProps)
            end

        |   DatatypeReplication{location, ...} => (* TODO *) (location, commonProps)

        |   ExpSeq(ptl, location) =>
                (location, exportList(exportTreeWithBpt, SOME asParent) ptl @ commonProps)

        |   Directive{location, ...} =>
                (* No need to process the individual identifiers. *)
                (location, commonProps)

        |   ExDeclaration(exbinds, location) =>
            let
                (* There are three possibilities here.  exception exc; exception exc of ty; exception exc = exc' *)
                fun exportExdec(navigation, ExBind{name, previous=EmptyTree, ofType=NONE, nameLoc, value=ref(Value{locations, ...}), ...}) =
                        (* Simple, generative exception with no type. *)
                        getStringAsTree(navigation, name, nameLoc, PTtype exnType :: definingLocationProps locations)

                |   exportExdec(navigation,
                        eb as ExBind{name, previous=EmptyTree, ofType=SOME ofType, nameLoc, fullLoc,
                                     value=ref(Value{locations, ...}), ...}) =
                        (* exception exc of type. *)
                    let
                        fun asParent () = exportExdec (navigation, eb)
                        fun getName () =
                            getStringAsTree({parent=SOME asParent, next=SOME getOfType, previous=NONE},
                                name, nameLoc, (* Type could be in here? *)definingLocationProps locations)
                        and getOfType () =
                            typeExportTree({parent=SOME asParent, previous=SOME getName, next=NONE}, ofType)
                    in
                        (fullLoc, PTfirstChild getName :: exportNavigationProps navigation)
                    end

                |   exportExdec(navigation,
                        eb as ExBind{name, previous, (* ofType=NONE, *) nameLoc, fullLoc, value=ref(Value{locations, ...}), ...}) =
                    let
                        fun asParent () = exportExdec (navigation, eb)
                        fun getName () =
                            getStringAsTree({parent=SOME asParent, next=SOME getPreviousExc, previous=NONE},
                                name, nameLoc, (* Type could be in here? *)definingLocationProps locations)
                        and getPreviousExc () =
                            getExportTree({parent=SOME asParent, previous=SOME getName, next=NONE}, previous)
                    in
                        (fullLoc, PTfirstChild getName :: exportNavigationProps navigation)
                    end

                val expChild = exportList(exportExdec, SOME asParent) exbinds
            in
                (location, expChild @ commonProps)
            end

        |   Raise(raiseExp, location) =>
            let
                fun getExp () = getExportTree({parent=SOME asParent, next=NONE, previous=NONE}, raiseExp)
            in
               (location, [PTfirstChild getExp] @ commonProps)
            end

        |   HandleTree{location, exp, hrules, listLocation, ...} =>
            let
                (* The first position is the expression, the second the matches *)
                fun getExpr () = getExportTree({parent=SOME asParent, previous=NONE, next=SOME getMatches}, exp)
                and getMatches () =
                    (listLocation,
                        exportList(exportMatch, SOME getMatches) hrules @
                            exportNavigationProps{parent=SOME asParent, previous=SOME getExpr, next=NONE})
            in
                (location, [PTfirstChild getExpr] @ commonProps)
            end

        |   While{location, test, body, breakPoint, ...}           =>
                (location,
                    exportList(exportTreeWithBpt, SOME asParent)
                        [(test, ref NONE), (body, breakPoint)] @ commonProps)

        |   Case{location, test, match, listLocation, ...}            =>
            let
                (* The first position is the expression, the second the matches *)
                fun getExpr () = getExportTree({parent=SOME asParent, previous=NONE, next=SOME getMatches}, test)
                and getMatches () =
                    (listLocation,
                        exportList(exportMatch, SOME getMatches) match @
                            exportNavigationProps{parent=SOME asParent, previous=SOME getExpr, next=NONE})
            in
                (location, [PTfirstChild getExpr] @ commonProps)
            end

        |   Andalso {location, first, second, ...} =>
                (location, exportList(getExportTree, SOME asParent) [first, second] @ commonProps)

        |   Orelse{location, first, second, ...} =>
                (location, exportList(getExportTree, SOME asParent) [first, second] @ commonProps)

        |   Labelled{location, expType=ref expType, recList, ...} =>
            let
                (* It's convenient to be able to click on the label part and get
                   the type of the expression or pattern on the right of the '='. *)
                fun exportField(navigation,
                        label as {name, nameLoc, valOrPat, expType=ref expType, fullLocation, ...}) =
                let
                    val patTree as (patLocation, _) = getExportTree(navigation, valOrPat)
                in
                    if patLocation = fullLocation
                    then
                        (* The parser rewrites { name, ...} as { name=name, ... } (more generally
                           { name: ty as pat, ...} as { name = name: ty as pat).
                           To avoid having nodes that overlap we return only the pattern part here. *)
                        patTree
                    else
                    let
                        (* The first position is the label, the second the type *)
                        fun asParent () = exportField (navigation, label)
                        fun getLab () =
                            getStringAsTree({parent=SOME asParent, next=SOME getExp, previous=NONE},
                                name, nameLoc, [PTtype expType])
                        and getExp () =
                            getExportTree({parent=SOME asParent, previous=SOME getLab, next=NONE}, valOrPat)
                    in
                        (fullLocation, PTfirstChild getLab :: exportNavigationProps navigation)
                    end
                end

                val expChild = exportList(exportField, SOME asParent) recList
            in
                (location, PTtype expType :: (expChild @ commonProps))
            end

        |   Selector{location, typeof, ...} => (location, PTtype typeof :: commonProps)

        |   List{elements, location, ...} =>
                (location, exportList(getExportTree, SOME asParent) elements @ commonProps)

        |   EmptyTree                      => (nullLocation, commonProps)

        |   WildCard location              => (location, commonProps)

        |   Unit location                  => (location, PTtype unitType :: commonProps)

        |   Parenthesised(p, _) => getExportTree(navigation, p)
    end
    
    fun getLocation c = #1 (getExportTree({parent=NONE, next=NONE, previous=NONE}, c))

    (* Extract the declaration location from the location list. *)
    fun declaredAt [] = LEX.nullLocation
    |   declaredAt (DeclaredAt loc :: _) = loc
    |   declaredAt (_::l) = declaredAt l

    (* Types that can be shared. *)
    structure Sharing =
    struct
        type lexan = lexan
        and  parsetree = parsetree
        and  matchtree = matchtree
        and  locationProp = locationProp
        and  pretty = pretty
        and  ptProperties = ptProperties
        and  typeParsetree = typeParsetree
    end

end;

