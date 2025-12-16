(*
    Copyright (c) 2013, 2015, 2020 David C.J. Matthews

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

    Title:      Parse Tree Structure and Operations.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985

*)

functor MATCH_COMPILER (
    structure BASEPARSETREE : BaseParseTreeSig
    structure PRINTTREE: PrintParsetreeSig
    structure LEX : LEXSIG
    structure CODETREE : CODETREE
    structure DEBUGGER : DEBUGGER
    structure TYPETREE : TYPETREESIG
    structure TYPEIDCODE: TYPEIDCODESIG
    structure STRUCTVALS : STRUCTVALSIG
    structure VALUEOPS : VALUEOPSSIG
    structure DATATYPEREP: DATATYPEREPSIG
    structure DEBUG: DEBUG


    structure MISC :
    sig
        (* These are handled in the compiler *)
        exception Conversion of string     (* string to int conversion failure *)
  
        (* This isn't handled at all (except generically) *)
        exception InternalError of string (* compiler error *)
    end

    structure ADDRESS : AddressSig

    sharing BASEPARSETREE.Sharing
    =       PRINTTREE.Sharing
    =       LEX.Sharing
    =       CODETREE.Sharing
    =       DEBUGGER.Sharing
    =       TYPETREE.Sharing
    =       TYPEIDCODE.Sharing
    =       STRUCTVALS.Sharing
    =       VALUEOPS.Sharing
    =       DATATYPEREP.Sharing
    =       ADDRESS
): MatchCompilerSig =
struct
    open BASEPARSETREE
    open PRINTTREE
    open CODETREE
    open TYPEIDCODE
    open LEX
    open TYPETREE
    open DEBUG
    open STRUCTVALS
    open VALUEOPS
    open MISC
    open DATATYPEREP

    datatype environEntry = datatype DEBUGGER.environEntry

    type debuggerStatus = DEBUGGER.debuggerStatus

    (* To simplify passing the context it is wrapped up in this type.
       This is a subset of the context used in CODEGEN_PARSETREE. *)
    type matchContext = { mkAddr: int->int, level: level, lex: lexan }
 
    (* Devised by Mike Fourman, Nick Rothwell and me (DCJM).  First coded
       up by Nick Rothwell for the Kit Compiler. First phase of the match
       compiler. The purpose of this phase is to take a match (a set of
       patterns) and bring together the elements that will be discriminated
       by testing any particular part of the value.  Where a pattern is a
       tuple, for example, it is possible to discriminate on each of the
       fields independently, but it may be more efficient to discriminate
       on one of the fields first, and then on the others. The aim is to
       produce a set of tests that discriminate between the patterns 
       quickly. *)
   
    abstype patSet = PatSet of int list

    with           
        (* Each leaf in the tree contains a number which identifies the
           pattern it came from. As well as linking back to the patterns,
           these numbers represent an ordering, because earlier patterns
           mask out later ones. *)
        (* A set of pattern identifiers. *)
        val empty       = PatSet [];
        fun singleton i = PatSet [i];

        fun list (PatSet p) = p;

        infix 3 :::;

        fun a ::: b = PatSet (a :: list b);

        fun isEmptySet (PatSet []) = true | isEmptySet _ = false

        fun first   (PatSet p) = hd p; 
        fun next    (PatSet p) = PatSet (tl p); 

        fun cardinality(PatSet p) = List.length p

        (* Set from i to j inclusive. *)
        fun from i j = if i > j then empty else i ::: from (i + 1) j;

        infix 3 plus;
        infix 4 inside;
        infix 5 intersect;
        infix 6 diff;
        infix 7 eq;
        infix 8 eqSc
        infix 9 neq;

        (* Union of sets. *)
        fun a plus b =
            if isEmptySet a then b
            else if isEmptySet b then a
            else if first a = first b then first a ::: (next a plus next b)
            else if first a < first b then first a ::: (next a plus b)
            else first b ::: (a plus next b);

        (* Set membership. *)
        fun i inside a =
            if isEmptySet a then false
            else if i = first a then true
            else if i < first a then false
            else i inside next a

        (* Intersection of sets. *) 
        fun a intersect b =
            if isEmptySet a orelse isEmptySet b
            then empty
            else if first a = first b 
            then first a ::: ((next a) intersect (next b))
            else if first a < first b 
            then (next a) intersect b
            else a intersect next b;

        (* Set difference. *)
        fun a diff b =
            if isEmptySet a 
            then empty
            else if isEmptySet b
            then a
            else if first a = first b
            then (next a) diff (next b) 
            else if first a < first b
            then first a ::: ((next a) diff b)
            else a diff next b;

        (* Set equality. *)
        fun (PatSet a) eq (PatSet b) = a = b

    end (* patSet *);

    datatype aot = 
        Aot of 
        { 
            patts:    aots,       (* Choices made at this point. *)
            defaults: patSet,     (* Patterns that do not discriminate on this node. *)
            vars:     values list (* The variables bound at this point. *)
        }
                        
    and aots = 
        TupleField of aot list       (* Each element of the list is a field of the tuple. *)
    |   Cons       of consrec list * int   (* List of constructors and the number of different constructors. *)
    |   Excons     of exconsrec list   (* Exception constructors. *)
    |   Scons      of sconsrec list  (* Int, char, string, real. *)
    |   Wild                         (* Patterns that do not discriminate at all. *) 

    (* Datatype constructors and exception constructors. *)
    withtype consrec =
        {
            constructor: values, (* The constructor itself. *)
            patts: patSet,       (* Patterns that use this constructor *)
            appliedTo: aot,      (* Patterns this constructor was applied to. *)
            polyVars: types list (* If this was polymorphic, the matched types. *)
        }

    and exconsrec =
        {
            constructor: values,
            patts: patSet,
            appliedTo: aot,
            exValue: machineWord option
        }

    and sconsrec =
        {
            eqFun:   codetree,    (* Equality functions for this type*)
            specVal: machineWord option,    (* The constant value. NONE here means we had a conversion error. *)
            patts:   patSet       (* Patterns containing this value. *)
        }

    fun makeAot(patts, defaults, vars) =
        Aot 
        { 
            patts    = patts,
            defaults = defaults,
            vars     = vars
        }

    fun makeConsrec(constructor, patts, appliedTo, polyVars): consrec = 
        {
            constructor = constructor,
            patts       = patts, 
            appliedTo   = appliedTo,
            polyVars    = polyVars
        }

    fun makeExconsrec(constructor, patts, appliedTo, exValue): exconsrec = 
        {
            constructor = constructor,
            patts       = patts, 
            appliedTo   = appliedTo,
            exValue     = exValue
        }

    fun makeSconsrec(eqFun, specVal, patts) : sconsrec =
        {
            eqFun    = eqFun,
            specVal  = specVal,
            patts    = patts
        }

    (* An empty wild card - can be expanded as required. *)
    val aotEmpty = makeAot(Wild, empty, [])

    (* A new wild card entry with the same defaults as a previous entry. *)
    fun wild (Aot {defaults, ...}) = makeAot(Wild, defaults, [])

    local
        (* Add a default (wild card or variable) to every node in the tree. *)
        fun addDefault (Aot {patts, defaults, vars}) patNo =
        let
    
            val newPatts =
                case patts of
                    TupleField pl => 
                        TupleField (map (fn a => addDefault a patNo) pl)
            
                |   Cons(cl, width) =>
                    let
                        fun addDefaultToConsrec {constructor, patts, appliedTo, polyVars} =
                            makeConsrec(constructor, patts, addDefault appliedTo patNo, polyVars)
                    in
                        Cons (map addDefaultToConsrec cl, width)
                    end
                     
                |   Excons cl =>
                    let
                        fun addDefaultToExconsrec {constructor, patts, appliedTo, exValue} =
                            makeExconsrec(constructor, patts, addDefault appliedTo patNo, exValue)
                    in
                        Excons (map addDefaultToExconsrec cl)
                    end
          
                |   otherPattern => (* Wild, Scons *) otherPattern
        in
            makeAot(newPatts, defaults plus singleton patNo, vars)
        end (* addDefault *)

        fun addVar (Aot {patts, defaults, vars}) var = makeAot(patts, defaults, var :: vars)

        (* Add a constructor to the tree.  It can only be added to a
           cons node or a wild card. *)
        fun addConstr(cons, noOfConstrs, doArg, tree as Aot {patts = Wild, defaults, vars, ...}, patNo, polyVars) =
            let (* Expand out the wildCard into a constructor node. *)          
                val cr = 
                    makeConsrec(cons, singleton patNo, (* Expand the argument *) doArg (wild tree), polyVars);
            in
                makeAot(Cons([cr], noOfConstrs), defaults, vars)
            end

        |   addConstr(cons as Value{name=consName, ...}, _, doArg, tree as Aot {patts = Cons(pl, width), defaults, vars}, patNo, polyVars) =
            let
                (* Merge this constructor with other occurences. *)
                fun addClist [] = (* Not there - add this on the end. *)
                    [makeConsrec(cons, singleton patNo, doArg (wild tree), polyVars)]
          
                |   addClist ((ccl as {constructor=Value{name=cName, ...}, patts, appliedTo, ... })::ccls) =
                    if cName = consName
                    then (* Merge in. *)
                        makeConsrec(cons, singleton patNo plus patts, doArg appliedTo, polyVars)
                            :: ccls
                    else (* Carry on looking. *) ccl :: addClist ccls;
            in
                makeAot (Cons (addClist pl, width), defaults, vars)
            end

        |   addConstr _ = raise InternalError "addConstr: badly-formed and-or tree"

            (* Add a special constructor to the tree.  Very similar to preceding. *)
        fun addSconstr(eqFun, cval, Aot {patts = Wild, defaults, vars, ...}, patNo, _) =
             (* Expand out the wildCard into a constructor node. *)
            makeAot (Scons [makeSconsrec(eqFun, cval, singleton patNo)], defaults, vars)
            
        |   addSconstr(eqFun, cval, Aot {patts = Scons pl, defaults, vars, ...}, patNo, lex) =
            let (* Must be scons *)
                (* Merge this constructor with other occurrences. *)
                (* Special constants may be overloaded so we don't have a fixed set of types
                   here.  We need to use the type-specific equality function to test.
                   Since only the basis library overloads constants we can assume that
                   eqFun is a constant. *)
                fun equalSpecials(SOME a, SOME b) =
                    let
                        val eqCode = mkEval(eqFun, [mkTuple[mkConst a, mkConst b]])
                    in
                        RunCall.unsafeCast(valOf(evalue(genCode(eqCode, debugParams lex, 0)())))
                    end
                |   equalSpecials _ = false

                fun addClist [] = (* Not there - add this on the end. *)
                        [makeSconsrec(eqFun, cval, singleton patNo)]
                |   addClist ((ccl as { specVal, patts, ...}) :: ccls) =
                        if equalSpecials(cval, specVal)
                        then (* Merge in. *)
                            makeSconsrec(eqFun, cval, singleton patNo plus patts) :: ccls
                        else (* Carry on looking. *) ccl :: addClist ccls
            in
                makeAot (Scons (addClist pl), defaults, vars)
            end

        |   addSconstr _ = raise InternalError "addSconstr: badly-formed and-or tree"

        (* Return the exception id if it is a constant.  It may be a
           top-level exception or it could be in a top-level structure. *)
        local
            fun testAccess(Global code) = evalue code
            |   testAccess(Selected{addr, base}) =
                (
                    case testAccess base of
                        NONE => NONE
                    |   SOME c => evalue(mkInd(addr, mkConst c))
                )
            |   testAccess _ = NONE
        in
            fun exceptionId(Value{access, ...}) = testAccess access
        end

        (* Add an exception constructor to the tree.  Similar to the above
           now that non-constant exceptions are excluded from codePatt. *)
        fun addExconstr(cons, doArg, tree as Aot {patts = Wild, defaults, vars, ...}, patNo) =
                (* Expand out the wildCard into a constructor node. *)
            let
                val cr =
                    makeExconsrec (cons, singleton patNo, doArg(wild tree), exceptionId cons)
            in
                makeAot (Excons [cr], defaults, vars)
            end
    
    
        |   addExconstr(cons, doArg, tree as Aot {patts = Excons cl, defaults, vars, ...}, patNo) =
            let
                (* See if this is a constant. *)
                val newExval = exceptionId cons
                (* Two exceptions can only be considered the same if they are both
                   constants and the same value. *)
                fun sameException(SOME a, SOME b) = PolyML.pointerEq(a, b)
                |   sameException _ = false

                (* It would not be safe to merge exceptions if we were *)
                fun addClist [] = (* Not there - add this on the end. *)
                    [makeExconsrec(cons, singleton patNo, doArg(wild tree), newExval)]

                |   addClist ((ccl as {constructor, patts, appliedTo, exValue, ... })::ccls) =
                    if sameException(newExval, exValue)
                    then (* Merge in. *)
                        makeExconsrec(constructor, singleton patNo plus patts, doArg appliedTo, exValue)
                            :: ccls
                    else (* Carry on looking. *) ccl :: addClist ccls
            in
                makeAot (Excons (addClist cl), defaults, vars)
            end
      
        |   addExconstr _ = raise InternalError "addExconstr: badly-formed and-or tree"
    in

        (* Take a pattern and merge it into an andOrTree. *)
        fun buildAot (Ident {value=ref ident, ... }, tree, patNo, line, context ) =
            let
                val polyVars = []
                fun doArg a = buildAot(WildCard nullLocation, a, patNo, line, context)
            in
                case ident of
                    Value{class=Constructor {ofConstrs, ...}, ...} =>
                      (* Only nullary constructors. Constructors with arguments
                         will be dealt with by ``isApplic'. *)
                        addConstr(ident, ofConstrs, doArg, tree, patNo, polyVars)
                |    Value{class=Exception, ...} =>
                          addExconstr(ident, doArg, tree, patNo)
                |   _ => (* variable - matches everything. Defaults here and pushes a var. *)
                          addVar (addDefault tree patNo) ident
            end

        |   buildAot (TupleTree{fields, location, ...},
                  tree as Aot {patts = Wild, defaults = treeDefaults, vars = treeVars, ...},
                  patNo, _, context) =
                (* Adding tuple to existing wild-card *)
            let
                val tlist = map (fn el => buildAot(el, wild tree, patNo, location, context)) fields
            in
                makeAot (TupleField tlist, treeDefaults, treeVars)
            end

        |   buildAot (TupleTree{fields, ...},
                  Aot {patts = TupleField pl, defaults = treeDefaults, vars = treeVars, ...},
                  patNo, line, context) =
            let (* Adding tuple to existing tuple. *)
                (* Merge each field of the tuple in with the corresponding
                   field of the existing tree. *)
                val tlist =
                    ListPair.mapEq (fn(t, a) => buildAot(t, a, patNo, line, context)) (fields, pl)
            in
                makeAot (TupleField tlist, treeDefaults, treeVars)
            end


        |   buildAot (TupleTree _, _, _, _, _) =
                raise InternalError "pattern is not a tuple in a-o-t"

        |   buildAot (vars as Labelled {recList, expType=ref expType, location, ...},
                      tree, patNo, _, context as { lex, ...}) =
            let
                (* Treat as a tuple, but in the order of the record entries.
                   Missing entries are replaced by wild-cards. The order of
                   the patterns given may bear no relation to the order in
                   the record which will be matched.
                   e.g. case X of (a = 1, ...) => ___ | (b = 2, a = 3) => ___ *)

                (* Check that the type is frozen. *)
                val () =
                    if recordNotFrozen expType
                    then errorNear (lex, true, vars, location,
                          "Can't find a fixed record type.")
                    else ()

                (* Get the maximum number of patterns. *)
                val wilds = List.tabulate(recordWidth expType, fn _ => WildCard nullLocation)

                (* Now REPLACE entries from the actual pattern, leaving
                   the defaulting ones behind. *)
                (* Take a pattern and add it into the list. *)
                fun mergen (_ :: t) 0 pat = pat :: t
                |   mergen (h :: t) n pat = h :: mergen t (n - 1) pat
                |   mergen []       _ _   = raise InternalError "mergen";

                fun enterLabel ({name, valOrPat, ...}, l) = 
                    (* Put this label in the appropriate place in the tree. *)
                    mergen l (entryNumber (name, expType)) valOrPat
      
                val tupleList = List.foldl enterLabel wilds recList
            in
                (* And process it as a tuple. *)
                buildAot(TupleTree{fields=tupleList, location=location, expType=ref expType}, tree, patNo, location, context)
            end

        |   buildAot (Applic{f = Ident{value = ref applVal, ...}, arg, location, ...},
                      tree, patNo, _, context) =
            let
                val polyVars = []
                fun doArg atree = buildAot(arg, atree, patNo, location, context)
            in
                case applVal of
                     Value{class=Constructor{ofConstrs, ...}, ...} =>
                        addConstr(applVal, ofConstrs, doArg, tree, patNo, polyVars)

                |    Value{class=Exception, ...} => addExconstr(applVal, doArg, tree, patNo)

                |    _ => tree (* Only if error *)
            end

        |   buildAot (Applic _ , tree, _, _, _) = tree (* Only if error *)

        |   buildAot (Unit _, tree, patNo, _, _) =
                (* There is only one value so it matches everything. *)
                addDefault tree patNo
      
        |   buildAot (WildCard _, tree, patNo, _, _) = addDefault tree patNo (* matches everything *)
      
        |   buildAot (List{elements, location, expType=ref expType, ...},
                      tree, patNo, _, context) =
            let (* Generate suitable combinations of cons and nil.
                e.g [1,2,3] becomes ::(1, ::(2, ::(3, nil))). *)
                (* Get the base type. *)
                val elementType = mkTypeVar (Generalisable, false)
                val TypeConstrSet(tsConstr, _) = listConstr
                val listType = mkTypeConstruction ("list", tsConstr, [elementType], [DeclaredAt inBasis])
                val _ = unifyTypes(listType, expType)
                val polyVars = [elementType]

                fun processList [] tree = 
                    (* At the end put in a nil constructor. *)
                    addConstr(nilConstructor, 2,
                        fn a => buildAot (WildCard nullLocation, a, patNo, location, context), tree, patNo, polyVars)
                | processList (h :: t) tree = (* Cons node. *)
                    let
                        fun mkConsPat (Aot {patts = TupleField [hPat, tPat], defaults, vars, ...}) =  
                            let   (* The argument is a pair consisting of the
                                     list element and the rest of the list. *)
                                val tlist = [buildAot(h, hPat, patNo, location, context), processList t tPat];
                            in
                                makeAot (TupleField tlist, defaults, vars)
                            end
                       | mkConsPat (tree  as Aot {patts = Wild, defaults, vars, ...}) =  
                            let
                                val hPat  = wild tree;
                                val tPat  = wild tree;
                                val tlist = [buildAot(h, hPat, patNo, location, context), processList t tPat];
                            in
                                makeAot (TupleField tlist, defaults, vars)
                            end
                        | mkConsPat _ = 
                            raise InternalError "mkConsPat: badly-formed parse-tree"
                    in
                        addConstr(consConstructor, 2, mkConsPat, tree, patNo, polyVars)
                    end
                (* end processList *);
            in
                processList elements tree
            end

        |   buildAot (vars as Literal{converter, literal, expType=ref expType, location},
                      tree, patNo, _, {lex, level, ...}) =
            let
                (* At the same time we have to get the equality function
                   for this type to plug into the code.  Literals are overloaded
                   so this may require first resolving the overload to the
                   preferred type. *)
                val constr as TypeConstrs {name=tcName,...} = typeConstrFromOverload expType
                val equality =
                    equalityForType(mkTypeConstruction(tcName, constr, [], []), level)
                val litValue: machineWord option =
                    getLiteralValue(converter, literal, expType, fn s => errorNear(lex, true, vars, location, s))
            in
                addSconstr(equality, litValue, tree, patNo, lex)
             end
    
        |   buildAot (Constraint {value, location, ...}, tree, patNo, _, context) = (* process the pattern *)
                buildAot(value, tree, patNo, location, context)
      
        |   buildAot (Layered {var, pattern, location}, tree, patNo, _, context) =(* process the pattern *)
            let  
                (* A layered pattern may involve a constraint which
                   has to be removed. *)
                fun getVar (Ident {value, ...}) = !value
                |   getVar (Constraint {value, ...}) = getVar value
                |   getVar _ = undefinedValue (* error *)
            in
                addVar (buildAot(pattern, tree, patNo, location, context)) (getVar var)
            end

        |   buildAot (Parenthesised(p, location), tree, patNo, _, context) =
               buildAot(p, tree, patNo, location, context)

        |   buildAot (_, tree, _, _, _) = tree (* error cases *)
    end


    fun buildTree (patts: matchtree list, context) =
    let   (* Merge together all the patterns into a single tree. *)
        fun maket []     _ tree = tree
        |   maket ((MatchTree{vars, location, ...})::t) patNo tree =
                maket t (patNo + 1) (buildAot(vars, tree, patNo, location, context))
    in
        maket patts 1 aotEmpty 
    end
  
    fun bindPattVars(arg, vars, { mkAddr, level, ...}) =
    let
        val addressOfVar = mkAddr 1
        val dec = mkDec (addressOfVar, arg)
        and load = mkLoadLocal addressOfVar

        (* Set the addresses of the variables and create debug entries. *)
        fun setAddr (Value{access=Local{addr=lvAddr, level=lvLevel}, ...}) =
            ( (* Set the address of the variable. *)
                lvAddr  := addressOfVar;
                lvLevel := level
            )

        | setAddr _ = raise InternalError "setAddr"

        val () = List.app setAddr vars
     in
        (load, dec)
     end

    local
        (* Find the "depth" of pattern i.e. the position of
           any defaults. If one of the fields is itself a
           tuple find the maximum depth of its fields, since
           if we decide to discriminate on this field we will
           come back and choose the deepest in that tuple. *)
        fun pattDepth (Aot {patts=TupleField pl, ...}, active) =
            List.foldl (fn (t, d) => Int.max(pattDepth(t, active), d)) 0 pl

        |   pattDepth (Aot {patts, defaults,...}, active) =
            let (* Wild cards, constructors etc. *)
                val activeDefaults = defaults intersect active
            in
                if not (isEmptySet activeDefaults)
                then first activeDefaults
                else
                    (* No default - the depth is the number of
                       patterns that will be discriminated. Apart
                       from Cons which could be a complete match,
                       all the other cases will only occur
                       if the match is not exhaustive. *)
                case patts of 
                    Cons (cl, _) => length cl + 1
                |   Excons cl => length cl + 1
                |   Scons  sl => length sl + 1
                |   _         => 0 (* Error? *)
            end
    in
        fun bestColumn(colsToDo, noOfCols, asTuples, active) =
        let
            fun findDeepest(column, bestcol, depth) =
            if column = noOfCols (* Finished. *)
            then bestcol
            else if column inside colsToDo
            then
            let
                val thisDepth = pattDepth (List.nth(asTuples, column), active)
            in
                if thisDepth > depth
                then findDeepest (column + 1, column, thisDepth)
                else findDeepest (column + 1, bestcol, depth)
            end
            else findDeepest (column + 1, bestcol, depth)
        in
            findDeepest(0, 0, 0)
        end
    end

    (* The result of compiling the pattern match code. *)
    datatype pattCodeOption =
        PattCodeLeaf (* All the discrimination is done. *)
    |   PattCodeBindTuple of (* The value is a tuple - take it apart. *)
            { tupleNo: int, next: pattCode }
    |   PattCodeTupleSelect of (* Select a field of a tuple. *)
            { tupleNo: int, fieldOffset: int, next: pattCode }
    |   PattCodeConstructors of (* Test a set of constructors *)
            {
                nConstrs: int, (* Number of constrs in datatype. 0 = infinite *)
                patterns: (pattCodeConstructor * pattCode) list, (* Constructor and pattern to follow. *)
                default: pattCode (* Pattern if none match *)
            }
    |   PattCodeNaive of (* Do all the discrimination for each pattern separately. *)
            { pattNo: int, tests: (naiveTest * values list) list } list
 
    and pattCodeConstructor =
        PattCodeDatatype of values
    |   PattCodeException of values
    |   PattCodeSpecial of codetree * machineWord option

    and naiveTest =
        NaiveWild
    |   NaiveBindTuple of int
    |   NaiveTupleSelect of { tupleNo: int, fieldOffset: int }
    |   NaivePattTest of pattCodeConstructor
 
    withtype pattCode =
    {
        leafSet: patSet,        (* Set of different patterns fired by the discrimination. *)
        leafCount: int,         (* Count of number of leaves - >= cardinality of leafSet *)
        vars: values list,      (* Variables bound to this node.  May be layered i.e. id as pat *)
        code: pattCodeOption    (* Code to apply at this node. *)
    }

    local
        fun pattCode(Aot {patts, defaults, vars, ...}, active: patSet, nextMatch: patSet * int -> pattCode, tupleNo) =
        let
            (* Get the set of defaults which are active. *)
            val activeDefaults = defaults intersect active

            fun makePattTest(patts, default, nConstrs) =
            let
                (* If we have included all the constructors the default may be
                   redundant. *)
                val nPatts = length patts
                val (initSet, initCount) =
                    if nPatts = nConstrs
                    then (empty, 0)
                    else (#leafSet default, #leafCount default)
                val defaultSet = #leafSet default
                (* If we have a default above a constructor then we may not need to
                   discriminate on the constructor.  This can occur in tuples where
                   we have already discriminated on a different constructor.
                   e.g (1, _) => ...| (_, SOME _) => ... | (_, NONE) => ...
                   The values (1, NONE) and (1, SOME _) will both match the first
                   pattern. *)
                val allSame = List.all (fn (_, { leafSet, ...}) => leafSet eq defaultSet) patts
            in
                if allSame
                then default
                else
                let
                    val unionSet = foldl (fn ((_, {leafSet, ...}), s) => s plus leafSet) initSet patts
                    val leafCount = foldl (fn ((_, {leafCount, ...}), n) => n + leafCount) initCount patts
                    val constrs =
                    {
                        leafSet = unionSet,
                        vars = [],
                        code = PattCodeConstructors{nConstrs = nConstrs, patterns=patts, default=default},
                        leafCount = leafCount
                    }
                in
                    (* If the patterns are blowing up we are better off using naive matching.
                       leafCount indicates the number of different times a pattern is fired.
                       The cardinality of the unionSet is the number of different patterns.
                       In particular we can have pathological cases that really blow up.
                       See Tests/Succeed/Test133.ML. *)
                    if leafCount > 1 andalso leafCount >= cardinality unionSet * 2 - 1
                    then makeNaive constrs
                    else constrs
                end
            end

            val codePatt =
                (* If the active set is empty (match is not exhaustive) or
                   everything will default we can skip further checks.  *)
                if isEmptySet active orelse active eq activeDefaults
                then nextMatch(active, tupleNo)
                else case patts of
                    TupleField [single] =>
                        (* Singleton tuple - this is just the same as the field. *)
                        pattCode(single, active, nextMatch, tupleNo)

                |   TupleField asTuples =>
                    let
                        val thisTuple = tupleNo
                        (* The address is used to refer to this tuple. *)
                        val nextTupleNo = tupleNo+1
                        (* A simple-minded scheme would despatch the first column
                           and then do the others. The scheme used here tries to do
                           better by choosing the column that has any wild card
                           furthest down the column. *)
                        val noOfCols = length asTuples
      
                        fun despatch colsToDo (active, tupleNo) =
                            (* If we have done all the columns we can stop. (Or if
                               the active set is empty). *)
                            if isEmptySet colsToDo orelse isEmptySet active
                            then nextMatch(active, tupleNo)
                            else
                            let
                                (* Choose the best column. *)
                                val bestcol = bestColumn(colsToDo, noOfCols, asTuples, active)
                                (* Discriminate on the constructors in it. *)
                                val code as { leafSet, leafCount, ...} =
                                    pattCode(List.nth(asTuples, bestcol), active,
                                        despatch (colsToDo diff (singleton bestcol)),
                                        tupleNo)
                                (* Code to do the selection. *)
                                val select = PattCodeTupleSelect{tupleNo = thisTuple, fieldOffset = bestcol, next = code }
                            in
                                { leafSet = leafSet, leafCount = leafCount, vars = [], code = select }
                            end
                        val takeApartTuple as { leafSet, leafCount, ...} = despatch (from 0 (noOfCols-1)) (active, nextTupleNo)
                        val code = PattCodeBindTuple { tupleNo=tupleNo, next = takeApartTuple }
                    in
                        { leafSet = leafSet, leafCount = leafCount, vars=[], code=code }
                    end

                |   Cons(cl, width) =>
                    let
                        fun doConstr({ patts, constructor, appliedTo, ...}, rest) =
                            let 
                                (* If this pattern is in the active set
                                   we discriminate on it. *)
                                val newActive = patts intersect active
                            in
                                if isEmptySet newActive
                                then (* No point *) rest
                                else
                                let
                                     val thenCode =
                                        pattCode(appliedTo, newActive plus activeDefaults, nextMatch, tupleNo)
                                in
                                    (PattCodeDatatype constructor, thenCode) :: rest
                               end 
                            end
                        val pattList = foldl doConstr [] cl
                    in
                        makePattTest(pattList, nextMatch(activeDefaults, tupleNo), width)
                    end

                |   Excons cl =>
                    let
                        (* We now process exception constructors in the same way as datatype constructors.
                           This is only valid because all the exception constructors are constants. *)
                        fun doConstr({ patts, constructor, appliedTo, ...}, rest) =
                            let 
                                (* If this pattern is in the active set
                                   we discriminate on it. *)
                                val newActive = patts intersect active
                            in
                                if isEmptySet newActive
                                then (* No point *) rest
                                else
                                let
                                     val thenCode =
                                        pattCode(appliedTo, newActive plus activeDefaults, nextMatch, tupleNo)
                                in
                                    (PattCodeException constructor, thenCode) :: rest
                               end 
                            end
                        val pattList = foldl doConstr [] cl
                    in
                        makePattTest(pattList, nextMatch(activeDefaults, tupleNo), 0)
                    end

                |   Scons sl =>
                    let (* Int, char, string *)
                        (* Generate if..then..else for each of the choices. *)
                        fun doConstr({ patts, eqFun, specVal, ...}, rest) =
                            let 
                                val newActive = patts intersect active
                            in
                                if isEmptySet newActive
                                then (* No point *) rest
                                else (PattCodeSpecial(eqFun, specVal), nextMatch(newActive plus activeDefaults, tupleNo)) :: rest
                            end
                        val pattList = foldl doConstr [] sl
                    in
                        makePattTest(pattList, nextMatch(activeDefaults, tupleNo), 0)
                    end

                |   Wild => nextMatch(activeDefaults, tupleNo)
        in
            { leafSet = #leafSet codePatt, leafCount = #leafCount codePatt, vars=vars @ #vars codePatt, code = #code codePatt }
        end

        (* Turn a decision tree into a series of tests for each pattern. *)
        and makeNaive(pattern as { leafSet, vars, ... }) =
        let
            fun createTests(_, { code = PattCodeLeaf, vars, ...}) = [(NaiveWild, vars)]

            |   createTests(pat, { code = PattCodeBindTuple{ tupleNo, next }, vars, ... }) =
                    (NaiveBindTuple tupleNo, vars) :: createTests(pat, next)

            |   createTests(pat, { code = PattCodeTupleSelect { tupleNo, fieldOffset, next }, vars, ...}) =
                    (NaiveTupleSelect { tupleNo = tupleNo, fieldOffset = fieldOffset }, vars) :: createTests(pat, next)

            |   createTests(pat, { code = PattCodeConstructors { patterns, default, ... }, vars, ...}) =
                    if pat inside #leafSet default (* If it's in the default set we don't discriminate here. *)
                    then (NaiveWild, vars) :: createTests(pat, default)
                    else
                    let
                        (* If it's not in the default it must be in one of the constructors. *)
                        val (constr, code) = valOf(List.find(fn (_, {leafSet, ...}) => pat inside leafSet) patterns)
                    in
                        (NaivePattTest constr, vars) :: createTests(pat, code)
                    end

            |   createTests(pat, { code = PattCodeNaive l, vars, ...}) =
                let
                    val { tests, ...} = valOf(List.find(fn{pattNo, ...} => pat = pattNo) l)
                in
                    (NaiveWild, vars) :: tests
                end

            fun createPatts setToDo =
                if isEmptySet setToDo
                then []
                else
                let
                    val pat = first setToDo
                    val entry = { pattNo = pat, tests = createTests(pat, pattern) }
                    val otherPatts = createPatts(setToDo diff singleton pat)
                in
                    (* Normally we want the patterns in order since earlier ones
                       will generally be more specific.  If 0 is in the set it
                       represents "non-exhaustive" and must go last. *)
                    if pat = 0
                    then otherPatts @ [entry]
                    else entry :: otherPatts
                end
        in
            { leafSet=leafSet, vars=vars, code=PattCodeNaive(createPatts leafSet), leafCount = cardinality leafSet }
        end
    in
        fun buildPatternCode(tree, noOfPats, alwaysNaive) =
        let
            fun firePatt(pattsLeft, _) =
            let
                val pattern =
                    if isEmptySet pattsLeft
                    then 0 (* This represents non-exhaustive. *)
                    else first pattsLeft
            in
                { vars = [], code = PattCodeLeaf, leafSet = singleton pattern, leafCount = 1 }
            end
            
            val patts = pattCode(tree, from 1 noOfPats, firePatt, 0)
        in
            if alwaysNaive
            then makeNaive patts
            else patts
        end
    end

    local
        val EXC_Bind        = 100
        val EXC_Match       = 101
         (* Raises an exception. *)
        fun raiseException(exName, exIden, line) =
            mkRaise (mkTuple [exIden, mkStr exName, CodeZero, codeLocation line]);
        (* Create exception values - Small integer values are used for
           run-time system exceptions. *)
        val bindExceptionVal  = mkConst (ADDRESS.toMachineWord EXC_Bind);
        val matchExceptionVal = mkConst (ADDRESS.toMachineWord EXC_Match);
    in
        (* Raise match and bind exceptions. *)        
        fun raiseBindException line = raiseException("Bind", bindExceptionVal, line)
        and raiseMatchException line = raiseException("Match", matchExceptionVal, line)
    end

    (* Turn the decision tree into real code. *)
    local
        (* Guard and inversion code for constructors *)
        fun constructorCode(PattCodeDatatype cons, arg, {level, ...}) =
                (
                    makeGuard (cons, arg, level),
                    makeInverse (cons, arg, level)
                )
        |   constructorCode(PattCodeException cons, arg, {level, ...}) =
                (
                    makeGuard (cons, arg, level),
                    makeInverse (cons, arg, level)
                )
        |   constructorCode(PattCodeSpecial(eqFun, cval), arg, _) =
                let
                    val constVal = case cval of SOME cv => mkConst cv | NONE => CodeZero
                in
                    (mkEval(eqFun, [mkTuple[arg, constVal]]), CodeZero (* Unused *))
                end

        (* Sequence of tests for naive match. *)
        fun makeNaiveTests([], _, _, _) = CodeTrue

        |   makeNaiveTests ((NaiveWild, _) :: rest, arg, tupleMap, context) = makeNaiveTests(rest, arg, tupleMap, context)

        |   makeNaiveTests ((NaiveBindTuple tupleNo, _) :: rest, arg, tupleMap, context) =
            let
                (* Bind it to a variable.  We don't set the addresses of the vars at this point. *)
                val (declLoad, declDec) = bindPattVars(arg, [], context)
            in
                mkEnv([declDec], makeNaiveTests(rest, arg, (tupleNo, declLoad) :: tupleMap, context))
            end

        |   makeNaiveTests ((NaiveTupleSelect { tupleNo, fieldOffset}, _) :: rest, _, tupleMap, context) =
            let
                val findTuple = List.find(fn(a, _) => tupleNo = a) tupleMap
            in
                makeNaiveTests(rest, mkInd(fieldOffset, #2 (valOf findTuple)), tupleMap, context)
            end

        |   makeNaiveTests ((NaivePattTest constr, _) :: rest, arg, tupleMap, context) =
            let
                (* Bind it to a variable.  This avoids making multiple copies of code. *)
                val (declLoad, declDec) = bindPattVars(arg, [], context)
                val (thisTest, inverse) = constructorCode(constr, declLoad, context)
            in
                mkEnv([declDec], mkCand(thisTest, makeNaiveTests(rest, inverse, tupleMap, context)))
            end

        (* Load all the variables. *)
        fun makeLoads([], _, _, _, _) = []

        |   makeLoads((pattern, vars) :: rest, patNo, arg, tupleMap, context) =
            let
                val (declLoad, declDec) = bindPattVars(arg, vars, context)

                val pattLoad =
                    case pattern of
                        NaiveWild => makeLoads(rest, patNo, declLoad, tupleMap, context)
                    |   NaiveBindTuple tupleNo =>
                            makeLoads(rest, patNo, declLoad, (tupleNo, declLoad) :: tupleMap, context)
                    |   NaiveTupleSelect { tupleNo, fieldOffset} =>
                        let
                            val findTuple = List.find(fn(a, _) => tupleNo = a) tupleMap
                        in
                            makeLoads(rest, patNo, mkInd(fieldOffset, #2 (valOf findTuple)), tupleMap, context)
                        end
                    |   NaivePattTest constr =>
                        let
                            val (_, inverse) = constructorCode(constr, declLoad, context)
                        in
                            makeLoads(rest, patNo, inverse, tupleMap, context)
                        end
            in
                declDec :: pattLoad
            end
    in
        
        fun codeGenerateMatch(patCode, arg, firePatt, context: matchContext as {level, ...}) =
        let
            fun codeMatch({ leafSet, vars, code, ...}, arg, tupleMap) =
            let
                (* Bind the current value to a codetree variable and set the addresses
                   of any ML variables to this. *)
                val (declLoad, declDec) = bindPattVars(arg, vars, context)


                val pattCode =
                    case code of
                        PattCodeLeaf => (* Finished - fire the pattern. *)
                            firePatt(first leafSet)

                    |   PattCodeBindTuple { tupleNo, next }=>
                            (* Bind the tuple number to this address. *)
                            codeMatch(next, arg, (tupleNo, declLoad) :: tupleMap)

                    |   PattCodeTupleSelect { tupleNo, fieldOffset, next } =>
                        let
                            (* The tuple number should be in the map.  Find the address and
                               select the field. *)
                            val findTuple = List.find(fn(a, _) => tupleNo = a) tupleMap
                        in
                            codeMatch(next, mkInd(fieldOffset, #2 (valOf findTuple)), tupleMap)
                        end

                    |   PattCodeConstructors { nConstrs, patterns, default } =>
                        let
                            fun doPattern((PattCodeDatatype cons, code) :: rest, 1) =
                                (* This is the last pattern and we have done all the others.
                                   We don't need to test this one and we don't use the default. *)
                                let
                                    val _ = null rest orelse raise InternalError "doPattern: not at end"
                                    val invertCode = makeInverse (cons, declLoad, level)
                                in
                                    codeMatch(code, invertCode, tupleMap)
                                end

                            |   doPattern([], _) = (* We've done all of them - do the default *)
                                    codeMatch(default, arg, tupleMap)

                            |   doPattern((constructor, matchCode) :: next, constrsLeft) =
                                let
                                    val (testCode, invertCode) = constructorCode(constructor, declLoad, context)
                                    val thenCode = codeMatch(matchCode, invertCode, tupleMap)
                                in
                                    mkIf(testCode, thenCode, doPattern(next, constrsLeft-1))
                                end
                        in
                            doPattern(patterns, nConstrs)
                        end

                    |   PattCodeNaive patterns =>
                        let

                            fun makePatterns [] = raise InternalError "makeTests: empty"
                            |   makePatterns ({ tests, pattNo} :: rest) =
                                let
                                    val pattDecs = makeLoads(tests, pattNo, arg, tupleMap, context)
                                    val pattCode = mkEnv(pattDecs, firePatt pattNo)
                                in
                                    (* If this is the last one there's no need for a test. *)
                                    if null rest
                                    then pattCode
                                    else mkIf(makeNaiveTests(tests, arg, tupleMap, context), pattCode, makePatterns rest)
                                end
                        in
                            makePatterns patterns
                        end
            in
                mkEnv([declDec], pattCode)
            end
        in
            codeMatch(patCode, arg, [])
        end

        (* Binding.  This should be a single naive match.  Generally it will be exhaustive
           so we will only have to load the variables. *)
        fun codeBinding(
                { leafSet, vars, 
                    code = PattCodeNaive({ tests, ...} :: _ (* Normally nil but could be PattCodeWild if non-exhaustive *)), ...}, 
                arg, line, context) =
            let
                (* Bind this to a variable and set any top-level variable(s). *)
                val (declLoad, declDec) = bindPattVars(arg, vars, context)
                (* Create any test code to raise the bind exception *)
                val testCode =
                    if not (0 inside leafSet)
                    then [] (* Exhaustive - no test needed. *)
                    else [mkNullDec(mkIf(makeNaiveTests(tests, declLoad, [], context), CodeZero, raiseBindException line))]
                (* Load the variables.  *)
                val pattDecs = makeLoads(tests, 1, declLoad, [], context)
            in
                declDec :: testCode @ pattDecs
            end

        |   codeBinding _ = raise InternalError "codeBinding: should be naive pattern match"
    end

    fun containsNonConstException(Aot{patts = TupleField fields, ...}) =
        List.foldl(fn (aot, t) => t orelse containsNonConstException aot) false fields

    |   containsNonConstException(Aot{patts = Cons(cl, _), ...}) =
            List.foldl(fn ({appliedTo, ...}, t) => t orelse containsNonConstException appliedTo) false cl

    |   containsNonConstException(Aot{patts = Excons cl, ...}) =
            List.foldl(fn ({appliedTo, exValue, ...}, t) =>
                t orelse not (isSome exValue) orelse containsNonConstException appliedTo) false cl

    |   containsNonConstException _ = false (* Scons or Wild *)

    (* Process a pattern in a binding. *)
    (* This previously used codePatt with special options to generate the correct
       structure for a binding.  This does the test separately from loading
       the variables.  If the pattern is not exhaustive this may do more work
       since the pattern is taken apart both in the test and for loading.  *)
    fun codeBindingPattern(vbDec, arg, line, context) =
    let
        (* Build the tree. *)
        val andortree = buildAot(vbDec, aotEmpty, 1, line, context)
        (* Build the pattern code *)
        val patternCode as { leafSet, ... } = buildPatternCode(andortree, 1, true (* Always *))
        (* It's not exhaustive if pattern zero is in the set. *)
        val exhaustive = not (0 inside leafSet)
        
        val codeDecs = codeBinding(patternCode, arg, line, context)
    in
        (codeDecs, exhaustive)
    end

    (* Process a set of patterns in a match. *)
    (* Naive match code.  Doesn't check for exhaustiveness or redundancy. *)
    fun codeMatchPatterns(alt, arg, isHandlerMatch, lineNo, codePatternExpression, context as { lex, ...}) =
    let
        val noOfPats  = length alt
        val andortree = buildTree(alt, context)
        (* If the match is sparse or there are any non-constant exceptions we
           need to use pattern-by-pattern matching.  Non-constant exceptions
           could involve exception aliasing and this complicates pattern
           matching.  It could break the rule that says that if a value
           matches one constructor it cannot then match any other.
           If we are compiling with debugging we also use the naive
           match.  *)
        val alwaysNaive = containsNonConstException andortree 
            orelse getParameter debugTag (debugParams lex)
        val patternCode as { leafSet, ... } = buildPatternCode(andortree, noOfPats, alwaysNaive)
        (* It's not exhaustive if pattern zero is in the set. *)
        val exhaustive = not (0 inside leafSet)

        fun firePatt 0 =
        (
            exhaustive andalso raise InternalError "codeDefault called but exhaustive";
            if isHandlerMatch
            then mkRaise arg
            else raiseMatchException lineNo
        )
        |   firePatt pattChosen = codePatternExpression(pattChosen - 1)
    in
        (codeGenerateMatch(patternCode, arg, firePatt, context), exhaustive)
    end

    (* Types that can be shared. *)
    structure Sharing =
    struct
        type parsetree = parsetree
        type level = level
        type codetree = codetree
        type matchtree = matchtree
        type codeBinding = codeBinding
        type lexan = lexan
    end

end;

