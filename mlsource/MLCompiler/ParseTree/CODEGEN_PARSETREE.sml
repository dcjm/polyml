(*
    Copyright (c) 2013-2015, 2020, 2025 David C.J. Matthews

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

functor CODEGEN_PARSETREE (
    structure BASEPARSETREE : BaseParseTreeSig
    structure PRINTTREE: PrintParsetreeSig
    structure EXPORTTREE: ExportParsetreeSig
    structure MATCHCOMPILER: MatchCompilerSig
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
    =       EXPORTTREE.Sharing
    =       MATCHCOMPILER.Sharing
    =       LEX.Sharing
    =       CODETREE.Sharing
    =       DEBUGGER.Sharing
    =       TYPETREE.Sharing
    =       TYPEIDCODE.Sharing
    =       STRUCTVALS.Sharing
    =       VALUEOPS.Sharing
    =       DATATYPEREP.Sharing
    =       ADDRESS
): CodegenParsetreeSig =
struct
    open BASEPARSETREE
    open PRINTTREE
    open EXPORTTREE
    open MATCHCOMPILER
    open CODETREE
    open TYPEIDCODE
    open LEX
    open TYPETREE
    open DEBUG
    open STRUCTVALS
    open VALUEOPS
    open MISC
    open DATATYPEREP
    open DEBUGGER

    datatype environEntry = datatype DEBUGGER.environEntry

    (* To simplify passing the context it is wrapped up in this type. *)
    type cgContext =
        {
            decName: string, debugEnv: debuggerStatus, mkAddr: int->int,
            level: level, lex: lexan, lastDebugLine: int ref,
            isOuterLevel: bool (* Used only to decide if we need to report non-exhaustive matches. *)
        }

    fun repDecName decName ({debugEnv, mkAddr, level, lex, lastDebugLine, isOuterLevel, ...}: cgContext) =
        { debugEnv=debugEnv, mkAddr=mkAddr, level=level,
          decName=decName, lex=lex, lastDebugLine=lastDebugLine, isOuterLevel = isOuterLevel}: cgContext
    and repDebugEnv debugEnv ({decName, mkAddr, level, lex, lastDebugLine, isOuterLevel, ...}: cgContext) =
        { debugEnv=debugEnv, mkAddr=mkAddr, level=level,
          decName=decName, lex=lex, lastDebugLine=lastDebugLine, isOuterLevel = isOuterLevel}: cgContext
    (* Create a new level.  Sets isOuterLevel to false. *)
    and repNewLevel(decName, mkAddr, level) ({debugEnv, lex, lastDebugLine, ...}: cgContext) =
        { debugEnv=debugEnv, mkAddr=mkAddr, level=level,
          decName=decName, lex=lex, lastDebugLine=lastDebugLine, isOuterLevel = false}: cgContext

    (* Try this pipeline function *)
    infix |>
    fun a |> f = f a

    val singleArg = mkLoadArgument 0

    (* Make a tuple out of a set of arguments or return the single
       argument if there is just one. *)
    fun mkArgTuple(from, nTuple) =
        if nTuple = 1 (* "tuple" is a singleton *)
        then mkLoadArgument from
        else if nTuple <= 0 then raise InternalError "mkArgTuple"
        else mkTuple(List.tabulate(nTuple, fn n => mkLoadArgument(n+from)))

    (* Load args by selecting from a tuple. *)
    fun loadArgsFromTuple([t], arg) = [(arg, t)](* "tuple" is a singleton *)
    |   loadArgsFromTuple(types, arg) =
            ListPair.zip(List.tabulate(List.length types, fn num => mkInd (num, arg)), types)

    (* tupleWidth returns the width of a tuple or record or 1 if it
       isn't one.  It is used to detect both argument tuples and results.
       When used for arguments the idea is that frequently a tuple is
       used as a way of passing multiple arguments and these can be
       passed on the stack.  When used for results the idea is to
       create the result tuple  on the stack and avoid garbage collector
       and allocator time.  If we could tell that the caller was simply going
       to explode it we would gain but if the caller needed a
       tuple on the heap we wouldn't.  We wouldn't actually lose
       if we were going to create a tuple and return it but we
       would lose if we exploded a tuple here and then created
       a new one in the caller.
       This version of the code assumes that if we create a tuple
       on one branch we're going to create one on others which may
       not be correct. *)
    (* This now returns the argument type for each entry so returns a list rather
       than a number. *)
    (* TODO: The codetree optimiser now handles tuples.  This is still needed for
       floating point.  It seems that this is also needed for top-level
       functions that return tuples since the optimiser looks at the use of the
       tuple rather than how it is created.  This code deals with a lot of
       cases because it is looking at the body to see if it creates a tuple. *)
    fun tupleWidth(TupleTree{expType=ref expType, ...}) = getCodetreeType(SimpleInstance expType)

    |  tupleWidth(Labelled{expType=ref expType, ...}) =
       if recordNotFrozen expType (* An error, but reported elsewhere. *)
       then [GeneralType] (* Safe enough *)
       else getCodetreeType(SimpleInstance expType)

    |  tupleWidth(Cond{thenpt, elsept, ...}) =
        (
            case tupleWidth thenpt of
                [_] => tupleWidth elsept
            |   w => w
        )

    |  tupleWidth(Constraint{value, ...}) = tupleWidth value

    |  tupleWidth(HandleTree{exp, ...}) =
          (* Look only at the expression and ignore
           the handlers on the, possibly erroneous,
           assumption that they won't normally be
           executed. *)
          tupleWidth exp

    |  tupleWidth(Localdec{body=[], ...}) = raise InternalError "tupleWidth: empty localdec"

    |  tupleWidth(Localdec{body, ...}) =
          (* We are only interested in the last expression. *)
          tupleWidth(#1 (List.last body))

    |  tupleWidth(Case{match, ...}) =
        let
            fun getWidth(MatchTree{exp, ...}) = tupleWidth exp
        in
            List.foldl(fn(v, [_]) => getWidth v | (_, s) => s)
                      [GeneralType] match
        end

    |  tupleWidth(Parenthesised(p, _)) = tupleWidth p

    |  tupleWidth(ExpSeq(p, _)) = tupleWidth(#1 (List.last p))

    |  tupleWidth(Ident{ expType=ref(expType, _), ...}) = getCodetreeType expType

    |  tupleWidth(Literal{ expType=ref expType, ...}) = getCodetreeType expType

    |  tupleWidth(Applic{ expType=ref expType, ...}) = getCodetreeType expType

    |  tupleWidth _ = [GeneralType]

    (* Start of the code-generator itself. *)
  
    (* Report unreferenced identifiers. *)

    fun reportUnreferencedValue lex
            (Value{name, references=SOME{exportedRef=ref false, localRef=ref nil, ...}, locations, ...}) =
        let
            fun getDeclLoc (DeclaredAt loc :: _) = loc
            |   getDeclLoc (_ :: locs) = getDeclLoc locs
            |   getDeclLoc [] = nullLocation (* Shouldn't happen. *)
        in
            warningMessage(lex, getDeclLoc locations,
                "Value identifier ("^name^") has not been referenced.")
        end
    |   reportUnreferencedValue _ _ = ()

    (* Process a list of possibly mutually recursive functions and identify those that
       are really referenced. *)
    fun reportUnreferencedValues(valList, lex) =
    let
        fun checkRefs valList =
        let
            fun unReferenced(Value{references=SOME{exportedRef=ref false, localRef=ref nil, ...}, ...}) = true
            |   unReferenced _ = false
            val (unrefed, refed) = List.partition unReferenced valList
            fun update(Value{references=SOME{localRef, recursiveRef, ...}, ...}, changed) =
                let
                    (* If it is referred to by a referenced function it is referenced. *)
                    fun inReferenced(_, refName) = List.exists (fn Value{name, ...} => name=refName) refed
                    val (present, absent) = List.partition inReferenced (!recursiveRef)
                in
                    if null present
                    then changed
                    else
                    (
                        localRef := List.map #1 present @ ! localRef;
                        recursiveRef := absent;
                        true
                    )
                end
            |   update(_, changed) = changed
        in
            (* Repeat until there's no change. *)
            if List.foldl update false unrefed then checkRefs unrefed else ()
        end
    in
        checkRefs valList;
        List.app (reportUnreferencedValue lex) valList
    end

    fun makeDebugEntries (vars: values list, {debugEnv, level, lex, mkAddr, ...}: cgContext) =
    let
        val (code, newDebug) =
            DEBUGGER.makeValDebugEntries(vars, debugEnv, level, lex, mkAddr)
    in
        (code, newDebug)
    end

    (* Add a breakpoint if debugging is enabled.  The bpt argument is set in
       the parsetree so that it can be found by the IDE. *)
    fun addBreakPointCall(bpt, location, {mkAddr, level, lex, debugEnv, ...}) =
    let
        open DEBUGGER
        val (lineCode, newStatus) = updateDebugLocation(debugEnv, location, lex)
        val code = breakPointCode(bpt, location, level, lex, mkAddr)
    in
        (lineCode @ code, newStatus)
    end

    (* In order to build a call stack in the debugger we need to know about
       function entry and exit. *)
    fun wrapFunctionInDebug(codeBody, name, argCode, argType, restype, location, {debugEnv, mkAddr, level, lex, ...}) =
        DEBUGGER.wrapFunctionInDebug(codeBody, name, argCode, argType, restype, location, debugEnv, level, lex, mkAddr)

    (* Create an entry in the static environment for the function. *)
(*    fun debugFunctionEntryCode(name, argCode, argType, location, {debugEnv, mkAddr, level, lex, ...}) =
        DEBUGGER.debugFunctionEntryCode(name, argCode, argType, location, debugEnv, level, lex, mkAddr)*)

    fun isConstructor (Value{class=Constructor _, ...}) = true
    |   isConstructor (Value{class=Exception _, ...})     = true
    |   isConstructor _                                  = false;

    (* Find all the variables declared by each pattern. *)
    fun getVariablesInPatt (Ident {value = ref ident, ...}, varl) =
            (* Ignore constructors *)
            if isConstructor ident then varl else ident :: varl
    |   getVariablesInPatt(TupleTree{fields, ...}, varl) = List.foldl getVariablesInPatt varl fields
    |   getVariablesInPatt(Labelled {recList, ...}, varl) =
            List.foldl (fn ({valOrPat, ...}, vl) => getVariablesInPatt(valOrPat, vl)) varl recList
        (* Application of a constructor: only the argument
            can contain vars. *)
    |   getVariablesInPatt(Applic {arg, ...}, varl) = getVariablesInPatt (arg, varl)
    |   getVariablesInPatt(List{elements, ...}, varl) = List.foldl getVariablesInPatt varl elements
    |   getVariablesInPatt(Constraint {value, ...}, varl) = getVariablesInPatt(value, varl)
    |   getVariablesInPatt(Layered {var, pattern, ...}, varl) =
             (* There may be a constraint on the variable
                so it is easiest to recurse. *)
            getVariablesInPatt(pattern, getVariablesInPatt(var, varl))
    |   getVariablesInPatt(Parenthesised(p, _), varl) = getVariablesInPatt(p, varl)
    |   getVariablesInPatt(_, varl) = varl (* constants and error cases. *);

    fun codeMatch(near, alt : matchtree list, arg,
                  isHandlerMatch, matchContext as { level, mkAddr, lex, ...}): codetree =
    let
        val noOfPats  = length alt
        (* Check for unreferenced variables. *)
        val () =
            if getParameter reportUnreferencedIdsTag (debugParams lex)
            then 
            let
                fun getVars(MatchTree{vars, ...}, l) = getVariablesInPatt(vars, l)
                val allVars = List.foldl getVars [] alt
            in
                List.app (reportUnreferencedValue lex) allVars
            end
            else ()

        val lineNo =
            case alt of
                MatchTree {location, ... } :: _ => location
            | _ => raise Match

        (* Save the argument in a variable. *)
        val decCode   = multipleUses (arg, fn () => mkAddr 1, level);

        (* Generate code to load it. *)
        val loadExpCode = #load decCode level;

        (* Generate a range of addresses for any functions that have to
           be generated for the expressions. *)  
        val baseAddr  = mkAddr noOfPats

        (* We want to avoid the code blowing up if we have a large expression which occurs
           multiple times in the resulting code. 
           e.g. case x of [1,2,3,4] => exp1 | _ => exp2
           Here exp2 will be called at several points in the code.  Most patterns occur
           only once, sometimes a few more times.  The first three times the pattern
           occurs the code is inserted directly.  Further cases are dealt with as
           function calls.  *)
        val insertDirectCount = 3 (* First three cases are inserted directly. *)

        (* Make an array to count the number of references to a pattern.
            This is used to decide whether to use a function for certain
            expressions or to make it inline. *)
        val uses = IntArray.array (noOfPats, 0);

        (* Called when a selection has been made to code-generate the expression. *)
        fun codePatternExpression pattChosenIndex =
        let
            val context = matchContext
            (* Increment the count for this pattern. *)
            val useCount = IntArray.sub(uses, pattChosenIndex) + 1
            val () = IntArray.update (uses, pattChosenIndex, useCount)
            val MatchTree {vars, exp, breakPoint, ... } = List.nth(alt, pattChosenIndex)
        in
            if useCount <= insertDirectCount
            then (* Use the expression directly *)
            let
                (* If debugging add debug entries for the variables then put in a break-point. *)
                val vl = getVariablesInPatt(vars, [])
                val (envDec, varDebugEnv) = makeDebugEntries(vl, context)
                val (bptCode, bptEnv) =
                    addBreakPointCall(breakPoint, getLocation exp, context |> repDebugEnv varDebugEnv)
            in
                mkEnv(envDec @ bptCode, codegen (exp, context |> repDebugEnv bptEnv))
            end
            else
            let (* Put in a call to the expression as a function. *)
                val thisVars    = getVariablesInPatt(vars, [])
                (* Make an argument list from the variables bound in the pattern. *)
                fun makeArg(Value{access=Local{addr=ref lvAddr, ...}, ...}) =
                        mkLoadLocal lvAddr
                |   makeArg _ = raise InternalError "makeArg"
                val argsForCall = List.map makeArg thisVars
            in
                mkEval(mkLoadLocal (baseAddr + pattChosenIndex), argsForCall)
            end
        end

        (* Generate the code and also check for redundancy
           and exhaustiveness. *)
        local
            val cmContext = { mkAddr = mkAddr, level = level, lex = lex }
        in
            val (matchCode, exhaustive) =
                codeMatchPatterns(alt, loadExpCode, isHandlerMatch, lineNo, codePatternExpression, cmContext)
        end

        (* Report inexhaustiveness if necessary.  TODO: It would be nice to have
           some example of a pattern that isn't matched for. *)
        (* If this is a handler we may have set the option to report exhaustiveness.
           This helps in tracking down handlers that don't treat Interrupt specially. *)
        val () = 
            if exhaustive
            then if isHandlerMatch andalso getParameter reportExhaustiveHandlersTag (debugParams lex)
            then errorNear (lex, false, near, lineNo, "Handler catches all exceptions.")
            else ()
            else if isHandlerMatch
            then ()
            else errorNear (lex, false, near, lineNo, "Matches are not exhaustive.")
        (* Report redundant patterns. *)
        local
            fun reportRedundant(patNo, 0) =
                let
                    val MatchTree {location, ... } = List.nth(alt, patNo)
                in
                    errorNear (lex, false, near, location,
                                "Pattern " ^ Int.toString (patNo+1) ^ " is redundant.")
                end
            |   reportRedundant _ = ()
        in
            val () = IntArray.appi reportRedundant uses
        end

        (* Generate functions for expressions that have been used more than 3 times. *)
        fun cgExps([], _, _, _, _, _, _) = []

        |   cgExps (MatchTree {vars, exp, breakPoint, ...} ::al,
                    base, patNo, uses, lex, near, cgContext as { decName, level, ...}) =
            if IntArray.sub(uses, patNo - 1) <= insertDirectCount
            then (* Skip if it has been inserted directly and we don't need a fn. *)
                cgExps(al, base, patNo + 1, uses, lex, near, cgContext)
            else
            let
                val functionLevel = newLevel level (* For the function. *)
                local
                    val addresses = ref 1
                in
                    fun fnMkAddrs n = ! addresses before (addresses := !addresses + n)
                end
    
                val fnContext = cgContext |> repNewLevel(decName, fnMkAddrs, functionLevel)

                (* We have to pass the variables as arguments.  Bind a local variable to the argument
                   so we can set the variable address as a local address. *)
                val pattVars = getVariablesInPatt(vars, [])
                val noOfArgs = length pattVars
                val argumentList = List.tabulate(noOfArgs, mkLoadArgument)
                val localAddresses = List.map(fn _ => fnMkAddrs 1) pattVars (* One address for each argument. *)
                val localDecs = ListPair.mapEq mkDec (localAddresses, argumentList)

                local
                    (* Set the addresses to be suitable for arguments.  At the
                       same time create a debugging environment if required. *)
                    fun setAddr (Value{access=Local{addr=lvAddr, level=lvLevel}, ...}, localAddr) =
                        (lvAddr  := localAddr; lvLevel := functionLevel)
                    |   setAddr _ = raise InternalError "setAddr"
                in
                    val _ = ListPair.appEq setAddr (pattVars, localAddresses)
                end

                (* If debugging add the debug entries for the variables then a break-point. *)
                val (envDec, varDebugEnv) = makeDebugEntries(pattVars, fnContext)
                val (bptCode, bptEnv) =
                    addBreakPointCall(breakPoint, getLocation exp, fnContext |> repDebugEnv varDebugEnv)

                val functionBody =
                    mkEnv(localDecs @ envDec @ bptCode, codegen (exp, fnContext |> repDebugEnv bptEnv))
                val patNoIndex = patNo - 1
            in
                mkDec(base + patNoIndex,
                    mkProc (functionBody, noOfArgs, decName ^ "/" ^ Int.toString patNo, getClosure functionLevel, fnMkAddrs 0)) ::
                    cgExps(al, base, patNo + 1, uses, lex, near, cgContext)
            end

        val expressionFuns =
            cgExps(alt, baseAddr, 1, uses, lex, near, matchContext)
    in
        (* Return the code in a block. *)
        mkEnv (#dec decCode @ expressionFuns, matchCode)
    end (* codeMatch *)

    (* Code-generates a piece of tree.  Returns the code and also the, possibly updated,
       debug context.  This is needed to record the last location that was set in the
       thread data. *)
    and codeGenerate(Ident {value = ref (v as Value{class = Exception{nullary}, ...}), location, ...},
                     { level, lex, debugEnv, ...}) = (* Exception identifier *)
        (codeExFunction (v, level, [], nullary, lex, location), debugEnv)

    |   codeGenerate(Ident {value = ref (v as Value{class = Constructor _, ...}), location, ...},
                     { level, lex, debugEnv, ...}) = (* Constructor identifier *)
        let
            (* The instance type is not necessarily the same as the type
               of the value of the identifier. e.g. in the expression
               1 :: nil, "::" has an instance type of
               int * list int -> list int but the type of "::" is
               'a * 'a list -> 'a list. *)
            (* When using the constructor as a value we just want the second word. *)
            val code = ValueConstructor.extractInjection(codeVal (v, level, [], lex, location))
        in
            (code, debugEnv)
        end

    |   codeGenerate(Ident {value = ref v, expType=ref(_, instanceVars), location, ...},
                     { level, lex, debugEnv, ...}) = (* Value identifier *)
        let
            val code = codeVal (v, level, instanceVars, lex, location)
        in
            (code, debugEnv)
        end
 
    |   codeGenerate(c as Literal{converter, literal, expType=ref expType, location}, { lex, debugEnv, ...}) =
        (
            case getLiteralValue(converter, literal, instanceToType expType, fn s => errorNear(lex, true, c, location, s)) of
                SOME w  => (mkConst w, debugEnv)
              | NONE    => (CodeZero, debugEnv)
        )

    |   codeGenerate(Applic {f = Ident {value = ref function, expType=ref(_, instanceVars), ...}, arg, location, ...}, context as { level, lex, ...}) =
        (* Some functions are special e.g. overloaded and type-specific functions.
           These need to picked out and processed by applyFunction. *)
        let
            val (argCode, argEnv) = codeGenerate (arg, context)
            val code = applyFunction (function, argCode, level, instanceVars, lex, location)
        in
            (code, argEnv)
        end

    |   codeGenerate(Applic {f, arg, ...}, context) =
        let
            val (fnCode, fnEnv) = codeGenerate(f, context)
            val (argCode, argEnv) = codeGenerate(arg, context |> repDebugEnv fnEnv)
        in
            (mkEval (fnCode, [argCode]), argEnv)
        end

    |   codeGenerate(Cond {test, thenpt, elsept, thenBreak, elseBreak, ...}, context) =
        let
            val (testCode, testEnv) = codeGenerate(test, context)
            val (thenBptCode, thenDebug) =
                addBreakPointCall(thenBreak, getLocation thenpt, context |> repDebugEnv testEnv)
            val (thenCode, _) = codeGenerate(thenpt, context |> repDebugEnv thenDebug)
            val (elseBptCode, elseDebug) =
                addBreakPointCall(elseBreak, getLocation elsept, context |> repDebugEnv testEnv)
            val (elseCode, _) = codeGenerate(elsept, context |> repDebugEnv elseDebug)
        in
            (mkIf (testCode, mkEnv(thenBptCode, thenCode), mkEnv(elseBptCode, elseCode)), testEnv)
        end

    |   codeGenerate(TupleTree{fields=[(*pt*)_], ...}, _) =
            (* There was previously a special case to optimise unary tuples but I can't
               understand how they can occur.  Check this and remove the special case
               if it really doesn't. *)
            raise InternalError "codegen: Unary tuple" (*codegen (pt, context)*)

    |   codeGenerate(TupleTree{fields, ...}, context as { debugEnv, ...}) = (* Construct a vector of objects. *)
            (mkTuple(map (fn x => codegen (x, context)) fields), debugEnv)

    |   codeGenerate(Labelled {recList = [{valOrPat, ...}], ...}, context) =
            codeGenerate (valOrPat, context) (* optimise unary records *)

    |   codeGenerate(Labelled {recList, expType=ref expType, ...}, context as { level, mkAddr, debugEnv, ...}) =
        let
            (* We must evaluate the expressions in the order they are
               written. This is not necessarily the order they appear
               in the record. *)
            val recordSize = length recList; (* The size of the record. *)
    
            (* First declare the values as local variables. *)
            (* We work down the list evaluating the expressions and putting
               the results away in temporaries. When we reach the end we
               construct the tuple by asking for each entry in turn. *) 
            fun declist [] look = ([], mkTuple (List.tabulate (recordSize, look)))
      
            |   declist ({name, valOrPat, ...} :: t) look =
                let
                    val thisDec = 
                        multipleUses (codegen (valOrPat, context), fn () => mkAddr 1, level);
        
                    val myPosition = entryNumber (name, expType);
        
                    fun lookFn i =
                        if i = myPosition then #load thisDec (level) else look i
                    val (otherDecs, tuple) = declist t lookFn
                in
                    (#dec thisDec @ otherDecs, tuple)
                end
        in
            (* Create the record and package it up as a block. *)
            (mkEnv (declist recList (fn _ => raise InternalError "missing in record")), debugEnv)
        end

    |   codeGenerate(c as Selector {name, labType, location, ...}, { decName, lex, debugEnv, ...}) =
            let
                (* Check that the type is frozen. *)
                val () =
                    if recordNotFrozen labType
                    then errorNear (lex, true, c, location, "Can't find a fixed record type.")
                    else ();

                val selectorBody : codetree =
                    if recordWidth labType = 1
                    then singleArg (* optimise unary tuples - no indirection! *)
                    else
                    let
                        val offset : int = entryNumber (name, labType);
                    in
                        mkInd (offset, singleArg)
                    end
                val code =(* Make an inline function. *)
                    mkInlproc (selectorBody, 1, decName ^ "#" ^ name, [], 0)
            in
                (code, debugEnv)
            end

    |   codeGenerate(Unit _, { debugEnv, ...}) = (* Use zero.  It is possible to have () = (). *)
            (CodeZero, debugEnv)

    |   codeGenerate(List{elements, location, ...}, context as { level, lex, debugEnv, ...}) =
            let (* Construct a list.  We need to apply the constructors appropriate to the type. *)
                fun consList [] = (* "nil" *)
                        ValueConstructor.extractInjection(
                            codeVal (nilConstructor, level, [], lex, location))
                |   consList (h::t) =
                    let (* :: *)
                        val H = codegen (h, context) and T = consList t
                        val polyVars = []
                    in
                        applyFunction (consConstructor, mkTuple [H,T], level, polyVars, lex, location)
                    end
            in
                (consList elements, debugEnv)
            end

    |   codeGenerate(Constraint {value, ...}, context) = codeGenerate (value, context) (* code gen. the value *)

    |   codeGenerate(c as Fn { location, ... }, context as { debugEnv, ...}) =
            (* Function *)
            (codeLambda(c, location, [], context), debugEnv)

    |   codeGenerate(Localdec {decs, body, ...}, context) =
            (* Local expressions only. Local declarations will be handled
                by codeSequence.*)
            let
                (* This is the continuation called when the declarations have been
                   processed.  We need to ensure that if there are local datatypes
                   we make new entries in the type value cache after them. *)
                (* TODO: This is a bit of a mess.  We want to return the result of the
                   last expression as an expression rather than a codeBinding. *)
                fun processBody (previousDecs: codeBinding list, nextContext as {debugEnv, ...}) =
                let
                    fun codeList ([], d) = ([], d)
                     |  codeList ((p, bpt) :: tl, d) =
                         (* Generate any break point code first, then this entry, then the rest. *)
                        let
                            val (lineChange, newEnv) =
                                addBreakPointCall(bpt, getLocation p, nextContext |> repDebugEnv d)
                            (* addBreakPointCall also updates the location info in case of a break-point
                               or a function call.  We want to pass that along. *)
                            val code = mkNullDec(codegen (p, nextContext |> repDebugEnv newEnv))
                            val (codeRest, finalEnv) = codeList (tl, newEnv)
                        in
                            (lineChange @ [code] @ codeRest, finalEnv)
                        end
                    val (exps, finalDebugEnv) = codeList (body, debugEnv)
                in
                    (previousDecs @ exps, finalDebugEnv)
                end

                val (decs, lastEnv) = codeSequence (decs, [], context, processBody)
            in
                (decSequenceWithFinalExp decs, lastEnv)
            end

    |   codeGenerate(ExpSeq(ptl, _), context as { debugEnv, ...}) =
          (* Sequence of expressions. Discard results of all except the last.*)
            let
                fun codeList ([], _) = raise InternalError "ExpSeq: empty sequence"
                 |  codeList ((p, bpt)::tl, d) =
                    let
                        val (bptCode, newEnv) =
                            addBreakPointCall(bpt, getLocation p, context |> repDebugEnv d)
                        (* Because addBreakPointCall updates the location info in the debug env
                           we need to pass this along in the same way as when making bindings. *)
                        val (thisCode, postCodeEnv) = codeGenerate (p, context |> repDebugEnv newEnv)
                    in
                        case tl of
                            [] => (bptCode, thisCode, postCodeEnv)
                        |   tl =>
                            let
                                val (otherDecs, expCode, postListEnv) = codeList(tl, postCodeEnv)
                            in
                                (bptCode @ (mkNullDec thisCode :: otherDecs), expCode, postListEnv)
                            end
                    end
                val (codeDecs, codeExp, finalEnv) = codeList(ptl, debugEnv)
            in
                (mkEnv (codeDecs, codeExp), finalEnv)
            end

    |   codeGenerate(Raise (pt, location), context as { level, mkAddr, ...}) =
            let
                val (raiseCode, raiseEnv) = codeGenerate(pt, context)
                val {dec, load} = multipleUses (raiseCode, fn () => mkAddr 1, level)
                val load = load level
                (* Copy the identifier, name and argument from the packet and add this location. *)
                val excPacket =
                    mkEnv(dec,
                        mkTuple[mkInd(0, load), mkInd(1, load), mkInd(2, load), codeLocation location])
            in
                (mkRaise excPacket, raiseEnv)
            end

    |   codeGenerate(c as HandleTree {exp, hrules, ...}, context as { debugEnv, mkAddr, ...}) =
          (* Execute an expression in the scope of a handler *)
            let
                val exPacketAddr = mkAddr 1
                val handleExp = codegen (exp, context)          
                val handlerCode = codeMatch (c, hrules, mkLoadLocal exPacketAddr, true, context)
            in
                (mkHandle (handleExp, handlerCode, exPacketAddr), debugEnv)
            end

    |   codeGenerate(While {test, body, breakPoint, ...}, context as { debugEnv, ...}) =
        let
            val (testCode, testEnv) = codeGenerate(test, context)
            val (bptCode, testDebug) =
                addBreakPointCall(breakPoint, getLocation body, context |> repDebugEnv testEnv)
            val (bodyCode, _) = codeGenerate(body, context |> repDebugEnv testDebug)
        in
            (mkWhile (testCode, mkEnv(bptCode, bodyCode)), debugEnv)
        end

    |   codeGenerate(c as Case {test, match, ...}, context as { debugEnv, ...}) =
      (* The matches are made into a series of tests and
         applied to the test expression. *)
        let
            val testCode = codegen (test, context)
        in
            (codeMatch (c, match, testCode, false, context), debugEnv)
        end

    |   codeGenerate(Andalso {first, second, ...}, context) =
        let
            val (firstCode, firstEnv) = codeGenerate(first,  context)
            (* Any updates to the debug context in the first part will carry over
               but we can't be sure whether any of the second part will be executed. *)
            val (secondCode, _) = codeGenerate(second, context |> repDebugEnv firstEnv)
        in
            (* Equivalent to  if first then second else false *)
            (mkCand (firstCode, secondCode), firstEnv)
        end

    |   codeGenerate(Orelse {first, second, ...}, context) =
        let
            val (firstCode, firstEnv) = codeGenerate(first,  context)
            (* Any updates to the debug context in the first part will carry over
               but we can't be sure whether any of the second part will be executed. *)
            val (secondCode, _) = codeGenerate(second, context |> repDebugEnv firstEnv)
        in
          (* Equivalent to  if first then true else second *)
            (mkCor (firstCode, secondCode), firstEnv)
        end

    |   codeGenerate(Parenthesised(p, _), context) = codeGenerate (p, context)

    |   codeGenerate(_, {debugEnv, ...}) = (CodeZero, debugEnv) (* empty and any others *)

    (* Old codegen function which discards the debug context. *)
    and codegen (c: parsetree, context) = #1 (codeGenerate(c, context))

    (* Code-generate a lambda (fn expression). *)
    and codeLambda(c, location, polyVars,
                    cpContext as
                        {mkAddr=originalmkAddr, level=originalLevel, decName, ...}) =
    let
        fun getFnBody (Constraint {value, ...}) = getFnBody value
        |   getFnBody (Fn{matches, ...})  = matches
        |   getFnBody (Parenthesised(p, _)) = getFnBody p
        |   getFnBody _ = raise InternalError "getFnBody: not a constrained fn-expression";

        val f        = getFnBody c;
        (* This function comprises a new declaration level *)
        val nLevel =
            if null polyVars then originalLevel else newLevel originalLevel

        local
            val addresses = ref 1
        in
            fun fnMkAddr n = (! addresses) before (addresses := ! addresses + n)
        end

        val (firstPat, resType, argType) = 
            case f of 
                MatchTree {vars, resType = ref rtype, argType = ref atype, ...} :: _  => (vars, rtype, atype)
            |   _ => raise InternalError "codeLambda: body of fn is not a clause list";

        val tupleSize = List.length(tupleWidth firstPat)
    in
        if tupleSize <> 1 andalso null polyVars
        then
        let
            (* If the first pattern is a tuple we make a tuple from the
               arguments and pass that in. Could possibly treat labelled 
               records in the same way but we have the problem of
               finding the size of the record.
               Currently, we don't apply this optimisation if the function is
               polymorphic. *)
            val newDecName : string = decName ^ "(" ^ Int.toString tupleSize ^ ")";

            val fnLevel  = newLevel nLevel
            val argumentCode = mkArgTuple(0, tupleSize)
            val newContext = cpContext |> repNewLevel(newDecName, fnMkAddr, fnLevel)

            fun codeAlts newDebugEnv =
            let
                val bodyContext = newContext |> repDebugEnv newDebugEnv
            in
                codeMatch (c, f, argumentCode, false, bodyContext)
            end

            val wrap =
                wrapFunctionInDebug(codeAlts, newDecName, argumentCode, argType, resType, location, newContext)
            val mainProc = mkProc(wrap, tupleSize, newDecName, getClosure fnLevel, fnMkAddr 0)
    
            (* Now make a block containing the procedure which expects
               multiple arguments and an inline procedure which expects
               a single tuple argument and calls the main procedure after
               taking the tuple apart. *)
            val thisDec = multipleUses (mainProc, fn () => originalmkAddr 1, originalLevel);

            val resProc =  (* Result procedure. *)
                let
                    val nLevel = newLevel originalLevel
                in
                    mkInlproc 
                        (mkEval(#load thisDec nLevel,
                            List.map #1 (loadArgsFromTuple(List.tabulate(tupleSize, fn _ => GeneralType), singleArg))),
                        1, decName ^ "(1)", getClosure nLevel, 0)
                end
        in
            mkEnv(#dec thisDec, resProc)
        end
    
        else
        let (* No tuple or polymorphic. *)
            val newDecName : string  = decName ^ "(1)";
            val fnLevel  = newLevel nLevel
            val newContext = cpContext |> repNewLevel(newDecName, fnMkAddr, fnLevel)
            
            fun codeAlts newDebugEnv =
            let
                val bodyContext = newContext |> repDebugEnv newDebugEnv
            in
                codeMatch (c, f, mkLoadArgument 0, false, bodyContext)
            end

            (* If we're debugging add the debug info before resetting the level. *)
            val wrapped =
                wrapFunctionInDebug(codeAlts, newDecName, mkLoadArgument 0, argType, resType, location, newContext)
            val pr = mkProc (wrapped, 1, newDecName, getClosure fnLevel,  fnMkAddr 0)
        in
            if null polyVars then pr
            else mkProc(pr, List.length polyVars, newDecName^"(P)", getClosure nLevel, 0)
        end
    end (* codeLambda *)


    (* Code-generates a sequence of declarations. *)
    and codeSequence ([], leading, codeSeqContext, processBody): codeBinding list * debuggerStatus =
            processBody(leading, codeSeqContext) (* Do the continuation. *)

    |   codeSequence ((firstEntry as FunDeclaration {dec, ...}, _) :: pTail, leading, codeSeqContext, processBody) =
        let
            val (firstDec, firstEnv) = codeFunBindings(dec, firstEntry, codeSeqContext)
        in
            codeSequence (pTail, leading @ firstDec, codeSeqContext |> repDebugEnv firstEnv, processBody)
        end

    |   codeSequence ((firstEntry as ValDeclaration {dec, location, ...}, bpt) :: pTail, leading, codeSeqContext as {lex, ...}, processBody) =
        let
            (* Check the types for escaped datatypes. *)
            local
                fun checkVars(ValBind{variables=ref vars, line, ...}) =
                    List.app(fn (Value{typeOf=ValueType(valTypeOf, _), ...}) => checkForEscapingDatatypes(valTypeOf,
                        fn message => errorNear (lex, true, firstEntry, line, message))) vars
            in
                val () = List.app checkVars dec
            end
            
            (* Put in a break point *)
            val (bptCode, bptDbEnv) = addBreakPointCall(bpt, location, codeSeqContext)
            val postBptContext = codeSeqContext |> repDebugEnv bptDbEnv
            (* Split the bindings into recursive and non-recursive.  These have to
               be processed differently. *)
            val (recBindings, nonrecBindings) =
                List.partition(fn ValBind{isRecursive, ...} => isRecursive) dec

            val nonRecCode = codeNonRecValBindings(nonrecBindings, firstEntry, postBptContext)
            val recCode =
                case recBindings of
                    [] => []
                |   _ => #1 (codeRecValBindings(recBindings, firstEntry, postBptContext))
            (* Construct the debugging environment by loading all variables. *)
            val vars = List.foldl(fn (ValBind{variables=ref v, ...}, vars) => v @ vars) [] dec
            val (decEnv, env) = makeDebugEntries (vars, postBptContext)
        in
            codeSequence (pTail, leading @ bptCode @ nonRecCode @ recCode @ decEnv,
                    codeSeqContext |> repDebugEnv env, processBody)
        end

    |   codeSequence ((Localdec {decs, body, varsInBody=ref vars, ...}, _) :: pTail, leading, codeSeqContext, processBody) =
        let (* Local declarations only *)
            (* The debug environment needs to reflect the local...in...end structure but if
               there are local datatypes we need to process all subsequent declarations in the
               scope of the "stopper" we've put onto the typeVarMap. *)
            fun processTail(previous, newContext) =
            let
                (* The debug env for the tail is the original environment together with the
                   variables in the body, excluding variables in the local...in part. *)
                val (decEnv, resEnv) = makeDebugEntries (vars, codeSeqContext) (* Original context. *)
            in
                codeSequence (pTail, previous @ decEnv, newContext |> repDebugEnv resEnv, processBody)
            end
        in
            (* Process the declarations then the tail. *)
            codeSequence (decs @ body, leading, codeSeqContext, processTail)
        end

    |   codeSequence ((ExDeclaration(tlist, _), _) :: pTail, leading,
                      codeSeqContext as {mkAddr, level, lex, ...}, processBody) =
        let
            fun codeEx (ExBind{value=ref exval, previous, ... }) =
            let
                val ex     = exval;
                (* This exception is treated in the same way as a local
                  variable except that the value it contains is created
                  by generating a word on the heap. The address of this word
                  constitutes a unique identifier. Non-generative exception
                  bindings i.e. exception ex=ex'  merely copy the word from
                  the previous exception. *)
                val (lvAddr, lvLevel, exType) =
                    case ex of
                        Value{access=Local{addr, level}, typeOf=ValueType(typeOf, _), ...} => (addr, level, typeOf)
                    |   _ => raise InternalError "lvAddr"
            in
                lvAddr  := mkAddr 1;
                lvLevel := level;
   
                mkDec 
                 (! lvAddr,
                  case previous of
                      EmptyTree => 
                        (* Generate a new exception. This is a single
                           mutable word which acts as a token. It is a
                           mutable to ensure that there is precisely one
                           copy of it. It contains a function to print values
                           of the type so when we raise the exception we can print
                           the exception packet without knowing the type. *)
                        mkExIden (exType, level)
                  | Ident{value=ref prevVal, location, ...} =>
                          (* Copy the previous value. N.B. We want the exception
                           identifier here so we can't call codegen. *)
                        codeVal (prevVal, level, [], lex, location)
                  | _ => raise InternalError "codeEx"
                 )
            end  (* codeEx *);

            val exdecs = map codeEx tlist

            fun getValue(ExBind{value=ref exval, ...}) = exval
            val (debugDecs, newDebugEnv) = makeDebugEntries(map getValue tlist, codeSeqContext)
 
        in
            codeSequence (pTail, leading @ exdecs @ debugDecs, codeSeqContext |> repDebugEnv newDebugEnv, processBody)
        end (* ExDeclaration *)

    |   codeSequence (
            (AbsDatatypeDeclaration {typelist, declist, equalityStatus = ref absEq, isAbsType, withtypes, ...}, _) :: pTail,
            leading, codeSeqContext as {mkAddr, level, debugEnv, lex, ...}, processBody) =
        let (* Code-generate the eq and print functions for the abstype first
               then the declarations, which may use these. *)
            (* The debugging environment for the declarations should include
               the constructors but the result shouldn't.  For the moment
               ignore the constructors. *)
            fun tcEquality(TypeConstrSet(TypeConstrs {identifier,...}, _)) = isEquality identifier
            fun tcSetEquality(TypeConstrs {identifier,...}, eq) = setEquality(identifier, eq)

            val typeCons = List.map(fn (DatatypeBind {tcon = ref tc, ...}) => tc) typelist
            val eqStatus = if isAbsType then absEq else List.map tcEquality typeCons

            local
                fun getConstrCode(DatatypeBind {tcon = ref (tc as TypeConstrSet(_, constrs)), typeVars, ...}, eqStatus) =
                let
                    val arity = List.length typeVars
                    (* Get the argument types or EmptyType if this is nullary. *)
                    fun getConstrType(Value{typeOf=ValueType(FunctionType{arg, ...}, _), name, ...}) = (name, SOME arg)
                    |   getConstrType(Value{name, ...}) = (name, NONE)
                    val constrTypesAndNames = List.map getConstrType constrs
                    val {constrs} = chooseConstrRepr(constrTypesAndNames, arity)
                in
                    ({typeConstr=tc, eqStatus=eqStatus}, constrs)
                end
            in
                val constrAndBoxSizeCode = ListPair.mapEq getConstrCode (typelist, eqStatus)
                val (tcEqBoxSize, constrsCode) = ListPair.unzip constrAndBoxSizeCode
            end

            local
                fun decConstrs(DatatypeBind {tcon = ref (TypeConstrSet(_, constrs)), ...}, reprs, (decs, debugEnv)) =
                let
                    (* Declare the constructors as local variables. *)
                    fun decCons(Value{access=Local{addr, level=lev}, ...}, repr) =
                        let
                            val newAddr = mkAddr 1
                        in
                            addr := newAddr;
                            lev := level;
                            mkDec(newAddr, repr)
                        end
                    |   decCons _ = raise InternalError "decCons: Not local"
                    val constrDecs = ListPair.map decCons (constrs, reprs)
                    val (newDecs, newDebug) =
                        makeDebugEntries(constrs, codeSeqContext |> repDebugEnv debugEnv)
                in
                    (constrDecs @ decs @ newDecs, newDebug)
                end
            in
                val (valConstrDecs: codeBinding list, constrDebugenv: debuggerStatus) =
                    ListPair.foldl decConstrs ([], debugEnv) (typelist, constrsCode)
            end

            val typeFunctions =
                createDatatypeFunctions(tcEqBoxSize, mkAddr, level,
                    getParameter createPrintFunctionsTag (debugParams lex))

            local
                (* Create debug entries for the type constructors and the new type ids. *)
                val (dataTypeDebugDecs, dataTypeDebugEnv) =
                    makeTypeConstrDebugEntries(typeCons, constrDebugenv, level, lex, mkAddr)
                val withTypeTypes = List.map(fn (TypeBind {tcon = ref tc, ...}) => tc) withtypes
                val (withTypeDebugDecs, withTypeDebugEnv) =
                    makeTypeConstrDebugEntries(withTypeTypes, dataTypeDebugEnv, level, lex, mkAddr)
            in
                val typeDebugDecs = dataTypeDebugDecs @ withTypeDebugDecs
                val typeDebugEnv = withTypeDebugEnv
            end

            (* Process the with..end part. We have to restore the equality attribute for abstypes
               here in case getPolymorphism requires it. *)
            val () =
                if isAbsType
                then ListPair.appEq(fn(TypeConstrSet(tc, _), eqt) => tcSetEquality (tc, eqt)) (typeCons, absEq)
                else ()
            val (localDecs, newDebug) =
                codeSequence (declist, [],
                              codeSeqContext |> repDebugEnv typeDebugEnv,
                              fn (code, {debugEnv, ...}) => (code, debugEnv))
            val () =
                if isAbsType
                then List.app(fn TypeConstrSet(tc, _) => tcSetEquality (tc, false)) typeCons else ()

            (* Then the subsequent declarations. *)
            val (tailDecs, finalEnv) =
                codeSequence (pTail, [], codeSeqContext |> repDebugEnv newDebug, processBody)
        in
            (* The code consists of previous declarations, the value constructors, the type IDs,
               debug declarations for the types and value constructors, any type values created for
               subsequent polymorphic calls, declarations in with...end and finally code after
               this declaration within the same "let..in..end" block. *)
            (leading @ valConstrDecs @ typeFunctions @ typeDebugDecs @ localDecs @ tailDecs, finalEnv)
        end

    |   codeSequence ((OpenDec {variables=ref vars, structures = ref structs, typeconstrs = ref types, ...}, _) :: pTail,
                      leading, codeSeqContext as { level, lex, mkAddr, ...}, processBody) =
        let
                (* All we need to do here is make debugging entries. *)
            val (firstDec, firstEnv) = makeDebugEntries(vars, codeSeqContext)
            val (secondDec, secondEnv) = makeTypeConstrDebugEntries(types, firstEnv, level, lex, mkAddr)
            val (thirdDec, thirdEnv) = makeStructDebugEntries(structs, secondEnv, level, lex, mkAddr)
        in
            codeSequence (pTail, leading @ firstDec @ secondDec @ thirdDec, codeSeqContext |> repDebugEnv thirdEnv, processBody)
        end

    |   codeSequence ((TypeDeclaration (typebinds, _), _) :: pTail, leading,
                      codeSeqContext as { debugEnv, level, lex, mkAddr, ...}, processBody) =
        let
            (* Just create debug entries for the type constructors. *)
            val typeCons = List.map(fn (TypeBind {tcon = ref tc, ...}) => tc) typebinds
            val (typeDebugDecs, typeDebugEnv) =
                makeTypeConstrDebugEntries(typeCons, debugEnv, level, lex, mkAddr)
        in
            codeSequence (pTail, leading @ typeDebugDecs, codeSeqContext |> repDebugEnv typeDebugEnv, processBody)
        end

    |   codeSequence (_ :: pTail, leading, (* Directive *) codeSeqContext, processBody) =
            codeSequence (pTail, leading, codeSeqContext, processBody)

    (* Code generate a set of fun bindings.  This is used for other function creation as
       well since it handles the most general case. *)
    and codeFunBindings(tlist: fvalbind list, near,
                        context as {decName, mkAddr, level, lex, ...}) =
        let
            (* Get the function variables. *)
            val functionVars = map (fn(FValBind{functVar = ref var, ...}) => var) tlist

            (* Check the types for escaped datatypes. *)
            local
                fun checkVars(FValBind{functVar=ref(Value{typeOf=ValueType(typeOf, _), ...}), location, ...}) =
                    checkForEscapingDatatypes(typeOf,
                        fn message => errorNear (lex, true, near, location, message))
            in
                val () = List.app checkVars tlist
            end
            (* Each function may result in either one or two functions
               actually being generated. If a function is not curried
               it will generate a single function of one argument, but
               if it is curried (e.g. fun f a b = ...) it will
               generate two mutually recursive functions. A function
               fun f a b = X will be translated into
               val rec f' = fn(a,b) => X and f = fn a => b => f'(a,b)
               with the second function (f) being inline. This allows
               the optimiser to replace references to f with all its
               arguments by f' which avoids building unneccessary
               closures. *)

            fun setValueAddress(
                  FValBind{functVar = ref(Value{access=Local{addr, level}, ...}), ...}, ad, lev) =
                    (addr := ad; level := lev)
            |   setValueAddress _ = raise InternalError "setValueAddress"

            (* Create a list of addresses for the functions.  This is the address
               used for the most general case.  Also set the variable addresses.
               These may be changed for polymorphic functions but will eventually
               be reset. *)

            val addressList = List.map (fn _ => mkAddr 2 (* We need two addresses. *)) tlist
            val () = ListPair.appEq (fn (t, a) => setValueAddress(t, a, level)) (tlist, addressList)

            (* Now we can process the function bindings. *)
            fun loadFunDecs ((fb as FValBind{numOfPatts = ref numOfPats, functVar = ref(Value{name, ...}),
                              clauses, argType = ref aType, resultType = ref resType, location, ...})::otherDecs,
                             addr :: otherAddresses) =
                let
                    (* Make up the function, and if there are several mutually
                       recursive functions, put it in the vector. *)
                    val procName  = decName ^ name;

                    (* Produce a list of the size of any tuples or labelled records
                       in the first clause. Tuples in the first clause are passed as
                       separate arguments. We could look at the other clauses and only
                       pass them as separate arguments if each clause contains a tuple.

                       We can treat labelled records exactly like tuples here - we only
                       need to worry about the mapping from labels to tuple offsets
                       when we create the record (getting the order of evaluation right)
                       and in the pattern-matching code (extracting the right fields).
                       We don't have to worry about that here, because all we're doing
                       is untupling and retupling, taking care always to put the values
                       back at exactly the same offset we got them from. *)
                    val tupleSeq : argumentType list list =
                        case clauses of
                            (FValClause{dec= { args, ...}, ...} :: _) => List.map tupleWidth args
                        |   _ => raise InternalError "badly formed parse tree";

                    local
                        fun getResultTuple(FValClause{exp, ...}) = tupleWidth exp

                        val resultTuples =
                            List.foldl(fn(t, [_]) => getResultTuple t  | (_, s) => s) [GeneralType] clauses

                        (* If we're debugging we want the result of the function so we don't do this optimisation. *)
                        (* The optimiser also detects functions returning tuples and turns them into containers.
                           That works for local functions but doesn't work if the function is exported e.g.
                           IntInf.divMod. *)
                        val resultTuple =
                            if (getParameter debugTag (debugParams lex)) then [GeneralType]
                            else resultTuples
                    in
                        val resTupleLength = List.length resultTuple
                        (*val _ = resTupleLength = 1 orelse raise InternalError "resTupleLength <> 1"*)
                        (* If there's a single argument return the type of that otherwise if we're tupling the
                           result is general. *)
                        val (resultType, extraArg) = case resultTuple of [one] => (one, 0) | _ => (GeneralType, 1)
                    end

                    (* Count the total number of arguments needed. *)
                    val totalArgs = List.foldl (op +) extraArg (List.map List.length tupleSeq)

                    (* The old test was "totalArgs = 1", but that's not really
                       right, because we could have one genuine arg plus a
                       lot of "()" patterns. We now use the normal inlining
                       mechanism to optimise this (unusual) case too. *)
                    val noInlineFunction =
                        numOfPats = 1 andalso totalArgs = 1 andalso tupleSeq = [[GeneralType]] andalso resultType = GeneralType

                    (* Turn the list of clauses into a match. *)
                    fun clauseToTree(FValClause {dec={ args, ...}, exp, line, breakPoint, ...}) =
                        MatchTree
                        {
                            vars =
                                if numOfPats = 1 then hd args
                                else TupleTree{fields=args, location=line, expType=ref BadType},
                            exp  = exp,
                            location = line,
                            argType = ref BadType,
                            resType = ref BadType,
                            breakPoint = breakPoint
                        }
                    val matches = map clauseToTree clauses

                    (* We arrange for the inner function to be called with
                    the curried arguments in reverse order, but the tupled
                    arguments in the normal order. For example, the
                    ML declaration:

                     fun g a b c              = ... gives the order <c,b,a>
                     fun g (a, b, c)          = ... gives the order <a,b,c>
                     fun g (a, b) c (d, e, f) = ... gives the order <d,e,f,c,a,b>

                   We want reverse the order of curried arguments to produce
                   better code. (The last curried argument often gets put
                   into the first argument register by the normal calling
                   mechanism, so we try to ensure that it stays there.)
                   We don't reverse the order of tupled arguments because
                   I'm still a bit confused about when a tuple is an
                   argument tuple (reversed?) and when it isn't (not reversed).

                   Just to add to this, if the function is polymorphic we
                   have to add the polymorphic arguments on at the end.

                 *)
                    local
                        (* Create the argument type list.  I'm sure this can be combined with the
                           next version of makeArgs but it's all too complicated. *)
                        fun makeArgs(parms, []) =
                            let
                                val resTupleSize = resTupleLength
                            in
                                if resTupleSize = 1
                                then parms
                                else parms @ [ContainerType resTupleSize]
                            end
                        |    makeArgs(parms, t::ts) = makeArgs (t @ parms, ts)
                    in
                        val argTypes = makeArgs ([], tupleSeq)
                    end

                    local
                        (* This function comprises a new declaration level *)
                        val nArgTypes = List.length argTypes
                        val fnLevel = newLevel level

                        val argList : codetree =
                            if numOfPats = 1
                            then mkArgTuple(nArgTypes-totalArgs, totalArgs-extraArg)
                            else
                            let
                                fun makeArgs([],  _) = []
                                |   makeArgs(h::t, n) = mkArgTuple(nArgTypes-n-List.length h, List.length h) :: makeArgs(t, n + List.length h)
                            in
                                mkTuple (makeArgs(tupleSeq, extraArg))
                            end

                        local
                            val addresses = ref 1
                        in
                            fun fnMkAddr n = (! addresses) before (addresses := ! addresses + n)
                        end

                        val innerProcName : string = 
                            concat ([procName,  "(" , Int.toString totalArgs, ")"]);

                        val fnContext =
                            context |> repNewLevel(innerProcName, fnMkAddr, fnLevel)

                        (* If we have (mutually) recursive references to polymorphic functions
                           we need to create local versions applied to the polymorphic variables.
                           We only need to consider functions that use the polymorphic variables
                           for this function.  If another function uses different variables it
                           can't be called from this one.  If it had been called from this any
                           type variables would have been fixed as monotypes or the type variables
                           of this function.
                           Except this is wrong in one case.  If one of the recursive calls involves
                           an exception (e.g. f (fn _ => raise Fail "") (or perhaps some other case
                           involving "don't care" polymorphic variables) it is possible to call a
                           function with more polymorphism. *)
                        local
                            fun createApplications(fVal::fVals, addr::addrList, otherDecs) =
                                (
                                    (* Monomorphic functions. *)
                                    setValueAddress(fVal, addr, level);
                                    createApplications(fVals, addrList, otherDecs)
                                )

                            |   createApplications(_, _,decs) = decs
                        in 
                            val appDecs =
                                if noInlineFunction then [] (* This may be directly recursive. *)
                                else createApplications (tlist, addressList, [])
                        end

                        local
                            (* Function body.  The debug state has a "start of function" entry that
                               is used when tracing and points to the arguments.  There are then
                               entries for the recursive functions so they can be used if we
                               break within the function. *)                            
                            fun codeBody fnEntryEnv =
                            let
                                val startContext = fnContext |> repDebugEnv fnEntryEnv
                                (* Create debug entries for recursive references. *)
                                val (recDecs, recDebugEnv) = makeDebugEntries(functionVars, startContext)
                                val bodyContext = fnContext |> repDebugEnv recDebugEnv

                                val codeMatches =
                                    mkEnv(recDecs, codeMatch (near, matches, argList, false, bodyContext))
                            in
                                (* If the result is a tuple we try to avoid creating it by adding
                                   an extra argument to the inline function and setting this to
                                   the result. *)
                                if resTupleLength = 1
                                then codeMatches
                                else
                                    (* The function sets the extra argument to the result
                                       of the body of the function.  We use the last
                                       argument for the container so that
                                       other arguments will be passed in registers in
                                       preference.  Since the container is used for the
                                       result this argument is more likely to have to be
                                       pushed onto the stack within the function than an
                                       argument which may have its last use early on. *)
                                    mkSetContainer(mkLoadParam(nArgTypes-1, fnLevel, fnLevel), codeMatches, resTupleLength)
                            end
                        in
                            (* If we're debugging add the debug info before resetting the level. *)
                            val codeForBody =
                                wrapFunctionInDebug(codeBody, procName, argList, aType, resType, location, fnContext)
                        end

                        val () =
                            if List.length argTypes = totalArgs then () else raise InternalError "Argument length problem"
                    in
                        val innerFun =
                            mkFunction{
                                body=mkEnv(appDecs, codeForBody),
                                argTypes=argTypes, resultType=resultType, name=innerProcName,
                                closure=getClosure fnLevel, numLocals=fnMkAddr 0}
                    end;
  
                    (* We now have a function which can be applied to the
                       arguments once we have them. If the function is curried 
                       we must make a set of nested inline procedures which
                       will take one of the parameters at a time. If all the
                       parameters are provided at once they will be
                       optimised away. *)
                   
                    val polyLevel = level

                    (* Make into curried functions *)
                    fun makeFuns(innerLevel, _, mkParms, []) =
                        let
                            (* Load a reference to the inner function. *)
                            val loadInnerFun = mkLoad (addr + 1, innerLevel, level)
                            val resTupleSize = resTupleLength
                            val parms = mkParms innerLevel
                        in
                            (* Got to the bottom. - put in a call to the procedure. *)
                            if resTupleSize = 1
                            then (mkCall (loadInnerFun, parms, resultType), 0)
                            else (* Create a container for the result, side-effect
                                    it in the function, then create a tuple from it.
                                    Most of the time this will be optimised away. *)
                            let
                                val containerAddr = 0 (* In a new space *)
                                val loadContainer = mkLoadLocal containerAddr
                            in
                                (mkEnv(
                                    [mkContainer(containerAddr, resTupleSize,
                                       mkCall(loadInnerFun, parms @ [(loadContainer, ContainerType resTupleSize)], GeneralType))],
                                    mkTupleFromContainer(containerAddr, resTupleSize)),
                                 containerAddr+1 (* One local *))
                            end
                        end
                |    makeFuns(innerLevel, decName, mkParms, t::ts) =
                        let (* Make a function. *)
                            val nLevel = newLevel innerLevel
                            val newDecName : string = decName ^ "(1)"
                            (* Arguments from this tuple precede older arguments,
                               but order of arguments within the tuple is preserved. *) 
                            fun nextParms l = loadArgsFromTuple(t, mkLoadParam (0, l, nLevel)) @ mkParms l
                            val (body, lCount) = makeFuns (nLevel, newDecName, nextParms, ts)
                        in
                            (mkInlproc (body, 1, newDecName, getClosure nLevel, lCount), 0)
                        end (* end makeFuns *);

                    (* Reset the address of the variable. *)
                    val () = setValueAddress(fb, addr, level)
               in
                    if noInlineFunction
                    then (addr, innerFun) :: loadFunDecs(otherDecs, otherAddresses)
                    else
                    let
                        val (baseFun, _) = makeFuns (polyLevel, procName, fn _ => [], tupleSeq)
                    in
                        (* Return the `inner' procedure and the inline
                          functions as a mutually recursive pair. Try putting
                          the inner function first to see if the optimiser
                          does better this way. *)
                        (addr + 1, innerFun) :: (addr, baseFun) :: loadFunDecs(otherDecs, otherAddresses)
                    end
               end (* loadFunDecs *)
            |   loadFunDecs _ = []

            val loaded = loadFunDecs(tlist, addressList)

            (* Set the final addresses in case they have changed.  N.B.  Do this before
               loading any debug references. *)
            val () = ListPair.appEq (fn (t, a) => setValueAddress(t, a, level)) (tlist, addressList)

            (* Construct the debugging environment for the rest of the scope. *)

            val (decEnv, newDebugEnv) = makeDebugEntries(functionVars, context)
            (* Check whether any of the functions were unreferenced. *)
            val _ =
                if getParameter reportUnreferencedIdsTag (debugParams lex)
                then reportUnreferencedValues(functionVars, lex)
                else ()

        in
            (* Put the declarations into a package of mutual decs. *)
            (mkMutualDecs loaded :: decEnv, newDebugEnv)
        end (* codeFunBindings *)

    (* Recursive val declarations.  Turn them into fun-bindings.  This avoids duplicating a lot
       of code and codeFunBindings does a lot of optimisation. *)
    and codeRecValBindings(valDecs, near, context) =
        let
            (* Turn this into a fun binding. *)
            fun valBindToFvalBind(ValBind{ exp, line, variables=ref vars, ...}, fVals) =
            let
                fun getMatches (Fn { matches: matchtree list, ... })  = matches
                |   getMatches (Constraint {value, ...}) = getMatches value
                |   getMatches (Parenthesised(p, _)) = getMatches p
                |   getMatches _       = raise InternalError "getMatches"

                fun matchTreeToClause(MatchTree{vars, exp, location, breakPoint, ...}) =
                let
                    val dec =
                        { ident = { name="", expType=ref BadType, location=location},
                            isInfix = false, args=[vars], constraint=NONE}
                in
                    FValClause{dec = dec, exp=exp, line=location, breakPoint = breakPoint }
                end
                
                val clauses = List.map matchTreeToClause (getMatches exp)
 
                fun mkFValBind(var as Value{typeOf=ValueType(typeOf, _), ...}) =
                let
                    val argType = mkTypeVar(Generalisable, false)
                    and resultType = mkTypeVar(Generalisable, false)
                    val () =
                        if isSome(unifyTypes(SimpleInstance typeOf, SimpleInstance(mkFunctionType(argType, resultType))))
                        then raise InternalError "mkFValBind"
                        else ()
                in
                    FValBind { clauses=clauses, numOfPatts=ref 1, functVar=ref var,
                               argType=ref argType, resultType=ref resultType, location=line }
                end
            in
                fVals @ List.map mkFValBind vars
            end

            val converted = List.foldl valBindToFvalBind [] valDecs
        in
            codeFunBindings(converted, near, context)
        end (* codeRecValBindings *)

    (* Non-recursive val bindings. *)
    and codeNonRecValBindings(valBindings, near, originalContext: cgContext as { decName, lex, isOuterLevel, ...}) =
        let
            (* Non-recursive val bindings. *)
            fun codeBinding (ValBind{dec=vbDec, exp=vbExp, line, variables=ref vars, ...}, otherDecs) =
            let (* A binding. *)
                (* Get a name for any functions. This is used for profiling and exception trace. *)
                val fName =
                    case vars of [] => "_" | _ => String.concatWith "|" (List.map (fn Value{name, ...} => name) vars)

                val localContext as {level, mkAddr, ...} = originalContext

                val exp = codegen (vbExp, localContext |> repDecName (decName ^ fName ^ "-"))
                (* Save the argument in a variable. *)
                val decCode = multipleUses (exp, fn () => mkAddr 1, level)

                (* Generate the code and also check for redundancy and exhaustiveness. *)
                local
                    val cmContext =
                        { mkAddr = mkAddr, level = level, lex = lex }
                in
                    val (bindCode, exhaustive) =
                        codeBindingPattern(vbDec, #load decCode level, line, cmContext)
                end

                (* Report inexhaustiveness if necessary. *)
                val () =
                    if not exhaustive andalso not isOuterLevel
                    then errorNear (lex, false, near, line, "Pattern is not exhaustive.")
                    else ()

                (* Check for unreferenced variables. *)
                val () =
                    if getParameter reportUnreferencedIdsTag (debugParams lex)
                    then List.app (reportUnreferencedValue lex) (getVariablesInPatt(vbDec, []))
                    else ()
                
                val resultCode = #dec decCode @ bindCode
            in
                otherDecs @ resultCode
            end
        in
            List.foldl codeBinding [] valBindings
        end (* codeNonRecValBindings *)

    (* Code generates the parse tree. *)
    fun gencode
            (pt : parsetree, lex: lexan, debugEnv: debuggerStatus, outerLevel, 
             mkOuterAddresses, structName: string, continuation) : codeBinding list * debuggerStatus =
        codeSequence ([(pt, ref NONE)], [],
            {decName=structName, mkAddr=mkOuterAddresses, level=outerLevel,
             debugEnv=debugEnv, lex=lex, lastDebugLine=ref 0, isOuterLevel = true},
             fn (code: codeBinding list, {debugEnv, ...}) => continuation(code, debugEnv))

    (* Types that can be shared. *)
    structure Sharing =
    struct
        type parsetree = parsetree
        and  lexan = lexan
        and  codetree = codetree
        and  environEntry = environEntry
        and  level = level
        and  codeBinding = codeBinding
        and  debuggerStatus  = debuggerStatus
    end

end;

