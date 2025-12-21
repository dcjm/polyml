(*
    Title:      Source level debugger for Poly/ML
    Author:     David Matthews
    Copyright  (c)   David Matthews 2000, 2014, 2015, 2020, 2025

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

functor DEBUGGER_ (

    structure STRUCTVALS : STRUCTVALSIG
    structure VALUEOPS : VALUEOPSSIG
    structure CODETREE : CODETREE
    structure TYPETREE: TYPETREESIG
    structure ADDRESS : AddressSig
    structure COPIER: COPIERSIG
    structure TYPEIDCODE: TYPEIDCODESIG
    structure LEX : LEXSIG
    structure DEBUG: DEBUG

    structure UTILITIES :
    sig
        val splitString: string -> { first:string,second:string }
    end

sharing STRUCTVALS.Sharing = VALUEOPS.Sharing = TYPETREE.Sharing = COPIER.Sharing =
        TYPEIDCODE.Sharing = CODETREE.Sharing = ADDRESS = LEX.Sharing
)
: DEBUGGER
=
struct
    open STRUCTVALS VALUEOPS CODETREE COPIER TYPETREE DEBUG

    (* The static environment contains these kinds of entries. *)
    datatype environEntry =
        EnvValue of string * valueType * locationProp list
    |   EnvException of string * valueType * locationProp list
    |   EnvVConstr of string * valueType * bool * int * locationProp list
    |   EnvTypeid of { original: typeId, freeId: typeId }
    |   EnvStructure of string * signatures * locationProp list
    |   EnvTConstr of string * typeConstrSet
    |   EnvStartFunction of string * location * types
    |   EnvEndFunction of string * location * types

    local
        open ADDRESS
    in
        (* Entries in the thread data.  The RTS allocates enough space for this.
           The first entry is 5 because earlier entries are used by Thread.Thread. *)
        val threadIdStack           = mkConst(toMachineWord 0w5) (* The static/dynamic/location entries for calling fns *)
        and threadIdCurrentStatic   = mkConst(toMachineWord 0w6) (* The static info for bindings i.e. name/type. *)
        and threadIdCurrentDynamic  = mkConst(toMachineWord 0w7) (* Dynamic infor for bindings i.e. actual run-time value. *)
        and threadIdCurrentLocation = mkConst(toMachineWord 0w8) (* Location in code: line number/offset etc. *)
        
        (* Global function entries.  These could be in storage allocated by the RTS. *)
        (* Specialised option type here.  Because a function is always boxed this
           avoids the need for an extra level of indirection. *)
        datatype ('a, 'b) functionOpt = NoFunction | AFunction of 'a -> 'b
        val globalOnEntry       = ref NoFunction
        and globalOnExit        = ref NoFunction
        and globalOnExitExc     = ref NoFunction
        and globalOnBreakPoint  = ref NoFunction

        val onEntryCode =
            mkLoadOperation(LoadStoreMLWord{isImmutable=false}, mkConst(toMachineWord globalOnEntry), CodeZero)
        and onExitCode =
            mkLoadOperation(LoadStoreMLWord{isImmutable=false}, mkConst(toMachineWord globalOnExit), CodeZero)
        and onExitExcCode =
            mkLoadOperation(LoadStoreMLWord{isImmutable=false}, mkConst(toMachineWord globalOnExitExc), CodeZero)
        and onBreakPointCode =
            mkLoadOperation(LoadStoreMLWord{isImmutable=false}, mkConst(toMachineWord globalOnBreakPoint), CodeZero)

        (* We need to ensure that any break-point code preserves the state.  It could be modified
           if we hit a break-point and run the interactive debugger with PolyML.Compiler.debug true. *)
        fun wrap (f:'a -> unit) (x: 'a) : unit =
        let
            val threadId: address = RunCall.unsafeCast(Thread.Thread.self())
            val stack = loadWord(threadId, 0w5)
            and static = loadWord(threadId, 0w6)
            and dynamic = loadWord(threadId, 0w7)
            and location = loadWord(threadId, 0w8)

            fun restore () =
            (
                assignWord(threadId, 0w5, stack);
                assignWord(threadId, 0w6, static);
                assignWord(threadId, 0w7, dynamic);
                assignWord(threadId, 0w8, location)
            )
        in
            f x handle exn => (restore(); PolyML.Exception.reraise exn);
            restore()
        end

        fun setOnEntry NONE = globalOnEntry := NoFunction
        |   setOnEntry (SOME(f: string * PolyML.location -> unit)) = globalOnEntry := AFunction (wrap f)

        and setOnExit NONE = globalOnExit := NoFunction
        |   setOnExit (SOME(f: string * PolyML.location -> unit)) = globalOnExit := AFunction (wrap f)

        and setOnExitException NONE = globalOnExitExc := NoFunction
        |   setOnExitException (SOME(f: string * PolyML.location -> exn -> unit)) =
                globalOnExitExc := AFunction (fn x => wrap (f x))

        and setOnBreakPoint NONE = globalOnBreakPoint := NoFunction
        |   setOnBreakPoint (SOME(f: PolyML.location * bool ref -> unit)) = globalOnBreakPoint := AFunction (wrap f)
    end

    

    (* When stopped at a break-point any Bound ids must be replaced by Free ids.
       We make new Free ids at this point.  *)
    fun envTypeId (id as TypeId{ description, idKind = Bound{arity, ...}, ...}) =
            EnvTypeid { original = id, freeId = makeFreeId(arity, Global CodeZero, isEquality id, description) }
    |   envTypeId id = EnvTypeid { original = id, freeId = id }

    fun searchEnvs match (staticEntry :: statics, dlist as dynamicEntry :: dynamics) =
    (
        case (match (staticEntry, dynamicEntry), staticEntry) of
            (SOME result, _) => SOME result
        |   (NONE, EnvTypeid _) => searchEnvs match (statics, dynamics)
        |   (NONE, EnvVConstr _) => searchEnvs match (statics, dynamics)
        |   (NONE, EnvValue _) => searchEnvs match (statics, dynamics)
        |   (NONE, EnvException _) => searchEnvs match (statics, dynamics)
        |   (NONE, EnvStructure _) => searchEnvs match (statics, dynamics)
        |   (NONE, EnvStartFunction _) => searchEnvs match (statics, dynamics)
        |   (NONE, EnvEndFunction _) => searchEnvs match (statics, dynamics)
                (* EnvTConstr doesn't have an entry in the dynamic list *)
        |   (NONE, EnvTConstr _) => searchEnvs match (statics, dlist)
            
    )
    
    |   searchEnvs _ _ = NONE
        (* N.B.  It is possible to have ([EnvTConstr ...], []) in the arguments so we can't treat
           that if either the static or dynamic list is nil and the other non-nil as an error. *)

    (* Exported functions that appear in PolyML.DebuggerInterface. *)
    type debugState = environEntry list * machineWord list * location

    fun searchType ((clist, rlist, _): debugState) typeid =
    let
        fun match (EnvTypeid{original, freeId }, valu) =
            if sameTypeId(original, typeid)
            then 
                case freeId of
                    TypeId{description, idKind as Free _, ...} =>
                        (* This can occur for datatypes inside functions. *)
                        SOME(TypeId { access= Global(mkConst valu), idKind=idKind, description=description})
                |   _ => raise Misc.InternalError "searchType: TypeFunction"
            else NONE
        |   match _ = NONE
    in
        case (searchEnvs match (clist, rlist), typeid) of
            (SOME t, _) => t
        |   (NONE, TypeId{description, idKind as TypeFn _, ...}) =>
                TypeId { access=Global CodeZero, description = description, idKind = idKind}

        |   (NONE, typeid as TypeId{description, idKind = Bound{arity, ...}, ...}) =>
                (* The type ID is missing.  Make a new temporary ID. *)
                makeFreeId(arity, Global(TYPEIDCODE.codeForUniqueId()), isEquality typeid, description)

        |   (NONE, typeid as TypeId{description, idKind = Free{arity, ...}, ...}) =>
                (* The type ID is missing.  Make a new temporary ID. *)
                makeFreeId(arity, Global(TYPEIDCODE.codeForUniqueId()), isEquality typeid, description)

    end

    (* Values must be copied so that compile-time type IDs are replaced by their run-time values. *)
    fun makeTypeConstr (state: debugState) (TypeConstrSet(TypeConstrs {identifier, name, locations, ...}, (*tcConstructors*) _)) =
        let
            val typeID = searchType state identifier
            val newTypeCons =
                TypeConstrs { name = name, identifier = typeID, locations = locations }

            val newValConstrs = (*map copyAConstructor tcConstructors*) []
        in
            TypeConstrSet(newTypeCons, newValConstrs)
        end

    (* When creating a structure we have to add a type map that will look up the bound Ids. *)
    fun makeStructure state (name, rSig, locations, valu) =
    let
        local
            val Signatures{ name = sigName, tab, typeIdMap, firstBoundIndex, locations=sigLocs, ... } = rSig
            fun getFreeId n = searchType state (makeBoundId(0 (* ??? *), Global CodeZero, n, false, false, basisDescription ""))
        in
            val newSig = makeSignature(sigName, tab, firstBoundIndex, sigLocs, composeMaps(typeIdMap, getFreeId), [])
        end
    in
        makeGlobalStruct (name, newSig, mkConst valu, locations)
    end

    local
        fun runTimeType (state: debugState) (ValueType(ty, templates)) =
        let
            fun copyId(TypeId{idKind=Free _, access=Global _ , ...}) = NONE (* Use original *)
            |   copyId id = SOME(searchType state id)
        in
            ValueType(copyType (ty, fn _ => NONE,
                    fn tcon => copyTypeConstr (tcon, copyId, fn s => s)), templates)
        end

        fun makeValueConstr (name, typeOf, nullary, constrs, access, locations) : values =
            Value
            { 
              name    = name,
              typeOf  = typeOf,
              access  = access,
              class   = Constructor { nullary = nullary, ofConstrs = constrs },
              locations = locations,
              references = NONE
            }
    in
        fun makeValue state (name, ty, location, valu) =
            mkGvar(name, runTimeType state ty, mkConst valu, location)
    
        and makeException state (name, ty, location, valu) =
            mkGex(name, runTimeType state ty, mkConst valu, location)
   
        and makeConstructor state (name, ty, nullary, count, location, valu) =
                makeValueConstr(name, runTimeType state ty, nullary, count, Global(mkConst valu), location)

        and makeAnonymousValue state (ty, valu) =
            makeValue state ("", ty, [], valu)
    end

    (* Functions to make the debug entries.   These are needed both in CODEGEN_PARSETREE for
       the core language and STRUCTURES for the module language. *)
    (* Debugger status within the compiler.
       During compilation the environment is built up as a pair
       consisting of the static data and code to compute the run-time data.
       The static data, a constant at run-time, holds the
       variable names and types.  The run-time code, when executed at
       run-time, returns the address of a list holding the actual values
       of the variables. "dynEnv" is always a "load" from a (codetree)
       variable.  It has type level->codetree rather than codetree
       because the next reference could be inside an inner function.
       "lastLoc" is the last location that was *)
    type debuggerStatus =
        {staticEnv: environEntry list, dynEnv: level->codetree, lastLoc: location}
    
    val initialDebuggerStatus: debuggerStatus =
        {staticEnv = [], dynEnv = fn _ => CodeZero, lastLoc = LEX.nullLocation }

    (* Set the current state in the thread data. *)
    fun updateState (level, mkAddr) (decs, debugEnv: debuggerStatus as {staticEnv, dynEnv, ...}) =
    let
        open ADDRESS
        val threadId = multipleUses(getCurrentThreadId, fn () => mkAddr 1, level)
        fun assignItem(offset, value) =
            mkNullDec(mkStoreOperation(LoadStoreMLWord{isImmutable=false}, #load threadId level, offset, value))
        val newDecs =
            decs @ #dec threadId @
                [assignItem(threadIdCurrentStatic, mkConst(toMachineWord staticEnv)),
                 assignItem(threadIdCurrentDynamic, dynEnv level)]
    in
        (newDecs, debugEnv)
    end

    fun makeValDebugEntries (vars: values list, debugEnv: debuggerStatus, level, lex, mkAddr) =
    if getParameter debugTag (LEX.debugParams lex)
    then
        let
            fun loadVar (var, (decs, {staticEnv, dynEnv, lastLoc, ...})) =
                let
                    val loadVal =
                        codeVal (var, level, [], lex, LEX.nullLocation)
                    val newEnv =
                    (* Create a new entry in the environment. *)
                          mkDatatype [ loadVal (* Value. *), dynEnv level ]
                    val { dec, load } = multipleUses (newEnv, fn () => mkAddr 1, level)
                    val ctEntry =
                        case var of
                            Value{class=Exception, name, typeOf, locations, ...} =>
                                EnvException(name, typeOf, locations)
                        |   Value{class=Constructor{nullary, ofConstrs, ...}, name, typeOf, locations, ...} =>
                                EnvVConstr(name, typeOf, nullary, ofConstrs, locations)
                        |   Value{name, typeOf, locations, ...} =>
                                EnvValue(name, typeOf, locations)
                in
                    (decs @ dec, {staticEnv = ctEntry :: staticEnv, dynEnv = load, lastLoc = lastLoc})
                end
        in
            updateState (level, mkAddr) (List.foldl loadVar ([], debugEnv) vars)
        end
    else ([], debugEnv)

    fun makeTypeConstrDebugEntries(typeCons, debugEnv, level, lex, mkAddr) =
    if not (getParameter debugTag (LEX.debugParams lex))
    then ([], debugEnv)
    else
    let
        fun foldIds((tc as TypeConstrSet(TypeConstrs {name, identifier=id,...}, _)) :: tcs, {staticEnv, dynEnv, lastLoc, ...}) =
            let
                val {second = typeName, ...} = UTILITIES.splitString name
            in
                case id of
                    TypeId{idKind = TypeFn _, ...} =>
                        foldIds(tcs, {staticEnv=EnvTConstr(typeName, tc) :: staticEnv, dynEnv=dynEnv, lastLoc = lastLoc})
                |   _ =>
                    let
                        (* This code will build a cons cell containing the run-time value
                           associated with the type Id as the hd and the rest of the run-time
                           environment as the tl. *)                
                        val loadTypeId = TYPEIDCODE.codeId(id, level)
                        val newEnv = mkDatatype [ loadTypeId, dynEnv level ]
                        val { dec, load } = multipleUses (newEnv, fn () => mkAddr 1, level)
                        (* Make an entry for the type constructor itself as well as the new type id.
                           The type Id is used both for the type constructor and also for any values
                           of the type. *)
                        val (decs, newEnv) =
                            foldIds(tcs, {staticEnv=EnvTConstr(typeName, tc) :: envTypeId id :: staticEnv, dynEnv=load, lastLoc = lastLoc})
                    in
                        (dec @ decs, newEnv)
                    end
            end
        |   foldIds([], debugEnv) = ([], debugEnv)
    in
        updateState (level, mkAddr) (foldIds(typeCons, debugEnv))
    end

    fun makeStructDebugEntries (strs: structVals list, debugEnv, level, lex, mkAddr) =
    if getParameter debugTag (LEX.debugParams lex)
    then
        let
            fun loadStruct (str as Struct { name, signat, locations, ...}, (decs, {staticEnv, dynEnv, lastLoc, ...})) =
                let
                    val loadStruct = codeStruct (str, level)
                    val newEnv = mkDatatype [ loadStruct (* Structure. *), dynEnv level ]
                    val { dec, load } = multipleUses (newEnv, fn () => mkAddr 1, level)
                    val ctEntry = EnvStructure(name, signat, locations)
                in
                    (decs @ dec, {staticEnv=ctEntry :: staticEnv, dynEnv=load, lastLoc = lastLoc})
                end
        in
            updateState (level, mkAddr) (List.foldl loadStruct ([], debugEnv) strs)
        end
    else ([], debugEnv)

    (* Create debug entries for typeIDs.  The idea is that if we stop in the debugger we
       can access the type ID, particularly for printing values of the type.
       "envTypeId" creates a free id for each bound id but the print and equality
       functions are extracted when we are stopped in the debugger. *)
    fun makeTypeIdDebugEntries(typeIds, debugEnv, level, lex, mkAddr) =
    if not (getParameter debugTag (LEX.debugParams lex))
    then ([], debugEnv)
    else
    let
        fun foldIds(id :: ids, {staticEnv, dynEnv, lastLoc, ...}) =
            let
                (* This code will build a cons cell containing the run-time value
                   associated with the type Id as the hd and the rest of the run-time
                   environment as the tl. *)                
                val loadTypeId =
                    case id of TypeId { access = Formal addr, ... } =>
                        (* If we are processing functor arguments we will have a Formal here. *)
                        mkInd(addr, mkLoadArgument 0)
                    |   _ => TYPEIDCODE.codeId(id, level)
                val newEnv = mkDatatype [ loadTypeId, dynEnv level ]
                val { dec, load } = multipleUses (newEnv, fn () => mkAddr 1, level)
                val (decs, newEnv) =
                    foldIds(ids, {staticEnv=envTypeId id :: staticEnv, dynEnv=load, lastLoc = lastLoc})
            in
                (dec @ decs, newEnv)
            end
        |   foldIds([], debugEnv) = ([], debugEnv)
    in
        updateState (level, mkAddr) (foldIds(typeIds, debugEnv))
    end

    (* Update the location info in the thread data if we want debugging info.
       If the location has not changed don't do anything.  Whether it has changed
       could depend on whether we're only counting line numbers or whether we
       have more precise location info with the IDE. *)
    fun updateDebugLocation(debuggerStatus as {staticEnv, dynEnv, lastLoc, ...}, location, lex) =
    if not (getParameter debugTag (LEX.debugParams lex)) orelse lastLoc = location
    then ([], debuggerStatus)
    else
    let
        open ADDRESS
        val setLocation =
            mkStoreOperation(LoadStoreMLWord{isImmutable=false},
                getCurrentThreadId, threadIdCurrentLocation, mkConst(toMachineWord location))
    in
        ([mkNullDec setLocation], {staticEnv=staticEnv, dynEnv=dynEnv, lastLoc=location})
    end

    (* Add debugging calls on entry and exit to a function. *)
    fun wrapFunctionInDebug(codeBody: debuggerStatus -> codetree, name: string, argCode, argType, resType: types, location,
                            entryEnv: debuggerStatus, level, lex, mkAddr) =
        if not (getParameter debugTag (LEX.debugParams lex))
        then codeBody entryEnv (* Code-generate the body without any wrapping. *)
        else
        let
            open ADDRESS
            
            val functionName = name (* TODO: munge this to get the root. *)
            
            fun addStartExitEntry({staticEnv, dynEnv, lastLoc, ...}, code, ty, startExit) =
            let
                val newEnv = mkDatatype [ code, dynEnv level ]
                val { dec, load } = multipleUses (newEnv, fn () => mkAddr 1, level)
                val ctEntry = startExit(functionName, location, ty)
            in
                (dec, {staticEnv=ctEntry :: staticEnv, dynEnv=load, lastLoc = lastLoc})
            end

            (* All the "on" functions take this as an argument. *)
            val onArgs = [mkConst(toMachineWord(functionName, location))]

            val threadId = multipleUses(getCurrentThreadId, fn () => mkAddr 1, level)
            fun loadIdEntry offset =
                multipleUses(mkLoadOperation(LoadStoreMLWord{isImmutable=false}, #load threadId level, offset), fn () => mkAddr 1, level)
            val currStatic = loadIdEntry threadIdCurrentStatic
            and currDynamic = loadIdEntry threadIdCurrentDynamic
            and currLocation = loadIdEntry threadIdCurrentLocation
            and currStack = loadIdEntry threadIdStack

            (* At the start of the function:
               1.  Push the previous state to the stack.
               2.  Create a debugging entry for the arguments
               3.  Update the state to the state on entry, including the args
               4.  Call the global onEntry function if it's set
               5.  Call the local onEntry function if it's set *)
            (* Save the previous state. *)
            val assignStack =
                mkStoreOperation(LoadStoreMLWord{isImmutable=false}, #load threadId level, threadIdStack,
                    mkDatatype[
                        #load currStatic level, #load currDynamic level,
                        #load currLocation level, #load currStack level])

            val prefixCode =
                #dec threadId @ #dec currStatic @ #dec currDynamic @ #dec currLocation @ #dec currStack @ [mkNullDec assignStack]

            (* Make a debugging entry for the arguments.  This needs to be set
               before we call onEntry so we can produce tracing info.  It also needs
               to be passed to the body of the function so that it is included in the
               debug status of the rest of the body. *)
            local
                val {staticEnv, dynEnv, lastLoc, ...} = entryEnv
                val newEnv = mkDatatype [ argCode, dynEnv level ]
                val { dec, load } = multipleUses (newEnv, fn () => mkAddr 1, level)
                val ctEntry = EnvStartFunction(functionName, location, argType)
            in
                val debuggerDecs = dec
                val bodyDebugEnv = {staticEnv = ctEntry :: staticEnv, dynEnv = load, lastLoc = lastLoc}
            end

            local
                val {staticEnv, dynEnv, ...} = bodyDebugEnv
                val assignStatic =
                    mkStoreOperation(LoadStoreMLWord{isImmutable=false}, #load threadId level, threadIdCurrentStatic,
                        mkConst(toMachineWord staticEnv))
                val assignDynamic =
                    mkStoreOperation(LoadStoreMLWord{isImmutable=false}, #load threadId level, threadIdCurrentDynamic,
                        dynEnv level)
                val assignLocation =
                    mkStoreOperation(LoadStoreMLWord{isImmutable=false}, #load threadId level, threadIdCurrentLocation,
                        mkConst(toMachineWord location))
                val onEntryFn = multipleUses(onEntryCode, fn () => mkAddr 1, level)
                val optCallOnEntry =
                    mkIf(mkTagTest(#load onEntryFn level, 0w0, 0w0), CodeZero, mkEval(#load onEntryFn level, onArgs))
            in
                val entryCode = debuggerDecs @
                    [mkNullDec assignStatic, mkNullDec assignDynamic, mkNullDec assignLocation] @
                    #dec onEntryFn @ [mkNullDec optCallOnEntry]
            end
            
            (* Restore the state.  Used both if the function returns normally or if
               it raises an exception.  We use the old state rather than popping the stack
               because that is more reliable if we have an asynchronous exception. *)
            local
                (* Set the entry in the thread vector to an entry from the top-of-stack. *)
                fun restoreEntry(offset, value) =
                    mkNullDec(
                        mkStoreOperation(LoadStoreMLWord{isImmutable=false}, #load threadId level, offset, value))
            in
                val restoreState =
                    [restoreEntry(threadIdCurrentStatic, #load currStatic level),
                     restoreEntry(threadIdCurrentDynamic, #load currDynamic level),
                     restoreEntry(threadIdCurrentLocation, #load currLocation level),
                     restoreEntry(threadIdStack, #load currStack level)]
            end

            local
                (* If an exception is raised we need to call the onExitException entry, restore the state
                   and reraise the exception. *)
                (* There are potential race conditions here if we have asynchronous exceptions. *)
                val exPacketAddr = mkAddr 1
                val onExitExcFn = multipleUses(onExitExcCode, fn () => mkAddr 1, level)
                (* OnExitException has an extra curried argument - the exception packet. *)
                val optCallOnExitExc =
                    mkIf(mkTagTest(#load onExitExcFn level, 0w0, 0w0), CodeZero,
                        mkEval(mkEval(#load onExitExcFn level, onArgs), [mkLoadLocal exPacketAddr]))
            in
                val exPacketAddr = exPacketAddr
                val exceptionCase =
                    mkEnv(#dec onExitExcFn @  [mkNullDec optCallOnExitExc]  @ restoreState,
                        mkRaise(mkLoadLocal exPacketAddr))
            end
            
            (* Code for the body and the exception. *)
            val bodyCode =
                multipleUses(mkHandle(codeBody bodyDebugEnv, exceptionCase, exPacketAddr), fn () => mkAddr 1, level)

            (* Code for normal exit. *)
            local
                val endFn = addStartExitEntry(entryEnv, #load bodyCode level, resType, EnvEndFunction)
                val (rtEnvDec, _) = updateState (level, mkAddr) endFn

                val onExitFn = multipleUses(onExitCode, fn () => mkAddr 1, level)
                val optCallOnExit =
                    mkIf(mkTagTest(#load onExitFn level, 0w0, 0w0), CodeZero, mkEval(#load onExitFn level, onArgs))
            in
                val exitCode = rtEnvDec @ #dec onExitFn @ [mkNullDec optCallOnExit]
            end
        in
            mkEnv(prefixCode @ entryCode @ #dec bodyCode @ exitCode @ restoreState, #load bodyCode level)
        end

    type breakPoint = bool ref
 
    (* Create a local break point and check the global and local break points. *)
    fun breakPointCode(breakPoint, location, level, lex, mkAddr) =
        if not (getParameter debugTag (LEX.debugParams lex)) then []
        else
        let
            open ADDRESS
            (* Create a new local breakpoint and assign it to the ref.
               It is possible for the ref to be already assigned a local breakpoint
               value if we are compiling a match.  In that case the same expression
               may be code-generated more than once but we only want one local
               break-point. *)
            val localBreakPoint =
                case breakPoint of
                    ref (SOME bpt) => bpt
                |   r as ref NONE =>
                    let val b = ref false in r := SOME b; b end;
            (* Call the breakpoint function if it's defined. *)
            val globalBpt = multipleUses(onBreakPointCode, fn () => mkAddr 1, level)
            val testCode =
                mkIf(
                    mkNot(mkTagTest(#load globalBpt level, 0w0, 0w0)), 
                    mkEval(#load globalBpt level,
                        [mkTuple[mkConst(toMachineWord location), mkConst(toMachineWord localBreakPoint)]]),
                    CodeZero
                )
        in
            #dec globalBpt @ [mkNullDec testCode]
        end

    structure Sharing =
    struct
        type types          = types
        type values         = values
        type machineWord    = machineWord
        type fixStatus      = fixStatus
        type structVals     = structVals
        type typeConstrSet  = typeConstrSet
        type signatures     = signatures
        type functors       = functors
        type locationProp   = locationProp
        type environEntry   = environEntry
        type typeId         = typeId
        type level          = level
        type lexan          = lexan
        type codeBinding    = codeBinding
        type codetree       = codetree
        type debuggerStatus = debuggerStatus
        type valueType      = valueType
    end
end;
