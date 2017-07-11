(*
    Copyright (c) 2012,13 David C.J. Matthews

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

functor CODETREE_REMOVE_REDUNDANT(
    structure BASECODETREE: BaseCodeTreeSig
    structure CODETREE_FUNCTIONS: CodetreeFunctionsSig

    sharing BASECODETREE.Sharing = CODETREE_FUNCTIONS.Sharing
) :
    sig
        type codetree
        type loadForm
        type codeUse
        val cleanProc : (codetree * codeUse list * (int -> loadForm) * int) -> codetree
        structure Sharing: sig type codetree = codetree and loadForm = loadForm and codeUse = codeUse end
    end
=
struct
    open BASECODETREE
    open CODETREE_FUNCTIONS
    exception InternalError = Misc.InternalError

    (* This function annotates the tree with information about how variables are used.  This assists
       the optimiser to choose the best alternative for code.  It also discards bindings that
       are unused and side-effect-free.  These can arise as the result of optimiser constructing
       bindings in case they are required.  That was originally its only function; hence the name. *)
    fun cleanProc (pt, procUses: codeUse list, prev: int * codeUse list -> loadForm, recursiveRef: codeUse list -> unit, localCount, checkArg) =
    let
        val locals = Array.array(localCount, [])
        fun addLocalUse addr use =
            Array.update(locals, addr, use @ Array.sub(locals, addr))

        fun cleanLambda(lambda as {body, isInline, name, argTypes, resultType, localCount, closure, ...}: lambdaForm,
                        lambdaUse) =
        let
            (* Rebuild the closure with the entries actually used. *)
            val closureUse = makeClosure()

            fun lookup (closureEntry, clUse) =
                let
                    (* Find the original closure entry. *)
                    val ext = List.nth(closure, closureEntry)
                    (* Process the closure entry.  We need to do this to record the
                       usage information even if we have already seen this entry. *)
                    val copied = cleanExtract(ext, clUse)
                in
                    addToClosure closureUse copied
                end

            (* This array records the way the arguments are used inside the function. *)
            val argUses = Array.array (List.length argTypes, [])
            fun checkArg(addr, uses) = Array.update(argUses, addr, uses @ Array.sub(argUses, addr))
            
            val recursiveRefRef = ref []
            fun addRef use = recursiveRefRef := use @ !recursiveRefRef

            val resultOfApps =
                List.foldl
                    (fn (UseApply (l, _), r) => l @ r | (UseExport, r) => UseExport :: r | (_, r) => UseGeneral :: r) []
            
            val bodyUse = resultOfApps lambdaUse

            val bodyCode = cleanProc(body, bodyUse, lookup, addRef, localCount, checkArg)
            val recursiveApps = !recursiveRefRef
            (* If we have called this function somewhere and used the result that gives us a hint on the
               preferred result.  If the function is recursive, though, we can't assume anything
               because the result of the recursive calls may be used in some other context.  For
               example they could be passed into an argument function which may require more fields.
               That in turn affects any functions whose results are used.  See Test138.ML.
               So, we check to see whether the result of recursive use has added anything to the
               original usage and reprocess the body if it has. *)
            val recursiveResults = resultOfApps recursiveApps
            val needReprocess =
                if null recursiveApps orelse
                    List.exists(fn UseGeneral => true | UseExport => true | _ => false) bodyUse
                then NONE (* Not recursive or already general or export *)
                else if List.all(fn UseField _ => true | _ => false) bodyUse andalso
                            List.all(fn UseField _ => true | _ => false) recursiveResults
                then (* All the entries are UseField - has the recursion added something new? *)
                    if List.foldl
                        (fn (UseField(n, _), false) =>
                            List.all
                                (fn UseField(m, _) => m <> n | _ => raise InternalError "not UseField")
                                bodyUse (* false if it's already there *)
                          | (_, true) => true
                          | _ => raise InternalError "not UseField")
                        false recursiveResults
                then SOME recursiveApps
                else NONE
                else SOME [UseGeneral] (* If there's anything else reprocess it with UseGeneral *)
        in
            case needReprocess of
                SOME reprocessWith => cleanLambda(lambda, lambdaUse @ reprocessWith)
            |   NONE =>
                let
                    val newClosure = extractClosure closureUse

                    val newArgTypes = ListPair.zip(map #1 argTypes, Array.foldr (op ::) [] argUses)
                in
                    {body=bodyCode, isInline=isInline, name=name,
                       closure=newClosure, argTypes=newArgTypes, resultType=resultType,
                       localCount=localCount, recUse = recursiveApps} : lambdaForm
                end
        end

        (* Process a load from a variable.  Locals and arguments operate on the relevant array,
           closure entries involve a look-up *)
        and cleanExtract(ext as LoadLocal addr, codeUse) =
            (
                (* Check we're actually adding to the usage. *)
                null codeUse andalso raise InternalError "cleanExtract: empty usage";
                addLocalUse addr codeUse;
                ext
            )

        |   cleanExtract(ext as LoadArgument addr, codeUse) =
            (
                checkArg(addr, codeUse);
                ext
            )

        |   cleanExtract(LoadClosure addr, codeUse) = prev(addr, codeUse)
        
        |   cleanExtract(LoadRecursive, codeUse) = (recursiveRef codeUse; LoadRecursive)

        and cleanCode (code, codeUse) =
        let
            fun doClean codeUse (Newenv(decs, exp)) =
                let
                    (* First process the expression so as to mark any references it makes. *)
                    val processedExp = cleanCode (exp, codeUse)
                
                    (* Process the declarations in reverse order.  A binding may be used in
                       a later declaration but apart from mutually-recursive functions no binding
                       can be used in an earlier one. *)
                    fun processDecs [] = []

                    |   processDecs(Declar{value, addr, ...} :: rest) =
                        let
                            val processedRest = processDecs rest
                            val decUses =
                                case Array.sub(locals, addr) of
                                    [] => if sideEffectFree value then [] else [UseGeneral]
                                |   uses => uses
                        in
                            (* We can drop bindings that are unused if they have no side-effects.
                               If we retain the binding we must set at least one reference. *)
                            if null decUses
                            then processedRest (* Skip it *)
                            else
                            let
                                val cleanvalue =
                                    case value of
                                        Lambda lambda => Lambda(cleanLambda(lambda, decUses))
                                    |   value => cleanCode (value, decUses)
                            in
                                Declar{value=cleanvalue, addr=addr, use=decUses} :: processedRest
                            end
                        end

                    |   processDecs(RecDecs decs :: rest) =
                        let
                            val processedRest = processDecs rest
                            (* We now know the entries that have actually been used
                               in the rest of the code.  We need to include those
                               declarations and any that they use.
                               The result we pass down may well exclude some or all
                               recursive uses.  We need to include UseGeneral in
                               the result for safety. *)
                            fun processMutuals([], excluded, true) =
                                    (* If we have included a function in this
                                       pass we have to reprocess the list of
                                       those we excluded before. *)
                                    processMutuals(excluded, [], false)
                             |  processMutuals([], _, false) =
                                    (* We didn't add anything more - finish *) []
                             |  processMutuals(
                                    (this as {addr, lambda, ...}) :: rest, excluded, added) =
                                (
                                    case Array.sub(locals, addr) of
                                        [] => (* Put this on the excluded list. *)
                                            processMutuals(rest, this::excluded, added)
                                    |   useSoFar =>
                                            (* Process this then the rest of the list. *)
                                            (addr, cleanLambda(lambda, UseGeneral :: useSoFar)) ::
                                                processMutuals(rest, excluded, true)
                                )
                            val entriesUsed = processMutuals(decs, [], false)
                            (* Get all the uses now we're finished and have identified
                               all the recursive uses. *)
                            val processedDecs =
                                map (fn(a, l) => {addr=a, lambda=l, use=Array.sub(locals, a)}) entriesUsed
                        in
                            if null processedDecs
                            then processedRest
                            else RecDecs processedDecs :: processedRest
                        end

                    |   processDecs(NullBinding exp :: rest) =
                        let
                            val processedRest = processDecs rest
                        in
                            if sideEffectFree exp
                            then processedRest
                            else NullBinding(cleanCode(exp, [UseGeneral])) :: processedRest
                        end

                    |   processDecs(Container{setter, size, addr, ...} :: rest) =
                        let
                            val processedRest = processDecs rest
                            val decUses =
                                case Array.sub(locals, addr) of
                                    [] => if sideEffectFree setter then [] else [UseGeneral]
                                |   uses => uses
                        in
                            (* We can drop bindings that are unused if they have no side-effects.
                               If we retain the binding we must set at least one reference. *)
                            (* Currently SetContainer is treated as having a side-effect so
                               we will never discard this even if the container is unused. *)
                            if null decUses
                            then processedRest (* Skip it *)
                            else Container{setter=cleanCode (setter, [UseGeneral]), addr=addr, size=size, use=decUses} :: processedRest
                        end

                    val processedDecs = processDecs decs
                in
                    SOME(mkEnv(processedDecs, processedExp))
                end (* Newenv *)

                (* Reference to a binding. *)
            |   doClean codeUse (Extract ext) = SOME(Extract(cleanExtract(ext, codeUse)))

                (* Select a field from a tuple.  We can't do this for selection from datatypes because
                   some fields may not be present on all paths. *)
            |   doClean codeUse (Indirect{base, offset, isVariant = false}) =
                    (* Try to pass down the use.  If the "base" is an Extract or another Indirect
                       we may be able to record this information.  If it is something else we can't. *)
                    SOME(Indirect{base=cleanCode(base, [UseField(offset, codeUse)]), offset=offset, isVariant=false})

            |   doClean codeUse (Tuple{ fields, isVariant = false}) =
                let
                    (* If the use of the tuple include UseGeneral or UseExport then every field is
                       required.  If, though, we have UseField we can transfer the corresponding
                       usage onto the field of the tuple. *)
                    fun fieldUse n (UseField(offset, uses), tl) =
                            if n = offset then uses @ tl else tl
                    |   fieldUse _ (use, tl) = use :: tl

                    fun fieldUses n =
                        (* For the moment, if we find that the field is not used we set the
                           usage to UseGeneral.  I'm not convinced it would be safe to
                           discard anything in the expression at this point. *)
                        case List.foldl(fieldUse n) [] codeUse of
                            [] => [UseGeneral]
                        |   other => other
                            
                    fun processField([], _) = []
                    |   processField(hd::tl, n) =
                            cleanCode(hd, fieldUses n) :: processField(tl, n+1)
                in
                    SOME(Tuple{ fields = processField(fields, 0), isVariant = false})
                end

            |   doClean codeUse (Lambda lam) = SOME(Lambda(cleanLambda(lam, codeUse)))

            |   doClean codeUse (Eval{function, argList, resultType}) =
                (* As with Indirect we try to pass this information down so that if
                   the function is a variable it will be marked as "called". *)
                let
                    val args = map (fn (c, t) => (cleanCode(c, [UseGeneral]), t)) argList
                    val argTuples = map #1 args
                in
                    SOME(
                        Eval{
                            function=cleanCode(function, [UseApply(codeUse, argTuples)]),
                            argList=args, resultType = resultType
                        })
                end

            |   doClean codeUse (Cond(i, t, e)) =
                    SOME(Cond(cleanCode(i, [UseGeneral]), cleanCode(t, codeUse), cleanCode(e, codeUse)))

            |   doClean use (BeginLoop{loop, arguments}) =
                let
                    val cleanBody = cleanCode(loop, use)
                    (* Remove unused arguments.  They're unnecessary and may cause problems
                       later on. *)
                    fun filterUnused [] = ([], [])
                    |   filterUnused (({use=[], value, ...}, _) :: args) =
                        let
                            val (used, discards) = filterUnused args
                            (* We only need to keep this if it might have a side-effect. *)
                        in
                           (used, NullBinding(cleanCode(value, [UseGeneral])) :: discards)
                        end
                    |   filterUnused(({value, addr, use}, t) :: args) =
                        let
                            val (used, discards) = filterUnused args
                        in
                            (({value=cleanCode(value, use), addr=addr, use=use}, t) :: used, discards)
                        end
                    val (usedArgs, discards) = filterUnused arguments
                in
                    if not(null discards)
                    then
                    let
                        fun splitArgs([], []) = ([], [])
                        |   splitArgs((arg, _) :: args, ({use=[], ...}, _) :: arguments) =
                            let
                                val (useArgs, discards) = splitArgs(args, arguments)
                            in
                                (* We actually only need to keep this argument if it might have
                                   a side-effect but keep it anyway. *)
                                (useArgs, NullBinding arg :: discards)
                            end
                        |   splitArgs(arg :: args, _ :: arguments) =
                            let
                                val (useArgs, discards) = splitArgs(args, arguments)
                            in
                                (arg :: useArgs, discards)
                            end
                        |   splitArgs _ = raise InternalError "splitArgs"

                        fun filterLoopArgs(Loop l) =
                            let
                                val (useArgs, discards) = splitArgs(l, arguments)
                            in
                                SOME(Newenv(discards, Loop useArgs))
                            end
                            (* Don't descend into functions or inner loops. *)
                        |   filterLoopArgs(instr as Lambda _) = SOME instr
                        |   filterLoopArgs(instr as BeginLoop _) = SOME instr
                        |   filterLoopArgs _ = NONE

                        val newLoop =
                            BeginLoop {loop = mapCodetree filterLoopArgs cleanBody, arguments = usedArgs}
                    in
                        SOME(Newenv(discards, newLoop))
                    end
                    else SOME(BeginLoop {loop = cleanBody, arguments = usedArgs})
                end
        
            |   doClean _ _ = NONE (* Anything else *)
        in
            (* If we recognise this as a special case we use the result otherwise
               we process it as a general value using UseGeneral as the usage. *)
            case doClean codeUse code of
                SOME result => result
            |   NONE => mapCodetree (doClean [UseGeneral]) code
        end

    in
        cleanCode (pt, procUses)
    end (* cleanProc *)

    val cleanProc =
        fn (code, procUse, prev, localCount) =>
            cleanProc(code, procUse, fn (i, _) => prev i, fn _ => (), localCount, fn _ => ())

    structure Sharing =
    struct
        type codetree = codetree
        and loadForm = loadForm
        and codeUse = codeUse
    end
end;
