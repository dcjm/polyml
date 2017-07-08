(*
    Copyright David C. J. Matthews 1991, 2009-10, 2012-13, 2015-17

    Title:      General purpose code generator.
    Author:     Dave Matthews, Edinburgh University / Prolingua Ltd.
    Copyright   D.C.J. Matthews 1991

    Copyright (c) 2000
        Cambridge University Technical Services Limited

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

(* This is the original code generator. *)

functor GENERATE_CODE (
    structure CODECONS : X86OLDFIRSTSTAGESIG
    structure BACKENDTREE: BackendIntermediateCodeSig
    structure DEBUG: DEBUGSIG
    structure X86FOREIGN: FOREIGNCALLSIG

    sharing CODECONS.Sharing = BACKENDTREE.Sharing
) :  GENCODESIG
=

struct
    open CODECONS

    (*open Address;*)
    (*open Misc;*) (* after address, so we get Misc.length, not Address.length *)
    exception InternalError = Misc.InternalError
    val wordEq = Address.wordEq
    open RuntimeCalls; (* for POLY_SYS numbers *)
    open BACKENDTREE;
    
    open RegSet
    
    (* We need to add life-time information to the code-tree.  That used to be
       done at a higher level. *)
    
    datatype oldGenCodeIC =
        OGCNewenv of oldGenCodeBinding list * oldGenCodeIC (* Set of bindings with an expression. *)

    |   OGCConstnt of machineWord * Universal.universal list (* Load a constant *)

    |   OGCExtract of oldGenCodeLoadForm * bool (* Get a local variable, an argument or a closure value *)

    |   OGCField of {base: oldGenCodeIC, offset: int }
         (* Load a field from a tuple or record *)
    
    |   OGCEval of (* Evaluate a function with an argument list. *)
        {
            function:  oldGenCodeIC,
            argList:   (oldGenCodeIC * argumentType) list,
            resultType: argumentType
        }

        (* Built-in functions. *)
    |   OGCBuiltIn0 of {oper: BuiltIns.builtIn0Ops}
    |   OGCBuiltIn1 of {oper: BuiltIns.builtIn1Ops, arg1: oldGenCodeIC}
    |   OGCBuiltIn2 of {oper: BuiltIns.builtIn2Ops, arg1: oldGenCodeIC, arg2: oldGenCodeIC}
    |   OGCBuiltIn3 of {oper: BuiltIns.builtIn3Ops, arg1: oldGenCodeIC, arg2: oldGenCodeIC, arg3: oldGenCodeIC}
    |   OGCBuiltIn4 of {oper: BuiltIns.builtIn4Ops, arg1: oldGenCodeIC, arg2: oldGenCodeIC, arg3: oldGenCodeIC, arg4: oldGenCodeIC}
    |   OGCBuiltIn5 of {oper: BuiltIns.builtIn5Ops, arg1: oldGenCodeIC, arg2: oldGenCodeIC, arg3: oldGenCodeIC, arg4: oldGenCodeIC, arg5: oldGenCodeIC}

    |   OGCLambda of oldGenCodeLambdaForm (* Lambda expressions. *)

    |   OGCCond of oldGenCodeIC * oldGenCodeIC * oldGenCodeIC (* If-then-else expression *)

    |   OGCCase of (* Case expressions *)
        {
            cases   : (oldGenCodeIC * word) list,
            test    : oldGenCodeIC,
            caseType: caseType,
            default : oldGenCodeIC
        }
    
    |   OGCBeginLoop of (* Start of tail-recursive inline function. *)
        { loop: oldGenCodeIC, arguments: (oldGenCodeSimpleBinding * argumentType) list }

    |   OGCLoop of (oldGenCodeIC * argumentType) list (* Jump back to start of tail-recursive function. *)

    |   OGCKillItems of
            (* Kill entries.  Used to mark a branch where a binding is no longer required.
               "killSet" is always an Extract with lastRef=true so the type should
               be loadForm list rather than oldGenCodeIC list. *)
            { expression: oldGenCodeIC, killSet: oldGenCodeIC list, killBefore: bool }

    |   OGCRaise of oldGenCodeIC (* Raise an exception *)

    |   OGCLdexc (* Load the exception (used at the start of a handler) *)

    |   OGCHandle of (* Exception handler. *) { exp: oldGenCodeIC, handler: oldGenCodeIC }

    |   OGCTuple of oldGenCodeIC list (* Tuple *)

    |   OGCContainer of int (* Create a container for a tuple on the stack. *)

    |   OGCSetContainer of (* Copy a tuple to a container. *)
        {
            container: oldGenCodeIC,
            tuple:     oldGenCodeIC,
            filter:    BoolVector.vector
        }

    |   OGCTagTest of { test: oldGenCodeIC, tag: word, maxTag: word }
    
    |   OGCLoadOperation of { kind: loadStoreKind, address: oldGenCodeAddress }
    
    |   OGCStoreOperation of { kind: loadStoreKind, address: oldGenCodeAddress, value: oldGenCodeIC }
    
    |   OGCBlockOperation of
            { kind: blockOpKind, sourceLeft: oldGenCodeAddress, destRight: oldGenCodeAddress, length: oldGenCodeIC }

    and oldGenCodeBinding =
        OGCDeclar  of oldGenCodeSimpleBinding (* Make a local declaration or push an argument *)
    |   OGCRecDecs of { addr: int, references: int, lambda: oldGenCodeLambdaForm } list (* Set of mutually recursive declarations. *)
    |   OGCNullBinding of oldGenCodeIC (* Just evaluate the expression and discard the result. *)

    and oldGenCodeLoadForm =
        OGCLoadLocal of int (* Local binding *)
    |   OGCLoadArgument of int (* Argument - 0 is first arg etc.*)
    |   OGCLoadClosure of int (* Closure - 0 is first closure item etc *)
    |   OGCLoadRecursive (* Recursive call *)

    withtype oldGenCodeSimpleBinding = 
    { (* Declare a value or push an argument. *)
        value:      oldGenCodeIC,
        addr:       int,
        references: int
    }

    and oldGenCodeLambdaForm =
    { (* Lambda expressions. *)
        body          : oldGenCodeIC,
        name          : string,
        closure       : oldGenCodeIC list,
        argTypes      : argumentType list,
        resultType    : argumentType,
        closureRefs   : int,
        localCount    : int,
        heapClosure   : bool,
        argLifetimes  : int list
    }

    and oldGenCodeAddress =
        (* Address form used in loads, store and block operations.  The base is an ML
           address if this is to/from ML memory or a (boxed) SysWord.word if it is
           to/from C memory.  The index is a value in units of the size of the item
           being loaded/stored and the offset is always in bytes. *)
        {base: oldGenCodeIC, index: oldGenCodeIC option, offset: word}

    local
        open BACKENDTREE

        fun mkDecRef(ct, i1, i2) =
            OGCDeclar{value = ct, addr = i1, references = i2}

        fun sideEffectFree (BICLambda _) = true
        |   sideEffectFree (BICConstnt _) = true
        |   sideEffectFree (BICExtract _) = true
        |   sideEffectFree (BICCond(i, t, e)) =
              sideEffectFree i andalso
              sideEffectFree t andalso
              sideEffectFree e
        |   sideEffectFree (BICNewenv(decs, exp)) = List.all sideEffectBinding decs andalso sideEffectFree exp
        |   sideEffectFree (BICHandle { exp, handler }) =
              sideEffectFree exp andalso sideEffectFree handler
        | sideEffectFree (BICTuple recs) = testList recs
        | sideEffectFree (BICField{base, ...}) = sideEffectFree base

            (* A built-in function may be side-effect free.  This can
               occur if we have, for example, "if exp1 orelse exp2"
               where exp2 can be reduced to "true", typically because it's
               inside an inline function and some of the arguments to the
               function are constants.  This then gets converted to
               (exp1; true) and we can eliminate exp1 if it is simply
               a comparison. *)
        | sideEffectFree (BICBuiltIn0 _) = false
        | sideEffectFree (BICBuiltIn1 _) = false
        | sideEffectFree (BICBuiltIn2 _) = false
        | sideEffectFree (BICBuiltIn3 _) = false
        | sideEffectFree (BICBuiltIn4 _) = false
        | sideEffectFree (BICBuiltIn5 _) = false

        | sideEffectFree(BICContainer _) = true
            (* But since SetContainer has a side-effect we'll always create the
               container even if it isn't used.  *)

        | sideEffectFree _ = false
                 (* Rest are unsafe (or too rare to be worth checking) *)

        and testList t = List.all sideEffectFree t
    
        and sideEffectBinding(BICDeclar{value, ...}) = sideEffectFree value
        |   sideEffectBinding(BICRecDecs _) = true (* These should all be lambdas *)
        |   sideEffectBinding(BICNullBinding c) = sideEffectFree c

    in
        (* This function transforms the codetree to add "lifetime" information
           for bindings ("declarations").  The idea is to aid the code-generator
           by indicating when a binding is no longer required and also to
           distinguish short-lived bindings from longer-lived bindings
           when deciding on register allocation.
           It also causes unused bindings to be discarded if they are
           not used and have no side-effects.  The previous passes may
           generate extra bindings and rely on this pass to remove them
           if they are not actually used. *)

        fun lifeTimes (pt: backendIC, localAddressCount): oldGenCodeIC =
        let
            fun copyCode (pt: backendIC, argUses, localCount): oldGenCodeIC =
            let
                (* Tables for local declarations. "localUses" is the last reference
                   for the declaration.  *)
                val localUses         = Array.array(localCount, 0)
                (* Because we count instructions from the end smaller values mean
                   further away and we want to count the smallest non-zero value. *)
                fun maxUse(m, 0) = m
                |   maxUse(0, m) = m
                |   maxUse(m, n) = Int.min(m, n)
      
                (* If we are inside a loop these entries indicate that
                   the declaration was made outside so the entries must
                   not be killed there. *)
                val outsideLoop       = Array.array (localCount, false)
                (* This also applies to all the parameters of the function which
                   could be passed in registers. *)
                val outsideLoopRef = ref false

                (* This counts the number of "instructions" from the end of the code (because
                   we process it depth first) to give a measure of how long a declaration is in
                   use.  This is used to control register spilling in the code-generator. *)
                val instrCount = ref 1

                abstype usageSet = UsageSet of {locals: int Vector.vector, args: int Vector.vector}
                with
                  (* Used to give us a "kill set" for an expression.
                     In the case of parallel flows of control (e.g. then- and else-parts
                     of an if-then-else) we can explicitly kill variables if they
                     appear in the kill set for one branch but not in another.
                     e.g. in  if x then y else z  assuming that x, y, z are not
                     used in subsequent expressions we can kill z in the then-branch
                     and y in the else-branch.  The advantage of this is that we don't
                     need to save variables if they are never used. *)
                    fun saveUsages() =
                        UsageSet{locals=Array.vector localUses, args=Array.vector argUses}

                    (* Restore the table to the saved values. *)
                    fun setToSaved(UsageSet{locals, args}): unit =
                    (
                        Array.copyVec{src=locals, dst=localUses, di=0};
                        Array.copyVec{src=args, dst=argUses, di=0}
                    )

                    (* Similar to setToSaved except that it sets the current set
                       to the union of the current set and the saved set. *)
                    fun addFromSaved(UsageSet{locals=locals, args=args}): unit =
                    (
                        Array.modifyi(fn(i, v) => maxUse(v, Vector.sub(args, i))) argUses;
                        Array.modifyi(fn(i, v) => maxUse(v, Vector.sub(locals, i))) localUses
                    )

                    (* Compute the differences between usage sets as kill entries. *)
                    fun computeKillSetLists usages =
                    let
                        (* We want to find the cases where the value is zero in at least one branch and
                           non-zero in at least one other branch.  Sum the zeros.  If the result is zero
                           the variable is not referenced at all in the set and we can ignore it.  Similarly
                           if it is the length of the list then it is referenced in all the branches
                           and again we can ignore it. *)
                        fun getAllLocals i =
                            List.foldl(fn(UsageSet{locals, ...}, l) => if Vector.sub(locals, i) = 0 then l+1 else l) 0 usages
                        val sumLocalSet = Vector.tabulate(localCount, getAllLocals)
                        val argCount = Array.length argUses
                        fun getAllArgs i =
                            List.foldl(fn(UsageSet{args, ...}, l) => if Vector.sub(args, i) = 0 then l+1 else l) 0 usages
                        val sumArgSet = Vector.tabulate(argCount, getAllArgs)
                        val fullSet = List.length usages

                        fun computeKills(UsageSet{locals, args}) =
                        let
                            (* Create lists of Extract entries with lastRef true to indicate that the
                               item is no longer required. *)
                            val killArgs =
                                Vector.foldli (fn (addr, n, l) =>
                                    if n = 0 orelse n = fullSet orelse Vector.sub(args, addr) <> 0 orelse ! outsideLoopRef
                                    then l 
                                    else if addr = 0 then OGCExtract(OGCLoadRecursive, true) :: l
                                    else OGCExtract(OGCLoadArgument(addr-1), true) :: l) [] sumArgSet
                        in
                            Vector.foldli (fn (addr, n, l) =>
                                    if n = 0 orelse n = fullSet orelse Vector.sub(locals, addr) <> 0 orelse Array.sub(outsideLoop, addr)
                                    then l 
                                    else OGCExtract(OGCLoadLocal addr, true) :: l) killArgs sumLocalSet
                        end
                    in
                        List.map computeKills usages
                    end
                end

                fun addKillSet(original, []): oldGenCodeIC = original (* No change *)
                |   addKillSet(OGCNewenv(decs, exp), killSet) = OGCNewenv(map OGCNullBinding killSet @ decs, exp)
                |   addKillSet(original, killSet) = OGCNewenv(map OGCNullBinding killSet, original)

                (* returns the translated node *)
                fun locaddr (BICLoadArgument argNo) =
                    let (* parameters *)
                        val () = instrCount := !instrCount+1;
                        val lastReference =
                            Array.sub(argUses, argNo+1 (* Zero is used for recursive *)) = 0 andalso not (!outsideLoopRef)
                    in 
                        (* Mark the argument as used. *)
                        Array.update (argUses, argNo+1, maxUse(! instrCount, Array.sub(argUses, argNo+1)));
                        OGCExtract(OGCLoadArgument argNo, lastReference)
                    end

                | locaddr (BICLoadLocal laddr) =
                    let 
                        val () = instrCount := !instrCount+1;
                        (* If this was outside a loop we can't mark this as the last
                           reference because it needs to be retained for the next time round. *)
                        val lastReference =
                            Array.sub(localUses, laddr) = 0 andalso not (Array.sub(outsideLoop, laddr))
                    in
                        Array.update (localUses, laddr, maxUse(! instrCount, Array.sub(localUses, laddr)));
                        OGCExtract(OGCLoadLocal laddr, lastReference)
                    end

                | locaddr BICLoadRecursive =
                    let
                        val () = instrCount := !instrCount+1;
                        val lastReference = Array.sub(argUses, 0) = 0 andalso not (!outsideLoopRef)
                    in
                        (* Mark the closure as used. *)
                        Array.update (argUses, 0, maxUse(! instrCount, Array.sub(argUses, 0)));
                        OGCExtract (OGCLoadRecursive, lastReference)
                    end

                | locaddr (BICLoadClosure laddr) =
                    let
                        val () = instrCount := !instrCount+1;
                        val lastReference = Array.sub(argUses, 0) = 0 andalso not (!outsideLoopRef)
                    in
                        (* Mark the closure as used. *)
                        Array.update (argUses, 0, maxUse(! instrCount, Array.sub(argUses, 0)));
                        OGCExtract (OGCLoadClosure laddr, lastReference)
                    end
              (* locaddr *)

                (* Map f onto a list tail first.  N.B. It doesn't reverse the list.
                   Generally used to map "insert" over a list where we need to
                   ensure that last references to variables are detected correctly. *)
                fun mapright _ [] = []
                |   mapright f (a::b) =
                    let
                        val rest = mapright f b
                    in
                        f a :: rest
                    end

                fun insert(BICEval { function, argList, resultType, ...}) =
                    let
                        (* Process the arguments first. *)
                        val newargs = mapright(fn (c, t) => (insert c, t)) argList
                        (* Then the body. *)
                        val func = insert function
                    in
                        OGCEval {function = func, argList = newargs, resultType=resultType}
                    end

                |   insert(BICBuiltIn0 {oper}) = OGCBuiltIn0 {oper=oper}
                |   insert(BICBuiltIn1 {oper, arg1}) = OGCBuiltIn1{oper=oper, arg1=insert arg1}
                    (* These must be processed in reverse order so the order of the arguments is
                       significant. *)
                |   insert(BICBuiltIn2 {oper, arg1, arg2}) = OGCBuiltIn2{oper=oper, arg2=insert arg2, arg1=insert arg1}
                |   insert(BICBuiltIn3 {oper, arg1, arg2, arg3}) =
                        OGCBuiltIn3{oper=oper, arg3=insert arg3, arg2=insert arg2, arg1=insert arg1}
                |   insert(BICBuiltIn4 {oper, arg1, arg2, arg3, arg4}) =
                        OGCBuiltIn4{oper=oper, arg4=insert arg4, arg3=insert arg3, arg2=insert arg2, arg1=insert arg1}
                |   insert(BICBuiltIn5 {oper, arg1, arg2, arg3, arg4, arg5}) =
                        OGCBuiltIn5{oper=oper, arg5=insert arg5, arg4=insert arg4, arg3=insert arg3, arg2=insert arg2, arg1=insert arg1}

                |   insert(BICExtract ext) = locaddr ext

                |   insert(BICField {base, offset}) = OGCField {base = insert base, offset = offset}

                |   insert(BICConstnt cp) = OGCConstnt cp (* Constants can be returned untouched. *)

                |   insert(BICBeginLoop{loop=body, arguments=argList, ...}) = (* Start of tail-recursive inline function. *)
                    let
                        (* If we have declarations outside the loop that are last used inside it we
                           must make sure they're not marked as last used within the loop.  That would
                           cause the register containing the value to become available for reuse which
                           could mean that when we jumped back to the start of the loop it was no
                           longer there.  We lift all such last-uses out of the loop and add them
                           after the loop. *)
                        val () = instrCount := !instrCount+1
                        val loopEndPosition = !instrCount
                        (* Save the usage state before we process the loop.  Because we're processing
                           the block tail first this indicates all the declarations that are in use
                           AFTER the loop. *)
                        val usagesAfterLoop = saveUsages()
                        val oldLoopEntries = Array.vector outsideLoop
                        (* Set every entry to the "outsideLoop" array to true. *)
                        val () = Array.modify (fn _ => true) outsideLoop
                        val wasInLoop = ! outsideLoopRef
                        val () = outsideLoopRef := true;

                        (* Make entries in the tables for the arguments. I'm not sure
                           if this is essential. *)
                        fun declareArg({addr=caddr, ...}, _) =
                        (
                            Array.update (localUses, caddr, 0);
                            Array.update (outsideLoop, caddr, false) (* Must do this. *)
                        )
                        val _ = List.app declareArg argList

                        (* Process the body. *)
                        val insBody = insert body

                        (* We want to set the arguments to "unreferenced".  These are effectively local to
                           the loop so they can safely be killed inside it.  However we want to record
                           the final references so we can attach them to the declarations. *)
                        local
                            fun processDec({addr, ...}, _) =
                                Array.sub(localUses, addr) before Array.update (localUses, addr, 0)
                        in
                            val loopArgUses = List.map processDec argList
                        end

                        val usagesBeforeLoop = saveUsages()
                        (* Restore the state. *)
                        val () = outsideLoopRef := wasInLoop
                        val () = Array.copyVec{src=oldLoopEntries, dst=outsideLoop, di=0}
                        val (killAfter, killBefore) =
                            case computeKillSetLists [usagesAfterLoop, usagesBeforeLoop] of
                                [thenKill, elseKill] => (thenKill, elseKill)
                            |   _ => raise InternalError "computeKillSets"
                        val _ = null killBefore orelse raise InternalError "Not killBefore"
                        (* Set the lifetime of everything in the killAfter set to be the
                           end of the loop.  Since their last references are inside the loop
                           this means extending the lifetime until the end. *)
                        local
                            fun extendLife(OGCExtract(OGCLoadArgument addr, _)) =
                                    Array.update (argUses, addr+1, loopEndPosition)
                            |   extendLife(OGCExtract(OGCLoadLocal addr, _)) =
                                    Array.update (localUses, addr, loopEndPosition)
                            |   extendLife(OGCExtract(OGCLoadRecursive, _)) =
                                    Array.update (argUses, 0, loopEndPosition)
                            |   extendLife _ = raise InternalError "Not an Extract"
                        in
                            val () = List.app extendLife killAfter
                        end
                        (* Finally the initial argument values. *)
                        local
                            fun copyDec(({addr, value, ...}, _), uses) =
                                {addr=addr, value=insert value, references=uses}
                        in
                            val newargs = mapright copyDec (ListPair.zipEq(argList, loopArgUses))
                        end
                        val loop =
                            OGCBeginLoop{loop=insBody, arguments=ListPair.zipEq(newargs, List.map #2 argList)}
                    in
                        (* Add the kill entries on after the loop. *)
                        if null killAfter then loop
                        else OGCKillItems{expression=loop, killSet=killAfter, killBefore=false}
                    end
    
                |   insert(BICLoop argList) = (* Jump back to start of tail-recursive function. *)
                            OGCLoop(mapright(fn (c, t) => (insert c, t)) argList)

                |   insert(BICRaise x) = OGCRaise (insert x)

                    (* See if we can use a case-instruction. Arguably this belongs
                       in the optimiser but it is only really possible when we have
                       removed redundant declarations. *)
                |   insert(BICCond(condTest, condThen, condElse)) =
                            copyCond (condTest, condThen, condElse)

                |   insert(BICNewenv(ptElistDecs, ptExp)) =
                    let
                        (* Process the body. Recurses down the list of declarations
                           and expressions processing each, and then reconstructs the
                           list on the way back. *)
                        fun copyDeclarations ([])   = []

                        |   copyDeclarations ((BICDeclar{addr=caddr, value=pt}) :: vs)  =
                            let
                                (* Set the table entries.  We don't reuse entries so this is just a check. *)
                                val _ = Array.sub(localUses, caddr) <> 0
                                           andalso raise InternalError "copyDeclarations: Already used"
                                val () = Array.update (outsideLoop, caddr, false) (* It's local *)
                                val rest = copyDeclarations vs
                                val wasUsed = Array.sub(localUses, caddr)
                                val () = instrCount := !instrCount+1
                            in
                                (* It is never used and it has no side-effects so we can ignore it. *)
                                if wasUsed = 0 andalso sideEffectFree pt
                                then rest
                                else
                                let
                                    val dec = insert pt
                                in
                                    (* Set the use count back to free otherwise this local
                                       declaration would become part of the kill set for the
                                       surrounding expression. *)
                                    Array.update (localUses, caddr, 0);
                                    mkDecRef(dec, caddr, wasUsed) :: rest
                                end
                            end (* copyDeclarations.isDeclar *)

                        |   copyDeclarations (BICRecDecs mutualDecs :: vs)  =
                            let
                                (* Recursive declarations. *)
                                (* Get the closure lists for all the declarations.  We assume that
                                   any of these can call any of the others so we just accumulate
                                   them into a single list. *)
                                local
                                    fun getClosure({lambda={closure, ...}, ...}, fcClosures) = (closure @ fcClosures)
                                    val fcClosures = List.foldl getClosure [] mutualDecs
                                in
                                    val fullClosureList = fcClosures
                                end

                                (* Make the declarations. *)
                                local
                                    fun applyFn{addr=caddr, ...} =     
                                        (
                                            Array.sub(localUses, caddr) <> 0 andalso raise InternalError "applyFn: Already used";
                                            Array.update (outsideLoop, caddr, false) (* It's local *)
                                        )
                                in
                                    val () = List.app applyFn mutualDecs
                                end
                  
                                (* Process the rest of the block. Identifies all other
                                   references to these declarations. *)
                                val restOfBlock = copyDeclarations vs

                                val () = instrCount := !instrCount+1

                                (* Process the closure entries and extract the ones that are the last refs. *)
                                val lastRefsForClosure =
                                    List.map OGCNullBinding
                                        (List.filter (fn OGCExtract(_, true) => true | _ => false)
                                            (map (insert o BICExtract) fullClosureList))

                                val copiedDecs =
                                    map (fn {addr, lambda} => {addr=addr, lambda=copyLambda lambda, references= 0})
                                        mutualDecs
           
                                (* Now we know all the references we can complete
                                   the declaration and put on the use-count. *)
                                fun copyEntries []      = []
                                |   copyEntries ({ addr, lambda, ...} ::ds) =
                                    let
                                        val wasUsed = Array.sub(localUses, addr)
                                    in
                                        if wasUsed = 0 (*andalso sideEffectFree value*)
                                        then copyEntries ds
                                        else 
                                        (
                                            (* Set the use count back to false otherwise this
                                               entry would become part of the kill set for the
                                               surrounding expression. *)
                                            Array.update(localUses, addr, 0);
                                            {lambda=lambda, addr=addr, references=wasUsed} :: copyEntries ds
                                        )
                                    end

                                val decs = copyEntries copiedDecs
                            in
                                (* Return the mutual declarations and the rest of the block. *)
                                if null decs
                                then lastRefsForClosure @ restOfBlock
                                else OGCRecDecs decs :: (lastRefsForClosure @ restOfBlock)
                            end (* copyDeclarations.isMutualDecs *)

                        |   copyDeclarations (BICNullBinding v :: vs)  =
                            let (* Not a declaration - process this and the rest. *)
                               (* Must process later expressions before earlier
                                   ones so that the last references to variables
                                   are found correctly. *)
                                val copiedRest = copyDeclarations vs
                                val copiedNode = insert v
                            in
                                (* Expand out blocks *)
                                case copiedNode of
                                    OGCNewenv(decs, exp) => decs @ (OGCNullBinding exp :: copiedRest)
                                |   _ => OGCNullBinding copiedNode :: copiedRest
                            end (* copyDeclarations *)

                        val insElist = copyDeclarations(ptElistDecs @ [BICNullBinding ptExp])

                        (* TODO: Tidy this up. *)
                        fun splitLast _ [] = raise InternalError "decSequenceWithFinalExp: empty"
                        |   splitLast decs [OGCNullBinding exp] = (List.rev decs, exp)
                        |   splitLast _ [_] = raise InternalError "decSequenceWithFinalExp: last is not a NullDec"
                        |   splitLast decs (hd::tl) = splitLast (hd:: decs) tl

                    in
                        case splitLast [] insElist of
                            ([], exp) => exp
                        |   (decs, exp) => OGCNewenv(decs, exp)
                    end (* isNewEnv *)
                
                |   insert(BICTuple recs) = (* perhaps it's a constant now? *)
                        OGCTuple (mapright insert recs) 

                |   insert BICLdexc = OGCLdexc (* just a constant so return it *)
      
                |   insert(BICLambda lambda) = OGCLambda(copyLambda lambda)
    
                |   insert(BICHandle { exp, handler }) =
                    let
                        (* The order here is important.  We want to make sure that
                           the last reference to a variable really is the last. *)
                       val hand = insert handler
                       val exp = insert exp
                    in
                      OGCHandle {exp = exp, handler = hand}
                    end

                |   insert(BICContainer c) = OGCContainer c

                |   insert(BICSetContainer {container, tuple, filter}) =
                        OGCSetContainer{container = insert container, tuple = insert tuple, filter = filter}

                |   insert(BICTagTest{test, tag, maxTag}) = OGCTagTest{test=insert test, tag=tag, maxTag=maxTag}
            
                |   insert(BICCase{cases, test, caseType, default}) =
                    let
                        (* We need to compute the usages for each of the branches: i.e. the
                           default plus each of the cases.  Because they are done in parallel
                           any of the branches contains the last reference of a variable then
                           we need to add kill entries to the other branches so that every
                           branch contains either a "real" final usage or a kill entry. *)
                        val usagesAfterCase = saveUsages()
                        val insDefault = insert default
                        val defaultUsage = saveUsages()
                        val () = setToSaved usagesAfterCase
                        fun processCase(c, tag) =
                        let
                            val () = setToSaved usagesAfterCase
                            val insCase = insert c
                            val caseUsage = saveUsages()
                        in
                            ((insCase, tag), caseUsage)
                        end
                        val (caseList, usageList) = ListPair.unzip (List.map processCase cases)
                        val kills = computeKillSetLists(defaultUsage :: usageList)
                        val casePlusKills = ListPair.mapEq(fn ((c, t), k) => (addKillSet(c, k), t)) (caseList, tl kills)
                        (* Restore the overall usage by setting the reference to the union of all the branches. *)
                        val () = List.app addFromSaved(defaultUsage :: usageList)
                    in
                        OGCCase{cases=casePlusKills, test=insert test, caseType=caseType,
                             default=addKillSet(insDefault, hd kills)}
                    end

                |   insert(BICLoadOperation{kind, address}) = OGCLoadOperation{address=copyAddress address, kind=kind}

                |   insert(BICStoreOperation{kind, address, value}) =
                        OGCStoreOperation{value=insert value, address=copyAddress address, kind=kind}

                |   insert(BICBlockOperation{kind, sourceLeft, destRight, length}) =
                        OGCBlockOperation{length=insert length, destRight=copyAddress destRight,
                            sourceLeft=copyAddress sourceLeft, kind=kind}

               and copyLambda({body=lambdaBody, argTypes,
                                 name=lambdaName, resultType, localCount, closure, heapClosure, ...}: bicLambdaForm) = 
                let
                    val numArgs = List.length argTypes
                    (* The size is one more than the number of arguments because the
                       first item is used to represent the closure.  Argument n is represented
                       by index n+1.  This is a historical anomaly and ought to be fixed
                       by using a separate ref for the closure. *)
                    val argUses      = Array.array(numArgs+1, 0)

                    (* process the body *)
                    val insertedCode: oldGenCodeIC = copyCode (lambdaBody, argUses, localCount)
                    (* All the closure entries ought to be loads but there used to be cases
                       where functions were compiled late in the process and then appeared
                       as constants in the closure.  Include this check for the moment. *)
                    val copiedClosure =
                        map(fn OGCExtract a => OGCExtract a | _ => raise InternalError "map closure") (mapright (insert o BICExtract) closure)
            
                    val argUseList = Array.foldr(op ::) [] argUses
                in
                    {
                        body          = insertedCode,
                        name          = lambdaName,
                        closure       = copiedClosure,
                        argTypes      = argTypes,
                        resultType    = resultType,
                        closureRefs   = hd argUseList,
                        localCount    = localCount,
                        heapClosure   = heapClosure,
                        argLifetimes  = tl argUseList (* Exclude the first item used for the closure itself. *)
                    }
                end


              and copyCond (condTest, condThen, condElse) =
                let
                    (* Process each of the arms, computing the kill sets for
                     each arm. *)
                    (* Save the current usage set.  Because we process the
                       codetree in reverse order to the control flow entries
                       in here show the variables which are in use after the
                       if-expression has completed. *)
                    val usagesAfterIf = saveUsages();

                    (* Process the then-part.  Save the usage set which
                       corresponds to variables which are in use in the
                       flow of control through the then-part and afterwards. *)
                    val insThen = insert condThen;
                    val thenUsage = saveUsages();

                    (* Reset the use-counts to the saved value. *)
                    val () = setToSaved usagesAfterIf;

                    (* Process the else-part. *)
                    val insElse = insert condElse;
                    val elseUsage = saveUsages();

                    (* Now compute the differences of the sets.
                       The differences are returned as Extract codetree entries. *)
                    val (killElseOnly, killThenOnly) =
                        case computeKillSetLists [thenUsage, elseUsage] of
                            [thenKill, elseKill] => (thenKill, elseKill)
                        |   _ => raise InternalError "computeKillSets"
                    (* Now ensure that all the variables that were used in the
                     then-part are marked as used.  It may be that they have already
                     been set if they also appeared in the else-part.
                     This sets the usage sets to the union of the then-part,
                     the else-part and code after the if-expression. *)
                    val () = addFromSaved thenUsage

                    (* Add kill entries to the other branch.  We simply add
                       Extract entries with lastRef=true before the appropriate
                       branch.  This does what we want since the code-generator
                       does not generate any code for them but it might make
                       the intermediate code easier to read if we used a different
                       instruction. *)

                    (* Process the condition AFTER the then- and else-parts. *)
                    val insFirst = insert condTest
                in
                    OGCCond (insFirst, addKillSet(insThen, killElseOnly), addKillSet(insElse, killThenOnly))
                end
                
            and copyAddress {base, index, offset} =
                (* Address - process index before the base. *)
                {index=Option.map insert index, base=insert base, offset=offset}

            in     
                insert pt
            end (* copyCode *)
         
            val insertedCode: oldGenCodeIC = copyCode (pt, Array.array(0, 0), localAddressCount);
        in
            insertedCode
        end (* lifeTimes *)
    
    end (* local *)

    local
        open Address
    in
        val F_mutable_words = Word8.orb (F_mutable, F_words)
        val F_words = Address.F_words
   
        val word0 = toMachineWord 0
        val word1 = toMachineWord 1

        val DummyValue : machineWord = word0 (* used as result of "raise e" etc. *)
        val False : machineWord = word0     (* false *)
        val True  : machineWord = word1     (* true *)
    end

    (* gets a value from the run-time system; 
       usually this is a closure, but sometimes it's an int.  *)
    val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation;

    val constntTrue  = OGCConstnt(True, [])
    val constntFalse = OGCConstnt(False, [])
    val consntZero   = OGCConstnt(word0, [])

    fun isNoResult NoResult     = true | isNoResult _ = false;

    (* Are we at the end of the function. *)
    datatype tail = EndOfProc of reg | NotEnd

    fun isEndOfProc (EndOfProc _) = true | isEndOfProc _ = false;

    fun chooseMergeRegister(_, EndOfProc res) = UseReg(singleton res)
    |   chooseMergeRegister(NoHint, _) = UseReg generalRegisters
    |   chooseMergeRegister(whereto, _) = whereto

    fun codeToCgType GeneralType = ArgGeneral | codeToCgType FloatingPtType = ArgFP

    fun getPower2 n =
    let
        fun p2 (n, l) =
            if n = 0w1 then SOME l
            else if Word.andb(n, 0w1) = 0w1 then NONE
            else p2(Word.>>(n, 0w1), l+0w1)
    in
        if n = 0w0 then NONE else p2(n,0w0)
    end
    
    fun createProfileObject _ (*functionName*) =
    let
        (* The profile object is a single mutable with the F_bytes bit set. *)
        open Address
        val profileObject = allocWordData(0w1, Word8.orb(F_mutable, F_bytes), toMachineWord 0w0);
    in
        toMachineWord profileObject
    end

    (* Code generate a function or global declaration *)
    fun codegen
       (pt               : oldGenCodeIC,
        declOnPrevLevel  : oldGenCodeLoadForm * (unit -> stackIndex * operations) * ttab -> stackIndex * operations,
        closureLifetime  : int,
        argTypes         : argumentType list,
        argLifetimes     : int list,
        resultType       : argumentType,
        localCount       : int,
        profileObject    : machineWord,
        debugSwitches    : Universal.universal list) : operations * int * bool * reg list =
    let
        val cvec: operations ref = ref []
        val callsAFunction = ref false
        fun codeGenerate(ops: operations, cvec) = cvec := ops @ ! cvec
        (* cgAndRes - various functions in X86FIRST_STAGE return a pair of the code
           and the result.  This separates them out. *)
        fun cgAndRes(opCode, result) = (codeGenerate(opCode, cvec); result)

        (* make the translation table *)
        val transtable = ttabCreate(localCount, debugSwitches)
        (* Map from declaration location to pstack entry. *)
        val decToPstack  = Array.array (localCount, noIndex)

        (* If this is set to one add the allocating function to each tuple. *)
        val addAllocatingFunction =
            DEBUG.getParameter DEBUG.profileAllocationTag debugSwitches = 1

        fun localDeclaration(index, locn, lifeTime) =
        (
            Array.update (decToPstack, locn, index);
            (* If the lifetime is zero remove the item. *)
            if lifeTime = 0
            then incrUseCount (transtable, index, ~1)
            else (setLifetime(transtable, index, lifeTime); [])
        )

        (* Header code for function. *)

        (* Push the return address - may have multiple references because
           we may exit at any of the "tails". *)
        val returnAddress = incsp transtable

        (* If discardClosure is true, all uses of the closure are
           directly-recursive calls which will be handled as "Recursive".
           This doesn't require the function closure as a parameter.
           SPF 22/5/95 
       
           Unfortunately, this is not quite true - we can still embed
           the function in a datatype, so we still require access to
           the closure. However, this is handled by storing the closure
           in the constants section (it *is* a constant) if we have
           any such uses of it.
           SPF 30/5/95 
       
           Note that it's important for correctness that we load "embedded"
           uses of an empty closure from the constants section. If we
           tried to be clever and use the value that we find in closureReg
           at function entry, we would generate bad code. That's because 
           functions with empty closures may get called using the PureCode
           calling convention, which doesn't actually initialise closureReg.
       
           Note also that it's the *calls* to codegen that have to be right,
           since the function that loads the closure is actually a parameter
           to codegen.
           SPF 2/1/97
        *)
        val closureOrSlAddr = parameterInRegister(regClosure, closureLifetime, transtable)
        val () = codeGenerate(activeRegister regClosure, cvec)


        (* Find out which arguments are in which registers. *)
        val argLocations = argRegs (List.map codeToCgType argTypes)
        val numberOfArgsOnStack = List.length(List.filter(not o isSome) argLocations)
        (* Create a vector the actual argument locations.  Those in registers are marked as entries
           on the pstack.  The values may be pushed to the real stack or moved to other registers
           but this will keep track of them.  Those on the stack are represented by negative values. *)
        datatype argLocation = ArgInReg of stackIndex | ArgOnStack of int
        local
            fun mapArgs ([], []) = ([], 0)
            |   mapArgs(SOME reg :: l, life:: lives) =
                    let
                        val (l', n) = mapArgs(l, lives)
                        val () = codeGenerate(activeRegister reg, cvec)
                    in
                        (ArgInReg(parameterInRegister (reg, life, transtable)) :: l', n)
                    end
            |   mapArgs(NONE :: l, _::lives) = let val (l', n) = mapArgs(l, lives) in (ArgOnStack(n-1) :: l', n-1) end
            |   mapArgs _ = raise InternalError "Mismatched argument types/lifetimes"
            val (args, _) = mapArgs(argLocations, argLifetimes)
        in
            val argRegTab = Vector.fromList args
        end
        
        (* Get the registers that must be saved if we take a stack limit trap. *)
        val saveArgRegs = List.mapPartial (fn i=> i) argLocations
        val saveRegs =
            if closureLifetime = 0 then saveArgRegs else regClosure :: saveArgRegs
    
        fun exit () =
        let
            val stackArgs = List.length(List.filter(not o isSome) argLocations)
            val exitCode = (* Reset to just above the return address. *)
                returnFromFunction stackArgs @ resetStack (realstackptr transtable - 1)
        in
            exiting transtable;
            exitCode
        end

        (* Allocate a segment of the required size. *)
        fun callgetvec (csize, flag, whereto, transtable) : stackIndex * operations =
        let
            (* Get a register for the result. *)
            val (resultReg, regCode) =
                getRegisterInSet(transtable, case whereto of UseReg rr => rr | _ => generalRegisters)
        
            val resAddr = pushReg (transtable, resultReg)

            (* Get the active register set after we've spilled any registers. *)
            val activeRegs = findActiveRegisters transtable
        in
            if addAllocatingFunction
            then
            let
               val moveCode = moveToVec (resAddr, pushConst(transtable, profileObject), csize, transtable)
            in
                (resAddr, moveCode @
                             allocStore {size=csize+1, flags=Word8.orb(flag, Address.F_profile), output=resultReg,
                                         saveRegs=activeRegs} @ regCode)
            end
            else (resAddr, allocStore {size=csize, flags=flag, output=resultReg, saveRegs=activeRegs} @ regCode)
        end;

        (*infix 9 sub;*)

        (* Loads a local, argument or closure value; translating local
           stack addresses to real stack offsets.
           N.B. In the case of non-local variables lastRef is true only for
           the last non-local variable, not the last use of this particular
           variable. *)
        fun locaddr (OGCLoadArgument addr, lastRef) =
            ( (* The arguments are numbered from -n upto -1.  The first few arguments are
                   in registers and the rest on the stack. *)
                case Vector.sub(argRegTab, addr) of
                    ArgInReg regEntry =>
                    (
                        (* If this is NOT the last reference we need to increment the
                           use count on the entry. *)
                        if lastRef then () else (incrUseCount(transtable, regEntry, 1); ());
                        (regEntry, [])
                    )
                |   ArgOnStack actualAddr => (pushStack (transtable, actualAddr), [])
            )
        |   locaddr (OGCLoadLocal addr, lastRef) =
            (*  reference to entry on the pstack. *)
            let
                val resIndex = Array.sub(decToPstack, addr)
                val freeCode =
                    if lastRef
                    then []
                        (* Last reference.  When we've finished with this entry it will be discarded. *)
                    else (* There's at least one more reference after this. *)
                        incrUseCount(transtable, resIndex, 1)
            in
                (resIndex, freeCode)
            end

        |   locaddr(closureOrRecursive, lastRef) =  (* cp relative *)
            let
                (* If this is the last reference to the closure we want
                   it to be removed afterwards.  makeSl is not always called
                   if, for example, the value is constant.  To ensure the
                   use-count is correct we increment it if it is used and
                   then decrement it afterwards.  DCJM 2/12/99. *)
                val (dec, code) =
                    declOnPrevLevel(closureOrRecursive,
                        fn () => (incrUseCount(transtable, closureOrSlAddr, 1); (closureOrSlAddr, [])),
                        transtable)
                val freeCode =
                    if lastRef andalso closureLifetime <> 0 
                    then incrUseCount(transtable, closureOrSlAddr, ~1) else []
            in
                (dec, freeCode @ code)
            end
         (* locaddr *);
    
        (* For each load of a local in the tree it calls the `add' function. *)
        fun identifyLoads expList add =
        let 
            (* Need to identify declarations within the current block.  This was originally
               there because declaration addresses could at one time be reused.  That shouldn't
               happen now. *)
            val newDecs : bool StretchArray.stretchArray =
               StretchArray.stretchArray (4, false)
           
            fun loads pt =
            case pt of
                OGCExtract (OGCLoadArgument locn, lastRef) =>
                (
                    case Vector.sub(argRegTab, locn) of
                        ArgInReg regEntry => if lastRef then add regEntry else ()
                    |   _ => ()
                )

            |   OGCExtract (OGCLoadLocal locn, lastRef) =>
                if not (StretchArray.sub (newDecs,locn)) andalso lastRef
                    (* Ignore new declarations. *)
                then add (Array.sub(decToPstack, locn))
                else ()

               (* If discardClosure is true, then we've already zeroed the
                  use-count for closureOrSlAddr, so don't adjust it now.
                  SPF 22/5/95 *)
            |   OGCExtract (OGCLoadClosure _, lastRef) =>
                if closureLifetime <> 0 (* Non-local *) andalso lastRef
                then add closureOrSlAddr (* Reference to the closure. *)
                else ()
          
            |   OGCEval {function, argList, ...} =>
                (
                    loads function;
                    List.app (fn (l, _) => loads l) argList
                )
            
            |   OGCField {base, ...} => loads base
            
            |   OGCNewenv(decs, exp) =>
                let
                    fun loadDecs(OGCDeclar {addr, value, ...}) =
                        (
                            (* Indicate that this is a new declaration. *)
                            StretchArray.update (newDecs, addr, true);
                            loads value (* Check the expression. *)
                        )
                    |   loadDecs(OGCRecDecs decs) =
                        (
                            (* First process the declarations to ensure that new declarations
                               are marked as such then process the values being declared. *)
                            List.app(
                                fn {addr, ...} => StretchArray.update (newDecs, addr, true)) decs;
                            List.app (fn{lambda, ...} => loads (OGCLambda lambda)) decs
                        )
                    |   loadDecs(OGCNullBinding c) = loads c
                in
                    List.app loadDecs decs;
                    loads exp
                end
            
            |   OGCTuple vl => List.app loads vl

            |   OGCBeginLoop{loop, arguments, ...} =>
                let
                    fun declArg({addr, value, ...}, _) =
                    (
                        (* Indicate that this is a new declaration. *)
                        StretchArray.update (newDecs, addr, true);
                        loads value (* Check the expression. *)
                    )
                in
                    List.app declArg arguments;
                    loads loop
                end

            |   OGCLoop argList => List.app (fn (l, _) => loads l) argList

            |   OGCHandle{exp, handler} => (loads exp; loads handler)

            |   _ => ()
        in
            List.app loads expList
        end
      
    (* code-generates code from the tree *)
    (* SPF 2/5/95 - primBoolOps added to prevent loop when
       trying to inline unsupported boolean primitives. We might
       get the calling sequence:
       
         genEval -> genCond -> genTest -> genOtherTests -> gencde -> genEval
         
       where both versions of genEval are for the same (unsupported)
       boolean comparison. If this occurs, the second call will have
       primBoolOps set to false, and will generate a call to the RTS.
       
       Note that "whereto" is only a HINT. There is no guarantee that specifying
       "UseReg r" will actually get the value loaded into that register. For example,
       the code that handles constants completely ignores this hint.
       SPF 15/8/96
     *)
    fun gencde (pt, primBoolOps, whereto, tailKind, loopAddr) : mergeResult =
    let 
      val needsResult : bool = not (isNoResult whereto)
      
      val result : mergeResult = 
        case pt of
          OGCEval {function, argList, resultType, ...} =>
            genEval (function, argList, resultType, primBoolOps, whereto, tailKind)
            
        |   OGCBuiltIn0{oper=BuiltIns.CurrentThreadId} => cgAndRes(threadSelf(transtable, whereto))

        |   OGCBuiltIn1{oper=BuiltIns.NotBoolean, arg1} =>
                (* Boolean "not" is coded as xor with "true" *)
                cgAndRes(xorWord(genToStack arg1, pushConst (transtable, True), transtable, whereto))

        |   instr as OGCBuiltIn1{oper=BuiltIns.IsTaggedValue, ...} => codeAsConditional(instr, whereto, tailKind)

        |   OGCBuiltIn1{oper=BuiltIns.MemoryCellLength, arg1} => cgAndRes(vectorLength(genToStack arg1, transtable, whereto))

        |   OGCBuiltIn1{oper=BuiltIns.MemoryCellFlags, arg1} => cgAndRes(vectorFlags(genToStack arg1, transtable, whereto))

        |   OGCBuiltIn1{oper=BuiltIns.ClearMutableFlag, arg1} => cgAndRes(lockVector(genToStack arg1, transtable, whereto))

        |   OGCBuiltIn1{oper=BuiltIns.StringLengthWord, arg1} => cgAndRes(stringLength(genToStack arg1, transtable, whereto))

        |   OGCBuiltIn1{oper=BuiltIns.AtomicIncrement, arg1} => cgAndRes(atomicIncrement(genToStack arg1, transtable, whereto))

        |   OGCBuiltIn1{oper=BuiltIns.AtomicDecrement, arg1} => cgAndRes(atomicDecrement(genToStack arg1, transtable, whereto))

        |   OGCBuiltIn1{oper=BuiltIns.AtomicReset, arg1} => cgAndRes(atomicReset(genToStack arg1, transtable, whereto))

        |   OGCBuiltIn1{oper=BuiltIns.LongWordToTagged, arg1} =>
                (* We can use the same operation as loading the length word of a string. *)
                cgAndRes(stringLength(genToStack arg1, transtable, whereto))

        |   OGCBuiltIn1{oper=BuiltIns.SignedToLongWord, arg1} =>
                cgAndRes(wordToLargeWord(true, genToStack arg1, transtable, whereto))

        |   OGCBuiltIn1{oper=BuiltIns.UnsignedToLongWord, arg1} =>
                cgAndRes(wordToLargeWord(false, genToStack arg1, transtable, whereto))

        |   instr as OGCBuiltIn2{oper=BuiltIns.WordComparison _, ...} => codeAsConditional(instr, whereto, tailKind)

        |   OGCBuiltIn2{oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithAdd, arg1, arg2} =>
                cgAndRes(addFixed(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithSub, arg1, arg2} =>
                cgAndRes(subtractFixed(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithMult, arg1, arg2} =>
                cgAndRes(multiplyFixed(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithQuot, arg1, arg2} =>
                cgAndRes(quotFixed(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.FixedPrecisionArith BuiltIns.ArithRem, arg1, arg2} =>
                cgAndRes(remFixed(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1, arg2} =>
                cgAndRes(addWord(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.WordArith BuiltIns.ArithSub, arg1, arg2} =>
                cgAndRes(subtractWord(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.WordArith BuiltIns.ArithMult, arg1, arg2} =>
            let
                open Address
                (* If the value is a power of two we use a shift *)
                val useShift =
                    case (arg1, arg2) of
                        (OGCConstnt(lit, _), _) => (case getPower2(toShort lit) of SOME shift => SOME(shift, arg2) | NONE => NONE)
                    |   (_, OGCConstnt(lit, _)) => (case getPower2(toShort lit) of SOME shift => SOME(shift, arg1) | NONE => NONE)
                    |   _ => NONE
            in
                case useShift of
                    SOME(shift, arg) => cgAndRes(upShiftWordConstant(genToStack arg, shift, transtable, whereto))
                |   _ => cgAndRes(multiplyWord(genToStack arg1, genToStack arg2, transtable, whereto))
            end

        |   OGCBuiltIn2{oper=BuiltIns.WordArith BuiltIns.ArithQuot, arg1, arg2} =>
            (
                (* Use a right shift if it's a power of two. *)
                case arg2 of
                    OGCConstnt(lit, _) =>
                    (
                        case getPower2(Address.toShort lit) of
                            SOME shift => cgAndRes(downShiftWordConstant(genToStack arg1, shift, transtable, whereto))
                        |   _ => cgAndRes(divideWord(genToStack arg1, genToStack arg2, transtable, whereto))
                    )
                |   _ => cgAndRes(divideWord(genToStack arg1, genToStack arg2, transtable, whereto))
            )
 
        |   OGCBuiltIn2{oper=BuiltIns.WordArith BuiltIns.ArithRem, arg1, arg2} =>
            (
                (* Use a mask if it's a power of two. *)
                case arg2 of
                    OGCConstnt(lit, _) =>
                    (
                        case getPower2(Address.toShort lit) of
                            SOME _ =>
                            let
                                (* The divisor is a power of two.  The mask is one less. *)
                                val mask = Address.toMachineWord(Address.toShort lit - 0w1)
                            in
                                cgAndRes(andWord(genToStack arg1, pushConst (transtable, mask), transtable, whereto))
                            end
                        |   _ => cgAndRes(modulusWord(genToStack arg1, genToStack arg2, transtable, whereto))
                    )
                |   _ => cgAndRes(modulusWord(genToStack arg1, genToStack arg2, transtable, whereto))
            )

        |   OGCBuiltIn2{oper=BuiltIns.SetStringLengthWord, arg1, arg2} =>
                cgAndRes(setStringLength(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.WordLogical BuiltIns.LogicalAnd, arg1, arg2} =>
                cgAndRes(andWord(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.WordLogical BuiltIns.LogicalOr, arg1, arg2} =>
                cgAndRes(orWord(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.WordLogical BuiltIns.LogicalXor, arg1, arg2} =>
                cgAndRes(xorWord(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.WordShift BuiltIns.ShiftLeft, arg1, arg2} =>
            (
                case arg2 of
                    OGCConstnt(lit, _) => (* If it's a constant it should be short since it's a word. *)
                        cgAndRes(upShiftWordConstant(genToStack arg1, Address.toShort lit, transtable, whereto))
                |   _ => cgAndRes(upShiftWordVariable(genToStack arg1, genToStack arg2, transtable, whereto))
            )

        |   OGCBuiltIn2{oper=BuiltIns.WordShift BuiltIns.ShiftRightLogical, arg1, arg2} =>
            (
                case arg2 of
                    OGCConstnt(lit, _) => (* If it's a constant it should be short since it's a word. *)
                        cgAndRes(downShiftWordConstant(genToStack arg1, Address.toShort lit, transtable, whereto))
                |   _ => cgAndRes(downShiftWordVariable(genToStack arg1, genToStack arg2, transtable, whereto))
            )

        |   OGCBuiltIn2{oper=BuiltIns.WordShift BuiltIns.ShiftRightArithmetic, arg1, arg2} =>
            (
                case arg2 of
                    OGCConstnt(lit, _) => (* If it's a constant it should be short since it's a word. *)
                        cgAndRes(downShiftWordArithmeticConstant(genToStack arg1, Address.toShort lit, transtable, whereto))
                |   _ => cgAndRes(downShiftWordArithmeticVariable(genToStack arg1, genToStack arg2, transtable, whereto))
            )

        |   OGCBuiltIn2{oper=BuiltIns.AllocateByteMemory, arg1, arg2} =>
                cgAndRes(allocStoreUninitialised(genToStack arg1, genToStack arg2, transtable, whereto))

        |   instr as OGCBuiltIn2{oper=BuiltIns.LargeWordComparison _, ...} => codeAsConditional(instr, whereto, tailKind)

        |   OGCBuiltIn2{oper=BuiltIns.LargeWordArith BuiltIns.ArithAdd, arg1, arg2} =>
                cgAndRes(addLargeWord(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.LargeWordArith BuiltIns.ArithSub, arg1, arg2} =>
                cgAndRes(subtractLargeWord(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.LargeWordArith BuiltIns.ArithMult, arg1, arg2} =>
                (* TODO: We could use a shift as with Word but N.B. the constants are addresses. *)
                cgAndRes(multiplyLargeWord(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.LargeWordArith BuiltIns.ArithQuot, arg1, arg2} =>
                (* TODO: Use shifts. *)
                cgAndRes(divideLargeWord(genToStack arg1, genToStack arg2, transtable, whereto))
 
        |   OGCBuiltIn2{oper=BuiltIns.LargeWordArith BuiltIns.ArithRem, arg1, arg2} =>
                (* TODO: Use shifts. *)
                cgAndRes(modulusLargeWord(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.LargeWordLogical BuiltIns.LogicalAnd, arg1, arg2} =>
                cgAndRes(andLargeWord(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.LargeWordLogical BuiltIns.LogicalOr, arg1, arg2} =>
                cgAndRes(orLargeWord(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.LargeWordLogical BuiltIns.LogicalXor, arg1, arg2} =>
                cgAndRes(xorLargeWord(genToStack arg1, genToStack arg2, transtable, whereto))

        |   OGCBuiltIn2{oper=BuiltIns.LargeWordShift BuiltIns.ShiftLeft, arg1, arg2} =>
            (
                case arg2 of
                    OGCConstnt(lit, _) => (* If it's a constant it should be short since it's a word. *)
                        cgAndRes(upShiftLargeWordConstant(genToStack arg1, Address.toShort lit, transtable, whereto))
                |   _ => cgAndRes(upShiftLargeWordVariable(genToStack arg1, genToStack arg2, transtable, whereto))
            )

        |   OGCBuiltIn2{oper=BuiltIns.LargeWordShift BuiltIns.ShiftRightLogical, arg1, arg2} =>
            (
                case arg2 of
                    OGCConstnt(lit, _) => (* If it's a constant it should be short since it's a word. *)
                        cgAndRes(downShiftLargeWordConstant(genToStack arg1, Address.toShort lit, transtable, whereto))
                |   _ => cgAndRes(downShiftLargeWordVariable(genToStack arg1, genToStack arg2, transtable, whereto))
            )

        |   OGCBuiltIn2{oper=BuiltIns.LargeWordShift BuiltIns.ShiftRightArithmetic, arg1, arg2} =>
            (
                case arg2 of
                    OGCConstnt(lit, _) => (* If it's a constant it should be short since it's a word. *)
                        cgAndRes(downShiftLargeWordArithmeticConstant(genToStack arg1, Address.toShort lit, transtable, whereto))
                |   _ => cgAndRes(downShiftLargeWordArithmeticVariable(genToStack arg1, genToStack arg2, transtable, whereto))
            )

        |   OGCBuiltIn3{oper=BuiltIns.StoreWord, arg1, arg2, arg3} =>
                cgAndRes(storeWord(genToStack arg1, genToStack arg2, genToStack arg3, transtable, whereto))

        |   OGCBuiltIn3{oper=BuiltIns.StoreByte, arg1, arg2, arg3} =>
                cgAndRes(storeByte(genToStack arg1, genToStack arg2, genToStack arg3, transtable, whereto))

        |   OGCBuiltIn3{oper=BuiltIns.AllocateWordMemory, arg1, arg2, arg3} =>
            let
                (* This is used to allocate all memory apart from tuples and
                   closures.  We particularly want to be able to profile the
                   allocation of refs so we consider them specially. *)
                open Address
            in
                case (arg1, arg2) of
                    (OGCConstnt(len, _), OGCConstnt(flag, _)) =>
                        (* Both the length and the flags are constants *)
                        if not(isShort len andalso isShort flag andalso toShort flag <= 0w255)
                        then raise InternalError "AllocateWordMemory - invalid length or flag"
                        else
                        let
                            val sFlag = Word8.fromLargeWord(Word.toLargeWord(toShort flag))
                            and sLen = toShort len
                        in
                            if sLen = 0w1 andalso sFlag = F_mutable
                            then
                            let
                                val initLoc = genToStackOrGeneralRegister arg3
                                val (vec, vecCode) = callgetvec (1, F_mutable, whereto, transtable)
                                val () = codeGenerate(vecCode, cvec)
                                val moveCode = moveToVec (vec, initLoc, 0, transtable)
                                val () = codeGenerate(moveCode, cvec)
                                val () = codeGenerate(allocationComplete, cvec)
                            in
                                case whereto of
                                    NoResult => NoMerge (* Unused. *)
                                |   _ => MergeIndex vec
                            end
                            else if sLen < 0w5 andalso (Word8.andb(sFlag, 0w3)) = F_words
                                (* Small fixed size allocations of words. *)
                            then cgAndRes(allocateStoreSmallFixedSize(Word.toInt sLen, sFlag, genToStack arg3, transtable, whereto))
                            else cgAndRes(allocStoreAndInitialise(genToStack arg1, genToStack arg2, genToStack arg3, transtable, whereto))
                        end

                |  _ => (* Variable flags. *)
                    cgAndRes(allocStoreAndInitialise(genToStack arg1, genToStack arg2, genToStack arg3, transtable, whereto))

            end

        |   OGCBuiltIn4{oper=BuiltIns.Built4PlaceHolder, ...} => raise InternalError "TODO: OGCBuiltIn4"

        |   instr as OGCBuiltIn5{oper=BuiltIns.ByteVecEqual, ...} => codeAsConditional(instr, whereto, tailKind)

        |   OGCBuiltIn5{oper=BuiltIns.ByteVecCompare, arg1, arg2, arg3, arg4, arg5} =>
                cgAndRes(
                    byteVecComparison(genToStack arg1, genToStack arg2, genToStack arg3, 
                                      genToStack arg4, genToStack arg5, transtable, whereto))

        |   OGCBuiltIn5{oper=BuiltIns.BlockMove {isByteMove=true}, arg1, arg2, arg3, arg4, arg5} =>
                cgAndRes(moveBytes(genToStack arg1, genToStack arg2, genToStack arg3, genToStack arg4, genToStack arg5, transtable, whereto))
        
        |   OGCBuiltIn5{oper=BuiltIns.BlockMove {isByteMove=false}, arg1, arg2, arg3, arg4, arg5} =>
                cgAndRes(moveWords(genToStack arg1, genToStack arg2, genToStack arg3, genToStack arg4, genToStack arg5, transtable, whereto))

        |   OGCLoadOperation{ kind=LoadStoreMLWord _, address={base, index, offset} } =>
                cgAndRes(
                    loadWord{base=genToStack base, index=genToStack(getOpt(index, consntZero)),
                             byteOffset=offset, transtable=transtable, whereto=whereto})

        |   OGCLoadOperation{ kind=LoadStoreMLByte _, address={base, index, offset} } =>
                cgAndRes(
                    loadByte{base=genToStack base, index=genToStack(getOpt(index, consntZero)),
                             byteOffset=offset, transtable=transtable, whereto=whereto})

        |   OGCStoreOperation _ => raise InternalError "TODO: OGCStoreOperation"
        
        |   OGCBlockOperation _ => raise InternalError "TODO: OGCBlockOperation"

        |   OGCExtract ext =>
            let
                val (loc, locCode) = locaddr ext
                val () = codeGenerate(locCode, cvec)
            in
                if needsResult
                then MergeIndex loc
                else (* If the result is not required discard it.  This is used
                        to remove variables which are not used on this path. *)
                (
                    codeGenerate(removeStackEntry(transtable, loc), cvec);
                    NoMerge
                )
            end

        |   OGCField {base, offset} =>
            let
                val baseCode = genToStack (base)
                val (index, indCode) = indirect (offset, baseCode, transtable)
                val () = codeGenerate(indCode, cvec)
            in  (* Get the value to be indirected on. *)
                MergeIndex index
            end

        |   OGCLambda lam => MergeIndex(genProc (lam, fn _ => (), whereto))

        |   OGCConstnt(w, _) => MergeIndex(pushConst (transtable, w))

        | OGCCond (testPart, thenPart, elsePart) =>
            genCond (testPart, thenPart, elsePart, whereto, tailKind, loopAddr)

        | OGCNewenv(decs, exp) =>
            let (* Processes a list of entries. *)
                val startMark = markStack transtable
                (* We may have the situation where we want the result in a specific register
                   but we actually have a Decl entry followed by an OGCExtract.
                   Don't do this unless we've asked for a specific register. *)
                val specificLoc =
                    case (exp, whereto) of
                        (OGCExtract(OGCLoadLocal addr, _), UseReg _) => SOME(addr, whereto)
                    |   _ => NONE

                val () = List.app (codeBinding specificLoc) decs
                val resultPosn = gencde (exp, true, whereto, tailKind, loopAddr)
                val () = checkBlockResult(transtable, resultPosn)
                val () = unmarkStack(transtable, startMark)
            in
                resultPosn
            end

        | OGCBeginLoop{loop=body, arguments=args} =>
          let
            (* Execute the body which will contain at least one Loop instruction.
               There will also be path(s) which don't contain Loops and these
               will drop through. *)
            (* Load the arguments.  We put them into registers at this stage
               to ensure that constants and "direct" entries are loaded.  They
               may go onto the stack, which is fine. It could be worth doing
               this in two passes, the first simply evaluating the arguments
               onto the pstack, the second loading them into registers since
               that would generate better code when some arguments are constants
               but others are expressions that push those constants onto the stack. *)
            fun genLoopArg ({addr, value, references}, argType) =
            let
                (* This is almost the same as a normal declaration except
                   that we have to make sure that we use a new location, stack or
                   register, since we're going to be changing the contents of
                   this location.  The easiest way to do that is to load it into
                   a register.  We could do better if we are loading the last
                   reference to the initial value in which case we could reuse
                   its location. *)
                val index = genToStack(value)
                (* Put this in a floating point register if it is a floating point value
                   otherwise a fixed point register. *)
                val prefSet =
                    case argType of
                        GeneralType => RegSet.generalRegisters
                    |   FloatingPtType => RegSet.floatingPtRegisters
                val (_, decl, decCode) = loadEntryToSet(transtable, index, prefSet, true)
                val () = codeGenerate(decCode, cvec)
                (* It should not be a non-heap function - just check. *) 
                val _ = 
                  case value of
                    OGCLambda {heapClosure = false, ...} => 
                        raise InternalError "LoopArg: static link function"
                  | _ => ()
            in
                localDeclaration (decl, addr, references);
                (* Normally "references" will be non-zero but it does seem that we
                   can get loop variables that are never used.  This may happen as a
                   result of multiple levels of inline function expansion.  If it's zero
                   we won't have a location for the loop argument. *)
                if references = 0 then noIndex else decl
            end

            val argIndexList = map genLoopArg args;
            (* We need to ensure that the state we return to after the loop is the same
               as it was at the start.  If we find inside the loop that we need to spill
               values from registers that were declared outside we need to move those
               spills to before the loop.  We first process the loop optimistically and
               then reprocess it we find we've had to spill. *)
            fun reprocessLoop n =
            let
                (* Include a check that we don't loop too many times. *)
                val _ = n > 20 andalso raise InternalError "reprocessLoop"
                (* Record the code at the start.  If we have to reprocess we discard everything
                   after this. *)
                val codeAtStart = !cvec
                val initialState = saveState transtable
                (* Now we have loaded the registers we can find out the destinations
                   i.e. the register or stack location they were in at the start of
                   the loop.  We have to do this after we've loaded all the arguments
                   because we may have pushed some onto the stack as we loaded the
                   later ones.  That's fine so long as when we loop we put the new
                   values in the same place.  *)
                val (argDestList, clearOps) = getLoopDestinations(argIndexList, transtable)
                val () = codeGenerate(clearOps, cvec)
                (* Start of loop.  This is where we jump to if the loop is taken. *)
                val (startLoopCode, startLoop) = backJumpLabel()
                val () = codeGenerate(startLoopCode, cvec)
                val startSp = realstackptr transtable
                val cacheSet = ref noRegisters and pushList = ref []

                fun onLoop () =
                (* This function is called whenever we loop.  The state here is the
                   state at the point we take the loop.  We need to record the state
                   at each of those points to produce a composite.  *)
                let
                    val (caches, pushes) = compareLoopStates(transtable, initialState, argIndexList)
                    val () = cacheSet := regSetUnion(caches, !cacheSet)
                    and () = pushList := pushes @ !pushList
                in
                    (* We have to make sure that the real stack pointer is consistent.
                       We may have pushed local values within the loop and these need
                       to be removed. *)
                    codeGenerate(resetStack (realstackptr transtable - startSp), cvec)
                end
                (* Compile the loop with the jumps back to the start. *)
                val runLoop =
                    gencde (body, true, whereto, tailKind,
                        SOME(startLoop, onLoop, argDestList))
                (* The state we have here is the state when we haven't taken the loop. *)
            in
                if ! cacheSet = noRegisters andalso null (! pushList) then runLoop
                else
                (
                    cvec := codeAtStart;
                    codeGenerate(restoreLoopState(transtable, initialState, ! cacheSet, ! pushList), cvec);
                    reprocessLoop(n+1)
                )
            end
          in
            reprocessLoop 0
          end

        | OGCLoop argList =>
            let
                val (startLoop, onLoop, argDestList) =
                    case loopAddr of
                        SOME l => l
                    |   NONE =>
                        raise InternalError "No BeginLoop for Loop instr"
                (* Evaluate the arguments.  Try to put them in the destination
                   register if we can.  It doesn't matter at this stage too much. *)
                fun evalArg((arg, _), dest) =
                let
                    val whereto =
                      case dest of
                            ArgToRegister reg => UseReg (singleton reg)
                        |   ArgToStack _ => NoHint
                        |   ArgDiscard => NoHint
                    val res = gencde (arg, true, whereto, NotEnd, NONE)
                in
                    case res of
                        MergeIndex index => index
                    |   NoMerge => raise InternalError "evalArg: no result"
                end
                    
                val argsOnPstack : stackIndex list =
                    ListPair.map evalArg (argList, argDestList)

                fun moveArgs([], []) = []
                |   moveArgs(arg :: args, ArgToRegister reg :: dests) =
                    let
                        (* Do it in reverse order so that we can delay locking
                           the register arguments. *)
                        val argEntries = moveArgs(args, dests)
                        val (argEntry, argCode) =
                            loadToSpecificReg (transtable, reg, arg, false)
                        val () = codeGenerate(argCode, cvec)
                    in
                        lockRegister(transtable, reg);
                        argEntry :: argEntries
                    end
                |   moveArgs(arg :: args, ArgToStack offset :: dests) =
                    let
                        val (argEntry, code) = storeInStack(transtable, arg, offset)
                    in
                        codeGenerate(code, cvec);
                        argEntry :: moveArgs(args, dests)
                    end
                |   moveArgs(arg :: args, ArgDiscard :: dests) =
                        (* If we're just discarding it return the location so we will
                           remove it from the stack. *)
                        arg :: moveArgs(args, dests)
                |   moveArgs _ =
                        raise InternalError "moveArgs: Mismatched arguments"

                (* the arguments are now all in their rightful places. *)
                val argEntries = moveArgs(argsOnPstack, argDestList);
            in
                (* Remove the entries and unlock the registers.  It may
                   be unnecessary to remove the entries because we're about
                   to fix up a jump but there's no harm in it. *)
                List.app (
                    fn (ArgToRegister reg) => codeGenerate(unlockRegister(transtable, reg), cvec)
                      | _ => ()) argDestList;
                List.app (fn index => codeGenerate(removeStackEntry(transtable, index), cvec))
                    argEntries;
                onLoop();
            
                (* Repeat. *)
                codeGenerate(jumpBack (startLoop, transtable), cvec);
                (* Put on a dummy result. *)
                if needsResult
                then MergeIndex(pushConst (transtable, DummyValue))
                else NoMerge (* Unused. *)
            end

        | OGCRaise exp =>
          let (* movl <exception>,resultReg; jmp raisex *)
            val () =
               (* Ensure the return address is on the stack in case
                  we are tracing exceptions. *)
               codeGenerate(pushSpecificEntry (transtable, returnAddress), cvec);
               
            val excVal = genToStack (exp);
            
            val (resultIndex, resultCode) = 
               loadToSpecificReg (transtable, resultReg ArgGeneral, excVal, true);

          in
            codeGenerate(raiseException @ resultCode, cvec);
            codeGenerate(removeStackEntry(transtable, resultIndex), cvec);
            exiting transtable; (* Nothing further *)

            (* Put a dummy value on the stack so that subsequent merge code works
               It really ought to ignore this since we've exited. *)
            if needsResult
            then MergeIndex(pushConst (transtable, DummyValue))
            else NoMerge (* Unused. *)
          end

        | OGCHandle {exp, handler} =>
            let
                (* Push all regs - we don't know what the state will be when 
                   we reach the handler. *)
                (* i.e. Push all registers except those whose last use occurs in the expression
                   we're handling. *) 
                val () = 
                    codeGenerate(pushAllBut (transtable, identifyLoads[exp], allRegisters), cvec);
                (* It's not clear what registers will be modified as a result of raising
                 and handling an exception.  Many functions may result in exceptions
                 being raised and rather than add the registers to the register set of
                 those functions it's probably better to include them in the modification
                 set here. DCJM 26/11/00. *)
                val _ = addModifiedRegSet(transtable, allRegisters);

                (* This is the real stack state at the start of the handler *)
                val startOfHandler = realstackptr transtable;
          
                (* Remember this pseudo-stack position for later merge *)
                val mark = markStack transtable

                (* Save old handler - push regHandler *)
                val () = codeGenerate(pushCurrentHandler, cvec)
                val oldIndex = incsp transtable
          
                (* Now it's on the real stack we can remove it from the pstack. *)
                local
                    (* Push address of new handler. *)
                    val rsp         = realstackptr transtable
                    val (handlerEntry, handlerLab, handlerCode)  = pushAddress (transtable, rsp + 1)
                    val () = codeGenerate(handlerCode, cvec)
    
                    (* Set the current handler to the stack pointer after these items. *)
                    val () = codeGenerate(storeToHandler regStackPtr, cvec)
                in
                    val handlerLab = handlerLab
                    and handlerEntry = handlerEntry
                end

                val whereto = chooseMergeRegister(whereto, tailKind)
 
                (* Code generate body, putting the result in result register. *)
                (* "NotEnd" because we have to come back to remove the handler. *)
                val bodyResult = genToRegister (exp, whereto, NotEnd, loopAddr);
                (* Reload the old value of regHandler i.e. remove handler. *)
                (* Remove the handler entries. *)
                val () = codeGenerate(removeStackEntry(transtable, handlerEntry), cvec)
                val () = codeGenerate(reloadHandler(transtable, oldIndex), cvec)

                (* Optimisation: return immediately, if possible, rather than
                   jumping and then returning. This may turn the following
                   unconditional branch into dead code, in which case it
                   will be removed by the lower-level code generator. *)
                val () =
                    if isEndOfProc tailKind andalso not (haveExited transtable)
                    then codeGenerate(exit (), cvec)
                    else ()
    
                (* Skip over the handler. *)
                val (skipHandler, skipCode) = unconditionalBranch (bodyResult, transtable)
                val () = codeGenerate(skipCode, cvec)
          
                (* Remove any result at the start of the handler.
                   Need this because fixupH does not do setState.
                   (It probably should do, though the state is fairly simple). *)
                val () =
                    case bodyResult of
                        MergeIndex bodyIndex => codeGenerate(removeStackEntry(transtable, bodyIndex), cvec)
                    |   NoMerge => ()
     
                (* Fix up the handler entry point - this resets the stack pointer
                   and clears the cache since the state is not known. *)
                val () = codeGenerate(fixupH (handlerLab, startOfHandler, transtable), cvec)

                (* The code for the handler body itself *)
                val handlerRes =  genToRegister (handler, whereto, tailKind, loopAddr)
                (* Merge the results. *)
                val (mergeRes, mergeCode) = merge (skipHandler, transtable, handlerRes, mark)
                val () = codeGenerate(mergeCode, cvec)
            in
                mergeRes
            end
        
        | OGCLdexc =>
            let
                val regResult = resultReg ArgGeneral
                (* Exception packet is returned in result register. *)
            in
                codeGenerate(getRegister (transtable, regResult), cvec);
                codeGenerate(activeRegister regResult, cvec);
                MergeIndex(pushReg (transtable, regResult))
            end

        | OGCCase {cases, test, default, caseType} =>
            let
                (* Cases are constructed by the optimiser out of if-then-else expressions. *)                
                val whereto = chooseMergeRegister(whereto, tailKind)
                
                (* Sort the cases into ascending order.  It's possible that we may have
                   duplicates if this came from an if-then-else construction so we
                   need to retain the ordering for items with the same case label. *)
                local
                    val labelCount = List.length cases
                    (* Add an extra field before sorting which retains the ordering for
                       equal labels. *)
                    val ordered = ListPair.zipEq (cases, List.tabulate(labelCount, fn n=>n))
                    fun leq ((_, w1: word), n1: int) ((_, w2), n2) =
                        if w1 = w2 then n1 <= n2 else w1 < w2
                    val sorted = List.map #1 (Misc.quickSort leq ordered)
                    (* Filter out any duplicates. *)
                    fun filter [] = []
                    |   filter [p] = [p]
                    |   filter ((p as (_, lab1)) :: (q as (_, lab2)) :: tl) =
                            if lab1 = lab2
                            then p :: filter tl
                            else p :: filter (q :: tl)
                in
                    val cases = filter sorted
                end

                val (isExhaustive, min, max) =
                    case caseType of
                        CaseTag max => (true, 0w0, max)
                    |   _ =>
                        let
                            val (_, aLabel) = hd cases
                            fun foldCases((_, w), (min, max)) = (Word.min(w, min), Word.max(w, max))
                            val (min, max) = List.foldl foldCases (aLabel, aLabel) cases
                        in
                            (false, min, max)
                        end
                val testValue = genToStack (test)
                val mark = markStack transtable

                (* Get exclusive use so that indexedCase can modify the registers. *)
                val (testReg, testIndex, testCode)  =
                    loadEntryToSet (transtable, testValue, RegSet.generalRegisters, true);
                (* Need a work register. *)
                val (workReg, regCode) = getRegisterInSet(transtable, generalRegisters)

                val (indexCaseInstr, caseLabels, defaultLabel) =
                    indexedCase{testReg=testReg, workReg=workReg, minCase=min, maxCase=max, 
                            isArbitrary = case caseType of CaseInt => true | _ => false,
                            isExhaustive=isExhaustive}

                val () = codeGenerate(indexCaseInstr @ regCode @ testCode, cvec)
                val () = codeGenerate(removeStackEntry (transtable, testIndex), cvec)
                val () = codeGenerate(freeRegister (transtable, workReg), cvec)

                val startOfCase = saveState transtable

                (* Put in the default case.  Even when the case is exhaustive one entry is
                   always treated as a default and not included in the list of cases. *)
                local
                    (* We have to set "branched" to true before calling fixup. *)
                    val () = exiting transtable
                    val startCode = fixup(makeLabels(NoMerge, defaultLabel, startOfCase), transtable)
                    val () = codeGenerate(startCode, cvec)

                    (* Go down the list of cases and fix up any default labels to come here.
                       Default entries are represented by "holes" in the case list. *)
                    fun genDefaults(indexVal, label :: labelList, cl as ((_, caseLabel) :: cps)) =
                        if indexVal = caseLabel
                        then genDefaults(indexVal+0w1, labelList, cps)
                        else
                        (
                            codeGenerate(forwardJumpLabel label, cvec);
                            genDefaults(indexVal+0w1, labelList, cl)
                        ) 
                    |   genDefaults(indexVal, label :: labelList, []) =
                        (
                            codeGenerate(forwardJumpLabel label, cvec);
                            genDefaults(indexVal+0w1, labelList, [])
                        )
                    |   genDefaults(_, [], _) = ()
                    
                    val () = genDefaults(min, caseLabels, cases)

                    val defaultRes =
                        genToRegister (default, whereto, tailKind, loopAddr);

                    (* Optimisation: return immediately, if possible, rather than
                       jumping and then returning. This may turn the following
                       unconditional branch into dead code, in which case it
                       will be removed by the lower-level code generator. *)
                    val () =
                       if isEndOfProc tailKind andalso not (haveExited transtable)
                       then codeGenerate(exit (), cvec)
                       else ();

                    val (lab, branchCode) = unconditionalBranch (defaultRes, transtable)
                    val () = codeGenerate(branchCode, cvec)

                    val () =
                        case defaultRes of
                            MergeIndex defaultIndex =>
                                codeGenerate(removeStackEntry (transtable, defaultIndex), cvec)
                        |   NoMerge => ()
                in
                    val exitDefault = lab
                end

                (* Generate the cases. *)
                fun genCases(indexVal, label :: labelList, (caseExp, caseLabel) :: cps) =
                    if indexVal <> caseLabel
                    then (* We have a hole.  Skip this item. *)
                        genCases(indexVal+0w1, labelList, (caseExp, caseLabel) :: cps)
                    else (* The index value corresponds to a label. *)
                    let
                        val startCode = fixup(makeLabels(NoMerge, label, startOfCase), transtable)
                        val () = codeGenerate(startCode, cvec)
                        val mark = markStack transtable

                        (* Generate this case and exit if tail-recursive. *)
                        val expResult =
                            genToRegister (caseExp, whereto, tailKind, loopAddr);

                        val () =
                            if isEndOfProc tailKind andalso not (haveExited transtable)
                            then codeGenerate(exit (), cvec)
                            else ();
                    in
                        if null cps
                        then (*  Finished. *) expResult (* Last expression. *)
                        else
                        let
                            val (lab, branchCode) = unconditionalBranch (expResult, transtable)
                            val () = codeGenerate(branchCode, cvec)

                            val () =
                                case expResult of
                                    MergeIndex expIndex =>
                                        codeGenerate(removeStackEntry(transtable, expIndex), cvec)
                                |   NoMerge => ();

                            val lastResult = genCases(indexVal+0w1, labelList, cps)
                            val (mergeRes, mergeCode) = (* Now fix up the exit label. *)
                                merge (lab, transtable, lastResult, mark)
                            val () = codeGenerate(mergeCode, cvec)
                        in
                            mergeRes
                        end
                    end
                | genCases _ = raise InternalError "genCase - null case list"

                val caseResult = genCases(min, caseLabels, cases)
                val (mergeRes, mergeCode) = merge (exitDefault, transtable, caseResult, mark)
                val () = codeGenerate(mergeCode, cvec)
            in
                mergeRes
            end      

        | OGCTuple reclist =>
            let
                val vecsize = List.length reclist
                val () =
                    if vecsize = 0 (* shouldn't occur *)
                    then raise InternalError "Zero sized vector"
                    else ()
                (* Since the vector is immutable, we have to evaluate
                   all the values before we can allocate it. *)
                val entries = List.map(fn h => genToStackOrGeneralRegister (h)) reclist

                fun loadSmallVector ([], _) = callgetvec (vecsize, Address.F_words, whereto, transtable)

                |   loadSmallVector (v::t, wordOffset) =
                    let
                        val (vec, vecCode) = loadSmallVector (t, wordOffset + 1)
                        val moveCode = moveToVec (vec, v, wordOffset, transtable)
                    in
                        (vec, moveCode @ vecCode)
                    end;
                val (vec, code) = loadSmallVector(entries, 0)
                val () = codeGenerate(code, cvec)
                val () = codeGenerate(allocationComplete, cvec)
            in
                MergeIndex vec
            end
        
        | OGCContainer size =>
            (* Reserve a number of words on the stack for use as a tuple on the
               stack.  The result is the address of this space. *)
            let
                val (reserveEntry, reserveCode) = reserveStackSpace(transtable, size)
            in
                codeGenerate(reserveCode, cvec);
                MergeIndex reserveEntry
            end

        |   OGCSetContainer{container, tuple, filter} =>
            (* Copy the contents of a tuple into a container. *)
            let
                val vec = genToStack container
            in
                case tuple of
                    OGCTuple cl =>
                        (* Simply set the container from the values filtering out those required. *)
                    let
                        fun setValues([], _, _) = ()

                        |   setValues(v::tl, sourceOffset, destOffset) =
                            let
                                val entry = genToStack v
                            in
                                (* Move the entry into the container.  Does not affect the
                                   use count for the container entry. *)
                                if sourceOffset < BoolVector.length filter andalso BoolVector.sub(filter, sourceOffset)
                                then
                                (
                                    codeGenerate(moveToVec (vec, entry, destOffset, transtable), cvec);
                                    setValues(tl, sourceOffset+1, destOffset+1)
                                )
                                else
                                (
                                    codeGenerate(removeStackEntry(transtable, entry), cvec);
                                    setValues(tl, sourceOffset+1, destOffset)
                                )
                            end
                    in
                        setValues(cl, 0, 0)
                    end

                |   _ =>
                    let
                        val tup = genToStack tuple
                        val last = BoolVector.foldli(fn (i, true, _) => i | (_, false, n) => n) ~1 filter

                        fun copy (sourceOffset, destOffset) =
                            if BoolVector.sub(filter, sourceOffset)
                            then
                            let
                                (* We need to ensure that the tuple entry is only removed
                                   when we load the last item from it. *)
                                val _ =
                                    if sourceOffset = last
                                    then ()
                                    else codeGenerate(incrUseCount(transtable, tup, 1), cvec)
                                val (entry, entryCode) = indirect (sourceOffset, tup, transtable)
                                val () = codeGenerate(entryCode, cvec)
                            in
                                codeGenerate(moveToVec (vec, entry, destOffset, transtable), cvec);
                                if sourceOffset = last
                                then ()
                                else copy (sourceOffset+1, destOffset+1)
                            end
                            else copy(sourceOffset+1, destOffset)
         
                    in
                        copy (0, 0)
                    end;

                codeGenerate(removeStackEntry(transtable, vec), cvec); (* Free the container entry. *)
                (* Return a void result if necessary. *)
                if isNoResult whereto then NoMerge
                else MergeIndex(pushConst (transtable, DummyValue))
            end

        |   instr as OGCTagTest _ => (* Better handled as a conditional *)
                codeAsConditional(instr, whereto, tailKind)

        |   OGCKillItems { expression, killSet, killBefore } =>
            let 
                (* This is inserted by the higher level code to get the use-counts
                   correct.  Kill entries are OGCExtract entries with lastRef true. *)
                fun cgKill toKill =
                    (gencde(toKill, true, NoResult, NotEnd, loopAddr); ())
            in
                if killBefore
                then (* Process the kill set before the expression. *)
                (
                    List.app cgKill killSet;
                    gencde (expression, primBoolOps, whereto, tailKind, loopAddr)
                )
                else (* Process the expression first, then kill the items *)
                let
                    val result =
                        gencde (expression, primBoolOps, whereto, tailKind, loopAddr)
                in
                    List.app cgKill killSet;
                    result
                end
            end
    in
        (* Various cases create results even if they're not required.  Remove them. *)
        case (result, whereto) of
            (NoMerge, NoResult) => NoMerge
        |   (NoMerge, _) => raise InternalError "gencde: Result wanted but none supplied"
        |   (MergeIndex m, NoResult) => (incrUseCount(transtable, m, ~1); NoMerge)
        |   (MergeIndex _, _) => result
    end (* gencde *) 

    (* Generate an expression putting the result in any register, and return
       the location of it on the stack. *)
    and genToStack (pt : oldGenCodeIC) : stackIndex =
        let
            val res = gencde (pt, true, NoHint, NotEnd, NONE)
        in
            case res of
                MergeIndex index => index
              | NoMerge => raise InternalError "genToStack: no result"
        end

    (* Reduce the expression to a constant, general register or simple address.  This
       differs from genToStack in that a value must not be in a floating point
       register.  This is important if we are about to put the value into a
       newly allocated object.  The floating point value will have to be
       moved into memory and that must be done before we allocate the new
       object. *)
    and genToStackOrGeneralRegister(pt : oldGenCodeIC) : stackIndex =
        let
            val res = gencde (pt, true, NoHint, NotEnd, NONE)
        in
            case res of
                MergeIndex index =>
                let
                    val (newIndex, code) = ensureNoAllocation(transtable, index)
                in
                    codeGenerate(code, cvec);
                    newIndex
                end
              | NoMerge => raise InternalError "genToStack: no result"
        end
(* ...
   (* Used when the result must be put in a register. *)
   and genToResult (pt, whereto, tailKind, loopAddr) : unit =
   let
     (* Stack results are forced into result register *)
     val toWhere = if isToPstack whereto then UseReg regResult else whereto;
     
     val result = gencde (pt, true, toWhere, tailKind, loopAddr);
   in
     (* If we need a result put it in the result reg.  We request exclusive use
    of it because otherwise there is a problem when merging the results
    of an if-then-else if the result register is somewhere else on the
    pstack (e.g. let a == ...; if ... then a else ...) *)
      case toWhere of
        UseReg rr => loadToSpecificReg (cvec, transtable, rr, result, true)
      | _        => ()
   end (* genToResult *)
... *)

   (* Used when the result must be put in a register. *)
    and genToRegister (pt, whereto, tailKind, loopAddr) : mergeResult =
    let
        val result = gencde (pt, true, whereto, tailKind, loopAddr)
    in
        case (whereto, result) of
            (NoResult, _) => NoMerge
        |   (UseReg rr, MergeIndex index) =>
                if haveExited transtable (* If we've raised an exception we can ignore this. *)
                then MergeIndex index
                else
                let
                    (* If we need a result put it in the result reg.  We request exclusive use
                       of it because otherwise there is a problem when merging the results
                       of an if-then-else if the result register is somewhere else on the
                       pstack (e.g. val a = ...; if ... then a else ...),
    
                       If we're at the end of a function, we're not merging, so we don't need
                       exclusive use. However, I don't think we actually save anything by trying
                       to make use of this fact so let's just be naive. SPF 27/11/96 *)
                    val (_, mergeItem, mergeCode) = loadEntryToSet (transtable, index, rr, true)
                in
                    codeGenerate(mergeCode, cvec);
                    MergeIndex mergeItem
                end
        |   (UseReg _, NoMerge) => raise InternalError "genToRegister: no result"
        |   (NoHint, _) => raise InternalError "genToRegister: not a register"
    end (* genToRegister *)

    (* `mutualRecursive' is used for mutually recursive functions
       where a function may not be able to fill in its closure if it does
       not function address has been pushed but before the code is generated. *)
    and genProc ({ closure=[], name=lambdaName, body=lambdaBody,
                   argTypes, resultType, argLifetimes, localCount, ... },
                 mutualRecursive: stackIndex -> unit, _) =
        let
            val profileObject = createProfileObject lambdaName
            (* Create a one word item for the closure.  This is returned for recursive references
               and filled in with the address of the code when we've finished. *)
            val closure = Address.allocWordData(0w1, F_mutable_words, Address.toMachineWord 0w0)
            val newCode = codeCreate (lambdaName, profileObject, debugSwitches)

            fun previous (OGCLoadRecursive, _, newtab) = (* load the address of the closure itself *)
                    (pushConst(newtab, Address.toMachineWord closure), [])
            |   previous (OGCLoadClosure _, _, _) =
                    raise InternalError "genProc: previous: NotConst"
           |    previous _ = raise InternalError "previous: local"
  
            val (ops, maxStack, callsAFunction, saveRegs) = 
                    codegen (lambdaBody, previous,
                        0, (* Discard regClosure *) argTypes, argLifetimes, resultType,
                        localCount, profileObject, debugSwitches)
        
            val codeAddr = copyCode (newCode, ops, maxStack, callsAFunction, saveRegs)
            val () = Address.assignWord(closure, 0w0, Address.toMachineWord codeAddr)
            val () = Address.lock closure
            val result = pushConst (transtable, Address.toMachineWord closure)
            (* Handle any other recursive functions. *)
            val () = mutualRecursive result
        in
            result
        end

    |   genProc ({ closure=closureList, heapClosure, name=lambdaName, body=lambdaBody,
                   argTypes, resultType, closureRefs, argLifetimes, localCount, ... },
                 mutualRecursive: stackIndex -> unit, whereto) =

        let
            (* First try loading all the items of the closure.  If
               there are mutually recursive references we may not be able to load them
               at this point.  *)
            fun loadClosure(c as OGCExtract(OGCLoadLocal addr, _)) =
                if Array.sub(decToPstack, addr) = noIndex
                then noIndex
                else genToStackOrGeneralRegister (c)
            |   loadClosure(c as OGCExtract(OGCLoadArgument _, _)) =
                    genToStackOrGeneralRegister (c)
            |   loadClosure c = genToStackOrGeneralRegister (c)
            val initialLocs = List.map loadClosure closureList
            (* This could be simplied.  There shouldn't be any constants now. *)
            val constants =
                List.map(fn i => if i = noIndex then NONE else isConstant(i, transtable)) initialLocs
            val nonConstCount = List.foldl(fn (NONE, n) => n+1 | (_, n) => n) 0 constants
                local
                    (* Convert the original index to a new index with the constants skipped. *)
                    fun makeIndex(NONE :: t, n) = SOME n :: makeIndex(t, n+1)
                    |   makeIndex(SOME _ :: t, n) = NONE :: makeIndex(t, n)
                    |   makeIndex([], _) = []
                in
                    val closureIndexes = Vector.fromList(makeIndex(constants, 1(*Starts from 1*)))
                end

            fun previous(OGCLoadRecursive, makeSl, _) =
                    makeSl () (* load the address of the closure itself *)
            |   previous(OGCLoadClosure locn, makeSl, newtab) =
                (
                    case List.nth(constants, locn) of
                        SOME lit => (pushConst (newtab, lit), [])
                    |   NONE =>
                        let
                            val newLocn = valOf(Vector.sub(closureIndexes, locn))
                            val (sl, closureCode) = makeSl() (* load the closure *)
                            val (entry, indCode) = indirect(newLocn, sl, newtab) (* load value from the closure *)
                        in
                            (entry, indCode @ closureCode)
                        end
                )
            |   previous(_, _, _) = raise InternalError "previous: local"

            val profileObject = createProfileObject lambdaName
            val newCode = codeCreate (lambdaName, profileObject, debugSwitches)
    
            val (ops, maxStack, callsAFunction, saveRegs) = (* code-gen function *)
                codegen (lambdaBody, previous,
                    closureRefs, argTypes, argLifetimes, resultType, localCount,
                    profileObject, debugSwitches)

            val codeAddr = copyCode (newCode, ops, maxStack, callsAFunction, saveRegs)
    
            val res = Address.toMachineWord codeAddr

            (* Build the closure.  If there are outstanding entries it has to be mutable and we
               can't complete it until we've done the other mutually recursive entries.  *)
            val incomplete = List.exists(fn i => i = noIndex) initialLocs
        in
            if heapClosure
            then
            let
                val (vector, vecCode) =
                    callgetvec (nonConstCount+1, if incomplete then F_mutable_words else F_words, whereto, transtable)

                val () = codeGenerate(vecCode, cvec)
                (* First word is the address of the code. *)
                val () = codeGenerate(moveToVec (vector, pushConst (transtable, res), 0, transtable), cvec)
                (* Put in everything else *)
                fun fillClosure(index::indices, NONE::constEntries, n) =
                    let
                        val indexOrDummy =
                            if index = noIndex
                            then (* Recursive entry.  This has to be initialised to avoid problems if we GC
                                    when allocating other closures. *)
                                pushConst (transtable, DummyValue)
                            else index
                        val vecAddr = valOf(Vector.sub(closureIndexes, n))
                    in
                        codeGenerate(moveToVec(vector, indexOrDummy, vecAddr, transtable), cvec);
                        fillClosure(indices, constEntries, n+1)
                    end
                |   fillClosure(index::indices, _::constEntries, n) =
                    (
                        (* It was a constant.  Remove it. *)
                        codeGenerate(incrUseCount(transtable, index, ~1), cvec);
                        fillClosure(indices, constEntries, n+1)
                    )
                |   fillClosure _ = ()
                val () = fillClosure(initialLocs, constants, 0)
                val () = codeGenerate(allocationComplete, cvec)

                (* Have to ensure that the closure remains on the psuedo-stack until
                   we've filled in all uses of it. The only references may be in the
                   closures of other functions so it's possible that its use-count
                   could be zero when `mutualRecursive' returns. Have to  increment
                   the use-count and then decrement it afterwards to make sure it
                    is still on the stack. *)
                val () = codeGenerate(incrUseCount (transtable, vector, 1), cvec)

                  (* Any mutually recursive references. *)
                val () = mutualRecursive vector

                (* We should now be able to fill in the recursive references. *)
                fun fillRecursive(index::indices, entry::entries, n) =
                    (
                        if index = noIndex (* Deferred entry*)
                        then
                        let
                            val loadEntry = genToStack entry
                            val addr = valOf(Vector.sub(closureIndexes, n))
                            val moveCode = moveToVec(vector, loadEntry, addr, transtable)
                        in
                            codeGenerate(moveCode, cvec)
                        end
                        else ();
                        fillRecursive(indices, entries, n+1)
                    )
                |   fillRecursive _ = ()

                val () = fillRecursive(initialLocs, closureList, 0)
            
                val () =
                    let
                        (* Finally we can lock this. *)
                        (* Increment the use count before the lock. *)
                        val () = codeGenerate(incrUseCount (transtable, vector, 1), cvec)
                        val (lockCode, _) = lockVector(vector, transtable, NoResult)
                    in
                        codeGenerate(lockCode, cvec)
                    end
                (* Restore the use count *)
                val () = codeGenerate(incrUseCount (transtable, vector, ~1), cvec)
            in
                vector
            end
        else
            let (* Stack closure *)
                (* Get the non-constant entries and release the constants. *)
                val nonConstEntries =
                    ListPair.foldr (fn (index, NONE, l) => index :: l |
                                       (index, _, l) => (codeGenerate(incrUseCount(transtable, index, ~1), cvec); l))
                                [] (initialLocs, constants)
                
                val (container, containerCode) =
                    createStackClosure(transtable, pushConst (transtable, res) :: nonConstEntries)

                val () = codeGenerate(containerCode, cvec)
                (* Have to ensure that the closure remains on the psuedo-stack until
                   we've filled in all uses of it. The only references may be in the
                   closures of other functions so it's possible that its use-count
                   could be zero when `mutualRecursive' returns. Have to  increment
                   the use-count and then decrement it afterwards to make sure it
                    is still on the stack. *)
                val () = codeGenerate(incrUseCount (transtable, container, 1), cvec)

                  (* Any mutually recursive references. *)
                val () = mutualRecursive container

                (* We should now be able to fill in the recursive references. *)
                fun fillRecursive(index::indices, entry::entries, n) =
                    (
                        if index = noIndex (* Deferred entry*)
                        then
                        let
                            val loadEntry = genToStack entry
                            val addr = valOf(Vector.sub(closureIndexes, n))
                            (* Move this into the stack. *)
                            val moveCode =
                                setRecursiveClosureEntry(container, loadEntry, addr, transtable)
                        in
                            codeGenerate(moveCode, cvec)
                        end
                        else ();
                        fillRecursive(indices, entries, n+1)
                    )
                |   fillRecursive _ = ()

                val () = fillRecursive(initialLocs, closureList, 0)

                (* Restore the use count *)
                val () = codeGenerate(incrUseCount (transtable, container, ~1), cvec)
            in
                container
            end
        end

    (* Generates test for if..then..else or while..do. Returns address of address field of jump.
       If jumpOn is true the jump is taken if the condition is true,
       if false it is taken if the condition is false. *)
    and genTest (pt, jumpOn) : labels =
    let (* See if we can generate a conditional instruction. *)
        (* Those we can't deal with specially are evaluated to the stack and tested. *)
        fun genOtherTests () =
        let
            (* We can't use genToStack here because we need primBoolOps to be false. *)
            val arg1Locn =
                case gencde (pt, false (* primBoolOps *), NoHint, NotEnd, NONE) of
                    MergeIndex index => index
                |   NoMerge => raise InternalError "genTest: no result"
            val arg2Locn = pushConst (transtable, False)
            val (label, testCode) =
                (if jumpOn then notEqualWord else equalWord) (arg1Locn, arg2Locn, transtable)
            val () = codeGenerate(testCode, cvec)
        in
            label
        end
    in
        case pt of
            OGCCond (testPart, thenPart, elsePart) =>
            let
                val mark1 = markStack transtable
                val mark2 = markStack transtable
          
                (* Test the condition part. *)
                val a : labels = genTest (testPart, false)
            in
                if isEmptyLabel a
                then (* The test evaluated to true.  We must only generate
                      the then-part.  This is more than an optimisation.
                      "Nojump" does not set the correct state for the
                      else-part which can cause problems. *)
                (
                    unmarkStack(transtable, mark2);
                    unmarkStack(transtable, mark1);
                    genTest (thenPart, jumpOn)
                )
                else if haveExited transtable
                then (* Unconditional jump.  Only need the else-part. *)
                (
                    unmarkStack(transtable, mark2);
                    unmarkStack(transtable, mark1);
                    codeGenerate(fixup (a, transtable), cvec);
                    genTest (elsePart, jumpOn)
                )
                else
                let
                    (* Now the `then-part' *)
                    val b : labels = genTest (thenPart, jumpOn);
              
                    (* Put in an unconditional jump round the `else-part'.
                       This will be taken if the `then-part' drops through. *)
                    val (notB, notCode) = unconditionalBranch (NoMerge, transtable)
                    val () = codeGenerate(notCode, cvec)
              
                    (* Fill in the label for the then-part part. *)
                    val () = codeGenerate(fixup (a, transtable), cvec);
              
                    (* Now do the `else-part' and jump on the inverse of the condition. *)
                    val notC = genTest (elsePart, not jumpOn);
              
                    (* i.e. we drop though if the condition is the one we should have
                       jumped on. Now merge in the first label so we have both cases
                       when we should jump together, *)
                    val (_, mergeBCode) = merge (b, transtable, NoMerge, mark2)
                    val () = codeGenerate(mergeBCode, cvec)
              
                    (* and now take the jump. *)
                    val (resultLab, resultCode) = unconditionalBranch (NoMerge, transtable)
                    val () = codeGenerate(resultCode, cvec)
              
                    (* Come here if we are not jumping. *)
                    val () = codeGenerate(fixup (notB, transtable), cvec);
                    val (_, mergeCCode) = merge (notC, transtable, NoMerge, mark1)
                    val () = codeGenerate(mergeCCode, cvec)
                in 
                    resultLab
                end
            end

            (* Constants - primarily for andalso/orelse. *)
        |   OGCConstnt(w, _) =>
            (* If true and we jump on true or false and jump on false *)
            (* then put in an unconditional jump. *)
            if wordEq (w, True) = jumpOn
            then
            let
                val (lab, code) = unconditionalBranch (NoMerge, transtable)
                val () = codeGenerate(code, cvec)
            in
                lab
            end
            else noJump (* else drop through. *)

        |   OGCNewenv(decs, exp) =>
            (
                List.app (codeBinding NONE) decs;
                genTest (exp, jumpOn)
            )

        |   OGCTagTest { test, tag, ... } =>
            let
                (* Convert this into a simple equality function. *)
                val code =
                    OGCBuiltIn2{oper=BuiltIns.WordComparison{test=BuiltIns.TestEqual, isSigned=false},
                        arg1=test, arg2=OGCConstnt(Address.toMachineWord tag, [])}
            in
                genTest(code, jumpOn)
            end

            
        |   OGCBuiltIn0{oper} =>
            (
                case oper of
                    BuiltIns.CurrentThreadId => raise InternalError "TODO-genTest: OGCBuiltIn0-CurrentThreadId"
            )

        |   OGCBuiltIn1{oper, arg1} =>
            (
                case oper of
                    BuiltIns.NotBoolean => genTest(arg1, not jumpOn)

                |   BuiltIns.IsTaggedValue =>
                    let
                        val argLocn = genToStack arg1
                        val (label, testCode) =
                            (if jumpOn then testShortInt else testNotShortInt) (argLocn, transtable)
                        val () = codeGenerate(testCode, cvec)
                    in
                        label
                    end

                |   BuiltIns.MemoryCellLength => raise InternalError "TODO-genTest: OGCBuiltIn1 - MemoryCellLength"
                |   BuiltIns.MemoryCellFlags => raise InternalError "TODO-genTest: OGCBuiltIn1 - MemoryCellFlags"
                |   BuiltIns.ClearMutableFlag => raise InternalError "TODO-genTest: OGCBuiltIn1 - ClearMutableFlag"
                |   BuiltIns.StringLengthWord => raise InternalError "TODO-genTest: OGCBuiltIn1 - StringLengthWord"
                |   BuiltIns.AtomicIncrement => raise InternalError "TODO-genTest: OGCBuiltIn1 - AtomicIncrement"
                |   BuiltIns.AtomicDecrement => raise InternalError "TODO-genTest: OGCBuiltIn1 - AtomicDecrement"
                |   BuiltIns.AtomicReset => raise InternalError "TODO-genTest: OGCBuiltIn1 - AtomicReset"
                |   BuiltIns.LongWordToTagged => raise InternalError "TODO-genTest: OGCBuiltIn1 - LongWordToTagged"
                |   BuiltIns.SignedToLongWord => raise InternalError "TODO-genTest: OGCBuiltIn1 - SignedToLongWord"
                |   BuiltIns.UnsignedToLongWord => raise InternalError "TODO-genTest: OGCBuiltIn1 - UnsignedToLongWord"
           )

        |   OGCBuiltIn2{oper, arg1, arg2} =>
            (
                case oper of
                    BuiltIns.WordComparison{test, isSigned} =>
                    let
                        open BuiltIns
                        val arg1Locn = genToStack arg1 and arg2Locn = genToStack arg2
                        val (label, testCode) =
                            case (test, jumpOn, isSigned) of
                                (* Equality and inequality are the same for signed and unsigned. *)
                                (TestEqual, true, _) => equalWord(arg1Locn, arg2Locn, transtable)
                            |   (TestEqual, false, _) => notEqualWord(arg1Locn, arg2Locn, transtable)
                            |   (TestNotEqual, true, _) => notEqualWord(arg1Locn, arg2Locn, transtable)
                            |   (TestNotEqual, false, _) => equalWord(arg1Locn, arg2Locn, transtable)
                                (* Unsigned (Word.word) comparisons. *)
                            |   (TestLess, true, false) => lessThanWord(arg1Locn, arg2Locn, transtable)
                            |   (TestLess, false, false) => greaterOrEqualWord(arg1Locn, arg2Locn, transtable)
                            |   (TestGreater, true, false) => greaterThanWord(arg1Locn, arg2Locn, transtable)
                            |   (TestGreater, false, false) => lessOrEqualWord(arg1Locn, arg2Locn, transtable)
                            |   (TestLessEqual, true, false) => lessOrEqualWord(arg1Locn, arg2Locn, transtable)
                            |   (TestLessEqual, false, false) => greaterThanWord(arg1Locn, arg2Locn, transtable)
                            |   (TestGreaterEqual, true, false) => greaterOrEqualWord(arg1Locn, arg2Locn, transtable)
                            |   (TestGreaterEqual, false, false) => lessThanWord(arg1Locn, arg2Locn, transtable)
                                (* Signed (FixedInt.int) comparisons. *)
                            |   (TestLess, true, true) => lessThanFixed(arg1Locn, arg2Locn, transtable)
                            |   (TestLess, false, true) => greaterOrEqualFixed(arg1Locn, arg2Locn, transtable)
                            |   (TestGreater, true, true) => greaterThanFixed(arg1Locn, arg2Locn, transtable)
                            |   (TestGreater, false, true) => lessOrEqualFixed(arg1Locn, arg2Locn, transtable)
                            |   (TestLessEqual, true, true) => lessOrEqualFixed(arg1Locn, arg2Locn, transtable)
                            |   (TestLessEqual, false, true) => greaterThanFixed(arg1Locn, arg2Locn, transtable)
                            |   (TestGreaterEqual, true, true) => greaterOrEqualFixed(arg1Locn, arg2Locn, transtable)
                            |   (TestGreaterEqual, false, true) => lessThanFixed(arg1Locn, arg2Locn, transtable)
                        val () = codeGenerate(testCode, cvec)
                    in
                        label
                    end

                |   BuiltIns.LargeWordComparison test =>
                    let
                        open BuiltIns
                        val arg1Locn = genToStack arg1 and arg2Locn = genToStack arg2
                        val (label, testCode) =
                            case (test, jumpOn) of
                                (TestEqual, true) => equalLargeWord(arg1Locn, arg2Locn, transtable)
                            |   (TestEqual, false) => notEqualLargeWord(arg1Locn, arg2Locn, transtable)
                            |   (TestNotEqual, true) => notEqualLargeWord(arg1Locn, arg2Locn, transtable)
                            |   (TestNotEqual, false) => equalLargeWord(arg1Locn, arg2Locn, transtable)
                            |   (TestLess, true) => lessThanLargeWord(arg1Locn, arg2Locn, transtable)
                            |   (TestLess, false) => greaterOrEqualLargeWord(arg1Locn, arg2Locn, transtable)
                            |   (TestGreater, true) => greaterThanLargeWord(arg1Locn, arg2Locn, transtable)
                            |   (TestGreater, false) => lessOrEqualLargeWord(arg1Locn, arg2Locn, transtable)
                            |   (TestLessEqual, true) => lessOrEqualLargeWord(arg1Locn, arg2Locn, transtable)
                            |   (TestLessEqual, false) => greaterThanLargeWord(arg1Locn, arg2Locn, transtable)
                            |   (TestGreaterEqual, true) => greaterOrEqualLargeWord(arg1Locn, arg2Locn, transtable)
                            |   (TestGreaterEqual, false) => lessThanLargeWord(arg1Locn, arg2Locn, transtable)
                        val () = codeGenerate(testCode, cvec)
                    in
                        label
                    end

                |   BuiltIns.FixedPrecisionArith _ => raise InternalError "TODO-genTest: OGCBuiltIn2-FixedPrecisionArith"

                |   BuiltIns.WordArith _ => raise InternalError "TODO-genTest: OGCBuiltIn2-WordArith"

                |   BuiltIns.SetStringLengthWord => raise InternalError "TODO-genTest: OGCBuiltIn2-SetStringLengthWord"

                |   BuiltIns.WordLogical _ => raise InternalError "TODO-genTest: OGCBuiltIn2-WordLogical"

                |   BuiltIns.WordShift _ => raise InternalError "TODO-genTest: OGCBuiltIn2-WordShift"

                |   BuiltIns.AllocateByteMemory => raise InternalError "TODO-genTest: OGCBuiltIn2-AllocateByteMemory"

                |   BuiltIns.LargeWordArith _ => raise InternalError "TODO-genTest: OGCBuiltIn2-LargeWordArith"

                |   BuiltIns.LargeWordLogical _ => raise InternalError "TODO-genTest: OGCBuiltIn2-LargeWordLogical"

                |   BuiltIns.LargeWordShift _ => raise InternalError "TODO-genTest: OGCBuiltIn2-LargeWordShift"
            )

        |   OGCBuiltIn3{oper, ...} =>
            (
                case oper of
                    BuiltIns.StoreWord => raise InternalError "TODO-genTest: OGCBuiltIn3-StoreWord"
                |   BuiltIns.StoreByte => raise InternalError "TODO-genTest: OGCBuiltIn3-StoreByte"
                |   BuiltIns.AllocateWordMemory => raise InternalError "TODO-genTest: OGCBuiltIn3-AllocateWordMemory"
            )

        |   OGCBuiltIn4{oper, ...} =>
            (
                case oper of
                    BuiltIns.Built4PlaceHolder => raise InternalError "TODO-genTest: OGCBuiltIn4"
            )

        |   OGCBuiltIn5{oper, arg1, arg2, arg3, arg4, arg5} =>
            (
                case oper of
                    BuiltIns.ByteVecEqual =>
                    let
                        val arg1Locn = genToStack arg1 and arg2Locn = genToStack arg2 and arg3Locn = genToStack arg3
                        and arg4Locn = genToStack arg4 and arg5Locn = genToStack arg5
                        val (label, testCode) =
                            (if jumpOn then testByteVecEq else testByteVecNe)
                                (arg1Locn, arg2Locn, arg3Locn, arg4Locn, arg5Locn, transtable)
                        val () = codeGenerate(testCode, cvec)
                    in
                        label
                    end

                |   BuiltIns.ByteVecCompare => raise InternalError "TODO-genTest: OGCBuiltIn5-ByteVecCompare"

                |   BuiltIns.BlockMove _ => raise InternalError "TODO-genTest: OGCBuiltIn5-BlockMove"
            )

        |   OGCLoadOperation { kind=LoadStoreMLWord _, ... } =>
                 (* TODO - This is "if !b then ...". We could use TEST instruction here. *)
                genOtherTests()

        |   OGCEval {function = OGCConstnt(oper, _), argList = args, ...} =>
            (* May be an interface operation which can be put in line. *)
            let
            in
                case args of
                    [] => genOtherTests () (* We don't currently have any nullary special cases *)

                |   [_] => genOtherTests () (* Non-special unary function.*)

                |   [(arg1, _), (arg2, _)] =>
                    (* binary special cases *)

                    if wordEq (oper,ioOp POLY_SYS_Real_eq)
                    then
                    let
                        val arg1Locn = genToStack arg1 and arg2Locn = genToStack arg2
                        val (label, testCode) =
                            (if jumpOn then equalReal else notEqualReal) (arg1Locn, arg2Locn, transtable)
                        val () = codeGenerate(testCode, cvec)
                    in
                        label
                    end
         
                    else if wordEq (oper,ioOp POLY_SYS_Real_neq)
                    then
                    let
                        val arg1Locn = genToStack arg1 and arg2Locn = genToStack arg2
                        val (label, testCode) =
                            (if jumpOn then notEqualReal else equalReal) (arg1Locn, arg2Locn, transtable)
                        val () = codeGenerate(testCode, cvec)
                    in
                        label
                    end

                    else if wordEq (oper,ioOp POLY_SYS_Real_geq)
                    then
                    let
                        val arg1Locn = genToStack arg1 and arg2Locn = genToStack arg2
                        val (label, testCode) =
                            (if jumpOn then greaterOrEqualReal else lessThanReal) (arg1Locn, arg2Locn, transtable)
                        val () = codeGenerate(testCode, cvec)
                    in
                        label
                    end

                    else if wordEq (oper,ioOp POLY_SYS_Real_leq)
                    then
                    let
                        val arg1Locn = genToStack arg1 and arg2Locn = genToStack arg2
                        val (label, testCode) =
                            (if jumpOn then lessOrEqualReal else greaterThanReal) (arg1Locn, arg2Locn, transtable)
                        val () = codeGenerate(testCode, cvec)
                    in
                        label
                    end

                    else if wordEq (oper,ioOp POLY_SYS_Real_gtr)
                    then
                    let
                        val arg1Locn = genToStack arg1 and arg2Locn = genToStack arg2
                        val (label, testCode) =
                            (if jumpOn then greaterThanReal else lessOrEqualReal) (arg1Locn, arg2Locn, transtable)
                        val () = codeGenerate(testCode, cvec)
                    in
                        label
                    end

                    else if wordEq (oper,ioOp POLY_SYS_Real_lss)
                    then
                    let
                        val arg1Locn = genToStack arg1 and arg2Locn = genToStack arg2
                        val (label, testCode) =
                            (if jumpOn then lessThanReal else greaterOrEqualReal) (arg1Locn, arg2Locn, transtable)
                        val () = codeGenerate(testCode, cvec)
                    in
                        label
                    end

                    else genOtherTests () (* Non-special binary function. *)

                | _ => (* Functions with more than 2 arguments. *)
                    genOtherTests ()
                end (* constant functions *)

        |   _ => genOtherTests ()(* Anything else *)
    end

    (* if/then/else, cand and cor. NB if/then/else may be translated
       into a CASE by the optimiser and code-generated there. *)
    and genCond (testExp, thenPt, elsePt, whereto, tailKind, loopAddr) =
        let
            val mark = markStack transtable
            (* We use the then-part to determine the register for the result so if
               it's simple we probably want to swap the else- and then-parts  *)
            val reverse =
                case thenPt of
                    OGCConstnt _ => true
                |   OGCExtract _ => true
                |   OGCRaise _ => true
                |   _ => false
            val (direction, thenExp, elseExp) =
                if reverse
                then (true, elsePt, thenPt)
                else (false, thenPt, elsePt)
            val lab  = genTest (testExp, direction) (* code for condition *)
            (* There used to be code in here to handle specially the case where the
             test expression was a constant.  I've taken that out, partly because
             the simple cases are dealt with by the optimiser but more seriously
             because it's necessary to deal with the slightly more general case
             where the test expression results in a constant (e.g. "if not false"
             or "if (print "something"; true)" ).  There was a bug in the case
             where the expression resulted in "true" since "lab" becomes "noJump"
             if the jump is never taken.  "fixup" leaves "exited" as true so no
             code is generated for the else-part but it doesn't set the pseudo-stack
             properly which can cause problems while processing the else-part.
             DCJM 27 June 2000. *)
        in
            if isEmptyLabel lab
            then
            ( (* Only the "then" part will be executed.  Don't generate the else-part. *)
                unmarkStack(transtable, mark);
                gencde (thenExp, true, whereto, tailKind, loopAddr)
            )
            else if haveExited transtable
            then
            ( (* Jump was unconditional - just generate the else-part. *)
                unmarkStack(transtable, mark);
                codeGenerate(fixup (lab, transtable), cvec);
                gencde (elseExp, true, whereto, tailKind, loopAddr)
            )
            else
            let
                (* Generate the then-part and see where the result is.  We need it in a
                   register but we don't want to decide in advance which register to use.
                   In particular, if the result is in a floating point register we don't
                   want to move it to a general register. *)
                val (thenResult, whereto) =
                    case (whereto, tailKind) of
                        (NoHint, NotEnd) =>
                        let
                            (* We don't have any preferences. *)
                            val initialThenResult =
                                gencde(thenExp, true, whereto, tailKind, loopAddr)
                        in
                            if haveExited transtable (* If we've raised an exception we can ignore this. *)
                            then (initialThenResult, NoHint)
                            else case initialThenResult of
                                MergeIndex res =>
                                let
                                    (* Is it in a register?  Merging requires exclusive use
                                       of the result register and it may be that this register
                                       is required elsewhere.  Use it as a hint for the register
                                       type we require and then load it.  If it's not required
                                       elsewhere this will just return the register it's in. *)
                                    val regSet =
                                        case isRegister(res, transtable) of
                                            SOME reg =>
                                                if inSet(reg, floatingPtRegisters)
                                                then floatingPtRegisters
                                                else generalRegisters
                                        |   NONE => generalRegisters
                                    val (_, mergeItem, mergeCode) = loadEntryToSet (transtable, res, regSet, true)
                                    val () = codeGenerate(mergeCode, cvec)
                                in
                                    (MergeIndex mergeItem,
                                        UseReg(singleton(valOf(isRegister(mergeItem, transtable)))))
                                end
                            |   NoMerge => raise InternalError "genCond: no result"
                        end
                    |   (_, EndOfProc res) =>
                        let
                            (* We want the result in the result reg. *)
                            val whereto = UseReg(singleton res)
                        in
                            (genToRegister (thenExp, whereto, tailKind, loopAddr), whereto)
                        end
                    |   (whereto, _) => (* No result or we have a specific register. *)
                            (genToRegister (thenExp, whereto, tailKind, loopAddr), whereto)

                val () = 
                    if isEndOfProc tailKind andalso not (haveExited transtable)
                    then codeGenerate(exit(), cvec)
                    else ()
              
                val (lab1, branchCode) = unconditionalBranch (thenResult, transtable)
                val () = codeGenerate(branchCode, cvec)
            
                (* Get rid of the result from the stack. If there is a result
                   then the "else-part" will push it. *)
                val () =
                    case thenResult of
                        MergeIndex thenIndex => codeGenerate(removeStackEntry(transtable, thenIndex), cvec)
                      | NoMerge => ()
              
                (* start of "else part" *)
                val () = codeGenerate(fixup (lab, transtable), cvec);
                val elseResult =
                    case whereto of
                        NoHint => (* Only if the then-part raised an exception *)
                            gencde(elseExp, true, whereto, tailKind, loopAddr)
                    |   _ => genToRegister (elseExp, whereto, tailKind, loopAddr)
                val (mergeRes, mergeCode) = merge (lab1, transtable, elseResult, mark)
                val () = codeGenerate(mergeCode, cvec)
            in 
                mergeRes
            end
        end (* genCond *)
        
        (* If we have an instruction that is better generated as a test we do it this way. *)
        and codeAsConditional(instr, whereto, tailKind) =
            genCond (instr, constntTrue, constntFalse, whereto, tailKind, NONE)

        (* Call a function. Detects special cases of calls to the run-time system
           to do simple operations such as int arithmetic and generates the
           instructions directly. For ordinary calls it has to distinguish between
           those called with a static-link and those called with a closure. *) 
        and genEval (evalFun, argList: (oldGenCodeIC * argumentType) list, resultType, primBoolOps, whereto, tailKind) : mergeResult =
        let

            (* Call a closure function. *)
            fun callClosure (clos : oldGenCodeIC option, canTail): mergeResult =
            let
                (* If we're actually calling the function where do the arguments go? *)
                val argLocations = argRegs (List.map (codeToCgType o #2)  argList)
                val modifiedArgRegs = List.map valOf (List.filter isSome argLocations)

                val needsResult = not (isNoResult whereto)
                val regResult = resultReg(codeToCgType resultType)

                (* Can use a jump if we're at the end, the closure is not the stack,
                   the result is in the right register (we don't need to convert floating point
                   to fixed point or vice versa) and none of the arguments are functions
                   with closures on the stack. *)
                local
                    fun nonContainer(OGCExtract(OGCLoadLocal addr, _), _) =
                            not(isContainer(Array.sub(decToPstack, addr), transtable))
                    |   nonContainer(OGCLambda{heapClosure, ...}, _) = heapClosure
                    |   nonContainer _ = true
                in
                    val isTail =
                        case tailKind of
                            EndOfProc reg => canTail andalso regResult = reg
                                             andalso List.all nonContainer argList
                        |   _ => false
                end

                (* Get the set of registers modified by this call.  We have to include
                   the argument, closure and code registers even if they're not actually
                   modified because otherwise we may find that we've locked them. *)
                val modifiedRegisters =
                    case clos of
                        SOME (OGCConstnt(w, _)) =>
                            if not (Address.isWords(Address.toAddress w))
                            then raise InternalError "callClosure: not closure"
                            else if Address.isMutable(Address.toAddress w)
                                (* It may be a forward reference i.e. a mutable closure that hasn't
                                   yet been set. *)
                            then allRegisters
                            else regSetUnion(listToSet(regClosure :: modifiedArgRegs), (*getRegisterSetForFunction w*)allRegisters)
                      | _ (* Recursive or not a constant. *) => allRegisters;

                (* Add the registers to the set modified by this function.
                   We don't need to do this for recursive calls.  In that
                   case we must push all the registers (so we set registerSet
                   to allRegisters) but the modification set for this function
                   is simply the registers modified by everything else. *)
                val _ =
                    case clos of
                        NONE => ()
                    |   _ => addModifiedRegSet(transtable, modifiedRegisters)

                (* In a tail-recursive call we may overwrite arguments on the stack.
                   We have to load any argument values we need before we overwrite them.*)
                fun checkTailArgument originalLocn =
                    if isTail
                    then
                    let
                        val (safeLocn, safeCode) = loadIfArg (transtable, originalLocn)
                        val () = codeGenerate(safeCode, cvec)
                    in
                        safeLocn
                    end
                    else originalLocn

                (* Have to guarantee that the expression to return
                   the function is evaluated before the arguments. *)
                val procLocn = 
                    case clos of
                        SOME(OGCConstnt _) => noIndex (* Unused. *)
                    |   SOME c          => checkTailArgument(genToStack c)
                    |   NONE            => noIndex  (* Unused. *)

                local
                    fun loadReg reg addr : stackIndex =
                        let
                          (* We don't need exclusive use of this value, because it
                             only gets modified by the function call itself, not
                             here. We either don't return from the function
                             (tail-call: we set exited) or we explicitly clear
                             the cache in setUpResult. *)
                          val (regIndex, regCode) =
                                loadToSpecificReg(transtable, reg, addr, false (* was bodyCall *));
                        in
                            codeGenerate(regCode, cvec);
                          (* Lock the register down so that it doesn't get
                             used to move values onto the stack. *)
                          lockRegister (transtable, reg);
                          regIndex
                        end
                in
                    fun loadProc (): (stackIndex option * bool * stackIndex list * reg list) =
                      case clos of
                         SOME(c as OGCConstnt(w, _)) =>
                            (* Do we need to load the closure register? *)
                            let
                                val addr = Address.toAddress w
                            in
                                if Address.isIoAddress addr orelse Address.flags addr <> Address.F_words
                                then (* If it's the address into the IO vector or it's a reference
                                        to code we haven't yet compiled we don't need the closure
                                        register but we can't do the indirection until run time. *)
                                    (SOME(loadReg regClosure (genToStack c)), true, [], [regClosure])
                                else
                                let
                                    val code : machineWord = Address.loadWord (addr, 0w0)
                                    val codeLocn = pushConst(transtable, code)
                                in
                                    if Address.length addr = 0w1
                                    then (* The closure is just one word - we don't need to
                                            put it in the closure register since the function
                                            won't need it.  Do the indirection now. *)
                                        (SOME codeLocn, false, [], [])
                                    else
                                        (* We need to load the closure register but we can jump
                                           directly to the call. *)
                                        (SOME codeLocn, false,
                                            [loadReg regClosure (pushConst(transtable, w))],
                                            [regClosure])
                                end
                            end
                       | SOME _ =>
                            (* Calling a non-constant - load the closure register and
                               set the code address as this with the "indirection"
                               flag set to true. *)
                            (SOME(loadReg regClosure procLocn), true, [], [regClosure])
                       | NONE => (* Recursive *)
                          (* If this function requires a closure we need to reload
                             the closure register with our original closure. *)
                          if closureLifetime = 0 then (NONE, false, [], [])
                          else (NONE, false, [loadReg regClosure closureOrSlAddr], [regClosure])
                end

                (* Code-generate each entry to the pstack.  If this is a tail recursive call we have to
                   load any values that are currently used as arguments because we may overwrite them later. *)
                local
                    fun loadArg((arg, _), argLocn) =
                    let
                        val originalLocn =
                            case argLocn of
                                SOME argReg =>
                                let (* Put into a register. *)
                                  (* If we are evaluating an expression we might as well put the
                                     result in the register we want to use. They may not stay
                                     there because loading other arguments may involve function
                                     calls which will use these registers. For that reason we
                                     don't put constants in yet. *)
                                  val whereto = case arg of OGCConstnt _ => NoHint | _ => UseReg(singleton argReg)
                                in
                                    case gencde (arg, true, whereto, NotEnd, NONE) of
                                        MergeIndex index => index
                                     |  NoMerge => raise InternalError "ldArgs: No result"
                                end
                            |   NONE => (* On the stack *) genToStack arg
                    in
                        checkTailArgument originalLocn
                    end
                in
                    val argsOnPstack = ListPair.mapEq loadArg(argList, argLocations)
                end
            in
                if isTail
                then  (* Enter a function by jumping rather than calling. *)
                let
                    (* Now move the arguments to their final destination. argAddr is a negative value and
                       is the address of the arguments in the original stack. *)
                    fun moveArgs ([], [], _) = []
                    |   moveArgs (arg::args, SOME argReg :: argTypes, argAddr) =
                        let
                            (* Do it in reverse order so that we can delay locking the register arguments. *)
                            val argEntries = moveArgs(args, argTypes, argAddr)
                            val (argEntry, argCode) = loadToSpecificReg (transtable, argReg, arg, false);
                        in
                            codeGenerate(argCode, cvec);
                            lockRegister (transtable, argReg);
                            argEntry :: argEntries
                        end
                    |   moveArgs (arg::args, NONE :: argTypes, argAddr) =
                        let
                            (* Store it in the stack, reloading anything it displaces. *)
                            val (argEntry, argCode) = storeInStack(transtable, arg, argAddr)
                            val () = codeGenerate(argCode, cvec)
                        in
                            argEntry :: moveArgs(args, argTypes, argAddr+1)
                        end
                    |   moveArgs _ = raise InternalError "moveArgs: Length mismatch"

                    (* the arguments are now all in their rightful places *)
                    val argEntries = moveArgs(argsOnPstack, argLocations, ~numberOfArgsOnStack)
    
                    (* Now load regClosure as appropriate. *)
                    val (codeAddrOpt, isIndirect, callEntries, registersLocked) = loadProc ()
                    (* Compute the number of stack arguments we're passing. *)
                    val stackArgCount = List.length(List.filter(not o isSome) argLocations)
        
                    (* Get the return address. *)
                    val returnReg : reg option =
                        (* The return address is on the stack.  Do we need to load it? *)
                        (* Only if we're passing a different number of arguments on
                           stack - this would change the offset of the return address. *)
                        if stackArgCount = numberOfArgsOnStack
                        then NONE (* Leave it there. *)
                        else
                        let
                            val (reg, regIndex, loadCode) =
                                loadEntryToSet (transtable, returnAddress, RegSet.generalRegisters, false)
                            val () = codeGenerate(loadCode, cvec)
                        in
                            codeGenerate(removeStackEntry(transtable, regIndex), cvec);
                            SOME reg
                        end
                    local
                        (* Move the stack pointer if necessary. *)           
                        val diffInArgs = numberOfArgsOnStack - stackArgCount
                        (* One more "arg" if the return address is passed on the stack. *)
                        val adjust = case returnReg of NONE => 1 | SOME _ => 0
                    in
                        val stackMove= realstackptr transtable + diffInArgs - adjust
                    end
                in
                    codeGenerate(resetStack stackMove, cvec);
                    (* Push the register with the return address. *)
                    case returnReg of NONE => () | SOME r => codeGenerate(pushRegisterToStack r, cvec);
                    (* Call the function.  If it's not recursive we have to get the
                       entry point. *)
                    (* We have to include a stack check in this function to ensure that
                       it's interruptible even though a tail jump doesn't require any
                       more stack. *)
                    callsAFunction := true; (* Don't really need this for RTS calls. *)
                    case codeAddrOpt of
                        NONE => codeGenerate(jumpToFunction Recursive, cvec)
                    |   SOME codeAddr =>
                            codeGenerate(jumpToCode(codeAddr, isIndirect, transtable), cvec);

                    (* Unlock any registers we locked. *)
                    List.app (fn r => codeGenerate(unlockRegister (transtable, r), cvec)) registersLocked;
                    (* Remove the arguments and code/closure registers. *)
                    List.app (fn index => codeGenerate(removeStackEntry(transtable, index), cvec))
                        (argEntries @ callEntries)
                    (* Since we've exited we don't need to clear the cache. *)
                end

                else (* Call a function.  Used in cases when it's not tail-recursive. *)
                let
                    (* Save any values to the stack other than those that are being
                       used in this call.  Values in registers not modified by the
                       call are locked in their current registers. *)
                    val (lockedRegs, pushInstrs) =
                        pushNonArguments(transtable, procLocn :: argsOnPstack, modifiedRegisters);
                    val () = codeGenerate(pushInstrs, cvec)

                    (* Push the arguments onto the real stack and/or load them
                       into the argument registers. *)
                    (* Second phase of argument evaluation.  Push the values onto the real stack
                         or load them into the argument registers.  The result is the stack base
                         for stack arguments together with a list of pseudo-stack entries for
                         the arguments. *)
                    fun pushArgs (argList : stackIndex list) : int * stackIndex list =
                    let
                        fun ldArgs ([], stackAddr, []) = (stackAddr, [])
                        |   ldArgs (argLoc :: t, stackAddr, SOME argReg :: t') =
                            let (* Put into a register. *)
                                (* Load the first before putting these into the registers. *)
                                val (rAddr : int, others) = ldArgs(t, stackAddr, t');
                                val (regEntry, regCode) = loadToSpecificReg (transtable, argReg, argLoc, false);
                            in
                                codeGenerate(regCode, cvec);
                                lockRegister (transtable, argReg);
                                (rAddr, regEntry :: others)
                            end
                        |   ldArgs (argLoc::t, stackAddr : int, NONE :: t') =
                            let (* Store on the real stack. *)
                                (* We take the current stack pointer as the base for the stack args. *)
                                val sAddr : int = 
                                if stackAddr < 0 then realstackptr transtable else stackAddr;
                                val (pushedEntry, pushCode) = pushValueToStack (transtable, argLoc, sAddr + 1)
                                val () = codeGenerate(pushCode, cvec)
                                val (rAddr, others) = ldArgs(t, sAddr + 1, t')
                            in
                                (rAddr, pushedEntry :: others)
                            end (* ldArgs *)
                        |   ldArgs _ = raise InternalError "ldArgs: Length mismatch"
                    in
                        ldArgs(argList, ~1, argLocations)
                    end (* pushArgs *)

                    val (endOfArgs, argEntries) = pushArgs argsOnPstack
              
                    (* load regClosure *)
                    val (codeAddrOpt, isIndirect, codeEntries, regsLocked) = loadProc ();
          
                    val checkContiguous =
                      (* Make sure that the arguments are contiguous on the
                         stack and that there is nothing beyond them on it. *)
                      if endOfArgs >= 0 then resetButReload (transtable, endOfArgs) else []
                    (* Record that we've called a function. *)
                    val () = callsAFunction := true; 
                    val callCode =
                      case codeAddrOpt of
                         NONE => callFunction Recursive
                      |  SOME codeAddr => callCode(codeAddr, isIndirect, transtable)
                in
                    codeGenerate(callCode @ checkContiguous, cvec);

                    (* Unlock any registers we locked. *)
                    List.app (fn r => codeGenerate(unlockRegister (transtable, r), cvec)) (lockedRegs @ regsLocked);
                    (* Remove the arguments and code/closure registers. *)
                    List.app (fn index => codeGenerate(removeStackEntry(transtable, index), cvec))
                        (codeEntries @ argEntries);

                    (* Remove any registers from the cache which may have been modified
                       by the function. *)
                    codeGenerate(removeRegistersFromCache(transtable, modifiedRegisters), cvec)
                end;

                (* Set up the results of the function call. *)
                (* Unlock  the argument registers. *)
                List.app(fn SOME r => codeGenerate(unlockRegister (transtable, r), cvec) | NONE => ()) argLocations;

                (* Remove any stack arguments.  Don't do this for tail calls*)
                if isTail
                then exiting transtable
                else List.app(fn SOME _ => () | NONE => decsp(transtable, 1)) argLocations;

                if not needsResult
                then NoMerge (* Unused *)
                else
                ( (* Result is returned in regResult. *)
                    codeGenerate(addRegUse (transtable, regResult), cvec); (* Needed? *)
                    MergeIndex(pushReg (transtable, regResult))
                )
            end (* callClosure *)

        in (* body of genEval *)
            case evalFun of
                OGCConstnt (oper, _) =>
                let
                    val args = List.map #1 argList
                    val addr = Address.toAddress oper
                in
                    if Address.isIoAddress addr
                    then
                    (
                        case args of
                            [arg] => (* Unary *)
                            if wordEq (oper,ioOp POLY_SYS_Abs_real)
                            then cgAndRes(absoluteReal(genToStack arg, transtable, whereto))

                            else if wordEq (oper,ioOp POLY_SYS_Neg_real)
                            then cgAndRes(negativeReal(genToStack arg, transtable, whereto))

                            else if wordEq (oper,ioOp POLY_SYS_fixed_to_real)
                            then cgAndRes(integerToReal(genToStack arg, transtable, whereto))

                            else callClosure (SOME evalFun, true)

                        |   [arg1, arg2] => (* Binary *)
                            if wordEq (oper,ioOp POLY_SYS_Add_real)
                            then cgAndRes(addReal(genToStack arg1, genToStack arg2, transtable, whereto))

                            else if wordEq (oper,ioOp POLY_SYS_Sub_real)
                            then cgAndRes(subtractReal(genToStack arg1, genToStack arg2, transtable, whereto))

                            else if wordEq (oper,ioOp POLY_SYS_Mul_real)
                            then cgAndRes(multiplyReal(genToStack arg1, genToStack arg2, transtable, whereto))

                            else if wordEq (oper,ioOp POLY_SYS_Div_real)
                            then cgAndRes(divideReal(genToStack arg1, genToStack arg2, transtable, whereto))

                            (* The point of the following code is to call genCond, which will call genTest
                               which will hopefully use machine instructions for these operations.
                               We could avoid this by duplicating most of the body of genTest
                               (the "jumping" boolean code generator) here, but we would like to
                               avoid that. *)
                            else if primBoolOps andalso
                            (wordEq (oper,ioOp POLY_SYS_Real_eq) orelse
                             wordEq (oper,ioOp POLY_SYS_Real_neq) orelse
                             wordEq (oper,ioOp POLY_SYS_Real_geq) orelse
                             wordEq (oper,ioOp POLY_SYS_Real_leq) orelse
                             wordEq (oper,ioOp POLY_SYS_Real_gtr) orelse
                             wordEq (oper,ioOp POLY_SYS_Real_lss))
                            then genCond
                                    (OGCEval {function = evalFun, argList = argList, resultType=resultType},
                                     constntTrue, constntFalse, whereto, tailKind, NONE)

                            else callClosure (SOME evalFun, true)

                        |  _ => callClosure (SOME evalFun, true)
                    )
  
                    else (* All other constant functions. *) callClosure (SOME evalFun, true)
                end

            |   OGCExtract (ext, lastRef) =>
                let (* Local function with non-empty closure. *)
                    val selfCall =
                        case ext of OGCLoadRecursive => true | _ => false
                    (* We cannot make a tail-recursive call to a function whose
                       closure is on the current stack because that would remove
                       the closure. *)
                    val canTail =
                        case ext of
                            OGCLoadLocal addr =>
                            let
                                val index = Array.sub(decToPstack, addr)
                            in
                                not(isContainer(index, transtable))
                            end
                        |   _ => true
                in 
                    (* Set the use count on the closure register if this is a
                       recursive call.  We have to do that for the recursive case
                       because we don't pass the OGCExtract entry in to callClosure.
                       DCJM 1/12/99. *)
                    if selfCall andalso not lastRef andalso closureLifetime <>0 
                    then codeGenerate(incrUseCount(transtable, closureOrSlAddr, 1), cvec)
                    else ();
                    callClosure (if selfCall then NONE else SOME evalFun, canTail)
                end (* OGCExtract *)

            |   evalLambda as OGCLambda{heapClosure, ...} =>
                    (* If we're going to put the closure on the stack we can't
                       call it with tail-recursion. *)
                    callClosure (SOME evalLambda, heapClosure)

            |   _ => (* The function is not being found by simply loading a value
                        from the stack or the closure and is not a constant. *)
                    callClosure (SOME evalFun, true)
        end (* genEval *)

        and codeBinding specific (OGCDeclar{addr, value, references}) = (* Declaration. *)
            let
                (* If the result of this block is this declaration choose a preferred register. *)
                val dest =
                    case specific of
                        SOME(destAddr, whereto) => if addr = destAddr then whereto else NoHint
                    |   NONE => NoHint
            in
                case value of
                    OGCLambda lam =>
                    let
                        fun nextMutual dec =
                            codeGenerate(localDeclaration (dec, addr, references), cvec)
                        val _ = genProc (lam, nextMutual, dest)
                    in
                        ()
                    end
                |   _ =>
                    let
                        val res = gencde (value, true, dest, NotEnd, NONE)
                        val decl =
                            case res of
                                MergeIndex index => index
                            |   NoMerge => raise InternalError "genToStack: no result"
                    in
                        codeGenerate(localDeclaration (decl, addr, references), cvec)
                    end
            end

        |   codeBinding _ (OGCRecDecs dl) =
            let
                (* Mutually recursive declarations. These can only be functions.
                   Recurse down the list
                   pushing the addresses of the closure vectors or forward
                   references to the code, then unwind the recursion and fill
                   in closures or compile the code. *)
                local
                    (* We now use the fact that decToPstack contains noindex to detect
                        mutual recursion in genProc.*)
                    fun setToEmpty({addr, ...}) = Array.update (decToPstack, addr, noIndex)
                in
                    val () = List.app setToEmpty dl
                end

                fun genMutualDecs []      = ()
                |   genMutualDecs (({lambda, addr, references, ...})::ds) =
                    let
                        (* This function is called once the closure has been
                           created but before the entries have been filled in. *) 
                        fun nextMutual r =
                        let
                            val () = codeGenerate(localDeclaration (r, addr, references), cvec)
                        in (* Now time to do the other closures. *)
                            genMutualDecs ds
                        end
                        val _ = genProc(lambda, nextMutual, NoHint)
                    in
                        ()
                    end
            in
                genMutualDecs dl
            end

        |   codeBinding _ (OGCNullBinding valu) = (* Expression in a sequence. *)
            (
                gencde (valu, true, NoResult, NotEnd, NONE);
                ()
            )

        val resReg = resultReg(codeToCgType resultType)
        val _ = genToRegister (pt, UseReg(singleton resReg), EndOfProc resReg, NONE)

        val () = if not (haveExited transtable) then codeGenerate(exit (), cvec) else ()
    in
    
        (* Having code generated the body of the function,
          it is copied into a new data segment. *)
        (!cvec, maxstack transtable, !callsAFunction, saveRegs)
    end (* codegen *)

    (* Result function.
       gencodeLambda compiles a top-level function.  This cannot have free variables.  Instead
       we pass in the address of a one-word mutable cell that can be used as the closure for
       recursive calls. *)
    fun codeLambda({ name, body, argTypes, resultType, argLifetimes, localCount, ...}:oldGenCodeLambdaForm, debugSwitches, closure) =
    let
        open Address
        val profileObject = createProfileObject name
        val newCode = codeCreate (name, profileObject, debugSwitches); 

        val (ops, maxStack, callsAFunction, saveRegs) =
            codegen
                (body,
                fn (_ , _, newtab) => (pushConst (newtab, toMachineWord closure), []),
                0, (* Discard regClosure *)
                argTypes, argLifetimes, resultType, localCount, profileObject, debugSwitches)

        val codeAddr = copyCode (newCode, ops, maxStack, callsAFunction, saveRegs)
    in
        assignWord(closure, 0w0, toMachineWord codeAddr);
        lock closure
    end
    
    fun gencodeLambda(lambda, debugSwitches, closure) =
    let
        val codeWithLifeTimes = lifeTimes(BICLambda lambda, 0)
    in
        case codeWithLifeTimes of
            OGCLambda clambda => codeLambda(clambda, debugSwitches, closure)
        |   _ => raise InternalError "gencodeLambda"
    end
    
    structure Foreign = X86FOREIGN
    
    structure Sharing =
    struct
        type backendIC = backendIC
        and  bicLoadForm = bicLoadForm
        and argumentType = argumentType
    end

end; (* GCODE functor body *)
