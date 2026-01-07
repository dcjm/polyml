(*
    Copyright (c) 2015, 2020 David C.J. Matthews

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
Lambda-lifting.  If every call point to a function can be identified we can
lift the free variables as extra parameters.  This avoids the need for a
closure on the heap.  It makes stack-closures largely redundant.  The
advantages of lambda-lifting over stack closures are that the containing
function of a stack-closure cannot call a stack-closure with tail-recursion
because the closure must remain on the stack until the function returns.
Also we can lambda-lift a function even if it is used in a function that
requires a full closure whereas we cannot use a stack closure for a
function if the closure would be used in a full, heap closure.

This pass is called after optimisation and after any functions that have
empty closures have been code-generated to constants.
*)
functor CodetreeLambdaLift (

    structure BaseCodeTree: BASECODETREE
    structure CodetreeFunctions: CODETREEFUNCTIONS
    structure Backend: CODEGENTREE
    structure Debug: DEBUG
    structure Pretty : PRETTY
    structure CodeArray: CODEARRAY

    sharing
        BaseCodeTree.Sharing
    =   CodetreeFunctions.Sharing
    =   Backend.Sharing
    =   Pretty.Sharing
    =   CodeArray.Sharing
): CODEGENTREE =
struct
    open BaseCodeTree
    open CodetreeFunctions
    exception InternalError = Misc.InternalError
    
    (* First pass: identify the functions whose only use are calls.  This annotates the tree
       by setting the "use" or any bindings or recursive uses that require a closure to
       [UseGeneral]. *)
    fun checkBody(code: codetree, closureRef: int -> unit, recursiveRef: unit -> unit, localCount) =
    let
        (* An entry for each local binding.  Set to true if we find a non-call reference. *)
        val localsNeedClosures = BoolArray.array(localCount, false)

        fun markExtract(LoadLocal n) = BoolArray.update(localsNeedClosures, n, true)
        |   markExtract LoadRecursive = recursiveRef()
        |   markExtract(LoadClosure n) = closureRef n
        |   markExtract(LoadArgument _) = ()

        fun checkCode(ext as Extract load) = (markExtract load; SOME ext)
            (* These are loads which aren't calls.  If they are functions they need closures. *)

        |   checkCode(Eval{function as Extract _, argList, resultType}) =
            (* A call of a function.  We don't need to mark the function as needing a closure. *)
                SOME(
                    Eval{function=function, argList=map(fn (c, t) => (checkMapCode c, t)) argList,
                         resultType=resultType})

        |   checkCode(Lambda lambda) = SOME(Lambda(checkLambda lambda))

        |   checkCode(Newenv(decs, exp)) =
            (* We want to add [UseGeneral] to bindings that require closures.  To do that
               we have to process the bindings in reverse order. *)
            let
                val processedExp = checkMapCode exp (* The expression first. *)
                
                fun getFlag addr =
                    if BoolArray.sub(localsNeedClosures, addr) then [UseGeneral] else []

                fun processDecs [] = []

                |   processDecs ((Declar { value, addr, ...}) :: tail) =
                    let
                        val pTail = processDecs tail (* Tail first *)
                        val pValue = checkMapCode value
                    in
                        Declar{value = pValue, addr=addr, use=getFlag addr} :: pTail
                    end

                |   processDecs (RecDecs l :: tail) =
                    let
                        val pTail = processDecs tail (* Tail first *)
                        (* Process the lambdas.  Because they're mutually recursive this may set the
                           closure flag for others in the set. *)
                        val pLambdas =
                            map (fn {lambda, addr, ...} =>
                                    {addr=addr, use=[], lambda=checkLambda lambda}) l
                        (* Can now pick up the closure flags. *)
                        val pDecs =
                            map(fn {lambda, addr, ...} =>
                                    {lambda=lambda, addr=addr, use=getFlag addr}) pLambdas
                    in
                        RecDecs pDecs :: pTail
                    end

                |   processDecs (NullBinding c :: tail) =
                    let
                        val pTail = processDecs tail
                    in
                        NullBinding(checkMapCode c) :: pTail
                    end

                |   processDecs (Container{ addr, size, setter,... } :: tail) =
                    let
                        val pTail = processDecs tail
                    in
                        Container{addr=addr, use=[], size=size, setter=checkMapCode setter} :: pTail
                    end
            in
                SOME(Newenv(processDecs decs, processedExp))
            end

        |   checkCode _ = NONE

        and checkLambda({body, closure, localCount, name, argTypes, resultType, ...}) =
        (* Lambdas - check the function body and any recursive uses. *)
        let
            val recNeedsClosure = ref false
            fun refToRecursive() = recNeedsClosure := true
            fun refToClosure n = markExtract(List.nth(closure, n))
            val processedBody = checkBody(body, refToClosure, refToRecursive, localCount)
        in
            {body=processedBody, isInline=DontInline, closure=closure, localCount=localCount, name=name,
             argTypes=argTypes, resultType=resultType, recUse=if !recNeedsClosure then [UseGeneral] else []}
        end

        and checkMapCode code = mapCodetree checkCode code
    in
        checkMapCode code
    end

    (* Second pass: Actually do the lambda-lifting. *)
    datatype lift =
        LiftLoad of loadForm (* Usually unlifted but also for recursive calls. *)
    |   LiftConst of codetree (* A lifted function. *)

    fun processBody(code: codetree, getClosure: int -> lift * loadForm list,
                    getRecursive: unit -> loadForm list, localCount, debugArgs): codetree =
    let
 
        val processedLambdas:
            (codetree * loadForm list) option array = Array.array(localCount, NONE)

        fun findBinding(ext as LoadLocal n) =
            (
                case Array.sub(processedLambdas, n) of 
                    SOME (c, l) => (LiftConst c, l)
                |   NONE => (LiftLoad ext, [])
            )
        |   findBinding(LoadRecursive) = (LiftLoad LoadRecursive, getRecursive())
            (* The code for the recursive case is always LoadRecursive but depending
               on whether it's been lifted or not there may be extra args. *)
        |   findBinding(LoadClosure n) = getClosure n
        |   findBinding(ext as LoadArgument _) = (LiftLoad ext, [])

        fun processCode(Eval{function=Extract ext, argList, resultType}) =
            let
                (* If this has been lifted we have to add the extra arguments.
                   The function may also now be a constant. *)
                val (newFunction, extraArgs) =
                    case findBinding ext of
                        (LiftConst c, l) => (c, l)
                    |   (LiftLoad e, l) => (Extract e, l)

                (* Process the original args.  There may be functions in there. *)
                val processedArgs = map(fn (c, t) => (processMapCode c, t)) argList
            in
                SOME(Eval{function=newFunction,
                     argList=processedArgs @ map(fn c => (Extract c, GeneralType)) extraArgs,
                     resultType=resultType})
            end

        |   processCode(Eval{function=Lambda(lambda as { recUse=[], ...}), argList, resultType}) =
            (* We have a call to a lambda.  This must be a recursive function otherwise it
               would have been expanded inline.  If the recursive references are just calls
               we can lambda-lift it. *)
            let
                val (fnConstnt, extraArgs) = hd(liftLambdas([(lambda, NONE)]))
                val processedArgs = map(fn (c, t) => (processMapCode c, t)) argList
            in
                SOME(Eval{function=fnConstnt,
                     argList=processedArgs @ map(fn c => (Extract c, GeneralType)) extraArgs,
                     resultType=resultType})
            end

        |   processCode(Extract ext) =
            (
                (* A load of a binding outside a call.  We need to process this to
                   rebuild the closure but if we get a lifted function it's an error. *)
                case findBinding ext of
                    (LiftLoad e, []) => SOME(Extract e)
                |   _ => raise InternalError "Lifted function out of context"
            )

        |   processCode(Lambda lambda) =
                (* Bare lambda or lambda in binding where we need a closure.
                   This can't be lambda-lifted but we still need to
                   process the body and rebuild the closure. *)
                SOME(Lambda(processLambdaWithClosure lambda))

        |   processCode(Newenv(decs, exp)) =
            let
                fun processDecs [] = []

                |   processDecs ((Declar { value = Lambda (lambda as { recUse=[], ...}), addr, use=[]}) :: tail) =
                    let
                        (* We can lambda-lift.  This results in a constant which is added to
                           the table.  We don't need an entry for the binding. *)
                        val constntAndArgs = hd(liftLambdas[(lambda, SOME addr(*or NONE*))])
                    in
                        Array.update(processedLambdas, addr, SOME constntAndArgs);
                        processDecs tail
                    end

                |   processDecs ((Declar { value, addr, ...}) :: tail) =
                        (* All other non-recursive bindings. *)
                        Declar{value = processMapCode value, addr=addr, use=[]} :: processDecs tail

                |   processDecs (RecDecs l :: tail) =
                    let
                        (* We only lambda-lift if all the functions are called.  We could
                           actually lift all those that are called and leave the others
                           but it's probably not worth it. *)
                        fun checkLift({lambda={recUse=[], ...}, use=[], ...}, true) = true
                        |   checkLift _ = false
                    in
                        if List.foldl checkLift true l
                        then
                        let
                            val results =
                                liftLambdas(map(fn{lambda, addr, ...} => (lambda, SOME addr)) l)
                        in
                            (* Add the code of the functions to the array. *)
                            ListPair.appEq(
                                fn (ca, {addr, ...}) => Array.update(processedLambdas, addr, SOME ca))
                                (results, l);
                            (* And just deal with the rest of the bindings. *)
                            processDecs tail
                        end
                        else
                        let
                            val pLambdas =
                                map (fn {lambda, addr, ...} =>
                                        {addr=addr, use=[], lambda=processLambdaWithClosure lambda}) l
                        in
                            RecDecs pLambdas :: processDecs tail
                        end
                    end

                |   processDecs (NullBinding c :: tail) =
                        NullBinding(processMapCode c) :: processDecs tail

                |   processDecs (Container{ addr, size, setter,... } :: tail) =
                        Container{addr=addr, use=[], size=size, setter=processMapCode setter} :: processDecs tail
            in
                SOME(Newenv(processDecs decs, processMapCode exp))
            end

        |   processCode _ = NONE

        and processLambdaWithClosure({body, closure, localCount, name, argTypes, resultType, ...}) =
        (* Lambdas that are not to be lifted.  They may still have functions inside that can
           be lifted.  They may also refer to functions that have been lifted. *)
        let
            (* We have to rebuild the closure.  If any of the closure entries were lifted
               functions they are now constants but their arguments have to be added to
               the closure. *)
            val newClosure = makeClosure()

            fun closureRef n =
            let
                val (localFunction, extraArgs) = findBinding(List.nth(closure, n))
                (* If the function is a local we have to add it to the closure.
                   If it is a lifted function the function itself will be a
                   constant except in the case of a recursive call.  We do
                   have to add the arguments to the closure. *)
                val resFunction =
                    case localFunction of
                        LiftLoad ext => LiftLoad(addToClosure newClosure ext)
                    |   c as LiftConst _ => c
                val resArgs = map(fn ext => addToClosure newClosure ext) extraArgs
            in
                (resFunction, resArgs)
            end

            val processedBody = processBody(body, closureRef, fn () => [], localCount, debugArgs)
        in
            {body=processedBody, isInline=DontInline, closure=extractClosure newClosure, localCount=localCount, name=name,
             argTypes=argTypes, resultType=resultType, recUse=[]}
        end

        and liftLambdas (bindings: (lambdaForm * int option) list) =
        (* Lambda-lift one or more functions.  The general, but least common, case is a
           set of mutually recursive functions.  More usually we have a single binding
           of a function or a single anonymous lambda.
           Lambda-lifting involves replacing the closure with arguments so it can only
           be used when we can identify all the call sites of the function and add
           the extra arguments. Because the transformed function has an empty closure
           (but see below for the mutually-recursive case) it can be code-generated
           immediately.  The code then becomes a constant.

           There are a few complications.  Although the additional, "closure"
           arguments are taken from the original function closure there may be
           changes if some of the closure entries are actually lambda-lifted
           functions.  In that case the function may become a constant, and
           so not need to be included in the arguments, but the additional
           arguments for that function may need to be added to the closure.
           The other complication is recursion, especially mutual recursion.
           If we have references to mutually recursive functions we actually
           leave those references in the closure.  This means that we actually
           code-generate mutually-recursive functions with non-empty closures
           but that is allowed if the references are only to other functions
           in the set.  The code-generator sorts that out. *)
        let
            (* We need to construct a new common closure.  This will be used by all
               the functions. *)
            val newClosure = makeClosure()

            fun closureEntry clItem =
            let
                val (localFunction, extraArgs) = findBinding clItem
                (* If the function is a local we have to add it to the closure.
                   If it is a lifted function the function itself will be a
                   constant except in the case of a recursive call.  We do
                   have to add the arguments to the closure. *)
                val resFunction =
                    case localFunction of
                        LiftLoad ext => LiftLoad(addToClosure newClosure ext)
                    |   c as LiftConst _ => c
                val resArgs = map(fn ext => addToClosure newClosure ext) extraArgs
            in
                (resFunction, resArgs)
            end

            local
                (* Check for an address which is one of the recursive set. *)
                val addressesUsed = List.mapPartial #2 bindings
            in
                fun isRecursive(LoadLocal n) = List.exists(fn p => p=n) addressesUsed
                |   isRecursive _ = false
            end

            local
                fun closureItem ext =
                    (* If it's a local we have to check that it's not one of our
                       mutually recursive set. These items aren't going to be
                       passed as arguments. *)
                    if isRecursive ext then () else (closureEntry ext; ())
            in
                val () = List.app(fn ({closure, ...}, _) => List.app closureItem closure) bindings
            end

            (* This composite closure is the set of additional arguments we need. *)
            val transClosure = extractClosure newClosure

            local
                val extraArgs = List.map(fn _ => (GeneralType, [])) transClosure
                val closureSize = List.length transClosure

                (* Process the function bodies. *)
                fun transformLambda({body, closure, localCount, name, argTypes, resultType, ...}, addr) =
                let
                    val argSize = List.length argTypes
                    val recArgs = List.tabulate(closureSize, fn n => LoadArgument(n+argSize))

                    (* References to other functions in the set are added to a
                       residual closure. *)
                    val residual = makeClosure()

                    fun closureRef clItem =
                    (* We have a reference to the (old) closure item.  We need to change that
                       to return the appropriate argument.  The exception is that if we
                       have a (recursive) reference to another function in the set we
                       instead use an entry from the residual closure. *)
                    let
                        val oldClosureItem = List.nth(closure, clItem)
                    in
                        if isRecursive oldClosureItem
                        then (LiftLoad(addToClosure residual oldClosureItem), recArgs)
                        else
                        let
                            val (localFunction, resArgs) = closureEntry oldClosureItem

                            fun mapToArg(LoadClosure n) = LoadArgument(n+argSize)
                            |   mapToArg _ = raise InternalError "mapToArg" (* Not a closure item. *)

                            val resFunction =
                                case localFunction of
                                    LiftLoad ext => LiftLoad(mapToArg ext)
                                |   c as LiftConst _ => c
                        in
                            (resFunction, map mapToArg resArgs)
                        end
                    end

                    (* Recursive case - add the extra args. *)
                    and recursiveRef() = recArgs

                    val processedBody = processBody(body, closureRef, recursiveRef, localCount, debugArgs)

                    val lambda = 
                        {body=processedBody, isInline=DontInline, closure=extractClosure residual,
                         localCount=localCount, name=name,
                         argTypes=argTypes @ extraArgs, resultType=resultType, recUse=[]}
                in
                    { lambda=lambda, addr=getOpt(addr, 0), use=[] }
                end
            
            in
                val bindingsForCode = List.map transformLambda bindings
            end

            local
                (* We may have a single anonymous lambda.  In that case we can give it
                   address zero. *)
                val addresses = map (fn (_, addr) => getOpt(addr, 0)) bindings
                (* Create "closures" for each entry.  These will be set by the
                   code-generator to the code of each function and will become the
                   closures we return.  Put them into the table. *)
                val maxAddr = List.foldl(fn (addr, n) => Int.max(addr, n)) 0 addresses
                (* To get the constant addresses we create bindings for the functions and
                   return a tuple with one entry for each binding. *)
                val extracts = List.map(Extract o LoadLocal) addresses
                val code = Newenv([RecDecs bindingsForCode], mkTuple extracts)
                (* Code-generate, "run" the code and extract the results. *)
                open Address
                val closure = CodeArray.makeConstantClosure()
                (* Turn this into a lambda to code-generate. *)
                val lambda:lambdaForm =
                {
                    body = code,
                    isInline = DontInline,
                    name = "<top level>",
                    closure = [],
                    argTypes = [(GeneralType, [])],
                    resultType = GeneralType,
                    localCount = maxAddr+1,
                    recUse = []
                }
                val props = Backend.codeGenerate(lambda, debugArgs, closure)
                val code: unit -> machineWord = RunCall.unsafeCast closure
                val codeConstnt = Constnt(code(), props)

                fun getItem([], _) = []
                |   getItem(_ :: l, n) = (mkInd(n, codeConstnt), transClosure) :: getItem(l, n+1)
            in
                (* Put in the results with the closures. *)
                val results = getItem(bindings, 0)
            end
        in
            results
        end
            
        and processMapCode code = mapCodetree processCode code
    in
        processMapCode code
    end
    
    type closureRef = CodeArray.closureRef

    fun codeGenerate(original: lambdaForm, debugArgs, closure) =
    let
        fun toplevel _ = raise InternalError "Top level reached"
        val checked = checkBody(Lambda original, toplevel, toplevel, 0)
        val processed =
            case processBody(checked, toplevel, toplevel, 0, debugArgs) of
                Lambda p => p
            |   _ => raise InternalError "CODETREE_LAMBDA_LIFT:codeGenerate"
    in
        Backend.codeGenerate(processed, debugArgs, closure)
    end

    structure Foreign = Backend.Foreign

    structure Sharing =
    struct
        open BaseCodeTree.Sharing
        type closureRef = closureRef
    end

end;
