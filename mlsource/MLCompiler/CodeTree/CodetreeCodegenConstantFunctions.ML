(*
    Copyright (c) 2013, 2015, 2017, 2020-1 David C.J. Matthews

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
If a function has an empty closure it can be code-generated immediately.  That may allow
other functions or tuples to be generated immediately as well.  As well as avoiding
run-time allocations this also allows the code-generator to use calls/jumps to constant
addresses.
*)
functor CodetreeCodegenConstantFunctions (
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
):
sig
    type codetree
    type machineWord = Address.machineWord
    val codeGenerate: codetree * int * Universal.universal list -> (unit -> machineWord) * Universal.universal list
    structure Foreign: FOREIGNCALL
    structure Sharing: sig type codetree = codetree end
end =
struct
    open BaseCodeTree
    open CodetreeFunctions
    open CodeArray
    open Address

    exception InternalError = Misc.InternalError

    datatype lookupVal = EnvGenLoad of loadForm | EnvGenConst of machineWord * Universal.universal list

    type cgContext =
    {
        lookupAddr: loadForm -> lookupVal,
        enterConstant: int * (machineWord * Universal.universal list) -> unit,
        debugArgs: Universal.universal list
    }

    (* Code-generate a function or set of mutually recursive functions that contain no free variables
       and run the code to return the address.  This allows us to further fold the address as
       a constant if, for example, it is used in a tuple. *)
    fun codeGenerateToConstant(lambda, debugSwitches, closure) =
    let
        val () =
            if Debug.getParameter Debug.codetreeAfterOptTag debugSwitches
            then Pretty.getCompilerOutput debugSwitches (BaseCodeTree.pretty(Lambda lambda)) else ()
    in
        Backend.codeGenerate(lambda, debugSwitches, closure)
    end

    (* If we are code-generating a function immediately we make a one-word
       mutable cell that will subsequently contain the address of the code.
       After it is locked this becomes the closure of the function.  By creating
       it here we can turn recursive references into constant references before
       we actually compile the function. *)

    fun cgFuns ({ lookupAddr, ...}: cgContext) (Extract ext) =
        (
            (* Look up the entry.  It may now be a constant.  If it isn't it may still
               have changed if it is a closure entry and other closure entries have
               been replaced by constants. *)
            case lookupAddr ext of
                EnvGenLoad load => SOME(Extract load)
            |   EnvGenConst w => SOME(Constnt w)
        )

    |   cgFuns (context as {debugArgs, ...}) (Lambda lambda) =
        let
            val copied as { closure=resultClosure, ...} = cgLambda(context, lambda, EnvGenLoad LoadRecursive)
        in
            case resultClosure of
                [] =>
                    let
                        (* Create a "closure" for the function. *)
                        val closure = makeConstantClosure()
                        (* Replace any recursive references by references to the closure.  There
                           may be inner functions that only make recursive calls to this.  By turning
                           the recursive references into constants we may be able to compile
                           them immediately as well. *)
                        val repLambda = cgLambda(context, lambda, EnvGenConst(toMachineWord closure, []))
                        val props = codeGenerateToConstant(repLambda, debugArgs, closure)
                    in
                        SOME(Constnt(toMachineWord closure, props))
                    end
            |   _ => SOME(Lambda copied)
        end

    |   cgFuns (context as { enterConstant, debugArgs, ...}) (Newenv(envBindings, envExp)) =
        let
            (* First expand out any mutually-recursive bindings.  This ensures that if
               we have any RecDecs left *)
            val expandedBindings =
                List.foldr (fn (d, l) => partitionMutualBindings d @ l) [] envBindings

            fun processBindings(Declar{value, addr, use} :: tail) =
                (
                    (* If this is a constant put it in the table otherwise create a binding. *)
                    case mapCodetree (cgFuns context) value of
                        Constnt w => (enterConstant(addr, w); processBindings tail)
                    |   code => Declar{value=code, addr=addr, use=use} :: processBindings tail
                )                    

            |   processBindings(NullBinding c :: tail) =
                    NullBinding(mapCodetree (cgFuns context) c) :: processBindings tail

            |   processBindings(RecDecs[{addr, lambda, use}] :: tail) =
                    (* Single recursive bindings - treat as simple binding *)
                    processBindings(Declar{addr=addr, value=Lambda lambda, use = use} :: tail)               

            |   processBindings(RecDecs recdecs :: tail) =
                let
                    (* We know that this forms a strongly connected component so it is only
                       possible to code-generate the group if no function has a free-variable
                       outside the group.  Each function must have at least one free
                       variable which is part of the group.  *)
                    fun processEntry {addr, lambda, use} =
                        {addr=addr, lambda=cgLambda(context, lambda, EnvGenLoad LoadRecursive), use=use}
                    val processedGroup = map processEntry recdecs

                    (* If every free variable is another member of the group we can
                       code-generate the group. *)
                    local
                        fun closureItemInGroup(LoadLocal n) =
                                List.exists(fn{addr, ...} => n = addr) processedGroup
                        |   closureItemInGroup _ = false

                        fun onlyInGroup{lambda={closure, ...}, ...} = List.all closureItemInGroup closure
                    in
                        val canCodeGen = List.all onlyInGroup processedGroup
                    end
                in
                    if canCodeGen
                    then
                    let
                        open Address
                        (* Create "closures" for each entry.  Add these as constants to the table. *)
                        fun createAndEnter {addr, ...} =
                            let val c = makeConstantClosure() in enterConstant(addr, (Address.toMachineWord c, [])); c end
                        val closures = List.map createAndEnter processedGroup
                        (* Code-generate each of the lambdas and store the code in the closure. *)
                        fun processLambda({lambda, addr, ...}, closure) =
                        let
                            val closureAsMachineWord = Address.toMachineWord closure
                            val repLambda =
                                cgLambda(context, lambda, EnvGenConst(closureAsMachineWord, []))
                            val props = codeGenerateToConstant(repLambda, debugArgs, closure)
                        in
                            (* Include any properties we may have added *)
                            enterConstant(addr, (closureAsMachineWord, props))
                        end
                        val () = ListPair.appEq processLambda (processedGroup, closures)
                    in
                        processBindings tail (* We've done these *)
                    end
                    
                    else RecDecs processedGroup :: processBindings tail
                end

            |   processBindings(Container{addr, use, size, setter} :: tail) =
                    Container{addr=addr, use=use, size=size,
                              setter = mapCodetree (cgFuns context) setter} :: processBindings tail
                
            |   processBindings [] = []

            val bindings = processBindings expandedBindings
            val body = mapCodetree (cgFuns context) envExp
        in
            case bindings of
                [] => SOME body
            |   _ => SOME(Newenv(bindings, body))
        end

    |   cgFuns context (Tuple{ fields, isVariant }) =
            (* Create any constant tuples that have arisen because they contain
               constant functions. *)
            SOME((if isVariant then mkDatatype else mkTuple)(map (mapCodetree (cgFuns context)) fields))

    |   cgFuns _ _ = NONE
    
    and cgLambda({lookupAddr, debugArgs, ...},
                 { body, isInline, name, closure, argTypes, resultType, localCount, recUse},
                 loadRecursive) =
    let
        val cArray = Array.array(localCount, NONE)
        val newClosure = makeClosure()

        fun lookupLocal(load as LoadLocal n) =
            (
                case Array.sub(cArray, n) of
                    NONE => EnvGenLoad load
                |   SOME w => EnvGenConst w
            )
        |   lookupLocal(LoadClosure n) =
            (
                case lookupAddr(List.nth (closure, n)) of
                    EnvGenLoad load => EnvGenLoad(addToClosure newClosure load)
                |   c as EnvGenConst _ => c
            )
        |   lookupLocal LoadRecursive = loadRecursive
        |   lookupLocal load = EnvGenLoad load (* Argument *)
        
        val context =
        {
            lookupAddr = lookupLocal,
            enterConstant = fn (n, w) => Array.update(cArray, n, SOME w),
            debugArgs = debugArgs
        }

        (* Process the body to deal with any sub-functions and also to bind
           in any constants from free variables. *)
        val newBody = mapCodetree (cgFuns context) body
        (* Build the resulting lambda. *)
        val resultClosure = extractClosure newClosure
    in
        { 
            body = newBody, isInline = isInline, name = name, closure = resultClosure,
            argTypes = argTypes, resultType = resultType, localCount = localCount,
            recUse = recUse
        }
    end

    fun codeGenerate(original, nLocals, debugArgs) =
    let
        val cArray = Array.array(nLocals, NONE)
        fun lookupAddr(load as LoadLocal n) =
            (
                case Array.sub(cArray, n) of
                    NONE => EnvGenLoad load
                |   SOME w => EnvGenConst w
            )
        |   lookupAddr _ = raise InternalError "lookupConstant: top-level reached"
            
        val context = 
        {
            lookupAddr = lookupAddr,
            enterConstant = fn (n, w) => Array.update(cArray, n, SOME w),
            debugArgs = debugArgs
        }
        
        val resultCode = mapCodetree (cgFuns context) original
        (* Turn this into a lambda to code-generate. *)
        val lambda:lambdaForm =
        {
            body = resultCode,
            isInline = DontInline,
            name = "<top level>",
            closure = [],
            argTypes = [(GeneralType, [])],
            resultType = GeneralType,
            localCount = nLocals,
            recUse = []
        }
        val closure = makeConstantClosure()

        val props = Backend.codeGenerate(lambda, debugArgs, closure)

        (* The code may consist of tuples (i.e. compiled ML structures) containing
           a mixture of Loads, where the values are yet to be compiled, and
           Constants, where the code has now been compiled.  We need to extract
           any properties from the constants and return the whole lot as
           tuple properties. *)
        fun extractProps(Constnt(_, p)) = p
        |   extractProps(Extract ext) =
            (
                case lookupAddr ext of
                    EnvGenLoad _ => []
                |   EnvGenConst(_, p) => p
            )
        |   extractProps(Tuple{fields, ...}) =
            let
                val fieldProps = map extractProps fields
            in
                if List.all null fieldProps
                then []
                else [Universal.tagInject CodeTags.tupleTag fieldProps]
            end
        |   extractProps(Newenv(_, exp)) = extractProps exp
        |   extractProps _ = []

        val newProps = extractProps original
        (* Cast this as a function. It is a function with a single argument. *)
        val resultFunction: unit -> machineWord = RunCall.unsafeCast closure

    in
        (resultFunction, CodeTags.mergeTupleProps(newProps, props))
    end
    
    structure Foreign = Backend.Foreign

    structure Sharing = struct type codetree = codetree end
end;
