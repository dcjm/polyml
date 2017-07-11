(*
    Copyright (c) 2012-13, 2015-16 David C.J. Matthews

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

functor CODETREE_STATIC_LINK_AND_CASES(
    structure BASECODETREE: BaseCodeTreeSig
    structure CODETREE_FUNCTIONS: CodetreeFunctionsSig
    structure GCODE: GENCODESIG
    structure DEBUG: DEBUGSIG
    structure PRETTY : PRETTYSIG
    structure BACKENDTREE: BackendIntermediateCodeSig

    sharing
        BASECODETREE.Sharing
    =   CODETREE_FUNCTIONS.Sharing
    =   GCODE.Sharing
    =   PRETTY.Sharing
    =   BACKENDTREE.Sharing
) : CodegenTreeSig
=
struct

    open BASECODETREE
    open Address
    open BACKENDTREE

    datatype caseType = datatype BACKENDTREE.caseType

    exception InternalError = Misc.InternalError

    open BACKENDTREE.CodeTags

    (* Property tag to indicate which arguments to a function are functions
       that are only ever called. *)
    val closureFreeArgsTag: int list Universal.tag = Universal.tag()

    fun staticLinkAndCases (pt, localAddressCount) =
    let
        fun copyCode (pt, nonLocals, recursive, localCount, localAddresses, argClosure) =
        let
            (* "closuresForLocals" is a flag indicating that if the declaration
               is a function a closure must be made for it. *)
            val closuresForLocals = Array.array(localCount, false)
            val newLocalAddresses = Array.array (localCount, 0)
            val argProperties = Array.array(localCount, [])

            (* Reference to local or non-local bindings.  This sets the "closure"
               property on the binding depending on how the binding will be used. *)
            fun locaddr (LoadLocal addr, closure) =
                let
                    val () =
                        if closure then Array.update (closuresForLocals, addr, true) else ()
                    val newAddr = Array.sub(newLocalAddresses, addr)
                in
                    BICLoadLocal newAddr
                end

            |   locaddr(LoadArgument addr, closure) =
                (
                    argClosure(addr, closure);
                    BICLoadArgument addr
                )

            |   locaddr(LoadRecursive, closure) = recursive closure
            |   locaddr(LoadClosure addr, closure) = #1 (nonLocals (addr, closure))

            (* Argument properties.  This returns information of which arguments can have
               functions passed in without requiring a full heap closure. *)
            fun argumentProps(LoadLocal addr) = Array.sub(argProperties, addr)
            |   argumentProps(LoadArgument _) = []
            |   argumentProps LoadRecursive = []
            |   argumentProps (LoadClosure addr) = #2 (nonLocals (addr, false))

            fun makeDecl addr =
            let
                val newAddr = ! localAddresses before (localAddresses := !localAddresses+1)
                val () = Array.update (closuresForLocals, addr, false)
                val () = Array.update (newLocalAddresses, addr, newAddr)
                val () = Array.update (argProperties, addr, [])
            in
                newAddr
            end

            fun insert(Eval { function = Extract LoadRecursive, argList, resultType, ...}) =
                let
                    (* Recursive.  If we pass an argument in the same position we
                       don't necessarily need a closure.  It depends on what else
                       happens to it. *)
                    fun mapArgs(n, (Extract (ext as LoadArgument m), t) :: tail) =
                            (BICExtract(locaddr(ext, n <> m)), t) :: mapArgs(n+1, tail)
                    |   mapArgs(n, (c, t) :: tail) = (insert c, t) :: mapArgs(n+1, tail)
                    |   mapArgs(_, []) = []

                    val newargs = mapArgs(0, argList)
                    val func  = locaddr(LoadRecursive, (* closure = *) false)
                in
                    (* If we are calling a function which has been declared this
                       does not require it to have a closure. Any other use of the
                       function would. *) 
                    BICEval {function = BICExtract func, argList = newargs, resultType=resultType}
                end

            |   insert(Eval { function = Extract ext, argList, resultType, ...}) =
                let
                    (* Non-recursive but a binding. *)
                    val cfArgs = argumentProps ext
                    fun isIn n = not(List.exists(fn m => m = n) cfArgs)
                        
                    fun mapArgs(n, (Extract ext, t) :: tail) =
                            (BICExtract(locaddr(ext, isIn n)), t) :: mapArgs(n+1, tail)
                    |   mapArgs(n, (Lambda lam, t) :: tail) =
                            (insertLambda(lam, isIn n), t) :: mapArgs(n+1, tail)
                    |   mapArgs(n, (c, t) :: tail) = (insert c, t) :: mapArgs(n+1, tail)
                    |   mapArgs(_, []) = []
                    val newargs = mapArgs(0, argList)
                    val func  = locaddr(ext, (* closure = *) false)
                in
                    (* If we are calling a function which has been declared this
                       does not require it to have a closure. Any other use of the
                       function would. *) 
                    BICEval {function = BICExtract func, argList = newargs, resultType=resultType}
                end

            |   insert(Eval { function = Constnt(w, p), argList, resultType, ...}) =
                let
                    (* Constant function. *)
                    val cfArgs =
                        case List.find (Universal.tagIs closureFreeArgsTag) p of
                            NONE => []
                        |   SOME u => Universal.tagProject closureFreeArgsTag u
                    fun isIn n = not(List.exists(fn m => m = n) cfArgs)
                        
                    fun mapArgs(n, (Extract ext, t) :: tail) =
                            (BICExtract(locaddr(ext, isIn n)), t) :: mapArgs(n+1, tail)
                    |   mapArgs(n, (Lambda lam, t) :: tail) =
                            (insertLambda(lam, isIn n), t) :: mapArgs(n+1, tail)
                    |   mapArgs(n, (c, t) :: tail) = (insert c, t) :: mapArgs(n+1, tail)
                    |   mapArgs(_, []) = []
                    val newargs = mapArgs(0, argList)
                in
                    BICEval {function = BICConstnt (w, p), argList = newargs, resultType=resultType}
                end

            |   insert(Eval { function = Lambda lam, argList, resultType, ...}) =
                let
                    (* Call of a lambda.  Typically this will be a recursive function that
                       can't be inlined. *)
                    val newargs = map(fn (c, t) => (insert c, t)) argList
                    val (copiedLambda, newClosure, makeRecClosure, _) = copyLambda lam
                    val func  = copyProcClosure (copiedLambda, newClosure, makeRecClosure)
                in
                    BICEval {function = func, argList = newargs, resultType=resultType}
                end

            |   insert(Eval { function, argList, resultType, ...}) =
                let
                    (* Process the arguments first. *)
                    val newargs = map(fn (c, t) => (insert c, t)) argList
                    val func  = insert function
                in
                    BICEval {function = func, argList = newargs, resultType=resultType}
                end

            |   insert GetThreadId = BICGetThreadId

            |   insert(Unary { oper, arg1 }) = BICUnary { oper = oper, arg1 = insert arg1 }

            |   insert(Binary { oper, arg1, arg2 }) = BICBinary { oper = oper, arg1 = insert arg1, arg2 = insert arg2 }

            |   insert(AllocateWordMemory {numWords, flags, initial}) =
                    BICAllocateWordMemory { numWords = insert numWords, flags = insert flags, initial = insert initial }

            |   insert(Extract ext) =
                    (* Load the value bound to an identifier. The closure flag is
                       set to true since the only cases where a closure is not needed,
                       eval and load-andStore, are handled separately. *)
                    BICExtract(locaddr(ext, (* closure = *) true))

            |   insert(Indirect {base, offset, ...}) = BICField {base = insert base, offset = offset}

            |   insert(Constnt wp) = BICConstnt wp (* Constants can be returned untouched. *)

            |   insert(BeginLoop{loop=body, arguments=argList, ...}) = (* Start of tail-recursive inline function. *)
                let
                    (* Make entries in the tables for the arguments. *)
                    val newAddrs = List.map (fn ({addr, ...}, _) => makeDecl addr) argList

                    (* Process the body. *)
                    val insBody = insert body
                    (* Finally the initial argument values. *)
                    local
                        fun copyDec(({value, ...}, t), addr) =
                                ({addr=addr, value=insert value}, t)
                    in
                        val newargs = ListPair.map copyDec (argList, newAddrs)
                    end
                in
                    (* Add the kill entries on after the loop. *)
                    BICBeginLoop{loop=insBody, arguments=newargs}
                end
    
            |   insert(Loop argList) = (* Jump back to start of tail-recursive function. *)
                        BICLoop(List.map(fn (c, t) => (insert c, t)) argList)

            |   insert(Raise x) = BICRaise (insert x)

                (* See if we can use a case-instruction. Arguably this belongs
                   in the optimiser but it is only really possible when we have
                   removed redundant declarations. *)
            |   insert(Cond(condTest, condThen, condElse)) =
                        reconvertCase(copyCond (condTest, condThen, condElse))

            |   insert(Newenv(ptElist, ptExp)) =
                let
                    (* Process the body. Recurses down the list of declarations
                       and expressions processing each, and then reconstructs the
                       list on the way back. *)
                    fun copyDeclarations ([])   = []

                    |   copyDeclarations (Declar({addr=caddr, value = Lambda lam, ...}) :: vs)  =
                        let
                            (* Binding a Lambda - process the function first. *)
                            val newAddr = makeDecl caddr
                            val (copiedLambda, newClosure, makeRecClosure, cfArgs) = copyLambda lam
                            val () = Array.update(argProperties, caddr, cfArgs)
                            (* Process all the references to the function. *)
                            val rest = copyDeclarations vs
                            (* We now know if we need a heap closure. *)
                            val dec =
                                copyProcClosure(copiedLambda, newClosure,
                                        makeRecClosure orelse Array.sub(closuresForLocals, caddr))
                        in
                            BICDeclar{addr=newAddr, value=dec} :: rest
                        end

                    |   copyDeclarations (Declar({addr=caddr, value = pt, ...}) :: vs)  =
                        let
                            (* Non-function binding. *)
                            val newAddr = makeDecl caddr
                            val rest = copyDeclarations vs
                        in
                            BICDeclar{addr=newAddr, value=insert pt} :: rest
                        end

                    |   copyDeclarations (RecDecs mutualDecs :: vs)  =
                        let
                          (* Mutually recursive declarations. Any of the declarations
                             may refer to any of the others. This causes several problems
                             in working out the use-counts and whether the functions 
                             (they should be functions) need closures. A function will
                             need a closure if any reference would require one (i.e. does
                             anything other than call it). The reference may be from one
                             of the other mutually recursive declarations and may be 
                             because that function requires a full closure. This means
                             that once we have dealt with any references in the rest of
                             the containing block we have to repeatedly scan the list of
                             declarations removing those which need closures until we
                             are left with those that do not. The use-counts can only be
                             obtained when all the non-local lists have been copied. *)
                  
                           (* First go down the list making a declaration for each entry.
                              This makes sure there is a table entry for all the
                              declarations. *)

                            val _ = List.map (fn {addr, ...} => makeDecl addr) mutualDecs

                            (* Process the rest of the block. Identifies all other
                               references to these declarations. *)
                            val restOfBlock = copyDeclarations vs

                            (* We now want to find out which of the declarations require
                               closures. First we copy all the declarations, except that
                               we don't copy the non-local lists of functions. *)
                            fun copyDec ({addr=caddr, lambda, ...}) = 
                                let
                                    val (dec, newClosure, makeRecClosure, cfArgs) = copyLambda lambda
                                    val () =
                                        if makeRecClosure then Array.update (closuresForLocals, caddr, true) else ()
                                    val () = Array.update(argProperties, caddr, cfArgs)

                                in
                                    (caddr, dec, newClosure)
                                end             

                            val copiedDecs = map copyDec mutualDecs
                   
                            (* We now have identified all possible references to the
                               functions apart from those of the closures themselves.
                               Any of closures may refer to any other function so we must 
                               iterate until all the functions which need full closures
                               have been processed. *)
                            fun processClosures([], outlist, true) =
                                (* Sweep completed. - Must repeat. *)
                                processClosures(outlist, [], false)
            
                            |   processClosures([], outlist, false) =
                                (* We have processed the whole of the list without finding
                                   anything which needs a closure. The remainder do not
                                   need full closures. *)
                                let
                                    fun mkLightClosure ((addr, value, newClosure)) = 
                                        let
                                            val clos = copyProcClosure(value, newClosure, false)
                                            val newAddr = Array.sub(newLocalAddresses, addr)
                                        in
                                            {addr=newAddr, value=clos}
                                        end          
                                in
                                    map mkLightClosure outlist
                                end
                  
                            |   processClosures((h as (caddr, value, newClosure))::t, outlist, someFound) =
                                if Array.sub(closuresForLocals, caddr)
                                then
                                let (* Must copy it. *)
                                    val clos = copyProcClosure(value, newClosure, true)
                                    val newAddr = Array.sub(newLocalAddresses, caddr)
                                in
                                    {addr=newAddr, value=clos} :: processClosures(t, outlist, true)
                                end
                                    (* Leave it for the moment. *)
                                else processClosures(t, h :: outlist, someFound)
                  
                            val decs = processClosures(copiedDecs, [], false)

                            local
                                fun isLambda{value=BICLambda _, ...} = true
                                |   isLambda _ = false
                            in
                                val (lambdas, nonLambdas) = List.partition isLambda decs
                            end
                            fun asMutual{addr, value = BICLambda lambda} = {addr=addr, lambda=lambda}
                            |   asMutual _ = raise InternalError "asMutual"
                        in
                            (* Return the mutual declarations and the rest of the block. *)
                            if null lambdas
                            then map BICDeclar nonLambdas @ restOfBlock         (* None left *)
                            else BICRecDecs (map asMutual lambdas) :: (map BICDeclar nonLambdas @ restOfBlock)
                        end (* copyDeclarations.isMutualDecs *)
          
                    |   copyDeclarations (NullBinding v :: vs)  =
                        let (* Not a declaration - process this and the rest. *)
                           (* Must process later expressions before earlier
                               ones so that the last references to variables
                               are found correctly. DCJM 30/11/99. *)
                            val copiedRest = copyDeclarations vs;
                            val copiedNode = insert v
                        in
                            (* Expand out blocks *)
                            case copiedNode of
                                BICNewenv(decs, exp) => decs @ (BICNullBinding exp :: copiedRest)
                            |   _ => BICNullBinding copiedNode :: copiedRest
                        end

                    |   copyDeclarations (Container{addr, size, setter, ...} :: vs) =
                        let
                            val newAddr = makeDecl addr
                            val rest = copyDeclarations vs
                            val setCode = insert setter
                        in
                            BICDecContainer{addr=newAddr, size=size} :: BICNullBinding setCode :: rest
                        end

                    val insElist = copyDeclarations(ptElist @ [NullBinding ptExp])

                    fun mkEnv([], exp) = exp
                    |   mkEnv(decs, exp) = BICNewenv(decs, exp)

                    fun decSequenceWithFinalExp decs =
                    let
                        fun splitLast _ [] = raise InternalError "decSequenceWithFinalExp: empty"
                        |   splitLast decs [BICNullBinding exp] = (List.rev decs, exp)
                        |   splitLast _ [_] = raise InternalError "decSequenceWithFinalExp: last is not a NullDec"
                        |   splitLast decs (hd::tl) = splitLast (hd:: decs) tl
                    in
                        mkEnv(splitLast [] decs)
                    end
                in
                    (* TODO: Tidy this up. *)
                    decSequenceWithFinalExp insElist
                end (* isNewEnv *)
                
            |   insert(Tuple { fields, ...}) = BICTuple (map insert fields)
      
            |   insert(Lambda lam) =
                    (* Using a lambda in a context other than a call or being passed
                       to a function that is known only to call the function.  It
                       requires a heap closure. *)
                    insertLambda(lam, true)

            |   insert(Handle { exp, handler, exPacketAddr }) =
                let
                    (* The order here is important.  We want to make sure that
                       the last reference to a variable really is the last. *)
                    val newAddr = makeDecl exPacketAddr
                    val hand = insert handler
                    val exp = insert exp
                in
                    BICHandle {exp = exp, handler = hand, exPacketAddr=newAddr}
                end

            |   insert(SetContainer {container, tuple, filter}) =
                    BICSetContainer{container = insert container, tuple = insert tuple, filter = filter}

            |   insert(TagTest{test, tag, maxTag}) = BICTagTest{test=insert test, tag=tag, maxTag=maxTag}

            |   insert(LoadOperation{kind, address}) = BICLoadOperation{kind=kind, address=insertAddress address}

            |   insert(StoreOperation{kind, address, value}) =
                    BICStoreOperation{kind=kind, address=insertAddress address, value=insert value}

            |   insert(BlockOperation{kind, sourceLeft, destRight, length}) =
                    BICBlockOperation{
                        kind=kind, sourceLeft=insertAddress sourceLeft,
                        destRight=insertAddress destRight, length=insert length}

            and insertLambda (lam, needsClosure) =
            let
                val (copiedLambda, newClosure, _, _) = copyLambda lam
            in
                copyProcClosure (copiedLambda, newClosure, needsClosure)
            end

            and insertAddress{base, index, offset} =
                {base=insert base, index=Option.map insert index, offset=offset}

          and copyCond (condTest, condThen, condElse) =
            let
              (* Process the then-part. *)
              val insThen = insert condThen
              (* Process the else-part.  If it's a conditional process it here. *)
              val insElse =
                case condElse of
                    Cond(i, t, e) => copyCond(i, t, e)
                |   _ => insert condElse
              (* Process the condition after the then- and else-parts. *)
              val insFirst = insert condTest
          
              type caseVal =
                { tag: word, test: codetree, caseType: caseType } option;
        
              (* True if both instructions are loads or indirections with the
                 same effect. More complicated cases could be considered but
                 function calls must always be treated as different.
                 Note: the reason we consider Indirect entries here
                 as well as Extract is because we (used to) defer Indirect entries.  *)
              datatype similarity = Different | Similar of bicLoadForm

              fun similar (BICExtract a, BICExtract b) = if a = b then Similar a else Different
              
               |  similar (BICField{offset=aOff, base=aBase}, BICField{offset=bOff, base=bBase}) =
                    if aOff <> bOff then Different else similar (aBase, bBase)
              
               |  similar _ = Different;

                (* If we have a call to the int equality operation *)
                (* then we may be able to use a case statement. *)
                fun (*findCase (BICBuiltIn(function, argList)) =
                let
                    val isArbitrary = function = RuntimeCalls.POLY_SYS_equal_short_arb
                    val isWord = function = RuntimeCalls.POLY_SYS_word_eq
                in
                    if isArbitrary orelse isWord
                    then  (* Should be just two arguments. *)
                    case argList of
                        [BICConstnt(c1, _), arg2] =>
                        if isShort c1
                        then SOME{tag=toShort c1, test=arg2, caseType = if isArbitrary then CaseInt else CaseWord}
                        else NONE (* Not a short constant. *)
                    
                     | [arg1, BICConstnt(c2, _)] =>
                        if isShort c2
                        then SOME{tag=toShort c2, test=arg1, caseType = if isArbitrary then CaseInt else CaseWord}
                        else NONE (* Not a short constant. *)
                    
                    | _ => NONE
                       (* Wrong number of arguments - should raise exception? *)
                
                    else NONE (* Function is not a comparison. *)
                end

             |  *)findCase(BICTagTest { test, tag, maxTag }) =
                    SOME { tag=tag, test=test, caseType=CaseTag maxTag }

             |  findCase _ = NONE
        
              val testCase = findCase insFirst
            in

              case testCase of
                    NONE => (* Can't use a case *)
                        BICCond (insFirst, insThen, reconvertCase insElse)
                |   SOME { tag=caseTags, test=caseTest, caseType=caseCaseTest } =>
                        (* Can use a case. Can we combine two cases?
                          If we have an expression like 
                               "if x = a then .. else if x = b then ..."
                          we can combine them into a single "case". *)
                        case insElse of
                            BICCase { cases=nextCases, test=nextTest, default=nextDefault, caseType=nextCaseType } =>
                            (
                                case (similar(nextTest, caseTest), caseCaseTest = nextCaseType) of
                                  (* Note - it is legal (though completely redundant) for the
                                     same case to appear more than once in the list. This is not
                                      checked for at this stage. *)
                                    (Similar _, true) =>
                                        BICCase 
                                        {
                                            cases   = (insThen, caseTags) ::
                                                        map (fn (c, l) => (c, l)) nextCases,
                                            test    = nextTest,
                                            default = nextDefault,
                                            caseType = caseCaseTest
                                        }

                                    | _ => (* Two case expressions but they test different
                                              variables. We can't combine them. *)
                                        BICCase
                                        {
                                            cases   = [(insThen, caseTags)],
                                            test    = caseTest,
                                            default = reconvertCase insElse,
                                            caseType=caseCaseTest
                                        }
                            )
                            | _ => (* insElse is not a case *)
                                BICCase
                                {
                                    cases   = [(insThen, caseTags)],
                                    test    = caseTest,
                                    default = insElse,
                                    caseType=caseCaseTest
                                }
            end

            (* Check something that's been created as a Case and see whether it is sparse.
               If it is turn it back into a sequence of conditionals.  This was previously
               done at the bottom level and the choice of when to use an indexed case was
               made by the architecture-specific code-generator.  That's probably unnecessary
               and complicates the code-generator. *)
            and reconvertCase(BICCase{cases, test, default, caseType}) =
                let
                    (* Count the number of cases and compute the maximum and minimum. *)
                    (* If we are testing on integers we could have negative values here.
                       Because we're using "word" here any negative values are treated as
                       large positive values and so we won't use a "case".
                       If this is a case on constructor tags we know the range.  There
                       will always be a "default" which may be anywhere in the range but
                       if we construct a jump table that covers all the values we don't need
                       the range checks. *)
                    val useIndexedCase =
                        case caseType of
                            CaseTag _ => (* Exhaustive *) List.length cases > 4
                        |   _ =>
                            let
                                val (_, aLabel) = hd cases
                                fun foldCases((_, w), (min, max)) = (Word.min(w, min), Word.max(w, max))
                                val (min, max) = List.foldl foldCases (aLabel, aLabel) cases
                                val numberOfCases = List.length cases
                            in
                                numberOfCases > 7 andalso Word.fromInt numberOfCases >= (max - min) div 0w3
                            end
                in
                    if useIndexedCase
                    then BICCase{cases=cases, test=test, default=default, caseType=caseType}
                    else
                    let
                        fun reconvert [] = default
                        |   reconvert ((c, t) :: rest) =
                            let
                                val test =
                                    case caseType of
                                        CaseInt => raise InternalError "reconvertCase"
                                            (*mkEval(BICConstnt(ioOp RuntimeCalls.POLY_SYS_equal_short_arb, []),
                                                   [test, BICConstnt(toMachineWord t, [])])*)
                                    |   CaseWord =>
                                            BICBinary{
                                                oper=BuiltIns.WordComparison{test=BuiltIns.TestEqual, isSigned=false},
                                                arg1=test, arg2=BICConstnt(toMachineWord t, [])}
                                    |   CaseTag maxTag => BICTagTest { test=test, tag=t, maxTag=maxTag }
                            in
                                BICCond(test, c, reconvert rest)
                            end
                    in
                        reconvert cases
                    end
                end
            |   reconvertCase t = t (* Just a simple conditional. *)
            

            (* If "makeClosure" is true the function will need a full closure.
               It may need a full closure even if makeClosure is false if it
               involves a recursive reference which will need a closure. *)
            and copyLambda ({body=lambdaBody, argTypes,
                             name=lambdaName, resultType, localCount, closure=lambdaClosure, ...}: lambdaForm) =
            let
                val newGrefs: loadForm list ref      = ref [] (* non-local references *)
                val newNorefs     = ref 0  (* number of non-local refs *)
                val makeClosureForRecursion = ref false
       
                (* A new table for the new function. *)
                fun prev (closureAddr, closure) =
                let 
                    val loadEntry = List.nth(lambdaClosure, closureAddr)

                    (* Returns the closure address of the non-local *)
                    fun makeClosureEntry([], _) = (* not found - construct new entry *)
                        let
                            val () = newGrefs := loadEntry ::  !newGrefs;
                            val newAddr = !newNorefs + 1;
                        in
                            newNorefs := newAddr; (* increment count *)
                            newAddr-1
                        end
        
                    |   makeClosureEntry(oldEntry :: t, newAddr) =
                        if oldEntry = loadEntry
                        then newAddr-1
                        else makeClosureEntry(t, newAddr - 1)

                    (* Set the closure flag if necessary and get the argument props.
                       At this point we discard the "Load" entry returned by nonLocals
                       and "recursive".  The closure will be processed later. *)
                    val argProps =
                        case loadEntry of
                            LoadLocal addr =>
                            let
                                val () =
                                    if closure 
                                    then Array.update (closuresForLocals, addr, true)
                                    else ()
                            in
                                Array.sub(argProperties, addr)
                            end

                        |   LoadArgument addr => (argClosure(addr, closure); [])

                        |   LoadRecursive => (recursive closure; [])
                            
                        |   LoadClosure entry => #2 (nonLocals (entry, closure))
                in
                    (* Just return the closure entry. *)
                    (BICLoadClosure(makeClosureEntry (!newGrefs, !newNorefs)), argProps)
                end

                fun recCall closure =
                    (* Reference to the closure itself. *)
                    ( if closure then makeClosureForRecursion := true else (); BICLoadRecursive )

                local
                    datatype tri = TriUnref | TriCall | TriClosure
                    val argClosureArray = Array.array(List.length argTypes, TriUnref)
                in
                    fun argClosure(n, t) =
                        Array.update(argClosureArray, n,
                            (* If this is true it requires a closure.  If it is false it
                               requires a closure if any other reference does. *)
                            if t orelse Array.sub(argClosureArray, n) = TriClosure then TriClosure else TriCall)
                    fun closureFreeArgs() =
                        Array.foldri(fn (n, TriCall, l) => n :: l | (_, _, l) => l) [] argClosureArray
                end

                (* process the body *)
                val newLocalAddresses = ref 0
                val (insertedCode, _) =
                    copyCode (lambdaBody, prev, recCall, localCount, newLocalAddresses, argClosure)
                val globalRefs = !newGrefs
                val cfArgs = closureFreeArgs()
            in
                (BICLambda 
                    {
                        body          = insertedCode,
                        name          = lambdaName,
                        closure       = [],
                        argTypes      = map #1 argTypes,
                        resultType    = resultType,
                        localCount    = ! newLocalAddresses,
                        heapClosure   = false
                    },
                 globalRefs, ! makeClosureForRecursion, cfArgs)
            end (* copyLambda *)

                (* Copy the closure of a function which has previously been
                processed by copyLambda. *)
            and copyProcClosure (BICLambda{ body, name, argTypes,
                                           resultType, localCount, ...}, newClosure, heapClosure) =
                let
                    (* process the non-locals in this function *)
                    (* If a heap closure is needed then any functions referred to
                       from the closure also need heap closures.*)
                    fun makeLoads ext = locaddr(ext, heapClosure)
               
                    val copyRefs = rev (map makeLoads newClosure)
                in
                    BICLambda
                      {
                        body          = body,
                        name          = name,
                        closure       = copyRefs,
                        argTypes      = argTypes,
                        resultType    = resultType,
                        localCount    = localCount,
                        heapClosure   = heapClosure orelse null copyRefs (* False if closure is empty *)
                      }
                end
            |  copyProcClosure(pt, _, _) = pt (* may now be a constant *)
            (* end copyProcClosure *)
        in
            case pt of
                Lambda lam => 
                    let
                        val (copiedLambda, newClosure, _, cfArgs) = copyLambda lam
                        val code = copyProcClosure (copiedLambda, newClosure, true)
                        val props =
                            if null cfArgs then [] else [Universal.tagInject closureFreeArgsTag cfArgs]
                    in
                        (code, props)
                    end

            |   c as Newenv(_, exp) =>
                    let
                        val code = insert c

                        fun getProps(Extract(LoadLocal addr)) =
                            let
                                val cfArgs = Array.sub(argProperties, addr)
                            in
                                if null cfArgs then [] else [Universal.tagInject closureFreeArgsTag cfArgs]
                            end

                        |   getProps(Tuple { fields, ...}) =
                            let
                                val fieldProps = map getProps fields
                            in
                                if List.all null fieldProps
                                then []
                                else [Universal.tagInject CodeTags.tupleTag fieldProps]
                            end

                        |   getProps _ = []

                        val props = getProps exp
                     in
                        (code, props)
                    end

            |   c as Constnt(_, p) => (insert c, p)

            |   pt => (insert pt, [])
        end (* copyCode *)

        val outputAddresses = ref 0
        fun topLevel _ = raise InternalError "outer level reached in copyCode"
        val (insertedCode, argProperties) = 
            copyCode (pt, topLevel, topLevel, localAddressCount, outputAddresses, fn _ => ())
    in
        (insertedCode, argProperties)
    end (* staticLinkAndCases *)

    fun codeGenerate(lambda: lambdaForm, debugSwitches, closure) =
    let
        val (code, argProperties) = staticLinkAndCases(Lambda lambda, 0)
        val backendCode = code
        val () =
            if DEBUG.getParameter DEBUG.codetreeAfterOptTag debugSwitches
            then PRETTY.getCompilerOutput debugSwitches (BACKENDTREE.pretty backendCode) else ()
        val bicLambda = case backendCode of BACKENDTREE.BICLambda lam => lam | _ => raise InternalError "Not BICLambda"
        val () = GCODE.gencodeLambda(bicLambda, debugSwitches, closure)
    in
        argProperties
    end
    
    structure Foreign = GCODE.Foreign
    
    (* Sharing can be copied from CODETREE. *)
    structure Sharing = BASECODETREE.Sharing
end;
