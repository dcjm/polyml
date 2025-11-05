(*
    Copyright (c) 2012-13, 2015-17, 2020-1, 2025 David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    Licence version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public Licence for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

functor CodetreeStaticLinkAndCases(
    structure BaseCodeTree: BASECODETREE
    structure CodetreeFunctions: CODETREEFUNCTIONS
    structure GCode: GENCODE
    structure Debug: DEBUG
    structure Pretty : PRETTY
    structure BackendTree: BACKENDINTERMEDIATECODE

    sharing
        BaseCodeTree.Sharing
    =   CodetreeFunctions.Sharing
    =   GCode.Sharing
    =   Pretty.Sharing
    =   BackendTree.Sharing
) : CODEGENTREE
=
struct
    
    (* This module converts the codetree used in the front-end to the form
       used in the back-end.  It recognises sequences of comparisons that
       can be converted into an indexed case.
       Previously it also recognised when a closure could be replaced by a
       static link but that has long since been superseded by the lambda
       lifter.  This was later adapted to recognise when it was possible
       to create a closure on the stack rather than on the heap.  That
       is no longer used so this has been greatly simplified. *)

    open BaseCodeTree
    open Address
    open BackendTree

    datatype caseType = datatype BackendTree.caseType

    exception InternalError = Misc.InternalError

    open BackendTree.CodeTags
    
    datatype maybeCase =
        IsACase of
        {
            cases   : (backendIC * word) list,
            test    : backendIC,
            caseType: caseType,
            default : backendIC
        }
    |   NotACase of backendIC

    fun staticLinkAndCases pt =
    let
        fun copyCode (pt: codetree):backendIC  =
        let
        
            fun locaddr (LoadLocal addr) = BICLoadLocal addr
            |   locaddr(LoadArgument addr) = BICLoadArgument addr
            |   locaddr LoadRecursive = BICLoadRecursive
            |   locaddr(LoadClosure addr) = BICLoadClosure addr

            fun insert(Eval { function, argList, resultType, ...}):backendIC =
                let
                    (* Process the arguments first. *)
                    val newargs = map(fn (c, t) => (insert c, t)) argList
                    val func  = insert function
                in
                    BICEval {function = func, argList = newargs, resultType=resultType}
                end

            |   insert(Nullary{oper}) = BICNullary{oper=oper}

            |   insert(Unary { oper, arg1 }) = BICUnary { oper = oper, arg1 = insert arg1 }

            |   insert(Binary { oper, arg1, arg2 }) = BICBinary { oper = oper, arg1 = insert arg1, arg2 = insert arg2 }
            
            |   insert(Arbitrary { oper=ArbCompare test, shortCond, arg1, arg2, longCall}) =
                let
                    val insArg1 = insert arg1 and insArg2 = insert arg2
                    and insCall = insert longCall and insShort = insert shortCond
                    (* We have to rewrite this.
                       e.g. if isShort i andalso isShort j then toShort i < toShort j else callComp(i, j) < 0
                       This isn't done at the higher level because we'd like to recognise cases of
                       comparisons with short constants *)
                    fun fixedComp(arg1, arg2) =
                        BICBinary { oper = BuiltIns.WordComparison{test=test, isSigned=true}, arg1 = arg1, arg2 = arg2 }
                in
                    BICCond(insShort, fixedComp(insArg1, insArg2), insCall)
                end

            |   insert(Arbitrary { oper=ArbArith arith, shortCond, arg1, arg2, longCall}) =
                let
                    val insArg1 = insert arg1 and insArg2 = insert arg2
                    and insCall = insert longCall and insShort = insert shortCond
                in
                    BICArbitrary{oper=arith, shortCond=insShort, arg1=insArg1, arg2=insArg2, longCall=insCall}
                end

            |   insert(AllocateWordMemory {numWords, flags, initial}) =
                    BICAllocateWordMemory { numWords = insert numWords, flags = insert flags, initial = insert initial }

            |   insert(Extract ext) =
                    (* Load the value bound to an identifier. The closure flag is
                       set to true since the only cases where a closure is not needed,
                       eval and load-andStore, are handled separately. *)
                    BICExtract(locaddr ext)

            |   insert(Indirect {base, offset, indKind=IndContainer}) = BICLoadContainer {base = insert base, offset = offset}

            |   insert(Indirect {base, offset, ...}) = BICField {base = insert base, offset = offset}

            |   insert(Constnt wp) = BICConstnt wp (* Constants can be returned untouched. *)

            |   insert(BeginLoop{loop=body, arguments=argList, ...}) = (* Start of tail-recursive inline function. *)
                let
                    val insBody = insert body
                    local
                        fun copyDec({value, addr, ...}, t) = ({addr=addr, value=insert value}, t)
                    in
                        val newargs = map copyDec argList
                    end
                in
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
                    fun copyDeclarations [] = []

                    |   copyDeclarations(Declar{addr, value = pt, ...} :: rest) =
                            BICDeclar{addr=addr, value=insert pt} :: copyDeclarations rest

                    |   copyDeclarations(RecDecs mutualDecs :: rest) =
                        let
                            fun copyEntry{addr, lambda, ...} = {addr=addr, lambda=insertLambda lambda}
                        in
                            BICRecDecs(map copyEntry mutualDecs) :: copyDeclarations rest
                        end
                    
                    |   copyDeclarations(NullBinding v :: rest) = BICNullBinding(insert v) :: copyDeclarations rest

                    |   copyDeclarations(Container{addr, size, setter, ...} :: rest) =
                            BICDecContainer{addr=addr, size=size} :: BICNullBinding (insert setter) :: copyDeclarations rest
                in
                    BICNewenv(copyDeclarations ptElist, insert ptExp)
                end (* isNewEnv *)
                
            |   insert(Tuple { fields, ...}) = BICTuple (map insert fields)
      
            |   insert(Lambda lam) = BICLambda(insertLambda lam)

            |   insert(Handle { exp, handler, exPacketAddr }) =
                let
                    val hand = insert handler
                    val exp = insert exp
                in
                    BICHandle {exp = exp, handler = hand, exPacketAddr=exPacketAddr}
                end

            |   insert(SetContainer {container, tuple=Constnt(tuple, _) , filter}) =
                let
                    (* Extract the fields now.  SetContainer with a Tuple argument copies the individual
                       fields whereas if the tuple is anything else the code will generate an indirection. *)
                    val tupleAddr = Address.toAddress tuple
                    fun getField i = BICConstnt(Address.loadWord(tupleAddr, Word.fromInt i), [])
                    val fields = List.tabulate(BoolVector.length filter, getField)
                in
                    BICSetContainer{container = insert container, tuple=BICTuple fields, filter = filter}
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

            and insertLambda{body, name, closure, argTypes, resultType, localCount, ... } : bicLambdaForm =
                { body = insert body, name=name, closure = map locaddr closure, argTypes=map #1 argTypes,
                  resultType=resultType, localCount=localCount }

            and insertAddress{base, index, offset} =
                {base=insert base, index=Option.map insert index, offset=offset}

          and copyCond (condTest, condThen, condElse): maybeCase =
            let
              (* Process the then-part. *)
              val insThen = insert condThen
              (* Process the else-part.  If it's a conditional process it here. *)
              val insElse =
                case condElse of
                    Cond(i, t, e) => copyCond(i, t, e)
                |   _ => NotACase(insert condElse)
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

                (* If we have a call to the int equality operation then we may be able to use
                   an indexed case.  N.B. This works equally for word values (unsigned) and
                   fixed precision int (unsigned) but is unsafe for arbitrary precision since
                   the lower levels assume that all values are tagged.
                   This could be used for PointerEq which is what arbitrary precision will generate
                   provided that there was an extra check for long values.  N.B. the same also
                   happens for
                   e.g. datatype t = A | B | C | D | E of int*int
                   i.e. one non-nullary constructor. *)
                fun findCase (BICBinary{oper=BuiltIns.WordComparison{test=BuiltIns.TestEqual, ...}, arg1, arg2}) =
                let
                in
                    case (arg1, arg2) of
                        (BICConstnt(c1, _), arg2) =>
                        if isShort c1
                        then SOME{tag=toShort c1, test=arg2, caseType = CaseWord}
                        else NONE (* Not a short constant. *)
                    
                     | (arg1, BICConstnt(c2, _)) =>
                        if isShort c2
                        then SOME{tag=toShort c2, test=arg1, caseType = CaseWord}
                        else NONE (* Not a short constant. *)
                    
                    | _ => NONE
                       (* Wrong number of arguments - should raise exception? *)
                end

             |  findCase(BICTagTest { test, tag, maxTag }) =
                    SOME { tag=tag, test=test, caseType=CaseTag maxTag }

             |  findCase _ = NONE
        
              val testCase = findCase insFirst
            in

              case testCase of
                    NONE => (* Can't use a case *)
                        NotACase(BICCond (insFirst, insThen, reconvertCase insElse))
                |   SOME { tag=caseTags, test=caseTest, caseType=caseCaseTest } =>
                        (* Can use a case. Can we combine two cases?
                          If we have an expression like 
                               "if x = a then .. else if x = b then ..."
                          we can combine them into a single "case". *)
                        case insElse of
                            IsACase { cases=nextCases, test=nextTest, default=nextDefault, caseType=nextCaseType } =>
                            (
                                case (similar(nextTest, caseTest), caseCaseTest = nextCaseType) of
                                  (* Note - it is legal (though completely redundant) for the
                                     same case to appear more than once in the list. This is not
                                      checked for at this stage. *)
                                    (Similar _, true) =>
                                        IsACase 
                                        {
                                            cases   = (insThen, caseTags) ::
                                                        map (fn (c, l) => (c, l)) nextCases,
                                            test    = nextTest,
                                            default = nextDefault,
                                            caseType = caseCaseTest
                                        }

                                    | _ => (* Two case expressions but they test different
                                              variables. We can't combine them. *)
                                        IsACase
                                        {
                                            cases   = [(insThen, caseTags)],
                                            test    = caseTest,
                                            default = reconvertCase insElse,
                                            caseType=caseCaseTest
                                        }
                            )
                            | NotACase elsePart => (* insElse is not a case *)
                                IsACase
                                {
                                    cases   = [(insThen, caseTags)],
                                    test    = caseTest,
                                    default = elsePart,
                                    caseType=caseCaseTest
                                }
            end

            (* Check something that's been created as a Case and see whether it is sparse.
               If it is turn it back into a sequence of conditionals.  This was previously
               done at the bottom level and the choice of when to use an indexed case was
               made by the architecture-specific code-generator.  That's probably unnecessary
               and complicates the code-generator. *)
            and reconvertCase(IsACase{cases, test, default, caseType}) =
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
                    then
                    let
                        (* Create a contiguous range of labels.  Eliminate any duplicates which are
                           legal but redundant. *)
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

                        (* Create labels for each of the cases.  Fill in any gaps with entries that
                           will point to the default.  We have to be careful if max happens to be
                           the largest value of Word.word.  In that case adding one to the range
                           will give us a value less than max. *)
                        fun extendCase(indexVal, cl as ((c, caseValue) :: cps)) =
                            if indexVal + min = caseValue
                            then SOME c :: extendCase(indexVal+0w1, cps)
                            else NONE :: extendCase(indexVal+0w1, cl)

                        |   extendCase(indexVal, []) =
                            (* We may not be at the end if this came from a CaseTag *)
                                if indexVal > max-min
                                then []
                                else NONE :: extendCase(indexVal+0w1, [])

                        val fullCaseRange = extendCase(0w0, cases)
                        val _ = Word.fromInt(List.length fullCaseRange) = max-min+0w1 orelse raise InternalError "Cases"
                    in
                        BICCase{cases=fullCaseRange, test=test, default=default, isExhaustive=isExhaustive, firstIndex=min}
                    end
                    else
                    let
                        fun reconvert [] = default
                        |   reconvert ((c, t) :: rest) =
                            let
                                val test =
                                    case caseType of
                                        CaseWord =>
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
            |   reconvertCase (NotACase t) = t (* Just a simple conditional. *)

        in
            insert pt
        end

        val insertedCode = copyCode pt
    in
        insertedCode
    end (* staticLinkAndCases *)
    
    type closureRef = GCode.closureRef

    fun codeGenerate(lambda: lambdaForm, debugSwitches, closure) =
    let
        val code = staticLinkAndCases(Lambda lambda)
        val backendCode = code
        val () =
            if Debug.getParameter Debug.codetreeAfterOptTag debugSwitches
            then Pretty.getCompilerOutput debugSwitches (BackendTree.pretty backendCode) else ()
        val bicLambda = case backendCode of BackendTree.BICLambda lam => lam | _ => raise InternalError "Not BICLambda"
        val () = GCode.gencodeLambda(bicLambda, debugSwitches, closure)
    in
        []
    end
    
    structure Foreign = GCode.Foreign
    
    (* Sharing can be copied from CODETREE. *)
    structure Sharing =
    struct
        open BaseCodeTree.Sharing
        type closureRef = closureRef
    end
end;
