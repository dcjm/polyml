(*
    Copyright (c) 2013, 2016-17, 2020-3, 2025-6 David C.J. Matthews

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
    This is a cut-down version of the optimiser which simplifies the code but
    does not apply any heuristics.  It follows chained bindings, in particular
    through tuples, folds constants expressions involving built-in functions,
    expands inline functions that have previously been marked as inlineable.
    It does not detect small functions that can be inlined nor does it
    code-generate functions without free variables.
*)

functor CodetreeSimplifier(
    structure BaseCodeTree: BASECODETREE

    structure CodetreeFunctions: CODETREEFUNCTIONS

    structure RemoveRedundant:
    sig
        type codetree
        type loadForm
        type codeUse
        val cleanProc : (codetree * codeUse list * (int -> loadForm) * int) -> codetree
        structure Sharing: sig type codetree = codetree and loadForm = loadForm and codeUse = codeUse end
    end

    structure Debug: DEBUG

    sharing
        BaseCodeTree.Sharing
    =   CodetreeFunctions.Sharing
    =   RemoveRedundant.Sharing
) :
    sig
        type codetree and codeBinding and envSpecial

        val simplifier:
            { code: codetree, numLocals: int, maxInlineSize: int } ->
                (codetree * codeBinding list * envSpecial) * int * bool
        val specialToGeneral:
            codetree * codeBinding list * envSpecial -> codetree

        structure Sharing:
        sig
            type codetree = codetree
            and codeBinding = codeBinding
            and envSpecial = envSpecial
        end
    end
=
struct
    open BaseCodeTree
    open Address
    open CodetreeFunctions
    open BuiltIns

    exception InternalError = Misc.InternalError

    exception RaisedException
    
    (* The bindings are held internally as a reversed list.  This
       is really only a check that the reversed and forward lists
       aren't confused. *)
    datatype revlist = RevList of codeBinding list

    type simpContext =
    {
        lookupAddr: loadForm -> envGeneral * envSpecial,
        enterAddr: int * (envGeneral * envSpecial) -> unit,
        nextAddress: unit -> int,
        reprocess: bool ref,
        maxInlineSize: int
    }

    fun envGeneralToCodetree(EnvGenLoad ext) = Extract ext
    |   envGeneralToCodetree(EnvGenConst w) = Constnt w

    fun mkDec (laddr, res) = Declar{value = res, addr = laddr, use=[]}

    fun mkEnv([], exp) = exp
    |   mkEnv(decs, exp as Extract(LoadLocal loadAddr)) =
        (
            (* A common case is where we have a binding as the last item
               and then a load of that binding.  Reduce this so other
               optimisations are possible.
               This is still something of a special case that could/should
               be generalised. *)
            case List.last decs of
                Declar{addr=decAddr, value, ... } =>
                    if loadAddr = decAddr
                    then mkEnv(List.take(decs, List.length decs - 1), value)
                    else Newenv(decs, exp)
            |   _ => Newenv(decs, exp)
        )
    |   mkEnv(decs, exp) = Newenv(decs, exp)

    fun isConstnt(Constnt _) = true
    |   isConstnt _ = false

    (* Wrap up the general, bindings and special value as a codetree node.  The
       special entry is discarded except for Constnt entries which are converted
       to ConstntWithInline.  That allows any inlineable code to be carried
       forward to later passes. *)
    fun specialToGeneral(g, RevList(b as _ :: _), s) = mkEnv(List.rev b, specialToGeneral(g, RevList [], s))
    |   specialToGeneral(Constnt(w, p), RevList [], s) = Constnt(w, setInline s p)
    |   specialToGeneral(g, RevList [], _) = g

    (* Convert a constant to a fixed value.  Used in some constant folding. *)
    val toFix: machineWord -> FixedInt.int = FixedInt.fromInt o Word.toIntX o toShort

    local
        val ffiSizeFloat: unit -> int = RunCall.rtsCallFast1 "PolySizeFloat"
        and ffiSizeDouble: unit -> int = RunCall.rtsCallFast1 "PolySizeDouble"
        val sysWordSize: word = RunCall.memoryCellLength(Word.toLargeWord 0w0) * RunCall.bytesPerWord
    in
        (* If we have a constant index value we convert that into a byte offset. We need
           to know the size of the item on this platform.  We have to make this check
           when we actually compile the code because the interpreted version will
           generally be run on a platform different from the one the pre-built
           compiler was compiled on. The ML word length will be the same because
           we have separate pre-built compilers for 32 and 64-bit.
           Loads from C memory use signed offsets.  Loads from ML memory never
           have a negative offset and are limited by the maximum size of a cell
           so can always be unsigned. *)
        fun getMultiplier (LoadStoreMLWord _)   = (Word.toInt RunCall.bytesPerWord, false (* unsigned *))
        |   getMultiplier (LoadStoreMLByte _)   = (1, false)
        |   getMultiplier LoadStoreC8           = (1, true (* signed *) )
        |   getMultiplier LoadStoreC16          = (2, true (* signed *) )
        |   getMultiplier LoadStoreC32          = (4, true (* signed *) )
        |   getMultiplier LoadStoreC64          = (8, true (* signed *) )
        |   getMultiplier LoadStoreCFloat       = (ffiSizeFloat(), true (* signed *) )
        |   getMultiplier LoadStoreCDouble      = (ffiSizeDouble(), true (* signed *) )
        |   getMultiplier LoadStoreUntaggedUnsigned = (Word.toInt RunCall.bytesPerWord, false (* unsigned *))
        |   getMultiplier (LoadStorePolyWord _)  = (Word.toInt RunCall.bytesPerWord, false)
        |   getMultiplier (LoadStoreNativeWord _)  = (Word.toInt sysWordSize, false)
    end

    fun simplify(c, s) = mapCodetree (simpGeneral s) c

    (* Process the codetree to return a codetree node.  This is used
       when we don't want the special case. *)
    and simpGeneral { lookupAddr, ...} (Extract ext) =
        let
            val (gen, spec) = lookupAddr ext
        in
            SOME(specialToGeneral(envGeneralToCodetree gen, RevList [], spec))
        end

    |   simpGeneral context (Newenv envArgs) =
            SOME(specialToGeneral(simpNewenv(envArgs, context, RevList [])))

    |   simpGeneral context (Lambda lambda) =
            SOME(Lambda(#1(simpLambda(lambda, context, NONE, NONE))))

    |   simpGeneral context (Eval {function, argList, resultType}) =
            SOME(specialToGeneral(simpFunctionCall(function, argList, resultType, context, RevList[])))

        (* BuiltIn0 functions can't be processed specially. *)

    |   simpGeneral context (Unary{oper, arg1}) =
            SOME(specialToGeneral(simpUnary(oper, arg1, context, RevList [])))

    |   simpGeneral context (Binary{oper, arg1, arg2}) =
            SOME(specialToGeneral(simpBinary(oper, arg1, arg2, context, RevList [])))

    |   simpGeneral context (Arbitrary{oper=ArbCompare test, shortCond, arg1, arg2, longCall}) =
            SOME(specialToGeneral(simpArbitraryCompare(test, shortCond, arg1, arg2, longCall, context, RevList [])))

    |   simpGeneral context (Arbitrary{oper=ArbArith arith, shortCond, arg1, arg2, longCall}) =
            SOME(specialToGeneral(simpArbitraryArith(arith, shortCond, arg1, arg2, longCall, context, RevList [])))

    |   simpGeneral context (AllocateWordMemory {numWords, flags, initial}) =
            SOME(specialToGeneral(simpAllocateWordMemory(numWords, flags, initial, context, RevList [])))

    |   simpGeneral context (Cond(condTest, condThen, condElse)) =
            SOME(specialToGeneral(simpIfThenElse(condTest, condThen, condElse, context, RevList [])))

    |   simpGeneral context (Tuple { fields, isVariant }) =
            SOME(specialToGeneral(simpTuple(fields, isVariant, context, RevList [])))

    |   simpGeneral context (Indirect{ base, offset, indKind }) =
            SOME(specialToGeneral(simpFieldSelect(base, offset, indKind, context, RevList [])))

    |   simpGeneral context (SetContainer{container, tuple, filter}) =
        let
            val optCont = simplify(container, context)
            val (cGen, cDecs, cSpec) = simpSpecial(tuple, context, RevList [])
        in
            case cSpec of
                (* If the tuple is a local binding it is simpler to pick it up from the
                   "special" entry. *)
                EnvSpecTuple(size, recEnv) =>
                let
                    val fields = List.tabulate(size, envGeneralToCodetree o #1 o recEnv)
                in
                    SOME(simpPostSetContainer(optCont, Tuple{isVariant=false, fields=fields}, cDecs, filter))
                end

            |   _ => SOME(simpPostSetContainer(optCont, cGen, cDecs, filter))
        end

    |   simpGeneral (context as { enterAddr, nextAddress, reprocess, ...}) (BeginLoop{loop, arguments, ...}) =
        let
            val didReprocess = ! reprocess
            (* To see if we really need the loop first try simply binding the
               arguments and process it.  It's often the case that if one
               or more arguments is a constant that the looping case will
               be eliminated. *)
            val withoutBeginLoop =
                simplify(mkEnv(List.map (Declar o #1) arguments, loop), context)
            
            fun foldLoop f n (Loop l) = f(l, n)
            |   foldLoop f n (Newenv(_, exp)) = foldLoop f n exp
            |   foldLoop f n (Cond(_, t, e)) = foldLoop f (foldLoop f n t) e
            |   foldLoop f n (Handle {handler, ...}) = foldLoop f n handler
            |   foldLoop f n (SetContainer{tuple, ...}) = foldLoop f n tuple
            |   foldLoop _ n _ = n
            (* Check if the Loop instruction is there.  This assumes that these
               are the only tail-recursive cases. *)
            val hasLoop = foldLoop (fn _ => true) false
        in
            if not (hasLoop withoutBeginLoop)
            then SOME withoutBeginLoop
            else
            let
                (* Reset "reprocess".  It may have been set in the withoutBeginLoop
                   that's not the code we're going to return. *)
                val () = reprocess := didReprocess
                (* We need the BeginLoop. Create new addresses for the arguments. *)
                fun declArg({addr, value, use, ...}, typ) =
                    let
                        val newAddr = nextAddress()
                    in
                        enterAddr(addr, (EnvGenLoad(LoadLocal newAddr), EnvSpecNone));
                        ({addr = newAddr, value = simplify(value, context), use = use }, typ)
                    end
                (* Now look to see if the (remaining) loops have any arguments that do not change.
                   Do this after processing because we could be eliminating other loops that
                   may change the arguments. *)
                val declArgs = map declArg arguments
                val beginBody = simplify(loop, context)
                
                local
                    fun argsMatch((Extract (LoadLocal argNo), _), ({addr, ...}, _)) = argNo = addr
                    |   argsMatch _ = false
                    
                    fun checkLoopArgs(loopArgs, checks) =
                    let
                        fun map3(loopA :: loopArgs, decA :: decArgs, checkA :: checkArgs) =
                            (argsMatch(loopA, decA) andalso checkA) :: map3(loopArgs, decArgs, checkArgs)
                        |   map3 _ = []
                    in
                        map3(loopArgs, declArgs, checks)
                    end
                in
                    val checkList = foldLoop checkLoopArgs (map (fn _ => true) arguments) beginBody
                end
            in
                if List.exists (fn l => l) checkList
                then
                let
                    (* Turn the original arguments into bindings. *)
                    local
                        fun argLists(true, (arg, _), (tArgs, fArgs)) = (Declar arg :: tArgs, fArgs)
                        |   argLists(false, arg, (tArgs, fArgs)) = (tArgs, arg :: fArgs)
                    in
                        val (unchangedArgs, filteredDeclArgs) = ListPair.foldrEq argLists ([], [])  (checkList, declArgs)
                    end
                    fun changeLoops (Loop loopArgs) =
                        let
                            val newArgs =
                                ListPair.foldrEq(fn (false, arg, l) => arg :: l | (true, _, l) => l) [] (checkList, loopArgs)
                        in
                            Loop newArgs
                        end
                    |   changeLoops(Newenv(decs, exp)) = Newenv(decs, changeLoops exp)
                    |   changeLoops(Cond(i, t, e)) = Cond(i, changeLoops t, changeLoops e)
                    |   changeLoops(Handle{handler, exp, exPacketAddr}) =
                            Handle{handler=changeLoops handler, exp=exp, exPacketAddr=exPacketAddr}
                    |   changeLoops(SetContainer{tuple, container, filter}) =
                            SetContainer{tuple=changeLoops tuple, container=container, filter=filter}
                    |   changeLoops code = code
                    
                    val beginBody = simplify(changeLoops loop, context)
                    (* Reprocess because we've lost any special part from the arguments that
                       haven't changed. *)
                    val () = reprocess := true
                in
                    SOME(mkEnv(unchangedArgs, BeginLoop {loop=beginBody, arguments=filteredDeclArgs}))
                end
                else SOME(BeginLoop {loop=beginBody, arguments=declArgs})
            end
        end

    |   simpGeneral context (TagTest{test, tag, maxTag}) =
        (
            case simplify(test, context) of
                Constnt(testResult, _) =>
                    if isShort testResult andalso toShort testResult = tag
                    then SOME CodeTrue
                    else SOME CodeFalse
            |   sTest => SOME(TagTest{test=sTest, tag=tag, maxTag=maxTag})
        )

    |   simpGeneral context (LoadOperation{kind, address}) =
        let
            (* Try to move constants out of the index. *)
            val (genAddress, RevList decAddress) = simpAddress(address, getMultiplier kind, context)
            (* If the base address and index are constant and this is an immutable
               load we can do this at compile time. *)
            val result =
                case (genAddress, kind) of
                    ({base=Constnt(baseAddr, _), index=NONE, offset}, LoadStoreMLWord _) =>
                    if isShort baseAddr
                    then LoadOperation{kind=kind, address=genAddress}
                    else
                    let
                        (* Ignore the "isImmutable" flag and look at the immutable status of the memory.
                           Check that this is a word object and that the offset is within range.
                           The code for Vector.sub, for example, raises an exception if the index
                           is out of range but still generates the (unreachable) indexing code. *)
                        val addr = toAddress baseAddr
                        val wordOffset = Word.fromInt offset div RunCall.bytesPerWord
                    in
                        if isMutable addr orelse not(isWords addr) orelse wordOffset >= length addr
                        then LoadOperation{kind=kind, address=genAddress}
                        else Constnt(toMachineWord(loadWord(addr, wordOffset)), [])
                    end

                |   ({base=Constnt(baseAddr, _), index=NONE, offset}, LoadStoreMLByte _) =>
                    if isShort baseAddr
                    then LoadOperation{kind=kind, address=genAddress}
                    else
                    let
                        val addr = toAddress baseAddr
                        val wordOffset = Word.fromInt offset div RunCall.bytesPerWord
                    in
                        if isMutable addr orelse not(isBytes addr) orelse wordOffset >= length addr
                        then LoadOperation{kind=kind, address=genAddress}
                        else Constnt(toMachineWord(loadByte(addr, Word.fromInt offset)), [])
                    end

                |   ({base=Constnt(baseAddr, _), index=NONE, offset}, LoadStoreUntaggedUnsigned) =>
                    if isShort baseAddr
                    then LoadOperation{kind=kind, address=genAddress}
                    else
                    let
                        val addr = toAddress baseAddr
                        (* We don't currently have loadWordUntagged in Address but it's only ever
                           used to load the string length word so we can use that. *)
                    in
                        if isMutable addr orelse not(isBytes addr) orelse offset <> 0
                        then LoadOperation{kind=kind, address=genAddress}
                        else Constnt(toMachineWord(String.size(RunCall.unsafeCast addr)), [])
                    end

                |   _ => LoadOperation{kind=kind, address=genAddress}
        in
            SOME(mkEnv(List.rev decAddress, result))
        end

    |   simpGeneral context (StoreOperation{kind, address, value}) =
        let
            val (genAddress, decAddress) = simpAddress(address, getMultiplier kind, context)
            val (genValue, RevList decValue, _) = simpSpecial(value, context, decAddress)
        in 
            SOME(mkEnv(List.rev decValue, StoreOperation{kind=kind, address=genAddress, value=genValue}))
        end

    |   simpGeneral (context as {reprocess, ...}) (BlockOperation{kind, sourceLeft, destRight, length}) =
        let
            val multiplier =
                case kind of
                    BlockOpMove{isByteMove=false} => Word.toInt RunCall.bytesPerWord
                |   BlockOpMove{isByteMove=true} => 1
                |   BlockOpEqualByte => 1
                |   BlockOpCompareByte => 1
            val (genSrcAddress, RevList decSrcAddress) = simpAddress(sourceLeft, (multiplier, false), context)
            val (genDstAddress, RevList decDstAddress) = simpAddress(destRight, (multiplier, false), context)
            val (genLength, RevList decLength, _) = simpSpecial(length, context, RevList [])
            (* If we have a short length move we're better doing it as a sequence of loads and stores.
               This is particularly useful with string concatenation.  Small here means three or less.
               Four and eight byte moves are handled as single instructions in the code-generator
               provided the alignment is correct. *)
            val shortLength =
                case genLength of
                    Constnt(lenConst, _) =>
                        if isShort lenConst then let val l = toShort lenConst in if l <= 0w3 then SOME l else NONE end else NONE
                |   _ => NONE
            val combinedDecs = List.rev decSrcAddress @ List.rev decDstAddress @ List.rev decLength
            val operation =
                case (shortLength, kind) of
                    (SOME length, BlockOpMove{isByteMove}) =>
                    let
                        val _ = reprocess := true (* Frequently the source will be a constant. *)
                        val {base=baseSrc, index=indexSrc, offset=offsetSrc} = genSrcAddress
                        and {base=baseDst, index=indexDst, offset=offsetDst} = genDstAddress
                        (* This is uses non-synchronised load and store so we set isImmutable to true *)
                        val moveKind =
                            if isByteMove then LoadStoreMLByte{isImmutable=true} else LoadStoreMLWord{isImmutable=true}
                        fun makeMoves offset =
                        if offset = Word.toInt length
                        then []
                        else NullBinding(
                                StoreOperation{kind=moveKind,
                                    address={base=baseDst, index=indexDst, offset=offsetDst+offset*multiplier},
                                    value=LoadOperation{kind=moveKind, address={base=baseSrc, index=indexSrc, offset=offsetSrc+offset*multiplier}}}) ::
                                makeMoves(offset+1)
                    in
                        mkEnv(combinedDecs @ makeMoves 0, CodeZero (* unit result *))
                    end

                |   (SOME length, BlockOpEqualByte) => (* Comparing with the null string and up to 3 characters. *)
                    let
                        val {base=baseSrc, index=indexSrc, offset=offsetSrc} = genSrcAddress
                        and {base=baseDst, index=indexDst, offset=offsetDst} = genDstAddress
                        val moveKind = LoadStoreMLByte{isImmutable=true}
                        
                        (* Build andalso tree to check each byte.  For the null string this simply returns "true". *)
                        fun makeComparison offset =
                        if offset = Word.toInt length
                        then CodeTrue
                        else Cond(
                                Binary{oper=WordComparison{test=TestEqual, isSigned=false},
                                    arg1=LoadOperation{kind=moveKind, address={base=baseSrc, index=indexSrc, offset=offsetSrc+offset*multiplier}},
                                    arg2=LoadOperation{kind=moveKind, address={base=baseDst, index=indexDst, offset=offsetDst+offset*multiplier}}},
                                makeComparison(offset+1),
                                CodeFalse)
                    in
                        mkEnv(combinedDecs, makeComparison 0)
                    end

                |   _ =>
                    mkEnv(combinedDecs, 
                        BlockOperation{kind=kind, sourceLeft=genSrcAddress, destRight=genDstAddress, length=genLength})
        in
            SOME operation
        end

    |   simpGeneral (context as {enterAddr, nextAddress, reprocess, ...}) (Handle{exp, handler, exPacketAddr}) =
        let (* We need to make a new binding for the exception packet. *)
            val expBody = simplify(exp, context)
            val newAddr = nextAddress()
            val () = enterAddr(exPacketAddr, (EnvGenLoad(LoadLocal newAddr), EnvSpecNone))
            val handleBody = simplify(handler, context)
        in
            (* If the expression cannot raise an exception we don't need the handler.  This could
               arise from constant folding where we know that expression cannot raise, say, Overflow.
               Similarly, if we know that the expression does raise an exception and the exception
               packet is itself exception-free, we can evaluate the handler directly. *)
            if exceptionFree expBody
            then (reprocess := true; SOME expBody)
            else case expBody of
                Raise expPacket =>
                    if exceptionFree expPacket
                    then (reprocess := true; SOME(mkEnv([mkDec(newAddr, expPacket)], handleBody)))
                    else SOME(Handle{exp=expBody, handler=handleBody, exPacketAddr=newAddr})

            |   _ => SOME(Handle{exp=expBody, handler=handleBody, exPacketAddr=newAddr})
        end

    |   simpGeneral _ _ = NONE

    (* Where we have an Indirect or Eval we want the argument as either a tuple or
       an inline function respectively if that's possible.  Getting that also involves
       various other cases as well. Because a binding may later be used in such a
       context we treat any binding in that way as well. *)
    and simpSpecial (Extract ext, { lookupAddr, ...}, tailDecs) =
        let
            val (gen, spec) = lookupAddr ext
        in
            (envGeneralToCodetree gen, tailDecs, spec)
        end

    |   simpSpecial (Newenv envArgs, context, tailDecs) = simpNewenv(envArgs, context, tailDecs)

    |   simpSpecial (Lambda lambda, context, tailDecs) =
        let
            val (gen, spec) = simpLambda(lambda, context, NONE, NONE)
        in
            (Lambda gen, tailDecs, spec)
        end

    |   simpSpecial (Eval {function, argList, resultType}, context, tailDecs) =
            simpFunctionCall(function, argList, resultType, context, tailDecs)

    |   simpSpecial (Unary{oper, arg1}, context, tailDecs) =
            simpUnary(oper, arg1, context, tailDecs)

    |   simpSpecial (Binary{oper, arg1, arg2}, context, tailDecs) =
            simpBinary(oper, arg1, arg2, context, tailDecs)

    |   simpSpecial (Arbitrary{oper=ArbCompare test, shortCond, arg1, arg2, longCall}, context, tailDecs) =
            simpArbitraryCompare(test, shortCond, arg1, arg2, longCall, context, tailDecs)

    |   simpSpecial (Arbitrary{oper=ArbArith arith, shortCond, arg1, arg2, longCall}, context, tailDecs) =
            simpArbitraryArith(arith, shortCond, arg1, arg2, longCall, context, tailDecs)

    |   simpSpecial (AllocateWordMemory{numWords, flags, initial}, context, tailDecs) =
            simpAllocateWordMemory(numWords, flags, initial, context, tailDecs)

    |   simpSpecial (Cond(condTest, condThen, condElse), context, tailDecs) =
            simpIfThenElse(condTest, condThen, condElse, context, tailDecs)

    |   simpSpecial (Tuple { fields, isVariant }, context, tailDecs) = simpTuple(fields, isVariant, context, tailDecs)

    |   simpSpecial (Indirect{ base, offset, indKind }, context, tailDecs) = simpFieldSelect(base, offset, indKind, context, tailDecs)

    |   simpSpecial (c: codetree, s: simpContext, tailDecs): codetree * revlist * envSpecial =
        let
            (* Anything else - copy it and then split it into the fields. *)
            fun split(Newenv(l, e), RevList tailDecs) = (* Pull off bindings. *)
                    split (e, RevList(List.rev l @ tailDecs))
            |   split(Constnt(m, p), tailDecs) = (Constnt(m, p), tailDecs, findInline p)
            |   split(c, tailDecs) = (c, tailDecs, EnvSpecNone)
        in
            split(simplify(c, s), tailDecs)
        end

    (* Process a Newenv.  We need to add the bindings to the context. *)
    and simpNewenv((envDecs: codeBinding list, envExp), context as { enterAddr, nextAddress, reprocess, ...}, tailDecs): codetree * revlist * envSpecial =
    let
        fun copyDecs ([], decs) =
            simpSpecial(envExp, context, decs) (* End of the list - process the result expression. *)

        |   copyDecs ((Declar{addr, value, ...} :: vs), decs) =
            (
                case simpSpecial(value, context, decs) of
                    (* If this raises an exception stop here. *)
                    vBinding as (Raise _, _, _) => vBinding

                |   vBinding =>
                    let
                        (* Add the declaration to the table. *)
                        val (optV, dec) = makeNewDecl(vBinding, context)
                        val () = enterAddr(addr, optV)                  
                    in
                        copyDecs(vs, dec)
                    end
            )

        |   copyDecs(NullBinding v :: vs, decs) = (* Not a binding - process this and the rest.*)
            (
                case simpSpecial(v, context, decs) of
                    (* If this raises an exception stop here. *)
                    vBinding as (Raise _, _, _) => vBinding

                |   (cGen, RevList cDecs, _) => copyDecs(vs, RevList(NullBinding cGen :: cDecs))
            )

        |   copyDecs(RecDecs mutuals :: vs, RevList decs) =
            (* Mutually recursive declarations. Any of the declarations may
               refer to any of the others. They should all be lambdas.

               The front end generates functions with more than one argument
               (either curried or tupled) as pairs of mutually recursive
               functions.  The main function body takes its arguments on
               the stack (or in registers) and the auxiliary inline function,
               possibly nested, takes the tupled or curried arguments and
               calls it.  If the main function is recursive it will first
               call the inline function which is why the pair are mutually
               recursive.
               As far as possible we want to use the main function since that
               uses the least memory.  Specifically, if the function recurses
               we want the recursive call to pass all the arguments if it
               can. *)
            let
                (* Reorder the function so the explicitly-inlined ones come first.
                   Their code can then be inserted into the main functions. *)
                local
                    val (inlines, nonInlines) =
                        List.partition (
                            fn {lambda = { isInline=DontInline, ...}, ... } => false | _ => true) mutuals
                in
                    val orderedDecs = inlines @ nonInlines
                end

                (* Go down the functions creating new addresses for them and entering them in the table. *)
                val addresses =
                    map (fn {addr, ... } =>
                        let
                            val decAddr = nextAddress()
                        in
                            enterAddr (addr, (EnvGenLoad(LoadLocal decAddr), EnvSpecNone));
                            decAddr
                        end)
                    orderedDecs

                fun processFunction({ lambda, addr, ... }, newAddr) =
                let
                    val (gen, spec) = simpLambda(lambda, context, SOME addr, SOME newAddr)
                    (* Update the entry in the table to include any inlineable function. *)
                    val () = enterAddr (addr, (EnvGenLoad (LoadLocal newAddr), spec))
                in
                    {addr=newAddr, lambda=gen, use=[]}
                end
                
                val rlist = ListPair.map processFunction (orderedDecs, addresses)
            in
                (* and put these declarations onto the list. *)
                copyDecs(vs, RevList(List.rev(partitionMutualBindings(RecDecs rlist)) @ decs))
            end

        |   copyDecs (Container{addr, size, setter, ...} :: vs, RevList decs) =
            let
                (* Enter the new address immediately - it's needed in the setter. *)
                val decAddr = nextAddress()
                val () = enterAddr (addr, (EnvGenLoad(LoadLocal decAddr), EnvSpecNone))
                val (setGen, RevList setDecs, _) = simpSpecial(setter, context, RevList [])
            in
                (* If we have inline expanded a function that sets the container
                   we're better off eliminating the container completely. *)
                case setGen of
                    SetContainer { tuple, filter, container } =>
                    let
                        (* Check the container we're setting is the address we've made for it. *)
                        val _ =
                            (case container of Extract(LoadLocal a) => a = decAddr | _ => false)
                                orelse raise InternalError "copyDecs: Container/SetContainer"
                        val newDecAddr = nextAddress()
                        val () = enterAddr (addr, (EnvGenLoad(LoadLocal newDecAddr), EnvSpecNone))
                        val tupleAddr = nextAddress()
                        val tupleDec = Declar{addr=tupleAddr, use=[], value=tuple}
                        val tupleLoad = mkLoadLocal tupleAddr
                        val resultTuple =
                            BoolVector.foldri(fn (i, true, l) => mkInd(i, tupleLoad) :: l | (_, false, l) => l) [] filter
                        val _ = List.length resultTuple = size
                                    orelse raise InternalError "copyDecs: Container/SetContainer size"
                        val containerDec = Declar{addr=newDecAddr, use=[], value=mkTuple resultTuple}
                        (* TODO:  We're replacing a container with what is notionally a tuple on the
                           heap.  It should be optimised away as a result of a further pass but we
                           currently have indirections from a container for these.
                           On the native platforms that doesn't matter but on 32-in-64 indirecting
                           from the heap and from the stack are different. *)
                        val _ = reprocess := true
                    in
                        copyDecs(vs, RevList(containerDec :: tupleDec :: setDecs @ decs))
                    end

                |   _ =>
                    let
                        (* The setDecs could refer the container itself if we've optimised this with
                           simpPostSetContainer so we must include them within the setter and not lift them out. *)
                        val dec = Container{addr=decAddr, use=[], size=size, setter=mkEnv(List.rev setDecs, setGen)}
                    in
                        copyDecs(vs, RevList(dec :: decs))
                    end
            end
    in
        copyDecs(envDecs, tailDecs)
    end

    (* Prepares a binding for entry into a look-up table.  Returns the entry
       to put into the table together with any bindings that must be made.
       If the general part of the optVal is a constant we can just put the
       constant in the table. If it is a load (Extract) it is just renaming
       an existing entry so we can return it.  Otherwise we have to make
       a new binding and return a load (Extract) entry for it. *)
    and makeNewDecl((Constnt w, RevList decs, spec), _) = ((EnvGenConst w, spec), RevList decs)
                (* No need to create a binding for a constant. *)

    |   makeNewDecl((Extract ext, RevList decs, spec), _) = ((EnvGenLoad ext, spec), RevList decs)
                (* Binding is simply giving a new name to a variable
                   - can ignore this declaration. *) 

    |   makeNewDecl((gen, RevList decs, spec), { nextAddress, ...}) =
        let (* Create a binding for this value. *)
            val newAddr = nextAddress()
        in
            ((EnvGenLoad(LoadLocal newAddr), spec), RevList(mkDec(newAddr, gen) :: decs))
        end

    and simpLambda({body, isInline, name, argTypes, resultType, closure, localCount, ...},
                  { lookupAddr, reprocess, maxInlineSize, ... }, myOldAddrOpt, myNewAddrOpt) =
        let
            (* A new table for the new function. *)
            val oldAddrTab = Array.array (localCount, NONE)
            val optClosureList = makeClosure()
            val isNowRecursive = ref false

            local
                fun localOldAddr (LoadLocal addr) = valOf(Array.sub(oldAddrTab, addr))
                |   localOldAddr (ext as LoadArgument _) = (EnvGenLoad ext, EnvSpecNone)
                |   localOldAddr (ext as LoadRecursive) = (EnvGenLoad ext, EnvSpecNone)
                |   localOldAddr (LoadClosure addr) =
                    let
                        val oldEntry = List.nth(closure, addr)
                        (* If the entry in the closure is our own address this is recursive. *)
                        fun isRecursive(EnvGenLoad(LoadLocal a), SOME b) =
                            if a = b then (isNowRecursive := true; true) else false
                        |   isRecursive _ = false
                    in
                        if isRecursive(EnvGenLoad oldEntry, myOldAddrOpt) then (EnvGenLoad LoadRecursive, EnvSpecNone)
                        else
                        let
                            val newEntry = lookupAddr oldEntry
                            val makeClosure = addToClosure optClosureList

                            fun convertResult(genEntry, specEntry) =
                                (* If after looking up the entry we get our new address it's recursive. *)
                                if isRecursive(genEntry, myNewAddrOpt)
                                then (EnvGenLoad LoadRecursive, EnvSpecNone)
                                else
                                let
                                    val newGeneral =
                                        case genEntry of
                                            EnvGenLoad ext => EnvGenLoad(makeClosure ext)
                                        |   EnvGenConst w => EnvGenConst w
                                    (* Have to modify the environment here so that if we look up free variables
                                       we add them to the closure. *)
                                    fun convertEnv env args = convertResult(env args)
                                    val newSpecial =
                                        case specEntry of
                                            EnvSpecTuple(size, env) => EnvSpecTuple(size, convertEnv env)
                                        |   EnvSpecInlineFunction(spec, env) => EnvSpecInlineFunction(spec, convertEnv env)
                                        |   EnvSpecUnary _ => EnvSpecNone (* Don't pass this in *)
                                        |   EnvSpecBinary _ => EnvSpecNone (* Don't pass this in *)
                                        |   EnvSpecNone => EnvSpecNone
                                in
                                    (newGeneral, newSpecial)
                                end
                        in
                            convertResult newEntry
                        end
                    end

                and setTab (index, v) = Array.update (oldAddrTab, index, SOME v)
            in
                val newAddressAllocator = ref 0

                fun mkAddr () = 
                    ! newAddressAllocator before newAddressAllocator := ! newAddressAllocator + 1

                val newCode =
                    simplify (body,
                    {
                        enterAddr = setTab, lookupAddr = localOldAddr,
                        nextAddress=mkAddr,
                        reprocess = reprocess,
                        maxInlineSize = maxInlineSize
                    })
            end

            val closureAfterOpt = extractClosure optClosureList
            val localCount = ! newAddressAllocator
            (* If we have mutually recursive "small" functions we may turn them into
               recursive functions.  We have to remove the "small" status from
               them to prevent them from being expanded inline anywhere else.  The
               optimiser may turn them back into "small" functions if the recursion
               is actually tail-recursion. *)
            val isNowInline =
                case isInline of
                    SmallInline =>
                        if ! isNowRecursive then DontInline else SmallInline
                |   InlineAlways =>
                        (* Functions marked as inline could become recursive as a result of
                           other inlining. *)
                        if ! isNowRecursive then DontInline else InlineAlways
                |   DontInline => DontInline

            (* Clean up the function body at this point if it could be inlined.
               There are examples where failing to do this can blow up.  This
               can be the result of creating both a general and special function
               inside an inline function. *)
            val cleanBody =
                if isNowInline = DontInline
                then newCode
                else RemoveRedundant.cleanProc(newCode, [UseExport], LoadClosure, localCount)

            (* The optimiser checks the size of a function and decides whether it can be inlined.
               However if we have expanded some other inlines inside the body it may now be too
               big.  In some cases we can get exponential blow-up.  We check here that the
               body is still small enough before allowing it to be used inline.
               The limit is set to 10 times the optimiser's limit because it seems that
               otherwise significant functions are not inlined. *)
            val stillInline =
                case isNowInline of
                    SmallInline =>
                        if evaluateInlining(cleanBody, List.length argTypes, maxInlineSize*10) <> TooBig
                        then SmallInline
                        else DontInline
                |   inl => inl

            val copiedLambda: lambdaForm =
                {
                    body          = cleanBody,
                    isInline      = isNowInline,
                    name          = name,
                    closure       = closureAfterOpt,
                    argTypes      = argTypes,
                    resultType    = resultType,
                    localCount    = localCount,
                    recUse        = []
                }

            (* The optimiser checks the size of a function and decides whether it can be inlined.
               However if we have expanded some other inlines inside the body it may now be too
               big.  In some cases we can get exponential blow-up.  We check here that the
               body is still small enough before allowing it to be used inline. *)
            val inlineCode =
                if stillInline <> DontInline
                then EnvSpecInlineFunction(copiedLambda, fn addr => (EnvGenLoad(List.nth(closureAfterOpt, addr)), EnvSpecNone))
                else EnvSpecNone
         in
            (
                copiedLambda,
                inlineCode
            )
        end

    and simpFunctionCall(function, argList, resultType, context as { reprocess, maxInlineSize, ...}, tailDecs) =
    let
        (* Function call - This may involve inlining the function. *)

        (* Get the function to be called and see if it is inline or
           a lambda expression. *)
        val (genFunct, decsFunct, specFunct) = simpSpecial(function, context, tailDecs)
        (* We have to make a special check here that we are not passing in the function
           we are trying to expand.  This could result in an infinitely recursive expansion.  It is only
           going to happen in very special circumstances such as a definition of the Y combinator.
           If we see that we don't attempt to expand inline.  It could be embedded in a tuple
           or the closure of a function as well as passed directly. See Tests/Succeed/Test132.ML *)
        val isRecursiveArg =
            case function of
                Extract extOrig =>
                    let
                        fun containsFunction(Extract thisArg, v) = (v orelse thisArg = extOrig, FOLD_DESCEND)
                        |   containsFunction(Lambda{closure, ...}, v) =
                                (* Only the closure, not the body *)
                                (foldl (fn (c, w) => foldtree containsFunction w (Extract c)) v closure, FOLD_DONT_DESCEND)
                        |   containsFunction(Eval _, v) = (v, FOLD_DONT_DESCEND) (* OK if it's called *)
                        |   containsFunction(_, v) = (v, FOLD_DESCEND)
                    in
                        List.exists(fn (c, _) => foldtree containsFunction false c) argList
                    end
            |   Constnt (cOrig, _) =>
                    if Address.isShort cOrig then false (* Functors are short constants and "always inline" *)
                    else
                    let
                        fun containsFunction(Constnt(thisC, _), v) = (v orelse wordEq(cOrig, thisC), FOLD_DESCEND)
                        |   containsFunction(Lambda{closure, ...}, v) =
                                (* Only the closure, not the body *)
                                (foldl (fn (c, w) => foldtree containsFunction w (Extract c)) v closure, FOLD_DONT_DESCEND)
                        |   containsFunction(Eval _, v) = (v, FOLD_DONT_DESCEND) (* OK if it's called *)
                        |   containsFunction(_, v) = (v, FOLD_DESCEND)
                    in
                       List.exists(fn (c, _) => foldtree containsFunction false c) argList 
                    end
            |   _ => false
    in
        case (specFunct, genFunct, isRecursiveArg) of
            (EnvSpecInlineFunction({body=lambdaBody, localCount, argTypes, ...}, functEnv), _, false) =>
            let
                val _ = List.length argTypes = List.length argList
                            orelse raise InternalError "simpFunctionCall: argument mismatch"
                val () = reprocess := true (* If we expand inline we have to reprocess *)
                and { nextAddress, reprocess, ...} = context

                (* Expand a function inline, either one marked explicitly to be inlined or one detected as "small". *)
                (* Calling inline proc or a lambda expression which is just called.
                   The function is replaced with a block containing declarations
                   of the parameters.  We need a new table here because the addresses
                   we use to index it are the addresses which are local to the function.
                   New addresses are created in the range of the surrounding function. *)
                val localVec = Array.array(localCount, NONE)

                local
                    fun processArgs([], bindings) = ([], bindings)
                    |   processArgs((arg, _)::args, bindings) =
                        let
                            val (thisArg, newBindings) = 
                                makeNewDecl(simpSpecial(arg, context, bindings), context)
                            val (otherArgs, resBindings) = processArgs(args, newBindings)
                        in
                            (thisArg::otherArgs, resBindings)
                        end
                    val (params, bindings) = processArgs(argList, decsFunct)
                    val paramVec = Vector.fromList params
                in
                    fun getParameter n = Vector.sub(paramVec, n)

                    (* Bindings necessary for the arguments *)
                    val copiedArgs = bindings
                end

                local
                    fun localOldAddr(LoadLocal addr) = valOf(Array.sub(localVec, addr))
                    |   localOldAddr(LoadArgument addr) = getParameter addr
                    |   localOldAddr(LoadClosure closureEntry) = functEnv closureEntry
                    |   localOldAddr LoadRecursive = raise InternalError "localOldAddr: LoadRecursive"

                    fun setTabForInline (index, v) = Array.update (localVec, index, SOME v)
                    val lambdaContext =
                    {
                        lookupAddr=localOldAddr, enterAddr=setTabForInline,
                        nextAddress=nextAddress, reprocess = reprocess,
                        maxInlineSize = maxInlineSize
                    }
                in
                    val (cGen, cDecs, cSpec) = simpSpecial(lambdaBody,lambdaContext, copiedArgs)
                end
            in
                (cGen, cDecs, cSpec)
            end

        |   (_, gen as Constnt _, _) => (* Not inlinable - constant function. *)
            let
                val copiedArgs = map (fn (arg, argType) => (simplify(arg, context), argType)) argList
                val evCopiedCode =
                    Eval {function = gen, argList = copiedArgs, resultType=resultType}
            in
                (evCopiedCode, decsFunct, EnvSpecNone)
            end

        |   (_, gen, _) => (* Anything else. *)
            let
                val copiedArgs = map (fn (arg, argType) => (simplify(arg, context), argType)) argList
                val evCopiedCode = 
                    Eval {function = gen, argList = copiedArgs, resultType=resultType}
            in
                (evCopiedCode, decsFunct, EnvSpecNone)
            end
    end
    
    (* Special processing for the current builtIn1 operations. *)
    (* Constant folding for built-ins.  These ought to be type-correct i.e. we should have
       tagged values in some cases and addresses in others.  However there may be run-time
       tests that would ensure type-correctness and we can't be sure that they will always
       be folded at compile-time.  e.g. we may have
        if isShort c then shortOp c else longOp c
       If c is a constant then we may try to fold both the shortOp and the longOp and one
       of these will be type-incorrect although never executed at run-time. *)

    and simpUnary(oper, arg1, context as { reprocess, ...}, tailDecs) =
    let
        val (genArg1, decArg1, specArg1) = simpSpecial(arg1, context, tailDecs)
    in
        case (oper, genArg1) of
            (NotBoolean, Constnt(v, _)) =>
            (
                reprocess := true;
                (if isShort v andalso toShort v = 0w0 then CodeTrue else CodeFalse, decArg1, EnvSpecNone)
            )

        |   (NotBoolean, genArg1) =>
            (
                (* NotBoolean:  This can be the result of using Bool.not but more usually occurs as a result
                   of other code.  We don't have TestNotEqual or IsAddress so both of these use NotBoolean
                   with TestEqual and IsTagged.  Also we can insert a NotBoolean as a result of a Cond.
                   We try to eliminate not(not a) and to push other NotBooleans down to a point where
                   a boolean is tested. *)
                case specArg1 of
                    EnvSpecUnary(NotBoolean, originalArg) =>
                    (
                        (* not(not a) - Eliminate. *)
                        reprocess := true;
                        (originalArg, decArg1, EnvSpecNone)
                    )
                 |  _ =>
                    (* Otherwise pass this on.  It is also extracted in a Cond. *)
                    (Unary{oper=NotBoolean, arg1=genArg1}, decArg1, EnvSpecUnary(NotBoolean, genArg1))
            )

        |   (IsTaggedValue, Constnt(v, _)) =>
            (
                reprocess := true;
                (if isShort v then CodeTrue else CodeFalse, decArg1, EnvSpecNone)
            )

        |   (IsTaggedValue, genArg1) =>
            (
                (* We use this to test for nil values and if we have constructed a record
                   (or possibly a function) it can't be null. *)
                case specArg1 of
                    EnvSpecTuple _ => (CodeFalse, decArg1, EnvSpecNone) before reprocess := true
                |   EnvSpecInlineFunction _ =>
                        (CodeFalse, decArg1, EnvSpecNone) before reprocess := true
                |   _ => (Unary{oper=oper, arg1=genArg1}, decArg1, EnvSpecNone)
            )
        |   (MemoryCellLength, Constnt(v, _)) =>
            (
                reprocess := true;
                (if isShort v then CodeZero else Constnt(toMachineWord(Address.length(toAddress v)), []), decArg1, EnvSpecNone)
            )

        |   (MemoryCellFlags, Constnt(v, _)) =>
            (
                reprocess := true;
                (if isShort v then CodeZero else Constnt(toMachineWord(Address.flags(toAddress v)), []), decArg1, EnvSpecNone)
            )

        |   (LongWordToTagged, Constnt(v, _)) =>
            (
                reprocess := true;
                (Constnt(toMachineWord(Word.fromLargeWord(RunCall.unsafeCast v)), []), decArg1, EnvSpecNone)
            )

        |   (LongWordToTagged, genArg1) =>
            (
                (* If we apply LongWordToTagged to an argument we have created with UnsignedToLongWord
                   we can return the original argument. *)
                case specArg1 of
                    EnvSpecUnary(UnsignedToLongWord, originalArg) =>
                    (
                        reprocess := true;
                        (originalArg, decArg1, EnvSpecNone)
                    )
                 |  _ => (Unary{oper=LongWordToTagged, arg1=genArg1}, decArg1, EnvSpecNone)
            )

        |   (SignedToLongWord, Constnt(v, _)) =>
            (
                reprocess := true;
                (Constnt(toMachineWord(Word.toLargeWordX(RunCall.unsafeCast v)), []), decArg1, EnvSpecNone)
            )

        |   (UnsignedToLongWord, Constnt(v, _)) =>
            (
                reprocess := true;
                (Constnt(toMachineWord(Word.toLargeWord(RunCall.unsafeCast v)), []), decArg1, EnvSpecNone)
            )

        |   (UnsignedToLongWord, genArg1) =>
                (* Add the operation as the special entry.  It can then be recognised by LongWordToTagged. *)
                (Unary{oper=oper, arg1=genArg1}, decArg1, EnvSpecUnary(UnsignedToLongWord, genArg1))

        |   (Log2Word, Constnt(v, _)) =>
            let
                val () = reprocess := true
                (* IntInf.log2 raises an exception if the argument is zero or negative.
                   Log2Word is currently only used for IntInf.log2 but just in case it is
                   ever used for anything else this covers it.  The implementations of
                   LogWord on X86 and ARM both return ~1 for zero argument since the
                   tag bit will be set even for zero. *)
                fun getLog2(0w0, n) = n
                |   getLog2(v, n) = getLog2(Word.>>(v, 0w1), n+1)
                val result =
                    if isShort v
                    then getLog2(toShort v, ~1)
                    else 0 (* Ensure this case is covered. *)
            in
                (Constnt(toMachineWord result, []), decArg1, EnvSpecNone)
            end

        |   _ => (Unary{oper=oper, arg1=genArg1}, decArg1, EnvSpecNone)
    end

    and simpBinary(oper, arg1, arg2, context as {reprocess, ...}, tailDecs) =
    let
        val (genArg1, decArg1, specArg1) = simpSpecial(arg1, context, tailDecs)
        val (genArg2, decArgs, specArg2) = simpSpecial(arg2, context, decArg1)
    in
        case (oper, genArg1, genArg2) of
            (WordComparison{test, isSigned}, Constnt(v1, _), Constnt(v2, _)) =>
            if not(isShort v1) orelse not(isShort v2) (* E.g. arbitrary precision on unreachable path. *)
            then (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)
            else
            let
                val () = reprocess := true
                val testResult =
                    case (test, isSigned) of
                        (* TestEqual can be applied to addresses. *)
                        (TestEqual, _)              => toShort v1 = toShort v2
                    |   (TestLess, false)           => toShort v1 < toShort v2
                    |   (TestLessEqual, false)      => toShort v1 <= toShort v2
                    |   (TestGreater, false)        => toShort v1 > toShort v2
                    |   (TestGreaterEqual, false)   => toShort v1 >= toShort v2
                    |   (TestLess, true)            => toFix v1 < toFix v2
                    |   (TestLessEqual, true)       => toFix v1 <= toFix v2
                    |   (TestGreater, true)         => toFix v1 > toFix v2
                    |   (TestGreaterEqual, true)    => toFix v1 >= toFix v2
                    |   (TestUnordered, _)          => raise InternalError "WordComparison: TestUnordered"
            in
                (if testResult then CodeTrue else CodeFalse, decArgs, EnvSpecNone)
            end

        |   (PointerEq, Constnt(v1, _), Constnt(v2, _)) =>
            (
                reprocess := true;
                (if RunCall.pointerEq(v1, v2) then CodeTrue else CodeFalse, decArgs, EnvSpecNone)
            )

        |   (PointerEq, Constnt(v1, _), genArg2) =>
            (
                (* The pattern match test for nil generates a pointer equality test. That's different
                   from the test for cons (::) which uses a test for short values.  Which test gets
                   generated depends on which comes first. *)
                case (specArg2, isShort v1) of
                    (EnvSpecTuple _, true) => (CodeFalse, decArgs, EnvSpecNone) before reprocess := true
                |   _ => (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)
            )

        |   (PointerEq, genArg1, Constnt(v2, _)) =>
            (
                (* This case probably never arises from pattern matching but include it anyway. *)
                case (specArg1, isShort v2) of
                    (EnvSpecTuple _, true) => (CodeFalse, decArgs, EnvSpecNone) before reprocess := true
                |   _ => (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)
            )

            (* A general equality test against a short constant can be converted into pointer equality since they will only
               be equal if they are the same short value.  This is reprocessed so the tests for PointerEq above will be
               applied if appropriate. *)
        |   (GeneralEquality, Constnt(v1, _), genArg2) =>
                if isShort v1
                then (Binary{oper=PointerEq, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone) before reprocess := true
                else (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)

        |   (GeneralEquality, genArg1, Constnt(v2, _)) =>
                if isShort v2
                then (Binary{oper=PointerEq, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone) before reprocess := true
                else (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)

        |   (FixedPrecisionArith arithOp, Constnt(v1, _), Constnt(v2, _)) =>
            if not(isShort v1) orelse not(isShort v2)
            then (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)
            else
            let
                val () = reprocess := true
                val v1S = toFix v1
                and v2S = toFix v2
                fun asConstnt v = Constnt(toMachineWord v, [])
                val raiseOverflow = Raise(Constnt(toMachineWord Overflow, []))
                val raiseDiv = Raise(Constnt(toMachineWord Div, [])) (* ?? There's usually an explicit test. *)
                val resultCode =
                    case arithOp of
                        ArithAdd => (asConstnt(v1S+v2S) handle Overflow => raiseOverflow)
                    |   ArithSub => (asConstnt(v1S-v2S) handle Overflow => raiseOverflow)
                    |   ArithMult => (asConstnt(v1S*v2S) handle Overflow => raiseOverflow)
                    |   ArithQuot => (asConstnt(FixedInt.quot(v1S,v2S)) handle Overflow => raiseOverflow | Div => raiseDiv)
                    |   ArithRem => (asConstnt(FixedInt.rem(v1S,v2S)) handle Overflow => raiseOverflow | Div => raiseDiv)
                    |   ArithDiv => (asConstnt(FixedInt.div(v1S,v2S)) handle Overflow => raiseOverflow | Div => raiseDiv)
                    |   ArithMod => (asConstnt(FixedInt.mod(v1S,v2S)) handle Overflow => raiseOverflow | Div => raiseDiv)
            in
                (resultCode, decArgs, EnvSpecNone)
            end

            (* Addition and subtraction of zero.  These can arise as a result of
               inline expansion of more general functions. *)
        |   (FixedPrecisionArith ArithAdd, arg1, Constnt(v2, _)) =>
            if isShort v2 andalso toShort v2 = 0w0
            then (arg1, decArgs, EnvSpecNone)
            else (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)

        |   (FixedPrecisionArith ArithAdd, Constnt(v1, _), arg2) =>
            if isShort v1 andalso toShort v1 = 0w0
            then (arg2, decArgs, EnvSpecNone)
            else (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)

        |   (FixedPrecisionArith ArithSub, arg1, Constnt(v2, _)) =>
            if isShort v2 andalso toShort v2 = 0w0
            then (arg1, decArgs, EnvSpecNone)
            else (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)

        |   (WordArith arithOp, Constnt(v1, _), Constnt(v2, _)) =>
            if not(isShort v1) orelse not(isShort v2)
            then (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)
            else
            let
                val () = reprocess := true
                val v1S = toShort v1
                and v2S = toShort v2
                fun asConstnt v = Constnt(toMachineWord v, [])
                val resultCode =
                    case arithOp of
                        ArithAdd => asConstnt(v1S+v2S)
                    |   ArithSub => asConstnt(v1S-v2S)
                    |   ArithMult => asConstnt(v1S*v2S)
                    |   ArithQuot => raise InternalError "WordArith: ArithQuot"
                    |   ArithRem => raise InternalError "WordArith: ArithRem"
                    |   ArithDiv => asConstnt(v1S div v2S)
                    |   ArithMod => asConstnt(v1S mod v2S)
            in
               (resultCode, decArgs, EnvSpecNone)
            end

        |   (WordArith ArithAdd, arg1, Constnt(v2, _)) =>
            if isShort v2 andalso toShort v2 = 0w0
            then (arg1, decArgs, EnvSpecNone)
            else (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)

        |   (WordArith ArithAdd, Constnt(v1, _), arg2) =>
            if isShort v1 andalso toShort v1 = 0w0
            then (arg2, decArgs, EnvSpecNone)
            else (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)

        |   (WordArith ArithSub, arg1, Constnt(v2, _)) =>
            if isShort v2 andalso toShort v2 = 0w0
            then (arg1, decArgs, EnvSpecNone)
            else (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)

        |   (WordLogical logOp, Constnt(v1, _), Constnt(v2, _)) =>
            if not(isShort v1) orelse not(isShort v2)
            then (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)
            else
            let
                val () = reprocess := true
                val v1S = toShort v1
                and v2S = toShort v2
                fun asConstnt v = Constnt(toMachineWord v, [])
                val resultCode =
                    case logOp of
                        LogicalAnd => asConstnt(Word.andb(v1S,v2S))
                    |   LogicalOr => asConstnt(Word.orb(v1S,v2S))
                    |   LogicalXor => asConstnt(Word.xorb(v1S,v2S))
            in
               (resultCode, decArgs, EnvSpecNone)
            end

        |   (WordLogical logop, arg1, Constnt(v2, _)) =>
            (* Return the zero if we are anding with zero otherwise the original arg *)
            if isShort v2 andalso toShort v2 = 0w0
            then (case logop of LogicalAnd => CodeZero | _ => arg1, decArgs, EnvSpecNone)
            else (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)

        |   (WordLogical logop, Constnt(v1, _), arg2) =>
            if isShort v1 andalso toShort v1 = 0w0
            then (case logop of LogicalAnd => CodeZero | _ => arg2, decArgs, EnvSpecNone)
            else (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)

        |   (WordShift shiftOp, Constnt(v1, _), Constnt(v2, _)) =>
            if not(isShort v1) orelse not(isShort v2)
            then (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)
            else
            let
                val () = reprocess := true
                val v1S = toShort v1
                and v2S = toShort v2
                fun asConstnt v = Constnt(toMachineWord v, [])
                val resultCode =
                    case shiftOp of
                        ShiftLeft => asConstnt(Word.<<(v1S,v2S))
                    |   ShiftRightLogical => asConstnt(Word.>>(v1S,v2S))
                    |   ShiftRightArithmetic => asConstnt(Word.~>>(v1S,v2S))
            in
               (resultCode, decArgs, EnvSpecNone)
            end

        |   _ => (Binary{oper=oper, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)
    end

    (* Arbitrary precision operations.  This is a sort of mixture of a built-in and a conditional. *)
    and simpArbitraryCompare(TestEqual, _, _, _, _, _, _) =
        (* We no longer generate this for equality.  General equality for arbitrary precision
           uses a combination of PointerEq and byte comparison. *)
            raise InternalError "simpArbitraryCompare: TestEqual"

    |   simpArbitraryCompare(test, shortCond, arg1, arg2, longCall, context as {reprocess, ...}, tailDecs) =
    let
        val (genCond, decCond, _ (*specArg1*)) = simpSpecial(shortCond, context, tailDecs)
        val (genArg1, decArg1, _ (*specArg1*)) = simpSpecial(arg1, context, decCond)
        val (genArg2, decArgs, _ (*specArg2*)) = simpSpecial(arg2, context, decArg1)
        val posFlags = Address.F_bytes and negFlags = Word8.orb(Address.F_bytes, Address.F_negative)
    in
        (* Fold any constant/constant operations but more importantly, if we
           have variable/constant operations where the constant is short we
           can avoid using the full arbitrary precision call by just looking
           at the sign bit. *)
        case (genCond, genArg1, genArg2) of
            (_, Constnt(v1, _), Constnt(v2, _)) =>
            let
                val a1: LargeInt.int = RunCall.unsafeCast v1
                and a2: LargeInt.int = RunCall.unsafeCast v2
                val testResult =
                    case test of
                        TestLess            => a1 < a2
                    |   TestGreater         => a1 > a2
                    |   TestLessEqual       => a1 <= a2
                    |   TestGreaterEqual    => a1 >= a2
                    |   _ => raise InternalError "simpArbitraryCompare: Unimplemented function"
            in
                (if testResult then CodeTrue else CodeFalse, decArgs, EnvSpecNone)
            end

        |   (Constnt(c1, _),  _, _) =>
            (* The condition is "isShort X andalso isShort Y".  This will have been reduced
               to a constant false or true if either (a) either argument is long or
               (b) both arguments are short.*)
                if isShort c1 andalso toShort c1 = 0w0
                then (* One argument is definitely long - generate the long form. *)
                    (simplify(longCall, context), decArgs, EnvSpecNone)
                else (* Both arguments are short.  That should mean they're constants. *)
                    (Binary{oper=WordComparison{test=test, isSigned=true}, arg1=genArg1, arg2=genArg2}, decArgs, EnvSpecNone)
                         before reprocess := true

        |   (_, genArg1, cArg2 as Constnt _) =>
            let (* The constant must be short otherwise the test would be false. *)
                val isNeg =
                    case test of
                        TestLess => true
                    |   TestLessEqual => true
                    |   _ => false
                (* Translate i < c into
                        if isShort i then toShort i < c else isNegative i *)
                val newCode =
                    Cond(Unary{oper=BuiltIns.IsTaggedValue, arg1=genArg1},
                        Binary { oper = BuiltIns.WordComparison{test=test, isSigned=true}, arg1 = genArg1, arg2 = cArg2 },
                        Binary { oper = BuiltIns.WordComparison{test=TestEqual, isSigned=false},
                                arg1=Unary { oper = MemoryCellFlags, arg1=genArg1 },
                                arg2=Constnt(toMachineWord(if isNeg then negFlags else posFlags), [])}
                        )
            in
                (newCode, decArgs, EnvSpecNone)
            end
        |   (_, cArg1 as Constnt _, genArg2) =>
            let
                (* We're testing c < i  so the test is
                   if isShort i then c < toShort i else isPositive i *)
                val isPos =
                    case test of
                        TestLess => true
                    |   TestLessEqual => true
                    |   _ => false
                val newCode =
                    Cond(Unary{oper=BuiltIns.IsTaggedValue, arg1=genArg2},
                        Binary { oper = BuiltIns.WordComparison{test=test, isSigned=true}, arg1 = cArg1, arg2 = genArg2 },
                        Binary { oper = BuiltIns.WordComparison{test=TestEqual, isSigned=false},
                                arg1=Unary { oper = MemoryCellFlags, arg1=genArg2 },
                                arg2=Constnt(toMachineWord(if isPos then posFlags else negFlags), [])}
                        )
            in
                (newCode, decArgs, EnvSpecNone)
            end
        |   _ => (Arbitrary{oper=ArbCompare test, shortCond=genCond, arg1=genArg1, arg2=genArg2, longCall=simplify(longCall, context)}, decArgs, EnvSpecNone)
    end
    
    and simpArbitraryArith(arith, shortCond, arg1, arg2, longCall, context, tailDecs) =
    let
        (* arg1 and arg2 are the arguments.  shortCond is the condition that must be
           satisfied in order to use the short precision operation i.e. each argument
           must be short. *)
        val (genCond, decCond, _ (*specArg1*)) = simpSpecial(shortCond, context, tailDecs)
        val (genArg1, decArg1, _ (*specArg1*)) = simpSpecial(arg1, context, decCond)
        val (genArg2, decArgs, _ (*specArg2*)) = simpSpecial(arg2, context, decArg1)
    in
        case (genArg1, genArg2, genCond) of
            (Constnt(v1, _), Constnt(v2, _), _) =>
            let
                val a1: LargeInt.int = RunCall.unsafeCast v1
                and a2: LargeInt.int = RunCall.unsafeCast v2
                (*val _ = print ("Fold arbitrary precision: " ^ PolyML.makestring(arith, a1, a2) ^ "\n")*)
            in
                case arith of
                    ArithAdd => (Constnt(toMachineWord(a1+a2), []), decArgs, EnvSpecNone)
                |   ArithSub => (Constnt(toMachineWord(a1-a2), []), decArgs, EnvSpecNone)
                |   ArithMult => (Constnt(toMachineWord(a1*a2), []), decArgs, EnvSpecNone)
                |   _ => raise InternalError "simpArbitraryArith: Unimplemented function"
            end
            
        |   (_, _, Constnt(c1, _)) =>
            if isShort c1 andalso toShort c1 = 0w0
            then (* One argument is definitely long - generate the long form. *)
                (simplify(longCall, context), decArgs, EnvSpecNone)
            else
                (Arbitrary{oper=ArbArith arith, shortCond=genCond, arg1=genArg1, arg2=genArg2, longCall=simplify(longCall, context)}, decArgs, EnvSpecNone)

        |   _ => (Arbitrary{oper=ArbArith arith, shortCond=genCond, arg1=genArg1, arg2=genArg2, longCall=simplify(longCall, context)}, decArgs, EnvSpecNone)
    end

    and simpAllocateWordMemory(numWords, flags, initial, context, tailDecs) =
    let
        val (genArg1, decArg1, _ (*specArg1*)) = simpSpecial(numWords, context, tailDecs)
        val (genArg2, decArg2, _ (*specArg2*)) = simpSpecial(flags, context, decArg1)
        val (genArg3, decArg3, _ (*specArg3*)) = simpSpecial(initial, context, decArg2)
    in 
        (AllocateWordMemory{numWords=genArg1, flags=genArg2, initial=genArg3}, decArg3, EnvSpecNone)
    end

    (* Loads, stores and block operations use address values.  The index value is initially
       an arbitrary code tree but we can recognise common cases of constant index values
       or where a constant has been added to the index.
       TODO: If these are C memory moves we can also look at the base address.
       The base address for C memory operations is a LargeWord.word value i.e.
       the address is contained in a box.  The base addresses for ML memory
       moves is an ML address i.e. unboxed. *)
    and simpAddress({base, index=NONE, offset}, _, context) =
        let
            val (genBase, decBase, _ (*specBase*)) = simpSpecial(base, context, RevList[])
        in
            ({base=genBase, index=NONE, offset=offset}, decBase)
        end

    |   simpAddress({base, index=SOME index, offset: int}, (multiplier: int, isSigned), context) =
        let
            val (genBase, RevList decBase, _) = simpSpecial(base, context, RevList[])
            val (genIndex, RevList decIndex, _ (* specIndex *)) = simpSpecial(index, context, RevList[])
            val (newIndex, newOffset) =
                case genIndex of
                    Constnt(indexOffset, _) =>
                        (* Convert small, positive offsets but leave large values as
                           indexes.  We could have silly index values here which will
                           never be executed because of a range check but should still compile. *)
                        if isShort indexOffset
                        then
                        let
                            val indexOffsetW = toShort indexOffset
                        in
                            if indexOffsetW < 0w1000 orelse isSigned andalso indexOffsetW > ~ 0w1000
                            then (NONE, offset + (if isSigned then Word.toIntX else Word.toInt)indexOffsetW * multiplier)
                            else (SOME genIndex, offset)
                        end
                        else (SOME genIndex, offset)
                |   _ => (SOME genIndex, offset)
        in
            ({base=genBase, index=newIndex, offset=newOffset}, RevList(decIndex @ decBase))
        end


(*
    (* A built-in function.  We can call certain built-ins immediately if
       the arguments are constants.  *)
    and simpBuiltIn(rtsCallNo, argList, context as { reprocess, ...}) =
    let
        val copiedArgs = map (fn arg => simpSpecial(arg, context)) argList
        open RuntimeCalls
        (* When checking for a constant we need to check that there are no bindings.
           They could have side-effects. *)
        fun isAConstant(Constnt _, [], _) = true
        |   isAConstant _ = false
    in
        (* If the function is an RTS call that is safe to evaluate immediately and all the
           arguments are constants evaluate it now. *)
        if earlyRtsCall rtsCallNo andalso List.all isAConstant copiedArgs
        then
        let
            val () = reprocess := true
            exception Interrupt = Thread.Thread.Interrupt

            (* Turn the arguments into a vector.  *)
            val argVector =
                case makeConstVal(mkTuple(List.map specialToGeneral copiedArgs)) of
                    Constnt(w, _) => w
                |   _ => raise InternalError "makeConstVal: Not constant"

            (* Call the function.  If it raises an exception (e.g. divide
               by zero) generate code to raise the exception at run-time.
               We don't do that for Interrupt which we assume only arises
               by user interaction and not as a result of executing the
               code so we reraise that exception immediately. *)
            val ioOp : int -> machineWord =
                RunCall.run_call1 RuntimeCalls.POLY_SYS_io_operation
            (* We need callcode_tupled here because we pass the arguments as
               a tuple but the RTS functions we're calling expect arguments in
               registers or on the stack. *)
            val call: (address * machineWord) -> machineWord =
                RunCall.run_call1 RuntimeCalls.POLY_SYS_callcode_tupled
            val code =
                Constnt (call(toAddress(ioOp rtsCallNo), argVector), [])
                    handle exn as Interrupt => raise exn (* Must not handle this *)
                    | exn => Raise (Constnt(toMachineWord exn, []))
        in
            (code, [], EnvSpecNone)
        end
            (* We can optimise certain built-ins in combination with others.
               If we have POLY_SYS_unsigned_to_longword combined with POLY_SYS_longword_to_tagged
               we can eliminate both.  This can occur in cases such as Word.fromLargeWord o Word8.toLargeWord.
               If we have POLY_SYS_cmem_load_X functions where the address is formed by adding
               a constant to an address we can move the addend into the load instruction. *)
            (* TODO: Could we also have POLY_SYS_signed_to_longword here? *)
        else if rtsCallNo = POLY_SYS_longword_to_tagged andalso
                (case copiedArgs of [(_, _, EnvSpecBuiltIn(r, _))] => r = POLY_SYS_unsigned_to_longword | _ => false)
        then
        let
            val arg = (* Get the argument of the argument. *)
                case copiedArgs of
                    [(_, _, EnvSpecBuiltIn(_, [arg]))] => arg
                |   _ => raise Bind
        in
            (arg, [], EnvSpecNone)
        end
        else if (rtsCallNo = POLY_SYS_cmem_load_8 orelse rtsCallNo = POLY_SYS_cmem_load_16 orelse
                 rtsCallNo = POLY_SYS_cmem_load_32 orelse rtsCallNo = POLY_SYS_cmem_load_64 orelse
                 rtsCallNo = POLY_SYS_cmem_store_8 orelse rtsCallNo = POLY_SYS_cmem_store_16 orelse
                 rtsCallNo = POLY_SYS_cmem_store_32 orelse rtsCallNo = POLY_SYS_cmem_store_64) andalso
                (* Check if the first argument is an addition.  The second should be a constant.
                   If the addend is a constant it will be a large integer i.e. the address of a
                   byte segment. *)
                let
                    (* Check that we have a valid value to add to a large word.
                       The cmem_load/store values sign extend their arguments so we
                       use toLargeWordX here. *)
                    fun isAcceptableOffset c =
                        if isShort c (* Shouldn't occur. *) then false
                        else
                        let
                            val l: LargeWord.word = RunCall.unsafeCast c
                        in
                            Word.toLargeWordX(Word.fromLargeWord l) = l
                        end
                in
                    case copiedArgs of (_, _, EnvSpecBuiltIn(r, args)) :: (Constnt _, _, _) :: _ =>
                        r = POLY_SYS_plus_longword andalso
                            (case args of
                                (* If they were both constants we'd have folded them. *)
                                [Constnt(c, _), _] => isAcceptableOffset c
                            |   [_, Constnt(c, _)] => isAcceptableOffset c
                            | _ => false)
                        | _ => false
                end
        then
        let
            (* We have a load or store with an added constant. *)
            val (base, offset) =
                case copiedArgs of
                    (_, _, EnvSpecBuiltIn(_, [Constnt(offset, _), base])) :: (Constnt(existing, _), _, _) :: _ =>
                        (base, Word.fromLargeWord(RunCall.unsafeCast offset) + toShort existing)
                |   (_, _, EnvSpecBuiltIn(_, [base, Constnt(offset, _)])) :: (Constnt(existing, _), _, _) :: _ =>
                        (base, Word.fromLargeWord(RunCall.unsafeCast offset) + toShort existing)
                |   _ => raise Bind
            val newDecs = List.map(fn h => makeNewDecl(h, context)) copiedArgs
            val genArgs = List.map(fn ((g, _), _) => envGeneralToCodetree g) newDecs
            val preDecs = List.foldr (op @) [] (List.map #2 newDecs)
            val gen = BuiltIn(rtsCallNo, base :: Constnt(toMachineWord offset, []) :: List.drop(genArgs, 2))
        in
            (gen, preDecs, EnvSpecNone)
        end
        else
        let
            (* Create bindings for the arguments.  This ensures that any side-effects in the
               evaluation of the arguments are performed in the correct order even if the
               application of the built-in itself is applicative.  The new arguments are
               either loads or constants which are applicative. *)
            val newDecs = List.map(fn h => makeNewDecl(h, context)) copiedArgs
            val genArgs = List.map(fn ((g, _), _) => envGeneralToCodetree g) newDecs
            val preDecs = List.foldr (op @) [] (List.map #2 newDecs)
            val gen = BuiltIn(rtsCallNo, genArgs)
            val spec =
                if reorderable gen
                then EnvSpecBuiltIn(rtsCallNo, genArgs)
                else EnvSpecNone
        in
            (gen, preDecs, spec)
        end
    end
*)
    and simpIfThenElse(condTest, condThen, condElse, context, tailDecs) =
    (* If-then-else.  The main simplification is if we have constants in the
       test or in both the arms. *)
    let
        val word0 = toMachineWord 0
        val word1 = toMachineWord 1
  
        val False = word0
        val True  = word1
    in
        case simpSpecial(condTest, context, tailDecs) of
            (* If the test is a constant we can return the appropriate arm and
               ignore the other.  *)
            (Constnt(testResult, _), bindings, _) =>
                let
                    val arm = 
                        if wordEq (testResult, False) (* false - return else-part *)
                        then condElse (* if false then x else y == y *)
                        (* if true then x else y == x *)
                        else condThen
                in
                    simpSpecial(arm, context, bindings)
                end
        |   (testGen, testbindings as RevList testBList, testSpec) =>
            let
                fun mkNot (Unary{oper=BuiltIns.NotBoolean, arg1}) = arg1
                |   mkNot arg = Unary{oper=BuiltIns.NotBoolean, arg1=arg}

                (* If the test involves a variable that was created with a NOT it's
                   better to move it in here. *)
                val testCond =
                    case testSpec of
                        EnvSpecUnary(BuiltIns.NotBoolean, arg1) => mkNot arg1
                    |   _ => testGen
            in
                case (simpSpecial(condThen, context, RevList[]), simpSpecial(condElse, context, RevList[])) of
                    ((thenConst as Constnt(thenVal, _), RevList [], _), (elseConst as Constnt(elseVal, _), RevList [], _)) =>
                        (* Both arms return constants.  This situation can arise in
                           situations where we have andalso/orelse where the second
                           "argument" has been reduced to a constant. *)
                        if wordEq (thenVal, elseVal)
                        then (* If the test has a side-effect we have to do it otherwise we can remove
                                it.  If we're in a nested andalso/orelse that may mean we can simplify
                                the next level out. *)
                            (thenConst (* or elseConst *),
                             if sideEffectFree testCond then testbindings else RevList(NullBinding testCond :: testBList),
                             EnvSpecNone)
              
                        (* if x then true else false == x *)
                        else if wordEq (thenVal, True) andalso wordEq (elseVal, False)
                        then (testCond, testbindings, EnvSpecNone)
          
                        (* if x then false else true == not x  *)
                        else if wordEq (thenVal, False) andalso wordEq (elseVal, True)
                        then (mkNot testCond, testbindings, EnvSpecNone)
          
                        else (* can't optimise *) (Cond (testCond, thenConst, elseConst), testbindings, EnvSpecNone)

                        (* Rewrite "if x then raise y else z" into "(if x then raise y else (); z)"
                           The advantage is that any tuples in z are lifted outside the "if". *)
                |   (thenPart as (Raise _, _:revlist, _), (elsePart, RevList elseBindings, elseSpec)) =>
                        (* then-part raises an exception *)
                        (elsePart, RevList(elseBindings @ NullBinding(Cond (testCond, specialToGeneral thenPart, CodeZero)) :: testBList), elseSpec)

                |   ((thenPart, RevList thenBindings, thenSpec), elsePart as (Raise _, _, _)) =>
                        (* else part raises an exception *)
                        (thenPart, RevList(thenBindings @ NullBinding(Cond (testCond, CodeZero, specialToGeneral elsePart)) :: testBList), thenSpec)

                |   (thenPart, elsePart) => (Cond (testCond, specialToGeneral thenPart, specialToGeneral elsePart), testbindings, EnvSpecNone)
            end
    end

    (* Tuple construction.  Tuples are also used for datatypes and structures (i.e. modules) *)
    and simpTuple(entries, isVariant, context, tailDecs) =
     (* The main reason for optimising record constructions is that they
        appear as tuples in ML. We try to ensure that loads from locally
        created tuples do not involve indirecting from the tuple but can
        get the value which was put into the tuple directly. If that is
        successful we may find that the tuple is never used directly so
        the use-count mechanism will ensure it is never created. *)
    let
        val tupleSize = List.length entries
        (* The record construction is treated as a block of local
           declarations so that any expressions which might have side-effects
           are done exactly once. *)
        (* We thread the bindings through here to avoid having to append the result. *)
        fun processFields([], bindings) = ([], bindings)
        |   processFields(field::fields, bindings) =
            let
                val (thisField, newBindings) = 
                    makeNewDecl(simpSpecial(field, context, bindings), context)
                val (otherFields, resBindings) = processFields(fields, newBindings)
            in
                (thisField::otherFields, resBindings)
            end
        val (fieldEntries, allBindings) = processFields(entries, tailDecs)

        (* Make sure we include any inline code in the result.  If this tuple is
           being "exported" we will lose the "special" part. *)
        fun envResToCodetree(EnvGenLoad(ext), _) = Extract ext
        |   envResToCodetree(EnvGenConst(w, p), s) = Constnt(w, setInline s p)

        val generalFields = List.map envResToCodetree fieldEntries

        val genRec =
            if List.all isConstnt generalFields
            then makeConstVal(Tuple{ fields = generalFields, isVariant = isVariant })
            else Tuple{ fields = generalFields, isVariant = isVariant }

        (* Get the field from the tuple if possible.  If it's a variant, though,
           we may try to get an invalid field.  See Tests/Succeed/Test167. *)
        fun getField addr =
            if addr < tupleSize
            then List.nth(fieldEntries, addr)
            else if isVariant
            then (EnvGenConst(toMachineWord 0, []), EnvSpecNone)
            else raise InternalError "getField - invalid index"

        val specRec = EnvSpecTuple(tupleSize, getField)
    in
        (genRec, allBindings, specRec)
    end

    and simpFieldSelect(base, offset, indKind, context, tailDecs) =
    let
        val (genSource, decSource, specSource) = simpSpecial(base, context, tailDecs)
    in
        (* Try to do the selection now if possible. *)
        case specSource of
            EnvSpecTuple(_, recEnv) =>
            let
                (* The "special" entry we've found is a tuple.  That means that
                   we are taking a field from a tuple we made earlier and so we
                   should be able to get the original code we used when we made
                   the tuple.  That might mean the tuple is never used and
                   we can optimise away the construction of it completely. *)
                val (newGen, newSpec) = recEnv offset
            in
                (envGeneralToCodetree newGen, decSource, newSpec)
            end
                   
        |   _ => (* No special case possible. If the tuple is a constant mkInd/mkVarField
                    will do the selection immediately. *)
            let
                val genSelect =
                    case indKind of
                        IndTuple => mkInd(offset, genSource)
                    |   IndVariant => mkVarField(offset, genSource)
                    |   IndContainer => mkIndContainer(offset, genSource)
            in
                (genSelect, decSource, EnvSpecNone)
            end
    end

    (* Process a SetContainer.  Unlike the other simpXXX functions this is called
       after the arguments have been processed.  We try to push the SetContainer
       to the leaves of the expression.  This is particularly important with tail-recursive
       functions that return tuples.  Without this the function will lose tail-recursion
       since each recursion will be followed by code to copy the result back to the
       previous container. *)
    and simpPostSetContainer(container, Tuple{fields, ...}, RevList tupleDecs, filter) =
        let
            (* Apply the filter now. *)
            fun select(n, hd::tl) =
                if n >= BoolVector.length filter
                then []
                else if BoolVector.sub(filter, n) then hd :: select(n+1, tl) else select(n+1, tl)
            |   select(_, []) = []
            val selected = select(0, fields)
            (* Frequently we will have produced an indirection from the same base.  These
               will all be bindings so we have to reverse the process. *)

            fun findOriginal a =
                List.find(fn Declar{addr, ...} => addr = a | _ => false) tupleDecs

            fun checkFields(last, Extract(LoadLocal a) :: tl) =
                (
                    case findOriginal a of
                        SOME(Declar{value=Indirect{base=Extract ext, indKind=IndContainer, offset, ...}, ...}) =>
                        (
                            case last of
                                NONE => checkFields(SOME(ext, [offset]), tl)
                            |   SOME(lastExt, offsets) =>
                                    (* It has to be the same base and with increasing offsets
                                       (no reordering). *)
                                    if lastExt = ext andalso offset > hd offsets
                                    then checkFields(SOME(ext, offset :: offsets), tl)
                                    else NONE
                        )
                    |   _ => NONE
                )
            |   checkFields(_, _ :: _) = NONE
            |   checkFields(last, []) = last

            fun fieldsToFilter fields =
            let
                val maxDest = List.foldl Int.max ~1 fields
                val filterArray = BoolArray.array(maxDest+1, false)
                val _ = List.app(fn n => BoolArray.update(filterArray, n, true)) fields
            in
                BoolArray.vector filterArray
            end
        in
            case checkFields(NONE, selected) of
                SOME (ext, fields) => (* It may be a container. *)
                    let
                        val filter = fieldsToFilter fields
                    in
                        case ext of
                            LoadLocal localAddr =>
                            let
                                (* Is this a container?  If it is and we're copying all of it we can
                                   replace the inner container with a binding to the outer.
                                   We have to be careful because it is possible that we may create
                                   and set the inner container, then have some bindings that do some
                                   side-effects with the inner container before then copying it to
                                   the outer container.  For simplicity and to maintain the condition
                                   that the container is set in the tails we only merge the containers
                                   if it's at the end (after any "filtering"). *)
                                val allSet = BoolVector.foldl (fn (a, t) => a andalso t) true filter

                                fun findContainer [] = NONE
                                |   findContainer (Declar{value, ...} :: tl) =
                                        if sideEffectFree value then findContainer tl else NONE
                                |   findContainer (Container{addr, size, setter, ...} :: tl) =
                                        if localAddr = addr andalso size = BoolVector.length filter andalso allSet
                                        then SOME (setter, tl)
                                        else NONE
                                |   findContainer _ = NONE
                            in
                                case findContainer tupleDecs of
                                    SOME (setter, decs) =>
                                        (* Put in a binding for the inner container address so the
                                           setter will set the outer container.
                                           For this to work all loads from the stack must use native word length. *)
                                        mkEnv(List.rev(Declar{addr=localAddr, value=container, use=[]} :: decs), setter)
                                |   NONE =>
                                        mkEnv(List.rev tupleDecs,
                                                SetContainer{container=container, tuple = mkTuple selected,
                                                    filter=BoolVector.tabulate(List.length selected, fn _ => true)})
                            end
                        |   _ =>
                            mkEnv(List.rev tupleDecs,
                                    SetContainer{container=container, tuple = mkTuple selected,
                                                    filter=BoolVector.tabulate(List.length selected, fn _ => true)})
                    end

            |   NONE =>
                    mkEnv(List.rev tupleDecs,
                         SetContainer{container=container, tuple = mkTuple selected,
                                       filter=BoolVector.tabulate(List.length selected, fn _ => true)})
        end

    |   simpPostSetContainer(container, Cond(ifpt, thenpt, elsept), RevList tupleDecs, filter) =
            mkEnv(List.rev tupleDecs,
                Cond(ifpt,
                    simpPostSetContainer(container, thenpt, RevList [], filter),
                    simpPostSetContainer(container, elsept, RevList [], filter)))

    |   simpPostSetContainer(container, Newenv(envDecs, envExp), RevList tupleDecs, filter) =
            simpPostSetContainer(container, envExp, RevList(List.rev envDecs @ tupleDecs), filter)

    |   simpPostSetContainer(container, BeginLoop{loop, arguments}, RevList tupleDecs, filter) =
            mkEnv(List.rev tupleDecs,
                BeginLoop{loop = simpPostSetContainer(container, loop, RevList [], filter),
                    arguments=arguments})

    |   simpPostSetContainer(_, loop as Loop _, RevList tupleDecs, _) =
            (* If we are inside a BeginLoop we only set the container on leaves
               that exit the loop.  Loop entries will go back to the BeginLoop
               so we don't add SetContainer nodes. *)
            mkEnv(List.rev tupleDecs, loop)

    |   simpPostSetContainer(container, Handle{exp, handler, exPacketAddr}, RevList tupleDecs, filter) =
            mkEnv(List.rev tupleDecs,
                Handle{
                    exp = simpPostSetContainer(container, exp, RevList [], filter),
                    handler = simpPostSetContainer(container, handler, RevList [], filter),
                    exPacketAddr = exPacketAddr})

    |   simpPostSetContainer(container, tupleGen, RevList tupleDecs, filter) =
            mkEnv(List.rev tupleDecs, mkSetContainer(container, tupleGen, filter))

    fun simplifier{code, numLocals, maxInlineSize} =
    let
        val localAddressAllocator = ref 0
        val addrTab = Array.array(numLocals, NONE)
        
        fun lookupAddr (LoadLocal addr) = valOf(Array.sub(addrTab, addr))
        |   lookupAddr (env as LoadArgument _) = (EnvGenLoad env, EnvSpecNone)
        |   lookupAddr (env as LoadRecursive) = (EnvGenLoad env, EnvSpecNone)
        |   lookupAddr (LoadClosure _) = raise InternalError "top level reached in simplifier"

        and enterAddr (addr, tab) = Array.update (addrTab, addr, SOME tab)

        fun mkAddr () = 
            ! localAddressAllocator before localAddressAllocator := ! localAddressAllocator + 1
        val reprocess = ref false
        val (gen, RevList bindings, spec) =
            simpSpecial(code,
                {lookupAddr = lookupAddr, enterAddr = enterAddr, nextAddress = mkAddr,
                 reprocess = reprocess, maxInlineSize = maxInlineSize}, RevList[])
    in
        ((gen, List.rev bindings, spec), ! localAddressAllocator, !reprocess)
    end
    
    fun specialToGeneral(g, b as _ :: _, s) = mkEnv(b, specialToGeneral(g, [], s))
    |   specialToGeneral(Constnt(w, p), [], s) = Constnt(w, setInline s p)
    |   specialToGeneral(g, [], _) = g


    structure Sharing =
    struct
        type codetree = codetree
        and codeBinding = codeBinding
        and envSpecial = envSpecial
    end
end;
