(*
    Copyright (c) 2012,13,16,18-23, 25 David C.J. Matthews

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

(* Miscellaneous construction and operation functions on the code-tree. *)

functor CodetreeFunctions(
    structure BaseCodeTree: BASECODETREE
    structure Strongly:
        sig val stronglyConnectedComponents: {nodeAddress: 'a -> int, arcs: 'a -> int list } -> 'a list -> 'a list list end
) : CODETREEFUNCTIONS
=
struct
    open BaseCodeTree
    open Strongly
    open Address
    exception InternalError = Misc.InternalError
    
    fun mkEnv([], exp) = exp
    |   mkEnv(decs, exp) = Newenv(decs, exp)

    val word0 = toMachineWord 0
    and word1 = toMachineWord 1

    val False = word0  
    and True  = word1 

    val F_mutable_words : Word8.word = Word8.orb (F_words, F_mutable)

    val CodeFalse = Constnt(False, [])
    and CodeTrue  = Constnt(True, [])
    and CodeZero  = Constnt(word0, [])
   
    (* Properties of code.  This indicates the extent to which the
       code has side-effects (i.e. where even if the result is unused
       the code still needs to be produced) or is applicative
       (i.e. where its value depends only arguments and can safely
       be reordered). *)

    (* These were originally explicitly defined to match information from the run-time
       system about built-in functions.  That's no longer needed so these can be arbitrary values. *)
    val PROPWORD_NORAISE  = 0wx4
    and PROPWORD_NOUPDATE = 0wx2
    and PROPWORD_NODEREF  = 0wx1

    local
        infix orb andb
        val op orb = Word.orb and op andb = Word.andb
        val noSideEffect = PROPWORD_NORAISE orb PROPWORD_NOUPDATE
        val applicative = noSideEffect orb PROPWORD_NODEREF
    in
        fun codeProps (Lambda _) = applicative

        |   codeProps (Constnt _) = applicative

        |   codeProps (Extract _) = applicative

        |   codeProps (TagTest{ test, ... }) = codeProps test

        |   codeProps (Cond(i, t, e)) = codeProps i andb codeProps t andb codeProps e

        |   codeProps (Newenv(decs, exp)) =
                List.foldl (fn (d, r) => bindingProps d andb r) (codeProps exp) decs

        |   codeProps (Handle { exp, handler, ... }) =
                (* A handler processes all the exceptions in the body *)
                (codeProps exp orb PROPWORD_NORAISE) andb codeProps handler

        |   codeProps (Tuple { fields, ...}) = testList fields

        |   codeProps (Indirect{base, ...}) = codeProps base

            (* A built-in function may be side-effect free.  This can
               occur if we have, for example, "if exp1 orelse exp2"
               where exp2 can be reduced to "true", typically because it's
               inside an inline function and some of the arguments to the
               function are constants.  This then gets converted to
               (exp1; true) and we can eliminate exp1 if it is simply
               a comparison. *)
        |   codeProps (Unary{oper, arg1}) =
            let
                open BuiltIns
                val operProps =
                    case oper of
                        NotBoolean => applicative
                    |   IsTaggedValue => applicative
                    |   MemoryCellLength => applicative
                        (* MemoryCellFlags could return a different result if a mutable cell was locked. *)
                    |   MemoryCellFlags => applicative
                    |   ClearMutableFlag => Word.orb(PROPWORD_NODEREF, PROPWORD_NORAISE)
                    |   LongWordToTagged => applicative
                    |   SignedToLongWord => applicative
                    |   UnsignedToLongWord => applicative
                    |   RealAbs _ => applicative (* Does not depend on rounding setting. *)
                    |   RealNeg _ => applicative (* Does not depend on rounding setting. *)
                        (* If we float a 64-bit int to a 64-bit floating point value we may
                           lose precision so this depends on the current rounding mode. *)
                    |   RealFixedInt _ => Word.orb(PROPWORD_NOUPDATE, PROPWORD_NORAISE)
                    |   FloatToDouble => applicative
                        (* This also depends on the current rounding mode. *)
                    |   DoubleToFloat => Word.orb(PROPWORD_NOUPDATE, PROPWORD_NORAISE)
                        (* May raise the overflow exception *)
                    |   RealToInt _ => PROPWORD_NOUPDATE orb PROPWORD_NODEREF
                    |   TouchAddress => PROPWORD_NORAISE (* Treat as updating a notional reference count. *)
                    |   AllocCStack => PROPWORD_NORAISE
                    |   LockMutex => PROPWORD_NORAISE
                    |   TryLockMutex => PROPWORD_NORAISE
                    |   UnlockMutex => PROPWORD_NORAISE
                    |   Log2Word => applicative
            in
                operProps andb codeProps arg1
            end

        |   codeProps (Binary{oper, arg1, arg2}) =
            let
                open BuiltIns
                val mayRaise = PROPWORD_NOUPDATE orb PROPWORD_NODEREF
                val operProps =
                    case oper of
                        WordComparison _ => applicative
                    |   FixedPrecisionArith _ => mayRaise
                    |   WordArith _ => applicative (* Quot and Rem don't raise exceptions - zero checking is done before. *)
                    |   WordLogical _ => applicative
                    |   WordShift _ => applicative
                    |   AllocateByteMemory => Word.orb(PROPWORD_NOUPDATE, PROPWORD_NORAISE)
                            (* Allocation returns a different value on each call. *)
                    |   LargeWordComparison _ => applicative
                    |   LargeWordArith _ => applicative (* Quot and Rem don't raise exceptions - zero checking is done before. *)
                    |   LargeWordLogical _ => applicative
                    |   LargeWordShift _ => applicative
                    |   RealComparison _ => applicative
                        (* Real arithmetic operations depend on the current rounding setting. *)
                    |   RealArith _ => Word.orb(PROPWORD_NOUPDATE, PROPWORD_NORAISE)
                    |   FreeCStack => PROPWORD_NORAISE orb PROPWORD_NODEREF
                    |   PointerEq => applicative
                    |   LongArbComparison  _ => applicative
                    |   LongArbArith  _ => applicative

            in
                operProps andb codeProps arg1 andb codeProps arg2
            end

        |   codeProps (Nullary{oper=BuiltIns.GetCurrentThreadId}) = Word.orb(PROPWORD_NOUPDATE, PROPWORD_NORAISE)
        |   codeProps (Nullary{oper=BuiltIns.CPUPause}) = PROPWORD_NORAISE
        |   codeProps (Nullary{oper=BuiltIns.CreateMutex}) = Word.orb(PROPWORD_NOUPDATE, PROPWORD_NORAISE)

        |   codeProps (Arbitrary{shortCond, arg1, arg2, longCall, ...}) =
                (* Arbitrary precision operations are applicative but the longCall is
                   a function call.  It should never have a side-effect so it might
                   be better to remove it. *)
                codeProps shortCond andb codeProps arg1 andb codeProps arg2 andb codeProps longCall

        |   codeProps (AllocateWordMemory {numWords, flags, initial}) =
            let
                val operProps = Word.orb(PROPWORD_NOUPDATE, PROPWORD_NORAISE)
            in
                operProps andb codeProps numWords andb codeProps flags andb codeProps initial
            end

        |   codeProps (Eval _) = 0w0

        |   codeProps(Raise exp) = codeProps exp andb (Word.notb PROPWORD_NORAISE)

            (* Treat these as unsafe at least for the moment.
               N.B. BeginLoop can raise Interrupt if it is interrupted externally. *)
        |   codeProps(BeginLoop _) = 0w0

        |   codeProps(Loop _) = 0w0

        |   codeProps (SetContainer{ container, tuple, ...}) =
                codeProps container andb codeProps tuple andb Word.orb(PROPWORD_NODEREF, PROPWORD_NORAISE)

        |   codeProps (LoadOperation {address, kind}) =
            let
                val operProps =
                    case kind of
                        LoadStoreMLWord {isImmutable=true} => applicative
                    |   LoadStoreMLByte {isImmutable=true} => applicative
                    |   _ => Word.orb(PROPWORD_NOUPDATE, PROPWORD_NORAISE)
            in
                operProps andb addressProps address
            end

        |   codeProps (StoreOperation {address, value, ...}) =
                Word.orb(PROPWORD_NODEREF, PROPWORD_NORAISE) andb addressProps address andb codeProps value
        
        |   codeProps (BlockOperation {kind, sourceLeft, destRight, length}) =
            let
                val operProps =
                    case kind of
                    BlockOpMove _ => PROPWORD_NORAISE
                |   BlockOpEqualByte => applicative
                |   BlockOpCompareByte => applicative
            in
                operProps andb addressProps sourceLeft andb addressProps destRight andb codeProps length
            end

        and testList t = List.foldl(fn (c, r) => codeProps c andb r) applicative t
    
        and bindingProps(Declar{value, ...}) = codeProps value
        |   bindingProps(RecDecs _) = applicative (* These should all be lambdas *)
        |   bindingProps(NullBinding c) = codeProps c
        |   bindingProps(Container{setter, ...}) = codeProps setter
        
        and addressProps{base, index=NONE, ...} = codeProps base
        |   addressProps{base, index=SOME index, ...} = codeProps base andb codeProps index

        (* sideEffectFree - does not raise an exception or make an assignment. *)
        fun sideEffectFree c = (codeProps c andb noSideEffect) = noSideEffect
        (* reorderable - does not raise an exception or access a reference. *)
        and reorderable c = codeProps c = applicative
        (* exceptionFree does not raise an exception but could have a side-effect. *)
        and exceptionFree c = (codeProps c andb PROPWORD_NORAISE) <> 0w0
    end

    (* Return the inline property if it is set. *)
    fun findInline [] = EnvSpecNone
    |   findInline (h::t) =
            if Universal.tagIs CodeTags.inlineCodeTag h
            then Universal.tagProject CodeTags.inlineCodeTag h
            else findInline t

    (* Makes a constant value from an expression which is known to be
       constant but may involve inline functions, tuples etc. *)
    fun makeConstVal (cVal:codetree) =
    let
        fun makeVal (c as Constnt _) = c
             (* should just be a tuple  *)
            (* Get a vector, copy the entries into it and return it as a constant. *)
        |   makeVal (Tuple {fields= [], ...}) = CodeZero (* should have been optimised already! *)
        |   makeVal (Tuple {fields, ...}) =
            let
                val tupleSize = List.length fields
                val vec : address = allocWordData(Word.fromInt tupleSize, F_mutable_words, word0)
                val fieldCode = map makeVal fields
      
                fun copyToVec ([], _) = []
                |   copyToVec (Constnt(w, prop) :: t, locn) =
                    (
                        assignWord (vec, locn, w);
                        prop :: copyToVec (t, locn + 0w1)
                    )
                |   copyToVec _ = raise InternalError "not constant"
                
                val props = copyToVec(fieldCode, 0w0)
                (* If any of the constants have properties create a tuple property
                   for the result. *)
                val tupleProps =
                    if List.all null props
                    then []
                    else
                    let
                        (* We also need to construct an EnvSpecTuple property because findInline
                           does not look at tuple properties. *)
                        val inlineProps = map findInline props
                        val inlineProp =
                            if List.all (fn EnvSpecNone => true | _ => false) inlineProps
                            then []
                            else
                            let
                                fun tupleEntry n =
                                    (EnvGenConst(loadWord(vec, Word.fromInt n), List.nth(props, n)),
                                     List.nth(inlineProps, n))
                            in
                                [Universal.tagInject CodeTags.inlineCodeTag (EnvSpecTuple(tupleSize, tupleEntry))]
                            end
                    in
                        Universal.tagInject CodeTags.tupleTag props :: inlineProp
                    end
            in
                lock vec;
                Constnt(toMachineWord vec, tupleProps)
            end
        |   makeVal _ = raise InternalError "makeVal - not constant or tuple"
    in
        makeVal cVal
    end

    local
        fun allConsts []       = true
        |   allConsts (Constnt _ :: t) = allConsts t
        |   allConsts _ = false
        
        fun mkRecord isVar xp =
        let
            val tuple = Tuple{fields = xp, isVariant = isVar }
        in
            if allConsts xp
            then (* Make it now. *) makeConstVal tuple
            else tuple
        end;
        
    in  
        val mkTuple = mkRecord false
        and mkDatatype = mkRecord true
    end

    (* Set the inline property.  If the property is already
       present it is replaced.  If the property we are setting is
       EnvSpecNone no property is set. *)
    fun setInline p (h::t) =
            if Universal.tagIs CodeTags.inlineCodeTag h
            then setInline p t
            else h :: setInline p t
    |   setInline EnvSpecNone [] = []
    |   setInline p [] = [Universal.tagInject CodeTags.inlineCodeTag p]

    (* These are very frequently used and it might be worth making
       special bindings for values such as 0, 1, 2, 3 etc to reduce
       garbage. *)
    fun checkNonZero n = if n < 0 then raise InternalError "mkLoadxx: argument negative" else n
    val mkLoadLocal = Extract o LoadLocal o checkNonZero
    and mkLoadArgument = Extract o LoadArgument o checkNonZero
    and mkLoadClosure = Extract o LoadClosure o checkNonZero

    (* Set the container to the fields of the record.  Try to push this
       down as far as possible. *)
    fun mkSetContainer(container, Cond(ifpt, thenpt, elsept), filter) =
        Cond(ifpt, mkSetContainer(container, thenpt, filter),
            mkSetContainer(container, elsept, filter))

    |  mkSetContainer(container, Newenv(decs, exp), filter) =
            Newenv(decs, mkSetContainer(container, exp, filter))

    |  mkSetContainer(_, r as Raise _, _) =
        r (* We may well have the situation where one branch of an "if" raises an
             exception.  We can simply raise the exception on that branch. *)

    |   mkSetContainer(container, Handle {exp, handler, exPacketAddr}, filter) =
            Handle{exp=mkSetContainer(container, exp, filter),
                   handler=mkSetContainer(container, handler, filter),
                   exPacketAddr = exPacketAddr}

    |   mkSetContainer(container, tuple, filter) =
            SetContainer{container = container, tuple = tuple, filter = filter }

    local
        val except: exn = InternalError "Invalid load encountered in compiler"
        (* Exception value to use for invalid cases.  We put this in the code
           but it should never actually be executed.  *)
        val raiseError = Raise (Constnt (toMachineWord except, []))
    in
        (* Look for an entry in a tuple. Used in both the optimiser and in mkInd. *)
        fun findEntryInBlock (Tuple { fields, isVariant, ...}, offset, isVar) =
            (
                isVariant = isVar orelse raise InternalError "findEntryInBlock: tuple/datatype mismatch";
                if offset < List.length fields
                then List.nth(fields, offset)
                (* This can arise if we're processing a branch of a case discriminating on
                   a datatype which won't actually match at run-time. e.g. Tests/Succeed/Test030. *)
                else if isVar
                then raiseError
                else raise InternalError "findEntryInBlock: invalid address"
            )

        |   findEntryInBlock (Constnt (b, props), offset, isVar) =
            let
                (* Find the tuple property if it is present and extract the field props. *)
                val fieldProps =
                    case List.find(Universal.tagIs CodeTags.tupleTag) props of
                        NONE => []
                    |   SOME p => List.nth(Universal.tagProject CodeTags.tupleTag p, offset)
            in
                case findInline props of
                    EnvSpecTuple(_, env) =>
                    (* Do the selection now.  This is especially useful if we
                       have a global structure  *)
                    (* At the moment at least we assume that we can get all the
                       properties from the tuple selection. *)
                    (
                        case env offset of
                            (EnvGenConst(w, p), inl) => Constnt(w, setInline inl p)
                        (* The general value from selecting a field from a constant tuple must be a constant. *)
                        |   _ => raise InternalError "findEntryInBlock: not constant"
                    )
                |   _ =>
                      (* The ML compiler may generate loads from invalid addresses as a
                         result of a val binding to a constant which has the wrong shape.
                         e.g. val a :: b = nil
                         It will always result in a Bind exception being generated 
                         before the invalid load, but we have to be careful that the
                         optimiser does not fall over.  *)
                    if isShort b
                        orelse not (Address.isWords (toAddress b))
                        orelse Address.length (toAddress b) <= Word.fromInt offset
                    then if isVar
                    then raiseError
                    else raise InternalError "findEntryInBlock: invalid address"
                    else Constnt (loadWord (toAddress b, Word.fromInt offset), fieldProps)
            end

        |   findEntryInBlock(base, offset, isVar) =
                Indirect {base = base, offset = offset, indKind = if isVar then IndVariant else IndTuple} (* anything else *)
     end
        
    (* Exported indirect load operation i.e. load a field from a tuple.
       We can't use  findEntryInBlock in every case since that discards
       unused entries in a tuple and at this point we haven't checked
       that the unused entries don't have
       side-effects/raise exceptions e.g. #1 (1, raise Fail "bad") *)
    local
        fun mkIndirect isVar (addr, base as Constnt _) = findEntryInBlock(base, addr, isVar)
        |   mkIndirect isVar (addr, base) =
                Indirect {base = base, offset = addr, indKind = if isVar then IndVariant else IndTuple}
    
    in
        val mkInd = mkIndirect false and mkVarField = mkIndirect true
    end
    
    fun mkIndContainer(addr, base) = Indirect{offset=addr, base=base, indKind=IndContainer}

    (* Create a tuple from a container. *)
    fun mkTupleFromContainer(addr, size) =
        Tuple{fields = List.tabulate(size, fn n => mkIndContainer(n, mkLoadLocal addr)), isVariant = false}

    (* Get the value from the code. *)
    fun evalue (Constnt(c, _)) = SOME c
    |   evalue _ = NONE

    (* This is really to simplify the change from mkEnv taking a codetree list to
       taking a codeBinding list * code.  This extracts the last entry which must
       be a NullBinding and packages the declarations with it. *)
    fun decSequenceWithFinalExp decs =
    let
        fun splitLast _ [] = raise InternalError "decSequenceWithFinalExp: empty"
        |   splitLast decs [NullBinding exp] = (List.rev decs, exp)
        |   splitLast _ [_] = raise InternalError "decSequenceWithFinalExp: last is not a NullDec"
        |   splitLast decs (hd::tl) = splitLast (hd:: decs) tl
    in
        mkEnv(splitLast [] decs)
    end
    
    local
        type node = { addr: int, lambda: lambdaForm, use: codeUse list }
        fun nodeAddress({addr, ...}: node) = addr
        and arcs({lambda={closure, ...}, ...}: node) =
            List.foldl(fn (LoadLocal addr, l) => addr :: l | (_, l) => l) [] closure
    in
        val stronglyConnected = stronglyConnectedComponents{nodeAddress=nodeAddress, arcs=arcs}
    end

    (* In general any mutually recursive declaration can refer to any
       other.  It's better to partition the recursive declarations into
       strongly connected components i.e. those that actually refer
       to each other.  *)
    fun partitionMutualBindings(RecDecs rlist) =
        let
            val processed = stronglyConnected rlist
            (* Convert the result.  Note that stronglyConnectedComponents returns the
               dependencies in the reverse order i.e. if X depends on Y but not the other
               way round then X will appear before Y in the list.  We need to reverse
               it so that X goes after Y. *)
            fun rebuild ([{lambda, addr, use}], tl) =
                   Declar{addr=addr, use=use, value=Lambda lambda} :: tl
            |   rebuild (multiple, tl) = RecDecs multiple :: tl
        in
            List.foldl rebuild [] processed
        end
        (* This is only intended for RecDecs but it's simpler to handle all bindings. *)
    |   partitionMutualBindings other = [other]


    (* Functions to help in building a closure. *)
    datatype createClosure = Closure of (loadForm * int) list ref
    
    fun makeClosure() = Closure(ref [])

        (* Function to build a closure.  Items are added to the closure if they are not already there. *)
    fun addToClosure (Closure closureList) (ext: loadForm): loadForm =
        case (List.find (fn (l, _) => l = ext) (!closureList), ! closureList) of
            (SOME(_, n), _) => (* Already there *) LoadClosure n
        |   (NONE, []) => (* Not there - first *) (closureList := [(ext, 0)]; LoadClosure 0)
        |   (NONE, cl as (_, n) :: _) => (closureList := (ext, n+1) :: cl; LoadClosure(n+1))

    fun extractClosure(Closure (ref closureList)) =
        List.foldl (fn ((ext, _), l) => ext :: l) [] closureList

    datatype inlineTest =
        TooBig
    |   NonRecursive
    |   TailRecursive of bool vector
    |   NonTailRecursive of bool vector

    fun evaluateInlining(function, numArgs, maxInlineSize) =
    let
        (* This checks for the possibility of inlining a function.  It sees if it is
           small enough according to some rough estimate of the cost and it also looks
           for recursive uses of the function.
           Typically if the function is small enough to inline there will be only
           one recursive use but we consider the possibility of more than one.  If
           the only uses are tail recursive we can replace the recursive calls by
           a Loop with a BeginLoop outside it.  If there are non-tail recursive
           calls we may be able to lift out arguments that are unchanged.  For
           example for fun map f [] = [] | map f (a::b) = f a :: map f b 
           it may be worth lifting out f and generating specific mapping
           functions for each application. *)
        val hasRecursiveCall = ref false (* Set to true if rec call *)
        val allTail = ref true (* Set to false if non recursive *)
        (* An element of this is set to false if the actual value if anything
           other than the original argument.  At the end we are then
           left with the arguments that are unchanged. *)
        val argMod = Array.array(numArgs, true)

        infix 6 --
        (* Subtract y from x but return 0 rather than a negative number. *)
        fun x -- y = if x >= y then x-y else 0

        (* Check for the code size and also recursive references.  N,B. We assume in hasLoop
           that tail recursion applies only with Cond, Newenv and Handler. *)
        fun checkUse _ (_, 0, _) = 0 (* The function is too big to inline. *)
 
        |   checkUse isMain (Newenv(decs, exp), cl, isTail) =
            let
                fun checkBind (Declar{value, ...}, cl) = checkUse isMain(value, cl, false)
                |   checkBind (RecDecs decs, cl) = List.foldl(fn ({lambda, ...}, n) => checkUse isMain (Lambda lambda, n, false)) cl decs
                |   checkBind (NullBinding c, cl) = checkUse isMain (c, cl, false)
                |   checkBind (Container{setter, ...}, cl) = checkUse isMain(setter, cl -- 1, false)
            in
                checkUse isMain (exp, List.foldl checkBind cl decs, isTail)
            end

        |   checkUse _      (Constnt(w, _), cl, _) = if isShort w then cl else cl -- 1

            (* A recursive reference in any context other than a call prevents any inlining. *)
        |   checkUse true   (Extract LoadRecursive, _, _) = 0
        |   checkUse _      (Extract _, cl, _) = cl -- 1

        |   checkUse isMain (Indirect{base, ...}, cl, _) = checkUse isMain (base, cl -- 1, false)

        |   checkUse _      (Lambda {body, argTypes, closure, ...}, cl, _) =
                (* For the moment, any recursive use in an inner function prevents inlining. *)
                if List.exists (fn LoadRecursive => true | _ => false) closure
                then 0
                else checkUse false (body, cl -- (List.length argTypes + List.length closure), false)

        |   checkUse true (Eval{function = Extract LoadRecursive, argList, ...}, cl, isTail) =
            let
                (* If the actual argument is anything but the original argument
                   then the corresponding entry in the array is set to false. *)
                fun testArg((exp, _), n) =
                (
                    if (case exp of Extract(LoadArgument a) => n = a | _ => false)
                    then ()
                    else Array.update(argMod, n, false);
                    n+1
                )
            in
                List.foldl testArg 0 argList;
                hasRecursiveCall := true;
                if isTail then () else allTail := false;
                List.foldl(fn ((e, _), n) => checkUse true (e, n, false)) (cl--3) argList
            end

        |   checkUse isMain (Eval{function, argList, ...}, cl, _) =
                checkUse isMain (function, List.foldl(fn ((e, _), n) => checkUse isMain (e, n, false)) (cl--2) argList, false)

        |   checkUse _ (Nullary _, cl, _) = cl -- 1
        |   checkUse isMain (Unary{arg1, ...}, cl, _) = checkUse isMain (arg1, cl -- 1, false)
        |   checkUse isMain (Binary{arg1, arg2, ...}, cl, _) = checkUseList isMain ([arg1, arg2], cl -- 1)
        |   checkUse isMain (Arbitrary{arg1, arg2, ...}, cl, _) = checkUseList isMain ([arg1, arg2], cl -- 4)
        |   checkUse isMain (AllocateWordMemory {numWords, flags, initial}, cl, _) =
                checkUseList isMain ([numWords, flags, initial], cl -- 1)

        |   checkUse isMain (Cond(i, t, e), cl, isTail) =
                checkUse isMain (i, checkUse isMain (t, checkUse isMain (e, cl -- 2, isTail), isTail), false)
        |   checkUse isMain (BeginLoop { loop, arguments, ...}, cl, _) =
                checkUse isMain (loop, List.foldl (fn (({value, ...}, _), n) => checkUse isMain (value, n, false)) cl arguments, false)
        |   checkUse isMain (Loop args, cl, _) = List.foldl(fn ((e, _), n) => checkUse isMain (e, n, false)) cl args
        |   checkUse isMain (Raise c, cl, _) = checkUse isMain (c, cl -- 1, false)
        |   checkUse isMain (Handle {exp, handler, ...}, cl, isTail) =
                checkUse isMain (exp, checkUse isMain (handler, cl, isTail), false)
        |   checkUse isMain (Tuple{ fields, ...}, cl, _) = checkUseList isMain (fields, cl)

        |   checkUse isMain (SetContainer{container, tuple = Tuple { fields, ...}, ...}, cl, _) =
                (* This can be optimised *)
                checkUse isMain (container, checkUseList isMain (fields, cl), false)
        |   checkUse isMain (SetContainer{container, tuple, filter}, cl, _) =
                checkUse isMain (container, checkUse isMain (tuple, cl -- (BoolVector.length filter), false), false)

        |   checkUse isMain (TagTest{test, ...}, cl, _) = checkUse isMain (test, cl -- 1, false)

        |   checkUse isMain (LoadOperation{address, ...}, cl, _) = checkUseAddress isMain (address, cl -- 1)

        |   checkUse isMain (StoreOperation{address, value, ...}, cl, _) =
                checkUse isMain (value, checkUseAddress isMain (address, cl -- 1), false)

        |   checkUse isMain (BlockOperation{sourceLeft, destRight, length, ...}, cl, _) =
                checkUse isMain (length,
                    checkUseAddress isMain (destRight, checkUseAddress isMain (sourceLeft, cl -- 1)), false)
        
        and checkUseList isMain (elems, cl) =
            List.foldl(fn (e, n) => checkUse isMain (e, n, false)) cl elems

        and checkUseAddress isMain ({base, index=NONE, ...}, cl) = checkUse isMain (base, cl, false)
        |   checkUseAddress isMain ({base, index=SOME index, ...}, cl) = checkUseList isMain ([base, index], cl)
        
        val costLeft = checkUse true (function, maxInlineSize, true)
    in
        if costLeft = 0
        then TooBig
        else if not (! hasRecursiveCall) 
        then NonRecursive
        else if ! allTail then TailRecursive(Array.vector argMod)
        else NonTailRecursive(Array.vector argMod)
    end
    
    structure Sharing =
    struct
        type codetree = codetree
        and codeBinding = codeBinding
        and loadForm = loadForm
        and createClosure = createClosure
        and envSpecial = envSpecial
    end

end;
