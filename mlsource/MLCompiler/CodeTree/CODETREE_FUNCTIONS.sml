(*
    Copyright (c) 2012,13,16 David C.J. Matthews

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

functor CODETREE_FUNCTIONS(
    structure BASECODETREE: BaseCodeTreeSig
    structure STRONGLY:
        sig val stronglyConnectedComponents: {nodeAddress: 'a -> int, arcs: 'a -> int list } -> 'a list -> 'a list list end
) : CodetreeFunctionsSig
=
struct
    open BASECODETREE
    open STRONGLY
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

    (* The RTS has a table of properties for RTS functions.  The 103 call
       returns these Or-ed into the register mask. *)
    val PROPWORD_NORAISE  = 0wx40000000
    and PROPWORD_NOUPDATE = 0wx20000000
    and PROPWORD_NODEREF  = 0wx10000000

    (* Since RTS calls are being eliminated leave residual versions of these. *)
    fun earlyRtsCall _ = false
    and sideEffectFreeRTSCall _ = false

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
        |   codeProps GetThreadId = Word.orb(PROPWORD_NOUPDATE, PROPWORD_NORAISE)

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
                    |   AtomicIncrement => PROPWORD_NORAISE
                    |   AtomicDecrement => PROPWORD_NORAISE
                    |   AtomicReset => Word.orb(PROPWORD_NODEREF, PROPWORD_NORAISE)
                    |   LongWordToTagged => applicative
                    |   SignedToLongWord => applicative
                    |   UnsignedToLongWord => applicative
                    |   RealAbs => applicative (* Does not depend on rounding setting. *)
                    |   RealNeg => applicative (* Does not depend on rounding setting. *)
                        (* If we float a 64-bit int to a 64-bit floating point value we may
                           lose precision so this depends on the current rounding mode. *)
                    |   FloatFixedInt => Word.orb(PROPWORD_NOUPDATE, PROPWORD_NORAISE)
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

            in
                operProps andb codeProps arg1 andb codeProps arg2
            end

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

            (* Treat these as unsafe at least for the moment. *)
        |   codeProps(BeginLoop _) = 0w0

        |   codeProps(Loop _) = 0w0

        |   codeProps (SetContainer _) = 0w0

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
                Indirect {base = base, offset = offset, isVariant = isVar} (* anything else *)
     end
        
    (* Exported indirect load operation i.e. load a field from a tuple.
       We can't use  findEntryInBlock in every case since that discards
       unused entries in a tuple and at this point we haven't checked
       that the unused entries don't have
       side-effects/raise exceptions e.g. #1 (1, raise Fail "bad") *)
    local
        fun mkIndirect isVar (addr, base as Constnt _) = findEntryInBlock(base, addr, isVar)
        |   mkIndirect isVar (addr, base) = Indirect {base = base, offset = addr, isVariant = isVar}
    
    in
        val mkInd = mkIndirect false and mkVarField = mkIndirect true
    end

    (* Create a tuple from a container. *)
    fun mkTupleFromContainer(addr, size) =
        Tuple{fields = List.tabulate(size, fn n => mkInd(n, mkLoadLocal addr)), isVariant = false}

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
    fun partitionMutableBindings(RecDecs rlist) =
        let
            val processed = stronglyConnected rlist
            (* Convert the result.  Note that stronglyConnectedComponents returns the
               dependencies in the reverse order i.e. if X depends on Y but not the other
               way round then X will appear before Y in the list.  We need to reverse
               it so that X goes after Y. *)
            fun rebuild ([], _) = raise InternalError "partitionMutableBindings" (* Should not happen *)
            |   rebuild ([{addr, lambda, use}], tl) = Declar{addr=addr, use=use, value=Lambda lambda} :: tl
            |   rebuild (multiple, tl) = RecDecs multiple :: tl
        in
            List.foldl rebuild [] processed
        end
        (* This is only intended for RecDecs but it's simpler to handle all bindings. *)
    |   partitionMutableBindings other = [other]


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

    structure Sharing =
    struct
        type codetree = codetree
        and codeBinding = codeBinding
        and loadForm = loadForm
        and createClosure = createClosure
        and envSpecial = envSpecial
    end

end;
