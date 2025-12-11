(*
    Copyright (c) 2012,13,15-22 David C.J. Matthews

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

functor CodeTreeConstruction (

structure Debug: DEBUG
structure Pretty : PRETTY
structure BaseCodeTree: BASECODETREE
structure CodetreeFunctions: CODETREEFUNCTIONS

structure Backend:
sig
    type codetree
    type machineWord = Address.machineWord
    val codeGenerate:
        codetree * int * Universal.universal list -> (unit -> machineWord) * Universal.universal list
    structure Foreign: FOREIGNCALL
    structure Sharing : sig type codetree = codetree end
end

structure Optimiser:
sig
    type codetree  and envSpecial and codeBinding
    val codetreeOptimiser: codetree  * Universal.universal list * int ->
        { numLocals: int, general: codetree, bindings: codeBinding list, special: envSpecial }
    structure Sharing: sig type codetree = codetree and envSpecial = envSpecial and codeBinding = codeBinding end
end

sharing type
    Pretty.pretty
=   BaseCodeTree.pretty

sharing
    BaseCodeTree.Sharing
=   CodetreeFunctions.Sharing
=   Backend.Sharing
=   Optimiser.Sharing

) : CODETREE =
struct
    open Address
    open StretchArray
    open BaseCodeTree
    open Pretty
    open CodetreeFunctions
  
    exception InternalError = Misc.InternalError
    and Interrupt = Thread.Thread.Interrupt
  
    infix 9 sub;

    fun mkDec (laddr, res) = Declar{value = res, addr = laddr, use=[]}

    fun deExtract(Extract ext) = ext | deExtract _ = raise InternalError "deExtract"
 
    datatype level =
        Level of { lev: int, closure: createClosure, lookup: int * int * bool -> loadForm }

    local
        (* We can have locals at the outer level. *)
        fun bottomLevel(addr, 0, false) =
            if addr < 0 then raise InternalError "load: negative"
            else LoadLocal addr
        |   bottomLevel _ = (* Either the level is wrong or it's a parameter. *)
                raise InternalError "bottom level"
    in
        val baseLevel =
            Level { lev = 0, closure = makeClosure(), lookup = bottomLevel }
    end
  
    fun newLevel (Level{ lev, lookup, ...}) =
    let
        val closureList = makeClosure()
        val makeClosure = addToClosure closureList

        fun check n = if n < 0 then raise InternalError "load: negative" else n

        fun thisLevel(addr, level, isParam) =
        if level < 0 then raise InternalError "mkLoad: level must be non-negative"
        else if level > 0
        then makeClosure(lookup(addr, level-1, isParam))
        else (* This level *) if isParam
        then LoadArgument(check addr)
        else LoadLocal(check addr)
    in
        Level { lev = lev+1, closure = closureList, lookup = thisLevel }
    end
    
    fun getClosure(Level{ closure, ...})  = List.map Extract (extractClosure closure)
 
    fun mkLoad (addr, Level { lev = newLevel, lookup, ... } , Level { lev = oldLevel, ... }) =
        Extract(lookup(addr, newLevel - oldLevel, false))

    and mkLoadParam(addr, Level { lev = newLevel, lookup, ... } , Level { lev = oldLevel, ... }) =
        Extract(lookup(addr, newLevel - oldLevel, true))

    (* Transform a function so that free variables are converted to closure form.  Returns the
       maximum local address used. *)

    fun genCode(pt, debugSwitches, numLocals) =
    let
        val printCodeTree      = Debug.getParameter Debug.codetreeTag debugSwitches
        and compilerOut        = Pretty.getCompilerOutput debugSwitches
        
(*        val printCodeTree = true
        and compilerOut = PRETTY.prettyPrint(TextIO.print, 70) *)

        (* If required, print it first.  This is the code that the front-end
           has produced. *)
        val () = if printCodeTree then compilerOut(pretty pt) else ()

        (* This ensures that everything is printed just before
           it is code-generated. *) 
        fun codeAndPrint(code, localCount) =
        let
            val () = if printCodeTree then compilerOut (BaseCodeTree.pretty code) else ();
        in
            Backend.codeGenerate(code, localCount, debugSwitches)
        end

        (* Optimise it. *)
        val { numLocals = localCount, general = gen, bindings = decs, special = spec } =
            Optimiser.codetreeOptimiser(pt, debugSwitches, numLocals)

        (* At this stage we have a "general" value and also, possibly a "special"
           value.  We could simply create mkEnv(decs, gen) and run preCode
           and genCode on that.  However, we would lose the ability to insert
           any inline functions from this code into subsequent top-level
           expressions.  We can't simply retain the "special" entry either
           because that may refer to values that have to be created once when
           the code is run.  Such values will be referenced by "load" entries
           which refer to entries in the "decs".  We construct a tuple which
           will contain the actual values after the code is run.  Then if
           we want the value at some time in the future when we use something
           from the "special" entry we can extract the corresponding value
           from this tuple.
           Previously, this code always generated a tuple containing every
           declaration.  That led to some very long compilation times because
           the back-end has some code which is quadratic in the number of entries
           on the stack.  We now try to prune bindings by only generating the tuple
           if we have an inline function somewhere and only generating bindings
           we actually need. *)
        fun simplifySpec (EnvSpecTuple(size, env)) =
            let
                (* Get all the field entries. *)
                fun simpPair (gen, spec) = (gen, simplifySpec spec)
                val fields = List.tabulate(size, simpPair o env)
            in
                if List.all(fn (_, EnvSpecNone) => true | _ => false) fields
                then EnvSpecNone
                else EnvSpecTuple(size, fn n => List.nth(fields, n))
            end
        |   simplifySpec s = s (* None or inline function. *)

    in
        case simplifySpec spec of
            EnvSpecNone =>
            let
                val (code, props) = codeAndPrint (mkEnv(decs, gen), localCount)
            in
                fn () => Constnt(code(), props)
            end

        |   simpleSpec  =>
            let
                (* The bindings are marked using a three-valued mark.  A binding is needed
                   if it is referenced in any way.  During the scan to find the references
                   we need to avoid processing an entry that has already been processed but
                   it is possible that a binding may be referenced as a general value only
                   (e.g. from a function closure) and separately as a special value.  See
                   Test148.ML *)
                datatype visit = UnVisited | VisitedGeneral | VisitedSpecial

                local
                    val refArray = Array.array(localCount, UnVisited)

                    fun findDecs EnvSpecNone = ()

                    |   findDecs (EnvSpecTuple(size, env)) =
                        let
                            val fields = List.tabulate(size, env)
                        in
                            List.app processGenAndSpec fields
                        end

                    |   findDecs (EnvSpecInlineFunction({closure, ...}, env)) =
                        let
                            val closureItems = List.tabulate(List.length closure, env)
                        in
                            List.app processGenAndSpec closureItems
                        end

                    |   findDecs (EnvSpecUnary _) = ()
                    |   findDecs (EnvSpecBinary _) = ()

                    and processGenAndSpec (gen, spec) =
                        (* The spec part needs only to be processed if this entry has
                           not yet been visited, *)
                        case gen of
                            EnvGenLoad(LoadLocal addr) =>
                                let
                                    val previous = Array.sub(refArray, addr)
                                in
                                    case (previous, spec) of
                                        (VisitedSpecial, _) => () (* Fully done *)
                                    |   (VisitedGeneral, EnvSpecNone) => () (* Nothing useful *)
                                    |   (_, EnvSpecNone) =>
                                            (* We need this entry but we don't have any special
                                               entry to process.  We could find another reference with a
                                               special entry. *)
                                            Array.update(refArray, addr, VisitedGeneral)
                                    |   (_, _) =>
                                            (
                                                (* This has a special entry.  Mark it and process. *)
                                                Array.update(refArray, addr, VisitedSpecial);
                                                findDecs spec
                                            )
                                end
                        |   EnvGenConst _ => ()
                        |   _ => raise InternalError "doGeneral: not LoadLocal or Constant"

                    val () = findDecs simpleSpec
                in
                    (* Convert to an immutable data structure.  This will continue
                       to be referenced in any inline function after the code has run. *)
                    val refVector = Array.vector refArray
                end
 
                val decArray = Array.array(localCount, CodeZero)
                
                fun addDec(addr, dec) =
                    if Vector.sub(refVector, addr) <> UnVisited then Array.update(decArray, addr, dec) else ()
    
                fun addDecs(Declar{addr, ...}) = addDec(addr, mkLoadLocal addr)
                |   addDecs(RecDecs decs) = List.app(fn {addr, ...} => addDec(addr, mkLoadLocal addr)) decs
                |   addDecs(NullBinding _) = ()
                |   addDecs(Container{addr, size, ...}) = addDec(addr, mkTupleFromContainer(addr, size))

                val () = List.app addDecs decs

                (* Construct the tuple and add the "general" value at the start. *)
                val resultTuple = mkTuple(gen :: Array.foldr(op ::) nil decArray)
                (* Now generate the machine code and return it as a function that can be called. *)
                val (code, codeProps) = codeAndPrint (mkEnv (decs, resultTuple), localCount)
            in
                (* Return a function that executes the compiled code and then creates the
                   final "global" value as the result. *)
                fn () =>
                    let
                        local
                            (* Execute the code.  This will perform any side-effects the user
                               has programmed and may raise an exception if that is required. *)
                            val resVector = code ()

                            (* The result is a vector containing the "general" value as the
                               first word and the evaluated bindings for any "special"
                               entries in subsequent words. *)
                            val decVals : address =
                                if isShort resVector
                                then raise InternalError "Result vector is not an address"
                                else toAddress resVector
                        in
                            fun resultWordN n = loadWord (decVals, n)
                            (* Get the general value, the zero'th entry in the vector. *)
                            val generalVal = resultWordN 0w0
                            (* Get the properties for a field in the tuple.  Because the result is
                               a tuple all the properties should be contained in a tupleTag entry. *)
                            val fieldProps =
                                case Option.map (Universal.tagProject CodeTags.tupleTag)
                                        (List.find(Universal.tagIs CodeTags.tupleTag) codeProps) of
                                    NONE => (fn _ => [])
                                |   SOME p => (fn n => List.nth(p, n))
                            val generalProps = fieldProps 0
                        end

                        (* Construct a new environment so that when an entry is looked 
                           up the corresponding constant is returned. *) 
                        fun newEnviron (oldEnv) args =
                        let
                            val (oldGeneral, oldSpecial) = oldEnv args
            
                            val genPair =
                                case oldGeneral of
                                    EnvGenLoad(LoadLocal addr) =>
                                        (
                                            (* For the moment retain this check.  It's better to have an assertion
                                               failure than a segfault. *)
                                            Vector.sub(refVector, addr) <> UnVisited orelse raise InternalError "Reference to non-existent binding";
                                            (resultWordN(Word.fromInt addr+0w1), fieldProps(addr+1))
                                        )
                                |   EnvGenConst c => c
                                |   _ => raise InternalError "codetree newEnviron: Not Extract or Constnt"
               
                            val specVal = mapSpec oldSpecial
                        in
                            (EnvGenConst genPair, specVal)
                        end
                        and mapSpec EnvSpecNone = EnvSpecNone
                        |   mapSpec (EnvSpecTuple(size, env)) = EnvSpecTuple(size, newEnviron env)
                        |   mapSpec (EnvSpecInlineFunction(spec, env)) = EnvSpecInlineFunction(spec, (newEnviron env))
                        |   mapSpec (EnvSpecUnary _) = EnvSpecNone
                        |   mapSpec (EnvSpecBinary _) = EnvSpecNone
                    in 
                        (* and return the whole lot as a global value. *)
                        Constnt(generalVal, setInline(mapSpec simpleSpec) generalProps)
                    end
            end
    end (* genCode *)


    (* Constructor functions for the front-end of the compiler. *)
    local
        fun mkSimpleFunction inlineType (lval, args, name, closure, numLocals) =
              {
                body          = lval,
                isInline      = inlineType,
                name          = if name = "" then "<anon>" else name,
                closure       = map deExtract closure,
                argTypes      = List.tabulate(args, fn _ => (GeneralType, [])),
                resultType    = GeneralType,
                localCount    = numLocals,
                recUse        = []
              }
    in
        val mkProc = Lambda o mkSimpleFunction DontInline (* Normal function *)
        and mkInlproc = Lambda o mkSimpleFunction InlineAlways (* Explicitly inlined by the front-end *)

        (* Unless Compiler.inlineFunctor is false functors are treated as macros and expanded
           when they are applied.  Unlike core-language functions they are not first-class
           values so if they are inline the "value" returned in the initial binding can just
           be zero except if there is something in the closure. Almost always
           the closure will be empty since free variables will come from previous topdecs and will
           be constants,  The exception is if a structure and a functor using the structure appear
           in the same topdec (no semicolon between them).  In that case we can't leave it.  We
           would have to update the closure even if we leave the body untouched but we could
           have closure entries that are constants.
           e.g. structure S = struct val x = 1 end functor F() = struct open S end *)
        fun mkMacroProc (args as (_, _, _, [], _)) =
            Constnt(toMachineWord 0,
                setInline (
                    EnvSpecInlineFunction(mkSimpleFunction InlineAlways args,
                        fn _ => raise InternalError "mkMacroProc: closure")) [])

        |   mkMacroProc args = Lambda(mkSimpleFunction InlineAlways args)
    end

    local
        fun mkFunWithTypes inlineType { body, argTypes=argsAndTypes, resultType, name, closure, numLocals } =
            Lambda
            {
                body          = body,
                isInline      = inlineType,
                name          = if name = "" then "<anon>" else name,
                closure       = map deExtract closure,
                argTypes      = map (fn t => (t, [])) argsAndTypes,
                resultType    = resultType,
                localCount    = numLocals,
                recUse        = []
            }
    in
        val mkFunction = mkFunWithTypes DontInline
        and mkInlineFunction = mkFunWithTypes InlineAlways
    end

    fun mkEval (ct, clist)   =
    Eval {
        function = ct,
        argList = List.map(fn c => (c, GeneralType)) clist,
        resultType=GeneralType
    }

    fun mkCall(func, argsAndTypes, resultType) =
    Eval {
        function = func,
        argList = argsAndTypes,
        resultType=resultType
    }

    (* Basic built-in operations. *)
    fun mkUnary (oper, arg1) = Unary { oper = oper, arg1 = arg1 }
    and mkBinary (oper, arg1, arg2) =
        Binary { oper = oper, arg1 = arg1, arg2 = arg2 }

    val getCurrentThreadId = Nullary{oper=BuiltIns.GetCurrentThreadId}
    val getCurrentThreadIdFn =
        mkInlproc(getCurrentThreadId, 1 (* Ignores argument *), "GetThreadId()", [], 0)

    val cpuPause = Nullary{oper=BuiltIns.CPUPause}
    val cpuPauseFn =
        mkInlproc(cpuPause, 1 (* Ignores argument *), "CPUPause()", [], 0)


    val createMutex = Nullary{oper=BuiltIns.CreateMutex}
    val createMutexFn =
        mkInlproc(createMutex, 1 (* Ignores argument *), "CreateMutex()", [], 0)
    
    fun mkAllocateWordMemory (numWords, flags, initial) =
        AllocateWordMemory { numWords = numWords, flags = flags, initial = initial }
    val mkAllocateWordMemoryFn =
        mkInlproc(
            mkAllocateWordMemory(mkInd(0, mkLoadArgument 0), mkInd(1, mkLoadArgument 0), mkInd(2, mkLoadArgument 0)),
            1, "AllocateWordMemory()", [], 0)

    (* Builtins wrapped as functions.  N.B.  These all take a single argument which may be a tuple. *)
    fun mkUnaryFn oper =
        mkInlproc(mkUnary(oper, mkLoadArgument 0), 1, BuiltIns.unaryRepr oper ^ "()", [], 0)
    and mkBinaryFn oper =
        mkInlproc(mkBinary(oper, mkInd(0, mkLoadArgument 0), mkInd(1, mkLoadArgument 0)), 1, BuiltIns.binaryRepr oper ^ "()", [], 0)
        
    local
        open BuiltIns
        (* Word equality.  The value of isSigned doesn't matter. *)
        val eqWord = WordComparison{test=TestEqual, isSigned=false}
    in
        fun mkNot arg = Unary{oper=NotBoolean, arg1=arg}
        and mkIsShort arg = Unary{oper=IsTaggedValue, arg1=arg}
        and mkEqualTaggedWord (arg1, arg2) =
            Binary{oper=eqWord, arg1=arg1, arg2=arg2}
        and mkEqualPointerOrWord (arg1, arg2) =
            Binary{oper=PointerEq, arg1=arg1, arg2=arg2}
        val equalTaggedWordFn = (* This takes two words, not a tuple. *)
            mkInlproc(mkBinary(eqWord, mkLoadArgument 0, mkLoadArgument 1), 2, "EqualWord()", [], 0)
        and equalPointerOrWordFn = (* This takes two words, not a tuple. *)
            mkInlproc(mkBinary(PointerEq, mkLoadArgument 0, mkLoadArgument 1), 2, "EqualWord()", [], 0)
    end

    fun mkLoadOperation(oper, base, index) =
        LoadOperation{kind=oper, address={base=base, index=SOME index, offset=0}}

    fun mkLoadOperationFn oper =
        mkInlproc(mkLoadOperation(oper, mkInd(0, mkLoadArgument 0), mkInd(1, mkLoadArgument 0)), 1,
            "loadOperation()", [], 0)

    fun mkStoreOperation(oper, base, index, value) =
        StoreOperation{kind=oper, address={base=base, index=SOME index, offset=0}, value=value}

    fun mkStoreOperationFn oper =
        mkInlproc(mkStoreOperation(oper, mkInd(0, mkLoadArgument 0), mkInd(1, mkLoadArgument 0), mkInd(2, mkLoadArgument 0)), 1,
            "storeOperation()", [], 0)

    fun mkBlockOperation {kind, leftBase, leftIndex, rightBase, rightIndex, length } =
        BlockOperation { kind = kind,
            sourceLeft={base=leftBase, index=SOME leftIndex, offset=0},
            destRight={base=rightBase, index=SOME rightIndex, offset=0}, length=length}

    (* Construct a function that takes five arguments.  The order is left-base, right-base, left-index, right-index, length. *)
    fun mkBlockOperationFn kind =
        mkInlproc(
            mkBlockOperation{kind=kind, leftBase=mkInd(0, mkLoadArgument 0), rightBase=mkInd(1, mkLoadArgument 0),
                  leftIndex=mkInd(2, mkLoadArgument 0), rightIndex=mkInd(3, mkLoadArgument 0), length=mkInd(4, mkLoadArgument 0)}, 1,
            "blockOperation()", [], 0)

    fun identityFunction (name : string) : codetree = 
        mkInlproc (mkLoadArgument 0, 1, name, [], 0) (* Returns its argument. *);
  
    (* Test a tag value. *)
    fun mkTagTest(test: codetree, tagValue: word, maxTag: word) =
        TagTest {test=test, tag=tagValue, maxTag=maxTag }

    fun mkHandle (exp, handler, exId) =
        Handle {exp = exp, handler = handler, exPacketAddr = exId}

    fun mkStr (strbuff:string) = Constnt (toMachineWord strbuff, [])

  (* If we have multiple references to a piece of code we may have to save
     it in a temporary and then use it from there. If the code has side-effects
      we certainly must do that to ensure that the side-effects are done
      exactly once and in the correct order, however if the code is just a
      constant or a load we can reduce the amount of code we generate by
      simply returning the original code. *)
    fun multipleUses (code as Constnt _, _, _) = 
        {load = (fn _ => code), dec = []}
(*
    |   multipleUses (code as Extract(LoadLegacy{addr, level=loadLevel, ...}), _, level) = 
        let (* May have to adjust the level. *)
            fun loadFn lev =
                if lev = level
                then code 
                else mkLoad (addr, loadLevel + lev, level))
        in
            {load = loadFn, dec = []}
        end

    |   multipleUses (code as Extract(LoadLocal addr), _, level) = 
        let (* May have to adjust the level. *)
            fun loadFn lev =
                if lev = level
                then code 
                else mkLoad (addr, lev - level)
        in
            {load = loadFn, dec = []}
        end

    |   multipleUses (code as Extract(LoadArgument _), _, level) = 
        let (* May have to adjust the level. *)
            fun loadFn lev =
                if lev = level
                then code
                else raise InternalError "multipleUses: different level"
                (*else mkLoad (addr, lev - level)*)
        in
            {load = loadFn, dec = []}
        end

    |   multipleUses (Extract _, _, _) = raise InternalError "multipleUses: TODO"
*)
    |   multipleUses (code, nextAddress, level) = 
        let
            val addr       = nextAddress();
            fun loadFn lev = mkLoad (addr, lev, level);
        in
            {load = loadFn, dec = [mkDec (addr, code)]}
        end (* multipleUses *);

    fun mkMutualDecs [] = raise InternalError "mkMutualDecs: empty declaration list"
    |   mkMutualDecs l =
    let
        fun convertDec(a, Lambda lam) = {lambda = lam, addr = a, use=[]}
        |   convertDec _ = raise InternalError "mkMutualDecs: Recursive declaration is not a function"
    in
        RecDecs(List.map convertDec l)
    end

    val mkNullDec = NullBinding
    
    fun mkContainer(addr, size, setter) =
        Container{addr=addr, size=size, use=[], setter=setter}

    val mkIf                = Cond
    and mkRaise             = Raise

    fun mkConst v = Constnt(v, [])

    (* For the moment limit these to general arguments. *)
    fun mkLoop args = Loop (List.map(fn c => (c, GeneralType)) args)
    and mkBeginLoop(exp, args) =
        BeginLoop{loop=exp, arguments=List.map(fn(i, v) => ({value=v, addr=i, use=[]}, GeneralType)) args}

    fun mkWhile(b, e) = (* Generated as   if b then (e; <loop>) else (). *)
        mkBeginLoop(mkIf(b, mkEnv([NullBinding e], mkLoop[]), CodeZero), [])

    (* We previously had conditional-or and conditional-and as separate
       instructions.  I've taken them out since they can be implemented
       just as efficiently as a normal conditional.  In addition they
       were interfering with the optimisation where the second expression
       contained the last reference to something.  We needed to add a
       "kill entry" to the other branch but there wasn't another branch
       to add it to.   DCJM 7/12/00. *)
    fun mkCor(xp1, xp2)  = mkIf(xp1, CodeTrue, xp2);
    fun mkCand(xp1, xp2)  = mkIf(xp1, xp2, CodeZero);

    val mkSetContainer =
        fn (container, tuple, size) => mkSetContainer(container, tuple, BoolVector.tabulate(size, fn _ => true))

    (* We don't generate the +, -, < etc operations directly here.  Instead we create functions
       that the basis library can use to create the final versions by applying these functions
       to the arguments and an RTS function.  The inline expansion system takes care of all the
       optimisation. 
       An arbitrary precision operation takes a tuple consisting of a pair of arguments and
       a function.  The code that is constructed checks both arguments to see if they are
       short.  If they are not or the short precision operation overflows the code to
       call the function is executed. *)
    local
        val argX = mkInd(0, mkLoadArgument 0) and argY = mkInd(1, mkLoadArgument 0)
        val testShort = mkCand(mkIsShort argX, mkIsShort argY)
        val longCall = mkEval(mkInd(2, mkLoadArgument 0), [mkTuple[argX, argY]])
    in

        fun mkArbitraryFn (oper as ArbArith arith) =
            mkInlproc(
                Arbitrary{oper=oper, shortCond=testShort, arg1=argX, arg2=argY, longCall=longCall },
                1, "Arbitrary" ^  BuiltIns.arithRepr arith ^ "()", [], 0)

        |   mkArbitraryFn (oper as ArbCompare test) =
            (* The long function here is PolyCompareArbitrary which returns -1,0,+1 so the
               result has to be compared with zero. *)
            let
                val comparedResult =
                    Binary{oper=BuiltIns.WordComparison{test=test, isSigned=true}, arg1=longCall, arg2=CodeZero}
            in
                mkInlproc(
                    Arbitrary{oper=oper, shortCond=testShort, arg1=argX, arg2=argY, longCall=comparedResult },
                    1, "Arbitrary" ^ BuiltIns.testRepr test ^ "()", [], 0)
            end
    end

    val loadRecursive = Extract LoadRecursive

    structure Foreign = Backend.Foreign

    structure Sharing =
    struct
        type machineWord = machineWord
        type codetree    = codetree
        type pretty      = pretty
        type argumentType=argumentType
        type codeBinding     = codeBinding
        type level       = level
    end

end (* CODETREE functor body *);
