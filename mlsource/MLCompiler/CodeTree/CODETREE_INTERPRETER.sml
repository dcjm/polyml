(*
    Copyright (c) 2017 David C.J. Matthews

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

(* Interpreter for codetree. *)

functor CODETREE_INTERPRETER(

    structure BASECODETREE: BaseCodeTreeSig

    structure BACKEND:
    sig
        type codetree
        type machineWord = Address.machineWord
        val codeGenerate:
            codetree * int * Universal.universal list -> (unit -> machineWord) * Universal.universal list
        structure Foreign: FOREIGNCALLSIG
        structure Sharing : sig type codetree = codetree end
    end
    
    sharing
        BASECODETREE.Sharing
    =   BACKEND.Sharing

): CodetreeInterpreterSig
=
struct
    open BASECODETREE
    open Address
    open BuiltIns
    
    val word0 = toMachineWord 0
    and word1 = toMachineWord 1
    
    type context =
    {
        locals: machineWord option array, closure: machineWord array,
        args: machineWord vector, recursion: machineWord
    }
    
    exception InternalError = Misc.InternalError
    
    val emptyVec : machineWord vector = Vector.fromList []

    val F_mutable_words : Word8.word = Word8.orb (F_words, F_mutable)
    
    exception LoopAgain of machineWord list

    (* We need different versions of this each number of arguments. *)
    local
        fun createFunctionCode nArgs =
        let
        
            val code =
                Lambda {
                    body =
                    Lambda {
                        body =
                            Eval {
                                function = Extract(LoadClosure 0),
                                argList = [
                                    (
                                        Tuple {
                                            fields = [
                                                Extract LoadRecursive,
                                                if nArgs = 0
                                                then Constnt(word0, [])
                                                else Tuple {
                                                    fields = List.tabulate(nArgs, fn n => Extract (LoadArgument n)),
                                                    isVariant = false
                                                }
                                            ],
                                            isVariant = false
                                        } ,
                                    GeneralType)
                                ],
                                resultType = GeneralType
                            },
                        isInline = NonInline,
                        name = "function1()()",
                        closure = [LoadArgument 0],
                        argTypes = List.tabulate (nArgs, fn _ => (GeneralType, [])),
                        resultType = GeneralType,
                        localCount = 0,
                        recUse = []
                        },
                    isInline = NonInline,
                    name = "function1()",
                    closure = [],
                    argTypes = [(GeneralType, [])], (* Argument is the interpreter function. *)
                    resultType = GeneralType,
                    localCount = 0,
                    recUse = []
                    }
            val (compileCode, _) = BACKEND.codeGenerate(code, 0, [])
            val buildCode = compileCode()
        in
            RunCall.unsafeCast buildCode : (machineWord * machineWord vector -> machineWord) -> machineWord -> machineWord
        end

        val functionTable = Vector.tabulate(20, fn i => createFunctionCode i)

    in
        fun createFunction n =
            Vector.sub(functionTable, n)
                handle Subscript => raise InternalError ("TODO: Function code for " ^ Int.toString n ^ " args")
    end
    
    fun interpretCode (context as { locals, ...} : context) (Newenv(decs, exp)) =
        let
            fun processBinding(Declar{value, addr, ...}) =
                    Array.update(locals, addr, SOME(interpretCode context value))

            |   processBinding(RecDecs l) =
                let
                    (* Allocate each closure and set the values in the local table. *)
                    val closures =
                        List.map (fn {lambda={closure, ...}, ...} => Array.array(List.length closure, word0)) l
                    fun createFunction({addr, lambda={argTypes, body, localCount, ...}, ...}, clArray) =
                    let
                        val fnCode = toMachineWord(getFunctionCode(List.length argTypes) (localCount, clArray, body))
                    in
                        Array.update(locals, addr, SOME fnCode)
                    end
                    val _ = ListPair.app createFunction (l, closures)
                    (* Reprocess the list, filling in the closure. *)
                    fun completeClosure({lambda={closure, ...}, ...}, clArray) =
                    let
                        fun fillClosure(clItem :: clRest, n) =
                            (Array.update(clArray, n, interpretExtract context clItem); fillClosure(clRest, n+1))
                        |   fillClosure _ = ()
                    in
                        fillClosure(closure, 0)
                    end
                    val _ = ListPair.app completeClosure (l, closures)
                in
                    ()
                end

            |   processBinding(NullBinding exp) = ignore (interpretCode context exp)

            |   processBinding(Container{addr, size, setter, ...}) =
                let
                    val vec : address = allocWordData(Word.fromInt size, F_mutable_words, word0)
                    val () = Array.update(locals, addr, SOME(toMachineWord vec))
                in
                    ignore (interpretCode context setter)
                end
        in
            List.app processBinding decs;
            interpretCode context exp
        end

    |   interpretCode _ (Constnt (m, _)) = m

    |   interpretCode context (Extract load) = interpretExtract context load
    
    |   interpretCode context (Indirect { base, offset, ... }) =
        let
            val b = interpretCode context base
        in
            loadWord (toAddress b, Word.fromInt offset)
        end

    |   interpretCode context (Eval { function, argList, ... }) =
        let
            val fVal = interpretCode context function
            (* TODO: If we have compiled a functor with inlineFunctor set then
               the constant value will be zero.  This will only happen if the
               functor was not compiled with optimisation off but the application
               of the functor was. *)
            val args = List.map(fn (c, _) => interpretCode context c) argList
            val _ = isShort fVal andalso raise InternalError "Eval - not a function"
        in
            RunCall.callCode(fVal, Vector.fromList args)
        end

    |   interpretCode _ GetThreadId = raise InternalError "TODO GetThreadId"

    |   interpretCode context (Unary { oper=NotBoolean, arg1 }) =
            if PolyML.pointerEq(interpretCode context arg1, word1)
            then word0
            else word1

    |   interpretCode context (Unary { oper=IsTaggedValue, arg1 }) =
            if isShort(interpretCode context arg1)
            then word1
            else word0

    |   interpretCode context (Unary { oper, arg1 }) =
            raise InternalError ("TODO Unary - " ^ unaryRepr oper)

    |   interpretCode context (Binary { oper=WordComparison{test=TestEqual, isSigned=false}, arg1, arg2 }) =
        let
            (* This is used for pointer equality as well as unsigned word. *)
            val arg1Val = interpretCode context arg1
            and arg2Val = interpretCode context arg2
        in
            if PolyML.pointerEq(arg1Val, arg2Val)
            then word1
            else word0
        end

    |   interpretCode context (Binary { oper, arg1, arg2 }) =
            raise InternalError ("TODO Binary - " ^ binaryRepr oper)

    |   interpretCode context (Arbitrary { oper, shortCond, arg1, arg2, longCall }) =
            raise InternalError "TODO Arbitrary"

    |   interpretCode context (AllocateWordMemory { numWords, flags, initial }) =
        let
            val wordCount = interpretCode context numWords
            and flagWord = interpretCode context flags
            and initVal = interpretCode context initial
        in
            RunCall.allocateWordMemory(toShort wordCount, toShort flagWord, initVal)
        end

    |   interpretCode context (Lambda { body, closure, argTypes, localCount, ... }) =
        let
            val clArray = Array.fromList(List.map (interpretExtract context) closure)
        in
            toMachineWord(getFunctionCode(List.length argTypes) (localCount, clArray, body))
        end

    |   interpretCode context (Cond(i, t, e)) =
        let
            val test = interpretCode context i
        in
            if isShort test andalso toShort test = 0w0
            then interpretCode context e
            else interpretCode context t
        end

    |   interpretCode (context as { locals, ...}) (BeginLoop{loop, arguments}) =
        let
            val () =
                List.app(fn ({addr, value, ...}, _) => Array.update(locals, addr, SOME(interpretCode context value)))
                    arguments
            fun runLoop () =
                interpretCode context loop
                    handle LoopAgain args =>
                    (
                        ListPair.app(fn (({addr, ...}, _), argVal) =>
                                Array.update(locals, addr, SOME argVal))
                            (arguments, args);
                        runLoop()
                    )
        in
            runLoop()
        end

    |   interpretCode context (Loop l) =
            raise LoopAgain (List.map (fn (v, _) => interpretCode context v) l)

    |   interpretCode context (Raise r) =
        let
            val packet: exn = RunCall.unsafeCast(interpretCode context r)
        in
            PolyML.Exception.reraise packet
        end

    |   interpretCode (context as {locals, ...}) (Handle{exp, handler, exPacketAddr}) =
        (
            interpretCode context exp
                handle exn =>
                let
                    val () = Array.update(locals, exPacketAddr, SOME(toMachineWord exn))
                in
                    interpretCode context handler
                end
        )

    |   interpretCode context (Tuple { fields, ...}) =
        let
            val tupleSize = List.length fields
            val vec : address = allocWordData(Word.fromInt tupleSize, F_mutable_words, word0)
            fun setFields(field::fields, n) =
                (assignWord (vec, n, interpretCode context field); setFields(fields, n + 0w1))
            |   setFields([], _) = ()
        in
            setFields(fields, 0w0);
            lock vec;
            toMachineWord vec
        end

    |   interpretCode context (SetContainer{container, tuple, filter}) =
        let
            (* Generally the source is a tuple and the code-generator
               recognises this as a special case.  It's not worth it here. *)
            val cAddr = toAddress(interpretCode context container)
            val sourceAddr = toAddress(interpretCode context tuple)
            val filterLength = BoolVector.length filter

            fun copyContainer(sourceWord, destWord) =
                if sourceWord = filterLength
                then ()
                else if BoolVector.sub(filter, sourceWord)
                then
                let
                    val fieldVal = loadWord (sourceAddr, Word.fromInt sourceWord)
                in
                    assignWord (cAddr, destWord, fieldVal);
                    copyContainer(sourceWord+1, destWord+0w1)
                end
                else copyContainer(sourceWord+1, destWord)
            val () = copyContainer(0, 0w0)
        in
            word0 (* Not used. *)
        end

    |   interpretCode context (TagTest{test, tag, ...}) =
        let
            val tVal = interpretCode context test
        in
            if PolyML.pointerEq(tVal, toMachineWord tag)
            then word1
            else word0
        end

    |   interpretCode context (LoadOperation{kind=LoadStoreMLWord _, address={base, index, offset}}) =
        let
            val bAddr = toAddress(interpretCode context base)
            val iAddr =
                case index of NONE => 0w0 | SOME ndx => toShort(interpretCode context ndx)
        in
            loadWord(bAddr, iAddr+offset)
        end

    |   interpretCode context (LoadOperation{kind=LoadStoreMLByte _, address}) =
            raise InternalError "TODO LoadOperation - LoadStoreMLByte"

    |   interpretCode context (LoadOperation{kind, address}) =
            raise InternalError "TODO LoadOperation"

    |   interpretCode context (StoreOperation{kind=LoadStoreMLWord _, address={base, index, offset}, value}) =
        let
            val bAddr = toAddress(interpretCode context base)
            val iAddr =
                case index of NONE => 0w0 | SOME ndx => toShort(interpretCode context ndx)
            val vVal = interpretCode context value
            val () = assignWord(bAddr, iAddr+offset, vVal)
        in
            word0
        end

    |   interpretCode context (StoreOperation{kind=LoadStoreMLByte _, address, value}) =
            raise InternalError "TODO StoreOperation - LoadStoreMLByte"

    |   interpretCode context (StoreOperation{kind, address, value}) =
            raise InternalError "TODO StoreOperation"

    |   interpretCode context (BlockOperation{kind, sourceLeft, destRight, length}) =
            raise InternalError "TODO BlockOperation"

    and interpretExtract { args, ...} (LoadArgument a) = Vector.sub(args, a)
    |   interpretExtract { locals, ...} (LoadLocal addr) = valOf(Array.sub(locals, addr))
    |   interpretExtract { closure, ...} (LoadClosure a) = Array.sub(closure, a)
    |   interpretExtract { recursion, ...} LoadRecursive = recursion


    and getFunctionCode n (nLocals, closure, body) =
        createFunction n
            (fn (recCall, argVec) =>
                interpretCode { closure=closure, locals=Array.array(nLocals, NONE),
                                args=argVec, recursion = recCall} body)

    fun norecursion _ = raise InternalError "norecursion"

    fun interpret (code, numLocals) =
        interpretCode { closure=Array.fromList[], locals=Array.array(numLocals, NONE), args=emptyVec,
                        recursion = toMachineWord norecursion } code

end;
