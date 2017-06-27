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
    
    type context =
    {
        locals: machineWord option array, closure: machineWord array,
        args: machineWord vector, recursion: machineWord
    }
    
    exception InternalError = Misc.InternalError
    
    val emptyVec : machineWord vector = Vector.fromList []

    val F_mutable_words : Word8.word = Word8.orb (F_words, F_mutable)
    
    val check = true

    (* We need different versions of this each number of arguments. *)
    
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
                                            Tuple {
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

    val function1 = createFunctionCode 1
    val function2 = createFunctionCode 2
    val function3 = createFunctionCode 3
    val function4 = createFunctionCode 4
    val function5 = createFunctionCode 5
    val function6 = createFunctionCode 6

    fun createFunction 1 = function1
    |   createFunction 2 = function2
    |   createFunction 3 = function3
    |   createFunction 4 = function4
    |   createFunction 5 = function5
    |   createFunction 6 = function6
    |   createFunction n = raise InternalError ("TODO: Function code for " ^ Int.toString n ^ " args")
    
    fun interpretCode (context as { locals, ...} : context) (Newenv(decs, exp)) =
        let
            fun processBinding(Declar{value, addr, ...}) =
                    Array.update(locals, addr, SOME(interpretCode context value))

            |   processBinding(RecDecs l) =
                let
                    (* Allocate each closure and set the values in the local table. *)
                    val closures =
                        List.map (fn {lambda={closure, ...}, ...} => Array.array(List.length closure, toMachineWord 0)) l
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

            |   processBinding(Container{addr, use, size, setter}) = raise Fail "TODO - Container"
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
            if check andalso (isShort b
                orelse not (Address.isWords (toAddress b))
                orelse Address.length (toAddress b) <= Word.fromInt offset)
            then raise InternalError "interpretCode - check"
            else loadWord (toAddress b, Word.fromInt offset)
        end

    |   interpretCode context (Eval { function, argList, ... }) =
        let
            val fVal = interpretCode context function
            val args = List.map(fn (c, _) => interpretCode context c) argList
        in
            RunCall.callCode(fVal, Vector.fromList args)
        end

    |   interpretCode _ GetThreadId = raise InternalError "TODO GetThreadId"

    |   interpretCode context (Unary { oper, arg1 }) =
            raise InternalError "TODO Unary"

    |   interpretCode context (Binary { oper, arg1, arg2 }) =
            raise InternalError "TODO Binary"

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

    |   interpretCode context (BeginLoop{loop, arguments}) =
            raise InternalError "TODO BeginLoop"

    |   interpretCode context (Loop l) =
            raise InternalError "TODO Loop"

    |   interpretCode context (Raise r) =
            raise InternalError "TODO Raise"

    |   interpretCode context (Handle{exp, handler, exPacketAddr}) =
            raise InternalError "TODO Handle"

    |   interpretCode context (Tuple { fields, ...}) =
        let
            val tupleSize = List.length fields
            val vec : address = allocWordData(Word.fromInt tupleSize, F_mutable_words, toMachineWord 0)
            fun setFields(field::fields, n) =
                (assignWord (vec, n, interpretCode context field); setFields(fields, n + 0w1))
            |   setFields([], _) = ()
        in
            setFields(fields, 0w0);
            lock vec;
            toMachineWord vec
        end

    |   interpretCode context (SetContainer{container, tuple, filter}) =
            raise InternalError "TODO SetContainer"

    |   interpretCode context (TagTest{test, tag, maxTag}) =
            raise InternalError "TODO TagTest"

    |   interpretCode context (LoadOperation{kind, address}) =
            raise InternalError "TODO LoadOperation"

    |   interpretCode context (StoreOperation{kind, address, value}) =
            raise InternalError "TODO SetContainer"

    |   interpretCode context (BlockOperation{kind, sourceLeft, destRight, length}) =
            raise InternalError "TODO SetContainer"

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
