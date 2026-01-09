(*
    Copyright (c) 2012, 2016-23, 2026 David C.J. Matthews

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

(* Intermediate code tree for the back end of the compiler. *)

structure BackendIntermediateCode: BACKENDINTERMEDIATECODE =
struct
    open Address
    
    structure BuiltIns =
    struct
        datatype testConditions =
            TestEqual
        |   TestLess
        |   TestLessEqual
        |   TestGreater
        |   TestGreaterEqual
        |   TestUnordered (* Reals only. *)

        datatype arithmeticOperations =
            ArithAdd
        |   ArithSub
        |   ArithMult
        |   ArithQuot
        |   ArithRem
        |   ArithDiv
        |   ArithMod

        datatype logicalOperations =
            LogicalAnd
        |   LogicalOr
        |   LogicalXor
    
        datatype shiftOperations =
            ShiftLeft
        |   ShiftRightLogical
        |   ShiftRightArithmetic

        datatype unaryOps =
            NotBoolean
        |   IsTaggedValue
        |   MemoryCellLength
        |   MemoryCellFlags
        |   ClearMutableFlag
        |   LongWordToTagged
        |   SignedToLongWord
        |   UnsignedToLongWord
        |   RealAbs of precision
        |   RealNeg of precision
        |   RealFixedInt of precision
        |   FloatToDouble
        |   DoubleToFloat
        |   RealToInt of precision * IEEEReal.rounding_mode
        |   TouchAddress
        |   AllocCStack
        |   LockMutex
        |   TryLockMutex
        |   UnlockMutex
        |   Log2Word

        and precision = PrecSingle | PrecDouble

        and binaryOps =
            WordComparison of { test: testConditions, isSigned: bool }
        |   FixedPrecisionArith of arithmeticOperations
        |   WordArith of arithmeticOperations
        |   WordLogical of logicalOperations
        |   WordShift of shiftOperations
        |   AllocateByteMemory
        |   LargeWordComparison of testConditions
        |   LargeWordArith of arithmeticOperations
        |   LargeWordLogical of logicalOperations
        |   LargeWordShift of shiftOperations
        |   RealComparison of testConditions * precision
        |   RealArith of arithmeticOperations * precision
        |   PointerEq
        |   FreeCStack
        |   GeneralEquality

        and nullaryOps =
            GetCurrentThreadId
        |   CPUPause
        |   CreateMutex

        fun unaryRepr NotBoolean = "NotBoolean"
        |   unaryRepr IsTaggedValue = "IsTaggedValue"
        |   unaryRepr MemoryCellLength = "MemoryCellLength"
        |   unaryRepr MemoryCellFlags = "MemoryCellFlags"
        |   unaryRepr ClearMutableFlag = "ClearMutableFlag"
        |   unaryRepr LongWordToTagged = "LongWordToTagged"
        |   unaryRepr SignedToLongWord = "SignedToLongWord"
        |   unaryRepr UnsignedToLongWord = "UnsignedToLongWord"
        |   unaryRepr (RealAbs prec) = "RealAbs" ^ precRepr prec
        |   unaryRepr (RealNeg prec) = "RealNeg" ^ precRepr prec
        |   unaryRepr (RealFixedInt prec) = "RealFixedInt" ^ precRepr prec
        |   unaryRepr FloatToDouble = "FloatToDouble"
        |   unaryRepr DoubleToFloat = "DoubleToFloat"
        |   unaryRepr (RealToInt (prec, mode)) = "RealToInt" ^ precRepr prec ^ rndModeRepr mode
        |   unaryRepr TouchAddress = "TouchAddress"
        |   unaryRepr AllocCStack = "AllocCStack"
        |   unaryRepr LockMutex = "LockMutex"
        |   unaryRepr TryLockMutex = "TryLockMutex"
        |   unaryRepr UnlockMutex = "UnlockMutex"
        |   unaryRepr Log2Word = "Log2Word"

        and binaryRepr (WordComparison{test, isSigned}) =
                "Test" ^ (testRepr test) ^ (if isSigned then "Signed" else "Unsigned")
        |   binaryRepr (FixedPrecisionArith arithOp) = (arithRepr arithOp) ^ "Fixed"
        |   binaryRepr (WordArith arithOp) =  (arithRepr arithOp) ^ "Word"
        |   binaryRepr (WordLogical logOp) =  (logicRepr logOp) ^ "Word"
        |   binaryRepr (WordShift shiftOp) =  (shiftRepr shiftOp) ^ "Word"
        |   binaryRepr AllocateByteMemory = "AllocateByteMemory"
        |   binaryRepr (LargeWordComparison test) = "Test" ^ (testRepr test) ^ "LargeWord"
        |   binaryRepr (LargeWordArith arithOp) =  (arithRepr arithOp) ^ "LargeWord"
        |   binaryRepr (LargeWordLogical logOp) =  (logicRepr logOp) ^ "LargeWord"
        |   binaryRepr (LargeWordShift shiftOp) =  (shiftRepr shiftOp) ^ "LargeWord"
        |   binaryRepr (RealComparison (test, prec)) = "Test" ^ testRepr test ^ precRepr prec
        |   binaryRepr (RealArith (arithOp, prec)) = arithRepr arithOp ^ precRepr prec
        |   binaryRepr PointerEq = "PointerEq"
        |   binaryRepr FreeCStack = "FreeCStack"
        |   binaryRepr GeneralEquality = "GeneralEquality"
        
        and nullaryRepr GetCurrentThreadId = "GetCurrentThreadId"
        |   nullaryRepr CPUPause = "CPUPause"
        |   nullaryRepr CreateMutex = "CreateMutex"
        
        and testRepr TestEqual          = "Equal"
        |   testRepr TestLess           = "Less"
        |   testRepr TestLessEqual      = "LessEqual"
        |   testRepr TestGreater        = "Greater"
        |   testRepr TestGreaterEqual   = "GreaterEqual"
        |   testRepr TestUnordered      = "Unordered"
        
        and arithRepr ArithAdd          = "Add"
        |   arithRepr ArithSub          = "Sub"
        |   arithRepr ArithMult         = "Mult"
        |   arithRepr ArithQuot         = "Quot"
        |   arithRepr ArithRem          = "Rem"
        |   arithRepr ArithDiv          = "Div"
        |   arithRepr ArithMod          = "Mod"

        and logicRepr LogicalAnd        = "And"
        |   logicRepr LogicalOr         = "Or"
        |   logicRepr LogicalXor        = "Xor"
        
        and shiftRepr ShiftLeft         = "Left"
        |   shiftRepr ShiftRightLogical = "RightLogical"
        |   shiftRepr ShiftRightArithmetic = "RightArithmetic"
        
        and precRepr PrecSingle         = "Single"
        |   precRepr PrecDouble         = "Double"

        and rndModeRepr IEEEReal.TO_NEAREST = "Round"
        |   rndModeRepr IEEEReal.TO_NEGINF = "Down"
        |   rndModeRepr IEEEReal.TO_POSINF = "Up"
        |   rndModeRepr IEEEReal.TO_ZERO = "Trunc"

    end

    datatype argumentType =
        GeneralType
    |   DoubleFloatType
    |   SingleFloatType
    |   ContainerType of int

    datatype backendIC =
        BICNewenv of bicCodeBinding list * backendIC (* Set of bindings with an expression. *)

    |   BICConstnt of machineWord * Universal.universal list (* Load a constant *)

    |   BICExtract of bicLoadForm (* Get a local variable, an argument or a closure value *)

    |   BICField of {base: backendIC, offset: int }
         (* Load a field from a tuple or record *)
    
    |   BICEval of (* Evaluate a function with an argument list. *)
        {
            function:  backendIC,
            argList:   (backendIC * argumentType) list,
            resultType: argumentType
        }

        (* Built-in functions. *)
    |   BICNullary of {oper: BuiltIns.nullaryOps}
    |   BICUnary of {oper: BuiltIns.unaryOps, arg1: backendIC}
    |   BICBinary of {oper: BuiltIns.binaryOps, arg1: backendIC, arg2: backendIC}
    
    |   BICArbitrary of
            {oper: BuiltIns.arithmeticOperations, shortCond: backendIC, arg1: backendIC, arg2: backendIC, longCall: backendIC}

    |   BICLambda of bicLambdaForm (* Lambda expressions. *)

    |   BICCond of backendIC * backendIC * backendIC (* If-then-else expression *)

    |   BICCase of (* Case expressions *)
        {
            cases   : backendIC option list, (* NONE means "jump to the default". *)
            test    : backendIC,
            default : backendIC,
            isExhaustive: bool,
            firstIndex: word
        }
    
    |   BICBeginLoop of (* Start of tail-recursive inline function. *)
        { loop: backendIC, arguments: (bicSimpleBinding * argumentType) list }

    |   BICLoop of (backendIC * argumentType) list (* Jump back to start of tail-recursive function. *)

    |   BICRaise of backendIC (* Raise an exception *)

    |   BICHandle of (* Exception handler. *) { exp: backendIC, handler: backendIC, exPacketAddr: int }

    |   BICTuple of backendIC list (* Tuple *)

    |   BICSetContainer of (* Copy a tuple to a container. *)
        {
            container: backendIC,
            tuple:     backendIC,
            filter:    BoolVector.vector
        }
    
    |   BICLoadContainer of {base: backendIC, offset: int } 

    |   BICTagTest of { test: backendIC, tag: word, maxTag: word }
    
    |   BICLoadOperation of { kind: loadStoreKind, address: bicAddress }
    
    |   BICStoreOperation of { kind: loadStoreKind, address: bicAddress, value: backendIC }
    
    |   BICBlockOperation of
            { kind: blockOpKind, sourceLeft: bicAddress, destRight: bicAddress, length: backendIC }

    |   BICAllocateWordMemory of {numWords: backendIC, flags: backendIC, initial: backendIC}

    and bicCodeBinding =
        BICDeclar  of bicSimpleBinding (* Make a local declaration or push an argument *)
    |   BICRecDecs of { addr: int, lambda: bicLambdaForm } list (* Set of mutually recursive declarations. *)
    |   BICNullBinding of backendIC (* Just evaluate the expression and discard the result. *)
    |   BICDecContainer of { addr: int, size: int } (* Create a container for a tuple on the stack. *)

    and caseType =
        CaseWord        (* Word or fixed-precision integer. *)
    |   CaseTag of word

    and bicLoadForm =
        BICLoadLocal of int (* Local binding *)
    |   BICLoadArgument of int (* Argument - 0 is first arg etc.*)
    |   BICLoadClosure of int (* Closure - 0 is first closure item etc *)
    |   BICLoadRecursive (* Recursive call *)

    and loadStoreKind =
        LoadStoreMLWord of {isImmutable: bool} (* Load/Store an ML word in an ML word cell. *)
    |   LoadStoreMLByte of {isImmutable: bool} (* Load/Store a byte, tagging and untagging as appropriate, in an ML byte cell. *)
    |   LoadStoreC8         (* Load/Store C values - The base address is a boxed SysWord.word value. *)
    |   LoadStoreC16
    |   LoadStoreC32
    |   LoadStoreC64
    |   LoadStoreCFloat
    |   LoadStoreCDouble
    |   LoadStoreUntaggedUnsigned
    |   LoadStorePolyWord of {isImmutable: bool}        (* Load/Store a PolyWord value to/from a large word *)
    |   LoadStoreNativeWord of {isImmutable: bool}      (* Load/Store a native word value to/from a large word *)

    and blockOpKind =
        BlockOpMove of {isByteMove: bool}
    |   BlockOpEqualByte
    |   BlockOpCompareByte

    withtype bicSimpleBinding = 
    { (* Declare a value or push an argument. *)
        value:      backendIC,
        addr:       int
    }

    and bicLambdaForm =
    { (* Lambda expressions. *)
        body          : backendIC,
        name          : string,
        closure       : bicLoadForm list,
        argTypes      : argumentType list,
        resultType    : argumentType,
        localCount    : int
    }

    and bicAddress =
        (* Address form used in loads, store and block operations.  The base is an ML
           address if this is to/from ML memory or a (boxed) SysWord.word if it is
           to/from C memory.  The index is a value in units of the size of the item
           being loaded/stored and the offset is always in bytes. *)
        {base: backendIC, index: backendIC option, offset: int}

    structure CodeTags =
    struct
        open Universal
        val tupleTag: universal list list tag = tag()

        fun splitProps _ [] = (NONE, [])
        |   splitProps tag (hd::tl) =
                if Universal.tagIs tag hd
                then (SOME hd, tl)
                else let val (p, l) = splitProps tag tl in (p, hd :: l) end

        fun mergeTupleProps(p, []) = p
        |   mergeTupleProps([], p) = p
        |   mergeTupleProps(m, n) =
            (
                case (splitProps tupleTag m, splitProps tupleTag n) of
                    ((SOME mp, ml), (SOME np, nl)) =>
                    let
                        val mpl = Universal.tagProject tupleTag mp
                        and npl = Universal.tagProject tupleTag np
                        val merge = ListPair.mapEq mergeTupleProps (mpl, npl)
                    in
                        Universal.tagInject tupleTag merge :: (ml @ nl)
                    end
                |   _ => m @ n
            )
    end
    
    fun loadStoreKindRepr(LoadStoreMLWord {isImmutable=true}) = "MLWordImmutable"
    |   loadStoreKindRepr(LoadStoreMLWord {isImmutable=false}) = "MLWord"
    |   loadStoreKindRepr(LoadStoreMLByte {isImmutable=true}) = "MLByteImmutable"
    |   loadStoreKindRepr(LoadStoreMLByte {isImmutable=false}) = "MLByte"
    |   loadStoreKindRepr LoadStoreC8 = "C8Bit"
    |   loadStoreKindRepr LoadStoreC16 = "C16Bit"
    |   loadStoreKindRepr LoadStoreC32 = "C32Bit"
    |   loadStoreKindRepr LoadStoreC64 = "C64Bit"
    |   loadStoreKindRepr LoadStoreCFloat = "CFloat"
    |   loadStoreKindRepr LoadStoreCDouble = "CDouble"
    |   loadStoreKindRepr LoadStoreUntaggedUnsigned = "MLWordUntagged"
    |   loadStoreKindRepr (LoadStorePolyWord {isImmutable=true}) = "PolyWordImmutable"
    |   loadStoreKindRepr (LoadStorePolyWord {isImmutable=false}) = "PolyWord"
    |   loadStoreKindRepr (LoadStoreNativeWord {isImmutable=true}) = "NativeWordImmutable"
    |   loadStoreKindRepr (LoadStoreNativeWord {isImmutable=false}) = "NativeWord"

    fun blockOpKindRepr (BlockOpMove{isByteMove=false}) = "MoveWord"
    |   blockOpKindRepr (BlockOpMove{isByteMove=true}) = "MoveByte"
    |   blockOpKindRepr BlockOpEqualByte = "EqualByte"
    |   blockOpKindRepr BlockOpCompareByte = "CompareByte"

    open Pretty

    fun pList ([]: 'b list, _: string, _: 'b->pretty) = []
    |   pList ([h],    _, disp) = [disp h]
    |   pList (h::t, sep, disp) =
        PrettyBlock (0, false, [],
            [
                disp h,
                PrettyBreak (0, 0),
                PrettyString sep
            ]
        ) ::
        PrettyBreak (1, 0) ::
        pList (t, sep, disp)

    fun pretty (pt : backendIC) : pretty =
    let
        
        fun printList(start, lst, sep) : pretty =
            PrettyBlock (1, true, [],
                PrettyString (start ^ "(") ::
                pList(lst, sep, pretty) @
                [ PrettyBreak (0, 0), PrettyString (")") ]
            )

        fun prettyArgType GeneralType = PrettyString "G"
        |   prettyArgType DoubleFloatType = PrettyString "D"
        |   prettyArgType SingleFloatType = PrettyString "F"
        |   prettyArgType (ContainerType m) = PrettyString ("M"^Int.toString m)
        
        fun prettyArg (c, t) =
                PrettyBlock(1, false, [], [pretty c, PrettyBreak (1, 0), prettyArgType t])

        fun prettyArgs(start, lst, sep) : pretty =
            PrettyBlock (1, true, [],
                PrettyString (start ^ "(") ::
                pList(lst, sep, prettyArg) @
                [ PrettyBreak (0, 0), PrettyString (")") ]
            )

        fun prettyAddress({base, index, offset}: bicAddress): pretty =
        let
        in
            PrettyBlock (1, true, [],
                [
                    PrettyString "[", PrettyBreak (0, 3),
                    pretty base,
                    PrettyBreak (0, 0), PrettyString ",", PrettyBreak (1, 0), 
                    case index of NONE => PrettyString "-" | SOME i => pretty i,
                    PrettyBreak (0, 0), PrettyString ",", PrettyBreak (1, 0),
                    PrettyString(Int.toString offset), PrettyBreak (0, 0), PrettyString "]"
                ])
        end

    in
        case pt of
            BICEval {function, argList, resultType} =>
            let
                val prettyArgs =
                    PrettyBlock (1, true, [],
                        PrettyString ("$(") ::
                        pList(argList, ",", prettyArg) @
                        [ PrettyBreak (0, 0), PrettyString (")") ]
                    )
            in
                PrettyBlock (3, false, [],
                    [ pretty function, PrettyBreak(1, 0), prettyArgType resultType, PrettyBreak(1, 0), prettyArgs ]
                )
            end

        |   BICUnary { oper, arg1 } =>
                PrettyBlock (3, false, [],
                    [ PrettyString(BuiltIns.unaryRepr oper), PrettyBreak(1, 0), printList("", [arg1], ",") ]
                )

        |   BICBinary { oper, arg1, arg2 } =>
                PrettyBlock (3, false, [],
                    [ PrettyString(BuiltIns.binaryRepr oper), PrettyBreak(1, 0), printList("", [arg1, arg2], ",") ]
                )

        |   BICNullary { oper } => PrettyString(BuiltIns.nullaryRepr oper)

        |   BICArbitrary { oper, shortCond, arg1, arg2, longCall } =>
                PrettyBlock (3, false, [],
                    [ PrettyString(BuiltIns.arithRepr oper), PrettyBreak(1, 0),
                        printList("", [shortCond, arg1, arg2, longCall], ",") ]
                )

        |   BICAllocateWordMemory { numWords, flags, initial } =>
                PrettyBlock (3, false, [],
                    [ PrettyString "AllocateWordMemory", PrettyBreak(1, 0), printList("", [numWords, flags, initial], ",") ]
                )

        |   BICExtract (BICLoadLocal addr) =>
            let
                val str : string =
                    concat ["LOCAL(", Int.toString addr, ")"]
            in
                PrettyString str
            end
         
        |   BICExtract (BICLoadArgument addr) =>
            let
                val str : string =
                    concat ["PARAM(", Int.toString addr, ")"]
            in
                PrettyString str
            end

        |   BICExtract (BICLoadClosure addr) =>
            let
                val str : string =
                    concat ["CLOS(", Int.toString addr, ")"]
            in
                PrettyString str
            end

        |   BICExtract (BICLoadRecursive) =>
            let
                val str : string =
                    concat ["RECURSIVE(", ")"]
            in
                PrettyString str
            end

        |   BICField {base, offset} =>
            let
                val str = "INDIRECT(" ^ Int.toString offset ^ ", ";
            in
                PrettyBlock(0, false, [],
                    [ PrettyString str, pretty base, PrettyString ")" ]
                )
            end
        
        |   BICLambda {body, name, closure, argTypes, resultType, localCount} =>
            let
                fun prettyArgTypes [] = []
                |   prettyArgTypes [last] = [prettyArgType last]
                |   prettyArgTypes (hd::tl) = prettyArgType hd :: PrettyBreak(1, 0) :: prettyArgTypes tl
            in
                PrettyBlock (1, true, [],
                    [
                        PrettyString ("LAMBDA("),
                        PrettyBreak (1, 0),
                        PrettyString name,
                        PrettyBreak (1, 0),
                        PrettyString (" LOCALS=" ^ Int.toString localCount),
                        PrettyBreak(1, 0),
                        PrettyBlock (1, false, [], PrettyString "ARGS=" :: prettyArgTypes argTypes),
                        PrettyBreak(1, 0),
                        PrettyBlock (1, false, [], [PrettyString "RES=", prettyArgType resultType]),
                        printList (" CLOS=", map BICExtract closure, ","),
                        PrettyBreak (1, 0),
                        pretty body,
                        PrettyString "){LAMBDA}"
                    ]
                )
            end
        
        |   BICConstnt (w, _) => PrettyString (stringOfWord w)
        
        |   BICCond (f, s, t) =>
            PrettyBlock (1, true, [],
                [
                    PrettyString "IF(",
                    pretty f,
                    PrettyString ", ",
                    PrettyBreak (0, 0),
                    pretty s,
                    PrettyString ", ",
                    PrettyBreak (0, 0),
                    pretty t,
                    PrettyBreak (0, 0),
                    PrettyString (")")
                ]
            )

        |   BICNewenv(decs, final) =>
            PrettyBlock (1, true, [],
                PrettyString ("BLOCK" ^ "(") ::
                pList(decs, ";", prettyBinding) @
                [ PrettyBreak (1, 0), pretty final, PrettyBreak (0, 0), PrettyString (")") ]
            )

        |   BICBeginLoop{loop=loopExp, arguments=args } =>
            let
                fun prettyArg (c, t) =
                    PrettyBlock(1, false, [],
                        [prettySimpleBinding c, PrettyBreak (1, 0), prettyArgType t])
            in
                PrettyBlock (3, false, [],
                    [
                        PrettyBlock (1, true, [],
                            PrettyString ("BEGINLOOP(") ::
                            pList(args, ",", prettyArg) @
                            [ PrettyBreak (0, 0), PrettyString (")") ]
                        ),
                        PrettyBreak (0, 0),
                        PrettyString "(",
                        PrettyBreak (0, 0),
                        pretty loopExp,
                        PrettyBreak (0, 0),
                        PrettyString ")"
                    ]
                )
            end
        
        |   BICLoop ptl => prettyArgs("LOOP", ptl, ",")
        
        |   BICRaise c =>
            PrettyBlock (1, true, [],
                [
                    PrettyString "RAISE(",
                    pretty c,
                    PrettyBreak (0, 0),
                    PrettyString (")")
                ]
            )
        
        |   BICHandle {exp, handler, exPacketAddr} =>
            PrettyBlock (3, false, [],
                [
                    PrettyString "HANDLE(",
                    pretty exp,
                    PrettyBreak (1, 0),
                    PrettyString ("WITH exid=" ^ Int.toString exPacketAddr),
                    PrettyBreak (1, 0),
                    pretty handler,
                    PrettyString ")"
                ]
            )

        |   BICCase {cases, test, default, isExhaustive, firstIndex, ...} =>
            PrettyBlock (1, true, [],
                PrettyString "CASE (" ::
                pretty test ::
                PrettyBreak (1, 0) ::
                PrettyString ("( from " ^ Word.toString firstIndex ^ (if isExhaustive then " exhaustive" else "")) ::
                PrettyBreak (1, 0) ::
                pList(cases, ",",
                    fn (SOME exp) =>
                        PrettyBlock (1, true, [],
                            [
                                PrettyString "=>",
                                PrettyBreak (1, 0),
                                pretty exp
                            ])
                    |   NONE => PrettyString "=> default"
                    ) @
                [
                    PrettyBreak (1, 0),
                    PrettyBlock (1, false, [],
                        [
                            PrettyString "ELSE:",
                            PrettyBreak (1, 0),
                            pretty default
                        ]
                    ),
                    PrettyBreak (1, 0), 
                    PrettyString (") {"^"CASE"^"}")
                ]
            )
         
        |   BICTuple ptl => printList("RECCONSTR", ptl, ",")
        
        |   BICSetContainer{container, tuple, filter} =>
            let
                val source = BoolVector.length filter
                val dest = BoolVector.foldl(fn (true, n) => n+1 | (false, n) => n) 0 filter
            in
                PrettyBlock (3, false, [],
                    [
                        PrettyString (concat["SETCONTAINER(", Int.toString dest, "/", Int.toString source, ", "]),
                        pretty container,
                        PrettyBreak (0, 0),
                        PrettyString ",",
                        PrettyBreak (1, 0),
                        pretty tuple,
                        PrettyBreak (0, 0),
                        PrettyString ")"
                    ]
                )
            end

        |   BICLoadContainer {base, offset} =>
            let
                val str = "INDIRECTCONTAINER(" ^ Int.toString offset ^ ", ";
            in
                PrettyBlock(0, false, [],
                    [ PrettyString str, pretty base, PrettyString ")" ]
                )
            end

        |   BICTagTest { test, tag, maxTag } =>
            PrettyBlock (3, false, [],
                [
                    PrettyString (concat["TAGTEST(", Word.toString tag, ", ", Word.toString maxTag, ","]),
                    PrettyBreak (1, 0),
                    pretty test,
                    PrettyBreak (0, 0),
                    PrettyString ")"
                ]
            )

        |   BICLoadOperation{ kind, address } =>
            PrettyBlock (3, false, [],
                [
                    PrettyString("Load" ^ loadStoreKindRepr kind),
                    PrettyBreak (1, 0),
                    prettyAddress address
                ]
            )

        |   BICStoreOperation{ kind, address, value } =>
            PrettyBlock (3, false, [],
                [
                    PrettyString("Store" ^ loadStoreKindRepr kind),
                    PrettyBreak (1, 0),
                    prettyAddress address,
                    PrettyBreak (1, 0),
                    PrettyString "<=",
                    PrettyBreak (1, 0),
                    pretty value
                ]
            )

        |   BICBlockOperation{ kind, sourceLeft, destRight, length } =>
            PrettyBlock (3, false, [],
                [
                    PrettyString(blockOpKindRepr kind ^ "("),
                    PrettyBreak (1, 0),
                    prettyAddress sourceLeft,
                    PrettyBreak (1, 0), PrettyString ",",
                    prettyAddress destRight,
                    PrettyBreak (1, 0), PrettyString ",",
                    pretty length,
                    PrettyBreak (1, 0), PrettyString ")"
                ]
            )

        (* That list should be exhaustive! *)
    end (* pretty *)

    and prettyBinding(BICDeclar dec) = prettySimpleBinding dec
       
    |   prettyBinding(BICRecDecs ptl) =
        let
            fun prettyRDec {lambda, addr} =
            PrettyBlock (1, false, [],
                [
                    PrettyString (concat ["DECL #", Int.toString addr, "="]),
                    PrettyBreak (1, 0),
                    pretty(BICLambda lambda)
                ]
            )
        in
            PrettyBlock (1, true, [],
                PrettyString ("MUTUAL" ^ "(") ::
                pList(ptl, " AND ", prettyRDec) @
                [ PrettyBreak (0, 0), PrettyString (")") ]
            )
        end

    |   prettyBinding(BICNullBinding c) = pretty c
        
    |   prettyBinding(BICDecContainer{addr, size}) =
            PrettyString (concat ["CONTAINER #", Int.toString addr, "=", Int.toString size])

    and prettySimpleBinding{value, addr} =
        PrettyBlock (1, false, [],
            [
                PrettyString (concat ["DECL #", Int.toString addr, "="]),
                PrettyBreak (1, 0),
                pretty value
            ]
        )

    structure Sharing =
    struct
        type backendIC = backendIC
        and  bicLoadForm = bicLoadForm
        and  caseType = caseType
        and  pretty = pretty
        and  argumentType = argumentType
        and  bicCodeBinding = bicCodeBinding
        and  bicSimpleBinding = bicSimpleBinding
        and  loadStoreKind = loadStoreKind
        and  blockOpKind = blockOpKind
        and  unaryOps = BuiltIns.unaryOps
        and  binaryOps = BuiltIns.binaryOps
        and  nullaryOps = BuiltIns.nullaryOps
        and  testConditions = BuiltIns.testConditions
        and  arithmeticOperations = BuiltIns.arithmeticOperations
    end

end;
