(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Updated David C.J. Matthews 2008-9, 2012, 2013, 2015-23, 2025

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
    Title:      Initialise ML Global Declarations.
    Author:     Dave Matthews,Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

functor INITIALISE_ (

    structure LEX: LEXSIG
    structure TYPETREE : TYPETREESIG
    structure STRUCTVALS : STRUCTVALSIG
    structure VALUEOPS : VALUEOPSSIG
    structure CODETREE : CODETREE
    structure EXPORTTREE: EXPORTTREESIG
    structure DATATYPEREP: DATATYPEREPSIG
    structure TYPEIDCODE: TYPEIDCODESIG
    structure MAKE: MAKESIG
    structure ADDRESS : AddressSig
    structure DEBUG: DEBUG
    structure DEBUGGER : DEBUGGER
    structure PRETTY : PRETTY

    structure MISC :
    sig
      val unescapeString : string -> string
      exception Conversion of string;     (* string to int conversion failure *)
    end

    structure VERSION:
    sig
       val compilerVersion: string
       val versionNumber: int
    end

    structure UNIVERSALTABLE:
    sig
        type universal = Universal.universal
        type univTable
        type 'a tag = 'a Universal.tag

        val univLookup: univTable * 'a tag * string -> 'a option

        val fold: (string * universal * 'a -> 'a) -> 'a -> univTable -> 'a
    end

    sharing STRUCTVALS.Sharing = VALUEOPS.Sharing = TYPETREE.Sharing = EXPORTTREE.Sharing
            = PRETTY.Sharing = CODETREE.Sharing = MAKE.Sharing = ADDRESS = DATATYPEREP.Sharing
            = TYPEIDCODE.Sharing = DEBUGGER.Sharing = LEX.Sharing = UNIVERSALTABLE
) : 

sig
  type gEnv
  val initGlobalEnv : {globalTable  : gEnv, intIsArbitraryPrecision: bool } -> unit
end =

struct
    open STRUCTVALS;
    open TYPETREE
    open VALUEOPS;
    open CODETREE;
    open ADDRESS;
    open MAKE;
    open MISC;
    open EXPORTTREE
    open DATATYPEREP

    val intInfType = mkTypeConstruction ("int",     intInfConstr,    [], [])
    and realType   = mkTypeConstruction ("real",    realConstr,   [], [])
    and charType   = mkTypeConstruction ("char",    charConstr,   [], [])
    and wordType   = mkTypeConstruction ("word",    wordConstr,   [], [])
    
    val declInBasis = [DeclaredAt inBasis]

    fun applyList _ []       = ()
    |   applyList f (h :: t) = (f h : unit; applyList f t);

    fun initGlobalEnv{globalTable  : gEnv, intIsArbitraryPrecision: bool } =
    let
        val Env globalEnv = MAKE.gEnvAsEnv globalTable
   
        val enterGlobalValue  = #enterVal  globalEnv;
        val enterGlobalType   = #enterType globalEnv;

        (* Some routines to help make the types. *)
        local
            (* careful - STRUCTVALS.intType differs from TYPETREE.intType *)
            open TYPETREE;
        in
            (* Make some type variables *)
            val tvA = BoundTypeVar("'a", TVIndex 0)
            and tvB = BoundTypeVar("'b", TVIndex 1)
            and tvC = BoundTypeVar("'c", TVIndex 2)
            and tvD = BoundTypeVar("'d", TVIndex 3)
            and tvE = BoundTypeVar("'e", TVIndex 4)
            and tvF = BoundTypeVar("'f", TVIndex 5)

            val defaultTemplate = TemplPlain { equality=false }
            fun templates n = List.tabulate(n, fn _ => defaultTemplate)

            (* Make some functions *)
            infixr 5 ->>
            fun a ->> b = mkFunctionType (a, b);
            
            infix 7 **;
            fun a ** b = mkProductType [a, b];
            
            (* Type identifiers for the types of the declarations. *)
            val Int    =
                if intIsArbitraryPrecision then intInfType else fixedIntType
            val String = stringType;
            val Bool   = boolType;
            val Unit   = unitType;
            val Char   = charType;
            val Word   = wordType;
            val Real   = realType
            val Exn    = exnType
            
            val mkTypeConstruction = mkTypeConstruction;
            
            val () = setPreferredInt(if intIsArbitraryPrecision then intInfConstr else fixedIntConstr)
        end;

        (* Function to make a type identifier with a pretty printer that just prints "?".
           None of the types are equality types so the equality function is empty. *)
        local
            fun monotypePrinter _ = PRETTY.PrettyString "?"
        in
            fun defaultEqAndPrintCode () =
                let
                    open TypeValue
                    val code =
                        createTypeValue{
                            eqCode = CodeZero, printCode = mkConst (toMachineWord (ref monotypePrinter))}
                in
                    Global (genCode(code, [], 0) ())
                end
        end

        fun makeTypeConstructor (name, uid, locations) =
            TypeConstrs
            {
                name       = name,
                identifier = uid,
                locations = locations
            }

        fun makeTypeAbbreviation(name, fullName, arity, typeResult, locations) =
            makeTypeConstructor(
                name, makeTypeFunction(arity, typeResult, basisDescription fullName),
                locations)

        (* Make an opaque type and add it to an environment. *)
        fun makeAndDeclareOpaqueType(typeName, fullName, env) =
        let
            val typeconstr =
                makeTypeConstructor(typeName,
                    makeFreeId(0, defaultEqAndPrintCode(), false, basisDescription fullName),
                    declInBasis);
        in
            #enterType env (typeName, TypeConstrSet(typeconstr, []));
            mkTypeConstruction (typeName, typeconstr, [], declInBasis)
        end;

        fun tsConstr(TypeConstrSet(ts, _)) = ts
        and tsConstructors(TypeConstrSet(_, tvs)) = tvs

        (* List of something *)
        fun List (base : types) : types =
            mkTypeConstruction ("list", tsConstr listConstr, [base], declInBasis);

        (* ref something *)
        fun Ref (base : types) : types  =
            mkTypeConstruction ("ref", refConstr, [base], declInBasis);
        
        fun Option (base : types) : types  =
            mkTypeConstruction ("option", tsConstr optionConstr, [base], declInBasis)

        (* Make a structure.  Returns the table as an
           environment so that entries can be added to the structure. *)
        fun makeStructure(parentEnv, name) =
        let
            val str as  Struct{signat=Signatures{tab, ...}, ...} = makeEmptyGlobal name
            val () = #enterStruct parentEnv (name, str)
            val Env env = makeEnv tab
        in
            env
        end

        val () = enterGlobalType ("unit", TypeConstrSet(unitConstr, []));

        local
            val falseCons =
                mkGconstr ("false", ValueType(Bool, []),
                    createNullaryConstructor(EnumForm{tag=0w0, maxTag=0w1}, "false"), true, 2, declInBasis)
            val trueCons  =
                mkGconstr ("true",  ValueType(Bool, []),
                    createNullaryConstructor(EnumForm{tag=0w1, maxTag=0w1}, "true"), true, 2, declInBasis)
            val boolEnv = makeStructure(globalEnv, "Bool") (* Bool structure *)
            val notFn =
                mkGvar("not", ValueType(Bool ->> Bool, []), mkUnaryFn BuiltIns.NotBoolean, declInBasis)
        in
            val () = #enterType boolEnv ("bool",  TypeConstrSet(boolConstr, [trueCons, falseCons]))
            val () = #enterVal boolEnv ("true",  trueCons)
            val () = #enterVal boolEnv ("false", falseCons)
            val () = #enterVal boolEnv ("not", notFn)
        end;
        
        val () = enterGlobalType ("int", TypeConstrSet(if intIsArbitraryPrecision then intInfConstr else fixedIntConstr, []))
        val () = enterGlobalType ("char", TypeConstrSet(charConstr, []))
        val () = enterGlobalType ("string", TypeConstrSet(stringConstr, []))
        val () = enterGlobalType ("real", TypeConstrSet(realConstr, []))

        val () = (* Enter :: and nil. *)
            List.app(fn(tv as Value{name, ...}) => enterGlobalValue(name, tv))
                (tsConstructors listConstr)
        val () = enterGlobalType  ("list", listConstr);

        val () = (* Enter NONE and SOME. *)
            List.app(fn(tv as Value{name, ...}) => enterGlobalValue(name, tv))
                (tsConstructors optionConstr)
        val () = enterGlobalType  ("option", optionConstr);

        local
            val refCons =
                    mkGconstr ("ref", ValueType(tvA ->> Ref tvA, templates 1),
                        createUnaryConstructor(RefForm, "ref"), false, 1, declInBasis)
        in
            val () = enterGlobalType  ("ref", TypeConstrSet(refConstr, [refCons]));
            val () = enterGlobalValue ("ref", refCons);
        end
        
        local
            open BuiltIns
            fun monoTypePrinter _ = PRETTY.PrettyString "?"

            val idCode =
                let
                    open TypeValue
                    val equalLongWordFn =
                        mkInlproc(
                            mkBinary(LargeWordComparison TestEqual, mkLoadArgument 0, mkLoadArgument 1), 2, "EqualLargeWord()", [], 0)
                    val code =
                        createTypeValue{
                            eqCode=equalLongWordFn,
                            printCode=mkConst (toMachineWord (ref monoTypePrinter)) }
                in
                   Global (genCode(code, [], 0) ())
                end
        in
            val largeWordType =
                makeTypeConstructor("word",
                    makeFreeId(0, idCode, true, basisDescription "LargeWord.word"), declInBasis)
            val LargeWord = mkTypeConstruction ("LargeWord.word", largeWordType, [], declInBasis)
        end

        val () = enterGlobalType ("exn", TypeConstrSet(exnConstr, []));

        val () = enterGlobalType ("word", TypeConstrSet(wordConstr, []));

        val runCallEnv = makeStructure(globalEnv, "RunCall")

        fun enterRunCall (name : string, entry : codetree, typ : types, templates) : unit =
        let
            val code = entry
            val value = mkGvar (name, ValueType(typ, templates), code, declInBasis);
        in
            #enterVal runCallEnv (name, value)
        end
  
        local
            (* unsafeCast.  Can be used to convert any value to any type. *)
            val unsafeCastEntry =
                mkInlproc (mkLoadArgument 0  (* just the parameter *), 1, "unsafeCast(1)", [], 0)

        in
            val () =
                enterRunCall ("unsafeCast", unsafeCastEntry, tvA ->> tvB, templates 2)
        end

        local
            open BuiltIns
        in
            (* isShort - test if a value is tagged rather than being an address. *)
            val () =
                enterRunCall ("isShort", mkUnaryFn IsTaggedValue, tvA ->> Bool, templates 1)
            (* pointer equality *)
            val () =
                enterRunCall ("pointerEq", mkBinaryFn PointerEq, tvA ** tvA ->> Bool, templates 1)
            (* load a word. The index is in words and is always zero or positive. *)
            val () = enterRunCall ("loadWord", mkLoadOperationFn(LoadStoreMLWord{isImmutable=false}), tvA ** Word ->> tvB, templates 2)
            (* Load a word from an immutable.  *)
            val () = enterRunCall ("loadWordFromImmutable", mkLoadOperationFn(LoadStoreMLWord{isImmutable=true}), tvA ** Word ->> tvB, templates 2)
            (* load a byte. The index is in bytes and is always zero or positive.  Probably the
               result should be a Word8.word value or a char. *)
            val () = enterRunCall ("loadByte", mkLoadOperationFn(LoadStoreMLByte{isImmutable=false}), tvA ** Word ->> tvB, templates 2)
            (* Load a byte from an immutable.  *)
            val () = enterRunCall ("loadByteFromImmutable", mkLoadOperationFn(LoadStoreMLByte{isImmutable=true}), tvA ** Word ->> tvB, templates 2)
            (* Load a 32-bit value into a large word *)
            val () = enterRunCall ("loadPolyWord", mkLoadOperationFn(LoadStorePolyWord{isImmutable=false}), tvA ** Word ->> LargeWord, templates 1)
            (* Load a 64-bit value into a large word *)
            val () = enterRunCall ("loadNativeWord", mkLoadOperationFn(LoadStoreNativeWord{isImmutable=false}), tvA ** Word ->> LargeWord, templates 1)
            (* Get the length of a heap cell. *)
            val () = enterRunCall ("memoryCellLength", mkUnaryFn MemoryCellLength, tvA ->> Word, templates 1)
            (* Return the flags.  Perhaps this could return a Word8.word value instead of a word. *)
            val () = enterRunCall ("memoryCellFlags", mkUnaryFn MemoryCellFlags, tvA ->> Word, templates 1)
            (* Return the number of bytes per word.  This is a constant since we have separate pre-built
               compilers for 32-bit and 64-bit.  N.B. The byte order is not a constant since we
               only have a single pre-built compiler for little-endian and big-endian interpreted code. *)
            val () = enterRunCall ("bytesPerWord", mkConst(toMachineWord wordSize), Word, templates 0)
            (* Store a word *)
            val () = enterRunCall ("storeWord", mkStoreOperationFn(LoadStoreMLWord{isImmutable=false}), mkProductType[tvA, Word, tvB] ->> Unit, templates 2)
            (* Store a byte *)
            val () = enterRunCall ("storeByte", mkStoreOperationFn(LoadStoreMLByte{isImmutable=false}), mkProductType[tvA, Word, tvB] ->> Unit, templates 2)
            (* Store a word without release semantics. *)
            val () = enterRunCall ("storeWordInitialise", mkStoreOperationFn(LoadStoreMLWord{isImmutable=true}), mkProductType[tvA, Word, tvB] ->> Unit, templates 2)
            (* Store a byte without release semantics *)
            val () = enterRunCall ("storeByteInitialise", mkStoreOperationFn(LoadStoreMLByte{isImmutable=true}), mkProductType[tvA, Word, tvB] ->> Unit, templates 2)
            (* Store a PolyWord value from a large word *)
            val () = enterRunCall ("storePolyWord", mkStoreOperationFn(LoadStorePolyWord{isImmutable=false}), mkProductType[tvA, Word, LargeWord] ->> Unit, templates 1)
            (* Store a native word value from a large word *)
            val () = enterRunCall ("storeNativeWord", mkStoreOperationFn(LoadStoreNativeWord{isImmutable=false}), mkProductType[tvA, Word, LargeWord] ->> Unit, templates 1)
            (* Lock a mutable cell. *)
            val () = enterRunCall ("clearMutableBit", mkUnaryFn ClearMutableFlag, tvA ->> Unit, templates 1)
            (* Allocate a byte cell.  The second argument is the flags byte.  It might be better
               if this were a Word8.word value.  *)
            val () = enterRunCall ("allocateByteMemory", mkBinaryFn AllocateByteMemory, Word ** Word ->> tvA, templates 1)
            (* Allocate a word cell. *)
            val () = enterRunCall ("allocateWordMemory", mkAllocateWordMemoryFn, mkProductType[Word, Word, tvA] ->> tvB, templates 2)
            (* Byte vector operations. *)
            val () = enterRunCall ("byteVectorEqual", mkBlockOperationFn BlockOpEqualByte, mkProductType[tvA, tvA, Word, Word, Word] ->> Bool, templates 1)
            val () = enterRunCall ("byteVectorCompare", mkBlockOperationFn BlockOpCompareByte, mkProductType[tvA, tvA, Word, Word, Word] ->> Int, templates 1)
            (* Block moves. *)
            val () = enterRunCall ("moveBytes", mkBlockOperationFn (BlockOpMove{isByteMove=true}), mkProductType[tvA, tvA, Word, Word, Word] ->> Unit, templates 1)
            val () = enterRunCall ("moveWords", mkBlockOperationFn (BlockOpMove{isByteMove=false}), mkProductType[tvA, tvA, Word, Word, Word] ->> Unit, templates 1)
            (* Untagged loads and stores. *)
            val () = enterRunCall ("loadUntagged",
                    mkLoadOperationFn LoadStoreUntaggedUnsigned, String ** Word ->> Word, [])
            val () = enterRunCall ("storeUntagged",
                    mkStoreOperationFn LoadStoreUntaggedUnsigned, mkProductType[String, Word, Word] ->> Unit, [])
            val () = enterRunCall ("touch", mkUnaryFn TouchAddress, tvA ->> Unit, templates 1)
        end

        local
            val debugOpts = [] (* Place to add debugging if necessary. *)
            (* [tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
               tagInject assemblyCodeTag true] *)
 
            fun makeCall rtsCall n entryName = rtsCall (entryName, n, debugOpts)
            val makeFastCall = makeCall CODETREE.Foreign.rtsCallFast
            (* We need to wrap this so that the arguments are passed in registers. *)

            fun makeRunCallTupled (argTypes, resultType, callN) : codetree =
            let
                val width = List.length argTypes
                val name = "rtsCall" ^ Int.toString width;

                local
                    val f     = mkLoadClosure 0        (* first item from enclosing scope *)
                    val tuple = mkLoadArgument 0       (* the inner parameter *)
                    val args  =
                        case argTypes of
                            [singleType] => [(tuple, singleType)]
                        |   argTypes =>
                            let
                                val argVals = List.tabulate(width, fn n => mkInd (n, tuple))
                            in
                                ListPair.zipEq(argVals, argTypes)
                            end
                in
                    val innerBody = mkCall (f, args, resultType)
                end

                local
                    (* The closure contains the address of the RTS call. *)
                    val f = mkEval(mkConst callN, [mkLoadArgument 0])
                    val innerLambda = mkInlproc (innerBody, 1, name ^ "(1)", [mkLoadLocal 0], 0)
                in
                    val outerBody = mkEnv([mkDec (0, f)], innerLambda)
                end

                val outerLambda  = mkInlproc (outerBody, 1, name, [], 1)
            in
                outerLambda
            end

            (* "Full" calls pass the thread Id as the first parameter. *)
            fun makeRunCallTupledFull (argTypes, resultType) =
            let
                val width = List.length argTypes
                val callN = toMachineWord(makeFastCall(width + 1))
                val name = "rtsCall" ^ Int.toString width;

                local
                    val f     = mkLoadClosure 0        (* first item from enclosing scope *)
                    val tuple = mkLoadArgument 0       (* the inner parameter *)
                    val args  =
                        case argTypes of
                            [singleType] => [(tuple, singleType)]
                        |   argTypes =>
                            let
                                val argVals = List.tabulate(width, fn n => mkInd (n, tuple))
                            in
                                ListPair.zipEq(argVals, argTypes)
                            end
                in
                    val innerBody = mkCall (f, (getCurrentThreadId, GeneralType) :: args, resultType)
                end

                local
                    (* The closure contains the address of the RTS call. *)
                    val f = mkEval(mkConst callN, [mkLoadArgument 0]) (* This creates the actual call. *)
                    val innerLambda = mkInlproc (innerBody, 1, name ^ "(1)", [mkLoadLocal 0], 0)
                in
                    val outerBody = mkEnv([mkDec (0, f)], innerLambda)
                end

                val outerLambda  = mkInlproc (outerBody, 1, name, [], 1)
            in
                outerLambda
            end

            local
                fun makeInlCode(makeCall, name) =
                let
                    val call1 = toMachineWord(makeCall 1)
                    val body = mkEval(mkConst call1, [mkLoadArgument 0])
                in
                    mkInlproc (body, 1, name, [], 0)
                end
            in
                val rtsCallFast1Entry = makeInlCode(makeFastCall, "rtsCallFast1")
            end
            
            local
                fun makeRtsCall(n, makeCall) = makeRunCallTupled(List.tabulate(n, fn _ => GeneralType), GeneralType, toMachineWord(makeCall n))
                fun makeFullRtsCall n = makeRunCallTupledFull(List.tabulate(n, fn _ => GeneralType), GeneralType)
            in
                val rtsCallFull0Entry = makeFullRtsCall 0
                and rtsCallFast0Entry = makeRtsCall(0, makeFastCall)
                val rtsCall0Type = String ->> Unit ->> tvA
 
                val rtsCall1Type = String ->> tvA ->> tvB
                val rtsCallFull1Entry = makeFullRtsCall 1
                val rtsCallFull2Entry = makeFullRtsCall 2
                and rtsCallFast2Entry = makeRtsCall(2, makeFastCall)
                val rtsCall2Type = String ->> TYPETREE.mkProductType [tvA,tvB] ->> tvC
                val rtsCallFull3Entry = makeFullRtsCall 3
                val rtsCallFast3Entry = makeRtsCall(3, makeFastCall)
                val rtsCall3Type = String ->> TYPETREE.mkProductType [tvA,tvB,tvC] ->> tvD
                val rtsCallFull4Entry = makeFullRtsCall 4
                val rtsCallFast4Entry = makeRtsCall(4, makeFastCall)
                val rtsCall4Type = String ->> TYPETREE.mkProductType [tvA,tvB,tvC,tvD] ->> tvE
                val rtsCallFull5Entry = makeFullRtsCall 5
                val rtsCall5Type = String ->> TYPETREE.mkProductType [tvA,tvB,tvC,tvD,tvE] ->> tvF
            end
        in
            val () = enterRunCall ("rtsCallFull0", rtsCallFull0Entry, rtsCall0Type, templates 1)
            val () = enterRunCall ("rtsCallFast0", rtsCallFast0Entry, rtsCall0Type, templates 1)
            val () = enterRunCall ("rtsCallFull1", rtsCallFull1Entry, rtsCall1Type, templates 2)
            val () = enterRunCall ("rtsCallFast1", rtsCallFast1Entry, rtsCall1Type, templates 2)
            val () = enterRunCall ("rtsCallFull2", rtsCallFull2Entry, rtsCall2Type, templates 3)
            val () = enterRunCall ("rtsCallFast2", rtsCallFast2Entry, rtsCall2Type, templates 3)
            val () = enterRunCall ("rtsCallFull3", rtsCallFull3Entry, rtsCall3Type, templates 4)
            val () = enterRunCall ("rtsCallFast3", rtsCallFast3Entry, rtsCall3Type, templates 4)
            val () = enterRunCall ("rtsCallFast4", rtsCallFast4Entry, rtsCall4Type, templates 5)
            val () = enterRunCall ("rtsCallFull4", rtsCallFull4Entry, rtsCall4Type, templates 5)
            val () = enterRunCall ("rtsCallFull5", rtsCallFull5Entry, rtsCall5Type, templates 6)
            
            val makeRunCallTupled = makeRunCallTupled (* Needed for reals. *)
        end

        local
            (* Create nullary exception. *)
            fun makeException0(name, id) =
            let
                val exc =
                    Value{ name = name, typeOf = ValueType(TYPETREE.exnType, []),
                           access = Global(mkConst(toMachineWord id)),
                           class = Exception, locations = declInBasis,
                           references = NONE, instanceTypes=NONE }
            in
                #enterVal runCallEnv (name, exc)
            end
            (* Create exception with parameter. *)
            and makeException1(name, id, exType) =
            let
                val exc =
                    Value{ name = name, typeOf = ValueType(exType ->> TYPETREE.exnType, []),
                           access = Global(mkConst(toMachineWord id)),
                           class = Exception, locations = declInBasis,
                           references = NONE, instanceTypes=NONE }
            in
                #enterVal runCallEnv (name, exc)
            end
            (* Exception numbers.  Most of these are hard-coded in the RTS. *)
            val EXC_interrupt   = 1
            val EXC_syserr      = 2
            val EXC_size        = 4
            val EXC_overflow    = 5
            val EXC_divide      = 7
            val EXC_conversion  = 8
            val EXC_XWindows    = 10
            val EXC_subscript   = 11
            val EXC_thread      = 12
            val EXC_Bind        = 100 (* In Match compiler. *)
            val EXC_Match       = 101
            val EXC_Fail        = 103
        in
            val () = List.app makeException0
                [
                    ("Interrupt",   EXC_interrupt),
                    ("Size",        EXC_size),
                    ("Bind",        EXC_Bind),
                    ("Div",         EXC_divide),
                    ("Match",       EXC_Match),
                    ("Overflow",    EXC_overflow),
                    ("Subscript",   EXC_subscript)
                 ]
             val () = List.app makeException1
                [
                    ("Fail",        EXC_Fail,           String),
                    ("Conversion",  EXC_conversion,     String),
                    ("XWindows",    EXC_XWindows,       String),
                    ("Thread",      EXC_thread,         String),
                    ("SysErr",      EXC_syserr,         String ** Option LargeWord)
                ]
        end
        

        (* Standard Basis structures for basic types.  These contain the definitions of the basic
           types and operations on them.  The structures are extended in the basis library and
           overloaded functions are extracted from them. *)
        local
            val largeIntEnv = makeStructure(globalEnv, "LargeInt")
            (* The comparison operations take two arbitrary precision ints and
               a general "compare" function that returns a fixed precision int. *)
            val compareType =
                mkProductType[intInfType, intInfType, intInfType ** intInfType ->> fixedIntType] ->> Bool
            val arithType =
                mkProductType[intInfType, intInfType, intInfType ** intInfType ->> intInfType] ->> intInfType

            fun enterArbitrary(name, oper, typ) =
            let
                val value = mkGvar (name, ValueType(typ, []), mkArbitraryFn oper, declInBasis)
            in
                #enterVal largeIntEnv (name, value)
            end
        in
            val () = #enterType largeIntEnv ("int", TypeConstrSet(intInfConstr, []))
            (* These functions are used internally. *)
            val () = enterArbitrary("less", ArbCompare BuiltIns.TestLess, compareType)
            val () = enterArbitrary("greater", ArbCompare BuiltIns.TestGreater, compareType)
            val () = enterArbitrary("lessEq", ArbCompare BuiltIns.TestLessEqual, compareType)
            val () = enterArbitrary("greaterEq", ArbCompare BuiltIns.TestGreaterEqual, compareType)
            val () = enterArbitrary("add", ArbArith BuiltIns.ArithAdd, arithType)
            val () = enterArbitrary("subtract", ArbArith BuiltIns.ArithSub, arithType)
            val () = enterArbitrary("multiply", ArbArith BuiltIns.ArithMult, arithType)
            val () = #enterVal largeIntEnv ("log2Word", mkGvar ("log2Word", ValueType(Word ->> Word, []), mkUnaryFn BuiltIns.Log2Word, declInBasis))
        end

        local
            val fixedIntEnv = makeStructure(globalEnv, "FixedInt")
            open BuiltIns

            fun enterBinary(name, oper, typ) =
            let
                val value = mkGvar (name, ValueType(typ, []), mkBinaryFn oper, declInBasis)
            in
                #enterVal fixedIntEnv (name, value)
            end
            
            val compareType = fixedIntType ** fixedIntType ->> Bool
            and binaryType  = fixedIntType ** fixedIntType ->> fixedIntType

            fun enterComparison(name, test) =
                enterBinary(name, WordComparison{test=test, isSigned=true}, compareType)
            and enterBinaryOp(name, oper) =
                enterBinary(name, FixedPrecisionArith oper, binaryType)
            
        in
            val () = #enterType fixedIntEnv ("int", TypeConstrSet(fixedIntConstr, []))
            val () = enterComparison("<",   TestLess)
            val () = enterComparison("<=",  TestLessEqual)
            val () = enterComparison(">",   TestGreater)
            val () = enterComparison(">=",  TestGreaterEqual)
            val () = enterBinaryOp("+",     ArithAdd)
            val () = enterBinaryOp("-",     ArithSub)
            val () = enterBinaryOp("*",     ArithMult)
            val () = enterBinaryOp("quot",  ArithQuot)
            val () = enterBinaryOp("rem",   ArithRem)
        end

        local
            open BuiltIns
            val largeWordEnv = makeStructure(globalEnv, "LargeWord")

            fun enterBinary(name, oper, typ) =
            let
                val value = mkGvar (name, ValueType(typ, []), mkBinaryFn oper, declInBasis)
            in
                #enterVal largeWordEnv (name, value)
            end
            
            val compareType = LargeWord ** LargeWord ->> Bool
            and binaryType  = LargeWord ** LargeWord ->> LargeWord
            and shiftType   = LargeWord ** Word ->> LargeWord (* The shift amount is a Word. *)

            fun enterComparison(name, test) =
                enterBinary(name, LargeWordComparison test, compareType)
            and enterBinaryOp(name, oper) =
                enterBinary(name, LargeWordArith oper, binaryType)
            and enterBinaryLogical(name, oper) =
                enterBinary(name, LargeWordLogical oper, binaryType)
            and enterBinaryShift(name, oper) =
                enterBinary(name, LargeWordShift oper, shiftType)
        in
            val () = #enterType largeWordEnv ("word", TypeConstrSet(largeWordType, []))
            val () = enterComparison("<",   TestLess)
            val () = enterComparison("<=",  TestLessEqual)
            val () = enterComparison(">",   TestGreater)
            val () = enterComparison(">=",  TestGreaterEqual)
            val () = enterBinaryOp("+",     ArithAdd)
            val () = enterBinaryOp("-",     ArithSub)
            val () = enterBinaryOp("*",     ArithMult)
            val () = enterBinaryOp("div",   ArithDiv)
            val () = enterBinaryOp("mod",   ArithMod)
            val () = enterBinaryLogical("orb",  LogicalOr)
            val () = enterBinaryLogical("andb", LogicalAnd)
            val () = enterBinaryLogical("xorb", LogicalXor)
            val () = enterBinaryShift("<<", ShiftLeft)
            val () = enterBinaryShift(">>", ShiftRightLogical)
            val () = enterBinaryShift("~>>", ShiftRightArithmetic)
            val LargeWord = LargeWord
        end

        local
            val wordStructEnv = makeStructure(globalEnv, "Word")
            open BuiltIns

            fun enterBinary(name, oper, typ) =
            let
                val value = mkGvar (name, ValueType(typ, []), mkBinaryFn oper, declInBasis)
            in
                #enterVal wordStructEnv (name, value)
            end
            
            val compareType = Word ** Word ->> Bool
            and binaryType  = Word ** Word ->> Word

            fun enterComparison(name, test) =
                enterBinary(name, WordComparison{test=test, isSigned=false}, compareType)
            and enterBinaryOp(name, oper) =
                enterBinary(name, WordArith oper, binaryType)
            and enterBinaryLogical(name, oper) =
                enterBinary(name, WordLogical oper, binaryType)
            and enterBinaryShift(name, oper) =
                enterBinary(name, WordShift oper, binaryType)
                
            val toLargeWordFn = mkGvar ("toLargeWord", ValueType(Word ->> LargeWord, []), mkUnaryFn UnsignedToLongWord, declInBasis)
            and toLargeWordXFn = mkGvar ("toLargeWordX", ValueType(Word ->> LargeWord, []), mkUnaryFn SignedToLongWord, declInBasis)
            and fromLargeWordFn = mkGvar ("fromLargeWord", ValueType(LargeWord ->> Word, []), mkUnaryFn LongWordToTagged, declInBasis)
        in
            val () = #enterType wordStructEnv ("word", TypeConstrSet(wordConstr, []))
            val () = enterComparison("<",   TestLess)
            val () = enterComparison("<=",  TestLessEqual)
            val () = enterComparison(">",   TestGreater)
            val () = enterComparison(">=",  TestGreaterEqual)
            val () = enterBinaryOp("+",     ArithAdd)
            val () = enterBinaryOp("-",     ArithSub)
            val () = enterBinaryOp("*",     ArithMult)
            val () = enterBinaryOp("div",   ArithDiv)
            val () = enterBinaryOp("mod",   ArithMod)
            val () = enterBinaryLogical("orb",  LogicalOr)
            val () = enterBinaryLogical("andb", LogicalAnd)
            val () = enterBinaryLogical("xorb", LogicalXor)
            val () = enterBinaryShift("<<", ShiftLeft)
            val () = enterBinaryShift(">>", ShiftRightLogical)
            val () = enterBinaryShift("~>>", ShiftRightArithmetic)
            val () = #enterVal wordStructEnv ("toLargeWord", toLargeWordFn)
            val () = #enterVal wordStructEnv ("toLargeWordX", toLargeWordXFn)
            val () = #enterVal wordStructEnv ("fromLargeWord", fromLargeWordFn)
        end
 
        local
            val charEnv = makeStructure(globalEnv, "Char")
            open BuiltIns
            (* Comparison functions are the same as Word. *)
            fun enterComparison(name, test) =
            let
                val typ = Char ** Char ->> Bool
                val entry = mkBinaryFn(WordComparison{test=test, isSigned=false})
                val value = mkGvar (name, ValueType(typ, []), entry, declInBasis)
            in
                #enterVal charEnv (name, value)
            end
        in
            val () = #enterType charEnv ("char", TypeConstrSet(charConstr, []))
            val () = enterComparison("<",   TestLess)
            val () = enterComparison("<=",  TestLessEqual)
            val () = enterComparison(">",   TestGreater)
            val () = enterComparison(">=",  TestGreaterEqual)
        end

        local
            val stringEnv = makeStructure(globalEnv, "String")
        in
            val () = #enterType stringEnv ("string", TypeConstrSet(stringConstr, []))
        end

        local                        
            val realEnv = makeStructure(globalEnv, "Real")

            (* These are only used in Real so are included here rather than in RunCall.
               rtsCallFastRealtoReal is used for functions such as sqrt.
               rtsCallFastGeneraltoReal is used for Real.fromLargeInt. *)
            val debugOpts = [] (* Place to add debugging if necessary. *)

            fun makeFastRealRealCall entryName = CODETREE.Foreign.rtsCallFastRealtoReal (entryName, debugOpts)
            and makeFastRealRealRealCall entryName = CODETREE.Foreign.rtsCallFastRealRealtoReal (entryName, debugOpts)
            and makeFastIntInfRealCall entryName = CODETREE.Foreign.rtsCallFastGeneraltoReal (entryName, debugOpts)
            and makeFastRealGeneralRealCall entryName = CODETREE.Foreign.rtsCallFastRealGeneraltoReal (entryName, debugOpts)

            val rtsCallFastR_REntry = makeRunCallTupled([DoubleFloatType], DoubleFloatType, toMachineWord makeFastRealRealCall)

            (* This needs to be tupled. *)
            val rtsCallFastRR_REntry = makeRunCallTupled([DoubleFloatType, DoubleFloatType], DoubleFloatType, toMachineWord makeFastRealRealRealCall)
            and rtsCallFastRI_REntry = makeRunCallTupled([DoubleFloatType, GeneralType], DoubleFloatType, toMachineWord makeFastRealGeneralRealCall)

            val rtsCallFastI_REntry = makeRunCallTupled([GeneralType], DoubleFloatType, toMachineWord makeFastIntInfRealCall)

            val rtsCallFastF_F = mkGvar ("rtsCallFastR_R", ValueType(String ->> Real ->> Real, []), rtsCallFastR_REntry, declInBasis)
            val rtsCallFastFF_F = mkGvar ("rtsCallFastRR_R", ValueType(String ->> Real ** Real ->> Real, []), rtsCallFastRR_REntry, declInBasis)
            val rtsCallFastFG_F = mkGvar ("rtsCallFastRI_R", ValueType(String ->> Real ** Int ->> Real, []), rtsCallFastRI_REntry, declInBasis)
            val rtsCallFastG_F = mkGvar ("rtsCallFastI_R", ValueType(String ->> intInfType ->> Real, []), rtsCallFastI_REntry, declInBasis)

            fun enterUnary(name, oper, typ) =
            let
                val value = mkGvar (name, ValueType(typ, []), mkUnaryFn oper, declInBasis)
            in
                #enterVal realEnv (name, value)
            end

            fun enterBinary(name, oper, typ) =
            let
                val value = mkGvar (name, ValueType(typ, []), mkBinaryFn oper, declInBasis)
            in
                #enterVal realEnv (name, value)
            end
            
            val compareType = Real ** Real ->> Bool
            and binaryType  = Real ** Real ->> Real
            and unaryType   = Real ->> Real
            and realToFixType = Real ->> fixedIntType

            open BuiltIns IEEEReal

            fun enterComparison(name, test) =
                enterBinary(name, RealComparison(test, PrecDouble), compareType)
            and enterBinaryOp(name, oper) =
                enterBinary(name, RealArith(oper, PrecDouble), binaryType)
        in
            val () = #enterType realEnv ("real", TypeConstrSet(realConstr, []))
            val () = #enterVal realEnv ("rtsCallFastR_R", rtsCallFastF_F)
            val () = #enterVal realEnv ("rtsCallFastRR_R", rtsCallFastFF_F)
            val () = #enterVal realEnv ("rtsCallFastRI_R", rtsCallFastFG_F)
            val () = #enterVal realEnv ("rtsCallFastI_R", rtsCallFastG_F)
            val () = enterComparison("<",   TestLess)
            val () = enterComparison("<=",  TestLessEqual)
            val () = enterComparison(">",   TestGreater)
            val () = enterComparison(">=",  TestGreaterEqual)
            val () = enterComparison("==",  TestEqual) (* real is not an eqtype. *)
            (* Included unordered mainly because it's easy to implement isNan. *)
            val () = enterComparison("unordered",  TestUnordered)
            val () = enterBinaryOp("+",     ArithAdd)
            val () = enterBinaryOp("-",     ArithSub)
            val () = enterBinaryOp("*",     ArithMult)
            val () = enterBinaryOp("/",     ArithDiv)
            val () = enterUnary("~",   RealNeg PrecDouble, unaryType)
            val () = enterUnary("abs", RealAbs PrecDouble, unaryType)
            val () = enterUnary("fromFixedInt", RealFixedInt PrecDouble, fixedIntType ->> Real)
            val () = enterUnary("truncFix", RealToInt(PrecDouble, TO_ZERO), realToFixType)
            val () = enterUnary("roundFix", RealToInt(PrecDouble, TO_NEAREST), realToFixType)
            val () = enterUnary("ceilFix", RealToInt(PrecDouble, TO_POSINF), realToFixType)
            val () = enterUnary("floorFix", RealToInt(PrecDouble, TO_NEGINF), realToFixType)
        end
        
        local
            val real32Env = makeStructure(globalEnv, "Real32")
            val floatType  = mkTypeConstruction ("real", floatConstr, [], [])
            val Float = floatType
            val debugOpts = [] (* Place to add debugging if necessary. *)

            fun makeFastFloatFloatCall entryName = CODETREE.Foreign.rtsCallFastFloattoFloat (entryName, debugOpts)
            and makeFastFloatFloatFloatCall entryName = CODETREE.Foreign.rtsCallFastFloatFloattoFloat (entryName, debugOpts)
            and makeFastIntInfFloatCall entryName = CODETREE.Foreign.rtsCallFastGeneraltoFloat (entryName, debugOpts)
            and makeFastFloatGeneralFloatCall entryName = CODETREE.Foreign.rtsCallFastFloatGeneraltoFloat (entryName, debugOpts)

            val rtsCallFastR_REntry = makeRunCallTupled([SingleFloatType], SingleFloatType, toMachineWord makeFastFloatFloatCall)

            (* This needs to be tupled. *)
            val rtsCallFastRR_REntry = makeRunCallTupled([SingleFloatType, SingleFloatType], SingleFloatType, toMachineWord makeFastFloatFloatFloatCall)
            and rtsCallFastRI_REntry = makeRunCallTupled([SingleFloatType, GeneralType], SingleFloatType, toMachineWord makeFastFloatGeneralFloatCall)

            val rtsCallFastI_REntry = makeRunCallTupled([GeneralType], SingleFloatType, toMachineWord makeFastIntInfFloatCall)

            val rtsCallFastF_F = mkGvar ("rtsCallFastF_F", ValueType(String ->> Float ->> Float, []), rtsCallFastR_REntry, declInBasis)
            val rtsCallFastFF_F = mkGvar ("rtsCallFastFF_F", ValueType(String ->> Float ** Float ->> Float, []), rtsCallFastRR_REntry, declInBasis)
            val rtsCallFastFG_F = mkGvar ("rtsCallFastFI_F", ValueType(String ->> Float ** Int ->> Float, []), rtsCallFastRI_REntry, declInBasis)
            val rtsCallFastG_F = mkGvar ("rtsCallFastI_F", ValueType(String ->> intInfType ->> Float, []), rtsCallFastI_REntry, declInBasis)

            fun enterUnary(name, oper, typ) =
            let
                val value = mkGvar (name, ValueType(typ, []), mkUnaryFn oper, declInBasis)
            in
                #enterVal real32Env (name, value)
            end
            
            fun enterBinary(name, oper, typ) =
            let
                val value = mkGvar (name, ValueType(typ, []), mkBinaryFn oper, declInBasis)
            in
                #enterVal real32Env (name, value)
            end
            
            val compareType = Float ** Float ->> Bool
            and binaryType  = Float ** Float ->> Float
            and unaryType   = Float ->> Float
            and floatToFixType = Float ->> fixedIntType

            open BuiltIns IEEEReal

            fun enterComparison(name, test) =
                enterBinary(name, RealComparison(test, PrecSingle), compareType)
            and enterBinaryOp(name, oper) =
                enterBinary(name, RealArith(oper, PrecSingle), binaryType)
        in
            val () = #enterType real32Env ("real", TypeConstrSet(floatConstr, []))
            val () = enterUnary("toLarge", BuiltIns.FloatToDouble, floatType ->> Real)
            (* Conversion with the current rounding mode. *)
            and () = enterUnary("fromReal", BuiltIns.DoubleToFloat, Real  ->> floatType)
            val () = #enterVal real32Env ("rtsCallFastR_R", rtsCallFastF_F)
            val () = #enterVal real32Env ("rtsCallFastRR_R", rtsCallFastFF_F)
            val () = #enterVal real32Env ("rtsCallFastRI_R", rtsCallFastFG_F)
            val () = #enterVal real32Env ("rtsCallFastI_R", rtsCallFastG_F)
            val () = enterComparison("<",   TestLess)
            val () = enterComparison("<=",  TestLessEqual)
            val () = enterComparison(">",   TestGreater)
            val () = enterComparison(">=",  TestGreaterEqual)
            val () = enterComparison("==",  TestEqual) (* Real32.real is not an eqtype. *)
            val () = enterComparison("unordered",  TestUnordered)
            val () = enterBinaryOp("+",     ArithAdd)
            val () = enterBinaryOp("-",     ArithSub)
            val () = enterBinaryOp("*",     ArithMult)
            val () = enterBinaryOp("/",     ArithDiv)
            val () = enterUnary("~",   RealNeg PrecSingle, unaryType)
            val () = enterUnary("abs", RealAbs PrecSingle, unaryType)
            val () = enterUnary("fromFixedInt", RealFixedInt PrecSingle, fixedIntType ->> floatType)
            val () = enterUnary("truncFix", RealToInt(PrecSingle, TO_ZERO), floatToFixType)
            val () = enterUnary("roundFix", RealToInt(PrecSingle, TO_NEAREST), floatToFixType)
            val () = enterUnary("ceilFix", RealToInt(PrecSingle, TO_POSINF), floatToFixType)
            val () = enterUnary("floorFix", RealToInt(PrecSingle, TO_NEGINF), floatToFixType)
        end

        val bootstrapEnv = makeStructure(globalEnv, "Bootstrap")
        
        fun enterBootstrap (name : string, entry : codetree, typ : types) : unit =
        let
            val value = mkGvar (name, ValueType(typ, []), entry, declInBasis)
        in
            #enterVal bootstrapEnv (name, value)
        end

        local
            val threadEnv = makeStructure(globalEnv, "Thread")

            open TypeValue
            fun monoTypePrinter _ = PRETTY.PrettyString "?"
            val code =
                createTypeValue{
                    eqCode=equalPointerOrWordFn,
                    printCode=mkConst (toMachineWord (ref monoTypePrinter))}
            (* Thread.thread type.  This is an equality type with pointer equality. *)
            val threadConstr= 
                makeTypeConstructor (
                    "thread",makeFreeId(0, Global (genCode(code, [], 0) ()), true, basisDescription "thread"),
                    [DeclaredAt inBasis])
            val threadType = mkTypeConstruction ("thread", threadConstr, [], []);
            val selfFunction = mkGvar ("self", ValueType(Unit ->> threadType, []), getCurrentThreadIdFn, declInBasis)
            val createMutexFunction = mkGvar("createMutex", ValueType(Unit ->> Ref Word, []), createMutexFn, declInBasis)
            and lockMutexFunction = mkGvar("lockMutex", ValueType(Ref Word ->> Bool, []), mkUnaryFn BuiltIns.LockMutex, declInBasis)
            and tryLockMutexFunction = mkGvar("tryLockMutex", ValueType(Ref Word ->> Bool, []), mkUnaryFn BuiltIns.TryLockMutex, declInBasis)
            and unlockMutexFunction = mkGvar("unlockMutex", ValueType(Ref Word ->> Bool, []), mkUnaryFn BuiltIns.UnlockMutex, declInBasis)
            and cpuPauseFunction = mkGvar("cpuPause", ValueType(Unit ->> Unit, []), cpuPauseFn, declInBasis)
        in
            val () = #enterType threadEnv ("thread", TypeConstrSet(threadConstr, []))
            val () = #enterVal threadEnv ("self", selfFunction)
            val () = #enterVal threadEnv ("createMutex", createMutexFunction)
            val () = #enterVal threadEnv ("lockMutex", lockMutexFunction)
            val () = #enterVal threadEnv ("tryLockMutex", tryLockMutexFunction)
            val () = #enterVal threadEnv ("unlockMutex", unlockMutexFunction)
            val () = #enterVal threadEnv ("cpuPause", cpuPauseFunction)
        end

        local
            val singleTemplate = templates 1
            val fmemEnv = makeStructure(globalEnv, "ForeignMemory")
            (* We don't have Word8.word or Word32.word at this point so the easiest way to
               deal with this is to make them polymorphic. *)
            val get8Function =
                mkGvar("get8", ValueType(LargeWord ** Word ->> tvA, singleTemplate), mkLoadOperationFn LoadStoreC8, declInBasis)
            val get16Function =
                mkGvar("get16", ValueType(LargeWord ** Word ->> Word, []), mkLoadOperationFn LoadStoreC16, declInBasis)
            val get32Function =
                mkGvar("get32", ValueType(LargeWord ** Word ->> tvA, singleTemplate), mkLoadOperationFn LoadStoreC32, declInBasis)
            val get64Function =
                mkGvar("get64", ValueType(LargeWord ** Word ->> LargeWord, []), mkLoadOperationFn LoadStoreC64, declInBasis)
            val getFloatFunction =
                mkGvar("getFloat", ValueType(LargeWord ** Word ->> Real, []), mkLoadOperationFn LoadStoreCFloat, declInBasis)
            val getDoubleFunction =
                mkGvar("getDouble", ValueType(LargeWord ** Word ->> Real, []), mkLoadOperationFn LoadStoreCDouble, declInBasis)
            val set8Function =
                mkGvar("set8",
                    ValueType(mkProductType[LargeWord, Word, tvA] ->> Unit, templates 1), mkStoreOperationFn LoadStoreC8,
                    declInBasis)
            val set16Function =
                mkGvar("set16",
                    ValueType(mkProductType[LargeWord, Word, Word] ->> Unit, []), mkStoreOperationFn LoadStoreC16, declInBasis)
            val set32Function =
                mkGvar("set32",
                    ValueType(mkProductType[LargeWord, Word, tvA] ->> Unit, templates 1), mkStoreOperationFn LoadStoreC32,
                    declInBasis)
            val set64Function =
                mkGvar("set64",
                    ValueType(mkProductType[LargeWord, Word, LargeWord] ->> Unit, []), mkStoreOperationFn LoadStoreC64, declInBasis)
            val setFloatFunction =
                mkGvar("setFloat",
                    ValueType(mkProductType[LargeWord, Word, Real] ->> Unit, []), mkStoreOperationFn LoadStoreCFloat, declInBasis)
            val setDoubleFunction =
                mkGvar("setDouble",
                    ValueType(mkProductType[LargeWord, Word, Real] ->> Unit, []), mkStoreOperationFn LoadStoreCDouble, declInBasis)
            val allocCStackFn =
                mkGvar("allocCStack", ValueType(Word ->> LargeWord, []), mkUnaryFn BuiltIns.AllocCStack, declInBasis)
            val freeCStackFn =
                mkGvar("freeCStack", ValueType(LargeWord ** Word ->> Unit, []), mkBinaryFn BuiltIns.FreeCStack, declInBasis)
        in
            val () = #enterVal fmemEnv ("get8", get8Function)
            val () = #enterVal fmemEnv ("get16", get16Function)
            val () = #enterVal fmemEnv ("get32", get32Function)
            val () = #enterVal fmemEnv ("get64", get64Function)
            val () = #enterVal fmemEnv ("getFloat", getFloatFunction)
            val () = #enterVal fmemEnv ("getDouble", getDoubleFunction)
            val () = #enterVal fmemEnv ("set8", set8Function)
            val () = #enterVal fmemEnv ("set16", set16Function)
            val () = #enterVal fmemEnv ("set32", set32Function)
            val () = #enterVal fmemEnv ("set64", set64Function)
            val () = #enterVal fmemEnv ("setFloat", setFloatFunction)
            val () = #enterVal fmemEnv ("setDouble", setDoubleFunction)
            val () = #enterVal fmemEnv ("allocCStack", allocCStackFn)
            (* Free is a binary operation that takes both the allocated address and the size.
               The size is used by the compiled code where this is implemented using the C-stack.
               The allocated address is intended for possible use by the interpreter where so
               that it can be implemented as malloc/free. *)
            val () = #enterVal fmemEnv ("freeCStack", freeCStackFn)
        end
        
        local
            val foreignEnv = makeStructure(globalEnv, "Foreign")

            local
                val EXC_foreign     = 23
                val foreignException =
                    Value{ name = "Foreign", typeOf = ValueType(String ->> TYPETREE.exnType, []),
                           access = Global(mkConst(toMachineWord EXC_foreign)),
                           class = Exception, locations = declInBasis,
                        references = NONE, instanceTypes=NONE }
            in
                val () = #enterVal foreignEnv ("Foreign", foreignException)
            end
            
            val arg0 = mkLoadArgument 0
            val arg1 = mkLoadArgument 1
            
            local
                val callForeignCall = mkEval(mkConst (toMachineWord CODETREE.Foreign.foreignCall), [arg0])
                val innerBody = mkEval(mkLoadClosure 0, [mkInd(0, arg0), mkInd(1, arg0), mkInd(2, arg0)])
                val outerBody =
                    mkEnv([mkDec(0, callForeignCall)], mkInlproc(innerBody, 1, "foreignCall(1)(1)", [mkLoadLocal 0], 0))
            in
                val foreignCallEntry = mkInlproc(outerBody, 1, "foreignCall(1)", [], 1)
            end

            local
                (* Build a callback.  First apply the compiler to the abi/argtype/restype values.
                   Then apply the result to a function to generate the final C callback code.
                   The C callback code calls the function with two arguments.  Here we have to
                   pass it a function that expects a tuple and unwrap it. *)
                val innerMost =
                    mkInlproc(mkEval(mkLoadClosure 0, [mkTuple[arg0, arg1]]), 2, "buildCallBack(1)(1)2", [mkLoadArgument 0], 0)
                val resultFn =
                    mkInlproc(mkEval(mkLoadClosure 0, [innerMost]), 1, "buildCallBack(1)(1)", [mkLoadLocal 0], 0)
                val firstBuild = mkEval(mkConst (toMachineWord CODETREE.Foreign.buildCallBack), [arg0])
                val outerBody = mkEnv([mkDec(0, firstBuild)], resultFn)
            in
                val buildCallBackEntry = mkInlproc(outerBody, 1, "buildCallBack(1)", [], 1)
            end
            
            (* Abi - an eqtype.  An enumerated type or short int. *)
            local
                open TypeValue
                fun monotypePrinter _ = PRETTY.PrettyString "?"
                val code =
                    createTypeValue{
                        eqCode = equalTaggedWordFn, printCode = mkConst (toMachineWord (ref monotypePrinter)) }
                val abiEqAndPrint = Global (genCode(code, [], 0) ())
            in
                val abiConstr =
                    makeTypeConstructor("abi",
                        makeFreeId(0, abiEqAndPrint, true,
                            basisDescription "Foreign.LowLevel.abi"), declInBasis)
            end
            val () = #enterType foreignEnv ("abi", TypeConstrSet(abiConstr, []))
            val abiType = mkTypeConstruction ("abi", abiConstr, [], declInBasis)
            (* It would be possible to put the definition of cType in here but it's complicated.
               It's easier to use an opaque type and put in a cast later. *)
            val ctypeConstr =
                makeTypeConstructor("ctype",
                        makeFreeId(0, defaultEqAndPrintCode(), false,
                            basisDescription "Foreign.LowLevel.ctype"), declInBasis)
            val () = #enterType foreignEnv ("ctype", TypeConstrSet(ctypeConstr, []))
            val ffiType = mkTypeConstruction ("ctype", ctypeConstr, [], declInBasis)
            val foreignCallType =
                mkProductType[abiType, List ffiType, ffiType] ->> mkProductType[LargeWord, LargeWord, LargeWord] ->> Unit
            val buildCallBackType =
                mkProductType[abiType, List ffiType, ffiType] ->> (mkProductType[LargeWord, LargeWord] ->> Unit) ->> LargeWord
        
            fun enterForeign (name, entry, typ) =
                #enterVal foreignEnv (name, mkGvar (name, typ, entry, declInBasis))
        in
            val () = enterForeign("foreignCall", foreignCallEntry, ValueType(foreignCallType, []))
            val () = enterForeign("buildCallBack", buildCallBackEntry, ValueType(buildCallBackType, []))
            (* Apply the abiList function here.  The ABIs depend on the platform in the interpreted version. *)
            val () = enterForeign("abiList", mkConst(toMachineWord(CODETREE.Foreign.abiList())), ValueType(List (String ** abiType), []))
        end

        local
            fun addVal (name : string, value : 'a, typ : types) : unit =
                enterBootstrap (name, mkConst (toMachineWord value), typ)
      
            (* These are only used during the bootstrap phase.  Replacements are installed once
               the appropriate modules of the basis library are compiled. *)
            fun intOfString s =
                let
                val radix =
                    if String.size s >= 3 andalso String.substring(s, 0, 2) = "0x"
                       orelse String.size s >= 4 andalso String.substring(s, 0, 3) = "~0x"
                    then StringCvt.HEX else StringCvt.DEC
                in
                    case StringCvt.scanString (Int.scan radix) s of
                        NONE => raise Conversion "Invalid integer constant"
                      | SOME res => res
                end
        
            fun wordOfString s =
                let
                val radix =
                    if String.size s > 2 andalso String.sub(s, 2) = #"x"
                    then StringCvt.HEX else StringCvt.DEC
                in
                    case StringCvt.scanString (Word.scan radix) s of
                        NONE => raise Conversion "Invalid word constant"
                      | SOME res => res
                end
            
            fun unescapeChar (s: string) : char =
            let
                fun rdr i =
                    if i = size s then NONE
                    else SOME(String.sub(s, i), i+1)
            in
                case Char.scan rdr 0 of
                    NONE => (* Bad conversion *)
                        raise Conversion "Invalid string constant"
                |   SOME(res, _) => res
            end

            val convstringCode = genCode(mkConst(toMachineWord unescapeString), [], 0) ()
            val convintCode = genCode(mkConst(toMachineWord intOfString), [], 0) ()
            val convwordCode = genCode(mkConst(toMachineWord wordOfString), [], 0) ()
            val convcharCode = genCode(mkConst(toMachineWord unescapeChar), [], 0) ()
            
            open Overloads
        in
            (* Flag to indicate which version of Int to compile *)
            val () = addVal ("intIsArbitraryPrecision", intIsArbitraryPrecision, Bool)
            (* Install the overloads now. *)
            val () = addOverload("convString", stringConstr, convstringCode)
            val () = addOverload("convInt", fixedIntConstr, convintCode)
            val () = addOverload("convInt", intInfConstr, convintCode)
            val () = addOverload("convWord", wordConstr, convwordCode)
            val () = addOverload("convChar", charConstr, convcharCode)
        end

    (* The only reason we have vector here is to get equality right.  We need
       vector to be an equality type and to have a specific equality function. *)
        local
            fun polyTypePrinter _ _ = PRETTY.PrettyString "?"
            (* The equality function takes the base equality type as an argument.
               The inner function takes two arguments which are the two vectors to
               compare, checks the lengths and if they're equal applies the
               base equality to each field. *)
            val eqCode =
                mkInlproc(
                    mkProc(
                        mkEnv([
                            (* Length of the items. *)
                            mkDec(0, mkUnary(BuiltIns.MemoryCellLength, mkLoadArgument 0)),
                            mkDec(1, mkUnary(BuiltIns.MemoryCellLength, mkLoadArgument 1)),
                            mkMutualDecs[(2, (* Loop function. *)
                                mkProc(
                                    mkIf(
                                        (* Finished? *)
                                        mkEqualTaggedWord(mkLoadClosure 0, mkLoadArgument 0),
                                        CodeTrue, (* Yes, all equal. *)
                                        mkIf(
                                            mkEval(
                                                TypeValue.extractEquality(mkLoadClosure 2), (* Base equality fn *)
                                                [
                                                    mkLoadOperation(LoadStoreMLWord{isImmutable=true},
                                                        mkLoadClosure 3, mkLoadArgument 0),
                                                    mkLoadOperation(LoadStoreMLWord{isImmutable=true},
                                                        mkLoadClosure 4, mkLoadArgument 0)
                                                ]),
                                            mkEval(mkLoadClosure 1, (* Recursive call with index+1. *)
                                                [
                                                    mkBinary(BuiltIns.WordArith BuiltIns.ArithAdd,
                                                        mkLoadArgument 0, mkConst(toMachineWord 1))
                                                ]),
                                            CodeFalse (* Not equal elements - result false *)
                                        )
                                    ),
                                1, "vector-loop",
                                    [mkLoadLocal 0 (* Length *), mkLoadLocal 2 (* Loop function *), 
                                     mkLoadClosure 0 (* Base equality function *), 
                                     mkLoadArgument 0 (* Vector 0 *), mkLoadArgument 1 (* Vector 1 *)], 0))]
                            ],
                            mkIf(
                                (* Test the lengths. *)
                                mkEqualTaggedWord(mkLoadLocal 0, mkLoadLocal 1),
                                (* Equal - test the contents. *)
                                mkEval(mkLoadLocal 2, [CodeZero]),
                                CodeFalse (* Not same length- result false *)
                            )
                        ),
                        2, "vector-eq", [mkLoadArgument 0], 3),
                    1, "vector-eq()", [], 0)

            val idCode = (* Polytype *)
                let
                    open TypeValue
                    val code =
                        createTypeValue{
                            eqCode=eqCode, printCode=mkConst (toMachineWord (ref polyTypePrinter))}
                in
                    Global (genCode(code, [], 0) ())
                end
        in
            val vectorType =
                makeTypeConstructor("vector",
                    makeFreeId(1, idCode, true, basisDescription "vector"), declInBasis)
            val () = enterGlobalType  ("vector", TypeConstrSet(vectorType, []))
        end

        (* We also need a type with byte-wise equality. *)
        local
            fun monoTypePrinter _ = PRETTY.PrettyString "?"
            (* This is a monotype equality function that takes two byte vectors and compares them
               byte-by-byte for equality.  Because they are vectors of bytes it's unsafe to load
               the whole words which could look like addresses if the bottom bit happens to be zero. *)
            val eqCode =
                mkProc(
                    mkEnv([
                        (* Length of the items. *)
                        mkDec(0, mkUnary(BuiltIns.MemoryCellLength, mkLoadArgument 0)),
                        mkDec(1, mkUnary(BuiltIns.MemoryCellLength, mkLoadArgument 1))
                        ],
                        mkIf(
                            (* Test the lengths. *)
                            mkEqualTaggedWord(mkLoadLocal 0, mkLoadLocal 1),
                            (* Equal - test the contents. *)
                            mkEnv([
                                (* ByteVecEqual takes a byte length so we have to multiply by
                                   the number of bytes per word. *)
                                mkDec(2,
                                    mkBinary(BuiltIns.WordArith BuiltIns.ArithMult,
                                        mkConst(toMachineWord RunCall.bytesPerWord), mkLoadLocal 0))
                                ],
                                mkBlockOperation{kind=BlockOpEqualByte,
                                    leftBase=mkLoadArgument 0, rightBase=mkLoadArgument 1, leftIndex=CodeZero,
                                    rightIndex=CodeZero, length=mkLoadLocal 2}),
                            CodeFalse (* Not same length- result false *)
                        )
                    ),
                    2, "byteVector-eq", [], 3)

            val idCode = (* Polytype *)
                let
                    open TypeValue
                    val code =
                        createTypeValue{
                            eqCode=eqCode, printCode=mkConst (toMachineWord (ref monoTypePrinter))}
                in
                    Global (genCode(code, [], 0) ())
                end
        in
            val byteVectorType =
                makeTypeConstructor("byteVector",
                    makeFreeId(0, idCode, true, basisDescription "byteVector"), declInBasis)
            val () = #enterType bootstrapEnv ("byteVector", TypeConstrSet(byteVectorType, []))
        end

    (* We also need array and Array2.array to be passed through here so that
       they have the special property of being eqtypes even if their argument
       is not.   "array" is defined to be in the global environment. *)
        val () = enterGlobalType  ("array", TypeConstrSet(arrayConstr, []))
        val () = #enterType bootstrapEnv ("array", TypeConstrSet(array2Constr, []))
        val () = #enterType bootstrapEnv ("byteArray", TypeConstrSet(byteArrayConstr, []))

        local
            val eqType = tvA ** tvA ->> Bool
            val eqTempl = TemplPlain { equality=true }
            val eqVal =
                Value{ name = "=", typeOf = ValueType(eqType, [eqTempl]), access = Overloaded Equal,
                       class = ValBound, locations = [DeclaredAt inBasis], references = NONE, instanceTypes = NONE}
            val neqVal =
                Value{ name = "<>", typeOf = ValueType(eqType, [eqTempl]), access = Overloaded NotEqual,
                       class = ValBound, locations = [DeclaredAt inBasis], references = NONE, instanceTypes = NONE}
        in
            val () = enterGlobalValue ("=", eqVal)
            val () = enterGlobalValue ("<>", neqVal)
        end        

        val polyMLEnv = makeStructure(globalEnv, "PolyML")
        val enterPolyMLVal  = #enterVal polyMLEnv

        local
        (* This version of the environment must match that used in the NameSpace
           structure. *)
            open TYPETREE
            (* Create a new structure for them. *)
            val nameSpaceEnv = makeStructure(polyMLEnv, "NameSpace")
            (* Substructures. *)
            val valuesEnv = makeStructure(nameSpaceEnv, "Values")
            and typesEnv = makeStructure(nameSpaceEnv, "TypeConstrs")
            and fixesEnv = makeStructure(nameSpaceEnv, "Infixes")
            and structsEnv = makeStructure(nameSpaceEnv, "Structures")
            and sigsEnv = makeStructure(nameSpaceEnv, "Signatures")
            and functsEnv = makeStructure(nameSpaceEnv, "Functors")

            (* Types for the basic values.  These are opaque. *)    
            val valueVal = makeAndDeclareOpaqueType("value", "PolyML.NameSpace.Values.value", valuesEnv)
            (* Representation of the type of a value. *)
            val Types = makeAndDeclareOpaqueType("typeExpression", "PolyML.NameSpace.Values.typeExpression", valuesEnv)
            val typeVal = makeAndDeclareOpaqueType("typeConstr", "PolyML.NameSpace.TypeConstrs.typeConstr", typesEnv)
            val fixityVal = makeAndDeclareOpaqueType("fixity", "PolyML.NameSpace.Infixes.fixity", fixesEnv)
            val signatureVal = makeAndDeclareOpaqueType("signatureVal", "PolyML.NameSpace.Signatures.signatureVal", sigsEnv)
            val structureVal = makeAndDeclareOpaqueType("structureVal", "PolyML.NameSpace.Structures.structureVal", structsEnv)
            val functorVal = makeAndDeclareOpaqueType("functorVal", "PolyML.NameSpace.Functors.functorVal", functsEnv)

            (* nameSpace type.  Labelled record. *)
            fun createFields(name, vType): { name: string, typeOf: types} list =
            let
                val enterFun = String ** vType ->> Unit
                val lookupFun = String ->> Option vType
                val allFun = Unit ->> List (String ** vType)
            in
                [mkLabelEntry("enter" ^ name, enterFun),
                 mkLabelEntry("lookup" ^ name, lookupFun),
                 mkLabelEntry("all" ^ name, allFun)]
            end
    
            (* We have to use the same names as we use in the env type because we're
               passing "env" values through the bootstrap. *)
            val valTypes = 
               [("Val", valueVal), ("Type", typeVal), ("Fix", fixityVal),
                ("Struct", structureVal), ("Sig", signatureVal), ("Funct", functorVal)]
    
            val fields = List.foldl (fn (p,l) => createFields p @ l) [] valTypes
    
            val recordType =
                makeTypeAbbreviation("nameSpace", "PolyML.NameSpace.nameSpace", 0, mkLabelled(sortLabels fields, true), declInBasis);
            val () = #enterType nameSpaceEnv ("nameSpace", TypeConstrSet(recordType, []));
            
            (* The result type of the compiler includes valueVal etc. *)
            val resultFields = List.map TYPETREE.mkLabelEntry
                [("values", List(String ** valueVal)),
                 ("fixes", List(String ** fixityVal)),
                 ("types", List(String ** typeVal)),
                 ("structures", List(String ** structureVal)),
                 ("signatures", List(String ** signatureVal)),
                 ("functors", List(String ** functorVal))]
          in
            val nameSpaceType = mkTypeConstruction ("nameSpace", recordType, [], declInBasis)
            val execResult = mkLabelled(sortLabels resultFields, true)
            type execResult =
                { fixes: (string * fixStatus) list, values: (string * values) list,
                  structures: (string * structVals) list, signatures: (string * signatures) list,
                  functors: (string * functors) list, types: (string * typeConstrSet) list }

            val valueVal = valueVal
            val typeVal = typeVal
            val fixityVal = fixityVal
            val signatureVal = signatureVal
            val structureVal = structureVal
            val functorVal = functorVal
            
            val Types = Types
            
            val valuesEnv = valuesEnv
            and typesEnv = typesEnv
            and fixesEnv = fixesEnv
            and structsEnv = structsEnv
            and sigsEnv = sigsEnv
            and functsEnv = functsEnv

         end
         
        local
            val typeconstr = locationConstr
            val () = #enterType polyMLEnv ("location", typeconstr);
        in
            val Location = mkTypeConstruction ("location", tsConstr typeconstr, [], declInBasis)
        end

        (* Interface to the debugger. *)
        local
            open TYPETREE
            val debuggerEnv = makeStructure(polyMLEnv, "DebuggerInterface")
            (* Make these opaque at this level. *)
            val locationPropList =
                makeAndDeclareOpaqueType("locationPropList", "PolyML.DebuggerInterface.locationPropList", debuggerEnv)
            val typeId =
                makeAndDeclareOpaqueType("typeId", "PolyML.DebuggerInterface.typeId", debuggerEnv)
            val machineWordType =
                makeAndDeclareOpaqueType("machineWord", "PolyML.DebuggerInterface.machineWord", debuggerEnv)
            (* For long term security keep these as different from global types and sigs.
               Values in the static environment need to be copied before they are global. *)
            val localType =
                makeAndDeclareOpaqueType("localType", "PolyML.DebuggerInterface.localType", debuggerEnv)
            val localTypeConstr =
                makeAndDeclareOpaqueType("localTypeConstr", "PolyML.DebuggerInterface.localTypeConstr", debuggerEnv)
            val localSig =
                makeAndDeclareOpaqueType("localSig", "PolyML.DebuggerInterface.localSig", debuggerEnv)
            open DEBUGGER
            (* Entries in the static list.  This type is only used within the implementation of
               DebuggerInterface in the basis library and does not appear in the final signature. *)
            val environEntryConstr =
                makeTypeConstructor("environEntry",
                    makeFreeId(0, defaultEqAndPrintCode(), false,
                        basisDescription "PolyML.DebuggerInterface.environEntry"), declInBasis)
            val environEntryType =
                mkTypeConstruction ("environEntry", environEntryConstr, [], declInBasis)
            val constrs = (* Order is significant. *)
               [ ("EnvEndFunction",     mkProductType[String, Location, localType]),
                 ("EnvException",       mkProductType[String, localType, locationPropList]),
                 ("EnvStartFunction",   mkProductType[String, Location, localType]),
                 ("EnvStructure",       mkProductType[String, localSig, locationPropList]),
                 ("EnvTConstr",         String ** localTypeConstr),
                 ("EnvTypeid",          typeId ** typeId),
                 ("EnvVConstr",         mkProductType[String, localType, Bool, Int, locationPropList]),
                 ("EnvValue",           mkProductType[String, localType, locationPropList])
                 ]
            (* This representation must match the representation defined in DEBUGGER_.sml. *)
            val numConstrs = List.length constrs
            val {constrs=constrReps, ...} = chooseConstrRepr(constrs, 0)
            val constructors =
                ListPair.map (fn ((s,t), code) =>
                    mkGconstr(s, ValueType(t ->> environEntryType, []), code, false, numConstrs, declInBasis))
                        (constrs, constrReps)
            fun valName (Value{name, ...}) = name
            val () = List.app (fn c => #enterVal debuggerEnv(valName c, c)) constructors
            (* Put these constructors onto the type. *)
            val () = #enterType debuggerEnv ("environEntry", TypeConstrSet(environEntryConstr, constructors))
            (* Debug state type. *)
            val debugStateConstr =
                makeTypeAbbreviation("debugState", "PolyML.DebuggerInterface.debugState", 0,
                    mkProductType[List environEntryType, List machineWordType, Location], declInBasis)
            val () = #enterType debuggerEnv ("debugState", TypeConstrSet(debugStateConstr, []))
            val debugStateType = mkTypeConstruction ("debugState", debugStateConstr, [], declInBasis)
        in
            val () = applyList (fn (name, v, t) =>
                                #enterVal debuggerEnv (name, mkGvar (name, ValueType(t, []), mkConst v, declInBasis)))
                [
                    ("makeValue",
                        toMachineWord(makeValue: debugState -> string * valueType * locationProp list * machineWord -> values),
                        debugStateType ->> mkProductType[String, localType, locationPropList, machineWordType] ->> valueVal),
                    ("makeException",
                        toMachineWord(makeException: debugState -> string * valueType * locationProp list * machineWord -> values),
                        debugStateType ->> mkProductType[String, localType, locationPropList, machineWordType] ->> valueVal),
                    ("makeConstructor",
                        toMachineWord(makeConstructor: debugState -> string * valueType * bool * int * locationProp list * machineWord -> values),
                        debugStateType ->> mkProductType[String, localType, Bool, Int, locationPropList, machineWordType] ->> valueVal),
                    ("makeAnonymousValue",
                        toMachineWord(makeAnonymousValue: debugState -> valueType * machineWord -> values),
                        debugStateType ->> mkProductType[localType, machineWordType] ->> valueVal),
                    ("makeStructure",
                        toMachineWord(makeStructure: debugState -> string * signatures * locationProp list * machineWord -> structVals),
                        debugStateType ->> mkProductType[String, localSig, locationPropList, machineWordType] ->> structureVal),
                    ("makeTypeConstr",
                        toMachineWord(makeTypeConstr: debugState -> typeConstrSet -> typeConstrSet),
                        debugStateType ->> localTypeConstr ->> typeVal),
                    ("unitValue", toMachineWord(mkGvar("", ValueType(unitType, []), CodeZero, []): values), valueVal), (* Used as a default *)

                    
                    ("setOnEntry", toMachineWord(setOnEntry: (string * PolyML.location -> unit) option -> unit),
                        Option (String ** Location ->> Unit) ->> Unit),
                    ("setOnExit", toMachineWord(setOnExit: (string * PolyML.location -> unit) option -> unit),
                        Option (String ** Location ->> Unit) ->> Unit),
                    ("setOnExitException", toMachineWord(setOnExitException: (string * PolyML.location -> exn -> unit) option -> unit),
                        Option (String ** Location ->> Exn ->> Unit) ->> Unit),
                    ("setOnBreakPoint", toMachineWord(setOnBreakPoint: (PolyML.location * bool ref -> unit) option -> unit),
                        Option (Location ** Ref Bool ->> Unit) ->> Unit)
                ]
        end

        local
            val typeconstr = contextConstr
        in
            val () = #enterType polyMLEnv ("context", typeconstr);
            val () = List.app(fn(tv as Value{name, ...}) => #enterVal polyMLEnv(name, tv))
                        (tsConstructors typeconstr)
        end

        local
            val typeconstr = prettyConstr
        in
            val () = #enterType polyMLEnv ("pretty", typeconstr);
            val () = List.app(fn(tv as Value{name, ...}) => #enterVal polyMLEnv(name, tv))
                        (tsConstructors typeconstr)
            val PrettyType = mkTypeConstruction ("pretty", tsConstr typeconstr, [], declInBasis)
        end

        (* Type-dependent functions. *)
        fun mkSpecialFun (name, typeof, templs, opn) =
            Value{ name = name, typeOf = ValueType(typeof, templs), access = Overloaded opn, class = ValBound,
                   locations = [DeclaredAt inBasis], references = NONE, instanceTypes = NONE}

        local
            val printVal =
                mkSpecialFun("print", tvA ->> tvA, templates 1, Print)
        in
            val () = enterPolyMLVal ("print", printVal)
        end;

        local
            val makeStringVal =
                mkSpecialFun("makestring", tvA ->> String, templates 1, MakeString)
        in
            val () = enterPolyMLVal ("makestring", makeStringVal)
        end;

        local
            val prettyRepVal  =
                mkSpecialFun("prettyRepresentation", tvA ** fixedIntType ->> PrettyType,
                    templates 1, GetPretty)
        in
            val () = enterPolyMLVal ("prettyRepresentation", prettyRepVal)
        end;
 
        local
            (* addPrettyPrinter installs a pretty printer for a type. *)
            val addPrettyType = (TYPETREE.fixedIntType ->> tvB ->> tvA ->> PrettyType) ->> Unit
            val addPrettyVal =
                mkSpecialFun("addPrettyPrinter", addPrettyType, templates 2, AddPretty)
        in
            val () = enterPolyMLVal ("addPrettyPrinter", addPrettyVal)
        end;

        (* This goes in RunCall since it's only for the basis library. *)
        local
            (* addOverload.  N.B. The order of the type variables is significant.  The code in VALUE_OPS expects
               the first bound variable to be the function result and the second bound variable to be the function
               argument. *)
            val addOverloadType = (tvB ->> tvA) ->> String ->> Unit
            val addOverloadVal  = mkSpecialFun("addOverload", addOverloadType, templates 2, AddOverload)
        in
            val () = #enterVal runCallEnv ("addOverload", addOverloadVal)
        end

        local
            (* Add a function to switch the default integer type. *)
            fun setType isArbitrary =
                setPreferredInt(if isArbitrary then intInfConstr else fixedIntConstr)
        in
            val () = #enterVal runCallEnv
                        ("setDefaultIntTypeArbitrary",
                            mkGvar ("setDefaultIntTypeArbitrary", ValueType(Bool ->> Unit, []), mkConst (toMachineWord setType), declInBasis))
        end

        local
            val sourceLocVal  = mkSpecialFun("sourceLocation", Unit ->> Location, [], GetLocation)
        in
            val () = enterPolyMLVal ("sourceLocation", sourceLocVal);
        end;

        local
            (* This is used as one of the arguments to the compiler function. *)
            open TYPETREE
            val uniStructEnv = makeStructure(bootstrapEnv, "Universal")

            fun enterUniversal (name : string, entry : codetree, typ: types, templates) : unit =
            let
                val code = entry
                val value = mkGvar (name, ValueType(typ, templates), code, declInBasis);
            in
                #enterVal uniStructEnv (name, value)
            end;

            local
                fun polyTypePrinter _ _ = PRETTY.PrettyString "?"
                open TypeValue
                val idCode =
                let
                    val code =
                        createTypeValue{
                                eqCode=CodeZero, (* Not an equality type *)
                                printCode=mkConst (toMachineWord (ref polyTypePrinter))}
                in
                    Global (genCode(code, [], 0) ())
                end
            in
                (* type 'a tag *)
                val tagConstr =
                    makeTypeConstructor("tag",
                        makeFreeId(1, idCode, false, basisDescription "tag"), declInBasis);
                val () = #enterType uniStructEnv ("tag", TypeConstrSet(tagConstr, []))
            end

            (* type universal *)
            val univConstr =
                makeTypeConstructor("universal",
                        makeFreeId(0, defaultEqAndPrintCode(), false, basisDescription "universal"), declInBasis);
            val () = #enterType uniStructEnv ("universal",  TypeConstrSet(univConstr, []));

            fun Tag base = mkTypeConstruction ("tag", tagConstr, [base], declInBasis)
            val Universal = mkTypeConstruction ("universal", univConstr, [], declInBasis)

            (* val tagInject  : 'a tag -> 'a -> universal *)
            val injectType = Tag tvA ->> tvA ->> Universal
            val () = enterUniversal ("tagInject",
                            mkConst (toMachineWord (Universal.tagInject: 'a Universal.tag -> 'a -> Universal.universal)),
                            injectType, templates 1)
            (* We don't actually need tagIs and tagProject since this is only used for
               the compiler.  Universal is redefined in the basis library. *)          
            val projectType = Tag tvA ->> Universal ->> tvA 
            val () = enterUniversal ("tagProject",
                            mkConst (toMachineWord(Universal.tagProject: 'a Universal.tag -> Universal.universal -> 'a)),
                            projectType, templates 1)
            val testType = Tag tvA ->> Universal ->> Bool
            val () = enterUniversal ("tagIs",
                            mkConst (toMachineWord(Universal.tagIs: 'a Universal.tag -> Universal.universal -> bool)),
                            testType, templates 1)
         in
            val Tag = Tag and Universal = Universal
        end
        
        local
            open TYPETREE
            (* Parsetree properties datatype. *)
            val propConstr =
                makeTypeConstructor("ptProperties",
                    makeFreeId(0, defaultEqAndPrintCode(), false, basisDescription "PolyML.ptProperties"), declInBasis);
            val PtProperties = mkTypeConstruction ("ptProperties", propConstr, [], declInBasis)

            (* Parsetree type. *)
            val parseTreeConstr =
                makeTypeAbbreviation("parseTree", "PolyML.parseTree", 0, Location ** List PtProperties, declInBasis);    
            val ParseTree = mkTypeConstruction ("parseTree", parseTreeConstr, [], declInBasis)
            val () = #enterType polyMLEnv ("parseTree", TypeConstrSet(parseTreeConstr, []));

            val constrs = (* Order is significant. *)
               [ ("PTbreakPoint",       Ref Bool),
                 ("PTcompletions",      List String),
                 ("PTdeclaredAt",       Location),
                 ("PTdefId",            fixedIntType),
                 ("PTfirstChild",       Unit ->> ParseTree),
                 ("PTnextSibling",      Unit ->> ParseTree),
                 ("PTopenedAt",         Location),
                 ("PTparent",           Unit ->> ParseTree),
                 ("PTpreviousSibling",  Unit ->> ParseTree),
                 ("PTprint",            fixedIntType ->> PrettyType),
                 ("PTreferences",       Bool ** List Location),
                 ("PTrefId",            fixedIntType),
                 ("PTstructureAt",      Location),
                 ("PTtype",             Types)
                 ];
            (* This representation must match the representation defined in ExportTree.sml. *)
            val numConstrs = List.length constrs
            val {constrs=constrReps, ...} = chooseConstrRepr(constrs, 0)
            fun valName (Value{name, ...}) = name
            val constructors =
                ListPair.map (fn ((s,t), code) =>
                    mkGconstr(s, ValueType(t ->> PtProperties, []), code, false, numConstrs, declInBasis))
                        (constrs, constrReps)
            val () = List.app (fn c => #enterVal polyMLEnv(valName c, c)) constructors
            (* Put these constructors onto the type. *)
            val () = #enterType polyMLEnv ("ptProperties", TypeConstrSet(propConstr, constructors));

        in
            val ParseTree = ParseTree
            and PtProperties = PtProperties
        end

        local
            open TYPETREE
 
            val compilerType : types =
                mkProductType[nameSpaceType, Unit ->> Option Char, List Universal] ->>
                    mkProductType[Option ParseTree, Option (Unit ->> execResult)]
            type compilerType =
                    nameSpace * (unit -> char option) * Universal.universal list -> exportTree option * (unit->execResult) option
        in
            val () = enterBootstrap ("use", mkConst (toMachineWord ((useIntoEnv globalTable []): string -> unit)), String ->> Unit)            
            val () =
                enterBootstrap ("useWithParms",
                    mkConst (toMachineWord ((useIntoEnv globalTable): Universal.universal list -> string -> unit)),
                    List Universal ->> String ->> Unit)            
            val () = enterPolyMLVal("compiler", mkGvar ("compiler", ValueType(compilerType, []), mkConst (toMachineWord (compiler: compilerType)), declInBasis));
            val () = enterBootstrap("globalSpace", mkConst (toMachineWord(gEnvAsNameSpace globalTable: nameSpace)), nameSpaceType)
            (* Add a print function so we can print a message at the start of a bootstrap phase. *)
            val () = enterBootstrap("print", mkConst (toMachineWord TextIO.print), String ->> Unit)
        end;
        
        local
            val addType = tvA ** tvA ->> tvA
            val negType = tvA ->> tvA
            val cmpType = tvA ** tvA ->> Bool

            fun mkOverloaded name typeof =
                Value{ name = name, typeOf = ValueType(typeof, [TemplOverload name]), access = Overloaded TypeDep,
                       class = ValBound, locations = [DeclaredAt inBasis], references = NONE, instanceTypes = NONE}
        in
            val () = enterGlobalValue ("+", mkOverloaded "+"   addType);
            val () = enterGlobalValue ("-", mkOverloaded "-"   addType);
            val () = enterGlobalValue ("*", mkOverloaded "*"   addType);
            val () = enterGlobalValue ("~", mkOverloaded "~"   negType);
            val () = enterGlobalValue ("abs", mkOverloaded "abs" negType);
            val () = enterGlobalValue (">=", mkOverloaded ">="  cmpType);
            val () = enterGlobalValue ("<=", mkOverloaded "<="  cmpType);
            val () = enterGlobalValue (">", mkOverloaded ">"   cmpType);
            val () = enterGlobalValue ("<", mkOverloaded "<"   cmpType);
            (* The following overloads are added in ML97 *)
            val () = enterGlobalValue ("div", mkOverloaded "div"   addType);
            val () = enterGlobalValue ("mod", mkOverloaded "mod"   addType);
            val () = enterGlobalValue ("/", mkOverloaded "/"   addType);
        end;

        local
            open DEBUG;
            local
                open TYPETREE
                val fields =
                [
                    mkLabelEntry("location", Location), mkLabelEntry("hard", Bool),
                    mkLabelEntry("message", PrettyType), mkLabelEntry("context", Option PrettyType)
                ]
            in
                val errorMessageProcType = mkLabelled(sortLabels fields, true) ->> Unit
                type errorMessageProcType =
                    { location: location, hard: bool, message: pretty, context: pretty option } -> unit
            end

            local
                open TYPETREE
                val optNav = Option(Unit->>ParseTree)
                val fields =
                [
                    mkLabelEntry("parent", optNav),
                    mkLabelEntry("next", optNav),
                    mkLabelEntry("previous", optNav)
                ]
            in
                val navigationType = mkLabelled(sortLabels fields, true)
                type navigationType =
                    { parent: (unit->exportTree) option, next: (unit->exportTree) option, previous: (unit->exportTree) option }
            end
            type 'a tag = 'a Universal.tag
        in
            val () = applyList (fn (name, v, t) => enterBootstrap(name, mkConst v, t))
                [
                ("compilerVersion",        toMachineWord (VERSION.compilerVersion: string),     String),
                ("compilerVersionNumber",  toMachineWord (VERSION.versionNumber: int),          Int),
                ("lineNumberTag",          toMachineWord (lineNumberTag : (unit->FixedInt.int) tag),     Tag (Unit->>fixedIntType)),
                ("offsetTag",              toMachineWord (offsetTag: (unit->FixedInt.int) tag),          Tag (Unit->>fixedIntType)),
                ("fileNameTag",            toMachineWord (fileNameTag: string tag),             Tag String),
                ("bindingCounterTag",      toMachineWord (bindingCounterTag: (unit->FixedInt.int) tag),  Tag (Unit->>fixedIntType)),
                ("maxInlineSizeTag",       toMachineWord (maxInlineSizeTag: FixedInt.int tag),           Tag fixedIntType),
                ("assemblyCodeTag",        toMachineWord (assemblyCodeTag: bool tag),           Tag Bool),
                ("parsetreeTag",           toMachineWord (parsetreeTag: bool tag),              Tag Bool),
                ("codetreeTag",            toMachineWord (codetreeTag: bool tag),               Tag Bool),
                ("icodeTag",               toMachineWord (icodeTag: bool tag),            Tag Bool),
                ("lowlevelOptimiseTag",    toMachineWord (lowlevelOptimiseTag: bool tag),       Tag Bool),
                ("codetreeAfterOptTag",    toMachineWord (codetreeAfterOptTag: bool tag),       Tag Bool),
                ("inlineFunctorsTag",      toMachineWord (inlineFunctorsTag: bool tag),         Tag Bool),
                ("compilerDebugTag",       toMachineWord (compilerDebugTag: int tag),           Tag Int),
                ("debugTag",               toMachineWord (debugTag: bool tag),                  Tag Bool),
                ("printDepthFunTag",       toMachineWord (DEBUG.printDepthFunTag: (unit->FixedInt.int) tag), Tag (Unit->>fixedIntType)),
                ("errorDepthTag",          toMachineWord (DEBUG.errorDepthTag: FixedInt.int tag),        Tag fixedIntType),
                ("lineLengthTag",          toMachineWord (DEBUG.lineLengthTag: FixedInt.int tag),        Tag fixedIntType),
                ("profileAllocationTag",   toMachineWord (DEBUG.profileAllocationTag: FixedInt.int tag), Tag fixedIntType),
                ("printOutputTag",         toMachineWord (PRETTY.printOutputTag: (pretty->unit) tag),  Tag (PrettyType->>Unit)) ,               
                ("compilerOutputTag",      toMachineWord (PRETTY.compilerOutputTag: (pretty->unit) tag), Tag (PrettyType->>Unit)),
                ("errorMessageProcTag",    toMachineWord (LEX.errorMessageProcTag: errorMessageProcType tag), Tag errorMessageProcType),
                ("rootTreeTag",            toMachineWord (EXPORTTREE.rootTreeTag: navigation tag), Tag navigationType),
                ("reportUnreferencedIdsTag", toMachineWord (reportUnreferencedIdsTag: bool tag), Tag Bool),
                ("reportExhaustiveHandlersTag", toMachineWord (reportExhaustiveHandlersTag: bool tag), Tag Bool),
                ("narrowOverloadFlexRecordTag", toMachineWord (narrowOverloadFlexRecordTag: bool tag), Tag Bool),
                ("languageExtensionsTag",  toMachineWord (languageExtensionsTag: bool tag), Tag Bool),
                ("createPrintFunctionsTag", toMachineWord (createPrintFunctionsTag: bool tag), Tag Bool),
                ("reportDiscardedValuesTag", toMachineWord (reportDiscardedValuesTag: FixedInt.int tag), Tag fixedIntType)
                 ]
        end;
 

    (* PolyML.CodeTree structure.  This exports the CodeTree structure into the ML space. *)
        local
            open CODETREE
            val codetreeEnv = makeStructure(polyMLEnv, "CodeTree")

            fun createType typeName =
                makeAndDeclareOpaqueType(typeName, "PolyML.CodeTree." ^ typeName, codetreeEnv)

            val CodeTree = createType "codetree"
            and MachineWord = createType "machineWord"
            and CodeBinding = createType "codeBinding"

            (* For the moment export these only for the general argument and result types. *)
            fun simpleFn (code, nArgs, name, closure, nLocals) =
                mkFunction{body=code, argTypes=List.tabulate(nArgs, fn _ => GeneralType),
                           resultType=GeneralType, name=name, closure=closure, numLocals=nLocals}
            and simpleInlineFn (code, nArgs, name, closure, nLocals) =
                mkInlineFunction{body=code, argTypes=List.tabulate(nArgs, fn _ => GeneralType),
                           resultType=GeneralType, name=name, closure=closure, numLocals=nLocals}
            and simpleCall(func, args) =
                mkCall(func, List.map (fn c => (c, GeneralType)) args, GeneralType)

        in
            val CodeTree = CodeTree

            val () = applyList (fn (name, v, t) =>
                                #enterVal codetreeEnv (name, mkGvar (name, ValueType(t, []), mkConst v, declInBasis)))
                [
                ("pretty", toMachineWord (CODETREE.pretty: codetree -> pretty), CodeTree ->> PrettyType),
                ("mkConstant", toMachineWord(mkConst: machineWord -> codetree), MachineWord ->> CodeTree),
                ("genCode", toMachineWord (genCode: codetree * Universal.universal list * int -> (unit->codetree)),
                        mkProductType[CodeTree, List Universal, Int] ->> (Unit ->> CodeTree)),
                ("evalue", toMachineWord (evalue: codetree -> machineWord option), CodeTree ->> Option MachineWord),
                ("mkFunction", toMachineWord (simpleFn: codetree * int * string * codetree list * int -> codetree),
                    mkProductType[CodeTree, Int, String, List CodeTree, Int] ->> CodeTree),
                ("mkInlineFunction", toMachineWord (simpleInlineFn: codetree * int * string * codetree list * int -> codetree),
                    mkProductType[CodeTree, Int, String, List CodeTree, Int] ->> CodeTree),
                ("mkCall", toMachineWord (simpleCall: codetree * codetree list -> codetree), CodeTree ** List CodeTree ->> CodeTree),
                ("mkLoadLocal", toMachineWord (mkLoadLocal: int -> codetree), Int ->> CodeTree),
                ("mkLoadArgument", toMachineWord (mkLoadArgument: int -> codetree), Int ->> CodeTree),
                ("mkLoadClosure", toMachineWord (mkLoadClosure: int -> codetree), Int ->> CodeTree),
                ("mkDec", toMachineWord (mkDec: int * codetree -> codeBinding), Int ** CodeTree ->> CodeBinding),
                ("mkInd", toMachineWord (mkInd: int * codetree -> codetree), Int ** CodeTree ->> CodeTree),
                ("mkIf", toMachineWord (mkIf: codetree * codetree * codetree -> codetree),
                    mkProductType[CodeTree, CodeTree, CodeTree] ->> CodeTree),
                ("mkWhile", toMachineWord (mkWhile: codetree * codetree -> codetree), CodeTree ** CodeTree ->> CodeTree),
                ("mkLoop", toMachineWord (mkLoop: codetree list -> codetree), List CodeTree ->> CodeTree),
                ("mkBeginLoop", toMachineWord (mkBeginLoop: codetree * (int * codetree) list -> codetree),
                    CodeTree ** List(Int ** CodeTree) ->> CodeTree),
                ("mkEnv", toMachineWord (mkEnv: codeBinding list * codetree -> codetree),
                    List CodeBinding ** CodeTree ->> CodeTree),
                ("mkMutualDecs", toMachineWord (mkMutualDecs: (int * codetree) list -> codeBinding),
                    List(Int ** CodeTree) ->> CodeBinding),
                ("mkTuple", toMachineWord (mkTuple: codetree list -> codetree), List CodeTree ->> CodeTree),
                ("mkRaise", toMachineWord (mkRaise: codetree -> codetree), CodeTree ->> CodeTree),
                ("mkHandle", toMachineWord (mkHandle: codetree * codetree * int -> codetree),
                        mkProductType[CodeTree, CodeTree, Int] ->> CodeTree),
                ("mkNullDec", toMachineWord (mkNullDec: codetree -> codeBinding), CodeTree ->> CodeBinding)
                ]
        end

        local (* Finish off the NameSpace structure now we have types such as pretty. *)
            open TYPETREE
 
            (* The exported versions expect full name spaces as arguments.  Because we convert
               the exported versions to machineWord and give them types as data structures the
               compiler can't actually check that the type we give matched the internal type. *)
            fun makeTypeEnv NONE =
                { lookupType = fn _ => NONE, lookupStruct = fn _ => NONE }
            |   makeTypeEnv(SOME(nameSpace: nameSpace)): printTypeEnv =
                {
                    lookupType = fn s => case #lookupType nameSpace s of NONE => NONE | SOME t => SOME(t, NONE),
                    lookupStruct = fn s => case #lookupStruct nameSpace s of NONE => NONE | SOME t => SOME(t, NONE)
                }

            local (* Values substructure.  This also has operations related to type expressions. *)
                fun codeForValue (Value{access = Global code, class = ValBound, ...}) = code
                |   codeForValue _ = raise Fail "Not a global value"
                and exportedDisplayTypeExp(ty, depth, nameSpace: nameSpace option) =
                    TYPETREE.display(ty, depth, makeTypeEnv nameSpace)
                and exportedDisplayValues(valu, depth, nameSpace: nameSpace option) =
                        displayValues(valu, depth, makeTypeEnv nameSpace)
                and propsForValue (Value {locations, typeOf=ValueType(typeOf, _), ...}) = PTtype typeOf :: mapLocationProps locations

                fun isException (Value{class = Exception, ...}) = true
                |   isException _ = false

                fun valName (Value{name, ...}) = name
                fun valTypeOf (Value{typeOf=ValueType(typeOf, _), ...}) = typeOf

                fun isConstructor (Value{class=Constructor _, ...}) = true
                |   isConstructor (Value{class=Exception, ...})     = true
                |   isConstructor _                                  = false;
    
            in
                val () = applyList (fn (name, v, t) =>
                                    #enterVal valuesEnv (name, mkGvar (name, ValueType(t, []), mkConst v, declInBasis)))
                    [
                        ("name",    toMachineWord (valName: values -> string), valueVal ->> String),
                        ("print",   toMachineWord (printValues: values * FixedInt.int -> pretty),
                                        mkProductType[valueVal, fixedIntType] ->> PrettyType),
                        ("printWithType", toMachineWord (exportedDisplayValues: values * FixedInt.int * nameSpace option -> pretty),
                                        mkProductType[valueVal, fixedIntType, Option nameSpaceType] ->> PrettyType),
                        ("printType",    toMachineWord(exportedDisplayTypeExp: types * FixedInt.int * nameSpace option -> pretty),
                            mkProductType[Types, fixedIntType, Option nameSpaceType] ->> PrettyType),
                        ("typeof",  toMachineWord (valTypeOf: values -> types), valueVal ->> Types),
                        ("code",    toMachineWord (codeForValue: values -> codetree), valueVal ->> CodeTree),
                        ("properties",   toMachineWord (propsForValue: values ->ptProperties list),
                            valueVal ->> List PtProperties),
                        ("isConstructor", toMachineWord(isConstructor: values -> bool), valueVal ->> Bool),
                        ("isException", toMachineWord(isException: values -> bool), valueVal ->> Bool)
                    ]
            end

            local (* TypeConstrs substructure. *)
                fun exportedDisplayTypeConstr(tyCons, depth, nameSpace: nameSpace option) =
                    TYPETREE.displayTypeConstrs(tyCons, depth, makeTypeEnv nameSpace)
                and propsForTypeConstr (TypeConstrSet(TypeConstrs {locations, ...}, _)) = mapLocationProps locations

                and nameForType (TypeConstrSet(TypeConstrs{name, ...}, _)) = name
            in
                val () = applyList (fn (name, v, t) =>
                                    #enterVal typesEnv (name, mkGvar (name, ValueType(t, []), mkConst v, declInBasis)))
                    [
                    ("name", toMachineWord(nameForType: typeConstrSet -> string), typeVal ->> String),
                    ("print",
                        toMachineWord (exportedDisplayTypeConstr: typeConstrSet * FixedInt.int * nameSpace option -> pretty),
                        mkProductType[typeVal, fixedIntType, Option nameSpaceType] ->> PrettyType),
                    ("properties",   toMachineWord (propsForTypeConstr: typeConstrSet ->ptProperties list),
                        typeVal ->> List PtProperties)
                    ]
            end

            local (* Structures substructure *)
                fun exportedDisplayStructs(str, depth, nameSpace: nameSpace option) =
                    displayStructures(str, depth, makeTypeEnv nameSpace)

                and codeForStruct (Struct{access = Global code, ...}) = code
                |   codeForStruct _ = raise Fail "Not a global structure"

                and propsForStruct (Struct {locations, ...}) = mapLocationProps locations

                and nameForStruct (Struct{name, ...}) = name
                
                fun nameSpaceForStruct(baseStruct as Struct{signat=Signatures { tab, ...}, ...}): nameSpace =
                let
                    open UNIVERSALTABLE
  
                    fun lookupVal s =
                    case univLookup (tab, valueVar, s) of
                        NONE => NONE
                    |   SOME v => SOME(makeSelectedValue(v, baseStruct))

                    and lookupType s =
                    case univLookup (tab, typeConstrVar, s) of
                        NONE => NONE
                    |   SOME t => SOME(makeSelectedType(t, baseStruct))

                    and lookupStruct s =
                    case univLookup (tab, structVar, s) of
                        NONE => NONE
                    |   SOME s => SOME(makeSelectedStructure(s, baseStruct))
                    
                    local
                        fun extractItems t tab =
                            UNIVERSALTABLE.fold
                                (fn (s, u, l) =>
                                    if Universal.tagIs t u
                                    then (s, Universal.tagProject t u) :: l else l
                                ) [] tab
                    in
                        fun allValues() =
                            map(fn (s, v) => (s, makeSelectedValue(v, baseStruct))) (extractItems valueVar tab)
                        and allTypes() =
                            map(fn (s, t) => (s, makeSelectedType(t, baseStruct))) (extractItems typeConstrVar tab)
                        and allStructs() =
                            map(fn (s, v) => (s, makeSelectedStructure(v, baseStruct))) (extractItems structVar tab)
                    end

                    fun enterFunction _ = raise Fail "updating a structure is not possible."
                    (* Raise an exception for any attempt to enter a new value.  Return
                       empty for the classes that can't exist in a structure. *)
                in
                    {
                        lookupVal = lookupVal,
                        lookupType = lookupType,
                        lookupStruct = lookupStruct,
                        lookupFix = fn _ => NONE,
                        lookupSig = fn _ => NONE,
                        lookupFunct = fn _ => NONE,
                        
                        enterVal = enterFunction,
                        enterType = enterFunction,
                        enterFix = enterFunction,
                        enterStruct = enterFunction,
                        enterSig = enterFunction,
                        enterFunct = enterFunction,

                        allVal = allValues,
                        allType = allTypes,
                        allStruct = allStructs,
                        allFix = fn () => [],
                        allSig = fn () => [],
                        allFunct = fn () => []
                    }
                end
            in
                val () = applyList (fn (name, v, t) =>
                                #enterVal structsEnv (name, mkGvar (name, ValueType(t, []), mkConst v, declInBasis)))
                    [
                    ("name", toMachineWord(nameForStruct: structVals -> string), structureVal ->> String),
                    ("print",
                        toMachineWord (exportedDisplayStructs: structVals * FixedInt.int * nameSpace option -> pretty),
                        mkProductType[structureVal, fixedIntType, Option nameSpaceType] ->> PrettyType),
                    ("code",   toMachineWord (codeForStruct: structVals -> codetree), structureVal ->> CodeTree),
                    ("properties",   toMachineWord (propsForStruct: structVals ->ptProperties list),
                        structureVal ->> List PtProperties),
                    ("contents", toMachineWord(nameSpaceForStruct: structVals -> nameSpace), structureVal ->> nameSpaceType) 
                    ]
            end
            
            local (* Signatures substructure *)
                fun exportedDisplaySigs(sign, depth, nameSpace: nameSpace option) =
                        displaySignatures(sign, depth, makeTypeEnv nameSpace)

                and propsForSig (Signatures {locations, ...}) = mapLocationProps locations

                and nameForSig (Signatures{name, ...}) = name
            in
                val () = applyList (fn (name, v, t) =>
                                    #enterVal sigsEnv (name, mkGvar (name, ValueType(t, []), mkConst v, declInBasis)))
                    [
                    ("name", toMachineWord(nameForSig: signatures -> string), signatureVal ->> String),
                    ("print",
                        toMachineWord (exportedDisplaySigs: signatures * FixedInt.int * nameSpace option -> pretty),
                        mkProductType[signatureVal, fixedIntType, Option nameSpaceType] ->> PrettyType),
                    ("properties",   toMachineWord (propsForSig: signatures ->ptProperties list),
                        signatureVal ->> List PtProperties)
                   ]
            end

            local (* Functors substructure *)
                fun exportedDisplayFunctors(funct, depth, nameSpace: nameSpace option) =
                        displayFunctors(funct, depth, makeTypeEnv nameSpace)

                and codeForFunct (Functor{access = Global code, ...}) = code
                |   codeForFunct _ = raise Fail "Not a global functor"
            
                and propsForFunctor (Functor {locations, ...}) = mapLocationProps locations

                and nameForFunctor (Functor{name, ...}) = name
            in
                val () = applyList (fn (name, v, t) =>
                                    #enterVal functsEnv (name, mkGvar (name, ValueType(t, []), mkConst v, declInBasis)))
                    [
                    ("name", toMachineWord(nameForFunctor: functors -> string), functorVal ->> String),
                    ("print",
                        toMachineWord (exportedDisplayFunctors: functors * FixedInt.int * nameSpace option -> pretty),
                        mkProductType[functorVal, fixedIntType, Option nameSpaceType] ->> PrettyType),
                    ("code",    toMachineWord (codeForFunct: functors -> codetree), functorVal ->> CodeTree),
                    ("properties",   toMachineWord (propsForFunctor: functors ->ptProperties list),
                        functorVal ->> List PtProperties)
                   ]
            end

            local (* Infixes substructure *)
                fun nameForFix(FixStatus(s, _)) = s
            in
                val () = applyList (fn (name, v, t) =>
                                    #enterVal fixesEnv (name, mkGvar (name, ValueType(t, []), mkConst v, declInBasis)))
                    [
                    ("name", toMachineWord(nameForFix: fixStatus -> string), fixityVal ->> String),
                    ("print",
                        toMachineWord (displayFixStatus: fixStatus -> pretty),
                        fixityVal ->> PrettyType)
                   ]
            end
        in
        end

    in
        ()
    end (* initGlobalEnv *);
end;
