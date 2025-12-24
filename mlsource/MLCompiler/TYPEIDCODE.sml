(*
    Copyright (c) 2009, 2013, 2015-16, 2020-21, 2025 David C. J. Matthews

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

functor TYPEIDCODE (
    structure LEX : LEXSIG;
    structure CODETREE : CODETREE
    structure TYPETREE : TYPETREESIG
    structure STRUCTVALS : STRUCTVALSIG
    structure DEBUG: DEBUG
    structure PRETTY : PRETTY
    structure ADDRESS : AddressSig
    
    sharing LEX.Sharing = STRUCTVALS.Sharing = PRETTY.Sharing = CODETREE.Sharing
            = TYPETREE.Sharing = ADDRESS
) : TYPEIDCODESIG =
struct
    open CODETREE PRETTY ADDRESS STRUCTVALS TYPETREE
   
    (* This module deals with handling the run-time values that carry type
       information.  At the moment that's just the equality and print
       operations but that will be extended.
       
       There are different versions according to whether this is a
       monomorphic constructor, a polymorphic constructor or a type.
       Monomorphic and polymorphic constructor values are passed around
       in the module system as run-time values for types and datatypes
       whereas type values are passed in the core language as an extra
       argument to polymorphic functions.

       Both monomorphic and polymorphic constructors contain a reference
       for the "printer" entry so that a pretty printer can be installed.
       The functions in polymorphic datatypes have to be applied to type
       values for the base types to construct a type value.  Monomorphic
       datatypes just need some transformation.
       The effective types in each case are
       PolyType : (T('a) -> <'a t, 'a t> -> bool) * (T('a) -> 'a t * int -> pretty) ref
       MonoType : (<t * t> -> bool) * (t * int -> pretty) ref
       Type:      (<t * t> -> bool) * (t * int -> pretty)
       where < > denotes multiple (poly-style) arguments rather than tuples.
       *)

    val arg1     = mkLoadArgument 0 (* Used frequently. *)
    val arg2     = mkLoadArgument 1

    val InternalError = Misc.InternalError

    val orb = Word8.orb
    infix 7 orb;
    val mutableFlags = F_words orb F_mutable

    (* codeAccess is copied from ValueOps. *)
    fun codeAccess (Global code, _) = code
      
    |   codeAccess (Local{addr=ref locAddr, level=ref locLevel}, level) =
            mkLoad (locAddr, level, locLevel)
     
    |   codeAccess (Selected{addr, base}, level) =
            mkInd (addr, codeAccess (base, level))
     
    |   codeAccess _ = raise InternalError "No access"

    (* Load an identifier. *)
    fun codeId(TypeId{access, ...}, level) = codeAccess(access, level)
    (* Pretty printer code.  These produce code to apply the pretty printer functions. *)
    fun codePrettyString(s: string) =
        mkDatatype[mkConst(toMachineWord tagPrettyString), mkConst(toMachineWord s)]

    and codePrettyBreak(n, m) =
        mkDatatype[mkConst(toMachineWord tagPrettyBreak), mkConst(toMachineWord n), mkConst(toMachineWord m)]

    and codePrettyBlock(n: int, t: bool, c: context list, args: codetree) =
        mkDatatype[mkConst(toMachineWord tagPrettyBlock), mkConst(toMachineWord n),
                mkConst(toMachineWord t), mkConst(toMachineWord c), args]

    (* Turn a list of codetrees into a run-time list. *)
    and codeList(c: codetree list, tail: codetree): codetree =
        List.foldr (fn (hd, tl) => mkTuple[hd, tl]) tail c

    (* Generate code to check that the depth is not less than the allowedDepth
       and if it is to print "..." rather than the given code. *)
    and checkDepth(depthCode: codetree, allowedDepth: int, codeOk, codeFail) =
        mkIf(mkBinary(BuiltIns.WordComparison{test=BuiltIns.TestLess, isSigned=true},
                depthCode, mkConst(toMachineWord allowedDepth)),
             codeFail, codeOk)

    (* Subtract one from the current depth to produce the depth for sub-elements. *)
    and decDepth depthCode =
        mkBinary(BuiltIns.FixedPrecisionArith BuiltIns.ArithSub, depthCode, mkConst(toMachineWord 1))

    val codePrintDefault = mkProc(codePrettyString "?", 1, "print-default", [], 0)

    fun tcIsAbbreviation (TypeConstrs {identifier = TypeId{idKind = TypeFn _, ...},...}) = true
    |   tcIsAbbreviation _ = false

    fun tcArity(TypeConstrs {identifier=TypeId{idKind=TypeFn{arity, ...},...}, ...}) = arity
    |   tcArity(TypeConstrs {identifier=TypeId{idKind=Bound{arity, ...},...}, ...}) = arity
    |   tcArity(TypeConstrs {identifier=TypeId{idKind=Free{arity, ...},...}, ...}) = arity

        local
            open TypeValue
(*            local
                open RunCall
            in
                (* Structural equality.  This is the fall back if we can't find a type-specific function. *)
                fun structuralEquality(a: Address.machineWord, b: Address.machineWord) =
                if PolyML.pointerEq(a, b) then true
                else if isShort a orelse isShort b then false
                else
                let
                    val aFlags = memoryCellFlags a
                    and bFlags = memoryCellFlags b
                    and aLen = memoryCellLength a
                    and bLen = memoryCellLength b
                in
                    if aFlags <> bFlags orelse aLen <> bLen then false
                    else if aFlags = 0w0 (* Word data *)
                    then
                    let
                        fun eqWords c =
                            if c = aLen then true
                            else structuralEquality(loadWordFromImmutable(a, c),
                                loadWordFromImmutable(b, c)) andalso eqWords(c+0w1)
                    in
                        eqWords 0w0
                    end
                    else if aFlags = 0w1 (* Byte data *)
                    then
                    let
                        val byteLen = aLen * bytesPerWord
                        fun eqBytes c =
                            if c = byteLen then true
                            else (loadByteFromImmutable(a, c): Word8.word) = loadByteFromImmutable(b, c) andalso eqBytes(c+0w1)
                    in
                        eqBytes 0w0
                    end
                    else false
                end
            end*)
            (* The printer and equality functions must be valid functions even when they
               will never be called.  We may have to construct dummy type values
               by applying a polymorphic type constructor to them and if
               they don't have the right form the optimiser will complain.
               If we're only using type values for equality type variables the default
               print function will be used in polymorphic functions so must print "?". *)
(*            val defaultEquality =
                mkProc(mkEval(mkConst(toMachineWord structuralEquality),
                    [mkTuple[mkLoadArgument 0, mkLoadArgument 1]]), 2, "structureEq", [], 0)*)
            local
                open BuiltIns
                val argA = mkLoadArgument 0 and argB = mkLoadArgument 1
                (* Variables *)
                val aFlags = 0 and bFlags = 1 and aLen = 2 and bLen = 3
                and loopVar = 4 and loopA = 5 and loopB = 6
                val nLocals = 7
                fun mkOrelse(a, b) = mkIf(a, CodeTrue, b)
                fun mkEqual(a, b) = mkBinary(WordComparison{test=TestEqual, isSigned=false }, a, b)
                fun mkNotEq(a, b) = mkUnary(NotBoolean, mkEqual(a, b))
                val wordFlags = mkConst(toMachineWord Address.F_words)
                and byteFlags = mkConst(toMachineWord Address.F_bytes)
                val bytesWord = mkConst(toMachineWord RunCall.bytesPerWord)
                val const1 = mkConst(toMachineWord 1)
                fun loadWord(base, index) =
                    mkLoadOperation(LoadStoreMLWord{isImmutable=true}, base, index)
            in
                val defaultEquality =
                    mkProc(
                        mkIf(mkEqualPointerOrWord(argA, argB), CodeTrue,
                            mkIf(
                                mkOrelse(mkUnary(BuiltIns.IsTaggedValue, argA), mkUnary(BuiltIns.IsTaggedValue, argB)),
                                CodeFalse,
                                mkEnv(
                                    [
                                        mkDec(aFlags, mkUnary(BuiltIns.MemoryCellFlags, argA)),
                                        mkDec(bFlags, mkUnary(BuiltIns.MemoryCellFlags, argB)),
                                        mkDec(aLen, mkUnary(BuiltIns.MemoryCellLength, argA)),
                                        mkDec(bLen, mkUnary(BuiltIns.MemoryCellLength, argB))
                                    ],
                                    mkIf(
                                        mkOrelse(mkNotEq(mkLoadLocal aFlags, mkLoadLocal bFlags),
                                            mkNotEq(mkLoadLocal aLen, mkLoadLocal bLen)),
                                        CodeFalse,
                                        mkIf(mkEqual(mkLoadLocal aFlags, wordFlags),

                                            (* Word data - loop to process every word.  TODO: loop for
                                               len-1 and then tail recurse for the last word.  This will
                                               work better for lists.  N.B. Check for length = 0 i.e. unit. *)
                                            mkBeginLoop(
                                                mkIf(mkEqual(mkLoadLocal loopVar, mkLoadLocal aLen),
                                                    CodeTrue,
                                                    mkEnv(
                                                        [
                                                            mkDec(loopA, loadWord(argA, mkLoadLocal loopVar)),
                                                            mkDec(loopB, loadWord(argB, mkLoadLocal loopVar))
                                                        ],
                                                        mkIf(
                                                            mkEval(loadRecursive, [mkLoadLocal loopA, mkLoadLocal loopB]),
                                                            mkLoop[mkBinary(WordArith ArithAdd, mkLoadLocal loopVar, const1)],
                                                            CodeFalse))
                                                ),
                                                [(loopVar, CodeZero)]
                                            ),

                                            mkIf(mkEqual(mkLoadLocal aFlags, byteFlags),
                                                (* Byte data. *)
                                                mkBlockOperation{kind=BlockOpEqualByte, leftBase=argA, rightBase=argB,
                                                    leftIndex=CodeZero, rightIndex=CodeZero,
                                                    length=mkBinary(WordArith ArithMult, mkLoadLocal aLen, bytesWord)},
                                                (* Any other flags e.g. F_mutable => false *)
                                                CodeFalse
                                            )
                                        )
                                    ))
                                )
                            ),
                        2, "structureEq", [], nLocals)
            end

            (* Code generate this now so we only get one entry. *)
            val codeTuple =
                mkTuple[
                    createTypeValue{ (* Unused type variable. *) eqCode=defaultEquality, printCode=codePrintDefault}
                ]
            val code = genCode(codeTuple, [], 0)()
        in
            (* Default code used for a type variable that is not referenced but
               needs to be provided to satisfy the type. *)
            val defaultTypeCode = mkInd(0, code)
        end

    (* Generate a print function for a type.  This is used both for PolyML.print when called as a
       function and also to generate a print function for a datatype.
       For the moment this complicates things.  If we have bound type variable in a general
       type we'd want to look it up but if it's in a datatype it's an argument. *)
    fun printerForType(ty, baseLevel, argTypes) =
    let
        fun printCode(TypeVar(TypeVariable{link=ref l, ...}), level) =
            (
                (* Only in a general type.  We need to reduce the type immediately to
                   eliminate bound variables. *)
                case followRefChainToEnd l of
                    NONE => TypeValue.extractPrinter defaultTypeCode
                |   SOME final => printCode(reduceToType final, level)
            )

        |   printCode(BoundTypeVar(_, index), level) =
            let
                 (* Return default code for a missing type variable.  This can occur
                   if we have unreferenced type variables that need to be supplied but
                   are treated as "don't care". *)
                val code =
                    case argTypes of
                        NONE => (fn _ => defaultTypeCode)
                    |   SOME typeVarMap => typeVarMap index
            in
                TypeValue.extractPrinter(code level)
            end

        |   printCode(TypeConstruction { constr=typConstr as TypeConstrs {identifier=TypeId{idKind = TypeFn _, ...},...}, args, ...}, level) =
                (* Type function *)
                printCode(makeEquivalent (typConstr, args), level)

        |   printCode(TypeConstruction { constr=TypeConstrs {identifier,...}, args, name, ...}, level) =
            let
                val nLevel = newLevel level
                (* Get the type Id and put in code to extract the printer ref. *)
                val codedId = codeId(identifier, nLevel)
                open TypeValue
                val printerRefAddress = extractPrinter codedId
                (* We need a type value here.  The printer field will be used to
                   print the type argument and the boxedness and size fields may
                   be needed to extract the argument from the constructed value. *)
                fun makePrinterId t = createTypeValue {eqCode=CodeZero, printCode=printCode(t, nLevel)}

                val argList = map makePrinterId args
            in
                case args of
                    [] => (* Create a function that, when called, will extract the function from
                             the reference and apply it the pair of the value and the depth. *)
                        mkProc(
                            mkEval(
                                mkLoadOperation(LoadStoreMLWord{isImmutable=false}, printerRefAddress, CodeZero),
                                [arg1]),
                            1, "print-"^name, getClosure nLevel, 0)
                |   _ =>  (* Construct a function, that when called, will extract the
                             function from the reference and apply it first to the
                             base printer functions and then to the pair of the value and depth. *)
                        mkProc(
                            mkEval(
                                mkEval(
                                    mkLoadOperation(LoadStoreMLWord{isImmutable=false}, printerRefAddress, CodeZero),
                                    argList),
                                [arg1]),
                            1, "print-"^name, getClosure nLevel, 0)
            end

        |   printCode(LabelledRecord [], _) = (* Empty tuple: This is the unit value. *) mkProc(codePrettyString "()", 1, "print-labelled", [], 0)

        |   printCode(LabelledRecord [{name, typeOf}], level) =
            let (* Optimised unary record *)
                val localLevel = newLevel level
                val entryCode = mkEval(printCode(typeOf, localLevel), [arg1])
                val printItem =
                    codeList([codePrettyString(name^" ="), codePrettyBreak(1, 0), entryCode, codePrettyString "}"], CodeZero)
            in
                mkProc(
                    codePrettyBlock(1, false, [],
                        mkTuple[codePrettyString "{", printItem]),
                    1, "print-labelled", getClosure localLevel, 0)
            end

        |   printCode(typ  as LabelledRecord recList, level) =
                printRecord(recList, isProductType typ andalso List.length recList >= 2, true, level)

        |   printCode(FunctionType _, _) = mkProc(codePrettyString "fn", 1, "print-function", [], 0)

        |   printCode(FlexibleRecordVar{recList=ref recl, fullList=ref full, ...}, level) =
            let
                (* The record should be frozen at this point but this instance
                   may not have all the fields.  There's no point in adding them
                   because they will be type variables that will print as "?". *)
                val {names, ...} = followRefChainToEnd full
                val recList = followRefChainToEnd recl
            in
                printRecord(recList, false, List.length names = List.length recList, level)
            end

        |   printCode(typ as OverloadSetVar _, level) =
            let
                val constr as TypeConstrs {name,...} = typeConstrFromOverload typ
            in
                printCode(mkTypeConstruction(name, constr, [], []), level)
            end

        |   printCode(_, _) = mkProc(codePrettyString "<empty>", 1, "print-empty", [], 0)

        and printRecord(recList, isTuple, isFrozen, level) =
        let
            val localLevel = newLevel level
            val valToPrint = mkInd(0, arg1) and depthCode = mkInd(1, arg1)
            val fields = List.tabulate(List.length recList, fn n => n)
            val items = ListPair.zipEq(recList, fields)
            (* The ordering on fields is designed to allow mixing of tuples and
               records (e.g. #1).  It puts shorter names before longer so that
               #11 comes after #2 and before #100.  For named records it does
               not make for easy reading so we sort those alphabetically when
               printing. *)
            val printItems =
                if isTuple then items
                else Misc.quickSort(fn ({name = a, ...}, _) => fn ({name = b, ...}, _) => a <= b) items

            fun asRecord([], _) = raise Empty (* Shouldn't happen. *)

            |   asRecord([({name, typeOf, ...}, offset)], _) =
                let
                    val entryCode =
                        (* Last field: no separator. *)
                        mkEval(printCode(typeOf, localLevel),
                                    [mkTuple[mkInd(offset, valToPrint), decDepth depthCode]])
                    val (start, terminator) =
                        if isTuple then ([], ")")
                        else ([codePrettyString(name^" ="), codePrettyBreak(1, 0)], if isFrozen then "}" else ", ...}")
                in
                    codeList(start @ [entryCode, codePrettyString terminator], CodeZero)
                end

            |   asRecord(({name, typeOf, ...}, offset) :: fields, depth) =
                let
                    val (start, terminator) =
                        if isTuple then ([], ")")
                        else ([codePrettyString(name^" ="), codePrettyBreak(1, 0)], if isFrozen then "}" else ", ...}")
                in
                    checkDepth(depthCode, depth,
                        codeList(
                            start @
                            [
                                mkEval(
                                    printCode(typeOf, localLevel),
                                    [mkTuple[mkInd(offset, valToPrint), decDepth depthCode]]),
                                codePrettyString ",",
                                codePrettyBreak (1, 0)
                            ],
                            asRecord(fields, depth+1)),
                        codeList([codePrettyString ("..." ^ terminator)], CodeZero)
                    )
                end
        in
            mkProc(
                codePrettyBlock(1, false, [],
                    mkTuple[codePrettyString (if isTuple then "(" else "{"), asRecord(printItems, 0)]),
                1, "print-labelled", getClosure localLevel, 0)
        end
    in
        printCode(ty, baseLevel)
    end

    and makeEq(ty, level: level, getTypeValueForID, typeVarMap): codetree =
    let
 
        fun equalityForConstruction(tyConstr as TypeConstrs {identifier=iden,...}, args): codetree =
        (* Generate an equality function for a datatype construction. *)
        let
            (* Get argument types parameters for polytypes.  There's a special case
               here for type vars, essentially the type arguments to the datatype, to avoid taking
               apart the type value record and then building it again.
               Is this correct any longer? *)
            fun getArg(TypeVar(TypeVariable{link=ref l, ...})) =
                (
                    case followRefChainToEnd l of
                        NONE => defaultTypeCode
                    |   SOME ty => getArg(reduceToType ty)
                )
            |   getArg ty =
                let
                    val eqFun = makeEq(ty, level, getTypeValueForID, typeVarMap)
                    open TypeValue
                in
                    (* We need a type value here.  The equality function will be used to compare *)
                    createTypeValue{eqCode=eqFun, printCode=CodeZero}
                end

            val resFun =
                (* Special case: If this is ref, Array.array or Array2.array we must use
                   pointer equality and not attempt to create equality functions for
                   the argument.  It may not be an equality type. *)
                if isPointerEqType tyConstr
                then equalPointerOrWordFn
                else
                let
                    open TypeValue
                    val codeForId = extractEquality(getTypeValueForID(iden, args, level))
                in
                    (* Apply the function we obtained to any type arguments. *)
                    if null args
                    then codeForId
                    else mkEval(codeForId, map getArg args)
                end
        in
            resFun
        end
    in
        case ty of
            TypeVar(TypeVariable{link=ref l, ...}) =>
            (
                case followRefChainToEnd l of
                    NONE => TypeValue.extractEquality defaultTypeCode
                |   SOME tyVal => makeEq(reduceToType tyVal, level, getTypeValueForID, typeVarMap)
            )

        |   BoundTypeVar(_, index) =>
            let
                val code =
                    case typeVarMap of
                        NONE => (fn _ => defaultTypeCode)
                    |   SOME typeVarMap => typeVarMap index
            in
                TypeValue.extractEquality(code level)
            end

        |   TypeConstruction{constr, args, ...} =>
                if tcIsAbbreviation constr  (* May be an alias *)
                then makeEq (makeEquivalent (constr, args), level, getTypeValueForID, typeVarMap)
                else equalityForConstruction(constr, args)

        |   LabelledRecord [{typeOf=singleton, ...}] =>
                (* Unary tuples are optimised - no indirection. *)
                makeEq(singleton, level, getTypeValueForID, typeVarMap)

        |   LabelledRecord recList =>
            (* Combine the entries.
                fun eq(a,b) = #1 a = #1 b andalso #2 a = #2 b ... *)
            let
                (* Have to turn this into a new function. *)
                val nLevel = newLevel level
                fun combineEntries ([], _) = CodeTrue
                |   combineEntries ({typeOf, ...} :: t, n) =
                    let
                        val compareElements =
                            makeEq(typeOf, nLevel, getTypeValueForID, typeVarMap)
                    in
                        mkCand(
                            mkEval(compareElements, [mkInd(n, arg1), mkInd(n, arg2)]),
                            combineEntries (t, n+1))
                    end
                val tupleCode = combineEntries(recList, 0)
             in
                mkProc(tupleCode, 2, "eq{...}(2)", getClosure nLevel, 0)
            end

        |   OverloadSetVar _ => (* Should have been resolved. *)
                equalityForConstruction(typeConstrFromOverload ty, [])

        |   FlexibleRecordVar{recList=ref _, fullList=ref _, ...} =>
                (* TODO: If we have all the fields treat it as a labelled record otherwise fall back to
                   structure equality. *)
                TypeValue.extractEquality defaultTypeCode

        |   _ => raise InternalError "Equality for function"
    end

    (* Create equality functions for a set of possibly mutually recursive datatypes. *)
    fun equalityForDatatypes(typeDataList, eqAddresses, baseEqLevel): (int * codetree) list =
    let
        val typesAndAddresses = ListPair.zipEq(typeDataList, eqAddresses)

        fun equalityForDatatype(({typeConstr=TypeConstrSet(tyConstr as TypeConstrs {name=tcName,...}, vConstrs), eqStatus, ...}, addr),
                                otherFns) =
        if eqStatus
        then
        let
            val constrArity = tcArity tyConstr
            val baseEqLevelP1 = newLevel baseEqLevel

            (* Argument type variables. *)
            val argTypeMap =
                case constrArity of
                    0 => NONE
                |   _ =>
                    let
                        (* Add the polymorphic variables after the ordinary ones. *)
                        (* Create functions to load these if they are used in the map.  They may be non-local!!! *)
                        val args = Vector.tabulate(constrArity, fn addr => fn l => mkLoadParam(addr+2, l, baseEqLevelP1))
                    in
                        SOME(fn TVIndex n => Vector.sub(args, n))
                    end

            (* If this is a reference to a datatype we're currently generating
               load that address otherwise fall back to the default. *)
            fun getEqFnForID(typeId, _, l) =
                (*
                if sameTypeId(typeId, tcIdentifier tyConstr) andalso null argTypes
                then (* Directly recursive. *)
                    TypeValue.createTypeValue{eqCode=mkLoadRecursive(l-baseLevel-1), printCode=CodeZero,
                                    boxedCode=boxedCode, sizeCode=sizeCode}
                else
                *)
                case List.find(fn({typeConstr=(TypeConstrSet(TypeConstrs {identifier=tsId,...}, _)), ...}, _) => sameTypeId(tsId, typeId)) typesAndAddresses of
                    SOME({ ...}, addr) =>  (* Mutually recursive. *)
                         TypeValue.createTypeValue{eqCode=mkLoad(addr, l, baseEqLevel), printCode=CodeZero}
                |   NONE => codeId(typeId, l)

            (* Filter out the ShortForm constructors.  They arise
               in situations such as datatype t = A of int*int | B | C
               i.e. where we have only one non-nullary constructor
               and it is a tuple.  In this case we can deal with all
               the nullary constructors simply by testing whether
               the two arguments are the same.  We don't have to
               discriminate the individual cases. *)
            fun processConstrs [] =
                (* The last of the alternatives is false *) CodeZero

            |   processConstrs (Value{class, access, typeOf=ValueType(typeOf, _), ...} :: rest) =
                let
                    val base = codeAccess(access, baseEqLevelP1)
                    open ValueConstructor
                    fun matches arg = mkEval(extractTest base, [arg])
                in
                    case class of
                        Constructor{nullary=true, ...} =>
                        let
                            (* Nullary constructors are represented either by short constants or
                               by constant tuples depending on the rest of the datatype.  If this
                               is a short constant the pointer equality is sufficient.
                               This appears to increase the code size but the test should be
                               optimised away because it is applied to a constant. (The
                               "injection function" of a nullary constructor is the
                               constant that represents the value).  We have to test
                               the tags if it is not short because we can't guarantee
                               that the constant tuple hasn't been duplicated. *)
                            val isShort = mkIsShort(extractInjection base)
                       in
                            mkIf(mkIf(isShort, CodeFalse, matches arg1), matches arg2, processConstrs rest)
                        end
                    |    _ => (* We have to unwrap the value. *)
                        let
                            (* Get the constructor argument given the result type.  We might
                               actually be able to take the argument type off directly but
                               there's some uncertainty about whether we use the same type
                               variables for the constructors as for the datatype. (This only
                               applies for polytypes). *)
                            val argTypes = List.tabulate(constrArity, fn n => BoundTypeVar("'a", TVIndex n))
                            val resType = constructorResult(typeOf, argTypes)

                            (* Code to extract the value. *)
                            fun destruct argNo =
                                mkEval(extractProjection(codeAccess(access, baseEqLevelP1)),
                                    [mkLoadParam(argNo, baseEqLevelP1, baseEqLevelP1)])

                            (* Test whether the values match. *)
                            val eqValue =
                                mkEval(
                                    makeEq(resType, baseEqLevelP1, getEqFnForID, argTypeMap),
                                    [destruct 0, destruct 1])
                        in
                            (* We have equality if both values match
                               this constructor and the values within
                               the constructor match. *)
                            mkIf(matches arg1, mkCand(matches arg2, eqValue), processConstrs rest)
                        end
                end

            (* processConstrs assumes that if there are nullary constructors we have already
               tested for bitwise equality.  We also do that if there is more than one
               constructor to try to speed up equality for deep structures.  *)
            val eqCode =
                case vConstrs of
                    [Value{class=Constructor{nullary=true, ...}, ...}] => CodeTrue
                |   [_] => processConstrs vConstrs
                |   _ => mkCor(mkEqualPointerOrWord(arg1, arg2), processConstrs vConstrs)
        in
            if constrArity = 0
            then (addr, mkProc(eqCode, 2, "eq-" ^ tcName ^ "(2)", getClosure baseEqLevelP1, 0)) :: otherFns
            else (* Polymorphic.  Add an extra inline functions. *)
            let
                val nLevel = newLevel baseEqLevel
                val nnLevel = newLevel nLevel
                (* Call the second function with the values to be compared and the base types. *)
                val polyArgs = List.tabulate(constrArity, fn i => mkLoadParam(i, nnLevel, nLevel))
            in
                (addr,
                    mkInlproc(
                        mkInlproc(
                            mkEval(mkLoad(addr+1, nnLevel, baseEqLevel), [arg1, arg2] @ polyArgs), 2, "eq-" ^ tcName ^ "(2)",
                                   getClosure nnLevel, 0),
                            constrArity, "eq-" ^ tcName ^ "(2)(P)", getClosure nLevel, 0)) ::
                (addr+1,
                    mkProc(eqCode, 2+constrArity,
                           "eq-" ^ tcName ^ "()", getClosure baseEqLevelP1, 0)) ::
                otherFns
            end
        end
        else (* Not an equality type.  This will not be called but it still needs to
                be a function to ensure it's valid inside mkMutualDecs. *)
            (addr, mkProc(CodeZero, 2, "no-eq", [], 0)) :: otherFns
    in
        List.foldl equalityForDatatype [] typesAndAddresses
    end

    (* Create a printer function for a datatype when the datatype is declared.
       We don't have to treat mutually recursive datatypes specially because
       this is called after the type IDs have been created. *)
    fun printerForDatatype(TypeConstrSet(typeCons as TypeConstrs{name, ...}, vConstrs), level) =
    let
        val argCode = mkInd(0, arg1)
        and depthCode = mkInd(1, arg1)
        val nLevel = newLevel level
        val constrArity = tcArity typeCons

        val (innerLevel, newTypeVarMap) =
            case constrArity of
                0 => (nLevel, NONE)
            |   _ =>
                let
                    val nnLevel = newLevel nLevel
                    val argAddrs = List.tabulate(constrArity, fn n => n)
                    val args = List.map(fn addr => fn l => mkLoadParam(addr, l, nLevel)) argAddrs
                    val argVec = Vector.fromList args
                in
                    (nnLevel, SOME(fn TVIndex n => Vector.sub(argVec, n)))
                end

        (* If we have an expression as the argument we parenthesise it unless it is
           a simple string, a tuple, a record or a list. *)
(*         fun parenthesise p =
            let
                val test =
                    case p of
                        PrettyBlock(_, _, _, items) =>
                        (
                            case items of
                                PrettyString first :: tl =>
                                    not(null tl) andalso
                                        first <> "(" andalso first <> "{" andalso  first <> "["
                            |   _ => false   
                        )
                    |   _ => false
            in
                if test
                then PrettyBlock(3, true, [], [ PrettyString "(", PrettyBreak(0, 0), p, PrettyBreak(0, 0), PrettyString ")" ])
                else p
            end
*)

        local
            fun eqStr (arg, str) = mkEqualPointerOrWord(arg, mkConst(toMachineWord str))
            (* eqStr assumes that all occurrences of the same single character string are shared. *)

            val isNotNull = mkNot o mkIsShort

            fun testTag(arg, tagV) =
            (* Test the tag in the first word of the datatype. *)
                mkTagTest(mkInd(0, arg), tagV, maxPrettyTag)

            fun listHd x = mkVarField(0, x)
            and listTl x = mkVarField(1, x)
        in
            val parenCode =
                mkProc(
                    mkIf(
                        testTag(mkLoadArgument 0, tagPrettyBlock),
                        (* then *)
                        mkEnv(
                            [mkDec(0, mkVarField(4, mkLoadArgument 0))], (* items *)
                            mkIf
                            (
                                (* not(null items) andalso not(null(tl items)) andalso
                                   not (isPrettyString(hd items) andalso bracket) *)
                                mkCand(
                                    isNotNull(mkLoadLocal 0),
                                    mkCand(
                                        isNotNull (listTl(mkLoadLocal 0)),
                                        mkNot
                                        (
                                            mkCand(testTag(listHd(mkLoadLocal 0), tagPrettyString),
                                            mkEnv(
                                                [mkDec(1, mkVarField(1, listHd(mkLoadLocal 0)))],
                                                mkCor(eqStr(mkLoadLocal 1, "("), mkCor(eqStr(mkLoadLocal 1, "{"), eqStr(mkLoadLocal 1, "[")))
                                                )
                                            )
                                        )
                                    )
                                ),
                                (* then: Parenthesise the argument. *)
                                codePrettyBlock(
                                    3, true, [],
                                    mkDatatype [
                                        codePrettyString "(",
                                        mkDatatype [
                                            codePrettyBreak(0, 0),
                                            mkDatatype [
                                                mkLoadArgument 0,
                                                mkDatatype [
                                                    codePrettyBreak(0, 0),
                                                    mkDatatype [codePrettyString ")", CodeZero ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ),
                                (* else *) mkLoadArgument 0
                            )
                        ),
                        (* else *) mkLoadArgument 0
                    ),
                1, "parenthesise", [], 2)
        end


        fun printerForConstructors
                (Value{name, typeOf=ValueType(typeOf, _), access, class = Constructor{nullary, ...}, locations, ...} :: rest) =
            let
                (* The "value" for a value constructor is a tuple containing
                   the test code, the injection and the projection functions. *)
                val constructorCode = codeAccess(access, innerLevel)

                open ValueConstructor

                val locProps = (* Get the declaration location. *)
                    List.foldl(fn (DeclaredAt loc, _) => [ContextLocation loc] | (_, l) => l) [] locations

                val nameCode =
                    codePrettyBlock(0, false, locProps, codeList([codePrettyString name], CodeZero))

                val printCode =
                    if nullary
                    then (* Just the name *) nameCode
                    else
                    let
                        val argTypes = List.tabulate(constrArity, fn n => BoundTypeVar("'a", TVIndex n))
                        val typeOfArg = constructorResult(typeOf, argTypes)
                        val getValue = mkEval(extractProjection constructorCode, [argCode])
                    in
                        codePrettyBlock(1, false, [],
                            codeList(
                                [
                                    (* Put it in a block with the declaration location. *)
                                    nameCode,
                                    codePrettyBreak (1, 0),
                                    (* Print the argument and parenthesise it if necessary. *)
                                    mkEval(parenCode,
                                        [
                                            mkEval(
                                                printerForType(typeOfArg, innerLevel, newTypeVarMap),
                                                [mkTuple[getValue, decDepth depthCode]]
                                            )]
                                        )
                                ], CodeZero))
                    end
            in
                (* If this was the last or only constructor we don't need to test. *)
                checkDepth(depthCode, 1,
                    if null rest
                    then printCode
                    else
                    let
                        val testValue = mkEval(extractTest constructorCode, [argCode])
                    in
                        mkIf(testValue, printCode, printerForConstructors rest)
                    end,
                    codePrettyString "...")
            end

        |   printerForConstructors _ = raise InternalError ("No constructors:"^name)
            
        val printerCode = printerForConstructors vConstrs
    in
        (* Wrap this in the functions for the base types. *)
        if constrArity = 0
        then mkProc(printerCode, 1, "print-"^name, getClosure innerLevel, 0)
        else mkProc(mkProc(printerCode, 1, "print-"^name, getClosure innerLevel, 0),
                    constrArity, "print"^name^"()", getClosure nLevel, 0)
    end    

    (* Opaque matching and functor application create new type IDs using an existing
       type as implementation.  The equality function is inherited whether the type
       was specified as an eqtype or not.  The print function is no longer inherited.
       Instead a new reference is installed with a default print function.  This hides
       the implementation. *)
    (* If this is a type function we're going to generate a new ref anyway so we
       don't need to copy it. *)
    fun codeGenerativeId{source=TypeId{idKind=TypeFn{arity=0, resType, ...}, ...}, isEq, level, ...} =
        let (* Monotype abbreviation. *)
            open TypeValue

            val eqCode =
                if not isEq then CodeZero
                else (* We need a function that takes two arguments rather than a single pair. *)
                    makeEq(resType, level, fn (typeId, _, l) => codeId(typeId, l), NONE)
            val printCode = printerForType(resType, level, NONE)
        in
            createTypeValue {
                eqCode = eqCode,
                printCode =
                mkAllocateWordMemory(
                    mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags), printCode)
            }
        end

    |   codeGenerativeId{source=TypeId{idKind=TypeFn{arity, resType, ...}, ...}, isEq, level, ...} =
        let (* Polytype abbreviation: All the entries in the tuple are functions that must
               be applied to the base type values when the type constructor is used. *)
            (* Create a new type value cache. *)
            val nArgs = arity

            fun createCode(makeCode, name) =
                let
                    val nLevel = newLevel level
                    val addrs = ref 0
                    val args = Vector.tabulate(arity, fn addr => fn l => mkLoadParam(addr, l, nLevel))
                    fun varToArgMap(TVIndex n) = Vector.sub(args, n)
                    val argTypeMap = SOME varToArgMap
                    val innerFnCode = makeCode(nLevel, argTypeMap)
                in
                    mkProc(innerFnCode, nArgs, name, getClosure nLevel, !addrs)
                end

            open TypeValue
            (* Create a print function.*)
            val printCode =
                createCode(fn(nLevel, argTypeMap) =>
                    printerForType(resType, nLevel, argTypeMap), "print-helper()")
            and eqCode =
                if not isEq then CodeZero
                else createCode(fn(nLevel, argTypeMap) =>
                        makeEq(resType, nLevel, fn (typeId, _, l) => codeId(typeId, l), argTypeMap), "equality()")
        in
            createTypeValue {
                eqCode = eqCode,
                printCode =
                mkAllocateWordMemory(
                    mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags),
                    printCode)
            }
        end

    |   codeGenerativeId{source=sourceId, isDatatype, mkAddr, level, ...} =
        let (* Datatype.  This is the same for monotype and polytypes except for the print fn. *)
            (* We hide the print function if the target is just a type name but if the target
               is a datatype it's probably better to have a print function.  We inherit it
               from the source although that may expose the representation of other types.
               e.g. structure S:> sig type t datatype s = A of t end = ... *)
            open TypeValue
            val { dec, load } = multipleUses (codeId(sourceId, level), fn () => mkAddr 1, level)
            val loadLocal = load level
            val arity =
                case sourceId of
                    TypeId{idKind=Bound{arity, ...},...} => arity
                |   TypeId{idKind=Free{arity, ...},...} => arity
                |   TypeId{idKind=TypeFn _,...} => raise InternalError "Already checked"

            val printFn =
                if isDatatype
                then mkLoadOperation(LoadStoreMLWord{isImmutable=false}, extractPrinter loadLocal, CodeZero)
                else if arity = 0 then codePrintDefault
                else mkProc(codePrintDefault, arity, "print-helper()", [], 0)
                
            val printCode =
                    mkAllocateWordMemory(
                        mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags), printFn)
        in
            mkEnv(
                dec,
                createTypeValue { eqCode = extractEquality loadLocal, printCode = printCode }
             )
        end


    (* Create the equality and type functions for a set of mutually recursive datatypes. *)
    fun createDatatypeFunctions(
            typeDatalist: {typeConstr: typeConstrSet, eqStatus: bool} list,
            mkAddr, level, makePrintFunction) =
    let
        (* Each entry has an equality function and a ref to a print function.
           The print functions for each type needs to indirect through the refs
           when printing other types so that if a pretty printer is later
           installed for one of the types the others will use the new pretty
           printer.  That means that the code has to be produced in stages. *)
        (* Create the equality functions.  Because mutual decs can only be functions we
           can't create the typeIDs themselves as mutual declarations. *)
        local
            (* If this is polymorphic make two addresses, one for the returned equality function and
               one for the inner function. *)
            fun makeEqAddr{typeConstr=TypeConstrSet(tyConstr, _), ...} =
                mkAddr(if tcArity tyConstr = 0 then 1 else 2)
        in
            val eqAddresses = List.map makeEqAddr typeDatalist (* Make addresses for the equalities. *)
        end
        val equalityFunctions =
            mkMutualDecs(equalityForDatatypes(typeDatalist, eqAddresses, level))

        (* Create the typeId values and set their addresses.  The print function is
           initially set as zero. *)
        local
            fun makeTypeId({typeConstr=TypeConstrSet(TypeConstrs {identifier=TypeId { access, ...},...}, _), ...}, eqAddr) =
            let
                val var = case access of Local a => a | _ => raise InternalError "not local"
                val newAddr = mkAddr 1
                open TypeValue
                val idCode =
                    createTypeValue
                    {
                        eqCode=mkLoadLocal eqAddr,
                        printCode=
                            mkAllocateWordMemory(
                                mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags),
                                 CodeZero (* Temporary - replaced by setPrinter. *))
                    }
            in
                #addr var := newAddr;
                #level var:= level;
                mkDec(newAddr, idCode)
            end
        in
            val typeIdCode = ListPair.map makeTypeId (typeDatalist, eqAddresses)
        end

        (* Create the print functions and set the printer code for each typeId. *)
        local

            fun setPrinter{typeConstr as TypeConstrSet(tCons as TypeConstrs{identifier, ...}, _), ...} =
            let
                val arity = tcArity tCons
                val printCode =
                    if makePrintFunction
                    then printerForDatatype(typeConstr, level)
                    else if arity = 0
                    then codePrintDefault
                    else mkProc(codePrintDefault, arity, "print-printdefault", [], 0)
            in
                mkNullDec(
                    mkStoreOperation(LoadStoreMLWord{isImmutable=false},
                        TypeValue.extractPrinter(codeId(identifier, level)), CodeZero, printCode))
            end
        in
            val printerCode = List.map setPrinter typeDatalist
        end
    in
        equalityFunctions :: typeIdCode @ printerCode
    end


    (* Exported function.  Returns a function from an ML pair of values to bool.
       N.B. This differs from the functions in the typeID which take a Poly pair. *)
    fun equalityForType(ty: types, level: level): codetree =
    let
        val nLevel = newLevel level
        (* The final result function must take a single argument. *)
        val resultCode =
            makeEq(ty, nLevel, fn (typeId, _, l) => codeId(typeId, l), NONE)
    in
        (* We need to wrap this up in a new inline function. *)
        mkInlproc(mkEval(resultCode, [mkInd(0, arg1), mkInd(1, arg1)]),
                  1, "equality", getClosure nLevel, 0)
    end

    (* Exported version. *)
    val printerForType =
        fn (ty, baseLevel) => printerForType(ty, baseLevel, NONE)

    (* This code is used when the type checker has to construct a unique monotype
       because a type variable has escaped to the top level.
       The equality code always returns true and the printer prints "?". *)
    fun codeForUniqueId() =
    let
        open TypeValue
        val alwaysTrue = mkProc(CodeTrue, 2, "codeForUniqueId-equal", [], 0)
        val printCode =
            mkAllocateWordMemory(
                mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags), codePrintDefault)
    in
        createTypeValue{ eqCode = alwaysTrue, printCode = printCode  }
    end

    structure Sharing =
    struct
        type typeId     = typeId
        type codetree   = codetree
        type types      = types
        type typeConstrs= typeConstrs
        type typeConstrSet=typeConstrSet
        type typeVar=typeVar
        type codeBinding    = codeBinding
        type level = level
    end
end;
