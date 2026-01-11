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

    (* If this is true we are just using additional arguments for equality type
       variables.  If false we are using them for all type variables and every
       polymorphic function is wrapped in a function that passes the type
       information. *)
    val justForEqualityTypes = true

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

    fun tcArity(TypeConstrs {identifier=TypeId{idKind=TypeFn{tyVars, ...},...}, ...}) = List.length tyVars
    |   tcArity(TypeConstrs {identifier=TypeId{idKind=Bound{arity, ...},...}, ...}) = arity
    |   tcArity(TypeConstrs {identifier=TypeId{idKind=Free{arity, ...},...}, ...}) = arity

    structure TypeVarMap =
    struct
        (* Entries are either type var maps or "stoppers". *)
        datatype typeVarMapEntry =
            TypeVarFormEntry of (typeVarForm * (level->codetree)) list
        |   TypeConstrListEntry of typeConstrs list

        type typeVarMap =
        {
            entryType: typeVarMapEntry, (* Either the type var map or a "stopper". *)
            cache: (* Cache of new type values. *)
                {typeOf: types, address: int, decCode: codeBinding} list ref,
            mkAddr: int->int, (* Make new addresses at this level. *)
            level: level (* Function nesting level. *)
        } list

        (* Default map. *)
        fun defaultTypeVarMap (mkAddr, level) = [{entryType=TypeConstrListEntry[], cache=ref [], mkAddr=mkAddr, level=level}]

        fun markTypeConstructors(typConstrs, mkAddr, level, tvs) =
                {entryType = TypeConstrListEntry typConstrs, cache = ref [], mkAddr=mkAddr, level=level} :: tvs

        fun getCachedTypeValues(({cache=ref cached, ...}) ::_): codeBinding list =
                (* Extract the values from the list.  The later values may refer to earlier
                   so the list must be reversed. *)
                List.rev (List.map (fn{decCode, ...} => decCode) cached)
        |   getCachedTypeValues _ = raise Misc.InternalError "getCachedTypeValues"

        (* Extend a type variable environment with a new map of type variables to load functions. *)
        fun extendTypeVarMap (tvMap: (typeVarForm * (level->codetree)) list, mkAddr, level, typeVarMap) =
            {entryType = TypeVarFormEntry tvMap, cache = ref [], mkAddr=mkAddr, level=level} :: typeVarMap

        (* If we find the type var in the map return it as a type.  This is used to
           eliminate apparently generalisable type vars from the list. *)
        fun mapTypeVars [] _ = NONE

        |   mapTypeVars ({entryType=TypeVarFormEntry typeVarMap, ...} :: rest) tyVar =
            (
            case List.find(fn(t, _) => sameTv(t, tyVar)) typeVarMap of
                SOME (tv, _) => SOME(TypeVar tv)
            |   NONE => mapTypeVars rest tyVar
            )

        |   mapTypeVars (_ :: rest) tyVar = mapTypeVars rest tyVar

        (* Check to see if a type constructor is in the "stopper" set and return the level
           if it is. *)
        fun checkTypeConstructor(_, []) = ~1 (* Not there. *)
        |   checkTypeConstructor(tyCons, {entryType=TypeVarFormEntry _, ...} :: rest) =
                checkTypeConstructor(tyCons, rest: typeVarMap)
        |   checkTypeConstructor(tyCons as TypeConstrs {identifier=consId,...}, {entryType=TypeConstrListEntry tConstrs, ...} :: rest) =
                if List.exists(fn (TypeConstrs {identifier,...}) => sameTypeId(identifier, consId)) tConstrs
                then List.length rest + 1
                else checkTypeConstructor(tyCons, rest)

        local
            open TypeValue
            (* The printer and equality functions must be valid functions even when they
               will never be called.  We may have to construct dummy type values
               by applying a polymorphic type constructor to them and if
               they don't have the right form the optimiser will complain.
               If we're only using type values for equality type variables the default
               print function will be used in polymorphic functions so must print "?". *)
            val errorFunction2 = mkProc(CodeZero, 2, "errorCode2", [], 0)
            val codeFn = mkProc(codePrettyString "fn", 1, "print-function", [], 0)

            local
                fun typeValForMonotype (TypeConstrs {name=tcName, identifier,...}) =
                let
                    val codedId = codeId(identifier, baseLevel)
                    val printerRefAddress = extractPrinter codedId
                    val printFn = (* Create a function to load the printer ref and apply to the args. *)
                        mkProc(
                            mkEval(
                                mkLoadOperation(LoadStoreMLWord{isImmutable=false}, printerRefAddress, CodeZero),
                                [arg1]),
                            1, "print-" ^ tcName, [], 0)
                in
                    createTypeValue{
                        eqCode=extractEquality codedId, printCode=printFn,
                        boxedCode=extractBoxed codedId, sizeCode=extractSize codedId}
                end
            in
                (* A few common types.  These are effectively always cached. *)
                val fixedIntCode = typeValForMonotype fixedIntConstr
                and intInfCode = typeValForMonotype intInfConstr
                and boolCode   = typeValForMonotype boolConstr
                and stringCode = typeValForMonotype stringConstr
                and charCode   = typeValForMonotype charConstr
            end

            (* Code generate this now so we only get one entry. *)
            val codeTuple =
                mkTuple[
                    createTypeValue{ (* Unused type variable. *)
                        eqCode=errorFunction2, printCode=codePrintDefault, boxedCode=boxedEither, sizeCode=singleWord},
                    createTypeValue{ (* Function. *)
                        eqCode=errorFunction2, printCode=codeFn, boxedCode=boxedAlways, sizeCode=singleWord},
                    fixedIntCode, intInfCode, boolCode, stringCode, charCode
                ]
            val code = genCode(codeTuple, [], 0)()
        in
            (* Default code used for a type variable that is not referenced but
               needs to be provided to satisfy the type. *)
            val defaultTypeCode = mkInd(0, code)
            val functionCode = mkInd(1, code)
            val cachedCode = [(fixedIntConstr, mkInd(2, code)), (intInfConstr, mkInd(3, code)),
                              (boolConstr, mkInd(4, code)), (stringConstr, mkInd(5, code)),
                              (charConstr, mkInd(6, code))]
        end

        fun findCachedTypeCode(typeVarMap: typeVarMap, typ): ((level->codetree) * int) option =
        let
            (* Test if we have the same type as the cached type. *)
            fun sameType (t1, t2) =
                case (eventual t1, eventual t2) of
                    (TypeVar tv1, TypeVar tv2) =>
                    (
                        case (tvValue tv1, tvValue tv2) of
                            (EmptyType, EmptyType) => sameTv(tv1, tv2)
                        |   _ => false
                    )
                |   (FunctionType{arg=arg1, result=result1}, FunctionType{arg=arg2, result=result2}) =>
                        sameType(arg1, arg2) andalso sameType(result1, result2)

                |   (LabelledType{recList=list1, ...}, LabelledType{recList=list2, ...}) =>
                        ListPair.allEq(
                            fn({name=n1, typeof=t1}, {name=n2, typeof=t2}) => n1 = n2 andalso sameType(t1, t2))
                            (list1, list2)
                
                |   (TypeConstruction{constr=c1, args=a1, ...}, TypeConstruction{constr=c2, args=a2, ...}) =>
                        sameTypeConstr(c1, c2) andalso ListPair.allEq sameType (a1, a2)

                |   _ => false

            and sameTypeConstr(TypeConstrs {identifier=tc1Id,...}, TypeConstrs {identifier=tc2Id,...}) = sameTypeId(tc1Id, tc2Id)


            fun findCodeFromCache([], _) = NONE
            |   findCodeFromCache(({cache=ref cache, level, ...} :: rest): typeVarMap, ty) =
                (
                    case List.find(fn {typeOf, ...} => sameType(typeOf, ty)) cache of
                        NONE => findCodeFromCache(rest, ty)
                    |   SOME{address, ...} => SOME(fn l => mkLoad(address, l, level), List.length rest +1)
                )
        in
            case typ of
                TypeVar tyVar =>
                (
                    case tvValue tyVar of
                        EmptyType =>
                        let (* If it's a type var it is either in the type var list or we return the
                               default.  It isn't in the cache. *)
                            fun findCodeFromTypeVar([], _) = ((fn _ => defaultTypeCode), 0)
                                (* Return default code for a missing type variable.  This can occur
                                   if we have unreferenced type variables that need to be supplied but
                                   are treated as "don't care". *)

                            |   findCodeFromTypeVar({entryType=TypeVarFormEntry typeVarMap, ...} :: rest, tyVar) =
                                (
                                case List.find(fn(t, _) => sameTv(t, tyVar)) typeVarMap of
                                    SOME(_, codeFn) => (codeFn, List.length rest+1)
                                |   NONE => findCodeFromTypeVar(rest, tyVar)
                                )

                            |   findCodeFromTypeVar(_ :: rest, tyVar) = findCodeFromTypeVar(rest, tyVar)
                        in
                            SOME(findCodeFromTypeVar(typeVarMap, tyVar))
                        end

                    |   OverloadSet _ =>
                        let
                            val constr as TypeConstrs {name,...} = typeConstrFromOverload(typ, false)
                        in
                            findCachedTypeCode(typeVarMap, mkTypeConstruction(name, constr, [], []))
                        end

                    |   ty => findCachedTypeCode(typeVarMap, ty)
                )

            |   TypeConstruction { constr, args, ...} =>
                    let
                        fun sameTypeConstr(TypeConstrs {identifier=tc1d,...}, TypeConstrs {identifier=tc2d,...}) = sameTypeId(tc1d, tc2d)
                    in
                        if tcIsAbbreviation constr (* Type abbreviation *)
                        then findCachedTypeCode(typeVarMap, makeEquivalent (constr, args))
                        else if null args
                        then (* Check the permanently cached monotypes. *)
                            case List.find(fn (t, _) => sameTypeConstr(t, constr)) cachedCode of
                                SOME (_, c) => SOME ((fn _ => c), ~1)
                            |   NONE => findCodeFromCache(typeVarMap, typ)
                        else findCodeFromCache(typeVarMap, typ)
                    end

            |   FunctionType _ => SOME(fn _ => functionCode, ~1) (* Every function has the same code. *)

            |   _ => findCodeFromCache(typeVarMap, typ)
        end

    end

    open TypeVarMap

    (* Find the earliest entry in the cache table where we can put this entry. *)
    fun getMaxDepth (typeVarMap: typeVarMap) (ty: types, maxSoFar:int) : int =
        case findCachedTypeCode(typeVarMap, ty) of
            SOME (_, cacheDepth) => Int.max(cacheDepth, maxSoFar)
        |   NONE =>
            let
            in
                case ty of
                    TypeVar tyVar =>
                    (
                        case tvValue tyVar of
                            OverloadSet _ => maxSoFar (* Overloads are all global. *)
                        |   EmptyType => maxSoFar
                        |   tyVal => getMaxDepth typeVarMap (tyVal, maxSoFar)
                    )

                |   TypeConstruction{constr, args, ...} =>
                        if tcIsAbbreviation constr  (* May be an alias *)
                        then getMaxDepth typeVarMap (makeEquivalent (constr, args), maxSoFar)
                        else List.foldl (getMaxDepth typeVarMap)
                                       (Int.max(maxSoFar, checkTypeConstructor(constr, typeVarMap))) args

                |   LabelledType {recList, ...} =>
                        List.foldl (fn ({typeof, ...}, m) =>
                                getMaxDepth typeVarMap (typeof, m)) maxSoFar recList

                |   _ => maxSoFar
            end

    (* Get the boxedness status for a type i.e. whether values of the type are always addresses,
       always tagged integers or could be either. *)
    fun boxednessForType(ty, level: level, getTypeValueForID, typeVarMap): codetree =
        case findCachedTypeCode(typeVarMap, ty) of
            SOME (code, _) => TypeValue.extractBoxed(code level)
        |   NONE =>
            let
                fun boxednessForConstruction(TypeConstrs {identifier,...}, args): codetree =
                (* Get the boxedness for a datatype construction. *)
                let
                    (* Get the boxedness functions for the argument types.
                       This applies only to polytypes. *)
                    fun getArg ty : codetree =
                    let
                        val boxedFun = boxednessForType(ty, level, getTypeValueForID, typeVarMap)
                        open TypeValue
                    in
                        (* We need a type value here although only the boxedFun will be used. *)
                        createTypeValue{eqCode=CodeZero, printCode=CodeZero, boxedCode=boxedFun, sizeCode=singleWord}
                    end

                    val codeForId =
                        TypeValue.extractBoxed(getTypeValueForID(identifier, args, level))
                in
                    (* Apply the function we obtained to any type arguments. *)
                    if null args then codeForId else mkEval(codeForId, map getArg args)
                end
            in
                case ty of
                    TypeVar tyVar =>
                    (
                        case tvValue tyVar of
                            OverloadSet _ => boxednessForConstruction(typeConstrFromOverload(ty, false), [])
                        |   EmptyType => raise InternalError "boxedness: should already have been handled"
                        |   tyVal => boxednessForType(tyVal, level, getTypeValueForID, typeVarMap)
                    )

                |   TypeConstruction{constr, args, ...} =>
                        if tcIsAbbreviation constr  (* May be an alias *)
                        then boxednessForType (makeEquivalent (constr, args), level, getTypeValueForID, typeVarMap)
                        else boxednessForConstruction(constr, args)

                |   LabelledType {recList=[{typeof=singleton, ...}], ...} =>
                        (* Unary tuples are optimised - no indirection. *)
                        boxednessForType(singleton, level, getTypeValueForID, typeVarMap)

                |   LabelledType _ => TypeValue.boxedAlways (* Tuple are currently always boxed. *)

                    (* Functions are handled in the cache case. *)
                |   _ => raise InternalError "boxednessForType: Unknown type"
            end

    (* Get the size for values of the type.  A value N other than 1 means that every value of the
       type is a pointer to a tuple of exactly N words.  Zero is never used.  *)
    fun sizeForType(ty, level, getTypeValueForID, typeVarMap): codetree =
        case findCachedTypeCode(typeVarMap, ty) of
            SOME (code, _) => TypeValue.extractSize(code level)
        |   NONE =>
            let
                fun sizeForConstruction(TypeConstrs {identifier,...}, args): codetree =
                (* Get the size for a datatype construction. *)
                let
                    (* Get the size functions for the argument types.
                       This applies only to polytypes. *)
                    fun getArg ty : codetree =
                    let
                        val sizeFun = sizeForType(ty, level, getTypeValueForID, typeVarMap)
                        open TypeValue
                    in
                        (* We need a type value here although only the sizeFun will be used. *)
                        createTypeValue{eqCode=CodeZero, printCode=CodeZero, boxedCode=CodeZero, sizeCode=sizeFun}
                    end

                    val codeForId =
                        TypeValue.extractSize(getTypeValueForID(identifier, args, level))
                in
                    (* Apply the function we obtained to any type arguments. *)
                    if null args then codeForId else mkEval(codeForId, map getArg args)
                end
            in
                case ty of
                    TypeVar tyVar =>
                    (
                        case tvValue tyVar of
                            OverloadSet _ => sizeForConstruction(typeConstrFromOverload(ty, false), [])
                        |   EmptyType => raise InternalError "size: should already have been handled"
                        |   tyVal => sizeForType(tyVal, level, getTypeValueForID, typeVarMap)
                    )

                |   TypeConstruction{constr, args, ...} =>
                        if tcIsAbbreviation constr  (* May be an alias *)
                        then sizeForType (makeEquivalent (constr, args), level, getTypeValueForID, typeVarMap)
                        else sizeForConstruction(constr, args)

                |   LabelledType {recList=[{typeof=singleton, ...}], ...} =>
                        (* Unary tuples are optimised - no indirection. *)
                        sizeForType(singleton, level, getTypeValueForID, typeVarMap)

                |   LabelledType{recList, ...} =>
                    let
                        val length = List.length recList
                    in
                        (* Set the length to the number of words that can be unpacked.
                           If there are more than 4 items it's probably not worth packing
                           them into other tuples so set this to one. *)
                        if length <= 4 (*!maxPacking*)
                        then mkConst(toMachineWord length)
                        else TypeValue.singleWord
                    end

                    (* Functions are handled in the cache case. *)
                |   _ => raise InternalError "sizeForType: Unknown type"
            end
 
    fun printerForType(ty, baseLevel, argTypes: typeVarMap) =
    let
        fun printCode(typ, level: level) =
            (
                case typ of
                    typ as TypeVar tyVar =>
                    (
                        case tvValue tyVar of
                            EmptyType =>
                            (
                                case findCachedTypeCode(argTypes, typ) of
                                    SOME (code, _) => TypeValue.extractPrinter(code level)
                                |   NONE => raise InternalError "printerForType: should already have been handled"
                            )

                        |   OverloadSet _ =>
                            let
                                val constr as TypeConstrs {name,...} = typeConstrFromOverload(typ, false)
                            in
                                printCode(mkTypeConstruction(name, constr, [], []), level)
                            end

                        |   _ =>  (* Just a bound type variable. *) printCode(tvValue tyVar, level)
                    )

                |   TypeConstruction { constr=typConstr as TypeConstrs {identifier,...}, args, name, ...} =>
                        if tcIsAbbreviation typConstr (* Handle type abbreviations directly *)
                        then printCode(makeEquivalent (typConstr, args), level)
                        else
                        let
                            val nLevel = newLevel level
                            (* Get the type Id and put in code to extract the printer ref. *)
                            val codedId = codeId(identifier, nLevel)
                            open TypeValue
                            val printerRefAddress = extractPrinter codedId
                            (* We need a type value here.  The printer field will be used to
                               print the type argument and the boxedness and size fields may
                               be needed to extract the argument from the constructed value. *)
                            fun makePrinterId t =
                            let
                                fun codeForId(typeId, _, l) = codeId(typeId, l)
                            in
                                createTypeValue
                                    {eqCode=CodeZero, printCode=printCode(t, nLevel),
                                     boxedCode=boxednessForType(t, nLevel, codeForId, argTypes),
                                     sizeCode=sizeForType(t, nLevel, codeForId, argTypes)}
                            end

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

                |   LabelledType { recList=[], ...} =>
                        (* Empty tuple: This is the unit value. *) mkProc(codePrettyString "()", 1, "print-labelled", [], 0)


                |   LabelledType {recList=[{name, typeof}], ...} =>
                    let (* Optimised unary record *)
                        val localLevel = newLevel level
                        val entryCode = mkEval(printCode(typeof, localLevel), [arg1])
                        val printItem =
                            codeList([codePrettyString(name^" ="), codePrettyBreak(1, 0), entryCode, codePrettyString "}"], CodeZero)
                    in
                        mkProc(
                            codePrettyBlock(1, false, [],
                                mkTuple[codePrettyString "{", printItem]),
                            1, "print-labelled", getClosure localLevel, 0)
                    end

                |   LabelledType (r as { recList, ...}) =>
                    let
                        (* See if this has fields numbered 1=, 2= etc.   N.B.  If it has only one field
                           we need to print 1= since we don't have singleton tuples. *)
                        fun isRec([], _) = true
                        |   isRec({name, ...} :: l, n) = name = Int.toString n andalso isRec(l, n+1)
                        val isTuple = recordIsFrozen r andalso isRec(recList, 1) andalso List.length recList >= 2
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

                        |   asRecord([({name, typeof, ...}, offset)], _) =
                            let
                                val entryCode =
                                    (* Last field: no separator. *)
                                    mkEval(printCode(typeof, localLevel),
                                                [mkTuple[mkInd(offset, valToPrint), decDepth depthCode]])
                                val (start, terminator) =
                                    if isTuple then ([], ")")
                                    else ([codePrettyString(name^" ="), codePrettyBreak(1, 0)], "}")
                            in
                                codeList(start @ [entryCode, codePrettyString terminator], CodeZero)
                            end

                        |   asRecord(({name, typeof, ...}, offset) :: fields, depth) =
                            let
                                val (start, terminator) =
                                    if isTuple then ([], ")")
                                    else ([codePrettyString(name^" ="), codePrettyBreak(1, 0)], "}")
                            in
                                checkDepth(depthCode, depth,
                                    codeList(
                                        start @
                                        [
                                            mkEval(
                                                printCode(typeof, localLevel),
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

                |   FunctionType _ => mkProc(codePrettyString "fn", 1, "print-function", [], 0)

                |   _ => mkProc(codePrettyString "<empty>", 1, "print-empty", [], 0)
            )
    in
        printCode(ty, baseLevel)
    end

    and makeEq(ty, level: level, getTypeValueForID, typeVarMap): codetree =
    let
 
        fun equalityForConstruction(TypeConstrs {identifier=iden,...}, args): codetree =
        (* Generate an equality function for a datatype construction. *)
        let
            (* Get argument types parameters for polytypes.  There's a special case
               here for type vars, essentially the type arguments to the datatype, to avoid taking
               apart the type value record and then building it again. *)
            fun getArg ty =
                if (case ty of TypeVar tyVar =>
                        (case tvValue tyVar of EmptyType => true | _ => false) | _ => false)
                then
                (
                    case findCachedTypeCode(typeVarMap, ty) of
                        SOME (code, _) => code level
                    |   NONE => raise InternalError "getArg"
                )
            else
                let
                    val eqFun = makeEq(ty, level, getTypeValueForID, typeVarMap)
                    open TypeValue
                in
                    (* We need a type value here.  The equality function will be used to compare
                       the argument type and the boxedness and size parameters may be needed for
                       the constructors. *)
                    createTypeValue{eqCode=eqFun, printCode=CodeZero,
                        boxedCode=boxednessForType(ty, level, getTypeValueForID, typeVarMap),
                        sizeCode=sizeForType(ty, level, getTypeValueForID, typeVarMap)}
                end

            val resFun =
                (* Special case: If this is ref, Array.array or Array2.array we must use
                   pointer equality and not attempt to create equality functions for
                   the argument.  It may not be an equality type. *)
                if isPointerEqType iden
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
            TypeVar tyVar =>
            (
                case tvValue tyVar of
                    OverloadSet _ =>
                         (* This seems to occur if there are what amount to indirect references to literals. *)
                        equalityForConstruction(typeConstrFromOverload(ty, false), [])

                |   EmptyType =>
                    (
                        case findCachedTypeCode(typeVarMap, ty) of
                            SOME (code, _) => TypeValue.extractEquality(code level)
                        |   NONE => raise InternalError "makeEq: should already have been handled"
                    )

                |   tyVal => makeEq(tyVal, level, getTypeValueForID, typeVarMap)
            )

        |   TypeConstruction{constr, args, ...} =>
                if tcIsAbbreviation constr  (* May be an alias *)
                then makeEq (makeEquivalent (constr, args), level, getTypeValueForID, typeVarMap)
                else equalityForConstruction(constr, args)

        |   LabelledType {recList=[{typeof=singleton, ...}], ...} =>
                (* Unary tuples are optimised - no indirection. *)
                makeEq(singleton, level, getTypeValueForID, typeVarMap)

        |   LabelledType {recList, ...} =>
            (* Combine the entries.
                fun eq(a,b) = #1 a = #1 b andalso #2 a = #2 b ... *)
            let
                (* Have to turn this into a new function. *)
                val nLevel = newLevel level
                fun combineEntries ([], _) = CodeTrue
                |   combineEntries ({typeof, ...} :: t, n) =
                    let
                        val compareElements =
                            makeEq(typeof, nLevel, getTypeValueForID, typeVarMap)
                    in
                        mkCand(
                            mkEval(compareElements, [mkInd(n, arg1), mkInd(n, arg2)]),
                            combineEntries (t, n+1))
                    end
                val tupleCode = combineEntries(recList, 0)
             in
                mkProc(tupleCode, 2, "eq{...}(2)", getClosure nLevel, 0)
            end

        |   _ => raise InternalError "Equality for function"
    end

    (* Create equality functions for a set of possibly mutually recursive datatypes. *)
    fun equalityForDatatypes(typeDataList, eqAddresses, baseEqLevel, typeVarMap): (int * codetree) list =
    let
        val typesAndAddresses = ListPair.zipEq(typeDataList, eqAddresses)

        fun equalityForDatatype(({typeConstr=TypeConstrSet(tyConstr as TypeConstrs {name=tcName,...}, vConstrs), eqStatus, (*boxedCode, sizeCode,*) ...}, addr),
                                otherFns) =
        if eqStatus
        then
        let
            val nTypeVars = tcArity tyConstr
            val argTypes =
                List.tabulate(tcArity tyConstr,
                    fn _ => makeTv{value=EmptyType, level=generalisable, nonunifiable=false,
                                 equality=false, printable=false})
            val baseEqLevelP1 = newLevel baseEqLevel

            (* Argument type variables. *)
            val (localArgList, argTypeMap) =
                case argTypes of
                    [] => ([], typeVarMap)
                |   _ =>
                    let
                        (* Add the polymorphic variables after the ordinary ones. *)
                        (* Create functions to load these if they are used in the map.  They may be non-local!!! *)
                        val args = List.tabulate(nTypeVars, fn addr => fn l => mkLoadParam(addr+2, l, baseEqLevelP1))
                        (* Put the outer args in the map *)
                        val varToArgMap = ListPair.zipEq(argTypes, args)
                        (* Load the local args to return. *)
                        val localArgList = List.tabulate (nTypeVars, fn addr => mkLoadParam(addr+2, baseEqLevelP1, baseEqLevelP1))
                        val addrs = ref 0 (* Make local declarations for any type values. *)
                        fun mkAddr n = !addrs before (addrs := !addrs + n)
                    in
                        (localArgList, extendTypeVarMap(varToArgMap, mkAddr, baseEqLevelP1, typeVarMap))
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
                    SOME({boxedCode, sizeCode, ...}, addr) =>  (* Mutually recursive. *)
                         TypeValue.createTypeValue{eqCode=mkLoad(addr, l, baseEqLevel), printCode=CodeZero,
                                                   boxedCode=boxedCode, sizeCode=sizeCode}
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

            |   processConstrs (Value{class, access, typeOf, ...} :: rest) =
                let
                    fun addPolymorphism c =
                        if nTypeVars = 0 orelse justForEqualityTypes then c else mkEval(c, localArgList)
                    val base = codeAccess(access, baseEqLevelP1)
                    open ValueConstructor
                    fun matches arg = mkEval(addPolymorphism(extractTest base), [arg])
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
                            val isShort = mkIsShort(addPolymorphism(extractInjection base))
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
                            val resType = constructorResult(typeOf, List.map TypeVar argTypes)

                            (* Code to extract the value. *)
                            fun destruct argNo =
                                mkEval(addPolymorphism(extractProjection(codeAccess(access, baseEqLevelP1))),
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
            if null argTypes
            then (addr, mkProc(eqCode, 2, "eq-" ^ tcName ^ "(2)", getClosure baseEqLevelP1, 0)) :: otherFns
            else (* Polymorphic.  Add an extra inline functions. *)
            let
                val nArgs = List.length argTypes
                val nLevel = newLevel baseEqLevel
                val nnLevel = newLevel nLevel
                (* Call the second function with the values to be compared and the base types. *)
                val polyArgs = List.tabulate(nArgs, fn i => mkLoadParam(i, nnLevel, nLevel))
            in
                (addr,
                    mkInlproc(
                        mkInlproc(
                            mkEval(mkLoad(addr+1, nnLevel, baseEqLevel), [arg1, arg2] @ polyArgs), 2, "eq-" ^ tcName ^ "(2)",
                                   getClosure nnLevel, 0),
                            nArgs, "eq-" ^ tcName ^ "(2)(P)", getClosure nLevel, 0)) ::
                (addr+1,
                    mkProc(mkEnv(getCachedTypeValues argTypeMap, eqCode), 2+nTypeVars,
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
    fun printerForDatatype(TypeConstrSet(typeCons as TypeConstrs{name, ...}, vConstrs), level, typeVarMap) =
    let
        val argCode = mkInd(0, arg1)
        and depthCode = mkInd(1, arg1)
        val nLevel = newLevel level
        val constrArity = tcArity typeCons
        val argTypes = 
            List.tabulate(constrArity,
                fn _ => makeTv{value=EmptyType, level=generalisable, nonunifiable=false,
                             equality=false, printable=false})

        val (localArgList, innerLevel, newTypeVarMap) =
            case constrArity of
                0 => ([], nLevel, typeVarMap)
            |   _ =>
                let
                    val nnLevel = newLevel nLevel
                    fun mkTcArgMap (argTypes, level, oldLevel) =
                        let
                            val nArgs = List.length argTypes
                            val argAddrs = List.tabulate(nArgs, fn n => n)
                            val args = List.map(fn addr => fn l => mkLoadParam(addr, l, oldLevel)) argAddrs
                        in
                            (ListPair.zipEq(argTypes, args), List.map (fn addr => mkLoadParam(addr, level, oldLevel)) argAddrs)
                        end
                    val (varToArgMap, localArgList) = mkTcArgMap(argTypes, nnLevel, nLevel)
                    val addrs = ref 1 (* Make local declarations for any type values. *)
                    fun mkAddr n = !addrs before (addrs := !addrs + n)
                in
                    (localArgList, nnLevel, extendTypeVarMap(varToArgMap, mkAddr, nLevel, typeVarMap))
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
                (Value{name, typeOf, access, class = Constructor{nullary, ...}, locations, ...} :: rest) =
            let
                (* The "value" for a value constructor is a tuple containing
                   the test code, the injection and the projection functions. *)
                val constructorCode = codeAccess(access, innerLevel)

                (* If this is a polytype the fields in the constructor tuple are functions that first
                   have to be applied to the type arguments to yield the actual injection/test/projection
                   functions.  For monotypes the fields contain the injection/test/projection
                   functions directly. *)
                fun addPolymorphism c =
                   if constrArity = 0 orelse justForEqualityTypes then c else mkEval(c, localArgList)

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
                        val typeOfArg = constructorResult(typeOf, List.map TypeVar argTypes)
                        val getValue = mkEval(addPolymorphism(extractProjection constructorCode), [argCode])
                        
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
                        val testValue = mkEval(addPolymorphism(extractTest constructorCode), [argCode])
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
        else mkProc(mkEnv(getCachedTypeValues newTypeVarMap,
                            mkProc(printerCode, 1, "print-"^name, getClosure innerLevel, 0)),
                    constrArity, "print"^name^"()", getClosure nLevel, 0)
    end    

    (* Opaque matching and functor application create new type IDs using an existing
       type as implementation.  The equality function is inherited whether the type
       was specified as an eqtype or not.  The print function is no longer inherited.
       Instead a new reference is installed with a default print function.  This hides
       the implementation. *)
    (* If this is a type function we're going to generate a new ref anyway so we
       don't need to copy it. *)
    fun codeGenerativeId{source=TypeId{idKind=TypeFn{tyVars=[], resType, ...}, ...}, isEq, mkAddr, level, ...} =
        let (* Monotype abbreviation. *)
            (* Create a new type value cache. *)
            val typeVarMap = defaultTypeVarMap(mkAddr, level)

            open TypeValue

            val eqCode =
                if not isEq then CodeZero
                else (* We need a function that takes two arguments rather than a single pair. *)
                    makeEq(resType, level, fn (typeId, _, l) => codeId(typeId, l), typeVarMap)
            val boxedCode =
                boxednessForType(resType, level, fn (typeId, _, l) => codeId(typeId, l), typeVarMap)
            val sizeCode =
                sizeForType(resType, level, fn (typeId, _, l) => codeId(typeId, l), typeVarMap)
            val printCode =
                printerForType(resType, level, typeVarMap)
        in
            mkEnv(
                TypeVarMap.getCachedTypeValues typeVarMap,
                createTypeValue {
                    eqCode = eqCode, boxedCode = boxedCode, sizeCode = sizeCode,
                    printCode =
                    mkAllocateWordMemory(
                        mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags), printCode)
                })
        end

    |   codeGenerativeId{source=TypeId{idKind=TypeFn{tyVars=argTypes, resType, ...}, ...}, isEq, mkAddr, level, ...} =
        let (* Polytype abbreviation: All the entries in the tuple are functions that must
               be applied to the base type values when the type constructor is used. *)
            (* Create a new type value cache. *)
            val typeVarMap = defaultTypeVarMap(mkAddr, level)
            val nArgs = List.length argTypes

            fun createCode(makeCode, name) =
                let
                    val nLevel = newLevel level
                    val addrs = ref 0
                    fun mkAddr n = !addrs before (addrs := !addrs + n)

                    local
                        val args =
                            List.tabulate(nArgs, fn addr => fn l => mkLoadParam(addr, l, nLevel))
                    in
                        val typeEnv = ListPair.zipEq(argTypes, args)
                    end
                    
                    val argTypeMap = extendTypeVarMap(typeEnv, mkAddr, nLevel, typeVarMap)
                    val innerFnCode = makeCode(nLevel, argTypeMap)
                in
                    mkProc(mkEnv(getCachedTypeValues argTypeMap, innerFnCode), nArgs, name, getClosure nLevel, !addrs)
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
            and boxedCode =
                createCode(fn(nLevel, argTypeMap) =>
                    boxednessForType(resType, nLevel, fn (typeId, _, l) => codeId(typeId, l), argTypeMap), "boxedness()")
            and sizeCode =
                createCode(fn(nLevel, argTypeMap) =>
                    sizeForType(resType, nLevel, fn (typeId, _, l) => codeId(typeId, l), argTypeMap), "size()")
        in
            mkEnv(
                TypeVarMap.getCachedTypeValues typeVarMap,
                createTypeValue {
                    eqCode = eqCode, boxedCode = boxedCode,
                    printCode =
                    mkAllocateWordMemory(
                        mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags),
                        printCode),
                    sizeCode = sizeCode
                })
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
                createTypeValue {
                    eqCode = extractEquality loadLocal, printCode = printCode,
                    boxedCode = extractBoxed loadLocal, sizeCode = extractSize loadLocal
                }
             )
        end


    (* Create the equality and type functions for a set of mutually recursive datatypes. *)
    fun createDatatypeFunctions(
            typeDatalist: {typeConstr: typeConstrSet, eqStatus: bool, boxedCode: codetree, sizeCode: codetree } list,
            mkAddr, level, typeVarMap, makePrintFunction) =
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
            mkMutualDecs(equalityForDatatypes(typeDatalist, eqAddresses, level, typeVarMap))

        (* Create the typeId values and set their addresses.  The print function is
           initially set as zero. *)
        local
            fun makeTypeId({typeConstr=TypeConstrSet(TypeConstrs {identifier=TypeId { access, ...},...}, _), boxedCode, sizeCode, ...}, eqAddr) =
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
                                 CodeZero (* Temporary - replaced by setPrinter. *)),
                        boxedCode = boxedCode,
                        sizeCode = sizeCode
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
                    then printerForDatatype(typeConstr, level, typeVarMap)
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
    fun equalityForType(ty: types, level: level, typeVarMap: typeVarMap): codetree =
    let
        val nLevel = newLevel level
        (* The final result function must take a single argument. *)
        val resultCode =
            makeEq(ty, nLevel, fn (typeId, _, l) => codeId(typeId, l), typeVarMap)
    in
        (* We need to wrap this up in a new inline function. *)
        mkInlproc(mkEval(resultCode, [mkInd(0, arg1), mkInd(1, arg1)]),
                  1, "equality", getClosure nLevel, 0)
    end

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
        createTypeValue{
            eqCode = alwaysTrue, printCode = printCode,
            boxedCode = boxedEither, sizeCode = singleWord }
    end

    val noEquality = mkProc(CodeFalse, 2, "noEquality", [], 0)
    (* Since we don't have a way of writing a "printity" type variable there are cases
       when the printer will have to fall back to this. e.g. if we have a polymorphic
       printing function as a functor argument. *)
    val noPrinter = codePrintDefault

    (* If this is a polymorphic value apply it to the type instance. *)
    fun applyToInstance'([], level, _, code) = code level (* Monomorphic. *)

    |   applyToInstance'(sourceTypes, level, polyVarMap, code) =
    let
        (* If we need either the equality or print function we generate a new
           entry and ignore anything in the cache. *)
        fun makePolyParameter {value=t, equality, printity} =
            if equality orelse printity
            then
                let
                    open TypeValue
                    fun getTypeValueForID(typeId, _, l) = codeId(typeId, l)
                    val eqCode =
                        if equality
                        then makeEq(t, level, fn (typeId, _, l) => codeId(typeId, l), polyVarMap)
                        else noEquality
                    val boxedCode = boxednessForType(t, level, getTypeValueForID, polyVarMap)
                    val printCode =
                        if printity then printerForType(t, level, polyVarMap) else noPrinter
                    val sizeCode = sizeForType(t, level, getTypeValueForID, polyVarMap)
                in
                    createTypeValue{
                        eqCode=eqCode, printCode=printCode,
                        boxedCode=boxedCode, sizeCode=sizeCode}
                end
            else (* If we don't require the equality or print function we can use the cache. *)
            case findCachedTypeCode(polyVarMap, t) of
                SOME (code, _) => code level
            |   NONE =>
                let
                    val maxCache = getMaxDepth polyVarMap (t, 1)
                    val cacheEntry = List.nth(polyVarMap, List.length polyVarMap - maxCache)
                    val { cache, mkAddr, level=decLevel, ...} = cacheEntry
                    local
                        open TypeValue
                        val boxedCode =
                            boxednessForType(t, decLevel, fn (typeId, _, l) => codeId(typeId, l), polyVarMap)
                        val sizeCode =
                            sizeForType(t, decLevel, fn (typeId, _, l) => codeId(typeId, l), polyVarMap)
                    in
                        val typeValue =
                            createTypeValue{
                                eqCode=noEquality, printCode=noPrinter,
                                boxedCode=boxedCode, sizeCode=sizeCode}
                    end
                    (* Make a new entry and put it in the cache. *)
                    val decAddr = mkAddr 1
                    val () = cache := {decCode = mkDec(decAddr, typeValue), typeOf = t, address = decAddr } :: !cache
                in
                    mkLoad(decAddr, level, decLevel)
                end
    in
        mkEval(code level, List.map makePolyParameter sourceTypes)
    end

    (* For now limit this to equality types. *)
    fun applyToInstance(sourceTypes, level, polyVarMap, code) =
        applyToInstance'(
            List.filter(fn {equality, ...} => not justForEqualityTypes orelse equality) sourceTypes,
            level, polyVarMap, code)

    structure Sharing =
    struct
        type typeId     = typeId
        type codetree   = codetree
        type types      = types
        type typeConstrs= typeConstrs
        type typeConstrSet=typeConstrSet
        type typeVarForm=typeVarForm
        type typeVarMap = typeVarMap
        type codeBinding    = codeBinding
        type level      = level
    end
end;
