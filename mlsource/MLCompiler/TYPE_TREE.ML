(*
    Original Poly version:
    Title:      Operations on type structures.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985

    ML translation and other changes:
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further development:
    Copyright (c) 2000-9, 2012-2018, 2020, 2025 David C.J. Matthews

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

functor TYPE_TREE (
    structure LEX : LEXSIG
    structure STRUCTVALS : STRUCTVALSIG;
    structure PRETTY : PRETTY
    structure CODETREE : CODETREE where type machineWord = Address.machineWord
    structure EXPORTTREE: EXPORTTREESIG;
    structure DEBUG: DEBUG

    structure UTILITIES :
    sig
        val mapTable: ('a * 'a -> bool) ->
                       {enter: 'a * 'b -> unit, lookup: 'a -> 'b option}
        val splitString:   string -> { first:string, second:string }
    end;

    structure MISC :
    sig
        exception InternalError of string;
  
        val lookupDefault : ('a -> 'b option) -> ('a -> 'b option) -> 'a -> 'b option
    end;

    sharing LEX.Sharing = PRETTY.Sharing = EXPORTTREE.Sharing = STRUCTVALS.Sharing
          = CODETREE.Sharing

) :  TYPETREESIG =

struct
  open MISC;
    open PRETTY;

    open STRUCTVALS;
    open LEX;
    open UTILITIES;
    open CODETREE;
    open EXPORTTREE

    fun sameTv(TypeVariable{link=lA, ...}, TypeVariable{link=lB, ...}) =
    let
        val (refA, _) = followRefChainToRef lA
        and (refB, _) = followRefChainToRef lB
    in
        refA = refB
    end 
     
    fun isFunctionType     (FunctionType     _) = true
    |   isFunctionType     _ = false;
    
    fun isBadType           BadType             = true
    |   isBadType          _ = false;

    fun typesFunctionType     (FunctionType     x) = x
    |   typesFunctionType     _ = raise Match;


(*************)

    fun mkTypeVar (level, equality) = 
        TypeVar (makeTv {value=NONE, level=level, equality=equality})

    fun mkTypeConstruction (name, typc, args, locations) =
        TypeConstruction {name = name, constr = typc,
                          args = args, locations = locations}

    local
        (* Turn a tuple into a record of the form {1=.., 2=... }*)
        fun maptoRecord ([], _) = []
        |   maptoRecord (H::T, i) = {name=Int.toString i, typeOf=H} :: maptoRecord (T, i+1)
    in
        fun mkProductType (typel: types list) = LabelledRecord(maptoRecord (typel, 1))
    end

    (* Helper functions copied from STRUCT_VALS.  Not exported.   These should
       generally be replaced by the direct pattern matching. *)

    fun tcArity(TypeConstrs {identifier=TypeId{idKind=TypeFn{arity, ...},...}, ...}) = arity
    |   tcArity(TypeConstrs {identifier=TypeId{idKind=Bound{arity, ...},...}, ...}) = arity
    |   tcArity(TypeConstrs {identifier=TypeId{idKind=Free{arity, ...},...}, ...}) = arity

    fun makeTypeConstructor (name, uid, locations) = TypeConstrs { name = name, identifier = uid, locations = locations }

    fun tsConstr(TypeConstrSet(ts, _)) = ts

    fun mkFunctionType (arg, result) = FunctionType {arg = arg, result = result};

    (* A labelled record. *)
    fun mkLabelled(l, true) = LabelledRecord l

    |   mkLabelled(l, false) =
        (* Flexible: create the list of actual fields and the list of field names shared with
           every instance.  Set the default properties for new fields. *)
            FlexibleRecordVar
            {
                recList = ref(ChainEnd l),
                fullList = ref(ChainEnd{names=map #name l, frozen=false}), 
                equality = ref(ChainEnd false),
                level = ref(ChainEnd Generalisable)
            }

    (* Must remove leading zeros because the labels are compared by
       string comparison. *)
    fun mkLabelEntry (name, t) = 
    let
        fun stripZeros s = 
            if size s <= 1  orelse String.str(String.sub(s, 0)) <> "0" 
            then s
            else stripZeros (String.substring(s, 1, size s-1));
    in
        {name = stripZeros name, typeOf = t}
    end

    (* Create a name for a type variable. *)
    fun makeTypeVariableName(n, isEq) =
    let
        fun name num = (if num >= 26 then name (num div 26 - 1) else "")
              ^ String.str (Char.chr (num mod 26 + Char.ord #"a"))
        val name = (if isEq then "''" else "'") ^ name(n + 1)
    in
        name
    end

    (* Create a bound variable and set its name. *)
    fun createNewBoundVar isEq (index as TVIndex n) = BoundTypeVar(makeTypeVariableName(n, isEq), index)

    (* Create a bound variable.  Give it the name 'a, 'b etc.  Create the first ten
       in advance to reduce duplication. *)
    local
        fun makeVar eq n = createNewBoundVar eq (TVIndex n)
        val nonEqVars = Vector.tabulate(10, makeVar false)
        and eqVars = Vector.tabulate(10, makeVar true)
    in
        fun createBoundVar true n = if n < 10 then Vector.sub(eqVars, n) else makeVar true n
        |   createBoundVar false n = if n < 10 then Vector.sub(nonEqVars, n) else makeVar false n
    end

    (* Functions to construct the run-time representations of type constructor values,
       type values and value constructors.  These are all tuples and centralising the
       code here avoids having the offsets as integers at various places.
       Monotype constructor and type values are almost the same except that
       type values have the printer entry as the function whereas monotype
       constructors have the print entry as a ref pointing to the function,
       allowing addPrettyPrint to set a printer for the type.  The entries
       for polytypes are functions that take the type values as arguments
       and return the corresponding values. *)
    structure TypeValue =
    struct
        val equalityOffset = 0
        and printerOffset = 1

        fun extractEquality idCode = mkInd(equalityOffset, idCode)
        and extractPrinter idCode = mkInd(printerOffset, idCode)

        fun createTypeValue{eqCode, printCode} = mkTuple[eqCode, printCode]
    end

    (* Value constructors are represented by tuples, either pairs for nullary constructors
       or triples for constructors with arguments.  For nullary functions the "injection"
       function is actually the value itself.  If this is a polytype all the entries are
       functions that take the type values for the base types as arguments. *)
    structure ValueConstructor =
    struct
        val testerOffset = 0
        val injectorOffset = 1
        val projectorOffset = 2

        fun extractTest constrCode = mkInd(testerOffset, constrCode)
        and extractInjection constrCode = mkInd(injectorOffset, constrCode)
        and extractProjection constrCode = mkInd(projectorOffset, constrCode)

        fun createValueConstr{testMatch, injectValue, projectValue} = mkTuple[testMatch, injectValue, projectValue]
        fun createNullaryConstr{ testMatch, constrValue } = mkTuple[testMatch, constrValue]
    end

    (* Eqtypes with built-in equality functions.  The printer functions are all replaced in the basis. *)
    local
        open Address PRETTY TypeValue
        fun defaultMonoTypePrinter _ = PrettyString "?"
        fun defaultPolyTypePrinter _ _ = PrettyString "?"

        fun eqAndPrintCode (eqCode, nArgs) =
        let
            val code =
                if nArgs = 0
                then createTypeValue{
                        eqCode=eqCode,
                        printCode=mkConst (toMachineWord (ref defaultMonoTypePrinter))}
                else createTypeValue{
                        eqCode=mkInlproc(eqCode, nArgs, "eq-helper()", [], 0),
                        printCode=mkConst (toMachineWord (ref defaultPolyTypePrinter))}
        in
            Global (genCode(code, [], 0) ())
        end

        fun makeConstr(name, fullName, eqFun) =
            makeTypeConstructor (name,
                makeFreeId(0, eqAndPrintCode(eqFun, 0), true, basisDescription fullName),
                [DeclaredAt inBasis])
    
        (* since code generator relies on these representations,
           we may as well export them *)

        (* Strings are now always vectors whose first word is the length.
           The old special case for single-character strings has been removed. *)
        local
            val stringEquality =
                mkInlproc(
                    (* This previously checked for pointer equality first.  That has
                       been removed.
                       Test the lengths first and only do the byte comparison if
                       they are the same.  This seems to save more time than including
                       the length word in the byte comparison. *)
                    mkCand(
                        mkEqualTaggedWord(
                            mkLoadOperation(LoadStoreUntaggedUnsigned, mkLoadArgument 0, CodeZero),
                            mkLoadOperation(LoadStoreUntaggedUnsigned, mkLoadArgument 1, CodeZero)),
                        mkBlockOperation{kind=BlockOpEqualByte, leftBase=mkLoadArgument 0, rightBase=mkLoadArgument 1,
                                leftIndex=mkConst(toMachineWord wordSize), rightIndex=mkConst(toMachineWord wordSize),
                                    (* Use argument 1 here rather than 0.  We could use either but this works
                                       better when we're using equality for pattern matching since it
                                       gets the length of the constant string.  It also works better
                                       for the, to me, more natural ordering of variable=constant. *)
                                length=mkLoadOperation(LoadStoreUntaggedUnsigned, mkLoadArgument 1, CodeZero)
                        }
                    ),
                    2, "stringEquality", [], 0)
        in
            val stringEquality = stringEquality
        end

        local
            (* Arbitrary precision values are normalised so if a value can be represented
               as a tagged fixed precision value it will be.
               Unlike strings it is much more likely that the value will be short so we
               generate equality as a test that handles the short case as inline code and
               the long case as a function call.  If either argument is a short constant
               this will be optimised away so the test will reduce to a test on whether
               the value equals the constant. *)
            val intEquality =
                mkEnv(
                    [mkDec(0,
                        (* Long-form equality - should not be inlined. *)
                        mkProc(
                            (* Equal if  signs are the same ... *)
                            mkCand(
                                mkEqualTaggedWord(
                                    mkUnary(BuiltIns.MemoryCellFlags, mkLoadArgument 0),
                                    mkUnary(BuiltIns.MemoryCellFlags, mkLoadArgument 1)
                                ),
                                mkEnv(
                                    [mkDec(0, mkUnary(BuiltIns.MemoryCellLength, mkLoadArgument 1))],
                                    mkCand(
                                        (* ... and the lengths are equal ... *)
                                        mkEqualTaggedWord(
                                            mkUnary(BuiltIns.MemoryCellLength, mkLoadArgument 0),
                                            mkLoadLocal 0
                                        ),
                                        (* ... and they're byte-wise equal .*)
                                        mkBlockOperation{kind=BlockOpEqualByte, leftBase=mkLoadArgument 0, rightBase=mkLoadArgument 1,
                                            leftIndex=CodeZero, rightIndex=CodeZero,
                                            length=mkBinary(BuiltIns.WordArith BuiltIns.ArithMult,
                                                mkConst(toMachineWord RunCall.bytesPerWord), mkLoadLocal 0)}
                                    )
                                )
                            ),
                            2, "arbitraryPrecisionEquality", [], 1)
                        )
                    ],
                mkInlproc(
                    mkCor( (* Either they're equal... *)
                           (* N.B. The values could be short or long.  That's particularly important
                              if we have a series of tests against short constants.  If we convert it to
                              an indexed case we MUST check that the value is short before computing
                              the index. *)
                        mkEqualPointerOrWord(mkLoadArgument 0, mkLoadArgument 1),
                        (* .. or if either is short the result is false ... *)
                        mkCand(
                            mkCand(
                                mkNot(mkIsShort(mkLoadArgument 0)),
                                mkNot(mkIsShort(mkLoadArgument 1))
                            ),
                            (* ... otherwise we have to test the vectors. *)
                            mkEval(mkLoadClosure 0, [mkLoadArgument 0, mkLoadArgument 1])
                        )
                     ),
                     2, "intInfEquality", [mkLoadLocal 0], 0)
                )
        in
            (* Code-generate the function and return the inline part. 
               We need to set the maximum inline size here to ensure the long form
               code is not inlined.  It would be better to have a way of turning off
               inlining for specific functions. *)
            val intEquality = genCode(intEquality, [Universal.tagInject DEBUG.maxInlineSizeTag 5], 1) ()
        end
    in
        val fixedIntConstr = makeConstr("int",  "FixedInt.int", equalTaggedWordFn) (* Fixed precision is always short *)
        val intInfConstr = makeConstr("int",    "IntInf.int", intEquality)
        val charConstr   = makeConstr("char",   "char", equalTaggedWordFn) (* Always short *)
        val stringConstr = makeConstr("string", "string", stringEquality)
        val wordConstr   = makeConstr("word",   "word", equalTaggedWordFn)

        (* Ref is a datatype with a single constructor.  The constructor is added in INITIALISE.
           Equality is special for "'a ref", "'a array" and "'a Array2.array".  They permit equality
           even if the 'a is not an eqType. *)
        val refConstr =
            makeTypeConstructor 
                ("ref",
                makeFreeId(1, eqAndPrintCode(equalPointerOrWordFn, 1), true, basisDescription "ref"),
                [DeclaredAt inBasis]);
        val arrayConstr =
            makeTypeConstructor 
                ("array",
                makeFreeId(1, eqAndPrintCode(equalPointerOrWordFn, 1), true, basisDescription "Array.array"),
                [DeclaredAt inBasis]);
        val array2Constr =
            makeTypeConstructor 
                ("array",
                makeFreeId(1, eqAndPrintCode(equalPointerOrWordFn, 1), true, basisDescription "Array2.array"),
                [DeclaredAt inBasis]);
        val byteArrayConstr =
            makeTypeConstructor 
                ("byteArray",
                makeFreeId(0, eqAndPrintCode(equalPointerOrWordFn, 0), true, basisDescription "byteArray"),
                [DeclaredAt inBasis]);
        (* Bool is a datatype.  The constructors are added in INITIALISE. *)
        val boolConstr =
            makeTypeConstructor 
                ("bool", makeFreeId(0, eqAndPrintCode(equalTaggedWordFn, 0), true, basisDescription "bool"),
                [DeclaredAt inBasis]);
    end

    fun isSameTypeIdAs(TypeConstrs{identifier=idA, ...}, TypeConstrs {identifier=idB, ...}) = sameTypeId(idA, idB)

    (* These polytypes allow equality even if the type argument is not an equality type. Used locally and also
       in TypeIDCode. *)
    fun isPointerEqType constr =
        isSameTypeIdAs (constr, refConstr) orelse
        isSameTypeIdAs (constr, arrayConstr) orelse
        isSameTypeIdAs (constr, array2Constr) orelse
        isSameTypeIdAs (constr, byteArrayConstr)

    (* Non-eqtypes *)
    local
        open Address PRETTY TypeValue

        fun makeType(name, descr) =
        let
            fun defaultPrinter _ = PrettyString "?"
            val code =
                createTypeValue{
                    eqCode=CodeZero (* No equality. *),
                    printCode=mkConst (toMachineWord (ref defaultPrinter))
                }
        in
            makeTypeConstructor (
                name, makeFreeId(0, Global (genCode(code, [], 0) ()), false, descr),
                [DeclaredAt inBasis])
        end
    in
        val realConstr   = makeType("real", basisDescription "real") (* Not an eqtype in ML97. *)
        (* Short real: Real32.real *)
        val floatConstr  =
            makeType("real", basisDescription "real")
        val exnConstr    = makeType("exn", basisDescription "exn");
        (* "undefConstr" is used as a place-holder during parsing for the actual type constructor.
           If the type constructor is not found this may appear in an error message. *)
        val undefConstr  =
            makeType("undefined", { location = inBasis, description = "Undefined", name = "undefined" })
    end
              
    fun isUndefinedTypeConstr cons = isSameTypeIdAs (cons, undefConstr)

    (* Similar to alphabetic ordering except that shorter labels come before longer ones.
       This has the advantage that numerical labels are compared by their numerical order
       i.e. 1 < 2 < 10 whereas alphabetic ordering puts "1" < "10" < "2". *)
    fun compareLabels (a, b) =
        case Int.compare(size a, size b) of EQUAL => String.compare(a, b) | order => order

    (* Sort using the label ordering.
       A simple sort routine - particularly if the list is already sorted. *)
    fun sortLabels [] = []
    |   sortLabels (s::rest) =
    let
        fun enter s _    [] = [s]
        |   enter s name (l as ( (h as {name=hname, ...}) :: t)) =
        (
            case compareLabels (name, hname) of
                GREATER => h :: enter s name t
            |   _ => s :: l
        )
    in  
        enter s (#name s) (sortLabels rest)
    end

    (* Table of overloads.  Entries are added to this during the bootstrap process using addOverload.
       Overloaded functions are the standard overloads: +, -, *, div, mod together with literal constant
       conversion functions convString etc. *)
    structure Overloads =
    struct
        open HashTable
        val nOverloads = 16
            (* +, -, *, div, mod, <, >, <=, >=, ~, abs, convInt, convWord, convReal, convChar, convString, *)
        val overloadTable: (typeConstrs * codetree) list hash =
            hashMake(nOverloads*3 div 2) (* Add some extra to avoid having to extend the table. *)

        (* Add to the overload list, removing any duplicates.  During the bootstrap process the overloads
           for literal converters are replaced. *)
        fun addOverload (n, cons, code) =
        let
            val existing = getOpt(hashSub(overloadTable, n), [])
            val filtered = List.filter(fn (c, _) => not(isSameTypeIdAs(c, cons))) existing
        in
            hashSet(overloadTable, n, (cons, code) :: filtered)
        end

        (* Return all the overloads for a particular identifier.  This is used to populate
           the overload list when an overloaded function is used. *)
        fun getOverloadTypes n = List.map #1 (getOpt(hashSub(overloadTable, n), []))

        (* Return the overload for a particular type.  It should only ever be called for a name and type
           that was previously returned by getOverloadTypes. *)
        fun getOverloadCode(n, TypeConstrs {identifier=consId,...}) =
            case hashSub(overloadTable, n) of
                SOME constrAndCode =>
                (
                    case List.find (fn (TypeConstrs {identifier=tId,...}, _) => sameTypeId (consId, tId)) constrAndCode of
                        SOME(_, v) => v
                    |   NONE => raise InternalError "getOverload: not overloaded on this type"
                )
            |   NONE => raise InternalError "getOverload: unknown overload"

      end

    (* Expand the field list of a flexible record to include all the fields that
       have been added to the generic instance since this record was created. *)
    fun expandFlexRecord(currentFields, names, level, equality) =
    let
        fun createNewField name = { name = name, typeOf = mkTypeVar(level, equality)}

        fun addToInstance([], []) = []
        |   addToInstance(generic :: geRest, []) = createNewField generic :: addToInstance(geRest, [])
        |   addToInstance([], instance) = instance
                (* This case can occur if we are producing an error message because of
                   a type-incorrect program so we just ignore it. *)
        |   addToInstance(generic :: geRest, inst as instance :: iRest) =
            (
                case compareLabels (generic, #name instance) of
                    EQUAL => instance :: addToInstance(geRest, iRest)
                |   LESS => createNewField generic :: addToInstance(geRest, inst)
                |   GREATER => (* This is another case that can occur with type-incorrect code. *)
                        instance :: addToInstance(generic :: geRest, iRest)
            )
    in
        if List.length names = List.length currentFields
        then currentFields(* All the generic fields are present so we don't need to do anything. *)
        else addToInstance(names, currentFields)
    end

    (* Test to see is a type constructor is in an overload set. *)
    fun isInSet(tcons, tyConlist) = List.exists(fn h => isSameTypeIdAs(tcons, h)) tyConlist

    val prefInt = ref fixedIntConstr

    (* Returns the preferred overload if there is one. *)
    fun preferredOverload typeset =
        if isInSet(!prefInt, typeset)
        then SOME(!prefInt)
        else if isInSet(realConstr, typeset)
        then SOME realConstr
        else if isInSet(wordConstr, typeset)
        then SOME wordConstr
        else if isInSet(charConstr, typeset)
        then SOME charConstr
        else if isInSet(stringConstr, typeset)
        then SOME stringConstr
        else NONE

    fun setPreferredInt c = prefInt := c

  (* This 3-valued logic is used because in a few cases we may not be sure
     if equality testing is allowed. If we have 2 mutually recursive datatypes
     t = x of s | ... and s = z of t we would first examine "t", find that
     it uses "s", look at "s", find that refers back to "t". To avoid
     infinite recursion we return the result that equality "maybe"
     allowed for "t" and hence for "s". However we may find that the
     other constructors for "t" do not allow equality and so equality
     will not be allowed for "s" either. *)
     
    datatype tri = Yes (* 3-valued logic *) | No | Maybe

    (* Copy a type, avoiding copying type structures unnecessarily.
       Used to make new type variables for all distinct type variables when
       generalising polymorphic functions, and to make new type stamps for
       type constructors when generalising signatures. *)
    fun copyType (at, indexBTV, copyTypeConstr) =
    let
        fun copyRecordList mapBTV =
            List.map(fn{name, typeOf} => {name=name, typeOf=copyType (typeOf, mapBTV, copyTypeConstr)})

        val (atyp, mapBTV) = canonicalise(at, indexBTV)
    in
        case atyp of
            TypeVar _ => atyp (* Unbound type variable. *)

        |   BoundTypeVar(_, index) =>
                (* Instantiate the type variable if there is a map.  Currently this can also be called when
                   processing signatures so allow for the look-up to fail and return the original. *)
                getOpt(mapBTV index, atyp)

        |   FreeTypeVar _ => atyp

        |   TypeConstruction {constr, args, locations, ...} => 
            let
                val copiedArgs = List.map(fn t => copyType (t, mapBTV, copyTypeConstr)) args
                val copiedConstr  as TypeConstrs {name=copiedName,...} = copyTypeConstr constr
                (* Use the name from the copied constructor.  This will normally
                   be the same as the original EXCEPT in the case where we are
                   using copyType to generate copies of the value constructors of
                   replicated datatypes. *)
            in
                mkTypeConstruction (copiedName, copiedConstr, copiedArgs, locations)
            end 
           
        |   FunctionType {arg, result} => 
            let
                val copiedArg = copyType (arg, mapBTV, copyTypeConstr)
                val copiedRes = copyType (result, mapBTV, copyTypeConstr)
            in
                FunctionType {arg = copiedArg, result = copiedRes}
            end

        |   FlexibleRecordVar{recList=ref recl, fullList as ref full, equality=ref eq, level=ref lvl} =>
            let
                val recList = followRefChainToEnd recl
                val {names, frozen} = followRefChainToEnd full
                and equality = followRefChainToEnd eq
                and level = followRefChainToEnd lvl
                (* Add any new fields.  Each field has to be a new type variable modelled on the generic but distinct. *)
                val copiedList = expandFlexRecord(copyRecordList mapBTV recList, names, level, equality)
            in
                if frozen
                then LabelledRecord copiedList
                else FlexibleRecordVar{recList=ref(ChainEnd copiedList), fullList=fullList,
                        equality = ref(ChainEnd equality), level = ref(ChainEnd level)}
            end

        |   LabelledRecord recList => LabelledRecord(copyRecordList mapBTV recList)

            (* Overload.  Return the type constructor if it's been resolved. *)
        |   typ as OverloadSetVar(ref c) =>
            (
                case followRefChainToEnd c of
                    [tcons as TypeConstrs{name, ...}] => mkTypeConstruction (name, tcons,[], [])
                |   _ => typ
            )

        |   BadType => BadType

    end (* copyType *)

    (* Generate the instance by applying any copying necessary. *)
    and reduceToType(Instance(typeOf, indexBTV)) =
        copyType(typeOf, indexBTV, fn tcon => copyTypeConstr (tcon, fn _ => NONE, fn s => s))
    |   reduceToType(SimpleInstance t) =
        copyType(t, fn _ => NONE, fn tcon => copyTypeConstr (tcon, fn _ => NONE, fn s => s))

    (* Copy a type constructor if it is Bound and in the required range.  If this refers to a type
       function copies that as well. Does not copy value constructors. When copying a signature
       type constructors are copied first and then passed in here as the cache argument. *)
    and copyTypeConstrWithCache (tcon as TypeConstrs{identifier=tcId, locations, name=tcName, ...}, typeMap, mungeName, cache) =
        case tcId of
            TypeId{idKind = TypeFn{arity, resType=equiv, ...}, description, access, ...} =>
            let
                val copiedEquiv =
                    copyType(equiv, fn _ => NONE, fn tcon => copyTypeConstr(tcon, typeMap, mungeName))
            in
                (* How do we find a type function? *)
                makeTypeConstructor (mungeName tcName,
                    makeGeneralTypeFunction(arity, copiedEquiv, description, access),
                        locations)
            end

       |    id =>
            (
                case typeMap id of
                    NONE =>
                    (
                        (*print(concat[tcName tcon, " not copied\n"]);*)
                        tcon (* No change *)
                    )
                |   SOME newId =>
                    let
                        val name = #second(splitString tcName)
                        (* We must only match here if they're really the same. *)
                        fun cacheMatch (TypeConstrs{identifier, name=tcName, ...}) =
                            sameTypeId(identifier, newId)
                                andalso #second(splitString tcName) = name
                    in
                        case List.find cacheMatch cache of
                            SOME tc =>
                            (
                                (*print(concat[tcName tcon, " copied as ", tcName tc, "\n"]);*)
                                tc (* Use the entry from the cache. *)
                            )
                        |   NONE =>
                            (* Either a hidden identifier or alternatively this can happen as part of
                               the matching process.
                               When matching a structure to a signature we first match up the type
                               constructors then copy the type of each value replacing bound type IDs
                               with the actual IDs as part of the checking process.
                               We will return SOME newId but we don't have a
                               cache so return NONE for List.find. *)
                            let
                                val newName = mungeName tcName
                            in
                                (*print(concat[tcName tcon, " not cached\n"]);*)
                                makeTypeConstructor(newName, newId, locations)
                            end
                    end
            )

    and copyTypeConstr (tcon, typeMap, mungeName) = copyTypeConstrWithCache(tcon, typeMap, mungeName, [])

    (* Create a type function and compute the relevant information. *)
    and makeGeneralTypeFunction(arity, resType, desc, access) =
    let
        (* If this refers to another type construction involving a type function add one
           to its type-function count.  When seeing if two type constructions unify we
           unwrap the larger count.  e.g. type s = int*int type t = s  when unifying s and t
           we want to unwrap the "t" and not the "s". *)
        val nextFunCount =
            case resType of
                TypeConstruction{constr=TypeConstrs{
                        identifier=TypeId{idKind=TypeFn{typeFunCount, ...}, ...}, ...}, ...} =>
                    typeFunCount+1
            |   _ => 1
        (* It is possible that one or more type variables are unreferenced.  They must be
           ignored during unification. *)
        val refArray = BoolArray.array(arity, false)
        local
            fun processTypes typ =
            let
                val t = eventual typ
            in
                case t of
                    TypeVar _=> raise InternalError "TypeVar"
                |   BoundTypeVar(_, TVIndex n) => BoolArray.update(refArray, n, true)
                |   FunctionType {arg, result} => (processTypes arg; processTypes result)
                |   LabelledRecord recList => List.app (processTypes o #typeOf) recList
                    (* Type construction of type function.  Only include referenced variables. *)
                |   TypeConstruction{constr=TypeConstrs{
                        identifier=TypeId{idKind=TypeFn{usedTvs, ...}, ...}, ...}, args, ...} =>
                    let
                        fun process(arg, n) =
                            (if BoolVector.sub(usedTvs, n) then processTypes arg else (); n+1)
                        val _ = List.foldl process 0 args
                    in
                        ()
                    end
                    (* Type construction of datatype. *)
                |   TypeConstruction {args, ...} => List.app processTypes args
                |   _ => ()
            end
        in
            val _ = processTypes resType
            val refVector = BoolArray.vector refArray
        end

        (* Equality *)
        (* Unlike datatype constructions a type function is equal to its definition.
           That means that only certain of the arguments can affect equality.
           e.g. type ('a, 'b, 'c) t = ('a * 'b ref).  (int, real, real) t admits equality. *)
        local
            val eqRefArray = BoolArray.array(arity, false)
            fun lookupBTV(TVIndex i) = BoolArray.update(eqRefArray, i, true)
            val eq = computeEqualityWithinTypeConstructor(resType, fn _ => No, lookupBTV)
        in
            val eqRes =
                case eq of
                    No => TypeFnEqNever
                |   _ => TypeFnEq(BoolArray.vector eqRefArray)
        end

        val typeFn =
        {
            arity = arity,
            resType = resType,
            usedTvs = refVector,
            typeFunCount = nextFunCount,
            equality = ref eqRes,
            uid = makeUniqueId()
        }
    in
        TypeId { access=access, description = desc, idKind = TypeFn typeFn}
    end

    (* Tests for equality for a type.  It is only used in datatype and type constructors.
       lookupTypId is used when computing equality for a set of possibly mutually recursive
       datatypes and types (from withtype) to get information about others in the set.
       referenceBoundId indicates that a bound ID was actually used and is used when
       computing equality for a type function.  It indicates that equality for the
       type function does actually depend on the bound variable.   *)
    
    and computeEqualityWithinTypeConstructor (ty, lookupTypId, referenceBoundId) : tri =
    let
        (* Can't use foldT because it is not monotonic: equality on ref 'a is allowed. *)
        (* Returns Yes only if equality testing is allowed for all types in the list. *)
        fun eqForList ([],    soFar) = soFar
        |   eqForList (x::xs, soFar) = 
            case computeEqualityWithinTypeConstructor (x, lookupTypId, referenceBoundId) of
                No    => No
            |   Maybe => eqForList (xs, Maybe)
            |   Yes   => eqForList (xs, soFar)
    in
        case ty of
            BoundTypeVar(_, index) => (referenceBoundId index; Yes) (* It is always true. *)

        |   FunctionType {...} => No  (* No equality on function types! *)

        |   TypeConstruction {constr=TypeConstrs
                {identifier as TypeId{idKind = TypeFn{equality, ...}, ...},...}, args, ...} =>
            let
                (* Check this identifier.  If this is a "withtype" it may be processed now
                   which will modify the "equality" ref. When computeEquality is called
                   other than from computeDatatypeEqualities lookupTypId returns No.
                   That doesn't matter for datatypes since we first look at the equality
                   state for the datatype and only call lookupTypId if it is false. *)
                val testId = lookupTypId identifier
            in
                case !equality of
                    TypeFnEqNever => No
                |   TypeFnEq bv =>
                    let
                        (* Only some of the arguments may be necessary for equality.  See comment in makeGeneralTypeFunction. *)
                        fun filterArgs(arg, (n, l)) = (n+1, if BoolVector.sub(bv, n) then arg::l else l)
                        val (_, filtered) = List.foldl filterArgs (0, []) args
                    in
                        eqForList (filtered, case testId of Maybe => Maybe | _ => Yes)
                    end
            end

        |   TypeConstruction {constr as TypeConstrs{identifier, ...}, args, ...} =>
            (* ref - Equality is permitted on refs of all types *)
            (* The Definition of Standard ML says that ref is the ONLY type
               constructor which is treated in this way.  The standard basis
               library says that other mutable types such as array should
               also work this way. *)
            if isUndefinedTypeConstr constr orelse isPointerEqType constr
            then Yes

            (* Others apart from ref and real *)
            else if isEquality identifier (* Equality allowed. *)
            then eqForList (args, Yes) (* Must be allowed for all the args *)
      
            else
            let (* Not an alias. - Look it up. *)
                val s = lookupTypId identifier
            in 
                if s = No then No else eqForList (args, s)
            end (* TypeConstruction *)
         
        |   LabelledRecord recList => (* Record equality if all subtypes *)
                eqForList (map #typeOf recList, Yes)

        |   BadType => Yes

        |   _ => raise InternalError "computeEqualityWithinTypeConstructor: bad type"
    end

    (* Gets the final type in a chain.  Extends flexible records by adding
       extra fields if necessary. *)
    and eventual (t as (TypeVar(TypeVariable{link=ref l, ...}))) =
        (
            (* Get the last entry in the chain and see if it has been set. *)
            case followRefChainToEnd l of
                NONE => t (* Not bound to anything - return the type variable *)
            |   SOME newVal => eventual(reduceToType newVal) (* Return the type it is bound to *)
        )
    
    |   eventual(t as FlexibleRecordVar{recList, fullList=ref fullList, equality=ref eql, level=ref lvl}) =
        (* Add on any fields to the record that were not previously present and have been added
           by a different instance. *)
        let
            val {names, frozen} = followRefChainToEnd fullList
            and (recListVar, currentFields) = followRefChainToRef recList
            val equality = followRefChainToEnd eql
            val level = followRefChainToEnd lvl
            val newFieldList = expandFlexRecord(currentFields, names, level, equality)
            val () = recListVar := ChainEnd newFieldList
        in
            if frozen (* Return a fixed record *)
            then LabelledRecord newFieldList
            else t
        end
    
    |   eventual t (* not a type variable *) = t

    and getCanonical(Instance(ty, bvMap)) = canonicalise(ty, bvMap)
    |   getCanonical(SimpleInstance ty) = canonicalise(ty, fn _ => NONE)

    (* Reduce the instance by following type variable chains, looking up bound type
       variables and expanding labelled records. *)
    and canonicalise(original as (TypeVar(TypeVariable{link=ref l, ...}), _)) =
        (
            (* Get the last entry in the chain and see if it has been set. *)
            case followRefChainToEnd l of
                NONE => original (* Not bound to anything - return the type variable *)
            |   SOME newVal => getCanonical newVal
        )

    |   canonicalise(original as (BoundTypeVar(_, index), bvMap)) =
        (
            case bvMap index of
                SOME ty =>
                (
                    case ty of
                        BoundTypeVar(_, newIndex) => if newIndex = index then raise InternalError "Looping type variables" else ()
                    |   _ => ();
                    canonicalise(ty, bvMap)
                )
            |   NONE => original
        )

    |   canonicalise(original as (FlexibleRecordVar{recList, fullList=ref fullList, equality=ref eql, level=ref lvl}, bvMap)) =
        (* Add on any fields to the record that were not previously present and have been added
           by a different instance. *)
        let
            val {names, frozen} = followRefChainToEnd fullList
            and (recListVar, currentFields) = followRefChainToRef recList
            val equality = followRefChainToEnd eql
            val level = followRefChainToEnd lvl
            val newFieldList = expandFlexRecord(currentFields, names, level, equality)
            (* Add the new fields onto the original,  There may be other references. *)
            val () = recListVar := ChainEnd newFieldList
        in
            if frozen (* Return a fixed record *)
            then (LabelledRecord newFieldList, bvMap)
            else original
        end

    |   canonicalise original = original

    (* Get a type from an instance.  Needed when we have to construct a type, say a tuple, from components. *)
    fun instanceToType(SimpleInstance ty) = ty
    |   instanceToType(i as Instance _) = TypeVar(makeTv {value=SOME i, level=Generalisable, equality=false })

    fun getInstance(Instance i) = i
    |   getInstance(SimpleInstance ty) = (ty, fn _ => NONE)

    datatype generalMatch = Matched of {old: typeVar, new: types}

    (* Create type variables.  If unifiable is true, the usual case, the type
       variables are new assignable variables.
       If it is false, used when creating a type to be matched from a structure, type variables
       are nonunifiable. This case occurs if we have a structure without an explicit signature.
       The signature is created from the structure but it might have had overloading or flex
       records that have only be resolved later. *)
    fun createTypeVariable (TemplPlain{ equality }) =
            TypeVar(makeTv {value=NONE, level=Generalisable, equality=equality})
    |   createTypeVariable (TemplOverload s) = OverloadSetVar(ref(ChainEnd(Overloads.getOverloadTypes s)))
    |   createTypeVariable (TemplFlexRec{ equality, recList, fullList }) =
            FlexibleRecordVar{
                recList=ref(ChainEnd recList), fullList=fullList,
                equality=ref(ChainEnd equality), level=ref(ChainEnd Generalisable)
            }

    (* Create a map for type variables.  Optimise the most common cases *)
    fun createTypeVariableMap [] = (fn _ => raise InternalError "createTypeVariableMap")

    |   createTypeVariableMap [a] =
        let
            val tv = createTypeVariable a
        in
            fn TVIndex 0 => tv | _ => raise InternalError "createTypeVariableMap"
        end

    |   createTypeVariableMap [a, b] =
        let
            val tvA = createTypeVariable a
            and tvB = createTypeVariable b
        in
            fn TVIndex 0 => tvA | TVIndex 1 => tvB | _ => raise InternalError "createTypeVariableMap"
        end

    |   createTypeVariableMap [a, b, c] =
        let
            val tvA = createTypeVariable a
            and tvB = createTypeVariable b
            and tvC = createTypeVariable c
        in
            fn TVIndex 0 => tvA | TVIndex 1 => tvB | TVIndex 2 => tvC | _ => raise InternalError "createTypeVariableMap"
        end

    |   createTypeVariableMap templs =
        let
            val tvs = map createTypeVariable templs
            val tvVector = Vector.fromList tvs
        in
            fn TVIndex n => Vector.sub(tvVector, n)
        end

    (* Create an instance for a generalisable value, e.g. a polymorphic function. *)
    fun generalise(atyp, templs) =
    let
        (* Create free type variables for each template. *)
        val freeTypeMap = createTypeVariableMap templs
        val typeVarList = List.tabulate(List.length templs, fn n => freeTypeMap(TVIndex n))
    in
        (Instance(atyp, SOME o freeTypeMap), typeVarList)
    end

    (* Apply a function to every element of a type. *)
    fun foldType f (typ, map) =
    let
        fun foldT (typ, map) v =
        let
            val (t, m)   = canonicalise(typ, map)
            val res = f (t, m) v (* Process this entry. *)
        in
            case t of
                TypeVar _ => res (* It can only be an unbound type variable. *)

            |   TypeConstruction {args, ...} => (* Then process the arguments. *)
                    List.foldr (fn (t, v) => foldT (t, m) v) res args
           
            |   FunctionType {arg, result} => foldT (arg, m) (foldT (result, m) res)

            |   LabelledRecord recList =>
                    List.foldr (fn ({ typeOf, ... }, v) => foldT(typeOf, m) v) res recList

            |   FlexibleRecordVar {recList=ref recList,...} =>
                    List.foldr (fn ({ typeOf, ... }, v) => foldT(typeOf, m) v) res (followRefChainToEnd recList)

            |   BoundTypeVar _ => res (* We currently use this to check for free type variables on a generalised value without using a map. *)

            |   FreeTypeVar _ => res

            |   BadType => res
          
            |   OverloadSetVar _ => res
        end
    in
        foldT(typ, map)
    end

    (* Can this be used to set an eqtype? Used in "where type" constructions in signatures. *)
    fun typePermitsEquality ty = computeEqualityWithinTypeConstructor (ty, fn _ => No, fn _ => ()) <> No

  (* When a datatype is declared we test to see if equality is allowed. The
     types are mutually recursive so value constructors of one type may
     take arguments involving values of any of the others. *)
    fun computeDatatypeEqualities(types: typeConstrSet list, boundIdEq) =
    let
        datatype state =
            Processed of tri              (* Already processed or processing. *)
        |   NotSeen   of typeConstrSet    (* Value is list of constrs. *);
    
        (* This table tells us, for each type constructor, whether it definitely
           admits equality, definitely does not or whether we have yet to look
           at it. *)
        local
            (* Extend the test to include type functions. isSameId currently returns false for type fns. *)
            fun isSameId(TypeId{idKind=TypeFn{uid=uidA, ...}, ...}, TypeId{idKind=TypeFn{uid=uidB, ...}, ...}) =
                    uidA = uidB
            |   isSameId(a, b) = sameTypeId(a, b)
        in
            val {enter:typeId * state -> unit, lookup} = mapTable isSameId
        end

        (* Look at each of the constructors in the list. Equality testing is
           only allowed if it is allowed for each of the alternatives. *)
        fun constrEq _           []       soFar = soFar (* end of list - all o.k. *)
        |   constrEq constructor ((Value{typeOf=ValueType(valTypeOf, _), ...}) :: t) soFar =
            (* The constructor may be a constant e.g.
               datatype 'a list = nil | ... or  a function e.g.
               datatype 'a list = ... cons of 'a * 'a list. *)
            if not (isFunctionType valTypeOf) (* Constant *)
            then constrEq constructor t soFar (* Go on to the next. *)
      
            else
            let
                (* Function - look at the argument type. *)
                (* Equality is allowed for any type-variable.  The only type variables
                   allowed are parameters to the datatype so if we have a type variable
                   then equality is allowed for this datatype.  *)
                val eq = 
                    computeEqualityWithinTypeConstructor (#arg (typesFunctionType valTypeOf),
                        genEquality, fn _ => ())
            in
                if eq = No
                then (* Not allowed. *) No
                else (* O.k. - go on to the next. *)
                    constrEq constructor t (if eq = Maybe then Maybe else soFar)
            end (* constrEq *)

        (* This procedure checks to see if equality is allowed for this datatype. *)
        and genEquality constructorId =
        let 
            (* Look it up to see if we have already done it. It may fail because
               we may have constructors that do not admit equality. *)
            val thisState =
                case (lookup constructorId, constructorId) of
                    (SOME inList, _) => inList
                |   (NONE, TypeId{idKind = Bound{offset, ...}, ...}) =>
                        Processed(if boundIdEq offset then Yes else No)
                |   _ => Processed No
        in
            case thisState of
                Processed tri => tri (* Have either done it already or are currently doing it. *)
            |   NotSeen (TypeConstrSet(tyConstr, valConstrs)) => (* notSeen - look at it now. *)
                let
                    (* Set the state to "Maybe". This prevents infinite recursion. *)
                    val () = enter (constructorId, Processed Maybe)
                in
                    case constructorId of
                        TypeId{idKind = TypeFn{resType, equality, arity, ...}, ...} => (* withtype *)
                        let
                            val eqRefArray = BoolArray.array(arity, false)
                            fun lookupBoundTV(TVIndex i) = BoolArray.update(eqRefArray, i, true)
                            val eq = computeEqualityWithinTypeConstructor (resType, genEquality, lookupBoundTV)
                            val eqRes =
                                case eq of
                                    No => TypeFnEqNever
                                |   _ => TypeFnEq(BoolArray.vector eqRefArray)
                        in
                            equality := eqRes;
                            eq
                        end

                    |   _ => (* datatype - process the constructors *)
                        let
                            val eq = constrEq tyConstr valConstrs Yes
                        in
                            (* Set the state we have found if it is "yes" or "no".  If it is
                               maybe we have a recursive reference which appears to admit
                               equality, but may not. E.g. if we have
                                         datatype t = A of s | B of int->int  and  s = C of t
                               if we start processing "t" we will go on to "s" and do that
                               before returning to "t". It is only later we find that "t" does
                               not admit equality. If we get "Maybe" as the final result when
                               all the recursion has been unwound we can set the result to
                               "yes", but any intermediate "Maybe"s have to be done again. *)
                            enter (constructorId, if eq = Maybe then thisState else Processed eq);
                            eq
                        end
                end
        end (* genEquality *);
    in
        let
            (* Enter every datatype into the list as "NotSeen".  computeEquality only
                calls lookupTypId if equality for a datatype is false.  Ensure that here
                although currently new datatypes always have this.  Also set withtypes although
                that doesn't really matter. *)
            fun enterInitial(dec as TypeConstrSet(TypeConstrs{identifier, ...}, _)) =
            (
                case identifier of
                    TypeId{idKind = TypeFn{equality, ...}, ...} => equality := TypeFnEqNever
                |   _ => setEquality(identifier, false);
                enter (identifier, NotSeen dec)
            )
        in
            List.app enterInitial types
        end;

        (* Apply genEquality to each element of the list.  If this is a withtype it will *)
        let
            fun generateEq(TypeConstrSet(TypeConstrs{identifier, ...}, _)) =
                let
                    val eqForCons = genEquality identifier
                in
                    (* If the result is "Maybe" it involves a recursive reference, but
                       the rest of the type allows equality. The type admits equality. *)
                    if eqForCons = No
                    then () (* Equality not allowed *)
                    else
                    ( (* Turn on equality. *)
                        case identifier of
                            TypeId{idKind = TypeFn _, ...} => ()
                        |   _ => setEquality(identifier, true);
                        enter(identifier, Processed Yes)
                    )
                end
        in
            List.app generateEq types
        end
    end (* computeDatatypeEqualities *)


    fun makeTypeFunction(arity, resType, desc) =
        makeGeneralTypeFunction(arity, resType, desc, Global CodeZero)

    (* The unit type is equivalent to the empty record. *)
    val unitConstr   =
        makeTypeConstructor ("unit",
            makeTypeFunction(
                0, LabelledRecord [],
                { location = inBasis, description = "unit", name = "unit" }),
            [DeclaredAt inBasis]);


  (* Type identifiers bound to standard type constructors. *)

    val unitType = mkTypeConstruction ("unit", unitConstr, [], [])

    val fixedIntType = mkTypeConstruction ("int",   fixedIntConstr,    [], [])
    val stringType = mkTypeConstruction ("string",  stringConstr, [], [])
    val boolType   = mkTypeConstruction ("bool",    boolConstr,   [], [])
    val exnType    = mkTypeConstruction ("exn",     exnConstr,    [], [])

    (* Checks to see whether a labelled record is in the form of
       a product i.e. 1=, 2=   We only need this for prettyprinting.
       Zero-length records (i.e. unit) and singleton records are not
       considered as tuples. *)
    fun isProductType(LabelledRecord(recList as _::_::_)) =
    let
        fun isRec [] _ = true
         |  isRec ({name, ...} :: l) n =
                name = Int.toString n andalso isRec l (n+1)
    in
        isRec recList 1
    end
    | isProductType _ = false;


    (* Compose typeID maps.  If the first map returns a Bound id we apply the second otherwise
       just return the result of the first. *)
    fun composeMaps(m1, m2) n =
    let
        fun map2 (TypeId{idKind=Bound{ offset, ...}, ...}) = m2 offset

        |   map2 (id as TypeId{idKind=Free _, ...}) = id

        |   map2 (TypeId{idKind=TypeFn{arity, resType=equiv, ...}, access, description, ...}) =
            let
                fun copyId(TypeId{idKind=Free _, ...}) = NONE
                |   copyId id = SOME(map2 id)
                (* If it's a type function e.g. this was a "where type" we have to apply the
                   map to any type identifiers in the type. *)
                val copiedEquiv =
                    copyType(equiv, fn _ => NONE,
                        fn tcon => copyTypeConstr (tcon, copyId, fn y => y))
            in
                makeGeneralTypeFunction(arity, copiedEquiv, description, access)
            end
    in
        map2(m1 n)
    end

    (* Similar to generalise but with the bound variables replaced by non-unifiable
       free variables.  This is used in signature matching.  A polymorphic function
       in a structure will match a similar monomorphic function but not the other way
       round.  This currently still needs to copy the type because it includes
       a type identifier map. *)
    fun createNonGeneralisableInstance((atyp, templs), copyId) =
    let
        fun createFreeTypeVariables([], _) = []
        |   createFreeTypeVariables(TemplPlain{ equality } :: templs, n) =
                (* The variables are created as Generalisable which means they can only be matched by Generalisable type
                   variables in the structure. *)
                FreeTypeVar{ name = makeTypeVariableName(n, equality), equality=equality, level=Generalisable, uid=makeUniqueId() } ::
                        createFreeTypeVariables(templs, n+1)
        |   createFreeTypeVariables _ = raise InternalError "createNonGeneralisableInstance: template"
        
        val freeVars = createFreeTypeVariables(templs, 0)
        fun boundVarMap(TVIndex n) = SOME(List.nth(freeVars, n))
        fun copyATypeConstr tcon = copyTypeConstr(tcon, copyId, fn s => s)
        val copiedTarget = copyType (atyp, boundVarMap, copyATypeConstr)
    in
        SimpleInstance copiedTarget
    end

    (* Basic procedure to print a type structure. *)
    type printTypeEnv =
        { lookupType: string -> (typeConstrSet * (int->typeId) option) option,
          lookupStruct: string -> (structVals * (int->typeId) option) option}

    val emptyTypeEnv = { lookupType = fn _ => NONE, lookupStruct = fn _ => NONE }

    (* prints a block of items *)
    fun tDisp (t : instanceType, depth : FixedInt.int, typeVarName : typeVar -> string, env: printTypeEnv,
               sigMap: (int->typeId)option) =
    let
        (* prints a block of items *)
        fun dispP (t, tMap, depth) =
        let
            (* prints a block of items *)
            fun parenthesise depth (t, bvMap) =
            if depth <= 1
            then PrettyString "..."
            else
                PrettyBlock (0, false, [],
                    [
                        PrettyString "(",
                        dispP (t, bvMap, depth - 1),
                        PrettyString ")"
                    ]);
    
            (* prints a sequence of items *)
            fun prettyList [] _ _ _: pretty list = []

            |   prettyList [H] bvMap depth separator =
                let
                    val v = eventual H;
                in
                    if separator = "*" andalso
                        (isFunctionType v orelse isProductType v)
                    then (* Must bracket the expression *) [parenthesise depth (v, tMap)]
                    else [dispP (v, bvMap, depth)]
                end

            |   prettyList (H :: T) bvMap depth separator =
                if depth <= 0
                then [PrettyString "..."]
                else
                let
                    val v = eventual H;
                in
                    PrettyBlock (0, false, [],
                        [(if separator = "*" andalso
                           (isFunctionType v orelse isProductType v)
                        then (* Must bracket the expression *) parenthesise depth (v, tMap)
                        else dispP (v, bvMap, depth)),
                        PrettyBreak (if separator = "," then 0 else 1, 0),
                        PrettyString separator
                        ]) ::
                    PrettyBreak (1, 0) ::
                    prettyList T bvMap (depth - 1) separator
                end

            fun printRecord(recList, bvMap, isFrozen, depth) =
                let
                    (* The ordering on fields is designed to allow mixing of tuples and
                       records (e.g. #1).  It puts shorter names before longer so that
                       #11 comes after #2 and before #100.  For named records it does
                       not make for easy reading so we sort those alphabetically when
                       printing. *)
                    val sortedRecList =
                        Misc.quickSort(fn {name = a, ...} => fn {name = b, ...} => a <= b) recList
                in
                    PrettyBlock (2, false, [],
                        PrettyString "{" ::
                        (let
                            fun pRec [] _ = []
                              | pRec ({name, typeOf} :: T) depth =
                                if depth <= 0 then [PrettyString "..."]
                                else
                                    [
                                    PrettyBlock(0, false, [],
                                        [
                                            PrettyBlock(0, false, [],
                                                [
                                                PrettyString (name ^ ":"),
                                                PrettyBreak(1, 0),
                                                dispP(typeOf, bvMap, depth - 1)
                                                ] @
                                                (if null T then [] else [PrettyBreak (0, 0), PrettyString ","])
                                            )
                                        ]@
                                        (if null T then [] else PrettyBreak (1, 0) :: pRec T (depth-1))
                                        )
                                    ]
                        in
                            pRec sortedRecList (depth - 1)
                        end) @
                        [ PrettyString (if isFrozen then "}" else case recList of [] =>   "...}" | _  => ", ...}")]
                        )
                end


            val (typ, bvMap) = canonicalise(t, tMap) (* Find the real type structure *)
        in 
            case typ of
                TypeVar tyVar => (* Unset type variable. *) PrettyString (typeVarName tyVar)

            |   BoundTypeVar(s, _) => PrettyString s

            |   FreeTypeVar{name, ...} => PrettyString name

                (* Type construction. *)                    
            |   TypeConstruction {args, name, constr=typeConstructor as TypeConstrs{name=constrName, identifier=tcId, locations, ...}, ...} =>
                let
                    val constrName = (* Use the type constructor name unless we're had an error. *)
                        if isUndefinedTypeConstr typeConstructor then name else constrName
                    (* There are three possible cases: we may not find any type with the
                       name, we may look up the name and find the type or we may look up the
                       name and find a different type. *)
                    datatype isFound = NotFound | FoundMatch | FoundNotMatch
                    
                    (* If we're printing a value that refers to a type constructor we
                       want to print the correct amount of any structure prefix for the
                       current context. *)
                    fun findType (_, []) = NotFound
                    |   findType ({ lookupType, ... }, [typeName]) =
                        let
                            (* Test whether two type constructors are the same after mapping.
                               This is used to try to find the correct "path" to a type constructor
                               when printing.  sameTypeId does not compare type functions. *)
                            fun eqTypeConstrs(TypeConstrs{identifier=xId, ...}, xMap, TypeConstrs{identifier=yId, ...}, yMap) =
                            let
                                val mapIdX =
                                    case (xId, xMap) of (TypeId{idKind=Bound{ offset, ...}, ...}, SOME map) => map offset | _ => xId
                                val mapIdY =
                                    case (yId, yMap) of (TypeId{idKind=Bound{ offset, ...}, ...}, SOME map) => map offset | _ => yId
                            in
                                case (mapIdX, mapIdY) of
                                    (TypeId { idKind = TypeFn{uid = idX, ...}, ...}, TypeId { idKind = TypeFn{uid = idY, ...}, ...}) =>
                                        idX = idY
                                |   (mapIdX, mapIdY) => sameTypeId(mapIdX, mapIdY)
                            end
                        in
                            (* This must be the name of a type. *)
                            case lookupType typeName of
                                SOME (t, map) =>
                                    if eqTypeConstrs(typeConstructor, sigMap, tsConstr t, map)
                                    then FoundMatch else FoundNotMatch
                            |   NONE => NotFound
                        end
                    |   findType ({ lookupStruct, ... }, structName :: tail) =
                        (
                            (* This must be the name of a structure.  Does it contain our type? *)
                            case lookupStruct structName of
                                SOME(Struct { signat, ...}, map) =>
                                    let
                                        val Signatures { tab, typeIdMap, ...} = signat
                                        val Env { lookupType, lookupStruct, ...} = makeEnv tab
                                        val newMap =
                                            case map of
                                                SOME map => composeMaps(typeIdMap, map)
                                            |   NONE => typeIdMap
                                        fun subLookupType s =
                                            case lookupType s of NONE => NONE | SOME t => SOME(t, SOME newMap)
                                        fun subLookupStruct s =
                                            case lookupStruct s of NONE => NONE | SOME t => SOME(t, SOME newMap)
                                    in
                                        findType({lookupType=subLookupType, lookupStruct=subLookupStruct}, tail)
                                    end
                            |   NONE => NotFound
                        )

                    (* See if we have this type in the current environment or in some structure in
                       the current environment.  The name we have may be a full structure path. *)
                    fun nameToList ("", l) = (l, NotFound) (* Not there. *)
                    |   nameToList (s, l) = 
                        let
                            val { first, second } = splitString s
                            val currentList = second :: l
                        in
                            case findType(env, currentList) of
                                FoundMatch => (currentList, FoundMatch)
                            |   FoundNotMatch =>
                                (
                                    case nameToList(first, currentList) of
                                        result as (_, FoundMatch) => result
                                    |   (l, _) => (l, FoundNotMatch)
                                )
                            |   NotFound => nameToList(first, currentList)
                        end
                    (* Try the type constructor name first.  This is usually accurate.  If not
                       fall back to the type identifier.  This may be needed in rarer cases. *)
                    val names =
                        case nameToList(constrName, []) of
                            (names, FoundMatch) => names (* Found the type constructor name. *)
                        |   (names, f) =>
                            let
                                (* Try the type identifier name. *)
                                val TypeId { description = { name=idName, ...}, ...} =
                                    case (sigMap, tcId) of
                                    (SOME map, TypeId{idKind=Bound{offset, ...}, ...}) => map offset
                                |   (_, id) => id
                                (* Only add "?" if we actually found a type with the required
                                   name but it wasn't the right one.  This allows us to print a
                                   sensible result where the type has been shadowed but doesn't
                                   affect situations such as where we create a unique type name
                                   for a free type variable. *)
                                fun addQuery n =
                                    case f of FoundNotMatch => "?" :: n | _ => n
                            in
                                if idName = "" then addQuery names
                                else
                                case nameToList(idName, []) of
                                    (idNames, FoundMatch) => idNames
                                |   (_, _) => addQuery names (* Print it as "?.t".  This isn't ideal but will help in
                                                              situations where we have redefined "t". *)
                            end
                    val newName = String.concatWith "." names
                    (* Get the declaration position for the type constructor. *)
                    val constrContext =
                        if isUndefinedTypeConstr typeConstructor then []
                        else
                            (
                                case List.find(fn DeclaredAt _ => true | _ => false) locations of
                                    SOME(DeclaredAt loc) => [ContextLocation loc]
                                |   _ => []
                            )
                    val constructorEntry = 
                        PrettyBlock(0, false, constrContext, [PrettyString newName(*constrName*)])
                in
                    case args of
                        [] => constructorEntry
                    |   args as hd :: tl =>
                        let
                            val argVal = eventual hd;
                        in
                            PrettyBlock (0, false, [],
                            [
                                (* If we have just a single argument and it's just a type constructor
                                   or a construction we don't need to parenthesise it. *)
                                if null tl andalso not (isProductType argVal orelse isFunctionType argVal)
                                then dispP (argVal, bvMap, depth - 1)
                                else if depth <= 1
                                then PrettyString "..."
                                else PrettyBlock(0, false, [],
                                         [PrettyString "(", PrettyBreak (0, 0)]
                                         @ prettyList args bvMap (depth - 1) ","
                                         @ [PrettyBreak (0, 0), PrettyString ")"]
                                     ),
                                PrettyBreak(1, 0),
                                constructorEntry (* The constructor. *)
                            ])
                        end
                end
        
            |   FunctionType {arg, result} =>
                if depth <= 0
                then PrettyString "..."
                else (* print out in infix notation *)
                let
                    val evArg = eventual arg;
                in
                    PrettyBlock (0, false, [],
                        [
                        (* If the argument is a function it must be printed as (a-> b)->.. *)
                        if isFunctionType evArg 
                        then parenthesise depth (evArg, bvMap)
                        else dispP (evArg, bvMap, depth - 1),
                        PrettyBreak(1, 2),
                        PrettyString "->",
                        PrettyBreak (1, 2),
                        dispP (result, bvMap, depth - 1)
                        ])
                end
                     
            |   LabelledRecord recList =>
                if depth <= 0
                then PrettyString "..."
                else if isProductType typ
                then (* Print as a product *)
                    PrettyBlock (0, false, [], (* Print them as t1 * t2 * t3 .... *)
                        prettyList (List.map (fn {typeOf, ...} => typeOf) recList) bvMap depth "*")
                else (* Print as a record *) printRecord(recList, bvMap, true, depth)

            |   FlexibleRecordVar{recList=ref recl, fullList=ref full, ...} =>
                let
                    val {frozen, ...} = followRefChainToEnd full
                    val recList = followRefChainToEnd recl
                in
                    printRecord(recList, bvMap, frozen, depth)
                end

            |   OverloadSetVar(ref chain) =>
                (* This typically arises when printing error messages in the second pass because
                   the third pass will select a single type e.g. int where possible.  To
                   simplify the messages select a single type if possible. *)
                (
                    case followRefChainToEnd chain of
                        [] => PrettyString "no type"
                    |   tconslist => 
                    (
                        case preferredOverload tconslist of
                            SOME(tcons as TypeConstrs {name,...})  => dispP(mkTypeConstruction (name, tcons,[], []), bvMap, depth)
                        |   NONE =>
                          (* Just print the type constructors separated by / *)
                        let
                            fun constrLocation(TypeConstrs{locations, ...}) =
                            (
                                case List.find(fn DeclaredAt _ => true | _ => false) locations of
                                    SOME(DeclaredAt loc) => [ContextLocation loc]
                                |   _ => []
                            )
                            (* Type constructor with context. *)
                            fun tconsItem(tcons as TypeConstrs {name,...}) =
                                PrettyBlock(0, false, constrLocation tcons, [PrettyString name])

                            fun printTCcons [] = []
                              | printTCcons [tcons] = [tconsItem tcons]
                              | printTCcons (tcons::rest) =
                                    tconsItem tcons :: PrettyBreak (0, 0) ::
                                    PrettyString "/" :: printTCcons rest
                        in
                            PrettyBlock (0, false, [], printTCcons tconslist)
                        end
                    )
                )

            |   BadType => PrettyString "bad"
        end (* dispP *)
        
        val (t, map) = getInstance t
    in
        dispP (t, map, depth)
    end (* tDisp *);

  (* Generate unique type-variable names. *)

    fun varNameSequence () : typeVar -> string = 
    (* We need to ensure that every distinct type variable has a distinct name.
       Each new type variable is given a name starting at "'a" and going on
       through the alphabet. *)
    let
        datatype names = Names of {name: string, entry: typeVar}
        val nameNum    = ref ~1
        val gNameList  = ref [] (* List of names *)
    in
        (* If the type is already there return the name we have given it
           otherwise make a new name and put it in the list. *)
        fn (var as TypeVariable{equality=ref eq, ...}) =>
            case List.find (fn (Names {entry,...}) => sameTv (entry, var)) (!gNameList) of
                NONE =>  (* Not on the list - make a new name *)
                let
                    fun name num = (if num >= 26 then name (num div 26 - 1) else "")
                          ^ String.str (Char.chr (num mod 26 + Char.ord #"a"))
                    val () = nameNum := !nameNum + 1
                    val n = (if followRefChainToEnd eq then "''" else "'") ^ name(!nameNum)
                    (* Should explicit type variables be distinguished? *)
                in
                    gNameList := Names{name=n, entry=var} :: !gNameList;
                    n
                end
           |    SOME (Names {name,...}) => name
    end (* varNameSequence *)

    (* Print a type (as a block of items) *)
    fun displayWithMap (t : instanceType, depth : FixedInt.int, env, sigMap) =
        tDisp (t, depth, varNameSequence (), env, sigMap)
    and display (t : instanceType, depth : FixedInt.int, env) =
        tDisp (t, depth, varNameSequence (), env, NONE)

    (* Print out zero, one or more type variables (unblocked) *)
    fun printTypeVariables _ ([], _, _) = [] (* No type vars i.e. monotype *)
 
    |   printTypeVariables pt ([oneVar], depth, typeV) = (* Single type var. *)
        [
            pt (oneVar, depth, typeV), 
            PrettyBreak (1, 0)
        ]

    |   printTypeVariables pt (vars, depth, typeV) =
        (* Must parenthesise them. *)
        if depth <= 1 then [PrettyString "..."]
        else
        [
            PrettyBlock(0, false, [],
                PrettyString "(" ::
                PrettyBreak(0, 0) ::
                (let
                    fun pVars vars depth: pretty list = 
                        if depth <= 0 then [PrettyString "..."]
                        else if null vars then []
                        else
                        [
                            pt (hd vars, depth, typeV),
                            PrettyBreak (0, 0)
                        ] @
                        (if null (tl vars) then []
                         else PrettyString "," :: PrettyBreak (1, 0) :: pVars (tl vars) (depth - 1)
                        )
                in
                    pVars vars depth
                end) @ [PrettyString ")"]
            ),
            PrettyBreak (1, 0)
        ]

    val printTypeVars =
        printTypeVariables (fn (var, depth, typeV) => tDisp (SimpleInstance var, depth, typeV, emptyTypeEnv, NONE))

    (* When we have finished processing a list of patterns we need to check
       that the record is now frozen. *)
    fun recordNotFrozen (TypeVar(TypeVariable{link=ref l, ...})) =
            (case followRefChainToEnd l of NONE => false | SOME t => recordNotFrozen(instanceToType t))
    |   recordNotFrozen (FlexibleRecordVar{fullList=ref full, ...}) = not(#frozen(followRefChainToEnd full))
    |   recordNotFrozen _ = false (* record or type alias *);

    (* If this is a nullary constructor it returns NONE otherwise it returns the argument type. *)
(*    fun exceptionArgType (FunctionType{arg, ...}) = SOME arg
    |   exceptionArgType _ = NONE*)

    (* Create an argument map for a type function. *)
    fun createTypeFnArgumentMap(arguments, argTyMap) =
    let
        fun createVar arg = TypeVar(makeTv {value=SOME(Instance(arg, argTyMap)), level=Generalisable, equality=false})
        val tvars = List.map createVar arguments
        val tvMap =
            case tvars of
                [] => (fn _ => NONE)
            |   [a] => (fn TVIndex 0 => SOME a | _ => NONE)
            |   [a, b] => (fn TVIndex 0 => SOME a |  TVIndex 1 => SOME b | _ => NONE)
            |   [a, b, c] => (fn TVIndex 0 => SOME a |  TVIndex 1 => SOME b |  TVIndex 2 => SOME c | _ => NONE)
            |   l =>
                let
                    val len = List.length l
                    val v = Vector.fromList l
                in
                    fn TVIndex n => if n < len then SOME(Vector.sub(v, n)) else NONE
                end
    in
        tvMap
    end

    (* Look for the occurrence of locally declared datatypes in the type of a value.
       This works by looking to see if we have a reference to a local datatype whose
       address has not yet been set.  If it hasn't that means it's either local to
       a function or refers to a datatype that occurs textually later.  That can
       happen if we have a ref that we assign later.  See Tests/Fail/Test064-69. *)
    fun checkForEscapingDatatypes(ty: types, errorFn: string->unit) : unit =
    let
        fun checkTypes (typ, _) (ok: bool) : bool =
        case typ of
            TypeConstruction {constr=TypeConstrs
                {identifier=TypeId{access=Local{addr, ...}, ...}, name, ...}, ...} =>
                if ok andalso !addr < 0
                then 
                (
                    errorFn("Type of expression contains local datatype (" ^ name
                            ^") outside its definition.");
                    false
                )
                else ok (* Could we have a "selected" entry with a local datatype? *)

        |   TypeConstruction {constr=TypeConstrs
                {identifier = TypeId{idKind = TypeFn{resType, ...}, ...},...}, ...} =>
                (* May be an alias for a type that contains a local datatype.
                   Don't return the actual arguments here; we've already checked them and
                   we don't want to expand the type any further. See Tests/Succeed/207. *)
                foldType checkTypes (resType, fn _ => SOME unitType) ok

        |   _ => ok
    in
        foldType checkTypes (ty, fn _ => NONE) true;
        ()
    end

    (* If we have a type construction which is an alias for another type
       we construct the alias by first instantiating all the type variables
       and then copying the type.
       This should be replaced by createTypeFnArgumentMap. *)
    fun makeEquivalent (TypeConstrs {identifier=TypeId{idKind=TypeFn{resType=typeResult, ...}, ...},...}, args) =
        let
            (* Map the arguments to type variables containing the arguments.  That ensures
               that if the arguments contain bound type variables we won't get a bound variable
               lookup loop. *)
            fun mapToTv t = TypeVar (makeTv {value=SOME(SimpleInstance t), level=Generalisable, equality=false})
            val vars = Vector.fromList(List.map mapToTv args)
            fun getArg(TVIndex n) = Vector.sub(vars, n)
        in
            copyType (typeResult, SOME o getArg, fn t => t (*copyTCons*))
        end

    |   makeEquivalent _ = raise InternalError "makeEquivalent: Not a type function"

    datatype matchResult =
        SimpleError of types * types * string
    |   TypeConstructorError of types * types * typeConstrs * typeConstrs

    (* Compute the common level for two type variables. *)
    fun minLevel (NotGeneralisable v1, NotGeneralisable v2) = NotGeneralisable(Int.min (v1, v2))
    |   minLevel(v1 as NotGeneralisable _, _) = v1
    |   minLevel(_, v2 as NotGeneralisable _) = v2
    |   minLevel(Generalisable, Generalisable) = Generalisable

    (* Will this type match an equality type?  Unlike computeEquality this takes an instance but bound
       variables must be bound to types.  It sets equality on type variables. *)
    fun equalityForInstance(typ, bvMap): bool =
    let
        fun eqForList ([], _) = true

        |   eqForList (x::xs, map) =
            if equalityForInstance (x, map)
            then eqForList (xs, map)
            else false
    in
        case canonicalise(typ, bvMap) of
            (TypeVar(TypeVariable{equality, ...}), _) =>
            let
                val (eqRef, isEq) = followRefChainToRef equality
                val () = if isEq then () else eqRef := ChainEnd true
                (* Don't actually have to link it because it can never change now. *)
            in
                true
            end

            (* Check that equality is permitted and if it is set the equality property so that
               any new fields will have to be equality types.  If equality is already true we don't
               actually need to do anything here. *)
        |   (FlexibleRecordVar{recList=ref recl, equality, ...}, tmap) =>
            let
                val (eqVar, isEq) = followRefChainToRef equality
                val recList = followRefChainToEnd recl
                val eqStatus = eqForList (List.map #typeOf recList, tmap)
            in
                if eqStatus andalso not isEq
                then eqVar := ChainEnd true
                else ();
                eqStatus
            end
        
        |   (FreeTypeVar{equality, ...}, _) => equality

        |   (FunctionType _, _) => false  (* No equality on function types! *)

        |   (TypeConstruction {constr=TypeConstrs
                {identifier=TypeId{idKind = TypeFn{equality=ref equal, ...}, ...},...}, args, ...}, tmap) =>
                (* Type function. *)
            (
                case equal of
                    TypeFnEqNever => false
                |   TypeFnEq bv =>
                    let
                        (* Only some of the arguments may be necessary for equality.  See comment in makeGeneralTypeFunction. *)
                        fun filterArgs(arg, (n, l)) = (n+1, if BoolVector.sub(bv, n) then arg::l else l)
                        val (_, filtered) = List.foldl filterArgs (0, []) args
                    in
                        eqForList (filtered, tmap)
                    end
            )

        |   (TypeConstruction {constr as TypeConstrs{identifier, ...}, args, ...}, tMap) =>
            (* ref - Equality is permitted on refs of all types *)
            (* The Definition of Standard ML says that ref is the ONLY type
               constructor which is treated in this way.  The standard basis
               library says that other mutable types such as array should
               also work this way. *)
            if isUndefinedTypeConstr constr orelse isPointerEqType constr
            then true

            (* Others apart from ref and real *)
            else isEquality identifier andalso eqForList (args, tMap)

        |   (LabelledRecord recList, tMap) => (* Record equality if all subtypes *)
                eqForList (map #typeOf recList, tMap)

        |   (OverloadSetVar rchain, _) =>
            (* Reduce the list to those that admit equality if possible. *)
            let
                val (final, typeset) = followRefChainToRef rchain
                fun tcEquality(TypeConstrs{identifier, ...}) = isEquality identifier
            in
                case List.filter tcEquality typeset of
                    [] => false (* And leave it unchanged so it can be reported as an error. *)
                |   newset => ( final := ChainEnd newset; true )
            end

        |   (BadType, _) => true (* To prevent further errors. *)

        |   (BoundTypeVar _, _) => (* This should have been looked up in canonicalise. *)
                raise InternalError "computeEqualityForInstance: Bound"

    end

    (* Type matching algorithm for both unification and signature matching. *)
    (* The mapping has now been moved out of here.  Instead when signature matching the
       target signature is copied before this is called which means that this
       process is now symmetric.  There may be some redundant tests left in here. *)
    fun unifyTypes(instanceA: instanceType, instanceB: instanceType) : matchResult option =
    let
        (* Get the result in here.  This isn't very ML-like but it greatly
           simplifies converting the code. *)
        val matchResult: matchResult option ref = ref NONE
        fun matchError error = (* Only report one error. *)
            case matchResult of ref (SOME _) => () | r => r := SOME error
        fun cantMatch(alpha, beta, text) = matchError(SimpleError(alpha, beta, text))

        (* Sets a type variable to a value. - Checks that the type variable
           we are assigning does not occur in the expression we are about to
           assign to it. Such cases can occur if we have infinitely-typed
           expressions such as fun a. a::a where a has type 'a list list ...
           Also propagates the level information of the type variable.
           Now also deals with flexible records. *)
        fun assign (var as TypeVariable{link=varLink, level=ref lvl, equality=ref eq}, t, bvMap, aType, bType) =
        let
            val (varLinkVar, _) = followRefChainToRef varLink
            val varLevelVal = followRefChainToEnd lvl
            val varEqVal = followRefChainToEnd eq
            (* Mapped over the type to be assigned. *)
            (* Returns "false" if it is safe to make the assignment. Sorts out
             imperative type variables and propagates level information.
             N.B. It does not propagate equality status. The reason is that
             if we are unifying ''a with 'b ref, the 'b does NOT become
             an equality type var. In all other cases it would. *)
            fun occursCheckFails _ true = true

            |   occursCheckFails args false =
                let
                    val (t, map) = canonicalise args
                in
                    case t of
                        TypeVar(TypeVariable{link=tLink, level=tLevel, equality=tEq})=>
                        let
                            val (tLinkVar, _) = followRefChainToRef tLink
                            val (tLevelVar, tLevelVal) = followRefChainToRef tLevel
                            val (tEqVar, tEqVal) = followRefChainToRef tEq
                            (* The level is the minimum of the two, and if we are unifying with
                               an equality type variable we must make this into one. *)
                            val minLev = minLevel(varLevelVal, tLevelVal)
                        in
                            (* If the links are the same we have an occurs check failure. *)
                            if varLinkVar = tLinkVar
                            then true
                            else
                            (
                                (* We have to link equality and level but only on the end.  Don't link them. *)
                                tEqVar := ChainEnd(varEqVal orelse tEqVal);
                                tLevelVar := ChainEnd minLev;
                                false (* OK. *)
                            )
                        end

                    |   TypeConstruction {args, constr=TypeConstrs{identifier = TypeId {idKind = TypeFn{usedTvs, ...}, ...}, ...}, ...} =>
                        (* Type function: Only process arguments that are actually used in the type expression. *)
                        let
                            fun checkArgs([], _, b) = b
                            |   checkArgs(arg::args, n, b) =
                                    checkArgs(args, n+1, 
                                        if BoolVector.sub(usedTvs, n) then occursCheckFails (arg, map) b else b)  
                        in
                            checkArgs(args, 0, false)
                        end

                    |   TypeConstruction {args, ...} =>
                            (* Datatypes. *)
                            List.foldl (fn (t, v) => occursCheckFails(t, map) v) false args

                    |   FunctionType {arg, result} =>
                            occursCheckFails(arg, map) false orelse occursCheckFails (result, map) false

                    |   LabelledRecord recList =>
                            List.foldl (fn ({ typeOf, ... }, v) => occursCheckFails (typeOf, map) v) false recList

                    |   FlexibleRecordVar{recList=ref recl, equality, level, ...} =>
                        let
                            (* Have to set the properties that will apply to new fields. *)
                            val (eqVar, equality) = followRefChainToRef equality
                            val (levelVar, level) = followRefChainToRef level
                            val minLev = minLevel(varLevelVal, level)
                            val () = eqVar := ChainEnd(equality orelse varEqVal)
                            val () = levelVar := ChainEnd minLev
                        in
                            List.foldl (fn ({typeOf, ... }, v) => occursCheckFails (typeOf, map) v) false (followRefChainToEnd recl)
                        end

                    |   _ => false

                end
                
            local
                (* Generally if the type we're assigning contains the type variable that
                   is an "occurs check" failure.  If we have a type function it is possible
                   that what looks like an occurs check failure is actually valid; either
                   because the type variable is not actually used or because the function
                   boils down to the variable itself.  We need to unwrap it sufficiently
                   that any apparent occurs check has been eliminated.  We must not assign
                   the original type to the type variable because that would create a loop. *)
                fun followVarsAndTypeFunctions(t, bvMap) =
                case canonicalise(t, bvMap) of
                    inst as (TypeConstruction{constr=TypeConstrs{identifier = TypeId {idKind = TypeFn{resType, ...}, ...}, ...}, args, ...}, newBvMap) =>
                        let
                            fun occurs(TypeVar tvar, _) b = b orelse sameTv(tvar, var) | occurs _ b = b
                        in
                            if List.foldl (fn (t, b) => foldType occurs (t, newBvMap) b) false args
                            then
                            let
                                val argMap = createTypeFnArgumentMap(args, newBvMap)
                            in
                                followVarsAndTypeFunctions(resType, argMap)
                            end 
                            else inst
                        end
                |   inst => inst
            in
                val finalInstance as (finalType, _) = followVarsAndTypeFunctions(t, bvMap)
            end
  
            (* We may actually have the same type variable after any type abbreviations
                have been followed. *)
            val reallyTheSame =
                case finalType of
                    TypeVar tv => sameTv (tv, var)
                |   _ => false

        in (* start of "assign" *)
            if reallyTheSame
            then () (* Don't apply the occurs check or check for non-unifiable. *)

            else if occursCheckFails finalInstance false
            then cantMatch (aType, bType, "(Type variable to be unified occurs in type)")

            (* If we're matching a structure to a signature we can't make a non-generalisable type variable polymorphic. *)
            else if (case (varLevelVal, t) of (NotGeneralisable _, FreeTypeVar{level=Generalisable, ...}) => true | _ => false)
            then cantMatch (aType, bType, "(Type variable is free at top level)")

            (* A type variable with a wider scope can't be unified with a more local explicit type variable.  *)
            else if (case (varLevelVal, t) of
                            (NotGeneralisable tvLevel, FreeTypeVar{level=NotGeneralisable level, ...}) => tvLevel < level | _ => false)
            then cantMatch (aType, bType, "(Type variable is free in surrounding scope)")

            else
            let (* Occurs check succeeded. *)
            in
                (* If we are unifying a type with an equality type variable
                   we must ensure that equality is allowed for that type. This
                   will turn most type variables into equality type vars. *)
                if varEqVal andalso not(equalityForInstance finalInstance)
                then cantMatch (aType, bType, "(Requires equality type)")
                else ();
                (* If we have had an error don't make the assignment.  At the very least
                   it could prevent us producing useful error information and it could
                   also result in unnecessary consequential errors. *)
                case !matchResult of
                    NONE => if reallyTheSame then () else varLinkVar := ChainEnd(SOME(Instance finalInstance))
                |   SOME _ => ()
            end
        end (* assign *)

        fun match (argsA, argsB, typeA, typeB) =
        let (* Check two records/tuples and return the combined type. *)
            (* First find see if typeA and typeB are unified to anything
               already, and get the end of a list of "flexibles".  *)

            val (tA, bvMapA) = canonicalise argsA
            and (tB, bvMapB) = canonicalise argsB

        in (* start of "match" *)
            case (tA, tB) of
                (* If either of these was an undefined type constructor don't try to match. 
                           TODO: There are further tests below for this which are now redundant. *)
                (BadType, _) => () (* If either is an error don't try to match *)
            |   (_, BadType) => ()

            |   (TypeVar (TypeVariable{link=linkA, level=levelA, equality=eqA}),
                 TypeVar (TypeVariable{link=linkB, level=levelB, equality=eqB})) => (* Unbound type variables. *)
                    let
                        (* Link the type variables together. *)
                        val (linkVarA, linkValA) = followRefChainToRef linkA
                        and (levelVarA, levelValA) = followRefChainToRef levelA
                        and (eqVarA, eqValA) = followRefChainToRef eqA
                        and (linkVarB, _) = followRefChainToRef linkB
                        and (levelVarB, levelValB) = followRefChainToRef levelB
                        and (eqVarB, eqValB) = followRefChainToRef eqB
                    in
                        if linkVarA = linkVarB then () else (linkVarA := ChainRef linkVarB; linkVarB := ChainEnd linkValA);
                        if eqVarA = eqVarB then () else (eqVarA := ChainRef eqVarB; eqVarB := ChainEnd(eqValA orelse eqValB));
                        if levelVarA = levelVarB then () else (levelVarA := ChainRef levelVarB; levelVarB := ChainEnd(minLevel(levelValA, levelValB)))
                    end

            |   (TypeVar typeAVar, _) => assign (typeAVar, tB, bvMapB, typeA, typeB)

            |   (_, TypeVar typeBVar) => (* and typeA is not *) assign (typeBVar, tA, bvMapA, typeA, tB) (* set typeB to typeA *)

            |   (BoundTypeVar _, _) => raise InternalError "match: bound variable in type"

            |   (_, BoundTypeVar _) => raise InternalError "match: bound variable in type"

                (* Type functions (type abbreviations).  We need to deal with these first because unwrapping
                   them could allow a valid match.  Try to avoid unwrapping unnecessarily. *)
            |   (TypeConstruction({constr =
                    TypeConstrs {identifier = TypeId{idKind = TypeFn{uid=uuidA, typeFunCount=tfCA, usedTvs, resType=rtA, ...}, ...},...}, args=tAargs, ...}), 
                 TypeConstruction ({constr =
                    TypeConstrs {identifier = TypeId{idKind = TypeFn{uid=uuidB, typeFunCount=tfCB, resType=rtB, ...}, ...},...}, args=tBargs, ...})) =>
                    if uuidA = uuidB
                    then (* They are the same ID: simply match the used type variables. *)
                    let
                        fun matchArgs(a, b, n) = (if BoolVector.sub(usedTvs, n) then match((a, bvMapA), (b, bvMapB), typeA, typeB) else (); n+1)
                        val _ = ListPair.foldl matchArgs 0 (tAargs, tBargs);
                    in
                        ()
                    end
                    else if tfCA >= tfCB
                        (* They are different: unwrap the one with the larger count.  It may refer to
                           the other one.  If they're equal we'll come back and unwrap the other later. *)
                    then match ((rtA, createTypeFnArgumentMap (tAargs, bvMapA)), (tB, bvMapB), typeA, typeB)
                    else match ((tA, bvMapA), (rtB, createTypeFnArgumentMap (tBargs, bvMapB)), typeA, typeB)

            |   (TypeConstruction({constr =
                    TypeConstrs {identifier = TypeId{idKind = TypeFn{resType, ...}, ...},...}, args=tAargs, ...}), _) =>
                    (* We need to unwrap A. *)
                        match ((resType, createTypeFnArgumentMap(tAargs, bvMapA)), (tB, bvMapB), typeA, typeB)

            |   (_,  TypeConstruction ({constr =
                        TypeConstrs {identifier = TypeId{idKind = TypeFn{resType, ...}, ...},...}, args=tBargs, ...})) =>
                    match ((tA, bvMapA), (resType, createTypeFnArgumentMap(tBargs, bvMapB)), typeA, typeB)

            |   (TypeConstruction({constr = tACons as TypeConstrs{identifier=tcIdA, ...}, args=tAargs, ...}), 
                 TypeConstruction ({constr = tBCons as TypeConstrs{identifier=tcIdB, ...}, args=tBargs, ...})) =>
                (
                    if isUndefinedTypeConstr tACons orelse isUndefinedTypeConstr tBCons
                    then ()
                    else if sameTypeId (tcIdA, tcIdB)
                    then
                    let (* Same type constructor - do the arguments match? *)
                        fun matchLists []      []    = ()
                          | matchLists (a::al) (b::bl) =
                          (  
                            match ((a, bvMapA), (b, bvMapB), typeA, typeB);
                            matchLists al bl
                          )
                          | matchLists _ _ = (* This should only happen as a result of
                                                a different error. *)
                                cantMatch (typeA, typeB, "(Different numbers of arguments)")
                    in
                            matchLists tAargs tBargs
                    end

                    (* When we have different type constructors, especially two with the same name,
                       we try to produce more information. *)
                    else matchError(TypeConstructorError(tA, tB, tACons, tBCons))
                )

            |   (OverloadSetVar chain, TypeConstruction {constr=tBCons, args=tBargs, ...}) =>
                (* The candidate is an overloaded type and the target is a type
                   construction. *)
                let
                    val (typeset, currentSet) = followRefChainToRef chain
                in
                    (* See if the target type is among those in the overload set. *)
                    if null tBargs (* Must be a nullary type constructor. *)
                            andalso isInSet(tBCons, currentSet)
                    then typeset := ChainEnd[tBCons] (* ok. *)
                        (* Overload sets arise primarily with literals such as "1" and it's
                           most likely that the error is a mismatch between int and another
                           type rather than that the user assumed that the literal was
                           overloaded on a type it actually wasn't. *)
                    else
                    (
                        case preferredOverload currentSet of
                            NONE => cantMatch (tA, tB, "(Different type constructors)")
                        |   SOME(prefType as TypeConstrs {name,...}) =>
                            matchError(
                                TypeConstructorError(
                                    mkTypeConstruction (name, prefType,[], []),
                                    tB, prefType, tBCons))
                    )
                end

            |   (TypeConstruction {constr=tACons, args=tAargs, ...}, OverloadSetVar chain) =>
                let
                    val (typeset, currentSet) = followRefChainToRef chain
                in
                    (* We should never find an overload set as the target for a signature
                       match but it is perfectly possible for tB to be an overload set
                       when unifying two types.  *)
                    if null tAargs andalso isInSet(tACons, currentSet)
                    then typeset := ChainEnd[tACons] (* ok. *)
                    else
                    (
                        case preferredOverload currentSet of
                            NONE => cantMatch (tA, tB, "(Different type constructors)")
                        |   SOME(prefType as TypeConstrs {name,...}) =>
                            matchError(
                                TypeConstructorError(
                                    tA, mkTypeConstruction (name, prefType,[], []),
                                    tACons, prefType))
                    )
                end

            |   (OverloadSetVar chainA, OverloadSetVar chainB) =>
                let
                    val (finalA, currentSetA) = followRefChainToRef chainA
                    val (finalB, currentSetB) = followRefChainToRef chainB
                in
                    if finalA = finalB (* Same reference? *)
                    then () (* If they're already linked don't do anything. *)
                    else
                    let
                        (* The lists aren't ordered so we just have to go
                           through by hand. *)
                        fun intersect(_, []) = []
                        |   intersect(a, H::T) =
                                if isInSet(H, a) then H::intersect(a, T) else intersect(a, T)
                        val newSet = intersect(currentSetA, currentSetB)
                    in
                        case newSet of
                            [] => cantMatch (typeA, typeB, "(Incompatible overloadings)")
                        |   _ =>
                        (
                            (* These have to be linked so that any subsequent unification affects all. *)
                            finalB := ChainEnd newSet; (* Either way round *)
                            finalA := ChainRef finalB
                        )
                    end
                end

            |   (TypeConstruction _, _) =>
                    cantMatch (tA, tB, "(Incompatible types)")

            |   (_, TypeConstruction _) => (* and typeA is not. *)
                    cantMatch (tB, tA, "(Incompatible types)")

            |   (FunctionType {arg=typAarg, result=typAres, ...},
                 FunctionType {arg=typBarg, result=typBres, ...}) =>
                ( (* must be unifiable functions *)
                    (* In principle it doesn't matter whether we unify arguments or
                       results first but it could affect the error messages.  Is this
                       the best way to do it? *)
                    match ((typAarg, bvMapA), (typBarg, bvMapB), typeA, typeB);
                    match ((typAres, bvMapA), (typBres, bvMapB), typeA, typeB)
                )

                (* Explicit type variables.  They can only match themselves. *)
            |   (FreeTypeVar{uid=uidA, ...}, FreeTypeVar{uid=uidB, ...}) =>
                    if uidA = uidB then ()
                    else cantMatch (typeA, typeB, "(Different explicit type variables)")

            |   (FreeTypeVar _, _) =>
                    cantMatch (typeA, typeB, "(Type contains an explicit type variable)")

            |   (_, FreeTypeVar _) =>
                    cantMatch (typeA, typeB, "(Type contains an explicit type variable)")

            |   (LabelledRecord recA, LabelledRecord recB) =>
                    (* Unify the records, but discard the result. *)
                    (matchRecords (recA, tA, bvMapA, true, false, recB, tB, bvMapB, true, false); ())

                (* eventual returns LabelledRecord if the FlexibleRecordVar is now frozen *)
            |   (LabelledRecord recA, FlexibleRecordVar{recList, equality=ref eqVar, fullList, ...}) =>
                let
                    val equality = followRefChainToEnd eqVar
                    val (reclRef, recB) = followRefChainToRef recList
                    val (fullRef, _) = followRefChainToRef fullList
                    val finalList = matchRecords (recA, tA, bvMapA, true, false, recB, tB, bvMapB, false, equality)
                    val () = reclRef := ChainEnd finalList
                    val () = fullRef := ChainEnd {names=map #name finalList, frozen=true}
                in
                    ()
                end

            |   (FlexibleRecordVar{recList, equality=ref eqVar, fullList, ...}, LabelledRecord recB) =>
                let
                    val equality = followRefChainToEnd eqVar
                    val (reclRef, recA) = followRefChainToRef recList
                    val (fullRef, _) = followRefChainToRef fullList
                    val finalList = matchRecords (recA, tA, bvMapA, false, equality, recB, tB, bvMapB, true, false)
                    val () = reclRef := ChainEnd finalList
                    val () = fullRef := ChainEnd {names=map #name finalList, frozen=true}
                in
                    ()
                end

            |   (FlexibleRecordVar{recList=recListA, equality=eqA, level=levA, fullList=fullListA, ...},
                 FlexibleRecordVar{recList=recListB, equality=eqB, level=levB, fullList=fullListB, ...}) =>
                let
                    val (reclRefA, recA) = followRefChainToRef recListA
                    val (reclRefB, recB) = followRefChainToRef recListB
                    val (eqVarA, eqA) = followRefChainToRef eqA
                    val (eqVarB, eqB) = followRefChainToRef eqB
                    val (levVarA, levelA) = followRefChainToRef levA
                    val (levVarB, levelB) = followRefChainToRef levB
                    val (fullRefA, _) = followRefChainToRef fullListA
                    val (fullRefB, _) = followRefChainToRef fullListB
                    val minLev =
                        case (levelA, levelB) of
                            (NotGeneralisable v1, NotGeneralisable v2) =>
                                NotGeneralisable(Int.min (v1, v2))
                        |   (v1 as NotGeneralisable _, _) => v1
                        |   (_, v2 as NotGeneralisable _) => v2
                        |   (Generalisable, Generalisable) => Generalisable
                in
                    if reclRefA = reclRefB (* Do we have the same record list? i.e. unifying with itself? *)
                    then ()
                    else
                    let
                        val finalList = matchRecords (recA, tA, bvMapA, false, eqA, recB, tB, bvMapB, false, eqB)
                        val fullNames = map #name finalList
                        val () = reclRefA := ChainEnd finalList
                        val () = reclRefB := ChainRef reclRefA
                        (* Even if they're different record instances they could already share generic instances. *)
                        val () =
                            if fullRefA = fullRefB
                            then ()
                            else (fullRefA := ChainEnd{names=fullNames, frozen=false}; fullRefB := ChainRef fullRefA)
                    in
                        ()
                    end;
                    (* Link the properties and set to the common value. *)
                    if eqVarA = eqVarB
                    then ()
                    else (eqVarA := ChainEnd(eqA orelse eqB); eqVarB := ChainRef eqVarA);
                    if levVarA = levVarB
                    then ()
                    else (levVarA := ChainEnd minLev; levVarB := ChainRef levVarA)
                end

            |   _ => cantMatch (tA, tB, "(Incompatible types)")

        end (* match *)

        and matchRecords(recA, typA, bvMapA, frozenA, equalityA, recB, typB, bvMapB, frozenB, equalityB) =
        let
            (* Match up the fields.  Return any fields that don't correspond along with the unified list.
               Unmatched fields are possible if the other record is flexible.
               If fields are discarded but the other record is an equality type they must nonetheless
               be checked for equality. *)
            fun matchFields([], [], combined, unmatchedA, unmatchedB) = (combined, unmatchedA, unmatchedB)

            |   matchFields(fieldA :: fieldsA, [], combined, unmatchedA, unmatchedB) =
                    matchFields(fieldsA, [], addField(fieldA, bvMapA) :: combined, fieldA :: unmatchedA, unmatchedB)

            |   matchFields([], fieldB :: fieldsB, combined, unmatchedA, unmatchedB) =
                    matchFields([], fieldsB, addField(fieldB, bvMapB) :: combined, unmatchedA, fieldB :: unmatchedB)

            |   matchFields(aList as (fieldA as {name=nameA, typeOf=typeA}) :: fieldsA,
                            bList as (fieldB as {name=nameB, typeOf=typeB}) :: fieldsB, combined, unmatchedA, unmatchedB) =
                (
                    case compareLabels (nameA, nameB) of
                        EQUAL =>
                        (
                            match ((typeA, bvMapA), (typeB, bvMapB), typA, typB);
                            matchFields(fieldsA, fieldsB, addField(fieldA, bvMapA) :: combined, unmatchedA, unmatchedB)
                        )
                    |   LESS (* aName < bName *) =>
                            matchFields(fieldsA, bList, addField(fieldA, bvMapA) :: combined, fieldA :: unmatchedA, unmatchedB)
                    |   GREATER (* aName > bName *) =>
                            matchFields(aList, fieldsB, addField(fieldB, bvMapB) :: combined, unmatchedA, fieldB :: unmatchedB)
                )

            (* When adding fields we need to use a type variable to include the maps. *)
            and addField({name, typeOf}, bvMap) =
                {name=name, typeOf=TypeVar(makeTv {value=SOME(Instance(typeOf, bvMap)), level=Generalisable, equality=false })}

            val (combinedList, unmatchedA, unmatchedB) = matchFields(recA, recB, [], [], [])
            val combinedList = List.rev combinedList (* Get the list back into canonical order *)
            (* Report on unmatched fields.  Try to be helpful here.  It could be that a field has
               been misspelled in one field in which case we will have two mismatches. *)
            val () =
                case (frozenA, frozenB, unmatchedA, unmatchedB) of
                    (true, true, {name=nameA, ...} :: _, {name=nameB, ...} :: _) =>
                        cantMatch (typA, typB, "(Field " ^ nameA ^ " present in one type but " ^ nameB ^ " in the other)")
                |   (_, true, {name=nameA, ...} :: _, _) =>
                        (* If B is frozen we cannot have any fields that aren't present in it. *)
                        cantMatch (typA, typB, "(Field " ^ nameA ^ " present in one type but not in the other)")
                |   (true, _, _, {name=nameB, ...} :: _) =>
                        (* If A is frozen we cannot have any fields that aren't present in it. *)
                        cantMatch (typA, typB, "(Field " ^ nameB ^ " present in one type but not in the other)")
                    (* Otherwise we either have a successful match or any excess fields are in the other record
                       in a flexible record. *)
                |   (_, _, unmatchedA, unmatchedB) =>
                    let
                        fun checkEquality bvMap {name, typeOf} =
                            if equalityForInstance(typeOf, bvMap)
                            then ()
                            else cantMatch (typA, typB, "(Field " ^ name ^ " requires an equality type)")
                    in
                        (* Apply the appropriate equality checks. *)
                        if equalityA then List.app (checkEquality bvMapB) unmatchedB  else ();
                        if equalityB then List.app (checkEquality bvMapA) unmatchedA else ()
                    end
        in
            combinedList
        end

        val argsA as (typeA, _) =
            case instanceA of
                SimpleInstance ty => (ty, fn _ => NONE)
            |   Instance i => i

        val argsB as (typeB, _)=
            case instanceB of
                SimpleInstance ty =>  (ty, fn _ => NONE)
            |   Instance i => i
        val () = match (argsA, argsB, typeA, typeB)
    in
        ! matchResult
    end (* unifyTypes *)

    (* Turn a result from matchTypes into a pretty structure so that it
       can be included in a message. *)
    fun unifyTypesErrorReport (_, alphaTypeEnv, betaTypeEnv, what) =
    let
        fun reportError(SimpleError(alpha: types, beta: types, reason)) =
            (* This previously used a single type variable sequence for
               both types.  It may be that this is needed to make
               sensible error messages. *)
            PrettyBlock(3, false, [],
                [
                    PrettyString ("Can't " ^ what (* "match" if a signature, "unify" if core lang. *)),
                    PrettyBreak (1, 0),
                    display (SimpleInstance alpha, 1000 (* As deep as necessary *), alphaTypeEnv),
                    PrettyBreak (1, 0),
                    PrettyString "to",
                    PrettyBreak (1, 0),
                    display (SimpleInstance beta, 1000 (* As deep as necessary *), betaTypeEnv),
                    PrettyBreak (1, 0),
                    PrettyString reason                        
                ])

        |   reportError(TypeConstructorError(alpha: types, beta: types, alphaCons, betaCons)) =
            let
                fun expandedTypeConstr(ty, tyEnv, TypeConstrs{identifier=TypeId { description, ...}, name=tcName, ...}) =
                let
                    fun lastPart name = #second(splitString name)

                    (* Print the type which includes the type constructor name with as
                       much additional information as we can. *)
                    fun printWithDesc{ location, name, description } =
                        PrettyBlock(3, false, [],
                            [ display (ty, 1000, tyEnv) ]
                            @ (if lastPart name = lastPart tcName then []
                               else
                                [
                                    PrettyBreak(1, 0),
                                    PrettyString "=",
                                    PrettyBreak(1, 0),
                                    PrettyBlock(0, false, [ContextLocation location], [PrettyString name])
                                ]
                              )
                            @ (if description = "" then []
                               else
                                [
                                    PrettyBreak(1, 0),
                                    PrettyBlock(0, false, [ContextLocation location],
                                        [PrettyString ("(*" ^ description ^ "*)")])
                                ]
                              )
                            )
                in
                    printWithDesc description
                end
            in
                PrettyBlock(3, false, [],
                    [
                        PrettyString ("Can't " ^ what (* "match" if a signature, "unify" if core lang. *)),
                        PrettyBreak (1, 0),
                        expandedTypeConstr(SimpleInstance alpha, alphaTypeEnv, alphaCons),
                        PrettyBreak (1, 0),
                        PrettyString (if what = "unify" then "with" else "to"),
                        PrettyBreak (1, 0),
                        expandedTypeConstr(SimpleInstance beta, betaTypeEnv, betaCons),
                        PrettyBreak (1, 0),
                        PrettyString "(Different type constructors)"                        
                    ])
            end
            
    in
        reportError
    end

    (* Prints out a type constructor e.g. type 'a fred = 'a * 'a
       or datatype 'a joe = bill of 'a list | mary of 'a * int or
       simply type 'a abs if the type is abstract. *)
    fun displayTypeConstrsWithMap (
        TypeConstrSet(
            TypeConstrs{identifier=TypeId{idKind=TypeFn{arity, resType=result, ...}, ...}, name, ...}, []), depth, typeEnv, sigMap) =
         (* Type function *)
        if depth <= 0 
        then PrettyString "..."
        else
        let
            val typeV = varNameSequence () (* Local sequence for this binding. *)
            val args = List.tabulate(arity, createBoundVar false)
        in
            PrettyBlock (3, false, [],
                PrettyString "type" ::
                PrettyBreak (1, 0) ::
                printTypeVars (args, depth, typeV) @
                [
                    PrettyString (#second(splitString name)),
                    PrettyBreak(1, 0),
                    PrettyString "=",
                    PrettyBreak(1, 0),
                    tDisp(SimpleInstance result, depth-1, typeV, typeEnv, sigMap)
                ]
                )
        end

    |   displayTypeConstrsWithMap (TypeConstrSet(tCons as TypeConstrs{identifier, name, ...}, [] (* No constructors *)), depth, _, _) =
        (* Abstract type or type in a signature. *)
        if depth <= 0 
        then PrettyString "..."
        else PrettyBlock (3, false, [],
            PrettyString (
                if isEquality identifier then "eqtype" else "type") ::
            PrettyBreak (1, 0) ::
            printTypeVars (List.tabulate(tcArity tCons,  createBoundVar false), depth, varNameSequence ()) @
            [PrettyString (#second(splitString name))]
            )

    |   displayTypeConstrsWithMap (TypeConstrSet(tCons as TypeConstrs{name=tcName, locations, ...}, tcConstructors), depth, typeEnv, sigMap) =
            (* It has constructors - datatype declaration *)
        if depth <= 0 
        then PrettyString "..."
        else
        let
            val typeV = varNameSequence ()
            (* Construct a ('a, 'b, 'c) tyCons construction for the result types
               of each of the constructors. *)
            val typeVars = List.tabulate(tcArity tCons,  createBoundVar false)
            val typeResult = mkTypeConstruction(tcName, tCons, typeVars, locations)

            (* Print a single constructor (blocked) *)
            fun pValConstr (first, name, typeOf, depth) =
            let
                val t = instanceToType(#1(generalise typeOf)) (* Why is this generalising? *)
                val firstBreak = PrettyBreak (1, if first then 2 else 0)
            in
                case t of
                    FunctionType { arg, result} =>
                    let
                        (* Constructor with an argument.  The constructor "type" is the argument.
                           We have to unify the result type of the function with the
                           ('a, 'b, 'c) tyCons type so that we get the correct type variables
                           in the argument.  We just print the argument of the function. *)
                        val _ = unifyTypes(SimpleInstance result, SimpleInstance typeResult)
                    in
                        [
                            firstBreak,
                            PrettyBlock (0, false, [],
                                PrettyBlock (0, false, [],
                                (if first then PrettyBreak (0, 2)
                                 else PrettyBlock (0, false, [], [PrettyString "|", PrettyBreak(1, 2)]) 
                                 ) ::
                                 (if depth <= 0 then [PrettyString "..."]
                                 else [ PrettyString name, PrettyBreak (1, 4), PrettyString "of"])
                            ) ::
                            (if depth > 0
                            then
                            [
                                PrettyBreak (1, 4),
                                (* print the type as a single block of output *)
                                tDisp (SimpleInstance arg, depth - 1, typeV, typeEnv, sigMap)
                            ]
                            else [])
                            )
                        ]
                    end

                |   _ =>
                    [
                        firstBreak,
                        PrettyBlock (0, false, [],
                            [if first then PrettyBreak (0, 2)
                            else PrettyBlock (0, false, [], [PrettyString "|", PrettyBreak(1, 2)]),
                            PrettyString (if depth <= 0 then "..." else name)]
                        )
                    ]
            end

            (* Print a sequence of constructors (unblocked) *)
            fun pValConstrRest ([],     _    ): pretty list = []
            |   pValConstrRest ((Value{typeOf=ValueType valType, name, ...}):: T, depth): pretty list =
                if depth < 0 then []           
                else pValConstr (false, name, valType, depth) @
                        pValConstrRest (T, depth - 1)
           
            fun pValConstrList ([],     _    ) = PrettyString "" (* shouldn't occur *)    
            |   pValConstrList ((Value{typeOf=ValueType valType, name, ...}) :: T, depth) =
                    PrettyBlock (2, true, [],
                        pValConstr (true, name, valType, depth) @
                        pValConstrRest (T, depth - 1)
                    )

            in
                PrettyBlock(0, false, [],
                    [
                        PrettyBlock(0, false, [],
                                PrettyString "datatype" ::
                                PrettyBreak (1, 2) ::
                                printTypeVars (typeVars, depth, typeV) @
                                [ PrettyString(#second(splitString tcName)), PrettyBreak(1, 0), PrettyString "=" ]
                        ),
                        pValConstrList (tcConstructors, depth - 1)
                    ]
                )
            end (* displayTypeConstrsWithMap *)

    fun displayTypeConstrs (tCons : typeConstrSet, depth : FixedInt.int, typeEnv) : pretty =
        displayTypeConstrsWithMap(tCons, depth, typeEnv, NONE)

    (* Return a type constructor from an overload.  If there are
       several (i.e. the overloading has not resolved to a single type)
       it returns the "best".  This is called in the third pass so it
       should never be called if there is not at least one type that
       is possible. *)
    fun typeConstrFromOverload overloadedFn =
    let
        fun prefType(OverloadSetVar(ref typeset)) =
            (
                case followRefChainToEnd typeset of
                    [tycons] => tycons
                |   types =>
                (
                    case preferredOverload types of
                        SOME tycons => tycons
                    |   NONE => raise InternalError "typeConstrFromOverload: No matching type"
                )
            )

        |   prefType(TypeConstruction{constr as TypeConstrs {identifier = TypeId{idKind = TypeFn _, ...},...}, args, ...}) =
                prefType (makeEquivalent (constr, args))

        |   prefType(TypeConstruction{constr, ...}) = constr

        |   prefType _ = raise InternalError "typeConstrFromOverload: No matching type"

        (* Given a function type returns the first argument if the
           function takes a tuple otherwise returns the only argument.
           Extended to include the case where the argument is not a function
           in order to work properly for overloaded literals. *)
        fun firstArg(FunctionType{arg=LabelledRecord ({typeOf, ...} ::_), ...}) = eventual typeOf
        |   firstArg(FunctionType{arg, ...}) = eventual arg
        |   firstArg t = t
    in
        prefType(firstArg(eventual overloadedFn))
    end

  (* Called at the end of a val or fun binding.
     There are various actions here depending on whether the expression is non-expansive
     and whether this is the top level.
     The current version side-effects type variables by adding on a generalisable type
     variable at the end of the chain if the type variable can be generalised.
     *)
    fun allowGeneralisation (instance, level, checkOverloadFlex, giveError) =
    let
        (* Map from type variables to bound variables we have already made. *)
        val typeVarMapList: (typeVar * tvIndex) list ref = ref nil
        val freeTypeVarMapList: (uniqueId * tvIndex) list ref = ref nil
        val flexRecordMapList: ({typeOf: types, name: string} list refChain ref * tvIndex) list ref = ref nil

        (* Perform checks and return the generic version with the templates. *)
        fun createGeneric(instance, templates: typeVarTemplate list) =
        case canonicalise instance of
            (typ as TypeVar(tvar as TypeVariable{level=ref lvl, equality=ref eq, ...}), _) =>
                if (case followRefChainToEnd lvl of NotGeneralisable v => v < level | Generalisable => false)
                then (* Explicit type variable at an outer level.  Same whether expansive or not.
                        For the moment just return this type variable. *)
                    (typ, templates)

                else (* If we've already made a bound variable here we need to reuse it. *)
                let
                    val isEq = followRefChainToEnd eq
                in
                    case List.find(fn (tv, _) => sameTv(tv, tvar)) (!typeVarMapList) of
                        NONE =>
                        let
                            val newIndex = TVIndex(List.length templates)
                            val () = typeVarMapList := (tvar, newIndex) :: (!typeVarMapList)
                        in
                            (createNewBoundVar isEq newIndex,
                                TemplPlain { equality= isEq} :: templates)
                        end
                    |   SOME(_, index) => (createNewBoundVar isEq index, templates)
                end

        |   (typ as FreeTypeVar{name, equality, level=NotGeneralisable fLevel, uid, ...}, _) =>
                (* If this is defined at an outer level it's not generalisable here. *)
                if fLevel < level then (typ, templates)
                else
                (
                    (* Generalisable here: See if we have already made a bound var. *)
                    case List.find(fn (id, _) => id = uid) (!freeTypeVarMapList) of
                        NONE =>
                        let
                            val newIndex = TVIndex(List.length templates)
                            val () = freeTypeVarMapList := (uid, newIndex) :: (!freeTypeVarMapList)
                        in
                            (createNewBoundVar equality newIndex, TemplPlain { equality=equality } :: templates)
                        end
                    |   SOME(_, index) => (BoundTypeVar(name, index), templates)
                )  
         
        |   (FreeTypeVar{level=Generalisable, ...}, _) => raise InternalError "createGeneric: generalisable free"

        |   (BoundTypeVar(_, index), bvMap) =>
            (
                case bvMap index of
                    SOME ty => createGeneric((ty, bvMap), templates)
                |   NONE => raise InternalError "createGeneric: already bound"
            )

        |   (typ as OverloadSetVar typeSetVar, _) =>
            if checkOverloadFlex
            then (* Narrow context for overloading: select "preferred" type. *)
            let
                val (v, typeset) = followRefChainToRef typeSetVar
            in
                case preferredOverload typeset of
                    SOME (constr as TypeConstrs {name, locations, ...}) =>
                    (
                        v := ChainEnd[constr];
                        (TypeConstruction{constr=constr, args=[], locations=locations, name=name}, templates)
                    )

                |   NONE => raise InternalError "general: No matching type"
            end
            else (* Normal setting: find overloading from full context. Return the current overload set. *)
                (typ, templates)

        |   (TypeConstruction {constr, args, locations, name}, bvMap) =>
            let
                fun copyEntry(ty, (resList, templs: typeVarTemplate list, n)) =
                let
                    val (copiedTyp, newtempls) = createGeneric((ty, bvMap), templs)
                in
                    (copiedTyp :: resList, newtempls, n+1)
                end
                val (copiedArgs, resTemplates, _) = List.foldl copyEntry ([], templates, 0) args
            in
                (TypeConstruction{constr=constr, args=List.rev copiedArgs, locations=locations, name=name}, resTemplates)
            end

        |   (FunctionType {arg, result}, bvMap) =>
            let
                val (copiedArg, argTemplates) = createGeneric((arg, bvMap), templates)
                val (copiedRes, resTemplates) = createGeneric((result, bvMap), argTemplates)
            in
                (FunctionType {arg = copiedArg, result = copiedRes}, resTemplates)
            end

        |   (LabelledRecord recList, bvMap) =>
            let
                fun copyRecordEntry({name, typeOf}, (fields, templs)) =
                let
                    val (copiedTyp, newtempls) = createGeneric((typeOf, bvMap), templs)
                in
                    ({name=name, typeOf=copiedTyp} :: fields, newtempls)
                end
                val (copiedList, resTempls) = List.foldl copyRecordEntry ([], templates) recList
            in
                (LabelledRecord(List.rev copiedList), resTempls)
            end

        |   (typ as FlexibleRecordVar{recList as ref recl, fullList as ref full, equality=ref eqv, level=ref lvl}, bvMap) =>
            let
                (* Construct a TemplFlexRec but make sure there is only one for each FlexibleRecordVar *)
                val (flexRec, _) = followRefChainToRef recList
            in
                case List.find(fn (fv, _) => fv = flexRec) (!flexRecordMapList) of
                    SOME(_, index) => (BoundTypeVar("'a", index), templates)
                |   NONE =>
                    let
                        val equality = followRefChainToEnd eqv
                        val tvLevel = followRefChainToEnd lvl
                    in
                        if (case tvLevel of NotGeneralisable v => v < level | Generalisable => false)
                        then (typ, templates)
                        else
                        let
                            val {frozen, ...} = followRefChainToEnd full
                            val recList = followRefChainToEnd recl
                            fun copyRecordEntry({name, typeOf}, (fields, templs)) =
                            let
                                val (copiedTyp, newtempls) = createGeneric((typeOf, bvMap), templs)
                            in
                                ({name=name, typeOf=copiedTyp} :: fields, newtempls)
                            end
                            val (copiedList, resTempls) = List.foldl copyRecordEntry ([], templates) recList
                        in
                            if frozen
                            then (LabelledRecord(List.rev copiedList), resTempls)
                            else
                            let
                                (* If we're using the narrow context it should have been resolved. *)
                                val () = if checkOverloadFlex then giveError("Type is an unresolved flexible record") else ()
                                val newIndex = TVIndex(List.length resTempls)
                                val newTempl =
                                    TemplFlexRec { equality=equality,
                                        recList=List.rev copiedList, fullList=fullList}
                                val () = flexRecordMapList := (flexRec, newIndex) :: ! flexRecordMapList
                            in
                                (createNewBoundVar false newIndex, newTempl :: resTempls)
                            end
                        end
                    end
            end

        |   (BadType, _) => (BadType, templates)

        val (ty, bvMap) = getCanonical instance

        val (newType, newTemplates) = createGeneric((ty, bvMap), [])
    in
        (newType, List.rev newTemplates)
    end (*  end allowGeneralisation *)

    (* See if the type contains non-unifiable i.e. explicit type variables but it's an expansive context. *)
    fun containsLocalFreeVariables(t, level) =
    let
        fun checkFree typ v =
        let
            val t = eventual typ
        in
            case t of
                TypeVar _ => v

                (* Explicit type variables at outer levels are allowed but not at this level. *)
            |   FreeTypeVar{level=NotGeneralisable fLev, ...} => v orelse fLev >= level
            |   FreeTypeVar{level=Generalisable, ...} => v (* Shouldn't occur *) 

                (* If this is a type function we mustn't check type variables that aren't actually used. *)
            |   TypeConstruction {args, constr=TypeConstrs{identifier=TypeId{idKind=TypeFn{usedTvs, ...}, ...}, ...}, ...} =>
                    #1 (List.foldl(fn (t, (v, n)) => (if BoolVector.sub(usedTvs, n) then checkFree t v else v, n+1)) (v, 0) args)

            |   TypeConstruction {args, ...} => (* Then process the arguments. *)
                    List.foldl (fn (t, v) => checkFree t v) v args
           
            |   FunctionType {arg, result} => checkFree arg (checkFree result v)
    
            |   LabelledRecord recList =>
                    List.foldl (fn ({ typeOf, ... }, v) => checkFree typeOf v) v recList

            |   FlexibleRecordVar{recList=ref recl, ...} =>
                    List.foldl(fn ({ typeOf, ... }, v) => checkFree typeOf v) v (followRefChainToEnd recl)

            |   BoundTypeVar _ => raise InternalError "containsLocalFreeVariables: boundvar"
            |   BadType => v
            |   OverloadSetVar _ => v
        end
    in
        checkFree (reduceToType(SimpleInstance t)) false
    end
    

    (* Check for free type variables at the top level.  Added for ML97.  This replaces the
       test in allowGeneralisation above and is applied to all top-level
       values including those in structures and functors.  *)
    fun checkForFreeTypeVariables(valName: string, ty: types, lex: lexan, printAndEqCode) : unit =
    let
        (* Generate new names for the type variables. *)
        val count = ref 0
        fun genName num =
            (if num >= 26 then genName (num div 26 - 1) else "")
            ^ String.str (Char.chr (num mod 26 + Char.ord #"a"));

        fun checkTypes (TypeVar (TypeVariable{level=ref lvl, equality=ref eq, link, ...}), _) () =
            if (case followRefChainToEnd lvl of Generalisable => false | NotGeneralisable _ => true)
            then (* The type variable is unbound and it is not generic i.e. it
                    must have come from an expansive expression. *)
                let
                    val name = "_" ^ genName(!count)
                    val _ = count := !count + 1;
                    val declLoc = location lex (* Not correct but OK for the moment. *)
                    val declDescription =
                        { location = declLoc, name = name, description = "Constructed from a free type variable." }
                    val tCons =
                        makeTypeConstructor (name,
                            makeFreeId(0, Global(printAndEqCode()), followRefChainToEnd eq, declDescription),
                            [DeclaredAt declLoc]);
                    val newVal = mkTypeConstruction(name, tCons, [], [])
                    val (refVal, _) = followRefChainToRef link
                in
                    warningMessage(lex, location lex, 
                        concat["The type of (", valName,
                            ") contains a free type variable. Setting it to a unique monotype."]);
                    refVal := ChainEnd(SOME(SimpleInstance newVal))
                end
            else ()
        |  checkTypes _ () = ()
    in
        foldType checkTypes (ty, fn _ => NONE) ();
        ()
    end

    (* See if a type abbreviation or "where type" has the form type t = s or
       type 'a t = 'a s etc and so is simply giving a new name to the type
       constructor.  If it is it then checks that the type constructor used
       (s in this example) is just a simple type name. *)
    fun typeNameRebinding(typeArgs, typeResult): typeId option =
    let
        fun eqTypeVar(BoundTypeVar(_, i), BoundTypeVar(_, j)) = i = j
        |   eqTypeVar _ = false
    in
        case typeResult of
            TypeConstruction {constr, args, ... } =>
                if not (ListPair.allEq eqTypeVar(args, typeArgs))
                then NONE
                else
                (
                    case constr of
                        TypeConstrs{identifier=TypeId{idKind=TypeFn _, ...}, ...} => NONE
                    |   TypeConstrs{identifier, ...} => SOME identifier
                )
        |   _ => NONE
    end

    local
        (* We have to use the full list; not all fields may be in recList. *)
        fun getFieldList(LabelledRecord rcrd) = map #name rcrd
        |   getFieldList(FlexibleRecordVar{fullList=ref full, ...}) = #names(followRefChainToEnd full)
        |   getFieldList(TypeVar(TypeVariable{link=ref l, ...})) = getFieldList(instanceToType(valOf(followRefChainToEnd l)))
        |   getFieldList(TypeConstruction{
                    constr as TypeConstrs{identifier=TypeId{idKind=TypeFn _, ...}, ...}, args, ...}) =
                getFieldList(makeEquivalent(constr, args)) (* Type function *)
        |   getFieldList _ = raise InternalError "getFieldList - not a record"
    in
        (* Returns the number of the entry in the list. Used to find out the
           location of fields in a labelled record for expressions and pattern
           matching. Assumes that the label appears in the list somewhere. *)
        fun entryNumber (label, rcrd) = 
        let
            val fields = getFieldList rcrd

            fun entry (name :: l) n = if name = label then n else entry l (n + 1)
            |   entry [] _ = raise InternalError "entryNumber: label not present"
        in
            entry fields 0
        end

        (* Size of a labelled record. *)
        and recordWidth rcrd = List.length(getFieldList rcrd)
        
    end

    (* Get the codetree "types".  This is used during code-generation to see if
       a function arguments or results are a tuple or contain floating-point values. *)
    local
        fun isFloatId constr =
            if isSameTypeIdAs(constr, realConstr) then DoubleFloatType
            else if isSameTypeIdAs(constr, floatConstr) then SingleFloatType
            else GeneralType
    in
        fun getCodetreeType (LabelledRecord []) = [GeneralType] (* Unit *)
        |   getCodetreeType (LabelledRecord recList) =
            let
                fun getType{typeOf, ...} =
                    case getCodetreeType typeOf of [single] => single | _ => GeneralType
            in
                List.map getType recList
            end

        |   getCodetreeType (TypeVar(TypeVariable{link=ref l, ...})) =
                (case followRefChainToEnd l of NONE => [GeneralType] | SOME t => getCodetreeType(reduceToType t))

        |   getCodetreeType (TypeConstruction{
                    constr as TypeConstrs{identifier=TypeId{idKind=TypeFn _, ...}, ...}, args, ...}) =
                getCodetreeType (makeEquivalent(constr, args))

        |   getCodetreeType (TypeConstruction{constr, args=[], ...}) = [isFloatId constr]

        |   getCodetreeType(OverloadSetVar(ref typeset)) =
            (
                case followRefChainToEnd typeset of
                    [single] =>  [isFloatId single]
                |   multiple =>
                    case preferredOverload multiple of
                        SOME t => [isFloatId t] (* real only.  float is never preferred. *)
                    |   NONE => [GeneralType]
            )

        |   getCodetreeType _ = [GeneralType]
        
        val getCodetreeType = getCodetreeType o instanceToType
    end

    fun checkDiscard(t: instanceType, lex: lexan): string option =
    let
        open DEBUG
        val checkLevel = getParameter reportDiscardedValuesTag (debugParams lex)

        fun isUnit(LabelledRecord []) = true (* Unit is actually an empty record *)
        |   isUnit(TypeConstruction{
                    constr as TypeConstrs{identifier=TypeId{idKind=TypeFn _, ...}, ...},
                    args, ...}) =
                isUnit(makeEquivalent(constr, args))
        |   isUnit(TypeVar _) = true (* Allow unbound type vars *)
        |   isUnit _ = false
        
        fun isAFunction(FunctionType _) = true
        |   isAFunction(TypeConstruction{
                    constr as TypeConstrs{identifier=TypeId{idKind=TypeFn _, ...}, ...},
                    args, ...}) =
                isAFunction(makeEquivalent(constr, args))
        |   isAFunction _ = false
    in
        case checkLevel of
            1 => if isAFunction (eventual(instanceToType t)) then SOME "A function value is being discarded." else NONE
        |   2 => if isUnit (eventual(instanceToType t)) then NONE else SOME "A non unit value is being discarded."
        |   _ => NONE
    end

    (* Returns true if this is the function that returns ref.  It is used when a constructor
       application has been found.  In all other cases this would be non-expansive but if the
       constructor is "ref" it has to be treated as expansive. *)
    fun isRefFunction t =
        case eventual t of
            FunctionType{result, ...} =>
                (case eventual result of
                    TypeConstruction{constr, ...} => isSameTypeIdAs (constr, refConstr)
                |   _ => false)
        |   _ => false

    structure Sharing =
    struct
        type types      = types
        and  values     = values
        and  typeId     = typeId
        and  structVals = structVals
        and  typeConstrs= typeConstrs
        and  typeConstrSet=typeConstrSet
        and  locationProp = locationProp
        and  pretty     = pretty
        and  lexan      = lexan
        and  ptProperties = ptProperties
        and  typeVar    = typeVar
        and  codetree   = codetree
        and  matchResult = matchResult
        and  generalMatch = generalMatch
        and  tvLevel = tvLevel
        and  valAccess  = valAccess
        and  tvIndex = tvIndex
        and  typeVarTemplate = typeVarTemplate
        and  argumentType = argumentType
        and  instanceType = instanceType
    end
end;
