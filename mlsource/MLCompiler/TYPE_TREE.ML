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

  (* added 16/4/96 SPF *)
  fun sameTypeVar (TypeVar x, TypeVar y) = sameTv (x, y)
    | sameTypeVar _                      = false;

  
  fun isTypeVar          (TypeVar          _) = true
    | isTypeVar          _ = false;
     
  fun isFunctionType     (FunctionType     _) = true
    | isFunctionType     _ = false;

  fun isEmpty             EmptyType           = true
    | isEmpty            _ = false;
    
  fun isBadType           BadType             = true
    | isBadType          _ = false;

  val emptyType            = EmptyType;

  fun typesTypeVar          (TypeVar          x) = x 
    | typesTypeVar          _ = raise Match;
    
  fun typesFunctionType     (FunctionType     x) = x
     | typesFunctionType     _ = raise Match;


(*************)

    fun mkTypeVar (level, equality, nonunifiable) = 
        TypeVar (makeTv {value=emptyType, level=level, equality=equality,
                         nonunifiable=nonunifiable });

    fun mkTypeConstruction (name, typc, args, locations) =
        TypeConstruction {name = name, constr = typc,
                          args = args, locations = locations}

    local
        (* Turn a tuple into a record of the form {1=.., 2=... }*)
        fun maptoRecord ([], _) = []
          | maptoRecord (H::T, i) = 
                {name=Int.toString i, typeof=H} :: maptoRecord (T,i+1)
    in
        fun mkProductType (typel: types list) =
        let
            val fields = maptoRecord (typel, 1)
        in
            LabelledType {recList = fields, fullList = FieldList(List.map #name fields, true)}
        end
    end

    (* Helper functions copied from STRUCT_VALS.  Not exported.   These should
       generally be replaced by the direct pattern matching. *)
    fun tcName (TypeConstrs {name,...})       = name

    fun tcArity(TypeConstrs {identifier=TypeId{idKind=TypeFn{arity, ...},...}, ...}) = arity
    |   tcArity(TypeConstrs {identifier=TypeId{idKind=Bound{arity, ...},...}, ...}) = arity
    |   tcArity(TypeConstrs {identifier=TypeId{idKind=Free{arity, ...},...}, ...}) = arity

    fun makeTypeConstructor (name, uid, locations) = TypeConstrs { name = name, identifier = uid, locations = locations }

    fun tsConstr(TypeConstrSet(ts, _)) = ts

    local
        fun follow (FlexibleList(ref r)) = follow r
        |   follow (FieldList c) = c
    in
        fun recordIsFrozen { fullList, ...} =
            #2 (follow fullList)
        and recordFields {fullList, ...} =
            #1 (follow fullList)
    end


  fun mkFunctionType (arg, result) = 
      FunctionType {arg = arg, result = result};

  fun mkOverloadSet [constr] =
    (* If there is just a single constructor in the set we make
       a type construction from it. *)
        mkTypeConstruction(tcName constr, constr, nil, [])
   | mkOverloadSet constrs = 
    let
        (* Make a type variable and point this at the overload set
           so we can narrow down the overloading. *)
        val var = mkTypeVar (generalisable, false, false)
        val set = OverloadSet {typeset=constrs};
    in
        tvSetValue (typesTypeVar var, set);
        var
    end

    fun mkLabelled (l, frozen) = 
    let
        val final = FieldList(map #name l, frozen)
        val lab =
            LabelledType {recList = l,
                          fullList = if frozen then final else FlexibleList(ref final) }
    in
        if frozen
        then lab
        else
        let
           (* Use a type variable so that the record can be expanded.
              This also provides a model (equality etc). for any fields that
              are added later. *)
            val var = mkTypeVar (generalisable, false, false)
            val () =
                if isTypeVar var
                then tvSetValue (typesTypeVar var, lab)
                else ();
        in
            var
        end
    end

    (* Must remove leading zeros because the labels are compared by
       string comparison. *)
    fun mkLabelEntry (name, t) = 
    let
        fun stripZeros s = 
            if size s <= 1  orelse String.str(String.sub(s, 0)) <> "0" 
            then s
            else stripZeros (String.substring(s, 1, size s-1));
    in
        {name = stripZeros name, typeof = t}
    end

    (* Create a name for a type variable. *)
    fun makeTypeVariableName(n, isEq) =
    let
        fun makeName n = if n < 26 then [chr(n+ord #"a")] else chr(n mod 26 + ord #"a") :: makeName(n div 26)
        val varName = List.rev(makeName n)
        val name = String.implode(if isEq then #"'" :: #"'" :: varName else #"'" :: varName)
    in
        name
    end

    (* Create a bound variable.  Give it the name 'a, 'b etc.  Create the first ten
       in advance to reduce duplication. *)
    local
        fun createNewBoundVar isEq n = BoundTypeVar(makeTypeVariableName(n, isEq), TVIndex n)
        val nonEqVars = Vector.tabulate(10, createNewBoundVar false)
        and eqVars = Vector.tabulate(10, createNewBoundVar true)
    in
        fun createBoundVar true n = if n < 10 then Vector.sub(eqVars, n) else createNewBoundVar true n
        |   createBoundVar false n = if n < 10 then Vector.sub(nonEqVars, n) else createNewBoundVar false n
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

        (* Add to the overload list.  Since this is intended only for the bootstrap there's
           no need to remove duplicates. *)
        fun addOverload (n, cons, code) =
        let
            val existing = getOpt(hashSub(overloadTable, n), [])
        in
            hashSet(overloadTable, n, (cons, code) :: existing)
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

  (* Chains down a list of type variables returning the type they are
     bound to. As a side-effect it also points all the type variables
     at this type to reduce the need for future chaining and to free
     unused type variables. Normally a type variable points to at most
     one other, which then points to "empty". However if we have unified
     two type variables by pointing one at the other, there may be type
     variables which pointed to the first and which cannot be found and
     redirected at the second until they themselves are examined. *)
     
    fun eventual (t as (TypeVar tv)) : types =
    let
        (* Note - don't change the level/copy information - the only type
           variable with this correct is the one at the end of the list. *)
        val oldVal = tvValue tv
        val newVal = eventual oldVal;   (* Search that *)
    in
        (* Update the type variable to point to the last in the chain. *)
        tvSetValue (tv, newVal); (* Put it on *)

        case newVal of
            EmptyType => t (* Not bound to anything - return the type variable *)
        |   LabelledType (r as { recList, fullList }) =>
            if List.length recList = List.length(recordFields r)
            then (* All the generic fields are present so we don't need to do anything. *)
                if recordIsFrozen r then newVal else t
            else (* We need to add fields from the generic. *)
            let
                (* Add any fields from the generic that aren't present in this instance. *)
                fun createNewField name =
                        { name = name,
                          (* The new type variable has to be created with the same properties
                             as if we had first generalised it from the generic and then
                             unified with this instance.
                             The level is inherited from the instance since the generic
                             will always have level = generalisable.  Nonunifiable must be false. *)
                            typeof = mkTypeVar (tvLevel tv, tvEquality tv, false)}

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

                val newList = addToInstance(recordFields r, recList)
                val newRecord =
                    LabelledType {recList = newList, fullList = fullList}
            in
                tvSetValue(tv, newRecord);
                if recordIsFrozen r then newRecord else t
            end
 
        |   OverloadSet _ => t (* Return the set of types. *)

        |   _ => newVal (* Return the type it is bound to *)
    end
    
    | eventual t (* not a type variable *) = t;


    (* Apply a function to every element of a type. *)
    fun foldType f =
    let
        fun foldT typ v =
        let
            val t   = eventual typ;
            val res = f t v; (* Process this entry. *)
        in
            case t of
                TypeVar tv => foldT (tvValue tv) res

            |   TypeConstruction {args, ...} => (* Then process the arguments. *)
                    List.foldr (fn (t, v) => foldT t v) res args
           
            |   FunctionType {arg, result} => foldT arg (foldT result res)
    
            |   LabelledType {recList,...} =>
                    List.foldr (fn ({ typeof, ... }, v) => foldT typeof v) res recList

            |   BoundFlexRecord(_, {recList, ...}) =>
                    List.foldr (fn ({ typeof, ... }, v) => foldT typeof v) res recList

            |   BoundTypeVar _ => res

            |   FreeTypeVar _ => res

            |   BadType => res
         
            |   EmptyType => res
          
            |   OverloadSet _ => res
        end
    in
        foldT
    end
    (* See if the types are the same. This is a bit of a fudge, but saves carrying
       around a flag saying whether the structures were copied. This is only an
       optimisation. If the values are different it will not go wrong. *)
    val identical : types * types -> bool = PolyML.pointerEq
    and identicalConstr : typeConstrs * typeConstrs -> bool = PolyML.pointerEq
    and identicalList : 'a list * 'a list -> bool = PolyML.pointerEq

    (* Copy a type, avoiding copying type structures unnecessarily.
       Used to make new type variables for all distinct type variables when
       generalising polymorphic functions, and to make new type stamps for
       type constructors when generalising signatures. *)
    fun copyType (at, indexBTV, copyTypeVar, copyFreeTypeVar, copyTypeConstr) =
    let
        fun copyList [] = []
        |   copyList (l as (h :: t)) =
        let
            val h' = copyType (h, indexBTV, copyTypeVar, copyFreeTypeVar, copyTypeConstr);
            val t' = copyList t;
        in
            if identical (h', h) andalso identicalList (t', t)
            then l
            else h' :: t'
        end  (* copyList *);

        fun copyRecordList [] = []
        |   copyRecordList (l as ({name, typeof} :: t)) =
        let
            val typeof' = copyType (typeof, indexBTV, copyTypeVar, copyFreeTypeVar, copyTypeConstr);
            val t' = copyRecordList t;
        in
            if identical (typeof', typeof) andalso identicalList (t', t)
            then l
            else {name=name, typeof=typeof'} :: t'
        end  (* copyList *);

        val atyp = eventual at
    in
        case atyp of
            TypeVar _ =>  (* Unbound type variable, flexible record or overloading. *)
                copyTypeVar atyp
    
        |   BoundTypeVar(_, index) =>
                (* Instantiate the type variable if there is a map.  Currently this can also be called when
                   processing signatures so allow for the look-up to fail and return the original. *)
                getOpt(indexBTV index, atyp)

        |   FreeTypeVar _ => copyFreeTypeVar atyp

        |   TypeConstruction {constr, args, locations, ...} => 
            let
                val copiedArgs   = copyList args;
                val copiedConstr = copyTypeConstr constr;
                (* Use the name from the copied constructor.  This will normally
                   be the same as the original EXCEPT in the case where we are
                   using copyType to generate copies of the value constructors of
                   replicated datatypes. *)
                val copiedName = tcName copiedConstr
            in
                if identicalList (copiedArgs, args) andalso identicalConstr (copiedConstr, constr)
                then atyp 
                else (* Must copy it. *) 
                    mkTypeConstruction (copiedName, copiedConstr, copiedArgs, locations)
            end 
           
        |   FunctionType {arg, result} => 
            let
                val copiedArg  = copyType (arg,    indexBTV, copyTypeVar, copyFreeTypeVar, copyTypeConstr);
                val copiedRes  = copyType (result, indexBTV, copyTypeVar, copyFreeTypeVar, copyTypeConstr);
            in
                if identical (copiedArg, arg) andalso identical (copiedRes, result)
                then atyp 
                else FunctionType {arg = copiedArg, result = copiedRes}
            end 

        |   LabelledType ({recList, fullList}) =>
                (* Rigid labelled records only.  Flexible ones are treated as type vars. *)
            let
                val copiedList = copyRecordList recList
            in
                if identicalList (copiedList, recList)
                then atyp 
                else LabelledType {recList = copiedList, fullList = fullList}
            end

        |   BoundFlexRecord(index, {recList, fullList}) =>
            (
                case indexBTV index of
                    SOME typeVar =>
                    let
                        val typV =
                            case typeVar of TypeVar tv => tv | _ => raise InternalError "copyType: BoundFlexRecord not variable"
                        val copiedList = copyRecordList recList
                    in
                        (* Set this to a labelled record. *)
                        tvSetValue(typV, LabelledType {recList=copiedList, fullList=fullList});
                        typeVar (* Return the type variable. *)
                    end
                    (* This can occur if we are processing a flexible record inside a structure.
                       Currently we copy the "signature".  *)
                |   NONE => BoundFlexRecord(index, {recList=copyRecordList recList, fullList=fullList})
            )

        |   EmptyType => EmptyType

        |   BadType =>
                BadType
        
        |   OverloadSet _ =>
                raise InternalError "copyType: OverloadSet found"

    end (* copyType *)

    datatype generalMatch = Matched of {old: typeVarForm, new: types};

    (* Copies the type replacing any generalisable type variables with new type variables.
       checkTv is backwards compatibility.
       This is used to create the instance of a polymorphic type.  It should actually
       be removed and replaced with mapping. *)
    fun generaliseTypes (atyp : types, lookupBound, checkTv: typeVarForm->types option) = 
    let
        val madeList = ref [] (* List of tyVars. *);

        fun tvs atyp =
        let
            val tyVar = typesTypeVar atyp;
        in
            case List.find(fn Matched{old, ...} => sameTv (old, tyVar)) (!madeList) of
                SOME(Matched{new, ...}) => new
            |   NONE =>
                (
                case checkTv tyVar of
                    SOME found => found
                |   NONE =>
                    let  (* Not on the list - make a new name *)
                        (* Make a unifiable type variable even if the original
                           is nonunifiable. *)
                        val n : types = 
                            mkTypeVar (generalisable, tvEquality tyVar, false)
                    in
                        (* Set the new variable to have the same value as the
                           existing.  That is only really needed if we have an
                           overload set. *)
                        tvSetValue (typesTypeVar n, tvValue tyVar);
                        madeList := Matched {old = tyVar, new = n} :: !madeList;
                        n
                    end
                )
        end

        fun copyFreeTypeVar atyp = atyp

        fun copyTypeVar (atyp as TypeVar tyVar) =
            if tvLevel tyVar <> generalisable
            then atyp (* Not generalisable. *)
            else (* Unbound, overload set or flexible record *)
            let
                val newTv = tvs atyp
            in
                (* If we have a type variable pointing to a flexible record we have to
                   copy the type pointed at by the variable. *)
                case tvValue tyVar of
                    valu as LabelledType _ =>
                        tvSetValue (typesTypeVar newTv, copyType (valu, SOME o lookupBound, copyTypeVar, copyFreeTypeVar, fn t => t))
                |   _ => ();
                newTv
            end

        |   copyTypeVar atyp = atyp
        
        val copied =
            (* Only process type variables. Return type constructors unchanged. *)
            copyType (atyp, SOME o lookupBound, copyTypeVar, copyFreeTypeVar, fn t => t (*copyTCons*))
    in
        (copied, ! madeList)
    end (* generaliseTypes *)

    (* Create type variables.  If unifiable is true, the usual case, the type
       variables are new assignable variables.
       If it is false, used when creating a type to be matched from a structure, type variables
       are nonunifiable. This case occurs if we have a structure without an explicit signature.
       The signature is created from the structure but it might have had overloading or flex
       records that have only be resolved later. *)
    fun createTypeVariable unifiable (TemplPlain{ equality }) =
            makeTv {value=emptyType, level=generalisable, equality=equality,
                         nonunifiable=not unifiable}
    |   createTypeVariable _ (TemplFree tv) = tv
    |   createTypeVariable _ (TemplOverload s) =
            makeTv {value=OverloadSet{typeset=Overloads.getOverloadTypes s}, level=generalisable, equality=false,
                         nonunifiable=false}

    (* Create a map for type variables.  Optimise the most common cases *)
    fun createTypeVariableMap _ [] = (fn _ => raise InternalError "createTypeVariableMap")

    |   createTypeVariableMap unifiable [a] =
        let
            val tv = createTypeVariable unifiable a
        in
            fn TVIndex 0 => tv | _ => raise InternalError "createTypeVariableMap"
        end

    |   createTypeVariableMap unifiable [a, b] =
        let
            val tvA = createTypeVariable unifiable a
            and tvB = createTypeVariable unifiable b
        in
            fn TVIndex 0 => tvA | TVIndex 1 => tvB | _ => raise InternalError "createTypeVariableMap"
        end

    |   createTypeVariableMap unifiable [a, b, c] =
        let
            val tvA = createTypeVariable unifiable a
            and tvB = createTypeVariable unifiable b
            and tvC = createTypeVariable unifiable c
        in
            fn TVIndex 0 => tvA | TVIndex 1 => tvB | TVIndex 2 => tvC | _ => raise InternalError "createTypeVariableMap"
        end

    |   createTypeVariableMap unifiable templs =
        let
            val tvs = map (createTypeVariable unifiable) templs
            val tvVector = Vector.fromList tvs
        in
            fn TVIndex n => Vector.sub(tvVector, n)
        end

    (* Exported wrapper for generaliseTypes. *)
    fun generalise(atyp, templs) =
    let
        (* Create free type variables for each template. *)
        val freeTypeMap = TypeVar o (createTypeVariableMap true) templs
        val (t, _) = generaliseTypes (atyp, freeTypeMap, fn _ => NONE)
        val typeVarList = List.tabulate(List.length templs, fn n => freeTypeMap(TVIndex n))
    in
        (t, typeVarList)
    end

    (* If we have a type construction which is an alias for another type
       we construct the alias by first instantiating all the type variables
       and then copying the type. *)
    fun makeEquivalent (TypeConstrs {identifier=TypeId{idKind=TypeFn{resType=typeResult, ...}, ...},...}, args) =
        let
            val vars = Vector.fromList args
            fun getArg(TVIndex n) = Vector.sub(vars, n)
        in
            #1 (generaliseTypes(typeResult, getArg, fn _ => NONE))
        end

    |   makeEquivalent _ = raise InternalError "makeEquivalent: Not a type function"

  (* This 3-valued logic is used because in a few cases we may not be sure
     if equality testing is allowed. If we have 2 mutually recursive datatypes
     t = x of s | ... and s = z of t we would first examine "t", find that
     it uses "s", look at "s", find that refers back to "t". To avoid
     infinite recursion we return the result that equality "maybe"
     allowed for "t" and hence for "s". However we may find that the
     other constructors for "t" do not allow equality and so equality
     will not be allowed for "s" either. *)
     
  datatype tri = Yes (* 3-valued logic *)
               | No
               | Maybe;


  (* Returns a flag saying if equality testing is allowed for values of
     the given type. "equality" is used both to generate the code for
     a specific call of equality e.g.  (a, b, c) = f(x), and to generate
     the equality operation for a type when it is declared. In the latter
     case type variables may be parameters which will be filled in later e.g.
     type 'a list = nil | op :: of ('a * 'a list). "search"
     is a function which looks up constructors in mutually recursive type
     declarations. "lookupTypeVar" deals with type variables. If they
     represent parameters to a type declaration equality 
     checking will be allowed. If we are unifying this type to an equality
     type variable they  will be unified to new equality type variables.
     Otherwise equality is not allowed. *)
    
    fun computeEquality (ty, lookupTypId, lookupTypeVar, lookupBTV) : tri =
    let
        (* Can't use foldT because it is not monotonic
           (equality on ref 'a is allowed). *)
        (* Returns Yes only if equality testing is allowed for all types
           in the list. *)
        fun eqForList ([],    soFar) = soFar
          | eqForList (x::xs, soFar) = 
            case computeEquality (x, lookupTypId, lookupTypeVar, lookupBTV) of
               No    => No
             | Maybe => eqForList (xs, Maybe)
             | Yes   => eqForList (xs, soFar);
    in
        case eventual ty of
            TypeVar tyVar => (* The type variable may point to a flexible record or
                          an overload set or it may be the end of the chain.
                          If this is a labelled record we have to make sure that
                          any fields we add also admit equality.
                          lookupTypeVar makes the type variable an equality type
                          so that any new fields are checked for equality but
                          we also have to call "equality" to check the existing
                          fields. *)
                if tvEquality tyVar then Yes
                else
                    (
                    case tvValue tyVar of
                        lab as LabelledType _ =>
                            (
                            case lookupTypeVar tyVar of
                                No => No
                              | _ => computeEquality (lab, lookupTypId, lookupTypeVar, lookupBTV)
                            )
                     | _ => lookupTypeVar tyVar
                    )

        |   BoundTypeVar(_, index) => lookupBTV index
        
        |   FreeTypeVar{equality, ...} => if equality then Yes else No

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
         
        |   LabelledType {recList, ...} => (* Record equality if all subtypes are (ignore frozen!) *)
                (* TODO: Avoid copying the list? *)
                eqForList (map (fn{typeof, ...}=>typeof) recList, Yes)

        |   BoundFlexRecord _ => raise InternalError "computeEquality: BoundFlexRecord"


        |   OverloadSet _ =>
                (* This should not happen because all overload sets should be pointed
                   to by type variables and so should be handled in the TypeVar case. *)
                raise InternalError "equality - Overloadset found"

        |   BadType => No

        |   EmptyType => No (* shouldn't occur *)
    end

    (* Can this be used to set an eqtype? Used in "where type" constructions in signatures. *)
    fun typePermitsEquality ty = computeEquality (ty, fn _ => No, fn _ => Yes, fn _ => Yes) <> No

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
                    computeEquality (#arg (typesFunctionType valTypeOf),
                        genEquality, fn _ => Yes, fn _ => Yes)
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
                            fun lookupTypeVar _ = raise InternalError "lookupTypeVar"
                            fun lookupBoundTV(TVIndex i) = (BoolArray.update(eqRefArray, i, true); Yes)
                            val eq = computeEquality (resType, genEquality, lookupTypeVar, lookupBoundTV)
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

    (* Create a type function and compute the relevant information. *)
    fun makeGeneralTypeFunction(arity, resType, desc, access) =
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
                |   LabelledType {recList,...} => List.app (processTypes o #typeof) recList
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
            fun lookupTypeVar _ = raise InternalError "lookupTypeVar"
            fun lookupBTV(TVIndex i) = (BoolArray.update(eqRefArray, i, true); Yes)
            val eq = computeEquality (resType, fn _ => No, lookupTypeVar, lookupBTV)
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

    fun makeTypeFunction(arity, resType, desc) =
        makeGeneralTypeFunction(arity, resType, desc, Global CodeZero)

    (* The unit type is equivalent to the empty record. *)
    val unitConstr   =
        makeTypeConstructor ("unit",
            makeTypeFunction(
                0, LabelledType {recList = [], fullList = FieldList([], true)},
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
    fun isProductType(LabelledType(r as {recList=recList as _::_::_, ...})) =
    let
        fun isRec [] _ = true
         |  isRec ({name, ...} :: l) n =
                name = Int.toString n andalso isRec l (n+1)
    in
        recordIsFrozen r andalso isRec recList 1
    end
    | isProductType _ = false;


    (* Test to see is a type constructor is in an overload set. *)
    fun isInSet(tcons, (H::T): typeConstrs list) =
            isSameTypeIdAs (tcons, H) orelse isInSet(tcons, T)
    |   isInSet(_, []: typeConstrs list) = false

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

    fun equalTypeIds(x, y) =
    let
        (* True if two types are equal. *)
        fun equalTypes (TypeConstruction{constr=xVal, args=xArgs, ...},
                        TypeConstruction{constr=yVal, args=yArgs, ...}) =
            isSameTypeIdAs(xVal, yVal) andalso equalTypeLists (xArgs, yArgs)

        | equalTypes (FunctionType x, FunctionType y) = 
            equalTypes (#arg x, #arg y)   andalso 
            equalTypes (#result x, #result y)
                 
        | equalTypes (LabelledType x, LabelledType y) =
            recordIsFrozen x andalso recordIsFrozen y andalso  
            equalRecordLists (#recList x, #recList y)

        | equalTypes (TypeVar x, TypeVar y)  = sameTv (x, y)

        | equalTypes (EmptyType, EmptyType) = true

        | equalTypes _ = false
                              
        and equalTypeLists ([], [])    = true
        |   equalTypeLists (x::xs, y::ys) =
               equalTypes(x, y) andalso equalTypeLists (xs, ys)
        |   equalTypeLists _           = false

        and equalRecordLists ([],        [])    = true
        |   equalRecordLists (x::xs, y::ys) =
               #name x = #name y andalso 
               equalTypes(#typeof x, #typeof y) andalso equalRecordLists (xs, ys)
        |   equalRecordLists _             = false
    in
        case (x, y) of
            (TypeId{idKind=TypeFn{resType=xEquiv, ...}, ...}, TypeId{idKind=TypeFn{resType=yEquiv, ...}, ...}) =>
                (* TODO TYPEFN.  Check if we have the same type fn and if so just look at the arguments.  Otherwise
                   unwrap the larger typeFunCount *)
                equalTypes(xEquiv, yEquiv)
        |   _ => sameTypeId(x, y) 
    end

    (* Copy a type constructor if it is Bound and in the required range.  If this refers to a type
       function copies that as well. Does not copy value constructors. *)
    fun copyTypeConstrWithCache (tcon as TypeConstrs{identifier=tcId, locations, ...}, typeMap, _, mungeName, cache) =
        case tcId of
            TypeId{idKind = TypeFn{arity, resType=equiv, ...}, description, access, ...} =>
            let
                val copiedEquiv =
                    copyType(equiv, fn _ => NONE, fn x => x, fn x => x,
                        fn tcon =>
                            copyTypeConstrWithCache (tcon, typeMap, fn x => x, mungeName, cache))
            in
                if identical (equiv, copiedEquiv)
                then tcon (* Type is identical and we don't want to change the name. *)
                else (* How do we find a type function? *)
                    makeTypeConstructor (mungeName(tcName tcon),
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
                        val name = #second(splitString (tcName tcon))
                        (* We must only match here if they're really the same. *)
                        fun cacheMatch (TypeConstrs{identifier, name=tcName, ...}) =
                            equalTypeIds(identifier, newId)
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
                                val newName = mungeName(tcName tcon)
                            in
                                (*print(concat[tcName tcon, " not cached\n"]);*)
                                makeTypeConstructor(newName, newId, locations)
                            end
                    end
            )

    (* Exported version. *)
    fun copyTypeConstr (tcon, typeMap, copyTypeVar, mungeName) =
        copyTypeConstrWithCache(tcon, typeMap, copyTypeVar, mungeName, [])

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
                    copyType(equiv, fn _ => NONE, fn x => x, fn x => x,
                        fn tcon => copyTypeConstr (tcon, copyId, fn x => x, fn y => y))
            in
                makeGeneralTypeFunction(arity, copiedEquiv, description, access)
            end
    in
        map2(m1 n)
    end

    (* Similar to generaliseTypes but with the bound variables replaced by non-unifiable
       free variables.  This is used in signature matching.  A polymorphic function
       in a structure will match a similar monomorphic function but not the other way
       round. *)
    fun createNonGeneralisableInstance((atyp, templs), copyId) =
    let
        fun createFreeTypeVariables([], _) = []
        |   createFreeTypeVariables(TemplPlain{ equality } :: templs, n) =
                FreeTypeVar{ name = makeTypeVariableName(n, equality), equality=equality, level=1, uid=makeUniqueId() } ::
                        createFreeTypeVariables(templs, n+1)
        |   createFreeTypeVariables _ = raise InternalError "createNonGeneralisableInstance: template"
        
        val freeVars = createFreeTypeVariables(templs, 0)
        fun boundVarMap(TVIndex n) = SOME(List.nth(freeVars, n))
        fun copyATypeConstr tcon = copyTypeConstr(tcon, copyId, fn x => x, fn s => s)
        fun copyTarget t = (* Leave type variables. *)
            copyType (t, boundVarMap, fn x => x, fn x => x, copyATypeConstr);
        val copiedTarget = copyTarget atyp
    in
        copiedTarget
    end

    (* Basic procedure to print a type structure. *)
    type printTypeEnv =
        { lookupType: string -> (typeConstrSet * (int->typeId) option) option,
          lookupStruct: string -> (structVals * (int->typeId) option) option}

    val emptyTypeEnv = { lookupType = fn _ => NONE, lookupStruct = fn _ => NONE }

    (* Test whether two type constructors are the same after mapping.
       This is used to try to find the correct "path" to a type constructor
       when printing. *)
    fun eqTypeConstrs(xTypeCons, xMap, yTypeCons, yMap) =
    let
        fun id x = x
        fun copyId (SOME mapTypeId) (TypeId{idKind=Bound{ offset, ...}, ...}) = SOME(mapTypeId offset)
        |   copyId _ _ = NONE
        val mappedX = copyTypeConstr(xTypeCons, copyId xMap, id, id)
        and mappedY = copyTypeConstr(yTypeCons, copyId yMap, id, id)
    in
        isSameTypeIdAs(mappedX, mappedY)
    end

    (* prints a block of items *)
    fun tDisp (t : types, depth : FixedInt.int, typeVarName : typeVarForm -> string, env: printTypeEnv,
               sigMap: (int->typeId)option) : pretty =
    let
        (* prints a block of items *)
        fun dispP (t : types, depth : FixedInt.int) : pretty =
        let
            (* prints a block of items *)
            fun parenthesise depth t =
            if depth <= 1
            then PrettyString "..."
            else
                PrettyBlock (0, false, [],
                    [
                        PrettyString "(",
                        dispP (t, depth - 1),
                        PrettyString ")"
                    ]);
    
            (* prints a sequence of items *)
            fun prettyList [] _ _: pretty list = []

            |   prettyList [H] depth separator =
                let
                    val v = eventual H;
                in
                    if separator = "*" andalso
                        (isFunctionType v orelse isProductType v)
                    then (* Must bracket the expression *) [parenthesise depth v]
                    else [dispP (v, depth)]
                end

            |   prettyList (H :: T) depth separator =
                if depth <= 0
                then [PrettyString "..."]
                else
                let
                    val v = eventual H;
                in
                    PrettyBlock (0, false, [],
                        [(if separator = "*" andalso
                           (isFunctionType v orelse isProductType v)
                        then (* Must bracket the expression *) parenthesise depth v
                        else dispP (v, depth)),
                        PrettyBreak (if separator = "," then 0 else 1, 0),
                        PrettyString separator
                        ]) ::
                    PrettyBreak (1, 0) ::
                    prettyList T (depth - 1) separator
                end;
        
            val typ = eventual t; (* Find the real type structure *)
        in 
            case typ of
                TypeVar tyVar =>
                let
                    val tyVal : types = tvValue tyVar;
                in
                    case tyVal of
                        EmptyType => PrettyString (typeVarName tyVar)
                    |   _         => dispP (tyVal, depth)
                end

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
                        (
                            (* This must be the name of a type. *)
                            case lookupType typeName of
                                SOME (t, map) =>
                                    if eqTypeConstrs(typeConstructor, sigMap, tsConstr t, map)
                                    then FoundMatch else FoundNotMatch
                            |   NONE => NotFound
                        )
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
                                then dispP (argVal, depth - 1)
                                else if depth <= 1
                                then PrettyString "..."
                                else PrettyBlock(0, false, [],
                                         [PrettyString "(", PrettyBreak (0, 0)]
                                         @ prettyList args (depth - 1) ","
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
                        then parenthesise depth evArg
                        else dispP (evArg, depth - 1),
                        PrettyBreak(1, 2),
                        PrettyString "->",
                        PrettyBreak (1, 2),
                        dispP (result, depth - 1)
                        ])
                end
                     
            |   LabelledType (r as {recList, ...}) =>
                if depth <= 0
                then PrettyString "..."
                else if isProductType typ
                then (* Print as a product *)
                    PrettyBlock (0, false, [], (* Print them as t1 * t2 * t3 .... *)
                        prettyList (map (fn {typeof, ...} => typeof) recList) depth "*")
                else (* Print as a record *)
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
                              | pRec ({name, typeof} :: T) depth =
                                if depth <= 0 then [PrettyString "..."]
                                else
                                    [
                                    PrettyBlock(0, false, [],
                                        [
                                            PrettyBlock(0, false, [],
                                                [
                                                PrettyString (name ^ ":"),
                                                PrettyBreak(1, 0),
                                                dispP(typeof, depth - 1)
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
                        [ PrettyString (if recordIsFrozen r then "}" else case recList of [] =>   "...}" | _  => ", ...}")]
                        )
                end

            |   BoundFlexRecord(_, reclist) => dispP (LabelledType reclist, depth)

            |   OverloadSet {typeset = []} => PrettyString "no type"

            |   OverloadSet {typeset = tconslist} =>
                (* This typically arises when printing error messages in the second pass because
                   the third pass will select a single type e.g. int where possible.  To
                   simplify the messages select a single type if possible. *)
                (
                    case preferredOverload tconslist of
                        SOME tcons => dispP(mkTypeConstruction (tcName tcons, tcons,[], []), depth)
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
                        fun tconsItem tcons =
                            PrettyBlock(0, false, constrLocation tcons, [PrettyString(tcName tcons)])

                        fun printTCcons [] = []
                          | printTCcons [tcons] = [tconsItem tcons]
                          | printTCcons (tcons::rest) =
                                tconsItem tcons :: PrettyBreak (0, 0) ::
                                PrettyString "/" :: printTCcons rest
                    in
                        PrettyBlock (0, false, [], printTCcons tconslist)
                    end
                )

            |   EmptyType => PrettyString "no type"
            
            |   BadType => PrettyString "bad"
        end (* dispP *)
    in
        dispP (t, depth)
    end (* tDisp *);

  (* Generate unique type-variable names. *)

    fun varNameSequence () : typeVarForm -> string = 
    (* We need to ensure that every distinct type variable has a distinct name.
       Each new type variable is given a name starting at "'a" and going on
       through the alphabet. *)
    let
        datatype names = Names of {name: string, entry: typeVarForm}
        val nameNum    = ref ~1
        val gNameList  = ref [] (* List of names *)
    in
        (* If the type is already there return the name we have given it
           otherwise make a new name and put it in the list. *)
        fn var =>
            case List.find (fn (Names {entry,...}) => sameTv (entry, var)) (!gNameList) of
                NONE =>  (* Not on the list - make a new name *)
                let
                    fun name num = (if num >= 26 then name (num div 26 - 1) else "")
                          ^ String.str (Char.chr (num mod 26 + Char.ord #"a"))
                    val () = nameNum := !nameNum + 1
                    val n = (if tvEquality var then "''" else "'") ^ name(!nameNum)
                    (* Should explicit type variables be distinguished? *)
                in
                    gNameList := Names{name=n, entry=var} :: !gNameList;
                    n
                end
           |    SOME (Names {name,...}) => name
    end (* varNameSequence *)

    (* Print a type (as a block of items) *)
    fun displayWithMap (t : types, depth : FixedInt.int, env, sigMap) =
        tDisp (t, depth, varNameSequence (), env, sigMap)
    and display (t : types, depth : FixedInt.int, env) =
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
        printTypeVariables (fn (var, depth, typeV) => tDisp (var, depth, typeV, emptyTypeEnv, NONE))

    (* Parse tree for types.  This is used to represent types in the source. *)
    datatype typeParsetree =
        ParseTypeConstruction of
            { name: string, args: typeParsetree list,
              location: location, nameLoc: location, argLoc: location,
              (* foundConstructor is set to the constructor when it has been
                 looked up.  This allows us to get the location where it was
                 declared if we export the parse-tree. *)
              foundConstructor: typeConstrs ref }
    |   ParseTypeProduct of
            { fields: typeParsetree list, location: location }
    |   ParseTypeFunction of
            { argType: typeParsetree, resultType: typeParsetree, location: location }
    |   ParseTypeLabelled of
            { fields: ((string * location) * typeParsetree * location) list,
              frozen: bool, location: location }
    |   ParseTypeId of { types: parseTypeVar, location: location }
    |   ParseTypeBad (* Place holder for errors. *)

    and parseTypeVar =
        ParseTypeFreeVar of { name: string, equality: bool, typeVar: types ref }
    |   ParseTypeBoundVar of { name: string, index: tvIndex }
    |   ParseTypeError (* Error case *)

    fun typeFromTypeParse(
            ParseTypeConstruction{ args, name, location, foundConstructor = ref constr, ...}) =
        let
            val argTypes = List.map typeFromTypeParse args
        in
            TypeConstruction {name = name, constr = constr,
                              args = argTypes, locations = [DeclaredAt location]}
        end

    |   typeFromTypeParse(ParseTypeProduct{ fields, ...}) =
            mkProductType(List.map typeFromTypeParse fields)
    
    |   typeFromTypeParse(ParseTypeFunction{ argType, resultType, ...}) =
            mkFunctionType(typeFromTypeParse argType, typeFromTypeParse resultType)
    
    |   typeFromTypeParse(ParseTypeLabelled{ fields, frozen, ...}) =
        let
            fun makeField((name, _), t, _) = mkLabelEntry(name, typeFromTypeParse t)        
        in
            mkLabelled(sortLabels(List.map makeField fields), frozen)
        end

    |   typeFromTypeParse(ParseTypeId{ types=ParseTypeFreeVar{typeVar=ref(typ as FreeTypeVar _), ...}, ...}) = typ

    |   typeFromTypeParse(ParseTypeId{ types=ParseTypeFreeVar _, ...}) = raise InternalError "typeFromTypeParse: Not set"

    |   typeFromTypeParse(ParseTypeId{ types=ParseTypeBoundVar{index=TVIndex index, ...}, ...}) = createBoundVar false index

    |   typeFromTypeParse(ParseTypeId{ types=ParseTypeError, ...}) =
            TypeVar(makeTv{value=emptyType, level=generalisable, equality=false, nonunifiable=true})
   
    |   typeFromTypeParse(ParseTypeBad) = BadType
    
    fun makeParseTypeConstruction((constrName, nameLoc), (args, argLoc), location) =
        ParseTypeConstruction{
            name = constrName, nameLoc = nameLoc, args = args, argLoc = argLoc,
            location = location, foundConstructor = ref undefConstr }

    fun makeParseTypeProduct(recList, location) =
        ParseTypeProduct{ fields = recList, location = location }

    fun makeParseTypeFunction(arg, result, location) =
        ParseTypeFunction{ argType = arg, resultType = result, location = location }

    fun makeParseTypeLabelled(recList, frozen, location) =
        ParseTypeLabelled{ fields = recList, frozen = frozen, location = location }

    fun makeParseTypeId(types, location) =
        ParseTypeId{ types = types, location = location }

    (* Free type variables can appear at various places in an ML program and denote the same type
       variable at each occurrence.  *)
    fun makeParseTypeFreeVar(name, isEq) =
        ParseTypeFreeVar{ name=name, typeVar = ref EmptyType, equality=isEq }

    (* Bound type variables appear in type and datatype definitions. *)
    fun makeParseTypeBoundVar(name, index) =
        ParseTypeBoundVar{ name=name, index=index }
    
    val parseTypeVarError = ParseTypeError

    fun getBoundTypeVar(ParseTypeBoundVar{index=TVIndex i, ...}) = createBoundVar false i
    |   getBoundTypeVar ParseTypeError = BadType
    |   getBoundTypeVar(ParseTypeFreeVar _) = raise InternalError "getBoundTypeVar: free"

    (* Set a free type variable to either an outer one or to the current level. *)
    fun setParseTypeVar(ParseTypeFreeVar{name, typeVar, equality, ...}, other, level) =
        let
            (* Should not have been previously set. *)
            val _ = case !typeVar of EmptyType => () | _ => raise InternalError "setParseTypeVar: already set"
            val toSet =
                case other of
                    SOME(ParseTypeFreeVar{typeVar=ref (otherTv as FreeTypeVar _), ...}) => otherTv
                |   SOME _ => raise InternalError "setParseTypeVar: other not free"
                |   NONE => FreeTypeVar {name=name, equality=equality, level=level, uid=makeUniqueId()}
        in
            typeVar := toSet
        end

    |   setParseTypeVar _ = raise InternalError "setParseTypeVar: not free"

    fun unitTree location = ParseTypeLabelled{ fields = [], frozen = true, location = location }

    (* Build an export tree from the parse tree. *)
    fun typeExportTree(navigation, p: typeParsetree) =
    let        
        val typeof = typeFromTypeParse p

        (* Common properties for navigation and printing. *)
        val commonProps =
            PTprint(fn d => display(typeof, d, emptyTypeEnv)) ::
            PTtype typeof ::
            exportNavigationProps navigation

        fun asParent () = typeExportTree(navigation, p)
        
    in
        case p of
            ParseTypeConstruction{ location, nameLoc, args, argLoc, ...} =>
            let
                (* If the constructor has been bound return the declaration location.
                   We have to attach the declaration location in the right place if
                   this is a polytype e.g. if we have "int list" here we will have
                   the location for "list" which is the second item not the first. *)
                val (name, decLoc) =
                    case typeof of
                        TypeConstruction { constr=TypeConstrs{locations, ...}, name, ...} =>
                            (name, mapLocationProps locations)
                    |   _ => ("", []) (* Error? *)
                val navNameAndArgs =
                (* Separate cases for nullary, unary and higher type constructions. *)
                    case args of
                        [] => decLoc (* Singleton e.g. int *)
                    |   [oneArg] =>
                        let (* Single arg e.g. int list. *)
                            (* Navigate between the type constructor and the argument.
                               Since the arguments come before the constructor we go there first. *)
                            fun getArg () =
                                typeExportTree({parent=SOME asParent, previous=NONE, next=SOME getName}, oneArg)
                            and getName () =
                                getStringAsTree({parent=SOME asParent, previous=SOME getArg, next=NONE},
                                            name, nameLoc, decLoc)
                        in
                            [PTfirstChild getArg]
                        end
                    |   args =>
                        let (* Multiple arguments e.g. (int, string) pair *)
                            fun getArgs () =
                                (argLoc,
                                    exportList(typeExportTree, SOME getArgs) args @
                                        exportNavigationProps{parent=SOME asParent, previous=NONE, next=SOME getName})
                            and getName () =
                                getStringAsTree({parent=SOME asParent, previous=SOME getArgs, next=NONE},
                                            name, nameLoc, decLoc)
                        in
                            [PTfirstChild getArgs]
                        end
            in
                (location, navNameAndArgs @ commonProps)
            end

        |   ParseTypeProduct{ location, fields, ...} =>
                (location, exportList(typeExportTree, SOME asParent) fields @ commonProps)

        |   ParseTypeFunction{ location, argType, resultType, ...} =>
                (location, exportList(typeExportTree, SOME asParent) [argType, resultType] @ commonProps)

        |   ParseTypeLabelled{ location, fields, ...} =>
            let
                fun exportField(navigation, label as ((name, nameLoc), t, fullLoc)) =
                    let
                        (* The first position is the label, the second the type *)
                        fun asParent () = exportField (navigation, label)
                        fun getLab () =
                            getStringAsTree({parent=SOME asParent, next=SOME getType, previous=NONE},
                                name, nameLoc, [PTtype(typeFromTypeParse t)])
                        and getType () =
                            typeExportTree({parent=SOME asParent, previous=SOME getLab, next=NONE}, t)
                    in
                        (fullLoc, PTfirstChild getLab :: exportNavigationProps navigation)
                    end
            in
                (location, exportList(exportField, SOME asParent) fields @ commonProps)
            end

        |   ParseTypeId{ location, ...} =>
                (location, commonProps)

        |   ParseTypeBad =>
                (nullLocation, commonProps)
    end

    fun displayTypeParse(types, depth, env) = display(typeFromTypeParse types, depth, env)

    local
        fun printTypeVar(ParseTypeBoundVar{name, ...}, _, _) = PrettyString name
        |   printTypeVar(ParseTypeFreeVar{name, ...}, _, _) = PrettyString name
        |   printTypeVar(ParseTypeError, _, _) = PrettyString "'error"
    in
        fun displayTypeVariables (vars : parseTypeVar list, depth : FixedInt.int) =
          printTypeVariables printTypeVar (vars, depth, varNameSequence ())
    end

    (* Associates type constructors from the environment with type identifiers
       (NOT type variables) *)
    fun assignTypes (tp : typeParsetree,  lookupType : string * location -> typeConstrSet, lex : lexan) =
    let
        fun typeFromTypeParse(ParseTypeConstruction{ args, name, location, foundConstructor, ...}) =
            let
                (* Assign constructor, then the parameters. *)
                val TypeConstrSet(constructor, _) = lookupType (name, location)
                val () =
                    (* Check that it has the correct arity. *)
                    case constructor of
                        TypeConstrs{name=tcName, ...} =>
                        let
                            val arity = tcArity constructor
                            val num = length args
                        in
                            if arity <> num
                            then (* Give an error message *)
                            errorMessage (lex, location,
                                String.concat["Type constructor (", tcName,
                                    ") requires ", Int.toString arity, " type(s) not ",
                                    Int.toString num])
                            else foundConstructor := constructor
                        end
                val argTypes = List.map typeFromTypeParse args
            in
                TypeConstruction {name = name, constr = constructor,
                                  args = argTypes, locations = [DeclaredAt location]}
            end

        |   typeFromTypeParse(ParseTypeProduct{ fields, ...}) =
                mkProductType(List.map typeFromTypeParse fields)
    
        |   typeFromTypeParse(ParseTypeFunction{ argType, resultType, ...}) =
                mkFunctionType(typeFromTypeParse argType, typeFromTypeParse resultType)
    
        |   typeFromTypeParse(ParseTypeLabelled{ fields, frozen, ...}) =
            let
                fun makeField((name, _), t, _) = mkLabelEntry(name, typeFromTypeParse t)        
            in
                mkLabelled(sortLabels(List.map makeField fields), frozen)
            end

        |   typeFromTypeParse(ParseTypeId{ types=ParseTypeFreeVar{typeVar=ref EmptyType, ...}, ...}) = raise InternalError "assignTypes: free type not set"
        |   typeFromTypeParse(ParseTypeId{ types=ParseTypeFreeVar{typeVar=ref typ, ...}, ...}) = typ
        |   typeFromTypeParse(ParseTypeId{ types=ParseTypeBoundVar{index=TVIndex index, ...}, ...}) = createBoundVar false index
        |   typeFromTypeParse(ParseTypeId{ types=ParseTypeError, ...}) =
                TypeVar(makeTv{value=emptyType, level=generalisable, equality=false, nonunifiable=true})

        |   typeFromTypeParse(ParseTypeBad) = BadType
    in
        typeFromTypeParse tp
    end;

    (* When we have finished processing a list of patterns we need to check
       that the record is now frozen. *)
    fun recordNotFrozen (TypeVar t) : bool = (* Follow the chain *) recordNotFrozen (tvValue t)
    |   recordNotFrozen (LabelledType r) = not(recordIsFrozen r)
    |   recordNotFrozen _ = false (* record or type alias *);

   (* Find the argument type which gives this result when the constructor
      is applied. If we have, for example, a value of type int list and
      we have discovered that this is a "::" node we have to work back by
      comparing the type of "::" ('a * 'a list -> 'a list) to find the
      argument of the constructor (int * int list) and hence how to print it.
     (Actually "list" is treated specially). *)
    fun constructorResult (FunctionType{arg, result=TypeConstruction{args, ...}}, typeArgs) =
        let
            (* Temporary check - they should be bound variables in order *)
            fun checkArg(n, BoundTypeVar(_, TVIndex m) ::args) =
                if n <> m then raise InternalError "checkArg" else checkArg(n+1, args)
            |   checkArg(n, []) = if n = List.length typeArgs then () else raise InternalError "checkArg"
            |   checkArg _ = raise InternalError "checkArg"
            val _ = checkArg(0, args)
            val argMap = Vector.fromList typeArgs
        in
            #1 (generaliseTypes(arg, fn(TVIndex n) => Vector.sub(argMap, n), fn _ => NONE))
        end
    | constructorResult _ = raise InternalError "Not a function type"

    (* Look for the occurrence of locally declared datatypes in the type of a value.
       This works by looking to see if we have a reference to a local datatype whose
       address has not yet been set.  If it hasn't that means it's either local to
       a function or refers to a datatype that occurs textually later.  That can
       happen if we have a ref that we assign later.  See Tests/Fail/Test064-69. *)
    fun checkForEscapingDatatypes(ty: types, errorFn: string->unit) : unit =
    let
        fun checkTypes (typ: types) (ok: bool) : bool =
        case typ of
            TypeConstruction {constr as TypeConstrs
                {identifier=TypeId{access=Local{addr, ...}, ...}, ...}, ...} =>
                if ok andalso !addr < 0
                then 
                (
                    errorFn("Type of expression contains local datatype (" ^ tcName constr
                            ^") outside its definition.");
                    false
                )
                else ok (* Could we have a "selected" entry with a local datatype? *)

        |    TypeConstruction {constr=TypeConstrs
                {identifier = TypeId{idKind = TypeFn{resType, ...}, ...},...}, ...} =>
                    (* May be an alias for a type that contains a local datatype. *)
                    foldType checkTypes resType ok

        |   _ => ok
    in
        foldType checkTypes ty true;
        ()
    end
    
    datatype matchResult =
        SimpleError of types * types * string
    |   TypeConstructorError of types * types * typeConstrs * typeConstrs

    (* Type matching algorithm for both unification and signature matching. *)
    (* The mapping has now been moved out of here.  Instead when signature matching the
       target signature is copied before this is called which means that this
       process is now symmetric.  There may be some redundant tests left in here. *)
    fun unifyTypes(Atype : types, Btype : types) : matchResult option =
    let
        (* Get the result in here.  This isn't very ML-like but it greatly
           simplifies converting the code. *)
        val matchResult: matchResult option ref = ref NONE
        fun matchError error = (* Only report one error. *)
            case matchResult of ref (SOME _) => () | r => r := SOME error
        fun cantMatch(alpha, beta, text) = matchError(SimpleError(alpha, beta, text))
  
        fun match (Atype : types, Btype : types) : unit =
        let (* Check two records/tuples and return the combined type. *)
            fun unifyRecords (rA as {recList=typAlist, fullList = gA},
                              rB as {recList=typBlist, fullList = gB},
                              typA : types, typB : types) : types =
            let        
                val typAFrozen = recordIsFrozen rA
                and typBFrozen = recordIsFrozen rB

                fun matchLabelled ([], []) = []
          
                     (* Something left in bList - this is fine if typeA is not frozen.
                        e.g.  (a: s, b: t) will match (a: s, ...) but not just (a:s). *)
                |   matchLabelled ([], bList as {name=bName, ...} :: _) =
                    ( 
                        if typAFrozen
                        then cantMatch (typA, typB, "(Field " ^ bName ^ " missing)")
                        else ();
                        bList (* return the remainder of the list *)
                    )

                |   matchLabelled (aList as {name=aName, ...} :: _, []) = (* Something left in bList *)
                    ( 
                        if typBFrozen
                        then cantMatch (typA, typB, "(Field " ^ aName ^ " missing)")
                        else ();
                        aList (* the rest of aList *)
                    )
        
                |   matchLabelled (aList as ((aVal as {name=aName,typeof=aType})::aRest),
                                   bList as ((bVal as {name=bName,typeof=bType})::bRest)) =
                    (* both not nil - look at the names. *)
                    (
                        case compareLabels (aName, bName) of
                            EQUAL => (* same name - must be unifiable types *)
                            ( (* The result is (either) one of these with the rest of the list. *)
                                match (aType, bType);
                                aVal :: matchLabelled (aRest, bRest)
                            )
                        |   LESS => (* aName < bName *)
                                (* The entries in each list are in order so this means that this
                                       entry is not in bList. If the typeB is frozen this is an error. *)
                                if typBFrozen (* Continue with the entry removed. *)
                                then (cantMatch (typA, typB, "(Field " ^ aName ^ " missing)"); aList)
                                else aVal :: matchLabelled (aRest, bList)
                        |   GREATER => (* aName > bName *)
                                if typAFrozen
                                then (cantMatch (typA, typB, "(Field " ^ bName ^ " missing)"); bList)
                                else bVal :: matchLabelled (aList, bRest)
                     )
 
                (* Return the combined list. Only actually used if both are flexible. *)
                val result =
                    if typAFrozen andalso typBFrozen andalso List.length typAlist <> List.length typBlist
                    then (* Don't attempt to unify the fields if we have the wrong number of items.
                            If we've added or removed an item from a tuple e.g. a function with
                            multiple arguments, it's more useful to know this than to get unification
                            errors on fields that don't match. *)
                        (cantMatch (typA, typB, "(Different number of fields)"); [])
                    else matchLabelled (typAlist, typBlist)

                fun lastFlex(FlexibleList(ref(r as FlexibleList _))) = lastFlex r
                |   lastFlex(FlexibleList r) = SOME r
                |   lastFlex(FieldList _) = NONE

            in
                if typAFrozen
                then (if typBFrozen then () else valOf(lastFlex gB) := gA; typA)
                else if typBFrozen
                then (valOf(lastFlex gA) := gB; typB)
                else
                let
                    (* We may have these linked already in which case we shouldn't do anything. *)
                    val lastA = valOf(lastFlex gA) and lastB = valOf(lastFlex gB)
                in
                    if lastA = lastB
                    then ()
                    else
                    let
                        fun getFields(FlexibleList(ref r)) = getFields r
                        |   getFields(FieldList(g, _)) = g
                        (* Merge the generics. *)
                        fun mergeGen([], a) = a
                        |   mergeGen(a, []) = a
                        |   mergeGen(a as aHd :: aTl, b as bHd :: bTl) =
                            (
                                case compareLabels (aHd, bHd) of
                                    EQUAL => aHd :: mergeGen(aTl, bTl)
                                |   LESS => aHd :: mergeGen(aTl, b)
                                |   GREATER  => bHd :: mergeGen(a, bTl)
                            )
                        val genericFields = FieldList(mergeGen(getFields gA, getFields gB), false)
                    in
                        (* If these are both flexible we have link all the generics together
                           so that if we freeze any one of them they all get frozen. *)
                        lastA := genericFields;
                        lastB := FlexibleList lastA
                    end;
                    LabelledType {recList = result, fullList = gA}
                end
            end (* unifyRecords *);

            (* Sets a type variable to a value. - Checks that the type variable
               we are assigning does not occur in the expression we are about to
               assign to it. Such cases can occur if we have infinitely-typed
               expressions such as fun a. a::a where a has type 'a list list ...
               Also propagates the level information of the type variable.
               Now also deals with flexible records. *)
            fun assign (var, t) =
            let
              (* Mapped over the type to be assigned. *)
              (* Returns "false" if it is safe to make the assignment. Sorts out
                 imperative type variables and propagates level information.
                 N.B. It does not propagate equality status. The reason is that
                 if we are unifying ''a with 'b ref, the 'b does NOT become
                 an equality type var. In all other cases it would. *)
                fun occursCheckFails _ true = true

                |   occursCheckFails ty false =
                    let
                        val t = eventual ty
                    in
                        case t of
                            TypeVar tvar =>
                            let
                                (* The level is the minimum of the two, and if we are unifying with
                                   an equality type variable we must make this into one. *)
                                val minLev =
                                    case (tvLevel var, tvLevel tvar) of
                                        (NotGeneralisable v1, NotGeneralisable v2) =>
                                            NotGeneralisable(Int.min (v1, v2))
                                    |   (v1 as NotGeneralisable _, _) => v1
                                    |   (_, v2 as NotGeneralisable _) => v2
                                    |   (Generalisable, Generalisable) => Generalisable
                                val oldValue = tvValue tvar
                            in
                                if tvLevel tvar <> minLev
                                then (* If it is nonunifiable we cannot make its level larger. *)
                                    if tvNonUnifiable tvar
                                then cantMatch (Atype, Btype, "(Type variable is free in surrounding scope)")
                                else
                                let
                                    (* Must make a new type var with the right properties *)
                                    (* This type variable may be a flexible record, in which
                                       case we have to save the record and put it on the new
                                       type variable.  We have to do this for the record itself so that new
                                       fields inherit the correct status and also for any existing fields. *)
                                    val newTv = mkTypeVar (minLev, tvEquality tvar, false)
                                in
                                    tvSetValue (typesTypeVar newTv, oldValue);
                                    tvSetValue (tvar, newTv)
                                end
                                else ();
                                (* Safe if vars are different but we also have to check any flexible records. *)
                                occursCheckFails oldValue (sameTv (tvar, var))
                            end

                        |   TypeConstruction {args, constr=TypeConstrs{identifier = TypeId {idKind = TypeFn{usedTvs, ...}, ...}, ...}, ...} =>
                            (* Type function: Only process arguments that are actually used in the type expression. *)
                            let
                                fun checkArgs([], _, b) = b
                                |   checkArgs(arg::args, n, b) =
                                        checkArgs(args, n+1, 
                                            if BoolVector.sub(usedTvs, n) then occursCheckFails arg b else b)  
                            in
                                checkArgs(args, 0, false)
                            end

                        |   TypeConstruction {args, ...} =>
                                (* Datatypes. *)
                                List.foldr (fn (t, v) => occursCheckFails t v) false args

                        |   FunctionType {arg, result} =>
                                occursCheckFails arg false orelse occursCheckFails result false

                        |   LabelledType {recList,...} =>
                                List.foldr (fn ({ typeof, ... }, v) => occursCheckFails typeof v) false recList

                        |   _ => false

                    end

                val varVal = tvValue var (* Current value of the variable to be set. *)

                local
                    (* Generally if the type we're assigning contains the type variable that
                       is an "occurs check" failure.  If we have a type function it is possible
                       that what looks like an occurs check failure is actually valid; either
                       because the type variable is not actually used or because the function
                       boils down to the variable itself.  We need to unwrap it sufficiently
                       that any apparent occurs check has been eliminated.  We must not assign
                       the original type to the type variable because that would create a loop. *)
                    fun followVarsAndTypeFunctions t =
                    case eventual t of
                        ev as TypeConstruction{constr as TypeConstrs{identifier = TypeId {idKind = TypeFn _, ...}, ...}, args, ...} =>
                            let
                                fun occurs(TypeVar tvar) b = b orelse sameTv(tvar, var) | occurs _ b = b
                            in
                                if List.foldl (fn (t, b) => foldType occurs t b) false args
                                then followVarsAndTypeFunctions(makeEquivalent (constr, args))
                                else ev
                            end
                    |   ev => ev
                in
                    val finalType = followVarsAndTypeFunctions t
                end
      
                (* We may actually have the same type variable after any type abbreviations
                    have been followed. *)
                val reallyTheSame =
                    case finalType of
                        TypeVar tv => sameTv (tv, var)
                    |   _ => false

            in (* start of "assign" *)
                case varVal of
                    LabelledType _ =>
                    (* Flexible record. Check that the records are compatible. *)
                        match (varVal, finalType)
                |   OverloadSet _ =>
                     (* OverloadSet.  Check that the sets match.  This is only in the
                        case where t is something other than an overload set since
                        we remove the overload set from a variable when unifying two
                        sets. *)
                        match (varVal, finalType)
                |   _ => ();
 
                if reallyTheSame
                then () (* Don't apply the occurs check or check for non-unifiable. *)

                (* If this type variable was put in explicitly then it can't be
                   assigned to something else. (We have already checked for the
                   type variables being the same). *)
                else if tvNonUnifiable var
                then cantMatch (Atype, Btype, "(Cannot unify with explicit type variable)")

                else if occursCheckFails finalType false
                then cantMatch (Atype, Btype, "(Type variable to be unified occurs in type)")

                else
                let (* Occurs check succeeded. *)
                    fun canMkEqTv (tvar : typeVarForm) : tri = 
                    (* Turn it into an equality type var. *)
                    if tvEquality tvar then Yes 
                    (* If it is nonunifiable we cannot make it into an equality type var. *)
                    else if tvNonUnifiable tvar then No
                    else (* Must make a new type var with the right properties *)
                    let  (* This type variable may be a flexible record or an overload set,
                            in which case we have to save the record and put it on the
                            new type variable.
                            We have to do both because we have to ensure that the existing
                            fields in the flexible record admit equality and ALSO that any
                            additional fields we may add by unification with other records
                            also admit equality. *)
                        val newTv = mkTypeVar (tvLevel tvar, true, false)
                        val oldValue = tvValue tvar
                    in
                        tvSetValue (tvar, newTv);
                        (* If this is an overloaded type we must remove any types that
                           don't admit equality. *)
                        case oldValue of
                            OverloadSet{typeset} =>
                            let
                                fun tcEquality(TypeConstrs{identifier, ...}) = isEquality identifier
                            (* Remove any types which do not admit equality. *)
                               fun filter [] = []
                               |    filter (h::t) =
                                        if tcEquality h then h :: filter t else filter t
                            in
                                case filter typeset of
                                    [] => No
                                |   [constr] =>
                                    ( (* Turn a singleton into a type construction. *)
                                        tvSetValue (typesTypeVar newTv,
                                            mkTypeConstruction(tcName constr, constr, nil, []));
                                        Yes
                                    )
                                |   newset =>
                                    (
                                        tvSetValue (typesTypeVar newTv, OverloadSet{typeset=newset});
                                        Yes
                                    )
                            end
                        |   _ => (* Labelled record or unbound variable. *)
                            (
                                tvSetValue (typesTypeVar newTv, oldValue);
                                Yes
                            )
                    end
      
                in
                    (* If we are unifying a type with an equality type variable
                       we must ensure that equality is allowed for that type. This
                       will turn most type variables into equality type vars. *)
                    if tvEquality var andalso computeEquality (finalType, fn _ => No, canMkEqTv, fn _ => raise InternalError "computeEq") = No
                    then cantMatch (Atype, Btype, "(Requires equality type)")
                      (* TODO: This can result in an unhelpful message if var is bound
                         to a flexible record since there is no indication in the
                         printed type that the flexible record is an equality type.
                         It would be improved if we set the value to be EmptyType.
                         At least then the type variable would be printed which would
                         be an equality type.
                         --- Adding the "Requires equality type" should improve things. *)
                    else ();
                    (* Actually make the assignment. It doesn't matter if var is 
                       a labelled record, because t will be either a fixed record
                       or a combination of the fields of var and t.  Likewise if
                       var was previously an overload set this may replace the set
                       by a single type construction. *)
                    (* If we have had an error don't make the assignment.  At the very least
                       it could prevent us producing useful error information and it could
                       also result in unnecessary consequential errors. *)
                    case !matchResult of
                        NONE => if reallyTheSame then () else tvSetValue (var, finalType)
                    |   SOME _ => ()
                end
            end (* assign *);

            (* First find see if typeA and typeB are unified to anything
               already, and get the end of a list of "flexibles".  *)

            val tA = eventual Atype
            and tB = eventual Btype

            fun isUndefinedType(TypeConstruction{constr, ...}) = isUndefinedTypeConstr constr
            |   isUndefinedType _ = false
        in (* start of "match" *)
            if isUndefinedType tA orelse isUndefinedType tB
            then () (* If either of these was an undefined type constructor don't try to match. 
                       TODO: There are further tests below for this which are now redundant. *)
            else
            case (tA, tB) of
                (* If either of these was an undefined type constructor don't try to match. 
                           TODO: There are further tests below for this which are now redundant. *)
                (BadType, _) => () (* If either is an error don't try to match *)
            |   (_, BadType) => ()

            |   (TypeVar typeAVar, TypeVar typeBVar) =>
                (* Unbound type variable, flexible record or overload set. *)
                let
                    (* Even if this is a one-way match we can allow type variables
                       in the typeA to be instantiated to anything in the typeB. *)
                    val typeAVal = tvValue typeAVar;
                    (* We have two unbound type variables or flex. records. *)
                in
                    if sameTv (typeAVar, typeBVar) (* same type variable? *)
                    then ()
                    else (* no - assign one to the other *)
                        if tvNonUnifiable typeAVar
                        (* If we have a nonunifiable type variable we want to assign
                           the typeB to  it. If the typeB is nonunifiable as well we
                           will get an error message. *)
                    then assign (typeBVar, tA)
                    else
                    let 
                        (* If they are both flexible records we first set the typeB
                           to the union of the records, and then set the typeA to
                           that. In that way we propagate properties such as
                           equality and level between the two variables. *)
                        val typBVal = tvValue typeBVar
                    in
                        case (typeAVal, typBVal) of
                            (LabelledType recA, LabelledType recB) =>
                            (
                                (* Turn these back into simple type variables to save
                                 checking the combined record against the originals
                                 when we make the assignment.
                                 (Would be safe but redundant). *)
                                tvSetValue (typeBVar, emptyType);
                                tvSetValue (typeAVar, emptyType);
                                assign (typeBVar,
                                      unifyRecords (recA, recB, typeAVal, typBVal));
                                assign (typeAVar, tB)
                            )
                        |   (OverloadSet{typeset=setA}, OverloadSet{typeset=setB}) =>
                            let
                                (* The lists aren't ordered so we just have to go
                                   through by hand. *)
                                fun intersect(_, []) = []
                                |   intersect(a, H::T) =
                                        if isInSet(H, a) then H::intersect(a, T) else intersect(a, T)
                                val newSet = intersect(setA, setB)
                            in
                                case newSet of
                                    [] => cantMatch (Atype, Btype, "(Incompatible overloadings)")
                                |   _ =>
                                    (
                                        tvSetValue (typeBVar, emptyType);
                                        tvSetValue (typeAVar, emptyType);
                                        (* I've changed this from OverloadSet{typeset=newset}
                                         to use mkOverloadSet.  The main reason was that it
                                         fixed a bug which resulted from a violation of the
                                         assumption that "equality" would not be passed an
                                         overload set except when pointed to by a type variable.
                                         It also removed the need for a separate test for
                                         singleton sets since mkOverloadSet deals with them.
                                         DCJM 1/9/00. *)
                                        assign (typeBVar, mkOverloadSet newSet);
                                        assign (typeAVar, tB)
                                    )
                            end
                        |   (EmptyType, _) => (* A is not a record or an overload set. *)
                                assign (typeAVar, tB)
                        |   (_, EmptyType) => (* A is a record but B isn't *)
                                assign (typeBVar, tA) (* typeB is ordinary type var. *)
                        |   _ => (* Bad combination of labelled record and overload set *)
                                cantMatch (Atype, Btype, "(Incompatible types)")
                    end
                end

            |   (TypeVar typeAVar, _) =>
                    (* typeB is not a type variable so set typeA to typeB.*)
                    (* Be careful if this is a non-unifiable type variable being matched to
                       the special case of the identity type-construction. *)
                (
                    if tvNonUnifiable typeAVar orelse (case tvValue typeAVar of OverloadSet _ => true | _ => false)
                    then
                    (
                        case tB of
                            TypeConstruction {constr as TypeConstrs{identifier=TypeId{idKind = TypeFn _, ...}, ...}, args, ...} =>
                                match(tA, eventual (makeEquivalent (constr, args)))
                        |  _ => assign (typeAVar, tB)
                    )
                    else assign (typeAVar, tB)
                )
     
            |   (_, TypeVar typeBVar) => (* and typeA is not *)
                (
                    (* We have to check for the special case of the identity type-construction. *)
                    if tvNonUnifiable typeBVar orelse (case tvValue typeBVar of OverloadSet _ => true | _ => false)
                    then
                        (
                        case tA of
                            TypeConstruction {constr as TypeConstrs{identifier=TypeId{idKind = TypeFn _, ...}, ...}, args, ...} =>
                                match(eventual (makeEquivalent (constr, args)), tB)
                        |  _ =>
                            (
                            case tB of
                                TypeVar tv =>
                                  (* This will fail if we are matching a signature because the
                                     typeB will be non-unifiable. *)
                                    assign (tv, tA) (* set typeB to typeA *)
                              | typB => match (tA, typB)
                            )
                        )
                    else
                        (
                        case tB of
                            TypeVar tv =>
                              (* This will fail if we are matching a signature because the
                                 typeB will be non-unifiable. *)
                                assign (tv, tA) (* set typeB to typeA *)
                          | typB => match (tA, typB)
                        )
                )

                (* Temporary - this can occur when matching a signature. Need to use free variables there. *)
            |   (BoundTypeVar (_, TVIndex iA), BoundTypeVar (_, TVIndex iB)) =>
                    (* For the moment just do this. *)
                    if iA = iB then () else cantMatch (tA, tB, "(Different bound type variables)")

            |   (BoundTypeVar _, _) => raise InternalError "match: bound variable in type"

            |   (_, BoundTypeVar _) => raise InternalError "match: bound variable in type"

                (* Type functions (type abbreviations).  We need to deal with these first because unwrapping
                   them could allow a valid match.  Try to avoid unwrapping unnecessarily. *)
            |   (TypeConstruction({constr = tACons as
                    TypeConstrs {identifier = TypeId{idKind = TypeFn{uid=uuidA, typeFunCount=tfCA, usedTvs, ...}, ...},...}, args=tAargs, ...}), 
                 TypeConstruction ({constr = tBCons as
                    TypeConstrs {identifier = TypeId{idKind = TypeFn{uid=uuidB, typeFunCount=tfCB, ...}, ...},...}, args=tBargs, ...})) =>
                    if uuidA = uuidB
                    then (* They are the same ID: simply match the used type variables. *)
                    let
                        fun matchArgs(a, b, n) = (if BoolVector.sub(usedTvs, n) then match(a, b) else (); n+1)
                        val _ = ListPair.foldl matchArgs 0 (tAargs, tBargs);
                    in
                        ()
                    end
                    else if tfCA >= tfCB
                        (* They are different: unwrap the one with the larger count.  It may refer to
                           the other one.  If they're equal we'll come back and unwrap the other later. *)
                    then match (makeEquivalent (tACons, tAargs), tB)
                    else match (tA, makeEquivalent (tBCons, tBargs))

            |   (TypeConstruction({constr = tACons as
                    TypeConstrs {identifier = TypeId{idKind = TypeFn _, ...},...}, args=tAargs, ...}), _) =>
                    (* We need to unwrap A. *)
                        match (makeEquivalent (tACons, tAargs), tB)

            |   (_,  TypeConstruction ({constr = tBCons as
                        TypeConstrs {identifier = TypeId{idKind = TypeFn _, ...},...}, args=tBargs, ...})) =>
                    match (tA, makeEquivalent (tBCons, tBargs))

            |   (TypeConstruction({constr = tACons as TypeConstrs{identifier=tcIdA, ...}, args=tAargs, ...}), 
                 TypeConstruction ({constr = tBCons as TypeConstrs{identifier=tcIdB, ...}, args=tBargs, ...})) =>
                (
                    if sameTypeId (tcIdA, tcIdB)
                    then
                    let (* Same type constructor - do the arguments match? *)
                        fun matchLists []      []    = ()
                          | matchLists (a::al) (b::bl) =
                          (  
                            match (a, b);
                            matchLists al bl
                          )
                          | matchLists _ _ = (* This should only happen as a result of
                                                a different error. *)
                                cantMatch (Atype, Btype, "(Different numbers of arguments)")
                    in
                            matchLists tAargs tBargs
                    end

                    (* When we have different type constructors, especially two with the same name,
                       we try to produce more information. *)
                    else matchError(TypeConstructorError(tA, tB, tACons, tBCons))
                )

            |   (OverloadSet {typeset}, TypeConstruction {constr=tBCons, args=tBargs, ...}) =>
                (* The candidate is an overloaded type and the target is a type
                   construction. *)
                (
                    (* See if the target type is among those in the overload set. *)
                        if null tBargs (* Must be a nullary type constructor. *)
                            andalso isInSet(tBCons, typeset)
                    then () (* ok. *)
                        (* Overload sets arise primarily with literals such as "1" and it's
                           most likely that the error is a mismatch between int and another
                           type rather than that the user assumed that the literal was
                           overloaded on a type it actually wasn't. *)
                    else
                    case preferredOverload typeset of
                        NONE => cantMatch (tA, tB, "(Different type constructors)")
                    |   SOME prefType =>
                        matchError(
                            TypeConstructorError(
                                mkTypeConstruction (tcName prefType, prefType,[], []),
                                tB, prefType, tBCons))
                )

            |   (TypeConstruction {constr=tACons, args=tAargs, ...}, OverloadSet {typeset}) =>
                (
                    (* We should never find an overload set as the target for a signature
                       match but it is perfectly possible for tB to be an overload set
                       when unifying two types.  *)
                    if null tAargs andalso isInSet(tACons, typeset)
                    then () (* ok. *)
                    else
                    case preferredOverload typeset of
                        NONE => cantMatch (tA, tB, "(Different type constructors)")
                    |   SOME prefType =>
                        matchError(
                            TypeConstructorError(
                                tA, mkTypeConstruction (tcName prefType, prefType,[], []),
                                tACons, prefType))
                )

            |   (OverloadSet _ , OverloadSet _) => raise InternalError "Unification: OverloadSet/OverloadSet"
                (* (OverloadSet , OverloadSet) should not occur because that should be
                 handled in the (TypeVar, TypeVar) case. *)

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
                    match (typAarg, typBarg);
                    match (typAres, typBres)
                )

                (* Explicit type variables.  They can only match themselves. *)
            |   (FreeTypeVar{uid=uidA, ...}, FreeTypeVar{uid=uidB, ...}) =>
                    if uidA = uidB then ()
                    else cantMatch (Atype, Btype, "(Different explicit type variables)")

            |   (FreeTypeVar _, _) =>
                    cantMatch (Atype, Btype, "(Type contains an explicit type variable)")

            |   (_, FreeTypeVar _) =>
                    cantMatch (Atype, Btype, "(Type contains an explicit type variable)")

            |   (EmptyType, EmptyType) => ()
                    (* This occurs only with exceptions - empty means no argument *)

            |   (LabelledType recA, LabelledType recB) =>
                    (* Unify the records, but discard the result because at least one of the
                     records is frozen. *)
                    (unifyRecords (recA, recB, tA, tB); ())
        
            |   _ => cantMatch (tA, tB, "(Incompatible types)")

        end (* match *)

    in
        match (Atype, Btype);
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
                    display (alpha, 1000 (* As deep as necessary *), alphaTypeEnv),
                    PrettyBreak (1, 0),
                    PrettyString "to",
                    PrettyBreak (1, 0),
                    display (beta, 1000 (* As deep as necessary *), betaTypeEnv),
                    PrettyBreak (1, 0),
                    PrettyString reason                        
                ])

        |   reportError(TypeConstructorError(alpha: types, beta: types, alphaCons, betaCons)) =
            let
                fun expandedTypeConstr(ty, tyEnv, tyCons as TypeConstrs{identifier=TypeId { description, ...}, ...}) =
                let
                    fun lastPart name = #second(splitString name)

                    (* Print the type which includes the type constructor name with as
                       much additional information as we can. *)
                    fun printWithDesc{ location, name, description } =
                        PrettyBlock(3, false, [],
                            [ display (ty, 1000, tyEnv) ]
                            @ (if lastPart name = lastPart(tcName tyCons) then []
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
                        expandedTypeConstr(alpha, alphaTypeEnv, alphaCons),
                        PrettyBreak (1, 0),
                        PrettyString (if what = "unify" then "with" else "to"),
                        PrettyBreak (1, 0),
                        expandedTypeConstr(beta, betaTypeEnv, betaCons),
                        PrettyBreak (1, 0),
                        PrettyString "(Different type constructors)"                        
                    ])
            end
            
    in
        reportError
    end

  (* Given a function type returns the first argument if the
     function takes a tuple otherwise returns the only argument.
     Extended to include the case where the argument is not a function
     in order to work properly for overloaded literals. *)
  fun firstArg(FunctionType{arg=
        LabelledType { recList = {typeof, ...} ::_, ...}, ...}) =
            eventual typeof
   |  firstArg(FunctionType{arg, ...}) = eventual arg
   |  firstArg t = t

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
                    tDisp(result, depth-1, typeV, typeEnv, sigMap)
                ]
                )
        end

    |   displayTypeConstrsWithMap (TypeConstrSet(tCons as TypeConstrs{identifier, ...}, [] (* No constructors *)), depth, _, _) =
        (* Abstract type or type in a signature. *)
        if depth <= 0 
        then PrettyString "..."
        else PrettyBlock (3, false, [],
            PrettyString (
                if isEquality identifier then "eqtype" else "type") ::
            PrettyBreak (1, 0) ::
            printTypeVars (List.tabulate(tcArity tCons,  createBoundVar false), depth, varNameSequence ()) @
            [PrettyString (#second(splitString(tcName tCons)))]
            )

    |   displayTypeConstrsWithMap (TypeConstrSet(tCons as TypeConstrs{name, locations, ...}, tcConstructors), depth, typeEnv, sigMap) =
            (* It has constructors - datatype declaration *)
        if depth <= 0 
        then PrettyString "..."
        else
        let
            val typeV = varNameSequence ()
            (* Construct a ('a, 'b, 'c) tyCons construction for the result types
               of each of the constructors. *)
            val typeVars = List.tabulate(tcArity tCons,  createBoundVar false)
            val typeResult = mkTypeConstruction(name, tCons, typeVars, locations)

            (* Print a single constructor (blocked) *)
            fun pValConstr (first, name, typeOf, depth) =
            let
                val (t, _) = generalise typeOf
                val firstBreak = PrettyBreak (1, if first then 2 else 0)
            in
                case t of
                    FunctionType { arg, result} =>
                    let
                        (* Constructor with an argument.  The constructor "type" is the argument.
                           We have to unify the result type of the function with the
                           ('a, 'b, 'c) tyCons type so that we get the correct type variables
                           in the argument.  We just print the argument of the function. *)
                        val _ = unifyTypes(result, typeResult)
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
                                tDisp (arg, depth - 1, typeV, typeEnv, sigMap)
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
                                [ PrettyString(#second(splitString(tcName tCons))), PrettyBreak(1, 0), PrettyString "=" ]
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
    fun typeConstrFromOverload(f, _) =
    let
    fun prefType(TypeVar tvar) =
            ( (* If we still have an overload set that's because it has
                 not reduced to a single type.  In ML 97 we default to
                 int, real, word, char or string in that order.  This
                 works correctly for overloading literals so long as
                 the literal conversion functions are correctly installed. *)
            case tvValue tvar of
                OverloadSet{typeset} =>
                    let
                        (* If we accept this type we have to freeze the
                            overloading to this type.
                            I'm not happy about doing this here but it
                            seems the easiest solution. *)
                        fun freezeType tcons =
                            (
                            tvSetValue(tvar,
                                mkTypeConstruction(tcName tcons, tcons, [], []));
                            tcons
                            )
                    in
                        case preferredOverload typeset of
                            SOME tycons => freezeType tycons
                        |   NONE => raise InternalError "typeConstrFromOverload: No matching type"
                    end
              | _ => raise InternalError "typeConstrFromOverload: No matching type" (* Unbound or flexible record. *)
            )
     |  prefType(TypeConstruction{constr as TypeConstrs {identifier = TypeId{idKind = TypeFn _, ...},...}, args, ...}) =
            prefType (makeEquivalent (constr, args))
     |  prefType(TypeConstruction{constr, ...}) = constr
     |  prefType _ = raise InternalError "typeConstrFromOverload: No matching type"
    in
        prefType(firstArg(eventual f))
    end;

    (* Return the result type of a function.  Also used to test if the value is
       a function type. *)
    fun getFnArgType t =
    case eventual t of
        FunctionType {arg, ... } => SOME arg
    |   _ => NONE
    

  (* Called at the end of a val or fun binding.
     There are various actions here depending on whether the expression is non-expansive
     and whether this is the top level.
     The current version side-effects type variables by adding on a generalisable type
     variable at the end of the chain if the type variable can be generalised.
     *)
    fun allowGeneralisation (t, level, checkOverloadFlex, giveError) =
    let
        (* Map from type variables to bound variables we have already made. *)
        val typeVarMapList: (typeVar * tvIndex) list ref = ref nil
        val freeTypeVarMapList: (uniqueId * tvIndex) list ref = ref nil

        (* Perform checks and return the generic version with the templates. *)
        fun createGeneric(t, templates: typeVarTemplate list) =
        case eventual t of
            typ as TypeVar tvar =>
            (
                case tvValue tvar of
                    OverloadSet {typeset, ...} =>
                    if checkOverloadFlex
                    then (* Narrow context for overloading: select "preferred" type. *)
                    (
                        case preferredOverload typeset of
                            SOME (constr as TypeConstrs {name, locations, ...}) =>
                                (TypeConstruction{constr=constr, args=[], locations=locations, name=name}, templates)

                        |   NONE => raise InternalError "general: No matching type"
                    )
                    else (* Normal setting: find overloading from full context. For the moment just return this type
                            variable containing the current overload set. *)
                        (typ, templates)

                |   LabelledType{recList, fullList} => (* Unresolved flexible record. *)
                    let
                        (* If we're using the narrow context it should have been resolved. *)
                        val () = if checkOverloadFlex then giveError("Type is an unresolved flexible record") else ()
                        (* Copy the fields that have been used as with frozen labelled records. *)
                        fun copyRecordEntry({name, typeof}, (fields, templs)) =
                        let
                            val (copiedTyp, newtempls) = createGeneric(typeof, templs)
                        in
                            ({name=name, typeof=copiedTyp} :: fields, newtempls)
                        end
                        val (copiedList, resTempls) = List.foldl copyRecordEntry ([], templates) recList
                        val newRecord = { recList=List.rev copiedList, fullList=fullList }
                    in
                        (BoundFlexRecord(TVIndex(List.length resTempls), newRecord),
                            TemplPlain { equality=tvEquality tvar } :: resTempls)
                    end

                |   _ =>
                    (
                        if (case tvLevel tvar of NotGeneralisable v => v < level | Generalisable => false)
                        then (* Explicit type variable at an outer level.  Same whether expansive or not.
                                For the moment just return this type variable. *)
                            (typ, templates)

                        else (* If we've already made a bound variable here we need to reuse it. *)
                        (
                            case List.find(fn (tv, _) => sameTv(tv, tvar)) (!typeVarMapList) of
                                NONE =>
                                let
                                    val newIndex = TVIndex(List.length templates)
                                    val () = typeVarMapList := (tvar, newIndex) :: (!typeVarMapList)
                                in
                                    (BoundTypeVar("'a", newIndex),
                                        TemplPlain { equality=tvEquality tvar } :: templates)
                                end
                            |   SOME(_, index) => (BoundTypeVar("'a", index), templates)
                        )
                    )
            )

        |   typ as FreeTypeVar{name, equality, level=fLevel, uid, ...} =>
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
                            (BoundTypeVar(name, newIndex), TemplPlain { equality=equality } :: templates)
                        end
                    |   SOME(_, index) => (BoundTypeVar(name, index), templates)
                )            

        |   BoundTypeVar _ => raise InternalError "createGeneric: already bound"
        |   BoundFlexRecord _ => raise InternalError "createGeneric: already bound"

        |   TypeConstruction {constr, args, locations, name} =>
            let
                fun copyEntry(ty, (resList, templs: typeVarTemplate list, n)) =
                let
                    val (copiedTyp, newtempls) = createGeneric(ty, templs)
                in
                    (copiedTyp :: resList, newtempls, n+1)
                end
                val (copiedArgs, resTemplates, _) = List.foldl copyEntry ([], templates, 0) args
            in
                (TypeConstruction{constr=constr, args=List.rev copiedArgs, locations=locations, name=name}, resTemplates)
            end

        |   FunctionType {arg, result} =>
            let
                val (copiedArg, argTemplates) = createGeneric(arg, templates)
                val (copiedRes, resTemplates) = createGeneric(result, argTemplates)
            in
                (FunctionType {arg = copiedArg, result = copiedRes}, resTemplates)
            end

        |   LabelledType ({recList, fullList}) =>
                (* Rigid labelled records only.  Flexible ones are treated as type vars. *)
            let
                fun copyRecordEntry({name, typeof}, (fields, templs)) =
                let
                    val (copiedTyp, newtempls) = createGeneric(typeof, templs)
                in
                    ({name=name, typeof=copiedTyp} :: fields, newtempls)
                end
                val (copiedList, resTempls) = List.foldl copyRecordEntry ([], templates) recList
            in
                (LabelledType {recList = List.rev copiedList, fullList = fullList}, resTempls)
            end

        |   EmptyType => raise InternalError "createGeneric: EmptyType"

        |   BadType => (BadType, templates)

        |   OverloadSet _ => raise InternalError "createGeneric: OverloadSet"

        val (newType, newTemplates) = createGeneric(t, [])
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
                TypeVar tvar =>
                    (* Explicit type variables at outer levels are allowed but not at this level. *)
                    if v then true
                    else if (case tvLevel tvar of NotGeneralisable v => v < level | Generalisable => false) then false
                    else tvNonUnifiable tvar

                (* Explicit type variables at outer levels are allowed but not at this level. *)
            |   FreeTypeVar{level=fLev, ...} => v orelse fLev >= level

                (* If this is a type function we mustn't check type variables that aren't actually used. *)
            |   TypeConstruction {args, constr=TypeConstrs{identifier=TypeId{idKind=TypeFn{usedTvs, ...}, ...}, ...}, ...} =>
                    #1 (List.foldl(fn (t, (v, n)) => (if BoolVector.sub(usedTvs, n) then checkFree t v else v, n+1)) (v, 0) args)

            |   TypeConstruction {args, ...} => (* Then process the arguments. *)
                    List.foldr (fn (t, v) => checkFree t v) v args
           
            |   FunctionType {arg, result} => checkFree arg (checkFree result v)
    
            |   LabelledType {recList,...} =>
                    List.foldr (fn ({ typeof, ... }, v) => checkFree typeof v) v recList

            |   BoundFlexRecord(_, {recList, ...}) =>
                    List.foldr (fn ({ typeof, ... }, v) => checkFree typeof v) v recList

            |   BoundTypeVar _ => v
            |   BadType => v
            |   EmptyType => v
            |   OverloadSet _ => v
        end
    in
        checkFree t false
    end
    

  (* Check for free type variables at the top level.  Added for ML97.  This replaces the
     test in allowGeneralisation above and is applied to all top-level
     values including those in structures and functors.  *)
  (* I've changed this from giving an error message, which prevented the
     code from evaluating, to giving a warning and setting the type
     variables to unique type variables.  That allows, for example,
     fun f x = raise x; f Subscript; to work.  DCJM 8/3/01. *)
  fun checkForFreeTypeVariables(valName: string, ty: types, lex: lexan, printAndEqCode) : unit =
  let
    (* Generate new names for the type variables. *)
    val count = ref 0
    fun genName num =
        (if num >= 26 then genName (num div 26 - 1) else "")
        ^ String.str (Char.chr (num mod 26 + Char.ord #"a"));

    fun checkTypes (TypeVar tvar) () =
        if isEmpty(tvValue tvar) andalso (case tvLevel tvar of Generalisable => false | NotGeneralisable _ => true)
        then (* The type variable is unbound (specifically, not
                an overload set) and it is not generic i.e. it
                must have come from an expansive expression. *)
            let
                val name = "_" ^ genName(!count)
                val _ = count := !count + 1;
                val declLoc = location lex (* Not correct but OK for the moment. *)
                val declDescription =
                    { location = declLoc, name = name, description = "Constructed from a free type variable." }
                val tCons =
                    makeTypeConstructor (name,
                        makeFreeId(0, Global(printAndEqCode()), tvEquality tvar, declDescription),
                        [DeclaredAt declLoc]);
                val newVal = mkTypeConstruction(name, tCons, [], [])
            in
                warningMessage(lex, location lex, 
                    concat["The type of (", valName,
                        ") contains a free type variable. Setting it to a unique monotype."]);
                tvSetValue (tvar, newVal)
            end
        else ()
     |  checkTypes _ () = ()

  in
      foldType checkTypes ty ();
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
        fun getFieldList(LabelledType rcrd) = recordFields rcrd
        |   getFieldList(TypeVar tvar) = getFieldList(tvValue tvar)
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
        fun getCodetreeType (LabelledType(rcrd as {recList, ...})) =
            (* If it's "unit" return a single general type.  If the record has not
               been frozen it's an error reported elsewhere.  It could be a flexible
               record that is frozen in which case we just pass the address of the
               whole record. *)
            if List.length recList = 0 orelse not(recordIsFrozen rcrd) orelse
                    List.length recList <> List.length (recordFields rcrd)
            then [GeneralType]
            else
            let
                fun getType{typeof, ...} =
                    case getCodetreeType typeof of [single] => single | _ => GeneralType
            in
                List.map getType recList
            end

        |   getCodetreeType (TypeVar tvar) = getCodetreeType (tvValue tvar)

        |   getCodetreeType (TypeConstruction{
                    constr as TypeConstrs{identifier=TypeId{idKind=TypeFn _, ...}, ...}, args, ...}) =
                getCodetreeType (makeEquivalent(constr, args))

        |   getCodetreeType (TypeConstruction{constr, args=[], ...}) = [isFloatId constr]

        |   getCodetreeType(OverloadSet {typeset, ...}) =
            (
                case preferredOverload typeset of
                    SOME t => [isFloatId t] (* real only.  float is never preferred. *)
                |   NONE => [GeneralType]
            )

        |   getCodetreeType _ = [GeneralType]
    end

    fun checkDiscard(t: types, lex: lexan): string option =
    let
        open DEBUG
        val checkLevel = getParameter reportDiscardedValuesTag (debugParams lex)

        fun isUnit(LabelledType{recList=[], ...}) = true (* Unit is actually an empty record *)
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
            1 => if isAFunction (eventual t) then SOME "A function value is being discarded." else NONE
        |   2 => if isUnit (eventual t) then NONE else SOME "A non unit value is being discarded."
        |   _ => NONE
    end

    structure Sharing =
    struct
        type types      = types
        and  values     = values
        and  typeId     = typeId
        and  structVals = structVals
        and  typeConstrs= typeConstrs
        and  typeConstrSet=typeConstrSet
        and  typeParsetree = typeParsetree
        and  parseTypeVar   = parseTypeVar
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
    end
end;
