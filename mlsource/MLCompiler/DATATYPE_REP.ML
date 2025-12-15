(*
    Copyright (c) 2009, 2013, 2015-16, 2020-21, 2025 David C.J. Matthews

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
    Title:      Operations on global and local values.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1986
*)

functor DATATYPE_REP (

structure CODETREE : CODETREE
structure STRUCTVALS : STRUCTVALSIG;
structure TYPESTRUCT : TYPETREESIG

structure MISC :
sig
  exception InternalError of string; (* compiler error *)
  
  val quickSort : ('a -> 'a -> bool) -> 'a list -> 'a list
end;

structure ADDRESS : AddressSig
structure COPIER: COPIERSIG
structure TYPEIDCODE: TYPEIDCODESIG

sharing STRUCTVALS.Sharing = TYPESTRUCT.Sharing
      = COPIER.Sharing = CODETREE.Sharing = ADDRESS = MISC
      = TYPEIDCODE.Sharing

) : DATATYPEREPSIG =

struct
    open MISC; 

    open CODETREE;
    open TYPESTRUCT; (* Open this first because unitType is in STRUCTVALS as well. *)
    open Universal; (* for tag etc. *)
    open STRUCTVALS;
    open ADDRESS;
    open TYPEIDCODE
    open COPIER

    val length   = List.length;

    val orb = Word8.orb
    infix 7 orb;

    (* These are the possible representations of a value constructor. *)
    datatype representations = 
        RefForm            (* As for OnlyOne but must be a monotype. *)
    |   UnitForm           (* If the only value in an enumeration. *)
    |   OnlyOne            (* If only one constructor, there is no tag or box. *)
                       (* Could be replaced by "UnboxedForm"? *)
    
    |   EnumForm of { tag: word, maxTag: word }    (* Enumeration - argument is the number. *)
    |   ShortForm of word  (* As EnumForm except that one datatype is BoxedForm. *)
    |   BoxedForm          (* Boxed but not tagged (only unary constructor) *)
    |   UnboxedForm of int (* Unboxed and untagged (only unary constructor) *)

    |   ConstForm of { value: machineWord, maxTag: word}   (* Constant - argument is a tagged value. *)
    |   TaggedBox of { tag: word, maxTag: word }          (* Union - tagged and boxed.  i.e. the representation is a
                                 pair whose first word is the tag and second is the value. *)
    |   TaggedTuple of { tag: word, maxTag: word, size: int }
                                (* Union - tagged but with in-line tuple. i.e. for a
                                   tuple of size n the representation is a tuple of size n+1
                                 whose first word contains the tag. *)


    val arg1     = mkLoadArgument 0 (* saves a lot of garbage *)

    val mutableFlags = F_words orb F_mutable;

    (* allocate 1 mutable word, initialise to "v" *)
    fun refApplyCode v =
        mkAllocateWordMemory(mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags), v)

    local
        fun mkTag (tag:word) : codetree = mkConst (toMachineWord tag);

        (* How to apply the constructor at run-time or, if, it's a constant make it now. *)
        fun constrApply (test: representations, arg) : codetree =
        let
            fun tagTupleApplyCode (tag, n, arg) =
                mkEval (mkInlproc (mkDatatype (mkTag tag :: List.tabulate(n, fn i => mkInd(i, arg1))), 1, "", [], 0), [arg])
            (* Even though unboxed tuples (e.g. list cells) are the same as tuples we need to add
               this extra step so that the result is a variant tuple i.e. the optimiser can tell
               that this may not always be a tuple. *)
            fun tupleApplyCode (n, arg) =
                mkEval (mkInlproc (mkDatatype (List.tabulate(n, fn i => mkInd(i, arg1))), 1, "", [], 0), [arg])
        in
            case test of
                UnboxedForm 0       => arg (* Function - never detupled. *)
            |   UnboxedForm size    => tupleApplyCode(size, arg)
            |   BoxedForm            => mkDatatype [arg]
            |   RefForm              => refApplyCode arg
            |   TaggedBox{tag, ...}  => mkDatatype [mkTag tag, arg]
            |   TaggedTuple{tag, size, ...} => tagTupleApplyCode(tag, size, arg)
            |   OnlyOne              => arg
            |   ConstForm{ value, ...} => mkConst value (* tagged value. *)
            |   EnumForm{tag, ...}   => mkConst (toMachineWord tag)
            |   ShortForm tag        => mkConst (toMachineWord tag)
            |   UnitForm             => CodeZero
        end

        (* The run-time test whether a value matches a constructor. *)
        fun constrMatch (test: representations, value:codetree) : codetree =
        let
            fun testTagOf(tag, maxTag, v) = mkTagTest (v, tag, maxTag)
      
            fun testBoxedTagOf (tag, maxTag, v) =
                testTagOf (tag, maxTag, mkInd (0, v))
                (* Tag is first field.  It is always present and is always the tag so
                   we can use mkInd here rather than mkVarField. *)

            val testBoxed = mkNot o mkIsShort (* not (isShort v) *)

            (* get the tag from a TaggedBox or ConstForm *)
            fun loadTag (u: machineWord) : machineWord = 
                loadWord (toAddress u, 0w0); (* tag is first field *)
        in
            case test of
                UnboxedForm _        => testBoxed value
            |   BoxedForm            => testBoxed value
            |   RefForm              => CodeTrue
            |   EnumForm{tag, maxTag}   => testTagOf(tag, maxTag, value)
            |   ShortForm tag        => mkEqualPointerOrWord(mkTag tag, value) (* Could be an address. *)
            |   TaggedBox{tag, maxTag}  => testBoxedTagOf(tag, maxTag, value)
            |   TaggedTuple{tag, maxTag, ...} => testBoxedTagOf(tag, maxTag, value)
            |   ConstForm{value=c, maxTag} => testBoxedTagOf(toShort (loadTag c), maxTag, value)
            |   OnlyOne              => CodeTrue
            |   UnitForm             => CodeTrue
        end

      (* The run-time code to destruct a construction. *)
      (* shouldn't the CodeZero's raise an exception instead?
         No, because I think there are circumstances in which the destructor
         code is created even for nullary constructors. *)
        fun constrDestruct (test: representations, value: codetree) : codetree =
        let
            (* Copy out the fields and build a tuple.  Used either if we
               have a tagged tuple (offset 1) or a tuple that does not
               need boxing (because we need to use mkVarField to extract
               the fields).  *)
            fun tupleDestructCode (n, arg, offset) =
                mkEval (
                    mkInlproc (mkTuple (List.tabulate(n, fn i => mkVarField(i+offset, arg1))), 1, "", [], 0),
                    [arg])

            (* Use loadWord not indirect because the optimiser reorders indirections. *)
            fun refDestructCode v = mkLoadOperation(LoadStoreMLWord{isImmutable=false}, v, CodeZero)
        in
            case test of
                UnboxedForm 0         => value (* Function - never detupled. *)
            |   UnboxedForm size      => tupleDestructCode(size, value, 0)
            |   BoxedForm            => mkVarField (0, value)
            |   RefForm              => refDestructCode value
            |   TaggedBox _         => mkVarField (1, value) (* contents is second field of record *)
            |   TaggedTuple { size, ...} => tupleDestructCode(size, value, 1)
            |   OnlyOne              => value
            |   EnumForm _         => CodeZero (* To keep optimiser happy. *)
            |   ShortForm _         => CodeZero (* To keep optimiser happy. *)
            |   ConstForm _          => CodeZero (* (rather than raising an exception) *)
            |   UnitForm             => CodeZero
        end

        open ValueConstructor
    in
     (* Constructors are now represented as run-time values.  A nullary constructor is
        a pair consisting of a test function and the constructor value.  A unary
        constructor is a triple: a test function, an injection function and a
        projection function.
        The above applies to monotypes.  If this is a polytype each of these is
        actually a function from the base type values to the functions. *)

        fun createNullaryConstructor (test, name): codetree =
        let
            val numTypes = 0
            val testFn = mkInlproc(constrMatch(test, arg1), 1, name, [], 0) (* Test function. *)
            and constrVal = constrApply(test, CodeZero) (* Value. *)
        in
            if numTypes = 0
            then createNullaryConstr{ testMatch = testFn, constrValue = constrVal }
            else createNullaryConstr{
                    testMatch = mkInlproc(testFn, numTypes, name, [], 0),
                    constrValue = mkInlproc(constrVal, numTypes, name, [], 0)}
        end
    
        fun createUnaryConstructor(test: representations, name: string): codetree =
        let
            val numTypes = 0
            val testMatch = mkInlproc(constrMatch(test, arg1), 1, name, [], 0) (* Test function. *)
            and injectValue = mkInlproc(constrApply(test, arg1), 1, name, [], 0) (* Injection function. *)
            and projectValue = mkInlproc(constrDestruct(test, arg1), 1, name, [], 0) (* Projection function. *)
        in
            if numTypes = 0
            then createValueConstr{testMatch = testMatch, injectValue = injectValue, projectValue = projectValue }
            else createValueConstr{
                    testMatch = mkInlproc(testMatch, numTypes, name, [], 0),
                    injectValue = mkInlproc(injectValue, numTypes, name, [], 0),
                    projectValue = mkInlproc(projectValue, numTypes, name, [], 0)}
        end
    end

(* 
     RefForm is used for "ref" (only). We use various representations
     of datatype / abstype constructors.
   
     Nullary constructors are represented as:
        UnitForm     (if it's the only constructor in the datatype)
        EnumForm     (if all the constructors are nullary)
        ShortForm     (if there's one non-nullary constructor)
        ConstForm    (otherwise)
    
     Unary constructors are represented as:
        OnlyOne      (if it's the only constructor in the datatype)
        UnboxedForm  (if it's the only unary constructor and the argument is always a tuple e.g. list)
        TaggedTuple  (if it's not the only unary constructor, applied to a tuple)
        BoxedForm    (if it's the only unary constructor and the argument may not be a tuple e.g. SOME)
        TaggedBox    (otherwise)
      
     Note that we use ConstForm, not EnumForm, for nullary constructors
     when the unary constructors are represented as TaggedTuple/TaggedBox
     because that allows the TaggedBox test to be:
       
         fn w => wordEq (loadWord (w,0), tag)
     
     rather than:
     
         fn w => not (isShort w) andalso wordEq (loadWord (w,0), tag)
     
     Note that EnumForm and ShortForm differ in that the tests for EnumForm use
     mkTagTest which can be converted into an indexed case.  This can't be
     used for ShortForm because the values for the datatype include addresses. 
*)
  datatype constructorKind =
    Nullary             (* a nullary constructor *)
  | UnaryGeneric        (* a normal unary constructor *)
  | UnaryFunction       (* unary constructor applied to a function *)
  | UnaryTupled of int  (* a unary constructor applied to a tuple of size n  *)
  ;

    fun getTupleKind(LabelledType {recList = [{typeof, ...}], ...}) =
        (* We cannot have flexible records here.   All the fields must be listed. *)
            (* Singleton records are always represented simply by the value. *)
          getTupleKind typeof
      
    |   getTupleKind(LabelledType {recList, ...}) = UnaryTupled (length recList)
      
    |   getTupleKind(FunctionType _) =  UnaryFunction
    
    |   getTupleKind(TypeConstruction {constr as TypeConstrs {identifier = TypeId{idKind = TypeFn _, ...},...}, args, ...}) =
            (* Type function *)
            getTupleKind (makeEquivalent(constr, args))
    
    |   getTupleKind _ = UnaryGeneric  (* Anything else *)

    (* This now creates the functions as well as choosing the representation. *)
    (* N.B. The representation for the "context" and "pretty" datatypes is defined in Pretty.sml.
       Any changes here that may affect the representation of a datatype may require
       changes there as well. *)
    fun chooseConstrRepr(cs, arity) =
    let
        fun checkArgKind (name, EmptyType) = (Nullary, name)
        |   checkArgKind (name, argType) = (getTupleKind argType, name)
        val kinds = map checkArgKind cs;

        fun chooseRepr [(Nullary, name)]       = [createNullaryConstructor(UnitForm, name)]
        |  chooseRepr [(UnaryGeneric, name)]  = [createUnaryConstructor(OnlyOne, name)]
        |  chooseRepr [(UnaryFunction, name)] = [createUnaryConstructor(OnlyOne, name)]
        |  chooseRepr [(UnaryTupled _, name)] = [createUnaryConstructor(OnlyOne, name)]
        |  chooseRepr l =
            let
                val unaryCount = List.foldl(fn((Nullary, _), n) => n | (_,n) => n+1) 0 l
            in
                case unaryCount of
                    0 => (* All are nullary. *)
                    let
                        val maxTag = Word.fromInt(List.length l)-0w1 (* Largest no is length-1 *)
                        fun createRepr(_, []) = []
                        |   createRepr(n, (_, name) :: t) =
                                createNullaryConstructor(EnumForm{tag=n, maxTag=maxTag}, name) :: createRepr (n + 0w1, t)
                    in
                        createRepr(0w0, l)
                    end

                |   1 =>
                    let
                        (* We use this version if all the constructors are nullary (i.e. constants)
                           except one.  The unary constructor is represented
                           by the boxed value and the nullary constructors by untagged integers. *)
                        (* Note that "UnaryTupled 0" (which would arise as a result of a declaration of the
                           form  datatype t = A of () | ... ) can't be represented as "UnboxedForm"
                          because "{}" is represented as a short (unboxed) integer. *)
                        fun chooseOptimisedRepr1(_, _, []) = []

                        |   chooseOptimisedRepr1(n, tvs, (Nullary, name) :: t) =
                                 createNullaryConstructor(ShortForm n, name) :: chooseOptimisedRepr1 (n + 0w1, tvs, t)

                        |   chooseOptimisedRepr1(n, tvs, (UnaryGeneric, name) :: t) =
                                   createUnaryConstructor(BoxedForm, name) :: chooseOptimisedRepr1(n, tvs, t)

                        |   chooseOptimisedRepr1(n, tvs, (UnaryFunction, name) :: t) =
                                   createUnaryConstructor(UnboxedForm 0, name)  :: chooseOptimisedRepr1(n, tvs, t)

                        |   chooseOptimisedRepr1(n, tvs, (UnaryTupled 0, name) :: t) =
                                   createUnaryConstructor(BoxedForm, name)    :: chooseOptimisedRepr1(n, tvs, t)

                        |   chooseOptimisedRepr1(n, tvs, (UnaryTupled s, name) :: t) = 
                                   createUnaryConstructor(UnboxedForm s, name)  :: chooseOptimisedRepr1(n, tvs, t)

                    in
                        chooseOptimisedRepr1(0w0, arity, l) (* can save the box *)
                    end

                |   _ =>
                    let
                        (* We use this version there's more than 1 unary constructor. *)
                        (* With this representation constructors of small tuples make tuples of
                         size n+1 whose first word is the tag.  Nullary constructors are represented
                         by single word objects containing the tag. *)
                        val maxTag = Word.fromInt(List.length l) - 0w1 (* Largest no is length - 1 *)

                        fun chooseOptimisedRepr2(_, _, []) = []
                        |   chooseOptimisedRepr2(n, tvs, h :: t) = 
                            let
                                val repr = 
                                    case h of
                                        (Nullary,       name) =>
                                        let
                                            (* Make an object with the appropriate tag. Doing it here means we
                                             only do it once for this object. *)
                                            fun genConstForm (n :word) : representations =
                                            let
                                                val vec : address = allocWordData (0w1, F_words, toMachineWord n)
                                                (* The new call does not require locking but the old code still
                                                   sets the F_mutable bit. *)
                                                val _ = if isMutable vec then lock vec else ()
                                            in
                                                ConstForm{value=toMachineWord vec, maxTag=maxTag}
                                            end
                                        in
                                            createNullaryConstructor(genConstForm n, name)
                                        end
                                            
                                    |   (UnaryGeneric,  name) => createUnaryConstructor(TaggedBox{tag=n, maxTag=maxTag}, name)
                                    |   (UnaryFunction, name) => createUnaryConstructor(TaggedBox{tag=n, maxTag=maxTag}, name)
                                    |   (UnaryTupled i, name) =>
                                            createUnaryConstructor(
                                                if i <= 4 (*!maxPacking*)
                                                then TaggedTuple {tag=n, size=i, maxTag=maxTag}
                                                else TaggedBox{tag=n, maxTag=maxTag}, name)
                            in
                                repr :: chooseOptimisedRepr2(n + 0w1, tvs, t)
                            end;
                    in
                        chooseOptimisedRepr2(0w0, arity, l) (* can use tagged tuples *)
                    end
            end
    in
        { constrs = chooseRepr kinds } 
    end;
 
   (* RefForm, NilForm and ConsForm are only used for built-in types *)


(*****************************************************************************)
(*             Standard values and exceptions.                               *)
(*****************************************************************************)
    fun makeTypeConstructor (name, uid, locations) = TypeConstrs { name = name, identifier = uid, locations = locations }

    (* Build a datatype within the basis. *)
    fun buildBasisDatatype(tcName, tIdPath, arity, isEqType: bool,
            mkValConstrs: typeConstrs -> (values * codetree) list) =
    let
        (* Create a temporary datatype.  The "name" we put in here is usually the
           same as the type constructor name except for datatypes in the PolyML structure
           which have the PolyML prefix. *)
        val description = basisDescription tIdPath
        val id =
            makeBoundId(arity, Local{addr = ref ~1, level = ref baseLevel}, 0 (* IdNumber*),
                isEqType, true, description)
        val dtype = makeTypeConstructor (tcName, id, [DeclaredAt inBasis]);
        (* Build the constructors. *)
        val valConstrsAndDecs = mkValConstrs dtype
        (* The constructors have to be ordered as in genValueConstrs in PARSE_TREE. *)
        fun leq (Value{name=xname, ...}, _) (Value{name=yname, ...}, _) = xname < yname;
        val sortedConstrs = quickSort leq valConstrsAndDecs;
        val initialTypeSet = TypeConstrSet(dtype, (List.map #1 valConstrsAndDecs))
        val addrs = ref 0
        fun mkAddrs n = ! addrs before (addrs := !addrs+n)

        fun declConstr(Value{access=Local{addr, level}, ...}, repr) =
            let
                val newAddr = mkAddrs 1
            in
                addr := newAddr;
                level := baseLevel;
                (mkDec(newAddr, repr), mkLoadLocal newAddr)
            end
        |   declConstr _ = raise InternalError "declConstr: not local"
        val (declConstrs, loadConstrs) = ListPair.unzip(List.map declConstr sortedConstrs)

        (* Create the datatype.  Sets the address of the local in "id". *)
        val dtCode =
            createDatatypeFunctions(
                [{typeConstr=initialTypeSet, eqStatus=isEqType}], mkAddrs, baseLevel, true)

        (* Compile and execute the code to build the functions and extract the result. *)
        val globalCode =
            genCode(
                mkEnv(declConstrs @ dtCode, mkTuple(codeId(id, baseLevel) :: loadConstrs)),
                [], !addrs)()
        val newId = makeFreeId(arity, Global(mkInd(0, globalCode)), isEqType, description)
        
        (* Get the value constructors out as globals. *)
        fun mkGlobal((Value{name, typeOf, class, locations,  ...}, _), (decs, offset)) =
            (decs @ [Value{name=name, typeOf=typeOf, class=class, locations=locations, references=NONE,
                     instanceTypes=NONE, access=Global(mkInd(offset, globalCode))}], offset+1)
        val (gConstrs, _) = List.foldl mkGlobal ([], 1 (* Offset 0 is the type ID *)) sortedConstrs

        (* Finally copy the datatype to put in the code. *)
    in
        fullCopyDatatype(TypeConstrSet(dtype, gConstrs), fn 0 => newId | _ => raise Subscript, "")
    end

  
    fun makeValueConstr (name, typeOf, nullary, constrs, access, locations) =
        Value
        { 
          name    = name,
          typeOf  = typeOf,
          access  = access,
          class   = Constructor { nullary = nullary, ofConstrs = constrs },
          locations = locations,
          references = NONE,
          instanceTypes = NONE
        }


    (* Nil and :: are used in parsetree for lists constructed
       using [ ... ] and are also used for initialisation. *)
    local
        fun makeConsAndNil listType =
        let
            val alpha         = BoundTypeVar("'a", TVIndex 0)
            val alphaList     = mkTypeConstruction ("list", listType, [alpha], [DeclaredAt inBasis]);
            val consType      = mkFunctionType (mkProductType [alpha, alphaList], alphaList)
            val listTemplate  = [TemplPlain{equality=false }]
            val nilConstructor  =
                makeValueConstr ("nil", ValueType(alphaList, listTemplate), true, 2, Local{addr=ref ~1, level=ref baseLevel}, [DeclaredAt inBasis])
            val consConstructor =
                makeValueConstr ("::",  ValueType(consType, listTemplate),  false, 2, Local{addr=ref ~1, level=ref baseLevel}, [DeclaredAt inBasis])
            val nilRepresentation = createNullaryConstructor(ShortForm 0w0, "nil")
            val consRepresentation = createUnaryConstructor(UnboxedForm 2, "::")
        in
            [(nilConstructor, nilRepresentation), (consConstructor, consRepresentation)]
        end
    in
        val listConstr =
            buildBasisDatatype("list", "list", 1, true, makeConsAndNil)
        val (nilConstructor, consConstructor) =
            case listConstr of
                TypeConstrSet(_, [consC as Value{name="::", ...}, nilC as Value{name="nil", ...}]) =>
                        (nilC, consC)
            |   _ => raise InternalError "nil and cons in wrong order"
    end

    local
        fun makeNoneAndSome optionType =
        let
            val alpha         = BoundTypeVar("'a", TVIndex 0)
            val alphaOption   = mkTypeConstruction ("option", optionType, [alpha], [DeclaredAt inBasis]);
            val someType      = mkFunctionType (alpha, alphaOption)
            val optionTemplate = [TemplPlain{equality=false }]
            val noneConstructor  =
                makeValueConstr ("NONE", ValueType(alphaOption, optionTemplate), true, 2, Local{addr=ref ~1, level=ref baseLevel}, [DeclaredAt inBasis]);
            val someConstructor =
                makeValueConstr ("SOME", ValueType(someType, optionTemplate), false, 2, Local{addr=ref ~1, level=ref baseLevel}, [DeclaredAt inBasis]);
            val noneRepresentation = createNullaryConstructor(ShortForm 0w0, "NONE")
            and someRepresentation = createUnaryConstructor(BoxedForm, "SOME")
        in
            [(noneConstructor, noneRepresentation), (someConstructor, someRepresentation)]
        end
    in
        val optionConstr =
            buildBasisDatatype("option", "option", 1, true, makeNoneAndSome)
        val (noneConstructor, someConstructor) =
            case optionConstr of
                TypeConstrSet(_, [noneC as Value{name="NONE", ...}, someC as Value{name="SOME", ...}]) => (noneC, someC)
            |   _ => raise InternalError "NONE and SOME in wrong order"
    end

    local
        fun listConstruct (base : types) : types =
        let
            val TypeConstrSet(listCons, _) = listConstr
        in
            mkTypeConstruction ("list", listCons, [base], [DeclaredAt inBasis])
        end;

        val intTypeConstr    = TYPESTRUCT.fixedIntType
        val stringTypeConstr = TYPESTRUCT.stringType
        val boolTypeConstr   = TYPESTRUCT.boolType
    in
        local
            val fields =
                [
                    mkLabelEntry("file", stringTypeConstr), mkLabelEntry("startLine", intTypeConstr),
                    mkLabelEntry("startPosition", intTypeConstr), mkLabelEntry("endLine", intTypeConstr),
                    mkLabelEntry("endPosition", intTypeConstr)
                ]
        in
            val locationCons =
                makeTypeConstructor("location",
                    makeTypeFunction(0, mkLabelled(sortLabels fields, true), basisDescription "PolyML.location"),
                    [DeclaredAt inBasis])
            val locationConstr = TypeConstrSet(locationCons, [])
        end

        local
            (* Pretty print context information. *)
            fun makeConstructors typeconstr =
            let
                val contextType = mkTypeConstruction ("context", typeconstr, [], [DeclaredAt inBasis])
                val locationType = mkTypeConstruction ("location", locationCons, [], [DeclaredAt inBasis])
                val constrs =
                   [ ("ContextLocation", locationType),
                     ("ContextProperty", mkProductType[stringTypeConstr, stringTypeConstr])];
                (* The representation of this datatype is given in Pretty.sml and must be the same
                   as the representation that chooseConstrRepr will use. *)
                val numConstrs = List.length constrs
                fun makeCons(s,t) =
                    makeValueConstr(s, ValueType(mkFunctionType(t, contextType), []), false, numConstrs,
                        Local{addr=ref ~1, level=ref baseLevel}, [DeclaredAt inBasis])
                val {constrs=constrCode} = chooseConstrRepr(constrs, 0)
            in
                ListPair.zipEq(List.map makeCons constrs, constrCode)
            end
        in
            val contextConstr =
            (* This is currently an eqtype so must be marked as such. *)
                buildBasisDatatype("context", "PolyML.context", 0, true, makeConstructors)
        end

        local
            fun makeConstructors typeconstr =
            let
                val TypeConstrSet(contextCons, _) = contextConstr
                val prettyType = mkTypeConstruction ("pretty", typeconstr, [], [DeclaredAt inBasis])
                val contextType = mkTypeConstruction ("context", contextCons, [], [DeclaredAt inBasis])

                val constrs =
                   [ ("PrettyBlock",
                        mkProductType[intTypeConstr, boolTypeConstr,
                                      listConstruct contextType, listConstruct prettyType]),
                     ("PrettyBreak", mkProductType[intTypeConstr, intTypeConstr]),
                     ("PrettyLineBreak", EmptyType),
                     ("PrettyString", stringTypeConstr),
                     ("PrettyStringWithWidth", mkProductType[stringTypeConstr, intTypeConstr])];
                (* The representation of this datatype is given in Pretty.sml and must be the same
                   as the representation that chooseConstrRepr will use. *)
                val numConstrs = List.length constrs
                fun makeCons(s,t) =
                let
                    val (ctype, nullary) =
                        case t of EmptyType => (prettyType, true) | t => (mkFunctionType(t, prettyType), false)
                in
                    makeValueConstr(s, ValueType(ctype, []), nullary, numConstrs,
                        Local{addr=ref ~1, level=ref baseLevel}, [DeclaredAt inBasis])
                end
                val {constrs=constrCode, ...} = chooseConstrRepr(constrs, 0)
            in
                ListPair.zipEq(List.map makeCons constrs, constrCode)
            end
        in
            val prettyConstr =
            (* This is currently an eqtype so must be marked as such. *)
                buildBasisDatatype("pretty", "PolyML.pretty", 0, true, makeConstructors)
        end
    end

    (* The representation of ptProperties is given in ExportTree.sml and must also match. *)

    (* Construct an exception identifier - This is a ref (so we can uniquely
       identify it) containing a print function for the type. *)
    fun mkExIden(ty, level) =
    let
        (* Get the constructor tuple, select the constructor operation, apply it to the type. *)
        fun vaGlobal   (Global   x) = x | vaGlobal   _ = raise Match
        val makeSome = mkInd(1, case someConstructor of Value { access, ...} => vaGlobal access)
        val makeNone = mkInd(1, case noneConstructor of Value { access, ...} => vaGlobal access)
        val printerCode =
            case ty of
                FunctionType { arg, ...} =>
                    mkEval(makeSome, [printerForType(arg, level)])
            |   _ => makeNone
    in
        refApplyCode printerCode
    end

    (* Types that can be shared. *)
    structure Sharing =
    struct
        type codetree       = codetree
        type types          = types
        type values         = values
        type typeConstrSet  = typeConstrSet
        type typeId         = typeId
        type level          = level
    end

end;



