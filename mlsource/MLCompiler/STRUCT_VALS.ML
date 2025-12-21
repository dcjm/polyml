(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Modified David C. J. Matthews 2009, 2012, 2015, 2025.

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
    Title:      Global and Local values.
    Author:     Dave Matthews,Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)
  
functor STRUCT_VALS (

structure CODETREE : CODETREE where type machineWord = Address.machineWord

structure UNIVERSALTABLE :
sig
  type 'a tag = 'a Universal.tag;
  type univTable
  type universal = Universal.universal
  
  val makeUnivTable: unit -> univTable;
  val univEnter:     univTable * 'a tag * string * 'a -> unit;
  val univLookup:    univTable * 'a tag * string -> 'a option;
  
  (* Freeze a mutable table so it is immutable. *)
  val univFreeze:       univTable -> univTable

    val fold: (string * universal * 'a -> 'a) -> 'a -> univTable -> 'a
end

) :> STRUCTVALSIG where type codetree = CODETREE.codetree and type univTable = UNIVERSALTABLE.univTable and type level = CODETREE.level
=  

struct
    open CODETREE;
  
    open Misc;
    open Universal;
    open UNIVERSALTABLE;
  
    (* Location for declarations. *)
    type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }

    (* The idea of this is reduce the number of mutable objects. *)
    datatype 'a possRef = FrozenRef of 'a | VariableRef of 'a ref
    fun pling(FrozenRef x) = x | pling(VariableRef(ref x)) = x
    fun updatePR(VariableRef r, x) = r := x | updatePR(FrozenRef _, _) = raise Fail "Assignment to frozen ref"

    (* References to identifiers.  exportedRef is set to true if the identifier is exported to
       the global environment or to a structure.  localRef contains the list of local uses.
       recursiveRef contains a list of (mutually) recursive references together with the name
       of the function that refers to this.  It is used when computing whether a recursive
       function is actually refered to from elsewhere. 
       This is an option type because this is used only within local identifiers. *)
    type references =
    {
        exportedRef: bool ref,
        localRef: location list ref,
        recursiveRef: (location * string) list ref
    } option
    
    fun makeRef(): references =
        SOME { exportedRef = ref false, localRef = ref nil, recursiveRef=ref nil }
   
  (* typeIds are used to distinguish between concrete types.  Two
     types that share will have the same identifier.  If the identifiers are
     different they are different types.
     There are three classes of type identifier.  Free identifiers are used for
     types in the global environment. Bound identifiers occur in signatures,
     functors and while compiling structures.  Type functions arise from
     type bindings (type abbreviations) or from "where type" definitions
     in signatures.
     The type identifier also contains the equality attribute.
     In ML97 only types have these identifiers.  In ML90 these were also
     needed for structures.
     Free and Bound IDs contain information to find the equality and
     value-printing functions. *)
    type typeIdDescription = { location: location, name: string, description: string }

    datatype typeId =
        TypeId of { access: valAccess, description: typeIdDescription, idKind: typeIdKind }

    (* A type constructor can be one of these kinds.
       Free ids are used for datatypes in the core language,
       Bound ids are used in signatures,
       Type functions (type abbreviations) refer to other types.
       To allow type functions to be unified without unrolling first we need to record which
       type variables are actually used and how they are used for equality.  It is possible
       that a type variable is never used (e.g. type 'a t = int) in which case "bool t" and
       "string t" both unify and "(int->int) t" admits equality.  The typeFunCount records
       the depth of the sequence of type functions e.g. type s(*1*) = int*int type t(*2*) = s
       so when unifying "x:s" and "y:t" we unwrap "t" rather than "s". *)
    and typeIdKind =
        Free of { uid: uniqueId, allowUpdate: bool, arity: int  }
    |   Bound of { offset: int, eqType: bool possRef, isDatatype: bool, arity: int }
    |   TypeFn of
        {
            arity:      int,
            resType:    types,
            usedTvs:    BoolVector.vector,
            typeFunCount: int,
            equality:   typeFnEq ref,
            uid:        uniqueId
        }

    and typeFnEq = TypeFnEqNever | TypeFnEq of BoolVector.vector


    (* Templates for type variables.  Normal type variables are TemplPlain.
       equality means this is an equality type.  The compiler adds equality
       functions to functions that contain an equality test and print functions to those that
       use PolyML.print.  Currently, with TypeIdCode.justForEqualityTypes set to true, only
       polymorphic functions that involve equality types will also print correctly with PolyML.print.
       TemplOverload is used for the functions at the outer level that can be overloaded. *)
    and typeVarTemplate =
        TemplPlain of { equality: bool }
    |   TemplOverload of string

    (* Variables used in unification.  These are instantiated from generic type variables. *)
    and typeVar =
        TypeVariable of
        {
            link: instanceType option refChain ref,
            equality: bool refChain ref,
            level: tvLevel refChain ref
        }

    (* A type variable can be set to one of these. TVLUnset is the initial state.  TVLCoreType is used
       when the type variable has been unified with a core type and includes the map for the bound type
       variables. TVLLink is used when the type variable has been unified with another type variable.
       TVLOverload is used when the type variable is set to an overload set. *)
    and typeVarLink =
        TVLUnset
    |   TVLCoreType of { types: types, map: tvIndex -> typeVar }
    |   TVLLink of typeVar
    |   TVLOverload of typeConstrs list

    (* Reference chain.  Any entry may be chained onto this. *)
    and 'a refChain = ChainRef of 'a refChain ref | ChainEnd of 'a

    (* Index for type variables.  This is simply to make the use clearer. *)
    and tvIndex = TVIndex of int

    (* Old type structure. *)
    (* A type is the union of these different cases. *)
    and types = 
        TypeVar          of typeVar

        (* Bound type variable. This is an index into a table or list of types.  The name is just for printing. *)
    |   BoundTypeVar of string * tvIndex

        (* Free type variable.  These are used where a type variable has been introduced explicitly.
           Normally they are NonGeneric and  can only be generalised in a scope less than "level".
           Generic is used when matching a signature and ensures that the value in the matching
           structure is fully polymorphic. *)
    |   FreeTypeVar of
        {
            name: string,
            equality: bool,
            level: tvLevel,
            uid: uniqueId
        }

    |   TypeConstruction of
        {
            name:  string,
            constr: typeConstrs,
            args:  types list,
            locations: locationProp list
        }

    |   FunctionType of
        { 
            arg:    types,
            result: types
        }

        (* A normal tuple or labelled record.  Tuples are labelled records with fields #1, #2 etc. *)
    |   LabelledRecord of {typeOf: types, name: string} list

        (* Flexible records i.e. derived from "{a, ...}". These can only occur with
           local values since the record must be frozen at the top level.
           The first is a variable, similar to TypeVar, that can be updated.
           A flexible record consists of the list of fields with their types which can be
           extended during unification.  The full list holds the field names.  Every
           instance has the same list of names.  When "frozen" is true the list cannot be
           extended. "properties" is used when new fields need to be added. *)
    |   FlexibleRecordVar of
        {
            recList: {typeOf: types, name: string} list refChain ref,
            fullList: {names: string list, frozen: bool} refChain ref,
            equality: bool refChain ref,
            level: tvLevel refChain ref
        }

        (* The bound variable stands in for the unspecified fields. *)
    |   BoundFlexRecord of
        {
            boundVar: tvIndex,
            recList: {typeOf: types, name: string} list,
            fullList: {names: string list, frozen: bool}  refChain ref
        }

        (* An overload set.  This is constructed from a TemplOverload template along with the current
           set of overloads for the identifier.  e.g. for "+" that might be FixedInt.int, LargeInt.int,
           Word.word etc.  As unification proceeds the set may reduce to a single type.
           These are chained so that if two sets are unified they become linked and subsequent
           use of either affects both. *)
    |   OverloadSetVar of typeConstrs list refChain ref

        (* For when there has been a type error or an undefined identifier *)
    |   BadType

    and typeConstrs = 
        TypeConstrs of
        {
            name:       string,
            identifier: typeId,
            locations:  locationProp list (* Location of declaration *)
        }

    and typeConstrSet = (* A type constructor with its, possible, value constructors. *)
        TypeConstrSet of typeConstrs * values list

    (* Instance types.  A type plus maps for bound type variables and type IDs. *)
    and instanceType =
        Instance of types * (tvIndex -> types option) * (typeId -> typeId option)
    |   SimpleInstance of types

    (* Access to a value, structure or functor. *)
    and valAccess =
        Global   of codetree
    |   Local    of { addr: int ref, level: level ref }
    |   Selected of { addr: int,     base:  valAccess }
    |   Formal   of int
    |   Overloaded of typeDependent (* Values only. *)

    (* Structures. *)
    and structVals = 
       Struct of
        {
            name:   string,
            signat: signatures,
            access: valAccess,
            locations: locationProp list
        }

    (* Signatures.  The representation of a signature deserves a bit of explanation.
       A signature is an environment: a set of values, type constructors and sub-structures.
       Behind the types associated with the type constructors and values
       are what the semantics calls "type names" but are referred to in the Poly/ML
       code as type-ids to avoid confusion with the text name of a type constructor.
       The same signature may be bound to a signature identifier, used as an argument
       to a functor, the result of a functor or the signature of a structure.  The
       environment is the same in each case; what is different is the type names.
       To avoid rebuilding the environment for each case the same environment is
       used but with different sets of typeIdMap, firstBoundIndex and boundIds.
       typeIdMap maps the "offset" of any bound id found in the environment to a
       type-id.  For structures the result will always be a free id and boundIds
       will be empty.  In other cases it may map to either a free id, perhaps as
       the result of "where type" constraint, or to a bound id.  Sharing constraints
       will cause different bound ids in the environment to map to the same resulting
       bound id.  Normally, firstBoundIndex will be zero and boundIds will be the
       set of bound ids that can be produced by typeIdMap.  The exception is the
       result signature of a functor.  In that case typeIdMap may return bound Ids
       in the set of the boundIds for the argument to the functor, which will have
       offsets >= 0 and < firstBoundIndex or in the set for the result of the functor
       with offsets >= firstBoundIndex and < firstBoundIndex+length boundIds.  When
       the functor is applied the typeIdMap for the structure that is produced
       maps the first set to the free ids of the actual argument and the maps the
       second set to new, unique free ids. *)
    and signatures =
        Signatures of
        { 
            name:               string,
            tab:                univTable,
            typeIdMap:          int -> typeId,
            firstBoundIndex:    int,
            boundIds:           typeId list,
            locations:          locationProp list
        }

    and functors =
        Functor of
        {
            name:       string,
            arg:        structVals,
            result:     signatures,
            access:     valAccess,
            locations:  locationProp list
        }

    (* Values. *)
    (* The overloaded functions divide up into basically two groups: Those =, 
       <>, print and makestring  which are infinitely overloaded and those 
       *, + etc  which are overloaded on a limited range of types. *)  
    and typeDependent =
        Print
    |   GetPretty
    |   MakeString
    |   AddPretty
    |   Equal
    |   NotEqual
    |   AddOverload
    |   TypeDep
    |   GetLocation

    and values =
        Value of
        {
            name: string,
            typeOf: valueType,
            access: valAccess,
            class: valueClass,
            locations: locationProp list, (* Location of declaration *)
            references: references
        }

    (* The "type" of a value.  In general this is polymorphic and when an instance is created assignable type
       variables are made for each of the templates.  Bound variables in #typeof are mapped onto these. *)
    and valueType = ValueType of types * typeVarTemplate list


    (* Classes of values. *)
    and valueClass =
        ValBound
    |   PattBound
    |   Exception
    |   Constructor of
        {
            nullary: bool, (* True if this is a single value (e.g. "nil") rather than a function. *)
            ofConstrs: int (* Total number of constructors in the datatype. *)
        }

    and locationProp =
        DeclaredAt of location
    |   OpenedAt of location
    |   StructureAt of location
    |   SequenceNo of FixedInt.int

    and tvLevel = Generalisable | NotGeneralisable of int

    withtype uniqueId = bool ref
        (* We use a ref here both because we can then set equality if we
           need but also because it allows us to create a unique Id. *)
    
  (* A set of type contructors.  This is used only during the
     compilation process and represents the set of possible types
     which may occur. It functions in much the same way as a type
     variable.  Because we only allow overloading on monomorphic
     type constructors such as "int" and "word" we can restrict the
     set to containing only type constructors rather than general types.
     This overload set was added for ML 97 because ML 97, unlike ML 90,
     defaults overloaded operators and constants if unification does
     not result in a single type being found.  
     The overload set is used in a similar way to a flexible record
     and will always be pointed at by a type variable so that the
     set can be replaced by a single type construction if the unification
     reduces to a single type. *)
    and overloadSetForm =
    {
        typeset: typeConstrs list
    }

    (* Reference chains.  These are used when unification between two entries
       makes a permanent link between all of them. *)
    (* Return the last reference in a chain. *)
    fun followRefChainToRef(v as ref(ChainEnd final)) = (v, final)
    |   followRefChainToRef(ref(ChainRef c)) = followRefChainToRef c
    
    fun followRefChainToEnd(ChainRef(ref v)) = followRefChainToEnd v
    |   followRefChainToEnd(ChainEnd e) = e

    (* Identifiers *)
    fun makeFreeId(arity, access, eq, desc) =
        TypeId { access=access, description = desc,
            idKind = Free {uid = ref eq, allowUpdate=false, arity=arity}}
    (* At the moment the only reason for distinguishing makeFreeId and makeFreeIdEqUpdate
       is that it allows us to check that we're actually permitting update when needed. *)
    fun makeFreeIdEqUpdate(arity, access, eq, desc) =
        TypeId { access=access, description = desc,
            idKind = Free {uid = ref eq, allowUpdate=true, arity=arity}}

    fun makeBoundId (arity, access, n, eq, isdt, desc) =
        TypeId { access=access, description = desc,
            idKind = Bound{offset=n, eqType=FrozenRef eq, isDatatype = isdt, arity=arity}}

    (* Within the body of a functor we make bound stamps but may need to
       set the equality attribute. *)
    fun makeBoundIdWithEqUpdate (arity, access, n, eq, isdt, desc) =
        TypeId { access=access, description = desc,
            idKind = Bound{offset=n, eqType=VariableRef(ref eq), isDatatype = isdt, arity=arity}}

    fun makeUniqueId() = ref false (* Just for type functions *)
            
    (* Find the number - assuming it is bound. *)
    fun offsetId (TypeId{idKind=Bound {offset, ...}, ...}) = offset
    |   offsetId _       = raise InternalError "offsetId: not a Bound";

    (* Are two type constructors the same? *)
    fun sameTypeId (TypeId{idKind=Free{uid = a, ...}, ...}, TypeId{idKind=Free {uid = b, ...}, ...}) = a = b
    |   sameTypeId (TypeId{idKind=Bound{offset=a, ...}, ...}, TypeId{idKind=Bound{offset=b, ...}, ...}) = a = b
    |   sameTypeId _ = false (* Includes type functions. *)

    fun isEquality (TypeId { idKind = Free{uid = ref eq, ...}, ...}) = eq
    |   isEquality (TypeId { idKind = Bound{eqType, ...}, ...}) = pling eqType
    |   isEquality (TypeId { idKind = TypeFn _, ...}) = raise InternalError "isEquality: TypeFn"

    (* Set the equality property.   Currently, free IDs are used for abstypes and
       datatypes that are local to a function as well as the usual case of using them
       for top-level types. *)
    fun setEquality(TypeId{idKind = Free{uid, allowUpdate=true, ...}, ...}, eq) = uid := eq
    |   setEquality(TypeId{idKind = Bound{eqType=VariableRef id, ...}, ...}, eq) = id := eq
    |   setEquality _ = raise InternalError "setEquality: can't set equality attribute"

    (* Signatures: Used for both signatures of local structures and for global structures 
       (name spaces). Strictly signatures do not contain fix-status functors
       or signatures but as we use these structures for top-level name-spaces
       we have to have tables for these. *)
    val makeSignatureTable = makeUnivTable

    (* Make a signature, freezing the table. *)
    fun makeSignature (name, table, fbi, locations, typeIdMap, boundIds) =
        Signatures { name = name,
               tab        = univFreeze table,
               typeIdMap  = typeIdMap,
               firstBoundIndex   = fbi,
               boundIds   = boundIds,
               locations = locations  }
    
  (* Types. *)

  (* A type variable is implemented as a true variable i.e. it can
     be assigned a particular type when it is unified. Initially it is
     set to EmptyType which represents an unset type variable.
     When it is unified with a type it is set to point to the type it
     has been unified with.  Type variables associated with variables
     have level set to the nesting level, others have level set to
     "generalisable". If two type variables are united their levels are 
     set to the lower of the two. If level is not "generalisable" the type
     variable is not generalisable. This is needed to deal with cases like
       fn a => let val x = a in x end      and
       fn a => let val x = hd a in x end
     The level is set to "generalisable" at the end of the block with that
     level.
     ``equality'' is true if this is an equality variable e.g. ''a. *)

    fun makeTv {value, level, equality } =
        TypeVariable{ link = ref (ChainEnd value), level= ref (ChainEnd level), equality = ref (ChainEnd equality) }

  (* Type constructors are identifiers which take zero or more types and yield a
     type as result. Their main property is that two type constructors can be 
     unified iff they are the same constructor. Another use for
     constructors is for aliasing types. In this case "typeVars" points to a list 
     of type variables which are used in the "equivalent" type. ``equality'' is a 
     flag indicating if the values can be tested for equality. *)

    val inBasis =
        { file = "Standard Basis", startLine = 0, startPosition = 0, endLine = 0, endPosition = 0}
    fun basisDescription name = { location = inBasis, description = "In Basis", name = name }


    (* Infix status. *)
    datatype infixity = 
        Infix of int
    |   InfixR of int
    |   Nonfix

    datatype fixStatus = FixStatus of string * infixity
  
    fun makeLocal () = Local { addr = ref ~1 (* Invalid addr - catch errors *), level = ref baseLevel }
       
    fun makeSelected (addr, Struct{access, ...}) = Selected { addr = addr, base = access }

    fun makeStruct (name, signat, access, locations) = 
        Struct { name = name, signat = signat, access = access, locations = locations }
    
    (* Global structure *)
    fun makeGlobalStruct (name, signat, code, locations) =
        makeStruct (name, signat, Global code, locations)

    (* These are used in INITIALISE so must be mutable. *)
    fun makeEmptyGlobal name =
        makeStruct (name,
            Signatures { name = "",
                   tab        = makeUnivTable(),
                   typeIdMap  = fn _ => raise Subscript,
                   firstBoundIndex   = 0, 
                   boundIds   = [],
                   locations =  [DeclaredAt inBasis] },
            Global CodeZero, [DeclaredAt inBasis])
     
    (* Local structure. *)
    fun makeLocalStruct (name, signat, location) = 
        makeStruct (name, signat, makeLocal (), location);
     
    (* Structure in a local structure or a functor argument. *)
    fun makeSelectedStruct (selected as Struct{access, name, signat, locations, ...}, base, openLocs) =
    case access of 
        Formal sel =>
           makeStruct(name, signat, makeSelected (sel, base), openLocs @ locations)
      | Global code => (* Need to add the locations. *)
           makeStruct(name, signat, Global code, openLocs @ locations)
      | _          => selected
  
    fun makeFormalStruct (name, signat, addr, location) =
      makeStruct (name, signat, Formal addr, location);
     
    (* Values. *)
    val undefinedValue    =
    Value{ name = "<undefined>", typeOf = ValueType(BadType, []), access = Global CodeZero, class = ValBound,
           locations = [DeclaredAt inBasis], references = NONE }

    fun isUndefinedValue(Value{name = "<undefined>", ...}) = true | isUndefinedValue _ = false

    (* Functor value. *)
    fun makeFunctor (name, arg, result, access, locations) = 
        Functor 
        {
            name = name,
            arg = arg,
            result = result,
            access = access,
            locations = locations
        }

    val valueVar:      values      tag = tag();
    val typeConstrVar: typeConstrSet tag = tag();
    val fixVar:        fixStatus   tag = tag();
    val structVar:     structVals  tag = tag();
    val signatureVar:  signatures  tag = tag();
    val functorVar:    functors    tag = tag();

    fun makeLook (t:'a tag) table n = univLookup (table, t, n)
    and makeEnter (t:'a tag) table (n, v) = univEnter (table, t, n, v)
    and makeAllNames (t:'a tag) table () =
        UNIVERSALTABLE.fold (fn (s, u, l) => if Universal.tagIs t u then s :: l else l) [] table

    datatype env = 
    Env of 
    { 
        lookupVal:    string -> values option,
        lookupType:   string -> typeConstrSet option,
        lookupFix:    string -> fixStatus option,
        lookupStruct: string -> structVals option,
        lookupSig:    string -> signatures option,
        lookupFunct:  string -> functors option,
        enterVal:     string * values      -> unit,
        enterType:    string * typeConstrSet -> unit,
        enterFix:     string * fixStatus   -> unit,
        enterStruct:  string * structVals  -> unit,
        enterSig:     string * signatures  -> unit,
        enterFunct:   string * functors    -> unit,
        allValNames:  unit -> string list,
        allStructNames: unit -> string list
    }

    (* This creates functions for entering and looking up names. *)
    fun makeEnv tab =
        Env {
            lookupVal      = makeLook  valueVar      tab,
            lookupType     = makeLook  typeConstrVar tab,
            lookupFix      = makeLook  fixVar        tab,
            lookupStruct   = makeLook  structVar     tab,
            lookupSig      = makeLook  signatureVar  tab,
            lookupFunct    = makeLook  functorVar    tab,
            enterVal       = makeEnter valueVar      tab,
            enterType      = makeEnter typeConstrVar tab,
            enterFix       = makeEnter fixVar        tab,
            enterStruct    = makeEnter structVar     tab,
            enterSig       = makeEnter signatureVar  tab,
            enterFunct     = makeEnter functorVar    tab,
            allValNames    = makeAllNames valueVar   tab,
            allStructNames = makeAllNames structVar  tab
        }

    structure Sharing =
    struct
        type codetree   = codetree
        and  signatures = signatures
        and  types      = types
        and  values     = values
        and  typeId     = typeId
        and  structVals = structVals
        and  valAccess  = valAccess
        and  typeConstrs= typeConstrs
        and  typeConstrSet=typeConstrSet
        and  env        = env
        and  univTable  = univTable
        and  fixStatus  = fixStatus
        and  infixity   = infixity
        and  functors   = functors
        and  locationProp = locationProp
        and  level      = level
        and  tvLevel    = tvLevel
        and  typeFnEq   = typeFnEq
        and  typeVarTemplate = typeVarTemplate
        and  typeVar    = typeVar
        and  typeVarLink = typeVarLink
        and  tvIndex    = tvIndex
        and  valueType  = valueType
        and  instanceType = instanceType
    end
end;
