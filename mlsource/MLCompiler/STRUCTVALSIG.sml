(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Modified David C. J. Matthews 2009, 2015, 2025.

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

signature STRUCTVALSIG =
sig
    type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }
    val inBasis: location

    type codetree and level
    type univTable

    (* Types *)
  
    datatype 'a possRef = FrozenRef of 'a | VariableRef of 'a ref
    val pling: 'a possRef -> 'a
    val updatePR: 'a possRef * 'a -> unit
  
    (* Standard type constructors. *)
  
    eqtype uniqueId
    
    type typeIdDescription = { location: location, name: string, description: string }
    type references =
        {
            exportedRef: bool ref, localRef: location list ref,
            recursiveRef: (location * string) list ref
        } option
    val makeRef: unit -> references

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
            resType: types,
            usedTvs: BoolVector.vector,
            typeFunCount: int,
            equality: typeFnEq ref,
            uid: uniqueId
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

    (* Variables used in unification.  These are instantiated from generic type variables.
       In addition to equality described above there is also the nonunifiable
       property.  This is set when type variables are entered explicitly and indicates that
       the type variable cannot be made less general.  e.g. (fn x => x+1): 'a->'a 
       is an error. *)
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

    (* Index for type variables.  This is simply to make the use clearer. *)
    and tvIndex = TVIndex of int

    (* Reference chain.  Any entry may be chained onto this. *)
    and 'a refChain = ChainRef of 'a refChain ref | ChainEnd of 'a

        (* A type is the union of these different cases. *)
    and types = 
        TypeVar of typeVar

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
            fullList: {names: string list, frozen: bool} refChain ref
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
            locations: locationProp list,
            references: references,
            instanceTypes: types list ref option
        }

    (* The "type" of a value.  In general this is polymorphic and when an instance is created assignable type
       variables are made for each of the templates.  Bound variables in #typeof are mapped onto these. *)
    and valueType = ValueType of types * typeVarTemplate list

    (* Classes of values. *)
    and valueClass =
        ValBound
    |   PattBound
    |   Exception
    |   Constructor of { nullary: bool, ofConstrs: int }

    (* Location properties.  A value may have some or all of these. *)
    and locationProp =
        DeclaredAt of location
    |   OpenedAt of location
    |   StructureAt of location
    |   SequenceNo of FixedInt.int

    and tvLevel = Generalisable | NotGeneralisable of int

    (* Reference chains.  These are used when unification between two entries
       makes a permanent link between all of them. *)
    val followRefChainToRef: 'a refChain ref -> 'a refChain ref * 'a
    val followRefChainToEnd: 'a refChain -> 'a

    (* type identifiers. *)
    val isEquality:   typeId -> bool
    val offsetId:     typeId -> int
    val sameTypeId:   typeId * typeId -> bool
    val setEquality:  typeId * bool -> unit

    val basisDescription: string -> typeIdDescription
    val makeFreeId: int * valAccess * bool * typeIdDescription -> typeId
    val makeFreeIdEqUpdate: int * valAccess * bool * typeIdDescription -> typeId
    val makeBoundId: int * valAccess * int * bool * bool * typeIdDescription -> typeId
    val makeBoundIdWithEqUpdate: int * valAccess * int * bool * bool * typeIdDescription -> typeId
    
    val makeUniqueId: unit -> uniqueId

    (* Types *)    
    val makeTv: {value: instanceType option, level: tvLevel, equality: bool } -> typeVar

    val typeToInstance: types -> instanceType

    (* Access to values, structures etc. *)
    val makeLocal:    unit -> valAccess
    val makeSelected: int * structVals -> valAccess

    val makeEmptyGlobal:   string -> structVals
    val makeGlobalStruct:  string * signatures * codetree * locationProp list -> structVals
    val makeLocalStruct:   string * signatures * locationProp list -> structVals
    val makeFormalStruct:  string * signatures * int * locationProp list -> structVals

    val makeSelectedStruct: structVals * structVals * locationProp list -> structVals

    (* Functors *)
    val makeFunctor: string * structVals * signatures * valAccess * locationProp list -> functors

    (* Signatures *)
    val makeSignatureTable: unit -> univTable
    val makeSignature: string * univTable * int * locationProp list * (int -> typeId) * typeId list -> signatures

    (* Values. *)
    val undefinedValue: values
    val isUndefinedValue: values -> bool

    (* Infix status *)
    datatype infixity = 
        Infix of int
    |   InfixR of int
    |   Nonfix

    datatype fixStatus = FixStatus of string * infixity

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

    val makeEnv: univTable -> env

    val valueVar:      values      Universal.tag
    val typeConstrVar: typeConstrSet Universal.tag
    val fixVar:        fixStatus   Universal.tag
    val structVar:     structVals  Universal.tag
    val signatureVar:  signatures  Universal.tag
    val functorVar:    functors    Universal.tag

    (* Types that can be shared. *)
    structure Sharing:
    sig
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

