(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Modified David C. J. Matthews 2009, 2015.

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
  
    type typeVarForm
    type uniqueId
    
    type typeIdDescription = { location: location, name: string, description: string }
    type references =
        {
            exportedRef: bool ref, localRef: location list ref,
            recursiveRef: (location * string) list ref
        } option
    val makeRef: unit -> references

    datatype typeId =
        TypeId of { access: valAccess, description: typeIdDescription, idKind: typeIdKind }

    and typeIdKind =
        Free of { uid: uniqueId, allowUpdate: bool, arity: int  }
    |   Bound of { offset: int, eqType: bool possRef, isDatatype: bool, arity: int }
    |   TypeFn of typeVarForm list * types

        (* A type is the union of these different cases. *)
    and types = 
        TypeVar of typeVarForm

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

    |   LabelledType  of labelledRec

    |   OverloadSet   of
        {
            typeset: typeConstrs list
        }

    |   BadType
  
    |   EmptyType

    and typeConstrs = 
        TypeConstrs of
        {
            name:       string,
            identifier: typeId,
            locations:  locationProp list (* Location of declaration *)
        }

    and labelFieldList =
        FieldList of string list * bool (* True if this is frozen *)
    |   FlexibleList of labelFieldList ref

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
            typeOf: types,
            access: valAccess,
            class: valueClass,
            locations: locationProp list,
            references: references,
            instanceTypes: types list ref option
        }

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

    withtype labelledRec =
    {
        (* Fields actually present in this record.  If this was flexible at some
           stage there may be extra fields listed in the full field list. *)
        recList: { name: string, typeof: types } list,
        (* The names of all the fields including extra fields. *)
        fullList: labelFieldList
    }


    (* type identifiers. *)
    val isEquality:   typeId -> bool
    val offsetId:     typeId -> int
    val idAccess:     typeId -> valAccess
    val sameTypeId:   typeId * typeId -> bool
    val setEquality:  typeId * bool -> unit

    val basisDescription: string -> typeIdDescription
    val makeFreeId: int * valAccess * bool * typeIdDescription -> typeId
    val makeFreeIdEqUpdate: int * valAccess * bool * typeIdDescription -> typeId
    val makeBoundId: int * valAccess * int * bool * bool * typeIdDescription -> typeId
    val makeBoundIdWithEqUpdate: int * valAccess * int * bool * bool * typeIdDescription -> typeId
    val makeTypeFunction: typeIdDescription * (typeVarForm list * types) -> typeId
    
    (* Types *)
    val badType:   types
    val emptyType: types

    val isBad:     types -> bool
    val isEmpty:   types -> bool

    val recordFields   : labelledRec -> string list
    val recordIsFrozen : labelledRec -> bool

    val tcName:            typeConstrs -> string
    val tcArity:           typeConstrs -> int
    val tcEquality:        typeConstrs -> bool
    val tcSetEquality:     typeConstrs * bool -> unit
    val tcIdentifier:      typeConstrs -> typeId
    val tcLocations:       typeConstrs -> locationProp list
    val tcIsAbbreviation:  typeConstrs -> bool

    val makeTypeConstructor:
        string * typeId * locationProp list -> typeConstrs

    datatype typeConstrSet = (* A type constructor with its, possible, value constructors. *)
        TypeConstrSet of typeConstrs * values list

    val tsConstr: typeConstrSet -> typeConstrs
    val tsConstructors: typeConstrSet -> values list

    val tvLevel:        typeVarForm -> int
    val tvEquality:     typeVarForm -> bool
    val tvPrintity:     typeVarForm -> bool
    val tvNonUnifiable: typeVarForm -> bool
    val tvValue:        typeVarForm -> types
    val tvSetValue:     typeVarForm * types -> unit

    val sameTv: typeVarForm * typeVarForm -> bool

    val makeTv:
        {value: types, level: int, equality: bool, nonunifiable: bool, printable: bool } -> typeVarForm

    val generalisable: int

    (* Access to values, structures etc. *)
    val makeGlobal:   codetree -> valAccess
    val makeLocal:    unit -> valAccess
    val makeSelected: int * structVals -> valAccess

    val vaGlobal:   valAccess -> codetree
    val vaLocal:    valAccess -> { addr: int ref, level: level ref }

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
    val valName: values -> string
    val valTypeOf: values -> types
    val undefinedValue: values
    val isUndefinedValue: values -> bool
    val isConstructor: values -> bool
    val isValueConstructor: values -> bool

    val makeOverloaded: string * types * typeDependent -> values
    val makeValueConstr: string * types * bool * int * valAccess * locationProp list -> values

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
            allValNames:  unit -> string list
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
        and  typeVarForm = typeVarForm
        and  level = level
    end
end;

