(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited
        
    Modified David C. J. Matthews 2009, 2010, 2015, 2016, 2018, 2025.

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

signature TYPETREESIG =
sig
    type types;
    type values;
    type typeConstrs;
    type typeConstrSet
    type typeVarForm
    type lexan;
    type typeId;
    type pretty;
    type ptProperties
    type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }
    type typeIdDescription = { location: location, name: string, description: string }
    type exportTree = location * ptProperties list
    type navigation =
        {parent: (unit -> exportTree) option,
         next: (unit -> exportTree) option,
         previous: (unit -> exportTree) option}
    type matchResult
    type locationProp
    type structVals
    type codetree
    type tvLevel
    type valAccess

    type printTypeEnv =
        { lookupType: string -> (typeConstrSet * (int->typeId) option) option,
          lookupStruct: string -> (structVals * (int->typeId) option) option}
    val emptyTypeEnv: printTypeEnv

    val mkTypeVar:          tvLevel * bool * bool * bool -> types
    val mkTypeConstruction: string * typeConstrs * types list * locationProp list -> types;
    val mkProductType:      types list -> types;
    val mkFunctionType:     types * types -> types;
    val mkLabelled:         {name: string, typeof: types } list * bool -> types;
    val mkLabelEntry:       string * types -> {name: string, typeof: types };
    val mkOverloadSet:      typeConstrs list -> types;
    val sortLabels:         {name: string, typeof: types } list -> {name: string, typeof: types } list;
    val entryNumber:        string * types -> int;
    val recordNotFrozen:    types -> bool;
    val recordWidth:        types -> int;
    val recordFieldMap:     (types -> 'a) -> types -> 'a list
    val makeEquivalent:     typeConstrs * types list -> types;
    val firstArg:           types -> types

    val makeGeneralTypeFunction:   typeVarForm list * types * typeIdDescription * valAccess -> typeId
    and makeTypeFunction:   typeVarForm list * types * typeIdDescription -> typeId

    (* Follow a chain of unified type variables *)
    val eventual:           types -> types

    (* Test for function type and return function argument. *)
    val getFnArgType:   types -> types option
   
    (* Unify two type variables which would otherwise be non-unifiable. *)
    val linkTypeVars: typeVarForm * typeVarForm -> unit;
    val setTvarLevel: typeVarForm * tvLevel -> unit

    (* Copy a type constructor. *)
    val copyTypeConstr:
        typeConstrs * (typeId -> typeId option) * (types -> types) * (string -> string) -> typeConstrs
    val copyTypeConstrWithCache:
        typeConstrs * (typeId -> typeId option) * (types -> types) *
        (string -> string) * typeConstrs list -> typeConstrs

    (* Copy a type. *)
    val copyType: types * (types -> types) * (typeConstrs -> typeConstrs) -> types;

    (* Compose two typeId maps. *)
    val composeMaps: (int -> typeId) * (int -> typeId) -> (int -> typeId)

    (* Print it out prettily *)
    val display: types * FixedInt.int * printTypeEnv -> pretty;
    val displayWithMap: types * FixedInt.int * printTypeEnv * (int->typeId) option -> pretty;

    (* Print out a type constructor. *)
    val displayTypeConstrs: typeConstrSet * FixedInt.int * printTypeEnv -> pretty;
    val displayTypeConstrsWithMap: typeConstrSet * FixedInt.int * printTypeEnv * (int->typeId) option -> pretty;

    (* A list of type variables. *)
    val displayTypeVariables: typeVarForm list * FixedInt.int -> pretty list;

    (* Returns the preferred type constructor from an overload. *)
    val typeConstrFromOverload: types * bool -> typeConstrs;

    (* Check a set of mutually recursive datatypes to see which admit equality. *)
    val computeDatatypeEqualities: typeConstrSet list * (int -> bool) -> unit;

    (* Unify two type structures to give a unified type. *)
    val unifyTypes: types * types -> matchResult option
    
    (* Pretty print the error message. *)
    val unifyTypesErrorReport: lexan * printTypeEnv * printTypeEnv * string -> matchResult -> pretty

    (* Check whether a type admits equality. *)
    val typePermitsEquality: types -> bool

    (* Generate new copies of all unbound type variables - this is used on all
       non-local values or constructors so that, for example, each occurence of
       "hd", which has type 'a list -> 'a, can be separately bound to types. *)
    val generalise: types -> types * {value: types, equality: bool, printity: bool} list
    (* Create an instance of an overloaded type. *)
    val generaliseOverload: types * typeConstrs list * bool -> types * types list;

    (* The same as generalise but with a function that looks up types. *)
    val generaliseWithMap:
        types * (typeVarForm -> types option) ->
            types * {value: types, equality: bool, printity: bool} list

    (* Return the type variables that would be generalised at this point. *)
    val getPolyTypeVars: types * (typeVarForm -> types option) -> typeVarForm list

    (* Release type variables at this nesting level.  Updates the type to the
       generalised version. *)
    val allowGeneralisation: types * int * bool *
                             lexan * location * (unit -> pretty) * printTypeEnv -> unit

    (* Check for a local datatype "escaping".  Added for ML97. *)
    val checkForEscapingDatatypes: types * (string -> unit) -> unit

    (* Check for free type variables.  Added for ML97. *)
    val checkForFreeTypeVariables: string * types * lexan * (unit->codetree) -> unit;

    val constructorResult: types * types list -> types;

    val identical:       types * types -> bool;

    val boolConstr:   typeConstrs;
    val fixedIntConstr:typeConstrs
    val intInfConstr: typeConstrs
    val charConstr:   typeConstrs;
    val stringConstr: typeConstrs;
    val wordConstr:   typeConstrs;
    val realConstr:   typeConstrs
    val floatConstr:  typeConstrs
    val refConstr:    typeConstrs;
    val arrayConstr:  typeConstrs;
    val array2Constr: typeConstrs;
    val byteArrayConstr: typeConstrs;
    val unitConstr:   typeConstrs;
    val exnConstr:    typeConstrs;
    val undefConstr:  typeConstrs;

    val boolType:       types
    val fixedIntType:   types
    val stringType:     types
    val unitType:       types
    val exnType:        types
    
    val isPointerEqType: typeId -> bool
    
    datatype floatKind = FloatDouble | FloatSingle
    val isFloatingPt: types -> floatKind option

    val isUndefinedTypeConstr: typeConstrs -> bool
    val isBadType:  types -> bool

    val sameTypeVar : types * types -> bool;

    (* If this is simply giving a new name to a type constructor returns the
       type identifier of the constructor that is being rebound. *)
    val typeNameRebinding: typeVarForm list * types -> typeId option

    val leastGeneral: types list -> types

    (* Parse tree operations. *)
    type typeParsetree
    val ParseTypeBad: typeParsetree
    val makeParseTypeConstruction:
        (string * location) * (typeParsetree list * location) * location -> typeParsetree
    val makeParseTypeProduct: typeParsetree list * location -> typeParsetree
    val makeParseTypeFunction: typeParsetree * typeParsetree * location -> typeParsetree
    val makeParseTypeLabelled:
        ((string * location) * typeParsetree * location) list * bool * location -> typeParsetree
    val makeParseTypeId: typeVarForm * location -> typeParsetree
    val unitTree: location -> typeParsetree
    val displayTypeParse: typeParsetree * FixedInt.int * printTypeEnv -> pretty;

    (* Fill in the values of type variables and make checks. *)
    val assignTypes: typeParsetree * (string * location -> typeConstrSet) * lexan -> types;
    
    (* Check the value we're discarding in an expression sequence or a let binding
       and return a string if it's not appropriate. *)
    val checkDiscard: types * lexan -> string option

    val typeExportTree: navigation * typeParsetree -> exportTree
    
    val setPreferredInt: typeConstrs -> unit

    structure TypeValue:
    sig
        val extractEquality: codetree -> codetree
        and extractPrinter: codetree -> codetree
        and extractBoxed: codetree -> codetree
        and extractSize: codetree -> codetree

        val boxedNever: codetree
        and boxedAlways: codetree
        and boxedEither: codetree

        val isBoxedNever: codetree
        and isBoxedAlways: codetree
        and isBoxedEither: codetree

        val singleWord: codetree
        
        val createTypeValue:
            {eqCode: codetree, printCode: codetree, boxedCode: codetree, sizeCode: codetree} -> codetree
    end

    structure ValueConstructor:
    sig
        val extractTest: codetree -> codetree
        val extractInjection: codetree -> codetree
        val extractProjection: codetree -> codetree
        val createValueConstr:
            { testMatch: codetree, injectValue: codetree, projectValue: codetree } -> codetree
        val createNullaryConstr:
            { testMatch: codetree, constrValue: codetree } -> codetree
    end

    (* Types that can be shared. *)
    structure Sharing:
    sig
        type types      = types
        and  values     = values
        and  typeId     = typeId
        and  structVals = structVals
        and  typeConstrs= typeConstrs
        and  typeConstrSet=typeConstrSet
        and  typeParsetree = typeParsetree
        and  locationProp = locationProp
        and  pretty     = pretty
        and  lexan      = lexan
        and  ptProperties = ptProperties
        and  typeVarForm = typeVarForm
        and  codetree   = codetree
        and  matchResult = matchResult
        and  tvLevel = tvLevel
        and  valAccess  = valAccess
    end

end;

