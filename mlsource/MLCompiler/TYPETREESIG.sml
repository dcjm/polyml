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
    type typeVar
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
    type tvIndex
    type typeVarTemplate
    type argumentType
    type instanceType

    type printTypeEnv =
        { lookupType: string -> (typeConstrSet * (int->typeId) option) option,
          lookupStruct: string -> (structVals * (int->typeId) option) option}
    val emptyTypeEnv: printTypeEnv

    val mkTypeVar:          tvLevel * bool -> types
    val mkTypeConstruction: string * typeConstrs * types list * locationProp list -> types;
    val mkProductType:      types list -> types;
    val mkFunctionType:     types * types -> types;
    val mkLabelled:         {name: string, typeOf: types } list * bool -> types;
    val mkLabelEntry:       string * types -> {name: string, typeOf: types };
    val sortLabels:         {name: string, typeOf: types } list -> {name: string, typeOf: types } list;
    val entryNumber:        string * types -> int;
    val recordNotFrozen:    types -> bool;
    val recordWidth:        types -> int;
    val makeEquivalent:     typeConstrs * types list -> types;
    val firstArg:           types -> types

    val makeGeneralTypeFunction:   int * types * typeIdDescription * valAccess -> typeId
    and makeTypeFunction:   int * types * typeIdDescription -> typeId

    (* Follow a chain of unified type variables *)
    val eventual:           types -> types

    (* Test for function type and return function argument. *)
    val getFnArgType:   types -> types option

    (* Copy a type constructor. *)
    val copyTypeConstr:
        typeConstrs * (typeId -> typeId option) * (string -> string) -> typeConstrs

    val copyTypeConstrWithCache:
        typeConstrs * (typeId -> typeId option) *
        (string -> string) * typeConstrs list -> typeConstrs

    (* Copy a type. *)
    val copyType: types * (tvIndex -> types option) * (typeConstrs -> typeConstrs) -> types;

    (* Copy the type if necessary to instantiate the bound variables. *)
    val reduceToType: instanceType -> types
    
    (* Create a type from an instance without necessarily reducing it. *)
    val instanceToType: instanceType -> types

    (* Extract the instance data from an instance or create one if necessary. *)
    val getInstance: instanceType -> types * (tvIndex -> types option) * (typeId -> typeId option)

    (* Compose two typeId maps. *)
    val composeMaps: (int -> typeId) * (int -> typeId) -> (int -> typeId)

    (* Print it out prettily *)
    val display: instanceType * FixedInt.int * printTypeEnv -> pretty;
    val displayWithMap: instanceType * FixedInt.int * printTypeEnv * (int->typeId) option -> pretty;

    (* Print out a type constructor. *)
    val displayTypeConstrs: typeConstrSet * FixedInt.int * printTypeEnv -> pretty;
    val displayTypeConstrsWithMap: typeConstrSet * FixedInt.int * printTypeEnv * (int->typeId) option -> pretty;

    (* Returns the preferred type constructor from an overload. *)
    val typeConstrFromOverload: types -> typeConstrs;

    (* Check a set of mutually recursive datatypes to see which admit equality. *)
    val computeDatatypeEqualities: typeConstrSet list * (int -> bool) -> unit;

    (* Unify two type structures to give a unified type. *)
    val unifyTypes: instanceType * instanceType -> matchResult option
    
    (* Pretty print the error message. *)
    val unifyTypesErrorReport: lexan * printTypeEnv * printTypeEnv * string -> matchResult -> pretty

    (* Check whether a type admits equality. *)
    val typePermitsEquality: types -> bool

    (* Generate new copies of all unbound type variables - this is used on all
       non-local values or constructors so that, for example, each occurence of
       "hd", which has type 'a list -> 'a, can be separately bound to types. *)
    val generalise: types * typeVarTemplate list -> instanceType * types list
 
    (* Similar to generalise but with the bound variables replaced by non-unifiable
       free variables.  This is used in signature matching.  A polymorphic function
       in a structure will match a similar monomorphic function but not the other way
       round. *)
    val createNonGeneralisableInstance: (types * typeVarTemplate list) * (typeId -> typeId option) -> instanceType

    (* Release type variables at this nesting level.  Returns a type with bound variables
       for those which are  generic along with templates for each bound variable.  Free
       type variables at lower levels are not affected. *)
    val allowGeneralisation: instanceType * int * bool * (string->unit) -> types * typeVarTemplate list

    (* See if the type contains non-unifiable i.e. explicit type variables but it's an
       expansive context. *)
    val containsLocalFreeVariables: types * int -> bool

    (* Check for a local datatype "escaping".  Added for ML97. *)
    val checkForEscapingDatatypes: types * (string -> unit) -> unit

    (* Check for free type variables.  Added for ML97. *)
    val checkForFreeTypeVariables: string * types * lexan * (unit->codetree) -> unit;

    val constructorResult: types * types list -> types;

    val identical:       types * types -> bool

    (* Get the codetree "types".  This is used during code-generation to see if
       a function arguments or results are a tuple or contain floating-point values. *)
    val getCodetreeType: instanceType -> argumentType list

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
    val undefConstr:  typeConstrs

    val boolType:       types
    val fixedIntType:   types
    val stringType:     types
    val unitType:       types
    val exnType:        types
    
    val isPointerEqType: typeConstrs -> bool
    val isUndefinedTypeConstr: typeConstrs -> bool
    val isBadType:  types -> bool

    (* Create a name for a type variable 'a, 'b or ''a, ''b etc. *)
    val makeTypeVariableName: int * bool -> string

    (* If this is simply giving a new name to a type constructor returns the
       type identifier of the constructor that is being rebound. *)
    val typeNameRebinding: types list * types -> typeId option

    (* Parse tree operations. *)
    type typeParsetree and parseTypeVar
    val ParseTypeBad: typeParsetree
    val makeParseTypeConstruction:
        (string * location) * (typeParsetree list * location) * location -> typeParsetree
    val makeParseTypeProduct: typeParsetree list * location -> typeParsetree
    val makeParseTypeFunction: typeParsetree * typeParsetree * location -> typeParsetree
    val makeParseTypeLabelled:
        ((string * location) * typeParsetree * location) list * bool * location -> typeParsetree
    val makeParseTypeId: parseTypeVar * location -> typeParsetree
    val makeParseTypeFreeVar: string * bool -> parseTypeVar
    val makeParseTypeBoundVar: string * tvIndex -> parseTypeVar
    val parseTypeVarError: parseTypeVar
    (* Set a free type variable to either an outer one or to the current level. *)
    val setParseTypeVar: parseTypeVar * parseTypeVar option * int -> unit
    val getBoundTypeVar: parseTypeVar -> types
    val unitTree: location -> typeParsetree
    val displayTypeParse: typeParsetree * FixedInt.int * printTypeEnv -> pretty;
    (* A list of type variables. *)
    val displayTypeVariables: parseTypeVar list * FixedInt.int -> pretty list

    (* Fill in the values of type variables and make checks. *)
    val assignTypes: typeParsetree * (string * location -> typeConstrSet) * lexan -> types;
    
    (* Check the value we're discarding in an expression sequence or a let binding
       and return a string if it's not appropriate. *)
    val checkDiscard: instanceType * lexan -> string option

    val typeExportTree: navigation * typeParsetree -> exportTree
    
    val setPreferredInt: typeConstrs -> unit

    (* Is this a labelled record with fields #1, #2 etc.?  Used for printing. *)
    val isProductType: types -> bool

    structure TypeValue:
    sig
        val extractEquality: codetree -> codetree
        and extractPrinter: codetree -> codetree
        val createTypeValue: {eqCode: codetree, printCode: codetree} -> codetree
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

    structure Overloads:
    sig
        val addOverload: string * typeConstrs * codetree -> unit
        val getOverloadTypes: string -> typeConstrs list
        val getOverloadCode: string * typeConstrs -> codetree
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
        and  parseTypeVar   = parseTypeVar
        and  locationProp = locationProp
        and  pretty     = pretty
        and  lexan      = lexan
        and  ptProperties = ptProperties
        and  typeVar    = typeVar
        and  codetree   = codetree
        and  matchResult = matchResult
        and  tvLevel = tvLevel
        and  valAccess  = valAccess
        and  tvIndex = tvIndex
        and  typeVarTemplate = typeVarTemplate
        and  argumentType = argumentType
        and  instanceType = instanceType
    end

end;

