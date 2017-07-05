(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited
        
    Modified David C. J. Matthews 2009-2015.

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

signature VALUEOPSSIG =
sig
    type lexan
    type codetree
    type types
    type values
    type structVals
    type functors
    type valAccess
    type typeConstrs
    type typeConstrSet
    type signatures
    type fixStatus
    type univTable
    type pretty
    type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }
    type locationProp
    type typeId
    type typeVarForm
    type typeVarMap
    type level
    type machineWord

    (* Construction functions. *)
    val mkGvar:        string * types * codetree * locationProp list -> values
    val mkValVar:      string * types * locationProp list -> values
    val mkPattVar:     string * types * locationProp list -> values
    val mkSelectedVar: values * structVals * locationProp list -> values
    val mkGconstr:     string * types * codetree * bool * int * locationProp list -> values
    val mkGex:         string * types * codetree * locationProp list -> values
    val mkEx:          string * types * locationProp list -> values

    type nameSpace =
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

        allVal:       unit -> (string*values) list,
        allType:      unit -> (string*typeConstrSet) list,
        allFix:       unit -> (string*fixStatus) list,
        allStruct:    unit -> (string*structVals) list,
        allSig:       unit -> (string*signatures) list,
        allFunct:     unit -> (string*functors) list
    };

    type printTypeEnv =
        { lookupType: string -> (typeConstrSet * (int->typeId) option) option,
          lookupStruct: string -> (structVals * (int->typeId) option) option}

    (* Print values. *)
    val displayFixStatus:  fixStatus -> pretty
    val displaySignatures: signatures * FixedInt.int * printTypeEnv -> pretty
    val displayStructures: structVals * FixedInt.int * printTypeEnv -> pretty
    val displayFunctors:   functors   * FixedInt.int * printTypeEnv -> pretty
    val displayValues: values * FixedInt.int * printTypeEnv -> pretty
    val printValues: values * FixedInt.int -> pretty

    val nullEnvironment : nameSpace

    val codeStruct:     structVals * level -> codetree
    val codeAccess:     valAccess  * level -> codetree
    val codeVal:
        values * level * typeVarMap * {value: types, equality: bool, printity: bool} list * lexan * location -> codetree
    val codeExFunction: values * level * typeVarMap * types list * lexan * location -> codetree
    val applyFunction:
        values * codetree * level * typeVarMap * {value: types, equality: bool, printity: bool} list *
            lexan * location -> codetree
    val getOverloadInstance: string * types * bool -> codetree*string
    val makeGuard:      values * types list * codetree * level * typeVarMap -> codetree 
    val makeInverse:    values * types list * codetree * level * typeVarMap -> codetree
                    
    val lookupStructure:  string * {lookupStruct: string -> structVals option} * 
                            string * (string -> unit) -> structVals option
                                           
    val lookupStructureAsSignature:
        (string -> structVals option) *  string * (string -> unit) -> structVals option
                                           
    val lookupValue:   string * {lookupVal: string -> values option, lookupStruct: string -> structVals option} * 
                          string * (string -> unit) -> values
                                
    val lookupTyp:   {lookupType: string -> typeConstrSet option,
                      lookupStruct: string -> structVals option} * 
                        string * (string -> unit) -> typeConstrSet

    val codeLocation: location -> codetree

    val getPolymorphism: values * types * typeVarMap -> {value: types, equality: bool, printity: bool} list
    
    val getLiteralValue: values * string * types * (string->unit) -> machineWord option

    (* Types that can be shared. *)
    structure Sharing:
    sig
        type lexan          = lexan
        type codetree       = codetree
        type types          = types
        type values         = values
        type structVals     = structVals
        type functors       = functors
        type valAccess      = valAccess
        type typeConstrs    = typeConstrs
        type typeConstrSet  = typeConstrSet
        type signatures     = signatures
        type fixStatus      = fixStatus
        type univTable      = univTable
        type pretty         = pretty
        type locationProp   = locationProp
        type typeId         = typeId
        type typeVarForm    = typeVarForm
        type typeVarMap     = typeVarMap
        type level          = level
        type machineWord    = machineWord
    end
end;
