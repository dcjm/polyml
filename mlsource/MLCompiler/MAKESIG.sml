(*
    Copyright (c) 2016 David C.J. Matthews

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

signature MAKESIG =
sig
    type env
    type gEnv
    type values
    type typeConstrSet
    type fixStatus
    type structVals
    type signatures
    type functors
    type ptProperties

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
      }

    type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }
    type exportTree = location * ptProperties list
      
    val compiler : nameSpace * (unit->char option) * Universal.universal list ->
        exportTree option * ( unit ->
           { fixes: (string * fixStatus) list, values: (string * values) list,
             structures: (string * structVals) list, signatures: (string * signatures) list,
             functors: (string * functors) list, types: (string * typeConstrSet) list }) option

    val makeGEnv   : unit -> gEnv
    val gEnvAsEnv  : gEnv -> env
    val gEnvAsNameSpace: gEnv -> nameSpace
    val useIntoEnv   : gEnv -> Universal.universal list -> string -> unit
    val useStringIntoEnv: gEnv -> string -> unit
    val shellProc   : gEnv -> unit -> unit    (* The command processor *)
    
    structure Sharing:
    sig
        type env = env
        type gEnv = gEnv
        type values = values
        type typeConstrSet = typeConstrSet
        type fixStatus = fixStatus
        type structVals = structVals
        type signatures = signatures
        type functors = functors
        type ptProperties = ptProperties
    end
end;
